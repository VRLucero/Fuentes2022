/*<TOAD_FILE_CHUNK>*/
CREATE OR REPLACE PACKAGE MANANTIAL.pkg_ppe IS
/*    Desarrollo inical  :  VLUCERO    Julio/agosto 2022  

*/ 
 G_Log UTL_FILE.file_type;
 G_mensaje Varchar2(2000); 
 FUNCTION  Control_ppe(r_inserta_recaud IN OUT PKG_RECAUDACIONES.vIncRecaudacionRec, pOperacion number , pSecuencia number  ) RETURN BOOLEAN ;
 /*
 Mantenido en repositorio GIT  
 */ 
 
END;
/

/*<TOAD_FILE_CHUNK>*/
CREATE OR REPLACE PACKAGE BODY MANANTIAL.PKG_PPE IS
----------------------------------------------------------------------
FUNCTION ControlFecha(pNroPlan Number, pVto number) RETURN BOOLEAN IS
/* Esta funcion  verifica si el plan especial tiene menos de una cantidad deerminada de dias de  emitido */ 
lRta BOOLEAN := TRUE;
l_fecha  date;   
BEGIN  
    BEGIN  
        SELECT ppe_fec_alta  INTO l_fecha 	    
	    FROM   Manantial.PLANES_ESPECIALES
	    WHERE  ppe_id = pNroPlan;
	EXCEPTION  WHEN others  THEN
	    l_Fecha := Sysdate - pVto;
	    G_mensaje:=G_mensaje||' ControlFecha():'|| SQLERRM ;
	    lRta := FALSE;   -- No se puede  continuar        
	END ;         
    IF l_Fecha < (Sysdate - pVto)  THEN
       lRta := FALSE; -- La propuesta del plan especial, ya esta vencida      
    END IF;            
    RETURN(lRta);
END;
-------------------------------------------------------------------------- 
FUNCTION ControlPlanGenerado(pNroPlan Number) RETURN number IS
/* Esta funcion debe determinar si el plan especial  ha sido ya generado . Retorna el nro de plan de pago definitivo si ya se genero.*/ 
lRta number := 0;   
BEGIN  
    BEGIN  
        SELECT ppe_ppl_id   INTO lRta  	    
	    FROM   Manantial.PLANES_ESPECIALES
	    WHERE  ppe_id = pNroPlan;
	EXCEPTION  WHEN NO_DATA_FOUND  THEN
	    lRta := 0 ;
	    G_Mensaje := G_Mensaje || ' ControlPlanGenerado():No existe el Plan Especial ' ;     
	WHEN others  THEN
	    lRta := 0 ;
	    G_Mensaje := G_Mensaje || ' ControlPlanGenerado():' || SQLERRM ;
	END ;       
    RETURN(lRta);
END; 
------------------------------------------------
FUNCTION RecPendiente(pRec PKG_RECAUDACIONES.vIncRecaudacionRec, pOperacion number, pSecuencia number ) RETURN number IS
/* generar un registro en recaudaciones con estado = 3 */
v_rec_seq number  := 0;    
l_inmuebles Varchar2(14);
v_fecha_insertada Date; 
p_increcaudacionrec PKG_RECAUDACIONES.vIncRecaudacionRec := pRec ;
p_obligacion Varchar2(14);
v_fecha_acred Date;
BEGIN  
    p_increcaudacionrec.p_estado := 3;
    
    SELECT Manantial. rec_seq.NEXTVAL   INTO v_rec_seq    FROM DUAL;
    
    v_fecha_insertada := NULL;
    p_obligacion      := NULL;
    l_inmuebles       := NULL;
    v_fecha_acred     := NULL;

    BEGIN
         SELECT inm_id  INTO l_inmuebles
           FROM Manantial.INMUEBLES
           WHERE inm_cuenta = p_increcaudacionrec.v_identificacion;
    EXCEPTION
         WHEN OTHERS THEN
            l_inmuebles := NULL;
            G_mensaje:=G_mensaje||' Sin INMUEBLE.';
    END;
    
    BEGIN
         SELECT NVL(rem_fec_acreditacion, rem__fecha)   INTO v_fecha_acred
         FROM Manantial.REMESAS
         WHERE rem__numero = p_increcaudacionrec.v_remesa_osm;
    EXCEPTION
         WHEN NO_DATA_FOUND THEN
            v_fecha_acred := NULL;
    END;
    IF p_increcaudacionrec.p_banco IS NULL THEN
       p_increcaudacionrec.p_banco :=  p_increcaudacionrec.p_ente;
    END IF; 
    BEGIN 
      INSERT INTO Manantial.RECAUDACIONES
                  (rec_id, rec_vco_codigo, rec_inm_id, rec_obl_id,  rec_erc_codigo, rec_cuenta, rec_fecha, rec_imp_cobrado, rec_nro_factura, rec_anio_periodo, rec_mes_periodo,
                   rec_se, rec_usr_alta, rec_fec_alta, rec_fec_aplicacion, rec_fec_proceso, rec_fec_contable, rec_banco, rec_secuencia, rec_usr_baja, rec_fec_baja,
                   rec_usr_mod, rec_fec_mod, rec_nro_operacion, rec_rem_numero, rec_rem_ere_codigo, rec_remesa, rec_fec_acredita, rec_clave_heredada, rec_lote)
           VALUES (v_rec_seq, p_increcaudacionrec.p_ente, l_inmuebles, p_obligacion, p_increcaudacionrec.p_estado, p_increcaudacionrec.v_identificacion,
                   p_increcaudacionrec.v_fecha_cobro, p_increcaudacionrec.v_importe, p_increcaudacionrec.v_factura, p_increcaudacionrec.v_anio, p_increcaudacionrec.v_periodo,
                   p_increcaudacionrec.p_se, p_increcaudacionrec.v_usuario, SYSDATE, v_fecha_insertada, SYSDATE,  sysdate  ,
                   p_increcaudacionrec.p_banco, pSecuencia, NULL, NULL,  NULL, NULL, pOperacion, p_increcaudacionrec.v_remesa_osm, p_increcaudacionrec.p_ente,p_increcaudacionrec.v_remesa, v_fecha_acred, NULL, NULL);
    EXCEPTION  WHEN Others THEN 
        v_rec_seq := 0;            
        G_mensaje:=G_mensaje||' RecPendiente():' ||SQLERRM ;
    END;     
    RETURN(v_rec_seq);
END; 
------------------------------------------------------------------------------------------------------
FUNCTION VerEstadoPlan(p_ppl_ID number  ) RETURN number IS
lRta  Number ;
/*   ESTADO DE PLANES 
     1      Pendiente 
     2      Cancelado (se termino de pagar ) 
     3      Anulado    
     4      Moroso 
     5      Activo-Caido                    */  
BEGIN 
   BEGIN 
      SELECT  ppl_estado INTO lRta  
      FROM Manantial.PLANES_PAGO 
      WHERE ppl_id = p_ppl_id ;
   EXCEPTION   WHEN others   THEN 
      lRta := 0 ;    
      G_mensaje:=G_mensaje||' VerEstadoPlan():'|| SQLERRM ; 
   END; 
   RETURN(lRta); 
END;  
------------------------------------------------                                                     
                                                                                           
FUNCTION AplicarCuota( p_aplicador PKG_RECAUDACIONES.vAplicarRecauRec, p_Operacion number ,  p_secuencia number, p_Cuota Number ) RETURN number IS
lRta    Number :=0;  
lError  Varchar2(200); 
        
CURSOR cLineas(p_Obl_id number ) IS                 
        SELECT CCT_TIPO_MOVIMIENTO, CCT_FEC_VENCIMIENTO, CCT_CUENTA, CCT_POR_IVA, CCT_POR_ALI, CCT_ID_MOVIMIENTO, CCT_SER_CODIGO, CCT_GRP_CODIGO, CCT_FEC_GENERACION,
               CCT_SUC_CODIGO, CCT_PEF_ANIO, CCT_PEF_PERIODO, CCT_TRE_CODIGO, CCT_IMP_DEBE, 
               CCT_IMP_DEBE - NVL( (SELECT SUM(APA_IMP_HABER) FROM Manantial.ASIG_PAGOS 
                                                            WHERE APA_CCT_OBL_ID = CCT_OBL_ID 
                                                            AND APA_CCT_ID_MOVIMIENTO = CCT_ID_MOVIMIENTO),0) CCT_SALDO ,
             CCT_IMP_IVA, CCT_IMP_ALI
        FROM Manantial.CUENTAS_CORRIENTES 
        WHERE CCT_OBL_ID = P_OBL_ID
        AND   CCT_ESTADO = 15 AND CCT_IMP_DEBE > 0;       
rLinea cLineas%ROWTYPE;
v_Obl  Manantial.OBLIGACIONES%ROWTYPE;
v_linea    Number;  
v_inm_id   Number;  
v_tipo_iva Number;
v_rec_id   Number;  
v_Plan     number := p_Aplicador.v_Factura;
l_monto_recargo Number := 0; 
l_dev_nov       Number := 0;
r_datos_iva  PKG_SERVICIOS_FIJOS.ivaRec; 
r_Importe    PKG_SERVICIOS_FIJOS.importerec;
r_novedades  Manantial.NOVEDADES_FACTURABLES%ROWTYPE;
BEGIN        
       lRta := p_Cuota;       
       BEGIN
           SELECT * INTO v_obl
           FROM Manantial.OBLIGACIONES 
           WHERE obl_id = p_Cuota;
       END;
       BEGIN
          SELECT max(cct_id_movimiento)+1 INTO v_linea
          FROM Manantial.CUENTAS_CORRIENTES WHERE cct_obl_id = p_Cuota;
       END;
       BEGIN
           SELECT inm_id,inm_tipo_responsable
           INTO v_inm_id, v_tipo_iva
           FROM Manantial.INMUEBLES
           WHERE inm_cuenta = p_aplicador.v_identificacion;
           EXCEPTION WHEN others THEN 
              v_inm_id := NULL;
              v_tipo_iva := NULL;
       END;
       BEGIN
           SELECT manantial.rec_seq.NEXTVAL INTO v_rec_id FROM dual;
       END;
       BEGIN
             INSERT INTO manantial.RECAUDACIONES
                              (rec_id, rec_vco_codigo, rec_inm_id, rec_obl_id, rec_erc_codigo, rec_cuenta, rec_fecha,
                               rec_imp_cobrado, rec_nro_factura, rec_anio_periodo, rec_mes_periodo, rec_se, rec_usr_alta,
                               rec_fec_alta, rec_fec_aplicacion, rec_fec_proceso, rec_fec_contable, rec_banco,
                               rec_secuencia, rec_usr_baja, rec_fec_baja, rec_usr_mod, rec_fec_mod, rec_nro_operacion,
                               rec_rem_numero, rec_rem_ere_codigo, rec_remesa, rec_conciliado, rec_fec_acredita,
                               rec_clave_heredada, rec_lote)
             VALUES (v_rec_id, p_aplicador.p_ente , v_inm_id, p_Cuota, p_aplicador.p_erc_codigo, p_aplicador.v_identificacion, p_aplicador.v_fecha_cobro,
                               p_aplicador.v_importe, p_aplicador.v_factura, p_aplicador.v_anio, p_aplicador.v_periodo, 'N', p_aplicador.v_usuario,
                               sysdate, sysdate, sysdate, sysdate, p_aplicador.p_banco,
                               p_secuencia, NULL, NULL,  p_aplicador.v_usuario, sysdate, p_Operacion,
                                p_aplicador.v_remesa_osm, p_aplicador.p_ente, p_aplicador.v_remesa, 'N', p_aplicador.v_fecha_cobro,
                               NULL, NULL);
       EXCEPTION WHEN Others THEN  
          lRta := 0;
          lError:= SQLERRM ;
          G_Mensaje := G_Mensaje || ' AplicarCuota(REC):' || lError;
          RETURN( 0 );                           
       END;
       IF p_aplicador.p_erc_codigo <>  1 THEN
          G_Mensaje := G_Mensaje || ' AplicarCuota(): ERC_CODIGO Erroneo =' || to_char(p_aplicador.p_erc_codigo) ;
          RETURN( 0 );  
       END IF;
        ---- Generar linea en Cuenta Corrientes ---
       BEGIN
          INSERT INTO Manantial.CUENTAS_CORRIENTES (CCT_OBL_ID, CCT_ID_MOVIMIENTO, CCT_SER_CODIGO, CCT_PEF_ANIO, CCT_PEF_PERIODO, CCT_TIPO_MOVIMIENTO, 
                        CCT_IMP_DEBE, CCT_IMP_HABER, CCT_CONCEPTO, CCT_FEC_GENERACION, CCT_FEC_CIERRE_CONTABLE, CCT_USR_ALTA, CCT_FEC_ALTA, CCT_REC_ID, 
                        CCT_NOV_ID, CCT_FEC_VENCIMIENTO, CCT_CNT_DIAS_RECARGO, CCT_BANCO, CCT_REMESA, CCT_SECUENCIA, CCT_FEC_APLICACION, CCT_FEC_PROCESO, 
                        CCT_FEC_COBRO, CCT_USR_BAJA, CCT_FEC_BAJA, CCT_USR_MOD, CCT_FEC_MOD, CCT_ESTADO, CCT_SE, CCT_OBL_ID_CUOTA, CCT_GRP_CODIGO, 
                        CCT_CUENTA, CCT_SUC_CODIGO, CCT_TRE_CODIGO, CCT_IMP_IVA, CCT_POR_IVA, CCT_IMP_ALI, CCT_POR_ALI)
          VALUES (p_Cuota, v_linea, 800088, v_obl.obl_pef_anio, v_obl.obl_pef_periodo, v_obl.obl_tpc_codigo,
                        0, p_aplicador.v_importe, 'Pago cuota plan '||v_obl.obl_nro_factura, sysdate, sysdate, p_aplicador.v_usuario, sysdate, v_rec_id,
                        NULL, sysdate, NULL, p_aplicador.p_banco, p_aplicador.v_remesa, p_secuencia, sysdate, sysdate,
                        p_aplicador.v_fecha_cobro, NULL, NULL ,NULL ,NULL , 30, 'N', NULL, v_obl.obl_grp_codigo,
                        v_obl.obl_cuenta, v_obl.obl_suc_codigo, v_tipo_iva, 0, 0, 0, 0  );
       -------------------------------------------------------                 
         lRta := v_rec_id ;  --- RETORNA EL REC_ID GENERADO   
       -------------------------------------------------------              
       EXCEPTION   WHEN others   THEN
          lRta := 0;
          lError:= SQLERRM ;
          G_Mensaje := G_Mensaje || ' AplicarCuota(CTACTE):' || lError;
          RETURN( 0 );
       END;
        ---- Generar ASIG_PAGOS  ------
       BEGIN
            FOR rLinea IN cLineas(p_Cuota) LOOP                    
                INSERT INTO Manantial.ASIG_PAGOS (APA_ID, APA_TIPO_MOVIMIENTO, APA_IMP_HABER, APA_CONCEPTO, APA_FEC_GENERACION, APA_FEC_CONTABILIZACION, APA_FEC_VENCIMIENTO, 
                             APA_BANCO, APA_REMESA, APA_FEC_APLICACION, APA_FEC_PROCESO, APA_FEC_COBRO, APA_ESTADO, APA_SE, APA_CUENTA, APA_IMP_IVA, APA_POR_IVA, 
                             APA_IMP_ALI, APA_POR_ALI, APA_CCT_OBL_ID, APA_CCT_ID_MOVIMIENTO, APA_SER_CODIGO, APA_REC_ID, APA_GRP_CODIGO, APA_SUC_CODIGO, APA_PEF_ANIO, 
                             APA_PEF_PERIODO, APA_TRE_CODIGO, APA_FEC_ALTA, APA_USR_ALTA)
                VALUES (APA_SEQ.NEXTVAL, rLinea.CCT_TIPO_MOVIMIENTO, rLinea.CCT_SALDO, 'Pago con Recibo '||v_obl.obl_nro_factura||' - Plan Pago' ,  rLinea.CCT_FEC_GENERACION, SYSDATE, rLinea.CCT_FEC_VENCIMIENTO,
                             p_aplicador.p_banco, p_aplicador.v_remesa, SYSDATE, SYSDATE, p_aplicador.v_fecha_cobro, 30, 'N', rLinea.CCT_CUENTA, rLinea.CCT_IMP_IVA * (rLinea.CCT_SALDO/rLinea.CCT_IMP_DEBE), rLinea.CCT_POR_IVA,
                             rLinea.CCT_IMP_ALI * (rLinea.CCT_SALDO/rLinea.CCT_IMP_DEBE), rLinea.CCT_POR_ALI, p_Cuota, rLinea.CCT_ID_MOVIMIENTO, rLinea.CCT_SER_CODIGO, V_REC_ID, rLinea.CCT_GRP_CODIGO, rLinea.CCT_SUC_CODIGO, rLinea.CCT_PEF_ANIO,
                             rLinea.CCT_PEF_PERIODO, rLinea.CCT_TRE_CODIGO, SYSDATE, p_aplicador.v_USUARIO);                                      
            END LOOP;        
       EXCEPTION   WHEN others   THEN
          lRta := 0;
          lError:= SQLERRM ;
          G_Mensaje := G_Mensaje || ' AplicarCuota(ASIG_PAGO):' || lError;
          RETURN( 0 );        
       END;    
        ---- Actualizar obligacion (Las cuotas se pagan completas )---   
       BEGIN
                UPDATE Manantial.OBLIGACIONES
                   set obl_saldo = 0,
                       obl_estado = 30, 
                       obl_fec_aplicacion = sysdate, 
                       obl_fec_mod = sysdate, 
                       obl_usr_mod = p_aplicador.v_usuario
                 WHERE obl_id = p_Cuota;
       EXCEPTION   WHEN others   THEN
          lRta := 0;
          lError:= SQLERRM ;
          G_Mensaje := G_Mensaje || ' AplicarCuota(OBL):' || lError;
          RETURN( 0 );          
       END;
       BEGIN
            UPDATE CUENTAS_CORRIENTES
                   set cct_estado = 30, cct_fec_cobro = p_aplicador.v_fecha_cobro, cct_fec_proceso = sysdate, cct_fec_aplicacion = sysdate,
                       cct_rec_id = v_rec_id, cct_banco = p_aplicador.p_banco, cct_remesa = p_aplicador.v_remesa, cct_secuencia = p_secuencia, cct_fec_mod = sysdate, 
                       cct_usr_mod = p_aplicador.v_usuario
            WHERE cct_obl_id = p_Cuota 
              AND cct_estado = 15 AND cct_imp_debe > 0;
       END;
       G_Mensaje := G_Mensaje || ' Pago aplic. obl_id: '|| to_char(p_Cuota) ;
       IF p_aplicador.v_fecha_cobro > (V_OBL.OBL_FEC_VENCIMIENTO + 2 ) AND p_aplicador.p_ente != 2 THEN
          BEGIN
                l_monto_recargo := CALCULO_RECARGOS(V_OBL.OBL_FEC_VENCIMIENTO, p_aplicador.v_fecha_cobro , V_OBL.OBL_IMP_ORIGINAL);                     
                r_datos_iva := PKG_SERVICIOS_FIJOS.f_imp_iva(V_TIPO_IVA, 640110, SYSDATE);
                /* Calcula el IVA correspondiente, en caso que tenga una nueva categoria */
                r_importe.iva       := (r_datos_iva.iva       * l_monto_recargo) / 100;
                r_importe.alicuota  := (r_datos_iva.alicuota  * l_monto_recargo) / 100;
                r_importe.percepcion:= (r_datos_iva.percepcion*(l_monto_recargo + r_importe.iva)) / 100;
                /* Completa y genera la Novedad Facturable de Recargos por Pago Fuera de Término */
                r_novedades.nov_imp_neto   := l_monto_recargo;
                r_novedades.nov_pef_anio   := P_APLICADOR.v_anio;
                r_novedades.nov_pef_periodo:= P_APLICADOR.v_periodo;
                r_novedades.nov_ser_codigo := 640110;
                r_novedades.nov_tipo_origen:= 'Z';
                r_novedades.nov_con_inm_id := V_OBL.OBL_INM_ID;  
                r_novedades.nov_tipo_novedad := 1;
                r_novedades.nov_fec_novedad:= SYSDATE;
                r_novedades.nov_estado     := 2;
                r_novedades.nov_nro_origen := V_obl.Obl_id; 
                r_novedades.nov_imp_iva_cf := 0;
                r_novedades.nov_imp_iva_ex := 0;
                r_novedades.nov_imp_iva_ri := 0;
                r_novedades.nov_imp_iva_rni:= 0;
                r_novedades.nov_imp_iva_mon:= 0;
                r_novedades.nov_imp_iva_ali:= 0;
                r_novedades.nov_imp_iva_per:= 0;
                r_novedades.nov_imp_cambio := 1;
                r_novedades.nov_inm_id := V_OBL.OBL_INM_ID;
                r_novedades.nov_obl_id := V_obl.Obl_id;
                r_novedades.nov_cli_id := V_OBL.OBL_cli_id;
                r_novedades.nov_dpc_id := NULL;
                r_novedades.nov_descripcion :='Recargo s/Cuota plan Pago:'|| 
                                                      LPAD(TO_CHAR(NVL(V_OBL.OBL_nro_factura, 0), '9999999999999999'), 17, ' ');
                r_novedades.nov_descripcion :=r_novedades.nov_descripcion||' Vto:'
                      || TO_CHAR(V_OBL.OBL_FEC_VENCIMIENTO, 'dd/mm/rrrr')
                      || ' Pag:'
                      || TO_CHAR(P_APLICADOR.v_fecha_cobro, 'dd/mm/rrrr');
                r_novedades.nov_fec_destino := NULL;
                r_novedades.nov_tipo_destino:= NULL;
                r_novedades.nov_cod_iva  := V_TIPO_IVA;
                r_novedades.nov_usr_alta := p_aplicador.v_usuario;
                r_novedades.nov_fec_alta := sysdate;
                r_novedades.nov_imp_iva_per := r_importe.percepcion;
                r_novedades.nov_imp_iva_ali := r_importe.alicuota;
                IF V_TIPO_IVA = 1 THEN
                     r_novedades.nov_imp_iva_cf := r_importe.iva;
                ELSIF V_TIPO_IVA = 2 OR V_TIPO_IVA = 7 THEN
                     r_novedades.nov_imp_iva_ex := r_importe.iva;
                ELSIF V_TIPO_IVA = 4 OR V_TIPO_IVA = 8 THEN
                     r_novedades.nov_imp_iva_ri := r_importe.iva;
                ELSIF V_TIPO_IVA = 3 OR V_TIPO_IVA = 9 THEN
                     r_novedades.nov_imp_iva_rni := r_importe.iva;
                ELSIF V_TIPO_IVA = 6 THEN
                     r_novedades.nov_imp_iva_mon := r_importe.iva;
                END IF;
                l_dev_nov := PKG_SERVICIOS_FIJOS.f_insertar_novedad(r_novedades);
               -- lRta :=  l_dev_nov;                   
          END;
       END IF;
   RETURN(lRta);
END; 

--------------------------------------------------------------------
FUNCTION Aplicar( p_Monto number, p_obl_id number,  p_plan   number, p_aplicador PKG_RECAUDACIONES.vAplicarRecauRec, p_Rec_ID number  ) RETURN number IS
CURSOR cLineas(p_Obl_id number ) IS                 
        SELECT CCT_TIPO_MOVIMIENTO, CCT_FEC_VENCIMIENTO, CCT_CUENTA, CCT_POR_IVA, CCT_POR_ALI, CCT_ID_MOVIMIENTO, CCT_SER_CODIGO, CCT_GRP_CODIGO, CCT_FEC_GENERACION,
               CCT_SUC_CODIGO, CCT_PEF_ANIO, CCT_PEF_PERIODO, CCT_TRE_CODIGO, CCT_IMP_DEBE, 
               CCT_IMP_DEBE - NVL( (SELECT SUM(APA_IMP_HABER) FROM Manantial.ASIG_PAGOS 
                                                            WHERE APA_CCT_OBL_ID = CCT_OBL_ID 
                                                            AND APA_CCT_ID_MOVIMIENTO = CCT_ID_MOVIMIENTO),0) CCT_SALDO ,
             CCT_IMP_IVA, CCT_IMP_ALI
        FROM Manantial.CUENTAS_CORRIENTES 
        WHERE CCT_OBL_ID = P_OBL_ID
        AND   CCT_ESTADO = 15 AND CCT_IMP_DEBE > 0;       
rLinea cLineas%ROWTYPE;
lRta       Number := p_Monto;  
v_linea    Number := 0;
v_inm_id   Number := 0;
v_tipo_iva Number := 0;
v_obl      Manantial.OBLIGACIONES%ROWTYPE;
v_Saldo    Number;  
BEGIN 
    BEGIN
       SELECT max(cct_id_movimiento)+1 INTO v_linea
       FROM Manantial.CUENTAS_CORRIENTES WHERE cct_obl_id = p_Obl_id;
       BEGIN
           SELECT * INTO v_obl
           FROM Manantial.OBLIGACIONES 
           WHERE obl_id = p_Obl_id;
       END;       
       BEGIN
           SELECT inm_id,inm_tipo_responsable
           INTO v_inm_id, v_tipo_iva
           FROM Manantial.INMUEBLES
           WHERE inm_cuenta = p_aplicador.v_identificacion;
           EXCEPTION WHEN others THEN 
              v_inm_id := NULL;
              v_tipo_iva := NULL;
       END;
        ---- Generar linea en Cuenta Corrientes ---
       BEGIN
          INSERT INTO Manantial.CUENTAS_CORRIENTES (CCT_OBL_ID, CCT_ID_MOVIMIENTO, CCT_SER_CODIGO, CCT_PEF_ANIO, CCT_PEF_PERIODO, CCT_TIPO_MOVIMIENTO, 
                        CCT_IMP_DEBE, CCT_IMP_HABER, CCT_CONCEPTO, CCT_FEC_GENERACION, CCT_FEC_CIERRE_CONTABLE, CCT_USR_ALTA, CCT_FEC_ALTA, CCT_REC_ID, 
                        CCT_NOV_ID, CCT_FEC_VENCIMIENTO, CCT_CNT_DIAS_RECARGO, CCT_BANCO, CCT_REMESA, CCT_SECUENCIA, CCT_FEC_APLICACION, CCT_FEC_PROCESO, 
                        CCT_FEC_COBRO, CCT_USR_BAJA, CCT_FEC_BAJA, CCT_USR_MOD, CCT_FEC_MOD, CCT_ESTADO, CCT_SE, CCT_OBL_ID_CUOTA, CCT_GRP_CODIGO, 
                        CCT_CUENTA, CCT_SUC_CODIGO, CCT_TRE_CODIGO, CCT_IMP_IVA, CCT_POR_IVA, CCT_IMP_ALI, CCT_POR_ALI)
                VALUES (p_Obl_id, v_linea, 800088, v_obl.obl_pef_anio, v_obl.obl_pef_periodo, v_obl.obl_tpc_codigo,
                        0, p_Monto , 'Pago cuota plan '||p_plan , sysdate, sysdate, p_aplicador.v_usuario, sysdate, P_rec_id,
                        NULL, sysdate, NULL, p_aplicador.p_banco, p_aplicador.v_remesa,  1 --p_secuencia
                        , sysdate, sysdate,
                        p_aplicador.v_fecha_cobro, NULL, NULL ,NULL ,NULL , 30, 'N', NULL, v_obl.obl_grp_codigo,
                        v_obl.obl_cuenta, v_obl.obl_suc_codigo, v_tipo_iva, 0, 0, 0, 0  );
       EXCEPTION   WHEN others   THEN
          lRta := 0;   
          G_Mensaje := G_Mensaje || ' Aplicar(CTACTE):' || SQLERRM ;
          RETURN( 0 );
       END;
         BEGIN
            FOR rLinea IN cLineas(p_obl_id) LOOP                    
                INSERT INTO Manantial.ASIG_PAGOS (APA_ID, APA_TIPO_MOVIMIENTO, APA_IMP_HABER, APA_CONCEPTO, APA_FEC_GENERACION, APA_FEC_CONTABILIZACION, APA_FEC_VENCIMIENTO, 
                             APA_BANCO, APA_REMESA, APA_FEC_APLICACION, APA_FEC_PROCESO, APA_FEC_COBRO, APA_ESTADO, APA_SE, APA_CUENTA, APA_IMP_IVA, APA_POR_IVA, 
                             APA_IMP_ALI, APA_POR_ALI, APA_CCT_OBL_ID, APA_CCT_ID_MOVIMIENTO, APA_SER_CODIGO, APA_REC_ID, APA_GRP_CODIGO, APA_SUC_CODIGO, APA_PEF_ANIO, 
                             APA_PEF_PERIODO, APA_TRE_CODIGO, APA_FEC_ALTA, APA_USR_ALTA)
                VALUES (APA_SEQ.NEXTVAL, rLinea.CCT_TIPO_MOVIMIENTO, rLinea.CCT_SALDO, 'Pago con Recibo '||v_obl.obl_nro_factura||' - Plan Pago' ,  rLinea.CCT_FEC_GENERACION, SYSDATE, rLinea.CCT_FEC_VENCIMIENTO,
                             p_aplicador.p_banco, p_aplicador.v_remesa, SYSDATE, SYSDATE, p_aplicador.v_fecha_cobro, 30, 'N', rLinea.CCT_CUENTA, rLinea.CCT_IMP_IVA * (rLinea.CCT_SALDO/rLinea.CCT_IMP_DEBE), rLinea.CCT_POR_IVA,
                             rLinea.CCT_IMP_ALI * (rLinea.CCT_SALDO/rLinea.CCT_IMP_DEBE), rLinea.CCT_POR_ALI, P_Obl_Id, rLinea.CCT_ID_MOVIMIENTO, rLinea.CCT_SER_CODIGO, P_REC_ID, rLinea.CCT_GRP_CODIGO, rLinea.CCT_SUC_CODIGO, rLinea.CCT_PEF_ANIO,
                             rLinea.CCT_PEF_PERIODO, rLinea.CCT_TRE_CODIGO, SYSDATE, p_aplicador.v_USUARIO);                                      
            END LOOP;        
       EXCEPTION   WHEN others   THEN
          lRta := 0;          
          G_Mensaje := G_Mensaje || ' Aplicar(ASIG_PAGO):' || SQLERRM;
          RETURN( 0 );        
       END;
       
      FOR rLinea IN cLineas(p_obl_id) LOOP
          IF  Abs(rLinea.cct_Saldo) < 0.02 THEN
              UPDATE Manantial.CUENTAS_CORRIENTES
                   set cct_estado = 30, cct_fec_cobro = p_aplicador.v_fecha_cobro, cct_fec_proceso = sysdate, cct_fec_aplicacion = sysdate,
                       cct_rec_id = p_rec_id, cct_banco = p_aplicador.p_banco, cct_remesa = p_aplicador.v_remesa, cct_secuencia = 1 --p_secuencia
                       , cct_fec_mod = sysdate, cct_usr_mod = p_aplicador.v_usuario
                  WHERE cct_obl_id = p_Obl_Id
                  AND   cct_id_movimiento = rLinea.cct_id_movimiento ; 
          END IF;  
      END LOOP;      
       ---- Actualizar obligacion (Saldo y Estado  )---   
       BEGIN
            UPDATE Manantial.OBLIGACIONES
               set obl_saldo = ( SELECT  sum(cct_imp_debe - cct_imp_Haber)  FROM Manantial.CUENTAS_CORRIENTES  WHERE cct_obl_id = obl_id),                        
                   obl_fec_aplicacion = sysdate, 
                   obl_fec_mod = sysdate, 
                   obl_usr_mod = p_aplicador.v_usuario
               WHERE obl_id = p_Obl_id ; 
       EXCEPTION   WHEN others   THEN
          lRta := 0;           
          G_Mensaje := G_Mensaje || ' Aplicar(OBL):' || SQLERRM ;
          RETURN( 0 );          
       END;    
       
       SELECT  obl_saldo INTO v_Saldo  
       FROM Manantial.OBLIGACIONES 
       WHERE obl_id = p_Obl_id ; 
       IF  v_Saldo IS NULL THEN
           G_Mensaje := G_Mensaje || ' Saldo OBL NULO'  ;
           RETURN (0); 
       ELSE 
          BEGIN
            UPDATE Manantial.OBLIGACIONES
               set obl_estado = 30 ,                        
                   obl_fec_aplicacion = sysdate, 
                   obl_fec_mod = sysdate, 
                   obl_usr_mod = p_aplicador.v_usuario
               WHERE obl_id = p_Obl_id ; 
          EXCEPTION   WHEN others   THEN
              lRta := 0;           
              G_Mensaje := G_Mensaje || ' Error al actualizar OBL :' || SQLERRM ;
              RETURN( 0 );          
          END;
       END IF;     
    END;
  RETURN(lRta); 
END; 

FUNCTION GenerarCredito( p_aplicador PKG_RECAUDACIONES.vAplicarRecauRec , p_monto number  )  RETURN number IS
r_importe_iva PKG_SERVICIOS_FIJOS.ImporteRec;
r_iva         PKG_SERVICIOS_FIJOS.IvaRec;
v_Nov_seq          Number;
v_nov_imp_neto     Number;
v_nov_imp_iva_cf   Number;
v_nov_imp_iva_ex   Number;
v_nov_imp_iva_per  Number;
v_nov_imp_iva_ri   Number; 
v_nov_imp_iva_ali  Number;
v_nov_imp_iva_mon  Number;
v_nov_imp_iva_rni  Number;
lRta               Number := 0;
v_con_id           Number;    
l_inm_id           Number; 
l_Tipo_responsable Number;  
Begin  
    Begin  
       Select  inm_id, inm_tipo_responsable  into l_inm_id, l_Tipo_responsable
       From  Manantial.inmuebles 
       where inm_cuenta =  p_aplicador.v_identificacion;
    Exception  When Others then        
	   G_mensaje:=G_mensaje||' err al recuperar INM para credito: '||sqlerrm ;
	   Return(0);
    End;  
    SELECT Manantial.Nov_seq.NEXTVAL  INTO v_Nov_seq    FROM DUAL;
    r_iva         :=PKG_SERVICIOS_FIJOS.f_imp_iva(l_Tipo_Responsable, 650001, SYSDATE);
    r_importe_iva :=F_IVA_INVERSO (p_Monto, r_iva.iva,	r_iva.alicuota, r_iva.percepcion);
    v_nov_imp_neto:= r_importe_iva.neto;    
    IF l_Tipo_Responsable = 1 THEN
        v_nov_imp_iva_cf := r_importe_iva.iva;
    ELSIF l_Tipo_Responsable IN(2,7)  THEN
        v_nov_imp_iva_ex := r_importe_iva.iva;
        IF l_Tipo_Responsable = 7 THEN
           v_nov_imp_iva_per := r_importe_iva.percepcion;
        END IF;
    ELSIF l_Tipo_Responsable IN(4,8)  THEN
        v_nov_imp_iva_ri := r_importe_iva.iva;
        IF l_Tipo_Responsable = 8 THEN
	       v_nov_imp_iva_per := r_importe_iva.percepcion;
        ELSE
           v_nov_imp_iva_ali := r_importe_iva.alicuota;
        END IF;
    ELSIF l_Tipo_Responsable IN(3,9)  THEN
        v_nov_imp_iva_rni := r_importe_iva.iva;
        IF l_Tipo_Responsable = 9 THEN
	       v_nov_imp_iva_per := r_importe_iva.percepcion;
        END IF;
    ELSIF l_Tipo_Responsable = 6  THEN
        v_nov_imp_iva_mon := r_importe_iva.iva;
        v_nov_imp_iva_ali := r_importe_iva.alicuota;
    END IF;
    BEGIN
        SELECT min(con_id) INTO v_con_id
        FROM Manantial.CONEXIONES
        WHERE con_inm_id   = l_inm_id  
              AND con_tipo = 1
              AND con_fec_baja IS NULL
              AND con_Facturable = 'S';
    EXCEPTION WHEN no_data_found THEN 
    	v_con_id := 1;
    END;     
    -----------------------------------------------------------     
    BEGIN
      INSERT INTO Manantial.NOVEDADES_FACTURABLES
             (NOV_ID   ,NOV_PEF_ANIO              , NOV_PEF_PERIODO             ,NOV_SER_CODIGO,NOV_CON_INM_ID,NOV_CON_ID,NOV_TIPO_NOVEDAD,NOV_FEC_NOVEDAD,NOV_ESTADO,NOV_TIPO_ORIGEN,NOV_NRO_ORIGEN               ,NOV_IMP_NETO  ,NOV_IMP_IVA_CF  ,NOV_IMP_IVA_EX  ,NOV_IMP_IVA_RI  ,NOV_IMP_IVA_RNI  ,NOV_IMP_IVA_MON  ,NOV_IMP_IVA_ALI  ,NOV_IMP_IVA_PER  ,NOV_INM_ID , NOV_CLI_ID,NOV_DESCRIPCION, 
              NOV_COD_IVA       ,NOV_USR_ALTA                 ,NOV_FEC_ALTA,NOV_MAC_CODIGO)
	  VALUES (v_Nov_seq,p_aplicador.v_anio,p_aplicador.v_Periodo,650001        ,l_Inm_id   ,v_con_id  ,10              ,Sysdate        ,2         ,'D'            ,p_aplicador.v_factura,v_NOV_IMP_NETO,v_NOV_IMP_IVA_CF,v_NOV_IMP_IVA_EX,v_NOV_IMP_IVA_RI,v_NOV_IMP_IVA_RNI,v_NOV_IMP_IVA_MON,v_NOV_IMP_IVA_ALI,v_NOV_IMP_IVA_PER,l_Inm_id,null   ,'Pago remanente plan especial:'||TO_CHAR(p_aplicador.v_factura),
			  l_Tipo_Responsable,p_aplicador.v_usuario,Sysdate     ,NULL);
	  lRta := v_Nov_seq; 		  
	EXCEPTION WHEN others THEN  
	  lRta := 0;
	  G_mensaje:=G_mensaje||' err al generar credito: '||sqlerrm ; 		  
    END;
    Return (lRta);
End; 
----------------------------------------------------------------------------------------------------------------------------------   

FUNCTION AplicarCapital( p_Cuota_id  number, p_aplicador PKG_RECAUDACIONES.vAplicarRecauRec , p_rec_ID number  )  RETURN number IS
CURSOR   cDeudas (p_ppe_id   number ) IS 
   SELECT rpo_obl_id, rpo_obl_saldo,  obl_saldo, obl_estado  , obl_fec_vencimiento  , obl_id 
   FROM   Manantial.REL_PPE_OBL , Manantial.OBLIGACIONES   
   WHERE  rpo_ppe_id = p_ppe_id  
   AND    rpo_obl_id = obl_id 
   AND    obl_estado = 15 
   ORDER BY obl_fec_vencimiento ;
rDeuda cDeudas%ROWTYPE;       
lRta            Number:=1;  
l_Monto         Number:=0;
l_Capital       number:=0;
l_Plan_Especial number:=0;
l_inm_id        number:=0;  
BEGIN 
   BEGIN 
      SELECT  ppe_id  INTO l_Plan_Especial   
      FROM Manantial.OBLIGACIONES, Manantial.PLANES_ESPECIALES     
      WHERE obl_id = p_Cuota_id 
      AND   ppe_ppl_id = obl_ppl_id;
   EXCEPTION  WHEN others   THEN 
      l_Plan_Especial := 0; 
   END ;
   IF l_plan_especial IS NULL THEN
      l_plan_especial := 0;    
   END IF;
    
   IF l_plan_especial = 0 THEN
      G_Mensaje := G_Mensaje || ' No se encuentra el plan Especial Original ';
      lRta := 0;    
   ELSE
      BEGIN
         SELECT  sum(nvl(cct_imp_debe,0))   INTO  l_Capital  
         FROM Manantial.CUENTAS_CORRIENTES 
         WHERE cct_obl_id = p_Cuota_id 
         AND   cct_ser_codigo = 640109   -- Capital  
         AND   cct_fec_baja IS NULL ;  
      END; 
      IF l_Capital IS NULL THEN 
         l_Capital := 0; 
      END IF; 
      IF l_Capital = 0 THEN    
         G_Mensaje := G_Mensaje || ' La Cuota no incluye CAPITAL ';
      ELSE  
          lRta := l_Capital;
          -- Realizar la aplicacion del importe de capital contenido en la cuota en la deuda incluida en el plan.                    
          FOR rDeuda IN cDeudas(l_Plan_Especial) LOOP
              l_Monto  :=0 ;                
              IF ((l_Capital >= rDeuda.Obl_saldo) AND (l_Capital > 0))  THEN
                 l_Monto  :=rDeuda.Obl_saldo;
                 l_Capital:=l_Capital - l_Monto ; 
              ELSE 
                 l_Monto  :=l_Capital ;
                 l_Capital:=0;     
              END IF;
              if l_Monto > 0 Then 
                 lRta := Aplicar( l_Monto, rDeuda.obl_id, p_Cuota_id,  p_aplicador, p_Rec_ID   );  -- Importe a aplicar, Factura donde aplicar, Nro_plan_pago que se cobro //                
                 if lRta = 0 THEN
                    G_Mensaje := G_Mensaje || ' Error aplicando capital a deudas ';
                    RETURN (0); 
                 End if;     
              End if;
          END LOOP;
          IF l_Capital > 0.10  THEN 
             -- Generar Credito a favor del cliente --
             G_Mensaje := G_Mensaje || ' Existe un saldo a favor de ' || to_char(l_Capital ) ;
             lRta := GenerarCredito(p_aplicador , l_capital);                
          END IF;            
      END IF;   
   END IF;  
   RETURN(lRta);
END; 


----------------------------------------------------------------------------------
FUNCTION InformarRecargos( p_Cuota_id  number, p_Usuario Varchar2 )  RETURN number IS
lRta      Number  :=1;
l_Recargo number  :=0;
l_Iva     number  :=0;
l_Ali     number  :=0;
l_inm_id  number  :=0;
l_cli_id  number  :=0;
l_tipo_iva  number:=0;
l_cuota     Number:=0; 
l_plan_Pago Number:=0; 
l_dev_nov   Number:=0; 
r_novedades  NOVEDADES_FACTURABLES%ROWTYPE;
r_datos_iva  PKG_SERVICIOS_FIJOS.ivaRec;
r_importe    PKG_SERVICIOS_FIJOS.importeRec;
BEGIN 
   BEGIN
      SELECT  sum(nvl(cct_imp_debe,0)) , sum(nvl(cct_imp_iva,0)) ,sum(nvl(cct_imp_ali,0)) ,max(obl_con_inm_id), max(obl_cuota_Plan) , max(obl_cli_id ), max(obl_tre_codigo), Max(obl_ppl_id)   
      INTO  l_Recargo ,l_iva, l_ali, l_inm_id , l_cuota , l_cli_id , l_tipo_iva, l_plan_pago   
      FROM Manantial.CUENTAS_CORRIENTES , Manantial.OBLIGACIONES 
      WHERE cct_obl_id = p_Cuota_id 
      AND   cct_ser_codigo = 640108   -- Recargo   
      AND   cct_obl_id = obl_id 
      AND   cct_fec_baja IS NULL ;  
   END; 
   IF l_Recargo IS NULL THEN 
      l_Recargo := 0; 
   END IF; 
   IF l_Recargo = 0 THEN
      G_Mensaje := G_Mensaje || ' Cuota sin RECARGOS ';
      RETURN(1);  
   END IF;  
   
  
   /* Carga los datos en la variable que se utiliza para insertar en novedades facturables */
   
   r_novedades.nov_pef_anio    := NULL;
   r_novedades.nov_pef_periodo := NULL;
   r_novedades.nov_con_inm_id  := l_inm_id;
   r_novedades.nov_tipo_novedad:= 40;
   r_novedades.nov_fec_novedad:= sysdate;
   r_novedades.nov_estado     := 2;
   r_novedades.nov_tipo_origen:= 'M';
   r_novedades.nov_nro_origen := NVL(l_Cuota, 0);
   r_novedades.nov_imp_cambio := 1;
   r_novedades.nov_inm_id := l_inm_id;
   r_novedades.nov_obl_id := p_Cuota_id;
   r_novedades.nov_cli_id := l_cli_id;
   r_novedades.nov_dpc_id      := NULL;
   r_novedades.nov_fec_destino := NULL;
   r_novedades.nov_tipo_destino:= NULL;
   r_novedades.nov_nro_destino := NULL;
   r_novedades.nov_cod_iva     := l_tipo_iva;
   r_novedades.nov_usr_alta    := p_Usuario;
        --r_novedades.nov_usr_alta := p_aplicarrecaurec.v_usuario;
   r_novedades.nov_fec_alta    := sysdate;
   r_novedades.nov_imp_iva_cf  := 0;
   r_novedades.nov_imp_iva_ex  := 0;
   r_novedades.nov_imp_iva_ri  := 0;
   r_novedades.nov_imp_iva_rni := 0;
   r_novedades.nov_imp_iva_mon := 0;
   r_novedades.nov_imp_iva_ali := 0;
   r_novedades.nov_imp_iva_per := 0;
   r_novedades.nov_imp_neto := l_Recargo - l_Iva - l_Ali ;               
   r_datos_iva := PKG_SERVICIOS_FIJOS.f_imp_iva(l_tipo_iva, 640108,sysdate);
     IF r_datos_iva.iva IS NULL THEN
        G_Mensaje := G_Mensaje || ' Error al determinar IVA ';
        Return(0);
     END IF;

    /* Calcula el IVA correspondiente, en caso que tenga una nueva categoria */
    
   r_importe.iva          := (r_datos_iva.iva * r_novedades.nov_imp_neto) / 100;
   r_importe.alicuota     := (r_datos_iva.alicuota * r_novedades.nov_imp_neto) / 100;
   r_importe.percepcion   := (r_datos_iva.percepcion *(r_novedades.nov_imp_neto + r_importe.iva)) / 100;
   r_novedades.nov_cod_iva:= l_tipo_iva;
      /* Si el codigo de servicio es interes o recargo inserta en
               novedades factureables, para que se genere la factura faltate */
   
   r_novedades.nov_descripcion:='Recargo de Actualiz.  P.de Pago Nro.: '|| LPAD(l_plan_pago, 10, '0')|| ' Cuota Nro.: ' || LPAD(l_Cuota, 2, '0');
   r_novedades.nov_imp_neto   := l_Recargo;
   r_novedades.nov_ser_codigo := 640108;
   l_dev_nov := PKG_SERVICIOS_FIJOS.f_insertar_novedad(r_novedades);

 
   r_novedades.nov_descripcion :='Su Pago Recargos de Act. P.Pago Nro.: '|| LPAD(l_plan_pago, 10, '0')|| ' Cuota Nro.: ' || LPAD(l_Cuota, 2, '0');
   r_novedades.nov_imp_neto :=  (l_Recargo - l_iva - l_ali) * (-1);
   l_dev_nov := PKG_SERVICIOS_FIJOS.f_insertar_novedad(r_novedades);

    
    r_novedades.nov_descripcion :='Su Pago IVA s/Recargos   P.Pago Nro.: ' || LPAD(l_plan_pago, 10, '0') || ' Cuota Nro.: ' || LPAD(l_Cuota, 2, '0');     
    r_novedades.nov_imp_neto :=(l_iva + l_ali)*(-1);
    l_dev_nov := PKG_SERVICIOS_FIJOS.f_insertar_novedad(r_novedades);      
    RETURN(lRta);   
END; 
 
-----------------------------------------------------------------------------------
FUNCTION InformarIntereses( p_Cuota_id  number , p_usuario varchar2 )  RETURN number IS

   ----------------------------------
lRta      Number  :=1;
l_Interes number  :=0;
l_Iva     number  :=0;
l_Ali     number  :=0;
l_inm_id  number  :=0;
l_cli_id  number  :=0;
l_tipo_iva  number:=0;
l_cuota     Number:=0; 
l_plan_Pago Number:=0; 
l_dev_nov   Number:=0; 
r_novedades  NOVEDADES_FACTURABLES%ROWTYPE;
r_datos_iva  PKG_SERVICIOS_FIJOS.ivaRec;
r_importe    PKG_SERVICIOS_FIJOS.importeRec;
BEGIN 
   BEGIN
      SELECT  sum(nvl(cct_imp_debe,0)) , sum(nvl(cct_imp_iva,0)) ,sum(nvl(cct_imp_ali,0)) ,max(obl_con_inm_id), max(obl_cuota_Plan) , max(obl_cli_id ), max(obl_tre_codigo), Max(obl_ppl_id)   
      INTO  l_Interes ,l_iva, l_ali, l_inm_id , l_cuota , l_cli_id , l_tipo_iva, l_plan_pago   
      FROM Manantial.CUENTAS_CORRIENTES , Manantial.OBLIGACIONES 
      WHERE cct_obl_id = p_Cuota_id 
      AND   cct_ser_codigo = 640106   -- Intereses   
      AND   cct_obl_id = obl_id 
      AND   cct_fec_baja IS NULL ;  
   END; 
   IF l_Interes IS NULL THEN 
      l_Interes := 0; 
   END IF; 
   IF l_Interes = 0 THEN
      G_Mensaje := G_Mensaje || ' Cuota sin INTERESES ';
      RETURN(1);  
   END IF;  
   
  
   /* Carga los datos en la variable que se utiliza para insertar en novedades facturables */
   
   r_novedades.nov_pef_anio := NULL;
   r_novedades.nov_pef_periodo := NULL;
   r_novedades.nov_con_inm_id := l_inm_id;
   r_novedades.nov_tipo_novedad := 40;
   r_novedades.nov_fec_novedad := sysdate;
   r_novedades.nov_estado := 2;
   r_novedades.nov_tipo_origen := 'M';
   r_novedades.nov_nro_origen := NVL(l_Cuota, 0);
   r_novedades.nov_imp_cambio := 1;
   r_novedades.nov_inm_id := l_inm_id;
   r_novedades.nov_obl_id := p_Cuota_id;
   r_novedades.nov_cli_id := l_cli_id;
   r_novedades.nov_dpc_id      := NULL;
   r_novedades.nov_fec_destino := NULL;
   r_novedades.nov_tipo_destino:= NULL;
   r_novedades.nov_nro_destino := NULL;
   r_novedades.nov_cod_iva     := l_tipo_iva;
   r_novedades.nov_usr_alta    := p_Usuario;
   r_novedades.nov_fec_alta    := sysdate;
   r_novedades.nov_imp_iva_cf  := 0;
   r_novedades.nov_imp_iva_ex  := 0;
   r_novedades.nov_imp_iva_ri  := 0;
   r_novedades.nov_imp_iva_rni := 0;
   r_novedades.nov_imp_iva_mon := 0;
   r_novedades.nov_imp_iva_ali := 0;
   r_novedades.nov_imp_iva_per := 0;
   r_novedades.nov_imp_neto := l_Interes - l_Iva - l_Ali ;               
   r_datos_iva := PKG_SERVICIOS_FIJOS.f_imp_iva(l_tipo_iva, 640106,sysdate);
   IF r_datos_iva.iva IS NULL THEN
      G_Mensaje := G_Mensaje || ' Error al determinar IVA ';
      Return(0);
   END IF;

    /* Calcula el IVA correspondiente, en caso que tenga una nueva categoria */
    
   r_importe.iva          := (r_datos_iva.iva * r_novedades.nov_imp_neto) / 100;
   r_importe.alicuota     := (r_datos_iva.alicuota * r_novedades.nov_imp_neto) / 100;
   r_importe.percepcion   := (r_datos_iva.percepcion *(r_novedades.nov_imp_neto + r_importe.iva)) / 100;
   r_novedades.nov_cod_iva:= l_tipo_iva;
   
   r_novedades.nov_descripcion:='Interes de Financ. Plan de Pago Nro.: '|| LPAD(l_plan_pago, 10, '0')|| ' Cuota Nro.: ' || LPAD(l_Cuota, 2, '0');
   r_novedades.nov_imp_neto   := l_Interes;
   r_novedades.nov_ser_codigo := 640106;
   l_dev_nov := PKG_SERVICIOS_FIJOS.f_insertar_novedad(r_novedades);
 
   r_novedades.nov_descripcion :='Su Pago Intereses  Plan de Pago Nro.: '|| LPAD(l_plan_pago, 10, '0')|| ' Cuota Nro.: ' || LPAD(l_Cuota, 2, '0');
   r_novedades.nov_imp_neto :=  (l_Interes - l_iva - l_ali) * (-1);
   l_dev_nov := PKG_SERVICIOS_FIJOS.f_insertar_novedad(r_novedades);
    
   r_novedades.nov_descripcion :='Su Pago IVA s/Inte.Plan de Pago Nro.: ' || LPAD(l_plan_pago, 10, '0') || ' Cuota Nro.: ' || LPAD(l_Cuota, 2, '0');     
   r_novedades.nov_imp_neto :=(l_iva + l_ali)*(-1);
   l_dev_nov := PKG_SERVICIOS_FIJOS.f_insertar_novedad(r_novedades);      
   RETURN(lRta);   
END; 

-------------------------------------------------
FUNCTION SeleccionarCuota(p_Plan Number) RETURN number  IS
/*  Esta funcion es la que determina el criterio de cual cuota seleccionar del plan de pago.
Ahora, toma la mas antigua  de las cuotas.   
*/ 
lRta Number :=0;  
l_Cuota  Number := 0; 
CURSOR cPlan(p_plan number ) IS  
       SELECT obl_id , obl_cuota_plan  FROM  Manantial.OBLIGACIONES  
       WHERE obl_ppl_id = p_plan 
       AND    obl_tpc_codigo IN (8,9)
       AND    obl_estado = 15 
       AND    obl_saldo > 0        
       ORDER BY  obl_fec_vencimiento asc ; 
rPlan cPlan%ROWTYPE;
BEGIN 
   OPEN cPlan(p_Plan);
   FETCH cPlan INTO rPlan;
   IF    cPlan%FOUND THEN
        lRta   := rPlan.Obl_id;
        l_Cuota:= rPlan.Obl_Cuota_plan;
   END IF;   
   CLOSE cPlan; 
   --G_mensaje:=G_mensaje||' Aplic en Cuota('||to_char(l_cuota)||') Obl_id:'|| to_char(lRta) ;
   G_mensaje:=G_mensaje||' Detecta Cuota('||to_char(l_cuota)||') Obl_id:'|| to_char(lRta) ;
   RETURN(lRta);
END; 
------------------------------------------------
FUNCTION  ControlImportes(p_obl_id number, p_Importe number  ) RETURN number IS
lRta   Number := 0 ;  
nSaldo Number := 0; 
BEGIN
   BEGIN 
      SELECT obl_saldo INTO nSaldo  
      FROM  manantial.OBLIGACIONES 
      WHERE obl_id = p_obl_id   
      AND   obl_estado  = 15  
      AND   obl_tpc_codigo IN (8,9);
    EXCEPTION WHEN others THEN  
      nSaldo := 0;          
      G_mensaje:=G_mensaje||' ControlImporte():'|| SQLERRM  ;
   END; 
   IF  nSaldo IS NULL THEN  
       nSaldo := 0; 
       G_mensaje:=G_mensaje||' ControlImporte(): NO_DATA_FOUND ' ;
   END IF; 
   IF  abs(nSaldo-p_Importe) <= 2 THEN
       lRta := 1;         
   ELSE 
       G_mensaje:= G_mensaje || ' ControlImporte(): Diferencia EN IMPORTES='|| to_char(abs(nSaldo-p_Importe))  ;    
   END IF; 
   RETURN(lRta); 
END; 
-----------------------------------------------------------------------------------------------------------
FUNCTION PagarCuota(p_ppl number, p_Rec PKG_RECAUDACIONES.vIncRecaudacionRec,p_Aplic PKG_RECAUDACIONES.vAplicarRecauRec,  p_Operacion number, p_Secuencia number ) RETURN number IS
/* Aplica el pago sobre la cuota del plan de pago . 
Luego, del total de Capital que contiene la cuota, aplica sobre las  facturas incluidas en el plan de pago.
si la  cuota contiene recargos o Intereses, genera las  novedades facturables para esos importes 
*/
l_Rec PKG_RECAUDACIONES.vAplicarRecauRec := p_Aplic;
lRta       number :=0 ; 
l_plan     number := p_Rec.v_Factura;
l_Cuota_id Number;   
l_Rec_id   Number; 
BEGIN
    l_Cuota_id:= SeleccionarCuota(p_ppl);
    If l_Cuota_id = 0 then
        RETURN(0);   -- no hay cuotas  disponibles  
    end if; 
    lRta := ControlImportes(l_Cuota_Id,p_Rec.v_importe);  -- Importe con diferencias menores de $2 
    
    IF lRta > 0  THEN        
       l_rec.p_erc_codigo := 1 ;  -- REC_ERC_CODIGO       
       lRta  := AplicarCuota(l_Rec, p_Operacion, p_Secuencia, l_Cuota_id);       
    END IF; 
    
    IF lRta > 0  THEN
       l_Rec_ID:= lRta ; 
       lRta    := AplicarCapital(l_Cuota_id, l_Rec, l_Rec_ID ) ; 
    END IF;
      
    IF lRta > 0  THEN
       lRta := InformarRecargos(l_Cuota_id, p_rec.v_usuario) ; 
    END IF;
    
    IF lRta > 0  THEN
       lRta := InformarIntereses(l_Cuota_id, p_rec.v_usuario) ; 
    END IF;
    IF lRta > 0  THEN
       -- Verificar que  existan cuotas para pagar, sino, cambiar estado del plan a PAGADO / CANCELADO
       l_Cuota_id:= SeleccionarCuota(p_ppl);
       If l_Cuota_id = 0 then
          -- YA no hay mas cuotas disponibles -- 
          Begin
              Update Manantial.planes_pago 
                     set ppl_estado = 2      -- Se termino de pagar 
                     Where ppl_id = p_ppl ;
               G_mensaje:=G_mensaje||' Plan TERMINADO ' ;        
          End;   
       end if;
    end if;     
    RETURN(lRta);
END; 

------------------------------------------------
FUNCTION RecAcreditada(p_ppl number , p_Rec PKG_RECAUDACIONES.vIncRecaudacionRec, p_Operacion number, p_Secuencia number ) RETURN number IS
/* generar un registro en recaudaciones con estado = 2 */
lRta        number:=1;
v_rec_seq   number  := 0;    
v_nov_seq   number  := 0;
l_inmuebles Varchar2(14);
l_cliente   number(10):= 0;
l_obligacion  number; 
l_tipo_responsable Number ; 
v_fecha_insertada Date; 
p_increcaudacionRec PKG_RECAUDACIONES.vIncRecaudacionRec := p_Rec ; 
v_fecha_acred Date;
r_importe_iva PKG_SERVICIOS_FIJOS.ImporteRec;
r_iva PKG_SERVICIOS_FIJOS.IvaRec;
v_nov_IMP_neto     NUMBER(15,2) := 0;
v_nov_IMP_IVA_CF   NUMBER(15,2) := 0;
v_nov_IMP_IVA_EX   NUMBER(15,2) := 0;
v_nov_IMP_IVA_RI   NUMBER(15,2) := 0;
v_nov_IMP_IVA_RNI  NUMBER(15,2) := 0;
v_nov_IMP_IVA_MON  NUMBER(15,2) := 0;
v_nov_IMP_IVA_ALI  NUMBER(15,2) := 0;
v_nov_IMP_IVA_PER  NUMBER(15,2) := 0;
v_con_id           NUMBER       := 0;
BEGIN  
    p_increcaudacionRec.p_estado := 2;    
    SELECT Manantial.rec_seq.NEXTVAL   INTO v_rec_seq    FROM DUAL;    
    v_fecha_insertada := NULL;
    l_obligacion      := NULL;
    l_inmuebles       := NULL;    
    v_fecha_acred     := NULL;
    BEGIN
        SELECT inm_id , inm_tipo_responsable INTO l_inmuebles ,l_tipo_responsable 
        FROM Manantial.INMUEBLES
        WHERE inm_cuenta = p_increcaudacionrec.v_identificacion;
    EXCEPTION WHEN OTHERS THEN
        l_inmuebles := NULL;
    END;  
    BEGIN
        SELECT obl_cli_id , obl_id  INTO l_cliente , l_obligacion   
        FROM Manantial.Obligaciones
        where  obl_id = ( Select min(obl_id)  from Manantial.Obligaciones  
                          WHERE obl_ppl_id = p_ppl   
                          AND   obl_tpc_codigo IN (8,9) 
                        );        
    EXCEPTION WHEN OTHERS THEN
        l_cliente   := 0;
        l_obligacion:= 0; 
    END;  
              
    BEGIN
        SELECT NVL(rem_fec_acreditacion, rem__fecha)   INTO v_fecha_acred
        FROM Manantial.REMESAS
        WHERE rem__numero = p_increcaudacionRec.v_remesa_osm;
    EXCEPTION WHEN NO_DATA_FOUND THEN
        v_fecha_acred := NULL;
    END;
    IF p_increcaudacionRec.p_banco IS NULL THEN
       p_increcaudacionRec.p_banco :=  p_increcaudacionRec.p_ente;
    END IF; 
    BEGIN 
      INSERT INTO Manantial.RECAUDACIONES
                  (rec_id, rec_vco_codigo,rec_inm_id , rec_obl_id,  rec_erc_codigo, rec_cuenta, rec_fecha, rec_imp_cobrado, rec_nro_factura, rec_anio_periodo, rec_mes_periodo,
                   rec_se, rec_usr_alta , rec_fec_alta,rec_fec_aplicacion, rec_fec_proceso, rec_fec_contable, rec_banco, rec_secuencia, rec_usr_baja, rec_fec_baja,
                   rec_usr_mod, rec_fec_mod, rec_nro_operacion, rec_rem_numero, rec_rem_ere_codigo, rec_remesa, rec_fec_acredita, rec_clave_heredada, rec_lote)
           VALUES (v_rec_seq, p_increcaudacionrec.p_ente, l_inmuebles, l_obligacion, p_increcaudacionrec.p_estado, p_increcaudacionrec.v_identificacion,
                   p_increcaudacionrec.v_fecha_cobro, p_increcaudacionrec.v_importe, p_increcaudacionrec.v_factura, p_increcaudacionrec.v_anio, p_increcaudacionrec.v_periodo,
                   p_increcaudacionrec.p_se, p_increcaudacionrec.v_usuario, SYSDATE, v_fecha_insertada, SYSDATE,  sysdate  ,
                   p_increcaudacionrec.p_banco, p_Secuencia, NULL, NULL,  NULL, NULL, p_Operacion, p_increcaudacionrec.v_remesa_osm, p_increcaudacionrec.p_ente,p_increcaudacionrec.v_remesa, v_fecha_acred, NULL, NULL);
    EXCEPTION WHEN Others THEN  
        lRta := 0;         
        G_mensaje:=G_mensaje||' '||sqlerrm ;             
    END; 
    
        SELECT Manantial.Nov_seq.NEXTVAL  INTO v_Nov_seq    FROM DUAL;
        r_iva         :=PKG_SERVICIOS_FIJOS.f_imp_iva(l_Tipo_Responsable, 650001, SYSDATE);
        r_importe_iva :=F_IVA_INVERSO (p_increcaudacionrec.v_importe, r_iva.iva,	r_iva.alicuota, r_iva.percepcion);
        v_nov_imp_neto:= r_importe_iva.neto;    
        IF l_Tipo_Responsable = 1 THEN
           v_nov_imp_iva_cf := r_importe_iva.iva;
        ELSIF l_Tipo_Responsable IN(2,7)  THEN
           v_nov_imp_iva_ex := r_importe_iva.iva;
           IF l_Tipo_Responsable = 7 THEN
              v_nov_imp_iva_per := r_importe_iva.percepcion;
           END IF;
        ELSIF l_Tipo_Responsable IN(4,8)  THEN
           v_nov_imp_iva_ri := r_importe_iva.iva;
           IF l_Tipo_Responsable = 8 THEN
	          v_nov_imp_iva_per := r_importe_iva.percepcion;
           ELSE
              v_nov_imp_iva_ali := r_importe_iva.alicuota;
           END IF;
        ELSIF l_Tipo_Responsable IN(3,9)  THEN
           v_nov_imp_iva_rni := r_importe_iva.iva;
           IF l_Tipo_Responsable = 9 THEN
	          v_nov_imp_iva_per := r_importe_iva.percepcion;
           END IF;
        ELSIF l_Tipo_Responsable = 6  THEN
           v_nov_imp_iva_mon := r_importe_iva.iva;
           v_nov_imp_iva_ali := r_importe_iva.alicuota;
        END IF;
        BEGIN
            SELECT min(con_id) INTO v_con_id
            FROM Manantial.CONEXIONES
            WHERE con_inm_id   = l_inmuebles 
                  AND con_tipo = 1
                  AND con_fec_baja IS NULL
                  AND con_Facturable = 'S';
        EXCEPTION WHEN no_data_found THEN 
    	          v_con_id := 1;
        END;     
     -----------------------------------------------------------     
    BEGIN
      INSERT INTO Manantial.NOVEDADES_FACTURABLES
             (NOV_ID   ,NOV_PEF_ANIO              , NOV_PEF_PERIODO             ,NOV_SER_CODIGO,NOV_CON_INM_ID,NOV_CON_ID,NOV_TIPO_NOVEDAD,NOV_FEC_NOVEDAD,NOV_ESTADO,NOV_TIPO_ORIGEN,NOV_NRO_ORIGEN               ,NOV_IMP_NETO  ,NOV_IMP_IVA_CF  ,NOV_IMP_IVA_EX  ,NOV_IMP_IVA_RI  ,NOV_IMP_IVA_RNI  ,NOV_IMP_IVA_MON  ,NOV_IMP_IVA_ALI  ,NOV_IMP_IVA_PER  ,NOV_INM_ID , NOV_CLI_ID,NOV_DESCRIPCION, 
              NOV_COD_IVA       ,NOV_USR_ALTA                 ,NOV_FEC_ALTA,NOV_MAC_CODIGO)
	  VALUES (v_Nov_seq,p_increcaudacionrec.v_anio,p_increcaudacionrec.v_Periodo,650001        ,l_Inmuebles   ,v_con_id  ,10              ,Sysdate        ,2         ,'D'            ,p_increcaudacionrec.v_factura,v_NOV_IMP_NETO,v_NOV_IMP_IVA_CF,v_NOV_IMP_IVA_EX,v_NOV_IMP_IVA_RI,v_NOV_IMP_IVA_RNI,v_NOV_IMP_IVA_MON,v_NOV_IMP_IVA_ALI,v_NOV_IMP_IVA_PER,l_Inmuebles,l_cliente  ,'Pago duplicado Plan especial :'||TO_CHAR(p_increcaudacionrec.v_factura),
			  l_Tipo_Responsable,p_increcaudacionrec.v_usuario,Sysdate     ,NULL);
	EXCEPTION WHEN others THEN  
	  lRta := 0;
	  G_mensaje:=G_mensaje||' '||sqlerrm ; 		  
    END;
    BEGIN 
       INSERT INTO  Manantial.REL_REC_NOV(rrn_id,rrn_rec_id,rrn_nov_id, rrn_fec_alta,rrn_usr_alta) 
               VALUES (Manantial.rrn_seq.NEXTVAL,v_rec_seq ,v_Nov_seq , sysdate     ,p_increcaudacionrec.v_usuario);               
    EXCEPTION WHEN others THEN  
       lRta := 0;  
       G_mensaje:=G_mensaje||' '||sqlerrm ;          
    END;     
    G_mensaje:=G_mensaje||' Importe Acreditado : '||to_char(p_increcaudacionrec.v_importe) ;
    RETURN(lRta);
END; 
-------------------------------------------------------------------------------------------
Function ControlPlan(p_ppe_id number) return boolean is
l_quita number :=0; 
l_Cuota number :=0;
Begin 
    Begin 
        Select   ppe_quita   into   l_quita 
        from Manantial.Planes_especiales  
        Where ppe_id = p_ppe_id ; 
    Exception  When others   then
        l_quita := 0;  
    end;     
    if l_quita is null then
       l_quita := -1;  
    end if; 
    If l_quita > 0 then   -- el plan tiene quita de capital  -- No lo procesa esta  rutina !!!!
       G_mensaje:=G_mensaje||' plan con quitas' ;    
       Return(False);
    else      
       Return(True);      
    end if;    
End;  
----------------------------------------------------------------------------------------------
Function Generar_Obligacion(p_nro_cuota number,p_fec_cuota date, p_anio number, p_periodo number, p_imp_cuota number, p_ppl_id number,p_mpp_id number,p_inm_id number, p_cli_id  number, p_usuario varchar2 ) return number is
l_id number;  
l_cuenta varchar2(14); 
l_dom_id number;  
l_grupo  number;
l_iva    number; 
l_neto   number; 
registro1  PKG_SERVICIOS_FIJOS.ivarec;
r_importe  PKG_SERVICIOS_FIJOS.ImporteRec;
Begin
   Begin 
   Select  inm_cuenta , MANANTIAL.Det_Postal_ID(inm_id), inm_grp_codigo , inm_tipo_responsable   
           into l_cuenta  , l_dom_id , l_grupo , l_iva 
           From Manantial.Inmuebles 
           where inm_id = p_inm_id;
    Exception when others then
        G_mensaje:=G_mensaje||' Error para recuperar datos del INM:' || sqlerrm  ;
        Return(0);       
   end ;     
    registro1:= PKG_SERVICIOS_FIJOS.F_IMP_IVA (l_Iva , 640110 , SYSDATE );
    registro1.iva       := NVL(registro1.iva       ,0);
    registro1.alicuota  := NVL(registro1.alicuota,  0);
    registro1.percepcion:= NVL(registro1.percepcion,0);
    r_importe := F_IVA_INVERSO(p_imp_cuota,registro1.iva,registro1.alicuota,registro1.percepcion);
    l_neto := r_importe.neto;    
   Select  Manantial.Obl_seq.NextVal into l_id From dual ;
   Begin  
   Insert into Manantial.Obligaciones(OBL_PEF_ANIO,OBL_PEF_PERIODO,OBL_FEC_VENCIMIENTO,OBL_IMP_ORIGINAL,OBL_SALDO  ,OBL_IMP_NETO,OBL_ID,OBL_CLI_ID,OBL_CON_INM_ID,OBL_FEC_GENERACION,OBL_ESTADO,OBL_NRO_FACTURA,OBL_USR_ALTA,OBL_FEC_ALTA,OBL_PPL_ID,OBL_INM_ID,OBL_TIPO_PLAN, OBL_cuota_plan,OBL_IMP_IVA_EXE,OBL_IMP_IVA_RI,OBL_IMP_IVA_RNI,OBL_IMP_IVA_MON ,OBL_SE,OBL_FEC_CONTABLE,OBL_POR_IVA_EXE,OBL_POR_IVA_RI,OBL_POR_IVA_RNI,OBL_POR_IVA_MON,OBL_IMP_IVA_CF,OBL_IMP_ALI_RNI,OBL_IMP_ALI_NO_CAT,OBL_POR_IVA_CF,OBL_ALI_RNI,OBL_ALI_NO_CAT,OBL_TPC_CODIGO,OBL_DOM_ID,OBL_Cuenta,OBL_SUC_CODIGO      ,OBL_GRP_CODIGO,OBL_TRE_CODIGO ) 
                              Values (p_anio      ,p_Periodo      ,p_fec_Cuota        ,p_imp_cuota     ,p_imp_cuota,l_neto      ,l_id  ,  p_cli_id,p_inm_id      ,Sysdate           , 15       ,p_ppl_id       , p_Usuario  ,Sysdate     ,p_ppl_id  ,p_inm_id  ,p_mpp_id     , p_nro_cuota   ,0              ,0             ,0              ,0               ,'N'   ,Sysdate         ,0              ,0             ,0              ,0              ,0             ,0              ,0                 ,0             ,0          ,0             ,8             ,l_dom_id  ,l_cuenta  ,substr(l_cuenta,1,3),l_grupo       , l_iva );   

   Exception when others then
      G_mensaje:=G_mensaje||' Error al insertar cuota (OBL) de plan:' || sqlerrm  ;
      Return(0);
   end;  
   Return (l_Id) ;  
End;
----------------------------------------------------------------------------------------------
Function Generar_CCT(p_Obl_id number ,p_servicio number, p_importe number) return number is
Cursor cObl is 
   Select  Obl_pef_anio,Obl_pef_Periodo,Obl_tpc_codigo,Obl_fec_Generacion,Obl_usr_alta,Obl_fec_alta,Obl_grp_codigo,Obl_cuenta,Obl_suc_codigo,Obl_tre_codigo  
           From Manantial.Obligaciones 
   where obl_id = p_obl_id; 
rObl cObl%RowType;    
l_id Number ;
l_Descripcion Varchar2(70); 
registro1  PKG_SERVICIOS_FIJOS.ivarec;
r_importe  PKG_SERVICIOS_FIJOS.ImporteRec;
nImpIva number; 
nImpAli number;
nImpPer number; 
Begin
  Begin  
     Select  max(cct_id_movimiento)+1 Into l_Id 
     From Manantial.cuentas_corrientes 
     where cct_obl_id  = p_obl_id ; 
  Exception  When no_data_Found Then
     l_id := 1;
  when others  Then 
     G_mensaje:=G_mensaje||' Error al determinar CCT_ID para cuota del plan:' || sqlerrm  ;
     Return(0);    
 End ; 
 Open  cObl; 
 Fetch cObl into rObl; 
If nvl(l_Id,0) = 0 Then 
   l_Id  := 1; 
end if;        
Select substr(ser_des_larga,1,70) into l_Descripcion   
    From   Manantial.Servicios   where ser_codigo = p_servicio ;
    
   registro1:= PKG_SERVICIOS_FIJOS.F_IMP_IVA (rObl.Obl_tre_codigo,640110,SYSDATE );
   registro1.iva       := NVL(registro1.iva       ,0);
   registro1.alicuota  := NVL(registro1.alicuota,  0);
   registro1.percepcion:= NVL(registro1.percepcion,0);
   r_importe := F_IVA_INVERSO(p_importe,registro1.iva,registro1.alicuota,registro1.percepcion);     
   nImpIva :=ROUND( r_importe.neto * registro1.iva     /100 ,2);
   nImpAli :=ROUND( r_importe.neto * registro1.Alicuota/100 ,2);
   nImpPer :=ROUND( (r_importe.neto+nImpIva) * registro1.Percepcion/100 ,2);
   -- Neto => r_importe.neto ;
  Begin      
     INSERT INTO Manantial.CUENTAS_CORRIENTES (CCT_OBL_ID, CCT_ID_MOVIMIENTO, CCT_SER_CODIGO, CCT_PEF_ANIO    ,CCT_PEF_PERIODO     ,CCT_TIPO_MOVIMIENTO,CCT_IMP_DEBE,CCT_IMP_HABER,CCT_CONCEPTO ,CCT_FEC_GENERACION     ,CCT_USR_ALTA     ,CCT_FEC_ALTA     ,CCT_GRP_CODIGO     ,CCT_CUENTA     ,CCT_SUC_CODIGO     ,CCT_TRE_CODIGO     ,CCT_IMP_IVA,CCT_POR_IVA  ,CCT_IMP_ALI    ,        CCT_POR_ALI                    )
                                       VALUES (p_Obl_id  , l_Id             , p_Servicio    ,rObl.Obl_pef_anio,rObl.Obl_pef_Periodo,8                  ,p_importe   ,0            ,l_Descripcion,rObl.Obl_fec_Generacion,rObl.Obl_usr_alta,rObl.Obl_fec_alta,rObl.Obl_grp_codigo,rObl.Obl_cuenta,rObl.Obl_suc_codigo,rObl.Obl_tre_codigo,nImpIva    ,registro1.iva,nImpAli+nImpPer,registro1.Alicuota+registro1.Percepcion);
  Exception  When Others then  
     G_mensaje:=G_mensaje||' Error al insertar cuota (CCT) de plan:' || sqlerrm  ;
     Return(0);                               
  End;
  Return (p_Obl_id); 
End; 
----------------------------------------------------------------------------------------------                     
Function GenerarPlan(p_inserta_recaud PKG_RECAUDACIONES.vIncRecaudacionRec ) return number is
 Cursor cPlanEspecial (p_plan number) Is 
      Select  * 
         from  Manantial.Planes_especiales, Manantial.Modelos_planes_Pago  
      where ppe_id     = p_Plan
      and   ppe_mpp_id = mpp_id;        
 rPlan  cPlanEspecial%RowType;
 Cursor cCuotas(p_plan number) Is 
     Select * from   Manantial.planes_especiales_cuotas
     where  ppc_ppe_id = p_plan 
     order by   ppc_nro_cuota ; 
 rCuota cCuotas%RowType;
 Cursor cDeudas (p_ppe_id   number ) IS 
   Select obl_id  
   From   Manantial.REL_PPE_OBL , Manantial.OBLIGACIONES   
   Where  rpo_ppe_id = p_ppe_id  
   and    rpo_obl_id = obl_id 
   and    obl_estado = 15 ;
rDeuda cDeudas%ROWTYPE;
 lRta   Number := 0;  
 n_Temp Number ; 
 n_Usuario        Varchar2(20) := p_inserta_recaud.v_Usuario;
 l_Obl_id         Number :=0 ;
 n_capitalCuotas  Number :=0 ; 
 n_Diff_capital   Number :=0 ;
 n_Demora         Number:= 0; 
 Begin 
    Open cPlanEspecial( p_inserta_recaud.v_Factura ) ;  -- NRO PLAN DE PAGO ESPECIAL
    fetch cPlanEspecial into rPlan ;
    if    cPlanEspecial%NotFound then
      G_mensaje:=G_mensaje||' no Existe el plan especial:'|| to_char(p_inserta_recaud.v_Factura);
      Return  0; 
    End if;      
    /* -------------- Control y Ajuste del plan de pago especial y de sus cuotas -----------------
         Se asigna 60 dias de la fecha de creacion del plan de pago especial hasta el pago de la primer cuota. 
         No se calcular recargos, y las fechas de  vencimiento del resto de las cuotas de ajusta hacia adelante a partir de la 
         fecha de pago de la cuota uno. 
    */    
    
    ---- Reajuste de las  fechas de Vencimiento de las cuotas ---         
        
    Begin   
        Update  Manantial.planes_especiales_cuotas
        Set    ppc_fec_cuota = decode(ppc_nro_cuota,1,rPlan.PPE_FECHA+60,rPlan.PPE_FECHA+30+(30*ppc_nro_cuota))  
        Where  ppc_ppe_id    = p_inserta_recaud.v_Factura;
      --  G_mensaje:=G_mensaje||' Ajusta Vto x'|| to_char(n_Demora) || ' dias' ;
    exception    when others then  
        G_mensaje:=G_mensaje||' NO-AJUSTABLE VTO' ;
    end;
                 
    rPlan.Ppe_primer_vto := rPlan.PPE_FECHA+60;   
     
    -- Insert del plan de pago Definitivo  --
    Select  Manantial.ppl_seq.NextVal into n_temp from dual; 
    Begin   
        Insert into  Manantial.Planes_Pago(PPL_ID, PPL_MPP_ID  , PPL_INM_ID     ,PPL_CLI_ID      ,PPL_DEUDA_HISTORICA      ,PPL_MONTO_RECARGOS       , PPL_IMP_CUOTA     ,PPL_FECHA, PPL_CNT_CUOTAS     ,PPL_PRIMER_VTO      , PPL_QUITA     ,PPL_TASA_INTERES      , PPL_TASA_RECARGOS    , PPL_TASA_BONIFICACIONES     , PPL_IMP_INTERESES     ,PPL_ESTADO,PPL_FEC_CADUCIDAD      ,PPL_MODALIDAD      , PPL_USR_ALTA,PPL_FEC_ALTA,PPL_BONIF_RECARGO      ,PPL_VTO_PAGO_INICIAL      , PPL_MONTO_PAGO_INICIAL     , PPL_INT_PAGO_INICIAL     ,PPL_MAO_CODIGO) 
                                   Values (n_temp, rPlan.mpp_id,rPlan.ppe_inm_id,rPlan.ppe_cli_id,rPlan.ppe_Deuda_Historica,rPlan.ppe_Monto_Recargos,rPlan.ppe_imp_cuota,Sysdate  ,rPlan.ppe_cnt_cuotas,rPlan.ppe_primer_vto,rPlan.ppe_Quita,rPlan.mpp_Tasa_Interes,rPlan.mpp_Tasa_Recargo,rPlan.ppe_Tasa_Bonificaciones,rPlan.ppe_imp_intereses,1         ,rPlan.ppe_fec_caducidad,rPlan.ppe_Modalidad,n_Usuario    ,sysdate     ,rPlan.ppe_bonif_Recargo,rPlan.Ppe_vto_pago_inicial,rPlan.ppe_Monto_pago_inicial,rPlan.ppe_int_pago_inicial,rPlan.mpp_mao_codigo  );
    Exception When Others then  
        G_mensaje:=G_mensaje||' Error en Insert de PLANES_PAGO :'|| sqlerrm ;
        Return 0;                                 
    End;
     --- Marca obligaciones incluidas en el plan de pago. 
     for rDeuda in cDeudas(p_inserta_recaud.v_Factura) Loop
         Update Manantial.Obligaciones 
             Set obl_ppl_id= n_temp 
             Where obl_id  = rDeuda.obl_id; 
     end loop; 
     ------- Control del cuadratura del Plan de pago y sus cuotas ----
     n_capitalCuotas:=0 ;
     For rCuota in cCuotas(p_inserta_recaud.v_Factura) loop
         n_capitalCuotas:=n_capitalCuotas + rCuota.ppc_capital ; 
     end loop; 
     n_Diff_capital:= rPlan.ppe_Deuda_Historica - n_capitalCuotas ;
     if n_Diff_capital < 0.2 Then   -- Ajustamos por diferencias mayores a  $0.2 
        n_Diff_capital :=0; 
     end if;   
     -----------------------------------------------------------------
     -- Insert de Cuotas del plan de pagos --
    For rCuota in cCuotas(p_inserta_recaud.v_Factura) loop
        --- Si hay diferencias, ajusto en la ultima cuota del plan -------
        if n_Diff_capital > 0 and  rCuota.ppc_nro_cuota = rPlan.ppe_cnt_cuotas Then
           rCuota.ppc_capital  := rCuota.ppc_capital   + n_Diff_capital ;
           rCuota.ppc_intereses:= rCuota.ppc_intereses - n_Diff_capital ;
           G_mensaje:=G_mensaje||' Ajusta capital cuota :'|| to_char(rPlan.ppe_cnt_cuotas) || ' por $:' || ltrim(to_char(n_Diff_capital,'99990D00')) ;
        end if; 
        l_Obl_id := Generar_Obligacion(rCuota.ppc_nro_cuota, rCuota.ppc_fec_cuota,rCuota.ppc_pef_anio,rCuota.ppc_pef_periodo,rCuota.ppc_imp_cuota,n_temp, rPlan.mpp_id,rPlan.ppe_inm_id,rPlan.ppe_cli_id ,n_Usuario);
        If l_Obl_id = 0 Then
           Return 0; 
        end if;   
        l_obl_id := Generar_CCT(l_Obl_id,640106,rCuota.ppc_intereses);
        If l_Obl_id = 0 Then
           Return 0; 
        end if;
        l_obl_id := Generar_CCT(l_Obl_id,640108,rCuota.ppc_recargos );
        If l_Obl_id = 0 Then
           Return 0; 
        end if;
        l_obl_id := Generar_CCT(l_Obl_id,640109,rCuota.ppc_capital  );
        If l_Obl_id = 0 Then
           Return 0; 
        end if;  
    End loop;
    lRta := n_temp;   -- Plan de pago definitivo que se  genero.     
/*                                  
       UPDATE Manantial.OBLIGACIONES  
       SET obl_imp_original = (SELECT ABS(SUM(cct_imp_debe-cct_imp_haber)) FROM Manantial.CUENTAS_CORRIENTES WHERE cct_obl_id = obl_id and cct_ser_codigo <> 800088 ),
             obl_saldo      = (SELECT ABS(SUM(cct_imp_debe-cct_imp_haber)) FROM Manantial.CUENTAS_CORRIENTES WHERE cct_obl_id = obl_id),,
             obl_imp_neto   = (SELECT ABS(SUM(DECODE(cct_imp_debe,0,(cct_imp_haber-cct_imp_iva-cct_imp_ali)*-1,cct_imp_debe-cct_imp_iva-cct_imp_ali))) FROM Manantial.CUENTAS_CORRIENTES WHERE cct_obl_id = obl_id)             
       WHERE obl_id = rObl.Obl_id;    
*/   
     Begin 
        Update  Manantial.Planes_Especiales  
        Set     ppe_ppl_id = n_Temp ,
                ppe_generado = 'S' ,
                ppe_fec_mod  = sysdate ,
                ppe_usr_mod  = n_Usuario 
        Where   ppe_id = p_inserta_recaud.v_Factura;
     Exception  When Others  Then      
        G_mensaje:=G_mensaje||' Error al vincular PPE <--> PPL :'|| sqlerrm ;
        lRta :=0; 
     End;           
   Return lRta ; 
 End;                   
 -------------------------------------------------------------------------

FUNCTION Control_ppe(r_inserta_recaud IN OUT PKG_RECAUDACIONES.vIncRecaudacionRec, pOperacion number, pSecuencia number ) RETURN BOOLEAN IS
/*   -- definicion de la estructura de la variable que se usa de ENTRADA/SALIDA   --
 TYPE vIncRecaudacionRec IS RECORD(
      p_estado           RECAUDACIONES.rec_erc_codigo%TYPE,
      p_se               RECAUDACIONES.rec_se%TYPE,
      p_ente             RECAUDACIONES.rec_rem_ere_codigo%TYPE,
      p_fecha            DATE,
      v_importe          OBLIGACIONES.obl_imp_original%TYPE,
      v_identificacion   OBLIGACIONES.obl_cuenta%TYPE,
      v_fecha_cobro      DATE,
      v_factura          OBLIGACIONES.obl_nro_factura%TYPE,
      v_anio             OBLIGACIONES.obl_pef_anio%TYPE,
      v_periodo          OBLIGACIONES.obl_pef_periodo%TYPE,
      p_banco            CUENTAS_CORRIENTES.cct_banco%TYPE,
      v_remesa           CUENTAS_CORRIENTES.cct_remesa%TYPE,
      v_remesa_osm       REMESAS.rem__numero%TYPE,
      v_usuario          VARCHAR2(15)
   );
    TYPE vAplicarRecauRec IS RECORD(
      p_ente             RECAUDACIONES.rec_rem_ere_codigo%TYPE,
      p_fecha            DATE,
      v_factura          OBLIGACIONES.obl_nro_factura%TYPE,
      v_periodo          OBLIGACIONES.obl_pef_periodo%TYPE,
      v_anio             OBLIGACIONES.obl_pef_anio%TYPE,
      v_identificacion   OBLIGACIONES.obl_cuenta%TYPE,
      v_importe          OBLIGACIONES.obl_imp_original%TYPE,
      v_fecha_cobro      DATE,
      v_cod_factura      VARCHAR2(4),
      v_remesa           CUENTAS_CORRIENTES.cct_remesa%TYPE,
      v_remesa_osm       REMESAS.rem__numero%TYPE,
      p_rec_id           RECAUDACIONES.rec_id%TYPE,
      p_erc_codigo       RECAUDACIONES.rec_erc_codigo%TYPE,
      p_banco            CUENTAS_CORRIENTES.cct_banco%TYPE,
      v_usuario          VARCHAR2(15)  
      );
*/  
l_Vigente    BOOLEAN:= FALSE ; 
l_Procesable BOOLEAN:= FALSE ;
l_EstadoPlan Number := 0; 
l_Ppl        number; 
l_dias_Vto   number:= 365 ;
nRta         Number := 0;  
r_aplicador PKG_RECAUDACIONES.vAplicarRecauRec;
l_Mes        varchar2(2) := to_char(sysdate, 'MM');
BEGIN        
    G_Log:= UTL_FILE.fopen('REPORTES', 'Det_Aplic_'||l_mes||'.log', 'A');    
    G_mensaje := To_char(sysdate, 'DD/MM/RRRR hh24:mi:ss') ||' '||r_inserta_recaud.v_identificacion ||' '||lpad(ltrim(to_char(r_inserta_recaud.v_importe,'9999999D99')),15)||' '||
                  lPad(ltrim(to_char(r_inserta_recaud.v_factura)),10)  || ' ' ||to_char(r_inserta_recaud.p_ente) ; 
    r_aplicador.p_rec_id     := NULL;
    r_aplicador.p_erc_codigo := NULL;      
    r_aplicador.p_ente       := r_inserta_recaud.p_ente; 
    r_aplicador.v_remesa_osm := r_inserta_recaud.v_remesa_osm;      
    r_aplicador.v_factura    := r_inserta_recaud.v_factura;
    r_aplicador.v_periodo    := r_inserta_recaud.v_periodo;
    r_aplicador.v_anio       := r_inserta_recaud.v_anio;
    r_aplicador.v_identificacion:= r_inserta_recaud.v_identificacion;
    r_aplicador.v_fecha_cobro:= r_inserta_recaud.v_fecha_cobro;
    r_aplicador.p_banco      := r_inserta_recaud.p_banco;
    r_aplicador.v_remesa     := r_inserta_recaud.v_remesa;
    r_aplicador.v_importe    := r_inserta_recaud.v_importe;
    r_aplicador.v_Usuario    := r_inserta_recaud.v_Usuario;
   -- Controla que sea   un plan SIN quitas. Caso contrario, NO LO TRATAMOS ACA.
    l_Procesable := ControlPlan(r_inserta_recaud.v_Factura);     
    if l_procesable Then      
       --Determinar si el plan de pago tiene mas de 1 anio de generado y es del tipo correcto --
       l_Vigente:= ControlFecha(r_inserta_recaud.v_Factura, l_dias_Vto);     
       IF  NOT(l_Vigente)  THEN
          G_mensaje:=G_mensaje||' Plan NO-VIGENTE. Pago Pdte';          
          nRta := RecPendiente(r_inserta_recaud, pOperacion, pSecuencia);   --- Generar la  recaudacion en estado = 3           
       ELSE 
          -- debe determinar si el plan definitivo ya esta generado -- 
          l_ppl := ControlPlanGenerado(r_inserta_recaud.v_Factura);
          IF l_ppl > 0 THEN  -- el plan definitivo esta  generado
             G_mensaje:=G_mensaje||' Plan Generado.ppl_id:'|| to_char(l_ppl);  
             l_EstadoPlan := VerEstadoPlan(l_ppl);         
             IF l_EstadoPlan IN (1,4,5)  THEN                                                    -- Plan ACTIVO, Moroso, CAIDO
                G_mensaje:=G_mensaje||' Plan Activo '; 
                nRta := PagarCuota(l_ppl, r_inserta_recaud, r_aplicador, pOperacion, pSecuencia);      
                if nRta = 0 then  
                   G_mensaje:=G_mensaje||' Pago Pdte';
                   nRta := RecPendiente(r_inserta_recaud, pOperacion, pSecuencia);              --- Generar la  recaudacion en estado = 3
                End if; 
             ELSIF  l_EstadoPlan = 2 THEN              
                --G_mensaje:=G_mensaje||' Plan Cancelado Pago Acreditado';                         -- Plan CANCELADO  (SE TERMINO DE PAGAR EL PLAN)  
                G_mensaje:=G_mensaje||' Plan Cancelado Pago Acreditado';                         -- Plan CANCELADO  (SE TERMINO DE PAGAR EL PLAN)
                nRta := RecAcreditada(l_ppl, r_inserta_recaud, pOperacion, pSecuencia);          --- Generar la  recaudacion en estado = 2
             ELSIF  l_EstadoPlan = 3 THEN                                                        -- Plan ANULADO
                G_mensaje:=G_mensaje||' Plan Anulado Pago Pdte'; 
                nRta := RecPendiente(r_inserta_recaud, pOperacion, pSecuencia);                  --- Generar la  recaudacion en estado = 3
             END IF;  
          ELSE       
          -- SINO Generar el plan definitivo  y aplicar pago en el plan cuota 1             
             nRta := GenerarPlan(r_inserta_recaud);         
             G_mensaje:=G_mensaje||' Generar plan Definitvo:'||to_char(nRta) ;
             IF nRta > 0 THEN    
                nRta := PagarCuota(nRta, r_inserta_recaud, r_aplicador,  pOperacion, pSecuencia);
             End if; 
             If nRta = 0 THEN
                G_mensaje:=G_mensaje||' Plan NO Generado  Pago Pdte';          
                nRta := RecPendiente(r_inserta_recaud, pOperacion, pSecuencia);   -- Recuadacion en estado = 3                   
             END IF;   
          END IF;
       END IF;
    End if; 
    if nRta > 0 Then  
       l_Procesable := TRUE;
       --G_MENSAJE := 'Aplicado-' || G_Mensaje;
       If  Upper(G_MENSAJE) like '%PAGO PDTE%' Then
          G_MENSAJE := 'Proc.PDTE-' || G_Mensaje;
       Else 
          G_MENSAJE := 'Procesado-' || G_Mensaje;
       end if; 
      /*  det_ppe(p_cuenta  => r_inserta_recaud.v_identificacion, 
                    p_via     => r_inserta_recaud.p_ente, 
                    p_importe => r_inserta_recaud.v_importe, 
                    p_salida  => 'Det_ppe_'||ltrim(to_char(r_inserta_recaud.p_ente,'999')));
             
      */  
    else       
       G_MENSAJE := 'DERIVADO-' || G_Mensaje;
       l_Procesable := FALSE; 
    end if;     
    UTL_FILE.put_line( G_Log, G_MENSAJE);
    UTL_FILE.fclose(G_LOG); 
    RETURN(l_Procesable);
END; 

END;
/

