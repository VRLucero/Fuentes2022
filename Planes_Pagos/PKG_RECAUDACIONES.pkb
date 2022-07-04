/*<TOAD_FILE_CHUNK>*/
CREATE OR REPLACE PACKAGE MANANTIAL.PKG_RECAUDACIONES IS
-------------- Modificaciones -----------------------------------
--- Modificado por ticket  6926408  VLUCERO  FEB_2014 -------
--- La rutina d calculo de  recargos daba problemas con la fecha fin del ultimo tramo. 
--- Modificado el 08/04/2014    VLUCERO  TCK_7011384   ----------------
 -- Se ajusta para que la NC use numeracion de las  oficinas comerciales ---------
 -- 01/06/2018  Se incorpora el tratamiento de la tarjeta ULTRA (tck 9092470) 
 -- 12/08/2020  Se cambia la rutina de calculo de recargos para que  utilice la nueva metodologia de  cociente de indices acumulados para la tarea. 
------------------------------------------------------------------

   lfile   UTL_FILE.file_type;

   TYPE vincorporacionrec IS RECORD(
      p_ente      RECAUDACIONES_EXTERNAS.rep_ere_codigo%TYPE,
      p_fecha     DATE,
      v_usuario   VARCHAR2(15)
   );

   TYPE vincrecaudacionrec IS RECORD(
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

   TYPE vactuarecaudrec IS RECORD(
      v_identificacion   OBLIGACIONES.obl_cuenta%TYPE,
      v_factura          OBLIGACIONES.obl_nro_factura%TYPE,
      v_anio             OBLIGACIONES.obl_pef_anio%TYPE,
      v_periodo          OBLIGACIONES.obl_pef_periodo%TYPE,
      p_rec_id           RECAUDACIONES.rec_id%TYPE,
      p_erc_codigo       RECAUDACIONES.rec_erc_codigo%TYPE,
      v_usuario          VARCHAR2(15)
   );

   TYPE vaplicarrecaurec IS RECORD(
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

   TYPE registro IS RECORD(
      codigo    NUMBER,
      descrip   VARCHAR2(100)
   );

  -- FUNCTION recp_0450(p_incorporador PKG_RECAUDACIONES.vincorporacionrec)     RETURN VARCHAR2;

FUNCTION recp_0450 RETURN varchar2;

   FUNCTION CALCULA_DV_BARRA(l_codigo_barra_1 VARCHAR2)
      RETURN VARCHAR2;

   FUNCTION incorpora_recaudacion(
      p_increcaudacionrec            PKG_RECAUDACIONES.vincrecaudacionrec,
      p_año_mes_contable             DATE,
      p_secuencia                    RECAUDACIONES.rec_secuencia%TYPE,
      p_obligacion          IN OUT   OBLIGACIONES.obl_id%TYPE,
      p_operacion                    RECAUDACIONES.rec_nro_operacion%TYPE)
      RETURN NUMBER;

   FUNCTION actualizar_recaudacion(p_actualizador PKG_RECAUDACIONES.vactuarecaudrec)
      RETURN VARCHAR2;

   FUNCTION aplicar_pagos(
      p_aplicarrecaurec            PKG_RECAUDACIONES.vaplicarrecaurec,
      p_secuencia                  RECAUDACIONES.rec_secuencia%TYPE,
      p_obligacion        IN OUT   OBLIGACIONES.obl_id%TYPE,
      p_ppl_id                     PLANES_PAGO.ppl_id%TYPE,
      p_operacion                  RECAUDACIONES.rec_nro_operacion%TYPE,
      p_busqueda_alter             NUMBER := 0,
      p_cnt_cuotas                 PLANES_PAGO.ppl_cnt_cuotas%TYPE := NULL)
      RETURN VARCHAR2;

   FUNCTION calculo_fecha_contable(v_fecha_cobro DATE, l_fecha_proceso DATE)
      RETURN DATE;

--FUNCTION Calculo_Recargos(p_fecha_inicial DATE,p_fecha_final DATE,p_monto NUMBER)
   FUNCTION CALCULO_RECARGOS(pdesde DATE, phasta DATE, pimporte NUMBER)
      RETURN NUMBER;

   FUNCTION f_verifica_datos(p_incorporador PKG_RECAUDACIONES.vincorporacionrec)
      RETURN VARCHAR2;

   FUNCTION inserta_asig_pagos(p_asig_pagos ASIG_PAGOS%ROWTYPE)
      RETURN ASIG_PAGOS.apa_id%TYPE;

   FUNCTION f_recupera_obl(
      p_erc_codigo       RECAUDACIONES.rec_erc_codigo%TYPE,
      p_ext_ppl          PLANES_PAGO.ppl_id%TYPE,
      p_cod_factura      VARCHAR2,
      p_busqueda_alter   VARCHAR2,
      p_inserta_recaud   PKG_RECAUDACIONES.vincrecaudacionrec,
      p_importe          OBLIGACIONES.obl_imp_original%TYPE)
      RETURN OBLIGACIONES%ROWTYPE;

   FUNCTION fc_generar_credito(p_obl_id OBLIGACIONES.obl_id%TYPE, p_aplicador PKG_RECAUDACIONES.vaplicarrecaurec, p_secuencia Number)
      RETURN VARCHAR2;
         
   FUNCTION f_factura_cuotas(p_secuencia number, p_aplicador PKG_RECAUDACIONES.vaplicarrecaurec, p_rec_id number)
      RETURN VARCHAR2;

   FUNCTION f_aplica_creditos(
      p_obl_id           OBLIGACIONES.obl_id%TYPE,
      p_usuario          ASIG_PAGOS.apa_usr_alta%TYPE,
      p_cct_rec_id       CUENTAS_CORRIENTES.cct_rec_id%TYPE,
      p_cct_banco        CUENTAS_CORRIENTES.cct_banco%TYPE,
      p_cct_remesa       CUENTAS_CORRIENTES.cct_remesa%TYPE,
      p_cct_secuencia    CUENTAS_CORRIENTES.cct_secuencia%TYPE,
      p_fec_cobro        DATE,
      p_fec_aplicacion   DATE)
      RETURN VARCHAR2;

   PROCEDURE inserta_cuenta_corriente(p_cuenta_corriente CUENTAS_CORRIENTES%ROWTYPE);

   PROCEDURE GENERA_NC_QUITA(
      p_plan               PLANES_PAGO.ppl_id%TYPE,
      p_cli_id             OBLIGACIONES.obl_cli_id%TYPE,
      p_inm_id             OBLIGACIONES.obl_inm_id%TYPE,
      p_cuenta             OBLIGACIONES.obl_cuenta%TYPE,
      p_grupo              OBLIGACIONES.obl_grp_codigo%TYPE,
      p_tipo_resp          OBLIGACIONES.obl_tre_codigo%TYPE,
      p_usuario            OBLIGACIONES.obl_usr_alta%TYPE,
      p_fec_prescripcion   DATE);

   FUNCTION encripta(clave VARCHAR2)
      RETURN VARCHAR2;

   FUNCTION crearrel(precid NUMBER, precrecid NUMBER, pnovid NUMBER, pusuario VARCHAR2)
      RETURN registro;

   FUNCTION actualizarel(precid NUMBER, pnovid NUMBER, precrecid NUMBER, pusuario VARCHAR2)
      RETURN registro;

   FUNCTION getnovid(precid NUMBER)
      RETURN NUMBER;
END PKG_RECAUDACIONES;
/

/*<TOAD_FILE_CHUNK>*/
CREATE OR REPLACE PACKAGE BODY MANANTIAL.PKG_RECAUDACIONES IS
   FUNCTION registra_log(cmensaje VARCHAR2)
      RETURN VARCHAR2 IS
      /* Modif. contingencia */
      v_path   par_generales.pge_valor%TYPE;
      cerror   NUMBER                         := 0;
      derror   VARCHAR2(200)                  := ' ';
   /* Fin modif. contingencia */
   BEGIN
      v_path := seguridad.pkg_parametros.getvalor('UTLFILE1', cerror, derror);

      IF cerror > 0 THEN
         RETURN derror;
      END IF;

      IF NOT(UTL_FILE.is_open(lfile)) THEN
         --lfile := UTL_FILE.fopen('\\osmsc020\reportes','Recaudacion'||TO_CHAR(SYSDATE,'ddmmhh24')||'.LOG','A');
         lfile := UTL_FILE.fopen(v_path, 'Recaudacion' || TO_CHAR(SYSDATE, 'ddmmhh24') || '.LOG', 'A');
      END IF;

      UTL_FILE.put_line(lfile, cmensaje);
      RETURN('Ok');
   END registra_log;    

   FUNCTION revisaacreditaciones
      RETURN NUMBER IS
      CURSOR cacre IS
         SELECT *
           FROM RECAUDACIONES
          WHERE rec_erc_codigo = 2
            AND TRUNC(rec_fec_alta) = TRUNC(SYSDATE)
            AND rec_id > 712441549   -- Valor para el dia de testing
            AND NOT EXISTS(SELECT 1
                             FROM REL_REC_NOV
                            WHERE rrn_rec_id = rec_id);

      ntemp   VARCHAR2(200);
      nret    NUMBER;
   BEGIN
      nret := 0;
      ntemp := registra_log(TO_CHAR(SYSDATE, 'dd/mm/rrrr hh24:mi:ss'));

      FOR racre IN cacre LOOP
         ntemp := registra_log(TO_CHAR(racre.rec_id) || ' Sin Referencia en REL_REC_NOV');
         nret := nret + 1;
      END LOOP;

      UTL_FILE.fclose(lfile);
      RETURN(nret);
   END revisaacreditaciones;


   FUNCTION fc_generar_credito(p_obl_id OBLIGACIONES.obl_id%TYPE, p_aplicador PKG_RECAUDACIONES.vaplicarrecaurec, p_secuencia  Number )
      RETURN VARCHAR2 IS
   CURSOR c_rec(p_rec_id NUMBER) IS
      SELECT *
        FROM manantial.RECAUDACIONES
       WHERE rec_id = p_rec_id;
--   v_fecha         DATE   := '19-nov-2019';
--   v_operacion     NUMBER := 45356;
   v_importe       NUMBER := 143.48;
   v_cliente       NUMBER;
--   v_remesa        NUMBER := 185712;
--   v_fecha_cobro   DATE   := '01-dec-2017';
--   v_fecha_acred   DATE   := '04-dec-2017';
   v_rec_id        NUMBER;
--   v_vco_codigo    NUMBER := 14;
--   -- importes de la novedad fac
   v_inm_id         INMUEBLES.inm_id%TYPE;
   v_tipo_iva        INMUEBLES.inm_tipo_responsable%TYPE;
   v_neto          NUMBER;
   v_iva_cf        NUMBER;
   v_iva_ex        NUMBER;
   v_iva_ri        NUMBER;
   v_iva_rni       NUMBER;
   v_iva_mon       NUMBER;
   v_iva_ali       NUMBER;
   v_iva_per       NUMBER;
   V_CON_ID        NUMBER;
--   v_banco         NUMBER := 7;
    BEGIN
       SELECT manantial.rec_seq.NEXTVAL
         INTO v_rec_id
         FROM dual;

          BEGIN
                SELECT inm_id,inm_tipo_responsable
                INTO v_inm_id, v_tipo_iva
                FROM INMUEBLES
                WHERE inm_cuenta = p_aplicador.v_identificacion;
                EXCEPTION WHEN others THEN 
                v_inm_id := NULL;
                v_tipo_iva := NULL;
          END;

          INSERT INTO manantial.RECAUDACIONES
                      (rec_id, rec_vco_codigo, rec_inm_id, rec_obl_id, rec_erc_codigo, rec_cuenta, rec_fecha,
                       rec_imp_cobrado, rec_nro_factura, rec_anio_periodo, rec_mes_periodo, rec_se, rec_usr_alta,
                       rec_fec_alta, rec_fec_aplicacion, rec_fec_proceso, rec_fec_contable, rec_banco,
                       rec_secuencia, rec_usr_baja, rec_fec_baja, rec_usr_mod, rec_fec_mod, rec_nro_operacion,
                       rec_rem_numero, rec_rem_ere_codigo, rec_remesa, rec_conciliado, rec_fec_acredita,
                       rec_clave_heredada, rec_lote)
               VALUES (v_rec_id, p_aplicador.p_ente , v_inm_id, p_obl_id, 2, p_aplicador.v_identificacion, p_aplicador.v_fecha_cobro,
                       p_aplicador.v_importe, p_aplicador.v_factura, p_aplicador.v_anio, p_aplicador.v_periodo, 'N', p_aplicador.v_usuario,
                       sysdate, NULL, sysdate, sysdate, p_aplicador.p_banco,
                       p_secuencia, NULL, NULL,  p_aplicador.v_usuario, sysdate, '',
                        p_aplicador.v_remesa_osm, p_aplicador.p_ente, p_aplicador.v_remesa, 'N', p_aplicador.v_fecha_cobro,
                       NULL, NULL);
         v_importe :=  p_aplicador.v_importe;
          FOR rec IN c_rec(v_rec_id) LOOP
             DECLARE
                l_nov_id   NUMBER(10);
             BEGIN
                BEGIN
                   SELECT rci_cli_id
                     INTO v_cliente
                     FROM manantial.REL_CLIENTE_INMUEBLE
                    WHERE rci_inm_id = rec.rec_inm_id AND rci_fec_baja IS NULL AND rci_factura_a = 'S';
                EXCEPTION
                   WHEN NO_DATA_FOUND THEN
                      DBMS_OUTPUT.put_line(rec.rec_cuenta);
                END;

                v_iva_cf := 0;
                v_iva_ex := 0;
                v_iva_ri := 0;
                v_iva_rni := 0;
                v_iva_mon := 0;
                v_iva_ali := 0;
                v_iva_per := 0;
                v_neto := 0;

                IF v_tipo_iva = 1 THEN
                   v_iva_cf := v_importe / 1.21 * 0.21;
                   v_neto := v_importe / 1.21;
                ELSIF v_tipo_iva = 2 THEN
                   v_iva_ex := v_importe / 1.21 * 0.21;
                   v_neto := v_importe / 1.21;
                ELSIF v_tipo_iva = 3 THEN
                   v_iva_rni := v_importe / 1.405 * 0.27;
                   v_iva_ali := v_importe / 1.405 * 0.13;
                   v_neto := v_importe / 1.405;
                ELSIF v_tipo_iva = 4 THEN
                   v_iva_ri := v_importe / 1.27 * 0.27;
                   v_neto := v_importe / 1.27;
                ELSIF v_tipo_iva = 6 THEN
                   v_iva_mon := v_importe / 1.27 * 0.27;
                   v_neto := v_importe / 1.27;
                ELSIF v_tipo_iva = 7 THEN
                   v_iva_ex := (v_importe / 1.21 / 1.135) * 0.21;
                   v_neto := v_importe / 1.21 / 1.135;
                   v_iva_per := (v_importe / 1.135) * 0.135;
                ELSIF v_tipo_iva = 8 THEN
                   v_iva_ri := (v_importe / 1.27 / 1.135) * 0.27;
                   v_neto := v_importe / 1.27 / 1.135;
                   v_iva_per := (v_importe / 1.135) * 0.135;
                ELSIF v_tipo_iva = 9 THEN
                   v_iva_rni := (v_importe / 1.27 / 1.135) * 0.27;
                   v_neto := v_importe / 1.27 / 1.135;
                   v_iva_per := (v_importe / 1.135) * 0.135;
                END IF;

                SELECT manantial.nov_seq.NEXTVAL
                  INTO l_nov_id
                  FROM dual;

                BEGIN
                   SELECT MIN(CON_ID) INTO V_CON_ID FROM CONEXIONES
                    WHERE CON_INM_ID = rec.rec_inm_id;
                   EXCEPTION WHEN OTHERS THEN V_CON_ID := 1;
                END;

                INSERT INTO manantial.NOVEDADES_FACTURABLES
                            (nov_id, nov_pef_anio, nov_pef_periodo, nov_ser_codigo,
                             nov_con_inm_id, nov_con_id, nov_nov_id, nov_tipo_novedad, nov_fec_novedad,
                             nov_estado, nov_tipo_origen, nov_nro_origen, nov_imp_neto, nov_imp_iva_cf,
                             nov_imp_iva_ex, nov_imp_iva_ri, nov_imp_iva_rni, nov_imp_iva_mon, nov_imp_iva_ali,
                             nov_imp_iva_per, nov_imp_cambio, nov_inm_id, nov_obl_id, nov_cli_id, nov_dpc_id,
                             nov_descripcion, nov_fec_destino, nov_tipo_destino, nov_nro_destino, nov_cod_iva,
                             nov_usr_alta, nov_fec_alta, nov_usr_mod, nov_fec_mod, nov_usr_baja, nov_fec_baja,
                             nov_cant_dias, nov_consumo, nov_fec_desde, nov_fec_hasta, nov_estado_ant,
                             nov_estado_act, nov_det_id, nov_recargo, nov_mac_codigo)
                     VALUES (l_nov_id, TO_CHAR(rec.rec_fec_alta, 'rrrr'), TO_CHAR(rec.rec_fec_alta, 'mm'), 650001,
                             rec.rec_inm_id, V_CON_ID, NULL, 10, rec.rec_fec_alta,
                             2, 'D', rec.rec_nro_factura, v_neto, v_iva_cf,
                             v_iva_ex, v_iva_ri, v_iva_rni, v_iva_mon, v_iva_ali,
                             v_iva_per, NULL, rec.rec_inm_id, NULL, v_cliente, NULL,
                             'Pago duplicado factura ' || rec.rec_nro_factura, NULL, NULL, NULL, v_tipo_iva,
                             rec.rec_usr_alta, rec.rec_fec_alta, NULL, NULL, NULL, NULL,
                             NULL, NULL, NULL, NULL, NULL,
                             NULL, NULL, 0, NULL);

                INSERT INTO manantial.REL_REC_NOV
                            (rrn_id, rrn_rec_id, rrn_nov_id, rrn_rec_rec_id, rrn_fec_alta, rrn_usr_alta)
                     VALUES (manantial.rrn_seq.NEXTVAL, v_rec_id, l_nov_id, 0, SYSDATE, rec.rec_usr_alta);
             END;
           END LOOP;
           RETURN 'OK';
    END;
    -------------------------------------------------
    FUNCTION fc_Aplica_Parte(p_obl_id OBLIGACIONES.obl_id%TYPE, p_aplicador PKG_RECAUDACIONES.vaplicarrecaurec,p_rec_id number, p_descrip_cct Varchar2,p_secuencia number  )    RETURN VARCHAR2 IS
    CURSOR SALDOS_LINEAS (P_OBL_ID NUMBER) IS
      SELECT CCT_TIPO_MOVIMIENTO, CCT_FEC_VENCIMIENTO, CCT_CUENTA, CCT_POR_IVA, CCT_POR_ALI, CCT_ID_MOVIMIENTO, CCT_SER_CODIGO, CCT_GRP_CODIGO, CCT_FEC_GENERACION,
             CCT_SUC_CODIGO, CCT_PEF_ANIO, CCT_PEF_PERIODO, CCT_TRE_CODIGO, CCT_IMP_DEBE, 
             CCT_IMP_DEBE - NVL( (SELECT SUM(APA_IMP_HABER) FROM Manantial.ASIG_PAGOS 
                                                            WHERE APA_CCT_OBL_ID = CCT_OBL_ID 
                                                            AND APA_CCT_ID_MOVIMIENTO = CCT_ID_MOVIMIENTO),0) CCT_SALDO ,
             CCT_IMP_IVA, CCT_IMP_ALI
      FROM Manantial.CUENTAS_CORRIENTES 
        WHERE CCT_OBL_ID = P_OBL_ID
         AND CCT_ESTADO = 15 AND CCT_IMP_DEBE > 0;
    Rta      Varchar2(100):='OK'; 
    v_obl_id OBLIGACIONES.obl_id%TYPE;
    v_linea number(10);
    v_inm_id RECAUDACIONES.rec_inm_id%TYPE;
    v_tipo_iva OBLIGACIONES.obl_tre_codigo%TYPE;
    v_rec_id RECAUDACIONES.rec_id%TYPE;
    v_obl OBLIGACIONES%ROWTYPE;
    l_monto_recargo NUMBER(15,2);
    r_datos_iva  PKG_SERVICIOS_FIJOS.ivarec;
    r_importe    PKG_SERVICIOS_FIJOS.importerec;
    r_novedades  NOVEDADES_FACTURABLES%ROWTYPE;
    l_dev_nov    NOVEDADES_FACTURABLES.nov_id%TYPE;
    BEGIN 
       v_obl_id := p_obl_id; 
       --busca datos de la obligacion
       BEGIN
           SELECT * INTO v_obl
           FROM Manantial.OBLIGACIONES WHERE obl_id = v_obl_id;
       END;
       --calcula la próxima linea en CCT
       BEGIN
          SELECT max(cct_id_movimiento)+1 INTO v_linea
          FROM Manantial.CUENTAS_CORRIENTES WHERE cct_obl_id = v_obl_id;
       END;     
        -- busca inm_id y tipo_iva
       BEGIN
           SELECT inm_id,inm_tipo_responsable
           INTO v_inm_id, v_tipo_iva
           FROM Manantial.INMUEBLES
           WHERE inm_cuenta = p_aplicador.v_identificacion;
           EXCEPTION WHEN others THEN 
              v_inm_id  := NULL;
              v_tipo_iva:= NULL;
       END;                    
       IF p_rec_id = 0 THEN
           BEGIN
              SELECT manantial.rec_seq.NEXTVAL INTO v_rec_id FROM dual;
           END;    
           -- genera el pago en la recaudación
           BEGIN
             INSERT INTO manantial.RECAUDACIONES
                              (rec_id, rec_vco_codigo, rec_inm_id, rec_obl_id, rec_erc_codigo, rec_cuenta, rec_fecha,
                               rec_imp_cobrado, rec_nro_factura, rec_anio_periodo, rec_mes_periodo, rec_se, rec_usr_alta,
                               rec_fec_alta, rec_fec_aplicacion, rec_fec_proceso, rec_fec_contable, rec_banco,
                               rec_secuencia, rec_usr_baja, rec_fec_baja, rec_usr_mod, rec_fec_mod, rec_nro_operacion,
                               rec_rem_numero, rec_rem_ere_codigo, rec_remesa, rec_conciliado, rec_fec_acredita,
                               rec_clave_heredada, rec_lote)
             VALUES (v_rec_id, p_aplicador.p_ente , v_inm_id, v_obl_id, p_aplicador.p_erc_codigo, p_aplicador.v_identificacion, p_aplicador.v_fecha_cobro,
                               p_aplicador.v_importe, p_aplicador.v_factura, p_aplicador.v_anio, p_aplicador.v_periodo, 'N', p_aplicador.v_usuario,
                               sysdate, NULL, sysdate, sysdate, p_aplicador.p_banco,
                               p_secuencia, NULL, NULL,  p_aplicador.v_usuario, sysdate, '',
                                p_aplicador.v_remesa_osm, p_aplicador.p_ente, p_aplicador.v_remesa, 'N', p_aplicador.v_fecha_cobro,
                               NULL, NULL);
           END;                  
       ELSE 
          v_rec_id := p_rec_id;
          BEGIN
              UPDATE manantial.RECAUDACIONES
              set rec_erc_codigo = 1, rec_fec_aplicacion = sysdate
              WHERE rec_id = v_rec_id;
          END;
       END IF;
       --------------------------------------------------------------------------------------------------------
       IF p_aplicador.p_erc_codigo <>  1 THEN
          RETURN ('OK'); 
       END IF;
       ----------------- SOLO CONTINUO SI EL PAGO ES APLICADO ------------------------------------------------- 
        -- genera linea de pago en la cct
       BEGIN
         INSERT INTO Manantial.CUENTAS_CORRIENTES (CCT_OBL_ID, CCT_ID_MOVIMIENTO, CCT_SER_CODIGO, CCT_PEF_ANIO, CCT_PEF_PERIODO, CCT_TIPO_MOVIMIENTO, 
                        CCT_IMP_DEBE, CCT_IMP_HABER, CCT_CONCEPTO, CCT_FEC_GENERACION, CCT_FEC_CIERRE_CONTABLE, CCT_USR_ALTA, CCT_FEC_ALTA, CCT_REC_ID, 
                        CCT_NOV_ID, CCT_FEC_VENCIMIENTO, CCT_CNT_DIAS_RECARGO, CCT_BANCO, CCT_REMESA, CCT_SECUENCIA, CCT_FEC_APLICACION, CCT_FEC_PROCESO, 
                        CCT_FEC_COBRO, CCT_USR_BAJA, CCT_FEC_BAJA, CCT_USR_MOD, CCT_FEC_MOD, CCT_ESTADO, CCT_SE, CCT_OBL_ID_CUOTA, CCT_GRP_CODIGO, 
                        CCT_CUENTA, CCT_SUC_CODIGO, CCT_TRE_CODIGO, CCT_IMP_IVA, CCT_POR_IVA, CCT_IMP_ALI, CCT_POR_ALI)
         VALUES (v_obl_id, v_linea, 800088, v_obl.obl_pef_anio, v_obl.obl_pef_periodo, v_obl.obl_tpc_codigo,
                        0, p_aplicador.v_importe, 'Pago con factura '||v_obl.obl_nro_factura||' -'||p_descrip_cct, sysdate, sysdate, p_aplicador.v_usuario, sysdate, v_rec_id,
                        NULL, sysdate, NULL, p_aplicador.p_banco, p_aplicador.v_remesa, p_secuencia, sysdate, sysdate,
                        p_aplicador.v_fecha_cobro, NULL, NULL ,NULL ,NULL , 30, 'N', NULL, v_obl.obl_grp_codigo,
                        v_obl.obl_cuenta, v_obl.obl_suc_codigo, v_tipo_iva, 0, 0, 0, 0  );
       END;        
       -----------------------------------------------------------------------------------
       -- genera líneas asig_pagos
       BEGIN
               FOR R IN SALDOS_LINEAS (V_OBL_ID) LOOP
                  IF p_descrip_cct = 'Cuota 1' THEN
                    INSERT INTO ASIG_PAGOS (APA_ID, APA_TIPO_MOVIMIENTO, APA_IMP_HABER, APA_CONCEPTO, APA_FEC_GENERACION, APA_FEC_CONTABILIZACION, APA_FEC_VENCIMIENTO, 
                                APA_BANCO, APA_REMESA, APA_FEC_APLICACION, APA_FEC_PROCESO, APA_FEC_COBRO, APA_ESTADO, APA_SE, APA_CUENTA, APA_IMP_IVA, APA_POR_IVA, 
                                APA_IMP_ALI, APA_POR_ALI, APA_CCT_OBL_ID, APA_CCT_ID_MOVIMIENTO, APA_SER_CODIGO, APA_REC_ID, APA_GRP_CODIGO, APA_SUC_CODIGO, APA_PEF_ANIO, 
                                APA_PEF_PERIODO, APA_TRE_CODIGO, APA_FEC_ALTA, APA_USR_ALTA)
                    VALUES (APA_SEQ.NEXTVAL, R.CCT_TIPO_MOVIMIENTO, R.CCT_SALDO*(p_aplicador.v_importe/V_OBL.OBL_SALDO), 'Pago con factura '||v_obl.obl_nro_factura||' - Cuota 1' , R.CCT_FEC_GENERACION, SYSDATE, R.CCT_FEC_VENCIMIENTO,
                                p_aplicador.p_banco, p_aplicador.v_remesa, SYSDATE, SYSDATE, p_aplicador.v_fecha_cobro, 30, 'N', R.CCT_CUENTA, R.CCT_IMP_IVA * (p_aplicador.v_importe/V_OBL.OBL_SALDO), R.CCT_POR_IVA,
                                R.CCT_IMP_ALI * (p_aplicador.v_importe/V_OBL.OBL_SALDO), R.CCT_POR_ALI, V_OBL_ID, R.CCT_ID_MOVIMIENTO, R.CCT_SER_CODIGO, V_REC_ID, R.CCT_GRP_CODIGO, R.CCT_SUC_CODIGO, R.CCT_PEF_ANIO,
                                R.CCT_PEF_PERIODO, R.CCT_TRE_CODIGO, SYSDATE, p_aplicador.v_USUARIO);
                  ELSE     
                    INSERT INTO Manantial.ASIG_PAGOS (APA_ID, APA_TIPO_MOVIMIENTO, APA_IMP_HABER, APA_CONCEPTO, APA_FEC_GENERACION, APA_FEC_CONTABILIZACION, APA_FEC_VENCIMIENTO, 
                                APA_BANCO, APA_REMESA, APA_FEC_APLICACION, APA_FEC_PROCESO, APA_FEC_COBRO, APA_ESTADO, APA_SE, APA_CUENTA, APA_IMP_IVA, APA_POR_IVA, 
                                APA_IMP_ALI, APA_POR_ALI, APA_CCT_OBL_ID, APA_CCT_ID_MOVIMIENTO, APA_SER_CODIGO, APA_REC_ID, APA_GRP_CODIGO, APA_SUC_CODIGO, APA_PEF_ANIO, 
                                APA_PEF_PERIODO, APA_TRE_CODIGO, APA_FEC_ALTA, APA_USR_ALTA)
                    VALUES (APA_SEQ.NEXTVAL, R.CCT_TIPO_MOVIMIENTO, R.CCT_SALDO, 'Pago con factura '||v_obl.obl_nro_factura||' - Cuota 2' ,  R.CCT_FEC_GENERACION, SYSDATE, R.CCT_FEC_VENCIMIENTO,
                                p_aplicador.p_banco, p_aplicador.v_remesa, SYSDATE, SYSDATE, p_aplicador.v_fecha_cobro, 30, 'N', R.CCT_CUENTA, R.CCT_IMP_IVA * (R.CCT_SALDO/R.CCT_IMP_DEBE), R.CCT_POR_IVA,
                                R.CCT_IMP_ALI * (R.CCT_SALDO/R.CCT_IMP_DEBE), R.CCT_POR_ALI, V_OBL_ID, R.CCT_ID_MOVIMIENTO, R.CCT_SER_CODIGO, V_REC_ID, R.CCT_GRP_CODIGO, R.CCT_SUC_CODIGO, R.CCT_PEF_ANIO,
                                R.CCT_PEF_PERIODO, R.CCT_TRE_CODIGO, SYSDATE, p_aplicador.v_USUARIO);
                  END IF;                    
               END LOOP; 
       END;                        
       -------------------------------------------------------------------------------------
       -- actualiza obligacion
       IF p_descrip_cct = 'Cuota 1' THEN
          BEGIN
                UPDATE Manantial.OBLIGACIONES
                   set obl_saldo = obl_saldo - p_aplicador.v_importe, 
                       obl_fec_aplicacion = sysdate, 
                       obl_fec_mod = sysdate, 
                       obl_usr_mod = p_aplicador.v_usuario
                 WHERE obl_id = v_obl_id;
            END;
       ELSE 
            BEGIN
                UPDATE Manantial.OBLIGACIONES
                   set obl_saldo = 0,
                       obl_estado = 30, 
                       obl_fec_aplicacion = sysdate, 
                       obl_fec_mod = sysdate, 
                       obl_usr_mod = p_aplicador.v_usuario
                 WHERE obl_id = v_obl_id;
            END;
       END IF;   
       ----------------------------------------------------------------------------------------
       IF p_descrip_cct = 'Cuota 1' THEN
          BEGIN
                UPDATE CUENTAS_CORRIENTES
                   set cct_fec_cobro = p_aplicador.v_fecha_cobro, cct_fec_proceso = sysdate, cct_fec_aplicacion = sysdate,
                       cct_rec_id = v_rec_id, cct_banco = p_aplicador.p_banco, cct_remesa = p_aplicador.v_remesa, cct_secuencia = p_secuencia, cct_fec_mod = sysdate, 
                       cct_usr_mod = p_aplicador.v_usuario
                 WHERE cct_obl_id = v_obl_id 
                   AND cct_estado = 15 AND cct_imp_debe > 0;
          END;
       ELSE 
          BEGIN
                UPDATE CUENTAS_CORRIENTES
                   set cct_estado = 30, cct_fec_cobro = p_aplicador.v_fecha_cobro, cct_fec_proceso = sysdate, cct_fec_aplicacion = sysdate,
                       cct_rec_id = v_rec_id, cct_banco = p_aplicador.p_banco, cct_remesa = p_aplicador.v_remesa, cct_secuencia = p_secuencia, cct_fec_mod = sysdate, 
                       cct_usr_mod = p_aplicador.v_usuario
                 WHERE cct_obl_id = v_obl_id 
                   AND cct_estado = 15 AND cct_imp_debe > 0;
          END;
          IF p_aplicador.v_fecha_cobro > V_OBL.OBL_FEC_VENCIMIENTO and p_aplicador.p_ente != 2 THEN
             BEGIN
                    l_monto_recargo := CALCULO_RECARGOS(V_OBL.OBL_FEC_VENCIMIENTO, p_aplicador.v_fecha_cobro , V_OBL.OBL_IMP_ORIGINAL);
                 --   INSERT INTO SR VALUES (to_char(l_monto_recargo));            
                    r_datos_iva := PKG_SERVICIOS_FIJOS.f_imp_iva(V_TIPO_IVA, 640110, SYSDATE);
                    /* Calcula el IVA correspondiente, en caso que tenga una nueva categoria */
                    r_importe.iva := (r_datos_iva.iva * l_monto_recargo) / 100;
                    r_importe.alicuota := (r_datos_iva.alicuota * l_monto_recargo) / 100;
                    r_importe.percepcion := (r_datos_iva.percepcion *(l_monto_recargo + r_importe.iva)) / 100;
                    /* Completa y genera la Novedad Facturable de Recargos por Pago Fuera de Término */
                    r_novedades.nov_imp_neto := l_monto_recargo;
                    r_novedades.nov_pef_anio := P_APLICADOR.v_anio;
                    r_novedades.nov_pef_periodo := P_APLICADOR.v_periodo;
                    r_novedades.nov_ser_codigo := 640110;
                    r_novedades.nov_tipo_origen := 'Z';
                    r_novedades.nov_con_inm_id := V_OBL.OBL_INM_ID;  
                    r_novedades.nov_tipo_novedad := 1;
                    r_novedades.nov_fec_novedad := SYSDATE;
                    r_novedades.nov_estado := 2;
                    r_novedades.nov_nro_origen := V_obl_id;
                    r_novedades.nov_imp_iva_cf := 0;
                    r_novedades.nov_imp_iva_ex := 0;
                    r_novedades.nov_imp_iva_ri := 0;
                    r_novedades.nov_imp_iva_rni := 0;
                    r_novedades.nov_imp_iva_mon := 0;
                    r_novedades.nov_imp_iva_ali := 0;
                    r_novedades.nov_imp_iva_per := 0;
                    r_novedades.nov_imp_cambio := 1;
                    r_novedades.nov_inm_id := V_OBL.OBL_INM_ID;
                    r_novedades.nov_obl_id := V_obl_id;
                    r_novedades.nov_cli_id := V_OBL.OBL_cli_id;
                    r_novedades.nov_dpc_id := NULL;
                       r_novedades.nov_descripcion :=
                             'Recargo s/Factura:'
                          || LPAD(TO_CHAR(NVL(V_OBL.OBL_nro_factura, 0), '9999999999999999'), 17, ' ');
                    r_novedades.nov_descripcion :=
                          r_novedades.nov_descripcion
                       || ' Vto:'
                       || TO_CHAR(V_OBL.OBL_FEC_VENCIMIENTO, 'dd/mm/rrrr')
                       || ' Pag:'
                       || TO_CHAR(P_APLICADOR.v_fecha_cobro, 'dd/mm/rrrr');
                    r_novedades.nov_fec_destino := NULL;
                    r_novedades.nov_tipo_destino := NULL;
                    r_novedades.nov_cod_iva := V_TIPO_IVA;
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
             END;
          END IF;
       END IF;      
       -----------------------------------------------------------------------------                           
       RETURN(Rta); 
    END; 
  

   FUNCTION f_factura_cuotas(p_secuencia number, p_aplicador PKG_RECAUDACIONES.vaplicarrecaurec, p_rec_id number)
      RETURN VARCHAR2 IS
   v_obl_id OBLIGACIONES.obl_id%TYPE;
   v_obl_ppl_id OBLIGACIONES.obl_ppl_id%TYPE;
   v_obl_estado OBLIGACIONES.obl_estado%TYPE;
   v_obl_saldo OBLIGACIONES.obl_saldo%TYPE;
   v_obl_imp_original OBLIGACIONES.obl_imp_original%TYPE;
   v_rta varchar2(400);
   v_obl OBLIGACIONES%ROWTYPE;
   v_linea number(10);
   v_rec_id RECAUDACIONES.rec_id%TYPE;
   v_inm_id RECAUDACIONES.rec_inm_id%TYPE;
   v_tipo_iva OBLIGACIONES.obl_tre_codigo%TYPE;
   l_monto_recargo NUMBER(15,2);
   r_datos_iva PKG_SERVICIOS_FIJOS.ivarec;
   r_importe   PKG_SERVICIOS_FIJOS.importerec;
   r_novedades  NOVEDADES_FACTURABLES%ROWTYPE;
   l_dev_nov NOVEDADES_FACTURABLES.nov_id%TYPE;
   v_aplicador PKG_RECAUDACIONES.vaplicarrecaurec := p_aplicador; 
   CURSOR SALDOS_LINEAS (P_OBL_ID NUMBER) IS
      SELECT CCT_TIPO_MOVIMIENTO, CCT_FEC_VENCIMIENTO, CCT_CUENTA, CCT_POR_IVA, CCT_POR_ALI, CCT_ID_MOVIMIENTO, CCT_SER_CODIGO, CCT_GRP_CODIGO, CCT_FEC_GENERACION,
             CCT_SUC_CODIGO, CCT_PEF_ANIO, CCT_PEF_PERIODO, CCT_TRE_CODIGO, CCT_IMP_DEBE, CCT_IMP_DEBE - NVL( (SELECT SUM(APA_IMP_HABER) FROM ASIG_PAGOS WHERE APA_CCT_OBL_ID
             = CCT_OBL_ID AND APA_CCT_ID_MOVIMIENTO = CCT_ID_MOVIMIENTO),0) CCT_SALDO , CCT_IMP_IVA, CCT_IMP_ALI
        FROM CUENTAS_CORRIENTES WHERE CCT_OBL_ID = P_OBL_ID
         AND CCT_ESTADO = 15 AND CCT_IMP_DEBE > 0;
   BEGIN
      -- busca la factura para obtener el OBL_ID
      BEGIN
        SELECT obl_id, obl_ppl_id, obl_estado, obl_saldo , obl_imp_original
        INTO v_obl_id, v_obl_ppl_id, v_obl_estado, v_obl_saldo, v_obl_imp_original
        FROM OBLIGACIONES 
        WHERE obl_nro_factura = p_aplicador.v_factura
          AND obl_cuenta = p_aplicador.v_identificacion;
        -- si no la encuentra o hay mas de una    
        EXCEPTION WHEN others THEN
           v_rta := fc_generar_credito ( NULL,p_aplicador, p_secuencia );
           RETURN 'OK';
      END;
      -- si la encontró
      IF v_obl_id IS NOT NULL THEN
         -- si está en un plan, o cancelada, o anulada 
         IF v_obl_ppl_id IS NOT NULL OR v_obl_estado = 30 OR v_obl_estado > 59 THEN
            -- generar crédito
            v_rta := fc_generar_credito ( v_obl_id,p_aplicador, p_secuencia);
            RETURN 'OK';
         -- si el saldo es mayor al importe de la cuota, la aplica   
         ELSIF v_obl_saldo > p_aplicador.v_importe+0.01 THEN
            -- aplicar 1ra cuota
            v_aplicador.p_erc_codigo := 1 ;  
            v_rta := fc_Aplica_Parte( v_obl_id, v_aplicador, p_rec_id , 'Cuota 1',p_secuencia );
            RETURN (v_rta);
            --busca datos de la obligacion
            --BEGIN
            --    SELECT * INTO v_obl
            --    FROM OBLIGACIONES WHERE obl_id = v_obl_id;
            --END;
            --calcula la próxima linea en CCT
           -- BEGIN
           --     SELECT max(cct_id_movimiento)+1 INTO v_linea
           --     FROM CUENTAS_CORRIENTES WHERE cct_obl_id = v_obl_id;
           -- END;
                -- busca inm_id y tipo_iva
           -- BEGIN
           --     SELECT inm_id,inm_tipo_responsable
           --     INTO v_inm_id, v_tipo_iva
           --     FROM INMUEBLES
           --     WHERE inm_cuenta = p_aplicador.v_identificacion;
           --     EXCEPTION WHEN others THEN 
           --     v_inm_id := NULL;
           --     v_tipo_iva := NULL;
           -- END;
             --IF p_rec_id = 0 THEN
                -- obtiene el REC_ID
               -- BEGIN
              --      SELECT manantial.rec_seq.NEXTVAL INTO v_rec_id FROM dual;
               -- END;    
                  -- genera el pago en la recaudación
              --  BEGIN
              --    INSERT INTO manantial.RECAUDACIONES
              --                (rec_id, rec_vco_codigo, rec_inm_id, rec_obl_id, rec_erc_codigo, rec_cuenta, rec_fecha,
              --                 rec_imp_cobrado, rec_nro_factura, rec_anio_periodo, rec_mes_periodo, rec_se, rec_usr_alta,
               --                rec_fec_alta, rec_fec_aplicacion, rec_fec_proceso, rec_fec_contable, rec_banco,
               --                rec_secuencia, rec_usr_baja, rec_fec_baja, rec_usr_mod, rec_fec_mod, rec_nro_operacion,
               --                rec_rem_numero, rec_rem_ere_codigo, rec_remesa, rec_conciliado, rec_fec_acredita,
               --                rec_clave_heredada, rec_lote)
               --        VALUES (v_rec_id, p_aplicador.p_ente , v_inm_id, v_obl_id, 1, p_aplicador.v_identificacion, p_aplicador.v_fecha_cobro,
               --                p_aplicador.v_importe, p_aplicador.v_factura, p_aplicador.v_anio, p_aplicador.v_periodo, 'N', p_aplicador.v_usuario,
                --               sysdate, NULL, sysdate, sysdate, p_aplicador.p_banco,
                --               p_secuencia, NULL, NULL,  p_aplicador.v_usuario, sysdate, '',
               --                 p_aplicador.v_remesa_osm, p_aplicador.p_ente, p_aplicador.v_remesa, 'N', p_aplicador.v_fecha_cobro,
               --                NULL, NULL);
               --  END;    
             --ELSE
             --   v_rec_id := p_rec_id;
             --    BEGIN
             --        UPDATE manantial.RECAUDACIONES
             --        set rec_erc_codigo = 1, rec_fec_aplicacion = sysdate
             --        WHERE rec_id = v_rec_id;
             --    END;    
             --END IF;              
             -- genera linea de pago en la cct
            --BEGIN
            --    INSERT INTO CUENTAS_CORRIENTES (CCT_OBL_ID, CCT_ID_MOVIMIENTO, CCT_SER_CODIGO, CCT_PEF_ANIO, CCT_PEF_PERIODO, CCT_TIPO_MOVIMIENTO, 
            --            CCT_IMP_DEBE, CCT_IMP_HABER, CCT_CONCEPTO, CCT_FEC_GENERACION, CCT_FEC_CIERRE_CONTABLE, CCT_USR_ALTA, CCT_FEC_ALTA, CCT_REC_ID, 
            --            CCT_NOV_ID, CCT_FEC_VENCIMIENTO, CCT_CNT_DIAS_RECARGO, CCT_BANCO, CCT_REMESA, CCT_SECUENCIA, CCT_FEC_APLICACION, CCT_FEC_PROCESO, 
            --            CCT_FEC_COBRO, CCT_USR_BAJA, CCT_FEC_BAJA, CCT_USR_MOD, CCT_FEC_MOD, CCT_ESTADO, CCT_SE, CCT_OBL_ID_CUOTA, CCT_GRP_CODIGO, 
            --            CCT_CUENTA, CCT_SUC_CODIGO, CCT_TRE_CODIGO, CCT_IMP_IVA, CCT_POR_IVA, CCT_IMP_ALI, CCT_POR_ALI)
            --    VALUES (v_obl_id, v_linea, 800088, v_obl.obl_pef_anio, v_obl.obl_pef_periodo, v_obl.obl_tpc_codigo,
            --            0, p_aplicador.v_importe, 'Pago con factura '||v_obl.obl_nro_factura||' - Cuota 1', sysdate, sysdate, p_aplicador.v_usuario, sysdate, v_rec_id,
            --            NULL, sysdate, NULL, p_aplicador.p_banco, p_aplicador.v_remesa, p_secuencia, sysdate, sysdate,
            --            p_aplicador.v_fecha_cobro, NULL, NULL ,NULL ,NULL , 30, 'N', NULL, v_obl.obl_grp_codigo,
            --            v_obl.obl_cuenta, v_obl.obl_suc_codigo, v_tipo_iva, 0, 0, 0, 0  );
            --END;
            -- genera líneas asig_pagos
            --BEGIN
            --   FOR R IN SALDOS_LINEAS (V_OBL_ID) LOOP
            --       INSERT INTO ASIG_PAGOS (APA_ID, APA_TIPO_MOVIMIENTO, APA_IMP_HABER, APA_CONCEPTO, APA_FEC_GENERACION, APA_FEC_CONTABILIZACION, APA_FEC_VENCIMIENTO, 
            --                    APA_BANCO, APA_REMESA, APA_FEC_APLICACION, APA_FEC_PROCESO, APA_FEC_COBRO, APA_ESTADO, APA_SE, APA_CUENTA, APA_IMP_IVA, APA_POR_IVA, 
            --                    APA_IMP_ALI, APA_POR_ALI, APA_CCT_OBL_ID, APA_CCT_ID_MOVIMIENTO, APA_SER_CODIGO, APA_REC_ID, APA_GRP_CODIGO, APA_SUC_CODIGO, APA_PEF_ANIO, 
            --                    APA_PEF_PERIODO, APA_TRE_CODIGO, APA_FEC_ALTA, APA_USR_ALTA)
            --        VALUES (APA_SEQ.NEXTVAL, R.CCT_TIPO_MOVIMIENTO, R.CCT_SALDO*(p_aplicador.v_importe/V_OBL.OBL_SALDO), 'Pago con factura '||v_obl.obl_nro_factura||' - Cuota 1' , R.CCT_FEC_GENERACION, SYSDATE, R.CCT_FEC_VENCIMIENTO,
            --                    p_aplicador.p_banco, p_aplicador.v_remesa, SYSDATE, SYSDATE, p_aplicador.v_fecha_cobro, 30, 'N', R.CCT_CUENTA, R.CCT_IMP_IVA * (p_aplicador.v_importe/V_OBL.OBL_SALDO), R.CCT_POR_IVA,
            --                    R.CCT_IMP_ALI * (p_aplicador.v_importe/V_OBL.OBL_SALDO), R.CCT_POR_ALI, V_OBL_ID, R.CCT_ID_MOVIMIENTO, R.CCT_SER_CODIGO, V_REC_ID, R.CCT_GRP_CODIGO, R.CCT_SUC_CODIGO, R.CCT_PEF_ANIO,
            --                    R.CCT_PEF_PERIODO, R.CCT_TRE_CODIGO, SYSDATE, p_aplicador.v_USUARIO);      
            --   END LOOP;
            --END;         -- si el saldo difiere con la cuota en hasta 1 centavo lo aplica, sino genera crédito
            -- actualiza obligacion
            --BEGIN
            --    UPDATE OBLIGACIONES
           --        set obl_saldo = obl_saldo - p_aplicador.v_importe, 
            --           obl_fec_aplicacion = sysdate, 
            --           obl_fec_mod = sysdate, 
            --           obl_usr_mod = p_aplicador.v_usuario
            --     WHERE obl_id = v_obl_id;
            --END;
            -- actualiza las otras líneas de la cct 
            --BEGIN
            --    UPDATE CUENTAS_CORRIENTES
            --       set cct_fec_cobro = p_aplicador.v_fecha_cobro, cct_fec_proceso = sysdate, cct_fec_aplicacion = sysdate,
            --           cct_rec_id = v_rec_id, cct_banco = p_aplicador.p_banco, cct_remesa = p_aplicador.v_remesa, cct_secuencia = p_secuencia, cct_fec_mod = sysdate, 
            --           cct_usr_mod = p_aplicador.v_usuario
            --     WHERE cct_obl_id = v_obl_id 
            --       AND cct_estado = 15 AND cct_imp_debe > 0;
            --END;
         ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   
         ELSIF abs(v_obl_saldo-p_aplicador.v_importe) <= 0.01 THEN
            -- aplicar 2da cuota
            -- si falta 1 centavo dar por cancelada
            -- si sobra 1 centavo no generar crédito
             v_aplicador.p_erc_codigo := 1 ;
             v_rta :=fc_Aplica_Parte( v_obl_id, v_aplicador, p_rec_id, 'Cuota 2', p_secuencia);
             RETURN (v_rta); 
            --busca datos de la obligacion
            --BEGIN
            --    SELECT * INTO v_obl
            --    FROM OBLIGACIONES WHERE obl_id = v_obl_id;
           -- END;
           --calcula la próxima linea en CCT
           --BEGIN
           --    SELECT max(cct_id_movimiento)+1 INTO v_linea
           --    FROM CUENTAS_CORRIENTES WHERE cct_obl_id = v_obl_id;
           --END;
           -- busca inm_id y tipo_iva
           --BEGIN
           --    SELECT inm_id,inm_tipo_responsable
           --    INTO v_inm_id, v_tipo_iva
           --    FROM INMUEBLES
           --     WHERE inm_cuenta = p_aplicador.v_identificacion;
           --     EXCEPTION WHEN others THEN 
           --     v_inm_id := NULL;
           --     v_tipo_iva := NULL;
           -- END;
           -- IF p_rec_id = 0 THEN
                -- obtiene el REC_ID
              --  BEGIN
              --      SELECT manantial.rec_seq.NEXTVAL INTO v_rec_id FROM dual;
              --  END;    
                  -- genera el pago en la recaudación
              --  BEGIN
              --    INSERT INTO manantial.RECAUDACIONES
              --                (rec_id, rec_vco_codigo, rec_inm_id, rec_obl_id, rec_erc_codigo, rec_cuenta, rec_fecha,
              --                 rec_imp_cobrado, rec_nro_factura, rec_anio_periodo, rec_mes_periodo, rec_se, rec_usr_alta,
              --                 rec_fec_alta, rec_fec_aplicacion, rec_fec_proceso, rec_fec_contable, rec_banco,
              --                 rec_secuencia, rec_usr_baja, rec_fec_baja, rec_usr_mod, rec_fec_mod, rec_nro_operacion,
              --                 rec_rem_numero, rec_rem_ere_codigo, rec_remesa, rec_conciliado, rec_fec_acredita,
              --                 rec_clave_heredada, rec_lote)
              --         VALUES (v_rec_id, p_aplicador.p_ente , v_inm_id, v_obl_id, 1, p_aplicador.v_identificacion, p_aplicador.v_fecha_cobro,
              --                 p_aplicador.v_importe, p_aplicador.v_factura, p_aplicador.v_anio, p_aplicador.v_periodo, 'N', p_aplicador.v_usuario,
              --                 sysdate, NULL, sysdate, sysdate, p_aplicador.p_banco,
              --                 p_secuencia, NULL, NULL,  p_aplicador.v_usuario, sysdate, '',
              --                  p_aplicador.v_remesa_osm, p_aplicador.p_ente, p_aplicador.v_remesa, 'N', p_aplicador.v_fecha_cobro,
              --                 NULL, NULL);
              --   END;
             --ELSE
             --   v_rec_id := p_rec_id;
            --     BEGIN
            --         UPDATE manantial.RECAUDACIONES
            --         set rec_erc_codigo = 1, rec_fec_aplicacion = sysdate
            --         WHERE rec_id = v_rec_id;
            --     END;    
            -- END IF;                  
             -- genera linea de pago en la cct
            --BEGIN
            --    INSERT INTO CUENTAS_CORRIENTES (CCT_OBL_ID, CCT_ID_MOVIMIENTO, CCT_SER_CODIGO, CCT_PEF_ANIO, CCT_PEF_PERIODO, CCT_TIPO_MOVIMIENTO, 
            --            CCT_IMP_DEBE, CCT_IMP_HABER, CCT_CONCEPTO, CCT_FEC_GENERACION, CCT_FEC_CIERRE_CONTABLE, CCT_USR_ALTA, CCT_FEC_ALTA, CCT_REC_ID, 
            --            CCT_NOV_ID, CCT_FEC_VENCIMIENTO, CCT_CNT_DIAS_RECARGO, CCT_BANCO, CCT_REMESA, CCT_SECUENCIA, CCT_FEC_APLICACION, CCT_FEC_PROCESO, 
            --            CCT_FEC_COBRO, CCT_USR_BAJA, CCT_FEC_BAJA, CCT_USR_MOD, CCT_FEC_MOD, CCT_ESTADO, CCT_SE, CCT_OBL_ID_CUOTA, CCT_GRP_CODIGO, 
            --            CCT_CUENTA, CCT_SUC_CODIGO, CCT_TRE_CODIGO, CCT_IMP_IVA, CCT_POR_IVA, CCT_IMP_ALI, CCT_POR_ALI)
            --    VALUES (v_obl_id, v_linea, 800088, v_obl.obl_pef_anio, v_obl.obl_pef_periodo, v_obl.obl_tpc_codigo,
            --            0, p_aplicador.v_importe, 'Pago con factura '||v_obl.obl_nro_factura||' - Cuota 2', sysdate, sysdate, p_aplicador.v_usuario, sysdate, v_rec_id,
            --            NULL, sysdate, NULL, p_aplicador.p_banco, p_aplicador.v_remesa, p_secuencia, sysdate, sysdate,
            --            p_aplicador.v_fecha_cobro, NULL, NULL ,NULL ,NULL , 30, 'N', NULL, v_obl.obl_grp_codigo,
            --            v_obl.obl_cuenta, v_obl.obl_suc_codigo, v_tipo_iva, 0, 0, 0, 0  );
            --END;
            -- genera líneas asig_pagos
            --BEGIN
            --   FOR R IN SALDOS_LINEAS (V_OBL_ID) LOOP
            --       INSERT INTO ASIG_PAGOS (APA_ID, APA_TIPO_MOVIMIENTO, APA_IMP_HABER, APA_CONCEPTO, APA_FEC_GENERACION, APA_FEC_CONTABILIZACION, APA_FEC_VENCIMIENTO, 
            --                    APA_BANCO, APA_REMESA, APA_FEC_APLICACION, APA_FEC_PROCESO, APA_FEC_COBRO, APA_ESTADO, APA_SE, APA_CUENTA, APA_IMP_IVA, APA_POR_IVA, 
            --                    APA_IMP_ALI, APA_POR_ALI, APA_CCT_OBL_ID, APA_CCT_ID_MOVIMIENTO, APA_SER_CODIGO, APA_REC_ID, APA_GRP_CODIGO, APA_SUC_CODIGO, APA_PEF_ANIO, 
            --                    APA_PEF_PERIODO, APA_TRE_CODIGO, APA_FEC_ALTA, APA_USR_ALTA)
            --        VALUES (APA_SEQ.NEXTVAL, R.CCT_TIPO_MOVIMIENTO, R.CCT_SALDO, 'Pago con factura '||v_obl.obl_nro_factura||' - Cuota 2' ,  R.CCT_FEC_GENERACION, SYSDATE, R.CCT_FEC_VENCIMIENTO,
            --                    p_aplicador.p_banco, p_aplicador.v_remesa, SYSDATE, SYSDATE, p_aplicador.v_fecha_cobro, 30, 'N', R.CCT_CUENTA, R.CCT_IMP_IVA * (R.CCT_SALDO/R.CCT_IMP_DEBE), R.CCT_POR_IVA,
            --                    R.CCT_IMP_ALI * (R.CCT_SALDO/R.CCT_IMP_DEBE), R.CCT_POR_ALI, V_OBL_ID, R.CCT_ID_MOVIMIENTO, R.CCT_SER_CODIGO, V_REC_ID, R.CCT_GRP_CODIGO, R.CCT_SUC_CODIGO, R.CCT_PEF_ANIO,
            --                    R.CCT_PEF_PERIODO, R.CCT_TRE_CODIGO, SYSDATE, p_aplicador.v_USUARIO);      
            --   END LOOP; 
            --END;
            -- actualiza obligacion
            --BEGIN
            --    UPDATE OBLIGACIONES
            --       set obl_saldo = 0,
            --           obl_estado = 30, 
            --           obl_fec_aplicacion = sysdate, 
            --           obl_fec_mod = sysdate, 
            --           obl_usr_mod = p_aplicador.v_usuario
            --     WHERE obl_id = v_obl_id;
           -- END;
            -- actualiza las otras líneas de la cct a estado 30
            --BEGIN
            --    UPDATE CUENTAS_CORRIENTES
           --        set cct_estado = 30, cct_fec_cobro = p_aplicador.v_fecha_cobro, cct_fec_proceso = sysdate, cct_fec_aplicacion = sysdate,
           --            cct_rec_id = v_rec_id, cct_banco = p_aplicador.p_banco, cct_remesa = p_aplicador.v_remesa, cct_secuencia = p_secuencia, cct_fec_mod = sysdate, 
           --            cct_usr_mod = p_aplicador.v_usuario
           --      WHERE cct_obl_id = v_obl_id 
           --        AND cct_estado = 15 AND cct_imp_debe > 0;
           -- END;
            --IF p_aplicador.v_fecha_cobro > V_OBL.OBL_FEC_VENCIMIENTO THEN
                -- generar rpft
            --    BEGIN
              --      l_monto_recargo := CALCULO_RECARGOS(V_OBL.OBL_FEC_VENCIMIENTO, p_aplicador.v_fecha_cobro , V_OBL.OBL_IMP_ORIGINAL);
                 --   INSERT INTO SR VALUES (to_char(l_monto_recargo));            
               --     r_datos_iva := PKG_SERVICIOS_FIJOS.f_imp_iva(V_TIPO_IVA, 640110, SYSDATE);
                    /* Calcula el IVA correspondiente, en caso que tenga una nueva categoria */
                --    r_importe.iva := (r_datos_iva.iva * l_monto_recargo) / 100;
                --    r_importe.alicuota := (r_datos_iva.alicuota * l_monto_recargo) / 100;
                --    r_importe.percepcion := (r_datos_iva.percepcion *(l_monto_recargo + r_importe.iva)) / 100;
                    /* Completa y genera la Novedad Facturable de Recargos por Pago Fuera de Término */
                 --   r_novedades.nov_imp_neto := l_monto_recargo;
                 --   r_novedades.nov_pef_anio := P_APLICADOR.v_anio;
                 --   r_novedades.nov_pef_periodo := P_APLICADOR.v_periodo;
                 --   r_novedades.nov_ser_codigo := 640110;
                 --   r_novedades.nov_tipo_origen := 'Z';
                 --   r_novedades.nov_con_inm_id := V_OBL.OBL_INM_ID;  
                 --   r_novedades.nov_tipo_novedad := 1;
                 --   r_novedades.nov_fec_novedad := SYSDATE;
                 --   r_novedades.nov_estado := 2;
                 --   r_novedades.nov_nro_origen := V_obl_id;
                 --   r_novedades.nov_imp_iva_cf := 0;
                 --   r_novedades.nov_imp_iva_ex := 0;
                 --   r_novedades.nov_imp_iva_ri := 0;
                  --  r_novedades.nov_imp_iva_rni := 0;
                 --   r_novedades.nov_imp_iva_mon := 0;
                 --   r_novedades.nov_imp_iva_ali := 0;
                --    r_novedades.nov_imp_iva_per := 0;
                --    r_novedades.nov_imp_cambio := 1;
                --    r_novedades.nov_inm_id := V_OBL.OBL_INM_ID;
                --    r_novedades.nov_obl_id := V_obl_id;
                --    r_novedades.nov_cli_id := V_OBL.OBL_cli_id;
                --    r_novedades.nov_dpc_id := NULL;
                 --      r_novedades.nov_descripcion :=
                --             'Recargo s/Factura:'
                --          || LPAD(TO_CHAR(NVL(V_OBL.OBL_nro_factura, 0), '9999999999999999'), 17, ' ');
                --    r_novedades.nov_descripcion :=
                --          r_novedades.nov_descripcion
                --       || ' Vto:'
                --       || TO_CHAR(V_OBL.OBL_FEC_VENCIMIENTO, 'dd/mm/rrrr')
                --       || ' Pag:'
                --       || TO_CHAR(P_APLICADOR.v_fecha_cobro, 'dd/mm/rrrr');
                --    r_novedades.nov_fec_destino := NULL;
                --    r_novedades.nov_tipo_destino := NULL;
                --    r_novedades.nov_cod_iva := V_TIPO_IVA;
                --    r_novedades.nov_usr_alta := p_aplicador.v_usuario;
                --    r_novedades.nov_fec_alta := sysdate;
                --    r_novedades.nov_imp_iva_per := r_importe.percepcion;
               --     r_novedades.nov_imp_iva_ali := r_importe.alicuota;
               --     IF V_TIPO_IVA = 1 THEN
               --        r_novedades.nov_imp_iva_cf := r_importe.iva;
               --     ELSIF V_TIPO_IVA = 2 OR V_TIPO_IVA = 7 THEN
               --        r_novedades.nov_imp_iva_ex := r_importe.iva;
               --     ELSIF V_TIPO_IVA = 4 OR V_TIPO_IVA = 8 THEN
               --        r_novedades.nov_imp_iva_ri := r_importe.iva;
               --     ELSIF V_TIPO_IVA = 3 OR V_TIPO_IVA = 9 THEN
               --        r_novedades.nov_imp_iva_rni := r_importe.iva;
              --      ELSIF V_TIPO_IVA = 6 THEN
              --         r_novedades.nov_imp_iva_mon := r_importe.iva;
              --      END IF;
              --      l_dev_nov := PKG_SERVICIOS_FIJOS.f_insertar_novedad(r_novedades);
                 --   INSERT INTO SR VALUES (to_char(l_dev_nov));            
              --  END;
            -- END IF;   
         ELSE           
           -- Registro ingreso TOTAL --------------------------------------------------------         
           v_aplicador.p_erc_codigo := 7 ;
           v_rta :=fc_Aplica_Parte( v_obl_id, v_aplicador, p_rec_id,'', p_secuencia);
           -------------- Registro Aplicado -------------------------------------------------
           v_aplicador.p_erc_codigo := 1 ;
           v_aplicador.v_importe    := v_obl_saldo ;
           v_rta :=fc_Aplica_Parte( v_obl_id, v_aplicador, p_rec_id, 'Cuota 2', p_secuencia);
           -------------  Registro  Credito por la Diferencia  ------------------------------   
           v_aplicador.v_importe    := p_aplicador.v_importe -  v_obl_saldo ;
           v_aplicador.p_erc_codigo := 2 ;
           v_rta := fc_generar_credito ( v_obl_id,v_aplicador, p_secuencia);
           RETURN 'OK';
         END IF;
      END IF;
      RETURN 'OK';
   END;

-----------------------------------------------------------------------------
 --  FUNCTION recp_0450(p_incorporador PKG_RECAUDACIONES.vincorporacionrec)      RETURN VARCHAR2 AS
 -- COMMIT;  DESCOMENTAR AL FINALIZAR   PRUEBAS LINEA 1650  
      FUNCTION recp_0450 RETURN VARCHAR2 AS
      CURSOR c_recaudaciones_externas(
         p_ente    RECAUDACIONES_EXTERNAS.rep_ere_codigo%TYPE,
         p_fecha   RECAUDACIONES_EXTERNAS.rep_fecha%TYPE) IS
         SELECT   rep_id, rep_registro
             FROM RECAUDACIONES_EXTERNAS
            WHERE rep_fec_baja IS NULL 
            AND rep_ere_codigo = p_ente 
            AND rep_fecha = p_fecha
            and rep_id =  42553602   -- cta '12200015800009'   
         ORDER BY rep_id;

      CURSOR c_servicio(l_obligacion OBLIGACIONES.obl_id%TYPE) IS
         SELECT cct_ser_codigo
           FROM CUENTAS_CORRIENTES
          WHERE cct_obl_id = l_obligacion;

      l_plan_especial            VARCHAR2(2);
      l_salto_aplicador          NUMBER(1);
      l_fecha_proceso            DATE;
      l_obl_saldo                NUMBER(15, 2);
      l_fecha_cierre_contable    DATE;
      l_cod_serv                 VARCHAR2(3);
      l_año_mes_contable         DATE;
      l_retorno                  VARCHAR2(300);
      l_verif_rec_ext            VARCHAR2(2);
      l_verificador              VARCHAR2(2);
      l_codigo_barra             VARCHAR2(60);
      l_anulado                  VARCHAR2(1);
      l_busqueda_alter           NUMBER(1)                                 := 0;
      l_tiene_plan               OBLIGACIONES.obl_ppl_id%TYPE;
      l_nov_imp_neto             CUENTAS_CORRIENTES.cct_imp_debe%TYPE;
      l_nov_imp_iva              CUENTAS_CORRIENTES.cct_imp_iva%TYPE;
      l_nov_imp_ali              CUENTAS_CORRIENTES.cct_imp_ali%TYPE;
      l_tipo_responsable         INMUEBLES.inm_tipo_responsable%TYPE;
      l_tipo_responsable_old     INMUEBLES.inm_tipo_responsable%TYPE;
      l_inmuebles                INMUEBLES.inm_id%TYPE;
      l_sucursal                 SUCURSALES.suc_codigo%TYPE;
      l_dev_nov                  NOVEDADES_FACTURABLES.nov_id%TYPE;
      l_ppl_id                   PLANES_PAGO.ppl_id%TYPE                   := 0;
      l_imp_credito              PLANES_PAGO.ppl_monto_pago_inicial%TYPE;
      l_ppl_cnt_cuotas           PLANES_PAGO.ppl_cnt_cuotas%TYPE;
      l_rep_id                   RECAUDACIONES_EXTERNAS.rep_id%TYPE        := 0;
      l_imp_transicion           RECAUDACIONES.rec_imp_cobrado%TYPE;
      l_rec_id                   RECAUDACIONES.rec_id%TYPE;
      l_secuencia                RECAUDACIONES.rec_secuencia%TYPE;
      l_operacion                RECAUDACIONES.rec_nro_operacion%TYPE;
      l_obligacion               OBLIGACIONES.obl_id%TYPE;
      l_obl_tpc_codigo           OBLIGACIONES.obl_tpc_codigo%TYPE;
      l_cli_id                   OBLIGACIONES.obl_cli_id%TYPE;
      l_obl_cuota_plan           OBLIGACIONES.obl_cuota_plan%TYPE;
      l_boleta_deuda             OBLIGACIONES.obl_boleta_deuda%TYPE;
      r_inserta_recaud           PKG_RECAUDACIONES.vincrecaudacionrec;
      r_aplicador                PKG_RECAUDACIONES.vaplicarrecaurec;
      r_recaudaciones_externas   c_recaudaciones_externas%ROWTYPE;
      r_novedades                NOVEDADES_FACTURABLES%ROWTYPE;
      v_mensaje                  VARCHAR2(300);
      p_incorporador          PKG_RECAUDACIONES.vincorporacionrec;
      l_iva                      PKG_SERVICIOS_FIJOS.ivarec;
      r_importe                  PKG_SERVICIOS_FIJOS.importerec;
      nrta                       registro;
      v_rta  varchar2(400);
      lPasoPPE   Boolean;     -- variable que indica el paso por la rutina externa de PPE 
   BEGIN
      p_incorporador.p_ente := 811;
      p_incorporador.p_fecha := TO_DATE('11052022','DDMMRRRR');
      p_incorporador.v_usuario := 'NMENDEZ';

      /* Determina la fecha contable en base a */
      SELECT MAX(cco_fec_cierre)
        INTO l_fecha_cierre_contable
        FROM CIERRES_CONTABLES;

      r_inserta_recaud.p_se := 'N';
      r_inserta_recaud.p_ente := p_incorporador.p_ente;
      r_inserta_recaud.p_fecha := p_incorporador.p_fecha;
      r_inserta_recaud.v_usuario := p_incorporador.v_usuario;
      r_aplicador.p_ente := p_incorporador.p_ente;
      r_aplicador.p_fecha := p_incorporador.p_fecha;
      r_aplicador.p_rec_id := NULL;
      r_aplicador.p_erc_codigo := NULL;
      r_aplicador.v_usuario := p_incorporador.v_usuario;
      l_año_mes_contable := NULL;
      l_fecha_proceso := SYSDATE;
      /* Verifica que existan los registros a incorporar */
     -- l_retorno := f_verifica_datos(p_incorporador);

      l_retorno := 'S';
      IF l_retorno <> 'S' THEN
         ROLLBACK;
         RETURN l_retorno;
      END IF;

      /* Determina el número de remesa de OSM */
      BEGIN
         SELECT rem__numero
           INTO r_inserta_recaud.v_remesa_osm
           FROM REMESAS
          WHERE rem__fecha = p_incorporador.p_fecha AND rem__ere_codigo = p_incorporador.p_ente AND ROWNUM = 1;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            l_retorno := 'No se encontro numero de remesa. Error: ' || SQLERRM;
            ROLLBACK;
            RETURN l_retorno;
         WHEN OTHERS THEN
            l_retorno := 'En obtención de numero de remesa de OMS. Error: ' || SQLERRM;
            ROLLBACK;
            RETURN l_retorno;
      END;

      r_aplicador.v_remesa_osm := r_inserta_recaud.v_remesa_osm;

      /* Abre el cursor que se utiliza para volcar los distintos
         registro de recaudaciones_externas en recaudaciones  */
      FOR r_recaudaciones_externas IN c_recaudaciones_externas(p_incorporador.p_ente, p_incorporador.p_fecha) LOOP
         
         l_rep_id := r_recaudaciones_externas.rep_id;
         l_salto_aplicador := 1;
         /* Realiza los cortes del archivo que se recupero del plano */
         IF SUBSTR(r_recaudaciones_externas.rep_registro, 65, 2) = '99' THEN
            l_busqueda_alter := 0;
                  r_inserta_recaud.v_factura :=
                     TO_NUMBER(   SUBSTR(r_recaudaciones_externas.rep_registro, 67, 6)
                               || SUBSTR(r_recaudaciones_externas.rep_registro, 40, 10));

         ELSE
            l_busqueda_alter := 1;
            r_inserta_recaud.v_factura := TO_NUMBER(SUBSTR(r_recaudaciones_externas.rep_registro, 40, 10));
         END IF;

         l_verif_rec_ext := SUBSTR(r_recaudaciones_externas.rep_registro, 81, 2);
         l_codigo_barra  := ('00' || SUBSTR(r_recaudaciones_externas.rep_registro, 1, 3))|| SUBSTR(r_recaudaciones_externas.rep_registro, 26, 55);
         l_verificador   := CALCULA_DV_BARRA(l_codigo_barra);
         r_inserta_recaud.p_banco := TO_NUMBER(SUBSTR(r_recaudaciones_externas.rep_registro, 4, 3));
         r_inserta_recaud.v_remesa:= TO_NUMBER(SUBSTR(r_recaudaciones_externas.rep_registro, 7, 3));
         l_secuencia     := TO_NUMBER(SUBSTR(r_recaudaciones_externas.rep_registro, 10, 2));
         r_inserta_recaud.v_fecha_cobro :=
                                    TO_DATE(SUBSTR(r_recaudaciones_externas.rep_registro, 12, 8), 'YYYY-MM-DD');
         l_operacion := TO_NUMBER(SUBSTR(r_recaudaciones_externas.rep_registro, 20, 6));
         r_inserta_recaud.v_identificacion :=
                                            LTRIM(RTRIM(SUBSTR(r_recaudaciones_externas.rep_registro, 26, 14)));
         --v_inserta_recaud.v_factura := TO_NUMBER(SUBSTR(v_recaudaciones_externas.rep_registro,67,6)||SUBSTR(v_recaudaciones_externas.rep_REGISTRO,40,10));
         -- MODIFICADO POR GENDZWEIG 04/12/2020
         r_inserta_recaud.v_importe := TO_NUMBER(SUBSTR(r_recaudaciones_externas.rep_registro, 73, 8)) / 100;
         l_plan_especial := SUBSTR(r_recaudaciones_externas.rep_registro, 62, 2);
         --INSERT INTO BORRAR(A) VALUES('l_plan_especial <> 42');
         IF l_plan_especial <> 42 THEN
             r_inserta_recaud.v_anio    := TO_NUMBER(SUBSTR(r_recaudaciones_externas.rep_registro, 56, 4));
             r_inserta_recaud.v_periodo := TO_NUMBER(SUBSTR(r_recaudaciones_externas.rep_registro, 60, 2));
         ELSE         
             --INSERT INTO BORRAR(A) VALUES('l_plan_especial = 42');
             BEGIN
                  SELECT obl_pef_anio,obl_pef_periodo
                   INTO r_inserta_recaud.v_anio,r_inserta_recaud.v_periodo
                   FROM OBLIGACIONES
                  WHERE obl_id =(SELECT /*+ INDEX (OBLIGACIONES OBL_NRO_FACTURA_IDX) */ 
                                        min(obl_id)
                                   FROM OBLIGACIONES
                                  WHERE obl_nro_factura = r_inserta_recaud.v_factura
                                    AND (obl_saldo      = r_inserta_recaud.v_importe OR obl_saldo = 0)
                                    AND obl_cuenta      = r_inserta_recaud.v_identificacion
                                    AND obl_estado      = 15
                                    AND (obl_tpc_codigo > 80 OR obl_tpc_codigo IN(8, 9))
                                 );
             EXCEPTION WHEN NO_DATA_FOUND THEN 
                 BEGIN
                      SELECT obl_pef_anio,obl_pef_periodo
                       INTO r_inserta_recaud.v_anio,r_inserta_recaud.v_periodo
                       FROM OBLIGACIONES
                      WHERE obl_id =(SELECT /*+ INDEX (OBLIGACIONES OBL_NRO_FACTURA_IDX) */ 
                                            max(obl_id)
                                       FROM OBLIGACIONES
                                      WHERE obl_nro_factura = r_inserta_recaud.v_factura
                                        AND (obl_saldo      = r_inserta_recaud.v_importe OR obl_saldo = 0)
                                        AND obl_cuenta      = r_inserta_recaud.v_identificacion
                                        AND obl_estado      = 30
                                        AND (obl_tpc_codigo > 80 OR obl_tpc_codigo IN(8, 9))
                                     );             
                 EXCEPTION WHEN OTHERS THEN
                    r_inserta_recaud.v_anio    := TO_NUMBER(SUBSTR(r_recaudaciones_externas.rep_registro, 56, 4));
                    r_inserta_recaud.v_periodo := TO_NUMBER(SUBSTR(r_recaudaciones_externas.rep_registro, 60, 2));
                 END;

             WHEN OTHERS THEN
                r_inserta_recaud.v_anio    := TO_NUMBER(SUBSTR(r_recaudaciones_externas.rep_registro, 56, 4));
                r_inserta_recaud.v_periodo := TO_NUMBER(SUBSTR(r_recaudaciones_externas.rep_registro, 60, 2));
             END;
         END IF;
         l_tipo_responsable_old := TO_NUMBER(SUBSTR(r_recaudaciones_externas.rep_registro, 64, 1));         
         l_sucursal := SUBSTR(r_recaudaciones_externas.rep_registro, 26, 3);
         l_anulado  := SUBSTR(r_recaudaciones_externas.rep_registro, 83, 1);
         l_cod_serv := SUBSTR(r_recaudaciones_externas.rep_registro, 53, 3);
         r_aplicador.v_cod_factura := SUBSTR(r_recaudaciones_externas.rep_registro, 50, 3);
         /* Asigna las variables que se usan para llamar a la funcion aplicar_pagos */
         r_aplicador.v_factura     := r_inserta_recaud.v_factura;
         r_aplicador.v_periodo     := r_inserta_recaud.v_periodo;
         r_aplicador.v_anio        := r_inserta_recaud.v_anio;
         r_aplicador.v_identificacion := r_inserta_recaud.v_identificacion;
         r_aplicador.v_fecha_cobro := r_inserta_recaud.v_fecha_cobro;
         r_aplicador.p_banco       := r_inserta_recaud.p_banco;
         r_aplicador.v_remesa      := r_inserta_recaud.v_remesa;
         r_aplicador.v_importe     := r_inserta_recaud.v_importe;
         /**********************************************************************************************/
         -- cambio introducido para las facturas bimestralas con pago en dos cuotas
         -- en las posiciones 62 y 63 del código de barras vendrá un 90 para la cuota 1 y un 91 para la cuota 2, que las identifica
         -- se optó por bifurcar la lógica completa
         -- si es un 90 o 91, llama a una nueva función, sino hace lo mismo que antes (todo el ELSE completo es el código anterior)
         /**********************************************************************************************/
         IF l_plan_especial IN(90,91) THEN
             IF nvl(l_anulado,'X') = 'A' THEN
                l_salto_aplicador := 0;
                r_inserta_recaud.p_estado := 5;
                l_obligacion:= NULL;
                l_rec_id :=
                   incorpora_recaudacion(r_inserta_recaud,
                                         l_año_mes_contable,
                                         l_secuencia,
                                         l_obligacion,
                                         l_operacion);
             ELSE                                     
                 v_rta := PKG_RECAUDACIONES.f_factura_cuotas(l_secuencia, r_aplicador, 0);
                 IF v_rta <> 'OK' THEN
                             l_retorno := 'No se pudo aplicar el pago. Error: ' || SQLERRM;
                             ROLLBACK;
                             RETURN l_retorno;
                 END IF;
             END IF;
         ELSE
             BEGIN
                SELECT inm_id, inm_tipo_responsable
                  INTO l_inmuebles, l_tipo_responsable
                  FROM Manantial.INMUEBLES
                 WHERE inm_cuenta = r_inserta_recaud.v_identificacion;
             EXCEPTION
                WHEN NO_DATA_FOUND THEN
                   l_inmuebles := NULL;
             END;
             BEGIN   /* BEGIN 001 */
                lPasoPPE := False;
                IF l_plan_especial = '43' AND NVL(l_anulado, 'S') <> 'A' THEN                                                                                 /* ABRE IF 005 */
                    /* Si el plan es especial y no se encuentra anulado */
                    ----------------------------------------------------------------------------
                    -- Verificar si el plan especial esta activo y si no ha sido ya generado  --                     
                    ----------------------------------------------------------------------------                    
                   lPasoPPE := pkg_ppe.Control_ppe(r_inserta_recaud, l_operacion,l_secuencia);
                   if lPasoPPE Then   
                      l_plan_especial := '00' ;  
                      l_anulado       := 'A';  
                      r_inserta_recaud.p_estado := 2; 
                   End if;
                end if;     
                -------------------------------------------------------------------------------------------         
                IF l_plan_especial = '43' AND NVL(l_anulado, 'S') <> 'A' THEN                                                                                 /* ABRE IF 005 */
                    /* Si el plan es especial y no se encuentra anulado */                       
                      /* Genera el plan de pago especial */                                                     
                      r_inserta_recaud := PKG_PLANES_ESPECIALES.f_planes_especiales(r_inserta_recaud,l_tipo_responsable_old,l_imp_credito);                   
                   /* Recupera la PK del inmueble */
                   BEGIN   /* BEGIN 002*/
                      SELECT inm_id, inm_tipo_responsable INTO l_inmuebles, l_tipo_responsable FROM INMUEBLES
                       WHERE inm_cuenta = r_inserta_recaud.v_identificacion AND inm_fec_baja IS NULL;
                   EXCEPTION WHEN OTHERS THEN
                         l_retorno := 'No pudo recuperar el Inmueble. Error: ' || SQLERRM;
                         ROLLBACK;
                         RETURN l_retorno;
                   END;   /* BEGIN 002 */

                   IF r_inserta_recaud.p_estado = 2 THEN   /* ABRE IF 006 */
                      l_rec_id := incorpora_recaudacion(r_inserta_recaud,l_año_mes_contable,l_secuencia,l_obligacion,l_operacion);
                      --------------------------------------------------------------------------------------
                      ---------- Determina la novedad insertada en el procedimiento de PKG_PLANES_ESPECIALES
                      --------------------------------------------------------------------------------------
                      BEGIN
                         SELECT MAX(nov_id) INTO l_dev_nov
                           FROM NOVEDADES_FACTURABLES
                          WHERE nov_tipo_novedad = 10
                            AND nov_tipo_origen = 'D'
                            AND nov_estado = 2
                            AND nov_con_inm_id = l_inmuebles
                            AND nov_nro_origen = r_inserta_recaud.v_factura
                            AND TRUNC(nov_fec_novedad) = TRUNC(SYSDATE)
                            AND NOT EXISTS(SELECT 1 FROM REL_REC_NOV WHERE rrn_nov_id = nov_id);
                      EXCEPTION WHEN OTHERS THEN
                            l_dev_nov := 0;
                      END;
                      IF l_dev_nov IS NULL THEN
                         l_dev_nov := 0;
                      END IF;

                      IF l_dev_nov > 0 THEN
                         -----------------------------------------------------------
                         ------ Registro en la rel_rec_nov el credito generado -----
                         -----------------------------------------------------------
                         nRta := crearRel(l_rec_id, 0, l_dev_nov, p_incorporador.v_usuario);
                      -----------------------------------------------------------
                      END IF;

                      l_salto_aplicador := 0;
                   ELSIF r_inserta_recaud.p_estado = 1 THEN
                      r_aplicador.v_factura := r_inserta_recaud.v_factura;
                      r_aplicador.v_importe := r_inserta_recaud.v_importe;
                      r_aplicador.v_periodo := r_inserta_recaud.v_periodo;
                      r_aplicador.v_anio := r_inserta_recaud.v_anio;
                   ELSIF r_inserta_recaud.p_estado = 4 THEN
                      l_imp_transicion := r_inserta_recaud.v_importe;
                      r_inserta_recaud.v_importe := l_imp_credito;
                      r_inserta_recaud.p_estado := 2;
                      l_rec_id := incorpora_recaudacion(r_inserta_recaud,l_año_mes_contable,l_secuencia,l_obligacion,l_operacion);
                      r_inserta_recaud.v_importe := l_imp_transicion;
                      r_inserta_recaud.p_estado := 1;
                      r_aplicador.v_factura := r_inserta_recaud.v_factura;
                      r_aplicador.v_periodo := r_inserta_recaud.v_periodo;
                      r_aplicador.v_anio := r_inserta_recaud.v_anio;
                      --------------------------------------------------------------------------------------
                      ---------- Determina la novedad insertada en el procedimiento de PKG_PLANES_ESPECIALES
                      -- agregado por SROMERO - pedido 6097242 - 28/09/12 --
                      --------------------------------------------------------------------------------------
                      BEGIN
                         SELECT MAX(nov_id) INTO l_dev_nov
                           FROM NOVEDADES_FACTURABLES
                          WHERE nov_tipo_novedad = 10
                            AND nov_tipo_origen = 'D'
                            AND nov_estado = 2
                            AND nov_con_inm_id = l_inmuebles
                            AND nov_nro_origen = r_inserta_recaud.v_factura
                            AND TRUNC(nov_fec_novedad) = TRUNC(SYSDATE)
                            AND NOT EXISTS(SELECT 1 FROM REL_REC_NOV WHERE rrn_nov_id = nov_id);
                      EXCEPTION WHEN OTHERS THEN
                            l_dev_nov := 0;
                      END;

                      IF l_dev_nov IS NULL THEN
                         l_dev_nov := 0;
                      END IF;
                      IF l_dev_nov > 0 THEN
                         -----------------------------------------------------------
                         ------ Registro en la rel_rec_nov el credito generado -----
                         -----------------------------------------------------------
                         nrta := crearrel(l_rec_id, 0, l_dev_nov, p_incorporador.v_usuario);
                      END IF;
                   -- FIN agregado por SROMERO - pedido 6097242 - 28/09/12 --
                   ELSIF r_inserta_recaud.p_estado < 0 THEN
                      --IF r_inserta_recaud.p_estado = -4 THEN
                      l_salto_aplicador := 0;
                      r_inserta_recaud.p_estado := 2;
                      l_rec_id :=
                      incorpora_recaudacion(r_inserta_recaud,l_año_mes_contable,l_secuencia,l_obligacion,l_operacion);
                      --------------------------------------------------------------------------------------
                      ---------- Determina la novedad insertada en el procedimiento de PKG_PLANES_ESPECIALES
                      -- agregado por SROMERO - pedido 6097242 - 28/09/12 --
                      --------------------------------------------------------------------------------------
                      BEGIN
                         SELECT MAX(nov_id) INTO l_dev_nov
                           FROM NOVEDADES_FACTURABLES
                          WHERE nov_tipo_novedad = 10
                            AND nov_tipo_origen = 'D'
                            AND nov_estado = 2
                            AND nov_con_inm_id = l_inmuebles
                            AND nov_nro_origen = r_inserta_recaud.v_factura
                            AND TRUNC(nov_fec_novedad) = TRUNC(SYSDATE)
                            AND NOT EXISTS(SELECT 1 FROM REL_REC_NOV WHERE rrn_nov_id = nov_id);
                      EXCEPTION WHEN OTHERS THEN
                            l_dev_nov := 0;
                      END;

                      IF l_dev_nov IS NULL THEN
                         l_dev_nov := 0;
                      END IF;

                      IF l_dev_nov > 0 THEN
                         -----------------------------------------------------------
                         ------ Registro en la rel_rec_nov el credito generado -----
                         -----------------------------------------------------------
                         nrta := crearrel(l_rec_id, 0, l_dev_nov, p_incorporador.v_usuario);
                      -----------------------------------------------------------
                      END IF;

                      -- FIN agregado por SROMERO - pedido 6097242 - 28/09/12 --

                      /**SR***************************************/
                      l_año_mes_contable :=
                      CALCULO_FEC_CONT(l_fecha_cierre_contable, r_inserta_recaud.v_fecha_cobro, l_fecha_proceso);
                      r_novedades.nov_pef_anio := r_inserta_recaud.v_anio;
                      r_novedades.nov_pef_periodo := r_inserta_recaud.v_periodo;
                      r_novedades.nov_ser_codigo := 650001;
                      r_novedades.nov_con_inm_id := l_inmuebles;
                      r_novedades.nov_tipo_novedad := 10;
                      r_novedades.nov_fec_novedad := l_fecha_proceso;
                      r_novedades.nov_estado := 2;
                      r_novedades.nov_tipo_origen := 'D';
                      r_novedades.nov_nro_origen := NVL(r_inserta_recaud.v_factura, 0);
                      r_novedades.nov_imp_iva_cf := 0;
                      r_novedades.nov_imp_iva_ex := 0;
                      r_novedades.nov_imp_iva_ri := 0;
                      r_novedades.nov_imp_iva_rni := 0;
                      r_novedades.nov_imp_iva_mon := 0;
                      r_novedades.nov_imp_cambio := NULL;
                      r_novedades.nov_inm_id := l_inmuebles;
                      r_novedades.nov_obl_id := l_obligacion;
                      r_novedades.nov_cli_id := l_cli_id;
                      r_novedades.nov_dpc_id := NULL;
                      r_novedades.nov_fec_destino := NULL;
                      r_novedades.nov_tipo_destino := NULL;
                      r_novedades.nov_nro_destino := NULL;
                      r_novedades.nov_cod_iva := l_tipo_responsable;
                      r_novedades.nov_usr_alta := p_incorporador.v_usuario;
                      r_novedades.nov_fec_alta := l_fecha_proceso;
                      IF l_ppl_id > 0 THEN   /* ABRE IF 007 */
                         r_novedades.nov_descripcion :=
                             '1-PAGO DUPLICADO DEL PLAN DE PAGO Nº ' || l_ppl_id || ' CUOTA: ' || l_obl_cuota_plan;
                      ELSE
                         r_novedades.nov_descripcion :=
                                           '1-PAGO DUPLICADO DE LA FAC. Nº ' || NVL(r_inserta_recaud.v_factura, 0);
                      END IF;   /* CIERRA IF 007 */
                      l_iva := PKG_SERVICIOS_FIJOS.f_imp_iva(l_tipo_responsable, 610001, r_inserta_recaud.v_fecha_cobro);
                      r_importe := F_IVA_INVERSO(r_inserta_recaud.v_importe, l_iva.iva, l_iva.alicuota, l_iva.percepcion);
                      l_nov_imp_neto := r_importe.neto;
                      l_nov_imp_iva := r_importe.iva;
                      l_nov_imp_ali := r_importe.alicuota + r_importe.percepcion;
                      r_novedades.nov_imp_neto := l_nov_imp_neto;
                      IF l_tipo_responsable > 6 THEN   /* ABRE IF 008 */
                         r_novedades.nov_imp_iva_ali := 0;
                         r_novedades.nov_imp_iva_per := ABS(l_nov_imp_ali);
                      ELSE
                         r_novedades.nov_imp_iva_ali := ABS(l_nov_imp_ali);
                         r_novedades.nov_imp_iva_per := 0;
                      END IF;   /* CIERRA IF 008 */
                      IF l_tipo_responsable = 1 THEN   /* ABRE IF 009 */
                         r_novedades.nov_imp_iva_cf := ABS(l_nov_imp_iva);
                      ELSIF l_tipo_responsable = 2 OR l_tipo_responsable = 7 THEN
                         r_novedades.nov_imp_iva_ex := ABS(l_nov_imp_iva);
                      ELSIF l_tipo_responsable = 4 OR l_tipo_responsable = 8 THEN
                         r_novedades.nov_imp_iva_ri := ABS(l_nov_imp_iva);
                      ELSIF l_tipo_responsable = 3 OR l_tipo_responsable = 9 THEN
                         r_novedades.nov_imp_iva_rni := ABS(l_nov_imp_iva);
                      ELSIF l_tipo_responsable = 6 THEN
                         r_novedades.nov_imp_iva_mon := ABS(l_nov_imp_iva);
                      END IF;   /* CIERRA IF 009 */
                      l_dev_nov := PKG_SERVICIOS_FIJOS.f_insertar_novedad(r_novedades);
                      ----------------------------------------------------
                      --- Registro en la rel_rec_nov el credito generado -
                      ----------------------------------------------------
                      nrta := crearrel(l_rec_id, 0, l_dev_nov, p_incorporador.v_usuario);
                      ----------------------------------------------------
                      v_mensaje := ACREDITA_DEUDA_ANTERIOR(l_rec_id, p_incorporador.v_usuario);
                   END IF;   /* CIERRA IF 006 */
                END IF;   /* CIERRA IF 005*/

                r_aplicador.v_importe := r_inserta_recaud.v_importe;
                IF NVL(l_anulado, 'S') != 'A' THEN                                                                                  /* ABRE IF 010 */
                   /* Recupera el plan de pago, de no ser un plan de pago retorna 0 */
                   BEGIN   /* BEGIN 003 */
                      SELECT ppl_id, ppl_cnt_cuotas
                        INTO l_ppl_id, l_ppl_cnt_cuotas
                        FROM PLANES_PAGO
                       WHERE ppl_id = r_inserta_recaud.v_factura;
                   EXCEPTION
                      WHEN NO_DATA_FOUND THEN
                         l_ppl_id := 0;
                         l_ppl_cnt_cuotas := NULL;
                   END;   /* BEGIN 003 */

                   /* Bandera para saber si la factura está dentro de un plan de pagos */
                   l_tiene_plan := 0;
                   IF l_plan_especial != '43' OR r_inserta_recaud.p_estado != 2 THEN                                                                               /* ABRE IF 011 */
                      /* Si no es un plan de pago especial y no se encuentra en estado pediente */
                      /* Determina si el registro pertenece a un plan de pago o no */
                      IF l_cod_serv = '089' AND r_aplicador.v_cod_factura = '080' AND l_ppl_id > 0 THEN
                         /* ABRE IF 012 */
                             -- Modificado por GEndzweig si tiene mas de una fila manejar la excepcion
                             /*INSERT INTO BORRAR(A) VALUES('ABRE IF 012 ');
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 012 r_inserta_recaud.v_factura '||r_inserta_recaud.v_factura);
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 012 r_inserta_recaud.v_importe '||r_inserta_recaud.v_importe);
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 012 r_inserta_recaud.v_identificacion '||r_inserta_recaud.v_identificacion);
                             */
                             SELECT   obl_id, obl_inm_id, obl_saldo, obl_tre_codigo, obl_tpc_codigo,obl_cuota_plan, obl_boleta_deuda
                               INTO l_obligacion, l_inmuebles, l_obl_saldo, l_tipo_responsable, l_obl_tpc_codigo,l_obl_cuota_plan, l_boleta_deuda
                               FROM OBLIGACIONES
                              WHERE obl_id = (SELECT /*+ INDEX (OBLIGACIONES OBL_NRO_FACTURA_IDX) */ min(obl_id)
                                                FROM OBLIGACIONES
                                               WHERE obl_nro_factura = r_inserta_recaud.v_factura
                                                 AND (obl_saldo = r_inserta_recaud.v_importe OR obl_saldo = 0)
                                                 AND obl_cuenta = r_inserta_recaud.v_identificacion
                                                 AND obl_estado = 15
                                                 AND (obl_tpc_codigo > 80 OR obl_tpc_codigo IN(8, 9))
                                                 );
                      ELSE   /* IF 012 */
                         IF NVL(l_busqueda_alter, 0) = 0 THEN   /* ABRE IF 013 */
                            BEGIN -- Modificado por GEndzweig si tiene mas de una fila manejar la excepcion
                             /*INSERT INTO BORRAR(A) VALUES('ABRE IF 013 ');
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 013 r_inserta_recaud.v_factura '||r_inserta_recaud.v_factura);
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 013 r_inserta_recaud.v_importe '||r_inserta_recaud.v_importe);
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 013 r_inserta_recaud.v_identificacion '||r_inserta_recaud.v_identificacion);
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 013 r_inserta_recaud.v_anio '||r_inserta_recaud.v_anio);
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 013 r_inserta_recaud.v_periodo '||r_inserta_recaud.v_periodo);
                             */
                                SELECT /*+ INDEX (OBLIGACIONES OBL_NRO_FACTURA_IDX) */
                                       obl_id, obl_inm_id, obl_saldo, obl_tre_codigo, obl_tpc_codigo,
                                       obl_ppl_id, obl_boleta_deuda
                                  INTO l_obligacion, l_inmuebles, l_obl_saldo, l_tipo_responsable, l_obl_tpc_codigo,
                                       l_tiene_plan, l_boleta_deuda
                                  FROM OBLIGACIONES
                                 WHERE obl_nro_factura = r_inserta_recaud.v_factura
                                   AND obl_pef_anio = r_inserta_recaud.v_anio
                                   AND obl_pef_periodo = r_inserta_recaud.v_periodo
                                   AND (obl_saldo = r_inserta_recaud.v_importe OR obl_saldo = 0)
                                   AND obl_cuenta = r_inserta_recaud.v_identificacion
                                   AND obl_estado != 65
                                   AND (obl_tpc_codigo > 80 OR obl_tpc_codigo IN(8, 9));
                            EXCEPTION WHEN TOO_MANY_ROWS THEN 
                             /*INSERT INTO BORRAR(A) VALUES('ABRE IF 013 TOO_MANY_ROWS');
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 013 r_inserta_recaud.v_factura '||r_inserta_recaud.v_factura);
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 013 r_inserta_recaud.v_importe '||r_inserta_recaud.v_importe);
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 013 r_inserta_recaud.v_identificacion '||r_inserta_recaud.v_identificacion);
                             */
                                  BEGIN 
                                      SELECT obl_id      ,obl_inm_id, obl_saldo    , obl_tre_codigo  , obl_tpc_codigo   ,obl_ppl_id, obl_boleta_deuda
                                        INTO l_obligacion,l_inmuebles, l_obl_saldo, l_tipo_responsable, l_obl_tpc_codigo,l_tiene_plan, l_boleta_deuda
                                        FROM OBLIGACIONES
                                       WHERE obl_id =(SELECT /*+ INDEX (OBLIGACIONES OBL_NRO_FACTURA_IDX) */
                                                          min(obl_id)
                                                        FROM OBLIGACIONES
                                                       WHERE obl_nro_factura = r_inserta_recaud.v_factura
                                                         --AND obl_pef_anio = r_inserta_recaud.v_anio
                                                         --AND obl_pef_periodo = r_inserta_recaud.v_periodo
                                                         AND (obl_saldo = r_inserta_recaud.v_importe OR obl_saldo = 0)
                                                         AND obl_cuenta = r_inserta_recaud.v_identificacion
                                                         AND obl_estado = 15
                                                         AND (obl_tpc_codigo > 80 OR obl_tpc_codigo IN(8, 9))
                                                        ); 
                                  EXCEPTION WHEN NO_DATA_FOUND THEN                      
                                      SELECT obl_id      ,obl_inm_id, obl_saldo    , obl_tre_codigo  , obl_tpc_codigo   ,obl_ppl_id, obl_boleta_deuda
                                        INTO l_obligacion,l_inmuebles, l_obl_saldo, l_tipo_responsable, l_obl_tpc_codigo,l_tiene_plan, l_boleta_deuda
                                        FROM OBLIGACIONES
                                       WHERE obl_id =(SELECT /*+ INDEX (OBLIGACIONES OBL_NRO_FACTURA_IDX) */
                                                          max(obl_id)
                                                        FROM OBLIGACIONES
                                                       WHERE obl_nro_factura = r_inserta_recaud.v_factura
                                                         --AND obl_pef_anio = r_inserta_recaud.v_anio
                                                         --AND obl_pef_periodo = r_inserta_recaud.v_periodo
                                                         AND (obl_saldo = r_inserta_recaud.v_importe OR obl_saldo = 0)
                                                         AND obl_cuenta = r_inserta_recaud.v_identificacion
                                                         AND obl_estado = 30
                                                         AND (obl_tpc_codigo > 80 OR obl_tpc_codigo IN(8, 9))
                                                        );
                                  END; 
                            END;
                         ELSE   /* IF 013 */
                            BEGIN -- Modificado por GEndzweig si tiene mas de una fila manejar la excepcion
                             /*INSERT INTO BORRAR(A) VALUES('ABRE IF 013 ELSE');
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 013 r_inserta_recaud.v_factura '||r_inserta_recaud.v_factura);
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 013 r_inserta_recaud.v_importe '||r_inserta_recaud.v_importe);
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 013 r_inserta_recaud.v_identificacion '||r_inserta_recaud.v_identificacion);
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 013 r_inserta_recaud.v_anio '||r_inserta_recaud.v_anio);
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 013 r_inserta_recaud.v_periodo '||r_inserta_recaud.v_periodo);
                            */
                                SELECT obl_id, obl_inm_id, obl_saldo, obl_tre_codigo, obl_tpc_codigo,
                                       obl_ppl_id, obl_boleta_deuda
                                  INTO l_obligacion, l_inmuebles, l_obl_saldo, l_tipo_responsable, l_obl_tpc_codigo,
                                       l_tiene_plan, l_boleta_deuda
                                  FROM OBLIGACIONES
                                 WHERE TO_NUMBER(SUBSTR(LPAD(TO_CHAR(obl_nro_factura), 16, '0'), 7, 10)) =
                                                                                             r_inserta_recaud.v_factura
                                   AND obl_pef_anio = r_inserta_recaud.v_anio
                                   AND obl_pef_periodo = r_inserta_recaud.v_periodo
                                   AND (obl_saldo = r_inserta_recaud.v_importe OR obl_saldo = 0)
                                   AND obl_cuenta = r_inserta_recaud.v_identificacion
                                   AND obl_estado != 65
                                   AND (obl_tpc_codigo > 80 OR obl_tpc_codigo IN(8, 9));
                            EXCEPTION WHEN TOO_MANY_ROWS THEN
                             /*INSERT INTO BORRAR(A) VALUES('ABRE IF 013 ELSE TOO_MANY_ROWS');
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 013 r_inserta_recaud.v_factura '||r_inserta_recaud.v_factura);
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 013 r_inserta_recaud.v_importe '||r_inserta_recaud.v_importe);
                             INSERT INTO BORRAR(A) VALUES(' ABRE IF 013 r_inserta_recaud.v_identificacion '||r_inserta_recaud.v_identificacion);
                             */
                                BEGIN 
                                    SELECT obl_id, obl_inm_id, obl_saldo, obl_tre_codigo, obl_tpc_codigo,obl_ppl_id, obl_boleta_deuda
                                      INTO l_obligacion, l_inmuebles, l_obl_saldo, l_tipo_responsable, l_obl_tpc_codigo,l_tiene_plan, l_boleta_deuda
                                      FROM OBLIGACIONES
                                     WHERE OBL_ID =(SELECT MIN(obl_id)
                                                      FROM OBLIGACIONES
                                                     WHERE TO_NUMBER(SUBSTR(LPAD(TO_CHAR(obl_nro_factura), 16, '0'), 7, 10)) = r_inserta_recaud.v_factura
                                                       --AND obl_pef_anio = r_inserta_recaud.v_anio
                                                       --AND obl_pef_periodo = r_inserta_recaud.v_periodo
                                                       AND (obl_saldo = r_inserta_recaud.v_importe OR obl_saldo = 0)
                                                       AND obl_cuenta = r_inserta_recaud.v_identificacion
                                                       AND obl_estado = 15
                                                       AND (obl_tpc_codigo > 80 OR obl_tpc_codigo IN(8, 9))
                                                       ) ;
                                EXCEPTION WHEN NO_DATA_FOUND THEN
                                    SELECT obl_id, obl_inm_id, obl_saldo, obl_tre_codigo, obl_tpc_codigo,obl_ppl_id, obl_boleta_deuda
                                      INTO l_obligacion, l_inmuebles, l_obl_saldo, l_tipo_responsable, l_obl_tpc_codigo,l_tiene_plan, l_boleta_deuda
                                      FROM OBLIGACIONES
                                     WHERE OBL_ID =(SELECT MAX(obl_id)
                                                      FROM OBLIGACIONES
                                                     WHERE TO_NUMBER(SUBSTR(LPAD(TO_CHAR(obl_nro_factura), 16, '0'), 7, 10)) = r_inserta_recaud.v_factura
                                                       --AND obl_pef_anio = r_inserta_recaud.v_anio
                                                       --AND obl_pef_periodo = r_inserta_recaud.v_periodo
                                                       AND (obl_saldo = r_inserta_recaud.v_importe OR obl_saldo = 0)
                                                       AND obl_cuenta = r_inserta_recaud.v_identificacion
                                                       AND obl_estado = 30
                                                       AND (obl_tpc_codigo > 80 OR obl_tpc_codigo IN(8, 9))
                                                       ) ;
                                END;
                            END;
                         END IF;   /* CIERRA IF 013 */
                      END IF;   /* CIERRA IF 012 */
                   END IF;   /* CIERRA IF 011 */
                END IF;   /* CIERRA IF 010 */

                /* Si la factura está dentro de un plan de pagos setea las otras banderas para que -
                   luego quede acreditado -
                   se agrega la condición de l_obl_tpc_codigo > 10 porque las cuotas siempre pertenecen a un plan - Pedido 2203378 */
                IF NVL(l_tiene_plan, 0) > 0 AND l_obl_tpc_codigo > 10 THEN   /* ABRE IF 014 */
                   l_obl_saldo := 0;
                   l_salto_aplicador := 1;
                END IF;   /* CIERRA IF 014 */
             EXCEPTION   /* BEGIN 001 */
                WHEN NO_DATA_FOUND THEN
                   /* Si no encuentra la obligacion inserta el registro en recaudaciones en estado 3 */
                   l_año_mes_contable := CALCULO_FEC_CONT(l_fecha_cierre_contable, r_inserta_recaud.v_fecha_cobro, l_fecha_proceso);
                   IF l_verificador = l_verif_rec_ext THEN   /* ABRE IF 001 */
                      l_salto_aplicador := 0;
                      r_inserta_recaud.p_estado := 2;
                      l_rec_id := incorpora_recaudacion(r_inserta_recaud,l_año_mes_contable,l_secuencia,l_obligacion,l_operacion);
                      /**SR**********************************************/
                      l_año_mes_contable := CALCULO_FEC_CONT(l_fecha_cierre_contable, r_inserta_recaud.v_fecha_cobro, l_fecha_proceso);
                      r_novedades.nov_pef_anio := r_inserta_recaud.v_anio;
                      r_novedades.nov_pef_periodo := r_inserta_recaud.v_periodo;
                      r_novedades.nov_ser_codigo := 650001;
                      r_novedades.nov_con_inm_id := l_inmuebles;
                      r_novedades.nov_tipo_novedad := 10;
                      r_novedades.nov_fec_novedad := l_fecha_proceso;
                      r_novedades.nov_estado := 2;
                      r_novedades.nov_tipo_origen := 'D';
                      r_novedades.nov_nro_origen := NVL(r_inserta_recaud.v_factura, 0);
                      r_novedades.nov_imp_iva_cf := 0;
                      r_novedades.nov_imp_iva_ex := 0;
                      r_novedades.nov_imp_iva_ri := 0;
                      r_novedades.nov_imp_iva_rni := 0;
                      r_novedades.nov_imp_iva_mon := 0;
                      r_novedades.nov_imp_cambio := NULL;
                      r_novedades.nov_inm_id := l_inmuebles;
                      r_novedades.nov_obl_id := l_obligacion;
                      r_novedades.nov_cli_id := l_cli_id;
                      r_novedades.nov_dpc_id := NULL;
                      r_novedades.nov_fec_destino := NULL;
                      r_novedades.nov_tipo_destino := NULL;
                      r_novedades.nov_nro_destino := NULL;
                      r_novedades.nov_cod_iva := l_tipo_responsable;
                      r_novedades.nov_usr_alta := p_incorporador.v_usuario;
                      r_novedades.nov_fec_alta := l_fecha_proceso;
                      IF l_ppl_id > 0 THEN   /* ABRE IF 002 */
                         r_novedades.nov_descripcion := '2-PAGO DUPLICADO DEL PLAN DE PAGO Nº ' || l_ppl_id || ' CUOTA: ' || l_obl_cuota_plan;
                      ELSE
                         r_novedades.nov_descripcion := '2-PAGO DUPLICADO DE LA FAC. Nº ' || NVL(r_inserta_recaud.v_factura, 0);
                      END IF;   /* CIERRA IF 002 */
                      l_iva := PKG_SERVICIOS_FIJOS.f_imp_iva(l_tipo_responsable, 610001, r_inserta_recaud.v_fecha_cobro);
                      r_importe := F_IVA_INVERSO(r_inserta_recaud.v_importe, l_iva.iva, l_iva.alicuota, l_iva.percepcion);
                      l_nov_imp_neto := r_importe.neto;
                      l_nov_imp_iva := r_importe.iva;
                      l_nov_imp_ali := r_importe.alicuota + r_importe.percepcion;
                      r_novedades.nov_imp_neto := l_nov_imp_neto;
                      IF l_tipo_responsable > 6 THEN   /* ABRE IF 003 */
                         r_novedades.nov_imp_iva_ali := 0;
                         r_novedades.nov_imp_iva_per := ABS(l_nov_imp_ali);
                      ELSE
                         r_novedades.nov_imp_iva_ali := ABS(l_nov_imp_ali);
                         r_novedades.nov_imp_iva_per := 0;
                      END IF;   /* CIERRA IF 003 */

                      IF l_tipo_responsable = 1 THEN   /* ABRE IF 004 */
                         r_novedades.nov_imp_iva_cf := ABS(l_nov_imp_iva);
                      ELSIF l_tipo_responsable = 2 OR l_tipo_responsable = 7 THEN
                         r_novedades.nov_imp_iva_ex := ABS(l_nov_imp_iva);
                      ELSIF l_tipo_responsable = 4 OR l_tipo_responsable = 8 THEN
                         r_novedades.nov_imp_iva_ri := ABS(l_nov_imp_iva);
                      ELSIF l_tipo_responsable = 3 OR l_tipo_responsable = 9 THEN
                         r_novedades.nov_imp_iva_rni := ABS(l_nov_imp_iva);
                      ELSIF l_tipo_responsable = 6 THEN
                         r_novedades.nov_imp_iva_mon := ABS(l_nov_imp_iva);
                      END IF;   /* CIERRA IF 004*/

                      l_dev_nov := PKG_SERVICIOS_FIJOS.f_insertar_novedad(r_novedades);
                      ----------------------------------------------------
                      --- Registro en la rel_rec_nov el credito generado -
                      ----------------------------------------------------
                      nrta := crearrel(l_rec_id, 0, l_dev_nov, p_incorporador.v_usuario);
                      ----------------------------------------------------
                      v_mensaje := ACREDITA_DEUDA_ANTERIOR(l_rec_id, p_incorporador.v_usuario);
                   END IF;   /* CIERRA IF 001*/
             WHEN OTHERS THEN   /* BEGIN 001 */
                   l_retorno :=
                         'OBTENIENDO DATOS DE OBLIGACIONES. ERROR: '
                      || SQLERRM
                      || ' / '
                      || r_inserta_recaud.v_factura
                      || ' / '
                      || r_inserta_recaud.v_anio
                      || ' / '
                      || r_inserta_recaud.v_periodo
                      || '-'
                      || r_inserta_recaud.v_importe
                      || '-'
                      || r_inserta_recaud.v_identificacion;
                   ROLLBACK;
                   RETURN l_retorno;
             END;   /* BEGIN 001 */

             /***********************/
             IF l_anulado = 'A' AND l_salto_aplicador = 1 THEN                                                   /* ABRE IF 015 */
                                                                 /* Si el registro esta anulado lo inserta en la tabla recaudaciones en estado 5 */
                l_salto_aplicador := 0;
                r_inserta_recaud.p_estado := 5;
                --- Modificacion para no superponer lo que hace la rutina de PPE externa  --------
                If Not lPasoPPE  Then   -- No paso por la rutina de PPE EXTERNA --  
                   l_rec_id :=incorpora_recaudacion(r_inserta_recaud,
                                                    l_año_mes_contable,
                                                    l_secuencia,
                                                    l_obligacion,
                                                    l_operacion);
                Else  
                   l_salto_aplicador := 0;                                     
                end if; 
                ------------------------------ Fin de modificacion de PPE  ----------------
                BEGIN   /* BEGIN 004 */
                   UPDATE RECAUDACIONES_EXTERNAS
                      SET rep_fec_mod = l_fecha_proceso,
                          rep_usr_mod = p_incorporador.v_usuario
                    WHERE rep_id = r_recaudaciones_externas.rep_id;
                EXCEPTION
                   WHEN OTHERS THEN
                      l_retorno := 'NO SE PUDO ACTUALIZAR RECAUDACIONES EXTERNAS. ERROR: ' || SQLERRM;
                      ROLLBACK;
                      RETURN l_retorno;
                END;   /* BEGIN 004 */
             ELSE   /* IF 015 */
                IF l_verificador <> l_verif_rec_ext AND l_salto_aplicador = 1 THEN                                                                                 /* ABRE IF 016 */
                                                                                     /* si el digito verifidador no coincide inserta en recaudaciones en estado 4 */
                   r_inserta_recaud.p_estado := 4;
                   l_rec_id :=
                      incorpora_recaudacion(r_inserta_recaud,
                                            l_año_mes_contable,
                                            l_secuencia,
                                            l_obligacion,
                                            l_operacion);
                   l_salto_aplicador := 0;

                   BEGIN   /* BEGIN 005 */
                      UPDATE RECAUDACIONES_EXTERNAS
                         SET rep_fec_mod = l_fecha_proceso,
                             rep_usr_mod = p_incorporador.v_usuario,
                             rep_fec_baja = l_fecha_proceso,
                             rep_usr_baja = p_incorporador.v_usuario
                       WHERE rep_id = r_recaudaciones_externas.rep_id;
                   EXCEPTION   /* BEGIN 005 */
                      WHEN OTHERS THEN
                         l_retorno :=
                               'No se pudo actualizar recaudaciones externas, en diferente recaudador. Error: '
                            || SQLERRM;
                         ROLLBACK;
                         RETURN l_retorno;
                   END;   /* BEGIN 005 */
                END IF;   /* CIERRA IF 016 */
             END IF;   /* CIERRA IF 015 */

             IF l_obl_saldo = 0 AND l_salto_aplicador = 1 THEN                                                   /* ABRE IF A*/
                                                                 /* si el saldo es igual a cero inserta en recaudaciones en esta 2, pago duplicado */
                IF l_boleta_deuda IS NULL THEN   /* ABRE IF 017 */
                   l_salto_aplicador := 0;
                   r_inserta_recaud.p_estado := 2;
                   l_rec_id :=
                      incorpora_recaudacion(r_inserta_recaud,
                                            l_año_mes_contable,
                                            l_secuencia,
                                            l_obligacion,
                                            l_operacion);
                   l_año_mes_contable :=
                         CALCULO_FEC_CONT(l_fecha_cierre_contable, r_inserta_recaud.v_fecha_cobro, l_fecha_proceso);
                   r_novedades.nov_pef_anio := r_inserta_recaud.v_anio;
                   r_novedades.nov_pef_periodo := r_inserta_recaud.v_periodo;
                   r_novedades.nov_ser_codigo := 650001;
                   r_novedades.nov_con_inm_id := l_inmuebles;
                   r_novedades.nov_tipo_novedad := 10;
                   r_novedades.nov_fec_novedad := l_fecha_proceso;
                   r_novedades.nov_estado := 2;
                   r_novedades.nov_tipo_origen := 'D';
                   r_novedades.nov_nro_origen := NVL(r_inserta_recaud.v_factura, 0);
                   r_novedades.nov_imp_iva_cf := 0;
                   r_novedades.nov_imp_iva_ex := 0;
                   r_novedades.nov_imp_iva_ri := 0;
                   r_novedades.nov_imp_iva_rni := 0;
                   r_novedades.nov_imp_iva_mon := 0;
                   r_novedades.nov_imp_cambio := NULL;
                   r_novedades.nov_inm_id := l_inmuebles;
                   r_novedades.nov_obl_id := l_obligacion;
                   r_novedades.nov_cli_id := l_cli_id;
                   r_novedades.nov_dpc_id := NULL;
                   r_novedades.nov_fec_destino := NULL;
                   r_novedades.nov_tipo_destino := NULL;
                   r_novedades.nov_nro_destino := NULL;
                   r_novedades.nov_cod_iva := l_tipo_responsable;
                   r_novedades.nov_usr_alta := p_incorporador.v_usuario;
                   r_novedades.nov_fec_alta := l_fecha_proceso;

                   IF l_ppl_id > 0 THEN   /* ABRE IF B */
                      r_novedades.nov_descripcion :=
                             '3-Pago duplicado del Plan de Pago Nº ' || l_ppl_id || ' cuota: ' || l_obl_cuota_plan;
                   ELSE
                      r_novedades.nov_descripcion :=
                                           '3-Pago duplicado de la Fac. Nº ' || NVL(r_inserta_recaud.v_factura, 0);
                   END IF;   /* CIERRA IF B */

                   SELECT SUM(cct_imp_debe - cct_imp_haber),
                          SUM(DECODE(cct_imp_debe, 0, cct_imp_iva, cct_imp_iva *(-1))),
                          SUM(DECODE(cct_imp_debe, 0, cct_imp_ali, cct_imp_ali *(-1)))
                     INTO l_nov_imp_neto,
                          l_nov_imp_iva,
                          l_nov_imp_ali
                     FROM CUENTAS_CORRIENTES
                    WHERE cct_obl_id = l_obligacion AND cct_ser_codigo != 800088;

                   r_novedades.nov_imp_neto := l_nov_imp_neto + l_nov_imp_iva + l_nov_imp_ali;

                   IF l_tipo_responsable > 6 THEN   /* ABRE IF C */
                      r_novedades.nov_imp_iva_ali := 0;
                      r_novedades.nov_imp_iva_per := ABS(l_nov_imp_ali);
                   ELSE
                      r_novedades.nov_imp_iva_ali := ABS(l_nov_imp_ali);
                      r_novedades.nov_imp_iva_per := 0;
                   END IF;   /* CIERRA IF C */

                   IF l_tipo_responsable = 1 THEN   /* ABRE IF D*/
                      r_novedades.nov_imp_iva_cf := ABS(l_nov_imp_iva);
                   ELSIF l_tipo_responsable = 2 OR l_tipo_responsable = 7 THEN
                      r_novedades.nov_imp_iva_ex := ABS(l_nov_imp_iva);
                   ELSIF l_tipo_responsable = 4 OR l_tipo_responsable = 8 THEN
                      r_novedades.nov_imp_iva_ri := ABS(l_nov_imp_iva);
                   ELSIF l_tipo_responsable = 3 OR l_tipo_responsable = 9 THEN
                      r_novedades.nov_imp_iva_rni := ABS(l_nov_imp_iva);
                   ELSIF l_tipo_responsable = 6 THEN
                      r_novedades.nov_imp_iva_mon := ABS(l_nov_imp_iva);
                   END IF;   /* CIERA IF D */

                   l_dev_nov := PKG_SERVICIOS_FIJOS.f_insertar_novedad(r_novedades);
                   ----------------------------------------------------
                   --- Registro en la rel_rec_nov el credito generado -
                   ----------------------------------------------------
                   nrta := crearrel(l_rec_id, 0, l_dev_nov, p_incorporador.v_usuario);

                   ----------------------------------------------------
                   IF l_dev_nov < 0 THEN   /* ABRE IF F */
                      l_retorno := 'Llamada insertar en novedades facturables, en Saldo 0. Error: ' || SQLERRM;
                      ROLLBACK;
                      RETURN l_retorno;
                   END IF;   /* CIERRA IF F */

                   /* Luego de generar el pago duplicado evalúa si lo puede aplicar a deuda anterior vencida */
                   l_retorno := ACREDITA_DEUDA_ANTERIOR(l_rec_id, p_incorporador.v_usuario);
                ELSE   /* IF 017 */
                   l_salto_aplicador := 0;
                   r_inserta_recaud.p_estado := 3;
                   l_rec_id :=
                      incorpora_recaudacion(r_inserta_recaud,
                                            l_año_mes_contable,
                                            l_secuencia,
                                            l_obligacion,
                                            l_operacion);
                END IF;   /* CIERRA IF 017 */
             END IF;   /* CIERRA IF A */

             /* Si todo esta bien llama a aplicar pagos */
             IF l_salto_aplicador = 1 THEN   /* ABRE IF 018 */
                l_retorno :=
                   aplicar_pagos(r_aplicador,
                                 l_secuencia,
                                 l_obligacion,
                                 l_ppl_id,
                                 l_operacion,
                                 l_busqueda_alter,
                                 l_ppl_cnt_cuotas);

                IF l_retorno <> 'Se han aplicado los pagos correctamente' THEN   /* ABRE IF 019 */
                   ROLLBACK;
                   RETURN l_retorno;
                END IF;   /* CIERRA IF 019 */
             END IF;   /* CIERRA IF 018 */
         END IF;
      END LOOP;

      -- COMMIT;  DESCOMENTAR AL FINALIZAR   PRUEBAS 
      l_retorno := 'Proceso de Incorporación Terminado';
      RETURN l_retorno;
   END recp_0450;

------------------------------------------------------------------------------
--*
--  aplicar Pago sin obligacion
--*
   FUNCTION aplicar_pago_sinobl(
      p_factura   RECAUDACIONES.rec_nro_factura%TYPE,
      p_periodo   RECAUDACIONES.rec_mes_periodo%TYPE,
      p_año       RECAUDACIONES.rec_anio_periodo%TYPE,
      p_cuenta    RECAUDACIONES.rec_cuenta%TYPE,
      p_rec_id    RECAUDACIONES.rec_id%TYPE,
      p_importe   RECAUDACIONES.rec_imp_cobrado%TYPE,
      p_usuario   usuarios.usr_usuario%TYPE)
      RETURN CHAR IS
      v_fecha_hoy         DATE                                := SYSDATE;
      r_importe_iva       PKG_SERVICIOS_FIJOS.importerec;
      r_iva               PKG_SERVICIOS_FIJOS.ivarec;
      v_nov_imp_neto      NUMBER(15, 2)                       := 0;
      v_nov_imp_iva_cf    NUMBER(15, 2)                       := 0;
      v_nov_imp_iva_ex    NUMBER(15, 2)                       := 0;
      v_nov_imp_iva_ri    NUMBER(15, 2)                       := 0;
      v_nov_imp_iva_rni   NUMBER(15, 2)                       := 0;
      v_nov_imp_iva_mon   NUMBER(15, 2)                       := 0;
      v_nov_imp_iva_ali   NUMBER(15, 2)                       := 0;
      v_nov_imp_iva_per   NUMBER(15, 2)                       := 0;
      v_obl_tre_codigo    OBLIGACIONES.obl_tre_codigo%TYPE;
      v_obl_cli_id        OBLIGACIONES.obl_cli_id%TYPE;
      --*agregado por gchavez 131101
      v_inm_id            INMUEBLES.inm_id%TYPE;
      v_nov_id            NOVEDADES_FACTURABLES.nov_id%TYPE;
      v_con_id            Number; 
   BEGIN
      BEGIN
         /* Recupera el tipo de responsable */
         SELECT obl_tre_codigo, obl_cli_id, obl_inm_id
           INTO v_obl_tre_codigo, v_obl_cli_id, v_inm_id
           FROM OBLIGACIONES
          WHERE obl_nro_factura = p_factura
            AND obl_pef_periodo = p_periodo
            AND obl_pef_anio = p_año
            AND obl_cuenta = p_cuenta
            AND ROWNUM = 1;
      EXCEPTION
         WHEN OTHERS THEN
            RETURN SQLERRM;
      END;

      r_iva := PKG_SERVICIOS_FIJOS.f_imp_iva(v_obl_tre_codigo, 650001, v_fecha_hoy);
      /* Determina el IVA y alicuota desde el importe total.. */
      r_importe_iva := F_IVA_INVERSO(p_importe, r_iva.iva, r_iva.alicuota, r_iva.percepcion);
      v_nov_imp_neto := r_importe_iva.neto;

      /* Determina el tipo de IVA.. */
      IF v_obl_tre_codigo = 1 THEN
         v_nov_imp_iva_cf := r_importe_iva.iva;
      ELSIF v_obl_tre_codigo IN(2, 7) THEN
         v_nov_imp_iva_ex := r_importe_iva.iva;

         IF v_obl_tre_codigo = 7 THEN
            v_nov_imp_iva_per := r_importe_iva.percepcion;
         END IF;
      ELSIF v_obl_tre_codigo IN(3, 8) THEN
         v_nov_imp_iva_ri := r_importe_iva.iva;

         IF v_obl_tre_codigo = 8 THEN
            v_nov_imp_iva_per := r_importe_iva.percepcion;
         ELSE
            v_nov_imp_iva_ali := r_importe_iva.alicuota;
         END IF;
      ELSIF v_obl_tre_codigo IN(4, 9) THEN
         v_nov_imp_iva_rni := r_importe_iva.iva;

         IF v_obl_tre_codigo = 9 THEN
            v_nov_imp_iva_per := r_importe_iva.percepcion;
         END IF;
      ELSIF v_obl_tre_codigo = 6 THEN
         v_nov_imp_iva_mon := r_importe_iva.iva;
         v_nov_imp_iva_ali := r_importe_iva.alicuota;
      END IF;

      BEGIN
         SELECT nov_seq.NEXTVAL
           INTO v_nov_id
           FROM DUAL;
      END;
      
      /* --------- Modificado el 30/12/2017 VLUCERO   para  identificar el ID de la primera conexion de Agua del inmueble  TCK : 9347057  */ 
      BEGIN 
        SELECT   min(con_id)  INTO  v_con_id  FROM  Manantial.CONEXIONES, Manantial.INMUEBLES
        WHERE   inm_id =   v_inm_id
        AND     Con_inm_id = inm_id 
        AND     Con_tipo = 1      -- cnx de agua  
        AND     Con_fec_baja IS NULL ; 
        IF v_con_id IS NULL THEN   -- NO HAY CONEXIONES DE AGUA EN EL INMUEBLE , ENTONCES busco una de CLOACAS  
           SELECT   min(con_id)  INTO  v_con_id  FROM  Manantial.CONEXIONES, Manantial.INMUEBLES
            WHERE   inm_id =   v_inm_id
            AND     Con_inm_id = inm_id 
            AND     Con_tipo = 2      -- cnx de Cloacas   
            AND     Con_fec_baja IS NULL ;
        END IF; 
      EXCEPTION WHEN Others THEN
          RETURN 'ERROR: Aplicar_pago_sinobl(): No existe CNX facturable';            
      END ; 
      /*   fin modificacion  */ 

      BEGIN
         INSERT INTO NOVEDADES_FACTURABLES
                     (nov_id, nov_pef_anio, nov_pef_periodo, nov_ser_codigo, nov_con_inm_id, nov_con_id,
                      nov_tipo_novedad, nov_fec_novedad, nov_estado, nov_tipo_origen, nov_nro_origen,
                      nov_imp_neto, nov_imp_iva_cf, nov_imp_iva_ex, nov_imp_iva_ri,
                      nov_imp_iva_rni, nov_imp_iva_mon, nov_imp_iva_ali, nov_imp_iva_per, nov_inm_id,
                      nov_cli_id, nov_descripcion,
                      nov_cod_iva, nov_usr_alta, nov_fec_alta, nov_mac_codigo)
              VALUES (v_nov_id, p_año, p_periodo, 650001, v_inm_id, v_con_id, 
                      10, v_fecha_hoy, 2, 'D', p_factura,
                      v_nov_imp_neto, v_nov_imp_iva_cf, v_nov_imp_iva_ex, v_nov_imp_iva_ri,
                      v_nov_imp_iva_rni, v_nov_imp_iva_mon, v_nov_imp_iva_ali, v_nov_imp_iva_per, v_inm_id,
                      v_obl_cli_id, 'Pago duplicado sobre factura nro. ' || TO_CHAR(p_factura),
                      v_obl_tre_codigo, p_usuario, v_fecha_hoy, NULL);
      EXCEPTION
         WHEN OTHERS THEN
            RETURN SQLERRM;
      END;

      /* Actualiza el estado de la recaudacion */
      BEGIN
         UPDATE RECAUDACIONES
            SET rec_erc_codigo = 2,
                rec_fec_mod = v_fecha_hoy,
                rec_usr_mod = p_usuario
          WHERE rec_id = p_rec_id;
      EXCEPTION
         WHEN OTHERS THEN
            RETURN SQLERRM;
      END;

      BEGIN
         INSERT INTO REL_REC_NOV
                     (rrn_id, rrn_rec_id, rrn_nov_id, rrn_rec_rec_id, rrn_fec_alta, rrn_usr_alta)
              VALUES (rrn_seq.NEXTVAL, p_rec_id, v_nov_id, NULL, SYSDATE, p_usuario);
      EXCEPTION
         WHEN OTHERS THEN
            RETURN SQLERRM;
      END;

      RETURN 'Se han acreditado pagos correctamente';
   EXCEPTION
      WHEN OTHERS THEN
         RETURN SQLERRM;
   END aplicar_pago_sinobl;

/* ********************************************************************** */
/*  A P L I C A D O R     D E     P A G O                                 */
/* ********************************************************************** */
   FUNCTION aplicar_pagos(
      p_aplicarrecaurec            PKG_RECAUDACIONES.vaplicarrecaurec,
      p_secuencia                  RECAUDACIONES.rec_secuencia%TYPE,
      p_obligacion        IN OUT   OBLIGACIONES.obl_id%TYPE,
      p_ppl_id                     PLANES_PAGO.ppl_id%TYPE,
      p_operacion                  RECAUDACIONES.rec_nro_operacion%TYPE,
      p_busqueda_alter             NUMBER := 0,
      p_cnt_cuotas                 PLANES_PAGO.ppl_cnt_cuotas%TYPE := NULL)
      RETURN VARCHAR2 IS
      CURSOR c_cuentas_corrientes(l_obl_id CUENTAS_CORRIENTES.cct_obl_id%TYPE) IS
         SELECT   cct_id_movimiento, MAX(cct_ser_codigo) cct_ser_codigo, MAX(cct_pef_anio) cct_pef_anio,
                  MAX(cct_pef_periodo) cct_pef_periodo, MAX(cct_tipo_movimiento) cct_tipo_movimiento,
                    MAX(cct_imp_debe)
                  - SUM(NVL(DECODE(apa_estado, 30, apa_imp_haber, 31, apa_imp_haber, 0), 0)) cct_imp_debe,
                  SUM(NVL(DECODE(apa_estado, 30, apa_imp_haber, 31, apa_imp_haber, 0), 0)) apa_imp_haber,
                  MAX(cct_fec_generacion) cct_fec_generacion,
                  MAX(cct_fec_cierre_contable) cct_fec_cierre_contable, MAX(cct_usr_alta) cct_usr_alta,
                  MAX(cct_fec_alta) cct_fec_alta, MAX(cct_rec_id) cct_rec_id, MAX(cct_nov_id) cct_nov_id,
                  MAX(cct_obl_id_cuota) cct_obl_id_cuota, MAX(cct_cnt_dias_recargo) cct_cnt_dias_recargo,
                  MAX(cct_fec_vencimiento) cct_fec_vencimiento, MAX(cct_imp_iva) cct_imp_iva,
                  MAX(cct_por_iva) cct_por_iva, MAX(cct_imp_ali) cct_imp_ali, MAX(cct_por_ali) cct_por_ali,
                  MAX(cct_tre_codigo) cct_tre_codigo
             FROM CUENTAS_CORRIENTES, ASIG_PAGOS
            WHERE cct_obl_id = l_obl_id
              AND cct_imp_haber = 0
              AND cct_estado = 15
              AND apa_cct_obl_id(+) = cct_obl_id
              AND apa_cct_id_movimiento(+) = cct_id_movimiento
         GROUP BY cct_obl_id, cct_id_movimiento;

      /* Cursor referenciado para usarlo en forma ascendente o descencente según corresponda */
      TYPE debitos_originales IS REF CURSOR;

      TYPE registro IS RECORD(
         r_obl_id           OBLIGACIONES.obl_id%TYPE,
         r_obl_saldo        OBLIGACIONES.obl_saldo%TYPE,
         r_obl_se           OBLIGACIONES.obl_se%TYPE,
         r_obl_tpc_codigo   OBLIGACIONES.obl_tpc_codigo%TYPE
      );

      c_plan_pago               debitos_originales;
      v_plan_pago               registro;
      stmt_str                  VARCHAR2(2000)
         := 'SELECT /*+ index(obligaciones obl_ppl_fk_i) */ obl_id, obl_saldo, obl_se, obl_tpc_codigo
                                   FROM obligaciones
                                  WHERE obl_ppl_id = :l_ppl_id
                                 AND obl_cuota_plan IS NULL
                                 AND obl_saldo > 0
                              ORDER BY obl_fec_vencimiento';

      CURSOR c_servicio(p_id_temporal DTE_CUENTAS_CORRIENTES.tem_cct_id%TYPE) IS
         SELECT NVL(SUM(tem_cct_monto), 0) tem_cct_monto
           FROM DTE_CUENTAS_CORRIENTES, SERVICIOS
          WHERE tem_cct_servicio = ser_codigo
            AND ser_calcula_iva = 'S'
            AND tem_calcula = 'S'
            AND tem_cct_id = p_id_temporal;

      CURSOR c_ser_sin(p_id_temporal DTE_CUENTAS_CORRIENTES.tem_cct_id%TYPE) IS
         SELECT   tem_cct_servicio, NVL(SUM(tem_cct_monto), 0) tem_cct_monto
             FROM DTE_CUENTAS_CORRIENTES, SERVICIOS
            WHERE tem_cct_servicio = ser_codigo
              AND ser_calcula_iva = 'N'
              AND tem_calcula = 'S'
              AND tem_cct_id = p_id_temporal
         GROUP BY tem_cct_servicio;

      l_año_mes_contable        DATE;
      l_retorno                 VARCHAR2(300);
      l_fecha_proceso           DATE;
      l_fec_proceso             DATE;
      l_fecha_venc_obl          DATE;
      l_monto_recargo           NUMBER(12, 2);
      l_cct_seq                 NUMBER(10);
      l_porcentaje              NUMBER;
      l_fecha_cierre_contable   DATE;   -- Verificar
      l_cuota_plan              OBLIGACIONES.obl_cuota_plan%TYPE;
      l_cuenta                  OBLIGACIONES.obl_cuenta%TYPE;
      l_fecha_eta_mor           OBLIGACIONES.obl_fec_ini_etapa_mor%TYPE;
      l_cod_etapa               OBLIGACIONES.obl_emo_cod_etapa%TYPE;
      l_ppl_id                  OBLIGACIONES.obl_ppl_id%TYPE              := p_ppl_id;
      l_obl_saldo               OBLIGACIONES.obl_saldo%TYPE;
      l_cli_id                  OBLIGACIONES.obl_cli_id%TYPE;
      r_importe_plan            OBLIGACIONES.obl_imp_original%TYPE        := 0;
      /* Capital de las Cuotas a Aplicar sobre débitos originales */
      l_asignado                OBLIGACIONES.obl_saldo%TYPE;
      l_se                      OBLIGACIONES.obl_se%TYPE;
      l_obl_id                  OBLIGACIONES.obl_id%TYPE;
      l_obl_tpc_codigo          OBLIGACIONES.obl_tpc_codigo%TYPE;
      l_obl_pmo_id              OBLIGACIONES.obl_pmo_id%TYPE;
      l_obl_inm_id              OBLIGACIONES.obl_inm_id%TYPE;
      l_cct_imp_debe            CUENTAS_CORRIENTES.cct_imp_debe%TYPE;
      /* Importe de Asig pagos cuando exiten pagos parciales sobre un débitos */
      l_sucursal                SUCURSALES.suc_codigo%TYPE;
      l_grupo                   SUCURSALES.suc_grp_codigo%TYPE;
      r_importe_asig_pagos      ASIG_PAGOS.apa_imp_haber%TYPE;
      l_tipo_responsable        INMUEBLES.inm_tipo_responsable%TYPE;
      l_tipo_iva                INMUEBLES.inm_tipo_responsable%TYPE;
      l_inmuebles               INMUEBLES.inm_id%TYPE;
      l_rec_id                  RECAUDACIONES.rec_id%TYPE;
      l_dev_nov                 NOVEDADES_FACTURABLES.nov_id%TYPE;
      l_cuentas_corrientes      c_cuentas_corrientes%ROWTYPE;
      r_cuentas_corrientes      CUENTAS_CORRIENTES%ROWTYPE;
      r_obligacion              OBLIGACIONES%ROWTYPE;
      r_obl_recuperada          OBLIGACIONES%ROWTYPE;
      r_inserta_recaud          PKG_RECAUDACIONES.vincrecaudacionrec;
      r_datos_iva               PKG_SERVICIOS_FIJOS.ivarec;
      r_importe                 PKG_SERVICIOS_FIJOS.importerec;
      r_asig_pagos              ASIG_PAGOS%ROWTYPE;
      r_aplicador               PKG_RECAUDACIONES.vaplicarrecaurec;
      r_actualizador            PKG_RECAUDACIONES.vactuarecaudrec;
      r_novedades               NOVEDADES_FACTURABLES%ROWTYPE;
      l_cuota                   OBLIGACIONES.obl_cuota_plan%TYPE;
      l_ultima_cuota            OBLIGACIONES.obl_cuota_plan%TYPE;
      l_modelo_plan             PLANES_PAGO.ppl_mpp_id%TYPE;
      v_monto_sin_multa         CUENTAS_CORRIENTES.cct_imp_debe%TYPE      := 0;
      v_monto_multa             CUENTAS_CORRIENTES.cct_imp_debe%TYPE      := 0;
      v_id_temporal             DTE_CUENTAS_CORRIENTES.tem_cct_id%TYPE;
      v_existe_servicio         NUMBER(2);
      v_servicio_insert         SERVICIOS.ser_codigo%TYPE;
      v_concepto                SERVICIOS.ser_des_corta%TYPE;
      l_sin_recargos            NUMBER(2)                                 := 0;
      l_fecha                   DATE;
      l_fec_prescripcion        DATE;
      v_quita_existe            NUMBER;
      --p_aplicarrecaurec       pkg_recaudaciones.vaplicarrecaurec;
      l_pago_inicial            NUMBER(3)                                 := 0;
   -- Utilizado para  controlar si el plan de pago tuvo un pago inicial
   BEGIN
      l_fecha_proceso := SYSDATE;
      l_obl_id := 0;
      l_retorno := 'Se han aplicado los pagos correctamente';
      r_inserta_recaud.p_ente := p_aplicarrecaurec.p_ente;
      r_inserta_recaud.p_fecha := p_aplicarrecaurec.p_fecha;
      r_inserta_recaud.v_importe := p_aplicarrecaurec.v_importe;
      r_inserta_recaud.v_identificacion := p_aplicarrecaurec.v_identificacion;
      r_inserta_recaud.v_fecha_cobro := p_aplicarrecaurec.v_fecha_cobro;
      r_inserta_recaud.v_factura := p_aplicarrecaurec.v_factura;
      r_inserta_recaud.v_anio := p_aplicarrecaurec.v_anio;
      r_inserta_recaud.v_periodo := p_aplicarrecaurec.v_periodo;
      r_inserta_recaud.p_banco := p_aplicarrecaurec.p_banco;
      r_inserta_recaud.v_remesa_osm := p_aplicarrecaurec.v_remesa_osm;
      r_inserta_recaud.v_remesa := p_aplicarrecaurec.v_remesa;
      r_inserta_recaud.v_usuario := p_aplicarrecaurec.v_usuario;
      /* Carga la variables que se usan para la actualizacion de la recaudacion */
      r_actualizador.v_identificacion := p_aplicarrecaurec.v_identificacion;
      r_actualizador.v_factura := p_aplicarrecaurec.v_factura;
      r_actualizador.v_anio := p_aplicarrecaurec.v_anio;
      r_actualizador.v_periodo := p_aplicarrecaurec.v_periodo;
      r_actualizador.p_rec_id := p_aplicarrecaurec.p_rec_id;
      r_actualizador.p_erc_codigo := p_aplicarrecaurec.p_erc_codigo;
      r_actualizador.v_usuario := p_aplicarrecaurec.v_usuario;
      /* Recupera datos de la Obligación a cancelar */
      r_obl_recuperada :=
         f_recupera_obl(p_aplicarrecaurec.p_erc_codigo,
                        l_ppl_id,
                        p_aplicarrecaurec.v_cod_factura,
                        p_busqueda_alter,
                        r_inserta_recaud,
                        p_aplicarrecaurec.v_importe);
      l_obl_saldo := r_obl_recuperada.obl_saldo;
      l_cuota_plan := r_obl_recuperada.obl_cuota_plan;
      l_obl_id := r_obl_recuperada.obl_id;
      l_fecha_venc_obl := r_obl_recuperada.obl_fec_vencimiento;
      l_cuenta := r_obl_recuperada.obl_cuenta;
      l_fecha_eta_mor := r_obl_recuperada.obl_fec_ini_etapa_mor;
      l_cod_etapa := r_obl_recuperada.obl_emo_cod_etapa;
      l_ppl_id := r_obl_recuperada.obl_ppl_id;
      l_cli_id := r_obl_recuperada.obl_cli_id;
      l_tipo_responsable := r_obl_recuperada.obl_tre_codigo;
      l_inmuebles := NVL(r_obl_recuperada.obl_inm_id, r_obl_recuperada.obl_con_inm_id);
      l_grupo := r_obl_recuperada.obl_grp_codigo;
      l_obl_pmo_id := r_obl_recuperada.obl_pmo_id;
      l_obl_tpc_codigo := r_obl_recuperada.obl_tpc_codigo;
      l_cuota := r_obl_recuperada.obl_cuota_plan;
      l_obl_inm_id := r_obl_recuperada.obl_inm_id;
      l_ultima_cuota := 0;
      l_modelo_plan := 0;

      --* AGREGADO POR GCHAVEZ 141101
      IF l_obl_id IS NULL AND p_aplicarrecaurec.p_ente = 2 THEN
         l_retorno :=
            aplicar_pago_sinobl(p_aplicarrecaurec.v_factura,
                                p_aplicarrecaurec.v_periodo,
                                p_aplicarrecaurec.v_anio,
                                p_aplicarrecaurec.v_identificacion,
                                p_aplicarrecaurec.p_rec_id,
                                p_aplicarrecaurec.v_importe,
                                p_aplicarrecaurec.v_usuario);
         RETURN l_retorno;
      END IF;

      --* 141101

      -- modificación pedido 2343266 - SROMERO -
      -- En los siguientes dos queries, se reemplazó add_months(sysdate,mpd_quita*(-1))
      -- por add_months(NVL(PPE_FEC_ALTA,PPL_FEC_ALTA),mpd_quita*(-1)), para lo cual tuvo que
      -- agregarse en el FROM la tabla PLANES_ESPECIALES, y la condición de join al final del WHERE
      BEGIN
         SELECT DECODE(mpp_cuota_quita, 'P', 1, ppl_cnt_cuotas), ppl_mpp_id,
                ADD_MONTHS(NVL(ppe_fec_alta, ppl_fec_alta),
                           MESES_QUITA(NVL(ppe_fec_alta, ppl_fec_alta), ppl_mpp_id, ppl_cnt_cuotas) *(-1))
           INTO l_ultima_cuota, l_modelo_plan,
                l_fec_prescripcion
           FROM PLANES_PAGO, MODELOS_PLANES_PAGO, DETALLE_MODELO_PLAN, PLANES_ESPECIALES
          WHERE ppl_id = l_ppl_id
            AND NVL(ppl_quita, 0) > 0
            AND mpp_id = ppl_mpp_id
            AND mpd_mpp_id(+) = ppl_mpp_id
            AND mpd_cant_cuotas(+) = ppl_cnt_cuotas
            AND mpd_tipo_quita(+) = 'T'
            AND mpd_fec_alta(+) <= ppl_fec_alta
            AND NVL(mpd_fec_baja(+), SYSDATE) >= ppl_fec_alta
            AND ppe_ppl_id(+) = ppl_id;
      EXCEPTION
         WHEN OTHERS THEN
            l_ultima_cuota := 0;
            l_modelo_plan := 0;
            l_fec_prescripcion := NULL;
      END;

      IF l_fec_prescripcion IS NULL THEN
         BEGIN
            SELECT ADD_MONTHS(NVL(ppe_fec_alta, ppl_fec_alta),
                              MESES_QUITA(NVL(ppe_fec_alta, ppl_fec_alta), ppl_mpp_id, ppl_cnt_cuotas) *(-1))
              INTO l_fec_prescripcion
              FROM PLANES_PAGO, MODELOS_PLANES_PAGO, DETALLE_MODELO_PLAN, PLANES_ESPECIALES
             WHERE ppl_id = l_ppl_id
               AND NVL(ppl_quita, 0) > 0
               AND mpp_id = ppl_mpp_id
               AND mpd_mpp_id(+) = ppl_mpp_id
               AND mpd_cant_cuotas(+) = ppl_cnt_cuotas + 1
               AND mpd_tipo_quita(+) = 'T'
               AND mpd_fec_alta(+) <= ppl_fec_alta
               AND NVL(mpd_fec_baja(+), SYSDATE) >= ppl_fec_alta
               AND ppe_ppl_id(+) = ppl_id;
         EXCEPTION
            WHEN NO_DATA_FOUND THEN
               l_fec_prescripcion := NULL;
         END;
      END IF;

      l_sucursal := TO_NUMBER(SUBSTR(r_inserta_recaud.v_identificacion, 1, 3));

      IF l_grupo IS NULL THEN
         BEGIN
            SELECT suc_grp_codigo
              INTO l_grupo
              FROM SUCURSALES
             WHERE suc_codigo = l_sucursal;
         EXCEPTION
            WHEN NO_DATA_FOUND THEN
               l_retorno := 'Recuperando codigo de grupo.';
               ROLLBACK;
               RETURN l_retorno;
         END;
      END IF;

      BEGIN
         SELECT inm_tipo_responsable
           INTO l_tipo_iva
           FROM INMUEBLES
          WHERE inm_id = l_inmuebles;

         --IF l_tipo_responsable IS NULL THEN
         l_tipo_responsable := l_tipo_iva;
      --END IF;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            l_tipo_iva := l_tipo_responsable;
      END;

      IF l_obl_saldo <= 0 THEN
         IF p_aplicarrecaurec.p_erc_codigo = 6 THEN
            -- commit;
            NULL;
         END IF;

         RETURN l_retorno;
      END IF;

      /* Si el saldo de la Obligación es mayor que cero aplica el pago */
      l_año_mes_contable :=
                       CALCULO_FEC_CONT(l_fecha_cierre_contable, r_inserta_recaud.v_fecha_cobro, l_fec_proceso);

      /* Recupera el siguiente id de movimiento de la cuenta corriente */
      BEGIN
         SELECT MAX(cct_id_movimiento) + 1
           INTO l_cct_seq
           FROM CUENTAS_CORRIENTES
          WHERE cct_obl_id = l_obl_id;
      EXCEPTION
         WHEN OTHERS THEN
            l_retorno := 'Obteniendo sequencias. Error: ' || SQLERRM;
            ROLLBACK;
            RETURN l_retorno;
      END;

      BEGIN
         SELECT NVL(MAX(tem_cct_id) + 1, 1)
           INTO v_id_temporal
           FROM DTE_CUENTAS_CORRIENTES;
      EXCEPTION
         WHEN OTHERS THEN
            l_retorno := 'Obteniendo id temporal. Error: ' || SQLERRM;
            ROLLBACK;
            RETURN l_retorno;
      END;

      IF    r_inserta_recaud.v_anio < 1995
         OR p_aplicarrecaurec.v_cod_factura IN('066', '076', '077', '096', '097', '098')
         OR TO_NUMBER(p_aplicarrecaurec.v_cod_factura) BETWEEN 10 AND 50 THEN
         l_se := 'S';
      ELSE
         l_se := 'N';
      END IF;

      IF p_aplicarrecaurec.p_rec_id IS NULL AND p_aplicarrecaurec.p_erc_codigo IS NULL THEN
         r_inserta_recaud.p_estado := 1;
         r_inserta_recaud.p_se := l_se;
         l_rec_id :=
            incorpora_recaudacion(r_inserta_recaud, l_año_mes_contable, p_secuencia, p_obligacion,
                                  p_operacion);
      ELSE
         l_retorno := actualizar_recaudacion(r_actualizador);
         l_rec_id := p_aplicarrecaurec.p_rec_id;
      END IF;

      BEGIN
         SELECT rec_fec_proceso
           INTO l_fec_proceso
           FROM RECAUDACIONES
          WHERE rec_id = l_rec_id;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            l_fec_proceso := l_fecha_proceso;
      END;

      /* De existir un crédito lo distribuye */
      l_retorno :=
         f_aplica_creditos(l_obl_id,
                           p_aplicarrecaurec.v_usuario,
                           l_rec_id,
                           p_aplicarrecaurec.p_banco,
                           p_aplicarrecaurec.v_remesa,
                           p_secuencia,
                           p_aplicarrecaurec.v_fecha_cobro,
                           l_fec_proceso);
      /* Carga la variable registro que se utiliza para inserta en novedades
         facturables, pero solo los datos que no cambian */
      r_cuentas_corrientes.cct_obl_id := l_obl_id;
      r_cuentas_corrientes.cct_id_movimiento := l_cct_seq;
      r_cuentas_corrientes.cct_ser_codigo := 800088;
      r_cuentas_corrientes.cct_pef_anio := r_inserta_recaud.v_anio;
      r_cuentas_corrientes.cct_pef_periodo := r_inserta_recaud.v_periodo;
      r_cuentas_corrientes.cct_tipo_movimiento := l_obl_tpc_codigo;
      r_cuentas_corrientes.cct_imp_debe := 0;
      r_cuentas_corrientes.cct_imp_haber := p_aplicarrecaurec.v_importe;
      r_cuentas_corrientes.cct_concepto :=
            'PAGO CON FACTURA '
         || r_obl_recuperada.obl_nro_factura
         || ' DEL '
         || LPAD(r_obl_recuperada.obl_pef_periodo, 2, '0')
         || '/'
         || r_obl_recuperada.obl_pef_anio;
      r_cuentas_corrientes.cct_fec_generacion := NVL(l_fec_proceso, SYSDATE);
      r_cuentas_corrientes.cct_fec_cierre_contable := NULL;
      r_cuentas_corrientes.cct_usr_alta := p_aplicarrecaurec.v_usuario;
      r_cuentas_corrientes.cct_fec_alta := l_fecha_proceso;
      r_cuentas_corrientes.cct_rec_id := l_rec_id;
      r_cuentas_corrientes.cct_nov_id := NULL;
      r_cuentas_corrientes.cct_fec_vencimiento := NULL;
      r_cuentas_corrientes.cct_cnt_dias_recargo := NULL;
      r_cuentas_corrientes.cct_banco := r_inserta_recaud.p_banco;
      r_cuentas_corrientes.cct_remesa := p_aplicarrecaurec.v_remesa;
      r_cuentas_corrientes.cct_secuencia := p_secuencia;
      r_cuentas_corrientes.cct_fec_aplicacion := l_fecha_proceso;
      r_cuentas_corrientes.cct_fec_proceso := NVL(l_fec_proceso, SYSDATE);
      r_cuentas_corrientes.cct_fec_cobro := r_inserta_recaud.v_fecha_cobro;
      r_cuentas_corrientes.cct_usr_baja := NULL;
      r_cuentas_corrientes.cct_fec_baja := NULL;
      r_cuentas_corrientes.cct_usr_mod := NULL;
      r_cuentas_corrientes.cct_fec_mod := NULL;
      r_cuentas_corrientes.cct_estado := 30;
      r_cuentas_corrientes.cct_se := l_se;
      r_cuentas_corrientes.cct_obl_id_cuota := NULL;
      r_cuentas_corrientes.cct_grp_codigo := l_grupo;
      r_cuentas_corrientes.cct_cuenta := r_inserta_recaud.v_identificacion;
      r_cuentas_corrientes.cct_suc_codigo := l_sucursal;
      r_cuentas_corrientes.cct_tre_codigo := l_tipo_responsable;
      r_cuentas_corrientes.cct_imp_iva := 0;
      r_cuentas_corrientes.cct_por_iva := 0;
      r_cuentas_corrientes.cct_imp_ali := 0;
      r_cuentas_corrientes.cct_por_ali := 0;
      /* Llamada a la insercion en cuentas corrientes */
      inserta_cuenta_corriente(r_cuentas_corrientes);

      /* REcorre los débitos originales a los que aplico el pago */
      FOR l_cuentas_corrientes IN c_cuentas_corrientes(l_obl_id) LOOP
         v_existe_servicio := 0;
         r_asig_pagos.apa_tipo_movimiento := l_cuentas_corrientes.cct_tipo_movimiento;
         r_asig_pagos.apa_imp_haber := l_cuentas_corrientes.cct_imp_debe;
         r_asig_pagos.apa_concepto :=
               'PAGO CON FACTURA '
            || r_obl_recuperada.obl_nro_factura
            || ' DEL '
            || LPAD(r_obl_recuperada.obl_pef_periodo, 2, '0')
            || '/'
            || r_obl_recuperada.obl_pef_anio;
         r_asig_pagos.apa_fec_generacion := NVL(l_cuentas_corrientes.cct_fec_generacion, SYSDATE);
         r_asig_pagos.apa_fec_contabilizacion := l_cuentas_corrientes.cct_fec_cierre_contable;
         r_asig_pagos.apa_fec_vencimiento := l_cuentas_corrientes.cct_fec_vencimiento;
         r_asig_pagos.apa_banco := r_inserta_recaud.p_banco;
         r_asig_pagos.apa_remesa := p_aplicarrecaurec.v_remesa;
         r_asig_pagos.apa_fec_aplicacion := NVL(l_fecha_proceso, SYSDATE);
         r_asig_pagos.apa_fec_proceso := NVL(l_fec_proceso, SYSDATE);
         r_asig_pagos.apa_fec_cobro := r_inserta_recaud.v_fecha_cobro;
         r_asig_pagos.apa_estado := 30;
         r_asig_pagos.apa_cuenta := r_inserta_recaud.v_identificacion;
         r_asig_pagos.apa_imp_iva := l_cuentas_corrientes.cct_imp_iva;
         r_asig_pagos.apa_por_iva := l_cuentas_corrientes.cct_por_iva;
         r_asig_pagos.apa_imp_ali := l_cuentas_corrientes.cct_imp_ali;
         r_asig_pagos.apa_por_ali := l_cuentas_corrientes.cct_por_ali;
         r_asig_pagos.apa_cct_obl_id := l_obl_id;
         r_asig_pagos.apa_cct_id_movimiento := l_cuentas_corrientes.cct_id_movimiento;
         r_asig_pagos.apa_ser_codigo := l_cuentas_corrientes.cct_ser_codigo;
         r_asig_pagos.apa_grp_codigo := l_grupo;
         r_asig_pagos.apa_suc_codigo := l_sucursal;
         r_asig_pagos.apa_pef_anio := l_cuentas_corrientes.cct_pef_anio;
         r_asig_pagos.apa_pef_periodo := l_cuentas_corrientes.cct_pef_periodo;
         r_asig_pagos.apa_tre_codigo := l_cuentas_corrientes.cct_tre_codigo;
         r_asig_pagos.apa_fec_alta := l_fecha_proceso;
         r_asig_pagos.apa_usr_alta := p_aplicarrecaurec.v_usuario;
         r_asig_pagos.apa_fec_mod := NULL;
         r_asig_pagos.apa_usr_mod := NULL;
         r_asig_pagos.apa_fec_baja := NULL;
         r_asig_pagos.apa_usr_baja := NULL;
         r_asig_pagos.apa_rec_id := l_rec_id;
         r_asig_pagos.apa_se := l_se;

         IF l_cuentas_corrientes.cct_ser_codigo IN(640109, 800089) THEN
            r_importe_plan := l_cuentas_corrientes.cct_imp_debe;
         END IF;

         /* Calcula los montos de los servicios por si existe multa */
         --IF l_cuentas_corrientes.cct_ser_codigo IN (780001,780002) THEN
         --   v_monto_multa := v_monto_multa + l_cuentas_corrientes.cct_imp_debe;
         --ELSE
         --   v_monto_sin_multa := v_monto_sin_multa + l_cuentas_corrientes.cct_imp_debe;
         --END IF;

         /****/
         BEGIN
            SELECT 1
              INTO v_existe_servicio
              FROM DTE_CUENTAS_CORRIENTES
             WHERE tem_cct_id = v_id_temporal
               AND tem_cct_servicio = l_cuentas_corrientes.cct_ser_codigo
               AND tem_cct_obl_id = l_obl_id;
         EXCEPTION
            WHEN NO_DATA_FOUND THEN
               BEGIN
                  INSERT INTO DTE_CUENTAS_CORRIENTES
                              (tem_cct_id, tem_calcula, tem_cct_servicio, tem_cct_obl_id,
                               tem_cct_monto)
                       VALUES (v_id_temporal, 'S', l_cuentas_corrientes.cct_ser_codigo, l_obl_id,
                               l_cuentas_corrientes.cct_imp_debe);
               EXCEPTION
                  WHEN OTHERS THEN
                     ROLLBACK;
                     l_retorno := 'Insertando en temporal. Error: ' || SQLERRM;
                     RETURN l_retorno;
               END;
         END;

         /* Si el servicio existe actualiza */
         IF v_existe_servicio = 1 THEN
            UPDATE DTE_CUENTAS_CORRIENTES
               SET tem_cct_monto = tem_cct_monto + l_cuentas_corrientes.cct_imp_debe
             WHERE tem_cct_id = v_id_temporal
               AND tem_cct_servicio = l_cuentas_corrientes.cct_ser_codigo
               AND tem_cct_obl_id = l_obl_id;
         END IF;

         /* Inserta en asig pagos */
         r_asig_pagos.apa_id := inserta_asig_pagos(r_asig_pagos);

         IF l_ppl_id IS NOT NULL AND l_ppl_id > 0 AND l_cuentas_corrientes.cct_ser_codigo IN(640106, 640108) 
            AND l_cuentas_corrientes.cct_imp_debe > 0 THEN  -- agregado por SROMERO - pedido 11123731 - 30/09/20
            /* Carga los datos en la variable que se utiliza para insertar en novedades facturables */
            r_novedades.nov_pef_anio := NULL;
            r_novedades.nov_pef_periodo := NULL;
            r_novedades.nov_con_inm_id := l_inmuebles;
            r_novedades.nov_tipo_novedad := 40;
            r_novedades.nov_fec_novedad := l_fecha_proceso;
            r_novedades.nov_estado := 2;
            r_novedades.nov_tipo_origen := 'M';
            r_novedades.nov_nro_origen := NVL(l_ppl_id, 0);
            r_novedades.nov_imp_cambio := 1;
            r_novedades.nov_inm_id := l_inmuebles;
            r_novedades.nov_obl_id := p_obligacion;
            r_novedades.nov_cli_id := l_cli_id;
            r_novedades.nov_dpc_id := NULL;
            r_novedades.nov_fec_destino := NULL;
            r_novedades.nov_tipo_destino := NULL;
            r_novedades.nov_nro_destino := NULL;
            r_novedades.nov_cod_iva := l_tipo_iva;
            r_novedades.nov_usr_alta := p_aplicarrecaurec.v_usuario;
            r_novedades.nov_fec_alta := l_fecha_proceso;
            r_novedades.nov_imp_iva_cf := 0;
            r_novedades.nov_imp_iva_ex := 0;
            r_novedades.nov_imp_iva_ri := 0;
            r_novedades.nov_imp_iva_rni := 0;
            r_novedades.nov_imp_iva_mon := 0;
            r_novedades.nov_imp_iva_ali := 0;
            r_novedades.nov_imp_iva_per := 0;
            r_novedades.nov_imp_neto :=
                 l_cuentas_corrientes.cct_imp_debe
               - l_cuentas_corrientes.cct_imp_iva
               - l_cuentas_corrientes.cct_imp_ali;
            r_datos_iva :=
               PKG_SERVICIOS_FIJOS.f_imp_iva(l_tipo_responsable,
                                             l_cuentas_corrientes.cct_ser_codigo,
                                             l_fecha_proceso);

            IF r_datos_iva.iva IS NULL THEN
               ROLLBACK;
               l_retorno := 'Llamada a función calculo de I.V.A.. Error: ' || SQLERRM;
               RETURN l_retorno;
            END IF;

            /* Calcula el IVA correspondiente, en caso que tenga una nueva categoria */
            r_importe.iva := (r_datos_iva.iva * r_novedades.nov_imp_neto) / 100;
            r_importe.alicuota := (r_datos_iva.alicuota * r_novedades.nov_imp_neto) / 100;
            r_importe.percepcion := (r_datos_iva.percepcion *(r_novedades.nov_imp_neto + r_importe.iva)) / 100;
            r_novedades.nov_cod_iva := l_tipo_responsable;

            /* Si el codigo de servicio es interes o recargo inserta en
               novedades factureables, para que se genere la factura faltate */
            IF l_cuentas_corrientes.cct_ser_codigo = 640106 THEN
               r_novedades.nov_descripcion :=
                     'Interes de Financ. Plan de Pago Nro.: '
                  || LPAD(l_ppl_id, 10, '0')
                  || ' Cuota Nro.: '
                  || LPAD(l_cuota_plan, 2, '0');
            ELSIF l_cuentas_corrientes.cct_ser_codigo = 640108 THEN
               r_novedades.nov_descripcion :=
                     'Recargo de Actualiz.  P.de Pago Nro.: '
                  || LPAD(l_ppl_id, 10, '0')
                  || ' Cuota Nro.: '
                  || LPAD(l_cuota_plan, 2, '0');
            END IF;

            r_novedades.nov_imp_neto := l_cuentas_corrientes.cct_imp_debe;
            r_novedades.nov_ser_codigo := l_cuentas_corrientes.cct_ser_codigo;
            l_dev_nov := PKG_SERVICIOS_FIJOS.f_insertar_novedad(r_novedades);

            IF l_cuentas_corrientes.cct_ser_codigo = 640106 THEN
               r_novedades.nov_descripcion :=
                     'Su Pago Intereses  Plan de Pago Nro.: '
                  || LPAD(l_ppl_id, 10, '0')
                  || ' Cuota Nro.: '
                  || LPAD(l_cuota_plan, 2, '0');
            ELSIF l_cuentas_corrientes.cct_ser_codigo = 640108 THEN
               r_novedades.nov_descripcion :=
                     'Su Pago Recargos de Act. P.Pago Nro.: '
                  || LPAD(l_ppl_id, 10, '0')
                  || ' Cuota Nro.: '
                  || LPAD(l_cuota_plan, 2, '0');
            END IF;

            r_novedades.nov_imp_neto :=
                 (  l_cuentas_corrientes.cct_imp_debe
                  - l_cuentas_corrientes.cct_imp_iva
                  - l_cuentas_corrientes.cct_imp_ali)
               * (-1);
            l_dev_nov := PKG_SERVICIOS_FIJOS.f_insertar_novedad(r_novedades);

            IF l_cuentas_corrientes.cct_ser_codigo = 640106 THEN
               r_novedades.nov_descripcion :=
                     'Su Pago IVA s/Inte.Plan de Pago Nro.: '
                  || LPAD(l_ppl_id, 10, '0')
                  || ' Cuota Nro.: '
                  || LPAD(l_cuota_plan, 2, '0');
            ELSIF l_cuentas_corrientes.cct_ser_codigo = 640108 THEN
               r_novedades.nov_descripcion :=
                     'Su Pago IVA s/Recargos   P.Pago Nro.: '
                  || LPAD(l_ppl_id, 10, '0')
                  || ' Cuota Nro.: '
                  || LPAD(l_cuota_plan, 2, '0');
            END IF;

            r_novedades.nov_imp_neto :=
                                       (l_cuentas_corrientes.cct_imp_iva + l_cuentas_corrientes.cct_imp_ali)
                                     * (-1);
            l_dev_nov := PKG_SERVICIOS_FIJOS.f_insertar_novedad(r_novedades);
         END IF;

         /* Actualiza el estado del movimiento de cuentas corrientes */
         UPDATE CUENTAS_CORRIENTES
            SET cct_estado = 30,
                cct_fec_cobro = r_inserta_recaud.v_fecha_cobro,
                cct_fec_aplicacion = l_fecha_proceso,
                cct_fec_proceso = NVL(l_fec_proceso, SYSDATE),
                cct_fec_mod = l_fecha_proceso,
                cct_usr_mod = p_aplicarrecaurec.v_usuario,
                cct_rec_id = l_rec_id,
                cct_banco = r_inserta_recaud.p_banco,
                cct_remesa = p_aplicarrecaurec.v_remesa,
                cct_secuencia = p_secuencia
          WHERE cct_obl_id = l_obl_id AND cct_id_movimiento = l_cuentas_corrientes.cct_id_movimiento;
      END LOOP;

      /* Actualiza obligaciones */
      BEGIN
         l_obl_saldo := l_obl_saldo - p_aplicarrecaurec.v_importe;

         UPDATE OBLIGACIONES
            SET obl_saldo = 0,
                obl_estado = 30,
                obl_fec_aplicacion = l_fecha_proceso,
                obl_fec_mod = l_fec_proceso,
                obl_usr_mod = p_aplicarrecaurec.v_usuario
          WHERE obl_id = l_obl_id;

         /* Si la obligación cancelada tiene etapa de morosidad activa, el procedimiento
            valida que el resto de las obligaciones de esa etapa siguen cumpliendo las
            condiciones de la etapa. Si esto no se da las vuelve a etapa cero */
         IF l_obl_pmo_id IS NOT NULL THEN
            NULL;   -- (SR) hasta tanto se defina si va desde la selección o confirmación
         --pkg_etapas_morosidad.valida_proceso_morosidad (l_obl_pmo_id,l_cuenta);
         END IF;
      EXCEPTION
         WHEN OTHERS THEN
            l_retorno := 'Actualizacion en obligaciones. Error: ' || SQLERRM;
            ROLLBACK;
            RETURN l_retorno;
      END;

      BEGIN
         UPDATE RECLAMOS
            SET rcl_estado = 'R',
                rcl_fec_mod = SYSDATE,
                rcl_usr_mod = p_aplicarrecaurec.v_usuario
          WHERE rcl_cuenta = r_inserta_recaud.v_identificacion
            --AND rcl_fecha = r_inserta_recaud.v_fecha_cobro
            AND rcl_nro_factura = r_inserta_recaud.v_factura
            AND rcl_anio_periodo = r_inserta_recaud.v_anio
            AND rcl_mes_periodo = r_inserta_recaud.v_periodo;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            NULL;
         WHEN OTHERS THEN
            l_retorno := 'Actualizacion en Reclamos. Error: ' || SQLERRM;
            ROLLBACK;
            RETURN l_retorno;
      END;

      ----------- Control de la existencia de la cuota del plan de pago en la tabla
      ----------- de cuotas reimpresas de planes de plagos.
      -----------                       Modificado : 05/02/2004   VLUCERO / ASEGATORE
      /*Begin
         Select 1
         into   l_Sin_recargos
         From   PpeFinal1
         Where  inmueble = l_inmuebles
            and  oblid      = l_obl_id
            and  origen     = 2 ;

      Exception When Others Then
          l_Sin_recargos := 0 ;
      End;
      */
      -----------------------------------------------------------------------------
      BEGIN
         SELECT MAX(nov_fec_hasta)
           INTO l_fecha
           FROM NOVEDADES_FACTURABLES
          WHERE nov_tipo_novedad = 45 AND nov_obl_id = l_obl_id;
      EXCEPTION
         WHEN OTHERS THEN
            l_fecha := TRUNC(SYSDATE - 1);
      END;

      IF l_fecha IS NULL THEN
         l_fecha := TRUNC(SYSDATE - 1);
      END IF;

      l_sin_recargos := 1;

      IF l_fecha < TRUNC(SYSDATE) THEN
         l_sin_recargos := 0;
      END IF;

      ------------------------------------------------------------------------------
      ------------------------------------------------------------------------------
      IF     r_inserta_recaud.v_fecha_cobro > l_fecha_venc_obl
         AND p_aplicarrecaurec.p_ente != 2
         AND   /* SR: Si la vía es débito automático no genera RPFT */
             l_sin_recargos = 0 THEN
         /* Determina si parte del monto cobrado pertenece a una multa si es asi calcula el racargo por
            la parte del monto que no pertenece a la multa, si no lo hace por el total cobrado          */
         --IF V_MONTO_MULTA = 0 THEN
         -- l_monto_recargo := calculo_recargos(l_fecha_venc_obl,
         --                                     r_inserta_recaud.v_fecha_cobro,
         --                                     p_aplicarrecaurec.v_importe);
         --ELSE
         -- l_monto_recargo := calculo_recargos(l_fecha_venc_obl,
         --                                     r_inserta_recaud.v_fecha_cobro,
         --                                     v_monto_sin_multa);
         --END IF;
         FOR r_servicio IN c_servicio(v_id_temporal) LOOP
            l_monto_recargo :=
                  CALCULO_RECARGOS(l_fecha_venc_obl, r_inserta_recaud.v_fecha_cobro, r_servicio.tem_cct_monto);
         END LOOP;

         IF NVL(l_monto_recargo, 0) > 0 THEN
            /* Determina el tipo de origen dependiendo si esta en una etapa judicial o no */
            IF r_obl_recuperada.obl_boleta_deuda IS NOT NULL THEN
               r_novedades.nov_tipo_origen := 'U';
               r_novedades.nov_nro_destino := r_obl_recuperada.obl_boleta_deuda;
            ELSE
               r_novedades.nov_tipo_origen := 'Z';
               r_novedades.nov_nro_destino := NULL;
            END IF;

            r_datos_iva := PKG_SERVICIOS_FIJOS.f_imp_iva(l_tipo_responsable, 640110, l_fecha_proceso);

            IF r_datos_iva.iva IS NULL THEN
               ROLLBACK;
               l_retorno := 'Llamada a función calculo de I.V.A. (Cod. Ser. 640110). Error: ' || SQLERRM;
               RETURN l_retorno;
            END IF;

            /* Calcula el IVA correspondiente, en caso que tenga una nueva categoria */
            r_importe.iva := (r_datos_iva.iva * l_monto_recargo) / 100;
            r_importe.alicuota := (r_datos_iva.alicuota * l_monto_recargo) / 100;
            r_importe.percepcion := (r_datos_iva.percepcion *(l_monto_recargo + r_importe.iva)) / 100;
            /* Completa y genera la Novedad Facturable de Recargos por Pago Fuera de Término */
            r_novedades.nov_imp_neto := l_monto_recargo;
            r_novedades.nov_pef_anio := r_inserta_recaud.v_anio;
            r_novedades.nov_pef_periodo := r_inserta_recaud.v_periodo;
            r_novedades.nov_ser_codigo := 640110;
            r_novedades.nov_con_inm_id := l_inmuebles;
            r_novedades.nov_tipo_novedad := 1;
            r_novedades.nov_fec_novedad := l_fecha_proceso;
            r_novedades.nov_estado := 2;
            r_novedades.nov_nro_origen := l_obl_id;
            r_novedades.nov_imp_iva_cf := 0;
            r_novedades.nov_imp_iva_ex := 0;
            r_novedades.nov_imp_iva_ri := 0;
            r_novedades.nov_imp_iva_rni := 0;
            r_novedades.nov_imp_iva_mon := 0;
            r_novedades.nov_imp_iva_ali := 0;
            r_novedades.nov_imp_iva_per := 0;
            r_novedades.nov_imp_cambio := 1;
            r_novedades.nov_inm_id := l_inmuebles;
            r_novedades.nov_obl_id := p_obligacion;
            r_novedades.nov_cli_id := l_cli_id;
            r_novedades.nov_dpc_id := NULL;

            IF l_ppl_id IS NOT NULL AND l_ppl_id > 0 THEN
               r_novedades.nov_descripcion :=
                     'Recargo s/Plan Pago:'
                  || LPAD(TO_CHAR(NVL(r_inserta_recaud.v_factura, 0), '9999999999999'), 14, ' ');
            ELSE
               r_novedades.nov_descripcion :=
                     'Recargo s/Factura:'
                  || LPAD(TO_CHAR(NVL(r_inserta_recaud.v_factura, 0), '9999999999999999'), 17, ' ');
            END IF;

            r_novedades.nov_descripcion :=
                  r_novedades.nov_descripcion
               || ' Vto:'
               || TO_CHAR(l_fecha_venc_obl, 'dd/mm/rrrr')
               || ' Pag:'
               || TO_CHAR(r_inserta_recaud.v_fecha_cobro, 'dd/mm/rrrr');
            r_novedades.nov_fec_destino := NULL;
            r_novedades.nov_tipo_destino := NULL;
            r_novedades.nov_cod_iva := l_tipo_responsable;
            r_novedades.nov_usr_alta := p_aplicarrecaurec.v_usuario;
            r_novedades.nov_fec_alta := l_fecha_proceso;
            r_novedades.nov_imp_iva_per := r_importe.percepcion;
            r_novedades.nov_imp_iva_ali := r_importe.alicuota;

            IF l_tipo_responsable = 1 THEN
               r_novedades.nov_imp_iva_cf := r_importe.iva;
            ELSIF l_tipo_responsable = 2 OR l_tipo_responsable = 7 THEN
               r_novedades.nov_imp_iva_ex := r_importe.iva;
            ELSIF l_tipo_responsable = 4 OR l_tipo_responsable = 8 THEN
               r_novedades.nov_imp_iva_ri := r_importe.iva;
            ELSIF l_tipo_responsable = 3 OR l_tipo_responsable = 9 THEN
               r_novedades.nov_imp_iva_rni := r_importe.iva;
            ELSIF l_tipo_responsable = 6 THEN
               r_novedades.nov_imp_iva_mon := r_importe.iva;
            END IF;

            l_dev_nov := PKG_SERVICIOS_FIJOS.f_insertar_novedad(r_novedades);

            IF l_dev_nov < 0 THEN
               l_retorno := 'Llamada a insercción en Novedades Facturables (Rec. PFT). Error: ' || SQLERRM;
               ROLLBACK;
               RETURN l_retorno;
            END IF;
         END IF;

         l_monto_recargo := 0;

         /* Calcula recargos si la factura tiene una multa */
         --IF v_monto_multa > 0 THEN
         --   l_monto_recargo := Calculo_Recargos(l_fecha_venc_obl,
         --                                       r_inserta_recaud.v_fecha_cobro,
         --                                       v_monto_multa);
         --END IF;
         FOR r_ser_sin IN c_ser_sin(v_id_temporal) LOOP
            l_monto_recargo :=
                   CALCULO_RECARGOS(l_fecha_venc_obl, r_inserta_recaud.v_fecha_cobro, r_ser_sin.tem_cct_monto);

            BEGIN
               /* Busca el servicio para insertar */
               SELECT rss_ser_codigo2, ser_des_corta
                 INTO v_servicio_insert, v_concepto
                 FROM REL_SERVICIO_SERVICIO, SERVICIOS
                WHERE rss_ser_codigo = r_ser_sin.tem_cct_servicio
                  AND rss_tipo = 1
                  AND rss_ser_codigo2 = ser_codigo;
            EXCEPTION
               WHEN OTHERS THEN
                  RETURN('Recuperando servicio 2 ' || SQLERRM);
            END;

            IF     NVL(l_monto_recargo, 0) > 0
               AND p_aplicarrecaurec.p_ente != 2   /* SR: Si la vía es débito automático no genera RPFT */
                                                THEN
               /* Determina el tipo de origen dependiendo si esta en una etapa judicial o no */
               IF r_obl_recuperada.obl_boleta_deuda IS NOT NULL THEN
                  r_novedades.nov_tipo_origen := 'U';
                  r_novedades.nov_nro_destino := r_obl_recuperada.obl_boleta_deuda;
               ELSE
                  r_novedades.nov_tipo_origen := 'Z';
                  r_novedades.nov_nro_destino := NULL;
               END IF;

               /* Completa y genera la Novedad Facturable de Recargos por Pago Fuera de Término */
               r_novedades.nov_pef_anio := r_inserta_recaud.v_anio;
               r_novedades.nov_pef_periodo := r_inserta_recaud.v_periodo;
               r_novedades.nov_ser_codigo := v_servicio_insert;   --780003;
               r_novedades.nov_con_inm_id := l_inmuebles;
               r_novedades.nov_tipo_novedad := 1;
               r_novedades.nov_fec_novedad := l_fecha_proceso;
               r_novedades.nov_estado := 2;
               r_novedades.nov_nro_origen := l_obl_id;
               r_novedades.nov_imp_neto := l_monto_recargo;
               r_novedades.nov_imp_iva_cf := 0;
               r_novedades.nov_imp_iva_ex := 0;
               r_novedades.nov_imp_iva_ri := 0;
               r_novedades.nov_imp_iva_rni := 0;
               r_novedades.nov_imp_iva_mon := 0;
               r_novedades.nov_imp_iva_ali := 0;
               r_novedades.nov_imp_iva_per := 0;
               r_novedades.nov_imp_cambio := 1;
               r_novedades.nov_inm_id := l_inmuebles;
               r_novedades.nov_obl_id := p_obligacion;
               r_novedades.nov_cli_id := l_cli_id;
               r_novedades.nov_dpc_id := NULL;
               --IF l_ppl_id IS NOT NULL AND l_ppl_id > 0 THEN
               --  r_novedades.nov_descripcion  := 'Recargo s/Plan Pago:'||LPAD(TO_CHAR(NVL(r_inserta_recaud.v_factura,0),'99999999999999'),14,' ');
               --ELSE
               --  r_novedades.nov_descripcion  := 'Recargo s/Factura:'||LPAD(TO_CHAR(NVL(r_inserta_recaud.v_factura,0),'9999999999999999'),17,' ');
               --END IF;
               r_novedades.nov_descripcion :=
                     v_concepto
                  || ': '
                  || LPAD(TO_CHAR(NVL(r_inserta_recaud.v_factura, 0), '9999999999999999'), 17, ' ');
               r_novedades.nov_descripcion :=
                     r_novedades.nov_descripcion
                  || ' Vto:'
                  || TO_CHAR(l_fecha_venc_obl, 'dd/mm/rrrr')
                  || ' Pag:'
                  || TO_CHAR(r_inserta_recaud.v_fecha_cobro, 'dd/mm/rrrr');
               r_novedades.nov_fec_destino := NULL;
               r_novedades.nov_tipo_destino := NULL;
               r_novedades.nov_cod_iva := l_tipo_responsable;
               r_novedades.nov_usr_alta := p_aplicarrecaurec.v_usuario;
               r_novedades.nov_fec_alta := l_fecha_proceso;
               r_novedades.nov_imp_iva_per := 0;
               r_novedades.nov_imp_iva_ali := 0;
               r_novedades.nov_imp_iva_cf := 0;
               r_novedades.nov_imp_iva_ex := 0;
               r_novedades.nov_imp_iva_ri := 0;
               r_novedades.nov_imp_iva_rni := 0;
               r_novedades.nov_imp_iva_mon := 0;
               l_dev_nov := PKG_SERVICIOS_FIJOS.f_insertar_novedad(r_novedades);

               IF l_dev_nov < 0 THEN
                  l_retorno := 'Llamada a insercción en Novedades Facturables por Multa. Error: ' || SQLERRM;
                  ROLLBACK;
                  RETURN l_retorno;
               END IF;
            END IF;
         END LOOP;
      END IF;

      IF l_cuenta IS NOT NULL THEN
         IF p_aplicarrecaurec.p_ente = 10 OR p_aplicarrecaurec.p_erc_codigo = 6 THEN
            BEGIN
               UPDATE BASE_MOROSIDAD
                  SET bmo_ebm_codigo = 3,
                      bmo_usr_mod = p_aplicarrecaurec.v_usuario,
                      bmo_fec_mod = l_fecha_proceso
                WHERE bmo_cuenta = l_cuenta
                  AND bmo_fec_inicio_etapa = l_fecha_eta_mor
                  AND bmo_ebm_codigo = l_cod_etapa;
            EXCEPTION
               WHEN OTHERS THEN
                  l_retorno := 'Insertando en base morosidad. Error: ' || SQLERRM;
                  ROLLBACK;
                  RETURN l_retorno;
            END;
         END IF;
      END IF;

      IF l_cuota_plan >= 0 AND NVL(r_importe_plan, 0) > 0 AND l_ppl_id > 0 THEN
         IF l_modelo_plan > 0 THEN
            stmt_str := stmt_str || ' desc';
         ELSE
            stmt_str := stmt_str || ' asc';
         END IF;

         OPEN c_plan_pago FOR stmt_str USING(l_ppl_id);

         LOOP
            FETCH c_plan_pago
             INTO v_plan_pago;

            EXIT WHEN c_plan_pago%NOTFOUND;
            --FOR v_plan_pago IN c_plan_pago(l_ppl_id) LOOP   /* Levanta las obligaciones de planes de pago que todavía no han sido canseladas */
            l_asignado := 0;
            l_retorno :=
               f_aplica_creditos(v_plan_pago.r_obl_id,
                                 p_aplicarrecaurec.v_usuario,
                                 l_rec_id,
                                 r_inserta_recaud.p_banco,
                                 p_aplicarrecaurec.v_remesa,
                                 1,
                                 r_inserta_recaud.v_fecha_cobro,
                                 NVL(l_fec_proceso, SYSDATE));

            FOR l_cuentas_corrientes IN c_cuentas_corrientes(v_plan_pago.r_obl_id) LOOP
               IF NVL(r_importe_plan, 0) > 0 AND l_cuentas_corrientes.cct_imp_debe > 0 THEN
                  r_importe_asig_pagos := l_cuentas_corrientes.apa_imp_haber;
                  l_cct_imp_debe := l_cuentas_corrientes.cct_imp_debe;
                  r_asig_pagos.apa_tipo_movimiento := l_cuentas_corrientes.cct_tipo_movimiento;
                  r_asig_pagos.apa_concepto := 'PAGOS';
                  r_asig_pagos.apa_fec_generacion := NVL(l_cuentas_corrientes.cct_fec_generacion, SYSDATE);
                  r_asig_pagos.apa_fec_contabilizacion := l_cuentas_corrientes.cct_fec_cierre_contable;
                  r_asig_pagos.apa_fec_vencimiento := l_cuentas_corrientes.cct_fec_vencimiento;
                  r_asig_pagos.apa_banco := r_inserta_recaud.p_banco;
                  r_asig_pagos.apa_remesa := p_aplicarrecaurec.v_remesa;
                  r_asig_pagos.apa_fec_aplicacion := NVL(l_fecha_proceso, SYSDATE);
                  r_asig_pagos.apa_fec_proceso := NVL(l_fec_proceso, SYSDATE);
                  r_asig_pagos.apa_fec_cobro := r_inserta_recaud.v_fecha_cobro;
                  r_asig_pagos.apa_cuenta := r_inserta_recaud.v_identificacion;
                  r_asig_pagos.apa_cct_obl_id := v_plan_pago.r_obl_id;
                  r_asig_pagos.apa_cct_id_movimiento := l_cuentas_corrientes.cct_id_movimiento;
                  r_asig_pagos.apa_ser_codigo := l_cuentas_corrientes.cct_ser_codigo;
                  r_asig_pagos.apa_rec_id := l_rec_id;
                  r_asig_pagos.apa_grp_codigo := l_grupo;
                  r_asig_pagos.apa_suc_codigo := l_sucursal;
                  r_asig_pagos.apa_pef_anio := l_cuentas_corrientes.cct_pef_anio;
                  r_asig_pagos.apa_pef_periodo := l_cuentas_corrientes.cct_pef_periodo;
                  r_asig_pagos.apa_tre_codigo := l_cuentas_corrientes.cct_tre_codigo;
                  r_asig_pagos.apa_fec_alta := l_fecha_proceso;
                  r_asig_pagos.apa_usr_alta := p_aplicarrecaurec.v_usuario;
                  r_asig_pagos.apa_fec_mod := NULL;
                  r_asig_pagos.apa_usr_mod := NULL;
                  r_asig_pagos.apa_fec_baja := NULL;
                  r_asig_pagos.apa_usr_baja := NULL;
                  r_asig_pagos.apa_se := l_se;
                  r_asig_pagos.apa_estado := 30;

                  IF l_cct_imp_debe <= r_importe_plan THEN
                     /* Si el importe a aplicar es mayor que el saldo del débito de la Cta. Cte.
                        inserta asig_pagos con estado 30 y cambia el estado en la Cta. Cte, */
                     r_asig_pagos.apa_imp_haber := l_cct_imp_debe;

                     IF r_importe_asig_pagos = 0 THEN
                        /* Si se cancela el total del débito se toma el iva de la cta. cte.*/
                        r_asig_pagos.apa_imp_iva := l_cuentas_corrientes.cct_imp_iva;
                        r_asig_pagos.apa_por_iva := l_cuentas_corrientes.cct_por_iva;
                        r_asig_pagos.apa_imp_ali := l_cuentas_corrientes.cct_imp_ali;
                        r_asig_pagos.apa_por_ali := l_cuentas_corrientes.cct_por_ali;
                     ELSE
                        /* Si se cancela PARCIALMENTE el débito se calcula el iva de la cta. cte.*/
                        l_porcentaje := (l_cct_imp_debe * 100) / l_cuentas_corrientes.cct_imp_debe;
                        r_asig_pagos.apa_imp_iva := (l_cuentas_corrientes.cct_imp_iva * l_porcentaje) / 100;
                        r_asig_pagos.apa_imp_ali := (l_cuentas_corrientes.cct_imp_ali * l_porcentaje) / 100;
                     END IF;

                     r_asig_pagos.apa_imp_haber := l_cct_imp_debe;
                     r_asig_pagos.apa_id := inserta_asig_pagos(r_asig_pagos);

                     UPDATE CUENTAS_CORRIENTES
                        SET cct_estado = 30,
                            cct_fec_cobro = r_inserta_recaud.v_fecha_cobro,
                            cct_fec_aplicacion = l_fecha_proceso,
                            cct_fec_proceso = NVL(l_fec_proceso, SYSDATE),
                            cct_fec_mod = l_fecha_proceso,
                            cct_usr_mod = p_aplicarrecaurec.v_usuario,
                            cct_rec_id = l_rec_id,
                            cct_banco = r_inserta_recaud.p_banco,
                            cct_remesa = p_aplicarrecaurec.v_remesa,
                            cct_secuencia = p_secuencia
                      WHERE cct_obl_id = v_plan_pago.r_obl_id
                        AND cct_id_movimiento = l_cuentas_corrientes.cct_id_movimiento;

                     l_asignado := l_asignado + l_cct_imp_debe;
                     r_importe_plan := r_importe_plan - l_cct_imp_debe;
                  ELSE
                     /* Si el importe a aplicar es MENOR que el saldo del débito de la Cta. Cte.
                        inserta asig_pagos */
                     UPDATE CUENTAS_CORRIENTES
                        SET cct_fec_cobro = r_inserta_recaud.v_fecha_cobro,
                            cct_fec_aplicacion = l_fecha_proceso,
                            cct_fec_proceso = NVL(l_fec_proceso, SYSDATE),
                            cct_fec_mod = l_fecha_proceso,
                            cct_usr_mod = p_aplicarrecaurec.v_usuario,
                            cct_rec_id = l_rec_id,
                            cct_banco = r_inserta_recaud.p_banco,
                            cct_remesa = p_aplicarrecaurec.v_remesa,
                            cct_secuencia = p_secuencia
                      WHERE cct_obl_id = v_plan_pago.r_obl_id
                        AND cct_id_movimiento = l_cuentas_corrientes.cct_id_movimiento;

                     l_porcentaje := (r_importe_plan * 100) / l_cuentas_corrientes.cct_imp_debe;
                     r_asig_pagos.apa_imp_iva := (l_cuentas_corrientes.cct_imp_iva * l_porcentaje) / 100;
                     r_asig_pagos.apa_imp_ali := (l_cuentas_corrientes.cct_imp_ali * l_porcentaje) / 100;
                     r_asig_pagos.apa_imp_haber := r_importe_plan;
                     r_asig_pagos.apa_id := inserta_asig_pagos(r_asig_pagos);
                     l_asignado := l_asignado + r_importe_plan;
                     r_importe_plan := 0;
                  END IF;
               END IF;

               IF r_importe_plan <= 0 THEN
                  EXIT;
               /* Si el importe ya se aplico completamente no se siguen recorriendo los débitos de la Cta. Cte. */
               END IF;
            END LOOP;

            BEGIN
               SELECT MAX(cct_id_movimiento) + 1
                 INTO l_cct_seq
                 FROM CUENTAS_CORRIENTES
                WHERE cct_obl_id = v_plan_pago.r_obl_id;
            EXCEPTION
               WHEN OTHERS THEN
                  l_retorno := 'Obteniendo sequencia de la Cta. Cte. (Déb. Originales) Error: ' || SQLERRM;
                  ROLLBACK;
                  RETURN l_retorno;
            END;

            /* Inserta el pago en la Cta. Cte.*/
            r_cuentas_corrientes.cct_obl_id := v_plan_pago.r_obl_id;
            r_cuentas_corrientes.cct_id_movimiento := l_cct_seq;
            r_cuentas_corrientes.cct_imp_haber := l_asignado;
            r_cuentas_corrientes.cct_se := v_plan_pago.r_obl_se;
            r_cuentas_corrientes.cct_tipo_movimiento := v_plan_pago.r_obl_tpc_codigo;
            /* Llamada a la insercion en cuentas corrientes */
            inserta_cuenta_corriente(r_cuentas_corrientes);

            /* Actualiza las obligaciones pertenecietes a un plan de pago */
            IF v_plan_pago.r_obl_saldo > l_asignado THEN
               BEGIN
                  UPDATE OBLIGACIONES
                     SET obl_saldo = obl_saldo - l_asignado,
                         obl_usr_mod = p_aplicarrecaurec.v_usuario,
                         obl_fec_mod = l_fecha_proceso,
                         obl_fec_aplicacion = l_fecha_proceso
                   WHERE obl_id = v_plan_pago.r_obl_id;
               EXCEPTION
                  WHEN OTHERS THEN
                     ROLLBACK;
                     l_retorno :=
                        'Actualiza Obligaciones (Saldo mayor a cero), por planes de pagos. Error: ' || SQLERRM;
                     RETURN l_retorno;
               END;
            ELSE
               BEGIN
                  UPDATE OBLIGACIONES
                     SET obl_saldo = 0,
                         obl_estado = 30,
                         obl_fec_aplicacion = SYSDATE,
                         obl_usr_mod = p_aplicarrecaurec.v_usuario,
                         obl_fec_mod = l_fecha_proceso
                   WHERE obl_id = v_plan_pago.r_obl_id;
               EXCEPTION
                  WHEN OTHERS THEN
                     l_retorno := 'Actualiza Obligaciones, por planes de pagos. Error: ' || SQLERRM;
                     RETURN l_retorno;
               END;

               BEGIN
                  UPDATE RECLAMOS
                     SET rcl_estado = 'R',
                         rcl_fec_mod = SYSDATE,
                         rcl_usr_mod = p_aplicarrecaurec.v_usuario
                   WHERE rcl_cuenta = r_inserta_recaud.v_identificacion
                     --AND rcl_fecha = r_INSERTA_RECAUD.v_fecha_cobro
                     AND rcl_nro_factura = r_inserta_recaud.v_factura
                     AND rcl_anio_periodo = r_inserta_recaud.v_anio
                     AND rcl_mes_periodo = r_inserta_recaud.v_periodo;
               EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                     NULL;
                  WHEN OTHERS THEN
                     l_retorno := 'Actualizacion en Reclamos. Error: ' || SQLERRM;
                     ROLLBACK;
                     RETURN l_retorno;
               END;
            END IF;

            IF r_importe_plan <= 0 THEN
               EXIT;
            /* Si el importe ya se aplico completamente no se siguen recorriendo los débitos de la Cta. Cte. */
            END IF;
         END LOOP;

         CLOSE c_plan_pago;

         IF (l_ultima_cuota = l_cuota OR(l_cuota = 0 AND l_ultima_cuota = 1)) AND l_modelo_plan > 0 THEN
            l_pago_inicial := 0;

            IF l_ultima_cuota = l_cuota AND l_ultima_cuota > 0 THEN
               BEGIN
                  SELECT COUNT(*)
                    INTO l_pago_inicial
                    FROM OBLIGACIONES, PLANES_ESPECIALES
                   WHERE obl_ppl_id = l_ppl_id
                     AND obl_tpc_codigo = 9
                     AND obl_estado = 30
                     AND obl_ppl_id = ppe_ppl_id
                     AND NVL(ppe_monto_pago_inicial, 0) = 0;
               EXCEPTION
                  WHEN OTHERS THEN
                     l_pago_inicial := 0;
               END;
            END IF;

            IF NVL(l_pago_inicial, 0) = 0 THEN
               /* Modificado por SRomero, antes de generar la NC por la quita valida si ya hizo la quita
                  en la primera cuota, por los casos en que se modifica en el modelo en qué cuota se debe
                  hacer la quita.
                  Se dio un caso en que el modelo decía generarla en la 1ra cuota, y se generó cuando entró
                  el pago de esa cuota, luego el modelo fue cambiado a "última", y cuando entró el pago de
                  la última cuota intentó generar de nuevo la quita, provocando un error que cortó la
                  incorporación de la cobranza.
                  Como solución rápida se valida si ya existe la quita, porque las soluciones ideales serían
                  bajar el dato a la cabecera del plan, o llevar un histórico de los modelos. */
               v_quita_existe := 0;

               BEGIN
                  SELECT 1
                    INTO v_quita_existe
                    FROM OBLIGACIONES
                   WHERE obl_ppl_id = l_ppl_id AND obl_tpc_codigo BETWEEN 60 AND 79;
               EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                     NULL;
               END;

               IF NVL(v_quita_existe, 0) = 0 THEN
                  GENERA_NC_QUITA(l_ppl_id,
                                  l_cli_id,
                                  l_obl_inm_id,
                                  l_cuenta,
                                  l_grupo,
                                  l_tipo_responsable,
                                  p_aplicarrecaurec.v_usuario,
                                  l_fec_prescripcion);
               END IF;
            END IF;
         END IF;
      END IF;

      IF p_aplicarrecaurec.p_erc_codigo = 6 THEN
         --COMMIT;
         NULL;
      END IF;

      l_retorno := 'Se han aplicado los pagos correctamente';
      RETURN l_retorno;
   END aplicar_pagos;

/* ****************************************************************** */
/*  Calcula la Fecha Contable                                         */
/* ****************************************************************** */
   FUNCTION calculo_fecha_contable(v_fecha_cobro DATE, l_fecha_proceso DATE)
      RETURN DATE AS
      l_año_mes_contable   DATE;
      v_año_mes_evento     NUMBER;
      v_año_mes_cierre     NUMBER;
      v_fecha_cierre       DATE;
      v_fecha              DATE;
      v_año_mes_proceso    NUMBER;
   BEGIN
      SELECT MAX(cco_fec_cierre)
        INTO v_fecha_cierre
        FROM CIERRES_CONTABLES;

      v_año_mes_evento := TO_NUMBER(TO_CHAR(v_fecha_cobro, 'rrrrmm'));
      v_año_mes_proceso := TO_NUMBER(TO_CHAR(l_fecha_proceso, 'rrrrmm'));
      v_año_mes_cierre := TO_NUMBER(TO_CHAR(v_fecha_cierre, 'rrrrmm'));

      IF l_fecha_proceso >= v_fecha_cierre THEN
         IF v_año_mes_evento < v_año_mes_proceso THEN
            l_año_mes_contable := TO_DATE(TO_CHAR(v_año_mes_proceso), 'yyyymm');
         ELSE
            l_año_mes_contable := TO_DATE(TO_CHAR(v_año_mes_evento), 'yyyymm');
         END IF;
      ELSE
         IF v_año_mes_evento = v_año_mes_cierre THEN
            l_año_mes_contable := TO_DATE(TO_CHAR(v_año_mes_evento), 'yyyymm');
         ELSE
            v_fecha := TO_DATE(TO_CHAR(v_año_mes_cierre), 'yyyymm');
            l_año_mes_contable := ADD_MONTHS(v_fecha, -1);
         END IF;
      END IF;

      RETURN l_año_mes_contable;
   END calculo_fecha_contable;

/*-----------------------------------------------------------------------------------------------
Nueva funcion para el calculo de actualizaciones de importes
Ticket  1025257
Vlucero  20/05/2013
Version inicial
-----------------------------------------------------------------------------------------------*/
   FUNCTION CALCULO_RECARGOS(pdesde DATE, phasta DATE, pimporte NUMBER)
      RETURN NUMBER IS
      CURSOR ctasas(desde DATE) IS
         SELECT   tar_inicio_vig ini, tar_fin_vig fin, tar_tasa tasa
             FROM manantial.TASAS_RECARGOS
            WHERE tar_fin_vig >= desde AND tar_inicio_vig <= desde AND tar_fec_baja IS NULL
         ORDER BY tar_inicio_vig;

      rtasa       ctasas%ROWTYPE;
      nmonto      NUMBER           := 0;   -- Monto de actualizacion
      nimporte    NUMBER           := 0;
      nted        NUMBER           := 0;   -- Tasa efectiva diaria
      ndias       NUMBER           := 0;
      ndesde      DATE;
      ncantidad   NUMBER           := 0;
      nmin        NUMBER           := 0;
      nmax        NUMBER           := 0;
      nfecdesde   DATE             := TRUNC(pdesde);
      nfechasta   DATE             := TRUNC(phasta);
   BEGIN
      --Validacion de  parametros

      -- sromero - pedido 6557876 - 10/07/2013 - si la obl aún no vence retorna 0.
      IF nfecdesde >= nfechasta THEN
         RETURN 0;
      END IF;
      
      RETURN( MANANTIAL.ACT_IMPORTE (pDesde , pHasta , pImporte));
      
      ---- Fin de la  rutina  --
      --   modificado el 12/08/2020   VLUCERO   tck_11037030
      
      --- Determino inicio
      BEGIN
         SELECT   tar_id
             INTO nmin
             FROM manantial.TASAS_RECARGOS
            WHERE tar_fin_vig >= nfecdesde AND tar_inicio_vig <= nfecdesde AND tar_fec_baja IS NULL
         ORDER BY tar_inicio_vig;
      EXCEPTION
         WHEN OTHERS THEN
            RETURN(0);
      END;

      ----- detecto   Final
      BEGIN
         SELECT   tar_id
             INTO nmax
             FROM manantial.TASAS_RECARGOS
            WHERE tar_fin_vig >= nfechasta AND tar_inicio_vig <= nfechasta AND tar_fec_baja IS NULL
         ORDER BY tar_inicio_vig;
      EXCEPTION
         WHEN OTHERS THEN
            RETURN(0);
      END;

      --Determinacion de cantidad de tasas  a utilizar
      BEGIN
         SELECT COUNT(DISTINCT tar_tasa)
           INTO ncantidad
           FROM manantial.TASAS_RECARGOS
          WHERE tar_id >= nmin AND tar_id <= nmax AND tar_fec_baja IS NULL;
      EXCEPTION
         WHEN OTHERS THEN
            ncantidad := 0;
      END;

      IF ncantidad IS NULL THEN
         ncantidad := 0;
      END IF;

      IF ncantidad = 0 THEN
         RETURN(0);
      END IF;

      -- Determino tasa para el periodo  inicial
      OPEN ctasas(nfecdesde);

      FETCH ctasas
       INTO rtasa;

      IF ctasas%NOTFOUND THEN
         RETURN(0);
      END IF;

      IF ncantidad = 1 THEN   -- se debe calcular para solo una sola tasa todo el periodo
         ndias :=(nfechasta - nfecdesde);
         nted := ROUND(POWER((1 +(rtasa.tasa / 100)),(1 / 30)) - 1, 6);
         nmonto := POWER(1 + nted, ndias) - 1;
         nmonto := nmonto * pimporte;
         RETURN(nmonto);
      ELSE   --- se deben recorrer los periodos secuencialmente para cada cambio de tasa.
         -- Calculo el primer tramo de la actualizacion
         nimporte := pimporte;
         ndesde := nfecdesde;
         ndias :=(rtasa.fin - ndesde);
         nted := ROUND(POWER((1 +(rtasa.tasa / 100)),(1 / 30)) - 1, 6);
         nmonto := POWER(1 + nted, ndias) - 1;
         nimporte := nimporte +(nmonto * nimporte);

--     dbms_output.Put_line('Inicio   Primer   tramo '||to_char(nDesde,'dd/mm/rrrr')||' Tasa'||
--                          To_char(rTasa.TASA,'999D999')||' Monto '||
--                          To_char(nImporte-pImporte  ,'99999D999')||' por '|| to_char(nDias,'999')  ||' dias'
--                         );
     --- Calculo el resto de los tramos
         WHILE nfechasta > rtasa.fin LOOP
            ndesde := rtasa.fin + 1;

            CLOSE ctasas;

            OPEN ctasas(ndesde);

            FETCH ctasas
             INTO rtasa;

            IF ctasas%NOTFOUND THEN
               RETURN(0);
            ELSE
            --- Modificado por ticket  6926408  VLUCERO  FEB_2014 -------
                IF  rTasa.FIN > nFecHasta THEN
                    rTasa.FIN := nFecHasta;
                END IF;
            ------------------------ Fin modificacion -------------------
            END IF;

            ndias :=(rtasa.fin - ndesde)+1;
            nted := ROUND(POWER((1 +(rtasa.tasa / 100)),(1 / 30)) - 1, 6);
            nmonto := POWER(1 + nted, ndias) - 1;
            nimporte := nimporte +(nmonto * nimporte);
--        dbms_output.Put_line('Inicio siguiente tramo '||to_char(nDesde,'dd/mm/rrrr') ||' Tasa'||
--                          To_char(rTasa.TASA,'999D999')||' Monto '||
--                          To_char(nImporte-pImporte  ,'99999D999')||' por '|| to_char(nDias,'999')  ||' dias'
--                         );
         END LOOP;

         RETURN(nimporte - pimporte);
      END IF;

      RETURN(pimporte);
   END;

-- Final de  nueva funcion

   /* *************************************************************                   */
/*  CALCULO DE RECARGOS POR PAGO FUERA DE TERMINO                                  */
---   ESTA ES LA  VERSION ANTERIOR DE LA  RUTINA  ( utiliza INDICES  quincenales )
/* *************************************************************                   */
--   FUNCTION calculo_recargos(p_fecha_inicial DATE, p_fecha_final DATE, p_monto NUMBER)
--      RETURN NUMBER AS
--     v_coeficiente_inicial   NUMBER(17, 10);
--     v_coeficiente_final     NUMBER(17, 10);
--     v_fecha_inicio          DATE;
--     v_fecha_fin             DATE;
--     v_cr                    NUMBER(17, 10);
--     v_fecha                 DATE;
--     v_fecha_obtenida        DATE;
--     v_dia                   NUMBER(2);
--   BEGIN
--     IF p_fecha_inicial >= p_fecha_final THEN
--        RETURN 0;
--     END IF;

   --     /*Verifica si la fecha inicial y final estan en el mismo año y mes */
--     IF TO_CHAR(p_fecha_inicial, 'rrrrmm') = TO_CHAR(p_fecha_final, 'rrrrmm') THEN
--        IF TO_NUMBER(TO_CHAR(p_fecha_inicial, 'dd')) < 16 AND TO_NUMBER(TO_CHAR(p_fecha_final, 'dd')) < 16 THEN
--           v_cr := 0;
--           RETURN v_cr;
--        ELSE
--           IF TO_NUMBER(TO_CHAR(p_fecha_inicial, 'dd')) > 15
--              AND TO_NUMBER(TO_CHAR(p_fecha_final, 'dd')) > 15 THEN
--              v_cr := 0;
--              RETURN v_cr;
--           ELSE
--              IF     TO_NUMBER(TO_CHAR(p_fecha_inicial, 'dd')) < 16
--                 AND TO_NUMBER(TO_CHAR(p_fecha_final, 'dd')) > 15 THEN
--                 v_dia := 1;
--              END IF;
--           END IF;
--        END IF;
--     ELSE
--        /* Verifica si la fecha inicial y sysdate esten en el mismo año y mes */
--        IF TO_CHAR(p_fecha_inicial, 'rrrrmm') = TO_CHAR(SYSDATE, 'rrrrmm') THEN
--           v_dia := 1;
--        ELSE
--           v_dia := 16;
--        END IF;
--     END IF;

   --     BEGIN
--        SELECT cad_coef_actualizacion
--          INTO v_coeficiente_inicial
--          FROM COEFICIENTES_ACT_DEUDA
--         WHERE cad_fec_baja IS NULL
--           AND cad_fecha =
--                  (SELECT /*+ index(coeficientes_act_deuda CAD_PK) */
--                          MAX(cad_fecha)
--                     FROM COEFICIENTES_ACT_DEUDA
--                    WHERE cad_fecha =
--                                   TO_DATE(TO_CHAR(p_fecha_inicial, 'rrrrmm') || TO_CHAR(v_dia),
--                                           'rrrrmmdd')
--                      AND cad_fec_baja IS NULL);
--     EXCEPTION
--        WHEN NO_DATA_FOUND THEN
--           v_cr := 999;
--        WHEN OTHERS THEN
--           v_cr := 9999;
--     END;

   --     BEGIN
--        SELECT cad_coef_actualizacion
--          INTO v_coeficiente_final
--          FROM COEFICIENTES_ACT_DEUDA
--         WHERE cad_fec_baja IS NULL
--           AND cad_fecha = (SELECT MAX(cad_fecha)
--                              FROM COEFICIENTES_ACT_DEUDA
--                             WHERE cad_fecha <= p_fecha_final AND cad_fec_baja IS NULL);
--     EXCEPTION
--        WHEN NO_DATA_FOUND THEN
--           v_cr := 999;
--        WHEN OTHERS THEN
--           v_cr := 9999;
--     END;

   --     v_cr := p_monto *(v_coeficiente_final / v_coeficiente_inicial);
--     v_cr := v_cr - p_monto;
--     RETURN v_cr;
--   END calculo_recargos;

   /***************************************************************************************/
/*               FUNCION QUE INSERTA EN LA TABLA RECAUDACIONES                         */
/***************************************************************************************/
   FUNCTION incorpora_recaudacion(
      p_increcaudacionrec            PKG_RECAUDACIONES.vincrecaudacionrec,
      p_año_mes_contable             DATE,
      p_secuencia                    RECAUDACIONES.rec_secuencia%TYPE,
      p_obligacion          IN OUT   OBLIGACIONES.obl_id%TYPE,
      p_operacion                    RECAUDACIONES.rec_nro_operacion%TYPE)
      RETURN NUMBER AS
      v_rec_seq           NUMBER(10);
      cuenta              NUMBER(4);
      v_fecha_insertada   DATE;
      p_vco_codigo        RECAUDACIONES.rec_vco_codigo%TYPE;
      l_fecha_proceso     DATE;
      l_inmuebles         INMUEBLES.inm_id%TYPE;
      v_fecha_acred       DATE;
   BEGIN
      l_fecha_proceso := SYSDATE;

      SELECT rec_seq.NEXTVAL
        INTO v_rec_seq
        FROM DUAL;

      v_fecha_insertada := l_fecha_proceso;

      BEGIN
         SELECT inm_id
           INTO l_inmuebles
           FROM INMUEBLES
          WHERE inm_cuenta = p_increcaudacionrec.v_identificacion;
      EXCEPTION
         WHEN OTHERS THEN
            l_inmuebles := NULL;
      END;

      IF p_increcaudacionrec.p_estado NOT IN(1, 6) THEN
         v_fecha_insertada := NULL;

         IF p_increcaudacionrec.p_estado = 5 THEN
            p_obligacion := NULL;
         END IF;
      ELSIF p_increcaudacionrec.p_estado = 3 THEN
         p_obligacion := NULL;
         l_inmuebles := NULL;
      END IF;

      v_fecha_acred := NULL;

      BEGIN
         SELECT NVL(rem_fec_acreditacion, rem__fecha)
           INTO v_fecha_acred
           FROM REMESAS
          WHERE rem__numero = p_increcaudacionrec.v_remesa_osm;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            v_fecha_acred := NULL;
      END;

      INSERT INTO RECAUDACIONES
                  (rec_id, rec_vco_codigo, rec_inm_id, rec_obl_id,
                   rec_erc_codigo, rec_cuenta,
                   rec_fecha, rec_imp_cobrado,
                   rec_nro_factura, rec_anio_periodo, rec_mes_periodo,
                   rec_se, rec_usr_alta, rec_fec_alta,
                   rec_fec_aplicacion, rec_fec_proceso, rec_fec_contable,
                   rec_banco, rec_secuencia, rec_usr_baja, rec_fec_baja,
                   rec_usr_mod, rec_fec_mod, rec_nro_operacion, rec_rem_numero, rec_rem_ere_codigo,
                   rec_remesa, rec_fec_acredita, rec_clave_heredada, rec_lote)
           VALUES (v_rec_seq, p_increcaudacionrec.p_ente, l_inmuebles, p_obligacion,
                   p_increcaudacionrec.p_estado, p_increcaudacionrec.v_identificacion,
                   p_increcaudacionrec.v_fecha_cobro, p_increcaudacionrec.v_importe,
                   p_increcaudacionrec.v_factura, p_increcaudacionrec.v_anio, p_increcaudacionrec.v_periodo,
                   p_increcaudacionrec.p_se, p_increcaudacionrec.v_usuario, NVL(l_fecha_proceso, SYSDATE),
                   v_fecha_insertada, NVL(l_fecha_proceso, SYSDATE), p_año_mes_contable,
                   NVL(p_increcaudacionrec.p_banco, p_increcaudacionrec.p_ente), p_secuencia, NULL, NULL,
                   NULL, NULL, p_operacion, p_increcaudacionrec.v_remesa_osm, p_increcaudacionrec.p_ente,
                   p_increcaudacionrec.v_remesa, v_fecha_acred, NULL, NULL);

      RETURN v_rec_seq;
   END incorpora_recaudacion;

/***************************************************************************************/
/*               FUNCION QUE ACTUALIZAR EN LA TABLA RECAUDACIONES                      */
/***************************************************************************************/
   FUNCTION actualizar_recaudacion(p_actualizador PKG_RECAUDACIONES.vactuarecaudrec)
      RETURN VARCHAR2 AS
      l_fecha_proceso   DATE          := SYSDATE;
      l_retorno         VARCHAR2(300);
   BEGIN
      UPDATE RECAUDACIONES
         SET rec_nro_factura = p_actualizador.v_factura,
             rec_anio_periodo = p_actualizador.v_anio,
             rec_mes_periodo = p_actualizador.v_periodo,
             rec_cuenta = p_actualizador.v_identificacion,
             rec_erc_codigo = p_actualizador.p_erc_codigo,
             rec_fec_aplicacion = l_fecha_proceso,
             rec_fec_mod = l_fecha_proceso,
             rec_usr_mod = p_actualizador.v_usuario
       WHERE rec_id = p_actualizador.p_rec_id;

      l_retorno := 'Actualización terminada';
      RETURN l_retorno;
   EXCEPTION
      WHEN OTHERS THEN
         l_retorno := 'En actualización de recaudaciones. Error: ' || SQLERRM;
         ROLLBACK;
         RETURN l_retorno;
   END actualizar_recaudacion;

/***************************************************************************************/
/*               FUNCION QUE INSERTA EN LA TABLA ASIG PAGOS                            */
/***************************************************************************************/
   FUNCTION inserta_asig_pagos(p_asig_pagos ASIG_PAGOS%ROWTYPE)
      RETURN ASIG_PAGOS.apa_id%TYPE IS
      v_apa_seq   ASIG_PAGOS.apa_id%TYPE;
   BEGIN
      SELECT apa_seq.NEXTVAL
        INTO v_apa_seq
        FROM DUAL;

      INSERT INTO ASIG_PAGOS
                  (apa_id, apa_tipo_movimiento, apa_imp_haber,
                   apa_concepto, apa_fec_generacion,
                   apa_fec_contabilizacion, apa_fec_vencimiento,
                   apa_banco, apa_remesa, apa_fec_aplicacion,
                   apa_fec_proceso, apa_fec_cobro, apa_estado,
                   apa_se, apa_cuenta, apa_imp_iva,
                   apa_por_iva, apa_imp_ali, apa_por_ali,
                   apa_cct_obl_id, apa_cct_id_movimiento,
                   apa_ser_codigo, apa_rec_id, apa_grp_codigo,
                   apa_suc_codigo, apa_pef_anio, apa_pef_periodo,
                   apa_tre_codigo, apa_fec_alta, apa_usr_alta,
                   apa_fec_mod, apa_usr_mod, apa_fec_baja,
                   apa_usr_baja)
           VALUES (v_apa_seq, p_asig_pagos.apa_tipo_movimiento, p_asig_pagos.apa_imp_haber,
                   p_asig_pagos.apa_concepto, p_asig_pagos.apa_fec_generacion,
                   p_asig_pagos.apa_fec_contabilizacion, p_asig_pagos.apa_fec_vencimiento,
                   p_asig_pagos.apa_banco, p_asig_pagos.apa_remesa, p_asig_pagos.apa_fec_aplicacion,
                   p_asig_pagos.apa_fec_proceso, p_asig_pagos.apa_fec_cobro, p_asig_pagos.apa_estado,
                   NVL(p_asig_pagos.apa_se, 'N'), p_asig_pagos.apa_cuenta, p_asig_pagos.apa_imp_iva,
                   p_asig_pagos.apa_por_iva, p_asig_pagos.apa_imp_ali, p_asig_pagos.apa_por_ali,
                   p_asig_pagos.apa_cct_obl_id, p_asig_pagos.apa_cct_id_movimiento,
                   p_asig_pagos.apa_ser_codigo, p_asig_pagos.apa_rec_id, p_asig_pagos.apa_grp_codigo,
                   p_asig_pagos.apa_suc_codigo, p_asig_pagos.apa_pef_anio, p_asig_pagos.apa_pef_periodo,
                   p_asig_pagos.apa_tre_codigo, p_asig_pagos.apa_fec_alta, p_asig_pagos.apa_usr_alta,
                   p_asig_pagos.apa_fec_mod, p_asig_pagos.apa_usr_mod, p_asig_pagos.apa_fec_baja,
                   p_asig_pagos.apa_usr_baja);

      RETURN v_apa_seq;
   END inserta_asig_pagos;

/***************************************************************************************/
/*                  PROCEDIMIENTO INSERTA EN CUENTAS CORRIENTES                        */
/***************************************************************************************/
   PROCEDURE inserta_cuenta_corriente(p_cuenta_corriente CUENTAS_CORRIENTES%ROWTYPE) IS
   BEGIN
      INSERT INTO CUENTAS_CORRIENTES
                  (cct_obl_id, cct_id_movimiento,
                   cct_ser_codigo, cct_pef_anio,
                   cct_pef_periodo, cct_tipo_movimiento,
                   cct_imp_debe, cct_imp_haber,
                   cct_concepto, cct_fec_generacion,
                   cct_fec_cierre_contable, cct_usr_alta,
                   cct_fec_alta, cct_rec_id,
                   cct_nov_id, cct_fec_vencimiento,
                   cct_cnt_dias_recargo, cct_banco,
                   cct_remesa, cct_secuencia,
                   cct_fec_aplicacion, cct_fec_proceso,
                   cct_fec_cobro, cct_usr_baja,
                   cct_fec_baja, cct_usr_mod,
                   cct_fec_mod, cct_estado, cct_se,
                   cct_obl_id_cuota, cct_grp_codigo,
                   cct_cuenta, cct_suc_codigo,
                   cct_tre_codigo, cct_imp_iva,
                   cct_por_iva, cct_imp_ali,
                   cct_por_ali)
           VALUES (p_cuenta_corriente.cct_obl_id, NVL(p_cuenta_corriente.cct_id_movimiento, 99999),
                   p_cuenta_corriente.cct_ser_codigo, p_cuenta_corriente.cct_pef_anio,
                   p_cuenta_corriente.cct_pef_periodo, p_cuenta_corriente.cct_tipo_movimiento,
                   p_cuenta_corriente.cct_imp_debe, p_cuenta_corriente.cct_imp_haber,
                   p_cuenta_corriente.cct_concepto, p_cuenta_corriente.cct_fec_generacion,
                   p_cuenta_corriente.cct_fec_cierre_contable, p_cuenta_corriente.cct_usr_alta,
                   p_cuenta_corriente.cct_fec_alta, p_cuenta_corriente.cct_rec_id,
                   p_cuenta_corriente.cct_nov_id, p_cuenta_corriente.cct_fec_vencimiento,
                   p_cuenta_corriente.cct_cnt_dias_recargo, p_cuenta_corriente.cct_banco,
                   p_cuenta_corriente.cct_remesa, p_cuenta_corriente.cct_secuencia,
                   p_cuenta_corriente.cct_fec_aplicacion, p_cuenta_corriente.cct_fec_proceso,
                   p_cuenta_corriente.cct_fec_cobro, p_cuenta_corriente.cct_usr_baja,
                   p_cuenta_corriente.cct_fec_baja, p_cuenta_corriente.cct_usr_mod,
                   p_cuenta_corriente.cct_fec_mod, p_cuenta_corriente.cct_estado, p_cuenta_corriente.cct_se,
                   p_cuenta_corriente.cct_obl_id_cuota, p_cuenta_corriente.cct_grp_codigo,
                   p_cuenta_corriente.cct_cuenta, p_cuenta_corriente.cct_suc_codigo,
                   p_cuenta_corriente.cct_tre_codigo, p_cuenta_corriente.cct_imp_iva,
                   p_cuenta_corriente.cct_por_iva, p_cuenta_corriente.cct_imp_ali,
                   p_cuenta_corriente.cct_por_ali);
   END inserta_cuenta_corriente;

/* ********************************************************************************* */
/* V E R I F I C A   L O S   D A T O S   A N T E S   D E   I N C O R P O R A R L O S */
/* ********************************************************************************* */
   FUNCTION f_verifica_datos(p_incorporador PKG_RECAUDACIONES.vincorporacionrec)
      RETURN VARCHAR2 IS
      l_pru_codigo      RESUMEN_CONTROLES.xnc_ere_codigo%TYPE;
      l_pru_date        RESUMEN_CONTROLES.xnc_fecha%TYPE;
      l_retorno         VARCHAR2(300);
      l_tprocesados     NUMBER;
      l_timporte        NUMBER;
      l_dprocesados     NUMBER;
      l_dimporte        NUMBER;
      l_fecha_proceso   DATE                                    := SYSDATE;
      v_rem_seq         REMESAS.rem__numero%TYPE;
   BEGIN
      /* Verifica que existan los registros a incorporar */
      BEGIN
         SELECT xnc_ere_codigo, xnc_fecha
           INTO l_pru_codigo, l_pru_date
           FROM RESUMEN_CONTROLES
          WHERE xnc_fec_baja IS NULL
            AND ROWNUM = 1
            AND xnc_ere_codigo = p_incorporador.p_ente
            AND xnc_fecha = p_incorporador.p_fecha;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            l_retorno := 'No se encontro codigo o fecha en resumen controles. Error: ' || SQLERRM;
            ROLLBACK;
            RETURN l_retorno;
         WHEN OTHERS THEN
            l_retorno := 'Obteniendo codigo o fecha en resumen control. Error: ' || SQLERRM;
            ROLLBACK;
            RETURN l_retorno;
      END;

      IF l_pru_codigo = p_incorporador.p_ente AND l_pru_date = p_incorporador.p_fecha THEN
         /* cuenta los totales de facturas y suma los importes
            de la cabecera y de las oservaciones dependiendo del ente recaudador */
         IF p_incorporador.p_ente = 3 THEN   /* X-Net */
            BEGIN
               SELECT SUM(TO_NUMBER(SUBSTR(xnc_registro, 1, 8))),
                      SUM(TO_NUMBER(   SUBSTR(xnc_registro, 9, 3)
                                    || SUBSTR(xnc_registro, 13, 3)
                                    || SUBSTR(xnc_registro, 17, 3)
                                    || '.'
                                    || SUBSTR(xnc_registro, 21, 2)))
                 INTO l_tprocesados,
                      l_timporte
                 FROM RESUMEN_CONTROLES
                WHERE xnc_fec_baja IS NULL
                  AND xnc_ere_codigo = p_incorporador.p_ente
                  AND xnc_fecha = p_incorporador.p_fecha;
            EXCEPTION
               WHEN NO_DATA_FOUND THEN
                  l_retorno :=
                     'No se encontraron datos para realizar comparacion en resumen control. Error: '
                     || SQLERRM;
                  ROLLBACK;
                  RETURN l_retorno;
               WHEN OTHERS THEN
                  BEGIN
                     SELECT SUM(TO_NUMBER(SUBSTR(xnc_registro, 1, 8))),
                            SUM(TO_NUMBER(   SUBSTR(xnc_registro, 9, 3)
                                          || SUBSTR(xnc_registro, 13, 3)
                                          || SUBSTR(xnc_registro, 17, 3)
                                          || ','
                                          || SUBSTR(xnc_registro, 21, 2)))
                       INTO l_tprocesados,
                            l_timporte
                       FROM RESUMEN_CONTROLES
                      WHERE xnc_fec_baja IS NULL
                        AND xnc_ere_codigo = p_incorporador.p_ente
                        AND xnc_fecha = p_incorporador.p_fecha;
                  EXCEPTION
                     WHEN OTHERS THEN
                        l_retorno :=
                             'Obteniendo datos en resumen control para comparacion (XNet). Error: ' || SQLERRM;
                        ROLLBACK;
                        RETURN l_retorno;
                  END;
            END;
         ELSIF p_incorporador.p_ente = 7 THEN   /* VEA */
            BEGIN
               SELECT SUM(TO_NUMBER(   REPLACE(SUBSTR(xnc_registro, 1, 2), '  ', '0')
                                    || (LTRIM(RTRIM(SUBSTR(xnc_registro, 4, 3)))))),
                      SUM(TO_NUMBER(   SUBSTR(xnc_registro, 9, 3)
                                    || SUBSTR(xnc_registro, 13, 3)
                                    || SUBSTR(xnc_registro, 17, 3)
                                    || '.'
                                    || SUBSTR(xnc_registro, 21, 2)))
                 INTO l_tprocesados,
                      l_timporte
                 FROM RESUMEN_CONTROLES
                WHERE xnc_fec_baja IS NULL
                  AND xnc_ere_codigo = p_incorporador.p_ente
                  AND xnc_fecha = p_incorporador.p_fecha;
            EXCEPTION
               WHEN NO_DATA_FOUND THEN
                  l_retorno :=
                     'No se encontraron datos para realizar comparacion en resumen control. Error: '
                     || SQLERRM;
                  ROLLBACK;
                  RETURN l_retorno;
               WHEN OTHERS THEN
                  l_retorno :=
                              'Obteniendo datos en resumen control para comparacion (VEA). Error: ' || SQLERRM;
                  ROLLBACK;
                  RETURN l_retorno;
            END;
         ELSIF p_incorporador.p_ente = 701 THEN   /* Rapi Pago */
            BEGIN
               SELECT SUM(TO_NUMBER(SUBSTR(xnc_registro, 1, 6))),
                      SUM(TO_NUMBER(SUBSTR(xnc_registro, 7, 10) || '.' || SUBSTR(xnc_registro, 17, 2)))
                 INTO l_tprocesados,
                      l_timporte
                 FROM RESUMEN_CONTROLES
                WHERE xnc_fec_baja IS NULL
                  AND xnc_ere_codigo = p_incorporador.p_ente
                  AND xnc_fecha = p_incorporador.p_fecha;
            EXCEPTION
               WHEN NO_DATA_FOUND THEN
                  l_retorno :=
                     'No se encontraron datos para realizar comparacion en resumen control. Error: '
                     || SQLERRM;
                  ROLLBACK;
                  RETURN l_retorno;
               WHEN OTHERS THEN
                  l_retorno :=
                         'Obteniendo datos en resumen control para comparacion (RapiPago). Error: ' || SQLERRM;
                  ROLLBACK;
                  RETURN l_retorno;
            END;
         ELSIF p_incorporador.p_ente IN(79, 80, 116, 191, 297, 699, 702, 704, 706, 707, 708, 713, 811,909) THEN
            /* Regional/Cred.Fiscal/Credicoop/Banex/GIRE/Pago Fácil/Nevada/Banelco/RedLink/Cobro Express/Bco.Nacion/ULTRA */
            BEGIN
               SELECT SUM(TO_NUMBER(SUBSTR(xnc_registro, 1, 6))),
                      SUM(TO_NUMBER(SUBSTR(xnc_registro, 7, 10) || '.' || SUBSTR(xnc_registro, 17, 2)))
                 INTO l_tprocesados,
                      l_timporte
                 FROM RESUMEN_CONTROLES
                WHERE xnc_fec_baja IS NULL
                  AND xnc_ere_codigo = p_incorporador.p_ente
                  AND xnc_fecha = p_incorporador.p_fecha;
            EXCEPTION
               WHEN NO_DATA_FOUND THEN
                  l_retorno :=
                     'No se encontraron datos para realizar comparacion en resumen control. Error: '
                     || SQLERRM;
                  ROLLBACK;
                  RETURN l_retorno;
               WHEN OTHERS THEN
                  BEGIN
                     SELECT SUM(TO_NUMBER(SUBSTR(xnc_registro, 1, 6))),
                            SUM(TO_NUMBER(SUBSTR(xnc_registro, 7, 10) || ',' || SUBSTR(xnc_registro, 17, 2)))
                       INTO l_tprocesados,
                            l_timporte
                       FROM RESUMEN_CONTROLES
                      WHERE xnc_fec_baja IS NULL
                        AND xnc_ere_codigo = p_incorporador.p_ente
                        AND xnc_fecha = p_incorporador.p_fecha;
                  EXCEPTION
                     WHEN OTHERS THEN
                        l_retorno :=
                                    'Obteniendo datos en resumen control para comparacion. Error: ' || SQLERRM;
                        ROLLBACK;
                        RETURN l_retorno;
                  END;
            END;
         END IF;

         /* Cantidad de reg. e importe en Recaudaciones Externas */
         BEGIN
            IF p_incorporador.p_ente IN(191, 79) THEN
               SELECT COUNT(rep_registro),
                      SUM(TO_NUMBER(SUBSTR(rep_registro, 73, 6) || '.' || SUBSTR(rep_registro, 79, 2)))
                 INTO l_dprocesados,
                      l_dimporte
                 FROM RECAUDACIONES_EXTERNAS
                WHERE rep_ere_codigo = p_incorporador.p_ente AND rep_fecha = p_incorporador.p_fecha;
            ELSE
               SELECT COUNT(rep_registro),
                      SUM(TO_NUMBER(SUBSTR(rep_registro, 73, 6) || '.' || SUBSTR(rep_registro, 79, 2)))
                 INTO l_dprocesados,
                      l_dimporte
                 FROM RECAUDACIONES_EXTERNAS
                WHERE rep_registro NOT LIKE '%A'
                  AND rep_ere_codigo = p_incorporador.p_ente
                  AND rep_fecha = p_incorporador.p_fecha;
            END IF;
         EXCEPTION
            WHEN NO_DATA_FOUND THEN
               l_retorno :=
                     'No se encontraron datos para realizar comparacion en recaudaciones_externas. Error: '
                  || SQLERRM;
               ROLLBACK;
               RETURN l_retorno;
            WHEN OTHERS THEN
               BEGIN
                  IF p_incorporador.p_ente IN(191, 79) THEN
                     SELECT COUNT(rep_registro),
                            SUM(TO_NUMBER(SUBSTR(rep_registro, 73, 6) || ',' || SUBSTR(rep_registro, 79, 2)))
                       INTO l_dprocesados,
                            l_dimporte
                       FROM RECAUDACIONES_EXTERNAS
                      WHERE rep_ere_codigo = p_incorporador.p_ente AND rep_fecha = p_incorporador.p_fecha;
                  ELSE
                     SELECT COUNT(rep_registro),
                            SUM(TO_NUMBER(SUBSTR(rep_registro, 73, 6) || ',' || SUBSTR(rep_registro, 79, 2)))
                       INTO l_dprocesados,
                            l_dimporte
                       FROM RECAUDACIONES_EXTERNAS
                      WHERE rep_registro NOT LIKE '%A'
                        AND rep_ere_codigo = p_incorporador.p_ente
                        AND rep_fecha = p_incorporador.p_fecha;
                  END IF;
               EXCEPTION
                  WHEN OTHERS THEN
                     l_retorno :=
                             'Obteniendo datos en recaudaciones externas para comparacion. Error: ' || SQLERRM;
                     ROLLBACK;
                     RETURN l_retorno;
               END;
         END;

         /* Compara el importe en recaudaciones externas con el total de facturas
            y suma los importes de la cabecera */
         IF l_tprocesados <> l_dprocesados THEN
            l_retorno := 'No coincide la cantidad de registros entre cabecera y detalle';
            ROLLBACK;
            RETURN l_retorno;
         ELSIF l_timporte <> l_dimporte THEN
            l_retorno := 'No coincide el importe total entre cabecera y detalle';
            ROLLBACK;
            RETURN l_retorno;
         END IF;
      END IF;

      /* si coinciden los totales y los importes actualiza las tablas
         recaudaciones y remesas */
      SELECT rem_seq.NEXTVAL
        INTO v_rem_seq
        FROM DUAL;

      /* Insercción de la Remesa */
      BEGIN
         INSERT INTO REMESAS
                     (rem__ere_codigo, rem__numero, rem__fecha, rem__imp_total, rem__cnt_comprobantes,
                      rem__usr_alta, rem__fec_alta, rem__usr_baja, rem__fec_baja, rem__usr_mod, rem__fec_mod,
                      rem_fec_acreditacion)
              VALUES (p_incorporador.p_ente, v_rem_seq, p_incorporador.p_fecha, l_timporte, l_tprocesados,
                      p_incorporador.v_usuario, l_fecha_proceso, NULL, NULL, NULL, NULL,
                      NULL);
      EXCEPTION
         WHEN OTHERS THEN
            l_retorno := 'En la inserción de remesas. Error: ' || SQLERRM;
            ROLLBACK;
            RETURN l_retorno;
      END;

      /* Actualización de Resumen_Controles */
      BEGIN
         UPDATE RESUMEN_CONTROLES
            SET xnc_fec_mod = l_fecha_proceso,
                xnc_usr_mod = p_incorporador.v_usuario
          WHERE xnc_fecha = TRUNC(p_incorporador.p_fecha)
            AND xnc_ere_codigo = p_incorporador.p_ente
            AND xnc_fec_baja IS NULL;
      EXCEPTION
         WHEN OTHERS THEN
            l_retorno := 'En la actualización de Resumen_Control. Error: ' || SQLERRM;
            ROLLBACK;
            RETURN l_retorno;
      END;

      RETURN('S');
   END f_verifica_datos;

/* ************************************************************* */
/* calcula el ultimo digito de la descripcion del archivo plano  */
/* ************************************************************* */
   FUNCTION CALCULA_DV_BARRA(l_codigo_barra_1 VARCHAR2)
      RETURN VARCHAR2 AS
      l_i              INTEGER;
      l_t              INTEGER;
      l_digito1        NUMERIC(3);
      l_digito2        NUMERIC(3);
      l_acumula        NUMERIC(8);
      l_resultado      VARCHAR2(20);
      l_longitud       INTEGER;
      l_codigo_barra   VARCHAR2(60);
      l_verificador    VARCHAR2(2);
   BEGIN
      l_t := 23;
      l_i := 60;
      l_codigo_barra := UPPER(l_codigo_barra_1);
      l_acumula := 0;

      WHILE(l_i > 0) LOOP
         IF l_t = 11 THEN
            l_t := 13;
         ELSE
            IF l_t = 13 THEN
               l_t := 17;
            ELSE
               IF l_t = 17 THEN
                  l_t := 19;
               ELSE
                  IF l_t = 19 THEN
                     l_t := 23;
                  ELSE
                     l_t := 11;
                  END IF;
               END IF;
            END IF;
         END IF;

         IF SUBSTR(l_codigo_barra, l_i, 1) IN('0', '1', '2', '3', '4', '5', '6', '7', '8', '9') THEN
            l_digito1 := ASCII(SUBSTR(l_codigo_barra, l_i, 1)) - 48;
         ELSE
            IF SUBSTR(l_codigo_barra, l_i, 1) BETWEEN 'A' AND 'Z' THEN
               l_digito1 := ASCII(SUBSTR(l_codigo_barra, l_i, 1));
            ELSE
               l_digito1 := 0;
            END IF;
         END IF;

         l_acumula := l_acumula + l_digito1 * l_t;
         l_i := l_i - 1;
      END LOOP;

      l_resultado := LTRIM(TO_CHAR(l_acumula));
      l_longitud := LENGTH(l_resultado);

      IF l_longitud < 4 THEN
         l_resultado := LPAD(l_resultado, 4, 0);
      END IF;

      l_digito1 := SUBSTR(SUBSTR(l_resultado, l_longitud - 3, 2), 1, 2);
      l_digito2 := SUBSTR(l_resultado, l_longitud - 1, 2);
      l_verificador := LTRIM(TO_CHAR(ABS(TO_NUMBER(l_digito1) - TO_NUMBER(l_digito2))));

      IF LENGTH(l_verificador) < 2 THEN
         l_verificador := '0' || l_verificador;
      END IF;

      RETURN l_verificador;
   END CALCULA_DV_BARRA;

/* *************************************************************************** */
/*  R E C U P E R A    O B L I G A C I O N                           */
/* *************************************************************************** */
   FUNCTION f_recupera_obl(
      p_erc_codigo       RECAUDACIONES.rec_erc_codigo%TYPE,
      p_ext_ppl          PLANES_PAGO.ppl_id%TYPE,
      p_cod_factura      VARCHAR2,
      p_busqueda_alter   VARCHAR2,
      p_inserta_recaud   PKG_RECAUDACIONES.vincrecaudacionrec,
      p_importe          OBLIGACIONES.obl_imp_original%TYPE)
      RETURN OBLIGACIONES%ROWTYPE IS
      /* Cursor utilizado para determinar la obligaciones original */
      CURSOR c_obligaciones(
         p_obl_nro_factura   OBLIGACIONES.obl_nro_factura%TYPE,
         p_obl_pef_anio      OBLIGACIONES.obl_pef_anio%TYPE,
         p_obl_pef_periodo   OBLIGACIONES.obl_pef_periodo%TYPE,
         p_obl_saldo_desde   OBLIGACIONES.obl_saldo%TYPE,
         p_obl_saldo_hasta   OBLIGACIONES.obl_saldo%TYPE,
         p_obl_cuenta        OBLIGACIONES.obl_cuenta%TYPE) IS
         SELECT /*+ INDEX (OBLIGACIONES OBL_NRO_FACTURA_IDX) */
                *
           FROM OBLIGACIONES
          WHERE obl_nro_factura = p_obl_nro_factura
            AND obl_pef_anio = p_obl_pef_anio
            AND obl_pef_periodo = p_obl_pef_periodo
            AND obl_saldo BETWEEN p_obl_saldo_desde AND p_obl_saldo_hasta
            AND obl_cuenta = p_obl_cuenta
            AND obl_estado != 65
            AND (obl_tpc_codigo > 80 OR obl_tpc_codigo IN(8, 9));

      /* Cursor utilizado para determinar la primer cuota del plan de pago */
      CURSOR c_cuota_plan_pago(
         p_obl_nro_factura   OBLIGACIONES.obl_nro_factura%TYPE,
         p_obl_saldo_desde   OBLIGACIONES.obl_saldo%TYPE,
         p_obl_saldo_hasta   OBLIGACIONES.obl_saldo%TYPE,
         p_obl_cuenta        OBLIGACIONES.obl_cuenta%TYPE) IS
         SELECT   /*+ INDEX (OBLIGACIONES OBL_NRO_FACTURA_IDX) */
                  *
             FROM OBLIGACIONES
            WHERE obl_nro_factura = p_obl_nro_factura
              AND obl_saldo BETWEEN p_obl_saldo_desde AND p_obl_saldo_hasta
              AND obl_cuenta = p_obl_cuenta
              AND obl_estado = 15
              AND (obl_tpc_codigo > 80 OR obl_tpc_codigo IN(8, 9))
         ORDER BY obl_cuota_plan;

      r_obligacion   OBLIGACIONES%ROWTYPE;
   BEGIN
      IF p_erc_codigo = 6 THEN
         IF p_ext_ppl = 0 AND TO_NUMBER(p_cod_factura) = 80 THEN
            IF NVL(p_busqueda_alter, 0) = 0 THEN
               OPEN c_obligaciones(p_inserta_recaud.v_factura,
                                   p_inserta_recaud.v_anio,
                                   p_inserta_recaud.v_periodo,
                                   p_importe - 1,
                                   p_importe + 1,
                                   p_inserta_recaud.v_identificacion);

               FETCH c_obligaciones
                INTO r_obligacion;

               CLOSE c_obligaciones;
            ELSE
               SELECT /*+ index(obligaciones OBL_CUENTA) */
                      *
                 INTO r_obligacion
                 FROM OBLIGACIONES
                WHERE TO_NUMBER(SUBSTR(LPAD(TO_CHAR(obl_nro_factura), 16, '0'), 7, 10)) =
                                                                                     p_inserta_recaud.v_factura
                  AND obl_pef_anio = p_inserta_recaud.v_anio
                  AND obl_pef_periodo = p_inserta_recaud.v_periodo
                  AND obl_saldo BETWEEN(p_importe - 1) AND(p_importe + 1)
                  AND obl_cuenta = p_inserta_recaud.v_identificacion
                  AND obl_estado != 65
                  AND (obl_tpc_codigo > 80 OR obl_tpc_codigo IN(8, 9));
            END IF;
         ELSE
            OPEN c_cuota_plan_pago(p_inserta_recaud.v_factura,
                                   p_importe - 1,
                                   p_importe + 1,
                                   p_inserta_recaud.v_identificacion);

            FETCH c_cuota_plan_pago
             INTO r_obligacion;

            CLOSE c_cuota_plan_pago;
         END IF;
      ELSE
         IF p_ext_ppl = 0 OR TO_NUMBER(p_cod_factura) != 80 THEN
            IF NVL(p_busqueda_alter, 0) = 0 THEN
               OPEN c_obligaciones(p_inserta_recaud.v_factura,
                                   p_inserta_recaud.v_anio,
                                   p_inserta_recaud.v_periodo,
                                   p_importe - 1,
                                   p_importe + 1,
                                   p_inserta_recaud.v_identificacion);

               FETCH c_obligaciones
                INTO r_obligacion;

               CLOSE c_obligaciones;
            ELSE
               BEGIN
                  SELECT /*+ index(obligaciones OBL_CUENTA) */
                         *
                    INTO r_obligacion
                    FROM OBLIGACIONES
                   WHERE TO_NUMBER(SUBSTR(LPAD(TO_CHAR(obl_nro_factura), 16, '0'), 7, 10)) =
                                                                                     p_inserta_recaud.v_factura
                     AND obl_pef_anio = p_inserta_recaud.v_anio
                     AND obl_pef_periodo = p_inserta_recaud.v_periodo
                     AND obl_saldo = p_inserta_recaud.v_importe
                     AND obl_cuenta = p_inserta_recaud.v_identificacion
                     AND obl_estado != 65
                     AND (obl_tpc_codigo > 80 OR obl_tpc_codigo IN(8, 9));
               EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                     ROLLBACK;
                     --INSERT INTO ver VALUES (p_inserta_recaud.v_factura);
                     --COMMIT;
                     RETURN NULL;
               END;
            END IF;
         ELSE
            OPEN c_cuota_plan_pago(p_inserta_recaud.v_factura,
                                   p_importe,
                                   p_importe,
                                   p_inserta_recaud.v_identificacion);

            FETCH c_cuota_plan_pago
             INTO r_obligacion;

            CLOSE c_cuota_plan_pago;
         END IF;
      END IF;

      RETURN r_obligacion;
   END f_recupera_obl;

/***************************************************************************************/
/*  A P L I C A C I O N   D E   C R E D I T O S                                        */
/***************************************************************************************/
   FUNCTION f_aplica_creditos(
      p_obl_id           OBLIGACIONES.obl_id%TYPE,
      p_usuario          ASIG_PAGOS.apa_usr_alta%TYPE,
      p_cct_rec_id       CUENTAS_CORRIENTES.cct_rec_id%TYPE,
      p_cct_banco        CUENTAS_CORRIENTES.cct_banco%TYPE,
      p_cct_remesa       CUENTAS_CORRIENTES.cct_remesa%TYPE,
      p_cct_secuencia    CUENTAS_CORRIENTES.cct_secuencia%TYPE,
      p_fec_cobro        DATE,
      p_fec_aplicacion   DATE)
      RETURN VARCHAR2 IS
      CURSOR c_cct_creditos IS
         SELECT *
           FROM CUENTAS_CORRIENTES
          WHERE cct_obl_id = p_obl_id
            AND cct_estado = 15
            AND cct_imp_haber > 0
            AND cct_ser_codigo <> 800088
            AND cct_rec_id IS NULL;

      CURSOR c_cct_debitos IS
         SELECT   *
             FROM CUENTAS_CORRIENTES
            WHERE cct_obl_id = p_obl_id AND cct_estado = 15 AND cct_imp_debe > 0
         ORDER BY cct_imp_debe;

      l_retorno          VARCHAR2(300)                          := NULL;
      l_fecha_proceso    DATE                                   := p_fec_aplicacion;
      l_imp_aplicar      CUENTAS_CORRIENTES.cct_imp_debe%TYPE   := 0;
      l_imp_distribuir   CUENTAS_CORRIENTES.cct_imp_debe%TYPE   := 0;
      l_porcentaje       NUMBER;
      v_ya_pagado        NUMBER(15, 2);
      l_cnt_creditos     NUMBER                                 := 0;
      r_asig_pagos       ASIG_PAGOS%ROWTYPE;
      l_estado           CUENTAS_CORRIENTES.cct_estado%TYPE     := 30;
   BEGIN
      FOR r_credito IN c_cct_creditos LOOP
         r_asig_pagos.apa_concepto := 'CREDITOS APLICADOS';
         r_asig_pagos.apa_banco := p_cct_banco;
         r_asig_pagos.apa_remesa := p_cct_remesa;
         r_asig_pagos.apa_fec_aplicacion := NVL(l_fecha_proceso, SYSDATE);
         r_asig_pagos.apa_fec_proceso := NVL(l_fecha_proceso, SYSDATE);
         r_asig_pagos.apa_fec_cobro := p_fec_cobro;
         r_asig_pagos.apa_cuenta := r_credito.cct_cuenta;
         r_asig_pagos.apa_rec_id := p_cct_rec_id;
         r_asig_pagos.apa_grp_codigo := r_credito.cct_grp_codigo;
         r_asig_pagos.apa_suc_codigo := r_credito.cct_suc_codigo;
         r_asig_pagos.apa_tre_codigo := r_credito.cct_tre_codigo;
         r_asig_pagos.apa_fec_alta := l_fecha_proceso;
         r_asig_pagos.apa_usr_alta := p_usuario;
         r_asig_pagos.apa_fec_mod := NULL;
         r_asig_pagos.apa_usr_mod := NULL;
         r_asig_pagos.apa_fec_baja := NULL;
         r_asig_pagos.apa_usr_baja := NULL;
         r_asig_pagos.apa_se := r_credito.cct_se;
         r_asig_pagos.apa_estado := 31;
         l_imp_aplicar := r_credito.cct_imp_haber;
         -- agregado por SROMERO - pedido 7904615 - 24/09/2015
         -- no limpiaba la cantidad de créditos para cada ciclo
         l_cnt_creditos := 0;
         -- FIN agregado por SROMERO - pedido 7904615 - 24/09/2015
         /* Recorre los Débitos buscando los del mismo código de servicio que el crédito y contando los distintos */
         FOR r_debito IN c_cct_debitos LOOP
            IF r_debito.cct_ser_codigo = r_credito.cct_ser_codigo THEN
               BEGIN
                  SELECT SUM(apa_imp_haber)
                    INTO v_ya_pagado
                    FROM ASIG_PAGOS
                   WHERE apa_cct_obl_id = r_debito.cct_obl_id
                     AND apa_cct_id_movimiento = r_debito.cct_id_movimiento;
               END;

               r_asig_pagos.apa_tipo_movimiento := r_debito.cct_tipo_movimiento;
               r_asig_pagos.apa_fec_generacion := r_debito.cct_fec_generacion;
               r_asig_pagos.apa_fec_contabilizacion := r_debito.cct_fec_cierre_contable;
               r_asig_pagos.apa_fec_vencimiento := r_debito.cct_fec_vencimiento;
               r_asig_pagos.apa_cct_obl_id := r_debito.cct_obl_id;
               r_asig_pagos.apa_cct_id_movimiento := r_debito.cct_id_movimiento;
               r_asig_pagos.apa_ser_codigo := r_debito.cct_ser_codigo;
               r_asig_pagos.apa_pef_anio := r_debito.cct_pef_anio;
               r_asig_pagos.apa_pef_periodo := r_debito.cct_pef_periodo;
               r_asig_pagos.apa_por_iva := r_debito.cct_por_iva;
               r_asig_pagos.apa_por_ali := r_debito.cct_por_ali;

               IF l_imp_aplicar + NVL(v_ya_pagado, 0) >= r_debito.cct_imp_debe THEN
                  l_porcentaje := ((r_debito.cct_imp_debe - NVL(v_ya_pagado, 0)) * 100)
                                  / r_debito.cct_imp_debe;
                  r_asig_pagos.apa_imp_iva := ROUND((r_debito.cct_imp_iva * l_porcentaje) / 100, 2);
                  r_asig_pagos.apa_imp_ali := ROUND((r_debito.cct_imp_ali * l_porcentaje) / 100, 2);
                  r_asig_pagos.apa_imp_haber := r_debito.cct_imp_debe - NVL(v_ya_pagado, 0);
                  l_imp_aplicar := l_imp_aplicar -(r_debito.cct_imp_debe - NVL(v_ya_pagado, 0));

                  --IF l_imp_aplicar+nvl(v_ya_pagado,0) >= r_debito.cct_imp_debe THEN
                  --  /* Si se candela totalmente el débito se coloca en estado 30 la cta. cte. */
                  --  r_asig_pagos.apa_imp_haber := r_debito.cct_imp_debe;
                  --  r_asig_pagos.apa_imp_iva := r_debito.cct_imp_iva;
                  --  r_asig_pagos.apa_imp_ali := r_debito.cct_imp_ali;
                  --  l_imp_aplicar := l_imp_aplicar - (r_debito.cct_imp_debe - nvl(v_ya_pagado,0));
                  UPDATE CUENTAS_CORRIENTES
                     SET cct_estado = 30,
                         cct_fec_cobro = p_fec_cobro,
                         cct_fec_aplicacion = l_fecha_proceso,
                         cct_fec_proceso = NVL(l_fecha_proceso, SYSDATE),
                         cct_fec_mod = l_fecha_proceso,
                         cct_usr_mod = p_usuario,
                         cct_rec_id = p_cct_rec_id,
                         cct_banco = p_cct_banco,
                         cct_remesa = p_cct_remesa,
                         cct_secuencia = p_cct_secuencia
                   WHERE cct_obl_id = p_obl_id AND cct_id_movimiento = r_debito.cct_id_movimiento;
               ELSE
                  /* Si se cancela PARCIALMENTE el débito se calcula el iva de la cta. cte.*/
                  l_porcentaje := (l_imp_aplicar * 100) / r_debito.cct_imp_debe;
                  r_asig_pagos.apa_imp_iva := ROUND((r_debito.cct_imp_iva * l_porcentaje) / 100, 2);
                  r_asig_pagos.apa_imp_ali := ROUND((r_debito.cct_imp_ali * l_porcentaje) / 100, 2);
                  r_asig_pagos.apa_imp_haber := l_imp_aplicar;
                  l_imp_aplicar := 0;
               END IF;

               r_asig_pagos.apa_id := inserta_asig_pagos(r_asig_pagos);
            ELSE
               l_cnt_creditos := l_cnt_creditos + 1;
            END IF;

            IF l_imp_aplicar <= 0 THEN
               /* Si el importe se aplico totalmente no sigue distribuyendolo a los demás débitos. */
               EXIT;
            END IF;
         END LOOP;

         UPDATE CUENTAS_CORRIENTES
            SET cct_estado = 30,
                cct_fec_cobro = p_fec_cobro,
                cct_fec_aplicacion = NVL(l_fecha_proceso, SYSDATE),
                cct_fec_proceso = NVL(l_fecha_proceso, SYSDATE),
                cct_fec_mod = NVL(l_fecha_proceso, SYSDATE),
                cct_usr_mod = p_usuario,
                cct_rec_id = p_cct_rec_id,
                cct_banco = p_cct_banco,
                cct_remesa = p_cct_remesa,
                cct_secuencia = p_cct_secuencia
          WHERE cct_obl_id = p_obl_id AND cct_id_movimiento = r_credito.cct_id_movimiento;

         IF l_imp_aplicar > 0 THEN
            /* Si el importe se aplico totalmente no sigue distribuyendolo a los demás débitos. */
            /* Recorre los Débitos buscando los de DISTINTO código de servicio que el crédito */
            FOR r_debito IN c_cct_debitos LOOP
               IF r_debito.cct_ser_codigo <> r_credito.cct_ser_codigo THEN
                  /* Si el crédito tiene importe para aplicar se distribuye equitativamente entre los demás débitos.
                     Como pueden existir débitos menores que l_imp_distribuir se los busca y se va recalculando
                     por eso se realiza en levantan los débitos en orden de importes */
                  l_imp_distribuir := ROUND(l_imp_aplicar / l_cnt_creditos, 2);
                  --DBMS_OUTPUT.PUT_LINE('Aplicar: '||l_imp_aplicar||' Cred: '||l_cnt_creditos||' Distribuir: '||l_imp_distribuir);
                  r_asig_pagos.apa_tipo_movimiento := r_debito.cct_tipo_movimiento;
                  r_asig_pagos.apa_fec_generacion := r_debito.cct_fec_generacion;
                  r_asig_pagos.apa_fec_contabilizacion := r_debito.cct_fec_cierre_contable;
                  r_asig_pagos.apa_fec_vencimiento := r_debito.cct_fec_vencimiento;
                  r_asig_pagos.apa_cct_obl_id := r_debito.cct_obl_id;
                  r_asig_pagos.apa_cct_id_movimiento := r_debito.cct_id_movimiento;
                  r_asig_pagos.apa_ser_codigo := r_debito.cct_ser_codigo;
                  r_asig_pagos.apa_pef_anio := r_debito.cct_pef_anio;
                  r_asig_pagos.apa_pef_periodo := r_debito.cct_pef_periodo;
                  r_asig_pagos.apa_por_iva := r_debito.cct_por_iva;
                  r_asig_pagos.apa_por_ali := r_debito.cct_por_ali;

                  BEGIN
                     SELECT SUM(apa_imp_haber)
                       INTO v_ya_pagado
                       FROM ASIG_PAGOS
                      WHERE apa_cct_obl_id = r_debito.cct_obl_id
                        AND apa_cct_id_movimiento = r_debito.cct_id_movimiento;
                  END;

                  --DBMS_OUTPUT.PUT_LINE('Débito: '||r_debito.cct_imp_debe||' Ya pag.: '||to_char(nvl(v_ya_pagado,0)));
                  IF l_imp_distribuir + NVL(v_ya_pagado, 0) >= r_debito.cct_imp_debe THEN
                     l_imp_distribuir := r_debito.cct_imp_debe - NVL(v_ya_pagado, 0);
                     --r_asig_pagos.apa_imp_haber := r_debito.cct_imp_debe;
                     --r_asig_pagos.apa_imp_iva   := r_debito.cct_imp_iva;
                     --r_asig_pagos.apa_imp_ali   := r_debito.cct_imp_ali;
                     l_porcentaje := (l_imp_distribuir * 100) / r_debito.cct_imp_debe;
                     r_asig_pagos.apa_imp_iva := ROUND((r_debito.cct_imp_iva * l_porcentaje) / 100, 2);
                     r_asig_pagos.apa_imp_ali := ROUND((r_debito.cct_imp_ali * l_porcentaje) / 100, 2);
                     r_asig_pagos.apa_imp_haber := l_imp_distribuir;
                     l_imp_aplicar := l_imp_aplicar - l_imp_distribuir;

                     --DBMS_OUTPUT.PUT_LINE('Mayor o Igual: '||l_imp_distribuir||' '||to_char(r_debito.cct_imp_debe-nvl(v_ya_pagado,0)));
                     UPDATE CUENTAS_CORRIENTES
                        SET cct_estado = 30,
                            cct_fec_cobro = p_fec_cobro,
                            cct_fec_aplicacion = NVL(l_fecha_proceso, SYSDATE),
                            cct_fec_proceso = NVL(l_fecha_proceso, SYSDATE),
                            cct_fec_mod = NVL(l_fecha_proceso, SYSDATE),
                            cct_usr_mod = p_usuario,
                            cct_rec_id = p_cct_rec_id,
                            cct_banco = p_cct_banco,
                            cct_remesa = p_cct_remesa,
                            cct_secuencia = p_cct_secuencia
                      WHERE cct_obl_id = p_obl_id AND cct_id_movimiento = r_debito.cct_id_movimiento;
                  ELSE
                     --DBMS_OUTPUT.PUT_LINE('Menor: '||l_imp_distribuir||' '||TO_CHAR(r_debito.cct_imp_debe-nvl(v_ya_pagado,0)));

                     /* Si se cancela PARCIALMENTE el débito se calcula el iva de la cta. cte.*/
                     l_porcentaje := (l_imp_distribuir * 100) / r_debito.cct_imp_debe;
                     r_asig_pagos.apa_imp_iva := ROUND((r_debito.cct_imp_iva * l_porcentaje) / 100, 2);
                     r_asig_pagos.apa_imp_ali := ROUND((r_debito.cct_imp_ali * l_porcentaje) / 100, 2);
                     r_asig_pagos.apa_imp_haber := l_imp_distribuir;
                     l_imp_aplicar := l_imp_aplicar - l_imp_distribuir;
                  END IF;

                  --DBMS_OUTPUT.PUT_LINE('Queda: '||l_imp_aplicar);
                  l_cnt_creditos := l_cnt_creditos - 1;
                  r_asig_pagos.apa_id := inserta_asig_pagos(r_asig_pagos);
               END IF;
            END LOOP;
         END IF;
      END LOOP;

      RETURN l_retorno;
   END f_aplica_creditos;

   PROCEDURE GENERA_NC_QUITA(
      p_plan               PLANES_PAGO.ppl_id%TYPE,
      p_cli_id             OBLIGACIONES.obl_cli_id%TYPE,
      p_inm_id             OBLIGACIONES.obl_inm_id%TYPE,
      p_cuenta             OBLIGACIONES.obl_cuenta%TYPE,
      p_grupo              OBLIGACIONES.obl_grp_codigo%TYPE,
      p_tipo_resp          OBLIGACIONES.obl_tre_codigo%TYPE,
      p_usuario            OBLIGACIONES.obl_usr_alta%TYPE,
      p_fec_prescripcion   DATE) IS
      CURSOR cplanes IS
         SELECT *
           FROM PLANES_PAGO
          WHERE ppl_id = p_plan;

      rplan               cplanes%ROWTYPE;

      CURSOR quita IS
         SELECT   /*+ index (obligaciones obl_ppl_fk_i) */
                  cct_obl_id, cct_id_movimiento
             FROM OBLIGACIONES, CUENTAS_CORRIENTES
            WHERE obl_ppl_id = p_plan
              AND obl_fec_vencimiento < NVL(p_fec_prescripcion, obl_fec_vencimiento + 1)
              AND cct_obl_id = obl_id
              AND cct_estado = 15
              AND cct_imp_debe > 0
              AND cct_ser_codigo <> 640106
              AND (   cct_ser_codigo <> 640108
                   OR (    cct_ser_codigo = 640108
                       AND NOT EXISTS(SELECT 1
                                        FROM NOVEDADES_FACTURABLES
                                       WHERE nov_id = cct_nov_id AND nov_tipo_novedad = 40)))
         ORDER BY obl_pef_anio, obl_pef_periodo;

      CURSOR obl_quita IS
         SELECT          /*+ index (obligaciones obl_ppl_fk_i) */
                DISTINCT cct_obl_id
                    FROM OBLIGACIONES, CUENTAS_CORRIENTES
                   WHERE obl_ppl_id = p_plan
                     AND cct_obl_id = obl_id
                     AND obl_fec_vencimiento < NVL(p_fec_prescripcion, obl_fec_vencimiento + 1)
                     AND cct_estado = 65
                     AND cct_imp_debe > 0
                     AND cct_ser_codigo <> 640106
                     AND (   cct_ser_codigo <> 640108
                          OR (    cct_ser_codigo = 640108
                              AND NOT EXISTS(SELECT 1
                                               FROM NOVEDADES_FACTURABLES
                                              WHERE nov_id = cct_nov_id AND nov_tipo_novedad = 40)));

      v_total_nc          OBLIGACIONES.obl_saldo%TYPE;
      v_neto_nc           OBLIGACIONES.obl_saldo%TYPE;
      v_neto              OBLIGACIONES.obl_saldo%TYPE;
      v_asig_pagos        OBLIGACIONES.obl_saldo%TYPE;
      v_obl_id            OBLIGACIONES.obl_id%TYPE;
      v_id_mov            CUENTAS_CORRIENTES.cct_id_movimiento%TYPE;
      v_numero_nc         OBLIGACIONES.obl_nro_factura%TYPE;
      l_retorno           VARCHAR2(300);
      v_acum_asig_pagos   NUMBER(12, 2);
      v_acum_neto         NUMBER(12, 2);
      nPto                NUMBER  := 0;   -- Punto de Ventas

   BEGIN
      /* Insertar cabecera de NC */
      BEGIN
         SELECT /*+ index (obligaciones obl_ppl_fk_i) */
                SUM(cct_imp_debe), SUM(cct_imp_debe - cct_imp_ali - cct_imp_iva)
           INTO v_total_nc, v_neto_nc
           FROM OBLIGACIONES, CUENTAS_CORRIENTES
          WHERE obl_ppl_id = p_plan
            AND cct_obl_id = obl_id
            AND obl_fec_vencimiento < NVL(p_fec_prescripcion, obl_fec_vencimiento + 1)
            AND cct_estado = 15
            AND cct_imp_debe > 0
            AND cct_ser_codigo <> 640106
            AND (   cct_ser_codigo <> 640108
                 OR (cct_ser_codigo = 640108
                     AND NOT EXISTS(SELECT 1
                                      FROM NOVEDADES_FACTURABLES
                                     WHERE nov_id = cct_nov_id AND nov_tipo_novedad = 40)));
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            v_total_nc := 0;
      END;
      /* Genera el OBL_ID */
      BEGIN
         SELECT obl_seq.NEXTVAL
           INTO v_obl_id
           FROM DUAL;
      END;
      /* Busca número de NC */
      BEGIN
         ------------- Modificado el 08/04/2014    VLUCERO  TCK_7011384   ----------------
         -- Se ajusta para que la NC use numeracion de las  oficinas comerciales ---------
         BEGIN
            SELECT pge_valor  INTO  nPto
            FROM  Seguridad.par_generales
            WHERE pge_codigo = 'PTOFICINAS';
         EXCEPTION  WHEN NO_DATA_FOUND THEN
            PKG_SERVICIOS_FIJOS_REF.p_error(SQLERRM, SQLCODE, 'Busca Pto de Venta', 'pkg_recaudaciones.genera_nc_quita', 'INMUEBLES', TO_CHAR(p_inm_id));
         WHEN OTHERS  THEN
            PKG_SERVICIOS_FIJOS_REF.p_error(SQLERRM, SQLCODE, 'Busca Pto de Venta', 'pkg_recaudaciones.genera_nc_quita', 'INMUEBLES', TO_CHAR(p_inm_id));
         END;
         ----------- Fin de modificacion -------------------------------------------------
      -- sromero - pedido 7005666 - 03/04/14 - se cambia la llamada por el nuevo formato
--         v_numero_nc := PKG_NUMEROS_COMPROBANTES.principal(61, 1, p_usuario);
         v_numero_nc := PKG_NUMEROS_COMPROBANTES.NumeraFactura(89,nPto, p_tipo_resp,p_usuario);
         IF NOT PKG_NUMEROS_COMPROBANTES.Actualiza_Rel(89,nPto,p_tipo_resp,v_obl_id,p_inm_id,p_Usuario,
                       1,v_numero_nc ) THEN
            PKG_SERVICIOS_FIJOS_REF.p_error(SQLERRM, SQLCODE, 'genera_nc_quita', 'pkg_recaudaciones.genera_nc_quita', 'INMUEBLES', TO_CHAR(p_inm_id));
         END IF;
      -- FIN sromero - pedido 7005666 - 03/04/14 - se cambia la llamada por el nuevo formato
      END;



      OPEN cplanes;

      FETCH cplanes
       INTO rplan;

      IF cplanes%NOTFOUND THEN
         rplan.ppl_mao_codigo := 151;   --150 ;
      END IF;

      CLOSE cplanes;

      IF rplan.ppl_mao_codigo IS NULL THEN
         rplan.ppl_mao_codigo := 151;
      END IF;

      /* Genera cabecera */
      IF v_total_nc > 0 THEN
         INSERT INTO OBLIGACIONES
                     (obl_id, obl_cli_id, obl_pef_anio,
                      obl_pef_periodo, obl_mao_codigo, obl_tpc_codigo, obl_con_inm_id, obl_cuenta,
                      obl_fec_generacion, obl_imp_original, obl_estado, obl_saldo, obl_fec_vencimiento,
                      obl_nro_factura, obl_imp_neto, obl_usr_alta, obl_fec_alta, obl_ppl_id, obl_inm_id,
                      obl_fec_proceso, obl_fec_aplicacion, obl_se, obl_fec_contable, obl_suc_codigo,
                      obl_grp_codigo, obl_tre_codigo)
              VALUES (v_obl_id, p_cli_id, TO_NUMBER(TO_CHAR(SYSDATE, 'rrrr')),
                      TO_NUMBER(TO_CHAR(SYSDATE, 'mm')),   --150,
                                                        rplan.ppl_mao_codigo,   -- Modificado por vlucero 11/05/2005
                                                                             61, p_inm_id, p_cuenta,
                      SYSDATE, v_total_nc, 30, 0, SYSDATE,
                      v_numero_nc, v_neto_nc, p_usuario, SYSDATE, p_plan, p_inm_id,
                      SYSDATE, SYSDATE, 'N', SYSDATE, SUBSTR(p_cuenta, 1, 3),
                      p_grupo, p_tipo_resp);
      END IF;

      l_retorno := f_aplica_creditos(v_obl_id, p_usuario, 1, 11, 1, 1, SYSDATE, SYSDATE);
      v_id_mov := 0;   -- numerador de las líneas de la NC -
      v_acum_asig_pagos := 0;
      v_acum_neto := 0;

      FOR r_quita IN quita LOOP
         v_id_mov := v_id_mov + 1;
         v_asig_pagos := 0;

         BEGIN
            SELECT SUM(apa_imp_haber), SUM(apa_imp_haber - apa_imp_ali - apa_imp_iva)
              INTO v_asig_pagos, v_neto
              FROM ASIG_PAGOS
             WHERE apa_cct_obl_id = r_quita.cct_obl_id AND apa_cct_id_movimiento = r_quita.cct_id_movimiento;
         EXCEPTION
            WHEN NO_DATA_FOUND THEN
               v_asig_pagos := 0;
         END;

         IF v_asig_pagos IS NULL THEN
            v_asig_pagos := 0;
         END IF;

         IF v_neto IS NULL THEN
            v_neto := 0;
         END IF;

         v_acum_asig_pagos := v_acum_asig_pagos + v_asig_pagos;
         v_acum_neto := v_acum_neto + v_neto;

         /* Insertar detalle de NC */
         INSERT INTO CUENTAS_CORRIENTES
                     (cct_obl_id, cct_id_movimiento, cct_ser_codigo, cct_pef_anio, cct_pef_periodo,
                      cct_tipo_movimiento, cct_imp_debe, cct_imp_haber, cct_concepto, cct_fec_generacion,
                      cct_fec_cierre_contable, cct_usr_alta, cct_fec_alta, cct_rec_id, cct_nov_id,
                      cct_fec_vencimiento, cct_cnt_dias_recargo, cct_banco, cct_remesa, cct_secuencia,
                      cct_fec_aplicacion, cct_fec_proceso, cct_fec_cobro, cct_usr_baja, cct_fec_baja,
                      cct_usr_mod, cct_fec_mod, cct_estado, cct_se, cct_obl_id_cuota, cct_grp_codigo,
                      cct_cuenta, cct_suc_codigo, cct_tre_codigo, cct_imp_iva, cct_por_iva, cct_imp_ali,
                      cct_por_ali)
            SELECT v_obl_id, v_id_mov,   -- cct_ser_codigo+90000000, -- Modificado por Vlucero 11/05/2005
                                      cct_ser_codigo, cct_pef_anio, cct_pef_periodo, 61, 0,
                   cct_imp_debe - v_asig_pagos, cct_concepto, SYSDATE, SYSDATE, p_usuario, SYSDATE, NULL, NULL,
                   SYSDATE, NULL, NULL, NULL, NULL, SYSDATE, SYSDATE, NULL, NULL, NULL, NULL, NULL, 30, 'N',
                   NULL, p_grupo, p_cuenta, SUBSTR(p_cuenta, 1, 3), p_tipo_resp,
                   cct_imp_iva *((cct_imp_debe - v_asig_pagos) / cct_imp_debe), cct_por_iva,
                   cct_imp_ali *((cct_imp_debe - v_asig_pagos) / cct_imp_debe), cct_por_ali
              FROM CUENTAS_CORRIENTES
             WHERE cct_obl_id = r_quita.cct_obl_id AND cct_id_movimiento = r_quita.cct_id_movimiento;

         /* Anular Cuentas corrients de débitos en quita */
         UPDATE CUENTAS_CORRIENTES
            SET cct_estado = 65,
                cct_fec_baja = SYSDATE,
                cct_usr_baja = p_usuario
          WHERE cct_obl_id = r_quita.cct_obl_id AND cct_id_movimiento = r_quita.cct_id_movimiento;
      END LOOP;

      /* Resto en la cabecera de la obligación el monto ya pagado, que al momento del insert no lo
         tenía. */
      UPDATE OBLIGACIONES
         SET obl_imp_original = obl_imp_original - v_acum_asig_pagos,
             obl_imp_neto = obl_imp_neto - v_acum_neto
       WHERE obl_id = v_obl_id;

      /* Anular Oligaciones de débitos en quita */
      FOR r_obl_quita IN obl_quita LOOP
         UPDATE OBLIGACIONES
            SET obl_estado = 65,
                obl_usr_baja = p_usuario,
                obl_fec_baja = SYSDATE,
                obl_obl_id_nc = v_obl_id,
                obl_fec_generacion_nc = SYSDATE,
                --obl_mao_codigo = 150
                obl_mao_codigo = rplan.ppl_mao_codigo   -- Modificado por Vlucero 11/05/2005
          WHERE obl_id = r_obl_quita.cct_obl_id;
      END LOOP;
   END GENERA_NC_QUITA;

   FUNCTION encripta(clave VARCHAR2)
      RETURN VARCHAR2 IS
      longitud     NUMBER;
      nuevaclave   VARCHAR2(20) := '';
   BEGIN
      longitud := LENGTH(clave);

      WHILE longitud > 0 LOOP
         nuevaclave := nuevaclave ||(9 - SUBSTR(clave, longitud, 1));
         longitud := longitud - 1;
      END LOOP;

      RETURN nuevaclave;
   END encripta;

-------------------------------------------------------------------------------------
-- Esta funcion crea el regitro en la rel entre Recaudaciones y Novedades_facturables.
-- Retorna el ID del registro creado para poder luego ser actualizarlo.
---                                                                         Vlucero.
--------------------------------------------------------------------------------------
   FUNCTION crearrel(precid NUMBER, precrecid NUMBER, pnovid NUMBER, pusuario VARCHAR2)
      RETURN registro IS
      nret    registro;
      ntemp   NUMBER;
   BEGIN
      nret.codigo := 0;
      nret.descrip := '';

      SELECT rrn_seq.NEXTVAL
        INTO ntemp
        FROM DUAL;

      BEGIN
         INSERT INTO REL_REC_NOV
                     (rrn_id, rrn_rec_id, rrn_nov_id, rrn_rec_rec_id, rrn_fec_alta, rrn_usr_alta)
              VALUES (ntemp, precid, pnovid, precrecid, SYSDATE, pusuario);
      EXCEPTION
         WHEN OTHERS THEN
            nret.codigo := -1;
            nret.descrip := 'rec_id : ' || TO_CHAR(precid) || ' Error :' || SQLERRM;
      END;

      RETURN(nret);
   END crearrel;

-------------------------------------------------------------------------------------
-- Esta funcion actualiza el registro de la rel
-- para mantener el vinculo entre las dos tablas cuando existen
-- particiones de los créditos.
---                                                                         Vlucero.
--------------------------------------------------------------------------------------
   FUNCTION actualizarel(precid NUMBER, pnovid NUMBER, precrecid NUMBER, pusuario VARCHAR2)
      RETURN registro IS
      nret   registro;
   BEGIN
      nret.codigo := 0;
      nret.descrip := '';

      BEGIN
         UPDATE REL_REC_NOV
            SET rrn_nov_id = pnovid,
                rrn_rec_rec_id = precrecid,
                rrn_usr_mod = pusuario,
                rrn_fec_mod = SYSDATE
          WHERE rrn_rec_id = precid;
      EXCEPTION
         WHEN OTHERS THEN
            nret.codigo := -2;
            nret.descrip := 'RRN_REC_ID : ' || TO_CHAR(precid) || ' Error :' || SQLERRM;
      END;

      RETURN(nret);
   END actualizarel;

-------------------------------------------------------------------------------------
-- Esta funcion retorna el valor del NOV_ID que se le asigno a un registro de Recaudaciones
-- por medio de su REC_ID
---                                                                         Vlucero.
--------------------------------------------------------------------------------------
   FUNCTION getnovid(precid NUMBER)
      RETURN NUMBER IS
      ntemp   NUMBER(10);
   BEGIN
      BEGIN
         SELECT rrn_nov_id
           INTO ntemp
           FROM REL_REC_NOV
          WHERE rrn_rec_id = precid;
      EXCEPTION
         WHEN OTHERS THEN
            ntemp := -1;
      END;

      RETURN(ntemp);
   END getnovid;
---------------------------------------------------------------------------------------
END PKG_RECAUDACIONES;
/***************************************************************************************/
/*                                FIN DEL PACKAGE                                      */
/***************************************************************************************/
/

