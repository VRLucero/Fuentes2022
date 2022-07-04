CREATE OR REPLACE PACKAGE MANANTIAL.pkg_planes_especiales IS

  TYPE R_MONTO_CUOTA IS RECORD (V_CANT_CUOTAS PLANES_PAGO.PPL_CNT_CUOTAS%TYPE,
	 			   	  		    V_INTERES     PLANES_PAGO.PPL_TASA_INTERES%TYPE,
							   	V_TOTAL       PLANES_PAGO.PPL_DEUDA_HISTORICA%TYPE,
							   	V_POR_BONIF   PLANES_PAGO.PPL_BONIF_RECARGO%TYPE,
							   	V_TIPO_RESP   OBLIGACIONES.OBL_TRE_CODIGO%TYPE,
								V_SERVICIO    NUMBER(10));

  TYPE R_DATOS_M_CUOTA IS RECORD (V_IMP_IVA       NUMBER(20,5),
                                  V_IMP_ALI  	  NUMBER(20,5),
								  V_IMP_PERC      NUMBER(20,5),
							      V_IVA    		  impuestos.imp_porcentaje%TYPE,
			  					  V_ALICUOTA	  impuestos.imp_alicuota%TYPE,
			                      V_PERCEPCION    impuestos.imp_alicuota%TYPE,
							   	  V_UNA_CUOTA     NUMBER(17,10),
								  V_UNA_INTERES   NUMBER(17,10),
								  TOTAL_IVA_PESOS NUMBER(15,2),
								  V_INTERES_TOTAL NUMBER(17,10),
								  V_MONTO_CUOTA   NUMBER(17,10));

  FUNCTION F_Planes_Especiales (p_inserta_recauda pkg_recaudaciones.VIncRecaudacionRec,
                                p_tipo_responsable inmuebles.inm_tipo_responsable%TYPE,
                                p_imp_credito OUT planes_pago.ppl_monto_pago_inicial%TYPE)
                                RETURN pkg_recaudaciones.VIncRecaudacionRec;


 FUNCTION F_Novedad_Credito (p_inserta_recauda pkg_recaudaciones.VIncRecaudacionRec)
                              RETURN NUMBER;

 FUNCTION F_Actualiza_PPE (p_ppe_id planes_especiales.ppe_id%TYPE,
                            p_inm_id planes_especiales.ppe_inm_id%TYPE,
                            p_usuario novedades_facturables.nov_usr_alta%TYPE,
                            p_estado VARCHAR2 := 'S',
							p_modelo planes_especiales.ppe_mpp_id%TYPE)
							RETURN NUMBER;

/* FUNCTION F_Deuda_Historica (p_cuenta inmuebles.inm_cuenta%TYPE,
 		  					 p_fecha_deuda DATE,
							 p_fecha_vto DATE,
							 P_MPP_TIPO_PLAN_PAGO MODELOS_PLANES_PAGO.MPP_TIPO_PLAN_PAGO%TYPE,
							 p_modelo NUMBER,
							 p_cuotas NUMBER)
                             RETURN planes_especiales.ppe_deuda_historica%TYPE;*/
 FUNCTION F_Deuda_Historica (P_PPE_ID planes_especiales.ppe_id%Type)
                             RETURN planes_especiales.ppe_deuda_historica%TYPE;

  FUNCTION F_Completa_Plan(p_plan planes_especiales%ROWTYPE,
                           p_cuenta inmuebles.inm_cuenta%TYPE,
	                       p_usuario planes_pago.ppl_usr_alta%TYPE,
                           p_tipo_responsable inmuebles.inm_tipo_responsable%TYPE,
	                       p_inserta_recauda pkg_recaudaciones.VIncRecaudacionRec,
                           p_nuevo_plan VARCHAR := 'N',
						   p_fecha_deuda DATE,
						   p_fecha_vto DATE)
                           RETURN pkg_recaudaciones.VIncRecaudacionRec;

  FUNCTION F_Recupera_domicilio(P_INM_ID INMUEBLES.INM_ID%TYPE)
                                RETURN NUMBER;

  FUNCTION F_Recupera_grupo(P_SUCURSAL SUCURSALES.SUC_CODIGO%TYPE)
                            RETURN NUMBER;

  FUNCTION F_Genera_Ideal(P_CAB_PLANES          PLANES_PAGO%ROWTYPE,
                          P_UPDATE_OBLIGACIONES PKG_PLANES_PAGO.UpdateObligacionesTyp,
	                      P_CUENTA              INMUEBLES.INM_CUENTA%TYPE,
			     		  P_ID_PLAN_ESP         PLANES_ESPECIALES.PPE_ID%TYPE,
						  P_TIPO_RESPONSABLE    INMUEBLES.INM_TIPO_RESPONSABLE%TYPE,
                          P_Recauda             pkg_recaudaciones.VIncRecaudacionRec,
						  P_TEM_CCT_ID          DTE_CUENTAS_CORRIENTES.TEM_CCT_ID%TYPE)
                          RETURN pkg_recaudaciones.VIncRecaudacionRec;

  FUNCTION F_CONFECCIONA_PLAN_PAGO (P_CAB_PLANES          PLANES_PAGO%ROWTYPE,
                                    P_UPDATE_OBLIGACIONES PKG_PLANES_PAGO.UpdateObligacionesTyp,
                                    P_CUENTA              INMUEBLES.INM_CUENTA%TYPE,
					                P_TIPO_RESPONSABLE    INMUEBLES.INM_TIPO_RESPONSABLE%TYPE,
                                    P_Recauda             pkg_recaudaciones.VIncRecaudacionRec,
									P_TEM_CCT_ID          DTE_CUENTAS_CORRIENTES.TEM_CCT_ID%TYPE)
									RETURN pkg_recaudaciones.VIncRecaudacionRec;

  FUNCTION genera_monto_cuotas(P_MONTO_CUOTA PKG_PLANES_ESPECIALES.R_MONTO_CUOTA,
                               P_TEM_CCT_ID  DTE_CUENTAS_CORRIENTES.TEM_CCT_ID%TYPE)
                               RETURN PKG_PLANES_ESPECIALES.R_DATOS_M_CUOTA;

  FUNCTION genera_fecha_cuota(p_fecha_inicial DATE, p_cant_dia NUMBER)
                              RETURN DATE;

  PROCEDURE ACTUALIZA_TEMPORAL_CON_IVA(P_MONTO_ACTUALIZA NUMBER,
                                       P_TEM_CCT_ID DTE_CUENTAS_CORRIENTES.TEM_CCT_ID%TYPE,p_prueba OUT VARCHAR2);

  PROCEDURE ACTUALIZA_TEMPORAL_SIN_IVA(P_MONTO_ACTUALIZA NUMBER,
                                       P_TEM_CCT_ID DTE_CUENTAS_CORRIENTES.TEM_CCT_ID%TYPE,
									   P_SERVICIO   CUENTAS_CORRIENTES.CCT_SER_CODIGO%TYPE,
									   P_PRUEBA OUT VARCHAR2);

  FUNCTION f_recalcula_quita(p_cuenta inmuebles.inm_cuenta%TYPE,
                           p_mpp_id planes_especiales.PPE_MPP_ID%TYPE,
                           p_fecha_plan DATE)
                           RETURN planes_pago.PPL_QUITA%TYPE;

END;
/

CREATE OR REPLACE PACKAGE BODY MANANTIAL.PKG_PLANES_ESPECIALES IS

/* ******************************************************************************** */

FUNCTION F_Planes_Especiales (p_inserta_recauda PKG_RECAUDACIONES.VIncRecaudacionRec,
		 					   p_tipo_responsable INMUEBLES.inm_tipo_responsable%TYPE,
                               p_imp_credito OUT PLANES_PAGO.ppl_monto_pago_inicial%TYPE
							  )
							  RETURN PKG_RECAUDACIONES.VIncRecaudacionRec IS


   l_inserta_recauda PKG_RECAUDACIONES.VIncRecaudacionRec := p_inserta_recauda;
   r_plan PLANES_ESPECIALES%ROWTYPE;
   l_rec_id RECAUDACIONES.rec_id%TYPE := 0;
   l_deuda PLANES_ESPECIALES.PPE_DEUDA_HISTORICA%TYPE := 0;
   l_actualiza NUMBER;
   l_planes_pago PLANES_PAGO%ROWTYPE;
   l_importe NOVEDADES_FACTURABLES.nov_imp_neto%TYPE := 0;
   l_cuenta VARCHAR2(14);
   v_fecha_limite DATE;
   v_fecha_plan DATE;
   v_modelo_plan NUMBER;
   v_fec_vigencia DATE;
   v_fec_vigencia_his DATE;
   v_fecha_modelo DATE;
   V_MPP_TIPO_PLAN_PAGO MODELOS_PLANES_PAGO.MPP_TIPO_PLAN_PAGO%TYPE;

   l_quita_actual PLANES_PAGO.ppl_quita%TYPE;
   V_FEC_ULT_MODIF DATE;
   v_iguales VARCHAR2(1);

--como parámetro...
--   p_imp_credito planes_pago.ppl_monto_pago_inicial%type;

BEGIN
   --sacar -

   p_imp_credito := 0;

   /* Recupera el Plan Especial correspondiente y evalúa su vigencia - PEDIDO 2878746 - SROMERO */
    BEGIN
      SELECT ppe_fec_alta, ppe_mpp_id
	    INTO v_fecha_plan, v_modelo_plan
	    FROM PLANES_ESPECIALES
	   WHERE ppe_id = l_inserta_recauda.v_factura
       -- cambio introducido por pedido 11172423 - sromero - 06/11/20
       -- no se evalúa el estado, sólo que esté generado en el último año 
          -- AND ppe_generado <> 'D';
          AND ppe_fec_alta BETWEEN sysdate-365 AND sysdate;        
         -- Fin cambio pedido 11172423
         EXCEPTION WHEN OTHERS THEN v_modelo_plan := 0;
    END;
    BEGIN
      SELECT mpp_fec_mod, NVL(mpp_fec_fin_vigencia,SYSDATE) INTO v_fecha_modelo, v_fec_vigencia
      FROM MODELOS_PLANES_PAGO WHERE mpp_id = v_modelo_plan;
      EXCEPTION WHEN OTHERS THEN v_fecha_modelo := SYSDATE;
         v_fec_vigencia := SYSDATE;
    END;
    v_fec_vigencia_his := NULL;
   IF v_fecha_modelo > v_fecha_plan THEN  -- si el modelo tuvo cambios después de generado el PPE --
      BEGIN -- busca en el histórico para ver hasta cuándo estaba vigente --
        SELECT NVL(hmp_mpp_fec_fin_vigencia,SYSDATE)
        INTO v_fec_vigencia_his
        FROM HIS_MODELOS_PLANES_PAGO
       WHERE hmp_id = (SELECT MIN(hmp_id) FROM HIS_MODELOS_PLANES_PAGO
                       WHERE hmp_mpp_id = v_modelo_plan AND hmp_fec_alta >= v_fecha_plan);
        EXCEPTION WHEN OTHERS THEN v_fec_vigencia_his := NULL; -- si no encuentra en el histórico no hace nada, toma v_fec_vigencia, la del modelo actualmente --
      END;
      -- cambio para evaluación de si la vigencia es el único cambio (extensión de la vigencia) --
      v_iguales := 'N';
      BEGIN
         SELECT FNC_COMPARA_MODELOS(v_modelo_plan,v_fecha_plan) INTO v_iguales FROM dual;
         EXCEPTION WHEN OTHERS THEN v_iguales := 'N';
      END;
   END IF;
   -- Lo que está dentro del THEN es lo que hacía originalmente, se agregó el ELSE - PEDIDO 2878746 - SROMERO --
IF v_fecha_plan <= NVL(v_fec_vigencia_his,v_fec_vigencia) OR v_iguales = 'S' THEN -- si el modelo todavía está vigente --
   BEGIN
      SELECT *
	    INTO r_plan
	    FROM PLANES_ESPECIALES
	   WHERE ppe_id = l_inserta_recauda.v_factura
       -- cambio introducido por pedido 11172423 - sromero - 06/11/20
       -- no se evalúa el estado, sólo que esté generado en el último año 
       --   AND ppe_generado = 'N';
          AND ppe_fec_alta BETWEEN sysdate-365 AND sysdate;        
         -- Fin cambio pedido 11172423
   EXCEPTION
      WHEN NO_DATA_FOUND THEN
      /* PEDIDO 2642490 - SROMERO - Se agrega que si no encuentra el PPE sin generar, antes de
      dejar como crédito busque si hay una cuota del plan de pago definitivo impaga por el mismo
      importe*/
         BEGIN
              SELECT 1,ppe_ppl_id,obl_saldo,obl_pef_anio,obl_pef_periodo
              INTO l_inserta_recauda.p_estado, l_inserta_recauda.v_factura, l_inserta_recauda.v_importe, l_inserta_recauda.v_anio, l_inserta_recauda.v_periodo
              FROM PLANES_ESPECIALES, OBLIGACIONES
              WHERE ppe_id = l_inserta_recauda.v_factura
                AND ppe_ppl_id IS NOT NULL --and ppe_generado = 'S'
                AND obl_ppl_id = ppe_ppl_id
                AND obl_tpc_codigo = 8
                AND obl_estado = 15
                AND obl_saldo = l_inserta_recauda.v_importe
                AND ROWNUM = 1;
              RETURN l_inserta_recauda;
            EXCEPTION WHEN OTHERS THEN
              /* Si tampoco encontró el plan definitivo con cuotas impagas y de igual importe genera el crédito */
              l_inserta_recauda.p_estado := 2;
              l_rec_id := F_Novedad_Credito(l_inserta_recauda);
              IF l_rec_id < 0 THEN
                 l_inserta_recauda.p_estado := l_rec_id;
              END IF;
              RETURN l_inserta_recauda;
         END;
      WHEN OTHERS THEN
	      /* Si el plan se encuentra descartado se debe genera crédito por pago duplicado */
 		  l_inserta_recauda.p_estado := 2;
          l_rec_id := F_Novedad_Credito(l_inserta_recauda);
		  IF l_rec_id < 0 THEN
             l_inserta_recauda.p_estado := l_rec_id;
		  END IF;
		  RETURN l_inserta_recauda;
   END;
ELSE
	      /* Si el plan no está vigente se debe genera crédito por pago duplicado */
 		  l_inserta_recauda.p_estado := 2;
          l_rec_id := F_Novedad_Credito(l_inserta_recauda);
		  IF l_rec_id < 0 THEN
             l_inserta_recauda.p_estado := l_rec_id;
		  END IF;
		  RETURN l_inserta_recauda;
END IF;

   -- PEDIDO 2642490 - SROMERO Determina si puede tomar lo que está en la tabla de modelos o debe pasar por el histórico --
   BEGIN
     SELECT mpp_fec_mod INTO v_fec_ult_modif
     FROM MODELOS_PLANES_PAGO
	  WHERE  MPP_ID = r_plan.ppe_mpp_id;
     EXCEPTION WHEN OTHERS THEN  v_fec_ult_modif := NULL;
   END;

   IF NVL(v_fec_ult_modif,TO_DATE('01011995','ddmmrrrr')) <= r_plan.ppe_fec_alta THEN -- Si no fue modificado luego del plan --
        BEGIN
          SELECT DECODE(MPP_TIPO_PLAN_PAGO,'R',TO_DATE('31129999','DDMMRRRR'),DECODE(mpp_incluye_resto,'S',NVL(R_PLAN.ppe_fec_alta,SYSDATE),NVL(DECODE(mpp_fecha_hasta,'FGP',TRUNC(SYSDATE),TO_DATE(MPP_FECHA_HASTA,'DD/MM/RRRR')),MPP_DEUDA_FEC_HASTA))),
                 MPP_TIPO_PLAN_PAGO
          INTO   V_FECHA_LIMITE,
                 V_MPP_TIPO_PLAN_PAGO
          FROM   MODELOS_PLANES_PAGO
          WHERE  MPP_ID = r_plan.ppe_mpp_id;
       EXCEPTION WHEN NO_DATA_FOUND THEN
                      V_FECHA_LIMITE := NULL;
       END;
    --   dbms_output.put_line('V_FECHA_LIMITE '||V_FECHA_LIMITE);
   ELSE -- si fue modificado pasa por el histórico --
        BEGIN
          SELECT DECODE(hmp_MPP_TIPO_PLAN_PAGO,'R',TO_DATE('31129999','DDMMRRRR'),DECODE(hmp_mpp_incluye_resto,'S',NVL(R_PLAN.ppe_fec_alta,SYSDATE),NVL(DECODE(hmp_mpp_fecha_hasta,'FGP',TRUNC(SYSDATE),TO_DATE(hmp_MPP_FECHA_HASTA,'DD/MM/RRRR')),hmp_MPP_DEUDA_FEC_HASTA))),
                 hmp_MPP_TIPO_PLAN_PAGO
          INTO   V_FECHA_LIMITE,
                 V_MPP_TIPO_PLAN_PAGO
          FROM   HIS_MODELOS_PLANES_PAGO
          WHERE  HMP_MPP_ID = r_plan.ppe_mpp_id
             AND HMP_ID = (SELECT MIN(B.HMP_ID) FROM HIS_MODELOS_PLANES_PAGO B
                                 WHERE B.HMP_MPP_ID = r_plan.ppe_mpp_id AND b.hmp_fec_alta >= r_plan.ppe_fec_alta);
       EXCEPTION WHEN NO_DATA_FOUND THEN
                      V_FECHA_LIMITE := NULL;
       END;
     --  dbms_output.put_line('V_FECHA_LIMITE '||V_FECHA_LIMITE);
   END IF;

   /* Recuperar el importe original */
   l_cuenta := l_inserta_recauda.v_identificacion;

--   l_deuda := F_Deuda_Historica(l_cuenta,NVL(r_plan.ppe_fec_alta,SYSDATE),NVL(V_FECHA_LIMITE,NVL(r_plan.ppe_fec_alta,SYSDATE)),V_MPP_TIPO_PLAN_PAGO,r_plan.ppe_mpp_id, r_plan.ppe_cnt_cuotas);
   l_deuda := F_DEUDA_HISTORICA(l_inserta_recauda.v_factura);

    dbms_output.put_line('cuenta '||l_cuenta||' deuda '||l_deuda );

   IF l_deuda = 0 THEN /* Si la deuda se encuentra cancelada por pago duplicado */

 	  l_inserta_recauda.p_estado := 2;
      l_rec_id := F_Novedad_Credito(l_inserta_recauda); /* Genera NC */
      l_actualiza := F_Actualiza_PPE (r_plan.ppe_id, r_plan.ppe_inm_id, p_inserta_recauda.V_USUARIO, 'D', r_plan.ppe_mpp_id); /* Actualiza el valor del campo PPE_GENERADO */

    dbms_output.put_line('l_rec_id '||l_rec_id||' l_actualiza '||l_actualiza );

   ELSIF ABS(l_deuda-(r_plan.ppe_deuda_historica+r_plan.ppe_quita)) < 0.03 THEN /* Genera el plan de pago */

 	  l_inserta_recauda.p_estado := 1;
      l_inserta_recauda := F_Completa_Plan(r_plan, l_inserta_recauda.v_identificacion, p_inserta_recauda.V_USUARIO, p_tipo_responsable, l_inserta_recauda,'N',NVL(r_plan.ppe_fec_alta,SYSDATE),NVL(V_FECHA_LIMITE,NVL(r_plan.ppe_fec_alta,SYSDATE))); /* Actualiza el valor del campo PPE_GENERADO */
      l_actualiza := F_Actualiza_PPE (r_plan.ppe_id, r_plan.ppe_inm_id, p_inserta_recauda.V_USUARIO, 'S', r_plan.ppe_mpp_id);

    dbms_output.put_line(' l_actualiza '||l_actualiza );

   ELSIF l_deuda > 0 AND l_deuda <> (r_plan.ppe_deuda_historica+r_plan.ppe_quita) THEN


      l_inserta_recauda.p_estado := 1;
      l_importe := l_inserta_recauda.v_importe; -- Pago original -

	  /* Determina el importe de la quita actual */
	  l_quita_actual := f_recalcula_quita(l_inserta_recauda.v_identificacion, R_PLAN.PPE_MPP_ID, r_plan.ppe_fec_alta);

      -- dbms_output.put_line('l_deuda - l_quita_actual '||to_char(l_deuda - l_quita_actual));

      IF (l_deuda - l_quita_actual) <= r_plan.ppe_imp_cuota THEN

		 /* Completa cabecera del plan de acuerdo a las nuevas condiciones */
		 l_inserta_recauda.v_importe := l_deuda - l_quita_actual; -- Deuda Historica
		 r_plan.ppe_cnt_cuotas := 1;

          -- dbms_output.put_line('l_inserta_recauda.v_importe '||l_inserta_recauda.v_importe);

         l_inserta_recauda := F_Completa_Plan(r_plan, l_inserta_recauda.v_identificacion, p_inserta_recauda.V_USUARIO, p_tipo_responsable, l_inserta_recauda, 'S',NVL(r_plan.ppe_fec_alta,SYSDATE),NVL(V_FECHA_LIMITE,NVL(r_plan.ppe_fec_alta,SYSDATE))); /* Actualiza el valor del capo PPE_GENERADO */

		 /* Genera nota de crédito por la diferencia entre la deuda historica y lo pagado */
         IF l_deuda - l_quita_actual < r_plan.ppe_imp_cuota THEN

             l_inserta_recauda.v_importe := l_importe - (l_deuda-l_quita_actual); -- Genera novedad por diferencia
             l_rec_id := F_Novedad_Credito(l_inserta_recauda);
		     l_inserta_recauda.p_estado := 4;
		     p_imp_credito := l_inserta_recauda.v_importe;
			 l_inserta_recauda.v_importe := l_deuda-l_quita_actual;

		 END IF;

      ELSE

	     l_inserta_recauda.v_importe := l_deuda;  -- Pasa la deuda historica
         l_inserta_recauda := F_Completa_Plan(r_plan, l_inserta_recauda.v_identificacion, p_inserta_recauda.V_USUARIO, p_tipo_responsable, l_inserta_recauda, 'S',NVL(r_plan.ppe_fec_alta,SYSDATE),NVL(V_FECHA_LIMITE,NVL(r_plan.ppe_fec_alta,SYSDATE))); /* Actualiza el valor del capo PPE_GENERADO */
         l_inserta_recauda.v_importe := l_importe; -- Recupera el pago original

	  END IF;

      l_actualiza := F_Actualiza_PPE (r_plan.ppe_id, r_plan.ppe_inm_id,p_inserta_recauda.V_USUARIO, 'D', r_plan.ppe_mpp_id); /* Actualiza el valor del capo PPE_GENERADO */

   ELSIF l_deuda < 0 THEN  /* Error en la recuperación de la deuda */

      l_inserta_recauda.p_estado := -1;
	  BEGIN
	  INSERT INTO ERRORES (ERO_ID, ERO_MODULO               , ERO_ERROR, ERO_COD_ERROR, ERO_TABLA, ERO_REGISTRO, ERO_OBSERVACION, ERO_USU_ALTA, ERO_FEC_ALTA)
	  VALUES (    ero_seq.NEXTVAL,'PKG_PLANES_ESPECIALES.F_Planes_Especiales()','l_deuda < 0',-1,            'PPE_ID'  ,r_plan.ppe_id,'Deuda Menor a Cero','PKG_PLANES_ESPECIALES', SYSDATE);
	  EXCEPTION WHEN OTHERS THEN
	     l_inserta_recauda.p_estado := -1;
	  END;

   ELSIF l_deuda > 0 AND l_deuda > (r_plan.ppe_deuda_historica+r_plan.ppe_quita) THEN /* En caso que la deuda sea mayor que el plan */

      l_inserta_recauda.p_estado := -1;
	  BEGIN
	  INSERT INTO ERRORES (ERO_ID, ERO_MODULO               , ERO_ERROR, ERO_COD_ERROR, ERO_TABLA, ERO_REGISTRO, ERO_OBSERVACION, ERO_USU_ALTA, ERO_FEC_ALTA)
	  VALUES (    ero_seq.NEXTVAL,'PKG_PLANES_ESPECIALES.F_Planes_Especiales()','l_deuda >Historica+Quita',-1,            'PPE_ID'  ,r_plan.ppe_id,TO_CHAR(l_deuda)||'>'||TO_CHAR(r_plan.ppe_deuda_historica)||'+'||TO_CHAR(r_plan.ppe_quita),'PKG_PLANES_ESPECIALES', SYSDATE);
	  EXCEPTION WHEN OTHERS THEN
	     l_inserta_recauda.p_estado := -1;
	  END;


   END IF;

   IF l_actualiza = -1 THEN
	       l_inserta_recauda.p_estado := -2;
		   BEGIN
		   INSERT INTO ERRORES (ERO_ID, ERO_MODULO               , ERO_ERROR, ERO_COD_ERROR, ERO_TABLA, ERO_REGISTRO, ERO_OBSERVACION, ERO_USU_ALTA, ERO_FEC_ALTA)
	       VALUES (    ero_seq.NEXTVAL,'PKG_PLANES_ESPECIALES.F_Actualiza_Ppe()','Error de Actualizacion',-1,            'PPE_ID'  ,r_plan.ppe_id,'Error al actualizar el estado del plan Especial','PKG_PLANES_ESPECIALES', SYSDATE);
           EXCEPTION WHEN OTHERS THEN
	         l_inserta_recauda.p_estado := -2;
	       END;
   END IF;
   IF l_rec_id = -1 THEN
	       l_inserta_recauda.p_estado := -3;
		   BEGIN
 		   INSERT INTO ERRORES (ERO_ID, ERO_MODULO               , ERO_ERROR, ERO_COD_ERROR, ERO_TABLA, ERO_REGISTRO, ERO_OBSERVACION, ERO_USU_ALTA, ERO_FEC_ALTA)
	       VALUES (    ero_seq.NEXTVAL,'PKG_PLANES_ESPECIALES.F_Novedad_Credito','Error de Actualizacion',-1,            'PPE_ID'  ,r_plan.ppe_id,'Error al generar el Credito','PKG_PLANES_ESPECIALES', SYSDATE);
		   EXCEPTION WHEN OTHERS THEN
	          l_inserta_recauda.p_estado := -3;
	       END;

   END IF;

   RETURN l_inserta_recauda;

END;


/* ************************************************************** */
/* Función que inserta las novedades_facturables del tipo crédito */
/* ************************************************************** */
FUNCTION F_Novedad_Credito
            (p_inserta_recauda PKG_RECAUDACIONES.VIncRecaudacionRec)
			 RETURN NUMBER IS

  r_novedad NOVEDADES_FACTURABLES%ROWTYPE;
  r_iva PKG_SERVICIOS_FIJOS.IvaRec;
  l_inm_id INMUEBLES.inm_id%TYPE;
  r_importe  PKG_SERVICIOS_FIJOS.ImporteRec;
  v_rec_id RECAUDACIONES.rec_id%TYPE;

BEGIN

   /* Recupera el tipo de responsable del inmueble */
   BEGIN
      SELECT inm_tipo_responsable, inm_id INTO r_novedad.nov_cod_iva, l_inm_id
	    FROM INMUEBLES WHERE inm_cuenta = p_inserta_recauda.v_identificacion;

   EXCEPTION
      WHEN OTHERS THEN
	     /* No pudo recuerar el tipo de responsable del inmuebles */
	     RETURN -4;
   END;

   /* Recupera el Iva correspondiente al responsable del inmueble */
   r_iva := PKG_SERVICIOS_FIJOS.F_Imp_IVA(r_novedad.nov_cod_iva,650001,p_inserta_recauda.P_FECHA);
   IF r_iva.iva IS NULL THEN
      /* No pudo recuperar el IVA */
	  RETURN -5;
   END IF;

   /* Genera la Novedad_Facturable del crédito por el pago duplicado */
   r_novedad.nov_imp_iva_cf := 0;
   r_novedad.nov_imp_iva_ex := 0;
   r_novedad.nov_imp_iva_ri := 0;
   r_novedad.nov_imp_iva_rni := 0;
   r_novedad.nov_imp_iva_mon := 0;
   r_novedad.nov_imp_cambio := 1;

   r_novedad.NOV_SER_CODIGO := 650001;
   r_novedad.NOV_CON_INM_ID := l_inm_id;
   r_novedad.NOV_INM_ID := l_inm_id;
   r_novedad.NOV_TIPO_NOVEDAD := 10;
   r_novedad.NOV_FEC_NOVEDAD := TRUNC(SYSDATE); --p_inserta_recauda.P_FECHA;
   r_novedad.NOV_ESTADO := 2;
   r_novedad.NOV_TIPO_ORIGEN := 'D';
   r_novedad.nov_obl_id := NULL;
   r_novedad.NOV_NRO_ORIGEN := p_inserta_recauda.v_factura;
   r_novedad.nov_descripcion := 'Pago Duplicado';
   r_novedad.nov_usr_alta := p_inserta_recauda.V_USUARIO;
   r_novedad.nov_fec_alta := SYSDATE;

   r_importe := F_IVA_INVERSO(p_inserta_recauda.V_IMPORTE,
                              r_iva.iva,
							  r_iva.alicuota,
							  r_iva.percepcion);
   r_novedad.NOV_IMP_NETO := r_importe.neto;

   /* Inserta la Novedad */
   PKG_SERVICIOS_FIJOS.P_novedad_facturable(r_novedad, r_iva);
 /*  BEGIN
      SELECT MAX(rec_id) INTO v_rec_id
      FROM RECAUDACIONES
      WHERE rec_nro_factura = p_inserta_recauda.v_factura
        AND rec_inm_id = l_inm_id
        AND rec_erc_codigo = 2;
      EXCEPTION WHEN NO_DATA_FOUND THEN v_rec_id := 0;
   END;
   BEGIN
      INSERT INTO REL_REC_NOV (RRN_ID, RRN_REC_ID, RRN_NOV_ID,  RRN_FEC_ALTA, RRN_USR_ALTA)
      VALUES (rrn_seq.NEXTVAL, NVL(v_rec_id,100000001), r_novedad.nov_id, SYSDATE, p_inserta_recauda.v_usuario);
   END;*/

   RETURN 1;

END;


/* ************************************************************* */
/* Actualiza el estado de generado de la tabla de plan especial */
/* ************************************************************* */
FUNCTION F_Actualiza_PPE (p_ppe_id PLANES_ESPECIALES.ppe_id%TYPE,
                          p_inm_id PLANES_ESPECIALES.ppe_inm_id%TYPE,
                          p_usuario NOVEDADES_FACTURABLES.nov_usr_alta%TYPE,
						  p_estado VARCHAR2 := 'S',
						  p_modelo PLANES_ESPECIALES.ppe_mpp_id%TYPE)
                          RETURN NUMBER IS

BEGIN

   UPDATE PLANES_ESPECIALES SET ppe_generado = DECODE(ppe_id,p_ppe_id,p_estado,'D'),
                                ppe_usr_mod = p_usuario,
								ppe_fec_mod = SYSDATE
     WHERE ppe_inm_id = p_inm_id
	   AND ppe_mpp_id = p_modelo;

   RETURN 1;

EXCEPTION
   WHEN OTHERS THEN
      RETURN -1;
END;


/* *************************************************************************** */
/* Recupera la Deuda Historica de las obligaciones que forman el plan de pago */
/* *************************************************************************** */
 FUNCTION F_DEUDA_HISTORICA (p_ppe_id PLANES_ESPECIALES.ppe_id%TYPE)
                             RETURN PLANES_ESPECIALES.ppe_deuda_historica%TYPE IS

	l_deuda PLANES_ESPECIALES.ppe_deuda_historica%TYPE := 0;
--    l_cuenta NUMBER;
--	v_deuda planes_especiales.ppe_deuda_historica%TYPE;
--	r_modelo modelos_planes_pago%ROWTYPE;

--    spr_actual VARCHAR2(2000) := 'select * from detalle_modelo_plan where mpd_mpp_id = :p_modelo and mpd_cant_cuotas = :p_cuotas
--          AND mpd_fec_alta <= :p_fec_plan AND NVL(mpd_fec_baja,:p_fec_plan) >= :p_fec_plan';

 /*  cursor sumas_por_rangos (p_cuotas number,
                            P_MODELO MODELOS_PLANES_PAGO.MPP_ID%TYPE) is
-- se cambia por cursor dinámico para poder utilizar la parametrización actual o el histórico --
   		  select *
		  from   detalle_modelo_plan
		  where  mpd_mpp_id = p_modelo
		  and    mpd_cant_cuotas = p_cuotas;*/

--   CURSOR c_obligaciones(P_REG_SUMAS detalle_modelo_plan%ROWTYPE,
--                         R_MODELO_PLAN         MODELOS_PLANES_PAGO%ROWTYPE)
--       IS SELECT obl_saldo
--       	  FROM   OBLIGACIONES
--          WHERE  OBL_CUENTA = P_CUENTA
--            AND  OBL_ESTADO IN (11,15)
--            AND  ((TRUNC(OBL_FEC_VENCIMIENTO) <= TRUNC(p_fecha_deuda) AND R_MODELO_PLAN.MPP_TIPO_PLAN_PAGO != 'R')
--			      OR (TRUNC(OBL_FEC_GENERACION) <= TRUNC(p_fecha_deuda) AND R_MODELO_PLAN.MPP_TIPO_PLAN_PAGO = 'R')
--				 )
--            AND  OBL_SE = 'N'
--            AND  OBL_PEF_ANIO >= 1995
--            AND  OBL_PPL_ID IS NULL
--            AND  OBL_SALDO > 0 AND obl_boleta_deuda IS NULL
--            AND  OBL_TPC_CODIGO > 79
--            AND  OBL_TPC_CODIGO <> 81
--            AND (
--			      (
--			       TRUNC(OBL_FEC_VENCIMIENTO)
--				         BETWEEN P_REG_SUMAS.MPD_FECHA_DEUDA_DESDE
--                               AND
--								 DECODE( P_REG_SUMAS.MPD_FECHA_HASTA,
--								            'FGP',p_fecha_deuda,
--			  	  						     TO_DATE(
--										 		     NVL(
--												 	     P_REG_SUMAS.MPD_FECHA_HASTA,
--														 TO_CHAR(OBL_FEC_VENCIMIENTO,'DD/MM/RRRR')
--													    ),
--												     'DD/MM/RRRR'
--										            )
--									   )
--				  )
--			     OR
--				  (R_MODELO_PLAN.MPP_TIPO_PLAN_PAGO = 'R')
--				) AND
--					 (
--					    (
--						   (
--						     TRUNC(OBL_FEC_VENCIMIENTO)
--							       BETWEEN NVL(R_MODELO_PLAN.MPP_DEUDA_FEC_DESDE,TRUNC(OBL_fEC_VENCIMIENTO))
--								         AND
--									       NVL(
--									           DECODE(R_MODELO_PLAN.MPP_FECHA_HASTA,
--												      'FGP',p_fecha_deuda,
--												       TO_DATE(
--														       NVL(
--															       R_MODELO_PLAN.MPP_FECHA_HASTA,
--															       TO_CHAR(OBL_FEC_VENCIMIENTO,'DD/MM/RRRR')
--														          ),
--														       'DD/MM/RRRR'
--													          )
--  							  			             ),
--										       TRUNC(OBL_FEC_VENCIMIENTO)
--										      )
--						   )
--						 OR
--						   (
--							 R_MODELO_PLAN.MPP_INCLUYE_RESTO = 'S'
--						   )
--						)
--					   OR
--						(R_MODELO_PLAN.MPP_TIPO_PLAN_PAGO = 'R')
--					 );
--TYPE detalle_del_plan IS REF CURSOR;
--sumas_por_rangos detalle_del_plan;
--v_fec_ult_modif DATE;
--v_reg_detalle detalle_Modelo_plan%ROWTYPE;
BEGIN

l_deuda := 0;
BEGIN
SELECT SUM(obl_saldo) INTO l_deuda
       	  FROM   OBLIGACIONES, REL_PPE_OBL
          WHERE  OBL_ESTADO IN (11,15)
            AND  OBL_SE = 'N'
            AND  OBL_PEF_ANIO >= 1995
            AND  OBL_PPL_ID IS NULL
            AND  OBL_SALDO > 0 AND obl_boleta_deuda IS NULL
            AND  OBL_TPC_CODIGO > 79
            AND  OBL_TPC_CODIGO <> 81
            AND  obl_id = rpo_obl_id
            AND  rpo_ppe_id = p_ppe_id;
END;
RETURN NVL(l_deuda,0);

---- PEDIDO 2642490 - SROMERO Determina si puede tomar la parametrización actual o debe pasar por el histórico --
--    BEGIN
--       SELECT mpp_fec_mod INTO v_fec_ult_modif
--       FROM modelos_planes_pago
--	  WHERE mpp_id = p_modelo;
--        EXCEPTION WHEN NO_DATA_FOUND THEN v_fec_ult_modif := NULL;
--    END;

--    BEGIN
--      IF v_fec_ult_modif < p_fecha_deuda THEN  -- no se modificó luego del plan --
--          SELECT * INTO r_modelo
--          FROM modelos_planes_pago
--          WHERE mpp_id = p_modelo;
--      ELSE  -- se modificó luego del plan --
--         BEGIN
--          SELECT HMP_MPP_ID, HMP_MPP_MIN_CNT_CUOTAS, HMP_MPP_FEC_INICIO_VIGENCIA, HMP_MPP_IMP_MIN_DEUDA,
--                 HMP_MPP_CNT_DIAS_CADUCIDAD, HMP_MPP_TIPO_PLAN_PAGO, HMP_MPP_MODALIDAD, HMP_MPP_USR_ALTA,
--                 HMP_MPP_FEC_ALTA, HMP_MPP_TCL_TIPO, HMP_MPP_MAX_CNT_CUOTAS, HMP_MPP_MIN_IMP_CUOTA,
--                 HMP_MPP_MAX_BONIF_RECARGO, HMP_MPP_MAX_BONIF_INTERES, HMP_MPP_TASA_INTERES,
--                 HMP_MPP_TASA_RECARGO, HMP_MPP_FEC_FIN_VIGENCIA, HMP_MPP_USR_BAJA, HMP_MPP_FEC_BAJA,
--                 HMP_MPP_USR_MOD, HMP_MPP_FEC_MOD, HMP_MPP_QUITA, HMP_MPP_MONTO_INICIAL, HMP_MPP_IMP_MAX_DEUDA,
--                 HMP_MPP_CON_PLAN_ACTIVO, HMP_MPP_DEUDA_FEC_DESDE, HMP_MPP_DEUDA_FEC_HASTA,
--                 HMP_MPP_INCLUYE_RESTO, HMP_MPP_CON_PAGO_NO_ING, HMP_MPP_CON_PEND_APLIC,
--                 HMP_MPP_EN_CONC_Y_QUIEBRA, HMP_MPP_EN_GEST_JUDICIAL, HMP_MPP_TERCERIZADOS,
--                 HMP_MPP_FEC_LIMITE_VIG, HMP_MPP_GRANDES_CLIENTES, HMP_MPP_VIGENCIA, HMP_MPP_FECHA_HASTA,
--                 HMP_MPP_CUOTA_QUITA, HMP_MPP_MAO_CODIGO , HMP_MPP_DESCRIPCION
--          INTO r_modelo
--          FROM his_modelos_planes_pago
--          WHERE hmp_mpp_id = p_modelo
--             AND HMP_ID = (SELECT MIN(B.HMP_ID) FROM HIS_MODELOS_PLANES_PAGO B
--                                 WHERE B.HMP_MPP_ID = p_modelo AND B.HMP_FEC_ALTA >= p_fecha_deuda );
--            -- PEDIDO 2642490 - SROMERO si debe pasar por el histórico cambia el cursor dinámico --
--            spr_actual := REPLACE(spr_actual,'mpd','hdm_mpd');
--            spr_actual := REPLACE(spr_actual,'detalle','his_detalle');
--            spr_actual := REPLACE(spr_actual,'*','HDM_MPD_MPP_ID, HDM_MPD_CANT_CUOTAS, HDM_MPD_FECHA_DEUDA_DESDE, HDM_MPD_FECHA_DEUDA_HASTA, HDM_MPD_QUITA, HDM_MPD_TIPO_QUITA, HDM_MPD_BONIF_RECARGOS, HDM_MPD_BONIF_INTERESES, HDM_MPD_IMP_MINIMO_CUOTA, HDM_MPD_USR_ALTA, HDM_MPD_FEC_ALTA, HDM_MPD_USR_BAJA, HDM_MPD_FEC_BAJA, HDM_MPD_USR_MOD, HDM_MPD_FEC_MOD, HDM_MPD_FECHA_HASTA');
--            EXCEPTION WHEN NO_DATA_FOUND THEN -- casos con cambios pero todavía sin histórico --
--            BEGIN
--              SELECT * INTO r_modelo
--              FROM modelos_planes_pago
--              WHERE mpp_id = p_modelo;
--            END;
--         END;
--      END IF;
--	  EXCEPTION WHEN NO_DATA_FOUND THEN
--	    BEGIN
--           IF P_MPP_TIPO_PLAN_PAGO IN ('W','Y') THEN
--                SELECT SUM(NVL(obl_saldo,0)) INTO l_deuda
--            	  FROM obligaciones
--            	 WHERE obl_cuenta = p_cuenta
--            	   AND obl_estado IN (11,15)
--            	   AND obl_ppl_id IS NULL
--            	   AND obl_pef_anio >= 1995
--    			   AND obl_boleta_deuda IS NULL
--           -- 	   AND trunc(OBL_FEC_VENCIMIENTO) <= trunc(p_fecha_vto)
--            	   AND TRUNC(OBL_FEC_generacion) <= TRUNC(p_fecha_deuda)
--            	   AND obl_tpc_codigo > 79 AND obl_tpc_codigo <> 81
--            	   AND obl_se = 'N';
--           ELSE

--                SELECT SUM(NVL(obl_saldo,0)) INTO l_deuda
--             	  FROM obligaciones
--             	 WHERE obl_cuenta = p_cuenta
--             	   AND obl_estado IN (11,15)
--             	   AND obl_ppl_id IS NULL
--    			   AND obl_boleta_deuda IS NULL
--             	   AND obl_pef_anio >= 1995
--             	   AND TRUNC(OBL_FEC_VENCIMIENTO) < TRUNC(p_fecha_vto)
--             	   AND TRUNC(OBL_FEC_generacion) <= TRUNC(p_fecha_deuda)
--             	   AND obl_tpc_codigo > 79 AND obl_tpc_codigo <> 81
--             	   AND obl_se = 'N';
--           END IF;
--		   RETURN NVL(L_DEUDA,0);
--	   END;
--	END;
--    v_deuda := 0;

--    OPEN sumas_por_rangos FOR spr_actual USING p_modelo,p_cuotas,p_fecha_deuda,p_fecha_deuda,p_fecha_deuda;
--    LOOP
--	      FETCH sumas_por_rangos INTO v_reg_detalle;
--		     EXIT WHEN sumas_por_rangos%NOTFOUND;
--		  FOR r IN c_obligaciones (v_reg_detalle, r_modelo) LOOP
--		      v_deuda := v_deuda + r.obl_saldo;
--		  END LOOP;
--    END LOOP;
--    CLOSE sumas_por_rangos;

--  /*  for reg_sumas in sumas_por_rangos (p_cuotas, p_modelo) loop
--	    begin
--		  for r in c_obligaciones (reg_sumas, r_modelo) loop
--		      v_deuda := v_deuda + r.obl_saldo;
--		  end loop;
--		end;
--	end loop;*/
--    RETURN NVL(v_deuda,0);
--	/*
--    begin
--       IF P_MPP_TIPO_PLAN_PAGO IN ('W','Y') THEN
--            select sum(nvl(obl_saldo,0)) into l_deuda
--        	  from obligaciones
--        	 where obl_cuenta = p_cuenta
--        	   and obl_estado in (11,15)
--        	   and obl_ppl_id is null
--        	   and obl_pef_anio >= 1995
--			   and obl_boleta_deuda is null
--       -- 	   AND trunc(OBL_FEC_VENCIMIENTO) <= trunc(p_fecha_vto)
--        	   AND trunc(OBL_FEC_generacion) <= trunc(p_fecha_deuda)
--        	   and obl_tpc_codigo > 79 and obl_tpc_codigo <> 81
--        	   and obl_se = 'N';
--       ELSE

--            select sum(nvl(obl_saldo,0)) into l_deuda
--         	  from obligaciones
--         	 where obl_cuenta = p_cuenta
--         	   and obl_estado in (11,15)
--         	   and obl_ppl_id is null
--			   and obl_boleta_deuda is null
--         	   and obl_pef_anio >= 1995
--         	   AND trunc(OBL_FEC_VENCIMIENTO) < trunc(p_fecha_vto)
--         	   AND trunc(OBL_FEC_generacion) <= trunc(p_fecha_deuda)
--         	   and obl_tpc_codigo > 79 and obl_tpc_codigo <> 81
--         	   and obl_se = 'N';
--       END IF;

--   exception

--    when no_data_found then
--	   return 0;  -- No se encontraron dedudas

--    when others then
--	   return -1;
--   end;

--	return nvl(l_deuda,0);
--*/
 END;


 /* ********************************************************************** */
 /* Función que completa los datos necesarios para generar el plan de pago */
 /* ********************************************************************** */

 FUNCTION F_Completa_Plan(p_plan PLANES_ESPECIALES%ROWTYPE,
                          p_cuenta INMUEBLES.inm_cuenta%TYPE,
	                      p_usuario PLANES_PAGO.ppl_usr_alta%TYPE,
                          p_tipo_responsable INMUEBLES.inm_tipo_responsable%TYPE,
	                      p_inserta_recauda PKG_RECAUDACIONES.VIncRecaudacionRec,
                          p_nuevo_plan VARCHAR := 'N',
						  p_fecha_deuda DATE,
						  p_fecha_vto DATE)
                          RETURN PKG_RECAUDACIONES.VIncRecaudacionRec IS

/*   cursor sumas_por_rangos (p_cuotas number,
                            P_MODELO MODELOS_PLANES_PAGO.MPP_ID%TYPE) is
-- se cambia por cursor dinámico para poder utilizar la parametrización actual o el histórico --
   		  select *
		  from   detalle_modelo_plan
		  where  mpd_mpp_id = p_modelo
		  and    mpd_cant_cuotas = p_cuotas;*/
    spr_actual VARCHAR2(2000) := 'select * from detalle_modelo_plan where mpd_mpp_id = :p_modelo and mpd_cant_cuotas = :p_cuotas
          AND mpd_fec_alta <= :p_fec_plan AND NVL(mpd_fec_baja,:p_fec_plan) >= :p_fec_plan';
    TYPE detalle_del_plan IS REF CURSOR;
    sumas_por_rangos detalle_del_plan;
    reg_sumas DETALLE_MODELO_PLAN%ROWTYPE;

   CURSOR c_obligaciones(P_REG_SUMAS DETALLE_MODELO_PLAN%ROWTYPE,
                         R_MODELO_PLAN         MODELOS_PLANES_PAGO%ROWTYPE)
       IS SELECT *
       	  FROM   OBLIGACIONES
          WHERE  OBL_CUENTA = P_CUENTA
            AND  OBL_ESTADO IN (11,15)
            AND  ((TRUNC(OBL_FEC_VENCIMIENTO) <= TRUNC(P_PLAN.PPE_FEC_ALTA) AND R_MODELO_PLAN.MPP_TIPO_PLAN_PAGO != 'R')
			      OR (TRUNC(OBL_FEC_GENERACION) <= TRUNC(P_PLAN.PPE_FEC_ALTA) AND R_MODELO_PLAN.MPP_TIPO_PLAN_PAGO = 'R')
				 )
            AND  OBL_SE = 'N'
            AND  OBL_PEF_ANIO >= 1995
            AND  OBL_PPL_ID IS NULL
            AND  OBL_SALDO > 0 AND obl_boleta_deuda IS NULL
            AND  OBL_TPC_CODIGO > 79
            AND  OBL_TPC_CODIGO <> 81
            AND  obl_id IN (SELECT rpo_obl_id FROM REL_PPE_OBL WHERE rpo_ppe_id = p_plan.ppe_id)
            AND (
			      (
			       TRUNC(OBL_FEC_VENCIMIENTO)
				         BETWEEN P_REG_SUMAS.MPD_FECHA_DEUDA_DESDE
                               AND
								 DECODE( P_REG_SUMAS.MPD_FECHA_HASTA,
								            'FGP',P_PLAN.PPE_FEC_ALTA,--SYSDATE,
			  	  						     TO_DATE(
										 		     NVL(
												 	     P_REG_SUMAS.MPD_FECHA_HASTA,
														 TO_CHAR(OBL_FEC_VENCIMIENTO,'DD/MM/RRRR')
													    ),
												     'DD/MM/RRRR'
										            )
									   )
				  )
			     OR
				  (R_MODELO_PLAN.MPP_TIPO_PLAN_PAGO = 'R')
				) AND
					 (
					    (
						   (
						     TRUNC(OBL_FEC_VENCIMIENTO)
							       BETWEEN NVL(R_MODELO_PLAN.MPP_DEUDA_FEC_DESDE,TRUNC(OBL_FEC_VENCIMIENTO))
								         AND
									       NVL(
									           DECODE(R_MODELO_PLAN.MPP_FECHA_HASTA,
												      'FGP',P_PLAN.PPE_FEC_ALTA,--SYSDATE,
												       TO_DATE(
														       NVL(
															       R_MODELO_PLAN.MPP_FECHA_HASTA,
															       TO_CHAR(OBL_FEC_VENCIMIENTO,'DD/MM/RRRR')
														          ),
														       'DD/MM/RRRR'
													          )
  							  			             ),
										       TRUNC(OBL_FEC_VENCIMIENTO)
										      )
						   )
						 OR
						   (
							 R_MODELO_PLAN.MPP_INCLUYE_RESTO = 'S'
						   )
						)
					   OR
						(R_MODELO_PLAN.MPP_TIPO_PLAN_PAGO = 'R')
					 );

   CURSOR c_servicio(P_OBL_ID OBLIGACIONES.OBL_ID%TYPE)
          IS SELECT CCT_SER_CODIGO,
		            SUM(NVL(CCT_IMP_DEBE,0)) - SUM(NVL(DECODE(APA_ESTADO,30,APA_IMP_HABER,31,APA_IMP_HABER,0),0)) CCT_IMP_DEBE
 		     FROM   CUENTAS_CORRIENTES, ASIG_PAGOS
             WHERE  cct_obl_id = P_OBL_ID
               AND  CCT_IMP_haber = 0
               AND  CCT_ESTADO = 15
               AND  apa_cct_obl_id (+) = cct_obl_id
               AND  apa_cct_id_movimiento (+) = cct_id_movimiento
             GROUP BY CCT_SER_CODIGO;

   r_planes_pago         PLANES_PAGO%ROWTYPE;
   r_update_obligaciones PKG_PLANES_PAGO.UPDATE_OBLIGACIONES;
   t_update_obligaciones PKG_PLANES_PAGO.UpdateObligacionesTyp;
   l_indice 			 NUMBER(5) := 0;
   l_inserta_recauda 	 PKG_RECAUDACIONES.VIncRecaudacionRec := p_inserta_recauda;
   l_recargo			 PLANES_PAGO.ppl_monto_recargos%TYPE := 0;
   l_importe 			 PLANES_PAGO.ppl_deuda_historica%TYPE := 0;
   l_fecha_plan 		 DATE := TRUNC(SYSDATE);
   nuevo_nro_plan 		 PLANES_PAGO.ppl_id%TYPE;
   V_TEMP_ID 			 DTE_CUENTAS_CORRIENTES.TEM_CCT_ID%TYPE := 0;
   V_EXISTE_SERVICIO 	 NUMBER(1) := 0;
   V_DATOS_IVA     		 PKG_SERVICIOS_FIJOS.ivarec;
   L_IMPORTE_IVA   		 PKG_SERVICIOS_FIJOS.importerec;
   V_TIPO_RESPONSABLE 	 INMUEBLES.INM_TIPO_RESPONSABLE%TYPE;
   V_SER_CALCULA_IVA 	 SERVICIOS.SER_CALCULA_IVA%TYPE;
   V_RECARGO   	   		 NUMBER(12,2) := 0;
   R_MODELO_PLAN         MODELOS_PLANES_PAGO%ROWTYPE;
   V_FEC_ULT_MODIF       DATE;

   v_fec_ini_vto DATE;
   l_quita_actual NUMBER(12,2) := 0; -- Utilizado para determinar el acumulado de la quita.

 BEGIN

   /* Completa los datos de las obligaciones viejas incluidas en el plan de pago */
   SELECT ppl_seq.NEXTVAL INTO nuevo_nro_plan FROM dual;
   BEGIN
     UPDATE PLANES_ESPECIALES SET ppe_ppl_id = nuevo_nro_plan
	 WHERE ppe_id = p_plan.ppe_id;
   END;

   /* ************************************/
   /* BUSCA EL ULTIMO ID DE LA TEMPORAL */
   /*************************************/
   SELECT NVL(MAX(TEM_CCT_ID),0) + 1
   INTO   V_TEMP_ID
   FROM   DTE_CUENTAS_CORRIENTES;

   SELECT INM_TIPO_RESPONSABLE
   INTO   V_TIPO_RESPONSABLE
   FROM   INMUEBLES
   WHERE  INM_CUENTA = P_CUENTA;

   V_DATOS_IVA := PKG_SERVICIOS_FIJOS.F_IMP_IVA(V_TIPO_RESPONSABLE,640106,SYSDATE);

   -- PEDIDO 2642490 - SROMERO Determina si puede tomar la parametrización actual o debe pasar por el histórico --
   BEGIN
      SELECT mpp_fec_mod INTO v_fec_ult_modif
      FROM MODELOS_PLANES_PAGO
	 WHERE MPP_ID = P_PLAN.PPE_MPP_ID;
     EXCEPTION WHEN OTHERS THEN v_fec_ult_modif := NULL;
   END;

   IF NVL(v_fec_ult_modif,TO_DATE('01011995','ddmmrrrr')) < P_PLAN.PPE_fec_alta THEN
       BEGIN -- modelo -
         SELECT *
         INTO R_MODELO_PLAN
         FROM MODELOS_PLANES_PAGO
         WHERE MPP_ID = P_PLAN.PPE_MPP_ID;
       END;
   ELSE
       BEGIN -- modelo -
         SELECT HMP_MPP_ID, HMP_MPP_MIN_CNT_CUOTAS, HMP_MPP_FEC_INICIO_VIGENCIA, HMP_MPP_IMP_MIN_DEUDA,
                HMP_MPP_CNT_DIAS_CADUCIDAD, HMP_MPP_TIPO_PLAN_PAGO, HMP_MPP_MODALIDAD, HMP_MPP_USR_ALTA,
                HMP_MPP_FEC_ALTA, HMP_MPP_TCL_TIPO, HMP_MPP_MAX_CNT_CUOTAS, HMP_MPP_MIN_IMP_CUOTA,
                HMP_MPP_MAX_BONIF_RECARGO, HMP_MPP_MAX_BONIF_INTERES, HMP_MPP_TASA_INTERES, HMP_MPP_TASA_RECARGO,
                HMP_MPP_FEC_FIN_VIGENCIA, HMP_MPP_USR_BAJA, HMP_MPP_FEC_BAJA, HMP_MPP_USR_MOD, HMP_MPP_FEC_MOD,
                HMP_MPP_QUITA, HMP_MPP_MONTO_INICIAL, HMP_MPP_IMP_MAX_DEUDA, HMP_MPP_CON_PLAN_ACTIVO,
                HMP_MPP_DEUDA_FEC_DESDE, HMP_MPP_DEUDA_FEC_HASTA, HMP_MPP_INCLUYE_RESTO, HMP_MPP_CON_PAGO_NO_ING,
                HMP_MPP_CON_PEND_APLIC, HMP_MPP_EN_CONC_Y_QUIEBRA, HMP_MPP_EN_GEST_JUDICIAL, HMP_MPP_TERCERIZADOS,
                HMP_MPP_FEC_LIMITE_VIG, HMP_MPP_GRANDES_CLIENTES, HMP_MPP_VIGENCIA, HMP_MPP_FECHA_HASTA, HMP_MPP_CUOTA_QUITA,
                HMP_MPP_MAO_CODIGO, HMP_MPP_DESCRIPCION
         INTO R_MODELO_PLAN
         FROM HIS_MODELOS_PLANES_PAGO
         WHERE HMP_MPP_ID = P_PLAN.PPE_MPP_ID
             AND HMP_ID = (SELECT MIN(B.HMP_ID) FROM HIS_MODELOS_PLANES_PAGO B
                                 WHERE B.HMP_MPP_ID = P_PLAN.PPE_MPP_ID AND B.HMP_FEC_ALTA >= P_PLAN.PPE_fec_alta);
         EXCEPTION WHEN NO_DATA_FOUND THEN   -- casos con cambios pero todavía sin histórico --
           BEGIN -- modelo -
             SELECT *
             INTO R_MODELO_PLAN
             FROM MODELOS_PLANES_PAGO
             WHERE MPP_ID = P_PLAN.PPE_MPP_ID;
           END;
       END;
   END IF;

-- PEDIDO 2642490 - SROMERO Determina si puede tomar la parametrización actual o debe pasar por el histórico --
    BEGIN
       SELECT MAX(mpd_fec_mod) INTO v_fec_ult_modif
       FROM DETALLE_MODELO_PLAN
	  WHERE mpd_mpp_id = P_PLAN.PPE_MPP_ID
        AND mpd_cant_cuotas = P_PLAN.PPE_CNT_CUOTAS
        AND mpd_fec_alta <= P_PLAN.PPE_FEC_ALTA
        AND NVL(mpd_fec_baja,P_PLAN.PPE_FEC_ALTA)  >= P_PLAN.PPE_FEC_ALTA;
        EXCEPTION WHEN NO_DATA_FOUND THEN v_fec_ult_modif := NULL;
    END;
    -- PEDIDO 2642490 - SROMERO si debe pasar por el histórico cambia el cursor dinámico --
    IF NVL(v_fec_ult_modif,TO_DATE('01011995','ddmmrrrr')) > P_PLAN.PPE_fec_alta THEN
            spr_actual := REPLACE(spr_actual,'mpd','hdm_mpd');
            spr_actual := REPLACE(spr_actual,'detalle','his_detalle');
            spr_actual := REPLACE(spr_actual,'*','HDM_MPD_MPP_ID, HDM_MPD_CANT_CUOTAS, HDM_MPD_FECHA_DEUDA_DESDE, HDM_MPD_FECHA_DEUDA_HASTA, HDM_MPD_QUITA, HDM_MPD_TIPO_QUITA, HDM_MPD_BONIF_RECARGOS, HDM_MPD_BONIF_INTERESES, HDM_MPD_IMP_MINIMO_CUOTA, HDM_MPD_USR_ALTA, HDM_MPD_FEC_ALTA, HDM_MPD_USR_BAJA, HDM_MPD_FEC_BAJA, HDM_MPD_USR_MOD, HDM_MPD_FEC_MOD, HDM_MPD_FECHA_HASTA');
    END IF;


   R_PLANES_PAGO.PPL_BONIF_RECARGO := 0;
   /* ABRE CURSOS RANGOS */
   OPEN sumas_por_rangos FOR spr_actual USING P_PLAN.PPE_MPP_ID, P_PLAN.PPE_CNT_CUOTAS, P_PLAN.PPE_FEC_ALTA, P_PLAN.PPE_FEC_ALTA, P_PLAN.PPE_FEC_ALTA;
    LOOP
	      FETCH sumas_por_rangos INTO reg_sumas;
		     EXIT WHEN sumas_por_rangos%NOTFOUND;


--   FOR REG_SUMAS IN SUMAS_POR_RANGOS(P_PLAN.PPE_CNT_CUOTAS, P_PLAN.PPE_MPP_ID) LOOP -- POR CADA RANGO DE FECHAS -

      IF REG_SUMAS.MPD_BONIF_RECARGOS > R_PLANES_PAGO.PPL_BONIF_RECARGO THEN
         R_PLANES_PAGO.PPL_BONIF_RECARGO := REG_SUMAS.MPD_BONIF_RECARGOS;
	  END IF;


      /* ABRE CURSOS OBLIGACIONES */
      FOR r_obligacion IN c_obligaciones(REG_SUMAS, R_MODELO_PLAN) LOOP
         r_update_obligaciones.v_obl_id := r_obligacion.obl_id;
         r_update_obligaciones.v_obl_ppl_id := nuevo_nro_plan;
         r_update_obligaciones.v_obl_tipo_plan := p_plan.ppe_mpp_id;
         r_update_obligaciones.v_obl_imp_recargo := 0;
         r_update_obligaciones.v_obl_usr_mod := p_usuario;
		 r_update_obligaciones.v_incluida := 'S';

         L_INDICE := L_INDICE + 1;
		 T_UPDATE_OBLIGACIONES(L_INDICE) := R_UPDATE_OBLIGACIONES;

		/* ABRE CURSOS SERVICIOS */
         FOR R_SERVICIOS IN C_SERVICIO(R_OBLIGACION.OBL_ID) LOOP

            L_IMPORTE_IVA.IVA        := 0;
      		L_IMPORTE_IVA.ALICUOTA   := 0;
      		L_IMPORTE_IVA.PERCEPCION := 0;
            V_RECARGO                := 0;
			V_SER_CALCULA_IVA        := NULL;
			V_EXISTE_SERVICIO        := 0;

            SELECT SER_CALCULA_IVA
 		    INTO   V_SER_CALCULA_IVA
		    FROM   SERVICIOS
		    WHERE  SER_CODIGO = R_SERVICIOS.CCT_SER_CODIGO;

            BEGIN

        	   SELECT 1
        	   INTO   V_EXISTE_SERVICIO
        	   FROM   DTE_CUENTAS_CORRIENTES
        	   WHERE  TEM_CCT_ID = V_TEMP_ID
        	     AND  TEM_CCT_SERVICIO = R_SERVICIOS.CCT_SER_CODIGO;

            EXCEPTION WHEN NO_DATA_FOUND THEN

                     V_RECARGO := ROUND((PKG_RECAUDACIONES.CALCULO_RECARGOS(r_obligacion.obl_fec_vencimiento,p_fecha_vto,NVL(R_SERVICIOS.CCT_IMP_DEBE,0)) * ((100 - reg_sumas.mpd_bonif_recargos) / 100)),2);

                     IF V_SER_CALCULA_IVA = 'S' THEN

                        L_IMPORTE_IVA.IVA        := (V_DATOS_IVA.IVA        * V_RECARGO)/100;
      				    L_IMPORTE_IVA.ALICUOTA   := (V_DATOS_IVA.ALICUOTA   * V_RECARGO)/100;
      				    L_IMPORTE_IVA.PERCEPCION := (V_DATOS_IVA.PERCEPCION * (V_RECARGO + L_IMPORTE_IVA.IVA))/100;

          	            INSERT INTO DTE_CUENTAS_CORRIENTES
           	                       (TEM_CCT_ID,
           	                        TEM_CALCULA,
           	                        TEM_CCT_SERVICIO,
           	                        TEM_CCT_OBL_ID,
           	                        TEM_CCT_MONTO)
           	                VALUES (V_TEMP_ID,
           	                        'S',
           	                        R_SERVICIOS.CCT_SER_CODIGO,
           	                        r_obligacion.obl_id,
           	                        V_RECARGO + NVL(L_IMPORTE_IVA.IVA,0) + NVL(L_IMPORTE_IVA.ALICUOTA,0) + NVL(L_IMPORTE_IVA.PERCEPCION,0));
					 ELSE

          	            INSERT INTO DTE_CUENTAS_CORRIENTES
           	                       (TEM_CCT_ID,
           	                        TEM_CALCULA,
           	                        TEM_CCT_SERVICIO,
           	                        TEM_CCT_OBL_ID,
           	                        TEM_CCT_MONTO)
           	                VALUES (V_TEMP_ID,
           	                        'S',
           	                        R_SERVICIOS.CCT_SER_CODIGO,
           	                        r_obligacion.obl_id,
           	                        V_RECARGO);
				     END IF;
            END;
            IF V_EXISTE_SERVICIO = 1 THEN

		 	   V_RECARGO := ROUND((PKG_RECAUDACIONES.CALCULO_RECARGOS(r_obligacion.obl_fec_vencimiento,p_fecha_vto,NVL(R_SERVICIOS.CCT_IMP_DEBE,0)) * ((100 - reg_sumas.mpd_bonif_recargos) / 100)),2);

               IF V_SER_CALCULA_IVA = 'S' THEN

                  L_IMPORTE_IVA.IVA        := (V_DATOS_IVA.IVA        * V_RECARGO)/100;
       		 	  L_IMPORTE_IVA.ALICUOTA   := (V_DATOS_IVA.ALICUOTA   * V_RECARGO)/100;
      			  L_IMPORTE_IVA.PERCEPCION := (V_DATOS_IVA.PERCEPCION * (V_RECARGO + L_IMPORTE_IVA.IVA))/100;

           	      UPDATE DTE_CUENTAS_CORRIENTES
                     SET tem_cct_monto    = tem_cct_monto + V_RECARGO + NVL(L_IMPORTE_IVA.IVA,0) + NVL(L_IMPORTE_IVA.ALICUOTA,0) + NVL(L_IMPORTE_IVA.PERCEPCION,0)
                   WHERE TEM_CCT_ID       = V_TEMP_ID
        	         AND TEM_CCT_SERVICIO = R_SERVICIOS.CCT_SER_CODIGO;

			   ELSE

           	      UPDATE DTE_CUENTAS_CORRIENTES
                     SET tem_cct_monto    = tem_cct_monto + V_RECARGO
                   WHERE TEM_CCT_ID       = V_TEMP_ID
        	         AND TEM_CCT_SERVICIO = R_SERVICIOS.CCT_SER_CODIGO;

			   END IF;

            END IF;

         END LOOP; /* CIERRA CURSOS SERVICIOS */

      END LOOP; /* CIERRA CURSOS OBLIGACIONES */

    END LOOP;
    CLOSE sumas_por_rangos;
--   END LOOP; /* CIERRA CURSOS RANGOS */


   /* Recalcula el monto de la quita */
   -- SI EL PLAN ORIGINAL INCLUIA QUITAS DE CAPITAL, VERIFICA SI CAMBIARON LAS CONDICIONES
   l_quita_actual := p_plan.ppe_quita;

   -- dbms_output.put_line('l_quita_actual 1: '||l_quita_actual );

   IF p_plan.ppe_quita > 0 THEN

        /* Obtiene la quita actualizada */
        l_quita_actual := f_recalcula_quita(p_cuenta, P_PLAN.PPE_MPP_ID, p_plan.ppe_fec_alta);

   END IF;

   -- dbms_output.put_line('l_quita_actual 2: '||l_quita_actual );

   /* Verifica si la quita actual es mayor que la original */
   IF l_quita_actual > p_plan.ppe_quita THEN
         l_quita_actual := p_plan.ppe_quita;
   END IF;

      -- dbms_output.put_line('l_quita_actual 3: '||l_quita_actual );

   IF P_NUEVO_PLAN = 'S' THEN

      SELECT SUM(TEM_CCT_MONTO)
      INTO   L_RECARGO
      FROM   DTE_CUENTAS_CORRIENTES
	  WHERE  TEM_CCT_ID = V_TEMP_ID;
     /* SELECT SUM(pkg_recaudaciones.calculo_recargos(obl_fec_vencimiento,p_plan.ppe_fec_alta,tem_cct_monto))*1.21
      INTO   L_RECARGO
      FROM   DTE_CUENTAS_CORRIENTES, obligaciones
      where obl_id = tem_cct_obl_id;*/
   END IF;
--insert into pba values ('tmp '||L_RECARGO);
   /* Completa el registro del nuevo plan de pago */
   R_PLANES_PAGO.PPL_ID                  := nuevo_nro_plan;
   R_PLANES_PAGO.PPL_MPP_ID              := p_plan.ppe_mpp_id;
   R_PLANES_PAGO.PPL_INM_ID              := p_plan.ppe_inm_id;
   R_PLANES_PAGO.PPL_CLI_ID              := p_plan.ppe_cli_id;
   R_PLANES_PAGO.PPL_FECHA               := p_inserta_recauda.V_FECHA_COBRO;
   R_PLANES_PAGO.PPL_PRIMER_VTO          := p_inserta_recauda.V_FECHA_COBRO;
   R_PLANES_PAGO.PPL_QUITA               := l_quita_actual;

   -- dbms_output.put_line('R_PLANES_PAGO.PPL_QUITA: '||R_PLANES_PAGO.PPL_QUITA);

   R_PLANES_PAGO.PPL_TASA_INTERES        := r_modelo_plan.MPP_TASA_INTERES;--p_plan.ppe_tasa_interes;
   R_PLANES_PAGO.PPL_TASA_RECARGOS       := p_plan.ppe_tasa_recargos;
   R_PLANES_PAGO.PPL_TASA_BONIFICACIONES := p_plan.ppe_tasa_bonificaciones;
   R_PLANES_PAGO.PPL_IMP_INTERESES       := p_plan.ppe_imp_intereses;
   R_PLANES_PAGO.PPL_ESTADO              := p_plan.ppe_estado;
   R_PLANES_PAGO.PPL_FEC_CADUCIDAD       := p_plan.ppe_fec_caducidad;
   R_PLANES_PAGO.PPL_MODALIDAD           := p_plan.ppe_modalidad;
   R_PLANES_PAGO.PPL_USR_ALTA            := p_usuario;
   R_PLANES_PAGO.PPL_FEC_ALTA            := SYSDATE;
   R_PLANES_PAGO.PPL_USR_BAJA            := NULL;
   R_PLANES_PAGO.PPL_FEC_BAJA            := NULL;
   R_PLANES_PAGO.PPL_USR_MOD             := NULL;
   R_PLANES_PAGO.PPL_FEC_MOD             := NULL;
   R_PLANES_PAGO.PPL_BONIF_RECARGO       := NVL(p_plan.ppe_bonif_recargo,0);
   R_PLANES_PAGO.PPL_VTO_PAGO_INICIAL    := p_plan.ppe_vto_pago_inicial;
   R_PLANES_PAGO.PPL_INT_PAGO_INICIAL    := p_plan.ppe_int_pago_inicial;

   IF p_nuevo_plan = 'N' THEN

      R_PLANES_PAGO.PPL_IMP_CUOTA          := p_plan.ppe_imp_cuota;
      R_PLANES_PAGO.PPL_MONTO_RECARGOS     := p_plan.ppe_monto_recargos;
      R_PLANES_PAGO.PPL_DEUDA_HISTORICA    := p_plan.ppe_deuda_historica;
      R_PLANES_PAGO.PPL_CNT_CUOTAS         := p_plan.ppe_cnt_cuotas;
      R_PLANES_PAGO.PPL_MONTO_PAGO_INICIAL := p_plan.ppe_monto_pago_inicial;

      /* Llama al procedimiento que genera el plan de pago */

      l_inserta_recauda := F_Genera_Ideal(r_planes_pago, t_update_obligaciones, p_cuenta, p_plan.ppe_id, p_tipo_responsable, l_inserta_recauda,V_TEMP_ID);
      l_inserta_recauda.V_IMPORTE := r_planes_pago.PPL_IMP_CUOTA;

   ELSE  /* Deuda historica distinta a la del plan especial */

      R_PLANES_PAGO.PPL_IMP_CUOTA          := 0;
      R_PLANES_PAGO.PPL_MONTO_RECARGOS     := l_recargo;
      R_PLANES_PAGO.PPL_DEUDA_HISTORICA    := l_inserta_recauda.v_importe; /* Nueva deuda hitorica */
      R_PLANES_PAGO.PPL_CNT_CUOTAS         := p_plan.ppe_cnt_cuotas - 1;
--insert into pba values ('paso '||R_PLANES_PAGO.PPL_MONTO_RECARGOS );
	  /* Si el importe de la deuda es menor que la deuda histórica */
	  IF p_plan.ppe_imp_cuota <= l_inserta_recauda.v_importe THEN
         R_PLANES_PAGO.PPL_MONTO_PAGO_INICIAL := p_plan.ppe_imp_cuota;
	  ELSE
         R_PLANES_PAGO.PPL_MONTO_PAGO_INICIAL := l_inserta_recauda.v_importe;
	  END IF;
      r_planes_pago.PPL_MONTO_RECARGOS := NVL(r_planes_pago.PPL_MONTO_RECARGOS ,0);
      /* Llama al procedimiento que genera el NUEVO plan de pago */
      l_inserta_recauda := F_CONFECCIONA_PLAN_PAGO (r_planes_pago,t_update_obligaciones,p_cuenta, p_tipo_responsable, l_inserta_recauda, V_TEMP_ID);

   END IF;

   l_inserta_recauda.V_FACTURA := r_planes_pago.PPL_ID;

   RETURN l_inserta_recauda;

 END;


 /* ****************************************************************** */
 /* Recupera el domicilio Postal del inmueble y el tipo de responsable */
 /* ****************************************************************** */
 FUNCTION F_Recupera_Domicilio(P_INM_ID INMUEBLES.INM_ID%TYPE)
                               RETURN NUMBER IS
    V_DOM_ID DOMICILIOS.DOM_ID%TYPE;
 BEGIN

    SELECT DOMICILIOS.DOM_ID
   	  INTO V_DOM_ID
   	  FROM DOMICILIOS , INMUEBLES
     WHERE INMUEBLES.INM_ID = DOMICILIOS.DOM_INM_ID
       AND INMUEBLES.INM_ID = P_INM_ID
	   AND DOMICILIOS.DOM_POSTAL = 'S'
	   AND DOMICILIOS.DOM_FEC_BAJA IS NULL;

      RETURN V_DOM_ID;

 EXCEPTION
    WHEN NO_DATA_FOUND THEN
       V_DOM_ID := 3;
       RETURN V_DOM_ID;
 END;


/* ************************************************************* */
/* Recupera el grupo de la sucursal                              */
/* ************************************************************* */
FUNCTION F_Recupera_Grupo(P_SUCURSAL SUCURSALES.SUC_CODIGO%TYPE)
                          RETURN NUMBER IS

   V_GRP_CODIGO SUCURSALES.SUC_GRP_CODIGO%TYPE;

BEGIN

   SELECT SUCURSALES.SUC_GRP_CODIGO
     INTO V_GRP_CODIGO
     FROM SUCURSALES
    WHERE SUCURSALES.SUC_CODIGO = P_SUCURSAL
      AND SUCURSALES.SUC_FEC_BAJA IS NULL;

   RETURN v_grp_codigo;

EXCEPTION
   WHEN NO_DATA_FOUND THEN
      RETURN NULL;
END;

/* *************************************************************************************/
/*  Genera Plan de Pago, bajo condiciones ideales                                      */
/* *************************************************************************************/
 FUNCTION F_Genera_Ideal(P_CAB_PLANES          PLANES_PAGO%ROWTYPE,
                         P_UPDATE_OBLIGACIONES PKG_PLANES_PAGO.UpdateObligacionesTyp,
	                     P_CUENTA              INMUEBLES.INM_CUENTA%TYPE,
			     		 P_ID_PLAN_ESP         PLANES_ESPECIALES.PPE_ID%TYPE,
						 P_TIPO_RESPONSABLE    INMUEBLES.INM_TIPO_RESPONSABLE%TYPE,
                         P_Recauda             PKG_RECAUDACIONES.VIncRecaudacionRec,
						 P_TEM_CCT_ID          DTE_CUENTAS_CORRIENTES.TEM_CCT_ID%TYPE)
						 RETURN PKG_RECAUDACIONES.VIncRecaudacionRec IS

  R_OBLIGACIONES       OBLIGACIONES%ROWTYPE;
  R_CUENTAS_CORRIENTES CUENTAS_CORRIENTES%ROWTYPE;
  V_OBL_ID             OBLIGACIONES.OBL_ID%TYPE;
  V_INDICE             NUMBER(5) := 0;
  V_INC_CCT_MOVIMIENTO CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO%TYPE := 0;
  V_CONCEPTO           VARCHAR2(30);
  V_GRUPO              SUCURSALES.SUC_GRP_CODIGO%TYPE := 0;
  V_OBLIGACION         PKG_PLANES_PAGO.ObligacionInsercionTyp;
  V_CUENTA_CORRIENTE   PKG_PLANES_PAGO.CuentasCorrieneTyp;
  L_IMPORTE            PKG_SERVICIOS_FIJOS.importerec;
  V_DATOS_IVA      	   PKG_SERVICIOS_FIJOS.ivarec;
  V_IMP_NUEVO_IVA      NUMBER(16,2);
  V_RETORNA 		   VARCHAR2(300);
  R_RETORNO            PKG_RECAUDACIONES.VINCRECAUDACIONREC := P_RECAUDA;
  V_REC_BON_IVA        NUMBER(15,2) := 0;
  V_REC_BON_SIN        NUMBER(15,2) := 0;
  V_SERVICIO           SERVICIOS.SER_CODIGO%TYPE;
  V_SERVICIO_INSERT    SERVICIOS.SER_CODIGO%TYPE;
  V_MONTO_SER_INSERT   PLANES_ESPECIALES_CUOTAS.PPC_RECARGOS%TYPE := 0;

  CURSOR C_PLAN_ESP(P_PPC_PPE_ID OBLIGACIONES.OBL_ID%TYPE)IS
				    SELECT PLANES_ESPECIALES_CUOTAS.PPC_PPE_ID,
                           PLANES_ESPECIALES_CUOTAS.PPC_NRO_CUOTA,
						   PLANES_ESPECIALES_CUOTAS.PPC_FEC_CUOTA,
 						   PLANES_ESPECIALES_CUOTAS.PPC_PEF_ANIO,
 						   PLANES_ESPECIALES_CUOTAS.PPC_PEF_PERIODO,
						   PLANES_ESPECIALES_CUOTAS.PPC_IMP_CUOTA,
						   PLANES_ESPECIALES_CUOTAS.PPC_CAPITAL,
						   PLANES_ESPECIALES_CUOTAS.PPC_RECARGOS,
						   PLANES_ESPECIALES_CUOTAS.PPC_INTERESES
                      FROM PLANES_ESPECIALES_CUOTAS
				   	 WHERE PLANES_ESPECIALES_CUOTAS.PPC_PPE_ID = P_PPC_PPE_ID ;
  R_PLAN_ESP C_PLAN_ESP%ROWTYPE;

CURSOR c_servicio(P_TEM_CCT_ID DTE_CUENTAS_CORRIENTES.TEM_CCT_ID%TYPE)
                    IS SELECT NVL(SUM(TEM_CCT_MONTO),0) TEM_CCT_MONTO
                       FROM   DTE_CUENTAS_CORRIENTES, SERVICIOS
                       WHERE  TEM_CCT_SERVICIO = SER_CODIGO
                         AND  SER_CALCULA_IVA = 'S'
                         AND  TEM_CALCULA = 'S'
                         AND  TEM_CCT_ID = P_TEM_CCT_ID
					   ORDER BY TEM_CCT_SERVICIO;

  CURSOR c_ser_sin(P_TEM_CCT_ID DTE_CUENTAS_CORRIENTES.TEM_CCT_ID%TYPE)
                    IS SELECT TEM_CCT_SERVICIO, NVL(SUM(TEM_CCT_MONTO),0) TEM_CCT_MONTO
                       FROM   DTE_CUENTAS_CORRIENTES, SERVICIOS
                       WHERE  TEM_CCT_SERVICIO = SER_CODIGO
                         AND  SER_CALCULA_IVA = 'N'
                         AND  TEM_CALCULA = 'S'
                         AND  TEM_CCT_ID = P_TEM_CCT_ID
						 AND  TEM_CCT_MONTO > 0
                       GROUP BY TEM_CCT_SERVICIO;
   R_SER_SIN C_SER_SIN%ROWTYPE;

   V_PRUEBA VARCHAR2(100);

BEGIN

   V_GRUPO := F_RECUPERA_GRUPO(SUBSTR(P_CUENTA,1,3));
   /* DATOS FIJOS OBLIGACIONES */
   r_obligaciones.OBL_CLI_ID            := P_CAB_PLANES.PPL_cli_id;
   r_obligaciones.OBL_LOT_COD_LOTE      := NULL;
   r_obligaciones.OBL_MAO_CODIGO        := NULL;
   r_obligaciones.OBL_CON_INM_ID        := P_CAB_PLANES.PPL_INM_ID;
   r_obligaciones.OBL_CON_ID            := NULL;
   r_obligaciones.OBL_FEC_GENERACION    := SYSDATE;
   r_obligaciones.OBL_ESTADO            := 15;
   r_obligaciones.OBL_NRO_FACTURA       := P_CAB_PLANES.PPL_ID;
   r_obligaciones.OBL_USR_ALTA          := P_CAB_PLANES.PPL_USR_ALTA;
   r_obligaciones.OBL_FEC_ALTA          := SYSDATE;
   r_obligaciones.OBL_OBL_ID            := NULL;
   r_obligaciones.OBL_PPL_ID            := P_CAB_PLANES.PPL_ID;
   r_obligaciones.OBL_INM_ID            := P_CAB_PLANES.PPL_INM_ID;
   r_obligaciones.OBL_COV_ID            := NULL;
   r_obligaciones.OBL_PMO_ID            := NULL;
   r_obligaciones.OBL_TIPO_PLAN         := 1;
   r_obligaciones.OBL_FEC_INI_ETAPA_MOR := NULL;
   r_obligaciones.OBL_FEC_FIN_ETAPA_MOR := NULL;
   r_obligaciones.OBL_EST_MOROSIDAD     := NULL;
   r_obligaciones.OBL_NRO_FORMULARIO    := NULL;
   r_obligaciones.OBL_FEC_PROCESO       := NULL;
   r_obligaciones.OBL_FEC_APLICACION    := NULL;
   r_obligaciones.OBL_BOLETA_DEUDA      := NULL;
   r_obligaciones.OBL_IMP_IVA_EXE       := 0;
   r_obligaciones.OBL_IMP_IVA_RI        := 0;
   r_obligaciones.OBL_IMP_IVA_RNI       := 0;
   r_obligaciones.OBL_IMP_IVA_MON       := 0;
   r_obligaciones.OBL_SALDO_MOROSIDAD   := NULL;
   r_obligaciones.OBL_IMP_RECARGO       := NULL;
   r_obligaciones.OBL_FEC_IMPRESION     := NULL;
   r_obligaciones.OBL_FEC_FACTURACION   := NULL;
   r_obligaciones.OBL_FEC_APROBACION    := NULL;
   r_obligaciones.OBL_FEC_DISTRIBUCION  := NULL;
   r_obligaciones.OBL_FEC_GENERACION_NC := NULL;
   r_obligaciones.OBL_USR_MOD           := NULL;
   r_obligaciones.OBL_FEC_MOD           := NULL;
   r_obligaciones.OBL_USR_BAJA          := NULL;
   r_obligaciones.OBL_FEC_BAJA          := NULL;
   r_obligaciones.OBL_SE                := 'N';
   r_obligaciones.OBL_TPC_CODIGO_NC     := NULL;
   r_obligaciones.OBL_OBL_ID_NC         := NULL;
   r_obligaciones.OBL_PPL_ID_CUOTA      := NULL;
   r_obligaciones.OBL_FEC_EST_MOROSO    := NULL;
   r_obligaciones.OBL_FEC_CONTABLE      := SYSDATE;
   r_obligaciones.OBL_NRO_COMPROB_NC    := NULL;
   r_obligaciones.OBL_ETA_COD_ETAPA     := NULL;
   r_obligaciones.OBL_EMO_COD_ETAPA     := NULL;
   r_obligaciones.OBL_POR_IVA_EXE       := 0;
   r_obligaciones.OBL_POR_IVA_RI        := 0;
   r_obligaciones.OBL_POR_IVA_RNI       := 0;
   r_obligaciones.OBL_POR_IVA_MON       := 0;
   r_obligaciones.OBL_IMP_IVA_CF        := 0;
   r_obligaciones.OBL_IMP_ALI_RNI       := 0;
   r_obligaciones.OBL_IMP_ALI_NO_CAT    := 0;
   r_obligaciones.OBL_POR_IVA_CF        := 0;
   r_obligaciones.OBL_ALI_RNI           := 0;
   r_obligaciones.OBL_ALI_NO_CAT        := 0;
   r_obligaciones.OBL_TPC_CODIGO        := 8;
   r_obligaciones.OBL_DOM_ID            := F_RECUPERA_DOMICILIO(P_CAB_PLANES.PPL_INM_ID);
   r_obligaciones.OBL_Cuenta            := P_CUENTA;
   r_obligaciones.OBL_SUC_CODIGO        := SUBSTR(P_CUENTA,1,3);
   r_obligaciones.OBL_GRP_CODIGO        := V_GRUPO;
   r_obligaciones.OBL_TRE_CODIGO        := P_TIPO_RESPONSABLE;

   /* DATOS FIJOS DE CUENTAS CORRIENTES **/
   R_CUENTAS_CORRIENTES.CCT_IMP_HABER           := 0;
   R_CUENTAS_CORRIENTES.CCT_FEC_GENERACION      := SYSDATE;
   R_CUENTAS_CORRIENTES.CCT_FEC_CIERRE_CONTABLE := SYSDATE;
   R_CUENTAS_CORRIENTES.CCT_USR_ALTA            := P_CAB_PLANES.PPL_USR_ALTA;
   R_CUENTAS_CORRIENTES.CCT_FEC_ALTA            := SYSDATE;
   R_CUENTAS_CORRIENTES.CCT_REC_ID              := NULL;
   R_CUENTAS_CORRIENTES.CCT_NOV_ID              := NULL;
   R_CUENTAS_CORRIENTES.CCT_CNT_DIAS_RECARGO    := 1;
   R_CUENTAS_CORRIENTES.CCT_BANCO               := NULL;
   R_CUENTAS_CORRIENTES.CCT_REMESA              := NULL;
   R_CUENTAS_CORRIENTES.CCT_SECUENCIA           := NULL;
   R_CUENTAS_CORRIENTES.CCT_FEC_APLICACION      := NULL;
   R_CUENTAS_CORRIENTES.CCT_FEC_PROCESO         := NULL;
   R_CUENTAS_CORRIENTES.CCT_FEC_COBRO           := NULL;
   R_CUENTAS_CORRIENTES.CCT_USR_BAJA            := NULL;
   R_CUENTAS_CORRIENTES.CCT_FEC_BAJA            := NULL;
   R_CUENTAS_CORRIENTES.CCT_USR_MOD             := NULL;
   R_CUENTAS_CORRIENTES.CCT_FEC_MOD             := NULL;
   R_CUENTAS_CORRIENTES.CCT_ESTADO              := 15;
   R_CUENTAS_CORRIENTES.CCT_SE                  := 'N';
   R_CUENTAS_CORRIENTES.CCT_OBL_ID_CUOTA        := NULL;
   R_CUENTAS_CORRIENTES.CCT_TIPO_MOVIMIENTO     := 8;
   R_CUENTAS_CORRIENTES.CCT_GRP_CODIGO          := V_GRUPO;
   R_CUENTAS_CORRIENTES.CCT_CUENTA              := P_CUENTA;
   R_CUENTAS_CORRIENTES.CCT_SUC_CODIGO          := SUBSTR(P_CUENTA,1,3);
   R_CUENTAS_CORRIENTES.CCT_TRE_CODIGO          := P_TIPO_RESPONSABLE;

   FOR R_PLAN_ESP IN C_PLAN_ESP(P_ID_PLAN_ESP) LOOP
      SELECT OBL_SEQ.NEXTVAL INTO V_OBL_ID FROM DUAL;
      BEGIN
	     IF NVL(R_PLAN_ESP.PPC_NRO_CUOTA,0) = 1 THEN
		    r_obligaciones.OBL_PEF_ANIO              := TO_NUMBER(TO_CHAR(r_retorno.V_FECHA_COBRO,'RRRR'));
			r_obligaciones.OBL_PEF_PERIODO     		 := TO_NUMBER(TO_CHAR(r_retorno.V_FECHA_COBRO,'MM'));
			r_obligaciones.OBL_FEC_VENCIMIENTO 		 := r_retorno.V_FECHA_COBRO;
            R_CUENTAS_CORRIENTES.CCT_FEC_VENCIMIENTO := r_retorno.V_FECHA_COBRO;
            r_retorno.v_anio                   		 := r_obligaciones.OBL_PEF_anio;
            r_retorno.v_periodo                		 := r_obligaciones.OBL_PEF_PERIODO;
		 ELSE
            r_obligaciones.OBL_PEF_ANIO              := TO_NUMBER(TO_CHAR(r_retorno.V_FECHA_COBRO + ((NVL(R_PLAN_ESP.PPC_NRO_CUOTA,0) - 1) * 30),'RRRR'));
			r_obligaciones.OBL_PEF_PERIODO   		 := TO_NUMBER(TO_CHAR(r_retorno.V_FECHA_COBRO + ((NVL(R_PLAN_ESP.PPC_NRO_CUOTA,0) - 1) * 30),'MM'));
			r_obligaciones.OBL_FEC_VENCIMIENTO 		 := (r_retorno.V_FECHA_COBRO + ((NVL(R_PLAN_ESP.PPC_NRO_CUOTA,0) - 1) * 30));
            R_CUENTAS_CORRIENTES.CCT_FEC_VENCIMIENTO := (r_retorno.V_FECHA_COBRO + ((NVL(R_PLAN_ESP.PPC_NRO_CUOTA,0) - 1) * 30));
		 END IF;

         R_OBLIGACIONES.OBL_ID              := V_OBL_ID;
         r_obligaciones.OBL_IMP_ORIGINAL    := NVL(R_PLAN_ESP.PPC_IMP_CUOTA,0);
         r_obligaciones.OBL_SALDO           := NVL(R_PLAN_ESP.PPC_IMP_CUOTA,0);
         r_obligaciones.OBL_IMP_NETO        := NVL(R_PLAN_ESP.PPC_IMP_CUOTA,0);
         r_obligaciones.OBL_CUOTA_PLAN      := NVL(R_PLAN_ESP.PPC_NRO_CUOTA,0);
         V_INDICE := V_INDICE + 1;
         V_OBLIGACION(V_INDICE) := R_OBLIGACIONES;
         /* LLENA LA VARIABLE UTILIZADA PARA INSERTAR EN CUENTAS CORRIENTES CON DATOS
            QUE SOLAMENTE VARIAN POR CADA CUOTA Y NO POR CADA INSERCION */
         R_CUENTAS_CORRIENTES.CCT_OBL_ID          := V_OBL_ID;
         R_CUENTAS_CORRIENTES.CCT_PEF_ANIO        := r_obligaciones.OBL_PEF_ANIO;
  		 R_CUENTAS_CORRIENTES.CCT_PEF_PERIODO     := r_obligaciones.OBL_PEF_PERIODO;

         /* RECUPERA DATOS PARA CALCULO DE IVA */
		 v_datos_iva := PKG_SERVICIOS_FIJOS.F_IMP_IVA(P_TIPO_RESPONSABLE,
			                                          640108,
												      SYSDATE);

         IF NVL(R_PLAN_ESP.PPC_RECARGOS,0) > 0 THEN

		    FOR R_SERVICIO IN C_SERVICIO(P_TEM_CCT_ID) LOOP
               V_REC_BON_IVA := R_SERVICIO.TEM_CCT_MONTO;
            END LOOP;

			IF NVL(R_PLAN_ESP.PPC_RECARGOS,0) < NVL(V_REC_BON_IVA,0) THEN /* ABRE ASIGNA RECARGOS */

		       /* ACTUALIZA LA TEMPORAL POR IMPORTE DEL RECARGO */

			   ACTUALIZA_TEMPORAL_CON_IVA(NVL(R_PLAN_ESP.PPC_RECARGOS,0),P_TEM_CCT_ID,V_PRUEBA);

		 	   L_IMPORTE := F_IVA_INVERSO(NVL(R_PLAN_ESP.PPC_RECARGOS,0),
			                             V_DATOS_IVA.IVA,
					   	 			     V_DATOS_IVA.ALICUOTA,
						 			     V_DATOS_IVA.PERCEPCION);
               V_IMP_NUEVO_IVA := (NVL(R_PLAN_ESP.PPC_RECARGOS,0)) - (L_IMPORTE.IVA + L_IMPORTE.ALICUOTA + L_IMPORTE.PERCEPCION);
			   IF V_DATOS_IVA.IVA = 0 THEN
			      R_CUENTAS_CORRIENTES.CCT_IMP_IVA := 0;
     		      R_CUENTAS_CORRIENTES.CCT_POR_IVA := 0;
			   ELSE
   		          R_CUENTAS_CORRIENTES.CCT_POR_IVA := V_DATOS_IVA.IVA;
   		 	      R_CUENTAS_CORRIENTES.CCT_IMP_IVA := NVL(L_IMPORTE.IVA,0);
			   END IF;
			   IF V_DATOS_IVA.ALICUOTA = 0 AND V_DATOS_IVA.PERCEPCION = 0 THEN
   		 	      R_CUENTAS_CORRIENTES.CCT_IMP_ALI := 0;
   		          R_CUENTAS_CORRIENTES.CCT_POR_ALI := 0;
			   ELSE
   		 	      R_CUENTAS_CORRIENTES.CCT_IMP_ALI := L_IMPORTE.ALICUOTA + L_IMPORTE.PERCEPCION;
			      R_CUENTAS_CORRIENTES.CCT_POR_ALI := V_DATOS_IVA.ALICUOTA + V_DATOS_IVA.PERCEPCION;
               END IF;

		       /* TERMINA DE LLENAR LA VARIABLE UTILIZADA EN LA INSERCION DE CUENTAS CORRIENTES */
		       V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO + 1;
               V_CONCEPTO := 'Recargo';
               R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO   := V_INC_CCT_MOVIMIENTO;
               R_CUENTAS_CORRIENTES.CCT_SER_CODIGO      := 640108;
               R_CUENTAS_CORRIENTES.CCT_IMP_DEBE        := NVL(R_PLAN_ESP.PPC_RECARGOS,0);
               R_CUENTAS_CORRIENTES.CCT_CONCEPTO        := NVL(V_CONCEPTO,'0');

			   IF NVL(R_PLAN_ESP.PPC_RECARGOS,0) > 0 THEN
			      V_CUENTA_CORRIENTE(V_INC_CCT_MOVIMIENTO) := R_CUENTAS_CORRIENTES;
			   END IF;

			ELSIF NVL(V_REC_BON_IVA,0) > 0 AND NVL(R_PLAN_ESP.PPC_RECARGOS,0) > V_REC_BON_IVA THEN /* SEGUNDA CONDICION ASIGNA RECARGOS */

		       /* ACTUALIZA LA TEMPORAL POR IMPORTE DEL RECARGO */

			   ACTUALIZA_TEMPORAL_CON_IVA(NVL(V_REC_BON_IVA,0),P_TEM_CCT_ID, V_PRUEBA);

		 	   L_IMPORTE := F_IVA_INVERSO(NVL(V_REC_BON_IVA,0),
			                             V_DATOS_IVA.IVA,
					   	 			     V_DATOS_IVA.ALICUOTA,
						 			     V_DATOS_IVA.PERCEPCION);
               V_IMP_NUEVO_IVA := (NVL(V_REC_BON_IVA,0)) - (L_IMPORTE.IVA + L_IMPORTE.ALICUOTA + L_IMPORTE.PERCEPCION);
			   IF V_DATOS_IVA.IVA = 0 THEN
			      R_CUENTAS_CORRIENTES.CCT_IMP_IVA := 0;
     		      R_CUENTAS_CORRIENTES.CCT_POR_IVA := 0;
			   ELSE
   		          R_CUENTAS_CORRIENTES.CCT_POR_IVA := V_DATOS_IVA.IVA;
   		 	      R_CUENTAS_CORRIENTES.CCT_IMP_IVA := NVL(L_IMPORTE.IVA,0);
			   END IF;
			   IF V_DATOS_IVA.ALICUOTA = 0 AND V_DATOS_IVA.PERCEPCION = 0 THEN
   		 	      R_CUENTAS_CORRIENTES.CCT_IMP_ALI := 0;
   		          R_CUENTAS_CORRIENTES.CCT_POR_ALI := 0;
			   ELSE
   		 	      R_CUENTAS_CORRIENTES.CCT_IMP_ALI := L_IMPORTE.ALICUOTA + L_IMPORTE.PERCEPCION;
			      R_CUENTAS_CORRIENTES.CCT_POR_ALI := V_DATOS_IVA.ALICUOTA + V_DATOS_IVA.PERCEPCION;
               END IF;

		       /* TERMINA DE LLENAR LA VARIABLE UTILIZADA EN LA INSERCION DE CUENTAS CORRIENTES */
		       V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO + 1;
               V_CONCEPTO := 'Recargo';
               R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO   := V_INC_CCT_MOVIMIENTO;
               R_CUENTAS_CORRIENTES.CCT_SER_CODIGO      := 640108;
               R_CUENTAS_CORRIENTES.CCT_IMP_DEBE        := NVL(V_REC_BON_IVA,0);
               R_CUENTAS_CORRIENTES.CCT_CONCEPTO        := NVL(V_CONCEPTO,'0');
			   IF NVL(V_REC_BON_IVA,0) > 0 THEN
			      V_CUENTA_CORRIENTE(V_INC_CCT_MOVIMIENTO) := R_CUENTAS_CORRIENTES;
			   END IF;

			   V_MONTO_SER_INSERT := NVL(R_PLAN_ESP.PPC_RECARGOS,0) - ROUND(V_REC_BON_IVA,2);

			   IF V_MONTO_SER_INSERT > 0 THEN

                  BEGIN
   			         OPEN C_SER_SIN(P_TEM_CCT_ID);
                     FETCH C_SER_SIN INTO R_SER_SIN;
                        IF C_SER_SIN%FOUND	THEN
                           V_REC_BON_SIN := R_SER_SIN.TEM_CCT_MONTO;
                           V_SERVICIO    := R_SER_SIN.TEM_CCT_SERVICIO;
                        ELSE
                     	   v_rec_bon_sin := 0;
                        END IF;
                     CLOSE C_SER_SIN;
			      END;

                  WHILE (V_MONTO_SER_INSERT > 0)  AND (V_REC_BON_SIN > 0) LOOP


               	  /* BUSCA EL SERVICIO PARA INSERTAR */
               	     BEGIN
               	        SELECT RSS_SER_CODIGO2, SER_DES_CORTA
               	        INTO   V_SERVICIO_INSERT, V_CONCEPTO
               	        FROM   REL_SERVICIO_SERVICIO, SERVICIOS
               	        WHERE  RSS_SER_CODIGO = V_SERVICIO
               	          AND  RSS_TIPO = 2
               	          AND  RSS_SER_CODIGO = SER_CODIGO;
               	     END;

				     IF V_MONTO_SER_INSERT > V_REC_BON_SIN THEN

			            R_CUENTAS_CORRIENTES.CCT_IMP_IVA := 0;
     		            R_CUENTAS_CORRIENTES.CCT_POR_IVA := 0;
       		   	        R_CUENTAS_CORRIENTES.CCT_IMP_ALI := 0;
   		                R_CUENTAS_CORRIENTES.CCT_POR_ALI := 0;

		                /* TERMINA DE LLENAR LA VARIABLE UTILIZADA EN LA INSERCION DE CUENTAS CORRIENTES */
		                V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO + 1;
                        R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO   := V_INC_CCT_MOVIMIENTO;
                        R_CUENTAS_CORRIENTES.CCT_SER_CODIGO      := V_SERVICIO_INSERT;
                        R_CUENTAS_CORRIENTES.CCT_IMP_DEBE        := NVL(V_REC_BON_SIN,0);
                        R_CUENTAS_CORRIENTES.CCT_CONCEPTO        := NVL(V_CONCEPTO,'0');
			            IF NVL(V_REC_BON_SIN,0) > 0 THEN
			               V_CUENTA_CORRIENTE(V_INC_CCT_MOVIMIENTO) := R_CUENTAS_CORRIENTES;
			            END IF;
						V_MONTO_SER_INSERT := V_MONTO_SER_INSERT - NVL(V_REC_BON_SIN,0);
					    /* ACTUALIZA LA TEMPORAL CON EL MONTO QUE VA A INSERTAR */
					    ACTUALIZA_TEMPORAL_SIN_IVA(V_REC_BON_SIN, P_TEM_CCT_ID, V_SERVICIO, V_PRUEBA);

					 ELSE

			            R_CUENTAS_CORRIENTES.CCT_IMP_IVA := 0;
     		            R_CUENTAS_CORRIENTES.CCT_POR_IVA := 0;
       		   	        R_CUENTAS_CORRIENTES.CCT_IMP_ALI := 0;
   		                R_CUENTAS_CORRIENTES.CCT_POR_ALI := 0;

		                /* TERMINA DE LLENAR LA VARIABLE UTILIZADA EN LA INSERCION DE CUENTAS CORRIENTES */
		                V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO + 1;
                        R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO   := V_INC_CCT_MOVIMIENTO;
                        R_CUENTAS_CORRIENTES.CCT_SER_CODIGO      := V_SERVICIO_INSERT;
                        R_CUENTAS_CORRIENTES.CCT_IMP_DEBE        := NVL(V_MONTO_SER_INSERT,0);
                        R_CUENTAS_CORRIENTES.CCT_CONCEPTO        := NVL(V_CONCEPTO,'0');
			            IF NVL(V_REC_BON_SIN,0) > 0 THEN
			               V_CUENTA_CORRIENTE(V_INC_CCT_MOVIMIENTO) := R_CUENTAS_CORRIENTES;
			            END IF;
					    ACTUALIZA_TEMPORAL_SIN_IVA(V_MONTO_SER_INSERT, P_TEM_CCT_ID, V_SERVICIO, V_PRUEBA );
						V_MONTO_SER_INSERT := 0;

				     END IF;

               		 BEGIN
   			      	    OPEN C_SER_SIN(P_TEM_CCT_ID);
                  		FETCH C_SER_SIN INTO R_SER_SIN;
                     	   IF C_SER_SIN%FOUND	THEN
                              V_REC_BON_SIN := R_SER_SIN.TEM_CCT_MONTO;
                              V_SERVICIO    := R_SER_SIN.TEM_CCT_SERVICIO;
                           ELSE
                     	      v_rec_bon_sin := 0;
                           END IF;
                        IF C_SER_SIN%ISOPEN THEN
                           CLOSE C_SER_SIN;
                        END IF;

			         END;

			      END LOOP;

			   END IF;
			ELSE /* INCUMPLIMIENTO DE ASIGNA RECARGOS */

			   V_MONTO_SER_INSERT := NVL(R_PLAN_ESP.PPC_RECARGOS,0);

			   IF V_MONTO_SER_INSERT > 0 THEN

                  BEGIN
   			         OPEN C_SER_SIN(P_TEM_CCT_ID);
                     FETCH C_SER_SIN INTO R_SER_SIN;
                        IF C_SER_SIN%FOUND	THEN
                           V_REC_BON_SIN := R_SER_SIN.TEM_CCT_MONTO;
                           V_SERVICIO    := R_SER_SIN.TEM_CCT_SERVICIO;
                        ELSE
                     	   v_rec_bon_sin := 0;
                        END IF;
                     CLOSE C_SER_SIN;
			      END;

                  WHILE (V_MONTO_SER_INSERT > 0)  AND (V_REC_BON_SIN > 0) LOOP
               	  /* BUSCA EL SERVICIO PARA INSERTAR */
               	     BEGIN
               	        SELECT RSS_SER_CODIGO2, SER_DES_CORTA
               	        INTO   V_SERVICIO_INSERT, V_CONCEPTO
               	        FROM   REL_SERVICIO_SERVICIO, SERVICIOS
               	        WHERE  RSS_SER_CODIGO = V_SERVICIO
               	          AND  RSS_TIPO = 2
               	          AND  RSS_SER_CODIGO = SER_CODIGO;
               	     END;
				     IF V_MONTO_SER_INSERT > V_REC_BON_SIN THEN

					    /* ACTUALIZA LA TEMPORAL CON EL MONTO QUE VA A INSERTAR */
					    ACTUALIZA_TEMPORAL_SIN_IVA(V_REC_BON_SIN, P_TEM_CCT_ID, V_SERVICIO, V_PRUEBA);

			            R_CUENTAS_CORRIENTES.CCT_IMP_IVA := 0;
     		            R_CUENTAS_CORRIENTES.CCT_POR_IVA := 0;
       		   	        R_CUENTAS_CORRIENTES.CCT_IMP_ALI := 0;
   		                R_CUENTAS_CORRIENTES.CCT_POR_ALI := 0;

		                /* TERMINA DE LLENAR LA VARIABLE UTILIZADA EN LA INSERCION DE CUENTAS CORRIENTES */
		                V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO + 1;
                        R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO   := V_INC_CCT_MOVIMIENTO;
                        R_CUENTAS_CORRIENTES.CCT_SER_CODIGO      := V_SERVICIO_INSERT;
                        R_CUENTAS_CORRIENTES.CCT_IMP_DEBE        := NVL(V_REC_BON_SIN,0);
                        R_CUENTAS_CORRIENTES.CCT_CONCEPTO        := NVL(V_CONCEPTO,'0');
			            IF NVL(V_REC_BON_SIN,0) > 0 THEN
			               V_CUENTA_CORRIENTE(V_INC_CCT_MOVIMIENTO) := R_CUENTAS_CORRIENTES;
			            END IF;

						V_MONTO_SER_INSERT := V_MONTO_SER_INSERT - NVL(V_REC_BON_SIN,0);

					 ELSE

					    ACTUALIZA_TEMPORAL_SIN_IVA(V_MONTO_SER_INSERT, P_TEM_CCT_ID, V_SERVICIO, V_PRUEBA);

			            R_CUENTAS_CORRIENTES.CCT_IMP_IVA := 0;
     		            R_CUENTAS_CORRIENTES.CCT_POR_IVA := 0;
       		   	        R_CUENTAS_CORRIENTES.CCT_IMP_ALI := 0;
   		                R_CUENTAS_CORRIENTES.CCT_POR_ALI := 0;

		                /* TERMINA DE LLENAR LA VARIABLE UTILIZADA EN LA INSERCION DE CUENTAS CORRIENTES */
		                V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO + 1;
                        R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO   := V_INC_CCT_MOVIMIENTO;
                        R_CUENTAS_CORRIENTES.CCT_SER_CODIGO      := V_SERVICIO_INSERT;
                        R_CUENTAS_CORRIENTES.CCT_IMP_DEBE        := NVL(V_MONTO_SER_INSERT,0);
                        R_CUENTAS_CORRIENTES.CCT_CONCEPTO        := NVL(V_CONCEPTO,'0');
			            IF NVL(V_REC_BON_SIN,0) > 0 THEN
			               V_CUENTA_CORRIENTE(V_INC_CCT_MOVIMIENTO) := R_CUENTAS_CORRIENTES;
			            END IF;
						V_MONTO_SER_INSERT := 0;

				     END IF;

                     BEGIN
   			            OPEN C_SER_SIN(P_TEM_CCT_ID);
                        FETCH C_SER_SIN INTO R_SER_SIN;
                           IF C_SER_SIN%FOUND	THEN
                              V_REC_BON_SIN := R_SER_SIN.TEM_CCT_MONTO;
                              V_SERVICIO    := R_SER_SIN.TEM_CCT_SERVICIO;
                           ELSE
                     	      v_rec_bon_sin := 0;
                           END IF;
                        IF C_SER_SIN%ISOPEN THEN
                           CLOSE C_SER_SIN;
                        END IF;

			         END;

			      END LOOP;

			   END IF;

            END IF; /* CIERRA ASIGNA RECARGOS */

	     END IF;

         IF NVL(R_PLAN_ESP.PPC_INTERESES,0) > 0 THEN

			L_IMPORTE := F_IVA_INVERSO(NVL(R_PLAN_ESP.PPC_INTERESES,0),
			                          V_DATOS_IVA.IVA,
						 			  V_DATOS_IVA.ALICUOTA,
						 			  V_DATOS_IVA.PERCEPCION);
            V_IMP_NUEVO_IVA := (NVL(R_PLAN_ESP.PPC_INTERESES,0)) - (L_IMPORTE.IVA + L_IMPORTE.ALICUOTA + L_IMPORTE.PERCEPCION);
			/* TERMINA DE LLENAR LA VARIABLE UTILIZADA EN LA INSERCION DE CUENTAS CORRIENTES */
			IF V_DATOS_IVA.IVA = 0 THEN
			   R_CUENTAS_CORRIENTES.CCT_IMP_IVA := 0;
     		   R_CUENTAS_CORRIENTES.CCT_POR_IVA := 0;
			ELSE
   		       R_CUENTAS_CORRIENTES.CCT_POR_IVA := V_DATOS_IVA.IVA;
   		 	   R_CUENTAS_CORRIENTES.CCT_IMP_IVA := NVL(L_IMPORTE.IVA,0);
			END IF;
			IF V_DATOS_IVA.ALICUOTA = 0 AND V_DATOS_IVA.PERCEPCION = 0 THEN
   		 	   R_CUENTAS_CORRIENTES.CCT_IMP_ALI := 0;
   		       R_CUENTAS_CORRIENTES.CCT_POR_ALI := 0;
			ELSE
   		 	   R_CUENTAS_CORRIENTES.CCT_IMP_ALI := L_IMPORTE.ALICUOTA + L_IMPORTE.PERCEPCION;
			   R_CUENTAS_CORRIENTES.CCT_POR_ALI := V_DATOS_IVA.ALICUOTA + V_DATOS_IVA.PERCEPCION;
            END IF;
   	        V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO +1;
            V_CONCEPTO := 'Interes';
            R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO   := V_INC_CCT_MOVIMIENTO;
            R_CUENTAS_CORRIENTES.CCT_SER_CODIGO      := 640106;
            R_CUENTAS_CORRIENTES.CCT_IMP_DEBE        := NVL(R_PLAN_ESP.PPC_INTERESES,0);
            R_CUENTAS_CORRIENTES.CCT_CONCEPTO        := NVL(V_CONCEPTO,'0');
			IF NVL(R_PLAN_ESP.PPC_INTERESES,0) > 0 THEN
               V_CUENTA_CORRIENTE(V_INC_CCT_MOVIMIENTO) := R_CUENTAS_CORRIENTES;
			END IF;
         END IF;

         IF NVL(R_PLAN_ESP.PPC_CAPITAL,0) > 0 THEN

		    R_CUENTAS_CORRIENTES.CCT_IMP_IVA := 0;
     		R_CUENTAS_CORRIENTES.CCT_POR_IVA := 0;
 		    R_CUENTAS_CORRIENTES.CCT_POR_ALI := 0;
   		 	R_CUENTAS_CORRIENTES.CCT_IMP_ALI := 0;
            V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO + 1;
            V_CONCEPTO := 'Capital';
            R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO   := V_INC_CCT_MOVIMIENTO;
            R_CUENTAS_CORRIENTES.CCT_SER_CODIGO      := 640109;
            R_CUENTAS_CORRIENTES.CCT_IMP_DEBE        := NVL(R_PLAN_ESP.PPC_CAPITAL,0);
            R_CUENTAS_CORRIENTES.CCT_CONCEPTO        := NVL(V_CONCEPTO,'0');
			IF NVL(R_PLAN_ESP.PPC_CAPITAL,0) > 0 THEN
               V_CUENTA_CORRIENTE(V_INC_CCT_MOVIMIENTO) := R_CUENTAS_CORRIENTES;
		    END IF;
         END IF;
	  END;
   END LOOP;

   PKG_PLANES_PAGO.INSERTA_PLAN_PAGO(P_CAB_PLANES);

   PKG_PLANES_PAGO.inserta_obligaciones(P_UPDATE_OBLIGACIONES,
                                        V_obligacion,
						                V_CUENTA_CORRIENTE);

   v_retorna := PKG_PLANES_PAGO.ACTUALI_OBLIGACION(P_CAB_PLANES.PPL_ID);
   IF v_retorna <> 'FINALIZO' THEN
       ROLLBACK;
   END IF;
   CORRIGEPP(p_cab_planes.ppl_id);

   RETURN r_retorno;

END;

/********************************************************************************************* */
  FUNCTION F_CONFECCIONA_PLAN_PAGO (P_CAB_PLANES          PLANES_PAGO%ROWTYPE,
                                    P_UPDATE_OBLIGACIONES PKG_PLANES_PAGO.UpdateObligacionesTyp,
                                    P_CUENTA              INMUEBLES.INM_CUENTA%TYPE,
					                P_TIPO_RESPONSABLE    INMUEBLES.INM_TIPO_RESPONSABLE%TYPE,
                                    P_RECAUDA             PKG_RECAUDACIONES.VIncRecaudacionRec,
									P_TEM_CCT_ID          DTE_CUENTAS_CORRIENTES.TEM_CCT_ID%TYPE)
									RETURN PKG_RECAUDACIONES.VIncRecaudacionRec IS

V_MONTO_CUOTA        PKG_PLANES_ESPECIALES.R_MONTO_CUOTA;
V_DATOS_CUOTA   	 PKG_PLANES_ESPECIALES.R_DATOS_M_CUOTA;
R_OBLIGACIONES       OBLIGACIONES%ROWTYPE;
R_CUENTAS_CORRIENTES CUENTAS_CORRIENTES%ROWTYPE;
R_PLANES_PAGO        PLANES_PAGO%ROWTYPE;
V_OBLIGACION_ID 	 OBLIGACIONES.OBL_ID%TYPE := 0;
V_INDICE        	 NUMBER(10) := 0;
V_INDICE_CTT         NUMBER(10) := 0;
V_CONCEPTO      	 CUENTAS_CORRIENTES.CCT_CONCEPTO%TYPE;
V_INC_CCT_MOVIMIENTO CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO%TYPE := 0;
V_CENTAVOS           NUMBER(17,10) := 0;
V_TOTAL_CUOTAS       NUMBER(2) := 0;
V_INTERES_SIN_IVA    NUMBER(17,10) := 0;
V_INTERES_CON_IVA    NUMBER(17,10) := 0;
V_DEUDA_CALCULO      NUMBER(17,10) := 0;
V_DATOS_IVA    	     PKG_SERVICIOS_FIJOS.ivarec;
L_IMPORTE            PKG_SERVICIOS_FIJOS.importerec;
V_CUOTA_SIN_INTERES  NUMBER(15,2) := 0;
V_IMPORTE_NETO       NUMBER(15,2) := 0;
V_NRO_CUOTA          NUMBER(2)    := 0;
V_DIAS               NUMBER(4)    := 0;
V_FECHA_VENC_CUOTA   DATE;
V_MONTO_CUOTA_HISTOR NUMBER(17,10) := 0;
V_OBLIGACION         PKG_PLANES_PAGO.ObligacionInsercionTyp;
V_CUENTA_CORRIENTE   PKG_PLANES_PAGO.CuentasCorrieneTyp;
V_RECARGO_BONIFICADO NUMBER(15,2) := 0;
V_CUOTA_CONT         NUMBER(4) := 0;
V_ACTUALIZACION      VARCHAR2(500);
V_RECAUDA            PKG_RECAUDACIONES.VIncRecaudacionRec := P_RECAUDA;
V_REC_BON_IVA        NUMBER(15,2) := 0;
V_REC_BON_SIN        NUMBER(15,2) := 0;
V_SERVICIO           SERVICIOS.SER_CODIGO%TYPE;
V_SERVICIO_INSERT    SERVICIOS.SER_CODIGO%TYPE;

CURSOR c_servicio(P_TEM_CCT_ID DTE_CUENTAS_CORRIENTES.TEM_CCT_ID%TYPE)
                    IS SELECT NVL(SUM(TEM_CCT_MONTO),0) TEM_CCT_MONTO
                       FROM   DTE_CUENTAS_CORRIENTES, SERVICIOS
                       WHERE  TEM_CCT_SERVICIO = SER_CODIGO
                         AND  SER_CALCULA_IVA = 'S'
                         AND  TEM_CALCULA = 'S'
                         AND  TEM_CCT_ID = P_TEM_CCT_ID
					   ORDER BY TEM_CCT_SERVICIO;

  CURSOR c_ser_sin(P_TEM_CCT_ID DTE_CUENTAS_CORRIENTES.TEM_CCT_ID%TYPE)
                    IS SELECT TEM_CCT_SERVICIO, NVL(SUM(TEM_CCT_MONTO),0) TEM_CCT_MONTO
                       FROM   DTE_CUENTAS_CORRIENTES, SERVICIOS
                       WHERE  TEM_CCT_SERVICIO = SER_CODIGO
                         AND  SER_CALCULA_IVA = 'N'
                         AND  TEM_CALCULA = 'S'
                         AND  TEM_CCT_ID = P_TEM_CCT_ID
						 AND  TEM_CCT_MONTO > 0
                       GROUP BY TEM_CCT_SERVICIO;
   R_SER_SIN C_SER_SIN%ROWTYPE;

   V_PRUEBA VARCHAR2(100);
   V_PRUEBA_S NUMBER(15,2) := 0;
   V_PRUEBA_N NUMBER(15,2) := 0;
BEGIN

      V_DATOS_IVA          := PKG_SERVICIOS_FIJOS.F_IMP_IVA(P_TIPO_RESPONSABLE,
                                                            640109,
				                                            SYSDATE);
      L_IMPORTE.IVA        := (V_DATOS_IVA.IVA * (P_CAB_PLANES.PPL_TASA_INTERES / 100))/100;
      L_IMPORTE.ALICUOTA   := (V_DATOS_IVA.ALICUOTA * (P_CAB_PLANES.PPL_TASA_INTERES / 100))/100;
      L_IMPORTE.PERCEPCION := (V_DATOS_IVA.PERCEPCION * ((P_CAB_PLANES.PPL_TASA_INTERES / 100) + L_IMPORTE.IVA))/100;

   /* CARGA LOS DATOS QUE UTILIZA PARA CALCULAR LAS CUOTAS DEL PLAN */
--   V_MONTO_CUOTA.V_TOTAL       := NVL(P_CAB_PLANES.PPL_DEUDA_HISTORICA,0) + (NVL(P_CAB_PLANES.PPL_MONTO_RECARGOS,0)* ((100 - P_CAB_PLANES.PPL_BONIF_RECARGO)/100)) - NVL(P_CAB_PLANES.PPL_MONTO_PAGO_INICIAL,0);
   V_MONTO_CUOTA.V_TOTAL       := NVL(P_CAB_PLANES.PPL_DEUDA_HISTORICA,0) + NVL(P_CAB_PLANES.PPL_MONTO_RECARGOS,0) - NVL(P_CAB_PLANES.PPL_MONTO_PAGO_INICIAL,0) - NVL(P_CAB_PLANES.PPL_QUITA ,0);
--   V_MONTO_CUOTA.V_INTERES     := P_CAB_PLANES.PPL_TASA_INTERES / 100;
   V_MONTO_CUOTA.V_INTERES     := (P_CAB_PLANES.PPL_TASA_INTERES / 100) ; --+ nvl(L_IMPORTE.IVA,0) + nvl(L_IMPORTE.ALICUOTA,0) + nvl(L_IMPORTE.PERCEPCION,0) ;
   V_MONTO_CUOTA.V_CANT_CUOTAS := P_CAB_PLANES.PPL_CNT_CUOTAS;
   V_MONTO_CUOTA.V_POR_BONIF   := P_CAB_PLANES.PPL_TASA_BONIFICACIONES;
   V_MONTO_CUOTA.V_TIPO_RESP   := P_TIPO_RESPONSABLE;
   V_MONTO_CUOTA.V_SERVICIO    := 640106;
   /* LLENA LA VARIABLE QUE SE UTILIZA PARA HACER LA INCERCION EN PLANES DE PAGO */
   R_PLANES_PAGO.PPL_DEUDA_HISTORICA     := P_CAB_PLANES.PPL_DEUDA_HISTORICA;
   R_PLANES_PAGO.PPL_MONTO_RECARGOS      := P_CAB_PLANES.PPL_MONTO_RECARGOS;
   R_PLANES_PAGO.PPL_ID                  := P_CAB_PLANES.PPL_ID;
   R_PLANES_PAGO.PPL_MPP_ID              := P_CAB_PLANES.PPL_MPP_ID;
   R_PLANES_PAGO.PPL_INM_ID              := P_CAB_PLANES.PPL_INM_ID;
   R_PLANES_PAGO.PPL_CLI_ID              := P_CAB_PLANES.PPL_CLI_ID;
   R_PLANES_PAGO.PPL_FECHA               := P_CAB_PLANES.PPL_FECHA;
   R_PLANES_PAGO.PPL_CNT_CUOTAS          := P_CAB_PLANES.PPL_CNT_CUOTAS;
   R_PLANES_PAGO.PPL_PRIMER_VTO          := P_CAB_PLANES.PPL_PRIMER_VTO;
   R_PLANES_PAGO.PPL_QUITA               := P_CAB_PLANES.PPL_QUITA;
   R_PLANES_PAGO.PPL_TASA_INTERES        := P_CAB_PLANES.PPL_TASA_INTERES;
   R_PLANES_PAGO.PPL_TASA_RECARGOS       := P_CAB_PLANES.PPL_TASA_RECARGOS;
   R_PLANES_PAGO.PPL_TASA_BONIFICACIONES := P_CAB_PLANES.PPL_TASA_BONIFICACIONES;
   R_PLANES_PAGO.PPL_ESTADO              := P_CAB_PLANES.PPL_ESTADO;
   R_PLANES_PAGO.PPL_MODALIDAD           := P_CAB_PLANES.PPL_MODALIDAD;
   R_PLANES_PAGO.PPL_USR_ALTA            := P_CAB_PLANES.PPL_USR_ALTA;
   R_PLANES_PAGO.PPL_FEC_ALTA            := P_CAB_PLANES.PPL_FEC_ALTA;
   R_PLANES_PAGO.PPL_USR_BAJA            := P_CAB_PLANES.PPL_USR_BAJA;
   R_PLANES_PAGO.PPL_FEC_BAJA            := P_CAB_PLANES.PPL_FEC_BAJA;
   R_PLANES_PAGO.PPL_USR_MOD             := P_CAB_PLANES.PPL_USR_MOD;
   R_PLANES_PAGO.PPL_FEC_MOD             := P_CAB_PLANES.PPL_FEC_MOD;
   R_PLANES_PAGO.PPL_BONIF_RECARGO       := P_CAB_PLANES.PPL_BONIF_RECARGO;
   /* CARGA DATOS EN LA VARIABLE QUE DESPUES SE USA PARA INSERTAR EN OBLIGACIONES */
   R_OBLIGACIONES.OBL_CLI_ID            := R_PLANES_PAGO.PPL_CLI_ID;
   R_OBLIGACIONES.OBL_LOT_COD_LOTE      := NULL;
   R_OBLIGACIONES.OBL_MAO_CODIGO        := NULL;
   R_OBLIGACIONES.OBL_CON_INM_ID        := R_PLANES_PAGO.PPL_INM_ID;
   R_OBLIGACIONES.OBL_CON_ID            := NULL;
   R_OBLIGACIONES.OBL_DOM_ID            := PKG_PLANES_ESPECIALES.F_RECUPERA_DOMICILIO(R_PLANES_PAGO.PPL_INM_ID);
   R_OBLIGACIONES.OBL_CUENTA            := P_CUENTA;
   R_OBLIGACIONES.OBL_FEC_GENERACION    := SYSDATE;
   R_OBLIGACIONES.OBL_ESTADO            := 15;
   R_OBLIGACIONES.OBL_NRO_FACTURA       := R_PLANES_PAGO.PPL_ID;
   R_OBLIGACIONES.OBL_USR_ALTA          := R_PLANES_PAGO.PPL_USR_ALTA;
   R_OBLIGACIONES.OBL_FEC_ALTA          := SYSDATE;
   R_OBLIGACIONES.OBL_OBL_ID            := NULL;
   R_OBLIGACIONES.OBL_PPL_ID            := R_PLANES_PAGO.PPL_ID;
   R_OBLIGACIONES.OBL_INM_ID            := R_PLANES_PAGO.PPL_INM_ID;
   R_OBLIGACIONES.OBL_COV_ID            := NULL;
   R_OBLIGACIONES.OBL_PMO_ID            := NULL;
   R_OBLIGACIONES.OBL_TIPO_PLAN         := R_PLANES_PAGO.PPL_MPP_ID;
   R_OBLIGACIONES.OBL_FEC_INI_ETAPA_MOR := NULL;
   R_OBLIGACIONES.OBL_FEC_FIN_ETAPA_MOR := NULL;
   R_OBLIGACIONES.OBL_EST_MOROSIDAD     := NULL;
   R_OBLIGACIONES.OBL_NRO_FORMULARIO    := NULL;
   R_OBLIGACIONES.OBL_FEC_PROCESO       := NULL;
   R_OBLIGACIONES.OBL_FEC_APLICACION    := NULL;
   R_OBLIGACIONES.OBL_BOLETA_DEUDA      := NULL;
   R_OBLIGACIONES.OBL_IMP_IVA_EXE       := 0;
   R_OBLIGACIONES.OBL_IMP_IVA_RI        := 0;
   R_OBLIGACIONES.OBL_IMP_IVA_RNI       := 0;
   R_OBLIGACIONES.OBL_IMP_IVA_MON       := 0;
   R_OBLIGACIONES.OBL_SALDO_MOROSIDAD   := NULL;
   R_OBLIGACIONES.OBL_IMP_RECARGO       := NULL;
   R_OBLIGACIONES.OBL_FEC_IMPRESION     := NULL;
   R_OBLIGACIONES.OBL_FEC_FACTURACION   := NULL;
   R_OBLIGACIONES.OBL_FEC_APROBACION    := NULL;
   R_OBLIGACIONES.OBL_FEC_DISTRIBUCION  := NULL;
   R_OBLIGACIONES.OBL_FEC_GENERACION_NC := NULL;
   R_OBLIGACIONES.OBL_USR_MOD           := NULL;
   R_OBLIGACIONES.OBL_FEC_MOD           := NULL;
   R_OBLIGACIONES.OBL_USR_BAJA          := NULL;
   R_OBLIGACIONES.OBL_FEC_BAJA          := NULL;
   R_OBLIGACIONES.OBL_SE                := 'N';
   R_OBLIGACIONES.OBL_TPC_CODIGO_NC     := NULL;
   R_OBLIGACIONES.OBL_OBL_ID_NC         := NULL;
   R_OBLIGACIONES.OBL_PPL_ID_CUOTA      := NULL;
   R_OBLIGACIONES.OBL_FEC_EST_MOROSO    := NULL;
   R_OBLIGACIONES.OBL_FEC_CONTABLE      := SYSDATE;
   R_OBLIGACIONES.OBL_NRO_COMPROB_NC    := NULL;
   R_OBLIGACIONES.OBL_ETA_COD_ETAPA     := NULL;
   R_OBLIGACIONES.OBL_EMO_COD_ETAPA     := NULL;
   R_OBLIGACIONES.OBL_SUC_CODIGO        := SUBSTR(P_CUENTA,1,3);
   R_OBLIGACIONES.OBL_GRP_CODIGO        := PKG_PLANES_ESPECIALES.F_RECUPERA_GRUPO(SUBSTR(P_CUENTA,1,3));
   R_OBLIGACIONES.OBL_TRE_CODIGO        := P_TIPO_RESPONSABLE;
   R_OBLIGACIONES.OBL_POR_IVA_EXE       := 0;
   R_OBLIGACIONES.OBL_POR_IVA_RI        := 0;
   R_OBLIGACIONES.OBL_POR_IVA_RNI       := 0;
   R_OBLIGACIONES.OBL_POR_IVA_MON       := 0;
   /* LLENO LOS DATOS QUE NO VARIAN DE CUENTAS_CORRIENTES */
   R_CUENTAS_CORRIENTES.CCT_IMP_HABER           := 0;
   R_CUENTAS_CORRIENTES.CCT_FEC_GENERACION      := SYSDATE;
   R_CUENTAS_CORRIENTES.CCT_FEC_CIERRE_CONTABLE := SYSDATE;
   R_CUENTAS_CORRIENTES.CCT_USR_ALTA            := R_PLANES_PAGO.PPL_USR_ALTA;
   R_CUENTAS_CORRIENTES.CCT_FEC_ALTA            := SYSDATE;
   R_CUENTAS_CORRIENTES.CCT_REC_ID              := NULL;
   R_CUENTAS_CORRIENTES.CCT_NOV_ID              := NULL;
   R_CUENTAS_CORRIENTES.CCT_CNT_DIAS_RECARGO    := 1;
   R_CUENTAS_CORRIENTES.CCT_BANCO               := NULL;
   R_CUENTAS_CORRIENTES.CCT_REMESA              := NULL;
   R_CUENTAS_CORRIENTES.CCT_SECUENCIA           := NULL;
   R_CUENTAS_CORRIENTES.CCT_FEC_APLICACION      := NULL;
   R_CUENTAS_CORRIENTES.CCT_FEC_PROCESO         := NULL;
   R_CUENTAS_CORRIENTES.CCT_FEC_COBRO           := NULL;
   R_CUENTAS_CORRIENTES.CCT_USR_BAJA            := NULL;
   R_CUENTAS_CORRIENTES.CCT_FEC_BAJA            := NULL;
   R_CUENTAS_CORRIENTES.CCT_USR_MOD             := NULL;
   R_CUENTAS_CORRIENTES.CCT_FEC_MOD             := NULL;
   R_CUENTAS_CORRIENTES.CCT_ESTADO              := 15;
   R_CUENTAS_CORRIENTES.CCT_SE                  := 'N';
   R_CUENTAS_CORRIENTES.CCT_OBL_ID_CUOTA        := NULL;
   R_CUENTAS_CORRIENTES.CCT_GRP_CODIGO          := PKG_PLANES_ESPECIALES.F_RECUPERA_GRUPO(SUBSTR(P_CUENTA,1,3));
   R_CUENTAS_CORRIENTES.CCT_CUENTA              := P_CUENTA;
   R_CUENTAS_CORRIENTES.CCT_SUC_CODIGO          := SUBSTR(P_CUENTA,1,3);
   R_CUENTAS_CORRIENTES.CCT_TRE_CODIGO          := P_TIPO_RESPONSABLE;
   /* REALIZA LA INSERCION EN CASO DE QUE TENGA UN PAGO INICIAL */
   IF  P_CAB_PLANES.PPL_MONTO_PAGO_INICIAL  > 0 THEN
 	  V_RECAUDA.V_ANIO    := TO_NUMBER(TO_CHAR((SYSDATE),'YYYY'));
	  V_RECAUDA.V_PERIODO := TO_NUMBER(TO_CHAR((SYSDATE),'MM'));
      SELECT OBL_SEQ.NEXTVAL INTO V_OBLIGACION_ID FROM DUAL;
      /* TERMINA DE CARGAR LA VARIABLE PARA HACER LA INSERCION EN OBLIGACIONES */
      R_OBLIGACIONES.OBL_ID              := V_OBLIGACION_ID;
      R_OBLIGACIONES.OBL_PEF_ANIO        := TO_NUMBER(TO_CHAR((SYSDATE),'YYYY'));
      R_OBLIGACIONES.OBL_PEF_PERIODO     := TO_NUMBER(TO_CHAR((SYSDATE),'MM'));
      R_OBLIGACIONES.OBL_TPC_CODIGO      := 9;
      R_OBLIGACIONES.OBL_IMP_ORIGINAL    := NVL(P_CAB_PLANES.PPL_MONTO_PAGO_INICIAL,0);
      R_OBLIGACIONES.OBL_SALDO           := NVL(P_CAB_PLANES.PPL_MONTO_PAGO_INICIAL,0);
	  R_OBLIGACIONES.OBL_FEC_VENCIMIENTO := P_CAB_PLANES.PPL_FECHA;
	  R_OBLIGACIONES.OBL_IMP_NETO        := NVL(P_CAB_PLANES.PPL_MONTO_PAGO_INICIAL,0);
      R_OBLIGACIONES.OBL_CUOTA_PLAN      := V_NRO_CUOTA;
      R_OBLIGACIONES.OBL_IMP_IVA_CF      := 0;
      R_OBLIGACIONES.OBL_IMP_ALI_RNI     := 0;
      R_OBLIGACIONES.OBL_IMP_ALI_NO_CAT  := 0;
      R_OBLIGACIONES.OBL_POR_IVA_CF      := 0;
      R_OBLIGACIONES.OBL_ALI_RNI         := 0;
      R_OBLIGACIONES.OBL_ALI_NO_CAT      := 0;
	  V_INDICE := V_INDICE + 1;
      V_OBLIGACION(V_INDICE) := R_OBLIGACIONES;
      /* TERMINA DE CARGAR LOS DATOS PARA LA INSERCION EN CUENTAS CORRIENTES */
      V_CONCEPTO := 'Pago inicial plan '||R_PLANES_PAGO.PPL_ID;
      V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO +1;
      R_CUENTAS_CORRIENTES.CCT_OBL_ID          := V_OBLIGACION_ID;
      R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO   := V_INC_CCT_MOVIMIENTO;
      R_CUENTAS_CORRIENTES.CCT_SER_CODIGO      := 640109;
      R_CUENTAS_CORRIENTES.CCT_PEF_ANIO        := TO_NUMBER(TO_CHAR((P_CAB_PLANES.PPL_FECHA),'YYYY'));
      R_CUENTAS_CORRIENTES.CCT_PEF_PERIODO     := TO_NUMBER(TO_CHAR((P_CAB_PLANES.PPL_FECHA),'MM'));
      R_CUENTAS_CORRIENTES.CCT_TIPO_MOVIMIENTO := 9;
      R_CUENTAS_CORRIENTES.CCT_IMP_DEBE        := NVL(P_CAB_PLANES.PPL_MONTO_PAGO_INICIAL ,0);
      R_CUENTAS_CORRIENTES.CCT_CONCEPTO        := V_CONCEPTO;
      R_CUENTAS_CORRIENTES.CCT_FEC_VENCIMIENTO := P_CAB_PLANES.PPL_FECHA;
      R_CUENTAS_CORRIENTES.CCT_IMP_IVA         := 0;
      R_CUENTAS_CORRIENTES.CCT_POR_IVA         := 0;
      R_CUENTAS_CORRIENTES.CCT_IMP_ALI         := 0;
      R_CUENTAS_CORRIENTES.CCT_POR_ALI         := 0;
	  V_INDICE_CTT := V_INDICE_CTT + 1;
      V_CUENTA_CORRIENTE(V_INDICE_CTT)         := R_CUENTAS_CORRIENTES;
   END IF;
   /* TERMINA DE CARAR LA CABECERA DEL PLAN */
   R_PLANES_PAGO.PPL_VTO_PAGO_INICIAL    := P_CAB_PLANES.PPL_FECHA;
   R_PLANES_PAGO.PPL_MONTO_PAGO_INICIAL  := P_CAB_PLANES.PPL_MONTO_PAGO_INICIAL;
   R_PLANES_PAGO.PPL_INT_PAGO_INICIAL    := 0;
   /* LLAMADA A LA FUCNION QUE GENERA LAS CUOTAS Y TODOS LOS DATOS QUE UTILIZA */
   IF V_MONTO_CUOTA.V_CANT_CUOTAS > 0 THEN
      V_DATOS_CUOTA                   := GENERA_MONTO_CUOTAS(V_MONTO_CUOTA,P_TEM_CCT_ID);
      R_PLANES_PAGO.PPL_FEC_CADUCIDAD := P_CAB_PLANES.PPL_FECHA + (30 * (V_MONTO_CUOTA.V_CANT_CUOTAS)) + 30;
      R_PLANES_PAGO.PPL_IMP_CUOTA     := V_DATOS_CUOTA.V_MONTO_CUOTA;
      -- cambiado por SROMERO - pedido 8188166 - 17/02/16
      -- estaba tomando la cuota sin interes
--      R_PLANES_PAGO.PPL_IMP_INTERESES := V_DATOS_CUOTA.V_INTERES_TOTAL * V_MONTO_CUOTA.V_CANT_CUOTAS;
      R_PLANES_PAGO.PPL_IMP_INTERESES := (ROUND(V_DATOS_CUOTA.V_MONTO_CUOTA-v_datos_cuota.v_una_cuota,2))*P_CAB_PLANES.PPL_CNT_CUOTAS;
      -- FIN cambiado por SROMERO - pedido 8188166 - 17/02/16
   ELSE
	  R_PLANES_PAGO.PPL_FEC_CADUCIDAD := P_CAB_PLANES.PPL_FECHA + 30;
	  R_PLANES_PAGO.PPL_IMP_CUOTA     := 0;
	  R_PLANES_PAGO.PPL_IMP_INTERESES := 0;
   END IF;

   IF P_CAB_PLANES.PPL_CNT_CUOTAS > 0 THEN
      R_PLANES_PAGO.PPL_PRIMER_VTO := P_CAB_PLANES.PPL_PRIMER_VTO + 30;
   END IF;

   PKG_PLANES_PAGO.INSERTA_PLAN_PAGO(R_PLANES_PAGO);

   /* ACTULIZA LAS OBLIGACIONES VIEJAS CON LOS DATOS DEL PLAN DE PAGO */
   -- cambiado por SROMERO - pedido 8188166 - 17/02/16
   -- estaba tomando la cuota sin interés
--   V_CENTAVOS           := ((V_MONTO_CUOTA.V_TOTAL)+ROUND(R_PLANES_PAGO.PPL_IMP_INTERESES,2))-(ROUND(V_DATOS_CUOTA.V_MONTO_CUOTA,2)*P_CAB_PLANES.PPL_CNT_CUOTAS);
   V_CENTAVOS           := ((V_MONTO_CUOTA.V_TOTAL)+(ROUND(V_DATOS_CUOTA.V_MONTO_CUOTA-v_datos_cuota.v_una_cuota,2))*P_CAB_PLANES.PPL_CNT_CUOTAS)-(ROUND(V_DATOS_CUOTA.V_MONTO_CUOTA,2)*P_CAB_PLANES.PPL_CNT_CUOTAS);
   -- FIN cambiado por SROMERO - pedido 8188166 - 17/02/16
   V_DEUDA_CALCULO      := V_MONTO_CUOTA.V_TOTAL;
   V_RECARGO_BONIFICADO := P_CAB_PLANES.PPL_MONTO_RECARGOS; --* ((100 - P_CAB_PLANES.PPL_BONIF_RECARGO)/100);

   WHILE P_CAB_PLANES.PPL_CNT_CUOTAS > V_CUOTA_CONT LOOP

 	  V_INC_CCT_MOVIMIENTO := 0;
      V_CUOTA_CONT         := V_CUOTA_CONT + 1;
      V_INTERES_SIN_IVA    := V_DEUDA_CALCULO * V_MONTO_CUOTA.V_INTERES;
      V_DATOS_IVA          := PKG_SERVICIOS_FIJOS.F_IMP_IVA(P_TIPO_RESPONSABLE,
                                                            640109,
				                                            SYSDATE);
     -- cambiado por SROMERO - pedido 8188166 - 17/02/16
     -- estaba calculando el IVA sobre la tasa y no sobre el importe
--      L_IMPORTE.IVA        := (V_DATOS_IVA.IVA * (P_CAB_PLANES.PPL_TASA_INTERES / 100))/100;
--      L_IMPORTE.ALICUOTA   := (V_DATOS_IVA.ALICUOTA * (P_CAB_PLANES.PPL_TASA_INTERES / 100))/100;
--      L_IMPORTE.PERCEPCION := (V_DATOS_IVA.PERCEPCION * ((P_CAB_PLANES.PPL_TASA_INTERES / 100) + L_IMPORTE.IVA))/100;
      L_IMPORTE.IVA        := V_DATOS_IVA.IVA * V_INTERES_SIN_IVA/100;
      L_IMPORTE.ALICUOTA   := V_DATOS_IVA.ALICUOTA * V_INTERES_SIN_IVA/100;
      L_IMPORTE.PERCEPCION := (V_DATOS_IVA.PERCEPCION * (V_INTERES_SIN_IVA + L_IMPORTE.IVA))/100;
     -- FIN cambiado por SROMERO - pedido 8188166 - 17/02/16
      V_INTERES_CON_IVA    := V_INTERES_SIN_IVA + L_IMPORTE.IVA + L_IMPORTE.ALICUOTA + L_IMPORTE.PERCEPCION;
    --  V_INTERES_CON_IVA    := V_DEUDA_CALCULO * V_MONTO_CUOTA.V_INTERES; -- ahora tiene IVA --
      V_CUOTA_SIN_INTERES  := V_DATOS_CUOTA.V_MONTO_CUOTA - V_INTERES_CON_IVA;
      V_IMPORTE_NETO       := V_CUOTA_SIN_INTERES + V_INTERES_SIN_IVA;
      V_DEUDA_CALCULO      := V_DEUDA_CALCULO - V_CUOTA_SIN_INTERES;
      V_NRO_CUOTA          := V_NRO_CUOTA + 1;
      V_DIAS               := V_DIAS + 30;
      V_FECHA_VENC_CUOTA   := P_CAB_PLANES.PPL_FECHA + V_DIAS; --genera_fecha_cuota(SYSDATE , v_dias);

      SELECT OBL_SEQ.NEXTVAL INTO V_OBLIGACION_ID FROM DUAL;

      BEGIN

         IF V_NRO_CUOTA = 1 THEN
     	    V_MONTO_CUOTA_HISTOR        := V_DATOS_CUOTA.V_MONTO_CUOTA;
          	V_DATOS_CUOTA.V_MONTO_CUOTA := V_DATOS_CUOTA.V_MONTO_CUOTA + V_CENTAVOS;
         ELSE
            V_DATOS_CUOTA.V_MONTO_CUOTA := V_MONTO_CUOTA_HISTOR;
         END IF;

         R_OBLIGACIONES.OBL_ID              := V_OBLIGACION_ID;
         R_OBLIGACIONES.OBL_PEF_ANIO        := TO_NUMBER(TO_CHAR((V_FECHA_VENC_CUOTA),'YYYY'));
         R_OBLIGACIONES.OBL_PEF_PERIODO     := TO_NUMBER(TO_CHAR((V_FECHA_VENC_CUOTA),'MM'));
         R_OBLIGACIONES.OBL_TPC_CODIGO      := 8;
         R_OBLIGACIONES.OBL_IMP_ORIGINAL    := NVL(V_DATOS_CUOTA.V_MONTO_CUOTA,0);
         R_OBLIGACIONES.OBL_SALDO           := NVL(V_DATOS_CUOTA.V_MONTO_CUOTA,0);
         R_OBLIGACIONES.OBL_FEC_VENCIMIENTO := V_FECHA_VENC_CUOTA;
         R_OBLIGACIONES.OBL_IMP_NETO        := NVL(V_IMPORTE_NETO,0);
         R_OBLIGACIONES.OBL_CUOTA_PLAN      := NVL(V_NRO_CUOTA,0);
         R_OBLIGACIONES.OBL_IMP_IVA_CF      := NVL(L_IMPORTE.IVA,0);
         R_OBLIGACIONES.OBL_IMP_ALI_RNI     := NVL(L_IMPORTE.ALICUOTA,0);
         R_OBLIGACIONES.OBL_IMP_ALI_NO_CAT  := NVL(L_IMPORTE.PERCEPCION,0);
         R_OBLIGACIONES.OBL_POR_IVA_CF      := NVL(V_DATOS_IVA.IVA,0);
         R_OBLIGACIONES.OBL_ALI_RNI         := NVL(V_DATOS_IVA.ALICUOTA,0);
         R_OBLIGACIONES.OBL_ALI_NO_CAT      := NVL(V_DATOS_IVA.PERCEPCION,0);
         V_INDICE := V_INDICE + 1;
         V_OBLIGACION(V_INDICE) := R_OBLIGACIONES;
         /* LLENA LA VARIABLE UTILIZADA PARA INSERTAR EN CUENTAS CORRIENTES CON DATOS
            QUE SOLAMENTE VARIAN POR CADA CUOTA Y NO POR CADA INSERCION */
         R_CUENTAS_CORRIENTES.CCT_OBL_ID          := V_OBLIGACION_ID;
         R_CUENTAS_CORRIENTES.CCT_PEF_ANIO        := TO_NUMBER(TO_CHAR((V_FECHA_VENC_CUOTA),'YYYY'));
         R_CUENTAS_CORRIENTES.CCT_PEF_PERIODO     := TO_NUMBER(TO_CHAR((V_FECHA_VENC_CUOTA),'MM'));
         R_CUENTAS_CORRIENTES.CCT_TIPO_MOVIMIENTO := 8;
         R_CUENTAS_CORRIENTES.CCT_FEC_VENCIMIENTO := V_FECHA_VENC_CUOTA;

      END;
      BEGIN

         /* ESTE IF SIRVE PARA CARGAR LOS DATOS EN LA VARIABLE DE CUENTAS CORRIENTES
            CON LOS DATOS QUE VARIAN EN CADA INSERCION */

         IF NVL(V_RECARGO_BONIFICADO,0) > 0 THEN
		    FOR R_SERVICIO IN C_SERVICIO(P_TEM_CCT_ID) LOOP
               V_REC_BON_IVA := R_SERVICIO.TEM_CCT_MONTO;
            END LOOP;
--            IF V_RECARGO_BONIFICADO > V_CUOTA_SIN_INTERES THEN
            IF NVL(V_REC_BON_IVA,0) > V_CUOTA_SIN_INTERES THEN
               V_RECARGO_BONIFICADO := V_RECARGO_BONIFICADO - V_CUOTA_SIN_INTERES;

		       /* ACTUALIZA LA TEMPORAL POR IMPORTE DEL RECARGO */
			   ACTUALIZA_TEMPORAL_CON_IVA(V_CUOTA_SIN_INTERES,P_TEM_CCT_ID, V_PRUEBA);

               BEGIN
                  /* INSERCION EN CUENTAS CORRIENTES EN CASO DE QUE EL RECARGO
                     SEA SUPERIOR A LA CUOTA DEL PLAN */
            	  V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO + 1;
                  L_IMPORTE := F_IVA_INVERSO(NVL(V_CUOTA_SIN_INTERES,0),V_DATOS_IVA.IVA,V_DATOS_IVA.ALICUOTA,V_DATOS_IVA.PERCEPCION);
                  V_CONCEPTO := 'Recargo';
                  R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO := V_INC_CCT_MOVIMIENTO;
                  R_CUENTAS_CORRIENTES.CCT_SER_CODIGO    := 640108;
                  R_CUENTAS_CORRIENTES.CCT_IMP_DEBE      := NVL(V_CUOTA_SIN_INTERES,0);
                  R_CUENTAS_CORRIENTES.CCT_CONCEPTO      := NVL(V_CONCEPTO,'0');
                  R_CUENTAS_CORRIENTES.CCT_IMP_IVA       := NVL(l_importe.iva,0);
                  R_CUENTAS_CORRIENTES.CCT_POR_IVA       := NVL(v_datos_iva.iva,0);
                  SELECT DECODE(NVL(l_importe.PERCEPCION,0), 0, NVL(l_importe.ALICUOTA,0),l_importe.PERCEPCION)
                  INTO   R_CUENTAS_CORRIENTES.CCT_IMP_ALI
                  FROM   DUAL;
--         		  R_CUENTAS_CORRIENTES.CCT_IMP_ALI       := NVL(l_importe.alicuota,0);
                  SELECT DECODE(NVL(v_datos_iva.PERCEPCION,0), 0, NVL(V_DATOS_IVA.ALICUOTA,0),v_datos_iva.PERCEPCION)
                  INTO   R_CUENTAS_CORRIENTES.CCT_POR_ALI
                  FROM   DUAL;
--         		  R_CUENTAS_CORRIENTES.CCT_POR_ALI       := NVL(v_datos_iva.alicuota,0);
				  V_INDICE_CTT := V_INDICE_CTT + 1;
                  V_CUENTA_CORRIENTE(V_INDICE_CTT)       := R_CUENTAS_CORRIENTES;
               END;
               BEGIN
   	              V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO +1;
                  l_importe := F_IVA_INVERSO(NVL(V_INTERES_CON_IVA,0),V_DATOS_IVA.IVA,V_DATOS_IVA.ALICUOTA,V_DATOS_IVA.PERCEPCION);
                  V_CONCEPTO := 'Interes';
                  /* INSERCION EN CUENTAS CORRIENTES POR EL INTERES DE LA CUOTA */
                  R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO := V_INC_CCT_MOVIMIENTO;
                  R_CUENTAS_CORRIENTES.CCT_SER_CODIGO    := 640106;
                  R_CUENTAS_CORRIENTES.CCT_IMP_DEBE      := NVL(V_INTERES_CON_IVA,0);
                  R_CUENTAS_CORRIENTES.CCT_CONCEPTO      := NVL(V_CONCEPTO,'0');
                  R_CUENTAS_CORRIENTES.CCT_IMP_IVA       := NVL(l_importe.iva,0);
                  R_CUENTAS_CORRIENTES.CCT_POR_IVA       := NVL(v_datos_iva.iva,0);
                  SELECT DECODE(NVL(l_importe.PERCEPCION,0), 0,NVL(l_importe.ALICUOTA,0),l_importe.PERCEPCION)
                  INTO   R_CUENTAS_CORRIENTES.CCT_IMP_ALI
                  FROM   DUAL;

                  SELECT DECODE(NVL(v_datos_iva.PERCEPCION,0), 0,NVL(V_DATOS_IVA.ALICUOTA,0),v_datos_iva.PERCEPCION)
                  INTO   R_CUENTAS_CORRIENTES.CCT_POR_ALI
                  FROM   DUAL;
				  V_INDICE_CTT := V_INDICE_CTT + 1;
                  V_CUENTA_CORRIENTE(V_INDICE_CTT)       := R_CUENTAS_CORRIENTES;
               END;

            ELSE

 	           /* ACTUALIZA LA TEMPORAL POR IMPORTE DEL RECARGO */
			   ACTUALIZA_TEMPORAL_CON_IVA(NVL(V_REC_BON_IVA,0),P_TEM_CCT_ID, V_PRUEBA);

               V_RECARGO_BONIFICADO := V_RECARGO_BONIFICADO - NVL(V_REC_BON_IVA,0);

               V_CUOTA_SIN_INTERES := V_CUOTA_SIN_INTERES - NVL(V_REC_BON_IVA,0);

               BEGIN
                  V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO + 1;
                  L_IMPORTE := F_IVA_INVERSO(NVL(V_REC_BON_IVA,0),V_DATOS_IVA.IVA,V_DATOS_IVA.ALICUOTA,V_DATOS_IVA.PERCEPCION);
                  V_CONCEPTO := 'Recargo';
                  R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO := V_INC_CCT_MOVIMIENTO;
                  R_CUENTAS_CORRIENTES.CCT_SER_CODIGO    := 640108;
                  R_CUENTAS_CORRIENTES.CCT_IMP_DEBE      := NVL(V_REC_BON_IVA,0);
                  R_CUENTAS_CORRIENTES.CCT_CONCEPTO      := NVL(V_CONCEPTO,'0');
                  SELECT DECODE(NVL(L_IMPORTE.PERCEPCION,0), 0,NVL(L_IMPORTE.ALICUOTA,0),L_IMPORTE.PERCEPCION)
                  INTO   R_CUENTAS_CORRIENTES.CCT_IMP_ALI
                  FROM   DUAL;

                  SELECT DECODE(NVL(V_DATOS_IVA.PERCEPCION,0), 0,NVL(V_DATOS_IVA.ALICUOTA,0),V_DATOS_IVA.PERCEPCION)
                  INTO   R_CUENTAS_CORRIENTES.CCT_POR_ALI
                  FROM   DUAL;
				  V_INDICE_CTT := V_INDICE_CTT + 1;
                  V_CUENTA_CORRIENTE(V_INDICE_CTT)       := R_CUENTAS_CORRIENTES;
               END;

               BEGIN
   			      OPEN C_SER_SIN(P_TEM_CCT_ID);
                  FETCH C_SER_SIN INTO R_SER_SIN;
                     IF C_SER_SIN%FOUND	THEN
                        V_REC_BON_SIN := R_SER_SIN.TEM_CCT_MONTO;
                        V_SERVICIO    := R_SER_SIN.TEM_CCT_SERVICIO;
                     ELSE
                     	  v_rec_bon_sin := 0;
                     END IF;
                  CLOSE C_SER_SIN;
			   END;

               WHILE (V_CUOTA_SIN_INTERES > 0) AND NVL(V_REC_BON_SIN,0) > 0 LOOP --AND (V_REC_BON_SIN > 0) LOOP
               /* BUSCA EL SERVICIO PARA INSERTAR */

                  BEGIN
                     SELECT RSS_SER_CODIGO2, SER_DES_CORTA
                     INTO   V_SERVICIO_INSERT, V_CONCEPTO
                     FROM   REL_SERVICIO_SERVICIO, SERVICIOS
                     WHERE  RSS_SER_CODIGO = V_SERVICIO
                       AND  RSS_TIPO = 2
                       AND  RSS_SER_CODIGO = SER_CODIGO;
                  END;

			      IF NVL(V_REC_BON_SIN,0) < V_CUOTA_SIN_INTERES THEN

					 V_RECARGO_BONIFICADO := V_RECARGO_BONIFICADO - NVL(V_REC_BON_SIN,0);

                     V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO + 1;
                     R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO := V_INC_CCT_MOVIMIENTO;
                     R_CUENTAS_CORRIENTES.CCT_SER_CODIGO    := V_SERVICIO_INSERT;
                     R_CUENTAS_CORRIENTES.CCT_IMP_DEBE      := NVL(V_REC_BON_SIN,0);
                     R_CUENTAS_CORRIENTES.CCT_CONCEPTO      := NVL(V_CONCEPTO,'0');
                     R_CUENTAS_CORRIENTES.CCT_IMP_ALI       := 0;
                     R_CUENTAS_CORRIENTES.CCT_POR_ALI       := 0;
                     R_CUENTAS_CORRIENTES.CCT_IMP_IVA       := 0;
                     R_CUENTAS_CORRIENTES.CCT_POR_IVA       := 0;
                     V_INDICE_CTT                           := V_INDICE_CTT + 1;
                     V_CUENTA_CORRIENTE(V_INDICE_CTT)       := R_CUENTAS_CORRIENTES;

					 V_CUOTA_SIN_INTERES := V_CUOTA_SIN_INTERES - NVL(V_REC_BON_SIN,0);

					 ACTUALIZA_TEMPORAL_SIN_IVA(V_REC_BON_SIN, P_TEM_CCT_ID, V_SERVICIO, V_PRUEBA);

				  ELSE

					 V_RECARGO_BONIFICADO := V_RECARGO_BONIFICADO - NVL(V_CUOTA_SIN_INTERES,0);

                     V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO + 1;
                     R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO := V_INC_CCT_MOVIMIENTO;
                     R_CUENTAS_CORRIENTES.CCT_SER_CODIGO    := V_SERVICIO_INSERT;
                     R_CUENTAS_CORRIENTES.CCT_IMP_DEBE      := NVL(V_CUOTA_SIN_INTERES,0);
                     R_CUENTAS_CORRIENTES.CCT_CONCEPTO      := NVL(V_CONCEPTO,'0');
                     R_CUENTAS_CORRIENTES.CCT_IMP_ALI       := 0;
                     R_CUENTAS_CORRIENTES.CCT_POR_ALI       := 0;
                     R_CUENTAS_CORRIENTES.CCT_IMP_IVA       := 0;
                     R_CUENTAS_CORRIENTES.CCT_POR_IVA       := 0;
                     V_INDICE_CTT                           := V_INDICE_CTT + 1;
                     V_CUENTA_CORRIENTE(V_INDICE_CTT)       := R_CUENTAS_CORRIENTES;

					 ACTUALIZA_TEMPORAL_SIN_IVA(V_CUOTA_SIN_INTERES, P_TEM_CCT_ID, V_SERVICIO, V_PRUEBA);

					 V_CUOTA_SIN_INTERES := 0;

				  END IF;

                  BEGIN
   			         OPEN C_SER_SIN(P_TEM_CCT_ID);
                     FETCH C_SER_SIN INTO R_SER_SIN;
                        IF C_SER_SIN%FOUND	THEN
                           V_REC_BON_SIN := R_SER_SIN.TEM_CCT_MONTO;
                           V_SERVICIO    := R_SER_SIN.TEM_CCT_SERVICIO;
                        ELSE
                     	   v_rec_bon_sin := 0;
                        END IF;
                     IF C_SER_SIN%ISOPEN THEN
                        CLOSE C_SER_SIN;
                     END IF;
			      END;

			   END LOOP;

	           IF V_CUOTA_SIN_INTERES > 0 THEN
                  BEGIN

                     V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO + 1;
                     V_CONCEPTO := 'Capital';
                     R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO := V_INC_CCT_MOVIMIENTO;
                     R_CUENTAS_CORRIENTES.CCT_SER_CODIGO    := 640109;
                     R_CUENTAS_CORRIENTES.CCT_IMP_DEBE      := NVL(V_CUOTA_SIN_INTERES,0);
                     R_CUENTAS_CORRIENTES.CCT_CONCEPTO      := NVL(V_CONCEPTO,'0');
                     R_CUENTAS_CORRIENTES.CCT_IMP_IVA       := 0;
                     R_CUENTAS_CORRIENTES.CCT_POR_IVA       := 0;
                     R_CUENTAS_CORRIENTES.CCT_IMP_ALI       := 0;
                     R_CUENTAS_CORRIENTES.CCT_POR_ALI       := 0;
			    	 V_INDICE_CTT := V_INDICE_CTT + 1;
                     V_CUENTA_CORRIENTE(V_INDICE_CTT)       := R_CUENTAS_CORRIENTES;
                  END;
			   END IF;
               BEGIN
   	              V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO +1;
                  l_importe := F_IVA_INVERSO(NVL(V_INTERES_CON_IVA,0),V_DATOS_IVA.IVA,V_DATOS_IVA.ALICUOTA,V_DATOS_IVA.PERCEPCION);
                  V_CONCEPTO := 'Interes';
                  R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO := V_INC_CCT_MOVIMIENTO;
                  R_CUENTAS_CORRIENTES.CCT_SER_CODIGO    := 640106;
                  R_CUENTAS_CORRIENTES.CCT_IMP_DEBE      := NVL(V_INTERES_CON_IVA,0);
                  R_CUENTAS_CORRIENTES.CCT_CONCEPTO      := NVL(V_CONCEPTO,'0');
                  SELECT DECODE(NVL(l_importe.PERCEPCION,0), 0,NVL(l_importe.ALICUOTA,0),l_importe.PERCEPCION)
                  INTO   R_CUENTAS_CORRIENTES.CCT_IMP_ALI
                  FROM   DUAL;

                  SELECT DECODE(NVL(v_datos_iva.PERCEPCION,0), 0,NVL(V_DATOS_IVA.ALICUOTA,0),v_datos_iva.PERCEPCION)
                  INTO   R_CUENTAS_CORRIENTES.CCT_POR_ALI
                  FROM   DUAL;
				  V_INDICE_CTT := V_INDICE_CTT + 1;
                  V_CUENTA_CORRIENTE(V_INDICE_CTT)       := R_CUENTAS_CORRIENTES;
               END;
            END IF;
         ELSE
            BEGIN
               V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO + 1;
               V_CONCEPTO := 'Capital';
               R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO := V_INC_CCT_MOVIMIENTO;
               R_CUENTAS_CORRIENTES.CCT_SER_CODIGO    := 640109;
               R_CUENTAS_CORRIENTES.CCT_IMP_DEBE      := NVL(V_CUOTA_SIN_INTERES,0);
               R_CUENTAS_CORRIENTES.CCT_CONCEPTO      := NVL(V_CONCEPTO,'0');
               R_CUENTAS_CORRIENTES.CCT_IMP_IVA       := 0;
               R_CUENTAS_CORRIENTES.CCT_POR_IVA       := 0;
               R_CUENTAS_CORRIENTES.CCT_IMP_ALI       := 0;
               R_CUENTAS_CORRIENTES.CCT_POR_ALI       := 0;
			   V_INDICE_CTT := V_INDICE_CTT + 1;
               V_CUENTA_CORRIENTE(V_INDICE_CTT)       := R_CUENTAS_CORRIENTES;
            END;
            BEGIN
   	           V_INC_CCT_MOVIMIENTO := V_INC_CCT_MOVIMIENTO +1;
               l_importe := F_IVA_INVERSO(NVL(V_INTERES_CON_IVA,0),V_DATOS_IVA.IVA,V_DATOS_IVA.ALICUOTA,V_DATOS_IVA.PERCEPCION);
               V_CONCEPTO := 'Interes';
               R_CUENTAS_CORRIENTES.CCT_ID_MOVIMIENTO := V_INC_CCT_MOVIMIENTO;
               R_CUENTAS_CORRIENTES.CCT_SER_CODIGO    := 640106;
               R_CUENTAS_CORRIENTES.CCT_IMP_DEBE      := NVL(V_INTERES_CON_IVA,0);
               R_CUENTAS_CORRIENTES.CCT_CONCEPTO      := NVL(V_CONCEPTO,'0');
               SELECT DECODE(NVL(l_importe.PERCEPCION,0), 0,NVL(l_importe.ALICUOTA,0),l_importe.PERCEPCION)
               INTO   R_CUENTAS_CORRIENTES.CCT_IMP_ALI
               FROM   DUAL;

               SELECT DECODE(NVL(v_datos_iva.PERCEPCION,0), 0,NVL(V_DATOS_IVA.ALICUOTA,0),v_datos_iva.PERCEPCION)
               INTO   R_CUENTAS_CORRIENTES.CCT_POR_ALI
               FROM   DUAL;
			   V_INDICE_CTT := V_INDICE_CTT + 1;
               V_CUENTA_CORRIENTE(V_INDICE_CTT)       := R_CUENTAS_CORRIENTES;
            END;
         END IF;
      END;

   END LOOP;
   PKG_PLANES_PAGO.inserta_obligaciones(P_UPDATE_OBLIGACIONES,
                                        V_OBLIGACION,
 						                V_CUENTA_CORRIENTE);
   V_ACTUALIZACION := PKG_PLANES_PAGO.ACTUALI_OBLIGACION(P_CAB_PLANES.PPL_ID);
   IF V_ACTUALIZACION <> 'FINALIZO' THEN
      ROLLBACK;
   END IF;
   CORRIGEPP(p_cab_planes.ppl_id);

   RETURN V_RECAUDA;
END;

/**************************************************************************************/

FUNCTION GENERA_MONTO_CUOTAS(P_MONTO_CUOTA PKG_PLANES_ESPECIALES.R_MONTO_CUOTA,
                             P_TEM_CCT_ID  DTE_CUENTAS_CORRIENTES.TEM_CCT_ID%TYPE)
  RETURN PKG_PLANES_ESPECIALES.R_DATOS_M_CUOTA IS

   V_INTERES       NUMBER(17,10) := 0;
   V_INTERES_PESOS NUMBER(17,10) := 0;
   V_RETORNA_PLAN  PKG_PLANES_ESPECIALES.R_DATOS_M_CUOTA;
   V_DATOS_IVA     PKG_SERVICIOS_FIJOS.ivarec;
   L_IMPORTE_IVA   PKG_SERVICIOS_FIJOS.importerec;

   V_MONTO_RESTA_IVA         NUMBER(17,10);
   V_MONTO_RESTA_IVA_CUOTA   NUMBER(17,10);
   V_MONTO_RESTA_SIN_INTERES NUMBER(17,10);
   V_RESTA_INTERES           NUMBER(17,10);
   V_INTERES_PESOS_IVA       NUMBER(17,10);

BEGIN

   IF P_MONTO_CUOTA.V_POR_BONIF < 100 THEN

      -- parte de la fórmula, sin multiplicar por la deuda --
      V_INTERES                      := (P_MONTO_CUOTA.V_interes / ( 1 - POWER((1 + P_MONTO_CUOTA.V_interes),(-P_MONTO_CUOTA.V_CANT_CUOTAS))));

      -- total de la cuota (deuda / cant_cuotas)
      V_RETORNA_PLAN.V_UNA_CUOTA     := P_MONTO_CUOTA.V_total / P_MONTO_CUOTA.V_CANT_CUOTAS;

      -- monto de la cuota, el primer cálculo por la deuda --
      V_RETORNA_PLAN.V_UNA_INTERES   := P_MONTO_CUOTA.V_total * V_INTERES;

      -- interés por cuota: diferencia entre cuota con interes y cuota sin interés, multiplicado por la bonificación --
      V_INTERES_PESOS                := (V_RETORNA_PLAN.V_UNA_INTERES - V_RETORNA_PLAN.V_UNA_CUOTA ) * (1-(NVL(P_MONTO_CUOTA.V_POR_BONIF,0)/100));

	  V_INTERES_PESOS_IVA            := V_INTERES_PESOS;

      V_DATOS_IVA     				 := PKG_SERVICIOS_FIJOS.F_IMP_IVA(P_MONTO_CUOTA.V_TIPO_RESP,
				                                                      P_MONTO_CUOTA.V_SERVICIO,
 							                         			      SYSDATE);
      L_IMPORTE_IVA.IVA              := (V_DATOS_IVA.IVA        * V_INTERES_PESOS_IVA)/100;
      L_IMPORTE_IVA.ALICUOTA   	     := (V_DATOS_IVA.ALICUOTA   * V_INTERES_PESOS_IVA)/100;
      L_IMPORTE_IVA.PERCEPCION 	     := (V_DATOS_IVA.PERCEPCION * (V_INTERES_PESOS_IVA + L_IMPORTE_IVA.IVA))/100;
      V_RETORNA_PLAN.V_IMP_IVA    	 := L_IMPORTE_IVA.IVA;
      V_RETORNA_PLAN.V_IMP_ALI    	 := L_IMPORTE_IVA.ALICUOTA;
      V_RETORNA_PLAN.V_IMP_PERC   	 := L_IMPORTE_IVA.PERCEPCION;
      V_RETORNA_PLAN.V_IVA        	 := V_DATOS_IVA.IVA;
      V_RETORNA_PLAN.V_ALICUOTA   	 := V_DATOS_IVA.ALICUOTA;
      V_RETORNA_PLAN.V_PERCEPCION    := V_DATOS_IVA.PERCEPCION;
      V_RETORNA_PLAN.TOTAL_IVA_PESOS := L_IMPORTE_IVA.IVA          + L_IMPORTE_IVA.ALICUOTA + L_IMPORTE_IVA.PERCEPCION;
      --V_RETORNA_PLAN.V_INTERES_TOTAL := V_INTERES_PESOS            + V_RETORNA_PLAN.TOTAL_IVA_PESOS;
      V_RETORNA_PLAN.V_INTERES_TOTAL := V_RETORNA_PLAN.V_UNA_INTERES-V_RETORNA_PLAN.V_UNA_CUOTA;
      --V_RETORNA_PLAN.V_MONTO_CUOTA   := V_RETORNA_PLAN.V_UNA_CUOTA + V_RETORNA_PLAN.V_INTERES_TOTAL;
      V_RETORNA_PLAN.V_MONTO_CUOTA   := V_RETORNA_PLAN.V_UNA_INTERES+V_RETORNA_PLAN.TOTAL_IVA_PESOS;

   ELSE

   	  UPDATE DTE_CUENTAS_CORRIENTES
	  SET    TEM_CCT_MONTO = 0
	  WHERE  TEM_CCT_ID    = P_TEM_CCT_ID;

      V_INTERES                     := 0;
      V_RETORNA_PLAN.V_UNA_CUOTA    := P_MONTO_CUOTA.V_total / P_MONTO_CUOTA.V_CANT_CUOTAS;
      V_RETORNA_PLAN.V_UNA_INTERES  := P_MONTO_CUOTA.V_total;
      V_INTERES_PESOS               := (V_RETORNA_PLAN.V_UNA_INTERES - V_RETORNA_PLAN.V_UNA_CUOTA ) * (1-(NVL(P_MONTO_CUOTA.V_POR_BONIF,0)/100));
      V_DATOS_IVA     				:= PKG_SERVICIOS_FIJOS.F_IMP_IVA(P_MONTO_CUOTA.V_TIPO_RESP,
				                                                     P_MONTO_CUOTA.V_SERVICIO,
 							                        			     SYSDATE);
      L_IMPORTE_IVA.IVA              := 0;
      L_IMPORTE_IVA.ALICUOTA   	     := 0;
      L_IMPORTE_IVA.PERCEPCION 	     := 0;
      V_RETORNA_PLAN.V_IMP_IVA    	 := 0;
      V_RETORNA_PLAN.V_IMP_ALI    	 := 0;
      V_RETORNA_PLAN.V_IMP_PERC   	 := 0;
      V_RETORNA_PLAN.V_IVA        	 := 0;
      V_RETORNA_PLAN.V_ALICUOTA   	 := 0;
      V_RETORNA_PLAN.V_PERCEPCION    := 0;
      V_RETORNA_PLAN.TOTAL_IVA_PESOS := 0;
      V_RETORNA_PLAN.V_INTERES_TOTAL := 0;
      V_RETORNA_PLAN.V_MONTO_CUOTA   := V_RETORNA_PLAN.V_UNA_CUOTA;

   END IF;

   RETURN V_RETORNA_PLAN;

END;

/**************************************************************************************/
FUNCTION genera_fecha_cuota(p_fecha_inicial DATE, p_cant_dia NUMBER)
                            RETURN DATE IS

   p_fecha_cuota DATE;
   v_feriado     NUMBER;
   v_anio        NUMBER(4);

BEGIN

   p_fecha_cuota := p_fecha_inicial + p_cant_dia;

   LOOP

      BEGIN
         SELECT cae_estado
         INTO   v_feriado
         FROM   CALENDARIOS
         WHERE  cae_fecha_calendario = p_fecha_cuota;
      EXCEPTION WHEN NO_DATA_FOUND THEN
      	       v_anio := TO_NUMBER(TO_CHAR(p_fecha_cuota,'rrrr'));
               INSERTAR_CALENDARIO(v_anio);
               BEGIN
                  SELECT cae_estado
                  INTO   v_feriado
                  FROM   CALENDARIOS
                  WHERE  cae_fecha_calendario = p_fecha_cuota;
               EXCEPTION WHEN NO_DATA_FOUND THEN
               	              NULL;
               END;
      END;
      IF v_feriado = 1 THEN
         EXIT;
      ELSE
         p_fecha_cuota := p_fecha_cuota + 1;
      END IF;

   END LOOP;

   RETURN p_fecha_cuota;

END;

/**************************************************************************************/

PROCEDURE ACTUALIZA_TEMPORAL_CON_IVA(P_MONTO_ACTUALIZA NUMBER,
                                     P_TEM_CCT_ID DTE_CUENTAS_CORRIENTES.TEM_CCT_ID%TYPE,
									 p_prueba OUT VARCHAR2) IS
   CURSOR c_servicio IS SELECT TEM_CCT_SERVICIO, TEM_CCT_OBL_ID, TEM_CCT_MONTO
                        FROM   DTE_CUENTAS_CORRIENTES, SERVICIOS
                        WHERE  TEM_CCT_SERVICIO = SER_CODIGO
                          AND  SER_CALCULA_IVA = 'S'
                          AND  TEM_CALCULA = 'S'
                          AND  TEM_CCT_MONTO > 0
                          AND  tem_CCT_ID = P_TEM_CCT_ID
                        ORDER BY TEM_CCT_SERVICIO;
   V_MONTO_ACTUALIZA NUMBER(15,2);
   BEGIN
   V_MONTO_ACTUALIZA := P_MONTO_ACTUALIZA;
   FOR R_SERVICIOS IN C_SERVICIO LOOP
      IF V_MONTO_ACTUALIZA > R_SERVICIOS.TEM_CCT_MONTO THEN
      	 V_MONTO_ACTUALIZA := V_MONTO_ACTUALIZA - R_SERVICIOS.TEM_CCT_MONTO;
      	 UPDATE DTE_CUENTAS_CORRIENTES
      	    SET TEM_CCT_MONTO = 0
      	  WHERE TEM_CCT_SERVICIO = R_SERVICIOS.TEM_CCT_SERVICIO
      	    AND TEM_CCT_OBL_ID   = R_SERVICIOS.TEM_CCT_OBL_ID
      	    AND TEM_CCT_ID       = P_TEM_CCT_ID
      	    AND TEM_CALCULA = 'S';
      ELSE
      	 UPDATE DTE_CUENTAS_CORRIENTES
      	    SET TEM_CCT_MONTO = R_SERVICIOS.TEM_CCT_MONTO - V_MONTO_ACTUALIZA
      	  WHERE TEM_CCT_SERVICIO = R_SERVICIOS.TEM_CCT_SERVICIO
      	    AND TEM_CCT_OBL_ID   = R_SERVICIOS.TEM_CCT_OBL_ID
      	    AND TEM_CCT_ID       = P_TEM_CCT_ID
      	    AND TEM_CALCULA = 'S';
      	 V_MONTO_ACTUALIZA := 0;
      	 EXIT;
      END IF;
   END LOOP;

   EXCEPTION WHEN OTHERS THEN
             	  P_PRUEBA := 'ERROR  '||SQLERRM;
   END;

/**************************************************************************************/

   PROCEDURE ACTUALIZA_TEMPORAL_SIN_IVA(P_MONTO_ACTUALIZA NUMBER,
                                        P_TEM_CCT_ID DTE_CUENTAS_CORRIENTES.TEM_CCT_ID%TYPE,
										P_SERVICIO   CUENTAS_CORRIENTES.CCT_SER_CODIGO%TYPE,
										P_PRUEBA OUT VARCHAR2) IS

   CURSOR c_servicio IS SELECT TEM_CCT_SERVICIO, TEM_CCT_OBL_ID, TEM_CCT_MONTO
                        FROM   DTE_CUENTAS_CORRIENTES, SERVICIOS
                        WHERE  TEM_CCT_SERVICIO = SER_CODIGO
                          AND  SER_CALCULA_IVA = 'N'
                          AND  TEM_CALCULA = 'S'
                          AND  TEM_CCT_MONTO > 0
                          AND  TEM_CCT_SERVICIO = P_SERVICIO
                          AND  tem_CCT_ID = P_TEM_CCT_ID;
   V_MONTO_ACTUALIZA NUMBER(15,2);
   aaa NUMBER(15,2) := 0;

   BEGIN

   V_MONTO_ACTUALIZA := P_MONTO_ACTUALIZA;

   FOR R_SERVICIOS IN C_SERVICIO LOOP

      IF V_MONTO_ACTUALIZA > R_SERVICIOS.TEM_CCT_MONTO THEN
      	 V_MONTO_ACTUALIZA := V_MONTO_ACTUALIZA - R_SERVICIOS.TEM_CCT_MONTO;
      	 UPDATE DTE_CUENTAS_CORRIENTES
      	    SET TEM_CCT_MONTO = 0
      	  WHERE TEM_CCT_SERVICIO = R_SERVICIOS.TEM_CCT_SERVICIO
      	    AND TEM_CCT_OBL_ID   = R_SERVICIOS.TEM_CCT_OBL_ID
      	    AND TEM_CCT_ID       = P_TEM_CCT_ID
      	    AND TEM_CALCULA = 'S';

      ELSE

      	 UPDATE DTE_CUENTAS_CORRIENTES
      	    SET TEM_CCT_MONTO    = TEM_CCT_MONTO - V_MONTO_ACTUALIZA
      	  WHERE TEM_CCT_SERVICIO = R_SERVICIOS.TEM_CCT_SERVICIO
      	    AND TEM_CCT_OBL_ID   = R_SERVICIOS.TEM_CCT_OBL_ID
      	    AND TEM_CCT_ID       = P_TEM_CCT_ID
      	    AND TEM_CCT_MONTO > 0
      	    AND TEM_CALCULA = 'S';
      	 V_MONTO_ACTUALIZA := 0;
      	 EXIT;

      END IF;

   END LOOP;

   EXCEPTION WHEN OTHERS THEN
             P_PRUEBA := 'SIN IVA '||SQLERRM;
   END;



/**************************************************************************************/
FUNCTION f_recalcula_quita(p_cuenta INMUEBLES.inm_cuenta%TYPE,
                         p_mpp_id PLANES_ESPECIALES.PPE_MPP_ID%TYPE,
                         p_fecha_plan DATE)
                         RETURN PLANES_PAGO.PPL_QUITA%TYPE IS

   R_MODELO_PLAN MODELOS_PLANES_PAGO%ROWTYPE;
   v_fec_ini_vto DATE;
   l_quita_acumulado PLANES_PAGO.PPL_QUITA%TYPE;
   v_ult_modif DATE;
BEGIN
      -- PEDIDO 2642490 - SROMERO determina si puede tomar lo que está actualmente en el modelo o debe ir al histórico --
      BEGIN
         SELECT NVL(mpp_fec_mod,p_fecha_plan) INTO v_ult_modif
         FROM MODELOS_PLANES_PAGO
         WHERE MPP_ID = p_mpp_id;
      END;

      IF NVL(v_ult_modif,TO_DATE('01011995','ddmmrrrr')) <= p_fecha_plan THEN  -- no se ha modificado luego de la generación del plan --
          SELECT *
            INTO R_MODELO_PLAN
            FROM MODELOS_PLANES_PAGO
           WHERE MPP_ID = p_mpp_id;
      ELSE  -- se ha modificado luego de la generación del plan, entonces pasa por el histórico --
        BEGIN
          SELECT HMP_MPP_ID, HMP_MPP_MIN_CNT_CUOTAS, HMP_MPP_FEC_INICIO_VIGENCIA, HMP_MPP_IMP_MIN_DEUDA,
            HMP_MPP_CNT_DIAS_CADUCIDAD, HMP_MPP_TIPO_PLAN_PAGO, HMP_MPP_MODALIDAD, HMP_MPP_USR_ALTA,
            HMP_MPP_FEC_ALTA, HMP_MPP_TCL_TIPO, HMP_MPP_MAX_CNT_CUOTAS, HMP_MPP_MIN_IMP_CUOTA,
            HMP_MPP_MAX_BONIF_RECARGO, HMP_MPP_MAX_BONIF_INTERES, HMP_MPP_TASA_INTERES, HMP_MPP_TASA_RECARGO,
            HMP_MPP_FEC_FIN_VIGENCIA, HMP_MPP_USR_BAJA, HMP_MPP_FEC_BAJA, HMP_MPP_USR_MOD, HMP_MPP_FEC_MOD,
            HMP_MPP_QUITA, HMP_MPP_MONTO_INICIAL, HMP_MPP_IMP_MAX_DEUDA, HMP_MPP_CON_PLAN_ACTIVO,
            HMP_MPP_DEUDA_FEC_DESDE, HMP_MPP_DEUDA_FEC_HASTA, HMP_MPP_INCLUYE_RESTO, HMP_MPP_CON_PAGO_NO_ING,
            HMP_MPP_CON_PEND_APLIC, HMP_MPP_EN_CONC_Y_QUIEBRA, HMP_MPP_EN_GEST_JUDICIAL, HMP_MPP_TERCERIZADOS,
            HMP_MPP_FEC_LIMITE_VIG, HMP_MPP_GRANDES_CLIENTES, HMP_MPP_VIGENCIA, HMP_MPP_FECHA_HASTA,
            HMP_MPP_CUOTA_QUITA, HMP_MPP_MAO_CODIGO, HMP_MPP_DESCRIPCION
            INTO R_MODELO_PLAN
            FROM HIS_MODELOS_PLANES_PAGO A
           WHERE HMP_MPP_ID = p_mpp_id
             AND HMP_ID = (SELECT MIN(B.HMP_ID) FROM HIS_MODELOS_PLANES_PAGO B
                                 WHERE B.HMP_MPP_ID = P_MPP_ID AND B.HMP_FEC_ALTA >= p_fecha_plan);
          EXCEPTION WHEN NO_DATA_FOUND THEN -- casos con cambios pero todavía sin histórico --
               BEGIN -- modelo -
                 SELECT *
                 INTO R_MODELO_PLAN
                 FROM MODELOS_PLANES_PAGO
                 WHERE MPP_ID = p_mpp_id;
               END;
        END;
      END IF;

      BEGIN
         SELECT MAX(NVL(mpd_fec_mod,p_fecha_plan)) INTO v_ult_modif
         FROM DETALLE_MODELO_PLAN
         WHERE mpd_MPP_ID = p_mpp_id
           AND mpd_fec_alta <= p_fecha_plan
           AND NVL(mpd_fec_baja,p_fecha_plan) >= p_fecha_plan;
      END;
      -- PEDIDO 2642490 - SROMERO
      IF NVL(v_ult_modif,TO_DATE('01011995','ddmmrrrr')) <= p_fecha_plan THEN  -- no se ha modificado luego de la generación del plan --
       BEGIN
         SELECT MIN(ADD_MONTHS(p_fecha_plan,TRUNC(mpd_quita)*(-1))) INTO v_fec_ini_vto
    	 FROM DETALLE_MODELO_PLAN
    	 WHERE mpd_mpp_id = p_mpp_id
    	   AND mpd_tipo_quita = 'T';
       EXCEPTION WHEN NO_DATA_FOUND THEN v_fec_ini_vto := TO_DATE('01011995','ddmmrrrr');
       END;
      ELSE  -- se ha modificado luego de la generación del plan, entonces pasa por el histórico --
       BEGIN
         SELECT MIN(ADD_MONTHS(p_fecha_plan,TRUNC(hdm_mpd_quita)*(-1))) INTO v_fec_ini_vto
    	 FROM HIS_DETALLE_MODELO_PLAN
    	 WHERE hdm_id = (SELECT MIN(hdm_id) FROM HIS_DETALLE_MODELO_PLAN
           WHERE hdm_mpd_mpp_id = p_mpp_id
    	   AND hdm_mpd_tipo_quita = 'T'
           AND hdm_mpd_fec_alta >= p_fecha_plan);
       EXCEPTION WHEN NO_DATA_FOUND THEN v_fec_ini_vto := TO_DATE('01011995','ddmmrrrr');
       END;
      END IF;

	   -- Obtiene el monto de la quita
       BEGIN
    	  SELECT SUM(NVL(obl_saldo,0)) INTO l_quita_acumulado
           		 FROM OBLIGACIONES
                WHERE obl_cuenta = p_cuenta
                  AND obl_estado = 15
                  AND TRUNC(obl_fec_vencimiento) <= v_fec_ini_vto
                  AND obl_se = 'N'
                  AND obl_pef_anio >= 1995
                  AND obl_tpc_codigo > 79
                  AND obl_saldo > 0 AND obl_boleta_deuda IS NULL
                  AND obl_ppl_id IS NULL
                  AND obl_tpc_codigo <> 81
                  AND (
                         (
                          TRUNC(obl_fec_VENCIMIENTO) BETWEEN
                    		        NVL(R_MODELO_PLAN.mpp_deuda_fec_desde,TRUNC(obl_fec_vencimiento)) AND
                    				NVL(
                    				    DECODE(
                    					       R_MODELO_PLAN.mpp_fecha_hasta,'FGP',SYSDATE,
                    			  	  		   TO_DATE(
                    						   		   NVL(
                    								   	   R_MODELO_PLAN.mpp_fecha_hasta,TO_CHAR(obl_Fec_vencimiento,'dd/mm/rrrr')
                    									  ),
                    						           'dd/mm/rrrr'
                    								  )
                    	  				      ),
                    				    TRUNC(obl_fec_vencimiento)
                    				   )
                    			   )/*
                    			   OR
                    			   (v_tipo_modelo = 'R')*/
                    			  );

           IF l_quita_acumulado IS NULL THEN
		       l_quita_acumulado := 0;
		   END IF;
       EXCEPTION
	       WHEN NO_DATA_FOUND THEN
		       l_quita_acumulado := 0;
       END;

   RETURN l_quita_acumulado;

 END;

/**************************************************************************************/

END PKG_PLANES_ESPECIALES;
/

