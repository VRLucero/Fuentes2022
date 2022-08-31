CREATE OR REPLACE FUNCTION det_recaudacion(p_rec_id number  ) RETURN varchar2 IS
/* 
   -- Desarrollo :  VLUCERO    Julio-Agosto 2022   -- 
*/ 
l_ret  Varchar2(14000) := '' ;
l_tmp  Varchar2(14000) := '' ;
HayPlan BOOLEAN := FALSE;
CURSOR cRec IS 
       SELECT *  FROM  Manantial.RECAUDACIONES  
       WHERE rec_id = p_rec_id ; 
rRec cRec%ROWTYPE;         
CURSOR  cAplic (p_id number) IS
   SELECT  Decode(obl_tpc_codigo,8,'C.Plan',
                                 9,'C.Plan',
                                81,'Rec.',
                                61,'C.Int',
                                69,'N.C',
                                89,'Fac',
                                88,'Fac',
                                86,'Fac',
                                80,'Fac',
                                to_char(obl_tpc_codigo)    
                  ) TIPO, 
           to_char(obl_nro_factura)  NRO_COMP , to_char(cct_imp_haber,'99999990D00') Importe   ,  obl_id, obl_tpc_codigo  
           FROM  Manantial.OBLIGACIONES , Manantial.CUENTAS_CORRIENTES 
           WHERE cct_rec_id = p_id
           AND   obl_id  = cct_obl_id  
           AND   cct_ser_codigo = 800088 
           ORDER BY cct_fec_generacion, cct_tipo_movimiento ;  
rApl cAplic%ROWTYPE;
CURSOR cReca(p_id number) IS
       SELECT  nov_id, nov_estado  ,(NOV_IMP_NETO+NOV_IMP_IVA_CF+NOV_IMP_IVA_EX+NOV_IMP_IVA_RI+NOV_IMP_IVA_RNI+NOV_IMP_IVA_MON+NOV_IMP_IVA_ALI+NOV_IMP_IVA_PER) nov_importe  
       FROM  Manantial.REL_REC_NOV , Manantial.NOVEDADES_FACTURABLES   
       WHERE  rrn_rec_id = p_id 
       AND    rrn_nov_id = nov_id ; 
rReca cReca%ROWTYPE;         
------------------------------------------------------------------
FUNCTION VeoFracciones(p_nov_id number) RETURN Varchar2 IS
lRta Varchar2(4000):= NULL;  
BEGIN 
   FOR  rImp IN (SELECT (NOV_IMP_NETO+NOV_IMP_IVA_CF+NOV_IMP_IVA_EX+NOV_IMP_IVA_RI+NOV_IMP_IVA_RNI+NOV_IMP_IVA_MON+NOV_IMP_IVA_ALI+NOV_IMP_IVA_PER) nov_importe  
                   FROM  Manantial.NOVEDADES_FACTURABLES  WHERE nov_nov_id = p_nov_id ) LOOP
        lRta := lRta || To_char(rImp.nov_importe , '9999990D00')||' +';             
   END LOOP;   
   IF lRta IS NULL THEN
      lRta := 'Sin detalle de apertura +'; 
   END IF; 
   RETURN(substr(lRta,1,length(lRta)-1));
END; 
FUNCTION veo_Apertura(p_nov_id number ) RETURN varchar2 IS
l_Rta  Varchar2(4000);
CURSOR cApertura IS 
       SELECT  nov_id, (NOV_IMP_NETO+NOV_IMP_IVA_CF+NOV_IMP_IVA_EX+NOV_IMP_IVA_RI+NOV_IMP_IVA_RNI+NOV_IMP_IVA_MON+NOV_IMP_IVA_ALI+NOV_IMP_IVA_PER
       ) nov_importe , nov_estado, decode(nov_estado,2,'Disponible',
                                                3,'Fraccionada',
                                                4,'Devuelta',
                                                5,'Tomada x Fact',
                                                11,'Incluida en Fac',
                                                15,'Incluida en Fac',
                                                52, 'Anulada',
                                                to_char(nov_estado) ) ESTADO
       FROM Manantial.NOVEDADES_FACTURABLES 
       WHERE nov_nov_id = p_nov_id
       ORDER BY nov_id ;
rApe cApertura%ROWTYPE; 
BEGIN
   l_Rta:='';
   FOR  rApe IN cApertura LOOP
       l_rta  := l_Rta ||' '|| to_char(rApe.nov_importe,'9999990D00') ||' '||  rApe.estado;
       IF rApe.nov_estado = 3 THEN
          l_rta  := l_Rta ||'::'|| veo_Apertura(rApe.nov_id );
       END IF; 
   END LOOP;  
   RETURN l_Rta;  
END; 
------------------------------------------------------------------
FUNCTION ver_acreditado(p__rec_id number ) RETURN Varchar2 IS
  l__ret varchar2(4000); 
  CURSOR cAcre IS 
   SELECT rrn_nov_id  NOV_ID, decode(nov_estado,2,'Disponible',
                                                3,'Fraccionada',
                                                4,'Devuelta',
                                                5,'Tomada x Fact',
                                                11,'Incluida en Fac',
                                                15,'Incluida en Fac',
                                                52, 'Anulada',
                                                to_char(nov_estado) ) ESTADO, NOV_ESTADO  ,
                                                (NOV_IMP_NETO+NOV_IMP_IVA_CF+NOV_IMP_IVA_EX+NOV_IMP_IVA_RI+NOV_IMP_IVA_RNI+NOV_IMP_IVA_MON+NOV_IMP_IVA_ALI+NOV_IMP_IVA_PER) nov_importe     
   FROM Manantial.REL_REC_NOV, Manantial.NOVEDADES_FACTURABLES 
   WHERE  rrn_rec_id = p__rec_id 
   AND    rrn_nov_id = nov_id ;
   rAcre cAcre%ROWTYPE;
   CURSOR cFac(p_nov_id number) IS
      SELECT  cct_imp_haber, obl_nro_factura  
      FROM  Manantial.CUENTAS_CORRIENTES, Manantial.OBLIGACIONES 
      WHERE cct_nov_id = p_nov_id 
      AND   cct_obl_id = obl_id ; 
   rFac cFac%ROWTYPE;      
BEGIN
   l__ret := '';
   FOR rAcre  IN cAcre LOOP
       --IF  rAcre.nov_estado = 3  THEN
       --   l__ret := l__ret ||' ' || Veo_apertura(rAcre.Nov_id);
       --ELSE  
          l__ret := l__ret ||' ' || rAcre.estado||' ' ||to_char(rAcre.nov_importe,'9999990D00')  ;
       --END IF; 
       IF  rAcre.nov_estado IN (11,15) THEN
           OPEN cFac(rAcre.Nov_id);
           FETCH cFac INTO rFac ; 
           IF  cFac%FOUND THEN
               l__ret := l__ret ||' ' || to_char(rFac.Obl_Nro_factura) ||' '||to_char(rFac.cct_imp_haber,'99999990D00')  ;
           END IF;           
       END IF;  
   END LOOP;
   RETURN(l__ret); 
END;
------------------------------------------------------------------    
BEGIN 
   OPEN  cRec; 
   FETCH cRec INTO rRec;
   IF cRec%NOTFOUND THEN
      l_Ret := 'No existe el REC_ID  indicado';
   ELSIF  rRec.Rec_erc_codigo IN ( 1, 6 )  THEN
          l_Ret := 'Pago:'||to_char(rRec.rec_fecha, 'dd/mm/YY')||' Aplicada';
          FOR rApl IN cAplic(rRec.Rec_Id) LOOP              
              IF rApl.obl_tpc_codigo IN (8,9)  THEN
                 HayPlan:= TRUE; 
                 l_Ret  := l_Ret || ' '||rApl.TIPO||' '||rApl.NRO_COMP||' '|| rApl.Importe  ;
                 FOR rLinea IN ( SELECT  decode(cct_ser_codigo,640106,'Interes',
                                                               640108,'Recargo',
                                                               780004,'Multas','Capital'
                                               ) Concepto, cct_imp_debe  FROM Manantial.CUENTAS_CORRIENTES WHERE cct_obl_id =rApl.obl_id AND cct_imp_debe > 0 ) LOOP
                     l_Ret  := l_Ret||' '||rLinea.Concepto||' '||to_char(rLinea.cct_imp_debe)  ; 
                 END LOOP;  
                 l_Ret  := l_Ret || chr(10) ;
              ELSE 
                 IF HayPlan THEN
                    l_Ret  := l_Ret || '                                               '||rApl.TIPO||' '||rApl.NRO_COMP||' '|| rApl.Importe || chr(10) ; 
                 ELSE 
                    l_Ret  := l_Ret || ' '||rApl.TIPO||' '||rApl.NRO_COMP||' '|| rApl.Importe || chr(10) ;
                 END IF; 
              END IF;              
          END LOOP;  
          IF l_Ret = 'Pago:'||to_char(rRec.rec_fecha, 'dd/mm/YY')||' Aplicada' THEN   -- No se aplico directo. Se trata como ACREDITADO primero             
             l_tmp:=  ver_acreditado(rRec.Rec_Id) ;
             IF length(l_Tmp) > 1 THEN        
                l_Ret:= 'Pago:'||to_char(rRec.rec_fecha, 'dd/mm/YY')||' Acredit->Aplic ' || l_tmp ;
                l_tmp := '';
             ELSE 
                l_Ret:= 'Pago:'||to_char(rRec.rec_fecha, 'dd/mm/YY')||' SIN Seguimiento' ;
             END IF;
          ELSE 
             l_Ret  := substr(l_Ret,1, length(l_Ret)-1) ;    
          END IF; 
   ELSIF  rRec.Rec_erc_codigo = 2 THEN          
          l_Ret := 'Pago:'||to_char(rRec.rec_fecha, 'dd/mm/YY')||' Acredit';
          l_tmp := ver_acreditado(rRec.Rec_Id) ;
          IF length(l_tmp) > 1  THEN
             l_Ret := l_Ret ||' c/Uso Parcial '||l_tmp;
             l_tmp := ''; 
          ELSE 
             l_Ret := l_Ret ||' Disponible ';
          END IF;              
   ELSIF  rRec.Rec_erc_codigo = 3 THEN
          l_Ret := 'Pago:'||to_char(rRec.rec_fecha, 'dd/mm/YY')||' Pdte';
   ELSIF  rRec.Rec_erc_codigo = 7 THEN   
          l_Ret := 'Pago:'||to_char(rRec.rec_fecha, 'dd/mm/YY')||' Fracc';
          OPEN cReca(rRec.Rec_id);
          FETCH  cReca  INTO rReca; 
          IF  cReca%FOUND THEN
              IF  rReca.nov_estado = 3 THEN                  
                 l_tmp := VeoFracciones(rReca.nov_id);
              ELSIF rReca.nov_estado = 2 THEN
                 l_tmp := ' Disponible '||to_char(rReca.nov_importe,'9999990D00');
              ELSE    
                 l_tmp := 'nov_estado =' || to_char(rReca.nov_estado) || ' ' ||to_char(rReca.nov_importe,'9999990D00'); 
              END IF;
              l_Ret := l_Ret || l_tmp;                        
          ELSE 
              l_Ret := l_Ret || 'Sin Seguimiento';
          END IF;                   
   ELSE
          l_Ret := 'Pago:'||to_char(rRec.rec_fecha, 'dd/mm/YY')||' Estado '||to_char(rRec.Rec_erc_codigo);
   END IF;     
   RETURN (l_Ret);  
END;  

