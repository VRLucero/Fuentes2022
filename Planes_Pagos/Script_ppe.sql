
Select * from  recaudaciones_externas 
where  rep_id = 42553602

DECLARE 
  RetVal VARCHAR2(200);
BEGIN 
  RetVal :=MANANTIAL.PKG_RECAUDACIONES.RECP_0450;
  --rollback;    
END;
 




Select * from   recaudaciones  where rec_cuenta = '12200015800009'
Order by rec_id desc 

Select  * from cuentas_corrientes 
where cct_rec_id = 744662078


Select * from planes_especiales 
where  ppe_id = 13435313

Select  * from planes_pago where ppl_id = 1763599



SELECT rep_id, rep_fecha, LTRIM(RTRIM(SUBSTR(rep_registro, 26, 14))),LTRIM(RTRIM(SUBSTR(rep_registro, 62, 2))),  AA.*
FROM RECAUDACIONES_EXTERNAS AA 
WHERE --RIM(RTRIM(SUBSTR(rep_registro, 26, 14))) IN  ( '07300619620000')
rep_id >  42906096 - 5000 
--rep_id =  42553602 
and  LTRIM(RTRIM(SUBSTR(rep_registro, 62, 2))) = '43' 

-- Ajustar linea 767 y  828 en PKG_RECAUDACIONES   


DECLARE 
  RetVal VARCHAR2(200);
BEGIN 
  RetVal :=MANANTIAL.PKG_RECAUDACIONES.RECP_0450;
  --rollback;    
END;


Select * from   recaudaciones  where rec_cuenta = '07300221830005'
Order by rec_id desc 

Select  * from  cuentas_corrientes  where cct_rec_id = 744693680

Select * from  novedades_facturables 
where nov_con_inm_id = 300006162
Order by nov_id desc 

Select * from   obligaciones 
where obl_cuenta = '07300221830005'
Order by obl_id desc 


Select * from planes_pago 
where ppl_id = 1751000

Select * from planes_especiales
where ppe_ppl_id =1751000

Select * from planes_especiales
where ppe_id = 13418924
and   ppe_ppl_id is not null
--and   ppe_cnt_cuotas > 1 
Order by  ppe_id desc  
 
select   * from   PLANES_ESPECIALES_CUOTAS
where ppc_ppe_id = 13418924
Order by  ppc_ppe_id desc    

Select * from   REL_PPE_OBL
where rpo_ppe_id = 13418924

 SELECT * FROM  Manantial.OBLIGACIONES  
       WHERE obl_ppl_id = 1751000 
       AND    obl_tpc_codigo IN (8,9)
       AND    obl_estado = 15 
       AND    obl_saldo > 0        
       ORDER BY  obl_fec_vencimiento asc ; 
 
select * from  Cuentas_Corrientes  where cct_obl_id = 832365587

Select * from planes_convenios
where pco_ppl_id = 1763190
  
Select max(rep_id)  From recaudaciones_externas

  SELECT *
           FROM REMESAS                
          WHERE rem__fecha = to_date('11052022','ddmmrrrr') AND 
          rem__ere_codigo = 811