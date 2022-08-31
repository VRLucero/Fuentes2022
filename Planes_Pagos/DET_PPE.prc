Create or replace procedure det_ppe(p_cuenta varchar2, p_via number, p_importe number, p_salida varchar2 ) is  
/* 
   -- Desarrollo :  VLUCERO    Julio-Agosto 2022   -- 
*/ 
l_archivo UTL_FILE.file_type; 
G_linea    varchar2(4000); 
G_aux      varchar2(4000);
l_rec_id    number; 
l_ppl_id    number;
l_inm_id    number;
cTex        Varchar2(14000);
cPos        Number;
cRet        Varchar2(14000); 
Begin
  l_archivo := UTL_FILE.fopen('REPORTES', p_Salida||'.txt', 'A');
   UTL_FILE.put_line( l_archivo, 'REC_ID;INM_CUENTA;INM_ID;REC_OBL_ID;REC_IMP_COBRADO;REC_NRO_COMPROB;;OBL_ID;OBL_CLI_ID;PEF_ANIO;PEF_PERIODO;OBL_NRO_CUTA;OBL_IMP_ORIG;OBL_IMP_NETO;OBL_ESTADO;OBL_FEC_VTO;OBL_FEC_APLICAC;'||
                                 'CCT_ID_MOV;CCT_SER_CODIGO;CCT_IMP_DEBE;CCT_IMP_HABER;CCT_IMP_IVA;CCT_IMP_ALI;;PPL_ID;PPL_MPP_ID;PPL_DEU_HIST;PPL_RECARGOS; PPL_INTERESES;PPL_CUOTAS;PPL_IMP_CUOTA;PPL_1ER_VTO;PPL_FEC_CADUCIDAD;'||
                                 'PPE_ID;PPE_DEU_HIST;PPE_RECARGOS;PPE_INTERESES;PPE_CNT_CUOTAS;PPE_IMP_CUOTA;PPE_FECHA;PPE_1ER_VTO;PPE_FEC_CADUCIDAD;PPE_PPL_ID');
   
  Select rec_id , 
         lPad(to_char(rec_id),10)     ||';'||  
         ''''||lPad(rec_Cuenta,14)    ||';'||
         lPad(to_char(rec_inm_id),10) ||';'||
         lPad(to_char(rec_obl_id),10) ||';'||
         lPad(to_char(rec_imp_cobrado,'99999990D00'),15) ||';'||
         lPad(to_char(rec_nro_factura),12)  
  into l_Rec_id , g_linea    
  from Manantial.recaudaciones 
  where  rec_cuenta = p_cuenta 
  and    rec_vco_codigo = p_via  
  and    trunc(rec_fec_alta)  = trunc(sysdate)
  --and    trunc(rec_fec_alta)  = to_date('29072022','ddmmrrrr') 
  and    rec_imp_cobrado = p_importe ;
  ------- Cuota del plan de pago 
   Select obl_ppl_id ,obl_con_inm_id 
      into l_ppl_id  , l_inm_id     
      From   Manantial.obligaciones 
      where obl_id in (Select cct_obl_id  from  cuentas_corrientes 
                      where cct_rec_id = l_Rec_id
                      and   cct_tipo_movimiento = 8) ;                      
  Select ';'||lPad(to_char(obl_id),10)   ||';'|| 
         lPad(to_char(obl_Cli_id),10)    ||';'||
         lPad(to_char(obl_pef_anio),5)   ||';'||
         lPad(to_char(obl_pef_periodo),2)||';'||
         to_char(obl_Cuota_plan,'99')    ||';'||
         lPad(to_char(obl_imp_original,'99999990D00'),15) ||';'||
         lPad(to_char(obl_imp_neto    ,'99999990D00'),15) ||';'||
         lPad(to_char(obl_estado ),5)    ||';'||
         lPad(to_char(obl_fec_vencimiento,'dd/mm/rrrr'),10) ||';'||
         lPad(to_char(obl_fec_aplicacion, 'dd/mm/rrrr'),10) ||';'
   into G_aux  
   From   obligaciones 
   where obl_id in (Select cct_obl_id  from  cuentas_corrientes 
   where cct_rec_id = l_Rec_id
   and   cct_tipo_movimiento = 8) ;
   g_Linea := g_Linea || ';' ||G_aux ;   
   ----- Cabecera del plan de pago  
   Select   ';;;;;;'||lPad(to_char(ppl_id),10) ||';'||
            lPad(to_char(ppl_mpp_id),10) ||';'||
            lPad(to_char(ppl_deuda_historica,'99999990D00'),15)||';'||
            lPad(to_char(ppl_monto_recargos,'99999990D00'),15) ||';'||
            lPad(to_char(ppl_imp_intereses,'99999990D00'),15)  ||';'||
            to_char(ppl_cnt_cuotas        ,'999')              ||';'||
            lPad(to_char(ppl_imp_cuota    ,'99999990D00'),15)  ||';'||
            lPad(to_char(ppl_primer_vto   ,'dd/mm/rrrr'),10)   ||';'||
            lPad(to_char(ppl_fec_caducidad,'dd/mm/rrrr'),10)
   into  G_aux 
   From planes_pago 
   where ppl_id =   l_ppl_id ; 
   g_Linea := g_Linea || lPad(';',160) ||G_aux ;
   ---- Plan de pago especial  
   Select  ';'|| lPad(to_char(ppe_id),10) ||';'||
                 lPad(to_char(ppe_deuda_historica,'99999990D00'),15) ||';'||
                 lPad(to_char(ppe_monto_recargos,'99999990D00'),15) ||';'||
                 lPad(to_char(ppe_imp_intereses,'99999990D00'),15) ||';'||
                 lPad(to_char(ppe_cnt_cuotas,'99'),5) ||';'||
                 lPad(to_char(ppe_imp_cuota,'99999990D00'),15) ||';'||
                 lPad(to_char(ppe_fecha,'dd/mm/rrrr'),10) ||';'||
                 lPad(to_char(ppe_primer_vto,'dd/mm/rrrr'),10) ||';'||
                 lPad(to_char(ppe_fec_caducidad,'dd/mm/rrrr'),10) ||';'||
                 lPad(to_char(ppe_ppl_id),10) 
      into   G_Aux   
      from planes_especiales 
      where ppe_ppl_id = l_ppl_id; 
   g_Linea := g_Linea || G_aux ;   
   UTL_FILE.put_line( l_archivo, G_Linea);
  
  -- Composicion cuota del plan  que se pago --- 
  For r in (
  Select   lPad(to_char(cct_id_movimiento),5) ||';'||
           lPad(to_char(cct_ser_codigo),10) ||';'||
           lPad(to_char(cct_imp_debe ,'99999990D00'),15) ||';'||
           lPad(to_char(cct_imp_haber,'99999990D00'),15) ||';'||
           lPad(to_char(cct_imp_iva  ,'99999990D00'),15) ||';'||
           lPad(to_char(cct_imp_ali  ,'99999990D00'),15) ||';'||
           ltrim(cct_concepto)   TEXTO   
  from cuentas_corrientes 
  where cct_obl_id in (
  Select cct_obl_id  from  cuentas_corrientes 
  where cct_rec_id = l_Rec_id
  and   cct_tipo_movimiento = 8)) loop
     g_Linea := rPad(r.TEXTO , 275 ) ;
     UTL_FILE.put_line( l_archivo, ';;;;;;;;;;;;;;;;;'||G_Linea||';'); 
  end loop;
  -------------------------------------------------------     
   cTex :=  det_recaudacion(l_Rec_id) ;   
   cPos := instr(cTex,chr(10) );    
   If  cPos > 0 Then
       cRet := substr(cTex,1,cPos-1);
       UTL_FILE.put_line( l_archivo, ';;;;;;;;;;;;;;;;;'||cRet);     
       While cPos > 0 Loop          
          cTex := ltrim(substr(cTex,cPos+1, length(cTex)));          
          cTex := substr(cTex,1,instr(cTex,' ',6))||';'||substr(cTex,instr(cTex,' ',6), length(cTex));
          cPos := instr(cTex,chr(10) );         
          If cPos = 0 then  
             cRet := substr(cTex,cPos+1, length(cTex));             
          Else 
             cRet := substr(cTex,1, cPos-1 );             
          End if;
          UTL_FILE.put_line( l_archivo, ';;;;;;;;;;;;;;;;;;'||cRet);    
       End loop; 
   Else     
      cRet := cTex;
      UTL_FILE.put_line( l_archivo, ';;;;;;;;;;;;;;;;;'||cRet);
   end if;
   For r in (Select   nov_ser_codigo, nov_descripcion , nov_tipo_novedad , sum(nov_imp_neto ) Importe_Neto 
             from   novedades_facturables 
             where nov_con_inm_id = l_inm_id 
             and   trunc(nov_fec_novedad) = trunc(sysdate)
             and   nov_imp_neto > 0  
             Group by nov_ser_codigo, nov_tipo_novedad, nov_descripcion ) Loop
       cRet := to_char(r.nov_tipo_novedad,'99') || ';' || to_char(r.nov_ser_codigo) || ';' || substr(r.nov_descripcion,1,70) || ';' || to_char(r.Importe_Neto,'9999990D00') ;
       UTL_FILE.put_line( l_archivo, ';;;;;;;;;;;;;;;;NOV_GENERADA;'||cRet);
   end loop; 
  UTL_FILE.put_line( l_archivo, ' ');
  UTL_FILE.fclose(l_Archivo);
End; 
 

/*

Select * from   obligaciones  where obl_ppl_id = 1783982

Select * from   cuentas_corrientes  where cct_obl_id = 833401532; 

Select  * from   obligaciones 
where obl_id in (Select cct_obl_id  from  cuentas_corrientes 
where cct_rec_id = 745084655
and   cct_tipo_movimiento = 8)  

Select * from planes_pago  
where ppl_id = 1783982

Select * from recaudaciones   where rec_id = 745084655 ; 

Select det_recaudacion(745084655) from dual   

DET_RECAUDACION(745084655)
Pago:28/07/22 Aplicada C.Plan 1783982      2867.09 Recargo 1005 Interes 463.81 Capital 1398.28
                                               Fac 318459603      1398.28


Select * from cuentas_corrientes 
where cct_obl_id in (
Select cct_obl_id  from  cuentas_corrientes 
where cct_rec_id = 745084655
and   cct_tipo_movimiento != 8)  



Select  * from planes_especiales 
where ppe_ppl_id = 1783982


Select * from  planes_especiales_cuotas
where ppc_ppe_id =   13489798

End; 
*/ 