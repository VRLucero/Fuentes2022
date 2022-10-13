

Declare 
 l_archivo UTL_FILE.file_type;
 l_Linea   varchar2(1000);  
Begin
   l_archivo := UTL_FILE.fopen('REPORTES', 'Det_Aplic_09.log', 'r');
   loop
     Begin  
        Utl_file.Get_Line(l_archivo, l_linea);
     Exception   when no_data_found Then
        dbms_output.put_line('** fin de archivo  ** ');  
        Exit ;    
     End ; 
     dbms_output.put_line(l_linea);     
     if substr(l_linea,1,9) = 'Procesado' then
       -- dbms_output.put_line( substr(l_linea,31,14) ||' ' || substr(l_linea,73,3) || ' '|| substr(l_linea,46,15) ||' ' || substr(l_linea,11,10) );
        Manantial.Det_ppe(p_cuenta => substr(l_linea,31,14), p_via=> substr(l_linea,73,3) , p_importe => to_number(substr(l_linea,46,15))  , p_fecha => to_date(substr(l_linea,11,10) ,'dd/mm/rrrr') , p_salida => 'Detalle_ppe' ) ; 
     end if;      
   end loop;    
end; 

 



Select * from    obligaciones  
where obl_ppl_id in 
(1795220 ,
1795217 ,
1795213 ,
1795102 ,
1795111 ,
1795112 ,
1795114 ,
1795117 ,
1795180 ,
1795156 ,
1795182 ,
1795201 ,
1795203 ,
1795204)
and obl_tpc_codigo in (8,9)
Order by    obl_ppl_id, obl_fec_vencimiento 


Select  * from   obligaciones 
where obl_id = 833175031

Select  * from recaudaciones 
where rec_cuenta = '12600131210006'
