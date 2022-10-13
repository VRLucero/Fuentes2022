
select max(obl_id) from    obligaciones ; 

832401802  ;

Select * from   obligaciones 
where obl_id > 832401802



Select distinct ter_descripcion  
from  tercerizaciones 
where  ter_fec_baja is null 


/*
  Filtro los registros  que en el cmpo TER_DESCRIPCION, tengan una cadena que comience 
  con "J" o "C" mayusculas, y esten seguidas de al menos dos digitos (pudiendo tener mas de dos) y esten seguidas de un espacio 
  antes de  cualquier otra cadena . 
*/ 

Select  REGEXP_SUBSTR(ter_descripcion,'^(J[0-9]{2,}|C[0-9]{2,})[[:space:]]*') CADENA_PARA_FILTRAR,
        REGEXP_SUBSTR(ter_descripcion,'^(C[0-9]{2,})*') EJEMPLO2,   -- Cadenas que inician con C y tienen al menos dos caracteres numericos (0..9) y luego cualquier cantidad de caracteres 
        REGEXP_SUBSTR(ter_descripcion,'^(C[0-9]{2,})[[:space:]]{2}') EJEMPLO3,   -- Idem anterior, pero con 2 espacios despues de los digitos        
        ter_descripcion  , ges_descripcion 
from  Manantial.tercerizaciones , gestores  
where  ter_fec_baja is null
--and   ter_descripcion = 'JOO5 FAJARDO' 
and   ter_ges_codigo = ges_codigo  (+)  



JOO5 FAJARDO



Select  REGEXP_SUBSTR(ter_descripcion,'^(J[0-9]{2,}|C[0-9]{2,})[[:space:]]*') CADENA_PARA_FILTRAR,
ter_descripcion  , ges_descripcion
from  Manantial.tercerizaciones  
left join Manantial.gestores  on  ter_ges_codigo = ges_codigo    
where  ter_fec_baja is null


--  gestores  internos  


Select  TO_CHAR(ter_codigo, '9990')  column1, substr(ter_descripcion,1,25)  column2,  NULL column3, NULL column4, NULL column5  
        from  Manantial.tercerizaciones  T
        left join Manantial.gestores  on  ter_ges_codigo = ges_codigo    
        where  ter_fec_baja is null
        and    ges_descripcion = 'GESTOR INTERNO'
        and    exists ( Select 1 from Manantial.cartera_bacup cb 
                        where   t.ter_codigo = cb.ter_codigo  
                        and     cb.fec_baja is null   )
        Order by  ter_codigo


Select  column1 ,   column2,  NULL column3, NULL column4, NULL column5
From (
Select  '001'  column1, 'TODOS LOS GESTORES INTERNOS' column2  from dual 
Union all 
Select  ltrim(TO_CHAR(ter_codigo, '9990'))  column1, rpad(substr(ter_descripcion,1,25),25)  column2   
from  Manantial.tercerizaciones  T
left join Manantial.gestores  on  ter_ges_codigo = ges_codigo    
where  ter_fec_baja is null
and    ges_descripcion = 'GESTOR INTERNO'
and    exists ( Select 1 from Manantial.cartera_bacup cb 
                where   t.ter_codigo = cb.ter_codigo  
                and     cb.fec_baja is null   )
)
Order by  1                 





Select inm_cuenta, ter_codigo   
from  cartera_bacup
where   1=1 --inm_cuenta  =  :p_cuenta  
and     fec_baja is null   
and     ter_codigo in (Select  ter_codigo  
from  Manantial.tercerizaciones  T
left join Manantial.gestores  on  ter_ges_codigo = ges_codigo    
where  ter_fec_baja is null
and    ges_descripcion = 'GESTOR INTERNO')
and   inm_cuenta like '059%'  

Select * from  cartera_bacup
where   ter_codigo  = 121
and     fec_baja is null     

  

--  gestores  pre_judiciales 

Select  REGEXP_SUBSTR(ter_descripcion,'^(J[0-9]{2,}|C[0-9]{2,})[[:space:]]*') CADENA_PARA_FILTRAR,
ter_descripcion  , ges_descripcion
from  Manantial.tercerizaciones  
left join Manantial.gestores  on  ter_ges_codigo = ges_codigo    
where  ter_fec_baja is null
and   REGEXP_SUBSTR(ter_descripcion,'^C[0-9]{2,}[[:space:]]*')  <> ' ' 
  


Select to_char(ges_codigo,'9999')  column1 , substr(ges_descripcion,1,25) column2 , to_char(count( *),'9999') column3, NULL column4, NULL column5 
from  Manantial.gestores  
left join Manantial.tercerizaciones T on  ter_ges_codigo = ges_codigo    
where  ter_fec_baja is null
and    ges_fec_baja is null  
and   REGEXP_SUBSTR(ter_descripcion,'^C[0-9]{2,}[[:space:]]*')  <> ' '
and    exists ( Select 1 from Manantial.cartera_bacup cb 
                where   t.ter_codigo = cb.ter_codigo  
                and     cb.fec_baja is null   )
group by ges_codigo, ges_descripcion              
                 



Select   inm_cuenta   from   cartera_bacup
where  fec_baja is null 
and    ter_codigo in (
Select  ter_codigo     From  Manantial.tercerizaciones
left join MANANTIAL.GESTORES  On  ter_ges_codigo = ges_codigo
where  ter_fec_baja is null
and   ges_codigo = :ges_codigo   -- 20   Pre-judiciales  
)
  

 



--  gestores  judiciales 

Select  REGEXP_SUBSTR(ter_descripcion,'^(J[0-9]{2,}|C[0-9]{2,})[[:space:]]*') CADENA_PARA_FILTRAR,
ter_descripcion  , ges_descripcion
from  Manantial.tercerizaciones  
left join Manantial.gestores  on  ter_ges_codigo = ges_codigo    
where  ter_fec_baja is null
and   REGEXP_SUBSTR(ter_descripcion,'^J[0-9]{2,}[[:space:]]*')  <> ' ' 
 


Select to_char(ges_codigo,'9999')  column1 , substr(ges_descripcion,1,25) column2 , to_char(count( *),'9999') column3, NULL column4, NULL column5 
from  Manantial.gestores  
left join Manantial.tercerizaciones T on  ter_ges_codigo = ges_codigo    
where  ter_fec_baja is null
and    ges_fec_baja is null  
and   REGEXP_SUBSTR(ter_descripcion,'^J[0-9]{2,}[[:space:]]*')  <> ' '
and    exists ( Select 1 from Manantial.cartera_bacup cb 
                where   t.ter_codigo = cb.ter_codigo  
                and     cb.fec_baja is null   )
group by ges_codigo, ges_descripcion              



Select to_char(ges_codigo,''9999'')  column1 , rPad(substr(ges_descripcion,1,25),25) ||''(''|| to_char(count( *),''9999'')||'')'' column2 , NULL  column3, NULL column4, NULL column5

Select * from (
Select  '001' column1, 'TODOS LOS GESTORES (999)' column2  from dual  
Union all  
Select ltrim(to_char(ges_codigo,'9999'))  column1 , rPad(substr(ges_descripcion,1,25),25) ||'('|| to_char(count( *),'9999')||')' column2
        from  Manantial.gestores  
        left join Manantial.tercerizaciones T on  ter_ges_codigo = ges_codigo    
        where  ter_fec_baja is null
        and    ges_fec_baja is null  
        and   REGEXP_SUBSTR(ter_descripcion,'^J[0-9]{2,}[[:space:]]*')  <> ' '
        and    exists ( Select 1 from Manantial.cartera_bacup cb 
                        where   t.ter_codigo = cb.ter_codigo  
                        and     cb.fec_baja is null   )                        
        group by ges_codigo, ges_descripcion
         )


Select   inm_cuenta   from   cartera_bacup
where  fec_baja is null 
and    ter_codigo in (
Select  ter_codigo     From  Manantial.tercerizaciones
left join MANANTIAL.GESTORES  On  ter_ges_codigo = ges_codigo
where  ter_fec_baja is null
and   ges_codigo = :ges_codigo   -- 27  Judiciales  
)  order by inm_cuenta 
  



Select to_char(ges_codigo,'9999')  column1 , substr(ges_descripcion,1,25) column2 , to_char(count( *),'9999') column3, NULL column4, NULL column5

Select  ter_codigo    
from  Manantial.gestores , Manantial.tercerizaciones T 
where  ter_ges_codigo = ges_codigo    
and    ter_fec_baja is null
and    ges_fec_baja is null  
and    substr(ter_descripcion,1,1) = 'J' 
and    substr(ter_descripcion,2,1) in ('0','1','2','3','4','5','6','7','8','9')
and    substr(ter_descripcion,3,1) in ('0','1','2','3','4','5','6','7','8','9')



Select  ter_codigo    
from  Manantial.gestores , Manantial.tercerizaciones T 
where  ter_ges_codigo = ges_codigo    
and    ter_fec_baja is null
and    ges_fec_baja is null  
and   REGEXP_SUBSTR(ter_descripcion,'^J[0-9]{2,}[[:space:]]*')  <> ' '




and    exists ( Select 1 from Manantial.cartera_bacup cb 
                where   t.ter_codigo = cb.ter_codigo  
                and     cb.fec_baja is null   )
group by ges_codigo, ges_descripcion 
  


CREATE TABLE Manantial.projects(
    project_id INT  PRIMARY KEY,
    project_name VARCHAR2(100) NOT NULL
);

CREATE TABLE Manantial.members(
    member_id INT  PRIMARY KEY,
    member_name VARCHAR2(100) NOT NULL,
    project_id INT,
    FOREIGN KEY (project_id) REFERENCES projects(project_id)
);



INSERT INTO Manantial.projects(project_id,project_name) 
VALUES(1,'ERP');

INSERT INTO Manantial.projects(project_id,project_name) 
VALUES(2,'Sales CRM');

INSERT INTO Manantial.members(member_id, member_name, project_id)
VALUES(1,'John Doe',1);

INSERT INTO Manantial.members(member_id, member_name, project_id)
VALUES (2,'Jane Doe',1);

INSERT INTO Manantial.members(member_id, member_name, project_id)
VALUES (3,'Jack Daniel',null);



SELECT 
    member_name, 
    project_name
FROM 
    Manantial.members m
full OUTER JOIN manantial.projects p ON p.project_id = m.project_id
ORDER BY   member_name;  


SELECT 
    member_name, 
    project_name
FROM 
    Manantial.members m
JOIN Manantial.projects p ON p.project_id = m.project_id
ORDER BY   member_name;  

SELECT 
    member_name, 
    project_name
FROM  Manantial.members m
LEFT JOIN Manantial.projects p ON p.project_id = m.project_id
ORDER BY   member_name;

SELECT 
    member_name, 
    project_name
FROM  Manantial.members m
RIGHT JOIN Manantial.projects p ON p.project_id = m.project_id
ORDER BY   member_name;

    

Select tipo,  descripcion  

Select  TO_CHAR(tipo, '09')  column1, descripcion  column2,  NULL column3, NULL column4, NULL column5 
from (    
Select   2 tipo , 'Gestores Internos' descripcion   from dual
Union all  
Select   3 tipo , 'Pre Judiciales (C)' descripcion  from dual
Union all  
Select   4 tipo , 'Judicializados (J)'  descripcion  from dual
) Order by  Tipo 




Select * from   cartera_bacup 
where inm_cuenta = '05600565870329'

 SELECT COUNT(1)  
  	        FROM Manantial.CARTERA_BACUP
 	 	    WHERE inm_cuenta = :inm_cuenta
            and fec_baja is null
            and usr_baja is null;


