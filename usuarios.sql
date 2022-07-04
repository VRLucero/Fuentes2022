DECLARE
  CURSOR usr IS
    SELECT usr_usuario FROM seguridad.USUARIOS
    WHERE usr_usuario IN ('AAMBROSO','COLIVARES','DRODRIGUEZ','JUVILLEGAS','LORTIZ','MGONZALEZ','RMARCILLA','JOORTIZ','JGUATTINI','GPORTA','ADELICIA','MBISOGNO','OQUINTEROS',
    'JVALDIVIESO','NMENDEZ','NCHRISTOL','MHECHEGARAY','LCOVIS','GEPEREZ','APRIOR','WPUEBLA','SDONOSO','MREINO','JOORTIZ',
    'OGUEVARA','JDOMINGUEZ','VCESCHIN','EMERCADO','GGIRON','NCHRISTOL','MHERNANDEZ','VPALMA','DVIDELA','GDENITA');
  v_ambiente VARCHAR2(10)   := 'TEST';
  v_inicializado VARCHAR2(1):= 'N';
  v_existe VARCHAR2(1);
BEGIN
  FOR r IN usr LOOP
      BEGIN
        UPDATE seguridad.USUARIOS 
           SET usr_clave = seguridad.PKG_SEGURIDAD.encriptar(usr_usuario), 
               usr_inicializado = v_inicializado, 
               usr_fec_clave= SYSDATE,
               usr_fec_baja = NULL, 
               usr_usr_baja = NULL 
         WHERE usr_usuario  = r.usr_usuario;
      END;  
      BEGIN
        SELECT 'S' INTO v_existe
          FROM seguridad.USUARIOS_AMBIENTES
         WHERE uam_usr_usuario = r.usr_usuario
           AND uam_amb_id = DECODE(v_ambiente,'TEST',3,'PROD',2,'TARI',3,'TAR2',4,'PRO1',6,0)
           AND uam_fec_baja IS NULL;
        EXCEPTION WHEN NO_DATA_FOUND THEN  
          BEGIN
            INSERT INTO seguridad.USUARIOS_AMBIENTES 
            VALUES (seguridad.uam_seq.NEXTVAL, r.usr_usuario, DECODE(v_ambiente,'TEST',3,'PROD',2,'TARI',3,'TAR2',4,'PRO1',6,0),
                    'VLUCERO',SYSDATE ,NULL,NULL,NULL,NULL);  
          END;
      END;
  END LOOP;  
  COMMIT;
END;






