DEF NEW GLOBAL SHARED VAR c-gsv-mip-lista-especialid AS CHAR NO-UNDO.

PROCEDURE pi-param-espec:

    IF CAN-FIND(FIRST usuar_espec WHERE usuar_espec.tp-especial = "412-MEC" THEN DO:  /*solic-318*/
        ASSIGN  c-gsv-mip-lista-especialid = "412-ELE,412-ELET,412-EXT,412-MEC,412-OPE,SEM-ESP".
    END.
    ELSE DO:
        ASSIGN  c-gsv-mip-lista-especialid = "422-ELE,422-ELET,422-EXT,422-MEC,422-OPE,SEM-ESP".
    END.

    
    IF CAN-FIND(FIRST usuar_espec NO-LOCK
                WHERE usuar_espec.cod_usuario = c-seg-usuario) THEN DO:
        ASSIGN c-gsv-mip-lista-especialid = "".
        FOR EACH usuar_espec NO-LOCK
            WHERE usuar_espec.cod_usuario = c-seg-usuario
               BY usuar_espec.int-1 BY usuar_espec.tp-especial:
            ASSIGN c-gsv-mip-lista-especialid = c-gsv-mip-lista-especialid + (IF c-gsv-mip-lista-especialid = "" THEN "" ELSE ",") + usuar_espec.tp-especial.
        END.
        ASSIGN c-gsv-mip-lista-especialid = c-gsv-mip-lista-especialid + ",SEM-ESP".
    END.
END.
