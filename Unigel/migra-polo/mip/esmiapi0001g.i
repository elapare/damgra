DEF NEW GLOBAL SHARED VAR c-gsv-mip-lista-especialid AS CHAR NO-UNDO.

PROCEDURE pi-param-espec:
    ASSIGN  c-gsv-mip-lista-especialid = STRING({cdp\poloestab.i 422}) + "-ELE," + STRING({cdp\poloestab.i 422}) +  "-ELET," + STRING({cdp\poloestab.i 422}) +  "-EXT," + STRING({cdp\poloestab.i 422}) +  "-MEC," + STRING({cdp\poloestab.i 422}) +  "-OPE,SEM-ESP".
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
