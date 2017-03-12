DEFINE PARAMETER BUFFER p-table FOR funcionario.
DEFINE PARAMETER BUFFER o-table FOR funcionario.

DEFINE VARIABLE i-aux       AS INTEGER     NO-UNDO.
DEFINE VARIABLE l-calc-func AS LOGICAL     NO-UNDO.

ASSIGN l-calc-func = NO .
DO i-aux = 1 TO 10:

    IF INDEX(program-name(i-aux),"bs2000") > 0  OR 
       INDEX(program-name(i-aux),"pe4000") > 0  OR
       INDEX(program-name(i-aux),"pe4300") > 0  OR
       INDEX(program-name(i-aux),"pe4320") > 0  OR
       INDEX(program-name(i-aux),"bs2020") > 0  OR 
       INDEX(program-name(i-aux),"pe4020") > 0  OR
       INDEX(program-name(i-aux),"pe2200") > 0  OR 
       INDEX(program-name(i-aux),"pe2220") > 0  OR
       INDEX(program-name(i-aux),"fp3140") > 0  OR
       INDEX(program-name(i-aux),"fp3200") > 0  THEN
        l-calc-func = YES.
     
END.

IF l-calc-func = NO THEN DO:

    IF  (p-table.cdn_estab = "422" OR p-table.cdn_estab = "412") THEN RUN pi-gera-func-epi. /*solic-318*/

    /* Integra‡Æo SIS */
    /* DESATIVADO FOI CRIADO O PROGRAMA ESFP0037 PARA INTEGRACAO
    
    RUN database/twp/tw-funcionario-sis.p (BUFFER p-table,
                                           BUFFER o-table).
    IF RETURN-VALUE <> "OK" THEN RETURN "NOK".*/

    
END.



RETURN "OK".

PROCEDURE pi-gera-func-epi.
   DO TRANSACTION:
      find first am-cd-funcionario-req where 
        am-cd-funcionario-req.cod-funcionario = p-table.cdn_funcionario no-error.
                
         if not avail am-cd-funcionario-req then do:
     
             if  p-table.dat_desligto_func <> ?  then next.
             
                create am-cd-funcionario-req.
                assign 
                    am-cd-funcionario-req.cod-funcionario = p-table.cdn_funcionario
                    am-cd-funcionario-req.nome            = p-table.nom_pessoa_fisic
                    am-cd-funcionario-req.cod-ccusto      = p-table.cod_rh_ccusto 
                    am-cd-funcionario-req.dt-admissao     = p-table.dat_admis_func
                    am-cd-funcionario-req.dt-desligamento = p-table.dat_desligto_func
                    am-cd-funcionario-req.dt-nascimento   = p-table.dat_nascimento
                    am-cd-funcionario-req.lotacao         = string(p-table.cdn_estab )
                    am-cd-funcionario-req.sexo            = p-table.idi_sexo.
          end.
          else 
              am-cd-funcionario-req.dt-desligamento = p-table.dat_desligto_func.

   END.

END.
