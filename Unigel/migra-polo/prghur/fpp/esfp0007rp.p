/********************************************************************************
* Programa: esfp0007.w
* Data....: Jun/2008
* Autor...: Fl vio Capitanio -  (11)9756-8761 
* Objetivo: Arquivos Interface HCM x Ronda - Atrav‚s de pasta padrÆo e logs
          : Processa via RPW
* VersÆo..: 2.09.000                            

*******************************************************************************/
/*---------------- Include de controle de VersÆo ------------------*/ 
{bf/buffersHCM.i}
{include/i-prgvrs.i esfp0007RP 2.09.00.000}

/******** Defini‡Æo Temp-table para Recebimento de Parametro **********************/
def temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    FIELD v_cdn_empres_usuar AS CHAR
    FIELD i-ep-ini         AS i
    FIELD i-ep-fim         AS i
    FIELD qt-dias          AS INTEGER.


/* recebimento de parƒmetros */
def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

{include/i-freeac.i}

create tt-param.
raw-transfer raw-param to tt-param.

/****************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/
def var h-acomp              as handle no-undo.
Def Var dat-trab             As Date.
DEF VAR nivcargo             AS INT FORMAT "999".
DEF VAR turno                AS INT. 
DEF VAR turma                AS INT.
DEF VAR empresa              AS INT.
/****************** Defini‡ao de  Frames e Forms do Relat¢rio 132 Colunas ***************************************/ 

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

/****************** FIM INCLUDE COM VARIµVEIS GLOBAIS *********************/


/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 


/*include padrÆo para vari veis para o log */
{include/i-rpvar.i}
/* defini‡Æo de vari veis e streams */
define stream s-exp.

/* defini‡Æo de frames do log */

/* include padrÆo para output de log */
{include/i-rpout.i &STREAM="stream str-rp" &TOFILE=tt-param.arq-destino}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i &STREAM="str-rp"}

/* bloco principal do programa */

find first param_empres_rh WHERE param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar no-lock no-error.
find empresa where
     empresa.ep-codigo = param_empres_rh.cdn_empresa 
     no-lock no-error.

assign c-empresa      = empresa.razao-social
       c-programa     = "esfp0007RP"
       c-versao       = "2.10"
       c-revisao      = "000"
       c-titulo-relat = "Exporta Arquivos Interface HCM x Ronda"
       c-sistema      = "Folha de Pagamento".

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Importando *}

run pi-inicializar in h-acomp (input RETURN-VALUE).

/* define a sa¡da para o arquivo de sa¡da informado na p gina de parƒmetros */

/*-------------------------- bloco principal do programa -------------------------*/
Output To  value(arq-entrada + "\colaborador_endereco.txt.") No-echo.
       Run pi-exporta-colaborador-endereco.
OUTPUT Close.
RUN  pi-acompanhar in h-acomp(input "Arquivos de colaborador gerado com sucesso").

RUN  pi-acompanhar in h-acomp(input "Gerando o arquivo de colaborador").
Output To  value(arq-entrada + "\afastamento.txt.") No-echo.
       Run pi-exporta-afastamento.
Output Close.

Output To  value(arq-entrada + "\cargo.txt.") No-echo.
       Run pi-exporta-cargo.
Output Close.

Output To  value(arq-entrada + "\colaborador_ccusto.txt.") No-echo.
       Run pi_colaborador_ccusto.
Output Close.

Output To  value(arq-entrada + "\ccusto.txt.") No-echo.
       Run pi-exporta-ccusto.
Output Close.

Output To  value(arq-entrada + "\colaborador.txt") No-echo.
       Run pi-exporta-colaborador.
Output Close.

Output To  value(arq-entrada + "\colaborador_compl_ficha.txt.") No-echo.
       Run pi-exporta-colaborador-compl-ficha.
Output Close.

Output To  value(arq-entrada + "\colaborador_turno.txt.") No-echo.
       Run pi_gera_turno.
Output Close.


run pi-finalizar in h-acomp.
/*Put  Stream s-exp "Arquivos gerados na pasta \\192.168.2.62\senior\importacao, verifique os arquivos " Form "x(100)" Skip. */
Input Stream s-exp close. /* fechamento do output do log */
{include/i-rpclo.i &STREAM="stream str-rp"}
return "OK":U.     

/* fim do programa */

Procedure pi_colaborador_ccusto:
/*    Run pi_gera_historico_passado. */
    Run pi_gera_historico_presente_cc.
End Procedure.

/*Procedure pi_gera_historico_passado:
    FOR EACH func_ccusto NO-LOCK WHERE
             func_ccusto.cdn_empresa >= tt-param.i-ep-ini  And 
             func_ccusto.cdn_empresa <= tt-param.i-ep-fim  And   
             func_ccusto.dat_fim_lotac_func >= tt-param.qt-dias And func_ccusto.dat_fim_lotac_func < 12/31/2999:
        run pi-acompanhar in h-acomp(input "Exporta FuncCusto: " + String( func_ccusto.cdn_funcionario)).

           PUT if func_ccusto.cdn_empresa = 420  /*solic-318*/ 
           then 120 else func_ccusto.cdn_empresa     AT 1   FORM "9999"
               1                                     AT 5   FORM "9"
               func_ccusto.cdn_funcionario           AT 6   FORM "999999999"
               func_ccusto.dat_fim_lotac_func        AT 15  FORM "99/99/9999"
               func_ccusto.cod_rh_ccusto             AT 25  FORM "x(06)"
               2                                     AT 31  FORM "9" SKIP.
     END.         

End Procedure.
*/

Procedure pi_gera_historico_presente_cc:
    FOR EACH func_ccusto NO-LOCK WHERE
             func_ccusto.cdn_empresa >= string(tt-param.i-ep-ini)  And 
             func_ccusto.cdn_empresa <= string(tt-param.i-ep-fim)  And
             func_ccusto.dat_fim_lotac_func >= 12/31/2999 AND /*Lima*/
             func_ccusto.dat_inic_lotac_func >= TODAY - tt-param.qt-dias AND
               func_ccusto.dat_inic_lotac_func <= TODAY /*) OR
              func_ccusto.dat_ult_atualiz <= tt-param.qt-dias)*/:
        run pi-acompanhar in h-acomp(input "Exporta Funcionario Custo: " + String( func_ccusto.cdn_funcionario)).

           PUT if int(func_ccusto.cdn_empresa) = 420 OR int(func_ccusto.cdn_empresa) = 410 
           then 120 else int(func_ccusto.cdn_empresa)     AT 1   FORM "9999"
               1                                     AT 5   FORM "9"
               func_ccusto.cdn_funcionario           AT 6   FORM "999999999"
               func_ccusto.dat_inic_lotac_func       AT 15  FORM "99/99/9999"
               func_ccusto.cod_rh_ccusto             AT 25  FORM "x(06)"
               1                                     AT 31  FORM "9" SKIP.
     END.         

End Procedure.

Procedure pi-exporta-ccusto:

    FOR EACH rh_ccusto NO-LOCK WHERE rh_ccusto.dat_ult_atualiz >= TODAY - tt-param.qt-dias AND
                                     rh_ccusto.cdn_empresa >= string(tt-param.i-ep-ini)  And 
                                     rh_ccusto.cdn_empresa <= string(tt-param.i-ep-fim)  :
        run pi-acompanhar in h-acomp(input "Exporta CCusto: " + String( rh_ccusto.des_rh_ccusto)).
        PUT if int(rh_ccusto.cdn_empresa) = 420 OR int(rh_ccusto.cdn_empresa) = 410  /*solic-318*/ 
           then 120 else int(rh_ccusto.cdn_empresa)       AT 1   FORM "9999"
            rh_ccusto.cod_rh_ccusto                  AT 5  FORM "x(09)"
            fn-free-accent(rh_ccusto.des_rh_ccusto)
                                                     AT 14  FORM "x(40)"
            TODAY                                    AT 64 FORM "99/99/9999"
            "00/00/0000"                             AT 74  SKIP.
    END.         
End Procedure.
Procedure pi-exporta-cargo:
  FOR EACH  cargo NO-LOCK WHERE cargo.dat_ult_atualiz >= TODAY - tt-param.qt-dias  :
       run pi-acompanhar in h-acomp(input "Exporta Cargo" + cargo.des_cargo).
       ASSIGN nivcargo = cargo.cdn_niv_cargo.                         /*Lima*/
       PUT 1                                              AT   1   FORM  "999"   /*Lima*/
         fn-free-accent(string(cargo.cdn_cargo)) + string(nivcargo,"999")
                                                          AT   4   FORM  "x(24)" /*Lima*/
         fn-free-accent(cargo.des_envel_pagto)            AT   28  FORM  "x(25)" 
         fn-free-accent(cargo.des_cargo)                  AT   53  FORM  "x(40)" SKIP.
  END.         

End Procedure.
Procedure pi-exporta-afastamento:
    Run pi_gera_historico_desligado.
    Run pi_gera_historico_retorno_afastado.             
    Run pi_gera_historico_presente.
End Procedure.
Procedure pi_gera_historico_desligado:    
    FOR EACH funcionario NO-LOCK WHERE 
             funcionario.cdn_empresa >= string(tt-param.i-ep-ini)  And
             funcionario.cdn_empresa <= string(tt-param.i-ep-fim)  And
             funcionario.dat_desligto_func >= TODAY - tt-param.qt-dias :
       /* MESSAGE "Func desligado " funcionario.cdn_funcionario SKIP
            VIEW-AS ALERT-BOX INFORMATION. */
        run pi-acompanhar in h-acomp(input "Exporta Colaborador Desligado: " + funcionario.nom_pessoa_fisic).
        FIND LAST sit_afast_func OF funcionario NO-LOCK NO-ERROR.
        FIND sit_afast OF sit_afast_func NO-LOCK NO-ERROR. 
        IF sit_afast.idi_signif_sit <> 6 THEN NEXT.
        
        PUT if int(sit_afast_func.cdn_empresa) = 420 OR int(sit_afast_func.cdn_empresa) = 410  /*solic-318*/ 
            then 120 else int(sit_afast_func.cdn_empresa)    AT 1   FORM "9999"
            1                                           AT 5   FORM "9"
            sit_afast_func.cdn_funcionario              AT 6   FORM "999999999"
            DAY(sit_afast_func.dat_inic_sit_afast)      AT 15  FORM "99"
            MONTH(sit_afast_func.dat_inic_sit_afast)    AT 17  FORM "99"
            year(sit_afast_func.dat_inic_sit_afast)     AT 19  FORM "9999"
            "00:00"                                     AT 23  FORM "X(05)"
            /*DAY(sit_afast_func.dat_term_sit_afast)    AT 28  FORM "99"
            MONTH(sit_afast_func.dat_term_sit_afast)    AT 30  FORM "99"
            IF year(sit_afast_func.dat_term_sit_afast) > 2050 THEN 2050 ELSE year(sit_afast_func.dat_term_sit_afast)  AT 32  FORM "9999"  */
            0                                           AT 28  FORM "99999999"
            "00:00"                                     AT 36  FORM "X(05)"  
            7                                           AT 41  FORM "999"
            1                                           AT 44  FORM "999" 
            0                                           AT 47  FORM "9" SKIP.
     END.         
     

End Procedure.

Procedure pi_gera_historico_retorno_afastado:    
    DEFINE BUFFER bst-funcionario FOR funcionario.
    DEFINE BUFFER btr-funcionario FOR funcionario.


    FOR EACH sit_afast NO-LOCK WHERE sit_afast.idi_signif_sit = 2,
        EACH sit_afast_func NO-LOCK WHERE
             sit_afast_func.cdn_sit_afast_func =  sit_afast.cdn_sit_afast_func AND
             sit_afast_func.cdn_empresa  >= string(tt-param.i-ep-ini) And
             sit_afast_func.cdn_empresa  <= string(tt-param.i-ep-fim) And 
             sit_afast_func.dat_term_sit_afast >= TODAY - tt-param.qt-dias   AND
             sit_afast_func.dat_ult_atualiz    >= TODAY - tt-param.qt-dias  AND
             sit_afast_func.dat_inic_sit_afast <= TODAY:
       


         run pi-acompanhar in h-acomp(input "Exporta Historico Afastamento: " + String( sit_afast_func.cdn_funcionario)).
        PUT /*1                                        AT 1   FORM "99"*/
            if int(sit_afast_func.cdn_empresa) = 420 OR int(sit_afast_func.cdn_empresa) = 410  /*solic-318*/ 
            then 120 else int(sit_afast_func.cdn_empresa)    AT 1   FORM "9999"
            1                                           AT 5   FORM "9"
            sit_afast_func.cdn_funcionario              AT 6   FORM "999999999"
            DAY(sit_afast_func.dat_inic_sit_afast)      AT 15  FORM "99"
            MONTH(sit_afast_func.dat_inic_sit_afast)    AT 17  FORM "99"
            year(sit_afast_func.dat_inic_sit_afast)     AT 19  FORM "9999"
            "00:00"                                     AT 23  FORM "X(05)"
            DAY(sit_afast_func.dat_term_sit_afast)      AT 28  FORM "99"
            MONTH(sit_afast_func.dat_term_sit_afast)    AT 30  FORM "99"
            IF year(sit_afast_func.dat_term_sit_afast) > 2050 THEN 2050 ELSE
               year(sit_afast_func.dat_term_sit_afast)AT 32  FORM "9999"
            "00:00"                                     AT 36  FORM "X(05)"  
            14                                          AT 41  FORM "999"
            2                                           AT 44  FORM "999"
            0                                           AT 47  FORM "9"        SKIP. /** Update ***/
            

     END.         
/*  tentativa de fazer retornar for‡ado transferidos   
     FOR EACH sit_afast NO-LOCK WHERE sit_afast.idi_signif_sit = 4,
         EACH sit_afast_func NO-LOCK WHERE
              sit_afast_func.cdn_sit_afast_func =  sit_afast.cdn_sit_afast_func AND
              sit_afast_func.cdn_empresa  >= string(tt-param.i-ep-ini) And
              sit_afast_func.cdn_empresa  <= string(tt-param.i-ep-fim) And 
              sit_afast_func.dat_term_sit_afast >= TODAY - tt-param.qt-dias   AND
              sit_afast_func.dat_ult_atualiz    >= TODAY - tt-param.qt-dias  AND
              sit_afast_func.dat_inic_sit_afast <= TODAY:


         FIND first bst-funcionario OF sit_afast_func where
                            bst-funcionario.dat_desligto_func <> ?  NO-LOCK NO-ERROR.

           IF AVAIL bst-funcionario  THEN DO:
             FIND FIRST btr-funcionario WHERE 
                 btr-funcionario.cod_id_feder      = bst-funcionario.cod_id_feder AND
                 btr-funcionario.dat_desligto_func = ?  AND
                 btr-funcionario.cdn_empresa       = bst-funcionario.cdn_empresa NO-LOCK USE-INDEX fncnr_idfdemp NO-ERROR.
             IF NOT AVAIL btr-funcionario THEN NEXT.
           END.

          run pi-acompanhar in h-acomp(input "Exporta Historico Afastamento: " + String( sit_afast_func.cdn_funcionario)).
         PUT /*1                                        AT 1   FORM "99"*/
             if int(sit_afast_func.cdn_empresa) = 420 OR int(sit_afast_func.cdn_empresa) = 410  /*solic-318*/ 
             then 120 else int(sit_afast_func.cdn_empresa)    AT 1   FORM "9999"
             1                                           AT 5   FORM "9"
             sit_afast_func.cdn_funcionario              AT 6   FORM "999999999"
             DAY(sit_afast_func.dat_inic_sit_afast)      AT 15  FORM "99"
             MONTH(sit_afast_func.dat_inic_sit_afast)    AT 17  FORM "99"
             year(sit_afast_func.dat_inic_sit_afast)     AT 19  FORM "9999"
             "00:00"                                     AT 23  FORM "X(05)"
             DAY(sit_afast_func.dat_term_sit_afast)      AT 28  FORM "99"
             MONTH(sit_afast_func.dat_term_sit_afast)    AT 30  FORM "99"
             IF year(sit_afast_func.dat_term_sit_afast) > 2050 THEN 2050 ELSE
                year(sit_afast_func.dat_term_sit_afast)AT 32  FORM "9999"
             "00:00"                                     AT 36  FORM "X(05)"  
             14                                          AT 41  FORM "999"
             2                                           AT 44  FORM "999"
             0                                           AT 47  FORM "9"        SKIP. /** Update ***/


      END.         
  */
End Procedure.

Procedure pi_gera_historico_presente:
    DEFINE BUFFER bst-funcionario FOR funcionario.
    DEFINE BUFFER btr-funcionario FOR funcionario.


    FOR EACH sit_afast NO-LOCK WHERE LOOKUP(STRING(sit_afast.idi_signif_sit),"2,4,5") > 0,
        EACH sit_afast_func NO-LOCK WHERE
                 sit_afast_func.cdn_sit_afast_func =  sit_afast.cdn_sit_afast_func AND
                 sit_afast_func.cdn_empresa  >= string(tt-param.i-ep-in) And
                 sit_afast_func.cdn_empresa  <= string(tt-param.i-ep-fim) And  
                 sit_afast_func.dat_inic_sit_afast >= TODAY - tt-param.qt-dias AND
                 sit_afast_func.dat_inic_sit_afast <= TODAY :

          IF  sit_afast.idi_signif_sit = 4 THEN DO:
            FIND first bst-funcionario OF sit_afast_func where
                        bst-funcionario.dat_desligto_func <> ?  NO-LOCK NO-ERROR.

           IF AVAIL bst-funcionario  THEN DO:
             FIND FIRST btr-funcionario WHERE 
                 btr-funcionario.cod_id_feder      = bst-funcionario.cod_id_feder AND
                 btr-funcionario.dat_desligto_func = ?  AND
                 btr-funcionario.cdn_empresa       = bst-funcionario.cdn_empresa NO-LOCK USE-INDEX fncnr_idfdemp NO-ERROR.
        
             IF AVAIL btr-funcionario THEN NEXT.

           END.

        END.

       /* IF LOOKUP(STRING(sit_afast.idi_signif_sit),"2,4,5") > 0 THEN */
        run pi-acompanhar in h-acomp(input "Exporta Colaborador Endere‡o: " + String (sit_afast_func.cdn_funcionario)).
        PUT /*1                                        AT 1   FORM "99"*/
            if int(sit_afast_func.cdn_empresa) = 420 OR int(sit_afast_func.cdn_empresa) = 410  /*solic-318*/ 
            then 120 else int(sit_afast_func.cdn_empresa)    AT 1   FORM "9999"
            1                                           AT 5   FORM "9"
            sit_afast_func.cdn_funcionario              AT 6   FORM "999999999"
            DAY(sit_afast_func.dat_inic_sit_afast)      AT 15  FORM "99"
            MONTH(sit_afast_func.dat_inic_sit_afast)    AT 17  FORM "99"
            year(sit_afast_func.dat_inic_sit_afast)     AT 19  FORM "9999"
            "00:00"                                     AT 23  FORM "X(05)"
            DAY(sit_afast_func.dat_term_sit_afast)      AT 28  FORM "99"
            MONTH(sit_afast_func.dat_term_sit_afast)    AT 30  FORM "99"
            IF year(sit_afast_func.dat_term_sit_afast) > 2050 THEN 2050 ELSE
               year(sit_afast_func.dat_term_sit_afast)  AT 32  FORM "9999"
            "00:00"                                     AT 36  FORM "X(05)"  
            IF sit_afast.idi_signif_sit = 5 THEN
               2
            ELSE
            IF sit_afast.idi_signif_sit = 4 THEN 
               7 
            ELSE 14                                     AT 41  FORM "999"
            1                                           AT 44  FORM "999"
            0                                           AT 47  FORM "9" Skip.
     END.         

End Procedure.

Procedure pi-exporta-colaborador-endereco:

    FOR EACH funcionario No-lock WHERE 
             funcionario.cdn_empresa >= string(tt-param.i-ep-ini)  And
             funcionario.cdn_empresa <= string(tt-param.i-ep-fim)  And
             funcionario.dat_desligto_func = ? AND 
            (funcionario.dat_admis_func >= TODAY - tt-param.qt-dias  OR
             funcionario.dat_admis_transf_func >= TODAY - tt-param.qt-dias ): 
       run pi-acompanhar in h-acomp(input "Exporta Colaborador Endere‡o: " + funcionario.nom_pessoa_fisic).
       FIND rh_pessoa_fisic OF funcionario NO-LOCK NO-ERROR.
       PUT if int(funcionario.cdn_empresa) = 420 OR int(funcionario.cdn_empresa) = 410  /*solic-318*/ 
            then 120 else int(funcionario.cdn_empresa)    AT 1   FORM "9999"
           1                                         AT 5  FORM "9"
           funcionario.cdn_funcionario               AT 6  FORM "999999999"
           fn-free-accent(substr(rh_pessoa_fisic.nom_ender_rh,1,LENGTH(rh_pessoa_fisic.nom_ender_rh)) + "-" + rh_pessoa_fisic.nom_bairro_rh)
                                                     AT 15 FORM "x(40)"
           int(substr(rh_pessoa_fisic.cod_livre_1,67,6)) AT 55 FORM "999999"
           fn-free-accent(rh_pessoa_fisic.nom_cidad_rh)
                                                     AT 61 FORM "x(25)"
           STRING(rh_pessoa_fisic.cod_cep_rh)        AT 86 FORM "x(10)"
           STRING(rh_pessoa_fisic.num_ddd) + STRING(rh_pessoa_fisic.num_telefone) AT 96 FORM "X(20)" 
           " "                                       AT 116 FORM "X(15)" /* ramal */
           rh_pessoa_fisic.nom_e_mail                AT 131 FORM "x(100)"
           rh_pessoa_fisic.cod_id_estad_fisic        AT 231 FORM "x(15)" SKIP.
     END.         
    /* MESSAGE "Fim do ieEndere‡o " SKIP VIEW-AS ALERT-BOX.*/
End Procedure.

Procedure pi-exporta-colaborador-compl-ficha:
      
    FOR EACH funcionario No-lock WHERE 
             funcionario.cdn_empresa >= string(tt-param.i-ep-ini)  And       
             funcionario.cdn_empresa <= string(tt-param.i-ep-fim)  And  
             funcionario.dat_desligto_func = ? AND (funcionario.dat_admis_func >= TODAY - tt-param.qt-dias OR
                                             funcionario.dat_admis_transf_func >= TODAY - tt-param.qt-dias ):
        run pi-acompanhar in h-acomp(input "Exporta Colaborador Ficha: " + funcionario.nom_pessoa_fisic).
       FIND rh_pessoa_fisic OF funcionario NO-LOCK NO-ERROR.
       PUT if int(funcionario.cdn_empresa) = 420 OR int(funcionario.cdn_empresa) = 410  /*solic-318*/ 
            then 120 else int(funcionario.cdn_empresa)    AT 1   FORM "9999"
           1                                     AT 5  FORM "9"
           funcionario.cdn_funcionario           AT 6  FORM "999999999"
           rh_pessoa_fisic.cod_id_feder          AT 15 FORM "999.999.999-99"
           funcionario.cod_pis                   AT 29 FORM "999.99999.99.9"
           funcionario.dat_pis_pasep             AT 43 FORM "99/99/9999" SKIP.

     END.        
End Procedure.
Procedure pi-exporta-colaborador:
    For Each funcionario No-lock Where  

             funcionario.cdn_empresa >= string(tt-param.i-ep-ini)  And
             funcionario.cdn_empresa <= string(tt-param.i-ep-fim)  And
             funcionario.dat_desligto_func = ? And  
            (funcionario.dat_admis_func        >= TODAY - tt-param.qt-dias  OR
             funcionario.dat_admis_transf_func >= TODAY - tt-param.qt-dias ) :
    FIND cargo OF funcionario NO-LOCK NO-ERROR.
    FIND rh_ccusto OF funcionario NO-LOCK NO-ERROR.
    run pi-acompanhar in h-acomp(input "Exporta Colaborador: " + funcionario.nom_pessoa_fisic).
    ASSIGN nivcargo = funcionario.cdn_niv_cargo.
       PUT 1                                     AT 1   FORM "99"
           if int(funcionario.cdn_empresa) = 420 OR int(funcionario.cdn_empresa) = 410  /*solic-318*/ 
            then 120 else int(funcionario.cdn_empresa)    AT 3   FORM "9999"
           1                                     AT 7   FORM "9"
           funcionario.cdn_funcionario           AT 8   FORM "999999999"
           fn-free-accent(funcionario.nom_pessoa_fisic)
                                                 AT 17 FORM "x(40)"
           fn-free-accent(funcionario.nom_abrev_pessoa_fisic)
                                                 AT 57 FORM "x(15)"
           funcionario.dat_admis_func            AT 72 FORM "99999999" 
           1                                     AT 80 FORM "999"
           funcionario.dat_admis_func            AT 83 FORM "99999999"
           0                                     AT 91 FORM "99999"
           "001"                                 AT 96 FORM "999"
           fn-free-accent(string(funcionario.cdn_cargo_basic))
                        + string(nivcargo,"999") AT 99 FORM "x(24)"
           1                                     AT 123 FORM "9999"
           if int(funcionario.cdn_estab) = 421 OR int(funcionario.cdn_estab) = 411  /*solic-318*/ then 121
           else if int(funcionario.cdn_estab) = 422 OR int(funcionario.cdn_estab) = 412  /*solic-318*/ then 123
           else if int(funcionario.cdn_estab) = 423 OR int(funcionario.cdn_estab) = 413  /*solic-318*/ then 122
           else if int(funcionario.cdn_estab) = 424 then 125
           else int(funcionario.cdn_estab)            AT 127   FORM "9999"
           funcionario.cdn_plano_lotac           AT 131 FORM "999"
           if int(funcionario.cdn_estab) = 421 OR int(funcionario.cdn_estab) = 411  /*solic-318*/ then "121"
           else if int(funcionario.cdn_estab) = 422 OR int(funcionario.cdn_estab) = 412  /*solic-318*/ then "123"
           else if int(funcionario.cdn_estab) = 423 OR int(funcionario.cdn_estab) = 413  /*solic-318*/ then "122"
           else if int(funcionario.cdn_estab) = 424 then "125"
           else string(int(funcionario.cdn_estab),"999")   AT 134 FORM "x(32)"
           1                                     AT 166 FORM "9"
           IF funcionario.idi_sexo = 1 THEN "M" ELSE "F" AT 167 FORM "x(01)"
           funcionario.dat_nascimento            AT 168 FORM "99999999"
           0                                     AT 176 FORM "999999999999"
           1                                     AT 188 FORM "999"
           1                                     AT 191 FORM "999"
           1                                     AT 194 FORM "999"
           1                                     AT 197 FORM "999"
           1                                     AT 200 FORM "999"
           1                                     AT 203 FORM "999" 
           1                                     AT 206 FORM "99" 
               SKIP.
     End.         

End Procedure.


PROCEDURE pi_gera_turno:
DEFINE BUFFER bst-funcionario FOR funcionario.
DEFINE BUFFER btr-funcionario FOR funcionario. 

    FOR EACH func_turno_trab NO-LOCK WHERE
             func_turno_trab.cdn_empresa            >= string(tt-param.i-ep-ini ) And 
             func_turno_trab.cdn_empresa            <= string(tt-param.i-ep-fim ) AND    
             func_turno_trab.dat_term_lotac_func    >= 12/31/2999 AND /*Lima*/
             func_turno_trab.dat_inic_lotac_func    >= TODAY - tt-param.qt-dias AND
             func_turno_trab.dat_inic_lotac_func        <= TODAY .
        

         
           FIND first bst-funcionario OF func_turno_trab where
                        bst-funcionario.dat_desligto_func <> ?  NO-LOCK NO-ERROR.

           IF AVAIL bst-funcionario  THEN DO:
             FIND FIRST btr-funcionario WHERE 
                 btr-funcionario.cod_id_feder      = bst-funcionario.cod_id_feder AND
                 btr-funcionario.dat_desligto_func = ?  AND
                 btr-funcionario.cdn_empresa       = bst-funcionario.cdn_empresa NO-LOCK USE-INDEX fncnr_idfdemp NO-ERROR.
        
             IF AVAIL btr-funcionario THEN NEXT.

           END.

     


        run pi-acompanhar in h-acomp(input "Exporta Funcionario Turno: " + String( func_turno_trab.cdn_funcionario)).

        ASSIGN turno = IF func_turno_trab.cdn_turno_trab      = 1 THEN 2 ELSE
                       (IF func_turno_trab.cdn_turno_trab     = 50 THEN 3 ELSE
                       (IF func_turno_trab.cdn_turno_trab     = 51 THEN 4 ELSE 1))
               turma = IF func_turno_trab.cdn_turno_trab      = 50 THEN func_turno_trab.cdn_turma_trab ELSE
                       (IF func_turno_trab.cdn_turno_trab     = 51 THEN func_turno_trab.cdn_turma_trab ELSE 1)
               empresa = IF int(func_turno_trab.cdn_empresa) = 420 OR int(func_turno_trab.cdn_empresa) = 410  /*solic-318*/  THEN 120 ELSE int(func_turno_trab.cdn_empresa).






           PUT empresa                                        AT 1   FORM "9999"
               1                                              AT 5   FORM "9"
               func_turno_trab.cdn_funcionario                AT 6   FORM "999999999"
               func_turno_trab.dat_inic_lotac_func            AT 15  FORM "99/99/9999"
               turno                                          AT 25  FORM "9999"
               turma                                          AT 29  FORM "9"
               0                                              AT 30  FORM "99999" SKIP.
     END.         

End Procedure.
