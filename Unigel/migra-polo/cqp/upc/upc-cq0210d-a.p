/**************************************
** UPC para o Janela de Retono do cq0210d.w
**
** Objetivo : Acompanha a upc CTLUPC007A.p
** Data: 09/11/04
**************************************/

def input parameter wgh-btok-cq0210d as widget-handle no-undo.
DEF INPUT PARAMETER h-de-inspecao-1  AS HANDLE NO-UNDO.

define new global shared var grw-exam-ficha as rowid no-undo.

FIND FIRST exam-ficha
    WHERE ROWID(exam-ficha) = grw-exam-ficha NO-LOCK NO-ERROR.
FIND FIRST ficha-cq
    WHERE ficha-cq.nr-ficha = exam-ficha.nr-ficha NO-LOCK NO-ERROR.
    
  /*SO FUNCIONA PARAPOLO*/  

IF SUBSTRING(ficha-cq.cod-estab,1,2) <> "42" THEN APPLY "choose":U TO wgh-btok-cq0210d.

 
FIND FIRST pol-param-estab
    WHERE pol-param-estab.cod-estabel = ficha-cq.cod-estab NO-LOCK NO-ERROR.
if not avail pol-param-estab then do:
    run utp/ut-msgs.p (input "show",
                       input 2,
                       input "Parametros POLO").
    return "NOK":U.
end.
ELSE DO:
    IF date(h-de-inspecao-1:SCREEN-VALUE) > pol-param-estab.data-cq and substring(pol-param-estab.cod-estabel,1,2) = "42" THEN DO:
        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT 25997,
                           INPUT "Transa‡Æo nÆo permitida!" + "~~" +
                             "Verifique a Data de CQ informada nos Parƒmetros do Estabelecimento - POLSF008.Estab:" + pol-param-estab.cod-estabel).
        return "NOK":U.
    END.
    else 
        APPLY "choose":U TO wgh-btok-cq0210d.
END.
/* fim do programa */
