/* UPC para travamento de pedido de clientes com problema NO TIPO DO PEDIDO. */
/* dever  ser cadastrada para  bodi159.                   */
/* Autor : Edson Louren‡o da Aparecida - 08/05/2007                   */

    
{include/i-epc200.i bodi159}
DEF TEMP-TABLE tt-ped-venda LIKE ped-venda 
          FIELD r-rowid AS ROWID. 

/* Include com defini¯Êo da temp-table RowErrors */
/*DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.*/

DEF INPUT PARAM  p-ind-event AS  CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

    
DEFINE VARIABLE l-erro AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE l-erro-canal AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE C-TIPO AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-canal AS CHARACTER  NO-UNDO.
DEF VAR h-BO                AS HANDLE NO-UNDO.

 
/* MESSAGE p-ind-event SKIP
     PROGRAM-NAME(1) SKIP
     PROGRAM-NAME(2) SKIP
     PROGRAM-NAME(3) SKIP
     PROGRAM-NAME(4) SKIP
     PROGRAM-NAME(5) 
                  

     VIEW-AS ALERT-BOX INFO BUTTONS OK.

  RETURN.*/


IF p-ind-event = "AfterValidateRecord" 
    AND (PROGRAM-NAME(4)  matches "*pdp/pd4000.w" OR PROGRAM-NAME(5)  matches "*pdp/pd4000.w") THEN DO:
   

    FIND FIRST tt-epc WHERE 
         tt-epc.cod-event = p-ind-event AND
         tt-epc.cod-parameter = "OBJECT-HANDLE" NO-LOCK NO-ERROR.
    IF  AVAIL tt-epc THEN DO:

        ASSIGN h-bo = WIDGET-HANDLE(tt-epc.val-parameter).
        
        RUN getRecord IN h-bo (OUTPUT TABLE tt-ped-venda).
        
        FIND FIRST tt-ped-venda NO-LOCK NO-ERROR.
        IF AVAIL tt-ped-venda THEN DO:
          ASSIGN c-tipo = tt-ped-venda.tp-pedido
                 c-canal = STRING(tt-ped-venda.cod-canal-venda).

          FIND FIRST estabelec WHERE estabelec.cod-estabel = tt-ped-venda.cod-estabel NO-LOCK NO-ERROR.

          IF estabelec.ep-codigo <> "420" AND estabelec.ep-codigo <> "410" /* and estabelec.ep-codigo <> 700 */ THEN RETURN "ok". /*solic-318*/ 
          
          IF NOT(INDEX("POREADQFXZ",trim(c-tipo)) <> 0) THEN l-erro = YES.
          
          IF INDEX("PORE",trim(c-tipo)) <> 0 THEN DO:
              
              IF SUBSTRING(tt-ped-venda.nat-operacao,2,1) <> "1" AND
                 SUBSTRING(tt-ped-venda.nat-operacao,2,1) <> "5" THEN l-erro = YES.

              FIND natur-oper WHERE natur-oper.nat-operacao = tt-ped-venda.nat-operacao NO-LOCK NO-ERROR.
              IF AVAIL natur-oper THEN DO: 
                  IF  natur-oper.emite-duplic THEN l-erro = NO.
                  IF  natur-oper.terceiros THEN l-erro = NO.
              END.
              

              
          END.
              
                    
          IF INDEX("AD",trim(c-tipo)) <> 0 AND 
              SUBSTRING(tt-ped-venda.nat-operacao,2,1) <> "9" THEN l-erro = YES.

        END.
            
    END.

   /* se tiver que inserir erro, usar  a rotina abaixo */

  IF l-erro THEN DO:
      FOR FIRST  tt-epc WHERE
            tt-epc.cod-event = p-ind-event AND
            tt-epc.cod-parameter = "OBJECT-HANDLE"  : /*handle da bo*/


          RUN _insertErrorManual IN h-bo /*(widget-handle(tt-epc.val-parameter) )*/
                                           ( INPUT 0,
                                             INPUT "EMS":U,
                                             INPUT "ERROR":U,
                                             INPUT "TIPO DE PEDIDO " + " >>>>( " + c-tipo + " )<<<<  " + "INVALIDO!" ,
                                             INPUT "bodi159 - Valida‡Æo de tipo de pedido: Verifique o tipo do pedido informado, a natureza de opera‡Æo e o emitente." + 
                                             CHR(10) + "Tipos V lidos atualmente: P O R E A D Q F X Z",
                                             INPUT "":U).
      END.

  END.
   
  IF l-erro-canal THEN DO:
    FOR FIRST  tt-epc WHERE
          tt-epc.cod-event = p-ind-event AND
          tt-epc.cod-parameter = "OBJECT-HANDLE"  : /*handle da bo*/


        RUN _insertErrorManual IN widget-handle(tt-epc.val-parameter) 
                                         ( INPUT 0,
                                           INPUT "EMS":U,
                                           INPUT "ERROR":U,
                                           INPUT "CANAL DE VENDA " + " >>>>( " + c-canal + " )<<<<  " + "INVALIDO!" ,
                                           INPUT "bodi159 - Valida‡Æo de canal de venda: Verifique o canal de venda." + 
                                           CHR(10) + "Canais V lidos atualmente: 1 e 2",
                                           INPUT "":U).
    END.

  END.


END.
IF l-erro OR l-erro-canal THEN RETURN "nok".
ELSE
    RETURN "ok".
