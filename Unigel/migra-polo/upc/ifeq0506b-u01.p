/***************************************************************************
**
**       Programa: ifeq0506b-u01.p
**
**       Autor...: Rodrigo Luis Frohlich - Vertice
**
**       Empresa: Vertice
**       
**       Data de Cria‡Æo: 28/02/2010
**
**       Objetivo: UPC - eq0506b.p
**
*****************************************************************************/ 
{include/i-prgvrs.i IFEQ0506-U01 2.06.00.000}

DEF INPUT PARAM p-ind-event  AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table  AS CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table  AS ROWID         NO-UNDO.

DEF TEMP-TABLE tt-valid-ped
    FIELD cdd-embarq LIKE embarque.cdd-embarq
    FIELD nome-abrev  LIKE ped-venda.nome-abrev
    FIELD nr-pedcli   LIKE ped-venda.nr-pedcli
    FIELD tipo        AS INTEGER
    FIELD mod-frete   AS CHARACTER.

DEF BUFFER b-tt-valid-ped FOR tt-valid-ped.

DEF VAR h-acomp      AS HANDLE    NO-UNDO.
DEF VAR l-ped-cif    AS LOGICAL   NO-UNDO.
DEF VAR l-ped-fob    AS LOGICAL   NO-UNDO.
DEF VAR c-cidade-ori AS CHARACTER NO-UNDO.
DEF VAR c-cidade-des AS CHARACTER NO-UNDO.
DEF VAR c-ds-produto AS CHARACTER NO-UNDO.
DEF VAR l-ped-cond-esp-vencida    AS LOGICAL   NO-UNDO.

/* {upc/ifeq0506b-u01.i} */

{utp/ut-glob.i}

DEF BUFFER b-ped-venda  FOR ped-venda.
DEF BUFFER b2-ped-venda FOR ped-venda.
    

DEFINE VARIABLE l-erro AS LOGICAL  NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE wh-eq0506b-br-table AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-eq0506b-cli-fim  AS HANDLE        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-eq0506b-estab-fim  AS HANDLE        NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE h-query-eq0506b     AS HANDLE        NO-UNDO.

DEFINE VARIABLE h-objeto AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE c-objeto AS CHARACTER     NO-UNDO.
DEFINE VARIABLE i-nr-pedido AS INTEGER       NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* MESSAGE */
/*     "p-ind-event :" p-ind-event            SKIP */
/*     "p-ind-object:" p-ind-object           SKIP */
/*     "p-wgh-object:" p-wgh-object:FILE-NAME SKIP */
/*     "p-wgh-frame :" p-wgh-frame:NAME       SKIP */
/*     "p-cod-table :" STRING(p-cod-table)    SKIP */
/*     "p-row-table :" STRING(p-row-table)    SKIP */
/* VIEW-AS ALERT-BOX INFO BUTTONS OK.              */

IF p-ind-event  = "initialize" AND
   p-ind-object = "browser"    AND
   c-objeto     = "eq0506b-b01.w" THEN DO:

    ASSIGN h-frame = p-wgh-frame:FIRST-CHILD
           h-frame = h-frame:FIRST-CHILD.

    DO WHILE h-frame <> ?:

        IF h-frame:NAME = "br-table" THEN DO:
            ASSIGN wh-eq0506b-br-table = h-frame.
        END.

        ASSIGN h-frame = h-frame:NEXT-SIBLING.
    END.

    ASSIGN wh-eq0506b-cli-fim = wh-eq0506b-br-table:ADD-CALC-COLUMN("CHARACTER","X(12)","","Cliente Final",2)
           wh-eq0506b-estab-fim = wh-eq0506b-br-table:ADD-CALC-COLUMN("CHARACTER","X(09)","","Est.Final",5)
           h-query-eq0506b    = wh-eq0506b-br-table:QUERY:GET-BUFFER-HANDLE(1).
    ON 'row-display':U OF wh-eq0506b-br-table PERSISTENT RUN upc/ifeq0506b-u03.p.
END.

IF p-ind-event =  "VALIDA-ALOCACAO" AND
   p-ind-object = "Container"       THEN DO:

/*     MESSAGE                                          */
/*         "p-ind-event :" p-ind-event             skip */
/*         "p-ind-object:" p-ind-object            skip */
/*         "p-wgh-object:" p-wgh-object:FILE-NAME  skip */
/*         "p-wgh-frame :" p-wgh-frame:NAME        skip */
/*         "p-cod-table :" STRING(p-cod-table)     skip */
/*         "p-row-table :" STRING(p-row-table)     skip */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.               */


    FIND FIRST embarque NO-LOCK
         WHERE ROWID(embarque) = p-row-table NO-ERROR.
    IF AVAIL embarque THEN DO:
        ASSIGN l-erro = NO.
        FOR EACH pre-fatur NO-LOCK
           WHERE pre-fatur.cdd-embarq = embarque.cdd-embarq,
           FIRST ped-venda NO-LOCK
           WHERE ped-venda.nome-abrev = pre-fatur.nome-abrev
             AND ped-venda.nr-pedcli  = pre-fatur.nr-pedcli:

            RUN pi-verifica-if-nor.
        END.
        
        IF CAN-FIND(FIRST tt-valid-ped
                    WHERE tt-valid-ped.tipo = 2) THEN DO:
            IF NOT CAN-FIND(FIRST if-permis-alt-ped
                            WHERE if-permis-alt-ped.cod-estabel = embarque.cod-estabel
                              AND if-permis-alt-ped.cod-usuario = c-seg-usuario) THEN DO:

                RUN utp/ut-msgs.p (INPUT "show",
                                   INPUT  17006,
                                   INPUT "NÆo permitido embarque cliente final, dever  ser embarcado apenas o pedido origem.").
    
                RETURN "NOK".
            END.
        END.

        IF CAN-FIND(FIRST tt-valid-ped
            WHERE tt-valid-ped.tipo = 4) THEN DO:
 
            FIND FIRST tt-valid-ped NO-LOCK NO-ERROR.

            RUN utp/ut-msgs.p (INPUT "show",
                               INPUT  17006,
                               INPUT "NÆo permitido embarque com tipo de pedido iqual a E." + " Pedido : " + 
                               tt-valid-ped.nr-pedcli + " Cliente : " + tt-valid-ped.nome-abrev).

            RETURN "NOK".

       END.

       IF CAN-FIND(FIRST tt-valid-ped
           WHERE tt-valid-ped.tipo = 5) THEN DO:

           FIND FIRST tt-valid-ped NO-LOCK NO-ERROR.

           RUN utp/ut-msgs.p (INPUT "show",
                              INPUT  17006,
                              INPUT "NÆo permitido embarque com pedido com condi‡Æo especial vencida." + " Pedido : " + 
                              tt-valid-ped.nr-pedcli + " Cliente : " + tt-valid-ped.nome-abrev).

           RETURN "NOK".

      END.



    END.
END.

PROCEDURE pi-verifica-if-nor :
/*------------------------------------------------------------------------------
  Purpose:     Verifica se existem itens incentivados e itens normais no mesmo pedido
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN l-ped-cond-esp-vencida = NO.
     
    FOR EACH cond-ped WHERE cond-ped.nr-pedido = ped-venda.nr-pedido NO-LOCK:

        IF cond-ped.data-pagto < TODAY THEN
           ASSIGN l-ped-cond-esp-vencida = YES. 

    END.



    FIND FIRST if-ped-venda EXCLUSIVE-LOCK
         WHERE if-ped-venda.nr-pedido         = ped-venda.nr-pedido
           AND if-ped-venda.nr-pedido-relac  <> 0 NO-ERROR.

    /* Pedido Incentivado Origem */
    IF AVAIL if-ped-venda THEN DO:

        CREATE tt-valid-ped.
        ASSIGN tt-valid-ped.cdd-embarq = embarque.cdd-embarq
               tt-valid-ped.nome-abrev  = ped-venda.nome-abrev
               tt-valid-ped.nr-pedcli   = ped-venda.nr-pedcli
               tt-valid-ped.tipo        = 1 /* Incentivado */
               tt-valid-ped.mod-frete   = IF ped-venda.cidade-cif <> "" THEN "CIF" ELSE "FOB".


       /* Busca Frete do tipo final */
       FIND FIRST b-ped-venda
            WHERE b-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac NO-LOCK NO-ERROR.
       IF AVAIL b-ped-venda THEN
           ASSIGN tt-valid-ped.mod-frete = IF b-ped-venda.cidade-cif <> "" THEN "CIF" ELSE "FOB".

    END.
    ELSE IF ped-venda.cond-redespa BEGINS "Pedido original relacionado" and ped-venda.cod-estabel <> "432" THEN DO: /*permite embarcar pedido do 432*/

        CREATE tt-valid-ped.
        ASSIGN tt-valid-ped.cdd-embarq = embarque.cdd-embarq
               tt-valid-ped.nome-abrev  = ped-venda.nome-abrev
               tt-valid-ped.nr-pedcli   = ped-venda.nr-pedcli
               tt-valid-ped.tipo        = 2 /* Incentivado - pedido final */
               tt-valid-ped.mod-frete   = IF ped-venda.cidade-cif <> "" THEN "CIF" ELSE "FOB".

    END.
    ELSE DO:

        CREATE tt-valid-ped.
        ASSIGN tt-valid-ped.cdd-embarq = embarque.cdd-embarq
               tt-valid-ped.nome-abrev  = ped-venda.nome-abrev
               tt-valid-ped.nr-pedcli   = ped-venda.nr-pedcli
               tt-valid-ped.tipo        = IF ped-venda.tp-pedido = "E" THEN 4 ELSE IF l-ped-cond-esp-vencida THEN 5 ELSE 3 /* Normal */
               tt-valid-ped.mod-frete   = IF ped-venda.cidade-cif <> "" THEN "CIF" ELSE "FOB".

    END.

    RETURN "OK".

END PROCEDURE. /* pi-verifica-if-nor */
