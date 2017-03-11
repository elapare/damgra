/**************************************************************\
***************************************************************
**  Programa: ccp\upc\upc-cp0313.p
**  Objetivo: Programa para Estornar REQS de ordens de reciclado
              quando houver estorno de ACA
**  Autor...: AMGRA- Edson 
**  Data....: 01/02/2007
**  Versao..: I.00.000
***************************************************************
\**************************************************************/

def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-row-table  as rowid         no-undo.
DEF NEW GLOBAL SHARED VAR wh-lotesres   AS handle NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-qryres   AS handle NO-UNDO.
DEFINE VARIABLE h-buffer AS HANDLE     NO-UNDO.
DEFINE VARIABLE i AS INTEGER    NO-UNDO.
DEFINE VARIABLE h-campo AS HANDLE     NO-UNDO.
DEF VAR wh-objeto                AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-nr-ord-produ-amg AS HANDLE     NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-it-codigo-amg    AS HANDLE     NO-UNDO.
DEFINE VARIABLE c-objeto AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-ncampos AS INTEGER    NO-UNDO.
 
DEFINE NEW GLOBAL SHARED VAR h-nr-reporte    AS HANDLE     NO-UNDO.

IF p-ind-event = 'INITIALIZE' AND
       p-ind-object = 'viewer' THEN DO:
    
           ASSIGN wh-objeto = p-wgh-frame:FIRST-CHILD.
           do while valid-handle(wh-objeto):
          
               if wh-objeto:type = "field-group" THEN
                  wh-objeto = wh-objeto:first-child.
               else
                  wh-objeto = wh-objeto:NEXT-SIBLING.
          
              IF VALID-HANDLE(wh-objeto) THEN DO:
               
                  IF wh-objeto:NAME = 'nr-ord-produ' THEN
                    ASSIGN h-nr-ord-produ-amg = wh-objeto.

                  IF wh-objeto:NAME = 'it-codigo' THEN DO:
                   ASSIGN  h-it-codigo-amg = wh-objeto.
                   LEAVE.
                  END.

                  

              END.
          
           END.


END.

IF VALID-HANDLE(h-nr-ord-produ-amg) THEN DO:
    FIND FIRST ord-prod WHERE 
              ord-prod.nr-ord-produ = INT(h-nr-ord-produ-amg:SCREEN-VALUE) and
              ord-prod.estado < 7  NO-LOCK NO-ERROR.

    IF AVAIL ord-prod THEN DO:
        FIND FIRST estabelec WHERE estabelec.cod-estabel = ord-prod.cod-estabel NO-LOCK NO-ERROR.
        

       IF  AVAIL estabelec AND estabelec.ep-codigo <> "420" THEN RETURN.
   end.

END.




IF p-ind-event = 'INITIALIZE' AND
       p-ind-object = 'browser' THEN DO:
    
           ASSIGN wh-objeto = p-wgh-frame:FIRST-CHILD.
           do while valid-handle(wh-objeto):
          
               if wh-objeto:type = "field-group" THEN
                  wh-objeto = wh-objeto:first-child.
               else
                  wh-objeto = wh-objeto:NEXT-SIBLING.
          
              IF VALID-HANDLE(wh-objeto) THEN DO:
              
                   IF wh-objeto:NAME="br_table" THEN DO:
                      ASSIGN wh-lotesres = wh-objeto
                              wh-qryres   = wh-lotesres:QUERY.
                         
                      ASSIGN h-buffer = wh-qryres:GET-BUFFER-HANDLE[1]
                              i-ncampos = h-buffer:NUM-FIELDS.

                      DO i= 1 TO i-ncampos:
                          ASSIGN h-campo = h-buffer:BUFFER-FIELD[i].
                          IF h-campo:NAME = "nr-reporte" THEN DO:
                                ASSIGN h-nr-reporte =  h-campo.
                                LEAVE.
                          END.
                             
                      END.


                   END.

              END.
          
           END.


END.





IF  p-ind-event = "depois-estorno" AND p-ind-object = "bt-ok" THEN DO:
    IF valid-handle(h-it-codigo-amg) THEN DO:
        FIND ITEM WHERE ITEM.IT-CODIGO = h-it-codigo-amg:SCREEN-VALUE  NO-LOCK NO-ERROR.
        IF AVAIL ITEM AND ITEM.ge-codigo = 79  THEN
            RUN cpp\upc\upc-cp0313-a.p.
    END.

    IF valid-handle(h-nr-ord-produ-amg) THEN DO:
         FIND FIRST ord-prod WHERE 
                   ord-prod.nr-ord-produ = INT(h-nr-ord-produ-amg:SCREEN-VALUE) and
                   ord-prod.estado < 7  NO-LOCK NO-ERROR.

         IF AVAIL ord-prod THEN DO:
             FIND FIRST estabelec WHERE estabelec.cod-estabel = ord-prod.cod-estabel NO-LOCK NO-ERROR.
             FIND FIRST lin-prod WHERE lin-prod.nr-linha = ord-prod.nr-linha and
                 lin-prod.cod-estabel = ord-prod.cod-estabel AND
                 lin-prod.sum-requis = 2 NO-LOCK no-error. /* ordem de servi‡o*/
                 
            IF AVAIL lin-prod AND AVAIL estabelec AND estabelec.ep-codigo = "420" THEN DO:
                
                RUN cpp\upc\upc-cp0313-rrq.p.
            END.
                
         END.
             

    END.
    
END.
    
