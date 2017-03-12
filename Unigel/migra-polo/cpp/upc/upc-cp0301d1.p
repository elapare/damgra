/*--------------------------------------------------------------------------------------------
**    Programa: upc-cp0301d1.p
**    Objetivo: alterar grupo de m†quina conforme relacionamento linha x gm
**    Autor: amgra - Edson
**    Data: 13/07/2009
**
--------------------------------------------------------------------------------------------*/
/* PROGRAMA: UPC-cp0301d1.P
   AUTOR : AMGRA - EDSON
   DATA : MAIO/2007
   DESCRICAO: UPC PARA MOSTRAR RESTRICOES DE CLIENTES X PRODUTOS QUANDO HOUVER.
   
*/



/*----- DEFINICAO DE PARAMETROS ------------------*/
def input parameter p-ind-event  as char          no-undo.
def input parameter p-ind-object as char          no-undo.
def input parameter p-wgh-object as handle        no-undo.
def input parameter p-wgh-frame  as widget-handle no-undo.
def input parameter p-cod-table  as char          no-undo.
def input parameter p-row-table  as rowid         no-undo.


DEF  VAR h-nr-pedido AS handle NO-UNDO.
DEF  VAR h-it-codigo AS handle NO-UNDO.
DEFINE VARIABLE c-nome-abrev AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-it-codigo AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-nr-pedcli AS CHARACTER  NO-UNDO.
DEF VAR wh-frame AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-objeto AS handle NO-UNDO.
DEF VAR wh-it-codigo-esp AS handle NO-UNDO.
DEF VAR wh-cod-refer-esp AS handle NO-UNDO.
DEFINE VARIABLE i-frame AS INTEGER    NO-UNDO.
DEFINE VARIABLE c AS INTEGER    NO-UNDO.
def                   var c-objeto as CHAR          no-undo.

DEFINE NEW GLOBAL SHARED  VARIABLE c-seg-usuario AS CHARACTER  NO-UNDO.
DEFINE VAR erro-ordem          AS INTEGER    NO-UNDO.
DEFINE VAR mens-erro           AS CHARACTER  NO-UNDO.

assign c-objeto = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/").

DEF NEW GLOBAL SHARED VAR h-fi-nr-ord-produ       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fi-cod-estabel        AS WIDGET-HANDLE NO-UNDO.


    
    IF (p-ind-event = 'INITIALIZE' AND
         p-ind-object = 'CONTAINER') THEN DO:
            
            Run pi-busca-widget (Input  "fi-nr-ord-produ", Input  p-wgh-frame, Output h-fi-nr-ord-produ).
            Run pi-busca-widget (Input  "fi-cod-estabel" , Input  p-wgh-frame, Output h-fi-cod-estabel ).
                    
    END.





IF valid-handle(h-fi-cod-estabel) and 
   p-ind-event  = "assign"    AND 
   p-ind-object = "VIEWER"  and  (substring(h-fi-cod-estabel:screen-value,1,2) = "42" OR substring(h-fi-cod-estabel:screen-value,1,2) = "41") THEN DO: /*solic-318*/

    IF NOT CAN-FIND(usuar_grp_usuar WHERE
                          usuar_grp_usuar.cod_grp     = "UIO" AND
                          usuar_grp_usuar.cod_usuario = c-seg-usuario
                          NO-LOCK) THEN DO:
        run utp/ut-msgs.p (input "show":U,
          input 17006,
          input "Vocà n∆o tem autorizaá∆o nesta operaá∆o!~~INCLUS«O, C‡PIA E ALTERAÄ«O somente com permiss∆o especial.").


                     RETURN "NOK".
              END.

END.

IF p-ind-event  = "assign"    AND 
   p-ind-object = "VIEWER"    
    AND c-objeto = "v04in271.w"   THEN DO:

   ASSIGN wh-frame = p-wgh-frame:FIRST-CHILD. /* pegando o Field-Group */
    ASSIGN wh-frame = wh-frame:FIRST-CHILD. /* pegando o 1o. Campo */

    DO WHILE wh-frame <> ? :
       IF wh-frame:type <> "field-group" THEN DO: 
        /*   MESSAGE wh-frame:NAME p-wgh-object:FILE-NAME
               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

          IF wh-frame:NAME = "fi-it-codigo"  THEN DO :
             assign wh-it-codigo-esp = wh-frame.
          END.
          IF wh-frame:NAME = "fi-cod-refer"  THEN DO :
             assign wh-cod-refer-esp = wh-frame.
             LEAVE.
          END.

  
          ASSIGN wh-frame = wh-frame:NEXT-SIBLING.
       END. 
       ELSE
          ASSIGN wh-frame = wh-frame:first-child.
    END.

    FIND ITEM WHERE ITEM.it-codigo = wh-it-codigo-esp:SCREEN-VALUE NO-LOCK NO-ERROR.
    
    IF NOT AVAIL ITEM THEN RETURN.

    IF item.tipo-con-est <> 4 THEN RETURN.

    find first ref-item WHERE 
         ref-item.cod-refer = wh-cod-refer-esp:SCREEN-VALUE AND
         ref-item.it-codigo = wh-it-codigo-esp:SCREEN-VALUE NO-LOCK no-error.
    
    IF AVAIL ref-item THEN RETURN.
    
    run utp/ut-msgs.p (input "show":U,
                          input 17006,
                          input "Referància n∆o existe para este item!").
    
    APPLY "entry" TO wh-cod-refer-esp.
    
    RETURN "nok".
    

END.

    IF p-ind-event = "destroy" AND 
        p-ind-object = "container" THEN DO:
        DO i-frame = 1 TO 2:
            
                                    
            assign h-objeto = p-wgh-frame:first-child
                   c = 0.

                  
            do  while valid-handle(h-objeto):
                 
               /* MESSAGE "objeto: " h-objeto:TYPE h-objeto:NAME 
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
                     iF  h-objeto:NAME = 'f-main'  THEN DO:

                        
                         
                         c = c +  1.
                         IF c = i-frame THEN
                            LEAVE.
                     END.
                       
                if h-objeto:type = "field-group"   THEN
                       h-objeto = h-objeto:first-child.
                ELSE
                       h-objeto = h-objeto:NEXT-SIBLING.
    
            END.
    
             assign h-objeto = h-objeto:first-child.
                  
            do  while valid-handle(h-objeto):
                 
               /* MESSAGE "objeto: " h-objeto:TYPE h-objeto:NAME 
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
                    IF  h-objeto:NAME = "fi-it-codigo" THEN DO:
                        ASSIGN h-it-codigo = h-objeto
                               c-it-codigo = h-it-codigo:SCREEN-VALUE.
                        LEAVE.
                        
                    END.
                    IF  h-objeto:NAME = "fi-nr-pedido" THEN DO:
                         
                        ASSIGN h-nr-pedido = h-objeto
                               c-nr-pedcli = h-nr-pedido:SCREEN-VALUE.
                        Leave.
                    END.
                       
                if h-objeto:type = "field-group"   THEN
                       h-objeto = h-objeto:first-child.
                ELSE
                       h-objeto = h-objeto:NEXT-SIBLING.
    
            END.
    
    
        END.
        
   
       FIND FIRST ped-venda WHERE ped-venda.nr-pedcli = c-nr-pedcli  NO-LOCK NO-ERROR.
        

       IF AVAIL PED-VENDA THEN
          RUN pdp\upc\upc-pd4000-a.p (INPUT ped-venda.cod-estabel, INPUT c-it-codigo, INPUT ped-venda.nome-abrev).
       
    END.

       


IF P-IND-EVENT = "AFTER-END-UPDATE" THEN DO:
   /* MESSAGE 
    p-ind-event          SKIP
    p-ind-object         SKIP
    p-wgh-object         SKIP
    p-wgh-frame          SKIP
    p-cod-table          SKIP
    STRING(p-row-table  )

    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */

    FIND FIRST ord-prod WHERE 
                ROWID(ord-prod) = p-row-table NO-LOCK NO-ERROR.

    IF AVAIL ORD-PROD THEN
       
     FIND first  oper-ord WHERE 
        oper-ord.nr-ord-produ = ord-prod.nr-ord-produ    and
        oper-ord.it-codigo    = ord-prod.it-codigo   AND
        oper-ord.cod-roteiro  = ord-prod.cod-roteiro AND
        oper-ord.estado = 1 AND
        oper-ord.qt-produzida = 0
        NO-LOCK NO-ERROR.
     
     IF NOT AVAIL oper-ord THEN RETURN "ok".

           
          
           RUN cpp\escp0059-a.p (INPUT ord-prod.cod-estabel,
                            INPUT ord-prod.nr-ord-produ ,
                            INPUT ord-prod.nr-linha , 
                            OUTPUT erro-ordem ,
                            OUTPUT mens-erro) NO-ERROR.

END.


Procedure pi-busca-widget:
        Def Input  Param p-nome  As Char.
        Def Input  Param p-frame        As Widget-handle.
        Def Output Param p-object   As Widget-handle.

        Def Var h-frame                  As Widget-handle.
        Def Var wh-objeto                  As Widget-handle.

        Assign h-frame = p-frame:First-child.

        Do While Valid-handle(h-frame):
                If h-frame:Type <> "field-group" Then Do:
                        If h-frame:Type = "frame" Then Do:

                                Run pi-busca-widget(Input  p-nome,
                                                                        Input  h-frame,
                                                                        Output wh-objeto).

                                If wh-objeto <> ? Then Do:
                                        Assign p-object = wh-objeto.
                                        Leave.
                                End.
                        End.

                        If h-frame:Name = p-nome Then Do:
                                Assign p-object = h-frame.
                                Leave.
                        End.

                        Assign h-frame = h-frame:Next-sibling.

                End.
                Else
                        Assign h-frame = h-frame:First-child.
        End.
End Procedure.


