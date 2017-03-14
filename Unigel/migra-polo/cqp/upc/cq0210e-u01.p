/*--------------------------------------------------------------------------------------------
**    Programa: cq0210e-u01
**    Objetivo: Traz a Qtde Total do Lote Dispon¡vel e Prrence a Unidade de Medida do Item 
**       Autor: Patr¡cia Girotto - Datasul SP
**        Data: 13/01/2004
**
--------------------------------------------------------------------------------------------*/


/*----- DEFINICAO DE PARAMETROS ------------------*/
def input parameter p-ind-event  as char          no-undo.
def input parameter p-ind-object as char          no-undo.
def input parameter p-wgh-object as handle        no-undo.
def input parameter p-wgh-frame  as widget-handle no-undo.
def input parameter p-cod-table  as char          no-undo.
def input parameter p-row-table  as rowid         no-undo.


/*----- Variaveis Globais ------------------------*/
DEF NEW GLOBAL SHARED VAR wh-cq0210e-cod-estabel AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cq0210e-cod-depos   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cq0210e-cod-depos-dest   AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-cq0210e-cod-localiz-dest AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cq0210e-cod-localiz AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-cq0210e-lote        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cq0210e-lote-dest        AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-cq0210e-it-codigo   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cq0210e-cod-refer   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cq0210e-quantidade  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cq0210e-un          AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cq0210e-bt-ok       AS WIDGET-HANDLE NO-UNDO.
def new global shared var wh-button  as widget-handle no-undo.

def new global shared var       c-seg-usuario      as char format "x(12)"  no-undo.
define new global shared var grw-exam-ficha as rowid no-undo.
define buffer b-ficha-cq for ficha-cq.
define new global shared var gqt-ficha-cq as dec no-undo.
define new global shared var grw-ficha-cq as rowid no-undo.
DEFINE VARIABLE i-emitente AS INTEGER     NO-UNDO.



/*----- Variaveis comuns do programa -------------*/
DEF VAR c-handle-obj   AS CHAR NO-UNDO.


/*----- DEFINICAO DE FUNCOES ---------------------*/
{tools/fc-falso.I}
{tools/fc-handle-obj.I}

 
IF  p-ind-object = "CONTAINER":U     AND  
    p-ind-event  = "INITIALIZE":U THEN DO:
    
     c-handle-obj = fc-handle-obj("bt-ok", p-wgh-frame).
     ASSIGN
      wh-cq0210e-bt-ok       = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
                       NO-ERROR.

 
    IF VALID-HANDLE(wh-cq0210e-bt-ok) THEN DO:
        wh-cq0210e-bt-ok:VISIBLE = NO.
        CREATE button wh-button 
          assign frame     = wh-cq0210e-bt-ok:frame
                 width     = wh-cq0210e-bt-ok:WIDTH
                 height    = wh-cq0210e-bt-ok:HEIGHT 
                 row       = wh-cq0210e-bt-ok:ROW
                 col       = wh-cq0210e-bt-ok:COL
                 visible   = yes
                 sensitive = yes
                 Label     = "&OK"
          triggers:
             ON CHOOSE Persistent Run cqp\upc\cq0210e-u01-a.p.
          end triggers.
    
    
    END.

end.

/*-----> Cria Objetos na Tela Principal <-------------------------*/
IF  p-ind-object = "VIEWER":U     AND  
    p-ind-event  = "INITIALIZE":U THEN DO:

    c-handle-obj = fc-handle-obj("cod-estabel,cod-depos,cod-localiz,cod-refer,lote,it-codigo,quantidade,un,c-cod-depos,c-cod-localiz,c-lote", p-wgh-frame).
    ASSIGN wh-cq0210e-cod-estabel = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
           wh-cq0210e-cod-depos   = WIDGET-HANDLE(ENTRY(2,c-handle-obj))
           wh-cq0210e-cod-localiz = WIDGET-HANDLE(ENTRY(3,c-handle-obj))
           wh-cq0210e-cod-refer   = WIDGET-HANDLE(ENTRY(4,c-handle-obj))
           wh-cq0210e-lote        = WIDGET-HANDLE(ENTRY(5,c-handle-obj))
           wh-cq0210e-it-codigo   = WIDGET-HANDLE(ENTRY(6,c-handle-obj))
           wh-cq0210e-quantidade  = WIDGET-HANDLE(ENTRY(7,c-handle-obj)) 
           wh-cq0210e-un          = WIDGET-HANDLE(ENTRY(8,c-handle-obj))
           wh-cq0210e-cod-depos-dest   = WIDGET-HANDLE(ENTRY(9,c-handle-obj))
           wh-cq0210e-cod-localiz-dest      = WIDGET-HANDLE(ENTRY(10,c-handle-obj))
           wh-cq0210e-lote-dest        = WIDGET-HANDLE(ENTRY(11,c-handle-obj))

                       NO-ERROR.
                 
           on "return" of wh-cq0210e-lote-dest
           Persistent Run cqp\upc\cq0210e-u01-a.p.
           
           on "return" of wh-cq0210e-cod-localiz-dest 
           Persistent Run cqp\upc\cq0210e-u01-a.p.
           
            on "return" of wh-cq0210e-cod-depos-dest 
           Persistent Run cqp\upc\cq0210e-u01-a.p.
           
        


                               
END.


IF p-ind-event  = "INITIALIZE":U AND
   p-ind-object = "CONTAINER":U  THEN DO:
   
   FIND ITEM WHERE ITEM.it-codigo = wh-cq0210e-it-codigo:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL ITEM  THEN DO:
      ASSIGN wh-cq0210e-un:SCREEN-VALUE = ITEM.un.
      wh-cq0210e-un:SENSITIVE = NO.
   END.
   
   i-emitente = 0.
   if valid-handle(wh-cq0210e-quantidade) then do:
   
    IF  grw-ficha-cq <> ?  THEN DO:
        gqt-ficha-cq = 0.
        FIND b-ficha-cq WHERE ROWID(b-ficha-cq) = grw-ficha-cq NO-LOCK NO-ERROR.
        IF AVAIL b-ficha-cq  THEN 
            ASSIGN gqt-ficha-cq =   b-ficha-cq.qt-original - b-ficha-cq.qt-aprovada -   b-ficha-cq.qt-rejeitada -   b-ficha-cq.qt-consumida
                i-emitente = b-ficha-cq.cod-emitente.

    
    end.
    
    /*
     if c-seg-usuario = "elapare"    then
       message string(grw-exam-ficha) skip
         STRING(grw-ficha-cq)
       
       view-as alert-box.

       if c-seg-usuario = "elapare" and AVAIL b-ficha-cq  then
       message string(gqt-ficha-cq) skip
       b-ficha-cq.nr-ficha skip
       b-ficha-cq.qt-original
       view-as alert-box.
*/
      
       ASSIGN wh-cq0210e-quantidade:SCREEN-VALUE = STRING(gqt-ficha-cq).
    
   end.
   
                                                                                                                          
      IF AVAIL ITEM AND wh-cq0210e-lote:SCREEN-VALUE <> "" and wh-cq0210e-cod-estabel:SCREEN-VALUE = STRING({cdp\poloestab.i 422}) AND i-emitente = 0 THEN DO: /*solic-318*/ 
         ASSIGN wh-cq0210e-un:SCREEN-VALUE = ITEM.un.
         wh-cq0210e-un:SENSITIVE = NO.
   

        FIND saldo-estoq WHERE
            saldo-estoq.cod-estabel = wh-cq0210e-cod-estabel:SCREEN-VALUE AND 
            saldo-estoq.cod-depos   = wh-cq0210e-cod-depos:SCREEN-VALUE   AND
            saldo-estoq.cod-localiz = wh-cq0210e-cod-localiz:SCREEN-VALUE AND 
            saldo-estoq.lote        = wh-cq0210e-lote:SCREEN-VALUE        AND 
            saldo-estoq.it-codigo   = wh-cq0210e-it-codigo:SCREEN-VALUE   AND 
            saldo-estoq.cod-refer   = wh-cq0210e-cod-refer:SCREEN-VALUE   NO-LOCK NO-ERROR.
        IF AVAIL saldo-estoq THEN DO:
            ASSIGN wh-cq0210e-quantidade:SCREEN-VALUE = STRING(saldo-estoq.qtidade-atu 
                                                    - saldo-estoq.qt-alocada 
                                                    - saldo-estoq.qt-aloc-prod
                                                    - saldo-estoq.qt-aloc-ped).
            wh-cq0210e-quantidade:SENSITIVE = NO.
        END.
        ELSE DO:
            ASSIGN wh-cq0210e-quantidade:SCREEN-VALUE = "0".
            wh-cq0210e-quantidade:SENSITIVE = YES.
        END.
   END.     

   IF AVAIL ITEM AND wh-cq0210e-lote:SCREEN-VALUE <> "" and wh-cq0210e-cod-estabel:SCREEN-VALUE = STRING({cdp\poloestab.i 422}) AND i-emitente > 0 THEN DO: /*solic-318*/ 
         ASSIGN wh-cq0210e-un:SCREEN-VALUE = ITEM.un.
         wh-cq0210e-un:SENSITIVE = NO.
         IF DEC(wh-cq0210e-quantidade:SCREEN-VALUE ) > 0 THEN
                 wh-cq0210e-quantidade:SENSITIVE = NO.
   END.
END.


   
