/***************************************************************************
**
**       Programa: pdu4000-u01.p
**
**       Autor...: Rodrigo Lu°s Frîhlich
**
**       Empresa: Datasul S∆o Paulo
**       
**       Data de Criaá∆o: 03/03/2011
**
**       Objetivo: UPC - pd4000.p
**
*****************************************************************************/ 
{include/i-prgvrs.i pdu4000-u01 2.06.00.000}

DEF INPUT PARAM p-ind-event  AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table  AS CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table  AS ROWID         NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-pd4000-bt-add-order      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-bt-add-order-f    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-bt-delete-order   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-bt-save-order     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-bt-save-order-f   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-bt-cancel-order   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-bt-cancel-order-f AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-nome-abrev        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-nome-abrev-f      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-nome-ab-fim       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-tx-nome-abrev     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-tx-nome-ab-fim    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-nr-pedcli         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-nr-pedcli-f       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-tx-nr-pedcli      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-cod-estabel       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-nat-operacao      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-cod-safra         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-pg-1              AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-pg-0              AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-pg-10             AS WIDGET-HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE wh-pd4000-nome-tr-red   AS WIDGET      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-pd4000-nome-transp   AS WIDGET      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-pd4000-cidade-cif    AS WIDGET      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-pd4000-modalid-frete AS WIDGET      NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-seg-usuario               AS CHAR          NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-c-unid-atend      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-l-aprov-cond-pag  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-txt-unid-atend    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-cod-canal-venda   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-descr-venda       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-cod-cond-pag      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-nr-tab-finan      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-nr-ind-finan      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-container         AS HANDLE        NO-UNDO.
DEF NEW GLOBAL SHARED VAR r-rowid-pd4000              AS ROWID         NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-btsaveitem        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-btsaveitem-f      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-btDeleteItem      AS WIDGET-HANDLE NO-UNDO.
def new global shared var wh-pd4000-cond-redesp       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-ult-event         AS CHARACTER     NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-atu-transp        AS LOGICAL       NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-pd4000-txt-unid-atend-2    AS WIDGET-HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE l-implanta      AS LOGICAL  INIT NO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-window       AS HANDLE         NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl  AS HANDLE         NO-UNDO.
DEFINE VARIABLE wh-pesquisa   AS WIDGET-HANDLE NO-UNDO.

DEFINE VARIABLE h_fpage01    AS HANDLE        NO-UNDO.
DEFINE VARIABLE h_fpage00    AS HANDLE        NO-UNDO. 
DEFINE VARIABLE h_fpage03    AS HANDLE        NO-UNDO.
DEFINE VARIABLE c-image      AS CHAR          NO-UNDO.
DEFINE VARIABLE h-fPage0     AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-fPage1     AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-fPage2     AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-fPage3     AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-fPage4     AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-fPage5     AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-fpage6     AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-fpage8     AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-fPage16    AS HANDLE        NO-UNDO.
DEFINE VARIABLE h-objeto     AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE c-unid-atend AS CHARACTER     NO-UNDO.
DEFINE VARIABLE i-nr-pedido  AS INTEGER       NO-UNDO.

DEF BUFFER b-ped-venda  FOR ped-venda.
DEF BUFFER b2-ped-venda FOR ped-venda.
DEF BUFFER b3-ped-venda FOR ped-venda.

/*WPA - Buffers*/
define buffer if-ped-venda  for espmulti.if-ped-venda.
define buffer if-natur-oper for espmulti.if-natur-oper.
define buffer if-estabelec  for espmulti.if-estabelec.

def buffer b-if-ped-venda for espmulti.if-ped-venda.

/*MESSAGE 
     "p-ind-event :" p-ind-event            SKIP 
     "p-ind-object:" p-ind-object           SKIP 
     /*"p-wgh-object:" p-wgh-object:FILE-NAME SKIP
     "p-wgh-frame :" p-wgh-frame:NAME       SKIP */
     "p-cod-table :" STRING(p-cod-table)    SKIP 
     "p-row-table :" STRING(p-row-table)    SKIP 
 VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

IF p-ind-event =  "before-Initialize" AND
   p-ind-object = "Container"         THEN DO:

    ASSIGN wh-pd4000-container = p-wgh-object
           wh-pd4000-pg-0      = p-wgh-frame 
           h_fpage00           = p-wgh-frame:FIRST-CHILD
           h_fpage00           = h_fpage00:FIRST-CHILD.
   
    DO WHILE VALID-HANDLE(h_fpage00):
        
        IF h_fpage00:TYPE = "frame"  AND 
           h_fpage00:NAME = "Fpage1" THEN DO:
            h_fpage01      = h_fpage00.
            wh-pd4000-pg-1 = h_fpage00.
        END.

        IF h_fpage00:TYPE = "frame" AND
           h_fpage00:NAME = "fpage3" THEN
            ASSIGN h_fpage03 = h_fpage00.
        
        IF h_fpage00:NAME   = "nome-abrev"  THEN
            wh-pd4000-nome-abrev  =  h_fpage00.
        
        IF h_fpage00:NAME   = "nr-pedcli"  THEN
            wh-pd4000-nr-pedcli =  h_fpage00. 
        
        h_fpage00 = h_fpage00:NEXT-SIBLING.
    END.

    ASSIGN h_fpage01 = h_fpage01:FIRST-CHILD
           h_fpage01 = h_fpage01:FIRST-CHILD.
    
    DO WHILE VALID-HANDLE(h_fpage01):
        IF h_fpage01:TYPE = "fill-in"     AND 
           h_fpage01:NAME = "cod-estabel" THEN
            wh-pd4000-cod-estabel = h_fpage01.

        IF h_fpage01:TYPE = "fill-in"     AND 
           h_fpage01:NAME = "nat-operacao" THEN
            wh-pd4000-nat-operacao = h_fpage01.

        IF h_fpage01:TYPE = "fill-in"     AND 
           h_fpage01:NAME = "c-cod-safra" THEN
            wh-pd4000-cod-safra = h_fpage01.

        IF h_fpage01:NAME = "c-desc-cod-canal-venda"  THEN
            wh-pd4000-descr-venda = h_fpage01.

        IF h_fpage01:NAME = "cod-cond-pag"  THEN
            wh-pd4000-cod-cond-pag = h_fpage01.

        IF h_fpage01:NAME = "nr-tab-finan"  THEN
            wh-pd4000-nr-tab-finan = h_fpage01.

        IF h_fpage01:NAME = "nr-ind-finan"  THEN
            wh-pd4000-nr-ind-finan = h_fpage01.

        IF h_fpage01:NAME = "BTADDORDER" THEN
         wh-pd4000-bt-add-order         = h_fpage01.

        IF h_fpage01:NAME = "BTDELETEORDER" THEN
         wh-pd4000-bt-delete-order         = h_fpage01.
        
        IF h_fpage01:TYPE = "BUTTON"      AND 
           h_fpage01:NAME = "BTSAVEORDER" THEN
            ASSIGN wh-pd4000-bt-save-order = h_fpage01.

        IF h_fpage01:TYPE = "BUTTON"        AND 
           h_fpage01:NAME = "BTCANCELORDER" THEN
            wh-pd4000-bt-cancel-order      = h_fpage01.

        h_fpage01 = h_fpage01:NEXT-SIBLING.
    END.

    IF VALID-HANDLE (wh-pd4000-bt-add-order) THEN DO:
     CREATE BUTTON wh-pd4000-bt-add-order-f
     ASSIGN FRAME         = wh-pd4000-pg-1  
            ROW           = wh-pd4000-bt-add-order:ROW 
            COL           = wh-pd4000-bt-add-order:COL 
            WIDTH         = wh-pd4000-bt-add-order:WIDTH 
            HEIGHT        = wh-pd4000-bt-add-order:HEIGHT
            NAME          = "wh-pd4000-bt-add-order-f"
            TOOLTIP       = wh-pd4000-bt-add-order:TOOLTIP
            SENSITIVE     = YES
            VISIBLE       = YES.
   END.

    IF VALID-HANDLE (wh-pd4000-bt-save-order) THEN DO:
        CREATE BUTTON wh-pd4000-bt-save-order-f
        ASSIGN FRAME   = wh-pd4000-pg-1  
             ROW       = wh-pd4000-bt-save-order:ROW 
             COL       = wh-pd4000-bt-save-order:COL 
             WIDTH     = wh-pd4000-bt-save-order:WIDTH 
             HEIGHT    = wh-pd4000-bt-save-order:HEIGHT
             NAME      = "wh-pd4000-bt-save-order-f"
             TOOLTIP   = wh-pd4000-bt-save-order:TOOLTIP
             SENSITIVE = NO
             VISIBLE   = YES.
             
             wh-pd4000-bt-save-order:WIDTH = .1.
              wh-pd4000-bt-save-order:HEIGHT = .1.
    END.

    IF VALID-HANDLE (wh-pd4000-bt-cancel-order) THEN DO:
        CREATE BUTTON wh-pd4000-bt-cancel-order-f
        ASSIGN FRAME         = wh-pd4000-pg-1  
               ROW           = wh-pd4000-bt-cancel-order:ROW 
               COL           = wh-pd4000-bt-cancel-order:COL 
               WIDTH         = wh-pd4000-bt-cancel-order:WIDTH 
               HEIGHT        = wh-pd4000-bt-cancel-order:HEIGHT
               NAME          = "wh-pd4000-bt-cancel-order-f"
               TOOLTIP       = wh-pd4000-bt-cancel-order:TOOLTIP
               SENSITIVE     = NO
               VISIBLE       = YES.
    END.

    IF VALID-HANDLE(wh-pd4000-descr-venda) THEN DO:
        wh-pd4000-descr-venda:WIDTH =  wh-pd4000-descr-venda:WIDTH - 8.
        CREATE FILL-IN  wh-pd4000-c-unid-atend
        ASSIGN FRAME       = wh-pd4000-pg-1
               WIDTH       = 5
               HEIGHT      = 0.80
               ROW         = wh-pd4000-descr-venda:ROW
               COL         = wh-pd4000-descr-venda:COL + 27.7
               FORMAT      = "x(03)"
               NAME        = "wh-pd4000-c-unid-atend"
               SENSITIVE   = NO
               VISIBLE     = YES
               TRIGGERS:
                  ON "MOUSE-SELECT-DBLCLICK":U, "F5":U PERSISTENT RUN upc/pdu4000-u01.p(INPUT "ZOOM"  ,
                                                                                        INPUT "ESTABELECIMENTO",
                                                                                        INPUT p-wgh-object,
                                                                                        INPUT p-wgh-frame ,   
                                                                                        INPUT p-cod-table ,
                                                                                        INPUT p-row-table ).
               END TRIGGERS.

        wh-pd4000-c-unid-atend:MOVE-TO-TOP().
        wh-pd4000-c-unid-atend:LOAD-MOUSE-POINTER("image/lupa.cur":U). 


        CREATE TEXT         wh-pd4000-txt-unid-atend
        ASSIGN FRAME        =   wh-pd4000-pg-1
               ROW          =   wh-pd4000-descr-venda:ROW + .10
               COL          =   wh-pd4000-descr-venda:COL + 19.3
               FORMAT       =   "x(11)"
               NAME         =   "wh-pd4000-txt-unid-atend"
               SENSITIVE    =   YES
               VISIBLE      =   YES
               SCREEN-VALUE =   "Unid.Atend:".


        CREATE TOGGLE-BOX wh-pd4000-l-aprov-cond-pag
        ASSIGN FRAME      = wh-pd4000-pg-1
               LABEL      = "Aprovado"
               FONT       = wh-pd4000-descr-venda:FONT
               WIDTH      = 10
               ROW        = wh-pd4000-descr-venda:ROW - 1
               COLUMN     = wh-pd4000-descr-venda:COL + 32.3
               VISIBLE    = YES
               SENSITIVE  = NO
               TOOLTIP    = "Aprovado a condiá∆o de pagamento no MLA"
               HEIGHT     = 1.
    END.

    ON 'LEAVE':U OF wh-pd4000-cod-cond-pag PERSISTENT RUN upc\pdu4000-u01.p ("LEAVE-COND-PAGTO",
                                                                             "LEAVE-COND-PAGTO",
                                                                             p-wgh-object,
                                                                             p-wgh-frame,
                                                                             p-cod-table,
                                                                             p-row-table).


    ON 'LEAVE':U OF wh-pd4000-nat-operacao PERSISTENT RUN upc\pdu4000-u01.p ("LEAVE-NATUREZA",
                                                                             "LEAVE-NATUREZA",
                                                                             p-wgh-object,
                                                                             p-wgh-frame,
                                                                             p-cod-table,
                                                                             p-row-table).

    ASSIGN c-image = wh-pd4000-bt-add-order:IMAGE-UP.
    wh-pd4000-bt-add-order-f:LOAD-IMAGE-UP(c-image).
    ASSIGN c-image = wh-pd4000-bt-add-order:IMAGE-DOWN.
    wh-pd4000-bt-add-order-f:LOAD-IMAGE-DOWN(c-image).
      
    ASSIGN c-image = wh-pd4000-bt-save-order:IMAGE-UP.
    wh-pd4000-bt-save-order-f:LOAD-IMAGE-UP(c-image).
    ASSIGN c-image = wh-pd4000-bt-save-order:IMAGE-DOWN.
    wh-pd4000-bt-save-order-f:LOAD-IMAGE-DOWN(c-image).

    ASSIGN c-image = wh-pd4000-bt-cancel-order:IMAGE-UP.
    wh-pd4000-bt-cancel-order-f:LOAD-IMAGE-UP(c-image).
    ASSIGN c-image = wh-pd4000-bt-cancel-order:IMAGE-DOWN.
    wh-pd4000-bt-cancel-order-f:LOAD-IMAGE-DOWN(c-image).
    
    IF VALID-HANDLE(wh-pd4000-bt-add-order-f) THEN
      wh-pd4000-bt-add-order-f:MOVE-TO-TOP().
    IF VALID-HANDLE(wh-pd4000-bt-cancel-order-f) THEN
        wh-pd4000-bt-cancel-order-f:MOVE-TO-TOP(). 
    IF VALID-HANDLE(wh-pd4000-bt-save-order-f) THEN
        wh-pd4000-bt-save-order-f:MOVE-TO-TOP().
    IF VALID-HANDLE(wh-pd4000-txt-unid-atend) THEN
        wh-pd4000-txt-unid-atend:SCREEN-VALUE = "Unid.Atend:".

    ON "choose":U OF wh-pd4000-bt-add-order-f PERSISTENT RUN upc/pdu4000-u01.p(INPUT "choose",
                                                                               INPUT "wh-pd4000-bt-add-order-f",
                                                                               INPUT p-wgh-object,
                                                                               INPUT p-wgh-frame,
                                                                               INPUT p-cod-table,
                                                                               INPUT p-row-table).
  
    ON "choose":U OF wh-pd4000-bt-save-order-f PERSISTENT RUN upc/pdu4000-u01.p(INPUT "choose",
                                                                                INPUT "wh-pd4000-bt-save-order-f",
                                                                                INPUT p-wgh-object,
                                                                                INPUT p-wgh-frame,
                                                                                INPUT p-cod-table,
                                                                                INPUT p-row-table).

    ON "choose":U OF wh-pd4000-bt-cancel-order-f PERSISTENT RUN upc/pdu4000-u01.p(INPUT "choose",
                                                                                  INPUT "wh-pd4000-bt-cancel-order-f",
                                                                                  INPUT p-wgh-object,
                                                                                  INPUT p-wgh-frame,
                                                                                  INPUT p-cod-table,
                                                                                  INPUT p-row-table). 

    /*Tabulaá∆o*/                                                                       
    ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
    ASSIGN h-objeto = h-objeto:FIRST-CHILD.
    
    DO  WHILE VALID-HANDLE(h-objeto):
        IF  h-objeto:TYPE <> "field-group" THEN DO:
            /***** HANDLE DAS FRAMES ONDE SER?O INSERIDOS CAMPOS *********/
       
             
            
            IF h-objeto:NAME = "fPage1" THEN
                ASSIGN h-fPage1 = h-objeto.

            IF h-objeto:NAME = "fPage2" THEN
                ASSIGN h-fPage2 = h-objeto.
    
            IF h-objeto:NAME = "fPage3" THEN 
                ASSIGN h-fpage3 = h-objeto.
    
            IF h-objeto:NAME = "fPage4" THEN
                ASSIGN h-fPage4 = h-objeto.

            IF h-objeto:NAME = "fPage5" THEN
                ASSIGN h-fPage5 = h-objeto.
    
            IF h-objeto:NAME = "fPage6" THEN
                ASSIGN h-fPage6 = h-objeto.
    
            IF h-objeto:NAME = "fPage8" THEN
                ASSIGN h-fPage8 = h-objeto.
                
            IF h-objeto:NAME = "fPage10" THEN
                ASSIGN wh-pd4000-pg-10 = h-objeto.     
    
            IF h-objeto:NAME = "fpage16" THEN 
                ASSIGN h-fpage16 = h-objeto.
    
            ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
        END.
        ELSE DO:
            ASSIGN h-objeto = h-objeto:first-child.
        END.
    END.


    ASSIGN h-objeto = wh-pd4000-pg-10:FIRST-CHILD.
    ASSIGN h-objeto = h-objeto:FIRST-CHILD.
    DO  WHILE VALID-HANDLE(h-objeto):
        IF  h-objeto:TYPE <> "field-group" THEN DO:
            /***** HANDLE DOS CAMPOS NA FRAME6 *********/
               
             IF h-objeto:NAME = 'cond-redespa'   THEN DO:    
                 ASSIGN  wh-pd4000-cond-redesp = h-objeto.
                 leave.
             END.
    
            ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
        END.
        ELSE DO:
           ASSIGN h-objeto = h-objeto:FIRST-CHILD.
        END.
    END.


    ASSIGN h-objeto = h-fPage1:FIRST-CHILD.
    ASSIGN h-objeto = h-objeto:FIRST-CHILD.
    DO  WHILE VALID-HANDLE(h-objeto):
        IF  h-objeto:TYPE <> "field-group" THEN DO:
            /***** HANDLE DOS CAMPOS NA FRAME6 *********/
             
             IF h-objeto:NAME = 'cod-canal-venda'   THEN DO:    
                 ASSIGN  wh-pd4000-cod-canal-venda = h-objeto.
             END.
    
            ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
        END.
        ELSE DO:
           ASSIGN h-objeto = h-objeto:FIRST-CHILD.
        END.
    END.

    wh-pd4000-c-unid-atend:MOVE-AFTER-TAB-ITEM(wh-pd4000-cod-canal-venda).

    
    ASSIGN h-objeto = h-fPage3:FIRST-CHILD.
    ASSIGN h-objeto = h-objeto:FIRST-CHILD.
    DO  WHILE VALID-HANDLE(h-objeto):
        IF  h-objeto:TYPE <> "field-group" THEN DO:
            /***** HANDLE DOS CAMPOS NA FRAME6 *********/
             
             IF h-objeto:NAME = 'nome-tr-red'   THEN DO:    
                 ASSIGN  wh-pd4000-nome-tr-red = h-objeto.
                 
             END.
             IF h-objeto:NAME = 'nome-transp'   THEN DO:    
                 ASSIGN  wh-pd4000-nome-transp = h-objeto.
                 
             END.
             IF h-objeto:NAME = 'cidade-cif'   THEN DO:    
                 ASSIGN  wh-pd4000-cidade-cif = h-objeto.
             END.
             IF h-objeto:NAME = 'c-cod-modalid-frete'  THEN DO:    
                 ASSIGN wh-pd4000-modalid-frete = h-objeto.
             END.
            ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
        END.
        ELSE DO:
           ASSIGN h-objeto = h-objeto:FIRST-CHILD.
        END.
    END.

    IF VALID-HANDLE(wh-pd4000-nome-abrev) THEN DO:
        
        /* Cliente */
        CREATE FILL-IN  wh-pd4000-nome-abrev-f
        ASSIGN FRAME       = wh-pd4000-pg-0
               WIDTH       = wh-pd4000-nome-abrev:WIDTH - 2
               HEIGHT      = wh-pd4000-nome-abrev:HEIGHT
               ROW         = wh-pd4000-nome-abrev:ROW
               COL         = wh-pd4000-nome-abrev:COL - 4.8
               FORMAT      = wh-pd4000-nome-abrev:FORMAT
               NAME        = wh-pd4000-nome-abrev:NAME
               SENSITIVE   = NO
               VISIBLE     = YES
               TRIGGERS:
                  ON "MOUSE-SELECT-DBLCLICK":U, "F5":U PERSISTENT RUN upc/pdu4000-u01.p(INPUT "ZOOM"  ,
                                                                                        INPUT "CLIENTE",
                                                                                        INPUT p-wgh-object,
                                                                                        INPUT p-wgh-frame ,   
                                                                                        INPUT p-cod-table ,
                                                                                        INPUT p-row-table ).
               END TRIGGERS.

        wh-pd4000-nome-abrev-f:MOVE-TO-TOP().
        wh-pd4000-nome-abrev-f:LOAD-MOUSE-POINTER("image/lupa.cur":U). 


        CREATE TEXT         wh-pd4000-tx-nome-abrev
        ASSIGN FRAME        =   wh-pd4000-pg-0
               ROW          =   wh-pd4000-nome-abrev:ROW + .13
               COL          =   wh-pd4000-nome-abrev:COL - 10.5
               FORMAT       =   "x(8)"
               NAME         =   "wh-pd4000-tx-nome-abrev"
               SENSITIVE    =   YES
               VISIBLE      =   YES
               SCREEN-VALUE =   "Cliente:".

        /* Cliente final */
        CREATE FILL-IN  wh-pd4000-nome-ab-fim
        ASSIGN FRAME       = wh-pd4000-pg-0
               WIDTH       = wh-pd4000-nome-abrev:WIDTH - 2
               HEIGHT      = wh-pd4000-nome-abrev:HEIGHT
               ROW         = wh-pd4000-nome-abrev:ROW
               COL         = wh-pd4000-nome-abrev:COL + 13
               FORMAT      = wh-pd4000-nome-abrev:FORMAT
               NAME        = wh-pd4000-nome-abrev:NAME
               SENSITIVE   = NO
               VISIBLE     = YES.

        CREATE TEXT         wh-pd4000-tx-nome-ab-fim
        ASSIGN FRAME        =   wh-pd4000-pg-0
               ROW          =   wh-pd4000-nome-abrev:ROW + .13
               COL          =   wh-pd4000-nome-abrev:COL + 7.5
               FORMAT       =   "x(8)"
               NAME         =   "wh-pd4000-tx-nome-ab-fim"
               SENSITIVE    =   YES
               VISIBLE      =   YES
               SCREEN-VALUE =   "Cli Fim:".
    END.

    IF VALID-HANDLE(wh-pd4000-nr-pedcli) THEN DO:
        
        CREATE FILL-IN  wh-pd4000-nr-pedcli-f
        ASSIGN FRAME       = wh-pd4000-pg-0
               WIDTH       = wh-pd4000-nr-pedcli:WIDTH - 4
               HEIGHT      = wh-pd4000-nr-pedcli:HEIGHT
               ROW         = wh-pd4000-nr-pedcli:ROW
               COL         = wh-pd4000-nr-pedcli:COL + 4.7
               FORMAT      = wh-pd4000-nr-pedcli:FORMAT
               NAME        = wh-pd4000-nr-pedcli:NAME
               SENSITIVE   = NO
               VISIBLE     = YES
               TRIGGERS:
                  ON "MOUSE-SELECT-DBLCLICK":U, "F5":U PERSISTENT RUN upc/pdu4000-u01.p(INPUT "ZOOM"  ,
                                                                                        INPUT "CLIENTE",
                                                                                        INPUT p-wgh-object,
                                                                                        INPUT p-wgh-frame ,   
                                                                                        INPUT p-cod-table ,
                                                                                        INPUT p-row-table ).
               END TRIGGERS.

        wh-pd4000-nr-pedcli-f:MOVE-TO-TOP().
        wh-pd4000-nr-pedcli-f:LOAD-MOUSE-POINTER("image/lupa.cur":U). 


        CREATE TEXT         wh-pd4000-tx-nr-pedcli
        ASSIGN FRAME        =   wh-pd4000-pg-0
               ROW          =   wh-pd4000-nr-pedcli:ROW + .13
               COL          =   wh-pd4000-nr-pedcli:COL - 4.1
               FORMAT       =   "x(12)"
               NAME         =   "wh-pd4000-tx-nr-pedcli"
               SENSITIVE    =   YES
               VISIBLE      =   YES
               SCREEN-VALUE =   "Ped Cliente:".
    END.


    wh-pd4000-btsaveitem = ?.
    wh-pd4000-btDeleteItem = ?.

    ASSIGN h-objeto = h-fPage6:FIRST-CHILD.
    ASSIGN h-objeto = h-objeto:FIRST-CHILD.
    DO  WHILE VALID-HANDLE(h-objeto):
        IF  h-objeto:TYPE <> "field-group" THEN DO:
            /***** HANDLE DOS CAMPOS NA FRAME6 *********/
    
             IF h-objeto:NAME = 'btsaveitem'   THEN DO:    
                 ASSIGN  wh-pd4000-btsaveitem = h-objeto.
             END.

             IF h-objeto:NAME = 'btDeleteItem'   THEN DO:    
                 ASSIGN  wh-pd4000-btDeleteItem = h-objeto.
             END.
    
            ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
        END.
        ELSE DO:
           ASSIGN h-objeto = h-objeto:FIRST-CHILD.
        END.
    END.

    IF VALID-HANDLE(wh-pd4000-btsaveitem)   THEN DO:
   
    
        CREATE BUTTON wh-pd4000-btsaveitem-f
        ASSIGN FRAME   = wh-pd4000-btsaveitem:frame  
             ROW       = wh-pd4000-btsaveitem:ROW 
             COL       = wh-pd4000-btsaveitem:COL 
             WIDTH     = wh-pd4000-btsaveitem:WIDTH 
             HEIGHT    = wh-pd4000-btsaveitem:HEIGHT
             NAME      = "wh-pd4000-btsaveitem-f"
             TOOLTIP   = wh-pd4000-btsaveitem:TOOLTIP
             SENSITIVE = NO
             VISIBLE   = YES.
             
         wh-pd4000-btsaveitem:width = .1.    
         wh-pd4000-btsaveitem:height = .1.

        ASSIGN c-image = wh-pd4000-btsaveitem:IMAGE-UP.
        wh-pd4000-btsaveitem-f:LOAD-IMAGE-UP(c-image).
        ASSIGN c-image = wh-pd4000-btsaveitem:IMAGE-DOWN.
        wh-pd4000-btsaveitem-f:LOAD-IMAGE-DOWN(c-image).

       ON "choose":U OF wh-pd4000-btsaveitem-f PERSISTENT RUN upc/pdu4000-u01.p(INPUT "choose",
                                                                                INPUT "wh-pd4000-btsaveitem-f",
                                                                                INPUT p-wgh-object,
                                                                                INPUT p-wgh-frame,
                                                                                INPUT p-cod-table,
                                                                                INPUT p-row-table).
    END.

    

END.

IF p-ind-event = "LEAVE-NATUREZA" AND p-ind-object = "LEAVE-NATUREZA" THEN DO:

    RUN pi-NatOperacao in p-wgh-object.
    run pi-CodCondPag       in p-wgh-object.

    IF wh-pd4000-cod-cond-pag:SCREEN-VALUE = "0" THEN
        ASSIGN wh-pd4000-nr-tab-finan:SCREEN-VALUE = "1"
               wh-pd4000-nr-ind-finan:SCREEN-VALUE = "1".
END.

IF p-ind-event = "LEAVE-COND-PAGTO" AND p-ind-object = "LEAVE-COND-PAGTO" THEN DO:

    run pi-CodCondPag in p-wgh-object.

    IF wh-pd4000-cod-cond-pag:SCREEN-VALUE = "0" THEN
        ASSIGN wh-pd4000-nr-tab-finan:SCREEN-VALUE = "1"
               wh-pd4000-nr-ind-finan:SCREEN-VALUE = "1".

END.

IF p-ind-event = "AfterClickTreeView" AND wh-pd4000-ult-event = "AfterDisplayOrder" AND VALID-HANDLE(wh-pd4000-bt-delete-order) THEN DO:

    FIND FIRST ext-user-coml NO-LOCK
         WHERE ext-user-coml.usuario = c-seg-usuario NO-ERROR.
    IF AVAIL ext-user-coml AND ext-user-coml.ind-elim-pedido THEN
        ASSIGN wh-pd4000-bt-delete-order:VISIBLE = YES.
    ELSE
        ASSIGN wh-pd4000-bt-delete-order:VISIBLE = NO.
END.

IF p-ind-event = "AfterClickTreeView" AND wh-pd4000-ult-event = "AfterDisplayItem" AND VALID-HANDLE(wh-pd4000-bt-delete-order) THEN DO:

    FIND FIRST ext-user-coml NO-LOCK
         WHERE ext-user-coml.usuario = c-seg-usuario NO-ERROR.
    IF AVAIL ext-user-coml AND ext-user-coml.ind-elim-pedido THEN
        ASSIGN wh-pd4000-btDeleteItem:VISIBLE = YES.
    ELSE
        ASSIGN wh-pd4000-btDeleteItem:VISIBLE = NO.
END.

IF  p-ind-event   = "choose"               AND 
   (p-ind-object  = "wh-pd4000-bt-add-order-f")  THEN DO:

    wh-pd4000-txt-unid-atend:SCREEN-VALUE = "Unid.Atend:".

    wh-pd4000-nome-abrev:VISIBLE     = YES.
    wh-pd4000-nr-pedcli:VISIBLE      = YES.
    wh-pd4000-nome-abrev-f:VISIBLE   = NO.
    wh-pd4000-nome-ab-fim:VISIBLE    = NO.
    wh-pd4000-tx-nome-abrev:VISIBLE  = NO.
    wh-pd4000-tx-nome-ab-fim:VISIBLE = NO.
    wh-pd4000-nr-pedcli-f:VISIBLE    = NO.
    wh-pd4000-tx-nr-pedcli:VISIBLE   = NO.

   IF p-ind-object = "wh-pd4000-bt-add-order-f" THEN 
      APPLY  "choose"  TO  wh-pd4000-bt-add-order.
   
   ASSIGN wh-pd4000-txt-unid-atend:SCREEN-VALUE = "Unid.Atend:".

END.

IF  p-ind-event   = "choose"                     AND
    p-ind-object  = "wh-pd4000-bt-save-order-f"  THEN DO:
    
    wh-pd4000-txt-unid-atend:SCREEN-VALUE = "Unid.Atend:".

    ASSIGN wh-pd4000-atu-transp = NO.



    FIND FIRST ped-venda exclusive-LOCK
         WHERE ped-venda.nome-abrev = wh-pd4000-nome-abrev:SCREEN-VALUE
           AND ped-venda.nr-pedcli  = wh-pd4000-nr-pedcli:SCREEN-VALUE NO-ERROR.
    IF AVAIL ped-venda THEN DO:

        fIND FIRST if-ped-venda NO-LOCK
             WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido AND if-ped-venda.nr-pedido-relac <> 0 NO-ERROR.
        IF  AVAIL if-ped-venda THEN DO:
            IF (ped-venda.cod-estabel <> "422"  AND wh-pd4000-cod-estabel:SCREEN-VALUE <> "422") OR (ped-venda.cod-estabel <> "412"  AND wh-pd4000-cod-estabel:SCREEN-VALUE <> "412")   THEN DO:  /*solic-318*/ 


               FIND FIRST if-estabelec NO-LOCK
                        WHERE if-estabelec.cod-estab-orig  = wh-pd4000-cod-estabel:SCREEN-VALUE
                          AND if-estabelec.cod-estab-inter = ""  NO-ERROR.
               IF AVAIL if-estabelec  THEN DO:

                   ASSIGN  wh-pd4000-c-unid-atend:SCREEN-VALUE = if-estabelec.cod-estab-dest.
                          /* if-ped-venda.cod-estab-atend = if-estabelec.cod-estab-dest.*/
            
                       FIND FIRST if-natur-oper OF if-estabelec
                            WHERE if-natur-oper.nat-oper-pedido = if-ped-venda.nat-oper-orig NO-LOCK NO-ERROR.
            
                       if avail if-natur-oper then  ASSIGN wh-pd4000-nat-operacao:SCREEN-VALUE  = if-natur-oper.nat-oper-v-ung.

               END.

            END.
           
        END.
    END.

    IF wh-pd4000-c-unid-atend:SCREEN-VALUE <> "" THEN DO:
        FIND FIRST estabelec NO-LOCK
             WHERE estabelec.cod-estabel = wh-pd4000-c-unid-atend:SCREEN-VALUE NO-ERROR.
    
        IF NOT AVAIL estabelec THEN DO:
            RUN utp/ut-msgs.p (INPUT "show",
                               INPUT  15825,
                               INPUT "Unidade de Atendimento n∆o encontrado.~~Unidade de Atendimento n∆o encontrado.").
            RETURN NO-APPLY.
        END.
        ELSE DO:

            /* Cria a tabela de relacionamento */

            IF wh-pd4000-cod-estabel:SCREEN-VALUE = wh-pd4000-c-unid-atend:SCREEN-VALUE THEN DO:
                RUN utp/ut-msgs.p (INPUT "show",
                                   INPUT  15825,
                                   INPUT "Unidade de Atendimento n∆o pode ser o mesmo estabelecimento de origem.").
                RETURN NO-APPLY.
            END.

            RUN pi-valid-campos.
 

            IF RETURN-VALUE = "NO-ERROR":U THEN RETURN NO-APPLY.

            ASSIGN c-unid-atend = TRIM(wh-pd4000-c-unid-atend:SCREEN-VALUE).
            
            FIND FIRST ped-venda exclusive-LOCK
                 WHERE ped-venda.nome-abrev = wh-pd4000-nome-abrev:SCREEN-VALUE
                   AND ped-venda.nr-pedcli  = wh-pd4000-nr-pedcli:SCREEN-VALUE NO-ERROR.
            IF AVAIL ped-venda THEN DO:

                fIND FIRST if-ped-venda no-LOCK
                     WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-ERROR.
                IF NOT AVAIL if-ped-venda THEN DO:
                    ped-venda.completo = no.
                end.
                ELSE IF if-ped-venda.cod-estab-atend <> c-unid-atend AND ped-venda.cod-sit-ped = 1 AND ped-venda.cod-estabel <> "999" THEN DO:
                    /* Troca Natureza e cliente para unigel comercial polo */
                    RUN pi-troca-unid-polo.
                     IF RETURN-VALUE = "NO-ERROR":U THEN RETURN NO-APPLY.
                END.
                
                
                
            end.

            APPLY "choose" TO wh-pd4000-bt-save-order.

            FIND FIRST ped-venda NO-LOCK
                 WHERE ped-venda.nome-abrev = wh-pd4000-nome-abrev:SCREEN-VALUE
                   AND ped-venda.nr-pedcli  = wh-pd4000-nr-pedcli:SCREEN-VALUE NO-ERROR.
                   
               
  
                   
            IF AVAIL ped-venda THEN DO:
            
            
            
                fIND FIRST if-ped-venda no-LOCK
                        WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido NO-ERROR.
                   IF  AVAIL if-ped-venda THEN DO: 
                        find first b-ped-venda where b-ped-venda.nr-pedido =  if-ped-venda.nr-pedido no-lock no-error.
                       
                        if avail if-ped-venda and avail b-ped-venda and (b-ped-venda.cod-estabel = "422" OR b-ped-venda.cod-estabel = "412") then run pi-replica-data-tipo-filho. /*solic-318*/ 
                                    
                   END.
                   else do:
                   
                        fIND FIRST if-ped-venda no-LOCK
                             WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido and if-ped-venda.nr-pedido-relac <> 0 NO-ERROR.
                        IF  AVAIL if-ped-venda THEN DO:
                             if avail if-ped-venda and (ped-venda.cod-estabel = "422" OR  ped-venda.cod-estabel = "412") then run pi-replica-data-tipo. /*solic-318*/ 
                
                        end.
           
                    
                
                    
                    END.

            
               
                ASSIGN wh-pd4000-c-unid-atend:SENSITIVE      = NO
                       wh-pd4000-bt-save-order-f:SENSITIVE   = IF p-ind-object  = "wh-pd4000-bt-save-order-f" THEN NO ELSE YES. 
                       wh-pd4000-txt-unid-atend:SCREEN-VALUE = "Unid.Atend:".
            
                IF wh-pd4000-bt-save-order:SENSITIVE = YES THEN do:
                    ASSIGN wh-pd4000-c-unid-atend:SENSITIVE = YES.
                END.                            
           

                FIND FIRST if-ped-venda EXCLUSIVE-LOCK
                     WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-ERROR.
                IF NOT AVAIL if-ped-venda THEN DO:
                    CREATE if-ped-venda.
                    ASSIGN if-ped-venda.nr-pedido  = ped-venda.nr-pedido
                           if-ped-venda.nome-abrev = ped-venda.nome-abrev
                           if-ped-venda.nr-pedcli  = ped-venda.nr-pedcli.
                END.
        
                ASSIGN if-ped-venda.cod-estab-atend = c-unid-atend.
            END.
        END.
    END.
    ELSE DO:
        
        /* N∆o deixa alterar a unidade de atendimento uma vez o pedido completo */
        RUN pi-valid-campos.

 
        IF RETURN-VALUE = "NO-ERROR":U THEN RETURN NO-APPLY.

        APPLY "choose" TO wh-pd4000-bt-save-order.

          FIND FIRST ped-venda NO-LOCK
                 WHERE ped-venda.nome-abrev = wh-pd4000-nome-abrev:SCREEN-VALUE
                   AND ped-venda.nr-pedcli  = wh-pd4000-nr-pedcli:SCREEN-VALUE NO-ERROR.
                   
               
  
                   
            IF AVAIL ped-venda THEN DO:
            
            
            
                fIND FIRST if-ped-venda no-LOCK
                        WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido NO-ERROR.
                   IF  AVAIL if-ped-venda THEN DO: 
                        find first b-ped-venda where b-ped-venda.nr-pedido =  if-ped-venda.nr-pedido no-lock no-error.
                       
                        if avail if-ped-venda and avail b-ped-venda and (b-ped-venda.cod-estabel = "422" OR b-ped-venda.cod-estabel = "412") then run pi-replica-data-tipo-filho. /*solic-318*/ 
                                    
                   END.
                   else do:
                   
                        fIND FIRST if-ped-venda no-LOCK
                             WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido and if-ped-venda.nr-pedido-relac <> 0 NO-ERROR.
                        IF  AVAIL if-ped-venda THEN DO:
                             if avail if-ped-venda and (ped-venda.cod-estabel = "422" OR ped-venda.cod-estabel = "412") then run pi-replica-data-tipo. /*solic-318*/ 
                
                        end.
           
                    
                
                    
                    END.


        
             end.
    END.
END.

IF  p-cod-table = "ped-venda"  AND 
    p-row-table <>  ?          THEN DO:
    ASSIGN r-rowid-pd4000 = p-row-table.
END.

IF p-ind-event   = "AfterClickTreeView"  AND
   p-ind-object  = "container"          THEN DO:

    FIND FIRST b-ped-venda 
         WHERE ROWID(b-ped-venda) = r-rowid-pd4000 NO-LOCK NO-ERROR.
    IF AVAIL b-ped-venda THEN DO:
        FIND FIRST if-ped-venda NO-LOCK
             WHERE if-ped-venda.nr-pedido = b-ped-venda.nr-pedido NO-ERROR.
        IF AVAIL if-ped-venda THEN
            ASSIGN wh-pd4000-c-unid-atend:SCREEN-VALUE = if-ped-venda.cod-estab-atend.
        ELSE
            ASSIGN wh-pd4000-c-unid-atend:SCREEN-VALUE = "".

        RUN pi-visualiz-cli.

        /*FIND FIRST mla-cond-pagto-ped
             WHERE mla-cond-pagto-ped.nr-pedido = b-ped-venda.nr-pedido NO-LOCK NO-ERROR.
        IF AVAIL mla-cond-pagto-ped THEN*/

        FIND FIRST estabelec
             WHERE estabelec.cod-estabel = b-ped-venda.cod-estabel NO-LOCK NO-ERROR.


        FIND LAST mla-doc-pend-aprov 
            WHERE mla-doc-pend-aprov.ep-codigo    = estabelec.ep-codigo
              AND mla-doc-pend-aprov.cod-estabel  = estabelec.cod-estabel
              AND mla-doc-pend-aprov.cod-tip-doc  = 519
              AND mla-doc-pend-aprov.chave-doc    = STRING(b-ped-venda.nr-pedido) NO-LOCK NO-ERROR.
        IF AVAIL mla-doc-pend-aprov THEN
            ASSIGN wh-pd4000-l-aprov-cond-pag:CHECKED = (mla-doc-pend-aprov.ind-situacao = 2).
        ELSE
            ASSIGN wh-pd4000-l-aprov-cond-pag:CHECKED = YES.
    END.
END.

IF p-ind-event  = "ZOOM"            AND
   p-ind-object = "ESTABELECIMENTO" THEN DO:

    ASSIGN l-implanta = YES.
    {include/zoomvar.i &prog-zoom=adzoom/z04ad107.w
                       &proghandle   = wh-window
                       &campohandle  = wh-pd4000-c-unid-atend
                       &campozoom    = cod-estabel}
END.


ASSIGN wh-pd4000-bt-add-order-f:SENSITIVE    = wh-pd4000-bt-add-order:SENSITIVE       
       wh-pd4000-bt-save-order-f:SENSITIVE   = wh-pd4000-bt-save-order:SENSITIVE
       wh-pd4000-c-unid-atend:SENSITIVE      = wh-pd4000-bt-save-order:SENSITIVE
       wh-pd4000-bt-cancel-order-f:SENSITIVE = wh-pd4000-bt-cancel-order:SENSITIVE           
       wh-pd4000-btsaveitem-f:SENSITIVE   = wh-pd4000-btsaveitem:SENSITIVE
        .
        
if valid-handle(wh-pd4000-cond-redesp)      then 
    wh-pd4000-cond-redesp:sensitive = no.

 
if p-ind-event = "pi-enable" and index(  program-name(4) + program-name(5) , "btupdate") > 0 then do:
    FIND FIRST b-ped-venda 
          WHERE ROWID(b-ped-venda) = r-rowid-pd4000 NO-LOCK NO-ERROR.
    IF AVAIL b-ped-venda  and (b-ped-venda.cod-estabel = "422" OR b-ped-venda.cod-estabel = "412")  THEN DO: /*solic-318*/ 
         FIND FIRST b-if-ped-venda NO-LOCK
              WHERE b-if-ped-venda.nr-pedido = b-ped-venda.nr-pedido NO-ERROR.
         IF AVAIL b-if-ped-venda and  b-if-ped-venda.nr-pedido-relac <> 0 THEN
             ASSIGN /*wh-pd4000-c-unid-atend:SENSITIVE = no*/
                    wh-pd4000-cod-estabel:sensitive = NO
                    wh-pd4000-nat-operacao:sensitive = no.
        
    end.
end.

IF VALID-HANDLE(wh-pd4000-cod-safra) THEN
    ASSIGN wh-pd4000-cod-safra:VISIBLE = NO.

if p-ind-event  = "choose" and
   p-ind-object = "wh-pd4000-bt-cancel-order-f" then do:

    run pi-btcancelorder in p-wgh-object.
   
    FIND FIRST b-ped-venda 
         WHERE ROWID(b-ped-venda) = r-rowid-pd4000 NO-LOCK NO-ERROR.
    IF AVAIL b-ped-venda THEN DO:
        FIND FIRST if-ped-venda NO-LOCK
             WHERE if-ped-venda.nr-pedido = b-ped-venda.nr-pedido NO-ERROR.
        IF AVAIL if-ped-venda THEN
            ASSIGN wh-pd4000-c-unid-atend:SCREEN-VALUE = if-ped-venda.cod-estab-atend.
        ELSE
            ASSIGN wh-pd4000-c-unid-atend:SCREEN-VALUE = "".

        RUN pi-visualiz-cli.
    END.
end.



IF  p-ind-event   = "choose"                     AND
    p-ind-object  = "wh-pd4000-btsaveitem-f"  THEN DO:

    APPLY "choose"  TO wh-pd4000-btsaveitem.


    FIND FIRST ped-venda NO-LOCK
                 WHERE ped-venda.nome-abrev = wh-pd4000-nome-abrev:SCREEN-VALUE
                   AND ped-venda.nr-pedcli  = wh-pd4000-nr-pedcli:SCREEN-VALUE NO-ERROR.
    IF AVAIL ped-venda THEN DO:
    
        fIND FIRST if-ped-venda no-LOCK
             WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-ERROR.
        IF  AVAIL if-ped-venda THEN DO:
             if avail if-ped-venda and (ped-venda.cod-estabel = "422" OR ped-venda.cod-estabel = "412") then run pi-replica-data-tipo. /*solic-318*/ 
                         
        END.
        fIND FIRST if-ped-venda no-LOCK
             WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido NO-ERROR.
        IF  AVAIL if-ped-venda THEN DO: 
             find first b-ped-venda where b-ped-venda.nr-pedido =  if-ped-venda.nr-pedido no-lock no-error.
            
             if avail if-ped-venda and avail b-ped-venda and (b-ped-venda.cod-estabel = "422" OR b-ped-venda.cod-estabel = "412")  then run pi-replica-data-tipo-filho. /*solic-318*/ 
                         
        END.
    END.
END.

IF p-ind-event =  "btSaveOrder" AND
   p-ind-object = "Viewer"      AND 
   wh-pd4000-atu-transp         THEN DO:

    FIND FIRST b-ped-venda 
         WHERE ROWID(b-ped-venda) = r-rowid-pd4000 NO-LOCK NO-ERROR.
    IF AVAIL b-ped-venda THEN DO:


        FIND FIRST if-ped-venda NO-LOCK
             WHERE if-ped-venda.nr-pedido = b-ped-venda.nr-pedido NO-ERROR.

        IF AVAIL if-ped-venda AND if-ped-venda.nr-pedido-relac <> 0 THEN
            ASSIGN wh-pd4000-nome-tr-red:SCREEN-VALUE   = ""
                   wh-pd4000-nome-transp:SCREEN-VALUE   = ""
                   wh-pd4000-cidade-cif:SCREEN-VALUE    = ""
                   wh-pd4000-modalid-frete:SCREEN-VALUE = "9".
    END.

END.

ASSIGN wh-pd4000-ult-event = p-ind-event.

RETURN "OK".

PROCEDURE pi-troca-unid-polo :
/*------------------------------------------------------------------------------
  Purpose:    Troca unidade de atendimento Polo
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    define var l-fora as logical no-undo.

    define buffer buf-estabelec for estabelec .
    define buffer buf-emitente for emitente .
    define buffer b3-ped-item for ped-item.
    
    /* Troca Natureza e cliente para unigel comercial polo */
    FIND FIRST if-natur-oper
         WHERE if-natur-oper.cod-estab-orig  = wh-pd4000-cod-estabel:SCREEN-VALUE
           AND if-natur-oper.cod-estab-inter = ""
           AND if-natur-oper.cod-estab-dest  = c-unid-atend
           AND if-natur-oper.nat-oper-pedido = if-ped-venda.nat-oper-orig NO-LOCK NO-ERROR.
    IF AVAIL if-natur-oper THEN DO:
        ASSIGN ped-venda.nat-operacao = if-natur-oper.nat-oper-v-ung.

        FIND FIRST b3-ped-venda
             WHERE b3-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL b3-ped-venda THEN DO:
            ASSIGN b3-ped-venda.completo    = NO
                   b3-ped-venda.cod-estabel = if-natur-oper.cod-estab-dest.

            find first buf-estabelec where buf-estabelec.cod-estabel = b3-ped-venda.cod-estabel no-lock no-error.
            find first buf-emitente  where buf-emitente.cod-emitente = b3-ped-venda.cod-emitente no-lock no-error.
        
            l-fora = (buf-estabelec.estado <> buf-emitente.estado).
        
            ASSIGN b3-ped-venda.nat-operacao = (if l-fora and nat-oper-venda-inter <> "" then nat-oper-venda-inter else if-natur-oper.nat-oper-venda).
            for each b3-ped-item of b3-ped-venda exclusive-lock.
                assign b3-ped-item.nat-operacao = b3-ped-venda.nat-operacao. 
            end.
        END.

    END.
    else do:
       /* FOR FIRST ped-venda no-LOCK
           WHERE ped-venda.nome-abrev = wh-pd4000-nome-abrev:SCREEN-VALUE
             AND ped-venda.nr-pedcli  = wh-pd4000-nr-pedcli:SCREEN-VALUE
             and ped-venda.cod-estabel = wh-pd4000-cod-estabel:SCREEN-VALUE.
         */    
           find FIRST if-ped-venda exclusive-LOCK
           WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido no-error.
                 
           if avail if-ped-venda then do:
           
                   assign wh-pd4000-c-unid-atend:SCREEN-VALUE = if-ped-venda.cod-estab-atend. 
                   
                     RUN utp/ut-msgs.p (INPUT "show",
                                   INPUT  15825,
                                   INPUT "N∆o Ç permitido alterar unidade de atendimento para " + c-unid-atend + "~~falta relacionamento com CFOP original " + if-ped-venda.nat-oper-orig + ", solicite cadastramento.").
                     RETURN "NO-ERROR":U.

           end.  
              
            
           /*
        end.
   */

    end.

END PROCEDURE. /* pi-troca-unid-polo */

PROCEDURE pi-valid-campos :
/*------------------------------------------------------------------------------
  Purpose:    atualiza pedido relacionado
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE cListaEmb AS CHARACTER   NO-UNDO.
    define var l-parcial as logical no-undo.
    
    
     if (wh-pd4000-cod-estabel:SCREEN-VALUE = "422" OR wh-pd4000-cod-estabel:SCREEN-VALUE = "412") and wh-pd4000-c-unid-atend:SCREEN-VALUE = "" then do: /*solic-318*/ 
       FOR FIRST ped-venda EXCLUSIVE-LOCK
           WHERE ped-venda.nome-abrev = wh-pd4000-nome-abrev:SCREEN-VALUE
             AND ped-venda.nr-pedcli  = wh-pd4000-nr-pedcli:SCREEN-VALUE
             and ped-venda.cod-estabel = wh-pd4000-cod-estabel:SCREEN-VALUE.
             
           find FIRST if-ped-venda exclusive-LOCK
           WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido no-error.
                 
           if avail if-ped-venda then do:
           
                if if-ped-venda.nr-pedido-relac = 0  then do:
                  delete if-ped-venda.
                      
                  return "ok".
                 end.
                 else   assign wh-pd4000-c-unid-atend:SCREEN-VALUE = if-ped-venda.cod-estab-atend. 
           end.  
              
            
           
        end.
    end.

     FOR FIRST ped-venda EXCLUSIVE-LOCK
        WHERE ped-venda.nome-abrev = wh-pd4000-nome-abrev:SCREEN-VALUE
          AND ped-venda.nr-pedcli  = wh-pd4000-nr-pedcli:SCREEN-VALUE
         /* AND ped-venda.completo   = YES*/,
        FIRST if-ped-venda NO-LOCK
        WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido:

/*         MESSAGE if-ped-venda.cod-estab-atend " <> " wh-pd4000-c-unid-atend:SCREEN-VALUE skip */
/*                 ped-venda.cod-estabel        " <> " wh-pd4000-cod-estabel:SCREEN-VALUE  skip */
/*                 ped-venda.nat-operaca        " <> " wh-pd4000-nat-operacao:SCREEN-VALUE skip */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                               */

        if (ped-venda.cod-estabel = "422" OR ped-venda.cod-estabel = "412") then do: /*solic-318*/ 

            IF if-ped-venda.cod-estab-atend <> wh-pd4000-c-unid-atend:SCREEN-VALUE and wh-pd4000-c-unid-atend:SCREEN-VALUE = "" THEN DO:
                RUN utp/ut-msgs.p (INPUT "show",
                                   INPUT  15825,
                                   INPUT "N∆o Ç permitido alterar unidade de atendimento para branco pedidos efetivados que possuem incentivo fiscal.").
                RETURN "NO-ERROR":U.
            END.
    
            IF wh-pd4000-cod-estabel:SCREEN-VALUE <> ped-venda.cod-estabel THEN DO:
                RUN utp/ut-msgs.p (INPUT "show",
                                   INPUT  15825,
                                   INPUT "N∆o Ç permitido alterar estabelecimento para pedidos efetivados que possuem incentivo fiscal.").
               RETURN "NO-ERROR":U.
            END.
    
            IF wh-pd4000-nat-operacao:SCREEN-VALUE <> ped-venda.nat-operacao THEN DO:
                RUN utp/ut-msgs.p (INPUT "show",
                                   INPUT  15825,
                                   INPUT "N∆o Ç permitido alterar natureza de operaá∆o para pedidos efetivados que possuem incentivo fiscal.").
               RETURN "NO-ERROR":U.
            END.
        end.


        /* Completa Pedido */

       
        IF if-ped-venda.cod-estab-atend <> wh-pd4000-c-unid-atend:SCREEN-VALUE OR
           ped-venda.cod-estabel        <> wh-pd4000-cod-estabel:SCREEN-VALUE  OR 
           ped-venda.nat-operacao       <> wh-pd4000-nat-operacao:SCREEN-VALUE  THEN DO:

            FOR EACH pre-fatur NO-LOCK OF ped-venda:
                ASSIGN cListaEmb = cListaEmb + "," + STRING(pre-fatur.cdd-embarq).
            END.

            IF cListaEmb <> "" THEN DO:

                ASSIGN cListaEmb = SUBSTRING(cListaEmb, 2).

                RUN utp/ut-msgs.p (INPUT "show",
                                   INPUT  15825,
                                   INPUT "Pedido relacionado com embarque!!!~~Pedido j† relacionado com o(s) embarque(s) " + cListaEmb + ". Favor liberar o pedido do(s) embarque(s) para efetuar este tipo de alteraá∆o.").
                RETURN "NO-ERROR":U.
            END.

            ASSIGN ped-venda.completo = NO.

            RUN utp/ut-msgs.p (INPUT "show",
                               INPUT  15825,
                               INPUT "Favor efetivar novamente o pedido devido o mesmo possuir relacionamento com Unigel Comercial.").
            RETURN NO-APPLY.


        END.
         /*IF ped-venda.nome-tr-red        <> wh-pd4000-nome-tr-red:SCREEN-VALUE  THEN DO:
            FIND FIRST b-if-ped-venda NO-LOCK
                     WHERE rowid(b-if-ped-venda) = ROWID(if-ped-venda) NO-ERROR.
            DO WHILE AVAIL b-if-ped-venda:
                FIND FIRST b2-ped-venda exclusive-LOCK
                     WHERE b2-ped-venda.nr-pedido = b-if-ped-venda.nr-pedido-relac NO-ERROR.
    
                FIND FIRST b-if-ped-venda NO-LOCK
                     WHERE b-if-ped-venda.nr-pedido = b2-ped-venda.nr-pedido AND b-if-ped-venda.nr-pedido-relac  <> 0  NO-ERROR.
                
                 if avail  b2-ped-venda then ASSIGN b2-ped-venda.nome-tr-red  = wh-pd4000-nome-tr-red:SCREEN-VALUE
                                                    wh-pd4000-nome-tr-red:SCREEN-VALUE = ""
                                                    b2-ped-venda.nome-trans  = wh-pd4000-nome-transp:SCREEN-VALUE
                                                    wh-pd4000-nome-transp:SCREEN-VALUE = "".
                     
                     

            END.

             



        END.*/
        if (ped-venda.cod-estabel = "422" OR ped-venda.cod-estabel = "412") then do: /*solic-318*/ 
            ASSIGN ped-venda.completo = NO.

            RUN utp/ut-msgs.p (INPUT "show",
                               INPUT  15825,
                               INPUT "Favor efetivar novamente o pedido devido o mesmo possuir relacionamento com Unigel Comercial.").
            RETURN NO-APPLY.
        end.
        
        
    END.
    
    
    
    FOR FIRST ped-venda EXCLUSIVE-LOCK
        WHERE ped-venda.nome-abrev = wh-pd4000-nome-abrev:SCREEN-VALUE
          AND ped-venda.nr-pedcli  = wh-pd4000-nr-pedcli:SCREEN-VALUE
          /*AND ped-venda.completo   = YES*/,
        FIRST if-ped-venda NO-LOCK
        WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido:
         
         if wh-pd4000-c-unid-atend:SCREEN-VALUE <> "" then do:
         
                RUN utp/ut-msgs.p (INPUT "show",
                                   INPUT  15825,
                                   INPUT "N∆o Ç permitido colocar unidade de atendimento em pedido relacionado, somente em pedido inicial.").
                wh-pd4000-c-unid-atend:SCREEN-VALUE =  "".                   
                RETURN "NO-ERROR":U.
         END.

    end.
END PROCEDURE. /* pi-valid-campos */

IF VALID-HANDLE(wh-pd4000-nome-abrev-f) AND VALID-HANDLE(wh-pd4000-nome-abrev) THEN
    ASSIGN wh-pd4000-nome-abrev-f:SENSITIVE = wh-pd4000-nome-abrev:SENSITIVE.


PROCEDURE pi-visualiz-cli:

    /* Procura pedido final */
    ASSIGN i-nr-pedido = 0.
    FIND FIRST if-ped-venda NO-LOCK
         WHERE if-ped-venda.nr-pedido         = b-ped-venda.nr-pedido
           AND if-ped-venda.nr-pedido-relac  <> 0 NO-ERROR.
    IF AVAIL if-ped-venda THEN DO:

        DO WHILE AVAIL if-ped-venda:
            FIND FIRST b2-ped-venda no-LOCK
                 WHERE b2-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac NO-ERROR.

            FIND FIRST if-ped-venda NO-LOCK
                 WHERE if-ped-venda.nr-pedido = b2-ped-venda.nr-pedido AND if-ped-venda.nr-pedido-relac  <> 0  NO-ERROR.
            
            if avail  b2-ped-venda then ASSIGN i-nr-pedido = b2-ped-venda.nr-pedido.
        END.
    END.

    IF i-nr-pedido > 0 THEN DO:
        FIND FIRST b2-ped-venda
             WHERE b2-ped-venda.nr-pedido = i-nr-pedido NO-LOCK NO-ERROR.
        IF AVAIL b2-ped-venda THEN
            ASSIGN wh-pd4000-nome-ab-fim:SCREEN-VALUE = b2-ped-venda.nome-abrev.

        ASSIGN wh-pd4000-nome-abrev-f:SCREEN-VALUE = TRIM(b-ped-venda.nome-abrev).
               wh-pd4000-nr-pedcli-f:SCREEN-VALUE  = TRIM(b-ped-venda.nr-pedcli).

        wh-pd4000-nome-abrev:VISIBLE     = NO.
        wh-pd4000-nr-pedcli:VISIBLE      = NO.
        wh-pd4000-nome-abrev-f:VISIBLE   = YES.
        wh-pd4000-nome-ab-fim:VISIBLE    = YES.
        wh-pd4000-tx-nome-abrev:VISIBLE  = YES.
        wh-pd4000-tx-nome-ab-fim:VISIBLE = YES.
        wh-pd4000-nr-pedcli-f:VISIBLE    = YES.
        wh-pd4000-tx-nr-pedcli:VISIBLE   = YES.
    END.
    ELSE DO:
        wh-pd4000-nome-abrev:VISIBLE     = YES.
        wh-pd4000-nr-pedcli:VISIBLE      = YES.
        wh-pd4000-nome-abrev-f:VISIBLE   = NO.
        wh-pd4000-nome-ab-fim:VISIBLE    = NO.
        wh-pd4000-tx-nome-abrev:VISIBLE  = NO.
        wh-pd4000-tx-nome-ab-fim:VISIBLE = NO.
        wh-pd4000-nr-pedcli-f:VISIBLE    = NO.
        wh-pd4000-tx-nr-pedcli:VISIBLE   = NO.
    END.

END PROCEDURE.


procedure pi-replica-data-tipo.

/*eliminada*/

end procedure.


procedure pi-replica-data-tipo-filho.

define buffer b-ped-item for ped-item.
define buffer b-ped-ent for ped-ent.
define buffer bfilho-ped-venda for ped-venda.
 for each ped-item of ped-venda  NO-LOCK,
     EACH ITEM WHERE ITEM.it-codigo = ped-item.it-codigo AND
                     ITEM.ge-codigo >= 40 AND ITEM.ge-codigo <= 49 NO-LOCK.
     for each ped-ent of ped-item exclusive-lock.
           IF  ped-ent.qt-pedida <> ped-item.qt-pedida THEN
                ASSIGN ped-ent.qt-pedida = ped-item.qt-pedida .

     end.
 END.


end procedure.





