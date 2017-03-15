/*************************************************************************************
**  Programa: upc-fp0903-u01.P
**  Objetivo: estabelecimentos para passar e-mail no ato do desligamento
**  Data....: 10/04/2016
**  Versao..: 2.06.001 - Edson - Amgra
**  chamado pelo upc-fp0903-u01    
  
**************************************************************************************/

def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-row-table  as rowid         no-undo.
/*--------------------------------------------------------*/
/*variavel global de permissao de estabelecimento*/
DEFINE NEW GLOBAL SHARED VAR gv-estab-amg AS CHAR NO-UNDO. 
DEFINE NEW GLOBAL SHARED  VARIABLE c-seg-usuario AS CHARACTER  NO-UNDO.

DEFINE VARIABLE  i-num-pessoa AS INTEGER     NO-UNDO.
def new global shared var wh-label  as widget-handle no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE wh-fp0903-br-table AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-fp0903-estab-fim  AS HANDLE        NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE h-query-fp0903       AS HANDLE        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cod_grp_mail       AS HANDLE        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-fp0903-bt-formar  AS HANDLE        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-fp0903-bt-estab  AS HANDLE        NO-UNDO.



def button bt_can
    label "Cancela"
    tooltip "Cancela"
    size 10 by 1
    auto-endkey.

def button bt_ok
    label "OK"
    tooltip "OK"
    size 10 by 1
    auto-go.

DEFINE VARIABLE idx AS INTEGER     NO-UNDO.
DEFINE VARIABLE ict AS INTEGER     NO-UNDO.
DEFINE VARIABLE c_trab AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_estab 
as CHARACTER  INITIAL "*" LABEL "Estab/Empr. P/ Enviar Email " 
    view-as editor  SCROLLBAR-VERTICAL TOOLTIP "Favor digite * p/ Todos ou Estabs ou Emprs. Ex: 321,412,380,.. (separados por v¡rgulas)." /*solic-318*/ 
    size 60 by 2
    bgcolor 15 FONT 1 
    no-undo.

def rectangle rt_cxcf
    size 82.7 by 1.5
    fgcolor 1 edge-pixels 2.

def FRAME f-relat
    c_estab at row 01.54 col 6.43
    bt_ok at row 04.54 col 10.43
    bt_can at row 04.54 col 25.43
    rt_cxcf AT ROW 04.2 COL 4
with 1 down side-labels no-validate keep-tab-order three-d
         size-char 90.00 by 6.21 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Estabelecimentos e/ou Empresas para Enviar E-mails".

  
    IF p-ind-event = "BEFORE-OPEN-QUERY" THEN DO:

        FOR EACH esp_grp_mail_func WHERE esp_grp_mail_func.cod_grp_mail   = wh-cod_grp_mail:SCREEN-VALUE  EXCLUSIVE-LOCK. 
            FIND FIRST grp_mail_func WHERE grp_mail_func.cod_grp_mail     = esp_grp_mail_func.cod_grp_mai AND
                                           grp_mail_func.num_pessoa_fisic = esp_grp_mail_func.num_pessoa_fisic NO-LOCK NO-ERROR.
            IF NOT AVAIL grp_mail_func THEN
                DELETE esp_grp_mail_func.
        END.
    
    END.
 
    
    IF p-ind-event = "choose-bt-estab" THEN DO:
        i-num-pessoa = 0.
        i-num-pessoa = INT(h-query-fp0903:BUFFER-FIELD("num_pessoa_fisic"):BUFFER-VALUE) NO-ERROR.

        IF VALID-HANDLE(wh-cod_grp_mail ) AND i-num-pessoa <> 0  THEN DO:
            FIND FIRST esp_grp_mail_func WHERE esp_grp_mail_func.cod_grp_mail     = wh-cod_grp_mail:SCREEN-VALUE AND
                                               esp_grp_mail_func.num_pessoa_fisic = i-num-pessoa   EXCLUSIVE-LOCK NO-ERROR.  
            IF NOT AVAIL esp_grp_mail_func  THEN DO:

                 CREATE esp_grp_mail_func.
                 ASSIGN esp_grp_mail_func.cod_grp_mail     = wh-cod_grp_mail:SCREEN-VALUE
                        esp_grp_mail_func.num_pessoa_fisic = i-num-pessoa                .
            END.
            FOR FIRST esp_grp_mail_func WHERE esp_grp_mail_func.cod_grp_mail     = wh-cod_grp_mail:SCREEN-VALUE AND
                                          esp_grp_mail_func.num_pessoa_fisic = i-num-pessoa EXCLUSIVE-LOCK.  


                ASSIGN  c_estab = esp_grp_mail_func.emp_estab.

                DISPLAY
                    c_estab
                    bt_ok 
                    bt_can
                    rt_cxcf
                    WITH FRAME f-relat.
                
                UPDATE
                    c_estab 
                    bt_ok
                    bt_can
                    WITH FRAME f-relat BGCOLOR wh-cod_grp_mail:FRAME:BGCOLOR .                                                

                ASSIGN c_trab = ""
                       ict    = 0
                       c_estab = replace(replace(REPLACE(c_estab," ",""),";",","),"-",",").
                DO idx = 1 TO NUM-ENTRIES(c_estab):
                    IF ENTRY(idx,c_estab) <> "" THEN DO:
                        IF ENTRY(idx,c_estab) = "*" OR CAN-FIND(FIRST rh_estab WHERE (rh_estab.cdn_estab = ENTRY(idx,c_estab) OR rh_estab.cdn_empresa = ENTRY(idx,c_estab)) ) THEN DO:
                            ict = ict + 1.
                            c_trab = c_trab + (IF ict = 1 THEN "" ELSE ",") + ENTRY(idx,c_estab).    
                        END.
                    END.
                END.

                IF INDEX(c_trab,"*") > 0 THEN
                    c_trab = "*".

                ASSIGN esp_grp_mail_func.emp_estab = c_trab.

                wh-fp0903-estab-fim:SCREEN-VALUE = esp_grp_mail_func.emp_estab.

            END.
        END.

    END.


    IF p-ind-event = "row-display-br-table" THEN DO:
        IF VALID-HANDLE(h-query-fp0903) AND VALID-HANDLE(wh-fp0903-estab-fim) THEN DO:
    
            i-num-pessoa = 0.
            i-num-pessoa = INT(h-query-fp0903:BUFFER-FIELD("num_pessoa_fisic"):BUFFER-VALUE) NO-ERROR.
            IF VALID-HANDLE(wh-cod_grp_mail ) AND i-num-pessoa <> 0  THEN DO:
                FOR FIRST esp_grp_mail_func WHERE esp_grp_mail_func.cod_grp_mail     = wh-cod_grp_mail:SCREEN-VALUE AND
                                                  esp_grp_mail_func.num_pessoa_fisic = i-num-pessoa NO-LOCK.            
                             wh-fp0903-estab-fim:SCREEN-VALUE = esp_grp_mail_func.emp_estab.
                END.
            END.                   
        END.        
    END.

    IF  VALID-HANDLE(wh-cod_grp_mail ) AND VALID-HANDLE(wh-fp0903-estab-fim)
          AND p-ind-event = "DISPLAY"   AND  p-ind-object = "VIEWER"  THEN DO:       
        
             ASSIGN wh-fp0903-estab-fim:VISIBLE = (wh-cod_grp_mail:SCREEN-VALUE  = "01").

             IF VALID-HANDLE(wh-fp0903-bt-estab) THEN
                    ASSIGN wh-fp0903-bt-estab:SENSITIVE = (wh-cod_grp_mail:SCREEN-VALUE  = "01").
    END.
                  
    IF NOT VALID-HANDLE(wh-fp0903-br-table) AND p-ind-event = "BEFORE-INITIALIZE"   AND  p-ind-object = "container"  THEN DO:    
    
       Run pi-busca-widget (Input  "cod_grp_mail"  , Input  p-wgh-frame, Output wh-cod_grp_mail ).
    END.

    IF NOT VALID-HANDLE(wh-fp0903-br-table) AND p-ind-event = "BEFORE-INITIALIZE"   AND  p-ind-object = "BROWSER"  THEN DO:    
        
        Run pi-busca-widget (Input  "br-table"  , Input  p-wgh-frame, Output wh-fp0903-br-table ).
        Run pi-busca-widget (Input  "bt-formar"  , Input  p-wgh-frame, Output wh-fp0903-bt-formar ).
    
     if valid-handle(wh-fp0903-bt-formar) and not valid-handle(wh-fp0903-bt-estab) then do:
                    CREATE BUTTON wh-fp0903-bt-estab
                     ASSIGN FRAME       = wh-fp0903-bt-formar:frame
                            WIDTH       = wh-fp0903-bt-formar:WIDTH + 3
                            HEIGHT      = wh-fp0903-bt-formar:HEIGHT
                            LABEL       = "Est/Emp.Destino"
                            ROW         = wh-fp0903-bt-formar:ROW
                            COLUMN      = wh-fp0903-bt-formar:COLUMN + 12
                            TOOLTIP     =  "* p/ todos, ou digite Estb/Empr ex: 321,322,412,380 para v rios." /*solic-318*/ 
                            NAME        = 'bt-estab-esp'
                            SENSITIVE   = YES
                            VISIBLE     = YES
                         TRIGGERS:
                            ON 'choose':U PERSISTENT RUN prghur\fpp\upc\upc-fp0903-u01.p  (INPUT "choose-bt-estab" ,
                                   INPUT p-ind-object,
                                   INPUT p-wgh-object,
                                   INPUT p-wgh-frame ,
                                   INPUT p-cod-table ,
                                   INPUT p-row-table ).
                         END TRIGGERS.
     
           end.


    IF VALID-HANDLE(wh-fp0903-br-table) THEN DO:
        ASSIGN 
                  wh-fp0903-estab-fim = wh-fp0903-br-table:ADD-CALC-COLUMN("CHARACTER","X(24)","","Est/Emp.Enviar E-mail",6)
                  h-query-fp0903    = wh-fp0903-br-table:QUERY:GET-BUFFER-HANDLE(1).
           ON 'row-display':U OF wh-fp0903-br-table PERSISTENT RUN  prghur\fpp\upc\upc-fp0903-u01.p (INPUT "row-display-br-table" ,
                                   INPUT p-ind-object,
                                   INPUT p-wgh-object,
                                   INPUT p-wgh-frame ,
                                   INPUT p-cod-table ,
                                   INPUT p-row-table ).
    
    
    END.

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

/*                      
                        IF INDEX("br-table,bt-formar",h-frame:Name) > 0  THEN
                           MESSAGE "p-ind-event " p-ind-event  SKIP h-frame:Name
                               VIEW-AS ALERT-BOX INFO BUTTONS OK.
                        */
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




