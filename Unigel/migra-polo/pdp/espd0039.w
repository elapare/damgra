&Scoped-define WINDOW-NAME C-Win

/*------------------------------------------------------------------------
File.............: espd0039.w
Description......: RELAT‡RIO DE BOBINAS PRODUZIDAS
Input Parameters : 
Output Parameters: 
Author...........:  
Created..........: 02/10/2013 
-----------------------------------------------------------------------*/


CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
/*&GLOBAL-DEFINE PGCLA f-pg-cla
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGIMP f-pg-imp
  */
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */


define temp-table tt-param
    field destino                as integer
    field arquivo                as char
    field usuario                as char
    field data-exec              as date
    field hora-exec              as integer
    field parametro              as logical
    field formato                as integer
    field v_num_tip_aces_usuar   as integer
    field ep-codigo              as CHARACTER
    field classifica             as integer
    field it-codigo-ini          as char
    field it-codigo-fim          as char
    field cod-estabel-ini        as char
    field ge-codigo-ini          as INTEGER
    field ge-codigo-fim          as INTEGER
    FIELD dt-producao-ini        AS DATE
    FIELD dt-producao-fim        AS DATE
    FIELD c-cod-depos-ini        AS CHAR
    FIELD c-cod-depos-fim        AS CHAR
    field c-destino              as char
    field i-nr-ord-produ-ini     as INTEGER
    field i-nr-ord-produ-fim     as INTEGER
    FIELD tg-imprime             AS LOGICAL
    FIELD tg-turma               AS LOGICAL.

def var raw-param      as raw no-undo.


DEFINE TEMP-TABLE tt-digita no-undo
    FIELD cod-cliente          AS CHAR    FORMAT "x(12)"          label "Cliente"
    INDEX chave IS PRIMARY cod-cliente.
    

Def    Buffer bf-digita   For tt-digita.

def temp-table tt-raw-digita
   field raw-digita      as raw.


/* Local Variable Definitions ---                                       */

def var l-ok           as logical no-undo.
def var c-arq-digita   as char    no-undo.
def var c-terminal     as char    no-undo.


/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-cla

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-classif 
&Scoped-Define DISPLAYED-OBJECTS rs-classif 


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rs-classif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Grupo / Fam°lia / Item", 1
     SIZE 42.86 BY 2.46 NO-UNDO.

DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 4 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
          "Arquivo", 2,
          "Terminal", 3,
          "Excel", 4
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
          "Batch", 2
          SIZE 27.72 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE tg-imprime AS LOGICAL INITIAL yes 
     LABEL "Imprime Somente Paletizados" 
     VIEW-AS TOGGLE-BOX
     SIZE 54 BY .83 NO-UNDO.


DEFINE VARIABLE tg-turma AS LOGICAL INITIAL yes 
     LABEL "Mostra M†quina,Turno,Turma (Um pouco mais demorado para gerar)" 
     VIEW-AS TOGGLE-BOX
     SIZE 54 BY .83 NO-UNDO.
                  


DEFINE VARIABLE c-cod-estabel-ini AS CHARACTER FORMAT "X(03)":U initial "422" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.





DEFINE VARIABLE dt-producao-ini AS DATE FORMAT "99/99/9999":U INITIAL today
     LABEL "Dt Produá∆o" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE dt-producao-fim AS DATE FORMAT "99/99/9999":U INITIAL today
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.


DEFINE VARIABLE c-it-codigo-ini AS CHAR FORMAT "x(16)":U INITIAL ""
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo-fim AS CHAR FORMAT "x(16)":U INITIAL "ZZZZZZZZZZZZZZZZ"  
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-depos-ini AS CHAR FORMAT "x(3)":U INITIAL "AZZ"
     LABEL "Deposito" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-depos-fim AS CHAR FORMAT "x(3)":U INITIAL "ZZZ"  
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.
 

DEFINE VARIABLE i-nr-ord-produ-ini AS INTEGER INITIAL 0  FORMAT ">>>,>>>,>>9"  
     LABEL "Ord.Prod" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE i-nr-ord-produ-fim AS INTEGER INITIAL 999999999  FORMAT ">>>,>>>,>>9"  
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.


DEFINE VARIABLE i-ge-codigo-ini AS INT FORMAT ">>9":U INITIAL 45 
     LABEL "Grupo Estoque" 
     VIEW-AS FILL-IN 
     SIZE 06 BY .88 NO-UNDO.

DEFINE VARIABLE i-ge-codigo-fim AS INT FORMAT ">>9":U INITIAL 47 
     VIEW-AS FILL-IN 
     SIZE 06 BY .88 NO-UNDO.



DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE IMAGE im-pg-cla
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.5.

DEFINE IMAGE im-pg-imp
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.5.

DEFINE IMAGE im-pg-par
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.5.

DEFINE IMAGE im-pg-sel
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.5.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.42
     BGCOLOR 7 .

DEFINE IMAGE IMAGE-1
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-2
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-3
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-4
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-5
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-6
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-7
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-8
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-9
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-10
    FILENAME "image\im-las"
    SIZE 3 BY .88.



    
DEFINE IMAGE IMAGE-11
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-12
    FILENAME "image\im-las"
    SIZE 3 BY .88.

    

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 11.5.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-left
     EDGE-PIXELS 0    
     SIZE .43 BY 11.21
     BGCOLOR 15 .

DEFINE RECTANGLE rt-folder-right
     EDGE-PIXELS 0    
     SIZE .43 BY 11.17
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-top
     EDGE-PIXELS 0    
     SIZE 78.29 BY .13
     BGCOLOR 15 .



/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 1.29 COL 2.14 HELP
          "Classificaá∆o para emiss∆o do relat¢rio" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.04
         SIZE 76.86 BY 10.75.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Cancelar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-top AT ROW 2.5 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     im-pg-cla AT ROW 1.5 COL 17.57
     im-pg-imp AT ROW 1.5 COL 48.86
     im-pg-par AT ROW 1.5 COL 33.29
     im-pg-sel AT ROW 1.5 COL 2.14
     RECT-18 AT ROW 2.5 COL 2  
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.13
         SIZE 81.14 BY 14.88
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-par
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 77 BY 10.5.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configuraá∆o da impressora"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execuá∆o" NO-LABEL
    
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76 BY 10.

    DEFINE FRAME f-pg-sel
     c-cod-estabel-ini  AT ROW 1.25 COL 20 COLON-ALIGNED
     
     dt-producao-ini    AT ROW 2.25 COL 20 COLON-ALIGNED           
     dt-producao-fim   AT ROW 2.25 COL 48 COLON-ALIGNED NO-LABEL  
    
     c-it-codigo-ini    AT ROW 3.25 COL 20 COLON-ALIGNED 
     c-it-codigo-fim    AT ROW 3.25 COL 48 COLON-ALIGNED  NO-LABEL

     c-cod-depos-ini    AT ROW 4.25 COL 20 COLON-ALIGNED             
     c-cod-depos-fim    AT ROW 4.25 COL 48 COLON-ALIGNED  NO-LABEL   

     i-ge-codigo-ini    AT ROW 5.25 COL 20 COLON-ALIGNED
     i-ge-codigo-fim    AT ROW 5.25 COL 48 COLON-ALIGNED NO-LABEL
   
     i-nr-ord-produ-ini    AT ROW 6.25 COL 20 COLON-ALIGNED
     i-nr-ord-produ-fim    AT ROW 6.25 COL 48 COLON-ALIGNED NO-LABEL

     tg-imprime   AT ROW 8.25 COL 20 COLON-ALIGNED         
     tg-turma  AT ROW 9.25 COL 20 COLON-ALIGNED
    /* IMAGE-1 AT ROW 01.25 COL 40
     IMAGE-2 AT ROW 01.25 COL 45
      */
     IMAGE-3 AT ROW 02.25 COL 40
     IMAGE-4 AT ROW 02.25 COL 45

     IMAGE-5 AT ROW 03.25 COL 40
     IMAGE-6 AT ROW 03.25 COL 45

     IMAGE-7 AT ROW 04.25 COL 40
     IMAGE-8 AT ROW 04.25 COL 45

     
     IMAGE-9  AT ROW 05.25 COL 40
     IMAGE-10 AT ROW 05.25 COL 45

     IMAGE-11  AT ROW 06.25 COL 40
     IMAGE-12 AT ROW 06.25 COL 45
     

     WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
     SIDE-LABELS NO-UNDERLINE THREE-D 
     AT COL 3 ROW 3.04
     SIZE 76.86 BY 10.42.


/* *********************** Procedure Settings ************************ */

/* *************************  Create Window  ************************** */


&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN   
         HIDDEN             = YES
         TITLE              = "RELAT‡RIO DE BOBINAS PRODUZIDAS"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 22.33
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.33
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-relat.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME f-pg-par:FRAME = FRAME f-relat:HANDLE.

/* SETTINGS FOR FRAME f-pg-cla
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME f-pg-imp
                                                                        */
/* SETTINGS FOR RADIO-SET rs-destino IN FRAME f-pg-imp
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rs-execucao IN FRAME f-pg-imp
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execuá∆o".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR IMAGE im-pg-cla IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE im-pg-par IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-relat
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-imp
/* Query rebuild information for FRAME f-pg-imp
     _Query            is NOT OPENED
*/  /* FRAME f-pg-imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* RELAT‡RIO DE SALDOS X CONSUMO NO PER÷ODO */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* RELAT‡RIO DE SALDOS X CONSUMO NO PER÷ODO */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda C-Win
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo C-Win
ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:
    {include/i-rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar C-Win
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Cancelar */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-config-impr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr C-Win
ON CHOOSE OF bt-config-impr IN FRAME f-pg-imp
DO:
   {include/i-rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar C-Win
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:
    
   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-cla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-cla C-Win
ON MOUSE-SELECT-CLICK OF im-pg-cla IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-imp C-Win
ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par C-Win
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel C-Win
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino C-Win
ON MOUSE-SELECT-CLICK OF rs-destino IN FRAME f-pg-imp
DO:

    case self:screen-value:
        when "1" then do:   
            FIND FIRST imprsor_usuar WHERE imprsor_usuar.log_imprsor_princ AND
                       imprsor_usuar.cod_usuario   = c-seg-usuario NO-LOCK NO-ERROR NO-WAIT.

            FIND FIRST impressora OF imprsor_usuar NO-LOCK NO-ERROR NO-WAIT.

            FIND FIRST layout_impres OF IMPRESSORA NO-LOCK NO-ERROR NO-WAIT.

            IF AVAIL imprsor_usuar THEN 
               c-arquivo:SCREEN-VALUE = impressora.nom_impressora + ":" + layout_impres.nom_impressora.
            ELSE
               c-arquivo:SCREEN-VALUE = "".

            assign c-arquivo:VISIBLE      = yes
                   c-arquivo:SENSITIVE    = NO
                   bt-arquivo:visible     = NO
                   bt-config-impr:visible = yes.
        end.
        when "2" then do:
            assign c-arquivo:SCREEN-VALUE = CAPS(SESSION:TEMP-DIRECTORY) + c-programa-mg97 + ".TMP"
                   c-arquivo:VISIBLE      = yes
                   c-arquivo:SENSITIVE    = yes
                   bt-arquivo:visible     = yes
                   bt-config-impr:visible = no.
        end.
        when "3" then do:
            assign c-arquivo:VISIBLE      = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = no.
        end.
        when "4" then do:
            assign c-arquivo:VISIBLE      = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = no.
        end.
    end case.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-execucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao C-Win
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-imp
DO:
   {include/i-rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-cla
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/*   Sistema de Seguranáa  - Validaá∆o da autorizaá∆o  */
{utp/ut9000.i "espd0039" "1.00.00.000"}

/* inicializaá‰es do template de relat¢rio */
{include/i-rpini.i}

    ASSIGN rs-destino:SCREEN-VALUE in frame f-pg-imp  = "4".

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-rplbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:


    RUN enable_UI.


    ASSIGN rs-destino:SCREEN-VALUE in frame f-pg-imp  = "4".


  
    {include/i-rpmbl.i}
    find first param-global no-lock no-error.
    if  not avail param-global then do:
        run utp/ut-msgs.p (input "show", input 2314, input "").
        return error.
    end.

    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects C-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available C-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/

  ASSIGN rs-destino:screen-value  in frame f-pg-imp = "4".

  ENABLE /*im-pg-imp*/ im-pg-sel RECT-18 bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-destino c-arquivo rs-execucao tg-imprime tg-turma 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  ENABLE RECT-7 RECT-9 bt-arquivo bt-config-impr c-arquivo tg-imprime  tg-turma 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  VIEW FRAME f-pg-par IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY rs-classif 
      WITH FRAME f-pg-cla IN WINDOW C-Win.
  ENABLE rs-classif 
      WITH FRAME f-pg-cla IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY 
         c-cod-estabel-ini 
         dt-producao-ini
         dt-producao-fim
         i-nr-ord-produ-ini     
         i-nr-ord-produ-fim     
         i-ge-codigo-ini     
         i-ge-codigo-fim     
         c-it-codigo-ini
         c-it-codigo-fim
         c-cod-depos-ini
         c-cod-depos-fim
             tg-imprime  tg-turma 

      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE 
      c-cod-estabel-ini 
      dt-producao-ini
      dt-producao-fim
      i-nr-ord-produ-ini     
      i-nr-ord-produ-fim     
      i-ge-codigo-ini     
      i-ge-codigo-fim     
      c-it-codigo-ini
      c-it-codigo-fim
      c-cod-depos-ini
      c-cod-depos-fim
           
       tg-imprime 
      tg-turma 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}

       im-pg-cla:hidden = YES.
       im-pg-imp:hidden = YES.
       im-pg-par:hidden = YES.

  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit C-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar C-Win 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

do  on error undo, return error
    on stop  undo, return error:     

    {include/i-rpexa.i}

    if  input frame f-pg-imp rs-destino = 2 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        if  return-value = "nok" then do:
            run utp/ut-msgs.p (input "show", input 73, input "").
            apply 'mouse-select-click' to im-pg-imp in frame f-relat.
            apply 'entry' to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.

    /* Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na p†gina 
       com problemas e colocar o focus no campo com problemas             */
       
    create tt-param.
    assign tt-param.usuario          = c-seg-usuario
           tt-param.destino          = input frame f-pg-imp rs-destino
           tt-param.data-exec        = today
           tt-param.hora-exec        = time
           tt-param.v_num_tip_aces_usuar = v_num_tip_aces_usuar
           tt-param.ep-codigo            = i-ep-codigo-usuario
           tt-param.classifica       = input frame f-pg-cla rs-classif.

    ASSIGN tt-param.it-codigo-ini    = input frame f-pg-sel c-it-codigo-ini
           tt-param.it-codigo-fim    = input frame f-pg-sel c-it-codigo-fim
           tt-param.cod-estabel-ini  = input frame f-pg-sel c-cod-estabel-ini
           tt-param.ge-codigo-ini    = input frame f-pg-sel i-ge-codigo-ini
           tt-param.ge-codigo-fim    = input frame f-pg-sel i-ge-codigo-fim
           tt-param.dt-producao-ini  = input frame f-pg-sel dt-producao-ini 
           tt-param.dt-producao-fim  = input frame f-pg-sel dt-producao-fim 
           tt-param.c-cod-depos-ini  = input frame f-pg-sel c-cod-depos-ini
           tt-param.c-cod-depos-fim  = input frame f-pg-sel c-cod-depos-fim
           tt-param.c-destino        = entry((tt-param.destino - 1) * 2 + 1,
                                       rs-destino:radio-buttons in frame f-pg-imp)
           tt-param.i-nr-ord-produ-ini    = input frame f-pg-sel i-nr-ord-produ-ini
           tt-param.i-nr-ord-produ-fim    = input frame f-pg-sel i-nr-ord-produ-fim 
           tt-param.tg-imprime            = input frame f-pg-sel   tg-imprime
           tt-param.tg-turma              = input frame f-pg-sel   tg-turma
        .

    if  tt-param.destino = 1 then
        assign tt-param.arquivo = "".
    else
    if  tt-param.destino = 2 then 
        assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
    else
        assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".
    
    /* Coloque aqui a l¢gica de gravaá∆o dos parÉmtros e seleá∆o na temp-table
       tt-param */ 

    {include/i-rpexb.i}

    if  session:set-wait-state("general":U) then.
    
    /*{include/i-rprun.i cep/espd0039rp.p} */
  
        {include/i-rprun.i pdp/espd0039rp.p} 


    

    {include/i-rpexc.i}

    if  session:set-wait-state("":U) then.
    
    {include/i-rptrm.i}
        

end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina C-Win 
PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P†gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{include/i-rptrp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records C-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-relat, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed C-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

