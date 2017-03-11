&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wReport 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{bf/buffersUni2.i}

{include/i-prgvrs.i espd0034 2.00.00.000}

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        espd0034
&GLOBAL-DEFINE Version        2.00.00.000
&GLOBAL-DEFINE VersionLayout  

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   Sele‡Æo,Parƒmetro,ImpressÆo

&GLOBAL-DEFINE PGLAY          NO
&GLOBAL-DEFINE PGSEL          YES
&GLOBAL-DEFINE PGCLA          NO
&GLOBAL-DEFINE PGPAR          YES
&GLOBAL-DEFINE PGDIG          NO
&GLOBAL-DEFINE PGIMP          YES
&GLOBAL-DEFINE PGLOG          NO

&GLOBAL-DEFINE RTF            NO

&GLOBAL-DEFINE page0Widgets   btOk ~
                              btCancel ~
                              btImportar ~
                              btHelp2
&GLOBAL-DEFINE page1Widgets   
&GLOBAL-DEFINE page2Widgets   
&GLOBAL-DEFINE page3Widgets   
&GLOBAL-DEFINE page4Widgets   rs-mercado
                              
&GLOBAL-DEFINE page5Widgets   
&GLOBAL-DEFINE page6Widgets   rsDestiny ~
                              btConfigImpr ~
                              btFile ~
                              rsExecution 
                              
&GLOBAL-DEFINE page7Widgets   
&GLOBAL-DEFINE page8Widgets   

&GLOBAL-DEFINE page0Text      
&GLOBAL-DEFINE page1Text      
&GLOBAL-DEFINE page2Text      
&GLOBAL-DEFINE page3Text      
&GLOBAL-DEFINE page4Text      fi-texto-1
&GLOBAL-DEFINE page5Text      
&GLOBAL-DEFINE page6Text      text-destino text-modo 
&GLOBAL-DEFINE page7Text      
&GLOBAL-DEFINE page8Text   

&GLOBAL-DEFINE page1Fields    
&GLOBAL-DEFINE page2Fields    fi-cod-estabel fi-it-codigo-ini fi-it-codigo-fim ~
                              fi-fm-codigo-ini fi-fm-codigo-fim fi-ge-codigo-ini fi-ge-codigo-fim ~
                              fi-nome-abrev-ini fi-nome-abrev-fim fi-tp-pedido-ini fi-tp-pedido-fim ~
                              fi-periodo-ini fi-periodo-fim
&GLOBAL-DEFINE page3Fields    
&GLOBAL-DEFINE page4Fields    
&GLOBAL-DEFINE page5Fields    
&GLOBAL-DEFINE page6Fields    cFile 
&GLOBAL-DEFINE page7Fields    
&GLOBAL-DEFINE page8Fields    

/* Parameters Definitions ---                                           */

define temp-table tt-param NO-UNDO
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    field v_num_tip_aces_usuar as integer
    field ep-codigo            as character
    field c-cod-estabel        LIKE movto-estoq.cod-estabel
    field c-it-codigo-ini      LIKE item.it-codigo
    field c-it-codigo-fim      LIKE item.it-codigo
    field c-fm-codigo-ini      LIKE movto-estoq.cod-depos
    field c-fm-codigo-fim      LIKE movto-estoq.cod-depos
    field i-ge-codigo-ini      LIKE item.ge-codigo
    field i-ge-codigo-fim      LIKE item.ge-codigo
    field c-nome-abrev-ini     LIKE emitente.nome-abrev
    field c-nome-abrev-fim     LIKE emitente.nome-abrev
    field c-tp-pedido-ini      as CHAR 
    field c-tp-pedido-fim      as CHAR 
    field dt-entrega-ini       as DATE 
    field dt-entrega-fim       as date
    FIELD i-mercado            as integer
    FIELD l-integr-xtrim       AS LOG.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-rtf              as char    no-undo.
def var c-arq-layout       as char    no-undo.      
def var c-arq-temp         as char    no-undo.
DEF VAR c-modelo-default   AS CHAR    NO-UNDO.

def stream s-imp.

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa est  rodando no WebEnabler*/
DEFINE SHARED VARIABLE hWenController AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar btOK btCancel btImportar btHelp2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wReport AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "&Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "&Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btImportar 
     LABEL "&Importar" 
     SIZE 10 BY 1 TOOLTIP "Importa a informa‡Æo da Campanha".

DEFINE BUTTON btOK 
     LABEL "&Executar" 
     SIZE 10 BY 1.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U INITIAL "422" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-fm-codigo-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-fm-codigo-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Fam¡lia" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-ge-codigo-fim AS INTEGER FORMAT "99":U INITIAL 46 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-ge-codigo-ini AS INTEGER FORMAT "99":U INITIAL 46 
     LABEL "Grupo de Estoque" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-nome-abrev-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-nome-abrev-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-periodo-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-periodo-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Per¡odo" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tp-pedido-fim AS CHARACTER FORMAT "X(16)":U INITIAL "R" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tp-pedido-ini AS CHARACTER FORMAT "X(16)":U INITIAL "A" 
     LABEL "Tipo de Pedido" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-23
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-24
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE fi-texto-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Mercado" 
      VIEW-AS TEXT 
     SIZE 8.86 BY .67 NO-UNDO.

DEFINE VARIABLE rs-mercado AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Interno", 1,
"Externo", 2,
"Ambos", 3
     SIZE 12 BY 3 NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 4.25.

DEFINE VARIABLE lIntegracaoXTrim AS LOGICAL INITIAL no 
     LABEL "Integra‡Æo XTrim" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.86 BY 1.08
     FONT 1 NO-UNDO.

DEFINE BUTTON btConfigImpr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON btFile 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE cFile AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.14 BY .63
     FONT 1 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsDestiny AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3,
"Excel", 4
     SIZE 44 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsExecution AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.86 BY .92
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 1.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 16.75 COL 2
     btCancel AT ROW 16.75 COL 12.29
     btImportar AT ROW 16.75 COL 22.57 HELP
          "Importa a informa‡Æo da Campanha" WIDGET-ID 66
     btHelp2 AT ROW 16.75 COL 80
     rtToolBar AT ROW 16.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage4
     lIntegracaoXTrim AT ROW 2.25 COL 8.14
     rs-mercado AT ROW 4.46 COL 13 NO-LABEL WIDGET-ID 2
     fi-texto-1 AT ROW 3.46 COL 6 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     RECT-13 AT ROW 3.71 COL 7 WIDGET-ID 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.75
         SIZE 84.43 BY 12.5
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage6
     rsDestiny AT ROW 2.38 COL 3.14 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     cFile AT ROW 3.63 COL 3.14 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     btFile AT ROW 3.5 COL 43 HELP
          "Escolha do nome do arquivo"
     btConfigImpr AT ROW 3.5 COL 43 HELP
          "Configura‡Æo da impressora"
     rsExecution AT ROW 9.5 COL 2.86 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.63 COL 1.86 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 8.75 COL 1.14 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 9 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.75
         SIZE 84.43 BY 12.5
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage2
     fi-cod-estabel AT ROW 1.42 COL 5.29 WIDGET-ID 82
     fi-it-codigo-ini AT ROW 2.5 COL 13.57
     fi-it-codigo-fim AT ROW 2.5 COL 54.14 NO-LABEL
     fi-fm-codigo-ini AT ROW 3.54 COL 11.57 WIDGET-ID 42
     fi-fm-codigo-fim AT ROW 3.54 COL 54.14 NO-LABEL WIDGET-ID 44
     fi-ge-codigo-ini AT ROW 4.58 COL 4.15 WIDGET-ID 52
     fi-ge-codigo-fim AT ROW 4.58 COL 54.14 NO-LABEL WIDGET-ID 50
     fi-nome-abrev-ini AT ROW 5.63 COL 11.86 WIDGET-ID 60
     fi-nome-abrev-fim AT ROW 5.63 COL 54.14 NO-LABEL WIDGET-ID 58
     fi-tp-pedido-ini AT ROW 6.71 COL 6.15 WIDGET-ID 68
     fi-tp-pedido-fim AT ROW 6.71 COL 54.14 NO-LABEL WIDGET-ID 66
     fi-periodo-ini AT ROW 7.83 COL 11 WIDGET-ID 76
     fi-periodo-fim AT ROW 7.83 COL 54.14 NO-LABEL WIDGET-ID 74
     IMAGE-1 AT ROW 2.5 COL 35.86
     IMAGE-2 AT ROW 2.5 COL 51.14
     IMAGE-15 AT ROW 3.54 COL 35.86 WIDGET-ID 46
     IMAGE-16 AT ROW 3.54 COL 51.14 WIDGET-ID 48
     IMAGE-21 AT ROW 4.58 COL 35.86 WIDGET-ID 54
     IMAGE-22 AT ROW 4.58 COL 51.14 WIDGET-ID 56
     IMAGE-23 AT ROW 5.63 COL 35.86 WIDGET-ID 62
     IMAGE-24 AT ROW 5.63 COL 51.14 WIDGET-ID 64
     IMAGE-27 AT ROW 6.71 COL 35.86 WIDGET-ID 70
     IMAGE-28 AT ROW 6.71 COL 51.14 WIDGET-ID 72
     IMAGE-29 AT ROW 7.83 COL 35.86 WIDGET-ID 78
     IMAGE-30 AT ROW 7.83 COL 51.14 WIDGET-ID 80
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.75
         SIZE 84.43 BY 12.5
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wReport ASSIGN
         HIDDEN             = YES
         TITLE              = "Pedidos de Vendas / Capacidade Produ‡Æo"
         HEIGHT             = 17
         WIDTH              = 90
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.14
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 114.14
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wReport 
/* ************************* Included-Libraries *********************** */

{report/report.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wReport
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage2:FRAME = FRAME fpage0:HANDLE
       FRAME fPage4:FRAME = FRAME fpage0:HANDLE
       FRAME fPage6:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* SETTINGS FOR FILL-IN fi-cod-estabel IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-fm-codigo-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-fm-codigo-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-ge-codigo-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-ge-codigo-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-it-codigo-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-it-codigo-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-nome-abrev-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-nome-abrev-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-periodo-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-periodo-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-tp-pedido-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi-tp-pedido-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME fPage4
                                                                        */
/* SETTINGS FOR TOGGLE-BOX lIntegracaoXTrim IN FRAME fPage4
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME fPage6
   Custom                                                               */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME fPage6     = 
                "Destino".

ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME fPage6     = 
                "Execu‡Æo".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wReport)
THEN wReport:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage2
/* Query rebuild information for FRAME fPage2
     _Query            is NOT OPENED
*/  /* FRAME fPage2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage6
/* Query rebuild information for FRAME fPage6
     _Query            is NOT OPENED
*/  /* FRAME fPage6 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON END-ERROR OF wReport /* Pedidos de Vendas / Capacidade Produ‡Æo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON WINDOW-CLOSE OF wReport /* Pedidos de Vendas / Capacidade Produ‡Æo */
DO:
  /* This event will close the window and terminate the procedure.  */
  {report/logfin.i}  
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wReport
ON CHOOSE OF btCancel IN FRAME fpage0 /* Fechar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME btConfigImpr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConfigImpr wReport
ON CHOOSE OF btConfigImpr IN FRAME fPage6
DO:
   {report/rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFile wReport
ON CHOOSE OF btFile IN FRAME fPage6
DO:
    {report/rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btHelp2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp2 wReport
ON CHOOSE OF btHelp2 IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btImportar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btImportar wReport
ON CHOOSE OF btImportar IN FRAME fpage0 /* Importar */
DO: 
    DEF VAR l-ok            AS LOG                NO-UNDO.
    DEF VAR c-arq           AS CHAR               NO-UNDO.
    def var chExcel         as com-handle         no-undo.
    def var chDataSheet     as com-handle         no-undo.
    DEF VAR i-linha         AS INT                NO-UNDO.
    DEF VAR i-nr-pedido     AS INT                NO-UNDO.
    DEF VAR i-nr-sequencia  AS INT                NO-UNDO.
    DEF VAR c-it-codigo     AS CHAR               NO-UNDO.
    DEF VAR c-cod-refer     AS CHAR               NO-UNDO.

    DEF VAR h-acomp         AS HANDLE             NO-UNDO.

    RUN pdp\espd0034a.w("espd0034",OUTPUT c-arq,OUTPUT l-ok).

    IF l-ok = YES AND search(c-arq) <> ? THEN DO:
        run utp/ut-acomp.p persistent set h-acomp.
        
        run pi-inicializar in h-acomp(input "Importando dados").

        create "excel.application":u chExcel.
    
        ASSIGN chExcel:DisplayAlerts = FALSE.
    
        chExcel:Workbooks:Open(c-arq).
        
        chExcel:VISIBLE = FALSE.
         
        ASSIGN chDataSheet = chExcel:Sheets:Item(1) NO-ERROR.

        ASSIGN i-linha = 8.
        
        b-busca-dados:
        REPEAT:
            IF chDataSheet:Range("D" + STRING(i-linha)):VALUE = "" OR 
               chDataSheet:Range("D" + STRING(i-linha)):VALUE = ?  THEN LEAVE b-busca-dados.

            ASSIGN i-nr-pedido        = chDataSheet:Range("D" + STRING(i-linha)):VALUE
                   i-nr-sequencia     = chDataSheet:Range("E" + STRING(i-linha)):VALUE
                   c-it-codigo        = chDataSheet:Range("F" + STRING(i-linha)):VALUE
                   c-cod-refer        = STRING(int(chDataSheet:Range("R" + STRING(i-linha)):VALUE),"99999999").

            run pi-acompanhar in h-acomp(input "Lendo Linha: " + string(i-linha)).
    
            FIND FIRST ped-venda NO-LOCK
                WHERE ped-venda.nr-pedido = i-nr-pedido NO-ERROR.
    
            FIND FIRST ped-item NO-LOCK
                WHERE ped-item.nome-abrev   = ped-venda.nome-abrev
                  AND ped-item.nr-pedcli    = ped-venda.nr-pedcli
                  AND ped-item.nr-sequencia = i-nr-sequencia    
                  AND ped-item.it-codigo    = c-it-codigo
                  AND ped-item.cod-refer    = c-cod-refer   NO-ERROR.
            
            IF AVAIL ped-item THEN DO:
    
                FIND FIRST if-ped-item EXCLUSIVE-LOCK
                    WHERE if-ped-item.nome-abrev   = ped-item.nome-abrev  
                      AND if-ped-item.nr-pedcli    = ped-item.nr-pedcli   
                      AND if-ped-item.nr-sequencia = ped-item.nr-sequencia
                      AND if-ped-item.it-codigo    = ped-item.it-codigo   
                      AND if-ped-item.cod-refer    = ped-item.cod-refer     NO-ERROR.
                IF NOT AVAIL if-ped-item THEN DO:
                    CREATE if-ped-item.
                    ASSIGN if-ped-item.nome-abrev   = ped-item.nome-abrev   
                           if-ped-item.nr-pedcli    = ped-item.nr-pedcli    
                           if-ped-item.nr-sequencia = ped-item.nr-sequencia 
                           if-ped-item.it-codigo    = ped-item.it-codigo    
                           if-ped-item.cod-refer    = ped-item.cod-refer.
                END.
                ASSIGN if-ped-item.campanha = chDataSheet:Range("U" + STRING(i-linha)):VALUE.
            END.
    
            ASSIGN i-linha = i-linha + 1.
        END.
    
        chExcel:Workbooks(1):CLOSE(YES).
        chExcel:QUIT().
        
        release object chDataSheet . /*NO-ERROR.*/
        release object chExcel     . /*NO-ERROR.*/

        run pi-finalizar in h-acomp.

        RUN utp/ut-msgs.p("show",
                          input 27979,
                          input "Procedimento realizado com sucesso").
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wReport
ON CHOOSE OF btOK IN FRAME fpage0 /* Executar */
DO:
   do  on error undo, return no-apply:
       run piExecute.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME rsExecution
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsExecution wReport
ON VALUE-CHANGED OF rsExecution IN FRAME fPage6
DO:
   {report/rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wReport 


/*:T--- L¢gica para inicializa‡Æo do programam ---*/
{report/mainblock.i}

ON 'value-changed':U OF rsDestiny IN FRAME fPage6
DO:
    do  with frame fPage6:
        case self:screen-value:
    
            when "1":U then do:
                assign cFile                                = c-imp-old
                       cFile:SCREEN-VALUE IN FRAME fPage6   = c-imp-old
                       cFile:SENSITIVE IN FRAME fPage6      = NO
                       cFile:VISIBLE IN FRAME fPage6        = YES
                       btFile:VISIBLE IN FRAME fPage6       = NO
                       btConfigImpr:VISIBLE IN FRAME fPage6 = YES.
    
                ASSIGN rsExecution:SENSITIVE = YES.
            end.
            when "2":U then do:
                assign cFile                 = IF rsExecution = 1 
                                                   THEN c-arq-old
                                                   ELSE c-arq-old-batch
                       cFile:SCREEN-VALUE    = cFile
                       cFile:sensitive       = yes
                       cFile:visible         = yes
                       btFile:visible        = yes.
    
                ASSIGN rsExecution:SENSITIVE = YES.
            end.
            when "3":U then do:
                assign cFile                 = ""
                       cFile:SCREEN-VALUE    = cFile
                       cFile:visible         = no
                       cFile:sensitive       = no
                       btFile:visible        = no
                       btConfigImpr:visible  = no.
    
                ASSIGN rsExecution:SENSITIVE = YES.
            END.
            WHEN "4":U THEN DO:
                assign cFile                 = ""
                       cFile:SCREEN-VALUE    = cFile
                       cFile:visible         = no
                       cFile:sensitive       = no
                       btFile:visible        = no
                       btConfigImpr:visible  = no.

                ASSIGN rsExecution = 1.
                DISP rsExecution.
    
                ASSIGN rsExecution:SENSITIVE = NO.
            END.
        end case.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wReport 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST param-xtrim NO-LOCK NO-ERROR.

    IF AVAIL param-xtrim THEN DO:
        ASSIGN fi-cod-estabel = param-xtrim.cod-estabel.
        DISP fi-cod-estabel WITH FRAME fPage2.
    END.
    
    FIND FIRST ext_usuar_grp_usuar
         WHERE ext_usuar_grp_usuar.cod_grp_usuar =  "ESPD0034"
           AND ext_usuar_grp_usuar.cod_usuario   = c-seg-usuario NO-LOCK NO-ERROR.
    IF AVAIL ext_usuar_grp_usuar THEN
        ASSIGN lIntegracaoXTrim:SENSITIVE IN FRAME fPage4 = YES.
    
    
    ASSIGN fi-periodo-ini = TODAY
           fi-periodo-fim = TODAY.

    DISP fi-periodo-ini
         fi-periodo-fim WITH FRAME fPage2.

    ASSIGN rsDestiny = 4.

    DISP rsDestiny WITH FRAME fPage6.

    ASSIGN rsExecution:SENSITIVE = NO.

    RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piExecute wReport 
PROCEDURE piExecute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

define var r-tt-digita as rowid no-undo.

/*:T** Relatorio ***/
do on error undo, return error on stop  undo, return error:
    {report/rpexa.i}

    /*15/02/2005 - tech1007 - Teste alterado pois RTF nÆo ‚ mais op‡Æo de Destino*/
    if input frame fPage6 rsDestiny = 2 and
       input frame fPage6 rsExecution = 1 then do:
        run utp/ut-vlarq.p (input input frame fPage6 cFile).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show":U, input 73, input "":U).
            apply "ENTRY":U to cFile in frame fPage6.
            return error.
        end.
    end.
    
    /*16/02/2005 - tech1007 - Teste alterado para validar o modelo informado quando for RTF*/
    &IF "{&RTF}":U = "YES":U &THEN
    IF ( input frame fPage6 cModelRTF = "" AND
         input frame fPage6 l-habilitaRtf = YES ) OR
       ( SEARCH(INPUT FRAME fPage6 cModelRTF) = ? AND
         input frame fPage6 rsExecution = 1 AND
         input frame fPage6 l-habilitaRtf = YES )
         THEN DO:
        run utp/ut-msgs.p (input "show":U, input 73, input "":U).
        /*30/12/2004 - tech1007 - Evento removido pois causa problemas no WebEnabler*/
        /*apply "CHOOSE":U to blModelRtf in frame fPage6.*/
        return error.
    END.
    &endif
    
    /*:T Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */
    
    /*:T Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    if  v_cdn_empres_usuar <> ?
    then
        assign i-ep-codigo-usuario = v_cdn_empres_usuar.

    create tt-param.
    assign tt-param.usuario              = c-seg-usuario
           tt-param.destino              = input frame fPage6 rsDestiny
           tt-param.data-exec            = today
           tt-param.hora-exec            = time
           tt-param.parametro            = YES
           tt-param.formato              = 2 /*132 colunas*/
           tt-param.v_num_tip_aces_usuar = v_num_tip_aces_usuar
           tt-param.ep-codigo            = i-ep-codigo-usuario.
    
    assign tt-param.c-cod-estabel      = INPUT FRAME fPage2 fi-cod-estabel   
           tt-param.c-it-codigo-ini    = INPUT FRAME fPage2 fi-it-codigo-ini 
           tt-param.c-it-codigo-fim    = INPUT FRAME fPage2 fi-it-codigo-fim 
           tt-param.c-fm-codigo-ini    = INPUT FRAME fPage2 fi-fm-codigo-ini 
           tt-param.c-fm-codigo-fim    = INPUT FRAME fPage2 fi-fm-codigo-fim 
           tt-param.i-ge-codigo-ini    = INPUT FRAME fPage2 fi-ge-codigo-ini 
           tt-param.i-ge-codigo-fim    = INPUT FRAME fPage2 fi-ge-codigo-fim 
           tt-param.c-nome-abrev-ini   = INPUT FRAME fPage2 fi-nome-abrev-ini
           tt-param.c-nome-abrev-fim   = INPUT FRAME fPage2 fi-nome-abrev-fim
           tt-param.c-tp-pedido-ini    = INPUT FRAME fPage2 fi-tp-pedido-ini 
           tt-param.c-tp-pedido-fim    = INPUT FRAME fPage2 fi-tp-pedido-fim 
           tt-param.dt-entrega-ini     = INPUT FRAME fPage2 fi-periodo-ini  
           tt-param.dt-entrega-fim     = INPUT FRAME fPage2 fi-periodo-fim  
           tt-param.l-integr-xtrim     = lIntegracaoXTrim:CHECKED IN  FRAME fPage4 
           tt-param.i-mercado          = INPUT FRAME fPage4 rs-mercado.       
  
 
    if tt-param.destino = 1 
    then 
        assign tt-param.arquivo = "":U.
    else if  tt-param.destino = 2 
        then assign tt-param.arquivo = input frame fPage6 cFile.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /*:T Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    
    
    
    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {report/rpexb.i}
    
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    {report/rprun.i pdp/espd0034rp.p}
    
    {report/rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {report/rptrm.i}
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

