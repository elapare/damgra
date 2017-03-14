&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/********************************************************************************
* Programa: esbs0012.w
* Data....: Jul/2016
* Autor...: Damgra - Esdon
* VersÆo..: 1.00.000                            

*******************************************************************************/
{include/i-prgvrs.i esbs0012 1.00.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGLAY 
&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGLOG f-pg-log
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    FIELD v_cdn_empres_usuar AS CHAR
    FIELD i-ep-ini         AS char
    FIELD i-ep-fim         AS CHAR
    field i-estab-ini      as char
    field i-estab-fim      as char
    field i-func-ini  as integer
    field i-func-fim  as integer
    field dt-competencia-ini  as date
    field dt-competencia-fim  as date.
     

                    
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE h-campo AS HANDLE      NO-UNDO.
def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-arq-term         as char    no-undo.
DEFINE VARIABLE c-nom-depend  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-nom-titular AS CHARACTER  NO-UNDO.
/* para executar o Excel */

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.

DEF VAR c-arq             AS CHAR                NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR                NO-UNDO.   
DEF VAR l-cria            AS LOG                 NO-UNDO.
DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
DEF VAR i-linha           AS INT                 NO-UNDO.
DEF VAR c-coluna          AS char                NO-UNDO.
DEFINE VARIABLE CONTADOR AS INTEGER    NO-UNDO.
DEF VAR c-arquivo_1 AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR c-arquivo_2 AS CHAR FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE aux-linha AS INTEGER     NO-UNDO.
DEFINE VARIABLE dt-aux AS DATE        NO-UNDO.



def var h-acomp              as handle no-undo.


{include/i-imdef.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-impor
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-import

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS im-pg-log im-pg-par im-pg-sel bt-executar ~
bt-cancelar bt-ajuda 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE IMAGE im-pg-log
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-par
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-sel
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 11.38
     FGCOLOR 0 .

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
     SIZE 78.72 BY .13
     BGCOLOR 15 .

DEFINE BUTTON bt-arquivo-destino 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr-destino 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo-destino AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-destino-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Imprime" 
      VIEW-AS TEXT 
     SIZE 9 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.72 BY .92 NO-UNDO.

DEFINE VARIABLE rs-todos AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todos", 1,
"Rejeitados", 2
     SIZE 34 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 1.71.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 1.71.

DEFINE BUTTON bt-arquivo-entrada 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY .88.

DEFINE VARIABLE c-arquivo-entrada AS CHARACTER INITIAL "c:~\temp~\Unimed" 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-entrada AS CHARACTER FORMAT "X(256)":U INITIAL "Pasta de Exporta‡Æo das Interfaces HCM x Unimed" 
      VIEW-AS TEXT 
     SIZE 22.86 BY .63 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.

DEFINE VARIABLE dt-competencia-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .88 NO-UNDO.

DEFINE VARIABLE dt-competencia-ini AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "Dt. Competˆncia" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .88 NO-UNDO.

DEFINE VARIABLE i-ano-ref AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Ano Folha" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 TOOLTIP "Ano Competˆncia" NO-UNDO.

DEFINE VARIABLE i-empresa-fim AS CHAR FORMAT "x(03)":U INITIAL "{cdp\poloestab.i 420}"/*solic-318*/
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .88 NO-UNDO.

DEFINE VARIABLE i-empresa-ini AS CHAR FORMAT "x(03)":U INITIAL "{cdp\poloestab.i 420}"/*solic-318*/
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .88 NO-UNDO.

DEFINE VARIABLE i-estab-fim AS CHAr FORMAT "x(05)":U INITIAL "424" 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .88 NO-UNDO.

DEFINE VARIABLE i-estab-ini AS CHAR FORMAT "x(05)":U INITIAL "{cdp\poloestab.i 421}" /*solic-318*/
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .88 NO-UNDO.

DEFINE VARIABLE i-func-fim AS INTEGER FORMAT ">>>>>>9":U INITIAL 9999999 
     VIEW-AS FILL-IN 
     SIZE 7.86 BY .88 NO-UNDO.

DEFINE VARIABLE i-func-ini AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Funcion rio" 
     VIEW-AS FILL-IN 
     SIZE 7.86 BY .88 NO-UNDO.

DEFINE VARIABLE i-mes-ref AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Mˆs Folha" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Mˆs Competˆncia Folha" NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-import
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Cancelar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     rt-folder AT ROW 2.5 COL 2
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-top AT ROW 2.54 COL 2.14
     im-pg-log AT ROW 1.5 COL 33.57
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-log
     rs-todos AT ROW 2.25 COL 3.29 NO-LABEL
     rs-destino AT ROW 4.5 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-arquivo-destino AT ROW 5.71 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-config-impr-destino AT ROW 5.71 COL 43.29 HELP
          "Configura‡Æo da impressora"
     c-arquivo-destino AT ROW 5.75 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 7.88 COL 3.14 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino-2 AT ROW 1.46 COL 4 NO-LABEL
     text-destino AT ROW 3.75 COL 3.86 NO-LABEL
     text-modo AT ROW 7.13 COL 1.14 COLON-ALIGNED NO-LABEL
     RECT-11 AT ROW 1.75 COL 2
     RECT-7 AT ROW 4.04 COL 2
     RECT-9 AT ROW 7.42 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.29 BY 10.46.

DEFINE FRAME f-pg-par
     c-arquivo-entrada AT ROW 9.88 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     bt-arquivo-entrada AT ROW 9.88 COL 43.14 HELP
          "Escolha do nome do arquivo"
     text-entrada AT ROW 8.83 COL 4.14 NO-LABEL
     RECT-8 AT ROW 9.17 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10.38.

DEFINE FRAME f-pg-sel
     i-empresa-ini AT ROW 2.25 COL 22 COLON-ALIGNED
     i-empresa-fim AT ROW 2.25 COL 45.14 COLON-ALIGNED NO-LABEL
     i-estab-ini AT ROW 3.25 COL 22 COLON-ALIGNED
     i-estab-fim AT ROW 3.25 COL 45.14 COLON-ALIGNED NO-LABEL
     i-func-ini AT ROW 4.25 COL 22 COLON-ALIGNED
     i-func-fim AT ROW 4.25 COL 45.14 COLON-ALIGNED NO-LABEL
     dt-competencia-ini AT ROW 5.25 COL 22 COLON-ALIGNED
     dt-competencia-fim AT ROW 5.25 COL 45.14 COLON-ALIGNED NO-LABEL
     i-mes-ref AT ROW 7.79 COL 22 COLON-ALIGNED WIDGET-ID 2
     i-ano-ref AT ROW 8.92 COL 22 COLON-ALIGNED WIDGET-ID 4
     IMAGE-1 AT ROW 2.25 COL 37.86
     IMAGE-2 AT ROW 2.25 COL 42.29
     IMAGE-3 AT ROW 3.25 COL 37.86
     IMAGE-4 AT ROW 3.25 COL 42.29
     IMAGE-5 AT ROW 4.25 COL 37.86
     IMAGE-6 AT ROW 4.25 COL 42.29
     IMAGE-7 AT ROW 5.25 COL 37.86
     IMAGE-8 AT ROW 5.25 COL 42.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-impor
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Relat¢rio de movimentos Unimed"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 30.25
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 30.25
         VIRTUAL-WIDTH      = 182.86
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
{include/w-impor.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-import
   FRAME-NAME                                                           */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-log
                                                                        */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-log
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-log     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-destino-2 IN FRAME f-pg-log
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino-2:PRIVATE-DATA IN FRAME f-pg-log     = 
                "Imprime".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-log
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-log     = 
                "Execu‡Æo".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FILL-IN text-entrada IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-entrada:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Pasta de Exporta‡Æo dos Arquivos Interfaces HCM x Unimed".

/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN dt-competencia-fim IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dt-competencia-ini IN FRAME f-pg-sel
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-log
/* Query rebuild information for FRAME f-pg-log
     _Query            is NOT OPENED
*/  /* FRAME f-pg-log */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Relat¢rio de movimentos Unimed */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Relat¢rio de movimentos Unimed */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda C-Win
ON CHOOSE OF bt-ajuda IN FRAME f-import /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-log
&Scoped-define SELF-NAME bt-arquivo-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-destino C-Win
ON CHOOSE OF bt-arquivo-destino IN FRAME f-pg-log
DO:
   /* {include/i-marq-sai.i c-arquivo-destino f-pg-log}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME bt-arquivo-entrada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-entrada C-Win
ON CHOOSE OF bt-arquivo-entrada IN FRAME f-pg-par
DO:
    {include/i-imarq.i c-arquivo-entrada f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-import
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar C-Win
ON CHOOSE OF bt-cancelar IN FRAME f-import /* Cancelar */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-log
&Scoped-define SELF-NAME bt-config-impr-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr-destino C-Win
ON CHOOSE OF bt-config-impr-destino IN FRAME f-pg-log
DO:
   {include/i-imimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-import
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar C-Win
ON CHOOSE OF bt-executar IN FRAME f-import /* Executar */
DO:
   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME i-ano-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-ano-ref C-Win
ON LEAVE OF i-ano-ref IN FRAME f-pg-sel /* Ano Folha */
DO:
  APPLY "LEAVE" TO i-mes-ref IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-mes-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-mes-ref C-Win
ON LEAVE OF i-mes-ref IN FRAME f-pg-sel /* Mˆs Folha */
DO:
  IF int(i-mes-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME}) < 1 OR 
      int(i-mes-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME}) > 12 THEN
      RETURN NO-APPLY.  
  ASSIGN
      dt-aux =  DATE(int(i-mes-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME}),01,int(i-ano-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME}))
      dt-competencia-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(dt-aux,"99/99/9999")
      dt-aux = dt-aux + 34
      dt-aux = DATE(MONTH(dt-aux),01,YEAR(dt-aux)) - 1 
      dt-competencia-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(dt-aux,"99/99/9999").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-import
&Scoped-define SELF-NAME im-pg-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-log C-Win
ON MOUSE-SELECT-CLICK OF im-pg-log IN FRAME f-import
DO:
    /*run pi-troca-pagina.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par C-Win
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-import
DO:
    /*run pi-troca-pagina.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel C-Win
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-import
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-log
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino C-Win
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-log
DO:
do  with frame f-pg-log:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo-destino:sensitive     = no
                   bt-arquivo-destino:visible      = no
                   bt-config-impr-destino:visible  = yes.
        end.
        when "2" then do:
            assign c-arquivo-destino:sensitive     = yes
                   bt-arquivo-destino:visible      = yes
                   bt-config-impr-destino:visible  = no.
        end.
        when "3" then do:
            assign c-arquivo-destino:sensitive     = no
                   bt-arquivo-destino:visible      = no
                   bt-config-impr-destino:visible  = no.
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-execucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao C-Win
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-log
DO:
   {include/i-imrse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-import
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "esbs0012" "1.00.00.000"}

/* inicializa‡äes do template de importa‡Æo */
{include/i-imini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-imlbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    
        
    RUN enable_UI.

    h-campo = FRAME f-import:FIRST-CHILD:FIRST-CHILD.
    DO WHILE VALID-HANDLE(h-campo):
 
        IF h-campo:TYPE = "text" THEN DO:
            IF h-campo:SCREEN-VALUE = "log" OR h-campo:SCREEN-VALUE = "Parƒmetros" THEN
                h-campo:VISIBLE = NO.
        END.
        IF h-campo:name = "im-pg-par" OR h-campo:name = "im-pg-log" THEN DO:
            
                h-campo:VISIBLE = NO.
        END.



        h-campo = h-campo:NEXT-SIBLING.
    END.


    FIND FIRST param_empres_rh WHERE param_empres_rh.cdn_empresa = v_cdn_empres_usuar NO-LOCK NO-ERROR.

          IF AVAIL param_empres_rh THEN
              ASSIGN
                 i-mes-ref:SCREEN-VALUE IN FRAME f-pg-sel = string(param_empres_rh.num_mes_refer_calc_efetd)
                 i-ano-ref:SCREEN-VALUE IN FRAME f-pg-sel = string(param_empres_rh.num_ano_refer_calc_efetd)
                 dt-aux = DATE(param_empres_rh.num_mes_refer_calc_efetd,01,param_empres_rh.num_ano_refer_calc_efetd)                    
                 dt-competencia-ini:SCREEN-VALUE IN FRAME f-pg-sel = string(dt-aux,"99/99/9999")
                 dt-aux = dt-aux + 34
                 dt-aux = DATE(MONTH(dt-aux),01,YEAR(dt-aux)) - 1 
                 dt-competencia-fim:SCREEN-VALUE IN FRAME f-pg-sel = string(dt-aux,"99/99/9999").

           ELSE
               ASSIGN 
                 dt-aux = TODAY
                 dt-aux = DATE(MONTH(dt-aux),01,YEAR(dt-aux)) - 1 
                 dt-aux = DATE(MONTH(dt-aux),01,YEAR(dt-aux)) 
                 dt-competencia-ini:SCREEN-VALUE IN FRAME f-pg-sel = string(dt-aux,"99/99/9999")
                 dt-aux = dt-aux + 34
                 dt-aux = DATE(MONTH(dt-aux),01,YEAR(dt-aux)) - 1 
                 dt-competencia-fim:SCREEN-VALUE IN FRAME f-pg-sel = string(dt-aux,"99/99/9999")
                 i-mes-ref:SCREEN-VALUE IN FRAME f-pg-sel = string(MONTH(dt-aux),"99")
                 i-ano-ref:SCREEN-VALUE IN FRAME f-pg-sel = string(YEAR(dt-aux),"9999").
 
  
    {include/i-immbl.i}

    /**
    {include/i-imvrf.i &programa=layout/lost0799 &versao-layout=001}
    **/  
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
  ENABLE im-pg-log im-pg-par im-pg-sel bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-import IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-import}
  DISPLAY i-empresa-ini i-empresa-fim i-estab-ini i-estab-fim i-func-ini 
          i-func-fim dt-competencia-ini dt-competencia-fim i-mes-ref i-ano-ref 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 
         i-empresa-ini i-empresa-fim i-estab-ini i-estab-fim i-func-ini 
         i-func-fim i-mes-ref i-ano-ref  dt-competencia-ini dt-competencia-fim
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-todos rs-destino c-arquivo-destino rs-execucao 
      WITH FRAME f-pg-log IN WINDOW C-Win.
  ENABLE RECT-11 RECT-7 RECT-9 rs-todos rs-destino bt-arquivo-destino 
         bt-config-impr-destino c-arquivo-destino rs-execucao 
      WITH FRAME f-pg-log IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-log}
  DISPLAY c-arquivo-entrada 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  ENABLE RECT-8 c-arquivo-entrada bt-arquivo-entrada 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
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

    /*{include/i-rpexa.i}
    if  input frame f-pg-log rs-destino = 2 and
        input frame f-pg-log rs-execucao = 1 then do:
        run utp/ut-vlarq.p (input input frame f-pg-log c-arquivo-destino).
        if  return-value = "nok" then do:
            run utp/ut-msgs.p (input "show",
                               input 73,
                               input "").
            apply 'mouse-select-click' to im-pg-log in frame f-import.
            apply 'entry' to c-arquivo-destino in frame f-pg-log.                   
            return error.
        end.
    end.*/
    
  /*  run utp/ut-vlarq.p (input input frame f-pg-par c-arquivo-entrada).
    if return-value = "nok" then do:
       run utp/ut-msgs.p (input "show",
                          input 73,
                          input "").
       apply 'mouse-select-click' to im-pg-par in frame f-import.
       apply 'entry' to c-arquivo-entrada in frame f-pg-par.
       return error.
    end.
    */

    /* Set the directory name in question */
   /* FILE-INFO:FILE-NAME = c-arquivo-entrada:SCREEN-VALUE in frame f-pg-par.
    
    IF SUBSTRING(c-arquivo-entrada:SCREEN-VALUE in frame f-pg-par,LENGTH(c-arquivo-entrada:SCREEN-VALUE in frame f-pg-par),1) <> "/" 
        AND SUBSTRING(c-arquivo-entrada:SCREEN-VALUE in frame f-pg-par,LENGTH(c-arquivo-entrada:SCREEN-VALUE in frame f-pg-par),1) <> "\"
         THEN
         c-arquivo-entrada:SCREEN-VALUE in frame f-pg-par = c-arquivo-entrada:SCREEN-VALUE in frame f-pg-par + "\".

    /* display the attributes of "dirname" */
    if FILE-INFO:FILE-TYPE = ? THEN DO:
         run utp/ut-msgs.p (input "show",
                          input 4715,
                          input c-arquivo-entrada:SCREEN-VALUE in frame f-pg-par).
       apply 'mouse-select-click' to im-pg-par in frame f-import.
       apply 'entry' to c-arquivo-entrada in frame f-pg-par.
       return error.

    END.*/
    

      /*       
    if  input frame f-pg-par fi-benef <> 0 then do:
        find First estab_benefic
            where estab_benefic.cdn_empresa = 400 And 
                  estab_benefic.cdn_beneficio = input frame f-pg-par fi-benef
            no-lock no-error.
        if  not avail estab_benefic then do:
            run utp/ut-msgs.p (input "show", input 56, input "Beneficio").
            apply 'mouse-select-click' to im-pg-par    in frame f-import.
            apply 'entry'              to fi-benef     in frame f-pg-par.
            /*return error.    */
            RETURN NO-APPLY. 
        end.
    end.
    if  input frame f-pg-par fi-benef = 0 then do:
          run utp/ut-msgs.p (input "show", input 56, input "Beneficio").
          apply 'mouse-select-click' to im-pg-par    in frame f-import.
          apply 'entry'              to fi-benef     in frame f-pg-par.
          /*return error.    */
          RETURN NO-APPLY. 
    end.
           */
                                                    /*
       find beneficio where beneficio.cdn_beneficio = input frame f-pg-par i-cd-benef no-lock no-error.
   if not available beneficio then do:
         run utp/ut-msgs.p (input "show",
                         input 3660,
                         input " ").
      apply "MOUSE-SELECT-CLICK" to im-pg-par in frame f-relat.
      apply "ENTRY" to i-cd-benef in frame f-pg-par.       
      assign c-prestador:screen-value in frame f-pg-par = " ".
      disable c-prestador with frame f-pg-par.
      return no-apply.
   end.                                               */

    /* Coloque aqui as valida‡äes das outras p ginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na p gina 
       com problemas e colocar o focus no campo com problemas             */    
         
          
         
    create tt-param.
    assign tt-param.usuario          = c-seg-usuario
           tt-param.v_cdn_empres_usuar = v_cdn_empres_usuar 
           tt-param.destino          = input frame f-pg-log rs-destino
           tt-param.todos            = input frame f-pg-log rs-todos
           tt-param.arq-entrada      = c-arquivo-entrada:SCREEN-VALUE in frame f-pg-par
           tt-param.i-ep-ini         = input frame f-pg-sel i-empresa-ini
           tt-param.i-ep-fim         = input frame f-pg-sel i-empresa-fim
           tt-param.i-estab-ini      = input frame f-pg-sel i-estab-ini
           tt-param.i-estab-fim      = input frame f-pg-sel i-estab-fim
           tt-param.i-func-ini  = input frame f-pg-sel i-func-ini
           tt-param.i-func-fim  = input frame f-pg-sel i-func-fim
           tt-param.dt-competencia-ini  = input frame f-pg-sel dt-competencia-ini
           tt-param.dt-competencia-fim  = input frame f-pg-sel dt-competencia-fim
        
           tt-param.data-exec        = today
           tt-param.hora-exec        = time.

 
    run utp/ut-acomp.p persistent set h-acomp.
    
    run pi-inicializar in h-acomp(input "Movimentos Encontrados:").
    
    ASSIGN c-arq            = SESSION:TEMP-DIRECTORY
           c-arq-anexo = ""
           i-linha = 3
           CONTADOR = 0
           c-arquivo_2 = c-arq  + 'esbs0012_' + replace(STRING(TIME,"HH:MM:SS"),":","") + '.csv' 
           c-arquivo_1 = c-arq + 'esbs0012_' + replace(STRING(TIME,"HH:MM:SS"),":","") + '.xls'. 
            



     OUTPUT TO VALUE(c-arquivo_2) NO-CONVERT.
           
      PUT "" SKIP.
      PUT "" SKIP.
    


       PUT   UNFORMATTED                                  
            "Empresa"  ";"
            "Estabelecimento"  ";"
            "Mat.Funcion rio"  ";"
            "Dependente"  ";"
            "Carteira Unimed"  ";"
            "Competˆncia"  ";"
            "Tipo Servi‡o"  ";"
            "Vl.Total"  ";"
            "Vl.Funcionario"  ";"
            "Vl.Empresa"  ";"
            "Vl.Excedente"  ";"
            "Descri‡Æo"  ";"
            "Dt. utilizacao"  ";"
            "Nome Usu rio"  ";"
            "Cod. Prestador"  ";"
            "CPF"  ";"
            "Lote"  ";"
            "Documento"  ";"
            "Nr.CHs"  ";"
            "Procedimento"  ";"
            "Quantidade"  ";"
            "Vl.Filme"  ";"
            "Vl.Material"  ";"
            "Vl.Procedimento"  ";"
            "Situa‡Æo"  ";"
            "Nome Dependente Relacionado"  ";"
            "Nome Titular Relacionado"  SKIP.
                  
       


     FOR EACH movto_unimed WHERE  movto_unimed.cdn_empresa      >= tt-param.i-ep-ini     AND 
                                    movto_unimed.cdn_empresa      <= tt-param.i-ep-fim     AND
                                    movto_unimed.cdn_estab        >= tt-param.i-estab-ini  AND  
                                    movto_unimed.cdn_estab        <= tt-param.i-estab-fim  AND  
                                    movto_unimed.cdn_funcionario  >= tt-param.i-func-ini   AND
                                    movto_unimed.cdn_funcionario  <= tt-param.i-func-fim   AND
                                    movto_unimed.dt_competencia  >= tt-param.dt-competencia-ini    AND     
                                    movto_unimed.dt_competencia  <= tt-param.dt-competencia-fim  NO-LOCK.

         find FIRST depend_func_unimed WHERE 
                            depend_func_unimed.cdn_depend_func = movto_unimed.cdn_depend_func and
                            depend_func_unimed.cdn_empresa     = movto_unimed.cdn_empresa     AND
                            depend_func_unimed.cdn_estab       = movto_unimed.cdn_estab       AND
                            depend_func_unimed.cdn_funcionario = movto_unimed.cdn_funcionario NO-LOCK NO-ERROR.

         ASSIGN 
                      c-nom-depend  = ""
                      c-nom-titular = "".
         FIND FIRST   funcionario WHERE 
             funcionario.cdn_empresa      = STRING(movto_unimed.cdn_empresa )    AND
             funcionario.cdn_estab        = STRING(movto_unimed.cdn_estab)       AND
             funcionario.cdn_funcionario  = movto_unimed.cdn_funcionario NO-LOCK NO-ERROR.

         IF AVAIL funcionario THEN
             c-nom-titular = funcionario.nom_pessoa_fisic.




         IF AVAIL  depend_func_unimed AND AVAIL funcionario THEN DO:

             FIND FIRST benefic_func  WHERE 
                    benefic_func.cdn_depend_func  = depend_func_unimed.cdn_depend_func  AND
                    benefic_func.cdn_empresa      = STRING(depend_func_unimed.cdn_empresa )     AND
                    benefic_func.cdn_estab        = STRING(depend_func_unimed.cdn_estab)        AND
                    benefic_func.cdn_funcionario  = depend_func_unimed.cdn_funcionario  AND 
                    benefic_func.cdn_beneficio >= 213 AND
                    benefic_func.cdn_beneficio <= 216 
                     NO-LOCK NO-ERROR.

             c-nom-depend = "".

             IF AVAIL benefic_func THEN DO:
             
                IF benefic_func.cdn_depend_func = 0  THEN 
                   c-nom-depend = funcionario.nom_pessoa_fisic.
                ELSE
                DO:
                  FIND FIRST depend_func  WHERE
                            depend_func.cdn_depend_func =  depend_func_unimed.cdn_depend_func  AND
                            depend_func.cdn_empresa     =  STRING(depend_func_unimed.cdn_empresa)    AND
                            depend_func.cdn_estab       =  STRING(funcionario.cdn_estab)        AND
                            depend_func.cdn_funcionario =  funcionario.cdn_funcionario  
                                  NO-LOCK NO-ERROR.

                  c-nom-depend = IF AVAIL  depend_func THEN depend_func.nom_depend_func ELSE "".

                END.
             END.
            
 

             

          END.
                         
                
           CONTADOR = CONTADOR + 1.
           run pi-acompanhar in h-acomp(input "Movimentos Encontrados: " +  STRING(CONTADOR)).
        
           ASSIGN i-linha = i-linha + 1.
    

         
         PUT UNFORMATTED

                    movto_unimed.cdn_empresa                    ";"
                    movto_unimed.cdn_estab                      ";"
                    movto_unimed.cdn_funcionario                ";"
                    movto_unimed.cdn_depend_func                ";"
                    movto_unimed.cdn_depend_func_unimed         ";"            
                    movto_unimed.dt_competencia                 ";"
                    movto_unimed.tipo_servico                   ";"
                    movto_unimed.vl_total                       ";"
                    movto_unimed.vl_funcionario                 ";"
                    movto_unimed.vl_empresa                     ";"
                    movto_unimed.vl_excedente                   ";"
                    movto_unimed.descricao                      ";"
                    movto_unimed.dt_utilizacao                  ";"    
                    movto_unimed.nome_usuario                   ";"
                    movto_unimed.cod_prestador                  ";"
                    movto_unimed.cpf                            ";"
                    movto_unimed.lote                           ";"
                    movto_unimed.documento                      ";"                  
                    movto_unimed.num_ch                         ";"
                    movto_unimed.procedimento                   ";"
                    movto_unimed.quantidade                     ";"                      
                    movto_unimed.vl_filme                       ";"
                    movto_unimed.vl_material                    ";"
                    movto_unimed.vl_procedimento                ";"
                    movto_unimed.idi_situacao                   ";"
                    c-nom-depend                                ";"
                    c-nom-titular                               SKIP.
                                                              
     END.                                        
                                                 
                                                 
      OUTPUT CLOSE.

    CREATE "Excel.Application" c-excel.
            ASSIGN c-excel:DisplayAlerts = FALSE.
            
            
            /* cria planilha*/
            
             c-planilha  = c-excel:Workbooks:OPEN(c-arquivo_2).
             c-planilha:SAVEas(c-arquivo_1,1,,,,,).
             /*c-excel:QUIT().      */
             /*RELEASE OBJECT c-excel.)*/
             
             c-planilha:CLOSE().
            
             /*CREATE "Excel.Application" c-excel.
             ASSIGN c-excel:DisplayAlerts = FALSE.*/
             ASSIGN c-planilha  = c-excel:Workbooks:OPEN(c-arquivo_1)
             c-relatorio = c-excel:Sheets:item(1)
             c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo_1.
            
            
             RUN pi-salva-planilha.
             c-excel:VISIBLE = yes.
             /*c-excel:QUIT().  */
             
             RELEASE OBJECT c-relatorio.
             RELEASE OBJECT c-planilha.
             RELEASE OBJECT c-excel.


    
    
    
    /* logica do relatorio */

    RUN pi-finalizar IN h-acomp.               






end.

END PROCEDURE.


PROCEDURE pi-salva-planilha:
   aux-linha = i-linha.
                   ASSIGN 

                       i-linha = 3

                   c-relatorio:range("a3:aa3"):Interior:ColorIndex = 55.
                   c-relatorio:range("a3:aa3"):Font:Name = "Arial".
                   c-relatorio:range("a3:aa3"):Font:FontStyle = "Negrito".
                   c-relatorio:range("a3:aa3"):Font:Size = 10.
                   c-relatorio:range("a3:aa3"):Font:ColorIndex = 2.
                   c-relatorio:Rows("3:3"):Autofilter(,,,).
                   c-relatorio:Cells:Select.
                   c-relatorio:Cells:EntireColumn:AutoFit.
                   c-relatorio:range("A1"):VALUE = "Relat¢rio de movimentos Unimed" + " - Data: " + STRING(TODAY,"99/99/9999").               
                   c-relatorio:range("A1"):Font:FontStyle = "Negrito".
                   c-relatorio:range("A1"):Font:Size = 11.
               
                   c-relatorio:Range("A1:i1"):Merge.
                   c-relatorio:Range("A1:i1"):Horizontalalignment = 3.
                   c-relatorio:range("a3:aa" + STRING(aux-linha) ):Borders(01):LineStyle = 1.
                   c-relatorio:range("a3:aa" + STRING(aux-linha) ):Borders(02):LineStyle = 1.
                   c-relatorio:range("a3:aa" + STRING(aux-linha) ):Borders(03):LineStyle = 1.
                   c-relatorio:range("a3:aa" + STRING(aux-linha) ):Borders(04):LineStyle = 1.
                   c-relatorio:range("a3:aa" + STRING(aux-linha) ):Horizontalalignment = 3.
                   /*c-relatorio:range("H3:AI3"):Horizontalalignment = 3.*/
                   
                   c-relatorio:range("a3:aa" + STRING(aux-linha) ):Horizontalalignment = 1.
                   c-relatorio:range("f4:f" + STRING(aux-linha) ):Horizontalalignment = 4.
                   c-relatorio:range("h4:h" + STRING(aux-linha) ):Horizontalalignment = 4.
                   c-relatorio:range("j4:j" + STRING(aux-linha) ):Horizontalalignment = 4.
                   c-relatorio:range("r4:u" + STRING(aux-linha) ):Horizontalalignment = 4.
                   c-relatorio:range("z4:z" + STRING(aux-linha) ):Horizontalalignment = 4.
                   c-relatorio:range("ab4:ad" + STRING(aux-linha) ):Horizontalalignment = 4.
                   c-relatorio:range("ag4:AA" + STRING(aux-linha) ):Horizontalalignment = 4.
                   c-relatorio:range("aA4:AB" + STRING(aux-linha) ):Horizontalalignment = 1.

                   c-relatorio:range("h4:k" + STRING(aux-linha) ):Style = "currency".
                   c-relatorio:range("v4:x" + STRING(aux-linha) ):Style = "currency".
                   c-relatorio:range("u4:u" + STRING(aux-linha)):NumberFormat = "0".

                      


                   c-relatorio:Cells:Select.
                   c-relatorio:Cells:EntireColumn:AutoFit.

                   c-relatorio:range("a1"):Select.
               
                   c-planilha:SAVE().
                  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina C-Win 
PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* if input frame f-pg-par fi-mes-ref = 0 then 
  do:
      find first param_empres_rh WHERE param_empres_rh.cdn_empresa >= 380 no-lock no-error.
      if avail param_empres_rh then 
          assign fi-ano-ref = param_empres_rh.num_ano_refer_calc_efetd
                 fi-mes-ref = param_empres_rh.num_mes_refer_calc_efetd. 
      disp fi-ano-ref 
           fi-mes-ref
            with frame f-pg-par.
   end. 
*/
{include/i-imtrp.i}

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
     Tables specified for this w-impor, and there are no
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

