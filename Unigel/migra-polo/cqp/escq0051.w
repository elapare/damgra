

&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME

&Scoped-define WINDOW-NAME w-relat

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 

/***************************************************************************
 **Programa.: escq0051.W 
 **Autor....: Amgra - Jos‚ Roberto
 **Objetivo.: Laudo de Qualidade Autom tico
 **Data.....: 26/05/2008
 **
 ***************************************************************************/
def buffer empresa for mgmulti.empresa.
{include/i-prgvrs.i escq0051 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA f-pg-cla
&GLOBAL-DEFINE PGDIG f-pg-dig
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    field v_num_tip_aces_usuar as integer
    field ep-codigo            as char
    field c-cod-estabel-ini    like ped-venda.cod-estabel
    FIELD c-serie              AS CHAR 
    field c-cdd-embarq-ini    AS INTEGER 
    field c-cdd-embarq-fim    AS INTEGER 
    FIELD pesq-jr              AS INT 
    FIELD tipo-geracao         AS INT 
    FIELD i-modelo             AS INT
    field tb-v-temp            AS LOG   
    .

DEFINE TEMP-TABLE tt-digita no-undo
    FIELD nome-abrev           LIKE emitente.nome-abrev LABEL "Cliente"
    FIELD nr-nota-fisc         LIKE nota-fiscal.nr-nota-fis LABEL "N.Fiscal"
    FIELD serie                LIKE nota-fiscal.serie   LABEL "S‚rie"
    FIELD it-codigo            AS CHAR FORMAT "x(16)"   LABEL "Filme"
    FIELD nr-pedido            AS INT  FORMAT ">>>>>>9" LABEL "Pedido"
    FIELD nr-sequencia         LIKE ped-item.nr-sequencia
    FIELD tipo-modelo          AS INT 
    INDEX chave IS PRIMARY UNIQUE nome-abrev  
                                  nr-nota-fisc
                                  it-codigo   
                                  nr-sequencia 
                                  nr-pedido.

define buffer b-tt-digita for tt-digita.
Def    Buffer bf-digita   For tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */
DEF VAR aux-lote           LIKE item-doc-est.lote NO-UNDO.
Def Var qt-lote            As Dec     No-undo.
def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var h-acomp            as handle no-undo.
def var v-num-reg-lidos    as int    no-undo.


DEFINE VARIABLE desc-ex AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-ext   AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-ex    AS INTEGER    NO-UNDO.
DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
DEFINE VARIABLE b       AS INTEGER    NO-UNDO.
DEFINE VARIABLE obs-ex  AS CHARACTER  FORMAT "x(30)" NO-UNDO.

DEFINE VARIABLE tipo-modelo-jr  AS INTEGER    NO-UNDO.


DEFINE VARIABLE nr-nota-fis-jr LIKE nota-fiscal.nr-nota-fis LABEL "Nr.Fiscal"  NO-UNDO.

/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 
DEFINE VARIABLE pesq-jr AS INTEGER INITIAL 1
    VIEW-AS RADIO-SET vertical
    RADIO-BUTTONS 
    "Por Notas Fiscais", 1,
 /* "Por Embarques", 2, */
    "Por Nr. de EXT", 2
    SIZE 35 BY 2 NO-UNDO.

DEFINE VARIABLE tipo-geracao AS INTEGER INITIAL 1
    VIEW-AS RADIO-SET vertical
    RADIO-BUTTONS 
    "Gera‡Æo para Conferˆncia", 1,
    "Gera‡Æo Oficial (Envio de E-mails)", 2
    SIZE 35 BY 1.50 NO-UNDO.

DEFINE VARIABLE i-modelo AS INTEGER INITIAL 1
    VIEW-AS RADIO-SET HORIZONTAL 
    RADIO-BUTTONS 
    "Normal", 1,
 /* "Mobil", 2, */
    "S.Cruz", 2,
    "Philip Morris", 3
    SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE tb-v-temp AS LOGICAL INITIAL no 
LABEL "Grava Planilha no v:\temp"
VIEW-AS TOGGLE-BOX 
SIZE 22 BY .83 
NO-UNDO.


DEFINE VARIABLE c-nr-ext AS CHAR FORMAT "x(15)" INITIAL " ":U 
     LABEL "Nr.da EXT" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Informe Nr.da EXT " NO-UNDO.


DEFINE VARIABLE dt-emis-ini AS DATE FORMAT "99/99/9999"  
     LABEL "Data do Faturamento" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Informe a Data Inicial do Faturamento " NO-UNDO.


DEFINE VARIABLE dt-emis-fim AS DATE FORMAT "99/99/9999"  
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Informe a Data Final do Faturamento " NO-UNDO.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */

&Scoped-define FRAME-NAME f-pg-cla 
&Scoped-define FRAME-NAME f-pg-sel 

&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita */

&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.nome-abrev tt-digita.nr-nota-fisc tt-digita,serie tt-digita.it-codigo tt-digita.nr-pedido nr-sequencia tt-digita.tipo-modelo 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.nome-abrev tt-digita.nr-nota-fisc tt-digita.serie tt-digita.it-codigo tt-digita.nr-pedido nr-sequencia tt-digita.tipo-modelo    
&Scoped-define ENABLED-TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-digita tt-digita
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-digita
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY br-digita FOR EACH tt-digita.
&Scoped-define TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-digita

/* Definitions for FRAME f-pg-dig                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-pg-dig ~
    ~{&OPEN-QUERY-br-digita}

/* Standard List Definitions */

&Scoped-Define ENABLED-OBJECTS rs-classif 
&Scoped-Define DISPLAYED-OBJECTS rs-classif 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rs-classif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Data Transa‡Æo/ Item", 1
     SIZE 28 BY 1 NO-UNDO.

DEFINE BUTTON bt-alterar 
     LABEL "Alterar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-inserir 
     LABEL "Inserir" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-recuperar 
     LABEL "Recuperar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-retirar 
     LABEL "Retirar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-salvar 
     LABEL "Salvar" 
     SIZE 15 BY 1.

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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 4 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
          "Arquivo", 2,
          "Terminal", 3,
          "Excel" , 4
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

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.29 BY 2.50.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.29 BY 6.12.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.29 BY 1.30.

DEFINE VARIABLE c-cod-estabel-ini AS CHAR label "Estabelecimento" FORMAT "x(3)"   /*solic-318*/ 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88
     TOOLTIP "Informe C¢digo do Estabelecimento " NO-UNDO.

DEFINE VARIABLE c-cdd-embarq-ini AS INT FORMAT ">>>>>>9" INITIAL "0":U 
     LABEL "N.Fiscal/Embarque" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Informe Nr.N.Fiscal/Embarque " NO-UNDO.

DEFINE VARIABLE c-cdd-embarq-fim AS INT FORMAT ">>>>>>9" INITIAL "9999999":U 
     VIEW-AS FILL-IN
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-serie-ini AS CHAR FORMAT "x(5)" INITIAL "20":U 
     LABEL "S‚rie" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Informe a S‚rie da N.Fiscal " NO-UNDO.

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


DEFINE IMAGE IMAGE-47
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-54
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-55
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-56
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-86
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-87
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-88
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-89
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-paletes 
     LABEL "Notas Fiscais" 
     SIZE 15 BY 1.

DEFINE IMAGE im-pg-cla
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-dig
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-imp
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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-digita SCROLLING.
&ANALYZE-RESUME


/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita w-relat _FREEFORM
  QUERY br-digita DISPLAY tt-digita.nome-abrev   
                          tt-digita.nr-nota-fisc
                          tt-digita.serie
                          tt-digita.it-codigo    
                          tt-digita.nr-sequencia
                          tt-digita.nr-pedido 
                          tt-digita.tipo-modelo

  ENABLE
    tt-digita.nr-nota-fisc 
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9
         BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configura‡Æo da impressora"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"

     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"

    bt-paletes AT ROW 14.54 COL 25 HELP
         "Monta Tabela de Notas Fiscais"

     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"

     rt-folder AT ROW 2.5 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     im-pg-cla AT ROW 1.5 COL 17.84   
     im-pg-dig AT ROW 1.5 COL 33.57
     im-pg-imp AT ROW 1.5 COL 49.28
/*     im-pg-par AT ROW 1.5 COL 33.57     */
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-paletes.

DEFINE FRAME f-pg-sel

   pesq-jr label "Op‡äes:"
     at ROW 1.2 col 10 HELP
     "Escolha como Pesquisar os Documentos"

   c-cod-estabel-ini label "Estabelecimento"
     at row 3.8 col 28 colon-aligned
     view-as fill-in 
     size 10 by .88
     font 1

   c-serie-ini label "S‚rie"
     at row 3.8 col 46 colon-aligned
     view-as fill-in 
     size 10 by .88
     font 1

   c-cdd-embarq-ini LABEL "Nr.N.Fiscal" 
     at row 4.8 col 28 colon-aligned
     view-as FILL-IN 
     size 10 by .88
     font 1
   
   c-cdd-embarq-fim NO-LABEL 
     at row 4.8 col 46 colon-aligned
     view-as FILL-IN 
     size 10 by .88
     font 1

   c-nr-ext LABEL "Nr. da EXT" 
     at row 5.8 col 28 colon-aligned
     view-as FILL-IN 
     size 10 by .88
     font 1

   dt-emis-ini LABEL "Data do Faturamento" 
     at row 6.8 col 28 colon-aligned
     view-as FILL-IN 
     size 10 by .88
     font 1

   dt-emis-fim NO-LABEL  
     at row 6.8 col 46 colon-aligned
     view-as FILL-IN 
     size 10 by .88
     font 1    

   tipo-geracao label "Op‡äes:"
     at ROW 8 col 16 HELP
     "Escolha o Tipo de Gera‡Æo"

   tb-v-temp     AT ROW 10 COL 20


/*
   i-modelo label "Modelo do Laudo"
     at ROW 9.9 col 8 HELP
     "Escolha o Modelo para o Laudo"
*/

   RECT-19 AT ROW 1.00 COL 11
   RECT-20 AT ROW 3.6  COL 11
   RECT-21 AT ROW 9.8  COL 11

   IMAGE-1  AT ROW 4.8 col 41
   IMAGE-2  AT ROW 4.8 col 44

   IMAGE-3  AT ROW 6.8 col 41
   IMAGE-4  AT ROW 6.8 col 44


   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D
   AT COL 3 ROW 2.85
   SIZE 76.86 BY 10.62.

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 1.5 COL 8 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.31.

DEFINE FRAME f-pg-dig
     br-digita AT ROW 1 COL 1
     bt-inserir AT ROW 10 COL 1
     bt-alterar AT ROW 10 COL 16
     bt-retirar AT ROW 10 COL 31
     bt-salvar AT ROW 10 COL 46
     bt-recuperar AT ROW 10 COL 61
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.31
         SIZE 76.86 BY 10.15.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-relat
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-relat ASSIGN
         HIDDEN             = YES
         TITLE              = "Certificado de Qualidade"
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-relat 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-relat.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-relat
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-pg-cla
                                                                        */
/* SETTINGS FOR FRAME f-pg-dig
                                                                        */
/* BROWSE-TAB br-digita 1 f-pg-dig */
/* SETTINGS FOR BUTTON bt-alterar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-retirar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-salvar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-imp
                                                                        */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu‡Æo".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-relat
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
THEN w-relat:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY br-digita FOR EACH tt-digita.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-digita */
&ANALYZE-RESUME

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


on LEAVE OF pesq-jr in FRAME f-pg-sel do:
   
   IF INT(pesq-jr:SCREEN-VALUE IN FRAME f-pg-sel) = 2 THEN

    
     ASSIGN c-cdd-embarq-ini:SCREEN-VALUE IN FRAME f-pg-sel = "0"
            c-cdd-embarq-fim:SCREEN-VALUE IN FRAME f-pg-sel = "9999999"
            dt-emis-ini:SCREEN-VALUE IN FRAME f-pg-sel = string(today)
            dt-emis-fim:SCREEN-VALUE IN FRAME f-pg-sel = string(today)
            c-cdd-embarq-ini :sensitive in frame f-pg-sel   = no
            c-cdd-embarq-fim :sensitive in frame f-pg-sel   = no
            c-nr-ext:sensitive in frame f-pg-sel             = yes 
            dt-emis-ini:sensitive in frame f-pg-sel          = yes 
            dt-emis-fim:sensitive in frame f-pg-sel          = yes. 

   ELSE

       ASSIGN c-nr-ext:SCREEN-VALUE IN FRAME f-pg-sel        = " "
              dt-emis-ini:SCREEN-VALUE IN FRAME f-pg-sel     = ?
              dt-emis-fim:SCREEN-VALUE IN FRAME f-pg-sel     = ?
              c-cdd-embarq-ini :sensitive in frame f-pg-sel = yes
              c-cdd-embarq-fim :sensitive in frame f-pg-sel = yes
              c-nr-ext:sensitive in frame f-pg-sel           = no 
              dt-emis-ini:sensitive in frame f-pg-sel        = no 
              dt-emis-fim:sensitive in frame f-pg-sel        = no.  


end.



&Scoped-define SELF-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON END-ERROR OF w-relat /* NFs Recebidas para Controle  Etiquetas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* NFs Recebidas para Controle  Etiquetas */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-digita
&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON DEL OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose' to bt-retirar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON END-ERROR OF br-digita IN FRAME f-pg-dig
ANYWHERE 
DO:
    if  br-digita:new-row in frame f-pg-dig then do:
        if  avail tt-digita then
            delete tt-digita. 
        if  br-digita:delete-current-row() in frame f-pg-dig then. 
    end.                                                               
    else do:
        get current br-digita.
        display tt-digita.nome-abrev  
                tt-digita.nr-nota-fisc
                tt-digita.serie
                tt-digita.it-codigo   
                tt-digita.nr-sequencia
                tt-digita.nr-pedido
                tt-digita.tipo-modelo
            with browse br-digita. 

    end.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ENTER OF br-digita IN FRAME f-pg-dig
ANYWHERE
DO:
  apply 'tab' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON INS OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose' to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-END OF br-digita IN FRAME f-pg-dig
DO:
   apply 'entry' to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-HOME OF br-digita IN FRAME f-pg-dig
DO:
  apply 'entry' to bt-recuperar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-ENTRY OF br-digita IN FRAME f-pg-dig
DO:
   /* trigger para inicializar campos da temp table de digita‡Æo */
/*   if  br-digita:new-row in frame f-pg-dig then do:
       assign tt-digita.nome:screen-value in browse br-digita = string(today, "99/99/9999").
   end.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
DO:
    /*  aqui que a grava‡Æo da linha da temp-table ? efetivada.
       Por‚m as valida‡äes dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment rio */
    
    if br-digita:NEW-ROW in frame f-pg-dig then 
    do transaction on error undo, return no-apply:
        create tt-digita.
        assign input browse br-digita tt-digita.nome-abrev  
               INPUT BROWSE br-digita tt-digita.nr-nota-fisc
               INPUT BROWSE br-digita tt-digita.serie
               input browse br-digita tt-digita.it-codigo   
               input browse br-digita tt-digita.nr-pedido 
               input browse br-digita tt-digita.tipo-modelo
               input browse br-digita tt-digita.nr-sequencia.  
            
            
            br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    end.
    else do transaction on error undo, return no-apply:
        
            assign input browse br-digita tt-digita.nome-abrev  
                   INPUT BROWSE br-digita tt-digita.nr-nota-fisc
                   INPUT BROWSE br-digita tt-digita.serie
                   input browse br-digita tt-digita.it-codigo     
                   input browse br-digita tt-digita.nr-pedido    
                   input browse br-digita tt-digita.tipo-modelo
                   input browse br-digita tt-digita.nr-sequencia.  

            display
                tt-digita.nome-abrev  
                tt-digita.nr-nota-fisc
                tt-digita.serie
                tt-digita.it-codigo   
                tt-digita.nr-sequencia   
                tt-digita.nr-pedido
                tt-digita.tipo-modelo
                with browse br-digita. 
                                          
   end.
 
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-relat
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
/*   {include/ajuda.i}   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-alterar w-relat
ON CHOOSE OF bt-alterar IN FRAME f-pg-dig /* Alterar */
DO:
  apply 'entry' to tt-digita.nome-abrev in browse br-digita. 
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo w-relat
ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:
    {include/i-rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-paletes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-paletes w-relat
ON CHOOSE OF bt-paletes IN FRAME f-relat /* Carrega Paletes */

DO:
    CLOSE QUERY br-digita.
    FOR EACH tt-digita.
        DELETE tt-digita.
    END.
    
    run pi-desabilita-folder.

END.


/* _UIB-CODE-BLOCK-END */

&ANALYZE-RESUME

&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-relat
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Fechar */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-config-impr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr w-relat
ON CHOOSE OF bt-config-impr IN FRAME f-pg-imp
DO:
   {include/i-rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-relat
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:
   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-inserir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inserir w-relat
ON CHOOSE OF bt-inserir IN FRAME f-pg-dig /* Inserir */
DO:
    enable bt-retirar bt-salvar bt-alterar with frame f-pg-dig.
           
    if  num-results('br-digita') > 0 then do:
        if  br-digita:insert-row('after') in frame f-pg-dig then.
    end.
    else do transaction:
        create tt-digita.
        open query br-digita for each tt-digita.
        apply 'entry' to tt-digita.nome-abrev in browse br-digita. 
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-recuperar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-recuperar w-relat
ON CHOOSE OF bt-recuperar IN FRAME f-pg-dig /* Recuperar */
DO:
    {include/i-rprcd.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-retirar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retirar w-relat
ON CHOOSE OF bt-retirar IN FRAME f-pg-dig /* Retirar */
DO:
    if  br-digita:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-digita.
        delete tt-digita.
        if  br-digita:delete-current-row() in frame f-pg-dig then.
    end.
    
    if num-results("br-digita") = 0 then
        assign bt-alterar:SENSITIVE in frame f-pg-dig = no
               bt-retirar:SENSITIVE in frame f-pg-dig = no
               bt-salvar:SENSITIVE in frame f-pg-dig  = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salvar w-relat
ON CHOOSE OF bt-salvar IN FRAME f-pg-dig /* Salvar */
DO:
   {include/i-rpsvd.i}
END.

/* _UIB-CODE-BLOCK-END */

/*
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-cla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-cla w-relat
ON MOUSE-SELECT-CLICK OF im-pg-cla IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */

*/

&ANALYZE-RESUME
&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-dig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-dig w-relat
ON MOUSE-SELECT-CLICK OF im-pg-dig IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-imp w-relat
ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
/*
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par w-relat
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
  run pi-troca-pagina.

END.

/* _UIB-CODE-BLOCK-END */
*/

&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel w-relat
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
  run pi-troca-pagina.
 
END.

/* _UIB-CODE-BLOCK-END */

&ANALYZE-RESUME

&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino w-relat
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = yes.
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = yes
                   bt-arquivo:visible      = yes
                   bt-config-impr:visible  = no.
        end.
        when "3" then do:
            assign c-arquivo:sensitive     = no
                   bt-arquivo:visible      = no
                   bt-config-impr:visible  = no.
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-execucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao w-relat
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-imp
DO:
   {include/i-rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-cla
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 

on LEAVE OF tt-digita.nome-abrev in browse br-digita do:
   run pi-leave-pallete.
end.


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "escq0051" "2.06.00.001"}

/* inicializa‡äes do template de relat¢rio */
{include/i-rpini.i}

 ASSIGN rs-destino = 4.

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

    c-cod-estabel-ini = STRING({cdp\poloestab.i 434}). /*solic-318*/ 
    RUN enable_UI.
    
    ASSIGN c-cdd-embarq-ini :sensitive in frame f-pg-sel = no
           c-cdd-embarq-fim :sensitive in frame f-pg-sel = no
           c-nr-ext:sensitive in frame f-pg-sel           = no 
           dt-emis-ini:sensitive in frame f-pg-sel        = no 
           dt-emis-fim:sensitive in frame f-pg-sel        = no. 


    assign im-pg-dig:sensitive in frame f-relat = no
           wh-label-dig:fgcolor                 = 7.

    
    assign im-pg-imp:sensitive in frame f-relat = no
           wh-label-dig:fgcolor                 = 7.

    assign im-pg-cla:sensitive in frame f-relat = no
           wh-label-cla:fgcolor                 = 7.

    /*if i-cod-termo:load-mouse-pointer("image/lupa.cur") in frame f-pg-par then.*/
    {include/i-rpmbl.i im-pg-cla}  

    {include/i-rpmbl.i}   
  
    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-relat  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-relat  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-relat  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
  THEN DELETE WIDGET w-relat.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-relat  _DEFAULT-ENABLE
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
  ENABLE /*bt-executar*/ bt-cancelar bt-paletes bt-ajuda /*im-pg-cla*/ im-pg-dig im-pg-imp 
        /* im-pg-par */im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.

  {&OPEN-BROWSERS-IN-QUERY-f-relat}

   
   DISPLAY  
   pesq-jr
   c-cod-estabel-ini 
   c-serie-ini
   c-cdd-embarq-ini
   c-cdd-embarq-fim
   c-nr-ext
   dt-emis-ini
   dt-emis-fim
   tipo-geracao
   tb-v-temp
 /*  i-modelo  */
   WITH FRAME f-pg-sel IN WINDOW w-relat.

   ENABLE   
   pesq-jr
   c-cod-estabel-ini 
   c-serie-ini
   c-cdd-embarq-ini
   c-cdd-embarq-fim
   c-nr-ext
   dt-emis-ini
   dt-emis-fim
   tipo-geracao
   tb-v-temp
 /*  i-modelo  */
   WITH FRAME f-pg-sel IN WINDOW w-relat.

  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  ENABLE rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-arquivo bt-config-impr c-arquivo rs-execucao RECT-7 
         RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}

  ENABLE br-digita bt-inserir bt-recuperar 
      WITH FRAME f-pg-dig IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-dig}
  VIEW w-relat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-relat 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-desabilita-folder w-relat 
PROCEDURE pi-desabilita-folder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
Do:
     /** Montando a tabela para tt-digita ***/
     run utp/ut-acomp.p persistent set h-acomp.

     {utp/ut-liter.i Gerando_movimento * I}

     run pi-inicializar in h-acomp (input "Carregando Paletes ...Aguarde").  

/*rotina de montagem do browse */ 

     FOR EACH tt-digita.
         DELETE tt-digita.
     END.


     IF INPUT FRAME f-pg-sel pesq-jr = 1 THEN DO:   /* por N.Fiscais */

         FOR EACH nota-fiscal WHERE
             nota-fiscal.cod-estabel        = INPUT FRAME f-pg-sel c-cod-estabel-ini AND
             nota-fiscal.serie              = INPUT FRAME f-pg-sel c-serie-ini       AND    
             INT (nota-fiscal.nr-nota-fis) >= INPUT FRAME f-pg-sel c-cdd-embarq-ini AND
             INT (nota-fiscal.nr-nota-fis) <= INPUT FRAME f-pg-sel c-cdd-embarq-fim AND
             nota-fiscal.dt-cancela         = ?
             USE-INDEX ch-nota NO-LOCK,

             EACH it-nota-fisc OF nota-fiscal NO-LOCK,

                 EACH ITEM OF it-nota-fisc NO-LOCK.


                 FIND FIRST natur-oper OF nota-fiscal NO-LOCK.
                 
                 IF AVAIL natur-oper AND natur-oper.terceiros = yes and
                    natur-oper.tp-oper-terc = 1 THEN NEXT.

                 
                 IF ITEM.ge-codigo <> 46 THEN NEXT.

                 IF ITEM.fm-codigo = "46-24" OR 
                    ITEM.fm-codigo = "46-26" OR
                    ITEM.fm-codigo = "46-25" THEN NEXT.
                    
                  
                 run pi-acompanhar in h-acomp(input "Nota fiscal: " + nota-fiscal.nr-nota-fis).
               
                 FIND FIRST polo-laudo-cliente WHERE
                     polo-laudo-cliente.nome-abrev = nota-fiscal.nome-ab-cli
                     NO-LOCK NO-ERROR.
               
                 IF NOT AVAIL polo-laudo-cliente THEN NEXT.  


    /* Rotina para testar se j  gerado laudo para conferˆncia */

             IF int(tipo-geracao:SCREEN-VALUE IN FRAME f-pg-sel) = 2 THEN DO:  /* oficial */           


                 FIND FIRST pol-param-relatorio WHERE
                     pol-param-relatorio.cod-prog-dtsul = "ESCQ0051" AND
                     pol-param-relatorio.cod-usuario    = nota-fiscal.nr-nota-fis AND
                     pol-param-relatorio.mes-refer      = int(nota-fiscal.serie)  AND  
                     pol-param-relatorio.ano-refer      = int(nota-fiscal.cod-estabel)
                     NO-LOCK NO-ERROR.
                     
                     
                IF NOT AVAIL pol-param-relatorio THEN                     
                     FIND FIRST pol-param-relatorio WHERE
                     pol-param-relatorio.cod-prog-dtsul = "ESCQ0051" AND
                     pol-param-relatorio.cod-usuario    = nota-fiscal.nr-nota-fis AND
                     pol-param-relatorio.mes-refer      = 20  AND  
                     pol-param-relatorio.ano-refer      = int(nota-fiscal.cod-estabel)
                     NO-LOCK NO-ERROR.
                     

                     
                 IF NOT AVAIL pol-param-relatorio THEN DO:

                     RUN mens-falta-conferencia.

                     NEXT.

                 END.

             END.
    
    /* Fim da Rotina para testar se j  gerado laudo para conferˆncia */



               IF INT(tipo-geracao:SCREEN-VALUE IN FRAME f-pg-sel) = 2 THEN DO:

                  FIND FIRST am-cq-laudo WHERE
                      am-cq-laudo.cod-estabel = nota-fiscal.cod-estabel AND
                      am-cq-laudo.nr-nota-fis = nota-fiscal.nr-nota-fis AND
                      am-cq-laudo.it-codigo   = it-nota-fisc.it-codigo  AND
                      am-cq-laudo.dec-1       = it-nota-fisc.nr-seq-fat 
                      NO-LOCK NO-ERROR.
                
                  IF AVAIL am-cq-laudo and am-cq-laudo.dec-1 = it-nota-fisc.nr-seq-fat and am-cq-laudo.dt-criacao >= 04/01/2009 THEN NEXT.

               END.

               FIND FIRST fat-ser-lote OF it-nota-fisc NO-LOCK NO-ERROR.

               IF NOT AVAIL fat-ser-lote THEN NEXT.

               ASSIGN tipo-modelo-jr = 0.

               FIND FIRST polo-laudo-cliente WHERE
                   polo-laudo-cliente.nome-abrev = nota-fiscal.nome-ab-cli
                   NO-LOCK NO-ERROR.

               IF AVAIL polo-laudo-cliente THEN DO:

                   ASSIGN tipo-modelo-jr = int(polo-laudo-cliente.dec-1).

                   IF tipo-modelo-jr = 1 OR 
                      tipo-modelo-jr > 3 THEN ASSIGN tipo-modelo-jr = 0.

               END.

               FIND tt-digita WHERE
                    tt-digita.nome-abrev   = nota-fiscal.nome-ab-cli  AND
                    tt-digita.nr-nota-fisc = nota-fiscal.nr-nota-fis  AND
                    tt-digita.it-codigo    = it-nota-fisc.it-codigo   AND
                    tt-digita.nr-pedido    = it-nota-fisc.nr-pedido   AND
                    tt-digita.nr-sequencia = it-nota-fisc.nr-seq-fat
                    NO-ERROR.
    
               IF NOT AVAIL tt-digita THEN 
                    CREATE tt-digita.
    
               ASSIGN tt-digita.nome-abrev   = nota-fiscal.nome-ab-cli 
                      tt-digita.nr-nota-fisc = nota-fiscal.nr-nota-fis
                      tt-digita.serie        = nota-fiscal.serie
                      tt-digita.it-codigo    = it-nota-fisc.it-codigo  
                      tt-digita.nr-pedido    = it-nota-fisc.nr-pedido 
                      tt-digita.tipo-modelo  = tipo-modelo-jr
                      tt-digita.nr-sequencia = it-nota-fisc.nr-seq-fat. 
    
                assign v-num-reg-lidos = v-num-reg-lidos + 1.
                run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

         END. /* nota-fiscal */

     END. /* pesq-jr = 1 - Por Nota Fiscal */

     IF INPUT FRAME f-pg-sel pesq-jr = 2 THEN DO:   /* por N.Ext */

         FOR EACH nota-fiscal WHERE
             nota-fiscal.cod-estabel        = INPUT FRAME f-pg-sel c-cod-estabel-ini AND
             nota-fiscal.dt-emis-nota      >= INPUT FRAME f-pg-sel dt-emis-ini       AND
             nota-fiscal.dt-emis-nota      <= INPUT FRAME f-pg-sel dt-emis-fim       AND
             nota-fiscal.dt-cancela         = ?
             USE-INDEX ch-sit-nota NO-LOCK,

             EACH it-nota-fisc OF nota-fiscal NO-LOCK,

                 EACH ITEM OF it-nota-fisc NO-LOCK.
/*
                 FIND FIRST natur-oper OF nota-fiscal NO-LOCK.

                 IF AVAIL natur-oper AND natur-oper.terceiros = yes and
                    natur-oper.tp-oper-terc = 1 THEN NEXT.
*/

                 FIND FIRST ped-venda WHERE
                     ped-venda.nr-pedido = it-nota-fisc.nr-pedido
                     NO-LOCK NO-ERROR.
                
                 IF NOT AVAIL ped-venda THEN NEXT.
                       
                 ASSIGN desc-ex = ped-venda.observacoes 
                        i-ext = INDEX(desc-ex,"EXT",1)
                        i-ex =  INDEX(desc-ex,"EX",1)
                        i = 0.
                 IF i-ext >  0 THEN
                     ASSIGN i = i-ext.
                 ELSE 
                     IF i-ex > 0 THEN ASSIGN i = i-ex.
                 
                 IF i > 0 THEN 
                     ASSIGN
                        desc-ex = SUBSTR(desc-ex, i,20) 
                        b =       INDEX(desc-ex,"/",1) + 2
                        desc-ex = SUBSTR(desc-ex, 1,b).
                     ELSE 
                        ASSIGN desc-ex = " ".
                 
                 ASSIGN obs-ex = desc-ex.

                 IF SUBSTRING(obs-ex,1,3) = "EXT" THEN 
                     ASSIGN SUBSTRING(obs-ex,1,3) = "   ".
               
                 IF SUBSTRING(obs-ex,1,4) = "EXT:" THEN 
                     ASSIGN SUBSTRING(obs-ex,1,4) = "    ".
               
                 IF SUBSTRING(obs-ex,1,2) = "EX" THEN 
                     ASSIGN SUBSTRING(obs-ex,1,2) = "  ".
               
                 IF SUBSTRING(obs-ex,1,3) = "EX:" THEN 
                     ASSIGN SUBSTRING(obs-ex,1,3) = "   ".
               
                 ASSIGN obs-ex = TRIM(obs-ex).


                 IF obs-ex <> INPUT FRAME f-pg-sel c-nr-ext THEN NEXT.

                 IF ITEM.ge-codigo <> 46 THEN NEXT.

                 IF ITEM.fm-codigo = "46-24" OR 
                    ITEM.fm-codigo = "46-26" OR
                    ITEM.fm-codigo = "46-25" THEN NEXT.

                 run pi-acompanhar in h-acomp(input "Nota fiscal: " + nota-fiscal.nr-nota-fis).
               
                 FIND FIRST polo-laudo-cliente WHERE
                     polo-laudo-cliente.nome-abrev = nota-fiscal.nome-ab-cli
                     NO-LOCK NO-ERROR.
               
                 IF NOT AVAIL polo-laudo-cliente THEN NEXT.  


    /* Rotina para testar se j  gerado laudo para conferˆncia */

             IF int(tipo-geracao:SCREEN-VALUE IN FRAME f-pg-sel) = 2 THEN DO:  /* oficial */

                 FIND FIRST pol-param-relatorio WHERE
                     pol-param-relatorio.cod-prog-dtsul = "ESCQ051" AND
                     pol-param-relatorio.cod-usuario    = nota-fiscal.nr-nota-fis AND
                     pol-param-relatorio.mes-refer      = int(nota-fiscal.serie)  AND
                     pol-param-relatorio.ano-refer      = int(nota-fiscal.cod-estabel)
                     NO-LOCK NO-ERROR.
                     
                   IF NOT AVAIL pol-param-relatorio THEN  /* eDSON 03-06-2011 PALEATIVO*/ 
                    
                     FIND FIRST pol-param-relatorio WHERE
                     pol-param-relatorio.cod-prog-dtsul = "ESCQ051" AND
                     pol-param-relatorio.cod-usuario    = nota-fiscal.nr-nota-fis AND
                     pol-param-relatorio.mes-refer      = 20  AND
                     pol-param-relatorio.ano-refer      = int(nota-fiscal.cod-estabel)
                     NO-LOCK NO-ERROR.



                 IF NOT AVAIL pol-param-relatorio THEN DO:

                     RUN mens-falta-conferencia.

                     NEXT.

                 END.

             END.
    
    /* Fim da Rotina para testar se j  gerado laudo para conferˆncia */



               IF INT(tipo-geracao:SCREEN-VALUE IN FRAME f-pg-sel) = 2 THEN DO:

                  FIND FIRST am-cq-laudo WHERE
                      am-cq-laudo.cod-estabel = nota-fiscal.cod-estabel AND
                      am-cq-laudo.nr-nota-fis = nota-fiscal.nr-nota-fis AND
                      am-cq-laudo.it-codigo   = it-nota-fisc.it-codigo
                      NO-LOCK NO-ERROR.
                
                
                  IF AVAIL am-cq-laudo and am-cq-laudo.dt-criacao >= 04/01/2009 THEN DO:
                
                      run utp/ut-msgs.p (input "show",
                                         input 135,
                                         input "Laudo e Nota Fiscal").
                
                      NEXT.
                
                  END.

               END.

               FIND FIRST fat-ser-lote OF it-nota-fisc NO-LOCK NO-ERROR.

               IF NOT AVAIL fat-ser-lote THEN NEXT.

               ASSIGN tipo-modelo-jr = 0.

               FIND FIRST polo-laudo-cliente WHERE
                   polo-laudo-cliente.nome-abrev = nota-fiscal.nome-ab-cli
                   NO-LOCK NO-ERROR.

               IF AVAIL polo-laudo-cliente THEN DO:

                   ASSIGN tipo-modelo-jr = int(polo-laudo-cliente.dec-1).

                   IF tipo-modelo-jr = 1 OR 
                      tipo-modelo-jr > 3 THEN ASSIGN tipo-modelo-jr = 0.

               END.


               FIND tt-digita WHERE
                    tt-digita.nome-abrev   = nota-fiscal.nome-ab-cli  AND
                    tt-digita.nr-nota-fisc = nota-fiscal.nr-nota-fis  AND 
                    tt-digita.it-codigo    = it-nota-fisc.it-codigo   AND
                    tt-digita.nr-pedido    = it-nota-fisc.nr-pedido   AND
                    tt-digita.nr-sequencia = it-nota-fisc.nr-seq-fat
                    NO-ERROR.
    
               IF NOT AVAIL tt-digita THEN 
                    CREATE tt-digita.
    
               ASSIGN tt-digita.nome-abrev   = nota-fiscal.nome-ab-cli 
                      tt-digita.nr-nota-fisc = nota-fiscal.nr-nota-fis
                      tt-digita.serie        = nota-fiscal.serie
                      tt-digita.it-codigo    = it-nota-fisc.it-codigo  
                      tt-digita.nr-pedido    = it-nota-fisc.nr-pedido  
                      tt-digita.tipo-modelo  = tipo-modelo-jr
                      tt-digita.nr-sequencia = it-nota-fisc.nr-seq-fat. 
    
                assign v-num-reg-lidos = v-num-reg-lidos + 1.
                run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

         END. /* nota-fiscal */

     END. /* pesq-jr = 1 - Por Nota Fiscal */





END. /* do: */

       assign im-pg-dig:sensitive in frame f-relat = yes
              wh-label-dig:fgcolor                 = ?
              im-pg-sel:sensitive in frame f-relat = yes
              wh-label-sel:fgcolor                 = ?.
       apply "mouse-select-click" to im-pg-dig in frame f-relat.
     /*** habilita **/
    if  num-results('br-digita') > 0 then do:
        if  br-digita:insert-row('after') in frame f-pg-dig then.
    end.
    else
       do transaction:
          open query br-digita for each tt-digita.
          apply 'entry' to tt-digita.nome-abrev in browse br-digita. 
       end.
    
    run pi-finalizar in h-acomp.
    enable bt-retirar with frame f-pg-dig.
    disable bt-inserir bt-alterar bt-salvar bt-recuperar WITH FRAME f-pg-dig.
    ENABLE bt-executar WITH FRAME f-relat.
/*    DISABLE bt-paletes WITH FRAME f-relat.*/
/*  end.    */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-relat 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def var r_rowid as rowid no-undo.

do on error undo, return error
   on stop  undo, return error:     

  {include/i-rpexa.i}
   
    Do:
        
    For Each  tt-digita:
        assign r_rowid = rowid(tt-digita).
             
    End.
  End. 

  if  input frame f-pg-imp rs-destino = 2 then do:
      run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
      if  return-value = "nok" then do:
          run utp/ut-msgs.p (input "show",
                             input 73,
                             input "").
          apply 'mouse-select-click' to im-pg-imp in frame f-relat.
          apply 'entry' to c-arquivo in frame f-pg-imp.                   
          return error.
      end.
  end.
        
  /* Coloque aqui as valida‡äes das outras p ginas, lembrando que elas
     devem apresentar uma mensagem de erro cadastrada, posicionar na p gina 
     com problemas e colocar o focus no campo com problemas             */    
  /*     
    if input frame f-pg-sel c-cod-estabel-fim < input frame f-pg-sel c-cod-estabel-ini THEN DO:
       message "Estabelecimento Final Menor que Inicial" 
           view-as alert-box ERROR TITLE "Atencao !".
       apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
       apply "ENTRY":U to c-cod-estabel-ini in frame f-pg-sel.
       return error.
    end.
    
    if input frame f-pg-sel c-nr-pedcli-fim < input frame f-pg-sel c-nr-pedcli-ini then do:
       message "Pedido Final Menor que Inicial" 
           view-as alert-box ERROR TITLE "Atencao !".
       apply "mouse-select-click" to im-pg-sel in frame f-relat.
       apply 'entry' to c-nr-pedcli-ini in frame f-pg-sel.  
       return error.
    end.    
  */
/*  END. */

  

  create tt-param.
  assign tt-param.usuario              = c-seg-usuario
         tt-param.destino              = input frame f-pg-imp rs-destino
         tt-param.data-exec            = today
         tt-param.hora-exec            = time. /*
         tt-param.:classifica           = input frame f-pg-cla rs-classif
         tt-param.desc-classifica      = entry((tt-param.classifica - 1) * 2 + 1, 
                                         rs-classif:radio-buttons in frame f-pg-cla). */
      if tt-param.destino = 1 
      then assign tt-param.arquivo = "".
      else if  tt-param.destino = 2 
           then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
           else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".

   ASSIGN tt-param.c-cod-estabel-ini    = INPUT FRAME f-pg-sel c-cod-estabel-ini
          tt-param.c-cdd-embarq-ini    = INPUT FRAME f-pg-sel c-cdd-embarq-ini 
          tt-param.c-cdd-embarq-fim    = INPUT FRAME f-pg-sel c-cdd-embarq-fim 
          tt-param.pesq-jr              = INPUT FRAME f-pg-sel pesq-jr 
          tt-param.tipo-geracao         = INPUT FRAME f-pg-sel tipo-geracao
          tt-param.tb-v-temp            = INPUT FRAME f-pg-sel tb-v-temp
          tt-param.i-modelo             = 0. /* INPUT FRAME f-pg-sel i-modelo */ 

    {include/i-rpexb.i}

    if  session:set-wait-state("general":U) then.
    
    {include/i-rprun.i cqp/escq0051rp.p} 
  
    {include/i-rpexc.i}

    if  session:set-wait-state("":U) then.
    
    {include/i-rptrm.i}
/*
    FOR EACH tt-digita.
        DELETE tt-digita.
    END.


    OPEN QUERY br-digita FOR EACH tt-digita.
*/
    apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
    ASSIGN bt-executar:SENSITIVE = NO.
    apply "ENTRY":U to c-cod-estabel-ini in frame f-pg-sel. 
    RETURN.
    
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-leave-pallete w-relat 
PROCEDURE pi-leave-pallete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* if tt-digita.b_it-codigo:screen-value in browse br-digita <> "0" then do:
   end. */  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{include/i-rptrp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-relat  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-digita"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-relat 
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

PROCEDURE mens-falta-conferencia.
    
       /* Dialog-box com Mensagem de Erro */
      
       DEFINE BUTTON db-bt-cancel AUTO-END-KEY 
            LABEL "&Fechar" 
            SIZE 10 BY 1
            BGCOLOR 8.
      
       DEFINE RECTANGLE db-rt-botoes
            EDGE-PIXELS 2 GRAPHIC-EDGE  
            SIZE 58 BY 1.42
            BGCOLOR 7.  
      
       DEFINE VARIABLE c-mensagem AS CHARACTER FORMAT "X(60)" NO-UNDO.
      
       ASSIGN c-mensagem = "NÆo foi emitido laudo p/conferˆncia da NF: " +
           string(nota-fiscal.nr-nota-fis).
      
       DEFINE RECTANGLE db-rect-1
        EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
        SIZE 59 BY 3.30.
      
       DEFINE FRAME db-frame-1
      
           c-mensagem NO-LABEL 
              at ROW 3 col 10 
      
           db-rect-1 AT ROW 1.9 COL 5
      
           db-bt-cancel      AT ROW 7.3 COL 23             
           db-rt-botoes      AT ROW 7.0 COL 1
           SPACE(0.28)
           WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
                THREE-D SCROLLABLE TITLE "Caixa de dialogo" FONT 1
                DEFAULT-BUTTON db-bt-cancel CANCEL-BUTTON db-bt-cancel.
      
      
       DISPLAY c-mensagem WITH FRAME db-frame-1.
      
       ENABLE db-bt-cancel 
           WITH FRAME db-frame-1. 
      
       WAIT-FOR "GO":U OF FRAME db-frame-1.
      
       /* Fim do Dialog-box com Mensagem de Error */

END PROCEDURE.






