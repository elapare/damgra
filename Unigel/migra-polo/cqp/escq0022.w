&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME

&Scoped-define WINDOW-NAME w-relat

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 

/***************************************************************************
 **Programa.: escq0022.W 
 **Autor....: JOS ROBERTO REIS CAMPOS
 **Objetivo.: Revalida‡Æo de Pallets
 **Data.....: 10/04/2005
 **Otimiza‡Æo de Performance em 06/03/2007
 ***************************************************************************/

{include/i-prgvrs.i escq0022 2.04.00.000}

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
    field c-cod-estabel-fim    like ped-venda.cod-estabel
    FIELD c-it-codigo-ini      LIKE saldo-estoq.it-codigo
    FIELD c-it-codigo-fim      LIKE saldo-estoq.it-codigo
    FIELD c-lote-ini           LIKE saldo-estoq.lote
    FIELD c-lote-fim           LIKE saldo-estoq.lote
    FIELD c-nr-pedido-ini      LIKE pallet.nr-pedido
    FIELD c-nr-pedido-fim      LIKE pallet.nr-pedido
    FIELD c-vali-ini           LIKE saldo-estoq.dt-vali-lote
    FIELD c-vali-fim           LIKE saldo-estoq.dt-vali-lote
    FIELD c-data-vali          LIKE saldo-estoq.dt-vali-lote
    FIELD pesq-jr              AS INT  
    FIELD l-pl-excel           AS LOG 
.

DEFINE TEMP-TABLE tt-digita no-undo
    FIELD lot-lote                 LIKE saldo-estoq.lote         FORMAT "x(10)" LABEL "Lote"
    FIELD lot-it-codigo            LIKE saldo-estoq.it-codigo    FORMAT "x(16)" LABEL "Filme"
    FIELD lot-dt-vali-lote         LIKE saldo-estoq.dt-vali-lote FORMAT "99/99/9999" LABEL "Data"
    FIELD lot-cod-estabel          LIKE saldo-estoq.cod-estabel  FORMAT "x(3)"  LABEL "Est"
    FIELD lot-cod-depos            LIKE saldo-estoq.cod-depos    FORMAT "x(3)"  LABEL "Dep"
    FIELD lot-cod-localiz          LIKE saldo-estoq.cod-localiz  FORMAT "x(10)" LABEL "Localiz."
    FIELD lot-cod-refer            LIKE saldo-estoq.cod-refer    FORMAT "x(8)"  LABEL "Refer."
    FIELD lot-quantidade           AS DEC                        FORMAT "->>>>9.9999" LABEL "Qtde"
    FIELD lot-revalidado           AS CHAR                       FORMAT "x(5)"  LABEL "Reval"       
    FIELD lot-nome-abrev           AS CHAR                       FORMAT "x(12)" LABEL "Emitente"
    INDEX chave IS PRIMARY UNIQUE lot-cod-estabel
                                  lot-cod-depos
                                  lot-dt-vali-lote
                                  lot-cod-localiz
                                  lot-lote
                                  lot-it-codigo
                                  lot-cod-refer.

DEFINE TEMP-TABLE tt-notas NO-UNDO
    FIELD nro-docto    LIKE saldo-terc.nro-docto
    FIELD sequencia    LIKE saldo-terc.sequencia
    FIELD saldo        LIKE saldo-terc.quantidade
    FIELD cod-emitente LIKE saldo-terc.cod-emitente
    FIELD serie        LIKE saldo-terc.serie
    FIELD cod-estabel  LIKE saldo-terc.cod-estabel
    FIELD it-codigo    LIKE saldo-terc.it-codigo
    INDEX chave-notas IS PRIMARY UNIQUE nro-docto
                                        sequencia.

DEFINE TEMP-TABLE tt-lotes NO-UNDO
    FIELD nro-docto    LIKE saldo-terc.nro-docto
    FIELD sequencia    LIKE saldo-terc.sequencia
    FIELD lote         LIKE saldo-terc.lote
    FIELD remessa      LIKE saldo-terc.quantidade
    FIELD retorno      LIKE saldo-terc.quantidade
    FIELD saldo        LIKE saldo-terc.quantidade
    FIELD cod-estabel  LIKE saldo-terc.cod-estabel
    FIELD cod-emitente LIKE saldo-terc.cod-emitente
    FIELD it-codigo    LIKE saldo-terc.it-codigo
    INDEX chave-lotes IS PRIMARY UNIQUE nro-docto
                                        sequencia
                                        lote.


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
DEFINE VARIABLE c-grupo-trans AS CHARACTER  NO-UNDO.

DEFINE VARIABLE c-erro AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-depos AS CHARACTER  NO-UNDO.

DEFINE VARIABLE l-pl-excel AS LOGICAL    NO-UNDO.

/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 
DEFINE VARIABLE c-conta-contabil-jr LIKE conta-contab.conta-contabil NO-UNDO.

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

&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.lot-marca tt-digita.lot-it-codigo tt-digita.lot-lote tt-digita.lot-cod-depos tt-digita.lot-cod-localiz tt-digita.lot-quantidade tt-digita.lot-dt-vali-lote tt-digita.lot-revalidado tt-digita.lot-cod-estabel tt-digita.lot-cod-refer   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.lot-it-codigo tt-digita.lot-lote tt-digita.lot-cod-depos tt-digita.lot-cod-localiz tt-digita.lot-quantidade tt-digita.lot-dt-vali-lote tt-digita.lot-revalidado tt-digita.lot-cod-estabel tt-digita.lot-cod-refer   
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

DEFINE BUTTON bt-gera-excel 
     LABEL "Gera Excel" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-retirar 
     LABEL "Retirar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-salvar 
     LABEL "Revalidar Lotes" 
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
     SIZE 71.29 BY 4.65.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71.29 BY 3.15.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71.29 BY 2.15.

DEFINE VARIABLE texto-filme AS CHARACTER FORMAT "X(12)":U INITIAL "Filme" 
      VIEW-AS TEXT 
     SIZE 15.86 BY .45 NO-UNDO.

DEFINE VARIABLE texto-pallet AS CHARACTER FORMAT "X(12)":U INITIAL "Pallet" 
      VIEW-AS TEXT 
     SIZE 15.86 BY .45 NO-UNDO.

DEFINE VARIABLE texto-vali AS CHARACTER FORMAT "X(14)":U INITIAL "Nova Validade" 
      VIEW-AS TEXT 
     SIZE 15.86 BY .45 NO-UNDO.

DEFINE VARIABLE pesq-jr AS INTEGER INITIAL 1
    VIEW-AS RADIO-SET VERTICAL
    RADIO-BUTTONS 
    "Estoque na F brica", 1,
    "Estoque em Terceiros", 2
    SIZE 35 BY 1.4 NO-UNDO.

DEFINE VARIABLE c-data-vali AS date label "Nova Validade" FORMAT "99/99/9999"
     INITIAL TODAY 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     TOOLTIP "Informe a Nova Validade" NO-UNDO.  

DEFINE VARIABLE c-vali-ini AS date label "Data de Validade" FORMAT "99/99/9999"
     INITIAL TODAY  
     VIEW-AS FILL-IN 
     SIZE 17 BY .88
     TOOLTIP "Informe a Data de Validade" NO-UNDO.  

DEFINE VARIABLE c-vali-fim AS date FORMAT "99/99/9999"
     INITIAL TODAY 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88
     TOOLTIP "Informe a Data de Validade" NO-UNDO.  

DEFINE VARIABLE c-cod-estabel-ini AS CHAR label "Estabelecimento" FORMAT "x(3)":U 
      /*solic-318*/ 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88
     TOOLTIP "Informe C¢digo do Estabelecimento " NO-UNDO.

DEFINE VARIABLE c-cod-estabel-fim AS char FORMAT "x(3)"
      /*solic-318*/ 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88
     TOOLTIP "Informe o Cod. do Estabelecimento " NO-UNDO.

DEFINE VARIABLE c-it-codigo-ini AS char label "Filme" FORMAT "x(16)":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88
     TOOLTIP "Informe o Cod. do Filme " NO-UNDO.

DEFINE VARIABLE c-it-codigo-fim AS char FORMAT "x(16)"
     INITIAL "ZZZZZZZZZZZZZZZZ"
     VIEW-AS FILL-IN 
     SIZE 17 BY .88
     TOOLTIP "Informe o Cod. do Filme " NO-UNDO.

DEFINE VARIABLE c-lote-ini AS char label "Lote" FORMAT "x(16)":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88
     TOOLTIP "Informe o Lote" NO-UNDO.

DEFINE VARIABLE c-lote-fim AS char FORMAT "x(16)"
     INITIAL "ZZZZZZZZZZZZZZZZ"
     VIEW-AS FILL-IN 
     SIZE 17 BY .88
     TOOLTIP "Informe o Lote" NO-UNDO.

DEFINE VARIABLE c-nr-pedido-ini AS int label "Pedido" FORMAT ">>>>>>9":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88
     TOOLTIP "Informe o Pedido de Venda" NO-UNDO.

DEFINE VARIABLE c-nr-pedido-fim AS int FORMAT ">>>>>>9"
     INITIAL "9999999"
     VIEW-AS FILL-IN 
     SIZE 17 BY .88
     TOOLTIP "Informe o Lote" NO-UNDO.

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
     LABEL "Selecione" 
     SIZE 10 BY 1.

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
  QUERY br-digita DISPLAY
        tt-digita.lot-it-codigo
        tt-digita.lot-lote
        tt-digita.lot-cod-depos
        tt-digita.lot-cod-localiz
        tt-digita.lot-quantidade
        tt-digita.lot-dt-vali-lote
        tt-digita.lot-revalidado 
        tt-digita.lot-cod-estabel
        tt-digita.lot-cod-refer   
  ENABLE
    tt-digita.lot-lote 
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 85 BY 15
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
         "Monta Tabela de Paletes"

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

   c-cod-estabel-ini 
     at row 1.75 col 18 colon-aligned
     font 1

   c-cod-estabel-fim 
     NO-LABEL
     at row 1.75 col 50 colon-aligned
     font 1

   c-it-codigo-ini 
     at row 2.95 col 18 colon-aligned
     font 1

   c-it-codigo-fim 
     NO-LABEL
     at row 2.95 col 50 colon-aligned
     font 1

   c-lote-ini 
     at row 4.55 col 18 colon-aligned
     font 1

   c-lote-fim 
     NO-LABEL
     at row 4.55 col 50 colon-aligned
     font 1

   c-nr-pedido-ini 
     at row 5.50 col 18 colon-aligned
     font 1

   c-nr-pedido-fim 
     NO-LABEL
     at row 5.50 col 50 colon-aligned
     font 1

    c-vali-ini
     at row 6.50 col 18 colon-aligned
     font 1

    c-vali-fim 
      NO-LABEL
      at row 6.50 col 50 colon-aligned
      font 1

    pesq-jr 
      at row 7.55 col 12 LABEL "Op‡äes" 
      font 1

   c-data-vali
    at row 9.90 col 18 colon-aligned
    font 1
   
   texto-filme  AT ROW 1    COL 2.00 COLON-ALIGNED NO-LABEL
   texto-pallet AT ROW 4.35 COL 2.00 COLON-ALIGNED NO-LABEL
   texto-vali   AT ROW 9.15 COL 2.00 COLON-ALIGNED NO-LABEL

   RECT-19  AT ROW 04.45 COL 02       
   RECT-20  AT ROW 01.15 COL 02
   RECT-29  AT ROW 09.30 COL 02
   image-86 AT ROW 02.95 COL 40       
   image-87 AT ROW 02.95 COL 46
   IMAGE-3  AT ROW 04.55 COL 40
   IMAGE-4  AT ROW 04.55 COL 46
   IMAGE-1  AT ROW 01.75 COL 40
   IMAGE-2  AT ROW 01.75 COL 46
   IMAGE-55 AT ROW 05.50 COL 40
   IMAGE-56 AT ROW 05.50 COL 46
   image-88 AT ROW 06.50 COL 40       
   image-89 AT ROW 06.50 COL 46

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
     br-digita  AT ROW 1  COL 1
     bt-inserir AT ROW 16.5 COL 46
     bt-alterar AT ROW 16.5 COL 16
     bt-retirar AT ROW 16.5 COL 31
     bt-salvar  AT ROW 16.5 COL 1
     bt-gera-excel AT ROW 16.5 COL 61
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.31
         SIZE 90 BY 17.65.


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
         HEIGHT             = 20
         WIDTH              = 105.14
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
   NO-DISPLAY NO-ENABLE ALIGN-L */                
                        
                        


ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu‡Æo".

ASSIGN 
       texto-filme:PRIVATE-DATA IN FRAME f-pg-sel     = 
                "Produto".

ASSIGN 
       texto-pallet:PRIVATE-DATA IN FRAME f-pg-sel     = 
                "Pallet".

ASSIGN 
       texto-Vali:PRIVATE-DATA IN FRAME f-pg-sel     = 
                "Nova Validade".

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
ON WINDOW-CLOSE OF w-relat 
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
        display
            tt-digita.lot-it-codigo
            tt-digita.lot-lote
            tt-digita.lot-cod-depos
            tt-digita.lot-cod-localiz
            tt-digita.lot-quantidade
            tt-digita.lot-dt-vali-lote
            tt-digita.lot-revalidado 
            tt-digita.lot-cod-estabel
            tt-digita.lot-cod-refer   
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
  apply 'entry' to bt-inserir in frame f-pg-dig.
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
    /*  aqui que a grava‡Æo da linha da temp-table ² efetivada.
       Por‚m as valida‡äes dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment rio */


    if br-digita:NEW-ROW in frame f-pg-dig then 
    do transaction on error undo, return no-apply:
        create tt-digita.
        assign INPUT BROWSE br-digita tt-digita.lot-it-codigo   
               input browse br-digita tt-digita.lot-lote        
               input browse br-digita tt-digita.lot-cod-depos   
               input browse br-digita tt-digita.lot-cod-localiz 
               input browse br-digita tt-digita.lot-quantidade  
               input browse br-digita tt-digita.lot-dt-vali-lote
               input browse br-digita tt-digita.lot-revalidado 
               input browse br-digita tt-digita.lot-cod-estabel 
               INPUT BROWSE br-digita tt-digita.lot-cod-refer.   
            
            br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    end.
    else do transaction on error undo, return no-apply:
        
            assign INPUT BROWSE br-digita tt-digita.lot-it-codigo   
                   input browse br-digita tt-digita.lot-lote        
                   input browse br-digita tt-digita.lot-cod-depos   
                   input browse br-digita tt-digita.lot-cod-localiz 
                   input browse br-digita tt-digita.lot-quantidade  
                   input browse br-digita tt-digita.lot-dt-vali-lote
                   input browse br-digita tt-digita.lot-revalidado 
                   input browse br-digita tt-digita.lot-cod-estabel 
                   INPUT BROWSE br-digita tt-digita.lot-cod-refer.   

      
       if num-results("br-digita") > 0 then
      
            display
                tt-digita.lot-it-codigo   
                tt-digita.lot-lote        
                tt-digita.lot-cod-depos   
                tt-digita.lot-cod-localiz 
                tt-digita.lot-quantidade  
                tt-digita.lot-dt-vali-lote
                tt-digita.lot-revalidado 
                tt-digita.lot-cod-estabel 
                tt-digita.lot-cod-refer   
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
    get current br-digita.
    assign input browse br-digita tt-digita.lot-lote.       
    apply 'entry' to tt-digita.lot-lote in browse br-digita. 

end.



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

    ASSIGN l-pl-excel = NO.


    CLOSE QUERY br-digita.
    FOR EACH tt-digita.
        DELETE tt-digita.
    END.
    
    FIND FIRST estabelec WHERE
        estabelec.cod-estabel = input frame f-pg-sel c-cod-estabel-ini
        NO-LOCK NO-ERROR.
    IF NOT AVAIL estabelec THEN DO:
        MESSAGE "Estabelecimento Errado"
            view-as alert-box ERROR TITLE "Aten‡Æo !".
        apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
        apply "ENTRY":U to c-cod-estabel-ini in frame f-pg-sel.
        RETURN.          
    END.

    
    FIND FIRST estabelec WHERE
        estabelec.cod-estabel = input frame f-pg-sel c-cod-estabel-fim
        NO-LOCK NO-ERROR.
    IF NOT AVAIL estabelec THEN DO:
        MESSAGE "Estabelecimento Errado"
            view-as alert-box ERROR TITLE "Aten‡Æo !".
        apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
        apply "ENTRY":U to c-cod-estabel-fim in frame f-pg-sel.
        RETURN.          
    END.

    FIND FIRST pol-param-estab WHERE
        pol-param-estab.cod-estabel = input frame f-pg-sel c-cod-estabel-ini
        NO-LOCK NO-ERROR.
    IF NOT AVAIL pol-param-estab THEN DO:
        MESSAGE "Estabelecimento Errado"
            view-as alert-box ERROR TITLE "Aten‡Æo !".
        apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
        apply "ENTRY":U to c-cod-estabel-ini in frame f-pg-sel.
        RETURN.
    END. 
    

    FIND FIRST pol-param-estab WHERE
        pol-param-estab.cod-estabel = input frame f-pg-sel c-cod-estabel-fim
        NO-LOCK NO-ERROR.
    IF NOT AVAIL pol-param-estab THEN DO:
        MESSAGE "Estabelecimento Errado"
            view-as alert-box ERROR TITLE "Aten‡Æo !".
        apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
        apply "ENTRY":U to c-cod-estabel-fim in frame f-pg-sel.
        RETURN.
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

       apply "mouse-select-click" to im-pg-sel in frame f-relat.
       apply "ENTRY":U to c-cod-estabel-ini in frame f-pg-sel.
                      
   end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-inserir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inserir w-relat
ON CHOOSE OF bt-inserir IN FRAME f-pg-dig /* Inserir */
DO:

    get current br-digita.
    assign input browse br-digita tt-digita.lot-lote.       
    apply 'entry' to tt-digita.lot-lote in browse br-digita. 


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-gera-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-gera-excel w-relat
ON CHOOSE OF bt-gera-excel IN FRAME f-pg-dig /* Excel */
DO:
    
   do  on error undo, return no-apply:

       ASSIGN l-pl-excel = YES.
       
       run pi-executar.

       apply "mouse-select-click" to im-pg-sel in frame f-relat.
       apply "ENTRY":U to c-cod-estabel-ini in frame f-pg-sel.
                      
   end.



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
ON CHOOSE OF bt-salvar IN FRAME f-pg-dig /* Revalidar */
DO:
    
   do  on error undo, return no-apply:
       
       run pi-executar.

       apply "mouse-select-click" to im-pg-sel in frame f-relat.
       apply "ENTRY":U to c-cod-estabel-ini in frame f-pg-sel.
                      
   end.


   
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

on LEAVE OF tt-digita.lot-lote in browse br-digita do:
   run pi-leave-pallete.
end.


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "escq0022" "2.06.00.001"}

/* inicializa‡äes do template de relat¢rio */
{include/i-rpini.i}

ASSIGN rs-destino = 4.
ASSIGN c-data-vali = c-data-vali + 180.

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
    c-cod-estabel-ini = STRING({cdp\poloestab.i 422}). /*solic-318*/ 
    c-cod-estabel-fim = STRING({cdp\poloestab.i 422}). /*solic-318*/ 
    
    RUN enable_UI.
    
    assign im-pg-dig:sensitive in frame f-relat = no
           wh-label-dig:fgcolor                 = 7.

    assign im-pg-cla:sensitive in frame f-relat = no
           wh-label-cla:fgcolor                 = 7.
                       
    assign im-pg-imp:sensitive in frame f-relat = no
           wh-label-imp:fgcolor                 = 7.

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
      c-cod-estabel-ini     
      c-cod-estabel-fim     
      c-it-codigo-ini       
      c-it-codigo-fim       
      c-lote-ini         
      c-lote-fim         
      c-nr-pedido-ini         
      c-nr-pedido-fim 
      c-vali-ini        
      c-vali-fim        
      pesq-jr
      c-data-vali        
   WITH FRAME f-pg-sel IN WINDOW w-relat.

   ENABLE
       c-cod-estabel-ini     
       c-cod-estabel-fim     
       c-it-codigo-ini       
       c-it-codigo-fim       
       c-lote-ini         
       c-lote-fim         
       c-nr-pedido-ini         
       c-nr-pedido-fim
       c-vali-ini        
       c-vali-fim        
       pesq-jr
       c-data-vali        
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

  ENABLE br-digita bt-salvar bt-gera-excel 
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

    assign v-num-reg-lidos = 0.

Do:
     /** Montando a tabela para tt-digita ***/
     run utp/ut-acomp.p persistent set h-acomp.

     {utp/ut-liter.i Gerando_movimento * I}

     run pi-inicializar in h-acomp (input "Selecionando os Lotes ...Aguarde").  
     

  IF INT (input frame f-pg-sel pesq-jr) =  1 THEN DO:

    /* for EACH grup-estoque WHERE
         grup-estoq.ge-codigo >= 55 NO-LOCK,*/

      for EACH grup-estoque WHERE
         grup-estoq.ge-codigo >= 45 AND  grup-estoq.ge-codigo <= 80 NO-LOCK,
         
        EACH ITEM WHERE
        ITEM.ge-codigo = grup-estoque.ge-codigo and
        item.it-codigo >= INPUT FRAME f-pg-sel c-it-codigo-ini AND
        item.it-codigo <= INPUT FRAME f-pg-sel c-it-codigo-fim
        USE-INDEX grupo no-lock,

        EACH estabelec where
             estabelec.cod-estabel >= INPUT FRAME f-pg-sel c-cod-estabel-ini AND
             estabelec.cod-estabel <= INPUT FRAME f-pg-sel c-cod-estabel-fim             
             NO-LOCK,

        EACH  ext-saldo-estoq NO-LOCK WHERE
         ext-saldo-estoq.it-codigo   =  item.it-codigo            AND
         ext-saldo-estoq.cod-estabel =  estabelec.cod-estabel     AND
         ext-saldo-estoq.log-saldo   =  YES                       AND 
         ext-saldo-estoq.lote        >= INPUT FRAME f-pg-sel c-lote-ini AND 
         ext-saldo-estoq.lote        <= INPUT FRAME f-pg-sel c-lote-fim  
        USE-INDEX saldo,
         
         each pallet no-lock
             where pallet.cod-estabel  = ext-saldo-estoq.cod-estabel AND 
                   pallet.it-codigo    = ext-saldo-estoq.it-codigo AND
                   pallet.nr-pallet    = ext-saldo-estoq.lote: 


         IF pallet.nr-pedido < INPUT FRAME f-pg-sel c-nr-pedido-ini OR 
            pallet.nr-pedido > INPUT FRAME f-pg-sel c-nr-pedido-fim THEN NEXT.
 
         assign v-num-reg-lidos = v-num-reg-lidos + 1.
         run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

         FOR EACH saldo-estoq NO-LOCK WHERE
                      saldo-estoq.cod-estabel  = ext-saldo-estoq.cod-estabel      AND
                      saldo-estoq.cod-depos    = ext-saldo-estoq.cod-depos        AND
                      saldo-estoq.cod-localiz  = ext-saldo-estoq.cod-localiz      AND
                      saldo-estoq.lote         = ext-saldo-estoq.lote             AND
                      saldo-estoq.it-codigo    = ext-saldo-estoq.it-codigo        AND
                      saldo-estoq.cod-refer    = ext-saldo-estoq.cod-refer        AND       
                    /*  saldo-estoq.dt-vali-lote  < TODAY AND */
                      saldo-estoq.dt-vali-lote >= INPUT FRAME f-pg-sel c-vali-ini AND
                      saldo-estoq.dt-vali-lote <= INPUT FRAME f-pg-sel c-vali-fim 
                      USE-INDEX estabel-dep :

             
             FIND tt-digita WHERE
                  tt-digita.lot-cod-estabel    = saldo-estoq.cod-estabel AND
                  tt-digita.lot-cod-depos      = saldo-estoq.cod-depos   AND
                  tt-digita.lot-dt-vali-lote   = saldo-estoq.dt-vali-lote AND
                  tt-digita.lot-cod-localiz    = saldo-estoq.cod-localiz AND
                  tt-digita.lot-lote           = saldo-estoq.lote        AND
                  tt-digita.lot-it-codigo      = saldo-estoq.it-codigo   AND
                  tt-digita.lot-cod-refer      = saldo-estoq.cod-refer
                  NO-ERROR.

             IF NOT AVAIL tt-digita THEN DO:
                  CREATE tt-digita.

                  ASSIGN tt-digita.lot-cod-estabel    = saldo-estoq.cod-estabel 
                         tt-digita.lot-cod-depos      = saldo-estoq.cod-depos   
                         tt-digita.lot-dt-vali-lote   = saldo-estoq.dt-vali-lote
                         tt-digita.lot-cod-localiz    = saldo-estoq.cod-localiz 
                         tt-digita.lot-lote           = saldo-estoq.lote        
                         tt-digita.lot-it-codigo      = saldo-estoq.it-codigo   
                         tt-digita.lot-cod-refer      = saldo-estoq.cod-refer.

                  IF saldo-estoq.cod-estabel = STRING({cdp\poloestab.i 423}) THEN ASSIGN tt-digita.lot-nome-abrev = "VGA".  /*solic-318*/ 
                  IF saldo-estoq.cod-estabel = STRING({cdp\poloestab.i 422}) THEN ASSIGN tt-digita.lot-nome-abrev = "MTN".  /*solic-318*/ 
                  IF saldo-estoq.cod-estabel > STRING({cdp\poloestab.i 422}) THEN ASSIGN tt-digita.lot-nome-abrev = "SP".   /*solic-318*/ 

             END.

             ASSIGN tt-digita.lot-quantidade    = saldo-estoq.qtidade-atu -
                                                  (saldo-estoq.qt-alocada   +
                                                   saldo-estoq.qt-aloc-prod + 
                                                   saldo-estoq.qt-aloc-ped).
              
              IF (saldo-estoq.dt-vali-lote - pallet.data-pallet) > 190 THEN
                  ASSIGN tt-digita.lot-revalidado = "Sim".
              ELSE
                  ASSIGN tt-digita.lot-revalidado = "". 

         END.
             
     END.
      
  END.

/* Rotina para encontrar paletes em Terceiros */

  IF INT (input frame f-pg-sel pesq-jr) =  2 THEN DO:
      
      FOR EACH tt-notas.
          DELETE tt-notas.
      END.

      FOR EACH tt-lotes.
          DELETE tt-lotes.
      END.



      /* Rotina para Obter as Remessas de Lotes p/terceiros */


    /* for EACH grup-estoque WHERE
         grup-estoq.ge-codigo >= 55 NO-LOCK,*/


     for EACH grup-estoque WHERE
         grup-estoq.ge-codigo >= 45 AND  grup-estoq.ge-codigo <= 80 NO-LOCK,
    


         
        EACH ITEM WHERE
        ITEM.ge-codigo = grup-estoque.ge-codigo and
        item.it-codigo >= INPUT FRAME f-pg-sel c-it-codigo-ini AND
        item.it-codigo <= INPUT FRAME f-pg-sel c-it-codigo-fim
        USE-INDEX grupo no-lock,

        EACH estabelec where
             estabelec.cod-estabel >= INPUT FRAME f-pg-sel c-cod-estabel-ini AND
             estabelec.cod-estabel <= INPUT FRAME f-pg-sel c-cod-estabel-fim             
             NO-LOCK,

        each saldo-terc no-lock
            where saldo-terc.cod-estabel  = estabelec.cod-estabel    AND    
                  saldo-terc.it-codigo    = item.it-codigo           AND  
                  saldo-terc.quantidade > 0 .

          FIND FIRST tt-notas WHERE
              tt-notas.nro-docto = saldo-terc.nro-docto AND
              tt-notas.sequencia = saldo-terc.sequencia
              NO-LOCK NO-ERROR.

          IF AVAIL tt-notas THEN NEXT.

          CREATE tt-notas.
          ASSIGN tt-notas.nro-docto    = saldo-terc.nro-docto 
                 tt-notas.sequencia    = saldo-terc.sequencia 
                 tt-notas.cod-emitente = saldo-terc.cod-emitente 
                 tt-notas.serie        = saldo-terc.serie 
                 tt-notas.cod-estabel  = saldo-terc.cod-estabel 
                 tt-notas.saldo        = saldo-terc.quantidade
                 tt-notas.it-codigo    = saldo-terc.it-codigo.

          /* rotina para encontrar os lotes enviados */
          assign v-num-reg-lidos = v-num-reg-lidos + 1.
          run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

          FOR EACH fat-ser-lote NO-LOCK WHERE
              fat-ser-lote.cod-estabel = saldo-terc.cod-estabel AND
              fat-ser-lote.serie       = saldo-terc.serie-docto AND
              fat-ser-lote.nr-nota-fis = saldo-terc.nro-docto   AND
              fat-ser-lote.nr-seq-fat  = saldo-terc.sequencia   AND 
              fat-ser-lote.it-codigo   = saldo-terc.it-codigo AND 
              fat-ser-lote.nr-serlote  >= INPUT FRAME f-pg-sel c-lote-ini AND
              fat-ser-lote.nr-serlote  <= INPUT FRAME f-pg-sel c-lote-fim. 

                                         
              FIND FIRST tt-lotes WHERE
                  tt-lotes.nro-docto = saldo-terc.nro-docto AND
                  tt-lotes.sequencia = saldo-terc.sequencia AND
                  tt-lotes.lote      = fat-ser-lote.nr-serlote
                  NO-LOCK NO-ERROR.

              IF NOT AVAIL tt-lotes THEN 
                 CREATE tt-lotes.

              ASSIGN tt-lotes.nro-docto    = saldo-terc.nro-docto 
                     tt-lotes.sequencia    = saldo-terc.sequencia 
                     tt-lotes.lote         = fat-ser-lote.nr-serlote
                     tt-lotes.cod-estabel  = saldo-terc.cod-estabel
                     tt-lotes.cod-emitente = saldo-terc.cod-emitente
                     tt-lotes.it-codigo    = fat-ser-lote.it-codigo.

              ASSIGN tt-lotes.remessa   = tt-lotes.remessa + fat-ser-lote.qt-baixada [1]
                     tt-lotes.saldo     = tt-lotes.remessa - tt-lotes.retorno.          
                      
          END.

      END.

      /* Rotina para Obter os Retornos de Lotes em terceiros */

      FOR EACH tt-notas NO-LOCK.

          assign v-num-reg-lidos = v-num-reg-lidos + 1.
          run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

          FOR EACH componente NO-LOCK WHERE
              componente.cod-emitente = tt-notas.cod-emitente AND
              componente.serie-comp   = tt-notas.serie        AND
              componente.nro-comp     = tt-notas.nro-docto    AND
              componente.seq-comp     = tt-notas.sequencia .
 
              FOR EACH rat-lote NO-LOCK WHERE
                  rat-lote.serie-docto  = componente.serie-docto AND
                  rat-lote.nro-docto    = componente.nro-docto   AND
                  rat-lote.cod-emitente = tt-notas.cod-emitente  AND
                  rat-lote.sequencia    = componente.sequencia   AND
                  rat-lote.nat-oper     = componente.nat-oper    AND
                  rat-lote.lote  >= INPUT FRAME f-pg-sel c-lote-ini AND
                  rat-lote.lote  <= INPUT FRAME f-pg-sel c-lote-fim.

                  FIND FIRST tt-lotes WHERE
                      tt-lotes.nro-docto = tt-notas.nro-docto AND
                      tt-lotes.sequencia = tt-notas.sequencia AND
                      tt-lotes.lote      = rat-lote.lote
                      NO-LOCK NO-ERROR.

                  IF NOT AVAIL tt-lotes THEN DO:
                      CREATE tt-lotes.
                      ASSIGN tt-lotes.nro-docto = tt-notas.nro-docto 
                             tt-lotes.sequencia = tt-notas.sequencia 
                             tt-lotes.lote      = rat-lote.lote.
                  END.

                  ASSIGN tt-lotes.cod-estabel  = tt-notas.cod-estabel
                         tt-lotes.cod-emitente = tt-notas.cod-emitente
                         tt-lotes.it-codigo    = rat-lote.it-codigo.

                  ASSIGN tt-lotes.retorno   = tt-lotes.retorno + rat-lote.quantidade
                         tt-lotes.saldo     = tt-lotes.remessa - tt-lotes.retorno.          

              END.
          END.
      END. 

   FOR EACH tt-lotes NO-LOCK.
     IF tt-lotes.saldo = 0 THEN NEXT.

     FIND first pallet 
             where pallet.cod-estabel   = tt-lotes.cod-estabel AND 
                   pallet.it-codigo     = tt-lotes.it-codigo   AND 
                   pallet.nr-pallet     = tt-lotes.lote
         NO-LOCK NO-ERROR.

     IF NOT AVAIL pallet THEN NEXT.
 
     IF pallet.nr-pedido < INPUT FRAME f-pg-sel c-nr-pedido-ini OR 
        pallet.nr-pedido > INPUT FRAME f-pg-sel c-nr-pedido-fim THEN NEXT.
 
     IF pallet.nr-pallet < INPUT FRAME f-pg-sel c-lote-ini OR 
        pallet.nr-pallet > INPUT FRAME f-pg-sel c-lote-fim THEN NEXT.
 
     assign v-num-reg-lidos = v-num-reg-lidos + 1.
     run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

     FIND first saldo-estoq 
         where saldo-estoq.it-codigo     = pallet.it-codigo   AND 
               saldo-estoq.cod-estabel   = pallet.cod-estabel AND 
               saldo-estoq.lote          = pallet.nr-pallet   AND 
            /*   saldo-estoq.dt-vali-lote  < TODAY AND  */ 
               saldo-estoq.dt-vali-lote >= INPUT FRAME f-pg-sel c-vali-ini AND
               saldo-estoq.dt-vali-lote <= INPUT FRAME f-pg-sel c-vali-fim 
               USE-INDEX ITEM-lote 
         NO-LOCK NO-ERROR.

     IF NOT AVAIL saldo-estoq THEN NEXT.
         
         FIND tt-digita WHERE
              tt-digita.lot-cod-estabel    = saldo-estoq.cod-estabel AND
              tt-digita.lot-cod-depos      = saldo-estoq.cod-depos   AND
              tt-digita.lot-dt-vali-lote   = saldo-estoq.dt-vali-lote AND
              tt-digita.lot-cod-localiz    = saldo-estoq.cod-localiz AND
              tt-digita.lot-lote           = saldo-estoq.lote        AND
              tt-digita.lot-it-codigo      = saldo-estoq.it-codigo   AND
              tt-digita.lot-cod-refer      = saldo-estoq.cod-refer
              NO-ERROR.

         IF NOT AVAIL tt-digita THEN DO:
              CREATE tt-digita.

              ASSIGN tt-digita.lot-cod-estabel    = saldo-estoq.cod-estabel 
                     tt-digita.lot-cod-depos      = saldo-estoq.cod-depos
                     tt-digita.lot-dt-vali-lote   = saldo-estoq.dt-vali-lote
                     tt-digita.lot-cod-localiz    = saldo-estoq.cod-localiz 
                     tt-digita.lot-lote           = saldo-estoq.lote        
                     tt-digita.lot-it-codigo      = saldo-estoq.it-codigo   
                     tt-digita.lot-cod-refer      = saldo-estoq.cod-refer.

              FIND FIRST emitente WHERE
                  emitente.cod-emitente = tt-lotes.cod-emitente
                  NO-LOCK NO-ERROR.

              IF AVAIL emitente THEN
                  ASSIGN tt-digita.lot-nome-abrev = emitente.nome-abrev.

         END.

         ASSIGN tt-digita.lot-quantidade    = tt-lotes.saldo.
             
         IF (saldo-estoq.dt-vali-lote - pallet.data-pallet) > 190 THEN
             ASSIGN tt-digita.lot-revalidado = "Sim".
         ELSE
             ASSIGN tt-digita.lot-revalidado = "". 
   END.


 END.

/* Fim Rotina para encontrar paletes em Terceiros */

       assign im-pg-dig:sensitive in frame f-relat = yes
              wh-label-dig:fgcolor                 = ?
              im-pg-sel:sensitive in frame f-relat = yes 
              wh-label-sel:fgcolor                 = ?.
       apply "mouse-select-click" to im-pg-dig in frame f-relat.
     /*** habilita **/
       do transaction:
          open query br-digita for each tt-digita.
          apply 'entry' to tt-digita.lot-lote in browse br-digita. 
       end.
    
    run pi-finalizar in h-acomp.
    enable  bt-retirar with frame f-pg-dig.
   /* disable bt-gera-excel WITH FRAME f-pg-dig. */
    ENABLE bt-executar WITH FRAME f-relat.
 end.    
  
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
        
    For Each  tt-digita :
        assign r_rowid = rowid(tt-digita).
             
    End.
  End. 
    
        
  /* Coloque aqui as valida‡äes das outras p ginas, lembrando que elas
     devem apresentar uma mensagem de erro cadastrada, posicionar na p gina 
     com problemas e colocar o focus no campo com problemas             */    
  /*     
    if input frame f-pg-sel c-cod-estabel-ini-fim < input frame f-pg-sel c-cod-estabel-ini-ini THEN DO:
       message "Estabelecimento Final Menor que Inicial" 
           view-as alert-box ERROR TITLE "Atencao !".
       apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
       apply "ENTRY":U to c-cod-estabel-ini-ini in frame f-pg-sel.
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
   
   ASSIGN tt-param.c-cod-estabel-ini = INPUT FRAME f-pg-sel c-cod-estabel-ini 
          tt-param.c-cod-estabel-fim = INPUT FRAME f-pg-sel c-cod-estabel-fim
          tt-param.c-it-codigo-ini   = INPUT FRAME f-pg-sel c-it-codigo-ini    
          tt-param.c-it-codigo-fim   = INPUT FRAME f-pg-sel c-it-codigo-fim    
          tt-param.c-lote-ini        = INPUT FRAME f-pg-sel c-lote-ini         
          tt-param.c-lote-fim        = INPUT FRAME f-pg-sel c-lote-fim         
          tt-param.c-nr-pedido-ini   = INPUT FRAME f-pg-sel c-nr-pedido-ini       
          tt-param.c-nr-pedido-fim   = INPUT FRAME f-pg-sel c-nr-pedido-fim       
          tt-param.pesq-jr           = INPUT FRAME f-pg-sel pesq-jr       
          tt-param.c-vali-ini        = INPUT FRAME f-pg-sel c-vali-ini        
          tt-param.c-vali-fim        = INPUT FRAME f-pg-sel c-vali-fim        
          tt-param.c-data-vali       = INPUT FRAME f-pg-sel c-data-vali
          tt-param.l-pl-excel        = l-pl-excel.    
       
    {include/i-rpexb.i}

    if  session:set-wait-state("general":U) then.

    {include/i-rprun.i cqp/escq0022rp.p} 

  
    {include/i-rpexc.i}

    if  session:set-wait-state("":U) then.
    
    {include/i-rptrm.i}

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/*----------------------------------------------------------------------------
   Rotina para testar a existencia do pedido e se pertence ao mesmo cliente
   e produto */


/*--------------------------------------------------------------------------*/





