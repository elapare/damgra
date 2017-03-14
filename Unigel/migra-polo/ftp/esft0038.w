&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME

&Scoped-define WINDOW-NAME w-relat

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/***************************************************************************
 **Programa.: esft0038.W 
 **Autor....: Damgra - JosÇ Roberto
 **Objetivo.: Etiqueta Especial 
 **Data.....: 05/05/2014
 ***************************************************************************/

{include/i-prgvrs.i esft0038 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

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
    field classifica           as integer
    field desc-classifica      as char format "x(40)"
    field i-nr-bobinas         AS integer
    field i-qtd-pallet         AS integer
    field c-cod-estabel-ini    like ped-venda.cod-estabel
    field c-nr-pedido-ini      like ped-venda.nr-pedido
    field c-nr-sequencia-ini   like ped-item.nr-sequencia
    field i-nr-pallet-ini      like pallet.nr-pallet
    field i-nr-pallet-fim      like pallet.nr-pallet
    field i-seq-ini            AS INT 
    field dir-etq              as char
    FIELD i-qt-copia AS INTE.



DEFINE TEMP-TABLE tt-digita no-undo
    FIELD nr-pedido            AS INT                          FORMAT "zzzzzz9"         LABEL "Pedido"
    FIELD nr-sequencia         AS INT                          FORMAT "zzzz9"           LABEL "Sq.Ped"
    FIELD nr-pallet            as int                          FORMAT ">>>>>9"          LABEL "Nr.Pallet"
    FIELD it-codigo            LIKE pallet.it-codigo           FORMAT "x(16)"           LABEL "Item"
    FIELD nr-bobinas           LIKE pallet.nr-bobinas          FORMAT "->>>>>>9"         LABEL "Qt.Bobs"
    FIELD i-seq-ini            AS INTEGER                      FORMAT ">>>9"            LABEL "Bob.Inicial"
    FIELD prod-cli             AS CHAR                         FORMAT "x(20)"           LABEL "Cod.Prod.Cliente"
    FIELD pedcli               AS CHAR                         FORMAT "x(20)"           LABEL "Nr.Ped.Cliente"
    FIELD largura              AS INT                          FORMAT ">>>9"            LABEL "Larg"
    FIELD obsetq               AS CHAR                         FORMAT "x(20)"           LABEL "Metragem"
    FIELD nome-abrev           LIKE ped-venda.nome-abrev       FORMAT "x(20)"           LABEL "Cliente"
    field diin                 as int                          format ">>>9"            label "Diin"
    field diex                 as int                          format ">>>9"            label "Diex"
    field qt-bob-ped           as int                          format ">>>>9"           label "Qt.Bobs.Ped"
    INDEX chave IS PRIMARY UNIQUE nr-pedido
                                  nr-sequencia
                                  nr-pallet.

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

DEFINE VARIABLE qtdbob-jr   AS INTEGER    NO-UNDO.
DEFINE VARIABLE soma-bob-jr AS INTEGER    NO-UNDO.
DEFINE VARIABLE bob-ult-jr  AS INTEGER    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */

&Scoped-define FRAME-NAME f-pg-cla 

&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita */


&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.nr-pedido tt-digita.nr-sequencia tt-digita.nr-pallet tt-digita.it-codigo tt-digita.nr-bobinas tt-digita.prod-cli tt-digita.pedcli tt-digita.largura tt-digita.obsetq tt-digita.nome-abrev tt-digita.diin tt-digita.diex tt-digita.qt-bob-ped
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.nr-bobinas  
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

    DEFINE VARIABLE qtd-plt-jr  AS INTEGER    NO-UNDO.

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rs-classif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Data Transaá∆o/ Item", 1
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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 2 
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

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60.29 BY 4.42.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60.29 BY 1.92.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60.29 BY 2.42.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE c-cod-estabel-ini AS CHAR label "Estab." initial "422" FORMAT "x(3)":U /*solic-318*/  
     VIEW-AS FILL-IN 
     SIZE 4 BY .88
     TOOLTIP "Informe C¢digo do Estabelecimento " NO-UNDO.

DEFINE VARIABLE c-nr-pedido-ini AS INT label "Pedido" FORMAT ">>>>>>9":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     TOOLTIP "Informe o Nr. do Pedido de Venda " NO-UNDO.

DEFINE VARIABLE c-nr-sequencia-ini AS INT label "Seq.Pedido" FORMAT ">>>>9":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88
     TOOLTIP "Informe a Seq.do Pedido de Venda " NO-UNDO.

DEFINE VARIABLE i-nr-pallet-ini AS int LABEL "Seq.Pallet" FORMAT ">>>9":U
     INITIAL 1
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     TOOLTIP "Informe Sequencia do Pallet " NO-UNDO.  

DEFINE VARIABLE i-nr-pallet-fim AS int FORMAT ">>>9":U
     INITIAL 9999
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     TOOLTIP "Informe Sequencia do Pallet " NO-UNDO.  

DEFINE VARIABLE i-nr-bobinas AS INT label "Bobs por Pallet" FORMAT ">>>>>>>9":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     TOOLTIP "Informe a Qtde.de Bobinas " NO-UNDO.

DEFINE VARIABLE i-qtd-pallet AS INT label "Qtd.Pallets" FORMAT ">>>>>>>9":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     TOOLTIP "Informe a Qtde.de Pallets " NO-UNDO.

DEFINE VARIABLE i-seq-ini AS INT INITIAL 1 label "Seq.Inicial" FORMAT ">>>>>>>9":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     TOOLTIP "Informe a Seq. Inicial.de Bobinas " NO-UNDO.

DEFINE VARIABLE i-bob-ped AS INT INITIAL 0 label "Bobs.Pedido" FORMAT ">>>>>>>9":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     TOOLTIP "Informe a Seq. Inicial.de Bobinas " NO-UNDO.

DEFINE VARIABLE i-qt-copia AS INT INITIAL 2 label "Qtde.Vias" FORMAT ">>>>>>>9":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     TOOLTIP "Informe a quantidade de vias de impressa para cada bobina " NO-UNDO.

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
     LABEL "Abre o Pedido" 
     SIZE 13 BY 1.

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
   tt-digita.nr-pedido  
   tt-digita.nr-sequencia
   tt-digita.nr-pallet  
   tt-digita.it-codigo  
   tt-digita.nr-bobinas 
    tt-digita.i-seq-ini
   tt-digita.prod-cli   
   tt-digita.pedcli     
   tt-digita.largura 
   tt-digita.diin
   tt-digita.diex
   tt-digita.qt-bob-ped
   tt-digita.obsetq     
   tt-digita.nome-abrev 

  ENABLE
    tt-digita.nr-bobinas 
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9
         BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

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
         SIZE 73.72 BY 10.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"

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
     c-cod-estabel-ini   AT ROW 1.5 COL 15 COLON-ALIGNED
     c-nr-pedido-ini     AT ROW 3   COL 15 COLON-ALIGNED
     c-nr-sequencia-ini  AT ROW 3   COL 42 COLON-ALIGNED
     i-nr-pallet-ini     AT ROW 4   COL 15 COLON-ALIGNED
     i-nr-pallet-fim     AT ROW 4   COL 42 colon-aligned NO-LABEL
     i-nr-bobinas        AT ROW 6   COL 15 COLON-ALIGNED
     i-qtd-pallet        at row 6   col 42 colon-aligned
     i-seq-ini           AT ROW 8.5 COL 15 COLON-ALIGNED
     i-bob-ped           AT ROW 8.5 COL 42 COLON-ALIGNED
    i-qt-copia           AT ROW 10.5 COL 15 COLON-ALIGNED

    image-1 AT ROW 4.00 COL 31
    image-2 AT ROW 4.00 COL 39
    RECT-20 AT ROW 1.00 COL 2.14
    RECT-21 AT ROW 5.62 COL 2.14
    RECT-22 AT ROW 7.72 COL 2.14
                                            

WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
       SIDE-LABELS NO-UNDERLINE THREE-D 
       AT COL 3 ROW 2.85
       SIZE 76.86 BY 10.62.

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 1.5 COL 8 HELP
          "Classificaá∆o para emiss∆o do relat¢rio" NO-LABEL
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
         TITLE              = "Etiqueta Especial"
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
                "Execuá∆o".

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

ON LEAVE OF i-nr-pallet-ini IN FRAME f-pg-sel
DO:


    assign i-nr-bobinas:SCREEN-VALUE IN FRAME f-pg-sel = ""
           i-qtd-pallet:SCREEN-VALUE IN FRAME f-pg-sel = ""
           i-bob-ped:SCREEN-VALUE IN FRAME f-pg-sel    = ""
        .

     for each ped-venda no-lock
             where ped-venda.cod-estabel  = INPUT FRAME f-pg-sel c-cod-estabel-ini     and 
                   ped-venda.nr-pedido    = INPUT FRAME f-pg-sel c-nr-pedido-ini 
                   USE-INDEX ch-pedseq, 
                   
             Each ped-item OF ped-venda No-lock Where
                  ped-item.nr-sequencia = INPUT FRAME f-pg-sel c-nr-sequencia-ini  AND
                  ped-item.cod-refer <> "".


             FIND FIRST var-result 
                  WHERE var-result.item-cotacao = ped-item.it-codigo
                  AND var-result.nr-estrut      = ped-item.nr-config
                  AND var-result.nome-var       = "BOBPALETE"  NO-LOCK NO-ERROR.

             IF AVAIL var-result THEN 
                ASSIGN i-nr-bobinas:SCREEN-VALUE IN FRAME f-pg-sel = STRING (var-result.valor-dec).

             FIND FIRST var-result 
                  WHERE var-result.item-cotacao = ped-item.it-codigo
                  AND var-result.nr-estrut      = ped-item.nr-config
                  AND var-result.nome-var       = "qtdpalete"  NO-LOCK NO-ERROR.

             IF AVAIL var-result THEN 
                ASSIGN i-qtd-pallet:SCREEN-VALUE IN FRAME f-pg-sel = STRING (var-result.valor-dec).

             FIND FIRST var-result 
                  WHERE var-result.item-cotacao = ped-item.it-codigo
                  AND var-result.nr-estrut      = ped-item.nr-config
                  AND var-result.nome-var       = "qtdbob"  NO-LOCK NO-ERROR.

             IF AVAIL var-result THEN 
                ASSIGN i-bob-ped:SCREEN-VALUE IN FRAME f-pg-sel = STRING (var-result.valor-dec).

     END.

    if input frame f-pg-sel i-nr-pallet-ini =
       input frame f-pg-sel i-nr-pallet-fim then
        assign i-seq-ini:sensitive in frame f-pg-sel = yes
               i-seq-ini:screen-value in frame f-pg-sel    = "1".
    else
        assign i-seq-ini:sensitive in frame f-pg-sel    = no
               i-seq-ini:screen-value in frame f-pg-sel = "1".

end.
     
ON LEAVE OF i-nr-pallet-fim IN FRAME f-pg-sel
DO:


    assign i-nr-bobinas:SCREEN-VALUE IN FRAME f-pg-sel = ""
           i-qtd-pallet:SCREEN-VALUE IN FRAME f-pg-sel = "".

     for each ped-venda no-lock
             where ped-venda.cod-estabel  = INPUT FRAME f-pg-sel c-cod-estabel-ini     and 
                   ped-venda.nr-pedido    = INPUT FRAME f-pg-sel c-nr-pedido-ini 
                   USE-INDEX ch-pedseq, 
                   
             Each ped-item OF ped-venda No-lock Where
                  ped-item.nr-sequencia = INPUT FRAME f-pg-sel c-nr-sequencia-ini  AND
                  ped-item.cod-refer <> "".


             FIND FIRST var-result 
                  WHERE var-result.item-cotacao = ped-item.it-codigo
                  AND var-result.nr-estrut      = ped-item.nr-config
                  AND var-result.nome-var       = "BOBPALETE"  NO-LOCK NO-ERROR.

             IF AVAIL var-result THEN 
                ASSIGN i-nr-bobinas:SCREEN-VALUE IN FRAME f-pg-sel = STRING (var-result.valor-dec).

             FIND FIRST var-result 
                  WHERE var-result.item-cotacao = ped-item.it-codigo
                  AND var-result.nr-estrut      = ped-item.nr-config
                  AND var-result.nome-var       = "qtdpalete"  NO-LOCK NO-ERROR.

             IF AVAIL var-result THEN 
                ASSIGN i-qtd-pallet:SCREEN-VALUE IN FRAME f-pg-sel = STRING (var-result.valor-dec).

             if input frame f-pg-sel i-nr-pallet-fim > var-result.valor-dec then
                 assign i-nr-pallet-fim:SCREEN-VALUE IN FRAME f-pg-sel = string(var-result.valor-dec).

             FIND FIRST var-result 
                  WHERE var-result.item-cotacao = ped-item.it-codigo
                  AND var-result.nr-estrut      = ped-item.nr-config
                  AND var-result.nome-var       = "qtdbob"  NO-LOCK NO-ERROR.

             IF AVAIL var-result THEN 
                ASSIGN i-bob-ped:SCREEN-VALUE IN FRAME f-pg-sel = STRING (var-result.valor-dec).


     END.

    if input frame f-pg-sel i-nr-pallet-ini =
       input frame f-pg-sel i-nr-pallet-fim then
        assign i-seq-ini:sensitive in frame f-pg-sel = yes.
    else
        assign i-seq-ini:sensitive in frame f-pg-sel = no
               i-seq-ini:screen-value in frame f-pg-sel = "1".

end.
     


ON LEAVE OF c-nr-sequencia-ini IN FRAME f-pg-sel
DO:

    assign i-nr-bobinas:SCREEN-VALUE IN FRAME f-pg-sel = ""
           i-qtd-pallet:SCREEN-VALUE IN FRAME f-pg-sel = "".

     for each ped-venda no-lock
             where ped-venda.cod-estabel  = INPUT FRAME f-pg-sel c-cod-estabel-ini     and 
                   ped-venda.nr-pedido    = INPUT FRAME f-pg-sel c-nr-pedido-ini 
                   USE-INDEX ch-pedseq, 
                   
             Each ped-item OF ped-venda No-lock Where
                  ped-item.nr-sequencia = INPUT FRAME f-pg-sel c-nr-sequencia-ini  AND
                  ped-item.cod-refer <> "".


             FIND FIRST var-result 
                  WHERE var-result.item-cotacao = ped-item.it-codigo
                  AND var-result.nr-estrut      = ped-item.nr-config
                  AND var-result.nome-var       = "BOBPALETE"  NO-LOCK NO-ERROR.

             IF AVAIL var-result THEN 
                ASSIGN i-nr-bobinas:SCREEN-VALUE IN FRAME f-pg-sel = STRING (var-result.valor-dec).

             FIND FIRST var-result 
                  WHERE var-result.item-cotacao = ped-item.it-codigo
                  AND var-result.nr-estrut      = ped-item.nr-config
                  AND var-result.nome-var       = "qtdpalete"  NO-LOCK NO-ERROR.

             IF AVAIL var-result THEN 
                ASSIGN i-qtd-pallet:SCREEN-VALUE IN FRAME f-pg-sel = STRING (var-result.valor-dec)
                      i-nr-pallet-fim:SCREEN-VALUE IN FRAME f-pg-sel = STRING (var-result.valor-dec).

             FIND FIRST var-result 
                  WHERE var-result.item-cotacao = ped-item.it-codigo
                  AND var-result.nr-estrut      = ped-item.nr-config
                  AND var-result.nome-var       = "qtdbob"  NO-LOCK NO-ERROR.

             IF AVAIL var-result THEN 
                ASSIGN i-bob-ped:SCREEN-VALUE IN FRAME f-pg-sel = STRING (var-result.valor-dec).


     END.

END.



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
        display   
            tt-digita.nr-pedido  
            tt-digita.nr-sequencia
            tt-digita.it-codigo  
            tt-digita.nr-bobinas 
            tt-digita.prod-cli   
            tt-digita.pedcli     
            tt-digita.largura
            tt-digita.diin
            tt-digita.diex
            tt-digita.qt-bob-ped
            tt-digita.obsetq     
            tt-digita.nome-abrev 
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
  apply 'entry' to bt-alterar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-ENTRY OF br-digita IN FRAME f-pg-dig
DO:
   /* trigger para inicializar campos da temp table de digitaá∆o */
/*   if  br-digita:new-row in frame f-pg-dig then do:
       assign tt-digita.nome:screen-value in browse br-digita = string(today, "99/99/9999").
   end.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
DO:
    /* ê aqui que a gravaá∆o da linha da temp-table ≤ efetivada.
       PorÇm as validaá‰es dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment†rio */
    
    if br-digita:NEW-ROW in frame f-pg-dig then 
    do transaction on error undo, return no-apply:
        create tt-digita.  
        assign INPUT BROWSE br-digita tt-digita.nr-pedido  
               input browse br-digita tt-digita.nr-sequencia  
               input browse br-digita tt-digita.it-codigo  
               input browse br-digita tt-digita.nr-bobinas 
               input browse br-digita tt-digita.prod-cli   
               input browse br-digita tt-digita.pedcli     
               input browse br-digita tt-digita.largura      
               input browse br-digita tt-digita.diin      
               input browse br-digita tt-digita.diex 
               input browse br-digita tt-digita.qt-bob-ped 
               input browse br-digita tt-digita.obsetq     
               input browse br-digita tt-digita.nome-abrev. 
            
            br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    end.
    else do transaction on error undo, return no-apply:
        
        assign INPUT BROWSE br-digita tt-digita.nr-pedido  
               input browse br-digita tt-digita.nr-sequencia  
               input browse br-digita tt-digita.it-codigo  
               input browse br-digita tt-digita.nr-bobinas 
               input browse br-digita tt-digita.prod-cli   
               input browse br-digita tt-digita.pedcli     
               input browse br-digita tt-digita.largura      
               input browse br-digita tt-digita.diin      
               input browse br-digita tt-digita.diex      
               input browse br-digita tt-digita.qt-bob-ped 
               input browse br-digita tt-digita.obsetq     
               input browse br-digita tt-digita.nome-abrev .

            display
                tt-digita.nr-pedido  
                tt-digita.nr-sequencia
                tt-digita.it-codigo  
                tt-digita.nr-bobinas 
                tt-digita.prod-cli   
                tt-digita.pedcli     
                tt-digita.largura
                tt-digita.diin
                tt-digita.diex
                tt-digita.qt-bob-ped
                tt-digita.obsetq     
                tt-digita.nome-abrev 
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
  apply 'entry' to tt-digita.nr-bobinas in browse br-digita. 
 
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
    enable bt-retirar bt-alterar with frame f-pg-dig.
           
    if  num-results('br-digita') > 0 then do:
        if  br-digita:insert-row('after') in frame f-pg-dig then.
    end.
    else do transaction:
        create tt-digita.
        open query br-digita for each tt-digita.
        apply 'entry' to tt-digita.nr-bobinas in browse br-digita. 
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

  
on LEAVE OF tt-digita.nr-bobinas in browse br-digita do:
   run pi-leave-pallete.
end.


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "esft0038" "2.06.00.001"}
        rs-destino:SENSITIVE IN FRAME f-pg-imp = NO.
/* inicializaá‰es do template de relat¢rio */
{include/i-rpini.i}

    ASSIGN rs-destino = 1.
          


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

    rs-destino:SENSITIVE IN FRAME f-pg-imp = NO.
    rs-execucao:SENSITIVE IN FRAME f-pg-imp = NO.
      bt-config-impr:SENSITIVE IN FRAME f-pg-imp = NO.

    assign i-seq-ini:sensitive in frame f-pg-sel = no
           i-nr-bobinas:sensitive in frame f-pg-sel = no.
    
    
    assign im-pg-dig:sensitive in frame f-relat = no
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

  DISPLAY c-cod-estabel-ini
          c-nr-pedido-ini   
          c-nr-sequencia-ini
          i-nr-pallet-ini
          i-nr-pallet-fim
          i-nr-bobinas  
          i-seq-ini
      i-qt-copia
      WITH FRAME f-pg-sel IN WINDOW w-relat.

  ENABLE c-cod-estabel-ini
         c-nr-pedido-ini  
         c-nr-sequencia-ini
         i-nr-pallet-ini  
         i-nr-pallet-fim
         i-nr-bobinas
         i-seq-ini  
      i-qt-copia
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

  ENABLE br-digita bt-inserir  
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

     run pi-inicializar in h-acomp (input "Carregando Pedidos ...Aguarde").  
     
     for each ped-venda no-lock
             where ped-venda.cod-estabel  = INPUT FRAME f-pg-sel c-cod-estabel-ini     and 
                   ped-venda.nr-pedido    = INT (INPUT FRAME f-pg-sel c-nr-pedido-ini) 
                   USE-INDEX ch-pedseq, 
                   
             Each ped-item OF ped-venda No-lock Where
                  ped-item.nr-sequencia = INPUT FRAME f-pg-sel c-nr-sequencia-ini and 
                  ped-item.cod-refer <> "" .

         assign qtd-plt-jr = input frame f-pg-sel i-nr-pallet-ini
                soma-bob-jr = 0.

         do while qtd-plt-jr <= input frame f-pg-sel i-nr-pallet-fim:

             FIND tt-digita WHERE
                  tt-digita.nr-pedido    = ped-venda.nr-pedido   AND
                  tt-digita.nr-sequencia = ped-item.nr-sequencia AND
                  tt-digita.nr-pallet    = qtd-plt-jr
                  NO-ERROR.

             IF NOT AVAIL tt-digita THEN 
                  CREATE tt-digita.

             ASSIGN tt-digita.nr-pedido    = ped-venda.nr-pedido 
                    tt-digita.nr-sequencia = ped-item.nr-sequencia 
                    tt-digita.nr-pallet    = qtd-plt-jr.

             assign tt-digita.nr-bobinas = INPUT FRAME f-pg-sel i-nr-bobinas.

             ASSIGN tt-digita.it-codigo   = ped-item.it-codigo
                    tt-digita.nome-abrev  = ped-venda.nome-abrev.

             FIND FIRST var-result 
                  WHERE var-result.item-cotacao = ped-item.it-codigo
                  AND var-result.nr-estrut      = ped-item.nr-config
                  AND var-result.nome-var       = "PEDCLI"  NO-LOCK NO-ERROR.

             IF AVAIL var-result THEN 
                ASSIGN tt-digita.pedcli = var-result.valor-char.

             FIND FIRST var-result 
                  WHERE var-result.item-cotacao = ped-item.it-codigo
                  AND var-result.nr-estrut      = ped-item.nr-config
                  AND var-result.nome-var       = "PRODCLIE"  NO-LOCK NO-ERROR.

             IF AVAIL var-result THEN 
                ASSIGN tt-digita.prod-cli   = var-result.valor-char.

             FIND FIRST var-result 
                  WHERE var-result.item-cotacao = ped-item.it-codigo
                  AND var-result.nr-estrut    = ped-item.nr-config
                  AND var-result.nome-var     = "LARGURA"  NO-LOCK NO-ERROR.

             IF AVAIL var-result THEN 
                ASSIGN tt-digita.largura = var-result.valor-dec.

             FIND FIRST var-result 
                  WHERE var-result.item-cotacao = ped-item.it-codigo
                  AND var-result.nr-estrut    = ped-item.nr-config
                  AND var-result.nome-var     = "DIIN"  NO-LOCK NO-ERROR.

             IF AVAIL var-result THEN 
                ASSIGN tt-digita.diin = var-result.valor-dec.

             FIND FIRST var-result 
                  WHERE var-result.item-cotacao = ped-item.it-codigo
                  AND var-result.nr-estrut    = ped-item.nr-config
                  AND var-result.nome-var     = "DIEX"  NO-LOCK NO-ERROR.

             IF AVAIL var-result THEN 
                ASSIGN tt-digita.diex = var-result.valor-dec.

             FIND FIRST var-result 
                  WHERE var-result.item-cotacao = ped-item.it-codigo
                  AND var-result.nr-estrut    = ped-item.nr-config
                  AND var-result.nome-var     = "QTDBOB"  NO-LOCK NO-ERROR.

             IF AVAIL var-result THEN 
                ASSIGN tt-digita.qt-bob-ped = var-result.valor-dec.

             FIND FIRST var-result 
                 WHERE var-result.item-cotacao = ped-item.it-codigo
                 AND var-result.nr-estrut    = ped-item.nr-config
                 AND var-result.nome-var     = "OBSETQ"  NO-LOCK NO-ERROR.

            IF AVAIL var-result THEN 
               ASSIGN tt-digita.obsetq = substring(var-result.valor-char,1,4).

            FIND FIRST var-result 
                 WHERE var-result.item-cotacao = ped-item.it-codigo
                 AND var-result.nr-estrut    = ped-item.nr-config
                 AND var-result.nome-var     = "qbultpale"  NO-LOCK NO-ERROR.

            IF AVAIL var-result THEN 
               ASSIGN bob-ult-jr = var-result.valor-dec.
            else
                ASSIGN bob-ult-jr = 0.

            if input frame f-pg-sel i-nr-pallet-fim = input frame f-pg-sel i-qtd-pallet and 
               qtd-plt-jr = input frame f-pg-sel i-nr-pallet-fim then
                   assign tt-digita.nr-bobinas = bob-ult-jr.

             if input frame f-pg-sel i-nr-pallet-ini = input frame f-pg-sel i-nr-pallet-fim THEN 
                   assign tt-digita.nr-bobinas = tt-digita.nr-bobinas - input frame f-pg-sel i-seq-ini + 1.

            IF tt-digita.nr-bobinas <= 0  THEN
                tt-digita.nr-bobinas = 1.
            tt-digita.i-seq-ini = input frame f-pg-sel i-seq-ini.
             
             assign v-num-reg-lidos = v-num-reg-lidos + 1.
             run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

             assign qtd-plt-jr = qtd-plt-jr + 1.

         end.
             
     END. 

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
          apply 'entry' to tt-digita.nr-pedido in browse br-digita. 
       end.
    
    run pi-finalizar in h-acomp.
    enable bt-retirar bt-alterar with frame f-pg-dig.
    disable bt-inserir bt-salvar WITH FRAME f-pg-dig.
    ENABLE bt-executar WITH FRAME f-relat.
  /*  DISABLE bt-paletes WITH FRAME f-relat.*/
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
        
  /* Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas
     devem apresentar uma mensagem de erro cadastrada, posicionar na p†gina 
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
         tt-param.hora-exec            = time
         tt-param.classifica           = input frame f-pg-cla rs-classif
         tt-param.desc-classifica      = entry((tt-param.classifica - 1) * 2 + 1, 
                                         rs-classif:radio-buttons in frame f-pg-cla).
      if tt-param.destino = 1 
      then assign tt-param.arquivo = "".
      else if  tt-param.destino = 2 
           then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
           else 
               assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".

   ASSIGN tt-param.c-cod-estabel-ini    = INPUT FRAME f-pg-sel c-cod-estabel-ini
          tt-param.c-nr-pedido-ini      = INT (INPUT FRAME f-pg-sel c-nr-pedido-ini)
          tt-param.c-nr-sequencia-ini   = INT (INPUT FRAME f-pg-sel c-nr-sequencia-ini)
          tt-param.i-nr-pallet-ini      = INPUT FRAME f-pg-sel i-nr-pallet-ini
          tt-param.i-seq-ini             = INT (INPUT FRAME f-pg-sel i-seq-ini)
          tt-param.i-nr-bobinas          = INT (INPUT FRAME f-pg-sel i-nr-bobinas)
          tt-param.i-qt-copia            = INT (INPUT FRAME f-pg-sel i-qt-copia).

    {include/i-rpexb.i}

    if  session:set-wait-state("general":U) then.
    
    {include/i-rprun.i ftp/esft0038rp.p} 
  
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
  Purpose: Gerencia a Troca de P†gina (folder)   
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

