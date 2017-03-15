&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME

&Scoped-define WINDOW-NAME w-relat

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/***************************************************************************
 **Programa.: esft0032.W 
 **Autor....: Bruno / Edson
 **Objetivo.: Gera Arquivo de Retornos - (COVRE)
 **Data.....: 15/06/2010
 ***************************************************************************/

{include/i-prgvrs.i esft0032 2.04.00.000}

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


/* Tabela Pedidos */
def NEW global SHARED temp-table tt-pedidos NO-UNDO
    field pedido AS INTEGER.

def new global shared var tt-TipPed AS CHAR FORMAT "X(5)" NO-UNDO.

ASSIGN tt-TipPed = "".

FOR EACH tt-pedidos:
    DELETE tt-pedidos.
END.
/**********************************/


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
    field c-cod-estabel-ini    like ped-venda.cod-estabel
    FIELD c-comercial-invoice  AS CHAR FORMAT "x(20)"
    FIELD c-obs1               AS CHAR FORMAT "x(30)"
    FIELD c-obs2               AS CHAR FORMAT "x(50)"
    FIELD c-carreta            AS CHAR FORMAT "x(10)"
    FIELD c-container          AS CHAR FORMAT "x(15)"
    field rs-pedido            AS INTEGER
    FIELD i-cod-emitente       LIKE emitente.cod-emitente
    field arquivo-dir          as char
    .
    

DEFINE TEMP-TABLE tt-digita no-undo
    FIELD marca                    AS CHAR                  FORMAT "x(1)"  LABEL "Marca"                 
    FIELD pal-nr-pedido            AS INT  FORMAT ">>>>>>9" LABEL "Pedido"
    FIELD pal-nr-pallet            AS CHAR FORMAT "x(15)"   label "Nr.Palete"
    FIELD pal-largura              AS int  FORMAT ">>>9"    LABEL "Larg"
    FIELD pal-diin                 AS int  FORMAT ">>>9"    LABEL "Diin"
    FIELD pal-diex                 AS int  FORMAT ">>>9"    LABEL "Diex"
    FIELD pal-it-codigo            LIKE movto-estoq.it-codigo LABEL "Item"
    FIELD pal-cod-refer            LIKE it-nota-fisc.cod-refer LABEL "Refer."
    FIELD pal-nro-docto            LIKE it-nota-fisc.nr-nota-fis LABEL "N.Fiscal"
    FIELD pal-nr-seq-fat           LIKE it-nota-fisc.nr-seq-fat  LABEL "Seq"
    FIELD pal-qtd-plt              AS INTEGER FORMAT "zz9"  LABEL "Qt.Plt"
    FIELD pal-nr-bobinas           LIKE pallet.nr-bobinas   LABEL "Qt.Bobs"
    FIELD pal-peso-liq             AS DECIMAL LABEL "Peso Liquido" FORMAT "->>>>>>>9.999"
    FIELD pal-peso-bru             AS DECIMAL LABEL "Peso Bruto" FORMAT "->>>>>>>9.999"
    FIELD pal-serie-docto          AS CHAR 

    INDEX chave IS PRIMARY UNIQUE pal-cod-refer 
                                  pal-nr-pedido
                                  pal-nr-pallet
                                  pal-nro-docto
                                  pal-serie-docto
                                  pal-nr-seq-fat
                                  
                                  .

define TEMP-TABLE tt-Consulta NO-UNDO
    FIELD it-codigo LIKE pallet.it-codigo
    FIELD nr-bobinas LIKE pallet.nr-bobinas
    FIELD nr-pallet LIKE pallet.nr-pallet
    FIELD peso-bruto LIKE pallet.peso-bruto
    FIELD peso-liquido LIKE pallet.peso-liquido
    FIELD situacao LIKE pallet.situacao
    FIELD nr-pedido LIKE pallet.nr-pedido
    FIELD tp-pedido LIKE ped-venda.tp-pedido
    FIELD cod-depos LIKE saldo-estoq.cod-depos
    FIELD ge-codigo LIKE item.ge-codigo
    FIELD cod-localiz LIKE pallet.cod-localiz
    FIELD recorte LIKE pallet.cod-localiz
    FIELD cod-refer LIKE pallet.cod-refer
    FIELD nro-docto LIKE saldo-terc.nro-docto
    FIELD serie-docto AS CHAR 
    FIELD sequencia LIKE saldo-terc.sequencia
    FIELD w-larg LIKE lote-carac-tec.vl-result
    FIELD w-diin LIKE lote-carac-tec.vl-result
    FIELD w-diex LIKE lote-carac-tec.vl-result
    FIELD Cliente LIKE ped-venda.nome-abrev.


DEFINE TEMP-TABLE tt-notas NO-UNDO
    FIELD nro-docto    LIKE saldo-terc.nro-docto
    FIELD sequencia    LIKE saldo-terc.sequencia
    FIELD saldo        LIKE saldo-terc.quantidade
    FIELD cod-emitente LIKE saldo-terc.cod-emitente
    FIELD serie        LIKE saldo-terc.serie
    FIELD cod-estabel  LIKE saldo-terc.cod-estabel
    INDEX chave-notas IS PRIMARY UNIQUE serie
                                        nro-docto
                                        sequencia.

DEFINE TEMP-TABLE tt-lotes NO-UNDO
    FIELD nro-docto    LIKE saldo-terc.nro-docto
    FIELD serie        LIKE saldo-terc.serie
    FIELD sequencia    LIKE saldo-terc.sequencia
    FIELD lote         LIKE saldo-terc.lote
    FIELD remessa      LIKE saldo-terc.quantidade
    FIELD retorno      LIKE saldo-terc.quantidade
    FIELD saldo        LIKE saldo-terc.quantidade
    FIELD cod-estabel  LIKE saldo-terc.cod-estabel
    FIELD cod-emitente LIKE saldo-terc.cod-emitente
    FIELD it-codigo    LIKE saldo-terc.it-codigo
    INDEX chave-lotes IS PRIMARY UNIQUE serie
                                        nro-docto
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

DEFINE VARIABLE tem-pedido-jr AS CHARACTER  NO-UNDO.

DEFINE VARIABLE larg-jr    AS INTEGER    NO-UNDO.
DEFINE VARIABLE diin-jr    AS INTEGER    NO-UNDO.
DEFINE VARIABLE diex-jr    AS INTEGER    NO-UNDO.

DEFINE VARIABLE varSaldo   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE refer-jr   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE txtCliente AS CHARACTER  NO-UNDO.
DEFINE VARIABLE w-lote     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE w-item     AS CHARACTER  NO-UNDO.

DEFINE VARIABLE w-larg     AS INT no-undo.
DEFINE VARIABLE w-diin     AS INT no-undo.
DEFINE VARIABLE w-diex     AS INT no-undo.

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

&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.marca tt-digita.pal-nr-pedido tt-digita.pal-nr-pallet tt-digita.pal-it-codigo tt-digita.pal-cod-refer tt-digita.pal-nro-docto tt-digita.pal-nr-seq-fat tt-digita.pal-largura tt-digita.nr-bobinas tt-digita.pal-peso-liq tt-digita.pal-peso-bru
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.marca  
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
     LABEL "Desmarca" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-inserir 
     LABEL "Marca" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-recuperar 
     LABEL " " 
     SIZE 15 BY 1.

DEFINE BUTTON bt-retirar 
     LABEL "Marca Todos" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-salvar 
     LABEL "Desmarca Todos" 
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
     
DEFINE VARIABLE t-arquivo-dest AS CHAR FORMAT "x(77)"
     INITIAL "Destino:":U  
     VIEW-AS TEXT  
     SIZE 6 BY .88 
     NO-UNDO   BGCOLOR 7.     

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

DEFINE BUTTON bt-ApagarTudo 
     LABEL "Apagar Tudo" 
     SIZE 12 BY 1.

DEFINE VARIABLE txtApagar AS INTEGER FORMAT ">>>,>>>,>>9":U 
     LABEL "Apagar" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .66 NO-UNDO.

DEFINE VARIABLE txtInsPedidos AS INTEGER FORMAT ">>>,>>>,>>9":U 
     LABEL "Inserir Pedidos:" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .66 NO-UNDO.

DEFINE VARIABLE lstPedidos AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 15 BY 3 NO-UNDO.

DEFINE VARIABLE rs-pedido AS INTEGER INITIAL 1 
VIEW-AS RADIO-SET HORIZONTAL 
RADIO-BUTTONS 
     "Intervalo", 1,
     "Diversos", 2
SIZE 25 BY 1 NO-UNDO. 

DEFINE VARIABLE c-cod-estabel-ini AS CHAR label "Estab." FORMAT "x(3)"
       /*solic-318*/ 
     VIEW-AS FILL-IN 
     SIZE 4 BY .66
     TOOLTIP "Informe C¢digo do Estabelecimento " NO-UNDO.

DEFINE VARIABLE c-it-codigo-ini AS CHAR label "Filme" FORMAT "x(16)" 
     INITIAL "":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY .66
     TOOLTIP "Informe o C¢digo do Filme " NO-UNDO.

DEFINE VARIABLE c-it-codigo-fim AS CHAR FORMAT "x(16)":U 
     INITIAL "ZZZZZZZZZZZZZ":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY .66 NO-UNDO.

DEFINE VARIABLE c-nr-pedido-ini AS INT label "Pedido" FORMAT ">>>>>>9"
     INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .66
     TOOLTIP "Informe Nr. do Pedido " NO-UNDO.

DEFINE VARIABLE c-nr-pedido-fim AS int FORMAT ">>>>>>9"
     INITIAL 9999999 
     VIEW-AS FILL-IN 
     SIZE 8 BY .66 NO-UNDO.

DEFINE VARIABLE c-nr-pallet-ini AS CHAR label "Pallet" FORMAT "x(16)"
     INITIAL "":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY .66
     TOOLTIP "Informe N£mero do Pallet " NO-UNDO.

DEFINE VARIABLE c-nr-pallet-fim AS CHAR FORMAT "x(16)"
     INITIAL "ZZZZZZZZZZZZZ":U  
     VIEW-AS FILL-IN 
     SIZE 17 BY .66 NO-UNDO.

DEFINE VARIABLE c-largura-ini AS INT label "Largura" FORMAT ">>>>9" 
     INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .66
     TOOLTIP "Informe a Largura do filme " NO-UNDO.

DEFINE VARIABLE c-largura-fim AS INT FORMAT ">>>>9"
     INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 6 BY .66 NO-UNDO.

DEFINE VARIABLE c-cod-refer-ini AS CHAR label "Referˆncia" FORMAT "x(16)"
     INITIAL "":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY .66
     TOOLTIP "Informe a Referˆncia " NO-UNDO.

DEFINE VARIABLE c-cod-refer-fim AS CHAR FORMAT "x(16)"
     INITIAL "ZZZZZZZZZZZZZ":U  
     VIEW-AS FILL-IN 
     SIZE 17 BY .66 NO-UNDO.

DEFINE VARIABLE c-nro-docto-ini AS CHAR label "NF.Origem" FORMAT "x(07)"
     INITIAL "":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY .66
     TOOLTIP "Informe a Nota Fiscal de Origem " NO-UNDO.

DEFINE VARIABLE c-nro-docto-fim AS CHAR FORMAT "x(07)"
     INITIAL "ZZZZZZZ":U  
     VIEW-AS FILL-IN 
     SIZE 17 BY .66 NO-UNDO.

DEFINE VARIABLE i-cod-emitente-ini AS INT label "Emitente" FORMAT ">>>>>>9" 
     INITIAL 105855 
     VIEW-AS FILL-IN 
     SIZE 6 BY .66
     TOOLTIP "Informe Cod. do Emitente " NO-UNDO.

DEFINE VARIABLE c-nome-ab-cli AS char label "Nome" FORMAT "x(45)"
     INITIAL "COVRE"
     VIEW-AS FILL-IN 
     SIZE 27 BY .66 NO-UNDO.


DEFINE VARIABLE c-arquivo-dest AS CHAR FORMAT "x(77)"
     INITIAL "v:\temp\":U  
     VIEW-AS FILL-IN 
     SIZE 35 BY .88
     NO-UNDO.

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
/*
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.
*/
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
   tt-digita.marca
   tt-digita.pal-nr-pedido
   tt-digita.pal-nr-pallet 
   tt-digita.pal-it-codigo 
   tt-digita.pal-cod-refer 
   tt-digita.pal-nro-docto
   tt-digita.pal-nr-seq-fat 
   tt-digita.pal-largura
   tt-digita.pal-nr-bobinas 
   tt-digita.pal-peso-liq 
   tt-digita.pal-peso-bru

  ENABLE
    tt-digita.marca 
    
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
         "Monta Tabela de Paletes"
    c-arquivo-dest at row 14.64 col 45 help
    "Arquivo de saida" no-label
      t-arquivo-dest AT ROW 14.64 COL 39 NO-LABEL font 1
 
    
    /* bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
*/
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
     c-cod-estabel-ini  AT ROW 1 COL 15.72  COLON-ALIGNED
     c-it-codigo-ini    AT ROW 1.8 COL 15.72  COLON-ALIGNED
     c-it-codigo-fim    AT ROW 1.8 COL 42.72  COLON-ALIGNED  NO-LABEL
     c-nr-pedido-ini    AT ROW 2.6 COL 15.72  COLON-ALIGNED
     c-nr-pedido-fim    AT ROW 2.6 COL 42.72  COLON-ALIGNED  NO-LABEL
     c-nr-pallet-ini    AT ROW 3.4 COL 15.72  COLON-ALIGNED
     c-nr-pallet-fim    AT ROW 3.4 COL 42.72  COLON-ALIGNED  NO-LABEL
     c-largura-ini      AT ROW 4.2 COL 15.72  COLON-ALIGNED
     c-largura-fim      AT ROW 4.2 COL 42.72  COLON-ALIGNED  NO-LABEL
     c-cod-refer-ini    AT ROW 5.0 COL 15.72  COLON-ALIGNED
     c-cod-refer-fim    AT ROW 5.0 COL 42.72  COLON-ALIGNED  NO-LABEL
     c-nro-docto-ini    AT ROW 5.8 COL 15.72  COLON-ALIGNED
     c-nro-docto-fim    AT ROW 5.8 COL 42.72  COLON-ALIGNED  NO-LABEL
     i-cod-emitente-ini AT ROW 6.6 COL 15.72  COLON-ALIGNED
     c-nome-ab-cli      AT ROW 6.6 COL 42.72  COLON-ALIGNED  NO-LABEL
     
/* Pedidos */
     rs-pedido NO-LABEL
       AT ROW 7.5 COL 19
     
     lstPedidos LABEL "Lista de Pedidos"
       AT ROW 8.5 col 17 COLON-ALIGNED
    
     txtInsPedidos LABEL "Inserir Pedidos"
      AT ROW 8.5 COL 44 COLON-ALIGNED
      VIEW-AS FILL-IN
      SIZE 12 BY .68
      FONT 1
    
     txtApagar LABEL "Apagar"
      AT ROW 09.5 COL 44 COLON-ALIGNED
      VIEW-AS FILL-IN
      SIZE 12 BY .68
      FONT 1
    
     bt-ApagarTudo LABEL "Apagar Tudo"
      AT ROW 10.5 COL 44 COLON-ALIGNED
    
     IMAGE-1  AT ROW 1.8 COL 36
     IMAGE-2  AT ROW 1.8 COL 41
     IMAGE-3  AT ROW 2.6 COL 36
     IMAGE-4  AT ROW 2.6 COL 41 
     IMAGE-47 AT ROW 3.4 COL 36
     IMAGE-54 AT ROW 3.4 COL 41
     IMAGE-55 AT ROW 4.2 COL 36
     IMAGE-56 AT ROW 4.2 COL 41 
     IMAGE-86 AT ROW 5.0 COL 36
     IMAGE-87 AT ROW 5.0 COL 41 
     IMAGE-88 AT ROW 5.8 COL 36
     IMAGE-89 AT ROW 5.8 COL 41 
     
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
         TITLE              = "Lista de Embarque (Nota Fiscal / Pallet)"
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

ON leave OF c-nro-docto-ini IN FRAME f-pg-sel 
DO:

   DEFINE VARIABLE c-nf   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE c-nf-x AS CHARACTER  NO-UNDO.


   ASSIGN c-nf = "0000000"
          c-nf-x = input frame f-pg-sel c-nro-docto-ini.

   ASSIGN substring(c-nf,(8 - LENGTH(c-nf-x)),LENGTH(c-nf-x)) = c-nf-x
          c-nro-docto-ini:SCREEN-VALUE IN FRAME f-pg-sel = c-nf.

        
END.


ON leave OF c-nro-docto-fim IN FRAME f-pg-sel 
DO:

   DEFINE VARIABLE c-nf   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE c-nf-x AS CHARACTER  NO-UNDO.


   ASSIGN c-nf = "0000000"
          c-nf-x = input frame f-pg-sel c-nro-docto-fim.

   ASSIGN substring(c-nf,(8 - LENGTH(c-nf-x)),LENGTH(c-nf-x)) = c-nf-x
          c-nro-docto-fim:SCREEN-VALUE IN FRAME f-pg-sel = c-nf.

        
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
            tt-digita.marca
            tt-digita.pal-nr-pedido
            tt-digita.pal-nr-pallet 
            tt-digita.pal-it-codigo 
            tt-digita.pal-cod-refer 
            tt-digita.pal-nro-docto
            tt-digita.pal-nr-seq-fat 
            tt-digita.pal-largura
            tt-digita.pal-nr-bobinas 
            tt-digita.pal-peso-liq 
            tt-digita.pal-peso-bru
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
    /*  aqui que a grava‡Æo da linha da temp-table ² efetivada.
       Por‚m as valida‡äes dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment rio */
    
    if br-digita:NEW-ROW in frame f-pg-dig then 
    do transaction on error undo, return no-apply:
        create tt-digita.
        assign INPUT BROWSE br-digita tt-digita.marca
               INPUT BROWSE br-digita tt-digita.pal-nr-pedido
               input browse br-digita tt-digita.pal-nr-pallet 
               input browse br-digita tt-digita.pal-it-codigo
               input browse br-digita tt-digita.pal-nro-docto
               input browse br-digita tt-digita.pal-nr-seq-fat
               input browse br-digita tt-digita.pal-largura
               input browse br-digita tt-digita.pal-nr-bobinas   
               input browse br-digita tt-digita.pal-peso-liq 
               input browse br-digita tt-digita.pal-peso-bru. 
            
            
            br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    end.
    else do transaction on error undo, return no-apply:
        
            assign INPUT BROWSE br-digita tt-digita.marca
                   INPUT BROWSE br-digita tt-digita.pal-nr-pedido
                   input browse br-digita tt-digita.pal-nr-pallet   
                   input browse br-digita tt-digita.pal-it-codigo
                   input browse br-digita tt-digita.pal-cod-refer
                   input browse br-digita tt-digita.pal-nro-docto
                   input browse br-digita tt-digita.pal-nr-seq-fat
                   input browse br-digita tt-digita.pal-largura
                   input browse br-digita tt-digita.pal-nr-bobinas   
                   input browse br-digita tt-digita.pal-peso-liq 
                   input browse br-digita tt-digita.pal-peso-bru.

            display
                tt-digita.marca
                tt-digita.pal-nr-pedido   
                tt-digita.pal-nr-pallet 
                tt-digita.pal-it-codigo 
                tt-digita.pal-cod-refer 
                tt-digita.pal-nro-docto 
                tt-digita.pal-nr-seq-fat
                tt-digita.pal-largura 
                tt-digita.pal-nr-bobinas    
                tt-digita.pal-peso-liq  
                tt-digita.pal-peso-bru 
                with browse br-digita. 
                                          
   end.
 
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/*
&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-relat
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
/*   {include/ajuda.i}   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
*/

&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-alterar w-relat
ON CHOOSE OF bt-alterar IN FRAME f-pg-dig /* Alterar */

DO:
    assign tt-digita.marca:screen-value in browse br-digita = " ".
    get current br-digita.
    assign input browse br-digita tt-digita.marca.       
    apply 'entry' to tt-digita.marca in browse br-digita. 

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


ON leave OF i-cod-emitente-ini IN FRAME f-pg-sel 
DO:

    FIND FIRST emitente WHERE
        emitente.cod-emitente = input frame f-pg-sel i-cod-emitente-ini
        NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN 
        ASSIGN c-nome-ab-cli:SCREEN-VALUE IN FRAME f-pg-sel = emitente.nome-abrev.
    ELSE
        ASSIGN c-nome-ab-cli:SCREEN-VALUE IN FRAME f-pg-sel = "NÇO EXISTE".

    RETURN.

END.




&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-paletes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-paletes w-relat
ON CHOOSE OF bt-paletes IN FRAME f-relat /* Carrega Paletes */
DO:
    CLOSE QUERY br-digita.
    FOR EACH tt-digita.
        DELETE tt-digita.
    END.
    
    FOR EACH tt-consulta.
        DELETE tt-consulta.
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

    assign tt-digita.marca:screen-value in browse br-digita = "*".
    get current br-digita.
    assign input browse br-digita tt-digita.marca.       
    apply 'entry' to tt-digita.marca in browse br-digita. 


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

    FOR EACH tt-digita.
        ASSIGN tt-digita.marca = "*".
    END.
    open query br-digita for each tt-digita .
    apply 'entry' to tt-digita.marca in browse br-digita. 

                             /*JR
    if  br-digita:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-digita.
        delete tt-digita.
        if  br-digita:delete-current-row() in frame f-pg-dig then.
    end.
                           JR  */
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

    FOR EACH tt-digita.
        ASSIGN tt-digita.marca = " ".
    END.
    open query br-digita for each tt-digita .
    apply 'entry' to tt-digita.marca in browse br-digita. 
   
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


ON VALUE-CHANGED OF rs-pedido
DO:

    IF rs-pedido:SCREEN-VALUE = "1" THEN DO:
        ASSIGN txtInsPedidos:READ-ONLY = TRUE
            txtApagar:READ-ONLY = TRUE
            c-nr-pedido-ini:READ-ONLY = FALSE
            c-nr-pedido-fim:READ-ONLY = FALSE.
    END.

    IF rs-pedido:SCREEN-VALUE = "2" THEN DO:
        ASSIGN txtInsPedidos:READ-ONLY = FALSE
            txtApagar:READ-ONLY = FALSE
            c-nr-pedido-ini:READ-ONLY = TRUE
            c-nr-pedido-fim:READ-ONLY = TRUE
            c-nr-pedido-ini:SCREEN-VALUE = "0"
            c-nr-pedido-fim:SCREEN-VALUE = "9999999"
            .
    END.

    
END.  


ON CHOOSE OF bt-ApagarTudo
DO:
    FOR EACH tt-pedidos:
        DELETE tt-pedidos.
    END.
    ASSIGN lstPedidos:SCREEN-VALUE = "".
    
END.


/*Pedidos*/
ON RETURN OF txtApagar
DO:
    FOR EACH tt-pedidos NO-LOCK WHERE tt-pedidos.Pedido = INTEGER(txtApagar:SCREEN-VALUE) :
        DELETE tt-pedidos.
    END.
    
    ASSIGN txtApagar:SCREEN-VALUE = "".
    ASSIGN lstPedidos:SCREEN-VALUE = "".

    FOR EACH tt-Pedidos NO-LOCK:
        ASSIGN lstPedidos:SCREEN-VALUE = string(lstPedidos:SCREEN-VALUE) + string(tt-pedidos.pedido) + CHR(13).
    END.
END.

ON RETURN OF txtInsPedidos
DO:
  FIND FIRST tt-pedidos NO-LOCK WHERE tt-pedidos.Pedido = integer(txtInsPedidos:SCREEN-VALUE) NO-ERROR.
  IF NOT AVAIL tt-pedidos THEN DO:
    CREATE tt-pedidos.
    ASSIGN tt-pedidos.Pedido = integer(txtInsPedidos:SCREEN-VALUE).
    ASSIGN lstPedidos:SCREEN-VALUE = "".
    ASSIGN txtInsPedidos:SCREEN-VALUE = "".
    FOR EACH tt-Pedidos NO-LOCK:
        ASSIGN lstPedidos:SCREEN-VALUE = string(lstPedidos:SCREEN-VALUE) + string(tt-pedidos.pedido) + CHR(13).
    END.
  END.
  ELSE
      ASSIGN txtInsPedidos:SCREEN-VALUE = "".
END. 

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

  
on LEAVE OF tt-digita.marca in browse br-digita do:
   run pi-leave-pallete.
end.


/* ***************************  Main Block  *************************** */


DEFINE VARIABLE c-computador AS CHARACTER FORMAT "x(20)" NO-UNDO.
DEFINE VARIABLE c-clientname AS CHARACTER FORMAT "x(20)" NO-UNDO.
    
{include/i-win.i}
RUN utp/ut-utils.p PERSISTENT SET h-prog.


RUN GetComputerName IN h-prog(OUTPUT c-computador).
run GetEnv in h-prog(input "CLIENTNAME", output c-clientname).

DELETE PROCEDURE h-prog.


ASSIGN c-cod-estabel-ini = "126".
    /*   c-serie       = "20".  */



/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "esft0032" "2.06.00.001"}

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
     c-cod-estabel-ini = STRING({cdp\poloestab.i 422}). /*solic-318*/ 
    

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
  ENABLE /*bt-executar*/ bt-cancelar bt-paletes c-arquivo-dest /*bt-ajuda*/ /*im-pg-cla*/ im-pg-dig /*im-pg-imp*/ 
        /* im-pg-par */im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  display  c-arquivo-dest t-arquivo-dest
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}

  DISPLAY c-cod-estabel-ini 
          c-it-codigo-ini   
          c-it-codigo-fim   
          c-nr-pedido-ini 
          c-nr-pedido-fim   
          c-nr-pallet-ini 
          c-nr-pallet-fim   
          c-largura-ini   
          c-largura-fim     
          c-cod-refer-ini 
          c-cod-refer-fim   
          c-nro-docto-ini 
          c-nro-docto-fim   
          i-cod-emitente-ini
          c-nome-ab-cli

          rs-pedido lstPedidos 
          txtInsPedidos bt-ApagarTudo
          txtApagar 

          IMAGE-1
          IMAGE-2
          IMAGE-3
          IMAGE-4
          IMAGE-47
          IMAGE-54
          IMAGE-55
          IMAGE-56
          IMAGE-86
          IMAGE-87   
          IMAGE-88
          IMAGE-89   

      WITH FRAME f-pg-sel IN WINDOW w-relat.

  ASSIGN lstPedidos:READ-ONLY    = TRUE
         txtInsPedidos:READ-ONLY = TRUE
         txtApagar:READ-ONLY     = TRUE.  

  ENABLE c-cod-estabel-ini 
         c-it-codigo-ini   
         c-it-codigo-fim   
         c-nr-pedido-ini   
         c-nr-pedido-fim   
         c-nr-pallet-ini   
         c-nr-pallet-fim   
         c-largura-ini     
         c-largura-fim     
         c-cod-refer-ini   
         c-cod-refer-fim   
         c-nro-docto-ini   
         c-nro-docto-fim   
         i-cod-emitente-ini
         rs-pedido lstPedidos 
         txtInsPedidos bt-ApagarTudo
         txtApagar
      
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

FOR EACH tt-notas.
    DELETE tt-notas.
END.
FOR EACH tt-lotes.
    DELETE tt-lotes.
END.
     for each saldo-terc no-lock
           where saldo-terc.cod-estabel  = INPUT FRAME f-pg-sel c-cod-estabel-ini and 
                 saldo-terc.cod-emitente = INT (INPUT FRAME f-pg-sel i-cod-emitente-ini) and 
                 saldo-terc.it-codigo  >=  INPUT FRAME f-pg-sel c-it-codigo-ini and 
                 saldo-terc.it-codigo  <=  INPUT FRAME f-pg-sel c-it-codigo-fim AND
                 saldo-terc.nro-docto  >=  INPUT FRAME f-pg-sel c-nro-docto-ini and 
                 saldo-terc.nro-docto  <=  INPUT FRAME f-pg-sel c-nro-docto-fim AND
                 saldo-terc.quantidade > 0
                 USE-INDEX estab-fornec .

         FIND FIRST tt-notas WHERE
             tt-notas.serie     = saldo-terc.serie     AND
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
                tt-notas.saldo        = saldo-terc.quantidade.
          
         /* rotina para encontrar os lotes enviados */
         assign v-num-reg-lidos = v-num-reg-lidos + 1.
         run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

         FOR EACH fat-ser-lote NO-LOCK WHERE
             fat-ser-lote.cod-estabel = saldo-terc.cod-estabel AND
             fat-ser-lote.serie       = saldo-terc.serie-docto AND
             fat-ser-lote.nr-nota-fis = saldo-terc.nro-docto   AND
             fat-ser-lote.nr-seq-fat  = saldo-terc.sequencia
             USE-INDEX ch-lote.

             FIND FIRST tt-lotes WHERE
                 tt-lotes.serie     = saldo-terc.serie     AND
                 tt-lotes.nro-docto = saldo-terc.nro-docto AND
                 tt-lotes.sequencia = saldo-terc.sequencia AND
                 tt-lotes.lote      = fat-ser-lote.nr-serlote
                 NO-LOCK NO-ERROR.

             IF NOT AVAIL tt-lotes THEN 
                CREATE tt-lotes.

             ASSIGN tt-lotes.nro-docto    = saldo-terc.nro-docto 
                    tt-lotes.serie        = saldo-terc.serie     
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
             componente.seq-comp     = tt-notas.sequencia 
             USE-INDEX comp.

             FOR EACH rat-lote NO-LOCK WHERE
                 rat-lote.serie-docto  = componente.serie-docto AND
                 rat-lote.nro-docto    = componente.nro-docto   AND
                 rat-lote.cod-emitente = tt-notas.cod-emitente  AND
                 rat-lote.sequencia    = componente.sequencia   AND
                 rat-lote.nat-oper     = componente.nat-oper.

                 FIND FIRST tt-lotes WHERE
                     tt-lotes.nro-docto = tt-notas.nro-docto AND
                     tt-lotes.serie     = tt-notas.serie     AND
                     tt-lotes.sequencia = tt-notas.sequencia AND
                     tt-lotes.lote      = rat-lote.lote
                     NO-LOCK NO-ERROR.

                 IF NOT AVAIL tt-lotes THEN 
                     CREATE tt-lotes.


                 ASSIGN tt-lotes.nro-docto    = tt-notas.nro-docto 
                        tt-lotes.serie        = tt-notas.serie     
                        tt-lotes.sequencia    = tt-notas.sequencia 
                        tt-lotes.lote         = rat-lote.lote
                        tt-lotes.cod-estabel  = tt-notas.cod-estabel
                        tt-lotes.cod-emitente = tt-notas.cod-emitente
                        tt-lotes.it-codigo    = rat-lote.it-codigo.

                 ASSIGN tt-lotes.retorno   = tt-lotes.retorno + rat-lote.quantidade
                        tt-lotes.saldo     = tt-lotes.remessa - tt-lotes.retorno.          

             END.
         END.
     END.

     FOR EACH tt-lotes NO-LOCK.

         IF tt-lotes.saldo = 0 THEN NEXT.

         FIND FIRST saldo-terc 
               where saldo-terc.cod-estabel  = tt-lotes.cod-estabel  AND 
                     saldo-terc.cod-emitente = tt-lotes.cod-emitente and 
                     saldo-terc.serie        = tt-lotes.serie        AND
                     saldo-terc.nro-docto    = tt-lotes.nro-docto    AND
                     saldo-terc.sequencia    = tt-lotes.sequencia    AND
                     saldo-terc.it-codigo    = tt-lotes.it-codigo
                     USE-INDEX documento. 

         IF NOT AVAIL saldo-terc THEN NEXT.

         FOR EACH pallet NO-LOCK WHERE
                pallet.cod-estabel = tt-lotes.cod-estabel and
                pallet.it-codigo = saldo-terc.it-codigo and 
                pallet.nr-pallet = tt-lotes.lote AND
                pallet.nr-pallet >= INPUT FRAME f-pg-sel c-nr-pallet-ini and 
                pallet.nr-pallet <= INPUT FRAME f-pg-sel c-nr-pallet-fim and 
                pallet.cod-refer >= INPUT FRAME f-pg-sel c-cod-refer-ini and 
                pallet.cod-refer <= INPUT FRAME f-pg-sel c-cod-refer-fim and 
                pallet.nr-pedido >= INT (INPUT FRAME f-pg-sel c-nr-pedido-ini) and 
                pallet.nr-pedido <= INT (INPUT FRAME f-pg-sel c-nr-pedido-fim)
                USE-INDEX ITEM ,

                each item NO-LOCK
                   where item.it-codigo = pallet.it-codigo 
                     USE-INDEX codigo:
            
                ASSIGN tem-pedido-jr = "N".
                
                IF INT (INPUT FRAME f-pg-sel rs-pedido) = 2 
                    THEN DO:

                    FOR EACH tt-pedidos NO-LOCK.
                        IF tt-pedidos.pedido = pallet.nr-pedido THEN
                            ASSIGN tem-pedido-jr = "S".
                    END.

                END.
            
                IF INT (INPUT FRAME f-pg-sel rs-pedido) = 2 
                   AND tem-pedido-jr = "N" THEN NEXT. 
              
              ASSIGN varSaldo   = 0
                     refer-jr   = ""
                     txtCliente = "". 

              ASSIGN varSaldo =  tt-lotes.saldo
                     refer-jr =  pallet.cod-refer.  


               IF varSaldo > 0 THEN DO:
                 FIND FIRST it-pallet WHERE 
                     it-pallet.nr-pallet   = pallet.nr-pallet AND
                     it-pallet.cod-estabel = pallet.cod-estabel AND
                     it-pallet.it-codigo   = pallet.it-codigo
                     NO-LOCK NO-ERROR.

                 IF AVAIL it-pallet THEN DO:
                     ASSIGN w-lote = it-pallet.lote-bobina.
                     ASSIGN w-item = it-pallet.it-codigo.
                 END.

                 FIND FIRST lote-carac-tec WHERE lote-carac-tec.it-codigo = w-item
                        AND lote-carac-tec.lote = w-lote 
                        AND lote-carac-tec.cd-comp   = "Diin" NO-LOCK NO-ERROR.
                    
                 IF AVAIL lote-carac-tec THEN
                         ASSIGN w-diin = INT (lote-carac-tec.vl-result).

                 FIND FIRST lote-carac-tec WHERE lote-carac-tec.it-codigo = w-item
                        AND lote-carac-tec.lote = w-lote 
                        AND lote-carac-tec.cd-comp   = "Diex" NO-LOCK NO-ERROR.
                    
                 IF AVAIL lote-carac-tec THEN
                    ASSIGN w-diex = INT (lote-carac-tec.vl-result).

                 ASSIGN w-larg = 0. 
                 IF pallet.cod-refer <> "" THEN DO:
                       FIND var-result USE-INDEX id WHERE 
                            var-result.item-cotacao  = pallet.it-codigo   AND
                            var-result.nr-estrut     = int(pallet.cod-refer)       AND
                            var-result.nome-var      = "LARGURA"  NO-LOCK NO-ERROR.
                       
                           IF AVAIL VAR-result THEN
                            ASSIGN w-larg = INT(var-result.des-result).
                 END.
                 
                     

                 assign v-num-reg-lidos = v-num-reg-lidos + 1.
                 run pi-acompanhar in h-acomp(input "Gerando Temp-Table Consulta: " + string(v-num-reg-lidos)).


                 IF w-larg < INT (INPUT FRAME f-pg-sel c-largura-ini) OR
                    w-larg > INT (INPUT FRAME f-pg-sel c-largura-fim)
                    THEN next.
                 
                   CREATE tt-Consulta.

                   ASSIGN tt-Consulta.it-codigo = pallet.it-codigo
                          tt-Consulta.nr-bobinas = pallet.nr-bobinas
                          tt-Consulta.nr-pallet = pallet.nr-pallet
                          tt-Consulta.peso-bruto = pallet.peso-bruto
                          tt-Consulta.peso-liquido = varSaldo
                          tt-Consulta.situacao = pallet.situacao
                          tt-Consulta.nr-pedido = pallet.nr-pedido
                          tt-Consulta.cod-depos = saldo-terc.cod-depos
                          tt-consulta.nro-docto = saldo-terc.nro-docto
                          tt-consulta.serie-docto = saldo-terc.serie-docto
                          tt-consulta.sequencia = saldo-terc.sequencia
                          tt-Consulta.ge-codigo = item.ge-codigo
                          tt-Consulta.cod-localiz = pallet.cod-localiz
                          tt-Consulta.recorte = substr(pallet.char-2,1,12)
                          tt-Consulta.cod-refer = refer-jr 
                          tt-Consulta.w-larg = w-larg
                          tt-Consulta.w-diin = w-diin
                          tt-Consulta.w-diex = w-diex
                          tt-Consulta.cliente = "".

                   /* itens sem referencia - largura = 0 */
                   IF refer-jr = "" THEN
                       ASSIGN tt-Consulta.w-larg = 0.      

                 ASSIGN w-larg = 0
                        w-diin = 0
                        w-diin = 0
                        w-lote = ""
                        w-item = "".

               END.
          END.
     END.

     assign v-num-reg-lidos = 0.
        
     for each tt-Consulta NO-LOCK 
         break BY tt-Consulta.nr-pedido
               BY tt-Consulta.cliente
               BY tt-consulta.it-codigo :

         assign v-num-reg-lidos = v-num-reg-lidos + 1.
         run pi-acompanhar in h-acomp(input "Gerando Montando Rel: " + string(v-num-reg-lidos)).
              
              FIND tt-digita WHERE
                   tt-digita.pal-nr-pedido = tt-consulta.nr-pedido     AND
                   tt-digita.pal-nr-pallet = tt-consulta.nr-pallet     AND
                   tt-digita.pal-nro-docto = tt-consulta.nro-docto     AND
                   tt-digita.pal-serie-docto = tt-consulta.serie-docto AND
                   tt-digita.pal-nr-seq-fat= tt-consulta.sequencia     
                   NO-ERROR.
            
              IF NOT AVAIL tt-digita THEN 
                   CREATE tt-digita.
            
              ASSIGN tt-digita.pal-nr-pedido = tt-consulta.nr-pedido 
                     tt-digita.pal-nr-pallet = tt-consulta.nr-pallet
                     tt-digita.marca = "*". 
            
              ASSIGN tt-digita.pal-largura = tt-consulta.w-larg
                     tt-digita.pal-diin    = tt-consulta.w-diin
                     tt-digita.pal-diex    = tt-consulta.w-diex.
            
              ASSIGN tt-digita.pal-it-codigo = tt-consulta.it-codigo
                     tt-digita.pal-cod-refer = tt-consulta.cod-refer
                     tt-digita.pal-qtd-plt   = 1
                     tt-digita.pal-nr-bobinas = tt-consulta.nr-bobinas
                     tt-digita.pal-peso-liq   = tt-consulta.peso-liquido
                     tt-digita.pal-peso-bru   = tt-consulta.peso-bruto.
            
              ASSIGN tt-digita.pal-nro-docto   = tt-consulta.nro-docto
                     tt-digita.pal-serie-docto = tt-consulta.serie-docto
                     tt-digita.pal-nr-seq-fat  = tt-consulta.sequencia.
             

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
          open query br-digita for each tt-digita .
          apply 'entry' to tt-digita.pal-nr-pallet in browse br-digita. 
       end.
    
    run pi-finalizar in h-acomp.
    enable bt-retirar bt-salvar bt-alterar bt-inserir with frame f-pg-dig.
    ENABLE bt-executar WITH FRAME f-relat.
   /* DISABLE bt-paletes WITH FRAME f-relat.*/
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
          tt-param.i-cod-emitente       = INPUT FRAME f-pg-sel i-cod-emitente-ini
          tt-param.rs-pedido            = input frame f-pg-sel rs-pedido
          tt-param.arquivo-dir        = input frame f-relat c-arquivo-dest.

    {include/i-rpexb.i}

    if  session:set-wait-state("general":U) then.
    
    {include/i-rprun.i ftp/esft0032rp.p} 
  
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

