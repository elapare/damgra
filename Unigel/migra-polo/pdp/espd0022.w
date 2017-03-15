&Scoped-define WINDOW-NAME C-Win

/*------------------------------------------------------------------------
File.............: espd0022.w
Description......: Alteraá∆o do N£mero do Pedido em Paletes em Terceiros
Input Parameters : 
Output Parameters: 
Author...........: Amgra - JosÇ Roberto
Created..........: 14/03/2008   
OBS..............: 
------------------------------------------------------------------------*/

def buffer if-ped-venda for espmulti.if-ped-venda.
define variable c-prog-gerado as character no-undo initial "espd0022".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

CREATE WIDGET-POOL. 

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */

&GLOBAL-DEFINE PGSEL f-pg-sel 
&GLOBAL-DEFINE PGPAR f-pg-par 
&GLOBAL-DEFINE PGDIG f-pg-dig 
&GLOBAL-DEFINE PGIMP f-pg-imp 

/* Include Com as Vari†veis Globais */

def new global shared var i-ep-codigo-usuario  like mgmulti.empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def new global shared var i-num-ped-exec-rpw  as integer no-undo.   
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
def new global shared var l-rpc as logical no-undo.
def new global shared var r-registro-atual as rowid no-undo.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var i-num-ped as integer no-undo.         
def new global shared var v_cdn_empres_usuar   like mgmulti.empresa.ep-codigo        no-undo.
def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab     as handle                   no-undo.
def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
def new global shared var v_num_tip_aces_usuar   as int                      no-undo.
def new global shared var rw-log-exec            as rowid                    no-undo.


/* Parameters Definitions ---                                           */ 


/* Temporary Table Definitions ---                                      */ 

define temp-table tt-raw-digita
    field raw-digita as raw.


define temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    field v_num_tip_aces_usuar as integer
    field ep-codigo            as character
    field cod-estabel          like ped-venda.cod-estabel 
    field c-it-codigo          AS CHAR
    field i-pd-origem          AS INT 
    field i-sq-origem          AS INT 
    field i-pd-destino         AS INT 
    field i-sq-destino         AS INT 
    field i-cod-emitente       AS INT.


define TEMP-TABLE tt-digita NO-UNDO
    FIELD lot-marca    AS CHAR     FORMAT "x(4)"          LABEL "Marca"
    FIELD cod-estabel  AS CHAR     FORMAT "x(4)"          LABEL "Est"
    FIELD nr-pedido    AS INT      FORMAT ">>>>>>>9"      LABEL "Pd.Orig"
    FIELD nr-sequencia AS INT      FORMAT ">>>9"          LABEL "Sq.Orig"
    FIELD nr-pallet    AS CHAR     FORMAT "x(30)"         LABEL "Pallet"
    FIELD it-codigo    AS CHAR     FORMAT "x(16)"         LABEL "Item"
    FIELD nr-bobinas   AS INT      FORMAT ">>>>>>9"       LABEL "Qt.Bobs"
    FIELD peso-bruto   AS DEC      FORMAT "->>>>>>>>9.99" LABEL "Peso Bruto"
    FIELD peso-liquido AS DEC      FORMAT "->>>>>>>>9.99" LABEL "Peso Liquido"
    FIELD situacao     AS INT      FORMAT ">>9"           LABEL "Sit"
    FIELD dt-pallet    AS DATE     FORMAT "99/99/9999"    LABEL "Dt.Palete"
    FIELD cod-refer    AS CHAR     FORMAT "x(10)"         LABEL "Referenc"
    FIELD w-larg       AS INT      FORMAT ">>>9"          LABEL "Larg"
    FIELD w-larg-dest  AS INT      FORMAT ">>>9"          LABEL "Larg.Dest"
    FIELD w-diin       AS INT      FORMAT ">>>9"          LABEL "Diin"
    FIELD w-diex       AS INT      FORMAT ">>>9"          LABEL "Diex"
    FIELD dt-vali      AS DATE     FORMAT "99/99/9999"    LABEL "Dt.Valid"
    INDEX codigo IS PRIMARY UNIQUE cod-estabel
                                   nr-pedido
                                   nr-sequencia
                                   nr-pallet
                                   it-codigo.

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

DEFINE TEMP-TABLE tt-itens NO-UNDO
    FIELD it-codigo    LIKE saldo-terc.it-codigo
    FIELD saldo        LIKE saldo-terc.quantidade
    FIELD cod-estabel  LIKE saldo-terc.cod-estabel
    FIELD cod-emitente LIKE saldo-terc.cod-emitente
    INDEX chave-itens  IS PRIMARY UNIQUE cod-estabel
                                         cod-emitente
                                         it-codigo.



define buffer b-tt-digita for tt-digita.
define Buffer bf-digita   For tt-digita.


/*DISABLE TRIGGERS FOR LOAD OF pallet. */


/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-tt-digita
   field raw-digita      as raw.
                    
                   

/* Local Variable Definitions ---                                       */ 

def var l-ok                 as logical no-undo. 
def var c-arq-digita         as char    no-undo. 
def var c-terminal           as char    no-undo. 
def var v-cod-pg-mouse-selec as char    no-undo. 
def var v-cod-prog-i-rprun   as char    no-undo. 
def var c-impressora-old     as char    no-undo. 
def var c-arquivo-old        as char    no-undo. 
def var c-destino-old        as char    no-undo. 
def var i-cont               as int     no-undo. 
def var v-cod-prog-gerado    as char    no-undo. 
def var v-cod-extens-arq     as char    no-undo initial "lst". 
DEFINE VARIABLE i-cor AS INTEGER     NO-UNDO.
def var h-acomp            as handle no-undo.
def var v-num-reg-lidos    as int    no-undo.


/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 

def var i-ordem        like ordem-compra.numero-ordem no-undo.
def var i-ordem-aux    like ordem-compra.numero-ordem no-undo.
def var i-ord-aux      like ordem-compra.nr-ord-orig  no-undo.
def var l-erro         as logical no-undo.
def var i-m-ordem      like ordem-compra.numero-ordem no-undo.
def var l-split       AS LOGICAL    NO-UNDO.
ASSIGN l-split = NO.

/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 

def new shared var c-cod-estabel     like ped-venda.cod-estabel   format "x(03)"          no-undo. /*solic-318*/ 
def new shared var c-it-codigo       AS CHAR                      format "x(16)"        initial ""      no-undo.
def new shared var i-pd-origem       AS INT                       format ">>>>>>>>9"    initial 0       no-undo.
def new shared var i-sq-origem       AS INT                       format ">>>>9"        initial 10      no-undo.
def new shared var i-pd-destino      AS INT                       format ">>>>>>>>9"    initial 0       no-undo.
def new shared var i-sq-destino      AS INT                       format ">>>>9"        initial 10      no-undo.
def new shared var i-cod-emitente    AS INT                       format ">>>>>>>>9"    initial 17261   no-undo.

DEFINE VARIABLE it-codigo-jr         AS CHARACTER                                   NO-UNDO.
DEFINE VARIABLE c-nome-abrev         AS CHARACTER  FORMAT "x(12)" INITIAL "TRPAULO" NO-UNDO.
DEFINE VARIABLE c-nome-cliente       AS CHARACTER  FORMAT "x(12)"                   NO-UNDO.

DEFINE VARIABLE i-pd-origem-ed       AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-pd-origem-jr       AS INTEGER    NO-UNDO.

DEFINE VARIABLE i-sq-origem-jr       AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-pd-destino-jr      AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-sq-destino-jr      AS INTEGER    NO-UNDO.

DEFINE VARIABLE item-origem-jr       AS CHARACTER  NO-UNDO.


DEFINE VARIABLE largura-jr           AS INTEGER    NO-UNDO.
DEFINE VARIABLE largura-ped          AS INTEGER    NO-UNDO.
DEFINE VARIABLE erro-larg            AS INTEGER    NO-UNDO.

DEFINE VARIABLE diex-orig-ed           AS INTEGER    NO-UNDO.
DEFINE VARIABLE diex-dest-ed           AS INTEGER    NO-UNDO.
DEFINE VARIABLE diin-orig-ed           AS INTEGER    NO-UNDO.
DEFINE VARIABLE diin-dest-ed           AS INTEGER    NO-UNDO.

DEFINE VARIABLE erro-diex              AS INTEGER    NO-UNDO.
DEFINE VARIABLE erro-diin              AS INTEGER    NO-UNDO.





DEFINE VARIABLE varCli LIKE ped-venda.nome-abrev.
DEFINE VARIABLE varSaldo AS decimal.
DEFINE VARIABLE w-lote LIKE it-pallet.lote-bobina.
DEFINE VARIABLE w-item LIKE it-pallet.it-codigo.
DEFINE VARIABLE w-larg LIKE lote-carac-tec.vl-result.
DEFINE VARIABLE w-diin LIKE lote-carac-tec.vl-result.
DEFINE VARIABLE w-diex LIKE lote-carac-tec.vl-result.
DEFINE VARIABLE refer-jr LIKE saldo-estoq.cod-refer.

/* ********************  Preprocessor Definitions  ******************** */ 

&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita */

&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.lot-marca tt-digita.cod-estabel tt-digita.nr-pedido tt-digita.nr-pallet tt-digita.it-codigo tt-digita.nr-bobinas tt-digita.peso-bruto tt-digita.peso-liquido tt-digita.situacao tt-digita.dt-pallet tt-digita.cod-refer tt-digita.w-larg tt-digita.w-larg-dest tt-digita.w-diin tt-digita.w-diex tt-digita.dt-vali 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.lot-marca tt-digita.cod-estabel tt-digita.nr-pedido tt-digita.nr-pallet tt-digita.it-codigo tt-digita.nr-bobinas tt-digita.peso-bruto tt-digita.peso-liquido tt-digita.situacao tt-digita.dt-pallet tt-digita.cod-refer tt-digita.w-larg tt-digita.w-larg-dest tt-digita.w-diin tt-digita.w-diex tt-digita.dt-vali 
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

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */ 

DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO. 


DEFINE BUTTON bt-arquivo
    IMAGE-UP FILE "image\im-sea"
    IMAGE-INSENSITIVE FILE "image\ii-sea"
    LABEL " "
    SIZE 4 BY 1.
    
DEFINE BUTTON bt-config-impr
    IMAGE-UP FILE "image\im-cfprt"
    LABEL " "
    SIZE 4 BY 1.
   
DEFINE RECTANGLE RECT-20
         EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
         SIZE 28 BY 4.5.


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
    
DEFINE IMAGE im-pg-imp
    FILENAME "image\im-fldup"
    SIZE 15.72 BY 1.19.
    
DEFINE IMAGE im-pg-par
    FILENAME "image\im-fldup"
    SIZE 15.72 BY 1.19.
    
DEFINE IMAGE im-pg-sel
    FILENAME "image\im-fldup"
    SIZE 15.72 BY 1.19.

DEFINE IMAGE im-pg-dig
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.19.


DEFINE VARIABLE c-arquivo AS CHARACTER 
VIEW-AS EDITOR MAX-CHARS 256 
SIZE 40 BY 1.00 
BGCOLOR 15  font 2 NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)" INITIAL "Destino"
VIEW-AS TEXT 
SIZE 8.57 BY .62 NO-UNDO. 

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)" INITIAL "Execuá∆o"
VIEW-AS TEXT 
SIZE 10.86 BY .62 NO-UNDO.

DEFINE VARIABLE text-parametro AS CHARACTER FORMAT "X(256)" INITIAL "ParÉmetros de Impress∆o"
VIEW-AS TEXT 
SIZE 24.72 BY .62 NO-UNDO.


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

DEFINE VARIABLE tb-parametro AS LOGICAL INITIAL no 
LABEL "Imprimir P†gina de ParÉmetros"
VIEW-AS TOGGLE-BOX 
SIZE 32 BY .83 
NO-UNDO.


DEFINE VARIABLE tb-largura AS LOGICAL INITIAL YES
LABEL "N∆o mostrar Larguras diferentes"
VIEW-AS TOGGLE-BOX 
SIZE 32 BY .83 
NO-UNDO.


DEFINE VARIABLE rs-formato AS INTEGER INITIAL 2 
VIEW-AS RADIO-SET HORIZONTAL 
RADIO-BUTTONS 
"80 colunas", 1,
"132 colunas", 2
SIZE 32 BY .92 NO-UNDO.


DEFINE BUTTON bt-arquivo-saida 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo-entrada AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-saida AS CHARACTER FORMAT "X(256)":U INITIAL "Arquivo de Entrada" 
      VIEW-AS TEXT 
     SIZE 19 BY .63 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.83.

DEFINE RECTANGLE RECT-7 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 46.29 BY 1.69.

DEFINE RECTANGLE RECT-10 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 46.29 BY 3.50.

DEFINE RECTANGLE RECT-22
         EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
         SIZE 15 BY 2.3.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 25.29 BY 2.30.

DEFINE VARIABLE l-param-1 AS LOGICAL INITIAL no 
LABEL "ParÉmetro 1"
VIEW-AS TOGGLE-BOX 
SIZE 44 BY 1.08 NO-UNDO. 

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
LABEL "Paletes"
SIZE 10 BY 1.

DEFINE BUTTON bt-marca 
     LABEL "Marca" 
     SIZE 12 BY 1.

DEFINE BUTTON bt-desmarca 
     LABEL "Desmarca" 
     SIZE 12 BY 1.

DEFINE BUTTON bt-marca-todos 
     LABEL "Marca Todos" 
     SIZE 12 BY 1.

DEFINE BUTTON bt-desmarca-todos 
     LABEL "Desmarca Todos" 
     SIZE 12 BY 1.


DEFINE BUTTON bt-retirar 
     LABEL "Retira Desmarcados" 
     SIZE 14.2 BY 1. 

DEFINE BUTTON bt-gera-ordem 
     LABEL "Tranf.Plt.Marcados" 
     SIZE 14 BY 1. 


DEFINE RECTANGLE RECT-1
EDGE-PIXELS 2 GRAPHIC-EDGE 
SIZE 79 BY 1.42 
BGCOLOR 7.

DEFINE RECTANGLE RECT-6
EDGE-PIXELS 0
SIZE 78.72 BY .12
BGCOLOR 7.

DEFINE RECTANGLE rt-folder
EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL
SIZE 79 BY 11.38
FGCOLOR 0.

DEFINE RECTANGLE rt-folder-left
EDGE-PIXELS 0
SIZE .43 BY 11.19
BGCOLOR 15.

DEFINE RECTANGLE rt-folder-right
EDGE-PIXELS 0
SIZE .43 BY 11.15
BGCOLOR 7.

DEFINE RECTANGLE rt-folder-top
EDGE-PIXELS 0
SIZE 78.72 BY .12
BGCOLOR 15 .



/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-digita SCROLLING.
&ANALYZE-RESUME


/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita C-Win _FREEFORM
  QUERY br-digita DISPLAY
      tt-digita.lot-marca
      tt-digita.cod-estabel  
      tt-digita.nr-pedido    
      tt-digita.nr-pallet       width 16
      tt-digita.it-codigo    
      tt-digita.w-larg       
      tt-digita.w-larg-dest
      tt-digita.nr-bobinas   
      tt-digita.peso-bruto   
      tt-digita.peso-liquido 
      tt-digita.situacao     
      tt-digita.dt-vali      
      tt-digita.cod-refer    
      tt-digita.w-diin       
      tt-digita.w-diex       
      tt-digita.dt-pallet    
/*
  ENABLE
    tt-digita.cod-estabel 
*/    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9
         BGCOLOR 15 .




/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
    bt-executar AT ROW 14.54 COL 3 HELP
"Executar"
    bt-paletes  AT ROW 14.54 COL 3 HELP
    " Encontra Paletes em Terceiros"
     bt-cancelar AT ROW 14.54 COL 25 HELP
"Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
"Ajuda"
     im-pg-sel AT ROW 1.5 COL 2.14
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-dig AT ROW 1.5 COL 33.57

     im-pg-imp AT ROW 1.5 COL 49.29
     "        "AT ROW 1.8 COL 66.00
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.65 COL 80.43
     RECT-6 AT ROW 13.73 COL 2.14
     RECT-1 AT ROW 14.31 COL 2
     WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
     SIDE-LABELS NO-UNDERLINE THREE-D
     AT COL 1 ROW 1
     SIZE 81 BY 15
     DEFAULT-BUTTON bt-paletes.

DEFINE FRAME f-pg-imp
    text-destino AT ROW 1.62 COL 3.86 NO-LABEL
    rs-destino AT ROW 2.38 COL 3.29 HELP
    "Destino de Impress∆o do Relat¢rio" NO-LABEL
    bt-arquivo AT ROW 3.58 COL 43.29 HELP
    "Escolha do nome do arquivo"
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
    "Configuraá∆o da impressora"
     c-arquivo AT ROW 3.56 COL 3.29 HELP
    "Nome do arquivo de destino do relat¢rio" NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     rs-execucao AT ROW 5.77 COL 3 HELP
    "Modo de Execuá∆o" NO-LABEL
     tb-parametro AT ROW 7.92 COL 3.2
     rs-formato AT ROW 8.8 COL 3 HELP
    "Formato de Impress∆o" NO-LABEL
     text-parametro AT ROW 7.17 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.31 COL 2.14
     RECT-10 AT ROW 7.46 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
    SIDE-LABELS NO-UNDERLINE THREE-D
    AT COL 3 ROW 3
    SIZE 73.72 BY 10.


DEFINE FRAME f-pg-sel  

    c-cod-estabel label "Estabelecimento"
      at row 1 col 16 colon-aligned
      view-as fill-in 
      size 4 by .88
      font 1

    i-cod-emitente label "Cd.Transport."
      at row 3 col 16 colon-aligned
      view-as fill-in 
      size 9 by .88
      font 1

    c-nome-abrev   NO-LABEL
      at row 3 col 42 colon-aligned
      view-as fill-in 
      size 30 by .88
      font 1

    i-pd-origem label "Pedido Origem"
      at row 5 col 16 colon-aligned
      view-as fill-in 
      size 9 by .88
      font 1

    i-sq-origem label "Seq"
      at row 5 col 29 colon-aligned
      view-as fill-in 
      size 5 by .88
      font 1

    c-it-codigo label "Cod.Item"
      at row 5 col 42 colon-aligned
      view-as fill-in 
      size 16 by .88
      font 1

    i-pd-destino label "Pedido Destino"
      at row 6 col 16 colon-aligned
      view-as fill-in 
      size 9 by .88
      font 1

    i-sq-destino label "Seq"
      at row 6 col 29 colon-aligned
      view-as fill-in 
      size 5 by .88
      font 1

    c-nome-cliente   NO-LABEL
      at row 6 col 42 colon-aligned
      view-as fill-in 
      size 30 by .88
      font 1
    
    tb-largura
     at row 8 col 16 colon-aligned
         font 1


   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D
   AT COL 3 ROW 2.85
   SIZE 76.86 BY 10.62.



DEFINE FRAME f-pg-dig  

    br-digita AT ROW 1 COL 1

    bt-marca    AT ROW 10 COL 01

    bt-desmarca AT ROW 10 COL 13

    bt-marca-todos AT ROW 10 COL 25

    bt-desmarca-todos AT ROW 10 COL 37

    bt-retirar AT ROW 10 COL 49.2
   
    bt-gera-ordem AT ROW 10 COL 63.6

   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D
   AT COL 3 ROW 3.31 /*2.85*/
   SIZE 76.86 BY 10.15.

DEFINE RECTANGLE ret-par-fill
   EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
   SIZE  74.06 BY .3.

DEFINE FRAME f-pg-par

   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D
   AT COL 3 ROW 3
   SIZE 75 BY 10.

/* ******** Acerto da posiá∆o dos labels e tamanho dos radio-set ******* */

DEFINE VARIABLE h-label AS WIDGET-HANDLE NO-UNDO.

/* *************************  Create Window  ************************** */

CREATE WINDOW C-Win ASSIGN
   HIDDEN             = YES
   TITLE              = "Alteraá∆o do N£mero do Pedido em Paletes em Terceiros - espd0022"
   HEIGHT             = 15
   WIDTH              = 81.14
   MAX-HEIGHT         = 22.35
   MAX-WIDTH          = 114.29
   VIRTUAL-HEIGHT     = 22.35
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

/* ***************  Runtime Attributes and UIB Settings  ************** */

ASSIGN FRAME f-pg-imp:FRAME = FRAME f-relat:HANDLE
       FRAME f-pg-par:FRAME = FRAME f-relat:HANDLE
       FRAME f-pg-dig:FRAME = FRAME f-relat:HANDLE
       FRAME f-pg-sel:FRAME = FRAME f-relat:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI" AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* ************************* Included-Libraries *********************** */


define variable wh-pesquisa             as handle               no-undo.
define variable c-arq-old               as char                 no-undo.

def new shared var c-nom-prog-upc-mg97  as char format "x(50)" no-undo.
def new shared var c-nom-prog-appc-mg97 as char format "x(50)" no-undo.
def new shared var c-nom-prog-dpc-mg97  as char format "x(50)" no-undo.
def new shared var c-programa-mg97      as char format "x(08)" no-undo.
def new shared var c-versao-mg97        as char format "x(08)" no-undo.

define new shared variable c-imp-old               as char                 no-undo.
define new shared variable c-arq-old-batch         as char                 no-undo.


assign frame f-relat:visible = no
       frame f-relat:font = 1.
&IF "{&PGSEL}" <> "" &THEN
    assign frame f-pg-sel:visible = no
           frame f-pg-sel:font = 1.
    &IF "{&botao}" <> "no" &THEN
        ON  GO OF frame {&pgsel} ANYWHERE DO:    
            if  self:type <> "editor" 
                or (self:type = "editor" 
                and keyfunction(lastkey) <> "RETURN") then do: 
                apply "choose" to bt-executar in frame f-relat.
            end.
        END.              
    &ENDIF  
&ENDIF
&IF "{&PGCLA}" <> "" &THEN
    assign frame f-pg-cla:visible = no
           frame f-pg-cla:font = 1.
    &IF "{&botao}" <> "no" &THEN
        ON  GO OF frame {&pgcla} ANYWHERE DO:    
            if  self:type <> "editor" 
                or (self:type = "editor" 
                and keyfunction(lastkey) <> "RETURN") then do: 
                apply "choose" to bt-executar in frame f-relat.
            end.
        END.              
    &ENDIF  
&ENDIF
&IF "{&PGPAR}" <> "" &THEN
    assign frame f-pg-par:visible = no
           frame f-pg-par:font = 1.
    &IF "{&botao}" <> "no" &THEN
        ON  GO OF frame {&pgpar} ANYWHERE DO:    
            if  self:type <> "editor" 
                or (self:type = "editor" 
                and keyfunction(lastkey) <> "RETURN") then do: 
                apply "choose" to bt-executar in frame f-relat.
            end.
        END.              
    &ENDIF  
&ENDIF
&IF "{&PGDIG}" <> "" &THEN
    assign frame f-pg-dig:visible = no
           frame f-pg-dig:font = 1.
    &IF "{&botao}" <> "no" &THEN
        ON  GO OF frame {&pgdig} ANYWHERE DO:    
            if  self:type <> "editor" 
                or (self:type = "editor" 
                and keyfunction(lastkey) <> "RETURN") then do: 
                apply "choose" to bt-executar in frame f-relat.
            end.
        END.              
    &ENDIF  
&ENDIF
&IF "{&PGIMP}" <> "" &THEN
    assign frame f-pg-imp:visible = no
           frame f-pg-imp:font = 1.
    &IF "{&botao}" <> "no" &THEN
        ON  GO OF frame {&pgimp} ANYWHERE DO:    
            if  self:type <> "editor" 
                or (self:type = "editor" 
                and keyfunction(lastkey) <> "RETURN") then do: 
                apply "choose" to bt-executar in frame f-relat.
            end.
        END.              
    &ENDIF  
&ENDIF



PROCEDURE pi-trata-state :
  DEFINE INPUT PARAMETER p-issuer-hdl   AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state        AS CHAR   NO-UNDO.
  
  case entry(1, p-state, '|'):
  end.
END PROCEDURE.

on "close" of this-procedure do:
    run btb/btb918za.p (input no).
    run disable_UI.
end.

/* ************************  Control Triggers  ************************ */

ON LEAVE OF i-cod-emitente IN FRAME f-pg-sel
DO:
    
     FIND FIRST emitente WHERE
         emitente.cod-emitente = INT (i-cod-emitente:SCREEN-VALUE IN FRAME f-pg-sel)
         NO-LOCK NO-ERROR.

     IF AVAIL emitente THEN
         ASSIGN c-nome-abrev:SCREEN-VALUE IN FRAME f-pg-sel = string(emitente.nome-abrev).
     ELSE 
         ASSIGN c-nome-abrev:SCREEN-VALUE IN FRAME f-pg-sel = "".

END.


ON LEAVE OF i-sq-destino IN FRAME f-pg-sel
DO:
    
     FIND FIRST ped-venda WHERE
         ped-venda.nr-pedido = INT (i-pd-destino:SCREEN-VALUE IN FRAME f-pg-sel)
         NO-LOCK NO-ERROR.

     IF AVAIL ped-venda THEN DO:

        FIND FIRST ped-item OF ped-venda WHERE
            ped-item.nr-sequencia = INT (i-sq-destino:SCREEN-VALUE IN FRAME f-pg-sel) AND
            ped-item.ind-componen  <> 3 
            NO-LOCK NO-ERROR.

        IF AVAIL ped-item THEN DO:

          IF c-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = "" THEN
            ASSIGN c-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = string(ped-item.it-codigo)
                   c-it-codigo:SENSITIVE = NO.

           FIND FIRST emitente WHERE
               emitente.cod-emitente = ped-venda.cod-emitente
               NO-LOCK NO-ERROR.
           
           IF AVAIL emitente THEN 
               ASSIGN c-nome-cliente:SCREEN-VALUE IN FRAME f-pg-sel = string(emitente.nome-abrev).
           ELSE 
               ASSIGN c-nome-cliente:SCREEN-VALUE IN FRAME f-pg-sel = "".

        END.

     END.

     ELSE
         ASSIGN c-nome-cliente:SCREEN-VALUE IN FRAME f-pg-sel = "".

END.

ON LEAVE OF i-sq-origem IN FRAME f-pg-sel
DO:
    
     FIND FIRST ped-venda WHERE
         ped-venda.nr-pedido = INT (i-pd-origem:SCREEN-VALUE IN FRAME f-pg-sel)
         NO-LOCK NO-ERROR.

     IF AVAIL ped-venda THEN DO: 

       FIND FIRST ped-item OF ped-venda WHERE
           ped-item.nr-sequencia = INT (i-sq-origem:SCREEN-VALUE IN FRAME f-pg-sel) AND
           ped-item.ind-componen  <> 3 
           NO-LOCK NO-ERROR.
       
       IF AVAIL ped-item THEN 
           ASSIGN c-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = string(ped-item.it-codigo)
                  c-it-codigo:SENSITIVE = NO
                  .
       ELSE 
           ASSIGN c-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = ""
                  c-it-codigo:SENSITIVE = YES.

     END.
     ELSE 
         ASSIGN c-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = ""
                c-it-codigo:SENSITIVE = YES .

END.

&Scoped-define BROWSE-NAME br-digita
&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON DEL OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose' to bt-retirar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON ROW-DISPLAY OF br-digita IN FRAME f-pg-dig
DO:
     RUN pi-changeRowColor(INPUT SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON ROW-ENTRY OF br-digita IN FRAME f-pg-dig
DO:
   /* trigger para inicializar campos da temp table de digitaá∆o */
/*   if  br-digita:new-row in frame f-pg-dig then do:
       assign tt-digita.nome:screen-value in browse br-digita = string(today, "99/99/9999").
   end.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
DO:
    /* ê aqui que a gravaá∆o da linha da temp-table ? efetivada.
       PorÇm as validaá‰es dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment†rio */
    
    if br-digita:NEW-ROW in frame f-pg-dig then 
    do transaction on error undo, return no-apply:
        create tt-digita.
        assign input browse br-digita tt-digita.lot-marca
               input browse br-digita tt-digita.cod-estabel   
               INPUT BROWSE br-digita tt-digita.nr-pedido     
               input browse br-digita tt-digita.nr-pallet     
               input browse br-digita tt-digita.it-codigo     
               input browse br-digita tt-digita.nr-bobinas    
               input browse br-digita tt-digita.peso-bruto    
               input browse br-digita tt-digita.peso-liquido  
               input browse br-digita tt-digita.situacao      
               INPUT BROWSE br-digita tt-digita.dt-pallet     
               input browse br-digita tt-digita.cod-refer     
               input browse br-digita tt-digita.w-larg        
               input browse br-digita tt-digita.w-larg-dest
               input browse br-digita tt-digita.w-diin        
               input browse br-digita tt-digita.w-diex        
               input browse br-digita tt-digita.dt-vali  .     
            
            br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    end.
    else do transaction on error undo, return no-apply:
        
            assign input browse br-digita tt-digita.lot-marca
                   input browse br-digita tt-digita.cod-estabel  
                   INPUT BROWSE br-digita tt-digita.nr-pedido    
                   input browse br-digita tt-digita.nr-pallet    
                   input browse br-digita tt-digita.it-codigo    
                   input browse br-digita tt-digita.nr-bobinas   
                   input browse br-digita tt-digita.peso-bruto   
                   input browse br-digita tt-digita.peso-liquido 
                   input browse br-digita tt-digita.situacao     
                   INPUT BROWSE br-digita tt-digita.dt-pallet    
                   input browse br-digita tt-digita.cod-refer    
                   input browse br-digita tt-digita.w-larg       
                   input browse br-digita tt-digita.w-larg-dest
                   input browse br-digita tt-digita.w-diin       
                   input browse br-digita tt-digita.w-diex       
                   input browse br-digita tt-digita.dt-vali  .   
                
            display
                tt-digita.lot-marca
                tt-digita.cod-estabel  
                tt-digita.nr-pedido    
                tt-digita.nr-pallet          
                tt-digita.it-codigo    
                tt-digita.nr-bobinas   
                tt-digita.peso-bruto   
                tt-digita.peso-liquido 
                tt-digita.situacao     
                tt-digita.dt-vali      
                tt-digita.cod-refer    
                tt-digita.w-larg       
                tt-digita.w-larg-dest
                tt-digita.w-diin       
                tt-digita.w-diex       
                tt-digita.dt-pallet    
                with browse br-digita. 
                                          
   end.
 
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
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
        DISPLAY 
            tt-digita.lot-marca
            tt-digita.cod-estabel  
            tt-digita.nr-pedido    
            tt-digita.nr-pallet           
            tt-digita.it-codigo    
            tt-digita.nr-bobinas   
            tt-digita.peso-bruto   
            tt-digita.peso-liquido 
            tt-digita.situacao     
            tt-digita.dt-vali      
            tt-digita.cod-refer    
            tt-digita.w-larg       
            tt-digita.w-larg-dest
            tt-digita.w-diin       
            tt-digita.w-diex       
            tt-digita.dt-pallet    
            with browse br-digita. 

    end.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita c-win
ON ENTER OF br-digita IN FRAME f-pg-dig
ANYWHERE
DO:
  apply 'tab' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca w-relat
ON CHOOSE OF bt-marca IN FRAME f-pg-dig /* Marca */
DO:

    assign tt-digita.lot-marca:screen-value in browse br-digita = " ***".
    get current br-digita.
    assign input browse br-digita tt-digita.lot-marca.       
    apply 'entry' to tt-digita.lot-marca in browse br-digita. 


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-desmarca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desmarca w-relat
ON CHOOSE OF bt-desmarca IN FRAME f-pg-dig /* Desmarca */
DO:
    assign tt-digita.lot-marca:screen-value in browse br-digita = " ".
    get current br-digita.
    assign input browse br-digita tt-digita.lot-marca.       
    apply 'entry' to tt-digita.lot-marca in browse br-digita. 

end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-marca-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca-todos w-relat
ON CHOOSE OF bt-marca-todos IN FRAME f-pg-dig /* Marca todos */
DO:

    FOR EACH tt-digita.
        ASSIGN tt-digita.lot-marca = " ***".
    END.
    open query br-digita for each tt-digita.
    apply 'entry' to tt-digita.lot-marca in browse br-digita. 


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desmarca-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desmarca-todos w-relat
ON CHOOSE OF bt-desmarca-todos IN FRAME f-pg-dig /* desmarca-todos */
DO:

    FOR EACH tt-digita.
        ASSIGN tt-digita.lot-marca = " ".
    END.
    open query br-digita for each tt-digita.
    apply 'entry' to tt-digita.lot-marca in browse br-digita. 
   
END.

/* _UIB-CODE-BLOCK-END */


&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-retirar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retirar w-relat
ON CHOOSE OF bt-retirar IN FRAME f-pg-dig /* Retirar */
DO:

    FOR EACH tt-digita WHERE
        tt-digita.lot-marca = " ".

        DELETE tt-digita.

    END.

    open query br-digita for each tt-digita.
    apply 'entry' to tt-digita.lot-marca in browse br-digita. 
   
END.

/* _UIB-CODE-BLOCK-END */


&ANALYZE-RESUME



&Scoped-define SELF-NAME bt-gera-ordem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-gera-ordem C-Win
ON CHOOSE OF bt-gera-ordem IN FRAME f-pg-dig /* Gera Ordem de Compra */
DO:

    ASSIGN erro-larg = 0.

    for each tt-digita WHERE 
             tt-digita.lot-marca = " ***" no-lock:


        FIND FIRST pallet  WHERE
            pallet.cod-estabel = tt-digita.cod-estabel AND
            pallet.it-codigo   = tt-digita.it-codigo   AND
            pallet.nr-pallet   = tt-digita.nr-pallet and
            pallet.situacao    = 2
            exclusive-LOCK NO-ERROR.

        IF AVAIL pallet THEN DO:

            /*verifica se o pedido vai aguentar ou se est† alocado*/

                  IF pallet.nr-pedido <> 0 AND pallet.nr-pedido <> i-pd-destino-jr  THEN DO: /*verifica alocaá∆o*/
                      run pdp\espd0025-1.p (input pallet.nr-pedido,
                                          INPUT pallet.nr-sequencia,
                                          INPUT pallet.peso-liquido,     /*quantidade*/
                                          INPUT pallet.it-codigo,
                                          INPUT pallet.nr-pallet,
                                          INPUT 0,  /*largura*/
                                          INPUT 0,  /*diex*/                                          
                                          INPUT 0,  /*diin*/                                          
                                          INPUT "retira").

                      IF RETURN-VALUE = "nok" THEN
                         RETURN NO-APPLY.
                  END.


                  IF i-pd-destino-jr <> 0 THEN DO:   /*verifica se cabe mais paletes com relaá∆o a % max de faturamento do pedido final*/

                      run pdp\espd0025-1.p (input i-pd-destino-jr,
                                          INPUT  i-sq-destino-jr,
                                          INPUT pallet.peso-liquido,     /*quantidade*/                                          
                                          INPUT pallet.it-codigo,
                                          INPUT pallet.nr-pallet,
                                          INPUT tt-digita.w-larg,  /*largura*/
                                          INPUT tt-digita.w-diex,  /*diex*/                                          
                                          INPUT tt-digita.w-diin,  /*diin*/                                          
                                          INPUT "insere").

                     IF RETURN-VALUE = "nok" THEN
                         RETURN NO-APPLY.
                  END.
   
        END.



     /* 
        IF i-pd-destino-jr > 0 AND 
           largura-ped <> tt-digita.w-larg THEN DO:


            /* Dialog mensagem erro de largura */

            DEFINE BUTTON db-bt-cancel2 AUTO-END-KEY 
                 LABEL "&N∆o Continua" 
                 SIZE 12 BY 1
                 BGCOLOR 8.
    
            DEFINE BUTTON db-bt-ok2 AUTO-GO 
                 LABEL "&Continua" 
                 SIZE 12 BY 1
                 BGCOLOR 8.
    

            DEFINE RECTANGLE db-rt-botoes2
                 EDGE-PIXELS 2 GRAPHIC-EDGE  
                 SIZE 58 BY 1.42
                 BGCOLOR 7.  

            DEFINE VARIABLE c-mensagem2 AS CHARACTER FORMAT "X(60)" NO-UNDO.

            ASSIGN c-mensagem2 = "Pallet: " + TRIM(tt-digita.nr-pallet) + " com Largura <> do Pedido Destino." .

            DEFINE RECTANGLE db-rect-2
             EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
             SIZE 60 BY 3.30.

            DEFINE FRAME db-frame-2

                c-mensagem2 NO-LABEL 
                   at ROW 3 col 10 

                db-rect-2 AT ROW 1.9 COL 5

                db-bt-ok2          AT ROW 7.3 COL 2.14
                db-bt-cancel2      AT ROW 7.3 COL 15             

                db-rt-botoes2      AT ROW 7.0 COL 1
                SPACE(0.28)
                WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
                     THREE-D SCROLLABLE TITLE "Caixa de dialogo" FONT 1
                     DEFAULT-BUTTON db-bt-cancel2 CANCEL-BUTTON db-bt-cancel2.


            DISPLAY c-mensagem2 WITH FRAME db-frame-2.

            ON "CHOOSE":U OF db-bt-cancel2 IN FRAME db-frame-2 DO:
                ASSIGN erro-larg = 9.
             RETURN.
            END.

            ON "CHOOSE":U OF db-bt-ok2 IN FRAME db-frame-2 DO:
/*valida mais abaixo qt e largura
               FIND ext_usuar_grp_usuar WHERE
                          ext_usuar_grp_usuar.cod_grp     = "LARGDIF" AND
                          ext_usuar_grp_usuar.cod_usuario = c-seg-usuario
                          NO-LOCK NO-ERROR.
                  
               if  not avail ext_usuar_grp_usuar  then 
               DO:
                      RUN utp/ut-msgs.p (INPUT ("show"),INPUT 18728,
                                           INPUT "colocar Pallet com largura diferente no pedido").
                              
                            ASSIGN erro-larg = 9.
               END.

  */

             RETURN.
            END.


            ENABLE db-bt-ok2 db-bt-cancel2 
                WITH FRAME db-frame-2. 

            WAIT-FOR "GO":U OF FRAME db-frame-2.

            /* fim Dialog mensagem erro de largura */


        END.*/

    END.

    IF erro-larg <> 0 THEN RETURN NO-APPLY.


    /* Transfere os pallets para o novo pedido */

     

    FOR EACH tt-digita where
        tt-digita.lot-marca = " ***" 
        EXCLUSIVE-LOCK.

        FIND FIRST pallet  WHERE
            pallet.cod-estabel = tt-digita.cod-estabel AND
            pallet.it-codigo   = tt-digita.it-codigo   AND
            pallet.nr-pallet   = tt-digita.nr-pallet and
            pallet.situacao    = 2
            exclusive-LOCK NO-ERROR.

        IF AVAIL pallet THEN DO:

            ASSIGN pallet.nr-pedido    = i-pd-destino-jr
                   pallet.nr-sequencia = i-sq-destino-jr. 

            IF i-pd-origem-jr = 0 AND i-pd-destino-jr <> 0 THEN DO:

                FIND FIRST am-pd-alocacao WHERE
                    am-pd-alocacao.it-codigo = pallet.it-codigo AND
                    am-pd-alocacao.nr-pallet = pallet.nr-pallet
                    EXCLUSIVE-LOCK NO-ERROR.

                IF AVAIL am-pd-alocacao THEN
                    DELETE am-pd-alocacao.
                
            END.

        END.

        IF NOT AVAIL pallet THEN DO:

            /* Dialog-box de Mensagens */

            DEFINE BUTTON db-bt-cancel AUTO-END-KEY 
                 LABEL "&Fechar" 
                 SIZE 10 BY 1
                 BGCOLOR 8.

            DEFINE RECTANGLE db-rt-botoes
                 EDGE-PIXELS 2 GRAPHIC-EDGE  
                 SIZE 58 BY 1.42
                 BGCOLOR 7.  

            DEFINE VARIABLE c-mensagem AS CHARACTER FORMAT "X(45)" NO-UNDO.

            ASSIGN c-mensagem = "Pallet N∆o Existe".

            DEFINE RECTANGLE db-rect-1
             EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
             SIZE 39 BY 3.30.

            DEFINE FRAME db-frame-1

                c-mensagem NO-LABEL 
                   at ROW 3 col 13 

                db-rect-1 AT ROW 1.9 COL 10

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

            /* Fim do Dialog-box */

        END.


    END.  /* for each tt-digita */

    /* Fim da Transferencia dos Paletes Selecionados */


    FOR EACH tt-digita EXCLUSIVE-LOCK.

           DELETE tt-digita.

    END.

    CLOSE QUERY br-digita.
    open query br-digita for each tt-digita.

               
END.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

ON END-ERROR OF C-Win
OR ENDKEY OF C-Win ANYWHERE DO:
   RETURN NO-APPLY.
END.


ON END-ERROR OF C-Win
OR ENDKEY OF C-Win ANYWHERE DO:
   RETURN NO-APPLY.
END.

ON WINDOW-CLOSE OF C-Win
DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.

ON ENDKEY OF FRAME f-relat DO:
  return no-apply.
END.

ON CHOOSE OF bt-ajuda IN FRAME f-relat
DO:

RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).

END.

ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:

run grapi/gr2003a.p (input-output c-arquivo, output l-ok, input v-cod-extens-arq).

if l-ok = yes then
  display c-arquivo with frame f-pg-imp.



END.

ON CHOOSE OF bt-cancelar IN FRAME f-relat
DO:
   apply "close" to this-procedure.
END.

ON CHOOSE OF bt-config-impr IN FRAME f-pg-imp
DO:

run grapi/gr2008.p (input-output c-arquivo).

disp c-arquivo with frame f-pg-imp.


END.


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-paletes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-paletes w-relat
ON CHOOSE OF bt-paletes IN FRAME f-relat /* Carrega Paletes */

DO: 

    FIND FIRST estabelec WHERE 
        estabelec.cod-estabel = c-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel
        NO-LOCK NO-ERROR.

    IF NOT AVAIL estabelec THEN DO:
       
       run utp/ut-msgs.p (input "show":U, input 2, "Estabelecimento").
       APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
       return NO-apply.

    END. 

    FIND FIRST emitente WHERE 
        emitente.cod-emitente = INT(i-cod-emitente:SCREEN-VALUE IN FRAME f-pg-sel)
        NO-LOCK NO-ERROR.

    IF NOT AVAIL emitente THEN DO:
       
       run utp/ut-msgs.p (input "show":U, input 2, "Emitente").
       APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
       return NO-apply.

    END. 


    FIND FIRST ITEM WHERE 
        ITEM.it-codigo = c-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel
        NO-LOCK NO-ERROR.

    IF NOT AVAIL ITEM OR ITEM.it-codigo = " " THEN DO:
       
       run utp/ut-msgs.p (input "show":U, input 2, "Item").
       APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
       return NO-apply.

    END. 

    ASSIGN item-origem-jr = "".

   IF (INT(i-pd-destino:SCREEN-VALUE IN FRAME f-pg-sel) = 
        INT(i-pd-origem:SCREEN-VALUE IN FRAME f-pg-sel)) AND
       (INT(i-sq-destino:SCREEN-VALUE IN FRAME f-pg-sel) =
        INT(i-sq-origem:SCREEN-VALUE IN FRAME f-pg-sel)) THEN DO:
       
       run utp/ut-msgs.p (input "show":U, input 18682, "Pedido de Origem e o~~ Pedido de Destino").
       APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
       return NO-apply.

    END.

    IF INT(i-pd-origem:SCREEN-VALUE IN FRAME f-pg-sel) > 0 THEN DO:

            FIND ped-venda WHERE 
                ped-venda.nr-pedido = INT(i-pd-origem:SCREEN-VALUE IN FRAME f-pg-sel)
                NO-LOCK NO-ERROR.

/*
            IF NOT AVAIL ped-venda OR
               ped-venda.cod-estabel <> c-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel THEN DO:
               
               run utp/ut-msgs.p (input "show":U, input 32582, "Pedido de Origem n∆o Ç do estabelecimento").
               APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
               return NO-apply.
            
            END.
            */
        
            IF NOT AVAIL ped-venda THEN DO:
               
               run utp/ut-msgs.p (input "show":U, input 2, "Pedido de Origem").
               APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
               return NO-apply.
        
            END.
    
    END.
    
    IF INT(i-pd-origem:SCREEN-VALUE IN FRAME f-pg-sel) > 0 THEN DO:
    
            FIND ped-venda WHERE 
                ped-venda.nr-pedido = INT(i-pd-origem:SCREEN-VALUE IN FRAME f-pg-sel)
                NO-LOCK NO-ERROR.
        
            IF AVAIL ped-venda THEN DO:
        
                FIND FIRST ped-item OF ped-venda WHERE
                    ped-item.nr-sequencia  = INT(i-sq-origem:SCREEN-VALUE IN FRAME f-pg-sel) AND
                    ped-item.ind-componen  <> 3 
                    NO-LOCK NO-ERROR.
        
            END.
        
            IF NOT AVAIL ped-venda OR 
               NOT AVAIL ped-item  THEN DO:
        
               run utp/ut-msgs.p (input "show":U, input 2, "Pedido de Origem").
               APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
               return NO-apply.
        
            END.
             
    
            ASSIGN item-origem-jr = ped-item.it-codigo.
    
            IF INT(i-pd-destino:SCREEN-VALUE IN FRAME f-pg-sel) = 0 THEN DO:
        
               FIND FIRST ped-item OF ped-venda WHERE
                   ped-item.nr-sequencia  = INT(i-sq-origem:SCREEN-VALUE IN FRAME f-pg-sel) AND
                   ped-item.ind-componen  <> 3 
                   NO-LOCK NO-ERROR.
        
               IF NOT AVAIL ped-item THEN DO:
        
                   run utp/ut-msgs.p (input "show":U, input 32582, "Item do Pedido de Origem N∆o Exite").
                   APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
                   return NO-apply.
        
               END.
    
               ASSIGN item-origem-jr = ped-item.it-codigo.
        
              /* ASSIGN cod-refer-ped = ped-item.cod-refer.*/
        
            END.
    
    END.

    IF INT(i-pd-origem:SCREEN-VALUE IN FRAME f-pg-sel) = 0 THEN 
        ASSIGN item-origem-jr = item.it-codigo.

    IF INT(i-pd-destino:SCREEN-VALUE IN FRAME f-pg-sel) > 0 THEN DO:

       FIND ped-venda WHERE 
           ped-venda.nr-pedido = INT(i-pd-destino:SCREEN-VALUE IN FRAME f-pg-sel)
           NO-LOCK NO-ERROR.

      IF NOT AVAIL ped-venda /*OR
          ped-venda.cod-estabel <> c-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel*/ THEN DO:
          
          run utp/ut-msgs.p (input "show":U, input 32582, "Pedido Destino n∆o existe").
          /*run utp/ut-msgs.p (input "show":U, input 32582, "Pedido Destino n∆o Ç do estabelecimento").*/
          APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
          return NO-apply.

       END.
    
       IF NOT AVAIL ped-venda THEN DO:
          
          run utp/ut-msgs.p (input "show":U, input 2, "Pedido de Destino").
          APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
          return NO-apply.
    
       END.

       IF ped-venda.cod-sit-ped > 2 THEN DO:

           run utp/ut-msgs.p (input "show":U, input 7069, " ").
           APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
           return NO-apply.

       END.

       FIND FIRST ped-item OF ped-venda WHERE
           ped-item.nr-sequencia  = INT(i-sq-destino:SCREEN-VALUE IN FRAME f-pg-sel) AND
           ped-item.ind-componen  <> 3 
           NO-LOCK NO-ERROR.
       
       IF NOT AVAIL ped-item OR ped-item.it-codigo <> item-origem-jr THEN DO:

           run utp/ut-msgs.p (input "show":U, input 17006, "Item do Pedido de Destino Diferente do Pedido Origem").
           APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
           return NO-apply.


       END.

       ASSIGN largura-ped = 0.

       FIND FIRST var-result 
           WHERE var-result.item-cotacao = ped-item.it-codigo
             AND var-result.nr-estrut    = ped-item.nr-config
             AND var-result.nome-var     = "LARGURA"  NO-LOCK NO-ERROR.

         IF AVAIL var-result THEN 
            ASSIGN largura-ped = var-result.valor-dec.


    END.

    CLOSE QUERY br-digita.

    FOR EACH tt-digita.
        DELETE tt-digita.
    END.

    ASSIGN i-pd-origem-jr  = INT(i-pd-origem:SCREEN-VALUE IN FRAME f-pg-sel)
          i-sq-origem-jr  = INT(i-sq-origem:SCREEN-VALUE IN FRAME f-pg-sel)
          i-pd-destino-jr = INT(i-pd-destino:SCREEN-VALUE IN FRAME f-pg-sel)
          i-sq-destino-jr = INT(i-sq-destino:SCREEN-VALUE IN FRAME f-pg-sel).

    /** Montando a tabela para tt-digita ***/

    run utp/ut-acomp.p persistent set h-acomp.

    {utp/ut-liter.i Encontrando Registros * I}

    run pi-inicializar in h-acomp (input "Carregando Paletes ...Aguarde"). 

    FOR EACH tt-notas EXCLUSIVE-LOCK.
        DELETE tt-notas.
    END.

    /* Rotina para Obter as Remessas de Lotes p/terceiros */

    for each saldo-terc no-lock
          where saldo-terc.cod-estabel   = estabelec.cod-estabel AND
                saldo-terc.cod-emitente  = emitente.cod-emitente and 
                saldo-terc.it-codigo     = item.it-codigo        and 
                saldo-terc.quantidade    > 0 
                USE-INDEX estab-item.

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
            fat-ser-lote.nr-seq-fat  = saldo-terc.sequencia.

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
                rat-lote.nat-oper     = componente.nat-oper.

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


    /* Procurar pallets enviados a terceiros na virada do ems-206 */
        
    for each saldo-terc no-lock
          where saldo-terc.cod-estabel   = estabelec.cod-estabel AND
                saldo-terc.cod-emitente  = emitente.cod-emitente and 
                saldo-terc.it-codigo     = item.it-codigo        and 
                saldo-terc.quantidade    > 0 
                USE-INDEX estab-item.


            FIND FIRST nota-fiscal WHERE
                nota-fiscal.cod-estabel = saldo-terc.cod-estabel AND
                nota-fiscal.serie       = saldo-terc.serie-docto AND
                nota-fiscal.nr-nota-fis = saldo-terc.nro-docto
                NO-LOCK NO-ERROR.
        
            IF NOT AVAIL nota-fiscal THEN DO:
        
                FIND FIRST tt-lotes WHERE
                    tt-lotes.nro-docto = saldo-terc.nro-docto AND
                    tt-lotes.sequencia = saldo-terc.sequencia AND
                    tt-lotes.lote      = saldo-terc.lote
                    NO-LOCK NO-ERROR.
        
                IF NOT AVAIL tt-lotes THEN 
                   CREATE tt-lotes.
        
                ASSIGN tt-lotes.nro-docto    = saldo-terc.nro-docto 
                       tt-lotes.sequencia    = saldo-terc.sequencia 
                       tt-lotes.lote         = saldo-terc.lote
                       tt-lotes.cod-estabel  = saldo-terc.cod-estabel
                       tt-lotes.cod-emitente = saldo-terc.cod-emitente
                       tt-lotes.it-codigo    = saldo-terc.it-codigo.
        
                ASSIGN tt-lotes.saldo        = tt-lotes.saldo + saldo-terc.quantidade.   
        
            END.
        
        END.
        
        /* Fim da Procura dos pallets enviados a terceiros na virada do ems-206 */ 





    FOR EACH tt-lotes NO-LOCK.

        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

        IF tt-lotes.saldo = 0 THEN NEXT.

        FIND FIRST saldo-terc 
              where saldo-terc.cod-estabel  = tt-lotes.cod-estabel AND 
                    saldo-terc.cod-emitente = tt-lotes.cod-emitente and 
                    saldo-terc.it-codigo    = tt-lotes.it-codigo
                    USE-INDEX estab-item
                    NO-LOCK NO-ERROR. 

        IF NOT AVAIL saldo-terc THEN NEXT.
         
         
        i-pd-origem-ed = i-pd-origem-jr .
        run pi-pallets.
        
        find first if-ped-venda where if-ped-venda.nr-pedido-relac = i-pd-origem-jr no-lock no-error.
        
        if avail if-ped-venda then do:
               i-pd-origem-ed = if-ped-venda.nr-pedido.
               run pi-pallets.
        
        end.
        
        find first if-ped-venda where if-ped-venda.nr-pedido = i-pd-origem-jr no-lock no-error.
        
        if avail if-ped-venda then do:
               i-pd-origem-ed = if-ped-venda.nr-pedido-relac.
               run pi-pallets.
        
        end.
        

        
        
        
         

    END. /*FOR EACH tt-lotes*/

    run pi-finalizar in h-acomp.

      CLOSE QUERY br-digita.

       assign im-pg-dig:sensitive in frame f-relat = yes
              im-pg-sel:sensitive in frame f-relat = yes.

       apply "mouse-select-click" to im-pg-dig in frame f-relat.

     /*** habilita **/
    if  num-results('br-digita') > 0 then do:
        if  br-digita:insert-row('after') in frame f-pg-dig then.
    end.
    else
       do transaction:
          open query br-digita for each tt-digita.
          apply 'entry' to tt-digita.cod-estabel in browse br-digita. 
       end.
    
    enable bt-marca         
           bt-desmarca      
           bt-marca-todos   
           bt-desmarca-todos
           bt-retirar  
           bt-gera-ordem with frame f-pg-dig.

    ENABLE bt-executar WITH FRAME f-relat.

END.






ON CHOOSE OF bt-executar IN FRAME f-relat
DO:

      do  on error undo, return no-apply:
          run pi-executar.   
      end.



      CLOSE QUERY br-digita.

       assign im-pg-dig:sensitive in frame f-relat = yes
              im-pg-sel:sensitive in frame f-relat = yes.

       apply "mouse-select-click" to im-pg-dig in frame f-relat.

     /*** habilita **/
    if  num-results('br-digita') > 0 then do:
        if  br-digita:insert-row('after') in frame f-pg-dig then.
    end.
    else
       do transaction:
          open query br-digita for each tt-digita.
          apply 'entry' to tt-digita.cod-estabel in browse br-digita. 
       end.
    
    enable bt-marca         
           bt-desmarca      
           bt-marca-todos   
           bt-desmarca-todos
           bt-retirar 
           bt-gera-ordem with frame f-pg-dig.

    ENABLE bt-executar bt-paletes WITH FRAME f-relat.
  



END.

ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
   run pi-troca-pagina.
END.

ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
   run pi-troca-pagina.
END.

ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
   run pi-troca-pagina.
END.


ON MOUSE-SELECT-CLICK OF im-pg-dig IN FRAME f-relat
DO:
   run pi-troca-pagina.
END.

ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
   do  with frame f-pg-imp:
       case self:screen-value:
          when "1" then do:
              if c-destino-old = "2" then assign c-impressora-old = c-arquivo:screen-value.
              assign c-arquivo:sensitive    = no
                     c-destino-old          = "1"
                     c-arquivo:visible      = yes
                     c-arquivo:screen-value  = c-arquivo-old
                     bt-arquivo:visible     = no
                     bt-config-impr:visible = yes.
            end.

            when "2" then do:
               if c-destino-old = "1" then assign c-arquivo-old = c-arquivo:screen-value.
               assign c-arquivo:sensitive     = yes
                      c-destino-old           = "2"
                      c-arquivo:visible       = yes
                      c-arquivo:screen-value  = c-impressora-old
                      bt-arquivo:visible      = yes
                      bt-config-impr:visible  = no.
            end.

            when "3" then do:
               if c-destino-old = "2" then assign c-impressora-old = c-arquivo:screen-value.
               if c-destino-old = "1" then assign c-arquivo-old = c-arquivo:screen-value.
               assign c-arquivo:sensitive     = no
                      c-destino-old           = "3"
                      c-arquivo:visible       = no
                      bt-arquivo:visible      = no
                      bt-config-impr:visible  = no.
            end.
       end case.
   end.
END.

ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-imp
DO:

/****************************************************************
**
** I-RPRSE.I - Gatilho "Value-Changed" de rs-execucao 
**
*****************************************************************/

ASSIGN rs-execucao.

IF rs-execucao = 2 THEN DO:
    IF rs-destino:SCREEN-VALUE IN FRAME f-pg-imp <> "1":U THEN
        ASSIGN rs-destino:SCREEN-VALUE IN FRAME f-pg-imp = "2":U
               c-arquivo       = IF c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = "":U
                                 THEN IF c-arquivo = "" 
                                      THEN c-arq-old
                                      ELSE c-arquivo
                                 ELSE c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp
               c-arq-old       = c-arquivo
               c-arq-old-batch = SUBSTRING(c-arquivo, R-INDEX(c-arquivo, "/":U) + 1).
    
    APPLY "VALUE-CHANGED":U TO rs-destino IN FRAME f-pg-imp.
    
    ASSIGN c-arquivo.
    
    rs-destino:DISABLE(c-terminal) IN FRAME f-pg-imp.
END.
ELSE DO:
    IF rs-destino:SCREEN-VALUE IN FRAME f-pg-imp <> "1":U THEN
        ASSIGN c-arquivo       = IF c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = "":U
                                 THEN c-arquivo
                                 ELSE c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp
               c-arq-old-batch = c-arquivo.
    
    APPLY "VALUE-CHANGED":U TO rs-destino IN FRAME f-pg-imp.
    
    rs-destino:ENABLE(c-terminal) IN FRAME f-pg-imp.
    
    ASSIGN c-arquivo.
END.


END.

/* ***************************  Main Block  *************************** */

ASSIGN CURRENT-WINDOW             = {&WINDOW-NAME}
   THIS-PROCEDURE:CURRENT-WINDOW  = {&WINDOW-NAME}.

assign v-cod-prog-gerado = "espd0022".


find first usuar_mestre
     where usuar_mestre.cod_usuario = c-seg-usuario no-lock no-error.

if avail usuar_mestre then 


def var c-tit as char no-undo.

run grapi/gr2013a.p (input v-cod-prog-gerado,
                    input "Alteraá∆o do N£mero do Pedido em Paletes em Terceiros",
                    input  v-cod-prog-gerado,
                    output c-tit,
                    output c-arquivo,
                    input v-cod-extens-arq).

if return-value = "adm-error" then do:
   APPLY "CLOSE" TO THIS-PROCEDURE.
   return no-apply.
end.

assign {&WINDOW-NAME}:title = c-tit
       c-arq-old        = c-arquivo
       c-impressora-old = c-arquivo.


 /*include de inicializaá∆o do relat¢rio */

 /*inicializaá‰es do template de relat¢rio */

assign {&window-name}:virtual-width-chars  = {&window-name}:width-chars  
       {&window-name}:virtual-height-chars = {&window-name}:height-chars 
       {&window-name}:min-width-chars      = {&window-name}:width-chars  
       {&window-name}:max-width-chars      = {&window-name}:width-chars  
       {&window-name}:min-height-chars     = {&window-name}:height-chars 
       {&window-name}:max-height-chars     = {&window-name}:height-chars.
assign c-terminal = " Terminal".

ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.


def var wh-label-sel     as widget-handle no-undo.
def var wh-label-cla     as widget-handle no-undo.
def var wh-label-par     as widget-handle no-undo.
def var wh-label-dig     as widget-handle no-undo.
def var wh-label-imp     as widget-handle no-undo.
def var wh-group         as widget-handle no-undo.
def var wh-child         as widget-handle no-undo.
def var c-list-folders   as char          no-undo.
def var i-current-folder as integer       no-undo.
def var i-new-folder     as integer       no-undo.
def var c-aux            as char          no-undo.
def var i-aux            as integer       no-undo.

ON  CLOSE OF THIS-PROCEDURE DO:
    run btb/btb918zb.p (input c-programa-mg97,
                        input-output rw-log-exec,
                        input no).
    RUN disable_ui.
END.


&if "{&PGIMP}" <> "" &then

  ON "ENTER" OF C-ARQUIVO IN FRAME F-PG-IMP OR
     "RETURN" OF C-ARQUIVO IN FRAME F-PG-IMP OR
     "CTRL-ENTER" OF C-ARQUIVO IN FRAME F-PG-IMP OR
     "CTRL-J" OF C-ARQUIVO IN FRAME F-PG-IMP OR
     "CTRL-Z" OF C-ARQUIVO IN FRAME F-PG-IMP do:
    RETURN NO-APPLY.
  END.  

  ON "\" OF C-ARQUIVO IN FRAME F-PG-IMP do:
    apply "/" to C-ARQUIVO in frame F-PG-IMP.
    return no-apply.       
  END.
&endif



/********************************************
** HELP FRAME
********************************************/
ON HELP OF FRAME f-relat
    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).

&IF "{&PGSEL}" <> "" &THEN 
ON HELP OF FRAME f-pg-sel
    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).
&ENDIF

&IF "{&PGPAR}" <> "" &THEN 
ON HELP OF FRAME f-pg-par
    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).
&ENDIF

&IF "{&PGIMP}" <> "" &THEN 
ON HELP OF FRAME f-pg-imp
    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).
&ENDIF



/********************************************************** 
** Traduá∆o p†gina seleá∆o - frame f-pg-sel
**********************************************************/
create text wh-label-sel
    assign frame        = frame f-relat:handle
           format       = "x(07)"
           font         = 1
           screen-value = "Seleá∆o"
           width        = 8
           row          = 1.8
           col          = im-pg-sel:col in frame f-relat + 1.86
           visible      = yes
     triggers:
         on mouse-select-click
            apply "mouse-select-click" to im-pg-sel in frame f-relat.           
     end triggers.                   
/********************************************************** 
** Traduá∆o p†gina Digitaá∆o - frame f-pg-dig
**********************************************************/
create text wh-label-dig
    assign frame        = frame f-relat:handle
           format       = "x(07)"
           font         = 1
           screen-value = "Paletes"
           width        = 8
           row          = 1.8
           col          = im-pg-dig:col in frame f-relat + 1.7
           visible      = yes
     triggers:
         on mouse-select-click
            apply "mouse-select-click" to im-pg-dig in frame f-relat.           
     end triggers.                   
/********************************************************** 
** Traduá∆o p†gina parÉmetros - frame f-pg-par
**********************************************************/
create text wh-label-par
    assign frame        = frame f-relat:handle
           format       = "x(10)"
           font         = 1
           screen-value = "ParÉmetros"
           width        = 11
           row          = 1.8
           col          = im-pg-par:col in frame f-relat + 1.7
           visible      = yes
     triggers:
         on mouse-select-click
            apply "mouse-select-click" to im-pg-par in frame f-relat.           
     end triggers.
/********************************************************** 
** Traduá∆o p†gina impress∆o - frame f-pg-imp
**********************************************************/
create text wh-label-imp
    assign frame        = frame f-relat:handle
           format       = "x(10)"
           font         = 1
           screen-value = "Impress∆o"
           width        = 10
           row          = 1.8
           col          = im-pg-imp:col in frame f-relat + 1.7
           visible      = yes
     triggers:
         on mouse-select-click
            apply "mouse-select-click" to im-pg-imp in frame f-relat.           
     end triggers.                   


/********************************************************** 
** Troca de p†gina por CTRL-TAB e SHIFT-CTRL-TAB
**********************************************************/

&IF "{&PGSEL}" <> "" &THEN 
    assign c-list-folders = c-list-folders + "im-pg-sel,".
&ENDIF
&IF "{&PGCLA}" <> "" &THEN 
    assign c-list-folders = c-list-folders + "im-pg-cla,".
&ENDIF
&IF "{&PGPAR}" <> "" &THEN 
    assign c-list-folders = c-list-folders + "im-pg-par,".
&ENDIF
&IF "{&PGDIG}" <> "" &THEN 
    assign c-list-folders = c-list-folders + "im-pg-dig,".
&ENDIF
&IF "{&PGIMP}" <> "" &THEN
    assign c-list-folders = c-list-folders + "im-pg-imp".
&ENDIF

if  substring(c-list-folders,length(c-list-folders)) = "," then 
    assign c-list-folders = substring(c-list-folders,1,length(c-list-folders) - 1 ).

on  CTRL-TAB,SHIFT-CTRL-TAB anywhere do:
    define variable h_handle  as handle no-undo.       
    define variable c_imagem  as character no-undo.
    define variable l_direita as logical no-undo.            

    l_direita = last-event:label = 'CTRL-TAB'.
        
    block1:
    repeat:        
        if  l_direita then do:
            if  i-current-folder = num-entries(c-list-folders) then
                i-current-folder = 1.
            else
                i-current-folder = i-current-folder + 1.
        end.
        else do:
            if  i-current-folder = 1 then
                i-current-folder = num-entries(c-list-folders).
            else
                i-current-folder = i-current-folder - 1.
        end.
    
        assign c_imagem = entry(i-current-folder,c-list-folders)
               h_handle = frame f-relat:first-child
               h_handle = h_handle:first-child.

        do  while valid-handle(h_handle):
            if  h_handle:type = 'image' and
                h_handle:name =  c_imagem then do:
                if  h_handle:sensitive = no then 
                    next block1.
                apply 'mouse-select-click' to h_handle.
                leave block1.
            end.
            h_handle = h_handle:next-sibling.
        end.
    end.
end.



/********************************************************** 
** Procedure de troca de p†gina por CTRL-TAB 
**********************************************************/
procedure pi-first-child:
        
    define input parameter wh-entry-folder as widget-handle.
    
    assign wh-entry-folder = wh-entry-folder:first-child
           wh-entry-folder = wh-entry-folder:first-child.
    do  while(valid-handle(wh-entry-folder)):
        if  wh-entry-folder:sensitive = yes 
        and wh-entry-folder:type <> 'rectangle' 
        and wh-entry-folder:type <> 'image'
        and wh-entry-folder:type <> 'browse' then do:
            apply 'entry' to wh-entry-folder.
            leave.
        end.
        else
            assign wh-entry-folder = wh-entry-folder:next-sibling.    
    end.
    
end.


/* define procedure externa para execucao do programa de visualizacao do relatorio */

PROCEDURE WinExec EXTERNAL "kernel32.dll":
  DEF INPUT  PARAM prg_name                          AS CHARACTER.
  DEF INPUT  PARAM prg_style                         AS SHORT.
END PROCEDURE.


&IF "{&PGIMP}":U <> "":U &THEN
    ON "LEAVE":U OF c-arquivo IN FRAME f-pg-imp DO:
        IF rs-execucao = 1 THEN
            ASSIGN c-arq-old = c-arquivo:SCREEN-VALUE.
        ELSE
            ASSIGN c-arq-old-batch = c-arquivo:SCREEN-VALUE.
    END.
    
    ON "VALUE-CHANGED":U OF rs-destino IN FRAME f-pg-imp DO:
        CASE rs-destino:SCREEN-VALUE IN FRAME f-pg-imp:
            WHEN "1":U THEN
                ASSIGN c-arquivo                                = c-imp-old
                       c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = c-imp-old
                       c-arquivo:SENSITIVE IN FRAME f-pg-imp    = NO
                       c-arquivo:VISIBLE IN FRAME f-pg-imp      = YES
                       bt-arquivo:VISIBLE IN FRAME f-pg-imp     = NO
                       bt-config-impr:VISIBLE IN FRAME f-pg-imp = YES.
            WHEN "2":U THEN
                ASSIGN c-arquivo = IF rs-execucao = 1 
                                   THEN c-arq-old
                                   ELSE c-arq-old-batch
                       c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = c-arquivo
                       c-arquivo:SENSITIVE IN FRAME f-pg-imp    = YES
                       c-arquivo:VISIBLE IN FRAME f-pg-imp      = YES
                       bt-arquivo:VISIBLE IN FRAME f-pg-imp     = YES
                       bt-config-impr:VISIBLE IN FRAME f-pg-imp = NO.
            WHEN "3":U THEN
                ASSIGN c-arquivo                                = "":U
                       c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = c-arquivo
                       c-arquivo:VISIBLE IN FRAME f-pg-imp      = NO
                       bt-arquivo:VISIBLE IN FRAME f-pg-imp     = NO
                       bt-config-impr:VISIBLE IN FRAME f-pg-imp = NO.
        END CASE.
    END.
&ENDIF


  assign wh-label-imp:screen-value = "Impress∆o".
PAUSE 0 BEFORE-HIDE.

MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    IF SESSION:SET-WAIT-STATE("":U) THEN.
        c-cod-estabel =  STRING({cdp\poloestab.i 422}). /*solic-318*/ 
    RUN enable_UI.

    ASSIGN text-destino:screen-value   IN FRAME f-pg-imp = "Destino".
    ASSIGN text-modo:screen-value      IN FRAME f-pg-imp = "Execuá∆o".
    ASSIGN text-parametro:screen-value IN FRAME f-pg-imp = "ParÉmetros de Impress∆o".
    
    assign /*rs-destino:radio-buttons in frame f-pg-imp = {varinc/var00002.i 07}
         */  rs-destino:screen-value  in frame f-pg-imp = "3":U.

    assign rs-formato:radio-buttons in frame f-pg-imp = {varinc/var00176.i 07}
           rs-formato:screen-value  in frame f-pg-imp = "2":U.


    assign v-cod-pg-mouse-selec = "im-pg-sel".

    apply "value-changed" to rs-destino in frame f-pg-imp.

    if v-cod-pg-mouse-selec = "im-pg-sel"
    then
        apply "mouse-select-click" to im-pg-sel in frame f-relat.

    if v-cod-pg-mouse-selec = "im-pg-par"
    then
        apply "mouse-select-click" to im-pg-par in frame f-relat.

    if v-cod-pg-mouse-selec = "im-pg-imp"
    then
        apply "mouse-select-click" to im-pg-imp in frame f-relat.

    if v-cod-pg-mouse-selec = "im-pg-dig"
    then
        apply "mouse-select-click" to im-pg-dig in frame f-relat.

     view c-win.
     apply "entry" to frame f-Relat.
     apply "entry" to c-win.

    if  im-pg-sel:sensitive in frame f-relat = no then do:
    run pi-muda-cor-label-folder(input "Seleá∆o").

    end.

    if  im-pg-dig:sensitive in frame f-relat = no then do:
    run pi-muda-cor-label-folder(input "Paletes").

    end.

    if  im-pg-par:sensitive in frame f-relat = no then do:
        run pi-muda-cor-label-folder(input "ParÉmetros").

    end.

   IF  NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.

END.
/* **********************  Internal Procedures  *********************** */

PROCEDURE adm-row-available :
   /* Define variables needed by this internal procedure.             */
  /* Process the newly available records (i.e. display fields, 
     open queries, and/or pass records on to any RECORD-TARGETS).    */
END PROCEDURE.

PROCEDURE disable_UI :
   IF SESSION:DISPLAY-TYPE = "GUI" AND VALID-HANDLE(C-Win)
   THEN DELETE WIDGET C-Win.
   IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE enable_UI :
   ENABLE /*bt-executar*/ bt-paletes bt-cancelar bt-ajuda /*im-pg-imp*/ im-pg-sel /*im-pg-dig*/
   WITH FRAME f-relat IN WINDOW C-Win.
   

  {&OPEN-BROWSERS-IN-QUERY-c-win}

   
   DISPLAY 
     c-cod-estabel
     i-cod-emitente
     c-nome-abrev
     i-pd-origem
     i-sq-origem
     c-it-codigo
     i-pd-destino
     i-sq-destino
     c-nome-cliente
      tb-largura

   WITH FRAME f-pg-sel IN WINDOW C-Win.

   ENABLE  
       c-cod-estabel
       i-cod-emitente
       i-pd-origem
       i-sq-origem
       c-it-codigo
       i-pd-destino
       i-sq-destino
       tb-largura

   WITH FRAME f-pg-sel IN WINDOW C-Win.
   
   DISPLAY rs-destino c-arquivo rs-execucao tb-parametro rs-formato
   WITH FRAME f-pg-imp IN WINDOW C-Win.
   ENABLE RECT-7 rs-destino bt-arquivo bt-config-impr c-arquivo RECT-9 rect-10 rs-execucao tb-parametro rs-formato
   WITH FRAME f-pg-imp IN WINDOW C-Win.
   
   DISPLAY 
   WITH FRAME f-pg-par IN WINDOW C-Win.

   ENABLE 
   WITH FRAME f-pg-par IN WINDOW C-Win.


  ENABLE br-digita bt-marca          
                   bt-desmarca       
                   bt-marca-todos    
                   bt-desmarca-todos 
                   bt-retirar        
      WITH FRAME f-pg-dig IN WINDOW c-win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-dig}
   
   VIEW C-Win.
END PROCEDURE.

PROCEDURE local-exit :
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN.
END PROCEDURE.

PROCEDURE pi-executar :

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
                tt-param.hora-exec            = time. /*
                tt-param.:classifica           = input frame f-pg-cla rs-classif
                tt-param.desc-classifica      = entry((tt-param.classifica - 1) * 2 + 1, 
                                                rs-classif:radio-buttons in frame f-pg-cla). */
             if tt-param.destino = 1 
             then assign tt-param.arquivo = "".
             else if  tt-param.destino = 2 
                  then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
                  else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".

              assign tt-param.i-cod-emitente        = input frame f-pg-sel i-cod-emitente   
                     tt-param.cod-estabel           = input frame f-pg-sel c-cod-estabel.   

           {include/i-rpexb.i}

           if  session:set-wait-state("general":U) then.

           {include/i-rprun.i pdp/espd0022rp.p} 

           {include/i-rpexc.i}

           if  session:set-wait-state("":U) then.

           {include/i-rptrm.i}
       end.


END PROCEDURE.

PROCEDURE pi-troca-pagina:

self:move-to-top() in frame f-relat.


case self:name:
    when "im-pg-sel" then do with frame f-relat:
        &IF "{&PGSEL}" <> "" &THEN
            view frame {&PGSEL}.
            run pi-first-child (input frame {&PGSEL}:handle).
            im-pg-sel:load-image("image/im-fldup") .
            assign im-pg-sel:height = 1.20
                   im-pg-sel:row    = 1.50.
        &ENDIF
        &IF "{&PGCLA}" <> "" &THEN
            hide frame {&PGCLA}.
              im-pg-cla:load-image("image/im-flddn") .
              im-pg-cla:move-to-bottom() .
            assign im-pg-cla:height = 1
                   im-pg-cla:row    = 1.6.
        &ENDIF
        &IF "{&PGPAR}" <> "" &THEN
            hide frame {&PGPAR}.
              im-pg-par:load-image("image/im-flddn") .
              im-pg-par:move-to-bottom() .
            assign im-pg-par:height = 1
                   im-pg-par:row    = 1.6.
        &ENDIF
        &IF "{&PGDIG}" <> "" &THEN
            hide frame {&PGDIG}.
              im-pg-dig:load-image("image/im-flddn") .
              im-pg-dig:move-to-bottom() .
            assign im-pg-dig:height = 1
                   im-pg-dig:row    = 1.6.
        &ENDIF
        &IF "{&PGIMP}" <> "" &THEN
            hide frame {&PGIMP}.
              im-pg-imp:load-image("image/im-flddn") .
              im-pg-imp:move-to-bottom() .
            assign im-pg-imp:height = 1
                   im-pg-imp:row    = 1.6.
        &ENDIF
    end.



    when "im-pg-cla" then do with frame f-relat:
        &IF "{&PGSEL}" <> "" &THEN
            hide frame {&PGSEL}.
              im-pg-sel:load-image("image/im-flddn") .
              im-pg-sel:move-to-bottom() .
            assign im-pg-sel:height = 1
                   im-pg-sel:row    = 1.6.
        &ENDIF
        &IF "{&PGCLA}" <> "" &THEN
            view frame {&PGCLA}.
            run pi-first-child (input frame {&PGCLA}:handle).
              im-pg-cla:load-image("image/im-flddn") .
            assign im-pg-cla:height = 1
                   im-pg-cla:row    = 1.6.
        &ENDIF
        &IF "{&PGPAR}" <> "" &THEN
            hide frame {&PGPAR}.
              im-pg-par:load-image("image/im-flddn") .
              im-pg-par:move-to-bottom() .
            assign im-pg-par:height = 1
                   im-pg-par:row    = 1.6.
        &ENDIF
        &IF "{&PGDIG}" <> "" &THEN
            hide frame {&PGDIG}.
              im-pg-dig:load-image("image/im-flddn") .
              im-pg-dig:move-to-bottom() .
            assign im-pg-dig:height = 1
                   im-pg-dig:row    = 1.6.
        &ENDIF
        &IF "{&PGIMP}" <> "" &THEN
            hide frame {&PGIMP}.
              im-pg-imp:load-image("image/im-flddn") .
              im-pg-imp:move-to-bottom() .
            assign im-pg-imp:height = 1
                   im-pg-imp:row    = 1.6.
        &ENDIF
    end.



    when "im-pg-par" then do with frame f-relat:
        &IF "{&PGSEL}" <> "" &THEN
            hide frame {&PGSEL}.
              im-pg-sel:load-image("image/im-flddn") .
              im-pg-sel:move-to-bottom() .
            assign im-pg-sel:height = 1
                   im-pg-sel:row    = 1.6.
        &ENDIF
        &IF "{&PGCLA}" <> "" &THEN
            hide frame {&PGCLA}.
              im-pg-cla:load-image("image/im-flddn") .
              im-pg-cla:move-to-bottom() .
            assign im-pg-cla:height = 1
                   im-pg-cla:row    = 1.6.
        &ENDIF
        &IF "{&PGPAR}" <> "" &THEN
            view frame {&PGPAR}.
            run pi-first-child (input frame {&PGPAR}:handle).
              im-pg-par:load-image("image/im-fldup") .
            assign im-pg-par:height = 1.20
                   im-pg-par:row    = 1.5.
        &ENDIF
        &IF "{&PGDIG}" <> "" &THEN
            hide frame {&PGDIG}.
              im-pg-dig:load-image("image/im-flddn") .
              im-pg-dig:move-to-bottom() .
            assign im-pg-dig:height = 1
                   im-pg-dig:row    = 1.6.
        &ENDIF
        &IF "{&PGIMP}" <> "" &THEN
            hide frame {&PGIMP}.
              im-pg-imp:load-image("image/im-flddn") .
              im-pg-imp:move-to-bottom() .
            assign im-pg-imp:height = 1
                   im-pg-imp:row    = 1.6.
        &ENDIF
    end.



    when "im-pg-dig" then do with frame f-relat:
        &IF "{&PGSEL}" <> "" &THEN
            hide frame {&PGSEL}.
              im-pg-sel:load-image("image/im-flddn") .
              im-pg-sel:move-to-bottom() .
            assign im-pg-sel:height = 1
                   im-pg-sel:row    = 1.6.
        &ENDIF
        &IF "{&PGCLA}" <> "" &THEN
            hide frame {&PGCLA}.
              im-pg-cla:load-image("image/im-flddn") .
              im-pg-cla:move-to-bottom() .
            assign im-pg-cla:height = 1
                   im-pg-cla:row    = 1.6.
        &ENDIF
        &IF "{&PGPAR}" <> "" &THEN
            hide frame {&PGPAR}.
              im-pg-par:load-image("image/im-flddn") .
              im-pg-par:move-to-bottom() .
            assign im-pg-par:height = 1
                   im-pg-par:row    = 1.6.
        &ENDIF
        &IF "{&PGDIG}" <> "" &THEN
            view frame {&PGDIG}.
            run pi-first-child (input frame {&PGDIG}:handle).
              im-pg-dig:load-image("image/im-flddn") .
            assign im-pg-dig:height = 1
                   im-pg-dig:row    = 1.6.
        &ENDIF
        &IF "{&PGIMP}" <> "" &THEN
            hide frame {&PGIMP}.
              im-pg-imp:load-image("image/im-flddn") .
              im-pg-imp:move-to-bottom() .
            assign im-pg-imp:height = 1
                   im-pg-imp:row    = 1.6.
        &ENDIF
    end.



    when "im-pg-imp" then do with frame f-relat:
        &IF "{&PGSEL}" <> "" &THEN
            hide frame {&PGSEL}.
              im-pg-sel:load-image("image/im-flddn") .
              im-pg-sel:move-to-bottom() .
            assign im-pg-sel:height = 1
                   im-pg-sel:row    = 1.6.
        &ENDIF
        &IF "{&PGCLA}" <> "" &THEN
            hide frame {&PGCLA}.
              im-pg-cla:load-image("image/im-flddn") .
              im-pg-cla:move-to-bottom() .
            assign im-pg-cla:height = 1
                   im-pg-cla:row    = 1.6.
        &ENDIF
        &IF "{&PGPAR}" <> "" &THEN
            hide frame {&PGPAR}.
              im-pg-par:load-image("image/im-flddn") .
              im-pg-par:move-to-bottom() .
            assign im-pg-par:height = 1
                   im-pg-par:row    = 1.6.
        &ENDIF
        &IF "{&PGDIG}" <> "" &THEN
            hide frame {&PGDIG}.
              im-pg-dig:load-image("image/im-flddn") .
              im-pg-dig:move-to-bottom() .
            assign im-pg-dig:height = 1
                   im-pg-dig:row    = 1.6.
        &ENDIF
        &IF "{&PGIMP}" <> "" &THEN
            view frame {&PGIMP}.
            run pi-first-child (input frame {&PGIMP}:handle).
              im-pg-imp:load-image("image/im-fldup") .
            assign im-pg-imp:height = 1.20
                   im-pg-imp:row    = 1.5.
        &ENDIF
    end.
end case.

i-current-folder = lookup(self:name,c-list-folders).


END PROCEDURE.

PROCEDURE send-records :
    /* Define variables needed by this internal procedure.               */ 
    /* For each requested table, put it':Us ROWID in the output list.      */
    /* Deal with any unexpected table requests before closing.           */ 
END PROCEDURE.

PROCEDURE state-changed :
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
    run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

Procedure pi-muda-cor-label-folder:
   def input parameter p-cod-label as char  no-undo.
   def var wh-pai   as widget-handle.
   def var wh-filho as widget-handle.

    assign wh-pai = frame f-relat:handle
           wh-pai = wh-pai:first-child.
   do while wh-pai <> ?:
       do  while valid-handle(wh-pai):
           assign wh-filho = wh-pai:first-child.
           do  while valid-handle(wh-filho):
               if  wh-filho:type = "TEXT"
                   then
                       if  wh-filho:screen-value = p-cod-label
                       then
                           assign wh-filho:fgcolor = 7.
                       assign wh-filho = wh-filho:next-sibling.
           end.
           assign wh-pai = wh-pai:next-sibling.
       end.
   end.
END PROCEDURE.

PROCEDURE OpenDocument:

    def input param c-doc as char  no-undo.
    def var c-exec as char  no-undo.
    def var h-Inst as int  no-undo.

    assign c-exec = fill("x",255).
    run FindExecutableA (input c-doc,
                         input "",
                         input-output c-exec,
                         output h-inst).

    if h-inst >= 0 and h-inst <=32 then
      run ShellExecuteA (input 0,
                         input "open",
                         input "rundll32.exe",
                         input "shell32.dll,OpenAs_RunDLL " + c-doc,
                         input "",
                         input 1,
                         output h-inst).

    run ShellExecuteA (input 0,
                       input "open",
                       input c-doc,
                       input "",
                       input "",
                       input 1,
                       output h-inst).

    if h-inst < 0 or h-inst > 32 then return "OK".
    else return "NOK".

END PROCEDURE.

PROCEDURE FindExecutableA EXTERNAL "Shell32.dll" persistent:

    define input parameter lpFile as char  no-undo.
    define input parameter lpDirectory as char  no-undo.
    define input-output parameter lpResult as char  no-undo.
    define return parameter hInstance as long.

END.

PROCEDURE ShellExecuteA EXTERNAL "Shell32.dll" persistent:

    define input parameter hwnd as long.
    define input parameter lpOperation as char  no-undo.
    define input parameter lpFile as char  no-undo.
    define input parameter lpParameters as char  no-undo.
    define input parameter lpDirectory as char  no-undo.
    define input parameter nShowCmd as long.
    define return parameter hInstance as long.

END PROCEDURE.



procedure pi-pallets:


        FOR EACH pallet NO-LOCK WHERE
             
               pallet.it-codigo  = saldo-terc.it-codigo and 
               pallet.nr-pallet  = tt-lotes.lote        AND
               pallet.nr-pedido  = i-pd-origem-ed       AND
               pallet.nr-sequencia = i-sq-origem-jr     and
               pallet.situacao = 2
               USE-INDEX ITEM ,

          each item NO-LOCK
               where item.it-codigo = pallet.it-codigo 
               USE-INDEX codigo:

/*message  pallet.it-codigo  skip
pallet.nr-pallet  skip
pallet.situacao

view-as alert-box.
*/

          ASSIGN varSaldo =  tt-lotes.saldo
                 refer-jr =  pallet.cod-refer.

          FIND FIRST saldo-estoq WHERE
              saldo-estoq.lote      = pallet.nr-pallet AND
              saldo-estoq.it-codigo = pallet.it-codigo 
              USE-INDEX lote 
              NO-LOCK NO-ERROR.

        /*  IF NOT AVAIL saldo-estoq THEN NEXT .*/
          
          IF (varSaldo <> 0 OR pallet.situacao = 1) THEN DO:

                 FIND FIRST ped-venda WHERE 
                     ped-venda.nr-pedido = pallet.nr-pedido 
                     NO-LOCK no-error. 
          
                 IF AVAIL ped-venda THEN DO:

                   /* IF ped-venda.cod-estabel <> pallet.cod-estabel THEN NEXT.*/
                    ASSIGN varCli = ped-venda.nome-abrev.

                 END.
                 ELSE

                     ASSIGN varCli = "".
                     
          
           END.
          
           IF varSaldo <> 0 THEN DO:

             FIND FIRST it-pallet WHERE 
                 it-pallet.it-codigo = pallet.it-codigo AND
                 it-pallet.nr-pallet = pallet.nr-pallet AND
                 it-pallet.cod-estabel = pallet.cod-estabel
                 USE-INDEX ITEM
                 NO-LOCK NO-ERROR.
          
             IF AVAIL it-pallet THEN DO:
                 ASSIGN w-lote = it-pallet.lote-bobina.
                 ASSIGN w-item = it-pallet.it-codigo.
             END.
          
             FOR EACH lote-carac-tec WHERE lote-carac-tec.it-codigo = w-item
                AND   lote-carac-tec.lote = w-lote NO-LOCK.
          
                IF lote-carac-tec.cd-comp   = "largura" THEN
                     ASSIGN w-larg = lote-carac-tec.vl-result.
          
          
                IF lote-carac-tec.cd-comp   = "Diin" THEN
                     ASSIGN w-diin = lote-carac-tec.vl-result.
                 
                IF lote-carac-tec.cd-comp   = "Diex" THEN
                     ASSIGN w-diex = lote-carac-tec.vl-result.
          
             END.

             IF NOT tb-largura:CHECKED IN FRAME f-pg-sel OR (tb-largura:CHECKED IN FRAME f-pg-sel AND largura-ped = w-larg) THEN DO:
              
                 find first tt-digita where 
                        tt-digita.cod-estabel  = pallet.cod-estabel  AND
                        tt-digita.nr-pedido    = pallet.nr-pedido    AND
                        tt-digita.nr-sequencia = pallet.nr-sequencia AND
                        tt-digita.nr-pallet    = pallet.nr-pallet    AND
                        tt-digita.it-codigo    = pallet.it-codigo no-error.
                        
                  if not avail tt-digita then 
                       CREATE tt-digita.
              
                 ASSIGN tt-digita.cod-estabel  = pallet.cod-estabel
                        tt-digita.nr-pedido    = pallet.nr-pedido
                        tt-digita.nr-sequencia = pallet.nr-sequencia
                        tt-digita.nr-pallet    = pallet.nr-pallet
                        tt-digita.it-codigo    = pallet.it-codigo
                        tt-digita.nr-bobinas   = pallet.nr-bobinas
                        tt-digita.peso-bruto   = pallet.peso-bruto
                        tt-digita.peso-liquido = tt-digita.peso-liquido + varSaldo
                        tt-digita.situacao     = pallet.situacao
                        tt-digita.dt-pallet    = pallet.data-pallet
                        tt-digita.cod-refer    = pallet.cod-refer
                        tt-digita.w-larg       = w-larg 
                        tt-digita.w-larg-dest  = largura-ped
                        tt-digita.w-diin       = w-diin 
                        tt-digita.w-diex       = w-diex .
    
              
                 IF AVAIL saldo-estoq THEN
                     ASSIGN tt-digita.dt-vali = saldo-estoq.dt-vali-lote.
              END.
             ASSIGN w-larg = 0
                    w-diin = 0
                    w-diin = 0
                    varCli = ""
                    w-lote = ""
                    w-item = "".
          
           END. /*IF varSaldo <> 0 */

        END. /*FOR EACH pallet*/
end procedure.

PROCEDURE  pi-changeRowColor.
    
    DEFINE INPUT PARAM p-row AS HANDLE NO-UNDO.
    

    IF tt-digita.w-larg-dest <> tt-digita.w-larg THEN DO:
        i-cor = 12.
    END.
    ELSE 
       i-cor = 15.

    
    
        tt-digita.lot-marca:BGCOLOR in BROWSE br-digita  = i-cor.
        tt-digita.cod-estabel:BGCOLOR in BROWSE br-digita  = i-cor. 
        tt-digita.nr-pedido:BGCOLOR in BROWSE br-digita  = i-cor .        
        tt-digita.nr-pallet:BGCOLOR in BROWSE br-digita  = i-cor.
        tt-digita.it-codigo:BGCOLOR in BROWSE br-digita  = i-cor.
        tt-digita.nr-bobinas:BGCOLOR in BROWSE br-digita  = i-cor.
        tt-digita.peso-bruto:BGCOLOR in BROWSE br-digita  = i-cor.
        tt-digita.peso-liquido:BGCOLOR in BROWSE br-digita  = i-cor.
        tt-digita.situacao:BGCOLOR in BROWSE br-digita  = i-cor.
        tt-digita.dt-pallet:BGCOLOR in BROWSE br-digita  = i-cor.
        tt-digita.cod-refer:BGCOLOR in BROWSE br-digita  = i-cor.
        tt-digita.w-larg:BGCOLOR in BROWSE br-digita  = i-cor.
        tt-digita.w-larg-dest:BGCOLOR in BROWSE br-digita  = i-cor.
        tt-digita.w-diin:BGCOLOR in BROWSE br-digita  = i-cor      .
        tt-digita.w-diex:BGCOLOR in BROWSE br-digita  = i-cor.
        tt-digita.dt-vali:BGCOLOR in BROWSE br-digita  = i-cor.
        



END PROCEDURE.
