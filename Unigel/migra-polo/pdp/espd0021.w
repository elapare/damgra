&Scoped-define WINDOW-NAME C-Win

/*------------------------------------------------------------------------
File.............: espd0021.w
Description......: Ajusta Referˆncia do Pallet p/ a Refer. do Pedido
Input Parameters : 
Output Parameters: 
Author...........: Amgra - Jos‚ Roberto.
Created..........: 24/03/2008   
OBS..............: 
------------------------------------------------------------------------*/
def buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "espd0021".

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

/* Include Com as Vari veis Globais */

def new global shared var i-ep-codigo-usuario  like empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def new global shared var i-num-ped-exec-rpw  as integer no-undo.   
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
def new global shared var l-rpc as logical no-undo.
def new global shared var r-registro-atual as rowid no-undo.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var i-num-ped as integer no-undo.         
def new global shared var v_cdn_empres_usuar   like empresa.ep-codigo        no-undo.
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
    field ep-codigo            as integer
    field cod-estabel          like ped-venda.cod-estabel 
    field c-it-codigo          AS CHAR
    field i-pd-origem          AS INT
    FIELD i-sq-origem          AS INT.


DEFINE TEMP-TABLE tt-digita no-undo
    FIELD lot-lote                 LIKE saldo-estoq.lote         FORMAT "x(12)"         LABEL "Pallet"
    FIELD lot-it-codigo            LIKE saldo-estoq.it-codigo    FORMAT "x(16)"         LABEL "Filme"
    FIELD lot-dt-vali-lote         LIKE saldo-estoq.dt-vali-lote FORMAT "99/99/9999"    LABEL "Data"
    FIELD lot-cod-estabel          LIKE saldo-estoq.cod-estabel  FORMAT "x(4)"          LABEL "Est"
    FIELD lot-cod-depos            LIKE saldo-estoq.cod-depos    FORMAT "x(4)"          LABEL "Dep"
    FIELD lot-cod-localiz          LIKE saldo-estoq.cod-localiz  FORMAT "x(10)"         LABEL "Localiz."
    FIELD lot-cod-refer            LIKE saldo-estoq.cod-refer    FORMAT "x(10)"         LABEL "Refer."
    FIELD lot-quantidade           LIKE saldo-estoq.qtidade-atu  FORMAT "->>>>>>9.9999" LABEL "Quantidade"
    FIELD lot-nova-refer           AS CHAR                       FORMAT "x(10)"         LABEL "Nova Ref"

    INDEX chave IS PRIMARY UNIQUE lot-cod-estabel
                                  lot-cod-depos
                                  lot-cod-localiz
                                  lot-lote
                                  lot-it-codigo
                                  lot-cod-refer.

define buffer b-tt-digita for tt-digita.
define Buffer bf-digita   For tt-digita.

/*
DISABLE TRIGGERS FOR LOAD OF pallet. 
*/

    /**************************************************************************
    **
    **   ceapi001.i - Include de definicoes da temp-table e variaveis 
    **
    **                da API ceapi001.p
    **
    **************************************************************************/

def temp-table tt-movto  
    FIELD cod-versao-integracao AS INTEGER FORMAT "999"
    FIELD cod-prog-orig         LIKE movto-estoq.cod-prog-orig
    FIELD l-mov-erro            AS LOGICAL INITIAL NO
    FIELD r-mov-inv             AS ROWID    
    FIELD r-mov-orig            AS ROWID /*registro original para valorizar o estorno, devolu‡Æo,retorno*/
    FIELD sequen-nf             LIKE movto-estoq.sequen-nf
    FIELD cod-depos             LIKE movto-estoq.cod-depos
    FIELD cod-emitente          LIKE movto-estoq.cod-emitente
    FIELD cod-estabel           LIKE movto-estoq.cod-estabel
    FIELD cod-refer             LIKE movto-estoq.cod-refer
    FIELD ct-codigo             LIKE movto-estoq.ct-codigo
    FIELD descricao-db          LIKE movto-estoq.descricao-db
    FIELD dt-nf-saida           LIKE movto-estoq.dt-nf-saida
    FIELD dt-trans              LIKE movto-estoq.dt-trans
    FIELD esp-docto             LIKE movto-estoq.esp-docto
    FIELD it-codigo             LIKE movto-estoq.it-codigo
    FIELD cod-localiz           LIKE movto-estoq.cod-localiz
    FIELD lote                  LIKE movto-estoq.lote
    FIELD nat-operacao          LIKE movto-estoq.nat-operacao
    FIELD nro-docto             LIKE movto-estoq.nro-docto
    FIELD num-sequen            LIKE movto-estoq.num-sequen
    FIELD numero-ordem          LIKE movto-estoq.numero-ordem
    FIELD nr-ord-produ          LIKE movto-estoq.nr-ord-produ
    FIELD peso-liquido          LIKE movto-estoq.peso-liquido
    FIELD quantidade            LIKE movto-estoq.quantidade
    FIELD referencia            LIKE movto-estoq.referencia
    FIELD sc-codigo             LIKE movto-estoq.sc-codigo
    FIELD serie-docto           LIKE movto-estoq.serie-docto
    FIELD tipo-preco            LIKE movto-estoq.tipo-preco
    FIELD tipo-trans            LIKE movto-estoq.tipo-trans
    FIELD tipo-valor            LIKE movto-estoq.tipo-valor
    FIELD un                    LIKE movto-estoq.un         
    FIELD valor-mat-m           LIKE movto-estoq.valor-mat-m
    FIELD valor-mat-o           LIKE movto-estoq.valor-mat-o
    FIELD valor-mat-p           LIKE movto-estoq.valor-mat-p
    FIELD valor-mob-m           LIKE movto-estoq.valor-mob-m
    FIELD valor-mob-o           LIKE movto-estoq.valor-mob-o
    FIELD valor-mob-p           LIKE movto-estoq.valor-mob-p
    FIELD valor-ggf-m           LIKE movto-estoq.valor-ggf-m
    FIELD valor-ggf-o           LIKE movto-estoq.valor-ggf-o
    FIELD valor-ggf-p           LIKE movto-estoq.valor-ggf-p
    FIELD valor-nota            LIKE movto-estoq.valor-nota
    FIELD vl-nota-fasb          LIKE movto-estoq.vl-nota-fasb
    FIELD nr-ord-refer          LIKE movto-estoq.nr-ord-refer
    FIELD nr-req-sum            LIKE movto-estoq.nr-req-sum
    FIELD cod-roteiro           LIKE movto-estoq.cod-roteiro
    FIELD nr-reporte            LIKE movto-estoq.nr-reporte
    FIELD item-pai              LIKE movto-estoq.item-pai
    FIELD op-codigo             LIKE movto-estoq.op-codigo
    FIELD cod-usu-ult-alter     LIKE movto-estoq.cod-usu-ult-alter
    FIELD conta-contabil        LIKE movto-estoq.conta-contabil
    FIELD conta-db              LIKE movto-estoq.conta-contabil
    FIELD ct-db                 LIKE movto-estoq.ct-codigo
    FIELD sc-db                 LIKE movto-estoq.sc-codigo
    FIELD dt-vali-lote          LIKE saldo-estoq.dt-vali-lote
    FIELD op-seq                LIKE movto-estoq.op-seq
    FIELD usuario               LIKE movto-estoq.usuario
    FIELD nr-trans              LIKE movto-estoq.nr-trans 
    FIELD cod-estabel-des       LIKE movto-estoq.cod-estabel-des
    FIELD origem-valor          LIKE movto-estoq.origem-valor
    FIELD num-ord-des           LIKE movto-estoq.num-ord-des
    FIELD num-seq-des           LIKE movto-estoq.num-seq-des
    FIELD num-ord-inv           LIKE movto-estoq.num-ord-inv
    FIELD valor-ipi             LIKE movto-estoq.valor-ipi
    FIELD valor-iss             LIKE movto-estoq.valor-iss
    FIELD valor-icm             LIKE movto-estoq.valor-icm
    FIELD vl-icm-fasb           LIKE movto-estoq.vl-icm-fasb
    FIELD vl-iss-fasb           LIKE movto-estoq.vl-iss-fasb
    FIELD vl-ipi-fasb           LIKE movto-estoq.vl-ipi-fasb 
    FIELD per-ppm               LIKE movto-estoq.per-ppm
    FIELD atualiza-ul-ent       AS LOGICAL
    FIELD i-sequen              AS INTEGER
    FIELD gera-saldo            AS LOGICAL INIT NO
    FIELD qt-alocada            AS DECIMAL
    field i-sequen-pai             as integer
    field log-ficha                as log
  .


    def  temp-table tt-erro no-undo
        field i-sequen as int             
        field cd-erro  as int
        field mensagem as char format "x(255)".


    /*******************************************************************************/

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

def var h-acomp            as handle no-undo.
def var v-num-reg-lidos    as int    no-undo.


/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 


/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

def new shared var c-cod-estabel     like ped-venda.cod-estabel   format "x(03)"      no-undo.  /*solic-318*/ 
def new shared var c-it-codigo       AS CHAR                      format "x(16)"    initial ""      no-undo.
def new shared var i-pd-origem       AS INT                       format ">>>>>>>>9"initial 0       no-undo.
def new shared var i-sq-origem       AS INT                       format ">>>>9"    initial 0       no-undo.

DEFINE VARIABLE it-codigo-jr         AS CHARACTER                                   NO-UNDO.
DEFINE VARIABLE cod-refer-ped        AS CHARACTER                                   NO-UNDO.
DEFINE VARIABLE c-nome-cliente       AS CHARACTER  FORMAT "x(12)"                   NO-UNDO.

DEFINE VARIABLE i-pd-origem-jr       AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-sq-origem-jr       AS INTEGER    NO-UNDO.

DEFINE VARIABLE varCli LIKE ped-venda.nome-abrev.
DEFINE VARIABLE varSaldo AS decimal.
DEFINE VARIABLE w-lote LIKE it-pallet.lote-bobina.
DEFINE VARIABLE w-item LIKE it-pallet.it-codigo.
DEFINE VARIABLE w-larg LIKE lote-carac-tec.vl-result.
DEFINE VARIABLE w-diin LIKE lote-carac-tec.vl-result.
DEFINE VARIABLE w-diex LIKE lote-carac-tec.vl-result.
DEFINE VARIABLE refer-jr LIKE saldo-estoq.cod-refer.

def var i-empresa     as char       no-undo.
DEF VAR data-trans-jr AS DATE       NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */ 

&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita */

&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.lot-it-codigo tt-digita.lot-lote tt-digita.lot-cod-depos tt-digita.lot-cod-localiz tt-digita.lot-quantidade tt-digita nova-refer tt-digita.lot-dt-vali-lote tt-digita.lot-cod-estabel tt-digita.lot-cod-refer   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.lot-it-codigo tt-digita.lot-lote tt-digita.lot-cod-depos tt-digita.lot-cod-localiz tt-digita.lot-quantidade tt-digita-nova-refer tt-digita.lot-dt-vali-lote tt-digita.lot-cod-estabel tt-digita.lot-cod-refer   
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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)" INITIAL "Execu‡Æo"
VIEW-AS TEXT 
SIZE 10.86 BY .62 NO-UNDO.

DEFINE VARIABLE text-parametro AS CHARACTER FORMAT "X(256)" INITIAL "Parƒmetros de ImpressÆo"
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
LABEL "Imprimir P gina de Parƒmetros"
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
LABEL "Parƒmetro 1"
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


DEFINE BUTTON bt-retirar 
     LABEL "Retirar" 
     SIZE 15 BY 1. 

DEFINE BUTTON bt-gera-ordem 
     LABEL "Transf.p/Nova Referˆncia" 
     SIZE 20 BY 1. 

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
      tt-digita.lot-lote                 
      tt-digita.lot-it-codigo            
      tt-digita.lot-dt-vali-lote            
      tt-digita.lot-cod-estabel          
      tt-digita.lot-cod-depos            
      tt-digita.lot-cod-localiz          
      tt-digita.lot-cod-refer            
      tt-digita.lot-quantidade 
      tt-digita.lot-nova-refer
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
    "Destino de ImpressÆo do Relat¢rio" NO-LABEL
    bt-arquivo AT ROW 3.58 COL 43.29 HELP
    "Escolha do nome do arquivo"
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
    "Configura‡Æo da impressora"
     c-arquivo AT ROW 3.56 COL 3.29 HELP
    "Nome do arquivo de destino do relat¢rio" NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     rs-execucao AT ROW 5.77 COL 3 HELP
    "Modo de Execu‡Æo" NO-LABEL
     tb-parametro AT ROW 7.92 COL 3.2
     rs-formato AT ROW 8.8 COL 3 HELP
    "Formato de ImpressÆo" NO-LABEL
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
      at row 1 col 22 colon-aligned
      view-as fill-in 
      size 4 by .88
      font 1


    i-pd-origem label "Pedido de Venda"
      at row 5 col 22 colon-aligned
      view-as fill-in 
      size 9 by .88
      font 1

    i-sq-origem label "Seq.do Pedido"
      at row 6 col 22 colon-aligned
      view-as fill-in 
      size 9 by .88
      font 1

    c-it-codigo label "Cod.Item"
      at row 6 col 42 colon-aligned
      view-as fill-in 
      size 16 by .88
      font 1  

    c-nome-cliente   NO-LABEL
      at row 8 col 22 colon-aligned
      view-as fill-in 
      size 37 by .88
      font 1

   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D
   AT COL 3 ROW 2.85
   SIZE 76.86 BY 10.62.



DEFINE FRAME f-pg-dig  

    br-digita AT ROW 1 COL 1

    bt-retirar AT ROW 10 COL 21
   
    bt-gera-ordem AT ROW 10 COL 41

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

/* ******** Acerto da posi‡Æo dos labels e tamanho dos radio-set ******* */

DEFINE VARIABLE h-label AS WIDGET-HANDLE NO-UNDO.

/* *************************  Create Window  ************************** */

CREATE WINDOW C-Win ASSIGN
   HIDDEN             = YES
   TITLE              = "Ajusta Referˆncia do Pallet p/ a Refer. do Pedido - espd0021"
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

ON LEAVE OF i-sq-origem IN FRAME f-pg-sel
DO:
    
     FIND FIRST ped-venda WHERE
         ped-venda.nr-pedido = INT (i-pd-origem:SCREEN-VALUE IN FRAME f-pg-sel)
         NO-LOCK NO-ERROR.

     IF AVAIL ped-venda THEN  DO:

       FIND FIRST ped-item OF ped-venda WHERE
           ped-item.nr-sequencia = INT (i-sq-origem:SCREEN-VALUE IN FRAME f-pg-sel) AND
           ped-item.ind-componen  <> 3 
           NO-LOCK NO-ERROR.

     END.

     IF AVAIL ped-item THEN DO: 

       FIND FIRST emitente WHERE
           emitente.cod-emitente = ped-venda.cod-emitente
           NO-LOCK NO-ERROR.

       IF AVAIL emitente THEN 
           ASSIGN c-nome-cliente:SCREEN-VALUE IN FRAME f-pg-sel = string(emitente.nome-abrev).
       ELSE 
           ASSIGN c-nome-cliente:SCREEN-VALUE IN FRAME f-pg-sel = "".

       ASSIGN c-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = string(ped-item.it-codigo).

     END.

     ELSE 
         ASSIGN c-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel = ""
                c-nome-cliente:SCREEN-VALUE IN FRAME f-pg-sel = "".

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
ON ROW-ENTRY OF br-digita IN FRAME f-pg-dig
DO:
   /* trigger para inicializar campos da temp table de digita‡Æo */
/*   if  br-digita:new-row in frame f-pg-dig then do:
       assign tt-digita.nome:screen-value in browse br-digita = string(today, "99/99/9999").
   end.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
DO:
    /*  aqui que a grava‡Æo da linha da temp-table ² efetivada.
       Por‚m as valida‡äes dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment rio */
    
    if br-digita:NEW-ROW in frame f-pg-dig then 
    do transaction on error undo, return no-apply:
        create tt-digita.
        assign input browse br-digita tt-digita.lot-lote            
               INPUT BROWSE br-digita tt-digita.lot-it-codigo       
               input browse br-digita tt-digita.lot-dt-vali-lote    
               input browse br-digita tt-digita.lot-cod-estabel     
               input browse br-digita tt-digita.lot-cod-depos       
               input browse br-digita tt-digita.lot-cod-localiz     
               input browse br-digita tt-digita.lot-cod-refer       
               input browse br-digita tt-digita.lot-quantidade
               input browse br-digita tt-digita.lot-nova-refer.    
            
            br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    end.
    else do transaction on error undo, return no-apply:
        
            assign input browse br-digita tt-digita.lot-lote           
                   INPUT BROWSE br-digita tt-digita.lot-it-codigo      
                   input browse br-digita tt-digita.lot-dt-vali-lote   
                   input browse br-digita tt-digita.lot-cod-estabel    
                   input browse br-digita tt-digita.lot-cod-depos      
                   input browse br-digita tt-digita.lot-cod-localiz    
                   input browse br-digita tt-digita.lot-cod-refer      
                   input browse br-digita tt-digita.lot-quantidade    
                   input browse br-digita tt-digita.lot-nova-refer.    
                
            display
                tt-digita.lot-lote        
                tt-digita.lot-it-codigo   
                tt-digita.lot-dt-vali-lote   
                tt-digita.lot-cod-estabel 
                tt-digita.lot-cod-depos   
                tt-digita.lot-cod-localiz 
                tt-digita.lot-cod-refer   
                tt-digita.lot-quantidade  
                tt-digita.lot-nova-refer
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
            tt-digita.lot-lote        
            tt-digita.lot-it-codigo   
            tt-digita.lot-dt-vali-lote   
            tt-digita.lot-cod-estabel 
            tt-digita.lot-cod-depos   
            tt-digita.lot-cod-localiz 
            tt-digita.lot-cod-refer   
            tt-digita.lot-quantidade 
            tt-digita.lot-nova-refer
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


&Scoped-define SELF-NAME bt-retirar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retirar C-Win
ON CHOOSE OF bt-retirar IN FRAME f-pg-dig /* Retirar */
DO:
    if  br-digita:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-digita.
        delete tt-digita.
        if  br-digita:delete-current-row() in frame f-pg-dig then.
    end.
    
    if num-results("br-digita") = 0 then
        assign bt-retirar:SENSITIVE in frame f-pg-dig = no.
               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME bt-gera-ordem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-gera-ordem C-Win
ON CHOOSE OF bt-gera-ordem IN FRAME f-pg-dig /* Gera Ordem de Compra */
DO:

    run utp/ut-acomp.p persistent set h-acomp.

    {utp/ut-liter.i Transferindo Pallets * I}

    run pi-inicializar in h-acomp (input "Transferindo os Pallets ...Aguarde"). 

    assign v-num-reg-lidos = 0.


    /*aqui a nova rotina */

    DO:

        FIND FIRST PARAM-global NO-LOCK NO-ERROR.
        IF AVAIL PARAM-global THEN DO:
    

           FIND FIRST PARAM-estoq NO-LOCK NO-ERROR.
        END.

        for each tt-movto exclusive-lock: delete tt-movto. end. 
        for each tt-erro exclusive-lock:  delete tt-erro.  end.

        find param-cp     no-lock no-error.
        find param-estoq  no-lock no-error.

        assign i-empresa = param-global.empresa-prin WHEN AVAIL param-global.          

        find estabelec where
             estabelec.cod-estabel = c-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel
             no-lock no-error.  

        run cdp/cd9970.p (input rowid(estabelec),
                          output i-empresa).

        for each tt-digita no-lock:

            find item where item.it-codigo = tt-digita.lot-it-codigo 
                no-lock no-error.

            if item.cod-obsoleto = 4 then do:
               run utp/ut-msgs.p (input "show",input 17006, input "Item est  obsoleto: " + tt-digita.lot-it-codigo).
               return no-apply.
            end.

            if item.it-codigo = "" or item.tipo-contr = 4 then do:
               run utp/ut-msgs.p (input "show",input 17006, input "Item de d‚bito direto: " + tt-digita.lot-it-codigo).
               return no-apply.
            end.

        end.


           blk-do:
           do transaction on error undo blk-do, return no-apply:

               run pi-acompanhar in h-acomp (input "Gravando Transferˆncias").

               /*REQUISICAO*/
               for each tt-digita no-lock:

                   find item where item.it-codigo = tt-digita.lot-it-codigo no-lock no-error.

                   find first saldo-estoq
                        where saldo-estoq.cod-estabel = tt-digita.lot-cod-estabel
                          and saldo-estoq.it-codigo   = tt-digita.lot-it-codigo
                          AND saldo-estoq.lote        = tt-digita.lot-lote
                          and saldo-estoq.qtidade-atu > 0 
                          USE-INDEX ITEM-lote
                          no-lock no-error.

                   if not avail saldo-estoq then do:
                      run pi-finalizar in h-acomp.
                      run utp/ut-msgs.p (input "show":U,
                                         input 17006,
                                         input "Item " + tt-digita.lot-it-codigo + " sem saldo!" + "~~" + "Item nÆo possui saldo-em estoque!").
                      undo blk-do, return no-apply.
                   end.


               /*TRANSFERENCIA*/
                   find item-uni-estab
                        where item-uni-estab.it-codigo   = tt-digita.lot-it-codigo
                          and item-uni-estab.cod-estabel = tt-digita.lot-cod-estabel
                          no-lock no-error.

                   find saldo-estoq
                        where saldo-estoq.cod-estabel = tt-digita.lot-cod-estabel
                          and saldo-estoq.cod-depos   = tt-digita.lot-cod-depos
                          and saldo-estoq.cod-localiz = tt-digita.lot-cod-localiz
                          and saldo-estoq.lote        = tt-digita.lot-lote
                          and saldo-estoq.it-codigo   = tt-digita.lot-it-codigo
                          and saldo-estoq.cod-refer   = tt-digita.lot-cod-refer
                          no-lock no-error.
                   if not avail saldo-estoq then do:
                      run pi-finalizar in h-acomp.
                      run utp/ut-msgs.p (input "show":U,
                                         input 17006,
                                         input "Saldo nÆo encontrado para o item " + tt-digita.lot-it-codigo).
                      undo blk-do, return no-apply.
                   end.

                   /*Saida*/
                   create tt-movto.
                   assign tt-movto.cod-versao-integracao  = 1
                          tt-movto.cod-prog-orig          = "espd0021"
                          tt-movto.tipo-trans             = 2
                          tt-movto.esp-docto              = 33
                          tt-movto.ct-codigo              = param-estoq.ct-tr-transf
                          tt-movto.sc-codigo              = param-estoq.sc-tr-transf
                          tt-movto.dt-trans               = data-trans-jr
                          tt-movto.dt-vali-lote           = tt-digita.lot-dt-vali-lote
                          tt-movto.nro-docto              = "0001"
                          tt-movto.serie-docto            = " "
                          tt-movto.cod-depos              = saldo-estoq.cod-depos
                          tt-movto.cod-estabel            = saldo-estoq.cod-estabel
                          tt-movto.it-codigo              = saldo-estoq.it-codigo
                          tt-movto.cod-refer              = saldo-estoq.cod-refer
                          tt-movto.cod-localiz            = saldo-estoq.cod-localiz
                          tt-movto.lote                   = saldo-estoq.lote
                          tt-movto.quantidade             = tt-digita.lot-quantidade
                          tt-movto.un                     = item.un
                          tt-movto.usuario                = c-seg-usuario.


                   /*Entrada */
                   create tt-movto.
                   assign tt-movto.cod-versao-integracao  = 1
                          tt-movto.ct-codigo              = param-estoq.ct-tr-transf
                          tt-movto.sc-codigo              = param-estoq.sc-tr-transf
                          tt-movto.cod-prog-orig          = "espd0021"
                          tt-movto.tipo-trans             = 1
                          tt-movto.esp-docto              = 33
                          tt-movto.dt-trans               = data-trans-jr
                          tt-movto.dt-vali-lote           = tt-digita.lot-dt-vali-lote
                          tt-movto.nro-docto              = "0001"
                          tt-movto.serie-docto            = " "
                          tt-movto.cod-depos              = saldo-estoq.cod-depos   
                          tt-movto.cod-estabel            = saldo-estoq.cod-estabel 
                          tt-movto.it-codigo              = saldo-estoq.it-codigo   
                          tt-movto.cod-refer              = tt-digita.lot-nova-refer   
                          tt-movto.cod-localiz            = saldo-estoq.cod-localiz 
                          tt-movto.lote                   = saldo-estoq.lote        
                          tt-movto.quantidade             = tt-digita.lot-quantidade
                          tt-movto.un                     = item.un
                          tt-movto.usuario                = c-seg-usuario.

               end. /*for each tt-digita*/

               run pi-acompanhar in h-acomp (input "Efetivando Transa‡Æo...Aguarde...").

               run cep/ceapi001.p (input-output table tt-movto,
                                   input-output table tt-erro,
                                   input yes).     

               FIND FIRST tt-erro NO-LOCK NO-ERROR.
               IF AVAIL tt-erro or return-value = "NOK":U THEN DO:
                  run pi-finalizar in h-acomp.
                  run cdp/cd0666.w (input table tt-erro).
                  undo blk-do, return no-apply.
               END.

           END.  /* do transaction */

    END.  /* do: */

    RUN pi-finalizar IN h-acomp.

    /* Transfere os pallets para a nova referencia */

    FOR EACH tt-digita EXCLUSIVE-LOCK.

        FIND FIRST pallet  WHERE
            pallet.cod-estabel = tt-digita.lot-cod-estabel AND
            pallet.it-codigo   = tt-digita.lot-it-codigo   AND
            pallet.nr-pallet   = tt-digita.lot-lote
            exclusive-LOCK NO-ERROR.

        IF AVAIL pallet THEN DO:

            ASSIGN pallet.cod-refer = tt-digita.lot-nova-refer.

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

            ASSIGN c-mensagem = "Pallet NÆo Existe".

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

    ASSIGN data-trans-jr = TODAY.

    FIND FIRST pol-param-estab WHERE
        pol-param-estab.cod-estabel = c-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel
        NO-LOCK NO-ERROR.

    IF AVAIL pol-param-estab AND data-trans-jr > pol-param-estab.data-palete THEN
        ASSIGN data-trans-jr = pol-param-estab.data-palete.



    FIND FIRST ITEM WHERE 
        ITEM.it-codigo = c-it-codigo:SCREEN-VALUE IN FRAME f-pg-sel
        NO-LOCK NO-ERROR.

    IF NOT AVAIL ITEM OR ITEM.it-codigo = " " THEN DO:
       
       run utp/ut-msgs.p (input "show":U, input 2, "Item").
       APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
       return NO-apply.

    END. 

    FIND ped-venda WHERE 
        ped-venda.nr-pedido = INT(i-pd-origem:SCREEN-VALUE IN FRAME f-pg-sel)
        NO-LOCK NO-ERROR.
    
    IF NOT AVAIL ped-venda THEN DO:
       
       run utp/ut-msgs.p (input "show":U, input 2, "Pedido de Venda").
       APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
       return NO-apply.
    
    END.

    ELSE DO:

       IF ped-venda.cod-sit-ped > 2 THEN DO: 
           run utp/ut-msgs.p (input "show":U, input 7069, " ").
           APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
           return NO-apply.

       END.

       FIND FIRST ped-item OF ped-venda WHERE
           ped-item.nr-sequencia = INT (i-sq-origem:SCREEN-VALUE IN FRAME f-pg-sel) AND
           ped-item.ind-componen  <> 3 
           NO-LOCK NO-ERROR.
       
       IF NOT AVAIL ped-item THEN DO:

           run utp/ut-msgs.p (input "show":U, input 32582, "Pedido sem o Item").
           APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
           return NO-apply.

       END.

       ASSIGN cod-refer-ped = ped-item.cod-refer.

    END.


    CLOSE QUERY br-digita.

    FOR EACH tt-digita.
        DELETE tt-digita.
    END.

    ASSIGN i-pd-origem-jr  = ped-venda.nr-pedido
           i-sq-origem-jr  = ped-item.nr-sequencia.

    /** Montando a tabela para tt-digita ***/

    run utp/ut-acomp.p persistent set h-acomp.

    {utp/ut-liter.i Encontrando Registros * I}

    run pi-inicializar in h-acomp (input "Carregando Paletes ...Aguarde"). 


    FOR EACH pallet NO-LOCK WHERE
           pallet.nr-pedido    = i-pd-origem-jr and 
           pallet.nr-sequencia = i-sq-origem-jr AND
           pallet.cod-estabel = c-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel AND
           pallet.it-codigo   = ped-item.it-codigo  and
           pallet.nr-sequencia = ped-item.nr-sequencia
           USE-INDEX pedido ,

      each item NO-LOCK
           where item.it-codigo = pallet.it-codigo 
           USE-INDEX codigo:

        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

        FOR EACH saldo-estoq NO-LOCK WHERE
             saldo-estoq.cod-estabel  = pallet.cod-estabel  AND
             saldo-estoq.lote         = pallet.nr-pallet    AND
             saldo-estoq.it-codigo    = pallet.it-codigo    AND
             saldo-estoq.cod-refer   <> ped-item.cod-refer  AND
             saldo-estoq.qtidade-atu > 0
            USE-INDEX lote :

            IF (saldo-estoq.qtidade-atu -    
               (saldo-estoq.qt-alocada   +    
                saldo-estoq.qt-aloc-prod +   
                saldo-estoq.qt-aloc-ped)) <= 0 THEN NEXT.
             
            FIND tt-digita WHERE
                 tt-digita.lot-cod-estabel    = saldo-estoq.cod-estabel AND
                 tt-digita.lot-cod-depos      = saldo-estoq.cod-depos   AND
                 tt-digita.lot-cod-localiz    = saldo-estoq.cod-localiz AND
                 tt-digita.lot-lote           = saldo-estoq.lote        AND
                 tt-digita.lot-it-codigo      = saldo-estoq.it-codigo   AND
                 tt-digita.lot-cod-refer      = saldo-estoq.cod-refer
                 NO-ERROR.

            IF NOT AVAIL tt-digita THEN DO:
                 CREATE tt-digita.

                 ASSIGN tt-digita.lot-cod-estabel    = saldo-estoq.cod-estabel 
                        tt-digita.lot-cod-depos      = saldo-estoq.cod-depos   
                        tt-digita.lot-cod-localiz    = saldo-estoq.cod-localiz 
                        tt-digita.lot-lote           = saldo-estoq.lote        
                        tt-digita.lot-it-codigo      = saldo-estoq.it-codigo   
                        tt-digita.lot-cod-refer      = saldo-estoq.cod-refer
                        tt-digita.lot-nova-refer     = cod-refer-ped.
            END.

            ASSIGN tt-digita.lot-dt-vali-lote  = saldo-estoq.dt-vali-lote 
                   tt-digita.lot-quantidade    = saldo-estoq.qtidade-atu -
                                                 (saldo-estoq.qt-alocada   +
                                                  saldo-estoq.qt-aloc-prod + 
                                                  saldo-estoq.qt-aloc-ped).

        END.  /* Saldo Estoq */

    END. /*FOR EACH pallet*/

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
          apply 'entry' to tt-digita.lot-cod-estabel in browse br-digita. 
       end.
    
    enable bt-retirar  bt-gera-ordem with frame f-pg-dig.
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
          apply 'entry' to tt-digita.lot-cod-estabel in browse br-digita. 
       end.
    
    enable bt-retirar bt-gera-ordem with frame f-pg-dig.
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

assign v-cod-prog-gerado = "espd0021".


find first usuar_mestre
     where usuar_mestre.cod_usuario = c-seg-usuario no-lock no-error.

if avail usuar_mestre then 


def var c-tit as char no-undo.

run grapi/gr2013a.p (input v-cod-prog-gerado,
                    input "Ajusta Referˆncia do Pallet p/ a Refer. do Pedido",
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


 /*include de inicializa‡Æo do relat¢rio */

 /*inicializa‡äes do template de relat¢rio */

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
** Tradu‡Æo p gina sele‡Æo - frame f-pg-sel
**********************************************************/
create text wh-label-sel
    assign frame        = frame f-relat:handle
           format       = "x(07)"
           font         = 1
           screen-value = "Sele‡Æo"
           width        = 8
           row          = 1.8
           col          = im-pg-sel:col in frame f-relat + 1.86
           visible      = yes
     triggers:
         on mouse-select-click
            apply "mouse-select-click" to im-pg-sel in frame f-relat.           
     end triggers.                   
/********************************************************** 
** Tradu‡Æo p gina Digita‡Æo - frame f-pg-dig
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
** Tradu‡Æo p gina parƒmetros - frame f-pg-par
**********************************************************/
create text wh-label-par
    assign frame        = frame f-relat:handle
           format       = "x(10)"
           font         = 1
           screen-value = "Parƒmetros"
           width        = 11
           row          = 1.8
           col          = im-pg-par:col in frame f-relat + 1.7
           visible      = yes
     triggers:
         on mouse-select-click
            apply "mouse-select-click" to im-pg-par in frame f-relat.           
     end triggers.
/********************************************************** 
** Tradu‡Æo p gina impressÆo - frame f-pg-imp
**********************************************************/
create text wh-label-imp
    assign frame        = frame f-relat:handle
           format       = "x(10)"
           font         = 1
           screen-value = "ImpressÆo"
           width        = 10
           row          = 1.8
           col          = im-pg-imp:col in frame f-relat + 1.7
           visible      = yes
     triggers:
         on mouse-select-click
            apply "mouse-select-click" to im-pg-imp in frame f-relat.           
     end triggers.                   


/********************************************************** 
** Troca de p gina por CTRL-TAB e SHIFT-CTRL-TAB
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
** Procedure de troca de p gina por CTRL-TAB 
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


  assign wh-label-imp:screen-value = "ImpressÆo".
PAUSE 0 BEFORE-HIDE.

MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    IF SESSION:SET-WAIT-STATE("":U) THEN.
        c-cod-estabel =  STRING({cdp\poloestab.i 422}). /*solic-318*/ 
    RUN enable_UI.

    ASSIGN text-destino:screen-value   IN FRAME f-pg-imp = "Destino".
    ASSIGN text-modo:screen-value      IN FRAME f-pg-imp = "Execu‡Æo".
    ASSIGN text-parametro:screen-value IN FRAME f-pg-imp = "Parƒmetros de ImpressÆo".
    
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
    run pi-muda-cor-label-folder(input "Sele‡Æo").

    end.

    if  im-pg-dig:sensitive in frame f-relat = no then do:
    run pi-muda-cor-label-folder(input "Paletes").

    end.

    if  im-pg-par:sensitive in frame f-relat = no then do:
        run pi-muda-cor-label-folder(input "Parƒmetros").

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
     i-pd-origem
     i-sq-origem
     c-it-codigo
     c-nome-cliente

   WITH FRAME f-pg-sel IN WINDOW C-Win.

   ENABLE  
       c-cod-estabel
       i-pd-origem
       i-sq-origem

   WITH FRAME f-pg-sel IN WINDOW C-Win.
   
   DISPLAY rs-destino c-arquivo rs-execucao tb-parametro rs-formato
   WITH FRAME f-pg-imp IN WINDOW C-Win.
   ENABLE RECT-7 rs-destino bt-arquivo bt-config-impr c-arquivo RECT-9 rect-10 rs-execucao tb-parametro rs-formato
   WITH FRAME f-pg-imp IN WINDOW C-Win.
   
   DISPLAY 
   WITH FRAME f-pg-par IN WINDOW C-Win.

   ENABLE 
   WITH FRAME f-pg-par IN WINDOW C-Win.


  ENABLE br-digita bt-retirar 
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

              assign tt-param.cod-estabel      = input frame f-pg-sel c-cod-estabel.   

           {include/i-rpexb.i}

           if  session:set-wait-state("general":U) then.

           {include/i-rprun.i pdp/espd0021rp.p} 

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

