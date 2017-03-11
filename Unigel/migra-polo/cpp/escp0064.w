&Scoped-define WINDOW-NAME C-Win

/*------------------------------------------------------------------------
File.............: escp0064.w
Description......: Resumo de Rodadas de EP
Input Parameters : 
Output Parameters: 
Author...........: DATASUL S.A.
Created..........: 21/07/2011 - 11:11 - Edson|Bruno - amgra
OBS..............: Otimizaá∆o de Performance em 13/03/2007
------------------------------------------------------------------------*/
define buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "escp0064".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

CREATE WIDGET-POOL. 

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */

&GLOBAL-DEFINE PGSEL f-pg-sel 
&GLOBAL-DEFINE PGPAR f-pg-par 
&GLOBAL-DEFINE PGIMP f-pg-imp 

/* Include Com as Vari†veis Globais */

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

/*define temp-table tt-raw-digita
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
    field ep-codigo            as char
    field c-cod-estabel-ini    like movto-estoq.cod-estabel
    field c-dt-trans-ini       like movto-estoq.dt-trans
    field c-dt-trans-fim       like movto-estoq.dt-trans
    field c-it-codigo-ini      like movto-estoq.it-codigo
    field c-it-codigo-fim      like movto-estoq.it-codigo
    field c-lote-ini           like movto-estoq.lote
    field c-lote-fim           like movto-estoq.lote
    field c-maq-ini            AS INT 
    field c-maq-fim            AS int
    field c-param-corte        AS int
    field c-param-status       AS int   
    field c-param-recorte      AS LOGICAL    
.*/



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

def var h-acomp              as handle no-undo.

/* Transfe Definitions */

def var raw-param        as raw no-undo.

/* Local Variable Definitions ---                                       */ 

DEFINE TEMP-TABLE tt-millRoll
    FIELD lote like movto-estoq.lote
    FIELD nr-ord-produ LIKE movto-estoq.nr-ord-produ
    FIELD linha AS INTEGER
    FIELD it-codigo LIKE movto-estoq.it-codigo
    FIELD saldo LIKE saldo-estoq.qtidade-atu
    FIELD qt-produzida LIKE movto-estoq.quantidade
    FIELD cod-estabel LIKE  movto-estoq.cod-estabel
    INDEX lote IS PRIMARY   UNIQUE
                         lote 
                         it-codigo
                         cod-estabel
                         linha.


define temp-table tt-lote NO-UNDO
    field nr-linha        AS   INT  
    field it-codigo       AS   CHAR 
    field cod-exame       AS   INT  
    field cod-comp        AS   INT  
    FIELD descricao       AS   CHAR FORMAT "X(60)" 
    field nr-lote         AS   CHAR 
    FIELD nr-ord-produ LIKE movto-estoq.nr-ord-produ
    field menor-result    AS   DEC  
    field maior-result    AS   DEC  
    field soma-result     AS   DEC
    field ultima-seqa     AS   INT 
    field resultado       AS   DEC  EXTENT 50 
    INDEX chave IS PRIMARY UNIQUE nr-linha
                                  it-codigo  
                                  cod-exame
                                  cod-comp   
                                  nr-lote.



DEFINE TEMP-TABLE tt-cortadas
      

    FIELD cod-estabel  LIKE movto-estoq.cod-estabel
    FIELD nr-ord-produ like movto-estoq.nr-ord-produ
    FIELD lote         like movto-estoq.lote
    FIELD it-codigo    LIKE movto-estoq.it-codigo
    FIELD saldo        LIKE saldo-estoq.qtidade-atu
    FIELD qt-produzida LIKE movto-estoq.quantidade
    FIELD qt-emenda    LIKE movto-estoq.quantidade
    FIELD dt-produzida LIKE movto-estoq.dt-trans
    FIELD lote-origem  AS CHAR 
    FIELD nr-pallet    like pallet.nr-pallet
    FIELD nr-pedido    LIKE ped-venda.nr-pedido
    FIELD dt-embarque  LIKE nota-fiscal.dt-emis-nota
    FIELD nr-nota-fis  LIKE nota-fiscal.nr-nota-fis
    FIELD nome-abrev   LIKE ped-venda.nome-abrev
    INDEX lote IS PRIMARY   UNIQUE
                         cod-estabel 
                         it-codigo   
                         lote        
                         nr-ord-produ.

DEFINE TEMP-TABLE tt-cortadas-prim
      

    FIELD cod-estabel  LIKE movto-estoq.cod-estabel
    FIELD nr-ord-produ like movto-estoq.nr-ord-produ
    FIELD lote         like movto-estoq.lote
    FIELD it-codigo    LIKE movto-estoq.it-codigo
    FIELD saldo        LIKE saldo-estoq.qtidade-atu
    FIELD qt-produzida LIKE movto-estoq.quantidade
    FIELD qt-emenda    LIKE movto-estoq.quantidade
    FIELD dt-produzida LIKE movto-estoq.dt-trans
    FIELD lote-origem  AS CHAR 
    FIELD nr-pallet    like pallet.nr-pallet
    FIELD nr-pedido    LIKE ped-venda.nr-pedido
    FIELD dt-embarque  LIKE nota-fiscal.dt-emis-nota
    FIELD nr-nota-fis  LIKE nota-fiscal.nr-nota-fis
    FIELD nome-abrev   LIKE ped-venda.nome-abrev
    INDEX lote IS PRIMARY   UNIQUE
                         cod-estabel 
                         it-codigo   
                         lote        
                         nr-ord-produ.
                         
                         

DEFINE TEMP-TABLE tt-bobinas-def
      

    FIELD it-codigo LIKE  lote-carac-tec.it-codigo
    FIELD sequencia LIKE  lote-res-carac.sequencia
    FIELD nr-tabela LIKE  lote-res-carac.nr-tabela
    FIELD defeito    AS CHAR
    FIELD quantidade    AS INTEGER
    FIELD peso          AS DECIMAL
    FIELD percent       AS DECIMAL

    INDEX lote IS PRIMARY   UNIQUE
                         defeito
                         it-codigo
                         sequencia
                         nr-tabela
                      .


DEFINE VARIABLE soma-result    AS INTEGER    NO-UNDO.
DEFINE VARIABLE soma-col       AS INTEGER    NO-UNDO.
DEFINE VARIABLE seq-jr         AS INTEGER    NO-UNDO.
DEFINE VARIABLE descricao-jr   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE it-codigo-ant  AS CHARACTER  INITIAL "ZZZZZZZZ" NO-UNDO.
DEFINE VARIABLE nr-linha-ant   AS INTEGER    INITIAL 9999       NO-UNDO.
DEFINE VARIABLE cod-exame-ant  AS INTEGER    INITIAL 99999999   NO-UNDO.
DEFINE VARIABLE cod-comp-ant   AS INTEGER    INITIAL 99999999   NO-UNDO.
DEFINE VARIABLE col-jr         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-exames        AS CHARACTER FORMAT "x(100)"  NO-UNDO.
define buffer b-movto-mat for movto-mat.
define buffer b-movto-estoq for movto-estoq.
def var i-nr-trans as integer no-undo.
def var l-req as logical no-undo.

 DEFINE BUFFER bf-ITEM   FOR ITEM.
  DEFINE BUFFER bf-movto-estoq   FOR movto-estoq.

 DEFINE BUFFER bf-movto-mat     FOR movto-mat.
 DEFINE BUFFER bf-lote-rastreab FOR lote-rastreab.
 DEFINE BUFFER bo-lote-rastreab FOR lote-rastreab.
 DEFINE VARIABLE l-log AS LOGICAL     NO-UNDO.

 DEFINE VARIABLE c-cod-estabel-fim like movto-estoq.cod-estabel INITIAL 422.
 DEFINE VARIABLE da-dt-trans-ini   like movto-estoq.dt-trans      INITIAL 04/01/2007.
 DEFINE VARIABLE da-dt-trans-fim   like movto-estoq.dt-trans      INITIAL 04/30/2007.
 DEFINE VARIABLE i-linha-ini AS INTEGER    NO-UNDO.
 DEFINE VARIABLE i-linha-fim AS INTEGER  INITIAL 999  NO-UNDO.
 DEFINE VARIABLE c-item-ini LIKE ITEM.it-codigo  NO-UNDO.
 DEFINE VARIABLE c-item-fim LIKE ITEM.it-codigo INITIAL "ZZZ"  NO-UNDO.
 DEFINE VARIABLE i-nr-ord-produ-ini AS INTEGER LABEL "Ord. Producao"   NO-UNDO.
 DEFINE VARIABLE i-nr-ord-produ-fim AS INTEGER LABEL "Ord. Producao" INITIAL 99999999  NO-UNDO.
 DEFINE VARIABLE d-total-prod AS DECIMAL     FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Total Produzido"  NO-UNDO.
 DEFINE VARIABLE d-total-saldo AS DECIMAL    FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Total Saldo"   NO-UNDO.
 DEFINE VARIABLE c-millrolls AS CHARACTER     LABEL "Jumbos"  NO-UNDO.
 DEFINE VARIABLE c-it-codigo-rel  AS CHARACTER   FORMAT "x(16)" LABEL "Item"  NO-UNDO.
 DEFINE VARIABLE dt-data-producao AS DATE    FORMAT "99/99/9999" LABEL "Data Producao"      NO-UNDO.
 DEFINE VARIABLE d-total-cortada AS DECIMAL    FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Total Cortado"   NO-UNDO.
 DEFINE VARIABLE d-quantidade AS DECIMAL     FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Quantidade"  NO-UNDO.
 DEFINE VARIABLE d-quantidade-aux AS DECIMAL     FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Quantidade"  NO-UNDO.

 DEFINE VARIABLE r-maior AS ROWID      NO-UNDO.
 DEFINE VARIABLE d-perc AS DECIMAL    FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Perc"   NO-UNDO.
 DEFINE VARIABLE d-maior AS DECIMAL  FORMAT "->>>,>>>,>>>,>>9.99" LABEL "Maior"  NO-UNDO.


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

/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 

/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 
def new shared var c-cod-estabel-ini    like movto-estoq.cod-estabel FORMAT "x(3)"       INITIAL "422" NO-UNDO.
def new shared var c-dt-trans-ini       like movto-estoq.dt-trans    FORMAT "99/99/9999" INITIAL today NO-UNDO. 
def new shared var c-dt-trans-fim       like movto-estoq.dt-trans    FORMAT "99/99/9999" INITIAL today NO-UNDO. 
def new shared var c-it-codigo-ini      like movto-estoq.it-codigo   FORMAT "x(15)"      INITIAL "" NO-UNDO. 
def new shared var c-it-codigo-fim      like movto-estoq.it-codigo   FORMAT "x(15)"      INITIAL "ZZZZZZZZZZZZZZZ" NO-UNDO. 
def new shared var c-lote-ini           like movto-estoq.lote        FORMAT "x(15)"      INITIAL "" NO-UNDO.
def new shared var c-lote-fim           like movto-estoq.lote        FORMAT "x(15)"      INITIAL "ZZZZZZZZZZZZZZZ" NO-UNDO.
def new shared var c-maq-ini            AS INT                       FORMAT ">>9"        INITIAL 0 NO-UNDO.
def new shared var c-maq-fim            AS int                       FORMAT ">>9"        INITIAL 999 NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */ 

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
    

DEFINE IMAGE IMAGE-51
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-52
    FILENAME "image\im-las"
    SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-12
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-21
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-22
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-31
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-32
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-41
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-42
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

DEFINE VARIABLE c-param-corte AS INTEGER INITIAL 3 
VIEW-AS RADIO-SET VERTICAL  
RADIO-BUTTONS  
"Prim†ria", 1,
"Secund†ria", 2,
"Ambas", 3
SIZE 12 BY 2.5 NO-UNDO.

DEFINE VARIABLE c-param-status AS INTEGER INITIAL 2 
VIEW-AS RADIO-SET VERTICAL 
RADIO-BUTTONS 
"Reprovadas", 1,
"Todas-Repr/Aprov", 2
SIZE 18 BY 1.8 NO-UNDO.

DEFINE VARIABLE c-param-recorte AS LOGICAL INITIAL yes 
LABEL "Mostra Bobs.Consumidas no Recorte?"
VIEW-AS TOGGLE-BOX 
SIZE 30 BY .83 
NO-UNDO.

DEFINE VARIABLE tb-parametro AS LOGICAL INITIAL no 
LABEL "Imprimir P†gina de ParÉmetros"
VIEW-AS TOGGLE-BOX 
SIZE 32 BY .83 
NO-UNDO.

DEFINE VARIABLE rs-formato AS INTEGER INITIAL 2 
VIEW-AS RADIO-SET HORIZONTAL 
RADIO-BUTTONS 
"80 colunas", 1,
"132 colunas", 2
SIZE 32 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-7 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 46.29 BY 1.69.

DEFINE RECTANGLE RECT-10 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 46.29 BY 3.50.

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

DEFINE RECTANGLE RECT-21
         EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
         SIZE 18 BY 4.

DEFINE RECTANGLE RECT-22
         EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
         SIZE 48 BY 4.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
    bt-executar AT ROW 14.54 COL 3 HELP
"Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
"Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
"Ajuda"
     im-pg-sel AT ROW 1.5 COL 2.14
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-imp AT ROW 1.5 COL 33.58
     "                "AT ROW 1.8 COL 66.00
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
     DEFAULT-BUTTON bt-executar.

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
   c-cod-estabel-ini label "Estabelecimento"
     at row 1 col 22 colon-aligned
     view-as fill-in 
     size 4 by .88
     font 1
   
   c-dt-trans-ini label "Per°odo"
     at row 2 col 22 colon-aligned
     view-as fill-in 
     size 10 by .88
     font 1

    c-dt-trans-fim no-label 
      at row 2 col 50 colon-aligned
      view-as fill-in 
      size 10 by .88
      font 1

    c-it-codigo-ini label "Filme"
      at row 3 col 22 colon-aligned
      view-as fill-in 
      size 16 by .88
      font 1

     c-it-codigo-fim no-label 
       at row 3 col 50 colon-aligned
       view-as fill-in 
       size 16 by .88
       font 1

    c-lote-ini label "Mill Roll"
      at row 4 col 22 colon-aligned
      view-as fill-in 
      size 16 by .88
      font 1

     c-lote-fim no-label 
       at row 4 col 50 colon-aligned
       view-as fill-in 
       size 16 by .88
       font 1

    c-maq-ini label "M†quina"
      at row 5 col 22 colon-aligned
      view-as fill-in 
      size 4 by .88
      font 1

     c-maq-fim no-label 
       at row 5 col 50 colon-aligned
       view-as fill-in 
       size 4 by .88
       font 1

     i-nr-ord-produ-ini label "Ord.Produá∆o"
      at row 6 col 22 colon-aligned
      view-as fill-in 
      size 10 by .88
      font 1

     i-nr-ord-produ-fim  no-label 
       at row 6 col 50 colon-aligned
       view-as fill-in 
       size 10 by .88
       font 1

    
   IMAGE-51 AT ROW 02.00 COL 42
   IMAGE-52 AT ROW 02.00 COL 47
        

   IMAGE-11 AT ROW 03.00 COL 42
   IMAGE-12 AT ROW 03.00 COL 47
   IMAGE-21 AT ROW 04.00 COL 42
   IMAGE-22 AT ROW 04.00 COL 47
   IMAGE-31 AT ROW 05.00 COL 42
   IMAGE-32 AT ROW 05.00 COL 47
   IMAGE-41 AT ROW 06.00 COL 42
   IMAGE-42 AT ROW 06.00 COL 47
   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D
   AT COL 3 ROW 2.85
   SIZE 76.86 BY 10.62.

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
   TITLE              = "Resumo de Rodadas de EP - escp0064"
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

ON CHOOSE OF bt-executar IN FRAME f-relat
DO:
   do  on error undo, return no-apply:
        run pi-executar.
    end.
END.

ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
   /*run pi-troca-pagina.    */
END.

ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
  run pi-troca-pagina.
END.

ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
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

assign v-cod-prog-gerado = "escp0064".


def var c-tit as char no-undo.

run grapi/gr2013a.p (input v-cod-prog-gerado,
                    input "Resumo de Rodadas de EP",
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
assign c-terminal = "Terminal".

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
    RUN enable_UI.
    
    ASSIGN text-destino:screen-value   IN FRAME f-pg-imp = "Destino".
    ASSIGN text-modo:screen-value      IN FRAME f-pg-imp = "Execuá∆o".
    ASSIGN text-parametro:screen-value IN FRAME f-pg-imp = "ParÉmetros de Impress∆o".

    assign /*rs-destino:radio-buttons in frame f-pg-imp = {varinc/var00002.i 07}*/
           rs-destino:screen-value  in frame f-pg-imp = "4":U.
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

     view c-win.
     apply "entry" to frame f-Relat.
     apply "entry" to c-win.

    if  im-pg-sel:sensitive in frame f-relat = no then do:
    run pi-muda-cor-label-folder(input "Seleá∆o").

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
   ENABLE bt-executar bt-cancelar bt-ajuda im-pg-imp im-pg-sel
   WITH FRAME f-relat IN WINDOW C-Win.
   
   DISPLAY  
   c-cod-estabel-ini
   c-dt-trans-ini
   c-dt-trans-fim
   c-it-codigo-ini
   c-it-codigo-fim
   c-lote-ini
   c-lote-fim
   c-maq-ini
   c-maq-fim
   i-nr-ord-produ-ini
   i-nr-ord-produ-fim
   
   WITH FRAME f-pg-sel IN WINDOW C-Win.

   ENABLE   
       c-cod-estabel-ini
       c-dt-trans-ini
       c-dt-trans-fim
       c-it-codigo-ini
       c-it-codigo-fim
       c-lote-ini
       c-lote-fim
       c-maq-ini
       c-maq-fim
       i-nr-ord-produ-ini
       i-nr-ord-produ-fim
   WITH FRAME f-pg-sel IN WINDOW C-Win.
   
   DISPLAY rs-destino c-arquivo rs-execucao tb-parametro rs-formato
   WITH FRAME f-pg-imp IN WINDOW C-Win.
   ENABLE RECT-7 rs-destino bt-arquivo bt-config-impr c-arquivo RECT-9 rect-10 rs-execucao tb-parametro rs-formato
   WITH FRAME f-pg-imp IN WINDOW C-Win.
   
   DISPLAY 
   WITH FRAME f-pg-par IN WINDOW C-Win.

   ENABLE 
   WITH FRAME f-pg-par IN WINDOW C-Win.
   
   VIEW C-Win.
END PROCEDURE.

PROCEDURE local-exit :
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN.
END PROCEDURE.

PROCEDURE pi-executar :

    FOR EACH tt-millRoll.
        DELETE tt-millRoll.
    END.

    FOR EACH tt-lote.
        DELETE tt-lote.
    END.

    FOR EACH tt-cortadas-prim.
        DELETE tt-cortadas-prim.
    END.

    FOR EACH tt-cortadas.
        DELETE tt-cortadas.
    END.


    FOR EACH tt-bobinas-def.
        DELETE tt-bobinas-def.
    END.   
    
    ASSIGN
     soma-result   = 0  
     soma-col      = 0  
     seq-jr        = 0  
     descricao-jr  = "" 
     it-codigo-ant = "ZZZZZZZZ" 
     nr-linha-ant  = 9999 
     cod-exame-ant = 99999999   
     cod-comp-ant  = 99999999   
     col-jr        = "" 
     c-exames      = "" 
     l-log         = NO
     d-total-prod  = 0 
     d-total-saldo = 0 
     c-millrolls   = ""
     c-it-codigo-rel  = "" 
     dt-data-producao = ? 
     d-total-cortada  = 0
     d-quantidade     = 0 
     d-quantidade-aux = 0
     d-perc  = 0   
     d-maior = 0
     c-arq   = ""              
     c-arq-anexo = "".      
        
        
        
        
        
        
         

    
    
    assign c-cod-estabel-ini = input frame f-pg-sel c-cod-estabel-ini 
           da-dt-trans-ini   = input frame f-pg-sel c-dt-trans-ini   
           da-dt-trans-fim   = input frame f-pg-sel c-dt-trans-fim   
           c-it-codigo-ini   = input frame f-pg-sel c-it-codigo-ini  
           c-it-codigo-fim   = input frame f-pg-sel c-it-codigo-fim  
           c-lote-ini        = input frame f-pg-sel c-lote-ini       
           c-lote-fim        = input frame f-pg-sel c-lote-fim       
           c-maq-ini         = input frame f-pg-sel c-maq-ini        
           c-maq-fim         = input frame f-pg-sel c-maq-fim        
           i-nr-ord-produ-ini = input frame f-pg-sel i-nr-ord-produ-ini 
           i-nr-ord-produ-fim = input frame f-pg-sel i-nr-ord-produ-fim.
          

 
   



    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

     ASSIGN c-modelo-planilha = search("modelos\mod-escp0064.xls") 
         c-arq             = SESSION:TEMP-DIRECTORY.

     RUN pi-cria-planilha.

 
     CONTADOR = 0.


       FOR each movto-estoq no-lock
         where movto-estoq.esp-docto = 1 AND
               movto-estoq.cod-estabel = c-cod-estabel-ini and 
               movto-estoq.dt-trans    >= da-dt-trans-ini and 
               movto-estoq.dt-trans    <= da-dt-trans-fim AND
               movto-estoq.cod-depos   <> "ARC" AND movto-estoq.quantidade <> 0 AND
               movto-estoq.it-codigo   >= c-it-codigo-ini AND
               movto-estoq.it-codigo   <= c-it-codigo-fim AND
               movto-estoq.lote        >= c-lote-ini AND
               movto-estoq.lote        <= c-lote-fim and
               movto-estoq.nr-ord-produ >= i-nr-ord-produ-ini  AND
               movto-estoq.nr-ord-produ <= i-nr-ord-produ-fim
           USE-INDEX esp-data,
            EACH  ITEM WHERE 
               ITEM.it-codigo   =  movto-estoq.it-codigo  AND
               item.ge-codigo >= 41 AND
               item.ge-codigo <= 42 AND
               (INDEX (ITEM.it-codigo,"MR",1) > 0)
              NO-LOCK  USE-INDEX codigo,
       
        EACH ord-prod WHERE ord-prod.nr-ord-produ = movto-estoq.nr-ord-produ AND
                            ord-prod.nr-linha    >= c-maq-ini AND
                            ord-prod.nr-linha    <= c-maq-fim NO-LOCK USE-INDEX codigo.

       
      FIND FIRST movto-mat WHERE
        movto-mat.nr-reporte = movto-estoq.nr-reporte AND
        movto-mat.esp-docto = 8
        USE-INDEX reporte
        NO-LOCK NO-ERROR.

        IF AVAIL movto-mat THEN NEXT. 

        CONTADOR = CONTADOR + 1.
        run pi-acompanhar in h-acomp(input "Movimentos Extrus∆o: " +  STRING(CONTADOR)).


      /*  FIND FIRST tt-millRoll NO-ERROR.
        IF AVAIL tt-millRoll AND tt-millRoll.it-codigo <> movto-estoq.it-codigo THEN NEXT.
        */
        FIND FIRST tt-millRoll WHERE  tt-millRoll.lote        = movto-estoq.lote AND
                                      tt-millRoll.cod-estabel = movto-estoq.cod-estabel AND
                                      tt-millRoll.it-codigo   = movto-estoq.it-codigo AND
                                      tt-millRoll.linha       = ord-prod.nr-linha NO-ERROR.  
       
        IF NOT AVAIL tt-millRoll THEN DO:
       
           CREATE tt-millRoll.
           ASSIGN 
               tt-millRoll.cod-estabel  = movto-estoq.cod-estabel
               tt-millRoll.lote         = movto-estoq.lote
               tt-millRoll.it-codigo    = movto-estoq.it-codigo
               tt-millRoll.linha        = ord-prod.nr-linha
               tt-millRoll.nr-ord-produ = ord-prod.nr-ord-produ
               tt-millRoll.saldo        = 0
               tt-millRoll.qt-produzida = 0.

            IF c-it-codigo-rel = "" THEN c-it-codigo-rel  =  movto-estoq.it-codigo.
            IF dt-data-producao = ? THEN dt-data-producao = movto-esto.dt-trans.
              
        END.
        
        ASSIGN
            tt-millRoll.qt-produzida = tt-millRoll.qt-produzida + movto-estoq.quantidade.
        

      END.


       
    ASSIGN    
        d-total-prod = 0
        c-millrolls = ""
        d-total-saldo = 0
        d-quantidade = 0
        d-quantidade-aux = 0
        d-total-cortada = 0.
        contador = 0.
    FOR EACH tt-millRoll.
        CONTADOR = CONTADOR + 1.
       run pi-acompanhar in h-acomp(input "Localizando Destino: " +  STRING(CONTADOR) + " - " + tt-millRoll.lote).


        d-total-prod = d-total-prod + tt-millRoll.qt-produzida.
        c-millrolls = c-millrolls +  tt-millRoll.lote + " - ".

        FOR EACH saldo-estoq WHERE 
            saldo-estoq.cod-estabel   =  tt-millRoll.cod-estabel  AND
            saldo-estoq.lote          =  tt-millRoll.lote         AND
            saldo-estoq.it-codigo     =  tt-millRoll.it-codigo    NO-LOCK USE-INDEX item-lote.

            d-total-saldo = d-total-saldo + saldo-estoq.qtidade-atu.

        END.


          

        RUN pi-filhos(INPUT tt-millRoll.lote , INPUT tt-millRoll.it-codigo).

                                     
     

    END.

 for each tt-cortadas-prim.
    d-total-cortada = d-total-cortada + tt-cortadas-prim.qt-produzida.
 end.
       CONTADOR = 0.

    d-quantidade = 0.
    d-perc = 0.
    d-maior = 0.
    r-maior = ?.

   FOR EACH tt-bobinas-def  WHERE tt-bobinas-def.defeito    =  "defpri" NO-LOCK.
     d-quantidade = d-quantidade + tt-bobinas-def.peso.
     IF  tt-bobinas-def.peso > d-maior THEN
         ASSIGN 
         d-maior = tt-bobinas-def.peso
         r-maior = ROWID(tt-bobinas-def).
   END.

   d-quantidade-aux = d-quantidade.

   FOR EACH tt-bobinas-def  WHERE tt-bobinas-def.defeito    =  "defpri" NO-LOCK.
      tt-bobinas-def.perc =  truncate(tt-bobinas-def.peso / d-quantidade * 100,2).
      d-perc = d-perc + tt-bobinas-def.perc.
   END.

   FIND tt-bobinas-def WHERE ROWID( tt-bobinas-def) = r-maior NO-LOCK NO-ERROR.
   IF avail  tt-bobinas-def THEN
       ASSIGN tt-bobinas-def.perc = tt-bobinas-def.perc + 100 - d-perc.


   d-quantidade = 0.
   d-perc = 0.
   d-maior = 0.
   r-maior = ?.

  FOR EACH tt-bobinas-def  WHERE tt-bobinas-def.defeito    =  "defsec" NO-LOCK.
    d-quantidade = d-quantidade + tt-bobinas-def.peso.
    IF  tt-bobinas-def.peso > d-maior THEN
        ASSIGN 
        d-maior = tt-bobinas-def.peso
        r-maior = ROWID(tt-bobinas-def).
  END.

  FOR EACH tt-bobinas-def  WHERE tt-bobinas-def.defeito    =  "defsec" NO-LOCK.
     tt-bobinas-def.perc =  truncate(tt-bobinas-def.peso / d-quantidade * 100,2).
     d-perc = d-perc + tt-bobinas-def.perc.
  END.

  FIND tt-bobinas-def WHERE ROWID( tt-bobinas-def) = r-maior NO-ERROR.
  IF avail  tt-bobinas-def THEN
      ASSIGN tt-bobinas-def.perc = tt-bobinas-def.perc + 100 - d-perc.



    c-millrolls =  SUBSTRING(TRIM(c-millrolls),1,LENGTH(trim(c-millrolls)) - 1).

    run pi-lista.
    
    /*k01*/
 

         ASSIGN 
            c-relatorio:range("E6"):VALUE   = c-it-codigo-rel  
            c-relatorio:range("I6"):VALUE   = dt-data-producao    
            c-relatorio:range("B81"):VALUE  = c-millrolls 
            c-relatorio:range("B82"):VALUE  = d-total-prod
            c-relatorio:range("B114"):VALUE = d-total-saldo
            c-relatorio:range("B116"):VALUE = d-total-cortada
            c-relatorio:range("B117"):VALUE = d-quantidade-aux
            c-relatorio:range("j115"):VALUE = TODAY.
       
            i-linha = 143.


        

         
      c-relatorio:range("A" + STRING(i-linha)):VALUE = "RESUMO POR TIPO DE DEFEITO - N«O REJEIÄ«O".
      c-relatorio:Range("A" + STRING(i-linha)):Font:FontStyle = "Negrito".
      c-relatorio:Range("A" + STRING(i-linha) + ":F" + STRING(i-linha) ):Merge.

       
     ASSIGN
       i-linha = i-linha + 1.

       c-relatorio:Range("E" + STRING(i-linha) + ":G" + STRING(i-linha) ):Merge.
       c-relatorio:Range("A" + STRING(i-linha) + ":E" + STRING(i-linha) ):Font:FontStyle = "Negrito".


     ASSIGN
             c-relatorio:range("A" + STRING(i-linha)):VALUE = "Item"
                   c-relatorio:range("B" + STRING(i-linha)):VALUE = "Quantidade"
                   c-relatorio:range("C" + STRING(i-linha)):VALUE = "Peso"
                   c-relatorio:range("D" + STRING(i-linha)):VALUE = "Percentual"
                   c-relatorio:range("E" + STRING(i-linha)):VALUE = "Descriá∆o".


        CONTADOR = 0.

     FOR EACH tt-bobinas-def  WHERE tt-bobinas-def.defeito    =  "defsec" NO-LOCK.

         FIND FIRST c-tab-res
                        where c-tab-res.nr-tabela =  tt-bobinas-def.nr-tabela  AND
                              c-tab-res.sequencia =  tt-bobinas-def.sequencia 
                              NO-LOCK NO-ERROR.
                
               
         CONTADOR = CONTADOR + 1.
        run pi-acompanhar in h-acomp(input "Movimentos Defeito Sec: " +  STRING(CONTADOR)).


           
        
         ASSIGN i-linha = i-linha + 1.
         c-relatorio:Range("E" + STRING(i-linha) + ":G" + STRING(i-linha) ):Merge.
         
        
         ASSIGN
            c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-bobinas-def.it-codigo
                  c-relatorio:range("B" + STRING(i-linha)):VALUE = tt-bobinas-def.quantidade
                  c-relatorio:range("C" + STRING(i-linha)):VALUE = tt-bobinas-def.peso
                  c-relatorio:range("D" + STRING(i-linha)):VALUE =  tt-bobinas-def.perc
                  c-relatorio:range("E" + STRING(i-linha)):VALUE =  IF AVAIL c-tab-res THEN  c-tab-res.descricao ELSE "".
                        
           

    END.

    ASSIGN i-linha = i-linha + 5.


      c-relatorio:range("A" + STRING(i-linha)):VALUE = "RESUMO POR TIPO DE DEFEITO - REJEIÄ«O".
      c-relatorio:Range("A" + STRING(i-linha)):Font:FontStyle = "Negrito".
      c-relatorio:Range("A" + STRING(i-linha) + ":F" + STRING(i-linha) ):Merge.




    ASSIGN i-linha = i-linha + 1.

      c-relatorio:Range("E" + STRING(i-linha) + ":G" + STRING(i-linha) ):Merge.
      c-relatorio:Range("A" + STRING(i-linha) + ":E" + STRING(i-linha) ):Font:FontStyle = "Negrito".
    
   ASSIGN
      c-relatorio:range("A" + STRING(i-linha)):VALUE = "Item"
            c-relatorio:range("B" + STRING(i-linha)):VALUE = "Quantidade"
            c-relatorio:range("C" + STRING(i-linha)):VALUE = "Peso"
            c-relatorio:range("D" + STRING(i-linha)):VALUE = "Percentual"
            c-relatorio:range("E" + STRING(i-linha)):VALUE = "Descriá∆o".

        CONTADOR = 0.

     FOR EACH tt-bobinas-def  WHERE tt-bobinas-def.defeito    =  "defpri" NO-LOCK.

         FIND FIRST c-tab-res
                        where c-tab-res.nr-tabela =  tt-bobinas-def.nr-tabela  AND
                              c-tab-res.sequencia =  tt-bobinas-def.sequencia 
                              NO-LOCK NO-ERROR.
                
         CONTADOR = CONTADOR + 1.
        run pi-acompanhar in h-acomp(input "Movimentos defeitos primarios: " +  STRING(CONTADOR)).

        
       ASSIGN i-linha = i-linha + 1.
        
       c-relatorio:Range("E" + STRING(i-linha) + ":G" + STRING(i-linha) ):Merge.


        
        ASSIGN

            c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-bobinas-def.it-codigo
                  c-relatorio:range("B" + STRING(i-linha)):VALUE = tt-bobinas-def.quantidade
                  c-relatorio:range("C" + STRING(i-linha)):VALUE = tt-bobinas-def.peso
                  c-relatorio:range("D" + STRING(i-linha)):VALUE =  tt-bobinas-def.perc
                  c-relatorio:range("E" + STRING(i-linha)):VALUE =   IF AVAIL c-tab-res THEN  c-tab-res.descricao ELSE "".

   
    END.

    ASSIGN i-linha = i-linha + 5.
      c-relatorio:range("A" + STRING(i-linha)):VALUE = "PEDIDOS DESTA RODADA".
      c-relatorio:Range("A" + STRING(i-linha)):Font:FontStyle = "Negrito".
      c-relatorio:Range("A" + STRING(i-linha) + ":F" + STRING(i-linha) ):Merge.


    ASSIGN i-linha = i-linha +  1.

    
      /*  c-relatorio:Range("F" + STRING(i-linha) + ":H" + STRING(i-linha) ):Merge.
        c-relatorio:Range("I" + STRING(i-linha) + ":J" + STRING(i-linha) ):Merge.*/
        c-relatorio:Range("g" + STRING(i-linha) + ":i" + STRING(i-linha) ):Merge.
               c-relatorio:Range("A" + STRING(i-linha) + ":O" + STRING(i-linha) ):Font:FontStyle = "Negrito".
        


        ASSIGN   
            c-relatorio:range("A" + STRING(i-linha)):VALUE = "Estab"
                 c-relatorio:range("B" + STRING(i-linha)):VALUE = "Item"
                 c-relatorio:range("C" + STRING(i-linha)):VALUE = "lote"
                 c-relatorio:range("D" + STRING(i-linha)):VALUE = "Peso"
                 c-relatorio:range("E" + STRING(i-linha)):VALUE = "Ordem Prod."
                 c-relatorio:range("F" + STRING(i-linha)):VALUE =  "Dt. Prod."
                 c-relatorio:range("G" + STRING(i-linha)):VALUE = "Lote Origem."    
                 c-relatorio:range("j" + STRING(i-linha)):VALUE = "Pallete"         
                 c-relatorio:range("k" + STRING(i-linha)):VALUE = "Pedido" 
                 c-relatorio:range("l" + STRING(i-linha)):VALUE = "Emiss∆o"
                 c-relatorio:range("m" + STRING(i-linha)):VALUE = "NF"         
                 c-relatorio:range("n" + STRING(i-linha)):VALUE = "Cliente". 



CONTADOR = 0.

    FOR EACH tt-cortadas .
        ASSIGN 
            
            tt-cortadas.lote-origem  = ""
            tt-cortadas.nr-pallet    = ""
            tt-cortadas.nr-pedido    = 0
            tt-cortadas.dt-embarque  = ?
            tt-cortadas.nr-nota-fis  = ""
            tt-cortadas.nome-abrev   = "".

        FOR EACH bo-lote-rastreab WHERE 
            bo-lote-rastreab.lote           =  tt-cortadas.lote   AND
            bo-lote-rastreab.it-codigo      =  tt-cortadas.it-codigo  NO-LOCK USE-INDEX lote .

            tt-cortadas.lote-origem =  tt-cortadas.lote-origem +  (IF tt-cortadas.lote-origem <> "" THEN "/" ELSE "") +  bo-lote-rastreab.lote-cons .
        END.

        FIND FIRST it-pallet WHERE 
            it-pallet.it-codigo   = tt-cortadas.it-codigo AND
            it-pallet.lote-bobina = tt-cortadas.lote NO-LOCK  USE-INDEX lote-bobina NO-ERROR.

        IF AVAIL it-pallet THEN DO:
            ASSIGN tt-cortadas.nr-pallet = it-pallet.nr-pallet.

            FIND FIRST pallet OF it-pallet NO-LOCK NO-ERROR.
            IF AVAIL pallet THEN DO:
                l-log = NO.
                REPEAT:
                
                    FOR EACH  fat-ser-lote WHERE 
                        fat-ser-lote.log-disp-planej = l-log AND
                        fat-ser-lote.nr-serlote = pallet.nr-pallet AND 
                        fat-ser-lote.it-codigo  = pallet.it-codigo NO-LOCK.
                    
                        FOR EACH it-nota-fisc OF fat-ser-lote no-lock,
                            EACH nota-fiscal OF it-nota-fisc NO-LOCK,
                            FIRST ped-item WHERE 
                                ped-item.it-codigo = it-nota-fisc.it-codigo AND
                                ped-item.nr-pedcli = it-nota-fisc.nr-pedcli and
                                ped-item.nome-abrev = nota-fiscal.nome-ab-cli USE-INDEX ch-cli-ped NO-LOCK,
                            FIRST ped-venda OF ped-item NO-LOCK,
                            EACH natur-oper WHERE 
                                natur-oper.nat-operacao = nota-fiscal.nat-operacao AND
                                NOT natur-oper.terceiros NO-LOCK BREAK BY nota-fiscal.dt-emis-nota.
                            
                            ASSIGN
                                 tt-cortadas.dt-embarque = nota-fiscal.dt-emis-nota
                                 tt-cortadas.nr-nota-fis = nota-fiscal.nr-nota-fis
                                 tt-cortadas.nome-abrev  = nota-fiscal.nome-ab-cli
                                 tt-cortadas.nr-pedido   =  ped-venda.nr-pedido.
                                
                    
                        END.
                    END.
                    IF l-log  THEN LEAVE.

                    l-log = YES.

                END.
            END.
        END.

        CONTADOR = CONTADOR + 1.
        run pi-acompanhar in h-acomp(input "Movimentos Pedidos atendidos: " +  STRING(CONTADOR)).

        


        ASSIGN i-linha = i-linha + 1.


        c-relatorio:Range("g" + STRING(i-linha) + ":i" + STRING(i-linha) ):Merge.
       /* c-relatorio:Range("j" + STRING(i-linha) + ":k" + STRING(i-linha) ):Merge.*/
        c-relatorio:Range("n" + STRING(i-linha) + ":o" + STRING(i-linha) ):Merge.
         

        ASSIGN   
            c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-cortadas.cod-estabel
                 c-relatorio:range("B" + STRING(i-linha)):VALUE = tt-cortadas.it-codigo
                 c-relatorio:range("C" + STRING(i-linha)):VALUE = tt-cortadas.lote
                 c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-cortadas.qt-produzida 
                 c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-cortadas.nr-ord-produ
                 c-relatorio:range("F" + STRING(i-linha)):VALUE =  tt-cortadas.dt-produzida
                 c-relatorio:range("G" + STRING(i-linha)):VALUE = tt-cortadas.lote-origem    
                 c-relatorio:range("j" + STRING(i-linha)):VALUE = tt-cortadas.nr-pallet         
                 c-relatorio:range("k" + STRING(i-linha)):VALUE = tt-cortadas.nr-pedido 
                 c-relatorio:range("l" + STRING(i-linha)):VALUE = tt-cortadas.dt-embarque
                 c-relatorio:range("m" + STRING(i-linha)):VALUE = tt-cortadas.nr-nota-fis         
                 c-relatorio:range("n" + STRING(i-linha)):VALUE = tt-cortadas.nome-abrev. 
  
        
        
   END.
   
  
CONTADOR = 0.

 FOR EACH tt-millRoll NO-LOCK.

               
        
         FOR EACH ficha-cq NO-LOCK WHERE
            ficha-cq.it-codigo = tt-millRoll.it-codigo AND
            ficha-cq.lote      = tt-millRoll.lote
           /* USE-INDEX it-lote*/ :
        
           FOR EACH pol-res-fic-cq-leitura NO-LOCK WHERE
               pol-res-fic-cq-leitura.nr-ficha   = ficha-cq.nr-ficha:
        
               IF DEC(pol-res-fic-cq-leitura.resultado)  = 0 THEN NEXT.
        
               FIND FIRST tt-lote WHERE
                   tt-lote.nr-linha  = tt-millRoll.linha                AND
                   tt-lote.it-codigo = tt-millRoll.it-codigo            AND
                   tt-lote.nr-lote   = tt-millRoll.lote                 AND
                   tt-lote.cod-exame = pol-res-fic-cq-leitura.cod-exame AND
                   tt-lote.cod-comp  = pol-res-fic-cq-leitura.cod-comp
                   NO-ERROR.
        
               IF NOT AVAIL tt-lote THEN DO:
        
                  CREATE tt-lote.
                  ASSIGN tt-lote.cod-comp  = pol-res-fic-cq-leitura.cod-comp
                         tt-lote.nr-linha  = tt-millRoll.linha    
                         tt-lote.nr-ord-produ = tt-millRoll.nr-ord-produ
                         tt-lote.cod-exame = pol-res-fic-cq-leitura.cod-exame
                         tt-lote.it-codigo = tt-millRoll.it-codigo  
                         tt-lote.nr-lote   = tt-millRoll.lote.
        
                  ASSIGN tt-lote.menor-result = 9999999999
                         tt-lote.maior-result = 0.

                  FIND FIRST comp-exame WHERE
                        comp-exame.cod-exame = tt-lote.cod-exame    AND
                        comp-exame.cod-comp  = tt-lote.cod-comp
                        NO-LOCK NO-ERROR.
                
                    IF AVAIL comp-exame THEN
                        ASSIGN tt-lote.descricao = comp-exame.descricao.
                    ELSE
                        ASSIGN tt-lote.descricao = "".
        
               END.
               tt-lote.soma-result = tt-lote.soma-result + DEC (pol-res-fic-cq-leitura.resultado).
        
               IF DEC (pol-res-fic-cq-leitura.resultado) < tt-lote.menor-result THEN
                  ASSIGN tt-lote.menor-result = DEC (pol-res-fic-cq-leitura.resultado).
        
               IF DEC (pol-res-fic-cq-leitura.resultado) > tt-lote.maior-result THEN
                  ASSIGN tt-lote.maior-result = DEC (pol-res-fic-cq-leitura.resultado).
        
               ASSIGN tt-lote.ultima-seq = tt-lote.ultima-seq + 1.
        
               IF tt-lote.ultima-seq < 51 THEN
                   ASSIGN tt-lote.resultado [tt-lote.ultima-seq] = DEC (pol-res-fic-cq-leitura.resultado).
        
        
           END.
        
        END.

END.


    ASSIGN i-linha = i-linha + 5
        c-relatorio:range("A" + STRING(i-linha)):VALUE = "PROPRIEDADES LABORAT‡RIO".
         c-relatorio:Range("A" + STRING(i-linha)):Font:FontStyle = "Negrito".
         c-relatorio:Range("A" + STRING(i-linha) + ":F" + STRING(i-linha) ):Merge.



 ASSIGN i-linha = i-linha + 1.


         c-relatorio:Range("B" + STRING(i-linha) + ":D" + STRING(i-linha) ):Merge.
         c-relatorio:Range("E" + STRING(i-linha) + ":G" + STRING(i-linha) ):Merge.
         c-relatorio:Range("H" + STRING(i-linha) + ":I" + STRING(i-linha) ):Merge.

         c-relatorio:Range("A" + STRING(i-linha) + ":O" + STRING(i-linha) ):Font:FontStyle = "Negrito".


         ASSIGN

           c-relatorio:range("A" + STRING(i-linha)):VALUE = "Linha"
                 c-relatorio:range("B" + STRING(i-linha)):VALUE = "Item"
                 c-relatorio:range("E" + STRING(i-linha)):VALUE = "Descriá∆o"
                 c-relatorio:range("H" + STRING(i-linha)):VALUE = "Menor Result."
                 c-relatorio:range("J" + STRING(i-linha)):VALUE = "Maior Result."
                 c-relatorio:range("K" + STRING(i-linha)):VALUE = "MÇdia Result."
                 c-relatorio:range("L" + STRING(i-linha)):VALUE = "seq-jr"   
                 c-relatorio:range("M" + STRING(i-linha)):VALUE = "Lote"        
                 c-relatorio:range("N" + STRING(i-linha)):VALUE = "Ord.Prod."
                 c-relatorio:range("O" + STRING(i-linha)):VALUE = "Exames".


for each tt-lote no-lock.

       IF tt-lote.nr-linha  <> nr-linha-ant  OR
          tt-lote.it-codigo <> it-codigo-ant OR
          tt-lote.cod-exame <> cod-exame-ant OR
          tt-lote.cod-comp  <> cod-comp-ant THEN 

          ASSIGN seq-jr = 0
                 nr-linha-ant  = tt-lote.nr-linha
                 it-codigo-ant = tt-lote.it-codigo
                 cod-exame-ant = tt-lote.cod-exame
                 cod-comp-ant  = tt-lote.cod-comp.


       ASSIGN seq-jr      = seq-jr + 1
              soma-result = 1
              soma-col    = 0
              c-exames = "".

        DO WHILE soma-result < 51.

           IF tt-lote.resultado [soma-result] <> 0 THEN

               ASSIGN soma-col = soma-col + 1
                      /*c-relatorio:Range(trim(entry(soma-col,col-jr) + string(i-linha))):VALUE = tt-lote.resultado [soma-result].*/
                       c-exames = c-exames + trim(STRING(tt-lote.resultado [soma-result],"->>,>>>,>>>,>>9.99")) + "/".
           ASSIGN soma-result = soma-result + 1.

       END.


       CONTADOR = CONTADOR + 1.
        run pi-acompanhar in h-acomp(input "Movimentos analises Lab: " +  STRING(CONTADOR)).


       
         ASSIGN i-linha = i-linha + 1.


         c-relatorio:Range("B" + STRING(i-linha) + ":D" + STRING(i-linha) ):Merge.
         c-relatorio:Range("E" + STRING(i-linha) + ":G" + STRING(i-linha) ):Merge.
         c-relatorio:Range("H" + STRING(i-linha) + ":I" + STRING(i-linha) ):Merge.
         c-relatorio:Range("O" + STRING(i-linha) + ":aO" + STRING(i-linha) ):Merge.

         ASSIGN

           c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-lote.nr-linha
                 c-relatorio:range("B" + STRING(i-linha)):VALUE = tt-lote.it-codigo
                 c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-lote.descricao
                 c-relatorio:range("H" + STRING(i-linha)):VALUE = tt-lote.menor-result
                 c-relatorio:range("J" + STRING(i-linha)):VALUE = tt-lote.maior-result
                 c-relatorio:range("K" + STRING(i-linha)):VALUE = tt-lote.soma-result / tt-lote.ultima-seq                 
                 c-relatorio:range("L" + STRING(i-linha)):VALUE = seq-jr   
                 c-relatorio:range("M" + STRING(i-linha)):VALUE = tt-lote.nr-lote      
                 c-relatorio:range("N" + STRING(i-linha)):VALUE = tt-lote.nr-ord-produ
                 c-relatorio:range("O" + STRING(i-linha)):VALUE = c-exames.
               

 END.



    /*OUTPUT CLOSE.*/

   RUN pi-finaliza-impressao.

    /*k01*/

   RUN pi-finalizar IN h-acomp.
             
END PROCEDURE.

PROCEDURE pi-cria-planilha:

DEF VAR c-arquivo AS CHAR NO-UNDO.

    c-arquivo = c-arq + 'escp0064' + STRING(time)+ '.xls'.

    OS-COPY VALUE(c-modelo-planilha) VALUE(c-arquivo).

    assign c-planilha  = c-excel:Workbooks:OPEN(c-arquivo)
           c-relatorio = c-excel:Sheets:item(1)
           c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo.

END PROCEDURE.


PROCEDURE pi-finaliza-impressao:
DEF VAR i         AS INT  NO-UNDO.
DEF VAR c-arquivo AS CHAR NO-UNDO.

    c-planilha:SAVE().
    c-planilha:CLOSE().
     
    c-excel:VISIBLE = true.

    DO i = 1 TO NUM-ENTRIES(c-arq-anexo):
        c-arquivo = ENTRY(i,c-arq-anexo).
        c-planilha  = c-excel:Workbooks:OPEN(c-arquivo).
    END.

    /*c-excel:QUIT().*/

    RELEASE OBJECT c-planilha.
    RELEASE OBJECT c-relatorio.
    RELEASE OBJECT c-excel.

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
              im-pg-cla:load-image("image/im-fldup") .
            assign im-pg-cla:height = 1.20
                   im-pg-cla:row    = 1.50.
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
              im-pg-dig:load-image("image/im-fldup") .
            assign im-pg-dig:height = 1.20
                   im-pg-dig:row    = 1.5.
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

    PROCEDURE pi-filhos.

        DEFINE INPUT PARAM  lote-pesquisa  AS CHARACTER   NO-UNDO.
        DEFINE INPUT PARAM  item-pesquisa  AS CHARACTER   NO-UNDO.





    FOR EACH lote-rastreab WHERE lote-rastreab.lote-cons      =  lote-pesquisa   AND
                                 lote-rastreab.it-codigo-cons =  item-pesquisa  NO-LOCK USE-INDEX lote-consumido.
  

    FOR EACH movto-mat 
                 where movto-mat.esp-docto = 1 AND
                       movto-mat.it-codigo    = lote-rastreab.it-codigo    AND
                       movto-mat.lote         = lote-rastreab.lote         AND
                       movto-mat.nr-ord-produ = lote-rastreab.nr-ord-produ AND
                       movto-mat.cod-estabel  = tt-millRoll.cod-estabel /*AND 
                       movto-estoq.cod-depos <> "ARC" */

                       NO-LOCK USE-INDEX lote,
         EACH movto-estoq 
                 where movto-estoq.esp-docto    = movto-mat.esp-docto    AND
                       movto-estoq.it-codigo    = movto-mat.it-codigo    AND
                       movto-estoq.lote         = movto-mat.lote         AND
                       movto-estoq.nr-ord-produ = movto-mat.nr-ord-produ AND
                       movto-estoq.cod-estabel  = movto-mat.cod-estabel  and
                       movto-estoq.cod-depos    = movto-mat.cod-depos    AND
                       movto-estoq.nr-reporte   = movto-mat.nr-reporte   AND
                       movto-estoq.cod-localiz  = movto-mat.cod-localiz
                       /*AND 
                       movto-estoq.cod-depos <> "ARC" */

                       NO-LOCK USE-INDEX item-est-dep .
                       
                       

          IF movto-estoq.lote = "recicl" THEN next.     



          FIND FIRST bf-movto-mat WHERE
                bf-movto-mat.nr-reporte = movto-estoq.nr-reporte AND
                bf-movto-mat.esp-docto = 8
                USE-INDEX reporte
                NO-LOCK NO-ERROR.

          IF AVAIL bf-movto-mat THEN NEXT.

          
          

          FIND FIRST ITEM WHERE ITEM.it-codigo = movto-estoq.it-codigo
             USE-INDEX codigo NO-LOCK NO-ERROR.

          IF NOT AVAIL ITEM OR item.ge-codigo < 40 AND ITEM.GE-codigo > 49 THEN NEXT.

         /* IF ITEM.GE-codigo > 69  and
              item.ge-codigo <> 85 THEN NEXT.*/

          if item.ge-codigo <> 47 and movto-estoq.cod-depos = "ARC" then next.

          if item.ge-codigo = 47 THEN DO:
               FIND FIRST rep-prod WHERE     rep-prod.nr-reporte = movto-estoq.nr-reporte NO-LOCK NO-ERROR.

               IF NOT AVAIL rep-prod THEN NEXT.

               IF  rep-prod.qt-refugo > 0 THEN DO:
                   FIND FIRST bf-movto-estoq WHERE
                       bf-movto-estoq.nr-reporte = movto-estoq.nr-reporte AND
                       bf-movto-estoq.nr-trans   =  movto-estoq.nr-trans + 1 no-lock USE-INDEX nr-reporte NO-ERROR.


                   IF NOT AVAIL bf-movto-estoq AND rep-prod.qt-refugo = movto-estoq.quantidade THEN NEXT.

               END.
          END.

           

          FIND FIRST rep-prod WHERE
                rep-prod.nr-reporte = movto-estoq.nr-reporte
                use-index codigo NO-LOCK NO-ERROR.

          IF AVAIL rep-prod AND rep-prod.qt-estorno <> 0 THEN NEXT.

          FIND FIRST ord-prod WHERE ord-prod.nr-ord-produ = movto-estoq.nr-ord-produ
                 NO-LOCK NO-ERROR.

          IF NOT AVAIL ord-prod THEN NEXT.
/*
          IF  (ord-prod.nr-linha < 200 OR ord-prod.nr-linha > 499)
                 THEN NEXT.

          IF  (ord-prod.nr-linha > 299 AND ord-prod.nr-linha < 400)
                 THEN NEXT.

*/


                       FIND BF-ITEM WHERE bf-item.it-codigo = lote-rastreab.it-codigo-cons no-lock no-error.
                       
                       if avail bf-item and (bf-item.ge-codigo = 41 OR bf-item.ge-codigo = 42) and index(bf-item.it-codigo,"MR") > 0 then do:

                       
                           i-nr-trans =  movto-estoq.nr-trans.
                           l-req = no.
                           
                           repeat:
                               i-nr-trans =  i-nr-trans - 1.
                               if i-nr-trans <  movto-estoq.nr-trans - 15 then leave.
                           
                               find first  b-movto-estoq   where b-movto-estoq.nr-trans = i-nr-trans no-lock use-index nr-trans no-error .
                               if not avail b-movto-estoq then next.
                               if b-movto-estoq.esp-docto = 1 then leave.
                                if b-movto-estoq.nr-ord-produ <> movto-estoq.nr-ord-produ then next.

                               if b-movto-estoq.esp-docto <> 28 then next.
                               if b-movto-estoq.lote = lote-rastreab.lote-cons then do: 
                                   l-req = yes.
                                   leave.
                               end.                         
                           
                           end.
                           
                           if l-req = no then next.

                            FIND FIRST tt-cortadas-prim WHERE  tt-cortadas-prim.cod-estabel  =  movto-estoq.cod-estabel AND
                                                                           tt-cortadas-prim.it-codigo    =  movto-estoq.it-codigo   AND
                                                                           tt-cortadas-prim.lote         =  movto-estoq.lote        AND
                                                                           tt-cortadas-prim.nr-ord-produ =  movto-estoq.nr-ord-produ NO-ERROR.
                            
                                            IF NOT AVAIL tt-cortadas-prim THEN DO:
                            
                                                CREATE  tt-cortadas-prim.
                                                ASSIGN 
                                                      tt-cortadas-prim.cod-estabel  =  movto-estoq.cod-estabel
                                                      tt-cortadas-prim.it-codigo    =  movto-estoq.it-codigo
                                                      tt-cortadas-prim.lote         =  movto-estoq.lote
                                                      tt-cortadas-prim.nr-ord-produ =  movto-estoq.nr-ord-produ
                                                      tt-cortadas-prim.qt-produzida =  movto-estoq.quantidade
                                                      tt-cortadas-prim.dt-produzida =  movto-estoq.dt-trans.  
                                                      
                                             end.           
                            

                                  
                                   
                       end.            


          FIND First bf-lote-rastreab WHERE bf-lote-rastreab.lote-cons      =  movto-estoq.lote      AND
                                            bf-lote-rastreab.it-codigo-cons =  movto-estoq.it-codigo NO-LOCK
                                            USE-INDEX lote-consumido  NO-ERROR.

          

          IF AVAIL bf-lote-rastreab THEN do:

              RUN pi-filhos(INPUT movto-estoq.lote , INPUT movto-estoq.it-codigo).

              NEXT.
          END.

          FIND FIRST tt-cortadas WHERE  tt-cortadas.cod-estabel  =  movto-estoq.cod-estabel AND
                                               tt-cortadas.it-codigo    =  movto-estoq.it-codigo   AND
                                               tt-cortadas.lote         =  movto-estoq.lote        AND
                                               tt-cortadas.nr-ord-produ =  movto-estoq.nr-ord-produ NO-ERROR.

                IF NOT AVAIL tt-cortadas THEN DO:

                    CREATE  tt-cortadas.
                    ASSIGN 
                          tt-cortadas.cod-estabel  =  movto-estoq.cod-estabel
                          tt-cortadas.it-codigo    =  movto-estoq.it-codigo
                          tt-cortadas.lote         =  movto-estoq.lote
                          tt-cortadas.nr-ord-produ =  movto-estoq.nr-ord-produ
                          tt-cortadas.qt-produzida =  movto-estoq.quantidade
                          tt-cortadas.dt-produzida =  movto-estoq.dt-trans.    

                          run pi-acompanhar in h-acomp(input "caracterist: " +  STRING(CONTADOR) + " - " + tt-millRoll.lote).



                          FIND  FIRST lote-carac-tec WHERE
                                                   lote-carac-tec.it-codigo = movto-estoq.it-codigo
                                                   and lote-carac-tec.lote = movto-estoq.lote
                                                   and lote-carac-tec.cd-comp = "defpri"
                                                   use-index codigo NO-LOCK NO-ERROR.

                                if avail lote-carac-tec then do:

                                    FOR EACH lote-res-carac
                                          where lote-res-carac.cd-folha = lote-carac-tec.cd-folha and
                                          lote-res-carac.it-codigo = lote-carac-tec.it-codigo and
                                          lote-res-carac.lote = lote-carac-tec.lote and
                                          lote-res-carac.nr-tabela = lote-carac-tec.nr-tabela and
                                          lote-res-carac.sequencia <> 0 and
                                          lote-res-carac.cd-comp = lote-carac-tec.cd-comp
                                          NO-LOCK .

                                          /* ASSIGN i-jr = i-jr + 1.

                                          assign defpri-jr [i-jr] = lote-res-carac.sequencia
                                                tabpri-jr [i-jr] = lote-res-carac.nr-tabela.*/



                                          FIND FIRST tt-bobinas-def WHERE tt-bobinas-def.it-codigo =  lote-carac-tec.it-codigo AND
                                                                        tt-bobinas-def.sequencia =  lote-res-carac.sequencia AND
                                                                        tt-bobinas-def.nr-tabela =  lote-res-carac.nr-tabela AND
                                                                        tt-bobinas-def.defeito    =  "defpri" NO-ERROR.

                                        IF NOT AVAIL tt-bobinas-def THEN DO:
                                            CREATE  tt-bobinas-def.
                                            ASSIGN 
                                                tt-bobinas-def.it-codigo =  lote-carac-tec.it-codigo
                                                tt-bobinas-def.sequencia =  lote-res-carac.sequencia
                                                tt-bobinas-def.nr-tabela =  lote-res-carac.nr-tabela
                                                tt-bobinas-def.defeito    =  "defpri".

                                         END.

                                         ASSIGN 
                                                tt-bobinas-def.quantidade =  tt-bobinas-def.quantidade + 1
                                                tt-bobinas-def.peso =  tt-bobinas-def.peso + movto-estoq.quantidade.



                                    END.

                                END.

                               /*IF defpri-jr <> 103 THEN next.*/

                               FIND  FIRST lote-carac-tec WHERE
                                                      lote-carac-tec.it-codigo = movto-estoq.it-codigo
                                                      and lote-carac-tec.lote = movto-estoq.lote
                                                      and lote-carac-tec.cd-comp = "defsec"
                                                      use-index codigo NO-LOCK NO-ERROR.

                                     if  avail lote-carac-tec then do:

                                         FOR EACH lote-res-carac
                                             where lote-res-carac.cd-folha = lote-carac-tec.cd-folha and
                                             lote-res-carac.it-codigo = lote-carac-tec.it-codigo and
                                             lote-res-carac.lote = lote-carac-tec.lote and
                                             lote-res-carac.nr-tabela = lote-carac-tec.nr-tabela and
                                             lote-res-carac.sequencia <> 0 and
                                             lote-res-carac.cd-comp = lote-carac-tec.cd-comp
                                             NO-LOCK.

                                             FIND FIRST tt-bobinas-def WHERE tt-bobinas-def.it-codigo =  lote-carac-tec.it-codigo AND
                                                                        tt-bobinas-def.sequencia =  lote-res-carac.sequencia AND
                                                                        tt-bobinas-def.nr-tabela =  lote-res-carac.nr-tabela AND
                                                                        tt-bobinas-def.defeito    =  "defsec" NO-ERROR.

                                            IF NOT AVAIL tt-bobinas-def THEN DO:
                                                CREATE  tt-bobinas-def.
                                                ASSIGN 
                                                    tt-bobinas-def.it-codigo =  lote-carac-tec.it-codigo
                                                    tt-bobinas-def.sequencia =  lote-res-carac.sequencia
                                                    tt-bobinas-def.nr-tabela =  lote-res-carac.nr-tabela
                                                    tt-bobinas-def.defeito    =  "defsec".

                                             END.

                                             ASSIGN 
                                                    tt-bobinas-def.quantidade =  tt-bobinas-def.quantidade + 1
                                                    tt-bobinas-def.peso       =  tt-bobinas-def.peso + movto-estoq.quantidade.

                                             END.

                                     END.

                END.

        END.

 END.
 END PROCEDURE.

 
procedure pi-lista.

output to v:\temp\tt-cortadas-prim.

for each tt-cortadas-prim no-lock.

disp tt-cortadas-prim with stream-io width 300 .

end.
output close.
end procedure.
