
&Scoped-define WINDOW-NAME C-Win

/*------------------------------------------------------------------------
File.............: escc0027.w
Description......: Gera‡Æo de Ordens de Compras/Cota‡äes da Lista de Necessidades
Input Parameters : 
Output Parameters: 
Author...........: Amgra - Edson
Created..........: 28/02/2013   
OBS..............: 
------------------------------------------------------------------------*/

define variable c-prog-gerado as character no-undo initial "escc0027".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

CREATE WIDGET-POOL. 

/* ***************************  Definitions  ************************** */
define buffer empresa for mgmulti.empresa.
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
    field ep-codigo            as char
    field cod-estabel          AS CHAR  
    field c-arquivo-jr         AS CHAR
    FIELD pesq-jr              AS INT 
    FIELD cod-emitente-jr      AS INT .


DEFINE TEMP-TABLE tt-digita no-undo
    FIELD cod-estabel              AS CHAR     FORMAT "x(4)"          LABEL "Est"
    FIELD cod-emitente             AS INT      FORMAT ">>>>>>>9"      LABEL "Fornec."
    FIELD nome-abrev               AS CHAR     FORMAT "x(12)"         LABEL "Nome        "
    FIELD it-codigo                AS CHAR     FORMAT "x(16)"         LABEL "Item"
    FIELD unid                     AS CHAR     FORMAT "x(2)"          LABEL "Un"
    FIELD quantidade               AS DEC      FORMAT "->>>>>>>>>>9.9999" LABEL "Quantidade"
    FIELD preco-item               AS DEC      FORMAT ">>>>>>>>>9.99"     LABEL "Preco Unit"
    FIELD cod-cond-pag             AS INT      FORMAT ">>9"               LABEL "C.Pag"
    FIELD dt-entrega               AS DATE     FORMAT "99/99/9999"        LABEL "Dt.Entrega"
    FIELD nr-tab-preco             AS INT      FORMAT ">>>>>>>>9"         LABEL "Nr.Tab.Pr"
    FIELD perc-icms                AS DEC      FORMAT ">>>>9.99"          LABEL "%Icms"
    FIELD perc-ipi                 AS DEC      FORMAT ">>>>9.99"          LABEL "%Ipi"
    FIELD perc-rat-compra          AS DEC      FORMAT ">>>>9.99"          LABEL "%Rat.Comp"
    FIELD usuar-comprador          AS CHAR     FORMAT "x(12)"             LABEL "Comprador"
    FIELD gerado-ordem             AS LOG      FORMAT "Sim/NÆo"           LABEL "Gerado Ord"
    FIELD descricao                AS CHAR     FORMAT "x(40)"             LABEL "Descri‡Æo"
    FIELD narrativa                AS CHAR     FORMAT "x(100)"    initial ""        LABEL "Narrativa"
    FIELD ct-codigo                AS CHAR     FORMAT "x(8)"              LABEL "Cta.Cont."
    FIELD sc-codigo                AS CHAR     FORMAT "x(8)"              LABEL "C.Custo."
    FIELD log-erro                 AS LOG
    FIELD linha                    AS INT
    FIELD sequencia                AS INT
    FIELD numero-ordem             AS INT
    FIELD usuar-aprov-chefe        AS CHAR     FORMAT "x(12)"
    FIELD origem                   AS INT 
    FIELD cod-transp               AS INT
    field nr-contrato              like contrato-for.nr-contrato
    field num-pedido               like ordem-compra.num-pedido 
 
    INDEX codigo IS PRIMARY UNIQUE cod-estabel
                                   it-codigo
                                   linha
                                   sequencia
                                   cod-emitente.



DEFINE TEMP-TABLE tt-fornec no-undo
    FIELD cod-emitente             AS INT      FORMAT ">>>>>>>9"    
    FIELD it-codigo                AS CHAR     FORMAT "x(16)"       
    FIELD perc-rat-compra          AS DEC      FORMAT ">>>>9.99"      
    FIELD sequencia                AS INT
    FIELD cod-estabel              AS CHAR
    FIELD nr-tab-preco             AS INT 
    FIELD nome-abrev               AS CHAR
    FIELD quantidade               AS DEC
    FIELD usuar-aprov-chefe        AS CHAR     FORMAT "x(12)"
    FIELD cod-transp               AS INT
    INDEX codigo IS PRIMARY UNIQUE cod-emitente
                                   it-codigo
                                   sequencia.


DEFINE TEMP-TABLE tt-lista no-undo
    FIELD cod-estabel              AS CHAR     FORMAT "x(3)"    
    FIELD it-codigo                AS CHAR     FORMAT "x(16)"       
    FIELD unid                     AS CHAR     FORMAT "x(2)"    
    FIELD quantidade               AS DEC      FORMAT "->>>>>>>>>>9.9999" 
    FIELD dt-entrega               AS DATE     FORMAT "99/99/9999"  
    FIELD narrativa                AS CHAR     FORMAT "x(100)"  
    FIELD ct-codigo                AS CHAR     FORMAT "x(8)"
    FIELD sc-codigo                AS CHAR     FORMAT "x(8)"
    FIELD linha                    AS INT
    FIELD origem                   AS INT   /*1=Planilha | 2=Ems */
    INDEX codigo IS PRIMARY UNIQUE linha.

def temp-table tt-ordem-compra no-undo like ordem-compra
    field l-split           as   logical                    initial no
    field l-gerou           as   logical         
    field r-ordem           as   rowid
    field ind-tipo-movto    as   integer format "99"        initial 1.

def temp-table tt-prazo-compra no-undo like prazo-compra
    field ind-tipo-movto    as   integer format "99"        initial 1.


/*
varsÆo que funcionava at‚ o pacote f029
def temp-table tt-ordem-compra no-undo like ordem-compra
    field num-sequencia     as   integer format "99999999"  
    field ind-tipo-movto    as   integer format "99"        initial 1
    field l-split           as   logical                    initial no
    field l-gerou           as   logical         
    field r-ordem           as   ROWID
index item it-codigo.

def temp-table tt-prazo-compra no-undo like prazo-compra
    field ind-tipo-movto    as   integer format "99"        initial 1.
*/

def temp-table tt-erro
    field i-sequen              as   integer  format "999999999"          
    field cd-erro               as   integer  format "99999"          
    field mensagem              as   char     format "x(60)"  .        



define buffer b-tt-digita for tt-digita.
define Buffer bf-digita   For tt-digita.


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
def var v-cod-extens-arq     as char    no-undo initial "xls". 

/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

def var i-ordem        like ordem-compra.numero-ordem no-undo.
def var i-ordem-aux    like ordem-compra.numero-ordem no-undo.
def var i-ord-aux      like ordem-compra.nr-ord-orig  no-undo.
def var l-erro         as logical no-undo.
def var i-m-ordem      like ordem-compra.numero-ordem no-undo.
def var l-split       AS LOGICAL    NO-UNDO.
ASSIGN l-split = NO.

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

def new shared var c-cod-estabel     AS CHAR  format "x(03)"    initial "412"   no-undo./*solic-318*/
def new shared var i-cod-emitente-jr AS INT   format ">>>>>>>9" initial 0       no-undo.

DEFINE VARIABLE c-nome-abrev         AS CHAR  FORMAT "x(12)"    INITIAL ""      NO-UNDO.

DEFINE VARIABLE dir-jr               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE valor-dec            AS DECIMAL    NO-UNDO.
DEFINE VARIABLE qtde-dec             AS DECIMAL    NO-UNDO.
DEFINE VARIABLE it-codigo-jr         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE erro-jr              AS INTEGER    NO-UNDO.
DEFINE VARIABLE mens-erro            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE soma-perc            AS DECIMAL    NO-UNDO.
DEFINE VARIABLE soma-jr              AS INTEGER    NO-UNDO.
DEFINE VARIABLE seq-jr               AS INTEGER    NO-UNDO.
DEFINE VARIABLE soma-qtde            AS DECIMAL    NO-UNDO.
DEFINE VARIABLE difer-qtde           AS DECIMAL    NO-UNDO.
DEFINE VARIABLE descricao-jr         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE tem-reg              AS INTEGER    NO-UNDO.
DEFINE VARIABLE seq-jr1              AS INTEGER    NO-UNDO.

DEFINE VARIABLE cod-emitente-velho   AS INTEGER    FORMAT ">>>>>>>9" NO-UNDO.
DEFINE VARIABLE cod-emitente-novo    AS INTEGER    FORMAT ">>>>>>>9" NO-UNDO.
DEFINE VARIABLE nome-abrev-novo      AS CHAR       FORMAT "x(12)"    NO-UNDO.
DEFINE VARIABLE nome-abrev-velho     AS CHAR       FORMAT "x(12)"    NO-UNDO.
DEFINE VARIABLE cnpj-velho           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cnpj-novo            AS CHARACTER  NO-UNDO.

DEFINE VARIABLE pesq-jr AS INTEGER INITIAL 1
    VIEW-AS RADIO-SET vertical
    RADIO-BUTTONS 
    "Por Planilha de Necessidades", 1,
    "Por Necessidades Geradas pelo EMS", 2
    SIZE 35 BY 1.8 NO-UNDO.




/* ********************  Preprocessor Definitions  ******************** */ 

&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita */

&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.cod-estabel /*tt-digita.cod-emitente tt-digita.nome-abrev*/ tt-digita.it-codigo tt-digita.unid tt-digita.quantidade tt-digita.preco-item tt-digita.cod-cond-pag tt-digita.dt-entrega tt-digita.usuar-comprador tt-digita.nr-tab-preco tt-digita.perc-icms tt-digita.perc-ipi tt-digita.perc-rat-compra tt-digita.gerado-ordem tt-digita.descricao tt-digita.narrativa tt-digita.ct-codigo tt-digita.sc-codigo  
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.cod-estabel /*tt-digita.cod-emitente tt-digita.nome-abrev*/ tt-digita.it-codigo tt-digita.unid tt-digita.quantidade tt-digita.preco-item tt-digita.cod-cond-pag tt-digita.dt-entrega tt-digita.usuar-comprador tt-digita.nr-tab-preco tt-digita.perc-icms tt-digita.perc-ipi tt-digita.perc-rat-compra tt-digita.gerado-ordem tt-digita.descricao tt-digita.narrativa tt-digita.ct-codigo tt-digita.sc-codigo  
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

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.13.

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


DEFINE BUTTON bt-retirar 
     LABEL "Retirar" 
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



/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-digita SCROLLING.
&ANALYZE-RESUME


/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita C-Win _FREEFORM
  QUERY br-digita DISPLAY
      tt-digita.cod-estabel        
     /* tt-digita.cod-emitente       
      tt-digita.nome-abrev     WIDTH 12    
      */
      tt-digita.it-codigo          
      tt-digita.unid               
      tt-digita.quantidade         
      tt-digita.dt-entrega         
/*      tt-digita.preco-item         
      tt-digita.cod-cond-pag       
      tt-digita.nr-tab-preco       
      tt-digita.perc-icms          
      tt-digita.perc-ipi           
      tt-digita.perc-rat-compra    
      tt-digita.usuar-comprador    */
      tt-digita.gerado-ordem       
      tt-digita.descricao          
/*      tt-digita.narrativa 
      tt-digita.ct-codigo
      tt-digita.sc-codigo*/

  ENABLE
    tt-digita.quantidade
    tt-digita.dt-entrega
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9
         BGCOLOR 15 .




/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
    bt-executar AT ROW 14.54 COL 3 HELP
"Importa Planilha de Necessidades"
     bt-cancelar AT ROW 14.54 COL 14 HELP
"Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
"Ajuda"
     im-pg-sel AT ROW 1.5 COL 2.14
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-dig AT ROW 1.5 COL 17.86 /*33.57*/

     im-pg-imp AT ROW 1.5 COL 33.57 /*49.29*/
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
     DEFAULT-BUTTON bt-executar.

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
      at row 3 col 20 colon-aligned
      view-as fill-in 
      size 10 by .88
      font 1

   /* i-cod-emitente-jr label "Fornecedor"
      at row 2 col 20 colon-aligned
      view-as fill-in 
      size 10 by .88
      font 1

    c-nome-abrev NO-LABEL 
      at row 2 col 35 colon-aligned
      view-as fill-in 
      size 20 by .88
      font 1
*/
   pesq-jr label "Op‡äes:"
     at ROW 6.00 col 12 HELP
     "Escolha Onde Buscar as Necessidades de Compras"

   RECT-5 AT ROW 5.87 COL 12.14

   text-saida AT ROW 9.33 COL 14.29 NO-LABEL

   RECT-8 AT ROW 9.27 COL 12.14

   c-arquivo-entrada  AT ROW 10 COL 14 HELP
        "Nome do arquivo de Origem do relat¢rio" NO-LABEL

   bt-arquivo-saida AT ROW 10 COL 53.86 HELP
        "Escolha do nome do arquivo"
   

   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D
   AT COL 3 ROW 2.85
   SIZE 76.86 BY 10.62.



DEFINE FRAME f-pg-dig  

    br-digita AT ROW 1 COL 1

    bt-retirar AT ROW 10 COL 01

     
    

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
   TITLE              = "Gera‡Æo de Ordens de Compras da Lista de Necessidades - escc0027"
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

ON VALUE-CHANGED OF pesq-jr IN FRAME f-pg-sel
DO:

   ASSIGN c-arquivo-entrada:SCREEN-VALUE IN FRAME f-pg-sel = dir-jr + "\escc0027" + ".xls". 

   IF pesq-jr:SCREEN-VALUE IN FRAME f-pg-sel = "1" THEN do:

     ENABLE  
         c-arquivo-entrada 
         bt-arquivo-saida
         rect-8 
     WITH FRAME f-pg-sel IN WINDOW C-Win.
       assign     c-arquivo-entrada:visible IN FRAME f-pg-sel = yes
                  bt-arquivo-saida:visible IN FRAME f-pg-sel = yes.
   end.
   ELSE do:
      /* ASSIGN c-arquivo-entrada:SCREEN-VALUE IN FRAME f-pg-sel = ""
              c-arquivo-entrada:sensitive IN FRAME f-pg-sel = no
              bt-arquivo-saida:sensitive IN FRAME f-pg-sel = no
               c-arquivo-entrada:bgcolor IN FRAME f-pg-sel = 21.
        */       
     disable  
         c-arquivo-entrada 
         bt-arquivo-saida
         
     WITH FRAME f-pg-sel /*IN WINDOW C-Win*/.
   end.
    FOR EACH tt-digita EXCLUSIVE-LOCK.

           DELETE tt-digita.

    END.

    CLOSE QUERY br-digita.
    open query br-digita for each tt-digita.



END.

    /* 
ON "leave":U OF i-cod-emitente-jr IN FRAME f-pg-sel 
DO:

    FIND FIRST emitente WHERE
        emitente.cod-emitente = int(i-cod-emitente-jr:SCREEN-VALUE IN FRAME f-pg-sel)
        NO-LOCK NO-ERROR.

    IF AVAIL emitente THEN
        ASSIGN c-nome-abrev:SCREEN-VALUE IN FRAME f-pg-sel = emitente.nome-abrev.

    ELSE

        ASSIGN c-nome-abrev:SCREEN-VALUE IN FRAME f-pg-sel = "".

    RETURN.

END.
*/
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
    /*  aqui que a grava‡Æo da linha da temp-table ? efetivada.
       Por‚m as valida‡äes dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment rio */
    
    if br-digita:NEW-ROW in frame f-pg-dig then 
    do transaction on error undo, return no-apply:
        create tt-digita.
        assign input browse br-digita tt-digita.cod-estabel        
              /* INPUT BROWSE br-digita tt-digita.cod-emitente       
               input browse br-digita tt-digita.nome-abrev         */
               input browse br-digita tt-digita.it-codigo          
               input browse br-digita tt-digita.unid               
               input browse br-digita tt-digita.quantidade         
              /* input browse br-digita tt-digita.preco-item         
               input browse br-digita tt-digita.cod-cond-pag       */
               INPUT BROWSE br-digita tt-digita.dt-entrega         
              /* input browse br-digita tt-digita.nr-tab-preco       
               input browse br-digita tt-digita.perc-icms          
               input browse br-digita tt-digita.perc-ipi           
               input browse br-digita tt-digita.perc-rat-compra    
               input browse br-digita tt-digita.usuar-comprador    */
               input browse br-digita tt-digita.gerado-ordem       
               INPUT BROWSE br-digita tt-digita.descricao          
/*               INPUT BROWSE br-digita tt-digita.narrativa 
               INPUT BROWSE br-digita tt-digita.ct-codigo          
               INPUT BROWSE br-digita tt-digita.sc-codigo*/ .
            
            br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    end.
    else do transaction on error undo, return no-apply:
        
            assign input browse br-digita tt-digita.cod-estabel       
/*                   INPUT BROWSE br-digita tt-digita.cod-emitente      
                   input browse br-digita tt-digita.nome-abrev        */
                   input browse br-digita tt-digita.it-codigo         
                   input browse br-digita tt-digita.unid              
                   input browse br-digita tt-digita.quantidade        
                   /*input browse br-digita tt-digita.preco-item        
                   input browse br-digita tt-digita.cod-cond-pag      */
                   INPUT BROWSE br-digita tt-digita.dt-entrega        
/*                   input browse br-digita tt-digita.nr-tab-preco      
                   input browse br-digita tt-digita.perc-icms         
                   input browse br-digita tt-digita.perc-ipi          
                   input browse br-digita tt-digita.perc-rat-compra   
                   input browse br-digita tt-digita.usuar-comprador   */
                   input browse br-digita tt-digita.gerado-ordem      
                   INPUT BROWSE br-digita tt-digita.descricao         
/*                   INPUT BROWSE br-digita tt-digita.narrativa        
                   INPUT BROWSE br-digita tt-digita.ct-codigo          
                   INPUT BROWSE br-digita tt-digita.sc-codigo*/ .
                
            display
                tt-digita.cod-estabel     
/*                tt-digita.cod-emitente    
                tt-digita.nome-abrev FORMAT "x(12)"     */
                tt-digita.it-codigo       
                tt-digita.unid            
                tt-digita.quantidade      
              /*  tt-digita.preco-item      
                tt-digita.cod-cond-pag    */
                tt-digita.dt-entrega      
/*                tt-digita.nr-tab-preco    
                tt-digita.perc-icms       
                tt-digita.perc-ipi        
                tt-digita.perc-rat-compra 
                tt-digita.usuar-comprador */
                tt-digita.gerado-ordem    
                tt-digita.descricao       
/*                tt-digita.narrativa 
                tt-digita.ct-codigo       
                tt-digita.sc-codigo */
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
            tt-digita.cod-estabel        
/*            tt-digita.cod-emitente       
            tt-digita.nome-abrev         */
            tt-digita.it-codigo          
            tt-digita.unid               
            tt-digita.quantidade         
/*            tt-digita.preco-item         
            tt-digita.cod-cond-pag       */
            tt-digita.dt-entrega         
/*            tt-digita.nr-tab-preco       
            tt-digita.perc-icms          
            tt-digita.perc-ipi           
            tt-digita.perc-rat-compra    
            tt-digita.usuar-comprador    */
            tt-digita.gerado-ordem       
            tt-digita.descricao          
/*            tt-digita.narrativa          
            tt-digita.ct-codigo       
            tt-digita.sc-codigo */
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




&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar C-Win
ON CHOOSE OF bt-executar IN FRAME f-relat /* Gera Ordem de Compra */
DO:

    /* Gera‡Æo das ordens de compras selecionadas */

    FOR EACH tt-digita NO-LOCK.

        IF tt-digita.quantidade <= 0 THEN DO: 
            run utp/ut-msgs.p (input "show":U, input 737, "Quantidade").
            return no-apply.                      
        END.

        IF tt-digita.dt-entrega < TODAY THEN DO:
            run utp/ut-msgs.p (input "show":U, input 3048, "Data de Entrega").
            return no-apply.      
        END.
    END.
    
    /*
    IF AVAIL ordem-compra THEN DO:
        /* Dialog-box de Mensagens */
        DEFINE BUTTON db-bt-cancel AUTO-END-KEY 
             LABEL "&Fechar" 
             SIZE 10 BY 1
             BGCOLOR 8.

        DEFINE RECTANGLE db-rt-botoes
             EDGE-PIXELS 2 GRAPHIC-EDGE  
             SIZE 58.5 BY 1.42
             BGCOLOR 7.  

        DEFINE VARIABLE c-mensagem AS CHARACTER FORMAT "X(77)" NO-UNDO.

        ASSIGN c-mensagem = "Erro na numera‡Æo da Ord.Compra".

        DEFINE RECTANGLE db-rect-1
         EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
         SIZE 58 BY 1.42.

        DEFINE FRAME db-frame-1

            c-mensagem NO-LABEL 
               at ROW 1.5 col 3 

            db-rect-1 AT ROW 1.2 COL 2

            db-bt-cancel      AT ROW 3.3 COL 23             
            db-rt-botoes      AT ROW 3.0 COL 1.50
            SPACE(0.28)
            WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
                 THREE-D SCROLLABLE TITLE "Erro - Verifique antes de Prossegur" FONT 1
                 DEFAULT-BUTTON db-bt-cancel CANCEL-BUTTON db-bt-cancel.


        DISPLAY c-mensagem WITH FRAME db-frame-1.

        ENABLE db-bt-cancel 
            WITH FRAME db-frame-1. 

        WAIT-FOR "GO":U OF FRAME db-frame-1.

        /* Fim do Dialog-box */
        return no-apply.      

    END.
    */
    le-digita:
    FOR EACH tt-digita WHERE tt-digita.log-erro = NO EXCLUSIVE-LOCK.
        
        for each tt-erro:
            delete tt-erro.
        end.
        for each tt-ordem-compra:
            delete tt-ordem-compra.
        end.
        for each tt-prazo-compra:
            delete tt-prazo-compra.
        end.

        atualiza:
        DO TRANS:
            find first param-compra EXCLUSIVE-LOCK no-error.
        
            IF NOT AVAIL param-compra THEN 
               UNDO atualiza, LEAVE le-digita.
        
            ASSIGN i-ordem    = param-compra.prox-ord-aut. 
            do while  can-FIND (FIRST ordem-compra WHERE
                ordem-compra.numero-ordem = (i-ordem * 1)
                NO-LOCK ):
                i-ordem = i-ordem + 1.
                if i-ordem >  param-compra.prox-ord-aut + 100 then leave.
            
            end.
        
        
            FIND FIRST ordem-compra WHERE
                ordem-compra.numero-ordem = (i-ordem * 1)
                NO-LOCK NO-ERROR.
        
            IF AVAIL ordem-compra THEN DO:
        
        
                 ASSIGN tt-digita.narrativa    = "Erro na numera‡Æo de parametro de compras, ordem j  existe"
                       tt-digita.gerado-ordem = NO
                       tt-digita.log-erro     = NO.
                 find first param-compra no-lock no-error.
                 UNDO atualiza, LEAVE le-digita.
               
            END.
        
            FOR EACH tt-ordem-compra EXCLUSIVE-LOCK.
                DELETE tt-ordem-compra.
            END.
        
            FIND FIRST ITEM WHERE
                ITEM.it-codigo = tt-digita.it-codigo
                NO-LOCK NO-ERROR.
        
            IF NOT AVAIL ITEM THEN NEXT.
        
            FIND item-uni-estab WHERE
                item-uni-estab.cod-estabel = tt-digita.cod-estabel AND
                item-uni-estab.it-codigo   = tt-digita.it-codigo
                NO-LOCK NO-ERROR.
        
            CREATE tt-ordem-compra.
        
            ASSIGN tt-ordem-compra.numero-ordem = (i-ordem * 1)
                   tt-ordem-compra.l-gerou      = no.
                   tt-ordem-compra.nr-contrato = 0.
                   tt-ordem-compra.cod-emitente = 0.
                /*   tt-ordem-compra.sit-ordem-contrat */
        
                   tt-ordem-compra.num-seq-item = 0.
        
            IF tt-digita.usuar-comprador <> "" THEN
                ASSIGN tt-ordem-compra.cod-comprado = tt-digita.usuar-comprador.
        
            IF AVAIL item-uni-estab AND item-uni-estab.cod-comprado <> "" THEN
                ASSIGN tt-ordem-compra.cod-comprado = item-uni-estab.cod-comprado.
            ELSE
                ASSIGN tt-ordem-compra.cod-comprado = item.cod-comprado.
        
        
        
            assign tt-ordem-compra.cod-cond-pag   = tt-digita.cod-cond-pag 
                   tt-ordem-compra.situacao       = 1
                   tt-ordem-compra.origem         = 3
                   tt-ordem-compra.usuario        = c-seg-usuario
                   tt-ordem-compra.cod-estabel    = tt-digita.cod-estabel
                   tt-ordem-compra.it-codigo      = tt-digita.it-codigo
                   tt-ordem-compra.num-pedido     = 0.
        
            IF tt-digita.ct-codigo = ""  THEN
               ASSIGN tt-ordem-compra.conta-contabil = item.conta-aplicacao 
                      tt-ordem-compra.ct-codigo      = item.ct-codigo
                      tt-ordem-compra.sc-codigo      = item.sc-codigo
                      tt-ordem-compra.tp-despesa     = item.tp-desp-padrao.
        
            ELSE
               ASSIGN tt-ordem-compra.conta-contabil = trim(tt-digita.ct-codigo) + TRIM(tt-digita.sc-codigo)
                      tt-ordem-compra.ct-codigo      = trim(tt-digita.ct-codigo)
                      tt-ordem-compra.sc-codigo      = trim(tt-digita.sc-codigo)
                      tt-ordem-compra.tp-despesa     = item.tp-desp-padrao.
        
            FIND FIRST estabelec WHERE estabelec.cod-estabel = tt-digita.cod-estabel NO-LOCK NO-ERROR.
        
            ASSIGN tt-ordem-compra.qt-solic       = tt-digita.quantidade
                   tt-ordem-compra.data-emissao   = today
                   tt-ordem-compra.impr-ficha     = param-compra.imprime-fich
                   tt-ordem-compra.ep-codigo      = IF AVAIL estabelec THEN estabelec.ep-codigo ELSE i-ep-codigo-usuario
                   tt-ordem-compra.data-atualiz   = today        
                   tt-ordem-compra.hora-atualiz   = string(time,"hh:mm:ss")
                   tt-ordem-compra.narrativa      = "Contrato - " + item.desc-item
                   tt-ordem-compra.natureza       = 1 /*Compra*/
                   tt-ordem-compra.requisitante   = c-seg-usuario
                   tt-ordem-compra.sequencia      = 0
                   tt-ordem-compra.num-ord-inv    = 0
                   tt-ordem-compra.dep-almoxar    = item.deposito-pad
                   tt-ordem-compra.codigo-icm     = 1.
        
            if  avail item-uni-estab then
                assign tt-ordem-compra.tp-despesa  = item-uni-estab.tp-desp-padrao
                       tt-ordem-compra.dep-almoxar = item-uni-estab.deposito-pad.
        
            assign tt-ordem-compra.cod-estab-gestor = if avail item-uni-estab then
                                                         item-uni-estab.cod-estab-gestor
                                                         else "".
        
            create tt-prazo-compra.
            assign tt-prazo-compra.numero-ordem = tt-ordem-compra.numero-ordem
                   tt-prazo-compra.parcela      = 1
                   tt-prazo-compra.natureza     = tt-ordem-compra.natureza
                   tt-prazo-compra.situacao     = tt-ordem-compra.situacao
                   tt-prazo-compra.it-codigo    = tt-ordem-compra.it-codigo
                   tt-prazo-compra.cod-alter    = no
                   tt-prazo-compra.quantidade   = tt-digita.quantidade
                   tt-prazo-compra.quant-saldo  = tt-digita.quantidade
                   tt-prazo-compra.quantid-orig = tt-digita.quantidade 
                   tt-prazo-compra.un           = item.un
                   tt-prazo-compra.pedido-clien = ""
                   tt-prazo-compra.nr-sequencia = 0
                   tt-prazo-compra.data-entrega = tt-digita.dt-entrega
                   tt-prazo-compra.quantid-orig = tt-digita.quantidade.
        
            assign tt-prazo-compra.data-orig = tt-prazo-compra.data-entrega
        
                   l-erro                    = no
                   i-m-ordem                 = tt-ordem-compra.numero-ordem.  
            
            l-split = yes.
            
            run ccp/ccapi012.p (input-output table tt-ordem-compra,
                                input-output table tt-prazo-compra,
                                output       table tt-erro,
                                input        l-split). 
        
            ASSIGN l-erro = NO.
        
            for each tt-erro:
                assign l-erro = yes.
                ASSIGN tt-digita.narrativa    = tt-erro.mensagem
                       tt-digita.gerado-ordem = NO
                       tt-digita.log-erro     = NO.
        
            end.
        
        
            IF l-erro THEN
                UNDO atualiza, LEAVE le-digita.
        
            IF l-erro = NO THEN DO:
            
               ASSIGN tt-digita.numero-ordem = (i-ordem * 1)
                      tt-digita.gerado-ordem = YES
                      tt-digita.log-erro     = NO.
               find first ordem-compra where ordem-compra.numero-ordem =  tt-digita.numero-ordem no-lock no-error.
               
               if avail ordem-compra then 
                   assign tt-digita.nr-contrato = ordem-compra.nr-contrato
                          tt-digita.num-pedido  = ordem-compra.num-pedido.      
               
               RELEASE param-compra.
               find first param-compra exclusive-lock no-error.
               assign param-compra.prox-ord-aut = i-ordem + 1.
            
            
            END. /* Fim da Gera‡Æo da Cota‡Æo Autom tica - Aprova‡Æo Autom tica */
            
        END. /*END TRANS*/
        find first param-compra no-lock no-error.


    END.  /* for each tt-digita */
    /* Fim da Gera‡Æo das ordens de compras selecionadas */

    do  on error undo, return no-apply:
        run pi-executar.   
    end.

    FOR EACH tt-digita EXCLUSIVE-LOCK.

           DELETE tt-digita.

    END.

    CLOSE QUERY br-digita.
    open query br-digita for each tt-digita.

        apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
        apply "ENTRY":U to c-cod-estabel in frame f-pg-sel.
        return .

               
END.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


ON CHOOSE OF bt-arquivo-saida IN FRAME f-pg-sel
DO:
    /*{include/i-imarq.i c-arquivo-entrada f-pg-sel}*/
    
     SYSTEM-DIALOG GET-FILE c-arquivo-entrada
                    FILTERS "*.xls" "*.xls",
                            "*.*" "*.*"            
       DEFAULT-EXTENSION "xls"
       INITIAL-DIR "spool" 
       USE-FILENAME
       UPDATE l-ok.
    if  l-ok = yes then do:
        assign c-arquivo-entrada  = replace(c-arquivo-entrada, "~\", "/").
        display c-arquivo-entrada  with frame f-pg-sel.
    end.
     


END.                                             

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
   
   run pi-carrega-ordens.
   
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

assign v-cod-prog-gerado = "escc0027".


find first usuar_mestre
     where usuar_mestre.cod_usuario = c-seg-usuario no-lock no-error.

if avail usuar_mestre then 

   assign dir-jr = usuar_mestre.nom_dir_spool.

if substring(dir-jr, length(dir-jr), 1) = "\" or
   substring(dir-jr, length(dir-jr), 1) = "/" then
   assign dir-jr = substring(dir-jr, 1, length(dir-jr) - 1).

ASSIGN DIR-jr = REPLACE(dir-jr, "/", "\"). 


ASSIGN c-arquivo-entrada = dir-jr + "\escc0027" + ".xls". 

def var c-tit as char no-undo.

run grapi/gr2013a.p (input v-cod-prog-gerado,
                    input "Gera‡Æo de Ordens de Compras da Lista de Necessidades",
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
           screen-value = "Ordens"
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
          /* screen-value = "Parƒmetros"*/
           width        = 11
           row          = 1.8
           col          = im-pg-par:col in frame f-relat + 1.7
           visible      = no 
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
    run pi-muda-cor-label-folder(input "Ordens").

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
   ENABLE bt-executar bt-cancelar bt-ajuda im-pg-imp im-pg-sel im-pg-dig
   WITH FRAME f-relat IN WINDOW C-Win.
   

  {&OPEN-BROWSERS-IN-QUERY-c-win}

   
   DISPLAY 

    c-cod-estabel 
/*    i-cod-emitente-jr
*/
    pesq-jr
    text-saida
    c-arquivo-entrada 
   WITH FRAME f-pg-sel IN WINDOW C-Win.

   ENABLE  
       c-cod-estabel 
/*       i-cod-emitente-jr*/
       pesq-jr
       c-arquivo-entrada 
       bt-arquivo-saida
       rect-8 
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


       do on error undo, return error
          on stop  undo, return error:     

         {include/i-rpexa.i}

           

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

              assign tt-param.c-arquivo-jr          = input frame f-pg-sel c-arquivo-entrada   
                     tt-param.cod-estabel           = input frame f-pg-sel c-cod-estabel  .   

           {include/i-rpexb.i}

           if  session:set-wait-state("general":U) then.

           {include/i-rprun.i ccp/escc0027rp.p} 

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

PROCEDURE pi-erro-planilha.

    ASSIGN erro-jr = 9
           tt-digita.narrativa = mens-erro
           tt-digita.log-erro  = YES.


END PROCEDURE.


procedure pi-carrega-ordens.

    /* L¢gica do programa para Importar a Planilha, Testar os Erros, e Gerar
       o Browse de ordens a serem criadas */
       
    IF  pesq-jr:SCREEN-VALUE IN FRAME f-pg-sel = "1" AND
        c-arquivo-entrada:SCREEN-VALUE IN FRAME f-pg-sel = "" THEN DO:

        run utp/ut-msgs.p (input "show":U, input 326, "Planilha de Necessidade").
        APPLY "entry" TO c-arquivo-entrada IN FRAME f-pg-sel.
        return 'ADM-ERROR':U.      
                        
    END.

    FIND FIRST estabelec WHERE
        estabelec.cod-estabel = c-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel
        NO-LOCK NO-ERROR.

    IF NOT AVAIL estabelec THEN DO:

        run utp/ut-msgs.p (input "show":U, input 2, "Estabelecimento").
        APPLY "entry" TO c-cod-estabel IN FRAME f-pg-sel.
        return 'ADM-ERROR':U.      


    END.

/*
    FIND FIRST emitente WHERE
        emitente.cod-emitente = int(i-cod-emitente-jr:SCREEN-VALUE IN FRAME f-pg-sel)
        NO-LOCK NO-ERROR.

    IF NOT AVAIL emitente THEN DO:

        run utp/ut-msgs.p (input "show":U, input 2, "Fornecedor").
        APPLY "entry" TO i-cod-emitente-jr IN FRAME f-pg-sel.
        return 'ADM-ERROR':U.      


    END.
    */

    /* para executar o Excel */

    DEF VAR c-arq             AS CHAR                NO-UNDO.
    DEF VAR c-arq-anexo       AS CHAR                NO-UNDO.   
    DEF VAR l-cria            AS LOG                 NO-UNDO.
    DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
    DEF VAR i-linha           AS INT                 NO-UNDO.
    DEF VAR c-coluna          AS char                NO-UNDO.

    ASSIGN i-linha = 0
           erro-jr = 0.

    FOR EACH tt-digita EXCLUSIVE-LOCK.
        DELETE tt-digita.
    END.         

    FOR EACH tt-lista EXCLUSIVE-LOCK.
        DELETE tt-lista.
    END.         


    IF  pesq-jr:SCREEN-VALUE IN FRAME f-pg-sel = "1" THEN DO:

      def var c-modelo-planilha  as char format "x(50)"         no-undo.
      def var c-excel            as com-handle                  NO-UNDO.
      def var c-planilha         as com-handle.
      def var c-relatorio        as com-handle.
      def var c-relatorio1       as com-handle.
     
      DEF VAR c-arquivo-jr AS CHAR NO-UNDO.
                                     
      ASSIGN i-linha = 2.
     
      ASSIGN c-arquivo-jr = search(c-arquivo-entrada:SCREEN-VALUE IN FRAME f-pg-sel). 
      
      IF c-arquivo-jr = ? OR c-arquivo-jr = "" THEN DO:
          run utp/ut-msgs.p (input "show":U, input 326, "Planilha de Necessidade").
          APPLY "entry" TO c-arquivo-entrada IN FRAME f-pg-sel.
          return 'ADM-ERROR':U.             
      END.
      
      /* Cria Aplica‡Æo do Excel */
     
      CREATE "Excel.Application" c-excel.
      ASSIGN c-excel:DisplayAlerts = FALSE.
     
     
      ASSIGN c-modelo-planilha = search(c-arquivo-jr) 
             c-arq             = SESSION:TEMP-DIRECTORY.
     
     
      DEF VAR c-arquivo AS CHAR NO-UNDO.
                                                 
          assign c-arquivo = c-arquivo-jr .  
     
          assign c-planilha  = c-excel:Workbooks:OPEN(c-arquivo)
                 c-relatorio = c-excel:Sheets:item(1)
                 c-arq-anexo = (IF c-arq-anexo <> "" THEN "," ELSE "") + c-arquivo.


      DO WHILE i-linha <> 0 :
      
         ASSIGN i-linha  = i-linha + 1
                it-codigo-jr = STRING (c-relatorio:range("A" + STRING(i-linha)):VALUE).
      
         IF it-codigo-jr = "" OR it-codigo-jr = ? THEN LEAVE.
      
         CREATE tt-lista.
      
         ASSIGN tt-lista.cod-estabel  = c-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel
                tt-lista.it-codigo    = it-codigo-jr
                tt-lista.unid         = STRING (c-relatorio:range("C" + STRING(i-linha)):VALUE)
                valor-dec = INT (DEC (c-relatorio:range("D" + STRING(i-linha)):VALUE) * 100) / 100
                tt-lista.quantidade   = valor-dec
                tt-lista.dt-entrega   = DATE (c-relatorio:range("E" + STRING(i-linha)):VALUE)
                tt-lista.narrativa    = STRING (c-relatorio:range("F" + STRING(i-linha)):VALUE)
                tt-lista.ct-codigo    = STRING (c-relatorio:range("G" + STRING(i-linha)):VALUE)
                tt-lista.sc-codigo    = STRING (c-relatorio:range("H" + STRING(i-linha)):VALUE)
                tt-lista.linha        = i-linha
                tt-lista.origem       = INT (pesq-jr:SCREEN-VALUE IN FRAME f-pg-sel) .

         IF tt-lista.ct-codigo = ? THEN 
             ASSIGN tt-lista.ct-codigo = "" 
                    tt-lista.sc-codigo = "".

         IF tt-lista.sc-codigo = ? THEN 
             ASSIGN tt-lista.ct-codigo = "" 
                    tt-lista.sc-codigo = "".

         IF tt-lista.dt-entrega <= TODAY THEN 
             ASSIGN tt-lista.dt-entrega = TODAY + 10.

      END.

    END.  /* pesq-jr = 1 */

    IF  pesq-jr:SCREEN-VALUE IN FRAME f-pg-sel = "2" THEN DO:

        FOR EACH necessidade-oc NO-LOCK.

            find first item where 
                 item.it-codigo = necessidade-oc.it-codigo no-lock no-error.

            IF NOT AVAIL ITEM THEN NEXT.

            ASSIGN i-linha = i-linha + 1.

            CREATE tt-lista.

            ASSIGN tt-lista.cod-estabel  = c-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel
                   tt-lista.it-codigo    = necessidade-oc.it-codigo
                   tt-lista.unid         = item.un
                   tt-lista.quantidade   = necessidade-oc.qt-orig
                   tt-lista.dt-entrega   = necessidade-oc.data-entrega
                   tt-lista.narrativa    = ""
                   tt-lista.linha        = i-linha
                   tt-lista.ct-codigo    = ""
                   tt-lista.sc-codigo    = ""
                   tt-lista.origem       = INT (pesq-jr:SCREEN-VALUE IN FRAME f-pg-sel) .


         IF tt-lista.dt-entrega <= TODAY THEN 
             ASSIGN tt-lista.dt-entrega = TODAY + 10.



        END.

    END. /* pesq-jr = 2 */

    ASSIGN tem-reg = 0.

    FOR EACH tt-lista NO-LOCK.
    
        ASSIGN it-codigo-jr = tt-lista.it-codigo.
    
        CREATE tt-digita.

        ASSIGN tt-digita.cod-estabel  = c-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel
              /* tt-digita.cod-emitente = int(i-cod-emitente-jr:SCREEN-VALUE IN FRAME f-pg-sel) 
               tt-digita.nome-abrev   = c-nome-abrev:SCREEN-VALUE IN FRAME f-pg-sel*/
               tt-digita.it-codigo    = it-codigo-jr
               tt-digita.unid         = tt-lista.unid
               tt-digita.quantidade   = tt-lista.quantidade
               tt-digita.dt-entrega   = tt-lista.dt-entrega
               tt-digita.ct-codigo    = tt-lista.ct-codigo
               tt-digita.sc-codigo    = tt-lista.sc-codigo
               tt-digita.narrativa    = if tt-lista.narrativa = ? then "" else tt-lista.narrativa
               tt-digita.linha        = tt-lista.linha
               tt-digita.origem       = tt-lista.origem.

         
        /* find first emitente where 
              emitente.cod-emitente = tt-digita.cod-emitente no-lock no-error.
    
          IF NOT AVAIL emitente THEN DO:
              ASSIGN mens-erro = "Fornecedor NÆo Cadastrado no EMS".
              RUN pi-erro-planilha.
              NEXT.
          END.
         */
         find first item where 
              item.it-codigo = it-codigo-jr  no-lock no-error.
    
          IF NOT AVAIL item THEN DO:
              ASSIGN mens-erro = "Item NÆo Cadastrado no EMS".
              RUN pi-erro-planilha.
              NEXT.
          END.
          ELSE DO:
              FOR FIRST item-uni-estab WHERE item-uni-estab.cod-estabel = c-cod-estabel:SCREEN-VALUE IN FRAME f-pg-sel and
                                             item-uni-estab.it-codigo = ITEM.it-codigo NO-LOCK.

                   IF  item-uni-estab.cod-obsoleto > 1 THEN DO:
             
                      ASSIGN mens-erro = "ItemxEstabelecimento obsoleto".
    
                      RUN pi-erro-planilha.
                      NEXT.
                  END.
              END.
             

          END.

          ASSIGN descricao-jr = ITEM.desc-item
                 tt-digita.descricao = descricao-jr.

         /* IF tt-digita.unid <> ITEM.un THEN DO:
              ASSIGN mens-erro = "Unidade Diferente da Unidade do Item".
              RUN pi-erro-planilha.
              NEXT.
          END.
    */
          IF tt-digita.quantidade < 1 THEN DO:
              ASSIGN mens-erro = "Quantidade de Compra Deve ser no M¡nimo 1 (um)".
              RUN pi-erro-planilha.
              NEXT.
          END.

          IF tt-digita.dt-entrega = ? THEN DO:
              ASSIGN mens-erro = "Data de Entrega Errada".
              RUN pi-erro-planilha.
              NEXT.
          END.
           IF tt-digita.dt-entrega <= today THEN DO:
              ASSIGN mens-erro = "Data de Entrega Errada".
              RUN pi-erro-planilha.
              NEXT.
          END.


          IF tt-digita.ct-codigo <> "" OR tt-digita.sc-codigo <> "" THEN DO:

              FIND FIRST conta-contab WHERE
                  conta-contab.ep-codigo = "120"                 and
                  conta-contab.ct-codigo = tt-digita.ct-codigo AND
                  conta-contab.sc-codigo = tt-digita.sc-codigo AND
                  conta-contab.tipo      = 1
                  NO-LOCK NO-ERROR.

              IF NOT AVAIL conta-contab THEN DO:
                  ASSIGN mens-erro = "Conta/Sub-Conta Cont bil NÆo Cadastrada no EMS".
                  RUN pi-erro-planilha.
                  NEXT.

              END.

          END.


          
     END.  /*for each tt-lista*/

     IF  pesq-jr:SCREEN-VALUE IN FRAME f-pg-sel = "1" THEN 
         c-planilha:CLOSE().

   IF pesq-jr:SCREEN-VALUE IN FRAME f-pg-sel = "1" and
      erro-jr <> 0 THEN DO:

      FOR EACH tt-digita.
          
          IF tt-digita.log-erro = NO THEN
             ASSIGN tt-digita.log-erro = YES
                    tt-digita.narrativa = "Solic.sem Erros - Aguardando Acertos das Erradas".
      END.
 


/*      do  on error undo, return no-apply:
          run pi-executar.   
      end.
*/
   END.

   IF pesq-jr:SCREEN-VALUE IN FRAME f-pg-sel = "2" and
      tem-reg = 0 THEN DO:

  /*    do  on error undo, return no-apply:
          run pi-executar.   
      end.
*/
   END.
  
   CLOSE QUERY br-digita.
    open query br-digita for each tt-digita.



 

 enable bt-retirar   with frame f-pg-dig.
   IF (erro-jr = 0 AND pesq-jr:SCREEN-VALUE IN FRAME f-pg-sel = "1") or
      (tem-reg = 1 AND pesq-jr:SCREEN-VALUE IN FRAME f-pg-sel = "2")
       THEN DO:

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

          open query br-digita for each tt-digita WHERE tt-digita.log-erro = NO.
          apply 'entry' to tt-digita.quantidade in browse br-digita. 
       end.
    
    enable bt-retirar   with frame f-pg-dig.
    ENABLE bt-executar WITH FRAME f-relat.
     

   END. /* erro-jr = 0 */


    /* Fim da L¢gica do programa para Importar a Planilha, Testar os Erros, e Gerar
       o Browse de ordens a serem criadas */
       


END.

