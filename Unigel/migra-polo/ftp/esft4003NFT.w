&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa for mgmulti.empresa.
{include/i-prgvrs.i ESFT4003NFT 1.00.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/*:T Preprocessadores do Template de Relat¢rio                            */
/*:T Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA /*f-pg-cla

&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG f-pg-dig*/
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */
def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-arquivo-destino  as char    no-undo.
def var c-arq-term         as char    no-undo.

DEFINE VARIABLE h-botao AS HANDLE     NO-UNDO.
define temp-table tt-param no-undo
    
    field arquivo          as char format "x(35)"
     field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

     

/* Def temp-table de erros. Ela tbým est˜ definida na include dbotterr.i */
def temp-table rowerrors no-undo
    field errorsequence    as int
    field errornumber      as int
    field errordescription as char FORMAT "X(60)"
    field errorparameters  as char
    field errortype        as char
    field errorhelp        as char
    field errorsubtype     as char.

/* Definicao da tabela temporaria tt-notas-geradas, include {dibo/bodi317ef.i1} */

DEFINE TEMP-TABLE tt-pedidos
    FIELD nr-pedido    LIKE ped-venda.nr-pedido
    FIELD it-codigo    LIKE ped-item.it-codigo
    FIELD nr-sequencia LIKE ped-item.nr-sequencia 
    FIELD vl-preuni    LIKE ped-item.vl-preuni
    field qt-pedida    LIKE ped-item.qt-pedida
    FIELD qt-transf    LIKE ped-item.qt-pedida
    INDEX chave-ped IS PRIMARY UNIQUE 
                nr-pedido   
                it-codigo   
                nr-sequencia.

DEFINE TEMP-TABLE tt-paletes
    FIELD nr-pedido    LIKE ped-venda.nr-pedido
    FIELD it-codigo    LIKE ped-item.it-codigo
    FIELD nr-sequencia LIKE ped-item.nr-sequencia 
    FIELD vl-preuni    LIKE ped-item.vl-preuni
    FIELD peso-bruto   LIKE pallet.peso-bruto
    FIELD lote         LIKE wt-fat-ser-lote.lote
    FIELD cod-depos    LIKE wt-fat-ser-lote.cod-depos     
    FIELD cod-localiz  LIKE wt-fat-ser-lote.cod-localiz   
    FIELD cod-refer    LIKE wt-fat-ser-lote.cod-refer     
    FIELD dt-vali-lote LIKE wt-fat-ser-lote.dt-vali-lote  
    FIELD quantidade   AS DECIMAL LABEL "Qtde"

    INDEX chave-ped IS PRIMARY UNIQUE 
                nr-pedido   
                it-codigo   
                nr-sequencia
                lote
                cod-depos   
                cod-localiz 
                cod-refer   

    .


def temp-table tt-notas-geradas no-undo
    field rw-nota-fiscal as   rowid
    field nr-nota        like nota-fiscal.nr-nota-fis
    field seq-wt-docto   like wt-docto.seq-wt-docto.

DEFINE TEMP-TABLE tt-lotes no-undo
    FIELD nro-docto                LIKE movto-estoq.nro-docto                            
    FIELD sequencia                LIKE movto-estoq.num-sequen                 
    FIELD cod-estabel              LIKE movto-estoq.cod-estabel                             
    FIELD it-codigo                LIKE movto-estoq.it-codigo                               
    FIELD lote                     LIKE movto-estoq.lote                                    
    FIELD cod-refer                LIKE movto-estoq.cod-refer                              
    FIELD cod-localiz              LIKE movto-estoq.cod-localiz                               
    FIELD cod-depos                LIKE movto-estoq.cod-depos                               
    FIELD cod-emitente             LIKE movto-estoq.cod-emitente
    FIELD serie-docto              LIKE movto-estoq.serie-docto 
    FIELD un                       LIKE movto-estoq.un                                      
    FIELD quantidade               LIKE movto-estoq.quantidade  
    FIELD peso-bruto               LIKE pallet.peso-bruto
    FIELD dt-trans                 LIKE movto-estoq.dt-trans
    FIELD dt-vali-lote             LIKE saldo-estoq.dt-vali-lote
    FIELD nat-operacao             LIKE movto-estoq.nat-operacao
    INDEX chave IS PRIMARY UNIQUE cod-estabel
                                  cod-emitente
                                  nro-docto
                                  serie-docto
                                  sequencia   
                                  it-codigo   
                                  lote        
                                  cod-refer  
                                  cod-localiz 
                                  cod-depos
                                 .

DEFINE TEMP-TABLE tt-lotes-it no-undo
    FIELD nro-docto                LIKE movto-estoq.nro-docto                            
    FIELD sequencia                LIKE movto-estoq.num-sequen                 
    FIELD cod-estabel              LIKE movto-estoq.cod-estabel                             
    FIELD it-codigo                LIKE movto-estoq.it-codigo                               
    FIELD cod-refer                LIKE movto-estoq.cod-refer   
    FIELD lote                     LIKE movto-estoq.lote 
    FIELD cod-emitente             LIKE movto-estoq.cod-emitente
    FIELD serie-docto              LIKE movto-estoq.serie-docto 
    FIELD un                       LIKE movto-estoq.un                                      
    FIELD quantidade               LIKE movto-estoq.quantidade                            
    FIELD valor                    AS DECIMAL                         
    FIELD dt-trans                 LIKE movto-estoq.dt-trans
    FIELD nat-operacao             LIKE movto-estoq.nat-operacao
    INDEX chave IS PRIMARY UNIQUE cod-estabel
                                  cod-emitente
                                  nro-docto
                                  serie-docto
                                  sequencia   
                                  it-codigo
                                  cod-refer
                                  lote.


DEF TEMP-TABLE tt-ped-venda LIKE ped-venda
    FIELD r-rowid   AS ROWID.

/* BO para totalizar o pedido */

def var bo-ped-venda-cal as handle no-undo.
def var bo-ped-venda-com as handle no-undo.

DEFINE BUFFER b-estabelec FOR estabelec.



/* Transfer Definitions */

def var raw-param        as raw no-undo.
def buffer b-item for item.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE l-retira AS LOGICAL INITIAL YES   NO-UNDO.
 
DEFINE NEW GLOBAL SHARED VARIABLE h-q1 AS HANDLE     NO-UNDO.
DEFINE QUERY q1 FOR tt-paletes SCROLLING .

def stream s-saida-nft.

DEFINE VARIABLE i-qt-itens AS INTEGER    NO-UNDO.

    def var h-bodi317pr                   as handle no-undo.
    def var h-bodi317sd                   as handle no-undo.
    def var h-bodi317im1bra               as handle no-undo.
    def var h-bodi317va                   as handle no-undo.
    def var h-bodi317in                   as handle no-undo.
    def var h-bodi317ef                   as handle no-undo.
    DEF VAR h-boin404te                   AS HANDLE NO-UNDO.
    def var l-proc-ok-aux                 as log    no-undo.
    def var c-ultimo-metodo-exec          as char   no-undo.
    def var c-serie                       as char   no-undo.
    def var da-dt-emis-nota               as date   no-undo.
    def var da-dt-base-dup                as date   no-undo.
    def var da-dt-prvenc                  as date   no-undo.
    def var c-nr-pedido                   as char   no-undo.
    def var c-nat-operacao                as char   NO-UNDO INITIAL "5901".
    def var c-cod-canal-venda             as char   no-undo.
    def var i-seq-wt-docto                as int    no-undo.
    def var i-seq-wt-it-docto             as int    no-undo.
    def var i-cont-itens                  as int    no-undo.
    def var c-cod-refer                   as char   no-undo.
    def var de-quantidade                 as dec    no-undo.
    def var de-vl-preori-ped              as dec    no-undo.
    def var de-val-pct-desconto-tab-preco as dec    no-undo.
    def var de-per-des-item               as dec    no-undo.
    DEF VAR p-c-nr-nota-terc              AS CHAR   no-undo.
    DEF VAR p-c-serie-terc                AS CHAR   NO-UNDO.
    DEF VAR p-c-nat-oper-terc             AS CHAR   NO-UNDO.
    DEF VAR c-serie-dev                   LIKE saldo-terc.serie NO-UNDO.
    DEF VAR c-nota-dev                    LIKE saldo-terc.nro-docto NO-UNDO.
    DEF VAR c-nat-op-dev                  LIKE saldo-terc.nat-operacao NO-UNDO.
    DEFINE VARIABLE i-seq AS INTEGER    NO-UNDO.
    DEFINE VARIABLE i-limite AS INTEGER    NO-UNDO.

     

    /* Defini¯?o de um buffer para tt-notas-geradas */
def buffer b-tt-notas-geradas for tt-notas-geradas.
DEFINE BUFFER b-tt-lotes FOR tt-lotes.
DEFINE VARIABLE c-seq-geradas AS CHARACTER  NO-UNDO.
 
DEFINE VARIABLE hShowMsg      AS HANDLE     NO-UNDO.
DEFINE VARIABLE c-it-codigo AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-cod-estabel AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-nome-abrev AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-lote           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-cod-localiz    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-cod-depos      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE d-dt-vali-lote AS DATE       NO-UNDO.
DEFINE VARIABLE d-quantidade AS DECIMAL    NO-UNDO.
DEFINE VARIABLE d-peso-bru-tot AS DECIMAL    NO-UNDO.
DEFINE VARIABLE d-peso-liq-tot AS DECIMAL    NO-UNDO.
DEFINE VARIABLE d-preco-unit AS DECIMAL    NO-UNDO.
DEFINE VARIABLE d-perc-atend AS DECIMAL    NO-UNDO.
 

 
                    
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-destino bt-arquivo bt-config-impr ~
c-arquivo rs-execucao RECT-7 RECT-9 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
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

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 2 
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

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.71.

DEFINE BUTTON bt-retira AUTO-END-KEY 
     LABEL "&Retira" 
     SIZE 10 BY .96.

DEFINE BUTTON bt-exclui 
     LABEL "Ex&clui" 
     SIZE 10 BY .96.

DEFINE BUTTON bt-limpar 
     LABEL "&Limpar" 
     SIZE 10 BY .96.

DEFINE VARIABLE ed-pedidos AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 20.72 BY 7.54 NO-UNDO.

DEFINE VARIABLE ed-obs AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 46 BY 1.6 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-nf AS CHARACTER FORMAT "X(15)":U 
     initial "{cdp\poloestab.i 434}"/*solic-318*/
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-nr-pedido-p AS CHARACTER FORMAT "X(15)":U 
     LABEL "&Pedido"  
     VIEW-AS FILL-IN TOOLTIP "Digite o n£mero do pedido e tecle ENTER para inclui."
    
     SIZE 13.86 BY .88 NO-UNDO
    .


DEFINE VARIABLE i-nr-sequencia-p AS integer FORMAT "999":U
     INITIAL 10
     LABEL "&Seq." 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .88 NO-UNDO.


DEFINE VARIABLE c-it-codigo-p AS CHARACTER FORMAT "X(15)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .88 NO-UNDO.




DEFINE VARIABLE c-natur-oper AS CHARACTER FORMAT "X(8)":U  INITIAL "6152TB"
     LABEL "&Nat.Opera‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome-abrev-nf AS CHARACTER FORMAT "X(15)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 34.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-serie-nf AS CHARACTER FORMAT "X(4)":U  initial "20"
     LABEL "Serie" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE d-emissao-nf AS DATE FORMAT "99/99/9999":U 
     LABEL "&Dt. EmissÆo NF" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.



DEFINE VARIABLE i-cod-emitente AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "&Emitente" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome-transp AS character FORMAT "x(12)" 
     LABEL "&Transportador" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.


DEFINE BROWSE b1 QUERY q1 DISPLAY 
    nr-pedido    WIDTH 6
    it-codigo   
    lote         WIDTH 10
    quantidade   width 7
    vl-preuni    WIDTH 7
    cod-refer    WIDTH 8
    nr-sequencia WIDTH 3
    cod-depos   
    cod-localiz 
    dt-vali-lote
    
     WITH  NO-ASSIGN SEPARATORS 
    SIZE 63.7 BY 6.9 /*TITLE "Customer Browse"*/.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "E&xecutar" 
     SIZE 10 BY 1.

DEFINE BUTTON Bt-tampa  NO-FOCUS /*FLAT-BUTTON*/
IMAGE-UP FILE "image/im-copbm.bmp":U
     LABEL "" 
     SIZE 90 BY 1.5.

DEFINE IMAGE im-pg-dig
     FILENAME "image\im-fldup":U
     SIZE 14 BY 1.

DEFINE IMAGE im-pg-imp
     FILENAME "image\im-fldup":U
     SIZE 14 BY 1.25.

DEFINE IMAGE im-pg-sel
     FILENAME "image\im-fldup":U
     SIZE 1 BY .21.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 91 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0  
     SIZE 89.86 BY .13
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-left
     EDGE-PIXELS 0  
     SIZE .43 BY 15.21
     BGCOLOR 15 .

DEFINE RECTANGLE rt-folder-right
     EDGE-PIXELS 0  
     SIZE .43 BY 15.17
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-top
     EDGE-PIXELS 0  
     SIZE 89.72 BY .13
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 5.2.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 22 BY 13.12.


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
     Bt-tampa AT ROW 1 COL 2 NO-TAB-STOP 
     bt-executar AT ROW 18.75 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 18.75 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 18.75 COL 81.29 HELP
          "Ajuda"
     RECT-6 AT ROW 17.63 COL 2.14
     RECT-1 AT ROW 18.5 COL 2
     im-pg-dig AT ROW 1.5 COL 2
     im-pg-imp AT ROW 1.5 COL 67
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 1.72
     rt-folder-right AT ROW 2.67 COL 91.72
     im-pg-sel AT ROW 2.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92.29 BY 19.08
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-sel
    c-cod-estabel-nf AT ROW 1.83 COL 15.72 COLON-ALIGNED
     d-emissao-nf AT ROW 2.83 COL 15.72 COLON-ALIGNED
     i-cod-emitente AT ROW 3.88 COL 15.72 COLON-ALIGNED
     c-nome-abrev-nf AT ROW 3.88 COL 27.29 COLON-ALIGNED NO-LABEL
     c-natur-oper AT ROW 1.83 COL 53.57 COLON-ALIGNED
     c-serie-nf AT ROW 2.83 COL 53.57 COLON-ALIGNED
     ed-obs AT ROW 4.9 COL 17.72 NO-LABEL
     c-nr-pedido-p    AT ROW 2 COL 69.72 COLON-ALIGNED
        HELP "Digite o n£mero do pedido e tecle ENTER para inclui."
     i-nr-sequencia-p AT ROW 3 COL 69.72 COLON-ALIGNED
     bt-retira AT ROW 13.6 COL 1.72
     c-nome-transp AT ROW 13.6 COL 27.29
     bt-exclui AT ROW 5.5 COL 65.72
     bt-limpar AT ROW 5.5 COL 76.43
     ed-pedidos AT ROW 6.58 COL 65.72 NO-LABEL
    
     
     
     c-it-codigo-p    AT ROW 4 COL 69.72 COLON-ALIGNED
/*     
     IMAGE-10 AT ROW 10.46 COL 35.72
     IMAGE-11 AT ROW 9.46 COL 35.72
     IMAGE-12 AT ROW 9.46 COL 32.57
     IMAGE-7 AT ROW 7.42 COL 32.57
     IMAGE-8 AT ROW 7.42 COL 35.72
     IMAGE-13 AT ROW 8.42 COL 35.72
     IMAGE-14 AT ROW 8.42 COL 32.57
     IMAGE-9 AT ROW 10.46 COL 32.57*/
     RECT-12 AT ROW 1.38 COL 1
     RECT-13 AT ROW 1.38 COL 65
    /* RECT-14 AT ROW 7.35 COL 1*/
    " Pedidos" VIEW-AS TEXT
          SIZE 7 BY .88 AT ROW 1 COL 75 RIGHT-ALIGNED
     " Observa‡Æo:" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 4.88 COL 16.29 RIGHT-ALIGNED
     "Destino" VIEW-AS TEXT
          SIZE 5.3 BY .77 AT ROW 1 COL 6 RIGHT-ALIGNED

    b1                AT ROW 6.6 COLUMN 1.12


    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 86.86 BY 14.62.


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
         TITLE              = "Notas de Transferˆncia"
         HEIGHT             = 19.29
         WIDTH              = 93.14
         MAX-HEIGHT         = 28.63
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.63
         VIRTUAL-WIDTH      = 146.29
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

/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR BUTTON Bt-tampa IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE im-pg-dig IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE im-pg-imp IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
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
ON END-ERROR OF w-relat /* Notas de Transferˆncia */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Notas de Transferˆncia */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-relat
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
   {include/ajuda.i}
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
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-relat
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Fechar */
DO:
   apply "close":U to this-procedure.
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

 


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME bt-exclui
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exclui w-relat
ON CHOOSE OF bt-exclui IN FRAME f-pg-sel /* Exclui */
DO:
    FOR EACH tt-pedidos  WHERE 
        tt-pedidos.nr-pedido = int(c-nr-pedido-p:SCREEN-VALUE) :
        DELETE tt-pedidos.

    END.

    FOR EACH tt-paletes  WHERE 
        tt-paletes.nr-pedido = int(c-nr-pedido-p:SCREEN-VALUE) :
        DELETE tt-paletes.

    END.
    RUN pi-mostra-paletes.
    APPLY 'entry' TO c-nr-pedido-p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-relat
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:

 

    IF trim(c-nome-transp:SCREEN-VALUE  IN FRAME f-pg-sel) = "" THEN DO:
         RUN utp/ut-msgs.p(INPUT "show",
                         INPUT 2,
                         INPUT "transporte " + "BRANCO"). 

        APPLY "entry" TO c-nome-transp  IN FRAME f-pg-sel.
        RETURN NO-APPLY.

    END.  



       DO TRANSACTION:
        do  on error undo, return no-apply:
            run pi-executar.
        end.
       END.


   
   FIND FIRST WT-DOCTO NO-LOCK NO-ERROR.
      IF AVAIL WT-DOCTO THEN   RELEASE WT-DOCTO NO-ERROR.

      IF length(TRIM(c-seq-geradas)) > 1 THEN
   RUN utp/ut-msgs.p(INPUT "show",
                     INPUT 29689,
                     INPUT "Gera‡Æo para Calculo de NFs de Itens " + "~~"  +
                           ". Sequˆncia(s):" + c-seq-geradas).
ELSE

    RUN utp/ut-msgs.p(INPUT "show",
                      INPUT 15684,
                      INPUT "Gera‡Æo para Calculo de NFs de Itens ").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME bt-limpar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-limpar w-relat
ON CHOOSE OF bt-limpar IN FRAME f-pg-sel /* Limpar */
DO:
  FOR EACH tt-pedidos:
        DELETE tt-pedidos.
  END.

  FOR EACH tt-paletes:
      DELETE tt-paletes.
  END.

  RUN pi-mostra-paletes.
  APPLY 'entry' TO c-nr-pedido-p.
     
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define BROWSE-NAME b1
&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME b1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b1 w-relat
ON DEL OF b1 IN FRAME f-pg-sel
DO:
   apply 'choose' to bt-retira in frame f-pg-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME bt-retira
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-limpar w-relat
ON CHOOSE OF bt-retira IN FRAME f-pg-sel /* Limpar */
DO:


    if  b1:num-selected-rows > 0 then do on error undo, return no-apply:
        get current q1.
        delete tt-paletes.
        if  b1:delete-current-row() in frame f-pg-sel then.
    end.

  ASSIGN l-retira = NO.

  RUN pi-mostra-paletes.
  APPLY 'entry' TO bt-retira.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&Scoped-define SELF-NAME c-cod-estabel-nf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-estabel-nf w-relat
ON LEAVE OF c-cod-estabel-nf IN FRAME f-pg-sel /* Estabelecimento */
DO:
    FIND FIRST estabelec NO-LOCK WHERE
         estabelec.cod-estabel = c-cod-estabel-nf:SCREEN-VALUE NO-ERROR.

    IF NOT AVAIL estabelec THEN DO:
         RUN utp/ut-msgs.p(INPUT "show",
                         INPUT 2,
                         INPUT "estabelec " + c-cod-estabel-nf:SCREEN-VALUE). 

        APPLY "entry" TO c-cod-estabel-nf.
        RETURN NO-APPLY.

    END.  


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME c-nr-pedido-pp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-pedido-p w-relat
ON RETURN OF c-nr-pedido-p IN FRAME f-pg-sel /* Item */
DO:
  IF trim(c-nr-pedido-p:SCREEN-VALUE) = "" THEN RETURN NO-APPLY.
  apply "return"  to i-nr-sequencia-p.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
  


&Scoped-define SELF-NAME i-nr-sequencia-p
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-nr-sequencia-p w-relat
ON RETURN OF i-nr-sequencia-p IN FRAME f-pg-sel /* Item */
DO:
  IF trim(c-nr-pedido-p:SCREEN-VALUE) = "" THEN RETURN NO-APPLY.

  FIND FIRST ped-venda NO-LOCK WHERE
       ped-venda.nr-pedido = int(c-nr-pedido-p:SCREEN-VALUE) NO-ERROR.

  IF NOT AVAIL ped-venda THEN DO:
       RUN utp/ut-msgs.p(INPUT "show",
                       INPUT 2,
                       INPUT "Pedido " + c-nr-pedido-p:SCREEN-VALUE). 
       
      APPLY "entry" TO c-nr-pedido-p.
      RETURN NO-APPLY.

  END.  

  FIND FIRST ped-item OF ped-venda WHERE ped-item.ind-componen < 3 and
  ped-item.nr-sequencia = int(i-nr-sequencia-p:screen-value)  no-lock no-error.
  
  IF NOT AVAIL ped-item THEN DO:
      RUN utp/ut-msgs.p(INPUT "show",
                      INPUT 2,
                      INPUT "Item do Pedido " + c-nr-pedido-p:SCREEN-VALUE). 

     APPLY "entry" TO c-nr-pedido-p.
     RETURN NO-APPLY.
  END.

  
  IF NOT CAN-FIND(FIRST pallet WHERE pallet.nr-pedido = ped-venda.nr-pedido NO-lock) THEN DO:
      RUN utp/ut-msgs.p(INPUT "show",
                      INPUT 2,
                      INPUT "Paletes p/ Pedido " + c-nr-pedido-p:SCREEN-VALUE). 

     APPLY "entry" TO c-nr-pedido-p.
     RETURN NO-APPLY.
  END.

  FOR EACH pallet WHERE pallet.nr-pedido = ped-venda.nr-pedido and
                        pallet.nr-sequencia = ped-item.nr-sequencia NO-lock,
      EACH saldo-estoq WHERE 
             saldo-estoq.cod-estabel = c-cod-estabel-nf:SCREEN-VALUE AND
             saldo-estoq.it-codigo   = pallet.it-codigo AND
             saldo-estoq.lote        = pallet.nr-pallet NO-LOCK.

      if NOT (saldo-estoq.qtidade-atu - (saldo-estoq.qt-aloc-ped  + 
                                      saldo-estoq.qt-aloc-prod +
                                      saldo-estoq.qt-alocada) <> 0 and
         saldo-estoq.cod-refer    = ped-item.cod-refer and
         saldo-estoq.dt-vali-lote >= date(d-emissao-nf:SCREEN-VALUE)) THEN NEXT.
    
      FIND FIRST tt-paletes WHERE 
                     tt-paletes.nr-pedido     = ped-venda.nr-pedido          AND
                     tt-paletes.it-codigo     = ped-item.it-codigo           AND
                     tt-paletes.nr-sequencia  = ped-item.nr-sequencia        AND
                     tt-paletes.lote          = saldo-estoq.lote         AND
                     tt-paletes.cod-depos     = saldo-estoq.cod-depos    AND   
                     tt-paletes.cod-localiz   = saldo-estoq.cod-localiz  AND
                     tt-paletes.cod-refer     = saldo-estoq.cod-refer    
           NO-ERROR.


      IF emitente.estado <> estabelec.estado THEN
                   ASSIGN d-preco-unit = ped-item.vl-preuni / 0.88.
               ELSE
                   ASSIGN d-preco-unit = ped-item.vl-preuni / 0.82.

            
      IF NOT AVAIL tt-paletes THEN DO:
       
           FIND LAST pr-it-per WHERE
             pr-it-per.it-codigo = ped-item.it-codigo  and
             pr-it-per.cod-estabel = estabelec.cod-estabel NO-LOCK NO-ERROR.

           IF AVAIL pr-it-per  THEN 

               ASSIGN d-preco-unit = pr-it-per.val-unit-ggf-m[1] + pr-it-per.val-unit-mat-m[1] .

           
           IF NOT AVAIL pr-it-per then do:
           
               find b-item where b-item.it-codigo = ped-item.it-codigo no-lock no-error.
               
                IF AVAIL item  THEN  

                   ASSIGN d-preco-unit = item.preco-ul-ent .

           end.   
           
               IF emitente.estado <> estabelec.estado THEN
                   ASSIGN d-preco-unit = d-preco-unit / 0.88.
               ELSE
                   ASSIGN d-preco-unit = d-preco-unit / 0.82.

           if d-preco-unit = 0 then  DO:
               
                
                RUN utp/ut-msgs.p(INPUT "show",
                      INPUT 2,
                      INPUT "Pre‡o de custo para Item do Pedido " + c-nr-pedido-p:SCREEN-VALUE). 
            
                 APPLY "entry" TO c-nr-pedido-p.
                 RETURN NO-APPLY.
           END.

            


          CREATE tt-paletes.
          ASSIGN
                  tt-paletes.nr-pedido     = ped-venda.nr-pedido   
                  tt-paletes.it-codigo     = ped-item.it-codigo    
                  tt-paletes.nr-sequencia  = ped-item.nr-sequencia 
                  tt-paletes.lote          = saldo-estoq.lote
                  tt-paletes.cod-depos     = saldo-estoq.cod-depos   
                  tt-paletes.cod-localiz   = saldo-estoq.cod-localiz 
                  tt-paletes.cod-refer     = saldo-estoq.cod-refer
                  tt-paletes.dt-vali-lote  = saldo-estoq.dt-vali-lote
                  tt-paletes.quantidade    = saldo-estoq.qtidade-atu
                  tt-paletes.vl-preuni     = d-preco-unit /*ped-item.vl-preuni*/
                  tt-paletes.peso-bruto    = pallet.peso-bruto.

      END.

  END.

  FIND FIRST tt-paletes WHERE tt-paletes.nr-pedido    = ped-venda.nr-pedido  and
                              tt-paletes.nr-sequencia = ped-item.nr-sequencia NO-ERROR.
  IF NOT AVAIL tt-paletes THEN DO:
      RUN utp/ut-msgs.p(INPUT "show",
                      INPUT 2,
                      INPUT "Saldo para Item do Pedido " + c-nr-pedido-p:SCREEN-VALUE). 

     APPLY "entry" TO c-nr-pedido-p.
     RETURN NO-APPLY.
  END.



  ASSIGN i-qt-itens = 0.

  FOR EACH tt-pedidos NO-LOCK.

      ASSIGN i-qt-itens = i-qt-itens + 1.

  END.

  ASSIGN c-it-codigo-p:SCREEN-VALUE = ped-item.it-codigo
         i-nr-sequencia-p:SCREEN-VALUE = string(ped-item.nr-sequencia,"999").

  FIND FIRST tt-pedidos NO-LOCK WHERE
       tt-pedidos.nr-pedido    = int(c-nr-pedido-p:SCREEN-VALUE)    AND
       tt-pedidos.nr-sequencia = int(i-nr-sequencia-p:SCREEN-VALUE) AND
       tt-pedidos.it-codigo    = c-it-codigo-p:SCREEN-VALUE    NO-ERROR.

  IF NOT AVAIL tt-pedidos THEN DO:
      

    
    CREATE tt-pedidos.

    ASSIGN 
        tt-pedidos.nr-pedido    = int(c-nr-pedido-p:SCREEN-VALUE   )
        tt-pedidos.nr-sequencia = int(i-nr-sequencia-p:SCREEN-VALUE)
        tt-pedidos.it-codigo    = c-it-codigo-p:SCREEN-VALUE   
        tt-pedidos.vl-preuni    = ped-item.vl-preuni.

     
        RUN pi-mostra-paletes.
        APPLY 'entry' TO c-nr-pedido-p.
    
 
    
    

  END.

  ELSE
      ASSIGN c-nr-pedido-p:SCREEN-VALUE = ""
             c-it-codigo-p:SCREEN-VALUE = ""
             i-nr-sequencia-p:SCREEN-VALUE = "010".
             
     APPLY 'entry' TO c-nr-pedido-p.    




END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-natur-oper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-natur-oper w-relat
ON LEAVE OF c-natur-oper IN FRAME f-pg-sel /* Nat.Opera‡Æo */
DO:
    FIND FIRST natur-oper NO-LOCK WHERE
         natur-oper.nat-operacao = c-natur-oper:SCREEN-VALUE NO-ERROR.

    IF NOT AVAIL natur-oper THEN DO:
         RUN utp/ut-msgs.p(INPUT "show",
                         INPUT 2,
                         INPUT "natur-oper " + c-natur-oper:SCREEN-VALUE). 

        APPLY "entry" TO c-natur-oper.
        RETURN NO-APPLY.

    END.  

    IF natur-oper.nat-operacao <> "5905gi" AND NOT natur-oper.TRANSf THEN DO:
         RUN utp/ut-msgs.p(INPUT "show",
                         INPUT 2,
                         INPUT "natureza de opera‡Æo de transferˆncia" + c-natur-oper:SCREEN-VALUE). 

        APPLY "entry" TO c-natur-oper.
        RETURN NO-APPLY.

    END.
    IF AVAIL emitente THEN DO:
        IF (emitente.estado <> estabelec.estado AND 
            substring(c-natur-oper:screen-value,1,1) = "5" ) OR 
            (emitente.estado = estabelec.estado AND 
            substring(c-natur-oper:screen-value,1,1) <> "5" ) THEN DO:
            RUN utp/ut-msgs.p(INPUT "show",
                        INPUT 5213,
                        INPUT "" ).
            APPLY "entry" TO c-natur-oper.
            RETURN NO-APPLY.

        END.
       
        IF  emitente.cod-emitente = 16779 THEN  
           ASSIGN c-natur-oper:SCREEN-VALUE = "5905GI".

    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME i-cod-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-cod-emitente w-relat
ON LEAVE OF i-cod-emitente IN FRAME f-pg-sel /* Emitente */
DO:
    FIND FIRST emitente NO-LOCK WHERE
         emitente.cod-emitente = int(i-cod-emitente:SCREEN-VALUE) NO-ERROR.

    IF NOT AVAIL emitente THEN DO:
         RUN utp/ut-msgs.p(INPUT "show",
                         INPUT 2,
                         INPUT "emitente " + i-cod-emitente:SCREEN-VALUE). 

        APPLY "entry" TO i-cod-emitente.
        RETURN NO-APPLY.

    END.  

    ASSIGN c-nome-abrev-nf:SCREEN-VALUE = emitente.nome-abrev.
    IF emitente.estado <> estabelec.estado THEN
       ASSIGN c-natur-oper:SCREEN-VALUE = emitente.nat-ope-ext .
    ELSE
       ASSIGN c-natur-oper:SCREEN-VALUE = emitente.nat-operacao.

    IF  emitente.cod-emitente <> 16779 AND substring(emitente.cgc,1,8) <> substring(estabelec.cgc,1,8) THEN DO:
        RUN utp/ut-msgs.p(INPUT "show",
                         INPUT 2,
                         INPUT "Emitente " + " com cnpj iniciado com " + substring(emitente.cgc,1,8) + "/xxxx-xx" ).

        APPLY "entry" TO i-cod-emitente.
        RETURN NO-APPLY.

    END.

    IF  emitente.cod-emitente = 101834 THEN
        ASSIGN c-natur-oper:SCREEN-VALUE = "5905GI".
        
    ASSIGN c-natur-oper:SCREEN-VALUE = "6152TB".
    

    
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME c-nome-transp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nome-transp w-relat
ON LEAVE OF c-nome-transp IN FRAME f-pg-sel /* Transportadora*/
DO:
    FIND FIRST transporte NO-LOCK WHERE
         transporte.nome-abrev = c-nome-transp:SCREEN-VALUE NO-ERROR.

    IF NOT AVAIL transporte THEN DO:
         RUN utp/ut-msgs.p(INPUT "show",
                         INPUT 2,
                         INPUT "transporte " + c-nome-transp:SCREEN-VALUE). 

        APPLY "entry" TO c-nome-transp.
        RETURN NO-APPLY.

    END.  

     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME i-cod-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-cod-emitente w-relat
ON MOUSE-SELECT-DBLCLICK  OF i-cod-emitente IN FRAME f-pg-sel /* Emitente */
DO:
    apply 'F5' to self.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME i-cod-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-cod-emitente w-relat
ON 'F5'  OF i-cod-emitente IN FRAME f-pg-sel /* Emitente */
DO:
      
      if  valid-handle(wh-pesquisa) and
          wh-pesquisa:TYPE = "PROCEDURE":U and
          wh-pesquisa:FILE-NAME = "adzoom/z07ad098.w":U then
            return.
          
      RUN adzoom/z07ad098.w persistent set wh-pesquisa.
    
      if  not valid-handle(wh-pesquisa) or 
              wh-pesquisa:TYPE <> "PROCEDURE":U or 
              wh-pesquisa:FILE-NAME <> "adzoom/z07ad098.w":U then
          return.
          
      /*run pi-seta-est in wh-pesquisa (input i-cod-emitente).*/
      
    
      RUN dispatch IN wh-pesquisa ('initialize':U).
      
      if valid-handle(wh-pesquisa) and
         wh-pesquisa:TYPE = "PROCEDURE":U and
         wh-pesquisa:FILE-NAME = "adzoom/z07ad098.w":U then do:
        
            RUN pi-entry IN wh-pesquisa.
        
          
        
            
            define variable c-lista-campo as char init '' no-undo.
                
            assign c-lista-campo = string(i-cod-emitente:handle in frame f-pg-sel) + '|':U + 'cod-emitente'.
                 
                
            run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    
            RUN add-link IN adm-broker-hdl
                           (INPUT wh-pesquisa,
                            INPUT 'State':U,
                            INPUT this-procedure).
            
    
        
      end.
                        
    assign l-implanta = no.
    

    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME






&Scoped-define SELF-NAME c-nome-transp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nome-transp w-relat
ON MOUSE-SELECT-DBLCLICK  OF c-nome-transp IN FRAME f-pg-sel /* Emitente */
DO:
    apply 'F5' to self.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME c-nome-transp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nome-transp w-relat
ON 'F5'  OF c-nome-transp IN FRAME f-pg-sel /* Emitente */
DO:
      
      if  valid-handle(wh-pesquisa) and
          wh-pesquisa:TYPE = "PROCEDURE":U and
          wh-pesquisa:FILE-NAME = "adzoom/z01ad268.w":U then
            return.
          
      RUN adzoom/z01ad268.w persistent set wh-pesquisa.
    
      if  not valid-handle(wh-pesquisa) or 
              wh-pesquisa:TYPE <> "PROCEDURE":U or 
              wh-pesquisa:FILE-NAME <> "adzoom/z01ad268.w":U then
          return.
          
      /*run pi-seta-est in wh-pesquisa (input c-nome-transp).*/
      
    
      RUN dispatch IN wh-pesquisa ('initialize':U).
      
      if valid-handle(wh-pesquisa) and
         wh-pesquisa:TYPE = "PROCEDURE":U and
         wh-pesquisa:FILE-NAME = "adzoom/z01ad268.w":U then do:
        
            RUN pi-entry IN wh-pesquisa.
        
          
        
            
            define variable c-lista-campo as char init '' no-undo.
                
            assign c-lista-campo = string(c-nome-transp:handle in frame f-pg-sel) + '|':U + 'nome-abrev'.
                 
                
            run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    
            RUN add-link IN adm-broker-hdl
                           (INPUT wh-pesquisa,
                            INPUT 'State':U,
                            INPUT this-procedure).
            
    
        
      end.
                        
    assign l-implanta = no.
    

    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





&Scoped-define SELF-NAME c-natur-oper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-natur-oper w-relat
ON MOUSE-SELECT-DBLCLICK  OF c-natur-oper IN FRAME f-pg-sel /* Emitente */
DO:
    apply 'F5' to self.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME c-natur-oper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-natur-oper w-relat
ON 'F5'  OF c-natur-oper IN FRAME f-pg-sel /* CFOP */
DO:
      
      if  valid-handle(wh-pesquisa) and
          wh-pesquisa:TYPE = "PROCEDURE":U and
          wh-pesquisa:FILE-NAME = "inzoom/z01in245.w":U then
            return.
          
      RUN inzoom/z01in245.w persistent set wh-pesquisa.
    
      if  not valid-handle(wh-pesquisa) or 
              wh-pesquisa:TYPE <> "PROCEDURE":U or 
              wh-pesquisa:FILE-NAME <> "inzoom/z01in245.w":U then
          return.
          
      /*run pi-seta-est in wh-pesquisa (input i-cod-emitente).*/
      
    
      RUN dispatch IN wh-pesquisa ('initialize':U).
      
      if valid-handle(wh-pesquisa) and
         wh-pesquisa:TYPE = "PROCEDURE":U and
         wh-pesquisa:FILE-NAME = "inzoom/z01in245.w":U then do:
        
            RUN pi-entry IN wh-pesquisa.
        
          
        
            
            define variable c-lista-campo as char init '' no-undo.
                
            assign c-lista-campo = string(c-natur-oper:handle in frame f-pg-sel) + '|':U + 'nat-operacao'.
                 
                
            run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    
            RUN add-link IN adm-broker-hdl
                           (INPUT wh-pesquisa,
                            INPUT 'State':U,
                            INPUT this-procedure).
            
    
        
      end.
                        
    assign l-implanta = no.
    

    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME c-nr-pedido-p
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-pedido-p w-relat
ON MOUSE-SELECT-DBLCLICK  OF c-nr-pedido-p IN FRAME f-pg-sel /* PEDIDO*/
DO:
    apply 'F5' to self.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME c-nr-pedido-p
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-pedido-p w-relat
ON 'F5'  OF c-nr-pedido-p IN FRAME f-pg-sel /* PEDIDO */
DO:
      
      if  valid-handle(wh-pesquisa) and
          wh-pesquisa:TYPE = "PROCEDURE":U and
          wh-pesquisa:FILE-NAME = "dizoom/z01di159.w":U then
            return.
          
      RUN dizoom/z01di159.w persistent set wh-pesquisa.
    
      if  not valid-handle(wh-pesquisa) or 
              wh-pesquisa:TYPE <> "PROCEDURE":U or 
              wh-pesquisa:FILE-NAME <> "dizoom/z01di159.w":U then
          return.
          
      /*run pi-seta-est in wh-pesquisa (input i-cod-emitente).*/
      
    
      RUN dispatch IN wh-pesquisa ('initialize':U).
      
      if valid-handle(wh-pesquisa) and
         wh-pesquisa:TYPE = "PROCEDURE":U and
         wh-pesquisa:FILE-NAME = "dizoom/z01di159.w":U then do:
        
            RUN pi-entry IN wh-pesquisa.
        
          
        
            
            define variable c-lista-campo as char init '' no-undo.
                
            assign c-lista-campo = string(c-nr-pedido-p:handle in frame f-pg-sel) + '|':U + 'nr-pedido'.
                 
                
            run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    
            RUN add-link IN adm-broker-hdl
                           (INPUT wh-pesquisa,
                            INPUT 'State':U,
                            INPUT this-procedure).
            
    
        
      end.
                        
    assign l-implanta = no.
    

    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel w-relat
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel


&Scoped-define FRAME-NAME f-relat
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 



/* ***************************  Main Block  *************************** */
i-cod-emitente:load-mouse-pointer("image/lupa.cur") in frame f-pg-sel.
c-nome-transp:load-mouse-pointer("image/lupa.cur") in frame f-pg-sel.
c-natur-oper:load-mouse-pointer("image/lupa.cur") in frame f-pg-sel.
c-nr-pedido-p:load-mouse-pointer("image/lupa.cur") in frame f-pg-sel.

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESFT4003NFT" "1.00.00.000"}

/*:T inicializa‡äes do template de relat¢rio */
{include/i-rpini.i}

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
   
     /* Code placed here will execute AFTER standard behavior.    */

      ASSIGN 
          c-serie-nf     = "20"
          c-cod-estabel-nf  = "{cdp\poloestab.i 434}"/*solic-318*/
          i-cod-emitente = 432 
          c-nome-transp = "" 
          c-natur-oper   = ""
          c-nome-abrev-nf   = ""
          
          d-emissao-nf   = TODAY.

    RUN enable_UI.

    h-botao = BT-TAMPA:HANDLE IN FRAME f-relat. .
   h-botao:MOVE-TO-TOP() .

   APPLY "entry" TO c-cod-estabel-nf IN FRAME f-pg-sel.

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

    DEFINE VARIABLE h-proc AS HANDLE     NO-UNDO.
    DEFINE VARIABLE h-next AS HANDLE     NO-UNDO.
    DEFINE VARIABLE c-prog AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE i-prog AS INTEGER    NO-UNDO.


        IF VALID-HANDLE(h-bodi317in) THEN
            IF h-bodi317in:PERSISTENT THEN DELETE PROCEDURE h-bodi317in.

        IF VALID-HANDLE(h-bodi317va) THEN
            IF h-bodi317va:PERSISTENT THEN DELETE PROCEDURE h-bodi317va.

        IF VALID-HANDLE(h-bodi317im1bra) THEN
            IF h-bodi317im1bra:PERSISTENT THEN DELETE PROCEDURE h-bodi317im1bra.

        IF VALID-HANDLE(h-bodi317sd) THEN
            IF h-bodi317sd:PERSISTENT THEN DELETE PROCEDURE h-bodi317sd.

        IF VALID-HANDLE(h-bodi317pr) THEN
            IF h-bodi317pr:PERSISTENT THEN DELETE PROCEDURE h-bodi317pr.

        IF VALID-HANDLE(h-bodi317ef) THEN
            IF h-bodi317ef:PERSISTENT THEN DELETE PROCEDURE h-bodi317ef.

        IF VALID-HANDLE(h-boin404te) THEN
            IF h-boin404te:PERSISTENT THEN DELETE PROCEDURE h-boin404te.

        FIND FIRST WT-DOCTO NO-LOCK NO-ERROR.
        RELEASE WT-DOCTO NO-ERROR.




     DO i-prog = 1 TO 4:
         h-next = SESSION:FIRST-PROCEDURE.



         DO WHILE VALID-HANDLE(h-next):

             h-proc = h-next.

             ASSIGN c-prog = ""
                    c-prog = h-proc:FILE-NAME NO-ERROR.


             IF c-prog MATCHES "*bodi*"  OR c-prog MATCHES "*bocx*" THEN DO:
                 h-next = h-next:NEXT-SIBLING.
                 IF valid-handle(h-proc) AND h-proc:PERSISTENT THEN DELETE PROCEDURE h-proc.
             END.
             ELSE h-next = h-next:NEXT-SIBLING.


         END.



     END.



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
  ENABLE bt-executar bt-cancelar bt-ajuda im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY c-cod-estabel-nf c-natur-oper  d-emissao-nf c-serie-nf 
          i-cod-emitente c-nome-abrev-nf ed-obs ed-pedidos  c-nome-transp
          c-nr-pedido-p i-nr-sequencia-p c-it-codigo-p
          
      WITH FRAME f-pg-sel IN WINDOW w-relat.
   ENABLE c-cod-estabel-nf c-natur-oper  d-emissao-nf c-serie-nf 
      c-nr-pedido-p i-nr-sequencia-p  /* c-it-codigo-p */
      i-cod-emitente ed-obs  bt-limpar bt-exclui bt-retira b1 ed-pedidos
      c-nome-transp
      
      WITH FRAME f-pg-sel IN WINDOW w-relat.

  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel} 

  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  VIEW w-relat.

  ed-pedidos:READ-ONLY = YES.
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
 

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-lote C-Win 
PROCEDURE pi-cria-lote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND first tt-lotes WHERE  
                 tt-lotes.cod-estabel     =   c-cod-estabel-nf:SCREEN-VALUE IN FRAME f-pg-sel  AND
                 tt-lotes.cod-emitente    =   int(i-cod-emitente:SCREEN-VALUE)                 AND
                 tt-lotes.nro-docto       =   string(tt-paletes.nr-pedido)                     AND
                 tt-lotes.sequencia       =   tt-paletes.nr-sequencia                          AND   
                 tt-lotes.it-codigo       =   tt-paletes.it-codigo                             AND   
                 tt-lotes.lote            =   c-lote                                           AND
                 tt-lotes.cod-refer       =   c-cod-refer                                      AND
                 tt-lotes.cod-localiz     =   c-cod-localiz                                    AND
                 tt-lotes.cod-depos       =   c-cod-depos                                      AND
                 tt-lotes.serie-docto     =   c-serie-nf:SCREEN-VALUE                          NO-ERROR.

    IF NOT AVAIL tt-lotes THEN DO:
        CREATE tt-lotes.
        ASSIGN
            tt-lotes.cod-estabel     =   c-cod-estabel-nf:SCREEN-VALUE IN FRAME f-pg-sel
            tt-lotes.cod-emitente    =   int(i-cod-emitente:SCREEN-VALUE)               
            tt-lotes.nro-docto       =   string(tt-paletes.nr-pedido                           )
            tt-lotes.sequencia       =   tt-paletes.nr-sequencia                        
            tt-lotes.it-codigo       =   tt-paletes.it-codigo                           
            tt-lotes.peso-bruto      =   tt-paletes.peso-bruto 
            tt-lotes.lote            =   c-lote                                         
            tt-lotes.cod-refer       =   c-cod-refer                                    
            tt-lotes.cod-localiz     =   c-cod-localiz                                  
            tt-lotes.cod-depos       =   c-cod-depos                                    
            tt-lotes.serie-docto     =   c-serie-nf:SCREEN-VALUE                        .
    END.

    FIND FIRST ITEM WHERE ITEM.it-codigo = tt-paletes.it-codigo NO-LOCK NO-ERROR.

           ASSIGN 
                    tt-lotes.un             =  ITEM.un
                    tt-lotes.quantidade     =  tt-lotes.quantidade + tt-paletes.quantidade                    
                    tt-lotes.dt-trans       =  date(d-emissao-nf:SCREEN-VALUE)                   
                    tt-lotes.dt-vali-lote   =  d-dt-vali-lote 
                    tt-lotes.nat-operacao   =  c-natur-oper:SCREEN-VALUE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-lote-it C-Win 
PROCEDURE pi-cria-lote-it :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    FIND first tt-lotes-it WHERE  
                 tt-lotes-it.cod-estabel     =   c-cod-estabel-nf:SCREEN-VALUE IN FRAME f-pg-sel  AND
                 tt-lotes-it.cod-emitente    =   int(i-cod-emitente:SCREEN-VALUE)                 AND
                 tt-lotes-it.nro-docto       =   string(tt-paletes.nr-pedido )                    AND
                 tt-lotes-it.sequencia       =   tt-paletes.nr-sequencia                          AND   
                 tt-lotes-it.it-codigo       =   tt-paletes.it-codigo                             AND   
                 tt-lotes-it.cod-refer       =   c-cod-refer                                      AND   
                /* tt-lotes-it.lote            =   c-lote                                           AND   */
                 tt-lotes-it.serie-docto     =   c-serie-nf:SCREEN-VALUE                           NO-ERROR.

    IF NOT AVAIL tt-lotes-it THEN DO:
        CREATE tt-lotes-it.
        ASSIGN
            tt-lotes-it.cod-estabel     =  c-cod-estabel-nf:SCREEN-VALUE IN FRAME f-pg-sel      
            tt-lotes-it.cod-emitente    =  int(i-cod-emitente:SCREEN-VALUE)                     
            tt-lotes-it.nro-docto       =  string(tt-paletes.nr-pedido )                                
            tt-lotes-it.sequencia       =  tt-paletes.nr-sequencia                               
            tt-lotes-it.it-codigo       =  tt-paletes.it-codigo                                 
            tt-lotes-it.cod-refer       =  c-cod-refer
            tt-lotes-it.lote            =  "" /*c-lote           */
            tt-lotes-it.serie-docto     =  c-serie-nf:SCREEN-VALUE                           .                                              
    END.                                                                             
                                           
    ASSIGN 
             tt-lotes-it.un             =  item.un                          
             tt-lotes-it.quantidade     =  tt-lotes-it.quantidade + tt-paletes.quantidade                    
             tt-lotes-it.dt-trans       =  date(d-emissao-nf:SCREEN-VALUE)                  
             tt-lotes-it.nat-operacao   =  c-natur-oper:SCREEN-VALUE.

    IF tt-paletes.vl-preuni > 0 THEN DO:

       ASSIGN tt-lotes-it.valor         =  tt-paletes.vl-preuni * tt-lotes-it.quantidade. 

    END.
    /*ELSE DO:

       FIND first item-estab WHERE  item-estab.it-codigo   =  tt-paletes.it-codigo AND
                                    item-estab.cod-estabel = c-cod-estabel-nf:SCREEN-VALUE IN FRAME f-pg-sel NO-LOCK NO-ERROR.

       IF NOT AVAIL ITEM-estab THEN  
             ASSIGN tt-lotes-it.valor = 0.
       ELSE
             ASSIGN tt-lotes-it.valor =  tt-lotes-it.valor + 
                                        (item-estab.val-unit-ggf-m[1] +
                                         item-estab.val-unit-mat-m[1] +
                                         item-estab.val-unit-mob-m[1])  .
    END.*/


          


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
DEFINE VARIABLE i-esp AS INTEGER    NO-UNDO.
do  on error undo, return error
    on stop  undo, return error:     

   /* {include/i-rpexa.i}*/

            
    /*:T Coloque aqui as valida‡äes das outras p ginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na p gina 
       com problemas e colocar o focus no campo com problemas             */    
         
    
    /*:T Coloque aqui a l¢gica de grava‡Æo dos parƒmtros e sele‡Æo na temp-table
       tt-param */ 

   /* {include/i-imexb.i}*/

    if  session:set-wait-state("general":U) then.

    /*{include/i-imrun.i xxp/xx9999rp.p}*/

    /*{include/i-imexc.i}*/

    /* logica principal do programa*/

     FOR EACH tt-lotes.
         DELETE tt-lotes.
     END.

     FOR EACH tt-lotes-it.
         DELETE tt-lotes-it.
     END.



      For Each tt-paletes NO-LOCK .
                                    
          ASSIGN 
                c-lote           = tt-paletes.lote
                c-cod-refer      = tt-paletes.cod-refer
                c-cod-localiz    = tt-paletes.cod-localiz 
                c-cod-depos      = tt-paletes.cod-depos
                d-quantidade     = tt-paletes.quantidade  
                d-dt-vali-lote   = tt-paletes.dt-vali-lote.
    
            RUN pi-cria-lote.
    
            RUN pi-cria-lote-it.
    
      End. /* for each*/
      
  RUN pi-gera-nota.
 








    if  session:set-wait-state("") then.
    
    /*{include/i-imtrm.i tt-param.arq-destino tt-param.destino}*/
    
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-cabeca C-Win 
PROCEDURE pi-gera-cabeca :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND emitente WHERE emitente.cod-emitente = int(i-cod-emitente:SCREEN-VALUE IN FRAME f-pg-sel) NO-LOCK NO-ERROR.

    IF NOT AVAIL emitente THEN
    DO:
      RUN utp/ut-msgs.p(INPUT "show",
                  INPUT 2,
                  INPUT ", emitente " + i-cod-emitente:SCREEN-VALUE IN FRAME f-pg-sel).
      RETURN "nok".
    END.
    
    ASSIGN c-nome-abrev = emitente.nome-abrev.
    
    
    
    /* Informa¯?es do embarque para c˜lculo */
    assign c-seg-usuario     = userid("mgadm")                /* Usu˜rio                    */
           c-cod-estabel     = c-cod-estabel-nf:SCREEN-VALUE IN FRAME f-pg-sel              /* Estabelecimento do pedido  */
           c-serie           = c-serie-nf:SCREEN-VALUE IN FRAME f-pg-sel                            /* Sýrie das notas            */
           c-nr-pedido       = ""                             /* Nr pedido do cliente       */
           da-dt-emis-nota   = date(d-emissao-nf:SCREEN-VALUE IN FRAME f-pg-sel)                     /* Data de emiss?o da nota    */
           c-cod-canal-venda = "9".
    
    
        /* Inicializa¯?o das BOS para C˜lculo */
        run dibo/bodi317in.p persistent set h-bodi317in.
        run inicializaBOS in h-bodi317in(output h-bodi317pr,
                                         output h-bodi317sd,     
                                         output h-bodi317im1bra,
                                         output h-bodi317va).
        RUN inbo/boin404te.p PERSISTENT SET h-boin404te.
        /* Inðcio da transa¯?o */
    
    
    
            /* Limpar a tabela de erros em todas as BOS */
            run emptyRowErrors        in h-bodi317in.
    
            run criaWtDocto in h-bodi317sd
                    (input  c-seg-usuario,
                     input  c-cod-estabel,
                     input  c-serie,
                     input  "1",
                     input  c-nome-abrev,
                     input  ?,
                     input  4,
                     input  4003,
                     input  da-dt-emis-nota,
                     input  0,
                     input  c-nat-operacao,
                     input  c-cod-canal-venda,
                     output i-seq-wt-docto,
                     output l-proc-ok-aux).
            /* Busca possðveis erros que ocorreram nas valida¯?es */
            run devolveErrosbodi317sd in h-bodi317sd(output c-ultimo-metodo-exec,
                                                     output table RowErrors).
    
            /* Pesquisa algum erro ou advert¬ncia que tenha ocorrido */
            find first RowErrors no-lock no-error.
    
            /* Caso tenha achado algum erro ou advert¬ncia, mostra em tela */
            if  avail RowErrors THEN DO:
               {method/ShowMessage.i1}
               {method/ShowMessage.i2 &Modal="yes"}
            END.
                
    
            /* Caso ocorreu problema nas valida¯?es, n?o continua o processo */
            if  not l-proc-ok-aux then
                RETURN "NOK".

                 
    
    
    /* Limpar a tabela de erros em todas as BOS */
    run emptyRowErrors        in h-bodi317in.
    
    ASSIGN i-seq = 0.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-nota C-Win 
PROCEDURE pi-gera-nota :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


  FIND FIRST para-fat NO-LOCK NO-ERROR.
  ASSIGN i-limite =  (para-fat.nr-item-nota - 1) * 10.
    
     
    
  ASSIGN c-seq-geradas = "".

  IF CAN-FIND(first tt-lotes-it) THEN DO:
 

    /* inicio bloco lotes comprados*/
    
    REPEAT TRANS:
         
        
       ASSIGN c-nat-operacao    = c-natur-oper:SCREEN-VALUE IN FRAME f-pg-sel.
       FIND first tt-lotes-it NO-LOCK NO-ERROR.

        IF AVAIL tt-lotes-it THEN 
             RUN pi-gera-cabeca. /* Gera cabe‡alho da NF  i-seq = 0*/
             ASSIGN 
                 d-peso-bru-tot = 0
                 d-peso-liq-tot = 0.
 
            if  RETURN-VALUE = "NOK" then
                            UNDO , leave.

            /* gera itens */
            FOR EACH tt-lotes-it  NO-LOCK.
                
                 do  i-cont-itens = 1 to 1:
                        assign c-it-codigo                   = tt-lotes-it.it-codigo  /* C¢digo do item     TEMP-TABLE ITEM PRODUZIDO*/
                               c-cod-refer                   = tt-lotes-it.cod-refer  /* Referˆncia do item TEMP-TABLE ITEM PRODUZIDO*/
                               de-quantidade                 = tt-lotes-it.quantidade /* Quantidade         TEMP-TABLE ITEM PRODUZIDO*/
                               de-vl-preori-ped              = tt-lotes-it.valor / tt-lotes-it.quantidade  /* Pre‡o unit rio  calculado pelo medio da entrada*/
                               de-val-pct-desconto-tab-preco = 0                    /* Desconto de tabela */
                               de-per-des-item               = 0.                   /* Desconto do item   */

                 IF  de-vl-preori-ped = 0 THEN DO:
                    RUN utp/ut-msgs.p(INPUT "show",
                                                INPUT 56,
                                                INPUT "Verifique o pre‡o unitario do item " +
                                                        c-it-codigo + "~~" + "Assumido R$ 1,00").
                              ASSIGN de-vl-preori-ped =  1.
                 END.
                 
                       
                
                        /* Limpar a tabela de erros em todas as BOS */
                        run emptyRowErrors        in h-bodi317in.
                    
                        /* Disponibilizar o registro WT-DOCTO na bodi317sd */
                        run localizaWtDocto in h-bodi317sd(input  i-seq-wt-docto,
                                                           output l-proc-ok-aux). 
                        ASSIGN i-seq = i-seq + 10.

                        /* Cria um item para nota fiscal. */
                        run criaWtItDocto in h-bodi317sd  (input  ?,
                                                           input  "",
                                                           input  i-seq,
                                                           input  c-it-codigo,
                                                           input  c-cod-refer,
                                                           input  ?,
                                                           output i-seq-wt-it-docto,
                                                           output l-proc-ok-aux).
                    
                        /* Busca poss¡veis erros que ocorreram nas valida‡äes */
                        run devolveErrosbodi317sd in h-bodi317sd(output c-ultimo-metodo-exec,
                                                                 output table RowErrors).
                    
                        /* Pesquisa algum erro ou advertˆncia que tenha ocorrido */
                        find first RowErrors no-lock no-error.
                        
                        /* Caso tenha achado algum erro ou advertˆncia, mostra em tela */
                        if  avail RowErrors THEN DO:
                            {method/ShowMessage.i1}
                            {method/ShowMessage.i2 &Modal="yes"}
                        END.
                             
                        
                  
                        /* Caso ocorreu problema nas valida‡äes, nÆo continua o processo */
                        if  not l-proc-ok-aux then
                            UNDO , leave.
                  /* MESSAGE 
                        c-it-codigo
                        i-seq-wt-docto SKIP                
                        i-seq-wt-it-docto SKIP             
                        de-quantidade SKIP                
                        de-vl-preori-ped SKIP              
                        de-val-pct-desconto-tab-preco SKIP 
                        de-per-des-item              



                        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

                        /* Grava informa‡äes gerais para o item da nota */
                        run gravaInfGeraisWtItDocto in h-bodi317sd 
                               (input i-seq-wt-docto,
                                input i-seq-wt-it-docto,
                                input de-quantidade,
                                input de-vl-preori-ped,
                                input de-val-pct-desconto-tab-preco,
                                input de-per-des-item).
                    
                        /* Limpar a tabela de erros em todas as BOS */
                        run emptyRowErrors        in h-bodi317in.
                    
                        /* Disp. registro WT-DOCTO, WT-IT-DOCTO e WT-IT-IMPOSTO na bodi317pr */
                        run localizaWtDocto       in h-bodi317pr(input  i-seq-wt-docto,
                                                                 output l-proc-ok-aux).
                        run localizaWtItDocto     in h-bodi317pr(input  i-seq-wt-docto,
                                                                 input  i-seq-wt-it-docto,
                                                                 output l-proc-ok-aux).
                        run localizaWtItImposto   in h-bodi317pr(input  i-seq-wt-docto,
                                                                 input  i-seq-wt-it-docto,
                                                                 output l-proc-ok-aux).
                    
                        /* Atualiza dados c lculados do item */
                        run atualizaDadosItemNota in h-bodi317pr(output l-proc-ok-aux).
                    
                        /* Busca poss¡veis erros que ocorreram nas valida‡äes */
                        run devolveErrosbodi317pr in h-bodi317pr(output c-ultimo-metodo-exec,
                                                                 output table RowErrors).
                    
                        /* Pesquisa algum erro ou advertˆncia que tenha ocorrido */
                        find first RowErrors no-lock no-error.
                        
                        /* Caso tenha achado algum erro ou advertˆncia, mostra em tela */
                        if  avail RowErrors THEN DO:
                            {method/ShowMessage.i1}
                            {method/ShowMessage.i2 &Modal="yes"}
                        END.
                             
                        /* Caso ocorreu problema nas valida‡äes, nÆo continua o processo */
                        if  not l-proc-ok-aux THEN NEXT.
                           /* UNDO , leave.*/
                    
                        /* Limpar a tabela de erros em todas as BOS */
                        run emptyRowErrors        in h-bodi317in.
                    
                        /* Valida informa‡äes do item */
                        run validaItemDaNota      in h-bodi317va(input  i-seq-wt-docto,
                                                                 input  i-seq-wt-it-docto,
                                                                 output l-proc-ok-aux).
                        /* Busca poss¡veis erros que ocorreram nas valida‡äes */
                        run devolveErrosbodi317va in h-bodi317va(output c-ultimo-metodo-exec,
                                                                 output table RowErrors).
                    
                        /* Pesquisa algum erro ou advertˆncia que tenha ocorrido */
                        find first RowErrors no-lock no-error.
                        
                        /* Caso tenha achado algum erro ou advertˆncia, mostra em tela */
                        if  avail RowErrors THEN DO:
                            {method/ShowMessage.i1}
                            {method/ShowMessage.i2 &Modal="yes"}
                        END.
                                                 
                        /* Caso ocorreu problema nas valida‡äes, nÆo continua o processo */
                        if  not l-proc-ok-aux then
                            UNDO , leave.
                 end.  /* do i-cont */
                 
                 if  not l-proc-ok-aux then
                            UNDO , leave.
                 
                 FOR EACH   wt-fat-ser-lote  WHERE 
                                          wt-fat-ser-lote.seq-wt-docto    =  i-seq-wt-docto AND
                                          wt-fat-ser-lote.it-codigo       =  tt-lotes-it.it-codigo  AND
                                          wt-fat-ser-lote.seq-wt-it-docto =  i-seq-wt-it-docto.

                        DELETE wt-fat-ser-lote.
        
                 END.
                 
                 FIND ITEM WHERE ITEM.it-codigo = tt-lotes-it.it-codigo NO-LOCK NO-ERROR.

                 FIND FIRST wt-it-docto WHERE wt-it-docto.seq-wt-docto    = i-seq-wt-docto and
                                              wt-it-docto.seq-wt-it-docto =  i-seq-wt-it-docto NO-ERROR.
                 IF AVAIL wt-it-docto THEN
                     ASSIGN wt-it-docto.peso-bru-it-inf = 0.
                  /*IF item.tipo-con-est >= 3 THEN DO:*/
                     FOR EACH tt-lotes WHERE
                              tt-lotes.cod-estabel      =    tt-lotes-it.cod-estabel   AND
                              tt-lotes.cod-emitente     =    tt-lotes-it.cod-emitente  AND
                              tt-lotes.nro-docto        =    tt-lotes-it.nro-docto     AND
                              tt-lotes.sequencia        =    tt-lotes-it.sequencia     AND
                              tt-lotes.it-codigo        =    tt-lotes-it.it-codigo     AND
                              tt-lotes.cod-refer        =    tt-lotes-it.cod-refer     AND
                           /*   tt-lotes.lote             =    tt-lotes-it.lote          AND*/
                              tt-lotes.serie-docto      =    tt-lotes-it.serie-docto   NO-LOCK.

                              CREATE wt-fat-ser-lote.
                                ASSIGN 
                                      wt-fat-ser-lote.char-1          = "Automatico"
                                      wt-fat-ser-lote.cod-depos       = tt-lotes.cod-depos       
                                      wt-fat-ser-lote.cod-localiz     = tt-lotes.cod-localiz    
                                      wt-fat-ser-lote.cod-refer       = tt-lotes.cod-refer      
                                      wt-fat-ser-lote.dt-vali-lote    = tt-lotes.dt-vali-lote   
                                      wt-fat-ser-lote.it-codigo       = tt-lotes.it-codigo       
                                      wt-fat-ser-lote.lote            = tt-lotes.lote           
                                      wt-fat-ser-lote.quantidade[1]   = tt-lotes.quantidade     
                                      wt-fat-ser-lote.qtd-contada[1]  = wt-fat-ser-lote.quantidade[1] 
                                      wt-fat-ser-lote.seq-wt-docto    = i-seq-wt-docto
                                      wt-fat-ser-lote.seq-wt-it-docto = i-seq-wt-it-docto.
                              
                              RELEASE wt-fat-ser-lote.

                              FIND wt-nota-embal WHERE wt-nota-embal.seq-wt-docto = i-seq-wt-docto AND
                                                       wt-nota-embal.sigla-emb =  "P04" NO-ERROR.
                              IF NOT AVAIL wt-nota-embal THEN DO:
                                  CREATE wt-nota-embal.
                                  ASSIGN 
                                      wt-nota-embal.seq-wt-docto = i-seq-wt-docto 
                                      wt-nota-embal.sigla-emb    =  "P04"
                                      wt-nota-embal.desc-vol     =  "PALETES"
                                      .
                              END.
                              ASSIGN wt-nota-embal.qt-volumes = wt-nota-embal.qt-volumes + 1
                                     d-peso-bru-tot           = d-peso-bru-tot + tt-lotes.peso-bruto
                                     d-peso-liq-tot           = d-peso-liq-tot + tt-lotes.quantidade.

                              RELEASE wt-nota-embal.

                              IF AVAIL wt-it-docto THEN
                                    ASSIGN wt-it-docto.peso-bru-it-inf =  wt-it-docto.peso-bru-it-inf + tt-lotes.peso-bruto
                                           wt-it-docto.quantidade[2]  = wt-it-docto.quantidade[1]
                                           wt-it-docto.un[2] = wt-it-docto.un[1].
                      END.
                      
                   /*END.*/
                 IF AVAIL wt-it-docto THEN
                    RELEASE wt-it-docto.
     
                 FIND FIRST wt-docto WHERE wt-docto.seq-wt-docto = i-seq-wt-docto NO-ERROR.

                 IF AVAIL wt-docto THEN 
                              ASSIGN wt-docto.observ-nota = ed-obs:SCREEN-VALUE IN FRAME f-pg-sel /* COLOCA OBSSERVA€ÇO DIGITADA NA TELA PARA TOA GERADA*/
                                     wt-docto.peso-bru-tot-inf = d-peso-bru-tot 
                                     wt-docto.peso-liq-tot-inf = d-peso-liq-tot 
                                     wt-docto.nome-transp  = c-nome-transp:screen-value.
                 RELEASE wt-docto.
                 FIND FIRST WT-DOCTO NO-LOCK NO-ERROR.
                 IF AVAIL WT-DOCTO THEN   RELEASE WT-DOCTO NO-ERROR.
             
                     /* wt-docto.peso-bru-tot-inf wt-docto.peso-liq-tot-inf*/


            END. /* for each tt-lotes-it */
           
            if  not l-proc-ok-aux THEN DO:
                RUN utp/ut-msgs.p(INPUT "show",
                          INPUT 15684,
                          INPUT ", na gera‡Æo de NF para Item: " + tt-lotes-it.it-codigo).
                undo, LEAVE.
            END.
                
    
            IF length(TRIM(c-seq-geradas)) > 1 THEN
                ASSIGN c-seq-geradas = c-seq-geradas + "," + STRING(i-seq-wt-docto).
            ELSE
                ASSIGN c-seq-geradas =  STRING(i-seq-wt-docto).
        
       LEAVE.                                                       
    END. /* fim do repeat trans */
      
  END. /* if can-find tt-lotes-it */
             
  FIND FIRST WT-DOCTO NO-LOCK NO-ERROR.
      IF AVAIL WT-DOCTO THEN   RELEASE WT-DOCTO NO-ERROR.

/* inicio da antiga rotina de mudar estabelecimento do pedido
 FOR EACH tt-pedidos NO-LOCK:

     ASSIGN tt-pedidos.qt-transf = 0
            tt-pedidos.qt-pedida = 0.
            
     FIND FIRST ped-venda WHERE 
                ped-venda.nr-pedido     = tt-pedidos.nr-pedido  NO-LOCK NO-ERROR.
                
     IF NOT AVAIL ped-venda THEN NEXT.


     FIND FIRST ped-item of ped-venda WHERE 
                ped-item.it-codigo     = tt-pedidos.it-codigo AND
                ped-item.nr-sequencia  = tt-pedidos.nr-sequencia NO-LOCK NO-ERROR.

     IF NOT AVAIL ped-item  THEN NEXT.

    
     
     FOR EACH pallet WHERE pallet.nr-pedido    = ped-venda.nr-pedido   AND
                           pallet.nr-sequencia = ped-item.nr-sequencia AND
                           pallet.it-codigo    = ped-item.it-codigo    and
                           pallet.cod-estabel  = c-cod-estabel-nf:SCREEN-VALUE IN FRAME f-pg-sel NO-LOCK USE-INDEX pedido.

         FOR EACH  movto-estoq WHERE
                           movto-estoq.it-codigo   = ped-item.it-codigo AND
                           movto-estoq.cod-refer   = ped-item.cod-refer AND
                           movto-estoq.esp-docto   = 23                 AND
                           movto-estoq.lote        = pallet.nr-pallet   AND
                           movto-estoq.tipo-trans  = 2                  and
                           movto-estoq.cod-estabel = c-cod-estabel-nf:SCREEN-VALUE IN FRAME f-pg-sel NO-LOCK .

             ASSIGN tt-pedidos.qt-transf = tt-pedidos.qt-transf + movto-estoq.quantidade.

         END.

         FOR EACH  movto-estoq WHERE
                           movto-estoq.it-codigo   = ped-item.it-codigo AND
                           movto-estoq.cod-refer   = ped-item.cod-refer AND
                           movto-estoq.esp-docto   = 22                 AND
                           movto-estoq.lote        = pallet.nr-pallet   AND
                           movto-estoq.tipo-trans  = 2                  and
                           movto-estoq.cod-estabel = c-cod-estabel-nf:SCREEN-VALUE IN FRAME f-pg-sel NO-LOCK .

             ASSIGN tt-pedidos.qt-transf = tt-pedidos.qt-transf + movto-estoq.quantidade.

         END.

     END.


    ASSIGN tt-pedidos.qt-pedida  = ped-item.qt-pedida.


 END.

 FOR EACH wt-fat-ser-lote NO-LOCK.
     FIND FIRST pallet WHERE 
         pallet.nr-pallet   =  wt-fat-ser-lote.lote AND
         pallet.it-codigo   =  wt-fat-ser-lote.it-codigo AND
         pallet.cod-estabel = c-cod-estabel-nf:SCREEN-VALUE IN FRAME f-pg-sel NO-LOCK NO-ERROR.
     IF NOT AVAIL pallet THEN  NEXT.

     FIND FIRST ped-venda WHERE ped-venda.nr-pedido = pallet.nr-pedido NO-LOCK NO-ERROR.

     IF NOT AVAIL ped-venda THEN NEXT.

     FIND FIRST tt-pedidos WHERE 
         tt-pedidos.nr-pedido    = ped-venda.nr-pedido AND
         tt-pedidos.it-codigo    = pallet.it-codigo    AND
         tt-pedidos.nr-sequencia = pallet.nr-sequencia NO-ERROR.
     
     IF AVAIL tt-pedidos THEN tt-pedidos.qt-transf = tt-pedidos.qt-transf + wt-fat-ser-lote.quantidade[1] .

 END.
  



 FOR EACH tt-pedidos NO-LOCK:

         FIND FIRST ped-venda WHERE ped-venda.nr-pedido = tt-pedidos.nr-pedido   NO-ERROR.
         
         IF not AVAIL ped-venda then next.
         
         
        find first  ped-item OF ped-venda WHERE
                 ped-item.ind-componen < 3 and
                 ped-item.nr-sequencia = tt-pedidos.nr-sequencia and
                 ped-item.tp-preco = 1
                no-lock no-error. 

         
         d-perc-atend = tt-pedidos.qt-transf / tt-pedidos.qt-pedida   * 100.
         
         IF avail  ped-item  THEN do: 
          output stream s-saida-nft to \\UNGUSB-VAP01\Sistemas\DTS\Log_Prd\esft4003nft.log append.
                        put stream s-saida-nft IF d-perc-atend < 90 then "nao Alterou " else "Vai alterar"  ";"
                             d-perc-atend ";"
                             tt-pedidos.qt-transf ";"
                             tt-pedidos.qt-pedida ";"
                             ped-venda.cod-estabel  ";" 
                             ped-venda.nat-operacao ";" 
                             ped-item.nat-operacao  ";" 
                             ped-item.vl-preuni     ";" 
                             ped-item.vl-preori     ";"  
                             ped-item.vl-pretab     ";"  
                             
                             
                             "" ";"
                             ped-venda.dt-entrega   ";" 
                             ped-item.dt-entrega  skip.
                       output stream s-saida-nft close. 
          end.
          
          
         IF d-perc-atend < 90  THEN NEXT.

       

        IF AVAIL ped-venda THEN DO:
        
     
        
            FOR EACH ped-item OF ped-venda WHERE
                 ped-item.ind-componen < 3 and
                 ped-item.nr-sequencia = tt-pedidos.nr-sequencia and
                 ped-item.tp-preco = 1
                 exclusive-lock .
        
               FIND FIRST b-estabelec WHERE
                   b-estabelec.cod-estabel = ped-venda.cod-estabel
                   NO-LOCK NO-ERROR.
        
               IF AVAIL b-estabelec THEN DO:
               
               
               
                  FIND FIRST pd-compl-pedido WHERE
                       pd-compl-pedido.ep-codigo            = b-estabelec.ep-codigo   AND
                       pd-compl-pedido.nr-pedido            = ped-venda.nr-pedido   AND
                       pd-compl-pedido.nr-sequencia         = ped-item.nr-sequencia AND
                       pd-compl-pedido.cod-estabel-fat      <> "" AND
                       pd-compl-pedido.nat-operacao-fat     <> "" AND
                       pd-compl-pedido.preco-venda-calc-fat <> 0  AND 
                       pd-compl-pedido.cod-estabel-fat      <> ped-venda.cod-estabel and
                       pd-compl-pedido.cod-estabel-prod     = c-cod-estabel-nf:SCREEN-VALUE
                      NO-LOCK NO-ERROR.
                  
                  IF AVAIL pd-compl-pedido THEN DO:
                  
                  
                  
                      ASSIGN ped-venda.cod-estabel  = pd-compl-pedido.cod-estabel-fat
                             ped-venda.nat-operacao = pd-compl-pedido.nat-operacao-fat
                             ped-item.nat-operacao  = pd-compl-pedido.nat-operacao-fat
                             ped-item.vl-preuni     = pd-compl-pedido.preco-venda-calc-fat
                             ped-item.vl-preori     = ped-item.vl-preuni
                             ped-item.vl-pretab     = ped-item.vl-preuni.
                             
                             if pd-compl-pedido.dt-faturamento <> ? then 
                             assign 
                             ped-venda.dt-entrega   = pd-compl-pedido.dt-faturamento
                             ped-item.dt-entrega    = pd-compl-pedido.dt-faturamento.
                             
                       
                       output stream s-saida-nft to \\UNGUSB-VAP01\Sistemas\DTS\Log_Prd\esft4003nft.log append.
                        put stream s-saida-nft "Alterou "  ";"
                             d-perc-atend ";"
                             tt-pedidos.qt-transf ";"
                             tt-pedidos.qt-pedida ";"
                             ped-venda.cod-estabel  ";" 
                             ped-venda.nat-operacao ";" 
                             ped-item.nat-operacao  ";" 
                             ped-item.vl-preuni     ";" 
                             ped-item.vl-preori     ";"  
                             ped-item.vl-pretab     ";"  
                             
                             
                             pd-compl-pedido.dt-faturamento ";"
                             ped-venda.dt-entrega   ";" 
                             ped-item.dt-entrega  skip.
                       output stream s-saida-nft close.      
                  
                      EMPTY TEMP-TABLE  tt-ped-venda.
        
                      CREATE tt-ped-venda.
                         BUFFER-COPY ped-venda TO tt-ped-venda.
                         ASSIGN tt-ped-venda.r-rowid = ROWID(ped-venda).
                         
                          ASSIGN 
                                 ped-venda.completo = no.


                         

                          IF NOT VALID-HANDLE(bo-ped-venda-com) THEN
                                   RUN dibo/bodi159com.p PERSISTENT SET bo-ped-venda-com.
                       
                         RUN setUserLog in bo-ped-venda-com (input "super").
                         RUN completeOrder in bo-ped-venda-com(input  ROWID(ped-venda),
                                                                 output table Rowerrors).
                          
                         
                         IF NOT CAN-FIND(FIRST RowErrors
                                     WHERE Rowerrors.ErrorSubType = "Error":U
                                       AND Rowerrors.ErrorType    = "EMS":U) THEN DO:
                       
                             ASSIGN 
                                 ped-venda.completo = yes.
                                 
                                 
                        
                         


                          
                          end.
                          
                          /* Totalizar o pedido de venda */
                          IF NOT VALID-HANDLE(bo-ped-venda-cal) THEN
                                   RUN dibo/bodi159cal.p PERSISTENT SET bo-ped-venda-cal.
                         run calculateOrder in bo-ped-venda-cal(input tt-ped-venda.r-rowid).
                       
                          
                           if valid-handle(bo-ped-venda-cal) then 
                          delete procedure bo-ped-venda-cal.
                          
                           IF VALID-HANDLE(bo-ped-venda-com) THEN
                            delete procedure bo-ped-venda-com.
        

                        
        
                  END.   /* AVAIL pd-compl-pedido */
        
               END.   /* AVAIL estabelec */
        
            END.   /* for each  ped-item */
        
        END.  /* AVAIL ped-venda */
        
  
END.

      fim da antiga rotina de mudar pedido de estabelecimento */

/* Fim do programa que calcula uma nota complementar */


END PROCEDURE.

PROCEDURE pi-mostra-paletes.    
    ASSIGN 
        c-nr-pedido-p:SCREEN-VALUE IN FRAME f-pg-sel = "" 
        ed-pedidos:SCREEN-VALUE = ""
        i-nr-sequencia-p:SCREEN-VALUE = "10"
        c-it-codigo-p:SCREEN-VALUE = "".

    FOR EACH tt-pedidos NO-LOCK:
        FIND FIRST ped-venda WHERE ped-venda.nr-pedido = tt-pedidos.nr-pedido NO-LOCK NO-ERROR.
        IF CAN-FIND(FIRST tt-paletes WHERE tt-paletes.nr-pedido = tt-pedidos.nr-pedido) THEN
        ASSIGN ed-pedidos:SCREEN-VALUE = string(ed-pedidos:SCREEN-VALUE) + string(tt-pedidos.nr-pedido) + "-" + ped-venda.nome-abrev + CHR(13).
        ELSE
            DELETE tt-pedidos.
    END.

    IF l-retira THEN
        OPEN QUERY q1 FOR EACH tt-paletes.

    ASSIGN l-retira = YES.

        /*b1:refresh() IN FRAME F-PG-sel.*/
      
END PROCEDURE.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*:T------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{include/i-rptrp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-relat  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-relat, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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


