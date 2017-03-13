
&Scoped-define WINDOW-NAME C-Win

/*------------------------------------------------------------------------
File.............: espd0061.w
Description......: espd0061 - Configura‡Æo de Pedido de Venda
Input Parameters : 
Output Parameters: 
Author...........: Damgra - Jos‚ Roberto
Created..........: 29/06/2011   
OBS..............: 
------------------------------------------------------------------------*/
DEFINE VARIABLE r-codigo-rwi  AS ROWID                     NO-UNDO.
DEFINE VARIABLE l-pd4000 AS LOGICAL     NO-UNDO.
define variable c-prog-gerado as character no-undo initial "espd0061".
define buffer if-ped-venda for espmulti.if-ped-venda.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

def buffer empresa for mgmulti.empresa.

CREATE WIDGET-POOL. 

/* ***************************  Definitions  ************************** */

DEFINE BUFFER bf-pd-config-pedido FOR pd-config-pedido. 
DEFINE BUFFER bcf-item FOR ITEM.
DEFINE BUFFER b-ped-venda FOR ped-venda.
DEFINE BUFFER b-ped-item FOR ped-item.
DEFINE BUFFER bf-ped-venda FOR ped-venda.

/* Preprocessadores do Template de Relat¢rio                            */

&GLOBAL-DEFINE FOLD1 f-fold-1 
&GLOBAL-DEFINE FOLD2 f-fold-2 
&GLOBAL-DEFINE FOLD3 f-fold-3 
&GLOBAL-DEFINE FOLD4 f-fold-4 

/* Include Com as Vari veis Globais */

DEFINE NEW GLOBAL SHARED VAR wh-ficodrefer      AS WIDGET-HANDLE.
DEFINE NEW GLOBAL SHARED VAR wh-ficodreferdesc  AS WIDGET-HANDLE.
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

/* Vari veis / Constantes utilizadas na configura‡Æo - cf0201b.w */

def temp-table tt-variavel no-undo
    field seq-mod        as   integer /*guarda a seq do modelo corrente*/                 
    FIELD seq-pai        AS   INTEGER /*guarda a seq do modelo pai*/
    field seq-var        as   integer                  
    field c-config       as   character format "x(1)"
    field nivel          as integer
    FIELD l-primeiro     AS   LOGICAL INITIAL NO
    field mo-codigo      like var-result.mo-codigo
    field nome-var       like var-result.nome-var
    field nome-cte       like var-result.nome-cte
    field sequencia-res  like var-result.sequencia
    field sequencia-var  like var-modelo.sequencia
    field descricao      like var-result.descricao
    field tipo-valor     like var-result.tipo-valor
    field valor-char     like var-result.valor-char
    field valor-dec      like var-result.valor-dec
    field tipo-result    like var-modelo.tipo-result 
    field c-tipo-result  like var-modelo.tipo-result 
    field abreviatura    like var-modelo.abreviatura
    field gera-narrativa like var-modelo.gera-narrativa
    field ind-tipo-var   like var-modelo.ind-tipo-var
    field nr-tabela      like var-modelo.nr-tabela
    field endereco       as rowid
    index codigo is primary unique seq-var
    index nivel nivel
    index modelo mo-codigo nivel seq-mod tipo-result
    index nome nome-var nivel mo-codigo seq-var.


DEFINE TEMP-TABLE tt-var-form NO-UNDO                                     
       FIELD nome-var    LIKE var-modelo.nome-var   
       FIELD sequencia   LIKE var-modelo.sequencia  
       FIELD tipo-result LIKE var-modelo.tipo-result
       FIELD formula     LIKE var-mod-form.formula. 

/******************************************************************************
 ** 
 **  INCLUDE  : CDAPI020A.I 
 **
 **  OBJETIVO : Definir a temp-table da API do Interpretador de F¢rmulas
 **
 ******************************************************************************/
 
 def temp-table tt-formula no-undo
     field formula               as char
     field result                as decimal format ">>>>,>>9.9999"
     field texto                 as char    format "x(256)"
     field tipo-res              as integer format "9" init 1 /* 1-Decimal, 2-Caracter */
     field cod-versao-integracao as integer format "999"
     field sequencia             as integer init 1
     index codigo is primary unique sequencia.
      
 def temp-table tt-var no-undo         
     field nome              as character         
     field valor             as decimal format ">>>,>>>,>>9.999999999"         
     field texto             as char    format "x(60)"         
     field nr-tabela         as integer format ">>9"         
     field tipo-res          as integer format "9"         
     field abrev-nar         as char    format "x(29)"
     field sequencia         as integer init 1
     index nome is primary nome sequencia. 

 def  temp-table tt-erro no-undo
     field i-sequen as int             
     field cd-erro  as int
     field mensagem as char format "x(255)".


/*******************************************************************************/

/* Transfer Definitions */
                   

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

def var h-acomp            as handle no-undo.
def var v-num-reg-lidos    as int    no-undo.




/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 


/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

/* Chave unica do registro */
DEFINE VARIABLE i-ep-codigo-jr       AS CHAR                                NO-UNDO.
DEFINE VARIABLE i-nr-pedido-jr       AS INT                                 NO-UNDO.
DEFINE VARIABLE i-nr-sequencia-jr    AS INT                                 NO-UNDO.
/*-------------------------*/                                               
                                                                            
DEFINE VARIABLE c-nome-empresa       AS CHARACTER FORMAT "x(50)"            NO-UNDO.
DEFINE VARIABLE c-nome-abrev         AS CHARACTER FORMAT "x(12)"            NO-UNDO.
DEFINE VARIABLE c-it-codigo          AS CHARACTER FORMAT "x(16)"            NO-UNDO.
DEFINE VARIABLE c-cod-estabel        AS CHARACTER                           NO-UNDO.
DEFINE VARIABLE i-nr-ord-produ       AS INTEGER                            NO-UNDO.
                                                                            
DEFINE VARIABLE c-mensagem-jr        AS CHARACTER FORMAT "X(60)"            NO-UNDO.
DEFINE VARIABLE c-tipo-botao         AS CHARACTER                           NO-UNDO.
DEFINE VARIABLE c-tipo-botao2        AS CHARACTER                           NO-UNDO.

DEFINE VARIABLE peso-bob-jr          AS DECIMAL   FORMAT "->>>>>9.999999"   NO-UNDO.
DEFINE VARIABLE diex-jr              AS INTEGER                             NO-UNDO.
DEFINE VARIABLE diin-jr              AS INTEGER                             NO-UNDO.
DEFINE VARIABLE larg-jr              AS INTEGER                             NO-UNDO.
DEFINE VARIABLE qtd-pedida-jr        AS DECIMAL   FORMAT "->>>>>>>>>9.99"   NO-UNDO.
DEFINE VARIABLE qtdbob-jr            AS INTEGER                             NO-UNDO.
DEFINE VARIABLE dens-bob             AS DECIMAL                             NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */ 

def new global shared var rw-nr-pedido-0061 as row       no-undo.
DEFINE VARIABLE i-nr-estrut-criada        AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-nr-pedido-config        AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-nr-sequencia-config     AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-embal-jr                AS CHARACTER  FORMAT "x(6)"       NO-UNDO.
DEFINE VARIABLE i-qt-bob-plt              AS INTEGER    FORMAT ">>>>>9"     NO-UNDO.
DEFINE VARIABLE i-qt-plt                  AS INTEGER    FORMAT ">>>>>9"     NO-UNDO.


DEFINE VARIABLE i-nr-pedido-final      AS INTEGER    FORMAT ">>>>>>>>9" NO-UNDO.
DEFINE VARIABLE c-nome-abrev-final     AS CHARACTER  FORMAT "x(12)"     NO-UNDO.
DEFINE VARIABLE c-nr-pedcli-final      AS CHARACTER  FORMAT "x(12)"     NO-UNDO.
DEFINE VARIABLE c-cod-estabel-final    AS CHARACTER  FORMAT "x(03)"     NO-UNDO.

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
   
DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY 2.


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
    
DEFINE IMAGE im-fold-4
    FILENAME "image\im-fldup"
    SIZE 15.72 BY 1.19.
    
DEFINE IMAGE im-fold-2
    FILENAME "image\im-fldup"
    SIZE 15.72 BY 1.19.
    
DEFINE IMAGE im-fold-1
    FILENAME "image\im-fldup"
    SIZE 15.72 BY 1.19.

DEFINE IMAGE im-fold-3
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

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.83.

DEFINE RECTANGLE RECT-7 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL
SIZE 109 BY 3.42 
BGCOLOR 7.

DEFINE RECTANGLE RECT-10 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 46.29 BY 3.50.

DEFINE RECTANGLE RECT-22
         EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
         SIZE 15 BY 2.3.

DEFINE RECTANGLE RECT-20 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 106 BY 13.5.

DEFINE VARIABLE l-param-1 AS LOGICAL INITIAL no 
LABEL "Parƒmetro 1"
VIEW-AS TOGGLE-BOX 
SIZE 44 BY 1.08 NO-UNDO. 


DEFINE BUTTON bt-primeiro
    IMAGE FILENAME "image\im-fir"
    SIZE 4 BY 1.

DEFINE BUTTON bt-anterior
    IMAGE FILENAME "image\im-pre"
    SIZE 4 BY 1.

DEFINE BUTTON bt-proximo
    IMAGE FILENAME "image\im-nex"
    SIZE 4 BY 1.

DEFINE BUTTON bt-final
    IMAGE FILENAME "image\im-las"
    SIZE 4 BY 1.

DEFINE BUTTON bt-goto
    IMAGE FILENAME "image\im-vapra"
    SIZE 4 BY 1.

DEFINE BUTTON bt-pesquisa
    IMAGE FILENAME "image\im-zoom"
    SIZE 4 BY 1.

DEFINE BUTTON bt-novo
    IMAGE FILENAME "image\im-add"
    SIZE 4 BY 1.

DEFINE BUTTON bt-copia
    IMAGE FILENAME "image\im-copy"
    SIZE 4 BY 1.

DEFINE BUTTON bt-altera
    IMAGE FILENAME "image\im-mod"
    SIZE 4 BY 1.

DEFINE BUTTON bt-deleta
    IMAGE FILENAME "image\im-era"
    SIZE 4 BY 1.

DEFINE BUTTON bt-cancela
    IMAGE FILENAME "image\im-can"
    SIZE 4 BY 1.

DEFINE BUTTON bt-grava
    IMAGE FILENAME "image\im-chck1"
    SIZE 4 BY 1.

DEFINE BUTTON bt-sai
    IMAGE FILENAME "image\im-exi"
    SIZE 4 BY 1.


DEFINE BUTTON bt-ajuda 
    IMAGE FILENAME "image\im-hel"
    SIZE 4 BY 1.

DEFINE RECTANGLE RECT-1
EDGE-PIXELS 2 GRAPHIC-EDGE 
SIZE 109 BY 1.42 
BGCOLOR 7.

DEFINE RECTANGLE RECT-6
EDGE-PIXELS 0
SIZE 108 BY .12
BGCOLOR 7.

DEFINE RECTANGLE rt-folder
EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL
SIZE 109 BY 14.3
FGCOLOR 0.

DEFINE RECTANGLE rt-folder-left
EDGE-PIXELS 0
SIZE .43 BY 13.9
BGCOLOR 15.

DEFINE RECTANGLE rt-folder-right
EDGE-PIXELS 0
SIZE .43 BY 13.9
BGCOLOR 7.

DEFINE RECTANGLE rt-folder-top
EDGE-PIXELS 0
SIZE 108 BY .13
BGCOLOR 15 .

DEFINE VARIABLE c-texto-jr AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 3000 SCROLLBAR-VERTICAL
     SIZE 53 BY 3 NO-UNDO.


DEFINE VARIABLE c-narrativa-jr AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 3000 SCROLLBAR-VERTICAL
     SIZE 53 BY 3 NO-UNDO.


DEFINE BUTTON bt-integra-ems 
     LABEL "Integra EMS" 
     SIZE 18 BY 1.5.



/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-tela

    bt-primeiro AT ROW 1.25 COL 4    HELP "In¡cio do Arquivo"
    bt-anterior AT ROW 1.25 COL 8.5  HELP "Registro Anterior"
    bt-proximo  AT ROW 1.25 COL 13   HELP "Pr¢ximo Registro"
    bt-final    AT ROW 1.25 COL 17.5 HELP "Final do Arquivo"
    bt-goto     AT ROW 1.25 COL 22   HELP "V  Para"
    bt-pesquisa AT ROW 1.25 COL 26.5 HELP "Pesquisa"
    bt-novo     AT ROW 1.25 COL 31   HELP "Inclui Novo Registro"
    bt-copia    AT ROW 1.25 COL 35.5 HELP "Copia Registro"
    bt-altera   AT ROW 1.25 COL 40   HELP "Altera Registro"
    bt-deleta   AT ROW 1.25 COL 44.5 HELP "Exclui Registro"
    bt-cancela  AT ROW 1.25 COL 49   HELP "Cancela Atualiza‡Æo"
    bt-grava    AT ROW 1.25 COL 53.5 HELP "Grava Registro"
    bt-sai      AT ROW 1.25 COL 100.5   HELP "Encerra Programa"
    bt-ajuda    AT ROW 1.25 COL 105.5 HELP "Ajuda"

/* Colocar aqui os campos chaves do registro */
   
    pd-config-pedido.ep-codigo label "Empresa"
      at row 2.7 col 20 colon-aligned
      view-as fill-in 
      size 10 by .88
      font 1

    c-nome-empresa NO-LABEL 
      at row 2.7 col 38 colon-aligned
      view-as fill-in 
      size 40 by .88
      font 1

    pd-config-pedido.nr-pedido label "Nr.Pedido"
      at row 3.7 col 20 colon-aligned
      view-as fill-in 
      size 10 by .88
      font 1

    c-nome-abrev LABEL "Cliente" 
      at row 3.7 col 38 colon-aligned
      view-as fill-in 
      size 40 by .88
      font 1

    pd-config-pedido.nr-sequencia label "Seq.Pedido"
      at row 4.7 col 20 colon-aligned
      view-as fill-in 
      size 10 by .88
      font 1
    
    c-it-codigo LABEL "Item" 
      at row 4.7 col 38 colon-aligned
      view-as fill-in 
      size 40 by .88
      font 1

    pd-config-pedido.nr-config label "Nr.Configura‡Æo"
      at row 2.7 col 95 colon-aligned
      view-as fill-in 
      size 12 by .88
      font 1

    i-nr-ord-produ LABEL "Ord. Produ‡Æo" 
      at row 3.7 col 95 colon-aligned
      view-as fill-in 
      size 12 by .88
      font 1

    c-cod-estabel LABEL "Estab.Atual" 
      at row 4.7 col 95 colon-aligned
      view-as fill-in 
      size 12 by .88
      font 1

/* ----------------------------------------- */

     im-fold-1 AT ROW 5.5 COL 2.14
     im-fold-2 AT ROW 5.5 COL 17.86 
     im-fold-3 AT ROW 5.5 COL 33.57
     im-fold-4 AT ROW 5.5 COL 49.29 
     rt-folder AT ROW 7.5 COL 2
     rt-folder-top AT ROW 7.54 COL 2.14
     rt-folder-left AT ROW 7.54 COL 2.14
     rt-folder-right AT ROW 7.65 COL 110 
     RECT-1 AT ROW 1.05 COL 2    
     RECT-9 AT ROW 2.55 COL 2   
     RECT-6 AT ROW 21.6 COL 2.25
     WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
     SIDE-LABELS NO-UNDERLINE THREE-D
     AT COL 1 ROW 1 FONT 1
     SIZE 110 BY 21. 

DEFINE FRAME f-fold-1  

    rect-20 AT ROW 1 COL 1.3
    
    pd-config-pedido.largura label "Largura"
         AT ROW 1.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 20 BY .88

    pd-config-pedido.diin label "Diƒm.Int."
         AT ROW 2.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 20 BY .88
    
    pd-config-pedido.diex label "Diƒm.Ext."
         AT ROW 3.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 20 BY .88
    
    pd-config-pedido.qtde-pedida-cliente label "Qtd.Ped.Cliente"
         AT ROW 4.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 20 BY .88
    
    pd-config-pedido.cod-prod-cliente label "Cod.Prod.Cliente"
         AT ROW 5.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 20 BY .88
    
    pd-config-pedido.nr-pedido-cliente label "Nr.Pedido Cliente"
         AT ROW 6.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 20 BY .88
    
    pd-config-pedido.dec-1 label "Peso da Bobina"
         AT ROW 7.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 20 BY .88
    
    pd-config-pedido.qtde-ajustada-ped label "Qtd.Ajustada no Pedido"
         AT ROW 8.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 20 BY .88
    
    pd-config-pedido.qtde-bobinas label "Qtd.de Bobinas"
         AT ROW 9.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 20 BY .88

    pd-config-pedido.dens-otica label "Densidade àtica"
         AT ROW 10.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 20 BY .88

    pd-config-pedido.cod-aplicacao label "C¢d.Aplica‡Æo"
         AT ROW 11.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 40 BY .88

    pd-config-pedido.unid-fat-cliente label "Unid.Faturada Cliente"
         AT ROW 12.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 20 BY .88
    
    pd-config-pedido.preco-unit label "Pre‡o Unit rio"
         AT ROW 13.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 20 BY .88
    
    

    i-qt-bob-plt label "Bobinas p/Pallet"
         AT ROW 1.3 COL 58 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 12 BY .88

    i-qt-plt label "Qtde. de Pallets"
         AT ROW 2.3 COL 58 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 12 BY .88
    
    pd-config-pedido.integrado-ems AT ROW 6 COL 55 
      

    bt-integra-ems AT ROW 9.54 COL 65 HELP
          "Integra a Configura‡Æo Atual no Datasul EMS"

    rect-19 AT ROW 5.4 COL 52

    i-nr-pedido-final label "Nr.Pedido Final"
         AT ROW 1.3 COL 85 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 15 BY .88

    c-nome-abrev-final label "Cliente Final"
         AT ROW 2.3 COL 85 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 15 BY .88

    c-nr-pedcli-final label "Nr.Pedcli"
         AT ROW 3.3 COL 85 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 15 BY .88

    c-cod-estabel-final label "Estabelec.Final"
         AT ROW 4.3 COL 85 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 15 BY .88

   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D
   AT COL 3 ROW 7.85 FONT 1
   SIZE 107 BY 13.7.

DEFINE FRAME f-fold-2

   pd-config-pedido.obs-qualidade  LABEL "Obs.Qualidade"
     at row 2.3  col 18 COLON-ALIGNED
     VIEW-AS editor 
     SIZE 60 BY 2

    pd-config-pedido.obs-gerais LABEL "Obs.Gerais"
      at row 5.3  col 18 COLON-ALIGNED
      VIEW-AS editor 
      SIZE 60 BY 2

    pd-config-pedido.obs-etiqueta LABEL "Obs.Etiqueta"
      at row 8.3  col 18 COLON-ALIGNED
      VIEW-AS editor 
      SIZE 60 BY 2

    pd-config-pedido.obs-embalagem LABEL "Obs.Embalagem"
      at row 11.3  col 18 COLON-ALIGNED
      VIEW-AS editor 
      SIZE 60 BY 2

   rect-20 AT ROW 1 COL 1.3
      
   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D
   AT COL 3 ROW 7.85 FONT 1
   SIZE 107 BY 13.7.

DEFINE FRAME f-fold-3  
     
   rect-20 AT ROW 1 COL 1.3

   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D
   AT COL 3 ROW 7.85 FONT 1
   SIZE 107 BY 13.7.

DEFINE FRAME f-fold-4

   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D
   AT COL 3 ROW 7.85 FONT 1
   SIZE 107 BY 13.7.


/* ******** Acerto da posi‡Æo dos labels e tamanho dos radio-set ******* */

DEFINE VARIABLE h-label AS WIDGET-HANDLE NO-UNDO.

/* *************************  Create Window  ************************** */

CREATE WINDOW C-Win ASSIGN
   HIDDEN             = YES
   TITLE              = "Configura‡Æo de Pedido de Venda"
   HEIGHT             = 21
   WIDTH              = 112
   MAX-HEIGHT         = 21
   MAX-WIDTH          = 112
   VIRTUAL-HEIGHT     = 21
   VIRTUAL-WIDTH      = 112
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

ASSIGN FRAME f-fold-4:FRAME = FRAME f-tela:HANDLE
       FRAME f-fold-2:FRAME = FRAME f-tela:HANDLE
       FRAME f-fold-3:FRAME = FRAME f-tela:HANDLE
       FRAME f-fold-1:FRAME = FRAME f-tela:HANDLE.

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


PROCEDURE pi-trata-state :
  DEFINE INPUT PARAMETER p-issuer-hdl   AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state        AS CHAR   NO-UNDO.
  
  case entry(1, p-state, '|'):
  end.
END PROCEDURE.

on "close" of this-procedure do:
  /*  run btb/btb918za.p (input no). */
    run disable_UI.
end.

/* ************************  Control Triggers  ************************ */

ON ENTRY OF pd-config-pedido.ep-codigo IN FRAME f-tela /* Empresa */
DO:

    IF int(pd-config-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela) = 0 THEN DO:

       FIND FIRST empresa NO-LOCK NO-ERROR.
       
       IF AVAIL empresa THEN
          ASSIGN pd-config-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela = STRING(empresa.ep-codigo)
                 c-nome-empresa:SCREEN-VALUE IN FRAME f-tela = empresa.nome.
       ELSE
          ASSIGN c-nome-empresa:SCREEN-VALUE IN FRAME f-tela = "".

    END.

    DISABLE bt-integra-ems WITH FRAME f-fold-1.
    
END.


ON ENTRY OF pd-config-pedido.largura IN FRAME f-fold-1 /* Entry na Largura */
DO:

   

END.

on LEAVE OF pd-config-pedido.ep-codigo in frame f-tela do:

   FIND FIRST empresa WHERE
       empresa.ep-codigo = STRING(pd-config-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela)
       NO-LOCK NO-ERROR.
   
   IF AVAIL empresa THEN
      ASSIGN c-nome-empresa:SCREEN-VALUE IN FRAME f-tela = empresa.nome.
   ELSE
      ASSIGN c-nome-empresa:SCREEN-VALUE IN FRAME f-tela = "".

end.    


on LEAVE OF pd-config-pedido.nr-pedido in frame f-tela do:

   FIND FIRST ped-venda WHERE
       ped-venda.nr-pedido = int(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)
       NO-LOCK NO-ERROR.
   
   IF AVAIL ped-venda THEN
      ASSIGN c-nome-abrev:SCREEN-VALUE IN FRAME f-tela  = ped-venda.nome-abrev
             c-cod-estabel:SCREEN-VALUE IN FRAME f-tela = ped-venda.cod-estabel.
   ELSE
      ASSIGN c-nome-abrev:SCREEN-VALUE IN FRAME f-tela  = ""
             c-cod-estabel:SCREEN-VALUE IN FRAME f-tela = "".

end.    


on LEAVE OF pd-config-pedido.nr-sequencia in frame f-tela do:

   FIND FIRST ped-venda WHERE
        ped-venda.nr-pedido    = int(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)  NO-LOCK NO-ERROR.
     IF AVAIL ped-venda THEN
     FIND FIRST ped-item OF ped-venda WHERE
             ped-item.nr-sequencia = int(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)      AND
             ped-item.ind-componen < 3 NO-LOCK NO-ERROR.

   IF AVAIL ped-item THEN
      ASSIGN c-it-codigo:SCREEN-VALUE IN FRAME f-tela  = ped-item.it-codigo
             pd-config-pedido.nr-config:SCREEN-VALUE IN FRAME f-tela = string(ped-item.cod-refer).
   ELSE
      ASSIGN c-it-codigo:SCREEN-VALUE IN FRAME f-tela  = ""
             pd-config-pedido.nr-config:SCREEN-VALUE IN FRAME f-tela = "".

      IF NOT AVAIL ped-item OR int(ped-item.cod-refer) = 1 THEN 
          ASSIGN pd-config-pedido.nr-config:SCREEN-VALUE IN FRAME f-tela = "".

end.    

ON MOUSE-SELECT-DBLCLICK OF pd-config-pedido.cod-aplicacao in frame f-fold-1 do: /* Aplica‡Æo */

   APPLY "f5" TO SELF.

END.

ON F5 OF pd-config-pedido.cod-aplicacao in frame f-fold-1 do: /* Aplica‡Æo */

    RUN pi-pesquisa-aplic.
END.

on LEAVE OF pd-config-pedido.largura in frame f-fold-1 do:

    IF pd-config-pedido.cod-prod-clie:SCREEN-VALUE IN FRAME f-fold-1 = "" THEN DO:

        FIND FIRST emitente WHERE
            emitente.nome-abrev = c-nome-abrev:SCREEN-VALUE IN FRAME f-tela
            NO-LOCK NO-ERROR.

        IF AVAIL emitente THEN DO:

            FIND FIRST am-pd-prod-cliente WHERE
                am-pd-prod-cliente.cod-emitente = emitente.cod-emitente                         AND
                am-pd-prod-cliente.it-codigo    = c-it-codigo:SCREEN-VALUE IN FRAME f-tela      AND
                am-pd-prod-cliente.largura      = INPUT FRAME f-fold-1 pd-config-pedido.largura
                NO-LOCK NO-ERROR.

            IF AVAIL am-pd-prod-cliente THEN
                ASSIGN pd-config-pedido.cod-prod-clie:SCREEN-VALUE IN FRAME f-fold-1 = am-pd-prod-cliente.cod-prod-cliente.


        END.

    END.

    RUN pi-calculo-formulas.

END.

on LEAVE OF pd-config-pedido.diin in frame f-fold-1 do:

    IF dec(trim(pd-config-pedido.diin:SCREEN-VALUE IN FRAME f-fold-1)) = 0  THEN DO:
      ASSIGN c-mensagem-jr = "Informe o diametro Interno.".
      RUN pi-mostra-mensagem.

      APPLY "entry" TO pd-config-pedido.diin IN FRAME f-fold-1.

      RETURN NO-APPLY.
    END.

    RUN pi-calculo-formulas.

END.

on LEAVE OF pd-config-pedido.diex in frame f-fold-1 do:

    IF DEC(pd-config-pedido.diex:SCREEN-VALUE in frame f-fold-1) <  dec(pd-config-pedido.diin:SCREEN-VALUE in frame f-fold-1) THEN DO:

        ASSIGN c-mensagem-jr = "Diametro externo menor que Diametro interno.".
        RUN pi-mostra-mensagem.

        RETURN NO-APPLY.
    END.

     IF dec(trim(pd-config-pedido.diex:SCREEN-VALUE IN FRAME f-fold-1)) = 0  THEN DO:
      ASSIGN c-mensagem-jr = "Informe o diametro externo.".
      RUN pi-mostra-mensagem.

      APPLY "entry" TO pd-config-pedido.diex IN FRAME f-fold-1.

      RETURN NO-APPLY.
    END.
       
    RUN pi-calculo-formulas.

END.

on LEAVE OF pd-config-pedido.qtde-pedida-cliente in frame f-fold-1 do:

    RUN pi-calculo-formulas.

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

ON ENDKEY OF FRAME f-tela DO:
  return no-apply.
END.


ON CHOOSE OF bt-primeiro IN FRAME f-tela
DO:

   RUN pi-le-primeiro.

END.


ON CHOOSE OF bt-proximo IN FRAME f-tela
DO:

   RUN pi-le-proximo.

END.


ON CHOOSE OF bt-anterior IN FRAME f-tela
DO:

   RUN pi-le-anterior.

END.


ON CHOOSE OF bt-final IN FRAME f-tela
DO:

   RUN pi-le-ultimo.

END.


ON CHOOSE OF bt-integra-ems IN FRAME f-fold-1
DO:

if  pd-config-pedido.integrado-ems:checked IN FRAME f-fold-1 then return no-apply.


   FIND FIRST ped-venda WHERE
       ped-venda.nr-pedido = int(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)
       NO-LOCK NO-ERROR.

   IF NOT AVAIL ped-venda THEN DO:

      ASSIGN c-mensagem-jr = "Pedido de Venda NÆo Existe".
      RUN pi-mostra-mensagem.

      APPLY "entry" TO pd-config-pedido.nr-pedido IN FRAME f-tela.

      RETURN NO-APPLY.

   END.

   FIND FIRST ped-item OF ped-venda WHERE
       ped-item.nr-sequencia = int(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela) AND
       ped-item.ind-componen < 3
       NO-LOCK NO-ERROR.

   IF NOT AVAIL ped-item THEN DO:

       ASSIGN c-mensagem-jr = "Item do Pedido de Venda NÆo Existe".
       RUN pi-mostra-mensagem.

       APPLY "entry" TO pd-config-pedido.nr-sequencia IN FRAME f-tela.

       RETURN NO-APPLY.

   END.

   ASSIGN i-nr-pedido-config    = ped-venda.nr-pedido
          i-nr-sequencia-config = ped-item.nr-sequencia
          i-nr-estrut-criada    = 0.

   RUN pdp/espd0061-1.w(input  i-nr-pedido-config,
                   INPUT  i-nr-sequencia-config,
                   OUTPUT i-nr-estrut-criada).

    for each ped-item where ped-item.nr-config = 1 and   /* acerta nr-config se algum pedido perdido*/
                            int (ped-item.cod-refer) > 1.
      assign ped-item.nr-config  = int (ped-item.cod-refer).
    end.
    
   FIND FIRST ped-venda WHERE
       ped-venda.nr-pedido = int(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)
       NO-LOCK NO-ERROR.

   IF NOT AVAIL ped-venda THEN RETURN.

   FIND FIRST ped-item OF ped-venda WHERE
       ped-item.nr-sequencia = int(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela) AND
       ped-item.ind-componen < 3
       NO-LOCK NO-ERROR.

   IF NOT AVAIL ped-item THEN RETURN.

   ASSIGN pd-config-pedido.nr-config:SCREEN-VALUE IN FRAME f-tela = string(int(ped-item.cod-refer),"99999999").
   
   if valid-handle(wh-ficodrefer) then  
        wh-ficodrefer:screen-value = string(int(ped-item.cod-refer),"99999999").
   if valid-handle(wh-ficodreferdesc) then  
        wh-ficodreferdesc:screen-value = "Pd:" + ped-item.nr-pedcli + "/" + ped-item.nome-abrev + ";" + "It.:"+ ped-item.it-codigo.
          


   IF int(ped-item.cod-refer) <> 0 AND int(ped-item.cod-refer) <> 1 THEN DO:   /* Item j  configurado */
         c-embal-jr = "".
      
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "CODEMBAL"           AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN  c-embal-jr = var-result.valor-char.

 

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "Largura"           AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN c-embal-jr =  (if var-result.valor-dec  = dec(pd-config-pedido.Largura:SCREEN-VALUE IN FRAME f-fold-1) then c-embal-jr else  "")
                    var-result.valor-dec  = dec(pd-config-pedido.Largura:SCREEN-VALUE IN FRAME f-fold-1) 
                    var-result.des-result = string(pd-config-pedido.Largura:SCREEN-VALUE IN FRAME f-fold-1).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "diin"              AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN c-embal-jr =  (if var-result.valor-dec  = dec(pd-config-pedido.diin:SCREEN-VALUE IN FRAME f-fold-1)  then c-embal-jr else  "")
                    var-result.valor-dec  = dec(pd-config-pedido.diin:SCREEN-VALUE IN FRAME f-fold-1) 
                    var-result.des-result = string(pd-config-pedido.diin:SCREEN-VALUE IN FRAME f-fold-1).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "diex"              AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN c-embal-jr =  (if var-result.valor-dec  = dec(pd-config-pedido.diex:SCREEN-VALUE IN FRAME f-fold-1)  then c-embal-jr else  "")
                    var-result.valor-dec  = dec(pd-config-pedido.diex:SCREEN-VALUE IN FRAME f-fold-1) 
                    var-result.des-result = string(pd-config-pedido.diex:SCREEN-VALUE IN FRAME f-fold-1).
                    
        if c-embal-jr = "" then do:            
                    
            FIND FIRST var-result WHERE 
                 var-result.nome-var     = "CODEMBAL"           AND 
                 var-result.nr-estrut    = int(ped-item.cod-refer)  AND
                 var-result.item-cotacao = ped-item.it-codigo           
                 EXCLUSIVE-LOCK NO-ERROR.
    
            IF AVAIL var-result THEN
                 ASSIGN var-result.des-result = c-embal-jr 
                        var-result.valor-char = c-embal-jr .

        end.

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "qtdpedido"         AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN var-result.valor-dec  = dec(pd-config-pedido.qtde-pedida-cliente:SCREEN-VALUE IN FRAME f-fold-1) 
                    var-result.des-result = string(pd-config-pedido.qtde-pedida-cliente:SCREEN-VALUE IN FRAME f-fold-1).
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "aplic"             AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN var-result.valor-char = string(pd-config-pedido.cod-aplicacao:SCREEN-VALUE IN FRAME f-fold-1)
                    var-result.des-result = string(pd-config-pedido.cod-aplicacao:SCREEN-VALUE IN FRAME f-fold-1). 
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "prodclie"          AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN var-result.valor-char = string(pd-config-pedido.cod-prod-cliente:SCREEN-VALUE IN FRAME f-fold-1)
                    var-result.des-result = string(pd-config-pedido.cod-prod-cliente:SCREEN-VALUE IN FRAME f-fold-1). 
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "pedcli"          AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN var-result.valor-char = string(pd-config-pedido.nr-pedido-cliente:SCREEN-VALUE IN FRAME f-fold-1)
                    var-result.des-result = string(pd-config-pedido.nr-pedido-cliente:SCREEN-VALUE IN FRAME f-fold-1). 

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "unfat"          AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN var-result.valor-char = string(pd-config-pedido.unid-fat-cliente:SCREEN-VALUE IN FRAME f-fold-1)
                    var-result.des-result = string(pd-config-pedido.unid-fat-cliente:SCREEN-VALUE IN FRAME f-fold-1). 

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "preun"             AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN var-result.valor-dec  = dec(pd-config-pedido.preco-unit:SCREEN-VALUE IN FRAME f-fold-1) 
                   var-result.des-result = string(pd-config-pedido.preco-unit:SCREEN-VALUE IN FRAME f-fold-1).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "densotic"          AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN var-result.valor-dec  = dec(pd-config-pedido.dens-otica:SCREEN-VALUE IN FRAME f-fold-1) 
                   var-result.des-result = string(pd-config-pedido.dens-otica:SCREEN-VALUE IN FRAME f-fold-1).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "obscq"             AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN var-result.valor-char = string(pd-config-pedido.obs-qualidade:SCREEN-VALUE IN FRAME f-fold-2)
                    var-result.des-result = string(pd-config-pedido.obs-qualidade:SCREEN-VALUE IN FRAME f-fold-2). 

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "obsger"             AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN var-result.valor-char = string(pd-config-pedido.obs-gerais:SCREEN-VALUE IN FRAME f-fold-2)
                    var-result.des-result = string(pd-config-pedido.obs-gerais:SCREEN-VALUE IN FRAME f-fold-2). 

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "obsetq"            AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN var-result.valor-char = string(pd-config-pedido.obs-etiqueta:SCREEN-VALUE IN FRAME f-fold-2)
                    var-result.des-result = string(pd-config-pedido.obs-etiqueta:SCREEN-VALUE IN FRAME f-fold-2). 

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "obsemb"            AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN var-result.valor-char = string(pd-config-pedido.obs-embalagem:SCREEN-VALUE IN FRAME f-fold-2)
                    var-result.des-result = string(pd-config-pedido.obs-embalagem:SCREEN-VALUE IN FRAME f-fold-2). 

 


        RUN pi-acha-embal(INPUT ped-item.it-codigo,        
                          INPUT int(ped-item.cod-refer)).
                          
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "CODEMBAL"           AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN  c-embal-jr = var-result.des-result.

 



        RUN CalculaVarFormula(INPUT ped-item.it-codigo,        
                              INPUT int(ped-item.cod-refer)).
                              
        FIND FIRST var-result WHERE 
                    var-result.nome-var     = "CODEMBAL"           AND 
                    var-result.nr-estrut    = int(ped-item.cod-refer)  AND
                    var-result.item-cotacao = ped-item.it-codigo           
                    EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN  c-embal-jr = var-result.des-result.
                              
                              
                    IF AVAIL var-result THEN
                 ASSIGN var-result.des-result = "" 
                        var-result.valor-char = "" .                              
 
        RUN pi-acha-embal(INPUT ped-item.it-codigo,        
                          INPUT int(ped-item.cod-refer)).
                         

       

   END.  /* if ... Item j  configurado */

   FIND FIRST pd-config-pedido WHERE
       pd-config-pedido.ep-codigo    = pd-config-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela AND
       pd-config-pedido.nr-pedido    = int(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela) AND
       pd-config-pedido.nr-sequencia = int(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)
       exclusive-lock NO-ERROR.
   
   IF AVAIL pd-config-pedido THEN 
       ASSIGN pd-config-pedido.integrado-ems = YES
              pd-config-pedido.nr-config     = int(ped-item.cod-refer).

   FIND FIRST pd-config-pedido WHERE
       pd-config-pedido.ep-codigo    = pd-config-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela AND
       pd-config-pedido.nr-pedido    = int(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela) AND
       pd-config-pedido.nr-sequencia = int(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)
       no-lock NO-ERROR.

   IF AVAIL pd-config-pedido THEN
       RUN pi-mostra-registro.

  

END.



ON CHOOSE OF bt-pesquisa IN FRAME f-tela
DO:
   ASSIGN i-ep-codigo-jr    = "0"
          i-nr-pedido-jr    = 0
          i-nr-sequencia-jr = 0.
                             
   RUN pi-pesquisa-reg.

   IF i-nr-pedido-jr <> 0 THEN DO:

     FIND FIRST pd-config-pedido WHERE
         pd-config-pedido.ep-codigo    = i-ep-codigo-jr       AND
         pd-config-pedido.nr-pedido    = i-nr-pedido-jr       AND
         pd-config-pedido.nr-sequencia = i-nr-sequencia-jr
         NO-LOCK NO-ERROR.
     
     IF AVAIL pd-config-pedido THEN
         RUN pi-mostra-registro.

   END.

END.

ON CHOOSE OF bt-goto IN FRAME f-tela
DO: 
    
    DEFINE BUTTON gt-bt-cancel AUTO-END-KEY 
         LABEL "&Cancelar" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE BUTTON gt-bt-ok AUTO-GO 
         LABEL "&OK" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE RECTANGLE gt-rt-botoes
         EDGE-PIXELS 2 GRAPHIC-EDGE  
         SIZE 58 BY 1.42
         BGCOLOR 7.

    DEFINE VARIABLE i-ep-codigo-gt    AS INTEGER NO-UNDO.
    DEFINE VARIABLE i-nr-pedido-gt    AS INTEGER NO-UNDO.
    DEFINE VARIABLE i-nr-sequencia-gt AS INTEGER NO-UNDO.
    
    DEFINE RECTANGLE gt-rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 4.50.
    
    DEFINE FRAME gt-frame-1

        i-ep-codigo-gt LABEL "Empresa" AT ROW 2.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 10 BY .88
        
        i-nr-pedido-gt LABEL "Nr.Pedido" AT ROW 3.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 10 BY .88
        
        i-nr-sequencia-gt LABEL "Seq.Pedido" AT ROW 4.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 10 BY .88
        
        gt-rect-1 AT ROW 1.9 COL 2

        gt-bt-ok          AT ROW 7.3 COL 2.14
        gt-bt-cancel      AT ROW 7.3 COL 13             
        gt-rt-botoes      AT ROW 7.0 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "Chave do Registro" FONT 1
             DEFAULT-BUTTON gt-bt-ok CANCEL-BUTTON gt-bt-cancel.

    ON "CHOOSE":U OF gt-bt-ok IN FRAME gt-frame-1 DO:

        ASSIGN i-ep-codigo-jr    = i-ep-codigo-gt:SCREEN-VALUE IN FRAME gt-frame-1
               i-nr-pedido-jr    = int(i-nr-pedido-gt:SCREEN-VALUE IN FRAME gt-frame-1)
               i-nr-sequencia-jr = int(i-nr-sequencia-gt:SCREEN-VALUE IN FRAME gt-frame-1).

     RETURN.

    END.

    ON ENTRY OF i-ep-codigo-gt IN FRAME gt-frame-1
    DO:

       FIND FIRST empresa NO-LOCK NO-ERROR.
       
       IF AVAIL empresa THEN
          ASSIGN i-ep-codigo-gt:SCREEN-VALUE IN FRAME gt-frame-1 = STRING(empresa.ep-codigo).

    END. 

    ENABLE i-ep-codigo-gt i-nr-pedido-gt i-nr-sequencia-gt gt-bt-ok gt-bt-cancel 
        WITH FRAME gt-frame-1. 
    
    WAIT-FOR "GO":U OF FRAME gt-frame-1.

    RUN le-registro-goto.

END.


ON CHOOSE OF bt-deleta IN FRAME f-tela
DO: 

   FIND FIRST pd-config-pedido WHERE
       pd-config-pedido.ep-codigo    = pd-config-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela AND
       pd-config-pedido.nr-pedido    = int(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela) AND
       pd-config-pedido.nr-sequencia = int(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)
       NO-LOCK NO-ERROR.

   IF NOT AVAIL pd-config-pedido THEN RETURN.

    DEFINE BUTTON ex-bt-cancel AUTO-END-KEY 
         LABEL "&Cancelar" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE BUTTON ex-bt-ok AUTO-GO 
         LABEL "&OK" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE RECTANGLE ex-rt-botoes
         EDGE-PIXELS 2 GRAPHIC-EDGE  
         SIZE 58 BY 1.42
         BGCOLOR 7.

    ASSIGN c-mensagem-jr = "Exclui Este Registro? ".

    DEFINE RECTANGLE ex-rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 4.50.
    
    DEFINE FRAME ex-frame-1
      
        c-mensagem-jr NO-LABEL 
           at ROW 3 col 10 
        
        ex-rect-1 AT ROW 1.9 COL 2

        ex-bt-ok          AT ROW 7.3 COL 2.14
        ex-bt-cancel      AT ROW 7.3 COL 13           
        ex-rt-botoes      AT ROW 7.0 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "ExclusÆo de Registro no Arquivo" FONT 1
             DEFAULT-BUTTON ex-bt-ok CANCEL-BUTTON ex-bt-cancel.

    ON "CHOOSE":U OF ex-bt-ok IN FRAME ex-frame-1 DO:

        FIND CURRENT pd-config-pedido EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL pd-config-pedido THEN
            DELETE pd-config-pedido.

        FIND NEXT pd-config-pedido NO-LOCK NO-ERROR.

        IF AVAIL pd-config-pedido THEN 
           RUN pi-mostra-registro. 

        ELSE DO:
            FIND PREV  pd-config-pedido NO-LOCK NO-ERROR.

            IF AVAIL pd-config-pedido THEN 
               RUN pi-mostra-registro. 

        END.

     RETURN.

    END.

    ENABLE ex-bt-cancel ex-bt-ok  
        WITH FRAME ex-frame-1. 
    
    DISPLAY c-mensagem-jr 
        WITH FRAME ex-frame-1.

    WAIT-FOR "GO":U OF FRAME ex-frame-1.

END.


ON CHOOSE OF bt-cancela IN FRAME f-tela
DO:

     if l-pd4000 then DO:
           
           apply "close" to this-procedure.
           RETURN.
    END.

   FIND CURRENT pd-config-pedido NO-LOCK NO-ERROR.

   IF AVAIL pd-config-pedido THEN
      RUN pi-mostra-registro.
   /* ELSE 
      RUN pi-limpa-campos. */

   RUN pi-disable-campos.
   RUN pi-disable-bt-grava.
   RUN pi-enable-outros-botoes.

END.


ON CHOOSE OF bt-novo IN FRAME f-tela
DO:
RETURN NO-APPLY.

    
   ASSIGN c-tipo-botao = "novo".

   RUN pi-enable-bt-grava.
   RUN pi-disable-outros-botoes.


   RUN pi-limpa-campos.


   RUN pi-enable-campos.

END.


ON CHOOSE OF bt-altera IN FRAME f-tela
DO:

   FIND FIRST pd-config-pedido WHERE
       pd-config-pedido.ep-codigo    = pd-config-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela AND
       pd-config-pedido.nr-pedido    = int(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela) AND
       pd-config-pedido.nr-sequencia = int(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)
       NO-LOCK NO-ERROR.


   IF ped-item.cod-sit-item > 2 THEN do: 
       ASSIGN c-mensagem-jr = "Item do Pedido nÆo esta disponivel!".
                RUN pi-mostra-mensagem.

       RETURN NO-APPLY.

   END.

    


   IF NOT AVAIL pd-config-pedido THEN RETURN.

   ASSIGN c-tipo-botao = "altera".

   RUN pi-enable-bt-grava.
   RUN pi-disable-outros-botoes.


   RUN pi-enable-campos.
   
   RUN pi-busca-config-atual.

END.


ON CHOOSE OF bt-copia IN FRAME f-tela
DO:
    RETURN NO-APPLY.

   FIND FIRST pd-config-pedido WHERE
       pd-config-pedido.ep-codigo    = pd-config-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela AND
       pd-config-pedido.nr-pedido    = int(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela) AND
       pd-config-pedido.nr-sequencia = int(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)
       NO-LOCK NO-ERROR.

   IF NOT AVAIL pd-config-pedido THEN RETURN.

   ASSIGN c-tipo-botao = "copia".

   RUN pi-enable-bt-grava.
   RUN pi-disable-outros-botoes.

   RUN pi-enable-campos.

END.


ON CHOOSE OF bt-grava IN FRAME f-tela
DO:

    DEFINE VARIABLE l-met AS LOGICAL    NO-UNDO.

    ASSIGN l-met = NO.

    IF c-tipo-botao <> "novo"  AND 
       c-tipo-botao <> "copia" AND  
       c-tipo-botao <> "altera" THEN RETURN.

    /* Aqui colocar as valida‡äes dos campos antes de serem
       gravados no arquivo */

      IF trim(pd-config-pedido.unid-fat-cliente:SCREEN-VALUE IN FRAME f-fold-1) <> "" THEN DO:
        
             IF DEC(trim(pd-config-pedido.preco-unit:SCREEN-VALUE IN FRAME f-fold-1)) = 0  THEN DO:
                ASSIGN c-mensagem-jr = "Informe o Preco unit rio para unidade de faturamento: " + pd-config-pedido.unid-fat-cliente:SCREEN-VALUE IN FRAME f-fold-1.
                RUN pi-mostra-mensagem.
        
                APPLY "entry" TO pd-config-pedido.preco-unit IN FRAME f-fold-1.
        
                RETURN NO-APPLY.
            END.
        
      END.
     
      FOR EACH estr-mod-cf WHERE estr-mod-cf.mo-codigo = ped-item.it-codigo NO-LOCK.

          FIND bcf-ITEM WHERE bcf-ITEM.it-codigo = estr-mod-cf.it-codigo NO-LOCK NO-ERROR.

          IF AVAIL bcf-ITEM THEN DO:
              l-met = (INDEX(bcf-ITEM.it-codigo,"met") > 0).

          END.
          
      END.
                                 
      IF l-met  THEN DO:
          IF dec(trim(pd-config-pedido.dens-otica:SCREEN-VALUE IN FRAME f-fold-1))  = 0 THEN DO:
                ASSIGN c-mensagem-jr = "Informe a Densidade àtica.".
                RUN pi-mostra-mensagem.
        
                APPLY "entry" TO pd-config-pedido.dens-otica IN FRAME f-fold-1.
        
                RETURN NO-APPLY.
         END.

      END.
         
 
    IF DEC(trim(pd-config-pedido.qtde-pedida-cliente:SCREEN-VALUE IN FRAME f-fold-1)) = 0  THEN DO:
        ASSIGN c-mensagem-jr = "Informe a Quantidade do cliente.".
        RUN pi-mostra-mensagem.

        APPLY "entry" TO pd-config-pedido.qtde-pedida-cliente IN FRAME f-fold-1.

        RETURN NO-APPLY.
    END.

    IF dec(trim(pd-config-pedido.diex:SCREEN-VALUE IN FRAME f-fold-1)) = 0  THEN DO:
      ASSIGN c-mensagem-jr = "Informe o diametro externo.".
      RUN pi-mostra-mensagem.

      APPLY "entry" TO pd-config-pedido.diex IN FRAME f-fold-1.

      RETURN NO-APPLY.
    END.


    IF dec(trim(pd-config-pedido.diin:SCREEN-VALUE IN FRAME f-fold-1)) = 0  THEN DO:
        ASSIGN c-mensagem-jr = "Informe o diametro interno.".
        RUN pi-mostra-mensagem.

        APPLY "entry" TO pd-config-pedido.diin IN FRAME f-fold-1.

        RETURN NO-APPLY.
    END.

    IF dec(trim(pd-config-pedido.largura:SCREEN-VALUE IN FRAME f-fold-1)) = 0 THEN DO:
        ASSIGN c-mensagem-jr = "Informe a Largura.".
        RUN pi-mostra-mensagem.

        APPLY "entry" TO pd-config-pedido.largura IN FRAME f-fold-1.

        RETURN NO-APPLY.
    END.


    IF trim(pd-config-pedido.cod-aplicacao:SCREEN-VALUE IN FRAME f-fold-1) = "" THEN DO:
        ASSIGN c-mensagem-jr = "Escolha uma Aplica‡Æo.".
        RUN pi-mostra-mensagem.

        APPLY "entry" TO pd-config-pedido.cod-aplicacao IN FRAME f-fold-1.

        RETURN NO-APPLY.
    END.

    FIND FIRST empresa WHERE
        empresa.ep-codigo = STRING(pd-config-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela)
        NO-LOCK NO-ERROR.

    IF NOT AVAIL empresa THEN DO:

        ASSIGN c-mensagem-jr = "Empresa NÆo Existe".
        RUN pi-mostra-mensagem.

        APPLY "entry" TO pd-config-pedido.ep-codigo IN FRAME f-tela.

        RETURN NO-APPLY.

    END.

    FIND FIRST ped-venda WHERE
        ped-venda.nr-pedido = int(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)
        NO-LOCK NO-ERROR.

    IF NOT AVAIL ped-venda THEN DO:

        ASSIGN c-mensagem-jr = "Pedido de Venda NÆo Existe".
        RUN pi-mostra-mensagem.

        APPLY "entry" TO pd-config-pedido.nr-pedido IN FRAME f-tela.

        RETURN NO-APPLY.

    END.

    FIND FIRST ped-item OF ped-venda WHERE
        ped-item.nr-sequencia = int(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela) AND
        ped-item.ind-componen < 3
        NO-LOCK NO-ERROR.

    IF NOT AVAIL ped-item THEN DO:

        ASSIGN c-mensagem-jr = "Item do Pedido de Venda NÆo Existe".
        RUN pi-mostra-mensagem.

        APPLY "entry" TO pd-config-pedido.nr-sequencia IN FRAME f-tela.

        RETURN NO-APPLY.

    END.

    /* ---------------------------------------------------- */

    ASSIGN c-tipo-botao2 = c-tipo-botao
           c-tipo-botao  = "".

    RUN pi-le-pela-chave.

    IF RETURN-VALUE = "nok" THEN DO: 
        RUN pi-limpa-campos.
        RUN pi-disable-campos.
        APPLY "choose" to bt-cancela IN FRAME f-tela.  
       RETURN.
    END.
       
    RUN pi-grava-registro.

    RUN pi-disable-campos.

 
    if l-pd4000 then DO:
           
           apply "close" to this-procedure.
           RETURN.
    END.

    IF c-tipo-botao2 = "novo" THEN DO:
        APPLY "choose" to bt-novo IN FRAME f-tela.
        RETURN.
    END.
    ELSE DO:
        APPLY "choose" to bt-cancela IN FRAME f-tela.
        RETURN.
    END.

END.


ON CHOOSE OF bt-ajuda IN FRAME f-tela
DO:


END.


ON CHOOSE OF bt-sai IN FRAME f-tela
DO:
   apply "close" to this-procedure.
END.

ON MOUSE-SELECT-CLICK OF im-fold-1 IN FRAME f-tela
DO:
   run pi-troca-pagina.
END.

ON MOUSE-SELECT-CLICK OF im-fold-4 IN FRAME f-tela
DO:
   run pi-troca-pagina.
END.

ON MOUSE-SELECT-CLICK OF im-fold-2 IN FRAME f-tela
DO:
   run pi-troca-pagina.
END.

ON MOUSE-SELECT-CLICK OF im-fold-3 IN FRAME f-tela
DO:
   run pi-troca-pagina.
END.

/* ***************************  Main Block  *************************** */

ASSIGN CURRENT-WINDOW             = {&WINDOW-NAME}
   THIS-PROCEDURE:CURRENT-WINDOW  = {&WINDOW-NAME}.

assign v-cod-prog-gerado = "espd0061".



def var c-tit as char no-undo.

ASSIGN c-tit = "espd0061 - Configura‡Æo de Pedido de Venda".
assign {&WINDOW-NAME}:title = c-tit
       c-arq-old        = c-arquivo
       c-impressora-old = c-arquivo.


assign {&window-name}:virtual-width-chars  = {&window-name}:width-chars  
       {&window-name}:virtual-height-chars = {&window-name}:height-chars 
       {&window-name}:min-width-chars      = {&window-name}:width-chars  
       {&window-name}:max-width-chars      = {&window-name}:width-chars  
       {&window-name}:min-height-chars     = {&window-name}:height-chars 
       {&window-name}:max-height-chars     = {&window-name}:height-chars.
assign c-terminal = " Terminal".


ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.


def var wh-label-fld-1     as widget-handle no-undo.
def var wh-label-cla     as widget-handle no-undo.
def var wh-label-fld-2     as widget-handle no-undo.
def var wh-label-fld-3     as widget-handle no-undo.
def var wh-label-fld-4     as widget-handle no-undo.
def var wh-group         as widget-handle no-undo.
def var wh-child         as widget-handle no-undo.
def var c-list-folders   as char          no-undo.
def var i-current-folder as integer       no-undo.
def var i-new-folder     as integer       no-undo.
def var c-aux            as char          no-undo.
def var i-aux            as integer       no-undo.

ON  CLOSE OF THIS-PROCEDURE DO:
  /*  run btb/btb918zb.p (input c-programa-mg97,
                        input-output rw-log-exec,
                        input no).   */
    RUN disable_ui.

END.

pd-config-pedido.cod-aplicacao:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME f-fold-1.
pd-config-pedido.cod-aplicacao:READ-ONLY IN FRAME f-fold-1 = YES.
pd-config-pedido.cod-aplicacao:BGCOLOR IN FRAME f-fold-1 = ?.
      


/********************************************************** 
** Tradu‡Æo p gina - frame f-fold-1
**********************************************************/
create text wh-label-fld-1
    assign frame        = frame f-tela:handle
           format       = "x(12)"
           font         = 1
           screen-value = "Configura‡Æo"
           width        = 11
           row          = 6.6
           col          = im-fold-1:col in frame f-tela + 1.86
           visible      = yes
     triggers:
         on mouse-select-click
            apply "mouse-select-click" to im-fold-1 in frame f-tela.           
     end triggers.     

/********************************************************** 
** Tradu‡Æo p gina - frame f-fold-2
**********************************************************/
create text wh-label-fld-2
         assign frame        = frame f-tela:handle
                format       = "x(12)"
                font         = 1
                screen-value = "Observa‡äes"
                width        = 11
                row          = 6.6
                col          = im-fold-2:col in frame f-tela + 1.86
                visible      = yes
          triggers:
              on mouse-select-click
                 apply "mouse-select-click" to im-fold-2 in frame f-tela.           
          end triggers.

/********************************************************** 
** Tradu‡Æo p gina - frame f-fold-3
**********************************************************/
     
create text wh-label-fld-3
    assign frame        = frame f-tela:handle
           format       = "x(12)"
           font         = 1
           screen-value = "     "
           width        = 11
           row          = 6.6
           col          = im-fold-3:col in frame f-tela + 1.86
           visible      = yes
     triggers:
         on mouse-select-click
            apply "mouse-select-click" to im-fold-3 in frame f-tela.           
     end triggers. 

/********************************************************** 
** Tradu‡Æo p gina - frame f-fold-4
**********************************************************/
create text wh-label-fld-4
    assign frame        = frame f-tela:handle
           format       = "x(10)"
           font         = 1
           screen-value = "    "
           width        = 11
           row          = 6.6
           col          = im-fold-4:col in frame f-tela + 1.86
           visible      = yes
     triggers:
         on mouse-select-click
            apply "mouse-select-click" to im-fold-4 in frame f-tela.           
     end triggers.                   


/********************************************************** 
** Troca de p gina por CTRL-TAB e SHIFT-CTRL-TAB
**********************************************************/

&IF "{&FOLD1}" <> "" &THEN 
    assign c-list-folders = c-list-folders + "im-fold-1,".
&ENDIF
&IF "{&PGCLA}" <> "" &THEN 
    assign c-list-folders = c-list-folders + "im-pg-cla,".
&ENDIF
&IF "{&FOLD2}" <> "" &THEN 
    assign c-list-folders = c-list-folders + "im-fold-2,".
&ENDIF
&IF "{&FOLD3}" <> "" &THEN 
    assign c-list-folders = c-list-folders + "im-fold-3,".
&ENDIF
&IF "{&FOLD4}" <> "" &THEN
    assign c-list-folders = c-list-folders + "im-fold-4".
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
               h_handle = frame f-tela:first-child
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



PAUSE 0 BEFORE-HIDE.

MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    IF SESSION:SET-WAIT-STATE("":U) THEN.
    RUN enable_UI.

    RUN pi-disable-bt-grava.   


    assign v-cod-pg-mouse-selec = "im-fold-1".


    if v-cod-pg-mouse-selec = "im-fold-1"
    then
        apply "mouse-select-click" to im-fold-1 in frame f-tela.

    if v-cod-pg-mouse-selec = "im-fold-2"
    then
        apply "mouse-select-click" to im-fold-2 in frame f-tela.

    if v-cod-pg-mouse-selec = "im-fold-4"
    then
        apply "mouse-select-click" to im-fold-4 in frame f-tela.

    if v-cod-pg-mouse-selec = "im-fold-3"
    then
        apply "mouse-select-click" to im-fold-3 in frame f-tela.

     view c-win.
     apply "entry" to frame f-tela.
     apply "entry" to c-win.

    if  im-fold-1:sensitive in frame f-tela = no then do:
    run pi-muda-cor-label-folder(input "Textos").

    end.
    
    if  im-fold-3:sensitive in frame f-tela = no then do:
    run pi-muda-cor-label-folder(input "Cadastro 3").

    end.

    if  im-fold-2:sensitive in frame f-tela = no then do:
        run pi-muda-cor-label-folder(input "Cadastro 2").

    END.

    if  im-fold-4:sensitive in frame f-tela = no then do:
        run pi-muda-cor-label-folder(input "Cadastro 4").

    end.
   
    /* Desabilita folders nÆo usadas */

    ASSIGN /*im-fold-2:VISIBLE in frame f-tela = no*/
           im-fold-3:VISIBLE in frame f-tela = no 
           im-fold-4:VISIBLE in frame f-tela = no.


    /* Se o programa foi chamado pelo PD4000 j  vem com o numero do pedido rw-nr-pedido-0061 */

    l-pd4000 = NO.
                        
    IF rw-nr-pedido-0061 <> ? THEN DO:

        l-pd4000 = YES.
   
    FIND FIRST ped-item where rowid(ped-item) = rw-nr-pedido-0061
               NO-LOCK NO-ERROR.
        ASSIGN rw-nr-pedido-0061 = ?.
                            
        IF AVAIL ped-item THEN DO:

           FIND FIRST ped-venda of ped-item
               NO-LOCK NO-ERROR.
   

        
             IF AVAIL ped-venda THEN DO:

                 FIND FIRST estabelec WHERE estabelec.cod-estabel = ped-venda.cod-estabel NO-LOCK NO-ERROR.

               FIND FIRST pd-config-pedido WHERE
                   pd-config-pedido.ep-codigo    = estabelec.ep-codigo                    AND
                   pd-config-pedido.nr-pedido    = ped-venda.nr-pedido    AND
                   pd-config-pedido.nr-sequencia = ped-item.nr-sequencia
                   NO-LOCK NO-ERROR.

               IF NOT AVAIL pd-config-pedido THEN DO:

                   CREATE pd-config-pedido.

                   ASSIGN pd-config-pedido.ep-codigo    = estabelec.ep-codigo
                          pd-config-pedido.nr-pedido    = ped-venda.nr-pedido
                          pd-config-pedido.nr-sequencia = ped-item.nr-sequencia
                          pd-config-pedido.nr-config    = int(ped-item.cod-refer)
                          pd-config-pedido.nr-config    = IF int(ped-item.cod-refer) < 2 THEN 0 ELSE int(ped-item.cod-refer).

                    RUN pi-cria-config-atual.
               END.

               ELSE DO:

                  IF AVAIL ped-item AND int(ped-item.cod-refer) <> 1 THEN DO:
                  
                      FIND FIRST var-result WHERE 
                           var-result.nome-var     = "bobpalete"         AND 
                           var-result.nr-estrut    = int(ped-item.cod-refer)  AND
                           var-result.item-cotacao = ped-item.it-codigo           
                           NO-LOCK NO-ERROR.
                  
                      IF AVAIL var-result THEN
                          ASSIGN i-qt-bob-plt = var-result.valor-dec.
                  
                      FIND FIRST var-result WHERE 
                           var-result.nome-var     = "qtdpalete"         AND 
                           var-result.nr-estrut    = int(ped-item.cod-refer)  AND
                           var-result.item-cotacao = ped-item.it-codigo           
                           NO-LOCK NO-ERROR.
                  
                      IF AVAIL var-result THEN
                          ASSIGN i-qt-plt = var-result.valor-dec.

                  END.

               END.

               RUN pi-mostra-registro.

               APPLY "choose" TO bt-altera IN FRAME f-tela.

           END.
        
        END.

    END.


    /* ----------------------------- */

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
   ENABLE bt-primeiro 
          bt-anterior 
          bt-proximo  
          bt-final 
          bt-goto
          bt-pesquisa
          bt-novo
          bt-copia
          bt-altera
          bt-deleta
          bt-cancela
          bt-grava  
          bt-sai
          bt-ajuda
          im-fold-4 im-fold-1 im-fold-2 im-fold-3
   WITH FRAME f-tela IN WINDOW C-Win.
   

  {&OPEN-BROWSERS-IN-QUERY-c-win}

   FIND FIRST pd-config-pedido NO-LOCK NO-ERROR.

   IF AVAIL pd-config-pedido THEN DO:

       RUN pi-mostra-registro.

   END.

   ENABLE  
    /*   pd-config-pedido.sequencia */
   WITH FRAME f-tela IN WINDOW C-Win.
   

   ENABLE  
    /*   pd-config-pedido.descricao   */
   WITH FRAME f-fold-1 IN WINDOW C-Win.
   
   
   DISPLAY 
   WITH FRAME f-fold-4 IN WINDOW C-Win.

   ENABLE 
   WITH FRAME f-fold-4 IN WINDOW C-Win.
   
   DISPLAY 
   WITH FRAME f-fold-2 IN WINDOW C-Win.

   ENABLE 
   WITH FRAME f-fold-2 IN WINDOW C-Win.

   DISPLAY 
   WITH FRAME f-fold-3 IN WINDOW C-Win.

   ENABLE 
   WITH FRAME f-fold-3 IN WINDOW C-Win.
   
   VIEW C-Win.

END PROCEDURE.

PROCEDURE local-exit :
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN.
END PROCEDURE.  

PROCEDURE pi-troca-pagina:

self:move-to-top() in frame f-tela.


case self:name:
    when "im-fold-1" then do with frame f-tela:
        &IF "{&FOLD1}" <> "" &THEN
            view frame {&FOLD1}.
            run pi-first-child (input frame {&FOLD1}:handle).
            im-fold-1:load-image("image/im-fldup") .
            assign im-fold-1:height = 1.20
                   im-fold-1:row    = 6.25.
        &ENDIF
        &IF "{&PGCLA}" <> "" &THEN
            hide frame {&PGCLA}.
              im-pg-cla:load-image("image/im-flddn") .
              im-pg-cla:move-to-bottom() .
            assign im-pg-cla:height = 1
                   im-pg-cla:row    = 6.25.
        &ENDIF
        &IF "{&FOLD2}" <> "" &THEN
            hide frame {&FOLD2}.
              im-fold-2:load-image("image/im-flddn") .
              im-fold-2:move-to-bottom() .
            assign im-fold-2:height = 1
                   im-fold-2:row    = 6.25.
        &ENDIF
        &IF "{&FOLD3}" <> "" &THEN
            hide frame {&FOLD3}.
              im-fold-3:load-image("image/im-flddn") .
              im-fold-3:move-to-bottom() .
            assign im-fold-3:height = 1
                   im-fold-3:row    = 6.25.
        &ENDIF
        &IF "{&FOLD4}" <> "" &THEN
            hide frame {&FOLD4}.
              im-fold-4:load-image("image/im-flddn") .
              im-fold-4:move-to-bottom() .
            assign im-fold-4:height = 1
                   im-fold-4:row    = 6.25.
        &ENDIF
    end.



    when "im-pg-cla" then do with frame f-tela:
        &IF "{&FOLD1}" <> "" &THEN
            hide frame {&FOLD1}.
              im-fold-1:load-image("image/im-flddn") .
              im-fold-1:move-to-bottom() .
            assign im-fold-1:height = 1
                   im-fold-1:row    = 6.25.
        &ENDIF
        &IF "{&PGCLA}" <> "" &THEN
            view frame {&PGCLA}.
            run pi-first-child (input frame {&PGCLA}:handle).
              im-pg-cla:load-image("image/im-flddn") .
            assign im-pg-cla:height = 1
                   im-pg-cla:row    = 6.25.
        &ENDIF
        &IF "{&FOLD2}" <> "" &THEN
            hide frame {&FOLD2}.
              im-fold-2:load-image("image/im-flddn") .
              im-fold-2:move-to-bottom() .
            assign im-fold-2:height = 1
                   im-fold-2:row    = 6.25.
        &ENDIF
        &IF "{&FOLD3}" <> "" &THEN
            hide frame {&FOLD3}.
              im-fold-3:load-image("image/im-flddn") .
              im-fold-3:move-to-bottom() .
            assign im-fold-3:height = 1
                   im-fold-3:row    = 6.25.
        &ENDIF
        &IF "{&FOLD4}" <> "" &THEN
            hide frame {&FOLD4}.
              im-fold-4:load-image("image/im-flddn") .
              im-fold-4:move-to-bottom() .
            assign im-fold-4:height = 1
                   im-fold-4:row    = 6.25.
        &ENDIF
    end.



    when "im-fold-2" then do with frame f-tela:
        &IF "{&FOLD1}" <> "" &THEN
            hide frame {&FOLD1}.
              im-fold-1:load-image("image/im-flddn") .
              im-fold-1:move-to-bottom() .
            assign im-fold-1:height = 1
                   im-fold-1:row    = 6.25.
        &ENDIF
        &IF "{&PGCLA}" <> "" &THEN
            hide frame {&PGCLA}.
              im-pg-cla:load-image("image/im-flddn") .
              im-pg-cla:move-to-bottom() .
            assign im-pg-cla:height = 1
                   im-pg-cla:row    = 6.25.
        &ENDIF
        &IF "{&FOLD2}" <> "" &THEN
            view frame {&FOLD2}.
            run pi-first-child (input frame {&FOLD2}:handle).
              im-fold-2:load-image("image/im-fldup") .
            assign im-fold-2:height = 1.20
                   im-fold-2:row    = 6.25.
        &ENDIF
        &IF "{&FOLD3}" <> "" &THEN
            hide frame {&FOLD3}.
              im-fold-3:load-image("image/im-flddn") .
              im-fold-3:move-to-bottom() .
            assign im-fold-3:height = 1
                   im-fold-3:row    = 6.25.
        &ENDIF
        &IF "{&FOLD4}" <> "" &THEN
            hide frame {&FOLD4}.
              im-fold-4:load-image("image/im-flddn") .
              im-fold-4:move-to-bottom() .
            assign im-fold-4:height = 1
                   im-fold-4:row    = 6.25.
        &ENDIF
    end.



    when "im-fold-3" then do with frame f-tela:
        &IF "{&FOLD1}" <> "" &THEN
            hide frame {&FOLD1}.
              im-fold-1:load-image("image/im-flddn") .
              im-fold-1:move-to-bottom() .
            assign im-fold-1:height = 1
                   im-fold-1:row    = 6.25.
        &ENDIF
        &IF "{&PGCLA}" <> "" &THEN
            hide frame {&PGCLA}.
              im-pg-cla:load-image("image/im-flddn") .
              im-pg-cla:move-to-bottom() .
            assign im-pg-cla:height = 1
                   im-pg-cla:row    = 6.25.
        &ENDIF
        &IF "{&FOLD2}" <> "" &THEN
            hide frame {&FOLD2}.
              im-fold-2:load-image("image/im-flddn") .
              im-fold-2:move-to-bottom() .
            assign im-fold-2:height = 1
                   im-fold-2:row    = 6.25.
        &ENDIF
        &IF "{&FOLD3}" <> "" &THEN
            view frame {&FOLD3}.
            run pi-first-child (input frame {&FOLD3}:handle).
              im-fold-3:load-image("image/im-flddn") .
            assign im-fold-3:height = 1
                   im-fold-3:row    = 6.25.
        &ENDIF
        &IF "{&FOLD4}" <> "" &THEN
            hide frame {&FOLD4}.
              im-fold-4:load-image("image/im-flddn") .
              im-fold-4:move-to-bottom() .
            assign im-fold-4:height = 1
                   im-fold-4:row    = 6.25.
        &ENDIF
    end.



    when "im-fold-4" then do with frame f-tela:
        &IF "{&FOLD1}" <> "" &THEN
            hide frame {&FOLD1}.
              im-fold-1:load-image("image/im-flddn") .
              im-fold-1:move-to-bottom() .
            assign im-fold-1:height = 1
                   im-fold-1:row    = 6.25.
        &ENDIF
        &IF "{&PGCLA}" <> "" &THEN
            hide frame {&PGCLA}.
              im-pg-cla:load-image("image/im-flddn") .
              im-pg-cla:move-to-bottom() .
            assign im-pg-cla:height = 1
                   im-pg-cla:row    = 6.25.
        &ENDIF
        &IF "{&FOLD2}" <> "" &THEN
            hide frame {&FOLD2}.
              im-fold-2:load-image("image/im-flddn") .
              im-fold-2:move-to-bottom() .
            assign im-fold-2:height = 1
                   im-fold-2:row    = 6.25.
        &ENDIF
        &IF "{&FOLD3}" <> "" &THEN
            hide frame {&FOLD3}.
              im-fold-3:load-image("image/im-flddn") .
              im-fold-3:move-to-bottom() .
            assign im-fold-3:height = 1
                   im-fold-3:row    = 6.25.
        &ENDIF
        &IF "{&FOLD4}" <> "" &THEN
            view frame {&FOLD4}.
            run pi-first-child (input frame {&FOLD4}:handle).
              im-fold-4:load-image("image/im-fldup") .
            assign im-fold-4:height = 1.20
                   im-fold-4:row    = 6.25.
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

    assign wh-pai = frame f-tela:handle
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

PROCEDURE pi-mostra-registro.

    ASSIGN pd-config-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela             = STRING (pd-config-pedido.ep-codigo)
           pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela             = STRING (pd-config-pedido.nr-pedido)
           pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela          = STRING (pd-config-pedido.nr-sequencia)
           pd-config-pedido.nr-config:SCREEN-VALUE IN FRAME f-tela             = STRING (pd-config-pedido.nr-config)

           pd-config-pedido.largura:SCREEN-VALUE IN FRAME f-fold-1             = STRING (pd-config-pedido.largura)
           pd-config-pedido.diin:SCREEN-VALUE IN FRAME f-fold-1                = STRING (pd-config-pedido.diin)
           pd-config-pedido.diex:SCREEN-VALUE IN FRAME f-fold-1                = STRING (pd-config-pedido.diex)
           pd-config-pedido.qtde-pedida-cliente:SCREEN-VALUE IN FRAME f-fold-1 = STRING (pd-config-pedido.qtde-pedida-cliente)
           pd-config-pedido.cod-aplicacao:SCREEN-VALUE IN FRAME f-fold-1       = STRING (pd-config-pedido.cod-aplicacao)
           i-qt-bob-plt:SCREEN-VALUE IN FRAME f-fold-1                         = STRING (i-qt-bob-plt)
           i-qt-plt:SCREEN-VALUE IN FRAME f-fold-1                             = STRING (i-qt-plt)

           pd-config-pedido.integrado-ems:checked in frame f-fold-1            = pd-config-pedido.integrado-ems

           pd-config-pedido.cod-prod-cliente:SCREEN-VALUE IN FRAME f-fold-1    = STRING (pd-config-pedido.cod-prod-cliente)
           pd-config-pedido.nr-pedido-cliente:SCREEN-VALUE IN FRAME f-fold-1   = STRING (pd-config-pedido.nr-pedido-cliente)
           pd-config-pedido.qtde-ajustada-ped:SCREEN-VALUE IN FRAME f-fold-1   = STRING (pd-config-pedido.qtde-ajustada-ped)
           pd-config-pedido.qtde-bobinas:SCREEN-VALUE IN FRAME f-fold-1        = STRING (pd-config-pedido.qtde-bobinas)
           pd-config-pedido.dec-1:SCREEN-VALUE IN FRAME f-fold-1               = STRING (pd-config-pedido.dec-1)
           pd-config-pedido.unid-fat-cliente:SCREEN-VALUE IN FRAME f-fold-1    = STRING (pd-config-pedido.unid-fat-cliente)
           pd-config-pedido.preco-unit:SCREEN-VALUE IN FRAME f-fold-1          = STRING (pd-config-pedido.preco-unit)
           pd-config-pedido.dens-otica:SCREEN-VALUE IN FRAME f-fold-1          = STRING (pd-config-pedido.dens-otica).


    FIND FIRST if-ped-venda WHERE
        if-ped-venda.nr-pedido = pd-config-pedido.nr-pedido
        NO-LOCK NO-ERROR.

    IF AVAIL if-ped-venda THEN DO:

        FIND bf-ped-venda WHERE
            bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac
            NO-LOCK NO-ERROR.

        IF AVAIL bf-ped-venda THEN DO:

            ASSIGN i-nr-pedido-final:SCREEN-VALUE IN FRAME f-fold-1    = STRING (bf-ped-venda.nr-pedido)   
                   c-nome-abrev-final:SCREEN-VALUE IN FRAME f-fold-1   = STRING (bf-ped-venda.nome-abrev) 
                   c-nr-pedcli-final:SCREEN-VALUE IN FRAME f-fold-1    = STRING (bf-ped-venda.nr-pedcli)  
                   c-cod-estabel-final:SCREEN-VALUE IN FRAME f-fold-1  = STRING (bf-ped-venda.cod-estabel).

        END.

        ELSE DO:

            ASSIGN i-nr-pedido-final:SCREEN-VALUE IN FRAME f-fold-1    = ""  
                   c-nome-abrev-final:SCREEN-VALUE IN FRAME f-fold-1   = "" 
                   c-nr-pedcli-final:SCREEN-VALUE IN FRAME f-fold-1    = "" 
                   c-cod-estabel-final:SCREEN-VALUE IN FRAME f-fold-1  = "".

        END.

    END.
        
    ASSIGN pd-config-pedido.obs-qualidade:SCREEN-VALUE IN FRAME f-fold-2       = STRING (pd-config-pedido.obs-qualidade)
           pd-config-pedido.obs-gerais:SCREEN-VALUE IN FRAME f-fold-2          = STRING (pd-config-pedido.obs-gerais)
           pd-config-pedido.obs-embalagem:SCREEN-VALUE IN FRAME f-fold-2       = STRING (pd-config-pedido.obs-embalagem)
           pd-config-pedido.obs-etiqueta:SCREEN-VALUE IN FRAME f-fold-2        = STRING (pd-config-pedido.obs-etiqueta).

    RUN pi-calculo-formulas.

    ENABLE pd-config-pedido.integrado-ems  WITH FRAME f-fold-1.
    ASSIGN pd-config-pedido.integrado-ems:SELECTABLE IN FRAME f-fold-1 = yes.


    FIND FIRST empresa WHERE
        empresa.ep-codigo = string(pd-config-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela)
        NO-LOCK NO-ERROR.
    
    IF AVAIL empresa THEN
       ASSIGN c-nome-empresa:SCREEN-VALUE IN FRAME f-tela = empresa.nome.
    ELSE
       ASSIGN c-nome-empresa:SCREEN-VALUE IN FRAME f-tela = "".

    FIND FIRST ped-venda WHERE
        ped-venda.nr-pedido = int(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)
        NO-LOCK NO-ERROR.
    
    IF AVAIL ped-venda THEN
       ASSIGN c-nome-abrev:SCREEN-VALUE IN FRAME f-tela  = ped-venda.nome-abrev
              c-cod-estabel:SCREEN-VALUE IN FRAME f-tela = ped-venda.cod-estabel.
    ELSE
       ASSIGN c-nome-abrev:SCREEN-VALUE IN FRAME f-tela  = ""
              c-cod-estabel:SCREEN-VALUE IN FRAME f-tela = "".
    
    
    IF AVAIL ped-venda THEN
        FIND FIRST ped-item OF ped-venda WHERE
             ped-item.nr-sequencia = int(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)      AND
             ped-item.ind-componen < 3 NO-LOCK NO-ERROR.
    
    IF AVAIL ped-item THEN DO:
        ASSIGN c-it-codigo:SCREEN-VALUE IN FRAME f-tela  = ped-item.it-codigo.
        FIND FIRST ord-prod WHERE 
                     ord-prod.nome-abrev   = ped-item.nome-abrev AND
                     ord-prod.nr-pedido    = ped-item.nr-pedcli AND
                     ord-prod.nr-sequencia = ped-item.nr-sequencia NO-LOCK NO-ERROR. 

          ASSIGN i-nr-ord-produ:SCREEN-VALUE IN FRAME f-tela  = "". 

          IF AVAIL ord-prod  THEN
              ASSIGN i-nr-ord-produ:SCREEN-VALUE IN FRAME f-tela  = string(ord-prod.nr-ord-produ).

    END.

    ELSE
       ASSIGN c-it-codigo:SCREEN-VALUE IN FRAME f-tela       = "".

    ENABLE bt-integra-ems WITH FRAME f-fold-1.  
    
                 
    
END PROCEDURE.

PROCEDURE pi-le-primeiro.

    FIND FIRST pd-config-pedido NO-LOCK NO-ERROR.

        IF AVAIL pd-config-pedido THEN DO:

           RUN pi-mostra-registro.

        END.

END PROCEDURE.


PROCEDURE pi-le-proximo.

    FIND FIRST pd-config-pedido WHERE
        pd-config-pedido.ep-codigo    = pd-config-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela AND
        pd-config-pedido.nr-pedido    = int(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela) AND
        pd-config-pedido.nr-sequencia = int(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)
        NO-LOCK NO-ERROR.

    FIND NEXT pd-config-pedido NO-LOCK NO-ERROR.

        IF AVAIL pd-config-pedido THEN DO:

           RUN pi-mostra-registro.

        END.

END PROCEDURE.


PROCEDURE pi-le-anterior.

    FIND FIRST pd-config-pedido WHERE
        pd-config-pedido.ep-codigo    = pd-config-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela AND
        pd-config-pedido.nr-pedido    = int(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela) AND
        pd-config-pedido.nr-sequencia = int(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)
        NO-LOCK NO-ERROR.

    FIND PREV pd-config-pedido NO-LOCK NO-ERROR.

        IF AVAIL pd-config-pedido THEN DO:

           RUN pi-mostra-registro.

        END.

END PROCEDURE.

PROCEDURE pi-le-ultimo.

    FIND LAST pd-config-pedido NO-LOCK NO-ERROR.

        IF AVAIL pd-config-pedido THEN DO:

           RUN pi-mostra-registro.

        END.

END PROCEDURE.



PROCEDURE le-registro-goto.

    FIND FIRST bf-pd-config-pedido WHERE
        bf-pd-config-pedido.ep-codigo    = i-ep-codigo-jr AND
        bf-pd-config-pedido.nr-pedido    = i-nr-pedido-jr AND
        bf-pd-config-pedido.nr-sequencia = i-nr-sequencia-jr 
        NO-LOCK NO-ERROR.

    IF NOT AVAIL bf-pd-config-pedido THEN DO:
        ASSIGN c-mensagem-jr = "REGISTRO NÇO EXISTE!".
        RUN pi-mostra-mensagem.
        RETURN.
    END.

    FIND FIRST pd-config-pedido WHERE
        pd-config-pedido.ep-codigo    = i-ep-codigo-jr AND
        pd-config-pedido.nr-pedido    = i-nr-pedido-jr AND
        pd-config-pedido.nr-sequencia = i-nr-sequencia-jr 
        NO-LOCK NO-ERROR.

    IF AVAIL pd-config-pedido THEN
        RUN pi-mostra-registro.

END PROCEDURE.

PROCEDURE pi-mostra-mensagem.

        DEFINE BUTTON mens-bt-cancel AUTO-END-KEY 
             LABEL "&Fechar" 
             SIZE 10 BY 1
             BGCOLOR 8.
        
        DEFINE RECTANGLE mens-rt-botoes
             EDGE-PIXELS 2 GRAPHIC-EDGE  
             SIZE 58 BY 1.42
             BGCOLOR 7.  
      
        DEFINE RECTANGLE mens-rect-1
         EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
         SIZE 59 BY 3.30.
        
        DEFINE FRAME mens-frame-1
      
            c-mensagem-jr NO-LABEL 
               at ROW 3 col 10 
            
            mens-rect-1 AT ROW 1.9 COL 2
      
            mens-bt-cancel      AT ROW 7.3 COL 23             
            mens-rt-botoes      AT ROW 7.0 COL 1
            SPACE(0.28)
            WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
                 THREE-D SCROLLABLE TITLE "Mensagem" FONT 1
                 DEFAULT-BUTTON mens-bt-cancel CANCEL-BUTTON mens-bt-cancel.
      
      
        DISPLAY c-mensagem-jr WITH FRAME mens-frame-1.
      
        ENABLE mens-bt-cancel 
            WITH FRAME mens-frame-1. 
        
        WAIT-FOR "GO":U OF FRAME mens-frame-1.


END PROCEDURE.


PROCEDURE pi-limpa-campos.

    ASSIGN pd-config-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela             = ""
           pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela             = ""
           pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela          = ""
           pd-config-pedido.nr-config:SCREEN-VALUE IN FRAME f-tela             = ""

           pd-config-pedido.largura:SCREEN-VALUE IN FRAME f-fold-1             = ""
           pd-config-pedido.diin:SCREEN-VALUE IN FRAME f-fold-1                = ""
           pd-config-pedido.diex:SCREEN-VALUE IN FRAME f-fold-1                = ""
           pd-config-pedido.qtde-pedida-cliente:SCREEN-VALUE IN FRAME f-fold-1 = ""
           pd-config-pedido.cod-aplicacao:SCREEN-VALUE IN FRAME f-fold-1       = ""
           i-qt-bob-plt:SCREEN-VALUE IN FRAME f-fold-1                         = ""
           i-qt-plt:SCREEN-VALUE IN FRAME f-fold-1                             = ""
           pd-config-pedido.integrado-ems:CHECKED IN FRAME f-fold-1            = no
           pd-config-pedido.cod-prod-cliente:SCREEN-VALUE IN FRAME f-fold-1    = ""
           pd-config-pedido.nr-pedido-cliente:SCREEN-VALUE IN FRAME f-fold-1   = ""
           pd-config-pedido.qtde-ajustada-ped:SCREEN-VALUE IN FRAME f-fold-1   = ""
           pd-config-pedido.qtde-bobinas:SCREEN-VALUE IN FRAME f-fold-1        = ""
           pd-config-pedido.unid-fat-cliente:SCREEN-VALUE IN FRAME f-fold-1    = ""
           pd-config-pedido.preco-unit:SCREEN-VALUE IN FRAME f-fold-1          = ""
           pd-config-pedido.dens-otica:SCREEN-VALUE IN FRAME f-fold-1          = ""
           pd-config-pedido.obs-qualidade:SCREEN-VALUE IN FRAME f-fold-2       = ""
           pd-config-pedido.obs-gerais:SCREEN-VALUE IN FRAME f-fold-2          = ""
           pd-config-pedido.obs-embalagem:SCREEN-VALUE IN FRAME f-fold-2       = "".
           pd-config-pedido.obs-etiqueta:SCREEN-VALUE IN FRAME f-fold-2        = "".

    ASSIGN c-nome-empresa:SCREEN-VALUE IN FRAME f-tela     = ""
           c-nome-abrev:SCREEN-VALUE IN FRAME f-tela       = ""
           c-it-codigo:SCREEN-VALUE IN FRAME f-tela        = ""
           c-cod-estabel:SCREEN-VALUE IN FRAME f-tela      = "".

END PROCEDURE.

PROCEDURE pi-enable-campos.

         
    IF c-tipo-botao <> "altera" THEN
       ENABLE pd-config-pedido.ep-codigo
              pd-config-pedido.nr-pedido
              pd-config-pedido.nr-sequencia
           WITH FRAME f-tela.
                                
    ENABLE pd-config-pedido.largura
           pd-config-pedido.diin
           pd-config-pedido.diex
           pd-config-pedido.qtde-pedida-cliente
           pd-config-pedido.cod-aplicacao
           pd-config-pedido.integrado-ems
           pd-config-pedido.cod-prod-cliente
           pd-config-pedido.nr-pedido-cliente
           pd-config-pedido.unid-fat-cliente
           pd-config-pedido.preco-unit
           pd-config-pedido.dens-otica
           bt-integra-ems
           WITH FRAME f-fold-1.
    
    FIND FIRST b-ped-venda WHERE b-ped-venda.nr-pedido = INT(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela) NO-LOCK NO-ERROR.
    IF AVAIL b-ped-venda THEN DO:
        FIND FIRST b-ped-item OF b-ped-venda WHERE b-ped-item.nr-sequencia = INT(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela) NO-LOCK NO-ERROR.
        IF AVAIL b-ped-item THEN DO:

             FIND FIRST ord-prod WHERE 
                 ord-prod.nome-abrev   = b-ped-item.nome-abrev AND
                 ord-prod.nr-pedido    = b-ped-item.nr-pedcli AND
                 ord-prod.nr-sequencia = b-ped-item.nr-sequencia NO-LOCK NO-ERROR. 

             IF AVAIL ord-prod THEN
                ASSIGN   pd-config-pedido.largura:SENSITIVE IN FRAME f-fold-1 = NO
                         pd-config-pedido.diin:SENSITIVE IN FRAME f-fold-1 = NO
                         pd-config-pedido.diex:SENSITIVE IN FRAME f-fold-1 = NO 
                         i-nr-ord-produ:SCREEN-VALUE IN FRAME f-tela  = string(ord-prod.nr-ord-produ).

                        
        END.
    END.

    IF pd-config-pedido.largura:SENSITIVE IN FRAME f-fold-1 = YES THEN DO:

        FIND FIRST if-ped-venda WHERE
            if-ped-venda.nr-pedido = pd-config-pedido.nr-pedido
            NO-LOCK NO-ERROR.
            
            IF AVAIL if-ped-venda THEN DO:
            
                FIND bf-ped-venda WHERE
                    bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac
                    NO-LOCK NO-ERROR.
            
                IF AVAIL bf-ped-venda THEN DO:
    
                    FIND FIRST ord-prod WHERE 
                     ord-prod.nome-abrev   = bf-ped-venda.nome-abrev AND
                     ord-prod.nr-pedido    = bf-ped-venda.nr-pedcli AND
                     ord-prod.nr-sequencia = INT(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela) NO-LOCK NO-ERROR. 
    
                     IF AVAIL ord-prod THEN
                        ASSIGN   pd-config-pedido.largura:SENSITIVE IN FRAME f-fold-1 = NO
                                 pd-config-pedido.diin:SENSITIVE IN FRAME f-fold-1 = NO
                                 pd-config-pedido.diex:SENSITIVE IN FRAME f-fold-1 = NO
                                 i-nr-ord-produ:SCREEN-VALUE IN FRAME f-tela  = string(ord-prod.nr-ord-produ).

                     
                END.
            END.
    END.

    IF pd-config-pedido.largura:SENSITIVE IN FRAME f-fold-1 = YES THEN DO:

        FIND FIRST if-ped-venda WHERE
            if-ped-venda.nr-pedido-relac = pd-config-pedido.nr-pedido
            NO-LOCK NO-ERROR.
            
            IF AVAIL if-ped-venda THEN DO:
            
                FIND bf-ped-venda WHERE
                    bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido
                    NO-LOCK NO-ERROR.
            
                IF AVAIL bf-ped-venda THEN DO:
    
                    FIND FIRST ord-prod WHERE 
                     ord-prod.nome-abrev   = bf-ped-venda.nome-abrev AND
                     ord-prod.nr-pedido    = bf-ped-venda.nr-pedcli AND
                     ord-prod.nr-sequencia = INT(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela) NO-LOCK NO-ERROR. 
    
                     IF AVAIL ord-prod THEN
                        ASSIGN   pd-config-pedido.largura:SENSITIVE IN FRAME f-fold-1 = NO
                                 pd-config-pedido.diin:SENSITIVE IN FRAME f-fold-1 = NO
                                 pd-config-pedido.diex:SENSITIVE IN FRAME f-fold-1 = NO
                                 i-nr-ord-produ:SCREEN-VALUE IN FRAME f-tela  = string(ord-prod.nr-ord-produ).
                     
                END.
            END.
    END.





    pd-config-pedido.cod-aplicacao:BGCOLOR IN FRAME f-fold-1 = 15.

    ENABLE pd-config-pedido.obs-qualidade
           pd-config-pedido.obs-gerais
           pd-config-pedido.obs-etiqueta
           pd-config-pedido.obs-embalagem
           WITH FRAME f-fold-2.

    IF c-tipo-botao <> "altera" THEN
       APPLY "entry" TO pd-config-pedido.ep-codigo IN FRAME f-tela.
    ELSE
       APPLY "entry" TO pd-config-pedido.largura IN FRAME f-fold-1.

    ASSIGN pd-config-pedido.integrado-ems:SELECTABLE IN FRAME f-fold-1 = yes.


END PROCEDURE.


PROCEDURE pi-disable-campos.

    DISABLE  pd-config-pedido.ep-codigo    
             pd-config-pedido.nr-pedido    
             pd-config-pedido.nr-sequencia 
        WITH FRAME f-tela.

    DISABLE pd-config-pedido.largura             
            pd-config-pedido.diin                
            pd-config-pedido.diex                
            pd-config-pedido.qtde-pedida-cliente 
            pd-config-pedido.cod-aplicacao 
            i-qt-bob-plt
            i-qt-plt
            pd-config-pedido.cod-prod-cliente    
            pd-config-pedido.nr-pedido-cliente   
            pd-config-pedido.integrado-ems 
            pd-config-pedido.unid-fat-cliente    
            pd-config-pedido.preco-unit          
            pd-config-pedido.dens-otica 
        WITH FRAME f-fold-1.

    pd-config-pedido.cod-aplicacao:BGCOLOR IN FRAME f-fold-1 = ?.

    DISABLE pd-config-pedido.obs-qualidade 
            pd-config-pedido.obs-gerais    
            pd-config-pedido.obs-etiqueta 
            pd-config-pedido.obs-embalagem
        WITH FRAME f-fold-2.

END PROCEDURE.

PROCEDURE pi-le-pela-chave.

    FIND FIRST pd-config-pedido WHERE
        pd-config-pedido.ep-codigo    = pd-config-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela AND
        pd-config-pedido.nr-pedido    = int(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela) AND
        pd-config-pedido.nr-sequencia = int(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)
        NO-LOCK NO-ERROR.

    IF AVAIL pd-config-pedido AND (c-tipo-botao2 = "novo" OR c-tipo-botao2 = "copia") THEN DO:

        ASSIGN c-mensagem-jr = "Registro J  Existe".
        RUN pi-mostra-mensagem.
        RETURN "nok".     

    END.

    IF NOT AVAIL pd-config-pedido AND c-tipo-botao2 = "altera" THEN DO:

        ASSIGN c-mensagem-jr = "Registro NÆo Existe".
        RUN pi-mostra-mensagem.
        RETURN "nok".

    END.

    RETURN "ok".

END PROCEDURE.


PROCEDURE pi-grava-registro.


    IF c-tipo-botao2 = "novo" OR c-tipo-botao2 = "copia" THEN DO:

       CREATE pd-config-pedido.
       ASSIGN pd-config-pedido.ep-codigo    = pd-config-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela
              pd-config-pedido.nr-pedido    = int(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)
              pd-config-pedido.nr-sequencia = int(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela).

    END.

    IF c-tipo-botao2 = "altera" THEN DO:

        FIND CURRENT pd-config-pedido EXCLUSIVE-LOCK NO-ERROR.

        IF NOT AVAIL pd-config-pedido THEN DO:
            ASSIGN c-mensagem-jr = "Registro NÆo esta mais disponivel".
            RUN pi-mostra-mensagem.
            RETURN "nok".
        END.

    END.

    ASSIGN pd-config-pedido.largura             = int(pd-config-pedido.largura:SCREEN-VALUE IN FRAME f-fold-1)            
           pd-config-pedido.diin                = int(pd-config-pedido.diin:SCREEN-VALUE IN FRAME f-fold-1)               
           pd-config-pedido.diex                = int(pd-config-pedido.diex:SCREEN-VALUE IN FRAME f-fold-1)               
           pd-config-pedido.qtde-pedida-cliente = dec(pd-config-pedido.qtde-pedida-cliente:SCREEN-VALUE IN FRAME f-fold-1)
           pd-config-pedido.cod-aplicacao       = pd-config-pedido.cod-aplicacao:SCREEN-VALUE IN FRAME f-fold-1      
           pd-config-pedido.integrado-ems       = IF input frame f-fold-1 pd-config-pedido.integrado-ems = YES THEN YES ELSE NO
           pd-config-pedido.cod-prod-cliente    = pd-config-pedido.cod-prod-cliente:SCREEN-VALUE IN FRAME f-fold-1   
           pd-config-pedido.nr-pedido-cliente   = pd-config-pedido.nr-pedido-cliente:SCREEN-VALUE IN FRAME f-fold-1  
           pd-config-pedido.qtde-ajustada-ped   = dec(pd-config-pedido.qtde-ajustada-ped:SCREEN-VALUE IN FRAME f-fold-1)  
           pd-config-pedido.qtde-bobinas        = int(pd-config-pedido.qtde-bobinas:SCREEN-VALUE IN FRAME f-fold-1)       
           pd-config-pedido.unid-fat-cliente    = pd-config-pedido.unid-fat-cliente:SCREEN-VALUE IN FRAME f-fold-1   
           pd-config-pedido.preco-unit          = dec(pd-config-pedido.preco-unit:SCREEN-VALUE IN FRAME f-fold-1)         
           pd-config-pedido.dens-otica          = dec(pd-config-pedido.dens-otica:SCREEN-VALUE IN FRAME f-fold-1)         
           pd-config-pedido.nr-config           = int(pd-config-pedido.nr-config:SCREEN-VALUE IN FRAME f-tela)               
           pd-config-pedido.obs-qualidade       = pd-config-pedido.obs-qualidade:SCREEN-VALUE IN FRAME f-fold-2      
           pd-config-pedido.obs-gerais          = pd-config-pedido.obs-gerais:SCREEN-VALUE IN FRAME f-fold-2         
           pd-config-pedido.obs-embalagem       = pd-config-pedido.obs-embalagem:SCREEN-VALUE IN FRAME f-fold-2    
           pd-config-pedido.obs-etiqueta        = pd-config-pedido.obs-etiqueta:SCREEN-VALUE IN FRAME f-fold-2.

           pd-config-pedido.integrado-ems       = NO.

    FIND CURRENT pd-config-pedido NO-LOCK NO-ERROR.

    pd-config-pedido.integrado-ems:checked IN FRAME f-fold-1 = no.

    run utp/ut-msgs.p (input "show":U, input 701, input string("a‡Æo: Deseja j  integrar configurador"   )).
  

      if return-value = "yes" then APPLY "choose"TO bt-integra-ems IN FRAME f-fold-1.

     

    RETURN "ok".

END PROCEDURE.

PROCEDURE pi-disable-bt-grava.

    assign bt-grava:SENSITIVE in frame f-tela   = no
           bt-cancela:SENSITIVE in frame f-tela = no.

END PROCEDURE.

PROCEDURE pi-enable-bt-grava.

    ENABLE bt-grava bt-cancela
        WITH FRAME f-tela.

END PROCEDURE.

PROCEDURE pi-disable-outros-botoes.

   ASSIGN bt-primeiro:SENSITIVE in frame f-tela   = no  
          bt-anterior:SENSITIVE in frame f-tela   = no  
          bt-proximo:SENSITIVE in frame f-tela    = no   
          bt-final:SENSITIVE in frame f-tela      = no  
          bt-goto:SENSITIVE in frame f-tela       = no 
          bt-pesquisa:SENSITIVE in frame f-tela   = no 
          bt-novo:SENSITIVE in frame f-tela       = no 
          bt-copia:SENSITIVE in frame f-tela      = no 
          bt-altera:SENSITIVE in frame f-tela     = no 
          bt-deleta:SENSITIVE in frame f-tela     = no 
          bt-sai:SENSITIVE in frame f-tela        = no. 
   
END PROCEDURE.

PROCEDURE pi-enable-outros-botoes.

   ENABLE bt-primeiro 
          bt-anterior 
          bt-proximo  
          bt-final 
          bt-goto
          bt-pesquisa
          bt-novo
          bt-copia
          bt-altera
          bt-deleta
          bt-sai
   WITH FRAME f-tela IN WINDOW C-Win.
   
   if index (program-name(1) + program-name(2) + program-name(3) + 
   program-name(4) + program-name(5) + program-name(6) + 
   program-name(7) + program-name(8) + program-name(9), "pd4000") > 0  then DO:

   
    ASSIGN bt-primeiro:SENSITIVE in frame f-tela   = no  
          bt-anterior:SENSITIVE in frame f-tela   = no  
          bt-proximo:SENSITIVE in frame f-tela    = no   
          bt-final:SENSITIVE in frame f-tela      = no  
          bt-goto:SENSITIVE in frame f-tela       = no 
          bt-pesquisa:SENSITIVE in frame f-tela   = no 
          bt-novo:SENSITIVE in frame f-tela       = no 
          bt-copia:SENSITIVE in frame f-tela      = no 
          bt-altera:SENSITIVE in frame f-tela     = no 
          bt-deleta:SENSITIVE in frame f-tela     = no 
         /* bt-sai:SENSITIVE in frame f-tela        = no*/ . 
         
    END.
   
   
END PROCEDURE.


PROCEDURE pi-pesquisa-reg.

       DEFINE BUTTON pq-bt-filtro
           IMAGE FILENAME "image\im-chck1"
           SIZE 8 BY 1.

       DEFINE BUTTON pq-bt-cancel AUTO-END-KEY 
            LABEL "&Cancelar" 
            SIZE 10 BY 1
            BGCOLOR 8.
       
       DEFINE BUTTON pq-bt-ok AUTO-GO 
            LABEL "&OK" 
            SIZE 10 BY 1
            BGCOLOR 8.
       
       DEFINE RECTANGLE pq-rt-botoes
            EDGE-PIXELS 2 GRAPHIC-EDGE  
            SIZE 88 BY 1.42
            BGCOLOR 7.
      
       DEFINE RECTANGLE pq-rect-1
        EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
        SIZE 59 BY 4.50.

       DEFINE VARIABLE i-ep-codigo-ini        AS INT FORMAT ">>9"        INITIAL 9         NO-UNDO.
       DEFINE VARIABLE i-ep-codigo-fim        AS INT FORMAT ">>9"        INITIAL 999       NO-UNDO.
       DEFINE VARIABLE i-nr-pedido-ini        AS INT FORMAT ">>>>>>>>9"  INITIAL 0         NO-UNDO.
       DEFINE VARIABLE i-nr-pedido-fim        AS INT FORMAT ">>>>>>>>9"  INITIAL 999999999 NO-UNDO.
       DEFINE VARIABLE i-nr-sequencia-ini     AS INT FORMAT ">>>>>9"     INITIAL 0         NO-UNDO.
       DEFINE VARIABLE i-nr-sequencia-fim     AS INT FORMAT ">>>>>9"     INITIAL 999999    NO-UNDO.

       DEFINE QUERY br-pesquisa FOR 
             pd-config-pedido SCROLLING.

       DEFINE BROWSE br-pesquisa
         QUERY br-pesquisa DISPLAY
           pd-config-pedido.ep-codigo 
           pd-config-pedido.nr-pedido 
           pd-config-pedido.nr-sequencia 
           pd-config-pedido.largura     FORMAT ">>>>>>>>9"
           pd-config-pedido.diin 
           pd-config-pedido.diex 
           pd-config-pedido.qtde-bobinas 
           pd-config-pedido.qtde-pedida-cliente 
           WITH SEPARATORS SIZE 83 BY 9
           BGCOLOR 15 .

       
       DEFINE FRAME gt-frame-pesq

           i-ep-codigo-ini label "Empresa"
             at row 1 col 20 colon-aligned
             view-as fill-in 
             size 17 by .88
             font 1
           
           
           i-ep-codigo-fim no-label
             at row 1 col 47 colon-aligned
             view-as fill-in 
             size 17 by .88
             font 1
           
           i-nr-pedido-ini label "Nr.do Pedido"
             at row 2 col 20 colon-aligned
             view-as fill-in 
             size 17 by .88
             font 1
           
           
           i-nr-pedido-fim no-label
             at row 2 col 47 colon-aligned
             view-as fill-in 
             size 17 by .88
             font 1
           
           i-nr-sequencia-ini label "Sequencia do Pedido"
             at row 3 col 20 colon-aligned
             view-as fill-in 
             size 17 by .88
             font 1
           
           
           i-nr-sequencia-fim no-label
             at row 3 col 47 colon-aligned
             view-as fill-in 
             size 17 by .88
             font 1
           
           pq-bt-filtro   AT ROW 1     COL 70 HELP "Pesquisa"
           
           IMAGE-1 AT ROW 01.00 COL 40
           IMAGE-2 AT ROW 01.00 COL 45
           
           IMAGE-3 AT ROW 02.00 COL 40
           IMAGE-4 AT ROW 02.00 COL 45
           
           IMAGE-5 AT ROW 03.00 COL 40
           IMAGE-6 AT ROW 03.00 COL 45
           
           br-pesquisa AT ROW 4 COL 1
      
           pq-bt-ok          AT ROW 13.3 COL 2.14
           pq-bt-cancel      AT ROW 13.3 COL 13             
           pq-rt-botoes      AT ROW 13.0 COL 1
           SPACE(0.28)

           WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
                THREE-D SCROLLABLE TITLE "Configura‡Æo de Pedidos" FONT 1
                DEFAULT-BUTTON pq-bt-ok CANCEL-BUTTON pq-bt-cancel.

      

       on "entry" OF i-ep-codigo-ini in frame gt-frame-pesq do:

           CLOSE QUERY br-pesquisa.

           open query br-pesquisa 
               for each pd-config-pedido where
               pd-config-pedido.ep-codigo    >= i-ep-codigo-ini:SCREEN-VALUE IN FRAME gt-frame-pesq     AND
               pd-config-pedido.ep-codigo    <= i-ep-codigo-fim:SCREEN-VALUE IN FRAME gt-frame-pesq     and
               pd-config-pedido.nr-pedido    >= int(i-nr-pedido-ini:SCREEN-VALUE IN FRAME gt-frame-pesq)     AND
               pd-config-pedido.nr-pedido    <= int(i-nr-pedido-fim:SCREEN-VALUE IN FRAME gt-frame-pesq)     AND
               pd-config-pedido.nr-sequencia >= int(i-nr-sequencia-ini:SCREEN-VALUE IN FRAME gt-frame-pesq)  AND
               pd-config-pedido.nr-sequencia <= int(i-nr-sequencia-fim:SCREEN-VALUE IN FRAME gt-frame-pesq).

             if  br-pesquisa:num-selected-rows IN FRAME gt-frame-pesq > 0 then do on error undo, return no-apply:
                 get current br-pesquisa.

                display
                    pd-config-pedido.ep-codigo            
                    pd-config-pedido.nr-pedido            
                    pd-config-pedido.nr-sequencia 
                    pd-config-pedido.largura              
                    pd-config-pedido.diin                 
                    pd-config-pedido.diex                 
                    pd-config-pedido.qtde-pedida-cliente  
                    pd-config-pedido.qtde-bobinas          
                    with browse br-pesquisa. 

             END.

       END.

       ON MOUSE-SELECT-DBLCLICK OF br-pesquisa IN FRAME gt-frame-pesq
       DO:

           APPLY "choose" to pq-bt-ok IN FRAME gt-frame-pesq.  

       END.



       ON "CHOOSE":U OF pq-bt-ok IN FRAME gt-frame-pesq DO:

           if  br-pesquisa:num-selected-rows IN FRAME gt-frame-pesq > 0 then do on error undo, return no-apply:
               get current br-pesquisa.
           
               ASSIGN i-ep-codigo-jr    = pd-config-pedido.ep-codigo
                      i-nr-pedido-jr    = pd-config-pedido.nr-pedido
                      i-nr-sequencia-jr = pd-config-pedido.nr-sequencia.

           end.
           
           ELSE
               ASSIGN i-ep-codigo-jr    = "0"
                      i-nr-pedido-jr    = 0
                      i-nr-sequencia-jr = 0.

           RETURN.
      
       END.

       ON CHOOSE OF pq-bt-filtro IN FRAME gt-frame-pesq
       DO:

           CLOSE QUERY br-pesquisa.
           open query br-pesquisa for each pd-config-pedido where
               pd-config-pedido.ep-codigo    >= i-ep-codigo-ini:SCREEN-VALUE IN FRAME gt-frame-pesq     AND
               pd-config-pedido.ep-codigo    <= i-ep-codigo-fim:SCREEN-VALUE IN FRAME gt-frame-pesq     and
               pd-config-pedido.nr-pedido    >= int(i-nr-pedido-ini:SCREEN-VALUE IN FRAME gt-frame-pesq)     AND
               pd-config-pedido.nr-pedido    <= int(i-nr-pedido-fim:SCREEN-VALUE IN FRAME gt-frame-pesq)     AND
               pd-config-pedido.nr-sequencia >= int(i-nr-sequencia-ini:SCREEN-VALUE IN FRAME gt-frame-pesq)  AND
               pd-config-pedido.nr-sequencia <= int(i-nr-sequencia-fim:SCREEN-VALUE IN FRAME gt-frame-pesq).  

           if  br-pesquisa:num-selected-rows IN FRAME gt-frame-pesq > 0 then do on error undo, return no-apply:

               get current br-pesquisa.

               display
                   pd-config-pedido.ep-codigo            
                   pd-config-pedido.nr-pedido            
                   pd-config-pedido.nr-sequencia  
                   pd-config-pedido.largura                
                   pd-config-pedido.diin                 
                   pd-config-pedido.diex                 
                   pd-config-pedido.qtde-pedida-cliente  
                   pd-config-pedido.qtde-bobinas         
                   with browse br-pesquisa. 

           END.

       END.

       ASSIGN i-ep-codigo-ini:SCREEN-VALUE IN FRAME gt-frame-pesq    = "0"
              i-ep-codigo-fim:SCREEN-VALUE IN FRAME gt-frame-pesq    = "999"
              i-nr-pedido-ini:SCREEN-VALUE IN FRAME gt-frame-pesq    = "0"           
              i-nr-pedido-fim:SCREEN-VALUE IN FRAME gt-frame-pesq    = "999999999"
              i-nr-sequencia-ini:SCREEN-VALUE IN FRAME gt-frame-pesq = "0"           
              i-nr-sequencia-fim:SCREEN-VALUE IN FRAME gt-frame-pesq = "999999".           


      
       ENABLE i-ep-codigo-ini    i-ep-codigo-fim 
              i-nr-pedido-ini    i-nr-pedido-fim 
              i-nr-sequencia-ini i-nr-sequencia-fim
              pq-bt-ok pq-bt-cancel 
              pq-bt-filtro br-pesquisa 
           WITH FRAME gt-frame-pesq. 
       
       WAIT-FOR "GO":U OF FRAME gt-frame-pesq.


END PROCEDURE.


PROCEDURE pi-pesquisa-aplic.

       DEFINE BUTTON pq-aplic-bt-filtro
           IMAGE FILENAME "image\im-chck1"
           SIZE 8 BY 1.

       DEFINE BUTTON pq-aplic-bt-cancel AUTO-END-KEY 
            LABEL "&Cancelar" 
            SIZE 10 BY 1
            BGCOLOR 8.
       
       DEFINE BUTTON pq-aplic-bt-ok AUTO-GO 
            LABEL "&OK" 
            SIZE 10 BY 1
            BGCOLOR 8.
       
       DEFINE RECTANGLE pq-aplic-rt-botoes
            EDGE-PIXELS 2 GRAPHIC-EDGE  
            SIZE 88 BY 1.42
            BGCOLOR 7.
      
       DEFINE RECTANGLE pq-aplic-rect-1
        EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
        SIZE 59 BY 4.50.

       DEFINE VARIABLE c-cod-aplic-ini        AS CHAR FORMAT "x(60)"     INITIAL ""                     NO-UNDO.
       DEFINE VARIABLE c-cod-aplic-fim        AS CHAR FORMAT "x(60)"     INITIAL "ZZZZZZZZZZZZZZZZZZZZ" NO-UNDO.

       DEFINE QUERY br-pesquisa FOR 
             c-tab-res SCROLLING.

       DEFINE BROWSE br-pesquisa
         QUERY br-pesquisa DISPLAY
           c-tab-res.nr-tabela 
           c-tab-res.sequencia 
           c-tab-res.descricao    FORMAT "x(95)"
           WITH SEPARATORS SIZE 83 BY 9
           BGCOLOR 15 .

       
       DEFINE FRAME gt-frame-pesq-aplic

           c-cod-aplic-ini label "Aplica‡Æo"
             at row 1 col 20 colon-aligned
             view-as fill-in 
             size 17 by .88
             font 1
           
           
           c-cod-aplic-fim no-label
             at row 1 col 47 colon-aligned
             view-as fill-in 
             size 17 by .88
             font 1
           
           
           pq-aplic-bt-filtro   AT ROW 1     COL 70 HELP "Pesquisa"
           
           IMAGE-1 AT ROW 01.00 COL 40
           IMAGE-2 AT ROW 01.00 COL 45
           
           
           br-pesquisa AT ROW 2 COL 1
      
           pq-aplic-bt-ok          AT ROW 13.3 COL 2.14
           pq-aplic-bt-cancel      AT ROW 13.3 COL 13             
           pq-aplic-rt-botoes      AT ROW 13.0 COL 1
           SPACE(0.28)

           WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
                THREE-D SCROLLABLE TITLE "Aplica‡Æo de Produtos" FONT 1
                DEFAULT-BUTTON pq-aplic-bt-ok CANCEL-BUTTON pq-aplic-bt-cancel.

      ON 'value-changed':U OF c-cod-aplic-ini IN FRAME gt-frame-pesq-aplic
DO:
          FIND FIRST var-pad-cf WHERE
          var-pad-cf.nome-var = "APLIC" NO-LOCK NO-ERROR.

      IF NOT AVAIL var-pad-cf THEN
          RETURN NO-APPLY.

      CLOSE QUERY br-pesquisa.

      open query br-pesquisa 
          for each c-tab-res where
          c-tab-res.nr-tabela     = var-pad-cf.nr-tabela                                               AND
          c-tab-res.descricao    >= c-cod-aplic-ini:SCREEN-VALUE IN FRAME gt-frame-pesq-aplic          AND
          c-tab-res.descricao    <= c-cod-aplic-fim:SCREEN-VALUE IN FRAME gt-frame-pesq-aplic.  

        if  br-pesquisa:num-selected-rows IN FRAME gt-frame-pesq-aplic > 0 then do on error undo, return no-apply:
            get current br-pesquisa.

           display
               c-tab-res.nr-tabela         
               c-tab-res.sequencia         
               c-tab-res.descricao 
               with browse br-pesquisa. 

        END.

        

END.

       on "entry" OF c-cod-aplic-ini in frame gt-frame-pesq-aplic do:

                     FIND FIRST var-pad-cf WHERE
               var-pad-cf.nome-var = "APLIC" NO-LOCK NO-ERROR.

           IF NOT AVAIL var-pad-cf THEN
               RETURN NO-APPLY.

           CLOSE QUERY br-pesquisa.

           open query br-pesquisa 
               for each c-tab-res where
               c-tab-res.nr-tabela     = var-pad-cf.nr-tabela                                               AND
               c-tab-res.descricao    >= c-cod-aplic-ini:SCREEN-VALUE IN FRAME gt-frame-pesq-aplic          AND
               c-tab-res.descricao    <= c-cod-aplic-fim:SCREEN-VALUE IN FRAME gt-frame-pesq-aplic.  

             if  br-pesquisa:num-selected-rows IN FRAME gt-frame-pesq-aplic > 0 then do on error undo, return no-apply:
                 get current br-pesquisa.

                display
                    c-tab-res.nr-tabela         
                    c-tab-res.sequencia         
                    c-tab-res.descricao 
                    with browse br-pesquisa. 

             END.


       END.

       ON MOUSE-SELECT-DBLCLICK OF br-pesquisa IN FRAME gt-frame-pesq-aplic
       DO:

           APPLY "choose" to pq-aplic-bt-ok IN FRAME gt-frame-pesq-aplic.  

       END.



       ON "CHOOSE":U OF pq-aplic-bt-ok IN FRAME gt-frame-pesq-aplic DO:

           if  br-pesquisa:num-selected-rows IN FRAME gt-frame-pesq-aplic > 0 then do on error undo, return no-apply:
               get current br-pesquisa.
           
               ASSIGN pd-config-pedido.cod-aplicacao:SCREEN-VALUE IN FRAME f-fold-1 = c-tab-res.descricao.

           end.
           
           RETURN.
      
       END.

       ON CHOOSE OF pq-aplic-bt-filtro IN FRAME gt-frame-pesq-aplic
       DO:

           CLOSE QUERY br-pesquisa.
           open query br-pesquisa 
               for each c-tab-res where
               c-tab-res.nr-tabela     = var-pad-cf.nr-tabela                                               AND
               c-tab-res.descricao    >= c-cod-aplic-ini:SCREEN-VALUE IN FRAME gt-frame-pesq-aplic          AND
               c-tab-res.descricao    <= c-cod-aplic-fim:SCREEN-VALUE IN FRAME gt-frame-pesq-aplic. 

           if  br-pesquisa:num-selected-rows IN FRAME gt-frame-pesq-aplic > 0 then do on error undo, return no-apply:

               get current br-pesquisa.

               display
                       c-tab-res.nr-tabela         
                       c-tab-res.sequencia         
                       c-tab-res.descricao 
                       with browse br-pesquisa. 

           END.

       END.

       ASSIGN c-cod-aplic-ini:SCREEN-VALUE IN FRAME gt-frame-pesq-aplic    = " "
              c-cod-aplic-fim:SCREEN-VALUE IN FRAME gt-frame-pesq-aplic    = "ZZZZZZZZZZZZZZZZZZZZ".

       ENABLE c-cod-aplic-ini    c-cod-aplic-fim 
              pq-aplic-bt-ok pq-aplic-bt-cancel 
              pq-aplic-bt-filtro br-pesquisa 
           WITH FRAME gt-frame-pesq-aplic. 
       
       WAIT-FOR "GO":U OF FRAME gt-frame-pesq-aplic.

END PROCEDURE.

PROCEDURE pi-calculo-formulas.

    ASSIGN diex-jr       = INT(pd-config-pedido.diex:SCREEN-VALUE IN FRAME f-fold-1)
           diin-jr       = INT(pd-config-pedido.diin:SCREEN-VALUE IN FRAME f-fold-1)
           larg-jr       = INT(pd-config-pedido.largura:SCREEN-VALUE IN FRAME f-fold-1)
           qtd-pedida-jr = DEC(pd-config-pedido.qtde-pedida-cliente:SCREEN-VALUE IN FRAME f-fold-1).

    ASSIGN dens-bob = 0.8778.

    /* Acha densidade da bobina para o item */

    FIND FIRST var-modelo WHERE 
         var-modelo.mo-codigo = c-it-codigo:SCREEN-VALUE IN FRAME f-tela AND
         var-modelo.nome-var  = "PESOBOB" NO-LOCK NO-ERROR.
    
    IF AVAIL var-modelo THEN DO:
    
       FIND FIRST var-mod-form
           WHERE var-mod-form.mo-codigo = var-modelo.mo-codigo
           AND   var-mod-form.sequencia = var-modelo.sequencia NO-LOCK NO-ERROR.
    
       IF AVAIL var-mod-form THEN DO:
    
           IF var-mod-form.formula MATCHES "*dens001*" THEN DO:
    
               FIND constante WHERE constante.nome-cte = "dens001" NO-LOCK NO-ERROR.
    
                  IF AVAIL constante THEN
                      ASSIGN dens-bob = decimal(constante.valor-dec).
    
           END.

           ELSE IF var-mod-form.formula MATCHES "*dens002*" THEN DO:
    
                FIND constante WHERE constante.nome-cte = "dens002" NO-LOCK NO-ERROR.
    
                  IF AVAIL constante THEN
                      ASSIGN dens-bob = decimal(constante.valor-dec).
    
           END.
    
           ELSE IF var-mod-form.formula MATCHES "*dens003*" THEN DO: 
    
               FIND constante WHERE constante.nome-cte = "dens003" NO-LOCK NO-ERROR.
    
               IF AVAIL constante THEN
                   ASSIGN dens-bob = decimal(constante.valor-dec).
    
           END.
    
           ELSE IF var-mod-form.formula MATCHES "*dsvg002*" THEN DO: 
    
                  FIND constante WHERE constante.nome-cte = "dsvg002" NO-LOCK NO-ERROR.
    
                  IF AVAIL constante THEN
                      ASSIGN dens-bob = decimal(constante.valor-dec).
    
           END.
    
       END.
    
    END.

    /* Fim do Acha densidade da bobina para o item */

    ASSIGN peso-bob-jr = ((((diex-jr * diex-jr) - (diin-jr * diin-jr))
                             * 3.14159265) / 4)
                             * ((larg-jr * dens-bob) / 1000) / 1000.

    ASSIGN qtdbob-jr = TRUNC(((qtd-pedida-jr / peso-bob-jr) + 0.99999999), 0)
           pd-config-pedido.qtde-bobinas:SCREEN-VALUE IN FRAME f-fold-1 = string(qtdbob-jr)
           pd-config-pedido.qtde-ajustada-ped:SCREEN-VALUE IN FRAME f-fold-1 = string(qtdbob-jr * peso-bob-jr)
           pd-config-pedido.dec-1:SCREEN-VALUE IN FRAME f-fold-1 = string(peso-bob-jr).

    /* Acha oo c¢digo de embalagem da mesma forma com foi feita no 
       configurado padrÆo da Datasul (f¢rmula) e c lcula a quantidade
       de bobina por pallet e a quantidade de pallet.  */

    if  c-embal-jr = "" then do:

    if diin-jr = 153 and  diex-jr >= 641 and diex-jr <= 880  and  larg-jr >= 801  and larg-jr <= 1050  then assign c-embal-jr = "102026". else 
    if diin-jr = 153 and  diex-jr >= 641 and diex-jr <= 880  and  larg-jr >= 351  and larg-jr <= 450   then assign c-embal-jr = "102046". else 
    if diin-jr = 153 and  diex-jr >= 200 and diex-jr <= 440  and  larg-jr >= 801  and larg-jr <= 1050  then assign c-embal-jr = "102086". else 
    if diin-jr = 153 and  diex-jr >= 200 and diex-jr <= 440  and  larg-jr >= 351  and larg-jr <= 450   then assign c-embal-jr = "102166". else 
    if diin-jr = 153 and  diex-jr >= 641 and diex-jr <= 880  and  larg-jr >= 1051 and larg-jr <= 1250  then assign c-embal-jr = "103026". else 
    if diin-jr = 153 and  diex-jr >= 641 and diex-jr <= 880  and  larg-jr >= 451  and larg-jr <= 550   then assign c-embal-jr = "103046". else 
    if diin-jr = 153 and  diex-jr >= 200 and diex-jr <= 440  and  larg-jr >= 451  and larg-jr <= 550   then assign c-embal-jr = "103166". else 
    if diin-jr = 153 and  diex-jr >= 381 and diex-jr <= 535  and  larg-jr >= 551  and larg-jr <= 800   then assign c-embal-jr = "104066". else 
    if diin-jr = 153 and  diex-jr >= 441 and diex-jr <= 535  and  larg-jr >= 801  and larg-jr <= 1000  then assign c-embal-jr = "105066". else 
    if diin-jr = 153 and  diex-jr >= 441 and diex-jr <= 535  and  larg-jr >= 200  and larg-jr <= 440   then assign c-embal-jr = "105126". else 
    if diin-jr = 153 and  diex-jr >= 441 and diex-jr <= 535  and  larg-jr >= 1001 and larg-jr <= 1250  then assign c-embal-jr = "106066". else 
    if diin-jr = 153 and  diex-jr >= 441 and diex-jr <= 535  and  larg-jr >= 441  and larg-jr <= 550   then assign c-embal-jr = "106126". else 
    if diin-jr = 153 and  diex-jr >= 441 and diex-jr <= 535  and  larg-jr >= 1251 and larg-jr <= 1460  then assign c-embal-jr = "107066". else 
    if diin-jr = 153 and  diex-jr >= 441 and diex-jr <= 535  and  larg-jr >= 1461 and larg-jr <= 1700  then assign c-embal-jr = "108066". else 
    if diin-jr = 77  and  diex-jr >= 200 and diex-jr <= 380  and  larg-jr >= 601  and larg-jr <= 800   then assign c-embal-jr = "109123". else 
    if diin-jr = 153 and  diex-jr >= 566 and diex-jr <= 585  and  larg-jr >= 801  and larg-jr <= 1050  then assign c-embal-jr = "110066". else 
    if diin-jr = 77  and  diex-jr >= 200 and diex-jr <= 380  and  larg-jr >= 801  and larg-jr <= 1050  then assign c-embal-jr = "110123". else 
    if diin-jr = 153 and  diex-jr >= 566 and diex-jr <= 585  and  larg-jr >= 1051 and larg-jr <= 1250  then assign c-embal-jr = "111066". else 
    if diin-jr = 77  and  diex-jr >= 200 and diex-jr <= 380  and  larg-jr >= 1051 and larg-jr <= 1250  then assign c-embal-jr = "111123". else 
    if diin-jr = 153 and  diex-jr >= 586 and diex-jr <= 640  and  larg-jr >= 471  and larg-jr <= 1050  then assign c-embal-jr = "112046". else 
    if diin-jr = 153 and  diex-jr >= 586 and diex-jr <= 640  and  larg-jr >= 300  and larg-jr <= 470   then assign c-embal-jr = "112086". else 
    if diin-jr = 153 and  diex-jr >= 641 and diex-jr <= 880  and  larg-jr >= 551  and larg-jr <= 800   then assign c-embal-jr = "113026". else 
    if diin-jr = 153 and  diex-jr >= 200 and diex-jr <= 440  and  larg-jr >= 551  and larg-jr <= 800   then assign c-embal-jr = "113086". else 
    if diin-jr = 153 and  diex-jr >= 566 and diex-jr <= 585  and  larg-jr >= 1251 and larg-jr <= 1460  then assign c-embal-jr = "114066". else 
    if diin-jr = 153 and  diex-jr >= 566 and diex-jr <= 585  and  larg-jr >= 1461 and larg-jr <= 1700  then assign c-embal-jr = "115066". else 
    if diin-jr = 77  and  diex-jr >= 441 and diex-jr <= 535  and  larg-jr >= 551  and larg-jr <= 800   then assign c-embal-jr = "104063". else 
    if diin-jr = 153 and  diex-jr >= 881 and diex-jr <= 1000 and  larg-jr >= 551  and larg-jr <= 800   then assign c-embal-jr = "104016". else 
    if diin-jr = 77  and  diex-jr >= 381 and diex-jr <= 535  and  larg-jr >= 200  and larg-jr <= 440   then assign c-embal-jr = "105123". else 
    if diin-jr = 77  and  diex-jr >= 381 and diex-jr <= 535  and  larg-jr >= 801  and larg-jr <= 1000  then assign c-embal-jr = "105063". else 
    if diin-jr = 153 and  diex-jr >= 881 and diex-jr <= 1000 and  larg-jr >= 801  and larg-jr <= 1000  then assign c-embal-jr = "105016". else 
    if diin-jr = 77  and  diex-jr >= 381 and diex-jr <= 535  and  larg-jr >= 1001 and larg-jr <= 1250  then assign c-embal-jr = "106063". else 
    if diin-jr = 77  and  diex-jr >= 381 and diex-jr <= 535  and  larg-jr >= 441  and larg-jr <= 550   then assign c-embal-jr = "106123". else 
    if diin-jr = 153 and  diex-jr >= 881 and diex-jr <= 1000 and  larg-jr >= 1001 and larg-jr <= 1250  then assign c-embal-jr = "106016". else 
    if diin-jr = 77  and  diex-jr >= 381 and diex-jr <= 535  and  larg-jr >= 1251 and larg-jr <= 1460  then assign c-embal-jr = "107063". else 
    if diin-jr = 153 and  diex-jr >= 881 and diex-jr <= 1000 and  larg-jr >= 1251 and larg-jr <= 1460  then assign c-embal-jr = "107016". else 
    if diin-jr = 77  and  diex-jr >= 381 and diex-jr <= 535  and  larg-jr >= 1461 and larg-jr <= 1700  then assign c-embal-jr = "108063". else 
    if diin-jr = 153 and  diex-jr >= 881 and diex-jr <= 1000 and  larg-jr >= 1461 and larg-jr <= 1700  then assign c-embal-jr = "108016". else 
    if diin-jr = 153 and  diex-jr >= 566 and diex-jr <= 585  and  larg-jr >= 400  and larg-jr <= 800   then assign c-embal-jr = "109066". else 
    if diin-jr = 77  and  diex-jr >= 200 and diex-jr <= 380  and  larg-jr >= 401  and larg-jr <= 500   then assign c-embal-jr = "110243". else 
    if diin-jr = 77  and  diex-jr >= 200 and diex-jr <= 380  and  larg-jr >= 501  and larg-jr <= 600   then assign c-embal-jr = "111243". else 
    if diin-jr = 77  and  diex-jr >= 200 and diex-jr <= 380  and  larg-jr >= 1251 and larg-jr <= 1460  then assign c-embal-jr = "114123". else 
    if diin-jr = 77  and  diex-jr >= 200 and diex-jr <= 380  and  larg-jr >= 1461 and larg-jr <= 1700  then assign c-embal-jr = "115123". else 
    if diin-jr = 153 and  diex-jr >= 536 and diex-jr <= 565  and  larg-jr >= 801  and larg-jr <= 1060  then assign c-embal-jr = "116066". else 
    if diin-jr = 153 and  diex-jr >= 536 and diex-jr <= 565  and  larg-jr >= 1061 and larg-jr <= 1220  then assign c-embal-jr = "117066". else 
    if diin-jr = 77  and  diex-jr >= 200 and diex-jr <= 380  and  larg-jr >= 200  and larg-jr <= 400   then assign c-embal-jr = "111363". else 
    if diin-jr = 153 and  diex-jr >= 536 and diex-jr <= 565  and  larg-jr >= 581  and larg-jr <= 800   then assign c-embal-jr = "118066". else 
    if diin-jr = 153 and  diex-jr >= 536 and diex-jr <= 565  and  larg-jr >= 400  and larg-jr <= 500   then assign c-embal-jr = "116126". else 
    if diin-jr = 153 and  diex-jr >= 536 and diex-jr <= 565  and  larg-jr >= 501  and larg-jr <= 580   then assign c-embal-jr = "117126". else 
    if diin-jr = 153 and  diex-jr >= 566 and diex-jr <= 585  and  larg-jr >= 1701 and larg-jr <= 1850  then assign c-embal-jr = "119066". 
end.
    IF qtdbob-jr > 0 AND c-embal-jr <> "" THEN 
       ASSIGN i-qt-bob-plt = int(SUBSTRING(c-embal-jr,4,2))
              i-qt-bob-plt:SCREEN-VALUE IN FRAME f-fold-1 = string(i-qt-bob-plt) 
              i-qt-plt = TRUNC(((qtdbob-jr / i-qt-bob-plt) + 0.99999999), 0)
              i-qt-plt:SCREEN-VALUE IN FRAME f-fold-1 = string(i-qt-plt).
    ELSE 
        ASSIGN i-qt-bob-plt:SCREEN-VALUE IN FRAME f-fold-1 = "" 
               i-qt-plt:SCREEN-VALUE IN FRAME f-fold-1     = "".

    /* ------------------------------------------------------------------ */
    
END PROCEDURE.

PROCEDURE pi-acha-embal:
    
 DEF INPUT PARAMETER p-item   AS CHAR NO-UNDO.
 DEF INPUT PARAMETER p-config AS INT NO-UNDO.
 def var c-narr as char no-undo.
 

/*forca uma alteracao na tabela cot-est-mast para disparar a trigger de embalagem*/

      FOR EACH cot-est-mast WHERE 
          cot-est-mast.item-cotacao = p-item AND
          cot-est-mast.nr-estrut    = p-config    .
    
           
              ASSIGN cot-est-mast.int-1 = cot-est-mast.int-1 + 1
                     cot-est-mast.descricao =  string(ped-item.nr-sequencia) + "/" + STRING(ped-venda.nr-pedido) + "/" + ped-venda.nome-abrev + "/" +  "POLO".
             
          
      END.
      
      c-narr = "".
      FOR EACH cot-est-mast WHERE 
      cot-est-mast.item-cotacao = p-item AND
      cot-est-mast.nr-estrut    = p-config    .

       
          ASSIGN cot-est-mast.int-1 = cot-est-mast.int-1 - 1
              cot-est-mast.descricao =  string(ped-item.nr-sequencia) + "/" + STRING(ped-venda.nr-pedido) + "/" + ped-venda.nome-abrev  .
        
          
      END.

      FOR first cot-est-mast WHERE 
          cot-est-mast.item-cotacao = p-item AND
          cot-est-mast.nr-estrut    = p-config    .

       
          
         for each var-result of cot-est-mast no-lock.
              c-narr = c-narr + var-result.nome-var + ":" + (if var-result.valor-dec <> 0 then string(var-result.valor-dec) else if var-result.des-result <> '' then var-result.des-result else var-result.valor-char ) + "; ". 
         end.
             
             cot-est-mast.narrativa = c-narr.
          
      END.

      FIND FIRST referencia WHERE referencia.cod-refer = STRING(p-config,"99999999") NO-ERROR.

      IF NOT AVAIL referencia THEN DO:
          CREATE referencia.
          ASSIGN referencia.cod-refer = STRING(p-config,"99999999")
                 referencia.descricao = "Pd:" + ped-item.nr-pedcli + "/" + ped-item.nome-abrev + ";" + "It.:"+ ped-item.it-codigo. 
      END.

      find first ref-item where 
            ref-item.cod-refer = STRING(p-config,"99999999") and
            ref-item.it-codigo = p-item no-error.

      if not avail ref-item then do:
            create ref-item.
            assign  ref-item.cod-refer = STRING(p-config,"99999999") 
            ref-item.it-codigo = p-item.
      end.


END PROCEDURE.

PROCEDURE CalculaVarFormula:
    
 DEF INPUT PARAMETER p-item   AS CHAR NO-UNDO.
 DEF INPUT PARAMETER p-config AS INT NO-UNDO.

    for each tt-variavel:
      delete tt-variavel.
    end.
    
    for each tt-var:
      delete tt-var.
    end.
    
    run CarregaConstantes (input-output table tt-variavel).
    
    for each tt-variavel: 
        
          find first tt-var 
               where tt-var.nome = tt-variavel.nome-var no-error.
          if not avail tt-var then
              create tt-var.
              assign tt-var.nome      = tt-variavel.nome-cte
                     tt-var.valor     = tt-variavel.valor-dec
                     tt-var.texto     = tt-variavel.valor-char
                     tt-var.nr-tabela = 0
                     tt-var.tipo-res  = tt-variavel.tipo-result
                     tt-var.abrev-nar = tt-variavel.abreviatura
                     tt-var.sequencia = tt-variavel.seq-var.
       
    end.
  
    for each var-result where 
             var-result.nr-estrut    = p-config 
        AND  var-result.item-cotacao = p-item  NO-LOCK: 

        find first var-modelo
             where var-modelo.mo-codigo = var-result.mo-codigo
               and var-modelo.nome-var  = var-result.nome-var no-lock no-error.

        IF (var-modelo.tipo-result > 3 AND 
            var-modelo.tipo-result < 7) OR
            var-modelo.tipo-result = 8 THEN DO:
            find var-mod-form where
                 var-mod-form.mo-codigo = var-modelo.mo-codigo and
                 var-mod-form.sequencia = var-modelo.sequencia no-lock no-error.
            IF AVAIL var-mod-form THEN DO:
                 create tt-var.
                 assign tt-var.nome      = var-modelo.nome-var
                        tt-var.valor     = var-result.valor-dec 
                        tt-var.texto     = var-result.valor-char 
                        tt-var.nr-tabela = 0
                        tt-var.tipo-res  = var-modelo.tipo-result
                        tt-var.abrev-nar = var-modelo.abreviatura
                        tt-var.sequencia = var-mod-form.sequencia.

                 /*cria tt com variaveis formulas*/
                 CREATE tt-var-form.
                 ASSIGN tt-var-form.nome-var     = var-modelo.nome-var
                        tt-var-form.sequencia    = var-modelo.sequencia
                        tt-var-form.tipo-result  = var-modelo.tipo-result
                        tt-var-form.formula      = var-mod-form.formula.
                
            END. /*var-mod-form*/
            
        END.
        ELSE DO:
           create tt-var.
           assign tt-var.nome      = var-modelo.nome-var
                  tt-var.valor     = var-result.valor-dec
                  tt-var.texto     = var-result.valor-char
                  tt-var.nr-tabela = 0
                  tt-var.tipo-res  = var-modelo.tipo-result
                  tt-var.abrev-nar = var-modelo.abreviatura
                  tt-var.sequencia = var-modelo.sequencia.
        END.
    END.
    /*Api de formula*/
    FOR EACH tt-var-form  NO-LOCK 
          BY tt-var-form.sequencia:
/*
        IF (tt-var-form.nome-var = "PESOBOB" OR
            tt-var-form.nome-var = "QTDBOB" OR
            tt-var-form.nome-var = "QTDCALC") THEN NEXT.
*/
        for each tt-formula:
            delete tt-formula.
        end.   
    
        create tt-formula.
        assign tt-formula.formula               = tt-var-form.formula
               tt-formula.tipo-res              = if tt-var-form.tipo-result = 8 then 2
                                                                                else 1
               tt-formula.cod-versao-integracao = 001.

        run cdp/cdapi020a.p (input-output table tt-formula,
                             input        table tt-var,
                             input-output table tt-erro,
                             input        yes).
        
        IF CAN-FIND(FIRST tt-erro) THEN RETURN "NOK".
         
        find first tt-formula.   
        IF AVAIL tt-formula THEN DO:

            IF tt-var-form.tipo-result = 5 THEN 
                 ASSIGN tt-formula.RESULT = TRUNCATE (tt-formula.RESULT, 0). 
             
            /*Atualiza resultado e tt-var*/
            FIND var-result 
                WHERE var-result.item-cotacao = p-item      
                  AND var-result.nr-estrut    = p-config 
                  AND var-result.nome-var     = tt-var-form.nome-var  EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL var-result THEN DO:
                ASSIGN var-result.valor-char = tt-formula.texto.
                       /*var-result.des-result = STRING(tt-formula.RESULT).*/
                if var-result.tipo-result <> 2 and 
                   var-result.tipo-result <> 3 and
                   var-result.tipo-result <> 8 then do:
                     assign var-result.des-result = trim(string(tt-formula.RESULT, "->>>>>>>9.9<<<<<"))
                            var-result.valor-dec  = tt-formula.RESULT. 
                                
                end.                             
                else  assign var-result.des-result = tt-formula.texto. 
                
                /*atualiza valor na tt-var das vari veis recalculadas*/
                FIND FIRST tt-var WHERE
                           tt-var.nome  = tt-var-form.nome-var EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL tt-var THEN
                        ASSIGN tt-var.valor = var-result.valor-dec 
                               tt-var.texto = var-result.valor-char. 
            END.
            
        END.
    END.

    RETURN "OK".

END PROCEDURE.

PROCEDURE CarregaConstantes :
def input-output param table for tt-variavel.

   def buffer b-tt-variavel for tt-variavel.

   for each constante
      where constante.ind-ativa = yes
      no-lock:

      find last b-tt-variavel no-error.

      create tt-variavel.
      assign tt-variavel.seq-mod        = 0
             tt-variavel.seq-var        = if avail b-tt-variavel then
                                             b-tt-variavel.seq-var + 1
                                          else 1
             tt-variavel.c-config       = "*"
             tt-variavel.mo-codigo      = ""
             tt-variavel.nome-var       = ""
             tt-variavel.nome-cte       = constante.nome-cte
             tt-variavel.sequencia-var  = 0
             tt-variavel.sequencia-res  = 10
             tt-variavel.descricao      = constante.descricao
             tt-variavel.tipo-valor     = 1
             tt-variavel.valor-char     = ""
             tt-variavel.valor-dec      = constante.valor-dec
             tt-variavel.abreviatura    = ""
             tt-variavel.gera-narrativa = no
             tt-variavel.ind-tipo-var   = 0
             tt-variavel.tipo-result    = 1
             tt-variavel.nr-tabela      = 0
             tt-variavel.endereco       = rowid(constante).
   end.
   
   RETURN "OK":U.

END PROCEDURE.


PROCEDURE pi-busca-config-atual.
    FIND FIRST ped-venda WHERE
        ped-venda.nr-pedido    = int(pd-config-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)  NO-LOCK NO-ERROR.
     IF AVAIL ped-venda THEN
     FIND FIRST ped-item OF ped-venda WHERE
             ped-item.nr-sequencia = int(pd-config-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)      AND
             ped-item.ind-componen < 3 NO-LOCK NO-ERROR.

      IF AVAIL ped-item AND int(ped-item.cod-refer) <> 1 THEN DO:

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "Largura"           AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.Largura:SCREEN-VALUE IN FRAME f-fold-1 = STRING(var-result.valor-dec).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "diin"              AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.diin:SCREEN-VALUE IN FRAME f-fold-1 = STRING(var-result.valor-dec).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "diex"              AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.diex:SCREEN-VALUE IN FRAME f-fold-1 = STRING(var-result.valor-dec).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "qtdpedido"         AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.qtde-pedida-cliente:SCREEN-VALUE IN FRAME f-fold-1 = STRING(var-result.valor-dec).
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "aplic"             AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.cod-aplicacao:SCREEN-VALUE IN FRAME f-fold-1 = STRING(var-result.valor-char).
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "prodclie"          AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.cod-prod-cliente:SCREEN-VALUE IN FRAME f-fold-1 = STRING(var-result.valor-char).
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "pedcli"            AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.nr-pedido-cliente:SCREEN-VALUE IN FRAME f-fold-1 = STRING(var-result.valor-char).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "obscq"             AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.obs-qualidade:SCREEN-VALUE IN FRAME f-fold-2 = STRING(var-result.valor-char).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "obsger"            AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.obs-gerais:SCREEN-VALUE IN FRAME f-fold-2 = STRING(var-result.valor-char).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "obsetq"            AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.obs-etiqueta:SCREEN-VALUE IN FRAME f-fold-2 = STRING(var-result.valor-char).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "obsemb"            AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.obs-embalagem:SCREEN-VALUE IN FRAME f-fold-2 = STRING(var-result.valor-char).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "qtdbob"            AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.qtde-bobinas:SCREEN-VALUE IN FRAME f-fold-1 = STRING(var-result.valor-dec).
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "qtdcalc"           AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.qtde-ajustada-ped:SCREEN-VALUE IN FRAME f-fold-1 = STRING(var-result.valor-dec).
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "pesobob"           AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.dec-1:SCREEN-VALUE IN FRAME f-fold-1 = STRING(var-result.valor-dec).
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "densotic"           AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.dens-otica:SCREEN-VALUE IN FRAME f-fold-1 = STRING(var-result.valor-dec).
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "unfat"            AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.unid-fat-cliente:SCREEN-VALUE IN FRAME f-fold-1 = STRING(var-result.valor-char).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "preun"             AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.preco-unit:SCREEN-VALUE IN FRAME f-fold-1 = STRING(var-result.valor-dec).
        
      END.

      DISABLE bt-integra-ems WITH FRAME f-fold-1.

END PROCEDURE.

PROCEDURE pi-cria-config-atual.

     IF AVAIL ped-item AND int(ped-item.cod-refer) <> 1 THEN DO:

         FIND FIRST var-result WHERE 
              var-result.nome-var     = "bobpalete"         AND 
              var-result.nr-estrut    = int(ped-item.cod-refer)  AND
              var-result.item-cotacao = ped-item.it-codigo           
              NO-LOCK NO-ERROR.

         IF AVAIL var-result THEN
             ASSIGN i-qt-bob-plt = var-result.valor-dec.

         FIND FIRST var-result WHERE 
              var-result.nome-var     = "qtdpalete"         AND 
              var-result.nr-estrut    = int(ped-item.cod-refer)  AND
              var-result.item-cotacao = ped-item.it-codigo           
              NO-LOCK NO-ERROR.

         IF AVAIL var-result THEN
             ASSIGN i-qt-plt = var-result.valor-dec.

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "Largura"           AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.Largura = var-result.valor-dec.

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "diin"              AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.diin = var-result.valor-dec.

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "diex"              AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.diex = var-result.valor-dec.

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "qtdpedido"         AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.qtde-pedida-cliente = var-result.valor-dec.
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "aplic"             AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.cod-aplicacao = STRING(var-result.valor-char).
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "prodclie"          AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.cod-prod-cliente = STRING(var-result.valor-char).
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "pedcli"            AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.nr-pedido-cliente = STRING(var-result.valor-char).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "obscq"             AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.obs-qualidade:SCREEN-VALUE IN FRAME f-fold-2 = STRING(var-result.valor-char).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "obsger"            AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.obs-gerais:SCREEN-VALUE IN FRAME f-fold-2 = STRING(var-result.valor-char).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "obsetq"            AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.obs-etiqueta:SCREEN-VALUE IN FRAME f-fold-2 = STRING(var-result.valor-char).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "obsemb"            AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.obs-embalagem:SCREEN-VALUE IN FRAME f-fold-2 = STRING(var-result.valor-char).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "qtdbob"            AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.qtde-bobinas = var-result.valor-dec.
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "qtdcalc"           AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.qtde-ajustada-ped = var-result.valor-dec.
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "pesobob"           AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.dec-1 = var-result.valor-dec.
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "densotic"           AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.dens-otica = var-result.valor-dec.
        
        FIND FIRST var-result WHERE 
             var-result.nome-var     = "unfat"            AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.unid-fat-cliente = STRING(var-result.valor-char).

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "preun"             AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)  AND
             var-result.item-cotacao = ped-item.it-codigo           
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pd-config-pedido.preco-unit = var-result.valor-dec.
        
      END.

      ASSIGN pd-config-pedido.integrado-ems = YES.

END PROCEDURE.



