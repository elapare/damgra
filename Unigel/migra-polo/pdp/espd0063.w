 
&Scoped-define WINDOW-NAME C-Win

/*------------------------------------------------------------------------
File.............: espd0063.w
Description......: espd0063 - Dados Adicionais de Ped. de Venda
Input Parameters : 
Output Parameters: 
Author...........: Damgra - JosÇ Roberto
Created..........: 11/08/2011   
OBS..............: 
------------------------------------------------------------------------*/
DEFINE VARIABLE r-codigo-rwi  AS ROWID                     NO-UNDO.
DEFINE VARIABLE l-pd4000      AS LOGICAL     NO-UNDO.

define variable c-prog-gerado as character no-undo initial "espd0063".

define buffer if-ped-venda   for espmulti.if-ped-venda.
define buffer if-natur-item  for espmulti.if-natur-item.
define buffer if-natur-fam   for espmulti.if-natur-fam.


def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

DEF BUFFER bf-estabelec  FOR estabelec.
DEF BUFFER bf-emitente   FOR emitente.
DEF BUFFER bf-natur-oper FOR natur-oper. 

DEF BUFFER bf2-ped-venda FOR ped-venda.
DEF BUFFER bf2-ped-item  FOR ped-item.

CREATE WIDGET-POOL. 

/* ***************************  Definitions  ************************** */
define buffer empresa for mgmulti.empresa.
DEFINE BUFFER bf-pd-compl-pedido FOR pd-compl-pedido. 
DEFINE BUFFER bcf-item FOR ITEM.
DEFINE BUFFER b-ped-venda FOR ped-venda.
DEFINE BUFFER b-ped-item FOR ped-item.

/* Preprocessadores do Template de Relat¢rio                            */

&GLOBAL-DEFINE FOLD1 f-fold-1 
&GLOBAL-DEFINE FOLD2 f-fold-2 
&GLOBAL-DEFINE FOLD3 f-fold-3 
&GLOBAL-DEFINE FOLD4 f-fold-4 

/* Include Com as Vari†veis Globais */

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

DEF NEW GLOBAL SHARED VAR wh-fi-data-ent-cliente-esp AS WIDGET-HANDLE NO-UNDO.  /* criada na upc upc-pd4000-u02.p */
 /* Parameters Definitions ---      
                                     */ 

DEF NEW GLOBAL SHARED VAR wh-nome-abrev      AS WIDGET-HANDLE .
/*DEF NEW GLOBAL SHARED VAR wh-cod-estabel     AS WIDGET-HANDLE .
DEF NEW GLOBAL SHARED VAR wh-nat-operacao    AS WIDGET-HANDLE .*/
DEF NEW GLOBAL SHARED VAR wh-nr-pedcli       AS WIDGET-HANDLE .
DEF NEW GLOBAL SHARED VAR wh-nr-sequencia    AS WIDGET-HANDLE NO-UNDO .
def new global shared var pd4000-chamou      as LOGICAL  NO-UNDO     .

/* Temporary Table Definitions ---                                      */ 


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
DEFINE VARIABLE l-estab-ativo     AS LOGICAL             NO-UNDO. /* se Tem Incentivo Fiscal inativo estab e branco */





/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 


DEFINE VARIABLE c-cod-estabel-db    AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE c-nat-operacao-db   AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE c-nome-abrev-db     AS CHARACTER FORMAT "X(12)" NO-UNDO.
DEFINE VARIABLE d-perc-icms-db      AS DECIMAL   FORMAT ">>9.99" NO-UNDO.
DEFINE VARIABLE d-perc-desc-icms-db AS DECIMAL   FORMAT ">>9.99" NO-UNDO.
DEFINE VARIABLE d-prec-pis-db       AS DECIMAL   FORMAT ">>>>>>>>9.9999" NO-UNDO.
DEFINE VARIABLE d-perc-encargo-db   AS DECIMAL   FORMAT ">>9.99" NO-UNDO.
DEFINE VARIABLE d-prec-sem-desc-db  AS DECIMAL   FORMAT ">>>>>>>>9.99" NO-UNDO.
DEFINE VARIABLE d-perc-desc-db      AS DECIMAL   FORMAT ">>9.99" NO-UNDO.
DEFINE VARIABLE d-prec-pedido-db    AS DECIMAL   FORMAT ">>>>>>>>9.9999" NO-UNDO.
DEFINE VARIABLE preco-jr            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE preco-2-jr          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE i-exc               AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-nr-tab-finan      AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-nr-ind-finan      AS INTEGER    NO-UNDO.
DEFINE VARIABLE d-perc-encargo      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE desconto-jr         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE preco-sem-desc-jr   AS DECIMAL    NO-UNDO.
/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 

/* Chave unica do registro */
DEFINE VARIABLE i-ep-codigo-jr       AS CHAR                                NO-UNDO.
DEFINE VARIABLE i-nr-pedido-jr       AS INT                                 NO-UNDO.
DEFINE VARIABLE i-nr-sequencia-jr    AS INT                                 NO-UNDO.
/*-------------------------*/                                               
                                                                            
DEFINE VARIABLE c-nome-empresa             AS CHARACTER FORMAT "x(50)"         NO-UNDO.
DEFINE VARIABLE c-nome-abrev               AS CHARACTER FORMAT "x(12)"         NO-UNDO.
DEFINE VARIABLE c-nome-estabel-prod        AS CHARACTER FORMAT "x(40)"         NO-UNDO.
DEFINE VARIABLE c-nome-estabel-fat         AS CHARACTER FORMAT "x(40)"         NO-UNDO.
DEFINE VARIABLE c-nome-natureza-prod       AS CHARACTER FORMAT "x(40)"         NO-UNDO.
DEFINE VARIABLE c-nome-natureza-fat        AS CHARACTER FORMAT "x(40)"         NO-UNDO.

DEFINE VARIABLE c-it-codigo                AS CHARACTER FORMAT "x(16)"         NO-UNDO.
DEFINE VARIABLE c-cod-estabel              AS CHARACTER FORMAT "x(3)"          NO-UNDO.

DEFINE VARIABLE c-mensagem-jr        AS CHARACTER FORMAT "X(60)"            NO-UNDO.
DEFINE VARIABLE c-tipo-botao         AS CHARACTER                           NO-UNDO.
DEFINE VARIABLE c-tipo-botao2        AS CHARACTER                           NO-UNDO.

DEFINE VARIABLE preco-venda-icms-prod AS DECIMAL  FORMAT "->>>>>>>9.99"     NO-UNDO.
DEFINE VARIABLE preco-venda-icms-fat  AS DECIMAL  FORMAT "->>>>>>>9.99"     NO-UNDO.

/* Vari†veis para rotina de encontrar o % de icms */
DEFINE VARIABLE estab-faturx          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cod-emitente-faturx   AS INTEGER    NO-UNDO.
DEFINE VARIABLE nat-operacao-faturx   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE perc-icms-faturx      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE perc-desc-icms-faturx AS DECIMAL    NO-UNDO.
/* ---------------------------------------------- */

/* ********************  Preprocessor Definitions  ******************** */ 

def new global shared var rw-nr-pedido-0063 as row        no-undo.
def new global shared var l-espd0065        AS logical    no-undo.

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
     SIZE 32.5 BY 5.


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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)" INITIAL "Execuá∆o"
VIEW-AS TEXT 
SIZE 10.86 BY .62 NO-UNDO.

DEFINE VARIABLE text-parametro AS CHARACTER FORMAT "X(256)" INITIAL "ParÉmetros de Impress∆o"
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
LABEL "ParÉmetro 1"
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


DEFINE VARIABLE c-narrativa-jr AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 3000 SCROLLBAR-VERTICAL
     SIZE 53 BY 3 NO-UNDO.



/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-tela

    bt-primeiro AT ROW 1.25 COL 4    HELP "In°cio do Arquivo"
    bt-anterior AT ROW 1.25 COL 8.5  HELP "Registro Anterior"
    bt-proximo  AT ROW 1.25 COL 13   HELP "Pr¢ximo Registro"
    bt-final    AT ROW 1.25 COL 17.5 HELP "Final do Arquivo"
    bt-goto     AT ROW 1.25 COL 22   HELP "V† Para"
    bt-pesquisa AT ROW 1.25 COL 26.5 HELP "Pesquisa"
    bt-novo     AT ROW 1.25 COL 31   HELP "Inclui Novo Registro"
    bt-copia    AT ROW 1.25 COL 35.5 HELP "Copia Registro"
    bt-altera   AT ROW 1.25 COL 40   HELP "Altera Registro"
    bt-deleta   AT ROW 1.25 COL 44.5 HELP "Exclui Registro"
    bt-cancela  AT ROW 1.25 COL 49   HELP "Cancela Atualizaá∆o"
    bt-grava    AT ROW 1.25 COL 53.5 HELP "Grava Registro"
    bt-sai      AT ROW 1.25 COL 100.5   HELP "Encerra Programa"
    bt-ajuda    AT ROW 1.25 COL 105.5 HELP "Ajuda"

/* Colocar aqui os campos chaves do registro */
   
    pd-compl-pedido.ep-codigo label "Empresa"
      at row 2.7 col 20 colon-aligned
      view-as fill-in 
      size 10 by .88
      font 1

    c-nome-empresa NO-LABEL 
      at row 2.7 col 38 colon-aligned
      view-as fill-in 
      size 40 by .88
      font 1

    pd-compl-pedido.nr-pedido label "Nr.Pedido"
      at row 3.7 col 20 colon-aligned
      view-as fill-in 
      size 10 by .88
      font 1

    c-nome-abrev LABEL "Cliente" 
      at row 3.7 col 38 colon-aligned
      view-as fill-in 
      size 40 by .88
      font 1

    pd-compl-pedido.nr-sequencia label "Seq.Pedido"
      at row 4.7 col 20 colon-aligned
      view-as fill-in 
      size 10 by .88
      font 1
    
    c-it-codigo LABEL "Item" 
      at row 4.7 col 38 colon-aligned
      view-as fill-in 
      size 40 by .88
      font 1

    pd-compl-pedido.cod-emitente LABEL "Cod.Cliente" 
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
    
    pd-compl-pedido.cod-estabel-prod label "Estab.Produá∆o"
         AT ROW 1.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 12 BY .88

    c-nome-estabel-prod NO-LABEL 
      at row 1.3 col 32 colon-aligned
      view-as fill-in 
      size 20 by .88
      font 1

    pd-compl-pedido.nat-operacao-prod label "Nat.Operaá∆o Prod."
         AT ROW 2.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 12 BY .88
    
    c-nome-natureza-prod NO-LABEL 
      at row 2.3 col 32 colon-aligned
      view-as fill-in 
      size 20 by .88
      font 1

    pd-compl-pedido.perc-icms-prod label "% Icms Est.Produá∆o"
         AT ROW 3.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 12 BY .88


    d-prec-sem-desc-db label "Preáo s/Desc c/Pis Cof"
         AT ROW 4.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 12 BY .88
    
    d-perc-desc-db label "% de Desconto"
         AT ROW 4.3 COL 42 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 10 BY .88
    
    pd-compl-pedido.preco-pis-cof label "Preáo c/Pis Cofins"
         AT ROW 4.3 COL 68 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 12 BY .88
    
    pd-compl-pedido.perc-enc-finan label "% Encargos Financ."
         AT ROW 5.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 12 BY .88
    
    pd-compl-pedido.preco-venda-calc-prod label "Pr.Pedido c\Enc s\Icms"
         AT ROW 6.3 COL 18 COLON-ALIGNED
          BGCOLOR 14
         VIEW-AS FILL-IN 
         SIZE 12 BY .88
    
    preco-venda-icms-prod label "Pr.Venda c/Icms (Prod)"
         AT ROW 5.3 COL 49 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 12 BY .88
    
    
    pd-compl-pedido.cod-estabel-fat label "Estab.Faturamento"
         AT ROW 1.3 COL 68 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 12 BY .88

    c-nome-estabel-fat NO-LABEL 
      at row 1.3 col 82 colon-aligned
      view-as fill-in 
      size 20 by .88
      font 1

    pd-compl-pedido.nat-operacao-fat label "Nat.Operaá∆o Fatur."
         AT ROW 2.3 COL 68 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 12 BY .88
    
    c-nome-natureza-fat NO-LABEL 
      at row 2.3 col 82 colon-aligned
      view-as fill-in 
      size 20 by .88
      font 1

    pd-compl-pedido.perc-icms-fat label "% Icms Est.Fatur."
         AT ROW 3.3 COL 68 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 12 BY .88
    
    pd-compl-pedido.preco-venda-calc-fat label "Preáo Venda Est.Fatur"
         AT ROW 6.3 COL 68 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 12 BY .88
    
    preco-venda-icms-fat label "Pr.Venda c/Icms (Fat)"
         AT ROW 6.3 COL 68 COLON-ALIGNED
         BGCOLOR 3
         VIEW-AS FILL-IN 
         SIZE 12 BY .88
    
    pd-compl-pedido.dt-faturamento label "Data p/Faturamento"
         AT ROW 7.3 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 12 BY .88
    
    pd-compl-pedido.dt-entrega-cli label "Data Entrega Cliente"
         AT ROW 7.3 COL 68 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 12 BY .88
    
    pd-compl-pedido.narrativa at row 8.8 col 6
    
    pd-compl-pedido.tp-atendimento AT ROW 9.3 COL 75 

    pd-compl-pedido.lib-faturamento LABEL "Liberado p/Fatur." AT ROW 11.8 COL 84.2
    
    pd-compl-pedido.log-1 label "Enviado E-mail Cliente" at row 12.7 col 84.2
    
    rect-19 AT ROW 8.8 COL 74

   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D
   AT COL 3 ROW 7.85 FONT 1
   SIZE 107 BY 13.7.

DEFINE FRAME f-fold-2

   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D
   AT COL 3 ROW 7.85 FONT 1
   SIZE 107 BY 13.7.

DEFINE FRAME f-fold-3  
     
   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D
   AT COL 3 ROW 7.85 FONT 1
   SIZE 107 BY 13.7.

DEFINE FRAME f-fold-4

   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D
   AT COL 3 ROW 7.85 FONT 1
   SIZE 107 BY 13.7.


/* ******** Acerto da posiá∆o dos labels e tamanho dos radio-set ******* */

DEFINE VARIABLE h-label AS WIDGET-HANDLE NO-UNDO.

/* *************************  Create Window  ************************** */

CREATE WINDOW C-Win ASSIGN
   HIDDEN             = YES
   TITLE              = "Dados Adicionais de Ped. de Venda"
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

ON ENTRY OF pd-compl-pedido.ep-codigo IN FRAME f-tela /* Empresa */
DO:

    IF pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela = "0" THEN DO:

       FIND FIRST empresa NO-LOCK NO-ERROR.
       
       IF AVAIL empresa THEN
          ASSIGN pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela = STRING(empresa.ep-codigo)
                 c-nome-empresa:SCREEN-VALUE IN FRAME f-tela = empresa.nome.
       ELSE
          ASSIGN c-nome-empresa:SCREEN-VALUE IN FRAME f-tela = "".

    END.

END.



on LEAVE OF pd-compl-pedido.perc-enc-finan in frame f-fold-1 do:

    APPLY "LEAVE" TO pd-compl-pedido.preco-pis-cof IN FRAME f-fold-1.
    APPLY "entry" TO  pd-compl-pedido.preco-pis-cof IN FRAME f-fold-1.

END.

on LEAVE OF d-prec-sem-desc-db in frame f-fold-1 do:

    ASSIGN desconto-jr = 0
           desconto-jr = DEC(d-perc-desc-db:SCREEN-VALUE IN FRAME f-fold-1)
           preco-sem-desc-jr = 0
           preco-sem-desc-jr = DEC(d-prec-sem-desc-db:SCREEN-VALUE IN FRAME f-fold-1).

    ASSIGN pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1 = STRING(preco-sem-desc-jr - (preco-sem-desc-jr * (desconto-jr * 0.01))).
APPLY "LEAVE" TO pd-compl-pedido.preco-pis-cof IN FRAME f-fold-1.
APPLY "entry" TO d-perc-desc-db IN FRAME f-fold-1.
END.

on LEAVE OF d-perc-desc-db in frame f-fold-1 do:

    ASSIGN desconto-jr = 0
           desconto-jr = DEC(d-perc-desc-db:SCREEN-VALUE IN FRAME f-fold-1)
           preco-sem-desc-jr = 0
           preco-sem-desc-jr = DEC(d-prec-sem-desc-db:SCREEN-VALUE IN FRAME f-fold-1).

    ASSIGN pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1 = STRING(preco-sem-desc-jr - (preco-sem-desc-jr * (desconto-jr * 0.01))).

APPLY "LEAVE" TO pd-compl-pedido.preco-pis-cof IN FRAME f-fold-1.
APPLY "entry" TO  pd-compl-pedido.preco-pis-cof IN FRAME f-fold-1.
END.


on LEAVE OF pd-compl-pedido.preco-pis-cof in frame f-fold-1 do:

    /* Recalcular o desconto */

    IF INPUT FRAME f-fold-1 pd-compl-pedido.preco-pis-cof >=  
       INPUT FRAME f-fold-1 d-prec-sem-desc-db OR 
       INPUT FRAME f-fold-1 pd-compl-pedido.preco-pis-cof = 0 THEN DO:

        ASSIGN d-prec-sem-desc-db:SCREEN-VALUE IN FRAME f-fold-1 =
               pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1
               d-perc-desc-db:SCREEN-VALUE IN FRAME f-fold-1 = "".
    END.

    ELSE DO:

        ASSIGN d-perc-desc-db:SCREEN-VALUE IN FRAME f-fold-1 =  
            STRING((1 - (INPUT FRAME f-fold-1 pd-compl-pedido.preco-pis-cof / 
                  INPUT FRAME f-fold-1 d-prec-sem-desc-db)) * 100 ).

    END.

    /*-----------------------*/

    FIND FIRST para-fat NO-LOCK NO-ERROR.

    FIND FIRST natur-oper WHERE
        natur-oper.nat-operacao = INPUT FRAME f-fold-1 pd-compl-pedido.nat-operacao-prod
        NO-LOCK NO-ERROR.
                                
    d-perc-desc-icms-db = 0.
     
    
        ASSIGN  d-perc-icms-db = INPUT FRAME f-fold-1 pd-compl-pedido.perc-icms-prod
                d-perc-desc-icms-db = IF AVAIL natur-oper  THEN (100 - natur-oper.per-des-icms) / 100 ELSE 0.

    IF DEC(pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1) <> 0 THEN DO:

        ASSIGN preco-jr = 0
               preco-jr = DEC(pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1).
               
               IF para-fat.ind-preco = YES THEN 
               preco-jr = preco-jr / ((100 - DEC(pd-compl-pedido.perc-icms-prod:SCREEN-VALUE IN FRAME f-fold-1)) / 100).
               
               preco-jr = preco-jr * (1 + (DEC(pd-compl-pedido.perc-enc-finan:SCREEN-VALUE IN FRAME f-fold-1)) / 100).
        
              IF para-fat.ind-preco = YES THEN 
                       ASSIGN pd-compl-pedido.preco-venda-calc-prod:SCREEN-VALUE IN FRAME f-fold-1 = STRING(preco-jr / d-perc-desc-icms-db).
              else  
                       ASSIGN pd-compl-pedido.preco-venda-calc-prod:SCREEN-VALUE IN FRAME f-fold-1 = STRING(preco-jr).  



              /* Calcula o preáo com icmcs para consulta na tela - Est. Produá∆o */

                  ASSIGN preco-2-jr = 0
                         preco-2-jr = DEC(pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1).

                  ASSIGN preco-2-jr = preco-2-jr / ((100 - DEC(pd-compl-pedido.perc-icms-prod:SCREEN-VALUE IN FRAME f-fold-1)) / 100).

                  ASSIGN preco-2-jr = preco-2-jr * (1 + (DEC(pd-compl-pedido.perc-enc-finan:SCREEN-VALUE IN FRAME f-fold-1)) / 100).

                  ASSIGN preco-venda-icms-prod:SCREEN-VALUE IN FRAME f-fold-1 = STRING(preco-2-jr / d-perc-desc-icms-db).

                                     
              /* ---------------------------------------------------------------- */

              /* Calcular o preáo de venda com icms no estab. de faturamento  */

    
                FIND FIRST if-ped-venda WHERE
                        if-ped-venda.nr-pedido = int(pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela) AND
                        if-ped-venda.nr-pedido-relac <> 0
                        NO-LOCK NO-ERROR.

                IF AVAIL if-ped-venda THEN DO:

                    FIND FIRST bf2-ped-venda WHERE
                        bf2-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac
                        NO-LOCK NO-ERROR.

                END.

                ELSE DO:

                    FIND FIRST bf2-ped-venda WHERE
                        bf2-ped-venda.nr-pedido = int(pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)
                        NO-LOCK NO-ERROR.

                END.

                IF AVAIL bf2-ped-venda THEN DO:

                    FIND FIRST bf2-ped-item OF bf2-ped-venda WHERE 
                        bf2-ped-item.nr-sequencia = int(pd-compl-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela) AND
                        bf2-ped-item.ind-componen < 3 NO-LOCK NO-ERROR.

                    IF AVAIL bf2-ped-item THEN DO:

                        ASSIGN estab-faturx        = bf2-ped-venda.cod-estabel
                               cod-emitente-faturx = bf2-ped-venda.cod-emitente
                               nat-operacao-faturx = bf2-ped-item.nat-operacao.

                        RUN pi-acha-perc-icms.

                        FIND FIRST para-fat NO-LOCK NO-ERROR.
                        
                        ASSIGN  d-perc-icms-db = perc-icms-faturx
                                d-perc-desc-icms-db = (100 - perc-desc-icms-faturx) / 100.
                        
                        IF DEC(pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1) <> 0 THEN DO:
                        
                        
                           /* Calcula o preáo com icmcs para consulta na tela - Est. faturamento */
                        
                               ASSIGN preco-2-jr = 0
                                      preco-2-jr = DEC(pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1).
                        
                               ASSIGN preco-2-jr = preco-2-jr / ((100 - DEC(d-perc-icms-db)) / 100).
                        
                               ASSIGN preco-2-jr = preco-2-jr * (1 + (DEC(pd-compl-pedido.perc-enc-finan:SCREEN-VALUE IN FRAME f-fold-1)) / 100).
                        
                               ASSIGN preco-venda-icms-fat:SCREEN-VALUE IN FRAME f-fold-1 = STRING(preco-2-jr / d-perc-desc-icms-db).

                               ASSIGN cod-estabel-fat:SCREEN-VALUE IN FRAME f-fold-1  = STRING(estab-faturx)
                                      nat-operacao-fat:SCREEN-VALUE IN FRAME f-fold-1 = STRING(nat-operacao-faturx)
                                      perc-icms-fat:SCREEN-VALUE IN FRAME f-fold-1    = STRING(perc-icms-faturx).

                               FIND FIRST natur-oper WHERE
                                    natur-oper.nat-operacao = pd-compl-pedido.nat-operacao-fat:SCREEN-VALUE IN FRAME f-fold-1
                                    NO-LOCK NO-ERROR.
                            
                                IF AVAIL natur-oper THEN
                                   ASSIGN c-nome-natureza-fat:SCREEN-VALUE IN FRAME f-fold-1   = natur-oper.denominacao.
                                ELSE
                                   ASSIGN c-nome-natureza-fat:SCREEN-VALUE IN FRAME f-fold-1  = "".


                        END.

                    END.  /* AVAIL bf2-ped-item */

                    ELSE
                        ASSIGN preco-venda-icms-fat:SCREEN-VALUE IN FRAME f-fold-1 = ""
                               cod-estabel-fat:SCREEN-VALUE IN FRAME f-fold-1      = ""
                               nat-operacao-fat:SCREEN-VALUE IN FRAME f-fold-1     = ""
                               perc-icms-fat:SCREEN-VALUE IN FRAME f-fold-1        = "".
                        
                END.  /* AVAIL bf2-ped-venda */

                ELSE
                    ASSIGN preco-venda-icms-fat:SCREEN-VALUE IN FRAME f-fold-1 = ""
                           cod-estabel-fat:SCREEN-VALUE IN FRAME f-fold-1      = ""
                           nat-operacao-fat:SCREEN-VALUE IN FRAME f-fold-1     = ""
                           perc-icms-fat:SCREEN-VALUE IN FRAME f-fold-1        = "".


              /* ---------------------------------------------------------------- */
        
    END.
        
        apply  'LEAVE' to pd-compl-pedido.cod-estabel-fat in frame f-fold-1.

END.


on LEAVE OF pd-compl-pedido.cod-estabel-fat in frame f-fold-1 do:
   /*xuxu*/
    FIND FIRST estabelec WHERE
        estabelec.cod-estabel = pd-compl-pedido.cod-estabel-fat:SCREEN-VALUE IN FRAME f-fold-1
        NO-LOCK NO-ERROR.                                                                      

    IF AVAIL estabelec THEN DO:

        ASSIGN c-nome-estabel-fat:SCREEN-VALUE IN FRAME f-fold-1 = estabelec.nome.

        FIND FIRST emitente WHERE
            emitente.cod-emitente = int(pd-compl-pedido.cod-emitente:SCREEN-VALUE IN FRAME f-tela)
            NO-LOCK NO-ERROR.
/*
        FIND FIRST pd-natur-oper-fat WHERE
            pd-natur-oper-fat.ep-codigo         = pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela              AND
            pd-natur-oper-fat.cod-estabel-prod  = string(pd-compl-pedido.cod-estabel-prod :SCREEN-VALUE IN FRAME f-fold-1) AND
            pd-natur-oper-fat.nat-operacao-prod = string(pd-compl-pedido.nat-operacao-prod:SCREEN-VALUE IN FRAME f-fold-1) AND
            pd-natur-oper-fat.cod-estabel-fat   = string(pd-compl-pedido.cod-estabel-fat:SCREEN-VALUE IN FRAME f-fold-1)   AND
            pd-natur-oper-fat.cod-emitente      = int(pd-compl-pedido.cod-emitente:SCREEN-VALUE IN FRAME f-tela)           AND
            pd-natur-oper-fat.estado-cli        = ""
            NO-LOCK NO-ERROR.

        IF NOT AVAIL pd-natur-oper-fat THEN 

           FIND FIRST pd-natur-oper-fat WHERE
               pd-natur-oper-fat.ep-codigo         = pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela              AND
               pd-natur-oper-fat.cod-estabel-prod  = string(pd-compl-pedido.cod-estabel-prod :SCREEN-VALUE IN FRAME f-fold-1) AND
               pd-natur-oper-fat.nat-operacao-prod = string(pd-compl-pedido.nat-operacao-prod:SCREEN-VALUE IN FRAME f-fold-1) AND
               pd-natur-oper-fat.cod-estabel-fat   = string(pd-compl-pedido.cod-estabel-fat:SCREEN-VALUE IN FRAME f-fold-1)   AND
               pd-natur-oper-fat.cod-emitente      = 0                                                                        AND           
               pd-natur-oper-fat.estado-cli        = emitente.estado
               NO-LOCK NO-ERROR.

        IF NOT AVAIL pd-natur-oper-fat THEN 

            FIND FIRST pd-natur-oper-fat WHERE
                pd-natur-oper-fat.ep-codigo         = pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela              AND
                pd-natur-oper-fat.cod-estabel-prod  = string(pd-compl-pedido.cod-estabel-prod :SCREEN-VALUE IN FRAME f-fold-1) AND
                pd-natur-oper-fat.nat-operacao-prod = string(pd-compl-pedido.nat-operacao-prod:SCREEN-VALUE IN FRAME f-fold-1) AND
                pd-natur-oper-fat.cod-estabel-fat   = string(pd-compl-pedido.cod-estabel-fat:SCREEN-VALUE IN FRAME f-fold-1)   AND
                pd-natur-oper-fat.cod-emitente      = 0                                                                        AND           
                pd-natur-oper-fat.estado-cli        = ""
                NO-LOCK NO-ERROR.
  */


  /*

        IF AVAIL pd-natur-oper-fat THEN DO:

           FIND FIRST natur-oper WHERE
               natur-oper.nat-operacao = pd-natur-oper-fat.nat-operacao-fat
               NO-LOCK NO-ERROR.
           
           ASSIGN c-nome-natureza-fat:SCREEN-VALUE IN FRAME f-fold-1 = natur-oper.denominacao.

           ASSIGN  d-perc-icms-db = 0
                   d-perc-desc-icms-db = (100 - natur-oper.per-des-icms) / 100.
           
           if natur-oper.cd-trib-icm = 1 then do:
           
                IF emitente.estado = estabelec.estado  THEN
                    d-perc-icms-db = 18.
                ELSE
                    d-perc-icms-db = 12.
           
                FIND FIRST unid-feder WHERE unid-feder.pais   = ESTABELEC.pais   AND
                                            unid-feder.estado = estabelec.estado 
                     NO-LOCK NO-ERROR.
           
                IF  AVAIL unid-feder THEN DO:
           
                   IF emitente.estado = estabelec.estado THEN
                      ASSIGN d-perc-icms-db = unid-feder.per-icms-int.
           
                   ELSE DO:
           
                       ASSIGN d-perc-icms-db = unid-feder.per-icms-ext.
           
                       DO i-exc = 1 TO 25:
           
                           IF unid-feder.est-exc [i-exc] =  "" THEN NEXT.
           
                           IF unid-feder.est-exc [i-exc] = emitente.estado THEN DO:
           
                               ASSIGN d-perc-icms-db = unid-feder.perc-exc [i-exc].
                               LEAVE.
           
                           END.
           
                       END.
           
                   END. 
           
                END.
           
           end.   /* tributado icm */
           
           if natur-oper.cd-trib-icm = 4 then do:
           
                IF emitente.estado = estabelec.estado  THEN
                    assign d-perc-icms-db = 12.
           
           end.    
           
           assign d-perc-encargo = dec(pd-compl-pedido.perc-enc-finan:SCREEN-VALUE IN FRAME f-fold-1).
           
           ASSIGN pd-compl-pedido.nat-operacao-fat:SCREEN-VALUE IN FRAME f-fold-1 = pd-natur-oper-fat.nat-operacao-fat  
                  pd-compl-pedido.perc-icms-fat:SCREEN-VALUE IN FRAME f-fold-1    = string(d-perc-icms-db).
               
           
           FIND FIRST para-fat NO-LOCK NO-ERROR.
           
           IF DEC(pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1) <> 0 THEN DO:
           
               ASSIGN preco-jr = 0
                      preco-jr = DEC(pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1).
                      
               IF para-fat.ind-preco = YES THEN 
                      preco-jr = preco-jr / ((100 - DEC(pd-compl-pedido.perc-icms-fat:SCREEN-VALUE IN FRAME f-fold-1)) / 100).
                      
                      preco-jr = preco-jr * (1 + (DEC(pd-compl-pedido.perc-enc-finan:SCREEN-VALUE IN FRAME f-fold-1)) / 100).
               
              IF para-fat.ind-preco = YES THEN 
                  ASSIGN pd-compl-pedido.preco-venda-calc-fat:SCREEN-VALUE IN FRAME f-fold-1 = STRING(preco-jr / d-perc-desc-icms-db).
              else  
                  ASSIGN pd-compl-pedido.preco-venda-calc-fat:SCREEN-VALUE IN FRAME f-fold-1 = STRING(preco-jr).


              /* Calcula o preáo com icmcs para consulta na tela - Est. Faturamento */

                  ASSIGN preco-2-jr = 0
                         preco-2-jr = DEC(pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1).

                  ASSIGN preco-2-jr = preco-2-jr / ((100 - DEC(pd-compl-pedido.perc-icms-fat:SCREEN-VALUE IN FRAME f-fold-1)) / 100).

                  ASSIGN preco-2-jr = preco-2-jr * (1 + (DEC(pd-compl-pedido.perc-enc-finan:SCREEN-VALUE IN FRAME f-fold-1)) / 100).

                  ASSIGN preco-venda-icms-fat:SCREEN-VALUE IN FRAME f-fold-1 = STRING(preco-2-jr / d-perc-desc-icms-db).

                                     
              /* ------------------------------------------------------------------ */

               
           END.
        
        END.

  */

    END.

END.


on LEAVE OF pd-compl-pedido.ep-codigo in frame f-tela do:

   FIND FIRST empresa WHERE
       empresa.ep-codigo = pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela
       NO-LOCK NO-ERROR.
   
   IF AVAIL empresa THEN
      ASSIGN c-nome-empresa:SCREEN-VALUE IN FRAME f-tela = empresa.nome.
   ELSE
      ASSIGN c-nome-empresa:SCREEN-VALUE IN FRAME f-tela = "".

end.    


on LEAVE OF pd-compl-pedido.nr-pedido in frame f-tela do:

   FIND FIRST ped-venda WHERE
       ped-venda.nr-pedido = int(pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)
       NO-LOCK NO-ERROR.
   
   IF AVAIL ped-venda THEN
      ASSIGN c-nome-abrev:SCREEN-VALUE IN FRAME f-tela  = ped-venda.nome-abrev
             c-cod-estabel:SCREEN-VALUE IN FRAME f-tela = ped-venda.cod-estabel.
   ELSE
      ASSIGN c-nome-abrev:SCREEN-VALUE IN FRAME f-tela  = ""
             c-cod-estabel:SCREEN-VALUE IN FRAME f-tela = "".

end.    


on LEAVE OF pd-compl-pedido.nr-sequencia in frame f-tela do:

   FIND FIRST ped-venda WHERE
        ped-venda.nr-pedido    = int(pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)  NO-LOCK NO-ERROR.
     IF AVAIL ped-venda THEN
     FIND FIRST ped-item OF ped-venda WHERE
             ped-item.nr-sequencia = int(pd-compl-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)      AND
             ped-item.ind-componen < 3 NO-LOCK NO-ERROR.

   IF AVAIL ped-item THEN
      ASSIGN c-it-codigo:SCREEN-VALUE IN FRAME f-tela  = ped-item.it-codigo.
   ELSE
      ASSIGN c-it-codigo:SCREEN-VALUE IN FRAME f-tela  = "".


end.    


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


ON CHOOSE OF bt-pesquisa IN FRAME f-tela
DO:
   ASSIGN i-ep-codigo-jr    = "0"
          i-nr-pedido-jr    = 0
          i-nr-sequencia-jr = 0.
                             
   RUN pi-pesquisa-reg.

   IF i-nr-pedido-jr <> 0 THEN DO:

     FIND FIRST pd-compl-pedido WHERE
         pd-compl-pedido.ep-codigo    = i-ep-codigo-jr       AND
         pd-compl-pedido.nr-pedido    = i-nr-pedido-jr       AND
         pd-compl-pedido.nr-sequencia = i-nr-sequencia-jr
         NO-LOCK NO-ERROR.
     
     IF AVAIL pd-compl-pedido THEN
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


    IF l-pd4000  = NO AND
       l-espd0065 = NO THEN RETURN NO-APPLY.


   FIND FIRST pd-compl-pedido WHERE
       pd-compl-pedido.ep-codigo    = pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela AND
       pd-compl-pedido.nr-pedido    = int(pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela) AND
       pd-compl-pedido.nr-sequencia = int(pd-compl-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)
       NO-LOCK NO-ERROR.

   IF NOT AVAIL pd-compl-pedido THEN RETURN.

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
             THREE-D SCROLLABLE TITLE "Exclus∆o de Registro no Arquivo" FONT 1
             DEFAULT-BUTTON ex-bt-ok CANCEL-BUTTON ex-bt-cancel.

    ON "CHOOSE":U OF ex-bt-ok IN FRAME ex-frame-1 DO:

        FIND CURRENT pd-compl-pedido EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL pd-compl-pedido THEN 
            DELETE pd-compl-pedido.
/*
        FIND NEXT pd-compl-pedido NO-LOCK NO-ERROR.

        IF AVAIL pd-compl-pedido THEN 
           RUN pi-mostra-registro. 
        ELSE DO:
            FIND PREV  pd-compl-pedido NO-LOCK NO-ERROR.
            IF AVAIL pd-compl-pedido THEN 
               RUN pi-mostra-registro.
            ELSE
                RUN pi-limpa-campos. 
        END.
*/

     
     RETURN.

    END.

    ENABLE ex-bt-cancel ex-bt-ok  
        WITH FRAME ex-frame-1. 
    
    DISPLAY c-mensagem-jr 
        WITH FRAME ex-frame-1.

    WAIT-FOR "GO":U OF FRAME ex-frame-1.

    apply "choose" to bt-sai in frame f-tela.
 

END.


ON CHOOSE OF bt-cancela IN FRAME f-tela
DO:

     if l-pd4000 = YES then DO:
           
           apply "close" to this-procedure.
           RETURN.
    END.

   FIND CURRENT pd-compl-pedido NO-LOCK NO-ERROR.

   IF AVAIL pd-compl-pedido THEN
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

   FIND FIRST pd-compl-pedido WHERE
       pd-compl-pedido.ep-codigo    = pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela AND
       pd-compl-pedido.nr-pedido    = int(pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela) AND
       pd-compl-pedido.nr-sequencia = int(pd-compl-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)
       NO-LOCK NO-ERROR.

   IF NOT AVAIL pd-compl-pedido THEN RETURN.

   IF AVAIL ped-item AND ped-item.cod-sit-item > 2 THEN do: 
       ASSIGN c-mensagem-jr = "Item do Pedido J† Liquidado- N∆o Disponivel!".
       RUN pi-mostra-mensagem.
       RETURN NO-APPLY.
   END.

   ASSIGN c-tipo-botao = "altera".

   RUN pi-enable-bt-grava.
  /* RUN pi-disable-outros-botoes.  */

   RUN pi-enable-campos.
   
   APPLY 'entry' TO d-prec-sem-desc-db IN FRAME f-fold-1.

END.


ON CHOOSE OF bt-copia IN FRAME f-tela
DO:
    RETURN NO-APPLY.

   FIND FIRST pd-compl-pedido WHERE
       pd-compl-pedido.ep-codigo    = pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela AND
       pd-compl-pedido.nr-pedido    = int(pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela) AND
       pd-compl-pedido.nr-sequencia = int(pd-compl-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)
       NO-LOCK NO-ERROR.

   IF NOT AVAIL pd-compl-pedido THEN RETURN.

   ASSIGN c-tipo-botao = "copia".

   RUN pi-enable-bt-grava.
   RUN pi-disable-outros-botoes.

   RUN pi-enable-campos.

END.


ON CHOOSE OF bt-grava IN FRAME f-tela
DO:


    IF c-tipo-botao <> "novo"  AND 
       c-tipo-botao <> "copia" AND  
       c-tipo-botao <> "altera" THEN RETURN.

    /* Aqui colocar as validaá‰es dos campos antes de serem
       gravados no arquivo */
 /*
    IF pd-compl-pedido.cod-estabel-fat :SCREEN-VALUE IN FRAME f-fold-1 <> "" THEN DO:

        FIND FIRST pd-natur-oper-fat WHERE
            pd-natur-oper-fat.ep-codigo         = pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela              AND
            pd-natur-oper-fat.cod-estabel-prod  = string(pd-compl-pedido.cod-estabel-prod :SCREEN-VALUE IN FRAME f-fold-1) AND
            pd-natur-oper-fat.nat-operacao-prod = string(pd-compl-pedido.nat-operacao-prod:SCREEN-VALUE IN FRAME f-fold-1) AND
            pd-natur-oper-fat.cod-estabel-fat   = string(pd-compl-pedido.cod-estabel-fat:SCREEN-VALUE IN FRAME f-fold-1)   
            NO-LOCK NO-ERROR.
        
        IF NOT AVAIL pd-natur-oper-fat THEN DO:
            ASSIGN c-mensagem-jr = "Natureza de Operaá∆o para Faturamento N∆o Prevista".
            RUN pi-mostra-mensagem.
        
            APPLY "entry" TO pd-compl-pedido.cod-estabel-fat IN FRAME f-fold-1.
        
            RETURN NO-APPLY.
        END.

        FIND FIRST natur-oper WHERE
            natur-oper.nat-operacao = pd-natur-oper-fat.nat-operacao-fat
            NO-LOCK NO-ERROR.
           
        IF NOT AVAIL pd-natur-oper-fat THEN DO:
            ASSIGN c-mensagem-jr = "Natureza de Operaá∆o para Faturamento N∆o Existe".
            RUN pi-mostra-mensagem.
        
            APPLY "entry" TO pd-compl-pedido.cod-estabel-fat IN FRAME f-fold-1.
        
            RETURN NO-APPLY.

        END.

    END.
    */
    IF (dec(pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1)         = 0 OR
       dec(pd-compl-pedido.preco-venda-calc-prod:SCREEN-VALUE IN FRAME f-fold-1)  = 0) AND
       l-pd4000 = YES THEN DO:

        ASSIGN c-mensagem-jr = "Erro no Preáo de Venda".
        RUN pi-mostra-mensagem.

        APPLY "entry" TO d-prec-sem-desc-db IN FRAME f-fold-1.

        RETURN NO-APPLY.

    END.

    FIND FIRST empresa WHERE
        empresa.ep-codigo = pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela        NO-LOCK NO-ERROR.

    IF NOT AVAIL empresa THEN DO:

        ASSIGN c-mensagem-jr = "Empresa N∆o Existe".
        RUN pi-mostra-mensagem.

        APPLY "entry" TO pd-compl-pedido.ep-codigo IN FRAME f-tela.

        RETURN NO-APPLY.

    END.

    FIND FIRST ped-venda WHERE
        ped-venda.nr-pedido = int(pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)
        NO-LOCK NO-ERROR.

    IF NOT AVAIL ped-venda THEN DO:

        ASSIGN c-mensagem-jr = "Pedido de Venda N∆o Existe".
        RUN pi-mostra-mensagem.

        APPLY "entry" TO pd-compl-pedido.nr-pedido IN FRAME f-tela.

        RETURN NO-APPLY.

    END.

    /*
    FIND FIRST ped-item OF ped-venda WHERE
        ped-item.nr-sequencia = int(pd-compl-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela) AND
        ped-item.ind-componen < 3
        NO-LOCK NO-ERROR.

    IF NOT AVAIL ped-item THEN DO:

        ASSIGN c-mensagem-jr = "Item do Pedido de Venda N∆o Existe".
        RUN pi-mostra-mensagem.

        APPLY "entry" TO pd-compl-pedido.nr-sequencia IN FRAME f-tela.

        RETURN NO-APPLY.

    END.
    */

    IF date(pd-compl-pedido.dt-entrega-cli:SCREEN-VALUE IN FRAME f-fold-1) < ped-venda.dt-entrega THEN DO:

        ASSIGN c-mensagem-jr = "Data Entrega p/ o Cliente Menor que Dt.Entrega do Pedido".
        RUN pi-mostra-mensagem.

        APPLY "entry" TO pd-compl-pedido.dt-entrega-cli IN FRAME f-fold-1.

        RETURN NO-APPLY.

    END.


    IF date(pd-compl-pedido.dt-faturamento:SCREEN-VALUE IN FRAME f-fold-1) < (ped-venda.dt-entrega - 10) THEN DO:

        ASSIGN c-mensagem-jr = "Data p/ Faturamento Menor que Dt.Entrega do Pedido".
        RUN pi-mostra-mensagem.

        APPLY "entry" TO pd-compl-pedido.dt-faturamento IN FRAME f-fold-1.

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

    apply "close" to this-procedure.
    RETURN.

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

assign v-cod-prog-gerado = "espd0063".



def var c-tit as char no-undo.

ASSIGN c-tit = "espd0063 - Dados Adicionais de Ped. de Venda".
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
def var wh-label-cla       as widget-handle no-undo.
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


/********************************************************** 
** Traduá∆o p†gina - frame f-fold-1
**********************************************************/
create text wh-label-fld-1
    assign frame        = frame f-tela:handle
           format       = "x(12)"
           font         = 1
           screen-value = "Compl.Pedido"
           width        = 11
           row          = 6.6
           col          = im-fold-1:col in frame f-tela + 1.86
           visible      = yes
     triggers:
         on mouse-select-click
            apply "mouse-select-click" to im-fold-1 in frame f-tela.           
     end triggers.     

/********************************************************** 
** Traduá∆o p†gina - frame f-fold-2
**********************************************************/
create text wh-label-fld-2
         assign frame        = frame f-tela:handle
                format       = "x(12)"
                font         = 1
                screen-value = "       "
                width        = 11
                row          = 6.6
                col          = im-fold-2:col in frame f-tela + 1.86
                visible      = yes
          triggers:
              on mouse-select-click
                 apply "mouse-select-click" to im-fold-2 in frame f-tela.           
          end triggers.

/********************************************************** 
** Traduá∆o p†gina - frame f-fold-3
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
** Traduá∆o p†gina - frame f-fold-4
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
** Troca de p†gina por CTRL-TAB e SHIFT-CTRL-TAB
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
   
    /* Desabilita folders n∆o usadas */

    ASSIGN im-fold-2:VISIBLE in frame f-tela = no
           im-fold-3:VISIBLE in frame f-tela = no 
           im-fold-4:VISIBLE in frame f-tela = no.


    /* Se o programa foi chamado pelo PD4000 j† vem com o numero do pedido rw-nr-pedido-0063 */


    ASSIGN l-pd4000 = NO.
    
    IF rw-nr-pedido-0063 <> ? THEN DO:
    
        IF l-espd0065 = NO THEN
           ASSIGN l-pd4000 = YES.

        FIND FIRST ped-item where rowid(ped-item) = rw-nr-pedido-0063
             NO-LOCK NO-ERROR.
               
        ASSIGN rw-nr-pedido-0063 = ?.
                            
        IF AVAIL ped-item THEN DO:

           FIND FIRST ped-venda of ped-item
               NO-LOCK NO-ERROR.           
        
             IF AVAIL ped-venda THEN DO:
             
               FIND FIRST estabelec WHERE estabelec.cod-estabel = ped-venda.cod-estabel
                    NO-LOCK NO-ERROR.

               FIND FIRST pd-compl-pedido WHERE
                   pd-compl-pedido.ep-codigo    = estabelec.ep-codigo    AND
                   pd-compl-pedido.nr-pedido    = ped-venda.nr-pedido    AND
                   pd-compl-pedido.nr-sequencia = ped-item.nr-sequencia
                   exclusive-lock NO-ERROR.

               IF NOT AVAIL pd-compl-pedido THEN DO:
                    CREATE pd-compl-pedido.

                   ASSIGN pd-compl-pedido.ep-codigo       = estabelec.ep-codigo 
                          pd-compl-pedido.nr-pedido       = ped-venda.nr-pedido
                          pd-compl-pedido.nr-sequencia    = ped-item.nr-sequencia
                          pd-compl-pedido.cod-emitente    = ped-venda.cod-emitente.

                   RUN pi-cria-compl-atual.

               END.
               
                
               RUN pi-mostra-registro.

               RUN pi-disable-outros-botoes.

               assign bt-grava:SENSITIVE in frame f-tela   = no.

               assign bt-altera:SENSITIVE in frame f-tela  = YES.

               APPLY "choose" TO bt-altera IN FRAME f-tela.

           END.
        
        END.

    END.
    

    IF rw-nr-pedido-0063 = ? AND pd4000-chamou = YES THEN DO:
    
        find first ped-item where rowid(ped-item) = rw-nr-pedido-0063
        no-lock no-error.
    
        IF l-espd0065 = NO THEN
           ASSIGN l-pd4000 = YES.

        FIND FIRST ped-venda where
            ped-venda.nr-pedcli  = string(wh-nr-pedcli:SCREEN-VALUE) AND
            ped-venda.nome-abrev = string(wh-nome-abrev:SCREEN-VALUE)
            NO-LOCK NO-ERROR.   
            
        IF AVAIL ped-venda THEN DO:

             ASSIGN rw-nr-pedido-0063 = ?.

             FIND FIRST estabelec WHERE estabelec.cod-estabel = ped-venda.cod-estabel
                  NO-LOCK NO-ERROR.

             FIND FIRST pd-compl-pedido WHERE
                 pd-compl-pedido.ep-codigo    = estabelec.ep-codigo    AND
                 pd-compl-pedido.nr-pedido    = ped-venda.nr-pedido    AND
                 pd-compl-pedido.nr-sequencia = int(wh-nr-sequencia:SCREEN-VALUE)
                 exclusive-LOCK NO-ERROR.

             IF NOT AVAIL pd-compl-pedido THEN DO:
                 

                 CREATE pd-compl-pedido.

                 ASSIGN pd-compl-pedido.ep-codigo       = estabelec.ep-codigo 
                        pd-compl-pedido.nr-pedido       = ped-venda.nr-pedido
                        pd-compl-pedido.nr-sequencia    = int(wh-nr-sequencia:SCREEN-VALUE)
                        pd-compl-pedido.cod-emitente    = ped-venda.cod-emitente.

                  RUN pi-cria-compl-atual.

             END.

             RUN pi-mostra-registro.

             RUN pi-disable-outros-botoes.

             assign bt-grava:SENSITIVE in frame f-tela   = no.

             assign bt-altera:SENSITIVE in frame f-tela  = YES.

             APPLY "choose" TO bt-altera IN FRAME f-tela.

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

   FIND FIRST pd-compl-pedido NO-LOCK NO-ERROR.

   IF AVAIL pd-compl-pedido THEN DO:

       RUN pi-mostra-registro.

   END.

   ENABLE  
    /*   pd-compl-pedido.sequencia */
   WITH FRAME f-tela IN WINDOW C-Win.
   

   ENABLE  
    /*   pd-compl-pedido.descricao   */
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
    ASSIGN pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela               = STRING (pd-compl-pedido.ep-codigo)
           pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela               = STRING (pd-compl-pedido.nr-pedido)
           pd-compl-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela            = STRING (pd-compl-pedido.nr-sequencia)
           pd-compl-pedido.cod-emitente:SCREEN-VALUE IN FRAME f-tela            = STRING (pd-compl-pedido.cod-emitente)
                                                                                
           pd-compl-pedido.cod-estabel-prod:SCREEN-VALUE IN FRAME f-fold-1      = STRING (pd-compl-pedido.cod-estabel-prod)
           pd-compl-pedido.cod-estabel-fat:SCREEN-VALUE IN FRAME f-fold-1       = STRING (pd-compl-pedido.cod-estabel-fat)
           pd-compl-pedido.nat-operacao-prod:SCREEN-VALUE IN FRAME f-fold-1     = STRING (pd-compl-pedido.nat-operacao-prod)
           pd-compl-pedido.nat-operacao-fat:SCREEN-VALUE IN FRAME f-fold-1      = STRING (pd-compl-pedido.nat-operacao-fat)
           pd-compl-pedido.perc-icms-prod:SCREEN-VALUE IN FRAME f-fold-1        = STRING (pd-compl-pedido.perc-icms-prod)
           pd-compl-pedido.perc-icms-fat:SCREEN-VALUE IN FRAME f-fold-1         = STRING (pd-compl-pedido.perc-icms-fat)
           pd-compl-pedido.perc-enc-finan:SCREEN-VALUE IN FRAME f-fold-1        = STRING (pd-compl-pedido.perc-enc-finan)
           pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1         = STRING (pd-compl-pedido.preco-pis-cof)
           pd-compl-pedido.preco-venda-calc-prod:SCREEN-VALUE IN FRAME f-fold-1 = STRING (pd-compl-pedido.preco-venda-calc-prod)
           pd-compl-pedido.preco-venda-calc-fat:SCREEN-VALUE IN FRAME f-fold-1  = STRING (pd-compl-pedido.preco-venda-calc-fat)
           pd-compl-pedido.dt-faturamento:SCREEN-VALUE IN FRAME f-fold-1        = STRING (pd-compl-pedido.dt-faturamento)
           pd-compl-pedido.dt-entrega-cli:SCREEN-VALUE IN FRAME f-fold-1        =   IF VALID-HANDLE(wh-fi-data-ent-cliente-esp) THEN wh-fi-data-ent-cliente-esp:SCREEN-VALUE ELSE STRING (pd-compl-pedido.dt-entrega-cli)
           pd-compl-pedido.narrativa:SCREEN-VALUE IN FRAME f-fold-1             = STRING (pd-compl-pedido.narrativa)
           pd-compl-pedido.tp-atendimento:SCREEN-VALUE IN FRAME f-fold-1        = STRING (pd-compl-pedido.tp-atendimento).

    ASSIGN preco-venda-icms-prod:SCREEN-VALUE IN FRAME f-fold-1  =  string(SUBSTRING (pd-compl-pedido.char-1,1,15), "x(15)")
           preco-venda-icms-fat:SCREEN-VALUE IN FRAME f-fold-1   =  string(SUBSTRING (pd-compl-pedido.char-1,16,15), "x(15)").       

    ASSIGN pd-compl-pedido.lib-faturamento:SCREEN-VALUE IN FRAME f-fold-1 = IF pd-compl-pedido.lib-faturamento = YES THEN "Sim" ELSE "N∆o".
    ASSIGN pd-compl-pedido.log-1:SCREEN-VALUE IN FRAME f-fold-1           = IF pd-compl-pedido.log-1 = YES THEN "Sim" ELSE "N∆o".


    ASSIGN d-perc-desc-db:SCREEN-VALUE IN FRAME f-fold-1     = string(SUBSTRING (pd-compl-pedido.char-1,31,10), "x(10)")
           d-prec-sem-desc-db:SCREEN-VALUE IN FRAME f-fold-1 = string(SUBSTRING (pd-compl-pedido.char-1,41,10), "x(10)").

    IF DEC(d-prec-sem-desc-db:SCREEN-VALUE IN FRAME f-fold-1) = 0  THEN
        ASSIGN  d-prec-sem-desc-db:SCREEN-VALUE IN FRAME f-fold-1 = pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1
                d-perc-desc-db:SCREEN-VALUE IN FRAME f-fold-1      = "".


    FIND FIRST empresa WHERE
        empresa.ep-codigo = pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela        NO-LOCK NO-ERROR.
    
    IF AVAIL empresa THEN
       ASSIGN c-nome-empresa:SCREEN-VALUE IN FRAME f-tela = empresa.nome.
    ELSE
       ASSIGN c-nome-empresa:SCREEN-VALUE IN FRAME f-tela = "".

    FIND FIRST ped-venda WHERE
        ped-venda.nr-pedido = int(pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)
        NO-LOCK NO-ERROR.
    
    IF AVAIL ped-venda THEN
       ASSIGN c-nome-abrev:SCREEN-VALUE IN FRAME f-tela  = ped-venda.nome-abrev
              c-cod-estabel:SCREEN-VALUE IN FRAME f-tela = ped-venda.cod-estabel.
    ELSE
       ASSIGN c-nome-abrev:SCREEN-VALUE IN FRAME f-tela  = ""
              c-cod-estabel:SCREEN-VALUE IN FRAME f-tela = "".
    
    IF AVAIL ped-venda THEN
        FIND FIRST ped-item OF ped-venda WHERE
             ped-item.nr-sequencia = int(pd-compl-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)      AND
             ped-item.ind-componen < 3 NO-LOCK NO-ERROR.
    
        IF AVAIL ped-item THEN 
            ASSIGN c-it-codigo:SCREEN-VALUE IN FRAME f-tela  = ped-item.it-codigo.
        ELSE
           ASSIGN c-it-codigo:SCREEN-VALUE IN FRAME f-tela       = "".

    FIND FIRST estabelec WHERE
        estabelec.cod-estabel = pd-compl-pedido.cod-estabel-prod:SCREEN-VALUE IN FRAME f-fold-1
        NO-LOCK NO-ERROR.

    IF AVAIL estabelec THEN
       ASSIGN c-nome-estabel-prod:SCREEN-VALUE IN FRAME f-fold-1  = estabelec.nome.
    ELSE
       ASSIGN c-nome-estabel-prod:SCREEN-VALUE IN FRAME f-fold-1  = "".

    FIND FIRST estabelec WHERE
        estabelec.cod-estabel = pd-compl-pedido.cod-estabel-fat:SCREEN-VALUE IN FRAME f-fold-1
        NO-LOCK NO-ERROR.

    IF AVAIL estabelec THEN
       ASSIGN c-nome-estabel-fat:SCREEN-VALUE IN FRAME f-fold-1   = estabelec.nome.
    ELSE
       ASSIGN c-nome-estabel-fat:SCREEN-VALUE IN FRAME f-fold-1  = "".

    FIND FIRST natur-oper WHERE
        natur-oper.nat-operacao = pd-compl-pedido.nat-operacao-fat:SCREEN-VALUE IN FRAME f-fold-1
        NO-LOCK NO-ERROR.

    IF AVAIL natur-oper THEN
       ASSIGN c-nome-natureza-fat:SCREEN-VALUE IN FRAME f-fold-1   = natur-oper.denominacao.
    ELSE
       ASSIGN c-nome-natureza-fat:SCREEN-VALUE IN FRAME f-fold-1  = "".

    FIND FIRST natur-oper WHERE
        natur-oper.nat-operacao = pd-compl-pedido.nat-operacao-prod:SCREEN-VALUE IN FRAME f-fold-1
        NO-LOCK NO-ERROR.

    IF AVAIL natur-oper THEN
       ASSIGN c-nome-natureza-prod:SCREEN-VALUE IN FRAME f-fold-1   = natur-oper.denominacao.
    ELSE
       ASSIGN c-nome-natureza-prod:SCREEN-VALUE IN FRAME f-fold-1  = "".

    ASSIGN pd-compl-pedido.preco-venda-calc-fat:VISIBLE = NO
           preco-venda-icms-prod:VISIBLE = NO.
       
     APPLY "LEAVE" TO pd-compl-pedido.preco-pis-cof IN FRAME f-fold-1.

END PROCEDURE.

PROCEDURE pi-le-primeiro.

    FIND FIRST pd-compl-pedido NO-LOCK NO-ERROR.

        IF AVAIL pd-compl-pedido THEN DO:

           RUN pi-mostra-registro.

        END.

END PROCEDURE.


PROCEDURE pi-le-proximo.

    FIND FIRST pd-compl-pedido WHERE
        pd-compl-pedido.ep-codigo    = pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela AND
        pd-compl-pedido.nr-pedido    = int(pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela) AND
        pd-compl-pedido.nr-sequencia = int(pd-compl-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)
        NO-LOCK NO-ERROR.

    FIND NEXT pd-compl-pedido NO-LOCK NO-ERROR.

        IF AVAIL pd-compl-pedido THEN DO:

           RUN pi-mostra-registro.

        END.

END PROCEDURE.


PROCEDURE pi-le-anterior.

    FIND FIRST pd-compl-pedido WHERE
        pd-compl-pedido.ep-codigo    = pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela AND
        pd-compl-pedido.nr-pedido    = int(pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela) AND
        pd-compl-pedido.nr-sequencia = int(pd-compl-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)
        NO-LOCK NO-ERROR.

    FIND PREV pd-compl-pedido NO-LOCK NO-ERROR.

        IF AVAIL pd-compl-pedido THEN DO:

           RUN pi-mostra-registro.

        END.

END PROCEDURE.

PROCEDURE pi-le-ultimo.

    FIND LAST pd-compl-pedido NO-LOCK NO-ERROR.

        IF AVAIL pd-compl-pedido THEN DO:

           RUN pi-mostra-registro.

        END.

END PROCEDURE.



PROCEDURE le-registro-goto.

    FIND FIRST bf-pd-compl-pedido WHERE
        bf-pd-compl-pedido.ep-codigo    = i-ep-codigo-jr AND
        bf-pd-compl-pedido.nr-pedido    = i-nr-pedido-jr AND
        bf-pd-compl-pedido.nr-sequencia = i-nr-sequencia-jr 
        NO-LOCK NO-ERROR.

    IF NOT AVAIL bf-pd-compl-pedido THEN DO:
        ASSIGN c-mensagem-jr = "REGISTRO N«O EXISTE!".
        RUN pi-mostra-mensagem.
        RETURN.
    END.

    FIND FIRST pd-compl-pedido WHERE
        pd-compl-pedido.ep-codigo    = i-ep-codigo-jr AND
        pd-compl-pedido.nr-pedido    = i-nr-pedido-jr AND
        pd-compl-pedido.nr-sequencia = i-nr-sequencia-jr 
        NO-LOCK NO-ERROR.

    IF AVAIL pd-compl-pedido THEN
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

    ASSIGN pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela     = ""
           pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela     = ""
           pd-compl-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela  = ""
           pd-compl-pedido.cod-emitente:SCREEN-VALUE IN FRAME f-tela  = "".


    ASSIGN pd-compl-pedido.cod-estabel-prod:SCREEN-VALUE IN FRAME f-fold-1      = "" 
           pd-compl-pedido.cod-estabel-fat:SCREEN-VALUE IN FRAME f-fold-1       = "" 
           pd-compl-pedido.nat-operacao-prod:SCREEN-VALUE IN FRAME f-fold-1     = "" 
           pd-compl-pedido.nat-operacao-fat:SCREEN-VALUE IN FRAME f-fold-1      = "" 
           pd-compl-pedido.perc-icms-prod:SCREEN-VALUE IN FRAME f-fold-1        = "" 
           pd-compl-pedido.perc-icms-fat:SCREEN-VALUE IN FRAME f-fold-1         = "" 
           pd-compl-pedido.perc-enc-finan:SCREEN-VALUE IN FRAME f-fold-1        = "" 
           pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1         = "" 
           d-prec-sem-desc-db:SCREEN-VALUE IN FRAME f-fold-1                    = "" 
           d-perc-desc-db:SCREEN-VALUE IN FRAME f-fold-1                        = "" 
           pd-compl-pedido.preco-venda-calc-prod:SCREEN-VALUE IN FRAME f-fold-1 = "" 
           pd-compl-pedido.preco-venda-calc-fat:SCREEN-VALUE IN FRAME f-fold-1  = "" 
           preco-venda-icms-prod:SCREEN-VALUE IN FRAME f-fold-1                 = "" 
           preco-venda-icms-fat:SCREEN-VALUE IN FRAME f-fold-1                  = "" 
           pd-compl-pedido.dt-faturamento:SCREEN-VALUE IN FRAME f-fold-1        = "" 
           pd-compl-pedido.dt-entrega-cli:SCREEN-VALUE IN FRAME f-fold-1        = "" 
           pd-compl-pedido.narrativa:SCREEN-VALUE IN FRAME f-fold-1             = "" 
           pd-compl-pedido.tp-atendimento:SCREEN-VALUE IN FRAME f-fold-1        = ""
           d-prec-sem-desc-db:SCREEN-VALUE IN FRAME f-fold-1  = ""
           d-perc-desc-db:SCREEN-VALUE IN FRAME f-fold-1      = ""
        
        .

    ASSIGN c-nome-empresa:SCREEN-VALUE IN FRAME f-tela     = ""
           c-nome-abrev:SCREEN-VALUE IN FRAME f-tela       = ""
           c-it-codigo:SCREEN-VALUE IN FRAME f-tela        = ""
           c-cod-estabel:SCREEN-VALUE IN FRAME f-tela      = "".
               
    ASSIGN c-nome-natureza-prod:SCREEN-VALUE IN FRAME f-fold-1 = ""
           c-nome-natureza-fat:SCREEN-VALUE IN FRAME f-fold-1  = ""
           c-nome-estabel-prod:SCREEN-VALUE IN FRAME f-fold-1  = ""
           c-nome-estabel-fat:SCREEN-VALUE IN FRAME f-fold-1   = "".

END PROCEDURE.

PROCEDURE pi-enable-campos.

                   /*aqui tirar esta linha */
                  /* ASSIGN l-pd4000 = YES.*/

define var l-432-polo as logical no-undo.
define buffer b1-ped-venda for ped-venda.

    IF l-pd4000  = NO AND
       l-espd0065 = NO THEN RETURN NO-APPLY.
    
    FIND first b-ped-venda where b-ped-venda.nr-pedido = int(pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)  NO-LOCK NO-ERROR.
    
    IF NOT AVAIL b-ped-venda THEN RETURN NO-APPLY.
    
    FIND FIRST if-ped-venda WHERE
            if-ped-venda.nr-pedido = b-ped-venda.nr-pedido AND
            if-ped-venda.nr-pedido-relac <> 0
            NO-LOCK NO-ERROR.
        
    IF AVAIL if-ped-venda THEN DO:
      FIND first b-ped-venda where b-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac  NO-LOCK NO-ERROR.
    
      IF NOT AVAIL b-ped-venda THEN RETURN NO-APPLY.
    END.



    FIND first b-ped-item of b-ped-venda where b-ped-item.ind-componen < 3 and
                                             b-ped-item.nr-sequencia = int(pd-compl-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela) NO-LOCK NO-ERROR.
    
    IF  avail b-ped-item  AND (b-ped-venda.cod-sit-ped >= 3 or b-ped-item.cod-sit-item >= 3 )THEN RETURN NO-APPLY.

     


    IF c-tipo-botao <> "altera" THEN
       ENABLE pd-compl-pedido.ep-codigo
              pd-compl-pedido.nr-pedido
              pd-compl-pedido.nr-sequencia
           WITH FRAME f-tela.

    ENABLE pd-compl-pedido.preco-pis-cof
           /*pd-compl-pedido.cod-estabel-fat*/

           pd-compl-pedido.lib-faturamento
           pd-compl-pedido.log-1
           pd-compl-pedido.tp-atendimento
           pd-compl-pedido.narrativa
           pd-compl-pedido.dt-entrega-cli
           pd-compl-pedido.dt-faturamento

           pd-compl-pedido.preco-pis-cof 
           d-prec-sem-desc-db
           d-perc-desc-db
           WITH FRAME f-fold-1.
         
 
    IF l-pd4000 = NO THEN
        DISABLE pd-compl-pedido.preco-pis-cof 
                d-prec-sem-desc-db
                d-perc-desc-db
            WITH FRAME f-fold-1.





  /*  run pi-verifica-if-nor (input INPUT FRAME f-fold-1 pd-compl-pedido.cod-estabel-prod, 
                            input c-it-codigo:SCREEN-VALUE IN FRAME f-tela , 
                            input INPUT FRAME f-fold-1 pd-compl-pedido.nat-operacao-prod, 
                            input int(pd-compl-pedido.cod-emitente:SCREEN-VALUE IN FRAME f-tela)).
    */
           /*



    pd-compl-pedido.cod-estabel-fat:sensitive = no.  /*l-estab-ativo.        */
   
    FIND FIRST if-ped-venda WHERE
            if-ped-venda.nr-pedido = int(pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)
            NO-LOCK NO-ERROR.
        
    IF AVAIL if-ped-venda THEN DO:
            pd-compl-pedido.cod-estabel-fat:sensitive = no.
    end.
    else    do:
            FIND first if-ped-venda WHERE
                  if-ped-venda.nr-pedido-relac = int(pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)
                NO-LOCK NO-ERROR.
         

            IF AVAIL if-ped-venda THEN do:
                 assign
                     pd-compl-pedido.cod-estabel-fat:sensitive = no
                     pd-compl-pedido.preco-pis-cof:SENSITIVE   = no
                     d-prec-sem-desc-db:SENSITIVE   = no  
                     d-perc-desc-db:SENSITIVE       = no  .  
                     
                  l-432-polo = no.   
                  for first b1-ped-venda where  b1-ped-venda.nr-pedido = if-ped-venda.nr-pedido and (b1-ped-venda.cod-estabel = "422" OR b1-ped-venda.cod-estabel = "412") no-lock. /*solic-318*/ 
                      l-432-polo = yes.                     
                  end.
                     
                  for first b-ped-venda where b-ped-venda.nr-pedido = if-ped-venda.nr-pedido  no-lock,
                     first b-ped-item of b-ped-venda where b-ped-item.ind-componen < 3 and
                                                           b-ped-item.nr-sequencia = int(pd-compl-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela) no-lock.
                  
                     if b-ped-venda.cod-sit-ped >= 3 or b-ped-item.cod-sit-item >= 3 then 
                        ASSIGN pd-compl-pedido.preco-pis-cof:sensitive = YES 
                               d-prec-sem-desc-db:sensitive = yes  
                               d-perc-desc-db:SENSITIVE     = yes .
                               
                  end.
                  
                  if l-432-polo then 
                      ASSIGN pd-compl-pedido.preco-pis-cof:sensitive = YES 
                             d-prec-sem-desc-db:SENSITIVE   = yes  
                             d-perc-desc-db:SENSITIVE       = yes .
                     
            end.   
    end.

    pd-compl-pedido.cod-estabel-fat:sensitive = no.
    pd-compl-pedido.nat-operacao-fat:sensitive = no.
    pd-compl-pedido.cod-estabel-fat:visible = no.
    pd-compl-pedido.nat-operacao-fat:visible = no.
*/

    IF c-tipo-botao <> "altera" THEN
       APPLY "entry" TO pd-compl-pedido.ep-codigo IN FRAME f-tela.
   /* ELSE
       IF c-tipo-botao = "altera" AND l-pd4000 = YES THEN
       APPLY "entry" TO pd-compl-pedido.cod-estabel-prod IN FRAME f-fold-1.
       ELSE
           APPLY "entry" TO pd-compl-pedido.cod-estabel-fat IN FRAME f-fold-1.
     */
END PROCEDURE.

PROCEDURE pi-disable-campos.

    DISABLE  pd-compl-pedido.ep-codigo    
             pd-compl-pedido.nr-pedido    
             pd-compl-pedido.nr-sequencia 
        WITH FRAME f-tela.

    DISABLE pd-compl-pedido.preco-pis-cof   
            d-prec-sem-desc-db
            d-perc-desc-db
            pd-compl-pedido.cod-estabel-fat 
            pd-compl-pedido.lib-faturamento
            pd-compl-pedido.log-1 
            pd-compl-pedido.tp-atendimento  
            pd-compl-pedido.narrativa       
            pd-compl-pedido.dt-entrega-cli  
            pd-compl-pedido.dt-faturamento
        WITH FRAME f-fold-1.

END PROCEDURE.

PROCEDURE pi-le-pela-chave.

    FIND FIRST pd-compl-pedido WHERE
        pd-compl-pedido.ep-codigo    = pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela AND
        pd-compl-pedido.nr-pedido    = int(pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela) AND
        pd-compl-pedido.nr-sequencia = int(pd-compl-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela)
        NO-LOCK NO-ERROR.

    IF AVAIL pd-compl-pedido AND (c-tipo-botao2 = "novo" OR c-tipo-botao2 = "copia") THEN DO:

        ASSIGN c-mensagem-jr = "Registro J† Existe".
        RUN pi-mostra-mensagem.
        RETURN "nok".     

    END.

    IF NOT AVAIL pd-compl-pedido AND c-tipo-botao2 = "altera" THEN DO:

        ASSIGN c-mensagem-jr = "Registro N∆o Existe".
        RUN pi-mostra-mensagem.
        RETURN "nok".

    END.

    RETURN "ok".

END PROCEDURE.


PROCEDURE pi-grava-registro.


    IF c-tipo-botao2 = "novo" OR c-tipo-botao2 = "copia" THEN DO:

       CREATE pd-compl-pedido.
       ASSIGN pd-compl-pedido.ep-codigo    = pd-compl-pedido.ep-codigo:SCREEN-VALUE IN FRAME f-tela              pd-compl-pedido.nr-pedido    = int(pd-compl-pedido.nr-pedido:SCREEN-VALUE IN FRAME f-tela)
              pd-compl-pedido.nr-sequencia = int(pd-compl-pedido.nr-sequencia:SCREEN-VALUE IN FRAME f-tela).

    END.

    IF c-tipo-botao2 = "altera" THEN DO:

        FIND CURRENT pd-compl-pedido EXCLUSIVE-LOCK NO-ERROR.

        IF NOT AVAIL pd-compl-pedido THEN DO:
            ASSIGN c-mensagem-jr = "Registro N∆o esta mais disponivel".
            RUN pi-mostra-mensagem.
            RETURN "nok".
        END.

    END.

    ASSIGN pd-compl-pedido.cod-estabel-prod      = STRING(pd-compl-pedido.cod-estabel-prod:SCREEN-VALUE IN FRAME f-fold-1)      
           pd-compl-pedido.cod-estabel-fat       = STRING(pd-compl-pedido.cod-estabel-fat:SCREEN-VALUE IN FRAME f-fold-1)       
           pd-compl-pedido.nat-operacao-prod     = STRING(pd-compl-pedido.nat-operacao-prod:SCREEN-VALUE IN FRAME f-fold-1)     
           pd-compl-pedido.nat-operacao-fat      = STRING(pd-compl-pedido.nat-operacao-fat:SCREEN-VALUE IN FRAME f-fold-1)      
           pd-compl-pedido.perc-icms-prod        = dec(pd-compl-pedido.perc-icms-prod:SCREEN-VALUE IN FRAME f-fold-1)        
           pd-compl-pedido.perc-icms-fat         = dec(pd-compl-pedido.perc-icms-fat:SCREEN-VALUE IN FRAME f-fold-1)         
           pd-compl-pedido.perc-enc-finan        = dec(pd-compl-pedido.perc-enc-finan:SCREEN-VALUE IN FRAME f-fold-1)        
           pd-compl-pedido.preco-pis-cof         = dec(pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1)         
           pd-compl-pedido.preco-venda-calc-prod = dec(pd-compl-pedido.preco-venda-calc-prod:SCREEN-VALUE IN FRAME f-fold-1) 
           pd-compl-pedido.preco-venda-calc-fat  = dec(pd-compl-pedido.preco-venda-calc-fat:SCREEN-VALUE IN FRAME f-fold-1)  
           pd-compl-pedido.dt-faturamento        = date(pd-compl-pedido.dt-faturamento:SCREEN-VALUE IN FRAME f-fold-1)        
           pd-compl-pedido.dt-entrega-cli        = date(pd-compl-pedido.dt-entrega-cli:SCREEN-VALUE IN FRAME f-fold-1)        
           pd-compl-pedido.narrativa             = STRING (pd-compl-pedido.narrativa:SCREEN-VALUE IN FRAME f-fold-1)             
           pd-compl-pedido.tp-atendimento        = int(pd-compl-pedido.tp-atendimento:SCREEN-VALUE IN FRAME f-fold-1).        

    ASSIGN substring(pd-compl-pedido.char-1,1,15)  = string(preco-venda-icms-prod:SCREEN-VALUE IN FRAME f-fold-1, "x(15)" ) 
           substring(pd-compl-pedido.char-1,16,15) = string(preco-venda-icms-fat:SCREEN-VALUE IN FRAME f-fold-1, "x(15)" ).

    ASSIGN substring(pd-compl-pedido.char-1,31,10) = string(d-perc-desc-db:SCREEN-VALUE IN FRAME f-fold-1, "x(10)" ) 
           substring(pd-compl-pedido.char-1,41,10) = string(d-prec-sem-desc-db:SCREEN-VALUE IN FRAME f-fold-1, "x(10)" ).


    ASSIGN pd-compl-pedido.lib-faturamento       = INPUT FRAME f-fold-1 pd-compl-pedido.lib-faturamento.
    ASSIGN pd-compl-pedido.log-1                 = INPUT FRAME f-fold-1 pd-compl-pedido.log-1.


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
      /*    bt-deleta:SENSITIVE in frame f-tela     = no  */
      /*    bt-sai:SENSITIVE in frame f-tela        = no  */ . 
   
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
       /*   bt-deleta:SENSITIVE in frame f-tela     = no */
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
             pd-compl-pedido SCROLLING.

       DEFINE BROWSE br-pesquisa
         QUERY br-pesquisa DISPLAY
           pd-compl-pedido.ep-codigo 
           pd-compl-pedido.nr-pedido 
           pd-compl-pedido.nr-sequencia 
           pd-compl-pedido.cod-estabel-prod
           pd-compl-pedido.nat-operacao-prod
           pd-compl-pedido.preco-pis-cof
           pd-compl-pedido.perc-enc-finan
           pd-compl-pedido.cod-estabel-fat
           pd-compl-pedido.nat-operacao-fat
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
                THREE-D SCROLLABLE TITLE "Configuraá∆o de Pedidos" FONT 1
                DEFAULT-BUTTON pq-bt-ok CANCEL-BUTTON pq-bt-cancel.

      

       on "entry" OF i-ep-codigo-ini in frame gt-frame-pesq do:

           CLOSE QUERY br-pesquisa.

           open query br-pesquisa 
               for each pd-compl-pedido where
               pd-compl-pedido.ep-codigo    >= i-ep-codigo-ini:SCREEN-VALUE IN FRAME gt-frame-pesq     AND
               pd-compl-pedido.ep-codigo    <= i-ep-codigo-fim:SCREEN-VALUE IN FRAME gt-frame-pesq     and
               pd-compl-pedido.nr-pedido    >= int(i-nr-pedido-ini:SCREEN-VALUE IN FRAME gt-frame-pesq)     AND
               pd-compl-pedido.nr-pedido    <= int(i-nr-pedido-fim:SCREEN-VALUE IN FRAME gt-frame-pesq)     AND
               pd-compl-pedido.nr-sequencia >= int(i-nr-sequencia-ini:SCREEN-VALUE IN FRAME gt-frame-pesq)  AND
               pd-compl-pedido.nr-sequencia <= int(i-nr-sequencia-fim:SCREEN-VALUE IN FRAME gt-frame-pesq).

             if  br-pesquisa:num-selected-rows IN FRAME gt-frame-pesq > 0 then do on error undo, return no-apply:
                 get current br-pesquisa.

                display
                    pd-compl-pedido.ep-codigo 
                    pd-compl-pedido.nr-pedido 
                    pd-compl-pedido.nr-sequencia 
                    pd-compl-pedido.cod-estabel-prod
                    pd-compl-pedido.nat-operacao-prod
                    pd-compl-pedido.preco-pis-cof
                    pd-compl-pedido.perc-enc-finan
                    pd-compl-pedido.cod-estabel-fat
                    pd-compl-pedido.nat-operacao-fat
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
           
               ASSIGN i-ep-codigo-jr    = pd-compl-pedido.ep-codigo
                      i-nr-pedido-jr    = pd-compl-pedido.nr-pedido
                      i-nr-sequencia-jr = pd-compl-pedido.nr-sequencia.

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
           open query br-pesquisa for each pd-compl-pedido where
               pd-compl-pedido.ep-codigo    >= i-ep-codigo-ini:SCREEN-VALUE IN FRAME gt-frame-pesq     AND
               pd-compl-pedido.ep-codigo    <= i-ep-codigo-fim:SCREEN-VALUE IN FRAME gt-frame-pesq     and
               pd-compl-pedido.nr-pedido    >= int(i-nr-pedido-ini:SCREEN-VALUE IN FRAME gt-frame-pesq)     AND
               pd-compl-pedido.nr-pedido    <= int(i-nr-pedido-fim:SCREEN-VALUE IN FRAME gt-frame-pesq)     AND
               pd-compl-pedido.nr-sequencia >= int(i-nr-sequencia-ini:SCREEN-VALUE IN FRAME gt-frame-pesq)  AND
               pd-compl-pedido.nr-sequencia <= int(i-nr-sequencia-fim:SCREEN-VALUE IN FRAME gt-frame-pesq).  

           if  br-pesquisa:num-selected-rows IN FRAME gt-frame-pesq > 0 then do on error undo, return no-apply:

               get current br-pesquisa.

               display
                   pd-compl-pedido.ep-codigo 
                   pd-compl-pedido.nr-pedido 
                   pd-compl-pedido.nr-sequencia 
                   pd-compl-pedido.cod-estabel-prod
                   pd-compl-pedido.nat-operacao-prod
                   pd-compl-pedido.preco-pis-cof
                   pd-compl-pedido.perc-enc-finan
                   pd-compl-pedido.cod-estabel-fat
                   pd-compl-pedido.nat-operacao-fat
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


PROCEDURE pi-cria-compl-atual.


    FIND FIRST natur-oper WHERE
        natur-oper.nat-operacao = IF AVAIL ped-item THEN ped-item.nat-operacao ELSE ped-venda.nat-operacao
        NO-LOCK NO-ERROR.

    FIND FIRST estabelec WHERE
        estabelec.cod-estabel = ped-venda.cod-estabel
        NO-LOCK NO-ERROR.

    FIND FIRST emitente WHERE
        emitente.cod-emitente = ped-venda.cod-emitente
        NO-LOCK NO-ERROR.

   
    ASSIGN  d-perc-icms-db = 0
            d-perc-desc-icms-db =  IF AVAIL natur-oper THEN (100 - natur-oper.per-des-icms) / 100 ELSE 0.

    if AVAIL  natur-oper AND  natur-oper.cd-trib-icm = 1 then do:

         IF emitente.estado = estabelec.estado  THEN
             d-perc-icms-db = 18.
         ELSE
             d-perc-icms-db = 12.

         FIND FIRST unid-feder WHERE unid-feder.pais   = ESTABELEC.pais   AND
                                     unid-feder.estado = estabelec.estado 
              NO-LOCK NO-ERROR.

         IF  AVAIL unid-feder THEN DO:

            IF emitente.estado = estabelec.estado THEN
               ASSIGN d-perc-icms-db = unid-feder.per-icms-int.

            ELSE DO:

                ASSIGN d-perc-icms-db = unid-feder.per-icms-ext.

                DO i-exc = 1 TO 25:

                    IF unid-feder.est-exc [i-exc] =  "" THEN NEXT.

                    IF unid-feder.est-exc [i-exc] = emitente.estado THEN DO:

                        ASSIGN d-perc-icms-db = unid-feder.perc-exc [i-exc].
                        LEAVE.

                    END.

                END.

            END. 

         END.

    end.   /* tributado icm */

    if AVAIL  natur-oper AND natur-oper.cd-trib-icm = 4 then do:

         IF emitente.estado = estabelec.estado  THEN
             assign d-perc-icms-db = 12.

    end.   


    ASSIGN i-nr-tab-finan = ped-venda.nr-tab-finan
           i-nr-ind-finan = ped-venda.nr-ind-finan.
                  
    FIND FIRST tab-finan-indice
          WHERE tab-finan-indice.nr-tab-finan = i-nr-tab-finan
            AND tab-finan-indice.num-seq      = i-nr-ind-finan NO-LOCK NO-ERROR.
    
    IF AVAIL tab-finan-indice THEN
        assign d-perc-encargo = (tab-finan-indice.tab-ind-fin - 1) * 100.

    ASSIGN pd-compl-pedido.cod-estabel-prod  = ped-venda.cod-estabel
           pd-compl-pedido.nat-operacao-prod = IF AVAIL  natur-oper THEN natur-oper.nat-operacao ELSE ""
           pd-compl-pedido.lib-faturamento   = NO
           pd-compl-pedido.log-1             = NO
           pd-compl-pedido.dt-faturamento    = ped-venda.dt-entrega
           pd-compl-pedido.dt-entrega-cli    = ped-venda.dt-entrega
           pd-compl-pedido.perc-icms-prod    = d-perc-icms-db
           pd-compl-pedido.perc-enc-finan    = d-perc-encargo.
           

    IF DEC(pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1) <> 0 THEN DO:
           MESSAGE "entrou com preco diferente de zero na inclus∆o"
               VIEW-AS ALERT-BOX INFO BUTTONS OK.

        FIND FIRST para-fat NO-LOCK NO-ERROR.


        ASSIGN preco-jr = 0
               preco-jr = DEC(pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1).
               
               IF para-fat.ind-preco = YES THEN 
                 preco-jr = preco-jr / ((100 - DEC(pd-compl-pedido.perc-icms-prod:SCREEN-VALUE IN FRAME f-fold-1)) / 100).
                 
               preco-jr = preco-jr * (1 + (DEC(pd-compl-pedido.perc-enc-finan:SCREEN-VALUE IN FRAME f-fold-1)) / 100).
        
               IF para-fat.ind-preco = YES THEN 
                        ASSIGN pd-compl-pedido.preco-venda-calc-prod:SCREEN-VALUE IN FRAME f-fold-1 = STRING(preco-jr / d-perc-desc-icms-db).
              else  
                        ASSIGN pd-compl-pedido.preco-venda-calc-prod:SCREEN-VALUE IN FRAME f-fold-1 = STRING(preco-jr). 


        /* Calcula o preáo com icmcs para consulta na tela - Est. Produá∆o */

            ASSIGN preco-2-jr = 0
                   preco-2-jr = DEC(pd-compl-pedido.preco-pis-cof:SCREEN-VALUE IN FRAME f-fold-1).

            ASSIGN preco-2-jr = preco-2-jr / ((100 - DEC(pd-compl-pedido.perc-icms-prod:SCREEN-VALUE IN FRAME f-fold-1)) / 100).

            ASSIGN preco-2-jr = preco-2-jr * (1 + (DEC(pd-compl-pedido.perc-enc-finan:SCREEN-VALUE IN FRAME f-fold-1)) / 100).

            ASSIGN preco-venda-icms-prod:SCREEN-VALUE IN FRAME f-fold-1 = STRING(preco-2-jr / d-perc-desc-icms-db).

                               
        /* ------------------------------------------------------------------- */
        
    END.

END PROCEDURE.

/*
PROCEDURE pi-verifica-if-nor :
/*------------------------------------------------------------------------------
  Purpose:     Verifica se existem itens incentivados e itens normais no mesmo pedido
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param p-cod-estabel  as char no-undo.
def input param p-it-codigo    as char no-undo.
def input param p-nat-operacao as char no-undo.
def input param p-cod-emitente as integer no-undo.

def buffer bf-item for item .

    
    ASSIGN  
           l-estab-ativo = yes.

    
    FOR  FIRST bf-item NO-LOCK
       WHERE bf-item.it-codigo = p-it-codigo:
       
 

        for FIRST if-natur-fam NO-LOCK
             WHERE if-natur-fam.cod-estab-orig  = p-cod-estabel
               AND if-natur-fam.nat-oper-pedido = p-nat-operacao
               AND if-natur-fam.fm-codigo       = bf-item.fm-codigo.
               
          if not can-FIND (FIRST if-natur-item NO-LOCK
               WHERE if-natur-item.cod-estab-orig  = p-cod-estabel
                 AND if-natur-item.nat-oper-pedido = p-nat-operacao
                 AND if-natur-item.it-codigo       = p-it-codigo) and 
                 not can-FIND (FIRST if-exec-cli NO-LOCK
           WHERE if-exec-cli.cod-emitente = p-cod-emitente)
       
  
          THEN DO:
              ASSIGN l-estab-ativo = no.
          END.
        end.  
    END.

    
END PROCEDURE. /* pi-verifica-if-nor */
*/

PROCEDURE pi-acha-perc-icms.    /* Encontra % de icms do pedido de venda */

    ASSIGN perc-icms-faturx      = 0
           perc-desc-icms-faturx = 0.

    FIND FIRST bf-estabelec WHERE
        bf-estabelec.cod-estabel = estab-faturx
        NO-LOCK NO-ERROR.                                                                      

    IF AVAIL bf-estabelec THEN DO:

       FIND FIRST bf-emitente WHERE
           bf-emitente.cod-emitente = cod-emitente-faturx
           NO-LOCK NO-ERROR.

       FIND FIRST bf-natur-oper WHERE
           bf-natur-oper.nat-operacao = nat-operacao-faturx
           NO-LOCK NO-ERROR.
       
       ASSIGN  perc-icms-faturx = 0
               perc-desc-icms-faturx = IF AVAIL bf-natur-oper THEN bf-natur-oper.per-des-icms ELSE 0.
       
       if AVAIL bf-natur-oper AND bf-natur-oper.cd-trib-icm = 1 then do:
       
            IF bf-emitente.estado = bf-estabelec.estado  THEN
                perc-icms-faturx = 18.
            ELSE
                perc-icms-faturx = 12.
            FIND FIRST unid-feder WHERE unid-feder.pais   = bf-estabelec.pais   AND
                                        unid-feder.estado = bf-estabelec.estado 
                 NO-LOCK NO-ERROR.
       
            IF  AVAIL unid-feder THEN DO:
       
               IF bf-emitente.estado = bf-estabelec.estado THEN
                  ASSIGN perc-icms-faturx = unid-feder.per-icms-int.
       
               ELSE DO:
       
                   ASSIGN perc-icms-faturx = unid-feder.per-icms-ext.
       
                   DO i-exc = 1 TO 25:
       
                       IF unid-feder.est-exc [i-exc] =  "" THEN NEXT.
       
                       IF unid-feder.est-exc [i-exc] = bf-emitente.estado THEN DO:
       
                           ASSIGN perc-icms-faturx = unid-feder.perc-exc [i-exc].
                           LEAVE.
       
                       END.
       
                   END.
       
               END. 
       
            END.
          
       end.   /* tributado icm */
          
       if AVAIL bf-natur-oper AND bf-natur-oper.cd-trib-icm = 4 then do:
       
            IF bf-emitente.estado = bf-estabelec.estado  THEN
                assign perc-icms-faturx = 12.
       
       end. 

    END.

END PROCEDURE.

