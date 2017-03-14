&GLOBAL-DEFINE aplica_facelift YES
&Scoped-define WINDOW-NAME C-Win

/*------------------------------------------------------------------------
File.............: espd0065.w
Description......: Gerenciador de Pedidos de Vendas 
Input Parameters : 
Output Parameters: 
Author...........: Amgra - JosÇ Roberto..
Created..........: 01/09/2011   
OBS..............: 
------------------------------------------------------------------------*/
DEFINE VARIABLE r-codigo-rwi  AS ROWID                     NO-UNDO.

define buffer if-ped-venda    for espmulti.if-ped-venda. 
define buffer bf-if-ped-venda for espmulti.if-ped-venda   .        
define buffer b-ped-venda for ped-venda.
define buffer b-estabelec for estabelec. 
define buffer b1-estabelec for estabelec. 
DEFINE BUFFER BF-pd-hist-pedido FOR pd-hist-pedido.

/* buffers para % de icms na confirmaá∆o do pedido */
DEF BUFFER bf-estabelec  FOR estabelec.
DEF BUFFER bf-emitente   FOR emitente.
DEF BUFFER bf-natur-oper FOR natur-oper. 



define variable c-prog-gerado as character no-undo initial "espd0065".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

CREATE WIDGET-POOL. 

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */

&GLOBAL-DEFINE PGSEL f-pg-sel 
&GLOBAL-DEFINE PGPAR f-pg-par 
&GLOBAL-DEFINE PGDIG f-pg-dig 
&GLOBAL-DEFINE PGIMP f-pg-imp 

/* Include Com as Vari†veis Globais */

def new global shared var gr-ped-venda AS ROWID  no-undo.
def new global shared var gr-ped-item  as rowid no-undo.


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
{method/dbotterr.i}

    DEFINE TEMP-TABLE tt-ped-assunto no-undo
        FIELD nr-pedido            AS INT     FORMAT ">>>>>>>9"       LABEL "Nr.Pedido"
        field nr-pedcli             as char
        INDEX chave IS PRIMARY UNIQUE nr-pedido.

    DEFINE TEMP-TABLE tt-pedido no-undo
        field marca                AS char    FORMAT "x(3)"           LABEL "Sel"
        FIELD cod-estabel          AS CHAR    FORMAT "x(3)"           LABEL "Es Ped"
        FIELD cod-estabel-fat      AS CHAR    FORMAT "x(3)"           LABEL "Es.Fat"
        field unig-com           as char    format "x(02)"          label "UC"
        FIELD nr-pedido            AS INT     FORMAT ">>>>>>>9"       LABEL "Nr.Pedido"
        FIELD nr-sequencia         AS INT     FORMAT ">>>>>9"         LABEL "Sq.Pd"
        FIELD nr-pedcli            AS CHAR    FORMAT "x(12)"          LABEL "Nr.PedFat"
        FIELD tp-pedido            AS CHAR    FORMAT "x(1)"           LABEL "Tp "
        FIELD it-codigo            AS CHAR    FORMAT "x(16)"          LABEL "Item"
        FIELD nome-abrev           AS CHAR    FORMAT "x(12)"          LABEL "Nome Cliente"
        FIELD nr-pedcli-cli        AS CHAR    FORMAT "x(12)"          LABEL "Ped.Cliente"
        FIELD cod-emitente         AS INT     FORMAT ">>>>>>>9"       LABEL "Cd.Cliente"
        FIELD qt-pedida            AS DEC     FORMAT ">>>>>>>9.9999"  label "Qt.Pedida"
        FIELD qt-atendida          AS DEC     FORMAT ">>>>>>>9.9999"  LABEL "Qt.Atendida"
        FIELD qt-configurada       AS DEC     FORMAT ">>>>>>>9.9999"  LABEL "Qt.Ped.Clie."
        FIELD qt-bobinas           AS INT     FORMAT ">>>>9"          LABEL "QtBob"
        FIELD qt-produzida         AS DEC     FORMAT ">>>>>>>9.9999"  LABEL "Qt.Produzida"
        FIELD nr-pedido-final      AS INT     FORMAT ">>>>>>>9"       LABEL "Ped.Final"
        FIELD nome-abrev-final     AS CHAR    FORMAT "x(12)"          LABEL "Cliente Final"
        FIELD qt-pedida-final      AS DEC     FORMAT ">>>>>>>9.9999"  label "Qt.Ped.Final"
        FIELD qt-atendida-final    AS DEC     FORMAT ">>>>>>>9.9999"  label "Qt.Atd.Final"
        FIELD larg                 AS INT     FORMAT ">>>>9"          label "Larg "
        FIELD diex                 AS INT     FORMAT ">>>>9"          label "Diex "
        FIELD diin                 AS INT     FORMAT ">>>>9"          label "Diin "
        FIELD dt-entrega           AS DATE    FORMAT "99/99/9999"     LABEL "Dt.Prod"
        FIELD dt-faturamento       AS DATE    FORMAT "99/99/9999"     LABEL "Dt.fatur"
        FIELD dt-entrega-cli       AS DATE    FORMAT "99/99/9999"     LABEL "Dt.Entr.Cli"
        FIELD nat-operacao         AS CHAR    FORMAT "x(7)"           LABEL "N.Oper  "
        FIELD nat-operacao-fat     AS CHAR    FORMAT "x(7)"           LABEL "N.Op.Fat"
        FIELD mercado              AS CHAR    FORMAT "x(3)"           LABEL "Merc "
        FIELD canal-venda          AS INT     FORMAT ">9"             LABEL "CV"
        FIELD situacao             AS CHAR    FORMAT "x(03)"          LABEL "Sit"
        FIELD preco-ex-imp         AS DEC     FORMAT ">>>>>>>>9.99"   LABEL "Pr.Ex.Imposto"
        FIELD preco-com-icm        AS DEC     FORMAT ">>>>>>>>9.99"   LABEL "Pr.com ICM"
        FIELD perc-desc-preco      AS DEC     FORMAT ">>>>9.99"       LABEL "% Desc"
        FIELD liber-fat            AS CHAR    FORMAT "x(3)"           LABEL "Lib Fat"
        FIELD enviado-email        AS CHAR    FORMAT "x(3)"           LABEL "Email Cli"
        INDEX chave IS PRIMARY UNIQUE nome-abrev
                                      nr-pedido
                                      nr-sequencia.




/************************************************************************************/

/* Transfer Definitions */

    DEFINE QUERY br-pedido FOR 
          tt-pedido SCROLLING.
                   

        /* Browse definitions                                                   */
    DEFINE BROWSE br-pedido
      QUERY br-pedido  DISPLAY
        tt-pedido.marca
        tt-pedido.cod-estabel       width 5
        tt-pedido.cod-estabel-fat  width 5  
        tt-pedido.liber-fat      width 5
        tt-pedido.unig-com       width 2   COLUMN-BGCOLOR 11
        tt-pedido.tp-pedido       width 2  FORMAT "X(3)" 
        tt-pedido.nr-pedido         width 7
        tt-pedido.nr-pedido-final width 7 COLUMN-BGCOLOR 11
        tt-pedido.nr-sequencia   width 4    format ">>9"
        tt-pedido.nr-pedcli         FORMAT "X(8)" 
        tt-pedido.dt-entrega
        tt-pedido.nome-abrev        FORMAT "X(12)" 
        tt-pedido.nome-abrev-final COLUMN-BGCOLOR 11
        tt-pedido.nr-pedcli-cli     FORMAT "X(12)" 

           
          
        tt-pedido.enviado-email width 4.8
        tt-pedido.it-codigo      width 13   FORMAT "X(20)" 
        tt-pedido.qt-pedida      width 9   
        tt-pedido.qt-atendida     width 9   
        tt-pedido.qt-configurada WIDTH 9   COLUMN-BGCOLOR 11
        tt-pedido.qt-bobinas WIDTH 4.5   COLUMN-BGCOLOR 11
        tt-pedido.qt-produzida   WIDTH 9
        tt-pedido.larg 
        tt-pedido.qt-pedida-final    
        tt-pedido.qt-atendida-final
                     
        tt-pedido.diin          
        tt-pedido.diex 
        tt-pedido.preco-ex-imp   
        tt-pedido.perc-desc-preco
        tt-pedido.mercado      
        tt-pedido.sit  
        tt-pedido.dt-faturamento
        tt-pedido.dt-entrega-cli
        tt-pedido.nat-operacao 
        tt-pedido.nat-operacao-fat
        WITH SEPARATORS SIZE 153.5 BY 12.2
             BGCOLOR 15 FONT 1 TITLE "Pedidos Selecionados".


/************************************************************************************/
    
    {utp/utapi009.i}
        /*------------ Envio e-mail -------------*/


        define temp-table tt-envio-w
            field versao-integracao   as integer format ">>9"
            field servidor            as char
            field porta               as integer init 0
            field exchange            as logical init no
            field destino             as char
            field copia               as char
            field remetente           as char
            field assunto             as char
            field mensagem            as char
            field arq-anexo           as char
            field importancia         as integer init 0
            field log-enviada         as logical init no
            field log-lida            as logical init no
            field acomp               as logical init yes.


        /*---------------------------------------*/

DEFINE BUFFER bf-ped-venda FOR ped-venda.
DEFINE BUFFER bf-ped-item  FOR ped-item.


/* Local Variable Definitions ---                                       */ 

def var l-ped-aberto         as logical INITIAL YES no-undo. 
def var l-ped-at-total       as logical INITIAL YES no-undo. 
def var l-ped-outros         as logical INITIAL YES no-undo.
def var l-ped-unigel-com     as logical INITIAL no  no-undo.

def var l-vai                as logical INITIAL YES no-undo.

 
def var l-interno            as logical INITIAL YES no-undo. 
def var l-externo            as logical INITIAL YES no-undo. 

def var l-ped-nao-integrado  as logical INITIAL no no-undo. 
def var l-ped-sem-compl      as logical INITIAL no no-undo. 
def var l-ped-nao-config     as logical INITIAL no no-undo. 
def var l-ped-nao-email      as logical INITIAL no no-undo. 

def var l-ped-lista-prime    as logical INITIAL YES no-undo. 
def var l-ped-est-estrat     as logical INITIAL YES no-undo. 
def var l-ped-producao       as logical INITIAL YES no-undo. 

def var l-ped-liber-fat      as logical INITIAL YES no-undo. 
def var l-ped-nao-liber-fat  as logical INITIAL YES no-undo. 

DEFINE VARIABLE c-e-mail-cliente     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-tem-email          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-email-responsavel  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-nome-responsavel   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE soma-email           AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-pos                AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-seq                AS INTEGER    NO-UNDO.
DEFINE VARIABLE l-tem                AS logical    NO-UNDO.


DEFINE VARIABLE cod-emitente-jr      AS INTEGER    NO-UNDO.

DEFINE VARIABLE assunto-jr           AS CHARACTER  NO-UNDO.

/* Vari†veis para rotina de encontrar o % de icms */

DEFINE VARIABLE estab-faturx          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cod-emitente-faturx   AS INTEGER    NO-UNDO.
DEFINE VARIABLE nat-operacao-faturx   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE perc-icms-faturx      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE perc-desc-icms-faturx AS DECIMAL    NO-UNDO.
DEFINE VARIABLE i-exc                 AS INTEGER   NO-UNDO.


DEFINE VARIABLE rs-atalhos1 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
         /* "pdp/ESPD020", 1,
          "pdp/ESPD014", 2,
          "ftp/FA001PO", 3,
          "pdp/ESPD003", 4,
          "pdp/ESPD018", 5,
          "pdp/ESPD024", 6,
          "pdp/ESPD026", 7,
          "pdp/ESPD035", 8,
          "pdp/ESPD060", 9,
          "pdp/ESPD062", 10*/
"cd0402",	1,
"cd0704",	2,
"cdr045a",	3,
"pdp/espd0005",	4,
"pdp/espd0014",	5,
"pdp/espd0018",	6,
"pdp/espd0020",	7,
"pdp/espd0022",	8,
"pdp/espd0024",	9,
"pdp/espd0025",	10

          
     SIZE 14 BY 7 NO-UNDO.
                                        

DEFINE VARIABLE rs-atalhos2 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
"pdp/espd0026",	1,
"pdp/espd0035",	2,
"pdp/espd0036",	3,
"pdp/espd0060",	4,
"pdp/espd0062",	5,
"ftp/esft0054",	6,
     
/*          "CD0704", 1,
          "CD0402", 2,
          "CDR045A", 3,
          "pdp/ESPD036", 4,
          "RESERVA", 5,
          "RESERVA", 6,*/
          "RESERVA", 7,
          "RESERVA", 8,
          "RESERVA", 9,
          "RESERVA", 10
     SIZE 14 BY 7 NO-UNDO.
 
DEFINE NEW GLOBAL SHARED VAR wh-vl-preori                  AS WIDGET-HANDLE.
DEFINE VARIABLE h-proc AS HANDLE      NO-UNDO.                                       
def var c-arq-digita         as char    INITIAL YES no-undo. 
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

DEF TEMP-TABLE tt-ped-venda LIKE ped-venda
    FIELD r-rowid   AS ROWID.

/* BO para totalizar o pedido */

def var bo-ped-venda-cal as handle no-undo.
def var bo-ped-venda-com as handle no-undo.


/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 

def new global shared var rw-nr-pedido-0061    as row       no-undo.
def new global shared var rw-nr-pedido-0063    as row       no-undo.
def new global shared var l-espd0065           AS logical   no-undo.
def new global shared var rw-nr-pedido-0064    as row       no-undo.
def new global shared var i-nr-pedido-indiv   as int       no-undo.
def new global shared var cod-estabel-indiv   as char      no-undo.

def new global shared var c-cod-estabel-ini-20   AS CHARACTER NO-UNDO.
def new global shared var c-cod-estabel-fim-20   AS CHARACTER NO-UNDO.

def new global shared var c-tp-pedido-ini-20     AS CHARACTER NO-UNDO.
def new global shared var c-tp-pedido-fim-20     AS CHARACTER NO-UNDO.

def new global shared var dt-entrega-ini-20      AS DATE      NO-UNDO.
def new global shared var dt-entrega-fim-20      AS DATE      NO-UNDO.

def new global shared var i-nr-pedido-ini-20     AS INT       NO-UNDO.
def new global shared var i-nr-pedido-fim-20     AS INT       NO-UNDO.

def new global shared var c-nome-abrev-ini-20    AS CHARACTER NO-UNDO.
def new global shared var c-nome-abrev-fim-20    AS CHARACTER NO-UNDO.

def new global shared var i-cod-emitente-ini-20  AS INT       NO-UNDO.
def new global shared var i-cod-emitente-fim-20  AS INT       NO-UNDO.

def new global shared var c-it-codigo-ini-20     AS CHARACTER NO-UNDO.
def new global shared var c-it-codigo-fim-20     AS CHARACTER NO-UNDO.



/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 


DEFINE VARIABLE c-cod-estabel-ini   AS CHARACTER FORMAT "X(3)"          initial "422"          NO-UNDO. /*solic-318*/ 
DEFINE VARIABLE c-cod-estabel-fim   AS CHARACTER FORMAT "X(3)"          initial "422"          NO-UNDO. /*solic-318*/ 

DEFINE VARIABLE c-tp-pedido-ini     AS CHARACTER FORMAT "X(1)"          INITIAL ""             NO-UNDO.
DEFINE VARIABLE c-tp-pedido-fim     AS CHARACTER FORMAT "X(1)"          INITIAL "Z"            NO-UNDO.

DEFINE VARIABLE dt-entrega-ini      AS DATE      FORMAT "99/99/9999"    INITIAL TODAY          NO-UNDO.
DEFINE VARIABLE dt-entrega-fim      AS DATE      FORMAT "99/99/9999"    INITIAL TODAY          NO-UNDO.

DEFINE VARIABLE i-nr-pedido-ini     AS INT       FORMAT ">>>>>>>>9"     INITIAL 0              NO-UNDO.
DEFINE VARIABLE i-nr-pedido-fim     AS INT       FORMAT ">>>>>>>>9"     INITIAL 999999999      NO-UNDO.

DEFINE VARIABLE c-nome-abrev-ini    AS CHARACTER FORMAT "X(12)"         INITIAL ""             NO-UNDO.
DEFINE VARIABLE c-nome-abrev-fim    AS CHARACTER FORMAT "X(12)"         INITIAL "ZZZZZZZZZZZZ" NO-UNDO.

DEFINE VARIABLE i-cod-emitente-ini  AS INT       FORMAT ">>>>>>>>9"     INITIAL 0              NO-UNDO.
DEFINE VARIABLE i-cod-emitente-fim  AS INT       FORMAT ">>>>>>>>9"     INITIAL 999999999      NO-UNDO.

DEFINE VARIABLE c-it-codigo-ini     AS CHARACTER FORMAT "X(16)"         INITIAL ""             NO-UNDO.
DEFINE VARIABLE c-it-codigo-fim     AS CHARACTER FORMAT "X(16)"         INITIAL "ZZZZZZZZZZZZ" NO-UNDO.

DEFINE VARIABLE c-nome-ab-rep-ini   AS CHARACTER FORMAT "X(12)"         INITIAL ""             NO-UNDO.
DEFINE VARIABLE c-nome-ab-rep-fim   AS CHARACTER FORMAT "X(12)"         INITIAL "ZZZZZZZZZZZZ" NO-UNDO.



/****************** Definiáao de Vari†veis de Trabalho *********************/ 

DEFINE VARIABLE erro-jr            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE mens-jr            AS CHARACTER  NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */ 

DEFINE VARIABLE diin-jr            AS INTEGER    NO-UNDO.
DEFINE VARIABLE diex-jr            AS INTEGER    NO-UNDO.
DEFINE VARIABLE larg-jr            AS INTEGER    NO-UNDO.
DEFINE VARIABLE situacao-jr        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE nr-pedcli-jr       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-mensagem-jr      AS CHARACTER  FORMAT "X(60)"  NO-UNDO.
DEFINE VARIABLE qt-produzida-jr    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE qt-configurada-jr    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE qt-bobinas-jr AS INTEGER     NO-UNDO.

DEFINE VARIABLE c-obs-email-jr AS CHARACTER 
VIEW-AS EDITOR  
SIZE 43 BY 5.00 NO-UNDO.


    
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
EDGE-PIXELS 2 GRAPHIC-EDGE 
SIZE 15 BY 1 
BGCOLOR 12.


DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY 2.8.


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
    
DEFINE IMAGE IMAGE-11
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-12
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-13
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-14
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-15
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-16
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
    SIZE 20 BY 1.19.

DEFINE IMAGE im-pg-dig
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.19.

DEFINE VARIABLE c-arquivo AS CHARACTER NO-UNDO.

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

DEFINE RECTANGLE RECT-7 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 42.29 BY 2.

DEFINE RECTANGLE RECT-8 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 42.29 BY 2.

DEFINE RECTANGLE RECT-18 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 42.29 BY 2.

DEFINE RECTANGLE RECT-28 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 42.29 BY 2.

DEFINE RECTANGLE RECT-2
EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL
SIZE 110 BY 8.7 
BGCOLOR 7.      

DEFINE RECTANGLE RECT-9
EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL
SIZE 157 BY 2.42 
BGCOLOR 7.      

DEFINE RECTANGLE RECT-10 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 157 BY 13.22
BGCOLOR 7.

DEFINE RECTANGLE RECT-11 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 66 BY 8.7
BGCOLOR 7.

DEFINE RECTANGLE RECT-22
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 15 BY 2.3
BGCOLOR 7.


DEFINE VARIABLE l-param-1 AS LOGICAL INITIAL no 
LABEL "ParÉmetro 1"
VIEW-AS TOGGLE-BOX 
SIZE 44 BY 1.08 NO-UNDO. 


DEFINE BUTTON bt-novo
    IMAGE FILENAME "image\im-add"
    SIZE 4 BY 1.


DEFINE BUTTON bt-cancela
    IMAGE FILENAME "image\im-can"
    SIZE 4 BY 1.

DEFINE BUTTON bt-sai
    IMAGE FILENAME "image\im-exi"
    SIZE 4 BY 1.


DEFINE BUTTON bt-ajuda 
    IMAGE FILENAME "image\im-hel"
    SIZE 4 BY 1.


DEFINE BUTTON bt-cancela2 AUTO-END-KEY 
    IMAGE FILENAME "image\im-can"
    SIZE 6 BY 1.5.


DEFINE BUTTON bt-marca AUTO-GO 
     LABEL "&Marca" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-desmarca AUTO-GO 
     LABEL "&Desmarca" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-marca-todos AUTO-GO 
     LABEL "&Marca Todos" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-desmarca-todos AUTO-GO 
     LABEL "&Desm.Todos" 
     SIZE 10 BY 1
     BGCOLOR 8 .


DEFINE BUTTON bt-filtro 
    IMAGE FILENAME "image\im-sav"
    SIZE 5 BY 1.0.

DEFINE BUTTON bt-atalho1 
    IMAGE FILENAME "image\im-sav"
    SIZE 5 BY 1.0.

DEFINE BUTTON bt-atalho2 
    IMAGE FILENAME "image\im-sav"
    SIZE 5 BY 1.0.

DEFINE BUTTON bt-pd4000 AUTO-GO 
     LABEL "&PD4000" 
     SIZE 10 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON bt-pd1001 AUTO-GO 
     LABEL "&PD1001" 
     SIZE 10 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON bt-consulta-config AUTO-GO 
     LABEL "&Consul.Config" 
     SIZE 10 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON bt-dados-adic AUTO-GO 
     LABEL "&Dados Adic." 
     SIZE 10 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON bt-hist-ped AUTO-GO 
     LABEL "&Hist¢rico" 
     SIZE 10 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON bt-libera-fatur AUTO-GO 
     LABEL "&Libera Fatur." 
     SIZE 10 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON bt-email-ped AUTO-GO 
     LABEL "&Email Cliente." 
     SIZE 10 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON bt-muda-estab AUTO-GO 
     LABEL "&Muda Estab." 
     SIZE 10 BY 1.5
     BGCOLOR 8 .



DEFINE RECTANGLE RECT-1
EDGE-PIXELS 2 GRAPHIC-EDGE 
SIZE 157 BY 1.42 
BGCOLOR 7.

DEFINE RECTANGLE RECT-6
EDGE-PIXELS 0     
SIZE 101.72 BY .12
BGCOLOR 7 .

DEFINE RECTANGLE rt-folder
EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL
SIZE 129 BY 14.5
FGCOLOR 0.

DEFINE RECTANGLE rt-folder-left
EDGE-PIXELS 0
SIZE .43 BY 14
BGCOLOR 15.

DEFINE RECTANGLE rt-folder-right
EDGE-PIXELS 0
SIZE .43 BY 14
BGCOLOR 7.

DEFINE RECTANGLE rt-folder-top
EDGE-PIXELS 0
SIZE 124 BY .12
BGCOLOR 15 .

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
    bt-novo     AT ROW 1.25 COL 4    HELP "Embarca"
    bt-cancela  AT ROW 1.25 COL 8.5  HELP "Cancela"
    bt-sai      AT ROW 1.25 COL 147  HELP "Encerra Programa"
    bt-ajuda    AT ROW 1.25 COL 152  HELP "Ajuda"

/* Colocar aqui os campos chaves do registro */

    c-cod-estabel-ini LABEL "Estab."
     at row 2.7 col 12 colon-aligned
     view-as fill-in 
     size 12 by .88
     font 1
   
    c-cod-estabel-fim NO-LABEL 
     at row 2.7 col 34 colon-aligned
     view-as fill-in 
     size 12 by .88
     font 1
   
    dt-entrega-ini LABEL "Dt.Entrega"
     at row 3.7 col 12 colon-aligned
     view-as fill-in 
     size 12 by .88
     font 1
    
    dt-entrega-fim NO-LABEL 
     at row 3.7 col 34 colon-aligned
     view-as fill-in 
     size 12 by .88
     font 1 

    c-tp-pedido-ini LABEL "Tp.Pedido"
     at row 4.7 col 12 colon-aligned
     view-as fill-in 
     size 12 by .88
     font 1
   
    c-tp-pedido-fim NO-LABEL 
     at row 4.7 col 34 colon-aligned
     view-as fill-in 
     size 12 by .88
     font 1
   
    i-nr-pedido-ini LABEL "Nr.Pedido"
     at row 5.7 col 12 colon-aligned
     view-as fill-in 
     size 12 by .88
     font 1
    
    i-nr-pedido-fim NO-LABEL 
     at row 5.7 col 34 colon-aligned
     view-as fill-in 
     size 12 by .88
     font 1
    
    c-nome-abrev-ini LABEL "Cliente"
      at row 6.7 col 12 colon-aligned
      view-as fill-in 
      size 12 by .88
      font 1

    c-nome-abrev-fim NO-LABEL 
      at row 6.7 col 34 colon-aligned
      view-as fill-in 
      size 12 by .88
      font 1

    i-cod-emitente-ini LABEL "Cd.Cliente"
      at row 7.7 col 12 colon-aligned
      view-as fill-in 
      size 12 by .88
      font 1
     
    i-cod-emitente-fim  NO-LABEL 
      at row 7.7 col 34 colon-aligned
      view-as fill-in 
      size 12 by .88
      font 1

    c-it-codigo-ini LABEL "Item"
     at row 8.7 col 12 colon-aligned
     view-as fill-in 
     size 12 by .88
     font 1
    
    c-it-codigo-fim NO-LABEL 
     at row 8.7 col 34 colon-aligned
     view-as fill-in 
     size 12 by .88
     font 1
    
    c-nome-ab-rep-ini LABEL "Representante"
      at row 9.7 col 12 colon-aligned
      view-as fill-in 
      size 12 by .88
      font 1

    c-nome-ab-rep-fim NO-LABEL 
      at row 9.7 col 34 colon-aligned
      view-as fill-in 
      size 12 by .88
      font 1
      
    "Critica: " at row 2.9 col 50  


    l-ped-nao-email lABEL "N∆o Confir.(Email)" AT ROW 2.9 COL 55 COLON-ALIGNED
    VIEW-AS TOGGLE-BOX 
    SIZE 18 BY .88
    
    l-ped-sem-compl lABEL "Sem Complementos" AT ROW 2.9 COL 70 COLON-ALIGNED
    VIEW-AS TOGGLE-BOX 
    SIZE 18 BY .88
         
    l-ped-nao-integrado lABEL "N∆o Integrados" AT ROW 3.6 COL 55 COLON-ALIGNED
    VIEW-AS TOGGLE-BOX 
    SIZE 18 BY .88
         
    l-ped-nao-config lABEL "N∆o Configurados" AT ROW 3.6 COL 70 COLON-ALIGNED
    VIEW-AS TOGGLE-BOX 
    SIZE 18 BY .88    
      
    "Filtro: " at row 4.9 col 50      
       
    l-interno lABEL "Mercado Interno" AT ROW 4.9 COL 55 COLON-ALIGNED
    VIEW-AS TOGGLE-BOX 
    SIZE 18 BY .88  

    l-externo lABEL "Mercado Externo" AT ROW 4.9 COL 70 COLON-ALIGNED
    VIEW-AS TOGGLE-BOX 
    SIZE 18 BY .88  

    l-ped-liber-fat lABEL "Liber.Faturamento" AT ROW 5.6 COL 55 COLON-ALIGNED
    VIEW-AS TOGGLE-BOX 
    SIZE 18 BY .88
         
    l-ped-nao-liber-fat lABEL "Nao Liber.Fatur" AT ROW 5.6 COL 70 COLON-ALIGNED
    VIEW-AS TOGGLE-BOX 
    SIZE 18 BY .88
       
         
    "Filtro: " at row 6.9 col 50      
       
    l-ped-lista-prime lABEL "Ped.Lista Prime" AT ROW 6.9 COL 55 COLON-ALIGNED
    VIEW-AS TOGGLE-BOX 
    SIZE 18 BY .88
         
    l-ped-est-estrat lABEL "Ped.Estoq.Estrat" AT ROW 6.9 COL 70 COLON-ALIGNED
    VIEW-AS TOGGLE-BOX 
    SIZE 18 BY .88
         
    l-ped-producao lABEL "Ped.Produá∆o" AT ROW 7.6 COL 55 COLON-ALIGNED
    VIEW-AS TOGGLE-BOX 
    SIZE 18 BY .88

    l-ped-unigel-com lABEL "Sem Unigel Comercial" AT ROW 7.6 COL 70 COLON-ALIGNED
    VIEW-AS TOGGLE-BOX 
    SIZE 18 BY .88     

         
    "Filtro: " at row 9.0 col 50      
       
    l-ped-aberto lABEL "Ped.em Aberto" AT ROW 9.0 COL 55 COLON-ALIGNED
    VIEW-AS TOGGLE-BOX 
    SIZE 18 BY .88
         
    l-ped-at-total lABEL "Ped.Atend.Total" AT ROW 9.0 COL 70 COLON-ALIGNED
    VIEW-AS TOGGLE-BOX 
    SIZE 18 BY .88

    l-ped-outros lABEL "Outros Pedidos" AT ROW 9.7 COL 55 COLON-ALIGNED
    VIEW-AS TOGGLE-BOX 
    SIZE 18 BY .88     


    bt-filtro   AT ROW 9.6  COL 86

    br-pedido   AT ROW 12   COL 3.2 
    
    rs-atalhos1 lABEL "Atalhos 1" AT ROW 2.7 COL 101 COLON-ALIGNED

    bt-atalho1          AT ROW 10      COL 112 
                        
    rs-atalhos2 lABEL "Atalhos 2" AT ROW 2.7 COL 121 COLON-ALIGNED

    bt-atalho2          AT ROW 10      COL 132 
                        

    bt-marca            AT ROW 25   COL 2
    bt-desmarca         AT ROW 25   COL 12.5
    bt-marca-todos      AT ROW 25   COL 23
    bt-desmarca-todos   AT ROW 25   COL 33.5


    bt-pd4000           AT ROW 25     COL 46
    bt-pd1001           AT ROW 25     COL 57
    bt-consulta-config  AT ROW 25     COL 68
    bt-dados-adic       AT ROW 25     COL 79
    bt-hist-ped         AT ROW 25     COL 90
    bt-libera-fatur     AT ROW 25     COL 101
    bt-email-ped        AT ROW 25     COL 112
    bt-muda-estab       AT ROW 25     COL 123

    bt-cancela2       AT ROW 25     COL 134 

    RECT-1  AT ROW 1.05 COL 2    
    RECT-2  AT ROW 2.55 COL 2    
    RECT-10 AT ROW 11.5 COL 2  
    RECT-11 AT ROW 2.55 COL 93
    
    rect-7  at row 2.7  col 49 
    rect-8  at row 4.7  col 49 
    rect-18 at row 6.7  col 49 
    rect-28 at row 8.7  col 49 




    IMAGE-1 AT ROW 2.8    col 27
    IMAGE-2 AT ROW 2.8    col 31
    IMAGE-3 AT ROW 3.8    col 27
    IMAGE-4 AT ROW 3.8    col 31

    IMAGE-5  AT ROW 4.8    col 27
    IMAGE-6  AT ROW 4.8    col 31
    IMAGE-7  AT ROW 5.8    col 27
    IMAGE-8  AT ROW 5.8    col 31
    IMAGE-9  AT ROW 6.8    col 27
    IMAGE-10 AT ROW 6.8    col 31
    IMAGE-11 AT ROW 7.8    col 27
    IMAGE-12 AT ROW 7.8    col 31
    IMAGE-13 AT ROW 8.8    col 27
    IMAGE-14 AT ROW 8.8    col 31
    IMAGE-15 AT ROW 9.8    col 27
    IMAGE-16 AT ROW 9.8    col 31

    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
    SIDE-LABELS NO-UNDERLINE THREE-D
    AT COL 1 ROW 1 FONT 1 
    SIZE 159.43 BY 25.88. 


/* ******** Acerto da posiá∆o dos labels e tamanho dos radio-set ******* */

DEFINE VARIABLE h-label AS WIDGET-HANDLE NO-UNDO.

/* *************************  Create Window  ************************** */

CREATE WINDOW C-Win ASSIGN
   HIDDEN             = YES
   TITLE              = "Gerenciador de Pedidos de Vendas"
   HEIGHT             = 25.88
   WIDTH              = 159.43
   MAX-HEIGHT         = 27.29
   MAX-WIDTH          = 172.57
   VIRTUAL-HEIGHT     = 27.29
   VIRTUAL-WIDTH      = 172.57
   RESIZE             = yes
   SCROLL-BARS        = no
   STATUS-AREA        = yes
   BGCOLOR            = ?
   FGCOLOR            = ?
   KEEP-FRAME-Z-ORDER = yes
   THREE-D            = yes
   MESSAGE-AREA       = no
   SENSITIVE          = yes
.

/* ***************  Runtime Attributes and UIB Settings  ************** */


IF SESSION:DISPLAY-TYPE = "GUI" AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* ************************* Included-Libraries *********************** */
def var h-facelift as handle no-undo.
if not valid-handle(h-facelift) then run btb/btb901zo.p persistent set h-facelift.

/* tirar este comentario 
run pi_aplica_facelift_thin in h-facelift (input frame f-relat:HANDLE).
DELETE PROCEDURE h-facelift. */

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
    run disable_UI.
end.

/* ************************  Control Triggers  ************************ */

ON ROW-DISPLAY OF br-pedido 
DO:
 RUN pi-changeRowColor(INPUT SELF).
END.

ON CHOOSE OF bt-email-ped IN FRAME f-relat /* Confirma Pedido - Envio E-mail */
DO:   

     

    RUN pi-envia-email-pedido.

    CLOSE QUERY br-pedido.

    open query br-pedido for each tt-pedido.
    apply 'entry' to tt-pedido.marca in browse br-pedido. 



END.


ON CHOOSE OF bt-marca-todos IN FRAME f-relat /* Marca Todos Pedidos */
DO:

    FOR EACH tt-pedido.
        ASSIGN tt-pedido.marca = "*".
    END.
    open query br-pedido for each tt-pedido.
    apply 'entry' to tt-pedido.marca in browse br-pedido. 


END.


ON CHOOSE OF bt-desmarca-todos IN FRAME f-relat /* Desmarca Todos Pedidos */
DO:

    FOR EACH tt-pedido.
        ASSIGN tt-pedido.marca = " ".
    END.
    open query br-pedido for each tt-pedido.
    apply 'entry' to tt-pedido.marca in browse br-pedido. 


END.



ON CHOOSE OF bt-marca IN FRAME f-relat /* Marca Pedido */
DO:

    assign tt-pedido.marca:screen-value in browse br-pedido = "*".
    get current br-pedido.
    assign input browse br-pedido tt-pedido.marca.       
    apply 'entry' to tt-pedido.marca in browse br-pedido. 


END.


ON CHOOSE OF bt-desmarca IN FRAME f-relat /* Desmarca Pedido */
DO:


    assign tt-pedido.marca:screen-value in browse br-pedido = " ".
    get current br-pedido.
    assign input browse br-pedido tt-pedido.marca.       
    apply 'entry' to tt-pedido.marca in browse br-pedido. 


END.


ON CHOOSE OF bt-pd4000 IN FRAME f-relat /* PD4000 */
DO:


IF VALID-HANDLE(wh-vl-preori) AND 
        wh-vl-preori:SENSITIVE = yes THEN do:

        wh-vl-preori:frame:window:move-to-top().

        RETURN.
    end.


    if num-results("br-pedido") > 0 THEN DO:
       get current br-pedido.

        FIND FIRST ped-venda WHERE
            ped-venda.nr-pedido = tt-pedido.nr-pedido
            NO-LOCK NO-ERROR.
        
        IF AVAIL ped-venda THEN DO:
        
            FIND FIRST ped-item OF ped-venda WHERE
                ped-item.it-codigo    = tt-pedido.it-codigo AND
                ped-item.nr-sequencia = tt-pedido.nr-sequencia
                NO-LOCK NO-ERROR.
            
            IF AVAIL ped-item THEN DO:
            
               ASSIGN gr-ped-item  = rowid(ped-item)
                      gr-ped-venda = rowid(ped-venda).
          
            END.
        
        END.

    END.




h-proc = SESSION:FIRST-PROCEDURE.
repeat:


h-proc = h-proc:NEXT-SIBLING.
IF NOT VALID-HANDLE(h-proc) THEN  LEAVE.
IF index(h-proc:FILE-NAME,"pd4000") > 0 THEN  LEAVE.
 
END.

 
IF VALID-HANDLE(h-proc)  THEN DO:
   
    RUN RepositionRecord IN h-proc (INPUT ROWID(ped-venda)).
    IF VALID-HANDLE(wh-vl-preori) then 
    wh-vl-preori:frame:window:move-to-top().
END.
else     do:
run pdp/pd4000.w persistent SET h-proc.
If valid-handle(h-proc) then
   run dispatch in h-proc ('initialize').
    IF VALID-HANDLE(wh-vl-preori) then 
    wh-vl-preori:frame:window:move-to-top().   
 
end.

  /*  APPLY "choose" TO bt-filtro IN FRAME f-relat.*/


END.


ON MOUSE-SELECT-DBLCLICK OF br-pedido IN FRAME f-relat
DO:
    apply 'choose' to bt-pd4000 IN FRAME f-relat.
    
END.


ON CHOOSE OF bt-pd1001 IN FRAME f-relat /* PD1001 */
DO:

    if num-results("br-pedido") > 0 THEN DO:
       get current br-pedido.

        FIND FIRST ped-venda WHERE
            ped-venda.nr-pedido = tt-pedido.nr-pedido
            NO-LOCK NO-ERROR.
        
        IF AVAIL ped-venda THEN DO:
        
            FIND FIRST ped-item OF ped-venda WHERE
                ped-item.it-codigo    = tt-pedido.it-codigo AND
                ped-item.nr-sequencia = tt-pedido.nr-sequencia
                NO-LOCK NO-ERROR.
            
            IF AVAIL ped-item THEN DO:
            
               ASSIGN gr-ped-item  = rowid(ped-item)
                      gr-ped-venda = rowid(ped-venda).
        
            END.
        
        END.

    END.
    
    run pdp/pd1001.w persistent SET h-proc.
If valid-handle(h-proc) then
   run dispatch in h-proc ('initialize').
    /*
     {&window-name}:sensitive = no.
    run pdp/pd1001.w.
     {&window-name}:sensitive = yes.
     */

END.

ON CHOOSE OF bt-consulta-config IN FRAME f-relat /* Consulta Configuraá∆o */

DO:

    FIND FIRST ped-venda WHERE
        ped-venda.nr-pedido = tt-pedido.nr-pedido
        NO-LOCK NO-ERROR.

    IF AVAIL ped-venda THEN DO:

        FIND FIRST ped-item OF ped-venda WHERE
            ped-item.it-codigo    = tt-pedido.it-codigo AND
            ped-item.nr-sequencia = tt-pedido.nr-sequencia
            NO-LOCK NO-ERROR.
        
        IF AVAIL ped-item THEN DO:
        
           ASSIGN rw-nr-pedido-0061 = rowid(ped-item).
      {&window-name}:sensitive = no.       
           run pdp/espd0061.w.
      {&window-name}:sensitive = yes.
        
        END.

    END.

END.

ON CHOOSE OF bt-dados-adic IN FRAME f-relat /* Dados Adicionais */
DO:

    FIND FIRST ped-venda WHERE
        ped-venda.nr-pedido = tt-pedido.nr-pedido
        NO-LOCK NO-ERROR.

    IF AVAIL ped-venda THEN DO:

           
               

        FIND FIRST ped-item OF ped-venda WHERE
            ped-item.it-codigo    = tt-pedido.it-codigo AND
            ped-item.nr-sequencia = tt-pedido.nr-sequencia
            NO-LOCK NO-ERROR.
        
        FIND FIRST estabelec WHERE
                 estabelec.cod-estabel = ped-venda.cod-estabel
                 NO-LOCK NO-ERROR.

        IF AVAIL ped-item AND  AVAIL estabelec THEN DO:

          

           FIND FIRST pd-compl-pedido WHERE
                   pd-compl-pedido.ep-codigo     = estabelec.ep-codigo   AND 
                   pd-compl-pedido.nr-pedido     = ped-venda.nr-pedido   AND
                   pd-compl-pedido.nr-sequencia  = ped-item.nr-sequencia 
                   NO-LOCK NO-ERROR.

             IF NOT AVAIL pd-compl-pedido THEN DO:
                 run utp/ut-msgs.p (input "show":U, input 17006, "Este pedido n∆o tem registro de complemento~~Favor utilizar a rotina de calculadora na tela do PD4000 para criar de forma correta.").
                 RETURN NO-APPLY.     
             END.

        
           ASSIGN rw-nr-pedido-0063 = rowid(ped-item)
                  l-espd0065        = YES.
     {&window-name}:sensitive = no.        
           run pdp/espd0063.w.
     {&window-name}:sensitive = yes.

           ASSIGN l-espd0065 = NO.
           
           run pdp\upc\trw-pd-compl-pedido-manual.p (input rowid(pd-compl-pedido)).
        
        END.

    END.

END.

ON CHOOSE OF bt-muda-estab IN FRAME f-relat /* Dados Adicionais */
DO:

    return no-apply.
                 /*
            FIND FIRST ped-venda WHERE
                ped-venda.nr-pedido = tt-pedido.nr-pedido
                 NO-ERROR.
        
            IF AVAIL ped-venda THEN DO:
            
            
                FIND FIRST if-ped-venda WHERE
                    if-ped-venda.nr-pedido = ped-venda.nr-pedido and
                    if-ped-venda.nr-pedido-relac <> 0
                    NO-LOCK NO-ERROR.
        
                IF AVAIL if-ped-venda THEN return no-apply.
        
        
                 FIND FIRST if-ped-venda WHERE
                    if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido
                    NO-LOCK NO-ERROR.
        
                 IF AVAIL if-ped-venda THEN return no-apply.
        
        
            
        
                FIND FIRST ped-item OF ped-venda WHERE
                    ped-item.it-codigo    = tt-pedido.it-codigo AND
                    ped-item.nr-sequencia = tt-pedido.nr-sequencia and
                    ped-item.cod-sit-item < 3
                     NO-ERROR.
                
                IF AVAIL ped-item THEN DO:
                
                    FIND FIRST estabelec WHERE
                           estabelec.cod-estabel = ped-venda.cod-estabel
                           NO-LOCK NO-ERROR.
                
                       IF AVAIL estabelec THEN DO:               
                       
                       
                          FIND FIRST pd-compl-pedido WHERE
                               pd-compl-pedido.ep-codigo            = estabelec.ep-codigo   AND
                               pd-compl-pedido.nr-pedido            = ped-venda.nr-pedido   AND
                               pd-compl-pedido.nr-sequencia         = ped-item.nr-sequencia AND
                               pd-compl-pedido.cod-estabel-fat      <> "" AND
                               pd-compl-pedido.nat-operacao-fat     <> "" AND
                               pd-compl-pedido.preco-venda-calc-fat <> 0  AND 
                               pd-compl-pedido.cod-estabel-fat      <> ped-venda.cod-estabel and
                               pd-compl-pedido.cod-estabel-prod     = ped-venda.cod-estabel
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
                                 run dibo/bodi159cal.p persistent set bo-ped-venda-cal.
        
                                 run calculateOrder in bo-ped-venda-cal(input tt-ped-venda.r-rowid).
                               
                                  
                                   if valid-handle(bo-ped-venda-cal) then 
                                  delete procedure bo-ped-venda-cal.
                                  
                                   IF VALID-HANDLE(bo-ped-venda-com) THEN
                                    delete procedure bo-ped-venda-com.
                
                          END.   /* AVAIL pd-compl-pedido */
                
                       END.   /* AVAIL estabelec */
                
                
                
                END.
        
            END.
               */
END.


ON CHOOSE OF bt-hist-ped IN FRAME f-relat /* Hist¢rico de Pedido */
DO:

    FIND FIRST ped-venda WHERE
        ped-venda.nr-pedido = tt-pedido.nr-pedido
        NO-LOCK NO-ERROR.

    IF AVAIL ped-venda THEN DO:

        FIND FIRST ped-item OF ped-venda WHERE
            ped-item.it-codigo    = tt-pedido.it-codigo AND
            ped-item.nr-sequencia = tt-pedido.nr-sequencia
            NO-LOCK NO-ERROR.
        
        IF AVAIL ped-item THEN DO:
        
           ASSIGN rw-nr-pedido-0064 = rowid(ped-item).
                 {&window-name}:sensitive = no.
                       run pdp/espd0064.w.
                 {&window-name}:sensitive = yes.
            do trans:  
                find first if-ped-venda where if-ped-venda.nr-pedido = ped-venda.nr-pedido no-lock no-error.
        
                if avail if-ped-venda and if-ped-venda.nr-pedido-relac <> 0 then do:
                
                 find first b-estabelec where b-estabelec.cod-estabel = ped-venda.cod-estabel no-lock no-error.
                 find first b1-estabelec where b1-estabelec.cod-estabel =  if-ped-venda.cod-estab-atend no-lock no-error.
                
                          for each  bf-pd-hist-pedido where 
                                                   bf-pd-hist-pedido.ep-codigo    = b1-estabelec.ep-codigo and
                                                   bf-pd-hist-pedido.nr-pedido    = if-ped-venda.nr-pedido-relac and
                                                   bf-pd-hist-pedido.nr-sequencia = ped-item.nr-sequencia exclusive-lock.
                                     delete bf-pd-hist-pedido.
                          end.                
                        for each  pd-hist-pedido where 
                                           pd-hist-pedido.ep-codigo    = b-estabelec.ep-codigo and
                                           pd-hist-pedido.nr-pedido    = ped-venda.nr-pedido and
                                           pd-hist-pedido.nr-sequencia = ped-item.nr-sequencia   no-lock.
                                           
                                                                   
                                 find first bf-pd-hist-pedido where 
                                                   bf-pd-hist-pedido.ep-codigo    = b1-estabelec.ep-codigo and
                                                   bf-pd-hist-pedido.nr-pedido    = if-ped-venda.nr-pedido-relac and
                                                   bf-pd-hist-pedido.nr-sequencia = ped-item.nr-sequencia and
                                                   bf-pd-hist-pedido.dt-trans     = pd-hist-pedido.dt-trans and
                                                   bf-pd-hist-pedido.hora-trans   = pd-hist-pedido.hora-trans exclusive-lock  no-error.
                                                   
                                                   
                                 if not avail  bf-pd-hist-pedido then do:
                                                 create bf-pd-hist-pedido.
                                 buffer-copy pd-hist-pedido  except nr-pedido ep-codigo nr-sequencia dt-trans hora-trans  to bf-pd-hist-pedido
                                                assign bf-pd-hist-pedido.ep-codigo = b1-estabelec.ep-codigo
                                                       bf-pd-hist-pedido.nr-pedido = if-ped-venda.nr-pedido-relac
                                                       bf-pd-hist-pedido.nr-sequencia = ped-item.nr-sequencia
                                                       bf-pd-hist-pedido.dt-trans     = pd-hist-pedido.dt-trans 
                                                       bf-pd-hist-pedido.hora-trans   = pd-hist-pedido.hora-trans .
                                                       
                                 end.
                                 else                       
                                                       
                                 
                                 buffer-copy pd-hist-pedido  except nr-pedido ep-codigo nr-sequencia dt-trans hora-trans to bf-pd-hist-pedido.
                                       
                         end.                                                         
                                            
                                                                               
                                                                                                   
                
                end.
              else do:
                
                    find first if-ped-venda where if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido no-lock no-error.
                    
                    if avail if-ped-venda then do:
                
                        find first b-ped-venda where b-ped-venda.nr-pedido = if-ped-venda.nr-pedido no-lock no-error.
                        
                        if not avail b-ped-venda then return "ok".
                        
                        find first b-estabelec  where b-estabelec.cod-estabel  = ped-venda.cod-estabel no-lock no-error.
                        find first b1-estabelec where b1-estabelec.cod-estabel = b-ped-venda.cod-estabel no-lock no-error.
                     
                                  for each  bf-pd-hist-pedido where 
                                                  bf-pd-hist-pedido.ep-codigo    = b1-estabelec.ep-codigo and
                                                  bf-pd-hist-pedido.nr-pedido    = if-ped-venda.nr-pedido and
                                                  bf-pd-hist-pedido.nr-sequencia = ped-item.nr-sequencia exclusive-lock.
                                                  
                                                  delete bf-pd-hist-pedido.
                                  end.                 
                                                        
                        for each  pd-hist-pedido where 
                                           pd-hist-pedido.ep-codigo    = b-estabelec.ep-codigo and
                                           pd-hist-pedido.nr-pedido    = ped-venda.nr-pedido and
                                           pd-hist-pedido.nr-sequencia = ped-item.nr-sequencia  no-lock.
                                         
                                     
                        
                                find first bf-pd-hist-pedido where 
                                                  bf-pd-hist-pedido.ep-codigo    = b1-estabelec.ep-codigo and
                                                  bf-pd-hist-pedido.nr-pedido    = if-ped-venda.nr-pedido and
                                                  bf-pd-hist-pedido.nr-sequencia = ped-item.nr-sequencia and
                                                  bf-pd-hist-pedido.dt-trans     = pd-hist-pedido.dt-trans and
                                                  bf-pd-hist-pedido.hora-trans   = pd-hist-pedido.hora-trans exclusive-lock  no-error .
                                                  
                                                  
                                if not avail  bf-pd-hist-pedido then do:
                                                create bf-pd-hist-pedido.
                                               buffer-copy pd-hist-pedido  except nr-pedido ep-codigo nr-sequencia dt-trans hora-trans  to bf-pd-hist-pedido
                                               assign bf-pd-hist-pedido.ep-codigo = b1-estabelec.ep-codigo
                                                      bf-pd-hist-pedido.nr-pedido = if-ped-venda.nr-pedido
                                                      bf-pd-hist-pedido.nr-sequencia = ped-item.nr-sequencia 
                                                      bf-pd-hist-pedido.dt-trans     = pd-hist-pedido.dt-trans 
                                                      bf-pd-hist-pedido.hora-trans   = pd-hist-pedido.hora-trans.
                                                      
                                 end.
                                 else                       
                                                       
                                 
                                 buffer-copy pd-hist-pedido  except nr-pedido ep-codigo nr-sequencia dt-trans hora-trans  to bf-pd-hist-pedido.
                                      
                           
                                                                                              
                   
                        end. /*for each*/
                
                    end. /*if avail id-ped-venda pai*/
                END.  /*else do*/      
            end. /*do trans*/
        end. /*if avail ped-item*/
        
      END. /*if avail ped-venda*/

   

END.  /*on choose*/

ON CHOOSE OF bt-libera-fatur IN FRAME f-relat /* Libera Faturamento */
DO:

    RUN pi-libera-faturamento.

    CLOSE QUERY br-pedido.

    open query br-pedido for each tt-pedido.
    apply 'entry' to tt-pedido.marca in browse br-pedido. 

END.

ON CHOOSE OF bt-atalho1 IN FRAME f-relat /* Atalho de Programas */
DO:
     
    ASSIGN c-cod-estabel-ini-20   =  string(c-cod-estabel-ini:SCREEN-VALUE IN FRAME f-relat)
           c-cod-estabel-fim-20   =  string(c-cod-estabel-fim:SCREEN-VALUE IN FRAME f-relat)
           c-tp-pedido-ini-20     =  string(c-tp-pedido-ini:SCREEN-VALUE IN FRAME f-relat)
           c-tp-pedido-fim-20     =  string(c-tp-pedido-fim:SCREEN-VALUE IN FRAME f-relat)
           dt-entrega-ini-20      =  date(dt-entrega-ini:SCREEN-VALUE IN FRAME f-relat)
           dt-entrega-fim-20      =  date(dt-entrega-fim:SCREEN-VALUE IN FRAME f-relat)
           i-nr-pedido-ini-20     =  int(i-nr-pedido-ini:SCREEN-VALUE IN FRAME f-relat)
           i-nr-pedido-fim-20     =  int(i-nr-pedido-fim:SCREEN-VALUE IN FRAME f-relat)
           c-nome-abrev-ini-20    =  string(c-nome-abrev-ini:SCREEN-VALUE IN FRAME f-relat)
           c-nome-abrev-fim-20    =  string(c-nome-abrev-fim:SCREEN-VALUE IN FRAME f-relat)
           i-cod-emitente-ini-20  =  int(i-cod-emitente-ini:SCREEN-VALUE IN FRAME f-relat)
           i-cod-emitente-fim-20  =  int(i-cod-emitente-fim:SCREEN-VALUE IN FRAME f-relat)
           c-it-codigo-ini-20     =  string(c-it-codigo-ini:SCREEN-VALUE IN FRAME f-relat)
           c-it-codigo-fim-20     =  string(c-it-codigo-fim:SCREEN-VALUE IN FRAME f-relat).

/* session:set-wait-state("general":U).*/
     {&window-name}:sensitive = no.
  
     

    IF INT(rs-atalhos1:SCREEN-VALUE IN FRAME f-relat) = 7 THEN
        RUN pdp/espd0020.w.

    IF INT(rs-atalhos1:SCREEN-VALUE IN FRAME f-relat) = 5 THEN DO:

        if num-results("br-pedido") > 0 THEN DO:
           get current br-pedido.
           ASSIGN i-nr-pedido-indiv   = tt-pedido.nr-pedido
                  cod-estabel-indiv   = tt-pedido.cod-estabel.
        END.
        ELSE
            ASSIGN i-nr-pedido-indiv   = 0
                   cod-estabel-indiv   = "".

        RUN pdp/espd0014.w.

    END.

    

    IF INT(rs-atalhos1:SCREEN-VALUE IN FRAME f-relat) = 4 THEN DO:

        if num-results("br-pedido") > 0 THEN DO:
           get current br-pedido.
           ASSIGN i-nr-pedido-indiv   = tt-pedido.nr-pedido
                  cod-estabel-indiv   = tt-pedido.cod-estabel.
        END.
        ELSE
            ASSIGN i-nr-pedido-indiv   = 0
                   cod-estabel-indiv   = "".

        RUN pdp/espd0005.w.

    END.
    
    IF INT(rs-atalhos1:SCREEN-VALUE IN FRAME f-relat) = 2 THEN
        RUN cdp/CD0704.w.

    IF INT(rs-atalhos1:SCREEN-VALUE IN FRAME f-relat) = 1 THEN
        RUN cdp/CD0402.w.

    IF INT(rs-atalhos1:SCREEN-VALUE IN FRAME f-relat) = 3 THEN
        RUN cdp/CDR045A.w.


IF INT(rs-atalhos1:SCREEN-VALUE IN FRAME f-relat) = 8 THEN
        RUN pdp/espd0022.w.

IF INT(rs-atalhos1:SCREEN-VALUE IN FRAME f-relat) = 9 THEN
        RUN pdp/espd0024.w.

    IF INT(rs-atalhos1:SCREEN-VALUE IN FRAME f-relat) = 10 THEN
        RUN pdp/espd0025.w.
        

    IF INT(rs-atalhos1:SCREEN-VALUE IN FRAME f-relat) = 6 THEN
        RUN pdp/espd0018.w.


{&window-name}:sensitive = yes.



/*    session:set-wait-state("":U).*/
    
    ASSIGN c-cod-estabel-ini-20   =  "" 
           c-cod-estabel-fim-20   =  "" 
           c-tp-pedido-ini-20     =  "" 
           c-tp-pedido-fim-20     =  "" 
           dt-entrega-ini-20      =  ?  
           dt-entrega-fim-20      =  ?  
           i-nr-pedido-ini-20     =  0  
           i-nr-pedido-fim-20     =  0  
           c-nome-abrev-ini-20    =  "" 
           c-nome-abrev-fim-20    =  "" 
           i-cod-emitente-ini-20  =  0  
           i-cod-emitente-fim-20  =  0  
           c-it-codigo-ini-20     =  "" 
           c-it-codigo-fim-20     =  ""
           i-nr-pedido-indiv      =  0 
           cod-estabel-indiv      =  "".

END.


ON CHOOSE OF bt-atalho2 IN FRAME f-relat /* Atalho de Programas */
DO:
     
    ASSIGN c-cod-estabel-ini-20   =  string(c-cod-estabel-ini:SCREEN-VALUE IN FRAME f-relat)
           c-cod-estabel-fim-20   =  string(c-cod-estabel-fim:SCREEN-VALUE IN FRAME f-relat)
           c-tp-pedido-ini-20     =  string(c-tp-pedido-ini:SCREEN-VALUE IN FRAME f-relat)
           c-tp-pedido-fim-20     =  string(c-tp-pedido-fim:SCREEN-VALUE IN FRAME f-relat)
           dt-entrega-ini-20      =  date(dt-entrega-ini:SCREEN-VALUE IN FRAME f-relat)
           dt-entrega-fim-20      =  date(dt-entrega-fim:SCREEN-VALUE IN FRAME f-relat)
           i-nr-pedido-ini-20     =  int(i-nr-pedido-ini:SCREEN-VALUE IN FRAME f-relat)
           i-nr-pedido-fim-20     =  int(i-nr-pedido-fim:SCREEN-VALUE IN FRAME f-relat)
           c-nome-abrev-ini-20    =  string(c-nome-abrev-ini:SCREEN-VALUE IN FRAME f-relat)
           c-nome-abrev-fim-20    =  string(c-nome-abrev-fim:SCREEN-VALUE IN FRAME f-relat)
           i-cod-emitente-ini-20  =  int(i-cod-emitente-ini:SCREEN-VALUE IN FRAME f-relat)
           i-cod-emitente-fim-20  =  int(i-cod-emitente-fim:SCREEN-VALUE IN FRAME f-relat)
           c-it-codigo-ini-20     =  string(c-it-codigo-ini:SCREEN-VALUE IN FRAME f-relat)
           c-it-codigo-fim-20     =  string(c-it-codigo-fim:SCREEN-VALUE IN FRAME f-relat).

/* session:set-wait-state("general":U).*/
     {&window-name}:sensitive = no.

    
    IF INT(rs-atalhos2:SCREEN-VALUE IN FRAME f-relat) = 3 THEN
        RUN pdp/espd0036.w.
        
         IF INT(rs-atalhos2:SCREEN-VALUE IN FRAME f-relat) = 1 THEN
        RUN pdp/espd0026.w.

        IF INT(rs-atalhos2:SCREEN-VALUE IN FRAME f-relat) = 6 THEN
        RUN ftp/esft0054.w.

   IF INT(rs-atalhos2:SCREEN-VALUE IN FRAME f-relat) = 2 THEN
        RUN pdp/espd0035.w.

    IF INT(rs-atalhos2:SCREEN-VALUE IN FRAME f-relat) = 4 THEN
        RUN pdp/espd0060.w.

    IF INT(rs-atalhos2:SCREEN-VALUE IN FRAME f-relat) = 5 THEN
        RUN pdp/espd0062.w.


{&window-name}:sensitive = yes.



   /* session:set-wait-state("":U).*/

    ASSIGN c-cod-estabel-ini-20   =  "" 
           c-cod-estabel-fim-20   =  "" 
           c-tp-pedido-ini-20     =  "" 
           c-tp-pedido-fim-20     =  "" 
           dt-entrega-ini-20      =  ?  
           dt-entrega-fim-20      =  ?  
           i-nr-pedido-ini-20     =  0  
           i-nr-pedido-fim-20     =  0  
           c-nome-abrev-ini-20    =  "" 
           c-nome-abrev-fim-20    =  "" 
           i-cod-emitente-ini-20  =  0  
           i-cod-emitente-fim-20  =  0  
           c-it-codigo-ini-20     =  "" 
           c-it-codigo-fim-20     =  ""
           i-nr-pedido-indiv      =  0 
           cod-estabel-indiv      =  "".

END.




ON CHOOSE OF bt-filtro IN FRAME f-relat /* Filtro de Pedidos */
DO:

    RUN pi-monta-browse.

    if num-results("br-pedido") > 0 THEN

       ASSIGN bt-cancela2:SENSITIVE in frame f-relat        = yes
              bt-pd4000:SENSITIVE in frame f-relat          = yes
              bt-pd1001:SENSITIVE in frame f-relat          = yes
              bt-consulta-config:SENSITIVE in frame f-relat = yes
              bt-dados-adic:SENSITIVE in frame f-relat      = yes
              bt-marca:SENSITIVE in frame f-relat           = yes
              bt-desmarca:SENSITIVE in frame f-relat        = yes
              bt-marca-todos:SENSITIVE in frame f-relat     = yes
              bt-desmarca-todos:SENSITIVE in frame f-relat  = yes
              bt-hist-ped:SENSITIVE in frame f-relat        = yes
              bt-libera-fatur:SENSITIVE in frame f-relat    = yes
              bt-email-ped:SENSITIVE in frame f-relat       = yes
              bt-muda-estab:SENSITIVE in frame f-relat       = yes.

              

END.


ON CHOOSE OF bt-cancela2 IN FRAME f-relat
DO:

    RUN pi-limpa-campos.

    APPLY "entry" TO c-cod-estabel-ini IN FRAME f-relat.

END.


ON ENTRY OF c-cod-estabel-ini IN FRAME f-relat /* Empresa */
DO:

    RUN pi-limpa-campos.

    ASSIGN bt-cancela2:SENSITIVE in frame f-relat       = no
           bt-consulta-config:SENSITIVE in frame f-relat= no
           bt-dados-adic:SENSITIVE in frame f-relat     = no
           bt-marca:SENSITIVE in frame f-relat          = no
           bt-desmarca:SENSITIVE in frame f-relat       = no
           bt-marca-todos:SENSITIVE in frame f-relat    = no
           bt-desmarca-todos:SENSITIVE in frame f-relat = no
           bt-hist-ped:SENSITIVE in frame f-relat       = no
           bt-libera-fatur:SENSITIVE in frame f-relat   = no
           bt-email-ped:SENSITIVE in frame f-relat      = no
           bt-muda-estab:SENSITIVE in frame f-relat     = no

           .
        
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


ON CHOOSE OF bt-cancela IN FRAME f-relat
DO:


   RUN pi-disable-campos.
   RUN pi-disable-bt-grava.
   RUN pi-enable-outros-botoes.
   RUN pi-limpa-campos.

   APPLY "entry" TO c-cod-estabel-ini IN FRAME f-relat.

END.



ON entry OF bt-novo IN FRAME f-relat
DO:

   RUN pi-enable-bt-cancela.

   RUN pi-disable-outros-botoes.


   RUN pi-limpa-campos. 
   
   RUN pi-mostra-campos.
   RUN pi-enable-campos.

   APPLY "entry" TO c-cod-estabel-ini IN FRAME f-relat.

END.

ON CHOOSE OF bt-novo IN FRAME f-relat
DO:

   RUN pi-enable-bt-cancela.

   RUN pi-disable-outros-botoes.


   RUN pi-limpa-campos. 
   
   RUN pi-mostra-campos.
   RUN pi-enable-campos.

   APPLY "entry" TO c-cod-estabel-ini IN FRAME f-relat.

END.

ON CHOOSE OF bt-ajuda IN FRAME f-relat
DO:


END.


ON CHOOSE OF bt-sai IN FRAME f-relat
DO:

   apply "close" to this-procedure.

END.

/* ***************************  Main Block  *************************** */


ASSIGN CURRENT-WINDOW             = {&WINDOW-NAME}
   THIS-PROCEDURE:CURRENT-WINDOW  = {&WINDOW-NAME}.

assign v-cod-prog-gerado = "espd0065".



def var c-tit as char no-undo.

ASSIGN c-tit = "espd0065 - Gerenciador de Pedidos de Vendas".

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
    RUN disable_ui. 
END.


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



PAUSE 0 BEFORE-HIDE.

MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    IF SESSION:SET-WAIT-STATE("":U) THEN.
    RUN enable_UI.

    RUN pi-disable-bt-grava.   


    assign v-cod-pg-mouse-selec = "im-pg-sel".


     view c-win.
     apply "entry" to frame f-Relat.
     apply "entry" to c-win.



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
   ENABLE  
          bt-novo
          bt-cancela
          bt-sai
          bt-ajuda
          bt-pd4000
          bt-pd1001
   WITH FRAME f-relat IN WINDOW C-Win.

   {&OPEN-BROWSERS-IN-QUERY-f-relat}
   {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}


  ENABLE br-pedido   
      WITH FRAME f-relat IN WINDOW c-win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}


  VIEW C-Win.

END PROCEDURE.

PROCEDURE local-exit :
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN.
END PROCEDURE.  

PROCEDURE pi-troca-pagina:


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

PROCEDURE pi-mostra-registro.


END PROCEDURE.

PROCEDURE pi-le-primeiro.


END PROCEDURE.


PROCEDURE pi-le-proximo.

    

END PROCEDURE.


PROCEDURE pi-le-anterior.

    

END PROCEDURE.

PROCEDURE pi-le-ultimo.

    

END PROCEDURE.



PROCEDURE le-registro-goto.
    
      
END PROCEDURE.


PROCEDURE pi-limpa-campos.

    FOR EACH tt-pedido.
        DELETE tt-pedido. 
    END. 

    CLOSE QUERY br-pedido.
    open query br-pedido for each tt-pedido.
    
    if num-results("br-pedido") > 0 THEN DO:
       get current br-pedido.
    END.

END PROCEDURE.

PROCEDURE pi-enable-campos.

    ENABLE c-cod-estabel-ini  
           c-cod-estabel-fim  
                              
           c-tp-pedido-ini    
           c-tp-pedido-fim    
                              
           dt-entrega-ini     
           dt-entrega-fim     
                              
           i-nr-pedido-ini    
           i-nr-pedido-fim    
                              
           c-nome-abrev-ini   
           c-nome-abrev-fim   
                              
           i-cod-emitente-ini 
           i-cod-emitente-fim 
                              
           c-it-codigo-ini    
           c-it-codigo-fim 

           c-nome-ab-rep-ini
           c-nome-ab-rep-fim
  
           l-ped-aberto
           l-ped-at-total        
           l-ped-outros 
           l-ped-unigel-com
           
           l-interno
           l-externo

           rs-atalhos1
           rs-atalhos2
                               
           l-ped-nao-email
           l-ped-nao-integrado        
           l-ped-nao-config
           l-ped-sem-compl
               
           l-ped-lista-prime   
           l-ped-est-estrat    
           l-ped-producao  
               
           l-ped-liber-fat     
           l-ped-nao-liber-fat
           
           bt-pd4000          
           bt-pd1001          
           bt-consulta-config 
           bt-dados-adic      
           bt-hist-ped        
           bt-libera-fatur 
           bt-email-ped
           bt-muda-estab

           bt-marca
           bt-desmarca
           bt-marca-todos
           bt-desmarca-todos

           bt-cancela2
           bt-filtro 
           bt-atalho1
           bt-atalho2

        WITH FRAME f-relat.

    APPLY "entry" TO c-cod-estabel-ini IN FRAME f-relat.


END PROCEDURE.

PROCEDURE pi-mostra-campos.

   DISPLAY c-cod-estabel-ini    
           c-cod-estabel-fim    
                                
           c-tp-pedido-ini      
           c-tp-pedido-fim      
                                
           dt-entrega-ini       
           dt-entrega-fim       
                                
           i-nr-pedido-ini      
           i-nr-pedido-fim      
                                
           c-nome-abrev-ini     
           c-nome-abrev-fim     
                                
           i-cod-emitente-ini   
           i-cod-emitente-fim   
                                
           c-it-codigo-ini      
           c-it-codigo-fim      

           c-nome-ab-rep-ini
           c-nome-ab-rep-fim
                                
           l-ped-aberto 
           l-ped-at-total
           l-ped-outros 
           l-ped-unigel-com
           l-interno 
           l-externo 
                   
           rs-atalhos1
           rs-atalhos2
                                
           l-ped-nao-email
           l-ped-nao-integrado         
           l-ped-nao-config
           l-ped-sem-compl
                
           l-ped-lista-prime    
           l-ped-est-estrat     
           l-ped-producao 
                 
           l-ped-liber-fat      
           l-ped-nao-liber-fat

           bt-pd4000          
           bt-pd1001          
           bt-consulta-config 
           bt-dados-adic      
           bt-hist-ped        
           bt-libera-fatur 
           bt-email-ped
           bt-muda-estab

           bt-marca
           bt-desmarca
           bt-marca-todos
           bt-desmarca-todos

           bt-cancela2
           bt-filtro 
           bt-atalho1
           bt-atalho2

        WITH FRAME f-relat.

END PROCEDURE.


PROCEDURE pi-disable-campos.


END PROCEDURE.

PROCEDURE pi-enable-todos-campos.


END PROCEDURE.



PROCEDURE pi-le-pela-chave.

END PROCEDURE.


PROCEDURE pi-grava-registro.

END PROCEDURE.

PROCEDURE pi-disable-bt-grava.

    assign /*bt-grava:SENSITIVE in frame f-relat   = no */
           bt-cancela:SENSITIVE in frame f-relat = no.

END PROCEDURE.

PROCEDURE pi-enable-bt-cancela.

    ENABLE bt-cancela
        WITH FRAME f-relat.

END PROCEDURE.

PROCEDURE pi-disable-outros-botoes.
   ASSIGN bt-novo:SENSITIVE in frame f-relat       = no. 
   
END PROCEDURE.

PROCEDURE pi-enable-outros-botoes.

   ENABLE bt-cancela  
          bt-sai
          bt-pd4000
          bt-pd1001
   WITH FRAME f-relat IN WINDOW C-Win.
   
END PROCEDURE.

PROCEDURE pi-monta-browse:

Do:

    FOR EACH tt-pedido.
        DELETE tt-pedido.
    END. 

    CLOSE QUERY br-pedido.

    open query br-pedido for each tt-pedido.
    apply 'entry' to tt-pedido.nr-pedido in browse br-pedido. 

    if num-results("br-pedido") > 0 THEN DO:
       get current br-pedido.
    END.

    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp(input "Lendo Carteira de Pedidos").

    assign v-num-reg-lidos = 0.

    FOR EACH ped-item WHERE
        ped-item.dt-entrega   >=  date(dt-entrega-ini:SCREEN-VALUE IN FRAME f-relat)        AND
        ped-item.dt-entrega   <=  date(dt-entrega-fim:SCREEN-VALUE IN FRAME f-relat)        AND
        ped-item.ind-componen <> 3                                                          AND
        ped-item.it-codigo    >= string(c-it-codigo-ini:SCREEN-VALUE IN FRAME f-relat)      AND
        ped-item.it-codigo    <= string(c-it-codigo-fim:SCREEN-VALUE IN FRAME f-relat) 
        USE-INDEX peditem-09
        NO-LOCK,

        EACH ped-venda  OF ped-item WHERE
      /*ped-venda.dt-entrega   >= date(dt-entrega-ini:SCREEN-VALUE IN FRAME f-relat)        AND
        ped-venda.dt-entrega   <= date(dt-entrega-fim:SCREEN-VALUE IN FRAME f-relat)        AND */
        ped-venda.nr-pedido    >= INT(i-nr-pedido-ini:SCREEN-VALUE IN FRAME f-relat)        AND
        ped-venda.nr-pedido    <= INT(i-nr-pedido-fim:SCREEN-VALUE IN FRAME f-relat)        AND
        ped-venda.tp-pedido    >= string(c-tp-pedido-ini:SCREEN-VALUE IN FRAME f-relat)     AND
        ped-venda.tp-pedido    <= string(c-tp-pedido-fim:SCREEN-VALUE IN FRAME f-relat)     AND
       
        ped-venda.cod-estabel  >= string(c-cod-estabel-ini:SCREEN-VALUE IN FRAME f-relat)   AND
        ped-venda.cod-estabel  <= string(c-cod-estabel-fim:SCREEN-VALUE IN FRAME f-relat)   AND
        ped-venda.cod-emitente >= INT(i-cod-emitente-ini:SCREEN-VALUE IN FRAME f-relat)     AND
        ped-venda.cod-emitente <= INT(i-cod-emitente-fim:SCREEN-VALUE IN FRAME f-relat)
        NO-LOCK,
        each item where item.it-codigo = ped-item.it-codigo and
                        item.ge-codigo >= 40 and 
                        item.ge-codigo <= 49 no-lock.
                        

        IF string(c-nome-ab-rep-ini:SCREEN-VALUE IN FRAME f-relat) <> "" THEN DO:

            FIND FIRST ped-repres OF ped-venda
               NO-LOCK NO-ERROR.

            IF NOT AVAIL ped-repres THEN NEXT.

            IF ped-repres.nome-ab-rep < string(c-nome-ab-rep-ini:SCREEN-VALUE IN FRAME f-relat) OR 
               ped-repres.nome-ab-rep > string(c-nome-ab-rep-fim:SCREEN-VALUE IN FRAME f-relat) 
               THEN NEXT.

        END.        

        FIND FIRST estabelec OF ped-venda NO-LOCK.

        IF NOT AVAIL estabelec THEN NEXT.
       
        FIND FIRST pd-compl-pedido WHERE
             pd-compl-pedido.ep-codigo     = estabelec.ep-codigo   AND 
             pd-compl-pedido.nr-pedido     = ped-venda.nr-pedido   AND
             pd-compl-pedido.nr-sequencia  = ped-item.nr-sequencia 
             NO-LOCK NO-ERROR.
             
        FIND FIRST pd-config-pedido WHERE
             pd-config-pedido.ep-codigo     = estabelec.ep-codigo   AND 
             pd-config-pedido.nr-pedido     = ped-venda.nr-pedido   AND
             pd-config-pedido.nr-sequencia  = ped-item.nr-sequencia 
             NO-LOCK NO-ERROR.
         
          l-vai = yes.
          
        if l-ped-nao-config:checked IN FRAME f-relat    = yes or
           l-ped-nao-email:checked IN FRAME f-relat     = yes or
           l-ped-nao-integrado:checked IN FRAME f-relat = yes or
           l-ped-sem-compl:checked IN FRAME f-relat     = yes then do:
           
           l-vai = no.
           
           IF l-ped-nao-config:checked IN FRAME f-relat = yes and 
              ped-item.cod-refer = "00000001" THEN l-vai = yes.
           
           if l-ped-sem-compl:checked IN FRAME f-relat  = yes and
              not avail pd-compl-pedido THEN l-vai = yes.

           if l-ped-nao-email:checked IN FRAME f-relat  = yes and
              avail pd-compl-pedido AND pd-compl-pedido.log-1 = NO THEN l-vai = yes.

           if l-ped-nao-email:checked IN FRAME f-relat  = yes and
              not avail pd-compl-pedido THEN l-vai = yes.

           if l-ped-nao-integrado:checked IN FRAME f-relat = yes and   
              avail pd-config-pedido and pd-config-pedido.integrado-ems = no
              THEN l-vai = yes. 
           
        end.
        
        if l-vai = no then next.
        
        
          
         FIND FIRST if-ped-venda WHERE
            if-ped-venda.nr-pedido = ped-venda.nr-pedido and if-ped-venda.nr-pedido-relac <> 0
            NO-LOCK NO-ERROR.

        IF  AVAIL if-ped-venda and  l-ped-unigel-com:checked IN FRAME f-relat = yes THEN next.
        
          

        
         IF  AVAIL if-ped-venda then do:
              find first b-ped-venda where b-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac no-lock no-error.
              
              if  avail b-ped-venda and  not ( b-ped-venda.nome-abrev   >=  c-nome-abrev-ini:SCREEN-VALUE IN FRAME f-relat    AND
                     b-ped-venda.nome-abrev   <= c-nome-abrev-fim:SCREEN-VALUE IN FRAME f-relat)     then next.
              
         
         end.
         else                 
         if   not ( ped-venda.nome-abrev   >=  c-nome-abrev-ini:SCREEN-VALUE IN FRAME f-relat    AND
              ped-venda.nome-abrev   <= c-nome-abrev-fim:SCREEN-VALUE IN FRAME f-relat)     then next.
        
       
      

  
        
        if l-ped-nao-config:checked IN FRAME f-relat    = no and
           l-ped-nao-email:checked IN FRAME f-relat     = no and
           l-ped-nao-integrado:checked IN FRAME f-relat = no and
           l-ped-sem-compl:checked IN FRAME f-relat     = no then do:           
        
        
          
        
           if substring(ped-venda.nat-operacao,1,1) = "7" and 
             l-externo:checked IN FRAME f-relat = NO THEN NEXT.
         
           if substring(ped-venda.nat-operacao,1,1) <> "7" and 
             l-interno:checked IN FRAME f-relat = NO THEN NEXT.
             
           if ped-item.cod-sit-item < 3 AND
             l-ped-aberto:checked IN FRAME f-relat = NO THEN NEXT.

           if ped-item.cod-sit-item = 3 AND
             l-ped-at-total:checked IN FRAME f-relat = NO THEN NEXT.

           if ped-item.cod-sit-item > 3 AND
             l-ped-outros:checked IN FRAME f-relat = NO THEN NEXT.           
           
           

           if not avail pd-compl-pedido and             
              l-ped-producao:checked IN FRAME f-relat   = NO
              then next.

           if avail pd-compl-pedido and
             l-ped-lista-prime:checked IN FRAME f-relat = NO and
             pd-compl-pedido.tp-atendimento = 1 THEN NEXT. 
           
           if avail pd-compl-pedido and
             l-ped-est-estrat:checked IN FRAME f-relat = NO and
             pd-compl-pedido.tp-atendimento = 2 THEN NEXT.
                     
           if avail pd-compl-pedido and
             l-ped-producao:checked IN FRAME f-relat = NO and
             pd-compl-pedido.tp-atendimento = 3 THEN NEXT.
             
           
           if not avail pd-compl-pedido and
              l-ped-nao-liber-fat:checked IN FRAME f-relat = no then next.
              
                      
         
              
           if avail pd-compl-pedido then do:
           
             if pd-compl-pedido.lib-faturamento = yes and
                l-ped-liber-fat:checked IN FRAME f-relat = no THEN NEXT.
           
             if pd-compl-pedido.lib-faturamento = no and
                l-ped-nao-liber-fat:checked IN FRAME f-relat = no THEN NEXT.

           
           
           end.  
                       
              
        END.

        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

        ASSIGN larg-jr       = 0
               diex-jr       = 0
               diin-jr       = 0
               nr-pedcli-jr  = "".
        
        FIND FIRST var-result WHERE var-result.nome-var = "pedcli"  AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)           AND
             var-result.item-cotacao = ped-item.it-codigo 
             NO-LOCK NO-ERROR.
        
        IF AVAIL var-result THEN
             ASSIGN nr-pedcli-jr = var-result.valor-char.
        
        FIND FIRST var-result WHERE var-result.nome-var = "Largura" AND 
             var-result.nr-estrut    = int(ped-item.cod-refer)           AND
             var-result.item-cotacao = ped-item.it-codigo 
             NO-LOCK NO-ERROR.
        
        IF AVAIL var-result THEN
             ASSIGN larg-jr = var-result.valor-dec.
        
        
        FIND FIRST var-result WHERE var-result.nome-var = "DIEX" AND 
             var-result.nr-estrut = int(ped-item.cod-refer)           AND
             var-result.item-cotacao = ped-item.it-codigo 
             NO-LOCK NO-ERROR.
        
        IF AVAIL var-result THEN
             ASSIGN diex-jr = var-result.valor-dec.

        FIND FIRST var-result WHERE var-result.nome-var = "DIIN" AND 
             var-result.nr-estrut = int(ped-item.cod-refer)           AND
             var-result.item-cotacao = ped-item.it-codigo 
             NO-LOCK NO-ERROR.
        
        IF AVAIL var-result THEN
             ASSIGN diin-jr = var-result.valor-dec.


        FIND FIRST var-result WHERE var-result.nome-var = "QTDPEDIDO" AND 
             var-result.nr-estrut = int(ped-item.cod-refer)           AND
             var-result.item-cotacao = ped-item.it-codigo 
             NO-LOCK NO-ERROR.
        
        qt-configurada-jr = 0.
        

        IF AVAIL var-result THEN
             ASSIGN qt-configurada-jr = var-result.valor-dec.

        FIND FIRST var-result WHERE var-result.nome-var = "QTDBOB" AND 
             var-result.nr-estrut = int(ped-item.cod-refer)           AND
             var-result.item-cotacao = ped-item.it-codigo 
             NO-LOCK NO-ERROR.
        
        
        qt-bobinas-jr = 0.

        IF AVAIL var-result THEN
             ASSIGN qt-bobinas-jr = var-result.valor-dec.


        IF ped-item.cod-sit-item = 1 THEN ASSIGN situacao-jr = "ABR".
        IF ped-item.cod-sit-item = 2 THEN ASSIGN situacao-jr = "ABR".
        IF ped-item.cod-sit-item = 3 THEN ASSIGN situacao-jr = "LIQ".
        IF ped-item.cod-sit-item = 4 THEN ASSIGN situacao-jr = "PEN".
        IF ped-item.cod-sit-item = 5 THEN ASSIGN situacao-jr = "SUS".
        IF ped-item.cod-sit-item = 6 THEN ASSIGN situacao-jr = "CAN".
        IF ped-item.cod-sit-item = 7 THEN ASSIGN situacao-jr = "OUT".

        CREATE tt-pedido.
        
        ASSIGN tt-pedido.cod-estabel     = ped-venda.cod-estabel
               tt-pedido.nr-pedido       = ped-venda.nr-pedido
               tt-pedido.nr-sequencia    = ped-item.nr-sequencia   
               tt-pedido.nr-pedcli       = ped-venda.nr-pedcli
               tt-pedido.tp-pedido       = ped-venda.tp-pedido    
               tt-pedido.it-codigo       = ped-item.it-codigo 
               tt-pedido.qt-pedida       = ped-item.qt-pedida   
               tt-pedido.qt-atendida     = ped-item.qt-atendida    
               tt-pedido.larg            = larg-jr      
               tt-pedido.diex            = diex-jr  
               tt-pedido.diin            = diin-jr 
               tt-pedido.nr-pedcli-cli   = nr-pedcli-jr
               tt-pedido.nome-abrev      = ped-venda.nome-abrev  
               tt-pedido.cod-emitente    = ped-venda.cod-emitente 
               tt-pedido.dt-entrega      = ped-item.dt-entrega
               tt-pedido.nat-operacao    = ped-item.nat-operacao   
               tt-pedido.preco-com-icm   = ped-item.vl-preuni
               tt-pedido.mercado         = IF substring(ped-item.nat-operacao,1,1) = "7" THEN "EXT" ELSE "INT"     
               tt-pedido.canal-venda     = ped-venda.cod-canal-venda   
               tt-pedido.situacao        = situacao-jr. 


   
        FIND FIRST bf-if-ped-venda WHERE
            bf-if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido
            NO-LOCK NO-ERROR.

        IF  AVAIL bf-if-ped-venda  then   tt-pedido.unig-com = "*".
         else  tt-pedido.unig-com = "". 


        IF AVAIL pd-compl-pedido THEN DO:

            ASSIGN tt-pedido.dt-entrega-cli   = pd-compl-pedido.dt-entrega-cli
                   tt-pedido.dt-faturamento   = pd-compl-pedido.dt-faturamento
                   tt-pedido.cod-estabel-fat  = ped-venda.cod-estabel /*pd-compl-pedido.cod-estabel-fat unigel comercial-edson*/
                   tt-pedido.nat-operacao-fat = ped-venda.nat-operacao /*pd-compl-pedido.nat-operacao-fat unigel comercial - edson*/
                   tt-pedido.preco-ex-imp     = pd-compl-pedido.preco-pis-cof.

            
            IF TRIM(SUBSTRING(pd-compl-pedido.char-1,51,10)) <> "" THEN
                  tt-pedido.nr-pedcli-cli = TRIM(SUBSTRING(pd-compl-pedido.char-1,51,10)) + "-" + TRIM(SUBSTRING(pd-compl-pedido.char-1,61,4)).


            ASSIGN tt-pedido.perc-desc-preco = DEC (substring(pd-compl-pedido.char-1,31,10)).

            IF pd-compl-pedido.cod-estabel-fat <> "" THEN
                ASSIGN tt-pedido.preco-com-icm = DEC (substring(pd-compl-pedido.char-1,16,15)).

            IF pd-compl-pedido.lib-faturamento = YES THEN
                ASSIGN tt-pedido.liber-fat = "Sim".

            IF pd-compl-pedido.log-1 = YES THEN
                ASSIGN tt-pedido.enviado-email = "Sim".
            ELSE
                ASSIGN tt-pedido.enviado-email = "N∆o".

        END.

        ELSE
            ASSIGN tt-pedido.enviado-email = "N∆o".

        ASSIGN tt-pedido.qt-configurada = qt-configurada-jr
               tt-pedido.qt-bobinas     = qt-bobinas-jr.

        ASSIGN qt-produzida-jr = 0.

        FOR EACH pallet WHERE
            pallet.it-codigo    = ped-item.it-codigo    AND
            pallet.nr-pedido    = ped-venda.nr-pedido   AND
            pallet.nr-sequencia = ped-item.nr-sequencia AND 
            pallet.situacao     = 2                     /*and 
            pallet.cod-estabel  = ped-venda.cod-estabel */
            no-lock USE-INDEX pedido
            BREAK BY pallet.nr-pallet.

            IF LAST-OF(pallet.nr-pallet) THEN
            ASSIGN qt-produzida-jr = qt-produzida-jr + pallet.peso-liquido.

        END.
        find first if-ped-venda where if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido no-lock no-error.
        
        if avail if-ped-venda then do:
         
            FOR EACH pallet WHERE
               pallet.it-codigo    = ped-item.it-codigo    AND
               pallet.nr-pedido    = if-ped-venda.nr-pedido   AND
               pallet.nr-sequencia = ped-item.nr-sequencia AND 
               pallet.situacao     = 2                     /*and 
               pallet.cod-estabel  = ped-venda.cod-estabel */
               no-lock USE-INDEX pedido
               BREAK BY pallet.nr-pallet.
   
               IF LAST-OF(pallet.nr-pallet) THEN
               ASSIGN qt-produzida-jr = qt-produzida-jr + pallet.peso-liquido.
   
           END.
           
        end.
        find first if-ped-venda where if-ped-venda.nr-pedido = ped-venda.nr-pedido no-lock no-error.
        
        if avail if-ped-venda then do:
         
            FOR EACH pallet WHERE
               pallet.it-codigo    = ped-item.it-codigo    AND
               pallet.nr-pedido    = if-ped-venda.nr-pedido-relac   AND
               pallet.nr-sequencia = ped-item.nr-sequencia AND 
               pallet.situacao     = 2                     /*and 
               pallet.cod-estabel  = ped-venda.cod-estabel */
               no-lock USE-INDEX pedido
               BREAK BY pallet.nr-pallet.
   
               IF LAST-OF(pallet.nr-pallet) THEN
               ASSIGN qt-produzida-jr = qt-produzida-jr + pallet.peso-liquido.
   
           END.
           
        end.



        ASSIGN tt-pedido.qt-produzida = qt-produzida-jr.

        FIND FIRST if-ped-venda WHERE
            if-ped-venda.nr-pedido = ped-venda.nr-pedido
            NO-LOCK NO-ERROR.

        IF AVAIL if-ped-venda THEN DO:

            FIND bf-ped-venda WHERE
                bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac
                NO-LOCK NO-ERROR.

            IF AVAIL bf-ped-venda THEN DO:

                FIND FIRST bf-ped-item OF bf-ped-venda WHERE
                       bf-ped-item.nr-sequencia = ped-item.nr-sequencia AND
                       bf-ped-item.ind-componen <> 3
                       NO-LOCK NO-ERROR.

                IF AVAIL bf-ped-item THEN DO:

                   ASSIGN tt-pedido.cod-estabel-fat   = bf-ped-venda.cod-estabel 
                          tt-pedido.nome-abrev-final  = bf-ped-venda.nome-abrev
                          tt-pedido.nr-pedido-final   = bf-ped-venda.nr-pedido
                          tt-pedido.qt-pedida-final   = bf-ped-item.qt-pedida   
                          tt-pedido.qt-atendida-final = bf-ped-item.qt-atendida.    
                   
                END.
                   

            END.

        END.
        else do:
                FIND FIRST if-ped-venda WHERE
                 if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido
                    NO-LOCK NO-ERROR.
        
                IF AVAIL if-ped-venda THEN DO:
        
                     
        
                           ASSIGN tt-pedido.cod-estabel-fat   = ped-venda.cod-estabel .    
                           
                     
        
                END.

        
        end.


    END. 

       /*** habilita **/

     open query br-pedido for each tt-pedido.

     if num-results("br-pedido") > 0 THEN DO:
        get current br-pedido.
        apply 'entry' to tt-pedido.nr-pedido in browse br-pedido. 
     END.

  end. 

  run pi-finalizar in h-acomp.

END PROCEDURE.

PROCEDURE pi-libera-faturamento.

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

        DEFINE VARIABLE c-cod-estabel-gt  AS CHAR    NO-UNDO.
        DEFINE VARIABLE i-nr-pedido-gt    AS INTEGER NO-UNDO.
        DEFINE VARIABLE i-nr-sequencia-gt AS INTEGER NO-UNDO.
        DEFINE VARIABLE c-nome-abrev-gt   AS CHAR    NO-UNDO.

        ASSIGN c-mensagem-jr = "Deseja Liberar Todos os Pedidos Marcados para Faturamento?".
        

        DEFINE RECTANGLE ex-rect-1
         EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
         SIZE 59 BY 6.50.

        DEFINE FRAME ex-frame-1


            c-mensagem-jr NO-LABEL 
               at ROW 4 col 10 

            ex-rect-1 AT ROW 1.9 COL 2

            ex-bt-ok          AT ROW 9.3 COL 2.14
            ex-bt-cancel      AT ROW 9.3 COL 13           
            ex-rt-botoes      AT ROW 9.0 COL 1
            SPACE(0.28)
            WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
                 THREE-D SCROLLABLE TITLE "Libera Pedidos para Faturamento" FONT 1
                 DEFAULT-BUTTON ex-bt-ok CANCEL-BUTTON ex-bt-cancel.

        ON "CHOOSE":U OF ex-bt-ok IN FRAME ex-frame-1 DO:

            RUN pi-ex-bt-ok.

            RETURN.

        END.

        ENABLE ex-bt-cancel ex-bt-ok  
            WITH FRAME ex-frame-1. 

        DISPLAY c-mensagem-jr 
            WITH FRAME ex-frame-1.

        WAIT-FOR "GO":U OF FRAME ex-frame-1.

END PROCEDURE.

 PROCEDURE pi-ex-bt-ok.
    
                FOR EACH tt-pedido WHERE tt-pedido.marca = "*".
    
                    ASSIGN tt-pedido.marca = "".
    
                    FIND FIRST estabelec WHERE
                        estabelec.cod-estabel = tt-pedido.cod-estabel
                        NO-LOCK NO-ERROR.
    
                    IF NOT AVAIL estabelec THEN NEXT.
    
                    FIND FIRST pd-compl-pedido WHERE
                        pd-compl-pedido.ep-codigo    = estabelec.ep-codigo   AND
                        pd-compl-pedido.nr-pedido    = tt-pedido.nr-pedido   AND
                        pd-compl-pedido.nr-sequencia = tt-pedido.nr-sequencia
                        EXCLUSIVE-LOCK NO-ERROR.
                    
                    IF AVAIL pd-compl-pedido THEN DO:
                    
                    
                        ASSIGN pd-compl-pedido.lib-faturamento = YES.

                        FIND FIRST pd-compl-pedido WHERE
                        pd-compl-pedido.ep-codigo    = estabelec.ep-codigo   AND
                        pd-compl-pedido.nr-pedido    = tt-pedido.nr-pedido   AND
                        pd-compl-pedido.nr-sequencia = tt-pedido.nr-sequencia
                        NO-LOCK NO-ERROR.

                        
                        IF AVAIL pd-compl-pedido THEN run pdp\upc\trw-pd-compl-pedido-manual.p (input rowid(pd-compl-pedido)).
                    END.
                    FIND FIRST pd-compl-pedido WHERE
                        pd-compl-pedido.ep-codigo    = estabelec.ep-codigo   AND
                        pd-compl-pedido.nr-pedido    = tt-pedido.nr-pedido   AND
                        pd-compl-pedido.nr-sequencia = tt-pedido.nr-sequencia
                        NO-LOCK NO-ERROR.
    
                END.
END PROCEDURE.

PROCEDURE pi-cria-planilha:
/*
   DEF VAR c-arquivo AS CHAR NO-UNDO.
*/
   ASSIGN c-arquivo = c-arq + 'mod-espd0065' + STRING(time)+ '.xls'.

   OS-COPY VALUE(c-modelo-planilha) VALUE(c-arquivo).
   
   ASSIGN c-arq-anexo = "".
   
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

END PROCEDURE.


PROCEDURE pi-gera-e-mail: 

   ASSIGN c-email-responsavel = "" 
          c-nome-responsavel  = "".

   FIND FIRST usuar_mestre WHERE            
        usuar_mestre.cod_usuario = c-seg-usuario 
        NO-LOCK NO-ERROR.

   IF NOT AVAIL usuar_mestre THEN RETURN "nok". 
       
   ASSIGN c-email-responsavel = usuar_mestre.cod_e_mail_local 
          c-nome-responsavel  = usuar_mestre.nom_usuario.

   FIND FIRST param-global NO-LOCK NO-ERROR.
   FIND FIRST param_email  NO-LOCK NO-ERROR.

   IF NOT AVAIL param-global THEN DO:
       MESSAGE "N∆o encontrado parametro global"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN "nok".
   END.

   IF c-email-responsavel = "" OR 
      c-e-mail-cliente = "" THEN RETURN "NOK".

   FOR EACH tt-envio.
       DELETE tt-envio.
   END.

   CREATE tt-envio.

   ASSIGN 
       tt-envio.versao-integracao = 1
       tt-envio.remetente   =  c-email-responsavel
       tt-envio.destino     =  c-e-mail-cliente
       tt-envio.copia       =  ""
       tt-envio.assunto     =  string(assunto-jr)  

       tt-envio.mensagem    = "Srs," + chr(10) + CHR(10)  + 
       "Segue anexo a planilha de confirmaá∆o do(s) pedido(s)" 
       + CHR(10) + CHR(10) +
       "Caso n∆o haja nenhuma comunicaá∆o em 24 horas, estaremos considerando pedido aceito." + CHR(10) +
       CHR(10) + 
       chr(10) + CHR(10) + "Obrigado." + CHR(10) + CHR(10) + c-nome-responsavel

       tt-envio.importancia = 2
       tt-envio.log-enviada = yes
       tt-envio.log-lida    = yes
       tt-envio.acomp       = yes
       tt-envio.arq-anexo   = c-arquivo
       
       tt-envio.servidor    = param_email.cod_servid_e_mail
       tt-envio.porta       = param_email.num_porta 
       tt-envio.exchange    = param_email.log_servid_exchange.
       

   OUTPUT TO VALUE(c-arq + "espd0065MAIL.txt").

   run utp/utapi009.p (input  table tt-envio,
                       output table tt-erros).    

   FOR EACH tt-erros NO-LOCK:
       MESSAGE tt-erros.cod-erro SKIP tt-erros.desc-erro
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.

   OUTPUT CLOSE. 

   RETURN "OK".

END PROCEDURE.

PROCEDURE pi-gera-planilha.

def var c-est-plan as char no-undo.
DEFINE VARIABLE l-ipi AS LOGICAL     NO-UNDO.
c-est-plan = "".

    ASSIGN cod-emitente-jr = 0
           soma-email      = 0
           c-tem-email     = "".

    FOR EACH tt-pedido WHERE
        tt-pedido.marca = "*" /*and
        tt-pedido.enviado-email <> "sim"*/ NO-LOCK.

        IF tt-pedido.cod-emitente <> cod-emitente-jr AND
           cod-emitente-jr <> 0 THEN DO:

            MESSAGE "N∆o Ç possivel enviar e-mail para mais de um cliente ao mesmo tempo"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN "nok".

        END.

        ASSIGN cod-emitente-jr = tt-pedido.cod-emitente
               soma-email      = soma-email + 1.

    END.

    IF soma-email > 25 THEN DO:

       MESSAGE "N∆o Ç possivel selecionar mais de 25 pedidos em cada e-mail"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN "nok".

    END.
                           

    ASSIGN soma-email = 0 
           i-linha    = 8.

    FOR EACH tt-pedido WHERE
        tt-pedido.marca = "*" /*AND 
        tt-pedido.enviado-email <> "sim"*/ NO-LOCK.

        ASSIGN soma-email = soma-email + 1.

        IF soma-email > 25 THEN NEXT.

        FIND FIRST emitente WHERE
            emitente.cod-emitente = tt-pedido.cod-emitente
            NO-LOCK NO-ERROR.

    FIND FIRST ped-venda WHERE
            ped-venda.nr-pedido = tt-pedido.nr-pedido
            NO-LOCK NO-ERROR.
        
        IF NOT AVAIL ped-venda THEN NEXT.

        FIND FIRST estabelec WHERE
               estabelec.cod-estabel = tt-pedido.cod-estabel-fat
               NO-LOCK NO-ERROR.

        IF NOT AVAIL estabelec THEN NEXT.   


        FIND FIRST pd-compl-pedido WHERE
             pd-compl-pedido.ep-codigo            = estabelec.ep-codigo   AND
             pd-compl-pedido.nr-pedido            = tt-pedido.nr-pedido   AND
             pd-compl-pedido.nr-sequencia         = tt-pedido.nr-sequencia 
            NO-LOCK NO-ERROR.
        IF NOT AVAIL pd-compl-pedido THEN NEXT.
 
        FIND FIRST ped-item OF ped-venda WHERE
            ped-item.it-codigo    = tt-pedido.it-codigo AND
            ped-item.nr-sequencia = tt-pedido.nr-sequencia AND
            ped-item.ind-componen < 3 
            NO-LOCK NO-ERROR.
        
        IF NOT AVAIL ped-item THEN NEXT.
  
        /* Rotina para encontrar o % de ICMS do pedido de venda */


        ASSIGN estab-faturx        = ped-venda.cod-estabel
               cod-emitente-faturx = ped-venda.cod-emitente
               nat-operacao-faturx = ped-item.nat-operacao.

        RUN pi-acha-perc-icms.


        
        l-ipi = NO.
        FOR FIRST   ITEM WHERE ITEM.it-codigo = ped-item.it-codigo NO-LOCK ,
                FIRST natur-oper WHERE natur-oper.nat-operacao = ped-item.nat-operacao NO-LOCK .
        
            l-ipi = (natur-oper.cd-trib-ipi = 1 and item.cd-trib-ipi <> 1).

            IF item.cd-trib-ipi = 1  THEN
                l-ipi = YES.
        
            
        
        END.


        ASSIGN i-linha = i-linha + 1.

        ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-pedido.cod-estabel-fat
               c-relatorio:range("B" + STRING(i-linha)):VALUE = tt-pedido.dt-entrega
               c-relatorio:range("C" + STRING(i-linha)):VALUE = tt-pedido.tp-pedido
               c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-pedido.nr-pedcli
               c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-pedido.nr-sequencia
               c-relatorio:range("F" + STRING(i-linha)):VALUE = tt-pedido.nome-abrev.
        
        ASSIGN c-relatorio:range("G" + STRING(i-linha)):VALUE  = tt-pedido.it-codigo
               c-relatorio:range("H" + STRING(i-linha)):VALUE  = tt-pedido.nr-pedcli-cli
               c-relatorio:range("I" + STRING(i-linha)):VALUE  = tt-pedido.qt-pedida
               c-relatorio:range("J" + STRING(i-linha)):VALUE  = tt-pedido.larg
               c-relatorio:range("K" + STRING(i-linha)):VALUE  = tt-pedido.diin
               c-relatorio:range("L" + STRING(i-linha)):VALUE  = tt-pedido.diex
               c-relatorio:range("N" + STRING(i-linha)):VALUE  = pd-compl-pedido.perc-enc-finan
               c-relatorio:range("O" + STRING(i-linha)):VALUE  = tt-pedido.preco-com-icm
               c-relatorio:range("P" + STRING(i-linha)):VALUE  = perc-icms-faturx
               c-relatorio:range("Q" + STRING(i-linha)):VALUE  = IF l-ipi THEN ped-item.aliquota-ipi ELSE 0.

        ASSIGN c-relatorio:range("M" + STRING(i-linha)):VALUE  = pd-compl-pedido.preco-pis-cof
               c-relatorio:range("U" + STRING(i-linha)):VALUE  = ped-venda.nome-transp
               c-relatorio:range("V" + STRING(i-linha)):VALUE  = ped-venda.nome-tr-red
               c-relatorio:range("R" + STRING(i-linha)):VALUE  = DEC(SUBSTRING(pd-compl-pedido.char-1,31,10)).  


        ASSIGN c-relatorio:range("S" + STRING(i-linha)):VALUE  = IF tt-pedido.cod-estabel-fat = "422" OR tt-pedido.cod-estabel-fat = "412" THEN "MTN" ELSE
                                                                 IF tt-pedido.cod-estabel-fat = "424" THEN "SBC" ELSE
                                                                 IF tt-pedido.cod-estabel-fat = "434" OR tt-pedido.cod-estabel-fat = "442" THEN "MTN" ELSE
                                                                 IF tt-pedido.cod-estabel-fat = "432" OR tt-pedido.cod-estabel-fat = "443" THEN "SBC" ELSE 
                                                                 "UNG". /*solic-318*/ 
        
        
           if index(c-est-plan, tt-pedido.cod-estabel-fat) = 0 then 
           
               c-est-plan = c-est-plan + "," + tt-pedido.cod-estabel-fat.

       

        ASSIGN c-relatorio:range("T" + STRING(i-linha)):VALUE  = IF ped-venda.cidade-cif = "" THEN "Fob" ELSE "Cif".


        FIND FIRST cond-pagto WHERE
            cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag
            NO-LOCK NO-ERROR.
        
        IF AVAIL cond-pagto THEN
           ASSIGN c-relatorio:range("W" + STRING(i-linha)):VALUE = cond-pagto.descricao.
           

        FIND FIRST usuar_mestre WHERE            
             usuar_mestre.cod_usuario = c-seg-usuario 
             NO-LOCK NO-ERROR.


        

        IF AVAIL usuar_mestre THEN  DO:

            IF c-seg-usuario = "eltpala" THEN 
               ASSIGN c-relatorio:range("A" + STRING(43)):VALUE  = usuar_mestre.nom_usuario
                      c-relatorio:range("A" + STRING(44)):VALUE  = "(11) 3478 5964".

            IF c-seg-usuario = "aabianc" THEN 
               ASSIGN c-relatorio:range("A" + STRING(43)):VALUE  = usuar_mestre.nom_usuario
                      c-relatorio:range("A" + STRING(44)):VALUE  = "(11) 3478 5965".

            IF c-seg-usuario = "jnsmore" THEN 
               ASSIGN c-relatorio:range("A" + STRING(43)):VALUE  = usuar_mestre.nom_usuario
                      c-relatorio:range("A" + STRING(44)):VALUE  = "(11) 3478 5966".

            IF c-seg-usuario = "rcapire" THEN 
               ASSIGN c-relatorio:range("A" + STRING(43)):VALUE  = usuar_mestre.nom_usuario
                      c-relatorio:range("A" + STRING(44)):VALUE  = "(11) 3478 5967".

             IF c-seg-usuario = "apcsilv" THEN 
               ASSIGN c-relatorio:range("A" + STRING(43)):VALUE  = usuar_mestre.nom_usuario
                      c-relatorio:range("A" + STRING(44)):VALUE  = "(11) 3478 5955".

        END.
           
        ASSIGN c-relatorio:range("O" + STRING(37)):VALUE  = c-obs-email-jr.

        assign c-tem-email = "sim".   

    END.  
    
    i-linha = 1.
    
        if INDEX( c-est-plan,"432") > 0 OR INDEX( c-est-plan,"443") > 0 then do: /*solic-318*/ 
                    ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE  = 
                    "POLO (UNIGEL COMERCIAL-SBC) - Pedido faturado po SP".
           i-linha = i-linha + 1.
        end.
        
         if INDEX( c-est-plan,"434") > 0 OR INDEX( c-est-plan,"442") > 0 then do: /*solic-318*/ 
                    ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE  = 
                    "POLO (UNIGEL COMERCIAL-CANOAS) - Pedido faturado po RS".
           i-linha = i-linha + 1.
        end. 
         
    
    if c-tem-email = "sim" then do:

      FOR EACH tt-pedido WHERE
          tt-pedido.marca = "*" /*AND 
          tt-pedido.enviado-email <> "sim"*/ NO-LOCK.
               
          FIND FIRST pd-compl-pedido WHERE
               pd-compl-pedido.ep-codigo            = estabelec.ep-codigo   AND
               pd-compl-pedido.nr-pedido            = tt-pedido.nr-pedido   AND
               pd-compl-pedido.nr-sequencia         = tt-pedido.nr-sequencia 
               NO-ERROR.
         
          IF AVAIL pd-compl-pedido THEN 
              ASSIGN pd-compl-pedido.log-1 = yes.
          FIND FIRST pd-compl-pedido WHERE
               pd-compl-pedido.ep-codigo            = estabelec.ep-codigo   AND
               pd-compl-pedido.nr-pedido            = tt-pedido.nr-pedido   AND
               pd-compl-pedido.nr-sequencia         = tt-pedido.nr-sequencia 
               NO-LOCK NO-ERROR.

      END.
      
    end.  

END PROCEDURE.

PROCEDURE pi-envia-email-pedido.

    FIND FIRST tt-pedido WHERE
        tt-pedido.marca = "*"
        NO-LOCK NO-ERROR.
    
    IF NOT AVAIL tt-pedido THEN RETURN.
    
    
     FIND FIRST if-ped-venda WHERE
            if-ped-venda.nr-pedido = tt-pedido.nr-pedido and
            if-ped-venda.nr-pedido-relac <> 0
            NO-LOCK NO-ERROR.

        IF AVAIL if-ped-venda THEN return no-apply.
        
    FIND FIRST emitente WHERE
        emitente.cod-emitente = tt-pedido.cod-emitente
        NO-LOCK NO-ERROR.

    IF NOT AVAIL emitente THEN RETURN.

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


    DEFINE VARIABLE c-e-mail-gt     AS CHAR FORMAT "x(400)" NO-UNDO.
    DEFINE VARIABLE c-nome-abrev-gt AS CHAR FORMAT "x(12)" NO-UNDO.
    
    DEFINE RECTANGLE gt-rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 8.50.
    
    DEFINE FRAME gt-frame-1

        c-nome-abrev-gt LABEL "Nome Abrv.Cliente" AT ROW 2.5 COL 15 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 43 BY .88
        
        c-e-mail-gt LABEL "Endereáo de E-mail" AT ROW 3.5 COL 15 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 43 BY .88

        c-obs-email-jr LABEL "Observaá‰es" at ROW 4.5 col 15 COLON-ALIGNED

        
        gt-rect-1 AT ROW 1.9 COL 2

        gt-bt-ok          AT ROW 11.3 COL 2.14
        gt-bt-cancel      AT ROW 11.3 COL 13             
        gt-rt-botoes      AT ROW 11.0 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "Envio de E-mail a Clientes" FONT 1
             DEFAULT-BUTTON gt-bt-ok CANCEL-BUTTON gt-bt-cancel.

    ON "CHOOSE":U OF gt-bt-ok IN FRAME gt-frame-1 DO:

        ASSIGN c-e-mail-cliente = c-e-mail-gt:SCREEN-VALUE IN FRAME gt-frame-1
               c-obs-email-jr   = c-obs-email-jr:SCREEN-VALUE IN FRAME gt-frame-1
               c-e-mail-cliente = replace(c-e-mail-cliente,";",",").

        
        if avail emitente and num-entries(c-e-mail-cliente) > 0 then do:
        
        
       
            for each cont-emit of emitente where cont-emit.area = "EMAIL-PED" .
                   delete cont-emit.
            end.

      
        
        
            do i-pos = 1 to num-entries(c-e-mail-cliente):
            
                l-tem = no.
                for each cont-emit of emitente where cont-emit.area = "EMAIL-PED" NO-LOCK.
                    if trim(cont-emit.e-mail) = trim(entry(i-pos,c-e-mail-cliente)) then do:
                        l-tem = yes.
                        leave.
                    end.
                end.
                
                if l-tem then next.
                
                find last cont-emit of emitente no-error.
                
                if not avail cont-emit then i-seq = 10.
                else
                i-seq = cont-emit.sequencia + 10.
                
                create cont-emit.
                assign
                    cont-emit.cod-emitente = emitente.cod-emitente
                    cont-emit.sequencia    = i-seq
                    cont-emit.e-mail       = entry(i-pos,c-e-mail-cliente)
                    cont-emit.area         = "EMAIL-PED"
                    cont-emit.observacao   = "cadastrado autom†tico - Pedidos"
                    cont-emit.nome         = entry(i-pos,c-e-mail-cliente)
                    cont-emit.int-2        = 1.
                    
            end.
        
        
        end.

                   /* Cria Aplicaá∆o do Excel */

        CREATE "Excel.Application" c-excel.
        ASSIGN c-excel:DisplayAlerts = FALSE .
        
        ASSIGN c-modelo-planilha = search("modelos/mod-espd0065.xls") 
               c-arq             = SESSION:TEMP-DIRECTORY.

        
        RUN pi-cria-planilha.

        RUN pi-gera-planilha.

        RUN pi-finaliza-impressao.

        /*c-excel:QUIT().*/
        RELEASE OBJECT c-relatorio.
        RELEASE OBJECT c-planilha.
        RELEASE OBJECT c-excel.
        
        
        if c-tem-email = "sim" then do:

           FOR EACH tt-ped-assunto.
               DELETE tt-ped-assunto.
           END.

           FOR EACH tt-pedido WHERE
               tt-pedido.marca = "*"
               NO-LOCK.

               FIND FIRST tt-ped-assunto WHERE
                   tt-ped-assunto.nr-pedido = tt-pedido.nr-pedido
             
                   NO-ERROR.

               IF NOT AVAIL tt-ped-assunto THEN DO:

                   CREATE tt-ped-assunto.
                   ASSIGN tt-ped-assunto.nr-pedido = tt-pedido.nr-pedido
                          tt-ped-assunto.nr-pedcli = tt-pedido.nr-pedcli.

               END.

           END.

           ASSIGN assunto-jr = "POLO - Confirmaá∆o de Pedido " + c-nome-abrev-gt + " ".

           FOR EACH tt-ped-assunto NO-LOCK.

               ASSIGN assunto-jr = assunto-jr + STRING(tt-ped-assunto.nr-pedcli) + ",".

           END.

           RUN pi-gera-e-mail.

        end.   
        
        
        RETURN.

    END.

    ASSIGN c-e-mail-gt = emitente.e-mail.

    for each cont-emit where cont-emit.cod-emitente = emitente.cod-emitente and
        cont-emit.area = "EMAIL-PED" no-lock.
        
     if index(c-e-mail-gt,cont-emit.e-mail) >  0 then next.
     
        if trim(c-e-mail-gt) = "" then c-e-mail-gt = cont-emit.e-mail.
        else
        c-e-mail-gt =  c-e-mail-gt + "," + cont-emit.e-mail.
     
    end.
    
    if trim(c-e-mail-gt) = "" then 
         ASSIGN c-e-mail-gt = emitente.e-mail.
    
    assign  c-nome-abrev-gt = emitente.nome-abrev.

    ASSIGN c-obs-email-jr = "".

    DISPLAY c-nome-abrev-gt c-e-mail-gt c-obs-email-jr WITH FRAME gt-frame-1.

    ENABLE c-e-mail-gt c-obs-email-jr gt-bt-ok gt-bt-cancel 
        WITH FRAME gt-frame-1. 
    
    WAIT-FOR "GO":U OF FRAME gt-frame-1.

    open query br-pedido for each tt-pedido.
    apply 'entry' to tt-pedido.marca in browse br-pedido. 

END PROCEDURE.


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
               perc-desc-icms-faturx = bf-natur-oper.per-des-icms.
       
       if bf-natur-oper.cd-trib-icm = 1 then do:
       
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
          
       if bf-natur-oper.cd-trib-icm = 4 then do:
       
            IF bf-emitente.estado = bf-estabelec.estado  THEN
                assign perc-icms-faturx = 12.
       
       end. 

    END.

END PROCEDURE.
PROCEDURE  pi-changeRowColor.
    DEFINE INPUT PARAM p-row AS HANDLE NO-UNDO.
    DEFINE VARIABLE i-cor AS INTEGER     NO-UNDO.
     i-cor = 11.
 IF (ABS(tt-pedido.qt-configurada - tt-pedido.qt-pedida) / tt-pedido.qt-pedida * 100) >= 2 THEN 
     i-cor = 14. 
 IF (ABS(tt-pedido.qt-configurada - tt-pedido.qt-pedida) / tt-pedido.qt-pedida * 100) >= 5 THEN       
        i-cor = 12.

 tt-pedido.qt-configurada:BGCOLOR in BROWSE br-pedido = i-cor.
   
END PROCEDURE.

RETURN 'ok'.

  






