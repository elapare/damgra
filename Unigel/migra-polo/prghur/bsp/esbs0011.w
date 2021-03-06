&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/* *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i esbs0011 1.0.00.00.000}
 
/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat�rio                            */
/* Obs: Retirar o valor do preprocessador para as p�ginas que n�o existirem  */

&GLOBAL-DEFINE PGLAY f-pg-lay


&GLOBAL-DEFINE PGLOG f-pg-log
&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGPAR f-pg-par

/* Parameters Definitions ---                                           */
{bf/buffersHCM.i}

/* Temporary Table Definitions ---                                      */

define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer.
    
define temp-table tt-row
    field r-movto as rowid
    field ano     as integer
    index r-row is primary unique
         ano
         r-movto.
         
define temp-table tt-refer_movto_benefic like refer_movto_benefic.
define temp-table tt-movto_benefic like movto_benefic.

DEF  TEMP-TABLE tt-beneficio NO-UNDO
    FIELD cdn_beneficio            LIKE beneficio.cdn_beneficio
    FIELD des_beneficio            LIKE beneficio.des_beneficio
    FIELD cdn_regra_benefic        LIKE regra_benefic.cdn_regra_benefic
    FIELD des_regra_benefic        LIKE regra_benefic.des_regra_benefic
    FIELD idi_tip_benefic          LIKE beneficio.idi_tip_benefic
    FIELD cdn_prestdor_serv        LIKE prestdor_serv.cdn_prestdor_serv
    FIELD cdn_prestdor_serv_aux    LIKE prestdor_serv.cdn_prestdor_serv
    FIELD log_beneficio_utiliz     AS LOG FORMAT "Sim/N�o"
    FIELD log_beneficio_utiliz_aux AS LOG FORMAT "Sim/N�o".

def  temp-table erros
    field num_cont                as int  format 99999
    field num_reg                 as int  format 99999
    field des_erro                as char format "x(30)"
    field log_erro                as log
    field cdn_empresa             like funcionario.cdn_empresa
    field cdn_estab               like funcionario.cdn_estab
    field cdn_funcionario         like funcionario.cdn_funcionario
    field cdn_depend_func         like depend_func.cdn_depend_func
    field num_digito_verfdor_func like funcionario.num_digito_verfdor_func
    field cdn_beneficio           like beneficio.cdn_beneficio
    FIELD cdn_regra_benefic       AS INT FORMAT "999"
    field num_mes_folha           like param_empres_rh.num_mes_refer_calc_efetd 
    field num_ano_folha           like param_empres_rh.num_ano_refer_calc_efetd
    field valor                   as dec  format "zz,zzz,zz9.99"
    field quant                   as dec  format "zzz,zz9.999"
    field parcelas                as int  format "99"
    field conteudo                as char format "x(15)".

def temp-table tt-row-mov
  field row-movto as row
  index chave is primary unique
       row-movto.
       
/* Transfer Definitions */
define buffer b_movto_benefic for movto_benefic.

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

DEFINE VARIABLE i-formula AS INTEGER     NO-UNDO.
def var c-proc as char no-undo.
def var v_log_erros as logical no-undo.
def var v_data as date no-undo.
def var v_log_folha_educnal as logical no-undo.
def var v_log_permis as logical no-undo.
def var v-mes-adm                      as   integer                                           no-undo.
def var v-ano-adm                      as   integer                                           no-undo.
def var v-dat-adm                      as   date      format "99/99/9999"                     no-undo.
def var i-dat-inic-mes as date format "99/99/9999" no-undo.
def var i-dat-term-mes as date format "99/99/9999" no-undo.
def var i-mes          as int  format "99"         no-undo.
def var i-ano          as int  format "9999"       no-undo.
def var v_achou as logical no-undo.
def var v_num_lote as integer no-undo.
def var v_row_movto_benefic as row no-undo.
def var v_des_mensagem                       as   char     format "x(40)"                     no-undo.
def var v_cod_matr                           as   char     format "x(08)"                     no-undo.
form
   erros.num_reg  
   erros.cdn_empresa
   erros.cdn_estab
   v_cod_matr
   erros.num_digito_verfdor_func
   erros.cdn_depend_func
   erros.cdn_beneficio
   erros.cdn_regra_benefic COLUMN-LABEL "   "
   erros.num_mes_folha
   erros.num_ano_folha
   erros.valor 
   erros.quant
   erros.parcelas
   erros.conteudo
   erros.des_erro
   with stream-io no-attr-space no-box width 132 57 down frame f-dados.
 

DEFINE VARIABLE dt-aux AS DATE        NO-UNDO.
def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-arq-term         as char    no-undo.

{include/i-imdef.i}

   /* variaveis auxiliares*/

DEF VAR c-arq             AS CHAR                NO-UNDO.
DEF VAR c-arquivo  AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR c-arquivo_1 AS CHAR FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE I-REGISTROS AS INTEGER     NO-UNDO.

DEFINE VARIABLE d-val-consultas-func  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-val-consultas-exced   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-val-exames-func     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-val-exames-emp      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-val-hospitalar-func AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-val-hospitalar-emp  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-val-consultas-total AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-val-exames-total    AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-val-hospitalar-total AS DECIMAL     NO-UNDO.

DEFINE VARIABLE i-qt-consultas-func  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE i-qt-exames-func     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE i-qt-hospitalar-func  AS DECIMAL     NO-UNDO.


DEFINE VARIABLE i-empresa-ini   AS char     NO-UNDO.  /*solic-318*/ 
DEFINE VARIABLE i-empresa-fim   AS char     NO-UNDO.  /*solic-318*/ 
DEFINE VARIABLE i-estab-ini     AS char    NO-UNDO.  /*solic-318*/ 
DEFINE VARIABLE i-estab-fim     AS char    NO-UNDO.  /*solic-318*/ 
DEFINE VARIABLE dt-admissao-ini AS DATE   INITIAL 01/01/2011     NO-UNDO.
DEFINE VARIABLE dt-admissao-fim AS DATE   INITIAL 05/31/2099     NO-UNDO.
DEFINE VARIABLE i-cdn_funcionario-ini AS INTEGER   INITIAL 0  NO-UNDO.
DEFINE VARIABLE i-cdn_funcionario-fim AS INTEGER   INITIAL 99999999  NO-UNDO.
DEFINE VARIABLE dt-referencia-ini AS DATE  INITIAL 06/01/2015      NO-UNDO.
DEFINE VARIABLE dt-referencia-fim AS DATE  INITIAL 06/30/2015      NO-UNDO.
DEFINE VARIABLE c-tipo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-tipo-DP AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-dep-vai AS LOGICAL     NO-UNDO.
DEFINE VARIABLE l-regera AS LOGICAL     NO-UNDO.
DEFINE VARIABLE c-tipo-tit AS CHARACTER   NO-UNDO.
DEFINE VARIABLE DT-REF AS DATE        NO-UNDO.
def var h-acomp              as handle no-undo.
def var dt-utilizacao as date no-undo.

/* define layout*/

DEFINE VARIABLE constante      AS CHARACTER format "x(1)"        INITIAL ""         NO-UNDO.
DEFINE VARIABLE constante-2    AS CHARACTER format "x(8)"        INITIAL "mvtobene" NO-UNDO.
DEFINE VARIABLE nr-reg         AS INTEGER   FORMAT 99999                            NO-UNDO.   
DEFINE VARIABLE empresa        AS CHAR      FORMAT "x(03)"                          NO-UNDO. 
DEFINE VARIABLE estabelec      AS CHAR      FORMAT "x(03)"                          NO-UNDO.  
DEFINE VARIABLE matricula      AS INTEGER   FORMAT 99999999                         NO-UNDO.             
DEFINE VARIABLE branco         AS INTEGER   FORMAT 9             INITIAL 0          NO-UNDO.            
DEFINE VARIABLE dependente     AS INTEGER   FORMAT 999                              NO-UNDO.           
DEFINE VARIABLE beneficio      AS INTEGER   FORMAT 999                              NO-UNDO.           
DEFINE VARIABLE regr-benef     AS INTEGER   FORMAT 999                              NO-UNDO.            
DEFINE VARIABLE mes-refer      AS INTEGER   FORMAT 99                               NO-UNDO.
DEFINE VARIABLE ano-refer      AS INTEGER   FORMAT 9999                             NO-UNDO.            
DEFINE VARIABLE valor          AS DECIMAL   FORMAT 99999999999                      NO-UNDO.          
DEFINE VARIABLE quantidade     AS DECIMAL   FORMAT 999999                           NO-UNDO.            
DEFINE VARIABLE parcelas       AS INTEGER   FORMAT 99                               NO-UNDO.            
DEFINE VARIABLE formula        AS INTEGER   FORMAT 9999                             NO-UNDO.            
DEFINE VARIABLE prest-serv     AS INTEGER   FORMAT 999999                           NO-UNDO.            
DEFINE VARIABLE dia-ocorr      AS INTEGER   FORMAT 99                               NO-UNDO.           
DEFINE VARIABLE mes-ocorr      AS INTEGER   FORMAT 99                               NO-UNDO.            
DEFINE VARIABLE ano-ocorr      AS INTEGER   FORMAT 9999                             NO-UNDO.            
DEFINE VARIABLE ocorr          AS INTEGER   FORMAT 99999999                         NO-UNDO.            
DEFINE VARIABLE documento      AS INTEGER   FORMAT 999999999999999                  NO-UNDO.            
DEFINE VARIABLE mot-lot-benef  AS INTEGER   FORMAT 999                              NO-UNDO.           
DEFINE VARIABLE dia-pag-efet   AS INTEGER   FORMAT 99                               NO-UNDO.          
DEFINE VARIABLE mes-pag-efet   AS INTEGER   FORMAT 99                               NO-UNDO.  
DEFINE VARIABLE ano-pag-efet   AS INTEGER   FORMAT 9999                             NO-UNDO.  
DEFINE VARIABLE per-ini        AS INTEGER   FORMAT 99999999                         NO-UNDO.  
DEFINE VARIABLE c-per-ini      AS char                           NO-UNDO.  
DEFINE VARIABLE c-per-fim      AS char                           NO-UNDO.  


DEFINE VARIABLE per-fim        AS INTEGER   FORMAT 99999999                         NO-UNDO.  
DEFINE VARIABLE constante-3    AS CHARACTER format "x(1)"        INITIAL ""         NO-UNDO. 
DEFINE VARIABLE h-campo AS HANDLE      NO-UNDO.



DEF STREAM s-saida.

Def Var c-campo     As Char.
Def Var c-byte      As Char.
Def Var c-byte1     As Char Extent 10 Form "X(70)".
Def Var idx         As Int.
Def Var idx1        As Int.
Def Var achou       As Log.
Def Var c-acento    As Char Form "x(01)" Extent 23.
Def Var c-acento1   As Char Form "x(01)" Extent 23.
def var nr-seq as integer.


/* defini��es */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-impor
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-import

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS im-pg-par bt-executar bt-cancelar bt-ajuda 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE IMAGE im-pg-lay
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-log
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

DEFINE BUTTON bt-editar 
     LABEL "Editar Layout" 
     SIZE 20 BY 1.
     
     
DEFINE VARIABLE l-gera-arq AS LOGICAL INITIAL no 
     LABEL "Gera arquivos de lotes para importa��o:" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY .83 NO-UNDO.



DEFINE VARIABLE ed-layout AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 76 BY 9.25
     FONT 2 NO-UNDO.

DEFINE BUTTON bt-arquivo-destino 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr-destino 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo-destino AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-destino-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Imprime" 
      VIEW-AS TEXT 
     SIZE 9 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu��o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

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

DEFINE VARIABLE rs-todos AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todos", 1,
"Rejeitados", 2
     SIZE 34 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 1.71.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 1.71.

DEFINE BUTTON bt-arquivo-entrada 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-arquivo-saida 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo-entrada AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 53.72 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE c-arquivo-saida AS CHARACTER INITIAL "v:/temp/mov-UNIMED.lst" 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 53.72 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE dt-refer-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE dt-refer-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt. Refer�ncia" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE i-ano-ref AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Ano Folha" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 TOOLTIP "Ano Compet�ncia Folha" NO-UNDO.

DEFINE VARIABLE i-cod-empresa-fim AS CHAR FORMAT "x(03)":U   /*solic-318*/ 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-empresa-ini AS CHAR FORMAT "x(03)":U   /*solic-318*/ 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-estab-fim AS CHAR FORMAT "x(05)":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-estab-ini AS CHAR FORMAT "x(05)":U  /*solic-318*/ 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-func-fim AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 9999999 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-func-ini AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     LABEL "Funcion�rio" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE i-mes-ref AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "M�s Folha" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "M�s Compet�ncia Folha" NO-UNDO.

DEFINE VARIABLE text-entrada AS CHARACTER FORMAT "X(256)":U INITIAL "Arquivo de Entrada" 
      VIEW-AS TEXT 
     SIZE 19 BY .63 NO-UNDO.

DEFINE VARIABLE text-entrada-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Arquivo de sa�da" 
      VIEW-AS TEXT 
     SIZE 19 BY .63 NO-UNDO.

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

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 2.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-lay
     ed-layout AT ROW 1 COL 1 NO-LABEL
     bt-editar AT ROW 10.38 COL 1 HELP
          "Dispara a Impress�o do Layout"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.46.

DEFINE FRAME f-pg-log
     rs-todos AT ROW 2.25 COL 3.29 NO-LABEL
     rs-destino AT ROW 4.5 COL 3.29 HELP
          "Destino de Impress�o do Relat�rio" NO-LABEL
     bt-config-impr-destino AT ROW 5.71 COL 43.29 HELP
          "Configura��o da impressora"
     bt-arquivo-destino AT ROW 5.71 COL 43.29 HELP
          "Escolha do nome do arquivo"
     c-arquivo-destino AT ROW 5.75 COL 3.29 HELP
          "Nome do arquivo de destino do relat�rio" NO-LABEL
     rs-execucao AT ROW 7.88 COL 3.14 HELP
          "Modo de Execu��o" NO-LABEL
     text-destino-2 AT ROW 1.46 COL 4 NO-LABEL
     text-destino AT ROW 3.75 COL 3.86 NO-LABEL
     text-modo AT ROW 7.13 COL 1.14 COLON-ALIGNED NO-LABEL
     RECT-11 AT ROW 1.75 COL 2
     RECT-7 AT ROW 4.04 COL 2
     RECT-9 AT ROW 7.42 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.29 BY 10.46.

DEFINE FRAME f-import
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu��o do relat�rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Cancelar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     im-pg-lay AT ROW 1.5 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
    
     rt-folder-top AT ROW 2.54 COL 2.14
     im-pg-log AT ROW 1.5 COL  2.14 /*49.29*/
     RECT-1 AT ROW 14.29 COL 2
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     im-pg-par AT ROW 1.5 COL 2.14 /* 33.57 */
     im-pg-sel AT ROW 1.5 COL   2.14 /* 17.86*/
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-sel
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62.

DEFINE FRAME f-pg-par
     i-cod-empresa-ini AT ROW 1.63 COL 18.43 COLON-ALIGNED
     i-cod-empresa-fim AT ROW 1.63 COL 40.72 COLON-ALIGNED NO-LABEL
     i-cod-estab-ini AT ROW 2.71 COL 18.43 COLON-ALIGNED
     i-cod-estab-fim AT ROW 2.71 COL 40.72 COLON-ALIGNED NO-LABEL
     dt-refer-ini AT ROW 3.79 COL 18.43 COLON-ALIGNED
     dt-refer-fim AT ROW 3.79 COL 40.72 COLON-ALIGNED NO-LABEL
     i-cod-func-ini AT ROW 4.83 COL 18.43 COLON-ALIGNED WIDGET-ID 4
     i-cod-func-fim AT ROW 4.83 COL 40.72 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     i-mes-ref AT ROW 5.88 COL 18.57 COLON-ALIGNED WIDGET-ID 14
     i-ano-ref AT ROW 5.92 COL 48.43 COLON-ALIGNED WIDGET-ID 12
     
     l-gera-arq AT ROW 6.92 COL 18.57 COLON-ALIGNED WIDGET-ID 22

     c-arquivo-entrada AT ROW 7.71 COL 3.29 HELP
          "Nome do arquivo de destino do relat�rio" NO-LABEL
     bt-arquivo-entrada AT ROW 7.71 COL 57.14 HELP
          "Escolha do nome do arquivo"
     c-arquivo-saida AT ROW 9.88 COL 3.29 HELP
          "Nome do arquivo de destino para exporta��o" NO-LABEL
     bt-arquivo-saida AT ROW 9.88 COL 57.14 HELP
          "Escolha do nome do arquivo" WIDGET-ID 10
     text-entrada AT ROW 6.67 COL 4.14 NO-LABEL
     text-entrada-2 AT ROW 8.83 COL 4.14 NO-LABEL
     IMAGE-1 AT ROW 1.63 COL 35.29
     IMAGE-2 AT ROW 1.63 COL 39.86
     IMAGE-3 AT ROW 2.71 COL 35.29
     IMAGE-4 AT ROW 2.71 COL 39.86
     IMAGE-5 AT ROW 3.79 COL 35.29
     IMAGE-6 AT ROW 3.79 COL 39.86
     RECT-10 AT ROW 9.17 COL 2
     RECT-8 AT ROW 7 COL 2
     IMAGE-7 AT ROW 4.83 COL 35.29 WIDGET-ID 6
     IMAGE-8 AT ROW 4.83 COL 39.86 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-impor
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Integra��o de Movimentos Unimed"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 29
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-impor.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-import
   FRAME-NAME                                                           */
/* SETTINGS FOR IMAGE im-pg-lay IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE im-pg-log IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE im-pg-sel IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-lay
                                                                        */
ASSIGN 
       ed-layout:RETURN-INSERTED IN FRAME f-pg-lay  = TRUE
       ed-layout:READ-ONLY IN FRAME f-pg-lay        = TRUE.

/* SETTINGS FOR FRAME f-pg-log
                                                                        */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-log
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-log     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-destino-2 IN FRAME f-pg-log
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino-2:PRIVATE-DATA IN FRAME f-pg-log     = 
                "Imprime".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-log
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-log     = 
                "Execu��o".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR BUTTON bt-arquivo-entrada IN FRAME f-pg-par
   NO-ENABLE                                                            */
ASSIGN 
       bt-arquivo-entrada:HIDDEN IN FRAME f-pg-par           = TRUE.

/* SETTINGS FOR EDITOR c-arquivo-entrada IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       c-arquivo-entrada:HIDDEN IN FRAME f-pg-par           = TRUE.

/* SETTINGS FOR FILL-IN dt-refer-fim IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dt-refer-ini IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-8 IN FRAME f-pg-par
   NO-ENABLE                                                            */
ASSIGN 
       RECT-8:HIDDEN IN FRAME f-pg-par           = TRUE.

/* SETTINGS FOR FILL-IN text-entrada IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-entrada:HIDDEN IN FRAME f-pg-par           = TRUE
       text-entrada:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Arquivo de Entrada".

/* SETTINGS FOR FILL-IN text-entrada-2 IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-entrada-2:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Arquivo de Sa�da".

/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-log
/* Query rebuild information for FRAME f-pg-log
     _Query            is NOT OPENED
*/  /* FRAME f-pg-log */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Integra��o de Movimentos Unimed */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Integra��o de Movimentos Unimed */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda C-Win
ON CHOOSE OF bt-ajuda IN FRAME f-import /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-log
&Scoped-define SELF-NAME bt-arquivo-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-destino C-Win
ON CHOOSE OF bt-arquivo-destino IN FRAME f-pg-log
DO:
    {include/i-imarq.i c-arquivo-destino f-pg-log}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME bt-arquivo-entrada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-entrada C-Win
ON CHOOSE OF bt-arquivo-entrada IN FRAME f-pg-par
DO:
    {include/i-imarq.i c-arquivo-entrada f-pg-par "'*.XLS' '*.xls' , 'todos' '*.*'"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-arquivo-saida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-saida C-Win
ON CHOOSE OF bt-arquivo-saida IN FRAME f-pg-par
DO:
    {include/i-imarq.i c-arquivo-saida f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-import
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar C-Win
ON CHOOSE OF bt-cancelar IN FRAME f-import /* Cancelar */
DO:
   apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-log
&Scoped-define SELF-NAME bt-config-impr-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr-destino C-Win
ON CHOOSE OF bt-config-impr-destino IN FRAME f-pg-log
DO:
   {include/i-imimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-lay
&Scoped-define SELF-NAME bt-editar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-editar C-Win
ON CHOOSE OF bt-editar IN FRAME f-pg-lay /* Editar Layout */
DO:
   {include/i-imedl.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-import
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar C-Win
ON CHOOSE OF bt-executar IN FRAME f-import /* Executar */
DO:
   do  on error undo, return no-apply:


    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = 1 /*input frame f-pg-log rs-destino*/
           tt-param.todos           = input frame f-pg-log rs-todos
           tt-param.arq-entrada     = input frame f-pg-par c-arquivo-entrada
           tt-param.data-exec       = today
           tt-param.hora-exec       = time.

     
        assign tt-param.arq-destino = input frame f-pg-par c-arquivo-saida.
       ASSIGN 
           i-empresa-ini = (i-cod-empresa-ini:SCREEN-VALUE IN FRAME f-pg-par)
           i-empresa-fim = (i-cod-empresa-fim:SCREEN-VALUE IN FRAME f-pg-par).

         FOR EACH empresa WHERE 
          empresa.ep-codigo >= i-empresa-ini AND 
          empresa.ep-codigo <= i-empresa-fim NO-LOCK:
          
          
               run pi-executar.
               OUTPUT STREAM s-saida Close. 

         END.

         run utp/ut-msgs.p (input "show",
                                   input 17006,
                                   input "Foram gerados arquivos para cada empresa.~~ " + tt-param.arq-destino + chr(10) + "Erros gravados em: " + "v:\temp\esbs0011-erros.txt").

   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME i-ano-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-ano-ref C-Win
ON LEAVE OF i-ano-ref IN FRAME f-pg-par /* Ano Folha */
DO:
  APPLY "LEAVE" TO i-mes-ref IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-mes-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-mes-ref C-Win
ON LEAVE OF i-mes-ref IN FRAME f-pg-par /* M�s Folha */
DO:
  IF int(i-mes-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME}) < 1 OR 
      int(i-mes-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME}) > 12 THEN
      RETURN NO-APPLY.  
  ASSIGN
      dt-aux =  DATE(int(i-mes-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME}),01,int(i-ano-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME}))
      dt-refer-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(dt-aux,"99/99/9999")
      dt-aux = dt-aux + 34
      dt-aux = DATE(MONTH(dt-aux),01,YEAR(dt-aux)) - 1 
      dt-refer-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(dt-aux,"99/99/9999").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-import
&Scoped-define SELF-NAME im-pg-lay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-lay C-Win
ON MOUSE-SELECT-CLICK OF im-pg-lay IN FRAME f-import
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-log C-Win
ON MOUSE-SELECT-CLICK OF im-pg-log IN FRAME f-import
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par C-Win
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-import
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel C-Win
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-import
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-log
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino C-Win
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-log
DO:
do  with frame f-pg-log:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo-destino:sensitive     = no
                   bt-arquivo-destino:visible      = no
                   bt-config-impr-destino:visible  = yes.
        end.
        when "2" then do:
            assign c-arquivo-destino:sensitive     = yes
                   bt-arquivo-destino:visible      = yes
                   bt-config-impr-destino:visible  = no.
        end.
        when "3" then do:
            assign c-arquivo-destino:sensitive     = no
                   bt-arquivo-destino:visible      = no
                   bt-config-impr-destino:visible  = no.
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME l-gera-arq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-gera-arq C-Win
ON VALUE-CHANGED OF l-gera-arq IN FRAME f-pg-par
DO:
do  with frame f-pg-par:
    if self:checked 
        then do:
            assign
               c-arquivo-saida:sensitive     = yes 
               bt-arquivo-saida:sensitive     = yes.
        end.
        else do:
             assign
               c-arquivo-saida:sensitive     = no 
               bt-arquivo-saida:sensitive     = no.

        end.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-execucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao C-Win
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-log
DO:
   {include/i-imrse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-import
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "esbs0011" "1.00.00.000"}

/*:T inicializa��es do template de importa��o */
{include/i-imini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-imlbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    i-cod-empresa-ini = STRING({cdp\poloestab.i 420}). /*solic-318*/ 
    i-cod-empresa-fim = STRING({cdp\poloestab.i 420}). /*solic-318*/ 
    i-empresa-ini = STRING({cdp\poloestab.i 420}). /*solic-318*/ 
    i-empresa-fim = STRING({cdp\poloestab.i 420}). /*solic-318*/ 
    i-estab-ini   = STRING({cdp\poloestab.i 422}). /*solic-318*/ 
    i-estab-fim   = STRING({cdp\poloestab.i 422}). /*solic-318*/
    i-cod-estab-ini = STRING({cdp\poloestab.i 421}). /*solic-318*/  
    i-cod-estab-fim = STRING({cdp\poloestab.i 423}). /*solic-318*/  

    RUN enable_UI.
  
    /*DT-REF = DATE(MONTH(TODAY),1,YEAR(TODAY)).

    dt-refer-ini:SCREEN-VALUE IN FRAME F-PG-par = STRING(DT-REF,"99/99/9999").

     DT-REF = DT-REF + 32.
       DT-REF = DATE(MONTH(DT-REF),1,YEAR(DT-REF)) - 1.

    dt-refer-FIM:SCREEN-VALUE IN FRAME F-PG-par = STRING(DT-REF,"99/99/9999").*/

    FIND FIRST param_empres_rh WHERE param_empres_rh.cdn_empresa = v_cdn_empres_usuar NO-LOCK NO-ERROR.

           IF AVAIL param_empres_rh THEN
               ASSIGN
                  i-mes-ref:SCREEN-VALUE IN FRAME f-pg-par = string(param_empres_rh.num_mes_refer_calc_efetd)
                  i-ano-ref:SCREEN-VALUE IN FRAME f-pg-par = string(param_empres_rh.num_ano_refer_calc_efetd)
                  dt-aux = DATE(param_empres_rh.num_mes_refer_calc_efetd,01,param_empres_rh.num_ano_refer_calc_efetd)                    
                  dt-refer-ini:SCREEN-VALUE IN FRAME f-pg-par = string(dt-aux,"99/99/9999")            
                  dt-aux = dt-aux + 34
                  dt-aux = DATE(MONTH(dt-aux),01,YEAR(dt-aux)) - 1 
                  dt-refer-fim:SCREEN-VALUE IN FRAME f-pg-par = string(dt-aux,"99/99/9999").

            ELSE
                ASSIGN 
                  dt-aux = TODAY
                  dt-aux = DATE(MONTH(dt-aux),01,YEAR(dt-aux)) - 1 
                  dt-aux = DATE(MONTH(dt-aux),01,YEAR(dt-aux)) 
                  dt-refer-ini:SCREEN-VALUE IN FRAME f-pg-par = string(dt-aux,"99/99/9999")           
                  dt-aux = dt-aux + 34
                  dt-aux = DATE(MONTH(dt-aux),01,YEAR(dt-aux)) - 1 
                  dt-refer-fim:SCREEN-VALUE IN FRAME f-pg-par = string(dt-aux,"99/99/9999")
                  i-mes-ref:SCREEN-VALUE IN FRAME f-pg-par = string(MONTH(dt-aux),"99")
                  i-ano-ref:SCREEN-VALUE IN FRAME f-pg-par = string(YEAR(dt-aux),"9999").

     



      h-campo = im-pg-lay:HANDLE IN FRAME f-import .
 
        DO WHILE valid-handle(h-campo:PREV-SIBLING):
          h-campo = h-campo:PREV-SIBLING.
        END.
        DO WHILE valid-handle(h-campo ):
              
          IF h-campo:TYPE = "TEXT" THEN 
              if  h-campo:SCREEN-VALUE <> "Par�metros" THEN
              
               h-campo:VISIBLE = NO.
              ELSE
                   h-campo:SCREEN-VALUE = "Sele��o".
        
          IF h-campo:name = "im-pg-lay" THEN
              h-campo:VISIBLE = no.
          IF h-campo:name = "im-pg-log" THEN
              h-campo:VISIBLE = no.
          IF h-campo:name = "im-pg-sel" THEN
              h-campo:VISIBLE = no.
          
          h-campo = h-campo:next-SIBLING.
        END.
         

    {include/i-immbl.i}


        
        APPLY 'mouse-select-click' to im-pg-par IN FRAME {&FRAME-NAME}.
    /*{include/i-imvrf.i &programa=esbs0011 &versao-layout=001}*/
  
    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects C-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available C-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
    DISPLAY rs-todos rs-destino c-arquivo-destino rs-execucao 
     WITH FRAME f-pg-log IN WINDOW C-Win.
 ENABLE RECT-11 RECT-7 RECT-9 rs-todos rs-destino bt-config-impr-destino 
        bt-arquivo-destino c-arquivo-destino rs-execucao 
     WITH FRAME f-pg-log IN WINDOW C-Win.
 {&OPEN-BROWSERS-IN-QUERY-f-pg-log}

    ENABLE im-pg-par bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-import IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-import}
  VIEW FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY ed-layout 
      WITH FRAME f-pg-lay IN WINDOW C-Win.
  ENABLE ed-layout bt-editar 
      WITH FRAME f-pg-lay IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-lay}
   DISPLAY i-cod-empresa-ini i-cod-empresa-fim i-cod-estab-ini i-cod-estab-fim 
          dt-refer-ini dt-refer-fim i-cod-func-ini i-cod-func-fim i-mes-ref 
          i-ano-ref l-gera-arq c-arquivo-saida 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 RECT-10 IMAGE-7 
         IMAGE-8 i-cod-empresa-ini i-cod-empresa-fim i-cod-estab-ini 
         i-cod-estab-fim i-cod-func-ini i-cod-func-fim i-mes-ref i-ano-ref l-gera-arq 
         /*c-arquivo-saida bt-arquivo-saida */
      WITH FRAME f-pg-par IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable C-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .
APPLY 'mouse-select-click' to im-pg-par IN FRAME {&FRAME-NAME}.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit C-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar C-Win 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE c-arq-emp AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-pos-arq AS INTEGER     NO-UNDO.
do  on error undo, return error
    on stop  undo, return error:     

   /* {include/i-rpexa.i}
     */
    if  input frame f-pg-log rs-destino = 2 and
        input frame f-pg-log rs-execucao = 1 then do:
        run utp/ut-vlarq.p (input input frame f-pg-log c-arquivo-destino).
        if  return-value = "nok":U then do:
            run utp/ut-msgs.p (input "show":U,
                               input 73,
                               input "").
            apply 'mouse-select-click':U to im-pg-log in frame f-import.
            apply 'entry':U to c-arquivo-destino in frame f-pg-log.                   
            return error.
        end.
    end.

    run utp/ut-vlarq.p (input input frame f-pg-par c-arquivo-saida).
        if  return-value = "nok":U then do:
            run utp/ut-msgs.p (input "show":U,
                               input 73,
                               input "").
            apply 'mouse-select-click':U to im-pg-par in frame f-import.
            apply 'entry':U to c-arquivo-saida in frame f-pg-par.                   
            return error.
        end.    
    
   /* assign file-info:file-name = input frame f-pg-par c-arquivo-entrada.
    if file-info:pathname = ? and
       input frame f-pg-log rs-execucao = 1 then do:
        run utp/ut-msgs.p (input "show":U,
                           input 326,
                           input c-arquivo-entrada).                               
        apply 'mouse-select-click':U to im-pg-par in frame f-import.
        apply 'entry':U to c-arquivo-entrada in frame f-pg-par.                
        return error.
    end. 
     */
            
    /*:T Coloque aqui as valida��es das outras p�ginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na p�gina 
       com problemas e colocar o focus no campo com problemas             */    
         
    OUTPUT STREAM s-saida Close. 
 

    /*:T Coloque aqui a l�gica de grava��o dos par�mtros e sele��o na temp-table
       tt-param */ 

/*    {include/i-imexb.i}
  */
    if  session:set-wait-state("general":U) then.



    Assign c-acento1[1] = "a"
           c-acento1[2] = "e"
           c-acento1[3] = "i"
           c-acento1[4] = "o"
           c-acento1[5] = "u"
           c-acento1[6] = "a"
           c-acento1[7] = "e"
           c-acento1[8] = "i"
           c-acento1[9] = "o"
           c-acento1[10] = "o"
           c-acento1[11] = "a"
           c-acento1[12] = "e"
           c-acento1[13] = "i"
           c-acento1[14] = "o"
           c-acento1[15] = "u"
           c-acento1[16] = "a"
           c-acento1[17] = "o"
           c-acento1[18] = "a"
           c-acento1[19] = "e"
           c-acento1[20] = "i"
           c-acento1[21] = "o"
           c-acento1[22] = "u"
           c-acento1[23] = "c".

    Assign c-acento[1] = "�"
           c-acento[2] = "�"
           c-acento[3] = "�"
           c-acento[4] = "�"
           c-acento[5] = "�"
           c-acento[6] = "�"
           c-acento[7] = "�"
           c-acento[8] = "�"
           c-acento[9] = "�"
           c-acento[10] = "�"
           c-acento[11] = "�"
           c-acento[12] = "�"
           c-acento[13] = "�"
           c-acento[14] = "�"
           c-acento[15] = "�"
           c-acento[16] = "�"
           c-acento[17] = "�"
           c-acento[18] = "�"
           c-acento[19] = "�"
           c-acento[20] = "�"
           c-acento[21] = "�"
           c-acento[22] = "�"
           c-acento[23] = "�".

    ASSIGN c-arq            = SESSION:TEMP-DIRECTORY
           c-arquivo = "esbs0011" + STRING(TIME) + ".txt"
           c-arquivo_1  = c-arq  + c-arquivo.

    run utp/ut-acomp.p persistent set h-acomp.
    {utp/ut-liter.i Exportando *}

    run pi-inicializar in h-acomp (input RETURN-VALUE).

    empty temp-table tt-row.
    empty temp-table tt-row-mov.

    empty temp-table tt-refer_movto_benefic.
    empty temp-table tt-movto_benefic.    
    nr-seq = 0.

     OUTPUT STREAM s-saida TO VALUE(c-arquivo_1) NO-CONVERT.
    
     ASSIGN 
         i-empresa-ini = (i-cod-empresa-ini:SCREEN-VALUE IN FRAME f-pg-par)
         i-empresa-fim = (i-cod-empresa-fim:SCREEN-VALUE IN FRAME f-pg-par)
         i-estab-ini   = (i-cod-estab-ini:SCREEN-VALUE IN FRAME f-pg-par)
         i-estab-fim   = (i-cod-estab-fim:SCREEN-VALUE IN FRAME f-pg-par)
         dt-referencia-ini = date(dt-refer-ini:SCREEN-VALUE IN FRAME f-pg-par)
         dt-referencia-fim = DATE(dt-refer-fim:SCREEN-VALUE IN FRAME f-pg-par)
         i-cdn_funcionario-ini = INT(i-cod-func-ini:SCREEN-VALUE IN FRAME f-pg-par)
         i-cdn_funcionario-fim = INT(i-cod-func-fim:SCREEN-VALUE IN FRAME f-pg-par).

     ASSIGN I-REGISTROS =  0
            nr-reg = 0
            v_num_lote = 0.



   empty temp-table tt-beneficio.

   FOR EACH beneficio NO-LOCK:
        IF beneficio.idi_param_benefic = 1 THEN DO: /*beneficio por estabelecimento*/
            CREATE tt-beneficio.
            ASSIGN tt-beneficio.cdn_beneficio     = beneficio.cdn_beneficio
                   tt-beneficio.des_beneficio     = beneficio.des_beneficio
                   tt-beneficio.cdn_regra_benefic = 0
                   tt-beneficio.des_regra_benefic = ""
                   tt-beneficio.idi_tip_benefic   = beneficio.idi_tip_benefic.
        END.
        ELSE DO:
            FOR EACH regra_benefic NO-LOCK WHERE
                regra_benefic.cdn_beneficio = beneficio.cdn_beneficio:
                CREATE tt-beneficio.
                ASSIGN tt-beneficio.cdn_beneficio     = beneficio.cdn_beneficio
                       tt-beneficio.des_beneficio     = beneficio.des_beneficio
                       tt-beneficio.cdn_regra_benefic = regra_benefic.cdn_regra_benefic
                       tt-beneficio.des_regra_benefic = regra_benefic.des_regra_benefic
                       tt-beneficio.idi_tip_benefic   = beneficio.idi_tip_benefic.
            END.
        END.
   END.
            
   FOR EACH rh_estab WHERE 
          rh_estab.cdn_empresa  = empresa.ep-codigo AND
          rh_estab.cdn_estab    >= i-estab-ini AND 
          rh_estab.cdn_estab    <= i-estab-fim NO-LOCK,
          EACH funcionario WHERE 
          funcionario.cdn_empresa      = empresa.ep-codigo     AND
          funcionario.cdn_estab        = rh_estab.cdn_estab    AND
          funcionario.cdn_funcionario >= i-cdn_funcionario-ini AND
          funcionario.cdn_funcionario <= i-cdn_funcionario-fim  NO-LOCK,
          EACH rh_pessoa_fisic of funcionario NO-LOCK.

         IF funcionario.dat_desligto_func <> ? THEN DO:
             IF funcionario.dat_desligto_func < dt-referencia-ini THEN NEXT.
         END.

         FIND LAST sit_afast_func OF funcionario 
                     WHERE sit_afast_func.cdn_sit_afast_func = 30 NO-LOCK NO-ERROR.

         IF AVAIL sit_afast_func THEN NEXT.

         I-REGISTROS =  I-REGISTROS + 1.


         IF NOT CAN-FIND(FIRST  benefic_func OF funcionario WHERE 
              benefic_func.cdn_beneficio >= 213 AND
              benefic_func.cdn_beneficio <= 216 AND
              benefic_func.cdn_beneficio <> 215 AND
              benefic_func.dat_term_benefic >= dt-referencia-ini and
              benefic_func.dat_inic_benefic <= dt-referencia-fim and
              benefic_func.idi_sit_benefic = 1 and
              benefic_func.cdn_depend_func = 0 NO-LOCK) THEN NEXT.

         FOR EACH depend_func_unimed WHERE                       
                      depend_func_unimed.cdn_empresa     = funcionario.cdn_empresa     AND
                      depend_func_unimed.cdn_estab       = funcionario.cdn_estab        AND
                      depend_func_unimed.cdn_funcionario = funcionario.cdn_funcionario NO-LOCK
                       BREAK BY depend_func_unimed.cdn_empresa
                             BY depend_func_unimed.cdn_estab
                             BY depend_func_unimed.cdn_funcionario 
                             BY depend_func_unimed.cdn_depend_func.
              

              IF FIRST-OF(depend_func_unimed.cdn_depend_func) THEN DO:
                  FOR EACH movto_unimed WHERE                      
                      movto_unimed.cdn_empresa      = funcionario.cdn_empresa            AND
                      movto_unimed.cdn_estab        = funcionario.cdn_estab              AND
                      movto_unimed.cdn_funcionario  = funcionario.cdn_funcionario        AND
                      movto_unimed.cdn_depend_func  = depend_func_unimed.cdn_depend_func AND
                      movto_unimed.cdn_depend_func_unimed  = depend_func_unimed.cdn_depend_func_unimed AND
                      movto_unimed.vl_total  <> 0                                        and
                      movto_unimed.dt_competencia  >= dt-referencia-ini                  and
                      movto_unimed.dt_competencia  <= dt-referencia-fim                  /*AND
                      NOT (movto_unimed.cdn_depend_func = 0 AND benefic_func.cdn_depend_func <> 0) 
                      not can-find(tt-row-mov where tt-row-mov.row-movto = rowid(movto_unimed) )*/
                      EXCLUSIVE-LOCK BREAK 
                       BY movto_unimed.cdn_empresa 
                       BY movto_unimed.cdn_estab 
                       BY movto_unimed.cdn_funcionario 
                       BY movto_unimed.cdn_depend_func.


                      ASSIGN movto_unimed.vl_funcionario = 0
                             movto_unimed.vl_empresa     = 0
                             movto_unimed.vl_excedente   = 0.

                     /*  create tt-row-mov.
                       assign tt-row-mov.row-movto =  rowid(movto_unimed).
                       */
                      dt-utilizacao = movto_unimed.dt_competencia.

                      run pi-acompanhar in h-acomp(input "Estab/Func:" + string( funcionario.cdn_estab ) + "-" +
                                                    String( funcionario.cdn_funcionario,"999999") +  " Gerados:" + STRING(I-REGISTROS,"9999999")).

                          IF FIRST-OF( movto_unimed.cdn_depend_func)THEN DO:
                                     assign d-val-consultas-func  = 0
                                            d-val-consultas-exced   = 0    
                                            d-val-exames-func     = 0
                                            d-val-exames-emp      = 0
                                            d-val-hospitalar-func = 0
                                            d-val-hospitalar-emp  = 0
                                            i-qt-consultas-func   = 0
                                            i-qt-exames-func      = 0
                                            i-qt-hospitalar-func  = 0
                                            d-val-consultas-total  = 0
                                            d-val-exames-total     = 0
                                            d-val-hospitalar-total = 0.

                                     /*  if  index(c-proc,string( movto_unimed.procedimento,"99999999")) > 0 then*/
                                           RUN pi-consulta-antes-competencia.

                          END.
                          if   index(movto_unimed.tipo_servico,"consulta") > 0 THEN DO: 
                              
                              run pi-consultas.  /* verifica se tem muitas consultas no exercicio conforme regra > 5 e > 9*/

                              ASSIGN                                   
                                  movto_unimed.vl_empresa = movto_unimed.vl_total - movto_unimed.vl_funcionario
                                  d-val-consultas-func    = d-val-consultas-func  + movto_unimed.vl_funcionario
                                  d-val-consultas-exced   = d-val-consultas-exced   + movto_unimed.vl_excedente
                                  d-val-consultas-total   = d-val-consultas-total + movto_unimed.vl_total
                                  i-qt-consultas-func     = i-qt-consultas-func   + 1.                             
                          END.
                          ELSE
                          if   index(movto_unimed.tipo_servico,"exame") > 0 THEN DO: 
                              
                              /* regra de exame 15%*/
                              ASSIGN 
                                  movto_unimed.vl_funcionario = TRUNCATE(movto_unimed.vl_total * 15 / 100,2)
                                  movto_unimed.vl_empresa = movto_unimed.vl_total - movto_unimed.vl_funcionario
                                  d-val-exames-func    = d-val-exames-func  + movto_unimed.vl_funcionario
                                  d-val-exames-emp     = d-val-exames-emp   + movto_unimed.vl_empresa
                                  d-val-exames-total   = d-val-exames-total + movto_unimed.vl_total
                                  i-qt-exames-func     = i-qt-exames-func   + 1.                               

                          END.
                          ELSE DO:
                                /* hospitalar e outros procedimentos 100% para empresa 0% para funcionario*/
                              ASSIGN 
                                  movto_unimed.vl_funcionario = 0
                                  movto_unimed.vl_empresa  = movto_unimed.vl_total - movto_unimed.vl_funcionario
                                  d-val-hospitalar-func    = d-val-hospitalar-func  + movto_unimed.vl_funcionario
                                  d-val-hospitalar-emp     = d-val-hospitalar-emp   + movto_unimed.vl_empresa
                                  d-val-hospitalar-total   = d-val-hospitalar-total +  movto_unimed.vl_total
                                  i-qt-hospitalar-func     =  i-qt-hospitalar-func  + 1.
                               

                          END.



                          IF LAST-OF( movto_unimed.cdn_depend_func) THEN  DO:



                              IF d-val-consultas-total > 0  THEN DO:
                                  FIND FIRST param_benefic_estab WHERE 
                                       param_benefic_estab.cdn_beneficio = 214 AND 
                                       param_benefic_estab.cdn_empresa   = movto_unimed.cdn_empresa AND
                                       param_benefic_estab.cdn_estab     = movto_unimed.cdn_estab NO-LOCK NO-ERROR.
                                  IF NOT AVAIL param_benefic_estab THEN
                                      FIND FIRST param_benefic_estab WHERE 
                                       param_benefic_estab.cdn_beneficio = 214 AND 
                                       param_benefic_estab.cdn_empresa   = "*"   NO-LOCK NO-ERROR.

                                  i-formula = 0.
                                  IF AVAIL param_benefic_estab  THEN DO:
                                      IF depend_func_unimed.cdn_depend_func = 0 THEN
                                          i-formula = param_benefic_estab.cdn_formul_benefic_padr_func.
                                      ELSE
                                          i-formula = param_benefic_estab.cdn_formul_benefic_padr_depend.

                                  END.
                                      

                              
                                  ASSIGN   nr-reg     = nr-reg + 1
                                           empresa    = movto_unimed.cdn_empresa 
                                           estabelec  = movto_unimed.cdn_estab 
                                           matricula  = movto_unimed.cdn_funcionario 
                                           dependente = depend_func_unimed.cdn_depend_func 
                                           branco     = funcionario.num_digito_verfdor_func /* */
                                           beneficio  = 214
                                           regr-benef = 0 /*benefic_func.cdn_regra_benefic*/
                                           mes-refer  = MONTH(dt-referencia-fim ) 
                                           ano-refer  = YEAR(dt-referencia-fim )   
                                           valor      = d-val-consultas-total   * 100
                                           quantidade = i-qt-consultas-func * 1000
                                           parcelas   = 1
                                           formula    = i-formula /*int(trim(SUBSTRING(benefic_func.cod_livre_1,1,4)))*/
                                           prest-serv = 0 /*conven_benefic.cdn_prestdor_serv  */
                                           /*dia-ocorr  =  day(movto_unimed.dt_competencia )
                                           mes-ocorr  =  MONTH(movto_unimed.dt_competencia )  
                                           ano-ocorr  =  YEAR(movto_unimed.dt_competencia )  */
                                           ocorr         = 0
                                           documento     = 0 
                                           mot-lot-benef = 2
                                           dia-pag-efet  = 0
                                           mes-pag-efet  = 0
                                           ano-pag-efet  = 0
                                           per-ini       = int(string(dt-referencia-ini,"99999999"))
                                           per-fim       = int(string(dt-referencia-fim,"99999999")).
                                   
                                   RUN pi-grava-linha.
                              END.

                              IF d-val-exames-total > 0  THEN DO:
                              
                                  FIND FIRST param_benefic_estab WHERE 
                                       param_benefic_estab.cdn_beneficio = 213 AND 
                                       param_benefic_estab.cdn_empresa   = movto_unimed.cdn_empresa AND
                                       param_benefic_estab.cdn_estab     = movto_unimed.cdn_estab NO-LOCK NO-ERROR.
                                  IF NOT AVAIL param_benefic_estab THEN
                                      FIND FIRST param_benefic_estab WHERE 
                                       param_benefic_estab.cdn_beneficio = 213 AND 
                                       param_benefic_estab.cdn_empresa   = "*"   NO-LOCK NO-ERROR.

                                  i-formula = 0.
                                  IF AVAIL param_benefic_estab  THEN DO:
                                      IF depend_func_unimed.cdn_depend_func = 0 THEN
                                          i-formula = param_benefic_estab.cdn_formul_benefic_padr_func.
                                      ELSE
                                          i-formula = param_benefic_estab.cdn_formul_benefic_padr_depend.

                                  END.
                                      


                                  ASSIGN   nr-reg     = nr-reg + 1
                                           empresa    = movto_unimed.cdn_empresa 
                                           estabelec  = movto_unimed.cdn_estab 
                                           matricula  = movto_unimed.cdn_funcionario 
                                           dependente = depend_func_unimed.cdn_depend_func 
                                           branco     = funcionario.num_digito_verfdor_func /* */
                                           beneficio  = 213
                                           regr-benef = 0 /*benefic_func.cdn_regra_benefic*/
                                           mes-refer  = MONTH(dt-referencia-fim ) 
                                           ano-refer  = YEAR(dt-referencia-fim )   
                                           valor      = d-val-exames-total   * 100
                                           quantidade = i-qt-exames-func * 1000
                                           parcelas   = 1
                                           formula    = i-formula /*int(trim(SUBSTRING(benefic_func.cod_livre_1,1,4)))*/
                                           prest-serv = 0 /*conven_benefic.cdn_prestdor_serv  */
                                           /*dia-ocorr  =  day(movto_unimed.dt_competencia )
                                           mes-ocorr  =  MONTH(movto_unimed.dt_competencia )  
                                           ano-ocorr  =  YEAR(movto_unimed.dt_competencia )  */
                                           ocorr         = 0
                                           documento     = 0 
                                           mot-lot-benef = 2
                                           dia-pag-efet  = 0
                                           mes-pag-efet  = 0
                                           ano-pag-efet  = 0
                                           per-ini       = int(string(dt-referencia-ini,"99999999"))
                                           per-fim       = int(string(dt-referencia-fim,"99999999")).
                                   
                                   RUN pi-grava-linha.
                              END.

                              IF  d-val-hospitalar-total > 0  THEN DO:
                              
                                  FIND FIRST param_benefic_estab WHERE 
                                       param_benefic_estab.cdn_beneficio = 216 AND 
                                       param_benefic_estab.cdn_empresa   = movto_unimed.cdn_empresa AND
                                       param_benefic_estab.cdn_estab     = movto_unimed.cdn_estab NO-LOCK NO-ERROR.
                                  IF NOT AVAIL param_benefic_estab THEN
                                      FIND FIRST param_benefic_estab WHERE 
                                       param_benefic_estab.cdn_beneficio = 216 AND 
                                       param_benefic_estab.cdn_empresa   = "*"   NO-LOCK NO-ERROR.

                                  i-formula = 0.
                                  IF AVAIL param_benefic_estab  THEN DO:
                                      IF depend_func_unimed.cdn_depend_func = 0 THEN
                                          i-formula = param_benefic_estab.cdn_formul_benefic_padr_func.
                                      ELSE
                                          i-formula = param_benefic_estab.cdn_formul_benefic_padr_depend.

                                  END.
                                      

                                  ASSIGN   nr-reg     = nr-reg + 1
                                           empresa    = movto_unimed.cdn_empresa 
                                           estabelec  = movto_unimed.cdn_estab 
                                           matricula  = movto_unimed.cdn_funcionario 
                                           dependente = depend_func_unimed.cdn_depend_func 
                                           branco     = funcionario.num_digito_verfdor_func /* */
                                           beneficio  = 216
                                           regr-benef = 0 /*benefic_func.cdn_regra_benefic*/
                                           mes-refer  = MONTH(dt-referencia-fim ) 
                                           ano-refer  = YEAR(dt-referencia-fim )   
                                           valor      = d-val-hospitalar-total   * 100
                                           quantidade = i-qt-hospitalar-func * 1000
                                           parcelas   = 1
                                           formula    = i-formula /*int(trim(SUBSTRING(benefic_func.cod_livre_1,1,4)))*/
                                           prest-serv = 0 /*conven_benefic.cdn_prestdor_serv  */
                                           /*dia-ocorr  =  day(movto_unimed.dt_competencia )
                                           mes-ocorr  =  MONTH(movto_unimed.dt_competencia )  
                                           ano-ocorr  =  YEAR(movto_unimed.dt_competencia )  */
                                           ocorr         = 0
                                           documento     = 0 
                                           mot-lot-benef = 2
                                           dia-pag-efet  = 0
                                           mes-pag-efet  = 0
                                           ano-pag-efet  = 0
                                           per-ini       = int(string(dt-referencia-ini,"99999999"))
                                           per-fim       = int(string(dt-referencia-fim,"99999999")).
                                   
                                   RUN pi-grava-linha.
                              END. 

                              IF  d-val-consultas-exced > 0  THEN DO:
                                   
                                  FIND FIRST param_benefic_estab WHERE 
                                       param_benefic_estab.cdn_beneficio = 217 AND 
                                       param_benefic_estab.cdn_empresa   = movto_unimed.cdn_empresa AND
                                       param_benefic_estab.cdn_estab     = movto_unimed.cdn_estab NO-LOCK NO-ERROR.
                                  IF NOT AVAIL param_benefic_estab THEN
                                      FIND FIRST param_benefic_estab WHERE 
                                       param_benefic_estab.cdn_beneficio = 217 AND 
                                       param_benefic_estab.cdn_empresa   = "*"   NO-LOCK NO-ERROR.

                                  i-formula = 0.
                                  IF AVAIL param_benefic_estab  THEN DO:
                                      IF depend_func_unimed.cdn_depend_func = 0 THEN
                                          i-formula = param_benefic_estab.cdn_formul_benefic_padr_func.
                                      ELSE
                                          i-formula = param_benefic_estab.cdn_formul_benefic_padr_depend.

                                  END.
                                      
                              
                                  ASSIGN   nr-reg     = nr-reg + 1
                                           empresa    = movto_unimed.cdn_empresa 
                                           estabelec  = movto_unimed.cdn_estab 
                                           matricula  = movto_unimed.cdn_funcionario 
                                           dependente = depend_func_unimed.cdn_depend_func 
                                           branco     = funcionario.num_digito_verfdor_func /* */
                                           beneficio  = 217
                                           regr-benef = 0 /*benefic_func.cdn_regra_benefic*/
                                           mes-refer  = MONTH(dt-referencia-fim ) 
                                           ano-refer  = YEAR(dt-referencia-fim )   
                                           valor      = d-val-consultas-exced   * 100
                                           quantidade = 0 * 1000
                                           parcelas   = 1
                                           formula    = i-formula /*int(trim(SUBSTRING(benefic_func.cod_livre_1,1,4)))*/
                                           prest-serv = 0 /*conven_benefic.cdn_prestdor_serv  */
                                           /*dia-ocorr  =  day(movto_unimed.dt_competencia )
                                           mes-ocorr  =  MONTH(movto_unimed.dt_competencia )  
                                           ano-ocorr  =  YEAR(movto_unimed.dt_competencia )  */
                                           ocorr         = 0
                                           documento     = 0 
                                           mot-lot-benef = 2
                                           dia-pag-efet  = 0
                                           mes-pag-efet  = 0
                                           ano-pag-efet  = 0
                                           per-ini       = int(string(dt-referencia-ini,"99999999"))
                                           per-fim       = int(string(dt-referencia-fim,"99999999")).
                                   
                                   RUN pi-grava-linha.
                              END. 

                          END. /*LAST-OF*/
                  END. /*for each movto_unimed*/

              END. /* IF FIRST-OF(depend_func_unimed.cdn_depend_func) */

         END.  /*FOR EACH depend_func_unimed*/

      END.  /*FOR EACH rh_estab WHERE */
        
      OUTPUT STREAM s-saida Close. 

 
IF NOT l-gera-arq:checked in frame f-pg-par THEN DO:

       c-per-ini       = string(dt-referencia-ini,"99999999").
       c-per-fim       = string(dt-referencia-fim,"99999999").
                                   


for each tt-refer_movto_benefic  no-lock,
      each tt-movto_benefic of tt-refer_movto_benefic no-lock,
          first funcionario where  
                       funcionario.cdn_empresa             = tt-movto_benefic.cdn_empresa  and
                       funcionario.cdn_estab               = tt-movto_benefic.cdn_estab    and
                       funcionario.cdn_funcionario         = tt-movto_benefic.cdn_funcionario no-lock.
                       
      find empresa no-lock where
         empresa.ep-codigo = tt-movto_benefic.cdn_empresa no-error.

 
   
   find param_folha_educnal no-lock where
        param_folha_educnal.cdn_empresa = tt-movto_benefic.cdn_empresa no-error.
        
   v_log_folha_educnal = if avail param_folha_educnal then yes else no.
         
                   
            FIND FIRST param_empres_rh NO-LOCK WHERE
                       param_empres_rh.cdn_empresa = tt-movto_benefic.cdn_empresa NO-ERROR.
            IF  NOT AVAIL param_empres_rh THEN DO:
                create erros.
                {utp/ut-table.i mguni empresa 1}
                run utp/ut-msgs.p (input "msg", input 56, input "").
    
                assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                       erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                       erros.des_erro                = return-value
                       erros.log_erro                = yes
                       erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                       erros.cdn_estab               = tt-movto_benefic.cdn_estab
                       erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                       erros.num_digito_verfdor_func = 0
                       erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                       erros.cdn_beneficio           = tt-movto_benefic.cdn_beneficio
                       erros.cdn_regra_benefic       = tt-movto_benefic.cdn_regra_benefic
                       erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                       erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                       erros.valor                   = tt-movto_benefic.val_calcul_efp
                       erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                       erros.parcelas                = 1
                       erros.conteudo                = string(tt-movto_benefic.num_seq_movto_benefic)
                       v_log_erros = yes.
                next.
            END.
            
            
            FIND FIRST rh_estab NO-LOCK WHERE
                       rh_estab.cdn_empresa = tt-movto_benefic.cdn_empresa AND
                       rh_estab.cdn_estab   = tt-movto_benefic.cdn_estab NO-ERROR.
            IF  NOT AVAIL rh_estab THEN DO:
                create erros.
                {utp/ut-table.i dthrpyc rh_estab 1}
                run utp/ut-msgs.p (input "msg", input 56, input "").

                 assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                       erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                       erros.des_erro                = return-value
                       erros.log_erro                = yes
                       erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                       erros.cdn_estab               = tt-movto_benefic.cdn_estab
                       erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                       erros.num_digito_verfdor_func = 0
                       erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                       erros.cdn_beneficio           = tt-movto_benefic.cdn_beneficio
                       erros.cdn_regra_benefic       = tt-movto_benefic.cdn_regra_benefic
                       erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                       erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                       erros.valor                   = tt-movto_benefic.val_calcul_efp
                       erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                       erros.parcelas                = 1
                       erros.conteudo                = string(tt-movto_benefic.num_seq_movto_benefic)
                       v_log_erros = yes.


                next.
            END.
            
          

            if  funcionario.dat_desligto_func <> ? then do:

                find last sit_afast_func where
                          sit_afast_func.cdn_empresa     = funcionario.cdn_empresa          
                      and sit_afast_func.cdn_estab       = funcionario.cdn_estab      
                      and sit_afast_func.cdn_funcionario = funcionario.cdn_funcionario no-lock no-error.
                if avail sit_afast_func then
                    find sit_afast of sit_afast_func no-lock no-error.
                
                if avail sit_afast then
                    if sit_afast.idi_signif_sit <> 4 then do:
                        create erros.
                        run utp/ut-msgs.p (input "msg",input 4682,input return-value).
                         assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = 0
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-movto_benefic.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-movto_benefic.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-movto_benefic.num_seq_movto_benefic)
                               v_log_erros = yes.
                               
                               next.
        
                    end.
        
           end.
         
           /*    Verifica mes/ano informado com mes/ano folha    */
    find param_empres_rh no-lock where
         param_empres_rh.cdn_empresa = tt-movto_benefic.cdn_empresa no-error.
    if  avail param_empres_rh then do:
        if  param_empres_rh.num_ano_refer_calc_efetd > tt-movto_benefic.num_ano_refer_movto_benefic then do:
            
            create erros.
            run utp/ut-msgs.p (input "msg",input 8215,input return-value).
             assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                       erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                       erros.des_erro                = return-value
                       erros.log_erro                = yes
                       erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                       erros.cdn_estab               = tt-movto_benefic.cdn_estab
                       erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                       erros.num_digito_verfdor_func = 0
                       erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                       erros.cdn_beneficio           = tt-movto_benefic.cdn_beneficio
                       erros.cdn_regra_benefic       = tt-movto_benefic.cdn_regra_benefic
                       erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                       erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                       erros.valor                   = tt-movto_benefic.val_calcul_efp
                       erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                       erros.parcelas                = 1
                       erros.conteudo                = string(tt-movto_benefic.num_seq_movto_benefic)
                       v_log_erros = yes.
                       
                       next.

        end.
        if  param_empres_rh.num_ano_refer_calc_efetd = tt-movto_benefic.num_ano_refer_movto_benefic then do:
            if  tt-movto_benefic.num_mes_refer_movto_benefic = param_empres_rh.num_mes_refer_calc_efetd then do:
                if  funcionario.cdn_sit_benefic > 0 then do:
                    create erros.
                    run utp/ut-msgs.p (input "msg",input 8214,input return-value).
                     assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                       erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                       erros.des_erro                = return-value
                       erros.log_erro                = yes
                       erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                       erros.cdn_estab               = tt-movto_benefic.cdn_estab
                       erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                       erros.num_digito_verfdor_func = 0
                       erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                       erros.cdn_beneficio           = tt-movto_benefic.cdn_beneficio
                       erros.cdn_regra_benefic       = tt-movto_benefic.cdn_regra_benefic
                       erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                       erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                       erros.valor                   = tt-movto_benefic.val_calcul_efp
                       erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                       erros.parcelas                = 1
                       erros.conteudo                = string(tt-movto_benefic.num_seq_movto_benefic)
                       v_log_erros = yes.
                       
                       next.

                end.
            end.
            if  tt-movto_benefic.num_mes_refer_movto_benefic < param_empres_rh.num_mes_refer_calc_efetd then do:
                create erros.
                run utp/ut-msgs.p (input "msg",input 8215,input return-value).
                 assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                       erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                       erros.des_erro                = return-value
                       erros.log_erro                = yes
                       erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                       erros.cdn_estab               = tt-movto_benefic.cdn_estab
                       erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                       erros.num_digito_verfdor_func = 0
                       erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                       erros.cdn_beneficio           = tt-movto_benefic.cdn_beneficio
                       erros.cdn_regra_benefic       = tt-movto_benefic.cdn_regra_benefic
                       erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                       erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                       erros.valor                   = tt-movto_benefic.val_calcul_efp
                       erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                       erros.parcelas                = 1
                       erros.conteudo                = string(tt-movto_benefic.num_seq_movto_benefic)
                       v_log_erros = yes.
                       
                       next.

            end.
        end.
 
 
    
    
    
   end. /*param empresa**/
                   

    BLOCO-BENEFICIO:
    /*celeste */ 
    FOR EACH tt-beneficio WHERE
        
        tt-beneficio.cdn_beneficio = tt-movto_benefic.cdn_beneficio AND 
        tt-beneficio.cdn_regra_benefic = tt-movto_benefic.cdn_regra_benefic /*AND
        tt-beneficio.log_beneficio_utiliz*/ :
        
        
        FIND beneficio NO-LOCK WHERE
             beneficio.cdn_beneficio = tt-beneficio.cdn_beneficio NO-ERROR.
        IF beneficio.idi_param_benefic = 1 THEN DO: /*Beneficio por estabelecimento*/
             
            find sind_estab_benefic no-lock where
                 sind_estab_benefic.cdn_empresa      = tt-movto_benefic.cdn_empresa and
                 sind_estab_benefic.cdn_estab        = tt-movto_benefic.cdn_estab and
                 sind_estab_benefic.cdn_beneficio    = tt-beneficio.cdn_beneficio and
                 sind_estab_benefic.cdn_sindicato    = funcionario.cdn_sindicato no-error.
            if  not avail sind_estab_benefic then do:
                find sind_estab_benefic no-lock where
                     sind_estab_benefic.cdn_empresa      = string(tt-movto_benefic.cdn_empresa) and
                     sind_estab_benefic.cdn_estab        = "*" and
                     sind_estab_benefic.cdn_beneficio    = tt-beneficio.cdn_beneficio and
                     sind_estab_benefic.cdn_sindicato    = funcionario.cdn_sindicato no-error.
                if  not avail sind_estab_benefic then do:
                    create erros.
                    run utp/ut-msgs.p (input "msg",input 8261,input return-value).
                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                           erros.des_erro                = return-value
                           erros.log_erro                = yes
                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                           erros.num_digito_verfdor_func = int(0)
                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                           erros.parcelas                = 1
                           erros.conteudo                = string(tt-beneficio.cdn_beneficio)
                           v_log_permis   = no.
                    assign v_log_erros = yes.
                    NEXT BLOCO-BENEFICIO.
                end.
            end.
    
            find last param_benefic_estab no-lock where 
                      param_benefic_estab.cdn_empresa                     = string(tt-movto_benefic.cdn_empresa) and
                     (param_benefic_estab.cdn_estab                       = string(tt-movto_benefic.cdn_estab) or
                      param_benefic_estab.cdn_estab                       = "*") and
                      param_benefic_estab.cdn_beneficio                   = tt-beneficio.cdn_beneficio and
                      param_benefic_estab.num_seq_niv_hier_funcnal_ini   <= funcionario.num_seq_niv_hier_funcnal and
                      param_benefic_estab.num_seq_niv_hier_funcnal_final >= funcionario.num_seq_niv_hier_funcnal and
                    ((param_benefic_estab.num_ano_inic_benefic_estab     <= tt-movto_benefic.num_ano_refer_movto_benefic) or
                     (param_benefic_estab.num_mes_inic_benefic_estab     <= tt-movto_benefic.num_mes_refer_movto_benefic)) and   
                    ((param_benefic_estab.num_ano_final_benefic_estab    >= tt-movto_benefic.num_ano_refer_movto_benefic) or
                     (param_benefic_estab.num_mes_final_benefic_estab    >  tt-movto_benefic.num_mes_refer_movto_benefic)) no-error.
            if  not avail param_benefic_estab then do:
                create erros.
                {utp/ut-table.i dthrpyc param_benefic_estab 1}.
                run utp/ut-msgs.p (input "msg",input 56,input return-value).
                assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                       erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                       erros.des_erro                = return-value
                       erros.log_erro                = yes
                       erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                       erros.cdn_estab               = tt-movto_benefic.cdn_estab
                       erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                       erros.num_digito_verfdor_func = int(0)
                       erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                       erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                       erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                       erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                       erros.valor                   = tt-movto_benefic.val_calcul_efp
                       erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                       erros.parcelas                = 1
                       erros.conteudo                = string(tt-beneficio.cdn_beneficio)
                       v_log_permis   = no.
                assign v_log_erros = yes.
                NEXT BLOCO-BENEFICIO.
            end.
            else do:
                /* Jaubert pediu para comentar a Lcgica abaixo comentada para solucionar FO 1671.857 */
                /*if  param_benefic_estab.log_param_benefic_ok = no then do:
                    create erros.
                    run utp/ut-msgs.p (input "msg",input 8132,input return-value).
                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                           erros.des_erro                = return-value
                           erros.log_erro                = yes
                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                           erros.num_digito_verfdor_func = int(0)
                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                           erros.parcelas                = 1
                           erros.conteudo                = string(param_benefic_estab.log_param_benefic_ok)
                           v_log_permis   = no.
                    assign v_log_erros = yes.
                    NEXT BLOCO-BENEFICIO.
                END.*/
    
                if  1 > param_benefic_estab.qtd_max_parc_benefic then do: 
                    create erros.
                    run utp/ut-msgs.p (input "msg",input 8141,input return-value).
                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                           erros.des_erro                = return-value
                           erros.log_erro                = yes
                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                           erros.num_digito_verfdor_func = int(0)
                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                           erros.parcelas                = 1
                           erros.conteudo                = string(1)
                           v_log_permis   = no.
                    assign v_log_erros = yes.
                    NEXT BLOCO-BENEFICIO.
                END.
    
                if  param_benefic_estab.log_usa_formul_calc_benefic = yes then do:
                    if  tt-movto_benefic.cdn_formul_calc_benefic = 0 then do:
                        assign tt-movto_benefic.cdn_formul_calc_benefic = if tt-movto_benefic.cdn_depend_func = 0 
                                                   then param_benefic_estab.cdn_formul_benefic_padr_func
                                                   else param_benefic_estab.cdn_formul_benefic_padr_depend.
                    END.
                    else do:
                        IF  not can-find(first efp_benefic where
                                            efp_benefic.cdn_empresa             = string(tt-movto_benefic.cdn_empresa)    and
                                           (efp_benefic.cdn_estab               = "*"                 or
                                            efp_benefic.cdn_estab               = string(tt-movto_benefic.cdn_estab))     and
                                            efp_benefic.cdn_beneficio           = tt-beneficio.cdn_beneficio  and
                                            efp_benefic.cdn_formul_calc_benefic = tt-movto_benefic.cdn_formul_calc_benefic) then do:
                            create erros.              
                            {utp/ut-field.i dthrpyc movto_benefic cdn_formul_calc_benefic 1}
                            run utp/ut-msgs.p (input "msg",input 15451,input return-value).
                            assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                   erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                   erros.des_erro                = return-value
                                   erros.log_erro                = yes
                                   erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                   erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                   erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                   erros.num_digito_verfdor_func = int(0)
                                   erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                   erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                   erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                   erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                   erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                   erros.valor                   = tt-movto_benefic.val_calcul_efp
                                   erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                   erros.parcelas                = 1
                                   erros.conteudo                = string(tt-movto_benefic.cdn_formul_calc_benefic)
                                   v_log_permis   = no.
                            assign v_log_erros = yes.
                            NEXT BLOCO-BENEFICIO.
                        END.
                    END.
                END.
    
                IF  beneficio.log_informa_prestdor_benefic AND
                    tt-beneficio.cdn_prestdor_serv = 0 THEN DO:
                    ASSIGN tt-beneficio.cdn_prestdor_serv = param_benefic_estab.cdn_prestdor_serv_padr.
                END.
    
                if  beneficio.log_informa_prestdor_benefic and
                    tt-beneficio.cdn_prestdor_serv = 0 then do:
                    create erros.
                    run utp/ut-msgs.p (input "msg",input 8147,input return-value).
                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                           erros.des_erro                = return-value
                           erros.log_erro                = yes
                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                           erros.num_digito_verfdor_func = int(0)
                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                           erros.parcelas                = 1
                           erros.conteudo                = string(tt-beneficio.cdn_prestdor_serv)
                           v_log_permis   = no.
                    assign v_log_erros = yes.
                    NEXT BLOCO-BENEFICIO.
                END.
    
                /*    Verifica se funcion�rio est� afastado      */
                &if "{&dthrpyc_version}" >= "2.06" &then 
                     find last sit_afast_func no-lock use-index stfstfnc_term where
                               sit_afast_func.cdn_empresa           = tt-movto_benefic.cdn_empresa            and
                               sit_afast_func.cdn_estab             = tt-movto_benefic.cdn_estab              and
                               sit_afast_func.cdn_funcionario       = tt-movto_benefic.cdn_funcionario        and
                               sit_afast_func.dat_inic_sit_afast   <= dt-referencia-ini        and
                               sit_afast_func.dat_term_sit_afast   >= dt-referencia-fim       no-error.
                &else
                     find last sit_afast_func no-lock where
                               sit_afast_func.cdn_empresa           = tt-movto_benefic.cdn_empresa            and
                               sit_afast_func.cdn_estab             = tt-movto_benefic.cdn_estab              and
                               sit_afast_func.cdn_funcionario       = tt-movto_benefic.cdn_funcionario        and
                               sit_afast_func.dat_inic_sit_afast   <= dt-referencia-ini        and
                               sit_afast_func.dat_term_sit_afast   >= dt-referencia-fim       no-error.
                &endif               
        
        
                if  avail sit_afast_func then do:
                find first  sit_benefic no-lock 
                     where  sit_benefic.cdn_empresa         = sit_afast_func.cdn_empresa
                       and (sit_benefic.cdn_estab           = sit_afast_func.cdn_estab  
                        or  sit_benefic.cdn_estab           = "*")
                       and  sit_benefic.cdn_beneficio       = beneficio.cdn_beneficio  
                       and  sit_benefic.cdn_sit_afast_func  = sit_afast_func.cdn_sit_afast_func no-error.
                    if  avail sit_benefic then do:
                        if  sit_benefic.idi_tip_relac_sit_benefic = 1 then do:  
                            assign v_data = sit_afast_func.dat_inic_sit_afast + sit_benefic.qti_dias_sit_influi_fp.
        
        
                            if  v_data > sit_afast_func.dat_term_sit_afast then
                                NEXT BLOCO-BENEFICIO.
        
        
                            if  v_data <= dt-referencia-fim then do:
                                create erros.
                                {utp/ut-field.i dthrpyc movto_benefic cdn_formul_calc_benefic 1}
                                run utp/ut-msgs.p (input "msg",input 8199,input return-value).
                                if  return-value <> "yes" then do:
                                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                           erros.des_erro                = return-value
                                           erros.log_erro                = yes
                                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                           erros.num_digito_verfdor_func = int(0)
                                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                           erros.parcelas                = 1
                                           erros.conteudo                = string(tt-movto_benefic.dat_ocor_movto_benefic)
                                           v_log_permis   = no.
                                    assign v_log_erros = yes.
                                    NEXT BLOCO-BENEFICIO.
                                END.
                            END.
                        END.
                    END.
                END.
        
        
                /*************************************************/
        
        
                if  beneficio.idi_tip_benefic <> 2 then do:
                    create erros.
                    {utp/ut-field.i dthrpyc movto_benefic cdn_formul_calc_benefic 1}
                    run utp/ut-msgs.p (input "msg",input 8133,input return-value).
                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                           erros.des_erro                = return-value
                           erros.log_erro                = yes
                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                           erros.num_digito_verfdor_func = int(0)
                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                           erros.parcelas                = 1
                           erros.conteudo                = string(beneficio.idi_tip_benefic)
                           v_log_permis   = no.
                    assign v_log_erros = yes.
                    NEXT BLOCO-BENEFICIO.
                end.
                if  beneficio.log_informa_prestdor_benefic = yes then do:
                    if  tt-beneficio.cdn_prestdor_serv <= 0 then do:
                        create erros.
                        {utp/ut-field.i dthrpyc movto_benefic cdn_prestdor_serv 1}
                        run utp/ut-msgs.p (input "msg",input 7938,input return-value).
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-beneficio.cdn_prestdor_serv)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    end.
                end.
        
                /*    Se Informado    */  
        
                if  beneficio.idi_distrib_benefic  = 2 then do:
                    find benefic_func no-lock where
                         benefic_func.cdn_empresa     = tt-movto_benefic.cdn_empresa and
                         benefic_func.cdn_estab       = tt-movto_benefic.cdn_estab and
                         benefic_func.cdn_funcionario = tt-movto_benefic.cdn_funcionario and
                         benefic_func.cdn_depend_func = tt-movto_benefic.cdn_depend_func and
                         benefic_func.cdn_beneficio   = tt-beneficio.cdn_beneficio and
                         benefic_func.cdn_regra_benefic = tt-beneficio.cdn_regra_benefic no-error.
                    if  not avail benefic_func then do:
                        create erros.
                        {utp/ut-field.i dthrpyc movto_benefic cdn_depend_func 1}
                        run utp/ut-msgs.p (input "msg",input 8318,input "").
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-movto_benefic.cdn_depend_func)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    end.
        
        
                    if  (benefic_func.dat_inic_benefic - 30) > tt-movto_benefic.dat_ocor_movto_benefic or
                        benefic_func.dat_term_benefic < tt-movto_benefic.dat_ocor_movto_benefic then do:
                        create erros.
                        run utp/ut-msgs.p (input "msg",
                                           input 4978,
                                           input "").
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-beneficio.cdn_beneficio)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    end.
        
        
                    if  benefic_func.idi_sit_benefic = 2 then do:
                        create erros.
                        run utp/ut-msgs.p (input "msg",
                                           input 4502,
                                           input "").
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-beneficio.cdn_beneficio)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    end.
                end.
                /*   Se Geral        */
                else do:
                    if  v_log_folha_educnal then do:
                        create erros.
                        run utp/ut-msgs.p (input "msg", input 8377, input "").
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(funcionario.dat_admis_func)
                               v_log_permis                  = no.
                    end.
                END.
        
                /*   Verifica se quantidade meses apos admissao sAo insuficientes para concessao do beneficio */
                if  (month(funcionario.dat_admis_func) + param_benefic_estab.qtd_min_meses_admis) > 12 then do:
                    assign v-mes-adm = month(funcionario.dat_admis_func) + param_benefic_estab.qtd_min_meses_admis
                           v-ano-adm = year(funcionario.dat_admis_func).
                    repeat while v-mes-adm > 12:
                           assign v-mes-adm = v-mes-adm - 12
                                  v-ano-adm = v-ano-adm + 1.
                    end.
                    assign  v-dat-adm = date(v-mes-adm,day(funcionario.dat_admis_func),v-ano-adm).
                end.
                else assign v-dat-adm = date(month(funcionario.dat_admis_func) + integer(param_benefic_estab.qtd_min_meses_admis),day(funcionario.dat_admis_func),year(funcionario.dat_admis_func)).
                if  tt-movto_benefic.num_ano_refer_movto_benefic < year(v-dat-adm) or
                    (tt-movto_benefic.num_ano_refer_movto_benefic  = year(v-dat-adm) and
                    tt-movto_benefic.num_mes_refer_movto_benefic  < month(v-dat-adm)) then do:
                    create erros.
                    run utp/ut-msgs.p (input "msg",input 8149,input return-value).
                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                           erros.des_erro                = return-value
                           erros.log_erro                = yes
                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                           erros.num_digito_verfdor_func = int(0)
                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                           erros.parcelas                = 1
                           erros.conteudo                = string(funcionario.dat_admis_func)
                           v_log_permis   = no.
                    assign v_log_erros = yes.
                    NEXT BLOCO-BENEFICIO.
                end.
        
        
                /***********************************************************************************************/     
                if  tt-movto_benefic.cdn_depend_func > 0 then do:
                    if  beneficio.log_depend_func = NO then do:
                        create erros.
                        run utp/ut-msgs.p (input "msg",input 8134,input return-value).
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-movto_benefic.cdn_depend_func)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    end.
        
        
                    /*    Se Junto    */  /* usado no dicionario */
                    if  beneficio.idi_control_depend_benefic = 1 then do:
                        if  tt-movto_benefic.cdn_depend_func <> 999 then do:
                            create erros.
                            {utp/ut-field.i dthrpyc movto_benefic cdn_depend_func 1}.
                            run utp/ut-msgs.p (input "msg",input 8262,input return-value).
                            assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                   erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                   erros.des_erro                = return-value
                                   erros.log_erro                = yes
                                   erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                   erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                   erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                   erros.num_digito_verfdor_func = int(0)
                                   erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                   erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                   erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                   erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                   erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                   erros.valor                   = tt-movto_benefic.val_calcul_efp
                                   erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                   erros.parcelas                = 1
                                   erros.conteudo                = string(tt-movto_benefic.cdn_depend_func)
                                   v_log_permis   = no.
                            assign v_log_erros = yes.
                            NEXT BLOCO-BENEFICIO.
                        end.
                        find last depend_func no-lock where
                                 depend_func.cdn_empresa     = tt-movto_benefic.cdn_empresa and
                                 depend_func.cdn_estab       = tt-movto_benefic.cdn_estab and
                                 depend_func.cdn_funcionario = tt-movto_benefic.cdn_funcionario no-error.
                        if  not avail depend_func then do:
                            create erros.
                            {utp/ut-field.i dthrpyc movto_benefic cdn_depend_func 1}.
                            run utp/ut-msgs.p (input "msg",input 8271,input return-value).
                            assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                   erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                   erros.des_erro                = return-value
                                   erros.log_erro                = yes
                                   erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                   erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                   erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                   erros.num_digito_verfdor_func = int(0)
                                   erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                   erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                   erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                   erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                   erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                   erros.valor                   = tt-movto_benefic.val_calcul_efp
                                   erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                   erros.parcelas                = 1
                                   erros.conteudo                = string(tt-movto_benefic.cdn_funcionario)
                                   v_log_permis   = no.
                            assign v_log_erros = yes.
                            NEXT BLOCO-BENEFICIO.
                        end.
                    end.
                    /*   Se Separado   */
        
        
                    else do: 
                        find depend_func no-lock where
                             depend_func.cdn_empresa     = tt-movto_benefic.cdn_empresa and
                             depend_func.cdn_estab       = tt-movto_benefic.cdn_estab and
                             depend_func.cdn_funcionario = tt-movto_benefic.cdn_funcionario and
                             depend_func.cdn_depend_func = tt-movto_benefic.cdn_depend_func no-error.
                        if  not avail depend_func then do:
                            create erros.
                            {utp/ut-field.i dthrpyc movto_benefic cdn_depend_func 1}.
                            run utp/ut-msgs.p (input "msg",input 8270,input return-value).
                            assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                   erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                   erros.des_erro                = return-value
                                   erros.log_erro                = yes
                                   erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                   erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                   erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                   erros.num_digito_verfdor_func = int(0)
                                   erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                   erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                   erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                   erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                   erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                   erros.valor                   = tt-movto_benefic.val_calcul_efp
                                   erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                   erros.parcelas                = 1
                                   erros.conteudo                = string(tt-movto_benefic.cdn_depend_func)
                                   v_log_permis   = no.
                            assign v_log_erros = yes.
                            NEXT BLOCO-BENEFICIO.
                        end.
        
        
                        find grau_depend_estab_benefic no-lock where
                             grau_depend_estab_benefic.cdn_empresa          = tt-movto_benefic.cdn_empresa and
                             grau_depend_estab_benefic.cdn_estab            = tt-movto_benefic.cdn_estab and
                             grau_depend_estab_benefic.cdn_beneficio        = tt-beneficio.cdn_beneficio and
                             grau_depend_estab_benefic.cdn_grau_depend_func = depend_func.idi_grau_depen_func no-error.
                        if  not avail grau_depend_estab_benefic then do:
                            find grau_depend_estab_benefic no-lock where
                                 grau_depend_estab_benefic.cdn_empresa          = tt-movto_benefic.cdn_empresa and
                                 grau_depend_estab_benefic.cdn_estab            = "*" and
                                 grau_depend_estab_benefic.cdn_beneficio        = tt-beneficio.cdn_beneficio and
                                 grau_depend_estab_benefic.cdn_grau_depend_func = depend_func.idi_grau_depen_func no-error.
                            if  not avail grau_depend_estab_benefic then do:
                                create erros.
                                {utp/ut-field.i dthrpyc grau_depend_estab_benefic cdn_grau_depend_func 1}.
                                run utp/ut-msgs.p (input "msg",input 56,input return-value).
                                assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                       erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                       erros.des_erro                = return-value
                                       erros.log_erro                = yes
                                       erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                       erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                       erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                       erros.num_digito_verfdor_func = int(0)
                                       erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                       erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                       erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                       erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                       erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                       erros.valor                   = tt-movto_benefic.val_calcul_efp
                                       erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                       erros.parcelas                = 1
                                       erros.conteudo                = string(tt-beneficio.cdn_beneficio)
                                       v_log_permis   = no.
                                assign v_log_erros = yes.
                                NEXT BLOCO-BENEFICIO.
                            end.
                        end.  
        
        
                        /*    Valida data m�xima dependente para receber benef!cio   */
                        /*  Masculino  */  
                        if  depend_func.idi_sexo = 1 then do:
                            IF depend_func.idi_estado_saude = 2 THEN DO:
                                if  year(today) - year(depend_func.dat_nascimento) > grau_depend_estab_benefic.qtd_idade_max_depend_masc + DEC(SUBSTR(grau_depend_estab_benefic.cod_livre_1,001,003)) then do:
                                    create erros.
                                    run utp/ut-msgs.p (input "msg",input 8148,input return-value).
                                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                           erros.des_erro                = return-value
                                           erros.log_erro                = yes
                                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                           erros.num_digito_verfdor_func = int(0)
                                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                           erros.parcelas                = 1
                                           erros.conteudo                = string(year(today) - year(depend_func.dat_nascimento))
                                           v_log_permis   = no.
                                    assign v_log_erros = yes.
                                    NEXT BLOCO-BENEFICIO.
                                end.
                                else do:
                                    if  (year(today) - year(depend_func.dat_nascimento) = grau_depend_estab_benefic.qtd_idade_max_depend_masc + DEC(SUBSTR(grau_depend_estab_benefic.cod_livre_1,001,003))) and
                                        (month(today) < month(depend_func.dat_nascimento)) then do:
                                        create erros.
                                        run utp/ut-msgs.p (input "msg",input 8148,input return-value).
                                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                               erros.des_erro                = return-value
                                               erros.log_erro                = yes
                                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                               erros.num_digito_verfdor_func = int(0)
                                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                               erros.parcelas                = 1
                                               erros.conteudo                = string(grau_depend_estab_benefic.qtd_idade_max_depend_masc + DEC(SUBSTR(grau_depend_estab_benefic.cod_livre_1,001,003)))
                                               v_log_permis   = no.
                                        assign v_log_erros = yes.
                                        NEXT BLOCO-BENEFICIO.
                                    end.    
                                end.
                            END.
                            ELSE DO:
                                 if  year(today) - year(depend_func.dat_nascimento) > grau_depend_estab_benefic.qtd_idade_max_depend_masc then do:
                                     create erros.
                                     run utp/ut-msgs.p (input "msg",input 8148,input return-value).
                                     assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                            erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                            erros.des_erro                = return-value
                                            erros.log_erro                = yes
                                            erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                            erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                            erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                            erros.num_digito_verfdor_func = int(0)
                                            erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                            erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                            erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                            erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                            erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                            erros.valor                   = tt-movto_benefic.val_calcul_efp
                                            erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                            erros.parcelas                = 1
                                            erros.conteudo                = string(year(today) - year(depend_func.dat_nascimento))
                                            v_log_permis   = no.
                                     assign v_log_erros = yes.
                                     NEXT BLOCO-BENEFICIO.
                                 end.
                                 else do:
                                     if  (year(today) - year(depend_func.dat_nascimento) = grau_depend_estab_benefic.qtd_idade_max_depend_masc) and
                                         (month(today) < month(depend_func.dat_nascimento)) then do:
                                         create erros.
                                         run utp/ut-msgs.p (input "msg",input 8148,input return-value).
                                         assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                                erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                                erros.des_erro                = return-value
                                                erros.log_erro                = yes
                                                erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                                erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                                erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                                erros.num_digito_verfdor_func = int(0)
                                                erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                                erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                                erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                                erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                                erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                                erros.valor                   = tt-movto_benefic.val_calcul_efp
                                                erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                                erros.parcelas                = 1
                                                erros.conteudo                = string(grau_depend_estab_benefic.qtd_idade_max_depend_masc)
                                                v_log_permis   = no.
                                          assign v_log_erros = yes.
                                          NEXT BLOCO-BENEFICIO.
                                     end.    
                                 end.
                            END.
                        end. /* Se Masculino */
        
        
                        /*   Se feminino     */
                        else do:
                            IF depend_func.idi_estado_saude_depend = 2 THEN DO:
                                if  year(today) - year(depend_func.dat_nascimento) > grau_depend_estab_benefic.qtd_idade_max_depend_fem + DEC(SUBSTR(grau_depend_estab_benefic.cod_livre_1,001,003)) then do:
                                  create erros.
                                  run utp/ut-msgs.p (input "msg",input 8148,input return-value).
                                  assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                         erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                         erros.des_erro                = return-value
                                         erros.log_erro                = yes
                                         erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                         erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                         erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                         erros.num_digito_verfdor_func = int(0)
                                         erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                         erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                         erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                         erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                         erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                         erros.valor                   = tt-movto_benefic.val_calcul_efp
                                         erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                         erros.parcelas                = 1
                                         erros.conteudo                = string(year(today) - year(depend_func.dat_nascimento))
                                         v_log_permis   = no.
                                  assign v_log_erros = yes.
                                  NEXT BLOCO-BENEFICIO.
                                end.
                                else do:
                                   if  (year(today) - year(depend_func.dat_nascimento) = grau_depend_estab_benefic.qtd_idade_max_depend_fem + DEC(SUBSTR(grau_depend_estab_benefic.cod_livre_1,001,003))) and
                                       (month(today) < month(depend_func.dat_nascimento)) then do:
                                       create erros.
                                       run utp/ut-msgs.p (input "msg",input 8148,input return-value).
                                       assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                              erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                              erros.des_erro                = return-value
                                              erros.log_erro                = yes
                                              erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                              erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                              erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                              erros.num_digito_verfdor_func = int(0)
                                              erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                              erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                              erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                              erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                              erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                              erros.valor                   = tt-movto_benefic.val_calcul_efp
                                              erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                              erros.parcelas                = 1
                                              erros.conteudo                = string(grau_depend_estab_benefic.qtd_idade_max_depend_fem + DEC(SUBSTR(grau_depend_estab_benefic.cod_livre_1,001,003)))
                                              v_log_permis   = no.
                                       assign v_log_erros = yes.
                                       NEXT BLOCO-BENEFICIO.
                                   end.    
                                end.
                            END.
                            ELSE DO:
                                 if  year(today) - year(depend_func.dat_nascimento) > grau_depend_estab_benefic.qtd_idade_max_depend_fem then do:
                                   create erros.
                                   run utp/ut-msgs.p (input "msg",input 8148,input return-value).
                                   assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                          erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                          erros.des_erro                = return-value
                                          erros.log_erro                = yes
                                          erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                          erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                          erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                          erros.num_digito_verfdor_func = int(0)
                                          erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                          erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                          erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                          erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                          erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                          erros.valor                   = tt-movto_benefic.val_calcul_efp
                                          erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                          erros.parcelas                = 1
                                          erros.conteudo                = string(year(today) - year(depend_func.dat_nascimento))
                                          v_log_permis   = no.
                                   assign v_log_erros = yes.
                                   NEXT BLOCO-BENEFICIO.
                                 end.
                                 else do:
                                   if  (year(today) - year(depend_func.dat_nascimento) = grau_depend_estab_benefic.qtd_idade_max_depend_fem) and
                                       (month(today) < month(depend_func.dat_nascimento)) then do:
                                       create erros.
                                       run utp/ut-msgs.p (input "msg",input 8148,input return-value).
                                       assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                              erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                              erros.des_erro                = return-value
                                              erros.log_erro                = yes
                                              erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                              erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                              erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                              erros.num_digito_verfdor_func = int(0)
                                              erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                              erros.cdn_beneficio           = tt-beneficio.cdn_beneficio 
                                              erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                              erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                              erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                              erros.valor                   = tt-movto_benefic.val_calcul_efp
                                              erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                              erros.parcelas                = 1
                                              erros.conteudo                = string(grau_depend_estab_benefic.qtd_idade_max_depend_fem)
                                              v_log_permis   = no.
                                       assign v_log_erros = yes.
                                       NEXT BLOCO-BENEFICIO.
                                   end.    
                                 end.
                            END.
                        end. /* Se Feminino */
                    end. /* Se separado */
                end. /* tt-movto_benefic.cdn_depend_func > 0 */
        
        
                if  1 <= 0 then do:  /*parcela forcado 1*/
                    create erros.
                    {utp/ut-liter.i Parcelas MBS R}
                    run utp/ut-msgs.p (input "msg", 
                                       input 7938, 
                                       input return-value).
                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                           erros.des_erro                = return-value
                           erros.log_erro                = yes
                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                           erros.num_digito_verfdor_func = int(0)
                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                           erros.parcelas                = 1
                           erros.conteudo                = string(1)
                           v_log_permis   = no.
                    assign v_log_erros = yes.
                    NEXT BLOCO-BENEFICIO.
                end.
        
                if  beneficio.idi_unid_medid_benefic = 1 /* Valor */ then do:
                    if  tt-movto_benefic.qtd_unid_acordo_efp <> 0 then do:
                        create erros.
                        {utp/ut-field.i dthrpyc movto_benefic qtd_unid_acordo_efp 1}.
                        run utp/ut-msgs.p (input "msg", 
                                           input 8135, 
                                           input return-value).
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-movto_benefic.qtd_unid_acordo_efp)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    end.
        
        
                    if  tt-movto_benefic.val_calcul_efp = 0 then do:
                        create erros.
                        {utp/ut-field.i dthrpyc movto_benefic val_calcul_efp 1}.
                        run utp/ut-msgs.p (input "msg", input 7938, input return-value).
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic                        
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-movto_benefic.val_calcul_efp)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    end.
                     
                end. /* beneficio.idi_unid_medid_benefic = 1 */
                ELSE DO:
                    if  beneficio.idi_unid_medid_benefic = 2 /* QUANTIDADE */ then do:
                        if  tt-movto_benefic.qtd_unid_acordo_efp = 0 then do:
                            create erros.
                            {utp/ut-field.i dthrpyc movto_benefic qtd_unid_acordo_efp 1}.
                            run utp/ut-msgs.p (input "msg", 
                                               input 7938, 
                                               input return-value).
                            assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                   erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                   erros.des_erro                = return-value
                                   erros.log_erro                = yes
                                   erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                   erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                   erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                   erros.num_digito_verfdor_func = int(0)
                                   erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                   erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                   erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                   erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                   erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                   erros.valor                   = tt-movto_benefic.val_calcul_efp
                                   erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                   erros.parcelas                = 1
                                   erros.conteudo                = string(tt-movto_benefic.qtd_unid_acordo_efp)
                                   v_log_permis                  = no.
                            assign v_log_erros = yes.
                            NEXT BLOCO-BENEFICIO.
                        END.
                        
                    END.
                END.
                if  beneficio.log_informa_prestdor_benefic = yes then do:
                    find conven_benefic no-lock where
                         conven_benefic.cdn_empresa       = tt-movto_benefic.cdn_empresa and
                        (conven_benefic.cdn_estab         = tt-movto_benefic.cdn_estab or
                         conven_benefic.cdn_estab         = "*") and 
                         conven_benefic.cdn_beneficio     = tt-beneficio.cdn_beneficio and
                         conven_benefic.cdn_prestdor_serv = tt-beneficio.cdn_prestdor_serv no-error.
                    IF  NOT AVAIL conven_benefic THEN DO:
                        create erros.
                        run utp/ut-msgs.p (input "msg", input 15907, input "":U).
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-beneficio.cdn_prestdor_serv)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    END.
                end.
        
        
 
        
                assign i-mes = tt-movto_benefic.num_mes_refer_movto_benefic + 1
                       i-ano = tt-movto_benefic.num_ano_refer_movto_benefic.
        
        
                if  i-mes > 12 then
                    assign i-mes = 1 
                           i-ano = i-ano + 1.
        
        
                assign i-dat-inic-mes = date(tt-movto_benefic.num_mes_refer_movto_benefic,01,tt-movto_benefic.num_ano_refer_movto_benefic)
                       i-dat-term-mes = date(i-mes,01,i-ano) - 1.
        
        
                find first excec_benefic_func no-lock where
                           excec_benefic_func.cdn_empresa     = tt-movto_benefic.cdn_empresa     and
                           excec_benefic_func.cdn_estab       = tt-movto_benefic.cdn_estab       and
                           excec_benefic_func.cdn_funcionario = tt-movto_benefic.cdn_funcionario and
                           excec_benefic_func.cdn_depend_func = tt-movto_benefic.cdn_depend_func and
                           excec_benefic_func.cdn_beneficio   = tt-beneficio.cdn_beneficio   and
                           excec_benefic_func.cdn_regra_benefic = tt-beneficio.cdn_regra_benefic AND
                           excec_benefic_func.dat_inic_excec_benefic_func <= i-dat-term-mes        and
                           excec_benefic_func.dat_term_excec_benefic_func >= i-dat-inic-mes no-error.
                if  avail excec_benefic_func then do:
                    create erros.
                    run utp/ut-msgs.p (input "msg",
                                       input 25718,
                                       input "":U).
                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                           erros.des_erro                = return-value
                           erros.log_erro                = yes
                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                           erros.num_digito_verfdor_func = int(0)
                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                           erros.parcelas                = 1
                           erros.conteudo                = string(tt-movto_benefic.val_calcul_efp)
                           v_log_permis   = no.
                    assign v_log_erros = yes.
                    NEXT BLOCO-BENEFICIO.
                end.
    
                
             
                /* Uma Parcela */
                do:
                
                if v_num_lote = 0 then do:
                 
                   find last  refer_movto_benefic   where
                         refer_movto_benefic.cdn_empresa                = tt-movto_benefic.cdn_empresa and
                         refer_movto_benefic.cdn_beneficio              = tt-beneficio.cdn_beneficio and
                         refer_movto_benefic.cdn_regra_benefic          = tt-beneficio.cdn_regra_benefic AND
                         refer_movto_benefic.num_mes_lote_movto_benefic = tt-movto_benefic.num_mes_refer_movto_benefic and
                         refer_movto_benefic.num_ano_lote_movto_benefic = tt-movto_benefic.num_ano_refer_movto_benefic and
                         refer_movto_benefic.num_lote_movto_benefic     > v_num_lote no-lock no-error.
                         
                    if avail refer_movto_benefic then  v_num_lote =  refer_movto_benefic.num_lote_movto_benefic + 1.
                    else   
                      v_num_lote = 1.
                         
                         
                end.
                    assign v_achou = no.
                    find refer_movto_benefic exclusive-lock where
                         refer_movto_benefic.cdn_empresa                = tt-movto_benefic.cdn_empresa and
                         refer_movto_benefic.cdn_beneficio              = tt-beneficio.cdn_beneficio and
                         refer_movto_benefic.cdn_regra_benefic          = tt-beneficio.cdn_regra_benefic AND
                         refer_movto_benefic.num_mes_lote_movto_benefic = tt-movto_benefic.num_mes_refer_movto_benefic and
                         refer_movto_benefic.num_ano_lote_movto_benefic = tt-movto_benefic.num_ano_refer_movto_benefic and
                         refer_movto_benefic.num_lote_movto_benefic     = v_num_lote no-error.
                    if  not avail refer_movto_benefic then do:
                        create refer_movto_benefic.
                        assign refer_movto_benefic.cdn_empresa                   = tt-movto_benefic.cdn_empresa
                               refer_movto_benefic.cdn_beneficio                 = tt-beneficio.cdn_beneficio
                               refer_movto_benefic.cdn_regra_benefic             = tt-beneficio.cdn_regra_benefic
                               refer_movto_benefic.num_ano_lote_movto_benefic    = tt-movto_benefic.num_ano_refer_movto_benefic
                               refer_movto_benefic.num_mes_lote_movto_benefic    = tt-movto_benefic.num_mes_refer_movto_benefic
                               refer_movto_benefic.num_lote_movto_benefic        = v_num_lote
                               refer_movto_benefic.cdn_prestdor_serv             = tt-beneficio.cdn_prestdor_serv
                               refer_movto_benefic.cdn_motiv_lote_benefic        = tt-refer_movto_benefic.cdn_motiv_lote_benefic.
                       &if "{&dthrpyc_dbtype}" <> "progress" &then
                           validate refer_movto_benefic no-error.
                       &endif
                    end.      
                    else assign v_achou = yes.
                     
                   
                    FIND last b_movto_benefic no-lock where
                              b_movto_benefic.cdn_empresa     = tt-movto_benefic.cdn_empresa     and
                              b_movto_benefic.cdn_estab       = tt-movto_benefic.cdn_estab       and
                              b_movto_benefic.cdn_funcionario = tt-movto_benefic.cdn_funcionario and
                              b_movto_benefic.num_seq_movto_benefic <= 9999999 no-error.
                    
                    create movto_benefic.
                    assign movto_benefic.cdn_empresa                 = tt-movto_benefic.cdn_empresa
                           movto_benefic.cdn_estab                   = tt-movto_benefic.cdn_estab
                           movto_benefic.cdn_funcionario             = tt-movto_benefic.cdn_funcionario               
                           movto_benefic.cdn_depend_func             = tt-movto_benefic.cdn_depend_func
                           movto_benefic.cdn_beneficio               = tt-beneficio.cdn_beneficio
                           movto_benefic.cdn_regra_benefic           = tt-beneficio.cdn_regra_benefic
                           movto_benefic.num_mes_refer_movto_benefic = tt-movto_benefic.num_mes_refer_movto_benefic
                           movto_benefic.num_ano_refer_movto_benefic = tt-movto_benefic.num_ano_refer_movto_benefic  
                           movto_benefic.num_mes_lote_movto_benefic  = tt-movto_benefic.num_mes_refer_movto_benefic
                           movto_benefic.num_ano_lote_movto_benefic  = tt-movto_benefic.num_ano_refer_movto_benefic
                           movto_benefic.val_calcul_efp              = tt-movto_benefic.val_calcul_efp
                           movto_benefic.qtd_unid_acordo_efp         = tt-movto_benefic.qtd_unid_acordo_efp
                           movto_benefic.cdn_formul_calc             = tt-movto_benefic.cdn_formul_calc_benefic
                           movto_benefic.cod_rh_ccusto               = funcionario.cod_rh_ccusto
                           movto_benefic.dat_ocor_movto_benefic      = tt-movto_benefic.dat_ocor_movto_benefic
                           movto_benefic.cod_docto_movto_benefic     = tt-movto_benefic.cod_docto_movto_benefic
                           movto_benefic.dat_pagto_efet_efp          = tt-movto_benefic.dat_pagto_efet_efp              
                           movto_benefic.num_lote_movto_benefic      = v_num_lote
                           movto_benefic.val_origin_movto_benefic    = movto_benefic.val_calcul_efp
                           movto_benefic.cdn_motiv_lote_benefic      = tt-refer_movto_benefic.cdn_motiv_lote_benefic
                           movto_benefic.cdn_prestdor_serv           = tt-movto_benefic.cdn_prestdor_serv
                           movto_benefic.qti_compos_benefic[1]       = 1
                           movto_benefic.val_compos_benefic[1]       = tt-movto_benefic.val_calcul_efp / tt-movto_benefic.qtd_unid_acordo_efp
                           movto_benefic.qti_dias_concedid           = tt-movto_benefic.qtd_unid_acordo_efp
                           movto_benefic.cdn_local_pagto             = funcionario.cdn_local_pagto.

                     
                    IF c-per-ini <> " " THEN
                       ASSIGN movto_benefic.dat_inic_period    = DATE(INT(SUBSTRING(c-per-ini,3,2)), INT(SUBSTRING(c-per-ini,1,2)), INT(SUBSTRING(c-per-ini,5,4)))
                              movto_benefic.dat_fim_period     = DATE(INT(SUBSTRING(c-per-fim,3,2)), INT(SUBSTRING(c-per-fim,1,2)), int(SUBSTRING(c-per-fim,5,4))).

                    
                    if  beneficio.log_informa_prestdor_benefic = yes then do:
                        assign movto_benefic.cdn_prestdor_serv = tt-beneficio.cdn_prestdor_serv.
                        if  avail conven_benefic then
                            assign movto_benefic.log_pago_pelo_sist = conven_benefic.log_pago_pelo_sist.
                    end.
                    if avail b_movto_benefic then
                       assign movto_benefic.num_seq_movto_benefic =
                              b_movto_benefic.num_seq_movto_benefic + 1.
                    else
                       assign movto_benefic.num_seq_movto_benefic = 1.
        /*
                    create erros.
                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                           erros.des_erro                = "Importacao realizada com sucesso."
                           erros.log_erro                = no
                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                           erros.num_digito_verfdor_func = int(0)
                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                           erros.quant                   = movto_benefic.qtd_unid_acordo_efp
                           erros.parcelas                = 1
                           erros.conteudo                = "".
        */
        
                    if  v_achou = yes then do:
                        assign refer_movto_benefic.qti_infor_lancto_lote_benefic = refer_movto_benefic.qti_infor_lancto_lote_benefic + 1
                               refer_movto_benefic.qti_infor_lote_benefic        = refer_movto_benefic.qti_infor_lote_benefic + movto_benefic.qtd_unid_acordo_efp
                               refer_movto_benefic.val_infor_lote_benefic        = refer_movto_benefic.val_infor_lote_benefic + movto_benefic.val_calcul_efp
                               refer_movto_benefic.vli_soma_lancto_lote_benefic  = refer_movto_benefic.qti_infor_lancto_lote_benefic
                               refer_movto_benefic.qtd_soma_quant_lote_benefic   = refer_movto_benefic.qti_infor_lote_benefic
                               refer_movto_benefic.val_soma_val_lote_benefic     = refer_movto_benefic.val_infor_lote_benefic.
                    end.
                    else do:
                        assign refer_movto_benefic.qti_infor_lancto_lote_benefic =  1
                               refer_movto_benefic.qti_infor_lote_benefic        = movto_benefic.qtd_unid_acordo_efp
                               refer_movto_benefic.val_infor_lote_benefic        = movto_benefic.val_calcul_efp
                               refer_movto_benefic.vli_soma_lancto_lote_benefic  = 1
                               refer_movto_benefic.qtd_soma_quant_lote_benefic   = movto_benefic.qtd_unid_acordo_efp
                               refer_movto_benefic.val_soma_val_lote_benefic     = movto_benefic.val_calcul_efp.
                    end.
                    validate movto_benefic.
                    ON WRITE OF movto_benefic OVERRIDE DO:
                    END.
                    assign movto_benefic.num_seq_movto_benefic_orig = movto_benefic.num_seq_movto_benefic.
                    ASSIGN v_row_movto_benefic = ROWID(movto_benefic).
                    FIND movto_benefic EXCLUSIVE-LOCK WHERE
                        ROWID(movto_benefic) = v_row_movto_benefic NO-ERROR.
                    assign movto_benefic.num_seq_movto_benefic = movto_benefic.num_seq_movto_benefic_orig.
                    ON WRITE OF movto_benefic REVERT.
                end. /* 1 = 1 (Uma parcela) */
            end. /* avail param_benefic_estab */
        END.
        ELSE DO: /*Benef!cio por Regra*/
            IF CAN-FIND (regra_benefic_empres_estab WHERE 
                         ((regra_benefic_empres_estab.cdn_empresa  = tt-movto_benefic.cdn_empresa AND
                           regra_benefic_empres_estab.cdn_estab    = tt-movto_benefic.cdn_estab) OR 
                          (regra_benefic_empres_estab.cdn_empresa  = "*" AND
                           regra_benefic_empres_estab.cdn_estab    = "*")) AND
                         regra_benefic_empres_estab.cdn_beneficio = tt-beneficio.cdn_beneficio AND
                         regra_benefic_empres_estab.cdn_regra_benefic = tt-beneficio.cdn_regra_benefic) THEN DO:
               IF CAN-FIND (regra_benefic_lotac WHERE
                            regra_benefic_lotac.cdn_beneficio     = tt-beneficio.cdn_beneficio AND
                            regra_benefic_lotac.cdn_regra_benefic = tt-beneficio.cdn_regra_benefic AND
                            ((regra_benefic_lotac.cdn_plano_lotac   = funcionario.cdn_plano_lotac  AND
                              regra_benefic_lotac.cod_unid_lotac    = funcionario.cod_unid_lotac) OR 
                             (regra_benefic_lotac.cdn_plano_lotac   = 0  AND
                              regra_benefic_lotac.cod_unid_lotac    = "0"))) THEN DO:
                  IF CAN-FIND (regra_benefic_sind WHERE
                               regra_benefic_sind.cdn_beneficio     = tt-beneficio.cdn_beneficio AND
                               regra_benefic_sind.cdn_regra_benefic = tt-beneficio.cdn_regra_benefic AND
                               ((regra_benefic_sind.cdn_sindicato     = funcionario.cdn_sindicato) or
                                (regra_benefic_sind.cdn_sindicato     = 0))) THEN DO:
                     IF NOT CAN-FIND (regra_benefic_cargo WHERE
                                      regra_benefic_cargo.cdn_beneficio     = tt-beneficio.cdn_beneficio AND
                                      regra_benefic_cargo.cdn_regra_benefic = tt-beneficio.cdn_regra_benefic AND
                                      ((regra_benefic_cargo.cdn_cargo_basic   = funcionario.cdn_cargo_basic AND
                                        regra_benefic_cargo.cdn_niv_cargo     = funcionario.cdn_niv_cargo) OR 
                                       (regra_benefic_cargo.cdn_cargo_basic   = 0 AND
                                        regra_benefic_cargo.cdn_niv_cargo     = 0))) THEN DO:
                        create erros.
                        run utp/ut-msgs.p (input "msg",input 30854,input return-value).
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-beneficio.cdn_beneficio)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                     END.
                  END.
                  ELSE DO:
                     create erros.
                     run utp/ut-msgs.p (input "msg",input 30856,input return-value).
                     assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                            erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                            erros.des_erro                = return-value
                            erros.log_erro                = yes
                            erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                            erros.cdn_estab               = tt-movto_benefic.cdn_estab
                            erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                            erros.num_digito_verfdor_func = int(0)
                            erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                            erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                            erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                            erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                            erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                            erros.valor                   = tt-movto_benefic.val_calcul_efp
                            erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                            erros.parcelas                = 1
                            erros.conteudo                = string(tt-beneficio.cdn_beneficio)
                            v_log_permis   = no.
                     assign v_log_erros = yes.
                     NEXT BLOCO-BENEFICIO.
                  END.
               END.
               ELSE DO:
                  create erros.
                  run utp/ut-msgs.p (input "msg",input 30855,input return-value).
                  assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                         erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                         erros.des_erro                = return-value
                         erros.log_erro                = yes
                         erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                         erros.cdn_estab               = tt-movto_benefic.cdn_estab
                         erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                         erros.num_digito_verfdor_func = int(0)
                         erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                         erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                         erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                         erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                         erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                         erros.valor                   = tt-movto_benefic.val_calcul_efp
                         erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                         erros.parcelas                = 1
                         erros.conteudo                = string(tt-beneficio.cdn_beneficio)
                         v_log_permis   = no.
                  assign v_log_erros = yes.
                  NEXT BLOCO-BENEFICIO.
               END.
            END.
            ELSE DO:
               create erros.
               run utp/ut-msgs.p (input "msg",input 30853,input return-value).
               assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                      erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                      erros.des_erro                = return-value
                      erros.log_erro                = yes
                      erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                      erros.cdn_estab               = tt-movto_benefic.cdn_estab
                      erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                      erros.num_digito_verfdor_func = int(0)
                      erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                      erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                      erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                      erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                      erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                      erros.valor                   = tt-movto_benefic.val_calcul_efp
                      erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                      erros.parcelas                = 1
                      erros.conteudo                = string(tt-beneficio.cdn_beneficio)
                      v_log_permis   = no.
               assign v_log_erros = yes.
               NEXT BLOCO-BENEFICIO.
            END.
            
            find last regra_benefic_param no-lock where 
                      regra_benefic_param.cdn_beneficio     = tt-beneficio.cdn_beneficio and
                      regra_benefic_param.cdn_regra_benefic = tt-beneficio.cdn_regra_benefic no-error.
            if  not avail regra_benefic_param then do:
                create erros.
                {utp/ut-table.i dthrpyc regra_benefic_param 1}.
                run utp/ut-msgs.p (input "msg",input 56,input return-value).
                assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                       erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                       erros.des_erro                = return-value
                       erros.log_erro                = yes
                       erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                       erros.cdn_estab               = tt-movto_benefic.cdn_estab
                       erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                       erros.num_digito_verfdor_func = int(0)
                       erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                       erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                       erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                       erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                       erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                       erros.valor                   = tt-movto_benefic.val_calcul_efp
                       erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                       erros.parcelas                = 1
                       erros.conteudo                = string(tt-beneficio.cdn_beneficio)
                       v_log_permis   = no.
                assign v_log_erros = yes.
                NEXT BLOCO-BENEFICIO.
            end.
            else do:
                /* Jaubert pediu para comentar a Lcgica abaixo comentada para solucionar FO 1671.857 */
                /*IF beneficio.idi_relacto_func = 1 THEN DO: /*�nico*/
                    if  regra_benefic_param.log_param_benefic_ok = no then do:
                        create erros.
                        run utp/ut-msgs.p (input "msg",input 8132,input return-value).
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(regra_benefic_param.log_param_benefic_ok)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    END.
                END. */
                if  1 > regra_benefic_param.qtd_max_parc_benefic then do: /*forcando parcela 1*/
                    create erros.
                    run utp/ut-msgs.p (input "msg",input 8141,input return-value).
                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                           erros.des_erro                = return-value
                           erros.log_erro                = yes
                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                           erros.num_digito_verfdor_func = int(0)
                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                           erros.parcelas                = 1
                           erros.conteudo                = string(1)
                           v_log_permis   = no.
                    assign v_log_erros = yes.
                    NEXT BLOCO-BENEFICIO.
                END.
                
                if  regra_benefic_param.log_usa_formul_calc_benefic = yes then do:
                    if  tt-movto_benefic.cdn_formul_calc_benefic = 0 then do:
                        assign tt-movto_benefic.cdn_formul_calc_benefic = if tt-movto_benefic.cdn_depend_func = 0 
                                                   then regra_benefic_param.cdn_formul_benefic_padr_func
                                                   else regra_benefic_param.cdn_formul_benefic_padr_depend.
                    END.
                    else do:
                        IF  not can-find(first formul_calc_benefic where
                                               formul_calc_benefic.cdn_empresa = "*" AND
                                               formul_calc_benefic.cdn_formul_calc_benefic = tt-movto_benefic.cdn_formul_calc_benefic AND
                                               formul_calc_benefic.num_seq_formul_benefic = 1) then do:
                            create erros.              
                            {utp/ut-field.i dthrpyc movto_benefic cdn_formul_calc_benefic 1}
                            run utp/ut-msgs.p (input "msg",input 15451,input return-value).
                            assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                   erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                   erros.des_erro                = return-value
                                   erros.log_erro                = yes
                                   erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                   erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                   erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                   erros.num_digito_verfdor_func = int(0)
                                   erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                   erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                   erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                   erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                   erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                   erros.valor                   = tt-movto_benefic.val_calcul_efp
                                   erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                   erros.parcelas                = 1
                                   erros.conteudo                = string(tt-movto_benefic.cdn_formul_calc_benefic)
                                   v_log_permis   = no.
                            assign v_log_erros = yes.
                            NEXT BLOCO-BENEFICIO.
                        END.
                    END.
                END. 
    
                IF  beneficio.log_informa_prestdor_benefic AND
                    tt-beneficio.cdn_prestdor_serv = 0 THEN DO:
                    FIND FIRST regra_benefic_conven NO-LOCK WHERE
                               regra_benefic_conven.cdn_empresa       = tt-movto_benefic.cdn_empresa       AND
                               regra_benefic_conven.cdn_beneficio     = tt-movto_benefic.cdn_beneficio     AND
                               regra_benefic_conven.cdn_regra_benefic = tt-movto_benefic.cdn_regra_benefic NO-ERROR.
                    IF AVAIL regra_benefic_conven THEN
                        ASSIGN tt-beneficio.cdn_prestdor_serv = regra_benefic_conven.cdn_prestdor_serv.
                    ASSIGN tt-beneficio.cdn_prestdor_serv = regra_benefic_param.cdn_prestdor_serv_padr.
                END.
    
                if  beneficio.log_informa_prestdor_benefic and
                    tt-beneficio.cdn_prestdor_serv = 0 then do:
                    create erros.
                    run utp/ut-msgs.p (input "msg",input 8147,input return-value).
                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                           erros.des_erro                = return-value
                           erros.log_erro                = yes
                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                           erros.num_digito_verfdor_func = int(0)
                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                           erros.parcelas                = 1
                           erros.conteudo                = string(tt-beneficio.cdn_prestdor_serv)
                           v_log_permis   = no.
                    assign v_log_erros = yes.
                    NEXT BLOCO-BENEFICIO.
                END. 
    
                /*    Verifica se funcion�rio est� afastado      */
                &if "{&dthrpyc_version}" >= "2.06" &then 
                     find last sit_afast_func no-lock use-index stfstfnc_term where
                               sit_afast_func.cdn_empresa           = tt-movto_benefic.cdn_empresa            and
                               sit_afast_func.cdn_estab             = tt-movto_benefic.cdn_estab              and
                               sit_afast_func.cdn_funcionario       = tt-movto_benefic.cdn_funcionario        and
                               sit_afast_func.dat_inic_sit_afast   <= dt-referencia-ini        and
                               sit_afast_func.dat_term_sit_afast   >= dt-referencia-fim       no-error.
                &else
                     find last sit_afast_func no-lock where
                               sit_afast_func.cdn_empresa           = tt-movto_benefic.cdn_empresa            and
                               sit_afast_func.cdn_estab             = tt-movto_benefic.cdn_estab              and
                               sit_afast_func.cdn_funcionario       = tt-movto_benefic.cdn_funcionario        and
                               sit_afast_func.dat_inic_sit_afast   <= dt-referencia-ini        and
                               sit_afast_func.dat_term_sit_afast   >= dt-referencia-fim       no-error.
                &endif               
        
        
                if  avail sit_afast_func then do:
                    find  sit_benefic no-lock where
                          sit_benefic.cdn_empresa        = sit_afast_func.cdn_empresa and
                         (sit_benefic.cdn_estab          = sit_afast_func.cdn_estab or
                          sit_benefic.cdn_estab          = "*")  and
                          sit_benefic.cdn_beneficio      = beneficio.cdn_beneficio  and
                          sit_benefic.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func no-error.
                    if  avail sit_benefic then do:
                        if  sit_benefic.idi_tip_relac_sit_benefic = 1 then do:  
                            assign v_data = sit_afast_func.dat_inic_sit_afast + sit_benefic.qti_dias_sit_influi_fp.
        
                            if  v_data > sit_afast_func.dat_term_sit_afast then
                                NEXT BLOCO-BENEFICIO.
        
                            if  v_data <= dt-referencia-fim then do:
                                create erros.
                                {utp/ut-field.i dthrpyc movto_benefic cdn_formul_calc_benefic 1}
                                run utp/ut-msgs.p (input "msg",input 8199,input return-value).
                                if  return-value <> "yes" then do:
                                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                           erros.des_erro                = return-value
                                           erros.log_erro                = yes
                                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                           erros.num_digito_verfdor_func = int(0)
                                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                           erros.parcelas                = 1
                                           erros.conteudo                = string(tt-movto_benefic.dat_ocor_movto_benefic)
                                           v_log_permis   = no.
                                    assign v_log_erros = yes.
                                    NEXT BLOCO-BENEFICIO.
                                END.
                            END.
                        END.
                    END.
                END. 
        
                /*************************************************/
        
                if  beneficio.idi_tip_benefic <> 2 then do:
                    create erros.
                    {utp/ut-field.i dthrpyc movto_benefic cdn_formul_calc_benefic 1}
                    run utp/ut-msgs.p (input "msg",input 8133,input return-value).
                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                           erros.des_erro                = return-value
                           erros.log_erro                = yes
                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                           erros.num_digito_verfdor_func = int(0)
                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                           erros.parcelas                = 1
                           erros.conteudo                = string(beneficio.idi_tip_benefic)
                           v_log_permis   = no.
                    assign v_log_erros = yes.
                    NEXT BLOCO-BENEFICIO.
                end.
                if  beneficio.log_informa_prestdor_benefic = yes then do:
                    if  tt-beneficio.cdn_prestdor_serv <= 0 then do:
                        create erros.
                        {utp/ut-field.i dthrpyc movto_benefic cdn_prestdor_serv 1}
                        run utp/ut-msgs.p (input "msg",input 7938,input return-value).
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-beneficio.cdn_prestdor_serv)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    end.
                end. 
        
                /*    Se Informado    */  
        
                /* criando benefic_func quando nAo existe - schossland */
                find benefic_func no-lock where
                         benefic_func.cdn_empresa       = tt-movto_benefic.cdn_empresa and
                         benefic_func.cdn_estab         = tt-movto_benefic.cdn_estab and
                         benefic_func.cdn_funcionario   = tt-movto_benefic.cdn_funcionario and
                         benefic_func.cdn_depend_func   = tt-movto_benefic.cdn_depend_func and
                         benefic_func.cdn_beneficio     = tt-beneficio.cdn_beneficio and
                         benefic_func.cdn_regra_benefic = tt-beneficio.cdn_regra_benefic no-error.
                if  not avail benefic_func then do:
                    create benefic_func.
                    assign benefic_func.cdn_empresa       = tt-movto_benefic.cdn_empresa 
                           benefic_func.cdn_estab         = tt-movto_benefic.cdn_estab 
                           benefic_func.cdn_funcionario   = tt-movto_benefic.cdn_funcionario 
                           benefic_func.cdn_depend_func   = tt-movto_benefic.cdn_depend_func 
                           benefic_func.cdn_beneficio     = tt-beneficio.cdn_beneficio 
                           benefic_func.cdn_regra_benefic = tt-beneficio.cdn_regra_benefic.
                end.
       
                if  beneficio.idi_distrib_benefic  = 2 then do:
                    find benefic_func no-lock where
                         benefic_func.cdn_empresa       = tt-movto_benefic.cdn_empresa and
                         benefic_func.cdn_estab         = tt-movto_benefic.cdn_estab and
                         benefic_func.cdn_funcionario   = tt-movto_benefic.cdn_funcionario and
                         benefic_func.cdn_depend_func   = tt-movto_benefic.cdn_depend_func and
                         benefic_func.cdn_beneficio     = tt-beneficio.cdn_beneficio and
                         benefic_func.cdn_regra_benefic = tt-beneficio.cdn_regra_benefic no-error.
                    if  not avail benefic_func then do:
                        create erros.
                        {utp/ut-field.i dthrpyc movto_benefic cdn_depend_func 1}
                        run utp/ut-msgs.p (input "msg",input 8318,input "").
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-movto_benefic.cdn_depend_func)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    end.
        
                    if  benefic_func.dat_inic_benefic > tt-movto_benefic.dat_ocor_movto_benefic or
                        benefic_func.dat_term_benefic < tt-movto_benefic.dat_ocor_movto_benefic then do:
                        create erros.
                        run utp/ut-msgs.p (input "msg",
                                           input 4978,
                                           input "").
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-beneficio.cdn_beneficio)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    end.
        
                    if  benefic_func.idi_sit_benefic = 2 then do:
                        create erros.
                        run utp/ut-msgs.p (input "msg",
                                           input 4502,
                                           input "").
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-beneficio.cdn_beneficio)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    end. 
                end. 
                /*   Se Geral        */
                else do:
                    if  v_log_folha_educnal then do:
                        create erros.
                        run utp/ut-msgs.p (input "msg", input 8377, input "").
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(funcionario.dat_admis_func)
                               v_log_permis                  = no.
                    end.
                END. 
        
                /*   Verifica se quantidade meses apos admissao sAo insuficientes para concessao do beneficio */
                if  (month(funcionario.dat_admis_func) + regra_benefic_param.qtd_min_meses_admis) > 12 then do:
                    assign v-mes-adm = month(funcionario.dat_admis_func) + regra_benefic_param.qtd_min_meses_admis
                           v-ano-adm = year(funcionario.dat_admis_func).
                    repeat while v-mes-adm > 12:
                           assign v-mes-adm = v-mes-adm - 12
                                  v-ano-adm = v-ano-adm + 1.
                    end.
                    assign  v-dat-adm = date(v-mes-adm,day(funcionario.dat_admis_func),v-ano-adm).
                end.
                else assign v-dat-adm = date(month(funcionario.dat_admis_func) + integer(regra_benefic_param.qtd_min_meses_admis),day(funcionario.dat_admis_func),year(funcionario.dat_admis_func)).
                if  tt-movto_benefic.num_ano_refer_movto_benefic < year(v-dat-adm) or
                    (tt-movto_benefic.num_ano_refer_movto_benefic  = year(v-dat-adm) and
                    tt-movto_benefic.num_mes_refer_movto_benefic  < month(v-dat-adm)) then do:
                    create erros.
                    run utp/ut-msgs.p (input "msg",input 8149,input return-value).
                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                           erros.des_erro                = return-value
                           erros.log_erro                = yes
                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                           erros.num_digito_verfdor_func = int(0)
                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                           erros.parcelas                = 1
                           erros.conteudo                = string(funcionario.dat_admis_func)
                           v_log_permis   = no.
                    assign v_log_erros = yes.
                    NEXT BLOCO-BENEFICIO.
                end.
       
                /***********************************************************************************************/     
                if  tt-movto_benefic.cdn_depend_func > 0 then do:
                    if  beneficio.log_depend_func = NO then do:
                        create erros.
                        run utp/ut-msgs.p (input "msg",input 8134,input return-value).
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-movto_benefic.cdn_depend_func)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    end.
        
                    /*    Se Junto    */  /* usado no dicionario */
                    if  beneficio.idi_control_depend_benefic = 1 then do:
                        if  tt-movto_benefic.cdn_depend_func <> 999 then do:
                            create erros.
                            {utp/ut-field.i dthrpyc movto_benefic cdn_depend_func 1}.
                            run utp/ut-msgs.p (input "msg",input 8262,input return-value).
                            assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                   erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                   erros.des_erro                = return-value
                                   erros.log_erro                = yes
                                   erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                   erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                   erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                   erros.num_digito_verfdor_func = int(0)
                                   erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                   erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                   erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                   erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                   erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                   erros.valor                   = tt-movto_benefic.val_calcul_efp
                                   erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                   erros.parcelas                = 1
                                   erros.conteudo                = string(tt-movto_benefic.cdn_depend_func)
                                   v_log_permis   = no.
                            assign v_log_erros = yes.
                            NEXT BLOCO-BENEFICIO.
                        end.
                        find last depend_func no-lock where
                                 depend_func.cdn_empresa     = tt-movto_benefic.cdn_empresa and
                                 depend_func.cdn_estab       = tt-movto_benefic.cdn_estab and
                                 depend_func.cdn_funcionario = tt-movto_benefic.cdn_funcionario no-error.
                        if  not avail depend_func then do:
                            create erros.
                            {utp/ut-field.i dthrpyc movto_benefic cdn_depend_func 1}.
                            run utp/ut-msgs.p (input "msg",input 8271,input return-value).
                            assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                   erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                   erros.des_erro                = return-value
                                   erros.log_erro                = yes
                                   erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                   erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                   erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                   erros.num_digito_verfdor_func = int(0)
                                   erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                   erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                   erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                   erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                   erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                   erros.valor                   = tt-movto_benefic.val_calcul_efp
                                   erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                   erros.parcelas                = 1
                                   erros.conteudo                = string(tt-movto_benefic.cdn_funcionario)
                                   v_log_permis   = no.
                            assign v_log_erros = yes.
                            NEXT BLOCO-BENEFICIO.
                        end.
                    end.
                    /*   Se Separado   */
        
                    else do: 
                        find depend_func no-lock where
                             depend_func.cdn_empresa     = tt-movto_benefic.cdn_empresa and
                             depend_func.cdn_estab       = tt-movto_benefic.cdn_estab and
                             depend_func.cdn_funcionario = tt-movto_benefic.cdn_funcionario and
                             depend_func.cdn_depend_func = tt-movto_benefic.cdn_depend_func no-error.
                        if  not avail depend_func then do:
                            create erros.
                            {utp/ut-field.i dthrpyc movto_benefic cdn_depend_func 1}.
                            run utp/ut-msgs.p (input "msg",input 8270,input return-value).
                            assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                   erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                   erros.des_erro                = return-value
                                   erros.log_erro                = yes
                                   erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                   erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                   erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                   erros.num_digito_verfdor_func = int(0)
                                   erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                   erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                   erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                   erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                   erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                   erros.valor                   = tt-movto_benefic.val_calcul_efp
                                   erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                   erros.parcelas                = 1
                                   erros.conteudo                = string(tt-movto_benefic.cdn_depend_func)
                                   v_log_permis   = no.
                            assign v_log_erros = yes.
                            NEXT BLOCO-BENEFICIO.
                        end.
        
                        find regra_benefic_grau_depend no-lock where
                             regra_benefic_grau_depend.cdn_beneficio        = tt-beneficio.cdn_beneficio and
                             regra_benefic_grau_depend.cdn_regra_benefic    = tt-beneficio.cdn_regra_benefic AND
                             regra_benefic_grau_depend.cdn_grau_depend_func = depend_func.idi_grau_depen_func no-error.
                        if  not avail regra_benefic_grau_depend then do:
                            create erros.
                            {utp/ut-field.i dthrpyc regra_benefic_grau_depend cdn_grau_depend_func 1}.
                            run utp/ut-msgs.p (input "msg",input 56,input return-value).
                            assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                   erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                   erros.des_erro                = return-value
                                   erros.log_erro                = yes
                                   erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                   erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                   erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                   erros.num_digito_verfdor_func = int(0)
                                   erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                   erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                   erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                   erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                   erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                   erros.valor                   = tt-movto_benefic.val_calcul_efp
                                   erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                   erros.parcelas                = 1
                                   erros.conteudo                = string(tt-beneficio.cdn_beneficio)
                                   v_log_permis   = no.
                            assign v_log_erros = yes.
                            NEXT BLOCO-BENEFICIO.
                        end.
                        
                        /*    Valida data m�xima dependente para receber benef!cio   */
                        /*  Masculino  */  
                        if  depend_func.idi_sexo = 1 then do:
                            IF depend_func.idi_estado_saude = 2 THEN DO:
                                if  year(today) - year(depend_func.dat_nascimento) > regra_benefic_grau_depend.qtd_idade_max_depend_masc + regra_benefic_grau_depend.qti_acresc_tempo_invdez then do:
                                    create erros.
                                    run utp/ut-msgs.p (input "msg",input 8148,input return-value).
                                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                           erros.des_erro                = return-value
                                           erros.log_erro                = yes
                                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                           erros.num_digito_verfdor_func = int(0)
                                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                           erros.parcelas                = 1
                                           erros.conteudo                = string(year(today) - year(depend_func.dat_nascimento))
                                           v_log_permis   = no.
                                    assign v_log_erros = yes.
                                    NEXT BLOCO-BENEFICIO.
                                end.
                                else do:
                                    if  (year(today) - year(depend_func.dat_nascimento) = regra_benefic_grau_depend.qtd_idade_max_depend_masc + regra_benefic_grau_depend.qti_acresc_tempo_invdez) and
                                        (month(today) < month(depend_func.dat_nascimento)) then do:
                                        create erros.
                                        run utp/ut-msgs.p (input "msg",input 8148,input return-value).
                                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                               erros.des_erro                = return-value
                                               erros.log_erro                = yes
                                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                               erros.num_digito_verfdor_func = int(0)
                                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                               erros.parcelas                = 1
                                               erros.conteudo                = string(regra_benefic_grau_depend.qtd_idade_max_depend_masc + regra_benefic_grau_depend.qti_acresc_tempo_invdez)
                                               v_log_permis   = no.
                                        assign v_log_erros = yes.
                                        NEXT BLOCO-BENEFICIO.
                                    end.    
                                end.
                            END.
                            ELSE DO:
                                 if  year(today) - year(depend_func.dat_nascimento) > regra_benefic_grau_depend.qtd_idade_max_depend_masc then do:
                                     create erros.
                                     run utp/ut-msgs.p (input "msg",input 8148,input return-value).
                                     assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                            erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                            erros.des_erro                = return-value
                                            erros.log_erro                = yes
                                            erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                            erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                            erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                            erros.num_digito_verfdor_func = int(0)
                                            erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                            erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                            erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                            erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                            erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                            erros.valor                   = tt-movto_benefic.val_calcul_efp
                                            erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                            erros.parcelas                = 1
                                            erros.conteudo                = string(year(today) - year(depend_func.dat_nascimento))
                                            v_log_permis   = no.
                                     assign v_log_erros = yes.
                                     NEXT BLOCO-BENEFICIO.
                                 end.
                                 else do:
                                     if  (year(today) - year(depend_func.dat_nascimento) = regra_benefic_grau_depend.qtd_idade_max_depend_masc) and
                                         (month(today) < month(depend_func.dat_nascimento)) then do:
                                         create erros.
                                         run utp/ut-msgs.p (input "msg",input 8148,input return-value).
                                         assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                                erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                                erros.des_erro                = return-value
                                                erros.log_erro                = yes
                                                erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                                erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                                erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                                erros.num_digito_verfdor_func = int(0)
                                                erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                                erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                                erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                                erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                                erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                                erros.valor                   = tt-movto_benefic.val_calcul_efp
                                                erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                                erros.parcelas                = 1
                                                erros.conteudo                = string(regra_benefic_grau_depend.qtd_idade_max_depend_masc)
                                                v_log_permis   = no.
                                          assign v_log_erros = yes.
                                          NEXT BLOCO-BENEFICIO.
                                     end.    
                                 end.
                            END.
                        end. /* Se Masculino */
        
                        /*   Se feminino     */
                        else do:
                            IF depend_func.idi_estado_saude_depend = 2 THEN DO:
                                if  year(today) - year(depend_func.dat_nascimento) > regra_benefic_grau_depend.qtd_idade_max_depend_fem + regra_benefic_grau_depend.qti_acresc_tempo_invdez then do:
                                  create erros.
                                  run utp/ut-msgs.p (input "msg",input 8148,input return-value).
                                  assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                         erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                         erros.des_erro                = return-value
                                         erros.log_erro                = yes
                                         erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                         erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                         erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                         erros.num_digito_verfdor_func = int(0)
                                         erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                         erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                         erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                         erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                         erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                         erros.valor                   = tt-movto_benefic.val_calcul_efp
                                         erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                         erros.parcelas                = 1
                                         erros.conteudo                = string(year(today) - year(depend_func.dat_nascimento))
                                         v_log_permis   = no.
                                  assign v_log_erros = yes.
                                  NEXT BLOCO-BENEFICIO.
                                end.
                                else do:
                                   if  (year(today) - year(depend_func.dat_nascimento) = regra_benefic_grau_depend.qtd_idade_max_depend_fem + regra_benefic_grau_depend.qti_acresc_tempo_invdez) and
                                       (month(today) < month(depend_func.dat_nascimento)) then do:
                                       create erros.
                                       run utp/ut-msgs.p (input "msg",input 8148,input return-value).
                                       assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                              erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                              erros.des_erro                = return-value
                                              erros.log_erro                = yes
                                              erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                              erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                              erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                              erros.num_digito_verfdor_func = int(0)
                                              erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                              erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                              erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                              erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                              erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                              erros.valor                   = tt-movto_benefic.val_calcul_efp
                                              erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                              erros.parcelas                = 1
                                              erros.conteudo                = string(regra_benefic_grau_depend.qtd_idade_max_depend_fem + regra_benefic_grau_depend.qti_acresc_tempo_invdez)
                                              v_log_permis   = no.
                                       assign v_log_erros = yes.
                                       NEXT BLOCO-BENEFICIO.
                                   end.    
                                end.
                            END.
                            ELSE DO:
                                 if  year(today) - year(depend_func.dat_nascimento) > regra_benefic_grau_depend.qtd_idade_max_depend_fem then do:
                                   create erros.
                                   run utp/ut-msgs.p (input "msg",input 8148,input return-value).
                                   assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                          erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                          erros.des_erro                = return-value
                                          erros.log_erro                = yes
                                          erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                          erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                          erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                          erros.num_digito_verfdor_func = int(0)
                                          erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                          erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                          erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                          erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                          erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                          erros.valor                   = tt-movto_benefic.val_calcul_efp
                                          erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                          erros.parcelas                = 1
                                          erros.conteudo                = string(year(today) - year(depend_func.dat_nascimento))
                                          v_log_permis   = no.
                                   assign v_log_erros = yes.
                                   NEXT BLOCO-BENEFICIO.
                                 end.
                                 else do:
                                   if  (year(today) - year(depend_func.dat_nascimento) = regra_benefic_grau_depend.qtd_idade_max_depend_fem) and
                                       (month(today) < month(depend_func.dat_nascimento)) then do:
                                       create erros.
                                       run utp/ut-msgs.p (input "msg",input 8148,input return-value).
                                       assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                              erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                              erros.des_erro                = return-value
                                              erros.log_erro                = yes
                                              erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                              erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                              erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                              erros.num_digito_verfdor_func = int(0)
                                              erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                              erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                              erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                              erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                              erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                              erros.valor                   = tt-movto_benefic.val_calcul_efp
                                              erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                              erros.parcelas                = 1
                                              erros.conteudo                = string(regra_benefic_grau_depend.qtd_idade_max_depend_fem)
                                              v_log_permis   = no.
                                       assign v_log_erros = yes.
                                       NEXT BLOCO-BENEFICIO.
                                   end.    
                                 end.
                            END.
                        end. /* Se Feminino */
                    end. /* Se separado */
                end. /* tt-movto_benefic.cdn_depend_func > 0 */
        
               /* if  1 <= 0 then do:  
                    create erros.
                    {utp/ut-liter.i Parcelas MBS R}
                    run utp/ut-msgs.p (input "msg", 
                                       input 7938, 
                                       input return-value).
                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                           erros.des_erro                = return-value
                           erros.log_erro                = yes
                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                           erros.num_digito_verfdor_func = int(0)
                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                           erros.parcelas                = 1
                           erros.conteudo                = string(1)
                           v_log_permis   = no.
                    assign v_log_erros = yes.
                    NEXT BLOCO-BENEFICIO.
                end.
        */
                if  beneficio.idi_unid_medid_benefic = 1 /* Valor */ then do:
                    if  tt-movto_benefic.qtd_unid_acordo_efp <> 0 then do:
                        create erros.
                        {utp/ut-field.i dthrpyc movto_benefic qtd_unid_acordo_efp 1}.
                        run utp/ut-msgs.p (input "msg", 
                                           input 8135, 
                                           input return-value).
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-movto_benefic.qtd_unid_acordo_efp)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    end.
        
                    if  tt-movto_benefic.val_calcul_efp = 0 then do:
                        create erros.
                        {utp/ut-field.i dthrpyc movto_benefic val_calcul_efp 1}.
                        run utp/ut-msgs.p (input "msg", input 7938, input return-value).
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic                        
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-movto_benefic.val_calcul_efp)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    end.
                     
                end. /* beneficio.idi_unid_medid_benefic = 1 */
                ELSE DO:
                    if  beneficio.idi_unid_medid_benefic = 2 /* QUANTIDADE */ then do:
                        if  tt-movto_benefic.qtd_unid_acordo_efp = 0 then do:
                            create erros.
                            {utp/ut-field.i dthrpyc movto_benefic qtd_unid_acordo_efp 1}.
                            run utp/ut-msgs.p (input "msg", 
                                               input 7938, 
                                               input return-value).
                            assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                                   erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                                   erros.des_erro                = return-value
                                   erros.log_erro                = yes
                                   erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                                   erros.cdn_estab               = tt-movto_benefic.cdn_estab
                                   erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                                   erros.num_digito_verfdor_func = int(0)
                                   erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                                   erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                                   erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                                   erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                                   erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                                   erros.valor                   = tt-movto_benefic.val_calcul_efp
                                   erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                                   erros.parcelas                = 1
                                   erros.conteudo                = string(tt-movto_benefic.qtd_unid_acordo_efp)
                                   v_log_permis                  = no.
                            assign v_log_erros = yes.
                            NEXT BLOCO-BENEFICIO.
                        END.
 
                    END.
                END.
                if  beneficio.log_informa_prestdor_benefic = yes then do:
                    FIND regra_benefic_conven NO-LOCK WHERE
                         regra_benefic_conven.cdn_empresa       = tt-movto_benefic.cdn_empresa AND
                         regra_benefic_conven.cdn_beneficio     = tt-beneficio.cdn_beneficio AND
                         regra_benefic_conven.cdn_regra_benefic = tt-beneficio.cdn_regra_benefic AND
                         regra_benefic_conven.cdn_prestdor_serv = tt-beneficio.cdn_prestdor_serv NO-ERROR.
                    IF  NOT AVAIL regra_benefic_conven THEN DO:
                        create erros.
                        run utp/ut-msgs.p (input "msg", input 15907, input "":U).
                        assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                               erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                               erros.des_erro                = return-value
                               erros.log_erro                = yes
                               erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                               erros.cdn_estab               = tt-movto_benefic.cdn_estab
                               erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                               erros.num_digito_verfdor_func = int(0)
                               erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                               erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                               erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                               erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                               erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                               erros.valor                   = tt-movto_benefic.val_calcul_efp
                               erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                               erros.parcelas                = 1
                               erros.conteudo                = string(tt-beneficio.cdn_prestdor_serv)
                               v_log_permis   = no.
                        assign v_log_erros = yes.
                        NEXT BLOCO-BENEFICIO.
                    END.
                end.
        
 
        
                assign i-mes = tt-movto_benefic.num_mes_refer_movto_benefic + 1
                       i-ano = tt-movto_benefic.num_ano_refer_movto_benefic.
        
        
                if  i-mes > 12 then
                    assign i-mes = 1 
                           i-ano = i-ano + 1.
        
                assign i-dat-inic-mes = date(tt-movto_benefic.num_mes_refer_movto_benefic,01,tt-movto_benefic.num_ano_refer_movto_benefic)
                       i-dat-term-mes = date(i-mes,01,i-ano) - 1.
        
                find first excec_benefic_func no-lock where
                           excec_benefic_func.cdn_empresa     = tt-movto_benefic.cdn_empresa     and
                           excec_benefic_func.cdn_estab       = tt-movto_benefic.cdn_estab       and
                           excec_benefic_func.cdn_funcionario = tt-movto_benefic.cdn_funcionario and
                           excec_benefic_func.cdn_depend_func = tt-movto_benefic.cdn_depend_func and
                           excec_benefic_func.cdn_beneficio   = tt-beneficio.cdn_beneficio   and
                           excec_benefic_func.cdn_regra_benefic = tt-beneficio.cdn_regra_benefic AND
                           excec_benefic_func.dat_inic_excec_benefic_func <= i-dat-term-mes        and
                           excec_benefic_func.dat_term_excec_benefic_func >= i-dat-inic-mes no-error.
                if  avail excec_benefic_func then do:
                    create erros.
                    run utp/ut-msgs.p (input "msg",
                                       input 25718,
                                       input "":U).
                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                           erros.des_erro                = return-value
                           erros.log_erro                = yes
                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                           erros.num_digito_verfdor_func = int(0)
                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                           erros.quant                   = tt-movto_benefic.qtd_unid_acordo_efp
                           erros.parcelas                = 1
                           erros.conteudo                = string(tt-movto_benefic.val_calcul_efp)
                           v_log_permis   = no.
                    assign v_log_erros = yes.
                    NEXT BLOCO-BENEFICIO.
                end.
               
          
                do:  /* uma parcela*/
                    assign v_achou = no.
                    
                     if v_num_lote = 0 then do:
                     
                       find last  refer_movto_benefic   where
                             refer_movto_benefic.cdn_empresa                = tt-movto_benefic.cdn_empresa and
                             refer_movto_benefic.cdn_beneficio              = tt-beneficio.cdn_beneficio and
                             refer_movto_benefic.cdn_regra_benefic          = tt-beneficio.cdn_regra_benefic AND
                             refer_movto_benefic.num_mes_lote_movto_benefic = tt-movto_benefic.num_mes_refer_movto_benefic and
                             refer_movto_benefic.num_ano_lote_movto_benefic = tt-movto_benefic.num_ano_refer_movto_benefic and
                             refer_movto_benefic.num_lote_movto_benefic     > v_num_lote no-lock no-error.
                             
                        if avail refer_movto_benefic then  v_num_lote =  refer_movto_benefic.num_lote_movto_benefic + 1.
                        else   
                          v_num_lote = 1.
                             
                             
                    end.

                    
 
                    find refer_movto_benefic exclusive-lock where
                         refer_movto_benefic.cdn_empresa                = tt-movto_benefic.cdn_empresa and
                         refer_movto_benefic.cdn_beneficio              = tt-beneficio.cdn_beneficio and
                         refer_movto_benefic.cdn_regra_benefic          = tt-beneficio.cdn_regra_benefic AND
                         refer_movto_benefic.num_mes_lote_movto_benefic = tt-movto_benefic.num_mes_refer_movto_benefic and
                         refer_movto_benefic.num_ano_lote_movto_benefic = tt-movto_benefic.num_ano_refer_movto_benefic and
                         refer_movto_benefic.num_lote_movto_benefic     = v_num_lote no-error.
                    if  not avail refer_movto_benefic then do:
                        create refer_movto_benefic.
                        assign refer_movto_benefic.cdn_empresa                   = tt-movto_benefic.cdn_empresa
                               refer_movto_benefic.cdn_beneficio                 = tt-beneficio.cdn_beneficio
                               refer_movto_benefic.cdn_regra_benefic             = tt-beneficio.cdn_regra_benefic
                               refer_movto_benefic.num_ano_lote_movto_benefic    = tt-movto_benefic.num_ano_refer_movto_benefic
                               refer_movto_benefic.num_mes_lote_movto_benefic    = tt-movto_benefic.num_mes_refer_movto_benefic
                               refer_movto_benefic.num_lote_movto_benefic        = v_num_lote
                               refer_movto_benefic.cdn_prestdor_serv             = tt-beneficio.cdn_prestdor_serv
                               refer_movto_benefic.cdn_motiv_lote_benefic        = tt-refer_movto_benefic.cdn_motiv_lote_benefic.
                       &if "{&dthrpyc_dbtype}" <> "progress" &then
                           validate refer_movto_benefic no-error.
                       &endif
                    end.      
                    else assign v_achou = yes.

                          
                    find last b_movto_benefic no-lock where
                              b_movto_benefic.cdn_empresa     = tt-movto_benefic.cdn_empresa     and
                              b_movto_benefic.cdn_estab       = tt-movto_benefic.cdn_estab       and
                              b_movto_benefic.cdn_funcionario = tt-movto_benefic.cdn_funcionario and
                              b_movto_benefic.num_seq_movto_benefic <= 9999999 no-error.

                    create movto_benefic.
                    assign movto_benefic.cdn_empresa                 = tt-movto_benefic.cdn_empresa
                           movto_benefic.cdn_estab                   = tt-movto_benefic.cdn_estab
                           movto_benefic.cdn_funcionario             = tt-movto_benefic.cdn_funcionario               
                           movto_benefic.cdn_depend_func             = tt-movto_benefic.cdn_depend_func
                           movto_benefic.cdn_beneficio               = tt-beneficio.cdn_beneficio
                           movto_benefic.cdn_regra_benefic           = tt-beneficio.cdn_regra_benefic
                           movto_benefic.num_mes_refer_movto_benefic = tt-movto_benefic.num_mes_refer_movto_benefic
                           movto_benefic.num_ano_refer_movto_benefic = tt-movto_benefic.num_ano_refer_movto_benefic  
                           movto_benefic.num_mes_lote_movto_benefic  = tt-movto_benefic.num_mes_refer_movto_benefic
                           movto_benefic.num_ano_lote_movto_benefic  = tt-movto_benefic.num_ano_refer_movto_benefic
                           movto_benefic.val_calcul_efp              = tt-movto_benefic.val_calcul_efp
                           movto_benefic.qtd_unid_acordo_efp         = tt-movto_benefic.qtd_unid_acordo_efp
                           movto_benefic.cdn_formul_calc             = tt-movto_benefic.cdn_formul_calc_benefic
                           movto_benefic.cod_rh_ccusto               = funcionario.cod_rh_ccusto
                           movto_benefic.dat_ocor_movto_benefic      = tt-movto_benefic.dat_ocor_movto_benefic
                           movto_benefic.cod_docto_movto_benefic     = tt-movto_benefic.cod_docto_movto_benefic
                           movto_benefic.dat_pagto_efet_efp          = tt-movto_benefic.dat_pagto_efet_efp              
                           movto_benefic.num_lote_movto_benefic      = v_num_lote
                           movto_benefic.num_seq_movto_benefic_orig  = movto_benefic.num_seq_movto_benefic
                           movto_benefic.val_origin_movto_benefic    = movto_benefic.val_calcul_efp
                           movto_benefic.cdn_motiv_lote_benefic      = tt-refer_movto_benefic.cdn_motiv_lote_benefic
                           movto_benefic.cdn_prestdor_serv           = tt-movto_benefic.cdn_prestdor_serv
                           movto_benefic.qti_compos_benefic[1]       = 1
                           movto_benefic.val_compos_benefic[1]       = tt-movto_benefic.val_calcul_efp / tt-movto_benefic.qtd_unid_acordo_efp
                           movto_benefic.qti_dias_concedid           = tt-movto_benefic.qtd_unid_acordo_efp
                           movto_benefic.cdn_local_pagto             = funcionario.cdn_local_pagto.

                     
                    IF c-per-ini <> " " THEN
                         ASSIGN movto_benefic.dat_inic_period   = DATE(INT(SUBSTRING(c-per-ini,3,2)), INT(SUBSTRING(c-per-ini,1,2)), INT(SUBSTRING(c-per-ini,5,4)))
                                movto_benefic.dat_fim_period    = DATE(INT(SUBSTRING(c-per-fim,3,2)), INT(SUBSTRING(c-per-fim,1,2)), int(SUBSTRING(c-per-fim,5,4))).
                    
                    if  beneficio.log_informa_prestdor_benefic = yes then do:
                        assign movto_benefic.cdn_prestdor_serv = tt-beneficio.cdn_prestdor_serv.
                        if  avail regra_benefic_conven then
                            assign movto_benefic.log_pago_pelo_sist = regra_benefic_conven.log_pago_pelo_sist.
                    end.
        
                    if avail b_movto_benefic then
                       assign movto_benefic.num_seq_movto_benefic =
                              b_movto_benefic.num_seq_movto_benefic + 1.
                    else
                       assign movto_benefic.num_seq_movto_benefic = 1.
/*
                    create erros.
                    assign erros.num_cont                = tt-movto_benefic.num_seq_movto_benefic
                           erros.num_reg                 = tt-movto_benefic.num_seq_movto_benefic
                           erros.des_erro                = "Importacao realizada com sucesso."
                           erros.log_erro                = no
                           erros.cdn_empresa             = tt-movto_benefic.cdn_empresa
                           erros.cdn_estab               = tt-movto_benefic.cdn_estab
                           erros.cdn_funcionario         = tt-movto_benefic.cdn_funcionario
                           erros.num_digito_verfdor_func = int(0)
                           erros.cdn_depend_func         = tt-movto_benefic.cdn_depend_func
                           erros.cdn_beneficio           = tt-beneficio.cdn_beneficio
                           erros.cdn_regra_benefic       = tt-beneficio.cdn_regra_benefic
                           erros.num_mes_folha           = tt-movto_benefic.num_mes_refer_movto_benefic
                           erros.num_ano_folha           = tt-movto_benefic.num_ano_refer_movto_benefic
                           erros.valor                   = tt-movto_benefic.val_calcul_efp
                           erros.quant                   = movto_benefic.qtd_unid_acordo_efp
                           erros.parcelas                = 1
                           erros.conteudo                = "".
  */                  
                    if  v_achou = yes then do:
                        assign refer_movto_benefic.qti_infor_lancto_lote_benefic = refer_movto_benefic.qti_infor_lancto_lote_benefic + 1
                               refer_movto_benefic.qti_infor_lote_benefic        = refer_movto_benefic.qti_infor_lote_benefic + movto_benefic.qtd_unid_acordo_efp
                               refer_movto_benefic.val_infor_lote_benefic        = refer_movto_benefic.val_infor_lote_benefic + movto_benefic.val_calcul_efp
                               refer_movto_benefic.vli_soma_lancto_lote_benefic  = refer_movto_benefic.qti_infor_lancto_lote_benefic
                               refer_movto_benefic.qtd_soma_quant_lote_benefic   = refer_movto_benefic.qti_infor_lote_benefic
                               refer_movto_benefic.val_soma_val_lote_benefic     = refer_movto_benefic.val_infor_lote_benefic.
                    end.
                    else do:
                        assign refer_movto_benefic.qti_infor_lancto_lote_benefic =  1
                               refer_movto_benefic.qti_infor_lote_benefic        = movto_benefic.qtd_unid_acordo_efp
                               refer_movto_benefic.val_infor_lote_benefic        = movto_benefic.val_calcul_efp
                               refer_movto_benefic.vli_soma_lancto_lote_benefic  = 1
                               refer_movto_benefic.qtd_soma_quant_lote_benefic   = movto_benefic.qtd_unid_acordo_efp
                               refer_movto_benefic.val_soma_val_lote_benefic     = movto_benefic.val_calcul_efp.
                    end.
                    validate refer_movto_benefic.
                    validate movto_benefic.
                    assign movto_benefic.num_seq_movto_benefic_orig = movto_benefic.num_seq_movto_benefic.
                    ASSIGN v_row_movto_benefic = ROWID(movto_benefic).
                    RELEASE movto_benefic.
                    FIND movto_benefic NO-LOCK WHERE
                        ROWID(movto_benefic) = v_row_movto_benefic NO-ERROR.
                end. /* 1 = 1 (Uma parcela) */
            end. 
        END.
    END.  /*beneficios*/


            
                   
                   
end.                   

END.  /*l-gera-arq not marcado*/

     run pi-finalizar in h-acomp.

     i-pos-arq = INDEX(tt-param.arq-destino,".").

     c-arq-emp = SUBSTRING(tt-param.arq-destino,1,i-pos-arq - 1) + "-" + STRING(empresa.ep-codigo) + SUBSTRING(tt-param.arq-destino,i-pos-arq,4).

     if l-gera-arq:checked in frame f-pg-par then 
         OS-COPY value(c-arquivo_1)  value(c-arq-emp).
    
     os-delete value(c-arquivo_1).

   output to v:\temp\esbs0011-erros.txt no-convert  .
   for each erros no-lock break by erros.num_reg:
           find funcionario no-lock where
                funcionario.cdn_empresa     = erros.cdn_empresa and
                funcionario.cdn_estab       = erros.cdn_estab   and
                funcionario.cdn_funcionario = erros.cdn_funcionario no-error.
           if avail funcionario then do:
              if v_log_folha_educnal then
                 assign v_cod_matr = string(funcionario.cdn_func_centrdor,"zzzzz9") + "/" + string(funcionario.cdn_tip_contrat_func,"99").
              else
                 assign v_cod_matr = string(funcionario.cdn_funcionario,"zzzzzzz9").
           end.
           else 
               assign v_cod_matr = string(erros.cdn_funcionario).

           assign v_cod_matr = fill(" ",8 - length(v_cod_matr,("character"))) + v_cod_matr.

           disp erros.cdn_empresa
                erros.cdn_estab
                v_cod_matr
                erros.num_digito_verfdor_func
                erros.cdn_depend_func
                erros.cdn_beneficio
                erros.cdn_regra_benefic
                erros.num_mes_folha
                erros.num_ano_folha
                erros.valor
                erros.quant
                erros.parcelas
                erros.conteudo
                erros.des_erro with frame f-dados.
           down with frame f-dados.
           if line-counter >= 63 then page.
       
   
   end.

   output close.    
  

 
 


/*    {include/i-imrun.i prghur/fpp/esbs0011rp.p}

    {include/i-imexc.i}

    if  session:set-wait-state("") then.
    
    {include/i-imtrm.i tt-param.arq-destino tt-param.destino}
    
    */
   
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-consulta-antes-competencia C-Win 
PROCEDURE pi-consulta-antes-competencia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def buffer b-movto_unimed for movto_unimed.
def var i-qtde as integer Label "Cons.Ano" no-undo.
def var d-valor as dec format ">>>,>>>,>>9.99" label "Valor complem" no-undo.

 
         

c-proc = "consulta".   /* codigos de consultas*/
 
    /*limpata temp table a cada novo dependente*/     
     EMPTY TEMP-TABLE tt-row.


    /*vai guardar todas consultas de 3 exercicios deste dependente das competencias anteriores*/
 
    

    for each
        b-movto_unimed where 
        b-movto_unimed.cdn_depend_func          = movto_unimed.cdn_depend_func and
        b-movto_unimed.cdn_empresa              = movto_unimed.cdn_empresa  and
        b-movto_unimed.cdn_estab                = movto_unimed.cdn_estab and
        b-movto_unimed.dt_competencia           < dt-referencia-ini and
        b-movto_unimed.cdn_funcionario          = movto_unimed.cdn_funcionario and
        b-movto_unimed.dt_competencia           >= date(1,1,year(dt-referencia-ini) - 3)  and
        index(b-movto_unimed.tipo_servico,"consulta") > 0   no-lock.
 
    
            find first tt-row where tt-row.r-movto = rowid(b-movto_unimed) no-error.
                if not avail tt-row then do:
                   
                   create tt-row.
                   assign tt-row.r-movto = rowid(b-movto_unimed)
                          tt-row.ano     = year(b-movto_unimed.dt_utilizacao).
                end.
              
    end.
    

               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-consultas C-Win 
PROCEDURE pi-consultas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF var d-valor as dec format ">>>,>>>,>>9.99" label "Valor complem" no-undo.
def buffer b-movto_unimed for movto_unimed.
def var i-qtde as integer Label "Cons.Ano" no-undo.
def var d-perc as dec no-undo.
 
         

    c-proc = "consulta".   /* codigos de consultas*/
 
     
    i-qtde = 1.  /* comeca com 1 porque j� considera mais este que este em leitura*/

    /*vai contar quantos tem at� agora no exercicio*/
     FOR EACH tt-row where tt-row.ano     = year(movto_unimed.dt_utilizacao) NO-LOCK.
           i-qtde = i-qtde + 1.  
     end.
    
   
     /*guarda o que esta em leitura para ser considerados pelos proximos*/
     find first tt-row where tt-row.r-movto = rowid(movto_unimed) no-error.
                if not avail tt-row then do:
                   
                   create tt-row.
                   assign tt-row.r-movto = rowid(movto_unimed)
                          tt-row.ano     = year(movto_unimed.dt_utilizacao).
                end.
     
       
   
        
    assign d-perc = 20.
    
    if i-qtde > 5 and i-qtde <= 9  then d-perc =  30 .  /* regra mais de 5 e menos de 9 consultas 30% ou seja mais 10%, pois j� paga 20%*/
    if i-qtde > 9                  then d-perc =  40 .  /* regra mais de 9 consultas 40% ou seja mais 20%, pois j� paga 20%*/
     
    /* estou usando 2 truncates para diminuir diferen�a possivel pelo calculo dos 20% na formula do calculo da folha*/
    
    movto_unimed.vl_funcionario =  truncate((d-perc - 20) * movto_unimed.vl_total / 100,2) + truncate(20 * movto_unimed.vl_total / 100,2).    
    movto_unimed.vl_excedente     =  truncate((d-perc - 20) * movto_unimed.vl_total / 100,2).       
       

               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-linha C-Win 
PROCEDURE pi-grava-linha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
PUT STREAM s-saida
               
        chr(34) FORMAT "x(01)"
        constante-2  
        nr-reg       
        empresa      
        estabelec    
        matricula    
        branco       
        dependente   
        beneficio    
        regr-benef    
        mes-refer    
        ano-refer    
        valor        
        quantidade   
        parcelas     
        formula      
        prest-serv   
        /*dia-ocorr    
        mes-ocorr    
        ano-ocorr    */
        ocorr
        documento    
        mot-lot-benef
        dia-pag-efet 
        mes-pag-efet 
        ano-pag-efet 
        per-ini      
        per-fim      
        chr(34) FORMAT "x(01)" SKIP.
        
        find tt-refer_movto_benefic exclusive-lock where
              tt-refer_movto_benefic.cdn_empresa                = string(empresa) and
              tt-refer_movto_benefic.cdn_beneficio              = beneficio and
              tt-refer_movto_benefic.cdn_regra_benefic          = regr-benef AND
              tt-refer_movto_benefic.num_mes_lote_movto_benefic = mes-refer and
              tt-refer_movto_benefic.num_ano_lote_movto_benefic = ano-refer and
              tt-refer_movto_benefic.num_lote_movto_benefic     = 1 no-error.
         if  not avail tt-refer_movto_benefic then do: 
             create tt-refer_movto_benefic.
             assign tt-refer_movto_benefic.cdn_empresa                   = string(empresa)
                    tt-refer_movto_benefic.cdn_beneficio                 = beneficio
                    tt-refer_movto_benefic.cdn_regra_benefic             = regr-benef
                    tt-refer_movto_benefic.num_ano_lote_movto_benefic    = ano-refer
                    tt-refer_movto_benefic.num_mes_lote_movto_benefic    = mes-refer
                    tt-refer_movto_benefic.num_lote_movto_benefic        = 1
                    tt-refer_movto_benefic.cdn_prestdor_serv             = prest-serv
                    tt-refer_movto_benefic.cdn_motiv_lote_benefic        = mot-lot-benef.
          end.                    
                    nr-seq = nr-seq + 1.
                    


                            create tt-movto_benefic.
                            assign tt-movto_benefic.num_seq_movto_benefic       =  nr-seq
                                   tt-movto_benefic.cdn_empresa                 = string(tt-refer_movto_benefic.cdn_empresa)
                                   tt-movto_benefic.cdn_estab                   = string(estabelec)
                                   tt-movto_benefic.cdn_funcionario             = matricula               
                                   tt-movto_benefic.cdn_depend_func             = dependente
                                   tt-movto_benefic.cdn_beneficio               = tt-refer_movto_benefic.cdn_beneficio
                                   tt-movto_benefic.cdn_regra_benefic           = tt-refer_movto_benefic.cdn_regra_benefic
                                   tt-movto_benefic.num_mes_refer_movto_benefic = tt-refer_movto_benefic.num_mes_lote_movto_benefic
                                   tt-movto_benefic.num_ano_refer_movto_benefic = tt-refer_movto_benefic.num_ano_lote_movto_benefic  
                                   tt-movto_benefic.val_calcul_efp              = valor / 100
                                   tt-movto_benefic.qtd_unid_acordo_efp         = quantidade / 1000
                                   tt-movto_benefic.cdn_formul_calc             = formula
                                   tt-movto_benefic.cod_rh_ccusto               = funcionario.cod_rh_ccusto
                                   tt-movto_benefic.dat_ocor_movto_benefic      = dt-utilizacao    
            
                                   tt-movto_benefic.cod_docto_movto_benefic     = "0"
                                   tt-movto_benefic.dat_pagto_efet_efp          = ?
                                   tt-movto_benefic.num_lote_movto_benefic      = tt-refer_movto_benefic.num_lote_movto_benefic
                                   tt-movto_benefic.num_mes_lote_movto_benefic  = tt-refer_movto_benefic.num_mes_lote_movto_benefic
                                   tt-movto_benefic.num_ano_lote_movto_benefic  = tt-refer_movto_benefic.num_ano_lote_movto_benefic
                                   tt-movto_benefic.val_origin_movto_benefic    = tt-movto_benefic.val_calcul_efp
                                   tt-movto_benefic.cdn_motiv_lote_benefic      = tt-refer_movto_benefic.cdn_motiv_lote_benefic
                                   tt-movto_benefic.cdn_prestdor_serv           = tt-refer_movto_benefic.cdn_prestdor_serv
                                   tt-movto_benefic.qti_compos_benefic[1]       = 1
                                   tt-movto_benefic.val_compos_benefic[1]       = tt-movto_benefic.val_calcul_efp / tt-movto_benefic.qtd_unid_acordo_efp
                                   tt-movto_benefic.qti_dias_concedid           = tt-movto_benefic.qtd_unid_acordo_efp
                                   tt-movto_benefic.cdn_local_pagto             = funcionario.cdn_local_pagto.


            assign tt-refer_movto_benefic.qti_infor_lancto_lote_benefic = tt-refer_movto_benefic.qti_infor_lancto_lote_benefic + 1
                                       tt-refer_movto_benefic.qti_infor_lote_benefic        = tt-refer_movto_benefic.qti_infor_lote_benefic + tt-movto_benefic.qtd_unid_acordo_efp
                                       tt-refer_movto_benefic.val_infor_lote_benefic        = tt-refer_movto_benefic.val_infor_lote_benefic + tt-movto_benefic.val_calcul_efp
                                       tt-refer_movto_benefic.vli_soma_lancto_lote_benefic  = tt-refer_movto_benefic.qti_infor_lancto_lote_benefic
                                       tt-refer_movto_benefic.qtd_soma_quant_lote_benefic   = tt-refer_movto_benefic.qti_infor_lote_benefic
                                       tt-refer_movto_benefic.val_soma_val_lote_benefic     = tt-refer_movto_benefic.val_infor_lote_benefic.
                         
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina C-Win 
PROCEDURE pi-troca-pagina :
/*:T------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P�gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{include/i-imtrp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_tira_acento C-Win 
PROCEDURE pi_tira_acento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  Assign c-byte = "".
  Do idx = 1 To Length(c-campo).
      Assign achou = No.
      Do idx1 = 1 To 23:
         If Substr(c-campo,idx,1) = c-acento[idx1] Then
         Do:
             Assign c-byte = c-byte  + c-acento1[idx1]
                     achou = Yes.
             Leave.
         End.
      End.
      If Not achou Then
        Assign c-byte = c-byte  + Substr(c-campo,idx,1).
  End.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records C-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-impor, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed C-Win 
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




