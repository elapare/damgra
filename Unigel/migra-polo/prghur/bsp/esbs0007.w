&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESBS0007 1.0.00.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

&GLOBAL-DEFINE PGLAY f-pg-lay
&GLOBAL-DEFINE PGSEL f-pg-sel

&GLOBAL-DEFINE PGLOG f-pg-log
&GLOBAL-DEFINE PGPAR f-pg-par

/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer.
    


/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE c-cpf AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-procedimento AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dt-aux AS DATE        NO-UNDO.
DEFINE VARIABLE dt-referencia-ini AS DATE        NO-UNDO.
DEFINE VARIABLE dt-referencia-fim AS DATE        NO-UNDO.
def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-arq-term         as char    no-undo.
DEFINE VARIABLE l-duplicado AS LOGICAL     NO-UNDO.
{include/i-imdef.i}

DEFINE VARIABLE DT-REF AS DATE       NO-UNDO.

DEFINE BUFFER b-funcionario FOR funcionario.
define buffer bb-funcionario for funcionario.

/* define  temp-table tipo de registro 0*/

    /* define  temp-table tipo de registro 2*/

    def temp-table tt-registro-0 NO-UNDO
    field dt-poste-comp as DATE  
    field dt-comp       as DATE  
    field tipo-extrato  as CHARACTER.

define temp-table tt-cpf
field cpf-tit as char  format "x(11)"
field cpf-dep as char  format "x(11)"
FIELD cdn_depend_func  LIKE depend_func.cdn_depend_func 
field cdn_empresa like depend_func.cdn_empresa
field cdn_estab   like depend_func.cdn_estab
field cdn_funcionario like depend_func.cdn_funcionario
 
index codigo is primary unique
        cpf-tit
        cpf-dep
         
index dep
        cpf-dep
        cpf-tit .


define temp-table tt-MOV
field cdn_empresa like depend_func.cdn_empresa
field cdn_estab   like depend_func.cdn_estab
field cdn_funcionario like depend_func.cdn_funcionario
FIELD cdn_depend_func  LIKE depend_func.cdn_depend_func 
FIELD valor  AS DEC
FIELD qtde  AS DEC

field cod as integer
 
index codigo is primary unique
       cdn_empresa  
       cdn_estab 
       cdn_funcionario
       cdn_depend_func
       cod.




/* define  temp-table tipo de registro 1*/

def temp-table tt-registro-1 NO-UNDO
    field c-empresa         AS CHARACTER
    field c-cgc             AS CHARACTER
    field c-moeda           AS CHARACTER
    field c-titular         AS CHARACTER
    field c-cpf             AS CHARACTER
    field c-componente      AS CHARACTER
    field c-matricula       AS CHARACTER
    field dt-inicio         AS DATE
    field dt-fim            AS DATE
    field dt-emissao        AS DATE
    field c-matricula-espec AS CHAR
    field i-subfatura       AS INTEGER.


def temp-table tt-registro-2 NO-UNDO
    field dt-poste-comp                 as DATE  
    field dt-comp                       as DATE  
    field tipo-extrato                  as CHARACTER
    field c-empresa                     AS CHARACTER
    field c-cgc                         AS CHARACTER
    field c-moeda                       AS CHARACTER
    FIELD c-titular                               AS CHAR  FORMAT "x(40)" 
    field c-cpf                         AS CHARACTER
    field c-componente                  AS CHARACTER
    field c-matricula                   AS CHARACTER
    field dt-inicio                     AS DATE
    field dt-fim                        AS DATE
    field dt-emissao                    AS DATE
    field c-matricula-espec             AS CHAR
    field i-subfatura                   AS INTEGER
    field cdn_empresa                 LIKE movto_bradesco.cdn_empresa 
    field cdn_estab                   LIKE movto_bradesco.cdn_estab 
    field cdn_funcionario             LIKE movto_bradesco.cdn_funcionario
    field cdn_depend_func             LIKE movto_bradesco.cdn_depend_func_bradesco
    field cdn_depend_func_bradesco    LIKE movto_bradesco.cdn_depend_func_bradesco
    field dt-utilizacao               AS DATE
    field c-nome-usuario              AS CHARACTER
    field c-nome-prestador            AS CHARACTER
    field c-tipo-m-utilizado          AS CHARACTER
    field c-cpf-prestador             AS CHARACTER
    field c-tipo-servico              AS CHARACTER
    field i-numero-sr                 AS INTEGER
    field vl-original                 AS DECIMAL
    field vl-reembolso                AS DECIMAL
    field vl-participacao             AS DECIMAL
    field vl-co-participacao          AS DECIMAL
    field c-documento                 AS CHARACTER
    field c-procedimento              AS integer
    field i-seq-procedimento          AS INTEGER
    field i-nr-certificado            AS INTEGER.
    


/* definiá‰es */
DEFINE VARIABLE dt-comp AS DATE       NO-UNDO.
DEF VAR c-arq             AS CHAR                NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR                NO-UNDO.   
DEF VAR i-linha           AS INT                 NO-UNDO.
DEF VAR i-cod             AS INT                 NO-UNDO.

DEF VAR c-coluna          AS char                NO-UNDO.
DEFINE VARIABLE cpf-atual AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-titular AS CHARACTER   NO-UNDO.
def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.
DEF VAR c-arquivo AS CHAR NO-UNDO.

DEFINE VARIABLE i-t1 AS INTEGER    NO-UNDO.
def var h-acomp              as handle no-undo.


/* define layout*/

DEFINE VARIABLE constante      AS CHARACTER format "x(1)"        INITIAL ""         NO-UNDO.
DEFINE VARIABLE constante-2    AS CHARACTER format "x(8)"        INITIAL "mvtobene" NO-UNDO.
DEFINE VARIABLE nr-reg         AS INTEGER   FORMAT 99999                            NO-UNDO.   
DEFINE VARIABLE c-empresa        AS CHAR      FORMAT "x(03)"                        NO-UNDO. 
DEFINE VARIABLE c-estabelec      AS CHAR      FORMAT "x(05)"                        NO-UNDO.  
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
DEFINE VARIABLE documento      AS DEC       FORMAT 999999999999999                  NO-UNDO.            
DEFINE VARIABLE mot-lot-benef  AS INTEGER   FORMAT 999                              NO-UNDO.           
DEFINE VARIABLE dia-pag-efet   AS INTEGER   FORMAT 99                               NO-UNDO.          
DEFINE VARIABLE mes-pag-efet   AS INTEGER   FORMAT 99                               NO-UNDO.  
DEFINE VARIABLE ano-pag-efet   AS INTEGER   FORMAT 9999                             NO-UNDO.  
DEFINE VARIABLE per-ini        AS INTEGER   FORMAT 99999999                         NO-UNDO.  
DEFINE VARIABLE per-fim        AS INTEGER   FORMAT 99999999                         NO-UNDO.  
DEFINE VARIABLE constante-3    AS CHARACTER format "x(1)"        INITIAL ""         NO-UNDO. 
                               



DEF STREAM s-saida.


DEF VAR c-arquivo_1 AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR c-arquivo_2 AS CHAR FORMAT "x(50)" NO-UNDO.

DEF VAR c-arquivo_1a AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR c-arquivo_2a AS CHAR FORMAT "x(50)" NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-impor
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-import

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS im-pg-par bt-executar bt-cancelar  ~
bt-ajuda 

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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
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

DEFINE VARIABLE c-arquivo-saida AS CHARACTER INITIAL "v:/temp/consiste.txt" 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 53.72 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE c-estab-fim AS CHAR FORMAT "x(05)":U INITIAL "424" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-estab-ini AS CHAR FORMAT "x(05)":U INITIAL "422" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE i-ano-ref AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Ano Folha" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 TOOLTIP "Ano Competància Folha" NO-UNDO.

DEFINE VARIABLE i-cdn_funcionario-fim AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 9999999 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE i-cdn_funcionario-ini AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     LABEL "Funcion†rio" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE i-empresa-fim AS CHAR FORMAT "x(03)":U INITIAL "420" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE i-empresa-ini AS CHAR FORMAT "x(03)":U INITIAL "420" 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE i-mes-ref AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Màs Folha" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Màs Competància da Folha para movimentos importados" NO-UNDO.

DEFINE VARIABLE text-entrada AS CHARACTER FORMAT "X(256)":U INITIAL "Arquivo de Entrada" 
      VIEW-AS TEXT 
     SIZE 19 BY .63 NO-UNDO.

/*DEFINE VARIABLE text-entrada-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Arquivo de sa°da" 
      VIEW-AS TEXT 
     SIZE 19 BY .63 NO-UNDO.
*/
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
     
DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 2.

     
/*
DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 2.

DEFINE VARIABLE tg-simula AS LOGICAL INITIAL no 
     LABEL "Somente Simulaá∆o de importaá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 43.57 BY .83 NO-UNDO.
*/

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-lay
     ed-layout AT ROW 1 COL 1 NO-LABEL
     bt-editar AT ROW 10.38 COL 1 HELP
          "Dispara a Impress∆o do Layout"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.46.

DEFINE FRAME f-pg-log
     rs-todos AT ROW 2.25 COL 3.29 NO-LABEL
     rs-destino AT ROW 4.5 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-config-impr-destino AT ROW 5.71 COL 43.29 HELP
          "Configuraá∆o da impressora"
     bt-arquivo-destino AT ROW 5.71 COL 43.29 HELP
          "Escolha do nome do arquivo"
     c-arquivo-destino AT ROW 5.75 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 7.88 COL 3.14 HELP
          "Modo de Execuá∆o" NO-LABEL
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
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Cancelar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     im-pg-lay AT ROW 1.5 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     im-pg-sel AT ROW 1.5 COL 17.86
     rt-folder-top AT ROW 2.54 COL 2.14
     im-pg-log AT ROW 1.5 COL 49.29
     RECT-1 AT ROW 14.29 COL 2
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     im-pg-par AT ROW 1.5 COL 33.57
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
     i-empresa-ini AT ROW 1.63 COL 18.43 COLON-ALIGNED
     i-empresa-fim AT ROW 1.63 COL 40.72 COLON-ALIGNED NO-LABEL
     c-estab-ini AT ROW 2.71 COL 18.43 COLON-ALIGNED
     c-estab-fim AT ROW 2.71 COL 40.72 COLON-ALIGNED NO-LABEL
     i-cdn_funcionario-ini AT ROW 3.79 COL 18.43 COLON-ALIGNED
     i-cdn_funcionario-fim AT ROW 3.79 COL 40.72 COLON-ALIGNED NO-LABEL
     i-mes-ref AT ROW 4.83 COL 18.43 COLON-ALIGNED /*WIDGET-ID 2*/
     i-ano-ref AT ROW 4.83 COL 48.72 COLON-ALIGNED /*WIDGET-ID 4*/

     c-arquivo-entrada AT ROW 7.71 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     bt-arquivo-entrada AT ROW 7.71 COL 57.14 HELP
          "Escolha do nome do arquivo"
     c-arquivo-saida AT ROW 9.88 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     bt-arquivo-saida AT ROW 9.88 COL 57.14 HELP
          "Escolha do nome do arquivo" /*WIDGET-ID 10 */
     text-entrada AT ROW 6.67 COL 4.14 NO-LABEL
/*     text-entrada-2 AT ROW 8.83 COL 4.14 NO-LABEL*/
     IMAGE-1 AT ROW 1.63 COL 35.29
     IMAGE-2 AT ROW 1.63 COL 39.86
     IMAGE-3 AT ROW 2.71 COL 35.29
     IMAGE-4 AT ROW 2.71 COL 39.86
     IMAGE-5 AT ROW 3.79 COL 35.29
     IMAGE-6 AT ROW 3.79 COL 39.86
     RECT-8 AT ROW 7 COL 2
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
         TITLE              = "Importaá∆o de Movimentos Unimed MTN"
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
                "Execuá∆o".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FILL-IN text-entrada IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-entrada:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Arquivo de Entrada".

/* SETTINGS FOR FILL-IN text-entrada-2 IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
   
   /*
ASSIGN 
       text-entrada-2:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Arquivo de Sa°da".
*/
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
ON END-ERROR OF C-Win /* Importaá∆o de Movimentos Unimed MTN */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Importaá∆o de Movimentos Unimed MTN */
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
    {include/i-imarq.i c-arquivo-entrada f-pg-par "'*.XLSX' '*.xlsx' ,'*.XLS' '*.xls' , 'todos' '*.*'"}
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
 
       
              
         run pi-executar.
               OUTPUT STREAM s-saida Close. 

        
         
       
       
       
       
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
ON LEAVE OF i-mes-ref IN FRAME f-pg-par /* Màs Folha */
DO:
  IF int(i-mes-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME}) < 1 OR 
      int(i-mes-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME}) > 12 THEN
      RETURN NO-APPLY.  
 /* ASSIGN
      dt-aux =  DATE(int(i-mes-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME}),01,int(i-ano-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME}))
      dt-referencia-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(dt-aux,"99/99/9999")
      dt-aux = dt-aux + 34
      dt-aux = DATE(MONTH(dt-aux),01,YEAR(dt-aux)) - 1 
      dt-referencia-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(dt-aux,"99/99/9999").
   */
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

/*{utp/ut9000.i "ESBS0007" "1.00.00.000"}
  */
/*:T inicializaá‰es do template de importaá∆o */
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

    RUN enable_UI.

    ASSIGN 
       dt-aux = TODAY
       dt-aux = DATE(MONTH(dt-aux),01,YEAR(dt-aux)) - 1 
       dt-aux = DATE(MONTH(dt-aux),01,YEAR(dt-aux)) 
    /*   dt-referencia-ini:SCREEN-VALUE IN FRAME f-pg-par = string(dt-aux,"99/99/9999")*/

       dt-aux = dt-aux + 34
       dt-aux = DATE(MONTH(dt-aux),01,YEAR(dt-aux)) - 1 
      /* dt-referencia-fim:SCREEN-VALUE IN FRAME f-pg-par = string(dt-aux,"99/99/9999").*/

       i-mes-ref:SCREEN-VALUE IN FRAME f-pg-par = string(MONTH(dt-aux),"99").
       i-ano-ref:SCREEN-VALUE IN FRAME f-pg-par = string(YEAR(dt-aux),"9999").

       c-arquivo-saida:visible in frame f-pg-par = no.
       bt-arquivo-saida:visible in frame f-pg-par = no.



    {include/i-immbl.i}


        
        APPLY 'mouse-select-click' to im-pg-par IN FRAME {&FRAME-NAME}.
    /*{include/i-imvrf.i &programa=ESBS0007 &versao-layout=001}*/
  
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
  ENABLE im-pg-par bt-executar bt-cancelar  bt-ajuda 
      WITH FRAME f-import IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-import}
  VIEW FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY ed-layout 
      WITH FRAME f-pg-lay IN WINDOW C-Win.
  ENABLE ed-layout bt-editar 
      WITH FRAME f-pg-lay IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-lay}
  DISPLAY rs-todos rs-destino c-arquivo-destino rs-execucao 
      WITH FRAME f-pg-log IN WINDOW C-Win.
  ENABLE RECT-11 RECT-7 RECT-9 rs-todos rs-destino bt-config-impr-destino 
         bt-arquivo-destino c-arquivo-destino rs-execucao 
      WITH FRAME f-pg-log IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-log}
  DISPLAY i-empresa-ini i-empresa-fim c-estab-ini c-estab-fim 
          i-cdn_funcionario-ini i-cdn_funcionario-fim i-mes-ref i-ano-ref 
           c-arquivo-entrada c-arquivo-saida 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 
         i-empresa-ini i-empresa-fim c-estab-ini c-estab-fim 
         i-cdn_funcionario-ini i-cdn_funcionario-fim i-mes-ref i-ano-ref 
         c-arquivo-entrada bt-arquivo-entrada c-arquivo-saida 
         bt-arquivo-saida 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-close-excel C-Win 
PROCEDURE pi-close-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    c-planilha:CLOSE().


    RELEASE OBJECT c-relatorio.
    RELEASE OBJECT c-planilha.
    RELEASE OBJECT c-excel.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-planilha C-Win 
PROCEDURE pi-cria-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /*  assign c-planilha  = c-excel:Workbooks:OPEN(c-modelo-planilha)
               c-relatorio = c-excel:Sheets:item(1)
               c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo.
        
        c-arquivo = c-arq + 'pr02' + STRING(time)+ '.xls'.*/
    /*c-arquivo = SESSION:TEMP-DIRECTORY + "ESBS0007" + STRING(TIME) + ".XLS".

    OS-COPY VALUE(c-modelo-planilha) VALUE(c-arquivo).*/

    c-arquivo = c-modelo-planilha.

    assign c-planilha  = c-excel:Workbooks:OPEN(c-arquivo)
           c-relatorio = c-excel:Sheets:item(1)
           c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo.

    /* i-linha = 2.
     dt-comp       =  DATE("01/" + string(c-relatorio:range("D" + STRING(i-linha)):VALUE)).


    IF dt-comp < DATE(dt-referencia-ini:SCREEN-VALUE IN FRAME f-pg-par) OR
         dt-comp > DATE(dt-referencia-fim:SCREEN-VALUE IN FRAME f-pg-par)   THEN DO:



           run utp/ut-msgs.p (input "show", input 17006, input "Arquivo fora do per°odo de referància ou inv†lido.").

        c-planilha:CLOSE().


        RELEASE OBJECT c-relatorio.
        RELEASE OBJECT c-planilha.
        RELEASE OBJECT c-excel.

          RETURN "nok".
    END.

      */

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
    
    assign file-info:file-name = input frame f-pg-par c-arquivo-entrada.
    if file-info:pathname = ? and
       input frame f-pg-log rs-execucao = 1 then do:
        run utp/ut-msgs.p (input "show":U,
                           input 326,
                           input c-arquivo-entrada).                               
        apply 'mouse-select-click':U to im-pg-par in frame f-import.
        apply 'entry':U to c-arquivo-entrada in frame f-pg-par.                
        return error.
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
    /*:T Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na p†gina 
       com problemas e colocar o focus no campo com problemas             */    
                                         
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = 1 /*input frame f-pg-log rs-destino*/
           tt-param.todos           = 1
           tt-param.arq-entrada     = input frame f-pg-par c-arquivo-entrada
           tt-param.data-exec       = today
           tt-param.hora-exec       = time.

     
        assign tt-param.arq-destino = input frame f-pg-par c-arquivo-saida.

    /*:T Coloque aqui a l¢gica de gravaá∆o dos parÉmtros e seleá∆o na temp-table
       tt-param */ 

/*    {include/i-imexb.i}
  */
    if  session:set-wait-state("general":U) then.

                          

        
def var c-cpf-tit as char no-undo.
def var c-cpf-dep as char no-undo.

FOR EACH tt-cpf.
    DELETE tt-cpf.
END.

FOR EACH tt-mov.
    DELETE tt-mov.
END.
for each funcionario where 
              funcionario.cdn_empresa    >= (i-empresa-ini:SCREEN-VALUE IN FRAME f-pg-par) AND
              funcionario.cdn_empresa    <= (i-empresa-fim:SCREEN-VALUE IN FRAME f-pg-par) AND  
              funcionario.cdn_estab      >= (c-estab-ini:SCREEN-VALUE IN FRAME f-pg-par)   AND  
              funcionario.cdn_estab      <= (c-estab-fim:SCREEN-VALUE IN FRAME f-pg-par)   AND 
              funcionario.cdn_funcionario >=  INT(i-cdn_funcionario-ini:SCREEN-VALUE IN FRAME f-pg-par)   AND
              funcionario.cdn_funcionario <= INT(i-cdn_funcionario-fim:SCREEN-VALUE IN FRAME f-pg-par)  NO-LOCK.
     
     

      if      funcionario.dat_desligto_func <> ? and
              funcionario.dat_desligto_func + 365 < today then next.
       
      if      funcionario.dat_desligto_func <> ?  then do:
     
        find first bb-funcionario where 
                bb-funcionario.cod_id_feder = funcionario.cod_id_feder and
                bb-funcionario.dat_desligto_func = ? no-lock use-index fncnr_idfdemp no-error .
                
        if avail bb-funcionario then next.
        
      end.
     
      FIND  LAST sit_afast_func OF funcionario 
                        WHERE sit_afast_func.cdn_sit_afast_func = 30 NO-LOCK NO-ERROR.
    
      IF AVAIL sit_afast_func THEN NEXT.
      
     
      c-cpf-tit = funcionario.cod_id_feder.
      
      c-cpf-dep = c-cpf-tit.
      
      find first tt-cpf where 
            tt-cpf.cpf-tit =  c-cpf-tit and
            tt-cpf.cpf-dep =  c-cpf-tit no-error.
            
      if not avail tt-cpf then do:
         create tt-cpf.
         
         assign 
             tt-cpf.cpf-tit         =  c-cpf-tit 
             tt-cpf.cpf-dep         =  c-cpf-tit
             tt-cpf.cdn_estab           =  funcionario.cdn_estab  
             tt-cpf.cdn_empresa     =  funcionario.cdn_empresa    
             tt-cpf.cdn_funcionario = funcionario.cdn_funcionario 
             tt-cpf.cdn_depend_func  = 0 .
         
      end.      

                
   
       FOR EACH depend_func of funcionario   NO-LOCK.
       
       
         c-cpf-dep = trim(substring(depend_func.cod_livre_1,1,11)).
         
         if c-cpf-dep = "" or c-cpf-dep = "00000000000" then NEXT.

         
         
         find first tt-cpf where 
                   tt-cpf.cpf-tit =  c-cpf-tit and
                   tt-cpf.cpf-dep =  c-cpf-dep no-error.
                   
             if not avail tt-cpf then do:
                create tt-cpf.
                
                assign 
                    tt-cpf.cpf-tit =  c-cpf-tit 
                    tt-cpf.cpf-dep =  c-cpf-dep
                    tt-cpf.cdn_estab       =  funcionario.cdn_estab  
                    tt-cpf.cdn_empresa     =  funcionario.cdn_empresa    
                    tt-cpf.cdn_funcionario =  funcionario.cdn_funcionario 
                    tt-cpf.cdn_depend_func  =  depend_func.cdn_depend_func .
    
                
             end.
       end.
   
   end.
   
                       
 
    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

    c-excel:VISIBLE = NO.

    ASSIGN c-modelo-planilha = search(tt-param.arq-entrada) 
           c-arq             = SESSION:TEMP-DIRECTORY.
  
    run utp/ut-acomp.p persistent set h-acomp.

   run pi-inicializar in h-acomp(input "Aguarde Abrindo planilha para tempor†rio").

    RUN pi-cria-planilha.
   

    IF RETURN-VALUE = "nok" THEN DO :
        RUN pi-close-excel.
        RUN pi-finalizar IN h-acomp.
        RETURN "nok".
    END.

    

     ASSIGN 
      dt-aux =  DATE(int(i-mes-ref:SCREEN-VALUE IN FRAME f-pg-par),01,int(i-ano-ref:SCREEN-VALUE IN FRAME f-pg-par))
      dt-referencia-ini= dt-aux 
      dt-aux = dt-aux + 34
      dt-aux = DATE(MONTH(dt-aux),01,YEAR(dt-aux)) - 1 
      dt-referencia-fim =  dt-aux.

 

      dt-comp  = dt-referencia-ini .

 ASSIGN c-arq            = SESSION:TEMP-DIRECTORY
          c-arquivo = "ESBS0007-mov-" + STRING(TIME) + ".txt"
          c-arquivo_2  = c-arq  + c-arquivo.
          c-arquivo_2a  = c-arquivo.



OUTPUT STREAM s-saida TO VALUE(c-arquivo_2) NO-CONVERT.  
           
   i-linha = 1.
       ASSIGN c-arq            = SESSION:TEMP-DIRECTORY
          c-arquivo = "ESBS0007-err-" + STRING(TIME) + ".csv"
          c-arquivo_1  = c-arq  + c-arquivo.
          c-arquivo_1a  = c-arquivo.


    OUTPUT TO value(c-arquivo_1) NO-CONVERT.
 
   PUT UNFORMATTED 
   "Linha ;Lote;Nota;C¢digo;Nome;;CPF;Data Nota;Prestador;Cod. Proc.;Descriá∆o do;CH Usa;Quant.;Nr CHs;Mate;Filme;Valor;Total;ERRO;Estab;c-Empresa;Matricula;dependente;Nome"

           SKIP.
           
       
         
    repeat:
      assign i-linha = i-linha + 1.

                 run pi-acompanhar in h-acomp(input string(i-linha)).    

      

      IF substring(trim(STRING(c-relatorio:range("A" + STRING(i-linha)):VALUE)),1,6) = "RESUMO"  THEN LEAVE.

      IF trim(STRING(c-relatorio:range("B" + STRING(i-linha)):VALUE)) = ""  THEN next.


      c-procedimento = trim(STRING(c-relatorio:range("J" + STRING(i-linha)):VALUE)).
     
      if c-procedimento = ?  then next.
 

 

      i-cod =  213. 
      IF  c-procedimento = "10101012" THEN    i-cod =  214.
      ELSE
          IF  c-procedimento = "10081" THEN   i-cod =  214.
           

      
       
 
           
       PUT UNFORMATTED   i-linha  ";"
           c-relatorio:range("B" + STRING(i-linha)):VALUE     ";"
           c-relatorio:range("C" + STRING(i-linha)):VALUE     ";"
           c-relatorio:range("D" + STRING(i-linha)):VALUE     ";"
           c-relatorio:range("E" + STRING(i-linha)):VALUE     ";;"
           c-relatorio:range("G" + STRING(i-linha)):VALUE     ";"
           c-relatorio:range("H" + STRING(i-linha)):VALUE     ";"
           c-relatorio:range("I" + STRING(i-linha)):VALUE     ";"                                 
           c-relatorio:range("J" + STRING(i-linha)):VALUE     ";"                                 
           c-relatorio:range("K" + STRING(i-linha)):VALUE     ";"                                 
           c-relatorio:range("L" + STRING(i-linha)):VALUE     ";"                                 
           c-relatorio:range("M" + STRING(i-linha)):VALUE     ";"                                 
           c-relatorio:range("N" + STRING(i-linha)):VALUE     ";"                                 
           c-relatorio:range("O" + STRING(i-linha)):VALUE     ";"                                 
           c-relatorio:range("P" + STRING(i-linha)):VALUE     ";"                                 
           c-relatorio:range("Q" + STRING(i-linha)):VALUE     ";"                                 
           c-relatorio:range("R" + STRING(i-linha)):VALUE     ";"                                 
           .                               
   
   
  
      
          
       c-cpf = trim(STRING(c-relatorio:range("G" + STRING(i-linha)):VALUE)).

       IF  c-cpf = "" THEN DO:
     
           PUT UNFORMATTED   "CPF FUNCIONARIO EM BRANCO"
           SKIP.

           NEXT.
       END.
       
       if index (c-cpf ,",") > 0 then 
          c-cpf = substring(c-cpf,1,index (c-cpf ,",") - 1).
          
       c-cpf =  "00000000000" + trim(c-cpf).
         
       c-cpf = substring(c-cpf,length (c-cpf) - 10,11).
    
    
       FIND FIRST tt-cpf WHERE  tt-cpf.cpf-dep = c-cpf NO-LOCK NO-ERROR.

       IF NOT AVAIL tt-cpf THEN DO:
            PUT UNFORMATTED   "CPF FUNCIONARIO NAO CADASTRADO"
           SKIP.

            NEXT.


       END.


             FIND FIRST funcionario WHERE 
                funcionario.cdn_estab       = tt-cpf.cdn_estab    AND
                funcionario.cdn_empresa     = tt-cpf.cdn_empresa  AND
                funcionario.cdn_funcionario = tt-cpf.cdn_funcionario    NO-LOCK NO-ERROR.

                
                    IF NOT AVAIL funcionario THEN do:
                          PUT UNFORMATTED   "FUNCIONARIO NAO CADASTRADO"
                           SKIP.
                        
                        NEXT.

                    END.
    
            

       

                    FIND FIRST tt-mov WHERE 
                       tt-mov.cdn_estab       = funcionario.cdn_estab      AND
                       tt-mov.cdn_empresa     = funcionario.cdn_empresa     AND     
                       tt-mov.cdn_funcionario = funcionario.cdn_funcionario AND
                       tt-mov.cdn_depend_func = tt-cpf.cdn_depend_func  and
                       tt-mov.cod             = i-cod  NO-ERROR.

                    IF NOT AVAIL tt-mov THEN DO:
                        CREATE tt-mov.
                         ASSIGN 
                              tt-mov.cdn_estab       = funcionario.cdn_estab      
                              tt-mov.cdn_empresa     = funcionario.cdn_empresa         
                              tt-mov.cdn_funcionario = funcionario.cdn_funcionario
                              tt-mov.cdn_depend_func = tt-cpf.cdn_depend_func
                              tt-mov.cod             = i-cod.
                    END.


                     tt-mov.valor = tt-mov.valor + DEC(c-relatorio:range("R" + STRING(i-linha)):VALUE).
                     tt-mov.qtde  = tt-mov.qtde  + 1.


        PUT UNFORMATTED   "SUCESSO"  ";"
            tt-mov.cdn_estab    ";"
            tt-mov.cdn_empresa  ";"
            tt-mov.cdn_funcionario ";"
            tt-mov.cdn_depend_func ";"
            funcionario.nom_pessoa_fisic
           SKIP.


     
        

        
             

    END.
    
 PUT UNFORMATTED "RESUMO;" SKIP.  /* PARA PODE IMPORTAR A PLANILHA REVISADA NOVAMENTE.*/ 

    FOR EACH tt-mov WHERE tt-mov.valor > 0.
        FIND FIRST funcionario WHERE 
           tt-mov.cdn_estab       = funcionario.cdn_estab      AND
           tt-mov.cdn_empresa     = funcionario.cdn_empresa     AND     
           tt-mov.cdn_funcionario = funcionario.cdn_funcionario  NO-ERROR.


 run pi-acompanhar in h-acomp(input string(nr-reg)).    

                                   
                                           ASSIGN   nr-reg     = nr-reg + 1
                                           c-empresa    = tt-mov.cdn_empresa 
                                           c-estabelec  = tt-mov.cdn_estab 
                                           matricula  = tt-mov.cdn_funcionario 
                                           dependente = 0 /* tt-mov.cdn_depend_func  */
                                           branco     = funcionario.num_digito_verfdor_func /* */
                                           beneficio  = tt-mov.cod
                                           regr-benef = 0 /*benefic_func.cdn_regra_benefic*/
                                           mes-refer  = MONTH(dt-referencia-fim ) 
                                           ano-refer  = YEAR(dt-referencia-fim )   
                                           valor      = tt-mov.valor  * 100
                                           quantidade = tt-mov.qtde * 1000
                                           parcelas   = 1
                                           formula    = 0 /*int(trim(SUBSTRING(benefic_func.cod_livre_1,1,4)))*/
                                           prest-serv = 0 /*conven_benefic.cdn_prestdor_serv  */
                                           /*dia-ocorr  =  day(movto_bradesco.dt_utilizacao )
                                           mes-ocorr  =  MONTH(movto_bradesco.dt_utilizacao )  
                                           ano-ocorr  =  YEAR(movto_bradesco.dt_utilizacao )  */
                                           ocorr         = 0
                                           documento     = 0 
                                           mot-lot-benef = 0
                                           dia-pag-efet  = 0
                                           mes-pag-efet  = 0
                                           ano-pag-efet  = 0
                                           per-ini       = int(string(dt-referencia-ini,"99999999"))
                                           per-fim       = int(string(dt-referencia-fim,"99999999")).

                                   
                                   
                                   
                                           RUN pi-grava-linha.
 
    END.
    
    PUT STREAM s-saida skip.

  RUN pi-close-excel.

  output stream s-saida close.

          OUTPUT CLOSE.

         
   RUN pi-finalizar IN h-acomp.

   OS-COPY value(c-arquivo_1)  V:/TEMP.
   OS-COPY value(c-arquivo_2)  V:/TEMP.


     os-delete value(c-arquivo_1).
     os-delete value(c-arquivo_2).



  
 run utp/ut-msgs.p (input "show", input 17006, input "FOI GERADO ARQUIVOS NO C:/TEMP:~~" + c-arquivo_1a + "," + c-arquivo_2a).

 
 


/*    {include/i-imrun.i prghur/fpp/ESBS0007rp.p}

    {include/i-imexc.i}

    if  session:set-wait-state("") then.
    
    {include/i-imtrm.i tt-param.arq-destino tt-param.destino}
    
    */
   
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina C-Win 
PROCEDURE pi-troca-pagina :
/*:T------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P†gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{include/i-imtrp.i}

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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-linha C-Win 
PROCEDURE pi-grava-linha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

PUT STREAM s-saida
               
        
        constante-2  ";"
        nr-reg       ";"
        c-empresa    ";"  
        c-estabelec  ";"  
        matricula    ";" 
        branco       ";"
        dependente   ";"
        beneficio    ";"
        regr-benef   ";"
        mes-refer    ";"
        ano-refer    ";"
        valor        ";"
        quantidade   ";"
        parcelas     ";"
        formula      ";"
        prest-serv   ";"
        /*dia-ocorr  */  
        /*mes-ocorr  */
        /*ano-ocorr  */
        0 ";"
        0 ";"
        0 ";"        
        documento  ";"  
        mot-lot-benef ";"
        dia-pag-efet  ";"
        mes-pag-efet  ";"
        ano-pag-efet  ";"
        per-ini      ";"
        per-fim      
        SKIP.
               
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
