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
{include/i-prgvrs.i esbs0009 1.0.00.00.000}

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
DEFINE VARIABLE c-cod-usuario AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cod-usuario-pai AS CHARACTER   NO-UNDO.
DEFINE BUFFER b-funcionario FOR funcionario.
DEFINE BUFFER b-depend_func_Unimed FOR depend_func_Unimed.
define buffer bb-funcionario for funcionario.
DEFINE VARIABLE c-nome-dependente AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-criou-depend AS LOGICAL     NO-UNDO.
DEFINE VARIABLE i-qtde-nome AS INTEGER     NO-UNDO.
DEFINE VARIABLE r-depend    AS ROWID       NO-UNDO.
DEFINE VARIABLE i-tam       AS INTEGER     NO-UNDO.
DEFINE VARIABLE h-campo AS HANDLE      NO-UNDO.
/* define  temp-table tipo de registro 0*/

    /* define  temp-table tipo de registro 2*/

     

define temp-table tt-cpf
field cpf-tit as char  format "x(11)"
field cpf-dep as char  format "x(11)"
FIELD cdn_depend_func  LIKE depend_func.cdn_depend_func 
field cdn_empresa like depend_func.cdn_empresa
field cdn_estab   like depend_func.cdn_estab
field cdn_funcionario like depend_func.cdn_funcionario
FIELD nom_pessoa_fisic like funcionario.nom_pessoa_fisic
FIELD nom_depend AS CHAR
index codigo is primary unique
        cpf-tit
        cpf-dep
        
         
index dep
        cpf-dep
        cpf-tit 
.


define temp-table tt-MOV LIKE movto_unimed. 


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
DEFINE VARIABLE nr-reg         AS INTEGER   FORMAT 99999                            NO-UNDO.   


/* define layout*/

/*
DEFINE VARIABLE constante      AS CHARACTER format "x(1)"        INITIAL ""         NO-UNDO.
DEFINE VARIABLE constante-2    AS CHARACTER format "x(8)"        INITIAL "mvtobene" NO-UNDO.
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
                               */



DEF STREAM s-saida.
DEFINE VARIABLE l-s-saida AS LOGICAL     NO-UNDO.


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

DEFINE VARIABLE c-arquivo-entrada AS CHARACTER  INITIAL ""
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 53.72 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE c-arquivo-saida AS CHARACTER INITIAL "v:/temp/consiste.txt" 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 53.72 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE c-estab-fim AS CHAR FORMAT "x(05)":U  
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-estab-ini AS CHAR FORMAT "x(05)":U   /*solic-318*/ 
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

DEFINE VARIABLE i-empresa-fim AS CHAR FORMAT "x(03)":U   /*solic-318*/ 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE i-empresa-ini AS CHAR FORMAT "x(03)":U   /*solic-318*/ 
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
 
       
       l-s-saida = NO.
              
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

/*{utp/ut9000.i "esbs0009" "1.00.00.000"}
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
    i-empresa-ini = STRING({cdp\poloestab.i 420}). /*solic-318*/ 
    i-empresa-fim = STRING({cdp\poloestab.i 420}). /*solic-318*/ 
    c-estab-ini   = STRING({cdp\poloestab.i 422}). /*solic-318*/ 
    c-estab-fim   = STRING({cdp\poloestab.i 423}). /*solic-318*/

    RUN enable_UI.
    FIND FIRST param_empres_rh WHERE param_empres_rh.cdn_empresa = v_cdn_empres_usuar NO-LOCK NO-ERROR.

    FIND FIRST usuar_mestre WHERE usuar_mestre.cod_usuar = c-seg-usuario NO-LOCK NO-ERROR.


     

       c-arquivo-saida:visible in frame f-pg-par = no.
       bt-arquivo-saida:visible in frame f-pg-par = no.

       IF AVAIL param_empres_rh THEN
           ASSIGN
              i-mes-ref:SCREEN-VALUE IN FRAME f-pg-par = string(param_empres_rh.num_mes_refer_calc_efetd)
              i-ano-ref:SCREEN-VALUE IN FRAME f-pg-par = string(param_empres_rh.num_ano_refer_calc_efetd).


       h-campo = im-pg-lay:HANDLE IN FRAME f-import .

      DO WHILE valid-handle(h-campo:PREV-SIBLING):
        h-campo = h-campo:PREV-SIBLING.
      END.
      DO WHILE valid-handle(h-campo ):
    
        IF h-campo:TYPE = "TEXT" THEN 
            if  h-campo:SCREEN-VALUE <> "layout" THEN
    
             h-campo:VISIBLE = NO.
            ELSE
                 h-campo:SCREEN-VALUE = "Seleá∆o".
    
        IF h-campo:name = "im-pg-sel" THEN
            h-campo:VISIBLE = no.
        IF h-campo:name = "im-pg-log" THEN
            h-campo:VISIBLE = no.
        IF h-campo:name = "im-pg-par" THEN
            h-campo:VISIBLE = no.
    
        h-campo = h-campo:next-SIBLING.
      END.
    

    {include/i-immbl.i}


        
        APPLY 'mouse-select-click' to im-pg-par IN FRAME {&FRAME-NAME}.
    /*{include/i-imvrf.i &programa=esbs0009 &versao-layout=001}*/
  
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
    /*c-arquivo = SESSION:TEMP-DIRECTORY + "esbs0009" + STRING(TIME) + ".XLS".

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
                 tt-cpf.nom_pessoa_fisic = funcionario.nom_pessoa_fisic
                 tt-cpf.nom_depend       = funcionario.nom_pessoa_fisic
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
                        tt-cpf.cpf-tit          =  c-cpf-tit 
                        tt-cpf.cpf-dep          =  c-cpf-dep
                        tt-cpf.cdn_estab        =  funcionario.cdn_estab  
                        tt-cpf.cdn_empresa      =  funcionario.cdn_empresa    
                        tt-cpf.cdn_funcionario  =  funcionario.cdn_funcionario 
                        tt-cpf.cdn_depend_func  =  depend_func.cdn_depend_func 
                        tt-cpf.nom_pessoa_fisic =  funcionario.nom_pessoa_fisic
                        tt-cpf.nom_depend       =  depend_func.nom_depend_func.
        
                    
                 end.
           end.
       
    END.
   
                       
 
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
        c-arquivo = "esbs0009-mov-" + STRING(TIME) + ".txt"
        c-arquivo_2  = c-arq  + c-arquivo.
        c-arquivo_2a  = c-arquivo.

    OUTPUT STREAM s-saida TO VALUE(c-arquivo_2) NO-CONVERT.  
    l-s-saida = NO.
           
    i-linha = 1.

    ASSIGN c-arq     = SESSION:TEMP-DIRECTORY
           c-arquivo = "esbs0009-err-" + STRING(TIME) + ".csv"
           c-arquivo_1  = c-arq  + c-arquivo.
           c-arquivo_1a  = c-arquivo.

    OUTPUT TO value(c-arquivo_1) NO-CONVERT.
 
    PUT UNFORMATTED 
     "Linha;Lote;Nota;C¢digo Usu†rio;Nome;CPF;Data Nota;Prestador;Cod. Proc.;Descriá∆o do Procedimento;Tipo Servico;;;Quant.;;Nr CHs;;Material;;Filme;;Valor;Total;ERRO;Estab;c-Empresa;Matricula;dependente;Nome"  
           SKIP.                  
             
    repeat:
            assign i-linha = i-linha + 1.
                     
            run pi-acompanhar in h-acomp(input string(i-linha)).    
          
            IF substring(trim(STRING(c-relatorio:range("A" + STRING(i-linha)):VALUE)),1,6) = "RESUMO"  THEN LEAVE.
        
            IF trim(STRING(c-relatorio:range("B" + STRING(i-linha)):VALUE)) = ""  THEN next.
           
            c-procedimento = trim(STRING(c-relatorio:range("J" + STRING(i-linha)):VALUE)).
            
            if c-procedimento = ?  then next.

            c-procedimento = REPLACE(c-procedimento,";","").
     
            

            PUT UNFORMATTED   
               i-linha                                            ";"
               c-relatorio:range("B" + STRING(i-linha)):VALUE     ";"
               c-relatorio:range("C" + STRING(i-linha)):VALUE     ";"
               c-relatorio:range("D" + STRING(i-linha)):VALUE     ";"
               c-relatorio:range("E" + STRING(i-linha)):VALUE     ";"
               c-relatorio:range("F" + STRING(i-linha)):VALUE     ";"
               c-relatorio:range("G" + STRING(i-linha)):VALUE     ";"
               c-relatorio:range("H" + STRING(i-linha)):VALUE     ";"
               c-relatorio:range("I" + STRING(i-linha)):VALUE     ";"                                 
               c-procedimento /*c-relatorio:range("J" + STRING(i-linha)):VALUE*/     ";"                                 
               c-relatorio:range("K" + STRING(i-linha)):VALUE     ";;;"                                            
               c-relatorio:range("N" + STRING(i-linha)):VALUE     ";;"                                 
               c-relatorio:range("P" + STRING(i-linha)):VALUE     ";;"                                 
               c-relatorio:range("R" + STRING(i-linha)):VALUE     ";;"                                 
               c-relatorio:range("T" + STRING(i-linha)):VALUE     ";;" 
               c-relatorio:range("V" + STRING(i-linha)):VALUE     ";" 
               c-relatorio:range("W" + STRING(i-linha)):VALUE     ";" .                               
       
            c-cod-usuario = TRIM(replace(c-relatorio:range("D" + STRING(i-linha)):VALUE,"-","")) .
    
            IF  c-cod-usuario = "" THEN DO:         
                PUT UNFORMATTED   "USUµRIO EM BRANCO" SKIP.    
                NEXT.
            END.
       
            FIND FIRST depend_func_Unimed WHERE 
                depend_func_Unimed.cdn_depend_func_Unimed = c-cod-usuario NO-LOCK NO-ERROR.
            c-cpf = trim(STRING(c-relatorio:range("f" + STRING(i-linha)):VALUE)).
      
            IF NOT AVAIL depend_func_Unimed AND c-cpf <> "" AND num-entries(c-cod-usuario,".") = 4 THEN DO:

                /*VAI TENTAR ACHAR PELO CPF cuidado porque o cpf do dependente pode ser do titurar*/
            
                if index (c-cpf ,",") > 0 then 
                    c-cpf = substring(c-cpf,1,index (c-cpf ,",") - 1).
            
                c-cpf =  "00000000000" + trim(c-cpf).
            
                c-cpf = substring(c-cpf,length (c-cpf) - 10,11).          
                        
                FIND FIRST tt-cpf WHERE  tt-cpf.cpf-dep = c-cpf NO-LOCK NO-ERROR.
                    
                IF  AVAIL tt-cpf THEN DO:
                    
                    FIND FIRST funcionario WHERE 
                        funcionario.cdn_estab       = tt-cpf.cdn_estab    AND
                        funcionario.cdn_empresa     = tt-cpf.cdn_empresa  AND
                        funcionario.cdn_funcionario = tt-cpf.cdn_funcionario    NO-LOCK NO-ERROR.        
                    
                    IF AVAIL funcionario THEN do:
                       

                       IF (c-cpf <> funcionario.cod_id_feder AND 
                           ENTRY(4,c-cod-usuario,".") <> "00") OR (c-cpf = funcionario.cod_id_feder AND 
                           ENTRY(4,c-cod-usuario,".") = "00") THEN DO:   /* SEGURANÄA PORQUE TEM DEPENDENTE COM CPF DO TITULAR*/

                    
                            FIND FIRST depend_func_Unimed WHERE 
                                depend_func_Unimed.cdn_empresa     = funcionario.cdn_empresa       AND
                                depend_func_Unimed.cdn_estab       = funcionario.cdn_estab     AND
                                depend_func_Unimed.cdn_funcionario = funcionario.cdn_funcionario AND            
                                depend_func_Unimed.cdn_depend_func = tt-cpf.cdn_depend_func  NO-ERROR.
                        
                            IF NOT AVAIL depend_func_Unimed THEN DO:
                                CREATE depend_func_Unimed.
                        
                                ASSIGN  
                                     depend_func_Unimed.cdn_depend_func_Unimed = c-cod-usuario
                                     depend_func_Unimed.cdn_empresa            = funcionario.cdn_empresa       
                                     depend_func_Unimed.cdn_estab              = funcionario.cdn_estab   
                                     depend_func_Unimed.cdn_funcionario        = funcionario.cdn_funcionario
                                     depend_func_Unimed.cdn_depend_func        = tt-cpf.cdn_depend_func
                                     depend_func_Unimed.dat_geracao_cadastro   = TODAY
                                     depend_func_Unimed.dat_entrada_plano      = TODAY
                                     depend_func_Unimed.dat_saida_plano        = 12/31/9999
                                     depend_func_Unimed.log_ativo              = YES
                                     depend_func_Unimed.cod_id_feder           = tt-cpf.cpf-dep.
                        
                        
                        
                                FOR FIRST benefic_func WHERE 
                                     ((benefic_func.cdn_beneficio  = 213) OR (benefic_func.cdn_beneficio = 214) OR (benefic_func.cdn_beneficio = 216)) AND
                                      benefic_func.cdn_empresa     = funcionario.cdn_empresa      AND  
                                      benefic_func.cdn_estab       = funcionario.cdn_estab        AND
                                      benefic_func.cdn_funcionario = funcionario.cdn_funcionario  AND
                                      benefic_func.cdn_depend_func = 0      NO-LOCK.
                        
                                   ASSIGN 
                                        depend_func_Unimed.dat_entrada_plano      =  benefic_func.dat_inic_benefic
                                        depend_func_Unimed.dat_saida_plano        =  benefic_func.dat_term_benefic
                                        depend_func_Unimed.log_ativo              =  benefic_func.idi_sit_benefic = 1.
                        
                                END.
                        
                            END.
                            ELSE
                                    IF trim(depend_func_Unimed.cdn_depend_func_Unimed) = "" THEN
                                         ASSIGN                                  
                                            depend_func_Unimed.cdn_depend_func_Unimed = c-cod-usuario.
                            
                            ASSIGN 
                                c-cod-usuario-pai = ENTRY(1,c-cod-usuario,".") + "." +
                                                    ENTRY(2,c-cod-usuario,".") + "." +
                                                    ENTRY(3,c-cod-usuario,".") + ".00".

                            IF  c-cod-usuario <> c-cod-usuario-pai THEN DO:          /* se o titular j† n∆o existir cria o pai pelo cpf do dependente*/                

                                  FIND FIRST b-depend_func_Unimed WHERE                     
                                      b-depend_func_Unimed.cdn_depend_func        = 0  and
                                      b-depend_func_Unimed.cdn_estab              = funcionario.cdn_estab          AND
                                      b-depend_func_Unimed.cdn_empresa            = funcionario.cdn_empresa        AND
                                      b-depend_func_Unimed.cdn_funcionario        = funcionario.cdn_funcionario    EXCLUSIVE-LOCK NO-ERROR.
                        
                                  IF NOT AVAIL b-depend_func_Unimed THEN DO:                
                                                
                                           CREATE b-depend_func_Unimed.
                                           BUFFER-COPY  depend_func_Unimed EXCEPT  cdn_depend_func_Unimed
                                                                                   cdn_depend_func       
                                                                                   cod_id_feder  TO b-depend_func_Unimed
                                                ASSIGN  
                                                 b-depend_func_Unimed.cdn_depend_func_Unimed = c-cod-usuario-pai
                                                 b-depend_func_Unimed.cdn_depend_func        = 0 
                                                 b-depend_func_Unimed.cod_id_feder           = funcionario.cod_id_feder.                                                                                               
                                  END.
                                  ELSE DO:
                                      IF b-depend_func_Unimed.cdn_depend_func_Unimed = "" THEN 
                                            b-depend_func_Unimed.cdn_depend_func_Unimed =  c-cod-usuario-pai .
                                      ELSE
                                          IF b-depend_func_Unimed.cdn_depend_func_Unimed <>  c-cod-usuario-pai THEN DO:
                        
                                              l-s-saida = YES.

                                              PUT  STREAM s-saida UNFORMATTED
                                                   "Analise esse cadastro;"
                                                   c-cod-usuario ";"
                                                   c-cod-usuario-pai ";"
                                                   b-depend_func_Unimed.cdn_depend_func_Unimed ";"
                                                   b-depend_func_Unimed.cdn_empresa            ";"
                                                   b-depend_func_Unimed.cdn_estab              ";"
                                                   b-depend_func_Unimed.cdn_funcionario        ";"
                                                   IF AVAIL funcionario THEN funcionario.nom_pessoa_fisic ELSE "N∆o existe funcion†rio" ";"
                                                   b-depend_func_Unimed.cdn_depend_func        ";"
                                                   b-depend_func_Unimed.dat_geracao_cadastro   ";"
                                                   b-depend_func_Unimed.dat_entrada_plano      ";"
                                                   b-depend_func_Unimed.dat_saida_plano        ";"
                                                   b-depend_func_Unimed.log_ativo              ";"
                                                   b-depend_func_Unimed.cod_id_feder           SKIP.
                        
                                          END.
                        
                                  END.
                        
                            END.
                   
                       END. /* FINAL 00 E CPF = FUNC OU FINLA <> 00 E CPF <> FUNC */

                    END.  /*IF AVAIL funcionario THEN do:*/

                END. /* IF  AVAIL tt-cpf THEN DO:*/

            END. /* IF NOT AVAIL depend_func_Unimed AND c-cpf <> "" AND num-entries(c-cod-usuario,".") = 4 THEN DO:*/



            /* vamos tentar achar o dependente do funcionario pelo titular e pelo nome do dependente*/
            IF NOT AVAIL depend_func_Unimed  AND num-entries(c-cod-usuario,".") = 4 THEN DO:
                ASSIGN 
                    c-cod-usuario-pai = ENTRY(1,c-cod-usuario,".") + "." +
                                        ENTRY(2,c-cod-usuario,".") + "." +
                                        ENTRY(3,c-cod-usuario,".") + ".00"
                    c-nome-dependente = c-relatorio:range("E" + STRING(i-linha)):VALUE
                    l-criou-depend = NO.
                
                IF c-cod-usuario-pai <> c-cod-usuario  THEN DO:
                    FOR FIRST b-depend_func_Unimed WHERE                     
                        b-depend_func_Unimed.cdn_depend_func_Unimed = c-cod-usuario-pai NO-LOCK,
                        FIRST funcionario WHERE 
                            funcionario.cdn_estab       = b-depend_func_Unimed.cdn_estab           AND
                            funcionario.cdn_empresa     = b-depend_func_Unimed.cdn_empresa         AND
                            funcionario.cdn_funcionario = b-depend_func_Unimed.cdn_funcionario    NO-LOCK.

                        i-tam = 5.

                         

                        DO WHILE true. 
                        
                            ASSIGN i-qtde-nome = 0
                                   r-depend    = ?.
    
                            FOR EACH depend_func OF funcionario where  
                                NOT CAN-FIND (FIRST depend_func_Unimed WHERE 
                                              depend_func_Unimed.cdn_estab        = funcionario.cdn_estab        AND
                                              depend_func_Unimed.cdn_empresa      = funcionario.cdn_empresa      AND
                                              depend_func_Unimed.cdn_funcionario  = funcionario.cdn_funcionario  AND
                                              depend_func_Unimed.cdn_depend_func  = depend_func.cdn_depend_func) NO-LOCK.
                                
                                IF trim(substring(depend_func.nom_depend_func,1,i-tam)) = trim(substring(c-nome-dependente,1,i-tam)) THEN
                                    ASSIGN i-qtde-nome = i-qtde-nome + 1
                                           r-depend = ROWID(depend_func).
                            END.
    
                            IF i-qtde-nome = 1 AND r-depend <> ? THEN DO:
                                FOR FIRST depend_func WHERE ROWID(depend_func) = r-depend NO-LOCK.
                                    FIND FIRST depend_func_Unimed WHERE 
                                        depend_func_Unimed.cdn_empresa     = funcionario.cdn_empresa     AND
                                        depend_func_Unimed.cdn_estab       = funcionario.cdn_estab       AND
                                        depend_func_Unimed.cdn_funcionario = funcionario.cdn_funcionario AND            
                                        depend_func_Unimed.cdn_depend_func = depend_func.cdn_depend_func  NO-ERROR.
                               
                                    IF NOT AVAIL depend_func_Unimed THEN DO:
                                        CREATE depend_func_Unimed.
                               
                                        ASSIGN  
                                             depend_func_Unimed.cdn_depend_func_Unimed = c-cod-usuario
                                             depend_func_Unimed.cdn_empresa            = funcionario.cdn_empresa      
                                             depend_func_Unimed.cdn_estab              = funcionario.cdn_estab    
                                             depend_func_Unimed.cdn_funcionario        = funcionario.cdn_funcionario
                                             depend_func_Unimed.cdn_depend_func        = depend_func.cdn_depend_func
                                             depend_func_Unimed.dat_geracao_cadastro   = TODAY
                                             depend_func_Unimed.dat_entrada_plano      = TODAY
                                             depend_func_Unimed.dat_saida_plano        = 12/31/9999
                                             depend_func_Unimed.log_ativo              = YES
                                             depend_func_Unimed.cod_id_feder           = trim(substring(depend_func.cod_livre_1,1,11)).
                                   
                                   
                                   
                                        FOR FIRST benefic_func WHERE 
                                             ((benefic_func.cdn_beneficio  = 213) OR (benefic_func.cdn_beneficio = 214) OR (benefic_func.cdn_beneficio = 216)) AND
                                              benefic_func.cdn_empresa     = funcionario.cdn_empresa        AND  
                                              benefic_func.cdn_estab       = funcionario.cdn_estab      AND               
                                              benefic_func.cdn_funcionario = funcionario.cdn_funcionario  AND
                                              benefic_func.cdn_depend_func = 0      NO-LOCK.
                                   
                                            ASSIGN 
                                                depend_func_Unimed.dat_entrada_plano      =  benefic_func.dat_inic_benefic
                                                depend_func_Unimed.dat_saida_plano        =  benefic_func.dat_term_benefic
                                                depend_func_Unimed.log_ativo              =  (benefic_func.idi_sit_benefic = 1).
                                            
                                        END.  /*FOR FIRST benefic_func WHERE */
                                   
                                        l-criou-depend = YES.
                                    END.  /*IF NOT AVAIL depend_func_Unimed THEN DO:*/                
                                    ELSE 
                                        l-criou-depend = YES.
    
                                    
                                END.  /*FOR FIRST depend_func WHERE ROWID(depend_func) = r-depend NO-LOCK.*/
    
                            END.  /*IF i-qtde-nome = 1 AND r-depend <> ? THEN DO:*/

                            IF l-criou-depend = YES THEN LEAVE.
                            IF i-qtde-nome = 0  THEN LEAVE.

                            IF i-tam = 5 THEN   /*achou mais de 1 com 5 letras iguais jose luiz e jose fernando*/
                                i-tam = 8.
                            ELSE            
                                IF i-tam = 8 THEN   /*achou mais de 1 com 8 letras iguais frederico luiz e frederico fernando*/
                                    i-tam = 11.
                                ELSE
                                    IF i-tam = 11 THEN /*achou mais de 1 com 11 letras iguais frederico roberto luiz e frederico roberval fernando*/
                                        i-tam = 14.
                                    ELSE
                                        IF i-tam = 14 THEN /*achou mais de 1 com 14 letras iguais frederico roberto luiz e frederico roberval fernando*/
                                        i-tam = 100.
                                        ELSE
                                            LEAVE.   /*achou mais de 1 com 100 letras iguais frederico roberto luiz e frederico roberto luiz (derrepente nome do filho = do avo tb dependente*/

                        END.  /*DO WHILE true.*/

                    END.  /*FOR FIRST b-depend_func_Unimed WHERE                     */

                END.  /*IF c-cod-usuario-pai <> c-cod-usuario  THEN DO:*/

            END.  /*if NOT AVAIL depend_func_Unimed*/




            IF NOT AVAIL depend_func_Unimed  THEN DO:
              PUT UNFORMATTED   "USUµRIO N«O CADASTRADO" SKIP.    
              NEXT.
      
            END.

            /* se o dependente teve CPF e foi criado  e o titular ainda n∆o est† na especifica, o dependente tenta criar o titular.*/
            IF  num-entries(c-cod-usuario,".") = 4 AND   ENTRY(4,c-cod-usuario,".") <> "00" THEN DO:
            ASSIGN 
                c-cod-usuario-pai = ENTRY(1,c-cod-usuario,".") + "." +
                                    ENTRY(2,c-cod-usuario,".") + "." +
                                    ENTRY(3,c-cod-usuario,".") + ".00".
                                  
                FIND FIRST b-depend_func_Unimed WHERE                     
                    b-depend_func_Unimed.cdn_depend_func        = 0  and
                    b-depend_func_Unimed.cdn_estab              = depend_func_Unimed.cdn_estab         AND
                    b-depend_func_Unimed.cdn_empresa            = depend_func_Unimed.cdn_empresa       AND
                    b-depend_func_Unimed.cdn_funcionario        = depend_func_Unimed.cdn_funcionario   EXCLUSIVE-LOCK NO-ERROR.

                IF NOT AVAIL b-depend_func_Unimed THEN DO:                

                    FOR FIRST funcionario WHERE 
                            funcionario.cdn_estab        = depend_func_Unimed.cdn_estab       AND
                            funcionario.cdn_empresa      = depend_func_Unimed.cdn_empresa     AND
                            funcionario.cdn_funcionario  = depend_func_Unimed.cdn_funcionario NO-LOCK.

                         CREATE b-depend_func_Unimed.
                         BUFFER-COPY  depend_func_Unimed EXCEPT  cdn_depend_func_Unimed
                                                                cdn_depend_func       
                                                                 cod_id_feder  TO b-depend_func_Unimed
                              ASSIGN  
                               b-depend_func_Unimed.cdn_depend_func_Unimed = c-cod-usuario-pai
                               b-depend_func_Unimed.cdn_depend_func        = 0 
                               b-depend_func_Unimed.cod_id_feder           = funcionario.cod_id_feder.

                    END.
                

                END.
                ELSE DO:
                    IF b-depend_func_Unimed.cdn_depend_func_Unimed = "" THEN 
                          b-depend_func_Unimed.cdn_depend_func_Unimed =  c-cod-usuario-pai .
                    ELSE
                        IF b-depend_func_Unimed.cdn_depend_func_Unimed <>  c-cod-usuario-pai THEN DO:

                            FOR FIRST funcionario WHERE 
                            funcionario.cdn_estab        = depend_func_Unimed.cdn_estab       AND
                            funcionario.cdn_empresa      = depend_func_Unimed.cdn_empresa     AND
                            funcionario.cdn_funcionario  = depend_func_Unimed.cdn_funcionario NO-LOCK.
                            END.

                            l-s-saida = YES.

                            PUT  STREAM s-saida UNFORMATTED
                                 "Analise esse cadastro;"
                                 c-cod-usuario ";"
                                 c-cod-usuario-pai ";"
                                 b-depend_func_Unimed.cdn_depend_func_Unimed ";"
                                 b-depend_func_Unimed.cdn_empresa            ";"
                                 b-depend_func_Unimed.cdn_estab              ";"
                                 b-depend_func_Unimed.cdn_funcionario        ";"
                                 IF AVAIL funcionario THEN funcionario.nom_pessoa_fisic ELSE "N∆o existe funcion†rio" ";"
                                 b-depend_func_Unimed.cdn_depend_func        ";"
                                 b-depend_func_Unimed.dat_geracao_cadastro   ";"
                                 b-depend_func_Unimed.dat_entrada_plano      ";"
                                 b-depend_func_Unimed.dat_saida_plano        ";"
                                 b-depend_func_Unimed.log_ativo              ";"
                                 b-depend_func_Unimed.cod_id_feder           SKIP.

                        END.
                        
                END.

            END.


            CREATE tt-mov.


            c-procedimento = c-relatorio:range("I" + STRING(i-linha)):VALUE.

            tt-mov.procedimento = 0 .            
            tt-mov.procedimento = INT(c-procedimento) NO-ERROR.

            ASSIGN
                tt-mov.cdn_depend_func          = depend_func_Unimed.cdn_depend_func 
                tt-mov.cdn_depend_func_unimed   = c-cod-usuario
                tt-mov.cdn_empresa              = depend_func_Unimed.cdn_empresa     
                tt-mov.cdn_estab                = depend_func_Unimed.cdn_estab
                tt-mov.cdn_funcionario          = depend_func_Unimed.cdn_funcionario 
                tt-mov.cod_prestador            = c-relatorio:range("H" + STRING(i-linha)):VALUE
                tt-mov.cpf                      = depend_func_Unimed.cod_id_feder
                tt-mov.lote                     = c-relatorio:range("B" + STRING(i-linha)):VALUE
                tt-mov.documento                = c-relatorio:range("C" + STRING(i-linha)):VALUE
                tt-mov.descricao                = c-relatorio:range("J" + STRING(i-linha)):VALUE
                tt-mov.dt_competencia           = dt-aux
                tt-mov.dt_utilizacao            = c-relatorio:range("G" + STRING(i-linha)):VALUE
                tt-mov.idi_situacao             = 1    
                tt-mov.nome_usuario             = c-relatorio:range("E" + STRING(i-linha)):VALUE
                tt-mov.num_ch                   = c-relatorio:range("P" + STRING(i-linha)):VALUE                
                tt-mov.quantidade               = c-relatorio:range("N" + STRING(i-linha)):VALUE
                tt-mov.tipo_servico             = c-relatorio:range("K" + STRING(i-linha)):VALUE
                tt-mov.tipo_servico             = replace(REPLACE(tt-mov.tipo_servico,"-","")," ","")
                tt-mov.vl_filme                 = c-relatorio:range("T" + STRING(i-linha)):VALUE
                tt-mov.vl_material              = c-relatorio:range("R" + STRING(i-linha)):VALUE
                tt-mov.vl_procedimento          = c-relatorio:range("V" + STRING(i-linha)):VALUE
                tt-mov.vl_total                 = c-relatorio:range("W" + STRING(i-linha)):VALUE.
    
    
            FIND FIRST funcionario WHERE 
                funcionario.cdn_estab        = depend_func_Unimed.cdn_estab       AND
                funcionario.cdn_empresa      = depend_func_Unimed.cdn_empresa     AND
                funcionario.cdn_funcionario  = depend_func_Unimed.cdn_funcionario NO-LOCK NO-ERROR.
    
    
            PUT UNFORMATTED   "SUCESSO" ";"
                tt-mov.cdn_estab        ";"
                tt-mov.cdn_empresa      ";"
                tt-mov.cdn_funcionario  ";"
                tt-mov.cdn_depend_func  ";"
                IF AVAIL funcionario THEN funcionario.nom_pessoa_fisic ELSE ""
                    SKIP.
    
    END.
        
    PUT UNFORMATTED "RESUMO;" SKIP.  /* PARA PODE IMPORTAR A PLANILHA REVISADA NOVAMENTE.*/ 

    FOR EACH tt-mov .

        tt-mov.documento = (IF NUM-ENTRIES(tt-mov.documento,",") > 1 THEN ENTRY(1,tt-mov.documento,",") ELSE  tt-mov.documento). /* titando casas decimais vindo do excel ,0000*/

        run pi-acompanhar in h-acomp(input "LIMPANDO: " + string(nr-reg)).    

        ASSIGN   nr-reg     = nr-reg + 1.

        FOR EACH movto_unimed WHERE        
            movto_unimed.cdn_empresa      = tt-mov.cdn_empresa     AND
            movto_unimed.cdn_estab        = tt-mov.cdn_estab       AND
            movto_unimed.cdn_funcionario  = tt-mov.cdn_funcionario AND
            movto_unimed.cdn_depend_func  = tt-mov.cdn_depend_func AND
            movto_unimed.dt_competencia   = tt-mov.dt_competencia  AND
            movto_unimed.lote             = tt-mov.lote            AND
            movto_unimed.documento        = tt-mov.documento       AND
            movto_unimed.dt_utilizacao    = tt-mov.dt_utilizacao   AND
            movto_unimed.cod_prestador    = tt-mov.cod_prestador   AND
            movto_unimed.procedimento     = tt-mov.procedimento    .

            DELETE movto_unimed.
        END.
    END.

    NR-REG = 0.

    FOR EACH tt-mov .

        run pi-acompanhar in h-acomp(input "CRIANDO: " + string(nr-reg)).    

        ASSIGN   nr-reg = nr-reg + 1.

        CREATE movto_unimed.

        ASSIGN
            movto_unimed.cdn_depend_func          =   tt-mov.cdn_depend_func        
            movto_unimed.cdn_depend_func_unimed   =   tt-mov.cdn_depend_func_unimed 
            movto_unimed.cdn_empresa              =   tt-mov.cdn_empresa            
            movto_unimed.cdn_estab                =   tt-mov.cdn_estab              
            movto_unimed.cdn_funcionario          =   tt-mov.cdn_funcionario        
            movto_unimed.cod_prestador            =   tt-mov.cod_prestador          
            movto_unimed.cpf                      =   tt-mov.cpf                    
            movto_unimed.lote                     =   tt-mov.lote                   
            movto_unimed.documento                =   tt-mov.documento
            movto_unimed.descricao                =   replace(replace(tt-mov.descricao,";"," "),CHR(10)," ")
            movto_unimed.dt_competencia           =   tt-mov.dt_competencia         
            movto_unimed.dt_utilizacao            =   tt-mov.dt_utilizacao          
            movto_unimed.idi_situacao             =   tt-mov.idi_situacao           
            movto_unimed.nome_usuario             =   tt-mov.nome_usuario           
            movto_unimed.num_ch                   =   tt-mov.num_ch                 
            movto_unimed.procedimento             =   tt-mov.procedimento           
            movto_unimed.quantidade               =   tt-mov.quantidade             
            movto_unimed.tipo_servico             =   tt-mov.tipo_servico           
            movto_unimed.vl_filme                 =   tt-mov.vl_filme               
            movto_unimed.vl_material              =   tt-mov.vl_material            
            movto_unimed.vl_procedimento          =   tt-mov.vl_procedimento        
            movto_unimed.vl_total                 =   tt-mov.vl_total               .
 
    END.
    
    PUT STREAM s-saida skip.

    RUN pi-close-excel.

    output stream s-saida close.

    OUTPUT CLOSE.
         
    RUN pi-finalizar IN h-acomp.

    OS-COPY value(c-arquivo_1)  V:/TEMP.
   IF l-s-saida THEN DO: 
       OS-COPY value(c-arquivo_2)  V:/TEMP.

   END.
    os-delete value(c-arquivo_1).
    os-delete value(c-arquivo_2).

  
    run utp/ut-msgs.p (input "show", input 17006, input "FOI GERADO ARQUIVOS NO C:/TEMP:~~" + c-arquivo_1a + (IF l-s-saida THEN "," + c-arquivo_2a ELSE "")).
     
    
    
    /*    {include/i-imrun.i prghur/fpp/esbs0009rp.p}
    
        {include/i-imexc.i}
    
        if  session:set-wait-state("") then.
        
        {include/i-imtrm.i tt-param.arq-destino tt-param.destino}
        
        */
   
        
END.
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



