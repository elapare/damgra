&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/*******************************************************************************
** tem essa opção
**    PROGRAMA: esp/ftp/ESFT0808.w
**        DATA: Mar‡o de 2017
**
**    OBJETIVO: Relat¢rio de Faturamento / Margem de Contribui‡Æo em Excel
**
**       AUTOR: Edson-Damgra
**
**      VERSAO: 5.06.00.MTQ - <data>
**
** Este fonte e de propriedade exclusiva da MANTIQUEIRA ALIMENTOS LTDA, 
** sua reproducao parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
********************************************************************************/
{include/i-prgvrs.i ESFT0808 5.06.00.MTQ}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
&ENDIF

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
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGIMP f-pg-imp

&GLOBAL-DEFINE RTF   YES
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

DEF TEMP-TABLE tt-param NO-UNDO /*coment rio*/
    FIELD destino               AS INT
    FIELD execucao              AS INT
    FIELD arquivo               AS CHAR FORMAT "x(35)"
    FIELD usuario               AS CHAR FORMAT "x(12)"
    FIELD data-exec             AS DATE
    FIELD enviar-email          AS LOG
    FIELD destinatarios         AS CHAR
    FIELD hora-exec             AS INT
    FIELD modelo-rtf            AS CHAR FORMAT "x(35)"
    FIELD l-habilitaRtf         AS LOG
    /*FIELD v_num_tip_aces_usuar  AS INT*/
    FIELD c-cod-estabel-ini     AS CHAR /* Folder Sel */
    FIELD c-cod-estabel-fim     AS CHAR
    FIELD c-serie-docto-ini     AS CHAR
    FIELD c-serie-docto-fim     AS CHAR
    FIELD c-nat-operacao-ini    AS CHAR
    FIELD c-nat-operacao-fim    AS CHAR
    FIELD i-cod-emitente-ini    AS INT
    FIELD i-cod-emitente-fim    AS INT
    FIELD c-nro-docto-ini       AS CHAR
    FIELD c-nro-docto-fim       AS CHAR
    FIELD dt-emisdoc-ini        AS DATE
    FIELD dt-emisdoc-fim        AS DATE
    FIELD dt-trandoc-ini        AS DATE
    FIELD dt-trandoc-fim        AS DATE
    FIELD c-item-ini            AS CHAR
    FIELD c-item-fim            AS CHAR
    FIELD i-ge-codigo-ini       AS INT 
    FIELD i-ge-codigo-fim       AS INT
    FIELD c-fm-codigo-ini       AS CHAR  
    FIELD c-fm-codigo-fim       AS CHAR
    FIELD c-fm-cod-com-ini      AS CHAR
    FIELD c-fm-cod-com-fim      AS CHAR /* Fim - Folder Sel */
    FIELD tip-natoper           AS INT  /* Folder Par */
    FIELD i-tipo-custo          AS INT
    FIELD l-estabel             AS LOG
    FIELD l-grupo               AS LOG
    FIELD l-familia             AS LOG
    FIELD l-familia-com         AS LOG
    FIELD l-item                AS LOG
    FIELD l-emitente            AS LOG
    FIELD l-representante       AS LOG
    FIELD l-estado              AS LOG
    FIELD l-cidade              AS LOG
    FIELD l-documento           AS LOG  
    FIELD l-n-gera-dupli        AS log /* Fim - Folder Par */.   

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical        no-undo.
/*def var c-arq-digita       as char           no-undo.*/
def var c-terminal         as char           no-undo.
def var c-rtf              as char           no-undo.
def var c-modelo-default   as char           no-undo.
def var wh-label-sel     as widget-handle    no-undo.
def var wh-label-cla     as widget-handle    no-undo.
def var wh-label-par     as widget-handle    no-undo.
def var wh-label-dig     as widget-handle    no-undo.
def var wh-label-imp     as widget-handle    no-undo.
def var wh-group         as widget-handle    no-undo.
def var wh-child         as widget-handle    no-undo.
def var c-list-folders   as char             no-undo.
def var i-current-folder as integer          no-undo.
def var i-new-folder     as integer          no-undo.
def var c-aux            as char             no-undo.
def var c-modelo-aux     as char             no-undo. 
def var l-rtf            as logical init  no no-undo.
def var i-aux            as integer          no-undo.

/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

def new shared var dt-ini as date format "99/99/9999" label "".
def new shared var v-log as logical format "sim/nao" label "" view-as toggle-box.

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

def new shared var c-desc-arquivo-ini AS CHAR format "x(20)" initial "" no-undo.
def new shared var c-desc-arquivo-fim AS CHAR format "x(20)" initial "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" no-undo.

/*
DEF NEW SHARED VAR v-cod-estabel-ini  LIKE docum-est.cod-estabel NO-UNDO INIT "".
DEF NEW SHARED VAR v-cod-estabel-fim  LIKE docum-est.cod-estabel INIT "zzzzz" NO-UNDO.
DEF NEW SHARED VAR v-serie-docto-ini  LIKE docum-est.cod-estabel NO-UNDO INIT "".
DEF NEW SHARED VAR v-serie-docto-fim  LIKE docum-est.cod-estabel INIT "zzz" NO-UNDO.
DEF NEW SHARED VAR v-nat-operacao-ini LIKE docum-est.nat-operacao NO-UNDO INIT "".
DEF NEW SHARED VAR v-nat-operacao-fim LIKE docum-est.nat-operacao INIT "799zzz" NO-UNDO.
DEF NEW SHARED VAR v-cod-emitente-ini LIKE docum-est.cod-emitente NO-UNDO INIT 0.
DEF NEW SHARED VAR v-cod-emitente-fim LIKE docum-est.cod-emitente init 9999999 NO-UNDO.
DEF NEW SHARED VAR v-nro-docto-ini    LIKE docum-est.nro-docto NO-UNDO INIT "".
DEF NEW SHARED VAR v-nro-docto-fim    LIKE docum-est.nro-docto INIT "9999999" NO-UNDO.
DEF NEW SHARED VAR v-dt-emisdoc-ini   LIKE docum-est.dt-emissao NO-UNDO.
DEF NEW SHARED VAR v-dt-emisdoc-fim   LIKE docum-est.dt-emissao NO-UNDO.
DEF NEW SHARED VAR v-dt-trandoc-ini   LIKE docum-est.dt-trans NO-UNDO.
DEF NEW SHARED VAR v-dt-trandoc-fim   LIKE docum-est.dt-trans NO-UNDO.

DEF NEW SHARED VAR v-tot              as integer FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF NEW SHARED VAR v-tot-item         as integer FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF NEW SHARED VAR v-tot-estab        as integer FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEF NEW SHARED VAR v-tot-geral        as integer FORMAT "->>,>>>,>>9.99" NO-UNDO.
*/

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa est  rodando no WebEnabler*/
DEFINE SHARED VARIABLE hWenController AS HANDLE NO-UNDO.

{include/i_fclpreproc.i}     
define new global shared variable h-facelift as handle no-undo.

def new shared var v_email_usuario_select 
    as character 
    format "x(2000)":U 
    view-as editor max-chars 2000
    size 30 by 1
    bgcolor 15 font 2
    label "Selecionados"
    column-label "Selecionados"
    no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rect-rtf RECT-7 RECT-9 rs-destino bt-arquivo ~
bt-config-impr c-arquivo editor-destinatarios bt-destinatarios ~
l-habilitaRtf bt-modelo-rtf rs-execucao text-rtf text-modelo-rtf 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo editor-destinatarios ~
l-habilitaRtf c-modelo-rtf rs-execucao text-rtf text-modelo-rtf 

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

DEFINE BUTTON bt-destinatarios 
     IMAGE-UP FILE "IMAGE/im-ran_a.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran_a.gif":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-modelo-rtf 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE c-modelo-rtf AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE editor-destinatarios AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
     SIZE 68.72 BY 1.75 TOOLTIP "Endere‡os de e-mail dos destinat rios".

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modelo-rtf AS CHARACTER FORMAT "X(256)":U INITIAL "Modelo:" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE text-rtf AS CHARACTER FORMAT "X(256)":U INITIAL "Rich Text Format(RTF)" 
      VIEW-AS TEXT 
     SIZE 20.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3,
"E-mail", 4
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.72 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74.86 BY 3.58.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE RECTANGLE rect-rtf
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 3.54.

DEFINE VARIABLE l-habilitaRtf AS LOGICAL INITIAL no 
     LABEL "RTF" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-tip-natoper AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Devolu‡Æo", 1,
"Sa¡da", 2,
"Todas", 3
     SIZE 17 BY 2.29 NO-UNDO.

DEFINE VARIABLE rs-tipo-custo AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Custo M‚dio", 1,
"Custo Produ‡Æo", 2
     SIZE 19.14 BY 2.29 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 3.25.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 3.25.

DEFINE VARIABLE l-cidade AS LOGICAL INITIAL no 
     LABEL "Mostrar Cidade" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE l-documento AS LOGICAL INITIAL no 
     LABEL "Mostrar Documento (Mostra tudo)" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .88 NO-UNDO.

DEFINE VARIABLE l-emitente AS LOGICAL INITIAL no 
     LABEL "Mostrar Emitente" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE l-estabel AS LOGICAL INITIAL no 
     LABEL "Mostrar Estabelecimento" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .88 NO-UNDO.

DEFINE VARIABLE l-estado AS LOGICAL INITIAL no 
     LABEL "Mostrar Estado" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE l-familia AS LOGICAL INITIAL no 
     LABEL "Mostrar Fam¡lia" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .88 NO-UNDO.

DEFINE VARIABLE l-familia-com AS LOGICAL INITIAL no 
     LABEL "Mostrar Fam¡lia Comercial" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .88 NO-UNDO.

DEFINE VARIABLE l-grupo AS LOGICAL INITIAL no 
     LABEL "Mostrar Grupo de Estoque" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .88 NO-UNDO.

DEFINE VARIABLE l-item AS LOGICAL INITIAL no 
     LABEL "Mostrar Item" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE l-n-gera-duplica AS LOGICAL INITIAL no 
     LABEL "NF que nÆo geram duplicatas" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.86 BY .88 NO-UNDO.

DEFINE VARIABLE l-representante AS LOGICAL INITIAL no 
     LABEL "Mostrar Representante" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-emitente-fim AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999999 
     CONTEXT-HELP-ID 1
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-emitente-ini AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "C¢d Emitente" 
     CONTEXT-HELP-ID 1
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-fim AS CHARACTER FORMAT "X(05)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     CONTEXT-HELP-ID 2
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-ini AS CHARACTER FORMAT "X(05)":U 
     LABEL "Estab" 
     CONTEXT-HELP-ID 1
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-emisdoc-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     CONTEXT-HELP-ID 4
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-emisdoc-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt Emis Docto" 
     CONTEXT-HELP-ID 3
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-trandoc-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     CONTEXT-HELP-ID 4
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-trandoc-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt Trans Docto" 
     CONTEXT-HELP-ID 3
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fm-cod-com-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     CONTEXT-HELP-ID 2
     VIEW-AS FILL-IN 
     SIZE 19.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fm-cod-com-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Fam¡lia Comercial" 
     CONTEXT-HELP-ID 1
     VIEW-AS FILL-IN 
     SIZE 19.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fm-codigo-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     CONTEXT-HELP-ID 2
     VIEW-AS FILL-IN 
     SIZE 19.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fm-codigo-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Fam¡lia" 
     CONTEXT-HELP-ID 1
     VIEW-AS FILL-IN 
     SIZE 19.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ge-codigo-fim AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 21 
     CONTEXT-HELP-ID 2
     VIEW-AS FILL-IN 
     SIZE 19.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ge-codigo-ini AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 20 
     LABEL "Grupo de Estoque" 
     CONTEXT-HELP-ID 1
     VIEW-AS FILL-IN 
     SIZE 19.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-item-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     CONTEXT-HELP-ID 2
     VIEW-AS FILL-IN 
     SIZE 19.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-item-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     CONTEXT-HELP-ID 1
     VIEW-AS FILL-IN 
     SIZE 19.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nat-operacao-fim AS CHARACTER FORMAT "X(6)":U 
     CONTEXT-HELP-ID 1
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nat-operacao-ini AS CHARACTER FORMAT "X(6)":U 
     LABEL "Nat Oper" 
     CONTEXT-HELP-ID 1
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nro-docto-fim AS CHARACTER FORMAT "X(15)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     CONTEXT-HELP-ID 2
     VIEW-AS FILL-IN 
     SIZE 19.72 BY .88 TOOLTIP "N£mero do Dodumento Fiscal Fim" NO-UNDO.

DEFINE VARIABLE fi-nro-docto-ini AS CHARACTER FORMAT "X(07)":U 
     LABEL "Doc Fiscal" 
     CONTEXT-HELP-ID 1
     VIEW-AS FILL-IN 
     SIZE 19.72 BY .88 TOOLTIP "N£mero do Documento Fiscal In¡cio" NO-UNDO.

DEFINE VARIABLE fi-serie-docto-fim AS CHARACTER FORMAT "X(05)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     CONTEXT-HELP-ID 2
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-serie-docto-ini AS CHARACTER FORMAT "X(05)":U 
     LABEL "Serie" 
     CONTEXT-HELP-ID 1
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88 TOOLTIP "Estabelecimento In¡cio".

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88 TOOLTIP "Estabelecimento Fim".

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE IMAGE im-pg-imp
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-par
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-sel
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE RECTANGLE rt-bt-background
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 12.38
     FGCOLOR 0 .

DEFINE RECTANGLE rt-folder-bottom
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-left
     EDGE-PIXELS 0    
     SIZE .43 BY 12.21
     BGCOLOR 15 .

DEFINE RECTANGLE rt-folder-right
     EDGE-PIXELS 0    
     SIZE .43 BY 12.17
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-top
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 1.63 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 2.71 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 2.71 COL 43.29 HELP
          "Configura‡Æo da impressora"
     c-arquivo AT ROW 2.75 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     editor-destinatarios AT ROW 2.75 COL 3.29 NO-LABEL WIDGET-ID 22
     bt-destinatarios AT ROW 2.75 COL 72.14 HELP
          "Incluir os endere‡os de e-mail" WIDGET-ID 24
     l-habilitaRtf AT ROW 5.67 COL 3.29
     c-modelo-rtf AT ROW 7.46 COL 3 HELP
          "Nome do arquivo de modelo do relat¢rio" NO-LABEL
     bt-modelo-rtf AT ROW 7.46 COL 43 HELP
          "Escolha do nome do arquivo"
     rs-execucao AT ROW 9.71 COL 2.86 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.04 COL 3.86 NO-LABEL
     text-rtf AT ROW 5 COL 1.14 COLON-ALIGNED NO-LABEL
     text-modelo-rtf AT ROW 6.79 COL 1.14 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 8.96 COL 1.14 COLON-ALIGNED NO-LABEL
     rect-rtf AT ROW 5.29 COL 2
     RECT-7 AT ROW 1.33 COL 2.14
     RECT-9 AT ROW 9.17 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.29 BY 11.5 WIDGET-ID 100.

DEFINE FRAME f-pg-sel
     fi-cod-estabel-ini AT ROW 1.25 COL 17 COLON-ALIGNED WIDGET-ID 2
     fi-cod-estabel-fim AT ROW 1.25 COL 48.72 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fi-serie-docto-ini AT ROW 2.25 COL 17 COLON-ALIGNED WIDGET-ID 50
     fi-serie-docto-fim AT ROW 2.25 COL 48.72 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     fi-nro-docto-ini AT ROW 3.25 COL 17 COLON-ALIGNED WIDGET-ID 90
     fi-nro-docto-fim AT ROW 3.25 COL 48.86 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     fi-nat-operacao-ini AT ROW 4.25 COL 17 COLON-ALIGNED WIDGET-ID 94
     fi-nat-operacao-fim AT ROW 4.25 COL 48.86 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     fi-cod-emitente-ini AT ROW 5.25 COL 17 COLON-ALIGNED WIDGET-ID 96
     fi-cod-emitente-fim AT ROW 5.25 COL 48.86 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     fi-dt-emisdoc-ini AT ROW 6.25 COL 17 COLON-ALIGNED WIDGET-ID 30
     fi-dt-emisdoc-fim AT ROW 6.25 COL 48.86 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     fi-dt-trandoc-ini AT ROW 7.25 COL 17 COLON-ALIGNED WIDGET-ID 42
     fi-dt-trandoc-fim AT ROW 7.25 COL 48.86 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     fi-item-ini AT ROW 8.25 COL 17 COLON-ALIGNED WIDGET-ID 114
     fi-item-fim AT ROW 8.25 COL 48.86 COLON-ALIGNED NO-LABEL WIDGET-ID 116
     fi-ge-codigo-ini AT ROW 9.25 COL 17 COLON-ALIGNED WIDGET-ID 120
     fi-ge-codigo-fim AT ROW 9.25 COL 48.86 COLON-ALIGNED NO-LABEL WIDGET-ID 118
     fi-fm-codigo-ini AT ROW 10.25 COL 17 COLON-ALIGNED WIDGET-ID 124
     fi-fm-codigo-fim AT ROW 10.25 COL 48.86 COLON-ALIGNED NO-LABEL WIDGET-ID 122
     fi-fm-cod-com-ini AT ROW 11.25 COL 17 COLON-ALIGNED WIDGET-ID 128
     fi-fm-cod-com-fim AT ROW 11.25 COL 48.86 COLON-ALIGNED NO-LABEL WIDGET-ID 126
     IMAGE-1 AT ROW 1.25 COL 39.72
     IMAGE-2 AT ROW 1.25 COL 47.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 11.63 WIDGET-ID 100.

DEFINE FRAME f-relat
     bt-executar AT ROW 15.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 15.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 15.54 COL 70 HELP
          "Ajuda"
     rt-bt-background AT ROW 15.29 COL 2
     rt-folder-bottom AT ROW 14.75 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     im-pg-imp AT ROW 1.5 COL 33.57
     im-pg-sel AT ROW 1.5 COL 2.14
     im-pg-par AT ROW 1.5 COL 17.86 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 16
         DEFAULT-BUTTON bt-executar WIDGET-ID 100.

DEFINE FRAME f-pg-par
     l-tip-natoper AT ROW 2.5 COL 5 NO-LABEL WIDGET-ID 2
     rs-tipo-custo AT ROW 2.5 COL 42 NO-LABEL WIDGET-ID 32
     l-estabel AT ROW 5.25 COL 5 WIDGET-ID 12
     l-n-gera-duplica AT ROW 5.25 COL 42 WIDGET-ID 30
     l-grupo AT ROW 6 COL 5 WIDGET-ID 10
     l-familia AT ROW 6.75 COL 5 WIDGET-ID 14
     l-familia-com AT ROW 7.5 COL 5 WIDGET-ID 16
     l-item AT ROW 8.25 COL 5 WIDGET-ID 18
     l-emitente AT ROW 9 COL 5 WIDGET-ID 20
     l-representante AT ROW 9.75 COL 5 WIDGET-ID 24
     l-estado AT ROW 10.5 COL 5 WIDGET-ID 26
     l-cidade AT ROW 11.25 COL 5 WIDGET-ID 28
     l-documento AT ROW 12 COL 5 WIDGET-ID 22
     "Tipo Nat Oper" VIEW-AS TEXT
          SIZE 13.57 BY .67 AT ROW 1.5 COL 4 WIDGET-ID 8
     "Tipo Custo" VIEW-AS TEXT
          SIZE 13.57 BY .67 AT ROW 1.5 COL 42 WIDGET-ID 38
     RECT-10 AT ROW 1.75 COL 3 WIDGET-ID 6
     RECT-11 AT ROW 1.75 COL 41 WIDGET-ID 36
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.75
         SIZE 77 BY 12 WIDGET-ID 200.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-relat
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-relat ASSIGN
         HIDDEN             = YES
         TITLE              = "ESFT0808 - Relat¢rio de Faturamento e Margem de Contribui‡Æo em Excel"
         HEIGHT             = 15.96
         WIDTH              = 81.14
         MAX-HEIGHT         = 22.33
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.33
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
/* REPARENT FRAME */
ASSIGN FRAME f-pg-par:FRAME = FRAME f-relat:HANDLE.

/* SETTINGS FOR FRAME f-pg-imp
   FRAME-NAME                                                           */
/* SETTINGS FOR EDITOR c-modelo-rtf IN FRAME f-pg-imp
   NO-ENABLE                                                            */
ASSIGN 
       editor-destinatarios:READ-ONLY IN FRAME f-pg-imp        = TRUE.

/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

ASSIGN 
       text-modelo-rtf:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Modelo:".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu‡Æo".

ASSIGN 
       text-rtf:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Rich Text Format(RTF)".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FRAME f-pg-sel
   Custom                                                               */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR RECTANGLE rt-bt-background IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-bottom IN FRAME f-relat
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
ON END-ERROR OF w-relat /* ESFT0808 - Relat¢rio de Obrigacoes Fiscais */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* ESFT0808 - Relat¢rio de Obrigacoes Fiscais */
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


&Scoped-define SELF-NAME bt-destinatarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-destinatarios w-relat
ON CHOOSE OF bt-destinatarios IN FRAME f-pg-imp
DO:
    run eszoom/z01esfn017.p.
    if v_email_usuario_select <> "" then
         assign editor-destinatarios:screen-value = v_email_usuario_select.
    else assign editor-destinatarios:screen-value = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-relat
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:
   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-modelo-rtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modelo-rtf w-relat
ON CHOOSE OF bt-modelo-rtf IN FRAME f-pg-imp
DO:
    def var c-arq-conv  as char no-undo.
    def var l-ok as logical no-undo.

    assign c-modelo-rtf = replace(input frame {&frame-name} c-modelo-rtf, "/", "~\").
    SYSTEM-DIALOG GET-FILE c-arq-conv
       FILTERS "*.rtf" "*.rtf",
               "*.*" "*.*"
       DEFAULT-EXTENSION "rtf"
       INITIAL-DIR "modelos" 
       MUST-EXIST
       USE-FILENAME
       UPDATE l-ok.
    if  l-ok = yes then
        assign c-modelo-rtf:screen-value in frame {&frame-name}  = replace(c-arq-conv, "~\", "/"). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME editor-destinatarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL editor-destinatarios w-relat
ON F5 OF editor-destinatarios IN FRAME f-pg-imp
DO:
  
    run c:\dev\zoom-email-usuario.p.

    if v_email_usuario_select <> "" then
         assign editor-destinatarios:screen-value = v_email_usuario_select.
    else assign editor-destinatarios:screen-value = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-imp w-relat
ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par w-relat
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
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
&Scoped-define SELF-NAME l-habilitaRtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-habilitaRtf w-relat
ON VALUE-CHANGED OF l-habilitaRtf IN FRAME f-pg-imp /* RTF */
DO:
    &IF "{&RTF}":U = "YES":U &THEN
    RUN pi-habilitaRtf.
    &endif
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME l-tip-natoper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-tip-natoper w-relat
ON VALUE-CHANGED OF l-tip-natoper IN FRAME f-pg-par
DO:
  assign l-tip-natoper.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino w-relat
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
        CASE rs-destino:SCREEN-VALUE IN FRAME f-pg-imp:
            WHEN "1" THEN DO:

                /* Alterado em 31/05/2005 - tech1139 - Altera»„es FO 1152.814*/
                &IF "{&RTF}":U = "YES":U &THEN
                IF l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp <> "No" THEN DO:
                   ASSIGN l-rtf = YES
                          c-modelo-aux = c-modelo-rtf:SCREEN-VALUE IN FRAME f-pg-imp.
                END.
                &endif
                /* Alterado em 31/05/2005 - tech1139 - Altera»„es FO 1152.814*/

                ASSIGN c-arquivo                                = c-imp-old
                       c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = c-imp-old
                       c-arquivo:SENSITIVE IN FRAME f-pg-imp    = NO
                       c-arquivo:VISIBLE IN FRAME f-pg-imp      = YES
                       bt-arquivo:VISIBLE IN FRAME f-pg-imp     = NO
                       bt-config-impr:VISIBLE IN FRAME f-pg-imp = YES
                       bt-destinatarios:visible                 = no
                       editor-destinatarios:visible             = no
                /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                verificar se o RTF estÿ ativo*/
                       &IF "{&RTF}":U = "YES":U &THEN
                       l-habilitaRtf:sensitive  = NO
                       l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "No"
                       l-habilitaRtf = NO
                       &endif
                /*Fim alteracao 17/02/2005*/
                       .
                if c-imp-old = "" then
                   run pi-impres-pad.
            END.
            WHEN "2" THEN DO:
                ASSIGN c-arquivo = IF rs-execucao = 1 
                                   THEN c-arq-old
                                   ELSE c-arq-old-batch
                       c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = c-arquivo
                       c-arquivo:SENSITIVE IN FRAME f-pg-imp    = YES
                       c-arquivo:VISIBLE IN FRAME f-pg-imp      = YES
                       bt-arquivo:VISIBLE IN FRAME f-pg-imp     = YES
                       bt-config-impr:VISIBLE IN FRAME f-pg-imp = NO
                       bt-destinatarios:visible                 = no
                       editor-destinatarios:visible             = no
                       /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                         verificar se o RTF estÿ ativo*/
                       &IF "{&RTF}":U = "YES":U &THEN
                       l-habilitaRtf:sensitive  = YES
                       &endif
                       /*Fim alteracao 17/02/2005*/
                       .

                /* Alterado em 31/05/2005 - tech1139 - Altera»„es FO 1152.814*/
                &IF "{&RTF}":U = "YES":U &THEN
                IF l-rtf = YES THEN DO:
                   ASSIGN l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "Yes"
                          l-rtf = NO
                          c-modelo-rtf = c-modelo-aux
                          c-modelo-rtf:SCREEN-VALUE IN FRAME f-pg-imp = c-modelo-aux.
                END.
                &endif
                /* Alterado em 31/05/2005 - tech1139 - Altera»„es FO 1152.814*/
                
            END.
            WHEN "3" THEN DO:
                ASSIGN c-arquivo                                = ""
                       c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = c-arquivo
                       c-arquivo:VISIBLE IN FRAME f-pg-imp      = NO
                       bt-arquivo:VISIBLE IN FRAME f-pg-imp     = NO                       
                       bt-config-impr:VISIBLE IN FRAME f-pg-imp = NO
                       bt-destinatarios:visible                 = no
                       editor-destinatarios:visible             = no
                       /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                         verificar se o RTF estÿ ativo*/
                       &IF "{&RTF}":U = "YES":U &THEN
                       l-habilitaRtf:sensitive  = YES
                       &endif
                       /*Fim alteracao 17/02/2005*/
                       .
                /*Alterado 15/02/2005 - tech1007 - Teste para funcionar corretamente no WebEnabler*/
                &IF "{&RTF}":U = "YES":U &THEN
                IF VALID-HANDLE(hWenController) THEN DO:
                    ASSIGN l-habilitaRtf:sensitive  = NO
                           l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "No"
                           l-habilitaRtf = NO.
                END.
                &endif
                /*Fim alteracao 15/02/2005*/                                        

                /* Alterado em 31/05/2005 - tech1139 - Altera»„es FO 1152.814*/
                &IF "{&RTF}":U = "YES":U &THEN
                IF l-rtf = YES THEN DO:
                    ASSIGN l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "Yes"
                           l-rtf = NO
                           c-modelo-rtf = c-modelo-aux
                           c-modelo-rtf:SCREEN-VALUE IN FRAME f-pg-imp = c-modelo-aux.
                END.
                &endif
                /* Alterado em 31/05/2005 - tech1139 - Altera»„es FO 1152.814*/

            END.
            WHEN "4" THEN DO:
                ASSIGN c-arquivo = IF rs-execucao = 1 
                                   THEN c-arq-old
                                   ELSE c-arq-old-batch
                       c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = c-arquivo
                       c-arquivo:SENSITIVE IN FRAME f-pg-imp    = no
                       c-arquivo:VISIBLE IN FRAME f-pg-imp      = no
                       bt-arquivo:VISIBLE IN FRAME f-pg-imp     = no
                       bt-config-impr:VISIBLE IN FRAME f-pg-imp = NO
                       bt-destinatarios:visible                 = yes
                       editor-destinatarios:visible             = yes
                       &IF "{&RTF}":U = "YES":U &THEN
                       l-habilitaRtf:sensitive  = YES
                       &endif
                       .

                &IF "{&RTF}":U = "YES":U &THEN
                IF l-rtf = YES THEN DO:
                   ASSIGN l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "Yes"
                          l-rtf = NO
                          c-modelo-rtf = c-modelo-aux
                          c-modelo-rtf:SCREEN-VALUE IN FRAME f-pg-imp = c-modelo-aux.
                END.
                &endif
            END.
        END CASE.
        &IF "{&RTF}":U = "YES":U &THEN
        RUN pi-habilitaRtf.
        &endif
        /*Fim alteracao 04/03/2005*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-execucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao w-relat
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-imp
DO:
   {masters/i-rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME rs-tipo-custo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tipo-custo w-relat
ON VALUE-CHANGED OF rs-tipo-custo IN FRAME f-pg-par
DO:
  assign l-tip-natoper.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESFT0808" "5.06.00.MTQ"}

{masters/i-aplica-facelift.i}
/*:T inicializa‡äes do template de relat¢rio */

{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{masters/i-rplbl.i} 


/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

assign fi-dt-emisdoc-ini = today - 30.
assign fi-dt-emisdoc-fim = today.
assign fi-dt-trandoc-ini = today - 30.
assign fi-dt-trandoc-fim = today.

assign fi-nat-operacao-ini = "100".
assign fi-nat-operacao-fim = "799zzz".
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.

    assign bt-destinatarios:visible     in frame f-pg-imp = no
           editor-destinatarios:visible in frame f-pg-imp = no.


    {include/i-rpmbl.i}

         apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
  
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
  ENABLE im-pg-imp im-pg-sel im-pg-par bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY l-tip-natoper rs-tipo-custo l-estabel l-n-gera-duplica l-grupo 
          l-familia l-familia-com l-item l-emitente l-representante l-estado 
          l-cidade l-documento 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE RECT-10 RECT-11 l-tip-natoper rs-tipo-custo l-estabel l-n-gera-duplica 
         l-grupo l-familia l-familia-com l-item l-emitente l-representante 
         l-estado l-cidade l-documento 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY fi-cod-estabel-ini fi-cod-estabel-fim fi-serie-docto-ini 
          fi-serie-docto-fim fi-nro-docto-ini fi-nro-docto-fim 
          fi-nat-operacao-ini fi-nat-operacao-fim fi-cod-emitente-ini 
          fi-cod-emitente-fim fi-dt-emisdoc-ini fi-dt-emisdoc-fim 
          fi-dt-trandoc-ini fi-dt-trandoc-fim fi-item-ini fi-item-fim 
          fi-ge-codigo-ini fi-ge-codigo-fim fi-fm-codigo-ini fi-fm-codigo-fim 
          fi-fm-cod-com-ini fi-fm-cod-com-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE fi-cod-estabel-ini fi-cod-estabel-fim fi-serie-docto-ini 
         fi-serie-docto-fim fi-nro-docto-ini fi-nro-docto-fim 
         fi-nat-operacao-ini fi-nat-operacao-fim fi-cod-emitente-ini 
         fi-cod-emitente-fim fi-dt-emisdoc-ini fi-dt-emisdoc-fim 
         fi-dt-trandoc-ini fi-dt-trandoc-fim fi-item-ini fi-item-fim 
         fi-ge-codigo-ini fi-ge-codigo-fim fi-fm-codigo-ini fi-fm-codigo-fim 
         fi-fm-cod-com-ini fi-fm-cod-com-fim IMAGE-1 IMAGE-2 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo editor-destinatarios l-habilitaRtf c-modelo-rtf 
          rs-execucao text-rtf text-modelo-rtf 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rect-rtf RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
         editor-destinatarios bt-destinatarios l-habilitaRtf bt-modelo-rtf 
         rs-execucao text-rtf text-modelo-rtf 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  VIEW w-relat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-objects w-relat 
PROCEDURE local-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-objects':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-relat 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var r-tt-digita as rowid no-undo.

do on error undo, return error on stop  undo, return error:
    {include/i-rpexa.i}

    if input frame f-pg-imp rs-destino = 4 and
       input frame f-pg-imp editor-destinatarios = ""  then do:
        run utp/ut-msgs.p (input "show":U, input 90006, input "").
        apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
        apply "ENTRY":U to c-arquivo in frame f-pg-imp.
        return error.
    end.

    /*14/02/2005 - tech1007 - Alterada condicao para nÆo considerar mai o RTF como destino*/
    if input frame f-pg-imp rs-destino = 2 and
       input frame f-pg-imp rs-execucao = 1 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show":U, input 73, input "").
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.

    /*14/02/2005 - tech1007 - Teste efetuado para nao permitir modelo em branco*/
    &IF "{&RTF}":U = "YES":U &THEN
    IF ( INPUT FRAME f-pg-imp c-modelo-rtf = "" AND
         INPUT FRAME f-pg-imp l-habilitaRtf = "Yes" ) OR
       ( SEARCH(INPUT FRAME f-pg-imp c-modelo-rtf) = ? AND
         input frame f-pg-imp rs-execucao = 1 AND
         INPUT FRAME f-pg-imp l-habilitaRtf = "Yes" )
         THEN DO:
        run utp/ut-msgs.p (input "show":U, input 73, input "").        
        apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
        /*30/12/2004 - tech1007 - Evento removido pois causa problemas no WebEnabler*/
        /*apply "CHOOSE":U to bt-modelo-rtf in frame f-pg-imp.*/
        return error.
    END.
    &endif
    /*Fim teste Modelo*/
    
    /*:T Coloque aqui as valida‡äes da p gina de Digita‡Æo, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p gina e colocar
       o focus no campo com problemas */
    /*browse br-digita:SET-REPOSITIONED-ROW (browse br-digita:DOWN, "ALWAYS":U).*/
        
    /*:T Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */
            
    /*:T Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    create  tt-param.
    assign  tt-param.usuario            = c-seg-usuario
            tt-param.execucao           = input frame f-pg-imp rs-execucao
            tt-param.data-exec          = today
            tt-param.hora-exec          = time
            &IF "{&RTF}":U = "YES":U &THEN
            tt-param.modelo-rtf         = INPUT FRAME f-pg-imp c-modelo-rtf
            /*Alterado 14/02/2005 - tech1007 - Armazena a informa‡Æo se o RTF est  habilitado ou nÆo*/
            tt-param.l-habilitaRtf      = INPUT FRAME f-pg-imp l-habilitaRtf
            /*Fim alteracao 14/02/2005*/ 
            &endif
            tt-param.c-cod-estabel-ini  = input FRAME f-pg-sel fi-cod-estabel-ini
            tt-param.c-cod-estabel-fim  = input FRAME f-pg-sel fi-cod-estabel-fim
            tt-param.c-serie-docto-ini  = input FRAME f-pg-sel fi-serie-docto-ini
            tt-param.c-serie-docto-fim  = input FRAME f-pg-sel fi-serie-docto-fim
            tt-param.c-nro-docto-ini    = input FRAME f-pg-sel fi-nro-docto-ini
            tt-param.c-nro-docto-fim    = input FRAME f-pg-sel fi-nro-docto-fim
            tt-param.c-nat-operacao-ini = input FRAME f-pg-sel fi-nat-operacao-ini
            tt-param.c-nat-operacao-fim = input FRAME f-pg-sel fi-nat-operacao-fim
            tt-param.i-cod-emitente-ini = input FRAME f-pg-sel fi-cod-emitente-ini
            tt-param.i-cod-emitente-fim = input FRAME f-pg-sel fi-cod-emitente-fim
            tt-param.dt-emisdoc-ini     = input FRAME f-pg-sel fi-dt-emisdoc-ini
            tt-param.dt-emisdoc-fim     = input FRAME f-pg-sel fi-dt-emisdoc-fim
            tt-param.dt-trandoc-ini     = input FRAME f-pg-sel fi-dt-trandoc-ini
            tt-param.dt-trandoc-fim     = input FRAME f-pg-sel fi-dt-trandoc-fim
            tt-param.c-item-ini         = input FRAME f-pg-sel fi-item-ini
            tt-param.c-item-fim         = input FRAME f-pg-sel fi-item-fim
            tt-param.i-ge-codigo-ini    = input FRAME f-pg-sel fi-ge-codigo-ini
            tt-param.i-ge-codigo-fim    = input FRAME f-pg-sel fi-ge-codigo-fim
            tt-param.c-fm-codigo-ini    = input FRAME f-pg-sel fi-fm-codigo-ini
            tt-param.c-fm-codigo-fim    = input FRAME f-pg-sel fi-fm-codigo-fim
            tt-param.c-fm-cod-com-ini   = input FRAME f-pg-sel fi-fm-cod-com-ini
            tt-param.c-fm-cod-com-fim   = input FRAME f-pg-sel fi-fm-cod-com-fim

            tt-param.tip-natoper        = input frame f-pg-par l-tip-natoper
            tt-param.i-tipo-custo       = input FRAME f-pg-par rs-tipo-custo
            tt-param.l-estabel          = input FRAME f-pg-par l-estabel
            tt-param.l-grupo            = input FRAME f-pg-par l-grupo
            tt-param.l-familia          = input FRAME f-pg-par l-familia
            tt-param.l-familia-com      = input FRAME f-pg-par l-familia-com
            tt-param.l-item             = input FRAME f-pg-par l-item
            tt-param.l-emitente         = input FRAME f-pg-par l-emitente
            tt-param.l-representante    = input FRAME f-pg-par l-representante
            tt-param.l-estado           = input FRAME f-pg-par l-estado
            tt-param.l-cidade           = input FRAME f-pg-par l-cidade
            tt-param.l-documento        = input FRAME f-pg-par l-documento
            tt-param.l-n-gera-dupli     = input FRAME f-pg-par l-n-gera-dupli
        .
    
    if input frame f-pg-imp rs-destino = 4 then
         assign tt-param.destino         = 2           
                tt-param.enviar-email    = yes
                tt-param.destinatarios   = input frame f-pg-imp editor-destinatarios.
    else assign tt-param.destino         = input frame f-pg-imp rs-destino
                tt-param.enviar-email    = no
                tt-param.destinatarios   = "".

    /*Alterado 14/02/2005 - tech1007 - Alterado o teste para verificar se a op‡Æo de RTF est  selecionada*/
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    /*Fim alteracao 14/02/2005*/

    /*:T Coloque aqui a/l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    
    if tt-param.enviar-email and not tt-param.l-habilitaRtf and
       substr(tt-param.arquivo, length(tt-param.arquivo) - 2, 3) <> CAPS("TXT") then
        substr(tt-param.arquivo, length(tt-param.arquivo) - 2, 3) = "XLSX".


    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    /* {include/i-rpexb.i}*/
    
    /*SESSION:SET-WAIT-STATE("general":U).*/
    
    /*{include/i-rprun.i xxp/xx9999rp.p}*/
    {include/i-rprun.i ftp/ESFT0808rp.p}
    
    /*{include/i-rpexc.i}*/
    
    /*SESSION:SET-WAIT-STATE("":U).*/
    
    /*{include/i-rptrm.i}*/
end.
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

