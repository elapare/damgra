&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i espe0013 1.02.00.002 } /*** 010002 ***/

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Fecha Banco Horas Para Compensa‡Æo.                                 */
/* <m¢dulo>:  HCM-PEP.                  */
{prghur/fpp/fp9200.i}


    
&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i espe0013 MPE}
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
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR 
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp

  
/* Parameters Definitions ---                                           */
{bf/buffersHCM.i}

/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
    field v_cdn_empres_usuar like empresa.ep-codigo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as log
    field  i_cdn_funcionario_ini AS INTEGER                                 
    field  i_cdn_funcionario_fim AS INTEGER                                 
    field  c_cdn_empresa_ini  AS CHARACTER                                  
    field  c_cdn_empresa_fim  AS CHARACTER                                  
    field  c_cdn_estab_ini AS CHARACTER                                     
    field  c_cdn_estab_fim AS CHARACTER                                     
    field  c_ccusto_ini AS CHARACTER                                        
    field  c_ccusto_fim AS CHARACTER                                        
    field  i_cdn_turno_ini AS INTEGER                                       
    field  i_cdn_turno_fim AS INTEGER                                       
    field  d_data_mpe_ini AS DATE                                           
    field  d_data_mpe_fim AS DATE                                           
    field  c_cod_unid_lotac_ini LIKE funcionario.cod_unid_lotac             
    field  c_cod_unid_lotac_fim LIKE funcionario.cod_unid_lotac             
    field  i_cdn_plano_Lotac_ini LIKE funcionario.cdn_plano_Lotac           
    field  i_cdn_plano_Lotac_fim LIKE funcionario.cdn_plano_Lotac 
    FIELD  l-mensal AS LOGICAL  
    FIELD  l-horista AS LOGICAL
    FIELD  l-semanal AS LOGICAL
    FIELD  l-quinzenal AS LOGICAL
    FIELD  l-tarefa AS LOGICAL
    FIELD  l-diarista AS LOGICAL
    FIELD  l-simula AS LOGICAL
    FIELD  i-tp-origem AS INTEGER
    FIELD  i-tp-destino AS INTEGER
    
    .       
    




/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-rtf              as char    no-undo.
def var c-modelo-default   as char    no-undo.

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa est  rodando no WebEnabler*/
DEFINE SHARED VARIABLE hWenController AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 bt-arquivo 
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

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 4 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3,
"Excel", 4
     SIZE 38 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.72 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.79.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE c_ccusto_fim AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 TOOLTIP "Centro de custos final".

DEFINE VARIABLE c_ccusto_ini AS CHARACTER FORMAT "X(12)" 
     LABEL "Centro de custo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 TOOLTIP "Centro de custos inicial".

DEFINE VARIABLE c_cdn_empresa_fim AS CHARACTER FORMAT "X(03)"    /*solic-318*/ 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "C¢digo Final da Empresa do Participante".

DEFINE VARIABLE c_cdn_empresa_ini AS CHARACTER FORMAT "X(3)"    /*solic-318*/ 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "C¢digo Inicial da Empresa do Participante".

DEFINE VARIABLE c_cdn_estab_fim AS CHARACTER FORMAT "X(05)"    /*solic-318*/ 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 TOOLTIP "C¢digo Final do Estabelecimento do Participante".

DEFINE VARIABLE c_cdn_estab_ini AS CHARACTER FORMAT "X(05)"    /*solic-318*/ 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 TOOLTIP "C¢digo Inicial do Estabelecimento do Participante".

DEFINE VARIABLE c_cod_unid_lotac_fim AS CHARACTER FORMAT "x(11)" INITIAL "ZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 TOOLTIP "C¢digo Final Unidade Lota‡Æo".

DEFINE VARIABLE c_cod_unid_lotac_ini AS CHARACTER FORMAT "x(11)" 
     LABEL "Unid Lota‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 TOOLTIP "C¢digo Inicial Unidade Lota‡Æo".

DEFINE VARIABLE d_data_mpe_fim AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 TOOLTIP "Data de processamento do ponto final".

DEFINE VARIABLE d_data_mpe_ini AS DATE FORMAT "99/99/9999" 
     LABEL "Data processo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 TOOLTIP "Data de processamento do ponto inicial".

DEFINE VARIABLE i-tp-destino AS INTEGER FORMAT ">9" INITIAL 6 
     LABEL "Banco BH Destino" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "N£mero do Banco de Horas de Destino".

DEFINE VARIABLE i-tp-origem AS INTEGER FORMAT ">9" INITIAL 1 
     LABEL "Banco BH Origem" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "N£mero do Banco de Horas de origem".

DEFINE VARIABLE i_cdn_funcionario_fim AS INTEGER FORMAT "zzzzzzz9" INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "C¢digo Final da Matr¡cula do Funcion rio".

DEFINE VARIABLE i_cdn_funcionario_ini AS INTEGER FORMAT "zzzzzzz9" INITIAL 0 
     LABEL "Matr¡cula" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "C¢digo Inicial da Matr¡cula do Funcion rio".

DEFINE VARIABLE i_cdn_plano_lotac_fim AS INTEGER FORMAT ">>9" INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "C¢digo Final Plano Lota‡Æo".

DEFINE VARIABLE i_cdn_plano_lotac_ini AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Plano Lota‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "C¢digo Inicial Plano Lota‡Æo".

DEFINE VARIABLE i_cdn_turno_fim AS INTEGER FORMAT ">>9" INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "C¢digo do turno Final".

DEFINE VARIABLE i_cdn_turno_ini AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Turno" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "C¢digo do turno inicial".

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-23
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-24
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-25
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-26
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-111
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42.43 BY 2.

DEFINE VARIABLE tg-diarista AS LOGICAL INITIAL yes 
     LABEL "Diarista" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-horista AS LOGICAL INITIAL yes 
     LABEL "Horista" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-mensal AS LOGICAL INITIAL yes 
     LABEL "Mensal" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-quinzenal AS LOGICAL INITIAL yes 
     LABEL "Quinzenal" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-semanal AS LOGICAL INITIAL yes 
     LABEL "Semanal" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-simula AS LOGICAL INITIAL yes 
     LABEL "." 
     VIEW-AS TOGGLE-BOX
     SIZE 3.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-tarefa AS LOGICAL INITIAL yes 
     LABEL "Tarefa" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

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

DEFINE IMAGE im-pg-sel
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-110
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 12.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 7 .

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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 15.04 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 15.04 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 15.04 COL 70 HELP
          "Ajuda"
     RECT-1 AT ROW 14.79 COL 2
     RECT-6 AT ROW 14.38 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     im-pg-imp AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
     RECT-110 AT ROW 2.5 COL 2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15.38
         DEFAULT-BUTTON bt-executar WIDGET-ID 100.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 3.33 COL 17 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 4.42 COL 57 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 4.42 COL 57 HELP
          "Configura‡Æo da impressora"
     c-arquivo AT ROW 4.46 COL 17 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 6.83 COL 16.57 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 2.75 COL 17.57 NO-LABEL
     text-modo AT ROW 6.08 COL 14.86 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 3.04 COL 15.86
     RECT-9 AT ROW 6.29 COL 15.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.5 WIDGET-ID 100.

DEFINE FRAME f-pg-sel
     tg-simula AT ROW 11.29 COL 69.72 WIDGET-ID 140
     c_cdn_empresa_ini AT ROW 1.13 COL 21 COLON-ALIGNED HELP
          "C¢digo Inicial da Empresa do Participante" WIDGET-ID 52
     c_cdn_empresa_fim AT ROW 1.13 COL 42.86 COLON-ALIGNED HELP
          "C¢digo Final da Empresa do Participante" NO-LABEL WIDGET-ID 70
     c_cdn_estab_ini AT ROW 2.13 COL 21 COLON-ALIGNED HELP
          "C¢digo Inicial do Estabelecimento do Participante" WIDGET-ID 54
     c_cdn_estab_fim AT ROW 2.13 COL 42.86 COLON-ALIGNED HELP
          "C¢digo Final do Estabelecimento do Participante" NO-LABEL WIDGET-ID 72
     i_cdn_funcionario_ini AT ROW 3.13 COL 21 COLON-ALIGNED HELP
          "C¢digo Inicial da Matr¡cula do Funcion rio" WIDGET-ID 56
     i_cdn_funcionario_fim AT ROW 3.13 COL 42.86 COLON-ALIGNED HELP
          "C¢digo Final da Matr¡cula do Funcion rio" NO-LABEL WIDGET-ID 74
     i_cdn_turno_ini AT ROW 4.13 COL 21 COLON-ALIGNED HELP
          "C¢digo do turno inicial" WIDGET-ID 114
     i_cdn_turno_fim AT ROW 4.13 COL 42.86 COLON-ALIGNED HELP
          "C¢digo do turno Final" NO-LABEL WIDGET-ID 112
     i_cdn_plano_lotac_ini AT ROW 5.13 COL 21 COLON-ALIGNED HELP
          "C¢digo Inicial Plano Lota‡Æo" WIDGET-ID 76
     i_cdn_plano_lotac_fim AT ROW 5.13 COL 42.86 COLON-ALIGNED HELP
          "C¢digo Final Plano Lota‡Æo" NO-LABEL WIDGET-ID 88
     c_cod_unid_lotac_ini AT ROW 6.13 COL 21 COLON-ALIGNED HELP
          "C¢digo Inicial Unidade Lota‡Æo" WIDGET-ID 78
     c_cod_unid_lotac_fim AT ROW 6.13 COL 42.86 COLON-ALIGNED HELP
          "C¢digo Final Unidade Lota‡Æo" NO-LABEL WIDGET-ID 90
     c_ccusto_ini AT ROW 7.13 COL 21 COLON-ALIGNED HELP
          "Centro de custos inicial" WIDGET-ID 92
     c_ccusto_fim AT ROW 7.13 COL 42.86 COLON-ALIGNED HELP
          "Centro de custos final" NO-LABEL WIDGET-ID 98
     d_data_mpe_ini AT ROW 8.17 COL 21 COLON-ALIGNED HELP
          "Data de processamento do ponto inicial" WIDGET-ID 106
     d_data_mpe_fim AT ROW 8.17 COL 42.86 COLON-ALIGNED HELP
          "Data de processamento do ponto final" NO-LABEL WIDGET-ID 104
     i-tp-origem AT ROW 9.21 COL 66.72 COLON-ALIGNED HELP
          "N£mero do Banco de Horas de origem" WIDGET-ID 136
     i-tp-destino AT ROW 10.25 COL 66.72 COLON-ALIGNED HELP
          "N£mero do Banco de Horas de Destino" WIDGET-ID 138
     tg-mensal AT ROW 10.04 COL 6.29 WIDGET-ID 116
     tg-semanal AT ROW 10.04 COL 18.72 WIDGET-ID 124
     tg-tarefa AT ROW 10.04 COL 30.72 WIDGET-ID 128
     tg-horista AT ROW 10.88 COL 6.29 WIDGET-ID 118
     tg-quinzenal AT ROW 10.88 COL 18.72 WIDGET-ID 126
     tg-diarista AT ROW 10.88 COL 30.72 WIDGET-ID 130
     "Categoria Salarial" VIEW-AS TEXT
          SIZE 13.86 BY .75 AT ROW 9.46 COL 4.14 WIDGET-ID 134
     "Somente Simula‡Æo:" VIEW-AS TEXT
          SIZE 14 BY .75 AT ROW 11.25 COL 67.72 RIGHT-ALIGNED WIDGET-ID 142
     IMAGE-11 AT ROW 1.13 COL 36.43 WIDGET-ID 60
     IMAGE-12 AT ROW 1.13 COL 41.57 WIDGET-ID 58
     IMAGE-13 AT ROW 2.13 COL 36.43 WIDGET-ID 62
     IMAGE-14 AT ROW 2.13 COL 41.57 WIDGET-ID 64
     IMAGE-15 AT ROW 3.13 COL 36.43 WIDGET-ID 66
     IMAGE-16 AT ROW 3.13 COL 41.57 WIDGET-ID 68
     IMAGE-17 AT ROW 5.13 COL 36.43 WIDGET-ID 80
     IMAGE-18 AT ROW 5.13 COL 41.57 WIDGET-ID 82
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.83
         SIZE 76.86 BY 11.25 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-pg-sel
     IMAGE-19 AT ROW 6.13 COL 36.43 WIDGET-ID 84
     IMAGE-20 AT ROW 6.13 COL 41.57 WIDGET-ID 86
     IMAGE-21 AT ROW 7.13 COL 36.43 WIDGET-ID 94
     IMAGE-22 AT ROW 7.13 COL 41.57 WIDGET-ID 96
     IMAGE-23 AT ROW 8.17 COL 36.43 WIDGET-ID 100
     IMAGE-24 AT ROW 8.17 COL 41.57 WIDGET-ID 102
     IMAGE-25 AT ROW 4.13 COL 36.43 WIDGET-ID 108
     IMAGE-26 AT ROW 4.13 COL 41.57 WIDGET-ID 110
     RECT-111 AT ROW 9.88 COL 2.57 WIDGET-ID 132
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.83
         SIZE 76.86 BY 11.25 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-relat ASSIGN
         HIDDEN             = YES
         TITLE              = "Fecha Banco Horas 10 horas Extras semanais"
         HEIGHT             = 15.5
         WIDTH              = 81.14
         MAX-HEIGHT         = 36.38
         MAX-WIDTH          = 194.86
         VIRTUAL-HEIGHT     = 36.38
         VIRTUAL-WIDTH      = 194.86
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
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON bt-config-impr IN FRAME f-pg-imp
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR c-arquivo IN FRAME f-pg-imp
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rs-destino IN FRAME f-pg-imp
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rs-execucao IN FRAME f-pg-imp
   NO-ENABLE                                                            */
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
   Custom                                                               */
/* SETTINGS FOR FILL-IN c_cdn_empresa_fim IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c_cdn_empresa_ini IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c_cdn_estab_fim IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c_cdn_estab_ini IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-tp-destino IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-tp-origem IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR TEXT-LITERAL "Somente Simula‡Æo:"
          SIZE 14 BY .75 AT ROW 11.25 COL 67.72 RIGHT-ALIGNED           */

/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
   NO-ENABLE                                                            */
ASSIGN 
       RECT-6:MANUAL-HIGHLIGHT IN FRAME f-relat = TRUE.

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
ON END-ERROR OF w-relat /* Fecha Banco Horas 10 horas Extras semanais */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Fecha Banco Horas 10 horas Extras semanais */
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


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME i-tp-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-tp-destino w-relat
ON VALUE-CHANGED OF i-tp-destino IN FRAME f-pg-sel /* Banco BH Destino */
DO:
    IF SELF:SCREEN-VALUE <> "1" AND SELF:SCREEN-VALUE <> "6" THEN DO:
       SELF:SCREEN-VALUE = "6".
    END.
    IF SELF:SCREEN-VALUE <> "1" AND SELF:SCREEN-VALUE <> "6" THEN
        RETURN NO-APPLY.
   IF SELF:SCREEN-VALUE = "1" THEN DO:
      i-tp-origem:SCREEN-VALUE = "6".
  END.
  ELSE DO:
    i-tp-origem:SCREEN-VALUE = "1".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-tp-origem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-tp-origem w-relat
ON VALUE-CHANGED OF i-tp-origem IN FRAME f-pg-sel /* Banco BH Origem */
DO:
    IF SELF:SCREEN-VALUE <> "1" AND SELF:SCREEN-VALUE <> "5" THEN DO:
       SELF:SCREEN-VALUE = "1".
    END.
        
  IF SELF:SCREEN-VALUE = "1" THEN DO:
      i-tp-destino:SCREEN-VALUE = "5".
  END.
  ELSE DO:
    i-tp-destino:SCREEN-VALUE = "1".
  END.
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
/*Alterado 15/02/2005 - tech1007 - Evento alterado para correto funcionamento dos novos widgets
  utilizados para a funcionalidade de RTF*/
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = YES.
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = yes
                   bt-arquivo:visible      = yes
                   bt-config-impr:visible  = NO.
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "espe0013" "1.00.00.000"}

/*:T inicializa‡äes do template de relat¢rio */
{include/i-rpini.i }

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-rplbl.i im-pg-sel }

 

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    c_cdn_empresa_ini = STRING({cdp\poloestab.i 420}). /*solic-318*/ 
    c_cdn_empresa_fim = STRING({cdp\poloestab.i 420}). /*solic-318*/ 
    c_cdn_estab_ini  = STRING({cdp\poloestab.i 422}). /*solic-318*/ 
    c_cdn_estab_fim  = STRING({cdp\poloestab.i 422}). /*solic-318*/ 

    RUN enable_UI.
    
    {include/i-rpmbl.i }
 
    assign c-arq-old = replace(c-arq-old,".lst",".xls").
   ASSIGN rs-destino:SCREEN-VALUE IN FRAME f-pg-imp = "4".

  /* c_cdn_empresa_ini:SCREEN-VALUE IN FRAME f-pg-sel =  v_cdn_empres_usuar .
   c_cdn_empresa_fim:SCREEN-VALUE IN FRAME f-pg-sel =  v_cdn_empres_usuar .  */
   


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
  ENABLE im-pg-imp im-pg-sel RECT-110 bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY tg-simula c_cdn_empresa_ini c_cdn_empresa_fim c_cdn_estab_ini 
          c_cdn_estab_fim i_cdn_funcionario_ini i_cdn_funcionario_fim 
          i_cdn_turno_ini i_cdn_turno_fim i_cdn_plano_lotac_ini 
          i_cdn_plano_lotac_fim c_cod_unid_lotac_ini c_cod_unid_lotac_fim 
          c_ccusto_ini c_ccusto_fim d_data_mpe_ini d_data_mpe_fim i-tp-origem 
          i-tp-destino tg-mensal tg-semanal tg-tarefa tg-horista tg-quinzenal 
          tg-diarista 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE tg-simula i_cdn_funcionario_ini i_cdn_funcionario_fim i_cdn_turno_ini 
         i_cdn_turno_fim i_cdn_plano_lotac_ini i_cdn_plano_lotac_fim 
         c_cod_unid_lotac_ini c_cod_unid_lotac_fim c_ccusto_ini c_ccusto_fim 
         d_data_mpe_ini d_data_mpe_fim tg-mensal tg-semanal tg-tarefa 
         tg-horista tg-quinzenal tg-diarista IMAGE-11 IMAGE-12 IMAGE-13 
         IMAGE-14 IMAGE-15 IMAGE-16 IMAGE-17 IMAGE-18 IMAGE-19 IMAGE-20 
         IMAGE-21 IMAGE-22 IMAGE-23 IMAGE-24 IMAGE-25 IMAGE-26 RECT-111 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 bt-arquivo 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  VIEW w-relat.
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

    
    /*:T Coloque aqui as valida‡äes da p gina de Digita‡Æo, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p gina e colocar
       o focus no campo com problemas */
    /*browse br-digita:SET-REPOSITIONED-ROW (browse br-digita:DOWN, "ALWAYS":U).*/
    
    
    /*:T Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */
    
    
    
    /*:T Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.v_cdn_empres_usuar           = v_cdn_empres_usuar
           tt-param.usuario                      = c-seg-usuario
           tt-param.destino                      = input frame f-pg-imp rs-destino
           tt-param.data-exec                    = today
           tt-param.hora-exec                    = TIME                         
           tt-param.i_cdn_funcionario_ini        = input frame f-pg-sel i_cdn_funcionario_ini                                 
           tt-param.i_cdn_funcionario_fim        = input frame f-pg-sel i_cdn_funcionario_fim                                 
           tt-param.c_cdn_empresa_ini            = input frame f-pg-sel c_cdn_empresa_ini                                     
           tt-param.c_cdn_empresa_fim            = input frame f-pg-sel c_cdn_empresa_fim                                     
           tt-param.c_cdn_estab_ini              = input frame f-pg-sel c_cdn_estab_ini                                      
           tt-param.c_cdn_estab_fim              = input frame f-pg-sel c_cdn_estab_fim                                      
           tt-param.c_ccusto_ini                 = input frame f-pg-sel c_ccusto_ini                                     
           tt-param.c_ccusto_fim                 = input frame f-pg-sel c_ccusto_fim                                     
           tt-param.i_cdn_turno_ini              = input frame f-pg-sel i_cdn_turno_ini                                     
           tt-param.i_cdn_turno_fim              = input frame f-pg-sel i_cdn_turno_fim                                     
           tt-param.d_data_mpe_ini               = input frame f-pg-sel d_data_mpe_ini                                   
           tt-param.d_data_mpe_fim               = input frame f-pg-sel d_data_mpe_fim                                   
           tt-param.c_cod_unid_lotac_ini         = input frame f-pg-sel c_cod_unid_lotac_ini        
           tt-param.c_cod_unid_lotac_fim         = input frame f-pg-sel c_cod_unid_lotac_fim        
           tt-param.i_cdn_plano_Lotac_ini        = input frame f-pg-sel i_cdn_plano_Lotac_ini      
           tt-param.i_cdn_plano_Lotac_fim        = input frame f-pg-sel i_cdn_plano_Lotac_fim        
           tt-param.l-mensal                     = input frame f-pg-sel tg-mensal    
           tt-param.l-horista                    = input frame f-pg-sel tg-horista       
           tt-param.l-semanal                    = input frame f-pg-sel tg-semanal       
           tt-param.l-quinzenal                  = input frame f-pg-sel tg-quinzenal     
           tt-param.l-tarefa                     = input frame f-pg-sel tg-tarefa        
           tt-param.l-diarista                   = input frame f-pg-sel tg-diarista  
           tt-param.l-simula                     = input frame f-pg-sel tg-simula          
           tt-param.i-tp-origem                  = input frame f-pg-sel i-tp-origem     
           tt-param.i-tp-destino                 = input frame f-pg-sel i-tp-destino    
        .
            





    
    /*Alterado 14/02/2005 - tech1007 - Alterado o teste para verificar se a op‡Æo de RTF est  selecionada*/
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    /*Fim alteracao 14/02/2005*/

    /*:T Coloque aqui a/l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    
    
    
    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i prghur/pep/espe0013rp.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
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
     Tables specified for this Window, and there are no
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

