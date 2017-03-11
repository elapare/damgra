&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME

&Scoped-define WINDOW-NAME w-relat

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 

/***************************************************************************
 **Programa.: esft0017.W 
 **Autor....: JOS ROBERTO REIS CAMPOS
 **Objetivo.: Gera Arquivo Texto de Remessas para Armazenagem (layout - TRPAULO)
 **Data.....: 10/01/2006
 ** Otimizado Performance em 14/02/2007
 ***************************************************************************/

{include/i-prgvrs.i esft0017 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA f-pg-cla
&GLOBAL-DEFINE PGDIG f-pg-dig
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

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
    FIELD c-dt-emis-nota       LIKE nota-fiscal.dt-emis-nota
    field c-cod-estabel        like nota-fiscal.cod-estabel
    FIELD c-nr-nota-fis-ini    LIKE nota-fiscal.nr-nota-fis
    FIELD c-nr-nota-fis-fim    LIKE nota-fiscal.nr-nota-fis
    FIELD c-cod-emitente       LIKE nota-fiscal.cod-emitente
    FIELD c-nat-operacao-ini   LIKE nota-fiscal.nat-operacao
    FIELD c-nat-operacao-fim   LIKE nota-fiscal.nat-operacao
    FIELD c-serie              LIKE nota-fiscal.serie
.

DEFINE TEMP-TABLE tt-digita no-undo
    FIELD marca                AS CHAR                        FORMAT "x(1)"  LABEL "Marca"                 
    FIELD nr-nota-fis          LIKE nota-fiscal.nr-nota-fis   FORMAT "x(10)" LABEL "Nota Fiscal"
    FIELD serie                LIKE nota-fiscal.serie         FORMAT "x(5)"  LABEL "S‚rie"
    FIELD dt-emis-nota         LIKE nota-fiscal.dt-emis-nota  FORMAT "99/99/9999" LABEL "Dt.EmissÆo"
    FIELD cod-emitente         LIKE nota-fiscal.cod-emitente  FORMAT ">>>>>>>9" LABEL "Emitente"
    FIELD nat-operacao         LIKE nota-fiscal.nat-operacao  FORMAT "x(8)"  LABEL "Nat.Oper"
    INDEX chave IS PRIMARY UNIQUE nr-nota-fis
                                  serie.

define buffer b-tt-digita for tt-digita.
Def    Buffer bf-digita   For tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */
DEF VAR aux-lote           LIKE item-doc-est.lote NO-UNDO.
Def Var qt-lote            As Dec     No-undo.
def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var h-acomp            as handle no-undo.
def var v-num-reg-lidos    as int    no-undo.


DEFINE VARIABLE c-nf   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-nf-x AS CHARACTER  NO-UNDO.



/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 
DEFINE VARIABLE c-conta-contabil-jr LIKE conta-contab.conta-contabil NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */

&Scoped-define FRAME-NAME f-pg-cla 
&Scoped-define FRAME-NAME f-pg-sel 

&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita */

&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.marca tt-digita.nr-nota-fis tt-digita.dt-emis-nota tt-digita.cod-emitente tt-digita.nat-operacao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.marca tt-digita.nr-nota-fis tt-digita.dt-emis-nota tt-digita.cod-emitente tt-digita.nat-operacao   
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

&Scoped-Define ENABLED-OBJECTS rs-classif 
&Scoped-Define DISPLAYED-OBJECTS rs-classif 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rs-classif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Data Transa‡Æo/ Item", 1
     SIZE 28 BY 1 NO-UNDO.

DEFINE BUTTON bt-alterar 
     LABEL "Desmarca" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-inserir 
     LABEL "Marca" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-recuperar 
     LABEL " " 
     SIZE 15 BY 1.

DEFINE BUTTON bt-retirar 
     LABEL "Marca Todos" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-salvar 
     LABEL "Desmarca Todos" 
     SIZE 15 BY 1.

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

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.71.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71.29 BY 4.15.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71.29 BY 4.15.

DEFINE VARIABLE texto-conta AS CHARACTER FORMAT "X(12)":U INITIAL "Emitente" 
      VIEW-AS TEXT 
     SIZE 15.86 BY .45 NO-UNDO.

DEFINE VARIABLE texto-origem AS CHARACTER FORMAT "X(14)":U INITIAL "Notas Fiscais" 
      VIEW-AS TEXT 
     SIZE 15.86 BY .45 NO-UNDO.


DEFINE VARIABLE c-dt-emis-nota AS date label "Data EmissÆo N.F." FORMAT "99/99/9999"
     INITIAL TODAY
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     TOOLTIP "Informe a Data de EmissÆo" NO-UNDO.  

DEFINE VARIABLE c-cod-estabel AS CHAR label "Estabelecimento" FORMAT "x(3)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88
     TOOLTIP "Informe C¢digo do Estabelecimento " NO-UNDO.

DEFINE VARIABLE c-serie AS CHAR label "S‚rie das NFs." FORMAT "x(5)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88
     TOOLTIP "Informe a S‚rie da N.Fiscal " NO-UNDO.

DEFINE VARIABLE c-nr-nota-fis-ini AS CHAR label "Nota Fiscal" FORMAT "x(07)":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88
     TOOLTIP "Informe N£mero da Nota Fiscal Inicial" NO-UNDO.

DEFINE VARIABLE c-nr-nota-fis-fim AS char FORMAT "x(07)"
     INITIAL "ZZZZZZZ"
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     TOOLTIP "Informe N£mero da Nota Fiscal Final" NO-UNDO.

DEFINE VARIABLE c-cod-emitente AS INT label "Emitente" FORMAT ">>>>>>9"
     INITIAL 17261
     VIEW-AS FILL-IN 
     SIZE 8 BY .88
     TOOLTIP "Informe Cod.Emitente" NO-UNDO.

DEFINE VARIABLE c-nat-operacao-ini AS char label "Nat.Opera‡Æo" FORMAT "x(8)"  
     INITIAL "5905GT"
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     TOOLTIP "Informe a Nat.Opera‡Æo Inicial" NO-UNDO.

DEFINE VARIABLE c-nat-operacao-fim AS char FORMAT "x(8)" INITIAL "5905GT"
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     TOOLTIP "Informe N£mero da Nota Fiscal Final" NO-UNDO.

DEFINE VARIABLE c-nome-ab-cli AS char label "Nome" FORMAT "x(45)"
     INITIAL "TRPAULO"
     VIEW-AS FILL-IN 
     SIZE 27 BY .88 NO-UNDO.


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


DEFINE IMAGE IMAGE-47
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-54
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-55
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-56
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-86
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-87
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-88
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-89
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-paletes 
     LABEL "Selecione" 
     SIZE 10 BY 1.

DEFINE IMAGE im-pg-cla
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-dig
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-imp
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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-digita SCROLLING.
&ANALYZE-RESUME


/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita w-relat _FREEFORM
  QUERY br-digita DISPLAY         
        tt-digita.marca           
        tt-digita.nr-nota-fis     
        tt-digita.dt-emis-nota    
        tt-digita.cod-emitente    
        tt-digita.nat-operacao    
  ENABLE
    tt-digita.marca 
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9
         BGCOLOR 15 .


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
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"

     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"

    bt-paletes AT ROW 14.54 COL 25 HELP
         "Monta Tabela de Paletes"

     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"

     rt-folder AT ROW 2.5 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     im-pg-cla AT ROW 1.5 COL 17.84   
     im-pg-dig AT ROW 1.5 COL 33.57
     im-pg-imp AT ROW 1.5 COL 49.28     
/*     im-pg-par AT ROW 1.5 COL 33.57     */
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-paletes.

DEFINE FRAME f-pg-sel

   c-cod-estabel
    at row 1.75 col 18 colon-aligned
    font 1

   c-dt-emis-nota
    at row 2.75 col 18 colon-aligned
    font 1

   c-cod-emitente
    at row 3.75 col 18 colon-aligned
    font 1


   c-nome-ab-cli
    NO-LABEL
    at row 3.75 col 41 colon-aligned
    font 1    

   c-nr-nota-fis-ini
    at row 6.15 col 18 colon-aligned
    font 1

   c-nr-nota-fis-fim
     at row 6.15 col 50 NO-LABEL 
     font 1
   
   c-nat-operacao-ini
    at row 7.15 col 18 colon-aligned
    font 1

   c-nat-operacao-fim
     at row 7.15 col 50 no-label
     font 1

    c-serie
     at row 8.15 col 18 colon-aligned
     font 1



   texto-conta  AT ROW 1    COL 2.00 COLON-ALIGNED NO-LABEL
   texto-origem AT ROW 5.60 COL 2.00 COLON-ALIGNED NO-LABEL

   RECT-19 AT ROW 05.75 COL 02       
   RECT-20 AT ROW 01.15 COL 02
   IMAGE-1 AT ROW 06.15 COL 40
   IMAGE-2 AT ROW 06.15 COL 44
   IMAGE-3 AT ROW 07.15 COL 40
   IMAGE-4 AT ROW 07.15 COL 44

   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
   SIDE-LABELS NO-UNDERLINE THREE-D
   AT COL 3 ROW 2.85
   SIZE 76.86 BY 10.62.

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 1.5 COL 8 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.31.

DEFINE FRAME f-pg-dig
     br-digita AT ROW 1 COL 1
     bt-inserir AT ROW 10 COL 1
     bt-alterar AT ROW 10 COL 16
     bt-retirar AT ROW 10 COL 31
     bt-salvar AT ROW 10 COL 46
     bt-recuperar AT ROW 10 COL 61
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.31
         SIZE 76.86 BY 10.15.


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
         TITLE              = "Certificado de Qualidade"
         HEIGHT             = 15
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
/* SETTINGS FOR FRAME f-pg-cla
                                                                        */
/* SETTINGS FOR FRAME f-pg-dig
                                                                        */
/* BROWSE-TAB br-digita 1 f-pg-dig */
/* SETTINGS FOR BUTTON bt-alterar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-retirar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-salvar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-imp
                                                                        */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L */                
                        
                        


ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu‡Æo".

ASSIGN 
       texto-conta:PRIVATE-DATA IN FRAME f-pg-sel     = 
                "Emitente".

ASSIGN 
       texto-origem:PRIVATE-DATA IN FRAME f-pg-sel     = 
                "Notas Fiscais".


/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-relat
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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY br-digita FOR EACH tt-digita.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-digita */
&ANALYZE-RESUME

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
ON END-ERROR OF w-relat /* NFs Recebidas para Controle  Etiquetas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat 
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-digita
&Scoped-define FRAME-NAME f-pg-dig

&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON DEL OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose' to bt-retirar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
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
        display
            tt-digita.marca              
            tt-digita.nr-nota-fis        
            tt-digita.dt-emis-nota       
            tt-digita.cod-emitente       
            tt-digita.nat-operacao       
            with browse br-digita. 

    end.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ENTER OF br-digita IN FRAME f-pg-dig
ANYWHERE
DO:
  apply 'tab' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON INS OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose' to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-END OF br-digita IN FRAME f-pg-dig
DO:
   apply 'entry' to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-HOME OF br-digita IN FRAME f-pg-dig
DO:
  apply 'entry' to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-ENTRY OF br-digita IN FRAME f-pg-dig
DO:
   /* trigger para inicializar campos da temp table de digita‡Æo */
/*   if  br-digita:new-row in frame f-pg-dig then do:
       assign tt-digita.nome:screen-value in browse br-digita = string(today, "99/99/9999").
   end.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
DO:
    /*  aqui que a grava‡Æo da linha da temp-table ? efetivada.
       Por‚m as valida‡äes dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment rio */


    if br-digita:NEW-ROW in frame f-pg-dig then 
    do transaction on error undo, return no-apply:
        create tt-digita.
        assign input browse br-digita tt-digita.marca              
               INPUT BROWSE br-digita tt-digita.nr-nota-fis        
               input browse br-digita tt-digita.dt-emis-nota       
               input browse br-digita tt-digita.cod-emitente       
               input browse br-digita tt-digita.nat-operacao.       
            
            br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    end.
    else do transaction on error undo, return no-apply:
        
            assign input browse br-digita tt-digita.marca              
                   INPUT BROWSE br-digita tt-digita.nr-nota-fis        
                   input browse br-digita tt-digita.dt-emis-nota       
                   input browse br-digita tt-digita.cod-emitente       
                   input browse br-digita tt-digita.nat-operacao.       

      
       if num-results("br-digita") > 0 then
      
            display
                tt-digita.marca              
                tt-digita.nr-nota-fis        
                tt-digita.dt-emis-nota       
                tt-digita.cod-emitente       
                tt-digita.nat-operacao       
                with browse br-digita. 
        
   end.
 
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-relat
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
/*   {include/ajuda.i}   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-alterar w-relat
ON CHOOSE OF bt-alterar IN FRAME f-pg-dig /* Alterar */
DO:
    assign tt-digita.marca:screen-value in browse br-digita = " ".
    get current br-digita.
    assign input browse br-digita tt-digita.marca.       
    apply 'entry' to tt-digita.marca in browse br-digita. 

end.



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

ON leave OF c-cod-emitente IN FRAME f-pg-sel 
DO:
    
    FIND FIRST emitente WHERE
        emitente.cod-emitente = input frame f-pg-sel c-cod-emitente
        NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN 
        ASSIGN c-nome-ab-cli:SCREEN-VALUE IN FRAME f-pg-sel = emitente.nome-abrev.
    ELSE
        ASSIGN c-nome-ab-cli:SCREEN-VALUE IN FRAME f-pg-sel = "NÇO EXISTE".

    RETURN.
        
END.

ON leave OF c-nr-nota-fis-ini IN FRAME f-pg-sel 
DO:

   DEFINE VARIABLE c-nf   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE c-nf-x AS CHARACTER  NO-UNDO.


   ASSIGN c-nf = "0000000"
          c-nf-x = input frame f-pg-sel c-nr-nota-fis-ini.

   ASSIGN substring(c-nf,(8 - LENGTH(c-nf-x)),LENGTH(c-nf-x)) = c-nf-x
          c-nr-nota-fis-ini:SCREEN-VALUE IN FRAME f-pg-sel = c-nf.

        
END.


ON leave OF c-nr-nota-fis-fim IN FRAME f-pg-sel 
DO:

   DEFINE VARIABLE c-nf   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE c-nf-x AS CHARACTER  NO-UNDO.


   ASSIGN c-nf = "0000000"
          c-nf-x = input frame f-pg-sel c-nr-nota-fis-fim.

   ASSIGN substring(c-nf,(8 - LENGTH(c-nf-x)),LENGTH(c-nf-x)) = c-nf-x
          c-nr-nota-fis-fim:SCREEN-VALUE IN FRAME f-pg-sel = c-nf.

        
END.




&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-paletes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-paletes w-relat
ON CHOOSE OF bt-paletes IN FRAME f-relat /* Carrega Paletes */
DO:
    FIND FIRST estabelec WHERE
        estabelec.cod-estabel = input frame f-pg-sel c-cod-estabel
        NO-LOCK NO-ERROR.
    IF NOT AVAIL estabelec THEN DO:
        MESSAGE "Estabelecimento Errado"
            view-as alert-box ERROR TITLE "Aten‡Æo !".
        apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
        apply "ENTRY":U to c-cod-estabel in frame f-pg-sel.
        RETURN.          
    END.
    
    FIND FIRST emitente WHERE
        emitente.cod-emitente = input frame f-pg-sel c-cod-emitente
        NO-LOCK NO-ERROR.
    IF NOT AVAIL emitente THEN DO:
        MESSAGE "Emitente NÆo Existe"
            view-as alert-box ERROR TITLE "Aten‡Æo !".
        apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
        apply "ENTRY":U to c-cod-emitente in frame f-pg-sel.
        RETURN.
    END.
        
    run pi-desabilita-folder. 

END.

/* _UIB-CODE-BLOCK-END */

&ANALYZE-RESUME

&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-relat
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Fechar */
DO:
   apply "close" to this-procedure.
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

   apply "close" to this-procedure.

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-inserir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inserir w-relat
ON CHOOSE OF bt-inserir IN FRAME f-pg-dig /* Inserir */
DO:

    assign tt-digita.marca:screen-value in browse br-digita = "*".
    get current br-digita.
    assign input browse br-digita tt-digita.marca.       
    apply 'entry' to tt-digita.marca in browse br-digita. 


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-recuperar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-recuperar w-relat
ON CHOOSE OF bt-recuperar IN FRAME f-pg-dig /* Recuperar */
DO:
    {include/i-rprcd.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-retirar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retirar w-relat
ON CHOOSE OF bt-retirar IN FRAME f-pg-dig /* Retirar */
DO:

    FOR EACH tt-digita.
        ASSIGN tt-digita.marca = "*".
    END.
    open query br-digita for each tt-digita.
    apply 'entry' to tt-digita.marca in browse br-digita. 

                             /*JR
    if  br-digita:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-digita.
        delete tt-digita.
        if  br-digita:delete-current-row() in frame f-pg-dig then.
    end.
                           JR  */
    if num-results("br-digita") = 0 then
        assign bt-alterar:SENSITIVE in frame f-pg-dig = no
               bt-retirar:SENSITIVE in frame f-pg-dig = no
               bt-salvar:SENSITIVE in frame f-pg-dig  = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salvar w-relat
ON CHOOSE OF bt-salvar IN FRAME f-pg-dig /* Salvar */
DO:

    FOR EACH tt-digita.
        ASSIGN tt-digita.marca = " ".
    END.
    open query br-digita for each tt-digita.
    apply 'entry' to tt-digita.marca in browse br-digita. 
   
END.

/* _UIB-CODE-BLOCK-END */

/*
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-cla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-cla w-relat
ON MOUSE-SELECT-CLICK OF im-pg-cla IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */

*/

&ANALYZE-RESUME
&Scoped-define FRAME-NAME f-relat
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
/*
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par w-relat
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
  run pi-troca-pagina.

END.

/* _UIB-CODE-BLOCK-END */
*/

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


&Scoped-define FRAME-NAME f-pg-cla
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 

on LEAVE OF tt-digita.marca in browse br-digita do:
   run pi-leave-pallete.
end.


/* ***************************  Main Block  *************************** */


DEFINE VARIABLE c-computador AS CHARACTER FORMAT "x(20)" NO-UNDO.
DEFINE VARIABLE c-clientname AS CHARACTER FORMAT "x(20)" NO-UNDO.
    
{include/i-win.i}
RUN utp/ut-utils.p PERSISTENT SET h-prog.


RUN GetComputerName IN h-prog(OUTPUT c-computador).
run GetEnv in h-prog(input "CLIENTNAME", output c-clientname).

DELETE PROCEDURE h-prog.


ASSIGN c-cod-estabel = "422"
       c-serie       = "20".



/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "esft0017" "2.06.00.001"}

/* inicializa‡äes do template de relat¢rio */
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
    
    RUN enable_UI.
    
    assign im-pg-dig:sensitive in frame f-relat = no
           wh-label-dig:fgcolor                 = 7.

    assign im-pg-cla:sensitive in frame f-relat = no
           wh-label-cla:fgcolor                 = 7.
                       
    assign im-pg-imp:sensitive in frame f-relat = no
           wh-label-imp:fgcolor                 = 7.

    /*if i-cod-termo:load-mouse-pointer("image/lupa.cur") in frame f-pg-par then.*/
    {include/i-rpmbl.i im-pg-cla}  

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
  ENABLE /*bt-executar*/ bt-cancelar bt-paletes bt-ajuda /*im-pg-cla*/ im-pg-dig /*im-pg-imp*/ 
        /* im-pg-par */im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.

  {&OPEN-BROWSERS-IN-QUERY-f-relat}

   
   DISPLAY  
       c-cod-estabel  
       c-serie
       c-dt-emis-nota     
       c-cod-emitente 
       c-nome-ab-cli
       c-nr-nota-fis-ini 
       c-nr-nota-fis-fim 
       c-nat-operacao-ini         
       c-nat-operacao-fim         
   WITH FRAME f-pg-sel IN WINDOW w-relat.

   ENABLE
       c-cod-estabel 
       c-serie
       c-dt-emis-nota     
       c-cod-emitente       
       c-nr-nota-fis-ini 
       c-nr-nota-fis-fim 
       c-nat-operacao-ini         
       c-nat-operacao-fim         
   WITH FRAME f-pg-sel IN WINDOW w-relat.

  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  ENABLE rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-arquivo bt-config-impr c-arquivo rs-execucao RECT-7 
         RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}

  ENABLE br-digita bt-inserir bt-recuperar 
      WITH FRAME f-pg-dig IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-dig}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-desabilita-folder w-relat 
PROCEDURE pi-desabilita-folder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
Do:
     /** Montando a tabela para tt-digita ***/
     run utp/ut-acomp.p persistent set h-acomp.

     {utp/ut-liter.i Gerando_movimento * I}

     run pi-inicializar in h-acomp (input "Selecionando os Lotes ...Aguarde").  
     
     for each nota-fiscal no-lock
             where nota-fiscal.cod-emitente  = INPUT FRAME f-pg-sel c-cod-emitente     AND 
                   nota-fiscal.dt-emis-nota  = INPUT FRAME f-pg-sel c-dt-emis-nota     AND 
                   nota-fiscal.cod-estabel   = INPUT FRAME f-pg-sel c-cod-estabel      AND 
                   nota-fiscal.serie         = INPUT FRAME f-pg-sel c-serie            AND
                   nota-fiscal.nr-nota-fis  >= INPUT FRAME f-pg-sel c-nr-nota-fis-ini  AND 
                   nota-fiscal.nr-nota-fis  <= INPUT FRAME f-pg-sel c-nr-nota-fis-fim  AND 
                   nota-fiscal.nat-operacao >= INPUT FRAME f-pg-sel c-nat-operacao-ini AND 
                   nota-fiscal.nat-operacao <= INPUT FRAME f-pg-sel c-nat-operacao-fim AND 
                   nota-fiscal.dt-cancela = ? 
                   USE-INDEX ch-emi-nota :
             
             FIND tt-digita WHERE
                  tt-digita.serie              = nota-fiscal.serie       AND
                  tt-digita.nr-nota-fis        = nota-fiscal.nr-nota-fis
                  NO-ERROR.

             IF NOT AVAIL tt-digita THEN 
                  CREATE tt-digita.

             ASSIGN tt-digita.nr-nota-fis    = nota-fiscal.nr-nota-fis   
                    tt-digita.serie          = nota-fiscal.serie
                    tt-digita.cod-emitente   = nota-fiscal.cod-emitente 
                    tt-digita.dt-emis-nota   = nota-fiscal.dt-emis-nota        
                    tt-digita.nat-operacao   = nota-fiscal.nat-operacao
                    tt-digita.marca      = "*".   

             assign v-num-reg-lidos = v-num-reg-lidos + 1.
             run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
             
     END.
      
       assign im-pg-dig:sensitive in frame f-relat = yes
              wh-label-dig:fgcolor                 = ?
              im-pg-sel:sensitive in frame f-relat = NO 
              wh-label-sel:fgcolor                 = 7.
       apply "mouse-select-click" to im-pg-dig in frame f-relat.
     /*** habilita **/
       do transaction:
          open query br-digita for each tt-digita.
          apply 'entry' to tt-digita.marca in browse br-digita. 
       end.
    
    run pi-finalizar in h-acomp.
    enable bt-inserir bt-alterar bt-salvar bt-retirar with frame f-pg-dig.
    disable bt-recuperar WITH FRAME f-pg-dig.
    ENABLE bt-executar WITH FRAME f-relat.
    DISABLE bt-paletes WITH FRAME f-relat.
 end.    
  
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
def var r_rowid as rowid no-undo.

do on error undo, return error
   on stop  undo, return error:     

  {include/i-rpexa.i}
   
    Do:
        
    For Each  tt-digita:
        assign r_rowid = rowid(tt-digita).
             
    End.
  End. 
    
        
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

   ASSIGN tt-param.c-dt-emis-nota         = INPUT FRAME f-pg-sel c-dt-emis-nota      
          tt-param.c-cod-estabel          = INPUT FRAME f-pg-sel c-cod-estabel       
          tt-param.c-serie                = INPUT FRAME f-pg-sel c-serie       
          tt-param.c-nr-nota-fis-ini      = INPUT FRAME f-pg-sel c-nr-nota-fis-ini   
          tt-param.c-nr-nota-fis-fim      = INPUT FRAME f-pg-sel c-nr-nota-fis-fim   
          tt-param.c-cod-emitente         = INPUT FRAME f-pg-sel c-cod-emitente      
          tt-param.c-nat-operacao-ini     = INPUT FRAME f-pg-sel c-nat-operacao-ini  
          tt-param.c-nat-operacao-fim     = INPUT FRAME f-pg-sel c-nat-operacao-fim.  


    {include/i-rpexb.i}

    if  session:set-wait-state("general":U) then.
    
    {include/i-rprun.i ftp/esft0017rp.p} 
  
    {include/i-rpexc.i}
/*
    if  session:set-wait-state("":U) then.
    
    {include/i-rptrm.i}  */
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-leave-pallete w-relat 
PROCEDURE pi-leave-pallete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* if tt-digita.b_it-codigo:screen-value in browse br-digita <> "0" then do:
   end. */  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


{include/i-rptrp.i}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-relat  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-digita"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

/*----------------------------------------------------------------------------
   Rotina para testar a existencia do pedido e se pertence ao mesmo cliente
   e produto */


/*--------------------------------------------------------------------------*/




