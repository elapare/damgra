&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wReport 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCE0016 2.06.00.001}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
&ENDIF

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESCE0016
&GLOBAL-DEFINE Version        2.06.00.001
&GLOBAL-DEFINE VersionLayout  

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   Sele‡Æo,ImpressÆo

&GLOBAL-DEFINE PGLAY          NO
&GLOBAL-DEFINE PGSEL          YES
&GLOBAL-DEFINE PGCLA          NO
&GLOBAL-DEFINE PGPAR          NO
&GLOBAL-DEFINE PGDIG          NO
&GLOBAL-DEFINE PGIMP          YES
&GLOBAL-DEFINE PGLOG          NO

&GLOBAL-DEFINE RTF            NO

&GLOBAL-DEFINE page0Widgets   btOk ~
                              btCancel ~
                              btHelp2
&GLOBAL-DEFINE page2Widgets   
&GLOBAL-DEFINE page4Widgets   
&GLOBAL-DEFINE page6Widgets   rsDestiny ~
                              rsDestinyEs ~
                              btConfigImpr ~
                              btFile ~
                              rsExecution
&GLOBAL-DEFINE page0Text      
&GLOBAL-DEFINE page2Text      
&GLOBAL-DEFINE page4Text      
&GLOBAL-DEFINE page6Text      text-destino text-modo

&GLOBAL-DEFINE page1Fields    
&GLOBAL-DEFINE page2Fields    c-cod-estabel c-ge-codigo-ini c-ge-codigo-fim c-fm-codigo-ini c-fm-codigo-fim c-it-codigo-ini c-it-codigo-fim c-depos-ini c-depos-fim dt-trans-ini dt-trans-fim i-nr-linha-ini i-nr-linha-fim pesq-jr
&GLOBAL-DEFINE page4Fields    
&GLOBAL-DEFINE page6Fields    cFile

/* Parameters Definitions ---                                           */

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
    field c-cod-estabel        LIKE movto-estoq.cod-estabel
    field c-ge-codigo-ini      LIKE item.ge-codigo
    field c-ge-codigo-fim      LIKE item.ge-codigo
    field c-it-codigo-ini      LIKE item.it-codigo
    field c-it-codigo-fim      LIKE item.it-codigo
    field c-fm-codigo-ini      LIKE item.fm-codigo 
    field c-fm-codigo-fim      LIKE item.fm-codigo 
    field dt-trans-ini         as DATE 
    field dt-trans-fim         as date
    field c-depos-ini          as char
    field c-depos-fim          as char
    field i-nr-linha-ini       as INT 
    field i-nr-linha-fim       as int
    FIELD pesq-jr              as integer
.
    /*Alterado 15/02/2005 - tech1007 - Criado campo l¢gico para verificar se o RTF foi habilitado*/
    /*Fim alteracao 15/02/2005*/

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-rtf              as char    no-undo.
def var c-arq-layout       as char    no-undo.      
def var c-arq-temp         as char    no-undo.
DEF VAR c-modelo-default   AS CHAR    NO-UNDO.
DEFINE VARIABLE l-cancelado AS LOGICAL     NO-UNDO.
def stream s-imp.

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa est  rodando no WebEnabler*/
DEFINE SHARED VARIABLE hWenController AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar btOK btCancel btHelp2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wReport AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "&Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "&Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "&Executar" 
     SIZE 10 BY 1.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE c-cod-estabel AS CHARACTER FORMAT "X(3)" INITIAL "412"  /*solic-318*/ 
     LABEL "Estabelecimento":R18 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-depos-fim AS CHARACTER FORMAT "x(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-depos-ini AS CHARACTER FORMAT "x(3)" 
     LABEL "Dep¢sito":R10 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-fm-codigo-fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-fm-codigo-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Fam¡lia":R9 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-ge-codigo-fim AS INTEGER FORMAT ">9" INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 3.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-ge-codigo-ini AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Grupo Estoque":R16 
     VIEW-AS FILL-IN 
     SIZE 3.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo-fim AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-it-codigo-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE dt-trans-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE dt-trans-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Per¡odo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE i-nr-linha-fim AS INTEGER FORMAT ">>9" INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE i-nr-linha-ini AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Linha Produ‡Æo":R17 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88
     FONT 1 NO-UNDO.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-23
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-24
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-25
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-26
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE pesq-jr AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Sem GGF", 1,
"Com GGF", 2
     SIZE 12 BY 2.25 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 3.5.

DEFINE BUTTON btConfigImpr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON btFile 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE cFile AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.14 BY .63
     FONT 1 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsDestiny AS INTEGER INITIAL 4 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3,
"Excel", 4
     SIZE 43.86 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsDestinyEs AS INTEGER INITIAL 4 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3,
"Excel", 4
     SIZE 43.86 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsExecution AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.86 BY .92
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45.86 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 1.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 16.75 COL 2
     btCancel AT ROW 16.75 COL 13
     btHelp2 AT ROW 16.75 COL 80
     rtToolBar AT ROW 16.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage6
     rsDestiny AT ROW 2.38 COL 3.14 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     cFile AT ROW 3.63 COL 3.14 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     btFile AT ROW 3.5 COL 43 HELP
          "Escolha do nome do arquivo"
     btConfigImpr AT ROW 3.5 COL 43 HELP
          "Configura‡Æo da impressora"
     rsExecution AT ROW 9.5 COL 2.86 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.63 COL 1.86 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 8.75 COL 1.14 COLON-ALIGNED NO-LABEL
     rsDestinyEs AT ROW 2.38 COL 3.14 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL WIDGET-ID 4
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 9 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.79
         SIZE 84.43 BY 11.96
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage2
     c-cod-estabel AT ROW 1.25 COL 19 COLON-ALIGNED HELP
          "Informe o estabelecimento" WIDGET-ID 78
     c-ge-codigo-ini AT ROW 2.25 COL 19 COLON-ALIGNED HELP
          "Grupo de estoque a que pertence o item" WIDGET-ID 80
     c-ge-codigo-fim AT ROW 2.25 COL 46.29 COLON-ALIGNED HELP
          "Grupo de estoque a que pertence o item" NO-LABEL WIDGET-ID 86
     c-fm-codigo-ini AT ROW 3.25 COL 19 COLON-ALIGNED HELP
          "Fam¡lia de material a que pertence o familia" WIDGET-ID 88
     c-fm-codigo-fim AT ROW 3.25 COL 46.29 COLON-ALIGNED HELP
          "Fam¡lia de material a que pertence o familia" NO-LABEL WIDGET-ID 94
     c-it-codigo-ini AT ROW 4.25 COL 16.86 HELP
          "C¢digo do item" WIDGET-ID 60
     c-it-codigo-fim AT ROW 4.25 COL 48.29 HELP
          "C¢digo do item" NO-LABEL WIDGET-ID 96
     c-depos-ini AT ROW 5.25 COL 19 COLON-ALIGNED WIDGET-ID 100
     c-depos-fim AT ROW 5.25 COL 46.29 COLON-ALIGNED NO-LABEL WIDGET-ID 102
     dt-trans-ini AT ROW 6.25 COL 14.71 WIDGET-ID 64
     dt-trans-fim AT ROW 6.25 COL 48.29 NO-LABEL WIDGET-ID 62
     i-nr-linha-ini AT ROW 7.25 COL 8.14 WIDGET-ID 52
     i-nr-linha-fim AT ROW 7.25 COL 48.29 NO-LABEL WIDGET-ID 98
     pesq-jr AT ROW 9.75 COL 29 NO-LABEL WIDGET-ID 70
     "Composi‡Æo de Valores" VIEW-AS TEXT
          SIZE 16 BY .54 AT ROW 8.75 COL 26 WIDGET-ID 76
     IMAGE-18 AT ROW 4.25 COL 38.29 WIDGET-ID 40
     IMAGE-19 AT ROW 4.25 COL 44.14 WIDGET-ID 42
     IMAGE-22 AT ROW 7.29 COL 38.29 WIDGET-ID 54
     IMAGE-23 AT ROW 7.29 COL 44.14 WIDGET-ID 56
     IMAGE-24 AT ROW 6.25 COL 38.29 WIDGET-ID 66
     IMAGE-25 AT ROW 6.25 COL 44.14 WIDGET-ID 68
     RECT-10 AT ROW 9 COL 21 WIDGET-ID 74
     IMAGE-26 AT ROW 2.25 COL 38.29 WIDGET-ID 82
     IMAGE-27 AT ROW 2.25 COL 44.14 WIDGET-ID 84
     IMAGE-28 AT ROW 3.25 COL 38.29 WIDGET-ID 90
     IMAGE-29 AT ROW 3.25 COL 44.14 WIDGET-ID 92
     IMAGE-30 AT ROW 5.25 COL 38.29 WIDGET-ID 104
     IMAGE-31 AT ROW 5.25 COL 44.14 WIDGET-ID 106
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.79
         SIZE 84.43 BY 11.96
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wReport ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 17
         WIDTH              = 90
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.14
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 114.14
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wReport 
/* ************************* Included-Libraries *********************** */

{report/report.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wReport
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage2:FRAME = FRAME fpage0:HANDLE
       FRAME fPage6:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* SETTINGS FOR FILL-IN c-it-codigo-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-it-codigo-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN dt-trans-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN dt-trans-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN i-nr-linha-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN i-nr-linha-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME fPage6
   Custom                                                               */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME fPage6     = 
                "Destino".

ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME fPage6     = 
                "Execu‡Æo".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wReport)
THEN wReport:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage2
/* Query rebuild information for FRAME fPage2
     _Query            is NOT OPENED
*/  /* FRAME fPage2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage6
/* Query rebuild information for FRAME fPage6
     _Query            is NOT OPENED
*/  /* FRAME fPage6 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON END-ERROR OF wReport
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON WINDOW-CLOSE OF wReport
DO:
  /* This event will close the window and terminate the procedure.  */
  {report/logfin.i}  
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wReport
ON CHOOSE OF btCancel IN FRAME fpage0 /* Fechar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME btConfigImpr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConfigImpr wReport
ON CHOOSE OF btConfigImpr IN FRAME fPage6
DO:
   {report/rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFile wReport
ON CHOOSE OF btFile IN FRAME fPage6
DO:
    {report/rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btHelp2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp2 wReport
ON CHOOSE OF btHelp2 IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wReport
ON CHOOSE OF btOK IN FRAME fpage0 /* Executar */
DO:
   do  on error undo, return no-apply:
       run piExecute.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME rsDestinyEs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsDestinyEs wReport
ON VALUE-CHANGED OF rsDestinyEs IN FRAME fPage6
DO:
do  with frame fPage6:
    case self:screen-value:
        when "1":U then do:
            assign cFile:sensitive       = no
                   cFile:visible         = yes
                   btFile:visible        = no
                   btConfigImpr:visible  = yes
                   /*Alterado 15/02/2005 - tech1007 - Alterado para suportar adequadamente com a 
                     funcionalidade de RTF*/
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = NO
                   l-habilitaRtf:SCREEN-VALUE IN FRAME fPage6 = "No"
                   l-habilitaRtf = NO
                   &endif
                   .
                   /*Fim alteracao 15/02/2005*/
        end.
        when "2":U then do:
            assign cFile:sensitive       = yes
                   cFile:visible         = yes
                   btFile:visible        = yes
                   btConfigImpr:visible  = no
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   .
        end.
        when "3":U then do:
            assign cFile:visible         = no
                   cFile:sensitive       = no
                   btFile:visible        = no
                   btConfigImpr:visible  = no
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   .
            /*Alterado 15/02/2005 - tech1007 - Teste para funcionar corretamente no WebEnabler*/
            &IF "{&RTF}":U = "YES":U &THEN
            IF VALID-HANDLE(hWenController) THEN DO:
                ASSIGN l-habilitaRtf:sensitive  = NO
                       l-habilitaRtf:SCREEN-VALUE IN FRAME fPage6 = "No"
                       l-habilitaRtf = NO.
            END.
            &endif
            /*Fim alteracao 15/02/2005*/
        END.
        when "4":U then do:
            assign cFile:sensitive       = yes
                   cFile:visible         = yes
                   btFile:visible        = yes
                   btConfigImpr:visible  = no
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   .
        end.
        /*Alterado 15/02/2005 - tech1007 - Condi‡Æo removida pois RTF nÆo ‚ mais um destino
        when "4":U then do:
            assign cFile:sensitive       = no
                   cFile:visible         = yes
                   btFile:visible        = no
                   btConfigImpr:visible  = yes
                   text-ModelRtf:VISIBLE   = YES
                   rect-rtf:VISIBLE       = YES
                   blModelRtf:VISIBLE       = yes.
        end.
        Fim alteracao 15/02/2005*/
    end case.
end.
&IF "{&RTF}":U = "YES":U &THEN
RUN pi-habilitaRtf.  
&endif
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsExecution
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsExecution wReport
ON VALUE-CHANGED OF rsExecution IN FRAME fPage6
DO:
   ASSIGN rsExecution = int(rsExecution:SCREEN-VALUE IN FRAME fPage6).

    IF  rsExecution = 2 THEN DO:
        IF  rsDestinyEs:SCREEN-VALUE IN FRAME fPage6 <> "1":U AND rsDestinyEs:SCREEN-VALUE IN FRAME fPage6 <> "4":U  THEN
            ASSIGN rsDestinyEs:SCREEN-VALUE IN FRAME fPage6 = "2":U.

        IF rsDestinyEs:SCREEN-VALUE IN FRAME fPage6 = "2":U OR rsDestinyEs:SCREEN-VALUE IN FRAME fPage6 = "4":U  THEN
            ASSIGN cFile           = IF cFile:SCREEN-VALUE IN FRAME fPage6 = "":U THEN 
                                        IF cFile = "" THEN c-arq-old
                                          ELSE cFile
                                     ELSE cFile:SCREEN-VALUE IN FRAME fPage6
                   c-arq-old       = cFile
                   c-arq-old-batch = SUBSTRING(cFile, R-INDEX(cFile, "/":U) + 1) 
                   cFile:SCREEN-VALUE IN FRAME fPage6 = c-arq-old-batch.
        
        
        APPLY "VALUE-CHANGED":U TO rsDestinyEs IN FRAME fPage6.
    
       
        
        ASSIGN cFile = cFile:SCREEN-VALUE IN FRAME fPage6.
    
        assign c-terminal = trim(c-terminal).
                
        rsDestinyEs:DISABLE(c-terminal) IN FRAME fPage6.
    END.
    ELSE DO:
        IF  rsDestinyEs:SCREEN-VALUE IN FRAME fPage6 <> "1":U THEN
            ASSIGN cFile           = IF cFile:SCREEN-VALUE IN FRAME fPage6 = "":U
                                     THEN cFile
                                     ELSE cFile:SCREEN-VALUE IN FRAME fPage6
                   c-arq-old-batch = cFile.
        
        APPLY "VALUE-CHANGED":U TO rsDestinyEs IN FRAME fPage6.
        
        assign c-terminal = trim(c-terminal).
        
        rsDestinyEs:ENABLE(c-terminal) IN FRAME fPage6.
    
        ASSIGN cFile.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wReport 


{utp/ut9000.i "OT0030" "2.06.00.001"}
/*:T--- L¢gica para inicializa‡Æo do programam ---*/
{report/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wReport 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN dt-trans-ini:SCREEN-VALUE IN FRAME fpage2  = STRING(TODAY,"99/99/9999")
           dt-trans-fim:SCREEN-VALUE IN FRAME fpage2  = STRING(TODAY,"99/99/9999")
           cFile       :SCREEN-VALUE IN FRAME fPage6 = "ESCE0016.LST"
           cFile       :SENSITIVE    IN FRAME fPage6 = YES
           /*cFile       :VISIBLE      IN FRAME fPage6 = YES*/
           rsDestinyEs   :SCREEN-VALUE IN FRAME fPage6 = "4":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piExecute wReport 
PROCEDURE piExecute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

define var r-tt-digita as rowid no-undo.

&IF DEFINED(PGIMP) <> 0 AND "{&PGIMP}":U = "YES":U &THEN
/*:T** Relatorio ***/
do on error undo, return error on stop  undo, return error:
    {report/rpexa.i}

    create tt-param.
    assign tt-param.usuario          = c-seg-usuario
           tt-param.destino          = input frame fPage6 rsDestinyEs
           tt-param.data-exec        = today
           tt-param.hora-exec        = time
           tt-param.c-cod-estabel    = input frame fPage2 c-cod-estabel   
           tt-param.c-ge-codigo-ini  = input frame fPage2 c-ge-codigo-ini 
           tt-param.c-ge-codigo-fim  = input frame fPage2 c-ge-codigo-fim 
           tt-param.c-fm-codigo-ini  = input frame fPage2 c-fm-codigo-ini 
           tt-param.c-fm-codigo-fim  = input frame fPage2 c-fm-codigo-fim 
           tt-param.c-it-codigo-ini  = input frame fPage2 c-it-codigo-ini 
           tt-param.c-it-codigo-fim  = input frame fPage2 c-it-codigo-fim 
           tt-param.dt-trans-ini     = input frame fPage2 dt-trans-ini           
           tt-param.dt-trans-fim     = input frame fPage2 dt-trans-fim           
           tt-param.c-depos-ini      = input frame fPage2 c-depos-ini           
           tt-param.c-depos-fim      = input frame fPage2 c-depos-fim           
           tt-param.i-nr-linha-ini   = input frame fPage2 i-nr-linha-ini           
           tt-param.i-nr-linha-fim   = input frame fPage2 i-nr-linha-fim           

           tt-param.pesq-jr          = input frame fPage2 pesq-jr.   .
    
    if tt-param.destino = 1 
    then 
        assign tt-param.arquivo = "":U.
    else if  tt-param.destino = 2 OR tt-param.destino = 4
        then assign tt-param.arquivo = input frame fPage6 cFile.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /*:T Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    
    
    
    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {report/rpexb.i}
    
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    {report/rprun.i cep/ESCE0016rp.p}
    
    {report/rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
   
    /* {report/rptrm.i} */
end.
&ELSE
/*:T** Importacao/Exportacao ***/
do  on error undo, return error
    on stop  undo, return error:     

    {report/rpexa.i}

    if  input frame fPage7 rsDestinyEs = 2 and
        input frame fPage7 rsExecution = 1 then do:
        run utp/ut-vlarq.p (input input frame fPage7 cDestinyFile).
        if  return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "SHOW":U,
                               input 73,
                               input "":U).
            apply "ENTRY":U to cDestinyFile in frame fPage7.                   
            return error.
        end.
    end.
    
    assign file-info:file-name = input frame fPage4 cInputFile.
    if  file-info:pathname = ? and
        input frame fPage7 rsExecution = 1 then do:
        run utp/ut-msgs.p (input "SHOW":U,
                           input 326,
                           input cInputFile).                               
        apply "ENTRY":U to cInputFile in frame fPage4.                
        return error.
    end. 
            
    /*:T Coloque aqui as valida‡äes das outras p ginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na p gina 
       com problemas e colocar o focus no campo com problemas             */    
         
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame fPage7 rsDestinyEs
           tt-param.todos           = input frame fPage7 rsAll
           tt-param.arq-entrada     = input frame fPage4 cInputFile
           tt-param.data-exec       = today
           tt-param.hora-exec       = time.

    if  tt-param.destino = 1 then
        assign tt-param.arq-destino = "":U.
    else
    if  tt-param.destino = 2 THEN
        assign tt-param.arq-destino = input frame fPage7 cDestinyFile.
    else
        assign tt-param.arq-destino = session:temp-directory + c-programa-mg97 + ".tmp":U.

    /*:T Coloque aqui a l¢gica de grava‡Æo dos parƒmtros e sele‡Æo na temp-table
       tt-param */ 

    {report/imexb.i}

    if  session:set-wait-state("GENERAL":U) then.

    {report/imrun.i xxp/xx9999rp.p}

    {report/imexc.i}

    if  session:set-wait-state("":U) then.
    
    {report/imtrm.i tt-param.arq-destino tt-param.destino}
    
end.
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

