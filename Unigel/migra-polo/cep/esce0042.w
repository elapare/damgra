&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ser
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
     
{include/i-prgvrs.i ESCE0042 2.06.00.000}  /*** 010000 ***/
     
DEF BUFFER ccusto FOR emsuni.ccusto.

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */
DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD diret-arq         AS CHARACTER
    FIELD destino           AS INTEGER
    FIELD arquivo           AS CHARACTER
    FIELD usuario           AS CHARACTER
    FIELD data-exec         AS DATE
    FIELD hora-exec         AS INTEGER
    FIELD cod-estab-origem  AS CHARACTER
    FIELD c-ct-codigo-orig  AS CHARACTER
    FIELD c-sc-codigo-orig  AS CHARACTER
    FIELD cod-estab-destino AS CHARACTER
    FIELD c-ct-codigo-dest  AS CHARACTER
    FIELD c-sc-codigo-dest  AS CHARACTER
    FIELD dt-trans          AS DATE.

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS RAW.

/* Transfer Definitions */
DEF VAR raw-param        AS RAW NO-UNDO.

                   
/* Local Variable Definitions ---                                       */
def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.

DEFINE NEW GLOBAL SHARED VAR v_rec_plano_cta_ctbl  AS RECID FORMAT ">>>>>>9":U NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR v_rec_cta_ctbl        AS RECID FORMAT ">>>>>>9":U NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR v_rec_plano_ccusto    AS RECID FORMAT ">>>>>>9":U NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR v_rec_ccusto          AS RECID FORMAT ">>>>>>9":U NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 rs-destino bt-arquivo ~
bt-config-impr c-arquivo rs-execucao text-destino text-modo 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao ~
text-destino text-modo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 c-ct-codigo-orig c-ct-codigo-dest 
&Scoped-define List-2 c-ct-codigo-orig c-ct-codigo-dest 
&Scoped-define List-3 c-ct-codigo-orig c-ct-codigo-dest 

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

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 2 
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

DEFINE VARIABLE c-cod-estab-destino AS CHARACTER FORMAT "X(3)":U INITIAL "404" 
     LABEL "Estab Destino" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88.

DEFINE VARIABLE c-cod-estab-origem AS CHARACTER FORMAT "X(3)":U INITIAL "362" 
     LABEL "Estab Origem" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88.

DEFINE VARIABLE c-ct-codigo-dest AS CHARACTER FORMAT "x(20)" INITIAL "215198" 
     LABEL "Conta Destino" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88.

DEFINE VARIABLE c-ct-codigo-orig AS CHARACTER FORMAT "x(20)" INITIAL "115198" 
     LABEL "Conta Origem" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88.

DEFINE VARIABLE c-desc-cc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-cc-dest AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-conta-contab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-conta-contab-dest AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-estab-destino AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY .88.

DEFINE VARIABLE c-desc-estab-origem AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY .88.

DEFINE VARIABLE c-sc-codigo-dest AS CHARACTER FORMAT "x(20)" INITIAL "99382" 
     LABEL "C. Custo Destino" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88.

DEFINE VARIABLE c-sc-codigo-orig AS CHARACTER FORMAT "x(20)" INITIAL "99432" 
     LABEL "C. Custo Origem" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88.

DEFINE VARIABLE dt-trans AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt Trans" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     im-pg-imp AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.25
         SIZE 81 BY 14.75
         DEFAULT-BUTTON bt-executar.

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

DEFINE FRAME f-pg-sel
     c-cod-estab-origem AT ROW 3 COL 18 COLON-ALIGNED WIDGET-ID 16
     c-desc-estab-origem AT ROW 3 COL 24.14 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     c-ct-codigo-orig AT ROW 4 COL 18 COLON-ALIGNED WIDGET-ID 30
     c-desc-conta-contab AT ROW 4 COL 39.43 COLON-ALIGNED NO-LABEL WIDGET-ID 112
     c-sc-codigo-orig AT ROW 5 COL 18 COLON-ALIGNED WIDGET-ID 36
     c-desc-cc AT ROW 5 COL 39.29 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     c-cod-estab-destino AT ROW 6 COL 18 COLON-ALIGNED WIDGET-ID 14
     c-desc-estab-destino AT ROW 6 COL 24.14 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     c-ct-codigo-dest AT ROW 7 COL 18 COLON-ALIGNED WIDGET-ID 114
     c-desc-conta-contab-dest AT ROW 7 COL 39.43 COLON-ALIGNED NO-LABEL WIDGET-ID 118
     c-sc-codigo-dest AT ROW 8 COL 18 COLON-ALIGNED WIDGET-ID 120
     c-desc-cc-dest AT ROW 8 COL 39.29 COLON-ALIGNED NO-LABEL WIDGET-ID 116
     dt-trans AT ROW 9 COL 18 COLON-ALIGNED WIDGET-ID 110     
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62.


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
         TITLE              = "Transferencia Estoque"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 28.54
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 28.54
         VIRTUAL-WIDTH      = 182.86
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
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME f-pg-sel
   Custom                                                               */
ASSIGN 
       c-cod-estab-destino:HIDDEN IN FRAME f-pg-sel           = TRUE.

ASSIGN 
       c-cod-estab-origem:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR FILL-IN c-ct-codigo-dest IN FRAME f-pg-sel
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN c-ct-codigo-orig IN FRAME f-pg-sel
   1 2 3                                                                */
/* SETTINGS FOR FILL-IN c-desc-cc IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-cc-dest IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-conta-contab IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-conta-contab-dest IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-estab-destino IN FRAME f-pg-sel
   NO-ENABLE                                                            */
ASSIGN 
       c-desc-estab-destino:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR FILL-IN c-desc-estab-origem IN FRAME f-pg-sel
   NO-ENABLE                                                            */
ASSIGN 
       c-desc-estab-origem:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR FRAME f-relat
                                                                        */
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
ON END-ERROR OF w-relat /* Transferencia Estoque */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Transferencia Estoque */
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
   do  on error undo, return :
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME c-cod-estab-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-estab-destino w-relat
ON F5 OF c-cod-estab-destino IN FRAME f-pg-sel /* Estab Destino */
DO:
    ASSIGN l-implanta = YES.
    {include/zoomvar.i &prog-zoom="adzoom/z01ad107.w"
                       &campo=c-cod-estab-destino
                       &campozoom=cod-estabel
                       &campo2=c-desc-estab-destino
                       &campozoom2=nome}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-estab-destino w-relat
ON LEAVE OF c-cod-estab-destino IN FRAME f-pg-sel /* Estab Destino */
DO:
  FIND FIRST estabelec NO-LOCK
       WHERE estabelec.cod-estabel = c-cod-estab-destino:SCREEN-VALUE IN FRAME f-pg-sel NO-ERROR.
  IF AVAIL estabelec THEN DO:
      ASSIGN c-desc-estab-destino:SCREEN-VALUE = estabelec.nome.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-estab-destino w-relat
ON MOUSE-SELECT-DBLCLICK OF c-cod-estab-destino IN FRAME f-pg-sel /* Estab Destino */
DO:
    APPLY 'F5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-cod-estab-origem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-estab-origem w-relat
ON F5 OF c-cod-estab-origem IN FRAME f-pg-sel /* Estab Origem */
DO:
    ASSIGN l-implanta = YES.
    {include/zoomvar.i &prog-zoom="adzoom/z01ad107.w"
                       &campo=c-cod-estab-origem
                       &campozoom=cod-estabel
                       &campo2=c-desc-estab-origem
                       &campozoom2=nome}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-estab-origem w-relat
ON LEAVE OF c-cod-estab-origem IN FRAME f-pg-sel /* Estab Origem */
DO:
  FIND FIRST estabelec NO-LOCK
       WHERE estabelec.cod-estabel = c-cod-estab-origem:SCREEN-VALUE IN FRAME f-pg-sel NO-ERROR.
  IF AVAIL estabelec THEN DO:
      ASSIGN c-desc-estab-origem:SCREEN-VALUE = estabelec.nome.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-estab-origem w-relat
ON MOUSE-SELECT-DBLCLICK OF c-cod-estab-origem IN FRAME f-pg-sel /* Estab Origem */
DO:
    APPLY 'F5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-ct-codigo-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-ct-codigo-dest w-relat
ON F5 OF c-ct-codigo-dest IN FRAME f-pg-sel /* Conta Destino */
DO:
    find first plano_cta_unid_organ no-lock 
         WHERE plano_cta_unid_organ.cod_unid_organ         = v_cod_empres_usuar 
           and plano_cta_unid_organ.ind_tip_plano_cta_ctbl = "Prim rio" no-error.

    FIND FIRST plano_cta_ctbl
         WHERE plano_cta_ctbl.cod_plano_cta_ctbl = plano_cta_unid_organ.cod_plano_cta_ctbl NO-LOCK NO-ERROR.


    assign v_rec_plano_cta_ctbl = recid(plano_cta_ctbl).
    
    IF  SEARCH("prgint/utb/utb080nc.r") = ? AND SEARCH("prgint/utb/utb080nc.p") = ? THEN DO:
        MESSAGE "Programa executavel nao foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb080nc.p"
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN.
    END.
    ELSE
        RUN prgint/utb/utb080nc.r /*prg_sea_portador*/.
    IF v_rec_cta_ctbl <> ? THEN DO:
        FIND FIRST cta_ctbl WHERE RECID(cta_ctbl) = v_rec_cta_ctbl NO-LOCK NO-ERROR.
        ASSIGN c-desc-conta-contab-dest:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(cta_ctbl.des_tit_ctbl)
               c-ct-codigo-dest:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(cta_ctbl.cod_cta_ctbl).
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-ct-codigo-dest w-relat
ON LEAVE OF c-ct-codigo-dest IN FRAME f-pg-sel /* Conta Destino */
DO:

    &SCOPED-DEFINE conta     c-ct-codigo-dest
    &SCOPED-DEFINE descricao c-desc-conta-contab-dest


    find first plano_cta_unid_organ no-lock 
         WHERE plano_cta_unid_organ.cod_unid_organ         = v_cod_empres_usuar 
           and plano_cta_unid_organ.ind_tip_plano_cta_ctbl = "Prim rio" no-error.

    FIND FIRST plano_cta_ctbl
         WHERE plano_cta_ctbl.cod_plano_cta_ctbl = plano_cta_unid_organ.cod_plano_cta_ctbl NO-LOCK NO-ERROR.
        
    FIND FIRST cta_ctbl
         WHERE cta_ctbl.cod_plano_cta_ctbl = plano_cta_ctbl.cod_plano_cta_ctbl
           AND cta_ctbl.cod_cta_ctbl       = input {&conta} NO-LOCK NO-ERROR.
    IF AVAIL cta_ctbl THEN
        ASSIGN c-desc-conta-contab-dest:SCREEN-VALUE IN FRAME f-pg-sel = cta_ctbl.des_tit_ctbl.
    ELSE
        ASSIGN c-desc-conta-contab-dest:SCREEN-VALUE IN FRAME f-pg-sel = "".


    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-ct-codigo-dest w-relat
ON MOUSE-SELECT-DBLCLICK OF c-ct-codigo-dest IN FRAME f-pg-sel /* Conta Destino */
DO:
    Apply "F5" to c-ct-codigo-dest in frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-ct-codigo-orig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-ct-codigo-orig w-relat
ON F5 OF c-ct-codigo-orig IN FRAME f-pg-sel /* Conta Origem */
DO:

    find first plano_cta_unid_organ no-lock 
         WHERE plano_cta_unid_organ.cod_unid_organ         = v_cod_empres_usuar 
           and plano_cta_unid_organ.ind_tip_plano_cta_ctbl = "Prim rio" no-error.

    FIND FIRST plano_cta_ctbl
         WHERE plano_cta_ctbl.cod_plano_cta_ctbl = plano_cta_unid_organ.cod_plano_cta_ctbl NO-LOCK NO-ERROR.


    assign v_rec_plano_cta_ctbl = recid(plano_cta_ctbl).
    
    IF  SEARCH("prgint/utb/utb080nc.r") = ? AND SEARCH("prgint/utb/utb080nc.p") = ? THEN DO:
        MESSAGE "Programa executavel nao foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb080nc.p"
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN.
    END.
    ELSE
        RUN prgint/utb/utb080nc.r /*prg_sea_portador*/.

    IF v_rec_cta_ctbl <> ? THEN DO:
        FIND cta_ctbl WHERE RECID(cta_ctbl) = v_rec_cta_ctbl NO-LOCK NO-ERROR.
        ASSIGN c-desc-conta-contab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(cta_ctbl.des_tit_ctbl)
               c-ct-codigo-orig:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(cta_ctbl.cod_cta_ctbl).
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-ct-codigo-orig w-relat
ON LEAVE OF c-ct-codigo-orig IN FRAME f-pg-sel /* Conta Origem */
DO:

    &SCOPED-DEFINE conta     c-ct-codigo-orig
    &SCOPED-DEFINE descricao c-desc-conta-contab


    find first plano_cta_unid_organ no-lock 
         WHERE plano_cta_unid_organ.cod_unid_organ         = v_cod_empres_usuar 
           and plano_cta_unid_organ.ind_tip_plano_cta_ctbl = "Prim rio" no-error.

    FIND FIRST plano_cta_ctbl
         WHERE plano_cta_ctbl.cod_plano_cta_ctbl = plano_cta_unid_organ.cod_plano_cta_ctbl NO-LOCK NO-ERROR.
        
    FIND FIRST cta_ctbl
         WHERE cta_ctbl.cod_plano_cta_ctbl = plano_cta_ctbl.cod_plano_cta_ctbl
           AND cta_ctbl.cod_cta_ctbl       = input {&conta} NO-LOCK NO-ERROR.
    IF AVAIL cta_ctbl THEN
        ASSIGN c-desc-conta-contab:SCREEN-VALUE IN FRAME f-pg-sel = cta_ctbl.des_tit_ctbl.
    ELSE
        ASSIGN c-desc-conta-contab:SCREEN-VALUE IN FRAME f-pg-sel = "".


    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-ct-codigo-orig w-relat
ON MOUSE-SELECT-DBLCLICK OF c-ct-codigo-orig IN FRAME f-pg-sel /* Conta Origem */
DO:
    Apply "F5" to c-ct-codigo-orig in frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-sc-codigo-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-sc-codigo-dest w-relat
ON F5 OF c-sc-codigo-dest IN FRAME f-pg-sel /* C. Custo Destino */
DO:

    FIND FIRST estabelec
         WHERE estabelec.cod-estabel = c-cod-estab-destino:screen-value in frame {&FRAME-NAME} no-lock no-error.
    IF NOT AVAIL estabelec THEN
        FIND FIRST estabelec no-lock no-error.
         

    FIND first plano_ccusto WHERE plano_ccusto.cod_empresa = string(estabelec.ep-codigo)  NO-LOCK NO-ERROR.


    assign v_rec_plano_ccusto = recid(plano_ccusto).
    
    if  search("prgint/utb/utb066ka.r") = ? and search("prgint/utb/utb066ka.p") = ? then do:
         message "Programa executavel nao foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb066ka.p"
                view-as alert-box error buttons ok.
         return.
     end.
     else
         run prgint/utb/utb066ka.p /*prg_sea_ccusto*/.

    if  v_rec_ccusto <> ? then do:
        find ccusto where recid(ccusto) = v_rec_ccusto no-lock no-error.
        assign c-sc-codigo-dest:screen-value in frame {&FRAME-NAME} = string(ccusto.cod_ccusto)
               c-desc-cc-dest  :screen-value in frame {&FRAME-NAME} = string(ccusto.des_tit_ctbl).
    end /* if */.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-sc-codigo-dest w-relat
ON LEAVE OF c-sc-codigo-dest IN FRAME f-pg-sel /* C. Custo Destino */
DO:

    &SCOPED-DEFINE cc      c-sc-codigo-dest
    &SCOPED-DEFINE desc-cc c-desc-conta-contab-dest

    FIND FIRST estabelec
         WHERE estabelec.cod-estabel = c-cod-estab-destino:screen-value in frame {&FRAME-NAME} no-lock no-error.
    IF NOT AVAIL estabelec THEN
        FIND FIRST estabelec no-lock no-error.        

    FIND first plano_ccusto WHERE plano_ccusto.cod_empresa = string(estabelec.ep-codigo)  NO-LOCK NO-ERROR.
    FIND FIRST emsuni.ccusto
         WHERE ccusto.cod_plano_ccusto = plano_ccusto.cod_plano_ccusto
           AND ccusto.cod_ccusto   = input {&cc} NO-LOCK NO-ERROR.
    IF AVAIL ccusto THEN
        ASSIGN c-desc-cc-dest:SCREEN-VALUE IN FRAME f-pg-sel  = ccusto.des_tit_ctbl.
    ELSE
        ASSIGN c-desc-cc-dest:SCREEN-VALUE IN FRAME f-pg-sel  = "".
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-sc-codigo-dest w-relat
ON MOUSE-SELECT-DBLCLICK OF c-sc-codigo-dest IN FRAME f-pg-sel /* C. Custo Destino */
DO:
    Apply "F5" to c-sc-codigo-dest in frame {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-sc-codigo-orig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-sc-codigo-orig w-relat
ON F5 OF c-sc-codigo-orig IN FRAME f-pg-sel /* C. Custo Origem */
DO:

    FIND FIRST estabelec
         WHERE estabelec.cod-estabel = c-cod-estab-origem:screen-value in frame {&FRAME-NAME} no-lock no-error.
    IF NOT AVAIL estabelec THEN
        FIND FIRST estabelec no-lock no-error.
         

    FIND first plano_ccusto WHERE plano_ccusto.cod_empresa = string(estabelec.ep-codigo)  NO-LOCK NO-ERROR.


    assign v_rec_plano_ccusto = recid(plano_ccusto).
    
    if  search("prgint/utb/utb066ka.r") = ? and search("prgint/utb/utb066ka.p") = ? then do:
         message "Programa executavel nao foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb066ka.p"
                view-as alert-box error buttons ok.
         return.
     end.
     else
         run prgint/utb/utb066ka.p /*prg_sea_ccusto*/.

    if  v_rec_ccusto <> ? then do:
        find ccusto where recid(ccusto) = v_rec_ccusto no-lock no-error.
        assign c-sc-codigo-orig:screen-value in frame {&FRAME-NAME} = string(ccusto.cod_ccusto)
               c-desc-cc       :screen-value in frame {&FRAME-NAME} = string(ccusto.des_tit_ctbl).
    end /* if */.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-sc-codigo-orig w-relat
ON LEAVE OF c-sc-codigo-orig IN FRAME f-pg-sel /* C. Custo Origem */
DO:

    &SCOPED-DEFINE cc      c-sc-codigo-orig
    &SCOPED-DEFINE desc-cc c-desc-conta-contab

    FIND FIRST estabelec
         WHERE estabelec.cod-estabel = c-cod-estab-origem:screen-value in frame {&FRAME-NAME} no-lock no-error.
    IF NOT AVAIL estabelec THEN
        FIND FIRST estabelec no-lock no-error.        

    FIND first plano_ccusto WHERE plano_ccusto.cod_empresa = string(estabelec.ep-codigo)  NO-LOCK NO-ERROR.
    FIND FIRST ccusto
         WHERE ccusto.cod_plano_ccusto = plano_ccusto.cod_plano_ccusto
           AND ccusto.cod_ccusto   = input {&cc} NO-LOCK NO-ERROR.

    IF AVAIL ccusto THEN
        ASSIGN c-desc-cc:SCREEN-VALUE IN FRAME f-pg-sel  = ccusto.des_tit_ctbl.
    ELSE
        ASSIGN c-desc-cc:SCREEN-VALUE IN FRAME f-pg-sel  = "".
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-sc-codigo-orig w-relat
ON MOUSE-SELECT-DBLCLICK OF c-sc-codigo-orig IN FRAME f-pg-sel /* C. Custo Origem */
DO:
    Apply "F5" to c-sc-codigo-orig in frame {&FRAME-NAME}.
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

IF c-cod-estab-origem:LOAD-MOUSE-POINTER ("image/lupa.cur") THEN.
IF c-cod-estab-destino:LOAD-MOUSE-POINTER ("image/lupa.cur") THEN. 
IF c-ct-codigo-orig:LOAD-MOUSE-POINTER ("image/lupa.cur") THEN. 
IF c-ct-codigo-dest:LOAD-MOUSE-POINTER ("image/lupa.cur") THEN. 
IF c-sc-codigo-orig:LOAD-MOUSE-POINTER ("image/lupa.cur") THEN. 
IF c-sc-codigo-dest:LOAD-MOUSE-POINTER ("image/lupa.cur") THEN. 


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/*Alimenta‡Æo de novos campos atrav‚s de Campos Livres*/

{utp/ut9000.i "ESCE0042" "2.06.00.000"}

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

    {include/i-rpmbl.i}

    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.

    FIND FIRST estabelec
         WHERE estabelec.cod-estabel = "362" NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN
        ASSIGN c-desc-estab-origem:SCREEN-VALUE IN FRAME f-pg-sel = estabelec.nome.

    FIND FIRST estabelec
         WHERE estabelec.cod-estabel = "404" NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN
        ASSIGN c-desc-estab-destino:SCREEN-VALUE IN FRAME f-pg-sel = estabelec.nome.


    ASSIGN dt-trans:SCREEN-VALUE IN FRAME f-pg-sel = STRING(TODAY,"99/99/9999").

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
  ENABLE im-pg-imp im-pg-sel RECT-1 bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY dt-trans c-cod-estab-origem c-desc-estab-origem c-cod-estab-destino 
          c-desc-estab-destino c-ct-codigo-orig c-desc-conta-contab 
          c-sc-codigo-orig c-desc-cc c-ct-codigo-dest c-desc-conta-contab-dest 
          c-sc-codigo-dest c-desc-cc-dest 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE dt-trans c-cod-estab-origem c-cod-estab-destino c-ct-codigo-orig 
         c-sc-codigo-orig c-ct-codigo-dest c-sc-codigo-dest 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao text-destino text-modo 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao text-destino text-modo 
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
    DEFINE VARIABLE i-num-ped-exec-rpw AS INTEGER   NO-UNDO.
    DEFINE VARIABLE c-arq-baca         AS CHARACTER NO-UNDO.

    {include/i-rpexa.i}
        
    if input frame f-pg-imp rs-destino = 2 and
       input frame f-pg-imp rs-execucao = 1 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show", input 73, input "").
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.

    IF NOT CAN-FIND(FIRST estabelec
                    WHERE estabelec.cod-estabel = INPUT FRAME f-pg-sel c-cod-estab-origem) THEN DO:

        run utp/ut-msgs.p (input "show", 
                           input 2, 
                           input "Estabelecimento Origem").
        RETURN NO-APPLY.

    END.

    IF NOT CAN-FIND(FIRST estabelec
                    WHERE estabelec.cod-estabel = INPUT FRAME f-pg-sel c-cod-estab-destino) THEN DO:

        run utp/ut-msgs.p (input "show", 
                           input 2, 
                           input "Estabelecimento Destino").
        RETURN NO-APPLY.

    END.

    IF INPUT FRAME f-pg-sel c-cod-estab-origem = INPUT FRAME f-pg-sel c-cod-estab-destino THEN DO:
        run utp/ut-msgs.p (input "show", 
                           input 17006, 
                           input "Estabelecimento Origem e Destino nÆo podem ser iguais").
        RETURN NO-APPLY.
    END.

    find first plano_cta_unid_organ no-lock 
         WHERE plano_cta_unid_organ.cod_unid_organ         = v_cod_empres_usuar 
           and plano_cta_unid_organ.ind_tip_plano_cta_ctbl = "Prim rio" no-error.

    IF NOT CAN-FIND(FIRST cta_ctbl
                    WHERE cta_ctbl.cod_plano_cta_ctbl = plano_cta_unid_organ.cod_plano_cta_ctbl
                      AND cta_ctbl.cod_cta_ctbl       = INPUT FRAME f-pg-sel c-ct-codigo-orig) THEN DO:

        run utp/ut-msgs.p (input "show", 
                           input 2, 
                           input "Conta Cont bil Origem").
        RETURN NO-APPLY.

    END.
    
    /* Centro de Custo Origem */
    FIND FIRST estabelec
         WHERE estabelec.cod-estabel = INPUT FRAME f-pg-sel c-cod-estab-origem NO-LOCK NO-ERROR.
    FIND first plano_ccusto WHERE plano_ccusto.cod_empresa = string(estabelec.ep-codigo)  NO-LOCK NO-ERROR.
    FIND FIRST ccusto
         WHERE ccusto.cod_plano_ccusto = plano_cta_ctbl.cod_plano_cta_ctbl
           AND ccusto.cod_ccusto       = INPUT FRAME f-pg-sel c-sc-codigo-orig NO-LOCK NO-ERROR.

    IF NOT AVAIL ccusto THEN DO:
        run utp/ut-msgs.p (input "show", 
                           input 2, 
                           input "Centro de Custo Origem").
        RETURN NO-APPLY.
    END.
    

    IF NOT CAN-FIND(FIRST cta_ctbl
                    WHERE cta_ctbl.cod_plano_cta_ctbl = plano_cta_unid_organ.cod_plano_cta_ctbl
                      AND cta_ctbl.cod_cta_ctbl       = INPUT FRAME f-pg-sel c-ct-codigo-dest) THEN DO:

        run utp/ut-msgs.p (input "show", 
                           input 2, 
                           input "Conta Cont bil Destino").
        RETURN NO-APPLY.

    END.

    /* Centro de Custo Destino */
    FIND FIRST estabelec
         WHERE estabelec.cod-estabel = INPUT FRAME f-pg-sel c-cod-estab-destino NO-LOCK NO-ERROR.
    FIND first plano_ccusto WHERE plano_ccusto.cod_empresa = string(estabelec.ep-codigo)  NO-LOCK NO-ERROR.
    FIND FIRST ccusto
         WHERE ccusto.cod_plano_ccusto = plano_cta_ctbl.cod_plano_cta_ctbl
           AND ccusto.cod_ccusto       = INPUT FRAME f-pg-sel c-sc-codigo-dest NO-LOCK NO-ERROR.

    IF NOT AVAIL ccusto THEN DO:
        run utp/ut-msgs.p (input "show", 
                           input 2, 
                           input "Centro de Custo Origem").
        RETURN NO-APPLY.
    END.

    create tt-param.
    assign tt-param.usuario           = c-seg-usuario
           tt-param.destino           = input frame f-pg-imp rs-destino
           tt-param.data-exec         = today
           tt-param.hora-exec         = time
           tt-param.cod-estab-origem  = INPUT FRAME f-pg-sel c-cod-estab-origem
           tt-param.c-ct-codigo-orig  = INPUT FRAME f-pg-sel c-ct-codigo-orig
           tt-param.c-sc-codigo-orig  = INPUT FRAME f-pg-sel c-sc-codigo-orig
           tt-param.cod-estab-destino = INPUT FRAME f-pg-sel c-cod-estab-destino
           tt-param.c-ct-codigo-dest  = INPUT FRAME f-pg-sel c-ct-codigo-dest
           tt-param.c-sc-codigo-dest  = INPUT FRAME f-pg-sel c-sc-codigo-dest
           tt-param.dt-trans          = INPUT FRAME f-pg-sel dt-trans.

    if tt-param.destino = 1 then 
        assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = "esce0042.lst". /*input frame f-pg-imp c-arquivo.*/
         else assign tt-param.arquivo = session:temp-directory + "esce0042.tmp".

    /* Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    
    /* Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).

    raw-transfer tt-param to raw-param.
    &IF "{&PGDIG}" <> "" &THEN
        for each tt-raw-digita:
            delete tt-raw-digita.
        end.
        for each tt-digita:
            create tt-raw-digita.
            raw-transfer tt-digita to tt-raw-digita.raw-digita.
        end.  
    &ENDIF    
    
    if rs-execucao:screen-value in frame f-pg-imp = "2" then do:
        run btb/btb911zb.p (input c-programa-mg97,
                            input "cep/esce0042rp.p",
                            input c-versao-mg97,
                            input 97,
                            input tt-param.arquivo,
                            input tt-param.destino,
                            input raw-param,
                            input table tt-raw-digita,
                            output i-num-ped-exec-rpw).
    if i-num-ped-exec-rpw <> 0 then                     
        run utp/ut-msgs.p (input "show":U, input 4169, input string(i-num-ped-exec-rpw)).                      
    end.                      
    else do:                                         
      run cep/esce0042rp.p (input raw-param, input table tt-raw-digita).
    end.

    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).

    {include/i-rptrm.i}

END PROCEDURE.

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

