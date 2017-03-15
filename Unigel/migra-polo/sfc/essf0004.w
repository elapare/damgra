


/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/************************************************************************
**
**  i-prgvrs.i - Programa para cria»’o do log de todos os programas 
**               e objetos do EMS 2.0 para objetos
**  {1} = objeto   provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/

def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[2.00.00.001[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.001"
       c-prg-obj = "essf0004".
if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "essf0004"
        no-lock no-error.
        
   if not avail prog_dtsul then do:
          if  c-prg-obj begins "btb":U then
              assign c-prg-obj = "btb~/":U + c-prg-obj.
          else if c-prg-obj begins "men":U then
                  assign c-prg-obj = "men~/":U + c-prg-obj.
          else if c-prg-obj begins "sec":U then
                  assign c-prg-obj = "sec~/":U + c-prg-obj.
          else if c-prg-obj begins "utb":U then
                  assign c-prg-obj = "utb~/":U + c-prg-obj.
          find prog_dtsul where
               prog_dtsul.nom_prog_ext begins c-prg-obj no-lock no-error.
   end .            /*if*/
    
    output to value(c-arquivo-log) append.
    put "essf0004" at 1 "2.00.00.001" at 69 today at 84 string(time,'HH:MM:SS':U) at 94 skip.
    if  avail prog_dtsul then do:
        if  prog_dtsul.nom_prog_dpc <> "" then
            put "DPC : ":U at 5 prog_dtsul.nom_prog_dpc  at 12 skip.
        if  prog_dtsul.nom_prog_appc <> "" then
            put "APPC: ":U at 5 prog_dtsul.nom_prog_appc at 12 skip.
        if  prog_dtsul.nom_prog_upc <> "" then
            put "UPC : ":U at 5 prog_dtsul.nom_prog_upc  at 12 skip.
    end.
    output close.        
end.  
error-status:error = no.
/***************************************************
** i_dbtype.i - Tipo de Gerenciadores utilizados
***************************************************/


        
    /* Preprocessadores que identificam os bancos do Produto EMS 5 */
                    
    /* Preprocessadores que identificam os bancos do Produto EMS 2 */
                                                                                                            
    /* Preprocessadores que identificam os bancos do Produto HR 2 */
            

/* Fim */

 

/*alteracao Anderson(tech540) em 04/02/2003 Include com a definicao 
da temp table utilizada nas includes btb008za.i1 e btb008za.i2 para 
execucao de programas via rpc*/
def temp-table tt-control-prog NO-UNDO
    field cod-versao-integracao as integer       format '999'
    field cod-erro              as integer       format '99999'
    field desc-erro             as character     format 'x(60)'
    field wgh-servid-rpc        as widget-handle format '>>>>>>9'.
 
 
/*fim alteracao Anderson 04/02/2003*/

   /*** 010001 ***/
/********************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */




 

/* Parameters Definitions ---                                           */

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field cod-estabel      as CHAR format "x(03)"
    field nr-pedcli-ini    as CHAR format "x(12)"
    field nr-pedcli-fim    as CHAR format "x(12)"
    field nr-pal-ini       as integer 
    field nr-bob-ini       as integer 
    field etq              as integer
    FIELD nr-sequencia-ini AS INTEGER
    FIELD nr-sequencia-fim AS INTEGER .

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
    field raw-digita      as raw.

def var l-ok               as logical no-undo.
def var c-terminal         as char    no-undo.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */


/* Name of designated FRAME-NAME and/or first browse and/or first query */

/* Standard List Definitions                                            */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */




/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wReport AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE cCodEstabel AS CHARACTER FORMAT "x(3)"   /*solic-318*/ 
     LABEL "Estabelecimento":R18 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88.

DEFINE VARIABLE cNrPedcliFim AS CHARACTER FORMAT "x(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88.

DEFINE VARIABLE cNrPedcliIni AS CHARACTER FORMAT "x(12)" 
     LABEL "Pedido":R17 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88.

DEFINE VARIABLE i-nr-sequencia-ini AS INT  FORMAT ">>>>9" 
     LABEL "Sq.Pedido":R17 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88.

DEFINE VARIABLE i-nr-sequencia-fim AS INT FORMAT ">>>>9" INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88.

DEFINE VARIABLE cNrPalini AS integer  INITIAL 1 
     LABEL "Nr. Palete Inicial":U
     VIEW-AS FILL-IN 
     SIZE 15 BY .88.

DEFINE VARIABLE cNrBobini AS INTEGER  INITIAL 1 
    LABEL "Nr. Bobina Inicial":U 
    VIEW-AS FILL-IN 
     SIZE 15 BY .88.


DEFINE VARIABLE rEtq AS INTEGER INITIAL 2
       LABEL "Op‡äes" VIEW-AS RADIO-SET VERTICAL
        RADIO-BUTTONS 
        "1 Etiqueta por linha", 1,
        "2 Etiquetas por linha", 2
        SIZE 25 BY 2 NO-UNDO.

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

DEFINE VARIABLE rsDestiny AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsExecution AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.72 BY .92
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 13.96 COL 2
     btCancel AT ROW 13.96 COL 13
     btHelp2 AT ROW 13.96 COL 80
     rtToolBar AT ROW 13.75 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 14.46
         FONT 1.

DEFINE FRAME fPage6
     rsDestiny AT ROW 2.38 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     btFile AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     btConfigImpr AT ROW 3.58 COL 43.29 HELP
          "Configura‡Æo da impressora"
     cFile AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rsExecution AT ROW 5.75 COL 3 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.63 COL 1.86 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.81
         SIZE 76.86 BY 10.15
         FONT 1.

DEFINE FRAME fPage2
     cCodEstabel AT ROW 2.25 COL 18 COLON-ALIGNED HELP
          "C¢digo do estabelecimento"
     cNrPedcliIni AT ROW 3.25 COL 18 COLON-ALIGNED HELP
          "N£mero do pedido do cliente"
     cNrPedcliFim AT ROW 3.25 COL 48 COLON-ALIGNED HELP
          "N£mero do pedido do cliente" NO-LABEL
     IMAGE-1 AT ROW 3.25 COL 35.14
     IMAGE-2 AT ROW 3.25 COL 46.86
    i-nr-sequencia-ini AT ROW 4.25 COL 18 COLON-ALIGNED HELP
         "Seq. do pedido do cliente"
    i-nr-sequencia-fim AT ROW 4.25 COL 48 COLON-ALIGNED HELP
         "Seq. do pedido do cliente" NO-LABEL
    IMAGE-3 AT ROW 4.25 COL 35.14
    IMAGE-4 AT ROW 4.25 COL 46.86
    cNrPalini AT ROW 5.25 COL 18 COLON-ALIGNED HELP
          "N£mero do palete inicial"
    cNrBobini AT ROW 6.25 COL 18 COLON-ALIGNED HELP
          "N£mero da bobina inicial"
    rEtq AT ROW 8.25 COL 18 COLON-ALIGNED HELP
          "N£mero de etiqueta por linha"

    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.81
         SIZE 76.86 BY 10.15
         FONT 1.


/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */


/* *************************  Create Window  ************************** */


IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wReport ASSIGN
         HIDDEN             = YES
         TITLE              = "ImpressÆo Mini Etiquetas"
         HEIGHT             = 14.5
         WIDTH              = 90.43
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22
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
ELSE wReport = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */



/* ************************* Included-Libraries *********************** */


/* Procedure Description
"Method Library principal para Maintenance Template, que cont‚m defini‡äes e chamadas a outras Method Libraries."
*/


/*--------------------------------------------------------------------------
    Library    : Report/Report.i
    Purpose    : Method Library principal para Report Template, que 
                 cont‚m defini‡äes e chamadas a outras Method Libraries 

    Authors    : Fabiano Espindola

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* Global Variable Definitions ---                                        */
DEFINE NEW GLOBAL SHARED VARIABLE hWindowStyles AS HANDLE NO-UNDO.

/* Local Temp-Table Definitions ---                                       */
/*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/

    
/* Procedure Description
"Include com defini‡Æo da temptable RowErrors."
*/


/*--------------------------------------------------------------------------
    Library    : method/dbotterr.i
    Purpose    : Include com defini‡Æo da temptable RowErrors

    Author     : John Cleber Jaraceski

    Notes      :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  **************************** */

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */



 


/* Local Variable Definitions ---                                         */
DEFINE VARIABLE cAction              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cButtonsState        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE deColPanel           AS DECIMAL     NO-UNDO
    DECIMALS 2 INITIAL 31.
DEFINE VARIABLE hFolder              AS HANDLE      NO-UNDO.
DEFINE VARIABLE hProgramZoom         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hQueryJoins          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hReportsJoins        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hShowMsg             AS HANDLE      NO-UNDO.
DEFINE VARIABLE lMultipleAdd         AS LOGICAL     NO-UNDO
    INITIAL YES.
DEFINE VARIABLE lCustomExecuted      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lOverrideExecuted    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE rCurrent             AS ROWID       NO-UNDO.

DEFINE VARIABLE c-nom-prog-dpc-mg97  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-nom-prog-appc-mg97 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-nom-prog-upc-mg97  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE wh-pesquisa          AS HANDLE      NO-UNDO.
DEFINE VARIABLE c-arq-old            AS CHAR        NO-UNDO.
DEFINE VARIABLE c-arq-old-batch      AS CHAR        NO-UNDO.

DEFINE VARIABLE c-imp-old            AS CHAR        NO-UNDO.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 



/* ************************* Included-Libraries *********************** */

/***************************************************************************
**  Programa : UT-GLOB.I
**  Include padrão para definição de variaveis globais.
***************************************************************************/ 


     
     def new global shared var i-ep-codigo-usuario  like mgmulti.empresa.ep-codigo no-undo.
     def new Global shared var l-implanta           as logical    init no.
     def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
     def new global shared var i-num-ped-exec-rpw   as integer no-undo.   
     def var rw-log-exec                            as rowid no-undo.
     def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
     def new global shared var l-rpc as logical no-undo.
     def var c-erro-rpc as character format "x(60)" initial " " no-undo.
     def var c-erro-aux as character format "x(60)" initial " " no-undo.
     def var c-ret-temp as char no-undo.
     def var h-servid-rpc as handle no-undo.     
     def new global shared var r-registro-atual as rowid no-undo.
     def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
     def new global shared var h-rsocial as handle no-undo.
     def new global shared var l-achou-prog as logical no-undo.
     
      /* Variáveis Padrão DWB / Datasul HR */
     def new global shared var i-num-ped as integer no-undo.         
     def new global shared var v_cdn_empres_usuar   like mgmulti.empresa.ep-codigo        no-undo.
     def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.
     def new global shared var h_prog_segur_estab     as handle                   no-undo.
     def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
     def new global shared var v_num_tip_aces_usuar   as int                      no-undo.


/* Transformacao Window */

    if session:window-system <> "TTY" then do:
                /* ---------------------------------------------------------------------------------------------------
   Autor      : Medeiros
   Data       : 01/Dez/97
   Objetivo   : Include usada nos programas que fazem interface 
                con a API do windows.
    
   Parametros : Nenhum.
--------------------------------------------------------------------------------- */


  
    /* 32-bit definitions, Progress 8.2+ */

    /* data types */
                   /* libraries */
                     /* messages */
/* mouse buttons */
/* scrollbars */
/* editors */
   /* some window styles */
/* some extended window styles */
/* system commands/menu */
/* placement order (Z-order) */
 
/* window-positioning flags */
/* get a handle to the procedure definitions */

   DEFINE NEW GLOBAL SHARED VARIABLE hpApi AS HANDLE NO-UNDO.
    IF NOT VALID-HANDLE(hpApi) OR
          hpApi:TYPE <> "PROCEDURE":U OR 
          hpApi:FILE-NAME <> "utp/ut-win.p":U THEN 
      RUN utp/ut-win.p PERSISTENT SET hpApi.
    /* forward function declarations. Must not be included in windows.p : */
   /* ---------------------------------------------------------------------------------------------------
   Autor      : Medeiros
   Data       : 01/Dez/97
   Objetivo   : Include usada nos programas que fazem interface 
                con a API do windows.
    
   Parametros : Nenhum.
--------------------------------------------------------------------------------- */   

/* prevent multiple inclusion: */


/* start persistent procedure holding the function implementations.
   The forward declarations are needed in winfunc.p, but the
   "run winfunc.p persistent" part must be prevented in winfunc.p : */
     
DEFINE NEW GLOBAL SHARED VARIABLE hpWinFunc AS HANDLE NO-UNDO.

  IF NOT VALID-HANDLE(hpWinFunc) or  
         hpWinFunc:TYPE <> "PROCEDURE":U or
         hpWinFunc:FILE-NAME <> "utp/ut-func.p":U THEN 
     RUN utp/ut-func.p PERSISTENT SET hpWinFunc.


/* --- the forward declarations : --- */

FUNCTION GetLastError      /* 1:1 implementation of API */
         RETURNS INTEGER   /* = dwErrorID */
         () 
         IN hpWinFunc.    

FUNCTION GetParent         /* 1:1 implementation of API */
         RETURNS INTEGER   /* = hWnd van parent */
         (input hwnd as INTEGER) 
         IN hpWinFunc.    

FUNCTION ShowLastError     /* calls GetLastError and views it as alert-box */
         RETURNS INTEGER   /* = dwErrorID */
         () 
         IN hpWinFunc.    

FUNCTION CreateProcess     /* wrapper for the big API definition */
         RETURNS INTEGER   /* = if success then hProcess else 0  */
         (input CommandLine as CHAR,
          input CurrentDir  as CHAR,
          input wShowWindow as INTEGER) 
         in hpWinFunc.    

/* &IF DEFINED(WINFUNC_I)=0 */

 

/* &IF DEFINED(WINDOWS_I)=0 */

 
      define var h-prog     as handle  no-undo.
      define var h-pai      as handle  no-undo.
      define var c-prog-tec as char    no-undo format "x(256)".
      define var i-template as integer no-undo.
    end.  

/* Transformacao Window */
/* Retorno RPC */

    procedure pi-seta-return-value:
    def input param ret as char no-undo.
    return ret.
  end procedure.


/* Retorno RPC */

/* ut-glob.i */

 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

/********************************************************************************
** Programa : include/i-sysvar.i
**
** Data : 02/06/1999
**
** Cria‡Æo : John Cleber Jaraceski
**
** Objetivo : Definicao das System Variables
**
** Ultima Alt : ?
*******************************************************************************/


        
    DEFINE VARIABLE c-programa-mg97       AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-versao-mg97         AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-modulo-mg97         AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-titulo-prog-mg97    AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-nom-manual-hlp-mg97 AS CHARACTER FORMAT "x(06)":U NO-UNDO.
    DEFINE VARIABLE c-cod-mod-mg97        AS CHARACTER                  NO-UNDO.
    DEFINE VARIABLE d-data-contrato       AS DATE                       NO-UNDO.
    DEFINE VARIABLE i-num-topico-hlp-mg97 AS INTEGER                    NO-UNDO.
    DEFINE VARIABLE i-user-conectados     AS INTEGER                    NO-UNDO.
    DEFINE VARIABLE i-licenca-usuar       AS INTEGER                    NO-UNDO.
    DEFINE VARIABLE l-acesso-livre        AS LOGICAL                    NO-UNDO.


/* include/i-sysvar.i ---                                                     */

 
/***************************************************************************
**
**   btb008za.i0  -  Include para defini‡Æo de fun‡äes do RPC.
**
***************************************************************************/

FUNCTION rpc_exec         RETURNS logical   (input p_cod_program as character) in h-servid-rpc.
FUNCTION rpc_server       RETURNS handle    (input p_cod_program as character) in h-servid-rpc.
FUNCTION rpc_program      RETURNS character (input p_cod_program as character) in h-servid-rpc.
FUNCTION rpc_tip_exec     RETURNS logical   (input p_cod_program as character) in h-servid-rpc.
FUNCTION rpc_exec_set     RETURNS logical   (input p_cod_program as character, 
                                             input p_log_value as logical)     in h-servid-rpc.
                                             
DEFINE VARIABLE c-lst-prg-rpc as char    no-undo.
DEFINE VARIABLE l-inf-prg     as logical no-undo.
DEFINE VARIABLE i-prg-rpc     as integer no-undo.
 
/*** Alterado por Farley - em 23/07/2003 ***/
 
/* _UIB-CODE-BLOCK-END */







/* ***************************  Main Block  *************************** */
cCodEstabel = STRING({cdp\poloestab.i 422}). /*solic-318*/ .
/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */




PROCEDURE applyReturn :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de RETURN
  Parameters:  
  Notes:       Torna-se necess rio o comando CASE pois a trigger de RETURN,
               aparentemente, executa autom ticamente a op‡Æo NO-APPLY
------------------------------------------------------------------------------*/
    CASE SELF:TYPE:
        /*--- Evento RETURN padrÆo para widgets do tipo editor ---*/
        WHEN "EDITOR":U THEN
            SELF:INSERT-STRING(CHR(10)).
        
        /*--- Evento RETURN padrÆo para widgets do tipo button ---*/
        WHEN "BUTTON":U THEN
            APPLY "CHOOSE":U TO SELF.
        
        /*--- Evento RETURN padrÆo para outros widgets ---*/
        OTHERWISE
            APPLY "RETURN":U TO SELF.
    END CASE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE destroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     Destr¢i programa
  Parameters:  
  Notes:       Destr¢i programas de: Folder, Consultas e Relat¢rios Relacionados
------------------------------------------------------------------------------*/
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("BeforedestroyInterface":U) <> "":U THEN DO:
    
        RUN BeforedestroyInterface IN THIS-PROCEDURE NO-ERROR.
    
    
    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*Inserida a chamada da include i-logfin1.i
    devido aos thintemplates nÆo gerarem log de 
    execucao de programas*/
    /*************************************************************************
**
** I-LOGFIN1.I - Encerra o Log de Execucao
**
**************************************************************************/

run btb/btb918zb.p (input c-programa-mg97,
                    input-output rw-log-exec,
                    input no).
 

    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Destr¢i DBO ---*/
        
    
    
    /*--- Destr¢i programa de folder ---*/
    
        IF VALID-HANDLE(hFolder) THEN
            DELETE PROCEDURE hFolder.
    
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("AfterdestroyInterface":U) <> "":U THEN DO:
    
        RUN AfterdestroyInterface IN THIS-PROCEDURE NO-ERROR.
    
    
    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    /*--- Destr¢i os Servidores RPC inicializados pelos DBOs ---*/
    /***************************************************************************
**
**   btb008za.i3  -  Include para finaliza‡Æo RPC.
**
***************************************************************************/


        assign l-inf-prg = no.    



    



if l-inf-prg = yes then do:
    run pi_destroy_rpc in h-servid-rpc ("").
end.
else do i-prg-rpc = 1 to num-entries(c-lst-prg-rpc):
    run pi_destroy_rpc in h-servid-rpc (entry(i-prg-rpc,c-lst-prg-rpc)).
end.
 
    
    /*Alteracao para deletar da mem¢ria o WindowStyles e o btb008za.p*/
    IF VALID-HANDLE(h-servid-rpc) THEN
    DO:
       DELETE PROCEDURE h-servid-rpc.
       ASSIGN h-servid-rpc = ?. /*Garantir que a vari vel nÆo vai mais apontar para nenhum handle de outro objeto - este problema apareceu na v9.1B com Windows2000*/
    END.

    /*--- Destr¢i janela associada ao programa ---*/
    IF VALID-HANDLE(wReport) THEN
        DELETE WIDGET wReport.
    
    /*--- Destr¢i programa ---*/
    IF THIS-PROCEDURE:PERSISTENT THEN
        DELETE PROCEDURE THIS-PROCEDURE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE disableFields :
/*------------------------------------------------------------------------------
  Purpose:     Desabilita os campos da temp-table {&tt-table} 
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("BeforedisableFields":U) <> "":U THEN DO:
    
        RUN BeforedisableFields IN THIS-PROCEDURE NO-ERROR.
    
    
    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-DISABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-DISABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-DISABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Desabilita campos contidos no {&page1Fields} ---*/
    
    
    /*--- Desabilita campos contidos no {&page2Fields} ---*/
    
        DO WITH FRAME fPage2:
            DISABLE cCodEstabel cNrPedcliIni cNrPedcliFim i-nr-sequencia-ini i-nr-sequencia-fim cNrPalini cNrBobini retq.
        END.
    
    
    /*--- Desabilita campos contidos no {&page3Fields} ---*/
    
    
    /*--- Desabilita campos contidos no {&page4Fields} ---*/
    
    
    /*--- Desabilita campos contidos no {&page5Fields} ---*/
    
    
    /*--- Desabilita campos contidos no {&page6Fields} ---*/
    
        DO WITH FRAME fPage6:
            DISABLE cFile.
        END.
    
    
    /*--- Desabilita campos contidos no {&page7Fields} ---*/
    
    
    /*--- Desabilita campos contidos no {&page8Fields} ---*/
    
    
    /*--- Desabilita widgets contidos no {&page1Widgets} ---*/
    
    
    /*--- Desabilita widgets contidos no {&page2Widgets} ---*/
    
        DO WITH FRAME fPage2:
            DISABLE cCodEstabel cNrPedcliIni cNrPedcliFim i-nr-sequencia-ini i-nr-sequencia-fim cNrPalini cNrBobini retq.
        END.
    

    /*--- Desabilita widgets contidos no {&page3Widgets} ---*/
    

    /*--- Desabilita widgets contidos no {&page4Widgets} ---*/
    

    /*--- Desabilita widgets contidos no {&page5Widgets} ---*/
    

    /*--- Desabilita widgets contidos no {&page6Widgets} ---*/
    
        DO WITH FRAME fPage6:
            DISABLE rsDestiny                               btConfigImpr                               btFile                               rsExecution.
        END.
    

    /*--- Desabilita widgets contidos no {&page7Widgets} ---*/
    

    /*--- Desabilita widgets contidos no {&page8Widgets} ---*/
    

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-DISABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-DISABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-DISABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("AfterdisableFields":U) <> "":U THEN DO:
    
        RUN AfterdisableFields IN THIS-PROCEDURE NO-ERROR.
    
    
    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE dispatch :
/*------------------------------------------------------------------------------
  Purpose:     Manter compatibilidade com SmartObjects
  Parameters:  recebe m‚todo a ser executado
  Notes:       Somente haver  tratamento para o m‚todo initialize, quando a 
               execu‡Æo deste m‚todo for solicitada ser  executado o m‚todo
               initializeInterface
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pMethod AS CHARACTER NO-UNDO.
    
    IF pMethod = "INITIALIZE":U THEN
        RUN initializeInterface IN THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE displayText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    
    
    
    
    
    
        DISPLAY text-destino text-modo WITH FRAME fPage6.
    
    
    

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE enableFields :
/*------------------------------------------------------------------------------
  Purpose:     Habilita os campos da temp-table {&tt-table} 
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE hFieldEntryAux    AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lExecutedEntryAux AS LOGICAL NO-UNDO.
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("BeforeenableFields":U) <> "":U THEN DO:
    
        RUN BeforeenableFields IN THIS-PROCEDURE NO-ERROR.
    
    
    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-ENABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-ENABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-ENABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Habilita campos contidos no {&page1Fields} ---*/
    
    
    /*--- Habilita campos contidos no {&page2Fields} ---*/
    
        ENABLE cCodEstabel cNrPedcliIni cNrPedcliFim  i-nr-sequencia-ini i-nr-sequencia-fim cNrPalini cNrBobini rEtq WITH FRAME fPage2.
        DISPLAY cCodEstabel cNrPedcliIni cNrPedcliFim  i-nr-sequencia-ini i-nr-sequencia-fim cNrPalini cNrBobini rEtq WITH FRAME fPage2.
    
    
    /*--- Habilita campos contidos no {&page3Fields} ---*/
    
    
    /*--- Habilita campos contidos no {&page4Fields} ---*/
    
    
    /*--- Habilita campos contidos no {&page5Fields} ---*/
    
    
    /*--- Habilita campos contidos no {&page6Fields} ---*/
    
        ENABLE cFile WITH FRAME fPage6.
        DISPLAY cFile WITH FRAME fPage6.
    
    
    /*--- Habilita campos contidos no {&page7Fields} ---*/
    
    
    /*--- Habilita campos contidos no {&page8Fields} ---*/
    

    /*--- Habilita widgets contidos no {&page0Widgets} ---*/
    
        ENABLE btOk                               btCancel                               btHelp2 WITH FRAME fPage0.
        DISPLAY btOk                               btCancel                               btHelp2 WITH FRAME fPage0.
    
    
    /*--- Habilita widgets contidos no {&page1Widgets} ---*/
    
    
    /*--- Habilita widgets contidos no {&page2Widgets} ---*/
    
        ENABLE cCodEstabel cNrPedcliIni cNrPedcliFim   i-nr-sequencia-ini i-nr-sequencia-fim cNrPalini cNrBobini rEtq WITH FRAME fPage2.
        DISPLAY cCodEstabel cNrPedcliIni cNrPedcliFim  i-nr-sequencia-ini i-nr-sequencia-fim cNrPalini cNrBobini rEtq WITH FRAME fPage2.
    

    /*--- Habilita widgets contidos no {&page3Widgets} ---*/
    

    /*--- Habilita widgets contidos no {&page4Widgets} ---*/
    

    /*--- Habilita widgets contidos no {&page5Widgets} ---*/
    

    /*--- Habilita widgets contidos no {&page6Widgets} ---*/
    
        ENABLE rsDestiny                               btConfigImpr                               btFile                               rsExecution WITH FRAME fPage6.
        DISPLAY rsDestiny                               btConfigImpr                               btFile                               rsExecution WITH FRAME fPage6.
        if rsDestiny:screen-value in frame fPage6 = "3" then 
            assign cFile:hidden in frame fPage6 = yes
                   btFile:hidden in frame fPage6 = yes
                   btConfigImpr:hidden in frame fPage6 = yes.  
    

    /*--- Habilita widgets contidos no {&page7Widgets} ---*/
    

    /*--- Habilita widgets contidos no {&page8Widgets} ---*/
    

    
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.


IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-ENABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-ENABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-ENABLE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("AfterenableFields":U) <> "":U THEN DO:
    
        RUN AfterenableFields IN THIS-PROCEDURE NO-ERROR.
    
    
    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE initializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     Inicialize programa
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*Alterado por Anderson (tech485) para o template mostrar o nome da empresa*/

    def var c_cod_empres_usuar as char no-undo.
    def var c_nom_razao_social as char no-undo.

    /*fim alteracao Anderson*/



    /*--- Inicializa‡Æo de OCXs ---*/
    IF THIS-PROCEDURE:GET-SIGNATURE("control_load":U) <> "":U THEN DO:
        RUN control_load IN THIS-PROCEDURE NO-ERROR.
        VIEW FRAME fPage0 IN WINDOW wReport.
    END.
    
    /*--- Executa valida‡äes de inicializa‡Æo ---*/
    ASSIGN c-programa-mg97 = CAPS("essf0004":U)
           c-versao-mg97   = "2.00.00.000":U.
    /*--------------------------------------------------------------------------
    File        : UTP/UT-VFSEC.I
    Purpose     : Verifica‡Æo da Seguran‡a

    Syntax      :

    Description : Verificar a seguran‡a

    Author(s)   : Fabiano
    Created     : 31/12/1997
    Notes       :
------------------------------------------------------------------------*/
/* NÆo faz a valida‡Æo para programas do tipo V  Para */
if index(replace(program-name(1),"~\","~/"),"go/g") = 0 then do:
  run men/men901za.p (input c-programa-mg97).
  if  return-value = "2012" then do:
      run utp/ut-msgs.p (input "show",
                         input 2858,
                         input c-programa-mg97).
      if "Window" = "SmartDialog" or this-procedure:persistent = no then
        return "adm-error".
      else do:     
        delete procedure this-procedure.
        return.
      end.  
  end.                       

  if  return-value = "2014" then do:
      run utp/ut-msgs.p (input "show",
                         input 3045,
                         input c-programa-mg97).
      if "Window" = "SmartDialog" then
        return "adm-error".
      else do:
        delete procedure this-procedure.
        return.
      end.  
  end.
end.  

/* ut-vfsec.i */
 
    
    /*Inserida a chamada da include i-logini.i
    devido aos thintemplates nÆo gerarem log de 
    execucao de programas*/
    /*************************************************************************
**                                                                        
**  I-LOGINI.I - Inicia Log de Execu‡Æo
**
*************************************************************************/

run btb/btb918zb.p (input c-programa-mg97,
                    input-output rw-log-exec,
                    input yes).

/* i-logini */

 
    
    /*Alterado por Anderson (tech485) para o template mostrar o nome da empresa*/
    /*{utp/ut9000.i "{&Program}" "{&Version}"} *//*esta foi comentada por estar causando erro nos thintemplates*/
    /*rodar pi-rsocial persistent para verifica»’o empresa usuario*/
   
    /* alterado por Valdir (tech264) novo m‚todo de teste do valid-handle */   
    if not valid-handle(h-rsocial) or
       h-rsocial:TYPE <> "PROCEDURE":U or
       h-rsocial:FILE-NAME <> "utp/ut-rsocial.p":U then do:
        if l-achou-prog then
            run utp/ut-rsocial.p persistent set h-rsocial.
    end.
    if l-achou-prog then
        run pi-rsocial in h-rsocial (output c_cod_empres_usuar, output c_nom_razao_social).
    
    find prog_dtsul no-lock
        where prog_dtsul.cod_prog_dtsul = c-programa-mg97 no-error.
    if  avail prog_dtsul then do:
        assign c-titulo-prog-mg97    = prog_dtsul.des_prog_dtsul
               c-nom-prog-upc-mg97   = prog_dtsul.nom_prog_upc
               c-nom-prog-appc-mg97  = prog_dtsul.nom_prog_appc
               c-nom-prog-dpc-mg97   = prog_dtsul.nom_prog_dpc
               i-num-topico-hlp-mg97 = prog_dtsul.num_topico.
       
/*          if session:window-system <> "TTY":U then 
            assign i-template          = prog_dtsul.ind_template.*/
       

        
        find procedimento no-lock
            where procedimento.cod_proced = prog_dtsul.cod_proced no-error.
        if  avail procedimento then do:
            find modul_dtsul no-lock
                where modul_dtsul.cod_modul_dtsul = procedimento.cod_modul_dtsul no-error. 
            if  avail modul_dtsul then do:
                assign c-modulo-mg97         = caps(modul_dtsul.nom_modul_dtsul_menu)
                       c-cod-mod-mg97        = caps(modul_dtsul.cod_modul_dtsul)
                       c-nom-manual-hlp-mg97 = "dochlp~/":U + string(modul_dtsul.num_manual_documen, "999999":U) + ".hlp":U.
            end.
        end.
    end.                                                      
    else do:
        assign c-titulo-prog-mg97    = caps(c-programa-mg97)
               c-nom-prog-upc-mg97   = ""
               c-nom-prog-appc-mg97  = ""
               i-num-topico-hlp-mg97 = 0
               c-nom-manual-hlp-mg97 = "dochlp~/000000.hlp":U.
    end.                 

    /* Tradu‡Æo T¡tulo dos Programas */
    run utp/ut-liter.p (input c-titulo-prog-mg97 ,
                        input "*":U,
                        input "":U). 

    Assign c-titulo-prog-mg97 = Return-value.
     
    
         assign wReport:title = if l-achou-prog then
                                       c-titulo-prog-mg97
                                     + " - ":U 
                                     + c-programa-mg97 
                                     + " - ":U 
                                     + c-versao-mg97  
                                     + " - ":U 
                                     + c_cod_empres_usuar
                                     + " - ":U 
                                     + c_nom_razao_social
                                     else 
                                       c-titulo-prog-mg97
                                     + " - ":U 
                                     + c-programa-mg97 
                                     + " - ":U 
                                     + c-versao-mg97.
    
    

    /*fim alteracao Anderson(tech485)*/

    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("BeforeinitializeInterface":U) <> "":U THEN DO:
    
        RUN BeforeinitializeInterface IN THIS-PROCEDURE NO-ERROR.
    
    
    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN DO:
        /*--- Destr¢i programa ---*/
        RUN destroyInterface IN THIS-PROCEDURE.
        
        RETURN "NOK":U.
    END.
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

/* _UIB-CODE-BLOCK-END */



 
    
    DO WITH FRAME fPage0:
    
    /*--- Executa programa de folder ---*/
    
        RUN utp/thinFolder.w PERSISTENT SET hFolder.
        
        RUN setOCX IN hFolder (INPUT THIS-PROCEDURE:GET-SIGNATURE("control_load":U) <> "":U) NO-ERROR.
        
        RUN initializeFolders IN hFolder (INPUT FRAME fPage0:HANDLE,
                                          INPUT 
                                                
                                                
                                                    
                                                    STRING(FRAME fPage2:HANDLE)
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                    
                                                    + ",":U +
                                                    
                                                    STRING(FRAME fPage6:HANDLE)
                                                

                                                ,
                                          INPUT "Sele‡Æo,ImpressÆo":U,
                                          INPUT NUM-ENTRIES("Sele‡Æo,ImpressÆo":U),
                                          INPUT ?,
                                          INPUT ?,
                                          INPUT ?,
                                          INPUT FRAME fPage0:HEIGHT - 
                                                
                                                        FRAME fPage2:ROW 
                                                    
                                                                        - 0.75 ).


    
    END.
    
    /*--- Executa programa de estilo de janelas ---*/
    IF  VALID-HANDLE(hWindowStyles) = NO OR
        hWindowStyles:TYPE <> "PROCEDURE":U OR
        (hWindowStyles:FILE-NAME <> "utp/WindowStyles.p":U AND
        hWindowStyles:FILE-NAME <> "utp/WindowStyles.r":U) THEN
        RUN utp/WindowStyles.p PERSISTENT SET hWindowStyles.
    
    /*--- Seta estilo de janela para thinMaintenance ---*/
    RUN disableMax IN hWindowStyles (INPUT wReport:hWnd).
    
    /*--- Inicializa DBOs ---*/
    /* RUN initializeDBOs IN THIS-PROCEDURE. */
    
    /*--- Habilita Widgets e Campos ---*/
    RUN enableFields IN THIS-PROCEDURE.
    
    /*--- Mostra textos nas frames ---*/
    RUN displayText IN THIS-PROCEDURE.
    
    /*--- Verifica se a inicializa‡Æo do DBO foi feita com sucesso ---*/
/*    &IF "{&hDBOTable}":U <> "":U &THEN
 *         IF NOT VALID-HANDLE({&hDBOTable}) THEN DO:
 *             /*--- Exibir mensagem de finaliza‡Æo do programa ---*/
 *             RUN utp/ut-msgs.p (INPUT "SHOW":U, 
 *                                INPUT 18881, 
 *                                INPUT CAPS("{&Program}":U) + "~~" + CAPS("{&Version}":U)).
 *             
 *             /*--- Destr¢i programa ---*/
 *             RUN destroyInterface IN THIS-PROCEDURE.
 *             
 *             RETURN "NOK":U.
 *         END.
 *     &ENDIF*/
    
    
        RUN setFolder IN hFolder (INPUT 1).
    
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.
    
    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "'?'":U,
                                    INPUT '?').
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("AfterinitializeInterface":U) <> "":U THEN DO:
    
        RUN AfterinitializeInterface IN THIS-PROCEDURE NO-ERROR.
    
    
    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN DO:
        /*--- Destr¢i programa ---*/
        RUN destroyInterface IN THIS-PROCEDURE.
        
        RETURN "NOK":U.
    END.

    /*** Alterado por Farley - em 23/07/2003 ***/
    
    
    /*--- Visualiza janela ---*/
    VIEW wReport.
    APPLY "ENTRY":U TO FRAME fPage0.
    APPLY "ENTRY":U TO wReport.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */




 

/* _UIB-CODE-BLOCK-END */





/* ***********  Runtime Attributes and AppBuilder Settings  *********** */


/* SETTINGS FOR WINDOW wReport
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage2:FRAME = FRAME fpage0:HANDLE
       FRAME fPage6:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR FRAME fPage2
   Custom                                                               */
/* SETTINGS FOR FRAME fPage6
                                                                        */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME fPage6     = 
                "Destino".

ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME fPage6     = 
                "Execu‡Æo".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wReport)
THEN wReport:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */



/* Setting information for Queries and Browse Widgets fields            */


/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */



/* Query rebuild information for FRAME fPage2
     _Query            is NOT OPENED
*/  /* FRAME fPage2 */



/* Query rebuild information for FRAME fPage6
     _Query            is NOT OPENED
*/  /* FRAME fPage6 */


 



/* ************************  Control Triggers  ************************ */


ON END-ERROR OF wReport /* ImpressÆo Mini Etiquetas */
OR ENDKEY OF wReport ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON WINDOW-CLOSE OF wReport /* ImpressÆo Mini Etiquetas */
DO:
  /* This event will close the window and terminate the procedure.  */
  /*************************************************************************
**
** LOGFIN.I - Encerra o Log de Execu‡Æo
**
**************************************************************************/

run btb/btb918zb.p (input c-programa-mg97,
                    input-output rw-log-exec,
                    input no).

/* Transformacao Window */

if session:window-system <> "TTY":U then do:
    case i-template:
        when 9 or when 10 or when 20 or when 30 or when 31 then do: 
            assign h-pai:sensitive = yes.
            apply "ENTRY":U to h-pai.
        end.
        when 13 then do:
            assign h-pai:sensitive = yes.
            apply "ENTRY":U to h-pai.
            run pi-entry-atributos-chave.
        end.
    end case.
end.  

/* Transformacao Window */
/* Elimina‡Æo de arquivos tempor rios */


/* Fim da elimina‡Æo de arquivos tempor rios */

/* end of logfin */
   
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btCancel IN FRAME fpage0 /* Fechar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btConfigImpr IN FRAME fPage6
DO:
   /***************************************************************
**
** RPIMP.I - Choose of bt-config-impr do template de relat¢rio
**
***************************************************************/ 

def var cTempFile as char no-undo.
def var cPrinter  as char no-undo.
def var cAuxFile  as char no-undo.
def var cLayout   as char no-undo.
def var cPrev     as char no-undo.

assign cPrev     = cFile:screen-value in frame fPage6
       cTempFile = replace(cFile:screen-value in frame fPage6,":":U,",":U).
if cFile:screen-value in frame fPage6 <> "" then do:
  if num-entries(cTempFile) = 4 then
    assign cPrinter = entry(1,cTempFile)
           cLayout  = entry(2,cTempFile)
           cAuxFile = entry(3,cTempFile) + ":":U + entry(4,cTempFile).
  if num-entries(cTempFile) = 3 then
    assign cPrinter = entry(1,cTempFile)
           cLayout  = entry(2,cTempFile)
           cAuxFile = entry(3,cTempFile).
  if num-entries(cTempFile) = 2 then
    assign cPrinter = entry(1,cTempFile)
           cLayout  = entry(2,cTempFile)
           cAuxFile = "".
end.         

run utp/ut-impr.w (input-output cPrinter, input-output cLayout, input-output cAuxFile).

if cAuxFile = "" then
  assign cFile = cPrinter + ":":U + cLayout.
else
  assign cFile = cPrinter + ":":U + cLayout + ":":U + cAuxFile.  

if cFile = ":":U then
  assign cFile = cPrev.

assign c-imp-old = cFile.
  
disp cFile with frame fPage6.

/* end of rpimp.i */
 
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btFile IN FRAME fPage6
DO:
    /*****************************************************************
**
** RPARQ - Choose of btFile no template de relat¢rio
**
*****************************************************************/

    def var cArqConv  as char no-undo.

    assign cArqConv = replace(input frame fPage6 cFile, "/":U, "\":U).
    SYSTEM-DIALOG GET-FILE cArqConv
       FILTERS "*.lst":U "*.lst":U,
               "*.*":U "*.*":U
       ASK-OVERWRITE 
       DEFAULT-EXTENSION "lst":U
       INITIAL-DIR session:temp-directory
       SAVE-AS
       USE-FILENAME
       UPDATE l-ok.
    if  l-ok = yes then do:
        assign cFile = replace(cArqConv, "\":U, "/":U).
        display cFile with frame fPage6.
    end.

/* end of rparq */
 
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btHelp2 IN FRAME fpage0 /* Ajuda */
DO:
    /*************************************************************************
**
** AJUDA.I - Include padrÆo para chamada do Help
**
**************************************************************************/


    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).


RETURN NO-APPLY.

/* include/ajuda.i */
 
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btOK IN FRAME fpage0 /* Executar */
DO:
   do  on error undo, return no-apply:
       run piExecute.
   end.
END.

/* _UIB-CODE-BLOCK-END */




ON VALUE-CHANGED OF rsDestiny IN FRAME fPage6
DO:
do  with frame fPage6:
    case self:screen-value:
        when "1" then do:
            assign cFile:sensitive       = no
                   cFile:visible         = yes
                   btFile:visible        = no
                   btConfigImpr:visible  = yes.
        end.
        when "2" then do:
            assign cFile:sensitive       = yes
                   cFile:visible         = yes
                   btFile:visible        = yes
                   btConfigImpr:visible  = no.
        end.
        when "3" then do:
            assign cFile:visible         = no
                   cFile:sensitive       = no
                   btFile:visible        = no
                   btConfigImpr:visible  = no.
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */




ON VALUE-CHANGED OF rsExecution IN FRAME fPage6
DO:
   /****************************************************************
**
** RPRSE.I - Gatilho "Value-Changed" de rs-execucao 
**
*****************************************************************/

ASSIGN rsExecution.

IF  rsExecution = 2 THEN DO:
    IF  rsDestiny:SCREEN-VALUE IN FRAME fPage6 <> "1":U THEN
        ASSIGN rsDestiny:SCREEN-VALUE IN FRAME fPage6 = "2":U
               cFile           = IF cFile:SCREEN-VALUE IN FRAME fPage6 = "":U
                                 THEN IF cFile = "" 
                                      THEN c-arq-old
                                      ELSE cFile
                                 ELSE cFile:SCREEN-VALUE IN FRAME fPage6
               c-arq-old       = cFile
               c-arq-old-batch = SUBSTRING(cFile, R-INDEX(cFile, "/":U) + 1).
    
    
    APPLY "VALUE-CHANGED":U TO rsDestiny IN FRAME fPage6.

    assign cFile:screen-value = c-arq-old-batch.
    
    ASSIGN cFile.

    assign c-terminal = trim(c-terminal).
            
    rsDestiny:DISABLE(c-terminal) IN FRAME fPage6.
END.
ELSE DO:
    IF  rsDestiny:SCREEN-VALUE IN FRAME fPage6 <> "1":U THEN
        ASSIGN cFile           = IF cFile:SCREEN-VALUE IN FRAME fPage6 = "":U
                                 THEN cFile
                                 ELSE cFile:SCREEN-VALUE IN FRAME fPage6
               c-arq-old-batch = cFile.
    
    APPLY "VALUE-CHANGED":U TO rsDestiny IN FRAME fPage6.
    
    assign c-terminal = trim(c-terminal).
    
    rsDestiny:ENABLE(c-terminal) IN FRAME fPage6.
    
    ASSIGN cFile.
END.

/* end of rprse.i */
 
END.

/* _UIB-CODE-BLOCK-END */







/*--- L¢gica para inicializa‡Æo do programam ---*/

/* Procedure Description
"Method Library que cont‚m a l¢gica da Main Block."
*/


/*--------------------------------------------------------------------------
    Library    : Report/MainBlock.i
    Purpose    : Method Library que cont‚m a l¢gica da Main Block 

    Authors    : Fabiano Espindola

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Seta cursor do mouse para espera ---*/
SESSION:SET-WAIT-STATE("GENERAL":U).

/*--- Evento de CLOSE padrÆo para THIS-PROCEDURE ---*/
ON CLOSE OF THIS-PROCEDURE 
   RUN destroyInterface IN THIS-PROCEDURE.

/*--- Evento de RETURN padrÆo para FRAME fPage0 ---*/
ON RETURN OF FRAME fPage0 ANYWHERE
    RUN applyReturn IN THIS-PROCEDURE.

/*--- Evento de CTRL-TAB padrÆo para THIS-PROCEDURE ---*/

    ON CTRL-TAB ANYWHERE
        RUN nextFolder IN hFolder.


/*--- Evento de SHIFT-CTRL-TAB padrÆo para THIS-PROCEDURE ---*/

    ON SHIFT-CTRL-TAB ANYWHERE
        RUN prevFolder IN hFolder.


/*--- Seta CURRENT-WINDOW como sendo a window atual ---*/
ASSIGN CURRENT-WINDOW                = wReport
       THIS-PROCEDURE:CURRENT-WINDOW = wReport.

/*--- PadrÆo para janelas GUI ---*/
PAUSE 0 BEFORE-HIDE.


    IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
        /*--- Inicializa programa ---*/
        RUN initializeInterface IN THIS-PROCEDURE.
        IF RETURN-VALUE = "NOK":U THEN
            RETURN "NOK":U.
    END.



    
        /***********************************************************************
**
**  RPINI.I - Inicializa‡äes do Template de Relat¢rio
**
***********************************************************************/

/* Preprocessor para elimina‡Æo de arquivos tempor rios */

assign wReport:virtual-width-chars  = wReport:width-chars
       wReport:virtual-height-chars = wReport:height-chars
       wReport:min-width-chars      = wReport:width-chars
       wReport:max-width-chars      = wReport:width-chars
       wReport:min-height-chars     = wReport:height-chars
       wReport:max-height-chars     = wReport:height-chars.




    assign cFile:row in frame fPage6    = cFile:row in frame fPage6 - 0.06
           cFile:height in frame fPage6 = 1.
    
    find usuar_mestre 
        where usuar_mestre.cod_usuario = c-seg-usuario 
        no-lock no-error.
    
    if  avail usuar_mestre then 
        assign cFile = if  length(usuar_mestre.nom_subdir_spool) <> 0 then 
                           caps(replace(usuar_mestre.nom_dir_spool, "~\":U, "~/":U) + "~/":U + replace(usuar_mestre.nom_subdir_spool, "~\":U, "~/":U) + "~/":U + "essf0004":U + "~.lst":U)
                       else caps(replace(usuar_mestre.nom_dir_spool, "~\":U, "~/":U) + "~/":U + "essf0004":U + "~.lst":U)
               c-arq-old = cFile.
    else 
        assign cFile     = caps("spool~/":U + "essf0004":U + "~.lst":U)
               c-arq-old = cFile.
    
    run utp/ut-liter.p (input "Terminal":U, 
                        input "*":U,
                        input "R":U).
    
    assign c-terminal = return-value
           rsDestiny  = 3.


/* end of rpini.i */
 
    




    
        /*****************************************************************
**
**  RPLBL.I - Cria os labels para os DumbFolder do relat¢rio
**
*******************************************************************/


    ON "LEAVE":U OF cFile IN FRAME fPage6 DO:
        IF rsExecution = 1 THEN
            ASSIGN c-arq-old = cFile:SCREEN-VALUE.
        ELSE
            ASSIGN c-arq-old-batch = cFile:SCREEN-VALUE.
    END.
    
    ON "ENTER":U      OF cFile IN FRAME fPage6 OR
       "RETURN":U     OF cFile IN FRAME fPage6 OR
       "CTRL-ENTER":U OF cFile IN FRAME fPage6 OR
       "CTRL-J":U     OF cFile IN FRAME fPage6 OR
       "CTRL-Z":U     OF cFile IN FRAME fPage6 do:
        RETURN NO-APPLY.
    END.
    
    ON "\":U OF cFile IN FRAME fPage6 DO:
        APPLY "/":U TO cFile IN FRAME fPage6.
        RETURN NO-APPLY.
    END.
    
    ON "VALUE-CHANGED":U OF rsDestiny IN FRAME fPage6 DO:
        CASE rsDestiny:SCREEN-VALUE IN FRAME fPage6:
            WHEN "1":U THEN DO:
                ASSIGN cFile                                = c-imp-old
                       cFile:SCREEN-VALUE IN FRAME fPage6   = c-imp-old
                       cFile:SENSITIVE IN FRAME fPage6      = NO
                       cFile:VISIBLE IN FRAME fPage6        = YES
                       btFile:VISIBLE IN FRAME fPage6       = NO
                       btConfigImpr:VISIBLE IN FRAME fPage6 = YES.
                       IF c-imp-old = "" OR c-imp-old = ? THEN
                           RUN pi-impres-pad.
            END.
            WHEN "2":U THEN
                ASSIGN cFile = IF rsExecution = 1 
                                   THEN c-arq-old
                                   ELSE c-arq-old-batch
                       cFile:SCREEN-VALUE IN FRAME fPage6 = cFile
                       cFile:SENSITIVE IN FRAME fPage6    = YES
                       cFile:VISIBLE IN FRAME fPage6      = YES
                       btFile:VISIBLE IN FRAME fPage6     = YES
                       btConfigImpr:VISIBLE IN FRAME fPage6 = NO.
            WHEN "3":U THEN
                ASSIGN cFile                                = ""
                       cFile:SCREEN-VALUE IN FRAME fPage6 = cFile
                       cFile:VISIBLE IN FRAME fPage6      = NO
                       btFile:VISIBLE IN FRAME fPage6     = NO
                       btConfigImpr:VISIBLE IN FRAME fPage6 = NO.
        END CASE.
    END.



/********************************************
** HELP FRAME
********************************************/
ON HELP OF FRAME fPage0 DO:
    /*************************************************************************
**
** AJUDA.I - Include padrÆo para chamada do Help
**
**************************************************************************/


    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).


RETURN NO-APPLY.

/* report/ajuda.i */
 
END.


ON HELP OF FRAME fPage2 DO:
    /*************************************************************************
**
** AJUDA.I - Include padrÆo para chamada do Help
**
**************************************************************************/


    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).


RETURN NO-APPLY.

/* report/ajuda.i */
 
END.









ON HELP OF FRAME fPage6 DO:
    /*************************************************************************
**
** AJUDA.I - Include padrÆo para chamada do Help
**
**************************************************************************/


    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).


RETURN NO-APPLY.

/* report/ajuda.i */
 
END.


/* i-rplbl */


procedure pi-impres-pad:

do with frame fPage6:
    find layout_impres_padr no-lock
         where layout_impres_padr.cod_usuario = c-seg-usuario
            and layout_impres_padr.cod_proced = c-programa-mg97  use-index lytmprsp_id /*cl_default_procedure_user of layout_impres_padr*/ no-error.
    if  not avail layout_impres_padr
    then do:
        find layout_impres_padr no-lock
             where layout_impres_padr.cod_usuario = "*"
               and layout_impres_padr.cod_proced = c-programa-mg97  use-index lytmprsp_id /*cl_default_procedure of layout_impres_padr*/ no-error.
        if  avail layout_impres_padr
        then do:
            find imprsor_usuar no-lock
                 where imprsor_usuar.nom_impressora = layout_impres_padr.nom_impressora
                   and imprsor_usuar.cod_usuario = string(c-seg-usuario)
                 use-index imprsrsr_id /*cl_layout_current_user of imprsor_usuar*/ no-error.
        end /* if */.
        if  not avail imprsor_usuar
        then do:
            find layout_impres_padr no-lock
                 where layout_impres_padr.cod_usuario = c-seg-usuario
                   and layout_impres_padr.cod_proced = "*"
                 use-index lytmprsp_id /*cl_default_user of layout_impres_padr*/ no-error.
        end /* if */.
    end /* if */.
    if  avail layout_impres_padr
    then do:
        assign cFile:screen-value in frame fPage6 = layout_impres_padr.nom_impressora
                                    + ":"
                                    + layout_impres_padr.cod_layout_impres.
    end /* if */.
    else do:
         cFile:screen-value in frame fPage6 = "".
    end /* else */.
end /* do dflt */.
end.
/*pi-impres-pad */


/* end of rplbl.i */
 
    



/*--- Block principal do programa ---*/
DO ON ERROR   UNDO, LEAVE
   ON END-KEY UNDO, LEAVE:
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
 
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* define procedure externa para execucao do programa de visualizacao do relatorio */

PROCEDURE WinExec EXTERNAL "kernel32.dll":U:
  DEF INPUT  PARAM prg_name                          AS CHARACTER.
  DEF INPUT  PARAM prg_style                         AS SHORT.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



 

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */


PROCEDURE piExecute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


/*** Relatorio ***/
do on error undo, return error on stop  undo, return error:
    /***************************************************************
**
** RPEXA.I - Saida A na PI-EXECUTAR do template de relat¢rio
**
***************************************************************/ 

def var c-arq-aux    as char no-undo.

/*Altera‡Æo feita por Anderson tech540 em 29/06/2001 para testa se a
p gina de impressÆo est  definida e se seu valor ‚ yes*/


if  input frame fPage6 rsDestiny  = 2 
and input frame fPage6 rsExecution = 1 then do:
    
    assign c-arq-aux = input frame fPage6 cFile
           c-arq-aux = replace(c-arq-aux, "/":U, "\":U).
    if  r-index(c-arq-aux, "\":U) > 0 then do:
        assign file-info:file-name = substring(c-arq-aux,1,r-index(c-arq-aux, "\":U)).
        if  file-info:full-pathname = ? or not file-info:file-type matches "*D*":U then do:
            run utp/ut-msgs.p (input "show":U, 
                               input 5749, 
                               input "").
            apply 'entry':U to cFile in frame fPage6.
            return error.
        end.
    end.
    
    assign file-info:file-name = c-arq-aux.
    if file-info:file-type matches "*D*":U then do:
        run utp/ut-msgs.p (input "show":U, 
                           input 73, 
                           input "").
        apply 'entry':U to cFile in frame fPage6.
        return error.
    end.
end.    



/* end of rpexa */
 
    
    if input frame fPage6 rsDestiny = 2 and
       input frame fPage6 rsExecution = 1 then do:
        run utp/ut-vlarq.p (input input frame fPage6 cFile).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show", input 73, input "").
            apply "ENTRY":U to cFile in frame fPage6.
            return error.
        end.
    end.
   
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame fPage6 rsDestiny
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           tt-param.cod-estabel     = input frame fPage2 cCodEstabel
           tt-param.nr-pedcli-ini   = input frame fPage2 cNrPedcliIni
           tt-param.nr-pedcli-fim   = input frame fPage2 cNrPedcliFim
           tt-param.nr-sequencia-ini = input frame fPage2 i-nr-sequencia-ini
           tt-param.nr-sequencia-fim = input frame fPage2 i-nr-sequencia-fim
           tt-param.nr-pal-ini      = input frame fPage2 cNrPalini
           tt-param.nr-bob-ini      = input frame fPage2 cNrBobini
           tt-param.etq             = input frame fPage2 rEtq.
    
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame fPage6 cFile.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /* Executar do programa RP.P que ir  criar o relat¢rio */
    /***************************************************************
**
** RPEXB.I - Saida B na PI-EXECUTAR do template de relat¢rio
**
***************************************************************/

def var cPrinter as char no-undo.
def var c-layout as char no-undo.

if  tt-param.destino = 1 then do:
  assign tt-param.arquivo = cFile:screen-value in frame fPage6
         cFile = cFile:screen-value in frame fPage6.
  if num-entries(tt-param.arquivo,":":U) = 2 then do:
    assign cPrinter = substring(cFile:screen-value in frame fPage6,1,index(cFile:screen-value in frame fPage6,":":U) - 1)
           c-layout = substring(cFile:screen-value in frame fPage6,index(cFile:screen-value in frame fPage6,":":U) + 1,length(cFile:screen-value in frame fPage6) - index(cFile:screen-value in frame fPage6,":":U)). 
    find imprsor_usuar no-lock
         where imprsor_usuar.nom_impressora = cPrinter
         and imprsor_usuar.cod_usuario = c-seg-usuario
         use-index imprsrsr_id no-error.
    if not avail imprsor_usuar then do:
      run utp/ut-msgs.p (input "show":U, 
                         input 4306, 
                         input c-seg-usuario).
      return error.
    end.       
    find layout_impres no-lock
         where layout_impres.nom_impressora = cPrinter
         and layout_impres.cod_layout_impres = c-layout no-error.
    if not avail layout_impres then do:
      run utp/ut-msgs.p (input "show":U, 
                         input 4306, 
                         input c-seg-usuario).
      return error.
    end.       
  end.  
  else do:
    if num-entries(cFile,":":U) < 2 then do:
      run utp/ut-msgs.p (input "show":U, 
                         input 4306, 
                         input c-seg-usuario).
      return error.
    end.
    assign tt-param.arquivo = cFile:screen-value in frame fPage6.
    assign cPrinter = entry(1,cFile,":":U)
           c-layout = entry(2,cFile,":":U). 
    find imprsor_usuar no-lock
         where imprsor_usuar.nom_impressora = cPrinter
         and imprsor_usuar.cod_usuario = c-seg-usuario
         use-index imprsrsr_id no-error.
    if not avail imprsor_usuar then do:
      run utp/ut-msgs.p (input "show":U, 
                         input 4306, 
                         input c-seg-usuario).
      return error.
    end.       
    find layout_impres no-lock
         where layout_impres.nom_impressora = cPrinter
         and layout_impres.cod_layout_impres = c-layout no-error.
    if not avail layout_impres then do:
      run utp/ut-msgs.p (input "show":U, 
                         input 4306, 
                         input c-seg-usuario).
      return error.
    end.       
  end.
end.  

/* end of rpexb.i */
 
    
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    /*****************************************************************
**
** RPRUN.I - Roda o programa RP do relat¢rio
** {1} = Nome do programa no formato xxp/xx9999.rp.p
*****************************************************************/

def var i-num-ped-exec-rpw as integer no-undo.
  
raw-transfer tt-param    to raw-param.




/*Altera‡Æo feita por Anderson tech540 em 29/06/2001 para testar se a pagina de 
impressÆo esta definida ou se seu valor ‚ NO*/

if rsExecution:screen-value in frame fPage6 = "2":U then do:
  run btb/btb911zb.p (input c-programa-mg97,
                      input "sfc/essf0004rp.p":U,
                      input c-versao-mg97,
                      input 97,
                      input tt-param.arquivo,
                      input tt-param.destino,
                      input raw-param,
                      input table tt-raw-digita,
                      output i-num-ped-exec-rpw).
  if i-num-ped-exec-rpw <> 0 then                     
    run utp/ut-msgs.p (input "show":U, 
                       input 4169, 
                       input string(i-num-ped-exec-rpw)).                      
end.

else do:                                         
  run sfc/essf0004rp.p (input raw-param,
           input table tt-raw-digita).
end.


/* end of rprun.i */
 
    
    /***************************************************************
**
** RPEXC.I - Saida C na PI-EXECUTAR do template de relat¢rio
**
***************************************************************/ 

/* end of rpexc.i */
 
    
    SESSION:SET-WAIT-STATE("":U).
    
    /**************************************************************************
**
** RPTRM - Realiza o destino de impressÆo Terminal
**
***************************************************************************/

define variable c-key-value as character no-undo.

if tt-param.destino = 3 then do:
    get-key-value section "Datasul_EMS2":U key "Show-Report-Program":U value c-key-value.
    
    if c-key-value = "":U or c-key-value = ?  then do:
        assign c-key-value = "Notepad.exe":U.
        put-key-value section "Datasul_EMS2":U key "Show-Report-Program":U value c-key-value no-error.
    end.
    
    run winexec (input c-key-value + chr(32) + tt-param.arquivo, input 1).
    
end.

/* end of rptrm.i */
 
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */


