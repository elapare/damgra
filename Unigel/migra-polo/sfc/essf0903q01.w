/* Connected Databases 
          polmfg           PROGRESS
*/
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/************************************************************************
**
**  i-prgvrs.i - Programa para criaªío do log de todos os programas 
**               e objetos do EMS 2.0 para objetos
**  {1} = objeto   provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/
def buffer empresa for mgmulti.empresa.
def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[9.99.99.999[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "9.99.99.999"
       c-prg-obj = "Q99XX999".
if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "Q99XX999"
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
    put "Q99XX999" at 1 "9.99.99.999" at 69 today at 84 string(time,'HH:MM:SS':U) at 94 skip.
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
/* alteraá∆o feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */
/*{include/i-wendef.i}*/
/* fim da alateraá∆o */
 
/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
CREATE WIDGET-POOL.
/* ***************************  Definitions  ************************** */
/* Parameters Definitions ---                                           */
/* Local Variable Definitions ---                                       */
/* _UIB-CODE-BLOCK-END */
/* ********************  Preprocessor Definitions  ******************** */
/* Internal Tables (found by Frame, Query & Browse Queries)             */
/* Definitions for QUERY Query-Main                                     */
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
/* _UIB-PREPROCESSOR-BLOCK-END */
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&QUERY-NAME
</KEY-OBJECT>
<FOREIGN-KEYS> 
</FOREIGN-KEYS>
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   
/* _UIB-CODE-BLOCK-END */
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&QUERY-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
************************
* Initialize Filter Attributes */
RUN set-attribute-list IN THIS-PROCEDURE ('
  Filter-Value=':U).
/************************
</FILTER-ATTRIBUTES> */   
/* _UIB-CODE-BLOCK-END */
/* ***********************  Control Definitions  ********************** */
/* Query definitions                                                    */
DEFINE QUERY Query-Main FOR 
     pallet   SCROLLING.
/* ************************  Frame Definitions  *********************** */
/* *********************** Procedure Settings ************************ */
/* Settings for THIS-PROCEDURE
   Type: SmartQuery
   Allow: Basic,Query
   Frames: 1
   Add Fields to: NEITHER
   Other Settings: PERSISTENT-ONLY COMPILE
 */
/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "i:\servicos\especificos\polo\programas\polo\sfc\essf0903q01.w should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.
/* *************************  Create Window  ************************** */
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW q-tables ASSIGN
         HEIGHT             = 1.33
         WIDTH              = 22.
/* END WINDOW DEFINITION */
                                                                        */
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/*:T SmartQuery,uib,50000
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
/* ************************* Included-Libraries *********************** */
/***************************************************************************
**  Programa : UT-GLOB.I
**  Include padr„o para definiÁ„o de variaveis globais.
***************************************************************************/ 
     
     def new global shared var i-ep-codigo-usuario  like empresa.ep-codigo no-undo.
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
     
      /* Vari·veis Padr„o DWB / Datasul HR */
     def new global shared var i-num-ped as integer no-undo.         
     def new global shared var v_cdn_empres_usuar   like empresa.ep-codigo        no-undo.
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
 
/*-------------------------------------------------------------------------
    Library     : query.i  
    Purpose     : Basic ADM methods for query objects
  
    Syntax      : {src/adm/method/query.i}
    Description :
  
    Author(s)   :
    Created     :
    Notes       :
    HISTORY: 
      
--------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
/* Dialog program to run to set runtime attributes - if not defined in master */
/* +++ This is the list of attributes whose values are to be returned
   by get-attribute-list, that is, those whose values are part of the
   definition of the object instance and should be passed to init-object
   by the UIB-generated code in adm-create-objects. */
  DEFINE VARIABLE adm-first-rowid AS ROWID NO-UNDO /* INIT ? */.
  DEFINE VARIABLE adm-last-rowid  AS ROWID NO-UNDO /* INIT ? */.
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
         HEIGHT             = 6.88
         WIDTH              = 66.
/* END WINDOW DEFINITION */
                                                                        */
 
/* ************************* Included-Libraries *********************** */
/*-------------------------------------------------------------------------
    File        : smart.i  
    Purpose     : Provides basic SmartObject functionality.
    Syntax      : {src/adm/method/smart.i}
    Description :
    Author(s)   :
    Created     :
    Notes       :
--------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
def var c-nom-prog-upc-mg97  as char format "x(50)" no-undo.
def var c-nom-prog-appc-mg97 as char format "x(50)" no-undo.
def var c-nom-prog-dpc-mg97  as char format "x(50)" no-undo.
def var c-ctrl-tab           as char                no-undo.
def var h-ctrl-tab           as handle              no-undo.
def var wh-entry-field       as widget-handle       no-undo.
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
         HEIGHT             = 2.93
         WIDTH              = 35.14.
/* END WINDOW DEFINITION */
                                                                        */
/* ************************* Included-Libraries *********************** */
/****************************************************************************
     PROCEDURE: attribut.i
       PURPOSE: holds general-use variable and table definitions
                for ADM Method Libraries
       REMARKS:
    PARAMETERS: NONE
      HISTORY:
*****************************************************************************/
/* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1994-6 - All Rights Reserved. */
/* Make sure not already included */
/* The new Progress widget attribute ADM-DATA is used to store ADM
   attributes and other ADM-specific information. This is new to 8.1, 
   so use PRIVATE-DATA to preserve the ability to compile with 8.0.
   Also there is a new keyword UNLESS-HIDDEN which allows a DISPLAY/ENABLE
   to bypass fields which are hidden. This is used in building alternate
   layouts. */
/* &IF PROVERSION GE "8.1":U &THEN   */
/*   &GLOB    adm-data      ADM-DATA */
/*   &GLOB    unless-hidden          */
/* &ELSE                             */
/* O teste de vers∆o do progress foi retirado pois na vers∆o 10 passaria a causar erros, 
j† que o teste usa string e neste caso 10 Ç menor que 8. Tivemos alguns problemas j† ao testar
a vers∆o beta e foi cadastrado um chamado de Bug - SW */
      
/* &ENDIF */
DEFINE VAR adm-object-hdl       AS HANDLE NO-UNDO. /* current object's handle */
DEFINE VAR adm-query-opened        AS LOGICAL NO-UNDO INIT NO.
DEFINE VAR adm-row-avail-state     AS LOGICAL NO-UNDO INIT ?.
DEFINE VAR adm-initial-lock        AS CHARACTER NO-UNDO INIT "NO-LOCK":U.
DEFINE VAR adm-new-record          AS LOGICAL NO-UNDO INIT no.
DEFINE VAR adm-updating-record     AS LOGICAL NO-UNDO INIT no.
DEFINE VAR adm-check-modified-all  AS LOGICAL NO-UNDO INIT no.
DEFINE NEW GLOBAL SHARED VAR adm-broker-hdl    AS HANDLE  NO-UNDO.
 
 
/* _UIB-CODE-BLOCK-END */
/* ***************************  Main Block  *************************** */
/* The code to assign the object handle (which becomes the ADM-OBJECT-HANDLE
   attribute below) for containers and for other objects has been combined
   here. Note that setting adm-object-hdl later in user code (including the
   main block of a MLI) will have no effect on the value of the attribute.
   To override these default settings (which should be appropriate for 
   virtually all objects) user code must 
     RUN set-attribute-list ('ADM-OBJECT-HANDLE=...').
   For SmartContainers, set the handle to the Frame handle if the
   Container Type is FRAME or DIALOG-BOX, else to WINDOW, unless the
   Container is "virtual" (no visualization), in which case leave it unknown.
   For other objects, set the handle to the default Frame handle if 
   there is one.
*/
  
/* Traduá∆o de Hard-Coded View-as */ 
/* If the broker handle either isn't valid or isn't the right process
   (it's possible the handle has been reused), then start the broker. 
   (But don't let the broker try to start itself!) */
RUN get-attribute IN adm-broker-hdl ('TYPE':U) NO-ERROR.
IF RETURN-VALUE NE "ADM-Broker":U THEN 
DO: 
    RUN adm/objects/broker.p PERSISTENT set adm-broker-hdl. 
    RUN set-broker-owner IN adm-broker-hdl (THIS-PROCEDURE).
END.
/* Initialize all the attributes which all SmartObjects must have. */
THIS-PROCEDURE:ADM-DATA = 
     'ADM1.1~`':U +         /* Version attribute */
     'SmartQuery~`':U +      /* Type attribute */
     '~`':U +       /* Container-Type attribute */
   
     'YES~`':U +                   /* Query-Object attribute */
   
     '~`':U +    /* External-Tables attribute */
     'pallet~`':U +    /* Internal-Tables attribute */
   
     '~`':U +     /* Enabled-Tables attribute */
   
     (IF adm-object-hdl = ? THEN "":U ELSE STRING(adm-object-hdl))
        + "~`":U +    /* Adm-Object-Handle attribute */
   
     'Key-Name,SortBy-Case~`':U +  /* Attribute-List attribute */
   
   
     'Record-Source,Record-Target,Navigation-Target~`':U + /* Supported-Links attribute */
   
     '~`':U +  /* ADM-Dispatch-Qualifier attr */
     '~`~`~`~`~`~`~`~`~`~`~`':U +   /* Placeholders for ADM-Parent, Layout,
                                      Enabled, Hidden, COntainer-Hidden,
                                      Initialized, Fields-Enabled, Current-Page,
                                      ADM-New-Record, UIB-Mode, 
                                      ADM-Deactivate-Links */
    /* PLUS THERE IS AN EXTRA TICK FOR THE DUMMY PREPROC
       which marks the end of the list. Do not disturb. */ 
     IF THIS-PROCEDURE:ADM-DATA = "":U OR THIS-PROCEDURE:ADM-DATA = ? 
         THEN "^^":U             /* plus placeholders for user-defined attrs. */
     /* Or if there are already attributes defined, don't throw them away. */
     ELSE "^":U + ENTRY(2, THIS-PROCEDURE:ADM-DATA, "^":U) + 
          "^":U + ENTRY(3, THIS-PROCEDURE:ADM-DATA, "^":U).
/* An "apply-layout" method is not necessary if there are no layout-cases */
  
/* _UIB-CODE-BLOCK-END */
/* **********************  Internal Procedures  *********************** */
PROCEDURE adm-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Applies "ENTRY" to the first enabled field or other 
               object in the SmartObject.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VAR c_Handle AS CHAR NO-UNDO.
  ASSIGN c_Handle = "".
  RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE, 
                                         INPUT 'TABLEIO-SOURCE':U,
                                         OUTPUT c_Handle ).
  IF c_Handle <> "" THEN                                       
  RUN broker-apply-entry IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE adm-destroy :
/* -----------------------------------------------------------
      Purpose:     Basic routine to delete a procedure and its
                   CONTAINED descendents
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/   
 
 RUN broker-destroy IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE adm-disable :
/* -----------------------------------------------------------
      Purpose:     Disables all enabled objects in the frame.
                   Note that this includes db fields if any.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/   
    /* EPC Before Disable do Container */
    
    
    /* EPC Disable do Container */
    
    RUN set-attribute-list ('ENABLED=no':U).
    RETURN.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE adm-edit-attribute-list :
/* -----------------------------------------------------------
      Purpose:    Runs the dialog to get runtime parameter settings
      Parameters:  <none>
      Notes:       Generally run by the UIB in design mode
    -------------------------------------------------------------*/   
  /* Must be defined in the Object*/
      RUN support/queryd.w (INPUT THIS-PROCEDURE).
  
      RETURN. 
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE adm-enable :
/* -----------------------------------------------------------
      Purpose:    Enable an object - all components except db fields,
                  which are enabled using enable-fields.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/   
   /* EPC Before Enable do Container */
   
   
   /* EPC Enable do Container */
   
   RUN set-attribute-list ('ENABLED=yes':U).
   RETURN.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE adm-exit :
/* -----------------------------------------------------------
      Purpose: Passes an exit request to its container    
      Parameters:  <none>
      Notes:  The convention is that the standard routine always
          passes an exit request to its CONTAINER-SOURCE. The container 
          that is actually able to initiate the exit should define
          a local version and *not* call the standard one.    
          That local-exit is built into the SmartWindow template.
    -------------------------------------------------------------*/   
     RUN notify ('exit':U).
  RETURN.  
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE adm-hide :
/* -----------------------------------------------------------
      Purpose:     Hides an object and sets any active links which
                   are dependent on hide/view off.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/   
  RUN broker-hide IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE adm-initialize :
/* -----------------------------------------------------------
      Purpose:     Enables and Views an object unless its attributes
                   indicate this should not be done.
                   Cascades 'initialize' to descendents.
      Parameters:  <none>
      Notes:       
   -------------------------------------------------------------*/   
   /* alteraªío feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */
   /*{include/i-wenreg.i}*/
   /* fim da alateraá∆o */
   /* EPC Before Initialize do Container */ 
   
   /* EPC Before Initialize do Viewer */ 
   
   /* EPC Before Initialize do Browser */
   
   RUN broker-initialize IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.
   /*   Alteraá∆o para corrigir o problema de algumas viewers n∆o mostrar
        o primeiro registro quando o programa Ç inicializado               
        Data : 20/Fev/97  - J.Carlos (PGS)  -  Egolf e SÇrgio (DATASUL)  */  
   
        
   
                                                               /*
    Nome :  C-PAGE.i       J.Carlos - 21/Fev/97
    Funªío : Guardar a pagina e o container-source da VIEWER.
*/
   def var c_Aux-var as char no-undo.
   RUN get-link-handle IN adm-broker-hdl (INPUT  THIS-PROCEDURE,
                                          INPUT  "CONTAINER-SOURCE":U,
                                          OUTPUT c_Aux-var).
   RUN set-attribute-list ("W-Container-Source = ":U + string(c_Aux-var)).
   RUN What-is-the-Page IN adm-broker-hdl (INPUT THIS-PROCEDURE).
   RUN set-attribute-list ("W-Page = ":U + RETURN-VALUE). 
 
   
   /* EPC - Initialize do Container */ 
   
   /* EPC - Initialize do Viewer */ 
   
   /* EPC - Initialize do Browser */
   
   
   /*Traduá∆o dos campos de tela*/
   
   /*final da traduá∆o dos campos de tela*/
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE adm-show-errors :
/* -----------------------------------------------------------
      Purpose:  Display system error messages on a runtime error.
      Parameters:  <none>
      Notes:    A localization of this method can look at the message
                number to display a custom error or suppress standard
                error display.
    -------------------------------------------------------------*/
    DEFINE VARIABLE        cntr                  AS INTEGER   NO-UNDO.
    DO cntr = 1 TO ERROR-STATUS:NUM-MESSAGES:
        MESSAGE ERROR-STATUS:GET-MESSAGE(cntr).
    END.
    RETURN.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE adm-UIB-mode :
/*--------------------------------------------------------------------------
  Purpose     : Set the objects attributes in "UIB Mode".  This is the
                "mode" it will have in design-mode in the UIB.
  Notes       :
  ------------------------------------------------------------------------*/
  RUN broker-UIB-mode IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE adm-view :
/* -----------------------------------------------------------
      Purpose:     Views an object and sets active links on.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/   
  RUN broker-view IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.
  
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE dispatch :
/* -----------------------------------------------------------
      Purpose:    Determines whether to run the LOCAL or STANDARD (adm-)
                  or no-prefix version of a method in the current procedure.
      Parameters: INPUT base method name (with no prefix),
      Notes:      In addition, if the developer has defined a custom prefix
                  as ADM-DISPATCH-QUALIFIER, then a method with this prefix
                  will be searched for after "local-" and before "adm-".
                  If the preprocessor ADM-SHOW-DISPATCH-ERRORS is defined
                  then the show-errors method will be dispatched if a
                  method name is not found in any form. This can be 
                  useful for debugging purposes.
    -------------------------------------------------------------*/   
    DEFINE INPUT PARAMETER p-method-name    AS CHARACTER NO-UNDO.
    RUN broker-dispatch IN adm-broker-hdl 
        (THIS-PROCEDURE, p-method-name) NO-ERROR.
    IF RETURN-VALUE = "ADM-ERROR":U THEN RETURN "ADM-ERROR":U.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE get-attribute :
/* -----------------------------------------------------------
      Purpose:     Returns the value of a std variable or attribute-table entry.
      Parameters:  INPUT attribute name, RETURN-VALUE (string)
      Notes:       
    -------------------------------------------------------------*/   
  DEFINE INPUT PARAMETER p-attr-name    AS CHARACTER NO-UNDO.
  RUN broker-get-attribute IN adm-broker-hdl
      (INPUT THIS-PROCEDURE, INPUT p-attr-name) NO-ERROR.
  RETURN RETURN-VALUE.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE get-attribute-list :
/* -----------------------------------------------------------
      Purpose:     Returns a list of all settable object attributes.
      Parameters:  OUTPUT comma-separated attribute list
      Notes:       This procedure does not return a list of *all*
                   attributes, but only those which are defined and
                   set by users (e.g., not HIDDEN, ENABLED... ).
                   In Version 8.1., an INPUT parameter has been added
                   to broker-get-attribute-list to allow a caller to
                   specify a particular list of attributes to return.
                   This standard call does not specify a list, so
                   the attributes in the ADM-ATTRIBUTE-LIST attribute
                   are returned.
    -------------------------------------------------------------*/   
  DEFINE OUTPUT PARAMETER p-attr-list AS CHARACTER NO-UNDO.
  RUN broker-get-attribute-list IN adm-broker-hdl
      (INPUT THIS-PROCEDURE, 
       INPUT ?,           /* Use the defined list of attributes to return */
       OUTPUT p-attr-list) NO-ERROR.
  RETURN.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE new-state :
/* -----------------------------------------------------------
   Purpose:     Stub to send state message off to the broker process.
   Parameters:  state name (CHARACTER) - may also contain one or more
                link names to pass state message through, as part of a
                comma-separated list.
   Notes:       
-------------------------------------------------------------*/   
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  RUN broker-new-state IN adm-broker-hdl (THIS-PROCEDURE, p-state) NO-ERROR.
  RETURN.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE notify :
/* -----------------------------------------------------------
   Purpose:     Stub to pass notify command to broker process
   Parameters:  method name (CHARACTER) - may also include one or more
                link types to pass message through as part of commas-separated
                list.
   Notes:       
-------------------------------------------------------------*/   
  DEFINE INPUT PARAMETER p-method AS CHARACTER NO-UNDO.
  RUN broker-notify IN adm-broker-hdl (THIS-PROCEDURE, p-method) NO-ERROR.
  IF RETURN-VALUE = "ADM-ERROR":U THEN 
      RETURN "ADM-ERROR":U.  
  RETURN.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-trad-widgets :
/*------------------------------------------------------------------------------
  Purpose:    Traduá∆o dos hard-coded view-as de atributos 
  Parameters: p-wh-frame - handle do frame
  Notes:       
------------------------------------------------------------------------------*/
  define input param p-wh-frame as widget-handle no-undo.
  define var wh-child     as widget-handle no-undo. 
  define var c-aux        as char          no-undo.
  define var i-aux        as integer       no-undo.  
  define var c-contexto   as char          no-undo init "".
  
  assign p-wh-frame:BGCOLOR = ?
         p-wh-frame:FONT    = 1
         p-wh-frame = p-wh-frame:FIRST-CHILD
         wh-child   = p-wh-frame:FIRST-CHILD.
  
  do  while valid-handle(wh-child):
      
      case wh-child:type:
          when "RADIO-SET" then do:
              if  wh-child:table <> ? then do:
                  assign c-aux = wh-child:radio-buttons.
                  if  wh-child:private-data <> "" 
                  and wh-child:private-data <> ? then 
                      assign c-contexto = wh-child:private-data. 
                  else
                      assign c-contexto = "*".  
                  do  i-aux = 1 to num-entries(wh-child:radio-buttons):
                      if  (i-aux mod 2) <> 0 then do:
                          run utp/ut-liter.p (input replace(entry(i-aux, wh-child:radio-buttons), chr(32), "_"),
                                              input c-contexto,
                                              input "R"). 
                          assign entry(i-aux, c-aux) = return-value.
                      end.
                  end.                                              
                  assign wh-child:radio-buttons = c-aux.
              end.
          end.
          when "BUTTON" then do:
              if  wh-child:label <> ?
              and wh-child:label <> "" then do:
                  run utp/ut-liter.p (input replace(wh-child:label, chr(32), "_"),
                                      input "",
                                      input "C"). 
                  assign wh-child:label = trim(return-value).
              end. 
              if  wh-child:help <> "" 
              and wh-child:help <> ? then do:
                  run utp/ut-liter.p (input replace(wh-child:help, chr(32), "_"),
                                      input "",
                                      input "R"). 
                  assign wh-child:help = return-value
                         wh-child:tooltip = trim(return-value).
              end.         
          end.
      end case.
      assign wh-child = wh-child:next-sibling.
  end.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE set-attribute-list :
/* -----------------------------------------------------------
      Purpose:     Accepts the value of the complete object attribute list
                   and runs procedures to set individual attributes.
      Parameters:  INPUT comma-separated attribute list.
      Notes:       Not all attributes are settable. Those which are a
                   part of an event such as enable/disable (which set
                   ENABLED on/off) or hide/view (which set HIDDEN on/off)
                   can be queried through get-attribute but cannot be set.
    -------------------------------------------------------------*/   
  DEFINE INPUT PARAMETER p-attr-list    AS CHARACTER NO-UNDO.
  RUN broker-set-attribute-list IN adm-broker-hdl
      (INPUT THIS-PROCEDURE, INPUT p-attr-list) NO-ERROR.
  RETURN.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE set-position :
/* -----------------------------------------------------------
  Purpose:     Moves an object to a specified position.
  Parameters:  ROW and COLUMN 
  Notes:       
-------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-row    AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER p-col    AS DECIMAL NO-UNDO.
    IF VALID-HANDLE(adm-object-hdl) THEN
    DO:     
      /* If this is a Window or a Dialog box which is being positioned,
         then the special value 0 means to center the object in that
         dimension (0,0 means center on the screen - 0 can be used to
         signal this because 0 is an invalid row or column position). */
      
      /* Set object's position */
      ASSIGN adm-object-hdl:ROW    =   p-row 
             adm-object-hdl:COLUMN =   p-col.
    END.  
    RETURN.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
 
/*-------------------------------------------------------------------------
    Library     : record.i  
    Purpose     : Base ADM methods for record handling objects
  
    Syntax      : {src/adm/method/record.i}
    Description :
  
    Author(s)   :
    Created     :
    HISTORY: 
--------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
/* Variable Definitions --                                              */
DEFINE VARIABLE tt-uib-reposition-query AS LOGICAL NO-UNDO INITIAL NO.
DEFINE VARIABLE tt-uib-browser-view     AS LOGICAL NO-UNDO INITIAL ?.
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
         HEIGHT             = 1.25
         WIDTH              = 35.86.
                                                                        */
 
/* ************************* Included-Libraries *********************** */
/* _UIB-CODE-BLOCK-END */
/* ***************************  Main Block  *************************** */
/* _UIB-CODE-BLOCK-END */
/* **********************  Internal Procedures  *********************** */
PROCEDURE adm-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Displays the fields in the current record and any other 
               objects in the DISPLAYED-OBJECTS list.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* EPC - Before Display da Viewer */
  
  /****************** Customizaá∆o PGS - Substituiá∆o de :
  &IF DEFINED(adm-browser) NE 0 AND 
    "{&FIELDS-IN-QUERY-{&BROWSE-NAME}}":U NE "":U &THEN
   Para:
     &IF DEFINED(adm-browser) NE 0 AND 
     DEFINED(FIELDS-IN-QUERY-{&BROWSE-NAME}) NE 0 &THEN 
    Em funá∆o de problemas de compilaá∆o com campos calculados
    dentro do BROWSE.
    Ricardo de Lima Perdig∆o - 01/07/1997 
  ************************************************************/ 
    
    
    
    /* Clear MODIFIED field attr. */
    RUN check-modified IN THIS-PROCEDURE ('clear':U) NO-ERROR.  
  /* EPC - Display da Viewer */
  
   
    RETURN.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE adm-open-query :
/* -----------------------------------------------------------
  Purpose:     Opens the default or browse query.
  Parameters:  <none>
  Notes:       If there's a dependency on an external table, and 
               no record from that table is available, the query
               is closed.
-------------------------------------------------------------*/
/* EPC - Before Open Query do Browser */
  
        OPEN QUERY Query-Main FOR EACH  pallet WHERE pallet.int-1 = 98 AND pallet.situacao = 1  NO-LOCK INDEXED-REPOSITION.


         /*  NO-LOCK
                   polo_off_pallet WHERE polo_off_pallet.log_ems NO-LOCK, 
           EACH pallet  WHERE
            pallet.cod-estabel = "422" AND
               pallet.nr-pallet = polo_off_pallet.nr_pallet   AND
               pallet.it-codigo = polo_off_pallet.it_codigo OPEN QUERY Query-Main FOR EACH pallet  WHERE pallet.int-1 = 98 NO-LOCK INDEXED-REPOSITION.*/
        
        ASSIGN adm-query-opened = YES 
               adm-last-rowid = ?.      /* we don't know the last record yet */
        /* Find out if this is the end of an Add or other operation that
           reopens the query and immediately does a reposition-query. If so,
           skip the get-first. */
        RUN get-attribute ('REPOSITION-PENDING':U).
        IF RETURN-VALUE NE "YES":U and
           not tt-uib-reposition-query THEN DO:
            RUN dispatch IN THIS-PROCEDURE ('get-first':U).
        END.
        assign tt-uib-reposition-query = no.
        IF NOT AVAILABLE (pallet) THEN
            RUN new-state ('no-record-available,SELF':U).
        ELSE 
            /* In case there previously was no record in the dataset: */
            RUN new-state ('record-available,SELF':U). 
        
  
/* EPC - Open Query do Browser */
/* L¢gica para controle das vari†veis adm-first-rowid e adm-last-rowid
   respons†veis pela navegaá∆o em DATABASE DIFERENTE DE PROGRESS    */
.
/* Fim da Atualizaá∆o das vari†veis de controle.*/
  RETURN.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE adm-row-changed :
/* -----------------------------------------------------------
      Purpose:    Executed when a new record or set of records
                  is retrieved locally (as opposed to passed on from
                  another procedure). Handles default display or browse open
                  code and then signals to RECORD-TARGETs that 
                  a fresh record or set of joined records is available.. 
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/   
      /* If there's a Frame or other valid container associated
         with this object, display the record's fields. */ 
      IF VALID-HANDLE(adm-object-hdl) THEN 
        RUN dispatch IN THIS-PROCEDURE ('display-fields':U).
      RUN notify ('row-available':U).
          
      RETURN. 
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE reposition-query :
/* -----------------------------------------------------------
  Purpose:     Gets the current rowid from the calling procedure,
               and repositions the current query to that record.
  Parameters:  Caller's procedure handle
  Notes:       
-------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-requestor-hdl     AS HANDLE NO-UNDO.
    DEFINE VAR repos-rowid                    AS ROWID NO-UNDO.
    
    RUN get-rowid IN p-requestor-hdl (OUTPUT repos-rowid).
    IF repos-rowid <> ? THEN
    DO:                                           
        REPOSITION Query-Main TO ROWID repos-rowid NO-ERROR.
        
/************ Customizacao Datasul S.A. - John Cleber Jaraceski - 02/07/1999
              Implementar novas caracteristicas para o panel p-navega.w 
              devido a deficiencia dos Templates PROGRESS com DATABASE DIFERENTE
              DE PROGRESS
              **********************************/
        
/***********************************************/
        
        RUN dispatch IN THIS-PROCEDURE ('get-next':U).
    END.
    
    /* In case this attribute was set earlier, turn it off. */
    RUN set-attribute-list ('REPOSITION-PENDING = NO':U).
    RETURN.    
  
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
 
/* _UIB-CODE-BLOCK-END */
/* ***************************  Main Block  *************************** */
ASSIGN adm-first-rowid = ?
       adm-last-rowid  = ?.   /* INIT doesn't work for ROWIDs */
/* _UIB-CODE-BLOCK-END */
/* **********************  Internal Procedures  *********************** */
PROCEDURE adm-get-first :
/* -----------------------------------------------------------
  Purpose:     Gets the first record in the default query.
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  
  
    IF NOT adm-query-opened THEN RETURN.
    GET FIRST Query-Main.
    IF AVAILABLE pallet THEN
    DO:
        IF NUM-ENTRIES("pallet":U," ":U) = 1 THEN
            ASSIGN adm-first-rowid = 
                ROWID (pallet).
        RUN new-state ('first-record,SELF':U).
    END.
    RUN dispatch IN THIS-PROCEDURE ('row-changed':U) .
    
  
    RETURN.
    
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE adm-get-last :
/* -----------------------------------------------------------
  Purpose:     gets the last record in the default query.
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  
  
    IF NOT adm-query-opened THEN RETURN.
    
    GET LAST Query-Main.
    IF AVAILABLE pallet THEN 
    DO:
        IF NUM-ENTRIES("pallet":U," ":U) = 1 THEN
            ASSIGN adm-last-rowid = 
                ROWID (pallet).
        IF adm-last-rowid NE ? AND adm-first-rowid = adm-last-rowid THEN
            RUN new-state ('only-record,SELF':U).  /* Just one rec in dataset */
        ELSE RUN new-state ('last-record,SELF':U).
    END.
    RUN dispatch IN THIS-PROCEDURE ('row-changed':U).
    
  
    RETURN.
    
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE adm-get-next :
/* -----------------------------------------------------------
  Purpose:     Gets the next record in the default query.
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  
  
    IF NOT adm-query-opened THEN RETURN.
    
    GET NEXT Query-Main.
    
/************ Customizacao Datasul S.A. - John Cleber Jaraceski - 29/10/1999
              Ao utilizar Dataserver Oracle OU SQL SERVER, deve-se verificar se o pr¢ximo registro
              posicionado pela query est† realmente dispon°vel
              **********************************/
    
        IF AVAIL(pallet) THEN
        DO:    
    
/***********************************************/
    
/************ Customizacao Datasul S.A. - John Cleber Jaraceski - 29/10/1999
              Implementar novas caracteristicas para o panel p-navega.w 
              devido a deficiencia dos Templates PROGRESS com DATABASE DIFERENTE DE PROGRESS
              **********************************/
        
/***********************************************/
        
        IF adm-last-rowid = ROWID(pallet)
             THEN RUN new-state ('last-record,SELF':U).
        ELSE RUN new-state ('not-first-or-last,SELF':U).
        
        RUN dispatch IN THIS-PROCEDURE ('row-changed':U).
    END.
    ELSE 
        RUN dispatch IN THIS-PROCEDURE ('get-last':U).  
  
    RETURN.
    
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE adm-get-prev :
/* -----------------------------------------------------------
  Purpose:     Gets the previous record in the default query.
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  
  
    IF NOT adm-query-opened THEN RETURN.
    
    GET PREV Query-Main. 
    IF AVAIL(pallet) THEN
    DO:
        IF adm-first-rowid = ROWID(pallet)
            THEN RUN new-state ('first-record,SELF':U).
        ELSE RUN new-state ('not-first-or-last,SELF':U).
        RUN dispatch IN THIS-PROCEDURE ('row-changed':U).
    END.
    ELSE 
        RUN dispatch IN THIS-PROCEDURE ('get-first':U).
  
    RETURN.
    
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE New-First-Record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER rRowid AS ROWID NO-UNDO.
    ASSIGN adm-first-rowid = rRowid.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
 
/* Procedure Description
"Biblioteca para customizaá∆o das querys"
*/
/*--------------------------------------------------------------------------
    Library     : c-query.i
    Purpose     : Implementar customizaá‰es nas querys
    Syntax      : {include/c-query.i}
    Description : Method-Library criada para fornecer customizaá∆o para
                  as querys a serem utilizadas pelos programas do
                  Magnus97
    Author(s)   : Vanei
    Created     : 10/01/1997
    Notes       : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE VARIABLE h-panel-nav AS HANDLE NO-UNDO.
define variable wh-pesquisa        as handle no-undo.
define variable wh-relacionamento  as handle no-undo.
define variable wh-consulta        as handle no-undo.
define variable v-row-table as rowid no-undo.
define variable wh-programa  as handle no-undo.
define variable c-container  as char   no-undo.
define variable wh-container as handle no-undo.
define variable container    as char   no-undo.
def new global shared var r-registro-atual as rowid     no-undo.
def new global shared var l-implanta       as logical    init no.
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
         HEIGHT             = 2.08
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
 
/* ************************* Included-Libraries *********************** */
/* _UIB-CODE-BLOCK-END */
/* ***************************  Main Block  *************************** */
  RUN modify-list-attribute IN adm-broker-hdl
        (INPUT THIS-PROCEDURE,
         INPUT "ADD":U,
         INPUT "ADM-ATTRIBUTE-LIST":U,
         INPUT "ProgPesquisa":U).
  RUN modify-list-attribute IN adm-broker-hdl
        (INPUT THIS-PROCEDURE,
         INPUT "ADD":U,
         INPUT "ADM-ATTRIBUTE-LIST":U,
         INPUT "ProgVaPara":U).
  RUN modify-list-attribute IN adm-broker-hdl
        (INPUT THIS-PROCEDURE,
         INPUT "ADD":U,
         INPUT "ADM-ATTRIBUTE-LIST":U,
         INPUT "ProgIncMod":U).
  RUN modify-list-attribute IN adm-broker-hdl
        (INPUT THIS-PROCEDURE,
         INPUT "ADD":U,
         INPUT "ADM-ATTRIBUTE-LIST":U,
         INPUT "Implantar":U).
/* _UIB-CODE-BLOCK-END */
/* **********************  Internal Procedures  *********************** */
PROCEDURE IncMod :
/*------------------------------------------------------------------------------
  Purpose:     Chama o programa de IncMod e reposiciona a query
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
    DEFINE VARIABLE nom_program AS CHARACTER NO-UNDO. /* vari†vel para receber o return-value */
    
    RUN get-attribute IN THIS-PROCEDURE (INPUT "ProgIncMod":U).
    
    ASSIGN nom_program = RETURN-VALUE.
    
    IF nom_program <> ? AND nom_program <> "":U THEN DO:
        ASSIGN v-row-table = ROWID(pallet).
        
        RUN VALUE(nom_program) PERSISTENT SET wh-programa
                (INPUT-OUTPUT v-row-table, 
                 INPUT p-state,
                 INPUT THIS-PROCEDURE).
        
        IF NOT VALID-HANDLE(wh-programa) OR 
               wh-programa:TYPE <> "PROCEDURE":U OR 
               wh-programa:FILE-NAME <> nom_program THEN 
           RETURN "ADM-ERROR":U.
        
        RUN dispatch IN wh-programa (INPUT "INITIALIZE":U).
        
        IF NOT VALID-HANDLE(wh-programa) OR 
               wh-programa:TYPE <> "PROCEDURE":U OR 
               wh-programa:FILE-NAME <> nom_program THEN 
           RETURN "ADM-ERROR":U.
        
        IF p-state = "INCLUI":U THEN
            RUN notify IN wh-programa (INPUT "ADD-RECORD":U).
        ELSE IF p-state = "MODIFICA":U THEN DO:
                RUN pi-desabilita-bts IN wh-programa.
                RUN pi-reposiciona IN wh-programa.
                RUN new-state IN wh-programa (INPUT "UPDATE-BEGIN":U).
             END.
             ELSE IF p-state = "COPIA":U THEN DO:
                     RUN pi-desabilita-bts IN wh-programa.
                     RUN pi-reposiciona IN wh-programa.
                     RUN notify IN wh-programa (INPUT "COPY-RECORD":U).
                  END.
        
        RUN pi-entry IN wh-programa.
    END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE Pesquisa :
/*------------------------------------------------------------------------------
  Purpose:     Chama o programa de pesquisa e reposiciona a query
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE nom_program as CHARACTER NO-UNDO. /* vari†vel para receber o return-value */
    
    RUN get-attribute ('Implantar':U).
    
    IF RETURN-VALUE <> ? AND RETURN-VALUE <> "":U
        THEN DO:
            if RETURN-VALUE = "yes":U then assign l-implanta = yes.
            if RETURN-VALUE = "no":U then assign l-implanta = no.
        END.
            
    RUN get-attribute ('ProgPesquisa':U).
    ASSIGN nom_program = RETURN-VALUE.
    
    IF  nom_program <> ?
    AND nom_program <> "":U
    AND (NOT VALID-HANDLE(WH-PESQUISA) OR 
         wh-pesquisa:TYPE <> "PROCEDURE":U OR 
         wh-pesquisa:FILE-NAME <> nom_program) THEN DO:
         
        run value(nom_program) persistent set wh-pesquisa.
        
        if valid-handle(wh-pesquisa) and
           wh-pesquisa:TYPE = "PROCEDURE":U and
           wh-pesquisa:FILE-NAME = nom_program then do:
         
          RUN add-link IN adm-broker-hdl (INPUT wh-pesquisa,
                                          INPUT 'State':U,
                                          INPUT THIS-PROCEDURE).
          
          /* O pre-processador a seguir Ç utilizado pela equipe do Produto HR */
          
          
          RUN dispatch IN wh-pesquisa ('initialize':U).
          if valid-handle(wh-pesquisa) and
             wh-pesquisa:TYPE = "PROCEDURE":U and
             wh-pesquisa:FILE-NAME = nom_program then          
            RUN pi-entry IN wh-pesquisa.
        end.  
    END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-consulta :
/*------------------------------------------------------------------------------
  Purpose:     Chama o programa de pesquisa e reposiciona a query
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var i-inicio    as integer no-undo.
def var i-fim       as integer no-undo.
def var rw-reserva  as rowid   no-undo.
RUN Who-Is-The-Container IN adm-broker-hdl
    (INPUT this-procedure,
     OUTPUT c-container ).
  assign rw-reserva = rowid(pallet).
  IF VALID-HANDLE(WIDGET-HANDLE(c-container)) THEN do:
    assign wh-container = widget-handle(c-container).
    assign i-inicio     = r-index(wh-container:file-name,"/") + 1
           i-fim        = r-index(wh-container:file-name,".w").
    if i-fim < r-index(wh-container:file-name,".r") then
       i-fim = r-index(wh-container:file-name,".r").  
    if i-inicio < r-index(wh-container:file-name,"\") then
       i-inicio = r-index(wh-container:file-name,"\") + 1.
   if i-inicio > 0 and i-fim > 0 then do:
      run utp/ut-cons.w (input substring(wh-container:file-name,i-inicio , i-fim - i-inicio)).
      run pi-reposiciona-query (input rw-reserva).
   end.
end.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-container :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN Who-Is-The-Container IN adm-broker-hdl
    (INPUT this-procedure,
     OUTPUT c-container ).
     
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-desabilita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run pi-container.
run pi-desab IN widget-handle(c-container).  
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-habilita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run pi-container.
run pi-habil IN widget-handle(c-container).  
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-posicao-query :
/*------------------------------------------------------------------------------
  Purpose:     Devolve a posicao do query
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def output parameter rw-posicao-query as rowid no-undo.
    assign rw-posicao-query = rowid(pallet).
    
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-reposiciona-query :
/*------------------------------------------------------------------------------
  Purpose:     Reposicionar a query
  Parameters:  INPUT rowid para reposicionamento.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-row-table AS ROWID NO-UNDO.
    
    SESSION:SET-WAIT-STATE("GENERAL":U).
    /*  N∆o reposiciona a query se a tabela estiver vazia. */
    IF NOT CAN-FIND(FIRST pallet) THEN DO:
        SESSION:SET-WAIT-STATE("":U).
        RETURN .
    END.    
    /* O pre-processador a seguir Ç utilizado pela equipe do Produto HR - 
       Este pre-processador deve ficar imediatamente antes do reposition da query, sob pena
       de causar um erro no produto HR */
    
    
    REPOSITION Query-Main TO ROWID p-row-table NO-ERROR.
    
    IF ERROR-STATUS:ERROR THEN DO:
        
        SESSION:SET-WAIT-STATE("":U).
        
        RUN utp/ut-msgs.p (INPUT "SHOW":U, INPUT 16690, INPUT "":U).
        
        SESSION:SET-WAIT-STATE("GENERAL":U).
        
        IF RETURN-VALUE = "NO":U THEN
        DO:
            /* se a resposta for no, seta o mouse para "normal" e retorna*/
            SESSION:SET-WAIT-STATE("":U).
            RETURN .
        END.
        ELSE
            ASSIGN tt-uib-reposition-query = yes.
        
        RUN dispatch IN THIS-PROCEDURE ("OPEN-QUERY":U).
        
        REPOSITION Query-Main TO ROWID p-row-table NO-ERROR.
    END.
/************ Customizacao Datasul S.A. - John Cleber Jaraceski - 02/07/1999
              Implementar novas caracteristicas para o panel p-navega.w 
              devido a deficiencia dos Templates PROGRESS com 
              DATABASE DIFERENTE DE PROGRESS
              **********************************/
    
/***********************************************/
    
    RUN dispatch IN THIS-PROCEDURE ('get-next':U).
    
    SESSION:SET-WAIT-STATE("":U).
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-trata-state :
/*------------------------------------------------------------------------------
  Purpose:     Trata as mudanáas de estado
  Parameters:  INPUT Handle da procedure pai
               INPUT C¢digo do Estado
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl   AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state        AS CHAR   NO-UNDO.
  
  CASE entry(1, p-state, "|":U):
      WHEN 'Pesquisa':U THEN DO:
          run Pesquisa.
      END.
      WHEN 'VaPara':U THEN DO:
          run VaPara.
      END.
      WHEN 'Inclui':U THEN DO:
          run IncMod (INPUT P-STATE).
      END.
      WHEN 'Modifica':U THEN DO:
          run IncMod (INPUT P-STATE) .
      END.
      WHEN 'Copia':U THEN DO:
          run IncMod (INPUT P-STATE).
      END.
      WHEN 'Consulta':U THEN DO:
          run pi-consulta.
      END.
      WHEN 'Reposiciona':U THEN DO:
          if  num-entries(p-state, "|":U) > 1 then do:
              run pi-reposiciona-query (to-rowid(entry(2, p-state, "|":U))).
          end.
      END.
   
/************ Customizacao Datasul S.A. - John Cleber Jaraceski - 02/07/1999
              Implementar novas caracteristicas para o panel p-navega.w 
              devido a deficiencia dos Templates PROGRESS com DATABASE ORACLE
              E SQL SERVER
              **********************************/
   
/***********************************************/
  END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE VaPara :
/*------------------------------------------------------------------------------
  Purpose:     Chama o programa de VaPara e reposiciona a query
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE nom_program AS CHARACTER NO-UNDO.
    RUN get-attribute ('ProgVaPara':U).
    
    ASSIGN nom_program = RETURN-VALUE.
    
    IF  nom_program <> ?
    AND nom_program <> "":U
    AND (NOT VALID-HANDLE(WH-PESQUISA) OR 
         wh-pesquisa:TYPE <> "PROCEDURE":U OR 
         wh-pesquisa:FILE-NAME <> nom_program) THEN DO:
        
        /* O pre-processador a seguir Ç utilizado pela equipe do Produto HR */
        run value(nom_program) (output v-row-table ).
        if  v-row-table <> ? then do:
            run pi-reposiciona-query (v-row-table).
        end.
    END.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
 
/* _UIB-CODE-BLOCK-END */
/* ***********  Runtime Attributes and AppBuilder Settings  *********** */
/* SETTINGS FOR WINDOW q-tables
  VISIBLE,,RUN-PERSISTENT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
/* Setting information for Queries and Browse Widgets fields            */
/* Query rebuild information for QUERY Query-Main
     _TblList          = "polmfg.pallet"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Design-Parent    is WINDOW q-tables @ ( 1.17 , 9.86 )
*/  /* QUERY Query-Main */
 
/* ***************************  Main Block  *************************** */
  
/* _UIB-CODE-BLOCK-END */
/* **********************  Internal Procedures  *********************** */
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  /* Define variables needed by this internal procedure.             */
  /* row-head.i - */
  DEFINE VARIABLE tbl-list           AS CHARACTER INIT "":U NO-UNDO.
  DEFINE VARIABLE rowid-list         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE row-avail-cntr     AS INTEGER INIT 0 NO-UNDO.
  DEFINE VARIABLE row-avail-rowid    AS ROWID NO-UNDO.
  DEFINE VARIABLE row-avail-enabled  AS LOGICAL NO-UNDO.
  DEFINE VARIABLE link-handle        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE record-source-hdl  AS HANDLE NO-UNDO.
  DEFINE VARIABLE different-row      AS LOGICAL NO-UNDO INIT no.
  DEFINE VARIABLE key-name           AS CHARACTER INIT ? NO-UNDO.
  DEFINE VARIABLE key-value          AS CHARACTER INIT ? NO-UNDO.
 
  /* Check that the previous record hasn't been modifed. */
  RUN check-modified IN THIS-PROCEDURE ('check':U) NO-ERROR.
  
  /* If nothing's been modified but we're in an update, then the record
     we're getting is the same one we're just finishing up with
     (update-complete state after an Add, for instance). So ignore it. */
  IF adm-updating-record THEN RETURN.
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = IF RETURN-VALUE = "YES":U THEN yes ELSE no.  
  
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'RECORD-SOURCE':U,
      OUTPUT link-handle) NO-ERROR.
  IF link-handle = "":U THEN     /* There's no active record source */
      RETURN.
  ASSIGN record-source-hdl = WIDGET-HANDLE(ENTRY(1,link-handle)).
  IF NUM-ENTRIES(link-handle) > 1 THEN  /* A list indicates multiple sources */
      MESSAGE "row-available in ":U THIS-PROCEDURE:FILE-NAME 
          "encountered more than one RECORD-SOURCE.":U SKIP
          "The first - ":U record-source-hdl:file-name " - will be used.":U
             VIEW-AS ALERT-BOX ERROR.
  
  /* Get the key needed by this Record-Target. */         
  RUN get-attribute ('Key-Name':U).
  key-name = RETURN-VALUE.
  IF key-name NE ? THEN DO:
    RUN send-key IN record-source-hdl (INPUT key-name, OUTPUT key-value)
      NO-ERROR.
    IF key-value NE ? THEN  /* At design time this won't succeed, so skip it. */
      RUN set-attribute-list (SUBSTITUTE ('Key-Value="&1"':U, key-value)).
  END.
 
  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  /* row-end.i */
IF VALID-HANDLE (adm-object-hdl) THEN  /* If there's a Frame, etc. then */
    RUN dispatch IN THIS-PROCEDURE ('display-fields':U). /* display the fields*/
  /* If there were external tables, then we check for different rows.
     If there is a KEY-NAME defined then always open the query. */
  IF key-name ne ? OR different-row
  THEN RUN dispatch IN THIS-PROCEDURE ('open-query':U).
  ELSE RUN notify IN THIS-PROCEDURE('row-available':U).
/* Note: open-query does its own notify of row-available */
 
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/
  /* Define variables needed by this internal procedure.               */
  /* snd-head.i - 7/23/95 */
  DEFINE INPUT PARAMETER p-tbl-list AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER p-rowid-list AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE link-handle  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE rowid-string AS CHARACTER NO-UNDO.
  
  DO i = 1 TO NUM-ENTRIES(p-tbl-list):
      IF i > 1 THEN p-rowid-list = p-rowid-list + ",":U.
      CASE ENTRY(i, p-tbl-list):
 
  /* For each requested table, put it's ROWID in the output list.      */
  /* snd-list - 8/21/95 */
    WHEN "pallet":U THEN p-rowid-list = p-rowid-list + 
        IF AVAILABLE pallet THEN STRING(ROWID(pallet))
        ELSE "?":U.
   
 
  /* Deal with any unexpected table requests before closing.           */
  /* snd-end.i */
        OTHERWISE 
        DO:
            RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE,
                INPUT "RECORD-SOURCE":U, OUTPUT link-handle) NO-ERROR.
            IF link-handle NE "":U THEN 
            DO:
                IF NUM-ENTRIES(link-handle) > 1 THEN  
                    MESSAGE "send-records in ":U THIS-PROCEDURE:FILE-NAME 
                            "encountered more than one RECORD-SOURCE.":U SKIP
                            "The first will be used.":U 
                            VIEW-AS ALERT-BOX ERROR.
                RUN send-records IN WIDGET-HANDLE(ENTRY(1,link-handle))
                    (INPUT ENTRY(i, p-tbl-list), OUTPUT rowid-string).
                p-rowid-list = p-rowid-list + rowid-string.
            END.
            ELSE
            DO:
                MESSAGE "Requested table":U ENTRY(i, p-tbl-list) 
                        "does not match tables in send-records":U 
                        "in procedure":U THIS-PROCEDURE:FILE-NAME ".":U SKIP
                        "Check that objects are linked properly and that":U
                        "database qualification is consistent.":U
                    VIEW-AS ALERT-BOX ERROR.     
                RETURN ERROR.
            END.
        END.
        END CASE.        
    END.                 /* i = 1 to num-entries */
 
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      /* qstates.i - query-specific ADM states  */
    WHEN 'update':U THEN RUN new-state ('update':U). 
    WHEN 'update-complete':U THEN DO:
        /* If this state message came from a query object (Query or Browser 
           with its own query) then we do *not* want to do row-changed, 
           because the row that was just updated was in our 
           Record-Target's query, not ours. */
        RUN get-attribute IN p-issuer-hdl ('QUERY-OBJECT':U).
        IF RETURN-VALUE NE "YES":U THEN
            RUN dispatch ('row-changed':U). 
        RUN new-state ('update-complete':U).
    END.
    WHEN 'delete-complete':U THEN RUN dispatch('get-next':U).
    /* Store these states persistently so they can be queried later
       by the Navigation Panel or other objects. */
    WHEN   'first-record':U        OR
      WHEN 'last-record':U         OR
      WHEN 'only-record':U         OR
      WHEN 'not-first-or-last':U   OR
      WHEN 'no-record-available':U OR
      WHEN 'no-external-record-available':U THEN 
        RUN set-attribute-list('Query-Position=':U + p-state).
    
 
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
