/* Connected Databases 
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
def var c-prg-vrs as char init "[[[2.00.00.000[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.000"
       c-prg-obj = "essf0903".
if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "essf0903"
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
    put "essf0903" at 1 "2.00.00.000" at 69 today at 84 string(time,'HH:MM:SS':U) at 94 skip.
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
DEFINE NEW GLOBAL SHARED VARIABLE hWenController AS HANDLE     NO-UNDO.
/*Constante utilizada apenas para verificar se a vari†vel acima foi definida */ 
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
def var p-table as rowid.
def new global shared var h-pai as handle no-undo.
DEF NEW GLOBAL SHARED VAR c-handle-win AS CHAR NO-UNDO.
/** Etiqueta ************************/
define temp-table tt-param2
    field destino          as integer
    field arquivo          as char
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field dir-etq          as char.
  
def temp-table tt-raw-digita
    field raw-digita      as raw.
/** Etiqueta ************************/
def var rw-posic-estab as rowid no-undo.
/* _UIB-CODE-BLOCK-END */
/* ********************  Preprocessor Definitions  ******************** */
/* Name of first Frame and/or Browse and/or first Query                 */
/* Standard List Definitions                                            */
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
/* _UIB-PREPROCESSOR-BLOCK-END */
/* ***********************  Control Definitions  ********************** */
/* Define the widget handle for the window                              */
DEFINE VAR w-cadpaifilho-ambos AS WIDGET-HANDLE NO-UNDO.
/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-arquivo 
       MENU-ITEM mi-primeiro    LABEL "&Primeiro"      ACCELERATOR "CTRL-HOME"
       MENU-ITEM mi-anterior    LABEL "An&terior"      ACCELERATOR "CTRL-CURSOR-LEFT"
       MENU-ITEM mi-proximo     LABEL "Pr&¢ximo"       ACCELERATOR "CTRL-CURSOR-RIGHT"
       MENU-ITEM mi-ultimo      LABEL "&Èltimo"        ACCELERATOR "CTRL-END"
       MENU-ITEM mi-va-para     LABEL "&V† para"       ACCELERATOR "CTRL-T"
       MENU-ITEM mi-pesquisa    LABEL "Pes&quisa"      ACCELERATOR "CTRL-F5"
       RULE
       MENU-ITEM mi-incluir     LABEL "&Incluir"       ACCELERATOR "CTRL-INS"
       MENU-ITEM mi-copiar      LABEL "C&opiar"        ACCELERATOR "CTRL-C"
       MENU-ITEM mi-alterar     LABEL "A&lterar"       ACCELERATOR "CTRL-A"
       MENU-ITEM mi-eliminar    LABEL "&Eliminar"      ACCELERATOR "CTRL-DEL"
       RULE
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".
DEFINE SUB-MENU mi-ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .
DEFINE MENU m-cadastro MENUBAR
       SUB-MENU  mi-arquivo     LABEL "&Arquivo"      
       SUB-MENU  mi-ajuda       LABEL "&Ajuda"        .
/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-cadpai AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_essf0903b01 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_essf0903q01 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_essf0903v01 AS HANDLE NO-UNDO.
/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-desmonta 
     IMAGE-UP FILE "image/im-cance.bmp":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Desmonta o Pallet".
DEFINE BUTTON bt-monta 
     IMAGE-UP FILE "image/im-conta.bmp":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Confirma o Pallet".
DEFINE BUTTON bt-copynao 
     IMAGE-UP FILE "image/ii-copy.bmp":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Copia desativada".
DEFINE BUTTON bt-print 
     IMAGE-UP FILE "image/im-ascii.bmp":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Imprimir Etiqueta do Pallet".
DEFINE BUTTON bt-transfer 
     IMAGE-UP FILE "image/im-copbm.bmp":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Transferir Estabelecimento Pallet".
DEFINE BUTTON bt-integra 
   /*  IMAGE-UP FILE "image/im-copbm.bmp":U*/
     LABEL "Integra" 
     SIZE 9 BY 1.25 TOOLTIP "Transferir Estabelecimento Pallet".
DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 90 BY 1.46
     BGCOLOR 7 .
/* ************************  Frame Definitions  *********************** */
DEFINE FRAME f-cad
     bt-copynao AT ROW 1.17 COL 30.8
     bt-monta AT ROW 1.17 COL 44.86
     bt-desmonta AT ROW 1.17 COL 49
     bt-print AT ROW 1.17 COL 53.14
     bt-transfer AT ROW 1.17 COL 57 HELP
          "Transferir Estabelecimento Pallet"
    bt-integra AT ROW 1.17 COL 62 HELP "Integra paletes reporte off-line"
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.13.
/* *********************** Procedure Settings ************************ */
/* Settings for THIS-PROCEDURE
   Type: w-paiamb
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
/* *************************  Create Window  ************************** */
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-cadpaifilho-ambos ASSIGN
         HIDDEN             = YES
         TITLE              = "Manutená∆o <Insira o complemento>"
         HEIGHT             = 17.13
         WIDTH              = 90
         MAX-HEIGHT         = 21.21
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.21
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE w-cadpaifilho-ambos = CURRENT-WINDOW.
ASSIGN w-cadpaifilho-ambos:MENUBAR    = MENU m-cadastro:HANDLE.
/* END WINDOW DEFINITION                                                */
/* ************************* Included-Libraries *********************** */
/*-------------------------------------------------------------------------
    Library     : containr.i  
    Purpose     : Default Main Block code and Method Procedures
                  for UIB-generated ADM Container procedures.
    Syntax      : {src/adm/method/containr.i}
    Description :
    Author(s)   :
    Created     :
    HISTORY:
-------------------------------------------------------------------------*/
/***********************  DEFINITIONS  ***********************************/
/* System Variable Definitions ---                                       */
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
** Criaá∆o : John Cleber Jaraceski
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
def new global shared var l-peso-bal as logical .
/* include/i-sysvar.i ---                                                     */
 
/* Local Variable Definitions ---                                        */
DEFINE VARIABLE i-ctrl-tab-page   AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-ctrl-tab-folder AS INTEGER   NO-UNDO.
DEFINE VARIABLE c-state-folder    AS CHARACTER NO-UNDO.
/* Dialog program to run to set runtime attributes - if not defined in master */
/* +++ This is the list of attributes whose values are to be returned
   by get-attribute-list, that is, those whose values are part of the
   definition of the object instance and should be passed to init-object
   by the UIB-generated code in adm-create-objects. */
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
         HEIGHT             = 1.5
         WIDTH              = 38.43.
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
  
    ASSIGN adm-object-hdl    =   w-cadpaifilho-ambos.
  
/* Traduá∆o de Hard-Coded View-as */ 
    
        run pi-trad-widgets (input frame f-cad:handle).
    
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
     'w-paiamb~`':U +      /* Type attribute */
     'WINDOW~`':U +       /* Container-Type attribute */
   
     'NO ~`':U +
   
     '~`':U +    /* External-Tables attribute */
     '~`':U +    /* Internal-Tables attribute */
   
     '~`':U +     /* Enabled-Tables attribute */
   
     (IF adm-object-hdl = ? THEN "":U ELSE STRING(adm-object-hdl))
        + "~`":U +    /* Adm-Object-Handle attribute */
   
     'Layout,Hide-on-Init~`':U +  /* Attribute-List attribute */
   
   
     '~`':U + /* Supported-Links attribute */
   
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
 
        /***************************************************************
**
** I-EPC100.I - EPC para Evento DESTROY de Container
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:
    run value(c-nom-prog-dpc-mg97) (input "DESTROY":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-cad:handle,
                                    input "",
                                    input ?).    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    
    run value(c-nom-prog-appc-mg97) (input "DESTROY":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame f-cad:handle,
                                     input "",
                                     input ?).    
end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:
    run value(c-nom-prog-upc-mg97) (input "DESTROY":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-cad:handle,
                                    input "",
                                    input ?).    
end.                                       
/* I-EPC100 */
 
 
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
    
           /***************************************************************
**
** I-EPC009.I - EPC para Evento Before DISABLE de Container
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:
    run value(c-nom-prog-dpc-mg97) (input "BEFORE-DISABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-cad:handle,
                                    input "",
                                    input ?).    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    
    run value(c-nom-prog-appc-mg97) (input "BEFORE-DISABLE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame f-cad:handle,
                                     input "",
                                     input ?).    
end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:
    run value(c-nom-prog-upc-mg97) (input "BEFORE-DISABLE", 
                                    input "CONTAINER",
                                    input this-procedure,
                                    input frame f-cad:handle,
                                    input "",
                                    input ?).    
end.                                       
/* I-EPC009.I */
 
    
    
    DISABLE bt-copynao bt-monta bt-desmonta bt-print bt-transfer  bt-integra rt-button WITH FRAME f-cad.
    RUN dispatch ('disable-fields':U).  
    
    /* EPC Disable do Container */
    
           /***************************************************************
**
** I-EPC009.I - EPC para Evento DISABLE de Container
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:
    run value(c-nom-prog-dpc-mg97) (input "DISABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-cad:handle,
                                    input "",
                                    input ?).    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    
    run value(c-nom-prog-appc-mg97) (input "DISABLE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame f-cad:handle,
                                     input "",
                                     input ?).    
end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:
    run value(c-nom-prog-upc-mg97) (input "DISABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-cad:handle,
                                    input "",
                                    input ?).    
end.                                       
/* I-EPC009.I */
 
    
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
      RUN adm/support/contnrd.w (INPUT THIS-PROCEDURE).
  
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
   
          /***************************************************************
**
** I-EPC008.I - EPC para Evento Before ENABLE de Container
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:
    run value(c-nom-prog-dpc-mg97) (input "BEFORE-ENABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-cad:handle,
                                    input "",
                                    input ?).    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    
    run value(c-nom-prog-appc-mg97) (input "BEFORE-ENABLE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame f-cad:handle,
                                     input "",
                                     input ?).    
end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:
    run value(c-nom-prog-upc-mg97) (input "BEFORE-ENABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-cad:handle,
                                    input "",
                                    input ?).    
end.                                       
/* I-EPC008 */
 
   
   
    ENABLE UNLESS-HIDDEN bt-copynao bt-monta bt-desmonta bt-print bt-transfer  bt-integra rt-button WITH FRAME f-cad.
    /* We also run enable_UI from here. */ 
    RUN enable_UI IN THIS-PROCEDURE NO-ERROR.
   
   /* EPC Enable do Container */
   
          /***************************************************************
**
** I-EPC008.I - EPC para Evento ENABLE de Container
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:
    run value(c-nom-prog-dpc-mg97) (input "ENABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-cad:handle,
                                    input "",
                                    input ?).    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    
    run value(c-nom-prog-appc-mg97) (input "ENABLE":U, 
                                     input "CONTAINER",
                                     input this-procedure,
                                     input frame f-cad:handle,
                                     input "",
                                     input ?).    
end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:
    run value(c-nom-prog-upc-mg97) (input "ENABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-cad:handle,
                                    input "",
                                    input ?).    
end.                                       
/* I-EPC008 */
 
   
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
   
   /* est† verificaá∆o se faz necess†ria devido aos programas */
      
    /* Tenta identificar pelo ADM-CONTAINER */
    
    
        
                    
        
    
    /* Se ainda nío identificou se ≤ window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
    /* Se ainda nío identificou se ≤ window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
    
    /*
    &IF DEFINED(WenIsWindow) &THEN
        RUN registerWindow IN hWenController (THIS-PROCEDURE, CURRENT-WINDOW).
    &ELSEIF DEFINED(WenIsDialog) &THEN
        RUN registerWindow IN hWenController (THIS-PROCEDURE, CURRENT-WINDOW).
    &ENDIF
    */
    /* Se tem janela */
    
        ON 'U9':U ANYWHERE
        DO:
            IF VALID-HANDLE(hWenController) THEN DO:
                RUN registerWindow IN hWenController (THIS-PROCEDURE, SELF).
            END.
        END.
    
    /* Se tem dialog */
    
            /* criados pelos DataViewer n∆o utilizarem a include i-prgvrs */ 
   /* e dessa forma n∆o chamarem a include i-wendef.i que define essa veri†vel. */
   
   /* fim da alateraá∆o */
   /* EPC Before Initialize do Container */ 
   
          /***************************************************************
**
** I-EPC007.I - EPC para Evento Before INITIALIZE de Container
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:
    run value(c-nom-prog-dpc-mg97) (input "BEFORE-INITIALIZE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-cad:handle,
                                    input "",
                                    input ?).    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    
    run value(c-nom-prog-appc-mg97) (input "BEFORE-INITIALIZE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame f-cad:handle,
                                     input "",
                                     input ?).    
end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:
    run value(c-nom-prog-upc-mg97) (input "BEFORE-INITIALIZE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-cad:handle,
                                    input "",
                                    input ?).    
end.                                       
/* I-EPC007.I */
 
   
   /* EPC Before Initialize do Viewer */ 
   
   /* EPC Before Initialize do Browser */
   
   RUN broker-initialize IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.
   /*   Alteraá∆o para corrigir o problema de algumas viewers n∆o mostrar
        o primeiro registro quando o programa Ç inicializado               
        Data : 20/Fev/97  - J.Carlos (PGS)  -  Egolf e SÇrgio (DATASUL)  */  
   
        
             IF  frame f-cad:scrollable THEN
                 ASSIGN frame f-cad:virtual-width-chars  = frame f-cad:width-chars
                        frame f-cad:virtual-height-chars = frame f-cad:height-chars.
        
   
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
 
   
        run get-link-handle in adm-broker-hdl
             (input this-procedure,
              input 'page':U,
              output c-ctrl-tab).
        assign h-ctrl-tab = if c-ctrl-tab <> "" then widget-handle(c-ctrl-tab) else ?.
   
   /* EPC - Initialize do Container */ 
   
          /***************************************************************
**
** I-EPC007.I - EPC para Evento INITIALIZE de Container
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:
    run value(c-nom-prog-dpc-mg97) (input "INITIALIZE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-cad:handle,
                                    input "",
                                    input ?).    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    
    run value(c-nom-prog-appc-mg97) (input "INITIALIZE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame f-cad:handle,
                                     input "",
                                     input ?).    
end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:
    run value(c-nom-prog-upc-mg97) (input "INITIALIZE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame f-cad:handle,
                                    input "",
                                    input ?).    
end.                                       
/* I-EPC007.I */
 
   
   /* EPC - Initialize do Viewer */ 
   
   /* EPC - Initialize do Browser */
   
   
       RUN get-attribute IN THIS-PROCEDURE ("ApplyFillIn":U).
       IF ENTRY(1, RETURN-VALUE, "|":U) = "YES":U THEN
          RUN ApplyFillIn IN WIDGET-HANDLE(ENTRY(2, RETURN-VALUE, "|":U)).
   
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
      
        DEFINE VARIABLE parent-hdl AS HANDLE NO-UNDO.
        IF adm-object-hdl:TYPE = "WINDOW":U THEN
        DO:
          IF p-row = 0 THEN p-row = 
            (SESSION:HEIGHT-CHARS - adm-object-hdl:HEIGHT-CHARS) / 2.
          IF p-col = 0 THEN p-col = 
            (SESSION:WIDTH-CHARS - adm-object-hdl:WIDTH-CHARS) / 2.
        END.
        /* A Dialog naturally centers on its parent and positions relative
           to its parent, so we must adjust for that. */
        ELSE IF adm-object-hdl:TYPE = "DIALOG-BOX":U THEN
        DO:
          parent-hdl = adm-object-hdl:PARENT.
          IF p-row = 0 THEN p-row = 
            ((SESSION:HEIGHT-CHARS - adm-object-hdl:HEIGHT-CHARS) / 2) -
              parent-hdl:ROW.
          IF p-col = 0 THEN p-col = 
            ((SESSION:WIDTH-CHARS - adm-object-hdl:WIDTH-CHARS) / 2) -
              parent-hdl:COL.
        END.
        /* If the row or column wound up being between 0 and 1 after the 
           calculation, change it, because otherwise Progress will complain. */
        IF p-row GE 0 AND p-row < 1 THEN p-row = 1.
        IF p-col GE 0 AND p-col < 1 THEN p-col = 1.
      
      /* Set object's position */
      ASSIGN adm-object-hdl:ROW    =   p-row 
             adm-object-hdl:COLUMN =   p-col.
    END.  
    RETURN.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
 
/* _UIB-CODE-BLOCK-END */
/* ***************************  Main Block  *************************** */
/* Initialize page number and object handle attributes. */
RUN set-attribute-list ("CURRENT-PAGE=0,ADM-OBJECT-HANDLE=":U +
    STRING(adm-object-hdl)). 
/* Best default for GUI applications - this will apply to the whole session: */
PAUSE 0 BEFORE-HIDE.
on  CTRL-TAB anywhere do:
    if valid-handle(focus) then
      apply "leave" to focus.
    assign c-state-folder = "no".
    if  valid-handle(h-ctrl-tab) then do:
        run get-attribute in h-ctrl-tab('folder-labels':U).
        assign i-ctrl-tab-folder = NUM-ENTRIES(return-value,'|':U).
        run get-attribute('current-page':U).
        assign i-ctrl-tab-page = int(return-value) + 1.
        do while c-state-folder = "no":
            run pi-state-folder in h-ctrl-tab (input i-ctrl-tab-page).
            assign c-state-folder = RETURN-VALUE.
            if c-state-folder = "no" 
            then assign i-ctrl-tab-page = i-ctrl-tab-page + 1.
            else run select-page(i-ctrl-tab-page).
            if i-ctrl-tab-page > i-ctrl-tab-folder 
            then do: 
                assign i-ctrl-tab-page = 1.
                leave.
            end.
        end.
        do while c-state-folder = "no":
            run pi-state-folder in h-ctrl-tab (input i-ctrl-tab-page).
            assign c-state-folder = RETURN-VALUE.
            if c-state-folder = "no" 
            then assign i-ctrl-tab-page = i-ctrl-tab-page + 1.
            else run select-page(i-ctrl-tab-page).
            if i-ctrl-tab-page > i-ctrl-tab-folder 
            then do: 
                assign i-ctrl-tab-page = 1.
                leave.
            end.
        end.
    end.
end.
on  SHIFT-CTRL-TAB anywhere do:
    if valid-handle(focus) then
      apply "leave" to focus.
    assign c-state-folder = "no".
    if  valid-handle(h-ctrl-tab) then do:
        run get-attribute in h-ctrl-tab('folder-labels':U).
        assign i-ctrl-tab-folder = NUM-ENTRIES(return-value,'|':U).
        run get-attribute('current-page':U).
        assign i-ctrl-tab-page = int(return-value) - 1.
        do while c-state-folder = "no":
            run pi-state-folder in h-ctrl-tab (input i-ctrl-tab-page).
            assign c-state-folder = RETURN-VALUE.
            if c-state-folder = "no" 
            then assign i-ctrl-tab-page = i-ctrl-tab-page - 1.
            else run select-page (i-ctrl-tab-page).
            if i-ctrl-tab-page < 1
            then do: 
                assign i-ctrl-tab-page = i-ctrl-tab-folder.
                leave.
            end.
        end.
        do while c-state-folder = "no":
            run pi-state-folder in h-ctrl-tab (input i-ctrl-tab-page).
            assign c-state-folder = RETURN-VALUE.
            if c-state-folder = "no" 
            then assign i-ctrl-tab-page = i-ctrl-tab-page - 1.
            else run select-page(i-ctrl-tab-page).
            if i-ctrl-tab-page < 1
            then do: 
                assign i-ctrl-tab-page = i-ctrl-tab-folder.
                leave.
            end.
        end.
    end.
end.
/* _UIB-CODE-BLOCK-END */
/* **********************  Internal Procedures  *********************** */
PROCEDURE adm-change-page :
/* -----------------------------------------------------------
      Purpose:    Views objects on a newly selected page, initializing
                  them if the page has not yet been seen.
      Parameters: <none>
      Notes:      In character mode, when switching from the main window
                  to a page which is another window (in GUI), the
                  main window's default frame must be hidden; and when
                  returning it must be viewed. This is done below.
-------------------------------------------------------------*/   
  /* EPC - Change Page de Container */ 
  
        /***************************************************************
**
** I-EPC014.I - EPC para Evento CHANGE-PAGE de Container 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
    
        run get-attribute('current-page':U). 
         /* DPC */
        if  c-nom-prog-dpc-mg97 <> "" and
            c-nom-prog-dpc-mg97 <> ? then do:                  
            run value(c-nom-prog-dpc-mg97) (input "CHANGE-PAGE":U, 
                                            input "CONTAINER":U,
                                            input h-ctrl-tab,
                                            input frame f-cad:handle  ,
                                            input "",
            
                                            input ?).
            
        end.
        
         /* APPC */
        if  c-nom-prog-appc-mg97 <> "" and
            c-nom-prog-appc-mg97 <> ? then do:           
            run value(c-nom-prog-appc-mg97)  (input "CHANGE-PAGE":U, 
                                             input "CONTAINER":U,
                                             input h-ctrl-tab,
                                             input frame f-cad:handle  ,
                                             input "",
            
                                             input ?).
            
        end.
         /* UPC */
        if  c-nom-prog-upc-mg97 <> "" and
            c-nom-prog-upc-mg97 <> ? then do:                  
            run value(c-nom-prog-upc-mg97) (input "CHANGE-PAGE":U, 
                                            input "CONTAINER":U,
                                            input h-ctrl-tab,
                                            input  frame f-cad:handle,
                                            input "",
            
                                            input ?).
            
        end. 
    
/* I-EPC014.I */
 
  
  RUN broker-change-page IN adm-broker-hdl (INPUT THIS-PROCEDURE) NO-ERROR.
  /* EPC - After Change Page de Container */ 
  
        /***************************************************************
**
** I-EPC014.I - EPC para Evento After CHANGE-PAGE de Container 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
    
        run get-attribute('current-page':U). 
         /* DPC */
        if  c-nom-prog-dpc-mg97 <> "" and
            c-nom-prog-dpc-mg97 <> ? then do:                  
            run value(c-nom-prog-dpc-mg97) (input "AFTER-CHANGE-PAGE":U, 
                                            input "CONTAINER":U,
                                            input h-ctrl-tab,
                                            input frame f-cad:handle  ,
                                            input "",
            
                                            input ?).
            
        end.
        
         /* APPC */
        if  c-nom-prog-appc-mg97 <> "" and
            c-nom-prog-appc-mg97 <> ? then do:           
            run value(c-nom-prog-appc-mg97)  (input "AFTER-CHANGE-PAGE":U, 
                                             input "CONTAINER":U,
                                             input h-ctrl-tab,
                                             input frame f-cad:handle  ,
                                             input "",
            
                                             input ?).
            
        end.
         /* UPC */
        if  c-nom-prog-upc-mg97 <> "" and
            c-nom-prog-upc-mg97 <> ? then do:                  
            run value(c-nom-prog-upc-mg97) (input "AFTER-CHANGE-PAGE":U, 
                                            input "CONTAINER":U,
                                            input h-ctrl-tab,
                                            input  frame f-cad:handle,
                                            input "",
            
                                            input ?).
            
        end. 
    
/* I-EPC014.I */
 
  
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE delete-page :
/* -----------------------------------------------------------
      Purpose:     Destroys all objects on the current page.
      Parameters:  INPUT page number
      Notes:       
    -------------------------------------------------------------*/   
  DEFINE INPUT PARAMETER p-page# AS INTEGER NO-UNDO.
  RUN broker-delete-page IN adm-broker-hdl 
      (INPUT THIS-PROCEDURE, INPUT p-page#).
  END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE init-object :
/* -----------------------------------------------------------
   Purpose:     RUNS an object procedure PERSISTENT and initializes
                default links
   Parameters:  INPUT procedure name, parent handle, attribute-list,
                OUTPUT procedure handle
   Notes:       init-object calls are generated by the UIB 
                in adm-create-objects
-------------------------------------------------------------*/   
  DEFINE INPUT PARAMETER  p-proc-name   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER  p-parent-hdl  AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER  p-attr-list   AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER p-proc-hdl    AS HANDLE    NO-UNDO.
  RUN broker-init-object IN adm-broker-hdl
      (INPUT THIS-PROCEDURE, INPUT p-proc-name, INPUT p-parent-hdl,
       INPUT p-attr-list, OUTPUT p-proc-hdl) NO-ERROR.
  RETURN.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE init-pages :
/* -----------------------------------------------------------
      Purpose:     Initializes one or more pages in a paging
                   control without actually viewing them. 
                   This can be used either for initializing pages
                   at startup without waiting for them to be
                   selected, or for creating additional or
                   replacement pages after startup.
      Parameters:  INPUT comma-separated list of page numbers
      Notes:       Generally this method does not need to be used,
                   unless the user specifically wants to incur the
                   overhead of creating and initializing pages before
                   they are first viewed. When one page in a multi-page
                   SmartContainer has a SmartLink dependency on another
                   page, the UIB will automatically generate the calls
                   to init-pages to assure that the right other pages have been
                   initialized when a page is selected for the first time.
    -------------------------------------------------------------*/   
  DEFINE INPUT PARAMETER p-page-list      AS CHARACTER NO-UNDO.  
  RUN broker-init-pages IN adm-broker-hdl (INPUT THIS-PROCEDURE,
      INPUT p-page-list) NO-ERROR.
  END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-retorna-appc :
/*------------------------------------------------------------------------------
  Purpose:  Retorna o nome do programa APPC   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  return c-nom-prog-appc-mg97.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-retorna-dpc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  return c-nom-prog-dpc-mg97.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-retorna-upc :
/*------------------------------------------------------------------------------
  Purpose:  Retonra o nome do programa UPC    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  return c-nom-prog-upc-mg97.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-retorna-vars-hlp :
/*------------------------------------------------------------------------------
  Purpose:   Retorna variaveis de acesso ao Help
  Parameters: p-num-topico-hlp - numero do topico do programa
              p-nom-manual-hlp - nome do arquivo hlp do modulo do programa
  Notes:       
------------------------------------------------------------------------------*/
define output parameter p-num-topico-hlp as integer no-undo.
define output parameter p-nom-manual-hlp as char format "x(06)" no-undo.
assign p-num-topico-hlp = i-num-topico-hlp-mg97
       p-nom-manual-hlp = c-nom-manual-hlp-mg97.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE select-page :
/* -----------------------------------------------------------
      Purpose:     Makes a new set of objects associated with a page number
                   current, by hiding the previous page, if any,
                   creating the objects in the new page if the page hasn't
                   been initialized, and viewing the new page.
      Parameters:  INPUT new page number
      Notes:       
    -------------------------------------------------------------*/   
  DEFINE INPUT PARAMETER p-page#     AS INTEGER   NO-UNDO.
  RUN broker-select-page IN adm-broker-hdl (INPUT THIS-PROCEDURE,
      INPUT p-page#) NO-ERROR.
  END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE view-page :
/* -----------------------------------------------------------
      Purpose:     Makes a new set of objects associated with a page number
                   current, without hiding the previous page, if any,
                   creating the objects in the new page if the page hasn't
                   been initialized, and viewing the new page.
      Parameters:  INPUT new page number
      Notes:       This method does not reset the value of adm-current-page,
                   because the new page is being viewed without hiding the
                   old one. adm-current-page is the most recently "selected"
                   page.
    -------------------------------------------------------------*/   
  DEFINE INPUT PARAMETER p-page#      AS INTEGER   NO-UNDO.
  RUN broker-view-page IN adm-broker-hdl (INPUT THIS-PROCEDURE,
      INPUT p-page#).
  END PROCEDURE.
/* This ENDIF statement needs to stay here (or with the last procedure in the
   include file) to balance the &IF adm-container at the top: */
/* _UIB-CODE-BLOCK-END */
 
/* Procedure Description
"Library para window cadastro pai-filho com atualizaá∆o dos filhos e do pai"
*/
/*--------------------------------------------------------------------------
    Library     : w-paiamb.i
    Purpose     : Permitir customizaá∆o para as window de cadastro pai-filho
                  com atualizaá∆o dos filhos e do pai
    Syntax      : {include/w-paiamb.i}
    Description : Library utilizada para customizaá∆o da window de cadastro
                  pai-filho com atualizaá∆o dos filhos e do pai
    Author(s)   : Vanei
    Created     : 14/01/1997
    Notes       :
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
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
/* _UIB-CODE-BLOCK-END */
/* ***************************  Main Block  *************************** */
run pi-trad-menu (input w-cadpaifilho-ambos:menubar).
/* _UIB-CODE-BLOCK-END */
/* **********************  Internal Procedures  *********************** */
PROCEDURE pi-after-initialize :
/*------------------------------------------------------------------------------
  Purpose:     C¢digo a ser executado ap¢s a inicializaá∆o
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-before-initialize :
/*------------------------------------------------------------------------------
  Purpose:     C¢digo a ser executado antes da inicializaá∆o
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-desab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
assign w-cadpaifilho-ambos:sensitive = no.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-disable-menu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var p-button-enable as char no-undo.
  
  RUN get-button-enable IN h_p-cadpai (OUTPUT p-button-enable).
   
  assign menu-item mi-incluir:sensitive  in menu m-cadastro = (entry(1,p-button-enable)= string(yes))
         menu-item mi-copiar:sensitive   in menu m-cadastro = NO /*(entry(2,p-button-enable)= string(yes))*/
         menu-item mi-alterar:sensitive  in menu m-cadastro = (entry(3,p-button-enable)= string(yes))
         menu-item mi-eliminar:sensitive in menu m-cadastro = (entry(4,p-button-enable)= string(yes)).
         
  RUN get-button-enable IN h_p-navega (OUTPUT p-button-enable).
  assign menu-item mi-primeiro:sensitive in menu m-cadastro = (entry(1,p-button-enable)= string(yes))
         menu-item mi-anterior:sensitive in menu m-cadastro = (entry(2,p-button-enable)= string(yes))
         menu-item mi-proximo:sensitive in menu m-cadastro = (entry(3,p-button-enable)= string(yes))
         menu-item mi-ultimo:sensitive in menu m-cadastro = (entry(4,p-button-enable)= string(yes))
         menu-item mi-va-para:sensitive in menu m-cadastro = (entry(5,p-button-enable)= string(yes))
         menu-item mi-pesquisa:sensitive in menu m-cadastro = (entry(6,p-button-enable)= string(yes)).
  
  RUN get-button-enable IN h_p-exihel (OUTPUT p-button-enable).
  assign menu-item mi-consultas:sensitive in menu m-cadastro = (entry(1,p-button-enable)= string(yes))
         menu-item mi-imprimir:sensitive in menu m-cadastro = (entry(2,p-button-enable)= string(yes))
         menu-item mi-sair:sensitive in menu m-cadastro = (entry(3,p-button-enable)= string(yes))
         menu-item mi-conteudo:sensitive in menu m-cadastro = (entry(4,p-button-enable)= string(yes)).
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-habil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign w-cadpaifilho-ambos:sensitive = yes.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-trad-menu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/****************************************************************
**
**  I-TRDMN.I - Traduá∆o dos Menus das Janelas
**              Conte£do da pi-trad-menu nas Method Library
**  20/03/1997 - Gilsinei
**  01/07/1998 - John C. Jaraceski
****************************************************************/
define input param p-wh-menu as widget-handle no-undo.
define var wh-menu-child      as widget-handle no-undo.
define var wh-menu-grandchild as widget-handle no-undo.
assign p-wh-menu = p-wh-menu:FIRST-CHILD.
do while valid-handle(p-wh-menu):
    if p-wh-menu:LABEL <> ? then do:
        if p-wh-menu:LABEL = "A&juda" or 
           p-wh-menu:LABEL = "&Ajuda" then
            run utp/ut-liter.p (input "Aj&uda", input "*", input "R").
        else
            run utp/ut-liter.p (input replace(p-wh-menu:LABEL, chr(32), "_"),
                                input "*",
                                input "R").
        
        assign p-wh-menu:LABEL = trim(RETURN-VALUE).
    end.
    
    if can-query(p-wh-menu, "FIRST-CHILD") then do:
        assign wh-menu-child = p-wh-menu:FIRST-CHILD.
        
        do while valid-handle(wh-menu-child):
            if  wh-menu-child:LABEL <> ? then do:
                if wh-menu-child:LABEL = "A&juda" or 
                   wh-menu-child:LABEL = "&Ajuda" then
                    run utp/ut-liter.p (input "Aj&uda", input "*", input "R").
                else
                    run utp/ut-liter.p (input replace(wh-menu-child:LABEL, chr(32), "_"),
                                        input "*",
                                        input "R").
                
                assign wh-menu-child:LABEL = trim(RETURN-VALUE).
            end.
            
            if can-query(wh-menu-child, "FIRST-CHILD") then do:
                assign wh-menu-grandchild = wh-menu-child:FIRST-CHILD.
                
                do while valid-handle(wh-menu-grandchild):
                    if wh-menu-grandchild:LABEL <> ? then do:
                        if wh-menu-grandchild:LABEL = "A&juda" or
                           wh-menu-grandchild:LABEL = "&Ajuda" then
                            run utp/ut-liter.p (input "Aj&uda", input "*", input "R").
                        else
                            run utp/ut-liter.p (input replace(wh-menu-grandchild:LABEL, chr(32), "_"),
                                                input "*",
                                                input "R").
                        
                        assign wh-menu-grandchild:LABEL = trim(RETURN-VALUE).
                    end.
                    
                    assign wh-menu-grandchild = wh-menu-grandchild:NEXT-SIBLING.
                end.
            end.
            
            assign wh-menu-child = wh-menu-child:NEXT-SIBLING.
        end.
    end.
    
    assign p-wh-menu = p-wh-menu:NEXT-SIBLING.
end.
/* I-TRDMN.I */
 
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-trata-state :
/*------------------------------------------------------------------------------
  Purpose:     Trata as mudanáas de estado (State-Changed)
  Parameters:  INPUT Handle da procedure pai
               INPUT C¢digo do Estado
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl   AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state        AS CHAR   NO-UNDO.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
 
/***************************************************************************
**  Programa : UT-GLOB.I
**  Include padr„o para definiÁ„o de variaveis globais.
***************************************************************************/ 
/* alteraªío feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */
/* est† verificaá∆o se faz necess†ria devido aos programas */
   
    /* Tenta identificar pelo ADM-CONTAINER */
    
    
        
                    
        
    
    /* Se ainda nío identificou se ≤ window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
    /* Se ainda nío identificou se ≤ window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
    
    /*
    &IF DEFINED(WenIsWindow) &THEN
        RUN registerWindow IN hWenController (THIS-PROCEDURE, CURRENT-WINDOW).
    &ELSEIF DEFINED(WenIsDialog) &THEN
        RUN registerWindow IN hWenController (THIS-PROCEDURE, CURRENT-WINDOW).
    &ENDIF
    */
    /* Se tem janela */
    
        ON 'U9':U ANYWHERE
        DO:
            IF VALID-HANDLE(hWenController) THEN DO:
                RUN registerWindow IN hWenController (THIS-PROCEDURE, SELF).
            END.
        END.
    
    /* Se tem dialog */
    
            /* criados pelos DataViewer n∆o utilizarem a include i-prgvrs */ 
/* e dessa forma n∆o chamarem a include i-wendef.i que define essa veri†vel. */
/* fim da alateraá∆o */
     
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
 
/* _UIB-CODE-BLOCK-END */
/* ***********  Runtime Attributes and AppBuilder Settings  *********** */
/* SETTINGS FOR WINDOW w-cadpaifilho-ambos
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   L-To-R                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadpaifilho-ambos)
THEN w-cadpaifilho-ambos:HIDDEN = yes.
/* _RUN-TIME-ATTRIBUTES-END */
 
/* ************************  Control Triggers  ************************ */
ON END-ERROR OF w-cadpaifilho-ambos /* Manutená∆o <Insira o complemento> */
OR ENDKEY OF w-cadpaifilho-ambos ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.
/* _UIB-CODE-BLOCK-END */
ON WINDOW-CLOSE OF w-cadpaifilho-ambos /* Manutená∆o <Insira o complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF bt-desmonta IN FRAME f-cad
DO:
  def var p-rowid as rowid no-undo.
  RUN pi-posicao-query IN h_essf0903q01 (OUTPUT p-rowid).
  find pallet where rowid(pallet) = p-rowid no-lock no-error.
  if avail pallet and pallet.situacao = 1 then do:
     run utp/ut-msgs.p (input "Show":U,
                        input 17006,
                        input "Pallet n∆o Confirmado!":U + "~~" + "N∆o pode ser desmontado.":U).
     return no-apply.
  end.
  
  if avail pallet and pallet.situacao <> 1 then do:
 
        find FIRST saldo-estoq
                  where saldo-estoq.cod-estabel = pallet.cod-estabel
                    and saldo-estoq.cod-depos   = "EXP":U
                    and saldo-estoq.cod-localiz = pallet.cod-localiz
                    and saldo-estoq.lote        = pallet.nr-pallet
                    and saldo-estoq.it-codigo   = pallet.it-codigo
                    /*and saldo-estoq.cod-refer   = pallet.cod-refer*/ 
                    AND saldo-estoq.qtidade-atu > 0 no-lock no-error.
             
             if not avail saldo-estoq then do:
                
              
                  
                run utp/ut-msgs.p (input "show":U,
                                   input 17006,
                                   input "Saldo n∆o encontrado para o item " + pallet.it-codigo + "~~" +
                                         "N∆o ser† poss°vel a despaletizaá∆o do pallet " + pallet.nr-pallet + ".").
                return no-apply.
      
                
             end.
   end.  
  
  
  
  
  
      
  run utp/ut-msgs.p (input "show":U,
                     input 27100,
                     input "Deseja desatualizar o Pallet?").
  if return-value = "yes" then do:
     run pi-desmonta-pallet in h_essf0903v01.
  end.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF bt-monta IN FRAME f-cad
DO:
  run pi-confirma-pallet in h_essf0903v01.
  if return-value <> "NOK":U then
     apply "choose":U to bt-print in frame f-cad.
END.
ON CHOOSE OF bt-copynao IN FRAME f-cad
DO:
   MESSAGE "Opá∆o temporariamente desativada"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF bt-print IN FRAME f-cad
DO:
  def var p-rowid as rowid no-undo.
  RUN pi-posicao-query IN h_essf0903q01 (OUTPUT p-rowid).
  find pallet where rowid(pallet) = p-rowid no-lock no-error.
  if avail pallet and pallet.situacao = 1 then do:
     run utp/ut-msgs.p (input "Show":U,
                        input 17006,
                        input "Pallet n∆o Confirmado!":U + "~~" + "N∆o pode ser impresso.":U).
     return no-apply.
  end.
  run utp/ut-msgs.p (input "show":U,
                     input 27100,
                     input "Deseja imprimir o Pallet?":U).
  if return-value = "yes" then
     run sfc/essf0903prp.p (input p-rowid).
     /*run sfc/essf0903p.w (input p-rowid).*/
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF bt-transfer IN FRAME f-cad
DO:
    def var p-rowid as rowid no-undo.
    RUN pi-posicao-query IN h_essf0903q01 (OUTPUT p-rowid).
    find pallet where rowid(pallet) = p-rowid no-lock no-error.
    IF AVAIL pallet THEN 
        RUN sfc/essf0903f.w (INPUT p-rowid).
    run pi-display in h_essf0903v01.
         
END.
ON CHOOSE OF bt-integra IN FRAME f-cad
DO:
    DEFINE VARIABLE i-paletes-integrados AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i-bobinas-integrados AS INTEGER     NO-UNDO.
    reportes:
do trans on error undo, leave:

   FOR EACH  polo_off_pallet WHERE  NOT polo_off_pallet.log_ems .
        FIND FIRST pallet WHERE  
            pallet.cod-estabel  = "422" AND
            pallet.nr-pallet    =  polo_off_pallet.nr_pallet AND
            pallet.it-codigo    =  polo_off_pallet.it_codigo NO-ERROR.

        IF NOT AVAIL pallet THEN DO:

            CREATE  pallet.
            i-paletes-integrados = i-paletes-integrados + 1.
            ASSIGN
                pallet.cod-estabel  = "422"
                pallet.nr-pallet    =  polo_off_pallet.nr_pallet 
                pallet.it-codigo    =  polo_off_pallet.it_codigo               
                pallet.nr-pedido    =  int(polo_off_pallet.nr_pedido )
                pallet.nr-sequencia =  int(polo_off_pallet.nr_sequencia)
               /* pallet.nr-bobinas */
                pallet.data-pallet  =  date(polo_off_pallet.data_pallet )
                pallet.cod-operador =  string(polo_off_pallet.cod_operador)
                pallet.peso-bruto   =  polo_off_pallet.peso_bruto 
               /* pallet.peso-liquido */
                pallet.situacao     =  1
                pallet.int-1 = 98

                .

        END.

    

    FOR EACH  polo_off_it_pallet WHERE   polo_off_it_pallet.log_ems = NO AND
         polo_off_it_pallet.nr_pallet =  polo_off_pallet.nr_pallet AND
         polo_off_it_pallet.it_codigo =  polo_off_pallet.it_codigo 
         .

         FIND FIRST pallet WHERE  
            pallet.cod-estabel  = "422" AND
            pallet.nr-pallet    =  polo_off_it_pallet.nr_pallet AND
            pallet.it-codigo    =  polo_off_it_pallet.it_codigo NO-ERROR.

         IF NOT AVAIL pallet THEN NEXT.

         ASSIGN 
                 pallet.cod-refer = polo_off_it_pallet.cod_refer. 
         FIND FIRST it-pallet WHERE  
            it-pallet.cod-estabel  = "422" AND
            it-pallet.nr-pallet    =  polo_off_it_pallet.nr_pallet AND
            it-pallet.it-codigo    =  polo_off_it_pallet.it_codigo and
            it-pallet.lote-bobina  =  polo_off_it_pallet.nr_bobina NO-ERROR.

         IF AVAIL it-pallet THEN NEXT.

         CREATE it-pallet.

         i-bobinas-integrados = i-bobinas-integrados + 1.
         ASSIGN
           it-pallet.cod-estabel  = "422" 
           it-pallet.nr-pallet    =  polo_off_it_pallet.nr_pallet 
           it-pallet.it-codigo    =  polo_off_it_pallet.it_codigo 
           it-pallet.lote-bobina  =  polo_off_it_pallet.nr_bobina 
           it-pallet.cod-refer-b  =  polo_off_it_pallet.cod_refer 
           it-pallet.saldo-bobina =  polo_off_it_pallet.peso_liq
           it-pallet.cod-depos-b  =  "PRO"    
           it-pallet.dt-vali-lote-b =  polo_off_it_pallet.dt_vali_lote.



        

       polo_off_it_pallet.log_ems = YES.

    END.
    
       pallet.peso-bruto   =  polo_off_pallet.peso_bruto .
       polo_off_pallet.log_ems =  YES .
    
END.

END.
MESSAGE  "Paletes integrados:"  i-paletes-integrados skip
         "Bobinas integradas:" i-bobinas-integrados
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
         
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF MENU-ITEM mi-alterar /* Alterar */
DO:
  RUN pi-alterar IN h_p-cadpai.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF MENU-ITEM mi-anterior /* Anterior */
DO:
  RUN pi-anterior IN h_p-navega.
END.
/* _UIB-CODE-BLOCK-END */
ON MENU-DROP OF MENU mi-arquivo /* Arquivo */
DO:
  run pi-disable-menu.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME f-cad
DO:
  RUN pi-ajuda IN h_p-exihel.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF MENU-ITEM mi-copiar /* Copiar */
DO:
 /* RUN pi-copiar IN h_p-cadpai.*/
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF MENU-ITEM mi-eliminar /* Eliminar */
DO:
  RUN pi-eliminar IN h_p-cadpai.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF MENU-ITEM mi-incluir /* Incluir */
DO:
  RUN pi-incluir IN h_p-cadpai.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF MENU-ITEM mi-pesquisa /* Pesquisa */
DO:
  RUN pi-pesquisa IN h_p-navega.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF MENU-ITEM mi-primeiro /* Primeiro */
DO:
  RUN pi-primeiro IN h_p-navega.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF MENU-ITEM mi-proximo /* Pr¢ximo */
DO:
  RUN pi-proximo IN h_p-navega.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  /*************************************************************************
**
** SOBRE.I - Include padrío para chamada do Sobre
** Data Criaªío: 22/07/97
** Criado por..: Fabiano
**
**************************************************************************/
    def var c-nom-prog-ext     as char no-undo.
    def var c-nom-prog-ext-aux as char no-undo.
    
    assign c-nom-prog-ext = program-name(1).
    if c-nom-prog-ext begins "USER-INTERFACE-TRIGGER":U then
        assign c-nom-prog-ext = substr(c-nom-prog-ext,24)
               file-info:file-name = c-nom-prog-ext
               c-nom-prog-ext-aux = file-info:full-pathname.
    run btb/btb901zb.p (c-programa-mg97,
                        c-nom-prog-ext-aux,
                        c-versao-mg97).    
/* include/sobre.i */
 
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF MENU-ITEM mi-ultimo /* Èltimo */
DO:
  RUN pi-ultimo IN h_p-navega.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF MENU-ITEM mi-va-para /* V† para */
DO:
  RUN pi-vapara IN h_p-navega.
END.
/* _UIB-CODE-BLOCK-END */
/* ***************************  Main Block  *************************** */
/* Include custom  Main Block code for SmartWindows. */
/* windowmn.i - Main Block code for objects which create windows.*/
/* Skip all of this if no window was created. */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
IF VALID-HANDLE(w-cadpaifilho-ambos) THEN DO:
    ASSIGN CURRENT-WINDOW                = w-cadpaifilho-ambos 
       w-cadpaifilho-ambos:KEEP-FRAME-Z-ORDER = YES
       THIS-PROCEDURE:CURRENT-WINDOW = w-cadpaifilho-ambos.
    /* The CLOSE event can be used from inside or outside the procedure to  */
    /* terminate it.                                                        */
    ON CLOSE OF THIS-PROCEDURE 
       RUN dispatch IN THIS-PROCEDURE ('destroy':U).
    RUN dispatch ('create-objects':U).
/* Execute this code only if not being run PERSISTENT, i.e., if in test mode
   of one kind or another or if this is a Main Window. Otherwise postpone 
   'initialize' until told to do so. */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
    /* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
    MAIN-BLOCK:
    DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
       ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
 
    /* Now enable the interface and wait for the exit condition.            */
       RUN dispatch ('initialize':U).
       
       IF RETURN-VALUE = 'ADM-ERROR':U THEN RETURN.
       
       IF NOT THIS-PROCEDURE:PERSISTENT THEN
           WAIT-FOR CLOSE OF THIS-PROCEDURE.
    END.
END.
END.
 
/* _UIB-CODE-BLOCK-END */
/* **********************  Internal Procedures  *********************** */
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.
  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).
  CASE adm-current-page: 
    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-navega.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navega ).
       RUN set-position IN h_p-navega ( 1.17 , 1.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 24.00 ) */
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-cadpai.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-cadpai ).
       RUN set-position IN h_p-cadpai ( 1.17 , 26.72 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 74.72 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sfc/essf0903v01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_essf0903v01 ).
       RUN set-position IN h_essf0903v01 ( 2.54 , 1.72 ) NO-ERROR.
       /* Size in UIB:  ( 3.25 , 88.57 ) */
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Itens' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 5.92 , 1.29 ) NO-ERROR.
       RUN set-size IN h_folder ( 12.13 , 89.29 ) NO-ERROR.
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sfc/essf0903b01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?,
                     ProgAtributo = ,
                     ProgIncMod = sfc\essf0903b.w,
                     MessageNum = 0,
                     MessageParam = ':U ,
             OUTPUT h_essf0903b01 ).
       RUN set-position IN h_essf0903b01 ( 7.13 , 2.14 ) NO-ERROR.
       /* Size in UIB:  ( 10.50 , 87.72 ) */
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sfc/essf0903q01.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'ProgPesquisa = sfc\essf0903z01.w,
                     ProgVaPara = sfc\essf0903g01.w,
                     ProgIncMod = sfc\essf0903a.w,
                     Implantar = no':U ,
             OUTPUT h_essf0903q01 ).
       RUN set-position IN h_essf0903q01 ( 1.00 , 66.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.63 , 7.72 ) */
       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).
       /* Links to SmartViewer h_essf0903v01. */
       RUN add-link IN adm-broker-hdl ( h_p-cadpai , 'TableIO':U , h_essf0903v01 ).
       RUN add-link IN adm-broker-hdl ( h_essf0903q01 , 'Record':U , h_essf0903v01 ).
       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).
       /* Links to BrowserCadastro2 h_essf0903b01. */
       RUN add-link IN adm-broker-hdl ( h_essf0903q01 , 'Record':U , h_essf0903b01 ).
       /* Links to SmartQuery h_essf0903q01. */
       RUN add-link IN adm-broker-hdl ( h_p-cadpai , 'State':U , h_essf0903q01 ).
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , h_essf0903q01 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_essf0903q01 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'State':U , h_essf0903q01 ).
       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navega ,
             bt-monta:HANDLE IN FRAME f-cad , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-cadpai ,
             h_p-navega , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-transfer:HANDLE IN FRAME f-cad , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_essf0903v01 ,
             h_p-exihel , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             h_essf0903v01 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_essf0903b01 ,
             h_folder , 'AFTER':U ).
    END. /* Page 0 */
  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
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
/* Note: open-query does its own notify of row-available */
RUN notify IN THIS-PROCEDURE ('row-available':U).
 
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
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadpaifilho-ambos)
  THEN DELETE WIDGET w-cadpaifilho-ambos.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
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
  ENABLE bt-copynao bt-monta bt-desmonta bt-print bt-transfer rt-button 
      WITH FRAME f-cad IN WINDOW w-cadpaifilho-ambos.
  
  VIEW w-cadpaifilho-ambos.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  /*************************************************************************
**
** I-LOGFIN.I - Encerra o Log de Execuªío
**
**************************************************************************/
/*************************************************************************
**
** I-LOGFIN1.I - Encerra o Log de Execucao
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
/* Eliminaªío de arquivos temporˇrios */
/* Fim da eliminaªío de arquivos temporˇrios */
/* i-logfin */
 
  
  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
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
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  /***********************************************************************
**
**  WIN-SIZE.I - Realiza o ajuste no tamanho da window e da frame
**               igualando ambos
*************************************************************************/
if w-cadpaifilho-ambos:width-chars < frame f-cad:width-chars then
    assign frame f-cad:width-chars = w-cadpaifilho-ambos:width-chars.
else if frame f-cad:width-chars < w-cadpaifilho-ambos:width-chars then
    assign w-cadpaifilho-ambos:width-chars = frame f-cad:width-chars.
if w-cadpaifilho-ambos:height-chars < frame f-cad:height-chars then
    assign frame f-cad:height-chars = w-cadpaifilho-ambos:height-chars.
else if frame f-cad:height-chars < w-cadpaifilho-ambos:height-chars then
    assign w-cadpaifilho-ambos:height-chars = frame f-cad:height-chars.
assign w-cadpaifilho-ambos:virtual-width-chars  = w-cadpaifilho-ambos:width-chars
       w-cadpaifilho-ambos:virtual-height-chars = w-cadpaifilho-ambos:height-chars
       w-cadpaifilho-ambos:min-width-chars      = w-cadpaifilho-ambos:width-chars
       w-cadpaifilho-ambos:max-width-chars      = w-cadpaifilho-ambos:width-chars
       w-cadpaifilho-ambos:min-height-chars     = w-cadpaifilho-ambos:height-chars
       w-cadpaifilho-ambos:max-height-chars     = w-cadpaifilho-ambos:height-chars.
/* win-size.i */
 
  run pi-before-initialize.
  /***********************************************************************
**  /*   */
**  UT9000.I - Definiá∆o das vari†veis de ambiente do Magnus 97
**  {1} = programa provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/
/* System Variable Definitions ---                                       */
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
** Criaá∆o : John Cleber Jaraceski
**
** Objetivo : Definicao das System Variables
**
** Ultima Alt : ?
*******************************************************************************/
/* include/i-sysvar.i ---                                                     */
 
def var c_cod_empres_usuar as char no-undo.
def var c_nom_razao_social as char no-undo.
    /*rodar pi-rsocial persistent para verificaá∆o empresa usuario*/
    if not valid-handle(h-rsocial)
    or h-rsocial:type <> "procedure":U
    or h-rsocial:file-name <> "utp/ut-rsocial.p":U then do:
        if l-achou-prog then
            run utp/ut-rsocial.p persistent set h-rsocial.
    end.
    if l-achou-prog then
        run pi-rsocial in h-rsocial (output c_cod_empres_usuar, output c_nom_razao_social).
    assign c-programa-mg97 = caps("essf0903")
           c-versao-mg97   = "2.00.00.000".
    find prog_dtsul no-lock
        where prog_dtsul.cod_prog_dtsul = c-programa-mg97 no-error.
    if  avail prog_dtsul then do:
        assign c-titulo-prog-mg97    = prog_dtsul.des_prog_dtsul
               c-nom-prog-upc-mg97   = prog_dtsul.nom_prog_upc
               c-nom-prog-appc-mg97  = prog_dtsul.nom_prog_appc
               c-nom-prog-dpc-mg97   = prog_dtsul.nom_prog_dpc
               i-num-topico-hlp-mg97 = prog_dtsul.num_topico.
       /*
          if session:window-system <> "TTY" then 
            assign i-template          = prog_dtsul.ind_template.
       */
        
        find procedimento no-lock
            where procedimento.cod_proced = prog_dtsul.cod_proced no-error.
        if  avail procedimento then do:
            find modul_dtsul no-lock
                where modul_dtsul.cod_modul_dtsul = procedimento.cod_modul_dtsul no-error. 
            if  avail modul_dtsul then do:
                assign c-modulo-mg97         = caps(modul_dtsul.nom_modul_dtsul_menu)
                       c-cod-mod-mg97        = caps(modul_dtsul.cod_modul_dtsul)
                       c-nom-manual-hlp-mg97 = "dochlp~/" + string(modul_dtsul.num_manual_documen, "999999") + ".hlp".
            end.
        end.
    end.                                                      
    else do:
        assign c-titulo-prog-mg97    = caps(c-programa-mg97)
               c-nom-prog-upc-mg97   = ""
               c-nom-prog-appc-mg97  = ""
               i-num-topico-hlp-mg97 = 0
               c-nom-manual-hlp-mg97 = "dochlp~/000000.hlp".
    end.                 
     
    
         assign w-cadpaifilho-ambos:title = if l-achou-prog then
                                       c-titulo-prog-mg97
                                     + " - " 
                                     + c-programa-mg97 
                                     + " - " 
                                     + c-versao-mg97  
                                     + " - " 
                                     + c_cod_empres_usuar
                                     + " - " 
                                     + c_nom_razao_social
                                     else 
                                       c-titulo-prog-mg97
                                     + " - " 
                                     + c-programa-mg97 
                                     + " - " 
                                     + c-versao-mg97.
    
    
 if today > 03/01/1998 then do:    
 /******************************* Validaá∆o ***********************************/   
    /* Verificaá∆o do registro do produto */
    if c-programa-mg97 <> "UT-CONF" then do:
      run utp/ut-vfreg.p (output l-acesso-livre).
      if l-acesso-livre = no then do:
        run utp/ut-msgs.p (input "show",
                           input 8525,
                           input "").      
        apply "close" to this-procedure.
        return.
      end.    
    end.  
    /* Verificaá∆o da data de validade do contrato */
    if c-programa-mg97 <> "UT-CONF" then do:
      run utp/ut-vfvld.p (output d-data-contrato).
      if d-data-contrato < today then do:
        run utp/ut-msgs.p (input "show",
                           input 8536,
                           input string(d-data-contrato)).      
        apply "close" to this-procedure.
        return.
      end.
    end.  
    /* Verificaá∆o do acesso ao modulo do programa com base no contrato */
    if c-programa-mg97 <> "UT-CONF" then do:
      run utp/ut-vfmod.p (input c-cod-mod-mg97, output l-acesso-livre).
      if l-acesso-livre = no then do:
        run utp/ut-msgs.p (input "show",
                           input 8527,
                           input c-cod-mod-mg97).      
        apply "close" to this-procedure.
        return.
      end.  
    end.  
    
    /* Verificaá∆o de usu†rios ativos */
    if c-programa-mg97 <> "UT-CONF" then do:
      run utp/ut-vfusr.p (output i-user-conectados, output i-licenca-usuar).
      if i-user-conectados > i-licenca-usuar then do:
        run utp/ut-msgs.p (input "show",
                           input 8532,
                           input string(i-user-conectados) + "~~" +
                                 string(i-licenca-usuar)).      
        apply "close" to this-procedure.
        return.
      end.
    end.  
/******************************************************************************/
 end.
    
    /* Verificaá∆o da seguranáa e login informado */
    /*--------------------------------------------------------------------------
    File        : UTP/UT-VFSEC.I
    Purpose     : Verificaá∆o da Seguranáa
    Syntax      :
    Description : Verificar a seguranáa
    Author(s)   : Fabiano
    Created     : 31/12/1997
    Notes       :
------------------------------------------------------------------------*/
/* N∆o faz a validaá∆o para programas do tipo V† Para */
if index(replace(program-name(1),"~\","~/"),"go/g") = 0 then do:
  run men/men901za.p (input c-programa-mg97).
  if  return-value = "2012" then do:
      run utp/ut-msgs.p (input "show",
                         input 2858,
                         input c-programa-mg97).
      if "w-paiamb" = "SmartDialog" or this-procedure:persistent = no then
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
      if "w-paiamb" = "SmartDialog" then
        return "adm-error".
      else do:
        delete procedure this-procedure.
        return.
      end.  
  end.
end.  
/* ut-vfsec.i */
 
    
    /* Inicio do log de execuá∆o de programas */
    /*************************************************************************
**                                                                        
**  I-LOGINI.I - Inicia Log de Execuá∆o
**
*************************************************************************/
run btb/btb918zb.p (input c-programa-mg97,
                    input-output rw-log-exec,
                    input yes).
/* i-logini */
  
    
    
      if session:window-system <> "TTY" then do:
       /********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/********************************************************************************
** Programa : include/i-trswin.i
**
** Data : 29/12/97
**
** Criaá∆o : John Cleber Jaraceski
**
** Objetivo : Realizar alteracoes em todos os programas que possuam interface 
**            com o usuario (window/dialog). 
**            Estas alteracoes sao :
**              - Centralizar Window
**              - Desabilitar MAX - RESIZE
**              - Ocultar MAX - MIN
**              - Tornar uma Window Modal
**
** Ultima Alt : 29/12/1997
*******************************************************************************/
/* Transformacao Window *****************************************************/
    case i-template:
        when 2 then do: /* Cadastro Simples */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.
        end.
        when 3 then do: /* Cadastro Simples - Alteracao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.
        end.
        when 4 then do: /* Cadastro Complexo */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.        
        end.
        when 5 then do: /* Cadastro Complexo - Alteracao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.        
        end.
        when 6 then do: /* Cadastro Simples - Inclusao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.        
        end.
        when 7 then do: /* Cadastro PaiXFilho - Atualiza Filho */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.        
        end.
        when 8 then do: /* Cadastro PaiXFilho - Atualiza Ambos */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
            
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.        
        end.
        when 9 then do: /* Inclui/Modifica Pai */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.        
        end.
        when 10 then do: /* Inclui/Modifica Filho */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.        
        end.
        when 11 then do: /* Formacao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.        
        end.
        when 12 then do: /* Parametros Unicos */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.        
        end.
        when 13 then do: /* Pesquisa */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.        
        end.
        when 14 then do: /* Consulta Simples */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.        
        end.
        when 15 then do: /* Consulta Complexa */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.        
        end.
        when 16 then do: /* Consulta Relacionamento */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.        
        end.
        when 17 then do: /* Relatorio */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.        
        end.
        when 20 then do: /* Janela Detalhe */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD.
            if  h-pai:handle = w-cadpaifilho-ambos:handle then
                assign h-pai           = h-pai:NEXT-SIBLING
                       h-pai:SENSITIVE = no.
            else 
                assign  h-pai = w-cadpaifilho-ambos:handle
                        h-pai = h-pai:parent.
            h-pai:sensitive = no.
  
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.        
        end.
        when 21 then do: /* Janela Mestre */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.        
        end.
        when 26 then do: /* Importacao/Exportacao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.
        end.
        when 29 then do: /* Relatorio Gerado Pelo DataViewer */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.
        end.
        when 30 then do: /* Formacao Sem Navegacao */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.
        end.
        when 31 then do: /* Estrutura */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.
        end.
        when 32 then do: /* Digitaá∆o Rapida */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-cadpaifilho-ambos:hWnd).
            delete procedure h-prog.
            
            assign w-cadpaifilho-ambos:HIDDEN = yes
            w-cadpaifilho-ambos:HIDDEN = no.
            apply "ENTRY":U to w-cadpaifilho-ambos.
        end.
    end case.
/* Transformacao Window *****************************************************/
 
      end. 
    
/* ut9000.i */
 
  
  run pi-recebe-cad-pai in h_essf0903v01 (input h_p-cadpai).
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
RUN Who-Is-The-Container IN adm-broker-hdl 
     (INPUT h_p-cadpai, OUTPUT c-handle-win).
    /* Code placed here will execute AFTER standard behavior.    */
  run pi-after-initialize.
  
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-disable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM p-desabilita AS LOGICAL NO-UNDO.
  IF p-desabilita = YES THEN
      DISABLE bt-copynao bt-monta bt-desmonta bt-print WITH FRAME f-cad.
  ELSE
      ENABLE bt-copynao bt-monta bt-desmonta bt-print WITH FRAME f-cad. 
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/
  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-paiamb, and there are no
     tables specified in any contained Browse, Query, or Frame. */
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
