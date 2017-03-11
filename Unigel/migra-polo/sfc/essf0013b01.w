

/* Connected Databases 
          svamfg           PROGRESS
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

def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[9.99.99.999[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "9.99.99.999"
       c-prg-obj = "B99XX999".
if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "B99XX999"
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
    put "B99XX999" at 1 "9.99.99.999" at 69 today at 84 string(time,'HH:MM:SS':U) at 94 skip.
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

/*:T Variaveis usadas internamente pelo estilo, favor nao elimina-las     */

/*:T v†ri†veis de uso globla */
def new global shared var v-row-parent as rowid no-undo.

/*:T vari†veis de uso local */
def var v-row-table  as rowid.

/*:T fim das variaveis utilizadas no estilo */

def var i-combo as int no-undo.
def var i-seq   as int no-undo.
def var i-cont  as int no-undo.
def var cont-select as int no-undo.
def var i-pai   as int no-undo.
def var i-cur   as int no-undo.
def var c-pai   as char no-undo.

def var chTreeView      as com-handle no-undo.
def var chTreeViewNode  as com-handle no-undo.
def var chImageList     as com-handle no-undo.

def temp-table tt-registros no-undo
    field i-seq   as integer
    field tipo    as integer
    FIELD otica   AS CHAR INITIAL "N"
    FIELD delta   AS CHAR INITIAL "N"
    field r-rowid as rowid.

def var i-cont-disp as int init 0 no-undo.

def var i-index as int no-undo.

def var l-folha-corrente as logical init no no-undo.

def new global shared var grw-lote-item as rowid no-undo.
def new global shared var gc-estado     as char  no-undo.
  DEFINE BUFFER bm-lote-carac-tec FOR lote-carac-tec.
  DEFINE VARIABLE i-mediaqtd AS INTEGER    NO-UNDO.
  DEFINE VARIABLE d-mediaotica AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE d-durezamenor AS decimal    NO-UNDO.
  DEFINE VARIABLE d-durezamaior AS decimal    NO-UNDO.
  DEFINE VARIABLE d-deltarho AS DECIMAL    NO-UNDO.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* Name of first Frame and/or Browse and/or first Query                 */

/* External Tables                                                      */


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR lote-item.
/* Internal Tables (found by Frame, Query & Browse Queries)             */

/* Definitions for BROWSE br-table                                      */


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */




/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS
><EXECUTING-CODE>
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
&BROWSE-NAME
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


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cb-folha AS CHARACTER FORMAT "X(256)":U 
     LABEL "Folha Lote" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24.14 BY 1 NO-UNDO.

DEFINE VARIABLE ed-texto AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 47.14 BY 3.54 NO-UNDO.

DEFINE VARIABLE fi-complemento AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 47.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dat-max AS DATE FORMAT "99/99/9999":U 
     LABEL "Max." 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dat-min AS DATE FORMAT "99/99/9999":U 
     LABEL "Min." 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-num-max AS DECIMAL FORMAT "->>>>>>,>>9.9999":U INITIAL 0 
     LABEL "Max." 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-num-min AS DECIMAL FORMAT "->>>>>>,>>9.9999":U INITIAL 0 
     LABEL "Min." 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-observacao AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 47.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-resultado-dat AS DATE FORMAT "99/99/9999":U 
     LABEL "Resultado" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-resultado-num AS DECIMAL FORMAT "->>>>>>,>>9.9999":U INITIAL 0 
     LABEL "Resultado" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-un-num AS CHARACTER FORMAT "x(15)":U 
     LABEL "Un" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 51.14 BY 16.17.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 37.72 BY 16.17.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.57 BY 2.92.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.57 BY 2.04.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.57 BY 2.04.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.57 BY 4.25.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.57 BY 2.92.

/* Query definitions                                                    */

DEFINE QUERY br-table FOR 
      lote-carac-tec SCROLLING.


/* Browse definitions                                                   */
DEFINE BROWSE br-table

  QUERY br-table NO-LOCK DISPLAY
      lote-carac-tec.cd-folha FORMAT "x(8)":U
      lote-carac-tec.cd-comp FORMAT "x(8)":U
/* _UIB-CODE-BLOCK-END */

    WITH NO-ASSIGN SEPARATORS SIZE 13.43 BY 3.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cb-folha AT ROW 1.17 COL 12.14 COLON-ALIGNED
     fi-num-min AT ROW 2.08 COL 47.72 COLON-ALIGNED
     fi-num-max AT ROW 2.08 COL 69.14 COLON-ALIGNED
     fi-un-num AT ROW 3.13 COL 69.14 COLON-ALIGNED
     fi-resultado-num AT ROW 3.17 COL 47.72 COLON-ALIGNED
     fi-complemento AT ROW 5.46 COL 38.57 COLON-ALIGNED NO-LABEL
     fi-observacao AT ROW 7.83 COL 38.57 COLON-ALIGNED NO-LABEL
     br-table AT ROW 8.08 COL 13.29
     ed-texto AT ROW 10 COL 40.72 NO-LABEL
     fi-dat-min AT ROW 14.71 COL 47.72 COLON-ALIGNED
     fi-dat-max AT ROW 14.71 COL 69.14 COLON-ALIGNED
     fi-resultado-dat AT ROW 15.75 COL 47.72 COLON-ALIGNED
     RECT-1 AT ROW 1 COL 38.86
     RECT-2 AT ROW 1 COL 1
     RECT-3 AT ROW 1.42 COL 39.57
     RECT-4 AT ROW 4.75 COL 39.57
     RECT-5 AT ROW 7.13 COL 39.57
     RECT-6 AT ROW 9.5 COL 39.57
     RECT-7 AT ROW 14.04 COL 39.57
     " Resultado NumÇrico" VIEW-AS TEXT
          SIZE 16.86 BY .67 AT ROW 1.17 COL 41.43
     " Resultado Complemento" VIEW-AS TEXT
          SIZE 22 BY .67 AT ROW 4.5 COL 41.43
     " Resultado Observaá∆o" VIEW-AS TEXT
          SIZE 20.14 BY .67 AT ROW 6.88 COL 41.43
     " Resultado Texto" VIEW-AS TEXT
          SIZE 13.86 BY .67 AT ROW 9.25 COL 41.43
     " Resultado Data" VIEW-AS TEXT
          SIZE 16.86 BY .67 AT ROW 13.79 COL 41.43
     SPACE(0.00) SKIP(2.54)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: svamfg.lote-item
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "i:\servicos\especificos\polo\programas\sfc\essf0013b01.w should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.



/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 16.17
         WIDTH              = 89.
/* END WINDOW DEFINITION */
                                                                        */



/* ************************* Included-Libraries *********************** */




/*-------------------------------------------------------------------------
    Library     : browser.i  
    Purpose     : Base ADM methods for Browser objects
  
    Syntax      : {src/adm/method/browser.i}

    Description :
  
    Author(s)   :
    Created     :
    HISTORY: 
--------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE adm-sts           AS LOGICAL NO-UNDO.
DEFINE VARIABLE adm-brs-in-update AS LOGICAL NO-UNDO INIT no.
DEFINE VARIABLE adm-brs-initted   AS LOGICAL NO-UNDO INIT no.




/* Dialog program to run to set runtime attributes - if not defined in master */


/* +++ This is the list of attributes whose values are to be returned
   by get-attribute-list, that is, those whose values are part of the
   definition of the object instance and should be passed to init-object
   by the UIB-generated code in adm-create-objects. */



/* _UIB-CODE-BLOCK-END */




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


  
      adm-object-hdl = FRAME F-Main:HANDLE.
  


/* Traduá∆o de Hard-Coded View-as */ 





    run pi-trad-widgets (input frame F-Main:handle).


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
     'SmartBrowser~`':U +      /* Type attribute */
     '~`':U +       /* Container-Type attribute */
   
     'YES~`':U +                   /* Query-Object attribute */
   
     'lote-item~`':U +    /* External-Tables attribute */
     'lote-carac-tec~`':U +    /* Internal-Tables attribute */
   
     '~`':U +
   
     (IF adm-object-hdl = ? THEN "":U ELSE STRING(adm-object-hdl))
        + "~`":U +    /* Adm-Object-Handle attribute */
   
     'Initial-Lock,Hide-on-Init,Disable-on-Init,Key-Name,Layout,Create-On-Add,SortBy-Case~`':U +  /* Attribute-List attribute */
   
   
     'Record-Source,Record-Target,TableIO-Target~`':U + /* Supported-Links attribute */
   
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
    

    
    DISABLE cb-folha br-table RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 RECT-6 RECT-7 WITH FRAME F-Main.
    RUN dispatch ('disable-fields':U).  
    

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
      RUN support/browserd.w (INPUT THIS-PROCEDURE).
  

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
   

   
    ENABLE UNLESS-HIDDEN cb-folha br-table RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 RECT-6 RECT-7 WITH FRAME F-Main.

    /* We also run enable_UI from here. */ 
    RUN enable_UI IN THIS-PROCEDURE NO-ERROR.
   

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
   
   /* est† verificaá∆o se faz necess†ria devido aos programas */
      


    /* Tenta identificar pelo ADM-CONTAINER */
    

    /* Se ainda nío identificou se ≤ window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
        /* Tenta identificar pelo PROCEDURE-TYPE */
        
        
            
            
        
    
    

    /* Se ainda nío identificou se ≤ window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
        /* Tenta identificar pelo WINDOW-NAME */
        
        
            
            
        
        
        

    
    
    
    /*
    &IF DEFINED(WenIsWindow) &THEN
        RUN registerWindow IN hWenController (THIS-PROCEDURE, CURRENT-WINDOW).
    &ELSEIF DEFINED(WenIsDialog) &THEN
        RUN registerWindow IN hWenController (THIS-PROCEDURE, CURRENT-WINDOW).
    &ENDIF
    */

    /* Se tem janela */
    

    /* Se tem dialog */
    


            /* criados pelos DataViewer n∆o utilizarem a include i-prgvrs */ 
   /* e dessa forma n∆o chamarem a include i-wendef.i que define essa veri†vel. */
   
   /* fim da alateraá∆o */

   /* EPC Before Initialize do Container */ 
   

   /* EPC Before Initialize do Viewer */ 
   

   /* EPC Before Initialize do Browser */
   
        /***************************************************************
**
** i-epc017.i - EPC para Evento Before INITIALIZE de SmartBrowser
**
***************************************************************/

def var c-container as char no-undo.
run Who-Is-The-Container in adm-broker-hdl
    (input THIS-PROCEDURE,
     output c-container).

     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

if  valid-handle(widget-handle(c-container)) then do:
    
     /* DPC */
    run pi-retorna-dpc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-dpc-mg97 = return-value.
        if  c-nom-prog-dpc-mg97 <> "" then do:
            run value(c-nom-prog-dpc-mg97) (input "BEFORE-INITIALIZE":U,
                                            input "BROWSER":U,
                                            input THIS-PROCEDURE,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
     /* APPC */
    run pi-retorna-appc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-appc-mg97 = return-value.
        if  c-nom-prog-appc-mg97 <> "" then do:
            run value(c-nom-prog-appc-mg97) (input "BEFORE-INITIALIZE":U,
                                            input "BROWSER":U,
                                            input THIS-PROCEDURE,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
    
     /* UPC */
    run pi-retorna-upc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-upc-mg97 = return-value.
        if  c-nom-prog-upc-mg97 <> "" then do:
            run value(c-nom-prog-upc-mg97) (input "BEFORE-INITIALIZE":U,
                                            input "BROWSER":U,
                                            input THIS-PROCEDURE,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
end.
/* i-epc017.i */

 
   

   RUN broker-initialize IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.
   /*   Alteraá∆o para corrigir o problema de algumas viewers n∆o mostrar
        o primeiro registro quando o programa Ç inicializado               
        Data : 20/Fev/97  - J.Carlos (PGS)  -  Egolf e SÇrgio (DATASUL)  */  
   
        
             IF  frame F-Main:scrollable THEN
                 ASSIGN frame F-Main:virtual-width-chars  = frame F-Main:width-chars
                        frame F-Main:virtual-height-chars = frame F-Main:height-chars.
        
   
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
   
        /***************************************************************
**
** i-epc017.i - EPC para Evento INITIALIZE de SmartBrowser
**
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

if  valid-handle(widget-handle(c-container)) then do:
    
     /* DPC */
    run pi-retorna-dpc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-dpc-mg97 = return-value.
        if  c-nom-prog-dpc-mg97 <> "" then do:
            run value(c-nom-prog-dpc-mg97) (input "INITIALIZE":U,
                                            input "BROWSER":U,
                                            input THIS-PROCEDURE,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
     /* APPC */
    run pi-retorna-appc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-appc-mg97 = return-value.
        if  c-nom-prog-appc-mg97 <> "" then do:
            run value(c-nom-prog-appc-mg97) (input "INITIALIZE":U,
                                            input "BROWSER":U,
                                            input THIS-PROCEDURE,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
    
     /* UPC */
    run pi-retorna-upc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-upc-mg97 = return-value.
        if  c-nom-prog-upc-mg97 <> "" then do:
            run value(c-nom-prog-upc-mg97) (input "INITIALIZE":U,
                                            input "BROWSER":U,
                                            input THIS-PROCEDURE,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
end.
/* i-epc017.i */

 
   

   

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
         p-wh-frame:FONT    = 2
         p-wh-frame = p-wh-frame:FIRST-CHILD
         wh-child   = p-wh-frame:FIRST-CHILD.
  

  do  while valid-handle(wh-child):

      
        if wh-child:TYPE <> "BROWSE" then do:
           if can-query(wh-child, "FONT") then
              if wh-child:FONT = ? then assign wh-child:FONT = 1 no-error.
        end.
      

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
    File        : tableio.i  
    Purpose     : Basic ADM methods for record changes

    Syntax      : {src/adm/method/tableio.i}

    Description :

    Author(s)   :
    Created     :
    Notes       : New 8Plus Version with Multiple Enabled Table support
    HISTORY: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* Note: adm-<nth>-enabled-table is defined for each updatable table - 
               that is, a table with enabled fields.
         adm-tableio-first-table is the first (or only) table in the join;
               this is used for repositioning the query after an add, e.g.
         adm-tableio-fields is the list of enabled fields or 
               browse columns in all enabled tables.
         adm-tableio-table is no longer used but has been kept for
               backward compatibility with any user code which may reference it.
         In addition, we map adm-first-enabled-table to ENABLED-TABLES 
               if it's not otherwise defined, again for backward compatibility 
               with 8.0A-generated programs.  */
/* For Viewers: */

/* For Browsers: */
  
  /* Allow users to define this preproc themselves for special cases: */
  
    
          
  

        

  DEFINE VARIABLE adm-first-table         AS ROWID NO-UNDO.  
  DEFINE VARIABLE adm-second-table        AS ROWID NO-UNDO. 
  DEFINE VARIABLE adm-third-table         AS ROWID NO-UNDO.
  DEFINE VARIABLE adm-adding-record       AS LOGICAL NO-UNDO INIT no.
  DEFINE VARIABLE adm-return-status       AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE adm-first-prev-rowid    AS ROWID     NO-UNDO.
  DEFINE VARIABLE adm-second-prev-rowid   AS ROWID     NO-UNDO.
  DEFINE VARIABLE adm-third-prev-rowid    AS ROWID     NO-UNDO.
  DEFINE VARIABLE adm-first-add-rowid     AS ROWID     NO-UNDO.
  DEFINE VARIABLE adm-second-add-rowid    AS ROWID     NO-UNDO.
  DEFINE VARIABLE adm-third-add-rowid     AS ROWID     NO-UNDO.
  DEFINE VARIABLE adm-first-tmpl-recid    AS RECID     NO-UNDO INIT ?.
  DEFINE VARIABLE adm-second-tmpl-recid   AS RECID     NO-UNDO INIT ?.
  DEFINE VARIABLE adm-third-tmpl-recid    AS RECID     NO-UNDO INIT ?.
  DEFINE VARIABLE adm-index-pos           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE adm-query-empty         AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE adm-create-complete     AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE adm-create-on-add       AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE group-assign-add        AS LOGICAL   NO-UNDO INIT ?.

/************ Customizacao Progress - Ricardo de Lima Perdigao - 13/03/1998
              Definicao de variaveis utilizadas no ADM-CURRENT-CHANGED.
              **********************************/
  DEFINE VARIABLE h_record-source         AS HANDLE    NO-UNDO.
  DEFINE VARIABLE h_record-target         AS HANDLE    NO-UNDO.
  DEFINE VARIABLE h_tableio-source        AS HANDLE    NO-UNDO.
  DEFINE VARIABLE h_query-browse          AS HANDLE    NO-UNDO.
/***********************************************/

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */



/* ************************  Function Prototypes ********************** */




FUNCTION setInitial RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */





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








/* ***************************  Main Block  *************************** */

/* If there are no ENABLED-TABLES, then remove the TABLEIO-TARGET link
   from the list of SUPPORTED-LINKS. */
  IF "":U = "":U THEN
    RUN modify-list-attribute IN adm-broker-hdl
      (THIS-PROCEDURE, "REMOVE":U, "SUPPORTED-LINKS":U, "TABLEIO-TARGET":U).

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */




PROCEDURE adm-add-record :
/* -----------------------------------------------------------  
      Purpose:     Initiates a record add. Displays initial values
                   but does not create the record. That is done by
                   adm-assign-record.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/  
      /* EPC - Before Add da Viewer */ 
      

   /* ALTERAÄ«O FEITA PARA ATENDER A VERSAO 10 - MARCILENE - 26-11-2003 *//* Code for SmartBrowsers */ MESSAGE "Object ":U THIS-PROCEDURE:FILE-NAME 
       "must have at least one Enabled Table to perform Add.":U
       VIEW-AS ALERT-BOX ERROR.
   

    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-assign-record :
/* -----------------------------------------------------------  
      Purpose:     Assigns changes to a single record as part of an
                   update, add, or copy. If this is an add or copy, 
                   the new record is created here.
      Parameters:  <none>
      Notes:       This method is intended to be invoked from 
                   adm-update-record, which starts a transaction.       
                   This allows multiple ASSIGNs in several objects 
                   (connected with the GROUP-ASSIGN link)
                   to be part of a single update transaction.
    -------------------------------------------------------------*/  

/* EPC - Validate da Viewer */ 


 MESSAGE 
       "Object ":U THIS-PROCEDURE:FILE-NAME
         "must have at least one Enabled Table to perform Assign.":U
           VIEW-AS ALERT-BOX ERROR.
   
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     ASSIGNs field values from within assign-record
  Parameters:  <none>
  Notes:       This can be replaced with a local version if the 
               ASSIGN statement needs to be customized in some way
               which is not supportable through ADM-CREATE-FIELDS
               and ADM-ASSIGN-FIELDS, for example, to assure that
               key fields which are assigned programatically don't
               force an additional database write. Also, custom
               validation can be done in local-assign-statement before
               or after the ASSIGN.
------------------------------------------------------------------------------*/

  /* Alterado por ANDERSON(TECH540) em 30/01/2003
   Campos CHAR n∆o podem guardar valores maiores que os definidos
   em seu formato. Mudado de ems_dbtype = "SQL" para "MSS" 
   A VALIDAÄ«O ê FEITA ATRAVêS DESTA INCLUDE */
   

   /*Fim alteraá∆o Anderson 33/01/2003*/

    /* EPC - After Validate da Viewer */ 
    

    /* EPC - Before Assign do Viewer */ 
    

/* ASSIGN for Frame Fields: *//* ASSIGN for Browse Fields: */

   /* EPC - Assign do Viewer */ 
   

  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-cancel-record :
/* -----------------------------------------------------------  
      Purpose:     Cancels a record update, add or copy operation
      Parameters:  <none>
      Notes:       This simply gets the query to reposition to what
                   was the current record before the add or copy began;
                   for an update of an object such as a Viewer it gets 
                   reset-record to redisplay the fields;
                   if a transaction was open for an object with its own query, 
                   such as a Browser, it reopens the query to redisplay 
                   original values for all records which may have been changed.
    -------------------------------------------------------------*/  

  /* EPC - Cancel da Viewer */ 
  

/* Delete the new browser row */

  /* EPC - After Cancel da Viewer */ 
  

  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-copy-record :
/* -----------------------------------------------------------  
      Purpose:     Allows the creation of a new record whose initial
                   values are the same as the current record buffer.


      Parameters:  <none>
      Notes:       This is like add-record except we start with the
                   current record buffer rather than the template record.
    -------------------------------------------------------------*/  

    MESSAGE "Object ":U THIS-PROCEDURE:FILE-NAME 
     "must have at least one Enabled Table to perform Copy.":U
       VIEW-AS ALERT-BOX ERROR.
   

    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-create-record :
/* -----------------------------------------------------------  
      Purpose:    Performs the actual CREATE <table> statement(s)
                  to create a new row for a query. This is normally
                  dispatched from adm-assign-record, but may be done 
                  from adm-add-record if the 'Create-On'Add' attribute
                  is set.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/  
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-current-changed :
/*------------------------------------------------------------------------------
  Purpose:     Upgrades the lock on the current record to EXCLUSIVE.
               Checks whether it has been changed and redisplays
               the values in the changed record if it has been changed.
  Parameters:  <none>
  Notes:       Can be customized to change the lock upgrade code or
               to replace or supplement the CURRENT-CHANGED function,
               for example, to save off this user's changes and
               reconcile them with the other copy of the record.
------------------------------------------------------------------------------*/


RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-delete-record :
/* -----------------------------------------------------------
      Purpose:     Deletes the current record.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/           

/************ Customizacao PGS - Ricardo de Lima Perdigao - 14/03/1997
              Resolve problema do botao de delete voltar a mensagem de registro not available
              **********************************/

/***************************************************************/
/* If I don't have my own query, */ MESSAGE 
       "Object ":U THIS-PROCEDURE:FILE-NAME 
         "must have at least one Enabled Table to perform Delete.":U
           VIEW-AS ALERT-BOX ERROR.
   

    RUN dispatch IN THIS-PROCEDURE ('apply-entry':U). 

    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-disable-fields :
/* -----------------------------------------------------------
      Purpose:     Disables fields in the {&ENABLED-FIELDS} list.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/           

   /* EPC - Disable da Viewer */ 
   

   
      /* If this object is linked to others to be updated together,
         then disable fields together: */
      RUN notify ('disable-fields, GROUP-ASSIGN-TARGET':U).

   /* EPC - After Disable da Viewer */ 
   

    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-enable-fields :
/* -----------------------------------------------------------
      Purpose:     Enable all db fields in the {&ENABLED-FIELDS} list
                   for the default frame. Refind the current record
                   SHARE-LOCKED, and redisplay it in case it has changed.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/           

   /* EPC - Enable da Viewer */ 
   

   
    /* Customizacao PGS Sofware para executar notify enable-fields em todos os group
    assign targets. O endif foi movido para acima do run notify, desta forma o notify
    e executado para todas as situaá‰es e nao apenas para quando houver adm-tableio-fields
    . Customizacao feita em funcao de Viewers so com campos chavers e/ou variaveis 
    Ricardo de Lima Perdigao - 05/05/1997 */


        /* If this object is linked to others to be updated together,
           then enable fields together: */
        RUN notify ('enable-fields, GROUP-ASSIGN-TARGET':U).

        RUN dispatch ('apply-entry':U). /*  Assure focus is in this object. */

   /* EPC - After Enable da Viewer */ 
   

    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-end-update :
/*------------------------------------------------------------------------------
  Purpose:   Does final update processing, including reopening the query
             on an add so that the new record becomes part of the query,
             and notifying others that a record has changed and that the 
             update is complete.  
  Parameters:  <none>
  Notes:     This is dispatched from adm-update-record if there is no 
             larger transaction active. Otherwise (with the Transaction
             Update Panel, for example) it must be invoked after the
             transaction is complete, because otherwise the query re-open
             may fail on some DataServers. 
------------------------------------------------------------------------------*/

  /* EPC - End-update da Viewer */ 
  


  /* record source is local - reopen if new */

  /* EPC - After End-update da Viewer */ 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-reset-record :
/* -----------------------------------------------------------  
      Purpose:     Redisplays values from the record buffer for the
                   current record.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/  

  /* EPC - Undo da Viewer */ 
  

  

  /* EPC - After Undo da Viewer */ 
  

    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-update-record :
/* -----------------------------------------------------------  
      Purpose:     Defines a transaction within which assign-record
                   commits changes to the current record. 
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/
    MESSAGE 
      "Object ":U THIS-PROCEDURE:FILE-NAME 
        "must have at least one Enabled Table to perform Update.":U
          VIEW-AS ALERT-BOX ERROR.
    

    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE check-modified :
/*------------------------------------------------------------------------------
  Purpose:     Either checks or clears the MODIFIED attribute of
               all the enabled widgets in this object. Done as part
               protecting users from losing updates in the record
               changes or the application exits.
  Parameters:  <none>
  Notes:       The code checks first to make sure the FRAME or BROWSE
               hasn't already been destroyed, and that the changed record
               is still available.
               If the CHECK-MODIFIED-ALL attribute is set to "YES"
               (maps to local variable adm-check-modified-all) then
               all fields will be checked; otherwise, by default,
               only enabled database record fields are checked.
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER check-state AS CHARACTER NO-UNDO.

DEFINE VARIABLE curr-widget       AS HANDLE      NO-UNDO.
DEFINE VARIABLE container-hdl-str AS CHARACTER   NO-UNDO.

/* Code for Browsers *//* ALTERAÄ«O FEITA PARA ATENDER A VERSAO 10 - MARCILENE - 26-11-2003 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE get-rowid :
/* -----------------------------------------------------------
      Purpose:      Furnishes the rowid of the current record
                    or "previously current" record (in the event
                    of a cancelled Add or Copy, for example) to a requesting
                    procedure (typically reposition-query).
                    Note that the rowid is saved only for certain update
                    operations, in order to allow repositioning after the
                    update is complete or has been cancelled. get-rowid 
                    should not be used as a general way to get the ROWID
                    of the current record. send-records should be used instead.
      Parameters:   OUTPUT record rowid.
      Notes:
    -------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER p-table           AS ROWID NO-UNDO.

    ASSIGN
    p-table   =   adm-first-table.    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE set-editors :
/* -----------------------------------------------------------
      Purpose:      Set the attributes of editor widgets properly.
                    They must be SENSITIVE AND READ-ONLY if disabled,
                    ELSE SENSITIVE AND not READ-ONLY. Otherwise they will
                    be unscrollable and possibly the text will be invisible
                    when they are disabled. Also used (by Add) to
                    clear any non-enabled editors (since we can't yet display
                    initial values into non-fill-ins).
      Parameters:   INPUT field-setting ("INITIALIZE" OR 
                    "ENABLED" or "DISABLED" or "CLEAR").
      Notes:        The checks are made only for editor widgets which are
                    mapped to database fields. In addition, a list is built
                    during 'initialize' of any editors whose initial state
                    is READ-ONLY, so that these are not enabled later on.
    -------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-field-setting  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE curr-widget             AS HANDLE    NO-UNDO.
    DEFINE VARIABLE read-only-list          AS CHARACTER NO-UNDO INIT "":U.

    ASSIGN curr-widget = FRAME F-Main:CURRENT-ITERATION. /* Field group*/
    ASSIGN curr-widget = curr-widget:FIRST-CHILD. /* First field */
    DO WHILE VALID-HANDLE (curr-widget):
        IF curr-widget:TYPE = "EDITOR":U AND curr-widget:TABLE NE ? AND
           curr-widget:HIDDEN = no THEN DO:
          CASE p-field-setting:
            WHEN "INITIALIZE":U THEN
            /* If any editor widgets have been marked as READ-ONLY then
               put them into an attribute list and leave them alone later. */
            DO:
              IF curr-widget:READ-ONLY = yes THEN read-only-list =
                  read-only-list + 
                    (IF read-only-list NE "":U THEN ",":U ELSE "":U) +
                     STRING(curr-widget).
            END.
            WHEN "DISABLE":U OR
            WHEN "ENABLE":U THEN
            DO:
                curr-widget:SENSITIVE = yes.  /* ALlow scrolling in any case.*/
                RUN get-attribute ('Read-Only-Editors':U).
                IF RETURN-VALUE = ? OR
                  LOOKUP (STRING(curr-widget), RETURN-VALUE) EQ 0 THEN 
                    curr-widget:READ-ONLY = 
                      IF p-field-setting = "ENABLE":U THEN no ELSE yes.
            END.
            WHEN "CLEAR":U THEN
                curr-widget:SCREEN-VALUE = "":U.    /* Clear for Add */
          END CASE.
        END.
        ASSIGN curr-widget = curr-widget:NEXT-SIBLING.
    END.

    IF p-field-setting = "INITIALIZE":U AND read-only-list NE "":U THEN
      RUN set-attribute-list ('Read-Only-Editors = "':U + read-only-list 
        + '"':U).

    RETURN.

END PROCEDURE.
/* end of &IF not defined adm-viewer */

/* _UIB-CODE-BLOCK-END */







PROCEDURE use-check-modified-all :
/*------------------------------------------------------------------------------
  Purpose:     Sets a variable whenever the 'Check-Modified-All'
               attribute is set, indicating whether the check-modified
               procedure should check just fields which are in database records
               (the default) or all enabled fields (Check-modified-all = yes).
  Parameters:  attribute value - "YES" means check all enabled fields.
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER p-attr-value AS CHARACTER NO-UNDO.

  ASSIGN adm-check-modified-all = IF p-attr-value = "YES":U THEN yes ELSE no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE use-create-on-add :
/*------------------------------------------------------------------------------
  Purpose:     Stores the value of the Create-On-Add attribute whenever
               it is set.
  Parameters:  attribute value
  Notes:       This attribute tells whether the developer wants a CREATE done
               when the Add button is pressed. 
               The default for Progress DBs is no -
               the Create is done when the record is Saved. 
               The default for non-Progress DBs is yes, because 
               there is no other way to display initial values properly. 
               Because the default (Unknown) value thus can be 
               interpreted differently depending on the source of the table,
               the adm-create-on-add variable is set explicitly to yes or no. 
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-attr-value AS CHARACTER NO-UNDO.

    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE use-initial-lock :
/*------------------------------------------------------------------------------
  Purpose:   Sets the local variable adm-initial-lock whenever the
             INITIAL-LOCK attribute is set for an object. This preserves
             compatibility with code in 8.0A which looks at adm-initial-lock,
             and saves the overhead of running get-attribute('INITIAL-LOCK')
             every time a record is read, since this attribute is normally
             set only once, during initialization.  
  Parameters:  attribute value: NO-LOCK, SHARE-LOCK, or EXCLUSIVE-LOCK
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER p-attr-value AS CHARACTER NO-UNDO.

  ASSIGN adm-initial-lock = p-attr-value.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */




/* ************************  Function Implementations ***************** */




FUNCTION setInitial RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Setar valores iniciais, na inclus∆o (ADD) quando o tipo de Database 
           for diferente de PROGRESS (por exemplo: Oracle, Sysbase, ...)
    Notes: 
------------------------------------------------------------------------------*/
  
  
  

  RETURN TRUE.
END FUNCTION.

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
    
    
 
      IF AVAILABLE lote-carac-tec THEN
         DISPLAY lote-carac-tec.cd-folha lote-carac-tec.cd-comp WITH BROWSE br-table
            NO-ERROR.
    
      DISPLAY UNLESS-HIDDEN cb-folha fi-num-min fi-num-max fi-un-num fi-resultado-num fi-complemento fi-observacao ed-texto fi-dat-min fi-dat-max fi-resultado-dat 
          WITH FRAME F-Main NO-ERROR.
    
  
    
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

     /***************************************************************
**
** i-epc035.i - EPC para Evento Before OPEN-QUERY de SmartBrowser
**
***************************************************************/

def var c-container as char no-undo.
run Who-Is-The-Container in adm-broker-hdl
    (input THIS-PROCEDURE,
     output c-container).

     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

if  valid-handle(widget-handle(c-container)) then do:
    
     /* DPC */
    run pi-retorna-dpc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-dpc-mg97 = return-value.
        if  c-nom-prog-dpc-mg97 <> "" then do:
            run value(c-nom-prog-dpc-mg97) (input "BEFORE-OPEN-QUERY":U,
                                            input "BROWSER":U,
                                            input THIS-PROCEDURE,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
     /* APPC */
    run pi-retorna-appc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-appc-mg97 = return-value.
        if  c-nom-prog-appc-mg97 <> "" then do:
            run value(c-nom-prog-appc-mg97) (input "BEFORE-OPEN-QUERY":U,
                                            input "BROWSER":U,
                                            input THIS-PROCEDURE,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
    
     /* UPC */
    run pi-retorna-upc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-upc-mg97 = return-value.
        if  c-nom-prog-upc-mg97 <> "" then do:
            run value(c-nom-prog-upc-mg97) (input "BEFORE-OPEN-QUERY":U,
                                            input "BROWSER":U,
                                            input THIS-PROCEDURE,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
end.
/* i-epc035.i */

 



  
    IF AVAILABLE(lote-item) THEN DO:
  
        
            OPEN QUERY br-table FOR EACH lote-carac-tec WHERE lote-carac-tec.it-codigo = lote-item.it-codigo   AND lote-carac-tec.lote = lote-item.lote NO-LOCK INDEXED-REPOSITION.
        
        adm-query-opened = yes.
        IF NUM-RESULTS("br-table":U) = 0 THEN /* query's empty */
            RUN new-state ('no-record-available,SELF':U).
        ELSE 
            RUN new-state ('record-available,SELF':U). 
  
    END. 
    ELSE
    DO:
        CLOSE QUERY br-table.
        /* Tell others that there's an external dependency not available. */
        RUN new-state ('no-external-record-available,SELF':U).
    END.
  
    IF NOT adm-updating-record THEN /* Suppress if in the middle of update*/
        RUN dispatch IN THIS-PROCEDURE ('row-changed':U).
    
    DEFINE VARIABLE tt-uib-container-source-char AS CHARACTER NO-UNDO.
    DEFINE VARIABLE tt-uib-container-source-hdl  AS WIDGET-HANDLE NO-UNDO.
    
    IF THIS-PROCEDURE:GET-SIGNATURE("ApplyFillIn") <> "":U AND
       tt-uib-browser-view = ? THEN DO:
       RUN get-link-handle IN ADM-BROKER-HDL (THIS-PROCEDURE, "CONTAINER-SOURCE":U, OUTPUT tt-uib-container-source-char).
       ASSIGN tt-uib-container-source-hdl = WIDGET-HANDLE(tt-uib-container-source-char). 
       
       IF VALID-HANDLE(tt-uib-container-source-hdl) THEN DO:
          RUN set-attribute-list IN tt-uib-container-source-hdl ("ApplyFillIn=YES|":U + STRING(THIS-PROCEDURE)).
          
          IF tt-uib-browser-view = ? THEN
             RUN ApplyFillIn.
          
          ASSIGN tt-uib-browser-view = NO.
       END.
    END.
    


/* EPC - Open Query do Browser */

     /***************************************************************
**
** i-epc036.i - EPC para Evento OPEN-QUERY de SmartBrowser
**
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

if  valid-handle(widget-handle(c-container)) then do:
    
     /* DPC */
    run pi-retorna-dpc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-dpc-mg97 = return-value.
        if  c-nom-prog-dpc-mg97 <> "" then do:
            run value(c-nom-prog-dpc-mg97) (input "AFTER-OPEN-QUERY":U,
                                            input "BROWSER":U,
                                            input THIS-PROCEDURE,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
     /* APPC */
    run pi-retorna-appc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-appc-mg97 = return-value.
        if  c-nom-prog-appc-mg97 <> "" then do:
            run value(c-nom-prog-appc-mg97) (input "AFTER-OPEN-QUERY":U,
                                            input "BROWSER":U,
                                            input THIS-PROCEDURE,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
    
     /* UPC */
    run pi-retorna-upc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-upc-mg97 = return-value.
        if  c-nom-prog-upc-mg97 <> "" then do:
            run value(c-nom-prog-upc-mg97) (input "AFTER-OPEN-QUERY":U,
                                            input "BROWSER":U,
                                            input THIS-PROCEDURE,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
end.
/* i-epc036.i */

 



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



    DEFINE VARIABLE table-name                 AS ROWID NO-UNDO.
    
    RUN get-rowid IN p-requestor-hdl (OUTPUT table-name).
    /* Note: row-changed was removed from this (after reposition)
       because the update-complete state will take care of that. */
    IF table-name <> ? THEN
        REPOSITION br-table TO ROWID table-name NO-ERROR.


    /* In case this attribute was set earlier, turn it off. */
    RUN set-attribute-list ('REPOSITION-PENDING = NO':U).

    RETURN.    
  
END PROCEDURE.



/* _UIB-CODE-BLOCK-END */




 

/* _UIB-CODE-BLOCK-END */





/* ************************  Control Triggers  ************************ */





/* ***************************  Main Block  *************************** */

/* Keep newly added entries from being at the top of the viewport. */
  adm-sts = br-table:SET-REPOSITIONED-ROW
    (br-table:DOWN,"CONDITIONAL":U). 

  /* Initialize attributes for update processing objects. */
  RUN set-attribute-list ('FIELDS-ENABLED=no,ADM-NEW-RECORD=no':U).



/* _UIB-CODE-BLOCK-END */



 

/* Procedure Description
"Biblioteca para customizaªío dos browses"
*/


/*--------------------------------------------------------------------------
    Library     : c-browse.i
    Purpose     : Implementar customizaªÑes nos browsers

    Syntax      : {include/c-browse.i}

    Description : Method-Library criada para fornecer customizaªío para
                  os browses a serem utilizadas pelos programas do
                  Magnus97

    Author(s)   : Weber
    Created     : 12/01/1997
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define variable wh-atributo     as handle    no-undo.
define variable wh-programa     as handle    no-undo.
def new global shared var r-registro-atual as rowid     no-undo.
def new global shared var gr-lote-carac-tec as rowid no-undo.
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
         HEIGHT             = .57
         WIDTH              = 40.86.
 /* END WINDOW DEFINITION */
                                                                        */



/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */


/* ***************************  Main Block  *************************** */

RUN modify-list-attribute IN adm-broker-hdl
        (INPUT THIS-PROCEDURE,
         INPUT "ADD":U,
         INPUT "ADM-ATTRIBUTE-LIST":U,
         INPUT "ProgAtributo":U).

RUN modify-list-attribute IN adm-broker-hdl
        (INPUT THIS-PROCEDURE,
         INPUT "ADD":U,
         INPUT "ADM-ATTRIBUTE-LIST":U,
         INPUT "ProgIncMod":U).
/* _UIB-CODE-BLOCK-END */

/* **********************  Internal Procedures  *********************** */



PROCEDURE pi-atributo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN get-attribute ('ProgAtributo':U).
    IF  RETURN-VALUE <>  ? AND RETURN-VALUE <> "":U THEN DO:
        GET CURRENT br-table.
        IF AVAIL lote-carac-tec THEN DO:
           ASSIGN gr-lote-carac-tec = ROWID(lote-carac-tec).
           run value(RETURN-VALUE) persistent set wh-atributo.
           RUN dispatch in wh-atributo (input 'initialize':U).
        END.
    end.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */





PROCEDURE pi-detalhe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var rw-posicao as rowid no-undo.
    if  valid-handle(wh-atributo) then
        return.
    RUN get-attribute ('ProgAtributo':U).
    IF  RETURN-VALUE <>  ?
    AND RETURN-VALUE <> "":U 
    THEN DO:             
      run pi-posicao-browse (output rw-posicao).
      if rw-posicao <> ? then do:
         run value(RETURN-VALUE) persistent set wh-atributo.
         if valid-handle(wh-atributo) then do:
           RUN dispatch in wh-atributo (input 'initialize':U).  
           run pi-reposiciona in wh-atributo (input-output rw-posicao).
         end.  
      end.   
    end.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */




PROCEDURE pi-eliminar :
/*------------------------------------------------------------------------------
  Purpose:     Devolve a posicao do browse
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

if valid-handle(wh-programa) then return.
if avail lote-carac-tec then do with frame F-Main:
    


/*--------------------------------------------------------------------------
    File        : confdel.i
    Purpose     : Pedir confirmaá∆o de eliminaá∆o

    Syntax      : {include/confdel.i}

    Description : Pede a confirmaá∆o de uma eliminaá∆o

    Author(s)   : Vanei
    Created     : 13/01/1997
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

DEFINE VARIABLE l-resposta  AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE i-msgnumber AS INTEGER            NO-UNDO.
DEFINE VARIABLE c-msgparam  AS CHARACTER          NO-UNDO.

RUN get-attribute IN THIS-PROCEDURE (INPUT "MessageNum":U).
ASSIGN i-msgnumber = INTEGER(RETURN-VALUE).

IF i-msgnumber <> ? AND i-msgnumber <> 0 THEN DO:
    RUN get-attribute IN THIS-PROCEDURE (INPUT "MessageParam":U).
    ASSIGN c-msgparam = STRING(RETURN-VALUE).
    
    RUN utp/ut-msgs.p (INPUT "SHOW":U, INPUT i-msgnumber, INPUT c-msgparam).
END.
ELSE
    RUN utp/ut-msgs.p (INPUT "SHOW":U, INPUT 550, INPUT "":U).

IF RETURN-VALUE = "YES":U THEN
    ASSIGN l-resposta = yes.
ELSE 
    ASSIGN l-resposta = no.

/* _UIB-CODE-BLOCK-END */



 
    if l-resposta then do:
        delete lote-carac-tec.
        if br-table:DELETE-CURRENT-ROW() then .

        find current lote-carac-tec no-lock no-error.
        if avail lote-carac-tec 
        then run new-state ("record-available, SELF":U).
        else run new-state ("no-record-available, SELF":U).

        return "OK":U.
    end.
    else return "NOK":U.
end.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */




PROCEDURE pi-posicao-browse :
/*------------------------------------------------------------------------------
  Purpose:     Devolve a posicao do browse
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def output parameter rw-posicao-browse as rowid no-undo.
    assign rw-posicao-browse = rowid(lote-carac-tec).

END PROCEDURE.
/* _UIB-CODE-BLOCK-END */




PROCEDURE pi-reposiciona-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-row-table AS ROWID NO-UNDO.

/************ Customizacao Datasul S.A. - John Cleber Jaraceski - 02/07/1999
              Implementar novas caracteristicas para o reposicionamento de  
              registros em browse devido a deficiencia dos Templates PROGRESS 
              com DATABASE DIFERENTE DE PROGRESS
              **********************************/
    
        REPOSITION br-table TO ROWID p-row-table NO-ERROR.
        RUN dispatch IN THIS-PROCEDURE ('get-next':U).
    
/***********************************************/

END PROCEDURE.
/* _UIB-CODE-BLOCK-END */




PROCEDURE pi-trata-state :
/*------------------------------------------------------------------------------
  Purpose:     Trata mundaªas de estado no browse
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter p-issuer-hdl as handle no-undo.
define input parameter p-state      as char   no-undo.
define var wh-objeto as widget-handle no-undo.
case p-state:
    when "Atributo":U then do:
        run pi-atributo.
    end.
    when "no-record-available":U then do:
        assign wh-objeto = br-table:HANDLE in frame F-Main.
        do while valid-handle(wh-objeto):
            if wh-objeto:NAME = "bt-idet":U or
               wh-objeto:NAME = "bt-detalhar":U then
                assign wh-objeto:SENSITIVE = no.

            if wh-objeto:TYPE = "field-group":U 
            then assign wh-objeto = wh-objeto:FIRST-CHILD.
            else assign wh-objeto = wh-objeto:NEXT-SIBLING.
        end.
    end.
    when "no-external-record-available":U then do:
        assign wh-objeto = br-table:HANDLE in frame F-Main.
        do while valid-handle(wh-objeto):
            if wh-objeto:NAME = "bt-idet":U or
               wh-objeto:NAME = "bt-detalhar":U then
                assign wh-objeto:SENSITIVE = no.

            if wh-objeto:TYPE = "field-group":U 
            then assign wh-objeto = wh-objeto:FIRST-CHILD.
            else assign wh-objeto = wh-objeto:NEXT-SIBLING.
        end.
    end.
    when "record-available":U then do:
        assign wh-objeto = br-table:HANDLE in frame F-Main.
        do while valid-handle(wh-objeto):
            if wh-objeto:NAME = "bt-idet":U or
               wh-objeto:NAME = "bt-detalhar":U then
                assign wh-objeto:SENSITIVE = yes.

            if wh-objeto:TYPE = "field-group":U 
            then assign wh-objeto = wh-objeto:FIRST-CHILD.
            else assign wh-objeto = wh-objeto:NEXT-SIBLING.
        end.
    end.
    when "dblclick":U then do:
        assign wh-objeto = br-table:HANDLE in frame F-Main.
        do while valid-handle(wh-objeto):
            if wh-objeto:NAME = "bt-idet":U or
               wh-objeto:NAME = "bt-detalhar":U then
                leave.

            if wh-objeto:TYPE = "field-group":U 
            then assign wh-objeto = wh-objeto:FIRST-CHILD.
            else assign wh-objeto = wh-objeto:NEXT-SIBLING.
        end.

        if valid-handle(wh-objeto) then
            apply "CHOOSE":U to wh-objeto.
    end.
end case.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */



 
/***************************************************************************
**  Programa : UT-GLOB.I
**  Include padr„o para definiÁ„o de variaveis globais.
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
     
      /* Vari·veis Padr„o DWB / Datasul HR */
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

 

/* _UIB-CODE-BLOCK-END */





/* ***********  Runtime Attributes and AppBuilder Settings  *********** */


/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit L-To-R                                       */
/* BROWSE-TAB br-table fi-observacao F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br-table:HIDDEN  IN FRAME F-Main                = TRUE.

/* SETTINGS FOR EDITOR ed-texto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-complemento IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dat-max IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dat-min IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-num-max IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-num-min IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-observacao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-resultado-dat IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-resultado-num IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-un-num IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */



/* Setting information for Queries and Browse Widgets fields            */


/* Query rebuild information for BROWSE br-table
     _TblList          = "svamfg.lote-carac-tec WHERE svamfg.lote-item <external> ... ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _JoinCode[1]      = "svamfg.lote-carac-tec.it-codigo = svamfg.lote-item.it-codigo
  AND svamfg.lote-carac-tec.lote = svamfg.lote-item.lote"
     _FldNameList[1]   = svamfg.lote-carac-tec.cd-folha
     _FldNameList[2]   = svamfg.lote-carac-tec.cd-comp
     _Query            is NOT OPENED
*/  /* BROWSE br-table */



/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */


 


/* **********************  Create OCX Containers  ********************** */





CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME F-Main:HANDLE
       ROW             = 2.04
       COLUMN          = 1.57
       HEIGHT          = 14.96
       WIDTH           = 36.86
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME F-Main:HANDLE
       ROW             = 11.5
       COLUMN          = 31
       HEIGHT          = 1.58
       WIDTH           = 5.43
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {0713E8A2-850A-101B-AFC0-4210102A8DA7} type: TreeView */
      CtrlFrame-2:NAME = "CtrlFrame-2":U .
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {58DA8D8F-9D6A-101B-AFC0-4210102A8DA7} type: ImageList */
      CtrlFrame:MOVE-AFTER(cb-folha:HANDLE IN FRAME F-Main).
      CtrlFrame-2:MOVE-AFTER(ed-texto:HANDLE IN FRAME F-Main).

END PROCEDURE.



/* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */


ON MOUSE-SELECT-DBLCLICK OF br-table IN FRAME F-Main
DO:
    RUN New-State("DblClick, SELF":U).
END.

/* _UIB-CODE-BLOCK-END */




ON ROW-ENTRY OF br-table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  /* brsentry.i - trigger code for ROW-ENTRY of SmartBrowse */
/* For the sake of backward compatibility with 8.0, FIRST-ENABLED-TABLE
   is mapped to ENABLED-TABLES if it is otherwise undefined. */

  

   
END.

/* _UIB-CODE-BLOCK-END */




ON ROW-LEAVE OF br-table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   /* brsleave.i - trigger code for ROW-LEAVE trigger of SmartBrowse*/
/* If the object selected is not a SmartPanel button 
   (which could be e.g. Cancel or Reset), then save any changes to the row. 
   Otherwise let the button take the appropriate action. */
DEFINE VARIABLE widget-enter  AS HANDLE NO-UNDO.
DEFINE VARIABLE widget-frame  AS HANDLE NO-UNDO.
DEFINE VARIABLE widget-parent AS HANDLE NO-UNDO.
  /* If the object has a valid frame attribute, see if it's a SmartPanel. */
  widget-enter = last-event:widget-enter.
  IF VALID-HANDLE(widget-enter) THEN widget-parent = widget-enter:PARENT.
  IF VALID-HANDLE(widget-parent) AND widget-parent:TYPE NE "BROWSE":U
    THEN widget-frame = widget-enter:FRAME.  /* Can't check FRAME on Brs flds */

  IF ((NOT VALID-HANDLE(widget-enter)) OR  /* Some events don't go to a widget*/
      (widget-parent:TYPE = "BROWSE":U) OR /* Clicked elsewhere in the Browser*/
      (NOT VALID-HANDLE(widget-frame)) OR  /* Check parent Frame if present */
      (NOT CAN-DO(widget-frame:PRIVATE-DATA, "ADM-PANEL":U))) /*SmartPanel?*/
  THEN DO:                                 /* If not a SmartPanel then do upd */

      IF adm-brs-in-update THEN    
      DO:
        MESSAGE 
        "You must complete or cancel the update before leaving the current row."
            VIEW-AS ALERT-BOX WARNING.
        RETURN NO-APPLY.
      END.
      /* If they selected some other object or the LEAVE was initiated 
         from outside then check before continuing. Otherwise just save. 
         If they were adding a new record and didn't change any initial values,
         make sure that gets Saved as well. */
      IF br-table:CURRENT-ROW-MODIFIED  OR 
        (adm-new-record AND BROWSE br-table:NUM-SELECTED-ROWS = 1) THEN
      DO:
        IF VALID-HANDLE (widget-parent) THEN
          IF CAN-QUERY(widget-parent, "TYPE":U) AND (widget-parent:TYPE = "BROWSE") THEN DO:
            RUN dispatch('update-record':U).
            IF RETURN-VALUE = "ADM-ERROR":U THEN 
                RETURN NO-APPLY.
        END.
      END.
  END.
 
END.

/* _UIB-CODE-BLOCK-END */




ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  /* brschnge.i - trigger code for VALUE-CHANGED trigger of SmartBrowse */

/* EPC - VALUE-CHANGED PARA SMARTBROWSER */ 

     /***************************************************************
**
** i-epc016.i - EPC para Evento VALUE-CHANGE de SmartBrowser
**
***************************************************************/

def var c-container  as char    no-undo.
run Who-Is-The-Container in adm-broker-hdl
    (input this-procedure,
     output c-container).

     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

if  valid-handle(widget-handle(c-container)) then do:
    
     /* DPC */
    run pi-retorna-dpc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-dpc-mg97 = return-value.
        if  c-nom-prog-dpc-mg97 <> "" then do:
            run value(c-nom-prog-dpc-mg97) (input "VALUE-CHANGED":U,
                                            input "BROWSER":U,
                                            input this-procedure,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
     /* APPC */
    run pi-retorna-appc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-appc-mg97 = return-value.
        if  c-nom-prog-appc-mg97 <> "" then do:
            run value(c-nom-prog-appc-mg97) (input "VALUE-CHANGED":U,
                                            input "BROWSER":U,
                                            input this-procedure,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
    
     /* UPC */
    run pi-retorna-upc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-upc-mg97 = return-value.
        if  c-nom-prog-upc-mg97 <> "" then do:
            run value(c-nom-prog-upc-mg97) (input "VALUE-CHANGED":U,
                                            input "BROWSER":U,
                                            input this-procedure,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
end.
/* i-epc016.i */
 


RUN get-attribute('ADM-NEW-RECORD':U). /* If this is triggered by initial*/
IF RETURN-VALUE NE "YES":U THEN        /*  values in an Add, ignore it. */
   RUN notify ('row-available':U).

/* EPC - After VALUE-CHANGED PARA SMARTBROWSER */ 

     /***************************************************************
**
** i-epc016.i - EPC para Evento After VALUE-CHANGE de SmartBrowser
**
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

if  valid-handle(widget-handle(c-container)) then do:
    
     /* DPC */
    run pi-retorna-dpc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-dpc-mg97 = return-value.
        if  c-nom-prog-dpc-mg97 <> "" then do:
            run value(c-nom-prog-dpc-mg97) (input "AFTER-VALUE-CHANGED":U,
                                            input "BROWSER":U,
                                            input this-procedure,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
     /* APPC */
    run pi-retorna-appc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-appc-mg97 = return-value.
        if  c-nom-prog-appc-mg97 <> "" then do:
            run value(c-nom-prog-appc-mg97) (input "AFTER-VALUE-CHANGED":U,
                                            input "BROWSER":U,
                                            input this-procedure,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
    
     /* UPC */
    run pi-retorna-upc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-upc-mg97 = return-value.
        if  c-nom-prog-upc-mg97 <> "" then do:
            run value(c-nom-prog-upc-mg97) (input "AFTER-VALUE-CHANGED":U,
                                            input "BROWSER":U,
                                            input this-procedure,
                                            input frame F-Main:handle,
                                            input "lote-carac-tec",
            
                                            input (if  avail lote-carac-tec then rowid(lote-carac-tec) else ?)).
            
        end.
    end.
end.
/* i-epc016.i */
 

 
  /* run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))). */
END.

/* _UIB-CODE-BLOCK-END */




ON VALUE-CHANGED OF cb-folha IN FRAME F-Main /* Folha Lote */
DO:
  run pi-monta-treeview (input cb-folha:screen-value in frame F-Main).
  find item
      where item.it-codigo = lote-item.it-codigo no-lock no-error.
  if avail item then do:
     if item.cd-folh-lote = input cb-folha:screen-value in frame F-Main then
        assign l-folha-corrente = yes.
     else
        assign l-folha-corrente = no.
  end.
  else
     assign l-folha-corrente = no.
END.

/* _UIB-CODE-BLOCK-END */




PROCEDURE CtrlFrame.TreeView.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
if chTreeView:nodes:count > 0 then do:
   if chTreeView:SelectedItem:image < 7 then do:
      assign i-index = chTreeView:SelectedItem:index.
      find first tt-registros
           where tt-registros.i-seq = i-index no-lock no-error.
      if avail tt-registros then do:
         run pi-valores.
         do with frame F-Main:
            assign fi-resultado-num:sensitive = no
                   fi-complemento:sensitive   = no
                   fi-observacao:sensitive    = no
                   ed-texto:sensitive         = yes 
                   ed-texto:read-only         = yes
                   fi-resultado-dat:sensitive = no.
         end.
      end.
   end.
   if chTreeView:SelectedItem:image = 7 or
      chTreeView:SelectedItem:image = 8 then do:
      run pi-desabilita-dados.
   end.

end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */




PROCEDURE CtrlFrame.TreeView.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
     
    if chTreeView:nodes:count > 0 then do:
   if chTreeView:SelectedItem:image >= 7 and
      chTreeView:SelectedItem:image <= 8 then do:
      run pi-valida-componente.
   end.
   else do:
      run pi-edita.
   end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */




PROCEDURE CtrlFrame.TreeView.KeyPress .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    KeyAscii
  Notes:       
------------------------------------------------------------------------------*/

define input-output parameter p-KeyAscii as integer no-undo.

assign chTreeViewNode = ?.

if chTreeView:Nodes:count > 0 and 
   p-KeyAscii = 13 or p-KeyAscii = 32 then do:

   run pi-edita.

end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */




PROCEDURE CtrlFrame.TreeView.KeyUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    KeyCode
    Shift
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT-OUTPUT PARAMETER p-KeyCode AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER p-Shift   AS INTEGER NO-UNDO.

if p-KeyCode = 38 or
   p-KeyCode = 40 then do:
   assign i-index = chTreeView:SelectedItem:index.
   find first tt-registros
        where tt-registros.i-seq = i-index no-lock no-error.
   if avail tt-registros then do:
      run pi-valores.
      do with frame F-Main:
         assign fi-resultado-num:sensitive = no
                fi-complemento:sensitive   = no
                fi-observacao:sensitive    = no
                ed-texto:sensitive         = yes
                ed-texto:read-only         = yes
                fi-resultado-dat:sensitive = no.
      end.
   end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */




ON F4 OF ed-texto IN FRAME F-Main
DO:
  run pi-exit (self:handle).
  if return-value = "NOK":U then
     return no-apply.
  apply 'entry' to CtrlFrame.
END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF ed-texto IN FRAME F-Main
DO:
  run pi-exit (self:handle).
  if return-value = "NOK":U then
     return no-apply.
  apply 'entry' to CtrlFrame.
END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF fi-complemento IN FRAME F-Main
DO:
  run pi-exit (self:handle).
  if return-value = "NOK":U then
     return no-apply.
  apply 'entry' to CtrlFrame.
END.

/* _UIB-CODE-BLOCK-END */




ON RETURN OF fi-complemento IN FRAME F-Main
DO:
  run pi-exit (self:handle).
  if return-value = "NOK":U then
     return no-apply.
  apply 'entry' to CtrlFrame.
END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF fi-observacao IN FRAME F-Main
DO:
  run pi-exit (self:handle).
  if return-value = "NOK":U then
     return no-apply.
  apply 'entry' to CtrlFrame.
END.

/* _UIB-CODE-BLOCK-END */




ON RETURN OF fi-observacao IN FRAME F-Main
DO:
  run pi-exit (self:handle).
  if return-value = "NOK":U then
     return no-apply.
  apply 'entry' to CtrlFrame.
END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF fi-resultado-dat IN FRAME F-Main /* Resultado */
DO:
  run pi-exit (self:handle).
  if return-value = "NOK":U then
     return no-apply.
  apply 'entry' to CtrlFrame.
END.

/* _UIB-CODE-BLOCK-END */




ON RETURN OF fi-resultado-dat IN FRAME F-Main /* Resultado */
DO:
  run pi-exit (self:handle).
  if return-value = "NOK":U then
     return no-apply.
  apply 'entry' to CtrlFrame.
END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF fi-resultado-num IN FRAME F-Main /* Resultado */
DO:
  run pi-exit (self:handle).
  if return-value = "NOK":U then
     return no-apply.
  apply 'entry' to CtrlFrame.
  
END.

/* _UIB-CODE-BLOCK-END */




ON RETURN OF fi-resultado-num IN FRAME F-Main /* Resultado */
DO:
  run pi-exit (self:handle).
  if return-value = "NOK":U then
     return no-apply.
  apply 'entry' to CtrlFrame.
END.

/* _UIB-CODE-BLOCK-END */







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
 

  /* Create a list of all the tables that we need to get.            */
  /* row-list.i - 4/15/96 */

  IF key-name eq ? THEN tbl-list = "lote-item":U.    

 

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  /* row-get.i - 8/07/96 */
  
      RUN send-records IN record-source-hdl
          (INPUT tbl-list, OUTPUT rowid-list) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN.  /* send-records not defined. */
  
 

  /* FIND each record specified by the RECORD-SOURCE.                */
  /* row-find.i  */

  IF key-name ne ?         /* Don't find first external table */
                           /* by rowid, if a key-field exists */
  THEN DO:
    RUN dispatch ('find-using-key':U). 
    IF RETURN-VALUE eq "ADM-ERROR":U THEN RETURN RETURN-VALUE.
  END.
  ELSE

  DO:
    row-avail-cntr = row-avail-cntr + 1.
    row-avail-rowid = TO-ROWID(ENTRY(row-avail-cntr,rowid-list)).
    IF row-avail-rowid NE ROWID(lote-item) THEN different-row = yes.
    IF row-avail-rowid ne ? THEN DO:
      /* Change record-available state only for enabled tables, 
         because it affects the state of Update panel buttons. */
      
            /* find-tbl.i - */
       DO:
         IF row-avail-enabled AND            /* fields are enabled */ 
            (adm-initial-lock = "SHARE-LOCK":U OR  
             adm-initial-lock = "EXCLUSIVE-LOCK":U) THEN /* +++ new for EXCL*/
         DO:
           IF adm-initial-lock = "SHARE-LOCK":U        
           THEN FIND lote-item WHERE ROWID(lote-item) = row-avail-rowid SHARE-LOCK NO-ERROR.
           ELSE DO TRANSACTION:
             /* For EXCLUSIVE-LOCK we get the lock momentarily to assure
                that no-one else can deadlock with us. Then the lock is
                downgraded to share-lock until the record is saved. */  
             FIND lote-item WHERE ROWID(lote-item) = row-avail-rowid EXCLUSIVE-LOCK NO-ERROR.               
           END. /* DO TRANSACTION... */
         END. /* IF...enabled... */
         ELSE FIND lote-item WHERE ROWID(lote-item) = row-avail-rowid  NO-LOCK NO-ERROR.
         IF ERROR-STATUS:ERROR THEN DO:
           RUN dispatch ('show-errors':U).
           RETURN "ADM-ERROR":U.
         END. /* IF...ERROR... */
       END. /* end of find-tbl.i */
      
    END. /* IF row-avail-rowid ne ? ... */
    ELSE DO:
         IF AVAILABLE lote-item THEN RELEASE lote-item.      /* Force NO-AVAILABLE */
         
           /* If this is a query object and the last row in the external
              table was deleted (rather than just moving to a new parent
              row with no rows for this query) then get open-query to CLOSE
              this object's dependent query. */
           IF NOT different-row THEN
             RUN dispatch('open-query').  
         
         /* Change record-available state only for enabled tables. */
         
     END.
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

  ASSIGN r-registro-atual = ROWID(lote-item).

 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/


DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "essf0013b01.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
  .
  RUN DISPATCH IN THIS-PROCEDURE("initialize-controls":U) NO-ERROR.
END.
ELSE MESSAGE "essf0013b01.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".



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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

assign cb-folha:list-items in frame F-Main = "".

find item 
    where item.it-codigo = lote-item.it-codigo no-lock no-error.

for each lote-carac-tec
   where lote-carac-tec.it-codigo = lote-item.it-codigo
     and lote-carac-tec.lote      = lote-item.lote
     break by lote-carac-tec.cd-folha descending:

     if first-of(lote-carac-tec.cd-folha) then do:
        cb-folha:add-last(lote-carac-tec.cd-folha).
        if lote-carac-tec.cd-folha = item.cd-folh-lote then 
           assign cb-folha:screen-value in frame F-Main = lote-carac-tec.cd-folha
                  l-folha-corrente                             = yes.
     end.
end.

if cb-folha:screen-value in frame F-Main= "" then
   disable cb-folha
         with frame F-Main.
else
   enable cb-folha
         with frame F-Main.

assign chTreeView  = ?
       chImageList = ?.

assign chTreeView  = chCtrlFrame:TreeView
       chImageList = chCtrlFrame-2:ImageList
       chTreeView:imageList = chImageList.

/*22/12/2003 - Speto*/
if gc-estado      = "TravaBarra":U and
   grw-lote-item <> ?  then
   run pi-carrega-valores-configuracao.
/*22/12/2003 - Speto*/

run pi-monta-treeview (input cb-folha:screen-value in frame F-Main).

apply "value-changed":U to cb-folha.

if chTreeView:nodes:count > 0 then
   chTreeView:nodes:item(1):selected = yes.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply 'value-changed':U to br-table in frame F-Main.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-carrega-valores-configuracao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def buffer b-lote-item for lote-item.
def buffer b-item      for item.

def buffer b-lote-carac-tec for lote-carac-tec.

def var i-refer as int no-undo.

    /*gc-estado      = "TravaBarra":U and
   grw-lote-item */

find first param-cp no-lock no-error.

find first b-lote-item
     where rowid(b-lote-item) = grw-lote-item no-lock no-error.
if avail b-lote-item then do:
   find b-item
        where b-item.it-codigo = b-lote-item.it-codigo no-lock no-error.

   find last movto-mat
       where movto-mat.it-codigo   = b-lote-item.it-codigo
         and movto-mat.lote        = b-lote-item.lote
         and movto-mat.esp-docto   = 1 
         and movto-mat.tipo-trans  = 1 
         and movto-mat.cod-depos  <> substring(param-cp.char-2,1,3)
         and movto-mat.quantidade  > 0 no-lock no-error. 
   find ord-prod where 
        ord-prod.nr-ord-produ = movto-mat.nr-ord-produ no-lock no-error.
 
   if avail movto-mat and 
      avail ord-prod  then do:
       for each b-lote-carac-tec
          where b-lote-carac-tec.it-codigo = b-lote-item.it-codigo
            and b-lote-carac-tec.lote      = b-lote-item.lote
            and b-lote-carac-tec.cd-folh   = b-item.cd-folh-lote exclusive-lock:
    
            assign i-refer = int(ord-prod.cod-refer) no-error.
            if error-status:error then
               assign i-refer = 0.

            find first var-result
                 where var-result.item-cotacao = b-lote-carac-tec.it-codigo
                   and var-result.nr-estrut    = i-refer
                   and var-result.nome-var     = b-lote-carac-tec.cd-comp no-lock no-error.
            if avail var-result then do:
               
               if var-result.ind-tipo-var      = 1 and      /*Configuracao*/
                  var-result.tipo-valor        = 2 then do: /*Variavel configurada*/

                   
                  if var-result.tipo-result       = 1 and
                     b-lote-carac-tec.tipo-result   = 1 AND
                      b-lote-carac-tec.vl-result = 0 THEN   DO: /*se j† tiver valor n∆o carrega do var-result*/ 
                      assign b-lote-carac-tec.vl-result = var-result.valor-dec.
                  END.
                     

                  if var-result.tipo-result       = 3 and  /*Tabela*/
                     b-lote-carac-tec.tipo-result   = 2 then do:
                     /*for each assign b-lote-carac-tec.vl-result = var-result.valor-dec.*/

                  end.
               end.
               IF   var-result.nome-var = "QTDBOB" 
               THEN ASSIGN b-lote-carac-tec.vl-result = 1.
            end.
    
       end.
   end.

end.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-desabilita-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

do with frame F-Main:
   assign fi-resultado-num:sensitive = no
          fi-complemento:sensitive   = no
          fi-observacao:sensitive    = no
          ed-texto:sensitive         = yes 
          ed-texto:read-only         = yes
          fi-resultado-dat:sensitive = no
          fi-num-min                 = 0
          fi-num-max                 = 0
          fi-un-num                  = ""
          fi-resultado-num           = 0
          fi-complemento             = ""
          fi-observacao              = ""
          ed-texto                   = ""
          fi-dat-min                 = ?
          fi-dat-max                 = ?
          fi-resultado-dat           = ?.
end.

run pi-display.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-display :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    disp fi-num-min
         fi-num-max
         fi-un-num
         fi-resultado-num
         fi-complemento  
         fi-observacao   
         ed-texto        
         fi-dat-min
         fi-dat-max
         fi-resultado-dat
        with frame F-Main.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-edita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   assign i-index = chTreeView:SelectedItem:index.
   find first tt-registros
        where tt-registros.i-seq = i-index no-lock no-error.
   if avail tt-registros then do:
      run pi-valores.
      case tt-registros.tipo:
        when 1 /*Numerico*/   then do: 
            apply "entry":U to fi-resultado-num in frame F-Main.
        end.
        when 2 /*Tabela*/     then do:
             if chTreeView:nodes:item(i-index):expanded = yes then
                chTreeView:nodes:item(i-index):expanded = no.
             else
                chTreeView:nodes:item(i-index):expanded = yes.
        end.
        when 3 /*Texto*/      then do:
            if l-folha-corrente = yes then do:
               assign ed-texto:sensitive in frame F-Main = yes
                      ed-texto:read-only in frame F-Main = no.
            end.
            apply "entry":U to ed-texto in frame F-Main.
        end.
        when 4 /*Observacao*/ then do:
            apply "entry":U to fi-observacao in frame F-Main.
        end.
        when 5 /*Titulo*/     then do:
            apply "entry":U to fi-complemento in frame F-Main.
        end.
        when 6 /*Data*/       then do:
            apply "entry":U to fi-resultado-dat in frame F-Main.
        end.
        when 7 or 
        when 8 then do:

            run pi-valida-componente.
            
        end.
      end case.
   end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-exit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  run pi-exit (self:handle).
  if return-value = "NOK":U then
     return no-apply.
------------------------------------------------------------------------------*/

def input parameter p-field as handle no-undo.

DEFINE BUFFER bm-tt-registros FOR tt-registros.
if not avail lote-carac-tec then do:
   run utp/ut-msgs.p (input "show":U,
                      input 17006,
                      input "Registro de Configuraá∆o do Lote n∆o est† dispon°vel!").
   assign p-field:sensitive = no.
   return "NOK":U.
end.
else 
   find current lote-carac-tec exclusive-lock no-error.



case p-field:name:
    when "fi-resultado-num" then do:
        if input frame F-Main fi-resultado-num < dec(input frame F-Main fi-num-min) or
           input frame F-Main fi-resultado-num > dec(input frame F-Main fi-num-max) then do:
           run utp/ut-msgs.p (input "show",
                              input 3662,
                              input "Resultado NumÇrico"                  + "~~" +
                                    input frame F-Main fi-num-min  + "~~" +
                                    input frame F-Main fi-num-max).
           return "NOK".
        end.
        else do:
           if avail lote-carac-tec then
              assign lote-carac-tec.vl-result = input frame F-Main fi-resultado-num.
        end.
    end.
    when "fi-complemento" then do:
        if avail lote-carac-tec then
           assign lote-carac-tec.complemento = input frame F-Main fi-complemento.
    end.
    when "fi-observacao" then do:
        IF lote-carac-tec.cd-comp = "TURMA" THEN DO:
            IF INDEX("ABCD",input frame F-Main fi-observacao) = 0  THEN  DO:
                run utp/ut-msgs.p (input "show",
                              input 3662,
                              input "TURMA"                  + "~~" +
                                    "A"  + "~~" +
                                    "D").
                 APPLY "ENTRY" TO p-field.
                 return "NOK".

            END.
        END.
        if avail lote-carac-tec then
           assign lote-carac-tec.observacao = input frame F-Main fi-observacao.
    end.
    when "ed-texto" then do:
        if avail lote-carac-tec   and 
           p-field:read-only = no then do:
           if not avail lote-msg-carac then do:
              create lote-msg-carac.
              assign lote-msg-carac.it-codigo  = lote-carac-tec.it-codigo
                     lote-msg-carac.lote       = lote-carac-tec.lote
                     lote-msg-carac.cd-comp    = lote-carac-tec.cd-comp
                     lote-msg-carac.cd-folha   = lote-carac-tec.cd-folha
                     lote-msg-carac.msg-exp    = input frame F-Main ed-texto
                     lote-carac-tec.observacao = substring(input frame F-Main ed-texto,1,20).
           end.
           else do:
              find current lote-msg-carac exclusive-lock no-error.
              if avail lote-msg-carac then
                 assign lote-msg-carac.msg-exp    = input frame F-Main ed-texto
                        lote-carac-tec.observacao = string(input frame F-Main ed-texto,"x(20)").
           end.
        end.
    end.
    when "fi-resultado-dat" then do:
        if input frame F-Main fi-resultado-dat < input frame F-Main fi-dat-min  or
           input frame F-Main fi-resultado-dat > input frame F-Main fi-dat-max  then do:
           run utp/ut-msgs.p (input "show",
                              input 3662,
                              input "Data"               + "~~" +
                                    string(input frame F-Main fi-dat-min, "99/99/9999")  + "~~" +
                                    string(input frame F-Main fi-dat-max, "99/99/9999")).
           return "NOK".
        end.
        else do:
           if avail lote-carac-tec then
              assign lote-carac-tec.dt-result = input frame F-Main fi-resultado-dat.
        end.
    end.
end case.


/* Edson - 03/04/2006 - para calculo da media da densidade otica
         existe um erro no cadstro DENS e DESN */


IF AVAIL lote-carac-tec AND (SUBSTRING(lote-carac-tec.cd-comp,1,4) = "DENS"
     OR SUBSTRING(lote-carac-tec.cd-comp,1,4) = "DESN") THEN
DO:
    ASSIGN d-mediaotica = 0
           i-mediaqtd   = 0.
    FOR EACH bm-tt-registros WHERE bm-tt-registros.otica <> "N" NO-LOCK.

          IF bm-tt-registros.otica = "R" THEN
          DO:
              FIND bm-lote-carac-tec WHERE rowid(bm-lote-carac-tec) = bm-tt-registros.r-rowid NO-LOCK NO-ERROR.
              IF AVAIL bm-lote-carac-tec AND bm-lote-carac-tec.vl-result > 0 THEN
                  ASSIGN d-mediaotica = d-mediaotica + bm-lote-carac-tec.vl-result
                         i-mediaqtd = i-mediaqtd + 1.
          END.
    END.
    IF i-mediaqtd > 0 THEN
    DO:
       FIND first bm-tt-registros WHERE bm-tt-registros.otica = "M" NO-LOCK NO-ERROR.
       IF AVAIL bm-tt-registros THEN
       DO:
          ASSIGN d-mediaotica = d-mediaotica / i-mediaqtd.
                 
          FIND bm-lote-carac-tec WHERE rowid(bm-lote-carac-tec) = bm-tt-registros.r-rowid EXCLUSIVE-LOCK.
          IF AVAIL bm-lote-carac-tec THEN
                 ASSIGN  bm-lote-carac-tec.vl-result = d-mediaotica.
          RELEASE  bm-lote-carac-tec.
             
       END.
    END.   
END. /* FIM DO IF PARA CALCULO DA MEDIA DE DENSIDADE OTICA */



IF AVAIL lote-carac-tec AND SUBSTRING(lote-carac-tec.cd-comp,1,6) = "DUREZA" THEN
DO:
    ASSIGN d-deltarho = 0
           d-durezamenor = 999999
           d-durezamaior = 0.
    FOR EACH bm-tt-registros WHERE bm-tt-registros.delta <> "N" NO-LOCK.

          IF bm-tt-registros.delta = "R" THEN
          DO:
              FIND bm-lote-carac-tec WHERE rowid(bm-lote-carac-tec) = bm-tt-registros.r-rowid NO-LOCK NO-ERROR.
              IF AVAIL bm-lote-carac-tec AND bm-lote-carac-tec.vl-result > 0 THEN DO:
                    IF bm-lote-carac-tec.vl-result > d-durezamaior THEN
                         ASSIGN d-durezamaior = bm-lote-carac-tec.vl-result.
                    IF bm-lote-carac-tec.vl-result > 0 AND bm-lote-carac-tec.vl-result < d-durezamenor THEN
                         ASSIGN d-durezamenor = bm-lote-carac-tec.vl-result.


              END.
          END.
    END.
     
    DO:
       FIND first bm-tt-registros WHERE bm-tt-registros.delta = "M" NO-LOCK NO-ERROR.
       IF AVAIL bm-tt-registros THEN
       DO:
          ASSIGN d-deltarho = IF d-durezamaior = 0 THEN 0 ELSE (d-durezamaior - d-durezamenor).
                 
          FIND bm-lote-carac-tec WHERE rowid(bm-lote-carac-tec) = bm-tt-registros.r-rowid EXCLUSIVE-LOCK.
          IF AVAIL bm-lote-carac-tec THEN
                 ASSIGN  bm-lote-carac-tec.vl-result = d-deltarho.
          RELEASE  bm-lote-carac-tec.
             
       END.
    END.   
END. /* FIM DO IF PARA CALCULO DA MEDIA DE DENSIDADE OTICA */



assign p-field:sensitive = no.

if p-field:name = "ed-texto" then
   assign p-field:read-only = yes
          p-field:sensitive = yes.


return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-expande-nodes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

if valid-handle(chTreeView) then do:
    do i-cont = 1 to chTreeView:nodes:count:
       if chTreeView:nodes:item(i-cont):expanded = no then
          chTreeView:nodes:item(i-cont):expanded = yes.
       else
          chTreeView:nodes:item(i-cont):expanded = no.
    end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-grava-componente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     def buffer b-tt-registros for tt-registros.

     assign i-cur = chTreeView:SelectedItem:index.
     find first b-tt-registros
          where b-tt-registros.i-seq = i-cur no-lock no-error.
     find first c-tab-res
          where rowid(c-tab-res) = b-tt-registros.r-rowid no-lock no-error.
     if avail c-tab-res then do:
        find first lote-res-carac
             where lote-res-carac.cd-comp    = lote-carac-tec.cd-comp 
               and lote-res-carac.cd-folha   = lote-carac-tec.cd-folh
               and lote-res-carac.it-codigo  = lote-carac-tec.it-codigo
               and lote-res-carac.lote       = lote-carac-tec.lote
               and lote-res-carac.nr-tabela  = lote-carac-tec.nr-tabela
               and lote-res-carac.sequencia  = c-tab-res.sequencia      exclusive-lock no-error.
     end.
     
     if chTreeView:SelectedItem:image = 7 then do:
        if avail lote-res-carac then
           delete lote-res-carac.
        
     end.
     else do:
         IF (ITEM.ge-codigo = 45 or ITEM.ge-codigo = 46) AND lote-carac-tec.cd-comp  = "DEFPRI" THEN      /*PRIMER N«O PODE TER DEFEITO PRIMARIO EDSON-AMGRA 29/04/2008*/
             chTreeView:SelectedItem:image = 7.
         ELSE DO:
            if not avail lote-res-carac then do:
               create lote-res-carac.
               assign lote-res-carac.cd-comp    = lote-carac-tec.cd-comp  
                      lote-res-carac.cd-folha   = lote-carac-tec.cd-folh  
                      lote-res-carac.it-codigo  = lote-carac-tec.it-codigo
                      lote-res-carac.lote       = lote-carac-tec.lote     
                      lote-res-carac.nr-tabela  = lote-carac-tec.nr-tabela
                      lote-res-carac.sequencia  = c-tab-res.sequencia.     
              
                
            end.
         END.

        
     end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-monta-treeview :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter p-folha as char no-undo.
  /*
  def temp-table tt-registros no-undo
      field i-seq   as integer
      field tipo    as integer 1-num,2-tab,3-txt,4-obs,5-tit,6-dat,7e8-comp
      field r-rowid as rowid.
  */

  chTreeView:nodes:clear.
  for each tt-registros:
      delete tt-registros.
  end.

  assign i-seq = 0
         c-pai = "".

  for each lote-carac-tec no-lock
     where lote-carac-tec.it-codigo = lote-item.it-codigo
       and lote-carac-tec.lote      = lote-item.lote
       and lote-carac-tec.cd-folha  = p-folha,
     each  comp-folh no-lock 
     where comp-folh.cd-folha   = lote-carac-tec.cd-folha
       and comp-folh.cd-comp    = lote-carac-tec.cd-comp
       and comp-folh.log-estado = yes
     break by comp-folh.seq-de-impres:

       assign i-seq = i-seq + 1
              c-pai = "i" + string(i-seq).

       if lote-carac-tec.tipo-result < 3 then    DO:
       
          chTreeView:nodes:add (,, "i" + string(i-seq), 
                                string(comp-folh.seq-de-impres) + " - " +
                                lote-carac-tec.cd-comp          + " - " +
                                comp-folh.abreviatura           + " - " +
                                comp-folh.un-medida, 
                                lote-carac-tec.tipo-result).
          

       END.
       else
          chTreeView:nodes:add (,, "i" + string(i-seq), 
                                string(comp-folh.seq-de-impres) + " - " +
                                lote-carac-tec.cd-comp          + " - " +
                                comp-folh.abreviatura, 
                                lote-carac-tec.tipo-result).


       create tt-registros.
       assign tt-registros.i-seq   = i-seq
              tt-registros.tipo    = lote-carac-tec.tipo-result
              tt-registros.r-rowid = rowid(lote-carac-tec).

       /* Edson - 03/04/2006 - para calculo da media da densidade otica
         existe um erro no cadstro DENS e DESN */

       IF  (SUBSTRING(lote-carac-tec.cd-comp,1,4) = "DENS"
            OR SUBSTRING(lote-carac-tec.cd-comp,1,4) = "DESN"
            OR lote-carac-tec.cd-comp = "D.OMEDIA")  
           THEN
             ASSIGN tt-registros.OTICA = IF lote-carac-tec.cd-comp = "D.OMEDIA"
                  THEN "M" ELSE "R" .

       IF  SUBSTRING(lote-carac-tec.cd-comp,1,6) = "DUREZA" THEN
             ASSIGN tt-registros.delta = "R".

       IF lote-carac-tec.cd-comp = "DELTARHO" THEN
             ASSIGN tt-registros.delta = "M".


      /*----------------------------------------------------------------*/

       /*Tabela*/
       if lote-carac-tec.tipo-result = 2 then do:
          for each c-tab-res no-lock 
             where c-tab-res.nr-tabela = lote-carac-tec.nr-tabela 
                by c-tab-res.sequencia:
                
                /*Marca o componente se j† existir*/
                find first lote-res-carac
                     where lote-res-carac.cd-comp    = lote-carac-tec.cd-comp 
                       and lote-res-carac.cd-folha   = lote-carac-tec.cd-folh
                       and lote-res-carac.it-codigo  = lote-carac-tec.it-codigo
                       and lote-res-carac.lote       = lote-carac-tec.lote
                       and lote-res-carac.nr-tabela  = lote-carac-tec.nr-tabela
                       and lote-res-carac.sequencia  = c-tab-res.sequencia no-lock no-error.

                IF c-tab-res.descricao MATCHES "*[D]*" /*[D] = Desativado*/ AND 
                   NOT AVAIL lote-res-carac                                 THEN NEXT.

                assign i-seq = i-seq + 1.
                
                chTreeView:nodes:add (c-pai, 4, "c" + string(i-seq), string(c-tab-res.sequencia) + " - " + c-tab-res.descricao, 7 /*Off*/ ).
                create tt-registros.
                assign tt-registros.i-seq   = i-seq
                       tt-registros.tipo    = 7 /*comp*/
                       tt-registros.r-rowid = rowid(c-tab-res).
                
                if avail lote-res-carac then
                   assign chTreeView:nodes:item(i-seq):image = 8.

          end.
       end.

  end.

  apply "entry" to ctrlFrame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-valida-componente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if l-folha-corrente = yes then do:
    /*Componentes da Tabela*/
    assign chTreeViewNode = chTreeView:SelectedItem:parent
           cont-select    = 0
           i-pai          = chTreeViewNode:index.

    find first tt-registros
         where tt-registros.i-seq = i-pai no-lock no-error.
    find first lote-carac-tec
         where rowid(lote-carac-tec) = tt-registros.r-rowid no-lock no-error.

    if chTreeView:SelectedItem:image = 8 then do:
       assign chTreeView:SelectedItem:image = 7.
    end.
    else do:
       if avail lote-carac-tec then do:
          find first comp-folh
               where comp-folh.cd-folh  = lote-carac-tec.cd-folha
                 and comp-folh.cd-comp  = lote-carac-tec.cd-comp no-lock no-error.
          if avail comp-folh then do:
             assign chTreeView:SelectedItem:image = 8.
             do i-cont = (chTreeViewNode:index + 1) to (chTreeViewNode:index + chTreeViewNode:children):
                if chTreeView:nodes:item(i-cont):image = 8 then
                   assign cont-select = cont-select + 1.
             end.
             if comp-folh.nr-max-esc < cont-select then do:
                assign chTreeView:SelectedItem:image = 7.
                run utp/ut-msgs.p (input "show",
                                   input 15948,
                                   input string(comp-folh.nr-max-esc) + "~~" + string(cont-select)).
                return "NOK":U.
             end.
             
          end.
       end.
    end.

    run pi-grava-componente.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-valores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i-nr-ord-produ-lote AS INTEGER     NO-UNDO.
    DEFINE VARIABLE l-ord-config AS LOGICAL     NO-UNDO.

    l-ord-config = YES.

         find first lote-carac-tec
              where rowid(lote-carac-tec) = tt-registros.r-rowid no-lock no-error.
         if avail lote-carac-tec then do:

             i-nr-ord-produ-lote = 0.
            IF   (lote-carac-tec.cd-comp = "DIIN" OR lote-carac-tec.cd-comp = "LARGURA") THEN DO:

                FIND FIRST pallet WHERE pallet.nr-pallet = lote-carac-tec.lote AND
                     
                                        pallet.it-codigo = lote-carac-tec.it-codigo AND
                                        pallet.cod-estabel = "422" NO-LOCK NO-ERROR.

                IF AVAIL pallet THEN DO:
                    FIND FIRST it-pallet OF pallet NO-LOCK NO-ERROR.

                    IF AVAIL it-pallet THEN DO:

                         FOR FIRST lote-prod WHERE 
                             lote-prod.it-codigo = it-pallet.it-codigo AND
                             lote-prod.lote      = it-pallet.lote-bobina NO-LOCK,
                           LAST movto-estoq WHERE 
                                   movto-estoq.nr-ord-produ = lote-prod.nr-ord-produ AND
                                   movto-estoq.lote = it-pallet.lote-bobina AND
                                   movto-estoq.esp-docto = 1 USE-INDEX ord-seq NO-LOCK .
                               
                              i-nr-ord-produ-lote =  movto-estoq.nr-ord-produ.
            
                              l-ord-config = (IF i-nr-ord-produ-lote >  70000000 AND 
                                                i-nr-ord-produ-lote <  80000000  THEN NO ELSE YES).
                                   
                         END.

                    END.
                END.
                ELSE DO:

                
                     FOR FIRST lote-prod WHERE 
                         lote-prod.it-codigo = lote-carac-tec.it-codigo AND
                         lote-prod.lote      = lote-carac-tec.lote NO-LOCK,
                       LAST movto-estoq WHERE 
                               movto-estoq.nr-ord-produ = lote-prod.nr-ord-produ AND
                               movto-estoq.lote = lote-carac-tec.lote AND
                               movto-estoq.esp-docto = 1 USE-INDEX ord-seq NO-LOCK .
                           
                          i-nr-ord-produ-lote =  movto-estoq.nr-ord-produ.
        
                          l-ord-config = (IF i-nr-ord-produ-lote >  70000000 AND 
                                            i-nr-ord-produ-lote <  80000000  THEN NO ELSE YES).
                               
                     END.
                END.
            END.

             find first comp-folh
                  where comp-folh.cd-folha = lote-carac-tec.cd-folha
                    and comp-folh.cd-comp  = lote-carac-tec.cd-comp no-lock no-error.
             if avail comp-folh then do:
                run pi-desabilita-dados.
                case tt-registros.tipo:
                    when 1 /*Numerico*/   then do: 
                         assign fi-resultado-num:format in frame F-Main = comp-folh.formato
                                fi-num-min       = comp-folh.vl-minimo
                                fi-num-max       = comp-folh.vl-maximo
                                fi-un-num        = comp-folh.un-medida
                                fi-resultado-num = lote-carac-tec.vl-result.
                         if l-folha-corrente = YES AND (l-ord-config OR DEC(lote-carac-tec.vl-result) = 0) then
                            enable fi-resultado-num with frame F-Main.
                    end.
                    when 2 /*Tabela*/     then do:
                         /*Nao faz nada*/
                    end.
                    when 3 /*Texto*/      then do:
                         find first lote-msg-carac
                              where lote-msg-carac.it-codigo = lote-carac-tec.it-codigo
                                and lote-msg-carac.lote      = lote-carac-tec.lote
                                and lote-msg-carac.cd-folha  = lote-carac-tec.cd-folha
                                and lote-msg-carac.cd-comp   = lote-carac-tec.cd-comp no-lock no-error.
                         assign ed-texto = if avail lote-msg-carac then 
                                           lote-msg-carac.msg-exp 
                                           else "":U.
                         if l-folha-corrente = yes then
                            enable ed-texto with frame F-Main.
                    end.
                    when 4 /*Observacao*/ then do:
                         assign fi-observacao = lote-carac-tec.observacao
                                fi-observacao:format in frame F-Main = comp-folh.formato.
                         if l-folha-corrente = yes then
                            enable fi-observacao with frame F-Main.
                    end.
                    when 5 /*Titulo*/     then do:
                         assign fi-complemento = lote-carac-tec.complemento.
                         if l-folha-corrente = yes then
                            enable fi-complemento with frame F-Main.
                    end.
                    when 6 /*Data*/       then do:
                        assign fi-dat-min       = comp-folh.dt-minima
                               fi-dat-max       = comp-folh.dt-maxima
                               fi-resultado-dat = lote-carac-tec.dt-result.
                        if l-folha-corrente = yes then
                           enable fi-resultado-dat with frame F-Main.
                    end.
                end case.
                run pi-display.

                status input comp-folh.ajuda.
                if tt-registros.tipo = 3 then
                   status input comp-folh.ajuda + " - Pressione F4 para sair do Texto!".

             end.
         end.
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
    WHEN "lote-item":U THEN p-rowid-list = p-rowid-list + 
        IF AVAILABLE lote-item THEN STRING(ROWID(lote-item))
        ELSE "?":U.
   
 
  /* snd-list - 8/21/95 */
    WHEN "lote-carac-tec":U THEN p-rowid-list = p-rowid-list + 
        IF AVAILABLE lote-carac-tec THEN STRING(ROWID(lote-carac-tec))
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
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      /* bstates.i - browser-specific ADM states */
    WHEN "update-begin":U THEN
    DO:
        adm-brs-in-update = yes.
        RUN dispatch ('enable-fields':U).
        RUN dispatch ('apply-entry':U).
        RUN new-state('update':U).
    END.
    WHEN "update":U THEN 
      DO:
      /* 'Update' means some other object just started an Update.
          Unless it was part of a Group-Assign with this Browser, 
          disable the browser until the update completes to prevent the
          user from changing records in the middle of the update. */
        DEFINE VARIABLE group-link AS CHARACTER NO-UNDO INIT "":U.
        RUN get-link-handle IN adm-broker-hdl
            (INPUT THIS-PROCEDURE, 'GROUP-ASSIGN-TARGET':U, OUTPUT group-link)
                NO-ERROR.
        IF LOOKUP(STRING(p-issuer-hdl), group-link) EQ 0 THEN 
          br-table:SENSITIVE IN FRAME F-Main = no.
      END.
    WHEN "update-complete":U THEN DO:
        br-table:SENSITIVE IN FRAME F-Main = yes.
        adm-brs-in-update = no.
    
        /* If this state message came from a query object (Query or Browser 
           with its own query) then we do *not* want to do row-changed, 
           because the row that was just updated was in our 
           Record-Target's query, not ours. */
        RUN get-attribute IN p-issuer-hdl ('QUERY-OBJECT':U).
        IF RETURN-VALUE NE "YES":U THEN
        DO:
          IF NUM-RESULTS("br-table":U) NE ? AND  /* query opened */
             NUM-RESULTS("br-table":U) NE 0 /* query's not empty */
          THEN DO:
            GET CURRENT br-table.
            RUN dispatch ('row-changed':U). 
          END.
        END.
    
        RUN new-state ('update-complete':U).  /* Pass on to others */
    END.
    WHEN "delete-complete":U THEN DO:
       DEFINE VARIABLE sts AS LOGICAL NO-UNDO.
       sts = br-table:DELETE-CURRENT-ROW() IN FRAME F-Main.
       IF NUM-RESULTS("br-table":U) = 0 THEN  /* Last row deleted? */
         RUN notify('row-available':U).  /* Make sure Targets get the message*/
    END.
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



