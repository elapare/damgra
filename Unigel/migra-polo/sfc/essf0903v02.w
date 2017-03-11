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
**  i-prgvrs.i - Programa para cria?ío do log de todos os programas 
**               e objetos do EMS 2.0 para objetos
**  {1} = objeto   provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/
/* colocado verificaá∆o de quantidade de emendas - amgra - 07/12/07*/
def buffer empresa for mgmulti.empresa.
def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[2.00.00.000[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.000"
       c-prg-obj = "essf0903V02".
if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "essf0903V02"
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
    put "essf0903V02" at 1 "2.00.00.000" at 69 today at 84 string(time,'HH:MM:SS':U) at 94 skip.
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
/* global variable definitions */
/* Parameters Definitions ---                                           */
/**************************************************************************
**
**   ceapi001.i - Include de definicoes da temp-table e variaveis 
**
**                da API ceapi001.p
**
**************************************************************************/
def temp-table tt-movto  
    field cod-versao-integracao as integer format "999"
    field cod-prog-orig         like movto-estoq.cod-prog-orig
    field l-mov-erro            as logical initial no
    field r-mov-inv             as rowid    
    field r-mov-orig            as rowid  /* registro original para  
                                             valorizar o estorno,
                                             devoluá∆o,retorno */
    field sequen-nf             like movto-estoq.sequen-nf
    field cod-depos             like movto-estoq.cod-depos
    field cod-emitente          like movto-estoq.cod-emitente
    field cod-estabel           like movto-estoq.cod-estabel
    field cod-refer             like movto-estoq.cod-refer
    field ct-codigo             like movto-estoq.ct-codigo
    field descricao-db          like movto-estoq.descricao-db
    field dt-nf-saida           like movto-estoq.dt-nf-saida
    field dt-trans              like movto-estoq.dt-trans
    field esp-docto             like movto-estoq.esp-docto
    field it-codigo             like movto-estoq.it-codigo
    field cod-localiz           like movto-estoq.cod-localiz
    field lote                  like movto-estoq.lote
    field nat-operacao          like movto-estoq.nat-operacao
    field nro-docto             like movto-estoq.nro-docto
    field num-sequen            like movto-estoq.num-sequen
    field numero-ordem          like movto-estoq.numero-ordem
    field nr-ord-produ          like movto-estoq.nr-ord-produ
    field peso-liquido          like movto-estoq.peso-liquido
    field quantidade            like movto-estoq.quantidade
    field referencia            like movto-estoq.referencia
    field sc-codigo             like movto-estoq.sc-codigo
    field serie-docto           like movto-estoq.serie-docto
    field tipo-preco            like movto-estoq.tipo-preco
    field tipo-trans            like movto-estoq.tipo-trans
    field tipo-valor            like movto-estoq.tipo-valor
    field un                    like movto-estoq.un         
    field valor-mat-m           like movto-estoq.valor-mat-m
    field valor-mat-o           like movto-estoq.valor-mat-o
    field valor-mat-p           like movto-estoq.valor-mat-p
    field valor-mob-m           like movto-estoq.valor-mob-m
    field valor-mob-o           like movto-estoq.valor-mob-o
    field valor-mob-p           like movto-estoq.valor-mob-p
    field valor-ggf-m           like movto-estoq.valor-ggf-m
    field valor-ggf-o           like movto-estoq.valor-ggf-o
    field valor-ggf-p           like movto-estoq.valor-ggf-p
    field valor-nota            like movto-estoq.valor-nota
    field vl-nota-fasb          like movto-estoq.vl-nota-fasb
    field nr-ord-refer          like movto-estoq.nr-ord-refer
    field nr-req-sum            like movto-estoq.nr-req-sum
    field cod-roteiro           like movto-estoq.cod-roteiro
    field nr-reporte            like movto-estoq.nr-reporte
    field item-pai              like movto-estoq.item-pai
    field op-codigo             like movto-estoq.op-codigo
    field cod-usu-ult-alter     like movto-estoq.cod-usu-ult-alter
    field conta-contabil        like movto-estoq.conta-contabil
    field conta-db              like movto-estoq.conta-contabil
    field ct-db                 like movto-estoq.ct-codigo
    field sc-db                 like movto-estoq.sc-codigo
    field dt-vali-lote          like saldo-estoq.dt-vali-lote
    field op-seq                like movto-estoq.op-seq
    field usuario               like movto-estoq.usuario
    field nr-trans              like movto-estoq.nr-trans 
    field cod-estabel-des       like movto-estoq.cod-estabel-des
    field origem-valor          like movto-estoq.origem-valor
    field num-ord-des           like movto-estoq.num-ord-des
    field num-seq-des           like movto-estoq.num-seq-des
    field num-ord-inv           like movto-estoq.num-ord-inv
    field valor-ipi             like movto-estoq.valor-ipi
    field valor-iss             like movto-estoq.valor-iss
    field valor-icm             like movto-estoq.valor-icm
    field vl-icm-fasb           like movto-estoq.vl-icm-fasb
    field vl-iss-fasb           like movto-estoq.vl-iss-fasb
    field vl-ipi-fasb           like movto-estoq.vl-ipi-fasb 
    field per-ppm               like movto-estoq.per-ppm
    field atualiza-ul-ent       as logical
    field i-sequen              as integer
    field gera-saldo            as logical init no
    field qt-alocada            as decimal.    
     
/* Fim Include ceapi001.i */
     /* Definicao de temp-table do movto-estoq */
/*******************************************************************
**
**  CD0666.I - Definicao temp-table de erros
**
*******************************************************************/
def new global shared var l-multi as logical initial yes.
def var l-del-erros as logical init yes.
def var v-nom-arquivo-cb as char format "x(50)" no-undo.
def var c-mensagem-cb    as char format "x(132)" no-undo.
def  temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)".
form
    space(04)
    tt-erro.cd-erro 
    space (02)
    c-mensagem-cb
    with width 132 no-box down stream-io frame f-consiste.
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/
run utp/ut-liter.p (input "Mensagem",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tt-erro.cd-erro:label in frame f-consiste = trim(return-value).
/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/
run utp/ut-liter.p (input "Descriá∆o",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-mensagem-cb:label in frame f-consiste = trim(return-value).
       /* Definicao da temp-table de erros */
/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.
def var i-empresa   as char no-undo.
def buffer b-pallet for pallet.
def var estado as char no-undo.
def var h-acomp as handle no-undo.
DEF VAR da-transf AS DATE NO-UNDO.
find FIRST param-global no-lock no-error.
find FIRST param-cp     no-lock no-error.
FIND FIRST param-estoq  NO-LOCK NO-ERROR.
/* _UIB-CODE-BLOCK-END */
/* ********************  Preprocessor Definitions  ******************** */
/* Name of first Frame and/or Browse and/or first Query                 */
/* External Tables                                                      */
/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR pallet.
/* Standard List Definitions                                            */
/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
/* _UIB-PREPROCESSOR-BLOCK-END */
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
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
/* ***********************  Control Definitions  ********************** */
/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-endereco AS CHARACTER FORMAT "x(20)":U 
     LABEL "Endereáo" 
     VIEW-AS FILL-IN 
     SIZE 19.57 BY .88 NO-UNDO.
DEFINE VARIABLE f1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.
DEFINE VARIABLE f2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.
DEFINE VARIABLE fi-desc-estabel AS CHARACTER FORMAT "x(40)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY .88 NO-UNDO.
DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "x(40)":U 
     VIEW-AS FILL-IN 
     SIZE 46.43 BY .88 NO-UNDO.
DEFINE VARIABLE fi-desc-operador AS CHARACTER FORMAT "x(40)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY .88 NO-UNDO.
DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.57 BY 3.33.
DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.57 BY 5.33.
/* ************************  Frame Definitions  *********************** */
DEFINE FRAME f-main
     pallet.cod-estabel AT ROW 1.13 COL 16.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
     fi-desc-estabel AT ROW 1.13 COL 23.29 COLON-ALIGNED NO-LABEL
     pallet.it-codigo AT ROW 2.13 COL 16.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     fi-desc-item AT ROW 2.13 COL 34.43 COLON-ALIGNED NO-LABEL
     pallet.nr-pallet AT ROW 3.13 COL 16.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.14 BY .88
     pallet.cod-localiz AT ROW 3.13 COL 41.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.14 BY .88
     c-endereco AT ROW 3.13 COL 66.43 COLON-ALIGNED
     pallet.data-pallet AT ROW 4.71 COL 21.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     f1 AT ROW 4.75 COL 59.72 COLON-ALIGNED NO-LABEL
     pallet.cod-operador AT ROW 5.71 COL 21.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     fi-desc-operador AT ROW 5.71 COL 31 COLON-ALIGNED NO-LABEL
     f2 AT ROW 5.75 COL 59.72 COLON-ALIGNED NO-LABEL
     pallet.nr-bobinas AT ROW 6.71 COL 21.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     pallet.nr-pedido AT ROW 6.71 COL 59.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.86 BY .88
     pallet.peso-liquido AT ROW 7.71 COL 21.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.43 BY .88
     pallet.nr-sequencia AT ROW 7.71 COL 59.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     pallet.peso-bruto AT ROW 8.71 COL 21.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19.43 BY .88
     pallet.cod-embal AT ROW 8.71 COL 59.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 4.46 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .
/* *********************** Procedure Settings ************************ */
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: polmfg.pallet
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
 */
/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "l:\servicos\especificos\polo\programas\polo\sfc\essf0903v02.w should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.
/* *************************  Create Window  ************************** */
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 8.83
         WIDTH              = 88.86.
/* END WINDOW DEFINITION */
                                                                        */
/* ************************* Included-Libraries *********************** */
/*-------------------------------------------------------------------------
    File        : viewer.i  
    Purpose     : Basic SmartViewer methods for the ADM
  
    Syntax      : {src/adm/method/viewer.i}
    Description :
  
    Author(s)   :
    Created     :
    Notes       :
    HISTORY: 
-------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
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
  
      adm-object-hdl = FRAME f-main:HANDLE.
  
/* Traduá∆o de Hard-Coded View-as */ 
    run pi-trad-widgets (input frame f-main:handle).
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
     'SmartViewer~`':U +      /* Type attribute */
     '~`':U +       /* Container-Type attribute */
   
     'NO ~`':U +
   
     'pallet~`':U +    /* External-Tables attribute */
     '~`':U +    /* Internal-Tables attribute */
   
     'pallet~`':U +     /* Enabled-Tables attribute */
   
     (IF adm-object-hdl = ? THEN "":U ELSE STRING(adm-object-hdl))
        + "~`":U +    /* Adm-Object-Handle attribute */
   
     'Initial-Lock,Hide-on-Init,Disable-on-Init,Key-Name,Layout,Create-On-Add~`':U +  /* Attribute-List attribute */
   
   
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
    
    
    DISABLE c-endereco rt-key rt-mold WITH FRAME f-main.
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
      RUN support/viewerd.w (INPUT THIS-PROCEDURE).
  
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
   
   
    ENABLE UNLESS-HIDDEN c-endereco rt-key rt-mold WITH FRAME f-main.
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
   /* altera?ío feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */
   
   /* est† verificaá∆o se faz necess†ria devido aos programas */
      
    /* Tenta identificar pelo ADM-CONTAINER */
    
    /* Se ainda nío identificou se ? window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
        /* Tenta identificar pelo PROCEDURE-TYPE */
        
        
            
            
        
    
    
    /* Se ainda nío identificou se ? window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
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
   
          /***************************************************************
**
** I-EPC001.I - EPC para Evento Before INITIALIZE de SmartViewer 
** 
***************************************************************/
def var c-container  as char    no-undo.
run Who-Is-The-Container in adm-broker-hdl
    (input this-procedure,
     output c-container).
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
if  valid-handle(widget-handle(c-container)) then do:
     /* APPC */
    run pi-retorna-appc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-appc-mg97 = return-value.
        run value(c-nom-prog-appc-mg97) (input "BEFORE-INITIALIZE":U, 
                                         input "VIEWER":U,
                                         input this-procedure,
                                         input frame f-main:handle,
                                         input "pallet",
        
                                         input (if  avail pallet then rowid(pallet) else ?)).    
        
    end.                                       
    else
        assign c-nom-prog-appc-mg97 = "".             
     /* UPC */
    run pi-retorna-upc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-upc-mg97 = return-value.
        run value(c-nom-prog-upc-mg97) (input "BEFORE-INITIALIZE":U, 
                                        input "VIEWER":U,
                                        input this-procedure,
                                        input frame f-main:handle,
                                        input "pallet",
        
                                        input (if  avail pallet then rowid(pallet) else ?)).    
        
    end.                                       
    else
        assign c-nom-prog-upc-mg97 = "".             
       /* DPC */
    run pi-retorna-dpc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-dpc-mg97 = return-value.
        run value(c-nom-prog-dpc-mg97) (input "BEFORE-INITIALIZE":U, 
                                        input "VIEWER":U,
                                        input this-procedure,
                                        input frame f-main:handle,
                                        input "pallet",
        
                                        input (if  avail pallet then rowid(pallet) else ?)).    
        
    end.                                       
    else
        assign c-nom-prog-dpc-mg97 = "".      
end.
/* I-EPC001.I */
 
   
   /* EPC Before Initialize do Browser */
   
   RUN broker-initialize IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.
   /*   Alteraá∆o para corrigir o problema de algumas viewers n∆o mostrar
        o primeiro registro quando o programa Ç inicializado               
        Data : 20/Fev/97  - J.Carlos (PGS)  -  Egolf e SÇrgio (DATASUL)  */  
   
        
             IF  frame f-main:scrollable THEN
                 ASSIGN frame f-main:virtual-width-chars  = frame f-main:width-chars
                        frame f-main:virtual-height-chars = frame f-main:height-chars.
        
   
                                                               /*
    Nome :  C-PAGE.i       J.Carlos - 21/Fev/97
    Fun?ío : Guardar a pagina e o container-source da VIEWER.
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
   
          /***************************************************************
**
** I-EPC001.I - EPC para Evento INITIALIZE de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
if  valid-handle(widget-handle(c-container)) then do:
     /* APPC */
    run pi-retorna-appc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-appc-mg97 = return-value.
        run value(c-nom-prog-appc-mg97) (input "INITIALIZE":U, 
                                         input "VIEWER":U,
                                         input this-procedure,
                                         input frame f-main:handle,
                                         input "pallet",
        
                                         input (if  avail pallet then rowid(pallet) else ?)).    
        
    end.                                       
    else
        assign c-nom-prog-appc-mg97 = "".             
     /* UPC */
    run pi-retorna-upc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-upc-mg97 = return-value.
        run value(c-nom-prog-upc-mg97) (input "INITIALIZE":U, 
                                        input "VIEWER":U,
                                        input this-procedure,
                                        input frame f-main:handle,
                                        input "pallet",
        
                                        input (if  avail pallet then rowid(pallet) else ?)).    
        
    end.                                       
    else
        assign c-nom-prog-upc-mg97 = "".             
       /* DPC */
    run pi-retorna-dpc in widget-handle(c-container).
    if  return-value <> ""
    and return-value <> ? then do:                  
        assign c-nom-prog-dpc-mg97 = return-value.
        run value(c-nom-prog-dpc-mg97) (input "INITIALIZE":U, 
                                        input "VIEWER":U,
                                        input this-procedure,
                                        input frame f-main:handle,
                                        input "pallet",
        
                                        input (if  avail pallet then rowid(pallet) else ?)).    
        
    end.                                       
    else
        assign c-nom-prog-dpc-mg97 = "".      
end.
/* I-EPC001.I */
 
   
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
  
       assign wh-entry-field = frame f-main:handle
              wh-entry-field = wh-entry-field:first-child
              wh-entry-field = wh-entry-field:first-child.
       do  while(valid-handle(wh-entry-field)):
           if  wh-entry-field:sensitive = yes 
           and wh-entry-field:type <> 'rectangle' then do:
               apply 'entry' to wh-entry-field.
               leave.
           end.
           else
               assign wh-entry-field = wh-entry-field:next-sibling.    
       end.
  
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
  
         /***************************************************************
**
** I-EPC006.I - EPC para Evento Before DISPLAY de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "BEFORE-DISPLAY":U, 
                                     input "VIEWER":U,
                                     input this-procedure,
                                     input frame f-main:handle,
                                     input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "BEFORE-DISPLAY":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "BEFORE-DISPLAY":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* I-EPC006.I */
 
  
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
    
    
      IF AVAILABLE pallet THEN
          DISPLAY UNLESS-HIDDEN pallet.cod-estabel pallet.it-codigo pallet.nr-pallet pallet.cod-localiz pallet.data-pallet pallet.cod-operador pallet.nr-bobinas 
          pallet.nr-pedido pallet.peso-liquido pallet.nr-sequencia
           pallet.peso-bruto pallet.cod-embal fi-desc-estabel fi-desc-item c-endereco f1 fi-desc-operador f2
            WITH FRAME f-main NO-ERROR. 
      ELSE
          CLEAR FRAME f-main ALL NO-PAUSE.        
  
    
    /* Clear MODIFIED field attr. */
    RUN check-modified IN THIS-PROCEDURE ('clear':U) NO-ERROR.  
  /* EPC - Display da Viewer */
  
         /***************************************************************
**
** I-EPC006.I - EPC para Evento DISPLAY de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "DISPLAY":U, 
                                     input "VIEWER":U,
                                     input this-procedure,
                                     input frame f-main:handle,
                                     input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "DISPLAY":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "DISPLAY":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* I-EPC006.I */
 
  
   
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
    /* In case this attribute was set earlier, turn it off. */
    RUN set-attribute-list ('REPOSITION-PENDING = NO':U).
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
  IF "pallet":U = "":U THEN
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
      
             /***************************************************************
**
** I-EPC011.I - EPC para Evento Before ADD de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "BEFORE-ADD":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "BEFORE-ADD":U, 
                                     input "VIEWER":U,
                                     input this-procedure,
                                     input frame f-main:handle,
                                     input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "BEFORE-ADD":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* I-EPC011.I */
 
      
   
  
   DEFINE VARIABLE trans-hdl-string  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cntr              AS INTEGER   NO-UNDO.
   DEFINE VARIABLE temp-rowid        AS ROWID     NO-UNDO.
   DEFINE VARIABLE saved-dictdb      AS CHARACTER NO-UNDO.
      /* Check MODIFIED field attribute for the prior record. */
      RUN check-modified IN THIS-PROCEDURE ('check':U) NO-ERROR.  
             /* Save the current rowid in case the add is cancelled. */
      ASSIGN adm-first-table = ROWID(pallet)
             adm-new-record = yes     /* Signal new rec being created. */
             adm-adding-record = yes  /* Signal that it's add not copy. */
             adm-query-empty = IF AVAILABLE(pallet)
                               THEN no ELSE yes. /* needed in Cancel */
      RUN set-attribute-list ("ADM-NEW-RECORD=yes":U).
      /* If for some reason this object's fields have not been enabled from
         outside, then do it here. */
      /* Alterado : J.Carlos - 02/04/96                                */
      /*            Alterei a ordem dos comandos Enables :             */
      /*            1o. executa o enable p/ os campos de chaves depois */
      /*            executa o enable p/ os outros campos.              */   
      /*            Isto evita que o cursor caia no campo errado.      */
      /* RUN dispatch('enable-fields':U). */ 
      
          ENABLE UNLESS-HIDDEN pallet.cod-estabel pallet.it-codigo pallet.nr-pallet WITH FRAME f-main.
      
      RUN dispatch('enable-fields':U).
      /* The first time a record is added, we determine whether this object
         is a Group-Assign-Target for another object with the same table. */
      IF group-assign-add = ? THEN
      DO: 
          RUN request-attribute IN adm-broker-hdl
            (THIS-PROCEDURE, 'GROUP-ASSIGN-SOURCE':U, 'ENABLED-TABLES':U).
          /* This will be true only if the object has a Group-Assign-Source
             *and* its first enabled table is the same as one in the G-A-S.
             If this is the case, we re-find the record the G-A-S has just
             created and add our fields to it. Otherwise we create the
             new record in this object. */
          IF LOOKUP("pallet":U, RETURN-VALUE) NE 0 THEN
            group-assign-add = yes.
          ELSE group-assign-add = no.
      END.
      /* The first time a record is added, we must get the RECID(s) of
         the template record(s) which hold initial values for Progress. 
         IF the Create-On-Add attribute is set to "yes", then we don't
         do this, because the CREATE will take care of initial values. 
         We do this only for Progress databases, however, since other
         DBs or Temp-Tables don't support the template record. */
      IF (adm-create-on-add = no) AND (adm-first-tmpl-recid = ?) AND
         (DBTYPE(LDBNAME(BUFFER pallet)) EQ "PROGRESS":U)
      THEN DO:
          saved-dictdb = LDBNAME("DICTDB":U).  /* save off current DICTDB */
          /* Change the DICTDB alias to the database of each table and
             run a separate procedure to retrieve the template RECID. */
          CREATE ALIAS DICTDB FOR DATABASE 
            VALUE(LDBNAME(BUFFER pallet)).
          RUN adm/objects/get-init.p (INPUT "pallet":U,
            OUTPUT adm-first-tmpl-recid).
          
          
          CREATE ALIAS DICTDB FOR DATABASE    /* Restore the orig. dictdb */
            VALUE(saved-dictdb).
        END.
      
          IF adm-create-on-add = no THEN 
          DO:
           IF DBTYPE(LDBNAME(BUFFER pallet)) 
             EQ "PROGRESS":U THEN
           DO:
            /* Retrieve and display the template record initial values if
               the record hasn't already been created and we're running
               against a Progress DB. */
            FIND pallet WHERE 
              RECID(pallet) = adm-first-tmpl-recid.
            
            
/************ Customizacao Datasul S.A. - John Cleber Jaraceski - 29/03/2000
              Incluir a opá∆o NO-PAUSE no comando CLEAR a fim de evitar a mensagem
              "Press space bar to continue."
              **********************************/             
             CLEAR FRAME f-main NO-PAUSE.
             DISPLAY UNLESS-HIDDEN pallet.cod-estabel pallet.it-codigo pallet.nr-pallet pallet.cod-localiz pallet.data-pallet pallet.cod-operador pallet.nr-bobinas pallet.nr-pedido
                  pallet.peso-liquido pallet.nr-sequencia pallet.peso-bruto pallet.cod-embal
              WITH FRAME f-main NO-ERROR.
/************ Customizacao Datasul S.A. - John Cleber Jaraceski - 11/10/1999
              Impedir que o registro acessado atravÇs do RECID _Template esteja 
              dispon°vel para alteraá∆o
              **********************************/
             RELEASE pallet.
             
             
/***********************************************/
           END.
/************ Customizacao Datasul S.A. - John Cleber Jaraceski - 13/04/1999
              Setar valores iniciais, na inclus∆o (ADD) quando o tipo de Database 
              for diferente de PROGRESS (por exemplo: Oracle, Sysbase, ...)
              **********************************/
            /*&IF PROVERSION BEGINS "9." &THEN*/
            /* ALTERAÄ«O FEITA PARA ATENDER A VERSAO 10 - MARCILENE - 26-11-2003 */
            ELSE setInitial().
            
/***********************************************/
          END.              /* END code for Progress DB initial values. */
          /* If the developer explicitly requested Create-On-Add or it's
             not a Progress DB then do the CREATE. */
          ELSE DO:
           DO TRANSACTION ON STOP  UNDO, RETURN "ADM-ERROR":U 
                          ON ERROR UNDO, RETURN "ADM-ERROR":U:
             adm-create-complete = no.  /* Signal whether Create succeeded. */
             RUN dispatch ('create-record':U).
             IF RETURN-VALUE = "ADM-ERROR":U THEN UNDO, RETURN "ADM-ERROR":U.
             DISPLAY UNLESS-HIDDEN pallet.cod-estabel pallet.it-codigo pallet.nr-pallet pallet.cod-localiz pallet.data-pallet pallet.cod-operador pallet.nr-bobinas pallet.nr-pedido
                  pallet.peso-liquido pallet.nr-sequencia pallet.peso-bruto pallet.cod-embal
                WITH FRAME f-main NO-ERROR.
           END.
           adm-create-complete = yes.  /* Signal Cancel that Create was done. */
          END. 
      /* Code for SmartBrowsers */
      /* EPC - Add da Viewer */ 
      
             /***************************************************************
**
** I-EPC011.I - EPC para Evento ADD de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" AND
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "ADD":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "ADD":U, 
                                     input "VIEWER":U,
                                     input this-procedure,
                                     input frame f-main:handle,
                                     input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "ADD":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* I-EPC011.I */
 
      
      RUN notify ('add-record, GROUP-ASSIGN-TARGET':U).
      RUN new-state('update':U). /* Signal that we're in a record update now. */
      RUN dispatch IN THIS-PROCEDURE ('apply-entry':U). 
   
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
       /***************************************************************
**
** I-EPC002.I - EPC para Evento VALIDATE de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "VALIDATE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
    
    if  return-value = "NOK":U then
        return "ADM-ERROR":U.
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "VALIDATE":U, 
                                     input "VIEWER":U,
                                     input this-procedure,
                                     input frame f-main:handle,
                                     input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
    if  return-value = "NOK":U then
        return "ADM-ERROR":U.
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" AND 
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "VALIDATE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
    if  return-value = "NOK":U then
        return "ADM-ERROR":U.
end.
/* I-EPC002.I */
 
        adm-updating-record = yes.    /* Signal row-available etc. */
        IF adm-new-record THEN DO:
          /* Unless we already did the CREATE in add-record, create the new
             record(s) for an Add here. */
          IF (NOT adm-adding-record) OR  /* Indicates Copy, not Add */
              (NOT adm-create-on-add) THEN
          DO:
             RUN dispatch ('create-record':U).
             IF RETURN-VALUE = "ADM-ERROR":U THEN UNDO, RETURN "ADM-ERROR":U.
          END.
          IF (adm-create-on-add = yes) OR (group-assign-add = yes) THEN
          DO:     /* Need to re-get the record to upgrade the lock */
           
            RUN dispatch ('current-changed':U). /* Get an EXCLUSIVE lock */
            IF RETURN-VALUE = "ADM-ERROR":U THEN 
               RETURN "ADM-ERROR":U.
          END.
        END.       /* END of processing for Add/Copy */
        ELSE DO:   /* for ordinary ASSIGNs: */
            RUN dispatch ('current-changed':U). /* Get an EXCLUSIVE lock */
            IF RETURN-VALUE = "ADM-ERROR":U THEN 
               RETURN "ADM-ERROR":U.
        END.
        RUN dispatch ('assign-statement':U). /* ASSIGN the fields */
        IF RETURN-VALUE = "ADM-ERROR":U THEN 
            UNDO, RETURN "ADM-ERROR":U.
        /* If this object is linked to others to be updated together,
           then do that here: */
        RUN notify ('assign-record,GROUP-ASSIGN-TARGET':U).
        IF RETURN-VALUE = "ADM-ERROR":U THEN UNDO, RETURN "ADM-ERROR":U.
        /* Display any fields assigned by CREATE. */
        IF adm-new-record THEN 
        DO:
            RUN get-attribute('Query-Position':U).  /* Is this the first rec?*/
            IF RETURN-VALUE = 'no-record-available':U THEN   /* yes it is...*/
            DO:
              RUN new-state('record-available':U).  /* Let Panels know. */
              RUN set-attribute-list('Query-Position = record-available':U).
            END.
            RUN dispatch('display-fields':U).
        END.
   
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
    
           /***************************************************************
**
** I-EPC002.I - EPC para Evento After VALIDATE de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "AFTER-VALIDATE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
    
    if  return-value = "NOK" then
        return "ADM-ERROR".
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "AFTER-VALIDATE":U, 
                                     input "VIEWER":U,
                                     input this-procedure,
                                     input frame f-main:handle,
                                     input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
    if  return-value = "NOK":U then
        return "ADM-ERROR":U.
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "AFTER-VALIDATE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
    if  return-value = "NOK":U then
        return "ADM-ERROR":U.
end.
/* I-EPC002.I */
 
    
    /* EPC - Before Assign do Viewer */ 
    
          /***************************************************************
**
** I-EPC003.I - EPC para Evento Before ASSIGN de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "BEFORE-ASSIGN":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
    if  return-value = "NOK":U then
        return error.                               
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "BEFORE-ASSIGN":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
    if  return-value = "NOK" then
        return error.                               
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "BEFORE-ASSIGN":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
    if  return-value = "NOK":U then
        return error.                               
end.
/* I-EPC003.I */
 
    
  /* ASSIGN for Frame Fields: */
    /* ADM-ASSIGN-FIELDS gives the developer the opportunity to define
       additional fields in the default frame which are to be assigned
       at the same time as the ENABLED-FIELDS list. */
    /* If this is a new record creation, also allow additional fields
       defined in ADM-CREATE-FIELDS to be entered. */
    
    IF adm-new-record THEN 
    DO:
        ASSIGN FRAME f-main pallet.cod-estabel pallet.it-codigo pallet.nr-pallet
            pallet.cod-localiz pallet.data-pallet pallet.cod-operador pallet.nr-pedido pallet.nr-sequencia pallet.cod-embal  NO-ERROR.
       /*** Customizacao PGS - Ricardo de Lima Perdigao - 14/03/1997
            Resolver o problema de desabilitar os campos da 
            lista ADM-CREATE-FIELDS antes do final da inclusao  
            A desabilitaá∆o dos campos de chaves foram passados para
            a rotina ADM-DISABLE-FIELDS. ( JosÇ Carlos - PGS - 20/03/97 ) ***/    
        /* --------------------   
        IF NOT ERROR-STATUS:ERROR THEN   /* Leave enabled if error */
            DISABLE {&UNLESS-HIDDEN} {&ADM-CREATE-FIELDS} 
              WITH FRAME {&FRAME-NAME}.
        ---------------------- */      
    END.
    ELSE
    
        ASSIGN FRAME f-main pallet.cod-localiz pallet.data-pallet
             pallet.cod-operador pallet.nr-pedido
             pallet.nr-sequencia pallet.cod-embal  
              NO-ERROR.
    IF ERROR-STATUS:ERROR THEN    /* DO error checking for non-browsers */
    DO: 
      RUN dispatch('show-errors':U).
      UNDO, RETURN "ADM-ERROR":U.
    END.
  /* ASSIGN for Browse Fields: */
   /* EPC - Assign do Viewer */ 
   
          /***************************************************************
**
** I-EPC003.I - EPC para Evento ASSIGN de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "ASSIGN":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
    if  return-value = "NOK":U then
        return error.                               
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "ASSIGN":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
    if  return-value = "NOK":U then
        return error.                               
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "ASSIGN":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
    if  return-value = "NOK":U then
        return error.                               
end.
/* I-EPC003.I */
 
   
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
  
         /***************************************************************
**
** I-EPC013.I - EPC para Evento CANCEL de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "CANCEL":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "CANCEL":U, 
                                     input "VIEWER":U,
                                     input this-procedure,
                                     input frame f-main:handle,
                                     input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" AND
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "CANCEL":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* I-EPC013.I */
 
  
  DEFINE VARIABLE source-str          AS CHARACTER NO-UNDO.
   /* Clear MODIFIED field attribute. */
   RUN check-modified IN THIS-PROCEDURE ('clear':U) NO-ERROR. 
   IF adm-new-record THEN           /* Reposition to prev record for add/copy */
   DO:
      /* If initial values were displayed through the use of the template
         record, then get rid of the template record.*/
      IF (adm-adding-record = yes) AND  /*  Add, not Copy */
         (adm-create-on-add = no)
      THEN DO:
        RELEASE pallet NO-ERROR.  
        
        
      END.
      /* If the actual record Create was done in add-record, then we must
         delete the record(s) to cancel. */
      ELSE IF (adm-adding-record = yes) AND  /* Add, not Copy */
        (adm-create-on-add = yes) AND
          (adm-create-complete = yes) /* Make sure rec actually created */
      THEN DO:
        
          RUN dispatch ('delete-record':U).
      END.
      /* Delete the new browser row */
      
          DISABLE UNLESS-HIDDEN pallet.cod-estabel pallet.it-codigo pallet.nr-pallet 
            WITH FRAME f-main.
      
      
          /* For Viewers, if this was a Cancel of an Add of the only
             record in the query, then disable the fields. */
          IF adm-query-empty THEN              /* Set for us in Add */
             RUN dispatch ('disable-fields':U).
      
      adm-new-record = no.
      RUN set-attribute-list ("ADM-NEW-RECORD=no":U).
      RUN notify ('cancel-record, GROUP-ASSIGN-TARGET':U).
   END.
   ELSE DO: 
       RUN dispatch ('reset-record':U). /*For Update, redisplay old values. */
       /*Inserido a chamada do notify para tirar o estado de update das viewers secund†rias
       inserido por tech540 (Anderson)*/
       RUN notify ('cancel-record, GROUP-ASSIGN-TARGET':U). /*Tirar o estado de update das viewers secund†rias*/
       /*fim alteraá∆o Anderson*/
   END.
   /* Alteraá∆o : J.Carlos - PGS - 03/Abr/97                                  
    *    A variavel "adm-updating-record" Ç usada na ADM-ROW-AVAILABLE p/ testar        
    *    se um registro deve ser lido ou n∆o.                       
    *    Se ela estiver com o valor "YES" indica que o Progress esta fazendo  
    *    uma atualizaá∆o (UPDATE) e assim o registro corrente n∆o precisa ser 
    *    lido pois o que esta no buffer j† Ç o registro corrente.             
    *    Ocorre que quando se esta fazendo uma inclus∆o (ADD) esta variavel   
    *    tambÇm Ç "setada" como "YES", isto Ç feito dentro do ADM-ASSIGN-RECORD. 
    *    Quando a inclus∆o termina normalmente tudo funciona bem MAS se a
    *    inclus∆o Ç abortada (UNDO, RETURN "ADM-ERROR") AP‡S ter sido executado o 
    *    ADM-ASSIGN-RECORD E em seguida Ç executado um ADM-CANCEL-RECORD (CANCEL),
    *    a variavel fica com este valor, e quando o Progress vai executar o 
    *    ADM-ROW-AVAILABLE , ele n∆o là o registro da base de dados. Assim
    *    uma SUJEIRA (um registro n∆o-gravado) fica na tela.
    *    Este ASSIGN abaixo Ç para resolver isto. O c¢digo original da Progress
    *    tinha este comando MAS estava colocado no lugar errado.
    *    Ele deve ficar antes do "RUN new-state('update-complete':U)".
    * ************************************************************ */
   adm-updating-record = no. 
   RUN get-link-handle IN adm-broker-hdl 
       (THIS-PROCEDURE, 'GROUP-ASSIGN-SOURCE':U, OUTPUT source-str).
   IF source-str EQ "":U THEN            /* If not just a group-assign-target,*/
     RUN new-state('update-complete':U). /* tell all to reset/refresh */
   
   /*adm-updating-record = no.*/
  /* EPC - After Cancel da Viewer */ 
  
         /***************************************************************
**
** I-EPC013.I - EPC para Evento After CANCEL de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "AFTER-CANCEL":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "AFTER-CANCEL":U, 
                                     input "VIEWER":U,
                                     input this-procedure,
                                     input frame f-main:handle,
                                     input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "AFTER-CANCEL":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* I-EPC013.I */
 
  
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
   
  
   DEFINE VARIABLE trans-hdl-string AS CHARACTER NO-UNDO.
      /* Check MODIFIED field attribute for the prior record. */
      RUN check-modified IN THIS-PROCEDURE ('check':U) NO-ERROR.  
             /* Save the current rowid in case the copy is cancelled. */
      ASSIGN adm-first-table = ROWID(pallet)
             adm-new-record = yes     /* Signal a new rec is being created. */
             adm-adding-record = no.  /* Signal this is copy not add. */
      RUN set-attribute-list ("ADM-NEW-RECORD=yes":U).
      /* If for some reason this object's fields have not been enabled from
         outside, then do it here. */
      RUN dispatch('enable-fields':U). 
      
          ENABLE UNLESS-HIDDEN pallet.cod-estabel pallet.it-codigo pallet.nr-pallet WITH FRAME f-main.
      
      /* The first time a record is added or copied, we determine whether 
         this object is a Group-Assign-Target for another object with the same 
         table. */
      IF group-assign-add = ? THEN
      DO: 
          RUN request-attribute IN adm-broker-hdl
            (THIS-PROCEDURE, 'GROUP-ASSIGN-SOURCE':U, 'ENABLED-TABLES':U).
          /* This will be true only if the object has a Group-Assign-Source
             *and* its first enabled table is the same as one in the G-A-S.
             If this is the case, we re-find the record the G-A-S has just
             created and add our fields to it. Otherwise we create the
             new record in this object. */
          IF LOOKUP("pallet":U, RETURN-VALUE) NE 0 THEN
            group-assign-add = yes.
          ELSE group-assign-add = no.
      END.
      
          RUN dispatch IN THIS-PROCEDURE ('apply-entry':U). 
      
      RUN notify ('copy-record, GROUP-ASSIGN-TARGET':U).
      RUN new-state('update':U). /* Signal that we're in a record update now. */
   
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
   
    DEFINE VARIABLE source-str          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE source-rowid-str    AS CHARACTER NO-UNDO.
       /* If this is a Group-Assign, then the first (or only) enabled
          table will be passed on from its parent. Any others will be 
          created here. */
       IF group-assign-add = yes THEN
       DO:
         RUN get-link-handle IN adm-broker-hdl 
           (THIS-PROCEDURE, 'GROUP-ASSIGN-SOURCE':U, 
              OUTPUT source-str).
         RUN send-records IN WIDGET-HANDLE (source-str)
             (INPUT "pallet":U, 
              OUTPUT source-rowid-str).
         FIND pallet WHERE 
             ROWID (pallet) = 
                 TO-ROWID(source-rowid-str) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO: 
           RUN dispatch('show-errors':U).
           UNDO, RETURN "ADM-ERROR":U.
         END.
       END.
       ELSE DO:
           CREATE pallet NO-ERROR.
           IF ERROR-STATUS:ERROR THEN
           DO: 
             RUN dispatch('show-errors':U).
             UNDO, RETURN "ADM-ERROR":U.
           END.
       END.
/************ Customizacao Datasul S.A. - John Cleber Jaraceski - 29/10/1999
              A gravaá∆o da vari†vel que contÇm o ROWID do registro recÇm criado
              foi transferida para ap¢s a gravaá∆o dos campos contidos no 
              preprocessador {&ADM-CREATE-FIELDS}, isto deve-se a fim de 
              corrigir uma deficiància dos Templates PROGRESS com DATABASE DIFERENTE
              DE PROGRESS
              **********************************/
        
            /* Save off the ROWID(s) of the new record(s) because under some
               circumstances, a browse may lose track of the record and it needs
               to be re-retrieved in assign-record. */
            
            adm-first-add-rowid = ROWID(pallet).
       
/***********************************************/
       
       
/************ Customizacao Datasul S.A. - John Cleber Jaraceski - 29/10/1999
              Implementar a gravaá∆o dos campos contidos no preprocessador 
              {&ADM-CREATE-FIELDS} a fim de corrigir uma deficiància dos 
              Templates PROGRESS com DATABASE DIFERENTE DE PROGRESS
              **********************************/
        
/***********************************************/
   
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
   /* Save the ROWID of the current record in case the lock upgrade fails. */
   ASSIGN adm-first-table = ROWID(pallet).
   
   
   /* Note that we do the FIND EXCLUSIVE-LOCK and CURRENT-CHANGED without
           checking the initial lock state first. This is because it is not
           possible to be certain what the actual lock state is at this time,
           and the check is not expensive if in fact the record was already
           locked. */
  FIND CURRENT pallet EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  IF NOT AVAILABLE pallet THEN DO:
/************ Customizacao Datasul S.A. - John Cleber Jaraceski - 13/03/1998
              Execucao de CANCEL-RECORD/GET-NEXT quando o registro nao estiver disponivel.
              Alterar mensagem padr∆o da PROGRESS, quando o registro nao estiver disponivel.
              Execucao de RELEASE/PROCEDURE quando o registro nao estiver disponivel.
              **********************************/
      
         if  locked pallet then do:
             run utp/ut-msgs.p (input "show", input 16096 , input "").
             find pallet no-lock where
                  rowid(pallet) = adm-first-table no-error.
             return "ADM-ERROR".
         end.
      
      run utp/ut-msgs.p (input "show", input 15217, input "").
      RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE, INPUT 'TABLEIO-SOURCE':U, OUTPUT h_tableio-source).
      if valid-handle(h_tableio-source) then do:
         RUN get-attribute IN h_tableio-source ('TYPE':U).
         if index(RETURN-VALUE, "panel") <> 0
         then do:
              RUN pi-cancelar IN h_tableio-source.
              RELEASE pallet NO-ERROR.  
              
              
              RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE, INPUT 'RECORD-SOURCE':U,  OUTPUT h_record-source).
              RUN dispatch IN h_record-source ('GET-NEXT':U).
         end.
         else do:
              RUN notify IN h_tableio-source ('CANCEL-RECORD':U).
              RUN pi-retorna-query-browse IN h_tableio-source NO-ERROR.
              if ERROR-STATUS:ERROR
              then do:
                   RUN dispatch IN h_tableio-source ('DESTROY':U) NO-ERROR.
                   RELEASE pallet NO-ERROR.
                   
                   
              end.
              else do:
                   assign h_query-browse = widget-handle(RETURN-VALUE).
                   RUN dispatch IN h_tableio-source ('DESTROY':U).
                   RELEASE pallet NO-ERROR.  
                   
                   
                   RUN get-attribute IN h_query-browse ('TYPE':U).
                   if index(RETURN-VALUE, "query") <> 0
                   then RUN dispatch IN h_query-browse ('GET-NEXT':U).
                   else if index(RETURN-VALUE, "browse") <> 0
                        then RUN dispatch IN h_query-browse ('OPEN-QUERY':U).
              end.
         end.
      end.
/************************************************/
      RETURN 'ADM-ERROR':U.
  END.
  ELSE IF CURRENT-CHANGED pallet THEN DO:
/************ Customizacao Datasul S.A. - John Cleber Jaraceski - 13/03/1998
              Alterar mensagem padr∆o da PROGRESS, quando o registro ja foi alterado por outro usuario.
              Execucao de DISPLAY-FIELDS/CANCEL-RECORD quando o registro ja foi alterado por outro usuario.
              Execucao de DESTROY/GET-CURRENT, quando o registro ja foi alterado por outro usuario.
              **********************************/
      run utp/ut-msgs.p (input "show", input 15216, input "").
      if  return-value <> 'yes' then do:    
      RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE, INPUT 'TABLEIO-SOURCE':U, OUTPUT h_tableio-source).
      if valid-handle(h_tableio-source) then do:
         RUN get-attribute IN h_tableio-source ('TYPE':U).
         if index(RETURN-VALUE, "panel") <> 0
         then do:
              RUN dispatch ('DISPLAY-FIELDS':U).
              RUN pi-cancelar IN h_tableio-source.
              UNDO, RETURN "ADM-ERROR":U.
         end.
         else do:
              RUN pi-retorna-query-browse IN h_tableio-source NO-ERROR.
              if ERROR-STATUS:ERROR
              then do:
                   RUN dispatch IN h_tableio-source ('DESTROY':U).
              end.
              else do:
                   assign h_query-browse = widget-handle(RETURN-VALUE).
                   RUN dispatch IN h_tableio-source ('DESTROY':U).
                   if valid-handle(h_query-browse) then do:
                      RUN get-attribute IN h_query-browse ('TYPE':U).
                      if index(RETURN-VALUE, "query") <> 0 
                      then RUN pi-reposiciona-query IN h_query-browse (INPUT rowid(pallet)).
                      else if index(RETURN-VALUE, "browse") <> 0
                           then RUN dispatch IN h_query-browse ('OPEN-QUERY':U).
                   end.
              end.
              RETURN "ADM-ERROR":U.
         end.
      end.
/************************************************/
      UNDO, RETURN "ADM-ERROR":U.
      end.
  END.
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
    IF NOT AVAILABLE (pallet) THEN RUN dispatch IN THIS-PROCEDURE ('row-available':U).      
/***************************************************************/
   DEFINE VARIABLE delete-failed AS LOGICAL NO-UNDO INIT no.
   
      DO TRANSACTION ON STOP UNDO, LEAVE 
                     ON ERROR UNDO, LEAVE:
        /* If this object has a group-assign-source for its first or only
           table, then don't try to re-delete the record here. */
        IF group-assign-add NE yes THEN
        DO:
          FIND CURRENT pallet EXCLUSIVE-LOCK NO-ERROR.
/************ Customizacao Datasul S.A. - Paulo Henrique Lazzarotti - 12/12/1999
              Verificar se o registro corrente est† dispon°vel para exclus∆o.
              **********************************/
          IF ERROR-STATUS:ERROR THEN
          DO: 
              run utp/ut-msgs.p (input "show", input 15217, input ""). 
              RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE, INPUT 'RECORD-SOURCE':U,  OUTPUT h_record-source).
              RUN dispatch in h_record-source ("GET-NEXT").
              RETURN "adm-error":U.
          END.
          /* EPC - Delete da Viewer */ 
          
                 /***************************************************************
**
** i-epc015.i - EPC para Evento DELETE de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:
    run value(c-nom-prog-dpc-mg97) (input "DELETE":U,
                                    input "VIEWER":U,
                                    input THIS-PROCEDURE,
                                    input frame f-main:HANDLE,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).
    
    
    if RETURN-VALUE = "NOK":U then
       return "ADM-ERROR":U.
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:
    run value(c-nom-prog-appc-mg97) (input "DELETE":U,
                                     input "VIEWER":U,
                                     input THIS-PROCEDURE,
                                     input frame f-main:HANDLE,
                                     input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).
    
    
    if RETURN-VALUE = "NOK":U then
       return "ADM-ERROR":U.
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:
    run value(c-nom-prog-upc-mg97) (input "DELETE":U,
                                    input "VIEWER":U,
                                    input THIS-PROCEDURE,
                                    input frame f-main:HANDLE,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).
    
    
    if RETURN-VALUE = "NOK":U then
       return "ADM-ERROR":U.
end.
/* i-epc015.i */
 
          
          DELETE pallet NO-ERROR.
          /* EPC - After Delete da Viewer */ 
          
                 /***************************************************************
**
** i-epc015.i - EPC para Evento After DELETE de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:
    run value(c-nom-prog-dpc-mg97) (input "AFTER-DELETE":U,
                                    input "VIEWER":U,
                                    input THIS-PROCEDURE,
                                    input frame f-main:HANDLE,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).
    
    
    if RETURN-VALUE = "NOK":U then
       return error "ADM-ERROR":U.
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:
    run value(c-nom-prog-appc-mg97) (input "AFTER-DELETE":U,
                                     input "VIEWER":U,
                                     input THIS-PROCEDURE,
                                     input frame f-main:HANDLE,
                                     input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).
    
    
    if RETURN-VALUE = "NOK":U then
       return error "ADM-ERROR":U.
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:
    run value(c-nom-prog-upc-mg97) (input "AFTER-DELETE":U,
                                    input "VIEWER":U,
                                    input THIS-PROCEDURE,
                                    input frame f-main:HANDLE,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).
    
    
    if RETURN-VALUE = "NOK":U then
       return error "ADM-ERROR":U.
end.
/* i-epc015.i */
 
          
        END.
        
        
        IF ERROR-STATUS:ERROR THEN
        DO: 
          RUN dispatch('show-errors':U).
          delete-failed = yes.
        END.    
        
      END.
      /* If I don't have my own query, */
         IF not delete-failed THEN           /*  if the delete succeeded, then*/
         RUN new-state ('delete-complete':U). /* get query object to sync up. */
      
     /*********** Customizacao PGS - Ricardo de Lima Perdigao - 26/06/1997
              Resolve problema do modifica retornar registro nao disponivel 
              apos um delete falhar por problema de validacao. 
              Linha anterior :
              IF delete-failed THEN
              UNDO, RETURN "ADM-ERROR":U.
              **********************************/
      IF delete-failed THEN
         RETURN "ADM-ERROR":U.
   
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
   
          /***************************************************************
**
** I-EPC005.I - EPC para Evento DISABLE de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "DISABLE":U, 
                                     input "VIEWER":U,
                                     input this-procedure,
                                     input frame f-main:handle,
                                     input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "DISABLE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "DISABLE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* I-EPC005.I */
 
   
   
     
       
           DISABLE UNLESS-HIDDEN pallet.cod-localiz
                pallet.data-pallet 
               pallet.cod-operador 
               pallet.nr-pedido 
                pallet.nr-sequencia 
               pallet.cod-embal 
               pallet.cod-estabel
                pallet.it-codigo
                pallet.nr-pallet 
             WITH FRAME f-main.
           /* Alteraá∆o : Foi acrescentado o pre-processor "adm-create-fields" no
                          comando DISABLE acima para que ele tambÇm desabilite os
                          campos de chave. ( J. Carlos - PGS - 20/03/97 ) */             
           RUN set-editors('DISABLE':U).    /* Adjust editor widgets */
       
       RUN set-attribute-list ("FIELDS-ENABLED=no":U).
     
   
      /* If this object is linked to others to be updated together,
         then disable fields together: */
      RUN notify ('disable-fields, GROUP-ASSIGN-TARGET':U).
   /* EPC - After Disable da Viewer */ 
   
          /***************************************************************
**
** I-EPC005.I - EPC para Evento After DISABLE de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "AFTER-DISABLE":U, 
                                     input "VIEWER":U,
                                     input this-procedure,
                                     input frame f-main:handle,
                                     input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "AFTER-DISABLE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "AFTER-DISABLE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* I-EPC005.I */
 
   
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
   
          /***************************************************************
**
** I-EPC004.I - EPC para Evento ENABLE de SmartViewer 
** 
***************************************************************/
/* alteraá∆o feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */
/* est† verificaá∆o se faz necess†ria devido aos programas */
   
    /* Tenta identificar pelo ADM-CONTAINER */
    
    /* Se ainda nío identificou se ? window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
        /* Tenta identificar pelo PROCEDURE-TYPE */
        
        
            
            
        
    
    
    /* Se ainda nío identificou se ? window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
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
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "ENABLE":U, 
                                     input "VIEWER":U,
                                     input this-procedure,
                                     input frame f-main:handle,
                                     input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "ENABLE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "ENABLE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* I-EPC004.I */
 
   
   
     
        RUN get-attribute ("FIELDS-ENABLED":U).
        IF RETURN-VALUE NE "YES":U THEN   /* Skip everything if fields are */
        DO:                               /*  already enabled. */
            IF AVAILABLE(pallet) AND
              adm-initial-lock = "SHARE-LOCK":U OR
                adm-initial-lock = "EXCLUSIVE-LOCK":U THEN  
            DO:                                    /* +++ New code for EXCL */ 
                IF adm-initial-lock = "SHARE-LOCK":U THEN
                  FIND CURRENT pallet SHARE-LOCK NO-ERROR.
                /* For an EXCLUSIVE-LOCK, get the lock momentarily just to
                   make sure no-one else can deadlock with you. Then downgrade
                   back to share-lock until the record is saved. */
                ELSE IF AVAILABLE (pallet) THEN
                DO TRANSACTION:
                  FIND CURRENT pallet 
                   EXCLUSIVE-LOCK NO-ERROR.
                END.
                IF ERROR-STATUS:ERROR THEN
                DO: 
                  RUN dispatch('show-errors':U).
                  UNDO, RETURN "ADM-ERROR":U.
                END.
                RUN dispatch ('display-fields':U).  /* reshow in case changed.*/
            END.
            
            
            
                ENABLE UNLESS-HIDDEN pallet.cod-localiz pallet.data-pallet pallet.cod-operador
                     pallet.nr-pedido pallet.nr-sequencia pallet.cod-embal  
                  WITH FRAME f-main.
                RUN set-editors('ENABLE':U).    /* Adjust editor widgets */
            
            RUN set-attribute-list ("FIELDS-ENABLED=yes":U).
        END.
     
     
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
   
          /***************************************************************
**
** I-EPC004.I - EPC para Evento After ENABLE de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "AFTER-ENABLE":U, 
                                     input "VIEWER":U,
                                     input this-procedure,
                                     input frame f-main:handle,
                                     input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "AFTER-ENABLE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" AND
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "AFTER-ENABLE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* I-EPC004.I */
 
   
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
  
         /***************************************************************
**
** I-EPC010.I - EPC para Evento END-UPDATE de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "END-UPDATE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "END-UPDATE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "END-UPDATE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* I-EPC010.I */
 
  
  
  DEFINE VARIABLE source-str          AS CHARACTER NO-UNDO.
   /* Give any final validation errors a chance to be seen and intercepted. */
   IF ERROR-STATUS:ERROR THEN
       RUN dispatch('show-errors':U).
   /* Clear MODIFIED field attr. */
   RUN check-modified IN THIS-PROCEDURE ('clear':U) NO-ERROR.  
   IF adm-new-record THEN DO:
          adm-new-record = no.
          RUN set-attribute-list ("ADM-NEW-RECORD=no":U).
          /* Save the new rowid so it will be repositioned to. */
          ASSIGN adm-first-table = ROWID(pallet).
          /* Get the record source, if any, to redisplay the changed record.
             First tell it that a reposition-query is coming so that it
             will suppress the intervening get-first event. */
          
/* Alteraá∆o : J.Carlos - PGS - 21/03/97
S¢ executar o Open-Query e o Reposition se a viewer Ç a viewer
principal. Isto Ç, se ela tem links de Group-Assign-Target */   
DEFINE VAR c_Handle AS CHAR.   
RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE, INPUT "GROUP-ASSIGN-source":U, OUTPUT c_Handle).   
IF NUM-ENTRIES(c_Handle) = 0 THEN 
DO :    
              RUN set-link-attribute IN adm-broker-hdl
                  (THIS-PROCEDURE, 'RECORD-SOURCE':U, 
                   'REPOSITION-PENDING=yes':U).
              RUN notify('open-query':U).
              RUN request IN adm-broker-hdl (INPUT THIS-PROCEDURE,
                  INPUT 'RECORD-SOURCE':U, INPUT 'reposition-query':U).
END.   /* Teste de Group-Assign-Target */             
          /* record source is local - reopen if new */
   END.
    /* Release the lock in case others are waiting for this record. */
    FIND CURRENT pallet NO-LOCK NO-ERROR.
    
    
   /* Tell any other objects in the transaction to clear themselves too. */
    RUN notify('end-update, GROUP-ASSIGN-TARGET':U).
  /* Signal query and its dependents, etc. to refresh and reset themselves. 
     Only do this, however, if we're not just a Group-Assign-Target
     of some other object; in that case, let it send the message. */
    RUN get-link-handle IN adm-broker-hdl 
        (THIS-PROCEDURE, 'GROUP-ASSIGN-SOURCE':U, OUTPUT source-str).
    IF source-str EQ "":U THEN
          RUN new-state('update-complete':U). /*Tell all to reset/refresh */
    
    adm-updating-record = no.
    RUN dispatch IN THIS-PROCEDURE ('apply-entry':U). 
  
  /* EPC - After End-update da Viewer */ 
  
         /***************************************************************
**
** I-EPC010.I - EPC para Evento After END-UPDATE de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "AFTER-END-UPDATE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "AFTER-END-UPDATE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "AFTER-END-UPDATE":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* I-EPC010.I */
 
  
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
  
         /***************************************************************
**
** I-EPC012.I - EPC para Evento UNDO de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "UNDO":U, 
                                     input "VIEWER":U,
                                     input this-procedure,
                                     input frame f-main:handle,
                                     input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "UNDO":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "UNDO":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* I-EPC012.I */
 
  
  
     
     RUN dispatch IN THIS-PROCEDURE ('display-fields':U).
     /* If this object is linked to others to be updated together,
        then reset all records: */
     RUN notify ('reset-record, GROUP-ASSIGN-TARGET':U).
     RUN dispatch IN THIS-PROCEDURE ('apply-entry':U). 
  
  /* EPC - After Undo da Viewer */ 
  
         /***************************************************************
**
** I-EPC012.I - EPC para Evento After UNDO de SmartViewer 
** 
***************************************************************/
     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
/* APPC */
if  c-nom-prog-appc-mg97 <> "" and
    c-nom-prog-appc-mg97 <> ? then do:                  
    run value(c-nom-prog-appc-mg97) (input "AFTER-UNDO":U, 
                                     input "VIEWER":U,
                                     input this-procedure,
                                     input frame f-main:handle,
                                     input "pallet",
    
                                     input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* UPC */
if  c-nom-prog-upc-mg97 <> "" and
    c-nom-prog-upc-mg97 <> ? then do:                  
    run value(c-nom-prog-upc-mg97) (input "AFTER-UNDO":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* DPC */
if  c-nom-prog-dpc-mg97 <> "" and
    c-nom-prog-dpc-mg97 <> ? then do:                  
    run value(c-nom-prog-dpc-mg97) (input "AFTER-UNDO":U, 
                                    input "VIEWER":U,
                                    input this-procedure,
                                    input frame f-main:handle,
                                    input "pallet",
    
                                    input (if  avail pallet then rowid(pallet) else ?)).    
    
end.
/* I-EPC012.I */
 
  
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
   
      DEFINE VARIABLE cRecrdSorc AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lGeneratedError AS LOGICAL NO-UNDO.
      DEFINE BUFFER bNewRecord FOR pallet.
      DO TRANSACTION ON STOP  UNDO, RETURN "ADM-ERROR":U 
                     ON ERROR UNDO, RETURN "ADM-ERROR":U :
        RUN dispatch ('assign-record':U).
        /*alteraá∆o valdir - 10/06/2004 - Atividade 114576*/
        VALIDATE pallet NO-ERROR.
        /*ASSIGN adm-first-add-rowid = ROWID({&adm-first-enabled-table}).*/
        IF ERROR-STATUS:ERROR OR
          (ERROR-STATUS:NUM-MESSAGES > 0) THEN  DO:  /*se ocorreu algum erro*/
           DEFINE VARIABLE iNumErro AS INTEGER    NO-UNDO.
           DO iNumErro = 1 TO ERROR-STATUS:NUM-MESSAGES: /*laáo para todos os erros*/
               CASE ERROR-STATUS:GET-NUMBER(iNumErro):
                   WHEN 132 OR /* registro j† existe */
                   WHEN 1502 THEN DO: /* registro deu erro no oracle, n∆o no progress*/
                       run utp/ut-msgs.p (input "show", input 1, 
                                          input "empresa").
                       /*RETURN "ADM-ERROR":U.*/
                       APPLY "ERROR".
                   END.
                   WHEN 4212 THEN DO: /*estouro de tamanho de campo */
                       run utp/ut-msgs.p (input "show", input 29761,
                                          INPUT "empresa").
                       APPLY "ERROR":U.
                   END.
                   OTHERWISE DO: /*qualquer outro erro que venha a acontecer*/
                       run utp/ut-msgs.p (input "show", input 29762, 
                                          input "oracle":U + "~~" +
                                                string(Error-Status:Get-Number(iNumErro)) + "~~" +
                                                string(ERROR-STATUS:GET-MESSAGE(iNumErro))).
                       APPLY "ERROR":U.
                   END.
               END CASE.
           END.
        END.
        /*fim alteracao valdir */
        IF  RETURN-VALUE = "ADM-ERROR":U THEN
            RETURN "ADM-ERROR":U.
      END.
      /* Do final update processing, unless there is a larger transaction
         open elsewhere, in which case it must be done when the
         transaction is ended. */
      IF NOT TRANSACTION THEN
          RUN dispatch ('end-update':U).
    IF NOT ERROR-STATUS:ERROR THEN
       ASSIGN lGeneratedError = NO.
    FIND FIRST bNewRecord NO-LOCK NO-ERROR.
    IF NOT ERROR-STATUS:ERROR /* AND ROWID(bNewRecord) = ROWID ({&adm-first-enabled-table}) */
    THEN DO:
      RUN get-link-handle IN adm-broker-hdl
       (INPUT THIS-PROCEDURE,
        INPUT "RECORD-SOURCE",
        OUTPUT cRecrdSorc).
      RUN New-First-Record IN WIDGET-HANDLE(cRecrdSorc) (INPUT ROWID(bNewRecord)) NO-ERROR.
      IF  NOT lGeneratedError AND ERROR-STATUS:ERROR THEN
          ASSIGN ERROR-STATUS:ERROR = NO.
    END.
    
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
/* Alteraá∆o :  J. Carlos - PGS - 04/04/97
   Quando esta procedure receber o parametro CHECK-STATE igual a "CHECK" ela
   ser† cancelada.
   Isto Ç para evitar que a mensagem padr∆o do UIB, que esta mais abaixo nesta proce-
   dure, seja exibida. Isto esta acontecendo em alguns momentos, considerados indevi-
   dos pela Datasul, por exemplo : 
        . Se o usu†rio esta alterando um registro e resolve cancelar a operaá∆o, 
          este teste Ç feito e a mensagem aparece. Ou ainda na mesma situaá∆o e 
          o usu†rio fecha a windows via o bot∆o de fechar da window ou aperta ALT-F4.
          Nestas situaá‰es a Datasul considera que n∆o Ç preciso avisar que algo 
          foi alterado e perguntar se quer salvar. Eles querem que saia de uma vez.
*/
    IF check-state = "check" THEN RETURN.   
/* ***************************************************************************** */   
IF NOT VALID-HANDLE(adm-object-hdl) THEN RETURN. /* Has object been destroyed?*/
  IF VALID-HANDLE(FRAME f-main:HANDLE) AND 
      AVAILABLE(pallet) THEN
  DO:
    ASSIGN curr-widget = FRAME f-main:FIRST-CHILD. /* Field group */
    ASSIGN curr-widget = curr-widget:FIRST-CHILD. /* First field */
    DO WHILE VALID-HANDLE (curr-widget):
        IF LOOKUP (curr-widget:TYPE, 
        "FILL-IN,COMBO-BOX,EDITOR,RADIO-SET,SELECTION-LIST,SLIDER,TOGGLE-BOX":U)
            NE 0 AND 
                 /* check ENABLED fields only unless attribute says otherwise */
                 (adm-check-modified-all = yes OR curr-widget:SENSITIVE) AND
                 /* check db fields only unless attribute says otherwise */
                 (adm-check-modified-all = yes OR curr-widget:TABLE NE ?) 
                 AND curr-widget:MODIFIED THEN 
/* Code for Browsers */
        DO:
            IF check-state = "check":U THEN /*Check for changes before leaving*/
            DO:
                /* If the Container has been hidden (for destroy, e.g.), 
                   force it to view again. */
                RUN request-attribute IN adm-broker-hdl (THIS-PROCEDURE,
                    'CONTAINER-SOURCE':U, 'HIDDEN':U).
                IF RETURN-VALUE = "YES":U THEN
                    RUN notify ('view,CONTAINER-SOURCE':U).
                MESSAGE IF 
/* ALTERAÄ«O FEITA PARA ATENDER A VERSAO 10 - MARCILENE - 26-11-2003 */
                           curr-widget:TABLE NE ? 
                                 THEN 
                  SUBSTITUTE ("Current &1 record has been changed.",
                    curr-widget:TABLE) 
                  ELSE "Current values have been changed."
                  SKIP "  Do you wish to save those changes?" skip(3)
                       "Field : " + curr-widget:name 
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
                            TITLE "Error in program " + THIS-PROCEDURE:FILE-NAME + " : "
                      UPDATE ANS AS LOGICAL.
               IF ANS THEN 
               DO:
                   RUN dispatch('update-record':U).
                   IF RETURN-VALUE = "ADM-ERROR":U THEN
                   DO:
                       MESSAGE "Changes to the previous record were not saved."
                           VIEW-AS ALERT-BOX ERROR.
                       RUN dispatch ('cancel-record':U). /* Reset all states. */
                   END.
               END.
               ELSE RUN dispatch('cancel-record':U).
               RETURN.
            END.
            ELSE IF check-state = "clear":U THEN
                curr-widget:MODIFIED = no.
        END.
        ASSIGN curr-widget = curr-widget:NEXT-SIBLING.
    END.
  END.
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
    ASSIGN curr-widget = FRAME f-main:CURRENT-ITERATION. /* Field group*/
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
    
/************ Customizacao Datasul S.A. - John Cleber Jaraceski - 14/09/1999
              Impedir que a vari†vel adm-create-on-add possua o valor YES
              **********************************/
        ASSIGN adm-create-on-add = NO.
/*        ASSIGN adm-create-on-add = 
 *             IF (p-attr-value EQ "NO":U) OR
 *                (p-attr-value NE "YES":U AND
 *                 DBTYPE(LDBNAME(BUFFER {&adm-first-enabled-table})) EQ "PROGRESS":U)
 *             THEN no ELSE yes.*/
/***********************************************/
    
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
  
  
    DEFINE VARIABLE ttuib-cDateFormat  AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE ttuib-daDateValue  AS DATE          NO-UNDO.
    DEFINE VARIABLE ttuib-hBuffer      AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE ttuib-hBufferField AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE ttuib-iIntValue    AS INTEGER       NO-UNDO.
    DEFINE VARIABLE ttuib-wh-initial   AS WIDGET-HANDLE NO-UNDO.
    CLEAR FRAME f-main ALL.
    DISPLAY UNLESS-HIDDEN pallet.cod-estabel pallet.it-codigo pallet.nr-pallet pallet.cod-localiz pallet.data-pallet pallet.cod-operador pallet.nr-bobinas pallet.nr-pedido pallet.peso-liquido pallet.nr-sequencia pallet.peso-bruto pallet.cod-embal
                WITH FRAME f-main NO-ERROR.
    ASSIGN ttuib-hBuffer = BUFFER pallet:HANDLE NO-ERROR.
    IF  NOT VALID-HANDLE(ttuib-hBuffer) THEN
        RETURN TRUE.
    ASSIGN ttuib-wh-initial = FRAME f-main:HANDLE
           ttuib-wh-initial = ttuib-wh-initial:FIRST-CHILD NO-ERROR.
    DO WHILE VALID-HANDLE(ttuib-wh-initial):
        IF CAN-QUERY(ttuib-wh-initial, "TABLE":U) AND
           CAN-QUERY(ttuib-wh-initial, "NAME":U) THEN
            IF ttuib-wh-initial:TABLE = "pallet":U THEN DO:
                ASSIGN ttuib-hBufferField = ttuib-hBuffer:BUFFER-FIELD(ttuib-wh-initial:NAME) NO-ERROR.
                CASE ttuib-hBufferField:DATA-TYPE:
                    WHEN "INTEGER":U THEN DO:
                        IF ttuib-wh-initial:TYPE = "RADIO-SET":U THEN DO:
                            IF ttuib-hBufferField:INITIAL = ? THEN
                                ASSIGN ttuib-iIntValue = 1 NO-ERROR.
                            ELSE
                                ASSIGN ttuib-iIntValue = INTEGER(ttuib-hBufferField:INITIAL) NO-ERROR.
                            ASSIGN ttuib-wh-initial:SCREEN-VALUE = STRING(ttuib-iIntValue) NO-ERROR.
                        END.
                        ELSE DO:
                            IF ttuib-hBufferField:INITIAL = ? THEN
                                ASSIGN ttuib-wh-initial:SCREEN-VALUE = "0":U NO-ERROR.
                            ELSE
                                ASSIGN ttuib-wh-initial:SCREEN-VALUE = LEFT-TRIM(ttuib-hBufferField:INITIAL) NO-ERROR.
                        END.
                    END.
                    WHEN "DATE":U THEN DO:
                        ASSIGN ttuib-cDateFormat = SESSION:DATE-FORMAT NO-ERROR.
                        SESSION:DATE-FORMAT = "mdy":U.
                        ASSIGN ttuib-wh-initial:SCREEN-VALUE = ?
                               ttuib-daDateValue             = DATE(ttuib-hBufferField:INITIAL) NO-ERROR.
                        SESSION:DATE-FORMAT = ttuib-cDateFormat.
                        /* Mudado em 19/08/2002 por Farley Niehues para validar o Initial value do campo Date em 01/01/1800 
                        no caso de banco de Dados SQL MSS */
                        IF ttuib-daDateValue <> ? THEN DO: 
                            ASSIGN ttuib-wh-initial:SCREEN-VALUE = STRING(ttuib-daDateValue, ttuib-wh-initial:FORMAT) NO-ERROR.
                            
                        END.
                    END.
                    WHEN "DECIMAL":U THEN DO:
                        IF ttuib-hBufferField:INITIAL = ? THEN
                            ASSIGN ttuib-wh-initial:SCREEN-VALUE = "0.0":U NO-ERROR.
                        ELSE
                            ASSIGN ttuib-wh-initial:SCREEN-VALUE = left-trim(ttuib-hBufferField:INITIAL) NO-ERROR.
                    END.
                    OTHERWISE DO:
                        IF ttuib-hBufferField:INITIAL = ? THEN
                            ASSIGN ttuib-wh-initial:SCREEN-VALUE = "":U NO-ERROR.
                        ELSE
                            ASSIGN ttuib-wh-initial:SCREEN-VALUE = ttuib-hBufferField:INITIAL NO-ERROR.
                    END.
                END CASE.
            END.
        IF ttuib-wh-initial:TYPE = "FIELD-GROUP":U THEN
            ASSIGN ttuib-wh-initial = ttuib-wh-initial:FIRST-CHILD NO-ERROR.
        ELSE
            ASSIGN ttuib-wh-initial = ttuib-wh-initial:NEXT-SIBLING NO-ERROR.
    END.
  
  
  RETURN TRUE.
END FUNCTION.
/* _UIB-CODE-BLOCK-END */
 
/* _UIB-CODE-BLOCK-END */
/* ************************  Control Triggers  ************************ */
/* ***************************  Main Block  *************************** */
  /* Initialize attributes for update processing objects. */
  RUN set-attribute-list ('FIELDS-ENABLED=no,ADM-NEW-RECORD=no':U).
/* _UIB-CODE-BLOCK-END */
 
/* Procedure Description
"Biblioteca para customizaá∆o das viewers"
*/
/*--------------------------------------------------------------------------
    Library     : c-viewer.i
    Purpose     : Implementar customizaá‰es nas viewers
    Syntax      : {include/c-viewer.i}
    Description : Method-Library criada para fornecer customizaá∆o para
                  as viewers a serem utilizadas pelos programas do
                  Magnus97
    Author(s)   : Vanei
    Created     : 13/01/1997
    Notes       : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
def var wh-pesquisa                        as handle    no-undo.
def new global shared var r-registro-atual as rowid     no-undo.
/* ut-glob.i */
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
                                                                        */
 
/* ************************* Included-Libraries *********************** */
DEFINE VARIABLE c_Aux-var AS CHARACTER NO-UNDO.
ON GO OF FRAME f-main OR ENTER OF FRAME f-main ANYWHERE DO:
    IF SELF:TYPE <> "editor":U OR 
       (SELF:TYPE = "editor":U AND KEYFUNCTION(LASTKEY) <> "RETURN":U) THEN DO:
        
        IF "pallet.cod-localiz pallet.data-pallet pallet.cod-operador pallet.nr-pedido pallet.nr-sequencia pallet.cod-embal":U <> "":U THEN DO:
            APPLY "LEAVE":U TO SELF.
            
            RUN get-link-handle IN adm-broker-hdl ( INPUT THIS-PROCEDURE,
                                                    INPUT "CONTAINER-SOURCE":U,
                                                   OUTPUT c_Aux-var).
            
            RUN pi-enter-go IN WIDGET-HANDLE(c_Aux-var) NO-ERROR.
            
            RETURN NO-APPLY.
        END.
    END.
    ELSE
        SELF:INSERT-STRING(CHR(10)).
END.
 
/* _UIB-CODE-BLOCK-END */
/* ***************************  Main Block  *************************** */
/* _UIB-CODE-BLOCK-END */
/* **********************  Internal Procedures  *********************** */
PROCEDURE pi-trata-state :
/*------------------------------------------------------------------------------
  Purpose:     Trata as mudanáas de estado (State-Changed)
  Parameters:  INPUT Handle da procedure pai
               INPUT C¢digo do Estado
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl   AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state        AS CHAR   NO-UNDO.
  
  case entry(1, p-state, '|'):
  end.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-desabilita-chave:
/*------------------------------------------------------------------------------
  Purpose:     Desabilita a chave da viewer
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  disable pallet.cod-estabel pallet.it-codigo pallet.nr-pallet   with frame f-main.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
 
/***************************************************************************
**  Programa : UT-GLOB.I
**  Include padr„o para definiÁ„o de variaveis globais.
***************************************************************************/ 
/* altera?ío feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */
/* est† verificaá∆o se faz necess†ria devido aos programas */
   
    /* Tenta identificar pelo ADM-CONTAINER */
    
    /* Se ainda nío identificou se ? window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
        /* Tenta identificar pelo PROCEDURE-TYPE */
        
        
            
            
        
    
    
    /* Se ainda nío identificou se ? window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
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
 
/***************************************************
** i_dbtype.i - Tipo de Gerenciadores utilizados
***************************************************/
/* Fim */
 
/* _UIB-CODE-BLOCK-END */
/* ***********  Runtime Attributes and AppBuilder Settings  *********** */
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.
/* SETTINGS FOR FILL-IN pallet.cod-estabel IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN f1 IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       f1:HIDDEN IN FRAME f-main           = TRUE
       f1:PRIVATE-DATA IN FRAME f-main     = 
                "Campo utilizado para recuperar valores do zoom de pedidos".
/* SETTINGS FOR FILL-IN f2 IN FRAME f-main
   NO-ENABLE                                                            */
ASSIGN 
       f2:HIDDEN IN FRAME f-main           = TRUE
       f2:PRIVATE-DATA IN FRAME f-main     = 
                "Campo utilizado para recuperar valores do zoom de pedidos".
/* SETTINGS FOR FILL-IN fi-desc-estabel IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-operador IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN pallet.it-codigo IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN pallet.nr-bobinas IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN pallet.nr-pallet IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN pallet.peso-bruto IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN pallet.peso-liquido IN FRAME f-main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
/* Setting information for Queries and Browse Widgets fields            */
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
 
/* ************************  Control Triggers  ************************ */
ON F5 OF pallet.cod-embal IN FRAME f-main /* Embalagem */
DO:
  
/*--------------------------------------------------------------------------
    File        : ZoomVar.i
    Purpose     : Chamar o programa de zoom para as chaves estrangeiras
                  para vari†veis e atributos que receber∆o seus valores
                  de atributos com nome diferente.
    Syntax      : {include/zoomvar.i}
    Description : Chama o programa de zoom para os atributos que representam
                  chaves estrangeiras
    Author(s)   : Vanei
    Created     : 16/01/1997
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
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
 
/* ***************************  Main Block  *************************** */
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
  
  if  valid-handle(wh-pesquisa) and
      wh-pesquisa:TYPE = "PROCEDURE":U and
      wh-pesquisa:FILE-NAME = "inzoom/z01in172.w":U then
        return.
      
  RUN inzoom/z01in172.w persistent set wh-pesquisa.
  if  not valid-handle(wh-pesquisa) or 
          wh-pesquisa:TYPE <> "PROCEDURE":U or 
          wh-pesquisa:FILE-NAME <> "inzoom/z01in172.w":U then
      return.
      
  
  
  RUN dispatch IN wh-pesquisa ('initialize':U).
  
  if valid-handle(wh-pesquisa) and
     wh-pesquisa:TYPE = "PROCEDURE":U and
     wh-pesquisa:FILE-NAME = "inzoom/z01in172.w":U then do:
    
        RUN pi-entry IN wh-pesquisa.
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(pallet.cod-embal:handle in frame f-main) + '|':U + 'it-codigo'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    
    
  end.
                    
assign l-implanta = no.
/* _UIB-CODE-BLOCK-END */
 
END.
/* _UIB-CODE-BLOCK-END */
ON MOUSE-SELECT-DBLCLICK OF pallet.cod-embal IN FRAME f-main /* Embalagem */
DO:
  apply "f5":U to self.
END.
/* _UIB-CODE-BLOCK-END */
ON F5 OF pallet.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
  
/*--------------------------------------------------------------------------
    File        : ZoomVar.i
    Purpose     : Chamar o programa de zoom para as chaves estrangeiras
                  para vari†veis e atributos que receber∆o seus valores
                  de atributos com nome diferente.
    Syntax      : {include/zoomvar.i}
    Description : Chama o programa de zoom para os atributos que representam
                  chaves estrangeiras
    Author(s)   : Vanei
    Created     : 16/01/1997
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
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
 
/* ***************************  Main Block  *************************** */
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
  
  if  valid-handle(wh-pesquisa) and
      wh-pesquisa:TYPE = "PROCEDURE":U and
      wh-pesquisa:FILE-NAME = "adzoom/z01ad107.w":U then
        return.
      
  RUN adzoom/z01ad107.w persistent set wh-pesquisa.
  if  not valid-handle(wh-pesquisa) or 
          wh-pesquisa:TYPE <> "PROCEDURE":U or 
          wh-pesquisa:FILE-NAME <> "adzoom/z01ad107.w":U then
      return.
      
  
  
  RUN dispatch IN wh-pesquisa ('initialize':U).
  
  if valid-handle(wh-pesquisa) and
     wh-pesquisa:TYPE = "PROCEDURE":U and
     wh-pesquisa:FILE-NAME = "adzoom/z01ad107.w":U then do:
    
        RUN pi-entry IN wh-pesquisa.
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(pallet.cod-estabel:handle in frame f-main) + '|':U + 'cod-estabel'.
        
            assign c-lista-campo = c-lista-campo + chr(10) + string(fi-desc-estabel:handle in frame f-main) + '|':U + 'nome'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    
    
  end.
                    
assign l-implanta = no.
/* _UIB-CODE-BLOCK-END */
  
END.
/* _UIB-CODE-BLOCK-END */
ON LEAVE OF pallet.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
    find first estabelec
         where estabelec.cod-estabel = input frame f-main  pallet.cod-estabel
         no-lock no-error.
    if avail estabelec then
       assign fi-desc-estabel = estabelec.nome.
    else
       assign fi-desc-estabel = "":U.
    disp fi-desc-estabel
        with frame f-main.
END.
/* _UIB-CODE-BLOCK-END */
ON MOUSE-SELECT-DBLCLICK OF pallet.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
  apply "f5":U to self.
END.
/* _UIB-CODE-BLOCK-END */
ON F5 OF pallet.cod-localiz IN FRAME f-main /* Localizaá∆o */
DO:
   
/*--------------------------------------------------------------------------
    File        : ZoomVar.i
    Purpose     : Chamar o programa de zoom para as chaves estrangeiras
                  para vari†veis e atributos que receber∆o seus valores
                  de atributos com nome diferente.
    Syntax      : {include/zoomvar.i}
    Description : Chama o programa de zoom para os atributos que representam
                  chaves estrangeiras
    Author(s)   : Vanei
    Created     : 16/01/1997
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
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
 
/* ***************************  Main Block  *************************** */
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
  
  if  valid-handle(wh-pesquisa) and
      wh-pesquisa:TYPE = "PROCEDURE":U and
      wh-pesquisa:FILE-NAME = "inzoom/z02in189.w":U then
        return.
      
  RUN inzoom/z02in189.w persistent set wh-pesquisa.
  if  not valid-handle(wh-pesquisa) or 
          wh-pesquisa:TYPE <> "PROCEDURE":U or 
          wh-pesquisa:FILE-NAME <> "inzoom/z02in189.w":U then
      return.
      
  
  
  RUN dispatch IN wh-pesquisa ('initialize':U).
  
  if valid-handle(wh-pesquisa) and
     wh-pesquisa:TYPE = "PROCEDURE":U and
     wh-pesquisa:FILE-NAME = "inzoom/z02in189.w":U then do:
    
        RUN pi-entry IN wh-pesquisa.
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(pallet.cod-localiz:handle in frame f-main) + '|':U + 'cod-localiz'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    
    
  end.
                    
assign l-implanta = no.
/* _UIB-CODE-BLOCK-END */
  
END.
/* _UIB-CODE-BLOCK-END */
ON MOUSE-SELECT-DBLCLICK OF pallet.cod-localiz IN FRAME f-main /* Localizaá∆o */
DO:
  apply "f5":U to self.
END.
/* _UIB-CODE-BLOCK-END */
ON F5 OF pallet.cod-operador IN FRAME f-main /* Operador */
DO:
    
/*--------------------------------------------------------------------------
    File        : ZoomVar.i
    Purpose     : Chamar o programa de zoom para as chaves estrangeiras
                  para vari†veis e atributos que receber∆o seus valores
                  de atributos com nome diferente.
    Syntax      : {include/zoomvar.i}
    Description : Chama o programa de zoom para os atributos que representam
                  chaves estrangeiras
    Author(s)   : Vanei
    Created     : 16/01/1997
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
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
 
/* ***************************  Main Block  *************************** */
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
  
  if  valid-handle(wh-pesquisa) and
      wh-pesquisa:TYPE = "PROCEDURE":U and
      wh-pesquisa:FILE-NAME = "inzoom/z01in518.w":U then
        return.
      
  RUN inzoom/z01in518.w persistent set wh-pesquisa.
  if  not valid-handle(wh-pesquisa) or 
          wh-pesquisa:TYPE <> "PROCEDURE":U or 
          wh-pesquisa:FILE-NAME <> "inzoom/z01in518.w":U then
      return.
      
  
  
  RUN dispatch IN wh-pesquisa ('initialize':U).
  
  if valid-handle(wh-pesquisa) and
     wh-pesquisa:TYPE = "PROCEDURE":U and
     wh-pesquisa:FILE-NAME = "inzoom/z01in518.w":U then do:
    
        RUN pi-entry IN wh-pesquisa.
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(pallet.cod-operador:handle in frame f-main) + '|':U + 'cod-operador'.
        
            assign c-lista-campo = c-lista-campo + chr(10) + string(fi-desc-operador:handle in frame f-main) + '|':U + 'nom-operador'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    
    
  end.
                    
assign l-implanta = no.
/* _UIB-CODE-BLOCK-END */
  
END.
/* _UIB-CODE-BLOCK-END */
ON LEAVE OF pallet.cod-operador IN FRAME f-main /* Operador */
DO:
  find first operador
       where operador.cod-operador = input frame f-main  pallet.cod-operador
       no-lock no-error.
  if avail operador then
     assign fi-desc-operador = operador.nom-operador.
  else
     assign fi-desc-operador = "":U.
  disp fi-desc-operador
      with frame f-main.
END.
/* _UIB-CODE-BLOCK-END */
ON MOUSE-SELECT-DBLCLICK OF pallet.cod-operador IN FRAME f-main /* Operador */
DO:
  apply "f5":U to self.
END.
/* _UIB-CODE-BLOCK-END */
ON F5 OF pallet.it-codigo IN FRAME f-main /* Item */
DO:
  
/*--------------------------------------------------------------------------
    File        : ZoomVar.i
    Purpose     : Chamar o programa de zoom para as chaves estrangeiras
                  para vari†veis e atributos que receber∆o seus valores
                  de atributos com nome diferente.
    Syntax      : {include/zoomvar.i}
    Description : Chama o programa de zoom para os atributos que representam
                  chaves estrangeiras
    Author(s)   : Vanei
    Created     : 16/01/1997
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
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
 
/* ***************************  Main Block  *************************** */
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
  
  if  valid-handle(wh-pesquisa) and
      wh-pesquisa:TYPE = "PROCEDURE":U and
      wh-pesquisa:FILE-NAME = "inzoom/z01in172.w":U then
        return.
      
  RUN inzoom/z01in172.w persistent set wh-pesquisa.
  if  not valid-handle(wh-pesquisa) or 
          wh-pesquisa:TYPE <> "PROCEDURE":U or 
          wh-pesquisa:FILE-NAME <> "inzoom/z01in172.w":U then
      return.
      
  
  
  RUN dispatch IN wh-pesquisa ('initialize':U).
  
  if valid-handle(wh-pesquisa) and
     wh-pesquisa:TYPE = "PROCEDURE":U and
     wh-pesquisa:FILE-NAME = "inzoom/z01in172.w":U then do:
    
        RUN pi-entry IN wh-pesquisa.
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(pallet.it-codigo:handle in frame f-main) + '|':U + 'it-codigo'.
        
            assign c-lista-campo = c-lista-campo + chr(10) + string(fi-desc-item:handle in frame f-main) + '|':U + 'desc-item'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    
    
  end.
                    
assign l-implanta = no.
/* _UIB-CODE-BLOCK-END */
  
END.
/* _UIB-CODE-BLOCK-END */
ON LEAVE OF pallet.it-codigo IN FRAME f-main /* Item */
DO:
  find first item
       where item.it-codigo = input frame f-main  pallet.it-codigo
       no-lock no-error.
  if avail item then
     assign fi-desc-item = item.desc-item.
  else
     assign fi-desc-item = "":U.
  disp fi-desc-item
      with frame f-main.
END.
/* _UIB-CODE-BLOCK-END */
ON MOUSE-SELECT-DBLCLICK OF pallet.it-codigo IN FRAME f-main /* Item */
DO:
  apply "f5":U to self.
END.
/* _UIB-CODE-BLOCK-END */
ON F5 OF pallet.nr-pedido IN FRAME f-main /* Pedido */
DO:
   
/*--------------------------------------------------------------------------
    File        : ZoomVar.i
    Purpose     : Chamar o programa de zoom para as chaves estrangeiras
                  para vari†veis e atributos que receber∆o seus valores
                  de atributos com nome diferente.
    Syntax      : {include/zoomvar.i}
    Description : Chama o programa de zoom para os atributos que representam
                  chaves estrangeiras
    Author(s)   : Vanei
    Created     : 16/01/1997
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
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
 
/* ***************************  Main Block  *************************** */
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
  
  if  valid-handle(wh-pesquisa) and
      wh-pesquisa:TYPE = "PROCEDURE":U and
      wh-pesquisa:FILE-NAME = "dizoom/z01di159.w":U then
        return.
      
  RUN dizoom/z01di159.w persistent set wh-pesquisa.
  if  not valid-handle(wh-pesquisa) or 
          wh-pesquisa:TYPE <> "PROCEDURE":U or 
          wh-pesquisa:FILE-NAME <> "dizoom/z01di159.w":U then
      return.
      
  
  
  RUN dispatch IN wh-pesquisa ('initialize':U).
  
  if valid-handle(wh-pesquisa) and
     wh-pesquisa:TYPE = "PROCEDURE":U and
     wh-pesquisa:FILE-NAME = "dizoom/z01di159.w":U then do:
    
        RUN pi-entry IN wh-pesquisa.
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(f1:handle in frame f-main) + '|':U + 'nr-pedcli'.
        
            assign c-lista-campo = c-lista-campo + chr(10) + string(f2:handle in frame f-main) + '|':U + 'nome-abrev'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    
    
  end.
                    
assign l-implanta = no.
/* _UIB-CODE-BLOCK-END */
 
   wait-for close of wh-pesquisa.                   
   find first ped-venda
        where ped-venda.nome-abrev = f2:screen-value in frame f-main
        and ped-venda.nr-pedcli <> "0"
          and ped-venda.nr-pedcli  = f1:screen-value in frame f-main no-lock no-error.
   if avail ped-venda then
      assign pallet.nr-pedido:screen-value in frame f-main = string(ped-venda.nr-pedido).
   else 
      assign pallet.nr-pedido:screen-value in frame f-main = "0":U
             pallet.nr-sequencia:screen-value IN FRAME f-main = "0".   
END.
/* _UIB-CODE-BLOCK-END */
ON MOUSE-SELECT-DBLCLICK OF pallet.nr-pedido IN FRAME f-main /* Pedido */
DO:
  apply "f5":U to self.
END.
/* _UIB-CODE-BLOCK-END */
ON leave OF pallet.nr-pedido IN FRAME f-main /* Pedido */
DO:
 find first ped-venda
          where ped-venda.nr-pedido = input frame f-main pallet.nr-pedido and ped-venda.nr-pedcli <> "0"
          no-lock no-error.
 IF AVAIL ped-venda  THEN 
        RUN pdp\upc\upc-pd4000-a.p (INPUT ped-venda.cod-estabel, INPUT input frame f-main pallet.it-codigo, INPUT  ped-venda.nome-abrev).
 else assign pallet.nr-sequencia:screen-value IN FRAME f-main = "0".
 
END.
ON leave OF pallet.nr-pallet IN FRAME f-main /* Pedido */
DO:
 
 find first pallet where pallet.nr-pallet = input frame f-main pallet.nr-pallet
          no-lock no-error.
 IF AVAIL pallet THEN do:
 
    run utp/ut-msgs.p (input "show",
                             input 1,
                             input "Pallet" + pallet.nr-pallet + " no item " + pallet.it-codigo ).
 
 apply "entry" to pallet.nr-pallet IN FRAME f-main.
 return no-apply.
 
 end.
 
END.
/* _UIB-CODE-BLOCK-END */
ON F5 OF pallet.nr-sequencia IN FRAME f-main /* Seq */
DO:
     find first ped-venda
          where ped-venda.nr-pedido = input frame f-main pallet.nr-pedido 
          no-lock no-error.
     if not avail ped-venda then do:
        run utp/ut-msgs.p (input "show":U,
                           input 2,
                           input "Pedido"). 
        return no-apply.
     end.
     
/*--------------------------------------------------------------------------
    File        : ZoomVar.i
    Purpose     : Chamar o programa de zoom para as chaves estrangeiras
                  para vari†veis e atributos que receber∆o seus valores
                  de atributos com nome diferente.
    Syntax      : {include/zoomvar.i}
    Description : Chama o programa de zoom para os atributos que representam
                  chaves estrangeiras
    Author(s)   : Vanei
    Created     : 16/01/1997
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
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
 
/* ***************************  Main Block  *************************** */
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
  
  if  valid-handle(wh-pesquisa) and
      wh-pesquisa:TYPE = "PROCEDURE":U and
      wh-pesquisa:FILE-NAME = "dizoom/z01di154.w":U then
        return.
      
  RUN dizoom/z01di154.w persistent set wh-pesquisa.
  if  not valid-handle(wh-pesquisa) or 
          wh-pesquisa:TYPE <> "PROCEDURE":U or 
          wh-pesquisa:FILE-NAME <> "dizoom/z01di154.w":U then
      return.
      
  run pi-seta-inicial in wh-pesquisa (input ped-venda.nome-abrev, input ped-venda.nr-pedcli).
  
  RUN dispatch IN wh-pesquisa ('initialize':U).
  
  if valid-handle(wh-pesquisa) and
     wh-pesquisa:TYPE = "PROCEDURE":U and
     wh-pesquisa:FILE-NAME = "dizoom/z01di154.w":U then do:
    
        RUN pi-entry IN wh-pesquisa.
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(pallet.nr-sequencia:handle in frame f-main) + '|':U + 'nr-sequencia'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    
    
  end.
                    
assign l-implanta = no.
/* _UIB-CODE-BLOCK-END */
 
END.
/* _UIB-CODE-BLOCK-END */
ON MOUSE-SELECT-DBLCLICK OF pallet.nr-sequencia IN FRAME f-main /* Seq */
DO:
  apply "f5":U to self.
END.
/* _UIB-CODE-BLOCK-END */
/* ***************************  Main Block  *************************** */
pallet.cod-estabel:load-mouse-pointer("image/lupa.cur") in frame f-main.
pallet.it-codigo:load-mouse-pointer("image/lupa.cur") in frame f-main.
pallet.cod-operador:load-mouse-pointer("image/lupa.cur") in frame f-main.
pallet.nr-pedido:load-mouse-pointer("image/lupa.cur") in frame f-main. 
pallet.nr-sequencia:load-mouse-pointer("image/lupa.cur") in frame f-main. 
pallet.cod-localiz:load-mouse-pointer("image/lupa.cur") in frame f-main. 
pallet.cod-embal:load-mouse-pointer("image/lupa.cur") in frame f-main. 
  
  
  /************************ INTERNAL PROCEDURES ********************/
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
  IF key-name eq ? THEN tbl-list = "pallet":U.    
 
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
    IF row-avail-rowid NE ROWID(pallet) THEN different-row = yes.
    IF row-avail-rowid ne ? THEN DO:
      /* Change record-available state only for enabled tables, 
         because it affects the state of Update panel buttons. */
      
        /* Don't bother with this for query objects because they manage
           the states in adm-open-query. */
        
          IF adm-row-avail-state NE yes THEN DO: 
             /* If we switched states and this is the primary or only table
                then signal this. */
             RUN new-state ('record-available':U). 
             RUN set-attribute-list ('Query-Position = record-available':U).
             adm-row-avail-state = yes.
          END.
        
      
            /* find-tbl.i - */
       DO:
         IF row-avail-enabled AND            /* fields are enabled */ 
            (adm-initial-lock = "SHARE-LOCK":U OR  
             adm-initial-lock = "EXCLUSIVE-LOCK":U) THEN /* +++ new for EXCL*/
         DO:
           IF adm-initial-lock = "SHARE-LOCK":U        
           THEN FIND pallet WHERE ROWID(pallet) = row-avail-rowid SHARE-LOCK NO-ERROR.
           ELSE DO TRANSACTION:
             /* For EXCLUSIVE-LOCK we get the lock momentarily to assure
                that no-one else can deadlock with us. Then the lock is
                downgraded to share-lock until the record is saved. */  
             FIND pallet WHERE ROWID(pallet) = row-avail-rowid EXCLUSIVE-LOCK NO-ERROR.               
           END. /* DO TRANSACTION... */
         END. /* IF...enabled... */
         ELSE FIND pallet WHERE ROWID(pallet) = row-avail-rowid  NO-LOCK NO-ERROR.
         IF ERROR-STATUS:ERROR THEN DO:
           RUN dispatch ('show-errors':U).
           RETURN "ADM-ERROR":U.
         END. /* IF...ERROR... */
       END. /* end of find-tbl.i */
      
    END. /* IF row-avail-rowid ne ? ... */
    ELSE DO:
         IF AVAILABLE pallet THEN RELEASE pallet.      /* Force NO-AVAILABLE */
         
         /* Change record-available state only for enabled tables. */
         
         /* This is the primary or only record in the row,  
            and there's no record available, so if we switched states, 
            then signal that. */
           
              IF adm-row-avail-state NE no THEN
              DO:                                
                RUN new-state ('no-record-available':U). 
                RUN set-attribute-list 
                  ('Query-Position = no-record-available':U).
                adm-row-avail-state = no.        
              END.     
            
         
     END.
  END.
 
  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  /* row-end.i */
IF VALID-HANDLE (adm-object-hdl) THEN  /* If there's a Frame, etc. then */
    RUN dispatch IN THIS-PROCEDURE ('display-fields':U). /* display the fields*/
/* Note: open-query does its own notify of row-available */
RUN notify IN THIS-PROCEDURE ('row-available':U).
  ASSIGN r-registro-atual = ROWID(pallet).
 
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
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  /* Code placed here will execute AFTER standard behavior.    */
  find first param-cp no-lock no-error.
  assign pallet.cod-estabel:screen-value in frame f-main = "422" /*param-cp.cod-estabel*/
         pallet.data-pallet:screen-value in frame f-main = string(today, "99/99/9999")
         pallet.cod-operador:screen-value in frame f-main = "000000".
/* ========================================================================
    Rotina para sugerir o operador logado */
 
 
   FIND FIRST operador WHERE
       operador.char-2 = c-seg-usuario
       NO-LOCK NO-ERROR.
 
   IF AVAIL operador THEN DO:
       ASSIGN pallet.cod-operador:screen-value in frame f-main = string(operador.cod-operador) 
              fi-desc-operador:SCREEN-VALUE in frame f-main = operador.nom-operador.
   END.
 
   ASSIGN pallet.cod-operador:SENSITIVE in frame f-main = NO.
 
 
/* fim Rotina para sugerir o operador logado 
========================================================================   */
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    
/*--------------------------------------------------------------------------
    File        : i-valid.i 
    Purpose     : Executa a rotina de valida?ío em todas as viwers com 
                  Group-Assign-Target
    Syntax      :
    Description :
    Author(s)   : Ricardo de Lima Perdigao
    Created     : 14/03/1997
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
  DEFINE VAR c_aux         AS CHARACTER NO-UNDO.
  DEFINE VAR i_aux         AS INTEGER   NO-UNDO.
  DEFINE VAR h_aux         AS HANDLE    NO-UNDO.
  DEFINE VAR c_page-viewer AS CHARACTER NO-UNDO.
  DEFINE VAR h_container   AS HANDLE    NO-UNDO.
  DEFINE VARIABLE i-c AS INTEGER    NO-UNDO.
  DEFINE BUFFER b-item FOR ITEM.
  
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
                                                                        */
 
/* ***************************  Main Block  *************************** */
  RUN get-link-handle IN adm-broker-hdl (INPUT this-procedure, INPUT "GROUP-ASSIGN-TARGET":U, OUTPUT c_aux).
  IF c_aux <> "" THEN DO: 
     RUN pi-validate IN THIS-PROCEDURE.
     IF RETURN-VALUE = "ADM-ERROR":U THEN UNDO, RETURN "ADM-ERROR":U. 
  END.
  DO i_aux = 1 TO NUM-ENTRIES(c_aux) ON ERROR UNDO, RETURN "ADM-ERROR":U : 
     ASSIGN h_aux = WIDGET-HANDLE(ENTRY(i_aux,c_aux)).
     RUN pi-validate in h_aux.
     IF RETURN-VALUE = "ADM-ERROR":U THEN UNDO, RETURN "ADM-ERROR":U.
  END.
/* _UIB-CODE-BLOCK-END */
 
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
   
    if adm-new-record then do:
       find first pallet
            where pallet.cod-estabel = input frame f-main pallet.cod-estabel
              and pallet.it-codigo   = input frame f-main pallet.it-codigo
              and pallet.nr-pallet   = input frame f-main pallet.nr-pallet 
              USE-INDEX pallet no-lock NO-ERROR .
       if avail pallet then do:
          run utp/ut-msgs.p (input "show",
                             input 1,
                             input "Pallet").
          apply "entry":U to pallet.cod-estabel in frame f-main.
          return "adm-error":U.
       end.
       ASSIGN i-c = 0.
       FOR EACH b-ITEM WHERE b-ITEM.GE-codigo >= 42 AND b-ITEM.GE-codigo < 48 NO-LOCK.
             IF CAN-FIND(FIRST b-pallet WHERE b-pallet.it-codigo = b-ITEM.it-codigo AND
                          b-pallet.nr-pallet = input frame f-main pallet.nr-pallet NO-LOCK) THEN
                   ASSIGN i-c = i-c + 1.
       END.
 
       if i-c > 0 then do:
          run utp/ut-msgs.p (input "show":U,
                             input 17006,
                             input "N£mero do pallet j† informado anteriormente!").
          return "ADM-ERROR":U.
       end.
/*  estava demorando muito foi substituido pela rotina acima */
/*
      find first b-pallet
            where b-pallet.nr-pallet = input frame f-main pallet.nr-pallet no-lock no-error.
       if avail b-pallet then do:
          run utp/ut-msgs.p (input "show":U,
                             input 17006,
                             input "N£mero do pallet j† informado anteriormente!").
          return "ADM-ERROR":U.
       end.
       
*/
    end.
    FIND FIRST pol-param-estab
       WHERE pol-param-estab.cod-estabel = input frame f-main 
                                               pallet.cod-estab NO-LOCK NO-ERROR.
    if not avail pol-param-estab then do:
         run utp/ut-msgs.p (input "show",
                             input 2,
                             input "ParÉmetros Estabelecimento").
         apply "entry":U to pallet.cod-estabel in frame f-main.
         return "adm-error":U.
    end.
if not(adm-new-record) then do:
      ASSIGN i-c = 0.
       FOR EACH b-ITEM WHERE b-ITEM.GE-codigo >= 42 AND b-ITEM.GE-codigo < 48 NO-LOCK.
             IF CAN-FIND(first b-pallet
              where b-pallet.it-codigo = b-item.it-codigo AND 
                    b-pallet.nr-pallet = input frame f-main pallet.nr-pallet and 
                    b-pallet.situacao <> 3 /*Transferido*/ 
                    and rowid(b-pallet)   <> rowid(pallet) no-lock) THEN
                   ASSIGN i-c = i-c + 1.
       END.
 
       if i-c > 0 then do:
           run utp/ut-msgs.p (input "show":U,
                              input 17006,
                              input "N£mero do pallet j† informado anteriormente!").
           return "ADM-ERROR":U.
       END.
 /* estava demorando muito colocado rotina acima - edson*/
/*
    find first b-pallet
         where b-pallet.nr-pallet = input frame f-main pallet.nr-pallet
           and b-pallet.situacao <> 3 /*Transferido*/
           and rowid(b-pallet)   <> rowid(pallet) 
           no-lock no-error.
    if avail b-pallet then do:
       run utp/ut-msgs.p (input "show":U,
                          input 17006,
                          input "N£mero do pallet j† informado anteriormente!").
       return "ADM-ERROR":U.
    end.
*/
END.
    find first estabelec
         where estabelec.cod-estabel = input frame f-main  pallet.cod-estabel
         no-lock no-error.
    if not avail estabelec then do:
         run utp/ut-msgs.p (input "show",
                             input 2,
                             input "Estabelecimento").
         apply "entry":U to pallet.cod-estabel in frame f-main.
         return "adm-error":U.
    end.
    find first item
         where item.it-codigo = input frame f-main  pallet.it-codigo
         no-lock no-error.
    if not avail item then do:
         run utp/ut-msgs.p (input "show",
                             input 2,
                             input "Item").
         apply "entry":U to pallet.it-codigo in frame f-main.
         return "adm-error":U.
    end.
    find first item-uni-estab
         where item-uni-estab.cod-estabel = input frame f-main pallet.cod-estabel
           and item-uni-estab.it-codigo   = input frame f-main pallet.it-codigo no-lock no-error.
    if avail item-uni-estab then do:
        find first mgemp.localizacao
             where localizacao.cod-estabel = input frame f-main pallet.cod-estabel
               and localizacao.cod-depos   = "EXP":U
               and localizacao.cod-localiz = input frame f-main pallet.cod-localiz 
               no-lock no-error.
        if not avail localizacao then do:
           run utp/ut-msgs.p (input "show",
                              input 17006,
                              input "Localizaá∆o n∆o encontrada!").
           return "ADM-ERROR":U.
        end.
    end.
    else do:
           run utp/ut-msgs.p (input "show",
                              input 17006,
                              input "Item n∆o encontrado para o estabelecimento informado!").
           return "ADM-ERROR":U.
    end.
    if input frame f-main pallet.data-pallet = ? then do:
       run utp/ut-msgs.p (input "show",
                          input 17006,
                          input "Data inv†lida!").
       apply "entry":U to pallet.data-pallet in frame f-main.
       return "adm-error":U.
    end.
    find first operador
         where operador.cod-operador = input frame f-main  pallet.cod-operador
         no-lock no-error.
    if not avail operador then do:
         run utp/ut-msgs.p (input "show",
                             input 2,
                             input "Operador").
         apply "entry":U to pallet.cod-operador in frame f-main.
         return "adm-error":U.
    end.
    
    if input frame f-main pallet.nr-pedido <> 0 then do:
        find first ped-venda
             where ped-venda.nr-pedido = input frame f-main pallet.nr-pedido 
             no-lock no-error.
        if not avail ped-venda then do:
           run utp/ut-msgs.p (input "show",
                              input 2,
                              input "Pedido").
           apply "entry":U to pallet.nr-pedido in frame f-main.
           return "adm-error":U.
        end.
    
        find first ped-item
             where ped-item.nome-abrev   = ped-venda.nome-abrev
               and ped-item.nr-pedcli    = ped-venda.nr-pedcli
               and ped-item.nr-sequencia = input frame f-main pallet.nr-sequencia
               and ped-item.it-codigo    = input frame f-main pallet.it-codigo
               /*and ped-item.cod-refer    = input frame {&frame-name} pallet.cod-refer*/ no-lock no-error.
        if not avail ped-item then do:
           run utp/ut-msgs.p (input "show",
                              input 2,
                              input "Item do Pedido":U).
           apply "entry":U to pallet.nr-pedido in frame f-main.
           return "ADM-ERROR":U.
        end.
        
        IF NOT adm-new-record 
        AND pallet.nr-pedido <> INPUT FRAME f-main pallet.nr-pedido THEN DO:
           /*Ao alterar o pedido o pallet Ç transferido automaticamente*/
           RUN pi-transf-pallet.
           FIND FIRST tt-erro NO-LOCK NO-ERROR.
           IF AVAIL tt-erro OR RETURN-VALUE = "NOK"
           THEN return "ADM-ERROR":U.
        END. 
    end.
    /* colocado verificaá∆o de quantidade de emendas - amgra - 07/12/07*/
    RUN sfc\essf0903a-amg.p (INPUT INPUT FRAME f-main pallet.nr-pedido).
    
    /* Dispatch standard ADM method.                             */
ASSIGN INPUT FRAME f-main c-endereco.
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    if adm-new-record then
       assign pallet.situacao = 1.
    
    find first ped-item
         where ped-item.nome-abrev   = ped-venda.nome-abrev
           and ped-item.nr-pedcli    = ped-venda.nr-pedcli
           and ped-item.nr-sequencia = input frame f-main pallet.nr-sequencia
           and ped-item.it-codigo    = input frame f-main pallet.it-codigo    no-lock no-error.
    if avail ped-item then do:
       assign pallet.cod-refer = ped-item.cod-refer.
    end.
    ASSIGN substring(pallet.char-1,1,20) = c-endereco.
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    
    
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* Dispatch standard ADM method.                             */ 
 
 RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .    
 if RETURN-VALUE = 'ADM-ERROR':U then                            
     return 'ADM-ERROR':U.                                       
 if AVAIL pallet 
 AND NOT adm-new-record THEN
     ASSIGN c-endereco:SCREEN-VALUE in frame f-main = substring(pallet.char-1,1,20).
 END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    
    
    if estado = "Modifica" then do:
       find first b-pallet
            where rowid(b-pallet) = v-row-parent no-lock no-error.
       if avail pallet and
          pallet.situacao = 2 then
          disable pallet.cod-localiz
                  pallet.data-pallet
                  pallet.cod-operador
                  pallet.nr-bobinas
                  pallet.peso-liquido
                  pallet.peso-bruto
                  pallet.cod-embal
                 with frame f-main.
                 
                 
    end.
    
    
    
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-seta-acao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input        parameter p-estado       as char.
  assign estado = p-estado.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-transf-pallet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
              
   if TODAY > pol-param-estab.data-palete  then 
       ASSIGN da-transf = pol-param-estab.data-palete.
   ELSE assign da-transf = TODAY.
   for each tt-movto exclusive-lock: delete tt-movto. end. 
   for each tt-erro exclusive-lock:  delete tt-erro.  end.
   run utp/ut-acomp.p persistent set h-acomp.
   
   run pi-inicializar in h-acomp (input "Transferindo Pallet").
   
   run pi-desabilita-cancela in h-acomp.
   
   blk-do:
   do transaction on error undo blk-do, return no-apply:
       
       assign i-empresa = param-global.empresa-prin WHEN AVAIL param-global.          
      find estabelec where
           estabelec.cod-estabel = pallet.cod-estabel no-lock no-error.          
      run cdp/cd9970.p (input rowid(estabelec),
                        output i-empresa).
       find conta-contab
            where conta-contab.ep-codigo      = i-empresa
              and conta-contab.conta-contabil = param-estoq.conta-transf no-lock no-error.
    
       run pi-acompanhar in h-acomp (input "Transferindo Pallet...").
       /*TRANSFERENCIA*/
           
       find item where item.it-codigo = pallet.it-codigo no-lock no-error.
       
       find FIRST saldo-estoq
            where saldo-estoq.cod-estabel = pallet.cod-estabel
              and saldo-estoq.cod-depos   = "EXP":U
              and saldo-estoq.cod-localiz = pallet.cod-localiz
              and saldo-estoq.lote        = pallet.nr-pallet
              and saldo-estoq.it-codigo   = pallet.it-codigo
              /*and saldo-estoq.cod-refer   = pallet.cod-refer*/ 
              AND saldo-estoq.qtidade-atu > 0 no-lock no-error.
       
       if not avail saldo-estoq then do:
          
          run pi-finalizar in h-acomp.
            
          run utp/ut-msgs.p (input "show":U,
                             input 17006,
                             input "Saldo n∆o encontrado para o item " + pallet.it-codigo + "~~" +
                                   "N∆o ser† poss°vel a transferància do pallet para este pedido." ).
          undo blk-do, return "NOK":U.
          
       end.
       
       find first ped-item
         where ped-item.nome-abrev   = ped-venda.nome-abrev
           and ped-item.nr-pedcli    = ped-venda.nr-pedcli
           and ped-item.nr-sequencia = input frame f-main pallet.nr-sequencia
           and ped-item.it-codigo    = input frame f-main pallet.it-codigo    no-lock no-error.
       
       find first pallet
            where pallet.cod-estabel = input frame f-main pallet.cod-estabel
              and pallet.it-codigo   = input frame f-main pallet.it-codigo
              and pallet.nr-pallet   = input frame f-main pallet.nr-pallet EXCLUSIVE-LOCK.
       ASSIGN pallet.nro-docto = substring(string(pallet.nr-pallet),1,1) 
                              + STRING(input frame f-main pallet.nr-pedido) + "-TR" 
           /*"/" + ENTRY(2,pallet.nr-pallet,"/")*/.
       /*Saida*/
       create tt-movto.
       assign tt-movto.cod-versao-integracao  = 1
              tt-movto.ct-codigo              = conta-contab.ct-codigo
              tt-movto.sc-codigo              = conta-contab.sc-codigo
              tt-movto.cod-prog-orig          = "essf0903"
              tt-movto.tipo-trans             = 2
              tt-movto.esp-docto              = 33
              tt-movto.conta-contabil         = param-estoq.conta-transf
              tt-movto.dt-trans               = da-transf
              tt-movto.dt-vali-lote           = pallet.data-pallet
              tt-movto.nro-docto              = pallet.nro-docto
              tt-movto.serie-docto            = pallet.serie-docto
              tt-movto.cod-depos              = "EXP":U
              tt-movto.cod-estabel            = pallet.cod-estabel
              tt-movto.it-codigo              = pallet.it-codigo
              tt-movto.cod-refer              = pallet.cod-refer
              tt-movto.cod-localiz            = pallet.cod-localiz
              tt-movto.lote                   = pallet.nr-pallet
              tt-movto.quantidade             = pallet.peso-liquido
              tt-movto.un                     = item.un
              tt-movto.usuario                = c-seg-usuario.
       /*Entrada*/
       create tt-movto.
       assign tt-movto.cod-versao-integracao  = 1
              tt-movto.ct-codigo              = conta-contab.ct-codigo
              tt-movto.sc-codigo              = conta-contab.sc-codigo
              tt-movto.cod-prog-orig          = "essf0903"
              tt-movto.tipo-trans             = 1
              tt-movto.esp-docto              = 33
              tt-movto.conta-contabil         = param-estoq.conta-transf
              tt-movto.dt-trans               = da-transf
              tt-movto.dt-vali-lote           = pallet.data-pallet 
              tt-movto.nro-docto              = pallet.nro-docto
              tt-movto.serie-docto            = pallet.serie-docto
              tt-movto.cod-depos              = "EXP":U
              tt-movto.cod-estabel            = pallet.cod-estabel
              tt-movto.it-codigo              = pallet.it-codigo
              tt-movto.cod-refer              = ped-item.cod-refer
              tt-movto.cod-localiz            = pallet.cod-localiz
              tt-movto.lote                   = pallet.nr-pallet
              tt-movto.quantidade             = pallet.peso-liquido
              tt-movto.un                     = item.un
              tt-movto.usuario                = c-seg-usuario.
       
       run pi-acompanhar in h-acomp (input "Efetivando Transaá∆o...Aguarde...").
       run cep/ceapi001.p (input-output table tt-movto,
                           input-output table tt-erro,
                           input yes).     
    
       FIND FIRST tt-erro NO-LOCK NO-ERROR.
       IF AVAIL tt-erro or return-value = "NOK":U THEN DO:
          run pi-finalizar in h-acomp.
          run cdp/cd0666.w (input table tt-erro).
          undo blk-do, return "NOK":U.
       END.
       /*amgra - jose roberto - 13/08/2008 - libera palete alocado*/
       IF AVAIL pallet AND
           pallet.nr-pedido <> 0 THEN DO:
           FIND FIRST am-pd-alocacao WHERE
               am-pd-alocacao.it-codigo = pallet.it-codigo AND
               am-pd-alocacao.nr-pallet = pallet.nr-pallet
               EXCLUSIVE-LOCK NO-ERROR.
           IF AVAIL am-pd-alocacao THEN
               DELETE am-pd-alocacao.
       END.
   end.
   run pi-finalizar in h-acomp.
 
      
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    
/*--------------------------------------------------------------------------
    File        : i-vldfrm.i
    Purpose     : Seleciona a pagina correta do folder e executa a validacao de
                  de frame nesta pˇgina. Utilizado em viewers de cadastro complexo.
    Syntax      :
    Description :
    Author(s)   : Ricardo de Lima Perdigao
    Created     : 14/03/1997
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
  DEF VAR i_page-viewer AS INTEGER   NO-UNDO.
  DEF VAR i_page-atual  AS INTEGER   NO-UNDO.
  DEF VAR h_Container   AS HANDLE    NO-UNDO.
  DEF VAR h_aux         AS HANDLE    NO-UNDO.
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
 
RUN get-attribute ("W-Page":U).
ASSIGN i_page-viewer = INT(RETURN-VALUE).
RUN get-attribute ("W-Container-Source":U).
ASSIGN h_Container = WIDGET-HANDLE(RETURN-VALUE).
RUN get-attribute IN h_Container ("CURRENT-PAGE":U).
ASSIGN i_page-atual = INT(RETURN-VALUE).
IF  not FRAME f-main:VALIDATE() THEN DO:
    IF  i_page-viewer <> i_page-atual AND i_page-viewer <> 0 THEN do:
        RUN select-page IN  h_Container (i_page-viewer).
        FRAME f-main:VALIDATE().
    end.
    RETURN "ADM-ERROR":U.            
END.
/* _UIB-CODE-BLOCK-END */
  /*:T Validaá∆o de dicion†rio */
    
/*:T    Segue um exemplo de validaá∆o de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */
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
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.
  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      /* vstates.i - viewer-specific ADM states */
    WHEN 'update-begin':U THEN DO:   /* Somebody pressed the Update button */
        RUN dispatch('enable-fields':U).
        if RETURN-VALUE = "ADM-ERROR":U then
           return "ADM-ERROR":U.
        RUN new-state ('update':U).  /* Tell others (query, nav panel... */
    END.
    WHEN 'update-complete':U THEN DO:
        RUN new-state ('update-complete':U).  /* Tell others... */
    END.
    
 
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
