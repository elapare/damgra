
/* Connected Databases 
          mgadm            PROGRESS
*/
def buffer operacao for mgind.operacao.

{sfc\essf0001.i1}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-saldo-estoq LIKE saldo-estoq
    FIELD log-ajuste AS LOG INIT YES.

DEFINE BUFFER b-tt-saldo-estoq  FOR tt-saldo-estoq.
DEFINE BUFFER bb-tt-saldo-estoq FOR tt-saldo-estoq.
DEFINE BUFFER b-item FOR ITEM.
DEFINE BUFFER b-item-uni-estab FOR  item-uni-estab.

def temp-table tt-itens-reporte
    field it-codigo    like item.it-codigo
    field desc-item    like item.desc-item
    field quantidade   like movto-estoq.quantidade   column-label "Quantidade ACA"
    field cod-depos    like tt-saldo-estoq.cod-depos    column-label "Depos." 
    field localizacao  like tt-saldo-estoq.cod-localiz  column-label "Localizaá∆o" 
    field lote         like tt-saldo-estoq.lote         column-label "Lote       "   
    field val-lote     like tt-saldo-estoq.dt-vali-lote init 12/31/9999 column-label "Dt.Val.Lote".

def buffer b-tt-itens-reporte for tt-itens-reporte.

{sfc\essf0001.i2}
{sfc\essf0001.i3}
{sfc\essf0001.i4}
{cep\ceapi001.i}

def var h-acomp as handle no-undo.


def temp-table tt-relatorio no-undo
    field it-codigo like item.it-codigo
    field descricao as char format "x(80)"
    field erro      as log init yes.

def temp-table tt-rel-aux   no-undo
    field it-codigo like item.it-codigo
    field i-sequen  like tt-erro.i-sequen
    field cd-erro   like tt-erro.cd-erro
    field descricao like tt-erro.mensagem.
    
def temp-table tt-lote-a-config   no-undo
    field it-codigo like item.it-codigo
    field desc-item like item.desc-item
    field lote      like tt-saldo-estoq.lote.

{sfc\essf0001.i5}

def buffer b-oper-ord for oper-ord.
def buffer b-ord-prod for ord-prod.
    
def var de-qtd-segs-inic-prep    as dec no-undo.
def var de-qtd-segs-fim-prep     as dec no-undo.
def var de-qtd-segs-inic-reporte like rep-oper-ctrab.qtd-segs-inic-reporte no-undo.
def var de-qtd-segs-fim-reporte  like rep-oper-ctrab.qtd-segs-fim-reporte  no-undo.
def var l-erro-ver as log no-undo.

def var i-num-seq-rep as int no-undo.

/******************************************************************************
 ** 
 **  INCLUDE  : CPAPI012.I 
 **
 **  OBJETIVO : Definir as temp-tables da API de Requis. de Materiais 
 **
 ******************************************************************************/
 
 def temp-table tt-requis no-undo
     field tipo-trans       as integer init 1
     field nr-ord-produ     like ord-prod.nr-ord-produ
     field quantidade       as decimal
     field data             as date
     field item-ini         as char    init "                "
     field item-fim         as char    init "ZZZZZZZZZZZZZZZZ"
     field deposito-ini     as char    init "   "
     field deposito-fim     as char    init "ZZZ"
     field op-codigo-ini    as integer init 0
     field op-codigo-fim    as integer init 9999
     field cod-localiz-ini  as char    init "   "          /*pacote 97*/
     field cod-localiz-fim  as char    init "ZZZZZZZZZZ"   /*pacote 97*/
     field procura-saldos   as logical
     field carrega-reservas as logical init yes
     field prog-seg         as char
     field time-out         as integer init 30
     field tentativas       as integer init 10
     field cod-versao-integracao as integer format "999"
     field nro-docto  like movto-mat.serie-docto
     field serie-docto like movto-mat.nro-docto.
 
 
/****************************************************************************
**
**   Include: CPAPI012.I1  - Definiá∆o das Temp-Tables Internas
**
*****************************************************************************/
 
/* TEMP-TABLES PARA VALIDAR LOTE A SEREM REQUISITADOS */

def temp-table tt-rw-tt-saldo-estoq /* EDSON - 30-10-2006 */
    field rw-tt-saldo-estoq         as rowid.



def temp-table tt-aloca 
    field cod-estabel      like aloca-reserva.cod-estabel
    field it-codigo        like aloca-reserva.it-codigo
    field nr-ord-produ     like aloca-reserva.nr-ord-produ
    field cod-depos        like aloca-reserva.cod-depos
    field cod-localiz      like aloca-reserva.cod-localiz
    field lote-serie       like reservas.lote-serie
    field quant-aloc       like aloca-reserva.quant-aloc   
    field quant-calc       like aloca-reserva.quant-aloc
    field qt-a-req         like aloca-reserva.quant-aloc
    field cod-refer        like tt-saldo-estoq.cod-refer
    field op-codigo        like aloca-reserva.op-codigo
    field cod-roteiro      like aloca-reserva.cod-roteiro
    field item-pai         like aloca-reserva.item-pai
    field un               like reservas.un
    field dt-vali-lote     like tt-saldo-estoq.dt-vali-lote
    field rw-aloca-reserva as rowid
    field sequencia        as integer
  &IF DEFINED (bf_man_per_ppm) &THEN
    field veiculo          like reservas.veiculo
    field per-ppm          like reservas.per-ppm
    field per-ppm-lote     like reservas.per-ppm
    field tipo-formula     like reservas.tipo-formula
    field qt-a-req-fis     like aloca-reserva.quant-aloc
    field qt-aloc-lote     like aloca-reserva.qt-aloc-lote
    field l-balanceado     as log
  &ENDIF
    index seq is primary unique nr-ord-produ sequencia
    index aloca item-pai 
                cod-roteiro         
                op-codigo
                it-codigo
                cod-estabel
                cod-depos
                cod-localiz
                lote-serie
    index rowid-aloca-reserva rw-aloca-reserva.
  
def temp-table tt-reservas 
    field selec               as logical init yes format "*/ "
    field proporcao           as decimal
    field log-sem-saldo       as logical
    field nr-ord-produ      like reservas.nr-ord-produ
    field cod-refer         like reservas.cod-refer
    field it-codigo         like reservas.it-codigo
    field quant-orig        like reservas.quant-orig  
    field quant-aloc        like reservas.quant-orig
    field quant-atend       like reservas.quant-orig
    field quant-calc        like reservas.quant-orig
    field quant-requis      like reservas.quant-orig
    field quant-requis-aloc like reservas.quant-orig
    field cod-depos         like reservas.cod-depos
    field cod-localiz       like reservas.cod-localiz
    field lote-serie        like reservas.lote-serie
    field dt-vali-lote      like tt-saldo-estoq.dt-vali-lote
    field dt-saida            as date format "99/99/9999" init today
    field un                like reservas.un
    field estado            like reservas.estado
    field tipo-sobra        like reservas.tipo-sobra
    field item-pai          like reservas.item-pai
    field op-codigo         like reservas.op-codigo
    field cod-roteiro       like reservas.cod-roteiro

    field per-ppm           like reservas.per-ppm
    field tipo-formula      like reservas.tipo-formula
    field qt-atend-lote     like reservas.qt-atend-lote
 
    field processada          as logical
    field rw-reserva          as rowid
    field rw-tt-saldo-estoq      as rowid
    field rw-mov-orig         as rowid
    field tipo-ordem          as integer
    field tempo               as integer
    field tentativas          as integer
    field sequencia           as integer
    index seq is primary unique nr-ord-produ sequencia
    index proc            nr-ord-produ processada
    index rw-reserva      rw-reserva
    index codigo          nr-ord-produ quant-orig processada
    index tempo           nr-ord-produ quant-orig processada tempo tentativas
    index idx             nr-ord-produ item-pai it-codigo cod-roteiro op-codigo cod-depos cod-localiz
    index item            nr-ord-produ it-codigo.      
    
   
         
/*
def temp-table tt-reservas 
    field selec               as logical init yes format "/**/ "
    field proporcao           as decimal
    field log-sem-saldo       as logical
    field nr-ord-produ      like reservas.nr-ord-produ
    field cod-refer         like reservas.cod-refer
    field it-codigo         like reservas.it-codigo
    field quant-orig        like reservas.quant-orig  
    field quant-aloc        like reservas.quant-orig
    field quant-atend       like reservas.quant-orig
    field quant-calc        like reservas.quant-orig
    field quant-requis      like reservas.quant-orig
    field quant-requis-aloc like reservas.quant-orig
    field cod-depos         like reservas.cod-depos
    field cod-localiz       like reservas.cod-localiz
    field lote-serie        like reservas.lote-serie
    field dt-vali-lote      like tt-saldo-estoq.dt-vali-lote
    field dt-saida            as date format "99/99/9999" init today
    field un                like reservas.un
    field estado            like reservas.estado
    field tipo-sobra        like reservas.tipo-sobra
    field item-pai          like reservas.item-pai
    field op-codigo         like reservas.op-codigo
    field cod-roteiro       like reservas.cod-roteiro
 
    field per-ppm           like reservas.per-ppm
    field tipo-formula      like reservas.tipo-formula
    field qt-atend-lote     like reservas.qt-atend-lote
 
    field processada          as logical
    field rw-reserva          as rowid
    field rw-tt-saldo-estoq      as rowid
    field rw-mov-orig         as rowid
    field tipo-ordem          as integer
    field tempo               as integer
    field tentativas          as integer
    field sequencia           as integer
    index seq is primary unique nr-ord-produ sequencia
    index proc            nr-ord-produ processada
    index rw-reserva      rw-reserva
    index codigo          nr-ord-produ quant-orig processada
    index tempo           nr-ord-produ quant-orig processada tempo tentativas
    index idx             nr-ord-produ item-pai it-codigo cod-roteiro op-codigo cod-depos cod-localiz
    index item            nr-ord-produ it-codigo
    index saldo  nr-ord-produ it-codigo cod-depos cod-localiz lote-serie cod-refer.   
*/
    
&IF DEFINED (bf_man_sfc_lc) &THEN
def temp-table tt-mat-reciclado 
    field nr-ord-produ like ord-prod.nr-ord-produ
    field es-codigo    like item-lista-compon.es-codigo
    field cod-depos    like reservas.cod-depos
    field cod-localiz  like reservas.cod-localiz
    field quant-orig   like reservas.quant-orig
    field quant-atend  like reservas.quant-atend
    field quant-requis like reservas.quant-requis
    field perc-requis  as dec format ">>9.99"
    field old-quant    like reservas.quant-requis
    field nro-ord-seq  as integer
    index codigo is unique primary nr-ord-produ nro-ord-seq es-codigo.
&ENDIF
 

def var v-qtd-segs-inic-aux      as dec    no-undo.  
def var v-qtd-segs-fim-aux       as dec    no-undo.

def new global shared var grw-lote-item as rowid no-undo.
def new global shared var gc-estado     as char  no-undo.

def var c-item as char no-undo.
def var c-lote as char no-undo.


def temp-table tt-digita no-undo
    field nr-ord-produ like ord-prod.nr-ord-produ
    field cod-estabel  like ord-prod.cod-estabel
    field nr-linha     like ord-prod.nr-linha
    field rw-lote-item as rowid 
    field arquivo      as char.


/** Etiqueta ************************/
define temp-table tt-param
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

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* Name of first Frame and/or Browse and/or first Query                 */

/* Internal Tables (found by Frame, Query & Browse Queries)             */

/* Definitions for BROWSE br-reporte                                    */


/* Definitions for FRAME f-cad                                          */

/* Standard List Definitions                                            */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */




/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Reciclado"    
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-can 
     IMAGE-UP FILE "image/im-can.bmp":U
     LABEL "Can" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-elimina 
     LABEL "Eliminar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-inclui 
     LABEL "Incluir" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-mod 
     IMAGE-UP FILE "image/im-param.bmp":U
     LABEL "Mod" 
     SIZE 4 BY 1.25.
     
DEFINE BUTTON bt-conv 
     IMAGE-UP FILE "image/im-paren.bmp":U
     LABEL "Conv" 
     TOOLTIP "Convers∆o de Saldo Item REF"
     SIZE 4 BY 1.25.
     
DEFINE BUTTON bt-modifica 
     LABEL "Modificar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-ok 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     LABEL "Ok" 
     SIZE 4 BY 1.25.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "x(3)":U 
     LABEL "Estabelec" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-maquina AS CHARACTER FORMAT "X(16)":U 
     LABEL "Centro de Trab." 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-operador AS CHARACTER FORMAT "99999-9":U 
     LABEL "Operador" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-estabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-maquina AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-operador AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-trans-f AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Fim" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-trans-i AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Inicio" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-hr-trans-f AS CHARACTER FORMAT "99:99":U 
     VIEW-AS FILL-IN 
     SIZE 8.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-hr-trans-i AS CHARACTER FORMAT "99:99":U 
     VIEW-AS FILL-IN 
     SIZE 8.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tempo AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Tempo Operaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-turno AS CHARACTER FORMAT "x(8)":U 
     LABEL "Turno" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 89.86 BY 6.46.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 90 BY 10.71.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 90 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */

DEFINE QUERY br-reporte FOR 
      tt-itens-reporte SCROLLING.


/* Browse definitions                                                   */
DEFINE BROWSE br-reporte

  QUERY br-reporte DISPLAY
      tt-itens-reporte.it-codigo
      tt-itens-reporte.desc-item
      tt-itens-reporte.quantidade 
      tt-itens-reporte.cod-depos 
      tt-itens-reporte.localizacao
      tt-itens-reporte.lote       
      tt-itens-reporte.val-lote
enable
    tt-itens-reporte.it-codigo
    tt-itens-reporte.quantidade 
    tt-itens-reporte.cod-depos  
    tt-itens-reporte.localizacao
    tt-itens-reporte.lote       
    tt-itens-reporte.val-lote
/* _UIB-CODE-BLOCK-END */

    WITH NO-ROW-MARKERS SEPARATORS SIZE 88.57 BY 9.17.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-mod AT ROW 1.13 COL 1.29
     bt-can AT ROW 1.13 COL 5.43
     bt-ok AT ROW 1.13 COL 9.43
     bt-conv AT ROW 1.13 COL 13.43
     fi-cod-estabel AT ROW 2.75 COL 17 COLON-ALIGNED
     fi-desc-estabel AT ROW 2.75 COL 22 COLON-ALIGNED NO-LABEL
     fi-turno AT ROW 3.75 COL 17 COLON-ALIGNED
     fi-cod-operador AT ROW 4.75 COL 17 COLON-ALIGNED
     fi-desc-operador AT ROW 4.75 COL 29 COLON-ALIGNED NO-LABEL
     fi-cod-maquina AT ROW 5.75 COL 17 COLON-ALIGNED
     fi-desc-maquina AT ROW 5.75 COL 34 COLON-ALIGNED NO-LABEL
     fi-dt-trans-i AT ROW 6.75 COL 17 COLON-ALIGNED
     fi-hr-trans-i AT ROW 6.75 COL 29.57 COLON-ALIGNED NO-LABEL
     fi-dt-trans-f AT ROW 7.75 COL 17 COLON-ALIGNED
     fi-hr-trans-f AT ROW 7.75 COL 29.57 COLON-ALIGNED NO-LABEL
     fi-tempo AT ROW 7.75 COL 67 COLON-ALIGNED
     bt-inclui AT ROW 18.58 COL 1.57
     bt-modifica AT ROW 18.58 COL 16.57
     bt-elimina AT ROW 18.58 COL 31.72
     br-reporte AT ROW 9.33 COL 1.57
     RECT-1 AT ROW 2.25 COL 1
     RECT-3 AT ROW 9.13 COL 1
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 19.


/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
 */


/* *************************  Create Window  ************************** */


IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 19.13
         WIDTH              = 90.29
         MAX-HEIGHT         = 20.42
         MAX-WIDTH          = 90.29
         VIRTUAL-HEIGHT     = 20.42
         VIRTUAL-WIDTH      = 90.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE w-livre = CURRENT-WINDOW.

ASSIGN w-livre:MENUBAR    = MENU m-livre:HANDLE.
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


  
    ASSIGN adm-object-hdl    =   w-livre.
  


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
     'w-livre~`':U +      /* Type attribute */
     'WINDOW~`':U +       /* Container-Type attribute */
   
     'NO ~`':U +
   
     '~`':U +    /* External-Tables attribute */
     'tt-itens-reporte~`':U +    /* Internal-Tables attribute */
   
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

 
    

    
    DISABLE bt-mod bt-conv RECT-1 RECT-3 rt-button WITH FRAME f-cad.
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

 
   

   
    ENABLE UNLESS-HIDDEN bt-mod bt-conv RECT-1 RECT-3 rt-button WITH FRAME f-cad.

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
"Library para window consulta simples"
*/


/*--------------------------------------------------------------------------
    Library     : w-livre.i
    Purpose     : Permitir customizaá∆o para as window de consulta simples

    Syntax      : {include/w-livre.i}

    Description : Library utilizada para customizaá∆o da window de consulta
                  simples

    Author(s)   : Gilsinei
    Created     : 06/03/1997
    Notes       :
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define variable wh-pesquisa                as handle no-undo.
define variable wh-relacionamento          as handle no-undo.
define variable wh-consulta                as handle no-undo.
define variable v-row-table                as rowid no-undo.
define variable wh-programa                as handle no-undo.
define variable c-container                as char   no-undo.
define variable wh-container               as handle no-undo.
define variable container                  as char   no-undo.
def new global shared var r-registro-atual as rowid  no-undo.

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

assign current-window:max-width-chars = current-window:width-chars
       current-window:max-height-chars = current-window:height-chars.

run pi-trad-menu (input w-livre:menubar).

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */




PROCEDURE pi-after-initialize :
/*------------------------------------------------------------------------------
  Purpose:     C¢digo a ser executado ap¢s a inicializaá∆o
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if  valid-handle(h_p-exihel) then
      run set-prog-parent in h_p-exihel (program-name(1)).

  /*Traduá∆o dos campos de tela*/

/*fim traduá∆o dos campos de tela*/
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
         OUTPUT c-container).

    assign i-inicio     = r-index(THIS-PROCEDURE:file-name,"/") + 1
           i-fim        = r-index(THIS-PROCEDURE:file-name,".w").

    if i-fim < r-index(THIS-PROCEDURE:file-name,".r") then
       i-fim = r-index(THIS-PROCEDURE:file-name,".r").
    if i-inicio < r-index(THIS-PROCEDURE:file-name,"\") then
       i-inicio = r-index(THIS-PROCEDURE:file-name,"\") + 1.

    run utp/ut-cons.w (input substring(THIS-PROCEDURE:file-name,i-inicio , i-fim - i-inicio)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-disable-menu :
def var p-button-enable as char no-undo.
  
  RUN get-button-enable IN h_p-exihel (OUTPUT p-button-enable).
  assign menu-item mi-consultas:sensitive in menu m-livre = (entry(1,p-button-enable)= string(yes))
         menu-item mi-imprimir:sensitive in menu m-livre = (entry(2,p-button-enable)= string(yes))
         menu-item mi-sair:sensitive in menu m-livre = (entry(3,p-button-enable)= string(yes))
         menu-item mi-conteudo:sensitive in menu m-livre = (entry(4,p-button-enable)= string(yes))
         menu-item mi-sobre:sensitive in menu m-livre = (entry(4,p-button-enable)= string(yes)).

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

  CASE entry(1, p-state, "|":U):
      WHEN 'Consulta':U THEN DO:
          run pi-consulta.
      END.
  END.
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


/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   Custom                                                               */
/* BROWSE-TAB br-reporte bt-elimina f-cad */
/* SETTINGS FOR BROWSE br-reporte IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-can IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-elimina IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-inclui IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-modifica IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-ok IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-estabel IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-maquina IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-operador IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-estabel IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-maquina IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-operador IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-trans-f IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-trans-i IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-hr-trans-f IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-hr-trans-i IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tempo IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-turno IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */



/* Setting information for Queries and Browse Widgets fields            */


/* Query rebuild information for BROWSE br-reporte
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens-reporte.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-reporte */


 



/* ************************  Control Triggers  ************************ */


ON END-ERROR OF w-livre /* Template Livre <Insira complemento> */
OR ENDKEY OF w-livre ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON WINDOW-CLOSE OF w-livre /* Template Livre <Insira complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON END-ERROR OF br-reporte IN FRAME f-cad
ANYWHERE 
DO:
  
        if  br-reporte:new-row in frame f-cad then do:
            if avail tt-itens-reporte then
               delete tt-itens-reporte.
            if br-reporte:delete-current-row() in frame f-cad then. 
        end.                                                               
        else do:
            get current br-reporte.
            display tt-itens-reporte.it-codigo
                    tt-itens-reporte.desc-item 
                    tt-itens-reporte.quantidade 
                    tt-itens-reporte.cod-depos  
                    tt-itens-reporte.localizacao
                    tt-itens-reporte.lote       
                    tt-itens-reporte.val-lote   
                with browse br-reporte. 
        end.
        return no-apply.
  
END.

/* _UIB-CODE-BLOCK-END */




ON ROW-LEAVE OF br-reporte IN FRAME f-cad
DO:
  
      if br-reporte:NEW-ROW in frame f-cad then 
        do transaction on error undo, return no-apply:
            if input browse br-reporte tt-itens-reporte.it-codigo <> "" then do:
                create tt-itens-reporte.
                assign input browse br-reporte tt-itens-reporte.it-codigo
                       input browse br-reporte tt-itens-reporte.quantidade 
                       input browse br-reporte tt-itens-reporte.cod-depos  
                       input browse br-reporte tt-itens-reporte.localizacao
                       input browse br-reporte tt-itens-reporte.lote       
                       input browse br-reporte tt-itens-reporte.val-lote.
                find first item 
                    where item.it-codigo = input browse br-reporte tt-itens-reporte.it-codigo no-lock no-error.
                if avail item then 
                    assign tt-itens-reporte.desc-item = item.desc-item.

                br-reporte:CREATE-RESULT-LIST-ENTRY() in frame f-cad.
            end.
            /*else do:
                br-reporte:delete-current-row().
            end.*/
        end.
        else do transaction on error undo, return no-apply:
            assign input browse br-reporte tt-itens-reporte.it-codigo
                   input browse br-reporte tt-itens-reporte.quantidade 
                   input browse br-reporte tt-itens-reporte.cod-depos  
                   input browse br-reporte tt-itens-reporte.localizacao
                   input browse br-reporte tt-itens-reporte.lote       
                   input browse br-reporte tt-itens-reporte.val-lote.
            find first item 
                where item.it-codigo = input browse br-reporte tt-itens-reporte.it-codigo no-lock no-error.
            if avail item then 
                assign tt-itens-reporte.desc-item:screen-value = item.desc-item.
        end.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-can IN FRAME f-cad /* Can */
DO:
  run pi-desabilita.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-elimina IN FRAME f-cad /* Eliminar */
DO:
  if  br-reporte:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-reporte.
        delete tt-itens-reporte.
        if br-reporte:delete-current-row() in frame f-cad then.
    end.
    
    if num-results("br-reporte":U) = 0 then
       assign bt-modifica:SENSITIVE in frame f-cad = no
              bt-elimina:SENSITIVE in frame f-cad  = no.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-inclui IN FRAME f-cad /* Incluir */
DO:
    assign bt-modifica:SENSITIVE in frame f-cad = yes
           bt-elimina:SENSITIVE  in frame f-cad = yes.
    
    if num-results("br-reporte":U) > 0 then
        br-reporte:INSERT-ROW("after":U) in frame f-cad.
    else do transaction:
        create tt-itens-reporte.
        open query br-reporte for each tt-itens-reporte.
        apply "entry":U to tt-itens-reporte.it-codigo in browse br-reporte. 
    end.

END.

/* _UIB-CODE-BLOCK-END */

ON CHOOSE OF bt-conv IN FRAME f-cad /* Conv */
DO:
    RUN sfc/essf0001a.w.
END.


ON CHOOSE OF bt-mod IN FRAME f-cad /* Mod */
DO:
  

  enable fi-cod-estabel
         fi-dt-trans-i
         fi-dt-trans-f
         fi-hr-trans-i
         fi-hr-trans-f
         fi-cod-operador
         fi-turno
         fi-cod-maquina 
         br-reporte
         bt-inclui
         bt-can
         bt-ok
      with frame f-cad.

  disable bt-mod
          bt-conv 
       with frame f-cad.

  assign fi-cod-estabel:screen-value   = "{cdp\poloestab.i 422}" /*solic-318*/
         fi-dt-trans-i:screen-value    = string(today, "99/99/9999")
         fi-hr-trans-i:screen-value    = string(time, "HH:MM")
         fi-dt-trans-f:screen-value    = string(today, "99/99/9999")
         fi-hr-trans-f:screen-value    = string(time, "HH:MM")
         /*fi-cod-estabel:screen-value = if avail param-cp  then param-cp.cod-estabel  else ""*/
         fi-turno:screen-value       = if avail param-sfc then param-sfc.cod-model-turno else "".
  
  /* ========================================================================
    Rotina para sugerir o operador logado */



  FIND FIRST operador WHERE
      operador.char-2 = c-seg-usuario
      NO-LOCK NO-ERROR.

  IF AVAIL operador THEN DO:
      ASSIGN fi-cod-operador:SCREEN-VALUE = operador.cod-operador
             fi-desc-operador:SCREEN-VALUE = operador.nom-operador.
  END.

  ASSIGN fi-cod-operador:SENSITIVE = NO.


/* fim Rotina para sugerir o operador logado 
========================================================================   */


  
  apply "leave" to fi-cod-estabel in frame f-cad.
  apply "entry" to fi-cod-estabel in frame f-cad.

  return no-apply.

END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-modifica IN FRAME f-cad /* Modificar */
DO:
  apply 'entry':U to tt-itens-reporte.it-codigo in browse br-reporte. 
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-ok IN FRAME f-cad /* Ok */
DO:

  run utp/ut-acomp.p persistent set h-acomp.
  run pi-inicializar in h-acomp (input "Validando Itens...").
  run pi-desabilita-cancela in h-acomp.
  run pi-valida-reportes.
  if return-value = "NOK" then do:
     run pi-finalizar in h-acomp.
     return no-apply.
  end.
  run pi-finalizar in h-acomp.
  

  run pi-efetiva-reportes.
  if return-value = "NOK" then
     return no-apply.
  
  run pi-desabilita.
END.

/* _UIB-CODE-BLOCK-END */




ON F5 OF fi-cod-estabel IN FRAME f-cad /* Estabelec */
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
        
        assign c-lista-campo = string(fi-cod-estabel:handle in frame f-cad) + '|':U + 'cod-estabel'.
        
            assign c-lista-campo = c-lista-campo + chr(10) + string(fi-desc-estabel:handle in frame f-cad) + '|':U + 'nome'.
        
        
        
        
        
        
        
        
        
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




ON LEAVE OF fi-cod-estabel IN FRAME f-cad /* Estabelec */
DO:
  find first estabelec
       where estabelec.cod-estabel = input frame f-cad fi-cod-estabel 
       no-lock no-error.
  assign fi-desc-estabel:screen-value in frame f-cad = if avail estabelec then estabelec.nome else "".

END.

/* _UIB-CODE-BLOCK-END */




ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel IN FRAME f-cad /* Estabelec */
DO:
  apply "f5":U to self.
END.

/* _UIB-CODE-BLOCK-END */




ON F5 OF fi-cod-maquina IN FRAME f-cad /* Centro de Trab. */
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
      wh-pesquisa:FILE-NAME = "inzoom/z01in513.w":U then
        return.
      
  RUN inzoom/z01in513.w persistent set wh-pesquisa.

  if  not valid-handle(wh-pesquisa) or 
          wh-pesquisa:TYPE <> "PROCEDURE":U or 
          wh-pesquisa:FILE-NAME <> "inzoom/z01in513.w":U then
      return.
      
  
  

  RUN dispatch IN wh-pesquisa ('initialize':U).
  
  if valid-handle(wh-pesquisa) and
     wh-pesquisa:TYPE = "PROCEDURE":U and
     wh-pesquisa:FILE-NAME = "inzoom/z01in513.w":U then do:
    
        RUN pi-entry IN wh-pesquisa.
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(fi-cod-maquina:handle in frame f-cad) + '|':U + 'cod-ctrab'.
        
            assign c-lista-campo = c-lista-campo + chr(10) + string(fi-desc-maquina:handle in frame f-cad) + '|':U + 'des-ctrab'.
        
        
        
        
        
        
        
        
        
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




ON LEAVE OF fi-cod-maquina IN FRAME f-cad /* Centro de Trab. */
DO:
   find first ctrab
        where ctrab.cod-ctrab = input frame f-cad fi-cod-maquina
        no-lock no-error.
   assign fi-desc-maquina:screen-value in frame f-cad = if avail ctrab then ctrab.des-ctrab else "".

END.

/* _UIB-CODE-BLOCK-END */




ON MOUSE-SELECT-DBLCLICK OF fi-cod-maquina IN FRAME f-cad /* Centro de Trab. */
DO:
  apply "F5":U to self.
END.

/* _UIB-CODE-BLOCK-END */




ON F5 OF fi-cod-operador IN FRAME f-cad /* Operador */
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
        
        assign c-lista-campo = string(fi-cod-operador:handle in frame f-cad) + '|':U + 'cod-operador'.
        
            assign c-lista-campo = c-lista-campo + chr(10) + string(fi-desc-operador:handle in frame f-cad) + '|':U + 'nom-operador'.
        
        
        
        
        
        
        
        
        
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




ON LEAVE OF fi-cod-operador IN FRAME f-cad /* Operador */
DO:
  find first operador
       where operador.cod-operador = input frame f-cad fi-cod-operador 
       no-lock no-error.
  assign fi-desc-operador:screen-value in frame f-cad = if avail operador then operador.nom-operador else "".

END.

/* _UIB-CODE-BLOCK-END */




ON MOUSE-SELECT-DBLCLICK OF fi-cod-operador IN FRAME f-cad /* Operador */
DO:
  apply "F5":U to self.
END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF fi-hr-trans-f IN FRAME f-cad
DO:
  run pi-horas (input 2).
END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF fi-hr-trans-i IN FRAME f-cad
DO:
  run pi-horas (input 1).
END.

/* _UIB-CODE-BLOCK-END */




ON F5 OF fi-turno IN FRAME f-cad /* Turno */
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
      wh-pesquisa:FILE-NAME = "inzoom/z01in467":U then
        return.
      
  RUN inzoom/z01in467 persistent set wh-pesquisa.

  if  not valid-handle(wh-pesquisa) or 
          wh-pesquisa:TYPE <> "PROCEDURE":U or 
          wh-pesquisa:FILE-NAME <> "inzoom/z01in467":U then
      return.
      
  
  

  RUN dispatch IN wh-pesquisa ('initialize':U).
  
  if valid-handle(wh-pesquisa) and
     wh-pesquisa:TYPE = "PROCEDURE":U and
     wh-pesquisa:FILE-NAME = "inzoom/z01in467":U then do:
    
        RUN pi-entry IN wh-pesquisa.
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(fi-turno:handle in frame f-cad) + '|':U + 'cod-model-turno'.
        
        
        
        
        
        
        
        
        
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




ON MOUSE-SELECT-DBLCLICK OF fi-turno IN FRAME f-cad /* Turno */
DO:
  apply "F5":U to self.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME f-cad
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */




ON MENU-DROP OF MENU mi-programa /* Reciclado */
DO:
  run pi-disable-menu.
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







/* ***************************  Main Block  *************************** */
fi-cod-estabel:load-mouse-pointer("image/lupa.cur") in frame f-cad.
fi-cod-operador:load-mouse-pointer("image/lupa.cur") in frame f-cad.
fi-turno:load-mouse-pointer("image/lupa.cur") in frame f-cad.
fi-cod-maquina:load-mouse-pointer("image/lupa.cur") in frame f-cad.

/* Include custom  Main Block code for SmartWindows. */
/* windowmn.i - Main Block code for objects which create windows.*/
/* Skip all of this if no window was created. */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
IF VALID-HANDLE(w-livre) THEN DO:
    ASSIGN CURRENT-WINDOW                = w-livre 
       w-livre:KEEP-FRAME-Z-ORDER = YES
       THIS-PROCEDURE:CURRENT-WINDOW = w-livre.

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

 

ON 'f5':U OF tt-itens-reporte.it-codigo in browse br-reporte or
   'mouse-select-dblclick':U OF tt-itens-reporte.it-codigo in browse br-reporte
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
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(tt-itens-reporte.it-codigo:handle in browse br-reporte) + '|':U + 'it-codigo'.
        
            assign c-lista-campo = c-lista-campo + chr(10) + string(tt-itens-reporte.desc-item:handle in browse br-reporte) + '|':U + 'desc-item'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    

    
  end.
                    
assign l-implanta = no.


wait-for close of wh-pesquisa.


/* _UIB-CODE-BLOCK-END */



  

    RETURN.
END.

ON 'leave':U OF tt-itens-reporte.it-codigo in browse br-reporte 
DO:
        find first param-global no-lock no-error.
        find first param-cq no-lock no-error.
      

        find first item where item.it-codigo = input browse br-reporte tt-itens-reporte.it-codigo
            no-lock no-error.
        if avail item then do:
           if avail tt-itens-reporte then
              assign tt-itens-reporte.desc-item = item.desc-item.
           else
              assign tt-itens-reporte.desc-item:screen-value in browse br-reporte = item.desc-item.
           ASSIGN tt-itens-reporte.val-lote:screen-value in browse br-reporte = string( TODAY + 180 ).

           if avail tt-itens-reporte then
              disp tt-itens-reporte.desc-item
               with browse br-reporte.
        end.
        else do:
            if avail tt-itens-reporte then
               assign tt-itens-reporte.desc-item = "":U.
            else
               assign tt-itens-reporte.desc-item:screen-value in browse br-reporte = "":U.
            
            ASSIGN tt-itens-reporte.val-lote:screen-value in browse br-reporte = string( TODAY + 180 ).

        end.

        if avail item then DO:  
           IF AVAIL tt-reporte THEN
           disp tt-itens-reporte.desc-item
               with browse br-reporte.

    
    
    
    /*            find first b-tt-itens-reporte                                                                    */
    /*                 where b-tt-itens-reporte.it-codigo = input browse {&browse-name} tt-itens-reporte.it-codigo */
    /*                   and rowid(b-tt-itens-reporte) <> rowid(tt-itens-reporte) no-lock no-error.                */
    /*            if avail b-tt-itens-reporte then do:                                                             */
    /*               run utp/ut-msgs.p (input "show",                                                              */
    /*                                  input 17006,                                                               */
    /*                                  input "Item j† informado").                                                */
    /*               apply "entry":U to tt-itens-reporte.it-codigo in browse {&browse-name}.                       */
    /*               return no-apply.                                                                              */
    /*            end.                                                                                             */
    
               /*Ajusta o  deposito de CQ*/
            IF param-global.modulo-cq and 
               param-cq.tipo-cq       > 1 and
               STRING (f-item-uni-estab(item.it-codigo,
                                        IF fi-cod-estabel:screen-value in frame f-cad = '' THEN item.cod-estabel
                                        ELSE fi-cod-estabel:screen-value in frame f-cad ,
                                        "contr-qualid":U)) = "yes":U THEN
               FOR FIRST estabelec FIELDS (deposito-cq) 
                   WHERE estabelec.cod-estabel = IF fi-cod-estabel:screen-value in frame f-cad = ''
                                                     THEN item.cod-estabel
                                                     ELSE fi-cod-estabel:screen-value in frame f-cad  NO-LOCK:
                             assign tt-itens-reporte.cod-depos:screen-value in browse br-reporte = estabelec.deposito-cq.
               END.
            ELSE 
                assign tt-itens-reporte.cod-depos:screen-value in browse br-reporte = f-item-uni-estab(item.it-codigo,
                                                                                          IF fi-cod-estabel:screen-value in frame f-cad = ''
                                                                                          THEN item.cod-estabel
                                                                                          ELSE fi-cod-estabel:screen-value in frame f-cad , "deposito-pad":U).
            assign tt-itens-reporte.localizacao:screen-value in browse br-reporte = f-item-uni-estab(item.cod-localiz,
                                                                                                            IF fi-cod-estabel:screen-value in frame f-cad = ''
                                                                                                            THEN item.cod-localiz
                                                                                                            ELSE fi-cod-estabel:screen-value in frame f-cad , "cod-localiz":U).
        END.
    RETURN.
END.

ON 'f5':U OF tt-itens-reporte.cod-depos in browse br-reporte or
   'mouse-select-dblclick':U OF tt-itens-reporte.cod-depos in browse br-reporte
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
      wh-pesquisa:FILE-NAME = "inzoom/z01in084.w":U then
        return.
      
  RUN inzoom/z01in084.w persistent set wh-pesquisa.

  if  not valid-handle(wh-pesquisa) or 
          wh-pesquisa:TYPE <> "PROCEDURE":U or 
          wh-pesquisa:FILE-NAME <> "inzoom/z01in084.w":U then
      return.
      
  
  

  RUN dispatch IN wh-pesquisa ('initialize':U).
  
  if valid-handle(wh-pesquisa) and
     wh-pesquisa:TYPE = "PROCEDURE":U and
     wh-pesquisa:FILE-NAME = "inzoom/z01in084.w":U then do:
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(tt-itens-reporte.cod-depos:handle in browse br-reporte) + '|':U + 'cod-depos'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    

    
  end.
                    
assign l-implanta = no.


wait-for close of wh-pesquisa.


/* _UIB-CODE-BLOCK-END */



  

    RETURN.
END.

ON 'f5':U OF tt-itens-reporte.localizacao in browse br-reporte or
   'mouse-select-dblclick':U OF tt-itens-reporte.localizacao in browse br-reporte
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
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(tt-itens-reporte.localizacao:handle in browse br-reporte) + '|':U + 'cod-localiz'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    

    
  end.
                    
assign l-implanta = no.


wait-for close of wh-pesquisa.


/* _UIB-CODE-BLOCK-END */



     
    RETURN.
END.

ON 'f5':U OF tt-itens-reporte.lote in browse br-reporte or
   'mouse-select-dblclick':U OF tt-itens-reporte.lote in browse br-reporte
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
      wh-pesquisa:FILE-NAME = "inzoom/z02in403.w":U then
        return.
      
  RUN inzoom/z02in403.w persistent set wh-pesquisa.

  if  not valid-handle(wh-pesquisa) or 
          wh-pesquisa:TYPE <> "PROCEDURE":U or 
          wh-pesquisa:FILE-NAME <> "inzoom/z02in403.w":U then
      return.
      
  
  

  RUN dispatch IN wh-pesquisa ('initialize':U).
  
  if valid-handle(wh-pesquisa) and
     wh-pesquisa:TYPE = "PROCEDURE":U and
     wh-pesquisa:FILE-NAME = "inzoom/z02in403.w":U then do:
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(tt-itens-reporte.lote:handle in browse br-reporte) + '|':U + 'lote'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    

    
  end.
                    
assign l-implanta = no.


wait-for close of wh-pesquisa.


/* _UIB-CODE-BLOCK-END */



  

    RETURN.
END.

procedure WinExec external "kernel32.dll":
    define input parameter prog_name    as character.
    define input parameter visual_style as short.
end procedure.

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
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.13 , 74.72 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-elimina:HANDLE IN FRAME f-cad , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
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
  DISPLAY fi-cod-estabel fi-desc-estabel fi-turno fi-cod-operador 
          fi-desc-operador fi-cod-maquina fi-desc-maquina fi-dt-trans-i 
          fi-hr-trans-i fi-dt-trans-f fi-hr-trans-f fi-tempo 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE bt-mod RECT-1 RECT-3 rt-button 
      WITH FRAME f-cad IN WINDOW w-livre.
  OPEN QUERY br-reporte FOR EACH tt-itens-reporte.
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE gera-mob-ggf-automatico :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 def var d-tempo as decimal no-undo.

  find first param-sfc no-lock no-error.
  IF NOT AVAIL param-sfc THEN return "NOK":U.

  /* Calcula tempo reporte */
  RUN pi-calcula-tempo-mob-ggf (OUTPUT d-tempo).

  /* Gera GGF autom†tico - Carrega automaticamente o browser MOB/GGF do SF0303B 
   com os movimentos de GGF da operaá∆o corrente */

IF param-sfc.log-gera-ggf-autom and
   ord-prod.reporte-ggf = 1 THEN DO:  
   
   CREATE tt-apont-mob.
   assign tt-apont-mob.nr-ord-prod     =  split-operac.nr-ord-produ
          tt-apont-mob.op-codigo       =  split-operac.op-codigo
          tt-apont-mob.it-codigo       =  split-operac.it-codigo
          tt-apont-mob.cod-roteiro     =  split-operac.cod-roteiro
          tt-apont-mob.tipo-movto      =  2
          tt-apont-mob.gm-codigo       =  split-operac.gm-codigo
          tt-apont-mob.hora-fim        =  int(entry(1,fi-hr-trans-f:SCREEN-VALUE IN FRAME f-cad,":"))            
          tt-apont-mob.hora-ini        =  int(entry(1,fi-hr-trans-i:SCREEN-VALUE IN FRAME f-cad,":"))  
          tt-apont-mob.min-fim         =  int(entry(2,fi-hr-trans-f:SCREEN-VALUE IN FRAME f-cad,":"))            
          tt-apont-mob.min-ini         =  int(entry(2,fi-hr-trans-i:SCREEN-VALUE IN FRAME f-cad,":"))
          tt-apont-mob.tempo           =  d-tempo
          tt-apont-mob.referencia      =  ord-prod.cod-refer.
   /* Rel¢gio Hexadecimal ou centesimal */                                                   

   IF INT(param-sfc.log-tipo-relogio) = 1 THEN
      ASSIGN tt-apont-mob.tipo-relogio = 1.
   ELSE 
      ASSIGN tt-apont-mob.tipo-relogio = 3. 
   
END.

/* Gera MOB autom†tico - Carrega automaticamente o browser MOB/GGF do SF0303B 
   com os movimentos de MOB da operaá∆o corrente */

IF param-sfc.log-gera-mod-autom and
   ord-prod.reporte-mob = 1  THEN DO:
   
    CREATE tt-apont-mob.
    assign tt-apont-mob.nr-ord-prod     =  split-operac.nr-ord-produ
           tt-apont-mob.op-codigo       =  split-operac.op-codigo
           tt-apont-mob.it-codigo       =  split-operac.it-codigo
           tt-apont-mob.cod-roteiro     =  split-operac.cod-roteiro
           tt-apont-mob.tipo-movto      =  1
           tt-apont-mob.cd-mob-dir      =  ""
           tt-apont-mob.gm-codigo       =  split-operac.gm-codigo
           tt-apont-mob.hora-fim        =  int(entry(1,fi-hr-trans-f:SCREEN-VALUE IN FRAME f-cad,":"))            
           tt-apont-mob.hora-ini        =  int(entry(1,fi-hr-trans-i:SCREEN-VALUE IN FRAME f-cad,":"))  
           tt-apont-mob.min-fim         =  int(entry(2,fi-hr-trans-f:SCREEN-VALUE IN FRAME f-cad,":"))            
           tt-apont-mob.min-ini         =  int(entry(2,fi-hr-trans-i:SCREEN-VALUE IN FRAME f-cad,":"))
           tt-apont-mob.tempo           =  d-tempo 
           tt-apont-mob.referencia      =  ord-prod.cod-refer. 

    

    /* Rel¢gio Hexadecimal ou centesimal */                                                   
    IF INT(param-sfc.log-tipo-relogio) = 1 THEN
       ASSIGN tt-apont-mob.tipo-relogio = 1.
    ELSE 
       ASSIGN tt-apont-mob.tipo-relogio = 3. 
END.
    
RETURN "OK":U.

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
  run pi-before-initialize.

  /***********************************************************************
**
**  WIN-SIZE.I - Realiza o ajuste no tamanho da window e da frame
**               igualando ambos
*************************************************************************/

if w-livre:width-chars < frame f-cad:width-chars then
    assign frame f-cad:width-chars = w-livre:width-chars.
else if frame f-cad:width-chars < w-livre:width-chars then
    assign w-livre:width-chars = frame f-cad:width-chars.

if w-livre:height-chars < frame f-cad:height-chars then
    assign frame f-cad:height-chars = w-livre:height-chars.
else if frame f-cad:height-chars < w-livre:height-chars then
    assign w-livre:height-chars = frame f-cad:height-chars.

assign w-livre:virtual-width-chars  = w-livre:width-chars
       w-livre:virtual-height-chars = w-livre:height-chars
       w-livre:min-width-chars      = w-livre:width-chars
       w-livre:max-width-chars      = w-livre:width-chars
       w-livre:min-height-chars     = w-livre:height-chars
       w-livre:max-height-chars     = w-livre:height-chars.

/* win-size.i */
 

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

    assign c-programa-mg97 = caps("essf0001")
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
     
    
         assign w-livre:title = if l-achou-prog then
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
      if "w-livre" = "SmartDialog" or this-procedure:persistent = no then
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
      if "w-livre" = "SmartDialog" then
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
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.
        end.
        when 3 then do: /* Cadastro Simples - Alteracao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.
        end.
        when 4 then do: /* Cadastro Complexo */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 5 then do: /* Cadastro Complexo - Alteracao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 6 then do: /* Cadastro Simples - Inclusao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 7 then do: /* Cadastro PaiXFilho - Atualiza Filho */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 8 then do: /* Cadastro PaiXFilho - Atualiza Ambos */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
            
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 9 then do: /* Inclui/Modifica Pai */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 10 then do: /* Inclui/Modifica Filho */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 11 then do: /* Formacao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 12 then do: /* Parametros Unicos */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 13 then do: /* Pesquisa */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 14 then do: /* Consulta Simples */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 15 then do: /* Consulta Complexa */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 16 then do: /* Consulta Relacionamento */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 17 then do: /* Relatorio */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 20 then do: /* Janela Detalhe */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD.

            if  h-pai:handle = w-livre:handle then
                assign h-pai           = h-pai:NEXT-SIBLING
                       h-pai:SENSITIVE = no.
            else 
                assign  h-pai = w-livre:handle
                        h-pai = h-pai:parent.

            h-pai:sensitive = no.
  
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 21 then do: /* Janela Mestre */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 26 then do: /* Importacao/Exportacao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.
        end.
        when 29 then do: /* Relatorio Gerado Pelo DataViewer */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.
        end.
        when 30 then do: /* Formacao Sem Navegacao */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.
        end.
        when 31 then do: /* Estrutura */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.
        end.
        when 32 then do: /* Digitaá∆o Rapida */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
            
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.
        end.
    end case.

/* Transformacao Window *****************************************************/

 
      end. 
    



/* ut9000.i */
 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.

  find first param-cp  no-lock no-error.
  find first param-sfc no-lock no-error.
  find first param-global no-lock no-error.
  find first param-cs no-lock no-error.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-calcula-tempo-mob-ggf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE OUTPUT PARAM p-tempo AS DECIMAL NO-UNDO.

   def var de-qtd-segs-inic-rep as decimal no-undo.
   def var de-qtd-segs-fim-rep  as decimal no-undo.
   def var de-qtd-tempo-util    as decimal no-undo.
   def var da-ini-aux           as date no-undo.
   def var da-fim-aux           as date no-undo.

   def var v-qtd-tempo-proces  as decimal.
   def var v-qtd-tempo-extra   as decimal.
   def var v-cod-model-turno   as char.
   def var v-num-turno         as integer. 

   def var v-val-refer-inic-rep as decimal no-undo.
   def var v-val-refer-fim-rep  as decimal no-undo.

   def var v-qtd-tempo-parada as decimal no-undo.
   def var v-qtd-tempo-parada-ext as decimal no-undo.
   
   if  param-sfc.log-tipo-relogio then do:
          run pi-formatted-time-to-sec (input (input frame f-cad fi-hr-trans-i),
                                        output de-qtd-segs-inic-rep).
          run pi-formatted-time-to-sec (input  (input frame f-cad fi-hr-trans-f),
                                        output de-qtd-segs-fim-rep).
      end.
      else
          assign de-qtd-segs-inic-rep = integer(input frame f-cad fi-hr-trans-i) * 36
                 de-qtd-segs-fim-rep  = integer(input frame f-cad fi-hr-trans-f)  * 36.

      if  input frame f-cad fi-dt-trans-i  <> ?
      and input frame f-cad fi-dt-trans-f <> ? then do:
          assign de-qtd-tempo-util = ?
                 da-ini-aux = date(string(input frame f-cad fi-dt-trans-i,"99/99/9999"))
                 da-fim-aux = date(string(input frame f-cad fi-dt-trans-f,"99/99/9999")).
             

             run pi-sfc-reporte-tempo IN h-boin536 (input split-operac.cod-ctrab,  
                                                  input da-fim-aux, 
                                                  input de-qtd-segs-fim-rep, 
                                                  input da-ini-aux, 
                                                  input de-qtd-segs-inic-rep, 
                                                  output v-qtd-tempo-proces, 
                                                  output v-qtd-tempo-extra, 
                                                  output v-cod-model-turno, 
                                                  output v-num-turno). 
                                                                 
             find grup-maquina where
                  grup-maquina.gm-codigo = split-operac.gm-codigo no-lock.
             IF AVAIL grup-maquina THEN DO:
                run pi-converte-data-segs-valor (input  da-ini-aux,
                                                 input  de-qtd-segs-inic-rep,
                                                 output v-val-refer-inic-rep).

                run pi-converte-data-segs-valor (input  da-fim-aux,
                                                 input  de-qtd-segs-fim-rep,
                                                 output v-val-refer-fim-rep).


                if grup-maquina.ind-tip-ctrab = 4  then 
                   run pi-sfc-tempo-paradas-mod IN h-boin536 (input  split-operac.cod-ctrab,
                                                            input  split-operac.gm-codigo,
                                                            input  "",
                                                            input  v-val-refer-inic-rep,
                                                            input  v-val-refer-fim-rep,
                                                            output v-qtd-tempo-parada,
                                                            output v-qtd-tempo-parada-ext). 
                else 
                  run pi-sfc-tempo-paradas-maq IN h-boin536  (input  split-operac.cod-ctrab,
                                                              input  split-operac.gm-codigo,
                                                              input  v-val-refer-inic-rep,
                                                              input  v-val-refer-fim-rep,
                                                              output v-qtd-tempo-parada,
                                                              output v-qtd-tempo-parada-ext). 
                                                                   
                ASSIGN p-tempo =   v-qtd-tempo-proces + v-qtd-tempo-extra - v-qtd-tempo-parada.
             END.

      end. 
      RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-cria-ops :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    run pi-acompanhar in h-acomp (input "Gera OP - Item: " + tt-itens-reporte.it-codigo).

    find item 
        where item.it-codigo = tt-itens-reporte.it-codigo no-lock no-error.

    create tt-ord-prod.
    assign tt-ord-prod.cod-versao-integracao = 003
           tt-ord-prod.cod-estabel           = input frame f-cad fi-cod-estabel
           tt-ord-prod.cod-depos             = tt-itens-reporte.cod-depos
           tt-ord-prod.cod-refer             = ""
           tt-ord-prod.lote-serie            = tt-itens-reporte.lote
           tt-ord-prod.prod-rep              = no
           tt-ord-prod.rep-prod              = 1
           tt-ord-prod.estado                = 1
           tt-ord-prod.dt-termino            = input frame f-cad fi-dt-trans-f
           tt-ord-prod.dt-orig               = input frame f-cad fi-dt-trans-f
           tt-ord-prod.it-codigo             = tt-itens-reporte.it-codigo
           tt-ord-prod.nr-linha              = int (f-item-uni-estab (input tt-itens-reporte.it-codigo,
                                                                      input input frame f-cad fi-cod-estabel,
                                                                      "nr-linha"))
           tt-ord-prod.un                    = item.un
           tt-ord-prod.origem                = "CP":U
           tt-ord-prod.qt-ordem              = tt-itens-reporte.quantidade
           tt-ord-prod.nr-sequencia          = 1
           tt-ord-prod.dt-inicio             = input frame f-cad fi-dt-trans-i
           tt-ord-prod.dt-emissao            = input frame f-cad fi-dt-trans-i
           tt-ord-prod.prog-seg              = "essf0001"
           tt-ord-prod.conta-ordem           = param-cp.conta-ordem
           tt-ord-prod.nr-ord-produ          = 0
           tt-ord-prod.faixa-numeracao       = 2
           tt-ord-prod.ind-tipo-movto        = 1
           tt-ord-prod.gera-relacionamentos  = yes
           tt-ord-prod.aloca-reserva         = no
           tt-ord-prod.aloca-lote            = no
           tt-ord-prod.narrativa             = "Ordem de produá∆o gerada por programa espec°fico! (POLO)"
           tt-ord-prod.tipo                  = 1.
           /*Moises Pereira 06/05/2015 - Revisao Custos Polo
           Alteracao do Tipo de Ordem de 9 Reaproveitamento para 1 Interna, para nao termos mais problema de valorizacao pelo medio do màs anterior*/
                                   
    if param-global.modulo-cs = yes then do:
       assign tt-ord-prod.calc-cs-mob = param-cs.calc-cs-mob
              tt-ord-prod.calc-cs-mat = param-cs.calc-cs-mat
              tt-ord-prod.calc-cs-ggf = param-cs.calc-cs-ggf
              tt-ord-prod.reporte-mob = int (f-item-uni-estab (input tt-itens-reporte.it-codigo,
                                                               input input frame f-cad fi-cod-estabel,
                                                               "reporte-mob"))
              tt-ord-prod.reporte-ggf = int (f-item-uni-estab (input tt-itens-reporte.it-codigo,
                                                               input input frame f-cad fi-cod-estabel,
                                                               "reporte-ggf")).
    end.  
    
    
    find lin-prod 
         where lin-prod.cod-estabel = tt-ord-prod.cod-estabel
           and lin-prod.nr-linha    = tt-ord-prod.nr-linha no-lock no-error.
    
    assign tt-ord-prod.cd-planejado = if avail lin-prod
                                      then if lin-prod.sum-requis = 1
                                           then lin-prod.cd-planejad
                                           else f-item-uni-estab (input tt-itens-reporte.it-codigo,
                                                                  input input frame f-cad fi-cod-estabel,
                                                                  "cd-planejado")
                                      else f-item-uni-estab (input tt-itens-reporte.it-codigo,
                                                             input input frame f-cad fi-cod-estabel,
                                                             "cd-planejado").                                                                     


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-desabilita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
clear frame f-cad all no-pause.

for each tt-itens-reporte:
    delete tt-itens-reporte.
end.
open query br-reporte for each tt-itens-reporte.

disable fi-cod-estabel
        fi-dt-trans-i
        fi-dt-trans-f
        fi-hr-trans-i
        fi-hr-trans-f
        fi-cod-operador
        fi-turno
        fi-cod-maquina 
        br-reporte
        bt-inclui
        bt-modifica
        bt-elimina
        bt-can
        bt-ok
     with frame f-cad.

enable bt-mod
       bt-conv
     with frame f-cad.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-efetiva-reportes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def buffer b-item for item.

def var c-arq-erro as char no-undo.

def var de-saldo       like movto-estoq.quantidade init 0 no-undo.
def var de-resto       like movto-estoq.quantidade init 0 no-undo.
def var de-tot-saldo   like movto-estoq.quantidade init 0 no-undo.
def var de-qt-anterior like movto-estoq.quantidade init 0 no-undo.
def var de-necessario  like movto-estoq.quantidade init 0 no-undo.

def var i-seq-reservas as int no-undo.

run utp/ut-acomp.p persistent set h-acomp.

for each tt-relatorio:
    delete tt-relatorio.
end.
for each tt-rel-aux:
    delete tt-rel-aux.
end.
for each tt-lote-a-config:
    delete tt-lote-a-config.
end.
for each tt-reservas:
    delete tt-reservas.
end.

RUN inbo/boin533.p PERSISTENT SET h-boin533.
RUN inbo/boin536.p PERSISTENT SET h-boin536.

blk-geral:
for each tt-itens-reporte exclusive-lock
    transaction on stop undo blk-geral, next blk-geral
                on error undo blk-geral, next blk-geral:

    run pi-inicializar in h-acomp (input "Item: " + tt-itens-reporte.it-codigo).

    run pi-acompanhar in h-acomp (input "Gerando OP...").
    
    run pi-desabilita-cancela in h-acomp.

    for each tt-ord-prod:
        delete tt-ord-prod.
    end.
    for each tt-erro:
        delete tt-erro.
    end.

    run pi-cria-ops.

    run cpp/cpapi301.p (input-output table tt-ord-prod,
                        input-output table tt-reapro,
                        input-output table tt-erro,
                        no).
    if return-value = "NOK":U then do:
       create tt-relatorio.
       assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
              tt-relatorio.descricao = "Erro ao criar a OP.".
       for each tt-erro:
           create tt-rel-aux.
           assign tt-rel-aux.it-codigo = tt-relatorio.it-codigo
                  tt-rel-aux.i-sequen  = tt-erro.i-sequen
                  tt-rel-aux.cd-erro   = tt-erro.cd-erro
                  tt-rel-aux.descricao = tt-erro.mensagem.
       end.
       undo blk-geral, next blk-geral.
    end.

    /*INICIO Reservas*/
    run pi-acompanhar in h-acomp (input "Verificando Saldo em Estoque...").
    
    /* limpa temp-tabLe - lotes data-validade - EDSON 31-10-2006*/ 
    FOR EACH tt-rw-tt-saldo-estoq .
        DELETE tt-rw-tt-saldo-estoq .
    END.


    find first tt-ord-prod exclusive-lock no-error.
    find ord-prod
         where rowid(ord-prod) = tt-ord-prod.rw-ord-prod no-lock no-error.
    if avail ord-prod then
       assign tt-ord-prod.nr-ord-produ = ord-prod.nr-ord-produ.

    for each reservas
       where reservas.nr-ord-produ = tt-ord-prod.nr-ord-produ exclusive-lock:
        delete reservas.
    end.

    /*Recomposicao de saldo na data de corte*/
    EMPTY TEMP-TABLE tt-saldo-estoq.
    for each saldo-estoq NO-LOCK 
       where saldo-estoq.cod-estabel = input frame f-cad fi-cod-estabel
         and saldo-estoq.cod-depos   = substr(param-cp.char-2,1,3)
         and saldo-estoq.cod-localiz = tt-itens-reporte.it-codigo 
         AND saldo-estoq.qtidade-atu > 0:

        CREATE tt-saldo-estoq.
        BUFFER-COPY saldo-estoq TO tt-saldo-estoq
        ASSIGN tt-saldo-estoq.log-ajuste = YES.
    END.

    FOR EACH movto-estoq NO-LOCK USE-INDEX estab-dep
        WHERE movto-estoq.cod-estabel = INPUT FRAME f-cad fi-cod-estabel
          AND movto-estoq.dt-trans    > input frame f-cad fi-dt-trans-f
          AND movto-estoq.cod-depos   = substr(param-cp.char-2,1,3)
          AND movto-estoq.cod-localiz = tt-itens-reporte.it-codigo:
        
        FIND FIRST tt-saldo-estoq NO-LOCK
            WHERE tt-saldo-estoq.cod-estabel = movto-estoq.cod-estabel 
              AND tt-saldo-estoq.cod-depos   = movto-estoq.cod-depos   
              AND tt-saldo-estoq.cod-localiz = movto-estoq.cod-localiz 
              AND tt-saldo-estoq.it-codigo   = movto-estoq.it-codigo   
              AND tt-saldo-estoq.cod-refer   = movto-estoq.cod-refer   
              AND tt-saldo-estoq.lote        = movto-estoq.lote        NO-ERROR.
        IF NOT AVAIL tt-saldo-estoq THEN DO:
            FIND FIRST saldo-estoq NO-LOCK                                       
                WHERE saldo-estoq.cod-estabel = movto-estoq.cod-estabel          
                  AND saldo-estoq.cod-depos   = movto-estoq.cod-depos            
                  AND saldo-estoq.cod-localiz = movto-estoq.cod-localiz          
                  AND saldo-estoq.it-codigo   = movto-estoq.it-codigo            
                  AND saldo-estoq.cod-refer   = movto-estoq.cod-refer            
                  AND saldo-estoq.lote        = movto-estoq.lote        NO-ERROR.
            IF NOT AVAIL saldo-estoq THEN NEXT.

            CREATE tt-saldo-estoq.
            BUFFER-COPY saldo-estoq TO tt-saldo-estoq
            ASSIGN tt-saldo-estoq.log-ajuste = YES.
        END.
        
        ASSIGN tt-saldo-estoq.qtidade-atu = tt-saldo-estoq.qtidade-atu + (movto-estoq.quantidade * (IF movto-estoq.tipo-trans = 1 /*Entrada*/ THEN -1 ELSE 1)).
    END.

    /*Implementaá∆o Custos*/
    
    /*Foram implementadas duas rotinas 
    piAjustaItemRefugo   - Converte Saldo do item final para item refugo, esta rotina 
                           n∆o ser† mais utilizada e ser† substituida pela rotina que 
                           apenas desconsidera o saldo 
    piSaldoItemNaoRefugo - Esta rotina tem por objetivo desconsiderar o saldo dos itens
                           que possuem item refugo parametrizado no CD1112                         
    */
    /*RUN piAjustaItemRefugo.*/
    RUN piSaldoItemNaoRefugo.    
    IF RETURN-VALUE = "NOK" THEN DO:
        create tt-relatorio.
        assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
               tt-relatorio.descricao = "Erro realizar a troca de saldo item refugo".
        for each tt-erro:
            create tt-rel-aux.
            assign tt-rel-aux.it-codigo = tt-relatorio.it-codigo
                   tt-rel-aux.i-sequen  = tt-erro.i-sequen
                   tt-rel-aux.cd-erro   = tt-erro.cd-erro
                   tt-rel-aux.descricao = tt-erro.mensagem.
        end.
        undo blk-geral, next blk-geral.
    END.
    
    if not can-find(first tt-saldo-estoq
                    where tt-saldo-estoq.cod-estabel = input frame f-cad fi-cod-estabel
                      and tt-saldo-estoq.cod-depos   = substr(param-cp.char-2,1,3)
                      and tt-saldo-estoq.cod-localiz = tt-itens-reporte.it-codigo no-lock) then do:
       create tt-relatorio.
       assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
              tt-relatorio.descricao = "N∆o existem materiais em estoque para produzir o reciclado!".
       undo blk-geral, next blk-geral.
    end.

    assign de-tot-saldo   = 0
           de-necessario  = tt-ord-prod.qt-ordem
           i-seq-reservas = 1.


    blk-saldo:
    for each tt-saldo-estoq
       where tt-saldo-estoq.cod-estabel = input frame f-cad fi-cod-estabel
         and tt-saldo-estoq.cod-depos   = substr(param-cp.char-2,1,3)
         and tt-saldo-estoq.cod-localiz = tt-itens-reporte.it-codigo 
         AND tt-saldo-estoq.qtidade-atu > 0 
         NO-LOCK 
         BREAK BY tt-saldo-estoq.dt-fabric :

         find b-item where b-item.it-codigo = tt-saldo-estoq.it-codigo no-lock no-error.

         assign de-saldo  = tt-saldo-estoq.qtidade-atu - (tt-saldo-estoq.qt-alocada + tt-saldo-estoq.qt-aloc-prod + tt-saldo-estoq.qt-aloc-ped).

         if de-saldo <= 0 then 
            next blk-saldo.
         
         find first reservas exclusive-lock where
                    reservas.nr-ord-produ = tt-ord-prod.nr-ord-produ and
                    reservas.item-pai     = tt-ord-prod.it-codigo    and
                    reservas.cod-roteiro  = ""                       and
                    reservas.op-codigo    = 0                        and
                    reservas.it-codigo    = tt-saldo-estoq.it-codigo    and
                    reservas.cod-refer    = tt-saldo-estoq.cod-refer no-error.
         if not avail reservas then do:
             create reservas.
             assign reservas.nr-ord-produ = tt-ord-prod.nr-ord-produ
                    reservas.item-pai     = tt-ord-prod.it-codigo
                    reservas.cod-roteiro  = ""
                    reservas.op-codigo    = 0
                    reservas.it-codigo    = tt-saldo-estoq.it-codigo
                    reservas.cod-refer    = tt-saldo-estoq.cod-refer
                    reservas.cod-depos    = tt-saldo-estoq.cod-depos
                    reservas.cod-localiz  = tt-saldo-estoq.cod-localiz
                    reservas.lote-serie   = tt-saldo-estoq.lote
                    reservas.tipo-sobra   = 4 /*Normal*/
                    reservas.dt-reserva   = tt-ord-prod.dt-inicio
                    reservas.un           = b-item.un
                    reservas.tp-atualiza  = tt-ord-prod.tipo
                    reservas.estado       = 1 /*Ativa*/.
         end.

         create tt-reservas.
         buffer-copy reservas to tt-reservas
             assign tt-reservas.rw-reserva   = rowid(reservas)
                    tt-reservas.lote-serie   = tt-saldo-estoq.lote
               /*     tt-reservas.selec        = yes */
                    tt-reservas.sequencia    = i-seq-reservas
                    tt-reservas.cod-localiz  = tt-saldo-estoq.cod-localiz
                    i-seq-reservas           = i-seq-reservas + 1.

        /* para validar o lote utilizado - edson 30-10-2006*/
         CREATE tt-rw-tt-saldo-estoq .
         ASSIGN  tt-rw-tt-saldo-estoq.rw-tt-saldo-estoq = ROWID(tt-saldo-estoq).

         if de-saldo < de-necessario then do:
            assign de-resto                 = de-necessario - de-saldo
                   reservas.quant-orig      = reservas.quant-orig + de-saldo
                   tt-reservas.quant-requis = de-saldo
                   de-tot-saldo             = de-tot-saldo + de-saldo
                   de-saldo                 = 0
                   de-necessario            = de-resto.
            next blk-saldo.
         end.
         else do:
            assign reservas.quant-orig      = reservas.quant-orig + de-necessario
                   tt-reservas.quant-requis = de-necessario
                   de-tot-saldo             = de-tot-saldo + de-necessario.
            leave blk-saldo.
         end.

    end.

    for each reservas no-lock where
             reservas.nr-ord-produ = tt-ord-prod.nr-ord-produ:

        for each tt-reservas where
                 tt-reservas.nr-ord-produ = reservas.nr-ord-produ and
                 tt-reservas.item-pai     = reservas.item-pai     and
                 tt-reservas.cod-roteiro  = reservas.cod-roteiro  and
                 tt-reservas.op-codigo    = reservas.op-codigo    and
                 tt-reservas.it-codigo    = reservas.it-codigo    and
                 tt-reservas.cod-refer    = reservas.cod-refer:

            assign tt-reservas.quant-orig = reservas.quant-orig.

        end.

    end.
    
    /*Nao existe saldo, entao a OP nao pode ser criada*/
    if de-tot-saldo = 0 OR tt-ord-prod.qt-ordem > de-tot-saldo then do:
       create tt-relatorio.
       assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
              tt-relatorio.descricao = "O Reciclado n∆o pode ser gerado devido a falta de saldo.".
       undo blk-geral, next blk-geral.
    end.

    /*Diminui a quantidade da ordem se necessario*/
    if tt-ord-prod.qt-ordem > de-tot-saldo then do:
       find current ord-prod exclusive-lock no-error.
       if not avail ord-prod then do:
          create tt-relatorio.
          assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
                 tt-relatorio.descricao = "Erro ao modificar a quantidade da OP.".
          undo blk-geral, next blk-geral.
       end.
       else do:
          assign ord-prod.narrativa          = ord-prod.narrativa + chr(10) + 
                                               "Esta OP teve a quantidade alterada devido a falta de saldo em estoque!" +
                                               " Antes a quantidade era de " + string(ord-prod.qt-ordem)
                 de-qt-anterior              = ord-prod.qt-ordem
                 ord-prod.qt-ordem           = de-tot-saldo

                 tt-ord-prod.qt-ordem        = de-tot-saldo
                 tt-itens-reporte.quantidade = ord-prod.qt-ordem.

          for each oper-ord
             where oper-ord.nr-ord-produ = ord-prod.nr-ord-produ exclusive-lock:
              find first operacao
                   where operacao.it-codigo   = (if oper-ord.cod-roteiro = "" then oper-ord.it-codigo else "")
                     and operacao.cod-roteiro = oper-ord.cod-roteiro
                     and operacao.op-codigo   = oper-ord.op-codigo     no-lock no-error.
              if not avail operacao then next.

              assign oper-ord.tempo-homem       = if   (ord-prod.qt-ordem / de-qt-anterior) * oper-ord.tempo-homem  = ? then 0 
                                                  else (ord-prod.qt-ordem / de-qt-anterior) * oper-ord.tempo-homem
                     oper-ord.tempo-maquin      = if   (ord-prod.qt-ordem / de-qt-anterior) * oper-ord.tempo-maquin = ? then 0
                                                  else (ord-prod.qt-ordem / de-qt-anterior) * oper-ord.tempo-maquin
                     oper-ord.qtd-capac-operac  = if   (ord-prod.qt-ordem / de-qt-anterior) * oper-ord.qtd-capac-operac = ? then 0
                                                  else (ord-prod.qt-ordem / de-qt-anterior) * oper-ord.qtd-capac-operac
                     oper-ord.qtd-previs-operac = if   (ord-prod.qt-ordem / de-qt-anterior) * oper-ord.qtd-previs-operac = ? then 0
                                                  else (ord-prod.qt-ordem / de-qt-anterior) * oper-ord.qtd-previs-operac
                     oper-ord.nr-unidades       =      (ord-prod.qt-ordem).
          end.

          /*Tempos da Split-Operac ajustados conforme a operacao*/
          find first op-sfc 
               where op-sfc.nr-ord-produ = ord-prod.nr-ord-produ exclusive-lock no-error.
          if avail op-sfc then do:
             assign op-sfc.qtd-op = ord-prod.qt-ordem.
             run GerarSplitOperac in h-boin533 (buffer op-sfc).
          end.
          /*Tempos da Split-Operac ajustados conforme a operacao*/

          create tt-relatorio.
          assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
                 tt-relatorio.descricao = "OP " + string(ord-prod.nr-ord-produ) + " teve a quantidade reduzida para " + string(ord-prod.qt-ordem) + ".".
       end.
    end.
    /*FINAL Reservas*/

    find first tt-ord-prod exclusive-lock no-error.
    
    run pi-acompanhar in h-acomp (input "Reportando OP...").
    
    run pi-processa-reporte-sfc.
    
    if return-value = "NOK":U then
       undo blk-geral, next blk-geral.
       
    /*Sucesso na criacao e reporte*/
    create tt-relatorio.
    assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
           tt-relatorio.descricao = "Criada e reportada a OP: " + string(ord-prod.nr-ord-produ).
     
    /*Configura o Lote*/
    assign c-item        = tt-itens-reporte.it-codigo
           c-lote        = tt-itens-reporte.lote
           gc-estado     = "TravaBarra":U
           grw-lote-item = ?.
        
    for last lote-item      no-lock
       where lote-item.it-codigo = c-item
         and lote-item.lote      = c-lote,
       first lote-carac-tec no-lock
       where lote-carac-tec.it-codigo = lote-item.it-codigo
         and lote-carac-tec.lote      = lote-item.lote:
         assign grw-lote-item = rowid(lote-item).
    end.
    
    if grw-lote-item <> ? then do:
       assign w-livre:sensitive = no.
       run pi-finalizar in h-acomp.  
       run sfc\essf0013.w.
       run utp/ut-acomp.p persistent set h-acomp.
       run pi-inicializar in h-acomp (input "Aguarde...").
       run pi-desabilita-cancela in h-acomp.
       assign w-livre:sensitive = yes.

       /*ETIQUETA*/
       run utp/ut-msgs.p (input "show",
                          input 27100,
                          input "Deseja imprimir a Etiqueta?").
       if return-value = "yes" then do:
          for each tt-digita:
              delete tt-digita.
          end.
          create tt-digita.
          assign tt-digita.nr-ord-produ = ord-prod.nr-ord-produ
                 tt-digita.cod-estabel  = ""
                 tt-digita.nr-linha     = 0
                 tt-digita.rw-lote-item = ?
                 tt-digita.arquivo      = "etq" + string(ord-prod.nr-ord-produ) + ".lst".
       
          run pi-etiqueta.
       end.
       /*ETIQUETA*/

    end.
    /*Configura o Lote*/

    delete tt-itens-reporte.

end.

if can-find(first tt-relatorio) then do:
    
    run pi-acompanhar in h-acomp (input "Imprimindo...Aguarde":U).
    
    run sfc/essf0001rp.p (input table tt-relatorio,
                               input table tt-rel-aux,
                               input table tt-lote-a-config).
    
    DEF VAR cEditor     AS CHAR.
    DEF VAR vLog        AS LOGICAL.

    GET-KEY-VALUE SECTION "Datasul_EMS2":U KEY "Show-Report-Program":U VALUE cEditor.
    
    find first usuar_mestre
         where usuar_mestre.cod_usuario = c-seg-usuario no-lock no-error.
    assign c-arq-erro = if avail usuar_mestre then usuar_mestre.nom_dir_spool else "v:\temp":U
           c-arq-erro = c-arq-erro + "\" + c-seg-usuario + "_erro.txt".

    IF  SEARCH(cEditor) = ? THEN DO:
        ASSIGN  cEditor = OS-GETENV("windir") + "\notepad.exe"
                vLog    = YES.
        IF  SEARCH(cEditor) = ? THEN DO:
            ASSIGN  cEditor = OS-GETENV("windir") + "\write.exe".
            IF  SEARCH(cEditor) = ? THEN DO:
                RUN utp/ut-msgs.p (INPUT "show",
                                   INPUT 27576,
                                   INPUT c-arq-erro).
                ASSIGN  vLog    = NO.
            END.
        END.
    END.
    
    RUN WinExec (INPUT cEditor + CHR(32) + c-arq-erro, 1).
    
end.
   

run pi-finalizar in h-acomp.

delete procedure h-boin533.
delete procedure h-boin536.


if can-find(first tt-relatorio
            where tt-relatorio.erro = yes) then do:
   open query br-reporte for each tt-itens-reporte.
   return "NOK":U.
end.

return "OK":U.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-etiqueta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var raw-param as raw no-undo.

for each tt-param:
    delete tt-param.
end.

create tt-param.
assign tt-param.usuario    = c-seg-usuario
       tt-param.destino    = 3
       tt-param.data-exec  = today
       tt-param.hora-exec  = time
       tt-param.dir-etq    = ""
       tt-param.arquivo    = "v:\temp\etiqueta.txt".

raw-transfer tt-param to raw-param.

for each tt-raw-digita:
    delete tt-raw-digita.
end.
for each tt-digita:
    create tt-raw-digita.
    raw-transfer tt-digita to tt-raw-digita.raw-digita.
end.

run sfc/essf0013prp.p (input raw-param,
                            input table tt-raw-digita).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-horas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input parameter p-hora as int no-undo.
/*
  p-hora = 1 then inicio
  p-hora = 2 then fim
*/

def var de-tempo-oper as dec no-undo.
def var c-cod-calend  as char no-undo.
def var c-cod-area    as char no-undo.
def var c-gm-codigo   as char no-undo.

def var dat-inic-reporte as date no-undo.
def var dat-fim-reporte  as date no-undo.

def var c-hra-inic-rep   as char no-undo.
def var c-hra-fim-rep   as char no-undo.

def var h-boin469b as handle no-undo.

run inbo/boin469b.p persistent set h-boin469b.

if input frame f-cad fi-dt-trans-i <> ? and 
   input frame f-cad fi-dt-trans-f <> ? then do:

        if  param-sfc.log-tipo-relogio then do:
            run pi-formatted-time-to-sec (Input input frame f-cad fi-hr-trans-f,
                                          output de-qtd-segs-fim-reporte).
            run pi-formatted-time-to-sec (Input input frame f-cad fi-hr-trans-i,
                                          output de-qtd-segs-inic-reporte).
        end.
    
        else do :
            assign de-qtd-segs-inic-reporte = integer(input frame f-cad fi-hr-trans-i) * 36
                   de-qtd-segs-fim-reporte  = integer(input frame f-cad fi-hr-trans-f)  * 36.
         end.
 
        assign de-tempo-oper    = ?
               dat-inic-reporte = input frame f-cad fi-dt-trans-i
               dat-fim-reporte  = input frame f-cad fi-dt-trans-f
               c-cod-calend     = ?
               c-cod-area       = ?
               c-gm-codigo      = ?.

        run CalcularTemposCtrab in h-boin469b (input-output c-cod-area,
                                               input-output c-gm-codigo,
                                               input        input frame f-cad fi-cod-maquina,
                                               input-output c-cod-calend,
                                               Input-output dat-fim-reporte,
                                               Input-output de-qtd-segs-fim-reporte,
                                               Input-output dat-inic-reporte,
                                               Input-output de-qtd-segs-inic-reporte,
                                               Input-output de-tempo-oper).
               
        assign fi-tempo = de-tempo-oper.

        disp fi-tempo
             with frame f-cad.
end.

if input frame f-cad fi-dt-trans-i <> ? and 
   input frame f-cad fi-dt-trans-f <> ? then do:

   if p-hora = 1 then do:
      if  param-sfc.log-tipo-relogio then
          run pi-formatted-time-to-sec (Input input frame f-cad fi-hr-trans-i,
                                        output de-qtd-segs-inic-reporte).
      else
          assign de-qtd-segs-inic-reporte = int(input frame f-cad fi-hr-trans-i) * 36.

      assign dat-fim-reporte         = ?
             de-qtd-segs-fim-reporte = ?
             de-tempo-oper           = input frame f-cad fi-tempo
             c-cod-calend            = ?
             c-cod-area              = input frame f-cad fi-cod-maquina
             c-gm-codigo             = ?.

      run CalcularTemposCtrab in h-boin469b (input-output c-cod-area,
                                             input-output c-gm-codigo,
                                             input        input frame f-cad fi-cod-maquina,
                                             input-output c-cod-calend,
                                             Input-output dat-fim-reporte,
                                             Input-output de-qtd-segs-fim-reporte,
                                             Input-output dat-inic-reporte,
                                             Input-output de-qtd-segs-inic-reporte,
                                             Input-output de-tempo-oper).
      if  param-sfc.log-tipo-relogio then
          run pi-sec-to-formatted-time (Input de-qtd-segs-fim-reporte,
                                        output c-hra-fim-rep).
      else
          assign c-hra-fim-rep = string(int(de-qtd-segs-fim-reporte / 36), "9999").

      assign fi-hr-trans-f = c-hra-fim-rep
             fi-dt-trans-f = dat-fim-reporte.

      disp fi-hr-trans-f
           fi-dt-trans-f
          with frame f-cad.
   end.

   if p-hora = 2 then do:
      
      if param-sfc.log-tipo-relogio then
         run pi-formatted-time-to-sec (Input input frame f-cad fi-hr-trans-f,
                                       output de-qtd-segs-fim-reporte).
      else
         assign de-qtd-segs-fim-reporte = integer(input frame f-cad fi-hr-trans-f) * 36.

        assign dat-inic-reporte         = ?
               de-qtd-segs-inic-reporte = ?
               de-tempo-oper            = input frame f-cad fi-tempo
               c-cod-calend     = ?
               c-cod-area       = ?
               c-gm-codigo      = ?.

        run CalcularTemposCtrab in h-boin469b (input-output c-cod-area,
                                               input-output c-gm-codigo,
                                               input        input frame f-cad fi-cod-maquina,
                                               input-output c-cod-calend,
                                               Input-output dat-fim-reporte,
                                               Input-output de-qtd-segs-fim-reporte,
                                               Input-output dat-inic-reporte,
                                               Input-output de-qtd-segs-inic-reporte,
                                               Input-output de-tempo-oper).
                                               
        if param-sfc.log-tipo-relogio then
           run pi-sec-to-formatted-time (Input de-qtd-segs-inic-reporte,
                                         output c-hra-inic-rep).
        else
           assign c-hra-inic-rep = string(int(de-qtd-segs-inic-reporte / 36), "9999"). 

        assign fi-hr-trans-i = c-hra-inic-rep
               fi-dt-trans-i = dat-inic-reporte.

        disp fi-hr-trans-i
             fi-dt-trans-i
            with frame f-cad.
   end.


end.

if  param-sfc.log-tipo-relogio then do:
    run pi-formatted-time-to-sec (Input input frame f-cad fi-hr-trans-i,
                                  output v-qtd-segs-inic-aux).
    run pi-formatted-time-to-sec (Input input frame f-cad fi-hr-trans-f,
                                  output v-qtd-segs-fim-aux).
end.
else do:
    assign v-qtd-segs-inic-aux = int(input frame f-cad fi-hr-trans-i) * 36
           v-qtd-segs-fim-aux  = int(input frame f-cad fi-hr-trans-f) * 36.
end.

delete procedure h-boin469b.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-processa-reporte-sfc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR i-nr-reporte AS INT NO-UNDO.
    
    
    assign l-erro-ver = no.

    for each tt-rep-prod:
        delete tt-rep-prod.
    end.
    for each tt-reporte:
        delete tt-reporte.
    end.
    for each tt-apont-mob:
        delete tt-apont-mob.
    end.

    find first ord-prod 
         where ord-prod.nr-ord-produ = tt-ord-prod.nr-ord-produ 
         exclusive-lock no-error.
    if not avail ord-prod then do:
       create tt-relatorio.
       assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
              tt-relatorio.descricao = "Ordem de produá∆o n∆o encontrada!".
       return "NOK":U.
    end.

    run pi-requisita-material.

    if return-value = "NOK":U then
       return "NOK":U.

    assign i-ind-refugo = INT (f-item-uni-estab (input ITEM.it-codigo,
                                                 input ord-prod.cod-estabel,
                                                 "ind-refugo":U)).

    find first oper-ord
         where oper-ord.nr-ord-produ = ord-prod.nr-ord-produ no-lock no-error.
    if not avail oper-ord then do:
       create tt-relatorio.
       assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
              tt-relatorio.descricao = "Operaá∆o da Ordem n∆o encontrada!".
       return "NOK":U.
    end.

    find first split-operac
         where split-operac.nr-ord-produ   = oper-ord.nr-ord-produ
           and split-operac.num-operac-sfc = oper-ord.num-operac-sfc exclusive-lock no-error.
    if not avail split-operac then
       run GerarOrdemSFC in h-boin533 (buffer ord-prod).
       
    find first estab-mfg no-lock
        where estab-mfg.cod-estabel = ord-prod.cod-estabel no-error.       

    create tt-rep-prod.
    assign tt-rep-prod.cod-versao-integracao = 1
           tt-rep-prod.nr-ord-prod       = ord-prod.nr-ord-prod
           tt-rep-prod.it-codigo         = ord-prod.it-codigo
           tt-rep-prod.tipo              = ord-prod.rep-prod
           tt-rep-prod.op-codigo         = if avail oper-ord then oper-ord.op-codigo    else 0
           tt-rep-prod.cod-roteiro       = if avail oper-ord then oper-ord.cod-roteiro  else ""
           tt-rep-prod.it-oper           = if avail oper-ord then oper-ord.it-codigo    else ""
           tt-rep-prod.pto-controle      = if avail oper-ord then oper-ord.pto-controle else 0
           tt-rep-prod.cod-depos-sai     = ? /*Pega o da reserva*/
           tt-rep-prod.cod-local-sai     = ? /*Pega o da reserva*/
           tt-rep-prod.data              = input frame f-cad fi-dt-trans-f
           tt-rep-prod.un                = ord-prod.un
           tt-rep-prod.qt-reporte        = tt-itens-reporte.quantidade
           tt-rep-prod.qt-refugo         = 0
           tt-rep-prod.cod-depos         = tt-itens-reporte.cod-depos
           tt-rep-prod.cod-localiz       = tt-itens-reporte.localizacao
           tt-rep-prod.lote-serie        = tt-itens-reporte.lote
           tt-rep-prod.cod-refer         = ""
           tt-rep-prod.dt-vali-lote      = tt-itens-reporte.val-lote
           tt-rep-prod.ct-refugo         = if avail estab-mfg then estab-mfg.ct-refugo else param-cp.ct-refugo
           tt-rep-prod.sc-refugo         = if avail estab-mfg then estab-mfg.sc-refugo else param-cp.sc-refugo    
           tt-rep-prod.prog-seg          = 'sf0303'
           tt-rep-prod.ct-codigo         = ""
           tt-rep-prod.nro-docto         = STRING(ord-prod.nr-ord-prod)
           tt-rep-prod.carrega-reservas  = yes
           tt-rep-prod.reserva           = yes
           tt-rep-prod.procura-saldos    = no
           tt-rep-prod.requis-automatica = no
           tt-rep-prod.baixa-reservas    = 1
           tt-rep-prod.finaliza-ordem    = yes
           tt-rep-prod.finaliza-oper     = yes.
        
    if  tt-rep-prod.ct-codigo = "" and 
        item.tipo-contr = 3 /* Consignado */ then do:
        
       
        
            /*assign tt-rep-prod.ct-codigo = param-cp.ct-codigo
                   tt-rep-prod.sc-codigo = param-cp.sc-codigo.*/
    end.
    
    /*MOB-GGF*/
    find first split-operac
         where split-operac.nr-ord-produ = tt-rep-prod.nr-ord-produ no-lock no-error.
    if avail split-operac then
       RUN gera-mob-ggf-automatico.
    /*MOB-GGF*/

    run cpp/cpapi001.p (input-output table tt-rep-prod,
                        input        table tt-refugo,
                        input        table tt-res-neg,
                        input        table tt-apont-mob,
                        input-output table tt-erro,
                        input yes).

    if return-value = "NOK":U then do:
       create tt-relatorio.
       assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
              tt-relatorio.descricao = "Erro ao reportar a OP.".
       for each tt-erro:
           create tt-rel-aux.
           assign tt-rel-aux.it-codigo = tt-relatorio.it-codigo
                  tt-rel-aux.i-sequen  = tt-erro.i-sequen
                  tt-rel-aux.cd-erro   = tt-erro.cd-erro
                  tt-rel-aux.descricao = tt-erro.mensagem.
       end.
       return "NOK":U.
    end.
       
        
    /*** Atualizaá∆o das tabelas do Ch∆o de F†brica ***/
    find first ctrab
         where ctrab.cod-ctrab = input frame f-cad fi-cod-maquina no-lock no-error.
    if not avail ctrab then do:
       create tt-relatorio.
       assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
              tt-relatorio.descricao = "Centro de trabalho n∆o encontrado!".
       return "NOK":U.
    end.

    create tt-reporte.
    IF   AVAIL split-operac 
    THEN assign tt-reporte.rw-split-operac  =  rowid(split-operac).
    ASSIGN tt-reporte.cod-ferr-prod         = ""
           tt-reporte.dat-fim-setup         = ?
           tt-reporte.dat-inic-setup        = ?
           tt-reporte.qtd-segs-fim-setup    = 0
           tt-reporte.qtd-segs-inic-setup   = 0
           tt-reporte.dat-fim-reporte       = input frame f-cad fi-dt-trans-f
           tt-reporte.dat-inic-reporte      = input frame f-cad fi-dt-trans-i
           tt-reporte.qtd-operac-refgda     = 0
           tt-reporte.qtd-operac-aprov      = tt-itens-reporte.quantidade
           tt-reporte.qtd-operac-reptda     = tt-itens-reporte.quantidade
           tt-reporte.qtd-operac-retrab     = 0
           
           tt-reporte.qtd-segs-fim-reporte  = v-qtd-segs-fim-aux
           tt-reporte.qtd-segs-inic-reporte = v-qtd-segs-inic-aux

           /*
           tt-reporte.qtd-segs-fim-reporte  = de-qtd-segs-fim-reporte
           tt-reporte.qtd-segs-inic-reporte = de-qtd-segs-inic-reporte
           */

           tt-reporte.cod-equipe            = input frame f-cad fi-cod-operador
           tt-reporte.num-contador-inic     = 0
           tt-reporte.num-contador-fim      = 0
    
        /*** ParÉmetros p/ reporte ***/
           tt-reporte.baixa-reservas        = 1
           tt-reporte.requisicao-automatica = no
           tt-reporte.busca-saldos          = no
           tt-reporte.requisita-configurado = no.

    
    find first tt-rep-prod no-error.
    if not avail tt-rep-prod then do:
       create tt-relatorio.
       assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
              tt-relatorio.descricao = "Reporte n∆o efetuado! SFC".
       return "NOK":U.
    end.
    IF   AVAIL split-operac THEN DO:
         find last rep-oper-ctrab where 
                   rep-oper-ctrab.nr-ord-produ = split-operac.nr-ord-produ NO-LOCK no-error.
                   
         if  avail rep-oper-ctrab then
             assign i-num-seq-rep = rep-oper-ctrab.num-seq-rep + 1.
         else
             assign i-num-seq-rep = 1.
             
         find last ord-rep no-lock
            where ord-rep.nr-ord-produ = split-operac.nr-ord-produ no-error.
    
         create tt-rep-oper-ctrab.
         buffer-copy tt-reporte to tt-rep-oper-ctrab
         assign tt-rep-oper-ctrab.nr-ord-produ   = split-operac.nr-ord-produ
                tt-rep-oper-ctrab.num-seq-rep    = i-num-seq-rep
                tt-rep-oper-ctrab.num-operac-sfc = split-operac.num-operac-sfc
                tt-rep-oper-ctrab.num-split-oper = split-operac.num-split-oper
                tt-rep-oper-ctrab.cod-ctrab      = ctrab.cod-ctrab
                tt-rep-oper-ctrab.nr-reporte     = (if avail ord-rep then ord-rep.nr-reporte else 0).
 
         ASSIGN i-nr-reporte = tt-rep-oper-ctrab.nr-reporte.

         run GerarRepOperCtrab in h-boin536 (input table tt-rep-oper-ctrab,
                                             input table tt-rep-refugo-oper,
                                             input table tt-rep-ic-oper,
                                             input table tt-rep-ic-oper-tab).
    END.
    if (oper-ord.val-perc-avanco   >= param-cp.var-rep or   
        param-cp.var-rep           >= 999) then do:
        
        if can-find(first b-oper-ord
                    where b-oper-ord.nr-ord-produ     = oper-ord.nr-ord-produ
                      and not b-oper-ord.log-operac-final 
                      and b-oper-ord.val-perc-avanco  < oper-ord.val-perc-avanco) then do:
            run utp/ut-msgs.p (input "msg":U,
                               input (if  param-sfc.log-consid-rede-pert  
                                      then 17988
                                      else 17987),
                               input "").
            create tt-relatorio.
            assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
                   tt-relatorio.descricao = return-value + " SFC".
            return "NOK":U.
        end.

        
        find first lin-prod where 
                   lin-prod.cod-estabel = ord-prod.cod-estabel and 
                   lin-prod.nr-linha    = ord-prod.nr-linha no-lock no-error.
        
        
        if avail lin-prod   AND 
           param-cp.ver-req and 
           lin-prod.sum-requis = 2 /*OP Servico*/ THEN DO:
           
           run cpp/cp0311g.p (input rowid(ord-prod)      ,
                              output l-erro-ver          ,
                              input "2"                  ,
                              input ord-prod.qt-reportada,
                              input yes                  ,
                              input tt-reporte.baixa-reservas).
                              
                              
        END.    
        if not l-erro-ver then do:
           find b-ord-prod where 
                rowid(b-ord-prod) = rowid(ord-prod) EXCLUSIVE-LOCK no-error.
           if not valid-handle (h-cpapi301) then
              run cpp/cpapi301.p persistent set h-cpapi301 (input-output table tt-ord-prod,
                                                            input-output table tt-reapro,
                                                            input-output table tt-erro,
                                                            input yes).
            
           run pi-finalizar-ordem-producao in h-cpapi301 (buffer b-ord-prod,
                                                          input yes). /* Altera estado da ordem */

           /* Integracao com o Chao-de-Fabrica (EMS) */
           if  param-global.modulo-ch     and 
               ord-prod.rep-prod     NE 4 then do:
               
               if  valid-handle (h-boin533)  AND 
                   h-boin533:TYPE = "PROCEDURE":U  AND 
                   h-boin533:FILE-NAME = "inbo/boin533.p":U then DO:
                   run alterarOrdemSFC in h-boin533 (buffer ord-prod).
               END.
               
           end.
        end.
    end.
    
    IF  AVAIL tt-rep-oper-ctrab THEN DO:
    
        FOR FIRST oper-ord
            WHERE oper-ord.nr-ord-produ = tt-rep-oper-ctrab.nr-ord-produ EXCLUSIVE-LOCK:
            RUN FecharOperacao IN h-boin536 (BUFFER oper-ord).
        END.
        
        /*Alteracao Projeto Revis∆o Custos - Moises Pereira 31/03/2015
        A rotina de estorno da produá∆o tinha um especifico para permitir o estorno das movimentaá‰es de REQ,
        devido a alteraá∆o do tipo de reporte de ordem para operaá∆o e ponto de controle estamos utilizando o 
        sf0315 e nesta rotina n∆o fizemos o especifico para estornar estas requisiá‰es nesta interface, desta forma
        estamos alterando os movimentos de estoque gravando o numero de reporte (nr-reporte) para que a rotina de
        estorno padr∆o faáa o trabalho por completo estornando ACA/ROP e REQ sem a necessidade de mais 1 especifico
        */
        FOR EACH movto-estoq EXCLUSIVE-LOCK USE-INDEX operacao
           WHERE movto-estoq.nr-ord-produ = tt-rep-oper-ctrab.nr-ord-produ
             AND movto-estoq.nr-reporte   = 0
             AND movto-estoq.esp-docto    = 28:
             
           FIND FIRST movto-mat EXCLUSIVE-LOCK USE-INDEX num-seq
               WHERE movto-mat.nr-ord-produ = movto-estoq.nr-ord-produ
                 AND movto-mat.num-sequen   = movto-estoq.num-sequen    NO-ERROR.
        
           ASSIGN movto-estoq.nr-reporte = i-nr-reporte.
        
           IF AVAIL movto-mat THEN
               ASSIGN movto-mat.nr-reporte = i-nr-reporte.
        END.
        /*********************************
        FIND ord-prod EXCLUSIVE-LOCK 
            WHERE ord-prod.nr-ord-produ = tt-rep-oper-ctrab.nr-ord-produ NO-ERROR.
        IF AVAIL ord-prod THEN DO:
            ASSIGN ord-prod.tipo = 1. /*Alteracao para tipo interna para evitar looping de ordens*/
            RELEASE ord-prod.
        END.        
        FIND ord-prod NO-LOCK 
            WHERE ord-prod.nr-ord-produ = tt-rep-oper-ctrab.nr-ord-produ NO-ERROR.
        *************************************/    
    END.
   
    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-requisita-material :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def var h-cpapi012 as handle no-undo.
def var c-return   as char   no-undo.

for each tt-requis:
    delete tt-requis.
end.

create tt-requis.
assign tt-requis.tipo-trans            = 1
       tt-requis.nr-ord-produ          = ord-prod.nr-ord-produ
       tt-requis.quantidade            = ord-prod.qt-ordem
       tt-requis.data                  = input frame f-cad fi-dt-trans-f
       tt-requis.item-ini              = ""
       tt-requis.item-fim              = "ZZZZZZZZZZZZZZZZ"
       tt-requis.procura-saldos        = no 
       tt-requis.carrega-reservas      = no
       tt-requis.op-codigo-ini         = 0
       tt-requis.op-codigo-fim         = 999999
       tt-requis.prog-seg              = "essfc001"
       tt-requis.cod-versao-integracao = 1
       tt-requis.nro-docto             = string(ord-prod.nr-ord-produ)
       tt-requis.deposito-ini          = ""
       tt-requis.deposito-fim          = "ZZZ".
    
FOR EACH tt-rw-tt-saldo-estoq.
    FIND b-tt-saldo-estoq WHERE ROWID(b-tt-saldo-estoq) = tt-rw-tt-saldo-estoq.rw-tt-saldo-estoq NO-LOCK.
    IF AVAIL b-tt-saldo-estoq THEN DO:
         
        IF b-tt-saldo-estoq.dt-vali-lote <= TODAY THEN DO:
            OUTPUT TO value("m:\dts\log_prd\lotesvalidados.txt") APPEND.
            
            FOR EACH bb-tt-saldo-estoq WHERE
                 bb-tt-saldo-estoq.it-codigo   = b-tt-saldo-estoq.it-codigo AND
                /* bb-tt-saldo-estoq.cod-estabel = b-tt-saldo-estoq.cod-estabel AND Edson - para validar lote de todos estabelecimentos*/
                 bb-tt-saldo-estoq.lote        = b-tt-saldo-estoq.lote EXCLUSIVE-LOCK USE-INDEX lote.
              DISP tt-requis.nr-ord-produ 
                    bb-tt-saldo-estoq.it-codigo
                    bb-tt-saldo-estoq.cod-depos 
                    bb-tt-saldo-estoq.cod-localiz 
                    bb-tt-saldo-estoq.cod-refer
                    bb-tt-saldo-estoq.lote  
                    bb-tt-saldo-estoq.dt-vali-lote  
                    bb-tt-saldo-estoq.qtidade-atu. 
              ASSIGN bb-tt-saldo-estoq.dt-vali-lote = 12/31/2999.
            END.

            OUTPUT CLOSE.
            /*RELEASE b-tt-saldo-estoq NO-ERROR.*/
        END.
 /*       FIND b-item WHERE b-item.it-codigo = b-tt-saldo-estoq.it-codigo NO-LOCK NO-ERROR.
        FIND b-item-uni-estab WHERE b-item-uni-estab.it-codigo = b-tt-saldo-estoq.it-codigo AND
             b-item-uni-estab.cod-estabel = b-tt-saldo-estoq.cod-estabel NO-LOCK NO-ERROR.

      MESSAGE tt-requis.nr-ord-produ SKIP
             b-tt-saldo-estoq.it-codigo      skip
             b-tt-saldo-estoq.lote          SKIP
             b-tt-saldo-estoq.dt-vali-lote  skip
             b-tt-saldo-estoq.qtidade-atu   SKIP
            "item:" b-item.loc-unica     skip
            "item-estab:" b-item-uni-estab.loc-unica
       VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

    END.


    
END.

run cpp/cpapi012.p persistent set h-cpapi012  (input        table tt-requis,
                                               input-output table tt-erro,
                                               input        yes).     

run pi-recebe-tt-reservas in h-cpapi012 (input        table tt-reservas).
run pi-processa-requis    in h-cpapi012 (input        table tt-requis,
                                         input-output table tt-erro,
                                         input        yes).
assign c-return = return-value.
run pi-finalizar in h-cpapi012.

if c-return = "NOK":U then do:
   create tt-relatorio.
   assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
          tt-relatorio.descricao = "Erro ao requisitar os materiais!".
   for each tt-erro:
       create tt-rel-aux.
       assign tt-rel-aux.it-codigo = tt-relatorio.it-codigo
              tt-rel-aux.i-sequen  = tt-erro.i-sequen
              tt-rel-aux.cd-erro   = tt-erro.cd-erro
              tt-rel-aux.descricao = tt-erro.mensagem.
       create tt-rel-aux.
       assign tt-rel-aux.it-codigo = tt-relatorio.it-codigo
              tt-rel-aux.i-sequen  = tt-erro.i-sequen
              tt-rel-aux.cd-erro   = tt-erro.cd-erro
              tt-rel-aux.descricao = "Verifique as parametrizaá‰es do Item: Localizaá∆o Unica":U.
   end.
   return "NOK":U.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-valida-reportes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

find first estabelec
     where estabelec.cod-estabel = input frame f-cad fi-cod-estabel 
     no-lock no-error.
if not avail estabelec then do:
   run utp/ut-msgs.p (input "show",
                      input 17006,
                      input "Estabelecimento n∆o encontrado!").
   return "NOK":U.
end.

if input frame f-cad fi-dt-trans-i = ? then do:
   run utp/ut-msgs.p (input "show",
                      input 17006,
                      input "Data inicial inv†lida!").
   return "NOK":U.
end.

if input frame f-cad fi-dt-trans-f = ? then do:
   run utp/ut-msgs.p (input "show",
                      input 17006,
                      input "Data final inv†lida!").
   return "NOK":U.
end.

FIND FIRST pol-param-estab
    WHERE pol-param-estab.cod-estabel =input frame f-cad fi-cod-estabel  NO-LOCK NO-ERROR.
if not avail pol-param-estab then do:
    run utp/ut-msgs.p (input "show",
                       input 2,
                       input "ParÉmetros do Estabelecimento").
     return "NOK":U.
end.
ELSE if input frame f-cad fi-dt-trans-f > pol-param-estab.data-reciclado then do:
    RUN utp/ut-msgs.p (INPUT "show",
                       INPUT 25997,
                       INPUT "Transaá∆o n∆o permitida!" + "~~" +
                             "Verifique a Data do Reciclado informada nos ParÉmetros do Estabelecimento - essf0008.").
    return "NOK":U.
end.

if input frame f-cad fi-dt-trans-f < 
   input frame f-cad fi-dt-trans-i then do:
   run utp/ut-msgs.p (input "show",
                      input 17006,
                      input "Data final menor que Data inicial!").
   return "NOK":U.
end.

if input frame f-cad fi-hr-trans-i = 
   input frame f-cad fi-hr-trans-f then do:
   run utp/ut-msgs.p (input "show",
                      input 17006,
                      input "Hora final deve ser maior que Hora inicial!").
   return "NOK":U.
end.

if (param-sfc.log-tipo-relogio 
    and substr(input frame f-cad fi-hr-trans-i,3,2) > '59')
    or  substr(input frame f-cad fi-hr-trans-i,1,2) > '23' then do:
        run utp/ut-msgs.p (input "show":U, 
                           input 3046, 
                           input "Inicio").
        apply 'entry' to fi-hr-trans-i in frame f-cad.
        return "NOK":U.
end.

if input frame f-cad fi-hr-trans-i = ? or
   length(input frame f-cad fi-hr-trans-i) < 4  then do:
   run utp/ut-msgs.p (input "show",
                      input 17006,
                      input "Hora inicial inv†lida!").
   return "NOK":U.
end.

if (param-sfc.log-tipo-relogio 
    and substr(input frame f-cad fi-hr-trans-f,3,2) > '59')
    or  substr(input frame f-cad fi-hr-trans-f,1,2) > '23' then do:
        run utp/ut-msgs.p (input "show":U, 
                           input 3046, 
                           input "Final").
        apply 'entry' to fi-hr-trans-f in frame f-cad.
        return "NOK":U.
end.

if input frame f-cad fi-hr-trans-f = ? or
   length(input frame f-cad fi-hr-trans-f) < 4  then do:
   run utp/ut-msgs.p (input "show":U, 
                      input 17006,
                      input "Hora final inv†lida!").
   return "NOK":U.
end.

find first operador
     where operador.cod-operador = input frame f-cad fi-cod-operador 
     no-lock no-error.
if not avail operador then do:
   run utp/ut-msgs.p (input "show",
                      input 17006,
                      input "Operador n∆o encontrado!").
   return "NOK":U.
end.


find first model-turno-semana
     where model-turno-semana.cod-model-turno = input frame f-cad fi-turno
     no-lock no-error.
if not avail model-turno-semana then do:
   run utp/ut-msgs.p (input "show",
                      input 17006,
                      input "Turno n∆o encontrado!").
   return "NOK":U.
end.

find first ctrab
     where ctrab.cod-ctrab = input frame f-cad fi-cod-maquina
     no-lock no-error.
if not avail ctrab then do:
   run utp/ut-msgs.p (input "show",
                      input 17006,
                      input "Centro de trabalho n∆o encontrado!").
   return "NOK":U.
end.


for each tt-itens-reporte:

    if tt-itens-reporte.it-codigo = "" then do:
       run utp/ut-msgs.p (input "show",
                          input 17006,
                          input "Item deve ser diferente de branco!").
       return "NOK":U.
    end.

    find first item
         where item.it-codigo = tt-itens-reporte.it-codigo no-lock no-error.
    if not avail item then do:
       run utp/ut-msgs.p (input "show",
                          input 17006,
                          input "Item " + tt-itens-reporte.it-codigo + " n∆o encontrado!").
       return "NOK":U.
    end.

    if tt-itens-reporte.quantidade < 0 then do:
       run utp/ut-msgs.p (input "show",
                          input 17006,
                          input "Quantidade do item " + tt-itens-reporte.it-codigo + " deve ser superior a zero!").
       return "NOK":U.
    end.

    find first deposito
         where deposito.cod-depos = tt-itens-reporte.cod-depos
         no-lock no-error.
    if not avail deposito then do:
       run utp/ut-msgs.p (input "show",
                          input 17006,
                          input "Dep¢sito n∆o encontrado para o item: " + tt-itens-reporte.it-codigo).
       return "NOK":U.
    end.

    find first mgemp.localizacao
         where localizacao.cod-localiz = tt-itens-reporte.localizacao
           and localizacao.cod-depos   = tt-itens-reporte.cod-depos   
           and localizacao.cod-estabel = input frame f-cad fi-cod-estabel
           no-lock no-error.
    if not avail localizacao then do:
       run utp/ut-msgs.p (input "show",
                          input 17006,
                          input "Localizaá∆o n∆o encontrado para o item: " + tt-itens-reporte.it-codigo).
       return "NOK":U.
    end.

    if tt-itens-reporte.lote = "" then do:
       run utp/ut-msgs.p (input "show",
                          input 17006,
                          input "Lote deve ser diferente de branco para o item: " + tt-itens-reporte.it-codigo).
       return "NOK":U.
    end.

    if tt-itens-reporte.val-lote = ? then do:
       run utp/ut-msgs.p (input "show",
                          input 17006,
                          input "Data inv†lida para o item: " + tt-itens-reporte.it-codigo).
       return "NOK":U.
    end.


end.
 
 
return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE RTB_xref_generator :
/* -----------------------------------------------------------
Purpose:    Generate RTB xrefs for SMARTOBJECTS.
Parameters: <none>
Notes:      This code is generated by the UIB.  DO NOT modify it.
            It is included for Roundtable Xref generation. Without
            it, Xrefs for SMARTOBJECTS could not be maintained by
            RTB.  It will in no way affect the operation of this
            program as it never gets executed.
-------------------------------------------------------------*/
  RUN "panel\p-exihel.w *RTB-SmObj* ".

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
    WHEN "tt-itens-reporte":U THEN p-rowid-list = p-rowid-list + 
        IF AVAILABLE tt-itens-reporte THEN STRING(ROWID(tt-itens-reporte))
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


PROCEDURE piAjustaItemRefugo:

    DEF BUFFER bf-item-uni-estab  FOR item-uni-estab.
    DEF BUFFER bf2-item-uni-estab FOR item-uni-estab.
    DEF BUFFER bf-item            FOR ITEM.
    
    /************************************
    OS-DELETE VALUE("\\client\d$\Temp\antes.txt").
    OUTPUT TO VALUE("\\client\d$\Temp\antes.txt") NO-CONVERT.
    FOR EACH tt-saldo-estoq:
        DISP tt-saldo-estoq.cod-estabel
             tt-saldo-estoq.cod-depos
             tt-saldo-estoq.cod-localiz
             tt-saldo-estoq.it-codigo
             tt-saldo-estoq.cod-refer
             tt-saldo-estoq.lote
             tt-saldo-estoq.qtidade-atu
            WITH WIDTH 300 STREAM-IO.
    END.
    OUTPUT CLOSE.
    ************************************/
    b-conversao:
    FOR EACH tt-saldo-estoq 
        WHERE tt-saldo-estoq.log-ajuste = YES:

        FIND FIRST bf-item-uni-estab NO-LOCK
            WHERE bf-item-uni-estab.it-codigo   = tt-saldo-estoq.it-codigo 
              AND bf-item-uni-estab.cod-estabel = tt-saldo-estoq.cod-estabel NO-ERROR.
        IF NOT AVAIL bf-item-uni-estab THEN NEXT.

        IF bf-item-uni-estab.log-control-estoq-refugo THEN DO:

            FIND FIRST estab-mfg NO-LOCK
                WHERE estab-mfg.cod-estabel = tt-saldo-estoq.cod-estabel NO-ERROR.

            FIND FIRST bf2-item-uni-estab NO-LOCK
                WHERE bf2-item-uni-estab.it-codigo   = bf-item-uni-estab.cod-item-refugo 
                  AND bf2-item-uni-estab.cod-estabel = bf-item-uni-estab.cod-estabel     NO-ERROR.
            IF NOT AVAIL bf2-item-uni-estab THEN DO:
                RUN pi-cria-erro(17006,"O item " + bf-item-uni-estab.cod-item-refugo + "/" + bf-item-uni-estab.cod-estabel + 
                                       " n∆o foi encontrando no item x estabelecimento CD1112").
                RETURN "NOK".
            END.

            IF bf2-item-uni-estab.preco-fiscal = 0 THEN DO:
                RUN pi-cria-erro(17006,"O item " + bf-item-uni-estab.cod-item-refugo + "/" + bf-item-uni-estab.cod-estabel + 
                                       " est† com o preáo fiscal zerado").
                RETURN "NOK".
            END.

            EMPTY TEMP-TABLE tt-movto.
            EMPTY TEMP-TABLE tt-erro.

            FIND FIRST bf-item NO-LOCK
                WHERE bf-item.it-codigo = tt-saldo-estoq.it-codigo NO-ERROR.

            create tt-movto.
            assign tt-movto.cod-versao-integracao = 001
                   tt-movto.cod-prog-orig         = "essf0001"
                   tt-movto.op-seq                = 0
                   tt-movto.it-codigo             = tt-saldo-estoq.it-codigo
                   tt-movto.cod-refer             = tt-saldo-estoq.cod-refer
                   tt-movto.lote                  = tt-saldo-estoq.lote
                   tt-movto.cod-estabel           = tt-saldo-estoq.cod-estabel
                   tt-movto.cod-depos             = tt-saldo-estoq.cod-depos
                   tt-movto.cod-localiz           = tt-saldo-estoq.cod-localiz
                   tt-movto.dt-trans              = input frame f-cad fi-dt-trans-f
                   tt-movto.tipo-trans            = 2 /*Saida*/
                   tt-movto.dt-vali-lote          = tt-saldo-estoq.dt-vali-lote
                   tt-movto.valor-mat-m[1]        = 0
                   tt-movto.valor-mob-m[1]        = 0
                   tt-movto.valor-ggf-m[1]        = 0
                   tt-movto.conta-contabil        = IF AVAIL estab-mfg THEN estab-mfg.conta-refugo ELSE ""
                   tt-movto.esp-docto             = 28 /*REQ*/ 
                   tt-movto.quantidade            = tt-saldo-estoq.qtidade-atu
                   tt-movto.un                    = bf-item.un
                   tt-movto.serie-docto           = "CS"
                   tt-movto.nro-docto             = STRING(TODAY,"99/99/9999")
                   tt-movto.tipo-valor            = 2 /* Valorizado */
                   tt-movto.usuario               = c-seg-usuario.

            FIND FIRST bf-item NO-LOCK
                WHERE bf-item.it-codigo = bf2-item-uni-estab.it-codigo NO-ERROR.
            
            create tt-movto.
            assign tt-movto.cod-versao-integracao = 001
                   tt-movto.cod-prog-orig         = "essf0001"
                   tt-movto.op-seq                = 0
                   tt-movto.it-codigo             = bf2-item-uni-estab.it-codigo
                   tt-movto.cod-refer             = ""
                   tt-movto.lote                  = ""
                   tt-movto.cod-estabel           = tt-saldo-estoq.cod-estabel
                   tt-movto.cod-depos             = tt-saldo-estoq.cod-depos
                   tt-movto.cod-localiz           = tt-saldo-estoq.cod-localiz
                   tt-movto.dt-trans              = input frame f-cad fi-dt-trans-f
                   tt-movto.tipo-trans            = 1 /*Entrada*/
                   tt-movto.dt-vali-lote          = ?
                   tt-movto.valor-mat-m[1]        = bf2-item-uni-estab.preco-fiscal
                   tt-movto.valor-mob-m[1]        = 0
                   tt-movto.valor-ggf-m[1]        = 0
                   tt-movto.conta-contabil        = IF AVAIL estab-mfg THEN estab-mfg.conta-refugo ELSE ""
                   tt-movto.esp-docto             = 5 /*DEV*/
                   tt-movto.quantidade            = tt-saldo-estoq.qtidade-atu * (IF bf-item-uni-estab.val-relac-refugo-item = 0 THEN 1 ELSE bf-item-uni-estab.val-relac-refugo-item)
                   tt-movto.un                    = bf-item.un
                   tt-movto.serie-docto           = "CS"
                   tt-movto.nro-docto             = STRING(TODAY,"99/99/9999")
                   tt-movto.tipo-valor            = 1 /* Informado */
                   tt-movto.usuario               = c-seg-usuario.
            
            run cep/ceapi001.p (input-output table tt-movto,
                                input-output table tt-erro,
                                input yes).

            IF CAN-FIND(FIRST tt-erro) THEN RETURN "NOK".

            FIND FIRST b-tt-saldo-estoq NO-LOCK
                WHERE b-tt-saldo-estoq.cod-estabel = tt-saldo-estoq.cod-estabel
                  AND b-tt-saldo-estoq.it-codigo   = bf2-item-uni-estab.it-codigo
                  AND b-tt-saldo-estoq.cod-refer   = ""
                  AND b-tt-saldo-estoq.cod-depos   = tt-saldo-estoq.cod-depos
                  AND b-tt-saldo-estoq.cod-localiz = tt-saldo-estoq.cod-localiz
                  AND b-tt-saldo-estoq.lote        = ""                            NO-ERROR.
            IF NOT AVAIL b-tt-saldo-estoq THEN DO:
                ASSIGN tt-saldo-estoq.it-codigo   = bf2-item-uni-estab.it-codigo
                       tt-saldo-estoq.cod-refer   = ""                        
                       tt-saldo-estoq.lote        = ""
                       tt-saldo-estoq.log-ajuste  = NO
                       tt-saldo-estoq.qtidade-atu = tt-saldo-estoq.qtidade-atu * (IF bf-item-uni-estab.val-relac-refugo-item = 0 THEN 1 ELSE bf-item-uni-estab.val-relac-refugo-item).
            END.
            ELSE DO:
                ASSIGN b-tt-saldo-estoq.qtidade-atu = b-tt-saldo-estoq.qtidade-atu + (tt-saldo-estoq.qtidade-atu * (IF bf-item-uni-estab.val-relac-refugo-item = 0 THEN 1 ELSE bf-item-uni-estab.val-relac-refugo-item))
                       b-tt-saldo-estoq.log-ajuste  = NO.

                DELETE tt-saldo-estoq.
            END.
        END.
    END.

    /************************************
    OS-DELETE VALUE("\\client\d$\Temp\depois.txt").
    OUTPUT TO VALUE("\\client\d$\Temp\depois.txt") NO-CONVERT.
    FOR EACH tt-saldo-estoq:
        DISP tt-saldo-estoq.cod-estabel
             tt-saldo-estoq.cod-depos
             tt-saldo-estoq.cod-localiz
             tt-saldo-estoq.it-codigo
             tt-saldo-estoq.cod-refer
             tt-saldo-estoq.lote
             tt-saldo-estoq.qtidade-atu
            WITH WIDTH 300 STREAM-IO.
    END.
    OUTPUT CLOSE.
    ************************************/

    RETURN "OK".
END.

PROCEDURE piSaldoItemNaoRefugo:
    DEF BUFFER bf-item-uni-estab  FOR item-uni-estab.
    DEF BUFFER bf2-item-uni-estab FOR item-uni-estab.
    DEF BUFFER bf-item            FOR ITEM.
    
    /************************************
    OS-DELETE VALUE("\\client\d$\Temp\antes.txt").
    OUTPUT TO VALUE("\\client\d$\Temp\antes.txt") NO-CONVERT.
    FOR EACH tt-saldo-estoq:
        DISP tt-saldo-estoq.cod-estabel
             tt-saldo-estoq.cod-depos
             tt-saldo-estoq.cod-localiz
             tt-saldo-estoq.it-codigo
             tt-saldo-estoq.cod-refer
             tt-saldo-estoq.lote
             tt-saldo-estoq.qtidade-atu
            WITH WIDTH 300 STREAM-IO.
    END.
    OUTPUT CLOSE.
    ************************************/
    b-conversao:
    FOR EACH tt-saldo-estoq 
        WHERE tt-saldo-estoq.log-ajuste = YES:

        FIND FIRST bf-item-uni-estab NO-LOCK
            WHERE bf-item-uni-estab.it-codigo   = tt-saldo-estoq.it-codigo 
              AND bf-item-uni-estab.cod-estabel = tt-saldo-estoq.cod-estabel NO-ERROR.
        IF NOT AVAIL bf-item-uni-estab THEN NEXT.

        IF bf-item-uni-estab.log-control-estoq-refugo THEN DO:
            DELETE tt-saldo-estoq.
        END.
    END.

    /************************************
    OS-DELETE VALUE("\\client\d$\Temp\depois.txt").
    OUTPUT TO VALUE("\\client\d$\Temp\depois.txt") NO-CONVERT.
    FOR EACH tt-saldo-estoq:
        DISP tt-saldo-estoq.cod-estabel
             tt-saldo-estoq.cod-depos
             tt-saldo-estoq.cod-localiz
             tt-saldo-estoq.it-codigo
             tt-saldo-estoq.cod-refer
             tt-saldo-estoq.lote
             tt-saldo-estoq.qtidade-atu
            WITH WIDTH 300 STREAM-IO.
    END.
    OUTPUT CLOSE.
    ************************************/

    RETURN "OK".
    
END.
