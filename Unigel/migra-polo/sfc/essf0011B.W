

/* Connected Databases 
          mgadm            PROGRESS
*/

/*:T*******************************************************************************
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
define buffer if-ped-venda for espmulti.if-ped-venda.
def buffer empresa for mgmulti.empresa.
def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[2.00.00.002[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.002"
       c-prg-obj = "essf0011B".
if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "essf0011B"
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
    put "essf0011B" at 1 "2.00.00.002" at 69 today at 84 string(time,'HH:MM:SS':U) at 94 skip.
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

 

/**** Alteraá∆o efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
/*************************************************
* i_dbvers.i - Include de vers∆o de banco de dados   
**************************************************/

/* Preprocessadores que identificam os bancos do Produto EMS 5 */

/* Preprocessadores que identificam os bancos do Produto EMS 2 */
/*RAC Incorporado na 2.04*/

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

/* Parameters Definitions --- */
/*DEFINE var  pr-campanha AS ROWID NO-UNDO.                                        
 FIND LAST campanha-polo  NO-LOCK NO-ERROR.
       pr-campanha = ROWID(campanha-polo) .
       */
DEFINE INPUT PARAMETER pr-campanha AS ROWID NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-linha  AS CHAR  NO-UNDO.
def var c-folha-esp as char no-undo.
/* Local Variable Definitions ---                                       */
DEF BUFFER b-campanha FOR campanha-polo.
DEF VAR de-saldo LIKE saldo-estoq.qtidade-atu.

DEF VAR c-data         AS CHAR NO-UNDO.
DEF VAR da-data        AS DATE NO-UNDO.

/* alteracao para pegar filtro cliente fim quando unigel comercial Edson 16-11-2012*/
DEFINE TEMP-TABLE tt-peduc NO-UNDO
    FIELD nome-abrev LIKE ped-venda.nome-abrev
    FIELD nr-pedido LIKE ped-venda.nr-pedido
    FIELD cod-estabel LIKE ped-venda.cod-estabel
    INDEX ped IS PRIMARY UNIQUE nr-pedido.

DEF TEMP-TABLE tt-pedido
  FIELD  gm-codigo     LIKE grup-maquina.gm-codigo
  FIELD  cd-produto    LIKE produto.cd-produto
  FIELD  it-codigo     LIKE campanha-polo.it-codigo
  FIELD  nr-pedcli     LIKE ped-venda.nr-pedcli
  FIELD  cod-emitente  LIKE emitente.cod-emitente
  FIELD  nome-abrev    LIKE emitente.nome-abrev
  FIELD  qt-pedida     AS DEC
  FIELD  dt-entrega    AS DATE FORMAT 99/99/9999
  FIELD  nr-sequencia  LIKE ped-item.nr-sequencia
  FIELD  ano-campanha  LIKE campanha-polo.ano-campanha
  FIELD  mes-campanha  LIKE campanha-polo.mes-campanha
  FIELD  sequencia     LIKE campanha-polo.sequencia
  FIELD  log-tipo      AS INT
  FIELD  cod-refer     LIKE res-atp.cod-refer
  FIELD  nr-config     LIKE ped-item.nr-config
  FIELD  nr-bobinas    AS INT
  FIELD  dt-campanhas  AS CHAR
  FIELD  qt-campanhas  AS CHAR
  FIELD  tot-campanhas AS DEC
  FIELD  op-campanhas  AS CHAR
  FIELD  qt-orig-ped   AS DEC
  FIELD  saldo-var     AS DEC /* Saldo da variaá∆o do pedido ap¢s arredondamento do n£mero de bobinas */
  FIELD  l-possui-prog AS LOG INITIAL NO
  FIELD  nome-abrev-fim    LIKE emitente.nome-abrev.
DEF TEMP-TABLE tt-ped-campanha
  FIELD  gm-codigo    LIKE grup-maquina.gm-codigo
  FIELD  cd-produto   LIKE produto.cd-produto
  FIELD  nr-pedcli    LIKE ped-venda.nr-pedcli
  FIELD  it-codigo    LIKE campanha-polo.it-codigo
  FIELD  nome-abrev   LIKE emitente.nome-abrev
  FIELD  qt-pedida    AS DEC
  FIELD  dt-entrega   AS DATE FORMAT 99/99/9999
  FIELD  nr-sequencia LIKE ped-item.nr-sequencia
  FIELD  ano-campanha LIKE campanha-polo.ano-campanha
  FIELD  mes-campanha LIKE campanha-polo.mes-campanha
  FIELD  sequencia    LIKE campanha-polo.sequencia
  FIELD  i-perc-var   LIKE emitente.perc-fat-ped
  FIELD  log-tipo     AS INT
  FIELD  nome-abrev-fim    LIKE emitente.nome-abrev.

DEF VAR r-campanha   AS ROWID NO-UNDO.
DEF VAR r-rowid-camp AS ROWID NO-UNDO.

DEF VAR p-cod-estabel   LIKE estabelec.cod-estabel  INITIAL '{cdp\poloestab.i 422}'. /*solic-318*/ 
DEF VAR p-nr-pedido-ini LIKE ped-venda.nr-pedido    INITIAL 0.   
DEF VAR p-nr-pedido-fim LIKE ped-venda.nr-pedido    INITIAL 999999999.   
DEF VAR p-nome-ini      LIKE ped-venda.nome-abrev   INITIAL "".  
DEF VAR p-nome-fim      LIKE ped-venda.nome-abrev   INITIAL "ZZZZZZZZZZZZ".  
DEF VAR p-tp-ped-ini    LIKE ped-venda.tp-ped       INITIAL "".
DEF VAR p-tp-ped-fim    LIKE ped-venda.tp-ped       INITIAL "ZZ".
DEF VAR p-data-ini      AS DATE FORMAT "99/99/9999".
DEF VAR p-data-fim      AS DATE FORMAT "99/99/9999".
DEF VAR p-mercado       AS INTEGER                  INITIAL 1.
DEF VAR p-acao          AS INTEGER NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE gl-essf0011b-initialize AS LOGICAL                  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE v-gm-codigo-essf0011v01 LIKE grup-maquina.gm-codigo NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gr-item-cli             AS ROWID                    NO-UNDO.

DEF VAR i-producao-hora       LIKE ext-rec-prod.qt-produto     NO-UNDO.
DEF VAR d-maq-largura-min     LIKE ctrab-carac-tec.vl-result   NO-UNDO.
DEF VAR d-maq-largura-max     LIKE ctrab-carac-tec.vl-result   NO-UNDO.
DEF VAR d-item-largura        LIKE ped-campanha.largura        NO-UNDO.
DEF VAR d-item-diam-int       LIKE ped-campanha.diam-int       NO-UNDO.
DEF VAR d-item-diam-ext       LIKE ped-campanha.diam-ext       NO-UNDO.
DEF VAR d-item-peso           LIKE ped-campanha.peso           NO-UNDO.
DEF VAR i-item-nr-bobinas     LIKE ped-campanha.nr-bobinas     NO-UNDO.
DEF VAR i-item-nr-min-bobinas LIKE ped-campanha.nr-min-bobinas NO-UNDO.
DEF VAR i-item-nr-max-bobinas LIKE ped-campanha.nr-max-bobinas NO-UNDO.
DEF VAR de-correcao-refugo    AS DECIMAL                       NO-UNDO.
DEF VAR l-cria-ped-campanha   AS LOG INIT YES                  NO-UNDO.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* Name of first Frame and/or Browse and/or first Query                 */

/* Internal Tables (found by Frame, Query & Browse Queries)             */

/* Definitions for BROWSE br-ped-campanha                               */


/* Definitions for BROWSE br-pedido                                     */


/* Definitions for FRAME f-cad                                          */

/* Standard List Definitions                                            */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */




/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-cadcom AS WIDGET-HANDLE NO-UNDO.

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
       MENU-ITEM mi-desfazer    LABEL "&Desfazer"      ACCELERATOR "CTRL-U"
       MENU-ITEM mi-cancelar    LABEL "&Cancelar"      ACCELERATOR "CTRL-F4"
       RULE
       MENU-ITEM mi-salvar      LABEL "Sal&var"        ACCELERATOR "CTRL-S"
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
       SUB-MENU  mi-ajuda       LABEL "A&juda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-cadsim AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_essf0011q02 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_essf0011v02 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "adeicon\next-au":U
     IMAGE-INSENSITIVE FILE "adeicon\next-ai":U
     LABEL "" 
     SIZE 6 BY 1.

DEFINE BUTTON bt-corte 
     IMAGE-UP FILE "image/im-calc.bmp":U
     IMAGE-INSENSITIVE FILE "image/im-calc.bmp":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Otimizaá∆o de Corte".

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon\prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 6 BY 1.

DEFINE BUTTON bt-manut 
     IMAGE-UP FILE "image/im-imems.bmp":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Manutená∆o de Reserva de ATP".

DEFINE BUTTON bt-param 
     IMAGE-UP FILE "image/gr-fil.bmp":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "ParÉmetros".

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 118.43 BY 11.13.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 118.57 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */

DEFINE QUERY br-ped-campanha FOR 
      tt-ped-campanha SCROLLING.

DEFINE QUERY br-pedido FOR 
      tt-pedido SCROLLING.


/* Browse definitions                                                   */
DEFINE BROWSE br-ped-campanha

  QUERY br-ped-campanha NO-LOCK DISPLAY
      tt-ped-campanha.nr-pedcli     COLUMN-LABEL "Pedido"       FORMAT "x(12)"          
      tt-ped-campanha.nr-sequencia  COLUMN-LABEL "SQ"           FORMAT ">>9" 
      tt-ped-campanha.nome-abrev    COLUMN-LABEL "Cliente"      FORMAT "x(14)"  
      tt-ped-campanha.nome-abrev-fim    COLUMN-LABEL "Cli.Fim"      FORMAT "x(14)"  
      tt-ped-campanha.i-perc-var    COLUMN-LABEL "%Var"         FORMAT ">>9"
      tt-ped-campanha.qt-pedida     COLUMN-LABEL "Qtd"          FORMAT ">>>>,>>9.9999"
      tt-ped-campanha.dt-entrega    COLUMN-LABEL "Data Entrega" FORMAT "99/99/9999"
      tt-ped-campanha.log-tipo      COLUMN-LABEL "Tipo"
/* _UIB-CODE-BLOCK-END */

    WITH NO-ROW-MARKERS SEPARATORS SIZE 60 BY 10.5 EXPANDABLE.

DEFINE BROWSE br-pedido

  QUERY br-pedido NO-LOCK DISPLAY
      tt-pedido.nr-pedcli     COLUMN-LABEL "Pedido"  FORMAT "x(12)"
      tt-pedido.nr-sequencia  COLUMN-LABEL "SQ"      FORMAT ">>9"
      tt-pedido.nome-abrev    COLUMN-LABEL "Cliente" FORMAT "x(12)"
      tt-pedido.nome-abrev-fim    COLUMN-LABEL "Cli.Fim" FORMAT "x(12)"
      tt-pedido.qt-pedida     COLUMN-LABEL "Qtd"     FORMAT '>>,>>9.99'
      tt-pedido.dt-entrega    COLUMN-LABEL "Data Entrega" FORMAT "99/99/9999"
      tt-pedido.log-tipo      COLUMN-LABEL "Tipo"
/* _UIB-CODE-BLOCK-END */

    WITH NO-ROW-MARKERS SEPARATORS SIZE 50 BY 10.5 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-corte AT ROW 1.38 COL 54 HELP
          "Otimizaá∆o de Corte"
     bt-manut AT ROW 1.38 COL 58 HELP
          "Manutená∆o de Reserva de ATP"
     br-pedido AT ROW 8.75 COL 2.57
     bt-param AT ROW 10.75 COL 54 HELP
          "ParÉmetros"
     bt-add AT ROW 12.5 COL 53.14
     bt-del AT ROW 14.13 COL 53.14
     br-ped-campanha AT ROW 8.75 COL 59.72
     RECT-18 AT ROW 8.38 COL 1.86
     rt-button AT ROW 1.21 COL 1.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120 BY 18.92.


/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: w-cadcom
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
 */


/* *************************  Create Window  ************************** */


IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-cadcom ASSIGN
         HIDDEN             = YES
         TITLE              = "Manutená∆o de Campanha - essf0011B"
         HEIGHT             = 19.08
         WIDTH              = 120.29
         MAX-HEIGHT         = 28.21
         MAX-WIDTH          = 156.29
         VIRTUAL-HEIGHT     = 28.21
         VIRTUAL-WIDTH      = 156.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE w-cadcom = CURRENT-WINDOW.

ASSIGN w-cadcom:MENUBAR    = MENU m-cadastro:HANDLE.
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

/* vari†vel que identifica dialogs - n∆o pode-se utilizar pre-processador */
/* porque ela n∆o Ç a primeira include do method libraries                */
/* Vari†vel criada para saber quando um programa Ç dialog.                */
/* Se for YES Ç dialog. A vari†vel foi criada devido a problemas          */
/* com traduá∆o - 1085750 - Valdir tech14187                              */
DEFINE VARIABLE adm-dialog AS LOGICAL INITIAL NO   NO-UNDO.



/*************************************************
* i_dbvers.i - Include de vers∆o de banco de dados   
**************************************************/

/* Preprocessadores que identificam os bancos do Produto EMS 5 */

/* Preprocessadores que identificam os bancos do Produto EMS 2 */
/*RAC Incorporado na 2.04*/

/* Preprocessadores que identificam os bancos do Produto HR 2 */

/* Fim */

 



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


  
    ASSIGN adm-object-hdl    =   w-cadcom.
  


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
     'w-cadcom~`':U +      /* Type attribute */
     'WINDOW~`':U +       /* Container-Type attribute */
   
     'NO ~`':U +
   
     '~`':U +    /* External-Tables attribute */
     'tt-ped-campanha tt-pedido~`':U +    /* Internal-Tables attribute */
   
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

 
    

    
    DISABLE bt-corte bt-manut br-pedido bt-param bt-add bt-del br-ped-campanha RECT-18 rt-button WITH FRAME f-cad.
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

 
   

   
    ENABLE UNLESS-HIDDEN bt-corte bt-manut br-pedido bt-param bt-add bt-del br-ped-campanha RECT-18 rt-button WITH FRAME f-cad.

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
         p-wh-frame:FONT    = 1.
  
  assign p-wh-frame = p-wh-frame:FIRST-CHILD
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
"Library para window cadastro complexo"
*/


/*--------------------------------------------------------------------------
    Library     : w-cadcom.i
    Purpose     : Permitir customizaá∆o para as window de cadastro complexo

    Syntax      : {include/w-cadcom.i}

    Description : Library utilizada para customizaá∆o da window de cadastro
                  complexo

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

run pi-trad-menu (input w-cadcom:menubar).

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-before-initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-disable-menu :
def var p-button-enable as char no-undo.
  
  RUN get-button-enable IN h_p-cadsim (OUTPUT p-button-enable).
  assign menu-item mi-incluir:sensitive  in menu m-cadastro = (entry(1,p-button-enable)= string(yes))
         menu-item mi-copiar:sensitive   in menu m-cadastro = (entry(2,p-button-enable)= string(yes))
         menu-item mi-alterar:sensitive  in menu m-cadastro = (entry(3,p-button-enable)= string(yes))
         menu-item mi-eliminar:sensitive in menu m-cadastro = (entry(4,p-button-enable)= string(yes))
         menu-item mi-desfazer:sensitive in menu m-cadastro = (entry(5,p-button-enable)= string(yes))
         menu-item mi-cancelar:sensitive in menu m-cadastro = (entry(6,p-button-enable)= string(yes))
         menu-item mi-salvar:sensitive   in menu m-cadastro = (entry(7,p-button-enable)= string(yes)).
  
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







PROCEDURE pi-enter-go :
/*------------------------------------------------------------------------------
  Purpose:     Trata salvar para go e return
  Notes:       
------------------------------------------------------------------------------*/
  run pi-salvar IN h_p-cadsim.
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


/* SETTINGS FOR WINDOW w-cadcom
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   Custom                                                               */
/* BROWSE-TAB br-pedido bt-manut f-cad */
/* BROWSE-TAB br-ped-campanha bt-del f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadcom)
THEN w-cadcom:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */



/* Setting information for Queries and Browse Widgets fields            */


/* Query rebuild information for BROWSE br-ped-campanha
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-campanha NO-LOCK
    BY tt-ped-campanha.nr-pedcli
    BY tt-ped-campanha.nome-abrev INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-ped-campanha */



/* Query rebuild information for BROWSE br-pedido
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-pedido NO-LOCK
    BY tt-pedido.nr-pedcli
    BY tt-pedido.nome-abrev INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-pedido */


 



/* ************************  Control Triggers  ************************ */


ON END-ERROR OF w-cadcom /* Manutená∆o de Campanha - essf0011B */
OR ENDKEY OF w-cadcom ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
 RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON WINDOW-CLOSE OF w-cadcom /* Manutená∆o de Campanha - essf0011B */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-add IN FRAME f-cad
DO:
    IF  AVAIL tt-pedido AND
        AVAIL campanha-polo  THEN DO:

        ASSIGN l-cria-ped-campanha = YES.
         
        

       FIND FIRST ped-item WHERE 
                  tt-pedido.it-codigo    = ped-item.it-codigo AND 
                  tt-pedido.nr-pedcli    = ped-item.nr-pedcli AND
                  tt-pedido.nome-abrev   = ped-item.nome-abrev AND
                  tt-pedido.nr-sequencia = ped-item.nr-sequencia NO-LOCK NO-ERROR.
       FIND ped-venda OF ped-item NO-LOCK NO-ERROR.                  


        IF AVAIL ped-venda THEN
                         RUN pdp\upc\upc-pd4000-a.p (INPUT ped-venda.cod-estabel,INPUT tt-pedido.it-codigo, INPUT tt-pedido.nome-abrev-fim).
        

        IF tt-pedido.l-possui-prog = YES THEN DO:

            RUN utp/ut-msgs.p (INPUT "show",
                               INPUT 27100,
                               INPUT 'O pedido atual j† possui planejamento. Deseja inclu°-lo na campanha mesmo assim ?' + '~~' +
                                     'Data das campanhas: ' + tt-pedido.dt-campanhas + ' ' +
                                     'Quantidade das campanhas: ' + tt-pedido.qt-campanhas + ' ' +
                                     'Total do pedido nas campanhas: ' + STRING(tt-pedido.tot-campanhas) + ' Kg ' +
                                     'Ordens de Produá∆o: ' + op-campanhas + ' ' +
                                     'Quantidade original do pedido: ' + STRING(tt-pedido.qt-orig-ped) + ' Kg').

            IF RETURN-VALUE = "NO":U THEN
    
                ASSIGN l-cria-ped-campanha = NO.

        END.

        IF l-cria-ped-campanha THEN DO:

            FIND CURRENT campanha-polo NO-LOCK NO-ERROR.
            
            IF   CAN-FIND(FIRST prog-corte WHERE prog-corte.gm-codigo  = campanha-polo.gm-codigo
                                             AND   prog-corte.ano        = campanha-polo.ano
                                             AND   prog-corte.mes        = campanha-polo.mes
                                         AND   prog-corte.cd-produto = campanha-polo.cd-produto
                                         AND   prog-corte.sequencia  = campanha-polo.sequencia)
            THEN DO:
                RUN utp/ut-msgs.p (INPUT "show",
                                   INPUT 17006,
                                   INPUT "J† existe programa de corte para a campanha."
                                         + "~~" 
                                         + "N∆o Ç poss°vel associar novos pedidos Ö campanha quando o programa de corte j† foi gerado").
                LEAVE.
            END.
    
            ASSIGN d-maq-largura-min     = 0
                   d-maq-largura-max     = 0
                   d-item-largura        = 0
                   d-item-diam-int       = 0
                   d-item-diam-ext       = 0
                   d-item-peso           = 0
                   i-item-nr-bobinas     = 0
                   i-item-nr-min-bobinas = 0
                   i-item-nr-max-bobinas = 0.
    
            /* Busca Caracteristicas Tecnicas deste Grupo Maquina */
            FIND FIRST ctrab-linha
                 WHERE ctrab-linha.cod-ctrab = campanha-polo.gm-codigo NO-LOCK NO-ERROR.
    
            IF  AVAIL ctrab-linha THEN DO:
                FIND FIRST ext-grup-maq
                     WHERE ext-grup-maq.gm-codigo = ctrab-linha.gm-codigo-corte NO-LOCK NO-ERROR.
                     
                     
                                 /*provisorio c-folha-esp  ext-grup-maq.cd-folh-maq*/   
                if ctrab-linha.cod-ctrab = "412-bms1" OR ctrab-linha.cod-ctrab = "422-bms1" then   c-folha-esp = "corte 1"./*solic-318*/                   
                if ctrab-linha.cod-ctrab = "412-bms2" OR ctrab-linha.cod-ctrab = "422-bms2" then   c-folha-esp = "corte 2"./*solic-318*/
                if ctrab-linha.cod-ctrab = "412-bms3" OR ctrab-linha.cod-ctrab = "422-bms3" then   c-folha-esp = "corte 3"./*solic-318*/
   
                     
    
                IF  AVAIL ext-grup-maq THEN DO:
                    FIND FIRST ctrab-carac-tec
                         WHERE ctrab-carac-tec.gm-codigo = ext-grup-maq.gm-codigo   AND
                               ctrab-carac-tec.cd-folha  = c-folha-esp AND
                               ctrab-carac-tec.cd-comp   = "LARG-MIN":U             NO-LOCK NO-ERROR.
    
                    IF  AVAIL ctrab-carac-tec THEN
                        ASSIGN d-maq-largura-min = ctrab-carac-tec.vl-result.
    
                    FIND FIRST ctrab-carac-tec
                         WHERE ctrab-carac-tec.gm-codigo = ext-grup-maq.gm-codigo   AND
                               ctrab-carac-tec.cd-folha  = c-folha-esp AND
                               ctrab-carac-tec.cd-comp   = "LARG-MAX":U             NO-LOCK NO-ERROR.
    
                    IF  AVAIL ctrab-carac-tec THEN
                        ASSIGN d-maq-largura-max = ctrab-carac-tec.vl-result.
                END.
            END.
    
            CASE tt-pedido.log-tipo:
                WHEN 1 THEN DO:
                    FIND FIRST cot-est-mast
                         WHERE cot-est-mast.item-cotacao = tt-pedido.it-codigo AND
                               cot-est-mast.nr-estrut    = tt-pedido.nr-config NO-LOCK NO-ERROR.
                END.
    
                WHEN 2 THEN DO:
                    FIND FIRST cot-est-mast
                         WHERE cot-est-mast.item-cotacao = tt-pedido.it-codigo          AND
                               cot-est-mast.nr-estrut    = INTEGER(tt-pedido.cod-refer) NO-LOCK NO-ERROR.
                END.
            END CASE.
    
            /* Busca Caracteristicas Tecnicas do Item do Pedido */
            IF  AVAIL cot-est-mast THEN DO:
                FIND FIRST var-result
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao AND
                           var-result.nr-estrut    = cot-est-mast.nr-estrut    AND
                           var-result.nome-var     = "LARGURA":U               NO-LOCK NO-ERROR.
    
                IF  AVAIL var-result THEN
                    ASSIGN d-item-largura = DECIMAL(var-result.des-result).
    
                IF  d-item-largura < d-maq-largura-min OR
                    d-item-largura > d-maq-largura-max THEN DO:
                    RUN utp/ut-msgs.p(INPUT "SHOW":U,
                                      INPUT "25997",
                                      INPUT "Dimens‰es do Item Inv†lidas." + "~~" + "A largura do item difere da largura m°nima e/ou m†xima permitida ao Grupo de M†quina de Corte.").
                    RETURN "NOK":U.
                END.
    
                FIND FIRST var-result
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao AND
                           var-result.nr-estrut    = cot-est-mast.nr-estrut    AND
                           var-result.nome-var     = "DIIN":U                  NO-LOCK NO-ERROR.
    
                IF  AVAIL var-result THEN
                    ASSIGN d-item-diam-int = DECIMAL(var-result.des-result).
    
                FIND FIRST var-result
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao AND
                           var-result.nr-estrut    = cot-est-mast.nr-estrut    AND
                           var-result.nome-var     = "DIEX":U                  NO-LOCK NO-ERROR.
    
                IF  AVAIL var-result THEN
                    ASSIGN d-item-diam-ext = DECIMAL(var-result.des-result).
    
                FIND FIRST var-result
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao AND
                           var-result.nr-estrut    = cot-est-mast.nr-estrut    AND
                           var-result.nome-var     = "QTDBOB":U                NO-LOCK NO-ERROR.
    
                IF  AVAIL var-result THEN
                    ASSIGN i-item-nr-bobinas = INTEGER(var-result.des-result).
    
                FIND FIRST var-result
                     WHERE var-result.item-cotacao = cot-est-mast.item-cotacao AND
                           var-result.nr-estrut    = cot-est-mast.nr-estrut    AND
                           var-result.nome-var     = "PESOBOB":U               NO-LOCK NO-ERROR.
    
                IF  AVAIL var-result THEN
                    ASSIGN d-item-peso = DECIMAL(var-result.des-result).
    
                ASSIGN i-item-nr-min-bobinas = i-item-nr-bobinas
                       i-item-nr-max-bobinas = i-item-nr-bobinas.
            END.
    
            /* Cria Registro na tt-ped-campanha */
            FIND FIRST tt-ped-campanha
                 WHERE tt-ped-campanha.gm-codigo    = campanha-polo.gm-codigo     AND
                       tt-ped-campanha.ano-campanha = campanha-polo.ano-campanha  AND
                       tt-ped-campanha.mes-campanha = campanha-polo.mes-campanha  AND
                       tt-ped-campanha.cd-produto   = campanha-polo.cd-produto    AND
                       tt-ped-campanha.sequencia    = campanha-polo.sequencia     AND
                       tt-ped-campanha.nome-abrev   = tt-pedido.nome-abrev   AND
                       tt-ped-campanha.nr-pedcli    = tt-pedido.nr-pedcli    AND
                       tt-ped-campanha.nr-sequencia = tt-pedido.nr-sequencia EXCLUSIVE-LOCK NO-ERROR.
                
            IF  NOT AVAIL tt-ped-campanha THEN DO:
    
                CREATE tt-ped-campanha.
    
                ASSIGN tt-ped-campanha.gm-codigo    = campanha-polo.gm-codigo
                       tt-ped-campanha.ano-campanha = campanha-polo.ano-campanha
                       tt-ped-campanha.mes-campanha = campanha-polo.mes-campanha
                       tt-ped-campanha.cd-produto   = campanha-polo.cd-produto
                       tt-ped-campanha.sequencia    = campanha-polo.sequencia
                       tt-ped-campanha.nome-abrev   = tt-pedido.nome-abrev
                       tt-ped-campanha.nome-abrev-fim   = tt-pedido.nome-abrev-fim
                       tt-ped-campanha.nr-pedcli    = tt-pedido.nr-pedcli
                       tt-ped-campanha.nr-sequencia = tt-pedido.nr-sequencia
                       tt-ped-campanha.qt-pedida    = tt-pedido.qt-pedida
                       tt-ped-campanha.dt-entrega   = tt-pedido.dt-entrega
                       tt-ped-campanha.log-tipo     = tt-pedido.log-tipo
                       tt-ped-campanha.it-codigo    = tt-pedido.it-codigo.
        
            END.
                
            
            /* Cria Registro na ped-campanha */
            FIND FIRST ped-campanha
                 WHERE ped-campanha.gm-codigo    = campanha-polo.gm-codigo     AND
                       ped-campanha.ano-campanha = campanha-polo.ano-campanha  AND
                       ped-campanha.mes-campanha = campanha-polo.mes-campanha  AND
                       ped-campanha.cd-produto   = campanha-polo.cd-produto    AND
                       ped-campanha.sequencia    = campanha-polo.sequencia     AND
                       ped-campanha.nome-abrev   = tt-pedido.nome-abrev   AND
                       ped-campanha.nr-pedcli    = tt-pedido.nr-pedcli    AND
                       ped-campanha.nr-sequencia = tt-pedido.nr-sequencia EXCLUSIVE-LOCK NO-ERROR.
    
            IF  NOT AVAIL ped-campanha THEN
                CREATE ped-campanha.
    
            ASSIGN ped-campanha.gm-codigo      = campanha-polo.gm-codigo
                   ped-campanha.ano-campanha   = campanha-polo.ano-campanha
                   ped-campanha.mes-campanha   = campanha-polo.mes-campanha
                   ped-campanha.cd-produto     = campanha-polo.cd-produto
                   ped-campanha.sequencia      = campanha-polo.sequencia
                   ped-campanha.nome-abrev     = tt-pedido.nome-abrev
                   ped-campanha.nr-pedcli      = tt-pedido.nr-pedcli
                   ped-campanha.nr-sequencia   = tt-pedido.nr-sequencia
                   ped-campanha.qt-pedida      = tt-pedido.qt-pedida
                   ped-campanha.dec-1          = tt-pedido.qt-pedida   /* Backup da quantidade original do pedido para recuperaá∆o posterior */
                   ped-campanha.dt-entrega     = tt-pedido.dt-entrega
                   ped-campanha.it-codigo      = tt-pedido.it-codigo
                   ped-campanha.cod-refer      = tt-pedido.cod-refer
                   ped-campanha.largura        = d-item-largura
                   ped-campanha.diam-int       = d-item-diam-int
                   ped-campanha.diam-ext       = d-item-diam-ext
                   ped-campanha.diam-ext-conj  = ped-campanha.diam-ext
                   ped-campanha.peso           = d-item-peso
                   ped-campanha.nr-bobinas     = tt-pedido.nr-bobinas
                   ped-campanha.nr-min-bobinas = tt-pedido.nr-bobinas
                   ped-campanha.nr-max-bobinas = tt-pedido.nr-bobinas
                   ped-campanha.dec-2          = tt-pedido.saldo-var. /* Saldo dispon°vel para a criaá∆o de bobinas a mais para o pedido */
                                                                      /* Esta quantidade ser† utilizada pelo programa polsf013, browser  */
                                                                      /* de pedidos, colunar 'var' */
    
            FIND CURRENT ped-campanha NO-LOCK NO-ERROR.
             
            
            /* Atualizacao dos Valores da Campanha */
            ASSIGN i-producao-hora = 1.
    
            FIND FIRST ext-rec-prod
                 WHERE ext-rec-prod.cd-produto = campanha-polo.cd-produto NO-LOCK NO-ERROR.
    
            IF  AVAIL ext-rec-prod THEN
                ASSIGN i-producao-hora = ext-rec-prod.qt-produto.
            ELSE 
                ASSIGN i-producao-hora = 0.
      
            FIND FIRST est-prod WHERE
                       est-prod.cd-produto = campanha-polo.cd-produto AND
                       est-prod.it-codigo  = ped-campanha.it-codigo NO-LOCK NO-ERROR.
    
            IF AVAILABLE est-prod AND est-prod.perc-refugo < 100 THEN
                ASSIGN de-correcao-refugo = 1 - est-prod.perc-refugo / 100.
            ELSE 
                ASSIGN de-correcao-refugo = 1.
    
            FIND CURRENT campanha-polo EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN campanha-polo.quantidade = campanha-polo.quantidade + ped-campanha.qt-pedida / de-correcao-refugo.  
                   campanha-polo.tempo      = IF i-producao-hora = 0 THEN 0 
                                         ELSE campanha-polo.quantidade / i-producao-hora.
            FIND CURRENT campanha-polo NO-LOCK NO-ERROR.
     
            /* Recalculo dos Tempos das Campanhas */
            RUN pi-recalcula-tempos IN THIS-PROCEDURE (INPUT campanha-polo.gm-codigo,
                                                       INPUT campanha-polo.int-1).
     
            IF  VALID-HANDLE(h_essf0011v02) THEN DO:
                RUN pi-reposiciona-query IN h_essf0011q02 (INPUT ROWID(campanha-polo)).
            END.
 
            FOR EACH tt-ped-campanha:
                FIND FIRST emitente
                     WHERE emitente.nome-abrev = tt-ped-campanha.nome-abrev-fim NO-LOCK NO-ERROR.
                IF AVAIL emitente THEN
                    ASSIGN tt-ped-campanha.i-perc-var = emitente.perc-fat-ped.
            END.
 
            OPEN QUERY br-ped-campanha FOR EACH tt-ped-campanha NO-LOCK     BY tt-ped-campanha.nr-pedcli     BY tt-ped-campanha.nome-abrev INDEXED-REPOSITION.

        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-corte IN FRAME f-cad
DO:

    def var p-rowid as rowid no-undo.


RUN pi-ver-dll.

    RUN pi-posicao-query IN h_essf0011q02 (OUTPUT p-rowid). 

    find campanha-polo where rowid(campanha-polo) = p-rowid no-lock no-error.

    IF  AVAIL campanha-polo THEN DO: 
        ASSIGN w-cadcom:SENSITIVE = NO.
        RUN sfc/essf0011d.w (INPUT ROWID(campanha-polo)).
        ASSIGN w-cadcom:SENSITIVE = YES.

        IF  VALID-HANDLE(h_essf0011v02) THEN DO:
            RUN pi-reposiciona-query IN h_essf0011q02 (INPUT ROWID(campanha-polo)).
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-del IN FRAME f-cad
DO:
    IF  AVAIL tt-ped-campanha AND
        AVAIL campanha-polo        THEN DO:

        FIND CURRENT campanha-polo NO-LOCK NO-ERROR.

        IF   CAN-FIND(FIRST prog-corte WHERE prog-corte.gm-codigo  = campanha-polo.gm-codigo
                                         AND   prog-corte.ano        = campanha-polo.ano
                                         AND   prog-corte.mes        = campanha-polo.mes
                                         AND   prog-corte.cd-produto = campanha-polo.cd-produto
                                         AND   prog-corte.sequencia  = campanha-polo.sequencia)
          THEN DO:
               RUN utp/ut-msgs.p (INPUT "show",
                                 INPUT 17006,
                                 INPUT "J† existe programa de corte para a campanha."
                                       + "~~" 
                                       + "N∆o Ç poss°vel desassociar pedidos da campanha quando o programa de corte j† foi gerado").
               LEAVE.
        END.



        /* Atualizacao dos Valores da Campanha */
        ASSIGN i-producao-hora = 1.

        FIND FIRST ext-rec-prod
             WHERE ext-rec-prod.cd-produto = campanha-polo.cd-produto NO-LOCK NO-ERROR.

        IF  AVAIL ext-rec-prod THEN
            ASSIGN i-producao-hora = ext-rec-prod.qt-produto.
        ELSE
            ASSIGN i-producao-hora = 0.

        FIND CURRENT campanha-polo EXCLUSIVE-LOCK NO-ERROR.

        FIND FIRST est-prod WHERE
                     est-prod.cd-produto = campanha-polo.cd-produto AND
                     est-prod.it-codigo  = tt-ped-campanha.it-codigo NO-LOCK NO-ERROR.

        IF AVAILABLE est-prod AND est-prod.perc-refugo < 100 THEN
              ASSIGN de-correcao-refugo = 1 - est-prod.perc-refugo / 100.
        ELSE 
              ASSIGN de-correcao-refugo = 1.

        ASSIGN campanha-polo.quantidade = max(0, campanha-polo.quantidade - tt-ped-campanha.qt-pedida / de-correcao-refugo).  
               campanha-polo.tempo      = IF i-producao-hora = 0 THEN 0 
                                      ELSE campanha-polo.quantidade / i-producao-hora.

        /* Recalculo dos Tempos das Campanhas */
        RUN pi-recalcula-tempos IN THIS-PROCEDURE (INPUT campanha-polo.gm-codigo,
                                                   INPUT campanha-polo.int-1).

        /* Elimina Registro na Tabela ped-campanha */
        FIND FIRST ped-campanha
             WHERE ped-campanha.gm-codigo    = tt-ped-campanha.gm-codigo    AND
                   ped-campanha.ano-campanha = tt-ped-campanha.ano-campanha AND
                   ped-campanha.mes-campanha = tt-ped-campanha.mes-campanha AND
                   ped-campanha.cd-produto   = tt-ped-campanha.cd-produto   AND
                   ped-campanha.sequencia    = tt-ped-campanha.sequencia    AND
                   ped-campanha.nome-abrev   = tt-ped-campanha.nome-abrev   AND
                   ped-campanha.nr-pedcli    = tt-ped-campanha.nr-pedcli    AND
                   ped-campanha.nr-sequencia = tt-ped-campanha.nr-sequencia EXCLUSIVE-LOCK NO-ERROR.

        IF  AVAIL ped-campanha THEN
            DELETE ped-campanha.

        DELETE tt-ped-campanha.

        /*** Verifica se ainda existe pedido associado a campanha para zerar quantidade **/
        IF NOT CAN-FIND (FIRST ped-campanha
                     WHERE ped-campanha.gm-codigo    = campanha-polo.gm-codigo    AND
                           ped-campanha.ano-campanha = campanha-polo.ano-campanha AND
                           ped-campanha.mes-campanha = campanha-polo.mes-campanha AND
                           ped-campanha.cd-produto   = campanha-polo.cd-produto   AND
                           ped-campanha.sequencia    = campanha-polo.sequencia) 
            AND campanha-polo.quantidade > 0 THEN do:
            
            FIND CURRENT campanha-polo EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN campanha-polo.quantidade = 0  
                   campanha-polo.tempo      = 0.

        END.

        IF  VALID-HANDLE(h_essf0011v02) THEN DO:
            RUN pi-reposiciona-query IN h_essf0011q02 (INPUT ROWID(campanha-polo)).
        END.

        FOR EACH tt-ped-campanha:
            FIND FIRST emitente
                 WHERE emitente.nome-abrev = tt-ped-campanha.nome-abrev-fim NO-LOCK NO-ERROR.
            IF AVAIL emitente THEN
                ASSIGN tt-ped-campanha.i-perc-var = emitente.perc-fat-ped.
        END.

        OPEN QUERY br-ped-campanha FOR EACH tt-ped-campanha NO-LOCK     BY tt-ped-campanha.nr-pedcli     BY tt-ped-campanha.nome-abrev INDEXED-REPOSITION.

    END.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-manut IN FRAME f-cad
DO:
    IF  AVAIL campanha-polo THEN DO:
        FIND FIRST item-cli
             WHERE item-cli.nome-abrev = "POLO RS":U     AND
                   item-cli.it-codigo  = campanha-polo.it-codigo NO-LOCK NO-ERROR.

        IF  AVAIL item-cli THEN DO:
            ASSIGN gr-item-cli              = ROWID(item-cli)
                   w-cadcom:SENSITIVE = NO.
            RUN pmp/pm0311.w.
            ASSIGN w-cadcom:SENSITIVE = YES.
        END.
        ELSE DO:
            RUN utp/ut-msgs.p(INPUT "SHOW":U,
                              INPUT "25997",
                              INPUT "Item do Cliente Inexistente." + "~~" + "Verifique se existe uma ocorrància cadastrada para o Cliente POLO RS e o Item " + STRING(campanha-polo.it-codigo) + ".").
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-param IN FRAME f-cad
DO:

     
    RUN sfc/essf0011c.w(INPUT-OUTPUT p-cod-estabel,
                             INPUT-OUTPUT p-nr-pedido-ini,
                             INPUT-OUTPUT p-nr-pedido-fim,
                             INPUT-OUTPUT p-nome-ini,
                             INPUT-OUTPUT p-nome-fim,
                             INPUT-OUTPUT p-tp-ped-ini,
                             INPUT-OUTPUT p-tp-ped-fim,
                             INPUT-OUTPUT p-data-ini,
                             INPUT-OUTPUT p-data-fim,
                             INPUT-OUTPUT p-mercado,
                             OUTPUT p-acao).
  
    IF p-acao <> 2  THEN DO: /*** bot∆o cancela do parÉmetro ***/

        FOR EACH tt-pedido:
            DELETE tt-pedido.
        END.
        FOR EACH tt-ped-campanha:
            DELETE tt-ped-campanha.
        END.
        OPEN QUERY br-pedido FOR EACH tt-pedido NO-LOCK     BY tt-pedido.nr-pedcli     BY tt-pedido.nome-abrev INDEXED-REPOSITION.
        OPEN QUERY br-ped-campanha FOR EACH tt-ped-campanha NO-LOCK     BY tt-ped-campanha.nr-pedcli     BY tt-ped-campanha.nome-abrev INDEXED-REPOSITION. 
        
        FIND FIRST campanha-polo
            WHERE rowid(campanha-polo) = r-campanha EXCLUSIVE-LOCK NO-ERROR.
          
        IF AVAIL campanha-polo THEN DO:
             
            RUN pi-cria-tt-pedido IN THIS-PROCEDURE.
            RUN pi-cria-tt-ped-campanha IN THIS-PROCEDURE. 
            OPEN QUERY br-pedido FOR EACH tt-pedido NO-LOCK     BY tt-pedido.nr-pedcli     BY tt-pedido.nome-abrev INDEXED-REPOSITION.
            OPEN QUERY br-ped-campanha FOR EACH tt-ped-campanha NO-LOCK     BY tt-ped-campanha.nr-pedcli     BY tt-ped-campanha.nome-abrev INDEXED-REPOSITION. 
        END.
    END.


END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF MENU-ITEM mi-alterar /* Alterar */
DO:
  RUN pi-alterar IN h_p-cadsim.
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




ON CHOOSE OF MENU-ITEM mi-cancelar /* Cancelar */
DO:
 /* RUN pi-cancelar IN h_p-cadsim. */
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




ON CHOOSE OF MENU-ITEM mi-copiar /* Copiar */
DO:
  RUN pi-copiar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF MENU-ITEM mi-desfazer /* Desfazer */
DO:
  RUN pi-desfazer IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF MENU-ITEM mi-eliminar /* Eliminar */
DO:
  RUN pi-eliminar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF MENU-ITEM mi-incluir /* Incluir */
DO:
  RUN pi-incluir IN h_p-cadsim.
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




ON CHOOSE OF MENU-ITEM mi-salvar /* Salvar */
DO:
  RUN pi-salvar IN h_p-cadsim.
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


ASSIGN c-data     = STRING(TODAY,"99/99/9999")
       p-data-ini = DATE("01" + SUBSTRING(c-data,3,8))
       da-data    = p-data-ini + 31
       c-data     = STRING(da-data,"99/99/9999")
       p-data-fim = DATE("01" + SUBSTRING(c-data,3,8)) - 1.

/* Include custom  Main Block code for SmartWindows. */
/* windowmn.i - Main Block code for objects which create windows.*/
/* Skip all of this if no window was created. */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
IF VALID-HANDLE(w-cadcom) THEN DO:
    ASSIGN CURRENT-WINDOW                = w-cadcom 
       w-cadcom:KEEP-FRAME-Z-ORDER = YES
       THIS-PROCEDURE:CURRENT-WINDOW = w-cadcom.

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
       RUN set-position IN h_p-navega ( 1.33 , 2.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 24.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-cadsim.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-cadsim ).
       RUN set-position IN h_p-cadsim ( 1.33 , 26.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 28.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.33 , 103.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sfc/essf0011v02.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_essf0011v02 ).
       RUN set-position IN h_essf0011v02 ( 2.88 , 1.72 ) NO-ERROR.
       /* Size in UIB:  ( 5.33 , 88.57 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'sfc/essf0011q02.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'ProgPesquisa = sfc/essf0011z02.w,
                     ProgVaPara = sfc/essf0011g02.w,
                     ProgIncMod = ,
                     Implantar = no':U ,
             OUTPUT h_essf0011q02 ).
       RUN set-position IN h_essf0011q02 ( 3.25 , 82.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.42 , 7.72 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-cadsim , 'State':U , h_p-exihel ).

       /* Links to SmartViewer h_essf0011v02. */
       RUN add-link IN adm-broker-hdl ( h_p-cadsim , 'TableIO':U , h_essf0011v02 ).
       RUN add-link IN adm-broker-hdl ( h_essf0011q02 , 'Record':U , h_essf0011v02 ).

       /* Links to SmartQuery h_essf0011q02. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , h_essf0011q02 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_essf0011q02 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'State':U , h_essf0011q02 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navega ,
             bt-corte:HANDLE IN FRAME f-cad , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-cadsim ,
             h_p-navega , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-manut:HANDLE IN FRAME f-cad , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_essf0011v02 ,
             h_p-exihel , 'AFTER':U ).
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadcom)
  THEN DELETE WIDGET w-cadcom.
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
  ENABLE bt-corte bt-manut br-pedido bt-param bt-add bt-del br-ped-campanha 
         RECT-18 rt-button 
      WITH FRAME f-cad IN WINDOW w-cadcom.
  OPEN QUERY br-ped-campanha FOR EACH tt-ped-campanha NO-LOCK     BY tt-ped-campanha.nr-pedcli     BY tt-ped-campanha.nome-abrev INDEXED-REPOSITION.    OPEN QUERY br-pedido FOR EACH tt-pedido NO-LOCK     BY tt-pedido.nr-pedcli     BY tt-pedido.nome-abrev INDEXED-REPOSITION.
  VIEW w-cadcom.
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

if w-cadcom:width-chars < frame f-cad:width-chars then
    assign frame f-cad:width-chars = w-cadcom:width-chars.
else if frame f-cad:width-chars < w-cadcom:width-chars then
    assign w-cadcom:width-chars = frame f-cad:width-chars.

if w-cadcom:height-chars < frame f-cad:height-chars then
    assign frame f-cad:height-chars = w-cadcom:height-chars.
else if frame f-cad:height-chars < w-cadcom:height-chars then
    assign w-cadcom:height-chars = frame f-cad:height-chars.

assign w-cadcom:virtual-width-chars  = w-cadcom:width-chars
       w-cadcom:virtual-height-chars = w-cadcom:height-chars
       w-cadcom:min-width-chars      = w-cadcom:width-chars
       w-cadcom:max-width-chars      = w-cadcom:width-chars
       w-cadcom:min-height-chars     = w-cadcom:height-chars
       w-cadcom:max-height-chars     = w-cadcom:height-chars.

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

    assign c-programa-mg97 = caps("essf0011B")
           c-versao-mg97   = "2.00.00.002".
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
     
    
         assign w-cadcom:title = if l-achou-prog then
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
      if "w-cadcom" = "SmartDialog" or this-procedure:persistent = no then
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
      if "w-cadcom" = "SmartDialog" then
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


    /*
     * Alteraá∆o 010007
     * De acordo com o KBase Progress P96679 na vers∆o 10 as dll n∆o entendem (?)
     * e devem receber (0).
     */
    DEFINE VARIABLE i-hwnd AS INTEGER     NO-UNDO.
    
    ASSIGN i-hwnd = w-cadcom:HWND.
    
    IF i-hwnd EQ ? AND PROVERSION GE "10" THEN
        ASSIGN i-hwnd = 0.

    /*
     * Fim da alteraá∆o 010007,
     * No restante do programa onde se usava {&WINDOW-NAME}:HWND
     * passou-se a usar i-hwnd.
     */

    case i-template:
        when 2 then do: /* Cadastro Simples */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.

            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.
        end.
        when 3 then do: /* Cadastro Simples - Alteracao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.
        end.
        when 4 then do: /* Cadastro Complexo */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.        
        end.
        when 5 then do: /* Cadastro Complexo - Alteracao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.        
        end.
        when 6 then do: /* Cadastro Simples - Inclusao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.        
        end.
        when 7 then do: /* Cadastro PaiXFilho - Atualiza Filho */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.        
        end.
        when 8 then do: /* Cadastro PaiXFilho - Atualiza Ambos */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
            
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.        
        end.
        when 9 then do: /* Inclui/Modifica Pai */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.        
        end.
        when 10 then do: /* Inclui/Modifica Filho */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.        
        end.
        when 11 then do: /* Formacao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.        
        end.
        when 12 then do: /* Parametros Unicos */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.        
        end.
        when 13 then do: /* Pesquisa */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.        
        end.
        when 14 then do: /* Consulta Simples */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.        
        end.
        when 15 then do: /* Consulta Complexa */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.        
        end.
        when 16 then do: /* Consulta Relacionamento */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.        
        end.
        when 17 then do: /* Relatorio */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.        
        end.
        when 20 then do: /* Janela Detalhe */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD.

            if  h-pai:handle = w-cadcom:handle then
                assign h-pai           = h-pai:NEXT-SIBLING
                       h-pai:SENSITIVE = no.
            else 
                assign  h-pai = w-cadcom:handle
                        h-pai = h-pai:parent.

            h-pai:sensitive = no.
  
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.        
        end.
        when 21 then do: /* Janela Mestre */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.        
        end.
        when 26 then do: /* Importacao/Exportacao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.
        end.
        when 29 then do: /* Relatorio Gerado Pelo DataViewer */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.
        end.
        when 30 then do: /* Formacao Sem Navegacao */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.
        end.
        when 31 then do: /* Estrutura */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input i-hwnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.
        end.
        when 32 then do: /* Digitaá∆o Rapida */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input i-hwnd).
            delete procedure h-prog.
            
            assign w-cadcom:HIDDEN = yes
            w-cadcom:HIDDEN = no.
            apply "ENTRY":U to w-cadcom.
        end.
    end case.

/* Transformacao Window *****************************************************/

 
      end. 
    



/* ut9000.i */
 

  /* Dispatch standard ADM method.                             */

   run pi-recebe-handle in h_essf0011v02 (input this-procedure).
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

   RUN pi-desabilita-botoes IN THIS-PROCEDURE.

   ASSIGN gl-essf0011b-initialize = YES.

   IF  pr-campanha <> ? THEN DO:

       /* Posiciona no Registro Selecionado essf0011 */
       IF  VALID-HANDLE(h_essf0011v02) THEN DO:
           RUN pi-reposiciona-query IN h_essf0011q02 (INPUT pr-campanha).
       END.

       FOR EACH tt-ped-campanha:
            FIND FIRST emitente
                 WHERE emitente.nome-abrev = tt-ped-campanha.nome-abrev-fim NO-LOCK NO-ERROR.
            IF AVAIL emitente THEN
                ASSIGN tt-ped-campanha.i-perc-var = emitente.perc-fat-ped.
        END.

        OPEN QUERY br-ped-campanha FOR EACH tt-ped-campanha NO-LOCK     BY tt-ped-campanha.nr-pedcli     BY tt-ped-campanha.nome-abrev INDEXED-REPOSITION.

       /* Verifica Ordens de Produá∆o */
       FIND campanha-polo WHERE ROWID(campanha-polo) = pr-campanha NO-LOCK NO-ERROR.
       IF AVAIL campanha-polo THEN DO:

           IF CAN-FIND (FIRST ord-prod WHERE ord-prod.nr-ord-produ = campanha-polo.nr-ord-produ) THEN
               DISABLE bt-add bt-del WITH FRAME f-cad.
           ELSE
               ENABLE bt-add bt-del WITH FRAME f-cad.

       END.

   END.

   /* Code placed here will execute AFTER standard behavior.    */
   RUN pi-before-initialize.

   


/*--------------------------------------------------------------------------
    File        : i-inifld.i
    Purpose     : Inicia todas paginas de um folder nos cadastros complexos
                  permitindo desta forma a validacao de todas as paginas do
                  folder
    Syntax      :

    Description :

    Author(s)   : Ricardo de Lima Perdigao
    Created     : 14/03/1997
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
                                                                        */

 






/* ***************************  Main Block  *************************** */
  
     RUN init-pages IN THIS-PROCEDURE ("1,2,3,4,5,6,7,8,9,10":U).
  

/* _UIB-CODE-BLOCK-END */



 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-atualiza-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFiNE INPUT PARAMETER p-rowid-camp AS ROWID NO-UNDO.

    ASSIGN r-campanha = p-rowid-camp.

    FIND FIRST campanha-polo
         WHERE ROWID(campanha-polo) = r-campanha NO-LOCK NO-ERROR.

    IF  AVAIL campanha-polo THEN DO:
        RUN pi-cria-tt-pedido       IN THIS-PROCEDURE.
        RUN pi-cria-tt-ped-campanha IN THIS-PROCEDURE. 
    END.

    OPEN QUERY br-pedido FOR EACH tt-pedido NO-LOCK     BY tt-pedido.nr-pedcli     BY tt-pedido.nome-abrev INDEXED-REPOSITION.
    OPEN QUERY br-ped-campanha FOR EACH tt-ped-campanha NO-LOCK     BY tt-ped-campanha.nr-pedcli     BY tt-ped-campanha.nome-abrev INDEXED-REPOSITION.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-cria-tt-ped-campanha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH tt-ped-campanha:
        DELETE tt-ped-campanha.
    END.

    FOR EACH  ped-campanha
        WHERE ped-campanha.gm-codigo    = campanha-polo.gm-codigo    AND
              ped-campanha.ano-campanha = campanha-polo.ano-campanha AND
              ped-campanha.mes-campanha = campanha-polo.mes-campanha AND
              ped-campanha.cd-produto   = campanha-polo.cd-produto   AND
              ped-campanha.sequencia    = campanha-polo.sequencia    NO-LOCK:

        IF  NOT CAN-FIND (FIRST tt-ped-campanha
                          WHERE tt-ped-campanha.gm-codigo    = ped-campanha.gm-codigo    AND
                                tt-ped-campanha.ano-campanha = ped-campanha.ano-campanha AND
                                tt-ped-campanha.mes-campanha = ped-campanha.mes-campanha AND
                                tt-ped-campanha.cd-produto   = ped-campanha.cd-produto   AND
                                tt-ped-campanha.sequencia    = ped-campanha.sequencia    AND
                                tt-ped-campanha.nome-abrev   = ped-campanha.nome-abrev   AND
                                tt-ped-campanha.nr-pedcli    = ped-campanha.nr-pedcli    AND
                                tt-ped-campanha.nr-sequencia = ped-campanha.nr-sequencia NO-LOCK) THEN DO:
            CREATE tt-ped-campanha.
            BUFFER-COPY ped-campanha TO tt-ped-campanha.

            tt-ped-campanha.nome-abrev-fim = tt-ped-campanha.nome-abrev.

            FOR FIRST ped-venda WHERE ped-venda.nome-abrev = ped-campanha.nome-abrev AND
                                      ped-venda.nr-pedcli  = ped-campanha.nr-pedcli NO-LOCK,
                FIRST if-ped-venda WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-LOCK.

                 tt-ped-campanha.nome-abrev-fim =  if-ped-venda.nome-abrev.

            END.
        END.
    END.

    FOR EACH tt-ped-campanha:
        FIND FIRST emitente
             WHERE emitente.nome-abrev = tt-ped-campanha.nome-abrev-fim NO-LOCK NO-ERROR.
        IF AVAIL emitente THEN
            ASSIGN tt-ped-campanha.i-perc-var = emitente.perc-fat-ped.
    END.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-cria-tt-pedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR de-peso-bobs AS DEC NO-UNDO.
    DEF VAR i-nr-bobinas AS DEC NO-UNDO.
    DEF VAR uc-nr-pedido LIKE  ped-venda.nr-pedido.
    DEF VAR uc-nome-abrev LIKE  ped-venda.nome-abrev.
    DEF VAR uc-cod-estabel LIKE  ped-venda.cod-estabel.
          
    DEFINE BUFFER b-ped-venda FOR ped-venda.

    FOR EACH tt-pedido:
        DELETE tt-pedido.
    END.
  
    EMPTY temp-tabLE tt-peduc.


    FOR EACH  ped-item
       WHERE 
             ped-item.it-codigo     = campanha-polo.it-codigo   AND
             ped-item.dt-entrega   >= p-data-ini            AND
             ped-item.dt-entrega   <= p-data-fim            AND
             ped-item.ind-componen <> 3                     AND
             ped-item.cod-sit-item <= 2                     AND
            (ped-item.qt-pedida - ped-item.qt-atendida) > 0 NO-LOCK,
       FIRST ped-venda OF ped-item
       WHERE ped-venda.cod-estabel  = p-cod-estabel   AND
             ped-venda.nr-pedido   >= p-nr-pedido-ini AND
             ped-venda.nr-pedido   <= p-nr-pedido-fim AND
             ped-venda.tp-ped      >= p-tp-ped-ini    AND
             ped-venda.tp-ped      <= p-tp-ped-fim    AND
             ped-venda.cod-sit-ped <= 2               /*AND  retirado avaliacao de credito Claudia/Baltazar 30/03/2011
             (ped-venda.cod-sit-aval = 3  OR ped-venda.cod-sit-aval = 2)  */          NO-LOCK,
       FIRST natur-oper
       WHERE natur-oper.nat-operacao = ped-venda.nat-operacao AND
            (natur-oper.mercado      = p-mercado              OR
             p-mercado = 3)                                   NO-LOCK:


       uc-nr-pedido = ped-venda.nr-pedido.
       uc-nome-abrev = ped-venda.nome-abrev.
       uc-cod-estabel = ped-venda.cod-estabel.


       FIND FIRST if-ped-venda WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido NO-LOCK NO-ERROR.

      IF AVAIL if-ped-venda THEN NEXT.


       FIND FIRST if-ped-venda WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

       IF AVAIL if-ped-venda THEN DO:


           FIND FIRST b-ped-venda WHERE b-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac NO-LOCK NO-ERROR.

           IF NOT AVAIL b-ped-venda THEN NEXT.

           uc-nr-pedido = ped-venda.nr-pedido.
           uc-nome-abrev = b-ped-venda.nome-abrev.
           uc-cod-estabel = if-ped-venda.cod-estab-atend.


       END.

       IF NOT( uc-nome-abrev   >= p-nome-ini            AND
               uc-nome-abrev   <= p-nome-fim  )          THEN NEXT.


       FIND FIRST tt-peduc WHERE tt-peduc.nr-pedido = uc-nr-pedido NO-ERROR.

       IF NOT AVAIL tt-peduc THEN DO:
           CREATE tt-peduc.
           ASSIGN 
               tt-peduc.nr-pedido = uc-nr-pedido
               tt-peduc.nome-abrev = uc-nome-abrev
               tt-peduc.cod-estabel = uc-cod-estabel.
       END.



END.

/* alteracao para pegar o cliente final quando unigel comercial Edson-Damgra -16-11-2012*/

   FOR EACH tt-peduc ,
        FIRST ped-venda WHERE  
             ped-venda.nr-pedido   = tt-peduc.nr-pedido,
       EACH  ped-item OF ped-venda 
       WHERE  
             ped-item.it-codigo     = campanha-polo.it-codigo   AND
             ped-item.dt-entrega   >= p-data-ini            AND
             ped-item.dt-entrega   <= p-data-fim            AND
             ped-item.ind-componen <> 3                     AND
             ped-item.cod-sit-item <= 2                     AND
            (ped-item.qt-pedida - ped-item.qt-atendida) > 0 NO-LOCK,

       FIRST natur-oper
       WHERE natur-oper.nat-operacao = ped-venda.nat-operacao AND
            (natur-oper.mercado      = p-mercado              OR
             p-mercado = 3)                                   NO-LOCK:



        /* Verifica se o Pedido esta Associado a esta Campanha */
        FIND FIRST ped-campanha
             WHERE ped-campanha.gm-codigo    = campanha-polo.gm-codigo    AND
                   ped-campanha.ano-campanha = campanha-polo.ano-campanha AND
                   ped-campanha.mes-campanha = campanha-polo.mes-campanha AND
                   ped-campanha.cd-produto   = campanha-polo.cd-produto   AND
                   ped-campanha.sequencia    = campanha-polo.sequencia    AND
                   ped-campanha.nome-abrev   = ped-item.nome-abrev   AND
                   ped-campanha.nr-pedcli    = ped-item.nr-pedcli    AND
                   ped-campanha.nr-sequencia = ped-item.nr-sequencia NO-LOCK NO-ERROR.

        IF  AVAIL ped-campanha THEN NEXT.

        /* Verifica se o Pallet tem Pedido e se ja foi Faturado */
        ASSIGN de-saldo = 0.

        FOR EACH  pallet
            WHERE pallet.cod-estabel = ped-venda.cod-estabel AND
                  pallet.it-codigo   = ped-item.it-codigo    AND
                  pallet.nr-pedido   = ped-venda.nr-pedido   AND
                  pallet.situacao    = 2                    and
                  pallet.nr-sequencia = ped-item.nr-sequencia  /*Edson 13062012*/

                   NO-LOCK:

            FIND FIRST saldo-estoq
                 WHERE saldo-estoq.it-codigo   = pallet.it-codigo   AND
                       saldo-estoq.cod-estabel = pallet.cod-estabel AND
                       saldo-estoq.lote        = pallet.nr-pallet   /*AND
                       saldo-estoq.qtidade-atu > 0*/                  NO-LOCK NO-ERROR.

            IF  AVAIL saldo-estoq THEN DO:
                IF saldo-estoq.qtidade-atu > 0 THEN /*Encontra pallet para o pedido nao faturado*/
                   ASSIGN de-saldo = de-saldo + saldo-estoq.qtidade-atu -
                                               (saldo-estoq.qt-alocada  +
                                                saldo-estoq.qt-aloc-ped +
                                                saldo-estoq.qt-aloc-prod).

                IF saldo-estoq.qtidade-atu = 0 THEN /*pallet j† foi faturado*/
                   ASSIGN de-saldo = de-saldo + pallet.peso-liquido.
            END.
        END.
        
        
        find first if-ped-venda where if-ped-venda.nr-pedido = ped-venda.nr-pedido no-lock no-error.
        
        if avail if-ped-venda then do:
        
            FOR EACH  pallet
                WHERE pallet.cod-estabel = if-ped-venda.cod-estab-atend AND
                      pallet.it-codigo   = ped-item.it-codigo    AND
                      pallet.nr-pedido   = if-ped-venda.nr-pedido-relac   AND
                      pallet.situacao    = 2                    and
                      pallet.nr-sequencia = ped-item.nr-sequencia  /*Edson 13062012*/
    
                       NO-LOCK:
    
                FIND FIRST saldo-estoq
                     WHERE saldo-estoq.it-codigo   = pallet.it-codigo   AND
                           saldo-estoq.cod-estabel = pallet.cod-estabel AND
                           saldo-estoq.lote        = pallet.nr-pallet   /*AND
                           saldo-estoq.qtidade-atu > 0*/                  NO-LOCK NO-ERROR.
    
                IF  AVAIL saldo-estoq THEN DO:
                    IF saldo-estoq.qtidade-atu > 0 THEN /*Encontra pallet para o pedido nao faturado*/
                       ASSIGN de-saldo = de-saldo + saldo-estoq.qtidade-atu -
                                                   (saldo-estoq.qt-alocada  +
                                                    saldo-estoq.qt-aloc-ped +
                                                    saldo-estoq.qt-aloc-prod).
    
                    IF saldo-estoq.qtidade-atu = 0 THEN /*pallet j† foi faturado*/
                       ASSIGN de-saldo = de-saldo + pallet.peso-liquido.
                END.
            END.

        
        
        end.
        
        

        IF de-saldo < ped-item.qt-pedida THEN DO: /*Descarta pedido totalmente atendido*/
            
            CREATE tt-pedido.
            ASSIGN tt-pedido.gm-codigo    = campanha-polo.gm-codigo
                   tt-pedido.cd-produto   = campanha-polo.cd-produto
                   tt-pedido.it-codigo    = ped-item.it-codigo
                   tt-pedido.nr-pedcli    = ped-item.nr-pedcli
                   tt-pedido.nome-abrev   = ped-item.nome-abrev
                   tt-pedido.nome-abrev-fim =  tt-peduc.nome-abrev
                   tt-pedido.qt-pedida    = (ped-item.qt-pedida - de-saldo)
                   tt-pedido.dt-entrega   = ped-item.dt-entrega
                   tt-pedido.nr-sequencia = ped-item.nr-sequencia
                   tt-pedido.ano-campanha = campanha-polo.ano-campanha
                   tt-pedido.mes-campanha = campanha-polo.mes-campanha
                   tt-pedido.sequencia    = campanha-polo.sequencia
                   tt-pedido.log-tipo     = 1
                   tt-pedido.nr-config    = ped-item.nr-config
                   tt-pedido.cod-refer    = ped-item.cod-refer
                   tt-pedido.saldo-var    = (ped-item.qt-pedida - de-saldo).  /* Saldo da variaá∆o do pedido (Quanto resta para a variaá∆o) */

            FIND FIRST cot-est-mast
                 WHERE cot-est-mast.item-cotacao = tt-pedido.it-codigo
                   AND cot-est-mast.nr-estrut    = INTEGER(tt-pedido.cod-refer) NO-LOCK NO-ERROR.
            IF AVAIL cot-est-mast THEN DO:

                FIND FIRST emitente WHERE
                     emitente.nome-abrev = tt-peduc.nome-abrev /*ped-venda.cod-emitente alteracao unigel comercial Edson-16-11-2012*/  NO-LOCK NO-ERROR.
                IF AVAIL emitente THEN DO:

                    IF de-saldo = 0 THEN DO:

                        FIND FIRST var-result
                             WHERE var-result.item-cotacao = cot-est-mast.item-cotacao AND
                                   var-result.nr-estrut    = cot-est-mast.nr-estrut    AND
                                   var-result.nome-var     = "QTDBOB":U                NO-LOCK NO-ERROR.

                        IF  AVAIL var-result THEN

                            ASSIGN tt-pedido.nr-bobinas = INTEGER(var-result.des-result)
                                   tt-pedido.saldo-var = (ped-item.qt-pedida * emitente.perc-fat-ped / 100).

                    END.
                    ELSE DO:

                        FIND FIRST var-result
                             WHERE var-result.item-cotacao = cot-est-mast.item-cotacao AND
                                   var-result.nr-estrut    = cot-est-mast.nr-estrut    AND
                                   var-result.nome-var     = "PESOBOB":U               NO-LOCK NO-ERROR.

                        IF AVAIL var-result THEN DO:

                            ASSIGN i-nr-bobinas = tt-pedido.qt-pedida / DEC(var-result.des-result)
                                   de-peso-bobs = TRUNC((i-nr-bobinas + 1), 0) * DEC(var-result.des-result).

                            IF (de-peso-bobs + de-saldo) > (ped-item.qt-pedida + ped-item.qt-pedida * emitente.perc-fat-ped / 100) THEN DO:

                                ASSIGN tt-pedido.nr-bobinas = TRUNC(i-nr-bobinas, 0).
                                IF tt-pedido.nr-bobinas <> 0 THEN
                                    
                                    ASSIGN tt-pedido.qt-pedida  = tt-pedido.nr-bobinas * DEC(var-result.des-result).

                            END.
                            ELSE DO:

                                ASSIGN tt-pedido.nr-bobinas = TRUNC(i-nr-bobinas + 1, 0)
                                       tt-pedido.qt-pedida  = tt-pedido.nr-bobinas * DEC(var-result.des-result).

                            END.

                            ASSIGN tt-pedido.saldo-var = (ped-item.qt-pedida * emitente.perc-fat-ped / 100) - (tt-pedido.qt-pedida - tt-pedido.saldo-var).
                             
                        END.
                    END.
                END.
            END.






            FOR EACH ped-campanha
               WHERE ped-campanha.nome-abrev = ped-item.nome-abrev
                 AND ped-campanha.it-codigo  = ped-item.it-codigo
                 AND ped-campanha.nr-pedcli  = ped-item.nr-pedcli 
                 and ped-campanha.nr-sequencia = ped-item.nr-sequencia /*Edson 13062012*/

                 NO-LOCK:

                IF tt-pedido.l-possui-prog = NO THEN

                    ASSIGN tt-pedido.dt-campanhas  = STRING(campanha-polo.dt-inicio)
                           tt-pedido.qt-campanhas  = STRING(ped-campanha.qt-pedida) + ' Kg'
                           tt-pedido.tot-campanhas = ped-campanha.qt-pedida
                           tt-pedido.op-campanhas  = STRING(ped-campanha.nr-ord-produ)
                           tt-pedido.qt-orig-ped   = ped-item.qt-pedida
                           tt-pedido.l-possui-prog = YES.
                ELSE

                    ASSIGN tt-pedido.dt-campanhas  = tt-pedido.dt-campanhas  + ' - ' + STRING(campanha-polo.dt-inicio)
                           tt-pedido.qt-campanhas  = tt-pedido.qt-campanhas  + ' - ' + STRING(ped-campanha.qt-pedida) + ' Kg'
                           tt-pedido.tot-campanhas = tt-pedido.tot-campanhas + ped-campanha.qt-pedida
                           tt-pedido.op-campanhas  = tt-pedido.op-campanhas  + ' - ' + STRING(ped-campanha.nr-ord-produ).
                
            END.
        END.
    END.

    FOR EACH  res-atp
        WHERE res-atp.it-codigo = campanha-polo.it-codigo NO-LOCK:

        /* Verifica se Pedido esta Associado a outra Campanha */
        FIND FIRST ped-campanha
             WHERE ped-campanha.nome-abrev = res-atp.nome-abrev AND
                   ped-campanha.it-codigo  = res-atp.it-codigo  AND 
                   ped-campanha.dt-entrega  = res-atp.dat-atp     
                    NO-LOCK NO-ERROR.

        IF  AVAIL ped-campanha THEN NEXT.

        CREATE tt-pedido.
        ASSIGN tt-pedido.it-codigo  = res-atp.it-codigo
               tt-pedido.nome-abrev = res-atp.nome-abrev
               tt-pedido.qt-pedida  = (res-atp.qtd-atp - res-atp.qtd-ped)
               tt-pedido.dt-entrega = res-atp.dat-atp
               tt-pedido.log-tipo   = 2
               tt-pedido.cod-refer  = res-atp.cod-refer
               tt-pedido.nr-sequencia = INT(res-atp.dat-atp).

    END. 

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-desabilita-botoes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE VARIABLE wh-panel-frame AS WIDGET-HANDLE NO-UNDO.
   DEFINE VARIABLE wh-bt-add      AS WIDGET-HANDLE NO-UNDO.
   DEFINE VARIABLE wh-bt-cop      AS WIDGET-HANDLE NO-UNDO.
   DEFINE VARIABLE wh-bt-mod      AS WIDGET-HANDLE NO-UNDO.
   DEFINE VARIABLE wh-bt-und      AS WIDGET-HANDLE NO-UNDO.
   DEFINE VARIABLE wh-bt-can      AS WIDGET-HANDLE NO-UNDO.
   DEFINE VARIABLE wh-bt-sav      AS WIDGET-HANDLE NO-UNDO.

   RUN pi-retorna-wh-objeto IN THIS-PROCEDURE (INPUT  FRAME f-cad:HANDLE,
                                               INPUT  "Panel-Frame":U,
                                               INPUT  2,
                                               OUTPUT wh-panel-frame).

   RUN pi-retorna-wh-objeto IN THIS-PROCEDURE (INPUT  wh-panel-frame,
                                               INPUT  "bt-add":U,
                                               INPUT  1,
                                               OUTPUT wh-bt-add).

   RUN pi-retorna-wh-objeto IN THIS-PROCEDURE (INPUT  wh-panel-frame,
                                               INPUT  "bt-cop":U,
                                               INPUT  1,
                                               OUTPUT wh-bt-cop).

   RUN pi-retorna-wh-objeto IN THIS-PROCEDURE (INPUT  wh-panel-frame,
                                               INPUT  "bt-mod":U,
                                               INPUT  1,
                                               OUTPUT wh-bt-mod).

   RUN pi-retorna-wh-objeto IN THIS-PROCEDURE (INPUT  wh-panel-frame,
                                               INPUT  "bt-und":U,
                                               INPUT  1,
                                               OUTPUT wh-bt-und).

   RUN pi-retorna-wh-objeto IN THIS-PROCEDURE (INPUT  wh-panel-frame,
                                               INPUT  "bt-can":U,
                                               INPUT  1,
                                               OUTPUT wh-bt-can).

   RUN pi-retorna-wh-objeto IN THIS-PROCEDURE (INPUT  wh-panel-frame,
                                               INPUT  "bt-sav":U,
                                               INPUT  1,
                                               OUTPUT wh-bt-sav).

   ASSIGN wh-bt-cop:VISIBLE = NO
          wh-bt-mod:VISIBLE = NO
          wh-bt-und:VISIBLE = NO
          wh-bt-add:COLUMN  = wh-bt-add:COLUMN + 8
          wh-bt-can:COLUMN  = wh-bt-can:COLUMN - 4
          wh-bt-sav:COLUMN  = wh-bt-sav:COLUMN - 4.

   RUN enable-copia    IN h_p-cadsim (INPUT NO).
   RUN enable-desfaz   IN h_p-cadsim (INPUT NO).
   RUN enable-modifica IN h_p-cadsim (INPUT NO).

   RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-habilita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAM p-habilita AS LOGICAL NO-UNDO.

    IF  p-habilita THEN
        ENABLE  bt-add bt-del bt-corte bt-manut bt-param br-pedido br-ped-campanha WITH FRAME f-cad.
    ELSE
        DISABLE bt-add bt-del bt-corte bt-manut bt-param br-pedido br-ped-campanha WITH FRAME f-cad.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-limpa-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH tt-pedido:
        DELETE tt-pedido.
    END.

    FOR EACH tt-ped-campanha:
        DELETE tt-ped-campanha.
    END.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-recalcula-tempos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER p-gm-codigo           LIKE campanha-polo.gm-codigo NO-UNDO.
    DEFINE INPUT PARAMETER p-prioridade-primeiro AS INTEGER              NO-UNDO.

    RUN sfc/essfapi0011.p (INPUT p-gm-codigo,
                                INPUT 1,
                                INPUT p-prioridade-primeiro,
                                INPUT NO,   /* Troca Prioridades           */
                                INPUT YES,  /* Recalculo Tempo             */
                                INPUT YES). /* Atualiza Primeira Campanha? */

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-retorna-wh-objeto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER p-grupo       AS WIDGET-HANDLE NO-UNDO.
    DEFINE INPUT  PARAMETER p-nome-objeto AS CHARACTER     NO-UNDO.
    DEFINE INPUT  PARAMETER p-nr-nome     AS INTEGER       NO-UNDO.
    DEFINE OUTPUT PARAMETER p-wh-objeto   AS WIDGET-HANDLE NO-UNDO.
    
    DEFINE VARIABLE wh-group   AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE wh-child   AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE i-contador AS INTEGER       NO-UNDO.
    
    ASSIGN wh-group = p-grupo:FIRST-CHILD.
    
    bloco-encontra-objeto:
    DO  WHILE wh-group <> ?:
        ASSIGN wh-child = wh-group:FIRST-CHILD.
        
        DO  WHILE wh-child <> ?:
            IF  wh-child:NAME = p-nome-objeto THEN DO:
                ASSIGN p-wh-objeto = wh-child
                       i-contador  = i-contador + 1.
                       
                IF  i-contador = p-nr-nome THEN LEAVE bloco-encontra-objeto.
            END.
            
            ASSIGN wh-child = wh-child:NEXT-SIBLING.
        END.
        ASSIGN wh-group = wh-group:NEXT-SIBLING.
    END.

    RETURN "OK":U.

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
    WHEN "tt-pedido":U THEN p-rowid-list = p-rowid-list + 
        IF AVAILABLE tt-pedido THEN STRING(ROWID(tt-pedido))
        ELSE "?":U.
   
 
  /* snd-list - 8/21/95 */
    WHEN "tt-ped-campanha":U THEN p-rowid-list = p-rowid-list + 
        IF AVAILABLE tt-ped-campanha THEN STRING(ROWID(tt-ped-campanha))
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




 

PROCEDURE pi-ver-dll.
    DEFINE VARIABLE arqDLL  AS CHARACTER   NO-UNDO.

arqDLL =  "Dformd.dll".
RUN pi-DLL (input arqDLL).
arqDLL =  "lindo2_0.dll".
RUN pi-DLL (input arqDLL).
arqDLL =  "Lindoau.dll".
RUN pi-DLL (input arqDLL).
arqDLL =  "Lindolm.dll".
RUN pi-DLL (input arqDLL).
arqDLL =  "Lingd80.dll".
RUN pi-DLL (input arqDLL).
arqDLL =  "Lingdb2.dll".
RUN pi-DLL (input arqDLL).
arqDLL =  "Lingf80.dll".
RUN pi-DLL (input arqDLL).
arqDLL =  "Lingfd80.dll".
RUN pi-DLL (input arqDLL).
arqDLL =  "Lingxl2.dll".
RUN pi-DLL (input arqDLL).
arqDLL =  "Mosek2_5.dll".
RUN pi-DLL (input arqDLL).
arqDLL =  "otimizador.dll".
RUN pi-DLL (input arqDLL).
arqDLL =  "Pthreadvse.dll".
RUN pi-DLL (input arqDLL).
END PROCEDURE.



PROCEDURE pi-DLL.
   
    DEF INPUT PARAM  arqDLL AS CHAR.
    DEFINE VARIABLE arqDLLori AS CHARACTER   NO-UNDO.

    arqDLLori = "x:\dll\" + arqDLL.
    arqDLLori =  SEARCH(arqDLLori).

    arqDLL = SESSION:TEMP-DIRECTORY + arqDLL.
    arqDLL = SEARCH(arqDLL).
    
 
    IF arqDLL = ? AND arqDLLori <> ? THEN
  OS-COPY  value(arqDLLori) value(SESSION:TEMP-DIRECTORY).

END PROCEDURE.




