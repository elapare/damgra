


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
def var c-prg-vrs as char init "[[[2.00.00.002[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.002"
       c-prg-obj = "essf0009A".
if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "essf0009A"
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
    put "essf0009A" at 1 "2.00.00.002" at 69 today at 84 string(time,'HH:MM:SS':U) at 94 skip.
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

 
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---          
                             */

def var h-janela as handle no-undo.

DEF NEW GLOBAL SHARED VAR gr-essf0009 AS ROWID NO-UNDO.
DEF VAR c-data         AS CHAR NO-UNDO.
DEF VAR da-data        AS DATE NO-UNDO.
DEF VAR i-mercado      AS INTEGER NO-UNDO.
DEF VAR de-saldo       LIKE saldo-estoq.qtidade-atu.
DEF VAR l-erro         AS LOGICAL INITIAL NO NO-UNDO.

DEF TEMP-TABLE tt-parametro
    FIELD cod-estabel    LIKE estabelec.cod-estabel
    FIELD nr-pedido      LIKE ped-venda.nr-pedido
    FIELD sequencia      LIKE ped-item.nr-sequencia
    FIELD qt-pedida      LIKE ped-item.qt-pedida
    FIELD it-codigo      LIKE ped-item.it-codigo
    FIELD largura        AS DEC FORMAT '>>>>>>,>>9.9999'
    FIELD diamin         AS DEC FORMAT '>>>>>>,>>9.9999'
    FIELD diamex         AS DEC FORMAT '>>>>>>,>>9.9999'
    FIELD qt-paletizada  LIKE pallet.peso-liquido
    FIELD cod-refer      LIKE ped-item.cod-refer.

DEFINE OUTPUT PARAMETER TABLE FOR tt-parametro.


DEF OUTPUT PARAMETER VarLargMais   as DECIMAL NO-UNDO.
DEF OUTPUT PARAMETER VarLargMenos  as decimal NO-UNDO.
DEF OUTPUT PARAMETER VarDiinMais   as decimal NO-UNDO.
DEF OUTPUT PARAMETER VarDiinMenos  as decimal NO-UNDO.
DEF OUTPUT PARAMETER VarDiexMais   as decimal NO-UNDO.
DEF OUTPUT PARAMETER VarDiexMenos  as DECIMAL NO-UNDO.
DEF OUTPUT PARAMETER p-recorte     AS LOGICAL NO-UNDO.
DEF OUTPUT PARAMETER p-codDeposIni AS CHAR    NO-UNDO.
DEF OUTPUT PARAMETER p-codDeposFim AS CHAR    NO-UNDO.
DEF OUTPUT PARAMETER pLarguraIni   AS DEC     NO-UNDO.
DEF OUTPUT PARAMETER pLarguraFim   AS DEC     NO-UNDO.
DEF OUTPUT PARAMETER pDiaminIni    AS DEC     NO-UNDO.
DEF OUTPUT PARAMETER pDiaminfim    AS DEC     NO-UNDO.
DEF OUTPUT PARAMETER pDiamexIni    AS DEC     NO-UNDO.
DEF OUTPUT PARAMETER pDiamexFim    AS DEC     NO-UNDO.
DEF OUTPUT PARAMETER p-acao        AS INT INIT 1 NO-UNDO.
DEF OUTPUT PARAMETER p-item        AS char    NO-UNDO.

DEF NEW GLOBAL SHARED VAR p-cod-estabel LIKE estabelec.cod-estabel NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-cod-item    LIKE ITEM.it-codigo        NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-depos-ini   AS   CHAR  INITIAL "EXP"   NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-depos-fim   AS   CHAR  INITIAL "EXP"   NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-pedido-ini  LIKE ped-venda.nr-pedido   NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-pedido-fim  LIKE ped-venda.nr-pedido   INITIAL 999999  NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-nome-ini    LIKE ped-venda.nome-abrev  NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-nome-fim    LIKE ped-venda.nome-abrev INITIAL "ZZZZZZZZZZZZ"  NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-tipo-ini    LIKE ped-venda.tp-pedido   NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-tipo-fim    LIKE ped-venda.tp-pedido  INITIAL "ZZ" NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-data-ini    LIKE ped-item.dt-entrega   NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-data-fim    LIKE ped-item.dt-entrega   NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-mercado     AS INT INITIAL 1           NO-UNDO.
DEF NEW GLOBAL SHARED VAR i-filtro      AS INT INITIAL 1           NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-largura-ini AS DEC                     NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-largura-fim AS DEC INITIAL 999         NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-Diamin-Ini  AS DEC                     NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-Diamin-fim  AS DEC INITIAL 999         NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-Diamex-ini  AS DEC                     NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-Diamex-fim  AS DEC INITIAL 999         NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-Larg-Mais   as DECIMAL INITIAL 999     NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-Larg-Menos  as decimal                 NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-Diin-Mais   as decimal INITIAL 999     NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-Diin-Menos  as decimal                 NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-Diex-Mais   as decimal INITIAL 999     NO-UNDO.
DEF NEW GLOBAL SHARED VAR p-Diex-Menos  as DECIMAL                 NO-UNDO.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* Name of first Frame and/or Browse and/or first Query                 */

/* Standard List Definitions                                            */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */




/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-item 
     IMAGE-UP FILE "adeicon/userflds.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE c-cod-depos-fim AS CHARACTER FORMAT "X(03)":U INITIAL "EXP" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-depos-ini AS CHARACTER FORMAT "X(03)":U INITIAL "EXP" 
     LABEL "Dep¢sito" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-estabel AS CHARACTER FORMAT "X(03)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-estab AS CHARACTER FORMAT "x(40)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-item AS CHARACTER FORMAT "x(40)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY .88 NO-UNDO.

DEFINE VARIABLE c-emitente-fim AS CHARACTER FORMAT "X(12)":U INITIAL "ZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-emitente-ini AS CHARACTER FORMAT "X(12)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE de-diam-ext AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "DiÉm. Ext" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE de-diam-ext-estoq AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "DiÉm. Ext" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE de-diam-ext-estoq-fim AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE de-diam-ext-fim AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE de-diam-int AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "DiÉm. Int" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE de-diam-int-estoq AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "DiÉm. Int" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE de-diam-int-estoq-fim AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE de-diam-int-fim AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE de-largura AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Largura" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE de-largura-estoq AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Largura" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE de-largura-estoq-fim AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE de-largura-fim AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE dt-entrega-fim AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE dt-entrega-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data de Entrega" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE nr-pedido-fim AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE nr-pedido-ini AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "N£mero Pedido" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE tp-pedido-fim AS CHARACTER FORMAT "X(02)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE tp-pedido-ini AS CHARACTER FORMAT "X(02)":U INITIAL "AAA" 
     LABEL "Tipo Pedido" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image/im-cllps.bmp":U
     SIZE 3 BY .67.

DEFINE IMAGE IMAGE-16
     FILENAME "image/im-expan.bmp":U
     SIZE 3 BY .67.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-filtro AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Transferància", 1,
"Recorte", 2
     SIZE 33.57 BY .54 NO-UNDO.

DEFINE VARIABLE rs-mercado AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Interno", 1,
"Externo", 2,
"Ambos", 3
     SIZE 28.86 BY .54 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 68 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 66.86 BY 1.79.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 67 BY 1.79.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 34 BY 4.75.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 34.29 BY 4.75.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 18.54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     c-cod-estabel AT ROW 1.17 COL 16 COLON-ALIGNED HELP
          "Estabelecimento"
     c-desc-estab AT ROW 1.17 COL 20 COLON-ALIGNED NO-LABEL
     c-it-codigo AT ROW 2.17 COL 16 COLON-ALIGNED HELP
          "Item"
     c-desc-item AT ROW 2.17 COL 32.14 COLON-ALIGNED NO-LABEL
     c-cod-depos-fim AT ROW 3.13 COL 49 COLON-ALIGNED HELP
          "Dep¢sito Final" NO-LABEL
     c-cod-depos-ini AT ROW 3.17 COL 16 COLON-ALIGNED HELP
          "Dep¢sito Inicial"
     nr-pedido-ini AT ROW 4.17 COL 16 COLON-ALIGNED HELP
          "N£mero Pedido Inicial"
     nr-pedido-fim AT ROW 4.17 COL 49 COLON-ALIGNED HELP
          "N£mero Pedido Final" NO-LABEL
     c-emitente-ini AT ROW 5.17 COL 16 COLON-ALIGNED HELP
          "Cliente Inicial"
     c-emitente-fim AT ROW 5.17 COL 49 COLON-ALIGNED HELP
          "Cliente Final" NO-LABEL
     tp-pedido-ini AT ROW 6.17 COL 16.14 COLON-ALIGNED HELP
          "N£mero Pedido Inicial"
     tp-pedido-fim AT ROW 6.17 COL 53 RIGHT-ALIGNED HELP
          "N£mero Pedido Final" NO-LABEL
     dt-entrega-ini AT ROW 7.17 COL 16.14 COLON-ALIGNED HELP
          "Data de Entrega Inicial"
     dt-entrega-fim AT ROW 7.17 COL 49 COLON-ALIGNED HELP
          "Date de Entrega Final" NO-LABEL
     rs-mercado AT ROW 9.17 COL 18.14 HELP
          "Mercado" NO-LABEL
     bt-item AT ROW 11.21 COL 53
     rs-filtro AT ROW 11.46 COL 18.43 HELP
          "Filtro" NO-LABEL
     de-largura AT ROW 14.33 COL 10.72 COLON-ALIGNED
     de-largura-fim AT ROW 14.33 COL 21.86 COLON-ALIGNED NO-LABEL
     de-diam-ext AT ROW 15.33 COL 10.72 COLON-ALIGNED
     de-diam-ext-fim AT ROW 15.33 COL 21.86 COLON-ALIGNED NO-LABEL
     de-diam-int AT ROW 16.33 COL 10.72 COLON-ALIGNED
     de-diam-int-fim AT ROW 16.38 COL 21.86 COLON-ALIGNED NO-LABEL
     de-largura-estoq AT ROW 14.33 COL 44.72 COLON-ALIGNED
     de-largura-estoq-fim AT ROW 14.33 COL 56.29 COLON-ALIGNED NO-LABEL
     de-diam-ext-estoq AT ROW 15.33 COL 44.72 COLON-ALIGNED
     de-diam-ext-estoq-fim AT ROW 15.33 COL 56.29 COLON-ALIGNED NO-LABEL
     de-diam-int-estoq AT ROW 16.33 COL 44.72 COLON-ALIGNED
     de-diam-int-estoq-fim AT ROW 16.33 COL 56.29 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 18.21 COL 2.72
     bt-cancelar AT ROW 18.21 COL 13.72
     bt-ajuda AT ROW 18.25 COL 56
     IMAGE-1 AT ROW 3.13 COL 32.29
     IMAGE-11 AT ROW 7.17 COL 32.29
     IMAGE-12 AT ROW 7.17 COL 46.14
     IMAGE-15 AT ROW 13.33 COL 49.43
     IMAGE-16 AT ROW 13.33 COL 59.86
     IMAGE-2 AT ROW 3.13 COL 46.14
     IMAGE-3 AT ROW 4.17 COL 32.29
     IMAGE-4 AT ROW 4.17 COL 46.14
     IMAGE-5 AT ROW 5.13 COL 32.29
     IMAGE-6 AT ROW 5.13 COL 46.14
     IMAGE-7 AT ROW 6.13 COL 32.29
     IMAGE-8 AT ROW 6.13 COL 46.14
     RECT-1 AT ROW 18 COL 1.72
     RECT-14 AT ROW 8.58 COL 2
     RECT-15 AT ROW 10.92 COL 2
     RECT-16 AT ROW 12.92 COL 2
     RECT-17 AT ROW 12.92 COL 36.29
     RECT-23 AT ROW 1 COL 1
     "De" VIEW-AS TEXT
          SIZE 4.14 BY .63 AT ROW 13.33 COL 13.72
     "Para" VIEW-AS TEXT
          SIZE 5 BY .67 AT ROW 13.33 COL 24
     "Faixa Variaá∆o Estoque" VIEW-AS TEXT
          SIZE 22 BY .63 AT ROW 12.71 COL 38.43
     "Mercado" VIEW-AS TEXT
          SIZE 8.14 BY .67 AT ROW 8.25 COL 3.86
     "Filtrar para" VIEW-AS TEXT
          SIZE 10.14 BY .67 AT ROW 10.67 COL 3.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 70.72 BY 19.04.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "Carac. Pedido" VIEW-AS TEXT
          SIZE 13.29 BY .63 AT ROW 12.71 COL 3.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 70.72 BY 19.04.


/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */


/* *************************  Create Window  ************************** */


IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "ParÉmetros - essf0009A"
         HEIGHT             = 19.17
         WIDTH              = 70.72
         MAX-HEIGHT         = 21.92
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.92
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE w-window = CURRENT-WINDOW.
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


  
    ASSIGN adm-object-hdl    =   w-window.
  


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
     'JanelaDetalhe~`':U +      /* Type attribute */
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
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "DESTROY":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame F-Main:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "DESTROY":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame F-Main:handle,
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
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "BEFORE-DISABLE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame F-Main:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "BEFORE-DISABLE", 
                                    input "CONTAINER",
                                    input this-procedure,
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    

end.                                       
/* I-EPC009.I */

 
    

    
    DISABLE c-cod-estabel c-it-codigo c-cod-depos-fim c-cod-depos-ini nr-pedido-ini nr-pedido-fim c-emitente-ini c-emitente-fim tp-pedido-ini tp-pedido-fim dt-entrega-ini dt-entrega-fim rs-mercado bt-item rs-filtro de-largura de-largura-fim de-diam-ext de-diam-ext-fim de-diam-int de-diam-int-fim de-largura-estoq de-largura-estoq-fim de-diam-ext-estoq de-diam-ext-estoq-fim de-diam-int-estoq de-diam-int-estoq-fim bt-ok bt-cancelar bt-ajuda IMAGE-1 IMAGE-11 IMAGE-12 IMAGE-15 IMAGE-16 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 RECT-1 RECT-14 RECT-15 RECT-16 RECT-17 RECT-23 WITH FRAME F-Main.
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
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "DISABLE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame F-Main:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "DISABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame F-Main:handle,
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
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "BEFORE-ENABLE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame F-Main:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "BEFORE-ENABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    

end.                                       
/* I-EPC008 */

 
   

   
    ENABLE UNLESS-HIDDEN c-cod-estabel c-it-codigo c-cod-depos-fim c-cod-depos-ini nr-pedido-ini nr-pedido-fim c-emitente-ini c-emitente-fim tp-pedido-ini tp-pedido-fim dt-entrega-ini dt-entrega-fim rs-mercado bt-item rs-filtro de-largura de-largura-fim de-diam-ext de-diam-ext-fim de-diam-int de-diam-int-fim de-largura-estoq de-largura-estoq-fim de-diam-ext-estoq de-diam-ext-estoq-fim de-diam-int-estoq de-diam-int-estoq-fim bt-ok bt-cancelar bt-ajuda IMAGE-1 IMAGE-11 IMAGE-12 IMAGE-15 IMAGE-16 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 RECT-1 RECT-14 RECT-15 RECT-16 RECT-17 RECT-23 WITH FRAME F-Main.

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
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "ENABLE":U, 
                                     input "CONTAINER",
                                     input this-procedure,
                                     input frame F-Main:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "ENABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame F-Main:handle,
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
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "BEFORE-INITIALIZE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame F-Main:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "BEFORE-INITIALIZE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame F-Main:handle,
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
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "INITIALIZE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame F-Main:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "INITIALIZE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame F-Main:handle,
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
                                            input frame F-Main:handle  ,
                                            input "",
            
                                            input ?).
            
        end.
        
         /* APPC */
        if  c-nom-prog-appc-mg97 <> "" and
            c-nom-prog-appc-mg97 <> ? then do:           
            run value(c-nom-prog-appc-mg97)  (input "CHANGE-PAGE":U, 
                                             input "CONTAINER":U,
                                             input h-ctrl-tab,
                                             input frame F-Main:handle  ,
                                             input "",
            
                                             input ?).
            
        end.
         /* UPC */
        if  c-nom-prog-upc-mg97 <> "" and
            c-nom-prog-upc-mg97 <> ? then do:                  
            run value(c-nom-prog-upc-mg97) (input "CHANGE-PAGE":U, 
                                            input "CONTAINER":U,
                                            input h-ctrl-tab,
                                            input  frame F-Main:handle,
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
                                            input frame F-Main:handle  ,
                                            input "",
            
                                            input ?).
            
        end.
        
         /* APPC */
        if  c-nom-prog-appc-mg97 <> "" and
            c-nom-prog-appc-mg97 <> ? then do:           
            run value(c-nom-prog-appc-mg97)  (input "AFTER-CHANGE-PAGE":U, 
                                             input "CONTAINER":U,
                                             input h-ctrl-tab,
                                             input frame F-Main:handle  ,
                                             input "",
            
                                             input ?).
            
        end.
         /* UPC */
        if  c-nom-prog-upc-mg97 <> "" and
            c-nom-prog-upc-mg97 <> ? then do:                  
            run value(c-nom-prog-upc-mg97) (input "AFTER-CHANGE-PAGE":U, 
                                            input "CONTAINER":U,
                                            input h-ctrl-tab,
                                            input  frame F-Main:handle,
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




 



/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define variable wh-pesquisa             as handle               no-undo.
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
         HEIGHT             = 1.83
         WIDTH              = 40.
 /* END WINDOW DEFINITION */
                                                                        */

 

/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */

/* **********************  Internal Procedures  *********************** */


PROCEDURE pi-after-initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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


PROCEDURE pi-enter-go :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    apply 'CHOOSE':U to bt-ok in frame F-Main.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */


PROCEDURE pi-entry :
/*------------------------------------------------------------------------------
  Purpose:     Trata as mudanªas de estado (State-Changed)
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    RUN new-state in THIS-PROCEDURE ("apply-entry":u).
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */


PROCEDURE pi-trata-state :
/*------------------------------------------------------------------------------
  Purpose:     Trata as mudanªas de estado (State-Changed)
  Parameters:  INPUT Handle da procedure pai
               INPUT CΩdigo do Estado
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl   AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state        AS CHAR   NO-UNDO.
  
  case entry(1, p-state, '|'):
  end.
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


/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   Custom                                                               */
/* SETTINGS FOR FILL-IN c-desc-estab IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-item IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tp-pedido-fim IN FRAME F-Main
   ALIGN-R                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */


 



/* ************************  Control Triggers  ************************ */


ON END-ERROR OF w-window /* ParÉmetros - essf0009A */
OR ENDKEY OF w-window ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON WINDOW-CLOSE OF w-window /* ParÉmetros - essf0009A */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME F-Main
DO:
  /*************************************************************************
**
** AJUDA.I - Include padr∆o para chamada do Help
**
**************************************************************************/


    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).


RETURN NO-APPLY.

/* include/ajuda.i */
 
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:

  ASSIGN p-acao = 2.

  APPLY "choose" TO bt-ok.
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-item IN FRAME F-Main /* Button 1 */
DO:
  
    FIND item-uni-estab
        WHERE item-uni-estab.it-codigo   = INPUT FRAME F-Main c-it-codigo
        AND   item-uni-estab.cod-estabel = INPUT FRAME F-Main c-cod-estabel
        NO-LOCK NO-ERROR.
    IF AVAIL item-uni-estab THEN 
        ASSIGN gr-essf0009 = ROWID(item-uni-estab).
    ELSE
        ASSIGN gr-essf0009 = ?.

    RUN sfc/essf0010.w.

END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  ASSIGN p-acao = 1.

  ASSIGN l-erro = NO.
  RUN pi-valida.

  /*** Recupera parÉmetros ***/
  ASSIGN p-cod-estabel   = INPUT FRAME F-Main c-cod-estabel
         p-cod-item      = INPUT FRAME F-Main c-it-codigo
         p-depos-ini     = INPUT FRAME F-Main c-cod-depos-ini
         p-depos-fim     = INPUT FRAME F-Main c-cod-depos-fim
         p-pedido-ini    = INPUT FRAME F-Main nr-pedido-ini
         p-pedido-fim    = INPUT FRAME F-Main nr-pedido-fim
         p-nome-ini      = INPUT FRAME F-Main c-emitente-ini
         p-nome-fim      = INPUT FRAME F-Main c-emitente-fim
         p-tipo-ini      = INPUT FRAME F-Main tp-pedido-ini
         p-tipo-fim      = INPUT FRAME F-Main tp-pedido-fim
         p-mercado       = INPUT FRAME F-Main rs-mercado
         i-filtro        = INPUT FRAME F-Main rs-filtro
         p-largura-ini   = INPUT FRAME F-Main de-largura
         p-largura-fim   = INPUT FRAME F-Main de-largura-fim
         p-Diamin-Ini    = INPUT FRAME F-Main de-diam-int
         p-Diamin-fim    = INPUT FRAME F-Main de-diam-int-fim
         p-Diamex-ini    = INPUT FRAME F-Main de-diam-ext
         p-Diamex-fim    = INPUT FRAME F-Main de-diam-ext-fim
         p-Larg-Mais     = INPUT FRAME F-Main de-largura-estoq-fim
         p-Larg-Menos    = INPUT FRAME F-Main de-largura-estoq
         p-Diin-Mais     = INPUT FRAME F-Main de-diam-int-estoq-fim
         p-Diin-Menos    = INPUT FRAME F-Main de-diam-int-estoq
         p-Diex-Mais     = INPUT FRAME F-Main de-diam-ext-estoq-fim
         p-Diex-Menos    = INPUT FRAME F-Main de-diam-ext-estoq.

  IF l-erro = NO THEN DO:
      RUN pi-cria-parametro.
      ASSIGN VarLargMais     = INPUT FRAME F-Main de-largura-estoq-fim
             VarLargMenos    = INPUT FRAME F-Main de-largura-estoq
             VarDiinMais     = INPUT FRAME F-Main de-diam-int-estoq-fim
             VarDiinMenos    = INPUT FRAME F-Main de-diam-int-estoq
             VarDiexMais     = INPUT FRAME F-Main de-diam-ext-estoq-fim
             VarDiexMenos    = INPUT FRAME F-Main de-diam-ext-estoq
             p-codDeposIni   = INPUT FRAME F-Main c-cod-depos-ini
             p-codDeposFim   = INPUT FRAME F-Main c-cod-depos-fim 
             pLarguraIni     = INPUT FRAME F-Main de-largura
             pLarguraFim     = INPUT FRAME F-Main de-largura-fim
             pDiaminIni      = INPUT FRAME F-Main de-diam-int
             pDiaminfim      = INPUT FRAME F-Main de-diam-int-fim
             pDiamexIni      = INPUT FRAME F-Main de-diam-ext
             pDiamexFim      = INPUT FRAME F-Main de-diam-ext-fim
             p-item          = INPUT FRAME F-Main c-it-codigo.

      IF INPUT FRAME F-Main rs-filtro = 1 THEN
          ASSIGN p-recorte = NO.
      ELSE
          ASSIGN p-recorte = YES.

      apply "close":U to this-procedure.
  END.
END.

/* _UIB-CODE-BLOCK-END */




ON F5 OF c-cod-estabel IN FRAME F-Main /* Estabelecimento */
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
        
        assign c-lista-campo = string(c-cod-estabel:handle in frame F-Main) + '|':U + 'cod-estabel'.
        
            assign c-lista-campo = c-lista-campo + chr(10) + string(c-desc-estab:handle in frame F-Main) + '|':U + 'nome'.
        
        
        
        
        
        
        
        
        
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




ON LEAVE OF c-cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  
    


/*--------------------------------------------------------------------------
    File        : leave.i
    Purpose     : Executar o find e display para chaves estrangeiras no
                  evento de leave do atributos

    Syntax      : {include/leave.i &tabela=NOME DA TABELA A PESQUISAR
                                   &atributo-ref=ATRIBUTO DA TABELA USADO COMO REFERENCIA
                                   &variavel-ref=VARIAVEL USADA PARA MOSTRAR A REFERENCIA
                                   &where=CLAUSULA USADA PARA O FIND}
    Description : Colocar no evento de leave dos atributos que s∆o chave
                  estrangeira para outras tabelas

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
                                                                        */

 






/* ***************************  Main Block  *************************** */

find estabelec
    where estabelec.cod-estabel = input frame F-Main c-cod-estabel no-lock no-error.
if  avail estabelec then do:
    assign c-desc-estab:screen-value in frame F-Main = string(estabelec.nome).


end.
else do:
    assign c-desc-estab:screen-value in frame F-Main = "".


end.

/* _UIB-CODE-BLOCK-END */



 


END.

/* _UIB-CODE-BLOCK-END */




ON MOUSE-SELECT-DBLCLICK OF c-cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */




ON F5 OF c-it-codigo IN FRAME F-Main /* Item */
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
        
        assign c-lista-campo = string(c-it-codigo:handle in frame F-Main) + '|':U + 'it-codigo'.
        
            assign c-lista-campo = c-lista-campo + chr(10) + string(c-desc-item:handle in frame F-Main) + '|':U + 'c-descricao'.
        
        
        
        
        
        
        
        
        
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




ON LEAVE OF c-it-codigo IN FRAME F-Main /* Item */
DO:
    


/*--------------------------------------------------------------------------
    File        : leave.i
    Purpose     : Executar o find e display para chaves estrangeiras no
                  evento de leave do atributos

    Syntax      : {include/leave.i &tabela=NOME DA TABELA A PESQUISAR
                                   &atributo-ref=ATRIBUTO DA TABELA USADO COMO REFERENCIA
                                   &variavel-ref=VARIAVEL USADA PARA MOSTRAR A REFERENCIA
                                   &where=CLAUSULA USADA PARA O FIND}
    Description : Colocar no evento de leave dos atributos que s∆o chave
                  estrangeira para outras tabelas

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
                                                                        */

 






/* ***************************  Main Block  *************************** */

find ITEM
    where item.it-codigo = input frame F-Main c-it-codigo no-lock no-error.
if  avail ITEM then do:
    assign c-desc-item:screen-value in frame F-Main = string(ITEM.desc-item).


end.
else do:
    assign c-desc-item:screen-value in frame F-Main = "".


end.

/* _UIB-CODE-BLOCK-END */



 

END.

/* _UIB-CODE-BLOCK-END */




ON MOUSE-SELECT-DBLCLICK OF c-it-codigo IN FRAME F-Main /* Item */
DO:
  APPLY "f5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */




ON VALUE-CHANGED OF rs-filtro IN FRAME F-Main
DO:
    IF INPUT FRAME F-Main rs-filtro = 2 THEN
      ENABLE bt-item WITH FRAME F-Main.
    ELSE
      DISABLE bt-item WITH FRAME F-Main.

END.

/* _UIB-CODE-BLOCK-END */




ON VALUE-CHANGED OF rs-mercado IN FRAME F-Main
DO:
    
    ASSIGN i-mercado = INPUT FRAME F-Main rs-mercado.

END.

/* _UIB-CODE-BLOCK-END */







/* ***************************  Main Block  *************************** */

ASSIGN c-data         = STRING(TODAY,"99/99/9999")
       dt-entrega-ini = DATE("01" + SUBSTRING(c-data,3,8))
       da-data        = dt-entrega-ini + 31
       c-data         = STRING(da-data,"99/99/9999")
       dt-entrega-fim = DATE("01" + SUBSTRING(c-data,3,8)) - 1.

IF  c-cod-estabel:LOAD-MOUSE-POINTER ("image\lupa.cur") IN FRAME F-Main THEN.
IF  c-it-codigo:LOAD-MOUSE-POINTER ("image\lupa.cur") IN FRAME F-Main THEN.


/* Include custom  Main Block code for SmartWindows. */
/* windowmn.i - Main Block code for objects which create windows.*/
/* Skip all of this if no window was created. */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
IF VALID-HANDLE(w-window) THEN DO:
    ASSIGN CURRENT-WINDOW                = w-window 
       w-window:KEEP-FRAME-Z-ORDER = YES
       THIS-PROCEDURE:CURRENT-WINDOW = w-window.

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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
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
  DISPLAY c-cod-estabel c-desc-estab c-it-codigo c-desc-item c-cod-depos-fim 
          c-cod-depos-ini nr-pedido-ini nr-pedido-fim c-emitente-ini 
          c-emitente-fim tp-pedido-ini tp-pedido-fim dt-entrega-ini 
          dt-entrega-fim rs-mercado rs-filtro de-largura de-largura-fim 
          de-diam-ext de-diam-ext-fim de-diam-int de-diam-int-fim 
          de-largura-estoq de-largura-estoq-fim de-diam-ext-estoq 
          de-diam-ext-estoq-fim de-diam-int-estoq de-diam-int-estoq-fim 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE c-cod-estabel c-it-codigo c-cod-depos-fim c-cod-depos-ini 
         nr-pedido-ini nr-pedido-fim c-emitente-ini c-emitente-fim 
         tp-pedido-ini tp-pedido-fim dt-entrega-ini dt-entrega-fim rs-mercado 
         bt-item rs-filtro de-largura de-largura-fim de-diam-ext 
         de-diam-ext-fim de-diam-int de-diam-int-fim de-largura-estoq 
         de-largura-estoq-fim de-diam-ext-estoq de-diam-ext-estoq-fim 
         de-diam-int-estoq de-diam-int-estoq-fim bt-ok bt-cancelar bt-ajuda 
         IMAGE-1 IMAGE-11 IMAGE-12 IMAGE-15 IMAGE-16 IMAGE-2 IMAGE-3 IMAGE-4 
         IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 RECT-1 RECT-14 RECT-15 RECT-16 RECT-17 
         RECT-23 
      WITH FRAME F-Main IN WINDOW w-window.
  
  VIEW w-window.
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



PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .



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

if w-window:width-chars < frame F-Main:width-chars then
    assign frame F-Main:width-chars = w-window:width-chars.
else if frame F-Main:width-chars < w-window:width-chars then
    assign w-window:width-chars = frame F-Main:width-chars.

if w-window:height-chars < frame F-Main:height-chars then
    assign frame F-Main:height-chars = w-window:height-chars.
else if frame F-Main:height-chars < w-window:height-chars then
    assign w-window:height-chars = frame F-Main:height-chars.

assign w-window:virtual-width-chars  = w-window:width-chars
       w-window:virtual-height-chars = w-window:height-chars
       w-window:min-width-chars      = w-window:width-chars
       w-window:max-width-chars      = w-window:width-chars
       w-window:min-height-chars     = w-window:height-chars
       w-window:max-height-chars     = w-window:height-chars.

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

    assign c-programa-mg97 = caps("essf0009A")
           c-versao-mg97   = "2.00.00.002".
    find prog_dtsul no-lock
        where prog_dtsul.cod_prog_dtsul = c-programa-mg97 no-error.
    if  avail prog_dtsul then do:
        assign c-titulo-prog-mg97    = prog_dtsul.des_prog_dtsul
               c-nom-prog-upc-mg97   = prog_dtsul.nom_prog_upc
               c-nom-prog-appc-mg97  = prog_dtsul.nom_prog_appc
               c-nom-prog-dpc-mg97   = prog_dtsul.nom_prog_dpc
               i-num-topico-hlp-mg97 = prog_dtsul.num_topico.
       
/*          if session:window-system <> "TTY" then 
            assign i-template          = prog_dtsul.ind_template.*/
       

        
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
     
    
         assign w-window:title = if l-achou-prog then
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
      if "JanelaDetalhe" = "SmartDialog" or this-procedure:persistent = no then
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
      if "JanelaDetalhe" = "SmartDialog" then
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
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.
        end.
        when 3 then do: /* Cadastro Simples - Alteracao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.
        end.
        when 4 then do: /* Cadastro Complexo */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 5 then do: /* Cadastro Complexo - Alteracao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 6 then do: /* Cadastro Simples - Inclusao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 7 then do: /* Cadastro PaiXFilho - Atualiza Filho */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 8 then do: /* Cadastro PaiXFilho - Atualiza Ambos */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
            
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 9 then do: /* Inclui/Modifica Pai */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 10 then do: /* Inclui/Modifica Filho */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 11 then do: /* Formacao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 12 then do: /* Parametros Unicos */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 13 then do: /* Pesquisa */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 14 then do: /* Consulta Simples */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 15 then do: /* Consulta Complexa */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 16 then do: /* Consulta Relacionamento */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 17 then do: /* Relatorio */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 20 then do: /* Janela Detalhe */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD.

            if  h-pai:handle = w-window:handle then
                assign h-pai           = h-pai:NEXT-SIBLING
                       h-pai:SENSITIVE = no.
            else 
                assign  h-pai = w-window:handle
                        h-pai = h-pai:parent.

            h-pai:sensitive = no.
  
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 21 then do: /* Janela Mestre */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 26 then do: /* Importacao/Exportacao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.
        end.
        when 29 then do: /* Relatorio Gerado Pelo DataViewer */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.
        end.
        when 30 then do: /* Formacao Sem Navegacao */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.
        end.
        when 31 then do: /* Estrutura */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.
        end.
        when 32 then do: /* Digitaá∆o Rapida */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
            
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.
        end.
    end case.

/* Transformacao Window *****************************************************/

 
      end. 
    



/* ut9000.i */
 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  FIND estabelec
      WHERE estabelec.cod-estabel = '412' OR estabelec.cod-estabel = '422'
      NO-LOCK NO-ERROR.
  IF AVAIL estabelec THEN
      ASSIGN c-cod-estabel:SCREEN-VALUE IN FRAME F-Main = estabelec.cod-estabel
             c-desc-estab:SCREEN-VALUE IN FRAME F-Main  = estabelec.nome.

  FIND ITEM 
      WHERE ITEM.it-codigo = INPUT FRAME F-Main c-it-codigo
      NO-LOCK NO-ERROR.
  IF AVAIL ITEM THEN
      ASSIGN c-desc-item:SCREEN-VALUE IN FRAME F-Main = ITEM.desc-item.
  ELSE 
      ASSIGN c-desc-item:SCREEN-VALUE IN FRAME F-Main = "".

  /*IF INPUT FRAME {&FRAME-NAME} rs-filtro = 1 THEN
      ASSIGN bt-item:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  ELSE
      ASSIGN bt-item:SENSITIVE IN FRAME {&FRAME-NAME} = YES. */


  IF i-filtro = 1 THEN
      ASSIGN bt-item:SENSITIVE IN FRAME F-Main = NO.
  ELSE
      ASSIGN bt-item:SENSITIVE IN FRAME F-Main = YES.



  ASSIGN c-it-codigo:SCREEN-VALUE IN FRAME F-Main     =  p-cod-item
         c-cod-depos-ini:SCREEN-VALUE IN FRAME F-Main =  p-depos-ini
         c-cod-depos-fim:SCREEN-VALUE IN FRAME F-Main =  p-depos-fim
         nr-pedido-ini:SCREEN-VALUE IN FRAME F-Main   =  string(p-pedido-ini)
         nr-pedido-fim:SCREEN-VALUE IN FRAME F-Main   =  string(p-pedido-fim)
         c-emitente-ini:SCREEN-VALUE IN FRAME F-Main  =  p-nome-ini
         c-emitente-fim:SCREEN-VALUE IN FRAME F-Main  =  p-nome-fim
         tp-pedido-ini:SCREEN-VALUE IN FRAME F-Main   =  string(p-tipo-ini)
         tp-pedido-fim:SCREEN-VALUE IN FRAME F-Main   =  string(p-tipo-fim)
         rs-mercado:SCREEN-VALUE IN FRAME F-Main      =  string(p-mercado)
         rs-filtro:SCREEN-VALUE IN FRAME F-Main       =  string(i-filtro) 
         de-largura:SCREEN-VALUE IN FRAME F-Main      =  string(p-largura-ini) 
         de-largura-fim:SCREEN-VALUE IN FRAME F-Main  =  string(p-largura-fim) 
         de-diam-int:SCREEN-VALUE IN FRAME F-Main     =  string(p-Diamin-Ini) 
         de-diam-int-fim:SCREEN-VALUE IN FRAME F-Main =  string(p-Diamin-fim) 
         de-diam-ext:SCREEN-VALUE IN FRAME F-Main     =  string(p-Diamex-ini) 
         de-diam-ext-fim:SCREEN-VALUE IN FRAME F-Main =  string(p-Diamex-fim) 
         de-largura-estoq-fim:SCREEN-VALUE IN FRAME F-Main = string(p-Larg-Mais) 
         de-largura-estoq:SCREEN-VALUE IN FRAME F-Main  = string(p-Larg-Menos) 
         de-diam-int-estoq-fim:SCREEN-VALUE IN FRAME F-Main = string(p-Diin-Mais) 
         de-diam-int-estoq:SCREEN-VALUE IN FRAME F-Main = string(p-Diin-Menos) 
         de-diam-ext-estoq-fim:SCREEN-VALUE IN FRAME F-Main = string(p-Diex-Mais) 
         de-diam-ext-estoq:SCREEN-VALUE IN FRAME F-Main = string(p-Diex-Menos).



  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-cria-parametro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* FOR EACH tt-parametro:
    DELETE tt-parametro.
END. */

ASSIGN i-mercado = INPUT FRAME F-Main rs-mercado.

FOR EACH ped-item NO-LOCK
    WHERE ped-item.nome-abrev   >=  INPUT FRAME F-Main c-emitente-ini
    AND   ped-item.nome-abrev   <=  INPUT FRAME F-Main c-emitente-fim
    AND   ped-item.it-codigo     =  INPUT FRAME F-Main c-it-codigo
    AND   ped-item.dt-entrega   >=  INPUT FRAME F-Main dt-entrega-ini
    AND   ped-item.dt-entrega   <=  INPUT FRAME F-Main dt-entrega-fim
    AND   ped-item.ind-componen <> 3
    AND   ped-item.cod-sit-item <= 2,
    FIRST ped-venda OF ped-item  NO-LOCK
          WHERE ped-venda.cod-estabel  = INPUT FRAME F-Main c-cod-estabel
            AND ped-venda.tp-ped      >= INPUT FRAME F-Main tp-pedido-ini
            AND ped-venda.tp-ped      <= INPUT FRAME F-Main tp-pedido-fim
            AND ped-venda.nr-pedido   >= INPUT FRAME F-Main nr-pedido-ini
            AND ped-venda.nr-pedido   <= INPUT FRAME F-Main nr-pedido-fim 
            AND ped-venda.cod-sit-ped <= 2,
    FIRST natur-oper NO-LOCK
      WHERE natur-oper.nat-operacao =  ped-venda.nat-operacao 
      AND   natur-oper.mercado      = i-mercado OR i-mercado = 3:

    CREATE tt-parametro.
    ASSIGN tt-parametro.nr-pedido   = ped-venda.nr-pedido
           tt-parametro.sequencia   = ped-item.nr-sequencia
           tt-parametro.qt-pedida   = ped-item.qt-pedida
           tt-parametro.cod-refer   = ped-item.cod-refer
           tt-parametro.it-codigo   = ped-item.it-codigo
           tt-parametro.cod-estabel = ped-venda.cod-estabel.

    /*---- Resultado Variaveis da Estrutura ----*/
    FIND FIRST cot-est-mast
        WHERE cot-est-mast.item-cotacao = ped-item.it-codigo 
          AND cot-est-mast.nr-estrut    = ped-item.nr-config NO-LOCK NO-ERROR.

    IF AVAIL cot-est-mast THEN DO:

        /* Largura Larg */
        FIND FIRST var-result 
            WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
              AND var-result.nr-estrut    = cot-est-mast.nr-estrut
              AND var-result.nome-var     = "LARGURA"  
             NO-LOCK NO-ERROR.
        IF AVAIL var-result THEN 
            ASSIGN tt-parametro.largura = dec(var-result.des-result).

        /*Diametro Interno  DIIN*/
        FIND FIRST var-result 
            WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
              AND var-result.nr-estrut    = cot-est-mast.nr-estrut
              AND var-result.nome-var     = "DIIN"  
            NO-LOCK NO-ERROR.
        IF AVAIL var-result THEN 
            ASSIGN tt-parametro.diamin = dec(var-result.des-result).
                
        /*Diametro Externo DIEX */
        FIND FIRST var-result 
            WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                 AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                 AND var-result.nome-var     = "DIEX"  
            NO-LOCK no-error.
        IF AVAIL var-result THEN 
            ASSIGN tt-parametro.diamex = dec(var-result.des-result).

        /*** VERIFICAR QUANTIDADE PALETIZADA ***/
        ASSIGN de-saldo = 0.
        FOR EACH pallet WHERE
                 pallet.cod-estabel = ped-venda.cod-estabel AND
                 pallet.it-codigo   = ped-item.it-codigo    AND
                 pallet.nr-pedido   = ped-venda.nr-pedido   and
                 pallet.situacao    = 2 NO-LOCK:
           FIND first saldo-estoq 
                where saldo-estoq.it-codigo    = pallet.it-codigo 
                  and saldo-estoq.cod-estabel  = pallet.cod-estabel
                  and saldo-estoq.lote         = pallet.nr-pallet
                  AND saldo-estoq.cod-depos   >= INPUT FRAME F-Main c-cod-depos-ini
                  AND saldo-estoq.cod-depos   <= INPUT FRAME F-Main c-cod-depos-fim
                  and saldo-estoq.qtidade-atu  > 0 NO-LOCK NO-ERROR.
           IF AVAIL saldo-estoq THEN DO:
               assign de-saldo = de-saldo + (saldo-estoq.qtidade-atu - 
                                (saldo-estoq.qt-alocada  +
                                 saldo-estoq.qt-aloc-ped +
                                 saldo-estoq.qt-aloc-prod)) .
           END.
           IF de-saldo = 0 THEN
               ASSIGN de-saldo = de-saldo + pallet.peso-liquido.
        END.
        ASSIGN tt-parametro.qt-paletizada = de-saldo.
    END.

END.

/*** Valida faixa carac. pedido ***/
FOR EACH tt-parametro
    WHERE tt-parametro.largura < INPUT FRAME F-Main de-largura
    OR    tt-parametro.largura > INPUT FRAME F-Main de-largura-fim
    OR    tt-parametro.diamex  < INPUT FRAME F-Main de-diam-ext
    OR    tt-parametro.diamex  > INPUT FRAME F-Main de-diam-ext-fim
    OR    tt-parametro.diamin  < INPUT FRAME F-Main de-diam-int
    OR    tt-parametro.diamin  > INPUT FRAME F-Main de-diam-int-fim
    or    tt-parametro.qt-paletizada >= tt-parametro.qt-pedida: /*descarta pedido totalmente atendido*/
    DELETE tt-parametro.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /*** VALIDAÄÂES ***/
  IF INPUT FRAME F-Main c-cod-estabel = "" THEN DO:
      ASSIGN l-erro = YES.
      RUN utp/ut-msgs.p(INPUT "show",
                        INPUT "25997",
                        INPUT "Estabelecimento incorreto!" + "~~" +
                              "O estabelecimento deve ser informado.").
      APPLY "entry" TO c-cod-estabel IN FRAME F-Main.
      RETURN NO-APPLY.
  END.

  IF INPUT FRAME F-Main c-it-codigo = "" THEN DO:
      ASSIGN l-erro = YES.
      RUN utp/ut-msgs.p(INPUT "show",
                        INPUT "25997",
                        INPUT "Item incorreto!" + "~~" +
                              "O item deve ser informado.").
      APPLY "entry" TO c-it-codigo IN FRAME F-Main.
      RETURN NO-APPLY.
  END.

  FIND estabelec
      WHERE estabelec.cod-estabel = INPUT FRAME F-Main c-cod-estabel
      NO-LOCK NO-ERROR.
  IF NOT AVAIL estabelec THEN DO:
      ASSIGN l-erro = YES.
      RUN utp/ut-msgs.p(INPUT "show",
                        INPUT "25997",
                        INPUT "Estabelecimento incorreto!" + "~~" +
                              "O estabelecimento informado n∆o existe.").
      APPLY "entry" TO c-cod-estabel IN FRAME F-Main.
      RETURN NO-APPLY.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this JanelaDetalhe, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */


