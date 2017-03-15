&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESRE0193 2.00.00.005}  /*** 010005 ***/
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

&GLOBAL-DEFINE PGLAY f-pg-lay
&GLOBAL-DEFINE PGSEL 
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGLOG f-pg-log

/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arq-entrada1     as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    field data-trans       as date
    field cod-estabel      AS CHAR
    field cod-emitente     AS INTEGER
    field natur-oper       AS CHAR
    field serie            AS CHAR
    FIELD qt-notas         AS INTEGER.



DEFINE TEMP-TABLE tt-ordem
    FIELD numero-ordem   LIKE ordem-compra.numero-ordem
    FIELD quantidade     AS DEC
    INDEX chave-ord IS PRIMARY UNIQUE 
                                  numero-ordem.


DEFINE TEMP-TABLE tt-notas
    FIELD nro-docto      LIKE docum-est.nro-docto 
    FIELD it-codigo      LIKE item-doc-est.it-codigo 
    FIELD it-codigo-forn AS CHAR FORMAT "x(16)" LABEL "Item Forn"
    FIELD sequencia      LIKE item-doc-est.sequencia
    FIELD cod-emitente   AS INT LABEL "Emit.NF" 
    FIELD cod-emit-ord   AS INT LABEL "Emit.Ord" 
    FIELD nat-operacao   LIKE docum-est.nat-operacao  
    FIELD serie          LIKE docum-est.serie  
    FIELD numero-ordem   LIKE ordem-compra.numero-ordem
    FIELD parcela       LIKE prazo-compra.parcela
    FIELD dt-emis-nota   LIKE docum-est.dt-emissao
    FIELD preco-item     AS DEC LABEL "Prec.NF"
    FIELD preco-forn     AS DEC LABEL "Prec.Ord"
    FIELD peso-liquido   LIKE item-doc-est.peso-liquido 
    FIELD class-fiscal   AS CHAR
    FIELD aliq-ipi       LIKE item-doc-est.aliquota-ipi
    FIELD trib-ipi       AS INT
    FIELD aliq-icm       LIKE item-doc-est.aliquota-icm 
    FIELD trib-icm       AS INT
    FIELD base-ipi       AS DEC
    FIELD base-icm       AS DEC
    FIELD vl-icm         AS DEC
    FIELD vl-ipi         AS DEC
    FIELD quantidade     AS DEC FORMAT ">>>,>>9.9999" LABEL "Qtde."
    /*
    item-doc-est.pr-total[1] 
    item-doc-est.pr-total[2] 
    item-doc-est.pr-total[3]
    item-doc-est.preco-total
     
    
     item-doc-est.base-icm[1] 
    item-doc-est.base-icm[2] 
    item-doc-est.baFse-ipi[1] 
    item-doc-est.base-ipi[2] 
    item-doc-est.cd-trib-icm 
    item-doc-est.cd-trib-ipi 
    item-doc-est.cod-depos 
    item-doc-est.cod-emitente 
    item-doc-est.valor-icm[1] 
    item-doc-est.valor-icm[2] 
    item-doc-est.valor-ipi[1] 
    item-doc-est.valor-ipi[2]
     
    docum-est.tot-valor 
    docum-est.valor-mercad 
    docum-est.icm-deb-cre 
    docum-est.ipi-deb-cre
    
    FIELD icms-aliq      LIKE  docum-est.aliquota-icm
    FIELD vl-frete       LIKE  polo-frete-conhecimento.vl-frete
    FIELD vl-icms        LIKE polo-frete-conhecimento.vl-icms
   */ INDEX chave IS PRIMARY UNIQUE 
                                  nro-docto 
                                  sequencia
                                  it-codigo.

DEFINE TEMP-TABLE tt-lotes
     FIELD nro-docto      LIKE docum-est.nro-docto 
     FIELD it-codigo      LIKE item-doc-est.it-codigo 
     FIELD sequencia      LIKE item-doc-est.sequencia
     FIELD lote           AS CHAR
     FIELD dt-validade    AS DATE
     FIELD quantidade     AS DEC

    INDEX chave1 IS PRIMARY UNIQUE 
                                  nro-docto 
                                  sequencia
                                  it-codigo
                                  lote.


/* Transfer Definitions */



def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
def temp-table tt-raw-digita-lote
   field raw-digita      as raw.

/* Local Variable Definitions --- 
                                      */
DEFINE VARIABLE dt-emiss AS DATE       NO-UNDO.

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-arq-term         as char    no-undo.
def stream s-entrada.
DEFINE VARIABLE i-seq-item AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-qt-frete AS INTEGER    NO-UNDO.
DEFINE VARIABLE d-vl-frete AS DECIMAL    NO-UNDO.
DEFINE VARIABLE c-char AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-nro-docto AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-it-codigo AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dt-medio-calc AS DATE       NO-UNDO.



/* Vari†veis usadas para gerar planilha excel. */

DEF VAR c-arq             AS CHAR  FORMAT "x(50)"  NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR  FORMAT "x(50)"  NO-UNDO.   
DEF VAR l-cria            AS LOG                 NO-UNDO.
DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
DEF VAR i-linha           AS INT                 NO-UNDO.

/* para executar o Excel */

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.

def var  ax-item  as char no-undo.

define buffer  bf-tt-notas for tt-notas.



def var h-acomp            as handle  no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE h-q1 AS HANDLE     NO-UNDO.

DEFINE QUERY q1 FOR tt-notas SCROLLING .

{include/i-imdef.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-impor
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-import

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-executar bt-carrega bt-excel bt-cancelar bt-ajuda im-pg-lay ~
im-pg-log im-pg-par 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-carrega 
     LABEL "Carrega" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-excel 
     LABEL "Excel" 
     SIZE 10 BY 1.


DEFINE IMAGE im-pg-lay
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-log
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-par
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

DEFINE BUTTON bt-editar 
     LABEL "Editar Layout" 
     SIZE 20 BY 1.

DEFINE VARIABLE ed-layout AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 76 BY 9.25
     FONT 2 NO-UNDO.

DEFINE BUTTON bt-arquivo-destino 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr-destino 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo-destino AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 3 
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
     SIZE 47 BY 2.92.



DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 47 BY 1.71.

DEFINE BUTTON bt-arquivo-entrada 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.
                     
DEFINE VARIABLE c-arquivo-entrada AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  LABEL "Arquivo de Entrada" NO-UNDO.
     
 

DEFINE VARIABLE ed-dt-trans AS DATE FORMAT "99/99/9999":U INITIAL ? 
     LABEL "Data de transaá∆o" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 12.86 BY .88 NO-UNDO.

DEFINE VARIABLE ed-cod-estabel AS char FORMAT "x(03)":U   /*solic-318*/ 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE ed-cod-emitente AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Emitente" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 9 BY .88 NO-UNDO.
/*
DEFINE VARIABLE ed-numero-ordem AS INTEGER FORMAT ">>>>>>>,>9" 
     LABEL "Ordem Compra" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE ed-protocolo AS char FORMAT "x(08)" 
     LABEL "Protocolo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.
*/
DEFINE VARIABLE ed-i-qt-frete AS integer FORMAT ">>>9" 
     LABEL "Qt.Itens" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE ed-d-vl-frete AS DEC FORMAT ">>>>,>>9.99" 
     LABEL "Vl.Tot.Doctos" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.



DEFINE BROWSE b1 QUERY q1 DISPLAY 
    nro-docto    
    it-codigo    
    it-codigo-forn
    sequencia
    numero-ordem 
    parcela
    quantidade
    preco-item 
    preco-forn
    cod-emitente 
    cod-emit-ord
    nat-operacao 
    serie        
     
    dt-emis-nota 
    peso-liquido 
    class-fiscal 
    aliq-ipi     
    trib-ipi     
    aliq-icm     
    trib-icm     
    base-ipi     
    base-icm     
    vl-icm       
    vl-ipi       
       ENABLE numero-ordem   parcela
     WITH  NO-ASSIGN SEPARATORS 
    SIZE 72.4 BY 4.9 /*TITLE "Customer Browse"*/.



DEFINE VARIABLE ed-nome-abrev AS char FORMAT "x(14)":U INITIAL ""
     /*NO-LABEL */
     VIEW-AS FILL-IN NATIVE 
     SIZE 15 BY .88 NO-UNDO.




DEFINE VARIABLE ed-natur-oper AS char FORMAT "X(10)":U INITIAL "" 
     LABEL "Nat.Operaá∆o" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE ed-serie AS char FORMAT "X(05)":U INITIAL "" 
     LABEL "Serie" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 04 BY .88 NO-UNDO.


/*
DEFINE VARIABLE text-entrada AS CHARACTER FORMAT "X(17)":U INITIAL "Arquivo de Entrada:" 
      VIEW-AS TEXT 
     SIZE 17 BY .63 NO-UNDO.
  */
DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 4.3.

/*DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.7.
  */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-log
     rs-destino AT ROW 2.08 COL 3.43 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-config-impr-destino AT ROW 3.25 COL 43.43 HELP
          "Configuraá∆o da impressora"
     bt-arquivo-destino AT ROW 3.25 COL 43.43 HELP
          "Escolha do nome do arquivo"
     c-arquivo-destino AT ROW 3.33 COL 3.43 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.46 COL 3.29 HELP
          "Modo de Execuá∆o" NO-LABEL
     text-destino AT ROW 1.33 COL 4 NO-LABEL
     text-modo AT ROW 4.71 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.63 COL 2.14

     RECT-9 AT ROW 5 COL 2.14

    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.29 BY 10.46.

DEFINE FRAME f-pg-lay
     ed-layout AT ROW 1 COL 1 NO-LABEL
     bt-editar AT ROW 10.38 COL 1 HELP
          "Dispara a Impress∆o do Layout"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.46.

DEFINE FRAME f-import
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Cancelar"
     bt-carrega AT ROW 14.54 COL 25 HELP
          "Carrega"
     bt-excel   AT ROW 14.54 COL 36 HELP
          "Excel"

     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     im-pg-lay AT ROW 1.5 COL 2.14
     im-pg-log AT ROW 1.5 COL 33.57
     im-pg-par AT ROW 1.5 COL 17.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.



DEFINE FRAME f-pg-par
          ed-dt-trans     AT ROW 1.25 COL 20.14 COLON-ALIGNED
     ed-cod-estabel  AT ROW 2.25 COL 20.14 COLON-ALIGNED
     ed-cod-emitente AT ROW 3.25 COL 20.14 COLON-ALIGNED
      ed-i-qt-frete   AT ROW 3.25 COL 60.14 COLON-ALIGNED
     ed-d-vl-frete   AT ROW 4.25 COL 60.14 COLON-ALIGNED
     ed-nome-abrev   AT ROW 3.25 COL 30.14 COLON-ALIGNED NO-LABEL
     ed-natur-oper   AT ROW 4.25 COL 20.14 COLON-ALIGNED
     ed-serie        AT ROW 4.25 COL 35.14 COLON-ALIGNED
    c-arquivo-entrada AT ROW 5.45 COL 20.14 COLON-ALIGNED HELP
          "Nome do arquivo de destino do relat¢rio" 
     bt-arquivo-entrada AT ROW 5.45 COL 62 HELP
          "Escolha do nome do arquivo"

   /*  text-entrada AT ROW 5.49 COL 4.29 NO-LABEL*/
     RECT-10 AT ROW 1 COL 2.14
     /*RECT-8 AT ROW 5.67 COL 2.14*/
     b1                AT ROW 6.45 COLUMN 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.14 ROW 3
         SIZE 77.57 BY 10.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-impor
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Importaá∆o de Notas Fiscais"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 22.33
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.33
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
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-impor.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-import
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-import
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-lay
                                                                        */
ASSIGN 
       ed-layout:RETURN-INSERTED IN FRAME f-pg-lay  = TRUE
       ed-layout:READ-ONLY IN FRAME f-pg-lay        = TRUE.

/* SETTINGS FOR FRAME f-pg-log
                                                                        */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-log
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-log     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-log
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-log     = 
                "Execuá∆o".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FILL-IN text-entrada IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */

/*ASSIGN 
       text-entrada:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Arquivo de Entrada:".*/

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-log
/* Query rebuild information for FRAME f-pg-log
     _Query            is NOT OPENED
*/  /* FRAME f-pg-log */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Importaá∆o de Notas Fiscais */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Importaá∆o de Notas Fiscais */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda C-Win
ON CHOOSE OF bt-ajuda IN FRAME f-import /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-log
&Scoped-define SELF-NAME bt-arquivo-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-destino C-Win
ON CHOOSE OF bt-arquivo-destino IN FRAME f-pg-log
DO:
    {include/i-imarq.i c-arquivo-destino f-pg-log}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/*     

&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME ed-numero-ordem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ed-numero-ordem C-Win
ON leave OF ed-numero-ordem IN FRAME f-pg-par
DO:
  FOR EACH tt-notas.
      DELETE tt-notas.
  END.

  ASSIGN b1:SENSITIVE  IN FRAME f-pg-par = NO.

  OPEN QUERY q1 FOR EACH tt-notas.
      
  ASSIGN c-it-codigo = "".

 IF int(ed-numero-ordem:SCREEN-VALUE) = 0 THEN RETURN .

  FIND FIRST ordem-compra NO-LOCK WHERE
       ordem-compra.numero-ordem = integer(ed-numero-ordem:SCREEN-VALUE) NO-ERROR.
  
  find FIRST cotacao-item OF ordem-compra WHERE cotacao-item.cot-aprovada AND  cotacao-item.log-1 NO-LOCK NO-ERROR.
  
  IF NOT AVAIL ordem-compra OR NOT AVAIL cotacao-item THEN DO:
       RUN utp/ut-msgs.p(INPUT "show",
                       INPUT 123,
                       INPUT ed-numero-ordem:SCREEN-VALUE + " n∆o aprovada ou"). 
       
      APPLY "entry" TO ed-numero-ordem.
      RETURN NO-APPLY.
  END.







  ASSIGN c-it-codigo = ordem-compra.it-codigo.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
*/
/*
&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME ed-protocolo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ed-protocolo C-Win
ON leave OF ed-protocolo IN FRAME f-pg-par
DO:
  FOR EACH tt-notas.
      DELETE tt-notas.
  END.

  ASSIGN b1:SENSITIVE  IN FRAME f-pg-par = NO.

  OPEN QUERY q1 FOR EACH tt-notas.
  END.

 /* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


*/

&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME ed-cod-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ed-cod-emitente C-Win
ON leave OF ed-cod-emitente IN FRAME f-pg-par
DO:
    FIND FIRST emitente NO-LOCK WHERE
           emitente.cod-emitente = integer(ed-cod-emitente:SCREEN-VALUE)
           NO-ERROR.

      IF AVAIL emitente THEN 

         ASSIGN ed-nome-abrev:SCREEN-VALUE = string(emitente.nome-abrev).
      ELSE
          ASSIGN ed-nome-abrev:SCREEN-VALUE = "".


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME ed-natur-oper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ed-natur-oper C-Win
ON leave OF ed-natur-oper IN FRAME f-pg-par
DO:
    FIND FIRST natur-oper NO-LOCK WHERE
           natur-oper.nat-operacao = ed-natur-oper:SCREEN-VALUE
           NO-ERROR.

      IF NOT AVAIL natur-oper THEN DO:
          RUN utp/ut-msgs.p(INPUT "show",
                  INPUT 2,
                  INPUT ", Natureza de operaá∆o " + ed-natur-oper:SCREEN-VALUE).

          RETURN NO-APPLY.
      END.
          


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

             
&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME bt-arquivo-entrada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-entrada C-Win
ON CHOOSE OF bt-arquivo-entrada IN FRAME f-pg-par
DO:
    {include/i-imarq.i c-arquivo-entrada f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
               


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME b1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b1 C-Win
ON ROW-LEAVE OF b1 IN FRAME f-pg-par
DO:
     if b1:NEW-ROW in frame f-pg-par then 
    do transaction on error undo, return no-apply: 
         RETURN NO-APPLY.
     END.
    ELSE do transaction on error undo, return no-apply:
        
            assign INPUT BROWSE b1 tt-notas.numero-ordem tt-notas.parcela
                 .
            display
                tt-notas.numero-ordem tt-notas.parcela
                 
                with browse b1. 

            IF tt-notas.numero-ordem = 0 THEN RETURN.

            FIND ordem-compra WHERE ordem-compra.numero-ordem = tt-notas.numero-ordem NO-LOCK NO-ERROR.
            FIND first prazo-compra WHERE prazo-compra.numero-ordem = tt-notas.numero-ordem and
                 prazo-compra.parcela = tt-notas.parcela NO-LOCK NO-ERROR.

            

            IF AVAIL ordem-compra AND AVAIL prazo-compra THEN DO:

                FIND ITEM WHERE ITEM.it-codigo = ordem-compra.it-codigo NO-LOCK NO-ERROR.

                ASSIGN tt-notas.it-codigo = ordem-compra.it-codigo
                       tt-notas.preco-forn = ordem-compra.preco-unit
                       tt-notas.cod-emit-ord = ordem-compra.cod-emitente
                       tt-notas.class-fiscal = ITEM.class-fiscal.
                    
                DISPLAY  
                    tt-notas.it-codigo
                    tt-notas.preco-forn 
                    tt-notas.cod-emit-ord
                    tt-notas.class-fiscal with browse b1.
                 

            END.
            ELSE DO:

                run utp/ut-msgs.p (input "show",
                                    input 2,
                                    input "Ordem de compra ou parcela da ordem").

                RETURN NO-APPLY.
            END.
            
    END.
                                    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define FRAME-NAME f-import
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar C-Win
ON CHOOSE OF bt-cancelar IN FRAME f-import /* Cancelar */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-log
&Scoped-define SELF-NAME bt-config-impr-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr-destino C-Win
ON CHOOSE OF bt-config-impr-destino IN FRAME f-pg-log
DO:
   {include/i-imimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-lay
&Scoped-define SELF-NAME bt-editar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-editar C-Win
ON CHOOSE OF bt-editar IN FRAME f-pg-lay /* Editar Layout */
DO:
   {include/i-imedl.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-import
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar C-Win
ON CHOOSE OF bt-executar IN FRAME f-import /* Executar */
DO:
   do  on error undo, return no-apply:
       
       if ax-item <> "" then do:
       run utp/ut-msgs.p (input "show",
                                      input 17006,
                                      input "Emitente da NF sem relacionamento com os item ~~" + ax-item).
               return no-apply.

       end.
       
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define FRAME-NAME f-import
&Scoped-define SELF-NAME bt-carrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-carrega C-Win
ON CHOOSE OF bt-carrega IN FRAME f-import /* carrega */
DO:
   do  on error undo, return no-apply:

       run pi-carrega.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define FRAME-NAME f-import
&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel C-Win
ON CHOOSE OF bt-excel IN FRAME f-import /* Excel */
DO:
   do  on error undo, return no-apply:

       run pi-excel.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME im-pg-lay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-lay C-Win
ON MOUSE-SELECT-CLICK OF im-pg-lay IN FRAME f-import
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-log C-Win
ON MOUSE-SELECT-CLICK OF im-pg-log IN FRAME f-import
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par C-Win
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-import
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define FRAME-NAME f-pg-log
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino C-Win
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-log
DO:
do  with frame f-pg-log:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo-destino:sensitive     = no
                   bt-arquivo-destino:visible      = no
                   bt-config-impr-destino:visible  = yes.
        end.
        when "2" then do:
            assign c-arquivo-destino:sensitive     = yes
                   bt-arquivo-destino:visible      = yes
                   bt-config-impr-destino:visible  = no.
        end.
        when "3" then do:
            assign c-arquivo-destino:sensitive     = no
                   bt-arquivo-destino:visible      = no
                   bt-config-impr-destino:visible  = no.
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-execucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao C-Win
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-log
DO:
   {include/i-imrse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-import
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESRE0193" "2.00.00.000"}

/* inicializaá‰es do template de importaá∆o */
{include/i-imini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-imlbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    ed-cod-estabel =  STRING({cdp\poloestab.i 422}). /*solic-318*/ 

    ASSIGN ed-dt-trans   = TODAY.
   /* FIND  FIRST param-estoq NO-LOCK no-error.
    IF AVAIL param-estoq THEN do: 
        ASSIGN dt-medio-calc = DATE("01/" + 
                               STRING(MONTH(param-estoq.mensal-ate + 35)) + "/" +
                               STRING(YEAR(param-estoq.mensal-ate + 35))) - 1.
   
        IF ed-dt-trans > dt-medio-calc THEN
            ASSIGN ed-dt-trans = dt-medio-calc.
    END.*/

    RUN enable_UI.
    

    {include/i-immbl.i im-pg-par}

    {include/i-imvrf.i &programa=esre0190 &versao-layout=001}

    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects C-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available C-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  
  ENABLE bt-executar bt-cancelar bt-carrega bt-excel bt-ajuda im-pg-lay im-pg-log im-pg-par 
      WITH FRAME f-import IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-import}
  DISPLAY 
      c-arquivo-entrada 
      ed-dt-trans 
      ed-cod-estabel 
      ed-cod-emitente 
      ed-i-qt-frete 
      ed-d-vl-frete /*b1*/ 
      ed-nome-abrev 
      ed-natur-oper 
      ed-serie        

      WITH FRAME f-pg-par IN WINDOW C-Win.

  ENABLE 
      c-arquivo-entrada 
      bt-arquivo-entrada
      ed-dt-trans ed-cod-estabel 
      ed-cod-emitente  
      /*ed-i-qt-frete ed-d-vl-frete b1*/ 
      ed-natur-oper 
      ed-serie RECT-10 /*RECT-8 */
      WITH FRAME f-pg-par IN WINDOW C-Win.
  
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY ed-layout 
      WITH FRAME f-pg-lay IN WINDOW C-Win.
  ENABLE ed-layout bt-editar 
      WITH FRAME f-pg-lay IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-lay}
  DISPLAY rs-destino c-arquivo-destino rs-execucao 
      WITH FRAME f-pg-log IN WINDOW C-Win.
  ENABLE rs-destino bt-config-impr-destino bt-arquivo-destino c-arquivo-destino 
         rs-execucao RECT-7 RECT-9 
      WITH FRAME f-pg-log IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-log}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit C-Win 
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


PROCEDURE pi-cria-planilha:

DEF VAR c-arquivo AS CHAR FORMAT "x(50)" NO-UNDO.

    c-arquivo = c-arq + 'esre0193' + STRING(time)+ '.xls'. 

    OS-COPY VALUE(c-modelo-planilha) VALUE(c-arquivo).
 
    assign c-planilha  = c-excel:Workbooks:OPEN(c-arquivo)
           c-relatorio = c-excel:Sheets:item(1)
           c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo.

END PROCEDURE.


PROCEDURE pi-finaliza-impressao:

DEF VAR i         AS INT  NO-UNDO.
DEF VAR c-arquivo AS CHAR NO-UNDO.
 
    c-planilha:SAVE().
    c-planilha:CLOSE().

    c-excel:VISIBLE = true.

 
    DO i = 1 TO NUM-ENTRIES(c-arq-anexo):
        c-arquivo = ENTRY(i,c-arq-anexo).
        c-planilha  = c-excel:Workbooks:OPEN(c-arquivo).

    END.

    /*c-excel:QUIT().*/
    RELEASE OBJECT c-excel.

END PROCEDURE.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-excel C-Win 
PROCEDURE pi-excel :
/* -----------------------------------------------------------
  Purpose:   
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     
    CREATE "Excel.Application" c-excel.
   ASSIGN c-excel:DisplayAlerts = FALSE.
   ASSIGN c-modelo-planilha = search("modelos/mod-esre0193.xls") 
          c-arq             = SESSION:TEMP-DIRECTORY
          c-arq-anexo = "".
   RUN pi-cria-planilha.  

ASSIGN i-linha = 3.

 FOR EACH tt-notas.
  
     ASSIGN i-linha = i-linha + 1
          c-relatorio:range("A" + string(i-linha)):VALUE = tt-notas.nro-docto    
          c-relatorio:range("B" + string(i-linha)):VALUE = tt-notas.it-codigo    
          c-relatorio:range("C" + string(i-linha)):VALUE = tt-notas.it-codigo-forn    
          c-relatorio:range("D" + string(i-linha)):VALUE = tt-notas.sequencia    
          c-relatorio:range("E" + string(i-linha)):VALUE = tt-notas.cod-emitente    
          c-relatorio:range("F" + string(i-linha)):VALUE = tt-notas.cod-emit-ord    
          c-relatorio:range("G" + string(i-linha)):VALUE = tt-notas.nat-operacao 
          c-relatorio:range("H" + string(i-linha)):VALUE = tt-notas.serie        
          c-relatorio:range("I" + string(i-linha)):VALUE = string(tt-notas.numero-ordem ) + "/" + STRING(tt-notas.parcela)
          c-relatorio:range("J" + string(i-linha)):VALUE = tt-notas.dt-emis-nota 
          c-relatorio:range("K" + string(i-linha)):VALUE = tt-notas.preco-item   
          c-relatorio:range("L" + string(i-linha)):VALUE = tt-notas.preco-forn   
          c-relatorio:range("M" + string(i-linha)):VALUE = tt-notas.peso-liquido 
          c-relatorio:range("N" + string(i-linha)):VALUE = tt-notas.class-fiscal 
          c-relatorio:range("O" + string(i-linha)):VALUE = tt-notas.aliq-ipi     
          c-relatorio:range("P" + string(i-linha)):VALUE = tt-notas.trib-ipi     
          c-relatorio:range("Q" + string(i-linha)):VALUE = tt-notas.aliq-icm     
          c-relatorio:range("R" + string(i-linha)):VALUE = tt-notas.trib-icm     
          c-relatorio:range("S" + string(i-linha)):VALUE = tt-notas.base-ipi     
          c-relatorio:range("T" + string(i-linha)):VALUE = tt-notas.base-icm     
          c-relatorio:range("U" + string(i-linha)):VALUE = tt-notas.vl-icm       
          c-relatorio:range("V" + string(i-linha)):VALUE = tt-notas.vl-ipi       
          c-relatorio:range("W" + string(i-linha)):VALUE = tt-notas.quantidade
          c-relatorio:range("X" + string(i-linha)):VALUE = tt-notas.quantidade * tt-notas.preco-item.   






 END.

  RUN pi-finaliza-impressao.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega C-Win 
PROCEDURE pi-carrega :
/* -----------------------------------------------------------
  Purpose:   
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   


  FOR EACH tt-notas.
      DELETE tt-notas.
  END.
  FOR EACH tt-lotes.
      DELETE tt-lotes.
  END.

  ASSIGN i-qt-frete = 0 
         d-vl-frete = 0.


  ASSIGN b1:SENSITIVE  IN FRAME f-pg-par = NO.

  OPEN QUERY q1 FOR EACH tt-notas.
    
  IF ed-natur-oper:SCREEN-VALUE = "" THEN DO:

        run utp/ut-msgs.p (input "show",
                                 input 265,
                                 input "NATUREZA OPERACAO").

        RETURN NO-APPLY.
  END.
   
  IF int(ed-cod-emitente:SCREEN-VALUE) = 0 THEN DO:

        run utp/ut-msgs.p (input "show",
                                 input 265,
                                 input "Emitente").

        RETURN NO-APPLY.
  END.
    
    
 
      IF search(c-arquivo-entrada:SCREEN-VALUE IN FRAME f-pg-par) = ?  THEN
         DO:
              run utp/ut-msgs.p (input "show",
                                  input 326,
                                  input c-arquivo-entrada).
              RETURN NO-APPLY.
         END.

        find param-re where param-re.usuario = c-seg-usuario no-lock no-error.
           if NOT avail param-re then  DO:
               run utp/ut-msgs.p (input "show",
                                 input 2,
                                 input "Usuario Recebimento").

           END.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Importaá∆o_de_Documentos *}
run pi-inicializar in h-acomp (input  Return-value ).

   
ASSIGN i-seq-item  = 0.

input stream s-entrada from value(c-arquivo-entrada:SCREEN-VALUE IN FRAME f-pg-par).

 repeat on error undo, leave
          on stop  undo, leave transaction:     
       import stream s-entrada unformatted
              c-char.              
                       
      if substring(c-char,1,1) = "1"
       then do: 
            ASSIGN dt-emiss = date(int(substring(c-char,33,2)),int(substring(c-char,31,2)),int(substring(c-char,35,4))).
            FIND FIRST emitente WHERE emitente.cgc = trim(substring(c-char,9,15)) NO-LOCK NO-ERROR.
            NEXT.                                 

       end.

       if substring(c-char,1,1) = "2" then do:
           find param-re where param-re.usuario = c-seg-usuario no-lock no-error.
           if avail param-re then     
              assign i-seq-item = i-seq-item + param-re.inc-seq.        
           else           
              next.  

        FIND FIRST ordem-compra WHERE ordem-compra.numero-ordem = int(trim(substring(c-char,65,10))) NO-LOCK NO-ERROR.

       
         
        IF AVAIL ordem-compra THEN DO:
            FIND ITEM WHERE ITEM.it-codigo = ordem-compra.it-codigo NO-LOCK NO-ERROR.

        END.

        CREATE tt-notas.
          ASSIGN  
              tt-notas.nro-docto     =  string(int(trim(substring(c-char,2,7))), "9999999") 
              tt-notas.it-codigo     =  IF AVAIL ordem-compra THEN ordem-compra.it-codigo ELSE  "" 
              tt-notas.it-codigo-forn =  substring(c-char,33,16)
              tt-notas.sequencia     =  i-seq-item
              tt-notas.cod-emitente  =  IF AVAIL emitente THEN emitente.cod-emitente ELSE 0
              tt-notas.nat-operacao  =  ed-natur-oper:SCREEN-VALUE IN FRAME f-pg-par
              tt-notas.serie         =  ed-serie:SCREEN-VALUE IN FRAME f-pg-par
              tt-notas.numero-ordem  =  IF AVAIL ordem-compra THEN ordem-compra.numero-ordem ELSE 0
              tt-notas.parcela       =  1
              tt-notas.cod-emit-ord  =  IF AVAIL ordem-compra THEN ordem-compra.cod-emitente ELSE 0
              tt-notas.dt-emis-nota  =  dt-emiss
              tt-notas.preco-item    =  int(trim(substring(c-char,89,13))) / 100
              tt-notas.preco-forn    =  IF AVAIL ordem-compra THEN ordem-compra.preco-unit ELSE 0
              tt-notas.peso-liquido  =  int(trim(substring(c-char,141,14))) / 10000
              tt-notas.class-fiscal  =  IF AVAIL ITEM THEN item.class-fiscal ELSE substring(c-char,151,10)
              tt-notas.aliq-ipi      =  int(trim(substring(c-char,165,5))) / 100
              tt-notas.trib-ipi      =  int(trim(substring(c-char,170,1))) 
              tt-notas.aliq-icm      =  int(trim(substring(c-char,197,5))) / 100
              tt-notas.trib-icm      =  int(trim(substring(c-char,202,1))) 
              tt-notas.base-ipi      =  int(trim(substring(c-char,171,13))) / 100
              tt-notas.base-icm      =  int(trim(substring(c-char,203,13))) / 100
              tt-notas.vl-icm        =  int(trim(substring(c-char,216,13))) / 100
              tt-notas.vl-ipi        =  int(trim(substring(c-char,184,13))) / 100
              tt-notas.quantidade    =  int(trim(substring(c-char,75,14))) / 10000.
              


           
                  
       
           run pi-acompanhar in h-acomp (input tt-notas.nro-docto + " " + tt-notas.it-codigo).
           ASSIGN     i-qt-frete = i-qt-frete + 1
                d-vl-frete = d-vl-frete + tt-notas.preco-item * tt-notas.quantidade.

         ASSIGN ed-i-qt-frete:SCREEN-VALUE IN FRAME f-pg-par = STRING(i-qt-frete)
                ed-d-vl-frete:SCREEN-VALUE IN FRAME f-pg-par = STRING(d-vl-frete,">,>>>,>>9.99").


       end.

       if substring(c-char,1,1) = "3" then do:
                

               CREATE tt-lotes.
                 ASSIGN  
                     tt-lotes.nro-docto  = tt-notas.nro-docto   
                     tt-lotes.it-codigo  = tt-notas.it-codigo     
                     tt-lotes.sequencia  = tt-notas.sequencia 
                     tt-lotes.lote       = substring(c-char,33,10)
                     tt-lotes.dt-validade = date(int(substring(c-char,45,2)),int(substring(c-char,43,2)),int(substring(c-char,47,4)))
                     tt-lotes.quantidade  =  int(trim(substring(c-char,51,14))) / 10000.
       end.


 

   end.

   input stream s-entrada close.

       
         


       
           run pi-acompanhar in h-acomp (input tt-notas.nro-docto + " " + tt-notas.it-codigo).





   run pi-finalizar in h-acomp.
    
   FIND FIRST tt-notas NO-LOCK NO-ERROR.

   IF AVAIL tt-notas THEN DO:
       ASSIGN b1:SENSITIVE  IN FRAME f-pg-par = YES.
       OPEN QUERY q1 FOR EACH tt-notas.
       b1:refresh() IN FRAME F-PG-PAR.
   END.


   ASSIGN h-q1 = QUERY q1:HANDLE.

  ax-item = "".
  
                 FOR EACH bf-tt-notas:

                      find first item-fornec where item-fornec.it-codigo = bf-tt-notas.it-codigo and
                         item-fornec.cod-emitente =  bf-tt-notas.cod-emitente no-lock no-error.
                      
                      if not  avail item-fornec then do:
                         ax-item =                          ax-item + 
                          bf-tt-notas.it-codigo + " - ".  
                             

                       end.
                       
                       
                   
                 end.
                 
                


   RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar C-Win 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*  IF int(ed-numero-ordem:SCREEN-VALUE IN FRAME f-pg-par) = 0 THEN DO:

         run utp/ut-msgs.p (input "show",
                                  input 265,
                                  input "ORDEM DE COMPRA").

         RETURN NO-APPLY.
   END.

   IF int(ed-protocolo:SCREEN-VALUE IN FRAME f-pg-par) = 0 THEN DO:

         run utp/ut-msgs.p (input "show",
                                  input 265,
                                  input "PROTOCOLO").

         RETURN NO-APPLY.
   END.
*/
  FIND FIRST tt-notas NO-ERROR.
  IF NOT AVAIL tt-notas THEN
  DO:

         run utp/ut-msgs.p (input "show",
                           input 7574,
                           input "").
          RETURN NO-APPLY.
  END.

  FOR EACH tt-ordem.
      DELETE tt-ordem.
  END.

  FOR EACH tt-notas NO-LOCK.

      IF tt-notas.cod-emitente = 0 OR tt-notas.cod-emitente <> int(ed-cod-emitente:SCREEN-VALUE IN FRAME f-pg-par)
          OR tt-notas.cod-emitente <> tt-notas.cod-emit-ord THEN DO:
          
                
          run utp/ut-msgs.p (input "show",
                                      input 36,
                                      input "Emitente da NF " + TRIM(tt-notas.nro-docto) + " Seq:" + STRING(tt-notas.sequencia) + " diferente do informado/da ordem ou ").
           
      END.

      IF tt-notas.preco-item <> tt-notas.preco-forn THEN DO:


          run utp/ut-msgs.p (input "show":U, input 4238, tt-notas.it-codigo + "~~diferente da ordem(" + STRING(tt-notas.numero-ordem) + ") ou").
          RETURN NO-APPLY.

      END.

      FIND FIRST tt-ordem WHERE tt-ordem.numero-ordem = tt-notas.numero-ordem NO-ERROR.
      IF NOT AVAIL tt-ordem THEN DO:
          CREATE tt-ordem.
          tt-ordem.numero-ordem = tt-notas.numero-ordem.
      END.
      
      tt-ordem.quantidade = tt-ordem.quantidade + tt-notas.quantidade.
  END.

 FOR EACH tt-ordem NO-LOCK.

  FIND FIRST ordem-compra NO-LOCK WHERE
       ordem-compra.numero-ordem = tt-ordem.numero-ordem NO-ERROR.

      IF NOT AVAIL ordem-compra OR ordem-compra.situacao <> 2 THEN DO:
           RUN utp/ut-msgs.p(INPUT "show",
                           INPUT 123,
                           INPUT string(tt-ordem.numero-ordem)  + " N∆o confirmada ou"). 
           
           
          RETURN NO-APPLY.
      END.
      
      for each tt-notas where tt-notas.numero-ordem = tt-ordem.numero-ordem no-lock.
      
         
          FIND first prazo-compra WHERE prazo-compra.numero-ordem = tt-notas.numero-ordem AND
              prazo-compra.parcela = tt-notas.parcela NO-LOCK NO-ERROR.
    
          IF NOT AVAIL prazo-compra OR prazo-compra.quant-saldo = 0 THEN DO:
               RUN utp/ut-msgs.p(INPUT "show",
                               INPUT 123,
                               INPUT string(tt-ordem.numero-ordem)  + " N∆o tem saldo ou"). 
               
               
              RETURN NO-APPLY.
          END.

       
       
       end.
       
       
        IF prazo-compra.quant-saldo < tt-ordem.quantidade THEN DO:
 
           RUN utp/ut-msgs.p(INPUT "show",
                          INPUT 123,
                          INPUT string(tt-ordem.numero-ordem) + " Saldo da Parcela da Ordem: " +
                              string(prazo-compra.quant-saldo) + " , Quantidade NF: " + 
                            STRING((tt-ordem.quantidade)) +
                             ". Saldo na ordem suficiente"). 


 
           RETURN NO-APPLY.

      END.
    
      IF (ordem-compra.qt-solic - ordem-compra.qt-acum-rec) < tt-ordem.quantidade THEN DO:
 
           RUN utp/ut-msgs.p(INPUT "show",
                          INPUT 123,
                          INPUT string(tt-ordem.numero-ordem) + " Saldo na Ordem: " + string(ordem-compra.qt-solic - ordem-compra.qt-acum-rec) + " , Quantidade NF: " + 
                            STRING((tt-ordem.quantidade)) +
                             ". Saldo na ordem suficiente"). 


 
           RETURN NO-APPLY.

      END.


           

  END.

do  on error undo, return error
    on stop  undo, return error:     

/*    {include/i-rpexa.i}*/

    /*if  input frame f-pg-log rs-destino = 2 then do:
        run utp/ut-vlarq.p (input input frame f-pg-log c-arquivo-destino).
        if  return-value = "nok" then do:
            run utp/ut-msgs.p (input "show",
                               input 73,
                               input "").
            apply 'mouse-select-click' to im-pg-log in frame f-import.
            apply 'entry' to c-arquivo-destino in frame f-pg-log.                   
            return error.
        end.
    end.

    assign file-info:file-name = input frame f-pg-par c-arquivo-entrada.
    if  file-info:pathname = ? then do:
        run utp/ut-msgs.p (input "show",
                           input 326,
                           input c-arquivo-entrada).                               
        apply 'mouse-select-click' to im-pg-par in frame f-import.
        apply 'entry' to c-arquivo-entrada in frame f-pg-par.                
        return error.
    end. 
      */
    /* Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na p†gina 
       com problemas e colocar o focus no campo com problemas             */    

    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-log rs-destino
           tt-param.arq-entrada1    = input frame f-pg-par c-arquivo-entrada
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           tt-param.data-trans      = input frame f-pg-par ed-dt-trans
           tt-param.cod-estabel     = input frame f-pg-par ed-cod-estabel 
           tt-param.cod-emitente    = int(input frame f-pg-par ed-cod-emitente)
           tt-param.natur-oper      = input frame f-pg-par ed-natur-oper  
           tt-param.serie           = input frame f-pg-par ed-serie  
            .

     
     
     
     
    if  tt-param.destino = 1 then
        assign tt-param.arq-destino = "".
    else
    if  tt-param.destino = 2 then 
        assign tt-param.arq-destino = input frame f-pg-log c-arquivo-destino.
    else
        assign tt-param.arq-destino = session:temp-directory + c-programa-mg97 + ".tmp".

    /* Coloque aqui a l¢gica de gravaá∆o dos parÉmtros e seleá∆o na temp-table
       tt-param */ 
 FOR EACH tt-raw-digita:
   delete tt-raw-digita.
 END.
  FOR EACH tt-raw-digita-lote:
   delete tt-raw-digita-lote.
 END.


  ASSIGN tt-param.qt-notas = 0.

  FOR EACH tt-notas NO-LOCK.
      CREATE tt-raw-digita.
      raw-transfer tt-notas    to tt-raw-digita.raw-digita.
      ASSIGN tt-param.qt-notas = tt-param.qt-notas + 1.
  END.
   
  FOR EACH tt-lotes NO-LOCK.
    CREATE tt-raw-digita.
    raw-transfer tt-lotes    to tt-raw-digita.raw-digita.
  END.




    {include/i-imexb.i}

    if  session:set-wait-state("general") then.

    {include/i-imrun.i rep/esre0193rp.p}

    {include/i-imexc.i}

    if  session:set-wait-state("") then.

    {include/i-imtrm.i tt-param.arq-destino tt-param.destino}

end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina C-Win 
PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P†gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{include/i-imtrp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records C-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-impor, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed C-Win 
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


