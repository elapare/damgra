&Scoped-define WINDOW-NAME C-Win

/*------------------------------------------------------------------------
File.............: essf0021.w
Description......: Setup de Produá∆o no Corte/Recorte/Metalizaá∆o 
Input Parameters : 
Output Parameters: 
Author...........: Amgra - JosÇ Roberto.
Created..........: 01/11/2010   
OBS..............: 
------------------------------------------------------------------------*/
DEFINE VARIABLE r-codigo-rwi  AS ROWID                     NO-UNDO.

{bf\buffersUni2.i}
    
define variable c-prog-gerado as character no-undo initial "essf0021".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

def new global shared var c-erro-amg as char  format "x(3)" no-undo.

CREATE WIDGET-POOL. 

/* ***************************  Definitions  ************************** */


/* Preprocessadores do Template de Relat¢rio                            */


/* Include Com as Vari†veis Globais */

def new global shared var i-ep-codigo-usuario  like empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def new global shared var i-num-ped-exec-rpw  as integer no-undo.   
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
def new global shared var l-rpc as logical no-undo.
def new global shared var r-registro-atual as rowid no-undo.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var i-num-ped as integer no-undo.         
def new global shared var v_cdn_empres_usuar   like empresa.ep-codigo        no-undo.
def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab     as handle                   no-undo.
def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
def new global shared var v_num_tip_aces_usuar   as int                      no-undo.
def new global shared var rw-log-exec            as rowid                    no-undo.


/* Parameters Definitions ---                                           */ 


/* Temporary Table Definitions ---                                      */ 

/* Temporary Table Definitions ---                                      */ 

    DEFINE TEMP-TABLE tt-estacao no-undo
        FIELD estacao             AS INT     FORMAT ">>9"            LABEL "Estaá∆o"
        FIELD nr-ord-produ        AS INT     FORMAT ">>>>>>>>9"      label "Ord.Producao"
        FIELD it-codigo           AS CHAR    FORMAT "x(16)"          LABEL "Item"
        FIELD lote                AS CHAR    FORMAT "x(10)"          LABEL "Lote"
        FIELD nr-pedido           AS INT     FORMAT ">>>>>>>>9"      label "Nr.Pedido"
        FIELD seq-ped             AS INT     FORMAT ">>>>9"          label "Seq"
        FIELD nome-abrev          AS CHAR    FORMAT "x(12)"          LABEL "Cliente"
        FIELD cod-depos           AS CHAR    FORMAT "x(03)"          LABEL "Dep"
        FIELD cod-localiz         AS CHAR    FORMAT "x(10)"          LABEL "Localiz"
        FIELD gera-pallet         AS CHAR    FORMAT "x(1)"           label "Gera Pallet"
        FIELD caract              AS CHAR    FORMAT "x(20)"          label "Caracter°sticas"
        FIELD nr-ord-off-spc      AS INT     FORMAT ">>>>>>>>9"
        FIELD nr-ord-off-grd      AS INT     FORMAT ">>>>>>>>9"
        FIELD letra-bobina        AS CHAR    FORMAT "x(1)" 
        FIELD nr-prim-bob         AS INT     FORMAT ">>>>>>>>9" 
        FIELD largura             AS INT     FORMAT ">>>9"            LABEL "Larg"
        INDEX chave IS PRIMARY UNIQUE estacao ASCENDING.


DEF BUFFER b-tt-estacao FOR tt-estacao.

/* Transfer Definitions */
    DEFINE QUERY br-estacao FOR 
          tt-estacao SCROLLING.
                   

        /* Browse definitions                                                   */
    DEFINE BROWSE br-estacao
      QUERY br-estacao  DISPLAY 
        tt-estacao.estacao         
        tt-estacao.nr-ord-produ  
        tt-estacao.it-codigo     FORMAT "x(17)"
        tt-estacao.largura       
        tt-estacao.Lote          FORMAT "x(10)"
        tt-estacao.nr-pedido    
        tt-estacao.seq-ped
        tt-estacao.nome-abrev    FORMAT "x(13)"
        tt-estacao.cod-depos     FORMAT "x(05)" 
        tt-estacao.cod-localiz   FORMAT "x(12)"
        tt-estacao.gera-pallet   FORMAT "x(1)"  
        tt-estacao.caract        FORMAT "x(34)"

        ENABLE
        tt-estacao.nr-ord-produ 
        tt-estacao.cod-depos
        tt-estacao.cod-localiz
/*        tt-estacao.gera-pallet  */
        tt-estacao.caract        

    /* _UIB-CODE-BLOCK-END */
    &ANALYZE-RESUME
        WITH SEPARATORS SIZE 103 BY 16
             BGCOLOR 15 FONT 1.





/************************************************************************************/

/* Transfer Definitions */

/* Local Variable Definitions ---                                       */ 
def new global shared var c-it-codigo-reserva-jr as char                     no-undo.

def var l-ok                 as logical no-undo. 
def var c-arq-digita         as char    no-undo. 
def var c-terminal           as char    no-undo. 
def var v-cod-pg-mouse-selec as char    no-undo. 
def var v-cod-prog-i-rprun   as char    no-undo. 
def var c-impressora-old     as char    no-undo. 
def var c-arquivo-old        as char    no-undo. 
def var c-destino-old        as char    no-undo. 
def var i-cont               as int     no-undo. 
def var v-cod-prog-gerado    as char    no-undo. 

def var h-acomp            as handle no-undo.
def var v-num-reg-lidos    as int    no-undo.


/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 


/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 

DEFINE VARIABLE i-nr-ord-produ-amg  AS INT       FORMAT ">>>>>>>9"      NO-UNDO.
DEFINE VARIABLE i-nr-ord-specs-amg  AS INT       FORMAT ">>>>>>>9"      NO-UNDO.
DEFINE VARIABLE i-nr-ord-grade-amg  AS INT       FORMAT ">>>>>>>9"      NO-UNDO.

DEFINE VARIABLE i-estacao-ini-amg   AS INT       FORMAT ">>9" INITIAL 0  NO-UNDO.
DEFINE VARIABLE i-estacao-fim-amg   AS INT       FORMAT ">>9" INITIAL 20 NO-UNDO.

DEFINE VARIABLE c-it-codigo-amg     AS CHARACTER FORMAT "X(16)"         NO-UNDO.
DEFINE VARIABLE c-it-codigo-spc     AS CHARACTER FORMAT "X(16)"         NO-UNDO.
DEFINE VARIABLE c-it-codigo-grd     AS CHARACTER FORMAT "X(16)"         NO-UNDO.

DEFINE VARIABLE c-desc-item-amg     AS CHARACTER FORMAT "X(40)"         NO-UNDO.
DEFINE VARIABLE c-desc-item-spc     AS CHARACTER FORMAT "X(40)"         NO-UNDO.
DEFINE VARIABLE c-desc-item-grd     AS CHARACTER FORMAT "X(40)"         NO-UNDO.

DEFINE VARIABLE c-letra-bob         AS CHARACTER FORMAT "X(1)"          NO-UNDO.
DEFINE VARIABLE i-nr-bobina         AS INT       FORMAT ">>>>>>>>9"     NO-UNDO.

DEFINE VARIABLE c-tipo-botao        AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE c-tipo-botao2       AS CHARACTER                        NO-UNDO.


/****************** Definiáao de Vari†veis de Trabalho *********************/ 

DEFINE VARIABLE i-jr              AS INTEGER    NO-UNDO.
DEFINE VARIABLE flag-estacao      AS INTEGER    NO-UNDO.
DEFINE VARIABLE text-string       AS CHARACTER  FORMAT "x(180)".
DEFINE VARIABLE pesq-nr-ord-produ AS INTEGER    NO-UNDO.



/* ********************  Preprocessor Definitions  ******************** */ 

DEFINE VARIABLE c-arquivo-jr AS CHARACTER FORMAT "x(100)" NO-UNDO.

/* ***********************  Control Definitions  ********************** */ 

DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO. 



DEFINE IMAGE IMAGE-1
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-2
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-3
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-4
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-5
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-6
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-7
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-8
    FILENAME "image\im-las"
    SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-10
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
    
DEFINE IMAGE im-pg-imp
    FILENAME "image\im-fldup"
    SIZE 15.72 BY 1.19.
    
DEFINE IMAGE im-pg-par
    FILENAME "image\im-fldup"
    SIZE 15.72 BY 1.19.
    
DEFINE IMAGE im-pg-sel
    FILENAME "image\im-fldup"
    SIZE 20 BY 1.19.

DEFINE IMAGE im-pg-dig
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.19.


DEFINE VARIABLE c-arquivo AS CHARACTER 
VIEW-AS EDITOR MAX-CHARS 256 
SIZE 40 BY 1.00 
BGCOLOR 15  font 2 NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)" INITIAL "Destino"
VIEW-AS TEXT 
SIZE 8.57 BY .62 NO-UNDO. 

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)" INITIAL "Execuá∆o"
VIEW-AS TEXT 
SIZE 10.86 BY .62 NO-UNDO.

DEFINE VARIABLE text-parametro AS CHARACTER FORMAT "X(256)" INITIAL "ParÉmetros de Impress∆o"
VIEW-AS TEXT 
SIZE 24.72 BY .62 NO-UNDO.



DEFINE RECTANGLE RECT-9
EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL
SIZE 108 BY 2.22 
BGCOLOR 7.      

DEFINE RECTANGLE RECT-10 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 108 BY 16.42
BGCOLOR 7.


DEFINE RECTANGLE RECT-11
EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL
SIZE 108 BY 4 
BGCOLOR 7.      


DEFINE BUTTON bt-novo
    IMAGE FILENAME "image\im-add"
    SIZE 4 BY 1.

DEFINE BUTTON bt-cancela
    IMAGE FILENAME "image\im-can"
    SIZE 4 BY 1.

DEFINE BUTTON bt-sai
    IMAGE FILENAME "image\im-exi"
    SIZE 4 BY 1.


DEFINE BUTTON bt-ajuda 
    IMAGE FILENAME "image\im-hel"
    SIZE 4 BY 1.


DEFINE BUTTON bt-cancela2 AUTO-END-KEY 
     LABEL "C&ancelar" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-atualiza AUTO-GO 
     LABEL "&Confirma" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-filtro AUTO-GO 
    IMAGE-UP FILE "image\im-copy":U
    LABEL "&Filtro" 
    SIZE 06 BY 1.0.


DEFINE BUTTON bt-copia-est AUTO-GO 
     LABEL "C¢pia p/pro&x.Estaá∆o" 
     SIZE 20 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-limpa-est AUTO-GO 
     LABEL "Limpa a &Estaá∆o" 
     SIZE 20 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-limpa-tudo AUTO-GO 
     LABEL "&Limpa Tudo" 
     SIZE 20 BY 1
     BGCOLOR 8 .

DEFINE RECTANGLE rt-botoes
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 108 BY 1.42
     BGCOLOR 7.


DEFINE RECTANGLE RECT-1
EDGE-PIXELS 2 GRAPHIC-EDGE 
SIZE 108 BY 1.42 
BGCOLOR 7.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
    bt-novo     AT ROW 1.25 COL 4    HELP "Inclui Novo Registro"
    bt-cancela  AT ROW 1.25 COL 8.5  HELP "Cancela"
    bt-sai      AT ROW 1.25 COL 100  HELP "Encerra Programa"
    bt-ajuda    AT ROW 1.25 COL 105  HELP "Ajuda"


    i-nr-ord-produ-amg LABEL "Ord.Produá∆o"
     at row 2.7 col 14 colon-aligned
     view-as fill-in 
     size 10 by .88
     font 1
   
    c-it-codigo-amg LABEL "Item"
     at row 2.7 col 29 colon-aligned
     view-as fill-in 
     size 17 by .88
     font 1

    c-desc-item-amg NO-LABEL
     at row 2.7 col 48 colon-aligned
     view-as fill-in 
     size 25 by .88
     font 1
   
    i-estacao-ini-amg LABEL "Estaá∆o"
     at row 2.7 col 84 colon-aligned
     view-as fill-in 
     size 04 by .88
     font 1
   
    i-estacao-fim-amg LABEL "AtÇ"
     at row 2.7 col 92 colon-aligned
     view-as fill-in 
     size 04 by .88
     font 1

    bt-filtro AT ROW 2.65 COL 100

    c-letra-bob LABEL "Letra do Nr.Bob"
     at row 3.7 col 14 colon-aligned
     view-as fill-in 
     size 04 by .88
     font 1  

    i-nr-bobina LABEL "Seq.1a.Bob"
     at row 3.7 col 29 colon-aligned
     view-as fill-in 
     size 17 by .88
     font 1
    
    br-estacao AT ROW 5 COL 4 

    bt-copia-est  AT ROW 21.7 COL 4
    bt-limpa-est  AT ROW 23.0 COL 4
    bt-limpa-tudo AT ROW 24.3 COL 4

    i-nr-ord-specs-amg LABEL "Ord.Off Specs"
     at row 21.7 col 35 colon-aligned
     view-as fill-in 
     size 17 by .88
     font 1
   
    c-it-codigo-spc LABEL "Item"
     at row 21.7 col 58 colon-aligned
     view-as fill-in 
     size 17 by .88
     font 1

    c-desc-item-spc NO-LABEL
     at row 21.7 col 80 colon-aligned
     view-as fill-in 
     size 25 by .88
     font 1

    i-nr-ord-grade-amg LABEL "Ord.Off Grade"
     at row 23.0 col 35 colon-aligned
     view-as fill-in 
     size 17 by .88
     font 1

    c-it-codigo-grd LABEL "Item"
     at row 23 col 58 colon-aligned
     view-as fill-in 
     size 17 by .88
     font 1

    c-desc-item-grd NO-LABEL
     at row 23 col 80 colon-aligned
     view-as fill-in 
     size 25 by .88
     font 1

    bt-atualiza  AT ROW 26   COL 4
    bt-cancela2  AT ROW 26   COL 20

    RECT-1  AT ROW 1.05 COL 2    
    RECT-9  AT ROW 2.55 COL 2    
    RECT-10 AT ROW 4.8  COL 2  
    RECT-11 AT ROW 21.5 COL 2  

    rt-botoes AT ROW 25.8 COL 2

    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
    SIDE-LABELS NO-UNDERLINE THREE-D
    AT COL 1 ROW 1 FONT 1 
    SIZE 110 BY 26.3. 

/* ******** Acerto da posiá∆o dos labels e tamanho dos radio-set ******* */

/* *************************  Create Window  ************************** */

CREATE WINDOW C-Win ASSIGN
   HIDDEN             = YES
   TITLE              = "Setup de Produá∆o no Corte/Recorte/Metalizaá∆o"
   HEIGHT             = 27
   WIDTH              = 111
   MAX-HEIGHT         = 27
   MAX-WIDTH          = 111
   VIRTUAL-HEIGHT     = 27
   VIRTUAL-WIDTH      = 111
   RESIZE             = yes
   SCROLL-BARS        = no
   STATUS-AREA        = yes
   BGCOLOR            = ?
   FGCOLOR            = ?
   KEEP-FRAME-Z-ORDER = yes
   THREE-D            = yes
   MESSAGE-AREA       = no
   SENSITIVE          = yes.

/* ***************  Runtime Attributes and UIB Settings  ************** */


IF SESSION:DISPLAY-TYPE = "GUI" AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* ************************* Included-Libraries *********************** */


define variable wh-pesquisa             as handle               no-undo.
define variable c-arq-old               as char                 no-undo.

def new shared var c-nom-prog-upc-mg97  as char format "x(50)" no-undo.
def new shared var c-nom-prog-appc-mg97 as char format "x(50)" no-undo.
def new shared var c-nom-prog-dpc-mg97  as char format "x(50)" no-undo.
def new shared var c-programa-mg97      as char format "x(08)" no-undo.
def new shared var c-versao-mg97        as char format "x(08)" no-undo.

define new shared variable c-imp-old               as char                 no-undo.
define new shared variable c-arq-old-batch         as char                 no-undo.


PROCEDURE pi-trata-state :
  DEFINE INPUT PARAMETER p-issuer-hdl   AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state        AS CHAR   NO-UNDO.
  
  case entry(1, p-state, '|'):
  end.
END PROCEDURE.

on "close" of this-procedure do:
    run disable_UI.
    apply 'choose' TO bt-sai IN FRAME f-relat.

end.

/* ************************  Control Triggers  ************************ */


 

ON MOUSE-SELECT-DBLCLICK OF br-estacao IN FRAME f-relat
DO:

 

 IF true or br-estacao:NUM-SELECTED-ROWS > 0 THEN DO ON ERROR UNDO, RETURN NO-APPLY:
     for first tt-estacao 
         where tt-estacao.estacao = int(tt-estacao.estacao:screen-value 
               in browse br-estacao) exclusive-lock:
     end.
     
     if avail tt-estacao then do:                                     
         if tt-estacao.gera-pallet:screen-value in browse br-estacao = "*":U 
         and tt-estacao.gera-pallet =  "*" then do:
             assign tt-estacao.gera-pallet:screen-value in browse br-estacao = "":U
                    tt-estacao.gera-pallet = "".
         end.
         else do:
             assign tt-estacao.gera-pallet:screen-value in browse br-estacao =  "*":U
                    tt-estacao.gera-pallet =  "*".
         end.
         apply "LEAVE":U to browse br-estacao.
     end.
  END.   
END.

ON F5 OF i-nr-ord-produ-amg IN FRAME f-relat /* Ordem de Produá∆o */
DO:

     ASSIGN pesq-nr-ord-produ = 999999999.
     RUN pi-pesquisa-ordem.

     IF pesq-nr-ord-produ <> 999999999 THEN
         ASSIGN i-nr-ord-produ-amg:SCREEN-VALUE IN FRAME f-relat = string(pesq-nr-ord-produ).

END.


ON MOUSE-SELECT-DBLCLICK OF i-nr-ord-produ-amg IN FRAME f-relat /* Ordem de Produá∆o */
DO:
  APPLY "f5" TO SELF.
END.


ON F5 OF tt-estacao.nr-ord-produ IN BROWSE br-estacao /* Ordem de Produá∆o no Borwse */
DO:

     ASSIGN pesq-nr-ord-produ = 999999999.
     RUN pi-pesquisa-ordem.

     IF pesq-nr-ord-produ <> 999999999 THEN
         ASSIGN tt-estacao.nr-ord-produ:SCREEN-VALUE IN BROWSE br-estacao = string(pesq-nr-ord-produ).

END.


ON MOUSE-SELECT-DBLCLICK OF tt-estacao.nr-ord-produ IN BROWSE br-estacao  /* Ordem de Produá∆o Browse*/
DO:
  APPLY "f5" TO SELF.
END.


ON F5 OF i-nr-ord-specs-amg IN FRAME f-relat /* Ordem de Off Specs */
DO:

    ASSIGN pesq-nr-ord-produ = 999999999.

    RUN pi-pesquisa-ordem.

    IF pesq-nr-ord-produ <> 999999999 THEN
        ASSIGN i-nr-ord-specs-amg:SCREEN-VALUE IN FRAME f-relat = string(pesq-nr-ord-produ).


END.


ON MOUSE-SELECT-DBLCLICK OF i-nr-ord-specs-amg IN FRAME f-relat /* Ordem de Off Specs */
DO:
  APPLY "f5" TO SELF.
END.


ON F5 OF i-nr-ord-grade-amg IN FRAME f-relat /* Ordem de Off Grade */
DO:

    ASSIGN pesq-nr-ord-produ = 999999999.

    RUN pi-pesquisa-ordem.

    IF pesq-nr-ord-produ <> 999999999 THEN
        ASSIGN i-nr-ord-grade-amg:SCREEN-VALUE IN FRAME f-relat = string(pesq-nr-ord-produ).


END.


ON MOUSE-SELECT-DBLCLICK OF i-nr-ord-grade-amg IN FRAME f-relat /* Ordem de Off Grade */
DO:
  APPLY "f5" TO SELF.
END.

ON 'leave':U OF   i-nr-bobina  IN FRAME f-relat
DO:
     FOR EACH tt-estacao .
     
     
            if  tt-estacao.nr-ord-produ <> 0 and tt-estacao.estacao <> 0 then do:
       
                ASSIGN c-letra-bob  = c-letra-bob:SCREEN-VALUE IN FRAME f-relat
                 i-nr-bobina  = int(i-nr-bobina:SCREEN-VALUE IN FRAME f-relat).
              FIND FIRST lote-prod WHERE lote-prod.lote = c-letra-bob:SCREEN-VALUE IN FRAME f-relat + i-nr-bobina:SCREEN-VALUE IN FRAME f-relat NO-LOCK NO-ERROR.
       
              DO WHILE AVAIL lote-prod:
                    i-nr-bobina = i-nr-bobina + 1.
                    i-nr-bobina:SCREEN-VALUE IN FRAME f-relat = STRING(i-nr-bobina).
                    FIND FIRST lote-prod WHERE lote-prod.lote = c-letra-bob:SCREEN-VALUE IN FRAME f-relat + i-nr-bobina:SCREEN-VALUE IN FRAME f-relat NO-LOCK NO-ERROR.
       
              END.
       
              tt-estacao.Lote         = c-letra-bob:SCREEN-VALUE IN FRAME f-relat + i-nr-bobina:SCREEN-VALUE IN FRAME f-relat  .
              i-nr-bobina = i-nr-bobina + 1.
              i-nr-bobina:SCREEN-VALUE IN FRAME f-relat = STRING(i-nr-bobina).
            end.
            else do:
                 tt-estacao.Lote  = "".
            
            end.  
     END.

     CLOSE query br-estacao.
       open query br-estacao for each tt-estacao.
       
       if num-results("br-estacao") > 0 THEN DO:
          get current br-estacao.
       END.
       
       apply 'entry' to tt-estacao.nr-ord-produ in browse br-estacao. 

 



END.
ON CHOOSE OF bt-copia-est IN FRAME f-relat /* Copia Estaá∆o */
DO:

    if num-results("br-estacao") > 0 THEN DO:
       get current br-estacao.

       IF tt-estacao.estacao = 20 OR 
          (tt-estacao.estacao + 1) > INPUT FRAME f-relat i-estacao-fim-amg THEN DO:

        run utp/ut-msgs.p (input "show":U, input 17006, "N∆o Ç poss°vel copiar estaá∆o acima de 20, ou erro no limite de estaá‰es").
        RETURN NO-APPLY.

       END.

       FIND FIRST b-tt-estacao WHERE
           b-tt-estacao.estacao = (tt-estacao.estacao + 1)
           NO-ERROR.

       IF NOT AVAIL b-tt-estacao THEN
           CREATE b-tt-estacao.

       ASSIGN c-letra-bob  = c-letra-bob:SCREEN-VALUE IN FRAME f-relat
          i-nr-bobina  = int(i-nr-bobina:SCREEN-VALUE IN FRAME f-relat).
       FIND FIRST lote-prod WHERE lote-prod.lote = c-letra-bob:SCREEN-VALUE IN FRAME f-relat + i-nr-bobina:SCREEN-VALUE IN FRAME f-relat NO-LOCK NO-ERROR.

       DO WHILE AVAIL lote-prod:
             i-nr-bobina = i-nr-bobina + 1.
             i-nr-bobina:SCREEN-VALUE IN FRAME f-relat = STRING(i-nr-bobina).
             FIND FIRST lote-prod WHERE lote-prod.lote = c-letra-bob:SCREEN-VALUE IN FRAME f-relat + i-nr-bobina:SCREEN-VALUE IN FRAME f-relat NO-LOCK NO-ERROR.

       END.


       ASSIGN b-tt-estacao.estacao      = (tt-estacao.estacao + 1)    
              b-tt-estacao.nr-ord-produ = tt-estacao.nr-ord-produ
              b-tt-estacao.it-codigo    = tt-estacao.it-codigo   
              b-tt-estacao.Lote         = c-letra-bob:SCREEN-VALUE IN FRAME f-relat + i-nr-bobina:SCREEN-VALUE IN FRAME f-relat  
              b-tt-estacao.nr-pedido    = tt-estacao.nr-pedido   
              b-tt-estacao.seq-ped      = tt-estacao.seq-ped     
              b-tt-estacao.nome-abrev   = tt-estacao.nome-abrev  
              b-tt-estacao.largura      = tt-estacao.largura
              b-tt-estacao.cod-depos    = tt-estacao.cod-depos   
              b-tt-estacao.cod-localiz  = tt-estacao.cod-localiz 
              b-tt-estacao.gera-pallet  = tt-estacao.gera-pallet 
              b-tt-estacao.carac        = tt-estacao.carac .

       ASSIGN i-nr-bobina = i-nr-bobina + 1
              i-nr-bobina:SCREEN-VALUE IN FRAME f-relat = STRING(i-nr-bobina).
             


       CLOSE query br-estacao.
       open query br-estacao for each tt-estacao.
       
       if num-results("br-estacao") > 0 THEN DO:
          get current br-estacao.
       END.
       
       apply 'entry' to tt-estacao.nr-ord-produ in browse br-estacao. 

    END.

END.


ON CHOOSE OF bt-limpa-est IN FRAME f-relat /* Limpa a Estaá∆o */
DO:

    if num-results("br-estacao") > 0 THEN DO:
       get current br-estacao.

       FIND FIRST b-tt-estacao WHERE
           b-tt-estacao.estacao = (tt-estacao.estacao)
           NO-ERROR.

       IF AVAIL b-tt-estacao THEN

           ASSIGN b-tt-estacao.nr-ord-produ  = 0
                  b-tt-estacao.it-codigo     = ""
                  b-tt-estacao.lote          = ""
                  b-tt-estacao.nr-pedido     = 0
                  b-tt-estacao.seq-ped       = 0
                  b-tt-estacao.largura       = 0
                  b-tt-estacao.nome-abrev    = ""
                  b-tt-estacao.cod-depos     = ""
                  b-tt-estacao.cod-localiz   = ""
                  b-tt-estacao.gera-pallet   = ""
                  b-tt-estacao.carac         = "".

       CLOSE query br-estacao.
       open query br-estacao for each tt-estacao.
       
       if num-results("br-estacao") > 0 THEN DO:
          get current br-estacao.
       END.
       
       apply 'entry' to tt-estacao.nr-ord-produ in browse br-estacao. 

    END.

END.


ON CHOOSE OF bt-limpa-tudo IN FRAME f-relat /* Limpa todas Estaá‰es */
DO:

    FOR EACH tt-estacao.

        ASSIGN tt-estacao.nr-ord-produ  = 0
               tt-estacao.it-codigo     = ""
               tt-estacao.lote          = ""
               tt-estacao.nr-pedido     = 0
               tt-estacao.seq-ped       = 0
               tt-estacao.largura       = 0
               tt-estacao.nome-abrev    = ""
               tt-estacao.cod-depos     = ""
               tt-estacao.cod-localiz   = ""
               tt-estacao.gera-pallet   = ""
               tt-estacao.carac         = "".

    END.

    CLOSE query br-estacao.
    open query br-estacao for each tt-estacao.
    
    if num-results("br-estacao") > 0 THEN DO:
       get current br-estacao.
    END.
    
    apply 'entry' to tt-estacao.nr-ord-produ in browse br-estacao. 

END.


ON CHOOSE OF bt-filtro IN FRAME f-relat /* Filtro de Estaá‰es */
DO:

    IF INPUT FRAME f-relat i-estacao-ini-amg = 0  OR
       INPUT FRAME f-relat i-estacao-fim-amg > 20 OR
      (INPUT FRAME f-relat i-estacao-fim-amg < INPUT FRAME f-relat i-estacao-ini-amg) THEN DO:

        run utp/ut-msgs.p (input "show":U, input 17006, "Erro no N£mero de Estaá‰es").
        APPLY "entry" TO i-estacao-ini-amg IN FRAME f-relat.
        RETURN NO-APPLY.

    END.

    FIND FIRST ord-prod WHERE
        ord-prod.nr-ord-produ = INPUT FRAME f-relat i-nr-ord-produ-amg
        NO-LOCK NO-ERROR.

    IF NOT AVAIL ord-prod THEN DO:

       run utp/ut-msgs.p (input "show":U, input 17006, "Ordem de Produá∆o N∆o Existe").
       APPLY "entry" TO i-nr-ord-produ-amg IN FRAME f-relat.
       RETURN NO-APPLY.

    END.


    IF ord-prod.estado > 6 THEN DO:
    
       run utp/ut-msgs.p (input "show":U, input 17006, "Ord.de Produá∆o J† Encerrado ou Terminada").
       APPLY "entry" TO i-nr-ord-produ-amg IN FRAME f-relat.
       RETURN NO-APPLY.  

    END.

    FOR EACH tt-estacao.
        DELETE tt-estacao.
    END.

    CLOSE query br-estacao.
    open query br-estacao for each tt-estacao.

    ASSIGN i-jr = INPUT FRAME f-relat i-estacao-ini-amg.

    DO WHILE i-jr <= INPUT FRAME f-relat i-estacao-fim-amg:

        CREATE tt-estacao.

        ASSIGN c-letra-bob  = c-letra-bob:SCREEN-VALUE IN FRAME f-relat
         i-nr-bobina  = int(i-nr-bobina:SCREEN-VALUE IN FRAME f-relat).
      FIND FIRST lote-prod WHERE lote-prod.lote = c-letra-bob:SCREEN-VALUE IN FRAME f-relat + i-nr-bobina:SCREEN-VALUE IN FRAME f-relat NO-LOCK NO-ERROR.

      DO WHILE AVAIL lote-prod:
            i-nr-bobina = i-nr-bobina + 1.
            i-nr-bobina:SCREEN-VALUE IN FRAME f-relat = STRING(i-nr-bobina).
            FIND FIRST lote-prod WHERE lote-prod.lote = c-letra-bob:SCREEN-VALUE IN FRAME f-relat + i-nr-bobina:SCREEN-VALUE IN FRAME f-relat NO-LOCK NO-ERROR.

      END.


        IF ord-prod.cod-refer <> "" THEN DO:
             FIND var-result 
                     WHERE var-result.item-cotacao = ord-prod.it-codigo
                       AND var-result.nr-estrut    = INT(ord-prod.cod-refer)
                       AND var-result.nome-var     = "LARGURA"  NO-LOCK NO-ERROR.
    
                 IF AVAIL var-result then
                     ASSIGN   tt-estacao.largura = int(var-result.des-result).


        END.


        ASSIGN tt-estacao.estacao = i-jr
               tt-estacao.nr-ord-produ = ord-prod.nr-ord-produ
               tt-estacao.it-codigo    = ord-prod.it-codigo
               tt-estacao.Lote         = c-letra-bob:SCREEN-VALUE IN FRAME f-relat + i-nr-bobina:SCREEN-VALUE IN FRAME f-relat  
               tt-estacao.nr-pedido    = int(ord-prod.nr-pedido)
               tt-estacao.seq-ped      = ord-prod.nr-sequencia
               tt-estacao.nome-abrev   = ord-prod.nome-abrev
               tt-estacao.cod-depos    = IF ord-prod.cod-depos = ""  THEN "pro" ELSE ord-prod.cod-depos
               tt-estacao.cod-localiz  = ""
               tt-estacao.gera-pallet  = "*"
               tt-estacao.carac        = "".

        ASSIGN i-nr-bobina = i-nr-bobina + 1
               i-nr-bobina:SCREEN-VALUE IN FRAME f-relat = STRING(i-nr-bobina).



        ASSIGN i-jr = i-jr + 1.

    END.

    CLOSE query br-estacao.
    open query br-estacao for each tt-estacao.
    
    if num-results("br-estacao") > 0 THEN DO:
       get current br-estacao.
    END.
/*
    apply 'entry' to tt-estacao.nr-ord-produ in browse br-estacao. 
*/

END.

on LEAVE OF i-estacao-fim-amg in frame f-relat do:

    apply 'choose' to bt-filtro in FRAME f-relat. 

END.

on "entry" OF i-nr-ord-produ-amg in frame f-relat do:

         
END.


on LEAVE OF i-nr-ord-produ-amg in frame f-relat do:

    FIND FIRST ord-prod WHERE
        ord-prod.nr-ord-produ = INPUT FRAME f-relat i-nr-ord-produ-amg
        NO-LOCK NO-ERROR.

    IF AVAIL ord-prod THEN DO:

        FIND FIRST ITEM WHERE
            ITEM.it-codigo = ord-prod.it-codigo
            NO-LOCK NO-ERROR.

        IF AVAIL ITEM THEN 
           ASSIGN c-it-codigo-amg:SCREEN-VALUE IN FRAME f-relat = ord-prod.it-codigo
                  c-desc-item-amg:SCREEN-VALUE IN FRAME f-relat = item.desc-item.
        ELSE
           ASSIGN c-it-codigo-amg:SCREEN-VALUE IN FRAME f-relat = ""
                  c-desc-item-amg:SCREEN-VALUE IN FRAME f-relat = "".

    END.

    ELSE
        ASSIGN i-nr-ord-produ-amg:SCREEN-VALUE IN FRAME f-relat = ""
               c-it-codigo-amg:SCREEN-VALUE IN FRAME f-relat    = ""
               c-desc-item-amg:SCREEN-VALUE IN FRAME f-relat    = "".



END.                                                

on LEAVE OF i-nr-ord-specs-amg in frame f-relat do:

    FIND FIRST ord-prod WHERE
        ord-prod.nr-ord-produ = INPUT FRAME f-relat i-nr-ord-specs-amg
        NO-LOCK NO-ERROR.

    IF AVAIL ord-prod THEN DO:

        FIND FIRST ITEM WHERE
            ITEM.it-codigo = ord-prod.it-codigo
            NO-LOCK NO-ERROR.

        IF AVAIL ITEM AND item.ge-codigo = 47 THEN 
           ASSIGN c-it-codigo-spc:SCREEN-VALUE IN FRAME f-relat = ord-prod.it-codigo
                  c-desc-item-spc:SCREEN-VALUE IN FRAME f-relat = item.desc-item.
        ELSE
           ASSIGN i-nr-ord-specs-amg:SCREEN-VALUE IN FRAME f-relat = ""
                  c-it-codigo-spc:SCREEN-VALUE IN FRAME f-relat = ""
                  c-desc-item-spc:SCREEN-VALUE IN FRAME f-relat = "".

    END.

    ELSE
        ASSIGN i-nr-ord-specs-amg:SCREEN-VALUE IN FRAME f-relat = ""
               c-it-codigo-spc:SCREEN-VALUE IN FRAME f-relat    = ""
               c-desc-item-spc:SCREEN-VALUE IN FRAME f-relat    = "".

END.                                                


on LEAVE OF i-nr-ord-grade-amg in frame f-relat do:

    FIND FIRST ord-prod WHERE
        ord-prod.nr-ord-produ = INPUT FRAME f-relat i-nr-ord-grade-amg
        NO-LOCK NO-ERROR.

    IF AVAIL ord-prod THEN DO:

        FIND FIRST ITEM WHERE
            ITEM.it-codigo = ord-prod.it-codigo
            NO-LOCK NO-ERROR.

        IF AVAIL ITEM AND item.ge-codigo = 47 THEN 
           ASSIGN c-it-codigo-grd:SCREEN-VALUE IN FRAME f-relat = ord-prod.it-codigo
                  c-desc-item-grd:SCREEN-VALUE IN FRAME f-relat = item.desc-item.
        ELSE
           ASSIGN i-nr-ord-grade-amg:SCREEN-VALUE IN FRAME f-relat = ""
                  c-it-codigo-grd:SCREEN-VALUE IN FRAME f-relat = ""
                  c-desc-item-grd:SCREEN-VALUE IN FRAME f-relat = "".

    END.


    ELSE
        ASSIGN i-nr-ord-grade-amg:SCREEN-VALUE IN FRAME f-relat = ""
               c-it-codigo-grd:SCREEN-VALUE IN FRAME f-relat    = ""
               c-desc-item-grd:SCREEN-VALUE IN FRAME f-relat    = "".


END.                                                

on LEAVE OF i-estacao-ini-amg in frame f-relat do:


END.                                                

on LEAVE OF tt-estacao.nr-ord-produ in BROWSE br-estacao do:

    IF INPUT BROWSE br-estacao tt-estacao.nr-ord-produ <> 0 THEN DO:

        FIND FIRST ord-prod WHERE
            ord-prod.nr-ord-produ = INPUT BROWSE br-estacao tt-estacao.nr-ord-produ
            NO-LOCK NO-ERROR.
        
        IF NOT AVAIL ord-prod THEN DO:
        
           run utp/ut-msgs.p (input "show":U, input 17006, "Ordem de Produá∆o N∆o Existe").
           RETURN NO-APPLY.
        
        END.
        
        IF ord-prod.estado > 6 THEN DO:
        
           run utp/ut-msgs.p (input "show":U, input 17006, "Ord.de Produá∆o J† Encerrado ou Terminada").
           RETURN NO-APPLY.  
        
        END.


        IF ord-prod.cod-refer <> "" THEN DO:
             FIND var-result 
                     WHERE var-result.item-cotacao = ord-prod.it-codigo
                       AND var-result.nr-estrut    = INT(ord-prod.cod-refer)
                       AND var-result.nome-var     = "LARGURA"  NO-LOCK NO-ERROR.
    
                 IF AVAIL var-result then
                     ASSIGN   tt-estacao.largura:SCREEN-VALUE IN BROWSE br-estacao = string(int(var-result.des-result)).


        END.
        ELSE DO:
            ASSIGN   tt-estacao.largura:SCREEN-VALUE IN BROWSE br-estacao = "".
                
        END.



        ASSIGN tt-estacao.it-codigo:SCREEN-VALUE IN BROWSE br-estacao    = ord-prod.it-codigo
               tt-estacao.nr-pedido:SCREEN-VALUE IN BROWSE br-estacao    = ord-prod.nr-pedido
               tt-estacao.seq-ped:SCREEN-VALUE IN BROWSE br-estacao      = string(ord-prod.nr-sequencia)
               tt-estacao.nome-abrev:SCREEN-VALUE IN BROWSE br-estacao   = ord-prod.nome-abrev
               tt-estacao.cod-depos:SCREEN-VALUE IN BROWSE br-estacao    = IF ord-prod.cod-depos = "" THEN "pro" ELSE ord-prod.cod-depos
               tt-estacao.cod-localiz:SCREEN-VALUE IN BROWSE br-estacao  = "".
             
    END.

END.

on "entry" OF i-estacao-fim-amg in frame f-relat do:


END.


ON CHOOSE OF bt-cancela2 IN FRAME f-relat
DO:


   RUN pi-disable-campos.
   RUN pi-disable-bt-grava.
   RUN pi-enable-outros-botoes.
   RUN pi-limpa-campos.

   APPLY "choose" TO bt-sai IN FRAME f-relat.

END.


ON CHOOSE OF bt-atualiza IN FRAME f-relat 
DO:

    FIND FIRST tt-estacao NO-LOCK NO-ERROR.
        
    IF NOT AVAIL tt-estacao THEN DO:

       run utp/ut-msgs.p (input "show":U, input 17006, "N∆o Informado Estaá‰es").
       APPLY "entry" TO i-nr-ord-produ-amg IN FRAME f-relat.
       RETURN NO-APPLY.  

    END.

    ASSIGN i-estacao-ini-amg:SCREEN-VALUE IN FRAME f-relat = STRING (tt-estacao.estacao). 

    FIND LAST tt-estacao NO-LOCK NO-ERROR.
        
    IF NOT AVAIL tt-estacao THEN DO:

       run utp/ut-msgs.p (input "show":U, input 17006, "N∆o Informado Estaá‰es").
       APPLY "entry" TO i-nr-ord-produ-amg IN FRAME f-relat.
       RETURN NO-APPLY.  

    END.

    ASSIGN i-estacao-fim-amg:SCREEN-VALUE IN FRAME f-relat = STRING (tt-estacao.estacao). 


    FOR EACH tt-estacao 
        where tt-estacao.nr-ord-produ <> 0 NO-LOCK.

        FIND FIRST ord-prod WHERE
            ord-prod.nr-ord-produ = tt-estacao.nr-ord-produ    
            NO-LOCK NO-ERROR.
        
        IF NOT AVAIL ord-prod THEN DO:
        
           run utp/ut-msgs.p (input "show":U, input 2, "Ord.de Produá∆o").
           APPLY "entry" TO i-nr-ord-produ-amg IN FRAME f-relat.
           RETURN NO-APPLY.  
        
        END.
        
        
        IF ord-prod.estado > 6 THEN DO:
        
           run utp/ut-msgs.p (input "show":U, input 17006, "Ord.de Produá∆o J† Encerrado ou Terminada").
           APPLY "entry" TO i-nr-ord-produ-amg IN FRAME f-relat.
           RETURN NO-APPLY.  
        
        END.
        
        IF ord-prod.it-codigo MATCHES "*OG" OR 
           ord-prod.it-codigo MATCHES "*OS" THEN DO:
           run utp/ut-msgs.p (input "show":U, input 17006, "Na estaá∆o somente Ç permitido Prime e Estoque!~~Ordens de Off grade e Off Spec devem ser informados nos campos Ord.Off Grade e Ord. Off Spec. Isto permitir† rastrear a ordem prime/estoque que originou o material off grade/spec.").           
           APPLY "entry" TO i-nr-ord-produ-amg IN FRAME f-relat.
           RETURN NO-APPLY.  
        END.
        
        FIND FIRST item where
           item.it-codigo = tt-estacao.it-codigo
           NO-LOCK NO-ERROR.
        
        IF NOT AVAIL item THEN do:
        
                run utp/ut-msgs.p (input "show":U, input 2, "Item").
                APPLY "entry" TO i-nr-ord-produ-amg IN FRAME f-relat.
                RETURN NO-APPLY.
        
        END. 

        FIND FIRST deposito WHERE 
            deposito.cod-depos = tt-estacao.cod-depos
            NO-LOCK NO-ERROR.

        IF NOT AVAIL deposito OR
           deposito.ind-dep-cq = YES or
           (deposito.cod-depos  = "ARC" and item.ge-codigo = 46) THEN DO:

            run utp/ut-msgs.p (input "show":U, input 17006, "N∆o Existe Dep¢sito, ou Dep¢sito Marcado para CQ").
            APPLY "entry" TO i-nr-ord-produ-amg IN FRAME f-relat.
            RETURN NO-APPLY.  

        END.

        FIND FIRST localizacao WHERE 
            localizacao.cod-localiz = tt-estacao.cod-localiz
            NO-LOCK NO-ERROR.

        IF NOT AVAIL localizacao THEN DO:

            run utp/ut-msgs.p (input "show":U, input 17006, "Localizaá∆o N∆o Existe").
            APPLY "entry" TO i-nr-ord-produ-amg IN FRAME f-relat.
            RETURN NO-APPLY.  

        END.

    END.

    ASSIGN i-nr-ord-specs-amg   =  INT(i-nr-ord-specs-amg:SCREEN-VALUE IN FRAME f-relat)
           c-it-codigo-spc      =  c-it-codigo-spc:SCREEN-VALUE IN FRAME f-relat.

    IF i-nr-ord-specs-amg <> 0 THEN DO:

        FIND FIRST ord-prod WHERE
            ord-prod.nr-ord-produ = i-nr-ord-specs-amg    
            NO-LOCK NO-ERROR.
        
        IF NOT AVAIL ord-prod THEN DO:
        
           run utp/ut-msgs.p (input "show":U, input 2, "Ord.de Off Specs").
           APPLY "entry" TO i-nr-ord-specs-amg IN FRAME f-relat.
           RETURN NO-APPLY.  
        
        END.
        
        
        IF ord-prod.estado > 6 THEN DO:
        
           run utp/ut-msgs.p (input "show":U, input 17006, "Ord.de Off Specs J† Encerrado ou Terminada").
           APPLY "entry" TO i-nr-ord-specs-amg IN FRAME f-relat.
           RETURN NO-APPLY.  
        
        END.
        
        
        FIND FIRST item where
           item.it-codigo = c-it-codigo-spc
           NO-LOCK NO-ERROR.
        
        IF NOT AVAIL item THEN do:
        
                run utp/ut-msgs.p (input "show":U, input 2, "Item Off Specs").
                APPLY "entry" TO i-nr-ord-specs-amg IN FRAME f-relat.
                RETURN NO-APPLY.
        
        END.

    END.

    ASSIGN i-nr-ord-grade-amg   =  INT(i-nr-ord-grade-amg:SCREEN-VALUE IN FRAME f-relat)
           c-it-codigo-grd      =  c-it-codigo-grd:SCREEN-VALUE IN FRAME f-relat.

    IF i-nr-ord-grade-amg <> 0 THEN DO:

        FIND FIRST ord-prod WHERE
            ord-prod.nr-ord-produ = i-nr-ord-grade-amg    
            NO-LOCK NO-ERROR.
        
        IF NOT AVAIL ord-prod THEN DO:
        
           run utp/ut-msgs.p (input "show":U, input 2, "Ord.de Off Grade").
           APPLY "entry" TO i-nr-ord-grade-amg IN FRAME f-relat.
           RETURN NO-APPLY.  
        
        END.
        
        IF ord-prod.estado > 6 THEN DO:
        
           run utp/ut-msgs.p (input "show":U, input 17006, "Ord.de Off Grade J† Encerrado ou Terminada").
           APPLY "entry" TO i-nr-ord-grade-amg IN FRAME f-relat.
           RETURN NO-APPLY.  
        
        END.
        
        FIND FIRST item where
           item.it-codigo = c-it-codigo-grd
           NO-LOCK NO-ERROR.
        
        IF NOT AVAIL item THEN do:
        
                run utp/ut-msgs.p (input "show":U, input 2, "Item Off Grade").
                APPLY "entry" TO i-nr-ord-grade-amg IN FRAME f-relat.
                RETURN NO-APPLY.
        
        END.

    END.


    ASSIGN c-letra-bob  = c-letra-bob:SCREEN-VALUE IN FRAME f-relat
           i-nr-bobina  = int(i-nr-bobina:SCREEN-VALUE IN FRAME f-relat).

    IF c-letra-bob = "" THEN DO:
        
       run utp/ut-msgs.p (input "show":U, input 17006, "Falta Informar Letra da Bobina").
       APPLY "entry" TO i-nr-ord-produ-amg IN FRAME f-relat.
       RETURN NO-APPLY.  

    END.

    IF i-nr-bobina = 0 THEN DO:
        
       run utp/ut-msgs.p (input "show":U, input 17006, "Falta Nr.da Primeira Bobina").
       APPLY "entry" TO i-nr-ord-produ-amg IN FRAME f-relat.
       RETURN NO-APPLY.  

    END.

    FIND FIRST tt-estacao WHERE
        tt-estacao.estacao = 0 
        NO-ERROR.

    IF NOT AVAIL tt-estacao THEN
        CREATE tt-estacao.

    ASSIGN tt-estacao.estacao = 0
           tt-estacao.letra-bobina   = c-letra-bob
           tt-estacao.nr-prim-bob    = i-nr-bobina
           tt-estacao.nr-ord-off-spc = i-nr-ord-specs-amg
           tt-estacao.nr-ord-off-grd = i-nr-ord-grade-amg.

    OUTPUT TO value(session:temp-directory + "essf0021.txt").

    FOR EACH tt-estacao NO-LOCK.

        IF tt-estacao.estacao = 0 THEN DO:

            PUT UNFORMATTED 
                tt-estacao.estacao        AT 01
                tt-estacao.nr-ord-off-spc AT 10
                tt-estacao.nr-ord-off-grd AT 20
                tt-estacao.letra-bobina   AT 30
                tt-estacao.nr-prim-bob    AT 40 SKIP.

        END.

        IF tt-estacao.estacao <> 0 THEN DO:

            PUT UNFORMATTED
                tt-estacao.estacao        AT 01
                tt-estacao.nr-ord-produ   AT 10
                tt-estacao.it-codigo      AT 20
                tt-estacao.nr-pedido      AT 40
                tt-estacao.seq-ped        AT 50 
                tt-estacao.nome-abrev     AT 60
                tt-estacao.cod-depos      AT 80
                tt-estacao.cod-localiz    AT 90
                IF tt-estacao.gera-pallet <> "*" AND trim(tt-estacao.gera-pallet) <> ""  THEN "" ELSE tt-estacao.gera-pallet   AT 100
                tt-estacao.lote AT 103 
                tt-estacao.largura AT 114
                    SKIP.

        END.

    END.

    OUTPUT CLOSE.

    APPLY "choose" TO bt-sai IN FRAME f-relat.

END.


ON ENTER OF br-estacao in frame f-relat
ANYWHERE
DO:
  apply 'tab' to self.

END.


ON ROW-LEAVE OF br-estacao in frame f-relat
DO:
    /* ê aqui que a gravaá∆o da linha da temp-table ? efetivada.
       PorÇm as validaá‰es dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment†rio */   
    
    if br-estacao:NEW-ROW in frame f-relat then 
    do transaction on error undo, return no-apply:
        create tt-estacao.  
        assign INPUT BROWSE br-estacao tt-estacao.estacao       
               input browse br-estacao tt-estacao.nr-ord-produ  
               INPUT BROWSE br-estacao tt-estacao.it-codigo     
               INPUT BROWSE br-estacao tt-estacao.lote     
               INPUT BROWSE br-estacao tt-estacao.nr-pedido     
               input browse br-estacao tt-estacao.seq-ped       
               input browse br-estacao tt-estacao.nome-abrev       
               input browse br-estacao tt-estacao.largura       
               input browse br-estacao tt-estacao.cod-depos     
               input browse br-estacao tt-estacao.cod-localiz   
               input browse br-estacao tt-estacao.gera-pallet   
               input browse br-estacao tt-estacao.caract.        
               br-estacao:CREATE-RESULT-LIST-ENTRY() in frame f-relat.
    end.
    else do transaction on error undo, return no-apply:
        
        IF AVAIL tt-estacao THEN
            assign INPUT BROWSE br-estacao tt-estacao.estacao       
                   input browse br-estacao tt-estacao.nr-ord-produ  
                   INPUT BROWSE br-estacao tt-estacao.it-codigo     
                   INPUT BROWSE br-estacao tt-estacao.lote     
                   INPUT BROWSE br-estacao tt-estacao.nr-pedido     
                   input browse br-estacao tt-estacao.seq-ped       
                   input browse br-estacao tt-estacao.nome-abrev       
                   input browse br-estacao tt-estacao.largura
                   input browse br-estacao tt-estacao.cod-depos     
                   input browse br-estacao tt-estacao.cod-localiz   
                   input browse br-estacao tt-estacao.gera-pallet   
                   input browse br-estacao tt-estacao.caract.        
                   
         IF AVAIL tt-estacao THEN
            display
                tt-estacao.estacao        
                tt-estacao.nr-ord-produ   
                tt-estacao.it-codigo      
                tt-estacao.largura
                tt-estacao.lote
                tt-estacao.nr-pedido      
                tt-estacao.seq-ped        
                tt-estacao.nome-abrev       
                tt-estacao.cod-depos      
                tt-estacao.cod-localiz    
                tt-estacao.gera-pallet    
                tt-estacao.caract         
                with browse br-estacao. 
                                          
   end.
 
END.




ON END-ERROR OF C-Win
OR ENDKEY OF C-Win ANYWHERE DO:
   RETURN NO-APPLY.
END.


ON END-ERROR OF C-Win
OR ENDKEY OF C-Win ANYWHERE DO:
   RETURN NO-APPLY.
END.

ON WINDOW-CLOSE OF C-Win
DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
   apply 'choose' TO bt-sai IN FRAME f-relat.
   RETURN NO-APPLY.
END.

ON ENDKEY OF FRAME f-relat DO:
  return no-apply.
END.

/*
ON CHOOSE OF bt-primeiro IN FRAME f-relat
DO:

   RUN pi-le-primeiro.

END.


ON CHOOSE OF bt-proximo IN FRAME f-relat
DO:

   RUN pi-le-proximo.

END.


ON CHOOSE OF bt-anterior IN FRAME f-relat
DO:

   RUN pi-le-anterior.

END.


ON CHOOSE OF bt-final IN FRAME f-relat
DO:

   RUN pi-le-ultimo.

END.

ON CHOOSE OF bt-pesquisa IN FRAME f-relat
DO:
            

END.

*/


/*
ON CHOOSE OF bt-goto IN FRAME f-relat
DO: 
    
    DEFINE BUTTON gt-bt-cancel AUTO-END-KEY 
         LABEL "&Cancelar" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE BUTTON gt-bt-ok AUTO-GO 
         LABEL "&OK" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE RECTANGLE gt-rt-botoes
         EDGE-PIXELS 2 GRAPHIC-EDGE  
         SIZE 58 BY 1.42
         BGCOLOR 7.

    DEFINE VARIABLE i-sequencia-gt AS INTEGER NO-UNDO.
    
    DEFINE RECTANGLE gt-rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 4.50.
    
    DEFINE FRAME gt-frame-1

        i-sequencia-gt LABEL "C¢digo do Usu†rio" AT ROW 2.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 16.14 BY .88
        
        gt-rect-1 AT ROW 1.9 COL 2

        gt-bt-ok          AT ROW 7.3 COL 2.14
        gt-bt-cancel      AT ROW 7.3 COL 13             
        gt-rt-botoes      AT ROW 7.0 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "Chave do Registro" FONT 1
             DEFAULT-BUTTON gt-bt-ok CANCEL-BUTTON gt-bt-cancel.

    ON "CHOOSE":U OF gt-bt-ok IN FRAME gt-frame-1 DO:

        ASSIGN i-empresa-amg = int(i-sequencia-gt:SCREEN-VALUE IN FRAME gt-frame-1).

     RETURN.

    END.

    ENABLE i-sequencia-gt gt-bt-ok gt-bt-cancel 
        WITH FRAME gt-frame-1. 
    
    WAIT-FOR "GO":U OF FRAME gt-frame-1.

    RUN le-registro-goto.

END.
*/

/*

ON CHOOSE OF bt-deleta IN FRAME f-relat
DO: 
    
    DEFINE BUTTON ex-bt-cancel AUTO-END-KEY 
         LABEL "&Cancelar" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE BUTTON ex-bt-ok AUTO-GO 
         LABEL "&OK" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE RECTANGLE ex-rt-botoes
         EDGE-PIXELS 2 GRAPHIC-EDGE  
         SIZE 58 BY 1.42
         BGCOLOR 7.

    ASSIGN c-cod-estabel-amg = "Exclui Este Registro? ".

    DEFINE RECTANGLE ex-rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 4.50.
    
    DEFINE FRAME ex-frame-1
      
        c-cod-estabel-amg NO-LABEL 
           at ROW 3 col 10 
        
        ex-rect-1 AT ROW 1.9 COL 2

        ex-bt-cancel      AT ROW 7.3 COL 2.14             
        ex-bt-ok          AT ROW 7.3 COL 13
        ex-rt-botoes      AT ROW 7.0 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "Exclus∆o de Registro no Arquivo" FONT 1
             DEFAULT-BUTTON ex-bt-ok CANCEL-BUTTON ex-bt-cancel.

    ON "CHOOSE":U OF ex-bt-ok IN FRAME ex-frame-1 DO:

        FIND CURRENT am-tp-texto-romaneio EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL am-tp-texto-romaneio THEN
            DELETE am-tp-texto-romaneio.


        FIND NEXT am-tp-texto-romaneio NO-LOCK NO-ERROR.

        IF AVAIL am-tp-texto-romaneio THEN 
           RUN pi-mostra-registro. 

        ELSE DO:
            FIND PREV  am-tp-texto-romaneio NO-LOCK NO-ERROR.

            IF AVAIL am-tp-texto-romaneio THEN 
               RUN pi-mostra-registro. 

        END.

     RETURN.

    END.

    ENABLE ex-bt-cancel ex-bt-ok  
        WITH FRAME ex-frame-1. 
    
    DISPLAY c-cod-estabel-amg 
        WITH FRAME ex-frame-1.

    WAIT-FOR "GO":U OF FRAME ex-frame-1.

END.
*/

ON CHOOSE OF bt-cancela IN FRAME f-relat
DO:


   RUN pi-disable-campos.
   RUN pi-disable-bt-grava.
   RUN pi-enable-outros-botoes.
   RUN pi-limpa-campos.

   APPLY "entry" TO i-nr-ord-produ-amg IN FRAME f-relat.

END.


ON entry OF bt-novo IN FRAME f-relat
DO: 

   RUN pi-enable-bt-cancela.
   RUN pi-disable-outros-botoes.

/*
   RUN pi-limpa-campos.
*/   
   RUN pi-enable-campos.

   APPLY "entry" TO i-nr-ord-produ-amg IN FRAME f-relat.

END.


ON CHOOSE OF bt-novo IN FRAME f-relat
DO:

   RUN pi-enable-bt-cancela.
   RUN pi-disable-outros-botoes.


   RUN pi-limpa-campos.
   
   RUN pi-enable-campos.

   APPLY "entry" TO i-nr-ord-produ-amg IN FRAME f-relat.

END.

/*
ON CHOOSE OF bt-altera IN FRAME f-relat
DO:

   ASSIGN c-tipo-botao = "altera".

   RUN pi-enable-bt-cancela.
   RUN pi-disable-outros-botoes.


   RUN pi-enable-campos.

END.


ON CHOOSE OF bt-copia IN FRAME f-relat
DO:

   ASSIGN c-tipo-botao = "copia".

   RUN pi-enable-bt-cancela.
   RUN pi-disable-outros-botoes.


   RUN pi-enable-campos.

END.


ON CHOOSE OF bt-grava IN FRAME f-relat
DO:

    IF c-tipo-botao <> "novo"  AND 
       c-tipo-botao <> "copia" AND  
       c-tipo-botao <> "altera" THEN RETURN.

    /* Aqui colocar as validaá‰es dos campos antes de serem
       gravados no arquivo */



    /* ---------------------------------------------------- */

    ASSIGN c-tipo-botao2 = c-tipo-botao
           c-tipo-botao  = "".

    RUN pi-le-pela-chave.

    IF RETURN-VALUE = "nok" THEN DO: 
        RUN pi-limpa-campos.
        RUN pi-disable-campos.
        APPLY "choose" to bt-cancela IN FRAME f-relat.  
       RETURN.
    END.
       
    RUN pi-grava-registro.

    RUN pi-disable-campos.

    IF c-tipo-botao2 = "novo" THEN DO:
        APPLY "choose" to bt-novo IN FRAME f-relat.
        RETURN.
    END.
    ELSE DO:
        APPLY "choose" to bt-cancela IN FRAME f-relat.
        RETURN.
    END.

END.

*/
ON CHOOSE OF bt-ajuda IN FRAME f-relat
DO:


END.


ON CHOOSE OF bt-sai IN FRAME f-relat
DO:


   apply "close" to this-procedure.

END.


/*
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
   run pi-troca-pagina.
END.

ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
   run pi-troca-pagina.
END.

ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
   run pi-troca-pagina.
END.

ON MOUSE-SELECT-CLICK OF im-pg-dig IN FRAME f-relat
DO:
   run pi-troca-pagina.
END.
*/
/* ***************************  Main Block  *************************** */

i-nr-ord-produ-amg:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME f-relat.
i-nr-ord-specs-amg:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME f-relat.
i-nr-ord-grade-amg:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME f-relat.

tt-estacao.nr-ord-produ:LOAD-MOUSE-POINTER ("image/lupa.cur") IN BROWSE br-estacao.

ASSIGN CURRENT-WINDOW             = {&WINDOW-NAME}
   THIS-PROCEDURE:CURRENT-WINDOW  = {&WINDOW-NAME}.

assign v-cod-prog-gerado = "essf0021".



def var c-tit as char no-undo.

ASSIGN c-tit = "essf0021 - Setup de Produá∆o no Corte/Recorte/Metalizaá∆o".
assign {&WINDOW-NAME}:title = c-tit
       c-arq-old        = c-arquivo
       c-impressora-old = c-arquivo.


assign {&window-name}:virtual-width-chars  = {&window-name}:width-chars  
       {&window-name}:virtual-height-chars = {&window-name}:height-chars 
       {&window-name}:min-width-chars      = {&window-name}:width-chars  
       {&window-name}:max-width-chars      = {&window-name}:width-chars  
       {&window-name}:min-height-chars     = {&window-name}:height-chars 
       {&window-name}:max-height-chars     = {&window-name}:height-chars.
assign c-terminal = " Terminal".


ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.


def var wh-label-sel     as widget-handle no-undo.
def var wh-label-cla     as widget-handle no-undo.
def var wh-label-par     as widget-handle no-undo.
def var wh-label-dig     as widget-handle no-undo.
def var wh-label-imp     as widget-handle no-undo.
def var wh-group         as widget-handle no-undo.
def var wh-child         as widget-handle no-undo.
def var c-list-folders   as char          no-undo.
def var i-current-folder as integer       no-undo.
def var i-new-folder     as integer       no-undo.
def var c-aux            as char          no-undo.
def var i-aux            as integer       no-undo.

ON  CLOSE OF THIS-PROCEDURE DO:
    RUN disable_ui. 
END.

on  CTRL-TAB,SHIFT-CTRL-TAB anywhere do:
    define variable h_handle  as handle no-undo.       
    define variable c_imagem  as character no-undo.
    define variable l_direita as logical no-undo.            

    l_direita = last-event:label = 'CTRL-TAB'.
        
    block1:
    repeat:        
        if  l_direita then do:
            if  i-current-folder = num-entries(c-list-folders) then
                i-current-folder = 1.
            else
                i-current-folder = i-current-folder + 1.
        end.
        else do:
            if  i-current-folder = 1 then
                i-current-folder = num-entries(c-list-folders).
            else
                i-current-folder = i-current-folder - 1.
        end.
    
        assign c_imagem = entry(i-current-folder,c-list-folders)
               h_handle = frame f-relat:first-child
               h_handle = h_handle:first-child.

        do  while valid-handle(h_handle):
            if  h_handle:type = 'image' and
                h_handle:name =  c_imagem then do:
                if  h_handle:sensitive = no then 
                    next block1.
                apply 'mouse-select-click' to h_handle.
                leave block1.
            end.
            h_handle = h_handle:next-sibling.
        end.
    end.
end.



/********************************************************** 
** Procedure de troca de p†gina por CTRL-TAB 
**********************************************************/
procedure pi-first-child:
        
    define input parameter wh-entry-folder as widget-handle.
    
    assign wh-entry-folder = wh-entry-folder:first-child
           wh-entry-folder = wh-entry-folder:first-child.
    do  while(valid-handle(wh-entry-folder)):
        if  wh-entry-folder:sensitive = yes 
        and wh-entry-folder:type <> 'rectangle' 
        and wh-entry-folder:type <> 'image'
        and wh-entry-folder:type <> 'browse' then do:
            apply 'entry' to wh-entry-folder.
            leave.
        end.
        else
            assign wh-entry-folder = wh-entry-folder:next-sibling.    
    end.
    
end.


/* define procedure externa para execucao do programa de visualizacao do relatorio */

PROCEDURE WinExec EXTERNAL "kernel32.dll":
  DEF INPUT  PARAM prg_name                          AS CHARACTER.
  DEF INPUT  PARAM prg_style                         AS SHORT.
END PROCEDURE.



PAUSE 0 BEFORE-HIDE.

MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    FOR EACH tt-estacao.
        DELETE tt-estacao.
    END.

    ASSIGN c-arquivo-jr = search(session:temp-directory + "essf0021.txt"). 
    
    IF c-arquivo-jr <> ? then do:
 

        INPUT FROM VALUE(c-arquivo-jr).



    REPEAT:

       IMPORT UNFORMATTED text-string.

       IF INT(SUBSTRING(text-string,1,9)) = 0 THEN DO:

           CREATE tt-estacao.

           ASSIGN tt-estacao.estacao         = INT(SUBSTRING(text-string,1,9))    
                  tt-estacao.nr-ord-off-spc  = INT(SUBSTRING(text-string,10,9))    
                  tt-estacao.nr-ord-off-grd  = INT(SUBSTRING(text-string,20,9))    
                  tt-estacao.letra-bobina    = SUBSTRING(text-string,30,1)         
                  tt-estacao.nr-prim-bob     = INT(SUBSTRING(text-string,40,9)).

       END.

       ELSE DO:

          CREATE tt-estacao.
          
          ASSIGN tt-estacao.estacao	     = INT(SUBSTRING(text-string,1,9))
                 tt-estacao.nr-ord-produ    = INT(SUBSTRING(text-string,10,9))
                 tt-estacao.it-codigo       = SUBSTRING(text-string,20,18)
                 tt-estacao.nr-pedido       = int(SUBSTRING(text-string,40,9))     
                 tt-estacao.seq-ped         = INT(SUBSTRING(text-string,50,9))
                 tt-estacao.nome-abrev      = SUBSTRING(text-string,60,15)
                 tt-estacao.cod-depos       = SUBSTRING(text-string,80,9)
                 tt-estacao.cod-localiz     = SUBSTRING(text-string,90,10)
                 tt-estacao.gera-pallet     = SUBSTRING(text-string,100,3) 
                 tt-estacao.lote            = SUBSTRING(text-string,103,10)
                 tt-estacao.largura         = int(SUBSTRING(text-string,114,5)).

       END.

    END.
    
    end.

    FOR EACH tt-estacao NO-LOCK.

        IF tt-estacao.estacao = 0 THEN DO:

            ASSIGN c-letra-bob = tt-estacao.letra-bobina        
                   i-nr-bobina = tt-estacao.nr-prim-bob          
                   i-nr-ord-specs-amg = tt-estacao.nr-ord-off-spc   
                   i-nr-ord-grade-amg = tt-estacao.nr-ord-off-grd.

           
            DELETE tt-estacao.

        END.

        ELSE DO:

            ASSIGN i-nr-ord-produ-amg = tt-estacao.nr-ord-produ.

            IF i-estacao-ini-amg = 0 THEN
                ASSIGN i-estacao-ini-amg = tt-estacao.estacao.

            IF i-estacao-fim-amg = 20 OR
               i-estacao-fim-amg < tt-estacao.estacao THEN
                ASSIGN i-estacao-fim-amg = tt-estacao.estacao.

        END.

    END.

    CLOSE query br-estacao.
    open query br-estacao for each tt-estacao.

    if num-results("br-estacao") > 0 THEN DO:
       get current br-estacao.
    END.


    IF SESSION:SET-WAIT-STATE("":U) THEN.
    RUN enable_UI.

    RUN pi-disable-bt-grava.   

    assign v-cod-pg-mouse-selec = "im-pg-sel".


     view c-win.

     apply "entry" to frame f-Relat.
     apply "entry" to c-win.
     APPLY "leave" TO i-nr-ord-specs-amg.
     APPLY "leave" TO i-nr-ord-grade-amg.
     APPLY "leave" TO i-nr-ord-produ-amg.



    /* ----------------------------- */

   IF  NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.


END.
/* **********************  Internal Procedures  *********************** */

PROCEDURE adm-row-available :
   /* Define variables needed by this internal procedure.             */
  /* Process the newly available records (i.e. display fields, 
     open queries, and/or pass records on to any RECORD-TARGETS).    */
END PROCEDURE.

PROCEDURE disable_UI :
   IF SESSION:DISPLAY-TYPE = "GUI" AND VALID-HANDLE(C-Win)
   THEN DELETE WIDGET C-Win.
   IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE enable_UI :

    DISPLAY i-nr-ord-produ-amg
                             
           i-estacao-ini-amg 
           i-estacao-fim-amg 

           c-letra-bob       
           i-nr-bobina       

           i-nr-ord-specs-amg
           i-nr-ord-grade-amg

        WITH FRAME f-relat IN WINDOW C-Win.

   ENABLE  
          bt-novo    
          bt-cancela
          bt-sai
          bt-ajuda
           
   WITH FRAME f-relat IN WINDOW C-Win.

   {&OPEN-BROWSERS-IN-QUERY-f-relat}

  ENABLE br-estacao   
      WITH FRAME f-relat IN WINDOW c-win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
   
  VIEW C-Win.

END PROCEDURE.

PROCEDURE local-exit :
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN.
END PROCEDURE.  

PROCEDURE pi-troca-pagina:


END PROCEDURE.

PROCEDURE send-records :
    /* Define variables needed by this internal procedure.               */ 
    /* For each requested table, put it':Us ROWID in the output list.      */
    /* Deal with any unexpected table requests before closing.           */ 
END PROCEDURE.

PROCEDURE state-changed :
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
    run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

Procedure pi-muda-cor-label-folder:
   def input parameter p-cod-label as char  no-undo.
   def var wh-pai   as widget-handle.
   def var wh-filho as widget-handle.

    assign wh-pai = frame f-relat:handle
           wh-pai = wh-pai:first-child.
   do while wh-pai <> ?:
       do  while valid-handle(wh-pai):
           assign wh-filho = wh-pai:first-child.
           do  while valid-handle(wh-filho):
               if  wh-filho:type = "TEXT"
                   then
                       if  wh-filho:screen-value = p-cod-label
                       then
                           assign wh-filho:fgcolor = 7.
                       assign wh-filho = wh-filho:next-sibling.
           end.
           assign wh-pai = wh-pai:next-sibling.
       end.
   end.
END PROCEDURE.

PROCEDURE OpenDocument:

    def input param c-doc as char  no-undo.
    def var c-exec as char  no-undo.
    def var h-Inst as int  no-undo.

    assign c-exec = fill("x",255).
    run FindExecutableA (input c-doc,
                         input "",
                         input-output c-exec,
                         output h-inst).

    if h-inst >= 0 and h-inst <=32 then
      run ShellExecuteA (input 0,
                         input "open",
                         input "rundll32.exe",
                         input "shell32.dll,OpenAs_RunDLL " + c-doc,
                         input "",
                         input 1,
                         output h-inst).

    run ShellExecuteA (input 0,
                       input "open",
                       input c-doc,
                       input "",
                       input "",
                       input 1,
                       output h-inst).

    if h-inst < 0 or h-inst > 32 then return "OK".
    else return "NOK".

END PROCEDURE.

PROCEDURE FindExecutableA EXTERNAL "Shell32.dll" persistent:

    define input parameter lpFile as char  no-undo.
    define input parameter lpDirectory as char  no-undo.
    define input-output parameter lpResult as char  no-undo.
    define return parameter hInstance as long.

END.

PROCEDURE ShellExecuteA EXTERNAL "Shell32.dll" persistent:

    define input parameter hwnd as long.
    define input parameter lpOperation as char  no-undo.
    define input parameter lpFile as char  no-undo.
    define input parameter lpParameters as char  no-undo.
    define input parameter lpDirectory as char  no-undo.
    define input parameter nShowCmd as long.
    define return parameter hInstance as long.

END PROCEDURE.

PROCEDURE pi-mostra-registro.
/*
    ASSIGN am-tp-texto-romaneio.sequencia:SCREEN-VALUE IN FRAME f-relat = STRING (am-tp-texto-romaneio.sequencia)
           opcao-jr:SCREEN-VALUE IN FRAME f-relat = STRING (am-tp-texto-romaneio.situacao)
           am-tp-texto-romaneio.descricao:SCREEN-VALUE IN FRAME f-pg-sel = STRING (am-tp-texto-romaneio.descricao)
           c-texto-jr:SCREEN-VALUE IN FRAME f-pg-sel = STRING (am-tp-texto-romaneio.texto)
           c-narrativa-jr:SCREEN-VALUE IN FRAME f-pg-sel = STRING (am-tp-texto-romaneio.narrativa).  
*/
END PROCEDURE.

PROCEDURE pi-le-primeiro.


END PROCEDURE.


PROCEDURE pi-le-proximo.

    

END PROCEDURE.


PROCEDURE pi-le-anterior.

    

END PROCEDURE.

PROCEDURE pi-le-ultimo.

    

END PROCEDURE.



PROCEDURE le-registro-goto.
    
      
END PROCEDURE.

PROCEDURE pi-mostra-mensagem.



END PROCEDURE.

PROCEDURE pi-limpa-campos.

    ASSIGN i-nr-ord-produ-amg:SCREEN-VALUE IN FRAME f-relat = ""
           i-estacao-ini-amg:SCREEN-VALUE IN FRAME f-relat = "1"
           i-estacao-fim-amg:SCREEN-VALUE IN FRAME f-relat = "20"
           c-letra-bob:SCREEN-VALUE IN FRAME f-relat = ""
           i-nr-bobina:SCREEN-VALUE IN FRAME f-relat = ""
           i-nr-ord-specs-amg:SCREEN-VALUE IN FRAME f-relat = ""
           i-nr-ord-grade-amg:SCREEN-VALUE IN FRAME f-relat = ""
           c-it-codigo-amg:SCREEN-VALUE IN FRAME f-relat = ""
           c-it-codigo-spc:SCREEN-VALUE IN FRAME f-relat = ""
           c-it-codigo-grd:SCREEN-VALUE IN FRAME f-relat = ""
           c-desc-item-amg:SCREEN-VALUE IN FRAME f-relat = ""
           c-desc-item-spc:SCREEN-VALUE IN FRAME f-relat = ""
           c-desc-item-grd:SCREEN-VALUE IN FRAME f-relat = "".


    FOR EACH tt-estacao.
        DELETE tt-estacao.
    END.

    CLOSE query br-estacao.
    open query br-estacao for each tt-estacao.


END PROCEDURE.

PROCEDURE pi-enable-campos.

    ENABLE i-nr-ord-produ-amg
                             
           i-estacao-ini-amg 
           i-estacao-fim-amg 
           bt-filtro

           c-letra-bob       
           i-nr-bobina       

           i-nr-ord-specs-amg
           i-nr-ord-grade-amg

           bt-copia-est
           bt-limpa-est
           bt-limpa-tudo

           bt-atualiza
           bt-cancela2
        WITH FRAME f-relat.

    ENABLE 
        WITH FRAME f-relat.

    APPLY "entry" TO i-nr-ord-produ-amg IN FRAME f-relat.


END PROCEDURE.


PROCEDURE pi-disable-campos.

END PROCEDURE.

PROCEDURE pi-le-pela-chave.


    RETURN "ok".

END PROCEDURE.


PROCEDURE pi-grava-registro.


    IF c-tipo-botao2 = "novo" OR c-tipo-botao2 = "copia" THEN DO:


    END.

    IF c-tipo-botao2 = "altera" THEN DO:

    END.



    RETURN "ok".
                

END PROCEDURE.

PROCEDURE pi-disable-bt-grava.

    assign /*bt-grava:SENSITIVE in frame f-relat   = no */
           bt-cancela:SENSITIVE in frame f-relat = no.

END PROCEDURE.

PROCEDURE pi-enable-bt-cancela.

    ENABLE bt-cancela
        WITH FRAME f-relat.

END PROCEDURE.

PROCEDURE pi-disable-outros-botoes.
   ASSIGN /*bt-primeiro:SENSITIVE in frame f-relat = no  
          bt-anterior:SENSITIVE in frame f-relat   = no  
          bt-proximo:SENSITIVE in frame f-relat    = no   
          bt-final:SENSITIVE in frame f-relat      = no  
          bt-goto:SENSITIVE in frame f-relat       = no 
          bt-pesquisa:SENSITIVE in frame f-relat   = no */
          bt-novo:SENSITIVE in frame f-relat       = no. 
        /*  bt-copia:SENSITIVE in frame f-relat    = no 
          bt-altera:SENSITIVE in frame f-relat     = no 
          bt-deleta:SENSITIVE in frame f-relat     = no */
   
END PROCEDURE.

PROCEDURE pi-enable-outros-botoes.

   ENABLE /*bt-primeiro 
          bt-anterior 
          bt-proximo  
          bt-final 
          bt-goto
          bt-pesquisa */
         /* bt-novo */
          bt-cancela   
         /* bt-copia
          bt-altera
          bt-deleta */
          bt-sai
   WITH FRAME f-relat IN WINDOW C-Win.
   
END PROCEDURE.

PROCEDURE pi-monta-browse-lotes:
Do:
     /** Montando a tabela para tt-estacao ***/

    FOR EACH tt-estacao.
        DELETE tt-estacao.
    END.

    CREATE tt-estacao.

    /*
    ASSIGN tt-estacao.estacao          = 
           tt-estacao.nr-ord-produ     = 
           tt-estacao.it-codigo        = 
           tt-estacao.nr-pedido        = 
           tt-estacao.seq-ped          = 
           tt-estacao.cod-depos        = 
           tt-estacao.cod-localiz      =  
           tt-estacao.gera-pallet      = 
           tt-estacao.caract           =  

    */

       /*** habilita **/

     open query br-estacao for each tt-estacao.
     apply 'entry' to tt-estacao.nr-ord-produ in browse br-estacao. 

     if num-results("br-estacao") > 0 THEN DO:
        get current br-estacao.
     END.

  end. 

END PROCEDURE.

PROCEDURE pi-pesquisa-ordem.

def var c-estado as char  EXTENT 9  format "x(14)" label "Estado" INITIAL ["N∆o Iniciada","Liberada","Alocada","Separada","Requisitada","Iniciada","Finalizada","Terminada"] no-undo.
    DEFINE QUERY br-ord-prod FOR 
          ord-prod , item, reservas  SCROLLING.

    DEFINE BROWSE br-ord-prod
      QUERY br-ord-prod  DISPLAY 
        ord-prod.cod-estabel    FORMAT "x(5)" 
        ord-prod.nr-ord-produ    
        ord-prod.it-codigo      FORMAT "x(20)"    
        ord-prod.cod-refer      FORMAT "x(15)"    
        ord-prod.dt-inicio      
        ord-prod.dt-termino   
        ord-prod.qt-ordem              
           
        c-estado[ord-prod.estado]       
        WITH SEPARATORS SIZE 85 BY 8
             BGCOLOR 15 FONT 1 TITLE "Ordens".

    
    DEFINE BUTTON pesq-bt-cancel AUTO-END-KEY 
         LABEL "&Cancelar" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE BUTTON pesq-bt-ok AUTO-GO 
         LABEL "&OK" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE RECTANGLE pesq-rt-botoes
         EDGE-PIXELS 2 GRAPHIC-EDGE  
         SIZE 98 BY 1.42
         BGCOLOR 7.

    DEFINE BUTTON pesq-bt-filtro 
         IMAGE-UP FILE "image\im-sav":U
         LABEL "Filtro" 
         SIZE 6.14 BY 1.


    DEFINE VARIABLE pesq-cod-estabel-ini AS CHAR FORMAT "x(3)"      INITIAL "{cdp\poloestab.i 422}" /*solic-318*/     NO-UNDO.
    DEFINE VARIABLE pesq-cod-estabel-fim AS CHAR FORMAT "x(3)"      INITIAL "{cdp\poloestab.i 422}" /*solic-318*/     NO-UNDO.
    DEFINE VARIABLE pesq-dt-inicio-ini   AS DATE FORMAT 99/99/9999  INITIAL TODAY     NO-UNDO.
    DEFINE VARIABLE pesq-dt-inicio-fim   AS DATE FORMAT 99/99/9999  INITIAL TODAY     NO-UNDO.
    DEFINE VARIABLE pesq-it-codigo-ini   AS CHAR FORMAT "x(16)"     INITIAL ""        NO-UNDO.
    DEFINE VARIABLE pesq-it-codigo-fim   AS CHAR FORMAT "x(16)"     INITIAL "ZZZZZZZZZZZZZZZZ" NO-UNDO.
    DEFINE VARIABLE pesq-ge-codigo-ini   AS integer FORMAT "99"     INITIAL 42        NO-UNDO.
    DEFINE VARIABLE pesq-ge-codigo-fim   AS integer FORMAT "99"     INITIAL 48 NO-UNDO.

    DEFINE VARIABLE pesq-nr-ord-ini      AS INT  FORMAT ">>>>>>>>9" INITIAL 0         NO-UNDO.
    DEFINE VARIABLE pesq-nr-ord-fim      AS INT  FORMAT ">>>>>>>>9" INITIAL 999999999 NO-UNDO.
    
    ASSIGN pesq-dt-inicio-ini = pesq-dt-inicio-ini - 30.

    DEFINE RECTANGLE pesq-rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 99 BY 14.50.
    
    DEFINE FRAME pesq-frame-1

        pesq-cod-estabel-ini LABEL "Estabelecimento" AT ROW 2.5 COL 14 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 14 BY .88
        
        pesq-cod-estabel-fim NO-LABEL AT ROW 2.5 COL 37 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 14 BY .88

        pesq-dt-inicio-ini LABEL "Dt.In°cio Ordem" AT ROW 3.5 COL 14 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 14 BY .88
        
        pesq-dt-inicio-fim NO-LABEL AT ROW 3.5 COL 37 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 14 BY .88

        pesq-it-codigo-ini LABEL "Item" AT ROW 4.5 COL 14 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 14 BY .88

        pesq-it-codigo-fim NO-LABEL AT ROW 4.5 COL 37 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 14 BY .88
  
        
        pesq-ge-codigo-ini LABEL "Gr.Estoque" AT ROW 5.5 COL 14 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 3 BY .88

         
        
        pesq-ge-codigo-fim NO-LABEL AT ROW 5.5 COL 37 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 3 BY .88
  

        pesq-nr-ord-ini LABEL "Nr.Ord.Produá∆o" AT ROW 6.5 COL 14 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 14 BY .88
        
        pesq-nr-ord-fim NO-LABEL AT ROW 6.5 COL 37 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 14 BY .88

        pesq-bt-filtro AT ROW 2.5 COL 55

    
        br-ord-prod AT ROW 7.8 COL 4 

        IMAGE-1 AT ROW 2.5 COL 31
        IMAGE-2 AT ROW 2.5 COL 35
        IMAGE-3 AT ROW 3.5 COL 31
        IMAGE-4 AT ROW 3.5 COL 35
        IMAGE-5 AT ROW 4.5 COL 31
        IMAGE-6 AT ROW 4.5 COL 35
        IMAGE-7 AT ROW 5.5 COL 31
        IMAGE-8 AT ROW 5.5 COL 35
        IMAGE-9 AT ROW 6.5 COL 31
        IMAGE-10 AT ROW 6.5 COL 35

        
        pesq-rect-1 AT ROW 1.9 COL 2

        pesq-bt-ok          AT ROW 17.3 COL 2.14
        pesq-bt-cancel      AT ROW 17.3 COL 13             
        pesq-rt-botoes      AT ROW 17.0 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "Pesquisa Ordem Produá∆o" FONT 1
             DEFAULT-BUTTON pesq-bt-ok CANCEL-BUTTON pesq-bt-cancel.

    ON "CHOOSE":U OF pesq-bt-ok IN FRAME pesq-frame-1 DO:

        IF AVAIL ord-prod THEN
           ASSIGN pesq-nr-ord-produ = ord-prod.nr-ord-produ.
        ELSE
           ASSIGN pesq-nr-ord-produ = 999999999.

        RETURN.

    END.


    ON CHOOSE OF pesq-bt-filtro IN FRAME pesq-frame-1 /* Filtro de Seleá∆o */
    DO:
  
      CLOSE QUERY br-ord-prod.
  
     /* Alteraá∆o 05/12/2011 para pesquisar ordem com item da reserva = ao do lote consumido */

      IF c-it-codigo-reserva-jr = "" THEN DO:
  
      open query br-ord-prod for each ord-prod WHERE
          ord-prod.dt-inicio      >= INPUT FRAME pesq-frame-1 pesq-dt-inicio-ini   AND 
          ord-prod.dt-inicio      <= INPUT FRAME pesq-frame-1 pesq-dt-inicio-fim   AND 
          ord-prod.estado          < 7                                             AND 
          ord-prod.nr-ord-produ   >= INPUT FRAME pesq-frame-1 pesq-nr-ord-ini      AND
          ord-prod.nr-ord-produ   <= INPUT FRAME pesq-frame-1 pesq-nr-ord-fim      AND
          ord-prod.it-codigo      >= INPUT FRAME pesq-frame-1 pesq-it-codigo-ini   AND
          ord-prod.it-codigo      <= INPUT FRAME pesq-frame-1 pesq-it-codigo-fim   AND
          ord-prod.cod-estabel    >= INPUT FRAME pesq-frame-1 pesq-cod-estabel-ini AND
          ord-prod.cod-estabel    <= INPUT FRAME pesq-frame-1 pesq-cod-estabel-fim 
          USE-INDEX dt-inicio
          NO-LOCK,

          each item where item.it-codigo = ord-prod.it-codigo and 
           item.ge-codigo >= INPUT FRAME pesq-frame-1 pesq-ge-codigo-ini and
           item.ge-codigo <= INPUT FRAME pesq-frame-1 pesq-ge-codigo-fim
           no-lock,

          FIRST reservas WHERE reservas.nr-ord-produ = ord-prod.nr-ord-produ NO-LOCK.

      END.
  
      IF c-it-codigo-reserva-jr <> "" THEN DO:
  
      open query br-ord-prod for each ord-prod WHERE
          ord-prod.dt-inicio      >= INPUT FRAME pesq-frame-1 pesq-dt-inicio-ini   AND 
          ord-prod.dt-inicio      <= INPUT FRAME pesq-frame-1 pesq-dt-inicio-fim   AND 
          ord-prod.estado          < 7                                             AND 
          ord-prod.nr-ord-produ   >= INPUT FRAME pesq-frame-1 pesq-nr-ord-ini      AND
          ord-prod.nr-ord-produ   <= INPUT FRAME pesq-frame-1 pesq-nr-ord-fim      AND
          ord-prod.it-codigo      >= INPUT FRAME pesq-frame-1 pesq-it-codigo-ini   AND
          ord-prod.it-codigo      <= INPUT FRAME pesq-frame-1 pesq-it-codigo-fim   AND
          ord-prod.cod-estabel    >= INPUT FRAME pesq-frame-1 pesq-cod-estabel-ini AND
          ord-prod.cod-estabel    <= INPUT FRAME pesq-frame-1 pesq-cod-estabel-fim 
          USE-INDEX dt-inicio
          NO-LOCK,

          each item where item.it-codigo = ord-prod.it-codigo and 
           item.ge-codigo >= INPUT FRAME pesq-frame-1 pesq-ge-codigo-ini and
           item.ge-codigo <= INPUT FRAME pesq-frame-1 pesq-ge-codigo-fim 
            NO-LOCK ,

          FIRST reservas WHERE reservas.nr-ord-produ = ord-prod.nr-ord-produ AND
                         reservas.it-codigo    = c-it-codigo-reserva-jr
                         NO-LOCK .

      END.

/*------------------------------------------------------------------------------------*/

  
      if num-results("br-ord-prod") > 0 THEN DO:
         get current br-ord-prod.
      END.

    END.


    on "entry" OF pesq-cod-estabel-ini in frame pesq-frame-1 do:

       APPLY "CHOOSE" TO pesq-bt-filtro  IN FRAME pesq-frame-1.

       APPLY "entry" TO pesq-cod-estabel-ini in frame pesq-frame-1.

       RETURN NO-APPLY.

    END.


    ON mouse-select-dblclick OF br-ord-prod IN FRAME pesq-frame-1
    DO:

        APPLY "CHOOSE" TO pesq-bt-ok IN FRAME pesq-frame-1.

    END.

    DISPLAY pesq-cod-estabel-ini
            pesq-cod-estabel-fim
            pesq-dt-inicio-ini
            pesq-dt-inicio-fim
            pesq-it-codigo-ini
            pesq-it-codigo-fim
            pesq-ge-codigo-ini
            pesq-ge-codigo-fim
            pesq-nr-ord-ini    
            pesq-nr-ord-fim
        WITH FRAME pesq-frame-1. 

    ENABLE pesq-cod-estabel-ini
           pesq-cod-estabel-fim
           pesq-dt-inicio-ini
           pesq-dt-inicio-fim
           pesq-it-codigo-ini
           pesq-it-codigo-fim
           pesq-ge-codigo-ini
           pesq-ge-codigo-fim
           pesq-nr-ord-ini
           pesq-nr-ord-fim
           pesq-bt-filtro
           br-ord-prod
           pesq-bt-ok 
           pesq-bt-cancel 
        WITH FRAME pesq-frame-1. 
    
    WAIT-FOR "GO":U OF FRAME pesq-frame-1.

END PROCEDURE.
  


