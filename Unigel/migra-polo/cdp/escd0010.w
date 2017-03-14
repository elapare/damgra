
&Scoped-define WINDOW-NAME C-Win

/*------------------------------------------------------------------------
File.............: escd0010.w
Description......: Consulta Requisi‡äes de Funcion rios 
Input Parameters : 
Output Parameters: 
Author...........: Amgra - Jos‚ Roberto.
Created..........: 08/06/2010   
OBS..............: 
------------------------------------------------------------------------*/
define buffer empresa for mgmulti.empresa.
DEFINE VARIABLE r-codigo-rwi  AS ROWID                     NO-UNDO.
    
define variable c-prog-gerado as character no-undo initial "escd0010".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

DEFINE BUFFER b-am-cd-item-vida-util FOR am-cd-item-vida-util.
CREATE WIDGET-POOL. 

/* ***************************  Definitions  ************************** */


/* Preprocessadores do Template de Relat¢rio                            */

&GLOBAL-DEFINE PGSEL f-pg-sel 
&GLOBAL-DEFINE PGPAR f-pg-par 
&GLOBAL-DEFINE PGDIG f-pg-dig 
&GLOBAL-DEFINE PGIMP f-pg-imp 

/* Include Com as Vari veis Globais */

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

    DEFINE TEMP-TABLE tt-digita no-undo
        FIELD cod-estabel          AS CHAR    FORMAT "x(3)"           LABEL "Est."
        FIELD cod-funcionario      AS INT     FORMAT ">>>>>>9"      LABEL "Cd.Func."
        FIELD it-codigo-pai            AS CHAR    FORMAT "x(16)"          LABEL "Item Controle"   
        FIELD it-codigo            AS CHAR    FORMAT "x(16)"          LABEL "Item Requisicao"   
        FIELD desc-item            AS CHAR    FORMAT "x(40)"          LABEL "Descri‡Æo"
        FIELD dt-requisicao        AS date    FORMAT "99/99/9999"     LABEL "Dt.Requis."
        FIELD nr-requisicao        AS INT     FORMAT ">>>>>>>9"      LABEL "Nr.Requis."
        FIELD seq-requisicao       AS INT     FORMAT ">>9"            LABEL "Seq."
        FIELD quantidade           AS DEC     FORMAT ">>>9.99" LABEL "Qt.Req."
        FIELD un                   AS CHAR    FORMAT "x(2)"           LABEL "Unid"
        FIELD usuar-requisicao     AS CHAR    FORMAT "x(12)"          LABEL "Usr.Requis"
        FIELD dias-validade        AS INT     FORMAT ">>>>>9"         LABEL "Dura‡Æo"
        FIELD quantidade-max       AS DEC     FORMAT ">>>9.99" LABEL "Qt.Perm"
        FIELD quantidade-fora      AS DEC     FORMAT ">>>9.99" LABEL "Qt.Liberada"
        FIELD dt-aut-fora-prazo    AS date    FORMAT "99/99/9999"     LABEL "Dt.Libera‡Æo"
        FIELD usuar-aut-fora-prazo AS CHAR    FORMAT "x(12)"          LABEL "Usr.Libera‡Æo"
        INDEX chave IS PRIMARY cod-estabel     ascending
                               cod-funcionario ascending
                               it-codigo-pai   ascending
                               dt-requisicao   descending
                               it-codigo       ascending
                               
                               nr-requisicao   descending
                               seq-requisicao  descending.



/************************************************************************************/

/* Transfer Definitions */
    DEFINE QUERY br-digita FOR 
          tt-digita SCROLLING.
                   

        /* Browse definitions  */
    DEFINE BROWSE br-digita
      QUERY br-digita  DISPLAY 
        tt-digita.cod-estabel           FORMAT "x(4)"  
        tt-digita.cod-funcionario         
        tt-digita.it-codigo-pai             FORMAT "x(16)" 
        tt-digita.it-codigo             FORMAT "x(16)" 
        tt-digita.dt-requisicao
        tt-digita.quantidade  
        tt-digita.desc-item             FORMAT "x(60)" 
                   
        tt-digita.nr-requisicao           
        tt-digita.seq-requisicao          
        tt-digita.quantidade              
        tt-digita.un                      
        tt-digita.usuar-requisicao      FORMAT "x(13)"   
        tt-digita.dias-validade   
        tt-digita.quantidade-max
        tt-digita.quantidade-fora  

        tt-digita.dt-aut-fora-prazo       
        tt-digita.usuar-aut-fora-prazo  
         ENABLE tt-digita.quantidade-fora 
       
    /* _UIB-CODE-BLOCK-END */
    &ANALYZE-RESUME
        WITH SEPARATORS SIZE 109 BY 12.2
             BGCOLOR 15 FONT 1 
           .
  


/* Local Variable Definitions ---                                       */ 

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


/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

def new global shared var c-nome-amg            as char no-undo.
def new global shared var i-cod-funcionario-amg as INT  no-undo.

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 


DEFINE VARIABLE c-cod-estabel          AS CHARACTER FORMAT "x(3)"          NO-UNDO.
DEFINE VARIABLE i-cod-funcionario      AS INT       FORMAT ">>>>>>>>9"     NO-UNDO.
DEFINE VARIABLE c-nome                 AS CHARACTER FORMAT "x(45)"         NO-UNDO.
DEFINE VARIABLE c-it-codigo-ini        AS CHAR      FORMAT "x(16)"         NO-UNDO.
DEFINE VARIABLE c-it-codigo-fim        AS CHAR      FORMAT "x(16)"         NO-UNDO.


/****************** Defini‡ao de Vari veis de Trabalho *********************/ 
DEFINE VARIABLE pri-vez    AS INTEGER    NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */ 

/* ***********************  Control Definitions  ********************** */ 

DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO. 

DEFINE BUTTON bt-cancel AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&Autoriza Nova Requisi‡Æo" 
     SIZE 23 BY 1
     BGCOLOR 8.

DEFINE BUTTON bt-del AUTO-GO 
     LABEL "&Cancela Autoriza‡Æo" 
     SIZE 23 BY 1
     BGCOLOR 8.

DEFINE RECTANGLE rt-botoes
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 109 BY 1.42
     BGCOLOR 7.


DEFINE BUTTON bt-arquivo
    IMAGE-UP FILE "image\im-sea"
    IMAGE-INSENSITIVE FILE "image\ii-sea"
    LABEL " "
    SIZE 4 BY 1.
    
DEFINE BUTTON bt-config-impr
    IMAGE-UP FILE "image\im-cfprt"
    LABEL " "
    SIZE 4 BY 1.
   
DEFINE RECTANGLE RECT-20
         EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
         SIZE 28 BY 4.5.


DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY 2.8.


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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)" INITIAL "Execu‡Æo"
VIEW-AS TEXT 
SIZE 10.86 BY .62 NO-UNDO.

DEFINE VARIABLE text-parametro AS CHARACTER FORMAT "X(256)" INITIAL "Parƒmetros de ImpressÆo"
VIEW-AS TEXT 
SIZE 24.72 BY .62 NO-UNDO.

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

DEFINE VARIABLE tb-parametro AS LOGICAL INITIAL no 
LABEL "Imprimir P gina de Parƒmetros"
VIEW-AS TOGGLE-BOX 
SIZE 32 BY .83 
NO-UNDO.

DEFINE VARIABLE rs-formato AS INTEGER INITIAL 2 
VIEW-AS RADIO-SET HORIZONTAL 
RADIO-BUTTONS 
"80 colunas", 1,
"132 colunas", 2
SIZE 32 BY .92 NO-UNDO.


DEFINE BUTTON bt-arquivo-saida 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo-entrada AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-saida AS CHARACTER FORMAT "X(256)":U INITIAL "Arquivo de Entrada" 
      VIEW-AS TEXT 
     SIZE 19 BY .63 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.83.

DEFINE RECTANGLE RECT-7 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 46.29 BY 2.92.


DEFINE RECTANGLE RECT-9
EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL
SIZE 109 BY 3.42 
BGCOLOR 7.      

DEFINE RECTANGLE RECT-10 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 109 BY 5.42
BGCOLOR 7.

DEFINE RECTANGLE RECT-22
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 15 BY 2.3
BGCOLOR 7.


DEFINE VARIABLE l-param-1 AS LOGICAL INITIAL no 
LABEL "Parƒmetro 1"
VIEW-AS TOGGLE-BOX 
SIZE 44 BY 1.08 NO-UNDO. 


DEFINE BUTTON bt-primeiro
    IMAGE FILENAME "image\im-fir"
    SIZE 4 BY 1.

DEFINE BUTTON bt-anterior
    IMAGE FILENAME "image\im-pre"
    SIZE 4 BY 1.

DEFINE BUTTON bt-proximo
    IMAGE FILENAME "image\im-nex"
    SIZE 4 BY 1.

DEFINE BUTTON bt-final
    IMAGE FILENAME "image\im-las"
    SIZE 4 BY 1.

DEFINE BUTTON bt-goto
    IMAGE FILENAME "image\im-vapra"
    SIZE 4 BY 1.

DEFINE BUTTON bt-pesquisa
    IMAGE FILENAME "image\im-zoom"
    SIZE 4 BY 1.

DEFINE BUTTON bt-novo
    IMAGE FILENAME "image\im-add"
    SIZE 4 BY 1.

DEFINE BUTTON bt-copia
    IMAGE FILENAME "image\im-copy"
    SIZE 4 BY 1.

DEFINE BUTTON bt-altera
    IMAGE FILENAME "image\im-mod"
    SIZE 4 BY 1.

DEFINE BUTTON bt-deleta
    IMAGE FILENAME "image\im-era"
    SIZE 4 BY 1.

DEFINE BUTTON bt-cancela
    IMAGE FILENAME "image\im-can"
    SIZE 4 BY 1.

DEFINE BUTTON bt-grava
    IMAGE FILENAME "image\im-chck1"
    SIZE 4 BY 1.

DEFINE BUTTON bt-sai
    IMAGE FILENAME "image\im-exi"
    SIZE 4 BY 1.


DEFINE BUTTON bt-ajuda 
    IMAGE FILENAME "image\im-hel"
    SIZE 4 BY 1.


DEFINE BUTTON bt-confirm AUTO-GO 
    IMAGE-UP FILE "image\im-sav":U
    LABEL "&Confirma" 
    SIZE 13 BY 1.5.

DEFINE RECTANGLE RECT-1
EDGE-PIXELS 2 GRAPHIC-EDGE 
SIZE 108 BY 1.42 
BGCOLOR 7.

DEFINE RECTANGLE RECT-6
EDGE-PIXELS 0
SIZE 81.72 BY .12
BGCOLOR 7.

DEFINE RECTANGLE rt-folder
EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL
SIZE 109 BY 14.5
FGCOLOR 0.

DEFINE RECTANGLE rt-folder-left
EDGE-PIXELS 0
SIZE .43 BY 14
BGCOLOR 15.

DEFINE RECTANGLE rt-folder-right
EDGE-PIXELS 0
SIZE .43 BY 14
BGCOLOR 7.

DEFINE RECTANGLE rt-folder-top
EDGE-PIXELS 0
SIZE 107 BY .12
BGCOLOR 15 .

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
    bt-novo     AT ROW 1.25 COL 4   HELP "Inclui Novo Registro"
    bt-cancela  AT ROW 1.25 COL 8.5  HELP "Cancela"
    bt-sai      AT ROW 1.25 COL 100   HELP "Encerra Programa"
    bt-ajuda    AT ROW 1.25 COL 105 HELP "Ajuda"

/* Colocar aqui os campos chaves do registro */

    c-cod-estabel LABEL "Estabelecimento"
     at row 2.7 col 14 colon-aligned
     view-as fill-in 
     size 17 by .88
     font 1
   

    i-cod-funcionario LABEL "Cod.Funcion rio"
     at row 3.7 col 14 colon-aligned
     view-as fill-in 
     size 17 by .88
     font 1
   
    c-nome NO-LABEL 
     at row 3.7 col 38 colon-aligned
     view-as fill-in 
     size 45 by .88
     font 1
   
    c-it-codigo-ini LABEL "Item"
     at row 4.7 col 14 colon-aligned
     view-as fill-in 
     size 17 by .88
     font 1
   
    c-it-codigo-fim NO-LABEL 
     at row 4.7 col 55 colon-aligned
     view-as fill-in 
     size 17 by .88
     font 1
   
   
    bt-confirm AT ROW 3.7 COL 95
    
    br-digita AT ROW 6.8 COL 2   


    IMAGE-1 AT ROW 4.7    COL 40
    IMAGE-2 AT ROW 4.7    COL 46


    RECT-1  AT ROW 1.05 COL 3    
    RECT-9  AT ROW 2.55 COL 2    


    bt-ok          AT ROW 19.8 COL 45
    bt-del         AT ROW 19.8 COL 80
    bt-cancel      AT ROW 19.8 COL 5             
    rt-botoes      AT ROW 19.6 COL 2

    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
    SIDE-LABELS NO-UNDERLINE THREE-D
    AT COL 1 ROW 1 FONT 1 
    SIZE 110 BY 21. 

  br-digita:NUM-LOCKED-COLUMNS IN FRAME f-relat = 6.
  br-digita:COLUMN-RESIZABLE IN FRAME f-relat = YES.
 
  


DEFINE RECTANGLE ret-par-fill
   EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
   SIZE  74.06 BY .3.


/* ******** Acerto da posi‡Æo dos labels e tamanho dos radio-set ******* */

DEFINE VARIABLE h-label AS WIDGET-HANDLE NO-UNDO.

/* *************************  Create Window  ************************** */

CREATE WINDOW C-Win ASSIGN
   HIDDEN             = YES
   TITLE              = "Consulta Requisi‡äes de Funcion rios"
   HEIGHT             = 21.4
   WIDTH              = 112
   MAX-HEIGHT         = 21.4
   MAX-WIDTH          = 112
   VIRTUAL-HEIGHT     = 21.4
   VIRTUAL-WIDTH      = 112
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
end.

/* ************************  Control Triggers  ************************ */


ON F5 OF c-it-codigo-ini IN FRAME f-relat /* Item */
DO:

      if  valid-handle(wh-pesquisa) and
          wh-pesquisa:TYPE = "PROCEDURE":U and
          wh-pesquisa:FILE-NAME = "inzoom/z02in172.w":U then
            return.

      RUN inzoom/z02in172.w persistent set wh-pesquisa.

      if  not valid-handle(wh-pesquisa) or
              wh-pesquisa:TYPE <> "PROCEDURE":U or
              wh-pesquisa:FILE-NAME <> "inzoom/z02in172.w":U then
          return.

      RUN dispatch IN wh-pesquisa ('initialize':U).

      if valid-handle(wh-pesquisa) and
         wh-pesquisa:TYPE = "PROCEDURE":U and
         wh-pesquisa:FILE-NAME = "inzoom/z02in172.w":U then do:

            RUN pi-entry IN wh-pesquisa.

            define variable c-lista-campo as char init '' no-undo.

            assign c-lista-campo = string(c-it-codigo-ini:handle in frame
                   f-relat) + '|':U + 'it-codigo' .

            run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).


      end.

    assign l-implanta = no.

END.


ON MOUSE-SELECT-DBLCLICK OF c-it-codigo-ini IN FRAME f-relat /* Item */
DO:
  APPLY "f5" TO SELF.
END.


ON F5 OF c-it-codigo-fim IN FRAME f-relat /* Item */
DO:

      if  valid-handle(wh-pesquisa) and
          wh-pesquisa:TYPE = "PROCEDURE":U and
          wh-pesquisa:FILE-NAME = "inzoom/z02in172.w":U then
            return.

      RUN inzoom/z02in172.w persistent set wh-pesquisa.

      if  not valid-handle(wh-pesquisa) or
              wh-pesquisa:TYPE <> "PROCEDURE":U or
              wh-pesquisa:FILE-NAME <> "inzoom/z02in172.w":U then
          return.

      RUN dispatch IN wh-pesquisa ('initialize':U).

      if valid-handle(wh-pesquisa) and
         wh-pesquisa:TYPE = "PROCEDURE":U and
         wh-pesquisa:FILE-NAME = "inzoom/z02in172.w":U then do:

            RUN pi-entry IN wh-pesquisa.

            define variable c-lista-campo as char init '' no-undo.

            assign c-lista-campo = string(c-it-codigo-fim:handle in frame
                   f-relat) + '|':U + 'it-codigo' .

            run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).


      end.

    assign l-implanta = no.

END.


ON MOUSE-SELECT-DBLCLICK OF c-it-codigo-fim IN FRAME f-relat /* Item */
DO:
  APPLY "f5" TO SELF.
END.


ON F5 OF i-cod-funcionario IN FRAME f-relat /* Funcionario */
DO:

    RUN cdp\escd0009.w.

    ASSIGN i-cod-funcionario:SCREEN-VALUE IN FRAME f-relat = string(i-cod-funcionario-amg)
           c-nome:SCREEN-VALUE IN FRAME f-relat = string(c-nome-amg).

END.

ON MOUSE-SELECT-DBLCLICK OF i-cod-funcionario IN FRAME f-relat /* Funcionario */
DO:
  APPLY "f5" TO SELF.
END.


on leave OF i-cod-funcionario in frame f-relat do:

    FIND FIRST am-cd-funcionario-req WHERE
        am-cd-funcionario-req.cod-funcionario = int(i-cod-funcionario:SCREEN-VALUE IN FRAME f-relat)
        NO-LOCK NO-ERROR.

    IF AVAIL am-cd-funcionario-req THEN
        ASSIGN c-nome:SCREEN-VALUE IN FRAME f-relat = am-cd-funcionario-req.nome.
    ELSE
        ASSIGN c-nome:SCREEN-VALUE IN FRAME f-relat = "".


end.    

ON CHOOSE OF bt-confirm IN FRAME f-relat
DO:

    ASSIGN c-cod-estabel         =  c-cod-estabel:SCREEN-VALUE IN FRAME f-relat 
           i-cod-funcionario     =  int(i-cod-funcionario:SCREEN-VALUE IN FRAME f-relat)
           c-it-codigo-ini       =  c-it-codigo-ini:SCREEN-VALUE IN FRAME f-relat       
           c-it-codigo-fim       =  c-it-codigo-fim:SCREEN-VALUE IN FRAME f-relat.       
                              

   RUN pi-monta-browse-mov.


   ASSIGN bt-confirm:SENSITIVE in frame f-relat = yes.

   APPLY "entry" TO c-cod-estabel IN FRAME f-relat.

END.


ON ENTER OF br-digita in frame f-relat
ANYWHERE
DO:
  apply 'tab' to self.
END.

ON ROW-LEAVE OF br-digita in frame f-relat
DO:
    /*  aqui que a grava‡Æo da linha da temp-table ? efetivada.
       Por‚m as valida‡äes dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment rio */   
    
    if br-digita:NEW-ROW in frame f-relat then 
    do transaction on error undo, return no-apply:
        create tt-digita.  
        assign INPUT BROWSE br-digita tt-digita.cod-estabel          
               input browse br-digita tt-digita.cod-funcionario      
               INPUT BROWSE br-digita tt-digita.it-codigo-pai 
               INPUT BROWSE br-digita tt-digita.it-codigo 
               INPUT BROWSE br-digita tt-digita.desc-item
               INPUT BROWSE br-digita tt-digita.dt-requisicao        
               input browse br-digita tt-digita.nr-requisicao        
               INPUT BROWSE br-digita tt-digita.seq-requisicao       
               INPUT BROWSE br-digita tt-digita.quantidade           
               input browse br-digita tt-digita.un          
               INPUT BROWSE br-digita tt-digita.dias-validade   
               input browse br-digita tt-digita.quantidade-max 
               input browse br-digita tt-digita.quantidade-fora 
               input browse br-digita tt-digita.usuar-requisicao     
               input browse br-digita tt-digita.dt-aut-fora-prazo    
               input browse br-digita tt-digita.usuar-aut-fora-prazo.
               br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-relat.    
    end.
    else do transaction on error undo, return no-apply:
        
        IF AVAIL tt-digita THEN
            assign INPUT BROWSE br-digita tt-digita.cod-estabel          
               input browse br-digita tt-digita.cod-funcionario      
               INPUT BROWSE br-digita tt-digita.it-codigo-pai            
               INPUT BROWSE br-digita tt-digita.it-codigo            
               INPUT BROWSE br-digita tt-digita.desc-item
               INPUT BROWSE br-digita tt-digita.dt-requisicao        
               input browse br-digita tt-digita.nr-requisicao        
               INPUT BROWSE br-digita tt-digita.seq-requisicao       
               INPUT BROWSE br-digita tt-digita.quantidade           
               input browse br-digita tt-digita.un         
               INPUT BROWSE br-digita tt-digita.dias-validade   
               input browse br-digita tt-digita.quantidade-max 
               input browse br-digita tt-digita.quantidade-fora 
               input browse br-digita tt-digita.usuar-requisicao     
               input browse br-digita tt-digita.dt-aut-fora-prazo    
               input browse br-digita tt-digita.usuar-aut-fora-prazo.
                   
         IF AVAIL tt-digita THEN DO:

         
            display
                tt-digita.cod-estabel         
                tt-digita.cod-funcionario     
                tt-digita.it-codigo-pai           
                tt-digita.it-codigo           
                tt-digita.desc-item
                tt-digita.dt-requisicao       
                tt-digita.nr-requisicao       
                tt-digita.seq-requisicao      
                tt-digita.quantidade          
                tt-digita.un             
                tt-digita.dias-validade   
                tt-digita.quantidade-max
                tt-digita.quantidade-fora 
                tt-digita.usuar-requisicao     
                tt-digita.dt-aut-fora-prazo   
                tt-digita.usuar-aut-fora-prazo
                with browse br-digita. 

        

         END.
                                          
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
            
   RUN programa-escd0010-z01.w (OUTPUT r-codigo-rwi) NO-ERROR.

   FIND FIRST am-tp-texto-romaneio WHERE
       ROWID(am-tp-texto-romaneio) = r-codigo-rwi
       NO-LOCK NO-ERROR.

   IF AVAIL am-tp-texto-romaneio THEN
       RUN pi-mostra-registro.

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

        i-sequencia-gt LABEL "C¢digo do Usu rio" AT ROW 2.5 COL 18 COLON-ALIGNED
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

        ASSIGN i-empresa-gcf = int(i-sequencia-gt:SCREEN-VALUE IN FRAME gt-frame-1).

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

    ASSIGN c-cod-estabel-gcf = "Exclui Este Registro? ".

    DEFINE RECTANGLE ex-rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 4.50.
    
    DEFINE FRAME ex-frame-1
      
        c-cod-estabel-gcf NO-LABEL 
           at ROW 3 col 10 
        
        ex-rect-1 AT ROW 1.9 COL 2

        ex-bt-cancel      AT ROW 7.3 COL 2.14             
        ex-bt-ok          AT ROW 7.3 COL 13
        ex-rt-botoes      AT ROW 7.0 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "ExclusÆo de Registro no Arquivo" FONT 1
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
    
    DISPLAY c-cod-estabel-gcf 
        WITH FRAME ex-frame-1.

    WAIT-FOR "GO":U OF FRAME ex-frame-1.

END.
*/


ON CHOOSE OF bt-ok IN FRAME f-relat
DO:
IF NOT CAN-FIND(usuar_grp_usuar WHERE
                          usuar_grp_usuar.cod_grp     = "UEP" AND
                          usuar_grp_usuar.cod_usuario = c-seg-usuario
                          NO-LOCK) THEN DO:
                          
                          MESSAGE "VOCE NÇO TEM AUTORIZA€ÇO NESTA OPERA€ÇO" VIEW-AS ALERT-BOX.
               
                     RETURN NO-APPLY.
              END.
    if  br-digita:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-digita.

        FIND FIRST am-cd-movto-req WHERE
            am-cd-movto-req.nr-requisicao  = tt-digita.nr-requisicao AND
            am-cd-movto-req.seq-requisicao = tt-digita.seq-requisicao
            USE-INDEX requisicao NO-ERROR.

        IF AVAIL am-cd-movto-req THEN DO:

        ASSIGN am-cd-movto-req.dt-aut-fora-prazo    = today
               am-cd-movto-req.usuar-aut-fora-prazo = c-seg-usuario
               am-cd-movto-req.int-1                = tt-digita.quantidade-fora.

        ASSIGN tt-digita.dt-aut-fora-prazo    = today
               tt-digita.usuar-aut-fora-prazo = c-seg-usuario.

        END.

        open query br-digita for each tt-digita.
        apply 'entry' to tt-digita.cod-funcionario in browse br-digita. 

        if num-results("br-digita") > 0 THEN 
           get current br-digita.


    end.

    APPLY 'entry' TO c-cod-estabel IN FRAME f-relat.


END.



ON CHOOSE OF bt-del IN FRAME f-relat
DO:

IF NOT CAN-FIND(usuar_grp_usuar WHERE
                          usuar_grp_usuar.cod_grp     = "UEP" AND
                          usuar_grp_usuar.cod_usuario = c-seg-usuario
                          NO-LOCK) THEN DO:
                          
                          MESSAGE "VOCE NÇO TEM AUTORIZA€ÇO NESTA OPERA€ÇO" VIEW-AS ALERT-BOX.
               
                     RETURN NO-APPLY.
              END.

    if  br-digita:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-digita.

        FIND FIRST am-cd-movto-req WHERE
            am-cd-movto-req.nr-requisicao  = tt-digita.nr-requisicao AND
            am-cd-movto-req.seq-requisicao = tt-digita.seq-requisicao
            USE-INDEX requisicao NO-ERROR.

        IF AVAIL am-cd-movto-req THEN DO:

        ASSIGN am-cd-movto-req.dt-aut-fora-prazo    = ?
               am-cd-movto-req.usuar-aut-fora-prazo = ""
               am-cd-movto-req.INT-1 = 0.

        ASSIGN tt-digita.dt-aut-fora-prazo    = ?
               tt-digita.usuar-aut-fora-prazo = ""
               tt-digita.quantidade-fora = 0.

        END.

        open query br-digita for each tt-digita.
        apply 'entry' to tt-digita.cod-funcionario in browse br-digita. 

        if num-results("br-digita") > 0 THEN 
           get current br-digita.


    end.

    APPLY 'entry' TO c-cod-estabel IN FRAME f-relat.


END.


ON CHOOSE OF bt-cancela IN FRAME f-relat
DO:


   RUN pi-disable-campos.
   RUN pi-disable-bt-grava.
   RUN pi-enable-outros-botoes.
   RUN pi-limpa-campos.

   APPLY "entry" TO c-cod-estabel IN FRAME f-relat.

END.

ON entry OF bt-novo IN FRAME f-relat
DO: 

   RUN pi-enable-bt-cancela.
   RUN pi-disable-outros-botoes.


   RUN pi-limpa-campos.
   
   RUN pi-enable-campos.

   APPLY "entry" TO c-cod-estabel IN FRAME f-relat.

END.



ON CHOOSE OF bt-novo IN FRAME f-relat
DO: 

   RUN pi-enable-bt-cancela.
   RUN pi-disable-outros-botoes.


   RUN pi-limpa-campos.
   
   RUN pi-enable-campos.

   APPLY "entry" TO c-cod-estabel IN FRAME f-relat.

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

    /* Aqui colocar as valida‡äes dos campos antes de serem
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

ON CHOOSE OF bt-cancel IN FRAME f-relat
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

c-it-codigo-ini:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME f-relat.
c-it-codigo-fim:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME f-relat.
i-cod-funcionario:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME f-relat.


ASSIGN CURRENT-WINDOW             = {&WINDOW-NAME}
   THIS-PROCEDURE:CURRENT-WINDOW  = {&WINDOW-NAME}.

assign v-cod-prog-gerado = "escd0010".



def var c-tit as char no-undo.

ASSIGN c-tit = "escd0010 - Consulta Requisi‡äes de Funcion rios".
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
** Procedure de troca de p gina por CTRL-TAB 
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

    IF SESSION:SET-WAIT-STATE("":U) THEN.
    RUN enable_UI.

    RUN pi-disable-bt-grava.   


    assign v-cod-pg-mouse-selec = "im-pg-sel".


     view c-win.
     apply "entry" to frame f-Relat.
     apply "entry" to c-win.



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
   ENABLE  
          bt-novo  
          bt-cancela 
          bt-sai
          bt-ajuda
           
   WITH FRAME f-relat IN WINDOW C-Win.

   {&OPEN-BROWSERS-IN-QUERY-f-relat}
   {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}


   DISPLAY 
   WITH FRAME f-pg-imp IN WINDOW C-Win.

   ENABLE 
   WITH FRAME f-pg-imp IN WINDOW C-Win.
   
   DISPLAY 
   WITH FRAME f-pg-par IN WINDOW C-Win.

   ENABLE 
   WITH FRAME f-pg-par IN WINDOW C-Win.

   DISPLAY 
   WITH FRAME f-pg-dig IN WINDOW C-Win.

   ENABLE 
   WITH FRAME f-pg-dig IN WINDOW C-Win.


  ENABLE br-digita   
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


PROCEDURE pi-limpa-campos.

    ASSIGN c-cod-estabel:SCREEN-VALUE IN FRAME f-relat     = STRING({cdp\poloestab.i 422})  /*solic-318*/ 
           i-cod-funcionario:SCREEN-VALUE IN FRAME f-relat = ""
           c-it-codigo-ini:SCREEN-VALUE IN FRAME f-relat = ""
           c-it-codigo-fim:SCREEN-VALUE IN FRAME f-relat = "ZZZZZZZZZZZZ".

    FOR EACH tt-digita.
        DELETE tt-digita.
    END.


     open query br-digita for each tt-digita.
     apply 'entry' to tt-digita.cod-funcionario in browse br-digita. 

     if num-results("br-digita") > 0 THEN DO:
        get current br-digita.
     END.


END PROCEDURE.

PROCEDURE pi-enable-campos.

    ENABLE c-cod-estabel
           i-cod-funcionario
           c-it-codigo-ini
           c-it-codigo-fim
        WITH FRAME f-relat.

    ENABLE bt-confirm bt-ok bt-del bt-cancel
        WITH FRAME f-relat.

    APPLY "entry" TO c-cod-estabel IN FRAME f-relat.


END PROCEDURE.


PROCEDURE pi-disable-campos.


END PROCEDURE.

PROCEDURE pi-le-pela-chave.


    RETURN "ok".

END PROCEDURE.


PROCEDURE pi-grava-registro.


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
           bt-novo:SENSITIVE in frame f-relat       = no 
         /*  bt-copia:SENSITIVE in frame f-relat    = no 
           bt-altera:SENSITIVE in frame f-relat     = no 
           bt-deleta:SENSITIVE in frame f-relat     = no */
           bt-sai:SENSITIVE in frame f-relat        = yes. 
   
END PROCEDURE.

PROCEDURE pi-enable-outros-botoes.

   ENABLE /*bt-novo*/
          bt-cancela   
          bt-sai
   WITH FRAME f-relat IN WINDOW C-Win.
   
END PROCEDURE.

PROCEDURE pi-monta-browse-mov:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
Do:
     /** Montando a tabela para tt-digita ***/

    FOR EACH tt-digita.
        DELETE tt-digita.
    END.


    ASSIGN c-cod-estabel     =  c-cod-estabel:SCREEN-VALUE IN FRAME f-relat 
           i-cod-funcionario =  int(i-cod-funcionario:SCREEN-VALUE IN FRAME f-relat)
           c-it-codigo-ini   =  c-it-codigo-ini:SCREEN-VALUE IN FRAME f-relat
           c-it-codigo-fim   =  c-it-codigo-fim:SCREEN-VALUE IN FRAME f-relat.


    FOR EACH am-cd-movto-req WHERE
        am-cd-movto-req.cod-estabel      = c-cod-estabel      AND
        am-cd-movto-req.cod-funcionario  = i-cod-funcionario  AND
        am-cd-movto-req.it-codigo       >= c-it-codigo-ini    AND
        am-cd-movto-req.it-codigo       <= c-it-codigo-fim
         use-index codigo.
        
        find first it-requisicao where 
         it-requisicao.nr-requisicao = am-cd-movto-req.nr-requisicao and
         it-requisicao.it-codigo =     am-cd-movto-req.it-codigo and
         it-requisicao.sequencia  =    am-cd-movto-req.seq-requisicao   and
          it-requisicao.cod-estabel =  am-cd-movto-req.cod-estabel no-lock no-error.
          
          if not avail  it-requisicao AND am-cd-movto-req.nr-requisicao <> 0 then  do:
          
          delete am-cd-movto-req.
          next.
          
          end.
          
         
        
        

        FIND FIRST ITEM WHERE
            ITEM.it-codigo = am-cd-movto-req.it-codigo
            NO-LOCK NO-ERROR.

        FIND FIRST  am-cd-item-vida-util WHERE
             am-cd-item-vida-util.cod-estabel = am-cd-movto-req.cod-estabel AND
             am-cd-item-vida-util.it-codigo   = am-cd-movto-req.it-codigo NO-LOCK NO-ERROR.

         FIND FIRST  b-am-cd-item-vida-util WHERE
             b-am-cd-item-vida-util.cod-estabel = am-cd-movto-req.cod-estabel AND
             b-am-cd-item-vida-util.it-codigo   = am-cd-item-vida-util.char-1 NO-LOCK NO-ERROR.

            

        IF NOT AVAIL ITEM THEN NEXT.

        CREATE tt-digita.
        
        ASSIGN tt-digita.cod-estabel          = am-cd-movto-req.cod-estabel 
               tt-digita.cod-funcionario      = am-cd-movto-req.cod-funcionario
               tt-digita.it-codigo-pai        = IF AVAIL am-cd-item-vida-util THEN am-cd-item-vida-util.char-1  ELSE ""
               tt-digita.dias-validade        = IF AVAIL b-am-cd-item-vida-util THEN b-am-cd-item-vida-util.dias-validade  ELSE 0 
               tt-digita.quantidade-max       = IF AVAIL b-am-cd-item-vida-util THEN b-am-cd-item-vida-util.qtde-max-req  ELSE 0
               tt-digita.quantidade-fora      = am-cd-movto-req.int-1
               tt-digita.it-codigo            = am-cd-movto-req.it-codigo            
               tt-digita.desc-item            = item.desc-item            
               tt-digita.dt-requisicao        = am-cd-movto-req.dt-requisicao        
               tt-digita.nr-requisicao        = am-cd-movto-req.nr-requisicao        
               tt-digita.seq-requisicao       = am-cd-movto-req.seq-requisicao       
               tt-digita.quantidade           = am-cd-movto-req.quantidade           
               tt-digita.un                   = am-cd-movto-req.un                   
               tt-digita.usuar-requisicao     = am-cd-movto-req.usuar-requisicao     
               tt-digita.dt-aut-fora-prazo    = am-cd-movto-req.dt-aut-fora-prazo    
               tt-digita.usuar-aut-fora-prazo = am-cd-movto-req.usuar-aut-fora-prazo.  

    END.


       /*** habilita **/

     open query br-digita for each tt-digita.
     apply 'entry' to tt-digita.cod-funcionario in browse br-digita. 

     if num-results("br-digita") > 0 THEN DO:
        get current br-digita.
     END.

  end. 

END PROCEDURE. 


