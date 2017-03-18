/*****************************************************************************
**       Programa: essf0003a-amg.p
**       Data....: 05/12/2007
**       Objetivo: Verifica Qtd. Bobinas com Emendas em Pedido de Venda
**       
*******************************************************************************/

DEFINE INPUT PARAMETER nr-pedido-jr       AS INT NO-UNDO.

DEFINE NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEFINE  VAR bob-ped-53  AS INT NO-UNDO.
DEFINE  VAR bob-pro-53  AS INT NO-UNDO.
DEFINE  VAR bob-eme-53  AS INT NO-UNDO.
DEFINE VARIABLE i-nr-pedido AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-nome-abrev AS CHARACTER  NO-UNDO.
DEFINE VARIABLE emenda-jr AS INTEGER    NO-UNDO.

DEFINE BUFFER bf-ped-venda FOR ped-venda.                          
DEFINE BUFFER bfi-ped-venda FOR ped-venda.     
DEFINE BUFFER bf-ped-item  FOR ped-item.                          
DEFINE BUFFER bf-pallet    FOR pallet.                          
DEFINE BUFFER bf-it-pallet FOR it-pallet.                          
DEFINE VARIABLE os-file AS CHARACTER NO-UNDO FORMAT "x(60)" LABEL "File". 
/*------------------------------------------------------------------------------*/


ASSIGN bob-ped-53 = 0
       bob-pro-53 = 0
       bob-eme-53 = 0.


FIND FIRST bf-ped-venda WHERE 
           bf-ped-venda.nr-pedido = nr-pedido-jr
           NO-LOCK  NO-ERROR.

FIND FIRST bf-ped-item OF bf-ped-venda WHERE
           bf-ped-item.ind-componen  <> 3 
           NO-LOCK NO-ERROR.

IF AVAIL bf-ped-venda   THEN DO:
    FIND FIRST if-ped-venda WHERE if-ped-venda.nr-pedido =  bf-ped-venda.nr-pedido NO-ERROR.
    IF AVAIL if-ped-venda THEN DO:
        FIND FIRST bfi-ped-venda WHERE 
           bfi-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac
           NO-LOCK  NO-ERROR.     
      


    END.
END.


IF AVAIL bf-ped-venda AND AVAIL bf-ped-item THEN DO:


IF AVAIL  bfi-ped-venda  THEN
    ASSIGN c-nome-abrev = bfi-ped-venda.nome-abrev.
ELSE
     
    ASSIGN c-nome-abrev = bf-ped-venda.nome-abrev.

     

    ASSIGN i-nr-pedido = bf-ped-venda.nr-pedido.

    FIND FIRST var-result WHERE
         var-result.nome-var     = "qtdbob"                AND 
         var-result.nr-estrut    = bf-ped-item.nr-config   AND
         var-result.item-cotacao = bf-ped-item.it-codigo 
         NO-LOCK NO-ERROR.

    IF AVAIL var-result THEN
         ASSIGN bob-ped-53 = var-result.valor-dec.

    FOR EACH bf-pallet WHERE
        bf-pallet.nr-pedido   = bf-ped-venda.nr-pedido AND
        bf-pallet.cod-estabel = bf-ped-venda.cod-estabel
        USE-INDEX pedido
        NO-LOCK ,

        EACH bf-it-pallet OF bf-pallet NO-LOCK.

          ASSIGN bob-pro-53 = bob-pro-53 + 1.
        
          FIND FIRST lote-carac-tec WHERE
                          lote-carac-tec.it-codigo = bf-it-pallet.it-codigo
                          and lote-carac-tec.lote  = bf-it-pallet.lote-bobina
                          and lote-carac-tec.cd-comp = "emenda"
                          NO-LOCK NO-ERROR.
        
          if avail lote-carac-tec then
             ASSIGN emenda-jr = lote-carac-tec.vl-resul.
        
          IF emenda-jr <> 0 THEN
              ASSIGN bob-eme-53 = bob-eme-53 + 1.


    END.



END.                                              

/*
MESSAGE "Bobs Ped  " bob-ped-53 SKIP
        "Bobs Prod " bob-pro-53 SKIP
        "Bobs Emen " bob-eme-53
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/

/* Dialog-box com Mensagem Final */

IF (bob-eme-53 / bob-pro-53) * 100 >  30 THEN DO:



DEFINE BUTTON db-bt-cancel AUTO-END-KEY 
     LABEL "&Fechar" 
     SIZE 10 BY 1
     BGCOLOR 8.

DEFINE RECTANGLE db-rt-botoes
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 58 BY 1.42
     BGCOLOR 7.  

DEFINE VARIABLE c-mensagem AS CHARACTER FORMAT  "X(45)" NO-UNDO.

ASSIGN c-mensagem = 
    "Quantidade de emendas no pedido > 30%:" + STRING(((bob-eme-53 / bob-pro-53) * 100), ">>9.99" ) + CHR(10) +
    "Bobinas paletizadas no pedido: " + STRING (bob-pro-53) + CHR(10) +
    "Bobinas c/Emendas Encontradas: " + STRING (bob-eme-53)    .

        OS-FILE = "m:\dts\log_prd".

        FILE-INFO:FILE-NAME = os-file.  
        
        IF FILE-INFO:PATHNAME = ? THEN DO:
           OS-FILE = "\\ungusb-vap01\Sistemas\DTS\Log_Prd\essf0003-30.LOG".
        
        END.
        ELSE DO:
            OS-FILE = "m:\dts\log_prd\essf0003-30.LOG".
        END.
        
        
        
        OUTPUT TO VALUE(OS-FILE) APPEND.
                DISP string(TODAY) "-" string(TIME,"hh:mm:ss") "-" 
                 " Aviso visto por: " string(c-seg-usuario) 
                 " Pedido:" i-nr-pedido NO-LABEL
                 " Cliente:" c-nome-abrev FORMAT "x(12)" NO-LABEL
                 " mensagem: " c-mensagem FORMAT "x(200)" NO-LABEL
                 
                 WITH WIDTH 300.
        OUTPUT CLOSE.
        


DEFINE RECTANGLE db-rect-1
 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
 SIZE 59 BY 4.30.

DEFINE FRAME db-frame-1

    c-mensagem NO-LABEL VIEW-AS EDITOR  SIZE 55 BY 3
       at ROW 2.7 col 2 NO-TAB-STOP FONT 0
    
    db-rect-1 AT ROW 1.9 COL 01

    db-bt-cancel      AT ROW 7.3 COL 23             
    db-rt-botoes      AT ROW 7.0 COL 1
    SPACE(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
         THREE-D SCROLLABLE TITLE "* A T E N € Ç O *" FONT 1
         DEFAULT-BUTTON db-bt-cancel CANCEL-BUTTON db-bt-cancel.


  DISPLAY c-mensagem WITH FRAME db-frame-1.
  
  ASSIGN c-mensagem:SENSITIVE = YES
         c-mensagem:read-only = yes
             c-mensagem:AUTO-RESIZE = yes.

ENABLE db-bt-cancel 
    WITH FRAME db-frame-1. 

WAIT-FOR "GO":U OF FRAME db-frame-1.

/* Fim do Dialog-box com Mensagem Final */




END.










return 'OK'.


/* fim do programa */
