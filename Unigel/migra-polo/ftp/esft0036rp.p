&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*
**    Programa: esft0036RP.P
**    Objetivo: Romaneio
**       Autor: Edgar Bispo - Datasul SP
** Atualiza‡Æo: 22/12/2003
*/

{include/i-prgvrs.i esft0036RP "2.04.00.001"}
def buffer empresa for mgmulti.empresa.
/*----- TEMP-TABLE DE PARAMETROS -----*/
{ftp/esft0036.i}

def temp-table tt-raw-digita
   field raw-digita      as raw.

/*---- DEINICAO DE PARAMETROS DO PROGRAMA -----*/
define input param raw-param as raw no-undo.
define input param table for tt-raw-digita.

/*----- DEFINICAO DE VARIAVEIS LOCAIS -----*/
DEF VAR i-cdd-embarq   LIKE embarque.cdd-embarq   NO-UNDO.
def VAR h-acomp         AS HANDLE                   NO-UNDO. 
DEF VAR c-dia-semana    AS CHAR EXTENT 7 
    INITIAL ["Domingo", "Segunda-feira", "Ter‡a-feira", 
             "Quarta-feira", "Quinta-feira", "Sexta-feira", "S bado"].
DEFINE NEW GLOBAL SHARED VARIABLE c-cod-usuario AS CHARACTER  NO-UNDO.

/*----- PREPARACAO DOS PARAMETROS -----*/
create tt-param.
raw-transfer raw-param to tt-param.

FOR EACH tt-raw-digita.

    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.RAW TO tt-digita.

END.

DEF TEMP-TABLE tt-imp NO-UNDO
    FIELD cdd-embarq       LIKE embarque.cdd-embarq
    FIELD dt-embarque       LIKE embarque.dt-embarque
    FIELD dia-semana        AS CHAR
    FIELD motorista         LIKE embarque.motorista
    FIELD placa             LIKE embarque.placa
    FIELD cod-transp        LIKE transporte.cod-transp
    FIELD transportadora    LIKE transporte.nome
    .

DEF TEMP-TABLE tt-it-imp NO-UNDO
    FIELD cdd-embarq   LIKE embarque.cdd-embarq   
    FIELD nr-pedido     LIKE ped-venda.nr-pedido
    FIELD tp-pedido     LIKE ped-venda.tp-pedido    
    FIELD c-linha       AS CHAR FORMAT "x(08)"      INITIAL "________"
    FIELD nr-pallet     LIKE pallet.nr-pallet       
    field nome-abrev    LIKE ped-venda.nome-abrev
    FIELD it-codigo     LIKE ped-item.it-codigo
    FIELD largura       AS INT FORMAT "->>,>>9"
    FIELD peso-liq      AS DEC FORMAT ">>,>>>,>>9.99"
    FIELD peso-bruto    AS DEC FORMAT ">>,>>>,>>9.99"
    FIELD qt-alocada    AS DEC FORMAT ">>,>>>,>>9.99"
    FIELD dt-entrega    LIKE ped-item.dt-entrega    
    FIELD destino       AS CHAR FORMAT "x"          
    FIELD qt-bobinas    LIKE pallet.nr-bobinas      
    INDEX in-it-imp cdd-embarq nr-pedido nr-pallet.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.

/*----- DEFINICAO DE STREAM -----*/
define new shared stream str-rp.

/*----- INCLUDES PARA O RELATORIO -----*/
{include/i-rpvar.i}

FORM
    "                       " AT 01     SKIP /*                 [ Entrada :__________ ]                    [ Saida.. :__________ ]                    [ N.Fiscal:__________ ]*/
    "Data de Embarque ....: " AT 01 tt-imp.dt-embarque FORMAT "99/99/9999" "Dia da Semana " AT 78   tt-imp.dia-semana FORMAT "x(20)"
    "Motorista............: " AT 01 tt-imp.motorista
    "Placa do Veiculo.....: " AT 01 tt-imp.placa
    "Transportadora ......: " AT 01 tt-imp.cod-transp   tt-imp.transportadora
    "                       " AT 01 SKIP /*[   2]*/
    "___________________________________________________________________________________________________________________________________________________________________________"  SKIP
    "                         I N S P E C A O    D O    V E I C U L O !   D E S T I N O    F I N A L      !       "  SKIP
    "                         (   ) Aprovado  . . . . . . (10 pontos) ! 1- Transportadora POLO            !     "    SKIP
    "                                                                 ! 2- Transportadora CLIENTE         !     "       SKIP
    "                         (   ) REPROVADO/RECHECADO . ( 0 ponto ) ! 3- Cliente Final                  !     "          SKIP
    "___________________________________________________________________________________________________________________________________________________________________________"  SKIP
    " NF. NR.         PEDIDO PALLET     CLIENTE       FILME               LARG       LIQUIDO         BRUTO DATA DES.  DESTINO    BOBS                       EM (DATA)     PONTOS"  SKIP
    "___________________________________________________________________________________________________________________________________________________________________________"  SKIP
    WITH STREAM-IO WIDTH 300 FRAME f-embarque NO-BOX NO-LABELS DOWN.

FORM
    tt-it-imp.c-linha       
    tt-it-imp.tp-pedido     
    tt-it-imp.nr-pedido     
    tt-it-imp.nr-pallet
    tt-it-imp.nome-abrev
    tt-it-imp.it-codigo
    tt-it-imp.largura
    tt-it-imp.peso-liq
    tt-it-imp.peso-bruto
    tt-it-imp.dt-entrega
    tt-it-imp.destino       AT 117
    tt-it-imp.qt-bobinas
    "                    ____/____/____  ______"
    WITH STREAM-IO WIDTH 300 FRAME f-item   NO-BOX NO-LABELS DOWN.

FORM
    "___________________________________________________________________________________________________________________________________________________________________________"  SKIP
    "Prioridade de Entrega : ________________________________________________________________________________"  SKIP
    "___________________________________________________________________________________________________________________________________________________________________________"  SKIP
    "IMPORTANTE : Para possivel anormalidade na entrega, favor descrever Motivo, Cliente, e Assinar:"   SKIP
    "___________________________________________________________________________________________________________________________________________________________________________"  SKIP
    "___________________________________________________________________________________________________________________________________________________________________________"  SKIP
    "___________________________________________________________________________________________________________________________________________________________________________"  SKIP
    "___________________________________________________________________________________________________________________________________________________________________________"  SKIP
    "___________________________________________________________________________________________________________________________________________________________________________"  SKIP
    "___________________________________________________________________________________________________________________________________________________________________________"  SKIP
    "___________________________________________________________________________________________________________________________________________________________________________"  SKIP
    "___________________________________________________________________________________________________________________________________________________________________________"  SKIP
    "                        CONFERI AS MERCADORIAS CONSTANTES                                           RECEBI AS MERCADORIAS CONSTANTES"       SKIP
    "                        DESTE ROMANEIO EM _____/_____/______.                                       DESTE ROMANEIO EM _____/_____/______."  SKIP
    "   "   SKIP
    "   "   SKIP
    "                        ------------------------------------                                        ------------------------------------"   SKIP
    "                        ASSINATURA  DO  CONFERENTE                                                    ASSINATURA DO MOTORISTA"         SKIP
    "___________________________________________________________________________________________________________________________________________________________________________"  SKIP
    "OBSERVACAO : A Transportadora : Este documento devera ser encaminhado para a POLO,"        SKIP
    "                                (A/C DIVISAO EXPEDICAO) apos efetuadas todas as entregas."
    WITH STREAM-IO WIDTH 300 FRAME f-rodape   NO-BOX NO-LABELS DOWN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 8.54
         WIDTH              = 30.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST empresa NO-LOCK WHERE empresa.ep-codigo = STRING({cdp\poloestab.i 420}).  /*solic-318*/ 
 
assign c-programa     = "esft0036RP"
       c-versao       = "2.04"
       c-revisao      = ".00.001"
       c-empresa      = empresa.razao
       c-sistema      = "Espec¡fico"
       c-titulo-relat = "ROMANEIO  DO DIA " + STRING(TODAY,"99/99/9999")
       c-rodape       = "DATASUL - " + c-sistema + " - " + c-prg-obj + " - V:" + c-prg-vrs
       c-rodape       = fill("-", 179 - length(c-rodape)) + c-rodape.

/*----- DIRECIONAMENTO DO RELATORIO -----*/
{include/i-rpout.i}

/*---- DEFINICAO DE FRAMES -----*/
FORM HEADER
     SKIP(1)
     c-empresa c-titulo-relat  FORMAT "X(30)" AT 55  
     "NR. " + string(i-cdd-embarq) FORMAT "x(10)" AT 104
     c-prg-obj FORMAT "x(10)" AT 126
     "PAG.N."  AT 163 PAGE-NUMBER FORMAT "99" SKIP(1)
    WITH FRAME f-cabec WIDTH 250 NO-LABELS NO-BOX PAGE-TOP.


view frame f-cabec.

RUN pi-executar.

{include/i-rpclo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-carrega-tt-imp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-tt-imp Procedure 
PROCEDURE pi-carrega-tt-imp :
/*------------------------------------------------------------------------------
  Purpose:     Procedure para carregar a temp-table de impressao
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH tt-imp:    DELETE tt-imp.  END.
    
    FOR EACH embarque NO-LOCK
        WHERE embarque.cdd-embarq  >= tt-param.cdd-embarq-ini
          AND embarque.cdd-embarq  <= tt-param.cdd-embarq-fim
          AND embarque.dt-embarque  >= tt-param.dt-periodo-ini
          AND embarque.dt-embarque  <= tt-param.dt-periodo-fim
          AND embarque.nome-transp  >= tt-param.nome-transp-ini
          AND embarque.nome-transp  <= tt-param.nome-transp-fim:

        CREATE tt-imp.
        ASSIGN 
            tt-imp.cdd-embarq      = embarque.cdd-embarq
            tt-imp.dt-embarque      = embarque.dt-embarque
            tt-imp.dia-semana       = c-dia-semana[WEEKDAY(embarque.dt-embarque)]
            tt-imp.motorista        = embarque.motorista
            tt-imp.placa            = embarque.placa.

        FIND FIRST transporte NO-LOCK
            WHERE transporte.nome-abrev = embarque.nome-transp
            NO-ERROR.
        IF AVAIL transporte THEN
            ASSIGN
                tt-imp.cod-transp       = transporte.cod-transp
                tt-imp.transportadora   = transporte.nome
            .
     
        FOR EACH it-dep-fat OF embarque:
            
            FIND FIRST ped-item OF it-dep-fat NO-LOCK NO-ERROR.
            IF NOT AVAIL ped-item THEN NEXT.

            FIND FIRST ped-venda OF ped-item NO-LOCK NO-ERROR.
           
            FIND FIRST tt-it-imp
                WHERE tt-it-imp.cdd-embarq = embarque.cdd-embarq
                  AND tt-it-imp.nr-pedido   = ped-venda.nr-pedido
                  AND tt-it-imp.nr-pallet   = it-dep-fat.nr-serlote
                NO-ERROR.
            IF NOT AVAIL tt-it-imp THEN DO:

                CREATE tt-it-imp.
                ASSIGN 
                    tt-it-imp.cdd-embarq   = embarque.cdd-embarq
                    tt-it-imp.nr-pedido     = ped-venda.nr-pedido 
                    tt-it-imp.nr-pallet     = it-dep-fat.nr-serlote
                    tt-it-imp.tp-pedido     = ped-venda.tp-pedido
                    tt-it-imp.nome-abrev    = ped-venda.nome-abrev
                    tt-it-imp.dt-entrega    = ped-item.dt-entrega
                    tt-it-imp.it-codigo     = it-dep-fat.it-codigo
                    .

            END.

             

            /* Carregar o peso liquido e o peso bruto */
            FIND FIRST pallet NO-LOCK
                WHERE pallet.cod-estabel    = it-dep-fat.cod-estabel
                  AND pallet.it-codigo      = it-dep-fat.it-codigo
                  AND pallet.nr-pallet      = it-dep-fat.nr-serlote
                NO-ERROR.
            IF NOT AVAIL pallet THEN
            FIND FIRST pallet NO-LOCK
                WHERE /*pallet.cod-estabel    = it-dep-fat.cod-estabel
                  AND*/ pallet.it-codigo      = it-dep-fat.it-codigo
                  AND pallet.nr-pallet      = it-dep-fat.nr-serlote
                NO-ERROR.

            

            IF AVAIL pallet THEN
                ASSIGN
                    tt-it-imp.peso-liq  = tt-it-imp.peso-liq    + pallet.peso-liquido
                    tt-it-imp.peso-bruto= tt-it-imp.peso-bruto  + pallet.peso-bruto
                    tt-it-imp.qt-bobinas= tt-it-imp.qt-bobinas  + pallet.nr-bobinas
                    tt-it-imp.qt-alocada= tt-it-imp.qt-alocada  + it-dep-fat.qt-alocada.
                                
            /* Carregar a largura */
              FIND FIRST ITEM WHERE ITEM.IT-CODIGO = it-dep-fat.it-codigo NO-LOCK NO-ERROR.
 
            FIND FIRST lote-carac-tec NO-LOCK
                WHERE lote-carac-tec.it-codigo = it-dep-fat.it-codigo
                  AND lote-carac-tec.lote      = it-dep-fat.nr-serlote
                  AND lote-carac-tec.cd-FOLHA  = (IF AVAIL ITEM THEN item.cd-folh-lote ELSE "cut tnt")
                  AND lote-carac-tec.cd-COMP  = "largura"
                NO-ERROR.

          

            IF NOT AVAIL lote-carac-tec OR round(lote-carac-tec.vl-result,0) = 0 THEN DO:

                FIND last it-pallet OF pallet NO-LOCK NO-ERROR.

                IF AVAIL it-pallet THEN DO:
                      FIND FIRST lote-carac-tec NO-LOCK
                        WHERE lote-carac-tec.it-codigo = it-dep-fat.it-codigo
                          AND lote-carac-tec.lote      = it-pallet.lote
                          AND lote-carac-tec.cd-FOLHA  = (IF AVAIL ITEM THEN item.cd-folh-lote ELSE "cut tnt")
                          AND lote-carac-tec.cd-COMP  = "largura"
                        NO-ERROR.
                END.

            END.

            IF AVAIL lote-carac-tec THEN
                ASSIGN
                    tt-it-imp.largura   = tt-it-imp.largura + round(lote-carac-tec.vl-result,0).

        END. /* FOR EACH it-dep-fat OF embarque: */

    END. /* FOR EACH embarque NO-LOCK */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-executar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar Procedure 
PROCEDURE pi-executar :
/*
** EXECUTA PROCESSO DE IMPRESSAO
*/

    run utp/ut-acomp.p persistent set h-acomp.
    
    run pi-inicializar in h-acomp("Gerando dados para relat¢rio").

    RUN pi-carrega-tt-imp.

    FOR EACH tt-imp NO-LOCK:

        i-cdd-embarq = tt-imp.cdd-embarq.
        VIEW frame f-cabec.

        RUN pi-acompanhar IN h-acomp ("Imprimindo... " + string(tt-imp.cdd-embarq)).
        
        DISP
            tt-imp.dt-embarque   
            tt-imp.dia-semana    
            tt-imp.motorista     
            tt-imp.placa         
            tt-imp.cod-transp    
            tt-imp.transportadora
            WITH FRAME f-embarque.
        DOWN WITH FRAME f-embarque.

        FOR EACH tt-it-imp 
            WHERE tt-it-imp.cdd-embarq = tt-imp.cdd-embarq
            BREAK   BY tt-it-imp.cdd-embarq
                    BY tt-it-imp.nr-pedido
                    BY tt-it-imp.nr-pallet:

            ACCUMULATE
                tt-it-imp.nr-pallet     (COUNT BY tt-it-imp.cdd-embarq)
                tt-it-imp.peso-liq      (TOTAL BY tt-it-imp.cdd-embarq)
                tt-it-imp.peso-bruto    (TOTAL BY tt-it-imp.cdd-embarq)
                tt-it-imp.qt-bobinas    (TOTAL BY tt-it-imp.cdd-embarq)
                tt-it-imp.qt-alocada    (TOTAL BY tt-it-imp.cdd-embarq)
                tt-it-imp.peso-liq      (SUB-TOTAL BY tt-it-imp.nr-pedido)
                tt-it-imp.peso-bruto    (SUB-TOTAL BY tt-it-imp.nr-pedido)
                tt-it-imp.qt-bobinas    (SUB-TOTAL BY tt-it-imp.nr-pedido)
                tt-it-imp.nr-pallet     (COUNT BY     tt-it-imp.nr-pedido)
                .

            DISP
                tt-it-imp.c-linha       WHEN FIRST-OF(tt-it-imp.nr-pedido)
                tt-it-imp.tp-pedido     WHEN FIRST-OF(tt-it-imp.nr-pedido)
                tt-it-imp.nr-pedido     WHEN FIRST-OF(tt-it-imp.nr-pedido)
                tt-it-imp.nr-pallet  
                tt-it-imp.nome-abrev 
                tt-it-imp.it-codigo  
                tt-it-imp.largura    
                tt-it-imp.peso-liq   
                tt-it-imp.peso-bruto 
                tt-it-imp.dt-entrega 
                tt-it-imp.destino
                tt-it-imp.qt-bobinas 
            WITH FRAME f-item.
            DOWN WITH FRAME f-item.

            IF LINE-COUNTER = 65 THEN DO:

                DISP
                    tt-imp.dt-embarque   
                    tt-imp.dia-semana    
                    tt-imp.motorista     
                    tt-imp.placa         
                    tt-imp.cod-transp    
                    tt-imp.transportadora
                    WITH FRAME f-embarque.
                DOWN WITH FRAME f-embarque.

            END.

            IF LAST-OF(tt-it-imp.nr-pedido) THEN DO:


                DOWN 1 WITH FRAME f-item.
                DISP
                    "Sub-total...:"   @ tt-it-imp.nome-abrev
                    STRING(ACCUM COUNT BY tt-it-imp.nr-pedido tt-it-imp.nr-pallet) + " PLTS." @ tt-it-imp.it-codigo
                    ACCUM SUB-TOTAL BY tt-it-imp.nr-pedido tt-it-imp.peso-liq   @ tt-it-imp.peso-liq  
                    ACCUM SUB-TOTAL BY tt-it-imp.nr-pedido tt-it-imp.peso-bruto @ tt-it-imp.peso-bruto
                    ACCUM SUB-TOTAL BY tt-it-imp.nr-pedido tt-it-imp.qt-bobinas @ tt-it-imp.qt-bobinas
                    WITH FRAME f-item.
                DOWN WITH FRAME f-item.

                PUT
                    "___________________________________________________________________________________________________________________________________________________________________________" SKIP
                    .

            END. /* IF LAST-OF(tt-it-imp.nr-pedido) THEN DO: */

            IF LAST-OF(tt-it-imp.cdd-embarq) THEN DO:

                PUT 
                    SKIP(1)
                    "PESO TOTAL DOS " + STRING(ACCUM COUNT BY tt-it-imp.cdd-embarq tt-it-imp.nr-pallet) 
                    + " PALLETS----->" FORMAT 'x(35)'   AT 40
                    ACCUM TOTAL BY tt-it-imp.cdd-embarq tt-it-imp.peso-liq     FORMAT "->,>>>,>>9.99"   AT 75
                    ACCUM TOTAL BY tt-it-imp.cdd-embarq tt-it-imp.peso-bruto   FORMAT "->,>>>,>>9.99"   AT 89
                    "BOBS:" AT 114
                    ACCUM TOTAL BY tt-it-imp.cdd-embarq tt-it-imp.qt-bobinas   AT 119
                    .


                PUT 
                    "QUANTIDADE TOTAL DOS ITENS---->" FORMAT 'x(35)'   AT 40
                    ACCUM TOTAL BY tt-it-imp.cdd-embarq tt-it-imp.qt-alocada FORMAT "->,>>>,>>9.99"   AT 75
                    SKIP(1)
                    .

            END. /* IF LAST-OF(tt-it-imp.cdd-embarq) THEN DO: */

        END. /* FOR EACH tt-it-imp NO-LOCK */

        IF (LINE-COUNTER + 21) > PAGE-SIZE THEN DO:

            PAGE.

            DISP
                tt-imp.dt-embarque   
                tt-imp.dia-semana    
                tt-imp.motorista     
                tt-imp.placa         
                tt-imp.cod-transp    
                tt-imp.transportadora
                WITH FRAME f-embarque.
            DOWN WITH FRAME f-embarque.

            PUT SKIP(3).

            DISP WITH FRAME f-rodape.

        END.
        ELSE DO:

            DISP WITH FRAME f-rodape.

        END.               

        PAGE.

    END.

    RUN pi-finalizar IN h-acomp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-print-editor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-print-editor Procedure 
PROCEDURE pi-print-editor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    def input param c-editor    as char    no-undo.
    def input param i-len       as integer no-undo.

    def var i-linha  as integer no-undo.
    def var i-aux    as integer no-undo.
    def var c-aux    as char    no-undo.
    def var c-ret    as char    no-undo.

    for each tt-editor:
        delete tt-editor.
    end.

    assign c-ret = chr(255) + chr(255).

    do  while c-editor <> "":
        if  c-editor <> "" then do:
            assign i-aux = index(c-editor, chr(10)).
            if  i-aux > i-len or (i-aux = 0 and length(c-editor) > i-len) then
                assign i-aux = r-index(c-editor, " ", i-len + 1).
            if  i-aux = 0 then
                assign c-aux = substr(c-editor, 1, i-len)
                       c-editor = substr(c-editor, i-len + 1).
            else
                assign c-aux = substr(c-editor, 1, i-aux - 1)
                       c-editor = substr(c-editor, i-aux + 1).
            if  i-len = 0 then
                assign entry(1, c-ret, chr(255)) = c-aux.
            else do:
                assign i-linha = i-linha + 1.
                create tt-editor.
                assign tt-editor.linha    = i-linha
                       tt-editor.conteudo = c-aux.
            end.
        end.
        if  i-len = 0 then
            return c-ret.
    end.
    return c-ret.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

