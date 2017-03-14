/*************************************************************************************
**  Programa: ESBODI317PR-U01.P
**  Objetivo: Tratamento da Unidade de Neg¢cio para o Faturamento referente ao BOB
**  Data....: 03/12/2003
**  VersÆo..: 2.04.001 - Edgar Bispo
**          Fazer a tratativa para os embarques que tenham a regra de n‚gocio BOB X KG
** alteração: 07/12/07 - Amgra - verificacao de quantidade de emendas no atendimento do pedido - 
**************************************************************************************/
def buffer empresa for mgmulti.empresa.
{include/i-prgvrs.i ESBODI317PR-U01 2.04.00.001}

/*-----> Define Temp-Table <------------------------------*/
{utp/ut-glob.i}    
{include/i-epc200.i1} /* Definicao da temp-table tt-epc */

/*-----> Define Parametros <------------------------------*/    
DEF INPUT        PARAM p-ind-event AS  CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE       FOR tt-epc.

def var bo-ped-venda-cal as handle no-undo.

DEF TEMP-TABLE tt-ped-venda no-undo LIKE ped-venda 
    FIELD r-rowid   AS ROWID.

empty temp-table tt-ped-venda.

DEF VAR r-wt-docto      AS ROWID NO-UNDO.

DEF VAR d-qtd-bob       AS DEC NO-UNDO.
DEF VAR d-qtd-un        AS DEC NO-UNDO.

DEF VAR d-peso-bruto    AS DEC NO-UNDO.
DEF VAR d-peso-liquido  AS DEC NO-UNDO.

DEF VAR d-ps-bru-nf     AS DEC NO-UNDO.
DEF VAR d-ps-liq-nf     AS DEC NO-UNDO.

DEF VAR c-un-fat        AS CHAR NO-UNDO.
DEF VAR d-vl-fat        AS DEC  NO-UNDO.

DEF VAR d-tot-prec-bb   AS DEC  NO-UNDO.
DEF VAR d-prec-kg       AS DEC  FORMAT ">>>,>>>,>>9.99999" NO-UNDO.
DEF VAR d-quantidade    AS DEC  NO-UNDO.
DEFINE VARIABLE amg-nr-pedido AS INTEGER    NO-UNDO.

DEFINE VARIABLE l-pallet AS LOGICAL     NO-UNDO.


FIND FIRST tt-epc 
     WHERE tt-epc.cod-event     = p-ind-event   AND
           tt-epc.cod-parameter = "Table-Rowid" NO-LOCK NO-ERROR.
IF AVAIL tt-epc THEN DO:

    ASSIGN r-wt-docto = TO-ROWID(tt-epc.val-parameter).
    
    /* Localiza as informacoes da NF */
    FIND FIRST wt-docto
         WHERE rowid(wt-docto) = r-wt-docto NO-ERROR.
    
    IF AVAIL wt-docto AND 
       p-ind-event = "beforeEfetuaQuebraWtDocto" THEN DO:
    

        FIND FIRST estabelec WHERE estabelec.cod-estabel = wt-docto.cod-estabel NO-LOCK NO-ERROR.
    
        IF AVAIL estabelec AND (estabelec.ep-codigo <> "420" and estabelec.ep-codigo <> "410" and estabelec.ep-codigo <> "700" and estabelec.ep-codigo <> "430") THEN DO: /*solic-318*/ 
            RUN limpa-handle.
            RETURN.
        END.

        /* Rodrigo - Procura Itens no grupo de estoque para melhorar performance */
        ASSIGN l-pallet = NO.
        FOR EACH wt-it-docto OF wt-docto NO-LOCK,
           FIRST ITEM NO-LOCK
           WHERE ITEM.IT-CODIGO = wt-it-docto.it-codigo
             AND (ITEM.ge-codigo = 46
              OR  ITEM.ge-codigo = 47):
            ASSIGN l-pallet = YES.
        END.

        IF l-pallet = NO THEN DO:
            RUN limpa-handle.
            RETURN "OK".
        END.
        
        d-ps-bru-nf = -1.
        FOR FIRST wt-it-docto     OF wt-docto,
            FIRST wt-fat-ser-lote OF wt-it-docto NO-LOCK:

            FIND FIRST pallet NO-LOCK
                 WHERE pallet.cod-estabel    = wt-docto.cod-estabel
                   AND pallet.it-codigo      = wt-fat-ser-lote.it-codigo
                   AND pallet.nr-pallet      = wt-fat-ser-lote.lote NO-ERROR.
            IF NOT AVAIL pallet THEN
                FIND FIRST pallet NO-LOCK
                     WHERE pallet.it-codigo      = wt-fat-ser-lote.it-codigo
                       AND pallet.nr-pallet      = wt-fat-ser-lote.lote NO-ERROR.
            
            IF AVAIL pallet THEN d-ps-bru-nf = 1. /* s¢ para saber se trata de POLO e PALLETS*/
        END.
    
        if  d-ps-bru-nf = -1 then do:
            RUN limpa-handle.
            return.  /*NAO  PALLET E NAO  POLO*/
        END.
    
        ASSIGN 
            d-ps-bru-nf = 0
            d-ps-liq-nf = 0.
    
        FOR EACH wt-it-docto        OF wt-docto:
   
            

            ASSIGN 
                c-un-fat        = wt-it-docto.un[1]
                d-vl-fat        = wt-it-docto.vl-preuni
                d-qtd-bob       = 0
                d-qtd-un        = 0
                d-peso-bruto    = 0
                d-peso-liquido  = 0
                d-quantidade    = 0
                .
    
            /* Ler os lotes do pre-faturamento */
    /* incluido rotina de verificacao de emendas no pedido - 06/12/07*/
            ASSIGN amg-nr-pedido = 0.
            FOR EACH wt-fat-ser-lote OF wt-it-docto NO-LOCK:
    
                /* Carregar o peso liquido e o peso bruto */
    
                FIND FIRST pallet NO-LOCK
                    WHERE pallet.cod-estabel    = wt-docto.cod-estabel
                      AND pallet.it-codigo      = wt-fat-ser-lote.it-codigo
                      AND pallet.nr-pallet      = wt-fat-ser-lote.lote
                    NO-ERROR.
    
                IF NOT AVAIL pallet  THEN
                FIND FIRST pallet NO-LOCK
                    WHERE /*pallet.cod-estabel    = wt-docto.cod-estabel
                      AND*/ pallet.it-codigo      = wt-fat-ser-lote.it-codigo
                      AND pallet.nr-pallet      = wt-fat-ser-lote.lote
                    NO-ERROR.
    
                IF NOT AVAIL pallet THEN NEXT.
    
                IF amg-nr-pedido = 0 THEN ASSIGN amg-nr-pedido = pallet.nr-pedido.
                IF pallet.nr-pallet = "p2120/01" THEN NEXT.
                
    /*para nÆo deixar faturar lote parcial*/
                FIND ITEM WHERE ITEM.IT-CODIGO = wt-fat-ser-lote.it-codigo NO-LOCK NO-ERROR.
                IF AVAIL ITEM AND ITEM.TIPO-CON-EST > 2 AND ITEM.ge-codigo >= 40 AND ITEM.ge-codigo < 50 THEN DO:
                    IF  wt-fat-ser-lote.quantidade[1] <> pallet.peso-liquido  AND (wt-docto.cod-estabel = "422" OR wt-docto.cod-estabel = "412" OR wt-docto.cod-estabel = "432" OR wt-docto.cod-estabel = "443" OR wt-docto.cod-estabel = "434" OR wt-docto.cod-estabel = "442") THEN DO: /*solic-318*/   /*NAO PODE MOSTRAR MENSAGEM PARA UNIGEL COMERCIAL*/
                        MESSAGE "Erro na quandidade do pallet:" wt-fat-ser-lote.lote SKIP
                            "VOLTE NO EQ0506 E VERIFIQUE A QUANTIDADE" SKIP
                            "ALOCADA. NÇO PODE HAVER EMBARQUE PARCIAL" SKIP
                            "DE PALETE!!!" SKIP
                            "NF:"  wt-fat-ser-lote.quantidade[1] " - PL:" pallet.peso-liquido VIEW-AS ALERT-BOX.
                        /*RETURN "NOK".*/
                        
                    END.
                    
                END.
    
                ASSIGN
                    d-peso-bruto    = d-peso-bruto   + pallet.peso-bruto
                    d-peso-liquido  = d-peso-liquido + pallet.peso-liquido
                    d-qtd-bob       = d-qtd-bob      + pallet.nr-bobinas.
                        
                /* Carregar a quantidade de unidade */
                FIND FIRST lote-carac-tec NO-LOCK
                    WHERE lote-carac-tec.it-codigo = wt-fat-ser-lote.it-codigo
                      AND lote-carac-tec.lote      = wt-fat-ser-lote.lote
                      AND lote-carac-tec.cd-folha  = "QTDUN"
                    NO-ERROR.
                IF AVAIL lote-carac-tec THEN
                    ASSIGN d-qtd-un = d-qtd-un + lote-carac-tec.vl-result.
    
                ASSIGN d-quantidade = d-quantidade + wt-fat-ser-lote.quantidade[1].
    
            END. /* FOR EACH wt-fat-ser-lote OF wt-it-docto NO-LOCK: */
    
          /*  IF amg-nr-pedido <> 0 THEN 
                RUN polo\sfc\polsf003a-amg.p (INPUT amg-nr-pedido).
            suspeito de gerar peso liquido errado*/
    
    
            IF d-peso-bruto <> 0 THEN
                ASSIGN
                    wt-it-docto.peso-bruto-it = d-peso-bruto
                    d-ps-bru-nf               = d-ps-bru-nf + d-peso-bruto.
                    
            IF d-peso-liquido <> 0 THEN
                ASSIGN 
                    wt-it-docto.peso-liq-it = d-peso-liquido
                    d-ps-liq-nf             = d-ps-liq-nf + d-peso-liquido
                .
    
             
            FIND FIRST ped-item
              WHERE ped-item.nome-abrev   = wt-docto.nome-abrev
                AND ped-item.nr-pedcli    = wt-it-docto.nr-pedcli
                AND ped-item.nr-sequencia = wt-it-docto.nr-seq-ped
                AND ped-item.it-codigo    = wt-it-docto.it-codigo
                AND ped-item.cod-refer    = wt-it-docto.cod-refer
              NO-ERROR.
            IF NOT AVAIL ped-item THEN NEXT.
            
            /* Verifica se foi informado uma unidade de faturamento */
            FIND FIRST var-result NO-LOCK
              WHERE var-result.item-cotacao = ped-item.it-codigo
                AND var-result.nr-estrut    = ped-item.nr-config
                AND var-result.nome-var     = "UNFAT"
                AND var-result.valor-char <> ""
              NO-ERROR.
            IF NOT AVAIL var-result THEN NEXT.
            
            ASSIGN c-un-fat = var-result.valor-char.
            
            /* Verifica se foi informado o preco unitario do faturamento */
            FIND FIRST var-result NO-LOCK
              WHERE var-result.item-cotacao = ped-item.it-codigo
                AND var-result.nr-estrut    = ped-item.nr-config
                AND var-result.nome-var     = "PREUN"
                AND var-result.valor-dec    <> 0
              NO-ERROR.
            IF NOT AVAIL var-result THEN NEXT.
    
                ASSIGN d-vl-fat = var-result.valor-dec.
    
                
            IF c-un-fat = "BB" AND 
               d-qtd-bob > 0   AND 
               d-quantidade > 0 THEN DO:
            
                ASSIGN
                    wt-it-docto.quantidade[2]   = d-qtd-bob
                    d-tot-prec-bb               = d-qtd-bob * d-vl-fat
                    d-prec-kg                   = d-tot-prec-bb / d-quantidade
                    wt-it-docto.vl-preuni       = d-prec-kg
                    wt-it-docto.vl-preori       = d-prec-kg
                    wt-it-docto.vl-pretab       = d-prec-kg
                    wt-it-docto.vl-merc-ori     = d-tot-prec-bb
                    wt-it-docto.vl-merc-tab     = d-tot-prec-bb
                    wt-it-docto.vl-merc-liq     = d-tot-prec-bb
    
                    ped-item.vl-preuni          = d-prec-kg
                    ped-item.vl-preori          = d-prec-kg
                    ped-item.vl-pretab          = d-prec-kg
                    .
            
                FIND FIRST ped-venda NO-LOCK
                    WHERE ped-venda.nome-abrev  = wt-docto.nome-abrev
                      AND ped-venda.nr-pedcli   = wt-it-docto.nr-pedcli
                    NO-ERROR.
                                
                 FIND FIRST tt-ped-venda NO-LOCK
                    WHERE tt-ped-venda.nome-abrev  = wt-docto.nome-abrev
                      AND tt-ped-venda.nr-pedcli   = wt-docto.nr-pedcli
                    NO-ERROR.

                 IF NOT AVAIL tt-ped-venda THEN DO:  
                    CREATE tt-ped-venda.
                    BUFFER-COPY ped-venda TO tt-ped-venda.
                    ASSIGN tt-ped-venda.r-rowid = ROWID(ped-venda).
                 END.
                /* BO para totalizar o pedido */
                IF NOT VALID-HANDLE(bo-ped-venda-cal) THEN
                    run dibo/bodi159cal.p persistent set bo-ped-venda-cal.

                /* Totalizar o pedido de venda */
                run calculateOrder in bo-ped-venda-cal(input tt-ped-venda.r-rowid).
    
                RUN limpa-handle.

            END. /* IF c-un-fat = "BOB" THEN DO: */
            
            IF c-un-fat <> "BB" AND
               c-un-fat <> "KG" AND 
               c-un-fat <> ""   AND 
               d-quantidade > 0 THEN DO:
            
                ASSIGN
                    wt-it-docto.quantidade[2]   = d-qtd-un
                    d-tot-prec-bb               = d-qtd-un * d-vl-fat
                    d-prec-kg                   = d-tot-prec-bb / d-quantidade
                    wt-it-docto.vl-preuni       = d-vl-fat
                    wt-it-docto.vl-preori       = d-vl-fat
                    wt-it-docto.vl-pretab       = d-vl-fat
                    wt-it-docto.vl-merc-ori     = d-tot-prec-bb
                    wt-it-docto.vl-merc-tab     = d-tot-prec-bb
                    wt-it-docto.vl-merc-liq     = d-tot-prec-bb
    
                    ped-item.vl-preuni          = d-prec-kg
                    ped-item.vl-preori          = d-prec-kg
                    ped-item.vl-pretab          = d-prec-kg
                    .
            
                FIND FIRST ped-venda NO-LOCK
                    WHERE ped-venda.nome-abrev  = wt-docto.nome-abrev
                      AND ped-venda.nr-pedcli   = wt-docto.nr-pedcli
                    NO-ERROR.

                 FIND FIRST tt-ped-venda NO-LOCK
                    WHERE tt-ped-venda.nome-abrev  = wt-docto.nome-abrev
                      AND tt-ped-venda.nr-pedcli   = wt-docto.nr-pedcli
                    NO-ERROR.

                 IF NOT AVAIL tt-ped-venda THEN DO:                                
                    CREATE tt-ped-venda.
                    BUFFER-COPY ped-venda TO tt-ped-venda.
                    ASSIGN tt-ped-venda.r-rowid = ROWID(ped-venda).
                 END.
                /* BO para totalizar o pedido */
                IF NOT VALID-HANDLE(bo-ped-venda-cal) THEN
                    run dibo/bodi159cal.p persistent set bo-ped-venda-cal.

                /* Totalizar o pedido de venda */
                run calculateOrder in bo-ped-venda-cal(input tt-ped-venda.r-rowid).
    
                RUN limpa-handle.

            END.
    
        END. /* FOR EACH wt-fat-ser-lote */
    
        IF d-ps-bru-nf > 0 THEN
            ASSIGN wt-docto.peso-bru-tot-inf = d-ps-bru-nf.
    
        IF d-ps-liq-nf > 0 THEN
            ASSIGN wt-docto.peso-liq-tot-inf = d-ps-liq-nf.
    
    END. /* if avail wt-docto */
END.

PROCEDURE limpa-handle:

    IF VALID-HANDLE(bo-ped-venda-cal) THEN
        DELETE PROCEDURE bo-ped-venda-cal.

END PROCEDURE.
