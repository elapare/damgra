/*------------------------------------------------------------------------
File.............: espd0034rp.p
Description......: Situaá∆o de Pedidos de Vendas / Capacidade de Produá∆o
Input Parameters : 
Output Parameters: 
Author...........: Amgra - JosÇ Roberto
Created..........: 19/09/2009  
OBS..............: 
------------------------------------------------------------------------*/
{bf/buffersUni2.i}

define variable c-prog-gerado as character no-undo initial "espd0034rp".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

/****************** Definiá∆o de Tabelas Tempor†rias do Relat¢rio **********************/
DEF BUFFER bf-saldo-estoq FOR saldo-estoq.
def buffer bf-ped-venda for ped-venda.
def buffer bf-ped-item  for ped-item.
def buffer bf-if-ped-venda for if-ped-venda.

def temp-table tt-raw-digita
    field raw-digita as raw.

{pdp\espd0034.i}
{pdp\espd0034var.i} /*Variaveis*/
{cdp\cd0666.i}
/****************** Definiáao de Vari†veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/****************** Definiáao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form tt-fat.cod-estabel label "Estabelecimento" format "x(3)" at 001
     with down width 132 side-labels no-box stream-io frame f-relat-01-132.

form tt-fat.dt-entrega label "Dt Entrega Orig" format "99/99/9999" at 001
     with down width 132 side-labels no-box stream-io frame f-relat-02-132.

form tt-fat.tp-pedido column-label "TP" format "x(2)" at 001
     tt-fat.cod-canal-venda COLUMN-LABEL "CV" FORMAT ">9" AT 04
     tt-fat.nr-pedido column-label "Pedido" format ">>>>>>>9" at 007
     tt-fat.nome-abrev column-label "Cliente" format "x(12)" at 016
     tt-fat.it-codigo column-label "Item" format "x(16)" at 029
     tt-fat.qt-pedida column-label "Qt Pedida" format ">>>>,>>9.99" at 046
     tt-fat.qt-atendida column-label "Qt Faturada" format ">>>>,>>9.99" at 060
     /*tt-fat.dt-entrega column-label "Dt Fatur" format "99/99/9999" at 074*/
     with down width 184 no-box stream-io frame f-relat-09-132.

create tt-param.
raw-transfer raw-param to tt-param.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.


define new shared buffer b_ped_exec_style for ped_exec.
define new shared buffer b_servid_exec_style for servid_exec.

define new shared stream str-rp.

assign c-programa     = "espd0034rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Sit.de Pedidos de Vendas / Capacidade de Produá∆o"
       c-sistema      = "PDP".

if  tt-param.formato = 2 then do:


form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    fill("-", 112) format "x(110)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabec.

form header
    fill("-", 132) format "x(132)" 
    with stream-io width 132 no-labels no-box frame f-linha.

form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    "Periodo:" i-numper-x at 08 "-"
    da-iniper-x at 14 "to" da-fimper-x
    fill("-", 74) format "x(72)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabper.

run grapi/gr2004.p.

form header
    c-rodape format "x(132)"
    with stream-io width 132 no-labels no-box page-bottom frame f-rodape.


end. /* tt-param.formato = 2 */

run grapi/gr2013c.p (input tt-param.destino,
                    input tt-param.arquivo,
                    input tt-param.usuario,
                    input no).

assign i-ep-codigo-usuario = tt-param.ep-codigo
       v_cdn_empres_usuar  = i-ep-codigo-usuario.

assign c-cod-estabel     = tt-param.c-cod-estabel    
       c-it-codigo-ini   = tt-param.c-it-codigo-ini  
       c-it-codigo-fim   = tt-param.c-it-codigo-fim  
       c-fm-codigo-ini   = tt-param.c-fm-codigo-ini  
       c-fm-codigo-fim   = tt-param.c-fm-codigo-fim  
       i-ge-codigo-ini   = tt-param.i-ge-codigo-ini  
       i-ge-codigo-fim   = tt-param.i-ge-codigo-fim  
       c-nome-abrev-ini  = tt-param.c-nome-abrev-ini 
       c-nome-abrev-fim  = tt-param.c-nome-abrev-fim 
       c-tp-pedido-ini   = tt-param.c-tp-pedido-ini  
       c-tp-pedido-fim   = tt-param.c-tp-pedido-fim  
       dt-entrega-ini    = tt-param.dt-entrega-ini   
       dt-entrega-fim    = tt-param.dt-entrega-fim   
       i-mercado         = tt-param.i-mercado.


find first empresa no-lock
    where empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  avail empresa
then
    assign c-empresa = empresa.razao-social.
else
    assign c-empresa = "".

/* for each e disp */


assign l-imprime = no.

if  tt-param.destino = 1 then
    assign v-cod-destino-impres = "Impressora".
else
    if  tt-param.destino = 2 then
        assign v-cod-destino-impres = "Arquivo".
    else
        IF tt-param.destino = 3 THEN
        assign v-cod-destino-impres = "Terminal".
        ELSE
            assign v-cod-destino-impres = "Excel".

/***************************************************************************************************
                                    MAIN BLOCK
***************************************************************************************************/

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

RUN pi-geracao-dados.

RUN pi-impressao-dados.

run pi-finalizar in h-acomp.

return 'OK'.

/* fim do programa */
/***************************************************************************************************
                                    PROCEDURES
***************************************************************************************************
pi-geracao-dados                                    
-- pi-saldo


pi-impressao-dados
-- pi-tot-ord
-- pi-limpa-xtrim
-- pi-cria-planilha
-- pi-finaliza-impressao
-- pi-grava-xtrim
-- pi-integracao-xtrim

pi-print-editor
***************************************************************************************************/

PROCEDURE pi-geracao-dados:
    assign v-num-reg-lidos = 0.

    ASSIGN i-linha = 7. 

    FIND FIRST param-cp NO-LOCK NO-ERROR.
    FIND FIRST param-xtrim NO-LOCK NO-ERROR.

    for  each ped-item no-lock
             where ped-item.dt-entrega >= dt-entrega-ini       and 
                   ped-item.dt-entrega <= dt-entrega-fim       and
                   ped-item.it-codigo  >= c-it-codigo-ini      and 
                   ped-item.it-codigo  <= c-it-codigo-fim      and
                   ped-item.ind-componen  <> 3 AND
                   (ped-item.cod-sit-item  = 1 OR ped-item.cod-sit-item  = 2) USE-INDEX peditem-09,
        EACH ped-venda OF ped-item NO-LOCK
             where ped-venda.cod-estabel = c-cod-estabel        and 
                   ped-venda.tp-pedido  >= c-tp-pedido-ini      and 
                   ped-venda.tp-pedido  <= c-tp-pedido-fim   ,

        EACH ITEM NO-LOCK WHERE 
            ITEM.it-codigo  = ped-item.it-codigo AND
            ITEM.ge-codigo >= i-ge-codigo-ini    AND
            ITEM.ge-codigo <= i-ge-codigo-fim    AND
            ITEM.fm-codigo >= c-fm-codigo-ini    AND
            ITEM.fm-codigo <= c-fm-codigo-fim. 

        ASSIGN nat-operacao-jr = ped-item.nat-operacao
               nome-abrev-jr   = ped-venda.nome-abrev
               nr-pedido-relac-jr = 0
               nome-abrev-pai   = ped-venda.nome-abrev.
               nome-abrev-filho = ped-venda.nome-abrev.

        FIND FIRST if-ped-venda NO-LOCK 
            WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-ERROR.

        ASSIGN qt-atendida-ed = ped-item.qt-atendida. 

        IF AVAIL if-ped-venda THEN DO:

            FIND bf-ped-venda WHERE
                bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac
                NO-LOCK NO-ERROR.

            IF AVAIL bf-ped-venda THEN DO:

               ASSIGN nome-abrev-pai   = ped-venda.nome-abrev.
                      nome-abrev-filho = bf-ped-venda.nome-abrev.

               FIND FIRST bf-ped-item NO-LOCK OF bf-ped-venda 
                   WHERE bf-ped-item.nr-sequencia = ped-item.nr-sequencia NO-ERROR.

               IF AVAIL bf-ped-item THEN
                  ASSIGN nat-operacao-jr    = bf-ped-item.nat-operacao 
                         nome-abrev-jr      = bf-ped-venda.nome-abrev  
                         nr-pedido-relac-jr = bf-ped-venda.nr-pedido
                         qt-atendida-ed     = bf-ped-item.qt-atendida. 
            END.
        END.
        else do:
            FIND FIRST if-ped-venda NO-LOCK 
                WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido NO-ERROR.

            IF AVAIL if-ped-venda THEN DO:
                FIND bf-ped-venda NO-LOCK 
                    WHERE bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido NO-ERROR.
    
                IF AVAIL bf-ped-venda THEN DO:
                    ASSIGN nome-abrev-pai   = bf-ped-venda.nome-abrev
                           nome-abrev-filho = ped-venda.nome-abrev.                     
                END.
            END.

        end.    

        IF not (nome-abrev-jr >= c-nome-abrev-ini and
           nome-abrev-jr <= c-nome-abrev-fim)     and 
           not (ped-venda.nome-abrev >= c-nome-abrev-ini and
           ped-venda.nome-abrev <= c-nome-abrev-fim)  THEN NEXT.

        IF integer(substring(string(nat-operacao-jr),1,1)) >= 7 THEN
            ASSIGN merc-jr = "F".
        ELSE
            ASSIGN merc-jr = "J".

        IF tt-param.i-mercado = 1 AND merc-jr <> "J" THEN NEXT.
        IF tt-param.i-mercado = 2 AND merc-jr <> "F" THEN NEXT.

        assign v-num-reg-lidos = v-num-reg-lidos + 1.

        run pi-acompanhar in h-acomp(input "Lendo Carteira de Pedidos: " + string(v-num-reg-lidos)).

        ASSIGN 
         var-bob-ped  = 0
         var-bob-prod = 0
         var-Larg     = 0 
         var-diin     = 0
         var-diex     = 0
         var-vga      = 0
         var-estoque  = 0
         var-mtn      = 0
         var-gru      = 0
         var-sbc      = 0
         var-outros   = 0
         var-terc     = 0
         var-transito = 0
         tem-transf-jr = "" 
         var-Merc      = ""
         var-pedcli    = ""
         tem-data-jr = 01/01/1900.


        /* Quantidade Bobina  BB-Ped */
        FIND var-result WHERE
             var-result.item-cotacao = ped-item.it-codigo  AND 
             var-result.nr-estrut    = ped-item.nr-config  AND 
             var-result.nome-var     = "QTDBOB"  NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN 
            ASSIGN  var-bob-ped = var-result.valor-dec.

        FIND FIRST var-result WHERE 
             var-result.nome-var     = "Largura"          AND 
             var-result.nr-estrut    = ped-item.nr-config AND
             var-result.item-cotacao = ped-item.it-codigo 
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN var-Larg = var-result.valor-dec.

        FIND FIRST var-result WHERE
             var-result.nome-var     = "diin"              AND 
             var-result.nr-estrut    = ped-item.nr-config  AND
             var-result.item-cotacao = ped-item.it-codigo 
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN var-diin = var-result.valor-dec.

        FIND FIRST var-result WHERE
             var-result.nome-var     = "DIEX"             AND 
             var-result.nr-estrut    = ped-item.nr-config AND
             var-result.item-cotacao = ped-item.it-codigo 
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN var-diex = var-result.valor-dec.

        ASSIGN qt-bob-jr   = 0
               i-nr-pedido = ped-venda.nr-pedido.

        run pi-saldo.

        IF nr-pedido-relac-jr <> 0 THEN do:
            
            ASSIGN i-nr-pedido = nr-pedido-relac-jr.

            run pi-saldo.
        End.

         FIND FIRST if-ped-venda NO-LOCK 
             WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido NO-ERROR.

        IF AVAIL if-ped-venda THEN DO:
            ASSIGN i-nr-pedido = if-ped-venda.nr-pedido.

            run pi-saldo.
        END.

        IF integer(substring(string(nat-operacao-jr),1,1,"character")) >= 7 THEN 
            ASSIGN var-Merc = "Externo".
        ELSE
            ASSIGN var-Merc = "Interno".

        IF integer(substring(string(nat-operacao-jr),1,1)) >= 7 THEN
            ASSIGN ext-jr = ped-venda.observaáoes.
        ELSE
            ASSIGN ext-jr = "".

        IF INDEX (ext-jr,")") > 0  THEN DO:
                    ext-jr = SUBSTRING(ext-jr,1,INDEX (ext-jr,")")).
        END.

        IF INDEX (ext-jr,"-") > 0  THEN DO:
                    ext-jr = trim(SUBSTRING(ext-jr,1,INDEX (ext-jr,"-") - 1)) .
        END.

        IF length(ext-jr) > 20 AND NUM-ENTRIES(ext-jr," ") >= 2 THEN DO:                    
            ext-jr = entry(1,ext-jr," ") + " " + entry(2,ext-jr," ").                   
        END.



        FIND FIRST if-ped-item NO-LOCK
            WHERE if-ped-item.nome-abrev    = ped-item.nome-abrev
              AND if-ped-item.nr-pedcli     = ped-item.nr-pedcli   
              AND if-ped-item.nr-sequencia  = ped-item.nr-sequencia
              AND if-ped-item.it-codigo     = ped-item.it-codigo   
              AND if-ped-item.cod-refer     = ped-item.cod-refer     NO-ERROR.

        CREATE tt-fat.
        ASSIGN tt-fat.var-Larg        = var-Larg
               tt-fat.var-bob-ped     = var-bob-ped
               tt-fat.var-bob-prod    = qt-bob-jr
               tt-fat.var-diin        = var-diin
               tt-fat.var-diex        = var-diex
               tt-fat.var-pedcli      = var-pedcli
               tt-fat.nr-ext          = ext-jr
               tt-fat.var-vga         = var-vga
               tt-fat.var-mtn         = var-mtn
               tt-fat.var-gru         = var-gru
               tt-fat.var-sbc         = var-sbc
               tt-fat.var-outros      = var-outros
               tt-fat.var-terc        = var-terc
               tt-fat.var-transito    = var-transito
               tt-fat.var-estoque     = var-estoque
               tt-fat.tem-transf      = tem-transf-jr
               tt-fat.nome-merc       = var-Merc
               tt-fat.cod-estabel     = ped-venda.cod-estabel
               tt-fat.dt-implant      = ped-venda.dt-implant
               tt-fat.nr-pedcli       = ped-venda.nr-pedcli
               tt-fat.dt-entrega      = ped-item.dt-entrega
               tt-fat.referencia      = ped-item.nr-config
               tt-fat.tp-pedido       = ped-venda.tp-pedido
               tt-fat.nr-sequencia    = ped-item.nr-sequencia
               tt-fat.cod-canal-venda = ped-venda.cod-canal-venda
               tt-fat.nome-abrev      = if nome-abrev-filho = nome-abrev-pai then nome-abrev-pai else nome-abrev-filho + "-" + nome-abrev-pai
               tt-fat.it-codigo       = ped-item.it-codig
               tt-fat.nr-pedido       = ped-venda.nr-pedido
               tt-fat.qt-pedida       = ped-item.qt-pedida
               tt-fat.qt-atendida     = qt-atendida-ed  
               tt-fat.vl-preuni       = ped-item.vl-preuni
               tt-fat.mo-codigo       = ped-venda.mo-codigo
               tt-fat.nome-ab-rep-jr  = nome-ab-rep-jr
               tt-fat.nome-transp     = ped-venda.nome-transp
               tt-fat.obs-pallet      = c-obs-pallet
               tt-fat.dt-val          = dt-validade
               tt-fat.nr-pedido-relac = nr-pedido-relac-jr
               tt-fat.nome-abrev-pai  = nome-abrev-pai
               tt-fat.nome-abrev-filho = nome-abrev-filho.

        DEF VAR de-qt-saldo  LIKE ord-prod.qt-ordem NO-UNDO.
        DEF VAR de-nr-bobina LIKE ord-prod.qt-ordem NO-UNDO.
        
        FOR EACH ord-prod NO-LOCK USE-INDEX cliente-ped
            WHERE ord-prod.nome-abrev   = tt-fat.nome-abrev-pai
              AND ord-prod.nr-pedido    = STRING(tt-fat.nr-pedido)
              AND ord-prod.nr-sequencia = tt-fat.nr-sequencia:
            
            /*Desconsidera ordens finalizadas*/
            IF ord-prod.estado > 6 THEN NEXT.
            
            FIND FIRST ext-ord-prod NO-LOCK
                WHERE ext-ord-prod.nr-ord-produ = ord-prod.nr-ord-produ NO-ERROR.

            IF NOT AVAIL ext-ord-prod THEN NEXT.
            
            IF ext-ord-prod.nr-bobinas = 0 OR ext-ord-prod.qt-pedida = 0 THEN NEXT.
        
            ASSIGN de-qt-saldo = ord-prod.qt-ordem - (ord-prod.qt-produzida + ord-prod.qt-refugada).
            IF de-qt-saldo < 0 THEN ASSIGN de-qt-saldo = 0.
        
            ASSIGN de-nr-bobina = de-qt-saldo / (ext-ord-prod.qt-pedida / ext-ord-prod.nr-bobinas).
        
            ASSIGN de-nr-bobina = ROUND(de-nr-bobina,0).

            IF de-nr-bobina > 0 THEN
                ASSIGN tt-fat.qt-ordem     = tt-fat.qt-ordem + de-qt-saldo
                       tt-fat.nr-bob-ordem = tt-fat.nr-bob-ordem + de-nr-bobina.
        END.
        
        IF AVAIL if-ped-item THEN
            ASSIGN tt-fat.campanha = if-ped-item.campanha.

        IF ped-item.nr-config <> 0 THEN DO:
            RUN pi-atualiza-var-result("GRAMATURA",ped-item.nr-config,ped-item.it-codigo).
            RUN pi-atualiza-var-result("DENSIDADE",ped-item.nr-config,ped-item.it-codigo).
            RUN pi-atualiza-var-result("ESPESSURA",ped-item.nr-config,ped-item.it-codigo).
        END.

        FIND FIRST var-result NO-LOCK 
            WHERE var-result.nome-var     = "GRAMATURA"        
              AND var-result.nr-estrut    = ped-item.nr-config 
              AND var-result.item-cotacao = ped-item.it-codigo NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN tt-fat.gramatura = var-result.valor-dec.

        FIND FIRST var-result NO-LOCK 
            WHERE var-result.nome-var     = "ESPESSURA"        
              AND var-result.nr-estrut    = ped-item.nr-config 
              AND var-result.item-cotacao = ped-item.it-codigo NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN tt-fat.espessura = var-result.valor-dec.
        
        FIND FIRST var-result NO-LOCK 
            WHERE var-result.nome-var     = "DENSIDADE"        
              AND var-result.nr-estrut    = ped-item.nr-config 
              AND var-result.item-cotacao = ped-item.it-codigo NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN tt-fat.densidade = var-result.valor-dec.
    END.
END.

procedure pi-saldo:
    
    FOR EACH pallet NO-LOCK USE-INDEX pedido 
        WHERE pallet.nr-pedido    = i-nr-pedido 
          AND pallet.nr-sequencia = ped-item.nr-sequencia 
          AND pallet.situacao     = 2 .
        
        ASSIGN qtde-jr    = 0
               qtde-jr-t  = 0
               qtde-jr-tr = 0.

        ASSIGN qt-bob-jr = qt-bob-jr + pallet.nr-bobinas.


        FOR EACH saldo-estoq NO-LOCK USE-INDEX lote 
            WHERE saldo-estoq.lote      = pallet.nr-pallet 
              AND saldo-estoq.it-codigo = pallet.it-codigo 
              AND saldo-estoq.qtidade-atu > 0 :
            ASSIGN qtde-jr = qtde-jr + saldo-estoq.qtidade-atu.
        END.

        /* Procura no Terceiro */
        ASSIGN qtde-jr-t    = 0
               soma-terc-jr = 0
               qtde-jr-tr   = 0
               soma-terc-tr = 0.

        IF qtde-jr = 0 THEN DO:

            FIND FIRST saldo-estoq NO-LOCK USE-INDEX lote
                WHERE saldo-estoq.lote      = pallet.nr-pallet 
                  AND saldo-estoq.it-codigo = pallet.it-codigo 
                  AND saldo-estoq.cod-refer = pallet.cod-refer NO-ERROR.

            IF AVAIL saldo-estoq THEN DO:

               ASSIGN soma-terc-jr = 0
                      soma-terc-tr = 0.
        
               FOR EACH movto-estoq NO-LOCK 
                   WHERE movto-estoq.it-codigo = saldo-estoq.it-codigo 
                     AND movto-estoq.cod-refer = saldo-estoq.cod-refer 
                     AND movto-estoq.lote      = saldo-estoq.lote 
                     AND (movto-estoq.esp-docto = 20 or movto-estoq.esp-docto = 22 OR movto-estoq.esp-docto = 21 OR movto-estoq.esp-docto = 23):
                                                                                
                                                                                                      
                   if movto-estoq.esp-docto = 22  and movto-estoq.cod-estabel = "424" then next.

                   FIND natur-oper NO-LOCK WHERE 
                         natur-oper.nat-operacao = movto-estoq.nat-operacao.

                   IF NOT AVAIL natur-oper OR 
                      (natur-oper.terceiro = NO AND
                       natur-oper.transf   = NO) THEN NEXT.
                       
                    if natur-oper.tp-oper-terc > 2 then next. /* consignaá∆o*/


                   IF movto-estoq.esp-docto = 23 THEN DO:

                       ASSIGN tem-transf-jr = movto-estoq.nro-docto.

                       IF movto-estoq.tipo-trans = 1 THEN
                           ASSIGN soma-terc-tr = soma-terc-tr + movto-estoq.quantidade.
                       ELSE
                           ASSIGN soma-terc-tr = soma-terc-tr - movto-estoq.quantidade.
                       
                   END.

                   ELSE DO:

                      IF movto-estoq.tipo-trans = 1 THEN
                          ASSIGN soma-terc-jr = soma-terc-jr + movto-estoq.quantidade.
                      ELSE
                          ASSIGN soma-terc-jr = soma-terc-jr - movto-estoq.quantidade.
                          
                              
                   END.
        
               END.

        
               ASSIGN soma-terc-jr = soma-terc-jr * -1
                      soma-terc-tr = soma-terc-tr * -1
                      qtde-jr-t  = qtde-jr-t  + soma-terc-jr
                      qtde-jr-tr = qtde-jr-tr + soma-terc-tr.

            END.
        END.  

        /* Fim procura no Terceiro */

        ASSIGN var-terc     = var-terc + qtde-jr-t
               var-transito = var-transito + qtde-jr-tr
               var-estoque  = var-estoque + qtde-jr.

        for each   saldo-terc 
            WHERE saldo-terc.cod-estabel = pallet.cod-estabel 
              AND saldo-terc.quantidade > 0 
              AND saldo-terc.lote = pallet.nr-pallet 
              AND saldo-terc.it-codigo = pallet.it-codigo 
              AND not can-find(first  nota-fiscal NO-LOCK 
                               WHERE nota-fiscal.cod-estabel = saldo-terc.cod-estabel 
                                 AND nota-fiscal.serie       = saldo-terc.serie-docto 
                                 AND nota-fiscal.nr-nota-fis = saldo-terc.nro-docto ):
            ASSIGN var-terc = var-terc +  saldo-terc.quantidade .
        end.
    END.   /*Pallet*/
    
end procedure.

procedure pi-tot-ord.

    FOR EACH  ord-prod NO-LOCK
        WHERE ord-prod.nome-abrev   = nome-abrev-pai
          AND ord-prod.nr-pedido    = string(tt-fat.nr-pedcli)
          AND ord-prod.nr-sequencia = tt-fat.nr-sequencia
          AND ord-prod.it-codigo    = tt-fat.it-codigo:

        /* Quantidade Produzida e Bobinas Produzidas - ACA descontando EAC*/
        FOR EACH  movto-estoq NO-LOCK USE-INDEX ord-seq
            WHERE movto-estoq.nr-ord-produ  = ord-prod.nr-ord-produ
              AND movto-estoq.it-codigo     = ord-prod.it-codigo
              AND movto-estoq.cod-estabel   = ord-prod.cod-estabel
              AND (movto-estoq.esp-docto    = 1  OR
                   movto-estoq.esp-docto    = 8)      /*ACA ou EAC*/
              AND movto-estoq.cod-depos     <> substr(param-cp.char-2,1,3) /*deposito reciclado*/
              and movto-estoq.quantidade    > 0:
             
            /*Soma os acabados*/
            IF movto-estoq.esp-docto = 1 THEN DO:
            
                assign de-qt-prod = de-qt-prod + movto-estoq.quantidade.
           
                /** Verifica se bobina (lote) est† atendendo este pedido **/
                FIND FIRST it-pallet WHERE it-pallet.lote-bobina = movto-estoq.lote NO-LOCK NO-ERROR.
                
                IF AVAIL it-pallet THEN DO:
                
                    FIND first pallet NO-LOCK 
                        WHERE pallet.cod-estabel = it-pallet.cod-estabel
                        AND   pallet.it-codigo   = it-pallet.it-codigo
                        AND   pallet.nr-pallet   = it-pallet.nr-pallet NO-ERROR.
                        
                    IF pallet.nr-pedido    = tt-fat.nr-pedido    and
                       pallet.nr-sequencia = tt-fat.nr-sequencia THEN
                        ASSIGN de-qt-prod-pedido = de-qt-prod-pedido + movto-estoq.quantidade.
                        
                END.
                ELSE DO: /*Soma bobinas produzidas n∆o paletizadas*/
                
                    ASSIGN de-qt-prod-pedido = de-qt-prod-pedido + movto-estoq.quantidade
                           tt-fat.var-bob-prod  = tt-fat.var-bob-prod + 1  
                           de-qt-prod-nao-pallet = de-qt-prod-nao-pallet + movto-estoq.quantidade.
                END.     
            END.
            
            /*Desconta os estornos*/
            IF movto-estoq.esp-docto = 8 THEN DO:
            
                assign de-qt-prod = de-qt-prod - movto-estoq.quantidade.

                /** Verifica se bobina (lote) est† atendendo este pedido **/
                FIND FIRST it-pallet WHERE it-pallet.lote-bobina = movto-estoq.lote NO-LOCK NO-ERROR.
                
                IF AVAIL it-pallet THEN DO:
                
                    FIND pallet NO-LOCK 
                        WHERE pallet.cod-estabel = it-pallet.cod-estabel
                        AND   pallet.it-codigo   = it-pallet.it-codigo
                        AND   pallet.nr-pallet   = it-pallet.nr-pallet NO-ERROR.
                        
                    IF pallet.nr-pedido    = tt-fat.nr-pedido    and
                       pallet.nr-sequencia = tt-fat.nr-sequencia THEN
                        ASSIGN de-qt-prod-pedido = de-qt-prod-pedido - movto-estoq.quantidade.
                END.
                ELSE DO: /*Subtrai bobinas produzidas n∆o paletizadas*/
                    ASSIGN de-qt-prod-pedido = de-qt-prod-pedido - movto-estoq.quantidade
                           tt-fat.var-bob-prod  = tt-fat.var-bob-prod - 1  
                           de-qt-prod-nao-pallet = de-qt-prod-nao-pallet - movto-estoq.quantidade.
                END.    
            END.
        END.
    END.           
    /* Fim da l¢gica para carregar OPs para o pedido-sequencia */
end procedure.

PROCEDURE pi-impressao-dados:

    RUN pi-limpa-xtrim.

    IF tt-param.destino = 4 THEN DO:

        /* Cria Aplicaá∆o do Excel */

        CREATE "Excel.Application" c-excel.
        ASSIGN c-excel:DisplayAlerts = FALSE.

        ASSIGN c-modelo-planilha = search("modelos/mod-espd0034.xls") 
               c-arq             = SESSION:TEMP-DIRECTORY.

        RUN pi-cria-planilha.

        ASSIGN c-relatorio:range("B" + STRING(3)):VALUE = c-cod-estabel  
               c-relatorio:range("B" + STRING(4)):VALUE = dt-entrega-ini
               c-relatorio:range("F" + STRING(4)):VALUE = dt-entrega-fim.
        /*Campos Adicionais*/
        ASSIGN c-relatorio:range("U" + STRING(7)):VALUE = "Campanha"
               c-relatorio:range("V" + STRING(7)):VALUE = "Densidade"
               c-relatorio:range("W" + STRING(7)):VALUE = "Espessura"
               c-relatorio:range("X" + STRING(7)):VALUE = "Gramatura"
               c-relatorio:range("Y" + STRING(6)):VALUE = "Qtde"
               c-relatorio:range("Y" + STRING(7)):VALUE = "OP Em aberto"
               c-relatorio:range("Z" + STRING(6)):VALUE = "Qtde Bobinas"
               c-relatorio:range("Z" + STRING(7)):VALUE = "OP em aberto".
    END.

    assign v-num-reg-lidos = 0.

    for each tt-fat no-lock
        break by tt-fat.nr-pedido
              BY tt-fat.nr-sequencia
              by tt-fat.cod-estabel
              by tt-fat.dt-entrega
              BY tt-fat.nome-merc
              by tt-fat.nome-abrev:

        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input "Gerando a Planilha Excel: " + string(v-num-reg-lidos)).


        /* l¢gica para carregar OPs para o pedido-sequencia */
        ASSIGN de-qt-prod-pedido = 0
               de-qt-prod-nao-pallet = 0
               de-qt-prod = 0.

        nome-abrev-pai = tt-fat.nome-abrev-pai.

        run pi-tot-ord.

        if tt-fat.nome-abrev-filho <> tt-fat.nome-abrev-pai then do:
           nome-abrev-pai = tt-fat.nome-abrev-filho.

          run pi-tot-ord.

        end.  

         /***  C‡DIGO PARA SA÷DA EM 132 COLUNAS ***/

         IF tt-param.destino <> 4 THEN DO:


           if  tt-param.formato = 2 then do:

            view stream str-rp frame f-cabec.
            view stream str-rp frame f-rodape.
            assign l-imprime = yes.
            if  first-of(tt-fat.cod-estabel) then do:
                display stream str-rp " " with stream-io no-box frame f-branco.
                display stream str-rp tt-fat.cod-estabel
                        with stream-io frame f-relat-01-132.
            end.

            if  first-of(tt-fat.dt-entrega) then do:
                display stream str-rp " " with stream-io no-box frame f-branco.
                display stream str-rp tt-fat.dt-entrega
                        with stream-io frame f-relat-02-132.
                display stream str-rp " " with stream-io no-box frame f-branco.
            end.

            display stream str-rp 
                tt-fat.tp-pedido
                tt-fat.cod-canal-venda
                tt-fat.nr-pedido
                tt-fat.nome-abrev 
                tt-fat.it-codigo
                tt-fat.qt-pedida
                tt-fat.qt-atendida
                tt-fat.var-pedcli
                tt-fat.nr-ext
                tt-fat.var-Larg
                tt-fat.var-diin
                tt-fat.var-diex
                tt-fat.var-mtn
                tt-fat.var-terc
                tt-fat.nome-merc
                tt-fat.nome-ab-rep-jr
                tt-fat.nome-transp
                    with stream-io frame f-relat-09-132.
                down stream str-rp with frame f-relat-09-132.
          end.
         END.


         IF tt-param.destino = 4 THEN DO:

            ASSIGN i-linha = i-linha + 1.

            ASSIGN perc-atend   = 0
                   perc-prod    = 0
                   saldo-atend  = tt-fat.qt-pedida - tt-fat.qt-atendida
                   perc-atend   = (tt-fat.qt-atendida / tt-fat.qt-pedida) * 100
                   produzido-jr = /* tt-fat.var-estoque + tt-fat.var-transito + tt-fat.var-terc + */ de-qt-prod-pedido
                   perc-atend   = ((tt-fat.var-estoque + tt-fat.var-transito + tt-fat.var-terc + tt-fat.qt-atendida) / tt-fat.qt-pedida) * 100
                   perc-prod    = (produzido-jr / tt-fat.qt-pedida) * 100
                   qt-bob-jr    = tt-fat.var-bob-ped - (tt-fat.var-bob-prod + tt-fat.nr-bob-ordem).

            IF qt-bob-jr < 0 THEN ASSIGN qt-bob-jr = 0.


            IF tt-fat.qt-atendida >= tt-fat.qt-pedida THEN ASSIGN qt-bob-jr = 0.
            
            ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = perc-atend  
                   c-relatorio:range("B" + STRING(i-linha)):VALUE = perc-prod
                   c-relatorio:range("C" + STRING(i-linha)):VALUE = tt-fat.tp-pedido      
                   c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-fat.nr-pedido       
                   c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-fat.nr-sequencia       
                   c-relatorio:range("F" + STRING(i-linha)):VALUE = tt-fat.it-codigo      
                   c-relatorio:range("G" + STRING(i-linha)):VALUE = tt-fat.nome-abrev     
                   c-relatorio:range("H" + STRING(i-linha)):VALUE = tt-fat.qt-pedida
                   c-relatorio:range("I" + STRING(i-linha)):VALUE = produzido-jr 
                   c-relatorio:range("J" + STRING(i-linha)):VALUE = (tt-fat.var-estoque + tt-fat.var-transito + tt-fat.var-terc + tt-fat.qt-atendida)
                   c-relatorio:range("K" + STRING(i-linha)):VALUE = tt-fat.qt-atendida 
                   c-relatorio:range("L" + STRING(i-linha)):VALUE = tt-fat.var-bob-ped      
                   c-relatorio:range("M" + STRING(i-linha)):VALUE = qt-bob-jr      
                   c-relatorio:range("N" + STRING(i-linha)):VALUE = tt-fat.var-Larg      
                   c-relatorio:range("O" + STRING(i-linha)):VALUE = tt-fat.var-diin      
                   c-relatorio:range("P" + STRING(i-linha)):VALUE = tt-fat.var-diex      
                   c-relatorio:range("Q" + STRING(i-linha)):VALUE = tt-fat.dt-entrega    
                   c-relatorio:range("R" + STRING(i-linha)):VALUE = tt-fat.referencia    
                   c-relatorio:range("S" + STRING(i-linha)):VALUE = tt-fat.nome-merc
                   c-relatorio:range("T" + STRING(i-linha)):VALUE = tt-fat.nr-ext
                   c-relatorio:range("U" + STRING(i-linha)):VALUE = tt-fat.campanha
                   c-relatorio:range("V" + STRING(i-linha)):VALUE = tt-fat.densidade
                   c-relatorio:range("W" + STRING(i-linha)):VALUE = tt-fat.espessura
                   c-relatorio:range("X" + STRING(i-linha)):VALUE = tt-fat.gramatura
                   c-relatorio:range("Y" + STRING(i-linha)):VALUE = tt-fat.qt-ordem    
                   c-relatorio:range("Z" + STRING(i-linha)):VALUE = tt-fat.nr-bob-ordem.

         END.

         IF tt-param.l-integr-xtrim THEN RUN pi-grava-xtrim.

         IF tt-param.destino <> 4 THEN DO:

           ASSIGN var-acu = var-acu + tt-fat.var-mtn
                  VAR-acu-t = VAR-acu-t + tt-fat.VAR-terc
                   var-emb-merc = var-emb-merc + tt-fat.var-mtn
                   var-emb-merc-t = var-emb-merc-t + tt-fat.var-terc.

           assign de-qt-pedida-tt-001 = de-qt-pedida-tt-001 + 
                                                tt-fat.qt-pedida
                  de-qt-atendida-tt-002 = de-qt-atendida-tt-002 + 
                                                  tt-fat.qt-atendida
                  var-atendida = var-atendida + tt-fat.qt-atendida
                  var-pedida = var-pedida + tt-fat.qt-pedida.

           if  last-of(tt-fat.nome-merc) then do:
               if  tt-param.formato = 2 then do:

                   display stream str-rp 
                       "" @
                       tt-fat.it-codigo
                       "-----------" @ 
                       tt-fat.qt-atendida
                       "-----------" @ 
                       tt-fat.qt-pedida
                       "------------" @
                       tt-fat.var-mtn
                       "------------" @
                       tt-fat.var-terc
                       with stream-io frame f-relat-09-132.
                   down stream str-rp with frame f-relat-09-132.
               end.

               PUT STREAM str-rp "Total Mercado: " TO 044.
               put stream str-rp var-pedida format ">>>>,>>9.99" to 056.
               put stream str-rp var-atendida format ">>>>,>>9.99" to 070.
               PUT STREAM str-rp var-emb-merc   FORMAT ">>>>,>>9.99" TO 116.
               PUT STREAM str-rp var-emb-merc-t FORMAT ">>>>,>>9.99" TO 129.
               put stream str-rp unformatted skip(1).
               put stream str-rp unformatted skip(1).

           end.

           if  last-of(tt-fat.dt-entrega) then do:
               if  tt-param.formato = 2 then do:

                   display stream str-rp 
                       "" @
                       tt-fat.it-codigo
                       "-----------" @ 
                       tt-fat.qt-atendida
                       "-----------" @ 
                       tt-fat.qt-pedida
                       "------------" @
                       tt-fat.var-mtn
                       "------------" @
                       tt-fat.var-terc
                       with stream-io frame f-relat-09-132.
                   down stream str-rp with frame f-relat-09-132.
               end.

               PUT STREAM str-rp "Total dia: " TO 044.
               put stream str-rp de-qt-pedida-tt-001 format ">>>>,>>9.99" to 056.
               put stream str-rp de-qt-atendida-tt-002 format ">>>>,>>9.99" to 070.
               PUT STREAM str-rp var-acu FORMAT ">>>>,>>9.99" TO 116.
               PUT STREAM str-rp var-acu-t FORMAT ">>>>,>>9.99" TO 129.
               put stream str-rp unformatted skip(1).
               put stream str-rp unformatted skip(1).

              ASSIGN qt-pedida-jr   = qt-pedida-jr   + de-qt-pedida-tt-001
                     qt-atendida-jr = qt-atendida-jr + de-qt-atendida-tt-002
                     VAR-acu-jr = VAR-acu-jr + VAR-acu + VAR-acu-t
                     var-acu = 0 
                     VAR-acu-t = 0.


           end.
        END.

        ASSIGN var-mtn      = 0
               var-vga      = 0
               var-gru      = 0
               var-sbc      = 0
               var-outros   = 0
               var-transito = 0
               var-terc     = 0.

    END.

    IF tt-param.destino <> 4 THEN DO:

       view stream str-rp frame f-cabec.
       view stream str-rp frame f-rodape.
       assign l-imprime = yes.
           display stream str-rp 
                with stream-io no-box frame f-Linha.

       PUT STREAM str-rp "Total do Periodo: " TO 040.
       put stream str-rp qt-pedida-jr   format ">>>>>>>>,>>9.99" to 056.
       put stream str-rp qt-atendida-jr format ">>>>>>>>,>>9.99" to 072.
       PUT STREAM str-rp var-acu-jr     FORMAT ">>>>>>>>,>>9.99" TO 112.

       view stream str-rp frame f-cabec.
       view stream str-rp frame f-rodape.
       assign l-imprime = yes.
           display stream str-rp 
                with stream-io no-box frame f-Linha.


       if  l-imprime = no then do:
           if  tt-param.formato = 2 then do:
               view stream str-rp frame f-cabec.
               view stream str-rp frame f-rodape.
           end.
           disp stream str-rp " " with stream-io frame f-nulo.
       end.

    END.

    IF tt-param.l-integr-xtrim THEN RUN pi-integracao-xtrim.

    if  tt-param.destino <> 4 then DO:

        if  tt-param.destino <> 1 then

            page stream str-rp.

        else do:

            if   tt-param.parametro = yes then

                 page stream str-rp.

        end.

        if  tt-param.parametro then do:

           disp stream str-rp "CLASSIFICAÄ«O" skip(01) with stream-io frame f-imp-cla.
           disp stream str-rp "   PDP"
                with stream-io side-labels overlay row 028 frame f-imp-cla.

           put stream str-rp unformatted skip(1) "IMPRESS«O" skip(1).

           put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
           put stream str-rp unformatted skip "    " "Execuá∆o: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
           put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
           put stream str-rp unformatted skip "    " "Usu†rio : " tt-param.usuario.

        end.

        else
            output stream str-rp close.
    END.

    IF tt-param.destino = 4 THEN DO:

       RUN pi-finaliza-impressao.

    END.

    RETURN 'OK'.
END.

PROCEDURE pi-cria-planilha:

    DEF VAR c-arquivo AS CHAR NO-UNDO.

    c-arquivo = c-arq + 'espd0034' + STRING(time)+ '.xls'.

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

procedure pi-print-editor:

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
end procedure.


PROCEDURE pi-limpa-xtrim:
    IF tt-param.l-integr-xtrim = YES THEN DO:
        /*Limpa tabelas de integraá∆o*/
        FOR EACH XTOrderList EXCLUSIVE-LOCK
            WHERE XTOrderList.SystemID   = 1
              AND XTOrderList.WorkareaId = 1
              AND XTOrderList.RunNumber  = "EXTERNAL":
            DELETE XTOrderList.
        END.

        FOR EACH XTOrderPropertyList EXCLUSIVE-LOCK
            WHERE XTOrderPropertyList.SystemID   = 1
              AND XTOrderPropertyList.WorkareaId = 1
              AND XTOrderPropertyList.RunNumber  = "EXTERNAL":
            DELETE XTOrderPropertyList.
        END.
    END.
END.

PROCEDURE pi-grava-xtrim:

    IF tt-fat.var-bob-ped - (tt-fat.var-bob-prod + tt-fat.nr-bob-ordem) > 0 THEN DO:

        FIND emitente NO-LOCK
            WHERE emitente.nome-abrev = tt-fat.nome-abrev-filho NO-ERROR.

        CREATE XTOrderList.
        ASSIGN XTOrderList.SystemId                                   = 1
               XTOrderList.WorkareaId                                 = 1
               XTOrderList.RunNumber                                  = "EXTERNAL"
               XTOrderList.OrderNumber                                = STRING(tt-fat.nr-pedido) + "|" + STRING(tt-fat.nr-sequencia)
               XTOrderList.SheetOrRoll                                = 1
               XTOrderList.CustBillToCustomer                         = tt-fat.nome-abrev
               XTOrderList.CustEndUser                                = tt-fat.nome-abrev
               XTOrderList.CustShipToCustomer                         = tt-fat.nome-abrev
               XTOrderList.RunGrade                                   = tt-fat.it-codigo
               XTOrderList.DateExMillDate                             = DATETIME(tt-fat.dt-entrega)
               XTOrderList.ParCombineWithOtherOrders                  = "VERDADEIRO"
               XTOrderList.QtyRolls                                   = tt-fat.var-bob-ped - (tt-fat.var-bob-prod + tt-fat.nr-bob-ordem)
               XTOrderList.QtyUndermake                               = emitente.per-max-canc
               XTOrderList.QtyOvermake                                = emitente.perc-fat-ped
               XTOrderList.QtyUnits                                   = 1
               XTOrderList.RollCoreDiameterMM                         = tt-fat.var-diin
               XTOrderList.RollDiameterMM                             = tt-fat.var-diex
               XTOrderList.RollWidthMM                                = tt-fat.var-Larg
               XTOrderList.RunBasisWeightGSM                          = tt-fat.gramatura
               XTOrderList.RunCaliperMicrons                          = tt-fat.espessura
               XTOrderList.RunDensityGM3                              = tt-fat.densidade
               XTOrderList.DetailMustMake                             = ?
               XTOrderList.PatMinAppInPattern                         = ?
               XTOrderList.PatMaxAppInPattern                         = ?
               XTOrderList.ParMinAppInParent                          = ?
               XTOrderList.ParMaxAppInParent                          = ?
               XTOrderList.QtyMinRolls                                = ?
               XTOrderList.QtyMaxRolls                                = ?
               XTOrderList.RollMaxPackedRollWeightKG                  = ?
               XTOrderList.PatIsUtilityOrder                          = ?
               XTOrderList.QtyMaxSheets                               = ?
               XTOrderList.SheetGuillotineWidthMM                     = ?
               XTOrderList.NoWindersAllowed                           = ?
               XTOrderList.SheetLengthMM                              = ?
               XTOrderList.PropertyLookupID                           = ?
               XTOrderList.SheetScoringKnifeCount                     = ?
               XTOrderList.OrderType                                  = ?
               XTOrderList.SheetLiner                                 = ?
               XTOrderList.HostedSheets                               = ?
               XTOrderList.SheetRotation                              = ?
               XTOrderList.HostedWeightKG                             = ?
               XTOrderList.SheetStacksPerPallet                       = ?
               XTOrderList.TrimmedSheets                              = ?
               XTOrderList.SheetWidthMM                               = ?
               XTOrderList.TrimmedWeightKG                            = ?
               XTOrderList.SheetDiameterMinMM                         = ?
               XTOrderList.DateMakeDate                               = ?
               XTOrderList.SheetDiameterMaxMM                         = ?
               XTOrderList.DetailPricePerTonne                        = ?
               XTOrderList.SheetRollLengthMeters                      = ?
               XTOrderList.DetailSpecialInstructions                  = ?
               XTOrderList.SheetDiameterMM                            = ?
               XTOrderList.DetailSourceFromSingleSite                 = ?
               XTOrderList.SheetRollLengthMinMeters                   = ?
               XTOrderList.DetailFaultTolerant                        = ?
               XTOrderList.SheetRollLengthMaxMeters                   = ?
               XTOrderList.ParFinishedGrade                           = ?
               XTOrderList.SheetScoringPositionsMM                    = ?
               XTOrderList.ParMaxEdgeTrimMM                           = ?
               XTOrderList.SheetScoringDescriptionMethod              = ?
               XTOrderList.ParMinEdgeTrimMM                           = ?
               XTOrderList.SheetsPerStack                             = ?
               XTOrderList.ParMaxReelWidthMM                          = ?
               XTOrderList.SheetWidthMultiplicity                     = ?
               XTOrderList.ParSplicingNotAllowed                      = ?
               XTOrderList.RollCoreType                               = ?
               XTOrderList.ParApplyLimitCount                         = ?
               XTOrderList.RollDiameterMaxMM                          = ?
               XTOrderList.ParThirdStageTargetNumOut                  = ?
               XTOrderList.RollDiameterMinMM                          = ?
               XTOrderList.PatMaxPercentAtPosition                    = ?
               XTOrderList.RollIsSideReel                             = ?
               XTOrderList.PatGrouping                                = ?
               XTOrderList.RollMultiPack                              = ?
               XTOrderList.PatIgnoreMinDistanceFromEdge               = ?
               XTOrderList.RollLengthMaxMeters                        = ?
               XTOrderList.QtyMaxWeightKG                             = ?
               XTOrderList.RollLengthMeters                           = ?
               XTOrderList.QtyMinUserDefined                          = ?
               XTOrderList.RollLengthMinMeters                        = ?
               XTOrderList.QtySheets                                  = ?
               XTOrderList.RollLiner                                  = ?
               XTOrderList.QtyWeightKG                                = ?
               XTOrderList.RollWeightKG                               = ?
               XTOrderList.QtyOriginalUndermake                       = ?
               XTOrderList.RollMultiPackMax                           = ?
               XTOrderList.SheetGuillotineIsRequired                  = ?
               XTOrderList.RollThirdStageRewinder                     = ?
               XTOrderList.OrderVersion                               = ?
               XTOrderList.RollThirdStageTargetDiam                   = ?
               XTOrderList.HostedRolls                                = ?
               XTOrderList.RollThirdStageTargetNoOut                  = ?
               XTOrderList.TrimmedRolls                               = ?
               XTOrderList.RollThirdStageTargetCore                   = ?
               XTOrderList.DateDeliveryDate                           = ?
               XTOrderList.RollCoreExtensionMM                        = ?
               XTOrderList.DetailShippingPriority                     = ?
               XTOrderList.RunBasisWeightFactor                       = ?
               XTOrderList.DetailPriority                             = ?
               XTOrderList.RunBasisWeightUnits                        = ?
               XTOrderList.ParLengthMultiplicity                      = ?
               XTOrderList.RunOrderGroup                              = ?
               XTOrderList.ParPositionIndicator                       = ?
               XTOrderList.RunCoreDiameterMM                          = ?
               XTOrderList.ParFamilyName                              = ?
               XTOrderList.RunCoreType                                = ?
               XTOrderList.ParThirdStageTargetDiameter                = ?
               XTOrderList.RunDiameterMM                              = ?
               XTOrderList.PatGroupingStrict                          = ?
               XTOrderList.RunReelDiameterUnits                       = ?
               XTOrderList.QtyMinSheets                               = ?
               XTOrderList.RunReelLengthMeters                        = ?
               XTOrderList.QtyUserDefined                             = ?
               XTOrderList.RunUserDefinedBaseUnit                     = ?
               XTOrderList.QtyImportedKG                              = ?
               XTOrderList.RunUserDefinedMultiply                     = ?
               XTOrderList.StockKey                                   = ?
               XTOrderList.RunUserDefinedScalar                       = ?
               XTOrderList.TrimmedUserDefined                         = ?
               XTOrderList.RunUserDefinedUOM                          = ?
               XTOrderList.DetailBlockID                              = ?
               XTOrderList.RunAllowJumboStockSplicing                 = ?
               XTOrderList.ParMaxPercentAtPosition                    = ?
               XTOrderList.RunAllowMultPatPerStockItem                = ?
               XTOrderList.ParThirdStageTargetCoreDiam                = ?
               XTOrderList.RunStockGrouping                           = ?
               XTOrderList.QtyMaxUserDefined                          = ?
               XTOrderList.PaperMachine                               = ?
               XTOrderList.QtyOriginalOvermake                        = ?
               XTOrderList.SecondaryMachine                           = ?
               XTOrderList.HostedUserDefined                          = ?
               XTOrderList.Winder                                     = ?
               XTOrderList.DetailPushLeft                             = ?
               XTOrderList.OriginalPaperMachine                       = ?
               XTOrderList.PatPositionIndicator                       = ?
               XTOrderList.NoPaperMachinesAllowed                     = ?
               XTOrderList.SheetGuillotineLengthMM                    = ?
               XTOrderList.NoExtruderMachinesAllowed                  = ?
               XTOrderList.ParMinReelWidthMM                          = ?
               XTOrderList.NoDuplexCuttersAllowed                     = ?
               XTOrderList.DetailBufferID                             = ?
               XTOrderList.NoCuttersAllowed                           = ?
               XTOrderList.QtyMinWeightKG                             = ?
               XTOrderList.NoRewindersAllowed                         = ?.

        CREATE XTOrderPropertyList.
        ASSIGN XTOrderPropertyList.SystemId     = 1
               XTOrderPropertyList.WorkareaId   = 1
               XTOrderPropertyList.RunNumber    = "EXTERNAL"
               XTOrderPropertyList.OrderNumber  = STRING(tt-fat.nr-pedido) + "|" + STRING(tt-fat.nr-sequencia)
               XTOrderPropertyList.PropCode     = "Mercado"
               XTOrderPropertyList.AccessMode   = 2
               XTOrderPropertyList.PropValue    = tt-fat.nome-merc.

        CREATE XTOrderPropertyList.
        ASSIGN XTOrderPropertyList.SystemId     = 1
               XTOrderPropertyList.WorkareaId   = 1
               XTOrderPropertyList.RunNumber    = "EXTERNAL"
               XTOrderPropertyList.OrderNumber  = STRING(tt-fat.nr-pedido) + "|" + STRING(tt-fat.nr-sequencia)
               XTOrderPropertyList.PropCode     = "Campanha"
               XTOrderPropertyList.AccessMode   = 2
               XTOrderPropertyList.PropValue    = tt-fat.campanha.
    END.
END.

PROCEDURE pi-integracao-xtrim:
    RUN sfc\essfapi0003.p (OUTPUT TABLE tt-erro).

    IF CAN-FIND(FIRST tt-erro) THEN DO:
        IF tt-param.destino = 4 /*Excel*/ THEN DO:
            RUN cdp\cd0666.w(INPUT TABLE tt-erro).
        END.
        ELSE DO:
            put stream str-rp unformatted
                SKIP(2) "Ocorreram erros durante o processo de integraá∆o X-Trim" SKIP(2).

            FOR EACH tt-erro:
                put stream str-rp unformatted
                    tt-erro.cd-erro SPACE(2)
                    tt-erro.mensagem SKIP.
            END.
        END.
    END.
END.

PROCEDURE pi-atualiza-var-result:
    DEF INPUT PARAMETER p-nome-var  AS CHAR                 NO-UNDO.
    DEF INPUT PARAMETER p-nr-config LIKE ped-item.nr-config NO-UNDO.
    DEF INPUT PARAMETER p-it-codigo LIKE ped-item.it-codigo NO-UNDO.

    DEF BUFFER bf1-var-pad-cf FOR var-pad-cf.

    DEF VAR i-seq AS INT NO-UNDO.

    FIND FIRST bf1-var-pad-cf NO-LOCK
        WHERE bf1-var-pad-cf.nome-var = p-nome-var NO-ERROR.

    IF NOT AVAIL bf1-var-pad-cf THEN RETURN "NOK".

    FIND FIRST var-result NO-LOCK 
        WHERE var-result.nome-var     = p-nome-var
          AND var-result.nr-estrut    = p-nr-config 
          AND var-result.item-cotacao = p-it-codigo NO-ERROR.

    IF AVAIL var-result AND var-result.valor-dec <> 0 THEN RETURN "OK".

    FIND FIRST var-modelo NO-LOCK
        WHERE var-modelo.mo-codigo = p-it-codigo 
          AND var-modelo.nome-var  = p-nome-var  NO-ERROR.

    IF AVAIL var-modelo AND var-modelo.valor-dec <> 0 THEN DO:
        FIND FIRST var-result EXCLUSIVE-LOCK
            WHERE var-result.item-cotacao = p-it-codigo
              AND var-result.nr-estrut    = p-nr-config
              AND var-result.nome-var     = p-nome-var      NO-ERROR.
        IF NOT AVAIL var-result THEN DO:
            FIND LAST var-result NO-LOCK USE-INDEX codigo
                WHERE var-result.item-cotacao = p-it-codigo
                  AND var-result.nr-estrut    = p-nr-config NO-ERROR.
            IF AVAIL var-result THEN
                ASSIGN i-seq = var-result.sequencia + 10.
            ELSE
                ASSIGN i-seq = 10.

            CREATE var-result.
            ASSIGN var-result.item-cotacao = p-it-codigo
                   var-result.mo-codigo    = p-it-codigo
                   var-result.nr-estrut    = p-nr-config
                   var-result.nome-var     = p-nome-var
                   var-result.sequencia    = i-seq
                   var-result.nome-cte     = "".
        END.

        ASSIGN var-result.descricao      = bf1-var-pad-cf.descricao
               var-result.tipo-result    = bf1-var-pad-cf.tipo-result
               var-result.ind-tipo-var   = bf1-var-pad-cf.ind-tipo-var
               var-result.valor-dec      = var-modelo.valor-dec.

        ASSIGN var-result.des-result = STRING(var-modelo.valor-dec).

        ASSIGN var-result.tipo-valor = if var-result.ind-tipo-var = 1 
                                       then 2 
                                       else 4.           
    END.
END.
