/*****************************************************************************
**
**       Programa: escp0039rp.p
**
**       Data....: 16/08/2005
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: NECESSIDADES DE MATERIAIS DE EMBALAGENS
**
**       VersÆo..: 1.00.000 - jrrcampos
**
**       OBS.....: 
**
*******************************************************************************/
define buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "escp0039RP".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

/****************** Defini‡Æo de Tabelas Tempor rias do Relat¢rio **********************/

/* Tabela Pedidos */
def NEW global SHARED temp-table tt-pedidos-39 NO-UNDO
    field pedido AS INTEGER
    FIELD nr-sequencia AS INT.

def new global shared var tt-TipPed AS CHAR FORMAT "X(5)" NO-UNDO.

def temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    field v_num_tip_aces_usuar as integer
    field ep-codigo            as char
    field da-cod-estabel         like ped-venda.cod-estabel
    field da-cod-canal-venda-ini like ped-venda.cod-canal-venda
    field da-cod-canal-venda-fim like ped-venda.cod-canal-venda
    FIELD da-it-codigo-ini       LIKE ped-item.it-codigo
    FIELD da-it-codigo-fim       LIKE ped-item.it-codigo
    FIELD da-nr-pedido-ini       LIKE ped-venda.nr-pedido
    FIELD da-nr-sequencia-ini    LIKE ped-item.nr-sequencia
    FIELD da-nr-pedido-fim       LIKE ped-venda.nr-pedido
    FIELD da-nr-sequencia-fim    LIKE ped-item.nr-sequencia
    FIELD da-nr-ord-produ-ini    LIKE ord-prod.nr-ord-produ
    FIELD da-nr-ord-produ-fim    LIKE ord-prod.nr-ord-produ
    FIELD pesq-jr                AS INTEGER
    FIELD rs-pedido              AS INTEGER
    FIELD rs-tipo-jr             AS INTEGER 
    .

DEFINE TEMP-TABLE tt-embalagem NO-UNDO
    FIELD tt-emb-nr-pedido     LIKE ped-venda.nr-pedido
    FIELD tt-emb-nr-sequencia  LIKE ped-item.nr-sequencia
    field tt-emb-it-codigo     LIKE ped-item.it-codigo
    field tt-emb-it-codigo-mt  LIKE ped-item.it-codigo
    FIELD tt-emb-descricao     LIKE item.desc-item
    FIELD tt-emb-cons-ped      AS DECIMAL 
    FIELD tt-emb-estoque       AS decimal
    FIELD tt-emb-saldo         AS DECIMAL
    INDEX ch-tab-embalagem IS PRIMARY UNIQUE  tt-emb-nr-pedido
                                              tt-emb-nr-sequencia
                                              tt-emb-it-codigo
                                              tt-emb-it-codigo-mt.


DEFINE TEMP-TABLE tt-lotes NO-UNDO
    FIELD tt-lot-lote      LIKE pallet.nr-pallet
    INDEX ch-tab-lote IS PRIMARY UNIQUE  tt-lot-lote.

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

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


/****************** FIM INCLUDE COM VARIµVEIS GLOBAIS *********************/
/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 


def new shared var da-cod-estabel         like ped-venda.cod-estabel format "999"  no-undo. /*solic-318*/ 
def new shared var da-cod-canal-venda-ini like ped-venda.cod-canal-venda format ">9" INITIAL 0 no-undo.
def new shared var da-cod-canal-venda-fim like ped-venda.cod-canal-venda format ">9" INITIAL "99" no-undo.
def new shared var da-it-codigo-ini       like ped-item.it-codigo format "x(16)" no-undo.
def new shared var da-it-codigo-fim       like ped-item.it-codigo format "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" no-undo.
def new shared var da-nr-pedido-ini       like ped-venda.nr-pedido format ">>>>>>9" INITIAL 0 no-undo.
def new shared var da-nr-sequencia-ini    like ped-item.nr-sequencia format ">>>>9" INITIAL 10 no-undo.
def new shared var da-nr-pedido-fim       like ped-venda.nr-pedido format ">>>>>>9" INITIAL 9999999 no-undo.
def new shared var da-nr-sequencia-fim    like ped-item.nr-sequencia format ">>>>9" INITIAL 10 no-undo.
def new shared var da-nr-ord-produ-ini    like ord-prod.nr-ord-produ format ">>>>>>9" INITIAL 0 no-undo.
def new shared var da-nr-ord-produ-fim    like ord-prod.nr-ord-produ format ">>>>>>9" INITIAL 9999999 no-undo.


/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 
DEFINE VARIABLE pesq-jr          AS INTEGER    NO-UNDO.
DEFINE VARIABLE saldo-ped        AS DECIMAL    NO-UNDO.
DEFINE VARIABLE ped-palete-jr    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE ped-bob-jr       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE soma-plt         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE soma-bob         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE nece-plt         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE nece-bob         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE embalagem-jr     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE it-codigo-jr     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE nr-pedido-jr     AS INTEGER    NO-UNDO.
DEFINE VARIABLE nr-sequencia-jr  AS INTEGER    NO-UNDO.
DEFINE VARIABLE nr-pedido-ant    AS INTEGER    NO-UNDO.
DEFINE VARIABLE traco-jr         AS CHARACTER  FORMAT "x(132)" NO-UNDO.
DEFINE VARIABLE nr-ord-produ-jr  AS INTEGER    NO-UNDO.
DEFINE VARIABLE qt-otimizada-jr  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE bb-otimizada-jr  AS DECIMAL    NO-UNDO.

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

/****************** Defini‡ao de Vari veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/

def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.

/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form tt-emb-nr-pedido     COLUMN-LABEL "Ped/Ordem"    FORMAT ">>>>>>>>9" AT 001
     tt-emb-nr-sequencia  COLUMN-LABEL "Ped/Ordem"    FORMAT ">>>>9"     AT 011
     tt-emb-it-codigo     column-label "Filme"        format "x(15)" at 017
     tt-emb-it-codigo-mt  COLUMN-LABEL "Material"     format "x(15)" AT 033
     tt-emb-descricao     column-label "Descri‡Æo"    format "x(42)" at 049
     tt-emb-cons-ped      COLUMN-LABEL "Prev.Consumo" FORMAT "->>>>>,>>9.99" AT 092
     tt-emb-estoque       COLUMN-LABEL "Estoque"      FORMAT "->>>>>,>>9.99" AT 106
     tt-emb-saldo         column-label "Neces.Mater." FORMAT "->>>>,>>9.99"  AT 120
     with down width 132 no-box stream-io frame f-relat-09-132.

form HEADER
    fill("-", 132) format "x(132)" SKIP 
    WITH DOWN WIDTH 132 NO-BOX STREAM-IO FRAME f-relat-linha.

create tt-param.
raw-transfer raw-param to tt-param.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.


def var rw-log-exec                            as rowid no-undo.
def var c-erro-rpc as character format "x(60)" initial " " no-undo.
def var c-erro-aux as character format "x(60)" initial " " no-undo.
def var c-ret-temp as char no-undo.
def var h-servid-rpc as handle no-undo.     
define var c-empresa       as character format "x(40)"      no-undo.
define var c-titulo-relat  as character format "x(50)"      no-undo.
define var i-numper-x      as integer   format "ZZ"         no-undo.
define var da-iniper-x     as date      format "99/99/9999" no-undo.
define var da-fimper-x     as date      format "99/99/9999" no-undo.
define var i-page-size-rel as integer                       no-undo.
define var c-programa      as character format "x(08)"      no-undo.
define var c-versao        as character format "x(04)"      no-undo.
define var c-revisao       as character format "999"        no-undo.

define new shared var c-impressora   as character                      no-undo.
define new shared var c-layout       as character                      no-undo.
define new shared var v_num_count     as integer                       no-undo.
define new shared var c-arq-control   as character                     no-undo.
define new shared var c-sistema       as character format "x(25)"      no-undo.
define new shared var c-rodape        as character                     no-undo.

define new shared buffer b_ped_exec_style for ped_exec.
define new shared buffer b_servid_exec_style for servid_exec.

define new shared stream str-rp.

assign c-programa     = "escp0039rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "NECESSIDADES DE MATERIAIS DE EMBALAGENS"
       c-sistema      = "".

if  tt-param.formato = 2 then do:


form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    fill("-", 112) format "x(110)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabec.

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

    assign da-cod-estabel             = tt-param.da-cod-estabel         
           da-cod-canal-venda-ini     = tt-param.da-cod-canal-venda-ini 
           da-cod-canal-venda-fim     = tt-param.da-cod-canal-venda-fim 
           da-it-codigo-ini           = tt-param.da-it-codigo-ini       
           da-it-codigo-fim           = tt-param.da-it-codigo-fim       
           da-nr-pedido-ini           = tt-param.da-nr-pedido-ini       
           da-nr-sequencia-ini        = tt-param.da-nr-sequencia-ini       
           da-nr-pedido-fim           = tt-param.da-nr-pedido-fim       
           da-nr-sequencia-fim        = tt-param.da-nr-sequencia-fim       
           da-nr-ord-produ-ini        = tt-param.da-nr-ord-produ-ini    
           da-nr-ord-produ-fim        = tt-param.da-nr-ord-produ-fim
           pesq-jr                    = tt-param.pesq-jr.    
       
FIND first empresa no-lock
    where empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  avail empresa
then
    assign c-empresa = empresa.razao-social.
else
    assign c-empresa = "".

/* for each e disp */

def var l-imprime as logical no-undo.

assign l-imprime = no.
if  tt-param.destino = 1 then
    assign v-cod-destino-impres = "Impressora".
else
    if  tt-param.destino = 2 then
        assign v-cod-destino-impres = "Arquivo".
    else
        assign v-cod-destino-impres = "Terminal".


run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0
       traco-jr = fill("-", 132).

IF tt-param.rs-tipo-jr = 1 AND tt-param.rs-pedido = 1 THEN DO:

    IF da-nr-pedido-fim = 9999999 THEN DO:
        FIND LAST ped-venda WHERE
            ped-venda.nr-pedido < da-nr-pedido-fim
            USE-INDEX ch-pedseq
            NO-LOCK NO-ERROR.

        IF AVAIL ped-venda THEN
            ASSIGN da-nr-pedido-fim = ped-venda.nr-pedido.
    END.

    FOR EACH tt-pedidos-39.
        DELETE tt-pedidos-39.
    END.

    ASSIGN nr-pedido-jr = da-nr-pedido-ini.

    IF nr-pedido-jr < 1 THEN
        ASSIGN nr-pedido-jr = 1.
    
    DO WHILE nr-pedido-jr <= da-nr-pedido-fim.

        CREATE tt-pedidos-39.
        ASSIGN tt-pedidos-39.Pedido = nr-pedido-jr
               tt-pedidos-39.nr-sequencia = 10
               nr-pedido-jr = nr-pedido-jr + 1.
      
        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
    
    END.

END.

assign v-num-reg-lidos = 0.


/* Rotina Para Pedido de Venda - Intervalo */

IF tt-param.rs-tipo-jr = 1 AND 
    tt-param.rs-pedido = 1 THEN DO:

    FOR EACH ped-venda WHERE 
        ped-venda.nr-pedido  >= da-nr-pedido-ini AND
        ped-venda.nr-pedido  <= da-nr-pedido-fim AND
        ped-venda.cod-canal-venda >= da-cod-canal-venda-ini AND
        ped-venda.cod-canal-venda <= da-cod-canal-venda-fim AND
        (ped-venda.cod-sit-ped = 1 OR ped-venda.cod-sit-ped = 2) AND  
        ped-venda.cod-estabel = da-cod-estabel   
        NO-LOCK 
        USE-INDEX ch-pedseq:
    
        IF ped-venda.nome-abrev = "POLO - VGA" OR 
           ped-venda.nome-abrev = "POLO - CDGRU" THEN NEXT.
    
        FOR EACH ped-item OF ped-venda
             where ped-item.nome-abrev = ped-venda.nome-abrev AND
                   ped-item.nr-pedcli = ped-venda.nr-pedcli AND
                   ped-item.nr-sequencia >= da-nr-sequencia-ini AND
                   ped-item.nr-sequencia <= da-nr-sequencia-fim AND
                   ped-item.it-codigo >= da-it-codigo-ini AND
                   ped-item.it-codigo <= da-it-codigo-fim  AND
                   ped-item.cod-sit-item < 3 AND  
                   ped-item.ind-componen < 3 AND  
                   ped-item.dt-canseq = ?                  
                   NO-LOCK .

        assign saldo-ped = ped-item.qt-pedida - ped-item.qt-atendida .
    
        IF saldo-ped <= 0 THEN next.
                
                /*Embalagem */
                FIND var-result WHERE 
                    var-result.item-cotacao = ped-item.it-codigo AND 
                    var-result.nr-estrut    = ped-item.nr-config AND
                    var-result.nome-var     = "CODEMBAL"  NO-LOCK no-error.
            
                IF AVAIL var-result then
                    ASSIGN embalagem-jr = var-result.des-result.
            
                IF embalagem-jr = "" THEN NEXT. 
    
                FIND FIRST polo-embalagem WHERE
                    polo-embalagem.cod-embal = INT (embalagem-jr)
                    NO-LOCK NO-ERROR.
            
                IF NOT AVAIL polo-embalagem THEN NEXT.
    
                ASSIGN soma-plt = 0
                       soma-bob = 0.
            
                FOR EACH pallet NO-LOCK WHERE
                    pallet.it-codigo = ped-item.it-codigo and  
                    pallet.nr-pedido = ped-venda.nr-pedido AND
                    pallet.nr-sequencia = ped-item.nr-sequencia AND
                    pallet.situacao = 2
                    USE-INDEX pedido :  
            
                    ASSIGN soma-plt = soma-plt + 1
                           soma-bob = soma-bob + pallet.nr-bobinas.
                END.
    
                FIND FIRST var-result 
                     WHERE var-result.item-cotacao = ped-item.it-codigo
                     AND var-result.nr-estrut    = ped-item.nr-config
                     AND var-result.nome-var     = "QTDPALETE"  NO-LOCK NO-ERROR.
            
                IF AVAIL var-result THEN 
                    ASSIGN ped-palete-jr = var-result.valor-dec.
            
                FIND FIRST var-result 
                     WHERE var-result.item-cotacao = ped-item.it-codigo
                     AND var-result.nr-estrut    = ped-item.nr-config
                     AND var-result.nome-var     = "QTDBOB"  NO-LOCK NO-ERROR.
            
                IF AVAIL var-result THEN 
                    ASSIGN ped-bob-jr = var-result.valor-dec.
            
                ASSIGN nece-plt = ped-palete-jr - soma-plt
                       nece-bob = ped-bob-jr - soma-bob.
                        
                IF nece-plt <= 0 OR nece-bob <= 0 THEN NEXT.
    
                FOR EACH polo-embalagem-estrut NO-LOCK where
                         polo-embalagem-estrut.cod-estabel =  polo-embalagem.cod-estabel AND
                         polo-embalagem-estrut.cod-mercado =  polo-embalagem.cod-mercado AND
                         polo-embalagem-estrut.cod-embal   =  polo-embalagem.cod-embal :
                    
                   FIND FIRST ITEM WHERE
                       ITEM.it-codigo = polo-embalagem-estrut.it-codigo
                       NO-LOCK NO-ERROR.
    
                   IF NOT AVAIL ITEM THEN NEXT.
    
                   IF pesq-jr = 1 THEN
                       ASSIGN it-codigo-jr = ped-item.it-codigo
                              nr-pedido-jr = ped-venda.nr-pedido
                              nr-sequencia-jr = ped-item.nr-sequencia.
                   ELSE
                       ASSIGN it-codigo-jr = ""
                              nr-pedido-jr = 0
                              nr-sequencia-jr = 0.
    
                   FIND FIRST tt-embalagem WHERE
                       tt-emb-nr-pedido    = nr-pedido-jr AND
                       tt-emb-nr-sequencia = nr-sequencia-jr AND
                       tt-emb-it-codigo    = it-codigo-jr AND
                       tt-emb-it-codigo-mt = polo-embalagem-estrut.it-codigo
                       NO-ERROR.
    
                   IF NOT AVAIL tt-embalagem THEN DO:
                       CREATE tt-embalagem.
                       ASSIGN tt-emb-nr-pedido    = nr-pedido-jr 
                              tt-emb-nr-sequencia = nr-sequencia-jr 
                              tt-emb-it-codigo    = it-codigo-jr 
                              tt-emb-it-codigo-mt = polo-embalagem-estrut.it-codigo
                              tt-emb-descricao    = ITEM.DESC-item.
                   END.
    
                   IF polo-embalagem-estrut.tipo-cons = "P" THEN
                       ASSIGN tt-emb-cons-ped = tt-emb-cons-ped + 
                              (polo-embalagem-estrut.quantidade * nece-plt).
                   ELSE
                       ASSIGN tt-emb-cons-ped = tt-emb-cons-ped + 
                              (polo-embalagem-estrut.quantidade * nece-bob).
                
                END.
          
          assign v-num-reg-lidos = v-num-reg-lidos + 1.
          run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
    
        END.
    END.
END.


/* Rotina Para Pedido de Venda - Diversos */

IF tt-param.rs-tipo-jr = 1 AND 
    tt-param.rs-pedido = 2 THEN DO:


  FOR EACH tt-pedidos-39.
    
    FOR EACH ped-venda WHERE 
        ped-venda.nr-pedido  = tt-pedidos-39.pedido AND
        ped-venda.cod-canal-venda >= da-cod-canal-venda-ini AND
        ped-venda.cod-canal-venda <= da-cod-canal-venda-fim AND
        (ped-venda.cod-sit-ped = 1 OR ped-venda.cod-sit-ped = 2) AND  
        ped-venda.cod-estabel = da-cod-estabel   
        NO-LOCK 
        USE-INDEX ch-pedseq:
    
        IF ped-venda.nome-abrev = "POLO - VGA" OR 
           ped-venda.nome-abrev = "POLO - CDGRU" THEN NEXT.
    
        FOR EACH ped-item OF ped-venda
             where ped-item.nome-abrev = ped-venda.nome-abrev AND
                   ped-item.nr-pedcli = ped-venda.nr-pedcli AND
                   ped-item.nr-sequencia = tt-pedidos-39.nr-sequencia AND
                   ped-item.it-codigo >= da-it-codigo-ini AND
                   ped-item.it-codigo <= da-it-codigo-fim  AND
                   ped-item.cod-sit-item < 3 AND  
                   ped-item.ind-componen < 3 AND  
                   ped-item.dt-canseq = ?                  
                   NO-LOCK .

        assign saldo-ped = ped-item.qt-pedida - ped-item.qt-atendida .
    
        IF saldo-ped <= 0 THEN next.
                
                /*Embalagem */
                FIND var-result WHERE 
                    var-result.item-cotacao = ped-item.it-codigo AND 
                    var-result.nr-estrut    = ped-item.nr-config AND
                    var-result.nome-var     = "CODEMBAL"  NO-LOCK no-error.
            
                IF AVAIL var-result then
                    ASSIGN embalagem-jr = var-result.des-result.
            
                IF embalagem-jr = "" THEN NEXT. 
    
                FIND FIRST polo-embalagem WHERE
                    polo-embalagem.cod-embal = INT (embalagem-jr)
                    NO-LOCK NO-ERROR.
            
                IF NOT AVAIL polo-embalagem THEN NEXT.
    
                ASSIGN soma-plt = 0
                       soma-bob = 0.
            
                FOR EACH pallet NO-LOCK WHERE
                    pallet.it-codigo = ped-item.it-codigo and  
                    pallet.nr-pedido = ped-venda.nr-pedido AND  
                    pallet.nr-sequencia = ped-item.nr-sequencia AND
                    pallet.situacao = 2
                    USE-INDEX pedido :  
            
                    ASSIGN soma-plt = soma-plt + 1
                           soma-bob = soma-bob + pallet.nr-bobinas.
                END.
    
                FIND FIRST var-result 
                     WHERE var-result.item-cotacao = ped-item.it-codigo
                     AND var-result.nr-estrut    = ped-item.nr-config
                     AND var-result.nome-var     = "QTDPALETE"  NO-LOCK NO-ERROR.
            
                IF AVAIL var-result THEN 
                    ASSIGN ped-palete-jr = var-result.valor-dec.
            
                FIND FIRST var-result 
                     WHERE var-result.item-cotacao = ped-item.it-codigo
                     AND var-result.nr-estrut    = ped-item.nr-config
                     AND var-result.nome-var     = "QTDBOB"  NO-LOCK NO-ERROR.
            
                IF AVAIL var-result THEN 
                    ASSIGN ped-bob-jr = var-result.valor-dec.
            
                ASSIGN nece-plt = ped-palete-jr - soma-plt
                       nece-bob = ped-bob-jr - soma-bob.
                        
                IF nece-plt <= 0 OR nece-bob <= 0 THEN NEXT.
    
                FOR EACH polo-embalagem-estrut NO-LOCK where
                         polo-embalagem-estrut.cod-estabel =  polo-embalagem.cod-estabel AND
                         polo-embalagem-estrut.cod-mercado =  polo-embalagem.cod-mercado AND
                         polo-embalagem-estrut.cod-embal   =  polo-embalagem.cod-embal :
                    
                   FIND FIRST ITEM WHERE
                       ITEM.it-codigo = polo-embalagem-estrut.it-codigo
                       NO-LOCK NO-ERROR.
    
                   IF NOT AVAIL ITEM THEN NEXT.
    
                   IF pesq-jr = 1 THEN
                       ASSIGN it-codigo-jr = ped-item.it-codigo
                              nr-pedido-jr = ped-venda.nr-pedido
                              nr-sequencia-jr = ped-item.nr-sequencia.
                   ELSE
                       ASSIGN it-codigo-jr = ""
                              nr-pedido-jr = 0
                              nr-sequencia-jr = 0.
    
                   FIND FIRST tt-embalagem WHERE
                       tt-emb-nr-pedido    = nr-pedido-jr AND
                       tt-emb-nr-sequencia = nr-sequencia-jr AND
                       tt-emb-it-codigo    = it-codigo-jr AND
                       tt-emb-it-codigo-mt = polo-embalagem-estrut.it-codigo
                       NO-ERROR.
    
                   IF NOT AVAIL tt-embalagem THEN DO:
                       CREATE tt-embalagem.
                       ASSIGN tt-emb-nr-pedido    = nr-pedido-jr 
                              tt-emb-nr-sequencia = nr-sequencia-jr 
                              tt-emb-it-codigo    = it-codigo-jr 
                              tt-emb-it-codigo-mt = polo-embalagem-estrut.it-codigo
                              tt-emb-descricao    = ITEM.DESC-item.
                   END.
    
                   IF polo-embalagem-estrut.tipo-cons = "P" THEN
                       ASSIGN tt-emb-cons-ped = tt-emb-cons-ped + 
                              (polo-embalagem-estrut.quantidade * nece-plt).
                   ELSE
                       ASSIGN tt-emb-cons-ped = tt-emb-cons-ped + 
                              (polo-embalagem-estrut.quantidade * nece-bob).
                
                END.
          
          assign v-num-reg-lidos = v-num-reg-lidos + 1.
          run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
    
        END.
    END.
  END.
END.


/* Fim Rotina de pedidos */

/* -----------------------------------------
   Rotina Para Ordem de Produ‡Æo 
   -----------------------------------------*/


IF tt-param.rs-tipo-jr = 2 AND tt-param.rs-pedido = 1 THEN DO:


    FOR EACH tt-pedidos-39.
        DELETE tt-pedidos-39.
    END.
    
    FOR EACH ord-prod WHERE 
        ord-prod.cod-estabel  =  da-cod-estabel   AND
        ord-prod.nr-ord-produ >= da-nr-ord-produ-ini AND
        ord-prod.nr-ord-produ <= da-nr-ord-produ-fim AND
        ord-prod.it-codigo    >= da-it-codigo-ini AND
        ord-prod.it-codigo    <= da-it-codigo-fim AND
        ord-prod.estado       <  7                
        USE-INDEX estabel
        NO-LOCK. 
        
        CREATE tt-pedidos-39.
        ASSIGN tt-pedidos-39.Pedido = ord-prod.nr-ord-produ.
      
        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
    
    END.

END.

assign v-num-reg-lidos = 0.

IF tt-param.rs-tipo-jr = 2 THEN DO:
          
    FOR EACH tt-lotes.
        DELETE tt-lotes.
    END.

    FOR EACH tt-pedidos-39.

      FOR EACH ord-prod NO-LOCK USE-INDEX codigo
          WHERE ord-prod.nr-ord-produ = tt-pedidos-39.pedido :
          
                /*Embalagem */
          FIND var-result WHERE 
              var-result.item-cotacao = ord-prod.it-codigo AND 
              var-result.nr-estrut    = ord-prod.nr-estrut AND
              var-result.nome-var     = "CODEMBAL"  NO-LOCK no-error.
          
          IF AVAIL var-result then
              ASSIGN embalagem-jr = var-result.des-result.
          
          IF embalagem-jr = "" THEN NEXT. 
    
          FIND FIRST polo-embalagem WHERE
              polo-embalagem.cod-embal = INT (embalagem-jr)
              NO-LOCK NO-ERROR.
          
          IF NOT AVAIL polo-embalagem THEN NEXT.
    
          ASSIGN soma-plt = 0
                 soma-bob = 0.

          /*Mois‚s - 21/08/2014 
          Altera‡Æo no ESCP039 para considerar o saldo liquido das ordens de 
          produ‡Æo para gera‡Æo de necessidade de embalagem */

          /* Quantidade Cortada e Bobina Cortada - ACA descontando EAC*/
          IF NOT AVAIL param-cp THEN
              FIND FIRST param-cp NO-LOCK NO-ERROR.

          FOR EACH  movto-estoq
              WHERE movto-estoq.nr-ord-produ = ord-prod.nr-ord-produ
                AND movto-estoq.it-codigo    = ord-prod.it-codigo
                AND movto-estoq.cod-estabel  = ord-prod.cod-estabel
                AND movto-estoq.cod-depos <> substring(param-cp.char-2,1,3) /*deposito reciclado*/
                AND (movto-estoq.esp-docto   = 1  OR
                     movto-estoq.esp-docto   = 8)  NO-LOCK
                     USE-INDEX ORD-SEQ :  /*ACA ou EAC*/

              /*Soma os acabados*/
              IF movto-estoq.esp-docto = 1 AND movto-estoq.quantidade > 0 THEN
                 ASSIGN soma-bob = soma-bob + 1.

              /*Desconta os estornos*/
              IF movto-estoq.esp-docto = 8 AND movto-estoq.quantidade > 0 THEN
                 ASSIGN soma-bob = soma-bob - 1.
          END.
          
          FIND var-result WHERE 
              var-result.item-cotacao = ord-prod.it-codigo AND 
              var-result.nr-estrut    = ord-prod.nr-estrut AND
              var-result.nome-var     = "QTDPALETE"  NO-LOCK no-error.
          
          IF AVAIL var-result THEN 
              ASSIGN ped-palete-jr = var-result.valor-dec.
          
          FIND var-result WHERE 
              var-result.item-cotacao = ord-prod.it-codigo AND 
              var-result.nr-estrut    = ord-prod.nr-estrut AND
              var-result.nome-var     = "QTDBOB"  NO-LOCK no-error.
          
          IF AVAIL var-result THEN 
              ASSIGN ped-bob-jr = var-result.valor-dec.

        /* altera‡Æo para pegar quantidade se existir extensao para ordem de produ‡Æo */

        FIND FIRST ped-campanha NO-LOCK 
            WHERE ped-campanha.nr-ord-produ = ord-prod.nr-ord-produ NO-ERROR.

        IF AVAIL ped-campanha THEN DO: 
            ASSIGN qt-otimizada-jr  = ped-campanha.qt-pedida
                   bb-otimizada-jr  = ped-campanha.nr-bobinas.
        END.
        ELSE DO:
            FIND FIRST ext-ord-prod NO-LOCK 
                WHERE ext-ord-prod.nr-ord-produ = ord-prod.nr-ord-produ NO-ERROR.

            IF AVAIL ext-ord-prod THEN DO: 
                ASSIGN qt-otimizada-jr  = ext-ord-prod.qt-pedida
                       bb-otimizada-jr  = ext-ord-prod.nr-bobinas.
            END.
            ELSE
                ASSIGN qt-otimizada-jr  = 0
                       bb-otimizada-jr  = 0.
        END.
    
        IF bb-otimizada-jr <> 0 THEN ASSIGN ped-bob-jr = bb-otimizada-jr.

        ASSIGN ped-bob-jr = ped-bob-jr - soma-bob.

        ASSIGN ped-palete-jr = TRUNCATE(((ped-bob-jr / polo-embalagem.qt-bobinas) + 0.9999),0).
    
        ASSIGN nece-plt = ped-palete-jr 
               nece-bob = ped-bob-jr.

        IF nece-plt <= 0 OR nece-bob <= 0 THEN NEXT.
    
          FOR EACH polo-embalagem-estrut NO-LOCK where
                   polo-embalagem-estrut.cod-estabel =  polo-embalagem.cod-estabel AND
                   polo-embalagem-estrut.cod-mercado =  polo-embalagem.cod-mercado AND
                   polo-embalagem-estrut.cod-embal   =  polo-embalagem.cod-embal :
              
             FIND FIRST ITEM WHERE
                 ITEM.it-codigo = polo-embalagem-estrut.it-codigo
                 NO-LOCK NO-ERROR.
    
             IF NOT AVAIL ITEM THEN NEXT.
    
             IF pesq-jr = 1 THEN
                 ASSIGN it-codigo-jr = ord-prod.it-codigo
                        nr-pedido-jr = ord-prod.nr-ord-produ
                        nr-sequencia-jr = 0.
             ELSE
                 ASSIGN it-codigo-jr = ""
                        nr-pedido-jr = 0
                        nr-sequencia-jr = 0.
    
             FIND FIRST tt-embalagem WHERE
                 tt-emb-nr-pedido    = nr-pedido-jr AND
                 tt-emb-nr-sequencia = nr-sequencia-jr AND
                 tt-emb-it-codigo    = it-codigo-jr AND
                 tt-emb-it-codigo-mt = polo-embalagem-estrut.it-codigo
                 NO-ERROR.
    
             IF NOT AVAIL tt-embalagem THEN DO:
                 CREATE tt-embalagem.
                 ASSIGN tt-emb-nr-pedido    = nr-pedido-jr 
                        tt-emb-nr-sequencia = nr-sequencia-jr
                        tt-emb-it-codigo    = it-codigo-jr 
                        tt-emb-it-codigo-mt = polo-embalagem-estrut.it-codigo
                        tt-emb-descricao    = ITEM.DESC-item.
             END.
    
             IF polo-embalagem-estrut.tipo-cons = "P" THEN
                 ASSIGN tt-emb-cons-ped = tt-emb-cons-ped + 
                        (polo-embalagem-estrut.quantidade * nece-plt).
             ELSE
                 ASSIGN tt-emb-cons-ped = tt-emb-cons-ped + 
                        (polo-embalagem-estrut.quantidade * nece-bob).
          
          END.
          
          assign v-num-reg-lidos = v-num-reg-lidos + 1.
          run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
    
        END.
    END.
END.

/* Fim Rotina de Ordens de Produ‡Æo */

FOR EACH tt-embalagem.

    FOR EACH ext-saldo-estoq WHERE
        ext-saldo-estoq.it-codigo   = tt-emb-it-codigo-mt AND
        ext-saldo-estoq.cod-estabel = da-cod-estabel      AND
        ext-saldo-estoq.LOG-saldo = YES ,
        
        EACH saldo-estoq NO-LOCK WHERE
             saldo-estoq.cod-estabel  = ext-saldo-estoq.cod-estabel      AND
             saldo-estoq.cod-depos    = ext-saldo-estoq.cod-depos        AND
             saldo-estoq.cod-localiz  = ext-saldo-estoq.cod-localiz      AND
             saldo-estoq.lote         = ext-saldo-estoq.lote             AND
             saldo-estoq.it-codigo    = ext-saldo-estoq.it-codigo        AND
             saldo-estoq.cod-refer    = ext-saldo-estoq.cod-refer.       
        
            ASSIGN tt-emb-estoque = tt-emb-estoque + saldo-estoq.qtidade-atu.

    END.

END.

FOR EACH tt-embalagem.

   ASSIGN tt-emb-saldo = tt-emb-cons-ped - tt-emb-estoque.

   IF tt-emb-sald < 0 THEN
       ASSIGN tt-emb-saldo = 0.

       /***  CàDIGO PARA SAÖDA EM 132 COLUNAS ***/
   IF tt-param.pesq-jr = 1 THEN DO:
       IF tt-emb-nr-pedido <> nr-pedido-ant AND
           nr-pedido-ant <> 0 THEN  
           PUT STREAM str-rp traco-jr AT 01
                             " "      AT 01.

       ASSIGN nr-pedido-ant = tt-emb-nr-pedido.
   END.

   view stream str-rp frame f-cabec.
   view stream str-rp frame f-rodape.
   assign l-imprime = yes.
   display stream str-rp
       tt-emb-nr-pedido
       tt-emb-nr-sequencia
       tt-emb-it-codigo
       tt-emb-it-codigo-mt
       tt-emb-descricao
       tt-emb-cons-ped
       tt-emb-estoque
       tt-emb-saldo
       with stream-io frame f-relat-09-132.
       down stream str-rp with frame f-relat-09-132.

END.
     

if  l-imprime = no then do:
    if  tt-param.formato = 2 then do:
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
    end.
    disp stream str-rp " " with stream-io frame f-nulo.
end.

run pi-finalizar in h-acomp.

if  tt-param.destino <> 1 then

    page stream str-rp.

else do:

    if   tt-param.parametro = yes then

         page stream str-rp.

end.

if  tt-param.parametro then do:


   disp stream str-rp "SELE€ÇO" skip(01) with stream-io frame f-imp-sel.
   disp stream str-rp 
      /*da-dt-entrega-ini colon 14 "|< >|"   at 38 da-dt-entrega-fim no-label*/
        with stream-io side-labels overlay row 038 frame f-imp-sel.

   disp stream str-rp "CLASSIFICA€ÇO" skip(01) with stream-io frame f-imp-cla.
   disp stream str-rp "   NECESSIDADES DE MATERIAIS DE EMBALAGENS"
        with stream-io side-labels overlay row 038 frame f-imp-cla.

   put stream str-rp unformatted skip(1) "IMPRESSÇO" skip(1).

   put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
   put stream str-rp unformatted skip "    " "Execu‡Æo: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
   put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
   put stream str-rp unformatted skip "    " "Usu rio : " tt-param.usuario.

end.

else
    output stream str-rp close.

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

return 'OK'.

/* fim do programa */
