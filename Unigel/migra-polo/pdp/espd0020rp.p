/*****************************************************************************
**
**       Programa: espd0020rp.p...
**
**       Data....: 13/08/2007
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Relat¢rio de Pedidos a Embarcar
**
**       Vers∆o..: 1.00.002 - JosÇ Roberto - Edson
**
**       OBS.....: Trocado nomenclatura de mk002po para padrao de especifico 
**
*******************************************************************************/
{bf/buffersUni2.i}
define buffer if-ped-venda for if-ped-venda.
define buffer b-ped-venda for ped-venda.
define buffer bf-if-ped-venda for if-ped-venda.

define variable c-prog-gerado as character no-undo initial "espd0020rp".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

/****************** Definiá∆o de Tabelas Tempor†rias do Relat¢rio **********************/
DEF BUFFER bf-saldo-estoq FOR saldo-estoq.
DEF BUFFER bf1-saldo-estoq FOR saldo-estoq.
def buffer bf-ped-venda for ped-venda.
def buffer bf-ped-item  for ped-item.
DEFINE VARIABLE dt-atu AS DATE        NO-UNDO.
DEFINE VARIABLE dt-ini AS DATE        NO-UNDO.
DEFINE VARIABLE dt-fim AS DATE        NO-UNDO.

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
    field ep-codigo            as character
    field c-cod-estabel-ini like ped-venda.cod-estabel
    field c-cod-estabel-fim like ped-venda.cod-estabel
    field da-dt-entorig-ini like ped-item.dt-entorig
    field da-dt-entorig-fim like ped-item.dt-entorig
    field c-tp-pedido-ini like ped-venda.tp-pedido
    field c-tp-pedido-fim like ped-venda.tp-pedido
    field c-nome-abrev-ini like ped-venda.nome-abrev
    field c-nome-abrev-fim like ped-venda.nome-abrev
    field i-cod-emitente-ini like ped-venda.cod-emitente
    field i-cod-emitente-fim like ped-venda.cod-emitente
    field c-it-codigo-ini like ped-item.it-codigo
    field c-it-codigo-fim like ped-item.it-codigo
    FIELD da-codrep-ini          LIKE repres.cod-rep
    FIELD da-codrep-fim          LIKE repres.cod-rep
    FIELD da-mercado-ini         AS CHAR 
    FIELD da-mercado-fim         AS CHAR 
    field da-canal-venda-ini     like nota-fiscal.cod-canal-venda
    field da-canal-venda-fim     like nota-fiscal.cod-canal-venda
    FIELD c-perc-atend           AS DEC
    field l-unig-com             as logical
    field l-simula-embarque      as logical
    field l-simula-multiplos     as logical
    field dt-embarque            AS DATE
    field dt-embarque-final      AS DATE
.


DEFINE TEMP-TABLE tt-notas NO-UNDO
    FIELD nro-docto    LIKE saldo-terc.nro-docto
    FIELD sequencia    LIKE saldo-terc.sequencia
    FIELD saldo        LIKE saldo-terc.quantidade
    FIELD cod-emitente LIKE saldo-terc.cod-emitente
    FIELD serie        LIKE saldo-terc.serie
    FIELD cod-estabel  LIKE saldo-terc.cod-estabel
    FIELD it-codigo    LIKE saldo-terc.it-codigo
    INDEX chave-notas IS PRIMARY UNIQUE
            nro-docto   
            sequencia   
            cod-emitente
            serie       
            cod-estabel 
.
    
    
                   
                 
DEFINE TEMP-TABLE tt-lotes NO-UNDO
    FIELD nro-docto    LIKE saldo-terc.nro-docto
    FIELD serie        LIKE saldo-terc.serie
    FIELD sequencia    LIKE saldo-terc.sequencia
    FIELD lote         LIKE saldo-terc.lote
    FIELD remessa      LIKE saldo-terc.quantidade
    FIELD retorno      LIKE saldo-terc.quantidade
    FIELD saldo        LIKE saldo-terc.quantidade
    FIELD cod-estabel  LIKE saldo-terc.cod-estabel
    FIELD cod-emitente LIKE saldo-terc.cod-emitente
    FIELD it-codigo    LIKE saldo-terc.it-codigo
    INDEX chave-lotes IS PRIMARY UNIQUE    it-codigo
                    lote
                    nro-docto
                    sequencia
                    cod-emitente
                    cod-estabel
                    serie
                     .


/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

def new global shared var i-ep-codigo-usuario  like mgmulti.empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def new global shared var i-num-ped-exec-rpw  as integer no-undo.   
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
def new global shared var l-rpc as logical no-undo.
def new global shared var r-registro-atual as rowid no-undo.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var i-num-ped as integer no-undo.         
def new global shared var v_cdn_empres_usuar   like mgmulti.empresa.ep-codigo        no-undo.
def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab     as handle                   no-undo.
def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
def new global shared var v_num_tip_aces_usuar   as int                      no-undo.
def new global shared var rw-log-exec            as rowid                    no-undo.


/****************** FIM INCLUDE COM VARIµVEIS GLOBAIS *********************/
/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 

/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 

def new shared var c-cod-estabel-ini  like ped-venda.cod-estabel format "x(3)" initial "" no-undo.
def new shared var c-cod-estabel-fim  like ped-venda.cod-estabel format "x(3)" initial "ZZZ" no-undo.
def new shared var da-dt-entorig-ini  like ped-item.dt-entrega format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dt-entorig-fim  like ped-item.dt-entrega format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var c-tp-pedido-ini    like ped-venda.tp-pedido format "x(2)" initial "" no-undo.
def new shared var c-tp-pedido-fim    like ped-venda.tp-pedido format "x(2)" initial "ZZ" no-undo.
def new shared var c-nome-abrev-ini   like ped-venda.nome-abrev format "x(12)" initial "" no-undo.
def new shared var c-nome-abrev-fim   like ped-venda.nome-abrev format "x(12)" initial "ZZZZZZZZZZZZ" no-undo.
def new shared var i-cod-emitente-ini like ped-venda.cod-emitente format ">>>>>>>>9" initial 0 no-undo.
def new shared var i-cod-emitente-fim like ped-venda.cod-emitente format ">>>>>>>>9" initial 999999999 no-undo.
def new shared var c-it-codigo-ini    like ped-item.it-codigo format "x(16)" initial "" no-undo.
def new shared var c-it-codigo-fim    like ped-item.it-codigo format "x(16)" initial "ZZZZZZZZZZZZZZZZ" no-undo.
def new shared var da-codrep-ini      LIKE repres.cod-rep     FORMAT ">>>9"  INITIAL 0 NO-undo.
def new shared var da-codrep-fim      LIKE repres.cod-rep     FORMAT ">>>9"  INITIAL 9999.
def new shared var da-mercado-ini     AS CHAR                 FORMAT "x(1)"  INITIAL "" NO-undo.
def new shared var da-mercado-fim     AS CHAR                 FORMAT "x(1)"  INITIAL "Z" NO-undo.
def new shared var da-canal-venda-ini like ped-venda.cod-canal-venda format ">>9" initial 0 no-undo.
def new shared var da-canal-venda-fim like ped-venda.cod-canal-venda format ">>9" initial 999 no-undo.
def new shared var c-perc-atend       AS DEC FORMAT ">>9.99%" initial 0 no-undo.

/****************** Definiáao de Vari†veis p/ Campos Virtuais do Relat¢rio *******************/ 
def var c-est as char initial "412,422,434,442,432,443" no-undo. /*solic-318*/ 
def var i-estab as integer no-undo.

DEFINE VARIABLE nome-ab-rep-jr     AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE codrep-jr          AS INTEGER                   NO-UNDO.
DEFINE VARIABLE merc-jr            AS CHAR                      NO-UNDO.
DEFINE VARIABLE saldo-atend        AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE tem-transf-jr      AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE nr-pedcli-jr       AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE linha-jr           AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE tem-data-jr        AS DATE                      NO-UNDO.
DEFINE VARIABLE saldo-pedido       AS DECIMAL                   NO-UNDO.
/****************** Definiáao de Vari†veis Campo do Layout do Relat¢rio **********************/ 

/****************** Definiáao de Vari†veis do Relat¢rio N∆o Pedidas em Tela ******************/ 

/****************** Definiáao de Vari†veis de Total do Relat¢rio *****************************/ 

def var de-qt-atendida-tt-002 like ped-item.qt-atendida no-undo.
def var de-qt-pedida-tt-001   like ped-item.qt-pedida no-undo.
def var qt-atendida-jr        like ped-item.qt-atendida no-undo.
def var qt-pedida-jr          like ped-item.qt-pedida no-undo.
DEF VAR d-perc-atend        AS DEC NO-UNDO.
def var tem-saldo-estoq       as logical no-undo.
DEFINE VARIABLE d-saldo-terc-sp AS DECIMAL    NO-UNDO.
DEFINE VARIABLE d-saldo-terc-rs AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cod-emitente-terc  AS INTEGER EXTENT 10 NO-UNDO.
DEFINE VARIABLE cod-estabel-terc   AS CHAR EXTENT 10 NO-UNDO.

DEFINE VARIABLE sld-emitente-terc  AS DECIMAL EXTENT 10 NO-UNDO.
DEFINE VARIABLE cod-emitente-jr    AS INTEGER           NO-UNDO.
DEFINE VARIABLE i-idx              AS INTEGER           NO-UNDO.
define buffer  b-movto-estoq for  movto-estoq.
/****************** Definiáao de Vari†veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Definiáao de Vari†veis de Processamento do Relat¢rio *********************/

def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.

def var i-ext AS INTEGER NO-UNDO.
DEFINE VARIABLE ext-jr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-obs-ped AS CHARACTER  NO-UNDO.

DEFINE VARIABLE c-obs-pallet AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dt-validade AS DATE       NO-UNDO.
DEFINE VARIABLE dt-producao AS DATE       NO-UNDO.

DEF VAR c-arq             AS CHAR                NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR                NO-UNDO.   
DEF VAR l-cria            AS LOG                 NO-UNDO.
DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
DEF VAR i-linha           AS INT                 NO-UNDO.
DEF VAR i-linhax           AS INT                 NO-UNDO.
DEF VAR c-coluna          AS char                NO-UNDO.

/* para executar o Excel */

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def new global shared var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.



/**********Tabela temporaria************/

define temp-table tt-fat NO-UNDO
    FIELD var-QTDBOB      AS INTEGER FORMAT ">>>>9"         LABEL "QT.BOB."
    FIELD var-QTDPEDIDO   AS INTEGER FORMAT ">>>>,>>>,>>9.9999"         LABEL "QTDPEDIDO"
    FIELD var-Larg        AS INTEGER FORMAT ">,>>9"         LABEL "Larg"
    FIELD var-diin        AS INTEGER FORMAT ">,>>9"         LABEL "DIN"
    FIELD var-diex        AS INTEGER FORMAT ">,>>9"         LABEL "DIEX"
    FIELD var-pedcli      AS CHAR    FORMAT "x(14)"         LABEL "NR.PED.CLIENTE"
    FIELD nr-ext          AS CHAR    FORMAT "x(12)"         LABEL "NR.EXT"
    FIELD var-ung-rs      AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Vga"
    FIELD var-mtn         AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Mtn"
    FIELD var-ung-sbc     AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Gru"
    FIELD var-sbc         AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Sbc"
    FIELD var-outros      AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Out"
    FIELD var-terc        AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Terc"
    FIELD var-transito    AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Trans"
    FIELD nome-merc       AS CHARACTER FORMAT "X(7)"        LABEL "Mercado"
    FIELD dep-terceiro    AS CHAR FORMAT "x12)"
    field tem-transf      AS CHAR FORMAT "x(3)"
    field cod-estabel     like ped-venda.cod-estabel
    field cod-estabel-fat like ped-venda.cod-estabel
    field dt-entrega      like ped-item.dt-entrega
    field dt-implant      like ped-venda.dt-implant
    field nr-pedcli       like ped-venda.nr-pedcli
    FIELD nr-sequencia    LIKE ped-item.nr-sequencia
    field tp-pedido       like ped-venda.tp-pedido
    FIELD cod-canal-venda LIKE ped-venda.cod-canal-venda
    field nome-abrev      like ped-venda.nome-abrev
    field it-codigo       like ped-item.it-codigo
    FIELD nr-pedido       LIKE ped-venda.nr-pedido
    FIELD qt-pedida       LIKE ped-item.qt-pedida
    FIELD qt-atendida     LIKE ped-item.qt-atendida
    FIELD vl-preuni       LIKE ped-item.vl-preuni
    FIELD mo-codigo       LIKE ped-venda.mo-codigo
    FIELD nome-ab-rep-jr  AS CHAR FORMAT "x(12)" LABEL "Represent."
    FIELD nome-transp     AS CHAR FORMAT "x(12)"    LABEL "Transport." 
    FIELD cidade-cif      AS CHAR FORMAT "x(20)"    LABEL "Cidade CIF"
    FIELD cidade-cli      AS CHAR FORMAT "x(20)"    LABEL "Cidade cliente"
    FIELD nome-transp-red AS CHAR FORMAT "x(12)"    LABEL "Transp.Red" 
    FIELD cida-transp-red AS CHAR FORMAT "x(12)"    LABEL "Cidade.Red" 
    FIELD obs-pallet      AS CHAR FORMAT "x(100)"    LABEL "Obs-Pallet." 
    FIELD dt-val          AS DATE FORMAT "99/99/9999" LABEL "Dt.validade"
    FIELD dt-producao     AS DATE FORMAT "99/99/9999" LABEL "Dt.Produá∆o"
    FIELD dt-faturamento  AS DATE FORMAT "99/99/9999"
    FIELD dt-entrega-cli  AS DATE FORMAT "99/99/9999"
    FIELD liber-fat       AS CHAR
    FIELD obs-fat         AS CHAR
    FIELD preco-ex-imp    AS DEC
    field preco-icms      AS DEC
    FIELD total-ped       AS DEC
    FIELD cif-fob         AS CHAR 
    FIELD cond-pagto      AS CHAR 
    field perc-enc-fin    as dec
    field liber-financ    as char
    FIELD sld-emitente    AS DECIMAL EXTENT 10
    FIELD cod-prod-cliente AS CHAR
    FIELD unigel-com      AS CHAR
    FIELD perc-desc        AS DEC
    FIELD preco-sem-desc   AS DEC
    FIELD embarque         AS CHAR
    FIELD desc-embarque    AS CHAR
    FIELD dt-simula-embarque AS DATE.
    

/****************** Definiáao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form tt-fat.cod-estabel label "Estabelecimento" format "x(3)" at 001
     with down width 132 side-labels no-box stream-io frame f-relat-01-132.

form tt-fat.dt-entrega label "Dt Embarque" format "99/99/9999" at 001
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

assign c-programa     = "espd0020rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Relat¢rio de Pedidos a Embarcar"
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

    assign c-cod-estabel-ini = tt-param.c-cod-estabel-ini
           c-cod-estabel-fim = tt-param.c-cod-estabel-fim
           c-cod-estabel-fim = tt-param.c-cod-estabel-fim
           da-dt-entorig-ini = tt-param.da-dt-entorig-ini
           da-dt-entorig-fim = tt-param.da-dt-entorig-fim
           c-tp-pedido-ini = tt-param.c-tp-pedido-ini
           c-tp-pedido-fim = tt-param.c-tp-pedido-fim
           c-nome-abrev-ini = tt-param.c-nome-abrev-ini
           c-nome-abrev-fim = tt-param.c-nome-abrev-fim
           i-cod-emitente-ini = tt-param.i-cod-emitente-ini
           i-cod-emitente-fim = tt-param.i-cod-emitente-fim
           c-it-codigo-ini = tt-param.c-it-codigo-ini
           c-it-codigo-fim = tt-param.c-it-codigo-fim
           da-codrep-ini  = tt-param.da-codrep-ini
           da-codrep-fim  = tt-param.da-codrep-fim 
           da-mercado-ini  = tt-param.da-mercado-ini
           da-mercado-fim  = tt-param.da-mercado-fim 
           da-canal-venda-ini  = tt-param.da-canal-venda-ini
           da-canal-venda-fim  = tt-param.da-canal-venda-fim 
           c-perc-atend        = tt-param.c-perc-atend 

.


find first empresa no-lock
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
        IF tt-param.destino = 3 THEN
        assign v-cod-destino-impres = "Terminal".
        ELSE
            assign v-cod-destino-impres = "Excel".


run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").


IF tt-param.destino = 4 THEN DO:
    
    /* Cria Aplicaá∆o do Excel */

    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

    ASSIGN c-modelo-planilha = search("modelos/mod-espd0020.xls") 
           c-arq             = SESSION:TEMP-DIRECTORY.

    RUN pi-cria-planilha.


END.


assign v-num-reg-lidos = 0.

/*Modificacoes
IF integer(substring(string(it-nota-fisc.nat-operacao),1,1,"character")) >= 7 THEN DO:
define temp-table tt-nec
    field es-codigo       as CHAR FORMAT "X(16)" LABEL "Cod Mat"
    field quant-nec       as DECIMAL FORMAT ">>>,>>>,>>9.99" LABEL "Qtd Nec"
    FIELD ped             AS INTEGER FORMAT ">>>,>>>,>>9"
    FIELD qtd-mat         AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    INDEX ch-prim  IS PRIMARY 
    es-codigo 
    quant-nec 
    ped 
    ASCENDING.
                        CREATE tt-nec.
                        tt-nec.es-codigo = estrutura.es-codigo.
                        tt-nec.quant-nec = var-nec.
                        tt-nec.ped = nr-pedido.
                        tt-nec.qtd-mat = var-qtd-mat.
*/


DEFINE VARIABLE var-Larg     LIKE tt-fat.var-Larg.
DEFINE VARIABLE var-QTDPEDIDO  LIKE tt-fat.var-QTDPEDIDO.
DEFINE VARIABLE var-QTDBOB  LIKE tt-fat.var-QTDBOB.

DEFINE VARIABLE var-diin     LIKE tt-fat.var-diin.
DEFINE VARIABLE var-diex     LIKE tt-fat.var-diex.
DEFINE VARIABLE var-ung-rs   LIKE tt-fat.var-ung-rs    .
DEFINE VARIABLE var-mtn      LIKE tt-fat.var-mtn    .
DEFINE VARIABLE var-ung-sbc  LIKE tt-fat.var-ung-sbc    .
DEFINE VARIABLE var-sbc      LIKE tt-fat.var-sbc    .
DEFINE VARIABLE var-outros   LIKE tt-fat.var-outros .
DEFINE VARIABLE var-outros-TOT   LIKE tt-fat.var-outros .
DEFINE VARIABLE var-terc     LIKE tt-fat.var-terc   .
DEFINE VARIABLE var-transito LIKE tt-fat.var-transito.
DEFINE VARIABLE qtde-jr      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE qtde-jr-t    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE qtde-jr-tr   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE soma-terc-jr AS DECIMAL    NO-UNDO.
DEFINE VARIABLE soma-terc-tr AS DECIMAL    NO-UNDO.
DEFINE VARIABLE var-Merc     LIKE tt-fat.nome-merc.
DEFINE VARIABLE var-pedcli   AS CHARACTER FORMAT "x(14)"  NO-UNDO.
DEFINE VARIABLE var-acu      AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
DEFINE VARIABLE var-acu-t    AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
DEFINE VARIABLE var-acu-jr   AS DECIMAL FORMAT ">>>,>>>,>>9.99" .

ASSIGN cod-emitente-terc = 0
       cod-estabel-terc  = "".

ASSIGN i-linhax = 7. 

if can-find(first ped-item where ped-item.nr-config = 1 and   /* acerta nr-config se algum pedido perdido*/
                            int (ped-item.cod-refer) > 1 USE-INDEX ch-config) then do:
                            
    for each ped-item where ped-item.nr-config = 1 and   /* acerta nr-config se algum pedido perdido*/
                                int (ped-item.cod-refer) > 1 USE-INDEX ch-config EXCLUSIVE-LOCK.
          assign ped-item.nr-config  = int (ped-item.cod-refer) NO-ERROR.
    end.
end.    

/*cria saldo de terceiros em lote*/
    EMPTY TEMP-TABLE tt-lotes.
    EMPTY TEMP-TABLE tt-notas.
  do i-estab = 1 to 5.

    for each   saldo-terc WHERE /*saldo-terc.cod-emitente = 17261 and*/
     saldo-terc.cod-estabel = entry(i-estab,c-est) and
        saldo-terc.quantidade > 0 and
        saldo-terc.lote <> "" and
        saldo-terc.it-codigo >= c-it-codigo-ini and
        saldo-terc.it-codigo <= c-it-codigo-fim

        NO-LOCK.
              
          FIND natur-oper NO-LOCK WHERE 
                         natur-oper.nat-operacao = saldo-terc.nat-operacao.

         IF NOT AVAIL natur-oper OR 
          (natur-oper.terceiro = NO AND
             natur-oper.transf   = NO) THEN NEXT.
             
         if natur-oper.tp-oper-terc > 2 then next. /* consignaá∆o*/


        

  v-num-reg-lidos = v-num-reg-lidos + 1.

  if substring(string(v-num-reg-lidos,"99999999"),7,2) = "00" then 
  run pi-acompanhar in h-acomp(input "Gerando saldo terceiro" + string(v-num-reg-lidos)).
  
          FIND FIRST nota-fiscal WHERE
            nota-fiscal.cod-estabel = saldo-terc.cod-estabel AND
            nota-fiscal.serie       = saldo-terc.serie-docto AND
            nota-fiscal.nr-nota-fis = saldo-terc.nro-docto
            NO-LOCK NO-ERROR.

       IF NOT AVAIL nota-fiscal THEN DO:

            FIND FIRST tt-lotes WHERE
            tt-lotes.it-codigo    = saldo-terc.it-codigo and
             tt-lotes.nro-docto    = saldo-terc.nro-docto and
               tt-lotes.sequencia    = saldo-terc.sequencia and
               tt-lotes.lote         = saldo-terc.lote and
               tt-lotes.cod-estabel  = saldo-terc.cod-estabel and
               tt-lotes.cod-emitente = saldo-terc.cod-emitente
            NO-LOCK NO-ERROR.


            IF NOT AVAIL tt-lotes THEN 
               CREATE tt-lotes.

            ASSIGN tt-lotes.nro-docto    = saldo-terc.nro-docto 
                   tt-lotes.sequencia    = saldo-terc.sequencia 
                   tt-lotes.lote         = saldo-terc.lote
                   tt-lotes.cod-estabel  = saldo-terc.cod-estabel
                   tt-lotes.cod-emitente = saldo-terc.cod-emitente
                   tt-lotes.it-codigo    = saldo-terc.it-codigo.

            ASSIGN tt-lotes.saldo        = tt-lotes.saldo + saldo-terc.quantidade.   

        END.
        else do:


            FIND FIRST tt-notas WHERE
                tt-notas.nro-docto    = saldo-terc.nro-docto AND
                tt-notas.sequencia    = saldo-terc.sequencia and
                tt-notas.cod-emitente = saldo-terc.cod-emitente and
                tt-notas.serie        = saldo-terc.serie and
                tt-notas.cod-estabel  = saldo-terc.cod-estabel 
                NO-LOCK NO-ERROR.

            IF AVAIL tt-notas THEN NEXT.



            CREATE tt-notas.
            ASSIGN tt-notas.nro-docto    = saldo-terc.nro-docto 
                   tt-notas.sequencia    = saldo-terc.sequencia 
                   tt-notas.cod-emitente = saldo-terc.cod-emitente 
                   tt-notas.serie        = saldo-terc.serie 
                   tt-notas.cod-estabel  = saldo-terc.cod-estabel 
                   tt-notas.saldo        = saldo-terc.quantidade
                   tt-notas.it-codigo    = saldo-terc.it-codigo.



                 FOR EACH fat-ser-lote NO-LOCK WHERE
                    fat-ser-lote.cod-estabel = saldo-terc.cod-estabel AND
                    fat-ser-lote.serie       = saldo-terc.serie-docto AND
                    fat-ser-lote.nr-nota-fis = saldo-terc.nro-docto   AND
                    fat-ser-lote.nr-serlote   <> ""  and
                    fat-ser-lote.nr-seq-fat  = saldo-terc.sequencia.

                   FIND FIRST tt-lotes WHERE
                            tt-lotes.it-codigo    = saldo-terc.it-codigo and
                             tt-lotes.nro-docto    = saldo-terc.nro-docto and
                               tt-lotes.sequencia    = saldo-terc.sequencia and
                               tt-lotes.lote         = fat-ser-lote.nr-serlote and
                               tt-lotes.cod-estabel  = saldo-terc.cod-estabel and
                               tt-lotes.cod-emitente = saldo-terc.cod-emitente
                            NO-LOCK NO-ERROR.


                    IF NOT AVAIL tt-lotes THEN 
                       CREATE tt-lotes.

                    ASSIGN tt-lotes.nro-docto    = saldo-terc.nro-docto 
                           tt-lotes.sequencia    = saldo-terc.sequencia 
                           tt-lotes.lote         = fat-ser-lote.nr-serlote
                           tt-lotes.cod-estabel  = saldo-terc.cod-estabel
                           tt-lotes.cod-emitente = saldo-terc.cod-emitente
                           tt-lotes.it-codigo    = fat-ser-lote.it-codigo.

                    ASSIGN tt-lotes.remessa   = tt-lotes.remessa + fat-ser-lote.qt-baixada [1]
                           tt-lotes.saldo     = tt-lotes.remessa - tt-lotes.retorno.          

                 END.



        END.

    END.

    FOR EACH tt-notas NO-LOCK.
              
        
  if substring(string(v-num-reg-lidos,"99999999"),7,2) = "00" then 
  run pi-acompanhar in h-acomp(input "Gerando saldo terceiro" + string(v-num-reg-lidos)).


        FIND FIRST componente WHERE
            componente.cod-emitente = tt-notas.cod-emitente AND
            componente.serie-comp   = tt-notas.serie        AND
            componente.nro-comp     = tt-notas.nro-docto    AND
            componente.seq-comp     = tt-notas.sequencia    AND
            componente.componente   = 1                     AND
            componente.dt-retorno   < 01/01/2012
            NO-LOCK NO-ERROR.  

        IF AVAIL componente THEN NEXT.



        FOR EACH componente NO-LOCK WHERE
            componente.cod-emitente = tt-notas.cod-emitente AND
            componente.serie-comp   = tt-notas.serie        AND
            componente.nro-comp     = tt-notas.nro-docto    AND
            componente.seq-comp     = tt-notas.sequencia.  

            FOR EACH rat-lote NO-LOCK WHERE
                rat-lote.lote        <> "" and
                rat-lote.serie-docto  = componente.serie-docto AND
                rat-lote.nro-docto    = componente.nro-docto   AND
                rat-lote.cod-emitente = tt-notas.cod-emitente  AND
                rat-lote.sequencia    = componente.sequencia   AND
                rat-lote.nat-oper     = componente.nat-oper.

                FIND FIRST fat-ser-lote WHERE
                     fat-ser-lote.cod-estabel = tt-notas.cod-estabel AND
                     fat-ser-lote.serie       = tt-notas.serie       AND
                     fat-ser-lote.nr-nota-fis = tt-notas.nro-docto   AND
                     fat-ser-lote.nr-seq-fat  = tt-notas.sequencia
                     NO-LOCK NO-ERROR.

                IF NOT AVAIL fat-ser-lote THEN NEXT.

                FIND FIRST tt-lotes WHERE
                   tt-lotes.it-codigo    = rat-lote.it-codigo and
                   tt-lotes.nro-docto    = tt-notas.nro-docto and
                   tt-lotes.sequencia    = tt-notas.sequencia and
                   tt-lotes.lote         = rat-lote.lote      and
                   tt-lotes.cod-estabel  = tt-notas.cod-estabel and
                   tt-lotes.cod-emitente = tt-notas.cod-emitente
                NO-LOCK NO-ERROR.

                

                IF NOT AVAIL tt-lotes THEN DO:
                    CREATE tt-lotes.
                    ASSIGN tt-lotes.it-codigo    = rat-lote.it-codigo
                           tt-lotes.nro-docto    = tt-notas.nro-docto 
                           tt-lotes.sequencia    = tt-notas.sequencia 
                           tt-lotes.lote         = rat-lote.lote
                           tt-lotes.cod-estabel  = tt-notas.cod-estabel
                           tt-lotes.cod-emitente = tt-notas.cod-emitente.
                end.     

                ASSIGN tt-lotes.retorno   = tt-lotes.retorno + rat-lote.quantidade
                       tt-lotes.saldo     = tt-lotes.remessa - tt-lotes.retorno.          

            END.
        END.
    END.




    end.  



for each ped-item where 
    /*ped-item.nr-pedcli = "78087"  AND*/
    ped-item.dt-entrega >= da-dt-entorig-ini and 
    ped-item.dt-entrega <= da-dt-entorig-fim and
    ped-item.it-codigo >= c-it-codigo-ini and 
    ped-item.it-codigo <= c-it-codigo-fim and
    ped-item.ind-componen  <> 3           AND
    ped-item.cod-sit-item  < 3 
    USE-INDEX peditem-09
    no-lock, 

    each ped-venda of ped-item no-lock
      where /* ped-venda.dt-entrega >= da-dt-entorig-ini and 
               ped-venda.dt-entrega <= da-dt-entorig-fim and */
               ped-venda.cod-emitente >= i-cod-emitente-ini and 
               ped-venda.cod-emitente <= i-cod-emitente-fim and  
               ped-venda.cod-estabel >= c-cod-estabel-ini   and 
               ped-venda.cod-estabel <= c-cod-estabel-fim   and
               ped-venda.nome-abrev >= c-nome-abrev-ini     and 
               ped-venda.nome-abrev <= c-nome-abrev-fim     and
               ped-venda.tp-pedido >= c-tp-pedido-ini       and 
               ped-venda.tp-pedido <= c-tp-pedido-fim       AND 
               ped-venda.cod-canal-venda >= da-canal-venda-ini and 
               ped-venda.cod-canal-venda <= da-canal-venda-fim and
               ped-venda.cod-sit-ped < 3
       break by ped-venda.cod-estabel
             by ped-item.dt-entrega
             by ped-venda.nome-abrev:
          
       ASSIGN nome-ab-rep-jr = ""
              codrep-jr = 0.


       /* S¢ considera itens da Polo */
       FIND FIRST ITEM OF ped-item NO-LOCK.
       IF NOT AVAIL ITEM THEN NEXT.
       IF ITEM.ge-codigo < 40 OR ITEM.ge-codigo > 49 THEN NEXT.
       /* -------------------------- */

       FIND ped-repre OF ped-venda NO-LOCK NO-ERROR.

       IF AVAIL ped-repre THEN DO:

          FIND repres WHERE 
               repres.nome-abrev = ped-repre.nome-ab-rep
               NO-LOCK NO-ERROR.

          IF AVAIL repres THEN
             ASSIGN nome-ab-rep-jr = ped-repre.nome-ab-rep
                    codrep-jr = repres.cod-rep.
    
       END.

    IF codrep-jr < da-codrep-ini OR codrep-jr >
        da-codrep-fim THEN NEXT.

    var-Merc      = "".

    IF integer(substring(string(ped-item.nat-operacao),1,1)) >= 7 THEN
         ASSIGN merc-jr  = "E"
                var-Merc = "Externo".
        ELSE
         ASSIGN merc-jr = "I"
                var-Merc = "Interno".


     c-obs-ped = ped-venda.observacoes.


    FIND FIRST if-ped-venda WHERE
              if-ped-venda.nr-pedido = ped-venda.nr-pedido
              NO-LOCK NO-ERROR.

    IF AVAIL if-ped-venda THEN DO:
    
      if not tt-param.l-unig-com then next.

          FIND bf-ped-venda WHERE
              bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac
              NO-LOCK NO-ERROR.

          IF AVAIL bf-ped-venda THEN DO:

              c-obs-ped = bf-ped-venda.observacoes.
              
              IF integer(substring(string(bf-ped-venda.nat-operacao),1,1)) >= 7 THEN
                   ASSIGN merc-jr  = "E"
                          var-Merc = "Externo".
                    ELSE
                     ASSIGN merc-jr = "I"
                            var-Merc = "Interno".


          END.
    END.

    
   
    IF merc-jr < da-mercado-ini OR merc-jr >
       da-mercado-fim THEN NEXT.
       
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
     /* if substring(string(v-num-reg-lidos,"99999999"),7,2) = "00" then */
    run pi-acompanhar in h-acomp(input "Gerando TempTable:" + string(v-num-reg-lidos)).
          
    

    ASSIGN 
     var-QTDBOB = 0
     var-QTDPEDIDO  = 0
     var-Larg     = 0 
     var-diin     = 0
     var-diex     = 0
     var-ung-rs   = 0
     var-mtn      = 0
     var-ung-sbc  = 0
     var-sbc      = 0
     var-outros   = 0
     var-terc     = 0
     var-transito = 0
     tem-transf-jr = "" 
     
     var-pedcli    = ""
     tem-data-jr = 01/01/1900.


    

    FIND FIRST var-result WHERE var-result.nome-var = "QTDBOB" AND 
         var-result.nr-estrut = int(ped-item.cod-refer) AND
         var-result.item-cotacao = ped-item.it-codigo 
         /*var-result.sequencia = ped-item.nr-sequencia */ NO-LOCK NO-ERROR.
    IF AVAIL var-result THEN
         ASSIGN var-QTDBOB = var-result.valor-dec.


    
    FIND FIRST var-result WHERE var-result.nome-var = "QTDPEDIDO" AND 
         var-result.nr-estrut = int(ped-item.cod-refer) AND
         var-result.item-cotacao = ped-item.it-codigo 
         /*var-result.sequencia = ped-item.nr-sequencia */ NO-LOCK NO-ERROR.
    IF AVAIL var-result THEN
         ASSIGN var-QTDPEDIDO = var-result.valor-dec.

    FIND FIRST var-result WHERE var-result.nome-var = "Largura" AND 
         var-result.nr-estrut = int(ped-item.cod-refer) AND
         var-result.item-cotacao = ped-item.it-codigo 
         /*var-result.sequencia = ped-item.nr-sequencia */ NO-LOCK NO-ERROR.
    IF AVAIL var-result THEN
         ASSIGN var-Larg = var-result.valor-dec.



    FIND FIRST var-result WHERE var-result.nome-var = "pedcli" AND 
         var-result.nr-estrut = int(ped-item.cod-refer) AND
         var-result.item-cotacao = ped-item.it-codigo 
         /*var-result.sequencia = ped-item.nr-sequencia */ NO-LOCK NO-ERROR.
    IF AVAIL var-result THEN
         ASSIGN var-pedcli = var-result.valor-char.

    FIND FIRST var-result WHERE var-result.nome-var = "DIIN" AND 
         var-result.nr-estrut = int(ped-item.cod-refer) AND
         var-result.item-cotacao = ped-item.it-codigo 
         /*var-result.sequencia = ped-item.nr-sequencia */ NO-LOCK NO-ERROR.
    IF AVAIL var-result THEN
         ASSIGN var-diin = var-result.valor-dec.

    FIND FIRST var-result WHERE var-result.nome-var = "DIEX" AND 
         var-result.nr-estrut = int(ped-item.cod-refer) AND
         var-result.item-cotacao = ped-item.it-codigo 
         /*var-result.sequencia = ped-item.nr-sequencia*/ NO-LOCK NO-ERROR.
    IF AVAIL var-result THEN
         ASSIGN var-diex = var-result.valor-dec.

    ASSIGN c-obs-pallet = ""
           dt-validade = 12/31/9999
           dt-producao = 01/01/2000.

    ASSIGN sld-emitente-terc = 0.


     RUN pi-saldo-pallet (INPUT ped-venda.nr-pedido).

    IF AVAIL if-ped-venda  THEN 
        RUN pi-saldo-pallet (INPUT if-ped-venda.nr-pedido-relac).

    FIND FIRST bf-if-ped-venda WHERE
             bf-if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido
             NO-LOCK NO-ERROR.

    IF AVAIL bf-if-ped-venda THEN do:
    
          RUN pi-saldo-pallet (INPUT bf-if-ped-venda.nr-pedido).
          find first b-ped-venda where b-ped-venda.nr-pedido = bf-if-ped-venda.nr-pedido no-lock no-error.
    end.

    
   /* IF ped-venda.tp-pedido = "A" THEN
        ASSIGN var-Merc = "Amostra". */
     
    ASSIGN ext-jr = "".
      
    if merc-jr = "E" then do:
     
        i-ext = index (c-obs-ped,"EX") .
        
        if i-ext > 0 then do:
           ext-jr = substring(c-obs-ped, i-ext,length(c-obs-ped)).
        
           i-ext = index(ext-jr,")").
           
           if i-ext > 0  then 
              ext-jr = substring(ext-jr,1,i-ext + 1 ).
              
           i-ext = index(ext-jr,"-").
           
           if i-ext > 0 and i-ext > 8   then 
              ext-jr = substring(ext-jr,1,i-ext - 1 ).
        end.

    end.


    FIND FIRST emitente WHERE emitente.cod-emitente = ped-venda.cod-emitente 
                no-lock NO-ERROR.
            
           
    
    
         
    CREATE tt-fat.

    IF ped-venda.nome-tr-red <> "" THEN DO:
        FOR FIRST transporte WHERE transporte.nome-abrev = ped-venda.nome-tr-red NO-LOCK.
            tt-fat.cida-transp-red = transporte.cidade.
        END.
    END.


    ASSIGN 
        tt-fat.var-QTDBOB      = var-QTDBOB
        tt-fat.var-QTDPEDIDO   = var-QTDPEDIDO
        tt-fat.var-Larg        = var-Larg
        tt-fat.var-diin        = var-diin
        tt-fat.var-diex        = var-diex
        tt-fat.var-pedcli      = var-pedcli
        tt-fat.nr-ext          = ext-jr
        tt-fat.var-ung-rs      = var-ung-rs
        tt-fat.var-mtn         = var-mtn
        tt-fat.var-ung-sbc     = var-ung-sbc
        tt-fat.var-sbc         = var-sbc
        tt-fat.var-outros      = var-outros
        tt-fat.var-terc        = var-terc
        tt-fat.var-transito    = var-transito
        tt-fat.tem-transf      = tem-transf-jr
        tt-fat.nome-merc       = var-Merc
        tt-fat.cod-estabel     = if AVAIL bf-if-ped-venda and avail b-ped-venda then b-ped-venda.cod-estabel else ped-venda.cod-estabel
        tt-fat.cod-estabel-fat = if avail bf-if-ped-venda then ped-venda.cod-estabel else if avail  if-ped-venda then if-ped-venda.cod-estab-atend else ped-venda.cod-estabel
        tt-fat.dt-implant      = ped-venda.dt-implant
        tt-fat.nr-pedcli       = ped-venda.nr-pedcli
        tt-fat.nr-sequencia    = ped-item.nr-sequencia
        tt-fat.dt-entrega      = ped-item.dt-entrega
        tt-fat.tp-pedido       = ped-venda.tp-pedido
        tt-fat.cod-canal-venda = ped-venda.cod-canal-venda
        tt-fat.nome-abrev      = ped-venda.nome-abrev
        tt-fat.it-codigo       = ped-item.it-codig
        tt-fat.nr-pedido       = ped-venda.nr-pedido
        tt-fat.nr-pedcli       = ped-venda.nr-pedcli
        tt-fat.qt-pedida       = ped-item.qt-pedida
        tt-fat.qt-atendida     = ped-item.qt-atendida
        tt-fat.vl-preuni       = ped-item.vl-preuni
        tt-fat.preco-icms      = ped-item.vl-preuni 
        tt-fat.mo-codigo       = ped-venda.mo-codigo
        tt-fat.nome-ab-rep-jr  = nome-ab-rep-jr
        tt-fat.nome-transp     = ped-venda.nome-transp
        tt-fat.cidade-cif      = ped-venda.cidade-cif
        tt-fat.cidade-cli      =  IF AVAIL emitente THEN emitente.cidade ELSE ""
        tt-fat.nome-transp-red = ped-venda.nome-tr-red
        tt-fat.obs-pallet      = c-obs-pallet
        tt-fat.dt-val          = dt-validade
        tt-fat.dt-producao     = dt-producao
        tt-fat.unigel-com      = if avail bf-if-ped-venda  then "UC" else ""
        tt-fat.embarque        = ""
        tt-fat.desc-embarque   = ""
        tt-fat.perc-desc       = 0
        tt-fat.preco-sem-desc  = 0.

    FIND FIRST am-pd-prod-cliente WHERE
         am-pd-prod-cliente.cod-emitente  = (IF AVAIL if-ped-venda and AVAIL bf-ped-venda then bf-ped-venda.cod-emitente else ped-venda.cod-emitente) AND 
         am-pd-prod-cliente.it-codigo     = ped-item.it-codigo     AND
         am-pd-prod-cliente.largura       = var-Larg 
         NO-LOCK NO-ERROR.

    IF AVAIL am-pd-prod-cliente THEN 
       ASSIGN 
           tt-fat.cod-prod-cliente =  am-pd-prod-cliente.cod-prod-cliente.
       ELSE
           ASSIGN tt-fat.cod-prod-cliente = "". 

    FIND FIRST estabelec OF ped-venda NO-LOCK.

    IF AVAIL estabelec THEN DO:

        FIND FIRST pd-compl-pedido WHERE
             pd-compl-pedido.ep-codigo     = estabelec.ep-codigo   AND 
             pd-compl-pedido.nr-pedido     = ped-venda.nr-pedido   AND
             pd-compl-pedido.nr-sequencia  = ped-item.nr-sequencia 
             NO-LOCK NO-ERROR.

        IF AVAIL pd-compl-pedido THEN DO:

           ASSIGN 
               tt-fat.preco-ex-imp   =  pd-compl-pedido.preco-pis-cof    
               tt-fat.dt-faturamento =  pd-compl-pedido.dt-faturamento   
               tt-fat.dt-entrega-cli =  pd-compl-pedido.dt-entrega-cli
               tt-fat.obs-fat        =  pd-compl-pedido.narrativa
               tt-fat.perc-desc      =  DEC(SUBSTRING(pd-compl-pedido.char-1,31,10))
               tt-fat.preco-sem-desc =  DEC(SUBSTRING(pd-compl-pedido.char-1,41,10)).

           IF TRIM(SUBSTRING(pd-compl-pedido.char-1,51,10)) <> "" THEN
               tt-fat.var-pedcli = TRIM(SUBSTRING(pd-compl-pedido.char-1,51,10)) + "-" + TRIM(SUBSTRING(pd-compl-pedido.char-1,61,4)).
                                      
           IF  tt-fat.preco-sem-desc = 0 THEN 
               ASSIGN tt-fat.preco-sem-desc = tt-fat.preco-ex-imp
                      tt-fat.perc-desc = 0.
               
           IF pd-compl-pedido.lib-faturamento = YES THEN
               ASSIGN tt-fat.liber-fat = "Sim".
           ELSE
               ASSIGN tt-fat.liber-fat = "N∆o".

           IF pd-compl-pedido.cod-estabel-fat <> "" THEN
               ASSIGN tt-fat.cod-estabel-fat = pd-compl-pedido.cod-estabel-fat
                      tt-fat.preco-icms      = DEC (substring(pd-compl-pedido.char-1,16,15)).
           ELSE
               ASSIGN tt-fat.cod-estabel-fat = pd-compl-pedido.cod-estabel-prod.

        END.

    END.
    
   IF AVAIL if-ped-venda and if-ped-venda.cod-estab-atend <> "" THEN 
   
      tt-fat.cod-estabel-fat = if-ped-venda.cod-estab-atend.
      
   IF AVAIL bf-if-ped-venda  THEN 
   
      tt-fat.cod-estabel-fat = ped-venda.cod-estabel.

   /* Rotina para vers∆o embarque */

      

    find first natur-oper of ped-item no-lock no-error.

    ASSIGN tt-fat.total-ped = (tt-fat.preco-icms * (1 + ((if natur-oper.cd-trib-ipi = 1 then ped-item.aliquota-ipi else 0) / 100))).
    
    find first tab-finan where
         tab-finan.nr-tab-finan = ped-venda.nr-tab-finan
         no-lock no-error.

    IF AVAIL tab-finan THEN DO:

        FIND FIRST tab-finan-indice WHERE
            tab-finan-indice.nr-tab-finan = tab-finan.nr-tab-finan AND
            tab-finan-indice.num-seq      = ped-venda.nr-ind-finan
            NO-LOCK NO-ERROR.

        IF AVAIL tab-finan-indice THEN
            assign tt-fat.perc-enc-fin = 
                (tab-finan-indice.tab-ind-fin - 1) * 100.

    END.

    if ped-venda.cod-sit-aval = 2 or ped-venda.cod-sit-aval = 3 then
       assign tt-fat.liber-financ = "Sim".
    else          
       assign tt-fat.liber-financ = "N∆o".

    FIND FIRST cond-pagto WHERE
        cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag
        NO-LOCK NO-ERROR.

    IF AVAIL cond-pagto THEN
       ASSIGN tt-fat.cond-pagto = cond-pagto.descricao.

    IF ped-venda.cidade-cif = "" THEN
        ASSIGN tt-fat.cif-fob = "Fob".
    ELSE
        ASSIGN tt-fat.cif-fob = "Cif".


    ASSIGN i-idx = 1.

    DO WHILE i-idx < 11.

        ASSIGN tt-fat.sld-emitente[i-idx] = sld-emitente-terc [i-idx].
        ASSIGN i-idx = i-idx + 1.

    END.

    IF tt-param.l-simula-embarque = NO THEN DO:

        ASSIGN tt-fat.embarque = ""
               tt-fat.desc-embarque = "".
    END.

    ELSE DO:

        ASSIGN saldo-pedido = (tt-fat.var-ung-rs + tt-fat.var-mtn + tt-fat.var-ung-sbc +
                               tt-fat.var-sbc + tt-fat.var-outros + tt-fat.var-terc + 
                               tt-fat.var-transito).  

          /*  IF NOT AVAIL if-ped-venda AND tt-fat.liber-fat = "sim" AND tt-fat.tp-pedido <> "E" AND
                tt-fat.dt-entrega <= tt-param.dt-embarque THEN

                ASSIGN tt-fat.embarque = "OK"
                       tt-fat.desc-embarque = "FAT". 
            */
            ASSIGN i-idx   = 1 
                   d-saldo-terc-sp = tt-fat.var-ung-sbc + tt-fat.var-sbc
                   d-saldo-terc-rs = tt-fat.var-ung-rs + tt-fat.var-mtn.

            DO WHILE i-idx < 10.

                IF cod-emitente-terc [i-idx] <> 0 THEN DO:

                    FIND FIRST emitente WHERE emitente.cod-emitente = cod-emitente-terc [i-idx]
                        no-lock NO-ERROR.

                    IF AVAIL emitente AND emitente.estado = "SP" THEN 
                         d-saldo-terc-sp = d-saldo-terc-sp + tt-fat.sld-emitente[i-idx].

                    IF AVAIL emitente AND emitente.estado = "RS" THEN 
                         d-saldo-terc-rs = d-saldo-terc-rs + tt-fat.sld-emitente[i-idx].



                END.
                 ASSIGN i-idx = i-idx + 1.

            END.   


    
            
        tt-fat.dt-simula-embarque = ?.

        DO dt-atu = tt-param.dt-embarque TO tt-param.dt-embarque-final.

 

            IF (tt-fat.cod-estabel-fat = "432" OR tt-fat.cod-estabel-fat = "443") AND tt-fat.liber-fat = "sim" AND  /*solic-318*/ 
               tt-fat.dt-entrega <= dt-atu AND
                 tt-fat.dt-entrega + (IF weekday(dt-atu) = 2 THEN 3 ELSE 1) <= dt-atu  THEN

               IF NOT tt-param.l-simula-multiplos OR tt-fat.dt-simula-embarque = ? OR tt-fat.dt-simula-embarque = dt-atu THEN
                ASSIGN tt-fat.embarque = "OK"
                       tt-fat.desc-embarque = (IF tt-param.l-simula-embarque THEN "FAT" ELSE "FAT ATRASO")
                       tt-fat.dt-simula-embarque = dt-atu. 


            IF (tt-fat.cod-estabel-fat = "432" OR tt-fat.cod-estabel-fat = "443") AND tt-fat.liber-fat = "sim" AND  /*solic-318*/ 
               tt-fat.dt-entrega <= dt-atu AND
                 tt-fat.dt-entrega + (IF weekday(dt-atu) = 2 THEN 3 ELSE 1) <> dt-atu  THEN

               IF NOT tt-param.l-simula-multiplos OR tt-fat.dt-simula-embarque = ? OR tt-fat.dt-simula-embarque = dt-atu THEN
                ASSIGN tt-fat.embarque = "OK"
                       tt-fat.desc-embarque = "TROCA NOTA"
                       tt-fat.dt-simula-embarque = dt-atu.  



            IF (tt-fat.cod-estabel-fat = "432" OR tt-fat.cod-estabel-fat = "443") AND tt-fat.liber-fat = "sim" AND  /*solic-318*/ 
                tt-fat.dt-entrega <= dt-atu AND 
                tt-fat.dt-entrega + (IF weekday(dt-atu) = 2 THEN 3 ELSE 1) <> dt-atu AND
               ( tt-fat.var-ung-sbc + tt-fat.var-sbc + d-saldo-terc-sp) > 0 THEN

                IF NOT tt-param.l-simula-multiplos OR tt-fat.dt-simula-embarque = ? OR tt-fat.dt-simula-embarque = dt-atu THEN
                    ASSIGN tt-fat.embarque = "OK"
                           tt-fat.desc-embarque = "FAT"
                           tt-fat.dt-simula-embarque = dt-atu.  

            IF (tt-fat.cod-estabel-fat = "432" OR tt-fat.cod-estabel-fat = "443") AND tt-fat.liber-fat = "sim" AND  /*solic-318*/ 
                tt-fat.dt-entrega <= dt-atu AND 
                tt-fat.dt-entrega + (IF weekday(dt-atu) = 2 THEN 3 ELSE 1) <> dt-atu AND
                d-saldo-terc-rs > 0 THEN

                IF NOT tt-param.l-simula-multiplos OR tt-fat.dt-simula-embarque = ? OR tt-fat.dt-simula-embarque = dt-atu THEN
                    ASSIGN tt-fat.embarque = "OK"
                           tt-fat.desc-embarque = "TROCA NOTA"
                           tt-fat.dt-simula-embarque = dt-atu. 



            IF  (tt-fat.cod-estabel-fat = "434" OR tt-fat.cod-estabel-fat = "442") AND   /*solic-318*/ 
                tt-fat.liber-fat = "sim"       AND
                tt-fat.dt-entrega <= dt-atu  AND 
                tt-fat.dt-entrega + (IF weekday(dt-atu) = 2 THEN 3 ELSE 1) <> dt-atu  THEN

                IF NOT tt-param.l-simula-multiplos OR tt-fat.dt-simula-embarque = ? OR tt-fat.dt-simula-embarque = dt-atu THEN
                    ASSIGN tt-fat.embarque = "OK"
                           tt-fat.desc-embarque = "FAT"
                           tt-fat.dt-simula-embarque = dt-atu. 


            d-perc-atend = (( tt-fat.qt-atendida + d-saldo-terc-sp) / tt-fat.qt-pedida ) * 100.
            

            IF ((tt-fat.cod-estabel-fat = "432" AND tt-fat.cod-estabel = "422") OR (tt-fat.cod-estabel-fat = "443" AND tt-fat.cod-estabel = "412")) AND  /*solic-318*/ 
               tt-fat.tp-pedido = "E" AND d-saldo-terc-rs > 0 THEN

                IF NOT tt-param.l-simula-multiplos OR tt-fat.dt-simula-embarque = ? OR tt-fat.dt-simula-embarque = dt-atu THEN
                    ASSIGN tt-fat.embarque = "OK"
                           tt-fat.desc-embarque = "ARMAZENA CD"
                           tt-fat.dt-simula-embarque = dt-atu. 

             IF ((tt-fat.cod-estabel-fat = "432" AND tt-fat.cod-estabel = "422") OR (tt-fat.cod-estabel-fat = "443" AND tt-fat.cod-estabel = "412")) AND  /*solic-318*/ 
               tt-fat.liber-fat <> "sim" AND d-saldo-terc-rs > 0 AND d-perc-atend <= 90 THEN

                IF NOT tt-param.l-simula-multiplos OR tt-fat.dt-simula-embarque = ? OR tt-fat.dt-simula-embarque = dt-atu THEN
                    ASSIGN tt-fat.embarque = "OK"
                           tt-fat.desc-embarque = "ARMAZENA CD"
                           tt-fat.dt-simula-embarque = dt-atu. 


             IF (tt-fat.cod-estabel-fat = "432" OR tt-fat.cod-estabel-fat = "443") AND tt-fat.liber-fat <> "sim" AND d-perc-atend <= 90 AND  /*solic-318*/ 
                tt-fat.dt-entrega = dt-atu  AND d-saldo-terc-rs = 0 and
                (d-saldo-terc-sp / (tt-fat.qt-pedida - tt-fat.qt-atendida)) * 100 < 90 
                 THEN
                 IF NOT tt-param.l-simula-multiplos OR tt-fat.dt-simula-embarque = ? OR tt-fat.dt-simula-embarque = dt-atu THEN
                     ASSIGN tt-fat.embarque = "OK"
                            tt-fat.desc-embarque = "ARMAZENA CD"
                            tt-fat.dt-simula-embarque = dt-atu.  


              IF ((tt-fat.cod-estabel-fat = "432" AND tt-fat.cod-estabel = "422") OR (tt-fat.cod-estabel-fat = "443" AND tt-fat.cod-estabel = "412")) AND d-perc-atend <= 90 AND  /*solic-318*/ 
               tt-fat.liber-fat = "sim" AND d-saldo-terc-rs > 0 and
                  tt-fat.dt-entrega >=  dt-atu  + 9 - WEEKDAY(dt-atu)THEN
                IF NOT tt-param.l-simula-multiplos OR tt-fat.dt-simula-embarque = ? OR tt-fat.dt-simula-embarque = dt-atu THEN
                    ASSIGN tt-fat.embarque = "OK"
                           tt-fat.desc-embarque = "ARMAZENA CD"
                           tt-fat.dt-simula-embarque = dt-atu. 


              IF  tt-fat.desc-embarque = "ARMAZENA CD" AND d-saldo-terc-rs = 0 AND tt-fat.var-transito > 0 THEN
                  IF NOT tt-param.l-simula-multiplos OR tt-fat.dt-simula-embarque = ? OR tt-fat.dt-simula-embarque = dt-atu THEN
                      ASSIGN tt-fat.desc-embarque = ""
                             tt-fat.embarque = ""
                             tt-fat.dt-simula-embarque = dt-atu.

        END.
    END. /* else do */

END.

DEFINE VARIABLE var-atendida AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
DEFINE VARIABLE var-pedida AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
DEFINE VARIABLE var-emb-merc   AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
DEFINE VARIABLE var-emb-merc-t AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
DEFINE VARIABLE VAR-branco   AS CHARACTER  INITIAL "" NO-UNDO.


IF tt-param.destino = 4 THEN DO:

    ASSIGN i-linhax = 4
           i-idx   = 1. 

    DO WHILE i-idx < 10.

        IF cod-emitente-terc [i-idx] <> 0 THEN DO:
        
            FIND FIRST emitente WHERE emitente.cod-emitente = cod-emitente-terc [i-idx]
                no-lock NO-ERROR.
            
            IF AVAIL emitente THEN DO:

                IF i-idx = 1 THEN ASSIGN c-relatorio:range("AI" + STRING(i-linhax)):VALUE = cod-estabel-terc[i-idx] + "-" + string(trim(emitente.nome-abrev) + "-" + emitente.estado).     
                IF i-idx = 2 THEN ASSIGN c-relatorio:range("AJ" + STRING(i-linhax)):VALUE = cod-estabel-terc[i-idx] + "-" + string(trim(emitente.nome-abrev) + "-" + emitente.estado).     
                IF i-idx = 3 THEN ASSIGN c-relatorio:range("AK" + STRING(i-linhax)):VALUE = cod-estabel-terc[i-idx] + "-" + string(trim(emitente.nome-abrev) + "-" + emitente.estado).     
                IF i-idx = 4 THEN ASSIGN c-relatorio:range("AL" + STRING(i-linhax)):VALUE = cod-estabel-terc[i-idx] + "-" + string(trim(emitente.nome-abrev) + "-" + emitente.estado).     
                IF i-idx = 5 THEN ASSIGN c-relatorio:range("AM" + STRING(i-linhax)):VALUE = cod-estabel-terc[i-idx] + "-" + string(trim(emitente.nome-abrev) + "-" + emitente.estado).     
                IF i-idx = 6 THEN ASSIGN c-relatorio:range("AN" + STRING(i-linhax)):VALUE = cod-estabel-terc[i-idx] + "-" + string(trim(emitente.nome-abrev) + "-" + emitente.estado).     
                IF i-idx = 7 THEN ASSIGN c-relatorio:range("AO" + STRING(i-linhax)):VALUE = cod-estabel-terc[i-idx] + "-" + string(trim(emitente.nome-abrev) + "-" + emitente.estado).     

                ASSIGN i-linhax = 7.

                IF i-idx = 1 THEN ASSIGN c-relatorio:range("AI" + STRING(i-linhax)):VALUE = cod-estabel-terc[i-idx] + "-" + string(trim(emitente.nome-abrev) + "-" + emitente.estado).     
                IF i-idx = 2 THEN ASSIGN c-relatorio:range("AJ" + STRING(i-linhax)):VALUE = cod-estabel-terc[i-idx] + "-" + string(trim(emitente.nome-abrev) + "-" + emitente.estado).     
                IF i-idx = 3 THEN ASSIGN c-relatorio:range("AK" + STRING(i-linhax)):VALUE = cod-estabel-terc[i-idx] + "-" + string(trim(emitente.nome-abrev) + "-" + emitente.estado).     
                IF i-idx = 4 THEN ASSIGN c-relatorio:range("AL" + STRING(i-linhax)):VALUE = cod-estabel-terc[i-idx] + "-" + string(trim(emitente.nome-abrev) + "-" + emitente.estado).     
                IF i-idx = 5 THEN ASSIGN c-relatorio:range("AM" + STRING(i-linhax)):VALUE = cod-estabel-terc[i-idx] + "-" + string(trim(emitente.nome-abrev) + "-" + emitente.estado).     
                IF i-idx = 6 THEN ASSIGN c-relatorio:range("AN" + STRING(i-linhax)):VALUE = cod-estabel-terc[i-idx] + "-" + string(trim(emitente.nome-abrev) + "-" + emitente.estado).     
                IF i-idx = 7 THEN ASSIGN c-relatorio:range("AO" + STRING(i-linhax)):VALUE = cod-estabel-terc[i-idx] + "-" + string(trim(emitente.nome-abrev) + "-" + emitente.estado).     


                ASSIGN i-linhax = 4.
                                                          
            END.

        END.
        ELSE DO:
        
        
             IF i-idx = 1 THEN ASSIGN c-relatorio:Columns("AI:AI"):Hidden = True.
             IF i-idx = 2 THEN ASSIGN c-relatorio:Columns("AJ:AJ"):Hidden = True.
             IF i-idx = 3 THEN ASSIGN c-relatorio:Columns("AK:AK"):Hidden = True.
             IF i-idx = 4 THEN ASSIGN c-relatorio:Columns("AL:AL"):Hidden = True.
             IF i-idx = 5 THEN ASSIGN c-relatorio:Columns("AM:AM"):Hidden = True.
             IF i-idx = 6 THEN ASSIGN c-relatorio:Columns("AN:AN"):Hidden = True.
             IF i-idx = 7 THEN ASSIGN c-relatorio:Columns("AO:AO"):Hidden = True.            
         END.

        ASSIGN i-idx = i-idx + 1.

    END.

    IF  tt-param.l-simula-embarque THEN DO:
         ASSIGN c-relatorio:Columns("AR:AS"):Hidden = TRUE
                c-relatorio:Columns("AX:BD"):Hidden = TRUE
                c-relatorio:range("J6"):VALUE = "Data Simula"
                c-relatorio:range("J7"):VALUE = "Embarque"
                c-relatorio:Columns("j:j"):ColumnWidth = 10
                c-relatorio:Columns("R:R"):Hidden = True.
                c-relatorio:Columns("U:V"):Hidden = True.
                c-relatorio:Columns("A:A"):Hidden = True.
                c-relatorio:Columns("Z:AD"):Hidden = True.

                IF NOT tt-param.l-simula-multiplos THEN
                     c-relatorio:Columns("j:j"):Hidden = True.

             IF var-outros-TOT = 0 THEN
                  ASSIGN c-relatorio:Columns("AP:AP"):Hidden = True.
    END.
    ELSE DO:
       ASSIGN c-relatorio:Columns("O:P"):Hidden = True.
    END.


END.

ASSIGN i-linhax = 7.

v-num-reg-lidos = 0.

for each tt-fat WHERE (IF tt-param.l-simula-multiplos AND tt-fat.tp-pedido = "E" THEN FALSE ELSE TRUE) no-lock
    break by tt-fat.cod-estabel
          by tt-fat.dt-entrega
          BY tt-fat.nome-merc
          by tt-fat.nome-abrev:
          
   
    
    v-num-reg-lidos = v-num-reg-lidos + 1.
    if substring(string(v-num-reg-lidos,"99999999"),8,1) = "0" then 
        run pi-acompanhar in h-acomp(input "Gerando planilha:" + string(v-num-reg-lidos)).



    if first-of(tt-fat.dt-entrega) then do:
        assign de-qt-atendida-tt-002 = 0
               de-qt-pedida-tt-001 = 0.
    end.
    if first-of(tt-fat.nome-merc) then do:
        assign var-atendida = 0
               var-pedida   = 0
               var-emb-merc   = 0
               var-emb-merc-t = 0.
    end.
    ASSIGN saldo-atend = tt-fat.qt-pedida - tt-fat.qt-atendida.

    IF c-perc-atend > 0 AND tt-fat.qt-atendida > 0 THEN DO:

        IF ((saldo-atend / tt-fat.qt-pedida) * 100) < c-perc-atend THEN NEXT.

    END.
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

 
        ASSIGN linha-jr = ""
               i-linhax = i-linhax + 1.
 
        ASSIGN linha-jr = 
               STRING(tt-fat.cod-estabel)                     + chr(160) +
               STRING(tt-fat.cod-estabel-fat)                 + chr(160) +

               STRING(tt-fat.dt-entrega    )                 + chr(160) +
               (IF tt-fat.dt-faturamento = ? THEN "" ELSE STRING(tt-fat.dt-faturamento,"99/99/9999"))                + chr(160) +
               (IF tt-fat.dt-entrega-cli = ? THEN "" ELSE STRING(tt-fat.dt-entrega-cli,"99/99/9999"))             + chr(160) +
                STRING(tt-fat.tp-pedido     )                 + chr(160) +
               STRING(tt-fat.nr-pedido     )                 + chr(160) +
               STRING(tt-fat.nr-sequencia  )                 + chr(160) +
               STRING(tt-fat.nr-pedcli     )                 + chr(160) +                         
               STRING(IF tt-param.l-simula-multiplos THEN (IF tt-fat.dt-simula-embarque = ? THEN "" ELSE string(tt-fat.dt-simula-embarque,"99/99/9999")) ELSE string(tt-fat.unigel-com)    )                 + chr(160) +
               STRING(tt-fat.liber-fat     )                 + chr(160) +

            
               replace(replace(replace(replace(STRING(tt-fat.obs-fat       ) , CHR(10)," "),chr(13),"-"),CHR(8)," "),CHR(9)," ")            + chr(160) +

               STRING(tt-fat.embarque      )                 + chr(160) +
               STRING(tt-fat.desc-embarque )                 + chr(160) +

               STRING(tt-fat.tem-transf    )                 + chr(160) +
               STRING(tt-fat.nome-abrev    )                 + chr(160) +
               STRING(tt-fat.it-codigo     )                 + chr(160) +
               STRING(replace(replace(tt-fat.var-pedcli ,chr(9),""),chr(160),"")  )                 + chr(160) +
               STRING(tt-fat.qt-pedida     )                 + chr(160) +
               STRING(tt-fat.qt-atendida   )                 + chr(160) +            
               STRING(tt-fat.var-QTDPEDIDO )                 + chr(160) +
               STRING(tt-fat.var-QTDBOB    )                 + chr(160) +
               STRING(tt-fat.var-Larg      )                 + chr(160) +
               STRING(tt-fat.var-diin      )                 + chr(160) +
               STRING(tt-fat.var-diex      )                 + chr(160) +
               STRING(tt-fat.preco-sem-desc)                 + chr(160) +
               STRING(tt-fat.perc-desc     )                 + chr(160) +
               STRING(tt-fat.preco-ex-imp )                  + chr(160) +
               STRING(tt-fat.preco-icms   )                  + chr(160) +
               STRING(tt-fat.total-ped * 
                      (tt-fat.var-ung-rs + tt-fat.var-mtn + tt-fat.var-ung-sbc +
                       tt-fat.var-sbc + tt-fat.var-outros + tt-fat.var-terc + 
                       tt-fat.var-transito))                 + chr(160) +

               STRING(tt-fat.var-mtn      )                 + chr(160) +
               STRING(tt-fat.var-transito )                 + chr(160) +
               STRING(tt-fat.var-ung-rs   )                 + chr(160) +
               STRING(tt-fat.var-ung-sbc  )                 + chr(160) +

               STRING(IF tt-fat.sld-emitente [1] <> 0 THEN tt-fat.sld-emitente [1] ELSE 0)                 + chr(160) +
               STRING(IF tt-fat.sld-emitente [2] <> 0 THEN tt-fat.sld-emitente [2] ELSE 0)                 + chr(160) +
               STRING(IF tt-fat.sld-emitente [3] <> 0 THEN tt-fat.sld-emitente [3] ELSE 0)                 + chr(160) +
               STRING(IF tt-fat.sld-emitente [4] <> 0 THEN tt-fat.sld-emitente [4] ELSE 0)                 + chr(160) +
               STRING(IF tt-fat.sld-emitente [5] <> 0 THEN tt-fat.sld-emitente [5] ELSE 0)                 + chr(160) +
               STRING(IF tt-fat.sld-emitente [6] <> 0 THEN tt-fat.sld-emitente [6] ELSE 0)                 + chr(160) +
               STRING(IF tt-fat.sld-emitente [7] <> 0 THEN tt-fat.sld-emitente [7] ELSE 0)                 + chr(160) +

               STRING(tt-fat.var-outros)                 + chr(160) +

               STRING((tt-fat.var-ung-rs + tt-fat.var-mtn + tt-fat.var-ung-sbc +                
                      tt-fat.var-sbc + tt-fat.var-outros + tt-fat.var-terc +      
                      tt-fat.var-transito))               + chr(160) +


               STRING(tt-fat.nr-ext    )                 + chr(160) +
               STRING(tt-fat.nome-merc )                 + chr(160) +
               STRING(tt-fat.cif-fob   )                 + chr(160) +


               STRING(tt-fat.nome-transp     )                 + chr(160) +
               STRING(tt-fat.cidade-cif      )                 + chr(160) +
               STRING(tt-fat.cidade-cli)                 + chr(160) +
            
               STRING(tt-fat.nome-transp-red )                 + chr(160) +
               string(tt-fat.cida-transp-red)                  + chr(160) +
               STRING(tt-fat.cond-pagto      )                 + chr(160) +
               STRING(tt-fat.nome-ab-rep-jr  )                 + chr(160) +


               STRING(if tt-fat.perc-enc-fin <> 0 then tt-fat.perc-enc-fin else 0) + chr(160) +


               STRING(tt-fat.liber-financ    )                 + chr(160) +
               STRING(tt-fat.dt-implant      )                 + chr(160) +

          
               STRING(tt-fat.cod-prod-cliente)                /* + chr(160)*/ .

                      ASSIGN var-outros-TOT = var-outros-TOT + tt-fat.var-outros.

        FIND FIRST if-ped-venda WHERE
            if-ped-venda.nr-pedido = tt-fat.nr-pedido
            NO-LOCK NO-ERROR.

        IF AVAIL if-ped-venda THEN DO:

            FIND bf-ped-venda WHERE
                bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac
                NO-LOCK NO-ERROR.

            IF AVAIL bf-ped-venda THEN DO:

                FIND FIRST bf-ped-item OF bf-ped-venda WHERE
                       bf-ped-item.nr-sequencia = tt-fat.nr-sequencia AND
                       bf-ped-item.ind-componen <> 3
                       NO-LOCK NO-ERROR.

                IF AVAIL bf-ped-item THEN DO:

                /*    ASSIGN linha-jr = linha-jr + STRING(bf-ped-venda.nome-abrev) + chr(160) +
                                                 STRING(bf-ped-venda.nr-pedido) + chr(160) + 
                                                 STRING(bf-ped-item.qt-pedida) + chr(160) +  
                                                 STRING(bf-ped-item.qt-atendida).
                  */
                END.
                   

            END.

        END.
 
        ASSIGN c-relatorio:range("A" + STRING(i-linhax)):value = string(linha-jr).  
         
     END.


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
           var-ung-rs      = 0
           var-ung-sbc      = 0
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

   PUT STREAM str-rp "Total do Periodo: " TO 044.
   put stream str-rp qt-pedida-jr   format ">>>>,>>9.99" to 056.
   put stream str-rp qt-atendida-jr format ">>>>,>>9.99" to 070.
   PUT STREAM str-rp var-acu-jr     FORMAT ">>>>,>>9.99" TO 116.
   
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

run pi-finalizar in h-acomp.

if  tt-param.destino <> 4 then DO:

    if  tt-param.destino <> 1 then
    
        page stream str-rp.
    
    else do:
    
        if   tt-param.parametro = yes then
    
             page stream str-rp.
    
    end.
    
    if  tt-param.parametro then do:
    
    
       disp stream str-rp "SELEÄ«O" skip(01) with stream-io frame f-imp-sel.
       disp stream str-rp 
          c-cod-estabel-ini colon 20 "|< >|"   at 44 c-cod-estabel-fim no-label
          da-dt-entorig-ini colon 20 "|< >|"   at 44 da-dt-entorig-fim no-label
          c-tp-pedido-ini colon 20 "|< >|"   at 44 c-tp-pedido-fim no-label
          c-nome-abrev-ini colon 20 "|< >|"   at 44 c-nome-abrev-fim no-label
          i-cod-emitente-ini colon 20 "|< >|"   at 44 i-cod-emitente-fim no-label
          c-it-codigo-ini colon 20 "|< >|"   at 44 c-it-codigo-fim no-label
            with stream-io side-labels overlay row 028 frame f-imp-sel.
    
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


    c-relatorio:range("a8:a" + STRING(i-linhax  )):SELECT.
    /*c-excel:SELECTION:TextToColumns (,
                                         1,
                                         ,
                                         ,
                                         ,
                                         TRUE,
                                         ,
                                         ,
                                         ,
                                         ,
                                         ,
                                         ) NO-ERROR.*/

  c-excel:selection:TextToColumns    (,         /* Destination          */
                                         1,        /* DataType             */
                                         ,         /* TextQualifier        */
                                         ,         /* ConsecutiveDelimiter */
                                         ,         /* Tab                  */
                                         ,         /* Semicolon            */
                                         ,         /* Comma                */
                                         ,         /* Space                */
                                         true,     /* Other                */
                                         CHR(160), /* OtherChar            */
                                         ,         /* FieldInfo            */
                                         ) no-error.



  
   RUN pi-finaliza-impressao.

   RETURN 'OK'.

END.

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


PROCEDURE pi-cria-planilha:

DEF VAR c-arquivo AS CHAR NO-UNDO.

    c-arquivo = c-arq + 'espd0020' + STRING(time)+ '.xls'.

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
 
    OS-COPY VALUE(c-arquivo) V:\TEMP.

END PROCEDURE.                  


PROCEDURE pi-saldo-pallet.
    DEF INPUT PARAM p-nr-pedido LIKE ped-venda.nr-pedido NO-UNDO.
      

    FOR EACH pallet WHERE 
        pallet.nr-pedido    = p-nr-pedido AND
        pallet.it-codigo    = ped-item.it-codigo AND 
        pallet.nr-sequencia = ped-item.nr-sequencia AND        
        pallet.situacao  = 2
        USE-INDEX pedido NO-LOCK.
        

        FOR EACH it-pallet OF pallet
            NO-LOCK.

            FIND LAST movto-mat WHERE
                          movto-mat.it-codigo = it-pallet.it-codigo AND
                          movto-mat.lote      = it-pallet.lote-bobina AND
                          movto-mat.esp-docto = 01 USE-INDEX lote NO-LOCK NO-ERROR.

            IF AVAIL movto-mat AND movto-mat.dt-trans > dt-producao THEN 
                ASSIGN dt-producao = movto-mat.dt-trans.

        END.

        IF substring(pallet.char-1,1,20) <> "" THEN
            c-obs-pallet = c-obs-pallet + trim(pallet.nr-pallet) + "-" + trim(substring(pallet.char-1,1,20)) + ", ".

        ASSIGN qtde-jr    = 0
               qtde-jr-t  = 0
               qtde-jr-tr = 0.


        FOR EACH saldo-estoq FIELDS (dt-vali-lote cod-estabel qtidade-atu it-codigo lote) WHERE
            saldo-estoq.lote      = pallet.nr-pallet AND 
            saldo-estoq.it-codigo = pallet.it-codigo AND 
            saldo-estoq.cod-refer = pallet.cod-refer AND  
            saldo-estoq.qtidade-atu > 0
            NO-LOCK USE-INDEX lote :

 
            IF saldo-estoq.dt-vali-lote < dt-validade THEN
                dt-validade = saldo-estoq.dt-vali-lote.
        
            IF saldo-estoq.cod-estabel = "423" OR saldo-estoq.cod-estabel = "413" THEN    /*solic-318*/ 
                ASSIGN var-outros = var-outros + saldo-estoq.qtidade-atu.
            ELSE
             IF (saldo-estoq.cod-estabel = "422" OR saldo-estoq.cod-estabel = "412") THEN /*solic-318*/ 
                 ASSIGN var-mtn = var-mtn + saldo-estoq.qtidade-atu.
             ELSE
              IF saldo-estoq.cod-estabel = "432" OR saldo-estoq.cod-estabel = "443" THEN  /*solic-318*/ 
                 ASSIGN var-ung-sbc = var-ung-sbc + saldo-estoq.qtidade-atu.
              ELSE
              IF saldo-estoq.cod-estabel = "434" OR saldo-estoq.cod-estabel = "442" THEN  /*solic-318*/ 
                 ASSIGN var-ung-rs = var-ung-rs + saldo-estoq.qtidade-atu.
              ELSE
              IF saldo-estoq.cod-estabel = "421" OR saldo-estoq.cod-estabel = "411" THEN  /*solic-318*/ 
                  ASSIGN var-outros = var-outros + saldo-estoq.qtidade-atu.
              ELSE
               IF saldo-estoq.cod-estabel = "424" THEN
                   ASSIGN var-sbc = var-sbc + saldo-estoq.qtidade-atu.
               ELSE
                 ASSIGN var-outros = var-outros + saldo-estoq.qtidade-atu.

          ASSIGN qtde-jr = qtde-jr + saldo-estoq.qtidade-atu.
          
          FOR EACH bf-saldo-estoq FIELDS ( it-codigo   
                                            cod-refer   
                                            cod-estabel 
                                            cod-depos   
                                            lote        
                                            cod-localiz ) 
                                  WHERE bf-saldo-estoq.it-codigo = saldo-estoq.it-codigo AND
                                        bf-saldo-estoq.lote      = saldo-estoq.lote NO-LOCK USE-INDEX lote.

              FOR EACH  movto-estoq FIELDS (nro-docto
                                            dt-trans) WHERE
                            movto-estoq.it-codigo   = bf-saldo-estoq.it-codigo   AND
                            movto-estoq.cod-refer   = bf-saldo-estoq.cod-refer   AND
                            movto-estoq.cod-estabel = bf-saldo-estoq.cod-estabel AND
                            movto-estoq.cod-depos   = bf-saldo-estoq.cod-depos   AND
                            movto-estoq.lote        = bf-saldo-estoq.lote        AND
                            movto-estoq.cod-localiz = bf-saldo-estoq.cod-localiz AND
                            movto-estoq.esp-docto = 23 NO-LOCK USE-INDEX item-estab.

                                            
               
                    IF tem-transf-jr = ""  THEN
                        ASSIGN tem-transf-jr = movto-estoq.nro-docto
                               tem-data-jr   = movto-estoq.dt-trans.

                    IF movto-estoq.dt-trans > tem-data-jr THEN
                        ASSIGN tem-transf-jr = movto-estoq.nro-docto
                              tem-data-jr    = movto-estoq.dt-trans.

              END.
              

          END.
          
        
        END.

/* Procura no Terceiro */
        ASSIGN qtde-jr-t    = 0
               soma-terc-jr = 0
               qtde-jr-tr   = 0
               soma-terc-tr = 0
               tem-saldo-estoq = no.


       IF qtde-jr = 0 THEN DO:

           FIND first  saldo-estoq WHERE
                       saldo-estoq.lote      = pallet.nr-pallet AND
                       saldo-estoq.it-codigo = pallet.it-codigo /*AND
                       saldo-estoq.cod-refer = pallet.cod-refer */no-lock
                USE-INDEX lote
                  NO-ERROR.

           IF AVAIL saldo-estoq THEN DO:

                
               IF saldo-estoq.dt-vali-lote < dt-validade THEN
                  dt-validade = saldo-estoq.dt-vali-lote.
           END.
               
           ASSIGN soma-terc-jr = 0
                  soma-terc-tr = 0
                     /* tem-transf-jr = ""*/.                   



               FOR EACH   tt-lotes WHERE
                      tt-lotes.lote      = pallet.nr-pallet AND
                      tt-lotes.it-codigo = pallet.it-codigo AND
                      tt-lotes.saldo > 0 no-lock.
                      
                   FIND FIRST estabelec  WHERE estabelec.cod-emitente = tt-lotes.cod-emitente NO-LOCK NO-ERROR.
                   
                   IF AVAIL estabelec THEN DO:
                   
                       ASSIGN tem-transf-jr = tt-lotes.nro-docto.

                       
                          ASSIGN soma-terc-tr = soma-terc-tr + tt-lotes.saldo.
                      

                   END.

                   ELSE DO:
                   
                      
                      ASSIGN soma-terc-jr = soma-terc-jr + tt-lotes.saldo.                                              
                      

                      ASSIGN cod-emitente-jr = tt-lotes.cod-emitente
                             i-idx = 1.

                      DO WHILE i-idx < 10.

                          IF ((cod-emitente-terc [i-idx] = cod-emitente-jr) AND (cod-estabel-terc [i-idx] = tt-lotes.cod-estabel)) OR
                             cod-emitente-terc [i-idx] = 0 THEN DO:
                              ASSIGN cod-emitente-terc [i-idx] = cod-emitente-jr
                                     cod-estabel-terc [i-idx]  = tt-lotes.cod-estabel.
                              LEAVE.
                          END.

                          ASSIGN i-idx = i-idx + 1.

                      END.

                        
                      ASSIGN sld-emitente-terc [i-idx] = sld-emitente-terc [i-idx] + tt-lotes.saldo.
                           
                       
                   END. /*ELSE DO TERCEIROS*/
        
               END.  /*TT-LOTES*/

             

              
    
            ASSIGN var-terc     = var-terc +  soma-terc-jr 
                   var-transito = var-transito + soma-terc-tr.

       END. /*IF SEM SALDO VAI VER TERCEIRO*/
    

 

    
    END.   /*Pallet*/
    

END PROCEDURE.
return 'OK'.

/* fim do programa */



