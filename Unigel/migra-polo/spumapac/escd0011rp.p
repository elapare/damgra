/*****************************************************************************
* Empresa  : SPUMAPAC/UNIGEL
* Projeto  : Migracao Trinseo-Limao
* Arquivo  : escd0011.p
* Descricao: Exporta dados da tabela Totvs para o sistema SpumaWeb - Dispara exportacoes individuais
* Data     : 2016-10-05
******************************************************************************/    

{include/i-prgvrs.i ESCD0011RP 1.00.00.000}  /*** 010000 ***/

def buffer empresa for mgmulti.empresa.

DEF NEW GLOBAL SHARED VAR c-dir-spool-servid-exec AS CHAR NO-UNDO.

/****************** Defini‡Æo de Tabelas Tempor rias do Relat¢rio **********************/
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
  field c-cod-estabel-ini    like ped-venda.cod-estabel
  field c-cod-estabel-fim    like ped-venda.cod-estabel
  field data-ini             like ped-item.dt-entorig
  field data-fim             like ped-item.dt-entorig
  field c-tp-pedido-ini      like ped-venda.tp-pedido
  field c-tp-pedido-fim      like ped-venda.tp-pedido
  field c-nome-abrev-ini     like ped-venda.nome-abrev
  field c-nome-abrev-fim     like ped-venda.nome-abrev
  field i-cod-emitente-ini   like ped-venda.cod-emitente
  field i-cod-emitente-fim   like ped-venda.cod-emitente
  field c-it-codigo-ini      like ped-item.it-codigo
  field c-it-codigo-fim      like ped-item.it-codigo
  FIELD da-codrep-ini        LIKE repres.cod-rep
  FIELD da-codrep-fim        LIKE repres.cod-rep
  FIELD rs-mercado           AS INTEGER
  field da-canal-venda-ini   like nota-fiscal.cod-canal-venda
  field da-canal-venda-fim   like nota-fiscal.cod-canal-venda
  FIELD c-perc-atend         AS DEC
  field l-unig-com           as logical
  field l-simula-embarque    as logical
  field dt-embarque          AS DATE
  FIELD l-email              AS LOG
  FIELD c-grp-usuar          AS CHAR.

DEFINE TEMP-TABLE tt-digita NO-UNDO
  FIELD nome-abrev   LIKE emitente.nome-abrev
  FIELD cod-emitente LIKE emitente.cod-emitente
  INDEX ch-nome-abrev IS PRIMARY nome-abrev.

def temp-table tt-raw-digita
    field raw-digita as raw.

DEFINE TEMP-TABLE tt-param-aux      LIKE tt-param.

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
            cod-estabel.
                 
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
                    serie.

DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

{utp/utapi019.i}
{btb/btb912zb.i}
{utp/ut-glob.i}

/****************** Defini‡ao de buffer *********************/ 
DEF BUFFER bf-saldo-estoq  FOR saldo-estoq.
DEF BUFFER bf1-saldo-estoq FOR saldo-estoq.
def buffer bf-ped-venda    for ped-venda.
def buffer bf-ped-item     for ped-item.
def buffer if-ped-venda    for if-ped-venda.
def buffer b-ped-venda     for ped-venda.
def buffer bf-if-ped-venda for if-ped-venda.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 
def var c-est as char initial "422,424,434,432,702" no-undo.
def var i-estab as integer no-undo.

DEFINE VARIABLE nome-ab-rep-jr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE codrep-jr          AS INTEGER   NO-UNDO.
DEFINE VARIABLE merc-jr            AS CHAR      NO-UNDO.
DEFINE VARIABLE saldo-atend        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE tem-transf-jr      AS CHARACTER NO-UNDO.
DEFINE VARIABLE nr-pedcli-jr       AS CHARACTER NO-UNDO.
DEFINE VARIABLE linha-jr           AS CHARACTER NO-UNDO.
DEFINE VARIABLE tem-data-jr        AS DATE      NO-UNDO.
DEFINE VARIABLE saldo-pedido       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE c-nome-plan-ped    AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nome-plan-fat    AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nome-plan-mes    AS CHARACTER NO-UNDO.

/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

def var de-qt-atendida-tt-002 like ped-item.qt-atendida no-undo.
def var de-qt-pedida-tt-001   like ped-item.qt-pedida   no-undo.
def var qt-atendida-jr        like ped-item.qt-atendida no-undo.
def var qt-pedida-jr          like ped-item.qt-pedida   no-undo.
DEF VAR d-perc-atend          AS DEC                    NO-UNDO.
def var tem-saldo-estoq       as logical                no-undo.
DEF VAR d-saldo-terc-sp       AS DECIMAL                NO-UNDO.
DEF VAR d-saldo-terc-rs       AS DECIMAL                NO-UNDO.
DEF VAR cod-emitente-terc     AS INTEGER EXTENT 10      NO-UNDO.

DEF VARIABLE sld-emitente-terc  AS DECIMAL EXTENT 10 NO-UNDO.
DEF VARIABLE cod-emitente-jr    AS INTEGER           NO-UNDO.
DEF VARIABLE i-idx              AS INTEGER           NO-UNDO.
def buffer  b-movto-estoq for  movto-estoq.
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
def var i-ext                AS INT    NO-UNDO.
DEF VAR ext-jr               AS CHAR   NO-UNDO.
DEF VAR c-obs-ped            AS CHAR   NO-UNDO.
DEF VAR c-obs-pallet         AS CHAR   NO-UNDO.
DEF VAR dt-validade          AS DATE   NO-UNDO.
DEF VAR dt-producao          AS DATE   NO-UNDO.
DEF VAR c-arq                AS CHAR   NO-UNDO.
DEF VAR c-arq-anexo          AS CHAR   NO-UNDO.   
DEF VAR l-cria               AS LOG    NO-UNDO.
DEF VAR i-pagina             AS INT    INIT 1 NO-UNDO.
DEF VAR i-linha              AS INT    NO-UNDO.
DEF VAR i-linhax             AS INT    NO-UNDO.
DEF VAR c-coluna             AS char   NO-UNDO.

/* para executar o Excel */

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 
DEFINE VARIABLE pedido-cliente  AS CHARACTER  NO-UNDO.
/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 
DEFINE VARIABLE ped-atend-jr       AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE saldo-jr           AS DECIMAL                   NO-UNDO.
/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

DEF VAR var-qped AS DEC FORMAT ">>>,>>>.99" LABEL "Qt.Pedida".

/**********Tabela temporaria************/

define temp-table tt-fat NO-UNDO
    FIELD var-QTDBOB      AS INTEGER FORMAT ">>>>9"         LABEL "QT.BOB."
    FIELD var-QTDPEDIDO     AS INTEGER FORMAT ">>>>,>>>,>>9.9999"         LABEL "QTDPEDIDO"
    FIELD var-Larg         AS INTEGER FORMAT ">,>>9"         LABEL "Larg"
    FIELD var-diin         AS INTEGER FORMAT ">,>>9"         LABEL "DIN"
    FIELD var-diex         AS INTEGER FORMAT ">,>>9"         LABEL "DIEX"
    FIELD var-pedcli       AS CHAR    FORMAT "x(14)"         LABEL "NR.PED.CLIENTE"
    FIELD nr-ext           AS CHAR    FORMAT "x(12)"         LABEL "NR.EXT"
    FIELD var-ung-rs       AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Vga"
    FIELD var-mtn          AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Mtn"
    FIELD var-ung-sbc      AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Gru"
    FIELD var-sbc          AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Sbc"
    FIELD var-outros       AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Out"
    FIELD var-terc         AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Terc"
    FIELD var-transito     AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Trans"
    FIELD nome-merc        AS CHARACTER FORMAT "X(7)"        LABEL "Mercado"
    FIELD dep-terceiro     AS CHAR FORMAT "x12)"
    field tem-transf       AS CHAR FORMAT "x(3)"
    field cod-estabel      like ped-venda.cod-estabel
    field cod-estabel-fat  like ped-venda.cod-estabel
    field dt-entrega       like ped-item.dt-entrega
    field dt-implant       like ped-venda.dt-implant
    field nr-pedcli        like ped-venda.nr-pedcli
    FIELD nr-sequencia     LIKE ped-item.nr-sequencia
    field tp-pedido        like ped-venda.tp-pedido
    FIELD cod-canal-venda  LIKE ped-venda.cod-canal-venda
    field nome-abrev       like ped-venda.nome-abrev
    field it-codigo        like ped-item.it-codigo
    FIELD nr-pedido        LIKE ped-venda.nr-pedido
    FIELD qt-pedida        LIKE ped-item.qt-pedida
    FIELD qt-atendida      LIKE ped-item.qt-atendida
    FIELD vl-preuni        LIKE ped-item.vl-preuni
    FIELD mo-codigo        LIKE ped-venda.mo-codigo
    FIELD nome-ab-rep-jr   AS CHAR FORMAT "x(12)" LABEL "Represent."
    FIELD nome-transp      AS CHAR FORMAT "x(12)"    LABEL "Transport." 
    FIELD nome-transp-red  AS CHAR FORMAT "x(12)"    LABEL "Transp.Red" 
    FIELD obs-pallet       AS CHAR FORMAT "x(100)"    LABEL "Obs-Pallet." 
    FIELD dt-val           AS DATE FORMAT "99/99/9999" LABEL "Dt.validade"
    FIELD dt-producao      AS DATE FORMAT "99/99/9999" LABEL "Dt.Produ‡Æo"
    FIELD dt-faturamento   AS DATE FORMAT "99/99/9999"
    FIELD dt-entrega-cli   AS DATE FORMAT "99/99/9999"
    FIELD liber-fat        AS CHAR
    FIELD obs-fat          AS CHAR
    FIELD preco-ex-imp     AS DEC
    field preco-icms       AS DEC
    FIELD total-ped        AS DEC
    FIELD cif-fob          AS CHAR 
    FIELD cond-pagto       AS CHAR 
    field perc-enc-fin     as dec
    field liber-financ     as char
    FIELD sld-emitente     AS DECIMAL EXTENT 10
    FIELD cod-prod-cliente AS CHAR
    FIELD unigel-com       AS CHAR
    FIELD perc-desc        AS DEC
    FIELD preco-sem-desc   AS DEC
    FIELD embarque         AS CHAR
    FIELD desc-embarque    AS CHAR
    FIELD tipo-info        AS CHAR
    INDEX ch-tipo-info IS PRIMARY tipo-info.

DEF TEMP-TABLE tt-imp-nota
    FIELD cod-estabel    LIKE it-nota-fisc.cod-estabel                                                                                 
    FIELD nome-ab-cli    LIKE it-nota-fisc.nome-ab-cli                                                                                 
    FIELD cd-emitente    LIKE it-nota-fisc.cd-emitente
    FIELD tp-pedido      LIKE ped-venda.tp-pedido                                                                                      
    FIELD nr-pedcli      LIKE it-nota-fisc.nr-pedcli                                                                                   
    FIELD nr-pedido      LIKE it-nota-fisc.nr-pedido                                                                                   
    FIELD nr-seq-ped     LIKE it-nota-fisc.nr-seq-ped                                                                                  
    FIELD it-codigo      LIKE it-nota-fisc.it-codigo                                                                                   
    FIELD pedido-cliente AS CHARACTER                                                                                       
    FIELD nr-nota-fis    LIKE it-nota-fisc.nr-nota-fis                                                                                 
    FIELD vl-preuni      LIKE it-nota-fisc.vl-preuni                                                                                   
    FIELD qt-faturada    LIKE it-nota-fisc.qt-faturada[1]                                                                              
    FIELD dt-emis-nota   LIKE it-nota-fisc.dt-emis-nota                                                                                
    FIELD codrep-jr      LIKE codrep-jr                                                                                                
    FIELD nome-ab-rep-jr LIKE nome-ab-rep-jr                                                                                           
    FIELD var-Larg       LIKE var-Larg                                                                                                 
    FIELD saldo-jr       AS DECIMAL
    FIELD observacoes    LIKE ped-venda.observacoes
    FIELD dt-entrega-cli AS CHARACTER.

/****************** Defini‡ao Variaveis Locais ***************************************/ 

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

DEFINE VARIABLE var-atendida AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
DEFINE VARIABLE var-pedida AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
DEFINE VARIABLE var-emb-merc   AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
DEFINE VARIABLE var-emb-merc-t AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
DEFINE VARIABLE VAR-branco   AS CHARACTER  INITIAL "" NO-UNDO.

def var l-imprime    as logical no-undo.
DEF VAR l-tt-digita  AS LOGICAL NO-UNDO.

/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form tt-fat.cod-estabel label "Estabelecimento" format "x(3)" at 001
     with down width 132 side-labels no-box stream-io frame f-relat-01-132.

form tt-fat.dt-entrega label "Dt Embarque" format "99/99/9999" at 001
     with down width 132 side-labels no-box stream-io frame f-relat-02-132.

form tt-fat.tp-pedido       column-label "TP"          format "x(2)" at 001
     tt-fat.cod-canal-venda COLUMN-LABEL "CV"          FORMAT ">9" AT 04
     tt-fat.nr-pedido       column-label "Pedido"      format ">>>>>>>9" at 007
     tt-fat.nome-abrev      column-label "Cliente"     format "x(12)" at 016
     tt-fat.it-codigo       column-label "Item"        format "x(16)" at 029
     tt-fat.qt-pedida       column-label "Qt Pedida"   format ">>>>,>>9.99" at 046
     tt-fat.qt-atendida     column-label "Qt Faturada" format ">>>>,>>9.99" at 060
     /*tt-fat.dt-entrega column-label "Dt Fatur" format "99/99/9999" at 074*/
     with down width 184 no-box stream-io frame f-relat-09-132.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.

define new shared stream str-rp.

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
   create tt-digita.
   raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.


/*****************************************************************************************************/


{spumapac\btiw_def1.i}


/* Processa alteracoes de credito (aprovacao/reprovacao) vindas da spumaweb */

DOS SILENT "M:\edi\spumapac\spumapac_in.bat" > "M:\edi\spumapac\recepcao.log".

RUN spumapac\btiw014.p.


/* Processa exportacao de dados */

OUTPUT TO VALUE (c-file-log)
    /*CONVERT TARGET "iso8859-1"*/.


PUT "*** ATUALIZACAO DE DADOS SPUMAWEB ***" FORMAT "X(50)"
        SKIP.


PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " > Iniciando geracao de arquivos de atualizacao" FORMAT "X(50)"
        SKIP.
   
PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando exp_nota_fiscal..." FORMAT "X(50)"
    SKIP.
RUN spumapac\exp_nota_fiscal.p.


PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando exp_pedido..." FORMAT "X(50)"
    SKIP.
RUN spumapac\exp_pedido.p.


PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando exp_tit_acr..." FORMAT "X(50)"
    SKIP.
RUN spumapac\exp_tit_acr.p.


PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando exp_it_pre_fat..." FORMAT "X(50)"
    SKIP.
RUN spumapac\exp_it_pre_fat.p.


PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando exp_clientes..." FORMAT "X(50)"
    SKIP.
RUN spumapac\exp_clientes.p.


PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando exp_transporte..." FORMAT "X(50)"
    SKIP.
RUN spumapac\exp_transporte.p.


PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando exp_esp_doc..." FORMAT "X(50)"
    SKIP.
RUN spumapac\exp_esp_doc.p.


PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando exp_fam_comerc..." FORMAT "X(50)"
    SKIP.
RUN spumapac\exp_fam_comerc.p.


PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando exp_portador..." FORMAT "X(50)"
    SKIP.
RUN spumapac\exp_portador.p.


PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando exp_unid_feder..." FORMAT "X(50)"
    SKIP.
RUN spumapac\exp_unid_feder.p.


PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando exp_cond_pagto..." FORMAT "X(50)"
    SKIP.
RUN spumapac\exp_cond_pagto.p.


PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando exp_itens..." FORMAT "X(50)"
    SKIP.
RUN spumapac\exp_itens.p.


PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando exp_natur_oper..." FORMAT "X(50)"
    SKIP.
RUN spumapac\exp_natur_oper.p.


PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando exp_repres..." FORMAT "X(50)"
    SKIP.
RUN spumapac\exp_repres.p.

/*
PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando exp_preco_item..." FORMAT "X(50)"
    SKIP.
RUN spumapac\exp_preco_item.p.
*/

/*
PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando exp_tabela_preco..." FORMAT "X(50)"
    SKIP.
RUN spumapac\exp_tabela_preco.p.
*/




PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando atu_docum_est..." FORMAT "X(50)"
    SKIP.
RUN spumapac\atu_docum_est.p.

PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando atu_dupli_apagar..." FORMAT "X(50)"
    SKIP.
RUN spumapac\atu_dupli_apagar.p.

PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando atu_fornec_conta..." FORMAT "X(50)"
    SKIP.
RUN spumapac\atu_fornec_conta.p.

PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando atu_fornec_contato..." FORMAT "X(50)"
    SKIP.
RUN spumapac\atu_fornec_contato.p.

PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando atu_item_doc_est..." FORMAT "X(50)"
    SKIP.
RUN spumapac\atu_item_doc_est.p.

PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando atu_natur_oper..." FORMAT "X(50)"
    SKIP.
RUN spumapac\atu_natur_oper.p.

PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando atu_serie..." FORMAT "X(50)"
    SKIP.
RUN spumapac\atu_serie.p.

PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando atu_tit_ap..." FORMAT "X(50)"
    SKIP.
RUN spumapac\atu_tit_ap.p.

PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando atu_bord_ap..." FORMAT "X(50)"
    SKIP.
RUN spumapac\atu_bord_ap.p.

PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando atu_bord_ap_item..." FORMAT "X(50)"
    SKIP.
RUN spumapac\atu_bord_ap_item.p.

PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando atu_rh_pessoa_fisic..." FORMAT "X(50)"
    SKIP.
RUN spumapac\atu_rh_pessoa_fisic.p.

PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando atu_funcionarios..." FORMAT "X(50)"
    SKIP.
RUN spumapac\atu_funcionarios.p.

PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando atu_clientes1..." FORMAT "X(50)"
    SKIP.
RUN spumapac\atu_clientes1.p.

PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando atu_item_narrativa..." FORMAT "X(50)"
    SKIP.
RUN spumapac\atu_item_narrativa.p.

PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " - Iniciando atu_saldo_estoq..." FORMAT "X(50)"
    SKIP.
RUN spumapac\atu_saldo_estoq.p.


PUT NOW FORMAT "99/99/9999 HH:MM:SS.SSS"
    " > Encerrando geracao de arquivos de atualizacao" FORMAT "X(50)"
        SKIP.
         
  
  
  
run pi-recria-pedido.
	  
  
OUTPUT CLOSE.
	

	

DOS SILENT "M:\edi\spumapac\spumapac.bat" > "M:\edi\spumapac\transmissao.log".



    
RETURN 'OK'.




procedure pi-recria-pedido:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE c-cod_servid_exec AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dt-aux     AS DATE        NO-UNDO.
    DEFINE VARIABLE i-cont-aux AS INTEGER     NO-UNDO.
	DEFINE VARIABLE hr-aux     AS CHARACTER   NO-UNDO.

    create tt-param-aux.
    BUFFER-COPY tt-param TO tt-param-aux.
	
    assign /*tt-param-aux.usuario   = tt-param.usuario
           tt-param-aux.destino   = 4
           tt-param-aux.data-exec = TODAY
           tt-param-aux.hora-exec = tt-param-aux.hora-exec
		   */
           tt-param-aux.arquivo   = "ESCD0011.LST".

/*
    ASSIGN dt-aux = TODAY + 1.

    ASSIGN tt-param-aux.data-ini = DATE("01/" + string(MONTH(dt-aux),"99") + "/" + STRING(year(dt-aux),"9999")).

    IF MONTH(tt-param-aux.data-ini) = 12 THEN
        ASSIGN tt-param-aux.data-fim = DATE("31/12/" + STRING(year(tt-param-aux.data-ini),"9999")).
    ELSE
        ASSIGN tt-param-aux.data-fim = DATE("01/" + string(MONTH(tt-param-aux.data-ini) + 1,"99") + "/" + STRING(year(tt-param-aux.data-ini),"9999")) - 1.
*/
		
		/*
    IF i-num-ped-exec-rpw = 0 THEN DO:
        FIND FIRST usuar_mestre
             WHERE usuar_mestre.cod_usuario = tt-param.usuario NO-LOCK NO-ERROR.

        IF AVAIL usuar_mestre AND usuar_mestre.cod_servid_exec <> "" THEN
            ASSIGN c-cod_servid_exec = usuar_mestre.cod_servid_exec.
        ELSE
            ASSIGN c-cod_servid_exec = "rpw1".
    END.
    ELSE DO:
        FIND FIRST ped_exec
             WHERE ped_exec.num_ped_exec = i-num-ped-exec-rpw NO-LOCK NO-ERROR.
        IF AVAIL ped_exec THEN
            ASSIGN c-cod_servid_exec = ped_exec.cod_servid_exec.
        ELSE
            ASSIGN c-cod_servid_exec = "".
    END.
		*/
		
	ASSIGN c-cod_servid_exec = "rpw3" /* "rpw7" */.

	if string(TIME, "HH:MM:SS") > "22:00:00" THEN
	DO:
		ASSIGN dt-aux = TODAY + 1
				hr-aux = "070000".
	END.
	ELSE
	DO:
		ASSIGN dt-aux = TODAY
				hr-aux = replace(string(TIME + 300, "HH:MM:SS"),":","").
	END.

	
    create tt_param_segur.
    assign tt_param_segur.tta_num_vers_integr_api      = 3
           tt_param_segur.tta_cod_aplicat_dtsul_corren = "MAN"
           tt_param_segur.tta_cod_empres_usuar         = v_cod_empres_usuar
           tt_param_segur.tta_cod_grp_usuar_lst        = v_cod_grp_usuar_lst
           tt_param_segur.tta_cod_idiom_usuar          = v_cod_idiom_usuar
           tt_param_segur.tta_cod_modul_dtsul_corren   = "PDP"
           tt_param_segur.tta_cod_pais_empres_usuar    = "BRA"
           tt_param_segur.tta_cod_usuar_corren         = v_cod_usuar_corren
           tt_param_segur.tta_cod_usuar_corren_criptog = v_cod_usuar_corren_criptog.

    create tt_ped_exec.
    assign tt_ped_exec.tta_num_seq                     = 1
           tt_ped_exec.tta_cod_usuario                 = v_cod_usuar_corren
           tt_ped_exec.tta_cod_prog_dtsul              = "ESCD0011"
           tt_ped_exec.tta_cod_prog_dtsul_rp           = "spumapac/escd0011rp.p"
           tt_ped_exec.tta_cod_release_prog_dtsul      = "1.00.00.001"
           tt_ped_exec.tta_dat_exec_ped_exec           = dt-aux /*TODAY*/
           tt_ped_exec.tta_hra_exec_ped_exec           = hr-aux /*replace(string(TIME + 600, "HH:MM:SS"),":","")*/ /*replace(string(tt-param-aux.hora-exec, "HH:MM:SS"),":","")*/
           tt_ped_exec.tta_cod_servid_exec             = c-cod_servid_exec
           tt_ped_exec.tta_cdn_estil_dwb               = 97.

    create tt_ped_exec_param.
    assign tt_ped_exec_param.tta_num_seq               = 1
           tt_ped_exec_param.tta_cod_dwb_file          = "escd0011rp.p"
           tt_ped_exec_param.tta_cod_dwb_output        = "Arquivo"
           tt_ped_exec_param.tta_nom_dwb_printer       = tt-param-aux.arquivo
           tt_ped_exec_param.tta_cod_dwb_print_layout  = tt-param-aux.arquivo.

    raw-transfer tt-param-aux to tt_ped_exec_param.tta_raw_param_ped_exec.

    assign i-cont-aux = 1.
    for each tt-raw-digita no-lock:
        create tt_ped_exec_param_aux.
        assign tt_ped_exec_param_aux.tta_num_seq            = 1
               tt_ped_exec_param_aux.tta_raw_param_ped_exec = tt-raw-digita.raw-digita
               tt_ped_exec_param_aux.tta_num_dwb_order      = i-cont-aux.
        assign i-cont-aux = i-cont-aux + 1.
    end.


	
    run btb/btb912zb.p (input-output table tt_param_segur,
                        input-output table tt_ped_exec,
                        input table tt_ped_exec_param,
                        input table tt_ped_exec_param_aux,
                        input table tt_ped_exec_sel).

    FIND FIRST tt_ped_exec.	
	
    PUT tt_ped_exec.ttv_num_msg_erro 
        tt_ped_exec.ttv_cod_msg_parameters  FORMAT "X(100)".
		

end procedure.





/* fim do programa */