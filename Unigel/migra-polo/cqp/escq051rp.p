
/***************************************************************************
 **Programa.: escq051rp.p 
 **Autor....: Amgra - JosÇ Roberto
 **Objetivo.: Laudo de Qualidade Autom†tico
 **Data.....: 26/05/2008
 **
 ***************************************************************************/
def buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "escq051RP".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

/****************** Definiá∆o de Tabelas Tempor†rias do Relat¢rio **********************/

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
    field c-cod-estabel-ini    like ped-venda.cod-estabel
    FIELD c-serie              AS CHAR 
    field c-cdd-embarq-ini    AS INTEGER 
    field c-cdd-embarq-fim    AS INTEGER 
    FIELD pesq-jr              AS INT 
    FIELD tipo-geracao         AS INT 
    FIELD i-modelo             AS INT
    field tb-v-temp            AS LOG   .

DEFINE TEMP-TABLE tt-digita no-undo
    FIELD nome-abrev           LIKE emitente.nome-abrev LABEL "Cliente"
    FIELD nr-nota-fisc         LIKE nota-fiscal.nr-nota-fis LABEL "N.Fiscal"
    FIELD serie                LIKE nota-fiscal.serie   LABEL "SÇrie"
    FIELD it-codigo            AS CHAR FORMAT "x(16)"   LABEL "Filme"
    FIELD nr-pedido            AS INT  FORMAT ">>>>>>9" LABEL "Pedido"
    FIELD nr-sequencia         LIKE ped-item.nr-sequencia
    FIELD tipo-modelo          AS INT 
    INDEX chave IS PRIMARY UNIQUE nome-abrev  
                                  nr-nota-fisc
                                  it-codigo   
                                  nr-sequencia 
                                  nr-pedido.

DEFINE TEMP-TABLE tt-digita2 no-undo
    FIELD nr-pedido            AS INT  FORMAT ">>>>>>9" LABEL "Pedido"
    FIELD nr-pallet            AS CHAR FORMAT "x(10)" LABEL "Nr.Palete"
    FIELD dt-trans             AS DATE FORMAT "99/99/9999" LABEL "Data Palete"
    FIELD it-codigo            AS CHAR FORMAT "x(16)" LABEL "Filme"
    FIELD sequencia            AS INT  FORMAT "zz9" LABEL "Seq.Pedido"
    FIELD nr-bobinas           AS INT  FORMAT ">>>>>>9" LABEL "Qtd.Bobs"
    FIELD peso-liq             AS DECIMAL LABEL "Peso Liquido" FORMAT "zzzzzzzz9.99"
    FIELD peso-bru             AS DECIMAL LABEL "Peso Bruto" FORMAT "zzzzzzzz9.99"
    FIELD nr-nota-fis          LIKE movto-estoq.nro-docto
    FIELD serie                LIKE nota-fiscal.serie
    INDEX chave IS PRIMARY UNIQUE nr-nota-fis
                                  it-codigo
                                  nr-pallet.

DEFINE TEMP-TABLE tt-notas no-undo
    FIELD it-codigo            AS CHAR FORMAT "x(16)" 
    FIELD nr-nota-fis          LIKE movto-estoq.nro-docto
    FIELD nr-pedido            AS INT
    FIELD nr-seq-ped           AS INT
    FIELD nr-sequencia         AS INT
    FIELD serie                AS CHAR 

    INDEX chave IS PRIMARY UNIQUE nr-nota-fis
                                  it-codigo
                                  nr-pedido
                                  nr-sequencia .

DEFINE TEMP-TABLE tt-bobinas
    FIELD ttbob-lote           AS CHAR 
    FIELD ttbob-linha          AS integer
    FIELD ttbob-it-codigo      AS CHAR
    FIELD ttbob-nr-ord-produ   AS INTEGER
    FIELD ttbob-dt-trans       AS DATE
    INDEX ch-tt-bobinas IS PRIMARY UNIQUE   ttbob-lote
                                            ttbob-linha. 
                                            
DEFINE TEMP-TABLE tt-pedidos
    FIELD ttped-nr-pedido      AS INT 
    FIELD ttped-larg           AS INT
    FIELD ttped-diex           AS INT
    FIELD ttped-diin           AS INT
    FIELD ttped-qtdbob         AS INT
    FIELD ttped-nr-nota-fis    LIKE movto-estoq.nro-docto
    FIELD ttped-pedcli         AS CHAR FORMAT "x(15)"
    FIELD ttped-zint           AS CHAR
    FIELD ttped-prodclie       AS CHAR
    FIELD ttped-ems-pedcli     AS CHAR
    INDEX ch-tt-pedidos IS PRIMARY UNIQUE   ttped-nr-pedido.
                                            
                                            
DEFINE TEMP-TABLE tt-pedido-2
    FIELD nr-pedido            AS INT 
    FIELD pedcli               AS CHAR 
    FIELD prodclie             AS CHAR
    INDEX ch-tt-pedido-2 IS PRIMARY UNIQUE nr-pedido.                                         




DEFINE TEMP-TABLE tt-bobinas2
    FIELD ttbob2-lote           AS CHAR 
    FIELD ttbob2-linha          AS integer
    FIELD ttbob2-it-codigo      AS CHAR
    FIELD ttbob2-nr-ord-produ   AS INTEGER
    FIELD ttbob2-dt-trans       AS DATE
    INDEX ch-tt-bobinas2 IS PRIMARY UNIQUE   ttbob2-lote
                                             ttbob2-linha
   index ch-ord-prod   ttbob2-nr-ord-produ  .                                              . 


DEFINE TEMP-TABLE tt-analises
    FIELD ttana-cod-exame       LIKE pol-res-fic-cq-leitura.cod-exame
    FIELD ttana-cod-comp        LIKE pol-res-fic-cq-leitura.cod-comp
    FIELD ttana-it-codigo       LIKE pol-res-fic-cq-leitura.it-codigo
    FIELD ttana-result          AS DECIMAL 
    FIELD ttana-qtd-result      AS INTEGER
    FIELD ttana-minimo          AS DECIMAL
    FIELD ttana-maximo          AS DECIMAL
    FIELD ttana-esp-min         AS DECIMAL
    FIELD ttana-esp-max         AS DECIMAL
    FIELD ttana-amostras        AS DECIMAL EXTENT 9000
    INDEX ch-tt-analises IS PRIMARY UNIQUE ttana-cod-exame 
                                           ttana-cod-comp.



DEFINE TEMP-TABLE tt-bob-prod
    FIELD lote           AS CHAR 
    FIELD dt-prod        AS DATE
    INDEX ch-tt-bobinas IS PRIMARY UNIQUE lote.


DEFINE TEMP-TABLE tt-analises-mr
    FIELD cod-exame       as   int
    FIELD cod-comp        as   int 
    FIELD nr-lote         AS   char  
    FIELD qtd-result      AS integer 
    FIELD soma-result     AS decimal
    FIELD media-result    AS decimal
    INDEX ch-tt-analises-mr IS PRIMARY UNIQUE cod-exame
                                              cod-comp
                                              nr-lote.

DEFINE TEMP-TABLE tt-am-cq-laudo        LIKE am-cq-laudo
    FIELD serie           AS CHAR.

DEFINE TEMP-TABLE tt-am-cq-result-laudo LIKE am-cq-result-laudo
    FIELD cod-an-cli AS CHAR.



{utp/utapi009.i}
    /*------------ Envio e-mail -------------*/

 
    define temp-table tt-envio-w
        field versao-integracao   as integer format ">>9"
        field servidor            as char
        field porta               as integer init 0
        field exchange            as logical init no
        field destino             as char
        field copia               as char
        field remetente           as char
        field assunto             as char
        field mensagem            as char
        field arq-anexo           as char
        field importancia         as integer init 0
        field log-enviada         as logical init no
        field log-lida            as logical init no
        field acomp               as logical init yes.


    /*---------------------------------------*/




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
/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 


/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 

def new shared var c-cod-estabel-ini like ped-venda.cod-estabel format "x(3)" initial "" no-undo.
def new shared var c-nr-pedcli-ini like ped-venda.nr-pedcli format "x(12)" initial "" no-undo.
def new shared var c-nr-pedcli-fim like ped-venda.nr-pedcli format "x(12)" initial "ZZZZZZZZZZZZ" no-undo.
def new shared var c-nr-pallet-ini like pallet.nr-pallet format "x(12)" initial "" no-undo.
def new shared var c-nr-pallet-fim like pallet.nr-pallet format "x(12)" initial "ZZZZZZZZZZZZ" no-undo.
def new shared var c-nr-sequencia-ini like ped-item.nr-sequencia format ">9" initial 1 no-undo.


/****************** Definiáao de Vari†veis p/ Campos Virtuais do Relat¢rio *******************/ 
DEFINE VARIABLE lote-mr       AS CHAR                      NO-UNDO.
DEFINE VARIABLE lote-mr1      AS CHAR                      NO-UNDO.
DEFINE VARIABLE lote-mr2      AS CHAR                      NO-UNDO.
DEFINE VARIABLE lote-mr3      AS CHAR                      NO-UNDO.
DEFINE VARIABLE bobina        AS CHAR                      NO-UNDO.
DEFINE VARIABLE item-mr       LIKE ped-item.it-codigo      NO-UNDO.
DEFINE VARIABLE item-mr1      LIKE ped-item.it-codigo      NO-UNDO.
DEFINE VARIABLE item-mr2      LIKE ped-item.it-codigo      NO-UNDO.
DEFINE VARIABLE item-mr3      LIKE ped-item.it-codigo      NO-UNDO.
DEFINE VARIABLE espes-jr1     AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE larg-jr1      AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE comp-jr1      AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE peso-jr1      AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE tot-peso-jr1  AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE tot-fator-jr1 AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE esp-min-jr    AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE esp-max-jr    AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE gra-min-jr    AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE gra-max-jr    AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE tot-min-jr    AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE tot-max-jr    AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE densidade-jr  AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE fm-codigo-jr  LIKE item.fm-codigo          NO-UNDO.
DEFINE VARIABLE diex-jr1      AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE lote-req      LIKE movto-estoq.lote        NO-UNDO.
DEFINE VARIABLE cmkt-req      LIKE movto-estoq.it-codigo   NO-UNDO.
DEFINE VARIABLE ordpro-req    LIKE movto-estoq.nr-ord-prod NO-UNDO.
DEFINE VARIABLE numseq-req    LIKE movto-estoq.num-sequen  NO-UNDO.
DEFINE VARIABLE tp-pedido-jr  LIKE ped-venda.tp-pedido     NO-UNDO.
DEFINE VARIABLE nome-abrev-jr LIKE ped-venda.nome-abrev    NO-UNDO.
DEFINE VARIABLE num-pedido    AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE flag-erro     AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE esp-min       AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE esp-max       AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE esp-med       AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE esp-jr        AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE soma-esp      AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE qtd-esp       AS INTEGER                   NO-UNDO.
DEFINE VARIABLE fim-mr        AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE op-rast       AS INTEGER                   NO-UNDO.
DEFINE VARIABLE lote-rast     AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE data-rast     AS DATE                      NO-UNDO.
DEFINE VARIABLE lar-min       AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE lar-max       AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE lar-med       AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE soma-lar      AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE qtd-lar       AS INTEGER                   NO-UNDO.
DEFINE VARIABLE die-min       AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE die-max       AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE die-med       AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE soma-die      AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE qtd-die       AS INTEGER                   NO-UNDO.
DEFINE VARIABLE pesq-jr       AS INT                       NO-UNDO.
DEFINE VARIABLE it-codigo-ped AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE diin-ped      AS INTEGER                   NO-UNDO.
DEFINE VARIABLE diex-ped      AS INTEGER                   NO-UNDO.
DEFINE VARIABLE larg-ped      AS INTEGER                   NO-UNDO.
DEFINE VARIABLE pedcli-ped    AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE tipo-result   AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE descricao-comp AS CHARACTER                NO-UNDO.
DEFINE VARIABLE minimo-jr     AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE maximo-jr     AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE RESULT-jr     AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE media-result-jr  AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE media-result-jr-x AS CHARACTER                NO-UNDO.
DEFINE VARIABLE qtd-bobinas   AS INTEGER                   NO-UNDO.
DEFINE VARIABLE soma-peso     AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE unidade-jr    AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE decimais-jr   AS INTEGER                   NO-UNDO.
DEFINE VARIABLE metodo-jr     AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE valor-tipico-jr AS DECIMAL                 NO-UNDO.
DEFINE VARIABLE data-jr       AS DATE                      NO-UNDO.
DEFINE VARIABLE coluna-excel  AS CHARACTER  FORMAT "x(20)" NO-UNDO INITIAL "CDEFGHICDEFGHI".
DEFINE VARIABLE soma-q-plt    AS INTEGER                   NO-UNDO.
DEFINE VARIABLE cod-estabel-jr AS CHARACTER  FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE tem-minimo    AS CHARACTER   FORMAT "x(1)" NO-UNDO.
DEFINE VARIABLE tem-maximo    AS CHARACTER   FORMAT "x(1)" NO-UNDO.

DEFINE VARIABLE nrnf-ant      AS CHARACTER  FORMAT "X(16)" INITIAL "" NO-UNDO.
DEFINE VARIABLE it-codigo-ant AS CHARACTER  FORMAT "X(16)" INITIAL "" NO-UNDO.
DEFINE VARIABLE pedcli-ant    AS CHARACTER  FORMAT "X(15)" NO-UNDO.
DEFINE VARIABLE pedido-x1     AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE pedido-x2     AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE larg-pol-x    AS CHARACTER  FORMAT "x(2000)" NO-UNDO.
DEFINE VARIABLE larg-pol-x1   AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE larg-x1       AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE larg-x2       AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE diex-x1       AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE diex-x2       AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE diin-x1       AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE diin-x2       AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE nrnf-x1       AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE pedcli-x1     AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE pedcli-x2     AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE qtdbob-x1     AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE qtdbob-x2     AS CHARACTER  FORMAT "x(90)" NO-UNDO.

DEFINE VARIABLE pallet-x1     AS CHARACTER  FORMAT "x(180)" NO-UNDO.
DEFINE VARIABLE pallet-x2     AS CHARACTER  FORMAT "x(180)" NO-UNDO.
DEFINE VARIABLE pallet-x3     AS CHARACTER  FORMAT "x(180)" NO-UNDO.
DEFINE VARIABLE pallet-x4     AS CHARACTER  FORMAT "x(180)" NO-UNDO.
DEFINE VARIABLE pallet-x5     AS CHARACTER  FORMAT "x(180)" NO-UNDO.
DEFINE VARIABLE pallet-x6     AS CHARACTER  FORMAT "x(180)" NO-UNDO.
DEFINE VARIABLE pallet-x7     AS CHARACTER  FORMAT "x(180)" NO-UNDO.
DEFINE VARIABLE pallet-x8     AS CHARACTER  FORMAT "x(180)" NO-UNDO.
DEFINE VARIABLE pallet-x9     AS CHARACTER  FORMAT "x(180)" NO-UNDO.
DEFINE VARIABLE pallet-x10    AS CHARACTER  FORMAT "x(180)" NO-UNDO.
DEFINE VARIABLE pallet-x11    AS CHARACTER  FORMAT "x(180)" NO-UNDO.
DEFINE VARIABLE pallet-x12    AS CHARACTER  FORMAT "x(180)" NO-UNDO.
DEFINE VARIABLE pallet-x13    AS CHARACTER  FORMAT "x(180)" NO-UNDO.

DEFINE VARIABLE conta-ped       AS INTEGER                 NO-UNDO.
DEFINE VARIABLE nome-estabel-jr AS CHARACTER               NO-UNDO.
DEFINE VARIABLE sai-laudo       AS logical                 NO-UNDO.

DEFINE VARIABLE nr-laudo-jr     AS INTEGER    NO-UNDO.
DEFINE VARIABLE gravou-laudo    AS INTEGER    NO-UNDO.
DEFINE VARIABLE cod-emitente-jr AS INTEGER    NO-UNDO.
DEFINE VARIABLE zint-jr         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prodclie-jr     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prodclie-jrx    AS CHARACTER  NO-UNDO.

DEFINE VARIABLE c-email-responsavel  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-nome-responsavel   AS CHARACTER  NO-UNDO.

DEFINE VARIABLE c-nr-pedido-1   AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-nr-sequencia-1 AS INTEGER   NO-UNDO.
DEFINE VARIABLE media-jr2       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE media-trab      AS DECIMAL    NO-UNDO.

DEFINE VARIABLE obs-ex  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE DESC-ex AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i       AS INTEGER    NO-UNDO.
DEFINE VARIABLE b       AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-ext   AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-ex    AS INTEGER    NO-UNDO.

DEFINE VARIABLE z  AS INTEGER    NO-UNDO.
DEFINE VARIABLE z1 AS INTEGER    NO-UNDO.

DEFINE VARIABLE dureza-x     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dureza-jr    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE soma-dure    AS INTEGER    NO-UNDO.
DEFINE VARIABLE soma-dure-x  AS INTEGER    NO-UNDO.

DEFINE VARIABLE larg-bob-jr     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE amostra-larg-jr AS DECIMAL   EXTENT 9000 NO-UNDO.
DEFINE VARIABLE soma-larg-bob   AS INTEGER    NO-UNDO.

DEFINE VARIABLE compr-bob-jr     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE amostra-compr-jr AS DECIMAL    EXTENT 9000 NO-UNDO.
DEFINE VARIABLE soma-compr-bob   AS INTEGER    NO-UNDO.

DEFINE VARIABLE uniform-jr   AS INTEGER    NO-UNDO.
DEFINE VARIABLE adesao-jr    AS INTEGER    NO-UNDO.
DEFINE VARIABLE soma-bom-jr  AS INTEGER    NO-UNDO.
DEFINE VARIABLE soma-ruim-jr AS INTEGER    NO-UNDO.

DEFINE VARIABLE dens-ped-x      AS CHAR       NO-UNDO.
DEFINE VARIABLE dens-ot-jr      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE soma-dens       AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-arquivo       AS CHAR       NO-UNDO.

DEFINE VARIABLE obs-empax       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE nr-ext-jr       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE referencia-pm   AS CHARACTER  NO-UNDO.

DEFINE VARIABLE nr-pedido-pm    AS INTEGER    NO-UNDO.
DEFINE VARIABLE desv-padrao-jr  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE i-jr            AS INTEGER    NO-UNDO.
DEFINE VARIABLE soma-dp         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE resul-dp        AS DECIMAL    NO-UNDO.

DEFINE VARIABLE soma-result-jr  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE qtde-result-jr  AS integer    NO-UNDO.

DEFINE VARIABLE dias-jr          AS INTEGER    NO-UNDO.
DEFINE VARIABLE dt-validade-jr   AS DATE       NO-UNDO.
define variable c-tem-analise    as char       no-undo.
define variable c-laudo-zerado   as char       no-undo.
define variable c-laudo-completo as char       no-undo.
DEFINE VARIABLE l-libera-zerado  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE mes-valid-jr     AS INTEGER    NO-UNDO.

DEFINE VARIABLE conta-bob   AS INTEGER    NO-UNDO.
DEFINE VARIABLE lin-bob     AS INTEGER    NO-UNDO.
DEFINE VARIABLE lin-bob2    AS INTEGER    NO-UNDO.
DEFINE VARIABLE linhas-novas AS INTEGER    NO-UNDO.
DEFINE VARIABLE a-plt AS INTEGER    NO-UNDO.
    DEFINE VARIABLE x-plt AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE b-plt AS INTEGER    NO-UNDO.

    DEFINE VARIABLE c-bob-col1  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-dat-col1  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-bob-col2  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-dat-col2  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-bob-col3  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-dat-col3  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE x-col-1     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE x-col-2     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE x-col-3     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-tt-bb AS INTEGER     NO-UNDO.


/****************** Definiáao de Vari†veis Campo do Layout do Relat¢rio **********************/ 

define variable amostras-jr     AS DECIMAL EXTENT 9000.

/****************** Definiáao de Vari†veis do Relat¢rio N∆o Pedidas em Tela ******************/ 

/* Vari†veis necess†rias para a rastreabilidade de bobinas 
   Programa externo escp033 e escp034 */

DEFINE var lote-rastrear AS CHAR FORMAT "x(10)" NO-UNDO.
DEFINE var it-codigo-rastrear AS CHAR FORMAT "x(16)" NO-UNDO.

DEFINE  NEW GLOBAL SHARED TEMP-TABLE tt-rastrear
    FIELD ttras-lote-cons         LIKE lote-rastreab.lote-cons
    FIELD ttras-it-codigo-cons    LIKE lote-rastreab.it-codigo-cons 
    FIELD ttras-nr-ord-produ-cons LIKE lote-rastreab.nr-ord-produ-cons
    FIELD ttras-pesq              AS   INTEGER
    INDEX ch-tt-rastrear IS PRIMARY UNIQUE ttras-lote-cons.


/* Vari†veis usadas para gerar planilha excel. */

DEF VAR h-acomp           AS HANDLE              NO-UNDO.
DEF VAR c-arq             AS CHAR                NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR                NO-UNDO.   
DEF VAR l-cria            AS LOG                 NO-UNDO.
DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
DEF VAR i-linha           AS INT                 NO-UNDO.
DEF VAR c-coluna          AS char                NO-UNDO.

/* para executar o Excel */

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.

/****************** Definiáao de Vari†veis de Total do Relat¢rio *****************************/ 

/****************** Definiáao de Vari†veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Definiáao de Vari†veis de Processamento do Relat¢rio *********************/

def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.

/****************** Definiáao de Forms do Relat¢rio 132 Colunas ***************************************/ 

FORM descricao-comp         COLUMN-LABEL "Analise"   FORMAT "x(30)"     AT 001
     ttana-minimo           COLUMN-LABEL "Res.Min."  FORMAT ">>>>>9.99" AT 035
     ttana-maximo           COLUMN-LABEL "Res.Max."  FORMAT ">>>>>9.99" AT 048
     media-result-jr           COLUMN-LABEL "Res.MÇdio" FORMAT ">>>>>9.99" AT 061 
     ttana-qtd-result       COLUMN-LABEL "Nr.Anals"  FORMAT ">>9"       AT 074
     valor-tipico-jr        COLUMN-LABEL "Vlr-T°pico" FORMAT ">>>>>9.99" AT 083
     unidade-jr             COLUMN-LABEL "Unidade"   FORMAT "x(12)"     AT 096
     metodo-jr              COLUMN-LABEL "Metodo"    FORMAT "x(15)"     AT 113
     with down width 132 no-box stream-io frame f-relat-09-132.

FORM "Cliente:" AT 001 nome-abrev-jr format "x(12)" AT 010 "Nr.Pedido:" AT 070
      tp-pedido-jr FORMAT "x(2)" AT 081 c-nr-pedcli-ini format "x(13)"  at 083 
     "Tipo do Filme:" AT 001 it-codigo-ped FORMAT "x(15)" AT 016
     "Largura da Bobina (mm):" AT 070 larg-ped FORMAT ">>>>9" AT 095 
     "Nr. de Bobinas:" AT 001 qtd-bobinas FORMAT ">>>>9" AT 017
     "DiÉmetro da Bobina (mm):" AT 070 diex-ped FORMAT ">>>>9" AT 095 
     "Validade:   6 meses ap¢s fatura" AT 001
     "N£cleo (mm):" AT 070 diin-ped FORMAT ">>>>9" AT 095
     with down width 132 no-box stream-io frame f-relat-lin-ped.

FORM "Conclus∆o:" AT 001 "(    ) Aprovado" AT 035 "(    ) Reprovado" AT 085
      with down width 132 no-box stream-io frame f-relat-lin-final-1.

FORM "Observaá‰es / Recomendaá‰es" AT 050 
     "---------------------------" AT 050
      with down width 132 no-box stream-io frame f-relat-lin-final-2.

FORM "---------------------------" AT 025 "-----------------------------" AT 075
     " Respons†vel/Assinatura    " AT 025 "           Data              " AT 075
      with down width 132 no-box stream-io frame f-relat-lin-final-3.

FORM "REF.: LAB-RE-122B" AT 010 "REVIS«O: 03" AT 050 "DATA:" AT 080 data-jr FORMAT "99/99/9999" AT 086
      with down width 132 no-box stream-io frame f-relat-lin-inic-1.

FORM "N£mero do Certificado:" AT 050 
      with down width 132 no-box stream-io frame f-relat-lin-inic-2.

FORM "Objetivo:   (    ) Liberaá∆o de Amostras" AT 010 "(    ) Liberaá∆o de Produá∆o" AT 080
      with down width 132 no-box stream-io frame f-relat-lin-inic-3.

form HEADER
    fill("-", 132) format "x(132)" SKIP 
    WITH DOWN WIDTH 132 NO-BOX STREAM-IO FRAME f-relat-linha.

form HEADER
    fill(" ", 132) format "x(132)" SKIP 
    WITH DOWN WIDTH 132 NO-BOX STREAM-IO FRAME f-relat-branco.

create tt-param.
raw-transfer raw-param to tt-param.

FOR EACH tt-raw-digita:
   CREATE tt-digita.
   RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

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

assign c-programa     = "escq051rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "CERTIFICADO DE QUALIDADE"
       c-sistema      = "".

if  tt-param.formato = 1 then do:


form header
    fill("-", 80) format "x(80)" skip
    c-empresa format "x(24)" c-titulo-relat at 30 format "x(30)"
    "Folha:" at 70 page-number(str-rp) at 76 format ">>>>9" skip
    fill("-", 60) format "x(58)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 80 no-labels no-box page-top frame f-cabec-80.
        
form header
    fill("-", 80) format "x(80)" skip
    c-empresa format "x(24)" c-titulo-relat at 30 format "x(30)"
    "Folha:" at 70 page-number(str-rp) at 76 format ">>>>9" skip
    "Per°odo:" i-numper-x at 08 "-"
    da-iniper-x at 14 "to" da-fimper-x
    fill("-", 22) format "x(20)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 80 no-labels no-box page-top frame f-cabper-80.

run grapi/gr2005.p.

form header
    c-rodape format "x(80)"
    with stream-io width 80 no-labels no-box page-bottom frame f-rodape-80.

end. /* tt-param.formato = 1 */ 

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

    assign c-cod-estabel-ini = tt-param.c-cod-estabel-ini
           pesq-jr = tt-param.pesq-jr
.

find first estabelec where estabelec.cod-estabel = c-cod-estabel-ini.

find first empresa no-lock
    where empresa.ep-codigo = if avail estabelec then estabelec.ep-codigo else i-ep-codigo-usuario no-error.
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
           ASSIGN v-cod-destino-impres = "excel".
      
  l-libera-zerado = NO.
  FOR FIRST ext_usuar_grp_usuar WHERE 
                    ext_usuar_grp_usuar.ativo AND
                    ext_usuar_grp_usuar.cod_usuario = c-seg-usuario AND
                    ext_usuar_grp_usuar.cod_grp_usuar = "LAUDOZER"   NO-LOCK.

      l-libera-zerado = YES.

  END.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").


FOR EACH tt-notas.
    DELETE tt-notas.
END.

FOR EACH tt-digita NO-LOCK.


    ASSIGN nr-pedido-pm = tt-digita.nr-pedido.


    FIND FIRST tt-notas WHERE
         tt-notas.nr-nota-fis  = tt-digita.nr-nota-fis AND
         tt-notas.it-codigo    = tt-digita.it-codigo   AND
         tt-notas.nr-pedido    = nr-pedido-pm  AND
         tt-notas.nr-sequencia = tt-digita.nr-sequencia
         NO-ERROR.
         
    find first it-nota-fisc where 
         it-nota-fisc.serie        = tt-digita.serie and
         it-nota-fisc.nr-nota-fis  = tt-digita.nr-nota-fis and
         it-nota-fisc.nr-seq-fat   = tt-digita.nr-sequencia and
         it-nota-fisc.cod-estabel  = tt-param.c-cod-estabel-ini
         no-lock no-error.

    if not avail it-nota-fisc then next.
         

    IF NOT AVAIL tt-notas THEN DO:
        CREATE tt-notas.

        ASSIGN tt-notas.it-codigo    = tt-digita.it-codigo
               tt-notas.nr-pedido    = nr-pedido-pm
               tt-notas.nr-nota-fis  = tt-digita.nr-nota-fis
               tt-notas.nr-sequencia = tt-digita.nr-sequencia.
               
        ASSIGN tt-notas.nr-seq-ped    = it-nota-fisc.nr-seq-ped
               tt-notas.serie         = it-nota-fisc.serie.
     end.
      
               


    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

END.
             
/*---------------------------------------------------------------------*/


FOR EACH tt-am-cq-laudo.
    DELETE tt-am-cq-laudo.
END.

FOR EACH tt-am-cq-result-laudo.
    DELETE tt-am-cq-result-laudo.
END.


FOR EACH tt-notas NO-LOCK.

    FOR EACH tt-bobinas:
        DELETE tt-bobinas.
    END.

    FOR EACH tt-pedidos:
        DELETE tt-pedidos.
    END.
    
    for each tt-pedido-2.
     delete tt-pedido-2.
    end.

    FOR EACH tt-digita2:
        DELETE tt-digita2.
    END.

    assign v-num-reg-lidos = 0.

    ASSIGN conta-ped = 0
           pallet-x1 = ""
           pallet-x2 = ""
           pallet-x3 = ""
           pallet-x4 = ""
           pallet-x5 = ""
           pallet-x6 = ""
           pallet-x7 = ""
           pallet-x8 = ""
           pallet-x9 = ""
           pallet-x10 = ""
           pallet-x11 = ""
           pallet-x12 = ""
           pallet-x13 = ""
           nrnf-x1   = ""
           nrnf-ant  = ""
           it-codigo-ant = ""
           obs-ex    = ""
           dens-ped-x = "".

        ASSIGN dureza-jr        = 0
               soma-dure        = 0
               larg-bob-jr      = 0 
               amostra-larg-jr  = 0
               soma-larg-bob    = 0
               compr-bob-jr     = 0
               amostra-compr-jr = 0
               soma-compr-bob   = 0.  


    ASSIGN esp-min  = 9999
           esp-max  = 0
           soma-esp = 0
           qtd-esp  = 0
           lar-min  = 9999
           lar-max  = 0
           soma-lar = 0
           qtd-lar  = 0
           die-min  = 9999
           die-max  = 0
           soma-die = 0
           qtd-die  = 0
           qtd-bobinas = 0
           soma-peso   = 0.


        ASSIGN dureza-jr        = 0
               soma-dure        = 0
               larg-bob-jr      = 0
               amostra-larg-jr  = 0
               soma-larg-bob    = 0
               compr-bob-jr     = 0
               amostra-compr-jr = 0
               soma-compr-bob   = 0.  

        FOR EACH tt-digita WHERE
            tt-digita.nr-nota-fis = tt-notas.nr-nota-fis AND
            tt-digita.it-codigo   = tt-notas.it-codigo   AND
            tt-digita.nr-pedido   = tt-notas.nr-pedido   and
            tt-digita.nr-sequencia = tt-notas.nr-sequencia
            NO-LOCK.

            ASSIGN tt-param.i-modelo = tt-digita.tipo-modelo.

            IF tt-param.i-modelo    = 3 AND
               tt-digita.nr-pedido <> tt-notas.nr-pedido THEN NEXT.


            FIND FIRST it-nota-fisc WHERE
                 it-nota-fisc.cod-estabel = tt-param.c-cod-estabel-ini AND
                 it-nota-fisc.serie       = tt-digita.serie            AND
                 it-nota-fisc.nr-nota-fis = tt-digita.nr-nota-fis      AND
                 it-nota-fisc.nr-seq-fat  = tt-digita.nr-sequencia     AND
                 it-nota-fisc.it-codigo   = tt-digita.it-codigo
                NO-LOCK NO-ERROR.

            IF NOT AVAIL it-nota-fisc THEN NEXT.

            FOR EACH fat-ser-lote OF it-nota-fisc NO-LOCK.

                FIND LAST pallet WHERE
                              pallet.nr-pallet = fat-ser-lote.nr-serlote AND
                              pallet.it-codigo = it-nota-fisc.it-codigo  AND 
                              pallet.nr-pedido = tt-digita.nr-pedido 
                              NO-LOCK NO-ERROR.

                IF NOT AVAIL pallet THEN DO:

                    FIND LAST pallet WHERE
                                  pallet.cod-estabel = it-nota-fisc.cod-estabel AND
                                  pallet.nr-pallet   = fat-ser-lote.nr-serlote  AND
                                  pallet.it-codigo   = it-nota-fisc.it-codigo   
                                  NO-LOCK NO-ERROR.

                END.

                IF NOT AVAIL pallet THEN NEXT. 


               FIND FIRST tt-digita2 WHERE
                    tt-digita2.nr-nota-fis  = tt-digita.nr-nota-fis  AND
                    tt-digita2.it-codigo    = pallet.it-codigo       AND
                    tt-digita2.nr-pallet    = pallet.nr-pallet       
                    NO-ERROR.

               IF NOT AVAIL tt-digita2 THEN 
                    CREATE tt-digita2.


               ASSIGN tt-digita2.nr-pallet  = pallet.nr-pallet 
                      tt-digita2.nr-pedido  = pallet.nr-pedido.   
                      
                      if tt-digita2.nr-pedido <> it-nota-fisc.nr-pedido and
                         it-nota-fisc.nr-pedido <> 0 then
                         assign tt-digita2.nr-pedido  = it-nota-fisc.nr-pedido.  
                    
               ASSIGN tt-digita2.dt-trans   = pallet.data-pallet
                      tt-digita2.nr-bobinas = pallet.nr-bobinas
                      tt-digita2.peso-liq   = pallet.peso-liquido
                      tt-digita2.peso-bru   = pallet.peso-bruto
                      tt-digita2.it-codigo  = pallet.it-codigo
                      tt-digita2.sequencia  = tt-digita.nr-sequencia
                      tt-digita2.nr-nota-fis = tt-digita.nr-nota-fis
                      tt-digita2.serie       = tt-digita.serie.

               assign v-num-reg-lidos = v-num-reg-lidos + 1.
               run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

            END.

        END.


    RUN pi-gera-laudo.

    ASSIGN v-num-reg-lidos = 0. 

END.

/* Se laudo oficial, verificar se existe an†lises com resultado zerado */

/*

assign c-tem-analise = "N".

find first tt-am-cq-result-laudo where 
     tt-am-cq-result-laudo.media-result <> 0
     no-lock no-error.

if not avail tt-am-cq-result-laudo then do:

          run utp/ut-msgs.p (input "show":U, input 17006, string("Laudo com Resultado Zerado")).

          run pi-finalizar in h-acomp.

          return 'OK'.

end.

*/
   


IF tt-param.tipo-geracao = 2 and 
    l-libera-zerado = NO
    /*(c-seg-usuario <> "wdsanto" and
     c-seg-usuario <> "aschnei" and
     c-seg-usuario <> "fgcardo" and
     c-seg-usuario <> "bapsilv" and
     c-seg-usuario <> "cakzago")*/ THEN DO:
    
    FOR EACH tt-am-cq-result-laudo NO-LOCK.
                
         if tt-param.i-modelo = 3 and   
                (tt-am-cq-result-laudo.cod-comp = 30 or
                 tt-am-cq-result-laudo.cod-comp = 35) then next.
      

        IF tt-am-cq-result-laudo.media-result = 0 THEN DO: 

          run utp/ut-msgs.p (input "show":U, input 17006, string("Laudo com Resultado Zerado, NF: " + STRING(tt-am-cq-result-laudo.nr-nota-fis))).

          run pi-finalizar in h-acomp.

          return 'OK'.

        END.

    END.

END.


/*---------------------------------------------------------------------*/



/* Geraá∆o da Planilha Excel */

    /* Cria Aplicaá∆o do Excel */

CREATE "Excel.Application" c-excel.
ASSIGN c-excel:DisplayAlerts = FALSE.
     

FOR EACH tt-am-cq-laudo WHERE
    tt-am-cq-laudo.gerado-excel = NO
    USE-INDEX ger-excel exclusive-lock.  


    IF tt-am-cq-laudo.int-1 = 0 THEN
        ASSIGN c-modelo-planilha = search("modelos\mod-escq051.xls") 
               c-arq             = SESSION:TEMP-DIRECTORY  /* "v:\temp\"*/ .

    IF tt-am-cq-laudo.int-1 = 2 THEN
        ASSIGN c-modelo-planilha = search("modelos\mod-escq051-3.xls") 
               c-arq             = SESSION:TEMP-DIRECTORY /* "v:\temp\"*/ .

    IF tt-am-cq-laudo.int-1 = 3 THEN
        ASSIGN c-modelo-planilha = search("modelos\mod-escq051-4.xls") 
               c-arq             = SESSION:TEMP-DIRECTORY /* "v:\temp\"*/ .

    RUN pi-cria-planilha.

    ASSIGN c-tem-analise    = "N"
           c-laudo-completo = "N"
           c-laudo-zerado   = "N".
           
    RUN pi-gera-planilha.
   
    RUN pi-mostra-planilha.
    
    if c-tem-analise = "N"  and  tt-param.tipo-geracao = 2 AND l-libera-zerado = NO /* c-seg-usuario <> "fgcardo" and c-seg-usuario <> "aschnei" and  c-seg-usuario <> "bapsilv"*/  then do:
 
       run utp/ut-msgs.p (input "show":U, input 17006, string("Laudo Sem Resultados")).

       run pi-finalizar in h-acomp.
       
       RUN pi-finaliza-impressao.       

       return 'OK'.

    END.

   /* if c-laudo-completo = "N" then do:

       run utp/ut-msgs.p (input "show":U, input 17006, string("Laudo Sem Preenchimento")).

       run pi-finalizar in h-acomp.
       
       RUN pi-finaliza-impressao.       

       return 'OK'.

    END.
    */
    if c-laudo-zerado <> "N" and  tt-param.tipo-geracao = 2 AND   l-libera-zerado = NO /*  c-seg-usuario <> "fgcardo" and c-seg-usuario <> "aschnei" and  c-seg-usuario <> "bapsilv" */ then do:

       run utp/ut-msgs.p (input "show":U, input 17006, string("Laudo Com Resultado Zerado")).

       run pi-finalizar in h-acomp.
       
       RUN pi-finaliza-impressao.       

       return 'OK'.

    END.


    IF tt-param.tipo-geracao = 1 THEN DO:

        FIND FIRST pol-param-relatorio WHERE
            pol-param-relatorio.cod-prog-dtsul = "ESCQ051" AND
            pol-param-relatorio.cod-usuario    = tt-am-cq-laudo.nr-nota-fis AND
            pol-param-relatorio.mes-refer      = int(tt-am-cq-laudo.serie)  AND
            pol-param-relatorio.ano-refer      = int(tt-am-cq-laudo.cod-estabel)
            EXCLUSIVE-LOCK NO-ERROR.

        IF NOT AVAIL pol-param-relatorio THEN DO:

            CREATE pol-param-relatorio.

            ASSIGN pol-param-relatorio.cod-prog-dtsul = "ESCQ051"                           
                   pol-param-relatorio.cod-usuario    = tt-am-cq-laudo.nr-nota-fis 
                   pol-param-relatorio.mes-refer      = int(tt-am-cq-laudo.serie)                                   
                   pol-param-relatorio.ano-refer      = int(tt-am-cq-laudo.cod-estabel). 
        END.

    END.


    IF tt-param.tipo-geracao = 2 THEN DO:

       IF /*c-laudo-completo = "S" AND*/
          c-tem-analise    = "S" AND
          c-laudo-zerado   = "N" THEN DO:

            RUN pi-gera-e-mail.
             
            RUN pi-gera-e-mail-2.

         /*   
            RUN pi-gera-e-mail.
         */

       END.

       ELSE DO:

              if     l-libera-zerado = NO /* c-seg-usuario <> "fgcardo" AND c-seg-usuario <> "aschnei" and  c-seg-usuario <> "bapsilv"*/ then do:
                run utp/ut-msgs.p (input "show":U, input 17006, string("Laudo Com Problema - N∆o enviado e-mail")).
          
           
                run pi-finalizar in h-acomp.
                
                RUN pi-finaliza-impressao.       
                
                return 'OK'.
            end.
            else
              run utp/ut-msgs.p (input "show":U, input 17006, string("Laudo Foi Gerado Com Problema e N∆o enviado e-mail")).

       END.
       
    END.

    ASSIGN tt-am-cq-laudo.gerado-excel = YES.


END.

/* Na rodada oficial deve gravar os arquivos de laudos */

IF tt-param.tipo-geracao = 2 THEN DO:
                      
    FOR EACH tt-am-cq-laudo NO-LOCK.

        ASSIGN nr-laudo-jr  = 1
               gravou-laudo = 0.
        
        FIND LAST am-cq-laudo NO-LOCK NO-ERROR.
        
        IF AVAIL am-cq-laudo THEN
            nr-laudo-jr = am-cq-laudo.nr-laudo + 1.
        
        CREATE am-cq-laudo.
        
        ASSIGN am-cq-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel 
               am-cq-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis
               am-cq-laudo.nr-laudo    = nr-laudo-jr
               am-cq-laudo.dec-1       = tt-am-cq-laudo.dec-1.

        ASSIGN am-cq-laudo.dt-criacao         = tt-am-cq-laudo.dt-criacao                
               am-cq-laudo.tipo-laudo         = tt-am-cq-laudo.tipo-laudo                
               am-cq-laudo.cod-emitente       = tt-am-cq-laudo.cod-emitente              
               am-cq-laudo.it-codigo          = tt-am-cq-laudo.it-codigo                 
               am-cq-laudo.qtd-Bobinas        = tt-am-cq-laudo.qtd-Bobinas               
               am-cq-laudo.peso-liquido       = tt-am-cq-laudo.peso-liquido              
               am-cq-laudo.observacao         = tt-am-cq-laudo.observacao                
               am-cq-laudo.char-1             = tt-am-cq-laudo.char-1                    
               am-cq-laudo.data-1             = tt-am-cq-laudo.data-1                    
               am-cq-laudo.int-1              = tt-am-cq-laudo.int-1                     
               am-cq-laudo.log-1              = tt-am-cq-laudo.log-1                     
               am-cq-laudo.nome-abrev         = tt-am-cq-laudo.nome-abrev                
               am-cq-laudo.nome-responsavel   = tt-am-cq-laudo.nome-responsavel          
               am-cq-laudo.pedido             = tt-am-cq-laudo.pedido                    
               am-cq-laudo.pallet             = tt-am-cq-laudo.pallet                    
               am-cq-laudo.larg               = tt-am-cq-laudo.larg                      
               am-cq-laudo.diin               = tt-am-cq-laudo.diin                      
               am-cq-laudo.diex               = tt-am-cq-laudo.diex                      
               am-cq-laudo.pedido-cliente     = tt-am-cq-laudo.pedido-cliente            
               am-cq-laudo.zint               = tt-am-cq-laudo.zint                      
               am-cq-laudo.enviado-email      = tt-am-cq-laudo.enviado-email             
               am-cq-laudo.dt-envio-email     = tt-am-cq-laudo.dt-envio-email            
               am-cq-laudo.tipo-envio-email   = tt-am-cq-laudo.tipo-envio-email             
               am-cq-laudo.gerado-excel       = tt-am-cq-laudo.gerado-excel              
               am-cq-laudo.cod-usuario        = tt-am-cq-laudo.cod-usuario. 
               substring(am-cq-laudo.char-1,1,5) = tt-am-cq-laudo.serie. 

        FOR EACH tt-am-cq-result-laudo WHERE 
                 tt-am-cq-result-laudo.cod-estabel  = tt-am-cq-laudo.cod-estabel  AND
                 tt-am-cq-result-laudo.nr-nota-fis  = tt-am-cq-laudo.nr-nota-fis  AND
                 tt-am-cq-result-laudo.nr-laudo     = tt-am-cq-laudo.nr-laudo
            NO-LOCK.   

            CREATE am-cq-result-laudo.

            ASSIGN am-cq-result-laudo.cod-estabel  = tt-am-cq-result-laudo.cod-estabel 
                   am-cq-result-laudo.nr-nota-fis  = tt-am-cq-result-laudo.nr-nota-fis 
                   am-cq-result-laudo.nr-laudo     = nr-laudo-jr    
                   am-cq-result-laudo.cod-exame    = tt-am-cq-result-laudo.cod-exame    
                   am-cq-result-laudo.cod-comp     = tt-am-cq-result-laudo.cod-comp     
                   am-cq-result-laudo.descricao    = tt-am-cq-result-laudo.descricao    
                   am-cq-result-laudo.metodo       = tt-am-cq-result-laudo.metodo       
                   am-cq-result-laudo.unidade      = tt-am-cq-result-laudo.unidade      
                   am-cq-result-laudo.soma-result  = tt-am-cq-result-laudo.soma-result  
                   am-cq-result-laudo.qtd-result   = tt-am-cq-result-laudo.qtd-result   
                   am-cq-result-laudo.media-result = tt-am-cq-result-laudo.media-result 
                   am-cq-result-laudo.menor-result = tt-am-cq-result-laudo.menor-result 
                   am-cq-result-laudo.maior-result = tt-am-cq-result-laudo.maior-result 
                   am-cq-result-laudo.espec-min    = tt-am-cq-result-laudo.espec-min    
                   am-cq-result-laudo.espec-max    = tt-am-cq-result-laudo.espec-max    
                   am-cq-result-laudo.espec-alvo   = tt-am-cq-result-laudo.espec-alvo   
                   am-cq-result-laudo.char-1       = tt-am-cq-result-laudo.char-1       
                   am-cq-result-laudo.data-1       = tt-am-cq-result-laudo.data-1       
                   am-cq-result-laudo.dec-1        = tt-am-cq-result-laudo.dec-1        
                   am-cq-result-laudo.int-1        = tt-am-cq-result-laudo.int-1        
                   am-cq-result-laudo.log-1        = tt-am-cq-result-laudo.log-1        
                   am-cq-result-laudo.nr-decimais  = tt-am-cq-result-laudo.nr-decimais.  

        END.


    END.

END.

/* Fim da rodada oficial deve gravar os arquivos de laudos */


RUN pi-finaliza-impressao.


run pi-finalizar in h-acomp.

return 'OK'.

/* fim do programa */

/*------------------------ procedures  ------------------------*/

PROCEDURE pi-gera-laudo:

    FOR EACH tt-digita2 NO-LOCK:
 
        ASSIGN conta-ped     = conta-ped + 1.

        IF tt-digita2.nr-pedido <> 0 THEN
           ASSIGN  c-nr-pedido-1 = tt-digita2.nr-pedido
                   i-nr-sequencia-1 = tt-digita2.sequencia .

        IF tt-digita2.nr-nota-fis <> nrnf-ant THEN DO:
    
           ASSIGN nrnf-x1 = (TRIM (nrnf-x1) + "/" + STRING (tt-digita2.nr-nota-fis))
                  nrnf-ant = tt-digita2.nr-nota-fis. 
                      
           FIND FIRST nota-fiscal WHERE
               nota-fiscal.cod-estabel = tt-param.c-cod-estabel-ini AND
               nota-fiscal.serie       = tt-digita2.serie            AND
               nota-fiscal.nr-nota-fis = tt-digita2.nr-nota-fis 
               NO-LOCK NO-ERROR.
           

           IF AVAIL nota-fiscal THEN DO:

            FOR EACH it-nota-fisc OF nota-fiscal where  
              it-nota-fisc.nr-pedido = tt-digita2.nr-pedido and
              it-nota-fisc.nr-seq-fat = tt-digita2.sequencia 
/*   16/7/2012 -edson                         it-nota-fisc.nr-seq-ped = tt-digita2.sequencia */

                                             
                  NO-LOCK.

                FIND FIRST ped-venda WHERE
                    ped-venda.nr-pedido = it-nota-fisc.nr-pedido
                    NO-LOCK NO-ERROR.

                IF AVAIL ped-venda THEN DO:
                      
                     ASSIGN desc-ex = ped-venda.observacoes  + " " +  ped-venda.local-entreg
                            i-ext = INDEX(desc-ex,"EXT",1)
                            i-ex =  INDEX(desc-ex,"EX",1)
                            i = 0.
                     IF i-ext >  0 THEN
                         ASSIGN i = i-ext.
                     ELSE 
                         IF i-ex > 0 THEN ASSIGN i = i-ex.
                    
                     IF i > 0 THEN 
                         ASSIGN
                            desc-ex = SUBSTR(desc-ex, i,20) 
                            b =       INDEX(desc-ex,"/",1) + 2
                            desc-ex = SUBSTR(desc-ex, 1,b).
                         ELSE 
                            ASSIGN desc-ex = " ".
                    
                     IF INDEX(obs-ex,DESC-ex) = 0 THEN  ASSIGN obs-ex = obs-ex + " " + DESC-ex.

                END. /* if avail ped-venda */

            END. /* for each it-nota-fisc */

           END. /* if avail nota-fiscal */
                        
        END.  /* tt-digita2.nr-nota-fis <> nrnf-ant */
        
        IF conta-ped < 17 THEN
        ASSIGN pallet-x1 = (TRIM (pallet-x1) + " " + STRING (tt-digita2.nr-pallet)).
          ELSE
            IF conta-ped < 33 THEN
              ASSIGN pallet-x2 = (TRIM (pallet-x2) + " " + STRING (tt-digita2.nr-pallet)).
            ELSE
              IF conta-ped < 49 THEN
                ASSIGN pallet-x3 = (TRIM (pallet-x3) + " " + STRING (tt-digita2.nr-pallet)).
              ELSE
                IF conta-ped < 65 THEN
                  ASSIGN pallet-x4 = (TRIM (pallet-x4) + " " + STRING (tt-digita2.nr-pallet)).
                ELSE
                  IF conta-ped < 81 THEN
                    ASSIGN pallet-x5 = (TRIM (pallet-x5) + " " + STRING (tt-digita2.nr-pallet)).
                  ELSE
                    IF conta-ped < 97 THEN
                      ASSIGN pallet-x6 = (TRIM (pallet-x6) + " " + STRING (tt-digita2.nr-pallet)).
                    ELSE
                      IF conta-ped < 113 THEN
                        ASSIGN pallet-x7 = (TRIM (pallet-x7) + " " + STRING (tt-digita2.nr-pallet)).
                      ELSE
                        IF conta-ped < 129 THEN
                          ASSIGN pallet-x8 = (TRIM (pallet-x8) + " " + STRING (tt-digita2.nr-pallet)).
                        ELSE
                          IF conta-ped < 145 THEN
                            ASSIGN pallet-x9 = (TRIM (pallet-x9) + " " + STRING (tt-digita2.nr-pallet)).
                          ELSE
                            IF conta-ped < 161 THEN
                              ASSIGN pallet-x10 = (TRIM (pallet-x10) + " " + STRING (tt-digita2.nr-pallet)).
                            ELSE
                              IF conta-ped < 177 THEN
                                ASSIGN pallet-x11 = (TRIM (pallet-x11) + " " + STRING (tt-digita2.nr-pallet)).
                              ELSE
                                IF conta-ped < 193 THEN
                                  ASSIGN pallet-x12 = (TRIM (pallet-x12) + " " + STRING (tt-digita2.nr-pallet)).
                                ELSE
                                  IF conta-ped < 209 THEN
                                    ASSIGN pallet-x13 = (TRIM (pallet-x13) + " " + STRING (tt-digita2.nr-pallet)).
    

        FIND FIRST tt-pedidos WHERE
             ttped-nr-pedido = tt-digita2.nr-pedido 
             NO-LOCK NO-ERROR.
    
             IF NOT AVAIL tt-pedidos THEN DO:
    
                 CREATE tt-pedidos.
                 ASSIGN ttped-nr-pedido = tt-digita2.nr-pedido
                        ttped-nr-nota-fis = tt-digita2.nr-nota-fis.
               
                 ASSIGN larg-ped = 0
                        diin-ped = 0
                        diex-ped = 0
                        pedcli-ped = ""
                        zint-jr    = ""
                        prodclie-jr = "".
    
                 FIND FIRST ped-venda WHERE
                      ped-venda.nr-pedido = tt-digita2.nr-pedido 
                      NO-LOCK NO-ERROR.
    
                 IF NOT AVAIL ped-venda THEN next.

                 ASSIGN ttped-ems-pedcli = ped-venda.nr-pedcli.
                 
                 assign esp-jr = 0.                 
    
                 FIND FIRST ped-item WHERE 
                      ped-item.nome-abrev = ped-venda.nome-abrev AND
                      ped-item.nr-pedcli = ped-venda.nr-pedcli AND
                      ped-item.ind-componen  < 3           AND
                      ped-item.nr-sequencia = tt-notas.nr-seq-ped and
                      ped-item.cod-refer <> "" 
                      NO-LOCK USE-INDEX ch-item-ped NO-ERROR.    
                                     
                 IF NOT AVAIL ped-item  THEN next.
                 
                 assign esp-jr = int(substring(ped-item.it-codigo,1,2)).
    
                 FIND FIRST cot-est-mast
                   WHERE cot-est-mast.item-cotacao = ped-item.it-codigo 
                   AND cot-est-mast.nr-estrut    = int(ped-item.cod-refer)
                   NO-LOCK NO-ERROR.
    
                   IF AVAIL cot-est-mast THEN DO:
             
                     FIND var-result 
                         WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                         AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                         AND var-result.nome-var     = "pedcli" NO-LOCK NO-ERROR.
    
                     IF AVAIL var-result THEN
                         ASSIGN  pedcli-ped = var-result.valor-char.

             
                     FIND var-result 
                         WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                         AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                         AND var-result.nome-var     = "prodclie" NO-LOCK NO-ERROR.
    
                     IF AVAIL var-result THEN
                         ASSIGN  prodclie-jr = var-result.valor-char.

    
        
        FIND FIRST tt-pedido-2 WHERE
             tt-pedido-2.nr-pedido = tt-notas.nr-pedido 
             NO-LOCK NO-ERROR.
  
             IF NOT AVAIL tt-pedido-2 THEN DO:
    
                 CREATE tt-pedido-2.
                 ASSIGN tt-pedido-2.nr-pedido = tt-notas.nr-pedido
                        tt-pedido-2.prodclie  = prodclie-jr
                        tt-pedido-2.pedcli    = pedcli-ped.
                        
             end.
               

    /*
          /* rotina para Buscar as densidades oticas cadastradas nos pedidos */
             
                     FIND var-result 
                         WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                         AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                         AND var-result.nome-var     = "densotic" NO-LOCK NO-ERROR.
    
                     IF AVAIL var-result AND var-result.valor-dec <> 0 THEN
                         ASSIGN  dens-ped-x = dens-ped-x + STRING (var-result.valor-dec, ">>>>>>>9.99").
    
          /*------------------------------------------------------------------*/
    */
    
                     FIND var-result 
                         WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                         AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                         AND var-result.nome-var     = "largura" NO-LOCK NO-ERROR.
    
                     IF AVAIL var-result THEN
                        ASSIGN  larg-ped = var-result.valor-dec.

    
                     FIND var-result 
                        WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                        AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                        AND var-result.nome-var     = "diin" NO-LOCK NO-ERROR.
    
                     IF AVAIL var-result THEN
                        ASSIGN  diin-ped = var-result.valor-dec.
    
                     FIND var-result 
                        WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                        AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                        AND var-result.nome-var     = "diex" NO-LOCK NO-ERROR.
    
                     IF AVAIL var-result THEN
                        ASSIGN  diex-ped = var-result.valor-dec.
    
                   END.


                    FIND var-result 
                        WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                        AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                        AND var-result.nome-var     = "zint" NO-LOCK NO-ERROR.
                
                    IF AVAIL var-result THEN
                        ASSIGN  zint-jr = var-result.valor-char.

                     
                     
                     
                   ASSIGN ttped-diex     = diex-ped
                          ttped-diin     = diin-ped
                          ttped-larg     = larg-ped
                          ttped-pedcli   = pedcli-ped
                          ttped-zint     = zint-jr
                          ttped-prodcli  = prodclie-jr.
    
             END.
    
             ASSIGN ttped-qtdbob = ttped-qtdbob + tt-digita2.nr-bobinas.
    
    END.
    
                        
     
    FIND FIRST ped-venda 
             where ped-venda.nr-pedido  =  tt-notas.nr-pedido     
             NO-LOCK NO-ERROR.             
                   

    IF AVAIL ped-venda THEN DO:
                    
        ASSIGN cod-estabel-jr  = ped-venda.cod-estabel
               nome-abrev-jr   = ped-venda.nome-abrev
               cod-emitente-jr = ped-venda.cod-emitente
               tp-pedido-jr    = ped-venda.tp-pedido
               c-nr-pedcli-ini = ped-venda.nr-pedcli.

       IF nome-abrev-jr = "NITRIFLEX" THEN
           ASSIGN nome-abrev-jr = "CHEMTON SA".

    

        FOR Each ped-item OF ped-venda No-lock Where
             ped-item.cod-refer <> "" AND
             ped-item.ind-componen <> 3 and
             ped-item.nr-sequencia = tt-notas.nr-seq-ped:
    
            FIND FIRST ITEM WHERE ITEM.it-codigo = ped-item.it-codigo
                 USE-INDEX codigo NO-LOCK NO-ERROR.
    
            IF NOT AVAIL ITEM THEN NEXT.
    
            ASSIGN fm-codigo-jr = ITEM.fm-codigo.

            FOR EACH tt-digita2 NO-LOCK:
            
               FIND FIRST pallet WHERE
                      pallet.it-codigo = tt-digita2.it-codigo AND 
                      pallet.nr-pallet = tt-digita2.nr-pallet
                      USE-INDEX ITEM
                      NO-LOCK NO-ERROR.
    

                    IF AVAIL pallet THEN DO:

                          ASSIGN qtd-bobinas = qtd-bobinas + pallet.nr-bobinas
                                 soma-peso   = soma-peso   + pallet.peso-liquido.
            
                        FOR EACH it-pallet OF pallet NO-LOCK :
    
                            FIND tt-bobinas WHERE
                                 ttbob-lote  = it-pallet.lote-bobina AND
                                 ttbob-linha = 1
                                 USE-INDEX ch-tt-bobinas NO-ERROR.
                            
                            IF NOT AVAIL tt-bobinas THEN DO:
                                  CREATE tt-bobinas.
                            
                                  ASSIGN ttbob-lote      = it-pallet.lote-bobina
                                     ttbob-linha     = 1
                                     ttbob-it-codigo = it-pallet.it-codigo
                                     ttbob-nr-ord-produ = 0
                                     ttbob-dt-trans  = 12/31/2010.
                            END.
                           
                           
                                /* Encontra a Largura e Comprimento das Bobinas Embarcadas */
                                
                                
                                FIND FIRST lote-carac-tec WHERE
                                     lote-carac-tec.it-codigo = ped-item.it-codigo     AND
                                     lote-carac-tec.lote      = it-pallet.lote-bobina  AND
                                     lote-carac-tec.cd-comp   = "largura"
                                     NO-LOCK NO-ERROR. 
                                     
                       
                                IF AVAIL lote-carac-tec AND DEC(lote-carac-tec.vl-result) > 0 THEN
                        
                                    ASSIGN larg-bob-jr   = larg-bob-jr + DEC(lote-carac-tec.vl-result)
                                           soma-larg-bob = soma-larg-bob + 1
                                           amostra-larg-jr [soma-larg-bob] = DEC(lote-carac-tec.vl-result) . 
                                
                                FIND FIRST lote-carac-tec WHERE
                                     lote-carac-tec.it-codigo = ped-item.it-codigo     AND
                                     lote-carac-tec.lote      = it-pallet.lote-bobina  AND
                                     lote-carac-tec.cd-comp   = "compr"
                                     NO-LOCK NO-ERROR. 
                        
                                IF AVAIL lote-carac-tec AND DEC(lote-carac-tec.vl-result) > 0 THEN
                        
                                    ASSIGN compr-bob-jr   = compr-bob-jr + DEC(lote-carac-tec.vl-result)
                                           soma-compr-bob = soma-compr-bob + 1
                                           amostra-compr-jr [soma-compr-bob] = DEC(lote-carac-tec.vl-result). 
                                
                        
                        
                        /* Fim da Largura e Comprimento */  
                            
                                      
                                /* Encontra a Dureza das Bobinas Embarcadas */
                                
                                ASSIGN dureza-x    = 0
                                       soma-dure-x = 0.
                                
                                FIND FIRST lote-carac-tec WHERE
                                     lote-carac-tec.it-codigo = ped-item.it-codigo     AND
                                     lote-carac-tec.lote      = it-pallet.lote-bobina  AND
                                     lote-carac-tec.cd-comp   = "durezae"
                                     NO-LOCK NO-ERROR. 
                        
                                IF AVAIL lote-carac-tec THEN
                        
                                    ASSIGN dureza-x     = dureza-x + DEC(lote-carac-tec.vl-result)
                                           soma-dure-x  = soma-dure-x + 1. 
                                
                                FIND FIRST lote-carac-tec WHERE
                                     lote-carac-tec.it-codigo = ped-item.it-codigo     AND
                                     lote-carac-tec.lote      = it-pallet.lote-bobina  AND
                                     lote-carac-tec.cd-comp   = "durezac"
                                     NO-LOCK NO-ERROR. 
                        
                                IF AVAIL lote-carac-tec THEN
                        
                                    ASSIGN dureza-x     = dureza-x + DEC(lote-carac-tec.vl-result)
                                           soma-dure-x  = soma-dure-x + 1. 
                                
                                FIND FIRST lote-carac-tec WHERE
                                     lote-carac-tec.it-codigo = ped-item.it-codigo     AND
                                     lote-carac-tec.lote      = it-pallet.lote-bobina  AND
                                     lote-carac-tec.cd-comp   = "durezad"
                                     NO-LOCK NO-ERROR. 
                        
                                IF AVAIL lote-carac-tec THEN
                        
                                    ASSIGN dureza-x     = dureza-x + DEC(lote-carac-tec.vl-result)
                                           soma-dure-x  = soma-dure-x + 1. 
                        
                                IF dureza-x <> 0 THEN
                                    ASSIGN dureza-x   = dureza-x / soma-dure-x 
                                           dureza-jr  = dureza-jr + dureza-x
                                           soma-dure  = soma-dure + 1.
                        
                        /* Fim da Dureza */  

                           
                                FIND LAST movto-mat 
                                     where 
                                           movto-mat.it-codigo = ped-item.it-codigo and
                                           movto-mat.lote      = it-pallet.lote-bobina AND
                                           movto-mat.esp-docto = 1 AND
                                           NOT CAN-FIND(FIRST rep-prod WHERE rep-prod.nr-reporte = movto-mat.nr-reporte AND rep-prod.qt-estorno > 0 NO-LOCK)
                                           USE-INDEX lote NO-LOCK NO-ERROR.
                            
                               IF NOT AVAIL movto-mat THEN next.
                           
                               FIND FIRST movto-estoq 
                                     where movto-estoq.nr-reporte = movto-mat.nr-reporte 
                                           NO-LOCK NO-ERROR.
                            
                                IF NOT AVAIL movto-estoq THEN next.
                                 
                              
                                ASSIGN lote-rast = movto-estoq.lote
                                       op-rast   = movto-estoq.nr-ord-produ
                                       data-rast = movto-estoq.dt-trans.
                                       
                            
                            
                                RUN grava-rastreabilidade.
                            
                            
                                ASSIGN lote-rastrear = it-pallet.lote-bobina 
                                       it-codigo-rastrear = it-pallet.it-codigo.
                            
                                 RUN cpp/escp033.p (INPUT lote-rastrear,
                                                    INPUT it-codigo-rastrear) 
                                                    NO-ERROR.
                            
                                 IF ERROR-STATUS:ERROR THEN DO:
                                    MESSAGE "Erro execuá∆o no programa externo"
                                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                                    NEXT.
                                 END.
                            
                                 FOR EACH tt-rastrear NO-LOCK.
                                     

                                     FIND FIRST movto-estoq WHERE
                                     movto-estoq.nr-ord-produ = ttras-nr-ord-produ-cons AND
                                     movto-estoq.num-sequen >= 1 AND 
                                     movto-estoq.it-codigo = ttras-it-codigo-cons AND 
                                     movto-estoq.lote      = ttras-lote-cons AND
                                     movto-estoq.esp-docto = 1 AND
                                     movto-estoq.cod-depos <> "ARC" AND
                                     NOT CAN-FIND(FIRST rep-prod WHERE rep-prod.nr-reporte = movto-estoq.nr-reporte AND rep-prod.qt-estorno > 0 NO-LOCK)
                                     USE-INDEX ord-seq   
                                     NO-LOCK NO-ERROR.
                            
                                   IF AVAIL movto-estoq THEN DO:
                                
                                      ASSIGN lote-rast = movto-estoq.lote
                                             op-rast   = movto-estoq.nr-ord-produ
                                             data-rast = movto-estoq.dt-trans.

                                   END.

                                   ELSE DO:
                                
                                      ASSIGN lote-rast = ttras-lote-cons
                                             op-rast   = 0
                                             data-rast = TODAY.

                                   END.
                                      
                            
                                      RUN grava-rastreabilidade.

                            

                                 END.
                            
                            
                            
                                assign v-num-reg-lidos = v-num-reg-lidos + 1.
                                run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
                            
                                ASSIGN nome-abrev-jr   = ped-venda.nome-abrev
                                       cod-emitente-jr = ped-venda.cod-emitente.
                        
                                IF nome-abrev-jr = "NITRIFLEX" THEN
                                   ASSIGN nome-abrev-jr = "CHEMTON SA".
                        
                        
                                ASSIGN tp-pedido-jr  = ped-venda.tp-pedido.
                                
                                IF espes-jr1 > 0 THEN DO:
                                   ASSIGN soma-esp = soma-esp + espes-jr1
                                          qtd-esp  = qtd-esp  + 1.
                                   IF espes-jr1 < esp-min THEN ASSIGN esp-min = espes-jr1.
                                   IF espes-jr1 > esp-max THEN ASSIGN esp-max = espes-jr1.
                            
                                END.
                            
                                IF larg-jr1 > 0 THEN DO:
                                   ASSIGN soma-lar = soma-lar + larg-jr1
                                          qtd-lar  = qtd-lar  + 1.
                                   IF larg-jr1 < lar-min THEN ASSIGN lar-min = larg-jr1.
                                   IF larg-jr1 > lar-max THEN ASSIGN lar-max = larg-jr1.
                            
                                END.
                            
                                IF diex-jr1 > 0 THEN DO:
                                   ASSIGN soma-die = soma-die + diex-jr1
                                          qtd-die  = qtd-die  + 1.
                                   IF diex-jr1 < die-min THEN ASSIGN die-min = diex-jr1.
                                   IF diex-jr1 > die-max THEN ASSIGN die-max = diex-jr1.
                            
                                END.
                                   
                        END.
                    END.  
            END.
       
        END. 
   

 
    ASSIGN qtd-bobinas = 0
           soma-peso   = 0.
    
    FOR EACH tt-digita2 NO-LOCK:
    
       FIND FIRST pallet WHERE
              pallet.it-codigo = tt-digita2.it-codigo AND 
              pallet.nr-pallet = tt-digita2.nr-pallet
              USE-INDEX ITEM
              NO-LOCK NO-ERROR.
    
       IF AVAIL pallet THEN 
          ASSIGN qtd-bobinas = qtd-bobinas + pallet.nr-bobinas
                 soma-peso   = soma-peso   + pallet.peso-liquido.
    
    END.
 
    IF v-num-reg-lidos > 0 THEN DO:
    
        FIND FIRST ped-venda WHERE 
             ped-venda.nr-pedido = tt-notas.nr-pedido 
             NO-LOCK NO-ERROR.
        IF AVAIL ped-venda  THEN DO:
             FIND FIRST ped-item WHERE 
              ped-item.nome-abrev = ped-venda.nome-abrev AND 
              ped-item.nr-pedcli = ped-venda.nr-pedcli   AND
              ped-item.nr-sequencia = tt-notas.nr-seq-ped and
              ped-item.ind-componen  < 3                 AND
              ped-item.cod-refer <> "" 
             NO-LOCK NO-ERROR.
    
             IF AVAIL ped-item  THEN 
               ASSIGN it-codigo-ped = ped-item.it-codigo.
        END.
         
       
        
    END.


    ASSIGN data-jr = TODAY.

    /* Gerar o arquivo tt-am-cq-laudo */

    DO: 

        ASSIGN nr-laudo-jr  = 1
               gravou-laudo = 0.
        
        FIND LAST am-cq-laudo NO-LOCK NO-ERROR.
        
        IF AVAIL am-cq-laudo THEN
            nr-laudo-jr = am-cq-laudo.nr-laudo + 1.

        REPEAT:

            FIND FIRST tt-am-cq-laudo WHERE
                 tt-am-cq-laudo.nr-laudo = nr-laudo-jr
                 NO-LOCK NO-ERROR.
            
            IF AVAIL tt-am-cq-laudo THEN
                ASSIGN nr-laudo-jr = nr-laudo-jr + 1.

            ELSE LEAVE.

        END.

    
        CREATE tt-am-cq-laudo.
        
        ASSIGN tt-am-cq-laudo.cod-estabel = tt-param.c-cod-estabel-ini 
               tt-am-cq-laudo.nr-nota-fis = tt-notas.nr-nota-fis
               tt-am-cq-laudo.dec-1       = tt-notas.nr-sequencia
               tt-am-cq-laudo.nr-laudo    = nr-laudo-jr.

        ASSIGN tt-am-cq-laudo.int-1       = tt-param.i-modelo
               tt-am-cq-laudo.serie       = tt-notas.serie.

    
        /* cabeáalho do excel */
    
         ASSIGN i-linha = 7.
    
         IF tp-pedido-jr = "A" THEN
            ASSIGN tt-am-cq-laudo.tipo-laudo = 2.
    
         IF tp-pedido-jr <> "A" THEN
            ASSIGN tt-am-cq-laudo.tipo-laudo = 1.
    
         ASSIGN i-linha = 82.
    


         FIND FIRST usuar_mestre WHERE
                usuar_mestre.cod_usuario = /*tt-param.usuario*/ "aschnei"
                NO-LOCK NO-ERROR.
            
         IF AVAIL usuar_mestre THEN
                ASSIGN tt-am-cq-laudo.nome-responsavel = usuar_mestre.nom_usuario
                       tt-am-cq-laudo.cod-usuario      = usuar_mestre.cod_usuario.

          FOR FIRST ext_usuar_grp_usuar WHERE 
                    ext_usuar_grp_usuar.ativo AND
                    ext_usuar_grp_usuar.cod_grp_usuar = "LAUDORESP"   NO-LOCK.


             FIND FIRST usuar_mestre WHERE
             usuar_mestre.cod_usuario = ext_usuar_grp_usuar.cod_usuario
             NO-LOCK NO-ERROR.
    
             IF AVAIL usuar_mestre THEN
                 ASSIGN tt-am-cq-laudo.nome-responsavel = usuar_mestre.nom_usuario
                        tt-am-cq-laudo.cod-usuario      = usuar_mestre.cod_usuario.
    
                     
    

          END.

    
         ASSIGN tt-am-cq-laudo.dt-criacao = data-jr.
         
         ASSIGN tt-am-cq-laudo.observacao = obs-ex.
    
         ASSIGN tt-am-cq-laudo.nome-abrev    = nome-abrev-jr
                tt-am-cq-laudo.cod-emitente  = cod-emitente-jr
                tt-am-cq-laudo.qtd-bobinas   = qtd-bobinas
                tt-am-cq-laudo.peso-liq      = soma-peso
                tt-am-cq-laudo.enviado-email = NO.
     
         ASSIGN tt-am-cq-laudo.it-codigo = it-codigo-ped.

         FIND FIRST polo-laudo-cliente WHERE
             polo-laudo-cliente.nome-abrev = nome-abrev-jr
             NO-LOCK NO-ERROR.

         IF AVAIL polo-laudo-cliente THEN 
             ASSIGN tt-am-cq-laudo.tipo-envio-email = 
                 int(substring(polo-laudo-cliente.char-1,1,1)) NO-ERROR.
         ELSE
             ASSIGN tt-am-cq-laudo.tipo-envio-email = 3.

         IF tt-am-cq-laudo.tipo-envio-email < 1 OR
            tt-am-cq-laudo.tipo-envio-email > 3 THEN
             ASSIGN tt-am-cq-laudo.tipo-envio-email = 3.

         ASSIGN conta-ped = 0
                pedido-x1 = ""
                pedido-x2 = ""
                nrnf-ant  = ""
                pedcli-ant  = ""
                larg-x1 = ""
                larg-x2 = ""
                diex-x1 = ""
                diex-x2 = ""
                diin-x1 = ""
                diin-x2 = ""
                pedcli-x1 = ""
                pedcli-x2 = ""
                qtdbob-x1 = ""
                qtdbob-x2 = ""
                zint-jr   = ""
                prodclie-jrx = "".

         FOR EACH tt-pedidos NO-LOCK.
    
             ASSIGN conta-ped = conta-ped + 1.

             ASSIGN pedido-x1 = (TRIM (pedido-x1) + " " + STRING (ttped-nr-pedido) + "(" + STRING(ttped-ems-pedcli) + ")").
    
             IF conta-ped < 12 THEN
                 ASSIGN larg-x1 = (TRIM (larg-x1) + " " + STRING (ttped-larg))
                        diin-x1 = (TRIM (diin-x1) + " " + STRING (ttped-diin))
                        diex-x1 = (TRIM (diex-x1) + " " + STRING (ttped-diex))
                        zint-jr = (TRIM (zint-jr) + " " + STRING (ttped-zint))
                        prodclie-jrx = (TRIM (prodclie-jrx) + " " + STRING (ttped-prodclie))
                        qtdbob-x1 = (TRIM (qtdbob-x1) + " " + STRING (ttped-qtdbob)).
                  ELSE
                      ASSIGN larg-x2 = (TRIM (larg-x2) + " " + STRING (ttped-larg))
                             diin-x2 = (TRIM (diin-x2) + " " + STRING (ttped-diin))
                             diex-x2 = (TRIM (diex-x2) + " " + STRING (ttped-diex))
                             zint-jr = (TRIM (zint-jr) + " " + STRING (ttped-zint))
                             prodclie-jrx = (TRIM (prodclie-jrx) + " " + STRING (ttped-prodclie))
                             qtdbob-x2 = (TRIM (qtdbob-x2) + " " + STRING (ttped-qtdbob)).
    
             IF conta-ped < 7 AND ttped-pedcli <> pedcli-ant THEN
                ASSIGN pedcli-x1 = (TRIM (pedcli-x1) + " " + STRING (ttped-pedcli))
                       pedcli-ant = ttped-pedcli.
                ELSE
                    IF conta-ped < 13 AND ttped-pedcli <> pedcli-ant THEN
                       ASSIGN pedcli-x2 = (TRIM (pedcli-x1) + " " + STRING (ttped-pedcli))
                              pedcli-ant = ttped-pedcli. 
    
         END.
    
             ASSIGN tt-am-cq-laudo.pedido = pedido-x1.
    
             ASSIGN tt-am-cq-laudo.larg = larg-x1 + " " + larg-x2.
    
             ASSIGN tt-am-cq-laudo.diex = diex-x1 + " " + diex-x2.

             ASSIGN tt-am-cq-laudo.zint = trim(prodclie-jrx).

             ASSIGN tt-am-cq-laudo.diin = diin-x1 + " " + diin-x2.
    
             ASSIGN tt-am-cq-laudo.pallet = pallet-x1 + " " + pallet-x2 + " " +
                                         pallet-x3 + " " + pallet-x4 + " " +
                                         pallet-x5 + " " + pallet-x6 + " " +
                                         pallet-x7 + " " + pallet-x8 + " " +
                                         pallet-x9 + " " + pallet-x10.
         
             ASSIGN tt-am-cq-laudo.pedido-cliente = pedcli-x1 + " " + pedcli-x2.
    
         ASSIGN i-linha = 49.
    
    END.


    
    FOR EACH tt-bobinas2 NO-LOCK:
        DELETE tt-bobinas2.
    END.
    
    FOR EACH tt-bobinas NO-LOCK:

           FIND tt-bobinas2 WHERE
              ttbob2-lote = ttbob-lote AND
              ttbob2-linha = ttbob-linha
           USE-INDEX ch-tt-bobinas2 NO-ERROR.
    
           IF NOT AVAIL tt-bobinas2 THEN DO:
              CREATE tt-bobinas2.
              ASSIGN ttbob2-lote = ttbob-lote
                ttbob2-linha = ttbob-linha
                ttbob2-it-codigo = ttbob-it-codigo
                ttbob2-nr-ord-produ = ttbob-nr-ord-produ
                ttbob2-dt-trans  = ttbob-dt-trans.
           END.
    
           IF ttbob-linha < 500 THEN DO:
           
              if tt-param.i-modelo <> 3 then do:
              
               find first tt-bobinas2 where tt-bobinas2.ttbob2-nr-ord-produ = ttbob-nr-ord-produ no-error.
               if not avail tt-bobinas2 then do:
              
                 FOR EACH movto-estoq NO-LOCK WHERE
                     movto-estoq.nr-ord-produ = ttbob-nr-ord-produ AND
                     movto-estoq.esp-docto = 1 AND
                     movto-estoq.cod-depos <> "ARC" 
                    /* USE-INDEX ord-seq*/ :
                     IF  CAN-FIND(FIRST rep-prod WHERE rep-prod.nr-reporte = movto-estoq.nr-reporte AND rep-prod.qt-estorno > 0 NO-LOCK) THEN NEXT. 
                     RUN grava-rastreabilidade-campanha.
                  
                 END.
               end.
                 
              
              end.
              
           END.

    END.
    
 END.  /*IF AVAIL PEDIDO*/

  
    /* Encontra a Uniformidade de Camadas das Bobinas Metalizadas */
    
    ASSIGN uniform-jr   = 0
           soma-bom-jr  = 0
           soma-ruim-jr = 0.

    
    FOR EACH tt-bobinas2:

    
       FIND FIRST lote-carac-tec WHERE
                      lote-carac-tec.it-codigo = ttbob2-it-codigo
                      and lote-carac-tec.lote =  ttbob2-lote     
                      and lote-carac-tec.cd-comp = "uniform"
                      NO-LOCK NO-ERROR.
      
       if avail lote-carac-tec then DO:
        
          FIND FIRST lote-res-carac
               where lote-res-carac.cd-folha = lote-carac-tec.cd-folha and
               lote-res-carac.it-codigo = lote-carac-tec.it-codigo and
               lote-res-carac.lote = lote-carac-tec.lote and
               lote-res-carac.nr-tabela = lote-carac-tec.nr-tabela and
               lote-res-carac.sequencia <> 0 and
               lote-res-carac.cd-comp = lote-carac-tec.cd-comp
               NO-LOCK NO-ERROR.
         
               IF AVAIL lote-res-carac THEN DO:

                   assign uniform-jr = lote-res-carac.sequencia.

                   IF uniform-jr = 10 THEN
                       ASSIGN soma-bom-jr = soma-bom-jr + 1.
                   ELSE
                       ASSIGN soma-ruim-jr = soma-ruim-jr + 1.

               END.

       END.

    END.

       IF uniform-jr > 0 THEN DO:

          IF soma-bom-jr >= soma-ruim-jr THEN
              ASSIGN uniform-jr = 10.
          ELSE
              ASSIGN uniform-jr = 20.


       END.

    
    /* Encontra a Ades∆o de Camadas das Bobinas Metalizadas */
    
    
    ASSIGN adesao-jr    = 0
           soma-bom-jr  = 0
           soma-ruim-jr = 0.
    
    FOR EACH tt-bobinas2 NO-LOCK:

       ASSIGN lote-rastrear      = ttbob2-lote 
              it-codigo-rastrear = ttbob2-it-codigo.

       RUN cpp/escp033.p (INPUT lote-rastrear,
                          INPUT it-codigo-rastrear) 
                          NO-ERROR.

       IF ERROR-STATUS:ERROR THEN DO:
          MESSAGE "Erro execuá∆o no programa externo"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
          NEXT.
       END.

       FOR EACH tt-rastrear NO-LOCK.

           FIND FIRST ord-prod WHERE
               ord-prod.nr-ord-produ = ttras-nr-ord-produ-cons AND
               ord-prod.nr-linha >= 300 AND
               ord-prod.nr-linha <= 399 
               USE-INDEX codigo
               NO-LOCK NO-ERROR.

           IF AVAIL ord-prod THEN DO:

             FIND FIRST lote-carac-tec WHERE
                            lote-carac-tec.it-codigo = ttras-it-codigo-cons
                            and lote-carac-tec.lote =  ttras-lote-cons     
                            and lote-carac-tec.cd-comp = "adesao"
                            NO-LOCK NO-ERROR.
            
             if avail lote-carac-tec then DO:
              
                FIND FIRST lote-res-carac
                     where lote-res-carac.cd-folha = lote-carac-tec.cd-folha and
                     lote-res-carac.it-codigo = lote-carac-tec.it-codigo and
                     lote-res-carac.lote = lote-carac-tec.lote and
                     lote-res-carac.nr-tabela = lote-carac-tec.nr-tabela and
                     lote-res-carac.sequencia <> 0 and
                     lote-res-carac.cd-comp = lote-carac-tec.cd-comp
                     NO-LOCK NO-ERROR.
               
                     IF AVAIL lote-res-carac THEN DO:
           
                         assign adesao-jr = lote-res-carac.sequencia.
           
                         IF adesao-jr = 10 THEN
                             ASSIGN soma-bom-jr = soma-bom-jr + 1.
                         ELSE
                             ASSIGN soma-ruim-jr = soma-ruim-jr + 1.
           
                     END.

             END.

           END.

       END.

       FIND FIRST lote-carac-tec WHERE
                      lote-carac-tec.it-codigo = ttbob2-it-codigo
                      and lote-carac-tec.lote =  ttbob2-lote     
                      and lote-carac-tec.cd-comp = "adesao"
                      NO-LOCK NO-ERROR.
      
       if avail lote-carac-tec then DO:
        
          FIND FIRST lote-res-carac
               where lote-res-carac.cd-folha = lote-carac-tec.cd-folha and
               lote-res-carac.it-codigo = lote-carac-tec.it-codigo and
               lote-res-carac.lote = lote-carac-tec.lote and
               lote-res-carac.nr-tabela = lote-carac-tec.nr-tabela and
               lote-res-carac.sequencia <> 0 and
               lote-res-carac.cd-comp = lote-carac-tec.cd-comp
               NO-LOCK NO-ERROR.
         
               IF AVAIL lote-res-carac THEN DO:

                   assign adesao-jr = lote-res-carac.sequencia.

                   IF adesao-jr = 10 THEN
                       ASSIGN soma-bom-jr = soma-bom-jr + 1.
                   ELSE
                       ASSIGN soma-ruim-jr = soma-ruim-jr + 1.

               END.

       END.

    END.

    IF adesao-jr > 0 THEN DO:

       IF soma-bom-jr >= soma-ruim-jr THEN
           ASSIGN adesao-jr = 10.
       ELSE
           ASSIGN adesao-jr = 20.


    END.

    
    /* Encontra a Densidade Otica das Bobinas Metalizadas */
    
    ASSIGN dens-ot-jr = 0
           soma-dens  = 0.
    
        FOR EACH tt-bobinas2 NO-LOCK:
    
            FIND FIRST lote-carac-tec WHERE
                 lote-carac-tec.it-codigo = ttbob2-it-codigo AND
                 lote-carac-tec.lote      = ttbob2-lote      AND
                 lote-carac-tec.cd-comp   = "d.omedia"
                 NO-LOCK NO-ERROR. 
    
            IF AVAIL lote-carac-tec THEN
    
                ASSIGN dens-ot-jr = dens-ot-jr + DEC(lote-carac-tec.vl-result)
                       soma-dens  = soma-dens + 1.
    
        END.
    /* Fim Densidade ‡tica */
    
    
    /* Monta gramatura e espessura calculadas dos mill rolls de origem
       para o estabelecimento 423 ou 413. */
    
    
         ASSIGN tot-peso-jr1  = 0
                tot-fator-jr1 = 0
                tot-min-jr = 99999999
                tot-max-jr = 0
                gra-min-jr = 99999999
                gra-max-jr = 0.
    
         IF c-cod-estabel-ini = STRING({cdp\poloestab.i 423}) THEN DO: /*solic-318*/ 
    
          FOR EACH tt-bobinas2 NO-LOCK:
    
           IF (INDEX (ttbob2-it-codigo,"MR",1) <> 0) THEN DO:
                            
            ASSIGN larg-jr1 = 0 
                   comp-jr1 = 0.
    
            FIND lote-carac-tec WHERE
              lote-carac-tec.it-codigo = ttbob2-it-codigo AND
              lote-carac-tec.lote = ttbob2-lote AND
              lote-carac-tec.cd-comp = "largura" 
              NO-LOCK NO-ERROR.
    
            IF AVAIL lote-carac-tec AND lote-carac-tec.vl-resul > 0 THEN
               ASSIGN larg-jr1 = lote-carac-tec.vl-resul.
    
            FIND lote-carac-tec WHERE
              lote-carac-tec.it-codigo = ttbob2-it-codigo AND
              lote-carac-tec.lote = ttbob2-lote AND
              lote-carac-tec.cd-comp = "compr" 
              NO-LOCK NO-ERROR.
    
            IF AVAIL lote-carac-tec AND lote-carac-tec.vl-resul > 0 THEN
               ASSIGN comp-jr1 = lote-carac-tec.vl-resul.   
            
            FIND LAST movto-mat 
             where 
                   movto-mat.esp-docto = 1 AND
                   movto-mat.it-codigo = ttbob2-it-codigo and
                   movto-mat.lote      = ttbob2-lote AND 
                   movto-mat.cod-depos <> "arc" AND 
                   NOT CAN-FIND(FIRST rep-prod WHERE rep-prod.nr-reporte = movto-mat.nr-reporte AND rep-prod.qt-estorno > 0 NO-LOCK)
                USE-INDEX lote
                   NO-LOCK  NO-ERROR.
    
            IF NOT AVAIL movto-mat THEN next.
                            
            ASSIGN peso-jr1 = movto-mat.quantidade.
    
            IF larg-jr1 = 0 OR comp-jr1 = 0 OR peso-jr1 = 0 THEN NEXT.
    
            ASSIGN tot-peso-jr1 = tot-peso-jr1 + peso-jr1
                   tot-fator-jr1 = tot-fator-jr1 + (comp-jr1 * larg-jr1).
    
            ASSIGN esp-min-jr = (peso-jr1 / (comp-jr1 * larg-jr1)) * 1000000 .
    
            IF esp-min-jr < gra-min-jr THEN
                ASSIGN gra-min-jr = esp-min-jr.
                   
            IF esp-min-jr > gra-max-jr THEN
                ASSIGN gra-max-jr = esp-min-jr.
    
            IF fm-codigo-jr = "BSA" OR fm-codigo-jr = "BST" OR 
               fm-codigo-jr = "BPX" THEN                             
                ASSIGN densidade-jr = 0.913.                         
                 ELSE                                                
                   ASSIGN densidade-jr = 0.905.

    
            ASSIGN esp-min-jr = (esp-min-jr / densidade-jr).
                    
            IF esp-min-jr < tot-min-jr THEN
                ASSIGN tot-min-jr = esp-min-jr.
                   
            IF esp-min-jr > tot-max-jr THEN
                ASSIGN tot-max-jr = esp-min-jr.
    
            ASSIGN esp-min-jr = 0. 
    
           END.
          END.
         END.

         
    /*----------------------------------------------------------------*/
    FOR EACH tt-analises :
        DELETE tt-analises.
    END.
    
    for each tt-analises-mr.
        delete tt-analises-mr.
    end.    


    /* Verifica e Busca an†lises existentes na tabela espec°fica
       am-cq-analise-pm (lotes da virada para o ASP */

    FOR EACH tt-bobinas2 NO-LOCK.


        FOR EACH am-cq-analise-pm WHERE
            am-cq-analise-pm.nr-pedido   = 0                       AND
            am-cq-analise-pm.nr-pallet   = tt-bobinas2.ttbob2-lote AND
            am-cq-analise-pm.lote-bobina = tt-bobinas2.ttbob2-lote 
            NO-LOCK.

                    
            FIND FIRST tt-analises WHERE
                ttana-cod-exame = am-cq-analise-pm.cod-exame AND
                ttana-cod-comp  = am-cq-analise-pm.cod-comp
                NO-ERROR.

            IF NOT AVAIL tt-analises THEN DO:
                CREATE tt-analises.
                ASSIGN ttana-cod-comp  = am-cq-analise-pm.cod-comp
                       ttana-cod-exame = am-cq-analise-pm.cod-exame.
            END.
            
            ASSIGN ttana-it-codigo = tt-bobinas2.ttbob2-it-codigo
                   RESULT-jr = DEC (am-cq-analise-pm.media-result)
                   ttana-result = ttana-result + RESULT-jr
                   ttana-qtd-result = ttana-qtd-result + 1
                   ttana-amostra[ttana-qtd-result] = RESULT-jr
                   minimo-jr = ttana-minimo
                   maximo-jr = ttana-maximo.

            IF minimo-jr = 0 THEN
                ASSIGN minimo-jr = 999999999.
            
            IF RESULT-jr < minimo-jr THEN
               ASSIGN minimo-jr = RESULT-jr.
            
            IF RESULT-jr > maximo-jr THEN
               ASSIGN maximo-jr = RESULT-jr.
            
            ASSIGN ttana-minimo = minimo-jr
                   ttana-maximo = maximo-jr.

            /* Rotina para gravar apenas uma mÇdia por lote */                        
 
            FIND FIRST tt-analises-mr WHERE
                tt-analises-mr.nr-lote      = tt-bobinas2.ttbob2-lote          AND
                tt-analises-mr.cod-exame    = am-cq-analise-pm.cod-exame       AND
                tt-analises-mr.cod-comp     = am-cq-analise-pm.cod-comp  
                NO-ERROR.
            
             if not avail tt-analises-mr then do:
            
                create tt-analises-mr.
                assign tt-analises-mr.nr-lote      = tt-bobinas2.ttbob2-lote               
                       tt-analises-mr.cod-exame    = am-cq-analise-pm.cod-exame     
                       tt-analises-mr.cod-comp     = am-cq-analise-pm.cod-comp.  
                  
             end.
            
             assign RESULT-jr = DEC (am-cq-analise-pm.media-result)
                    tt-analises-mr.qtd-result   = tt-analises-mr.qtd-result + 1
                    tt-analises-mr.soma-result  = tt-analises-mr.soma-result + RESULT-jr
                    tt-analises-mr.media-result = tt-analises-mr.soma-result / tt-analises-mr.qtd-result.
                   

        END.  /* FOR EACH am-cq-analise-pm */


    END.   /* FOR EACH tt-bobinas2 */


   /* ---------------------------------------------------------------*/

    
    FOR EACH tt-bobinas2 :

       /* Busca An†lises nas bobinas cortadas e metalizadas */

         IF tt-bobinas2.ttbob2-linha >= 00 THEN DO:  
         
          FOR EACH ficha-cq NO-LOCK WHERE
                   ficha-cq.situacao < 5 AND 
                   ficha-cq.it-codigo = tt-bobinas2.ttbob2-it-codigo AND  
                   ficha-cq.lote      = tt-bobinas2.ttbob2-lote AND
                   ficha-cq.cod-estabel = STRING({cdp\poloestab.i 422})                 AND /*solic-318*/ 
                   ficha-cq.cod-depos   = "CQ"  AND
                   ficha-cq.cod-localiz = ""
                  USE-INDEX it-lote :

             FOR EACH pol-res-fic-cq-leitura NO-LOCK WHERE
                 pol-res-fic-cq-leitura.nr-ficha = ficha-cq.nr-ficha :
                 
                 IF DEC(pol-res-fic-cq-leitura.resultado)  = 0 THEN NEXT.                 
                    
                 FIND FIRST tt-analises WHERE
                     ttana-cod-exame = pol-res-fic-cq-leitura.cod-exame AND
                     ttana-cod-comp  = pol-res-fic-cq-leitura.cod-comp
                     NO-ERROR.

                 IF NOT AVAIL tt-analises THEN DO:
                     CREATE tt-analises.
                     ASSIGN ttana-cod-comp  = pol-res-fic-cq-leitura.cod-comp
                            ttana-cod-exame = pol-res-fic-cq-leitura.cod-exame.
                 END.
                 
                 ASSIGN ttana-it-codigo = pol-res-fic-cq-leitura.it-codigo
                        RESULT-jr = DEC (pol-res-fic-cq-leitura.resultado)                        
                        ttana-qtd-result = ttana-qtd-result + 1
                        ttana-result = ttana-result + RESULT-jr
                        ttana-amostras[ttana-qtd-result] = RESULT-jr
                        minimo-jr = ttana-minimo
                        maximo-jr = ttana-maximo.

                 IF minimo-jr = 0 THEN
                     ASSIGN minimo-jr = 999999999.

                 IF RESULT-jr < minimo-jr THEN
                    ASSIGN minimo-jr = RESULT-jr.

                 IF RESULT-jr > maximo-jr THEN
                    ASSIGN maximo-jr = RESULT-jr.

                 ASSIGN ttana-minimo = minimo-jr
                        ttana-maximo = maximo-jr.

                /* Rotina para gravar apenas uma mÇdia por lote */                        
 
                FIND FIRST tt-analises-mr WHERE
                    tt-analises-mr.nr-lote      = tt-bobinas2.ttbob2-lote          AND
                    tt-analises-mr.cod-exame    = pol-res-fic-cq-leitura.cod-exame AND
                    tt-analises-mr.cod-comp     = pol-res-fic-cq-leitura.cod-comp
                    NO-ERROR.
               
                 if not avail tt-analises-mr then do:
            
                    create tt-analises-mr.
                    assign tt-analises-mr.nr-lote      = tt-bobinas2.ttbob2-lote               
                           tt-analises-mr.cod-exame    = pol-res-fic-cq-leitura.cod-exame    
                           tt-analises-mr.cod-comp     = pol-res-fic-cq-leitura.cod-comp.
                      
                 end.
            
                 assign RESULT-jr = DEC (pol-res-fic-cq-leitura.resultado)
                        tt-analises-mr.qtd-result   = tt-analises-mr.qtd-result + 1
                        tt-analises-mr.soma-result  = tt-analises-mr.soma-result + RESULT-jr
                        tt-analises-mr.media-result = tt-analises-mr.soma-result / tt-analises-mr.qtd-result.
                   
                   
                /* fim da Rotina para gravar apenas uma mÇdia por lote */                
                        
                  
             END.

          END.

       END.
   
      /* Fim da Busca An†lises nas bobinas cortadas e metalizadas */

       IF tt-bobinas2.ttbob2-linha > 99 THEN NEXT.

       FIND FIRST am-cq-analise WHERE
           am-cq-analise.nr-ord-produ = ttbob2-nr-ord-produ
           NO-LOCK NO-ERROR.

       IF AVAIL am-cq-analise THEN DO:
    
           FIND FIRST usuar_mestre WHERE
             usuar_mestre.cod_usuario = am-cq-analise.cod-resp
             NO-LOCK NO-ERROR.
    
           IF AVAIL usuar_mestre THEN
             ASSIGN tt-am-cq-laudo.nome-responsavel = usuar_mestre.nom_usuario
                    tt-am-cq-laudo.cod-usuario      = usuar_mestre.cod_usuario
                    c-nome-responsavel              = usuar_mestre.nom_usuario
                    c-email-responsavel             = usuar_mestre.cod_e_mail_local.

       END.

       
    END.



    /* Procurar an†lises digitadas em lotes (mill rolls por ordem */

    FOR EACH tt-bobinas2 :

/*
       IF tt-bobinas2.ttbob2-linha > 99 THEN NEXT.
*/

/*
       FIND FIRST lote-prod WHERE
           lote-prod.lote = tt-bobinas2.ttbob2-lote
           NO-LOCK NO-ERROR.

       IF NOT AVAIL lote-prod THEN NEXT.
*/

       FIND FIRST am-cq-analise WHERE
           am-cq-analise.nr-ord-produ = tt-bobinas2.ttbob2-nr-ord-produ AND
           am-cq-analise.nr-lote      = tt-bobinas2.ttbob2-lote 
           NO-LOCK NO-ERROR.

       IF NOT AVAIL am-cq-analise THEN
           FIND FIRST am-cq-analise WHERE
               am-cq-analise.nr-ord-produ = 0 AND
               am-cq-analise.nr-lote      = tt-bobinas2.ttbob2-lote 
               NO-LOCK NO-ERROR.

       IF NOT AVAIL am-cq-analise THEN
           FIND FIRST am-cq-analise WHERE
               am-cq-analise.nr-ord-produ = tt-bobinas2.ttbob2-nr-ord-produ AND
               am-cq-analise.nr-lote      = "" 
               NO-LOCK NO-ERROR.

       IF AVAIL am-cq-analise THEN DO:
    
           FOR EACH am-cq-result-analise WHERE 
               am-cq-result-analise.nr-ord-produ = am-cq-analise.nr-ord-produ /* AND
               am-cq-result-analise.nr-lote      = am-cq-analise.nr-lote  */
               NO-LOCK.
        
               FIND FIRST tt-analises WHERE
                   ttana-cod-exame = am-cq-result-analise.cod-exame AND
                   ttana-cod-comp  = am-cq-result-analise.cod-comp
                   NO-ERROR.
        
               IF NOT AVAIL tt-analises THEN DO:
                   CREATE tt-analises.
                   ASSIGN ttana-cod-comp  = am-cq-result-analise.cod-comp
                          ttana-cod-exame = am-cq-result-analise.cod-exame.
               END.
               
        
               /* Alterado para a an†lise digitada pelo ESCQ053 sobreponha qualquer 
                  outro resultado j† enconcontrado */

               /*

               ASSIGN ttana-it-codigo = tt-bobinas2.ttbob2-it-codigo
                      RESULT-jr = DEC (am-cq-result-analise.media-result)
                      ttana-result = ttana-result + RESULT-jr
                      ttana-qtd-result = ttana-qtd-result + 1
                      ttana-amostra[ttana-qtd-result] = RESULT-jr
                      minimo-jr = ttana-minimo
                      maximo-jr = ttana-maximo.
        
               */

               ASSIGN ttana-it-codigo = tt-bobinas2.ttbob2-it-codigo
                      RESULT-jr = DEC (am-cq-result-analise.media-result)
                      ttana-result = RESULT-jr
                      ttana-qtd-result = 1
                      ttana-amostra[ttana-qtd-result] = RESULT-jr
                      minimo-jr = ttana-minimo
                      maximo-jr = ttana-maximo.

               /* fim da alteraá∆o para a an†lise digitada pelo ESCQ053 sobreponha qualquer 
                  outro resultado j† enconcontrado */
        
               IF minimo-jr = 0 THEN
                   ASSIGN minimo-jr = 999999999.
        
               IF RESULT-jr < minimo-jr THEN
                  ASSIGN minimo-jr = RESULT-jr.
        
               IF RESULT-jr > maximo-jr THEN
                  ASSIGN maximo-jr = RESULT-jr.
        
               ASSIGN ttana-minimo = DEC (am-cq-result-analise.menor-result)
                      ttana-maximo = DEC (am-cq-result-analise.maior-result).

               
                          
           END.

       END.

    END.

    /* Fim da Procura an†lises digitadas em lotes/ordem de produá∆o */




    /* Procurar an†lises digitadas em lotes (pallets) */

    FOR EACH tt-digita2.

       FIND FIRST am-cq-analise WHERE
           am-cq-analise.nr-ord-produ = 0    AND
           am-cq-analise.nr-lote      = tt-digita2.nr-pallet
           NO-LOCK NO-ERROR.

       IF AVAIL am-cq-analise THEN DO:
    
           FIND FIRST usuar_mestre WHERE
             usuar_mestre.cod_usuario = am-cq-analise.cod-resp
             NO-LOCK NO-ERROR.
    
           IF AVAIL usuar_mestre THEN
             ASSIGN tt-am-cq-laudo.nome-responsavel = usuar_mestre.nom_usuario
                    tt-am-cq-laudo.cod-usuario      = usuar_mestre.cod_usuario
                    c-nome-responsavel              = usuar_mestre.nom_usuario
                    c-email-responsavel             = usuar_mestre.cod_e_mail_local.

       END.

       IF AVAIL am-cq-analise THEN DO:

           FOR EACH am-cq-result-analise WHERE 
               am-cq-result-analise.nr-ord-produ = am-cq-analise.nr-ord-produ AND
               am-cq-result-analise.nr-lote      = am-cq-analise.nr-lote
               NO-LOCK.
        
               FIND FIRST tt-analises WHERE
                   ttana-cod-exame = am-cq-result-analise.cod-exame AND
                   ttana-cod-comp  = am-cq-result-analise.cod-comp
                   NO-ERROR.
        
               IF NOT AVAIL tt-analises THEN DO:
                   CREATE tt-analises.
                   ASSIGN ttana-cod-comp  = am-cq-result-analise.cod-comp
                          ttana-cod-exame = am-cq-result-analise.cod-exame.
               END.
               
               /* Alterado para a an†lise digitada pelo ESCQ053 sobreponha qualquer 
                  outro resultado j† enconcontrado */

               /*
               ASSIGN ttana-it-codigo = tt-digita2.it-codigo
                      RESULT-jr = DEC (am-cq-result-analise.media-result)
                      ttana-result = ttana-result + RESULT-jr
                      ttana-qtd-result = ttana-qtd-result + 1
                      ttana-amostra[ttana-qtd-result] = RESULT-jr
                      minimo-jr = ttana-minimo
                      maximo-jr = ttana-maximo.
               */
        
               ASSIGN ttana-it-codigo = tt-digita2.it-codigo
                      RESULT-jr = DEC (am-cq-result-analise.media-result)
                      ttana-result = RESULT-jr
                      ttana-qtd-result = 1
                      ttana-amostra[ttana-qtd-result] = RESULT-jr
                      minimo-jr = ttana-minimo
                      maximo-jr = ttana-maximo.

               /* fim da alteraá∆o para a an†lise digitada pelo ESCQ053 sobreponha qualquer 
                  outro resultado j† enconcontrado */
        
               IF minimo-jr = 0 THEN
                   ASSIGN minimo-jr = 999999999.
        
               IF RESULT-jr < minimo-jr THEN
                  ASSIGN minimo-jr = RESULT-jr.
        
               IF RESULT-jr > maximo-jr THEN
                  ASSIGN maximo-jr = RESULT-jr.
        
               ASSIGN ttana-minimo = DEC (am-cq-result-analise.menor-result)
                      ttana-maximo = DEC (am-cq-result-analise.maior-result).

 
                          
           END.

       END.

    END.

    /* Fim da Procura an†lises digitadas em lotes (pallets) */

    
    /* Rotina para ficar apenas com analises digitadas no especifico
       para os laudo Philip Morris */

    IF tt-param.i-modelo = 3 THEN DO:

        FOR EACH tt-analises.
            DELETE tt-analises.
        END.

        FOR EACH tt-digita2.

            FOR EACH am-cq-analise-pm WHERE
                am-cq-analise-pm.nr-pedido = tt-digita2.nr-pedido AND
                am-cq-analise-pm.nr-pallet = tt-digita2.nr-pallet 
                NO-LOCK.
        
               FIND FIRST tt-analises WHERE
                   ttana-cod-exame = am-cq-analise-pm.cod-exame AND
                   ttana-cod-comp  = am-cq-analise-pm.cod-comp
                   NO-ERROR.
        
               IF NOT AVAIL tt-analises THEN DO:
                   CREATE tt-analises.
                   ASSIGN ttana-cod-comp  = am-cq-analise-pm.cod-comp
                          ttana-cod-exame = am-cq-analise-pm.cod-exame.
               END.                                         
        
               ASSIGN ttana-it-codigo = tt-digita2.it-codigo
                      RESULT-jr = DEC (am-cq-analise-pm.media-result)
                      ttana-result = ttana-result + RESULT-jr
                      ttana-qtd-result = ttana-qtd-result + 1
                      ttana-amostra[ttana-qtd-result] = RESULT-jr
                      minimo-jr = ttana-minimo
                      maximo-jr = ttana-maximo.
        
               IF minimo-jr = 0 THEN
                   ASSIGN minimo-jr = 999999999.
        
               IF RESULT-jr < minimo-jr THEN
                  ASSIGN minimo-jr = RESULT-jr.
        
               IF RESULT-jr > maximo-jr THEN
                  ASSIGN maximo-jr = RESULT-jr.
        
               ASSIGN ttana-minimo = minimo-jr
                      ttana-maximo = maximo-jr.
                          
            END.


            FOR FIRST if-ped-venda WHERE if-ped-venda.nr-pedido-relac = tt-digita2.nr-pedido NO-LOCK,

           EACH am-cq-analise-pm WHERE
             am-cq-analise-pm.nr-pedido = if-ped-venda.nr-pedido AND
             am-cq-analise-pm.nr-pallet = tt-digita2.nr-pallet 
             NO-LOCK.

            FIND FIRST tt-analises WHERE
                ttana-cod-exame = am-cq-analise-pm.cod-exame AND
                ttana-cod-comp  = am-cq-analise-pm.cod-comp
                NO-ERROR.

            IF NOT AVAIL tt-analises THEN DO:
                CREATE tt-analises.
                ASSIGN ttana-cod-comp  = am-cq-analise-pm.cod-comp
                       ttana-cod-exame = am-cq-analise-pm.cod-exame.
            END.                                         

            ASSIGN ttana-it-codigo = tt-digita2.it-codigo
                   RESULT-jr = DEC (am-cq-analise-pm.media-result)
                   ttana-result = ttana-result + RESULT-jr
                   ttana-qtd-result = ttana-qtd-result + 1
                   ttana-amostra[ttana-qtd-result] = RESULT-jr
                   minimo-jr = ttana-minimo
                   maximo-jr = ttana-maximo.

            IF minimo-jr = 0 THEN
                ASSIGN minimo-jr = 999999999.

            IF RESULT-jr < minimo-jr THEN
               ASSIGN minimo-jr = RESULT-jr.

            IF RESULT-jr > maximo-jr THEN
               ASSIGN maximo-jr = RESULT-jr.

            ASSIGN ttana-minimo = minimo-jr
                   ttana-maximo = maximo-jr.

         END.



        END.
    END. 

     
    ASSIGN tem-minimo = "N"
           tem-maximo = "N".
    
    FOR EACH tt-analises.

    /* Rotina para acertar qtd de analises, mÇdia e desvio padrao */
    

       /* vou tirar esta rotina - 16/02/2012 - JR 
       
        IF tt-param.i-modelo <> 3 THEN DO:

            assign soma-result-jr = 0
                   qtde-result-jr = 0
                   amostras-jr    = 0
                   i-jr           = 0.
    

            for each tt-analises-mr where
                tt-analises-mr.cod-exame = ttana-cod-exame and
                tt-analises-mr.cod-comp  = ttana-cod-comp no-lock.
                
                assign soma-result-jr = soma-result-jr + tt-analises-mr.media-result
                       qtde-result-jr = qtde-result-jr + 1
                       amostras-jr [qtde-result-jr] = tt-analises-mr.media-result no-error.
            
            end.    
        
            if soma-result-jr > 0 and qtde-result-jr > 0 then do:
            
               assign ttana-result    = soma-result-jr 
                      ttana-qtd-resul = qtde-result-jr
                      ttana-amostra   = 0.
                      
               assign i-jr = 1.
               
                  do while i-jr <= ttana-qtd-resul and i-jr < 9000:
                     
                     if  amostras-jr[i-jr] > 0  then 
                         assign ttana-amostra[i-jr]   = amostras-jr[i-jr]
                                i-jr = i-jr + 1.
                         
                  end.       
                      
            end. 

        END.

        */

        ASSIGN sai-laudo = no.
    
        ASSIGN media-result-jr = ttana-result / ttana-qtd-result.

        ASSIGN desv-padrao-jr = 0.

        IF tt-param.i-modelo = 3 and
           ttana-qtd-result > 1  THEN DO:  /*Calcular Desvio Padr∆o P.Morris*/

           ASSIGN i-jr = 1
                  soma-dp = 0.
           
           DO WHILE i-jr <= ttana-qtd-result AND i-jr < 9000.
           
           
               ASSIGN resul-dp = ttana-amostras [i-jr] - media-result-jr
                      resul-dp = resul-dp * resul-dp.
           
           
               ASSIGN soma-dp = soma-dp + resul-dp.
           
               ASSIGN i-jr = i-jr + 1.
           
           END.
           
           ASSIGN i-jr = ttana-qtd-result - 1
                  soma-dp = soma-dp / i-jr.
           
           ASSIGN desv-padrao-jr = SQRT(soma-dp).


        END. /*Fim de Calcular Desvio Padr∆o P.Morris*/



        IF  (ttana-cod-exame = 2006 AND ttana-cod-comp  = 2) OR
            (ttana-cod-exame = 2006 AND ttana-cod-comp  = 3) THEN DO:

            ASSIGN media-trab = media-result-jr / 2.

            IF int(media-trab) >= 1 THEN
               ASSIGN media-result-jr = int(media-trab) * 2.

        END.
    
        ASSIGN descricao-comp = ttana-it-codigo
               tipo-result = "Aprovado"
               unidade-jr = " "
               metodo-jr = " "
               valor-tipico-jr = 0
               decimais-jr = 2.
    
        FIND FIRST polo-esp-cliente-cq WHERE
             polo-esp-cliente-cq.nome-abrev  = nome-abrev-jr  AND 
             polo-esp-cliente-cq.it-codigo = it-codigo-ped    AND
             polo-esp-cliente-cq.cod-exame = ttana-cod-exame  AND
             polo-esp-cliente-cq.cod-comp  = ttana-cod-comp
             NO-LOCK NO-ERROR. 
    
        IF AVAIL polo-esp-cliente-cq  AND polo-esp-cliente-cq.log-imprime-laudo = YES 
           THEN 
            ASSIGN descricao-comp = polo-esp-cliente-cq.descricao
                   esp-min-jr     = polo-esp-cliente-cq.espec-min
                   esp-max-jr     = polo-esp-cliente-cq.espec-max
                   valor-tipico-jr = polo-esp-cliente-cq.espec-alvo
                   unidade-jr     = polo-esp-cliente-cq.unidade
                   decimais-jr    = polo-esp-cliente-cq.nr-decimais
                   metodo-jr      = polo-esp-cliente-cq.metodo
                   sai-laudo      = yes.
        
        IF sai-laudo = no THEN DO:
          ASSIGN nome-estabel-jr = "POLO RS".
    
    
          FIND FIRST polo-esp-cliente-cq WHERE
             polo-esp-cliente-cq.nome-abrev  = nome-estabel-jr  AND 
             polo-esp-cliente-cq.it-codigo = it-codigo-ped      AND
             polo-esp-cliente-cq.cod-exame = ttana-cod-exame    AND
             polo-esp-cliente-cq.cod-comp  = ttana-cod-comp
             NO-LOCK NO-ERROR. 
    
          IF AVAIL polo-esp-cliente-cq  AND polo-esp-cliente-cq.log-imprime-laudo = YES 
             THEN 
              ASSIGN descricao-comp = polo-esp-cliente-cq.descricao
                   esp-min-jr     = polo-esp-cliente-cq.espec-min
                   esp-max-jr     = polo-esp-cliente-cq.espec-max
                   valor-tipico-jr = polo-esp-cliente-cq.espec-alvo
                   unidade-jr     = polo-esp-cliente-cq.unidade
                   metodo-jr      = polo-esp-cliente-cq.metodo
                   decimais-jr    = polo-esp-cliente-cq.nr-decimais
                   sai-laudo      = yes.
    
    
        END.
    
        IF sai-laudo = yes THEN DO:
        
    
         DO:

             CREATE  tt-am-cq-result-laudo.

             ASSIGN gravou-laudo = 1.

             ASSIGN tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel
                    tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis
                    tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo.

          ASSIGN i-linha = i-linha + 1.
    
           ASSIGN tt-am-cq-result-laudo.descricao = descricao-comp.
    
           ASSIGN tt-am-cq-result-laudo.media-result = media-result-jr.
    
           ASSIGN media-jr2    = media-result-jr
                  media-result-jr = valor-tipico-jr.
    
           ASSIGN tt-am-cq-result-laudo.espec-alvo = valor-tipico-jr.
    
           IF unidade-jr = "g/m2" THEN unidade-jr = "g/m˝".
       
           ASSIGN tt-am-cq-result-laudo.unidade      = unidade-jr
                  tt-am-cq-result-laudo.metodo       = metodo-jr
                  tt-am-cq-result-laudo.nr-decimais  = decimais-jr
                  tt-am-cq-result-laudo.cod-exame    = ttana-cod-exame
                  tt-am-cq-result-laudo.cod-comp     = ttana-cod-comp 
                  tt-am-cq-result-laudo.soma-resul   = ttana-result       
                  tt-am-cq-result-laudo.qtd-result   = ttana-qtd-result   
                  tt-am-cq-result-laudo.menor-result = ttana-minimo       
                  tt-am-cq-result-laudo.maior-result = ttana-maximo       
                  tt-am-cq-result-laudo.espec-min    = ttana-esp-min      
                  tt-am-cq-result-laudo.espec-max    = ttana-esp-max.
                  
            

           IF tt-param.i-modelo = 3 THEN
               ASSIGN tt-am-cq-result-laudo.espec-max = desv-padrao-jr.
    

         END.

        end.

    END.

      
    
    /* Rotina para Gerar na Planilha as Uniformidades de Camadas das Bobinas Metalizadas */
    
    IF uniform-jr > 0 AND tt-param.i-modelo <> 3 THEN DO:
    
        FIND FIRST polo-esp-cliente-cq WHERE
             polo-esp-cliente-cq.nome-abrev  = nome-abrev-jr  AND 
             polo-esp-cliente-cq.it-codigo = it-codigo-ped    AND
             polo-esp-cliente-cq.cod-exame = 2006             AND
             polo-esp-cliente-cq.cod-comp  = 73
             NO-LOCK NO-ERROR. 
    
        IF AVAIL polo-esp-cliente-cq  AND polo-esp-cliente-cq.log-imprime-laudo = YES AND
           tt-param.destino = 4 THEN DO: 
        
            ASSIGN i-linha = i-linha + 1.
            
            FIND FIRST tt-am-cq-result-laudo WHERE
                tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel AND
                tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis AND
                tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    AND
                tt-am-cq-result-laudo.cod-exame   = 2006                       AND
                tt-am-cq-result-laudo.cod-comp    = 73
                EXCLUSIVE-LOCK NO-ERROR.  

            IF NOT AVAIL tt-am-cq-result-laudo THEN DO:
                CREATE tt-am-cq-result-laudo.
                ASSIGN tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel 
                       tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis 
                       tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    
                       tt-am-cq-result-laudo.cod-exame   = 2006                    
                       tt-am-cq-result-laudo.cod-comp    = 73.                 
            END.

            ASSIGN tt-am-cq-result-laudo.descricao   = "Uniformidade de Camada Metalizada"
                   tt-am-cq-result-laudo.media-resul = uniform-jr
                   tt-am-cq-result-laudo.unidade     = " - "
                   tt-am-cq-result-laudo.metodo      = " - ".
                   
       END.
    
    END.
    
     
    
    /* Rotina para Gerar na Planilha a Ades∆o de Metal das Bobinas Metalizadas */
    
    IF adesao-jr > 0 AND tt-param.i-modelo <> 3 THEN DO:
    
        FIND FIRST polo-esp-cliente-cq WHERE
             polo-esp-cliente-cq.nome-abrev  = nome-abrev-jr  AND 
             polo-esp-cliente-cq.it-codigo = it-codigo-ped    AND
             polo-esp-cliente-cq.cod-exame = 2006             AND
             polo-esp-cliente-cq.cod-comp  = 74
             NO-LOCK NO-ERROR. 
    
        IF AVAIL polo-esp-cliente-cq  AND polo-esp-cliente-cq.log-imprime-laudo = YES AND
           tt-param.destino = 4 THEN DO: 
        
            ASSIGN i-linha = i-linha + 1.
             
            FIND FIRST tt-am-cq-result-laudo WHERE
                tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel AND
                tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis AND
                tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    AND
                tt-am-cq-result-laudo.cod-exame   = 2006                    AND
                tt-am-cq-result-laudo.cod-comp    = 74
                EXCLUSIVE-LOCK NO-ERROR.

            IF NOT AVAIL tt-am-cq-result-laudo THEN DO:
                CREATE tt-am-cq-result-laudo.
                ASSIGN tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel 
                       tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis 
                       tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    
                       tt-am-cq-result-laudo.cod-exame   = 2006                    
                       tt-am-cq-result-laudo.cod-comp    = 74.                 
            END.

            ASSIGN tt-am-cq-result-laudo.descricao   = "Ades∆o Metal/Metal Strenght"
                   tt-am-cq-result-laudo.media-resul = adesao-jr
                   tt-am-cq-result-laudo.unidade     = " - "
                   tt-am-cq-result-laudo.metodo      = " - ".
                   
       END.
    
    END.
    
    /* Rotina para Gerar na Planilha as MÇdias das Densidades Oticas das Bobinas Metalizadas */
    
    IF dens-ot-jr > 0 AND tt-param.i-modelo <> 3 THEN DO:
    
        ASSIGN dens-ot-jr = dens-ot-jr / soma-dens.
    
        FIND FIRST polo-esp-cliente-cq WHERE
             polo-esp-cliente-cq.nome-abrev  = nome-abrev-jr  AND 
             polo-esp-cliente-cq.it-codigo = it-codigo-ped    AND
             polo-esp-cliente-cq.cod-exame = 2006             AND
             polo-esp-cliente-cq.cod-comp  = 36
             NO-LOCK NO-ERROR. 
             
        if not avail polo-esp-cliente-cq then
           FIND FIRST polo-esp-cliente-cq WHERE
             polo-esp-cliente-cq.nome-abrev  = "POLO RS"   AND 
             polo-esp-cliente-cq.it-codigo = it-codigo-ped    AND
             polo-esp-cliente-cq.cod-exame = 2006             AND
             polo-esp-cliente-cq.cod-comp  = 36
             NO-LOCK NO-ERROR. 
        
        IF AVAIL polo-esp-cliente-cq  AND polo-esp-cliente-cq.log-imprime-laudo = YES AND
           tt-param.destino = 4 THEN DO:            
        
            ASSIGN i-linha = i-linha + 1.
            
            ASSIGN media-result-jr-x = STRING (dens-ot-jr,">>>>>>>>9.99").
             
            FIND FIRST tt-am-cq-result-laudo WHERE
                tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel AND
                tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis AND
                tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    AND
                tt-am-cq-result-laudo.cod-exame   = 2006                    AND
                tt-am-cq-result-laudo.cod-comp    = 36
                EXCLUSIVE-LOCK NO-ERROR.
             
            IF NOT AVAIL tt-am-cq-result-laudo THEN DO:
                CREATE tt-am-cq-result-laudo.
                ASSIGN tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel 
                       tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis 
                       tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    
                       tt-am-cq-result-laudo.cod-exame   = 2006                    
                       tt-am-cq-result-laudo.cod-comp    = 36.                 
            END.

            ASSIGN tt-am-cq-result-laudo.descricao   = "Densidade ‡tica/Optical Density"
                   tt-am-cq-result-laudo.media-resul = dens-ot-jr
                   tt-am-cq-result-laudo.espec-alvo  = polo-esp-cliente-cq.espec-alvo.
   
                   
       END.
    
    END.
    
    /*-----------------------------------------------------------------------*/
 
    
    /* Rotina para montar descriá∆o Empax nas observaá‰es do Laudo */
    
    ASSIGN obs-empax = "".
    
    for each polo-esp-cliente-cq WHERE
             polo-esp-cliente-cq.nome-abrev  = nome-abrev-jr  AND 
             polo-esp-cliente-cq.it-codigo = it-codigo-ped AND
             polo-esp-cliente-cq.metodo    =  "empax"
             USE-INDEX cli-item
             NO-LOCK . 

        ASSIGN obs-empax = obs-empax + polo-esp-cliente-cq.descricao.

    END.
    IF AVAIL tt-am-cq-laudo THEN
         ASSIGN tt-am-cq-laudo.observacao = tt-am-cq-laudo.observacao + "-" + obs-empax.
 

    /*-----------------------------------------------------------------------*/
    
    
    /* Rotina para mostrar an†lises que tenham sido cadastradas e n∆o possuem
       resultados de laborat¢rio.  */
    
    for each polo-esp-cliente-cq WHERE
             polo-esp-cliente-cq.nome-abrev  = nome-abrev-jr  AND 
             polo-esp-cliente-cq.it-codigo = it-codigo-ped AND
             polo-esp-cliente-cq.cod-comp <> 70 AND
             polo-esp-cliente-cq.cod-comp <> 71 AND
             polo-esp-cliente-cq.log-imprime-laudo = yes
             NO-LOCK . 

        IF   polo-esp-cliente-cq.cod-exame = 0 AND
         polo-esp-cliente-cq.cod-comp  = 0  THEN  NEXT.
    
        if tt-param.i-modelo = 3 THEN next.
        
        find first tt-analises where
                   ttana-cod-exame = polo-esp-cliente-cq.cod-exame AND
                   ttana-cod-comp = polo-esp-cliente-cq.cod-comp
                   no-lock no-error.    
    
        if avail tt-analises then next.
    
           create tt-analises.
           assign ttana-cod-comp  = polo-esp-cliente-cq.cod-comp
                  ttana-cod-exame = polo-esp-cliente-cq.cod-exame.
               
           ASSIGN media-result-jr = 0.
    
           ASSIGN descricao-comp = polo-esp-cliente-cq.descricao
                  esp-min-jr     = polo-esp-cliente-cq.espec-min
                  esp-max-jr     = polo-esp-cliente-cq.espec-max
                  valor-tipico-jr = polo-esp-cliente-cq.espec-alvo
                  unidade-jr     = polo-esp-cliente-cq.unidade
                  metodo-jr      = polo-esp-cliente-cq.metodo
                  decimais-jr    = polo-esp-cliente.nr-decimais.
    
              
           DO:
    
                ASSIGN i-linha = i-linha + 1.
    
                ASSIGN media-jr2    = media-result-jr
                       media-result-jr = valor-tipico-jr.
                 
               FIND FIRST tt-am-cq-result-laudo WHERE
                   tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel AND
                   tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis AND
                   tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    AND
                   tt-am-cq-result-laudo.cod-exame   = polo-esp-cliente-cq.cod-exame AND
                   tt-am-cq-result-laudo.cod-comp    = polo-esp-cliente-cq.cod-comp
                   EXCLUSIVE-LOCK NO-ERROR.
            
               IF NOT AVAIL tt-am-cq-result-laudo THEN DO:
                   CREATE tt-am-cq-result-laudo.
                   ASSIGN tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel 
                          tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis 
                          tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    
                          tt-am-cq-result-laudo.cod-exame   = polo-esp-cliente-cq.cod-exame                     
                          tt-am-cq-result-laudo.cod-comp    = polo-esp-cliente-cq.cod-comp.                  
            
               ASSIGN tt-am-cq-result-laudo.descricao   = descricao-comp
                      tt-am-cq-result-laudo.media-resul = media-jr2
                      tt-am-cq-result-laudo.nr-decimais = decimais-jr
                      tt-am-cq-result-laudo.espec-alvo  = valor-tipico-jr
                      tt-am-cq-result-laudo.unidade     = unidade-jr
                      tt-am-cq-result-laudo.metodo      = metodo-jr.
                      
               END.
                   
    
           END.
    END.
   
    
    IF c-cod-estabel-ini = STRING({cdp\poloestab.i 423}) THEN /*solic-318*/ 
       ASSIGN nome-estabel-jr = "POLO MG".
         ELSE
           ASSIGN nome-estabel-jr = "POLO RS".  
    
 
    for each polo-esp-cliente-cq WHERE
             polo-esp-cliente-cq.nome-abrev  = nome-estabel-jr  AND 
             polo-esp-cliente-cq.it-codigo = it-codigo-ped    AND
             polo-esp-cliente-cq.cod-comp <> 70 AND
             polo-esp-cliente-cq.cod-comp <> 71  
             NO-LOCK . 
    
         IF   polo-esp-cliente-cq.cod-exame = 0 AND
         polo-esp-cliente-cq.cod-comp  = 0  THEN  NEXT.

        if tt-param.i-modelo = 3 THEN next.
     
        find first tt-analises where
                   ttana-cod-comp = polo-esp-cliente-cq.cod-comp AND
                   ttana-cod-exame = polo-esp-cliente-cq.cod-exame
                   no-lock no-error.    
    
        if avail tt-analises then next.
    
           create tt-analises.
           
           assign ttana-cod-comp = polo-esp-cliente-cq.cod-comp
                  ttana-cod-exame = polo-esp-cliente-cq.cod-exame.
               
           ASSIGN media-result-jr = 0.
    
           ASSIGN descricao-comp = polo-esp-cliente-cq.descricao
                  esp-min-jr     = polo-esp-cliente-cq.espec-min
                  esp-max-jr     = polo-esp-cliente-cq.espec-max
                  valor-tipico-jr = polo-esp-cliente-cq.espec-alvo
                  unidade-jr     = polo-esp-cliente-cq.unidade
                  metodo-jr      = polo-esp-cliente-cq.metodo
                  decimais-jr    = polo-esp-cliente.nr-decimais.
    
              
               DO:
    
                 ASSIGN i-linha = i-linha + 1.
    
                 ASSIGN media-jr2    = media-result-jr
                        media-result-jr = valor-tipico-jr.
                 
                FIND FIRST tt-am-cq-result-laudo WHERE
                    tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel AND
                    tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis AND
                    tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    AND
                    tt-am-cq-result-laudo.cod-exame   = polo-esp-cliente-cq.cod-exame AND
                    tt-am-cq-result-laudo.cod-comp    = polo-esp-cliente-cq.cod-comp
                    EXCLUSIVE-LOCK NO-ERROR.
                
                IF NOT AVAIL tt-am-cq-result-laudo THEN DO:
                    CREATE tt-am-cq-result-laudo.
                    ASSIGN tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel 
                           tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis 
                           tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    
                           tt-am-cq-result-laudo.cod-exame   = polo-esp-cliente-cq.cod-exame                     
                           tt-am-cq-result-laudo.cod-comp    = polo-esp-cliente-cq.cod-comp.                  
                
                ASSIGN tt-am-cq-result-laudo.descricao   = descricao-comp
                       tt-am-cq-result-laudo.media-resul = media-jr2
                       tt-am-cq-result-laudo.nr-decimais = decimais-jr
                       tt-am-cq-result-laudo.espec-alvo  = valor-tipico-jr
                       tt-am-cq-result-laudo.unidade     = unidade-jr
                       tt-am-cq-result-laudo.metodo      = metodo-jr.
                   
                END.

    
            END.
    END.
    
    
    /*-----------------------------------------------------------------------*/

    /* Rotina para Gerar na Planilha as Larguras e Comprimentos das Bobinas */
    
    IF larg-bob-jr > 0 AND tt-param.i-modelo > 1 and tt-param.i-modelo <> 3 THEN DO:
    
                  
        ASSIGN larg-bob-jr = larg-bob-jr / soma-larg-bob.
    
        ASSIGN desv-padrao-jr = 0.

        IF tt-param.i-modelo = 3 and
           soma-larg-bob > 1  THEN DO:  /*Calcular Desvio Padr∆o (largura) P.Morris*/

           ASSIGN i-jr = 1
                  soma-dp = 0.

           DO WHILE i-jr <= soma-larg-bob AND i-jr < 9000.

               ASSIGN resul-dp = amostra-larg-jr [i-jr] - larg-bob-jr.

               IF resul-dp < 0 THEN 
                   ASSIGN resul-dp = resul-dp * -1.

               ASSIGN soma-dp = soma-dp + resul-dp.

               ASSIGN i-jr = i-jr + 1.

           END.

           ASSIGN i-jr = i-jr - 1
                  soma-dp = (soma-dp * soma-dp) / (i-jr - 1).

           ASSIGN desv-padrao-jr = SQRT(soma-dp).


        END. /*Fim de Calcular Desvio Padr∆o (largura) P.Morris*/
        
        


        FIND FIRST tt-am-cq-result-laudo WHERE
            tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel AND
            tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis AND
            tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    AND
            tt-am-cq-result-laudo.cod-exame   = 2011                    AND
            tt-am-cq-result-laudo.cod-comp    = 17
            EXCLUSIVE-LOCK NO-ERROR.
        
        IF NOT AVAIL tt-am-cq-result-laudo THEN DO:
            CREATE tt-am-cq-result-laudo.
            ASSIGN tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel 
                   tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis 
                   tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    
                   tt-am-cq-result-laudo.cod-exame   = 2011                     
                   tt-am-cq-result-laudo.cod-comp    = 17.                  
        END.
        
        ASSIGN tt-am-cq-result-laudo.descricao   = "Largura da Bobina"
               tt-am-cq-result-laudo.media-resul = larg-bob-jr
               tt-am-cq-result-laudo.qtd-result  = soma-larg-bob
               tt-am-cq-result-laudo.espec-alvo  = larg-ped
               tt-am-cq-result-laudo.nr-decimais = 0
               tt-am-cq-result-laudo.unidade     = "mm"
               tt-am-cq-result-laudo.metodo      = "".

        IF tt-param.i-modelo = 3 THEN
            ASSIGN tt-am-cq-result-laudo.espec-max = desv-padrao-jr.


    END.
                   
    IF compr-bob-jr > 0 AND tt-param.i-modelo > 1 and tt-param.i-modelo <> 3 THEN DO:
    
        ASSIGN compr-bob-jr = compr-bob-jr / soma-compr-bob.
    
        ASSIGN desv-padrao-jr = 0.

        IF tt-param.i-modelo = 3 and
           soma-compr-bob > 1  THEN DO:  /*Calcular Desvio Padr∆o (comprimento) P.Morris*/

           ASSIGN i-jr = 1
                  soma-dp = 0.

           DO WHILE i-jr <= soma-compr-bob AND i-jr < 9000.

               ASSIGN resul-dp = amostra-compr-jr [i-jr] - compr-bob-jr.

               IF resul-dp < 0 THEN 
                   ASSIGN resul-dp = resul-dp * -1.

               ASSIGN soma-dp = soma-dp + resul-dp.

               ASSIGN i-jr = i-jr + 1.

           END.

           ASSIGN i-jr = i-jr - 1
                  soma-dp = (soma-dp * soma-dp) / (i-jr - 1).

           ASSIGN desv-padrao-jr = SQRT(soma-dp).


        END. /*Fim de Calcular Desvio Padr∆o (comprimento) P.Morris*/

    

        FIND FIRST tt-am-cq-result-laudo WHERE
            tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel AND
            tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis AND
            tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    AND
            tt-am-cq-result-laudo.cod-exame   = 2011                    AND
            tt-am-cq-result-laudo.cod-comp    = 18
            EXCLUSIVE-LOCK NO-ERROR.
        
        IF NOT AVAIL tt-am-cq-result-laudo THEN DO:
            CREATE tt-am-cq-result-laudo.
            ASSIGN tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel 
                   tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis 
                   tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    
                   tt-am-cq-result-laudo.cod-exame   = 2011                     
                   tt-am-cq-result-laudo.cod-comp    = 18.                  
        END.
        
        ASSIGN tt-am-cq-result-laudo.descricao   = "DiÉmetro Externo da Bobina"
               tt-am-cq-result-laudo.media-resul = diex-ped
               tt-am-cq-result-laudo.qtd-result  = 1
               tt-am-cq-result-laudo.espec-alvo  = diex-ped
               tt-am-cq-result-laudo.nr-decimais = 0
               tt-am-cq-result-laudo.unidade     = "mm"
               tt-am-cq-result-laudo.metodo      = "".    
    
    
        FIND FIRST tt-am-cq-result-laudo WHERE
            tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel AND
            tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis AND
            tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    AND
            tt-am-cq-result-laudo.cod-exame   = 2011                    AND
            tt-am-cq-result-laudo.cod-comp    = 19
            EXCLUSIVE-LOCK NO-ERROR.
        
        IF NOT AVAIL tt-am-cq-result-laudo THEN DO:
            CREATE tt-am-cq-result-laudo.
            ASSIGN tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel 
                   tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis 
                   tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    
                   tt-am-cq-result-laudo.cod-exame   = 2011                     
                   tt-am-cq-result-laudo.cod-comp    = 19.                  
        END.
        
        ASSIGN tt-am-cq-result-laudo.descricao   = "Comprimento da Bobina"
               tt-am-cq-result-laudo.media-resul = compr-bob-jr
               tt-am-cq-result-laudo.qtd-result  = soma-compr-bob
               tt-am-cq-result-laudo.espec-alvo  = ( ( ( (diex-ped * diex-ped) - (94 * 94) ) * 3.1416) / 4) / esp-jr
               tt-am-cq-result-laudo.nr-decimais = 0
               tt-am-cq-result-laudo.unidade     = "m"
               tt-am-cq-result-laudo.metodo      = "".

        IF tt-param.i-modelo = 3 THEN
            ASSIGN tt-am-cq-result-laudo.espec-max = desv-padrao-jr.

    END.
    
    /* Fim da Rotina para Gerar na Planilha as Larguras e Comprimentos das Bobinas */
 
    
    /* Rotina para Gerar na Planilha as MÇdias das Durezas */
    
    IF dureza-jr > 0 and tt-param.i-modelo <> 3 THEN DO:
    
        ASSIGN dureza-jr = dureza-jr / soma-dure.
    
        FIND FIRST polo-esp-cliente-cq WHERE
             polo-esp-cliente-cq.nome-abrev  = nome-abrev-jr  AND 
             polo-esp-cliente-cq.it-codigo = it-codigo-ped    AND
             polo-esp-cliente-cq.cod-exame = 2011             AND
             polo-esp-cliente-cq.cod-comp  = 36
             NO-LOCK NO-ERROR. 
             
           if not avail polo-esp-cliente-cq then
             FIND FIRST polo-esp-cliente-cq WHERE
             polo-esp-cliente-cq.nome-abrev  = "POLO RS"   AND 
             polo-esp-cliente-cq.it-codigo = it-codigo-ped    AND
             polo-esp-cliente-cq.cod-exame = 2011             AND
             polo-esp-cliente-cq.cod-comp  = 36
             NO-LOCK NO-ERROR.       

    
        IF AVAIL polo-esp-cliente-cq  AND polo-esp-cliente-cq.log-imprime-laudo = YES AND
           tt-param.destino = 4 THEN DO: 
        
            ASSIGN i-linha = i-linha + 1.
            
            ASSIGN media-result-jr-x = STRING (dureza-jr,">>>>>>>>9").

            FIND FIRST tt-am-cq-result-laudo WHERE
                tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel AND
                tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis AND
                tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    AND
                tt-am-cq-result-laudo.cod-exame   = 2011                    AND
                tt-am-cq-result-laudo.cod-comp    = 36
                EXCLUSIVE-LOCK NO-ERROR.
            
            IF NOT AVAIL tt-am-cq-result-laudo THEN DO:
                CREATE tt-am-cq-result-laudo.
                ASSIGN tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel 
                       tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis 
                       tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    
                       tt-am-cq-result-laudo.cod-exame   = 2011                     
                       tt-am-cq-result-laudo.cod-comp    = 36.                  
            END.
            
            ASSIGN tt-am-cq-result-laudo.descricao   = "Dureza/Hardness"
                   tt-am-cq-result-laudo.media-resul = dureza-jr
                   tt-am-cq-result-laudo.espec-alvo  = 35
                   tt-am-cq-result-laudo.nr-decimais = 0
                   tt-am-cq-result-laudo.unidade     = "Rho"
                   tt-am-cq-result-laudo.metodo      = "".
                   
       END.
    
    END.
    
    /*-----------------------------------------------------------------------*/
    
    /* Tira an†lise de espessura e gramatura do laudo */
    ASSIGN tot-peso-jr1 = 0.

    IF tot-peso-jr1 <> 0 and tt-param.i-modelo <> 3 THEN DO:
    
               FIND FIRST polo-esp-cliente-cq WHERE
                    polo-esp-cliente-cq.nome-abrev  = nome-estabel-jr  AND 
                    polo-esp-cliente-cq.it-codigo = it-codigo-ped      AND
                    polo-esp-cliente-cq.cod-exame = 2006               AND
                    polo-esp-cliente-cq.cod-comp  = 70
                    NO-LOCK NO-ERROR. 
    
               IF NOT AVAIL polo-esp-cliente-cq THEN DO:
    
                   FIND FIRST polo-esp-cliente-cq WHERE
                        polo-esp-cliente-cq.nome-abrev  = nome-abrev-jr  AND 
                        polo-esp-cliente-cq.it-codigo = it-codigo-ped    AND
                        polo-esp-cliente-cq.cod-exame = 2006             AND
                        polo-esp-cliente-cq.cod-comp  = 70
                        NO-LOCK NO-ERROR. 
    
               END.
    
               IF AVAIL polo-esp-cliente-cq THEN 
    
                   ASSIGN descricao-comp = polo-esp-cliente-cq.descricao
                          esp-min-jr     = tot-min-jr
                          esp-max-jr     = tot-max-jr
                          valor-tipico-jr = polo-esp-cliente-cq.espec-alvo
                          unidade-jr     = polo-esp-cliente-cq.unidade
                          metodo-jr      = polo-esp-cliente-cq.metodo
                          decimais-jr    = polo-esp-cliente-cq.nr-decimais.  
                  ELSE                                                  
                      ASSIGN descricao-comp = "ESPESSURA"
                             esp-min-jr     = tot-min-jr
                             esp-max-jr     = tot-max-jr
                             valor-tipico-jr = 0
                             unidade-jr     = "MICRONS"
                             metodo-jr      = ""
                             decimais-jr    = 2. 
    
               IF fm-codigo-jr = "BSA" OR fm-codigo-jr = "BST" OR 
                  fm-codigo-jr = "BPX" THEN                             
                   ASSIGN densidade-jr = 0.913.                         
                    ELSE                                                
                      ASSIGN densidade-jr = 0.905.

                      MESSAGE "falta de-para familia" fm-codigo-jr
                          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
               ASSIGN media-result-jr = ((tot-peso-jr1 / tot-fator-jr1) / densidade-jr)
                      * 1000000.
    
               DO:
    
                  ASSIGN i-linha = i-linha + 1.
                
                
                  FIND FIRST tt-am-cq-result-laudo WHERE
                      tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel AND
                      tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis AND
                      tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    AND
                      tt-am-cq-result-laudo.cod-exame   = 2006                    AND
                      tt-am-cq-result-laudo.cod-comp    = 70
                      EXCLUSIVE-LOCK NO-ERROR.
                  
                  IF NOT AVAIL tt-am-cq-result-laudo THEN DO:
                      CREATE tt-am-cq-result-laudo.
                      ASSIGN tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel 
                             tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis 
                             tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    
                             tt-am-cq-result-laudo.cod-exame   = 2006                     
                             tt-am-cq-result-laudo.cod-comp    = 70. 

                  END.
               
                  ASSIGN tt-am-cq-result-laudo.descricao   = descricao-comp
                         tt-am-cq-result-laudo.media-resul = media-result-jr
                         tt-am-cq-result-laudo.nr-decimais = decimais-jr
                         tt-am-cq-result-laudo.espec-alvo  = valor-tipico-jr
                         tt-am-cq-result-laudo.unidade     = unidade-jr
                         tt-am-cq-result-laudo.metodo      = metodo-jr.

    
               END.  
    
    
               FIND FIRST polo-esp-cliente-cq WHERE
                    polo-esp-cliente-cq.nome-abrev  = nome-estabel-jr  AND 
                    polo-esp-cliente-cq.it-codigo = it-codigo-ped      AND
                    polo-esp-cliente-cq.cod-exame = 2006               AND
                    polo-esp-cliente-cq.cod-comp  = 71
                    NO-LOCK NO-ERROR. 
    
               IF NOT AVAIL polo-esp-cliente-cq THEN DO:
    
                   FIND FIRST polo-esp-cliente-cq WHERE
                        polo-esp-cliente-cq.nome-abrev  = nome-abrev-jr  AND 
                        polo-esp-cliente-cq.it-codigo = it-codigo-ped    AND
                        polo-esp-cliente-cq.cod-exame = 2006             AND
    
                        polo-esp-cliente-cq.cod-comp  = 71
                        NO-LOCK NO-ERROR. 
    
               END.
    
               IF AVAIL polo-esp-cliente-cq THEN 
    
               ASSIGN descricao-comp = polo-esp-cliente-cq.descricao
                      esp-min-jr     = gra-min-jr
                      esp-max-jr     = gra-max-jr 
                      valor-tipico-jr = polo-esp-cliente-cq.espec-alvo
                      unidade-jr     = polo-esp-cliente-cq.unidade
                      metodo-jr      = polo-esp-cliente-cq.metodo
                      decimais-jr    = polo-esp-cliente-cq.nr-decimais.  
              ELSE                                                  
                  ASSIGN descricao-comp = "GRAMATURA"
                         esp-min-jr     = gra-min-jr
                         esp-max-jr     = gra-max-jr 
                         valor-tipico-jr = 0
                         unidade-jr     = "G/M2"
                         metodo-jr      = ""
                         decimais-jr    = 2. 
    
           ASSIGN media-result-jr = ((tot-peso-jr1 / tot-fator-jr1)
                  * 1000000) .
    
         DO:
    
         ASSIGN i-linha = i-linha + 1.
     
                
         FIND FIRST tt-am-cq-result-laudo WHERE
             tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel AND
             tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis AND
             tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    AND
             tt-am-cq-result-laudo.cod-exame   = 2006                    AND
             tt-am-cq-result-laudo.cod-comp    = 71
             EXCLUSIVE-LOCK NO-ERROR.
         
         IF NOT AVAIL tt-am-cq-result-laudo THEN DO:
             CREATE tt-am-cq-result-laudo.
             ASSIGN tt-am-cq-result-laudo.cod-estabel = tt-am-cq-laudo.cod-estabel 
                    tt-am-cq-result-laudo.nr-nota-fis = tt-am-cq-laudo.nr-nota-fis 
                    tt-am-cq-result-laudo.nr-laudo    = tt-am-cq-laudo.nr-laudo    
                    tt-am-cq-result-laudo.cod-exame   = 2006                     
                    tt-am-cq-result-laudo.cod-comp    = 71. 

         END.
         
         ASSIGN tt-am-cq-result-laudo.descricao   = descricao-comp
                tt-am-cq-result-laudo.media-resul = media-result-jr
                tt-am-cq-result-laudo.nr-decimais = decimais-jr
                tt-am-cq-result-laudo.espec-alvo  = valor-tipico-jr
                tt-am-cq-result-laudo.unidade     = unidade-jr
                tt-am-cq-result-laudo.metodo      = metodo-jr.

    
       END. 
    
    END.

 IF AVAIL tt-am-cq-laudo THEN
    ASSIGN tt-am-cq-laudo.enviado-email = NO
           tt-am-cq-laudo.gerado-excel  = NO.

END PROCEDURE. 



PROCEDURE grava-rastreabilidade-campanha.

    FIND FIRST ord-prod WHERE 
         ord-prod.nr-ord-produ = movto-estoq.nr-ord-produ
         USE-INDEX codigo
         NO-LOCK NO-ERROR.

    IF AVAIL ord-prod THEN DO:
    
       FIND tt-bobinas2 WHERE
          ttbob2-lote = movto-estoq.lote AND
          ttbob2-linha = ord-prod.nr-linha
          USE-INDEX ch-tt-bobinas2 NO-ERROR.

       IF NOT AVAIL tt-bobinas2 THEN DO:
          CREATE tt-bobinas2.
          ASSIGN ttbob2-lote = movto-estoq.lote
                 ttbob2-linha = ord-prod.nr-linha
                 ttbob2-it-codigo = ord-prod.it-codigo
                 ttbob2-nr-ord-produ = ord-prod.nr-ord-produ
                 ttbob2-dt-trans  = movto-estoq.dt-trans.
       END.
    END.
END PROCEDURE.

PROCEDURE grava-rastreabilidade.

    FIND FIRST ord-prod WHERE 
         ord-prod.nr-ord-produ = op-rast
         USE-INDEX codigo
         NO-LOCK NO-ERROR.

    IF AVAIL ord-prod THEN DO:
    
       FIND tt-bobinas WHERE
          ttbob-lote = lote-rast AND
          ttbob-linha = ord-prod.nr-linha
          USE-INDEX ch-tt-bobinas NO-ERROR.

       IF NOT AVAIL tt-bobinas THEN DO:
          CREATE tt-bobinas.
          ASSIGN ttbob-lote = /*movto-estoq.lote */ lote-rast
                 ttbob-linha = ord-prod.nr-linha
                 ttbob-it-codigo = ord-prod.it-codigo
                 ttbob-nr-ord-produ = ord-prod.nr-ord-produ
                 ttbob-dt-trans  = data-rast.
       END.

    END.

    ELSE DO:

        FIND tt-bobinas WHERE
           ttbob-lote = lote-rast AND
           ttbob-linha = 1
           USE-INDEX ch-tt-bobinas NO-ERROR.

        IF NOT AVAIL tt-bobinas THEN DO:
           CREATE tt-bobinas.
           ASSIGN ttbob-lote = lote-rast
                  ttbob-linha = 1
                  ttbob-it-codigo = tt-rastrear.ttras-it-codigo-cons 
                  ttbob-nr-ord-produ = 0
                  ttbob-dt-trans  = data-rast.

        END.

    END.

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

PROCEDURE ver-mr-trat.

    FIND FIRST movto-mat WHERE
               movto-mat.it-codigo = cmkt-req and
               movto-mat.lote = lote-req AND
               movto-mat.esp-docto = 1 AND
               NOT CAN-FIND(FIRST rep-prod WHERE rep-prod.nr-reporte = movto-mat.nr-reporte AND rep-prod.qt-estorno > 0 NO-LOCK)
               USE-INDEX lote NO-LOCK NO-ERROR.
                
    IF NOT AVAIL movto-mat THEN NEXT.

    ASSIGN lote-rast = movto-mat.lote
           op-rast   = movto-mat.nr-ord-produ
           data-rast = movto-mat.dt-trans.
           
    RUN grava-rastreabilidade.

    ASSIGN lote-mr = "X".
    ASSIGN flag-erro = "X".
    
    DO WHILE lote-mr = "X" AND flag-erro = "X" :
       
       FIND FIRST ord-prod WHERE
            ord-prod.nr-ord-produ = movto-mat.nr-ord-produ AND
            ord-prod.it-codigo = movto-mat.it-codigo AND
            ord-prod.cod-estabel = movto-mat.cod-estabel
            USE-INDEX estabel NO-LOCK NO-ERROR.

       IF NOT AVAIL ord-prod THEN 
          ASSIGN flag-erro = "erro". 
          
       IF ord-prod.nr-linha < 100 AND flag-erro = "X" THEN DO:
          ASSIGN lote-mr = movto-mat.lote.
          assign item-mr = movto-mat.it-codigo.
          
       END.
     
       IF lote-mr = "X" AND flag-erro = "X" THEN DO:

           ASSIGN ordpro-req = movto-mat.nr-ord-prod.
           ASSIGN numseq-req = (movto-mat.num-sequen - 1).

           FIND FIRST movto-mat WHERE
                movto-mat.nr-ord-prod = ordpro-req and
                movto-mat.num-sequen = numseq-req AND
                movto-mat.esp-docto = 28
                USE-INDEX num-seq NO-LOCK NO-ERROR.

           IF NOT AVAIL movto-mat THEN
              ASSIGN flag-erro = "erro".
              
           IF flag-erro = "X" THEN DO:
           
              ASSIGN lote-req = movto-mat.lote.
              ASSIGN cmkt-req = movto-mat.it-codigo.

               FIND FIRST movto-mat WHERE
                    movto-mat.it-codigo = cmkt-req and
                    movto-mat.lote = lote-req AND
                    movto-mat.esp-docto = 1 AND
                    NOT CAN-FIND(FIRST rep-prod WHERE rep-prod.nr-reporte = movto-mat.nr-reporte AND rep-prod.qt-estorno > 0 NO-LOCK)
                    USE-INDEX lote NO-LOCK NO-ERROR.

               IF NOT AVAIL movto-mat THEN 
               ASSIGN flag-erro = "erro".
               
               ASSIGN lote-rast = movto-mat.lote
                      op-rast   = movto-mat.nr-ord-produ
                      data-rast = movto-mat.dt-trans.
               
               RUN grava-rastreabilidade.


           END.
       END.
    END. 
END PROCEDURE.

/* Include da procedure - pi-gera-planilha */

{cqp\escq0051-i2.i}


PROCEDURE pi-cria-planilha:

    ASSIGN c-arq-anexo = tt-am-cq-laudo.nome-abrev
           c-arq-anexo = REPLACE(c-arq-anexo,"/","-")
           c-arq-anexo = REPLACE(c-arq-anexo,"\","-")
           c-arq-anexo = REPLACE(c-arq-anexo," ","-")
           c-arq-anexo = REPLACE(c-arq-anexo,",","-").
    

    c-arquivo = c-arq + trim(c-arq-anexo) + '-' + 
                        trim(tt-am-cq-laudo.nr-nota-fis) + '-' + 
                        string(tt-am-cq-laudo.nr-laudo) + '.xls'.

    OS-COPY VALUE(c-modelo-planilha) VALUE(c-arquivo).

    assign c-planilha  = c-excel:Workbooks:OPEN(c-arquivo)
           c-relatorio = c-excel:Sheets:item(1)
           c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo.

END PROCEDURE.


PROCEDURE pi-mostra-planilha:
DEF VAR i         AS INT  NO-UNDO.


    c-planilha:SAVE().
    c-planilha:CLOSE().
     
/*
    IF tt-param.tipo-geracao = 1 THEN DO:
*/
        
        
     if  tt-param.tb-v-temp then 
     os-copy value(c-arquivo) v:\temp.
     
     c-excel:VISIBLE = true.
        
            c-planilha  = c-excel:Workbooks:OPEN(c-arquivo).

/*
    END.
*/    

END PROCEDURE.

PROCEDURE pi-gera-e-mail: 

    IF tt-am-cq-laudo.tipo-envio-email = 1 THEN DO: 

        FIND usuar_mestre WHERE            
             usuar_mestre.cod_usuario = /*"aschnei"*/ tt-am-cq-laudo.cod-usuario 
             NO-LOCK NO-ERROR.

        IF NOT AVAIL usuar_mestre THEN DO: 

            FOR FIRST ext_usuar_grp_usuar WHERE 
                    ext_usuar_grp_usuar.ativo AND
                    ext_usuar_grp_usuar.cod_grp_usuar = "LAUDORESP"   NO-LOCK.


                 FIND FIRST usuar_mestre WHERE
                 usuar_mestre.cod_usuario = ext_usuar_grp_usuar.cod_usuario
                 NO-LOCK NO-ERROR.
        
            

             END.


              IF NOT AVAIL usuar_mestre THEN   
                FIND usuar_mestre WHERE            
                     usuar_mestre.cod_usuario = "aschnei" 
                     NO-LOCK NO-ERROR.

        END.

        ASSIGN c-email-responsavel = usuar_mestre.cod_e_mail_local 
               c-nome-responsavel  = usuar_mestre.nom_usuar.

        FIND FIRST param-global NO-LOCK NO-ERROR.

        IF NOT AVAIL param-global THEN DO:
            MESSAGE "N∆o encontrado parametro global"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN "nok".
        END.

        FIND polo-laudo-cliente WHERE
            polo-laudo-cliente.nome-abrev = tt-am-cq-laudo.nome-abrev
            NO-LOCK NO-ERROR.

        IF NOT AVAIL polo-laudo-cliente THEN RETURN "NOK".

        IF c-email-responsavel = "" OR 
           polo-laudo-cliente.end-email = "" THEN RETURN "NOK".

        ASSIGN nr-ext-jr = "EXT. Nr.: ".

        FIND FIRST emitente WHERE
            emitente.cod-emitente = tt-am-cq-laudo.cod-emitente
            NO-LOCK NO-ERROR.

        IF AVAIL emitente AND emitente.natureza = 3 THEN
            ASSIGN nr-ext-jr = nr-ext-jr + TRIM(tt-am-cq-laudo.observacao).
        ELSE
            ASSIGN nr-ext-jr = "".


        FOR EACH tt-envio.
            DELETE tt-envio.
        END.

        CREATE tt-envio.

        ASSIGN 
            tt-envio.versao-integracao = 1
            tt-envio.exchange    =  param-global.log-1
            tt-envio.remetente   =  c-email-responsavel
            tt-envio.destino     =  trim(polo-laudo-cliente.end-email)
            tt-envio.copia       =  ""
            tt-envio.assunto     =  nr-ext-jr + " Certificado de Qualidade - Certificate of Analysis - Polo"  

            tt-envio.mensagem    = "Srs," + chr(10) + CHR(10) + 
            "Segue anexo os certificados de Qualidade referente aos pedidos numero: " + CHR(10) +
            trim(tt-am-cq-laudo.pedido) + CHR(10) + CHR(10) +
            nr-ext-jr + CHR(10) + CHR(10) +
            "Favor confirmar o recebimento desse e-mail." + CHR(10) +
            CHR(10) + 
            chr(10) + CHR(10) + "Obrigado." + CHR(10) + CHR(10) + c-nome-responsavel +
            chr(10) + CHR(10) + chr(10) + CHR(10) +

            "Dear Sirs," + chr(10) + CHR(10) +
            "Please find attached orderÔs certificate of analysis number:" + CHR(10) +
            trim(tt-am-cq-laudo.pedido) + CHR(10) + CHR(10) +
            nr-ext-jr + CHR(10) + CHR(10) +
            "Please confirm the receipt of this message." + CHR(10) +
            CHR(10) + 
            chr(10) + CHR(10) + "Thanks." + CHR(10) + CHR(10) + c-nome-responsavel

            tt-envio.importancia = 2
            tt-envio.log-enviada = yes
            tt-envio.log-lida    = yes
            tt-envio.acomp       = yes
            tt-envio.arq-anexo   = c-arquivo
            tt-envio.servidor    = param-global.serv-mail          
            tt-envio.porta       = param-global.porta-mail             
            .

        run pi-acompanhar in h-acomp (input "Enviando E-mail p/ " + tt-envio.destino ). 

        OUTPUT TO VALUE(c-arq + "escq051MAIL.TMP").
        run utp/utapi009.p (input  table tt-envio,
                            output table tt-erros).     
        OUTPUT CLOSE. 

        FIND FIRST tt-erros NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-erros THEN
           ASSIGN tt-am-cq-laudo.enviado-email  = YES
                  tt-am-cq-laudo.dt-envio-email = TODAY.
           
        RETURN "OK".

    END.

END PROCEDURE.


PROCEDURE pi-gera-e-mail-2: 

        FIND usuar_mestre WHERE            
             usuar_mestre.cod_usuario = c-seg-usuario 
             NO-LOCK NO-ERROR.

        IF AVAIL usuar_mestre THEN 
           ASSIGN c-email-responsavel = usuar_mestre.cod_e_mail_local 
                  c-nome-responsavel  = usuar_mestre.nom_usuar.

        FIND FIRST param-global NO-LOCK NO-ERROR.

        IF NOT AVAIL param-global THEN DO:
            MESSAGE "N∆o encontrado parametro global"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN "nok".
        END.


        FOR EACH tt-envio.
            DELETE tt-envio.
        END.

        CREATE tt-envio.

        ASSIGN 
            tt-envio.versao-integracao = 1
            tt-envio.exchange    =  param-global.log-1
            tt-envio.remetente   =  IF c-email-responsavel <> "" THEN c-email-responsavel ELSE "carine.zago@polofilms.com.br"
            tt-envio.destino     =  "carine.zago@polofilms.com.br"
            tt-envio.copia       =  c-email-responsavel
            tt-envio.assunto     =  "Certificado de Qualidade - Polo"  

            tt-envio.mensagem    = "Srs," + chr(10) + CHR(10) + 
            "Segue anexo o certificado de Qualidade Oficial " + CHR(10) +
            trim(tt-am-cq-laudo.pedido) + CHR(10) + CHR(10) +
            chr(10) + CHR(10) + "Obrigado." + CHR(10) + CHR(10) + c-nome-responsavel 

            tt-envio.importancia = 2
            tt-envio.log-enviada = yes
            tt-envio.log-lida    = yes
            tt-envio.acomp       = yes
            tt-envio.arq-anexo   = c-arquivo
            tt-envio.servidor    = param-global.serv-mail          
            tt-envio.porta       = param-global.porta-mail             
            .

        run pi-acompanhar in h-acomp (input "Enviando E-mail p/ " + tt-envio.destino ). 

        OUTPUT TO VALUE(c-arq + "escq051MAIL.TMP").
        run utp/utapi009.p (input  table tt-envio,
                            output table tt-erros).     
        OUTPUT CLOSE. 

        FIND FIRST tt-erros NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-erros THEN
           ASSIGN tt-am-cq-laudo.enviado-email  = YES
                  tt-am-cq-laudo.dt-envio-email = TODAY.
           
        RETURN "OK".

END PROCEDURE.


PROCEDURE pi-finaliza-impressao:

    RELEASE OBJECT c-excel.
 
END PROCEDURE.




                                    



