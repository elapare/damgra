/*****************************************************************************
**
**       Programa: escq0019rp.p
**
**       Data....: 26/09/2005
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: ANµLISES REALIZADAS POR PEDIDO DE VENDA
**
**       VersÆo..: 1.00.000 - jrrcampos
**
**       OBS.....: 
**
*******************************************************************************/
define variable c-prog-gerado as character no-undo initial "escq0019RP".
def buffer empresa for mgmulti.empresa.
def new global shared var c-arquivo-log  as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

/****************** Defini‡Æo de Tabelas Tempor rias do Relat¢rio **********************/

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
    field c-nr-pedido-1        like ped-venda.nr-pedido
    field c-nr-pedido-2        like ped-venda.nr-pedido
    field c-nr-pedido-3        like ped-venda.nr-pedido
    field c-nr-pedido-4        like ped-venda.nr-pedido
    field c-nr-pedido-5        like ped-venda.nr-pedido
    field c-nr-pedido-6        like ped-venda.nr-pedido
    field c-nr-pedido-7        like ped-venda.nr-pedido
    field c-nr-pedido-8        like ped-venda.nr-pedido
    field c-nr-pedido-9        like ped-venda.nr-pedido
    field c-nr-pedido-10       like ped-venda.nr-pedido
    field c-nr-pedido-11       like ped-venda.nr-pedido
    field c-nr-pedido-12       like ped-venda.nr-pedido
    field c-nr-pedido-13       like ped-venda.nr-pedido
    field c-nr-pedido-14       like ped-venda.nr-pedido
    field c-nr-pedido-15       like ped-venda.nr-pedido
    field c-nr-pedido-16       like ped-venda.nr-pedido
    field c-nr-pedido-17       like ped-venda.nr-pedido
    field c-nr-pedido-18       like ped-venda.nr-pedido
    field c-nr-pedido-19       like ped-venda.nr-pedido
    field c-nr-pedido-20       like ped-venda.nr-pedido
    field c-nr-pedido-21       like ped-venda.nr-pedido
    FIELD pesq-jr              AS INT  
.


DEFINE TEMP-TABLE tt-digita no-undo
    FIELD pal-nr-pedido            AS INT  FORMAT ">>>>>>9" LABEL "Pedido"
    FIELD pal-nr-pallet            AS CHAR FORMAT "x(10)" LABEL "Nr.Palete"
    FIELD pal-dt-trans             AS DATE FORMAT "99/99/9999" LABEL "Data Palete"
    FIELD pal-it-codigo            AS CHAR FORMAT "x(13)" LABEL "Filme"
    FIELD pal-sequencia            AS INT  FORMAT "zz9" LABEL "Seq.Pedido"
    FIELD pal-nr-bobinas           AS INT  FORMAT ">>>>>>9" LABEL "Qtd.Bobs"
    FIELD pal-peso-liq             AS DECIMAL LABEL "Peso Liquido" FORMAT "zzzzzzzz9.99"
    FIELD pal-peso-bru             AS DECIMAL LABEL "Peso Bruto" FORMAT "zzzzzzzz9.99"
    FIELD pal-nr-nota-fisc         LIKE movto-estoq.nro-docto
    INDEX chave IS PRIMARY UNIQUE pal-nr-pedido
                                  pal-nr-pallet.

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
    FIELD ttped-nr-nota-fisc   LIKE movto-estoq.nro-docto
    FIELD ttped-pedcli         AS CHAR FORMAT "x(15)"
    INDEX ch-tt-pedidos IS PRIMARY UNIQUE   ttped-nr-pedido.
                                            
DEFINE TEMP-TABLE tt-bobinas2
    FIELD ttbob2-lote           AS CHAR 
    FIELD ttbob2-linha          AS integer
    FIELD ttbob2-it-codigo      AS CHAR
    FIELD ttbob2-nr-ord-produ   AS INTEGER
    FIELD ttbob2-dt-trans       AS DATE
    INDEX ch-tt-bobinas2 IS PRIMARY UNIQUE   ttbob2-lote
                                             ttbob2-linha. 


DEFINE TEMP-TABLE tt-analises
    FIELD ttana-cod-comp        LIKE pol-res-fic-cq-leitura.cod-comp
    FIELD ttana-it-codigo       LIKE pol-res-fic-cq-leitura.it-codigo
    FIELD ttana-result          AS DECIMAL 
    FIELD ttana-qtd-result      AS INTEGER
    FIELD ttana-minimo          AS DECIMAL
    FIELD ttana-maximo          AS DECIMAL
    FIELD ttana-esp-min         AS DECIMAL
    FIELD ttana-esp-max         AS DECIMAL
    INDEX ch-tt-analises IS PRIMARY UNIQUE  ttana-cod-comp.


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

def new shared var c-cod-estabel-ini like ped-venda.cod-estabel format "x(3)" initial "" no-undo.
def new shared var c-nr-pedcli-ini like ped-venda.nr-pedcli format "x(12)" initial "" no-undo.
def new shared var c-nr-pedcli-fim like ped-venda.nr-pedcli format "x(12)" initial "ZZZZZZZZZZZZ" no-undo.
def new shared var c-nr-pallet-ini like pallet.nr-pallet format "x(12)" initial "" no-undo.
def new shared var c-nr-pallet-fim like pallet.nr-pallet format "x(12)" initial "ZZZZZZZZZZZZ" no-undo.
def new shared var c-nr-sequencia-ini like ped-item.nr-sequencia format ">9" initial 1 no-undo.


/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 
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
DEFINE VARIABLE media-result  AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE qtd-bobinas   AS INTEGER                   NO-UNDO.
DEFINE VARIABLE unidade-jr    AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE metodo-jr     AS CHARACTER                 NO-UNDO.
DEFINE VAR    valor-tipico-jr AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE data-jr       AS DATE                      NO-UNDO.
DEFINE VARIABLE coluna-excel  AS CHARACTER  FORMAT "x(20)" NO-UNDO INITIAL "CDEFGHICDEFGHI".
DEFINE VARIABLE soma-q-plt    AS INTEGER                   NO-UNDO.
DEFINE VARIABLE cod-estabel-jr AS CHARACTER  FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE tem-minimo    AS CHARACTER   FORMAT "x(1)" NO-UNDO.
DEFINE VARIABLE tem-maximo    AS CHARACTER   FORMAT "x(1)" NO-UNDO.

DEFINE VARIABLE nrnf-ant      AS CHARACTER  FORMAT "X(16)" NO-UNDO.
DEFINE VARIABLE pedcli-ant    AS CHARACTER  FORMAT "X(15)" NO-UNDO.
DEFINE VARIABLE pedido-x1     AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE pedido-x2     AS CHARACTER  FORMAT "x(90)" NO-UNDO.
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
DEFINE VARIABLE pallet-x1     AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE pallet-x2     AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE pallet-x3     AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE pallet-x4     AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE pallet-x5     AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE pallet-x6     AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE pallet-x7     AS CHARACTER  FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE pallet-x8     AS CHARACTER  FORMAT "x(90)" NO-UNDO.

DEFINE VARIABLE conta-ped       AS INTEGER                 NO-UNDO.
DEFINE VARIABLE nome-estabel-jr AS CHARACTER               NO-UNDO.
DEFINE VARIABLE sai-laudo       AS logical                 NO-UNDO.

/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/* Vari veis necess rias para a rastreabilidade de bobinas 
   Programa externo escp033 e escp034 */

DEFINE var lote-rastrear AS CHAR FORMAT "x(10)" NO-UNDO.
DEFINE var it-codigo-rastrear AS CHAR FORMAT "x(16)" NO-UNDO.

DEFINE  NEW GLOBAL SHARED TEMP-TABLE tt-rastrear
    FIELD ttras-lote-cons         LIKE lote-rastreab.lote-cons
    FIELD ttras-it-codigo-cons    LIKE lote-rastreab.it-codigo-cons 
    FIELD ttras-nr-ord-produ-cons LIKE lote-rastreab.nr-ord-produ-cons
    FIELD ttras-pesq              AS   INTEGER
    INDEX ch-tt-rastrear IS PRIMARY UNIQUE ttras-lote-cons.


/* Vari veis usadas para gerar planilha excel. */

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

/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

/****************** Defini‡ao de Vari veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/

def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.

/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 

FORM descricao-comp         COLUMN-LABEL "Analise"   FORMAT "x(30)"     AT 001
     ttana-minimo           COLUMN-LABEL "Res.Min."  FORMAT ">>>>>9.99" AT 035
     ttana-maximo           COLUMN-LABEL "Res.Max."  FORMAT ">>>>>9.99" AT 048
     media-result           COLUMN-LABEL "Res.M‚dio" FORMAT ">>>>>9.99" AT 061 
     ttana-qtd-result       COLUMN-LABEL "Nr.Anals"  FORMAT ">>9"       AT 074
     valor-tipico-jr        COLUMN-LABEL "Vlr-T¡pico" FORMAT ">>>>>9.99" AT 083
     unidade-jr             COLUMN-LABEL "Unidade"   FORMAT "x(12)"     AT 096
     metodo-jr              COLUMN-LABEL "Metodo"    FORMAT "x(15)"     AT 113
     with down width 132 no-box stream-io frame f-relat-09-132.

FORM "Cliente:" AT 001 nome-abrev-jr format "x(12)" AT 010 "Nr.Pedido:" AT 070
      tp-pedido-jr FORMAT "x(2)" AT 081 c-nr-pedcli-ini format "x(13)"  at 083 
     "Tipo do Filme:" AT 001 it-codigo-ped FORMAT "x(15)" AT 016
     "Largura da Bobina (mm):" AT 070 larg-ped FORMAT ">>>>9" AT 095 
     "Nr. de Bobinas:" AT 001 qtd-bobinas FORMAT ">>>>9" AT 017
     "Diƒmetro da Bobina (mm):" AT 070 diex-ped FORMAT ">>>>9" AT 095 
     "Validade:   6 meses ap¢s fatura" AT 001
     "N£cleo (mm):" AT 070 diin-ped FORMAT ">>>>9" AT 095
     with down width 132 no-box stream-io frame f-relat-lin-ped.

FORM "ConclusÆo:" AT 001 "(    ) Aprovado" AT 035 "(    ) Reprovado" AT 085
      with down width 132 no-box stream-io frame f-relat-lin-final-1.

FORM "Observa‡äes / Recomenda‡äes" AT 050 
     "---------------------------" AT 050
      with down width 132 no-box stream-io frame f-relat-lin-final-2.

FORM "---------------------------" AT 025 "-----------------------------" AT 075
     " Respons vel/Assinatura    " AT 025 "           Data              " AT 075
      with down width 132 no-box stream-io frame f-relat-lin-final-3.

FORM "REF.: LAB-RE-122B" AT 010 "REVISÇO: 03" AT 050 "DATA:" AT 080 data-jr FORMAT "99/99/9999" AT 086
      with down width 132 no-box stream-io frame f-relat-lin-inic-1.

FORM "                 " AT 050 
      with down width 132 no-box stream-io frame f-relat-lin-inic-2.

FORM "Objetivo:   (    ) Libera‡Æo de Amostras" AT 010 "(    ) Libera‡Æo de Produ‡Æo" AT 080
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

assign c-programa     = "escq0019rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "ANALISES REALIZADAS P/PEDIDO DE VENDA"
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
    "Per¡odo:" i-numper-x at 08 "-"
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
           ASSIGN v-cod-destino-impres = "excel".


run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

IF tt-param.destino = 4 THEN DO:
    
    /* Cria Aplica‡Æo do Excel */

    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

    ASSIGN c-modelo-planilha = search("modelos/mod-escq0019.xls") 
           c-arq             = SESSION:TEMP-DIRECTORY.

    RUN pi-cria-planilha.


END.

assign v-num-reg-lidos = 0.

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
       qtd-bobinas = 0.

FOR EACH tt-bobinas NO-LOCK:
    DELETE tt-bobinas.
END.

FOR EACH tt-pedidos NO-LOCK:
    DELETE tt-pedidos.
END.

ASSIGN conta-ped = 0
       pallet-x1 = ""
       pallet-x2 = ""
       pallet-x3 = ""
       pallet-x4 = ""
       pallet-x5 = ""
       pallet-x6 = ""
       pallet-x7 = ""
       pallet-x8 = ""
       nrnf-x1   = ""
       nrnf-ant  = "".

FOR EACH tt-digita NO-LOCK:

    ASSIGN conta-ped = conta-ped + 1. 

    IF tt-digita.pal-nr-nota-fis <> nrnf-ant THEN
       ASSIGN nrnf-x1 = (TRIM (nrnf-x1) + "/" + STRING (tt-digita.pal-nr-nota-fis))
              nrnf-ant = tt-digita.pal-nr-nota-fis. 
    
    IF conta-ped < 9 THEN
    ASSIGN pallet-x1 = (TRIM (pallet-x1) + " " + STRING (tt-digita.pal-nr-pallet)).
      ELSE
        IF conta-ped < 17 THEN
          ASSIGN pallet-x2 = (TRIM (pallet-x2) + " " + STRING (tt-digita.pal-nr-pallet)).
        ELSE
          IF conta-ped < 25 THEN
            ASSIGN pallet-x3 = (TRIM (pallet-x3) + " " + STRING (tt-digita.pal-nr-pallet)).
          ELSE
            IF conta-ped < 33 THEN
              ASSIGN pallet-x4 = (TRIM (pallet-x4) + " " + STRING (tt-digita.pal-nr-pallet)).
            ELSE
              IF conta-ped < 41 THEN
                ASSIGN pallet-x5 = (TRIM (pallet-x5) + " " + STRING (tt-digita.pal-nr-pallet)).
              ELSE
                IF conta-ped < 49 THEN
                  ASSIGN pallet-x6 = (TRIM (pallet-x6) + " " + STRING (tt-digita.pal-nr-pallet)).
                ELSE
                  IF conta-ped < 57 THEN
                    ASSIGN pallet-x7 = (TRIM (pallet-x7) + " " + STRING (tt-digita.pal-nr-pallet)).
                  ELSE
                    IF conta-ped < 65 THEN
                      ASSIGN pallet-x8 = (TRIM (pallet-x8) + " " + STRING (tt-digita.pal-nr-pallet)).

    
    FIND FIRST tt-pedidos WHERE
         ttped-nr-pedido = tt-digita.pal-nr-pedido 
         NO-LOCK NO-ERROR.

         IF NOT AVAIL tt-pedidos THEN DO:

             CREATE tt-pedidos.
             ASSIGN ttped-nr-pedido = tt-digita.pal-nr-pedido
                    ttped-nr-nota-fisc = tt-digita.pal-nr-nota-fisc.
           
             ASSIGN larg-ped = 0
                    diin-ped = 0
                    diex-ped = 0
                    pedcli-ped = "".

             FIND FIRST ped-venda WHERE
                  ped-venda.nr-pedido = tt-digita.pal-nr-pedido 
                  NO-LOCK NO-ERROR.

             IF NOT AVAIL ped-venda THEN next.

             FIND FIRST ped-item WHERE 
                  ped-item.nome-abrev = ped-venda.nome-abrev AND
                  ped-item.nr-pedcli = ped-venda.nr-pedcli AND
                  ped-item.cod-refer <> "" 
                  NO-LOCK NO-ERROR.

             IF NOT AVAIL ped-item  THEN next.

             FIND FIRST cot-est-mast
               WHERE cot-est-mast.item-cotacao = ped-item.it-codigo 
               AND cot-est-mast.nr-estrut    = ped-item.nr-config
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

               ASSIGN ttped-diex = diex-ped
                      ttped-diin = diin-ped
                      ttped-larg = larg-ped
                      ttped-pedcli = pedcli-ped.

         END.

         ASSIGN ttped-qtdbob = ttped-qtdbob + tt-digita.pal-nr-bobinas.

END.


FIND FIRST ped-venda 
         where ped-venda.nr-pedido  = tt-param.c-nr-pedido-1
         NO-LOCK NO-ERROR.

IF AVAIL ped-venda THEN DO:

    ASSIGN cod-estabel-jr = ped-venda.cod-estabel
           nome-abrev-jr  = ped-venda.nome-abrev
           tp-pedido-jr   = ped-venda.tp-pedido
           c-nr-pedcli-ini = ped-venda.nr-pedcli.

    FOR Each ped-item OF ped-venda No-lock Where
         ped-item.cod-refer <> "" :

        FIND FIRST ITEM WHERE ITEM.it-codigo = ped-item.it-codigo
             USE-INDEX codigo NO-LOCK NO-ERROR.

        IF NOT AVAIL ITEM THEN NEXT.

        ASSIGN fm-codigo-jr = ITEM.fm-codigo.

        FOR EACH tt-digita NO-LOCK:
        
           FIND FIRST pallet WHERE
                  pallet.it-codigo = tt-digita.pal-it-codigo AND 
                  pallet.nr-pallet = tt-digita.pal-nr-pallet
                  USE-INDEX ITEM
                  NO-LOCK NO-ERROR.

           IF AVAIL pallet THEN DO:
           
              ASSIGN qtd-bobinas = qtd-bobinas + pallet.nr-bobinas.

              FOR EACH it-pallet OF pallet NO-LOCK :
               
               FIND LAST movto-mat 
                    where movto-mat.it-codigo = ped-item.it-codigo and
                          movto-mat.lote      = it-pallet.lote-bobina  AND
                          movto-mat.esp-docto = 1 
                          USE-INDEX lote
                          NO-LOCK NO-ERROR.
            
                IF NOT AVAIL movto-mat THEN next.
            
                FIND LAST movto-estoq 
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
                 MESSAGE "Erro execu‡Æo no programa externo"
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
                  movto-estoq.cod-depos <> "ARC"
                  USE-INDEX ord-seq   
                  NO-LOCK NO-ERROR.

                IF AVAIL movto-estoq THEN DO:
           
                   ASSIGN lote-rast = movto-estoq.lote
                          op-rast   = movto-estoq.nr-ord-produ
                          data-rast = movto-estoq.dt-trans.

                   RUN grava-rastreabilidade.

                END.
              END.



    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    ASSIGN nome-abrev-jr = ped-venda.nome-abrev.
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

IF v-num-reg-lidos > 0 THEN DO:
    
    FIND FIRST ped-venda WHERE 
         ped-venda.nr-pedcli = c-nr-pedcli-ini 
         USE-INDEX ch-nr-pedcli
         NO-LOCK NO-ERROR.

    IF AVAIL ped-venda THEN DO:
        FIND FIRST ped-item WHERE 
             ped-item.nome-abrev = ped-venda.nome-abrev AND
             ped-item.nr-pedcli = ped-venda.nr-pedcli   AND
             ped-item.ind-componen  < 3           AND
             ped-item.cod-refer <> "" 
             USE-INDEX ch-item-ped
             NO-LOCK NO-ERROR.

        IF AVAIL ped-item  THEN 
           ASSIGN it-codigo-ped = ped-item.it-codigo.

    END.

END.

ASSIGN data-jr = TODAY.

IF tt-param.destino <> 4 THEN DO:

    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp
        data-jr NO-LABEL
    with stream-io frame f-relat-lin-inic-1.
    down stream str-rp with frame f-relat-lin-inic-1.

    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp 
    with stream-io frame f-relat-linha.
    DOWN stream str-rp with frame f-relat-linha.

    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp
    with stream-io frame f-relat-lin-inic-2.
    down stream str-rp with frame f-relat-lin-inic-2.

    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp 
    with stream-io frame f-relat-linha.
    DOWN stream str-rp with frame f-relat-linha.

    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp
    with stream-io frame f-relat-lin-inic-3.
    down stream str-rp with frame f-relat-lin-inic-3.

    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp 
    with stream-io frame f-relat-linha.
    DOWN stream str-rp with frame f-relat-linha.

     view stream str-rp frame f-cabec.
     view stream str-rp frame f-rodape.
     assign l-imprime = yes.
     display stream str-rp
        tp-pedido-jr NO-LABEL
        c-nr-pedcli-ini NO-LABEL
        nome-abrev-jr NO-LABEL
        it-codigo-ped NO-LABEL
        diin-ped NO-LABEL
        diex-ped NO-LABEL
        larg-ped NO-LABEL
        qtd-bobinas NO-LABEL   
     with stream-io frame f-relat-lin-ped.
     down stream str-rp with frame f-relat-lin-ped.
     
     view stream str-rp frame f-cabec.
     view stream str-rp frame f-rodape.
     assign l-imprime = yes.
     display stream str-rp 
     with stream-io frame f-relat-linha.
     DOWN stream str-rp with frame f-relat-linha.
     
END.

ELSE DO:
    /* cabe‡alho do excel */

     ASSIGN i-linha = 7.

     IF tp-pedido-jr = "A" THEN
        ASSIGN c-relatorio:range("B" + STRING(i-linha)):VALUE = "(  X  )".

     IF tp-pedido-jr <> "A" THEN
        ASSIGN c-relatorio:range("E" + STRING(i-linha)):VALUE = "     (  X  )".

     ASSIGN i-linha = 76.

     FIND FIRST usuar_mestre WHERE
         usuar_mestre.cod_usuario = tt-param.usuario
         NO-LOCK NO-ERROR.

     IF AVAIL usuar_mestre THEN
         ASSIGN c-relatorio:range("B" + STRING(i-linha)):VALUE = usuar_mestre.nom_usuario.
            ELSE
              ASSIGN c-relatorio:range("B" + STRING(i-linha)):VALUE = "".

     ASSIGN c-relatorio:range("F" + STRING(i-linha)):VALUE = data-jr.
     
     ASSIGN c-relatorio:range("C" + "09"):VALUE = nome-abrev-jr
            c-relatorio:range("C" + "11"):VALUE = qtd-bobinas.
     
     ASSIGN c-relatorio:range("C" + "10"):VALUE = it-codigo-ped.

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
            qtdbob-x2 = "".

     FOR EACH tt-pedidos NO-LOCK.

         ASSIGN conta-ped = conta-ped + 1.

         IF conta-ped < 12 THEN
             ASSIGN pedido-x1 = (TRIM (pedido-x1) + " " + STRING (ttped-nr-pedido))
                    larg-x1 = (TRIM (larg-x1) + " " + STRING (ttped-larg))
                    diin-x1 = (TRIM (diin-x1) + " " + STRING (ttped-diin))
                    diex-x1 = (TRIM (diex-x1) + " " + STRING (ttped-diex))
                    qtdbob-x1 = (TRIM (qtdbob-x1) + " " + STRING (ttped-qtdbob)).
              ELSE
                  ASSIGN pedido-x2 = (TRIM (pedido-x2) + " " + STRING (ttped-nr-pedido))
                         larg-x2 = (TRIM (larg-x2) + " " + STRING (ttped-larg))
                         diin-x2 = (TRIM (diin-x2) + " " + STRING (ttped-diin))
                         diex-x2 = (TRIM (diex-x2) + " " + STRING (ttped-diex))
                         qtdbob-x2 = (TRIM (qtdbob-x2) + " " + STRING (ttped-qtdbob)).

         IF conta-ped < 7 AND ttped-pedcli <> pedcli-ant THEN
            ASSIGN pedcli-x1 = (TRIM (pedcli-x1) + " " + STRING (ttped-pedcli))
                   pedcli-ant = ttped-pedcli.
            ELSE
                IF conta-ped < 13 AND ttped-pedcli <> pedcli-ant THEN
                   ASSIGN pedcli-x2 = (TRIM (pedcli-x1) + " " + STRING (ttped-pedcli))
                          pedcli-ant = ttped-pedcli. 

     END.

         ASSIGN c-relatorio:range("C" + "12"):VALUE = nrnf-x1.
         ASSIGN c-relatorio:range("B" + "15"):VALUE = pedido-x1.
         ASSIGN c-relatorio:range("B" + "16"):VALUE = pedido-x2.

         ASSIGN c-relatorio:range("B" + "19"):VALUE = larg-x1.
         ASSIGN c-relatorio:range("B" + "20"):VALUE = larg-x2.

         ASSIGN c-relatorio:range("B" + "23"):VALUE = diex-x1.
         ASSIGN c-relatorio:range("B" + "24"):VALUE = diex-x2.

         ASSIGN c-relatorio:range("B" + "27"):VALUE = diin-x1.
         ASSIGN c-relatorio:range("B" + "28"):VALUE = diin-x2.

         ASSIGN c-relatorio:range("A" + "31"):VALUE = pallet-x1
                c-relatorio:range("A" + "32"):VALUE = pallet-x2
                c-relatorio:range("A" + "33"):VALUE = pallet-x3
                c-relatorio:range("A" + "34"):VALUE = pallet-x4
                c-relatorio:range("A" + "35"):VALUE = pallet-x5
                c-relatorio:range("A" + "36"):VALUE = pallet-x6
                c-relatorio:range("A" + "37"):VALUE = pallet-x7
                c-relatorio:range("A" + "38"):VALUE = pallet-x8.
     
         ASSIGN c-relatorio:range("A" + "40"):VALUE = pedcli-x1.
         ASSIGN c-relatorio:range("A" + "41"):VALUE = pedcli-x2.

     ASSIGN i-linha = 45.

END.

     FOR EACH tt-bobinas2 NO-LOCK:
         DELETE tt-bobinas2.
     END.

     FOR EACH tt-bobinas NO-LOCK:
         
         IF pesq-jr = 1 OR pesq-jr = 5 OR pesq-jr = 4 THEN DO:
       
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

            IF pesq-jr = 5 AND ttbob-linha < 100 THEN DO:
               FOR EACH movto-estoq NO-LOCK WHERE
                   movto-estoq.nr-ord-produ = ttbob-nr-ord-produ AND
                   movto-estoq.esp-docto = 1 AND
                   movto-estoq.cod-depos <> "ARC"
                   USE-INDEX ord-seq:

                   RUN grava-rastreabilidade-campanha.
               END.
            END.
         END.

         IF pesq-jr = 2 AND (INDEX (ttbob-it-codigo,"MR",1) <> 0) THEN DO:
       
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
         END.

         IF pesq-jr = 3 AND (((INDEX (ttbob-it-codigo,"MR",1) <> 0)) OR
             ttbob-linha > 199) THEN DO:
       
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
         END.


     END.

END.

/* Monta gramatura e espessura calculdas dos mill rolls de origem
   para o estabelecimento 423 ou 413. */


     ASSIGN tot-peso-jr1  = 0
            tot-fator-jr1 = 0
            tot-min-jr = 99999999
            tot-max-jr = 0
            gra-min-jr = 99999999
            gra-max-jr = 0.

     IF c-cod-estabel-ini = STRING({cdp\poloestab.i 423}) THEN DO:  /*solic-318*/ 

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
         where movto-mat.esp-docto = 1 AND
               movto-mat.it-codigo = ttbob2-it-codigo and
               movto-mat.lote      = ttbob2-lote AND 
               movto-mat.cod-depos <> "arc"
               USE-INDEX lote
               NO-LOCK NO-ERROR.

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
FOR EACH tt-analises NO-LOCK:
    DELETE tt-analises.
END.

FOR EACH tt-bobinas2 NO-LOCK:
         
   FOR EACH ficha-cq NO-LOCK WHERE
             ficha-cq.it-codigo = ttbob2-it-codigo AND
             ficha-cq.lote = ttbob2-lote
            USE-INDEX it-lote :

             FOR EACH pol-res-fic-cq-leitura NO-LOCK WHERE
                 pol-res-fic-cq-leitura.nr-ficha = ficha-cq.nr-ficha :
                 
                 IF DEC(pol-res-fic-cq-leitura.resultado)  = 0 THEN NEXT.

                 FIND FIRST tt-analises WHERE
                     ttana-cod-comp  = pol-res-fic-cq-leitura.cod-comp
                     NO-ERROR.

                 IF NOT AVAIL tt-analises THEN DO:
                     CREATE tt-analises.
                     ASSIGN ttana-cod-comp  = pol-res-fic-cq-leitura.cod-comp.
                 END.

                 ASSIGN ttana-it-codigo = pol-res-fic-cq-leitura.it-codigo
                        RESULT-jr = DEC (pol-res-fic-cq-leitura.resultado)
                        ttana-result = ttana-result + RESULT-jr
                        ttana-qtd-result = ttana-qtd-result + 1
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

FOR EACH tt-analises NO-LOCK:

          ASSIGN media-result = ttana-result / ttana-qtd-result.

          ASSIGN descricao-comp = STRING (it-codigo-ped + " - " + STRING(ttana-cod-comp))
                 tipo-result = "Aprovado"
                 unidade-jr = " "
                 metodo-jr = " "
                 valor-tipico-jr = 0.


          IF c-cod-estabel-ini = STRING({cdp\poloestab.i 423}) THEN  /*solic-318*/ 
              ASSIGN nome-estabel-jr = "POLO MG".
            ELSE
              ASSIGN nome-estabel-jr = "POLO RS".


          FIND FIRST polo-esp-cliente-cq WHERE
             polo-esp-cliente-cq.nome-abrev  = nome-estabel-jr  AND 
             polo-esp-cliente-cq.it-codigo = it-codigo-ped    AND
             polo-esp-cliente-cq.cod-comp  = ttana-cod-comp
             NO-LOCK NO-ERROR. 

          IF AVAIL polo-esp-cliente-cq   
             THEN 
              ASSIGN descricao-comp = polo-esp-cliente-cq.descricao
                   esp-min-jr     = polo-esp-cliente-cq.espec-min
                   esp-max-jr     = polo-esp-cliente-cq.espec-max
                   valor-tipico-jr = polo-esp-cliente-cq.espec-alvo
                   unidade-jr     = polo-esp-cliente-cq.unidade
                   metodo-jr      = polo-esp-cliente-cq.metodo.



          
    /***  CàDIGO PARA SAÖDA EM 132 COLUNAS ***/
           IF tt-param.destino <> 4 THEN DO:
      
             view stream str-rp frame f-cabec.
             view stream str-rp frame f-rodape.
             assign l-imprime = yes.
             display stream str-rp
               descricao-comp
               ttana-minimo
               ttana-maximo
               media-result
               ttana-qtd-result
               valor-tipico-jr
               unidade-jr
               metodo-jr
               with stream-io frame f-relat-09-132.
               down stream str-rp with frame f-relat-09-132.  
           END.

           ELSE DO:

             ASSIGN i-linha = i-linha + 1.

             ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = descricao-comp.

             ASSIGN c-relatorio:range("D" + STRING(i-linha)):VALUE = media-result.

             ASSIGN c-relatorio:range("E" + STRING(i-linha)):VALUE = valor-tipico-jr.

             IF unidade-jr = "g/m2" THEN unidade-jr = "g/mý".
             
             ASSIGN c-relatorio:range("F" + STRING(i-linha)):VALUE = unidade-jr 
                    c-relatorio:range("G" + STRING(i-linha)):VALUE = metodo-jr.

           END.  
END.

IF c-cod-estabel-ini = STRING({cdp\poloestab.i 423}) THEN  /*solic-318*/ 
   ASSIGN nome-estabel-jr = "POLO MG".
     ELSE
       ASSIGN nome-estabel-jr = "POLO RS".  


IF tot-peso-jr1 <> 0 THEN DO:

           FIND FIRST polo-esp-cliente-cq WHERE
                polo-esp-cliente-cq.nome-abrev  = nome-estabel-jr  AND 
                polo-esp-cliente-cq.it-codigo = it-codigo-ped    AND
                polo-esp-cliente-cq.cod-comp  = 70
                NO-LOCK NO-ERROR. 

           IF NOT AVAIL polo-esp-cliente-cq THEN DO:

               FIND FIRST polo-esp-cliente-cq WHERE
                    polo-esp-cliente-cq.nome-abrev  = nome-abrev-jr  AND 
                    polo-esp-cliente-cq.it-codigo = it-codigo-ped    AND
                    polo-esp-cliente-cq.cod-comp  = 70
                    NO-LOCK NO-ERROR. 

           END.

           IF AVAIL polo-esp-cliente-cq THEN 

               ASSIGN descricao-comp = polo-esp-cliente-cq.descricao
                      esp-min-jr     = tot-min-jr
                      esp-max-jr     = tot-max-jr
                      valor-tipico-jr = polo-esp-cliente-cq.espec-alvo
                      unidade-jr     = polo-esp-cliente-cq.unidade
                      metodo-jr      = polo-esp-cliente-cq.metodo.  
              ELSE                                                  
                  ASSIGN descricao-comp = "ESPESSURA"
                         esp-min-jr     = tot-min-jr
                         esp-max-jr     = tot-max-jr
                         valor-tipico-jr = 0
                         unidade-jr     = "MICRONS"
                         metodo-jr      = "". 

           IF fm-codigo-jr = "BSA" OR fm-codigo-jr = "BST" OR 
              fm-codigo-jr = "BPX" THEN                             
               ASSIGN densidade-jr = 0.913.                         
                ELSE                                                
                  ASSIGN densidade-jr = 0.905.

           ASSIGN media-result = ((tot-peso-jr1 / tot-fator-jr1) / densidade-jr)
                  * 1000000.

           IF tt-param.destino <> 4 THEN DO:

            view stream str-rp frame f-cabec.
            view stream str-rp frame f-rodape.
            assign l-imprime = yes.
            display stream str-rp
              descricao-comp
              media-result
              unidade-jr
              metodo-jr
            with stream-io frame f-relat-09-132.
            down stream str-rp with frame f-relat-09-132.  
           END.

           ELSE DO:

           ASSIGN i-linha = i-linha + 1.

           ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = descricao-comp.
           ASSIGN c-relatorio:range("D" + STRING(i-linha)):VALUE = media-result.
           ASSIGN c-relatorio:range("E" + STRING(i-linha)):VALUE = valor-tipico-jr.
           ASSIGN c-relatorio:range("F" + STRING(i-linha)):VALUE = unidade-jr 
                  c-relatorio:range("G" + STRING(i-linha)):VALUE = metodo-jr.

           END.  


           FIND FIRST polo-esp-cliente-cq WHERE
                polo-esp-cliente-cq.nome-abrev  = nome-estabel-jr  AND 
                polo-esp-cliente-cq.it-codigo = it-codigo-ped    AND
                polo-esp-cliente-cq.cod-comp  = 71
                NO-LOCK NO-ERROR. 

           IF NOT AVAIL polo-esp-cliente-cq THEN DO:

               FIND FIRST polo-esp-cliente-cq WHERE
                    polo-esp-cliente-cq.nome-abrev  = nome-abrev-jr  AND 
                    polo-esp-cliente-cq.it-codigo = it-codigo-ped    AND
                    polo-esp-cliente-cq.cod-comp  = 71
                    NO-LOCK NO-ERROR. 

           END.

           IF AVAIL polo-esp-cliente-cq THEN 

           ASSIGN descricao-comp = polo-esp-cliente-cq.descricao
                  esp-min-jr     = gra-min-jr
                  esp-max-jr     = gra-max-jr 
                  valor-tipico-jr = polo-esp-cliente-cq.espec-alvo
                  unidade-jr     = polo-esp-cliente-cq.unidade
                  metodo-jr      = polo-esp-cliente-cq.metodo.  
          ELSE                                                  
              ASSIGN descricao-comp = "GRAMATURA"
                     esp-min-jr     = gra-min-jr
                     esp-max-jr     = gra-max-jr 
                     valor-tipico-jr = 0
                     unidade-jr     = "G/M2"
                     metodo-jr      = "". 

       ASSIGN media-result = ((tot-peso-jr1 / tot-fator-jr1)
              * 1000000) .

       IF tt-param.destino <> 4 THEN DO:

        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp
          descricao-comp
          media-result
          unidade-jr
          metodo-jr
        with stream-io frame f-relat-09-132.
        down stream str-rp with frame f-relat-09-132.  
      END.

       ELSE DO:

     ASSIGN i-linha = i-linha + 1.

     ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = descricao-comp.
     ASSIGN c-relatorio:range("D" + STRING(i-linha)):VALUE = media-result.
     ASSIGN c-relatorio:range("E" + STRING(i-linha)):VALUE = valor-tipico-jr.
     ASSIGN c-relatorio:range("F" + STRING(i-linha)):VALUE = unidade-jr 
            c-relatorio:range("G" + STRING(i-linha)):VALUE = metodo-jr.

   END. 

END. 

IF tt-param.destino <> 4 THEN DO:

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.
assign l-imprime = yes.
display stream str-rp 
with stream-io frame f-relat-branco.
DOWN 2 stream str-rp with frame f-relat-branco.

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.
assign l-imprime = yes.
display stream str-rp 
with stream-io frame f-relat-linha.
DOWN stream str-rp with frame f-relat-linha.


   view stream str-rp frame f-cabec.
   view stream str-rp frame f-rodape.
   assign l-imprime = yes.
   display stream str-rp
   with stream-io frame f-relat-lin-final-1.
   down stream str-rp with frame f-relat-lin-final-1.

   view stream str-rp frame f-cabec.
   view stream str-rp frame f-rodape.
   assign l-imprime = yes.
   display stream str-rp 
   with stream-io frame f-relat-branco.
   DOWN 2 stream str-rp with frame f-relat-branco.
   
   view stream str-rp frame f-cabec.
   view stream str-rp frame f-rodape.
   assign l-imprime = yes.
   display stream str-rp 
   with stream-io frame f-relat-linha.
   DOWN stream str-rp with frame f-relat-linha.

   view stream str-rp frame f-cabec.
   view stream str-rp frame f-rodape.
   assign l-imprime = yes.
   display stream str-rp
   with stream-io frame f-relat-lin-final-2.
   down stream str-rp with frame f-relat-lin-final-2.

   view stream str-rp frame f-cabec.
   view stream str-rp frame f-rodape.
   assign l-imprime = yes.
   display stream str-rp 
   with stream-io frame f-relat-branco.
   DOWN 2 stream str-rp with frame f-relat-branco.
   
   view stream str-rp frame f-cabec.
   view stream str-rp frame f-rodape.
   assign l-imprime = yes.
   display stream str-rp 
   with stream-io frame f-relat-linha.
   DOWN stream str-rp with frame f-relat-linha.
   
   view stream str-rp frame f-cabec.
   view stream str-rp frame f-rodape.
   assign l-imprime = yes.
   display stream str-rp 
   with stream-io frame f-relat-branco.
   DOWN 2 stream str-rp with frame f-relat-branco.
   
   view stream str-rp frame f-cabec.
   view stream str-rp frame f-rodape.
   assign l-imprime = yes.
   display stream str-rp
   with stream-io frame f-relat-lin-final-3.
   down stream str-rp with frame f-relat-lin-final-3.

   view stream str-rp frame f-cabec.
   view stream str-rp frame f-rodape.
   assign l-imprime = yes.
   display stream str-rp 
   with stream-io frame f-relat-linha.
   DOWN stream str-rp with frame f-relat-linha.

END.

   IF tt-param.destino = 4 THEN DO:

      RUN pi-finaliza-impressao.
      RUN pi-finalizar IN h-acomp.

      RETURN 'OK'.

   END.

IF tt-param.destino <> 4 THEN DO:

if  l-imprime = no then do:
    if  tt-param.formato = 1 then do:
        view stream str-rp frame f-cabec-80.
        view stream str-rp frame f-rodape-80.
    end.

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
      c-cod-estabel-ini colon 23 
        with stream-io side-labels overlay row 034 frame f-imp-sel.

   disp stream str-rp "CLASSIFICA€ÇO" skip(01) with stream-io frame f-imp-cla.
   disp stream str-rp "   LAUDO DE QUALIDADE"
        with stream-io side-labels overlay row 034 frame f-imp-cla.

   put stream str-rp unformatted skip(1) "IMPRESSÇO" skip(1).

   put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
   put stream str-rp unformatted skip "    " "Execu‡Æo: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
   put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
   put stream str-rp unformatted skip "    " "Usu rio : " tt-param.usuario.

end.

else
    output stream str-rp close.

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

PROCEDURE ver-mr-trat.

    FIND FIRST movto-mat WHERE
               movto-mat.it-codigo = cmkt-req and
               movto-mat.lote = lote-req AND
               movto-mat.esp-docto = 1
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
                    movto-mat.esp-docto = 1
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


PROCEDURE pi-cria-planilha:

DEF VAR c-arquivo AS CHAR NO-UNDO.

    c-arquivo = c-arq + 'laudo' + STRING(time)+ '.xls'.

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


return 'OK'.

/* fim do programa */
