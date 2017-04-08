
/***************************************************************************
 **Programa.: escq0052.W 
 **Autor....: Amgra - Jos‚ Roberto
 **Objetivo.: Re-emissÆo de Laudo de Qualidade
 **Data.....: 01/06/2008
 **
 ***************************************************************************/
def buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "escq0052RP".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
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
    field c-cdd-embarq-ini    AS INTEGER 
    field c-cdd-embarq-fim    AS INTEGER 
    field l-envia-email        AS LOGICAL  
    FIELD pesq-jr              AS INT  
    FIELD i-modelo             AS INT
    field tb-v-temp            AS LOG  
.


DEFINE TEMP-TABLE tt-digita no-undo
    FIELD nome-abrev           LIKE emitente.nome-abrev LABEL "Cliente"
    FIELD nr-nota-fisc         LIKE nota-fiscal.nr-nota-fis LABEL "N.Fiscal"
    FIELD it-codigo            AS CHAR FORMAT "x(16)"   LABEL "Filme"
    FIELD nr-laudo             AS INT  FORMAT ">>>>>>9" LABEL "Nr.Laudo"
    INDEX chave IS PRIMARY UNIQUE nome-abrev  
                                  nr-nota-fisc
                                  it-codigo   
                                  nr-laudo.


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


DEFINE TEMP-TABLE tt-bob-prod
    FIELD lote           AS CHAR 
    FIELD dt-prod        AS DATE
    INDEX ch-tt-bobinas IS PRIMARY UNIQUE lote.


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
DEFINE VARIABLE pedido-x1     AS CHARACTER  FORMAT "x(90)" NO-UNDO.
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


DEFINE VARIABLE c-email-responsavel  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-nome-responsavel   AS CHARACTER  NO-UNDO.


DEFINE VARIABLE c-nr-pedido-1   AS INTEGER    NO-UNDO.
DEFINE VARIABLE media-jr2       AS DECIMAL    NO-UNDO.

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

DEFINE VARIABLE dens-ped-x      AS CHAR       NO-UNDO.
DEFINE VARIABLE dens-ot-jr      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE soma-dens       AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-arquivo       AS CHAR       NO-UNDO.

DEFINE VARIABLE nr-ext-jr       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE referencia-pm   AS CHARACTER  NO-UNDO.


DEFINE VARIABLE dias-jr         AS INTEGER    NO-UNDO.
DEFINE VARIABLE dt-validade-jr  AS DATE       NO-UNDO.

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

FORM "N£mero do Certificado:" AT 050 
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

assign c-programa     = "escq0052rp"
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


ASSIGN v-num-reg-lidos = 0. 

/* Gera‡Æo da Planilha Excel */

    /* Cria Aplica‡Æo do Excel */

CREATE "Excel.Application" c-excel.
ASSIGN c-excel:DisplayAlerts = FALSE.



FOR EACH tt-digita NO-LOCK,

    EACH am-cq-laudo WHERE
      am-cq-laudo.nr-laudo = tt-digita.nr-laudo
      USE-INDEX laudo exclusive-lock.


    IF am-cq-laudo.int-1 = 0 THEN
        ASSIGN c-modelo-planilha = search("modelos\mod-escq0051.xls") 
               c-arq             = SESSION:TEMP-DIRECTORY.

    IF am-cq-laudo.int-1 = 2 THEN
        ASSIGN c-modelo-planilha = search("modelos\mod-escq0051-3.xls") 
               c-arq             = SESSION:TEMP-DIRECTORY.


    IF am-cq-laudo.int-1 = 3 THEN
        ASSIGN c-modelo-planilha = search("modelos\mod-escq0051-4.xls") 
               c-arq             = SESSION:TEMP-DIRECTORY.



      RUN pi-cria-planilha.
      RUN pi-gera-planilha.
      RUN pi-mostra-planilha.
      RUN pi-gera-e-mail.

      ASSIGN am-cq-laudo.gerado-excel = YES.


END.

/* Fim da Gera‡Æo da Planilha Excel */


RUN pi-finaliza-impressao.


run pi-finalizar in h-acomp.

return 'OK'.

/* fim do programa */

/*------------------------ procedures  ------------------------*/

/* Include da procedure - pi-gera-planilha. */

{cqp\escq0051-i1.i}

PROCEDURE pi-cria-planilha:


    ASSIGN c-arq-anexo = am-cq-laudo.nome-abrev
           c-arq-anexo = REPLACE(c-arq-anexo,"/","-")
           c-arq-anexo = REPLACE(c-arq-anexo,"\","-")
           c-arq-anexo = REPLACE(c-arq-anexo," ","-")
           c-arq-anexo = REPLACE(c-arq-anexo,",","-").
    

    c-arquivo = c-arq + trim(c-arq-anexo) + '-' + 
                        trim(am-cq-laudo.nr-nota-fis) + '-' + 
                        string(am-cq-laudo.nr-laudo) + '.xls'.


    OS-COPY VALUE(c-modelo-planilha) VALUE(c-arquivo).

    assign c-planilha  = c-excel:Workbooks:OPEN(c-arquivo)
           c-relatorio = c-excel:Sheets:item(1)
           c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo.

END PROCEDURE.


PROCEDURE pi-mostra-planilha:
DEF VAR i         AS INT  NO-UNDO.


    c-planilha:SAVE().
    c-planilha:CLOSE().

    c-excel:VISIBLE = true.


        c-planilha  = c-excel:Workbooks:OPEN(c-arquivo).
if  tt-param.tb-v-temp then 
     os-copy value(c-arquivo) v:\temp.

END PROCEDURE.


PROCEDURE pi-gera-e-mail: 

    IF  tt-param.l-envia-email = YES THEN DO: 

        FIND usuar_mestre WHERE
                usuar_mestre.cod_usuario = "aschnei" /*am-cq-laudo.cod-usuario*/
                NO-LOCK NO-ERROR.


         FOR FIRST ext_usuar_grp_usuar WHERE 
                    ext_usuar_grp_usuar.ativo AND
                    ext_usuar_grp_usuar.cod_grp_usuar = "LAUDORESP"   NO-LOCK.


                 FIND FIRST usuar_mestre WHERE
                 usuar_mestre.cod_usuario = ext_usuar_grp_usuar.cod_usuario
                 NO-LOCK NO-ERROR.
        
            

         END.


     
        IF NOT AVAIL usuar_mestre THEN RETURN "NOK".

            ASSIGN c-email-responsavel = usuar_mestre.cod_e_mail_local 
                   c-nome-responsavel  = usuar_mestre.nom_usuar.

            FIND FIRST param-global NO-LOCK NO-ERROR.

            IF NOT AVAIL param-global THEN DO:
                MESSAGE "NÆo encontrado parametro global"
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN "nok".
            END.

        FIND polo-laudo-cliente WHERE
            polo-laudo-cliente.nome-abrev = am-cq-laudo.nome-abrev
            NO-LOCK NO-ERROR.

        IF NOT AVAIL polo-laudo-cliente THEN RETURN "NOK".


        IF c-email-responsavel = "" OR 
            polo-laudo-cliente.end-email = "" THEN RETURN "NOK".


        ASSIGN nr-ext-jr = "EXT. Nr.: ".

        FIND FIRST emitente WHERE
            emitente.cod-emitente = am-cq-laudo.cod-emitente
            NO-LOCK NO-ERROR.

        IF AVAIL emitente AND emitente.natureza = 3 THEN
            ASSIGN nr-ext-jr = nr-ext-jr + TRIM(am-cq-laudo.observacao).
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
            tt-envio.destino     =  polo-laudo-cliente.end-email
            tt-envio.copia       =  "carine.zago@polofilms.com.br,julia.hamann@polofilms.com.br"
            tt-envio.assunto     =  nr-ext-jr + " Certificado de Qualidade - Certificate of Analysis - Polo"  

            tt-envio.mensagem    = "Srs," + chr(10) + CHR(10) + 
            "Segue anexo os certificados de Qualidade referente aos pedidos numero: " + CHR(10) +
            trim(am-cq-laudo.pedido) + CHR(10) + CHR(10) +
            nr-ext-jr + CHR(10) + CHR(10) +
            "Favor confirmar o recebimento desse e-mail." + CHR(10) +
            CHR(10) + 
            chr(10) + CHR(10) + "Obrigado." + CHR(10) + CHR(10) + c-nome-responsavel +
            chr(10) + CHR(10) + chr(10) + CHR(10) +

            "Dear Sirs," + chr(10) + CHR(10) +
            "Please find attached orderïs certificate of analysis number:" + CHR(10) +
            trim(am-cq-laudo.pedido) + CHR(10) + CHR(10) +
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


        OUTPUT TO VALUE(c-arq + "escq0051MAIL.TMP").
        run utp/utapi009.p (input  table tt-envio,
                            output table tt-erros).     
        OUTPUT CLOSE. 


        FIND FIRST tt-erros NO-LOCK NO-ERROR.

        FIND FIRST tt-erros NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-erros THEN
           ASSIGN am-cq-laudo.enviado-email  = YES
                  am-cq-laudo.dt-envio-email = TODAY.
           
        RETURN "OK".

    END.


END PROCEDURE.


PROCEDURE pi-finaliza-impressao:

    RELEASE OBJECT c-excel.

END PROCEDURE.



