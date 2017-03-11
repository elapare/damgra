/*****************************************************************************
**
**       Programa: escp0063rp.p
**
**       Data....: 28/07/2008
**
**       Autor...: Amgra - Jos‚ Roberto
**
**       Objetivo: Posi‡Æo Di ria - PCP
**
**       VersÆo..: 1.00.000.000 
**
**       OBS.....: 
**
*******************************************************************************/
define variable c-prog-gerado as character no-undo initial "escp0063RP".
define buffer empresa for mgmulti.empresa.
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
    field txt-pis-cofins       as decimal
    field c-cod-estabel-ini    like nota-fiscal.cod-estabel
    field c-canal-venda-ini    like nota-fiscal.cod-canal-venda
    field dt-data-hoje         like nota-fiscal.dt-emis-nota
    field dt-data-ontem        like nota-fiscal.dt-emis-nota
    field dt-data-inicio       like nota-fiscal.dt-emis-nota.


DEFINE TEMP-TABLE tt-dados
    FIELD cod-estabel          LIKE movto-estoq.cod-estabel

    FIELD pedidos-mi-p         AS DECIMAL EXTENT 2 
    FIELD pedidos-me-p         AS DECIMAL EXTENT 2
    FIELD pedidos-mi-e         AS DECIMAL EXTENT 2 
    FIELD pedidos-me-e         AS DECIMAL EXTENT 2
    FIELD budget-mi            AS DECIMAL
    FIELD budget-me            AS DECIMAL
    FIELD cotac-mi             AS DECIMAL
    FIELD cotac-me             AS DECIMAL
    FIELD prod-mi-p            AS DECIMAL 
    FIELD prod-me-p            AS DECIMAL 
    FIELD prod-mi-e            AS DECIMAL 
    FIELD prod-me-e            AS DECIMAL 
    FIELD ped-prx-mes-mi-p     AS DECIMAL
    FIELD ped-prx-mes-me-p     AS DECIMAL
    FIELD prod-prx-mes-mi-p    AS DECIMAL
    FIELD prod-prx-mes-me-p    AS DECIMAL
    FIELD ped-prx-mes-mi-e     AS DECIMAL
    FIELD ped-prx-mes-me-e     AS DECIMAL
    FIELD prod-prx-mes-mi-e    AS DECIMAL
    FIELD prod-prx-mes-me-e    AS DECIMAL

    FIELD pedidos-mi-tms-p     AS DECIMAL EXTENT 2 
    FIELD pedidos-me-tms-p     AS DECIMAL EXTENT 2
    FIELD pedidos-mi-tms-e     AS DECIMAL EXTENT 2 
    FIELD pedidos-me-tms-e     AS DECIMAL EXTENT 2
    FIELD budget-mi-tms        AS DECIMAL
    FIELD budget-me-tms        AS DECIMAL
    FIELD cotac-mi-tms         AS DECIMAL
    FIELD cotac-me-tms         AS DECIMAL
    FIELD prod-mi-tms-p        AS DECIMAL 
    FIELD prod-me-tms-p        AS DECIMAL 
    FIELD prod-mi-tms-e        AS DECIMAL 
    FIELD prod-me-tms-e        AS DECIMAL 
    FIELD ped-prx-mes-mi-tms-p  AS DECIMAL
    FIELD ped-prx-mes-me-tms-p  AS DECIMAL
    FIELD prod-prx-mes-mi-tms-p AS DECIMAL
    FIELD prod-prx-mes-me-tms-p AS DECIMAL
    FIELD ped-prx-mes-mi-tms-e  AS DECIMAL
    FIELD ped-prx-mes-me-tms-e  AS DECIMAL
    FIELD prod-prx-mes-mi-tms-e AS DECIMAL
    FIELD prod-prx-mes-me-tms-e AS DECIMAL

    FIELD volume-ln-1          AS DECIMAL EXTENT 2 
    FIELD reprov-ln-1          AS DECIMAL
    FIELD reserv-ped-ln-1      AS DECIMAL

    FIELD volume-ln-2          AS DECIMAL EXTENT 2 
    FIELD reprov-ln-2          AS DECIMAL
    FIELD reserv-ped-ln-2      AS DECIMAL

    FIELD volume-ln-3          AS DECIMAL EXTENT 2 
    FIELD reprov-ln-3          AS DECIMAL
    FIELD reserv-ped-ln-3      AS DECIMAL

    FIELD est-prime-pedido-421 AS DECIMAL EXTENT 2 
    FIELD est-prime-pedido-422 AS DECIMAL EXTENT 2 
    FIELD est-prime-pedido-426 AS DECIMAL EXTENT 2 
    FIELD est-prime-pedido-tsg AS DECIMAL EXTENT 2 
    FIELD est-prime-pedido-rds AS DECIMAL EXTENT 2 
    FIELD est-prime-pedido-gre AS DECIMAL EXTENT 2 
    FIELD est-prime-pedido-out AS DECIMAL EXTENT 2 

    FIELD est-prime-sem-pd-421 AS DECIMAL EXTENT 2 
    FIELD est-prime-sem-pd-422 AS DECIMAL EXTENT 2 
    FIELD est-prime-sem-pd-426 AS DECIMAL EXTENT 2 
    FIELD est-prime-sem-pd-tsg AS DECIMAL EXTENT 2 
    FIELD est-prime-sem-pd-rds AS DECIMAL EXTENT 2 
    FIELD est-prime-sem-pd-gre AS DECIMAL EXTENT 2 
    FIELD est-prime-sem-pd-out AS DECIMAL EXTENT 2 

    FIELD est-prime-aloc-venda AS DECIMAL EXTENT 2 
    FIELD est-prime-aloc-recor AS DECIMAL EXTENT 2 
    FIELD est-prime-aloc-metal AS DECIMAL EXTENT 2 
    FIELD est-prime-aloc-restr AS DECIMAL EXTENT 2 

    FIELD est-prime-cliente    AS DECIMAL EXTENT 2 
    FIELD est-prime-ep         AS DECIMAL EXTENT 2 



    FIELD est-offsp-pedido-421 AS DECIMAL EXTENT 2 
    FIELD est-offsp-pedido-422 AS DECIMAL EXTENT 2 
    FIELD est-offsp-pedido-426 AS DECIMAL EXTENT 2 
    FIELD est-offsp-pedido-tsg AS DECIMAL EXTENT 2 
    FIELD est-offsp-pedido-rds AS DECIMAL EXTENT 2 
    FIELD est-offsp-pedido-gre AS DECIMAL EXTENT 2 
    FIELD est-offsp-pedido-out AS DECIMAL EXTENT 2 

    FIELD est-offsp-sem-pd-421 AS DECIMAL EXTENT 2 
    FIELD est-offsp-sem-pd-422 AS DECIMAL EXTENT 2 
    FIELD est-offsp-sem-pd-426 AS DECIMAL EXTENT 2 
    FIELD est-offsp-sem-pd-tsg AS DECIMAL EXTENT 2 
    FIELD est-offsp-sem-pd-rds AS DECIMAL EXTENT 2 
    FIELD est-offsp-sem-pd-gre AS DECIMAL EXTENT 2 
    FIELD est-offsp-sem-pd-out AS DECIMAL EXTENT 2 

    FIELD est-offsp-aloc-venda AS DECIMAL EXTENT 2 
    FIELD est-offsp-aloc-recor AS DECIMAL EXTENT 2 
    FIELD est-offsp-aloc-metal AS DECIMAL EXTENT 2 
    FIELD est-offsp-aloc-restr AS DECIMAL EXTENT 2 

    FIELD est-offsp-cliente    AS DECIMAL EXTENT 2 
    FIELD est-offsp-ep         AS DECIMAL EXTENT 2 

    FIELD fat-prime-int-421    AS DECIMAL EXTENT 4 
    FIELD fat-prime-int-422    AS DECIMAL EXTENT 4 
    FIELD fat-prime-int-426    AS DECIMAL EXTENT 4 
    FIELD fat-prime-int-out    AS DECIMAL EXTENT 4 

    FIELD fat-prime-ext-421    AS DECIMAL EXTENT 4 
    FIELD fat-prime-ext-422    AS DECIMAL EXTENT 4 
    FIELD fat-prime-ext-426    AS DECIMAL EXTENT 4 
    FIELD fat-prime-ext-out    AS DECIMAL EXTENT 4 

    FIELD fat-int-amostra      AS DECIMAL EXTENT 4 
    FIELD fat-int-off-specs    AS DECIMAL EXTENT 4 
    FIELD fat-int-off-prev     AS DECIMAL  

    FIELD fat-ext-amostra      AS DECIMAL EXTENT 4 
    FIELD fat-ext-off-specs    AS DECIMAL EXTENT 4 
    FIELD fat-ext-off-prev     AS DECIMAL  

    FIELD fat-doacao           AS DECIMAL EXTENT 4 
    FIELD fat-refugo           AS DECIMAL EXTENT 4 
    FIELD fat-refugo-prev      AS DECIMAL 

    FIELD qt-devolucao-mi      AS DECIMAL 
    FIELD qt-devolucao-me      AS DECIMAL 

    FIELD cons-mat-prima-dia   AS DECIMAL  

    INDEX ch-tt-dados IS PRIMARY UNIQUE  cod-estabel. 


DEFINE TEMP-TABLE tt-grupo-mat
    FIELD cod-estabel          LIKE movto-estoq.cod-estabel
    FIELD tipo-grupo           LIKE am-cp-pcp-grupo-mat.tipo-grupo
    FIELD cod-grupo            LIKE am-cp-pcp-grupo-mat.cod-grupo

    FIELD descricao            LIKE am-cp-pcp-grupo-mat.descricao
    FIELD target               LIKE am-cp-pcp-grupo-mat.target
    FIELD consumo-medio        AS DECIMAL 
    FIELD estoque-hoje         AS DECIMAL 
    FIELD entradas-fora        AS DECIMAL 
    FIELD saidas-fora          AS DECIMAL 
    FIELD estoque-polo         AS DECIMAL 
    FIELD estoque-externo      AS DECIMAL 

    INDEX ch-tt-grupo-mat IS PRIMARY UNIQUE  cod-estabel
                                             tipo-grupo
                                             cod-grupo. 


DEFINE TEMP-TABLE tt-lotes
    FIELD cod-estabel          LIKE saldo-estoq.cod-estabel
    FIELD it-codigo            LIKE saldo-estoq.it-codigo  
    FIELD lote                 LIKE saldo-estoq.lote       
    FIELD cod-depos            LIKE saldo-estoq.cod-depos  
    FIELD saldo-atu            AS DECIMAL 
    FIELD saldo-hoje           AS DECIMAL 
    FIELD saldo-ontem          AS DECIMAL 
    FIELD variacao-ontem       AS DECIMAL
    FIELD entradas             AS DECIMAL 
    FIELD saidas               AS DECIMAL 

    INDEX ch-tt-lotes     IS PRIMARY UNIQUE  cod-estabel
                                             it-codigo
                                             lote
                                             cod-depos. 


DEFINE TEMP-TABLE tt-notas-terc NO-UNDO
    FIELD nro-docto    LIKE saldo-terc.nro-docto
    FIELD sequencia    LIKE saldo-terc.sequencia
    FIELD saldo        LIKE saldo-terc.quantidade
    FIELD cod-emitente LIKE saldo-terc.cod-emitente
    FIELD serie        LIKE saldo-terc.serie
    FIELD cod-estabel  LIKE saldo-terc.cod-estabel
    FIELD it-codigo    LIKE saldo-terc.it-codigo
    INDEX chave-notas IS PRIMARY UNIQUE nro-docto
                                        sequencia.

DEFINE TEMP-TABLE tt-lotes-terc NO-UNDO
    FIELD nro-docto     LIKE saldo-terc.nro-docto
    FIELD sequencia     LIKE saldo-terc.sequencia
    FIELD lote          LIKE saldo-terc.lote
    FIELD remessa       LIKE saldo-terc.quantidade
    FIELD retorno       LIKE saldo-terc.quantidade
    FIELD saldo         LIKE saldo-terc.quantidade
    FIELD remessa-ontem LIKE saldo-terc.quantidade
    FIELD retorno-ontem LIKE saldo-terc.quantidade
    FIELD saldo-ontem   LIKE saldo-terc.quantidade
    FIELD cod-estabel   LIKE saldo-terc.cod-estabel
    FIELD cod-emitente  LIKE saldo-terc.cod-emitente
    FIELD it-codigo     LIKE saldo-terc.it-codigo
    INDEX chave-lotes IS PRIMARY UNIQUE nro-docto
                                        sequencia
                                        lote.






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

def new shared var c-cod-estabel-ini  like nota-fiscal.cod-estabel     format "X(3)" initial "422" no-undo.
def new shared var c-canal-venda-ini  like nota-fiscal.cod-canal-venda format ">>9" initial 0 no-undo.
def new shared var dt-data-hoje       like nota-fiscal.dt-emis-nota    format "99/99/9999" initial today no-undo.
def new shared var dt-data-ontem      like nota-fiscal.dt-emis-nota    format "99/99/9999" initial today no-undo.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

DEFINE VARIABLE mes-jr                AS INTEGER    NO-UNDO.
DEFINE VARIABLE data-prx-jr           AS DATE       NO-UNDO.
DEFINE VARIABLE data-fim-prx-jr       AS DATE       NO-UNDO.
DEFINE VARIABLE esp-jr                AS INTEGER    NO-UNDO.
DEFINE VARIABLE data-fim-mes-jr       AS DATE       NO-UNDO.

DEFINE VARIABLE saldo-atu-jr          AS DECIMAL    NO-UNDO.
DEFINE VARIABLE entradas-jr           AS DECIMAL    NO-UNDO.
DEFINE VARIABLE saidas-jr             AS DECIMAL    NO-UNDO.

DEFINE VARIABLE saldo-atu-ontem       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE entradas-ontem-jr     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE saidas-ontem-jr       AS DECIMAL    NO-UNDO.

DEFINE VARIABLE merc-jr               AS CHARACTER  NO-UNDO.

DEFINE VARIABLE qtde-prod-jr          AS DECIMAL    NO-UNDO.
DEFINE VARIABLE qtde-jr-t             AS DECIMAL    NO-UNDO.
DEFINE VARIABLE soma-terc-jr          AS DECIMAL    NO-UNDO.
DEFINE VARIABLE qtde-jr-tr            AS DECIMAL    NO-UNDO.
DEFINE VARIABLE soma-terc-tr          AS DECIMAL    NO-UNDO.
DEFINE VARIABLE var-terc              AS DECIMAL    NO-UNDO.
DEFINE VARIABLE var-transito          AS DECIMAL    NO-UNDO.

DEFINE VARIABLE tem-pedido-jr         AS LOGICAL    NO-UNDO.

DEFINE VARIABLE txt-pis-cofins        AS decimal    NO-UNDO.

DEFINE VARIABLE tp-pedido-jr          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE tp-produto-jr         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE var-tp-merc           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE var-vl-liq            AS DECIMAL    NO-UNDO.
DEFINE VARIABLE var-pis-cofins        AS DECIMAL    NO-UNDO.
DEFINE VARIABLE qt-faturada-jr        AS DECIMAL    NO-UNDO.

DEFINE VARIABLE soma-fat-int          AS DECIMAL    NO-UNDO.
DEFINE VARIABLE soma-fat-ext          AS DECIMAL    NO-UNDO.

DEFINE VARIABLE qt-devolucao-mi       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE qt-devolucao-me       AS DECIMAL    NO-UNDO.


/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 
DEFINE VARIABLE meses-x      AS CHARACTER  INITIAL
     "JANEIRO  FEVEREIROMAR€O    ABRIL    MAIO     JUNHO    JULHO    AGOSTO   SETEMBRO OUTUBRO  NOVEMBRO DEZEMBRO" NO-UNDO.
DEFINE VARIABLE mes-cab      AS CHARACTER  FORMAT "x(10)" NO-UNDO. 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/* para executar o Excel */

DEF VAR c-arq             AS CHAR                NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR                NO-UNDO.   
DEF VAR l-cria            AS LOG                 NO-UNDO.
DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
DEF VAR i-linha           AS INT                 NO-UNDO.
DEF VAR c-coluna          AS char                NO-UNDO.

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.

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

form HEADER
    fill("-", 170) format "x(170)" SKIP 
    WITH DOWN WIDTH 170 NO-BOX STREAM-IO FRAME f-relat-linha.

form HEADER
    fill(" ", 170) format "x(170)" SKIP 
    WITH DOWN WIDTH 170 NO-BOX STREAM-IO FRAME f-relat-branco.


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

assign c-programa     = "escp0063rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Posi‡Æo Di ria - PCP"
       c-sistema      = "".

form header
    fill("-", 170) format "x(170)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 159 page-number(str-rp) at 166 format ">>>>9" skip
    fill("-", 148) format "x(148)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") 
    mes-cab AT 01 "  TOTAL" SKIP
    fill("-", 170) format "x(170)" skip
    with stream-io width 170 no-labels no-box page-top frame f-cabec.


run grapi/gr2004.p.

form header
    c-rodape format "x(170)"
    with stream-io width 170 no-labels no-box page-bottom frame f-rodape.

run grapi/gr2013c.p (input tt-param.destino,
                    input tt-param.arquivo,
                    input tt-param.usuario,
                    input no).

    assign i-ep-codigo-usuario = tt-param.ep-codigo
           v_cdn_empres_usuar  = i-ep-codigo-usuario.
           
    
         
find first empresa no-lock
    where empresa.ep-codigo = tt-param.ep-codigo no-error.
if  avail empresa
then
    assign c-empresa = empresa.razao-social.
else
    assign c-empresa = "".


    assign c-cod-estabel-ini  = tt-param.c-cod-estabel-ini 
           dt-data-hoje       = tt-param.dt-data-hoje      
           dt-data-ontem      = tt-param.dt-data-ontem     
           dt-data-inicio     = tt-param.dt-data-inicio    
           c-canal-venda-ini  = tt-param.c-canal-venda-ini .



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

run pi-inicializar in h-acomp(input "Produ‡Æo no Per¡odo:").

assign v-num-reg-lidos = 0.

ASSIGN mes-jr = MONTH (dt-data-hoje). 

ASSIGN mes-cab = SUBSTRING (meses-x,((mes-jr * 9) - 8),9).

ASSIGN data-prx-jr = dt-data-inicio + 32
       data-prx-jr = DATE (MONTH (data-prx-jr),1,YEAR (data-prx-jr))
       data-fim-mes-jr = data-prx-jr - 1
       data-fim-prx-jr = data-prx-jr + 32.
       data-fim-prx-jr = (DATE (MONTH (data-fim-prx-jr),1,YEAR (data-fim-prx-jr))) - 1.


IF tt-param.destino = 4 THEN DO:
    
    /* Cria Aplica‡Æo do Excel */

    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

    ASSIGN c-modelo-planilha = search("modelos\mod-escp0063.xls") 
           c-arq             = SESSION:TEMP-DIRECTORY.

    RUN pi-cria-planilha.  

END.

IF tt-param.destino = 4 THEN DO:

    ASSIGN i-linha = 2.

    ASSIGN c-relatorio:range("G" + STRING(i-linha)):VALUE = mes-cab.
    
    ASSIGN c-relatorio:range("I" + STRING(i-linha)):VALUE = dt-data-hoje.  

END.

FOR EACH tt-dados :
    DELETE tt-dados.
END.

ASSIGN txt-pis-cofins = 9.25.



/* Rotina para buscar parƒmetros do dia e do mˆs */

CREATE tt-dados.

ASSIGN tt-dados.cod-estabel = c-cod-estabel-ini.

FIND FIRST am-cp-pcp-param-dia WHERE
           am-cp-pcp-param-dia.cod-estabel = c-cod-estabel-ini AND
           am-cp-pcp-param-dia.data-param  = dt-data-hoje
           NO-LOCK NO-ERROR.

IF AVAIL am-cp-pcp-param-dia THEN DO:

    ASSIGN tt-dados.cotac-mi                  = am-cp-pcp-param-dia.previsao-cotac-mi
           tt-dados.cotac-me                  = am-cp-pcp-param-dia.previsao-cotac-me
           tt-dados.cotac-mi-tms              = am-cp-pcp-param-dia.previsao-tms-mi
           tt-dados.cotac-me-tms              = am-cp-pcp-param-dia.previsao-tms-me
           tt-dados.reserv-ped-ln-1           = am-cp-pcp-param-dia.mr-pedidos-ln1
           tt-dados.reserv-ped-ln-2           = am-cp-pcp-param-dia.mr-pedidos-ln2
           tt-dados.reserv-ped-ln-3           = am-cp-pcp-param-dia.mr-pedidos-ln3
           tt-dados.est-prime-aloc-venda [1]  = am-cp-pcp-param-dia.est-prime-venda
           tt-dados.est-prime-aloc-recor [1]  = am-cp-pcp-param-dia.est-prime-recorte
           tt-dados.est-prime-aloc-metal [1]  = am-cp-pcp-param-dia.est-prime-base-metal
           tt-dados.est-prime-aloc-restr [1]  = am-cp-pcp-param-dia.est-prime-restricao
           tt-dados.est-offsp-aloc-venda [1]  = am-cp-pcp-param-dia.est-off-specs-venda
           tt-dados.est-offsp-aloc-recor [1]  = am-cp-pcp-param-dia.est-off-specs-recorte
           tt-dados.est-offsp-aloc-metal [1]  = am-cp-pcp-param-dia.est-off-base-metal
           tt-dados.est-offsp-aloc-restr [1]  = am-cp-pcp-param-dia.est-off-specs-restricao.

END.


FIND FIRST am-cp-pcp-param-dia WHERE
           am-cp-pcp-param-dia.cod-estabel = c-cod-estabel-ini AND
           am-cp-pcp-param-dia.data-param  = dt-data-ontem
           NO-LOCK NO-ERROR.

IF AVAIL am-cp-pcp-param-dia THEN DO:

    ASSIGN tt-dados.est-prime-aloc-venda [2]  = am-cp-pcp-param-dia.est-prime-venda
           tt-dados.est-prime-aloc-recor [2]  = am-cp-pcp-param-dia.est-prime-recorte
           tt-dados.est-prime-aloc-metal [2]  = am-cp-pcp-param-dia.est-prime-base-metal
           tt-dados.est-prime-aloc-restr [2]  = am-cp-pcp-param-dia.est-prime-restricao
           tt-dados.est-offsp-aloc-venda [2]  = am-cp-pcp-param-dia.est-off-specs-venda
           tt-dados.est-offsp-aloc-recor [2]  = am-cp-pcp-param-dia.est-off-specs-recorte
           tt-dados.est-offsp-aloc-metal [2]  = am-cp-pcp-param-dia.est-off-base-metal
           tt-dados.est-offsp-aloc-restr [2]  = am-cp-pcp-param-dia.est-off-specs-restricao.

END.


FIND FIRST am-cp-pcp-param-mes WHERE
           am-cp-pcp-param-mes.cod-estabel = c-cod-estabel-ini    AND
           am-cp-pcp-param-mes.mes-per     = MONTH (dt-data-hoje) AND
           am-cp-pcp-param-mes.ano-per     = YEAR (dt-data-hoje)
           NO-LOCK NO-ERROR.

IF AVAIL am-cp-pcp-param-mes THEN DO:

    ASSIGN tt-dados.budget-mi              = am-cp-pcp-param-mes.vendas-prod-mi
           tt-dados.budget-me              = am-cp-pcp-param-mes.vendas-prod-me
           tt-dados.budget-mi-tms          = am-cp-pcp-param-mes.vendas-tms-mi
           tt-dados.budget-me-tms          = am-cp-pcp-param-mes.vendas-tms-me
           tt-dados.fat-int-off-prev       = am-cp-pcp-param-mes.vendas-off-spc-mi
           tt-dados.fat-ext-off-prev       = am-cp-pcp-param-mes.vendas-off-spc-me
           tt-dados.fat-refugo-prev        = am-cp-pcp-param-mes.vendas-refugo-mi + am-cp-pcp-param-mes.vendas-refugo-me
           tt-dados.cons-mat-prima-dia     = am-cp-pcp-param-mes.cons-mat-prima-dia.

    ASSIGN c-relatorio:range ("N8"):VALUE  = am-cp-pcp-param-mes.capac-prod-dia-corte / 1000
           c-relatorio:range ("N23"):VALUE = am-cp-pcp-param-mes.capac-prod-dia-metal / 1000
           c-relatorio:range ("N34"):VALUE = am-cp-pcp-param-mes.capac-prod-dia-ln1 / 1000
           c-relatorio:range ("N35"):VALUE = am-cp-pcp-param-mes.capac-prod-dia-ln2 / 1000
           c-relatorio:range ("N36"):VALUE = am-cp-pcp-param-mes.capac-prod-dia-ln3 / 1000
           c-relatorio:range ("G161"):VALUE = am-cp-pcp-param-mes.cons-mat-prima-dia / 1000
           c-relatorio:range ("I161"):VALUE = am-cp-pcp-param-mes.target-mat-prima-dias.


END.

/* Fim da Rotina para buscar parƒmetros do dia e do mˆs */


/* Rotina para buscar estoques de Mill Rolls */

FOR EACH tt-lotes.
    DELETE tt-lotes.
END.

FOR EACH ITEM NO-LOCK WHERE
    ITEM.GE-codigo = 41  
    USE-INDEX grupo.
        
    IF INDEX(ITEM.it-codigo,"MR",1) = 0 THEN NEXT.
    
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    FOR EACH saldo-estoq WHERE
        saldo-estoq.cod-estabel = c-cod-estabel-ini AND
        saldo-estoq.it-codigo   = item.it-codigo    AND
        saldo-estoq.lote <> "recicl"                AND
        saldo-estoq.cod-depos <> "ARC"              AND
        saldo-estoq.qtidade-atu <> 0
        USE-INDEX estabel-item
        NO-LOCK.

        FIND FIRST tt-lotes WHERE
            tt-lotes.cod-estabel = saldo-estoq.cod-estabel  AND
            tt-lotes.it-codigo   = saldo-estoq.it-codigo    AND
            tt-lotes.lote        = saldo-estoq.lote         AND
            tt-lotes.cod-depos   = saldo-estoq.cod-depos   
            NO-LOCK NO-ERROR.
       
        IF NOT AVAIL tt-lotes THEN DO:
       
            CREATE tt-lotes.
            ASSIGN tt-lotes.cod-estabel = saldo-estoq.cod-estabel 
                   tt-lotes.it-codigo   = saldo-estoq.it-codigo   
                   tt-lotes.lote        = saldo-estoq.lote        
                   tt-lotes.cod-depos   = saldo-estoq.cod-depos.
       
        END.
       
        ASSIGN tt-lotes.saldo-atu = tt-lotes.saldo-atu + saldo-estoq.qtidade-atu.

    END. /* for each saldo-estoq */

    FOR EACH movto-estoq NO-LOCK WHERE
        movto-estoq.it-codigo   = item.it-codigo          AND
        movto-estoq.cod-estabel = c-cod-estabel-ini       AND
        movto-estoq.dt-trans    > tt-param.dt-data-ontem
        USE-INDEX item-data.

        FIND FIRST tt-lotes WHERE
            tt-lotes.cod-estabel = movto-estoq.cod-estabel  AND
            tt-lotes.it-codigo   = movto-estoq.it-codigo    AND
            tt-lotes.lote        = movto-estoq.lote         AND
            tt-lotes.cod-depos   = movto-estoq.cod-depos   
            NO-LOCK NO-ERROR.
       
        IF NOT AVAIL tt-lotes THEN DO:
       
            CREATE tt-lotes.

            ASSIGN tt-lotes.cod-estabel = movto-estoq.cod-estabel 
                   tt-lotes.it-codigo   = movto-estoq.it-codigo   
                   tt-lotes.lote        = movto-estoq.lote        
                   tt-lotes.cod-depos   = movto-estoq.cod-depos.

        END.

        IF movto-estoq.dt-trans <= tt-param.dt-data-hoje THEN DO:
    
           IF movto-estoq.tipo-trans = 1 THEN
               ASSIGN tt-lotes.variacao-ontem = tt-lotes.variacao-ontem - movto-estoq.quantidade.
           ELSE
               ASSIGN tt-lotes.variacao-ontem = tt-lotes.variacao-ontem + movto-estoq.quantidade.
    
        END.
    
        IF movto-estoq.dt-trans > tt-param.dt-data-hoje THEN DO:
    
           IF movto-estoq.tipo-trans = 1 THEN
               ASSIGN tt-lotes.entradas = tt-lotes.entradas + movto-estoq.quantidade.
           ELSE
               ASSIGN tt-lotes.saidas   = tt-lotes.saidas   + movto-estoq.quantidade.
    
        END.
        
    END. /* for each movto-estoq */

    
END. /* for each item */

FOR EACH tt-lotes.

    ASSIGN tt-lotes.saldo-atu   = tt-lotes.saldo-atu - tt-lotes.entradas + 
           tt-lotes.saidas
           tt-lotes.saldo-ontem = tt-lotes.saldo-atu + tt-lotes.variacao-ontem.

    IF tt-lotes.saldo-atu = 0 AND tt-lotes.saldo-ontem = 0 THEN NEXT.

    FIND FIRST lote-prod WHERE
        lote-prod.lote      = tt-lotes.lote        AND
        lote-prod.it-codigo = tt-lotes.it-codigo
        NO-LOCK NO-ERROR.

    IF NOT AVAIL lote-prod THEN NEXT.

    FIND FIRST ord-prod WHERE
        ord-prod.nr-ord-produ = lote-prod.nr-ord-prod
        NO-LOCK NO-ERROR.

    IF NOT AVAIL ord-prod THEN NEXT.

    IF ord-prod.nr-linha = 2 THEN DO:

        ASSIGN tt-dados.volume-ln-2 [1] = tt-dados.volume-ln-2 [1] + tt-lotes.saldo-atu
               tt-dados.volume-ln-2 [2] = tt-dados.volume-ln-2 [2] + tt-lotes.saldo-ontem.

        IF tt-lotes.cod-depos = "CQ" OR
           tt-lotes.saldo-atu < 400 THEN

            ASSIGN tt-dados.reprov-ln-2 = tt-dados.reprov-ln-2 + tt-lotes.saldo-atu.
    END.

    IF ord-prod.nr-linha = 3 THEN DO:

        ASSIGN tt-dados.volume-ln-3 [1] = tt-dados.volume-ln-3 [1] + tt-lotes.saldo-atu
               tt-dados.volume-ln-3 [2] = tt-dados.volume-ln-3 [2] + tt-lotes.saldo-ontem.

        IF tt-lotes.cod-depos = "CQ" OR 
           tt-lotes.saldo-atu < 400 THEN

            ASSIGN tt-dados.reprov-ln-3 = tt-dados.reprov-ln-3 + tt-lotes.saldo-atu.
    END.

    IF ord-prod.nr-linha <> 2 AND ord-prod.nr-linha <> 3 THEN DO:

        ASSIGN tt-dados.volume-ln-1 [1] = tt-dados.volume-ln-1 [1] + tt-lotes.saldo-atu
               tt-dados.volume-ln-1 [2] = tt-dados.volume-ln-1 [2] + tt-lotes.saldo-ontem.

        IF tt-lotes.cod-depos = "CQ" OR
           tt-lotes.saldo-atu < 400 THEN

            ASSIGN tt-dados.reprov-ln-1 = tt-dados.reprov-ln-1 + tt-lotes.saldo-atu.
    END.



END. /* for each tt-lotes */

/* Fim da Rotina para buscar estoques de Mill Rolls */


/* Rotina para buscar estoques e consumos de Materias Primas/Aditivos */

FOR EACH tt-grupo-mat.
    DELETE tt-grupo-mat.
END.

FOR EACH am-cp-pcp-grupo-mat WHERE
         am-cp-pcp-grupo-mat.cod-estabel = c-cod-estabel-ini
         NO-LOCK.
    
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    CREATE tt-grupo-mat.

    ASSIGN tt-grupo-mat.cod-estabel = am-cp-pcp-grupo-mat.cod-estabel
           tt-grupo-mat.tipo-grupo  = am-cp-pcp-grupo-mat.tipo-grupo
           tt-grupo-mat.cod-grupo   = am-cp-pcp-grupo-mat.cod-grupo 
           tt-grupo-mat.descricao   = am-cp-pcp-grupo-mat.descricao 
           tt-grupo-mat.target      = am-cp-pcp-grupo-mat.target .   

    FOR EACH am-cp-pcp-it-grupo-mat WHERE
             am-cp-pcp-it-grupo-mat.cod-estabel = am-cp-pcp-grupo-mat.cod-estabel AND
             am-cp-pcp-it-grupo-mat.tipo-grupo  = am-cp-pcp-grupo-mat.tipo-grupo        AND
             am-cp-pcp-it-grupo-mat.cod-grupo   = am-cp-pcp-grupo-mat.cod-grupo
             NO-LOCK.    

        ASSIGN saldo-atu-jr  =  0.

        FOR EACH saldo-estoq WHERE
            saldo-estoq.cod-estabel  = am-cp-pcp-it-grupo-mat.cod-estabel AND
            saldo-estoq.it-codigo    = am-cp-pcp-it-grupo-mat.it-codigo   AND
            saldo-estoq.qtidade-atu <> 0
            USE-INDEX estabel-item
            NO-LOCK.

            ASSIGN saldo-atu-jr = saldo-atu-jr + saldo-estoq.qtidade-atu.

        END.

        ASSIGN entradas-jr = 0
               saidas-jr   = 0.
    

        FOR EACH movto-estoq NO-LOCK WHERE
            movto-estoq.it-codigo   = am-cp-pcp-it-grupo-mat.it-codigo AND
            movto-estoq.cod-estabel = c-cod-estabel-ini       AND
            movto-estoq.dt-trans    > tt-param.dt-data-hoje
            USE-INDEX item-data.
    
           IF movto-estoq.tipo-trans = 1 THEN
               ASSIGN entradas-jr = entradas-jr + movto-estoq.quantidade.
           ELSE
               ASSIGN saidas-jr   = saidas-jr   + movto-estoq.quantidade.

        END.

        ASSIGN saldo-atu-jr = saldo-atu-jr - entradas-jr + saidas-jr.
    
        ASSIGN tt-grupo-mat.estoque-hoje = tt-grupo-mat.estoque-hoje + saldo-atu-jr.

        /* estoque em terceiros */

        FOR EACH saldo-terc WHERE
            saldo-terc.cod-estabel = am-cp-pcp-it-grupo-mat.cod-estabel AND
            saldo-terc.it-codigo   = am-cp-pcp-it-grupo-mat.it-codigo   AND
            saldo-terc.quantidade  <> 0                                 
            USE-INDEX estab-item
            NO-LOCK.

            ASSIGN tt-grupo-mat.estoque-externo = tt-grupo-mat.estoque-externo + saldo-terc.quantidade.

        END.

        /* M‚dia de Consumo */

        IF am-cp-pcp-it-grupo-mat.tipo-grupo = 1 THEN DO:

            ASSIGN esp-jr = 5.

            DO WHILE esp-jr = 5 OR esp-jr = 7 or
                     esp-jr = 28 OR esp-jr = 30 OR esp-jr = 31 :

              assign v-num-reg-lidos = v-num-reg-lidos + 1.
              run pi-acompanhar in h-acomp(input string("M‚dia de Consumo:" + am-cp-pcp-it-grupo-mat.it-codigo)).

              for each movto-estoq no-lock
                where movto-estoq.esp-docto   = esp-jr            AND
                      movto-estoq.cod-estabel = c-cod-estabel-ini AND 
                      movto-estoq.dt-trans   >= (tt-param.dt-data-hoje - 180) and 
                      movto-estoq.dt-trans   <= tt-param.dt-data-hoje         AND
                      movto-estoq.it-codigo   = am-cp-pcp-it-grupo-mat.it-codigo  
                      USE-INDEX item-data .

                  IF movto-estoq.esp-docto = 28 OR movto-estoq.esp-docto = 30 THEN
                      ASSIGN tt-grupo-mat.consumo-medio = tt-grupo-mat.consumo-medio +
                             movto-estoq.quantidade.
                  ELSE
                      ASSIGN tt-grupo-mat.consumo-medio = tt-grupo-mat.consumo-medio -
                             movto-estoq.quantidade. 
              END.
                        
              IF esp-jr = 5 THEN ASSIGN esp-jr = 7.
              ELSE
                  IF esp-jr = 7 THEN ASSIGN esp-jr = 28.
                  ELSE
                      IF esp-jr = 28 THEN ASSIGN esp-jr = 30.
                      ELSE
                          IF esp-jr = 30 THEN ASSIGN esp-jr = 31.
                          ELSE
                              IF esp-jr = 31 THEN ASSIGN esp-jr = 99.

            END.  /* do while esp-jr */ 

        END. /* IF am-cp-pcp-it-grupo-mat.tipo-grupo = 1 */

    END. /* FOR EACH am-cp-pcp-it-grupo-mat */

    ASSIGN tt-grupo-mat.estoque-polo = tt-grupo-mat.estoque-hoje.

    IF am-cp-pcp-grupo-mat.tipo-grupo = 1 THEN 
       ASSIGN tt-grupo-mat.consumo-medio = tt-grupo-mat.consumo-medio / 6.

END. /* FOR EACH am-cp-pcp-grupo-mat */



/* Fim da Rotina para buscar estoques e consumos de Materias Primas/Aditivos */


/* Busca pedidos e estoques do mˆs e do pr¢ximo mˆs - geral e tms */

for each ped-venda no-lock
         where ped-venda.dt-entrega     >= dt-data-inicio      and 
               ped-venda.dt-entrega     <= data-fim-prx-jr     and
               ped-venda.cod-sit-ped     < 4                   and 
               ped-venda.cod-canal-venda = c-canal-venda-ini   AND
               (ped-venda.tp-pedido = "E" OR ped-venda.tp-pedido = "P")
               USE-INDEX ch-pre-fat,

    each ped-item no-lock
         where ped-item.nome-abrev    = ped-venda.nome-abrev  and
               ped-item.nr-pedcli     = ped-venda.nr-pedcli   and
               ped-item.dt-entrega   >= dt-data-inicio        and 
               ped-item.dt-entrega   <= data-fim-prx-jr       and
               ped-item.ind-componen <> 3                     AND
               ped-item.cod-sit-item  < 4 :

    IF ped-venda.tp-pedido <> "P" AND ped-venda.tp-pedido <> "E" THEN NEXT.

    FIND ITEM WHERE ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

    IF NOT AVAIL ITEM THEN NEXT.

    IF ITEM.ge-codigo <> 46 THEN NEXT.

    IF integer(substring(string(ped-item.nat-operacao),1,1)) >= 7 THEN
        ASSIGN merc-jr = "E".
         ELSE
          ASSIGN merc-jr = "I".

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input "Lendo Pedidos/Pallets:" + string(v-num-reg-lidos)).

    FOR EACH pallet WHERE 
        pallet.nr-pedido = ped-venda.nr-pedido AND
        pallet.situacao  = 2
        USE-INDEX pedido NO-LOCK.

        ASSIGN qtde-prod-jr = 0.

        FOR EACH saldo-estoq WHERE
            saldo-estoq.lote      = pallet.nr-pallet AND 
            saldo-estoq.it-codigo = pallet.it-codigo AND 
            saldo-estoq.cod-refer = pallet.cod-refer AND
            saldo-estoq.qtidade-atu > 0
            NO-LOCK USE-INDEX lote :
        
          ASSIGN qtde-prod-jr = qtde-prod-jr + saldo-estoq.qtidade-atu.

        END.

/* Procura no Terceiro */

        ASSIGN qtde-jr-t    = 0
               soma-terc-jr = 0
               qtde-jr-tr   = 0
               soma-terc-tr = 0.

        IF qtde-prod-jr = 0 THEN DO:

            FIND FIRST saldo-estoq WHERE
                       saldo-estoq.lote      = pallet.nr-pallet AND
                       saldo-estoq.it-codigo = pallet.it-codigo AND
                       saldo-estoq.cod-refer = pallet.cod-refer
                USE-INDEX lote
                NO-LOCK NO-ERROR.

            IF AVAIL saldo-estoq THEN DO:

               ASSIGN soma-terc-jr = 0
                      soma-terc-tr = 0.
        
               FOR EACH movto-estoq NO-LOCK WHERE
                   movto-estoq.it-codigo = saldo-estoq.it-codigo AND
                   movto-estoq.cod-refer = saldo-estoq.cod-refer AND
                   movto-estoq.lote      = saldo-estoq.lote AND
                   (movto-estoq.esp-docto = 22 OR 
                    movto-estoq.esp-docto = 21 OR
                    movto-estoq.esp-docto = 23).

                   FIND natur-oper NO-LOCK WHERE 
                         natur-oper.nat-operacao = movto-estoq.nat-operacao.

                   IF NOT AVAIL natur-oper OR 
                      (natur-oper.terceiro = NO AND
                       natur-oper.transf   = NO) THEN NEXT.
                   
                   IF movto-estoq.esp-docto = 23 THEN DO:

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
        
               END.  /* FOR EACH movto-estoq */

        
               ASSIGN soma-terc-jr = soma-terc-jr * -1
                      soma-terc-tr = soma-terc-tr * -1
                      qtde-jr-t  = qtde-jr-t  + soma-terc-jr
                      qtde-jr-tr = qtde-jr-tr + soma-terc-tr.

            END.

        END. /* IF qtde-prod-jr = 0 */ 

/* Fim procura no Terceiro */

        ASSIGN var-terc     = /*var-terc + */ qtde-jr-t
               var-transito = /*var-transito +*/ qtde-jr-tr.


        IF ped-item.dt-entrega < data-prx-jr THEN DO:

            IF merc-jr = "I" THEN DO:
                IF ped-venda.tp-pedido = "P" THEN
                   ASSIGN tt-dados.prod-mi-p = tt-dados.prod-mi-p  + qtde-prod-jr + var-terc + var-transito.
                ELSE 
                   ASSIGN tt-dados.prod-mi-e = tt-dados.prod-mi-e  + qtde-prod-jr + var-terc + var-transito.
            END.

            ELSE DO:
                IF ped-venda.tp-pedido = "P" THEN
                   ASSIGN tt-dados.prod-me-p  = tt-dados.prod-me-p  + qtde-prod-jr + var-terc + var-transito.
                ELSE
                   ASSIGN tt-dados.prod-me-e  = tt-dados.prod-me-e  + qtde-prod-jr + var-terc + var-transito.
            END.

            IF ITEM.fm-cod-com = "46-10-40" THEN DO:

              IF merc-jr = "I" THEN DO:
                  IF ped-venda.tp-pedido = "P" THEN
                     ASSIGN tt-dados.prod-mi-tms-p  = tt-dados.prod-mi-tms-p  + qtde-prod-jr + var-terc + var-transito.
                  ELSE
                     ASSIGN tt-dados.prod-mi-tms-e  = tt-dados.prod-mi-tms-e  + qtde-prod-jr + var-terc + var-transito.
              END.

              ELSE DO:
                  IF ped-venda.tp-pedido = "P" THEN
                     ASSIGN tt-dados.prod-me-tms-p  = tt-dados.prod-me-tms-p  + qtde-prod-jr + var-terc + var-transito.
                  ELSE
                     ASSIGN tt-dados.prod-me-tms-e  = tt-dados.prod-me-tms-e  + qtde-prod-jr + var-terc + var-transito.

              END.

            END.

        END. /* IF ped-item.dt-entrega < data-prx-jr */

        IF ped-item.dt-entrega >= data-prx-jr THEN DO:

            IF merc-jr = "I" THEN DO:
                IF ped-venda.tp-pedido = "P" THEN
                   ASSIGN tt-dados.prod-prx-mes-mi-p = tt-dados.prod-prx-mes-mi-p + qtde-prod-jr + var-terc + var-transito.
                ELSE
                   ASSIGN tt-dados.prod-prx-mes-mi-e = tt-dados.prod-prx-mes-mi-e + qtde-prod-jr + var-terc + var-transito.
            END.

            ELSE DO:
                IF ped-venda.tp-pedido = "P" THEN
                   ASSIGN tt-dados.prod-prx-mes-me-p = tt-dados.prod-prx-mes-me-p + qtde-prod-jr + var-terc + var-transito.
                ELSE 
                   ASSIGN tt-dados.prod-prx-mes-me-e = tt-dados.prod-prx-mes-me-e + qtde-prod-jr + var-terc + var-transito.
            END.

            IF ITEM.fm-cod-com = "46-10-40" THEN DO:

              IF merc-jr = "I" THEN DO:
                  IF ped-venda.tp-pedido = "P" THEN
                     ASSIGN tt-dados.prod-prx-mes-mi-tms-p = tt-dados.prod-prx-mes-mi-tms-p + qtde-prod-jr + var-terc + var-transito.
                  ELSE
                     ASSIGN tt-dados.prod-prx-mes-mi-tms-e = tt-dados.prod-prx-mes-mi-tms-e + qtde-prod-jr + var-terc + var-transito.
              END.
              ELSE DO:
                  IF ped-venda.tp-pedido = "P" THEN
                     ASSIGN tt-dados.prod-prx-mes-me-tms-p = tt-dados.prod-prx-mes-me-tms-p + qtde-prod-jr + var-terc + var-transito.
                  ELSE
                     ASSIGN tt-dados.prod-prx-mes-me-tms-e = tt-dados.prod-prx-mes-me-tms-e + qtde-prod-jr + var-terc + var-transito.

              END.

            END.

        END. /* ped-item.dt-entrega >= data-prx-jr */
    
    END. /*for each Pallet*/


    IF ped-item.dt-entrega < data-prx-jr THEN DO:

       IF merc-jr = "I" THEN DO: 
           IF ped-venda.tp-pedido = "P" THEN
              ASSIGN tt-dados.pedidos-mi-p [1] = tt-dados.pedidos-mi-p [1] + ped-item.qt-pedida
                     tt-dados.prod-mi-p        = tt-dados.prod-mi-p        + ped-item.qt-atendida.
           ELSE 
              ASSIGN tt-dados.pedidos-mi-e [1] = tt-dados.pedidos-mi-e [1] + ped-item.qt-pedida
                     tt-dados.prod-mi-e        = tt-dados.prod-mi-e        + ped-item.qt-atendida.

       END.

       ELSE DO:
           IF ped-venda.tp-pedido = "P" THEN
              ASSIGN tt-dados.pedidos-me-p [1] = tt-dados.pedidos-me-p [1] + ped-item.qt-pedida
                     tt-dados.prod-me-p        = tt-dados.prod-me-p        + ped-item.qt-atendida.
           ELSE
              ASSIGN tt-dados.pedidos-me-e [1] = tt-dados.pedidos-me-e [1] + ped-item.qt-pedida
                     tt-dados.prod-me-e        = tt-dados.prod-me-e        + ped-item.qt-atendida.
       END.


       IF ITEM.fm-cod-com = "46-10-40" THEN DO:

         IF merc-jr = "I" THEN
             IF ped-venda.tp-pedido = "P" THEN
                ASSIGN tt-dados.pedidos-mi-tms-p [1] = tt-dados.pedidos-mi-tms-p [1] + ped-item.qt-pedida
                       tt-dados.prod-mi-tms-p        = tt-dados.prod-mi-tms-p        + ped-item.qt-atendida.
             ELSE
                ASSIGN tt-dados.pedidos-mi-tms-e [1] = tt-dados.pedidos-mi-tms-e [1] + ped-item.qt-pedida
                       tt-dados.prod-mi-tms-e        = tt-dados.prod-mi-tms-e        + ped-item.qt-atendida.

          ELSE DO:
              IF ped-venda.tp-pedido = "P" THEN
                 ASSIGN tt-dados.pedidos-me-tms-p [1] = tt-dados.pedidos-me-tms-p [1] + ped-item.qt-pedida
                        tt-dados.prod-me-tms-p        = tt-dados.prod-me-tms-p        + ped-item.qt-atendida.
              ELSE
                 ASSIGN tt-dados.pedidos-me-tms-e [1] = tt-dados.pedidos-me-tms-e [1] + ped-item.qt-pedida
                        tt-dados.prod-me-tms-e        = tt-dados.prod-me-tms-p        + ped-item.qt-atendida.

          END.

       END.

    END. /* ped-item.dt-entrega < data-prx-jr */



    IF ped-item.dt-entrega >= data-prx-jr THEN DO:

       IF merc-jr = "I" THEN DO: 
           IF ped-venda.tp-pedido = "P" THEN
              ASSIGN tt-dados.ped-prx-mes-mi-p  = tt-dados.ped-prx-mes-mi-p  + ped-item.qt-pedida
                     tt-dados.prod-prx-mes-mi-p = tt-dados.prod-prx-mes-mi-p + ped-item.qt-atendida.
           ELSE
              ASSIGN tt-dados.ped-prx-mes-mi-e  = tt-dados.ped-prx-mes-mi-e  + ped-item.qt-pedida
                     tt-dados.prod-prx-mes-mi-e = tt-dados.prod-prx-mes-mi-e + ped-item.qt-atendida.

       END.

       ELSE DO:
           IF ped-venda.tp-pedido = "P" THEN
              ASSIGN tt-dados.ped-prx-mes-me-p  = tt-dados.ped-prx-mes-me-p  + ped-item.qt-pedida
                     tt-dados.prod-prx-mes-me-p = tt-dados.prod-prx-mes-me-p + ped-item.qt-atendida.
           ELSE 
              ASSIGN tt-dados.ped-prx-mes-me-e  = tt-dados.ped-prx-mes-me-e  + ped-item.qt-pedida
                     tt-dados.prod-prx-mes-me-e = tt-dados.prod-prx-mes-me-e + ped-item.qt-atendida.
       END.

       IF ITEM.fm-cod-com = "46-10-40" THEN DO:

         IF merc-jr = "I" THEN DO:
             IF ped-venda.tp-pedido = "P" THEN
                ASSIGN tt-dados.ped-prx-mes-mi-tms-p  = tt-dados.ped-prx-mes-mi-tms-p  + ped-item.qt-pedida
                       tt-dados.prod-prx-mes-mi-tms-p = tt-dados.prod-prx-mes-mi-tms-p + ped-item.qt-atendida.
             ELSE
                ASSIGN tt-dados.ped-prx-mes-mi-tms-e  = tt-dados.ped-prx-mes-mi-tms-e  + ped-item.qt-pedida
                       tt-dados.prod-prx-mes-mi-tms-e = tt-dados.prod-prx-mes-mi-tms-e + ped-item.qt-atendida.
         END.

          ELSE DO:
              IF ped-venda.tp-pedido = "P" THEN
                 ASSIGN tt-dados.ped-prx-mes-me-tms-p  = tt-dados.ped-prx-mes-me-tms-p  + ped-item.qt-pedida
                        tt-dados.prod-prx-mes-me-tms-p = tt-dados.prod-prx-mes-me-tms-p + ped-item.qt-atendida.
              ELSE
                 ASSIGN tt-dados.ped-prx-mes-me-tms-e  = tt-dados.ped-prx-mes-me-tms-e  + ped-item.qt-pedida
                        tt-dados.prod-prx-mes-me-tms-e = tt-dados.prod-prx-mes-me-tms-e + ped-item.qt-atendida. 
          END.

       END.

    END. /* ped-item.dt-entrega >= data-prx-jr */

END. /* for each ped-venda */


/* Rotina para compor os estoques de Primes e Off-Specs */

assign v-num-reg-lidos = 0.

FOR EACH ITEM NO-LOCK WHERE
    ITEM.GE-codigo >= 41 AND
    ITEM.GE-codigo <= 49 
    USE-INDEX grupo.
        
    IF INDEX(ITEM.it-codigo,"MR",1) <> 0 THEN NEXT.

    IF substring(item.fm-codigo,1,2) <> "46" and
       substring(item.fm-codigo,1,2) <> "47" THEN NEXT.

    FOR EACH saldo-estoq WHERE
        saldo-estoq.it-codigo   = item.it-codigo    AND
        saldo-estoq.lote <> "recicl"                AND
        saldo-estoq.cod-depos <> "ARC"              AND
        saldo-estoq.qtidade-atu <> 0
        USE-INDEX item-lote
        NO-LOCK. 

        assign v-num-reg-lidos = v-num-reg-lidos + 1.
      
        run pi-acompanhar in h-acomp(input "Lendo Pallets para Estoques:" + 
                                     string(item.it-codigo) + " " + 
                                     string(v-num-reg-lidos)).

        ASSIGN saldo-atu-jr = saldo-estoq.qtidade-atu. 

        ASSIGN entradas-jr       = 0
               saidas-jr         = 0
               entradas-ontem-jr = 0
               saidas-ontem-jr   = 0.

        FOR EACH movto-estoq NO-LOCK WHERE
            movto-estoq.it-codigo   = saldo-estoq.it-codigo   AND
            movto-estoq.cod-estabel = saldo-estoq.cod-estabel AND
            movto-estoq.cod-depos   = saldo-estoq.cod-depos   AND
            movto-estoq.lote        = saldo-estoq.lote        AND
            movto-estoq.cod-localiz = saldo-estoq.cod-localiz AND
            movto-estoq.cod-refer   = saldo-estoq.cod-refer   AND
            movto-estoq.dt-trans    > tt-param.dt-data-ontem   
            USE-INDEX item-estab.
    
           IF movto-estoq.dt-trans > tt-param.dt-data-hoje THEN DO:
               IF movto-estoq.tipo-trans = 1 THEN
                   ASSIGN entradas-jr = entradas-jr + movto-estoq.quantidade.
               ELSE
                   ASSIGN saidas-jr   = saidas-jr   + movto-estoq.quantidade.
           END.

           IF movto-estoq.dt-trans >  tt-param.dt-data-ontem and
              movto-estoq.dt-trans <= tt-param.dt-data-hoje THEN DO:
               IF movto-estoq.tipo-trans = 1 THEN
                   ASSIGN entradas-ontem-jr = entradas-ontem-jr + movto-estoq.quantidade.
               ELSE
                   ASSIGN saidas-ontem-jr   = saidas-ontem-jr   + movto-estoq.quantidade.
           END.

        END.

        ASSIGN saldo-atu-jr    = saldo-atu-jr - entradas-jr + saidas-jr
               saldo-atu-ontem = saldo-atu-jr - entradas-ontem-jr + saidas-ontem-jr.

        FIND FIRST pallet WHERE 
            pallet.it-codigo = saldo-estoq.it-codigo AND
            pallet.nr-pallet = saldo-estoq.lote AND
            pallet.situacao  = 2
            USE-INDEX item NO-LOCK NO-ERROR.
        
        IF NOT AVAIL pallet THEN NEXT.

        IF pallet.nr-pedido <> 0 THEN ASSIGN tem-pedido-jr = YES.
            else
                ASSIGN tem-pedido-jr = NO.

        IF tem-pedido-jr = YES THEN DO: /* tem pedido */

           FIND FIRST ped-venda WHERE
               ped-venda.nr-pedido       = pallet.nr-pedido AND
               ped-venda.cod-canal-venda = c-canal-venda-ini
               NO-LOCK NO-ERROR.

           IF NOT AVAIL ped-venda THEN ASSIGN tem-pedido-jr = NO.

        END.

        IF tem-pedido-jr = YES THEN DO: /* Tem Pedido */

          IF ITEM.ge-codigo = 46 THEN DO:  /* Prime */
        
             IF ped-venda.tp-pedido = "E" THEN
        
                 ASSIGN tt-dados.est-prime-cliente [1] = tt-dados.est-prime-cliente [1]
                        + saldo-atu-jr
                        tt-dados.est-prime-cliente [2] = tt-dados.est-prime-cliente [2]
                                     + saldo-atu-ontem.


        
             IF ped-venda.tp-pedido <> "E" THEN DO:
        
                IF saldo-estoq.cod-estabel = "422" THEN
                    ASSIGN tt-dados.est-prime-pedido-422 [1] = tt-dados.est-prime-pedido-422 [1] 
                           + saldo-atu-jr
                           tt-dados.est-prime-pedido-422 [2] = tt-dados.est-prime-pedido-422 [2] 
                           + saldo-atu-ontem.
            
                ELSE
                 IF saldo-estoq.cod-estabel = "421" THEN
                     ASSIGN tt-dados.est-prime-pedido-421 [1] = tt-dados.est-prime-pedido-421 [1] 
                            + saldo-atu-jr
                            tt-dados.est-prime-pedido-421 [2] = tt-dados.est-prime-pedido-421 [2] 
                            + saldo-atu-ontem.

            
                 ELSE
                  IF saldo-estoq.cod-estabel = "425" OR saldo-estoq.cod-estabel = "426" THEN
                      ASSIGN tt-dados.est-prime-pedido-426 [1] = tt-dados.est-prime-pedido-426 [1] 
                             + saldo-atu-jr
                             tt-dados.est-prime-pedido-426 [2] = tt-dados.est-prime-pedido-426 [2] 
                             + saldo-atu-ontem.

            
                  ELSE
                      ASSIGN tt-dados.est-prime-pedido-out [1] = tt-dados.est-prime-pedido-out [1] 
                             + saldo-atu-jr
                             tt-dados.est-prime-pedido-out [2] = tt-dados.est-prime-pedido-out [2] 
                             + saldo-atu-ontem.                             
        
        
             END. /* ped-venda.tp-pedido <> "E" */
        
          END. /* Prime */
        
          IF ITEM.ge-codigo <> 46 THEN DO:  /* Off-Specs */
        
             IF ped-venda.tp-pedido = "E" THEN
        
                 ASSIGN tt-dados.est-offsp-cliente [1] = tt-dados.est-offsp-cliente [1]
                        + saldo-atu-jr
                        tt-dados.est-offsp-cliente [2] = tt-dados.est-offsp-cliente [2]
                        + saldo-atu-ontem.

        
             IF ped-venda.tp-pedido <> "E" THEN DO:
        
                IF saldo-estoq.cod-estabel = "422" THEN
                    ASSIGN tt-dados.est-offsp-pedido-422 [1] = tt-dados.est-offsp-pedido-422 [1] 
                           + saldo-atu-jr
                           tt-dados.est-offsp-pedido-422 [2] = tt-dados.est-offsp-pedido-422 [2] 
                           + saldo-atu-ontem.

            
                ELSE
                 IF saldo-estoq.cod-estabel = "421" THEN
                     ASSIGN tt-dados.est-offsp-pedido-421 [1] = tt-dados.est-offsp-pedido-421 [1] 
                            + saldo-atu-jr
                            tt-dados.est-offsp-pedido-421 [2] = tt-dados.est-offsp-pedido-421 [2] 
                            + saldo-atu-ontem.

            
                 ELSE
                  IF saldo-estoq.cod-estabel = "425" OR saldo-estoq.cod-estabel = "426" THEN
                      ASSIGN tt-dados.est-offsp-pedido-426 [1] = tt-dados.est-offsp-pedido-426 [1] 
                             + saldo-atu-jr
                             tt-dados.est-offsp-pedido-426 [2] = tt-dados.est-offsp-pedido-426 [2] 
                             + saldo-atu-ontem.

            
                  ELSE
                      ASSIGN tt-dados.est-offsp-pedido-out [1] = tt-dados.est-offsp-pedido-out [1] 
                             + saldo-atu-jr
                             tt-dados.est-offsp-pedido-out [2] = tt-dados.est-offsp-pedido-out [2] 
                             + saldo-atu-ontem.

        
             END. /* IF ped-venda.tp-pedido <> "E" */    
        
          END. /* offsp */

        END.   /* tem pedido = yes */


        IF tem-pedido-jr = NO THEN DO:    /* NÆo tem Pedido */

          IF ITEM.ge-codigo = 46 AND INDEX(ITEM.it-codigo,"EP",1) <> 0 THEN DO:  /* Prime-EP */ 

              ASSIGN tt-dados.est-prime-ep [1] = tt-dados.est-prime-ep [1] 
                     + saldo-atu-jr
                     tt-dados.est-prime-ep [2] = tt-dados.est-prime-ep [2] 
                     + saldo-atu-ontem.

          END.

          IF ITEM.ge-codigo = 46 AND INDEX(ITEM.it-codigo,"EP",1) = 0 THEN DO:  /* Prime */ 
        
             IF saldo-estoq.cod-estabel = "422" THEN
                 ASSIGN tt-dados.est-prime-sem-pd-422 [1] = tt-dados.est-prime-sem-pd-422 [1] 
                        + saldo-atu-jr
                        tt-dados.est-prime-sem-pd-422 [2] = tt-dados.est-prime-sem-pd-422 [2] 
                        + saldo-atu-ontem.
                 
            
             ELSE
              IF saldo-estoq.cod-estabel = "421" THEN
                  ASSIGN tt-dados.est-prime-sem-pd-421 [1] = tt-dados.est-prime-sem-pd-421 [1] 
                         + saldo-atu-jr
                         tt-dados.est-prime-sem-pd-421 [2] = tt-dados.est-prime-sem-pd-421 [2] 
                         + saldo-atu-ontem.

            
              ELSE
               IF saldo-estoq.cod-estabel = "425" OR saldo-estoq.cod-estabel = "426" THEN
                   ASSIGN tt-dados.est-prime-sem-pd-426 [1] = tt-dados.est-prime-sem-pd-426 [1] 
                          + saldo-atu-jr
                          tt-dados.est-prime-sem-pd-426 [2] = tt-dados.est-prime-sem-pd-426 [2] 
                          + saldo-atu-ontem.

            
               ELSE
                   ASSIGN tt-dados.est-prime-sem-pd-out [1] = tt-dados.est-prime-sem-pd-out [1] 
                          + saldo-atu-jr
                          tt-dados.est-prime-sem-pd-out [2] = tt-dados.est-prime-sem-pd-out [2] 
                          + saldo-atu-ontem.

        
          END. /* Prime */

          IF ITEM.ge-codigo <> 46 AND INDEX(ITEM.it-codigo,"EP",1) <> 0 THEN DO:  /* Off-Specs-EP */ 
          
              ASSIGN tt-dados.est-offsp-ep [1] = tt-dados.est-offsp-ep [1] 
                     + saldo-atu-jr
                     tt-dados.est-offsp-ep [2] = tt-dados.est-offsp-ep [2] 
                     + saldo-atu-ontem.
          
          END.
        
          IF ITEM.ge-codigo <> 46 AND INDEX(ITEM.it-codigo,"EP",1) = 0 THEN DO:  /* Off-Specs */
        
            IF saldo-estoq.cod-estabel = "422" THEN
                ASSIGN tt-dados.est-offsp-sem-pd-422 [1] = tt-dados.est-offsp-sem-pd-422 [1] 
                       + saldo-atu-jr
                       tt-dados.est-offsp-sem-pd-422 [2] = tt-dados.est-offsp-sem-pd-422 [2] 
                       + saldo-atu-ontem.

            
            ELSE
             IF saldo-estoq.cod-estabel = "421" THEN
                 ASSIGN tt-dados.est-offsp-sem-pd-421 [1] = tt-dados.est-offsp-sem-pd-421 [1] 
                        + saldo-atu-jr
                        tt-dados.est-offsp-sem-pd-421 [2] = tt-dados.est-offsp-sem-pd-421 [2] 
                        + saldo-atu-ontem.

            
             ELSE
              IF saldo-estoq.cod-estabel = "425" OR saldo-estoq.cod-estabel = "426" THEN
                  ASSIGN tt-dados.est-offsp-sem-pd-426 [1] = tt-dados.est-offsp-sem-pd-426 [1] 
                         + saldo-atu-jr
                         tt-dados.est-offsp-sem-pd-426 [2] = tt-dados.est-offsp-sem-pd-426 [2] 
                         + saldo-atu-ontem.

            
              ELSE
                  ASSIGN tt-dados.est-offsp-sem-pd-out [1] = tt-dados.est-offsp-sem-pd-out [1] 
                         + saldo-atu-jr
                         tt-dados.est-offsp-sem-pd-out [2] = tt-dados.est-offsp-sem-pd-out [2] 
                         + saldo-atu-ontem.

        
          END. /* offsp */


        END.   /* IF tem-pedido-jr = NO  */ 



    END. /* FOR EACH saldo-estoq */ 

    assign v-num-reg-lidos = 0. 

END. /* FOR EACH ITEM */


/* Fim da Rotina para compor os estoques de primes e off-specs */



/* Rotina para buscar estoques armazenados em terceiros. */

FOR EACH tt-lotes-terc.
    DELETE tt-lotes-terc.
END.

FOR EACH tt-notas-terc.
    DELETE tt-notas-terc.
END.

assign v-num-reg-lidos = 0. 

FOR EACH ITEM NO-LOCK WHERE
    ITEM.GE-codigo >= 41 AND
    ITEM.GE-codigo <= 49 
    USE-INDEX grupo.
        
    IF INDEX(ITEM.it-codigo,"MR",1) <> 0 THEN NEXT.

    IF substring(item.fm-codigo,1,2) <> "46" and
       substring(item.fm-codigo,1,2) <> "47" THEN NEXT. 

    for each saldo-terc no-lock
          where saldo-terc.it-codigo  = item.it-codigo  and 
                saldo-terc.quantidade > 0
                USE-INDEX ITEM-emit .

        FIND FIRST it-nota-fisc WHERE
            it-nota-fisc.cod-estabel = saldo-terc.cod-estabel AND
            it-nota-fisc.serie       = saldo-terc.serie-docto AND
            it-nota-fisc.nr-nota-fis = saldo-terc.nro-docto   AND
            it-nota-fisc.nr-seq-fat  = saldo-terc.sequencia
            NO-LOCK NO-ERROR.

        IF NOT AVAIL it-nota-fisc THEN NEXT.
    
        FIND FIRST tt-notas-terc WHERE
            tt-notas-terc.nro-docto = saldo-terc.nro-docto AND
            tt-notas-terc.sequencia = saldo-terc.sequencia
            NO-LOCK NO-ERROR.
    
        IF AVAIL tt-notas-terc THEN NEXT.
    
        CREATE tt-notas-terc.
        ASSIGN tt-notas-terc.nro-docto    = saldo-terc.nro-docto 
               tt-notas-terc.sequencia    = saldo-terc.sequencia 
               tt-notas-terc.cod-emitente = saldo-terc.cod-emitente 
               tt-notas-terc.serie        = saldo-terc.serie 
               tt-notas-terc.cod-estabel  = saldo-terc.cod-estabel 
               tt-notas-terc.saldo        = saldo-terc.quantidade
               tt-notas-terc.it-codigo    = saldo-terc.it-codigo.
    
        /* rotina para encontrar os lotes enviados */

        assign v-num-reg-lidos = v-num-reg-lidos + 1. 
      
        run pi-acompanhar in h-acomp(input "Lendo Estoques em Terc.-1: " + 
                                     string(item.it-codigo) + " " + 
                                     string(v-num-reg-lidos)).

        FOR EACH fat-ser-lote NO-LOCK WHERE
            fat-ser-lote.cod-estabel = saldo-terc.cod-estabel AND
            fat-ser-lote.serie       = saldo-terc.serie-docto AND
            fat-ser-lote.nr-nota-fis = saldo-terc.nro-docto   AND
            fat-ser-lote.nr-seq-fat  = saldo-terc.sequencia.
            
            FIND FIRST tt-lotes-terc WHERE
                tt-lotes-terc.nro-docto = saldo-terc.nro-docto AND
                tt-lotes-terc.sequencia = saldo-terc.sequencia AND
                tt-lotes-terc.lote      = fat-ser-lote.nr-serlote
                NO-LOCK NO-ERROR.
          
            IF NOT AVAIL tt-lotes-terc THEN 
               CREATE tt-lotes-terc.
    
            ASSIGN tt-lotes-terc.nro-docto    = saldo-terc.nro-docto 
                   tt-lotes-terc.sequencia    = saldo-terc.sequencia 
                   tt-lotes-terc.lote         = fat-ser-lote.nr-serlote
                   tt-lotes-terc.cod-estabel  = saldo-terc.cod-estabel
                   tt-lotes-terc.cod-emitente = saldo-terc.cod-emitente
                   tt-lotes-terc.it-codigo    = fat-ser-lote.it-codigo.
          
            IF it-nota-fisc.dt-emis-nota <= dt-data-hoje THEN
               ASSIGN tt-lotes-terc.remessa   = tt-lotes-terc.remessa + fat-ser-lote.qt-baixada [1]
                      tt-lotes-terc.saldo     = tt-lotes-terc.remessa - tt-lotes-terc.retorno.          
    
          
            IF it-nota-fisc.dt-emis-nota <= dt-data-ontem THEN
               ASSIGN tt-lotes-terc.remessa-ontem = tt-lotes-terc.remessa-ontem + fat-ser-lote.qt-baixada [1]
                      tt-lotes-terc.saldo-ontem   = tt-lotes-terc.remessa-ontem - tt-lotes-terc.retorno-ontem.          
    
        END.
    
    END.

END.





/* Rotina para Obter os Retornos de Lotes em terceiros */

assign v-num-reg-lidos = 0.

FOR EACH tt-notas-terc NO-LOCK.

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
      
    run pi-acompanhar in h-acomp(input "Lendo Estoques em Terc.-2: " + 
                                 string(tt-notas-terc.it-codigo) + " " + 
                                 string(v-num-reg-lidos)).

    FOR EACH componente NO-LOCK WHERE
        componente.cod-emitente = tt-notas-terc.cod-emitente AND
        componente.serie-comp   = tt-notas-terc.serie        AND
        componente.nro-comp     = tt-notas-terc.nro-docto    AND
        componente.seq-comp     = tt-notas-terc.sequencia .

        FOR EACH rat-lote NO-LOCK WHERE
            rat-lote.serie-docto  = componente.serie-docto AND
            rat-lote.nro-docto    = componente.nro-docto   AND
            rat-lote.cod-emitente = tt-notas-terc.cod-emitente  AND
            rat-lote.sequencia    = componente.sequencia   AND
            rat-lote.nat-oper     = componente.nat-oper.

            FIND FIRST tt-lotes-terc WHERE
                tt-lotes-terc.nro-docto = tt-notas-terc.nro-docto AND
                tt-lotes-terc.sequencia = tt-notas-terc.sequencia AND
                tt-lotes-terc.lote      = rat-lote.lote
                NO-LOCK NO-ERROR.

            IF NOT AVAIL tt-lotes-terc THEN DO:
                CREATE tt-lotes-terc.
                ASSIGN tt-lotes-terc.nro-docto = tt-notas-terc.nro-docto 
                       tt-lotes-terc.sequencia = tt-notas-terc.sequencia 
                       tt-lotes-terc.lote      = rat-lote.lote.
            END.
            
            ASSIGN tt-lotes-terc.cod-estabel  = tt-notas-terc.cod-estabel
                   tt-lotes-terc.cod-emitente = tt-notas-terc.cod-emitente
                   tt-lotes-terc.it-codigo    = rat-lote.it-codigo.

            IF componente.dt-retorno <= dt-data-hoje THEN
               ASSIGN tt-lotes-terc.retorno   = tt-lotes-terc.retorno + rat-lote.quantidade
                      tt-lotes-terc.saldo     = tt-lotes-terc.remessa - tt-lotes-terc.retorno.          

            IF componente.dt-retorno <= dt-data-ontem THEN
               ASSIGN tt-lotes-terc.retorno-ontem = tt-lotes-terc.retorno-ontem + rat-lote.quantidade
                      tt-lotes-terc.saldo-ontem   = tt-lotes-terc.remessa-ontem - tt-lotes-terc.retorno-ontem.          

        END.
    END.
END.

assign v-num-reg-lidos = 0.
   
FOR EACH tt-notas-terc NO-LOCK.

    FIND FIRST ITEM WHERE
        ITEM.it-codigo = tt-notas-terc.it-codigo
        NO-LOCK NO-ERROR.

    IF NOT AVAIL ITEM THEN NEXT.

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
      
    run pi-acompanhar in h-acomp(input "Lendo Estoques em Terc.-3: " + 
                                 string(item.it-codigo) + " " + 
                                 string(v-num-reg-lidos)).

    FOR EACH tt-lotes-terc NO-LOCK WHERE
        tt-lotes-terc.nro-docto  = tt-notas-terc.nro-docto AND 
        tt-lotes-terc.sequencia  = tt-notas-terc.sequencia. 
    
        IF tt-lotes-terc.saldo = 0 THEN NEXT.

        FIND FIRST pallet WHERE
            pallet.it-codigo = tt-lotes-terc.it-codigo AND
            pallet.nr-pallet = tt-lotes-terc.lote
            NO-LOCK NO-ERROR.

        IF AVAIL pallet AND pallet.nr-pedido <> 0 THEN
            ASSIGN tem-pedido-jr = YES.
        ELSE
            ASSIGN tem-pedido-jr = NO.

        IF ITEM.ge-codigo < 47 THEN  DO:   /* Prime */

           IF tem-pedido-jr = YES THEN DO: /* tem pedido */
           
              FIND FIRST ped-venda WHERE
                  ped-venda.nr-pedido       = pallet.nr-pedido AND
                  ped-venda.cod-canal-venda = c-canal-venda-ini
                  NO-LOCK NO-ERROR.
           
              IF NOT AVAIL ped-venda THEN ASSIGN tem-pedido-jr = NO.
           
           END.

           IF tem-pedido-jr = YES THEN DO:

              IF ped-venda.tp-pedido = "E" THEN DO:
        
                ASSIGN tt-dados.est-prime-cliente [1] = tt-dados.est-prime-cliente [1]
                       + tt-lotes-terc.saldo
                       tt-dados.est-prime-cliente [2] = tt-dados.est-prime-cliente [2]
                                    + tt-lotes-terc.saldo-ontem.

              END.

              IF ped-venda.tp-pedido <> "E" THEN DO:

                 IF tt-lotes-terc.cod-emitente = 100435 THEN
                    ASSIGN est-prime-pedido-rds [1] = est-prime-pedido-rds [1] + 
                           tt-lotes-terc.saldo
                           est-prime-pedido-rds [2] = est-prime-pedido-rds [2] + 
                           tt-lotes-terc.saldo-ontem.
                 ELSE
                     IF tt-lotes-terc.cod-emitente = 103656 THEN
                        ASSIGN est-prime-pedido-tsg [1] = est-prime-pedido-tsg [1] + 
                               tt-lotes-terc.saldo
                               est-prime-pedido-tsg [2] = est-prime-pedido-tsg [2] + 
                               tt-lotes-terc.saldo-ontem.
                     ELSE
                         IF tt-lotes-terc.cod-emitente = 9399 THEN
                            ASSIGN est-prime-pedido-gre [1] = est-prime-pedido-gre [1] + 
                                   tt-lotes-terc.saldo
                                   est-prime-pedido-gre [2] = est-prime-pedido-gre [2] + 
                                   tt-lotes-terc.saldo-ontem.
                 ELSE
                    ASSIGN est-prime-pedido-out [1] = est-prime-pedido-out [1] + 
                           tt-lotes-terc.saldo
                           est-prime-pedido-out [2] = est-prime-pedido-out [2] + 
                           tt-lotes-terc.saldo-ontem.

              END. /*ped-venda.tp-pedido <> "E" */


           END.  /* tem-pedido = yes */

           IF tem-pedido-jr = NO THEN DO:


              IF INDEX(ITEM.it-codigo,"EP",1) <> 0 THEN DO:  /* Prime-EP */ 
              
                  ASSIGN tt-dados.est-prime-ep [1] = tt-dados.est-prime-ep [1] 
                         + tt-lotes-terc.saldo
                         tt-dados.est-prime-ep [2] = tt-dados.est-prime-ep [2] 
                         + tt-lotes-terc.saldo-ontem.
              
              END.
              
              IF INDEX(ITEM.it-codigo,"EP",1) = 0 THEN DO:  /* Prime */ 
              

                 IF tt-lotes-terc.cod-emitente = 100435 THEN
                    ASSIGN est-prime-sem-pd-rds [1] = est-prime-sem-pd-rds [1] + 
                           tt-lotes-terc.saldo
                           est-prime-sem-pd-rds [2] = est-prime-sem-pd-rds [2] + 
                           tt-lotes-terc.saldo-ontem.
                 ELSE
                     IF tt-lotes-terc.cod-emitente = 103656 THEN
                        ASSIGN est-prime-sem-pd-tsg [1] = est-prime-sem-pd-tsg [1] + 
                               tt-lotes-terc.saldo
                               est-prime-sem-pd-tsg [2] = est-prime-sem-pd-tsg [2] + 
                               tt-lotes-terc.saldo-ontem.
                     ELSE
                         IF tt-lotes-terc.cod-emitente = 9399 THEN
                            ASSIGN est-prime-sem-pd-gre [1] = est-prime-sem-pd-gre [1] + 
                                   tt-lotes-terc.saldo
                                   est-prime-sem-pd-gre [2] = est-prime-sem-pd-gre [2] + 
                                   tt-lotes-terc.saldo-ontem.
                 ELSE
                    ASSIGN est-prime-sem-pd-out [1] = est-prime-sem-pd-out [1] + 
                           tt-lotes-terc.saldo
                           est-prime-sem-pd-out [2] = est-prime-sem-pd-out [2] + 
                           tt-lotes-terc.saldo-ontem.

              END. /* prime <> ep */

           END.  /* tem-pedido = no */

        END.   /* Prime */

        IF ITEM.ge-codigo > 46 THEN  DO:   /* Off-Specs */

           IF tem-pedido-jr = YES THEN DO: /* tem pedido */
           
              FIND FIRST ped-venda WHERE
                  ped-venda.nr-pedido       = pallet.nr-pedido AND
                  ped-venda.cod-canal-venda = c-canal-venda-ini
                  NO-LOCK NO-ERROR.
           
              IF NOT AVAIL ped-venda THEN ASSIGN tem-pedido-jr = NO.
           
           END.

           IF tem-pedido-jr = YES THEN DO:

              IF ped-venda.tp-pedido = "E" THEN DO:
        
                ASSIGN tt-dados.est-offsp-cliente [1] = tt-dados.est-offsp-cliente [1]
                       + tt-lotes-terc.saldo
                       tt-dados.est-offsp-cliente [2] = tt-dados.est-offsp-cliente [2]
                                    + tt-lotes-terc.saldo-ontem.

              END.

              IF ped-venda.tp-pedido <> "E" THEN DO:

                  IF tt-lotes-terc.cod-emitente = 100435 THEN
                     ASSIGN est-offsp-pedido-rds [1] = est-offsp-pedido-rds [1] + 
                            tt-lotes-terc.saldo
                            est-offsp-pedido-rds [2] = est-offsp-pedido-rds [2] + 
                            tt-lotes-terc.saldo-ontem.
                  ELSE
                      IF tt-lotes-terc.cod-emitente = 103656 THEN
                         ASSIGN est-offsp-pedido-tsg [1] = est-offsp-pedido-tsg [1] + 
                                tt-lotes-terc.saldo
                                est-offsp-pedido-tsg [2] = est-offsp-pedido-tsg [2] + 
                                tt-lotes-terc.saldo-ontem.
                      ELSE
                          IF tt-lotes-terc.cod-emitente = 9399 THEN
                             ASSIGN est-offsp-pedido-gre [1] = est-offsp-pedido-gre [1] + 
                                    tt-lotes-terc.saldo
                                    est-offsp-pedido-gre [2] = est-offsp-pedido-gre [2] + 
                                    tt-lotes-terc.saldo-ontem.
                  ELSE
                     ASSIGN est-offsp-pedido-out [1] = est-offsp-pedido-out [1] + 
                            tt-lotes-terc.saldo
                            est-offsp-pedido-out [2] = est-offsp-pedido-out [2] + 
                        tt-lotes-terc.saldo-ontem.

              END.  /*ped-venda.tp-pedido <> "E" */


           END.  /* tem-pedido = yes */

           IF tem-pedido-jr = NO THEN DO:

              IF INDEX(ITEM.it-codigo,"EP",1) <> 0 THEN DO:  /* offspc-EP */ 
              
                  ASSIGN tt-dados.est-offsp-ep [1] = tt-dados.est-offsp-ep [1] 
                         + tt-lotes-terc.saldo
                         tt-dados.est-offsp-ep [2] = tt-dados.est-offsp-ep [2] 
                         + tt-lotes-terc.saldo-ontem.
              
              END.
              
              IF INDEX(ITEM.it-codigo,"EP",1) = 0 THEN DO:  /* offsp */ 

                 IF tt-lotes-terc.cod-emitente = 100435 THEN
                    ASSIGN est-offsp-sem-pd-rds [1] = est-offsp-sem-pd-rds [1] + 
                           tt-lotes-terc.saldo
                           est-offsp-sem-pd-rds [2] = est-offsp-sem-pd-rds [2] + 
                           tt-lotes-terc.saldo-ontem.
                 ELSE
                     IF tt-lotes-terc.cod-emitente = 103656 THEN
                        ASSIGN est-offsp-sem-pd-tsg [1] = est-offsp-sem-pd-tsg [1] + 
                               tt-lotes-terc.saldo
                               est-offsp-sem-pd-tsg [2] = est-offsp-sem-pd-tsg [2] + 
                               tt-lotes-terc.saldo-ontem.
                     ELSE
                         IF tt-lotes-terc.cod-emitente = 9399 THEN
                            ASSIGN est-offsp-sem-pd-gre [1] = est-offsp-sem-pd-gre [1] + 
                                   tt-lotes-terc.saldo
                                   est-offsp-sem-pd-gre [2] = est-offsp-sem-pd-gre [2] + 
                                   tt-lotes-terc.saldo-ontem.
                 ELSE
                    ASSIGN est-offsp-sem-pd-out [1] = est-offsp-sem-pd-out [1] + 
                           tt-lotes-terc.saldo
                           est-offsp-sem-pd-out [2] = est-offsp-sem-pd-out [2] + 
                           tt-lotes-terc.saldo-ontem.

              END. /* offsp <> EP */

           END.  /* tem-pedido = no */

        END.   /* Off Specs */


    END. /* for each tt-lotes-terc */

END. /* for each tt-notas-terc */   


/* Fim da Rotina para Obter os Retornos de Lotes em terceiros */



/* Rotina para Obter o Faturamento */

for each nota-fiscal no-lock
         where nota-fiscal.dt-emis-nota >= dt-data-inicio and 
               nota-fiscal.dt-emis-nota <= dt-data-hoje   AND
               nota-fiscal.cod-canal-venda >= c-canal-venda-ini AND   
               nota-fiscal.dt-cancela = ?
               USE-INDEX ch-sit-nota,
    each it-nota-fisc OF nota-fiscal no-lock
         where it-nota-fisc.dt-cancela = ? :

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
    
    FIND FIRST ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo
         NO-LOCK NO-ERROR.

    IF NOT AVAIL ITEM THEN NEXT.

    ASSIGN tp-pedido-jr = "ZZ".
    
    FIND FIRST ped-venda 
        where ped-venda.nr-pedido = it-nota-fisc.nr-pedido
        NO-LOCK NO-ERROR.
    
    IF  AVAIL ped-venda THEN  
        ASSIGN tp-pedido-jr = ped-venda.tp-pedido.

    FIND FIRST natur-oper WHERE natur-oper.nat-operacao = it-nota-fisc.nat-operacao
              NO-LOCK NO-ERROR.

    IF NOT AVAIL natur-oper THEN NEXT.

    IF natur-oper.ind-contabilizacao = FALSE THEN next.

    IF tp-pedido-jr <> "A" AND tp-pedido-jr <> "D" AND
       natur-oper.emite-duplic = FALSE THEN NEXT. 

    IF integer(substring(string(it-nota-fisc.nat-operacao),1,1,"character")) >= 7 THEN 
       ASSIGN var-tp-merc = "Externo"
              var-vl-liq = it-nota-fisc.vl-merc-liq.

    ELSE 
        ASSIGN var-pis-cofins = (it-nota-fisc.vl-merc-liq * txt-pis-cofins) / 100
               var-vl-liq = it-nota-fisc.vl-merc-liq - var-pis-cofins - it-nota-fisc.vl-icms-it
               var-tp-merc = "Interno".
        
    IF ITEM.GE-codigo > 79 OR item.ge-codigo < 41 THEN
       ASSIGN tp-pedido-jr = "ZZ".

    ASSIGN tp-produto-jr = "".

    ASSIGN qt-faturada-jr = (if(it-nota-fisc.baixa-estoq = true) then it-nota-fisc.qt-faturada[1] else 0).

    IF tp-pedido-jr = "ZZ" THEN
       ASSIGN tp-produto-jr = "outros".

    IF tp-pedido-jr = "A" THEN
        ASSIGN tp-produto-jr = "amostra".

    IF tp-pedido-jr = "D" THEN
        ASSIGN tp-produto-jr = "Doacao".

    IF tp-produto-jr = "" AND ITEM.ge-codigo = 47 THEN
        ASSIGN tp-produto-jr = "off-spec".

    IF tp-produto-jr = "" THEN
        ASSIGN tp-produto-jr = "prime".


    IF tp-produto-jr = "doacao" THEN
        ASSIGN tt-dados.fat-doacao [1] = tt-dados.fat-doacao [1] + qt-faturada-jr
               tt-dados.fat-doacao [2] = tt-dados.fat-doacao [2] + var-vl-liq.


    IF tp-produto-jr = "outros" THEN
        ASSIGN tt-dados.fat-refugo [1] = tt-dados.fat-refugo [1] + qt-faturada-jr
               tt-dados.fat-refugo [2] = tt-dados.fat-refugo [2] + var-vl-liq.


    IF var-tp-merc = "Interno" THEN DO:

        IF tp-produto-jr = "amostra" THEN
            ASSIGN tt-dados.fat-int-amostra [1] = tt-dados.fat-int-amostra [1] + qt-faturada-jr
                   tt-dados.fat-int-amostra [2] = tt-dados.fat-int-amostra [2] + var-vl-liq.


        IF tp-produto-jr = "off-spec" THEN
            ASSIGN tt-dados.fat-int-off-spec [1] = tt-dados.fat-int-off-spec [1] + qt-faturada-jr
                   tt-dados.fat-int-off-spec [2] = tt-dados.fat-int-off-spec [2] + var-vl-liq.


        IF tp-produto-jr = "prime" THEN DO:

           IF nota-fiscal.cod-estabel = "421" THEN
            ASSIGN tt-dados.fat-prime-int-421 [1] = tt-dados.fat-prime-int-421 [1] + qt-faturada-jr
                   tt-dados.fat-prime-int-421 [2] = tt-dados.fat-prime-int-421 [2] + var-vl-liq.
            ELSE
               IF nota-fiscal.cod-estabel = "422" THEN
                ASSIGN tt-dados.fat-prime-int-422 [1] = tt-dados.fat-prime-int-422 [1] + qt-faturada-jr
                       tt-dados.fat-prime-int-422 [2] = tt-dados.fat-prime-int-422 [2] + var-vl-liq.
               ELSE
                   IF nota-fiscal.cod-estabel = "426" THEN
                    ASSIGN tt-dados.fat-prime-int-426 [1] = tt-dados.fat-prime-int-426 [1] + qt-faturada-jr
                           tt-dados.fat-prime-int-426 [2] = tt-dados.fat-prime-int-426 [2] + var-vl-liq.
                   ELSE
                       IF nota-fiscal.cod-estabel = "out" THEN
                        ASSIGN tt-dados.fat-prime-int-out [1] = tt-dados.fat-prime-int-out [1] + qt-faturada-jr
                               tt-dados.fat-prime-int-out [2] = tt-dados.fat-prime-int-out [2] + var-vl-liq.


        END.

    END. /* Mercado Interno */

    IF var-tp-merc = "Externo" THEN DO:

        IF tp-produto-jr = "amostra" THEN
            ASSIGN tt-dados.fat-ext-amostra [1] = tt-dados.fat-ext-amostra [1] + qt-faturada-jr
                   tt-dados.fat-ext-amostra [2] = tt-dados.fat-ext-amostra [2] + var-vl-liq.


        IF tp-produto-jr = "off-spec" THEN
            ASSIGN tt-dados.fat-ext-off-spec [1] = tt-dados.fat-ext-off-spec [1] + qt-faturada-jr
                   tt-dados.fat-ext-off-spec [2] = tt-dados.fat-ext-off-spec [2] + var-vl-liq.


        IF tp-produto-jr = "prime" THEN DO:

           IF nota-fiscal.cod-estabel = "421" THEN
            ASSIGN tt-dados.fat-prime-ext-421 [1] = tt-dados.fat-prime-ext-421 [1] + qt-faturada-jr
                   tt-dados.fat-prime-ext-421 [2] = tt-dados.fat-prime-ext-421 [2] + var-vl-liq.
            ELSE
               IF nota-fiscal.cod-estabel = "422" THEN
                ASSIGN tt-dados.fat-prime-ext-422 [1] = tt-dados.fat-prime-ext-422 [1] + qt-faturada-jr
                       tt-dados.fat-prime-ext-422 [2] = tt-dados.fat-prime-ext-422 [2] + var-vl-liq.
               ELSE
                   IF nota-fiscal.cod-estabel = "426" THEN
                    ASSIGN tt-dados.fat-prime-ext-426 [1] = tt-dados.fat-prime-ext-426 [1] + qt-faturada-jr
                           tt-dados.fat-prime-ext-426 [2] = tt-dados.fat-prime-ext-426 [2] + var-vl-liq.
                   ELSE
                       IF nota-fiscal.cod-estabel = "out" THEN
                        ASSIGN tt-dados.fat-prime-ext-out [1] = tt-dados.fat-prime-ext-out [1] + qt-faturada-jr
                               tt-dados.fat-prime-ext-out [2] = tt-dados.fat-prime-ext-out [2] + var-vl-liq.


        END.


    END. /* Mercado Externo */

    /* Acumulado at‚ ontem */

    IF nota-fiscal.dt-emis-nota <= dt-data-ontem THEN DO:


    IF tp-produto-jr = "doacao" THEN
        ASSIGN tt-dados.fat-doacao [3] = tt-dados.fat-doacao [3] + qt-faturada-jr
               tt-dados.fat-doacao [4] = tt-dados.fat-doacao [4] + var-vl-liq.


    IF tp-produto-jr = "outros" THEN
        ASSIGN tt-dados.fat-refugo [3] = tt-dados.fat-refugo [3] + qt-faturada-jr
               tt-dados.fat-refugo [4] = tt-dados.fat-refugo [4] + var-vl-liq.


    IF var-tp-merc = "Interno" THEN DO:

        IF tp-produto-jr = "amostra" THEN
            ASSIGN tt-dados.fat-int-amostra [3] = tt-dados.fat-int-amostra [3] + qt-faturada-jr
                   tt-dados.fat-int-amostra [4] = tt-dados.fat-int-amostra [4] + var-vl-liq.


        IF tp-produto-jr = "off-spec" THEN
            ASSIGN tt-dados.fat-int-off-spec [3] = tt-dados.fat-int-off-spec [3] + qt-faturada-jr
                   tt-dados.fat-int-off-spec [4] = tt-dados.fat-int-off-spec [4] + var-vl-liq.


        IF tp-produto-jr = "prime" THEN DO:

           IF nota-fiscal.cod-estabel = "421" THEN
            ASSIGN tt-dados.fat-prime-int-421 [3] = tt-dados.fat-prime-int-421 [3] + qt-faturada-jr
                   tt-dados.fat-prime-int-421 [4] = tt-dados.fat-prime-int-421 [4] + var-vl-liq.
            ELSE
               IF nota-fiscal.cod-estabel = "422" THEN
                ASSIGN tt-dados.fat-prime-int-422 [3] = tt-dados.fat-prime-int-422 [3] + qt-faturada-jr
                       tt-dados.fat-prime-int-422 [4] = tt-dados.fat-prime-int-422 [4] + var-vl-liq.
               ELSE
                   IF nota-fiscal.cod-estabel = "426" THEN
                    ASSIGN tt-dados.fat-prime-int-426 [3] = tt-dados.fat-prime-int-426 [3] + qt-faturada-jr
                           tt-dados.fat-prime-int-426 [4] = tt-dados.fat-prime-int-426 [4] + var-vl-liq.
                   ELSE
                       IF nota-fiscal.cod-estabel = "out" THEN
                        ASSIGN tt-dados.fat-prime-int-out [3] = tt-dados.fat-prime-int-out [3] + qt-faturada-jr
                               tt-dados.fat-prime-int-out [4] = tt-dados.fat-prime-int-out [4] + var-vl-liq.


        END.

    END. /* Mercado Interno */

    IF var-tp-merc = "Externo" THEN DO:

        IF tp-produto-jr = "amostra" THEN
            ASSIGN tt-dados.fat-ext-amostra [3] = tt-dados.fat-ext-amostra [3] + qt-faturada-jr
                   tt-dados.fat-ext-amostra [4] = tt-dados.fat-ext-amostra [4] + var-vl-liq.


        IF tp-produto-jr = "off-spec" THEN
            ASSIGN tt-dados.fat-ext-off-spec [3] = tt-dados.fat-ext-off-spec [3] + qt-faturada-jr
                   tt-dados.fat-ext-off-spec [4] = tt-dados.fat-ext-off-spec [4] + var-vl-liq.


        IF tp-produto-jr = "prime" THEN DO:

           IF nota-fiscal.cod-estabel = "421" THEN
            ASSIGN tt-dados.fat-prime-ext-421 [3] = tt-dados.fat-prime-ext-421 [3] + qt-faturada-jr
                   tt-dados.fat-prime-ext-421 [4] = tt-dados.fat-prime-ext-421 [4] + var-vl-liq.
            ELSE
               IF nota-fiscal.cod-estabel = "422" THEN
                ASSIGN tt-dados.fat-prime-ext-422 [3] = tt-dados.fat-prime-ext-422 [3] + qt-faturada-jr
                       tt-dados.fat-prime-ext-422 [4] = tt-dados.fat-prime-ext-422 [4] + var-vl-liq.
               ELSE
                   IF nota-fiscal.cod-estabel = "426" THEN
                    ASSIGN tt-dados.fat-prime-ext-426 [3] = tt-dados.fat-prime-ext-426 [3] + qt-faturada-jr
                           tt-dados.fat-prime-ext-426 [4] = tt-dados.fat-prime-ext-426 [4] + var-vl-liq.
                   ELSE
                       IF nota-fiscal.cod-estabel = "out" THEN
                        ASSIGN tt-dados.fat-prime-ext-out [3] = tt-dados.fat-prime-ext-out [3] + qt-faturada-jr
                               tt-dados.fat-prime-ext-out [4] = tt-dados.fat-prime-ext-out [4] + var-vl-liq.


        END.


    END. /* Mercado Externo */






    END. /* Acumulado at‚ ontem */

    
END.  /*for each */

/* Fim da Rotina para Obter o Faturamento */

/* Rotina de Devolu‡äes */

ASSIGN qt-devolucao-mi = 0
       qt-devolucao-me = 0.

FOR EACH devol-cli WHERE
    devol-cli.dt-devol >= dt-data-inicio AND
    devol-cli.dt-devol <= dt-data-hoje   
    USE-INDEX ch-dt-item
    NO-LOCK,

    EACH nota-fiscal OF devol-cli NO-LOCK.

    IF nota-fiscal.cod-canal-venda <> c-canal-venda-ini THEN NEXT.
                                        
    IF integer(substring(string(devol-cli.nat-operacao),1,1,"character")) = 3 THEN 
       ASSIGN qt-devolucao-me = qt-devolucao-me + devol-cli.qt-devolvida.
    ELSE 
       ASSIGN qt-devolucao-mi = qt-devolucao-mi + devol-cli.qt-devolvida.

END.

ASSIGN tt-dados.qt-devolucao-mi = tt-dados.qt-devolucao-mi + qt-devolucao-mi
       tt-dados.qt-devolucao-me = tt-dados.qt-devolucao-me + qt-devolucao-me.


/* Fim da Rotina de Devolu‡äes */


/* Ler a temp-table e montar a planilha excel */

FIND FIRST tt-dados WHERE
    tt-dados.cod-estabel = c-cod-estabel-ini
    NO-LOCK NO-ERROR.

IF AVAIL tt-dados THEN DO:

    ASSIGN c-relatorio:range ("B6"):VALUE   = tt-dados.budget-mi / 1000 
           c-relatorio:range ("B7"):VALUE   = tt-dados.budget-me / 1000
           c-relatorio:range ("C6"):VALUE   = tt-dados.pedidos-mi-p [1] / 1000
           c-relatorio:range ("C7"):VALUE   = tt-dados.pedidos-me-p [1] / 1000
           c-relatorio:range ("C10"):VALUE   = tt-dados.pedidos-mi-e [1] / 1000
           c-relatorio:range ("C11"):VALUE   = tt-dados.pedidos-me-e [1] / 1000

           c-relatorio:range ("E6"):VALUE   = tt-dados.cotac-mi / 1000
           c-relatorio:range ("E7"):VALUE   = tt-dados.cotac-me / 1000

           c-relatorio:range ("F6"):VALUE   = tt-dados.prod-mi-p  / 1000
           c-relatorio:range ("F7"):VALUE   = tt-dados.prod-me-p  / 1000
           c-relatorio:range ("F10"):VALUE   = tt-dados.prod-mi-e  / 1000
           c-relatorio:range ("F11"):VALUE   = tt-dados.prod-me-e  / 1000

           c-relatorio:range ("I6"):VALUE   = tt-dados.ped-prx-mes-mi-p / 1000
           c-relatorio:range ("I7"):VALUE   = tt-dados.ped-prx-mes-me-p / 1000
           c-relatorio:range ("I10"):VALUE   = tt-dados.ped-prx-mes-mi-e / 1000
           c-relatorio:range ("I11"):VALUE   = tt-dados.ped-prx-mes-me-e / 1000

           c-relatorio:range ("J6"):VALUE   = tt-dados.prod-prx-mes-mi-p / 1000
           c-relatorio:range ("J7"):VALUE   = tt-dados.prod-prx-mes-me-p / 1000
           c-relatorio:range ("J10"):VALUE   = tt-dados.prod-prx-mes-mi-e / 1000
           c-relatorio:range ("J11"):VALUE   = tt-dados.prod-prx-mes-me-e / 1000

           c-relatorio:range ("N6"):VALUE   = (tt-dados.pedidos-mi-p [1] - tt-dados.pedidos-mi-p [2]) / 1000
           c-relatorio:range ("N7"):VALUE   = (tt-dados.pedidos-me-p [1] - tt-dados.pedidos-me-p [2]) / 1000
           c-relatorio:range ("N10"):VALUE   = (tt-dados.pedidos-mi-e [1] - tt-dados.pedidos-mi-e [2]) / 1000
           c-relatorio:range ("N11"):VALUE   = (tt-dados.pedidos-me-e [1] - tt-dados.pedidos-me-e [2]) / 1000.
        
        
    ASSIGN c-relatorio:range ("B21"):VALUE   = tt-dados.budget-mi-tms / 1000 
           c-relatorio:range ("B22"):VALUE   = tt-dados.budget-me-tms / 1000

           c-relatorio:range ("C21"):VALUE   = tt-dados.pedidos-mi-tms-p [1] / 1000
           c-relatorio:range ("C22"):VALUE   = tt-dados.pedidos-me-tms-p [1] / 1000
           c-relatorio:range ("C25"):VALUE   = tt-dados.pedidos-mi-tms-e [1] / 1000
           c-relatorio:range ("C26"):VALUE   = tt-dados.pedidos-me-tms-e [1] / 1000

           c-relatorio:range ("E21"):VALUE   = tt-dados.cotac-mi-tms / 1000
           c-relatorio:range ("E22"):VALUE   = tt-dados.cotac-me-tms / 1000

           c-relatorio:range ("F21"):VALUE   = tt-dados.prod-mi-tms-p  / 1000
           c-relatorio:range ("F22"):VALUE   = tt-dados.prod-me-tms-p  / 1000
           c-relatorio:range ("F25"):VALUE   = tt-dados.prod-mi-tms-e  / 1000
           c-relatorio:range ("F26"):VALUE   = tt-dados.prod-me-tms-e  / 1000

           c-relatorio:range ("I21"):VALUE   = tt-dados.ped-prx-mes-mi-tms-p / 1000
           c-relatorio:range ("I22"):VALUE   = tt-dados.ped-prx-mes-me-tms-p / 1000
           c-relatorio:range ("I25"):VALUE   = tt-dados.ped-prx-mes-mi-tms-e / 1000
           c-relatorio:range ("I26"):VALUE   = tt-dados.ped-prx-mes-me-tms-e / 1000

           c-relatorio:range ("J21"):VALUE   = tt-dados.prod-prx-mes-mi-tms-p / 1000
           c-relatorio:range ("J22"):VALUE   = tt-dados.prod-prx-mes-me-tms-p / 1000
           c-relatorio:range ("J25"):VALUE   = tt-dados.prod-prx-mes-mi-tms-e / 1000
           c-relatorio:range ("J26"):VALUE   = tt-dados.prod-prx-mes-me-tms-e / 1000

           c-relatorio:range ("N21"):VALUE   = (tt-dados.pedidos-mi-tms-p [1] - tt-dados.pedidos-mi-tms-p [2]) / 1000
           c-relatorio:range ("N22"):VALUE   = (tt-dados.pedidos-me-tms-p [1] - tt-dados.pedidos-me-tms-p [2]) / 1000
           c-relatorio:range ("N25"):VALUE   = (tt-dados.pedidos-mi-tms-e [1] - tt-dados.pedidos-mi-tms-e [2]) / 1000
           c-relatorio:range ("N26"):VALUE   = (tt-dados.pedidos-me-tms-e [1] - tt-dados.pedidos-me-tms-e [2]) / 1000.
        

    ASSIGN c-relatorio:range ("D34"):VALUE   = tt-dados.volume-ln-1 [1] / 1000 
           c-relatorio:range ("D35"):VALUE   = tt-dados.volume-ln-2 [1] / 1000
           c-relatorio:range ("D36"):VALUE   = tt-dados.volume-ln-3 [1] / 1000

           c-relatorio:range ("F34"):VALUE   = (tt-dados.volume-ln-1 [1] - tt-dados.volume-ln-1 [2]) / 1000        
           c-relatorio:range ("F35"):VALUE   = (tt-dados.volume-ln-2 [1] - tt-dados.volume-ln-2 [2]) / 1000        
           c-relatorio:range ("F36"):VALUE   = (tt-dados.volume-ln-3 [1] - tt-dados.volume-ln-3 [2]) / 1000        

           c-relatorio:range ("G34"):VALUE   = tt-dados.reprov-ln-1 / 1000
           c-relatorio:range ("G35"):VALUE   = tt-dados.reprov-ln-2 / 1000
           c-relatorio:range ("G36"):VALUE   = tt-dados.reprov-ln-3 / 1000

           c-relatorio:range ("J34"):VALUE   = tt-dados.reserv-ped-ln-1 / 1000
           c-relatorio:range ("J35"):VALUE   = tt-dados.reserv-ped-ln-2 / 1000
           c-relatorio:range ("J36"):VALUE   = tt-dados.reserv-ped-ln-3 / 1000.

    ASSIGN c-relatorio:range ("D58"):VALUE   = tt-dados.est-prime-aloc-venda [1] / 1000 
           c-relatorio:range ("D59"):VALUE   = tt-dados.est-prime-aloc-recor [1] / 1000
           c-relatorio:range ("D60"):VALUE   = tt-dados.est-prime-aloc-metal [1] / 1000
           c-relatorio:range ("D61"):VALUE   = tt-dados.est-prime-aloc-restr [1] / 1000

           c-relatorio:range ("D86"):VALUE   = tt-dados.est-offsp-aloc-venda [1] / 1000
           c-relatorio:range ("D87"):VALUE   = tt-dados.est-offsp-aloc-recor [1] / 1000
           c-relatorio:range ("D88"):VALUE   = tt-dados.est-offsp-aloc-metal [1] / 1000
           c-relatorio:range ("D89"):VALUE   = tt-dados.est-offsp-aloc-restr [1] / 1000.


    ASSIGN c-relatorio:range ("F58"):VALUE   = (tt-dados.est-prime-aloc-venda [1] -  tt-dados.est-prime-aloc-venda [2]) / 1000 
           c-relatorio:range ("F59"):VALUE   = (tt-dados.est-prime-aloc-recor [1] -  tt-dados.est-prime-aloc-recor [2]) / 1000
           c-relatorio:range ("F60"):VALUE   = (tt-dados.est-prime-aloc-metal [1] -  tt-dados.est-prime-aloc-metal [2]) / 1000
           c-relatorio:range ("F61"):VALUE   = (tt-dados.est-prime-aloc-restr [1] -  tt-dados.est-prime-aloc-restr [2]) / 1000
                                                                                                                        
           c-relatorio:range ("F86"):VALUE   = (tt-dados.est-offsp-aloc-venda [1] -  tt-dados.est-offsp-aloc-venda [2]) / 1000
           c-relatorio:range ("F87"):VALUE   = (tt-dados.est-offsp-aloc-recor [1] -  tt-dados.est-offsp-aloc-recor [2]) / 1000
           c-relatorio:range ("F88"):VALUE   = (tt-dados.est-offsp-aloc-metal [1] -  tt-dados.est-offsp-aloc-metal [2]) / 1000
           c-relatorio:range ("F89"):VALUE   = (tt-dados.est-offsp-aloc-restr [1] -  tt-dados.est-offsp-aloc-restr [2]) / 1000.


    ASSIGN c-relatorio:range ("D42"):VALUE   = tt-dados.est-prime-pedido-421 [1] / 1000
           c-relatorio:range ("F42"):VALUE   = (tt-dados.est-prime-pedido-421 [1] - tt-dados.est-prime-pedido-421 [2]) / 1000
           c-relatorio:range ("D43"):VALUE   = tt-dados.est-prime-pedido-422 [1] / 1000
           c-relatorio:range ("F43"):VALUE   = (tt-dados.est-prime-pedido-422 [1] - tt-dados.est-prime-pedido-422 [2]) / 1000
           c-relatorio:range ("D44"):VALUE   = tt-dados.est-prime-pedido-426 [1] / 1000
           c-relatorio:range ("F44"):VALUE   = (tt-dados.est-prime-pedido-426 [1] - tt-dados.est-prime-pedido-426 [2]) / 1000
           c-relatorio:range ("D45"):VALUE   = tt-dados.est-prime-pedido-tsg [1] / 1000
           c-relatorio:range ("F45"):VALUE   = (tt-dados.est-prime-pedido-tsg [1] - tt-dados.est-prime-pedido-tsg [2]) / 1000
           c-relatorio:range ("D46"):VALUE   = tt-dados.est-prime-pedido-rds [1] / 1000
           c-relatorio:range ("F46"):VALUE   = (tt-dados.est-prime-pedido-rds [1] - tt-dados.est-prime-pedido-rds [2]) / 1000
           c-relatorio:range ("D47"):VALUE   = tt-dados.est-prime-pedido-gre [1] / 1000
           c-relatorio:range ("F47"):VALUE   = (tt-dados.est-prime-pedido-gre [1] - tt-dados.est-prime-pedido-gre [2]) / 1000
           c-relatorio:range ("D48"):VALUE   = tt-dados.est-prime-pedido-out [1] / 1000
           c-relatorio:range ("F48"):VALUE   = (tt-dados.est-prime-pedido-out [1] - tt-dados.est-prime-pedido-out [2]) / 1000.



    ASSIGN c-relatorio:range ("D50"):VALUE   = tt-dados.est-prime-sem-pd-421 [1] / 1000
           c-relatorio:range ("F50"):VALUE   = (tt-dados.est-prime-sem-pd-421 [1] - tt-dados.est-prime-sem-pd-421 [2]) / 1000
           c-relatorio:range ("D51"):VALUE   = tt-dados.est-prime-sem-pd-422 [1] / 1000
           c-relatorio:range ("F51"):VALUE   = (tt-dados.est-prime-sem-pd-422 [1] - tt-dados.est-prime-sem-pd-422 [2]) / 1000
           c-relatorio:range ("D52"):VALUE   = tt-dados.est-prime-sem-pd-426 [1] / 1000
           c-relatorio:range ("F52"):VALUE   = (tt-dados.est-prime-sem-pd-426 [1] - tt-dados.est-prime-sem-pd-426 [2]) / 1000
           c-relatorio:range ("D53"):VALUE   = tt-dados.est-prime-sem-pd-tsg [1] / 1000
           c-relatorio:range ("F53"):VALUE   = (tt-dados.est-prime-sem-pd-tsg [1] - tt-dados.est-prime-sem-pd-tsg [2]) / 1000
           c-relatorio:range ("D54"):VALUE   = tt-dados.est-prime-sem-pd-rds [1] / 1000
           c-relatorio:range ("F54"):VALUE   = (tt-dados.est-prime-sem-pd-rds [1] - tt-dados.est-prime-sem-pd-rds [2]) / 1000
           c-relatorio:range ("D55"):VALUE   = tt-dados.est-prime-sem-pd-gre [1] / 1000
           c-relatorio:range ("F55"):VALUE   = (tt-dados.est-prime-sem-pd-gre [1] - tt-dados.est-prime-sem-pd-gre [2]) / 1000
           c-relatorio:range ("D56"):VALUE   = tt-dados.est-prime-sem-pd-out [1] / 1000
           c-relatorio:range ("F56"):VALUE   = (tt-dados.est-prime-sem-pd-out [1] - tt-dados.est-prime-sem-pd-out [2]) / 1000.


    ASSIGN c-relatorio:range ("D63"):VALUE   = tt-dados.est-prime-cliente [1] / 1000
           c-relatorio:range ("F63"):VALUE   = (tt-dados.est-prime-cliente [1] - tt-dados.est-prime-cliente [2]) / 1000
           c-relatorio:range ("D64"):VALUE   = tt-dados.est-prime-ep [1] / 1000                                    
           c-relatorio:range ("F64"):VALUE   = (tt-dados.est-prime-ep [1] - tt-dados.est-prime-ep [2]) / 1000.

    ASSIGN c-relatorio:range ("D70"):VALUE   = tt-dados.est-offsp-pedido-421 [1] / 1000
           c-relatorio:range ("F70"):VALUE   = (tt-dados.est-offsp-pedido-421 [1] - tt-dados.est-offsp-pedido-421 [2]) / 1000
           c-relatorio:range ("D71"):VALUE   = tt-dados.est-offsp-pedido-422 [1] / 1000
           c-relatorio:range ("F71"):VALUE   = (tt-dados.est-offsp-pedido-422 [1] - tt-dados.est-offsp-pedido-422 [2]) / 1000
           c-relatorio:range ("D72"):VALUE   = tt-dados.est-offsp-pedido-426 [1] / 1000
           c-relatorio:range ("F72"):VALUE   = (tt-dados.est-offsp-pedido-426 [1] - tt-dados.est-offsp-pedido-426 [2]) / 1000
           c-relatorio:range ("D73"):VALUE   = tt-dados.est-offsp-pedido-tsg [1] / 1000
           c-relatorio:range ("F73"):VALUE   = (tt-dados.est-offsp-pedido-tsg [1] - tt-dados.est-offsp-pedido-tsg [2]) / 1000
           c-relatorio:range ("D74"):VALUE   = tt-dados.est-offsp-pedido-rds [1] / 1000
           c-relatorio:range ("F74"):VALUE   = (tt-dados.est-offsp-pedido-rds [1] - tt-dados.est-offsp-pedido-rds [2]) / 1000
           c-relatorio:range ("D75"):VALUE   = tt-dados.est-offsp-pedido-gre [1] / 1000
           c-relatorio:range ("F75"):VALUE   = (tt-dados.est-offsp-pedido-gre [1] - tt-dados.est-offsp-pedido-gre [2]) / 1000
           c-relatorio:range ("D76"):VALUE   = tt-dados.est-offsp-pedido-out [1] / 1000
           c-relatorio:range ("F76"):VALUE   = (tt-dados.est-offsp-pedido-out [1] - tt-dados.est-offsp-pedido-out [2]) / 1000.



    ASSIGN c-relatorio:range ("D78"):VALUE   = tt-dados.est-offsp-sem-pd-421 [1] / 1000
           c-relatorio:range ("F78"):VALUE   = (tt-dados.est-offsp-sem-pd-421 [1] - tt-dados.est-offsp-sem-pd-421 [2]) / 1000
           c-relatorio:range ("D79"):VALUE   = tt-dados.est-offsp-sem-pd-422 [1] / 1000
           c-relatorio:range ("F79"):VALUE   = (tt-dados.est-offsp-sem-pd-422 [1] - tt-dados.est-offsp-sem-pd-422 [2]) / 1000
           c-relatorio:range ("D80"):VALUE   = tt-dados.est-offsp-sem-pd-426 [1] / 1000
           c-relatorio:range ("F80"):VALUE   = (tt-dados.est-offsp-sem-pd-426 [1] - tt-dados.est-offsp-sem-pd-426 [2]) / 1000
           c-relatorio:range ("D81"):VALUE   = tt-dados.est-offsp-sem-pd-tsg [1] / 1000
           c-relatorio:range ("F81"):VALUE   = (tt-dados.est-offsp-sem-pd-tsg [1] - tt-dados.est-offsp-sem-pd-tsg [2]) / 1000
           c-relatorio:range ("D82"):VALUE   = tt-dados.est-offsp-sem-pd-rds [1] / 1000
           c-relatorio:range ("F82"):VALUE   = (tt-dados.est-offsp-sem-pd-rds [1] - tt-dados.est-offsp-sem-pd-rds [2]) / 1000
           c-relatorio:range ("D83"):VALUE   = tt-dados.est-offsp-sem-pd-gre [1] / 1000
           c-relatorio:range ("F83"):VALUE   = (tt-dados.est-offsp-sem-pd-gre [1] - tt-dados.est-offsp-sem-pd-gre [2]) / 1000
           c-relatorio:range ("D84"):VALUE   = tt-dados.est-offsp-sem-pd-out [1] / 1000
           c-relatorio:range ("F84"):VALUE   = (tt-dados.est-offsp-sem-pd-out [1] - tt-dados.est-offsp-sem-pd-out [2]) / 1000.


    ASSIGN c-relatorio:range ("D91"):VALUE   = tt-dados.est-offsp-cliente [1] / 1000
           c-relatorio:range ("F91"):VALUE   = (tt-dados.est-offsp-cliente [1] - tt-dados.est-offsp-cliente [2]) / 1000
           c-relatorio:range ("D92"):VALUE   = tt-dados.est-offsp-ep [1] / 1000                                    
           c-relatorio:range ("F92"):VALUE   = (tt-dados.est-offsp-ep [1] - tt-dados.est-offsp-ep [2]) / 1000.

    ASSIGN c-relatorio:range ("E98"):VALUE   = tt-dados.fat-ext-amostra [1] / 1000
           c-relatorio:range ("G98"):VALUE   = tt-dados.fat-ext-amostra [2]

           c-relatorio:range ("E99"):VALUE   = tt-dados.fat-ext-off-specs [1] / 1000
           c-relatorio:range ("G99"):VALUE   = tt-dados.fat-ext-off-specs [2]      

           c-relatorio:range ("E105"):VALUE   = tt-dados.fat-doacao [1] / 1000
           c-relatorio:range ("G105"):VALUE   = tt-dados.fat-doacao [2]      

           c-relatorio:range ("E111"):VALUE   = tt-dados.fat-refugo [1] / 1000  
           c-relatorio:range ("G111"):VALUE   = tt-dados.fat-refugo [2]        

           c-relatorio:range ("E104"):VALUE   = tt-dados.fat-int-amostra [1] / 1000
           c-relatorio:range ("G104"):VALUE   = tt-dados.fat-int-amostra [2]
    
           c-relatorio:range ("E106"):VALUE   = tt-dados.fat-int-off-specs [1] / 1000
           c-relatorio:range ("G106"):VALUE   = tt-dados.fat-int-off-specs [2]      
           
           c-relatorio:range ("E100"):VALUE   = tt-dados.fat-prime-ext-422 [1] / 1000
           c-relatorio:range ("G100"):VALUE   = tt-dados.fat-prime-ext-422 [2]      

           c-relatorio:range ("E101"):VALUE   = tt-dados.fat-prime-ext-421 [1] / 1000
           c-relatorio:range ("G101"):VALUE   = tt-dados.fat-prime-ext-421 [2]      
           
           c-relatorio:range ("E102"):VALUE   = tt-dados.fat-prime-ext-426 [1] / 1000
           c-relatorio:range ("G102"):VALUE   = tt-dados.fat-prime-ext-426 [2]      
           
           c-relatorio:range ("E103"):VALUE   = tt-dados.fat-prime-ext-out [1] / 1000
           c-relatorio:range ("G103"):VALUE   = tt-dados.fat-prime-ext-out [2]      
           
           c-relatorio:range ("E107"):VALUE   = tt-dados.fat-prime-int-422 [1] / 1000
           c-relatorio:range ("G107"):VALUE   = tt-dados.fat-prime-int-422 [2]      

           c-relatorio:range ("E108"):VALUE   = tt-dados.fat-prime-int-421 [1] / 1000
           c-relatorio:range ("G108"):VALUE   = tt-dados.fat-prime-int-421 [2]      
           
           c-relatorio:range ("E109"):VALUE   = tt-dados.fat-prime-int-426 [1] / 1000
           c-relatorio:range ("G109"):VALUE   = tt-dados.fat-prime-int-426 [2]      
           
           c-relatorio:range ("E110"):VALUE   = tt-dados.fat-prime-int-out [1] / 1000
           c-relatorio:range ("G110"):VALUE   = tt-dados.fat-prime-int-out [2] .     

    ASSIGN c-relatorio:range ("E125"):VALUE   = tt-dados.qt-devolucao-mi / 1000
           c-relatorio:range ("E126"):VALUE   = tt-dados.qt-devolucao-me / 1000
           c-relatorio:range ("E127"):VALUE   = (tt-dados.qt-devolucao-mi + tt-dados.qt-devolucao-me) / 1000.


    ASSIGN soma-fat-ext = tt-dados.fat-prime-ext-422 [1] +
                          tt-dados.fat-prime-ext-421 [1] +
                          tt-dados.fat-prime-ext-426 [1] +
                          tt-dados.fat-prime-ext-out [1]. 

    ASSIGN soma-fat-ext = soma-fat-ext -
                         (tt-dados.fat-prime-ext-422 [3] +
                          tt-dados.fat-prime-ext-421 [3] +
                          tt-dados.fat-prime-ext-426 [3] +
                          tt-dados.fat-prime-ext-out [3]). 


    ASSIGN soma-fat-int = tt-dados.fat-prime-int-422 [1] +
                          tt-dados.fat-prime-int-421 [1] +
                          tt-dados.fat-prime-int-426 [1] +
                          tt-dados.fat-prime-int-out [1]. 


    ASSIGN soma-fat-int = soma-fat-int -
                         (tt-dados.fat-prime-int-422 [3] +
                          tt-dados.fat-prime-int-421 [3] +
                          tt-dados.fat-prime-int-426 [3] +
                          tt-dados.fat-prime-int-out [3]). 

    ASSIGN c-relatorio:range ("I116"):VALUE   = soma-fat-int / 1000
           c-relatorio:range ("I117"):VALUE   = soma-fat-ext / 1000.


END.      


/* planilha dos estoque de aditivos e m.primas */

ASSIGN i-linha = 135.

FOR EACH tt-grupo-mat WHERE
         tt-grupo-mat.cod-estabel = c-cod-estabel-ini AND
         tt-grupo-mat.tipo-grupo  = 1
         NO-LOCK.

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    ASSIGN i-linha = i-linha + 1.

    IF i-linha > 155 THEN NEXT.

    ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-grupo-mat.descricao
           c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-grupo-mat.estoque-polo / 1000
           c-relatorio:range("F" + STRING(i-linha)):VALUE = tt-grupo-mat.estoque-externo / 1000
           c-relatorio:range("G" + STRING(i-linha)):VALUE = tt-grupo-mat.consumo-medio / 1000
           c-relatorio:range("I" + STRING(i-linha)):VALUE = tt-grupo-mat.target.

END.


ASSIGN i-linha = 160.


FOR EACH tt-grupo-mat WHERE
         tt-grupo-mat.cod-estabel = c-cod-estabel-ini AND
         tt-grupo-mat.tipo-grupo  = 2
         NO-LOCK.

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    ASSIGN i-linha = i-linha + 1.

    IF i-linha > 164 THEN NEXT.

    ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-grupo-mat.descricao
           c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-grupo-mat.estoque-polo / 1000
           c-relatorio:range("F" + STRING(i-linha)):VALUE = tt-grupo-mat.estoque-externo / 1000.

END.

/* fim da planilha dos estoque de aditivos e m.primas */

IF tt-param.destino = 4 THEN DO:

   RUN pi-finaliza-impressao.
   RUN pi-finalizar IN h-acomp.

   RETURN 'OK'.

END.

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
        with stream-io side-labels overlay row 034 frame f-imp-sel.

   disp stream str-rp "CLASSIFICA€ÇO" skip(01) with stream-io frame f-imp-cla.
   disp stream str-rp "  Posi‡Æo Di ria - PCP"
        with stream-io side-labels overlay row 034 frame f-imp-cla.

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


PROCEDURE pi-cria-planilha:

DEF VAR c-arquivo AS CHAR NO-UNDO.

    c-arquivo = c-arq + 'escp0063' + STRING(time)+ '.xls'.

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
