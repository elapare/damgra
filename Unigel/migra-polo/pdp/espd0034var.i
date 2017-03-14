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
/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

def new shared var c-cod-estabel     like movto-estoq.cod-estabel  format "X(3)"        initial "422"             no-undo. /*solic-318*/ 
def new shared var c-it-codigo-ini   like item.it-codigo           format "x(16)"       initial ""                no-undo.
def new shared var c-it-codigo-fim   like item.it-codigo           format "x(16)"       initial "ZZZZZZZZZZZZZ"   no-undo.
def new shared var c-fm-codigo-ini   like item.fm-codigo           format "x(10)"       initial ""                no-undo.
def new shared var c-fm-codigo-fim   like item.fm-codigo           format "x(10)"       initial "ZZZZZZZZZZ"      no-undo.
def new shared var i-ge-codigo-ini   like item.ge-codigo           format ">>9"         initial 55                no-undo.
def new shared var i-ge-codigo-fim   like item.ge-codigo           format ">>9"         initial 55                no-undo.
def new shared var c-nome-abrev-ini  LIKE emitente.nome-abrev      format "x(12)"       initial ""                no-undo.
def new shared var c-nome-abrev-fim  LIKE emitente.nome-abrev      format "x(12)"       initial "ZZZZZZZZZZZZ"    no-undo.
def new shared var c-tp-pedido-ini   AS   CHAR                     format "x(1)"        initial "A"               no-undo.
def new shared var c-tp-pedido-fim   AS   CHAR                     format "x(1)"        initial "R"               no-undo.
def new shared var dt-entrega-ini    AS   DATE                     format "99/99/9999"  initial today             no-undo.
def new shared var dt-entrega-fim    AS   DATE                     format "99/99/9999"  initial today             no-undo.


/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

DEFINE VARIABLE nome-ab-rep-jr     AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE codrep-jr          AS INTEGER                   NO-UNDO.
DEFINE VARIABLE merc-jr            AS CHAR                      NO-UNDO.
DEFINE VARIABLE saldo-atend        AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE tem-transf-jr      AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE nr-pedcli-jr       AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE tem-data-jr        AS DATE                      NO-UNDO.
DEFINE VARIABLE nat-operacao-jr    AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE nome-abrev-jr      AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE nome-abrev-pai     AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE nome-abrev-filho   AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE nr-pedido-relac-jr AS INT                       NO-UNDO.
DEFINE VARIABLE c-nome-abrev-ord   as char                      no-undo.

/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 

DEFINE VARIABLE var-atendida           AS DECIMAL FORMAT ">>>,>>>,>>9.99"  NO-UNDO.
DEFINE VARIABLE var-pedida             AS DECIMAL FORMAT ">>>,>>>,>>9.99"  NO-UNDO.
DEFINE VARIABLE var-emb-merc           AS DECIMAL FORMAT ">>>,>>>,>>9.99"  NO-UNDO.
DEFINE VARIABLE var-emb-merc-t         AS DECIMAL FORMAT ">>>,>>>,>>9.99"  NO-UNDO.
DEFINE VARIABLE VAR-branco             AS CHARACTER  INITIAL ""            NO-UNDO.
DEFINE VARIABLE perc-atend             AS DECIMAL                          NO-UNDO.
DEFINE VARIABLE perc-prod              AS DECIMAL                          NO-UNDO.
DEFINE VARIABLE var-estoque            AS DECIMAL                          NO-UNDO.
DEFINE VARIABLE produzido-jr           AS DECIMAL                          NO-UNDO.
DEFINE VARIABLE qt-bob-jr              AS DECIMAL                          NO-UNDO.
DEFINE VARIABLE qt-atendida-jr         AS DECIMAL                          NO-UNDO.
define variable de-qt-prod-pedido      as decimal                          no-undo.
define variable de-qt-prod-nao-pallet  as decimal                          no-undo.
define variable de-qt-prod             as decimal                          no-undo.
define VARIABLE i-nr-pedido            as integer                          no-undo.


/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 



/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

def var de-qt-atendida-tt-002 like ped-item.qt-atendida no-undo.
def var de-qt-pedida-tt-001   like ped-item.qt-pedida no-undo.
def var qt-atendida-ed        like ped-item.qt-atendida no-undo.
def var qt-pedida-jr          like ped-item.qt-pedida no-undo.

/***************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/

def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.

DEFINE VARIABLE ext-jr       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-obs-pallet AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dt-validade  AS DATE       NO-UNDO.

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



/**********Tabela temporaria************/

define temp-table tt-fat NO-UNDO
    FIELD var-Larg         AS INTEGER FORMAT ">,>>9"         LABEL "Larg"
    FIELD var-bob-ped      AS INTEGER FORMAT ">,>>9"         LABEL "Bob"
    FIELD var-bob-prod     AS INTEGER FORMAT ">,>>9"         LABEL "Bob"
    FIELD var-diin         AS INTEGER FORMAT ">,>>9"         LABEL "DIN"
    FIELD var-diex         AS INTEGER FORMAT ">,>>9"         LABEL "DIEX"
    FIELD var-pedcli       AS CHAR    FORMAT "x(14)"         LABEL "NR.PED.CLIENTE"
    FIELD nr-ext           AS CHAR    FORMAT "x(12)"         LABEL "NR.EXT"
    FIELD var-vga          AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Vga"
    FIELD var-mtn          AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Mtn"
    FIELD var-gru          AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Gru"
    FIELD var-sbc          AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Sbc"
    FIELD var-outros       AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Out"
    FIELD var-terc         AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Terc"
    FIELD var-transito     AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Trans"
    FIELD nome-merc        AS CHARACTER FORMAT "X(7)"        LABEL "Mercado"
    FIELD gramatura        AS DECIMAL FORMAT "->>>>,>>9.99" DECIMALS 10 LABEL "Gramatura"
    FIELD espessura        AS DECIMAL FORMAT "->>>>,>>9.99" DECIMALS 10 LABEL "Espessura"
    FIELD densidade        AS DECIMAL FORMAT "->>>>,>>9.99" DECIMALS 10 LABEL "Densidade"
    field tem-transf       AS CHAR FORMAT "x(3)"
    field cod-estabel      like ped-venda.cod-estabel
    field dt-entrega       like ped-item.dt-entrega
    field dt-implant       like ped-venda.dt-implant
    field nr-pedcli        like ped-venda.nr-pedcli
    field tp-pedido        like ped-venda.tp-pedido
    FIELD cod-canal-venda  LIKE ped-venda.cod-canal-venda
    field nome-abrev       like ped-venda.nome-abrev
    field it-codigo        like ped-item.it-codigo
    FIELD nr-pedido        LIKE ped-venda.nr-pedido
    FIELD nr-sequencia     LIKE ped-item.nr-sequencia
    FIELD qt-pedida        LIKE ped-item.qt-pedida
    FIELD qt-atendida      LIKE ped-item.qt-atendida
    FIELD vl-preuni        LIKE ped-item.vl-preuni
    FIELD mo-codigo        LIKE ped-venda.mo-codigo
    FIELD nome-ab-rep-jr   AS CHAR FORMAT "x(12)" LABEL "Represent."
    FIELD nome-transp      AS CHAR FORMAT "x(12)"    LABEL "Transport." 
    FIELD obs-pallet       AS CHAR FORMAT "x(100)"    LABEL "Obs-Pallet." 
    FIELD dt-val           AS DATE FORMAT "99/99/9999" LABEL "Dt.validade"
    FIELD var-estoque      AS DECIMAL 
    FIELD referencia       AS DECIMAL
    FIELD nr-pedido-relac  AS INT
    field nome-abrev-pai   as char
    field nome-abrev-filho as char
    FIELD campanha         AS CHAR
    FIELD qt-ordem         AS DEC DECIMALS 4
    FIELD nr-bob-ordem     AS DEC DECIMALS 4.


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

def var l-imprime as logical no-undo.
DEFINE VARIABLE var-Larg     LIKE tt-fat.var-Larg.
DEFINE VARIABLE var-bob-ped  AS   DECIMAL .
DEFINE VARIABLE var-bob-prod AS   DECIMAL .
DEFINE VARIABLE var-diin     LIKE tt-fat.var-diin.
DEFINE VARIABLE var-diex     LIKE tt-fat.var-diex.
DEFINE VARIABLE var-vga      LIKE tt-fat.var-vga    .
DEFINE VARIABLE var-mtn      LIKE tt-fat.var-mtn    .
DEFINE VARIABLE var-gru      LIKE tt-fat.var-gru    .
DEFINE VARIABLE var-sbc      LIKE tt-fat.var-sbc    .
DEFINE VARIABLE var-outros   LIKE tt-fat.var-outros .
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
