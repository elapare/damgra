/*****************************************************************************
**
**       Programa: ESCE0016rp.p
**
**       Data....: 10/05/2009
**
**       Autor...: Amgra - Jos‚ Roberto
**
**       Objetivo: Saldos/Movimentos de Estoque de Materiais
**
**       VersÆo..: 
**
**       OBS.....: 
**
*******************************************************************************/
define variable c-prog-gerado as character no-undo initial "ESCE0016RP".
define buffer empresa for mgmulti.empresa.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

DEF NEW GLOBAL SHARED VAR c-dir-spool-servid-exec AS CHAR NO-UNDO.


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
    field c-cod-estabel        LIKE movto-estoq.cod-estabel
    field c-ge-codigo-ini      LIKE item.ge-codigo
    field c-ge-codigo-fim      LIKE item.ge-codigo
    field c-it-codigo-ini      LIKE item.it-codigo
    field c-it-codigo-fim      LIKE item.it-codigo
    field c-fm-codigo-ini      LIKE item.fm-codigo 
    field c-fm-codigo-fim      LIKE item.fm-codigo 
    field dt-trans-ini         as DATE 
    field dt-trans-fim         as date
    field c-depos-ini          as char
    field c-depos-fim          as char
    field i-nr-linha-ini       as INT 
    field i-nr-linha-fim       as int
    FIELD pesq-jr              as integer
.


DEFINE TEMP-TABLE tt-itens
    FIELD tt-it-codigo         LIKE movto-estoq.it-codigo
    FIELD tt-fm-codigo         LIKE item.fm-codigo
    FIELD tt-ge-codigo         LIKE item.ge-codigo
    FIELD tt-unid              LIKE item.un
    FIELD tt-desc-item         LIKE item.desc-item
    FIELD tt-saldo-atu         LIKE movto-estoq.quantidade
    FIELD tt-ini-qtd           LIKE movto-estoq.quantidade
    FIELD tt-ini-val           LIKE movto-estoq.valor-mat-m [1]
    FIELD tt-ent-qtd           LIKE movto-estoq.quantidade
    FIELD tt-ent-val           LIKE movto-estoq.valor-mat-m [1]
    FIELD tt-sai-qtd           LIKE movto-estoq.quantidade
    FIELD tt-sai-val           LIKE movto-estoq.valor-mat-m [1] 
    FIELD tt-saldo-terc        LIKE movto-estoq.quantidade
    FIELD tt-flag-mes          AS   INT
    INDEX ch-tt-itens IS PRIMARY UNIQUE  tt-ge-codigo
                                         tt-fm-codigo
                                         tt-it-codigo.

DEFINE TEMP-TABLE tt-movtos
    FIELD ttm-it-codigo         LIKE movto-estoq.it-codigo
    FIELD ttm-ge-codigo         LIKE item.ge-codigo
    FIELD ttm-fm-codigo         LIKE item.fm-codigo
    FIELD ttm-quantidade        AS   decimal EXTENT 8
    FIELD ttm-valor             AS   decimal EXTENT 8
    FIELD prd-quantidade        AS   decimal EXTENT 6
    FIELD prd-valor             AS   decimal EXTENT 6
    FIELD con-quantidade        AS   decimal EXTENT 6
    FIELD con-valor             AS   decimal EXTENT 6
    FIELD tra-quantidade        AS   decimal EXTENT 5
    FIELD tra-valor             AS   decimal EXTENT 5
    INDEX ch-tt-movtos IS PRIMARY UNIQUE  ttm-ge-codigo
                                          ttm-fm-codigo
                                          ttm-it-codigo.

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

def new shared var c-cod-estabel     like movto-estoq.cod-estabel  format "X(3)"        initial "422"             no-undo.
def new shared var c-ge-codigo-ini   like item.ge-codigo           format ">>9"         initial 0                 no-undo.
def new shared var c-ge-codigo-fim   like item.ge-codigo           format ">>9"         initial 999               no-undo.
def new shared var c-it-codigo-ini   like item.it-codigo           format "x(16)"       initial 0                 no-undo.
def new shared var c-it-codigo-fim   like item.it-codigo           format "x(16)"       initial "ZZZZZZZZZZZZZ"   no-undo.
def new shared var c-fm-codigo-ini   like item.fm-codigo           format "x(10)"       initial ""                no-undo.
def new shared var c-fm-codigo-fim   like item.fm-codigo           format "x(10)"       initial "ZZZZZZZZZZ"      no-undo.
def new shared var dt-trans-ini      like movto-estoq.dt-trans     format "99/99/9999"  initial today             no-undo.
def new shared var dt-trans-fim      like movto-estoq.dt-trans     format "99/99/9999"  initial today             no-undo.
def new shared var c-depos-ini       like movto-estoq.cod-depos    format "x(03)"       initial ""                no-undo.
def new shared var c-depos-fim       like movto-estoq.cod-depos    format "x(03)"       initial "ZZZ"             no-undo.
def new shared var i-nr-linha-ini    AS INT                        format ">>9"         initial 0                 no-undo.
def new shared var i-nr-linha-fim    AS INT                        format ">>9"         initial 999               no-undo.



/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 

DEFINE VARIABLE d-medio-ini      AS DECIMAL    NO-UNDO.

DEFINE VARIABLE esp-docto-jr     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE tipo-trans-jr    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cgc-jr           AS CHARACTER  FORMAT "x(19)" NO-UNDO.
DEFINE VARIABLE ins-estadual-jr  AS CHARACTER  FORMAT "x(19)" NO-UNDO.
DEFINE VARIABLE entradas-jr      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE saidas-jr        AS DECIMAL    NO-UNDO.
DEFINE VARIABLE nr-trans-jr      AS CHARACTER  FORMAT "x(16)" NO-UNDO.
DEFINE VARIABLE saldo-atu        AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dia-jrx          AS INTEGER    NO-UNDO.
DEFINE VARIABLE DESC-item-jr     AS CHARACTER  FORMAT "x(19)" NO-UNDO.
DEFINE VARIABLE estoque-jr       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE tipo-tot-jr      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ent-qtde-jr      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE ent-valo-jr      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE ent-ipi-jr       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE sai-qtde-jr      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE sai-valo-jr      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE sai-ipi-jr       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE tot-qtde-jr      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE tot-valo-jr      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE tot-ipi-jr       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE des-total-jr     AS CHARACTER  FORMAT "X(15)" NO-UNDO.
DEFINE VARIABLE tote-valo-jr     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE tote-ipi-jr      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE tots-valo-jr     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE tots-ipi-jr      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE GE-codigo-ant    AS INTEGER    INITIAL 0 NO-UNDO.
DEFINE VARIABLE flag-mes         AS INTEGER    NO-UNDO.

DEFINE VARIABLE saldo-terc-jr    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE saldo-atu-val    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE valor-ent-jr     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE ggf-ent-jr       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE valor-sai-jr     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE ggf-sai-jr       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE esp-jr           AS INTEGER    NO-UNDO.
DEFINE VARIABLE idx-jr           AS INTEGER    NO-UNDO.
DEFINE VARIABLE idx-pr           AS INTEGER    NO-UNDO.
DEFINE VARIABLE nr-linha-jr      AS INTEGER    NO-UNDO.

DEFINE VARIABLE ano-jr           AS INT        FORMAT 9999                     NO-UNDO.
DEFINE VARIABLE mes-jr           AS INT        FORMAT 99                       NO-UNDO.
DEFINE VARIABLE dia-jr           AS INT        FORMAT 99           INITIAL 1   NO-UNDO.
DEFINE VARIABLE dt-fech-jr       AS DATE       FORMAT 99/99/9999               NO-UNDO.
DEFINE VARIABLE dt-comeco        AS DATE       FORMAT 99/99/9999               NO-UNDO.
DEFINE VARIABLE depos-jr         AS INTEGER    NO-UNDO.

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

/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 



form HEADER
    fill("-", 140) format "x(140)" SKIP 
    WITH DOWN WIDTH 140 NO-BOX STREAM-IO FRAME f-relat-linha.

form HEADER
    fill(" ", 140) format "x(140)" SKIP 
    WITH DOWN WIDTH 140 NO-BOX STREAM-IO FRAME f-relat-branco.


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

assign c-programa     = "ESCE0016rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Saldos/Movimentos de Estoque de Materiais"
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
           
    assign c-cod-estabel      = tt-param.c-cod-estabel   
           c-ge-codigo-ini    = tt-param.c-ge-codigo-ini 
           c-ge-codigo-fim    = tt-param.c-ge-codigo-fim 
           c-it-codigo-ini    = tt-param.c-it-codigo-ini 
           c-it-codigo-fim    = tt-param.c-it-codigo-fim 
           c-fm-codigo-ini    = tt-param.c-fm-codigo-ini 
           c-fm-codigo-fim    = tt-param.c-fm-codigo-fim 
           dt-trans-ini       = tt-param.dt-trans-ini    
           dt-trans-fim       = tt-param.dt-trans-fim
           c-depos-ini        = tt-param.c-depos-ini   
           c-depos-fim        = tt-param.c-depos-fim   
           i-nr-linha-ini     = tt-param.i-nr-linha-ini 
           i-nr-linha-fim     = tt-param.i-nr-linha-fim.  

find first empresa no-lock
    where empresa.ep-codigo = tt-param.ep-codigo no-error.
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

run pi-inicializar in h-acomp(input "Movimentos Encontrados:").

IF tt-param.destino = 4 THEN DO:
    
    /* Cria Aplica‡Æo do Excel */

    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

    ASSIGN c-modelo-planilha = search("modelos/mod-ESCE0016.xls") 
           c-arq             = IF i-num-ped-exec-rpw = 0 THEN SESSION:TEMP-DIRECTORY ELSE c-dir-spool-servid-exec + '\'.

    RUN pi-cria-planilha.


END.


assign v-num-reg-lidos = 0.

IF tt-param.destino = 4 THEN DO:
       
       ASSIGN c-relatorio:range("I" + STRING(4)):VALUE = tt-param.dt-trans-ini        
              c-relatorio:range("K" + STRING(4)):VALUE = tt-param.dt-trans-fim.        

END.


ASSIGN i-linha = 7. 

FOR EACH tt-itens :
    DELETE tt-itens.
END.

FOR EACH tt-movtos :
    DELETE tt-movtos.
END.

FIND FIRST param-estoq NO-LOCK.

IF AVAIL param-estoq THEN DO:

    ASSIGN dt-fech-jr = param-estoq.ult-fech-dia + 1.
        
END.

ASSIGN dt-comeco = dt-trans-ini.

IF dt-fech-jr < dt-comeco THEN
    ASSIGN dt-comeco = dt-fech-jr. 

find first estabelec where estabelec.cod-estabel = c-cod-estabel no-lock no-error.

FOR EACH ITEM NO-LOCK WHERE
    ITEM.it-codigo >= c-it-codigo-ini AND
    ITEM.it-codigo <= c-it-codigo-fim AND
    ITEM.fm-codigo >= c-fm-codigo-ini AND
    ITEM.fm-codigo <= c-fm-codigo-fim AND
    ITEM.GE-codigo >= c-ge-codigo-ini AND
    ITEM.GE-codigo <= c-ge-codigo-fim
    USE-INDEX grupo.
    
    FOR each item-estab WHERE 
             item-estab.cod-estabel = c-cod-estabel AND 
             item-estab.it-codigo = ITEM.it-codigo NO-LOCK:
       
        ASSIGN entradas-jr  = 0
               saidas-jr    = 0
               valor-ent-jr = 0
               valor-sai-jr = 0
               saldo-terc-jr = 0
               d-medio-ini = 0
               .

        ASSIGN saldo-atu-val = item-estab.sald-ini-mat-m [1]
                d-medio-ini = item-estab.val-unit-mat-m[1] 
                

                .
               
                
                
        IF tt-param.pesq-jr = 2 THEN
           ASSIGN saldo-atu-val = saldo-atu-val + (item-estab.sald-ini-ggf-m [1] + item-estab.sald-ini-mob-m [1])
                   d-medio-ini  =   d-medio-ini + item-estab.val-unit-ggf-m[1]   +  item-estab.val-unit-mob-m[1] 
           .

        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input "Lendo os movimentos: " + string(v-num-reg-lidos)).
        
        ASSIGN flag-mes = 0.

        FOR EACH movto-estoq NO-LOCK WHERE
            movto-estoq.it-codigo   = item.it-codigo     AND
            movto-estoq.dt-trans   >= dt-comeco          AND
            movto-estoq.cod-estabel = ITEM-estab.cod-estabel and
            movto-estoq.cod-depos  >= c-depos-ini        and 
            movto-estoq.cod-depos  <= c-depos-fim   
            USE-INDEX ITEM-data.

            IF movto-estoq.nr-ord-produ > 0 THEN DO:
               
               FIND FIRST ord-prod WHERE
                   ord-prod.nr-ord-produ = movto-estoq.nr-ord-produ
                   NO-LOCK NO-ERROR.

               IF NOT AVAIL ord-prod OR
                  ord-prod.nr-linha < i-nr-linha-ini or
                  ord-prod.nr-linha > i-nr-linha-fim THEN NEXT.

            END.
            
            IF movto-estoq.dt-trans >= dt-fech-jr THEN DO:

                IF movto-estoq.tipo-trans = 1 THEN DO:
            
                   ASSIGN saldo-atu-val  = saldo-atu-val + movto-estoq.valor-mat-m [1].
            
                   IF tt-param.pesq-jr = 2 THEN
                       ASSIGN saldo-atu-val = saldo-atu-val + (movto-estoq.valor-ggf-m [1] + movto-estoq.valor-mob-m [1]).
            
                END.
            
                ELSE DO:
            
                    ASSIGN saldo-atu-val  = saldo-atu-val - movto-estoq.valor-mat-m [1].
            
                    IF tt-param.pesq-jr = 2 THEN
                        ASSIGN saldo-atu-val = saldo-atu-val - (movto-estoq.valor-ggf-m [1] + movto-estoq.valor-mob-m [1]).
            
                END.
            
            
            END.
            
            IF movto-estoq.dt-trans >= dt-trans-ini THEN DO:
            
              IF movto-estoq.tipo-trans = 1 THEN DO:
            
                 ASSIGN entradas-jr  = entradas-jr  + movto-estoq.quantidade
                        valor-ent-jr = valor-ent-jr + movto-estoq.valor-mat-m [1].
            
                 IF tt-param.pesq-jr = 2 THEN
                     ASSIGN valor-ent-jr = valor-ent-jr + (movto-estoq.valor-ggf-m [1] + movto-estoq.valor-mob-m [1]).
            
              END.
            
              ELSE DO:
            
                 ASSIGN saidas-jr    = saidas-jr    + movto-estoq.quantidade
                        valor-sai-jr = valor-sai-jr + movto-estoq.valor-mat-m [1].
            
                 IF tt-param.pesq-jr = 2 THEN
                    ASSIGN valor-sai-jr = valor-sai-jr + (movto-estoq.valor-ggf-m [1] + movto-estoq.valor-mob-m [1]).
            
              END.
            
            END.

            IF movto-estoq.dt-trans >= dt-trans-ini AND movto-estoq.dt-trans <= dt-trans-fim THEN DO:

                ASSIGN flag-mes = 9.

                FIND FIRST tt-movtos WHERE
                    tt-movtos.ttm-it-codigo  = movto-estoq.it-codigo AND
                    tt-movtos.ttm-fm-codigo  = item.fm-codigo        AND
                    tt-movtos.ttm-ge-codigo  = item.ge-codigo 
                    NO-LOCK NO-ERROR.

                IF NOT AVAIL tt-movtos THEN DO:
                    CREATE tt-movtos.
                    ASSIGN tt-movtos.ttm-it-codigo  = movto-estoq.it-codigo 
                           tt-movtos.ttm-fm-codigo  = item.fm-codigo 
                           tt-movtos.ttm-ge-codigo  = item.ge-codigo. 
                END.

                IF movto-estoq.esp-docto = 21 OR movto-estoq.esp-docto = 22 OR movto-estoq.esp-docto = 18 or
                   movto-estoq.esp-docto = 23 THEN
                    ASSIGN esp-jr = 2.  /* NFs */
                ELSE
                 IF movto-estoq.esp-docto = 01 OR movto-estoq.esp-docto = 08 THEN
                     ASSIGN esp-jr = 3.  /* Produ‡Æo */
                 ELSE
                   IF movto-estoq.esp-docto = 28 OR movto-estoq.esp-docto = 30 OR
                      movto-estoq.esp-docto = 31 OR movto-estoq.esp-docto = 05 OR
                      movto-estoq.esp-docto = 07  THEN
                       ASSIGN esp-jr = 4.  /* Consumo */
                   ELSE
                     IF movto-estoq.esp-docto = 33 THEN
                         ASSIGN esp-jr = 5.  /* Transferencias */
                     ELSE
                       IF movto-estoq.esp-docto = 90 THEN
                           ASSIGN esp-jr = 6.  /* Devolu‡äes */
                       ELSE
                           ASSIGN esp-jr = 7.  /* Outros */

                ASSIGN idx-jr = esp-jr.

                IF esp-jr = 3 OR esp-jr = 4 THEN DO:     /* Ver a ordem de produ‡Æo para produ‡Æo/consumo */

                    ASSIGN nr-linha-jr = 900
                           idx-pr      = 6.

                    FIND FIRST ord-prod WHERE
                        ord-prod.nr-ord-produ = movto-estoq.nr-ord-produ
                        NO-LOCK NO-ERROR.

                    IF AVAIL ord-prod THEN
                        ASSIGN nr-linha-jr = ord-prod.nr-linha.
                        
                    if avail ord-prod and item.ge-codigo = 20 then
                       assign nr-linha-jr = 600.    

                    IF nr-linha-jr > 0 AND nr-linha-jr < 200 THEN
                        ASSIGN idx-pr = 1.

                    IF nr-linha-jr > 199 AND nr-linha-jr < 300 THEN
                        ASSIGN idx-pr = 2.

                    IF nr-linha-jr > 299 AND nr-linha-jr < 400 THEN
                        ASSIGN idx-pr = 3.

                    IF nr-linha-jr > 399 AND nr-linha-jr < 500 THEN
                        ASSIGN idx-pr = 4.

                    IF nr-linha-jr > 499 AND nr-linha-jr < 600 THEN
                        ASSIGN idx-pr = 1.

                    IF nr-linha-jr > 599 AND nr-linha-jr < 900 THEN
                        ASSIGN idx-pr = 5.

                END.

             /* Fim de Ver a ordem de produ‡Æo para produ‡Æo/consumo */


                IF movto-estoq.tipo-trans = 1 THEN DO:

                    ASSIGN tt-movtos.ttm-quantidade [idx-jr] = tt-movtos.ttm-quantidade [idx-jr] + movto-estoq.quantidade.
                           tt-movtos.ttm-valor [idx-jr]      = tt-movtos.ttm-valor [idx-jr] + movto-estoq.valor-mat-m [1].

                    IF tt-param.pesq-jr = 2 THEN
                        ASSIGN tt-movtos.ttm-valor [idx-jr] = tt-movtos.ttm-valor [idx-jr] + (movto-estoq.valor-ggf-m [1] + movto-estoq.valor-mob-m [1]).


                    IF esp-jr = 5 THEN DO:      /* Transferencia - abrir por dep¢sito nas entradas */

                        ASSIGN depos-jr = 5.

                        IF movto-estoq.cod-depos = "CQ" THEN
                            ASSIGN depos-jr = 1.
                        ELSE
                            IF movto-estoq.cod-depos = "PRO" THEN
                                ASSIGN depos-jr = 2.
                            ELSE
                                IF movto-estoq.cod-depos = "EXP" THEN
                                    ASSIGN depos-jr = 3.
                                ELSE
                                    IF movto-estoq.cod-depos = "ARC" THEN
                                        ASSIGN depos-jr = 4.
                                    ELSE
                                            ASSIGN depos-jr = 1.


                    ASSIGN tt-movtos.tra-quantidade [depos-jr] = tt-movtos.tra-quantidade [depos-jr] + movto-estoq.quantidade.
                           tt-movtos.tra-valor [depos-jr]      = tt-movtos.tra-valor [depos-jr] + movto-estoq.valor-mat-m [1].

                    IF tt-param.pesq-jr = 2 THEN
                        ASSIGN tt-movtos.tra-valor [depos-jr] = tt-movtos.tra-valor [depos-jr] + (movto-estoq.valor-ggf-m [1] + movto-estoq.valor-mob-m [1]).


                    END. /* fim da Transferencia - abrir por dep¢sito  nas entradas */

                    
                    IF esp-jr = 3 THEN DO:  /* Abertura da Produ‡Æo na entrada */

                        ASSIGN tt-movtos.prd-quantidade [idx-pr] = tt-movtos.prd-quantidade [idx-pr] + movto-estoq.quantidade.
                               tt-movtos.prd-valor [idx-pr]      = tt-movtos.prd-valor [idx-pr] + movto-estoq.valor-mat-m [1].

                        IF tt-param.pesq-jr = 2 THEN
                            ASSIGN tt-movtos.prd-valor [idx-pr] = tt-movtos.prd-valor [idx-pr] + (movto-estoq.valor-ggf-m [1] + movto-estoq.valor-mob-m [1]).

                    END.

                    IF esp-jr = 4 THEN DO:  /* Abertura do Consumo na entrada */

                        ASSIGN tt-movtos.con-quantidade [idx-pr] = tt-movtos.con-quantidade [idx-pr] + movto-estoq.quantidade.
                               tt-movtos.con-valor [idx-pr]      = tt-movtos.con-valor [idx-pr] + movto-estoq.valor-mat-m [1].

                        IF tt-param.pesq-jr = 2 THEN
                            ASSIGN tt-movtos.con-valor [idx-pr] = tt-movtos.con-valor [idx-pr] + (movto-estoq.valor-ggf-m [1] + movto-estoq.valor-mob-m [1]).

                    END.

                END.

                ELSE DO:

                    ASSIGN tt-movtos.ttm-quantidade [idx-jr] = tt-movtos.ttm-quantidade [idx-jr] - movto-estoq.quantidade.
                           tt-movtos.ttm-valor [idx-jr]      = tt-movtos.ttm-valor [idx-jr] - movto-estoq.valor-mat-m [1].

                    IF tt-param.pesq-jr = 2 THEN
                        ASSIGN tt-movtos.ttm-valor [idx-jr] = tt-movtos.ttm-valor [idx-jr] - (movto-estoq.valor-ggf-m [1] + movto-estoq.valor-mob-m [1]).

                    IF esp-jr = 3 THEN DO:  /* Abertura da Produ‡Æo na saida */

                        ASSIGN tt-movtos.prd-quantidade [idx-pr] = tt-movtos.prd-quantidade [idx-pr] - movto-estoq.quantidade.
                               tt-movtos.prd-valor [idx-pr]      = tt-movtos.prd-valor [idx-pr] - movto-estoq.valor-mat-m [1].

                        IF tt-param.pesq-jr = 2 THEN
                            ASSIGN tt-movtos.prd-valor [idx-pr] = tt-movtos.prd-valor [idx-pr] - (movto-estoq.valor-ggf-m [1] + movto-estoq.valor-mob-m [1]).

                    END.

                    IF esp-jr = 4 THEN DO:  /* Abertura do Consumo na saida */

                        ASSIGN tt-movtos.con-quantidade [idx-pr] = tt-movtos.con-quantidade [idx-pr] - movto-estoq.quantidade.
                               tt-movtos.con-valor [idx-pr]      = tt-movtos.con-valor [idx-pr] - movto-estoq.valor-mat-m [1].

                        IF tt-param.pesq-jr = 2 THEN
                            ASSIGN tt-movtos.con-valor [idx-pr] = tt-movtos.con-valor [idx-pr] - (movto-estoq.valor-ggf-m [1] + movto-estoq.valor-mob-m [1]).

                    END. 


                    IF esp-jr = 5 THEN DO:      /* Transferencia - abrir por dep¢sito nas sa¡das */

                        ASSIGN depos-jr = 5.

                        IF movto-estoq.cod-depos = "CQ" THEN
                            ASSIGN depos-jr = 1.
                        ELSE
                            IF movto-estoq.cod-depos = "PRO" THEN
                                ASSIGN depos-jr = 2.
                            ELSE
                                IF movto-estoq.cod-depos = "EXP" THEN
                                    ASSIGN depos-jr = 3.
                                ELSE
                                    IF movto-estoq.cod-depos = "ARC" THEN
                                        ASSIGN depos-jr = 4.
                                    ELSE
                                            ASSIGN depos-jr = 1.


                    ASSIGN tt-movtos.tra-quantidade [depos-jr] = tt-movtos.tra-quantidade [depos-jr] - movto-estoq.quantidade.
                           tt-movtos.tra-valor [depos-jr]      = tt-movtos.tra-valor [depos-jr] - movto-estoq.valor-mat-m [1].

                    IF tt-param.pesq-jr = 2 THEN
                        ASSIGN tt-movtos.tra-valor [depos-jr] = tt-movtos.tra-valor [depos-jr] - (movto-estoq.valor-ggf-m [1] + movto-estoq.valor-mob-m [1]).


                    END. /* fim da Transferencia - abrir por dep¢sito  nas sa¡das */


                END.

            END. /*(movto-estoq-dentro do mˆs)*/
      
        END.  /*(movto-estoq)*/

        ASSIGN saldo-atu = 0
               saldo-terc-jr = 0.

        FOR EACH saldo-estoq NO-LOCK WHERE
                 saldo-estoq.it-codigo    = ITEM.it-codigo                   AND
                 saldo-estoq.cod-estabel  = item-estab.cod-estabel           AND
                 saldo-estoq.cod-depos >= c-depos-ini  and 
                 saldo-estoq.cod-depos <= c-depos-fim  and 
                 saldo-estoq.qtidade-atu <> 0
                 USE-INDEX ITEM.       
            
            ASSIGN saldo-atu = saldo-atu + saldo-estoq.qtidade-atu.
         
        END.  
        
        FIND FIRST tt-itens WHERE
                   tt-itens.tt-it-codigo  = item.it-codigo AND
                   tt-itens.tt-fm-codigo  = item.fm-codigo AND
                   tt-itens.tt-ge-codigo  = item.ge-codigo
                   NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-itens THEN DO:
            CREATE tt-itens.
            ASSIGN tt-itens.tt-it-codigo  = item.it-codigo 
                   tt-itens.tt-fm-codigo  = item.fm-codigo 
                   tt-itens.tt-ge-codigo  = item.ge-codigo
                   tt-itens.tt-desc-item  = item.desc-item
                   tt-itens.tt-unid       = item.un.    
        END.
        
        
     
                
        

        ASSIGN tt-itens.tt-saldo-atu   = saldo-atu
               tt-itens.tt-ini-qtd     = saldo-atu - entradas-jr + saidas-jr
               tt-itens.tt-ini-val     = saldo-atu-val - valor-ent-jr + valor-sai-jr
               tt-itens.tt-ent-qtd     = entradas-jr
               tt-itens.tt-ent-val     = valor-ent-jr
               tt-itens.tt-sai-qtd     = saidas-jr
               tt-itens.tt-sai-val     = valor-sai-jr
               tt-itens.tt-flag-mes    = flag-mes.  
               if c-depos-ini <> "" or c-depos-fim <> "ZZZ" then
        
            tt-itens.tt-ini-val =    tt-itens.tt-ini-qtd  *  d-medio-ini.

    END. /*(item-estab)*/

END. /*(item)*/

assign v-num-reg-lidos = 0.

FOR EACH tt-itens 
    BREAK BY tt-ge-codigo
          BY tt-it-codigo.

    /* Verificar saldo em Terceiros */

    ASSIGN saldo-terc-jr = 0.

    FOR EACH saldo-terc WHERE
        saldo-terc.cod-estabel = tt-param.c-cod-estabel AND
        saldo-terc.it-codigo   = tt-itens.tt-it-codigo  AND
        saldo-terc.quantidade > 0
        NO-LOCK.

        ASSIGN saldo-terc-jr = saldo-terc-jr + saldo-terc.quantidade.

    END.

    FOR EACH componente where
        componente.it-codigo   = tt-itens.tt-it-codigo  AND
        componente.dt-retorno  > dt-trans-fim
        USE-INDEX retorno NO-LOCK.

        FIND FIRST saldo-terc OF componente NO-LOCK NO-ERROR.

        IF NOT AVAIL saldo-terc OR 
           saldo-terc.cod-estabel <> tt-param.c-cod-estabel THEN NEXT.

        IF componente.componente = 1 THEN
           ASSIGN saldo-terc-jr = saldo-terc-jr - componente.quantidade.
        ELSE
           ASSIGN saldo-terc-jr = saldo-terc-jr + componente.quantidade.

    END.

    IF saldo-terc-jr < 0 THEN ASSIGN saldo-terc-jr = 0.

    ASSIGN tt-itens.tt-saldo-terc = saldo-terc-jr.


    /* NÆo Sair itens sem movimenta‡Æo e saldos */
    
    IF tt-itens.tt-saldo-atu  = 0 AND
       tt-itens.tt-ini-qtd    = 0 AND
       tt-itens.tt-ini-val    = 0 AND
       tt-itens.tt-saldo-terc = 0 AND
       tt-itens.tt-flag-mes  <> 9 THEN  NEXT.


    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input "Gerando Planilha: " + string(v-num-reg-lidos)).
        


    /*------------------------------------------*/

    FIND FIRST tt-movtos WHERE
        tt-movtos.ttm-it-codigo = tt-itens.tt-it-codigo AND
        tt-movtos.ttm-fm-codigo = tt-itens.tt-fm-codigo AND
        tt-movtos.ttm-ge-codigo = tt-itens.tt-ge-codigo 
        NO-LOCK NO-ERROR.

    IF tt-param.destino = 4 THEN DO:

        ASSIGN i-linha = i-linha + 1.

        ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-param.c-cod-estabel  
               c-relatorio:range("B" + STRING(i-linha)):VALUE = tt-itens.tt-it-codigo
               c-relatorio:range("C" + STRING(i-linha)):VALUE = tt-itens.tt-desc-item
               c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-itens.tt-unid     
               c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-itens.tt-ini-qtd     
               c-relatorio:range("F" + STRING(i-linha)):VALUE = tt-itens.tt-ini-val
               c-relatorio:range("U" + STRING(i-linha)):VALUE = tt-itens.tt-ge-codigo.

        IF AVAIL tt-movtos THEN DO:

            ASSIGN c-relatorio:range("G" + STRING(i-linha)):VALUE = tt-movtos.ttm-quantidade [2]  
                   c-relatorio:range("H" + STRING(i-linha)):VALUE = tt-movtos.ttm-valor [2]  
                   c-relatorio:range("I" + STRING(i-linha)):VALUE = tt-movtos.ttm-quantidade [3]  
                   c-relatorio:range("J" + STRING(i-linha)):VALUE = tt-movtos.ttm-valor [3]       
                   c-relatorio:range("K" + STRING(i-linha)):VALUE = tt-movtos.ttm-quantidade [4]  
                   c-relatorio:range("L" + STRING(i-linha)):VALUE = tt-movtos.ttm-valor [4]       
                   c-relatorio:range("M" + STRING(i-linha)):VALUE = tt-movtos.ttm-quantidade [5]  
                   c-relatorio:range("N" + STRING(i-linha)):VALUE = tt-movtos.ttm-valor [5]       
                   c-relatorio:range("O" + STRING(i-linha)):VALUE = tt-movtos.ttm-quantidade [6]  
                   c-relatorio:range("P" + STRING(i-linha)):VALUE = tt-movtos.ttm-valor [6]       
                   c-relatorio:range("Q" + STRING(i-linha)):VALUE = tt-movtos.ttm-quantidade [7]  
                   c-relatorio:range("R" + STRING(i-linha)):VALUE = tt-movtos.ttm-valor [7].

            ASSIGN c-relatorio:range("S" + STRING(i-linha)):VALUE = (tt-itens.tt-ini-qtd + tt-movtos.ttm-quantidade [2] + 
                                                                     tt-movtos.ttm-quantidade [3] +
                                                                     tt-movtos.ttm-quantidade [4] +
                                                                     tt-movtos.ttm-quantidade [5] +
                                                                     tt-movtos.ttm-quantidade [6] +
                                                                     tt-movtos.ttm-quantidade [7]). 
                                                                    

            ASSIGN c-relatorio:range("T" + STRING(i-linha)):VALUE = (tt-itens.tt-ini-val + tt-movtos.ttm-valor [2] + 
                                                                     tt-movtos.ttm-valor [3] +
                                                                     tt-movtos.ttm-valor [4] +
                                                                     tt-movtos.ttm-valor [5] +
                                                                     tt-movtos.ttm-valor [6] +
                                                                     tt-movtos.ttm-valor [7]). 

            ASSIGN c-relatorio:range("V"  + STRING(i-linha)):VALUE = tt-movtos.prd-quantidade [1]  
                   c-relatorio:range("W"  + STRING(i-linha)):VALUE = tt-movtos.prd-valor [1]  
                   c-relatorio:range("AG" + STRING(i-linha)):VALUE = tt-movtos.con-quantidade [1]
                   c-relatorio:range("AH" + STRING(i-linha)):VALUE = tt-movtos.con-valor [1]     

                   c-relatorio:range("X"  + STRING(i-linha)):VALUE = tt-movtos.prd-quantidade [2]
                   c-relatorio:range("Y"  + STRING(i-linha)):VALUE = tt-movtos.prd-valor [2]     
                   c-relatorio:range("AI" + STRING(i-linha)):VALUE = tt-movtos.con-quantidade [2]
                   c-relatorio:range("AJ" + STRING(i-linha)):VALUE = tt-movtos.con-valor [2]   

                   c-relatorio:range("Z"  + STRING(i-linha)):VALUE = tt-movtos.prd-quantidade [3]
                   c-relatorio:range("AA" + STRING(i-linha)):VALUE = tt-movtos.prd-valor [3]     
                   c-relatorio:range("AK" + STRING(i-linha)):VALUE = tt-movtos.con-quantidade [3]
                   c-relatorio:range("AL" + STRING(i-linha)):VALUE = tt-movtos.con-valor [3]     

                   c-relatorio:range("AB" + STRING(i-linha)):VALUE = tt-movtos.prd-quantidade [4]
                   c-relatorio:range("AC" + STRING(i-linha)):VALUE = tt-movtos.prd-valor [4]     
                   c-relatorio:range("AM" + STRING(i-linha)):VALUE = tt-movtos.con-quantidade [4]
                   c-relatorio:range("AN" + STRING(i-linha)):VALUE = tt-movtos.con-valor [4]     

                   c-relatorio:range("AD" + STRING(i-linha)):VALUE = tt-movtos.prd-quantidade [5]
                   c-relatorio:range("AE" + STRING(i-linha)):VALUE = tt-movtos.prd-valor [5]     
                   c-relatorio:range("AO" + STRING(i-linha)):VALUE = tt-movtos.con-quantidade [5]
                   c-relatorio:range("AP" + STRING(i-linha)):VALUE = tt-movtos.con-valor [5]     
                   c-relatorio:range("AQ" + STRING(i-linha)):VALUE = tt-movtos.con-quantidade [6]
                   c-relatorio:range("AR" + STRING(i-linha)):VALUE = tt-movtos.con-valor [6]  

                   c-relatorio:range("AT" + STRING(i-linha)):VALUE = tt-movtos.tra-quantidade [1]
                   c-relatorio:range("AU" + STRING(i-linha)):VALUE = tt-movtos.tra-valor [1]     
                   c-relatorio:range("AV" + STRING(i-linha)):VALUE = tt-movtos.tra-quantidade [2]
                   c-relatorio:range("AW" + STRING(i-linha)):VALUE = tt-movtos.tra-valor [2]     
                   c-relatorio:range("AX" + STRING(i-linha)):VALUE = tt-movtos.tra-quantidade [3]
                   c-relatorio:range("AY" + STRING(i-linha)):VALUE = tt-movtos.tra-valor [3]     
                   c-relatorio:range("AZ" + STRING(i-linha)):VALUE = tt-movtos.tra-quantidade [4]
                   c-relatorio:range("BA" + STRING(i-linha)):VALUE = tt-movtos.tra-valor [4]     
                   c-relatorio:range("BB" + STRING(i-linha)):VALUE = tt-movtos.tra-quantidade [5]
                   c-relatorio:range("BC" + STRING(i-linha)):VALUE = tt-movtos.tra-valor [5] .    



        END.  /* IF AVAIL tt-movtos  */

        ELSE 
            ASSIGN c-relatorio:range("S" + STRING(i-linha)):VALUE = tt-itens.tt-ini-qtd
                   c-relatorio:range("T" + STRING(i-linha)):VALUE = tt-itens.tt-ini-val. 

        ASSIGN c-relatorio:range("AS" + STRING(i-linha)):VALUE = tt-itens.tt-saldo-terc. 

    END.  /* tt-param.destino = 4 */

END.  /* for each tt-itens */


 IF tt-param.destino = 4 and avail estabelec THEN DO:

     find first empresa no-lock
            where empresa.ep-codigo = estabelec.ep-codigo no-error.
     
     if  avail empresa then


        ASSIGN c-relatorio:range("A" + STRING(1)):VALUE = "Empresa: " + empresa.razao-social + " / Estab: " + estabelec.nome.  
      
 end.     

run pi-finalizar in h-acomp.


IF tt-param.destino = 4 THEN DO:



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

    c-arquivo = c-arq + 'ESCE0016' + STRING(time)+ '.xls'.

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

    IF i-num-ped-exec-rpw = 0 THEN DO:
        c-excel:VISIBLE = true.

        DO i = 1 TO NUM-ENTRIES(c-arq-anexo):
            c-arquivo = ENTRY(i,c-arq-anexo).
            c-planilha  = c-excel:Workbooks:OPEN(c-arquivo).
        END.
    END.
    ELSE
        c-excel:QUIT().

    RELEASE OBJECT c-excel.

END PROCEDURE.                  



return 'OK'.

/* fim do programa */


