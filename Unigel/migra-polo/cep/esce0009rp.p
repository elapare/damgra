/*****************************************************************************
**
**       Programa: esce0009rp.p
**
**       Data....: 04/01/2006
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: REG. CONTR.PRODU€ÇO E ESTOQUE
**
**       VersÆo..: 1.00.000 - Jos‚ Roberto R Campos
**
**       OBS.....: 
**
*******************************************************************************/
define variable c-prog-gerado as character no-undo initial "esce0009RP".
def buffer empresa for mgmulti.empresa.
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
    field c-cod-estabel        like movto-estoq.cod-estabel
    field c-ano                AS INTEGER 
    field c-mes                AS INTEGER
    FIELD c-fm-codigo-ini      LIKE item.fm-codigo
    FIELD c-fm-codigo-fim      LIKE item.fm-codigo
    FIELD c-ge-codigo-ini      LIKE item.ge-codigo
    FIELD c-ge-codigo-fim      LIKE item.ge-codigo
    FIELD c-pag-ini            AS INTEGER
    .

DEFINE TEMP-TABLE tt-itens
    FIELD tt-it-codigo         LIKE movto-estoq.it-codigo
    FIELD tt-fm-codigo         LIKE item.fm-codigo
    FIELD tt-ge-codigo         LIKE item.ge-codigo
    FIELD tt-saldo-atu         LIKE movto-estoq.quantidade
    FIELD tt-saldo-ini         LIKE movto-estoq.quantidade
    FIELD tt-entradas          LIKE movto-estoq.quantidade
    FIELD tt-saidas            LIKE movto-estoq.quantidade
    FIELD tt-flag-mes          AS   INT
    INDEX ch-tt-itens IS PRIMARY UNIQUE  tt-it-codigo
                                         tt-fm-codigo
                                         tt-ge-codigo.

DEFINE TEMP-TABLE tt-movtos
    FIELD ttm-it-codigo         LIKE movto-estoq.it-codigo
    FIELD ttm-esp-docto         LIKE movto-estoq.esp-docto
    FIELD ttm-nro-docto         LIKE movto-estoq.nro-docto
    FIELD ttm-serie-docto       LIKE movto-estoq.serie-docto
    FIELD ttm-num-sequen        LIKE movto-estoq.num-sequen
    FIELD ttm-nr-trans          LIKE movto-estoq.nro-docto
    FIELD ttm-dt-trans          LIKE movto-estoq.dt-trans
    FIELD ttm-dia-trans         AS INTEGER
    FIELD ttm-conta-contabil    LIKE movto-estoq.conta-contabil
    FIELD ttm-nat-operacao      LIKE movto-estoq.nat-operacao
    FIELD ttm-tipo-trans        LIKE movto-estoq.tipo-trans
    FIELD ttm-codigo-e-s        AS INTEGER
    FIELD ttm-quantidade        LIKE movto-estoq.quantidade
    FIELD ttm-valor-nota        LIKE movto-estoq.valor-nota
    FIELD ttm-valor-ipi         LIKE movto-estoq.valor-ipi
    FIELD ttm-estoque           LIKE movto-estoq.quantidade
    INDEX ch-tt-movtos IS PRIMARY UNIQUE  ttm-it-codigo
                                          ttm-tipo-trans
                                          ttm-dia-trans
                                          ttm-esp-docto
                                          ttm-nr-trans.

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

def new shared var c-cod-estabel        like movto-estoq.cod-estabel   format "x(3)" initial "423" no-undo.
def new shared var c-ano                AS INTEGER                     format 9999 INITIAL 2005. 
def new shared var c-mes                AS INTEGER                     format 99 INITIAL 1.
def new shared var c-fm-codigo-ini      LIKE item.fm-codigo            format "x(10)" INITIAL "" no-undo.
def new shared var c-fm-codigo-fim      LIKE item.fm-codigo            FORMAT "x(10)" INITIAL "ZZZZZZZZZZ" NO-UNDO.
def new shared var c-ge-codigo-ini      LIKE item.ge-codigo            format 999 initial 0 no-undo.
def new shared var c-ge-codigo-fim      LIKE item.ge-codigo            format 999 initial 999 no-undo.
def new shared var c-pag-ini            AS INTEGER                     format 99999 initial 1.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 

DEFINE VARIABLE esp-docto-jr     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE tipo-trans-jr    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cgc-jr           AS CHARACTER  FORMAT "x(19)" NO-UNDO.
DEFINE VARIABLE ins-estadual-jr  AS CHARACTER  FORMAT "x(19)" NO-UNDO.
DEFINE VARIABLE data-ini-jr      AS DATE       NO-UNDO.
DEFINE VARIABLE entradas-jr      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE saidas-jr        AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dia-jr           AS INT        INITIAL 1 NO-UNDO.
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

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 
DEFINE VARIABLE DESC-jr AS CHARACTER EXTENT 37 INITIAL 
    ["ACA","ACT","","DD ","DEV","DIV","DRM","EAC","","","","","","ICM","INV","IPL","MOB","NC ","","NFD","NFE","NFS","NFT","","REF","RCS","RDD","REQ","RFS","RM ","RRQ","STR","TRA","","SOB","","VAR"].

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

form esp-docto-jr           no-LABEL        FORMAT "x(3)"      AT 001
     ttm-serie-docto        NO-LABEL        FORMAT "x(5)"      AT 005
     ttm-nro-docto          NO-LABEL        FORMAT "x(16)"     AT 011
     ttm-dt-trans           NO-LABEL        FORMAT 99/99/9999  AT 028
     ttm-dia-trans          NO-LABEL        FORMAT ">99"       AT 039
     ttm-conta-contabil     NO-LABEL        FORMAT "x(16)"     AT 043
     ttm-nat-operacao       NO-LABEL        FORMAT "x(6)"      at 060
     tipo-trans-jr          NO-LABEL        FORMAT "x(3)"      AT 067
     ttm-codigo-e-s         NO-LABEL        FORMAT ">>9"       AT 071
     ttm-quantidade         NO-LABEL        FORMAT "->>>>>>,>>9.9999" AT 075
     ttm-valor-nota         NO-LABEL        FORMAT "->>>>>>>>,>>9.99" AT 092
     ttm-valor-ipi          NO-LABEL        FORMAT "->>>>>,>>9.99" AT 109
     estoque-jr             NO-LABEL        FORMAT "->>>>>>,>>9.9999" AT 123
     with down width 140 no-box stream-io frame f-relat-09-132.

form des-total-jr        AT 020
     tipo-tot-jr         at 055          FORMAT "x(3)"
     tot-qtde-jr         NO-LABEL        FORMAT "->>>>>>,>>9.9999" AT 075
     tot-valo-jr         NO-LABEL        FORMAT "->>>>>>>>,>>9.99" AT 092
     tot-ipi-jr          NO-LABEL        FORMAT "->>>>>,>>9.99" AT 109
     WITH down width 140 no-box stream-io frame f-total-09-132.


FORM
    "PRODUTO:"               AT 001
    tt-itens.tt-it-codigo    NO-LABEL       FORMAT "x(16)" AT 010
    "DESCRI€ÇO:"             AT 028
    DESC-item-jr             NO-LABEL       FORMAT "x(19)" AT 039
    "UNIDADE:"               AT 060
    ITEM.un                  AT 069
    "CLAS.FISCAL:"           AT 072
    ITEM.CLASS-FISCAL        AT 085
    "ESTOQUE ANT:"           AT 097
    tt-itens.tt-saldo-ini       AT 110         FORMAT "->>>,>>>,>>9.9999"
    SKIP (1)
    with down width 132 no-box stream-io frame f-item-09-132.


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

assign c-programa     = "esce0009rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "REG. CONTR.PRODU€ÇO E ESTOQUE"
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
    fill("-", 140) format "x(140)" skip
    "FIRMA" AT 001 c-empresa c-titulo-relat at 50
    "CàDIGOS DE ENTRADAS E SAÖDAS" AT 110 SKIP
    "INSC. EST.:" AT 001 ins-estadual-jr AT 013
    "CNPJ.: " AT 040 cgc-jr AT 048
    "1 - No pr¢prio Estabelecimento" AT 110 SKIP
    "FOLHA:" at 001 (page-number(str-rp) + c-pag-ini) at 008 format ">>>>9" 
    "MÒS OU PERÖODO/ANO: " AT 040 c-mes AT 060 "/" AT 065 c-ano AT 068
    "2 - Em outro Estabelecimento" AT 110 skip
    "3 - Diversos" AT 110 SKIP
    "*------------DOCUMENTO-----------* *--------------LAN€AMENTO---------------* *---------------------ENTRADAS E SAÖDAS---------------------*" AT 001 SKIP
    "ESP SRIE NUMERO           DATA       DIA CTA.CONTABIL     FISCAL E/S COD       QUANTIDADE            VALOR           IPI          ESTOQUE" AT 001 SKIP
    fill("-", 140) format "x(140)" skip                          
    with stream-io width 140 no-labels no-box page-top frame f-cabec.

form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    "Periodo:" i-numper-x at 08 "-"
    da-iniper-x at 14 "to" da-fimper-x
    fill("-", 74) format "x(72)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") 
    "Conta" AT 006 SKIP
    fill("-", 132) format "x(132)" skip(1) 
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
           
    assign c-cod-estabel    = tt-param.c-cod-estabel  
           c-ano            = tt-param.c-ano          
           c-mes            = tt-param.c-mes          
           c-fm-codigo-ini  = tt-param.c-fm-codigo-ini
           c-fm-codigo-fim  = tt-param.c-fm-codigo-fim
           c-ge-codigo-ini  = tt-param.c-ge-codigo-ini
           c-ge-codigo-fim  = tt-param.c-ge-codigo-fim
           c-pag-ini        = tt-param.c-pag-ini      
        .


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
        assign v-cod-destino-impres = "Terminal".

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Movimentos Encontrados:").

assign v-num-reg-lidos = 0.

IF c-pag-ini < 1 THEN
    ASSIGN c-pag-ini = 0.
ELSE
    ASSIGN c-pag-ini = c-pag-ini - 1.

FOR EACH tt-itens :
    DELETE tt-itens.
END.

FOR EACH tt-movtos :
    DELETE tt-movtos.
END.

FIND FIRST estabelec WHERE
    estabelec.cod-estabel = c-cod-estabel
    NO-LOCK NO-ERROR.

IF AVAIL estabelec THEN
    ASSIGN  cgc-jr = estabelec.cgc
            ins-estadual-jr = estabelec.ins-estadual.

ASSIGN data-ini-jr = DATE(c-mes,dia-jr,c-ano).

FOR EACH ITEM NO-LOCK WHERE
    ITEM.fm-codigo >= c-fm-codigo-ini AND
    ITEM.fm-codigo <= c-fm-codigo-fim AND
    ITEM.GE-codigo >= c-ge-codigo-ini AND
    ITEM.GE-codigo <= c-ge-codigo-fim
    USE-INDEX grupo.
    
    IF (ITEM.GE-codigo >= 83 AND ITEM.GE-codigo <= 89) OR 
       ITEM.GE-codigo = 98 OR  ITEM.GE-codigo = 99 OR
       ITEM.GE-codigo = 80 THEN NEXT.

    IF ITEM.it-codigo = "244999-4" THEN NEXT.

    FOR each item-estab WHERE 
             item-estab.cod-estabel = c-cod-estabel AND 
             item-estab.it-codigo = ITEM.it-codigo NO-LOCK:
       
        ASSIGN entradas-jr = 0
               saidas-jr   = 0.
      
        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input item.it-codigo).
        
        ASSIGN flag-mes = 0.

        FOR EACH movto-estoq NO-LOCK WHERE
            movto-estoq.it-codigo = item.it-codigo AND
            movto-estoq.dt-trans >= data-ini-jr AND
            movto-estoq.cod-estabel = ITEM-estab.cod-estabel
            USE-INDEX ITEM-data.
      
            IF movto-estoq.tipo-trans = 1 THEN
               ASSIGN entradas-jr = entradas-jr + movto-estoq.quantidade.
                ELSE
                 ASSIGN saidas-jr = saidas-jr + movto-estoq.quantidade.
      
            IF INT (MONTH (movto-estoq.dt-trans)) = c-mes AND
               INT (YEAR  (movto-estoq.dt-trans)) = c-ano THEN DO:
      
                ASSIGN nr-trans-jr = movto-estoq.nro-docto
                       dia-jrx     = INT (DAY(movto-estoq.dt-trans)).

                IF movto-estoq.esp-docto < 20 OR
                   movto-estoq.esp-docto > 23 THEN
                    ASSIGN nr-trans-jr = ""
                           dia-jrx     = 1.

                ASSIGN flag-mes = 9.

                FIND FIRST tt-movtos WHERE
                    tt-movtos.ttm-it-codigo  = movto-estoq.it-codigo AND
                    tt-movtos.ttm-tipo-trans = movto-estoq.tipo-trans AND
                    tt-movtos.ttm-dia-trans  = dia-jrx AND
                    tt-movtos.ttm-esp-docto  = movto-estoq.esp-docto AND
                    tt-movtos.ttm-nr-trans   = nr-trans-jr
                    NO-LOCK NO-ERROR.

                IF NOT AVAIL tt-movtos THEN DO:
                    CREATE tt-movtos.
                    ASSIGN tt-movtos.ttm-it-codigo  = movto-estoq.it-codigo 
                           tt-movtos.ttm-tipo-trans = movto-estoq.tipo-trans 
                           tt-movtos.ttm-dia-trans  = dia-jrx 
                           tt-movtos.ttm-esp-docto  = movto-estoq.esp-docto 
                           tt-movtos.ttm-nr-trans   = nr-trans-jr.  
                END.

                ASSIGN tt-movtos.ttm-dt-trans       = DATE (month(movto-estoq.dt-trans),dia-jrx,year(movto-estoq.dt-trans))
                       tt-movtos.ttm-codigo-e-s     = 1               
                       tt-movtos.ttm-quantidade     = tt-movtos.ttm-quantidade + movto-estoq.quantidade

                       tt-movtos.ttm-valor-nota     = tt-movtos.ttm-valor-nota + movto-estoq.valor-mat-m [1] +    
                                                      movto-estoq.valor-mob-m [1] + movto-estoq.valor-ggf-m [1]
                            
                       tt-movtos.ttm-valor-ipi      = tt-movtos.ttm-valor-ipi + movto-estoq.valor-ipi.     
      

                IF movto-estoq.esp-docto > 19 AND
                   movto-estoq.esp-docto < 24 THEN
                    ASSIGN tt-movtos.ttm-serie-docto    = movto-estoq.serie-docto
                           tt-movtos.ttm-nro-docto      = movto-estoq.nro-docto
                           tt-movtos.ttm-num-sequen     = movto-estoq.num-sequen    
                           tt-movtos.ttm-conta-contabil = movto-estoq.conta-contabil
                           tt-movtos.ttm-nat-operacao   = movto-estoq.nat-operacao.  




            END. /*(movto-estoq-dentro do mˆs)*/
      
        END.  /*(movto-estoq)*/

        ASSIGN saldo-atu = 0.

        FOR EACH ext-saldo-estoq NO-LOCK WHERE
            ext-saldo-estoq.it-codigo   = ITEM.it-codigo    AND
            ext-saldo-estoq.cod-estabel = item-estab.cod-estabel AND
            ext-saldo-estoq.log-saldo   = YES ,
    
            EACH saldo-estoq NO-LOCK WHERE
                 saldo-estoq.cod-estabel  = ext-saldo-estoq.cod-estabel      AND
                 saldo-estoq.cod-depos    = ext-saldo-estoq.cod-depos        AND
                 saldo-estoq.cod-localiz  = ext-saldo-estoq.cod-localiz      AND
                 saldo-estoq.lote         = ext-saldo-estoq.lote             AND
                 saldo-estoq.it-codigo    = ext-saldo-estoq.it-codigo        AND
                 saldo-estoq.cod-refer    = ext-saldo-estoq.cod-refer.       
            
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
                   tt-itens.tt-ge-codigo  = item.ge-codigo.    
        END.

        ASSIGN tt-itens.tt-saldo-atu   = saldo-atu
               tt-itens.tt-saldo-ini   = saldo-atu - entradas-jr + saidas-jr
               tt-itens.tt-entradas    = entradas-jr
               tt-itens.tt-saidas      = saidas-jr
               tt-itens.tt-flag-mes    = flag-mes.  


    END. /*(item-estab)*/

END. /*(item)*/


FOR EACH tt-itens NO-LOCK
    BREAK BY tt-ge-codigo
          BY tt-it-codigo.

    /* NÆo Sair itens sem movimenta‡Æo e saldos */
    
    IF tt-itens.tt-saldo-atu = 0 AND
       tt-itens.tt-saldo-ini = 0 AND
       tt-itens.tt-flag-mes  <> 9 THEN  NEXT.

    /*------------------------------------------*/

    FIND FIRST ITEM WHERE 
        ITEM.it-codigo = tt-itens.tt-it-codigo
        NO-LOCK NO-ERROR.

    IF NOT AVAIL ITEM THEN NEXT.

    IF tt-ge-codigo <> GE-codigo-ant AND GE-codigo-ant <> 0 THEN DO:
        
        PUT STREAM str-rp " ".

        ASSIGN tipo-tot-jr = "Ent"
               tot-qtde-jr = 0
               tot-valo-jr = tote-valo-jr
               tot-ipi-jr  = tote-ipi-jr
               des-total-jr = "TOTAL DO GRUPO:".
        
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp
            des-total-jr          NO-LABEL
            tipo-tot-jr           NO-LABEL
            tot-qtde-jr           NO-LABEL WHEN tot-qtde-jr <> 0
            tot-valo-jr           NO-LABEL
            tot-ipi-jr            NO-LABEL
            with stream-io frame f-total-09-132.
            down stream str-rp with frame f-total-09-132. 
            

        ASSIGN tipo-tot-jr = "Sai"
               tot-qtde-jr = 0
               tot-valo-jr = tots-valo-jr
               tot-ipi-jr  = tots-ipi-jr
               des-total-jr = "   ".
        
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp
            des-total-jr          NO-LABEL
            tipo-tot-jr           NO-LABEL
            tot-qtde-jr           NO-LABEL WHEN tot-qtde-jr <> 0
            tot-valo-jr           NO-LABEL
            tot-ipi-jr            NO-LABEL
            with stream-io frame f-total-09-132.
            down stream str-rp with frame f-total-09-132. 
        
        PUT STREAM str-rp " ".

        ASSIGN tote-valo-jr = 0
               tote-ipi-jr  = 0
               tots-valo-jr = 0
               tots-ipi-jr  = 0.  

    END.

    ASSIGN GE-codigo-ant = tt-ge-codigo.

    ASSIGN DESC-item-jr = ITEM.descricao-1. 
        
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp
            tt-itens.tt-it-codigo NO-LABEL
            DESC-item-jr          NO-LABEL
            ITEM.un               NO-LABEL
            ITEM.CLASS-FISCAL     NO-LABEL   
            tt-itens.tt-saldo-ini NO-LABEL      
            with stream-io frame f-item-09-132.
            down stream str-rp with frame f-item-09-132. 
            
    ASSIGN estoque-jr   = tt-itens.tt-saldo-ini
           ent-qtde-jr  = 0
           ent-valo-jr  = 0
           ent-ipi-jr   = 0
           sai-qtde-jr  = 0
           sai-valo-jr  = 0
           sai-ipi-jr   = 0.

    FOR EACH tt-movtos NO-LOCK WHERE
        tt-movtos.ttm-it-codigo = tt-itens.tt-it-codigo. 

        ASSIGN esp-docto-jr = desc-jr [tt-movtos.ttm-esp-docto].

        IF tt-movtos.ttm-tipo-trans = 1 THEN
            ASSIGN tipo-trans-jr = "Ent"
                   estoque-jr  = estoque-jr  + ttm-quantidade
                   ent-qtde-jr = ent-qtde-jr + ttm-quantidade
                   ent-valo-jr = ent-valo-jr + ttm-valor-nota
                   ent-ipi-jr  = ent-ipi-jr  + ttm-valor-ipi.
             ELSE
              ASSIGN tipo-trans-jr = "Sai"
                     estoque-jr  = estoque-jr - ttm-quantidade
                     sai-qtde-jr = sai-qtde-jr + ttm-quantidade
                     sai-valo-jr = sai-valo-jr + ttm-valor-nota
                     sai-ipi-jr  = sai-ipi-jr  + ttm-valor-ipi.
        
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp
            esp-docto-jr          NO-LABEL
            ttm-serie-docto       NO-LABEL
            ttm-nro-docto         NO-LABEL
            ttm-dt-trans          NO-LABEL
            ttm-dia-trans         NO-LABEL
            ttm-conta-contabil    NO-LABEL
            ttm-nat-operacao      NO-LABEL
            tipo-trans-jr         NO-LABEL
            ttm-codigo-e-s        NO-LABEL
            ttm-quantidade        NO-LABEL
            ttm-valor-ipi         NO-LABEL
            ttm-valor-nota        NO-LABEL
            estoque-jr            NO-LABEL
            with stream-io frame f-relat-09-132.
            down stream str-rp with frame f-relat-09-132. 

    END.

    IF (ent-qtde-jr + ent-valo-jr + ent-ipi-jr) <> 0 OR 
       (sai-qtde-jr + sai-valo-jr + sai-ipi-jr) <> 0 THEN DO:

        PUT STREAM str-rp " ".

        ASSIGN tipo-tot-jr = "Ent"
               tot-qtde-jr = ent-qtde-jr
               tot-valo-jr = ent-valo-jr
               tot-ipi-jr  = ent-ipi-jr
               des-total-jr = "TOTAL DO ITEM:".
        

        ASSIGN tote-valo-jr = tote-valo-jr + ent-valo-jr
               tote-ipi-jr  = tote-ipi-jr  + ent-ipi-jr.
        
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp
            des-total-jr          NO-LABEL
            tipo-tot-jr           NO-LABEL
            tot-qtde-jr           NO-LABEL
            tot-valo-jr           NO-LABEL
            tot-ipi-jr            NO-LABEL
            with stream-io frame f-total-09-132.
            down stream str-rp with frame f-total-09-132. 
            
            ASSIGN tipo-tot-jr = "Sai"
                   tot-qtde-jr = sai-qtde-jr
                   tot-valo-jr = sai-valo-jr
                   tot-ipi-jr  = sai-ipi-jr
                   des-total-jr = "".

            ASSIGN tots-valo-jr = tots-valo-jr + sai-valo-jr
                   tots-ipi-jr  = tots-ipi-jr  + sai-ipi-jr.  

            view stream str-rp frame f-cabec.
            view stream str-rp frame f-rodape.
            assign l-imprime = yes.
            display stream str-rp
                des-total-jr          NO-LABEL
                tipo-tot-jr           NO-LABEL
                tot-qtde-jr           NO-LABEL
                tot-valo-jr           NO-LABEL
                tot-ipi-jr            NO-LABEL
                with stream-io frame f-total-09-132.
                down stream str-rp with frame f-total-09-132. 

           PUT STREAM str-rp " ".

    END.

END.

ASSIGN tipo-tot-jr = "Ent"
       tot-qtde-jr = 0
       tot-valo-jr = tote-valo-jr
       tot-ipi-jr  = tote-ipi-jr
       des-total-jr = "TOTAL DO GRUPO:".

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.
assign l-imprime = yes.
display stream str-rp
    des-total-jr          NO-LABEL
    tipo-tot-jr           NO-LABEL
    tot-qtde-jr           NO-LABEL WHEN tot-qtde-jr <> 0
    tot-valo-jr           NO-LABEL
    tot-ipi-jr            NO-LABEL
    with stream-io frame f-total-09-132.
    down stream str-rp with frame f-total-09-132. 
    

ASSIGN tipo-tot-jr = "Sai"
       tot-qtde-jr = 0
       tot-valo-jr = tots-valo-jr
       tot-ipi-jr  = tots-ipi-jr
       des-total-jr = "   ".

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.
assign l-imprime = yes.
display stream str-rp
    des-total-jr          NO-LABEL
    tipo-tot-jr           NO-LABEL
    tot-qtde-jr           NO-LABEL WHEN tot-qtde-jr <> 0
    tot-valo-jr           NO-LABEL
    tot-ipi-jr            NO-LABEL
    with stream-io frame f-total-09-132.
    down stream str-rp with frame f-total-09-132. 

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
   disp stream str-rp "  REG. CONTR.PRODU€ÇO E ESTOQUE"
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

return 'OK'.

/* fim do programa */
