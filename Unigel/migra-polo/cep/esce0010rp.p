/*****************************************************************************
**
**       Programa: esce0010rp.p
**
**       Data....: 19/01/2006
**
**       Autor...: JosÇ Roberto
**
**       Objetivo: Resumo Movimentos de Estoque (Fechamento do Màs)
**
**       Vers∆o..: 1.00.000 - jrrcampos
**
**       OBS.....: 
**
*******************************************************************************/
def buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "esce0010RP".

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
    field c-cod-estabel        LIKE movto-estoq.cod-estabel
    field c-ge-codigo-ini      LIKE item.ge-codigo
    field c-ge-codigo-fim      LIKE item.ge-codigo
    field c-it-codigo-ini      LIKE item.it-codigo
    field c-it-codigo-fim      LIKE item.it-codigo
    field c-cod-depos-ini      LIKE movto-estoq.cod-depos
    field c-cod-depos-fim      LIKE movto-estoq.cod-depos
    field c-mes                as integer
    field c-ano                as integer
    FIELD pesq-jr              as integer
.


DEFINE TEMP-TABLE tt-itens
    FIELD ttite-cod-estabel          LIKE movto-estoq.cod-estabel
    FIELD ttite-it-codigo            LIKE movto-estoq.it-codigo

    FIELD ttite-data-ult-fecham      AS DATE

    FIELD ttite-v-atu-mat            AS DECIMAL
    FIELD ttite-v-atu-mat-ggf        AS DECIMAL

    FIELD ttite-v-ant-mat            AS DECIMAL
    FIELD ttite-v-ant-mat-ggf        AS DECIMAL

    FIELD ttite-v-ent-mat            AS DECIMAL
    FIELD ttite-v-ent-mat-ggf        AS DECIMAL

    FIELD ttite-v-sai-mat            AS DECIMAL
    FIELD ttite-v-sai-mat-ggf        AS DECIMAL

    FIELD ttite-q-ant                AS DECIMAL
    FIELD ttite-medio-mat            AS DECIMAL
    FIELD ttite-medio-mat-ggf        AS DECIMAL

    INDEX ch-tt-itens IS PRIMARY UNIQUE  ttite-cod-estabel
                                         ttite-it-codigo. 
                                        

DEFINE TEMP-TABLE tt-grupos
    FIELD ttgru-cod-estabel          LIKE movto-estoq.cod-estabel
    FIELD ttgru-ge-codigo            LIKE item.ge-codigo
    FIELD ttgru-it-codigo            LIKE movto-estoq.it-codigo
    FIELD ttgru-cod-depos            LIKE movto-estoq.cod-depos


    FIELD ttgru-q-ant                AS DECIMAL
    FIELD ttgru-q-atu                AS DECIMAL

    FIELD ttgru-q-ent-tot            AS DECIMAL
    FIELD ttgru-q-sai-tot            AS DECIMAL

    FIELD ttgru-q-ent                AS DECIMAL
    FIELD ttgru-q-sai                AS DECIMAL

    FIELD ttgru-v-ent-mat            AS DECIMAL
    FIELD ttgru-v-ent-mat-ggf        AS DECIMAL

    FIELD ttgru-v-sai-mat            AS DECIMAL
    FIELD ttgru-v-sai-mat-ggf        AS DECIMAL

    INDEX ch-tt-grupos IS PRIMARY UNIQUE  ttgru-cod-estabel
                                          ttgru-ge-codigo 
                                          ttgru-it-codigo 
                                          ttgru-cod-depos. 


DEFINE TEMP-TABLE tt-relat
    FIELD ttrel-cod-estabel          LIKE movto-estoq.cod-estabel
    FIELD ttrel-ge-codigo            LIKE item.ge-codigo
    FIELD ttrel-cod-depos            LIKE movto-estoq.cod-depos
    
    FIELD ttrel-q-ant                AS DECIMAL
    FIELD ttrel-q-ent                AS DECIMAL
    FIELD ttrel-q-sai                AS DECIMAL
    FIELD ttrel-q-atu                AS DECIMAL

    FIELD ttrel-v-ant                AS DECIMAL
    FIELD ttrel-v-ent                AS DECIMAL
    FIELD ttrel-v-sai                AS DECIMAL
    FIELD ttrel-v-atu                AS DECIMAL
    
    FIELD ttrel-it-codigo            AS CHAR

    INDEX ch-tt-relat IS PRIMARY UNIQUE  ttrel-cod-estabel
                                         ttrel-ge-codigo 
                                         ttrel-it-codigo
                                         ttrel-cod-depos. 


DEFINE TEMP-TABLE tt-relat5
    FIELD ttrel5-cod-estabel          LIKE movto-estoq.cod-estabel
    FIELD ttrel5-ge-codigo            LIKE item.ge-codigo
    FIELD ttrel5-cod-depos            LIKE movto-estoq.cod-depos
    
    FIELD ttrel5-q-ant                AS DECIMAL
    FIELD ttrel5-q-ent                AS DECIMAL
    FIELD ttrel5-q-sai                AS DECIMAL
    FIELD ttrel5-q-atu                AS DECIMAL

    FIELD ttrel5-v-ant                AS DECIMAL
    FIELD ttrel5-v-ent                AS DECIMAL
    FIELD ttrel5-v-sai                AS DECIMAL
    FIELD ttrel5-v-atu                AS DECIMAL
    
    FIELD ttrel5-it-codigo            AS CHAR

    INDEX ch-tt-relat5 IS PRIMARY UNIQUE  ttrel5-cod-estabel
                                          ttrel5-ge-codigo 
                                          ttrel5-cod-depos. 


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

def new shared var c-cod-estabel     like movto-estoq.cod-estabel  format "X(3)" initial "412" no-undo. /*solic-318*/ 
def new shared var c-ge-codigo-ini   like item.ge-codigo           format ">>9"  initial 0     no-undo.
def new shared var c-ge-codigo-fim   like item.ge-codigo           format ">>9"  initial 999   no-undo.
def new shared var c-it-codigo-ini   like item.it-codigo           format "x(16)" initial 0     no-undo.
def new shared var c-it-codigo-fim   like item.it-codigo           format "x(16)" initial "ZZZZZZZZZZZZZ"   no-undo.
def new shared var c-cod-depos-ini   like movto-estoq.cod-depos    format "x(3)" initial ""    no-undo.
def new shared var c-cod-depos-fim   like movto-estoq.cod-depos    format "x(3)" initial "ZZZ" no-undo.
def new shared var c-mes             AS INT                        format 99     initial 01    no-undo.
def new shared var c-ano             AS INT                        format 9999   initial 2004  no-undo.
def new shared var pesq-jr           AS INT                        format 9      initial 1     no-undo.


/****************** Definiáao de Vari†veis p/ Campos Virtuais do Relat¢rio *******************/ 
DEFINE VARIABLE per-ac        AS DECIMAL                                    NO-UNDO.
DEFINE VARIABLE quantidade-jr AS DECIMAL                                    NO-UNDO.
DEFINE VARIABLE ano-jr        AS INT        FORMAT 9999                     NO-UNDO.
DEFINE VARIABLE mes-jr        AS INT        FORMAT 99                       NO-UNDO.
DEFINE VARIABLE dia-jr        AS INT        FORMAT 99           INITIAL 1   NO-UNDO.
DEFINE VARIABLE dt-fech-jr    AS DATE       FORMAT 99/99/9999               NO-UNDO.
DEFINE VARIABLE dt-ini-jr     AS DATE       FORMAT 99/99/9999               NO-UNDO.
DEFINE VARIABLE tipo-jr       AS CHARACTER  FORMAT "x(10)"      INITIAL "Com G.G.F" NO-UNDO.

DEFINE VARIABLE  tipo-total    AS CHAR       NO-UNDO.
DEFINE VARIABLE  total-q-ant   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  total-v-ant   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  total-q-ent   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  total-v-ent   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  total-q-sai   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  total-v-sai   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  total-q-atu   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  total-v-atu   AS DECIMAL    NO-UNDO.

DEFINE VARIABLE  grupo-ant     AS INTEGER    NO-UNDO.
DEFINE VARIABLE  grupo-q-ant   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  grupo-v-ant   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  grupo-q-ent   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  grupo-v-ent   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  grupo-q-sai   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  grupo-v-sai   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  grupo-q-atu   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  grupo-v-atu   AS DECIMAL    NO-UNDO.

DEFINE VARIABLE  geral-q-ant   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  geral-v-ant   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  geral-q-ent   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  geral-v-ent   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  geral-q-sai   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  geral-v-sai   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  geral-q-atu   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  geral-v-atu   AS DECIMAL    NO-UNDO.


/****************** Definiáao de Vari†veis Campo do Layout do Relat¢rio **********************/ 

/****************** Definiáao de Vari†veis do Relat¢rio N∆o Pedidas em Tela ******************/ 

/****************** Definiáao de Vari†veis de Total do Relat¢rio *****************************/ 

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

/****************** Definiáao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form ttrel5-cod-estabel    column-label "Est"   format "x(3)"  at 001
     ttrel5-ge-codigo      column-label "Grupo" format ">>>>9" at 005
     ttrel5-cod-depos      column-label "Dep"   format "x(3)"  at 011

     ttrel5-q-ant          COLUMN-LABEL "Qtd.Inicial"   FORMAT "->>>,>>>,>>9.9999" at 015
     ttrel5-v-ant          COLUMN-LABEL "Vlr.Inicial"   FORMAT "->>>,>>>,>>9.9999" at 033

     ttrel5-q-ent          COLUMN-LABEL "Qtd.Entrada"   FORMAT "->>>,>>>,>>9.9999" at 051
     ttrel5-v-ent          COLUMN-LABEL "Vlr.Entrada"   FORMAT "->>>,>>>,>>9.9999" at 069

     ttrel5-q-sai          COLUMN-LABEL "Qtd.Saida  "   FORMAT "->>>,>>>,>>9.9999" at 087
     ttrel5-v-sai          COLUMN-LABEL "Vlr.Saida  "   FORMAT "->>>,>>>,>>9.9999" at 105

     ttrel5-q-atu          COLUMN-LABEL "Qtd.Final  "   FORMAT "->>>,>>>,>>9.9999" at 123
     ttrel5-v-atu          COLUMN-LABEL "Vlr.Final  "   FORMAT "->>>,>>>,>>9.9999" at 141

     with down width 160 no-box stream-io frame f-relat-09-132.


form tipo-total            format "x(11)"  at 001

     total-q-ant           FORMAT "->>>,>>>,>>9.9999" at 015
     total-v-ant           FORMAT "->>>,>>>,>>9.9999" at 033

     total-q-ent           FORMAT "->>>,>>>,>>9.9999" at 051
     total-v-ent           FORMAT "->>>,>>>,>>9.9999" at 069

     total-q-sai           FORMAT "->>>,>>>,>>9.9999" at 087
     total-v-sai           FORMAT "->>>,>>>,>>9.9999" at 105

     total-q-atu           FORMAT "->>>,>>>,>>9.9999" at 123
     total-v-atu           FORMAT "->>>,>>>,>>9.9999" at 141
     with down width 160 no-box stream-io frame f-total-09-132.

form HEADER
    fill("-", 160) format "x(160)" SKIP 
    WITH DOWN WIDTH 160 NO-BOX STREAM-IO FRAME f-relat-linha.

form HEADER
    fill(" ", 160) format "x(160)" SKIP 
    WITH DOWN WIDTH 160 NO-BOX STREAM-IO FRAME f-relat-branco.

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

assign c-programa     = "esce0010rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Resumo Movimentos de Estoque (Fechamento do Màs)"
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
    fill("-", 160) format "x(160)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    "Per°odo..:" AT 001 c-mes "  /  " c-ano   SKIP
    fill("-", 112) format "x(110)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") "Tipo: " tipo-jr skip(1)
    with stream-io width 160 no-labels no-box page-top frame f-cabec.

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

    assign c-cod-estabel     = tt-param.c-cod-estabel    
           c-ge-codigo-ini   = tt-param.c-ge-codigo-ini  
           c-ge-codigo-fim   = tt-param.c-ge-codigo-fim  
           c-it-codigo-ini   = tt-param.c-it-codigo-ini  
           c-it-codigo-fim   = tt-param.c-it-codigo-fim  
           c-cod-depos-ini   = tt-param.c-cod-depos-ini  
           c-cod-depos-fim   = tt-param.c-cod-depos-fim  
           c-mes             = tt-param.c-mes            
           c-ano             = tt-param.c-ano            
           pesq-jr           = tt-param.pesq-jr          
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
        assign v-cod-destino-impres = "Terminal".


run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

IF pesq-jr = 1 THEN
    ASSIGN tipo-jr = "Sem G.G.F".
    ELSE
    ASSIGN tipo-jr = "Com G.G.F".

FOR EACH tt-itens:
    DELETE tt-itens.
END.

FOR EACH tt-grupos:
    DELETE tt-grupos.
END.

FIND FIRST param-estoq NO-LOCK.

IF AVAIL param-estoq THEN DO:
    ASSIGN ano-jr =  INT (SUBSTRING (param-estoq.ult-per-fech,1,4)) 
           mes-jr =  INT (SUBSTRING (param-estoq.ult-per-fech,5,2)).

    ASSIGN dt-fech-jr = DATE(mes-jr,dia-jr,ano-jr)
           dt-fech-jr = dt-fech-jr + 31
           mes-jr     = INT (month(dt-fech-jr))
           ano-jr     = INT (year(dt-fech-jr))
           dt-fech-jr = DATE(mes-jr,dia-jr,ano-jr)
           dt-fech-jr = dt-fech-jr - 1
           dt-ini-jr  = DATE(c-mes,dia-jr,c-ano).

END.

   
FOR each item-estab NO-LOCK WHERE 
         item-estab.cod-estabel = c-cod-estabel AND
         item-estab.it-codigo >= c-it-codigo-ini AND
         item-estab.it-codigo <= c-it-codigo-fim.

    
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
    
    CREATE tt-itens.
    ASSIGN ttite-cod-estabel     = ITEM-estab.cod-estabel
           ttite-it-codigo       = ITEM-estab.it-codigo
           ttite-data-ult-fecham = dt-fech-jr
           ttite-v-atu-mat       = ITEM-estab.sald-ini-mat-m [1]
           ttite-v-atu-mat-ggf   = ITEM-estab.sald-ini-mat-m [1] + ITEM-estab.sald-ini-ggf-m [1].

END.

assign v-num-reg-lidos = 0.

FOR EACH ITEM FIELDS (it-codigo ge-codigo) WHERE
         item.it-codigo >= c-it-codigo-ini AND
         item.it-codigo <= c-it-codigo-fim
         NO-LOCK. 

    IF ITEM.ge-codigo < c-ge-codigo-ini OR 
       ITEM.GE-codigo > c-ge-codigo-fim THEN NEXT.

   /* FOR EACH ext-saldo-estoq NO-LOCK WHERE
        ext-saldo-estoq.it-codigo   = ITEM.it-codigo    AND
        ext-saldo-estoq.cod-estabel = c-cod-estabel     AND
        ext-saldo-estoq.log-saldo   = YES ,
    
        EACH saldo-estoq NO-LOCK WHERE
             saldo-estoq.cod-estabel  = ext-saldo-estoq.cod-estabel      AND
             saldo-estoq.cod-depos    = ext-saldo-estoq.cod-depos        AND
             saldo-estoq.cod-localiz  = ext-saldo-estoq.cod-localiz      AND
             saldo-estoq.lote         = ext-saldo-estoq.lote             AND
             saldo-estoq.it-codigo    = ext-saldo-estoq.it-codigo        AND
             saldo-estoq.cod-refer    = ext-saldo-estoq.cod-refer.       
        
     */

    FOR EACH saldo-estoq NO-LOCK WHERE
          saldo-estoq.it-codigo   = ITEM.it-codigo    AND
          saldo-estoq.cod-estabel = c-cod-estabel     AND
          saldo-estoq.cod-depos >= c-cod-depos-ini    and
          saldo-estoq.cod-depos <= c-cod-depos-fim  AND
          saldo-estoq.qtidade-atu <> 0  USE-INDEX estabel-item.  

        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
        
        FIND FIRST tt-grupos  WHERE
            ttgru-cod-estabel = saldo-estoq.cod-estabel AND
            ttgru-ge-codigo   = item.ge-codigo          AND
            ttgru-it-codigo   = saldo-estoq.it-codigo   AND
            ttgru-cod-depos   = saldo-estoq.cod-depos
            NO-ERROR.
        
        IF NOT AVAIL tt-grupos THEN DO:
            CREATE tt-grupos.
            ASSIGN ttgru-cod-estabel = saldo-estoq.cod-estabel 
                   ttgru-ge-codigo   = item.ge-codigo          
                   ttgru-it-codigo   = saldo-estoq.it-codigo   
                   ttgru-cod-depos   = saldo-estoq.cod-depos.
        END.
        
        ASSIGN ttgru-q-atu = ttgru-q-atu + saldo-estoq.qtidade-atu.
    

    END.  /* Saldo-estoq */

    FOR EACH movto-estoq NO-LOCK WHERE
        movto-estoq.it-codigo = item.it-codigo AND
        movto-estoq.dt-trans >= dt-ini-jr and
        movto-estoq.cod-estabel = c-cod-estabel
        USE-INDEX ITEM-data.
        
        IF movto-estoq.cod-depos < c-cod-depos-ini or
           movto-estoq.cod-depos > c-cod-depos-fim THEN NEXT.  
        
        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

        FIND FIRST tt-grupos  WHERE
            ttgru-cod-estabel = movto-estoq.cod-estabel AND
            ttgru-ge-codigo   = item.ge-codigo          AND
            ttgru-it-codigo   = movto-estoq.it-codigo   AND
            ttgru-cod-depos   = movto-estoq.cod-depos
            NO-ERROR.
        
        IF NOT AVAIL tt-grupos THEN DO:
            CREATE tt-grupos.
            ASSIGN ttgru-cod-estabel = movto-estoq.cod-estabel 
                   ttgru-ge-codigo   = item.ge-codigo          
                   ttgru-it-codigo   = movto-estoq.it-codigo   
                   ttgru-cod-depos   = movto-estoq.cod-depos.   
        END.

        IF movto-estoq.tipo-trans = 1 THEN
           ASSIGN ttgru-q-ent-tot = ttgru-q-ent-tot + movto-estoq.quantidade.
           ELSE
           ASSIGN ttgru-q-sai-tot = ttgru-q-sai-tot + movto-estoq.quantidade.

        ASSIGN ttgru-q-ant = ttgru-q-atu - ttgru-q-ent-tot + ttgru-q-sai-tot.

        IF INT (MONTH (movto-estoq.dt-trans)) = c-mes AND
           INT (YEAR  (movto-estoq.dt-trans)) = c-ano THEN DO: 
        
          IF movto-estoq.tipo-trans = 1 THEN
             ASSIGN ttgru-q-ent = ttgru-q-ent + movto-estoq.quantidade
                    ttgru-v-ent-mat = ttgru-v-ent-mat + movto-estoq.valor-mat-m [1]
                    ttgru-v-ent-mat-ggf = ttgru-v-ent-mat-ggf + movto-estoq.valor-mat-m [1] + movto-estoq.valor-ggf-m [1].
             ELSE
             ASSIGN ttgru-q-sai = ttgru-q-sai + movto-estoq.quantidade
                    ttgru-v-sai-mat = ttgru-v-sai-mat + movto-estoq.valor-mat-m [1]
                    ttgru-v-sai-mat-ggf = ttgru-v-sai-mat-ggf + movto-estoq.valor-mat-m [1] + movto-estoq.valor-ggf-m [1].
    
        END. /* Movimentos do Màs */

        IF movto-estoq.dt-trans <= dt-fech-jr THEN DO:

           FIND FIRST tt-itens WHERE
               ttite-cod-estabel     = movto-estoq.cod-estabel AND
               ttite-it-codigo       = movto-estoq.it-codigo 
               NO-LOCK NO-ERROR.
        
           IF NOT AVAIL tt-itens THEN DO:
        
              CREATE tt-itens.
              ASSIGN ttite-cod-estabel     = movto-estoq.cod-estabel
                     ttite-it-codigo       = movto-estoq.it-codigo.
        
           END.

           ASSIGN ttite-data-ult-fecham = dt-fech-jr.

        
          IF movto-estoq.tipo-trans = 1 THEN
             ASSIGN ttite-v-ent-mat = ttite-v-ent-mat + movto-estoq.valor-mat-m [1]
                    ttite-v-ent-mat-ggf = ttite-v-ent-mat-ggf + movto-estoq.valor-mat-m [1] + movto-estoq.valor-ggf-m [1].
             ELSE
             ASSIGN ttite-v-sai-mat = ttite-v-sai-mat + movto-estoq.valor-mat-m [1]
                    ttite-v-sai-mat-ggf = ttite-v-sai-mat-ggf + movto-estoq.valor-mat-m [1] + movto-estoq.valor-ggf-m [1].
    
          ASSIGN ttite-v-ant-mat = ttite-v-atu-mat - ttite-v-ent-mat + ttite-v-sai-mat
                 ttite-v-ant-mat-ggf = ttite-v-atu-mat-ggf - ttite-v-ent-mat-ggf + ttite-v-sai-mat-ggf.

        END. /* valores atÇ o ultimo fechamento */
   
    END.  /* movto-estoq */

END. /* item */

assign v-num-reg-lidos = 0.

FOR EACH tt-grupos NO-LOCK.
    
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    ASSIGN ttgru-q-ant = ttgru-q-atu - ttgru-q-ent-tot + ttgru-q-sai-tot.
    
    FIND FIRST tt-itens WHERE
        ttite-cod-estabel     = ttgru-cod-estabel AND
        ttite-it-codigo       = ttgru-it-codigo 
        NO-LOCK NO-ERROR.
    
    IF NOT AVAIL tt-itens THEN DO:
    
       CREATE tt-itens.
       ASSIGN ttite-cod-estabel     = ttgru-cod-estabel
              ttite-it-codigo       = ttgru-it-codigo.
    
    END.

    ASSIGN ttite-v-ant-mat = ttite-v-atu-mat - ttite-v-ent-mat + ttite-v-sai-mat
           ttite-v-ant-mat-ggf = ttite-v-atu-mat-ggf - ttite-v-ent-mat-ggf + ttite-v-sai-mat-ggf.

    ASSIGN ttite-data-ult-fecham = dt-fech-jr
           ttite-q-ant = ttite-q-ant + ttgru-q-ant.
    
    IF ttite-q-ant > 0 AND ttite-v-ant-mat > 0 THEN
           ttite-medio-mat = ttite-v-ant-mat / ttite-q-ant.
    ELSE
      ASSIGN ttite-medio-mat = 0.

    IF ttite-q-ant > 0 AND ttite-v-ant-mat-ggf > 0 THEN  
           ttite-medio-mat-ggf = ttite-v-ant-mat-ggf / ttite-q-ant.  
    ELSE
      ASSIGN ttite-medio-mat-ggf = 0.

END.

FOR EACH tt-grupos NO-LOCK :
    
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    FIND FIRST tt-itens WHERE
        ttite-cod-estabel     = ttgru-cod-estabel AND
        ttite-it-codigo       = ttgru-it-codigo 
        NO-LOCK NO-ERROR.
    
    IF NOT AVAIL tt-itens THEN next.  

    FIND FIRST tt-relat WHERE
        ttrel-cod-estabel     = ttgru-cod-estabel AND
        ttrel-it-codigo       = ttgru-it-codigo   AND
        ttrel-ge-codigo       = ttgru-ge-codigo   AND
        ttrel-cod-depos       = ttgru-cod-depos
        NO-ERROR.
    
    IF NOT AVAIL tt-relat THEN do:
        CREATE tt-relat.
        ASSIGN ttrel-cod-estabel     = ttgru-cod-estabel
               ttrel-it-codigo       = ttgru-it-codigo   
               ttrel-ge-codigo       = ttgru-ge-codigo 
               ttrel-cod-depos       = ttgru-cod-depos.  
    END.

    ASSIGN ttrel-q-ant = ttrel-q-ant + ttgru-q-ant
           ttrel-q-atu = ttrel-q-ant + ttgru-q-ent - ttgru-q-sai
           ttrel-q-ent = ttrel-q-ent + ttgru-q-ent
           ttrel-q-sai = ttrel-q-sai + ttgru-q-sai.

    IF pesq-jr = 1 THEN
        ASSIGN ttrel-v-ant = ttrel-v-ant + (ttrel-q-ant * ttite-medio-mat)
               ttrel-v-ent = ttrel-v-ent + ttgru-v-ent-mat
               ttrel-v-sai = ttrel-v-sai + ttgru-v-sai-mat.
    ELSE
        ASSIGN ttrel-v-ant = ttrel-v-ant + (ttrel-q-ant * ttite-medio-mat-ggf)
               ttrel-v-ent = ttrel-v-ent + ttgru-v-ent-mat-ggf
               ttrel-v-sai = ttrel-v-sai + ttgru-v-sai-mat-ggf.

    ASSIGN ttrel-v-atu = ttrel-v-ant + ttrel-v-ent - ttrel-v-sai.

END.  

FOR EACH tt-relat.

    FIND FIRST tt-relat5 WHERE
        ttrel5-cod-estabel     = ttrel-cod-estabel AND
        ttrel5-ge-codigo       = ttrel-ge-codigo   AND
        ttrel5-cod-depos       = ttrel-cod-depos
        NO-ERROR.
    
    IF NOT AVAIL tt-relat5 THEN do:
        CREATE tt-relat5.
        ASSIGN ttrel5-cod-estabel     = ttrel-cod-estabel
               ttrel5-ge-codigo       = ttrel-ge-codigo 
               ttrel5-cod-depos       = ttrel-cod-depos.  
    END.

    ASSIGN ttrel5-q-ant = ttrel5-q-ant + ttrel-q-ant
           ttrel5-q-atu = ttrel5-q-atu + ttrel-q-atu
           ttrel5-q-ent = ttrel5-q-ent + ttrel-q-ent
           ttrel5-q-sai = ttrel5-q-sai + ttrel-q-sai.

   ASSIGN ttrel5-v-ant = ttrel5-v-ant + ttrel-v-ant
          ttrel5-v-ent = ttrel5-v-ent + ttrel-v-ent
          ttrel5-v-sai = ttrel5-v-sai + ttrel-v-sai
          ttrel5-v-atu = ttrel5-v-atu + ttrel-v-atu.

END.  


    /***  C‡DIGO PARA SA÷DA EM 132 COLUNAS ***/

FOR EACH tt-relat5.

    ASSIGN ttrel5-q-atu = ttrel5-q-ant + ttrel5-q-ent - ttrel5-q-sai.

        IF ttrel5-ge-codigo <> grupo-ant AND grupo-ant <> 0 THEN 
           RUN TOTAL-grupo.

        ASSIGN grupo-ant = ttrel5-ge-codigo.      

        ASSIGN grupo-q-ant = grupo-q-ant + ttrel5-q-ant    
               grupo-v-ant = grupo-v-ant + ttrel5-v-ant    
               grupo-q-ent = grupo-q-ent + ttrel5-q-ent    
               grupo-v-ent = grupo-v-ent + ttrel5-v-ent    
               grupo-q-sai = grupo-q-sai + ttrel5-q-sai    
               grupo-v-sai = grupo-v-sai + ttrel5-v-sai    
               grupo-q-atu = grupo-q-atu + ttrel5-q-atu    
               grupo-v-atu = grupo-v-atu + ttrel5-v-atu. 
        
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp 
            ttrel5-cod-estabel 
            ttrel5-ge-codigo   
            ttrel5-cod-depos   
            ttrel5-q-ant       
            ttrel5-v-ant       
            ttrel5-q-ent       
            ttrel5-v-ent       
            ttrel5-q-sai       
            ttrel5-v-sai       
            ttrel5-q-atu       
            ttrel5-v-atu   
            with stream-io frame f-relat-09-132.
            down stream str-rp with frame f-relat-09-132.

END.

    RUN TOTAL-grupo.
    

    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp 
    with stream-io frame f-relat-linha.
    down stream str-rp with frame f-relat-linha.

    ASSIGN tipo-total = "TOT GERAL:". 

    ASSIGN total-q-ant = geral-q-ant  
           total-v-ant = geral-v-ant  
           total-q-ent = geral-q-ent  
           total-v-ent = geral-v-ent  
           total-q-sai = geral-q-sai  
           total-v-sai = geral-v-sai  
           total-q-atu = geral-q-atu  
           total-v-atu = geral-v-atu.   

        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp 
            tipo-total     NO-LABEL
            total-q-ant    NO-LABEL   
            total-v-ant    NO-LABEL   
            total-q-ent    NO-LABEL   
            total-v-ent    NO-LABEL   
            total-q-sai    NO-LABEL   
            total-v-sai    NO-LABEL   
            total-q-atu    NO-LABEL   
            total-v-atu    NO-LABEL   
            with stream-io frame f-total-09-132.
            down stream str-rp with frame f-total-09-132.   


    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp 
    with stream-io frame f-relat-linha.
    down stream str-rp with frame f-relat-linha.


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


   disp stream str-rp "SELEÄ«O" skip(01) with stream-io frame f-imp-sel.
   disp stream str-rp 
      c-cod-estabel   colon 20 "|< >|"   at 44  no-label
        with stream-io side-labels overlay row 032 frame f-imp-sel.

   disp stream str-rp "CLASSIFICAÄ«O" skip(01) with stream-io frame f-imp-cla.
   disp stream str-rp "   Resumo Movimentos de Estoque (Fechamento do Màs)"
        with stream-io side-labels overlay row 032 frame f-imp-cla.

   put stream str-rp unformatted skip(1) "IMPRESS«O" skip(1).

   put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
   put stream str-rp unformatted skip "    " "Execuá∆o: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
   put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
   put stream str-rp unformatted skip "    " "Usu†rio : " tt-param.usuario.

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

PROCEDURE TOTAL-grupo.
    
    ASSIGN tipo-total = "TOT GRUPO:".
        
    ASSIGN total-q-ant = grupo-q-ant  
           total-v-ant = grupo-v-ant  
           total-q-ent = grupo-q-ent  
           total-v-ent = grupo-v-ent  
           total-q-sai = grupo-q-sai  
           total-v-sai = grupo-v-sai  
           total-q-atu = grupo-q-atu  
           total-v-atu = grupo-v-atu.                 

    PUT STREAM str-rp " " AT 01.

        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp 
            tipo-total     NO-LABEL
            total-q-ant    NO-LABEL   
            total-v-ant    NO-LABEL   
            total-q-ent    NO-LABEL   
            total-v-ent    NO-LABEL   
            total-q-sai    NO-LABEL   
            total-v-sai    NO-LABEL   
            total-q-atu    NO-LABEL   
            total-v-atu    NO-LABEL   
            with stream-io frame f-total-09-132.
            down stream str-rp with frame f-total-09-132.   

    PUT STREAM str-rp " " AT 01.
    
    ASSIGN geral-q-ant = geral-q-ant + grupo-q-ant  
           geral-v-ant = geral-v-ant + grupo-v-ant  
           geral-q-ent = geral-q-ent + grupo-q-ent  
           geral-v-ent = geral-v-ent + grupo-v-ent  
           geral-q-sai = geral-q-sai + grupo-q-sai  
           geral-v-sai = geral-v-sai + grupo-v-sai  
           geral-q-atu = geral-q-atu + grupo-q-atu  
           geral-v-atu = geral-v-atu + grupo-v-atu. 

    ASSIGN grupo-q-ant = 0
           grupo-v-ant = 0
           grupo-q-ent = 0
           grupo-v-ent = 0
           grupo-q-sai = 0
           grupo-v-sai = 0
           grupo-q-atu = 0
           grupo-v-atu = 0.

END PROCEDURE.

return 'OK'.

/* fim do programa */
