/*****************************************************************************
**
**       Programa: escq0002rp.p
**
**       Data....: 22/02/2005
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: RASTREABILIDADE - LAUDO DE QUALIDADE
**
**       Vers∆o..: 1.00.000 - jrrcampos
**
**       OBS.....: 
**
*******************************************************************************/
def buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "escq0002RP".

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
    field c-cod-estabel-fim    like ped-venda.cod-estabel
    field i-nr-pedido-ini      like ped-venda.nr-pedido
    field i-nr-sequencia-ini   like ped-item.nr-sequencia
    field c-nr-pallet-ini      like pallet.nr-pallet
    field c-nr-pallet-fim      like pallet.nr-pallet
    FIELD pesq-jr              AS INT  
.


DEFINE TEMP-TABLE tt-bobinas
    FIELD ttbob-lote           AS CHAR 
    FIELD ttbob-linha          AS integer
    FIELD ttbob-it-codigo      AS CHAR
    FIELD ttbob-nr-ord-produ   AS INTEGER
    FIELD ttbob-dt-trans       AS DATE
    INDEX ch-tt-bobinas IS PRIMARY UNIQUE   ttbob-lote
                                            ttbob-linha. 
                                            
DEFINE TEMP-TABLE tt-bobinas2
    FIELD ttbob2-lote           AS CHAR 
    FIELD ttbob2-linha          AS integer
    FIELD ttbob2-it-codigo      AS CHAR
    FIELD ttbob2-nr-ord-produ   AS INTEGER
    FIELD ttbob2-dt-trans       AS DATE
    FIELD ttbob2-nr-ficha       AS INT
    INDEX ch-tt-bobinas2 IS PRIMARY UNIQUE   ttbob2-lote
                                             ttbob2-linha. 


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
    INDEX ch-tt-analises IS PRIMARY UNIQUE  ttana-cod-exame
                                            ttana-cod-comp.


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

def new shared var c-cod-estabel-ini  like ped-venda.cod-estabel format "x(3)" initial "" no-undo.
def new shared var c-cod-estabel-fim  like ped-venda.cod-estabel format "x(3)" initial "ZZZ" no-undo.
def new shared var i-nr-pedido-ini    like ped-venda.nr-pedido format ">>>>>>>>9" initial 0 no-undo.
def new shared var c-nr-pallet-ini    like pallet.nr-pallet format "x(12)" initial "" no-undo.
def new shared var c-nr-pallet-fim    like pallet.nr-pallet format "x(12)" initial "ZZZZZZZZZZZZ" no-undo.
def new shared var i-nr-sequencia-ini like ped-item.nr-sequencia format ">>>>9" initial 0 no-undo.

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
DEFINE VARIABLE diex-jr1      AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE espes-jr      AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE gramat-jr     AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE comp-jr1      AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE peso-jr1      AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE densidade-jr  AS DECIMAL                   NO-UNDO.
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
DEFINE VARIABLE tipo-result   AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE descricao-comp AS CHARACTER                NO-UNDO.
DEFINE VARIABLE minimo-jr     AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE maximo-jr     AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE RESULT-jr     AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE media-result  AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE esp-min-jr    AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE esp-max-jr    AS DECIMAL                   NO-UNDO.


/****************** Definiáao de Vari†veis Campo do Layout do Relat¢rio **********************/ 

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

FORM ttbob2-lote            COLUMN-LABEL "Nr.Bobina"     FORMAT "x(15)" at 001
     ttbob2-linha           COLUMN-LABEL "Linha"         FORMAT ">>>>9" at 020
     ttbob2-it-codigo       COLUMN-LABEL "Produto"       FORMAT "x(16)" AT 030
     ttbob2-nr-ord-produ    COLUMN-LABEL "Ord.Prod."     FORMAT ">>>>>>>>9" AT 050
     ttbob2-dt-trans        COLUMN-LABEL "Dt.Prod."      FORMAT "99/99/9999" AT 063
     ttbob2-nr-ficha        COLUMN-LABEL "Nr.Roteiro"    FORMAT ">>>>>>>>>9" AT 077
     espes-jr               COLUMN-LABEL "Espes.Calc"    FORMAT ">>>>>9.999" at 089
     gramat-jr              COLUMN-LABEL "Gramat.Calc"   FORMAT ">>>>>9.999" at 101

     with down width 132 no-box stream-io frame f-relat-09-132.

form tp-pedido-jr           COLUMN-LABEL "  "      FORMAT "x(2)"      AT 001
     i-nr-pedido-ini        column-label "Pedido"  format ">>>>>>>>9" at 003
     i-nr-sequencia-ini     column-label "Seq.Pd." format ">>>>9"     at 013
     nome-abrev-jr          column-label "Cliente" format "x(12)"     at 022
     it-codigo-ped          COLUMN-LABEL "Produto" FORMAT "x(15)"     AT 037
     diin-ped               COLUMN-LABEL "D.Int"   FORMAT ">>>>9"     AT 057
     diex-ped               COLUMN-LABEL "D.Ext"   FORMAT ">>>>9"     AT 067
     larg-ped               COLUMN-LABEL "Larg"    FORMAT ">>>>9"     AT 077
     c-nr-pallet-ini        COLUMN-LABEL "Palete Inic" FORMAT "x(15)" AT 087
     c-nr-pallet-fim        COLUMN-LABEL "Palete Final" FORMAT "x(15)" AT 107
     with down width 132 no-box stream-io frame f-relat-lin-ped.

form esp-min         COLUMN-LABEL "Esps.M°nima"  FORMAT ">>>>>>>9.99"  AT 001
     esp-max         COLUMN-LABEL "Esps.M†xima"  FORMAT ">>>>>>>9.99"  AT 020
     esp-med         COLUMN-LABEL "Esps.MÇdia "  FORMAT ">>>>>>>9.99"  AT 040
     with down width 132 no-box stream-io frame f-relat-lin-esp.

form lar-min         COLUMN-LABEL "Larg.M°nima"  FORMAT ">>>>>>>>>99"  AT 001
     lar-max         COLUMN-LABEL "Larg.M†xima"  FORMAT ">>>>>>>>>99"  AT 020
     lar-med         COLUMN-LABEL "Larg.MÇdia "  FORMAT ">>>>>>>>>99"  AT 040
     with down width 132 no-box stream-io frame f-relat-lin-lar.

form die-min         COLUMN-LABEL "Diex.M°nima"  FORMAT ">>>>>>>>>99"  AT 001
     die-max         COLUMN-LABEL "Diex.M†xima"  FORMAT ">>>>>>>>>99"  AT 020
     die-med         COLUMN-LABEL "Diex.MÇdia "  FORMAT ">>>>>>>>>99"  AT 040
     with down width 132 no-box stream-io frame f-relat-lin-die.


form HEADER
    fill("-", 132) format "x(132)" SKIP 
    WITH DOWN WIDTH 132 NO-BOX STREAM-IO FRAME f-relat-linha.

form HEADER
    fill(" ", 132) format "x(132)" SKIP 
    WITH DOWN WIDTH 132 NO-BOX STREAM-IO FRAME f-relat-branco.


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

assign c-programa     = "escq0002rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "RASTREABILIDADE - LAUDO DE QUALIDADE"
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

    assign c-cod-estabel-ini  = tt-param.c-cod-estabel-ini
           c-cod-estabel-fim  = tt-param.c-cod-estabel-fim
           c-cod-estabel-fim  = tt-param.c-cod-estabel-fim
           i-nr-pedido-ini    = tt-param.i-nr-pedido-ini
           c-nr-pallet-ini    = tt-param.c-nr-pallet-ini
           c-nr-pallet-fim    = tt-param.c-nr-pallet-fim
           i-nr-sequencia-ini = tt-param.i-nr-sequencia-ini
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
        assign v-cod-destino-impres = "Terminal".


run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

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
       qtd-die  = 0.

FOR EACH tt-bobinas NO-LOCK:
    DELETE tt-bobinas.
END.

for each ped-venda no-lock
         where ped-venda.nr-pedido   = i-nr-pedido-ini , 

    Each ped-item OF ped-venda No-lock Where
         ped-item.cod-sit-item < 3 AND
         ped-item.nr-sequencia = i-nr-sequencia-ini ,
    
             EACH pallet NO-LOCK WHERE
                  pallet.it-codigo = ped-item.it-codigo AND 
                  pallet.nr-pedido = ped-venda.nr-pedido AND
                  pallet.nr-sequencia = ped-item.nr-sequencia
                  USE-INDEX pedido ,

                  EACH it-pallet OF pallet NO-LOCK :
    

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


    ASSIGN larg-jr1 = 0.
    ASSIGN diex-jr1 = 0.
    ASSIGN espes-jr1 = 0.

    IF movto-estoq.cod-estabel = "423" THEN DO:
    
       FIND lote-carac-tec WHERE
                 lote-carac-tec.it-codigo = movto-estoq.it-codigo AND
                 lote-carac-tec.lote = movto-estoq.lote AND
                 lote-carac-tec.cd-comp = "lgreal" 
                 NO-LOCK NO-ERROR.
         
       IF AVAIL lote-carac-tec AND lote-carac-tec.vl-resul > 0 THEN
          ASSIGN larg-jr1 = lote-carac-tec.vl-resul.
       
       FIND lote-carac-tec WHERE
                 lote-carac-tec.it-codigo = movto-estoq.it-codigo AND
                 lote-carac-tec.lote = movto-estoq.lote AND
                 lote-carac-tec.cd-comp = "dereal" 
                 NO-LOCK NO-ERROR.
         
       IF AVAIL lote-carac-tec AND lote-carac-tec.vl-resul > 0 THEN
          ASSIGN diex-jr1 = lote-carac-tec.vl-resul.
    
    END.

    IF movto-estoq.cod-estabel <> "423" THEN DO:
    
       FIND lote-carac-tec WHERE
                 lote-carac-tec.it-codigo = movto-estoq.it-codigo AND
                 lote-carac-tec.lote = movto-estoq.lote AND
                 lote-carac-tec.cd-comp = "larg" 
                 NO-LOCK NO-ERROR.
         
       IF AVAIL lote-carac-tec AND lote-carac-tec.vl-resul > 0 THEN
          ASSIGN larg-jr1 = lote-carac-tec.vl-resul.
       
       FIND lote-carac-tec WHERE
                 lote-carac-tec.it-codigo = movto-estoq.it-codigo AND
                 lote-carac-tec.lote = movto-estoq.lote AND
                 lote-carac-tec.cd-comp = "diex" 
                 NO-LOCK NO-ERROR.
         
       IF AVAIL lote-carac-tec AND lote-carac-tec.vl-resul > 0 THEN
          ASSIGN diex-jr1 = lote-carac-tec.vl-resul.
    
    END.

    FIND lote-carac-tec WHERE
                 lote-carac-tec.it-codigo = movto-estoq.it-codigo AND
                 lote-carac-tec.lote = movto-estoq.lote AND
                 lote-carac-tec.cd-comp = "espes" 
                 NO-LOCK NO-ERROR.
         
    IF AVAIL lote-carac-tec AND lote-carac-tec.vl-resul > 0 THEN
       ASSIGN espes-jr1 = lote-carac-tec.vl-resul.
     
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

IF v-num-reg-lidos > 0 THEN DO:
    
    FIND FIRST ped-venda WHERE 
         ped-venda.nr-pedido = i-nr-pedido-ini 
         NO-LOCK NO-ERROR.

    IF AVAIL ped-venda THEN DO:
        FIND FIRST ped-item OF ped-venda WHERE 
             ped-item.ind-componen  < 3           AND
             ped-item.nr-sequencia = i-nr-sequencia-ini 
             USE-INDEX ch-item-ped
             NO-LOCK NO-ERROR.
    
    IF AVAIL ped-item  THEN DO:

       ASSIGN it-codigo-ped = ped-item.it-codigo.
       
       FIND FIRST cot-est-mast
        WHERE cot-est-mast.item-cotacao = ped-item.it-codigo 
        AND cot-est-mast.nr-estrut      = ped-item.nr-config
        NO-LOCK NO-ERROR.

       IF AVAIL cot-est-mast THEN DO:
         
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
    END.
  END.

     view stream str-rp frame f-cabec.
     view stream str-rp frame f-rodape.
     assign l-imprime = yes.
     display stream str-rp
        tp-pedido-jr
        i-nr-pedido-ini
        i-nr-sequencia-ini
        nome-abrev-jr
        it-codigo-ped
        diin-ped
        diex-ped
        larg-ped
        c-nr-pallet-ini
        c-nr-pallet-fim
            
     with stream-io frame f-relat-lin-ped.
     down stream str-rp with frame f-relat-lin-ped.

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

     FOR EACH tt-analises NO-LOCK:
         DELETE tt-analises.
     END.

     FOR EACH tt-bobinas2 :

/* Rotina para C†lculo de Espessura e Gramatura em Mill Rolls  */

     ASSIGN espes-jr  = 0
            gramat-jr = 0.
     
     IF c-cod-estabel-ini = "423" THEN DO: 

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
        
        FIND FIRST movto-mat 
         where movto-mat.esp-docto = 1 AND
               movto-mat.it-codigo = ttbob2-it-codigo and
               movto-mat.lote      = ttbob2-lote AND
               movto-mat.cod-depos <> "arc"
               USE-INDEX lote
               NO-LOCK NO-ERROR.

        IF AVAIL movto-mat THEN do: 
                        
           ASSIGN peso-jr1 = movto-mat.quantidade.

           IF larg-jr1 <> 0 and comp-jr1 <> 0 and peso-jr1 <> 0 THEN do:

           ASSIGN gramat-jr = (peso-jr1 / (comp-jr1 * larg-jr1)) * 1000000 .
           
           find first item where
                item.it-codigo = ttbob2-it-codigo
                no-lock no-error.
                
           if avail item then do:

             IF item.fm-codigo = "BSA" OR item.fm-codigo = "BST" OR 
                item.fm-codigo = "BPX" THEN                             
                 ASSIGN densidade-jr = 0.913.                         
                  ELSE                                                
                   ASSIGN densidade-jr = 0.905.

        ASSIGN espes-jr = (gramat-jr / densidade-jr).
                

       END.
      END.
     END.
    end.
   end.  
     
/*-----------------------------------------------------------------*/

         FIND FIRST ficha-cq WHERE
             ficha-cq.it-codigo = ttbob2-it-codigo AND
             ficha-cq.lote = ttbob2-lote
             USE-INDEX it-lote
             NO-LOCK NO-ERROR.

         IF AVAIL ficha-cq THEN
             ttbob2-nr-ficha = ficha-cq.nr-ficha.
       
     
     
    /***  C‡DIGO PARA SA÷DA EM 132 COLUNAS ***/

        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp
            ttbob2-lote
            ttbob2-linha
            ttbob2-it-codigo
            ttbob2-nr-ord-produ
            ttbob2-dt-trans
            ttbob2-nr-ficha
            espes-jr
            gramat-jr
            with stream-io frame f-relat-09-132.
            down stream str-rp with frame f-relat-09-132.  
   
   end.
END. 

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
      c-cod-estabel-ini colon 23 "|< >|"   at 47 c-cod-estabel-fim no-label
      i-nr-pedido-ini colon 23 "|< >|"   
      c-nr-pallet-ini colon 23 "|< >|"   at 47 c-nr-pallet-fim no-label
        with stream-io side-labels overlay row 034 frame f-imp-sel.

   disp stream str-rp "CLASSIFICAÄ«O" skip(01) with stream-io frame f-imp-cla.
   disp stream str-rp "   RASTREABILIDADE - LAUDO DE QUALIDADE"
        with stream-io side-labels overlay row 034 frame f-imp-cla.

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
          ASSIGN ttbob-lote = lote-rast
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



return 'OK'.

/* fim do programa */
