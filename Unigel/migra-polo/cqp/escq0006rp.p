/*****************************************************************************
**
**       Programa: escq0006rp.p
**
**       Data....: 30/07/2009
**
**       Autor...: Jos‚ Roberto
**
**       Objetivo: RASTREABILIDADE DE PEDIDOS C/DEFEITOS DE QUALIDADE
**
**       OBS.....: 
**
*******************************************************************************/
 def buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "escq0006rp".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

/****************** Defini‡Æo de Tabelas Tempor rias do Relat¢rio **********************/

def temp-table tt-raw-digita
    field raw-digita as raw.

/* Tabela Pedidos */
def NEW global SHARED temp-table tt-pedidos-06 
    field pedido       AS INTEGER
    FIELD nr-sequencia AS INT
    INDEX ch-tt-pedidos-06 IS PRIMARY UNIQUE pedido
                                             nr-sequencia.

def new global shared var tt-TipPed AS CHAR FORMAT "X(5)" NO-UNDO.


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
    field c-cod-estabel-ini like ped-venda.cod-estabel.
.


DEFINE TEMP-TABLE tt-bobinas
    FIELD ttbob-nr-pedido      AS INT
    FIELD ttbob-nr-sequencia   AS INT
    FIELD ttbob-nr-pallet      AS CHAR
    FIELD ttbob-lote-bobina    AS CHAR
    FIELD ttbob-lote           AS CHAR 
    FIELD ttbob-linha          AS integer
    FIELD ttbob-it-codigo      AS CHAR
    FIELD ttbob-nr-ord-produ   AS INTEGER
    FIELD ttbob-dt-trans       AS DATE
    FIELD ttbob-oper           AS char
    INDEX ch-tt-bobinas IS PRIMARY UNIQUE  ttbob-nr-pedido    
                                           ttbob-nr-sequencia 
                                           ttbob-nr-pallet
                                           ttbob-lote-bobina
                                           ttbob-linha 
                                           ttbob-lote.

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



/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

def new shared var c-cod-estabel-ini like ped-venda.cod-estabel format "x(3)"  no-undo.  /*solic-318*/ 


/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 
DEFINE VARIABLE pesq-jr       AS INTEGER                   NO-UNDO.
DEFINE VARIABLE op-rast       AS INTEGER                   NO-UNDO.
DEFINE VARIABLE lote-rast     AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE lote-rast-jr  AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE data-rast     AS DATE                      NO-UNDO.
DEFINE VARIABLE oper-rast     AS CHAR                      NO-UNDO.
DEFINE VARIABLE diin-ped      AS INTEGER                   NO-UNDO.
DEFINE VARIABLE diex-ped      AS INTEGER                   NO-UNDO.
DEFINE VARIABLE larg-ped      AS INTEGER                   NO-UNDO.
DEFINE VARIABLE turma-jr      AS CHAR                      NO-UNDO.
DEFINE VARIABLE maq-jr        AS INTEGER                   NO-UNDO.
DEFINE VARIABLE linha-jr      AS INTEGER                   NO-UNDO.
DEFINE VARIABLE durezae-jr    AS INTEGER                   NO-UNDO.
DEFINE VARIABLE durezac-jr    AS INTEGER                   NO-UNDO.
DEFINE VARIABLE durezad-jr    AS INTEGER                   NO-UNDO.
DEFINE VARIABLE DESC-def1     AS CHARACTER FORMAT "X(20)"  NO-UNDO.
DEFINE VARIABLE DESC-def2     AS CHARACTER FORMAT "X(20)"  NO-UNDO.

DEFINE VARIABLE lote-jr       AS CHARACTER FORMAT "x(10)"  NO-UNDO.
DEFINE VARIABLE nome-op-jr    AS CHARACTER FORMAT "x(12)"  NO-UNDO.


/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/* Vari veis usadas para gerar planilha excel. */

DEF VAR c-arq             AS CHAR  FORMAT "x(50)"  NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR  FORMAT "x(50)"  NO-UNDO.   
DEF VAR l-cria            AS LOG                 NO-UNDO.
DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
DEF VAR i-linha           AS INT                 NO-UNDO.

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

def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.

/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 

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

assign c-programa     = "escq0006rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "RASTREABILIDADE DE PEDIDOS C/DEFEITOS DE QUALIDADE"
       c-sistema      = "ce".

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

    assign c-cod-estabel-ini   = tt-param.c-cod-estabel-ini         
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

IF tt-param.destino = 4 THEN DO:

               /* Cria Aplica‡Æo do Excel */
 
   CREATE "Excel.Application" c-excel.
   ASSIGN c-excel:DisplayAlerts = FALSE.
   ASSIGN c-modelo-planilha = search("modelos/mod-escq0006.xls") 
          c-arq             = SESSION:TEMP-DIRECTORY.

   RUN pi-cria-planilha.  

END. 

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

ASSIGN c-relatorio:range("B" + string("3")):VALUE = c-cod-estabel-ini.

ASSIGN pesq-jr = 1
       i-linha = 6.

FOR EACH tt-pedidos-06:

    FOR EACH tt-bobinas NO-LOCK:
        DELETE tt-bobinas.
    END.
    
    for each ped-venda no-lock
             where ped-venda.nr-pedido = tt-pedidos-06.pedido , 
    
        Each ped-item OF ped-venda No-lock Where
             ped-item.nr-sequencia = tt-pedidos-06.nr-sequencia AND
             ped-item.ind-componen < 3,
        
             EACH pallet NO-LOCK WHERE
                      pallet.it-codigo = ped-item.it-codigo AND 
                      pallet.nr-pedido = ped-venda.nr-pedido AND
                      pallet.nr-sequencia = ped-item.nr-sequencia ,
    
                 EACH it-pallet OF pallet NO-LOCK :
         
        FIND LAST movto-mat 
             where movto-mat.it-codigo = ped-item.it-codigo and
                   movto-mat.lote      = it-pallet.lote-bobina  AND
                   movto-mat.esp-docto = 1 
                   USE-INDEX lote
                   NO-LOCK NO-ERROR.
        
         IF NOT AVAIL movto-mat THEN next.   

        
         FIND LAST movto-estoq 
              where movto-estoq.nr-reporte = movto-mat.nr-reporte  and
                    movto-estoq.esp-docto  = 1 
              use-index nr-reporte      
              NO-LOCK NO-ERROR.
        
         IF NOT AVAIL movto-estoq THEN next.
        
        
        ASSIGN lote-rast = movto-estoq.lote
               op-rast   = movto-estoq.nr-ord-produ
               data-rast = movto-estoq.dt-trans
               oper-rast = movto-estoq.usuario.
               

               
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
    
           FIND LAST movto-estoq WHERE
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
                     data-rast = movto-estoq.dt-trans
                     oper-rast = movto-estoq.usuario.
    
              RUN grava-rastreabilidade.
    
           END.

         END.
         
        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
    
    END.  /* for each ped-venda */

    FOR EACH tt-bobinas NO-LOCK.

        FIND FIRST ped-venda WHERE
            ped-venda.nr-pedido = tt-bobinas.ttbob-nr-pedido
            NO-LOCK NO-ERROR.

        IF NOT AVAIL ped-venda THEN NEXT.

        FIND FIRST ped-item OF ped-venda WHERE 
             ped-item.nr-sequen  = ttbob-nr-sequencia   and
             ped-item.ind-componen < 3 
             NO-LOCK NO-ERROR.

        IF NOT AVAIL ped-item THEN NEXT.
       
         FIND FIRST cot-est-mast
          WHERE cot-est-mast.item-cotacao = ped-item.it-codigo 
          AND cot-est-mast.nr-estrut    = ped-item.nr-config
          NO-LOCK NO-ERROR.
        
         IF AVAIL cot-est-mast THEN DO:
           
            FIND var-result 
                WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                    AND var-result.nr-estrut  = cot-est-mast.nr-estrut
                    AND var-result.nome-var   = "largura" NO-LOCK NO-ERROR.
            
            IF AVAIL var-result THEN
                ASSIGN  larg-ped = var-result.valor-dec.
            
            FIND var-result 
                WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                    AND var-result.nr-estrut  = cot-est-mast.nr-estrut
                    AND var-result.nome-var   = "diin" NO-LOCK NO-ERROR.
            
            IF AVAIL var-result THEN
                ASSIGN  diin-ped = var-result.valor-dec.
            
            FIND var-result 
                WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                    AND var-result.nr-estrut  = cot-est-mast.nr-estrut
                    AND var-result.nome-var   = "diex" NO-LOCK NO-ERROR.
            
            IF AVAIL var-result THEN
                ASSIGN  diex-ped = var-result.valor-dec.
         
         END.

         IF ttbob-lote = "" AND i-linha <> 6  THEN
             ASSIGN i-linha = i-linha + 1
                    c-relatorio:range("A" + string(i-linha)):VALUE = "-------------".

         ASSIGN i-linha = i-linha + 1
                c-relatorio:range("A" + string(i-linha)):VALUE = ped-venda.nome-abrev
                c-relatorio:range("B" + STRING(i-linha)):VALUE = string(string(ped-venda.nr-pedido) + "-" + STRING(ped-item.nr-sequencia))  
                c-relatorio:range("C" + STRING(i-linha)):VALUE = ped-item.it-codigo
                c-relatorio:range("D" + STRING(i-linha)):VALUE = ttbob-nr-pallet
                c-relatorio:range("E" + STRING(i-linha)):VALUE = ttbob-lote-bobina
                c-relatorio:range("F" + string(i-linha)):VALUE = IF (ttbob-lote <> ttbob-lote-bobina) THEN ttbob-lote ELSE ""
                c-relatorio:range("G" + STRING(i-linha)):VALUE = ttbob-nr-ord-produ
                c-relatorio:range("H" + STRING(i-linha)):VALUE = ttbob-dt-trans.
         ASSIGN turma-jr = ""
                durezae-jr  = 0
                durezac-jr  = 0
                durezad-jr  = 0
                desc-def1   = ""
                desc-def2   = ""
                maq-jr      = 0.

         ASSIGN lote-rast-jr = ttbob-lote-bobina.
         IF ttbob-lote <> ""  THEN ASSIGN lote-rast-jr = ttbob-lote.
         
         FIND FIRST lote-carac-tec WHERE
                lote-carac-tec.it-codigo   = ttbob-it-codigo
                and lote-carac-tec.lote    = lote-rast-jr
                and lote-carac-tec.cd-comp = "maq"
                NO-LOCK NO-ERROR.
        
         if avail lote-carac-tec then 
             ASSIGN maq-jr = lote-carac-tec.vl-result.
        
         FIND FIRST lote-carac-tec WHERE
                lote-carac-tec.it-codigo   = ttbob-it-codigo
                and lote-carac-tec.lote    = lote-rast-jr
                and lote-carac-tec.cd-comp = "turma"
                NO-LOCK NO-ERROR.
        
         if avail lote-carac-tec then 
             ASSIGN turma-jr = lote-carac-tec.observacao.
        
         FIND FIRST lote-carac-tec WHERE
                          lote-carac-tec.it-codigo = ttbob-it-codigo
                          and lote-carac-tec.lote = lote-rast-jr
                          and lote-carac-tec.cd-comp = "durezae"
                          NO-LOCK NO-ERROR.
        
         if avail lote-carac-tec then 
             ASSIGN durezae-jr = lote-carac-tec.vl-result.
        
         FIND FIRST lote-carac-tec WHERE
                          lote-carac-tec.it-codigo = ttbob-it-codigo
                          and lote-carac-tec.lote = lote-rast-jr
                          and lote-carac-tec.cd-comp = "durezac"
                          NO-LOCK NO-ERROR.
        
         if avail lote-carac-tec then 
             ASSIGN durezac-jr = lote-carac-tec.vl-result.
        
        
         FIND FIRST lote-carac-tec WHERE
                          lote-carac-tec.it-codigo = ttbob-it-codigo
                          and lote-carac-tec.lote = lote-rast-jr
                          and lote-carac-tec.cd-comp = "durezad"
                          NO-LOCK NO-ERROR.
        
         if avail lote-carac-tec then 
             ASSIGN durezad-jr = lote-carac-tec.vl-result.
        
        
         FIND FIRST lote-carac-tec WHERE
                          lote-carac-tec.it-codigo = ttbob-it-codigo
                          and lote-carac-tec.lote = lote-rast-jr
                          and lote-carac-tec.cd-comp = "defpri"
                          NO-LOCK NO-ERROR.
         
         if avail lote-carac-tec then do:
                              
            FIND FIRST lote-res-carac
                 where lote-res-carac.cd-folha = lote-carac-tec.cd-folha and
                 lote-res-carac.it-codigo = lote-carac-tec.it-codigo and
                 lote-res-carac.lote = lote-carac-tec.lote and
                 lote-res-carac.nr-tabela = lote-carac-tec.nr-tabela and
                 lote-res-carac.sequencia <> 0 and
                 lote-res-carac.cd-comp = lote-carac-tec.cd-comp
                 NO-LOCK NO-ERROR.
         
            IF AVAIL lote-res-carac THEN DO:
         
               FIND FIRST c-tab-res
                    where c-tab-res.nr-tabela = lote-res-carac.nr-tabela AND
                          c-tab-res.sequencia = lote-res-carac.sequencia
                    NO-LOCK NO-ERROR.
         
                    IF AVAIL c-tab-res THEN
                       DESC-def1 = c-tab-res.descricao.
         
            END.
         
         END.
        
        FIND FIRST lote-carac-tec WHERE
                         lote-carac-tec.it-codigo = ttbob-it-codigo
                         and lote-carac-tec.lote = lote-rast-jr
                         and lote-carac-tec.cd-comp = "defsec"
                         NO-LOCK NO-ERROR.
        
        if avail lote-carac-tec then do:
                             
           FIND FIRST lote-res-carac
                where lote-res-carac.cd-folha = lote-carac-tec.cd-folha and
                lote-res-carac.it-codigo = lote-carac-tec.it-codigo and
                lote-res-carac.lote = lote-carac-tec.lote and
                lote-res-carac.nr-tabela = lote-carac-tec.nr-tabela and
                lote-res-carac.sequencia <> 0 and
                lote-res-carac.cd-comp = lote-carac-tec.cd-comp
                NO-LOCK NO-ERROR.
        
           IF AVAIL lote-res-carac THEN DO:
        
              FIND FIRST c-tab-res
                   where c-tab-res.nr-tabela = lote-res-carac.nr-tabela AND
                         c-tab-res.sequencia = lote-res-carac.sequencia
                   NO-LOCK NO-ERROR.
        
                   IF AVAIL c-tab-res THEN
                      DESC-def2 = c-tab-res.descricao.
        
           END.
        
        END.
        
        ASSIGN c-relatorio:range("I" + STRING(i-linha)):VALUE = turma-jr
               c-relatorio:range("J" + STRING(i-linha)):VALUE = maq-jr
               c-relatorio:range("L" + STRING(i-linha)):VALUE = desc-def1
               c-relatorio:range("M" + STRING(i-linha)):VALUE = desc-def2 
               c-relatorio:range("N" + STRING(i-linha)):VALUE = larg-ped 
               c-relatorio:range("O" + STRING(i-linha)):VALUE = diin-ped 
               c-relatorio:range("P" + STRING(i-linha)):VALUE = diex-ped 
               c-relatorio:range("Q" + STRING(i-linha)):VALUE = durezae-jr 
               c-relatorio:range("R" + STRING(i-linha)):VALUE = durezac-jr 
               c-relatorio:range("S" + STRING(i-linha)):VALUE = durezad-jr .
        
        
         ASSIGN lote-jr =   lote-rast-jr .
         RUN pi-acha-operador.
         ASSIGN c-relatorio:range("K" + STRING(i-linha)):VALUE = nome-op-jr.
         if trim( nome-op-jr) = "" then do:
            ASSIGN lote-jr =    ttbob-lote-bobina.
            RUN pi-acha-operador.
             ASSIGN c-relatorio:range("K" + STRING(i-linha)):VALUE = nome-op-jr.
         
         end.
         
        
    END.  /* FOR EACH tt-bobinas */

END.  /*for each tt-pedidos-06 */


IF tt-param.destino = 4 THEN DO:

   RUN pi-finaliza-impressao.
   RUN pi-finalizar IN h-acomp.

   RETURN 'OK'.

END.

if  tt-param.destino <> 1 then

    page stream str-rp.

else do:

    if   tt-param.parametro = yes then

         page stream str-rp.

end.

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


PROCEDURE grava-rastreabilidade.


    FIND FIRST ord-prod WHERE 
         ord-prod.nr-ord-produ = op-rast
         USE-INDEX codigo
         NO-LOCK NO-ERROR.

    IF AVAIL ord-prod THEN DO:

        IF lote-rast = it-pallet.lote-bobina THEN
           ASSIGN lote-rast-jr = ""
                  linha-jr     = 0.
        ELSE
           ASSIGN lote-rast-jr = lote-rast
                  linha-jr     = ord-prod.nr-linha.
    
       FIND tt-bobinas WHERE
          ttbob-nr-pedido    = ped-venda.nr-pedido   AND
          ttbob-nr-sequencia = ped-item.nr-sequencia AND
          ttbob-nr-pallet    = it-pallet.nr-pallet   AND
          ttbob-lote-bobina  = it-pallet.lote-bobina AND
          ttbob-lote         = lote-rast-jr          AND
          ttbob-linha        = linha-jr
          USE-INDEX ch-tt-bobinas NO-ERROR.

       IF NOT AVAIL tt-bobinas THEN DO:

          CREATE tt-bobinas.
          ASSIGN ttbob-nr-pedido    = ped-venda.nr-pedido    
                 ttbob-nr-sequencia = ped-item.nr-sequencia  
                 ttbob-nr-pallet    = it-pallet.nr-pallet   
                 ttbob-lote-bobina  = it-pallet.lote-bobina 
                 ttbob-lote         = lote-rast-jr
                 ttbob-linha        = linha-jr
                 ttbob-it-codigo    = ord-prod.it-codigo
                 ttbob-nr-ord-produ = ord-prod.nr-ord-produ
                 ttbob-dt-trans     = data-rast.
                 ttbob-oper         = oper-rast.
       END.
    END.
END PROCEDURE.

PROCEDURE pi-cria-planilha:

DEF VAR c-arquivo AS CHAR NO-UNDO.

    c-arquivo = c-arq + 'escq0006' + STRING(time)+ '.xls'.

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


PROCEDURE pi-acha-operador. /* mesma rotina do escp030*/

    ASSIGN nome-op-jr = "".

    FIND FIRST lote-prod WHERE
        lote-prod.lote = lote-jr 
        USE-INDEX lote
        NO-LOCK NO-ERROR.

    IF AVAIL lote-prod THEN DO:

        FIND LAST movto-mat WHERE
            movto-mat.lote = lote-prod.lote AND
            movto-mat.it-codigo = lote-prod.it-codigo AND
            movto-mat.esp-docto = 1
            USE-INDEX lote
            NO-LOCK NO-ERROR.

        IF AVAIL movto-mat THEN  DO:

           FIND FIRST rep-oper-ctrab WHERE
                rep-oper-ctrab.nr-ord-prod = movto-mat.nr-ord-prod and
                rep-oper-ctrab.num-seq-rep = movto-mat.num-sequen 
                
                NO-LOCK NO-ERROR.

           IF AVAIL rep-oper-ctrab THEN DO:

              FIND FIRST rep-oper-mod WHERE
                  rep-oper-mod.nr-ord-produ = movto-mat.nr-ord-prod AND
                  rep-oper-mod.num-seq-rep  = rep-oper-ctrab.num-seq-rep
                  USE-INDEX id
                  NO-LOCK NO-ERROR.
           
              IF AVAIL rep-oper-mod THEN DO:
           
                  FIND FIRST operador WHERE
                      operador.cod-operador = rep-oper-mod.cod-operador
                      USE-INDEX id
                      NO-LOCK NO-ERROR.
           
                  IF AVAIL operador THEN
                      ASSIGN nome-op-jr = operador.nom-operador.
           
              END.

           END.


           IF nome-op-jr = "" THEN DO:

               FIND FIRST movto-estoq OF movto-mat NO-LOCK.

               IF AVAIL movto-estoq THEN DO:
               
                   FIND FIRST usuar_mestre WHERE
                       usuar_mestre.cod_usuario = movto-estoq.usuario
                       NO-LOCK NO-ERROR.

                   IF AVAIL usuar_mestre THEN
                       ASSIGN nome-op-jr = usuar_mestre.nom_usuario.

               END.

           END.

        END.    

    END.

END PROCEDURE.


/* retirada esta rotina original e colocado a mesma rotina do escp030
PROCEDURE pi-acha-operador.

    ASSIGN nome-op-jr = "".

    FIND FIRST lote-prod WHERE
        lote-prod.lote = lote-jr 
        USE-INDEX lote
        NO-LOCK NO-ERROR.

    IF AVAIL lote-prod THEN DO:

        FIND LAST movto-mat WHERE
            movto-mat.lote = lote-prod.lote AND
            movto-mat.it-codigo = lote-prod.it-codigo AND
            movto-mat.esp-docto = 1
            USE-INDEX lote
            NO-LOCK NO-ERROR.

        IF AVAIL movto-mat THEN  DO:

           FIND FIRST rep-oper-ctrab WHERE
                rep-oper-ctrab.nr-reporte = movto-mat.nr-reporte
                USE-INDEX nr-reporte
                NO-LOCK NO-ERROR.

           IF AVAIL rep-oper-ctrab THEN DO:

              FIND FIRST rep-oper-mod WHERE
                  rep-oper-mod.nr-ord-produ = movto-mat.nr-ord-prod AND
                  rep-oper-mod.num-seq-rep  = rep-oper-ctrab.num-seq-rep
                  USE-INDEX id
                  NO-LOCK NO-ERROR.
           
              IF AVAIL rep-oper-mod THEN DO:
           
                  FIND FIRST operador WHERE
                      operador.cod-operador = rep-oper-mod.cod-operador
                      USE-INDEX id
                      NO-LOCK NO-ERROR.
           
                  IF AVAIL operador THEN
                      ASSIGN nome-op-jr = operador.nom-operador.
           
              END.

           END.
           
           else do:
           
            find first rep-oper-mod where
                  rep-oper-mod.nr-ord-produ = movto-mat.nr-ord-prod  and
                  rep-oper-mod.num-seq-rep  = movto-mat.num-sequen      
                  no-lock no-error.  
               
            if avail rep-oper-mod then do:  
        
                  FIND FIRST operador WHERE
                      operador.cod-operador = rep-oper-mod.cod-operador
                      USE-INDEX id
                      NO-LOCK NO-ERROR.
           
                  IF AVAIL operador THEN                         
                      ASSIGN nome-op-jr = operador.nom-operador.           
                  
                  end.
                  
             else do:
                  
                find first rep-oper-mod where
                  rep-oper-mod.nr-ord-produ = movto-mat.nr-ord-prod
                  no-lock no-error.  
                  
                if avail rep-oper-mod then do:  
        
                  FIND FIRST operador WHERE
                      operador.cod-operador = rep-oper-mod.cod-operador
                      USE-INDEX id
                      NO-LOCK NO-ERROR.
           
                  IF AVAIL operador THEN                         
                      ASSIGN nome-op-jr = operador.nom-operador.   
                      
                end.        
                  
             end.
                  
           end.                      


        END.    

    END.

END PROCEDURE.
*/


return 'OK'.

/* fim do programa */

