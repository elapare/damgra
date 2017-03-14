&Scoped-define WINDOW-NAME C-Win

/*------------------------------------------------------------------------
File.............: essf0022.w
Description......: Reporte de Produá∆o no Corte/Recorte/Metalizaá∆o 
Input Parameters : 
Output Parameters: 
Author...........: Amgra - JosÇ Roberto. 
Created..........: 03/11/20109    
OBS..............: 
------------------------------------------------------------------------*/
DEFINE VARIABLE r-codigo-rwi  AS ROWID                     NO-UNDO.
    
define variable c-prog-gerado as character no-undo initial "essf0022".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

def new global shared var c-erro-amg as char  format "x(3)" no-undo.
DEF BUFFER b-ord-prod FOR ord-prod.
DEF BUFFER b-item FOR ITEM.
DEFINE BUFFER bf-reservas FOR reservas.

define var c-emenda-ant as char no-undo.


CREATE WIDGET-POOL. 

/* ***************************  Definitions  ************************** */


/* Preprocessadores do Template de Relat¢rio                            */
/** Etiqueta ************************/
define temp-table tt-param2
    field destino          as integer
    field arquivo          as char
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field dir-etq          as char.
  
def temp-table tt-raw-digita
    field raw-digita      as raw.
/** Etiqueta ************************/


/* Include Com as Vari†veis Globais */
DEFINE VARIABLE c-lido AS CHARACTER   NO-UNDO.

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
def new global shared var c-it-codigo-reserva-jr as char                     no-undo.


/* Parameters Definitions ---                                           */ 


/* Temporary Table Definitions ---                                      */ 

/* Temporary Table Definitions ---                                      */ 



/*****************************************************************************
**
**   cd9203.i - Function f-item-uni-estab
**              
******************************************************************************/

/*** Esta include no ems 2.01 n∆o dever† possuir nenhum c¢digo, serve 
     apenas para desenvolvimento tratar o conceito de miniflexibilizaá∆o.
     Utilizado apenas para MANUFATURA. ***/

/*** RELEASE 2.02 ***/

/* Funá‰es ch∆o f†brica e lista de componentes 2.02 - Logoplaste - Manufatura */


/*** RELEASE 2.03 ***/

/* Integraá∆o Magnus x SFC 2.03 - Klimmek - Manufatura *//* Relacionamento Linha Produá∆o x Estabelecimento     *//* Transaá∆o Reporte Ass°ncrono                        *//* Alteraá‰es Gerais EMS 2.03                          */

/*** RELEASE 2.04 ***/

/* Alteraá‰es Gerais EMS 2.04                          *//* Integraá∆o EAI                                     */ 

FUNCTION f-item-uni-estab RETURNS char (input c-it-codigo   as char,
                                        input c-cod-estabel as char,
                                        input c-campo       as char).

    def buffer b-item for item.                 

    find b-item 
        where b-item.it-codigo = c-it-codigo no-lock no-error.

/***  Tempor†rio - os preáos (cs0102) na release 2.04 ou inferior ainda n∆o est∆o sendo tratados 
      pela tabela item-uni-estab, d£vidas com RogÇrio Vieira. ***/

    
        case c-campo:    
            when "data-base" then
                 return string (b-item.data-base).
            when "preco-base" then
                 return string (b-item.preco-base).
            when "data-ult-rep" then
                 return string (b-item.data-ult-rep).
            when "preco-repos" then
                 return string (b-item.preco-repos).
            when "data-ult-ent" then
                 return string (b-item.data-ult-ent).
            when "preco-ul-ent" then
                 return string (b-item.preco-ul-ent).
            when "preco-fiscal" then
                 return string (b-item.preco-fiscal).
    
            when "ind-refugo" then
                 return string (b-item.ind-refugo).
    
        end.
    

/*** FIM ***/

    

        def buffer b-item-uni-estab for item-uni-estab.

        if  c-cod-estabel = ? then
            assign c-cod-estabel = b-item.cod-estabel.

        find b-item-uni-estab use-index codigo
            where b-item-uni-estab.it-codigo   = c-it-codigo 
              and b-item-uni-estab.cod-estabel = c-cod-estabel no-lock no-error.

        if  avail b-item-uni-estab then do:
            case c-campo:
                 when "altera-conta" then
                      return string (b-item-uni-estab.altera-conta).

                 when "cd-freq" then
                      return string (b-item-uni-estab.cd-freq).

                 when "cod-estab-gestor" then
                      return string (b-item-uni-estab.cod-estab-gestor).

                 when "cod-fat-ponder" then
                      return string (b-item-uni-estab.cod-fat-ponder).

                 when "cod-grp-compra" then
                      return string (b-item-uni-estab.cod-grp-compra).

                 when "crit-cc" then
                      return string (b-item-uni-estab.crit-cc).

                 when "crit-ce" then
                      return string (b-item-uni-estab.crit-ce).

                 when "data-pr-fisc" then
                      return string (b-item-uni-estab.data-pr-fisc).

                 when "data-ult-ressup" then
                      return string (b-item-uni-estab.data-ult-ressup).

                 when "dep-rej-cq" then
                      return string (b-item-uni-estab.dep-rej-cq).

                 when "deposito-cq" then
                      return string (b-item-uni-estab.deposito-cq).

                 when "fator-ponder[1]" then
                      return string (b-item-uni-estab.fator-ponder[1]).

                 when "fator-ponder[2]" then
                      return string (b-item-uni-estab.fator-ponder[2]).

                 when "fator-ponder[3]" then
                      return string (b-item-uni-estab.fator-ponder[3]).

                 when "fator-ponder[4]" then
                      return string (b-item-uni-estab.fator-ponder[4]).

                 when "fator-ponder[5]" then
                      return string (b-item-uni-estab.fator-ponder[5]).

                 when "fator-ponder[6]" then
                      return string (b-item-uni-estab.fator-ponder[6]).

                 when "fator-ponder[7]" then
                      return string (b-item-uni-estab.fator-ponder[7]).

                 when "fator-ponder[8]" then
                      return string (b-item-uni-estab.fator-ponder[8]).

                 when "fator-ponder[9]" then
                      return string (b-item-uni-estab.fator-ponder[9]).

                 when "fator-ponder[10]" then
                      return string (b-item-uni-estab.fator-ponder[10]).

                 when "fator-ponder[11]" then
                      return string (b-item-uni-estab.fator-ponder[11]).

                 when "fator-ponder[12]" then
                      return string (b-item-uni-estab.fator-ponder[12]).

                 when "ind-cons-prv" then
                      return string (b-item-uni-estab.ind-cons-prv).

                 when "ind-lista-csp" then
                      return string (b-item-uni-estab.ind-lista-csp).

                 when "ind-lista-mrp" then
                      return string (b-item-uni-estab.ind-lista-mrp).

                 when "ind-refugo" then
                      return string (b-item-uni-estab.ind-refugo).

                 when "lim-var-qtd" then
                      return string (b-item-uni-estab.lim-var-qtd).

                 when "lim-var-valor" then
                      return string (b-item-uni-estab.lim-var-valor).

                 when "log-ad-consumo" then
                      return string (b-item-uni-estab.log-ad-consumo).

                 when "log-finaliz-op" then
                      return string (b-item-uni-estab.log-finaliz-op).

                 when "lote-per-max" then
                      return string (b-item-uni-estab.lote-per-max).

                 when "ponto-encomenda" then
                      return string (b-item-uni-estab.ponto-encomenda).

                 when "prioridade-aprov" then
                      return string (b-item-uni-estab.prioridade-aprov).

                 when "qt-min-res-fabr" then
                      return string (b-item-uni-estab.qt-min-res-fabr).

                 when "res-min-fabri" then
                      return string (b-item-uni-estab.res-min-fabri).

                 when "tp-codigo" then
                      return string (b-item-uni-estab.tp-codigo).

                 when "tp-ressup" then
                      return string (b-item-uni-estab.tp-ressup).

                 when "var-qtd-re" then
                      return string (b-item-uni-estab.var-qtd-re).

                 when "var-qtd-res-fabr" then
                      return string (b-item-uni-estab.var-qtd-res-fabr).

                 when "var-tempo-res-fabr" then
                      return string (b-item-uni-estab.var-tempo-res-fabr).

                 when "var-val-re-maior" then
                      return string (b-item-uni-estab.var-val-re-maior).

                 when "var-val-re-menor" then
                      return string (b-item-uni-estab.var-val-re-menor).

                 when "variacao-perm" then
                      return string (b-item-uni-estab.variacao-perm).

                 when "vl-ggf-ant" then
                      return string (b-item-uni-estab.vl-ggf-ant).

                 when "vl-mat-ant" then
                      return string (b-item-uni-estab.vl-mat-ant).

                 when "vl-mob-ant" then
                      return string (b-item-uni-estab.vl-mob-ant).

                 /*****************************************************************************
**
**   cd9203.i1 - Campos item x item-uni-estab
**
******************************************************************************/

     when "cap-est-fabr" then
          return string (b-item-uni-estab.cap-est-fabr).
     when "cd-planejado" then
          return string (b-item-uni-estab.cd-planejado).
     when "char-1" then
          return string (b-item-uni-estab.char-1).
     when "char-2" then
          return string (b-item-uni-estab.char-2).
     when "check-sum" then
          return string (b-item-uni-estab.check-sum).
     when "ciclo-contag" then
          return string (b-item-uni-estab.ciclo-contag).
     when "classe-repro" then
          return string (b-item-uni-estab.classe-repro).
     when "classif-abc" then
          return string (b-item-uni-estab.classif-abc).
     when "cod-comprado" then
          return string (b-item-uni-estab.cod-comprado).
     when "cod-estabel" then
          return string (b-item-uni-estab.cod-estabel).
     when "cod-localiz" then
          return string (b-item-uni-estab.cod-localiz).
     when "cod-obsoleto" then
          return string (b-item-uni-estab.cod-obsoleto).
     when "consumo-aad" then
          return string (b-item-uni-estab.consumo-aad).
     when "consumo-prev" then
          return string (b-item-uni-estab.consumo-prev).
     when "contr-plan" then
          return string (b-item-uni-estab.contr-plan).
     when "contr-qualid" then
          return string (b-item-uni-estab.contr-qualid).
     when "conv-tempo-seg" then
          return string (b-item-uni-estab.conv-tempo-seg).
     when "criticidade" then
          return string (b-item-uni-estab.criticidade).
     when "curva-abc" then
          return string (b-item-uni-estab.curva-abc).
     when "data-1" then
          return string (b-item-uni-estab.data-1).
     when "data-2" then
          return string (b-item-uni-estab.data-2).
     when "data-base" then
          return string (b-item-uni-estab.data-base).
     when "data-ult-con" then
          return string (b-item-uni-estab.data-ult-con).
     when "data-ult-ent" then
          return string (b-item-uni-estab.data-ult-ent).
     when "data-ult-rep" then
          return string (b-item-uni-estab.data-ult-rep).
     when "data-ult-sai" then
          return string (b-item-uni-estab.data-ult-sai).
     when "dec-1" then
          return string (b-item-uni-estab.dec-1).
     when "dec-2" then
          return string (b-item-uni-estab.dec-2).
     when "demanda" then
          return string (b-item-uni-estab.demanda).
     when "deposito-pad" then
          return string (b-item-uni-estab.deposito-pad).
     when "div-ordem" then
          return string (b-item-uni-estab.div-ordem).
     when "emissao-ord" then
          return string (b-item-uni-estab.emissao-ord).
     when "fator-refugo" then
          return string (b-item-uni-estab.fator-refugo).
     when "horiz-fixo" then
          return string (b-item-uni-estab.horiz-fixo).
     when "ind-calc-meta" then
          return string (b-item-uni-estab.ind-calc-meta).
     when "ind-prev-demanda" then
          return string (b-item-uni-estab.ind-prev-demanda).
     when "int-1" then
          return string (b-item-uni-estab.int-1).
     when "int-2" then
          return string (b-item-uni-estab.int-2).
     when "it-codigo" then
          return string (b-item-uni-estab.it-codigo).
     when "loc-unica" then
          return string (b-item-uni-estab.loc-unica).
     when "log-1" then
          return string (b-item-uni-estab.log-1).
     when "log-2" then
          return string (b-item-uni-estab.log-2).
     when "lote-economi" then
          return string (b-item-uni-estab.lote-economi).
     when "lote-minimo" then
          return string (b-item-uni-estab.lote-minimo).
     when "lote-multipl" then
          return string (b-item-uni-estab.lote-multipl).
     when "nat-despesa" then
          return string (b-item-uni-estab.nat-despesa).
     when "nr-linha" then
          return string (b-item-uni-estab.nr-linha).
     when "periodo-fixo" then
          return string (b-item-uni-estab.periodo-fixo).
     when "perm-saldo-neg" then
          return string (b-item-uni-estab.perm-saldo-neg).
     when "politica" then
          return string (b-item-uni-estab.politica).
     when "preco-base" then
          return string (b-item-uni-estab.preco-base).
     when "preco-fiscal" then
          return string (b-item-uni-estab.preco-fiscal).
     when "preco-repos" then
          return string (b-item-uni-estab.preco-repos).
     when "preco-ul-ent" then
          return string (b-item-uni-estab.preco-ul-ent).
     when "prioridade" then
          return string (b-item-uni-estab.prioridade).
     when "qt-max-ordem" then
          return string (b-item-uni-estab.qt-max-ordem).
     when "qtd-batch-padrao" then
          return string (b-item-uni-estab.qtd-batch-padrao).
     when "qtd-refer-custo-dis" then
          return string (b-item-uni-estab.qtd-refer-custo-dis).
     when "quant-perda" then
          return string (b-item-uni-estab.quant-perda).
     when "quant-segur" then
          return string (b-item-uni-estab.quant-segur).
     when "rep-prod" then
          return string (b-item-uni-estab.rep-prod).
     when "reporte-ggf" then
          return string (b-item-uni-estab.reporte-ggf).
     when "reporte-mob" then
          return string (b-item-uni-estab.reporte-mob).
     when "res-cq-comp" then
          return string (b-item-uni-estab.res-cq-comp).
     when "res-cq-fabri" then
          return string (b-item-uni-estab.res-cq-fabri).
     when "res-for-comp" then
          return string (b-item-uni-estab.res-for-comp).
     when "res-int-comp" then
          return string (b-item-uni-estab.res-int-comp).
     when "ressup-fabri" then
          return string (b-item-uni-estab.ressup-fabri).
     when "sit-aloc" then
          return string (b-item-uni-estab.sit-aloc).
     when "tempo-segur" then
          return string (b-item-uni-estab.tempo-segur).
     when "tipo-est-seg" then
          return string (b-item-uni-estab.tipo-est-seg).
     when "tipo-lote-ec" then
          return string (b-item-uni-estab.tipo-lote-ec).
     when "tipo-requis" then
          return string (b-item-uni-estab.tipo-requis).
     when "tipo-sched" then
          return string (b-item-uni-estab.tipo-sched).
     when "tp-aloc-lote" then
          return string (b-item-uni-estab.tp-aloc-lote).
     when "val-fator-custo-dis" then
          return string (b-item-uni-estab.val-fator-custo-dis).
     when "var-mob-maior" then
          return string (b-item-uni-estab.var-mob-maior).
     when "var-mob-menor" then
          return string (b-item-uni-estab.var-mob-menor).
     when "var-rep" then
          return string (b-item-uni-estab.var-rep).
     when "var-transf" then
          return string (b-item-uni-estab.var-transf).
     when "variac-acum" then
          return string (b-item-uni-estab.variac-acum).
     when "vl-var-max" then
          return string (b-item-uni-estab.vl-var-max).
     when "vl-var-min" then
          return string (b-item-uni-estab.vl-var-min).

 
            end case.
        end.
    

    return "".

END FUNCTION.

/* Fim da Include */
      /* Function f-item-uni-estab        */
    



define temp-table tt-pallet no-undo
    field nr-pallet    as char
    field nr-ord-produ AS INT
    field situacao     AS INT
    INDEX codigo
          nr-pallet.

def temp-table tt-rep-oper-ctrab  no-undo like rep-oper-ctrab
    field cod-ferr-prod                   like split-operac.cod-ferr-prod
    field dat-fim-setup                   like split-operac.dat-fim-setup
    field dat-inic-setup                  like split-operac.dat-inic-setup
    field qtd-segs-fim-setup              like split-operac.qtd-segs-fim-setup
    field qtd-segs-inic-setup             like split-operac.qtd-segs-inic-setup.

def temp-table tt-rep-refugo-oper no-undo like rep-refugo-oper.
def temp-table tt-rep-ic-oper     no-undo like rep-ic-oper.
def temp-table tt-rep-ic-oper-tab no-undo like rep-ic-oper-tab.




/*******************************************************************
**
**  CD0666.I - Definicao temp-table de erros
**
*******************************************************************/
def new global shared var l-multi as logical initial yes.
def new global shared var l-peso-bal as logical .
def new global shared var h-paizao  AS HANDLE .



def var l-del-erros as logical init yes.
def var v-nom-arquivo-cb as char format "x(50)" no-undo.
def var c-mensagem-cb    as char format "x(132)" no-undo.

def  temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)".

form
    space(04)
    tt-erro.cd-erro 
    space (02)
    c-mensagem-cb
    with width 132 no-box down stream-io frame f-consiste.


def temp-table tt-erro2 like tt-erro.

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Mensagem",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tt-erro.cd-erro:label in frame f-consiste = trim(return-value).

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Descriá∆o",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-mensagem-cb:label in frame f-consiste = trim(return-value).


{cdp/cdcfgman.i}
{cdp/cdcfgmat.i}
{cpp\cpapi001.i}

/******************************************************************************
 ** 
 **  INCLUDE  : CPAPI012.I 
 **
 **  OBJETIVO : Definir as temp-tables da API de Requis. de Materiais 
 **
 ******************************************************************************/
 
 def temp-table tt-requis no-undo
     field tipo-trans       as integer init 1
     field nr-ord-produ     like ord-prod.nr-ord-produ
     field quantidade       as decimal
     field data             as date
     field item-ini         as char    init "                "
     field item-fim         as char    init "ZZZZZZZZZZZZZZZZ"
     field deposito-ini     as char    init "   "
     field deposito-fim     as char    init "ZZZ"
     field op-codigo-ini    as integer init 0
     field op-codigo-fim    as integer init 9999
     field cod-localiz-ini  as char    init "   "          /*pacote 97*/
     field cod-localiz-fim  as char    init "ZZZZZZZZZZ"   /*pacote 97*/     
     field procura-saldos   as logical
     field carrega-reservas as logical init yes
     field prog-seg         as char
     field time-out         as integer init 30
     field tentativas       as integer init 10
     field cod-versao-integracao as integer format "999"
     field nro-docto  like movto-mat.serie-docto
     field serie-docto like movto-mat.nro-docto.
 

    
    def temp-table tt-reporta no-undo
    field nr-ord-produ  like ord-prod.nr-ord-produ
    field it-codigo     like item.it-codigo
    field nr-pedcli     like ped-venda.nr-pedcli
    field nome-abrev    like ped-venda.nome-abrev
    field qt-ordem      like ord-prod.qt-ordem
    field qt-reporte    like rep-prod.qt-reporte
    field cod-depos     like movto-estoq.cod-depos
    field cod-localiz   like movto-estoq.cod-localiz
    FIELD gera-pallet   AS CHAR FORMAT "x(11)" LABEL "Gera Palete"
    field des-carac      as  char format "x(60)" label "Caracter°sticas"
    index id nr-ord-produ.

def temp-table tt-lote-reporte no-undo
    field nr-ord-produ like ord-prod.nr-ord-produ
    field lote         like movto-estoq.lote
    field quantidade   like movto-estoq.quantidade
    field dt-vali-lote  like saldo-estoq.dt-vali-lote
    index id nr-ord-produ lote.
 
def temp-table tt-param no-undo
    field it-codigo  like item.it-codigo
    field h-handle    as  handle
    field lote       like movto-estoq.lote
    field emenda1    like movto-estoq.lote
    field emenda2    like movto-estoq.lote
    field qt-lote    like movto-estoq.quantidade
    field qt-emenda1 like movto-estoq.quantidade
    field qt-emenda2 like movto-estoq.quantidade.

def temp-table tt-digita no-undo
    field nr-ord-produ like ord-prod.nr-ord-produ
    field cod-estabel  like ord-prod.cod-estabel
    field nr-linha     like ord-prod.nr-linha
    field rw-lote-item as rowid 
    field arquivo      as char.


def new global shared var grw-lote-item as rowid  no-undo.
def new global shared var gc-estado     as char   no-undo.
/* Vari†veis para atualizaá∆o da nova tabela de rastreabilidade */

define VARIABLE nr-ord-produ-jr-pro LIKE movto-estoq.nr-ord-produ.
define variable cod-estabel-jr-pro  LIKE movto-estoq.cod-estabel.
define variable it-codigo-jr-pro    LIKE movto-estoq.it-codigo.
define variable lote-jr-pro  AS CHAR          NO-UNDO.
define variable lote-jr-con1 AS CHAR          NO-UNDO.
define variable lote-jr-con2 AS CHAR          NO-UNDO.
define variable lote-jr-con3 AS CHAR          NO-UNDO.

DEFINE VARIABLE conf AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-porta AS CHARACTER   NO-UNDO.
DEFINE STREAM StreamName.
/* Variaveis das Procedures de Reporte */
def var hReporte     as handle no-undo.
def var hRequis      as handle no-undo.
def var hRepSfc      as handle no-undo.
def var deLote       as dec    no-undo.
def var deEmenda1    as dec    no-undo.
def var deEmenda2    as dec    no-undo.
def var deQuantNec   as dec    no-undo.
def var deTotal      as dec    no-undo.
def var deRefUni     as dec    no-undo.
def var deRefTot     as dec    no-undo.
def var deSegIni     as dec    no-undo.
def var deSegFim     as dec    no-undo.
def var lErro        as log    no-undo.
def var cErro        as char   no-undo.
def var cTexto       as char   no-undo.
def var iCont        as int    no-undo.
def var iSeqEtq      as int    no-undo.
def var cInformCompl as char no-undo.
/* Variaveis das Procedures de Reporte */


    DEFINE TEMP-TABLE tt-estacao no-undo
        FIELD estacao             AS INT     FORMAT ">>9"            LABEL "Estaá∆o"
        FIELD nr-ord-produ        AS INT     FORMAT ">>>>>>>>9"      label "Ord.Producao"
        FIELD it-codigo           AS CHAR    FORMAT "x(16)"          LABEL "Item"
        FIELD lote                AS CHAR    FORMAT "x(10)"          LABEL "Lote"
        FIELD nr-pedido           AS INT     FORMAT ">>>>>>>>9"      label "Nr.Pedido"
        FIELD seq-ped             AS INT     FORMAT ">>>>9"          label "Seq"
        FIELD nome-abrev          AS CHAR    FORMAT "x(12)"          LABEL "Cliente"
        FIELD cod-depos           AS CHAR    FORMAT "x(03)"          LABEL "Dep"
        FIELD cod-localiz         AS CHAR    FORMAT "x(10)"          LABEL "Localiz"
        FIELD gera-pallet         AS CHAR    FORMAT "x(1)"           label "Gera Pallet"
        FIELD caract              AS CHAR    FORMAT "x(20)"          label "Caracter°sticas"
        FIELD nr-ord-off-spc      AS INT     FORMAT ">>>>>>>>9"
        FIELD nr-ord-off-grd      AS INT     FORMAT ">>>>>>>>9"
        FIELD letra-bobina        AS CHAR    FORMAT "x(1)" 
        FIELD nr-prim-bob         AS INT     FORMAT ">>>>>>>>9" 
        INDEX chave IS PRIMARY UNIQUE estacao ASCENDING.

    DEFINE BUFFER b-tt-estacao FOR tt-estacao.


    DEFINE TEMP-TABLE tt-defeito no-undo
        FIELD nr-tabela           AS INT     FORMAT ">>>9"           LABEL "Nr.Tab."
        FIELD cod-def             AS INT     FORMAT ">>>9"           label "Cod.Def."
        FIELD descricao           AS CHAR    FORMAT "x(40)"          LABEL "Descriá∆o"
        INDEX chave IS PRIMARY UNIQUE nr-tabela
                                      cod-def.

    DEFINE TEMP-TABLE tt-defpri no-undo
        FIELD nr-tabela           AS INT     FORMAT ">>>9"           LABEL "Nr.Tab."
        FIELD cod-def             AS INT     FORMAT ">>>9"           label "Cod.Def."
        FIELD descricao           AS CHAR    FORMAT "x(40)"          LABEL "Descriá∆o"
        INDEX chave IS PRIMARY UNIQUE nr-tabela
                                      cod-def.

    DEFINE TEMP-TABLE tt-defsec no-undo
        FIELD nr-tabela           AS INT     FORMAT ">>>9"           LABEL "Nr.Tab."
        FIELD cod-def             AS INT     FORMAT ">>>9"           label "Cod.Def."
        FIELD descricao           AS CHAR    FORMAT "x(40)"          LABEL "Descriá∆o"
        INDEX chave IS PRIMARY UNIQUE nr-tabela
                                      cod-def.


    DEFINE TEMP-TABLE tt-lote-cons no-undo
        FIELD seq                 AS INT     FORMAT ">>9"
        FIELD lote                AS CHAR    FORMAT "x(10)"          
        FIELD it-codigo           AS CHAR    FORMAT "x(16)"          
        FIELD cod-depos           AS CHAR    FORMAT "x(03)"          
        FIELD cod-localiz         AS CHAR    FORMAT "x(10)"          
        FIELD cod-refer           AS CHAR    FORMAT "x(08)"          
        FIELD cod-estabel         AS CHAR    FORMAT "x(03)"          
        FIELD saldo               AS DEC  
        FIELD consumo             AS DEC
        INDEX chave IS PRIMARY UNIQUE seq
                                      lote          
                                      it-codigo    
                                      cod-depos    
                                      cod-localiz  
                                      cod-refer    
                                      cod-estabel.  




DEFINE VARIABLE i-jr       AS INTEGER      NO-UNDO.
DEFINE VARIABLE flag-estacao AS INTEGER    NO-UNDO.

/* Transfer Definitions */

/************************************************************************************/

/* Transfer Definitions */

/* Local Variable Definitions ---                                       */ 
 DEFINE VARIABLE i-mediaqtd AS INTEGER    NO-UNDO.
  DEFINE VARIABLE d-mediaotica AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE d-durezamenor AS decimal    NO-UNDO.
  DEFINE VARIABLE d-durezamaior AS decimal    NO-UNDO.
  DEFINE VARIABLE d-deltarho AS DECIMAL    NO-UNDO.
def var l-ok                 as logical no-undo. 
def var c-arq-digita         as char    no-undo. 
def var c-terminal           as char    no-undo. 
def var v-cod-pg-mouse-selec as char    no-undo. 
def var v-cod-prog-i-rprun   as char    no-undo. 
def var c-impressora-old     as char    no-undo. 
def var c-arquivo-old        as char    no-undo. 
def var c-destino-old        as char    no-undo. 
def var i-cont               as int     no-undo. 
def var v-cod-prog-gerado    as char    no-undo. 

def var h-acomp            as handle no-undo.
def var v-num-reg-lidos    as int    no-undo.


/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 


/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 

DEFINE VARIABLE c-cod-estabel       AS CHAR      FORMAT "x(3)"   initial "422"           NO-UNDO. /*solic-318*/ 
DEFINE VARIABLE c-cod-ctrab         AS CHAR      FORMAT "x(16)"             NO-UNDO.
DEFINE VARIABLE c-cod-operador      AS CHAR      FORMAT "x(07)"             NO-UNDO.
DEFINE VARIABLE c-lote-cons         AS CHAR      FORMAT "x(10)"             NO-UNDO.
DEFINE VARIABLE c-emenda-1          AS CHAR      FORMAT "x(10)"             NO-UNDO.
DEFINE VARIABLE c-emenda-2          AS CHAR      FORMAT "x(10)"             NO-UNDO.
                                                                            
DEFINE VARIABLE c-nome-estab        AS CHARACTER FORMAT "X(40)"             NO-UNDO.
DEFINE VARIABLE c-desc-ctrab        AS CHARACTER FORMAT "X(40)"             NO-UNDO.
DEFINE VARIABLE c-nome-operador     AS CHARACTER FORMAT "X(40)"             NO-UNDO.
                                                                            
DEFINE VARIABLE c-desc-item-cons    AS CHARACTER FORMAT "X(40)"             NO-UNDO.
DEFINE VARIABLE c-desc-item-emen1   AS CHARACTER FORMAT "X(40)"             NO-UNDO.
DEFINE VARIABLE c-desc-item-emen2   AS CHARACTER FORMAT "X(40)"             NO-UNDO.
                                                                            
DEFINE VARIABLE c-it-codigo-cons    AS CHARACTER FORMAT "X(16)"             NO-UNDO.
DEFINE VARIABLE c-it-codigo-emen1   AS CHARACTER FORMAT "X(16)"             NO-UNDO.
DEFINE VARIABLE c-it-codigo-emen2   AS CHARACTER FORMAT "X(16)"             NO-UNDO.
                                                                            
DEFINE VARIABLE d-saldo-bob         AS DEC       FORMAT "->>>>>>>9.9999"    NO-UNDO.

DEFINE VARIABLE i-nr-doff           AS INT       FORMAT ">>9"               NO-UNDO.
                                                                            
DEFINE VARIABLE dt-trans-ini        AS date      FORMAT 99/99/9999          INITIAL today NO-UNDO.
DEFINE VARIABLE c-hr-inic-prod      AS CHAR      FORMAT "99:99"             NO-UNDO.

DEFINE VARIABLE dt-trans-fim        AS date      FORMAT 99/99/9999          INITIAL today NO-UNDO.
DEFINE VARIABLE c-hr-fim-prod       AS CHAR      FORMAT "99:99"             NO-UNDO.

DEFINE VARIABLE i-estacao           AS INT       FORMAT ">>9"               NO-UNDO.
DEFINE VARIABLE i-nr-ord-produ      AS INT       FORMAT ">>>>>>>9"          NO-UNDO.
DEFINE VARIABLE c-it-codigo-prod    AS CHARACTER FORMAT "X(16)"             NO-UNDO.
DEFINE VARIABLE i-nr-pedido         AS INT       FORMAT ">>>>>>>9"          NO-UNDO.
DEFINE VARIABLE i-seq-ped           AS INT       FORMAT ">>>>9"             NO-UNDO.

DEFINE VARIABLE c-lote-bobina       AS CHAR      FORMAT "x(10)"             NO-UNDO.
DEFINE VARIABLE d-peso-liquido      AS DEC       FORMAT "->>>>>>>9.9999"    NO-UNDO.
DEFINE VARIABLE d-peso-bal          AS CHAR       FORMAT "x(20)"    NO-UNDO.

DEFINE VARIABLE i-turno             AS INT       FORMAT ">9"                NO-UNDO.
DEFINE VARIABLE i-maquina           AS INT       FORMAT ">>>>>>>9"          NO-UNDO.
DEFINE VARIABLE c-turma             AS CHAR      FORMAT "x(1)"              NO-UNDO.
DEFINE VARIABLE i-largura           AS INT       FORMAT ">>>>>>>9"          NO-UNDO.
DEFINE VARIABLE i-diin              AS INT       FORMAT ">>>>>>>9"          NO-UNDO.
DEFINE VARIABLE i-diex              AS INT       FORMAT ">>>>>>>9"          NO-UNDO.
DEFINE VARIABLE i-espessura         AS INT       FORMAT ">>9"               NO-UNDO.
DEFINE VARIABLE i-compr             AS INT       FORMAT ">>>>>>>9"          NO-UNDO.
DEFINE VARIABLE i-dureza-e          AS INT       FORMAT ">>>>>>>9"          NO-UNDO.
DEFINE VARIABLE i-dureza-c          AS INT       FORMAT ">>>>>>>9"          NO-UNDO.
DEFINE VARIABLE i-dureza-d          AS INT       FORMAT ">>>>>>>9"          NO-UNDO.
DEFINE VARIABLE i-delta             AS INT       FORMAT ">>>>>>>9"          NO-UNDO.
DEFINE VARIABLE i-larg-util         AS INT       FORMAT ">>>>>>>9"          NO-UNDO.
DEFINE VARIABLE i-emendas           AS INT       FORMAT ">>>>>>>9"          NO-UNDO.

/****************** Definiáao de Vari†veis de Trabalho *********************/ 

DEFINE VARIABLE saldo-lote-cons     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE saldo-emenda1-cons  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE saldo-emenda2-cons  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE text-string         AS CHARACTER  FORMAT "x(180)".
DEFINE VARIABLE dias-jr             AS INTEGER    NO-UNDO.
DEFINE VARIABLE horai-jr            AS DECIMAL    NO-UNDO.
DEFINE VARIABLE horaf-jr            AS DECIMAL    NO-UNDO.
DEFINE VARIABLE operador-jr         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE peso-jr             AS DECIMAL    NO-UNDO.
DEFINE VARIABLE tp-defeito          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE nr-bobina-def       AS CHARACTER  NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */ 

DEFINE VARIABLE c-arquivo-jr AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE c-temp-jr AS CHARACTER FORMAT "x(100)" NO-UNDO.
c-temp-jr = session:temp-directory.


/* ***********************  Control Definitions  ********************** */ 

DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO. 


DEFINE VARIABLE rs-opcao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Produá∆o Bobinas", 1,
          "Perdas / Refugo", 2
     SIZE 34 BY .92 NO-UNDO.


DEFINE VARIABLE rs-tipo AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Prime", 1,
          "Off Specs", 2,
          "Off grade", 3
     SIZE 34 BY .92 NO-UNDO.



DEFINE IMAGE IMAGE-1
    FILENAME "image\im-fir"
    SIZE 3 BY .88.
    
DEFINE IMAGE IMAGE-2
    FILENAME "image\im-las"
    SIZE 3 BY .88.
    
    
DEFINE IMAGE im-pg-imp
    FILENAME "image\im-fldup"
    SIZE 15.72 BY 1.19.
    
DEFINE IMAGE im-pg-par
    FILENAME "image\im-fldup"
    SIZE 15.72 BY 1.19.
    
DEFINE IMAGE im-pg-sel
    FILENAME "image\im-fldup"
    SIZE 20 BY 1.19.

DEFINE IMAGE im-pg-dig
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.19.


DEFINE VARIABLE c-arquivo AS CHARACTER 
VIEW-AS EDITOR MAX-CHARS 256 
SIZE 40 BY 1.00 
BGCOLOR 15  font 2 NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)" INITIAL "Destino"
VIEW-AS TEXT 
SIZE 8.57 BY .62 NO-UNDO. 

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)" INITIAL "Execuá∆o"
VIEW-AS TEXT 
SIZE 10.86 BY .62 NO-UNDO.

DEFINE VARIABLE text-parametro AS CHARACTER FORMAT "X(256)" INITIAL "ParÉmetros de Impress∆o"
VIEW-AS TEXT 
SIZE 24.72 BY .62 NO-UNDO.



DEFINE RECTANGLE RECT-7
EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL
SIZE 80 BY 1.42 
BGCOLOR 7.  

DEFINE RECTANGLE RECT-8
EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL
SIZE 80 BY 1.42 
BGCOLOR 7.  

DEFINE RECTANGLE RECT-9
EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL
SIZE 108 BY 3.42 
BGCOLOR 7.      

DEFINE RECTANGLE RECT-10 
EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
SIZE 108 BY 14.72
BGCOLOR 7.


DEFINE RECTANGLE RECT-11
EDGE-PIXELS 2 GRAPHIC-EDGE NO-FILL
SIZE 108 BY 4.7 
BGCOLOR 7. 

DEFINE BUTTON bt-novo
    IMAGE FILENAME "image\im-add"
    SIZE 4 BY 1.

DEFINE BUTTON bt-cancela
    IMAGE FILENAME "image\im-can"
    SIZE 4 BY 1.

DEFINE BUTTON bt-sai
    IMAGE FILENAME "image\im-exi"
    SIZE 4 BY 1.

DEFINE BUTTON btpeso 
     IMAGE-UP FILE "image\im-bloq":U
     IMAGE-INSENSITIVE FILE "image\im-bloqi":U
     LABEL "peso" 
     SIZE 4 BY 1
     FONT 4.


DEFINE BUTTON bt-ajuda 
    IMAGE FILENAME "image\im-hel"
    SIZE 4 BY 1.


DEFINE BUTTON bt-cancela2 AUTO-END-KEY 
     LABEL "C&ancelar" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-atualiza AUTO-GO 
     LABEL "&Confirma" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-setup AUTO-GO 
     LABEL "Setup &M†quina" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-defpri AUTO-GO 
     LABEL "Def.&Prim†rio" 
     SIZE 13 BY 1
     BGCOLOR 8 .


DEFINE BUTTON bt-defsec AUTO-GO 
     LABEL "Def.&Secund†rio" 
     SIZE 13 BY 1
     BGCOLOR 8 .


DEFINE RECTANGLE rt-botoes
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 108 BY 1.42
     BGCOLOR 7.


DEFINE RECTANGLE RECT-1
EDGE-PIXELS 2 GRAPHIC-EDGE 
SIZE 108 BY 1.42 
BGCOLOR 7.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
    bt-novo     AT ROW 1.25 COL 4    HELP "Inclui Novo Registro"
    bt-cancela  AT ROW 1.25 COL 8.5  HELP "Cancela"
    btpeso AT ROW 1.25 COL 90 HELP
          "Libera digitaá∆o de peso"
    bt-sai      AT ROW 1.25 COL 100  HELP "Encerra Programa"
    bt-ajuda    AT ROW 1.25 COL 105  HELP "Ajuda"

    c-cod-estabel LABEL "Estabelecimento"
     at row 2.7 col 14 colon-aligned
     view-as fill-in 
     size 06 by .88
     font 1
   
    c-nome-estab NO-LABEL
     at row 2.7 col 35 colon-aligned
     view-as fill-in 
     size 45 by .88
     font 1

    c-cod-ctrab LABEL "Centro Trabalho"
     at row 3.7 col 14 colon-aligned
     view-as fill-in 
     size 16 by .88
     font 1
   
    c-desc-ctrab NO-LABEL
     at row 3.7 col 35 colon-aligned
     view-as fill-in 
     size 45 by .88
     font 1

    c-cod-operador LABEL "Operador"
     at row 4.7 col 14 colon-aligned
     view-as fill-in 
     size 16 by .88
     font 1
   
    c-nome-operador NO-LABEL
     at row 4.7 col 35 colon-aligned
     view-as fill-in 
     size 45 by .88
     font 1

    c-lote-cons LABEL "Lote"
     at row 6.7 col 14 colon-aligned
     view-as fill-in 
     size 16 by .88
     font 1
   
    c-it-codigo-cons LABEL "Item"
     at row 6.7 col 35 colon-aligned
     view-as fill-in 
     size 17 by .88
     font 1

    c-desc-item-cons NO-LABEL
     at row 6.7 col 55 colon-aligned
     view-as fill-in 
     size 25 by .88
     font 1

    c-emenda-1 LABEL "Emenda 1"
     at row 7.7 col 14 colon-aligned
     view-as fill-in 
     size 16 by .88
     font 1
   
    c-it-codigo-emen1 LABEL "Item"
     at row 7.7 col 35 colon-aligned
     view-as fill-in 
     size 17 by .88
     font 1

    c-desc-item-emen1 NO-LABEL
     at row 7.7 col 55 colon-aligned
     view-as fill-in 
     size 25 by .88
     font 1

    c-emenda-2 LABEL "Emenda 2"
     at row 8.7 col 14 colon-aligned
     view-as fill-in 
     size 16 by .88
     font 1
   
    c-it-codigo-emen2 LABEL "Item"
     at row 8.7 col 35 colon-aligned
     view-as fill-in 
     size 17 by .88
     font 1

    c-desc-item-emen2 NO-LABEL
     at row 8.7 col 55 colon-aligned
     view-as fill-in 
     size 25 by .88
     font 1

    d-saldo-bob LABEL "Saldo" 
     at row 7.7 col 88 colon-aligned
     view-as fill-in 
     size 17 by .88
     font 1
 
    rs-opcao LABEL "Opá∆o" AT ROW 10.7 COL 20

    bt-setup AT ROW 10.7 COL 80

    i-nr-doff LABEL "Nr.Doff"
     at row 12.7 col 14 colon-aligned
     view-as fill-in 
     size 06 by .88
     font 1

    dt-trans-ini LABEL "In°cio"
     at row 12.7 col 35 colon-aligned
     view-as fill-in 
     size 11 by .88
     font 1

    c-hr-inic-prod NO-LABEL 
     at row 12.7 col 50 colon-aligned
     view-as fill-in 
     size 10 by .88
     font 1

    dt-trans-fim LABEL "TÇrmino"
     at row 12.7 col 70 colon-aligned
     view-as fill-in 
     size 11 by .88
     font 1

    c-hr-fim-prod NO-LABEL 
     at row 12.7 col 85 colon-aligned
     view-as fill-in 
     size 10 by .88
     font 1

    
   
    i-estacao LABEL "Estaá∆o"
     at row 14.7 col 14 colon-aligned
     view-as fill-in 
     size 04 by .88
     font 1

    i-nr-ord-produ LABEL "Ord.Produá∆o"
     at row 14.7 col 30 colon-aligned
     view-as fill-in 
     size 17 by .88
     font 1
   
    c-it-codigo-prod LABEL "Item"
     at row 14.7 col 53 colon-aligned
     view-as fill-in 
     size 17 by .88
     font 1

    i-nr-pedido LABEL "Pedido"
     at row 14.7 col 77 colon-aligned
     view-as fill-in 
     size 10 by .88
     font 1

    i-seq-ped LABEL "Seq"
     at row 14.7 col 92 colon-aligned
     view-as fill-in 
     size 06 by .88
     font 1

    rs-tipo LABEL "Tipo" AT ROW 16.7 COL 20

    c-lote-bobina LABEL "Nr.Bobina"
     at row 18.3 col 14 colon-aligned
     view-as fill-in 
     size 15 by .88
     font 1
    
    d-peso-liquido LABEL "Peso L°quido"
     at row 19.7 col 14 colon-aligned
     view-as fill-in 
     size 15 by .88
     font 1

    d-peso-bal LABEL "Peso &Balanáa" 
    HELP "Posicione a bobina na Balanáa e confirme o Peso"
    
     at row 19.7 col 14.1 colon-aligned
     view-as fill-in 
     size .9 by .88
     font 1
     

    i-turno LABEL "Turno"
     at row 21.3 col 14 colon-aligned
     view-as fill-in 
     size 15 by .88
     font 1

    i-maquina LABEL "M†quina"
     at row 22.3 col 14 colon-aligned
     view-as fill-in 
     size 15 by .88
     font 1

    c-turma LABEL "Turma"
     at row 23.3 col 14 colon-aligned
     view-as fill-in 
     size 15 by .88
     font 1

    i-largura LABEL "Largura"
     at row 24.3 col 14 colon-aligned
     view-as fill-in 
     size 15 by .88
     font 1

    i-diin LABEL "D.Interno"
     at row 21.3 col 48 colon-aligned
     view-as fill-in 
     size 15 by .88
     font 1

    i-diex LABEL "D.Externo"
     at row 22.3 col 48 colon-aligned
     view-as fill-in 
     size 15 by .88
     font 1

    i-espessura LABEL "Espessura"
     at row 23.3 col 48 colon-aligned
     view-as fill-in 
     size 15 by .88
     font 1

    i-compr LABEL "Comprimento"
     at row 24.3 col 48 colon-aligned
     view-as fill-in 
     size 15 by .88
     font 1

    i-larg-util LABEL "Largura Ètil"
     at row 21.3 col 75 colon-aligned
     view-as fill-in 
     size 10 by .88
     font 1


    i-emendas LABEL "Nr.Emendas"
     at row 21.3 col 97 colon-aligned
     view-as fill-in 
     size 10 by .88
     font 1    
    
    
   

    i-dureza-e LABEL "Dureza (E)"
     at row 22.3 col 75 colon-aligned
     view-as fill-in 
     size 07 by .88
     font 1

    i-dureza-c LABEL "(C)"
     at row 22.3 col 88 colon-aligned
     view-as fill-in 
     size 07 by .88
     font 1

    i-dureza-d LABEL "(D)"
     at row 22.3 col 100 colon-aligned
     view-as fill-in 
     size 07 by .88
     font 1
           
     i-delta LABEL "Delta RHO"
     at row 23.3 col 75 colon-aligned
     view-as fill-in 
     size 10 by .88
     font 1

    bt-defpri AT ROW 24.4 COL 72
    bt-defsec AT ROW 24.4 COL 92

    bt-atualiza  AT ROW 26 COL 4
    bt-cancela2  AT ROW 26 COL 20

    RECT-1  AT ROW 1.05 COL 2    
    RECT-9  AT ROW 2.55 COL 2    
    RECT-10 AT ROW 6.1  COL 2  
    RECT-11 AT ROW 20.9 COL 2  
    RECT-8  AT ROW 10.5 COL 16.5
    RECT-7  AT ROW 16.4 COL 16.5

    rt-botoes AT ROW 25.8 COL 2

    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
    SIDE-LABELS NO-UNDERLINE THREE-D
    AT COL 1 ROW 1 FONT 1 
    SIZE 110 BY 26.3. 

/* ******** Acerto da posiá∆o dos labels e tamanho dos radio-set ******* */

/* *************************  Create Window  ************************** */

CREATE WINDOW C-Win ASSIGN
   HIDDEN             = YES
   TITLE              = "Reporte de Produá∆o no Corte/Recorte/Metalizaá∆o"
   HEIGHT             = 27
   WIDTH              = 111
   MAX-HEIGHT         = 27
   MAX-WIDTH          = 111
   VIRTUAL-HEIGHT     = 27
   VIRTUAL-WIDTH      = 111
   RESIZE             = yes
   SCROLL-BARS        = no
   STATUS-AREA        = yes
   BGCOLOR            = ?
   FGCOLOR            = ?
   KEEP-FRAME-Z-ORDER = yes
   THREE-D            = yes
   MESSAGE-AREA       = no
   SENSITIVE          = yes.

/* ***************  Runtime Attributes and UIB Settings  ************** */


IF SESSION:DISPLAY-TYPE = "GUI" AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* ************************* Included-Libraries *********************** */


define variable wh-pesquisa             as handle               no-undo.
define variable c-arq-old               as char                 no-undo.

def new shared var c-nom-prog-upc-mg97  as char format "x(50)" no-undo.
def new shared var c-nom-prog-appc-mg97 as char format "x(50)" no-undo.
def new shared var c-nom-prog-dpc-mg97  as char format "x(50)" no-undo.
def new shared var c-programa-mg97      as char format "x(08)" no-undo.
def new shared var c-versao-mg97        as char format "x(08)" no-undo.

define new shared variable c-imp-old               as char                 no-undo.
define new shared variable c-arq-old-batch         as char                 no-undo.


PROCEDURE pi-trata-state :
  DEFINE INPUT PARAMETER p-issuer-hdl   AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state        AS CHAR   NO-UNDO.
  
  case entry(1, p-state, '|'):
  end.
END PROCEDURE.

on "close" of this-procedure do:
    run disable_UI.
    apply 'choose' TO bt-sai IN FRAME f-relat.

end.

/* ************************  Control Triggers  ************************ */


ON F5 OF c-cod-ctrab IN FRAME f-relat /* Centro de Trabalho */
DO:

      if  valid-handle(wh-pesquisa) and
          wh-pesquisa:TYPE = "PROCEDURE":U and
          wh-pesquisa:FILE-NAME = "inzoom/z01in513.w":U then
            return.

      RUN inzoom/z01in513.w persistent set wh-pesquisa.

      if  not valid-handle(wh-pesquisa) or
              wh-pesquisa:TYPE <> "PROCEDURE":U or
              wh-pesquisa:FILE-NAME <> "inzoom/z01in513.w":U then
          return.

      RUN dispatch IN wh-pesquisa ('initialize':U).

      if valid-handle(wh-pesquisa) and
         wh-pesquisa:TYPE = "PROCEDURE":U and
         wh-pesquisa:FILE-NAME = "inzoom/z01in513.w":U then do:

            RUN pi-entry IN wh-pesquisa.

            define variable c-lista-campo as char init '' no-undo.

            assign c-lista-campo = string(c-cod-ctrab:handle in frame
                   f-relat) + '|':U + 'cod-ctrab' .

            run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).


      end.

    assign l-implanta = no.

END.


ON MOUSE-SELECT-DBLCLICK OF c-cod-ctrab IN FRAME f-relat /* Centro de Trabalho */
DO:
  APPLY "f5" TO SELF.
END.



ON F5 OF c-cod-operador IN FRAME f-relat /* Operador */
DO:

      if  valid-handle(wh-pesquisa) and
          wh-pesquisa:TYPE = "PROCEDURE":U and
          wh-pesquisa:FILE-NAME = "inzoom/z01in518.w":U then
            return.

      RUN inzoom/z01in518.w persistent set wh-pesquisa.

      if  not valid-handle(wh-pesquisa) or
              wh-pesquisa:TYPE <> "PROCEDURE":U or
              wh-pesquisa:FILE-NAME <> "inzoom/z01in518.w":U then
          return.

      RUN dispatch IN wh-pesquisa ('initialize':U).

      if valid-handle(wh-pesquisa) and
         wh-pesquisa:TYPE = "PROCEDURE":U and
         wh-pesquisa:FILE-NAME = "inzoom/z01in518.w":U then do:

            RUN pi-entry IN wh-pesquisa.

            define variable c-lista-campo as char init '' no-undo.

            assign c-lista-campo = string(c-cod-operador:handle in frame
                   f-relat) + '|':U + 'cod-operador' .

            run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).


      end.

    assign l-implanta = no.

END.


ON MOUSE-SELECT-DBLCLICK OF c-cod-operador IN FRAME f-relat /* Operador */
DO:
  APPLY "f5" TO SELF.
END.

ON F5 OF i-nr-ord-produ IN FRAME f-relat /* Ordem de Produá∆o */
DO:

      if  valid-handle(wh-pesquisa) and
          wh-pesquisa:TYPE = "PROCEDURE":U and
          wh-pesquisa:FILE-NAME = "inzoom/z01in271.w":U then
            return.

      RUN inzoom/z01in271.w persistent set wh-pesquisa.

      if  not valid-handle(wh-pesquisa) or
              wh-pesquisa:TYPE <> "PROCEDURE":U or
              wh-pesquisa:FILE-NAME <> "inzoom/z01in271.w":U then
          return.

      RUN dispatch IN wh-pesquisa ('initialize':U).

      if valid-handle(wh-pesquisa) and
         wh-pesquisa:TYPE = "PROCEDURE":U and
         wh-pesquisa:FILE-NAME = "inzoom/z01in271.w":U then do:

            RUN pi-entry IN wh-pesquisa.

            define variable c-lista-campo as char init '' no-undo.

            assign c-lista-campo = string(i-nr-ord-produ:handle in frame
                   f-relat) + '|':U + 'nr-ord-produ' .

            run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).


      end.

    assign l-implanta = no.

END.


ON MOUSE-SELECT-DBLCLICK OF i-nr-ord-produ IN FRAME f-relat /* Ordem de Produá∆o */
DO:
  APPLY "f5" TO SELF.
END.


ON F5 OF c-cod-estabel IN FRAME f-relat /* Estabelecimento */
DO:

      if  valid-handle(wh-pesquisa) and
          wh-pesquisa:TYPE = "PROCEDURE":U and
          wh-pesquisa:FILE-NAME = "adzoom/z01ad107.w":U then
            return.

      RUN adzoom/z01ad107.w persistent set wh-pesquisa.

      if  not valid-handle(wh-pesquisa) or
              wh-pesquisa:TYPE <> "PROCEDURE":U or
              wh-pesquisa:FILE-NAME <> "adzoom/z01ad107.w":U then
          return.

      RUN dispatch IN wh-pesquisa ('initialize':U).

      if valid-handle(wh-pesquisa) and
         wh-pesquisa:TYPE = "PROCEDURE":U and
         wh-pesquisa:FILE-NAME = "adzoom/z01ad107.w":U then do:

            RUN pi-entry IN wh-pesquisa.

            define variable c-lista-campo as char init '' no-undo.

            assign c-lista-campo = string(c-cod-estabel:handle in frame
                   f-relat) + '|':U + 'cod-estabel' .

            run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).


      end.

    assign l-implanta = no.

END.


ON MOUSE-SELECT-DBLCLICK OF c-cod-estabel IN FRAME f-relat /* Estabelecimento */
DO:
  APPLY "f5" TO SELF.
END.

on LEAVE OF c-lote-bobina in frame f-relat do:
    FIND FIRST lote-prod WHERE lote-prod.it-codigo = tt-estacao.it-codigo AND
        lote-prod.lote = c-lote-bobina:SCREEN-VALUE in frame f-relat NO-LOCK NO-ERROR.

    IF AVAIL lote-prod  THEN
      run utp/ut-msgs.p (input "show":U, input 17006, "Atená∆o, j† existe o Item " +  trim(tt-estacao.it-codigo)  + " com a Bobina " + c-lote-bobina:SCREEN-VALUE in frame f-relat + "~~Isto Ç apenas um alerta, siga em frente se tiver certeza." ).   
end. 

ON 'value-changed':U OF i-dureza-e in frame f-relat
DO:
    RUN pi-deltarho.
        
END.

ON 'value-changed':U OF i-dureza-c in frame f-relat
DO:
    RUN pi-deltarho.
        
END.

ON 'value-changed':U OF i-dureza-d in frame f-relat
DO:
    RUN pi-deltarho.
        
END.


on LEAVE OF c-cod-estabel in frame f-relat do:
    
   FIND FIRST estabelec WHERE
       estabelec.cod-estabel = c-cod-estabel:SCREEN-VALUE IN FRAME f-relat
       NO-LOCK NO-ERROR.
   

   IF AVAIL estabelec THEN
      ASSIGN c-nome-estab:SCREEN-VALUE IN FRAME f-relat = estabelec.nome.
   ELSE
      ASSIGN c-nome-estab:SCREEN-VALUE IN FRAME f-relat = "".


end. 

on VALUE-CHANGED OF rs-opcao in frame f-relat do:

    IF INPUT FRAME f-relat rs-opcao = 2 THEN  do:
        ASSIGN 
               D-PESO-liquido:Screen-value IN FRAME f-relat = ""
               i-nr-doff:SCREEN-VALUE IN FRAME f-relat     = "0"
               c-lote-bobina:SCREEN-VALUE IN FRAME f-relat = "RECICL"
               i-nr-doff:SENSITIVE IN FRAME f-relat     = NO
               c-lote-bobina:SENSITIVE IN FRAME f-relat = NO.
                
                     ASSIGN 
                         btpeso:SENSITIVE IN FRAME f-relat         = NO
                         D-PESO-liquido:SENSITIVE IN FRAME f-relat = YES
                         D-PESO-liquido:col IN FRAME f-relat = 16
                         D-PESO-bal:SENSITIVE IN FRAME f-relat     = NO
                         D-PESO-bal:VISIBLE IN FRAME f-relat     = no.

               
    end.               
    ELSE  do:
    
         ASSIGN 
               D-PESO-liquido:Screen-value IN FRAME f-relat = ""
               c-lote-bobina:SCREEN-VALUE IN FRAME f-relat = ""
               i-nr-doff:SENSITIVE IN FRAME f-relat     = YES
               c-lote-bobina:SENSITIVE IN FRAME f-relat = YES.
 
                IF L-PESO-BAL = yes THEN DO :
                      ASSIGN 
                          btpeso:SENSITIVE IN FRAME f-relat         = NO
                          D-PESO-liquido:SENSITIVE IN FRAME f-relat = YES
                          D-PESO-liquido:col IN FRAME f-relat = 16
                          D-PESO-bal:SENSITIVE IN FRAME f-relat     = NO
                          D-PESO-bal:VISIBLE IN FRAME f-relat     = no
                             .
                end.
                else do:
                      ASSIGN 
                          btpeso:SENSITIVE IN FRAME f-relat         = yes
                          D-PESO-liquido:SENSITIVE IN FRAME f-relat = no
                          D-PESO-liquido:col IN FRAME f-relat = 17
                          D-PESO-bal:SENSITIVE IN FRAME f-relat     = yes
                          D-PESO-bal:VISIBLE IN FRAME f-relat     = yes.

                
                end.
               
               end.
               
               
               


END.

on VALUE-CHANGED OF rs-tipo in frame f-relat do:
    APPLY "leave" TO i-estacao in frame f-relat.
    
END.


    
on LEAVE OF d-peso-liquido in frame f-relat do:

    IF DEC(d-peso-liquido:SCREEN-VALUE IN FRAME f-relat) >
       DEC(d-saldo-bob:SCREEN-VALUE IN FRAME f-relat) THEN DO:
    
       run utp/ut-msgs.p (input "show":U, input 17006, "Sem Saldo Suficiente para Consumo").
       RETURN NO-APPLY.  

    END.

end. 


ON "entry" OF d-peso-liquido in frame f-relat do:
   
 
 
   do  on error undo, return no-apply:
        d-peso-liquido:SCREEN-VALUE IN FRAME f-relat = "".
         
       RUN pi-peso.
   end.

END.



ON VALUE-CHANGED OF d-peso-bal IN frame f-relat
DO:
    ASSIGN c-lido = D-PESO-BAL:SCREEN-VALUE IN frame f-relat.
    IF SUBSTRING(c-lido,1,1) = chr(216) AND 
       SUBSTRING(c-lido,LENGTH(trim(c-lido)),1)=chr(216) and
      LENGTH(trim(c-lido)) <> 1 THEN DO:
               ASSIGN D-PESO-LIQUIDO:SCREEN-VALUE in frame f-relat = 
                   SUBSTRING(c-lido,2,LENGTH(TRIM(c-lido)) - 2) NO-ERROR.
               apply "leave":U to d-peso-liquido in frame f-relat. 
    END.
      
    /*MESSAGE tt-aux.peso-bal:SCREEN-VALUE IN BROWSE brlote
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    RETURN.
END.



on LEAVE OF c-cod-ctrab in frame f-relat do:
    
   FIND FIRST ctrab WHERE
       ctrab.cod-ctrab = c-cod-ctrab:SCREEN-VALUE IN FRAME f-relat
       NO-LOCK NO-ERROR.
   

   IF AVAIL ctrab THEN
      ASSIGN c-desc-ctrab:SCREEN-VALUE IN FRAME f-relat = des-ctrab.
   ELSE
      ASSIGN c-desc-ctrab:SCREEN-VALUE IN FRAME f-relat = "".


end. 


on LEAVE OF c-cod-operador in frame f-relat do:

   FIND FIRST operador WHERE
       operador.cod-operador = c-cod-operador:SCREEN-VALUE IN FRAME f-relat
       NO-LOCK NO-ERROR.
    

   IF AVAIL operador THEN
      ASSIGN c-nome-operador:SCREEN-VALUE IN FRAME f-relat = operador.nom-operador.
   ELSE
      ASSIGN c-nome-operador:SCREEN-VALUE IN FRAME f-relat = "".


end. 

ON LEAVE OF c-lote-cons IN FRAME f-relat /* Lote Consumo */
DO:
        
    ASSIGN saldo-lote-cons = 0.

    IF c-lote-cons:SCREEN-VALUE IN FRAME f-relat <> ""  AND
       (c-lote-cons:SCREEN-VALUE IN FRAME f-relat = c-emenda-1:SCREEN-VALUE IN FRAME f-relat OR 
        c-lote-cons:SCREEN-VALUE IN FRAME f-relat = c-emenda-2:SCREEN-VALUE IN FRAME f-relat) THEN DO:

       assign c-lote-cons:screen-value in frame f-relat      = ""
              c-it-codigo-cons:screen-value in frame f-relat = ""
              c-desc-item-cons:screen-value in frame f-relat = "".
    
       run utp/ut-msgs.p (input "show",
                          input 17006,
                          input "Lote Repetido").

       RETURN NO-APPLY.
    

    END.

    if self:screen-value <> "" then do:
        for first saldo-estoq fields (it-codigo) use-index lote no-lock where
                  saldo-estoq.lote        = self:screen-value and
                  saldo-estoq.qtidade-atu <> 0 AND
                  saldo-estoq.cod-estabel = c-cod-estabel:screen-value in frame f-relat AND
                     NOT (saldo-estoq.cod-depos  = "EXP" AND  (saldo-estoq.cod-estabel = "412" OR saldo-estoq.cod-estabel = "422")  /*solic-318*/ ):
        end.

        IF NOT AVAIL saldo-estoq THEN DO:

             FOR first saldo-estoq fields (it-codigo) use-index lote no-lock where
                  saldo-estoq.lote        = self:screen-value and
                  saldo-estoq.cod-estabel = c-cod-estabel:screen-value in frame f-relat AND
                  NOT (saldo-estoq.cod-depos  = "EXP" AND  (saldo-estoq.cod-estabel = "412" OR saldo-estoq.cod-estabel = "422")  /*solic-318*/ ): 
             end.

        END.

        if avail saldo-estoq then do:
        
            for first item fields (it-codigo desc-item) no-lock where
                      item.it-codigo = saldo-estoq.it-codigo:
            end.

            assign c-it-codigo-cons:screen-value      in frame f-relat = item.it-codigo
                   c-desc-item-cons:screen-value in frame f-relat = item.desc-item.

        end.

        else do:

            assign c-lote-cons:screen-value in frame f-relat      = ""
                   c-it-codigo-cons:screen-value in frame f-relat = ""
                   c-desc-item-cons:screen-value in frame f-relat = "".
    
            run utp/ut-msgs.p (input "show",
                               input 17006,
                               input "Lote n∆o encontrado").
             RETURN NO-APPLY.
        end.

    end.

    else
        assign c-lote-cons:screen-value in frame f-relat      = ""
               c-it-codigo-cons:screen-value in frame f-relat = ""       
               c-desc-item-cons:screen-value in frame f-relat = "".       
                                                              
  
    IF (c-it-codigo-cons:screen-value in frame f-relat <> "" AND  c-emenda-1:SCREEN-VALUE IN FRAME f-relat <> "" AND
       c-it-codigo-cons:screen-value in frame f-relat <> c-emenda-1:SCREEN-VALUE IN FRAME f-relat ) OR
        (c-it-codigo-cons:screen-value in frame f-relat <> "" AND  c-emenda-2:SCREEN-VALUE IN FRAME f-relat <> "" AND
       c-it-codigo-cons:screen-value in frame f-relat <> c-emenda-2:SCREEN-VALUE IN FRAME f-relat ) OR
        (c-emenda-1:screen-value in frame f-relat <> "" AND  c-emenda-2:SCREEN-VALUE IN FRAME f-relat <> "" AND
       c-emenda-1:screen-value in frame f-relat <> c-emenda-2:SCREEN-VALUE IN FRAME f-relat )   THEN DO:


         run utp/ut-msgs.p (input "show",
                               input 17006,
                               input "Item do Lote n∆o Incompativel").

         assign c-lote-cons:screen-value in frame f-relat      = ""
                c-it-codigo-cons:screen-value in frame f-relat = ""
                c-desc-item-cons:screen-value in frame f-relat = "".

             RETURN NO-APPLY.

    END.
    run CalculaQuantSaldo.

    FIND b-item WHERE b-item.it-codigo = c-it-codigo-cons:screen-value in frame f-relat NO-LOCK NO-ERROR. 

    IF AVAIL b-item THEN DO:

         find first lote-carac-tec
              where lote-carac-tec.it-codigo = c-it-codigo-cons:screen-value in frame f-relat
                and lote-carac-tec.lote      = c-lote-cons:screen-value in frame f-relat
                and lote-carac-tec.cd-folh   = b-item.cd-folh-lote
                and lote-carac-tec.cd-comp   = "ESPES" no-lock no-error.


         if  avail lote-carac-tec THEN  DO:

             find first comp-folh
                    where comp-folh.cd-folh = lote-carac-tec.cd-folh
                      and comp-folh.cd-comp = "ESPES" no-lock no-error.
            
               if avail lote-carac-tec and 
                  avail comp-folh      then do:
                  case lote-carac-tec.tipo-result:
                      when 1 then i-espessura:SCREEN-VALUE IN FRAME f-relat = string(lote-carac-tec.vl-result, comp-folh.formato).
                      otherwise i-espessura:SCREEN-VALUE IN FRAME f-relat = "".
                  end case.
               end.


         END.

    END.


        

END.



ON LEAVE OF c-emenda-1 IN FRAME f-relat /* Emenda 1 */
DO:

    ASSIGN saldo-emenda1-cons = 0.

    IF c-emenda-1:SCREEN-VALUE IN FRAME f-relat   <> "" AND
       (c-emenda-1:SCREEN-VALUE IN FRAME f-relat   = c-lote-cons:SCREEN-VALUE IN FRAME f-relat OR 
        c-emenda-1:SCREEN-VALUE IN FRAME f-relat   = c-emenda-2:SCREEN-VALUE IN FRAME f-relat) THEN DO:

        assign c-emenda-1:screen-value in frame f-relat        = ""
               c-it-codigo-emen1:screen-value in frame f-relat = ""
               c-desc-item-emen1:screen-value in frame f-relat = "".
    
       run utp/ut-msgs.p (input "show",
                          input 17006,
                          input "Lote Repetido").
    
                             RETURN NO-APPLY.
    END.

    
    if self:screen-value <> "" then do:
    
        if not can-find (first saldo-estoq use-index lote where
                               saldo-estoq.lote        = self:screen-value and
                               saldo-estoq.it-codigo   = c-it-codigo-cons:screen-value in frame f-relat and
                               saldo-estoq.cod-estabel = c-cod-estabel:screen-value in frame f-relat) then do:
            
            run utp/ut-msgs.p (input "show",
                               input 17006,
                               input "Lote n∆o encontrado").
    
            assign c-emenda-1:screen-value in frame f-relat        = ""
                   c-it-codigo-emen1:screen-value in frame f-relat = ""
                   c-desc-item-emen1:screen-value in frame f-relat = "".

        end.

        else do:

            FOR first saldo-estoq fields (it-codigo) use-index lote no-lock where
                 saldo-estoq.lote        = self:screen-value and
                 saldo-estoq.cod-estabel = c-cod-estabel:screen-value in frame f-relat AND
                 NOT (saldo-estoq.cod-depos  = "EXP" AND  (saldo-estoq.cod-estabel = "412" OR saldo-estoq.cod-estabel = "422")  /*solic-318*/ ): 
            end.

            if avail saldo-estoq then do:

                FIND FIRST item  where
                     item.it-codigo = saldo-estoq.it-codigo
                     no-lock NO-ERROR.
                
                IF AVAIL ITEM THEN 
                    assign c-it-codigo-emen1:screen-value      in frame f-relat = item.it-codigo
                           c-desc-item-emen1:screen-value      in frame f-relat = item.desc-item.
                ELSE
                    assign c-it-codigo-emen1:screen-value      in frame f-relat = ""
                           c-desc-item-emen1:screen-value      in frame f-relat = "".

            end.
        
        end.

    end.

    ELSE
        assign c-emenda-1:screen-value in frame f-relat        = ""
               c-it-codigo-emen1:screen-value in frame f-relat = ""
               c-desc-item-emen1:screen-value in frame f-relat = "".


     



    run CalculaQuantSaldo.
    

END.


ON LEAVE OF c-emenda-2 IN FRAME f-relat /* Emenda 2 */
DO:

    ASSIGN saldo-emenda2-cons = 0.

    IF  c-emenda-2:SCREEN-VALUE IN FRAME f-relat <> "" AND
        (c-emenda-2:SCREEN-VALUE IN FRAME f-relat   = c-lote-cons:SCREEN-VALUE IN FRAME f-relat OR 
         c-emenda-2:SCREEN-VALUE IN FRAME f-relat   = c-emenda-1:SCREEN-VALUE IN FRAME f-relat)  THEN DO:

        assign c-emenda-2:screen-value in frame f-relat        = ""
               c-it-codigo-emen2:screen-value in frame f-relat = ""
               c-desc-item-emen2:screen-value in frame f-relat = "".
    
       run utp/ut-msgs.p (input "show",
                          input 17006,
                          input "Lote Repetido").
    

    END.
    
    if self:screen-value <> "" then do:
    
        if not can-find (first saldo-estoq use-index lote where
                               saldo-estoq.lote        = self:screen-value and
                               saldo-estoq.it-codigo   = c-it-codigo-cons:screen-value in frame f-relat and
                               saldo-estoq.cod-estabel = c-cod-estabel:screen-value in frame f-relat) then do:
            
            run utp/ut-msgs.p (input "show",
                               input 17006,
                               input "Lote n∆o encontrado").
    
            assign c-emenda-2:screen-value in frame f-relat        = ""
                   c-it-codigo-emen2:screen-value in frame f-relat = ""
                   c-desc-item-emen2:screen-value in frame f-relat = "".

        end.

        else do:

            FOR first saldo-estoq fields (it-codigo) use-index lote no-lock where
                 saldo-estoq.lote        = self:screen-value and
                 saldo-estoq.cod-estabel = c-cod-estabel:screen-value in frame f-relat AND
                 NOT (saldo-estoq.cod-depos  = "EXP" AND  (saldo-estoq.cod-estabel = "412" OR saldo-estoq.cod-estabel = "422")  /*solic-318*/ ): 
            end.

            if avail saldo-estoq then do:

                FIND FIRST item  where
                     item.it-codigo = saldo-estoq.it-codigo
                     no-lock NO-ERROR.

                IF AVAIL ITEM THEN 
                    assign c-it-codigo-emen2:screen-value      in frame f-relat = item.it-codigo
                           c-desc-item-emen2:screen-value      in frame f-relat = item.desc-item.
                ELSE
                    assign c-it-codigo-emen2:screen-value      in frame f-relat = ""
                           c-desc-item-emen2:screen-value      in frame f-relat = "".

            end.
        
        end.

    end.

    ELSE
        assign c-emenda-2:screen-value in frame f-relat        = ""
               c-it-codigo-emen2:screen-value in frame f-relat = ""
               c-desc-item-emen2:screen-value in frame f-relat = "".

    run CalculaQuantSaldo.

END.

ON LEAVE OF c-hr-inic-prod IN FRAME f-relat /* Hora In°cio */
DO:

    ASSIGN c-hr-inic-prod = c-hr-inic-prod:SCREEN-VALUE IN FRAME f-relat.

    IF (int(SUBSTRING(c-hr-inic-prod,1,2))) < 6 THEN
        ASSIGN i-turno:SCREEN-VALUE IN FRAME f-relat = "2".
    ELSE
        IF (int(SUBSTRING(c-hr-inic-prod,1,2))) < 18 THEN
            ASSIGN i-turno:SCREEN-VALUE IN FRAME f-relat = "1".
        ELSE
            ASSIGN i-turno:SCREEN-VALUE IN FRAME f-relat = "2". 

END.


ON CHOOSE OF bt-defpri IN FRAME f-relat /* Defeito Prim†rio */
DO:
IF INPUT FRAME f-relat rs-tipo = 1 THEN return no-apply.

     c-it-codigo-prod = c-it-codigo-prod:SCREEN-VALUE IN FRAME f-relat.

  FIND FIRST item where
       item.it-codigo = c-it-codigo-prod
       NO-LOCK NO-ERROR.

    IF NOT AVAIL item THEN do:
    
            run utp/ut-msgs.p (input "show":U, input 2, "Item").
            APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
            RETURN NO-APPLY.
    
    END.

    IF ITEM.ge-codigo = 45 or item.ge-codigo = 46   THEN return no-apply.



    FOR EACH tt-defeito.
        DELETE tt-defeito.
    END.

    FOR EACH tt-defpri.

        CREATE tt-defeito.

        ASSIGN tt-defeito.nr-tabela = tt-defpri.nr-tabela
               tt-defeito.cod-def   = tt-defpri.cod-def  
               tt-defeito.descricao = tt-defpri.descricao.

    END.

    FOR EACH tt-defpri.
        DELETE tt-defpri.
    END.

    ASSIGN tp-defeito    = "PRI"
           nr-bobina-def = c-lote-bobina:SCREEN-VALUE IN FRAME f-relat.

    RUN sfc/essf0023.w (INPUT-OUTPUT table tt-defeito,
                             INPUT tp-defeito,
                             INPUT nr-bobina-def).

    FOR EACH tt-defeito.

        CREATE tt-defpri.

        ASSIGN tt-defpri.nr-tabela = tt-defeito.nr-tabela
               tt-defpri.cod-def   = tt-defeito.cod-def  
               tt-defpri.descricao = tt-defeito.descricao.
    END.


END.
    

ON CHOOSE OF bt-defsec IN FRAME f-relat /* Defeito Secund†rio */
DO:


    FOR EACH tt-defeito.
        DELETE tt-defeito.
    END.

    FOR EACH tt-defsec.

        CREATE tt-defeito.

        ASSIGN tt-defeito.nr-tabela = tt-defsec.nr-tabela
               tt-defeito.cod-def   = tt-defsec.cod-def  
               tt-defeito.descricao = tt-defsec.descricao.

    END.

    FOR EACH tt-defsec.
        DELETE tt-defsec.
    END.

    ASSIGN tp-defeito = "SEC"
           nr-bobina-def = c-lote-bobina:SCREEN-VALUE IN FRAME f-relat.

    RUN sfc/essf0023.w (INPUT-OUTPUT table tt-defeito,
                             INPUT tp-defeito,
                             INPUT nr-bobina-def).

    FOR EACH tt-defeito.

        CREATE tt-defsec.

        ASSIGN tt-defsec.nr-tabela = tt-defeito.nr-tabela
               tt-defsec.cod-def   = tt-defeito.cod-def  
               tt-defsec.descricao = tt-defeito.descricao.
    END.


END.
    
ON CHOOSE OF bt-setup IN FRAME f-relat /* Copia Estaá∆o */
DO:


      ASSIGN c-it-codigo-reserva-jr = c-it-codigo-cons:screen-value in frame f-relat.

    RUN sfc/essf0021.w.

    FOR EACH tt-estacao.
        DELETE tt-estacao.
    END.
    
    c-temp-jr = session:temp-directory.


    ASSIGN c-arquivo-jr = search(c-temp-jr + "essf0021.txt"). 
    
    IF c-arquivo-jr <> ? then do:

        INPUT FROM VALUE(c-arquivo-jr). 

    REPEAT:

       IMPORT UNFORMATTED text-string.

       IF INT(SUBSTRING(text-string,1,9)) = 0 THEN DO:

           CREATE tt-estacao.

           ASSIGN tt-estacao.estacao         = INT(SUBSTRING(text-string,1,9))    
                  tt-estacao.nr-ord-off-spc  = INT(SUBSTRING(text-string,10,9))    
                  tt-estacao.nr-ord-off-grd  = INT(SUBSTRING(text-string,20,9))    
                  tt-estacao.letra-bobina    = SUBSTRING(text-string,30,1)         
                  tt-estacao.nr-prim-bob     = INT(SUBSTRING(text-string,40,9)).

       END.

       ELSE DO:

          CREATE tt-estacao.
          
          ASSIGN tt-estacao.estacao         = INT(SUBSTRING(text-string,1,9))
                 tt-estacao.nr-ord-produ    = INT(SUBSTRING(text-string,10,9))
                 tt-estacao.it-codigo       = SUBSTRING(text-string,20,18)
                 tt-estacao.nr-pedido       = int(SUBSTRING(text-string,40,9))     
                 tt-estacao.seq-ped         = INT(SUBSTRING(text-string,50,9))
                 tt-estacao.nome-abrev      = SUBSTRING(text-string,60,15)
                 tt-estacao.cod-depos       = SUBSTRING(text-string,80,9)
                 tt-estacao.cod-localiz     = SUBSTRING(text-string,90,10)
                 tt-estacao.gera-pallet     = SUBSTRING(text-string,100,3)
                 tt-estacao.lote            = SUBSTRING(text-string,103,10). 

       END.

    END.
    
    end.

END.

on LEAVE OF i-estacao in frame f-relat do:

    IF INPUT FRAME f-relat rs-tipo = 1 THEN DO:

        FIND FIRST tt-estacao WHERE
            tt-estacao.estacao = INPUT FRAME f-relat i-estacao
            NO-LOCK NO-ERROR.
    
        IF NOT AVAIL tt-estacao OR tt-estacao.estacao < 1 or
            tt-estacao.nr-ord-produ = 0 THEN DO:
    
           run utp/ut-msgs.p (input "show":U, input 17006, "Estaá∆o N∆o Prevista no Setup").
           
           find first tt-estacao no-lock no-error.
           if not avail tt-estacao then
              apply "choose" to bt-setup in frame f-relat.
           else           
           RETURN NO-APPLY.  
    
        END.

        ASSIGN i-nr-ord-produ:SCREEN-VALUE IN FRAME f-relat = string(tt-estacao.nr-ord-produ)
               i-nr-pedido:SCREEN-VALUE IN FRAME f-relat    = string(tt-estacao.nr-pedido)
               i-seq-ped:SCREEN-VALUE IN FRAME f-relat      = string(tt-estacao.seq-ped).

    END.

    ELSE DO:

        FIND FIRST tt-estacao WHERE
            tt-estacao.estacao = 0
            NO-LOCK NO-ERROR.
    
        IF NOT AVAIL tt-estacao THEN DO:
    
           run utp/ut-msgs.p (input "show":U, input 17006, "Falta incluir parÉmetros de Setup").
           RETURN NO-APPLY.  
    
        END.

        IF INPUT FRAME f-relat rs-tipo = 2 THEN
           ASSIGN i-nr-ord-produ:SCREEN-VALUE IN FRAME f-relat = string(tt-estacao.nr-ord-off-spc)
                  i-nr-pedido:SCREEN-VALUE IN FRAME f-relat    = string(tt-estacao.nr-pedido)
                  i-seq-ped:SCREEN-VALUE IN FRAME f-relat      = string(tt-estacao.seq-ped).
            
        ELSE
            ASSIGN i-nr-ord-produ:SCREEN-VALUE IN FRAME f-relat = string(tt-estacao.nr-ord-off-grd)
                   i-nr-pedido:SCREEN-VALUE IN FRAME f-relat    = string(tt-estacao.nr-pedido)
                   i-seq-ped:SCREEN-VALUE IN FRAME f-relat      = string(tt-estacao.seq-ped).

    END.

    IF INPUT FRAME f-relat rs-opcao <> 2 THEN DO:

        FIND FIRST tt-estacao WHERE
            tt-estacao.estacao = INPUT FRAME f-relat i-estacao
            NO-LOCK NO-ERROR.
        
        IF AVAIL tt-estacao THEN 
           ASSIGN c-lote-bobina:SCREEN-VALUE IN FRAME f-relat  = tt-estacao.lote.

    END.

    ELSE DO:    /* Perdas */

        ASSIGN c-lote-bobina:SCREEN-VALUE IN FRAME f-relat  = "RECICL".

    END.


    APPLY 'leave' TO i-nr-ord-produ in frame f-relat.

END.


on LEAVE OF i-nr-ord-produ in frame f-relat do:
 
    FIND FIRST ord-prod WHERE
        ord-prod.nr-ord-produ = INPUT FRAME f-relat i-nr-ord-produ
        NO-LOCK NO-ERROR.

    IF AVAIL ord-prod THEN DO:

      IF INPUT FRAME f-relat rs-tipo = 1 THEN
            RUN pi-especificacao.


        FIND FIRST ITEM WHERE
            ITEM.it-codigo = ord-prod.it-codigo
            NO-LOCK NO-ERROR.

        IF AVAIL ITEM THEN 
           ASSIGN c-it-codigo-prod:SCREEN-VALUE IN FRAME f-relat = ord-prod.it-codigo.
        ELSE
           ASSIGN c-it-codigo-prod:SCREEN-VALUE IN FRAME f-relat = "".
           
        FIND FIRST cot-est-mast
            WHERE cot-est-mast.item-cotacao = ord-prod.it-codigo 
              AND cot-est-mast.nr-estrut    = ord-prod.nr-estrut  and
              cot-est-mast.nr-estrut <> 1

              NO-LOCK NO-ERROR.
        IF NOT AVAIL cot-est-mast THEN
         FIND FIRST cot-est-mast
            WHERE cot-est-mast.item-cotacao = ord-prod.it-codigo 
              AND cot-est-mast.nr-estrut    = int(ord-prod.cod-refer)
              NO-LOCK NO-ERROR.
        
        IF AVAIL cot-est-mast THEN DO:
        
            FIND var-result 
                WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                  AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                  AND var-result.nome-var     = "LARGURA"  NO-LOCK NO-ERROR.
        
            IF AVAIL var-result then

                ASSIGN i-largura:SCREEN-VALUE IN FRAME f-relat = 
                   STRING(int(var-result.des-result)).
 
            FIND var-result 
                WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                  AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                  AND var-result.nome-var     = "DIIN"  NO-LOCK NO-ERROR.
        
            IF AVAIL var-result then

                ASSIGN i-diin:SCREEN-VALUE IN FRAME f-relat = 
                   STRING(int(var-result.des-result)).

            FIND var-result 
                WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                  AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                  AND var-result.nome-var     = "DIEX"  NO-LOCK NO-ERROR.
        
            IF AVAIL var-result then

                ASSIGN i-diex:SCREEN-VALUE IN FRAME f-relat = 
                   STRING(int(var-result.des-result)).

        END.

        ASSIGN i-largura:SENSITIVE IN FRAME f-relat= yes
               i-diin:Sensitive IN FRAME f-relat = yes.
               i-diex:Sensitive IN FRAME f-relat = yes.

        IF AVAIL ITEM AND ITEM.ge-codigo = 47 THEN DO:
            ASSIGN  rs-tipo:SCREEN-VALUE in FRAME f-relat = "2".
            IF INDEX(ITEM.it-codigo,"OG") > 0 THEN
                 ASSIGN  rs-tipo:SCREEN-VALUE in FRAME f-relat = "3".

            ASSIGN i-largura:SENSITIVE IN FRAME f-relat= YES
                   i-diin:Sensitive IN FRAME f-relat = YES.

        END.
        

    END.

    ELSE
        ASSIGN i-nr-ord-produ:SCREEN-VALUE IN FRAME f-relat   = ""
               c-it-codigo-prod:SCREEN-VALUE IN FRAME f-relat = "".


END.                                                


on "entry" OF i-estacao in frame f-relat do:


END.


ON CHOOSE OF bt-cancela2 IN FRAME f-relat
DO:


   RUN pi-disable-campos.
   RUN pi-disable-bt-grava.
   RUN pi-enable-outros-botoes.
   RUN pi-limpa-campos.
   
   APPLY "choose" TO bt-sai IN FRAME f-relat.

END.


ON CHOOSE OF bt-atualiza IN FRAME f-relat
DO:

     IF DEC(i-dureza-e:SCREEN-VALUE IN FRAME f-relat) <= 0 OR
       DEC(i-dureza-c:SCREEN-VALUE IN FRAME f-relat) <= 0 OR
       DEC(i-dureza-d:SCREEN-VALUE IN FRAME f-relat) <= 0  THEN DO:
    
            run utp/ut-msgs.p (input "show":U, input 17006, "FALTA INFORMAR ALGUMA DUREZA~~POR FAVOR INFORME AS 3 DUREZAS").
               APPLY "entry" TO i-dureza-e IN FRAME f-relat.
               RETURN NO-APPLY.  

    END.

    c-emenda-ant = i-emendas:SCREEN-VALUE IN FRAME f-relat. /*salva qtde emenda caso alterado manualmente.*/
    
    RUN CalculaQuantSaldo.
    
    i-emendas:SCREEN-VALUE IN FRAME f-relat =     c-emenda-ant. /* volta quantidade emenda caso alterou manualmente*/


    FIND FIRST tt-estacao WHERE
        tt-estacao.estacao = INPUT FRAME f-relat i-estacao
        NO-LOCK NO-ERROR.

    IF NOT AVAIL tt-estacao OR tt-estacao.estacao < 1 or
        tt-estacao.nr-ord-produ = 0 THEN DO:

       run utp/ut-msgs.p (input "show":U, input 17006, "Estaá∆o N∆o Prevista no Setup").
       APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
       RETURN NO-APPLY.  

    END.

    FIND FIRST estabelec WHERE
        estabelec.cod-estabel = c-cod-estabel:SCREEN-VALUE IN FRAME f-relat
        NO-LOCK NO-ERROR.
    
    
    IF NOT AVAIL estabelec THEN DO:
    
       run utp/ut-msgs.p (input "show":U, input 17006, "Estabelecimento Errado").
       APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
       RETURN NO-APPLY.  

    END.
    
    FIND FIRST ctrab WHERE
        ctrab.cod-ctrab = c-cod-ctrab:SCREEN-VALUE IN FRAME f-relat
        NO-LOCK NO-ERROR.
    
    IF NOT AVAIL ctrab THEN DO:
    
        run utp/ut-msgs.p (input "show":U, input 17006, "Centro de Trabalho Errado").
        APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
        RETURN NO-APPLY.

    END.

    ASSIGN operador-jr = c-cod-operador:SCREEN-VALUE IN FRAME f-relat.

    FIND FIRST operador WHERE
        operador.cod-operador = operador-jr
        NO-LOCK NO-ERROR.

    IF NOT AVAIL operador THEN DO:

        run utp/ut-msgs.p (input "show":U, input 17006, "Operador de Produá∆o Errado").
        APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
        RETURN NO-APPLY.

    END.

    ASSIGN i-nr-ord-produ   = INT(i-nr-ord-produ:SCREEN-VALUE IN FRAME f-relat)
           c-it-codigo-prod = c-it-codigo-prod:SCREEN-VALUE IN FRAME f-relat.
    
    FIND FIRST ord-prod WHERE
        ord-prod.nr-ord-produ = i-nr-ord-produ    
        NO-LOCK NO-ERROR.
    
    IF NOT AVAIL ord-prod THEN DO:
    
       run utp/ut-msgs.p (input "show":U, input 2, "Ord.de Produá∆o").
       APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
       RETURN NO-APPLY.  

    END.


    IF ord-prod.estado > 6 THEN DO:
    
       run utp/ut-msgs.p (input "show":U, input 17006, "Ord.de Produá∆o J† Encerrado ou Terminada").
       APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
       RETURN NO-APPLY.  

    END.

    FIND FIRST item where
       item.it-codigo = c-it-codigo-prod
       NO-LOCK NO-ERROR.

    IF NOT AVAIL item THEN do:
    
            run utp/ut-msgs.p (input "show":U, input 2, "Item").
            APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
            RETURN NO-APPLY.
    
    END.

    IF ITEM.ge-codigo = 47  THEN DO:
        FIND FIRST tt-defpri NO-ERROR.
        IF NOT AVAIL tt-defpri THEN DO:

               run utp/ut-msgs.p (input "show":U, input 17006, "Item Off Spec/Off Cuts obrigat¢rio Defeito prim†rio").
               APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
               RETURN NO-APPLY.  


        END.
    END.


    FOR EACH tt-lote-cons NO-LOCK.

       FIND FIRST reservas WHERE
           reservas.nr-ord-produ = i-nr-ord-produ    AND
           reservas.it-codigo    = tt-lote-cons.it-codigo
           NO-LOCK NO-ERROR.
       
       IF NOT AVAIL reservas THEN DO:
       
            run utp/ut-msgs.p (input "show":U, input 17006, "Lote Consumido N∆o Previsto na Reserva da Ordem").
            APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
            return no-apply.  
       
       END.

    END.

    IF DEC(d-peso-liquido:SCREEN-VALUE IN FRAME f-relat) <= 0 THEN DO:
    
       run utp/ut-msgs.p (input "show":U, input 17006, "Peso L°quido Errado").
       APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
       RETURN NO-APPLY.  

    END.

    IF DEC(d-peso-liquido:SCREEN-VALUE IN FRAME f-relat) >
       DEC(d-saldo-bob:SCREEN-VALUE IN FRAME f-relat) THEN DO:
    
       run utp/ut-msgs.p (input "show":U, input 17006, "Sem Saldo Suficiente para Consumo").
       APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
       RETURN NO-APPLY.  

    END.
    

   


    ASSIGN dias-jr = DATE(dt-trans-fim:SCREEN-VALUE IN FRAME f-relat) -
                     DATE(dt-trans-ini:SCREEN-VALUE IN FRAME f-relat).

    IF dias-jr <> 0 AND dias-jr <> 1 THEN DO:
    
       run utp/ut-msgs.p (input "show":U, input 17006, "Erro da Data de Produá∆o").
       APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
       RETURN NO-APPLY.  

    END.

    ASSIGN c-hr-inic-prod = c-hr-inic-prod:SCREEN-VALUE IN FRAME f-relat
           c-hr-fim-prod  = c-hr-fim-prod:SCREEN-VALUE IN FRAME f-relat.

    IF (int(SUBSTRING(c-hr-inic-prod,1,2)) > 24 OR
        int(SUBSTRING(c-hr-inic-prod,4,2)) > 60)
           THEN DO: 
    
            run utp/ut-msgs.p (input "show":U, input 32582, "Hora de Produá∆o Errada").
            APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
            return no-apply.
    
    END.
    
    
    IF (int(SUBSTRING(c-hr-fim-prod,1,2)) > 24 OR
        int(SUBSTRING(c-hr-fim-prod,4,2)) > 60)
           THEN DO: 
    
            run utp/ut-msgs.p (input "show":U, input 32582, "Hora de Produá∆o Errada").
            APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
            return no-apply.
    
    END.

    ASSIGN horai-jr = int(SUBSTRING(c-hr-inic-prod,1,2)) +
                      DEC(int(SUBSTRING(c-hr-inic-prod,4,2))) / 100.

    ASSIGN horaf-jr = int(SUBSTRING(c-hr-fim-prod,1,2)) +
                      DEC(int(SUBSTRING(c-hr-fim-prod,4,2))) / 100.

    IF dias-jr = 1 THEN
        ASSIGN horaf-jr = horaf-jr + 24.

    IF (horaf-jr - horai-jr) < 0 OR
       (horaf-jr - horai-jr) >= 12  THEN DO:

        run utp/ut-msgs.p (input "show":U, input 32582, "Hora In°cio/Fim Errada").
        APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
        return no-apply.

    END.

    IF INPUT FRAME f-relat rs-opcao <> 2 THEN DO:

        IF INT(i-nr-doff:SCREEN-VALUE IN FRAME f-relat) = 0 OR
           INT(i-nr-doff:SCREEN-VALUE IN FRAME f-relat) > 100 THEN DO:
        
            run utp/ut-msgs.p (input "show":U, input 32582, "Nr.do Doff Errado").
            APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
            return no-apply.
        
        END.

    END.

    /* Geraá∆o de dados para integraá∆o no EMS */

    ASSIGN peso-jr = DEC(d-peso-liquido:SCREEN-VALUE IN FRAME f-relat).

    FOR EACH tt-lote-cons where
        tt-lote-cons.saldo > 0 AND 
        peso-jr > 0
        NO-LOCK.

        IF peso-jr <= tt-lote-cons.saldo THEN
            ASSIGN tt-lote-cons.consumo = peso-jr
                   peso-jr              = 0.

        ELSE
            ASSIGN tt-lote-cons.consumo = tt-lote-cons.saldo
                   peso-jr              = peso-jr - tt-lote-cons.consumo.

    END.

    FIND FIRST param-global.
find first param-cq no-lock no-error.
find first param-cp no-lock no-error.

RUN eliminaTT.

RUN criaTT.

IF RETURN-VALUE = "nok" THEN RETURN NO-APPLY.

run validaCampos .
if return-value = 'nok' then
    return no-apply.


 run processaReportes.
if return-value = 'nok' then
    return no-apply.

IF INPUT FRAME f-relat rs-tipo = 1 THEN
  RUN pi-gera-pallet.
if return-value = 'nok' then
    return no-apply.


    /*OUTPUT TO "v:\temp\essf0022.txt" APPEND.

    PUT "Estabelecimento: " c-cod-estabel:SCREEN-VALUE IN FRAME f-relat SKIP
        "C.Trabalho: " c-cod-ctrab:SCREEN-VALUE IN FRAME f-relat SKIP
        "Operador: " c-cod-operador:SCREEN-VALUE IN FRAME f-relat SKIP
        "Lote Consumido 1: " c-lote-cons:SCREEN-VALUE IN FRAME f-relat " Item: " c-it-codigo-cons:SCREEN-VALUE IN FRAME f-relat SKIP
        "Lote Consumido 2: " c-emenda-1:SCREEN-VALUE IN FRAME f-relat " Item: " c-it-codigo-emen1:SCREEN-VALUE IN FRAME f-relat SKIP
        "Lote Consumido 3: " c-emenda-2:SCREEN-VALUE IN FRAME f-relat " Item: " c-it-codigo-emen2:SCREEN-VALUE IN FRAME f-relat SKIP
        "Saldo das Bobinas origem: " d-saldo-bob:SCREEN-VALUE IN FRAME f-relat SKIP
        "Nr. do Doff: " i-nr-doff:SCREEN-VALUE IN FRAME f-relat SKIP
        "Data Inic.Producao: " dt-trans-ini:SCREEN-VALUE IN FRAME f-relat FORMAT "x(10)" SKIP
        "Hora Inic.Producao: " c-hr-fim-prod:SCREEN-VALUE IN FRAME f-relat SKIP
        "Data fim Producao: " dt-trans-fim:SCREEN-VALUE IN FRAME f-relat FORMAT "x(10)" SKIP
        "Hora fim Producao: " c-hr-fim-prod:SCREEN-VALUE IN FRAME f-relat SKIP
        "Estacao: " i-estacao:SCREEN-VALUE IN FRAME f-relat SKIP
        "Nr.Ordem Producao: " i-nr-ord-produ:SCREEN-VALUE IN FRAME f-relat SKIP
        "Item Produzido: " c-it-codigo-prod:SCREEN-VALUE IN FRAME f-relat SKIP
        "Pedido Venda: " i-nr-pedido:SCREEN-VALUE IN FRAME f-relat " Seq.: " i-seq-ped:SCREEN-VALUE IN FRAME f-relat SKIP
        "Nr.Bobina Produzida: " c-lote-bobina:SCREEN-VALUE IN FRAME f-relat SKIP
        "Peso Liquido Bobina: " d-peso-liquido:SCREEN-VALUE IN FRAME f-relat SKIP.

      

    FIND FIRST tt-estacao WHERE
        tt-estacao.estacao = INPUT FRAME f-relat i-estacao
        NO-LOCK NO-ERROR.
    
    IF AVAIL tt-estacao THEN 
        PUT "Deposito Prod:    " tt-estacao.cod-depos SKIP
            "Localizacao Prod: " tt-estacao.cod-localiz SKIP.

    FOR EACH tt-lote-cons where
        tt-lote-cons.consumo > 0 
        NO-LOCK.

        PUT "Consumo - Lote: " STRING (tt-lote-cons.lote)
            " Qtde.Consumo: " STRING (tt-lote-cons.consumo)
            " Deposito: " STRING (tt-lote-cons.cod-depos)
            " Localizacao: " STRING (tt-lote-cons.cod-localiz)
            " Referencia: " STRING (tt-lote-cons.cod-refer) SKIP.

    END.

    PUT "Turno: " i-turno:SCREEN-VALUE IN FRAME f-relat SKIP
        "Maquina: " i-maquina:SCREEN-VALUE IN FRAME f-relat SKIP
        "Turma:       " c-turma:SCREEN-VALUE IN FRAME f-relat SKIP
        "Largura:     " i-largura:SCREEN-VALUE IN FRAME f-relat SKIP
        "D.Interno:   " i-diin:SCREEN-VALUE IN FRAME f-relat SKIP
        "D.Externo:   " i-diex:SCREEN-VALUE IN FRAME f-relat SKIP
        "Espessura:   " i-espessura:SCREEN-VALUE IN FRAME f-relat SKIP
        "Comprimento: " i-compr:SCREEN-VALUE IN FRAME f-relat SKIP
        "Dureza Esq:  " i-dureza-e:SCREEN-VALUE IN FRAME f-relat SKIP
        "Dureza Cen:  " i-dureza-c:SCREEN-VALUE IN FRAME f-relat SKIP
        "Dureza Dir:  " i-dureza-d:SCREEN-VALUE IN FRAME f-relat SKIP
        "Delta RHO:   " i-delta:SCREEN-VALUE IN FRAME f-relat SKIP
        "Larg.Util:   " i-larg-util:SCREEN-VALUE IN FRAME f-relat SKIP
        "Opcao Prd/Perd:" int(rs-opcao:SCREEN-VALUE IN FRAME f-relat) SKIP
        "Tipo Pr/Os/Og: " int(rs-tipo:SCREEN-VALUE IN FRAME f-relat) SKIP.

    PUT SKIP
        "===================================================================" FORMAT "X(100)" SKIP.

    OUTPUT CLOSE.

      */
   IF INPUT FRAME f-relat rs-opcao <> 2 THEN DO:

      ASSIGN i-estacao:SCREEN-VALUE IN FRAME f-relat = 
             string(INT(i-estacao:SCREEN-VALUE IN FRAME f-relat) + 1)
             d-peso-liquido:SCREEN-VALUE IN FRAME f-relat = ""
             d-peso-bal:SCREEN-VALUE IN FRAME f-relat = "".
      
        
      
       ASSIGN c-lote-bobina:SCREEN-VALUE IN FRAME f-relat  = "".

   END.

   ELSE DO:   /* Perdas */

       ASSIGN rs-opcao:SCREEN-VALUE IN FRAME f-relat = "1"
              c-lote-bobina:SCREEN-VALUE IN FRAME f-relat = ""
              d-peso-liquido:SCREEN-VALUE IN FRAME f-relat = ""
              d-peso-bal:SCREEN-VALUE IN FRAME f-relat = "".

   END.
    ASSIGN rs-tipo:SCREEN-VALUE IN FRAME f-relat = "1".
    APPLY "value-changed" TO rs-tipo  IN FRAME f-relat.


    
    FOR EACH tt-defpri.
        DELETE tt-defpri.
    END.

    FOR EACH tt-defsec.
        DELETE tt-defsec.
    END.



   APPLY "entry" TO i-estacao IN FRAME f-relat.
   
END.

ON END-ERROR OF C-Win
OR ENDKEY OF C-Win ANYWHERE DO:
   RETURN NO-APPLY.
END.


ON END-ERROR OF C-Win
OR ENDKEY OF C-Win ANYWHERE DO:
   RETURN NO-APPLY.
END.

ON WINDOW-CLOSE OF C-Win
DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
   apply 'choose' TO bt-sai IN FRAME f-relat.
   RETURN NO-APPLY.
END.

ON ENDKEY OF FRAME f-relat DO:
  return no-apply.
END.

/*
ON CHOOSE OF bt-primeiro IN FRAME f-relat
DO:

   RUN pi-le-primeiro.

END.


ON CHOOSE OF bt-proximo IN FRAME f-relat
DO:

   RUN pi-le-proximo.

END.


ON CHOOSE OF bt-anterior IN FRAME f-relat
DO:

   RUN pi-le-anterior.

END.


ON CHOOSE OF bt-final IN FRAME f-relat
DO:

   RUN pi-le-ultimo.

END.

ON CHOOSE OF bt-pesquisa IN FRAME f-relat
DO:
            

END.

*/


/*
ON CHOOSE OF bt-goto IN FRAME f-relat
DO: 
    
    DEFINE BUTTON gt-bt-cancel AUTO-END-KEY 
         LABEL "&Cancelar" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE BUTTON gt-bt-ok AUTO-GO 
         LABEL "&OK" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE RECTANGLE gt-rt-botoes
         EDGE-PIXELS 2 GRAPHIC-EDGE  
         SIZE 58 BY 1.42
         BGCOLOR 7.

    DEFINE VARIABLE i-sequencia-gt AS INTEGER NO-UNDO.
    
    DEFINE RECTANGLE gt-rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 4.50.
    
    DEFINE FRAME gt-frame-1

        i-sequencia-gt LABEL "C¢digo do Usu†rio" AT ROW 2.5 COL 18 COLON-ALIGNED
         VIEW-AS FILL-IN 
         SIZE 16.14 BY .88
        
        gt-rect-1 AT ROW 1.9 COL 2

        gt-bt-ok          AT ROW 7.3 COL 2.14
        gt-bt-cancel      AT ROW 7.3 COL 13             
        gt-rt-botoes      AT ROW 7.0 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "Chave do Registro" FONT 1
             DEFAULT-BUTTON gt-bt-ok CANCEL-BUTTON gt-bt-cancel.

    ON "CHOOSE":U OF gt-bt-ok IN FRAME gt-frame-1 DO:

        ASSIGN i-empresa = int(i-sequencia-gt:SCREEN-VALUE IN FRAME gt-frame-1).

     RETURN.

    END.

    ENABLE i-sequencia-gt gt-bt-ok gt-bt-cancel 
        WITH FRAME gt-frame-1. 
    
    WAIT-FOR "GO":U OF FRAME gt-frame-1.

    RUN le-registro-goto.

END.
*/

/*

ON CHOOSE OF bt-deleta IN FRAME f-relat
DO: 
    
    DEFINE BUTTON ex-bt-cancel AUTO-END-KEY 
         LABEL "&Cancelar" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE BUTTON ex-bt-ok AUTO-GO 
         LABEL "&OK" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE RECTANGLE ex-rt-botoes
         EDGE-PIXELS 2 GRAPHIC-EDGE  
         SIZE 58 BY 1.42
         BGCOLOR 7.

    ASSIGN c-cod-estabel = "Exclui Este Registro? ".

    DEFINE RECTANGLE ex-rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 4.50.
    
    DEFINE FRAME ex-frame-1
      
        c-cod-estabel NO-LABEL 
           at ROW 3 col 10 
        
        ex-rect-1 AT ROW 1.9 COL 2

        ex-bt-cancel      AT ROW 7.3 COL 2.14             
        ex-bt-ok          AT ROW 7.3 COL 13
        ex-rt-botoes      AT ROW 7.0 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "Exclus∆o de Registro no Arquivo" FONT 1
             DEFAULT-BUTTON ex-bt-ok CANCEL-BUTTON ex-bt-cancel.

    ON "CHOOSE":U OF ex-bt-ok IN FRAME ex-frame-1 DO:

        FIND CURRENT am-tp-texto-romaneio EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL am-tp-texto-romaneio THEN
            DELETE am-tp-texto-romaneio.


        FIND NEXT am-tp-texto-romaneio NO-LOCK NO-ERROR.

        IF AVAIL am-tp-texto-romaneio THEN 
           RUN pi-mostra-registro. 

        ELSE DO:
            FIND PREV  am-tp-texto-romaneio NO-LOCK NO-ERROR.

            IF AVAIL am-tp-texto-romaneio THEN 
               RUN pi-mostra-registro. 

        END.

     RETURN.

    END.

    ENABLE ex-bt-cancel ex-bt-ok  
        WITH FRAME ex-frame-1. 
    
    DISPLAY c-cod-estabel 
        WITH FRAME ex-frame-1.

    WAIT-FOR "GO":U OF FRAME ex-frame-1.

END.
*/

ON CHOOSE OF bt-cancela IN FRAME f-relat
DO:


   RUN pi-disable-campos.
   RUN pi-disable-bt-grava.
   RUN pi-enable-outros-botoes.
   RUN pi-limpa-campos.

    
    FOR EACH tt-defpri.
        DELETE tt-defpri.
    END.

    FOR EACH tt-defsec.
        DELETE tt-defsec.
    END.



   APPLY "entry" TO c-cod-estabel IN FRAME f-relat.

END.


ON entry OF bt-novo IN FRAME f-relat
DO: 

   RUN pi-enable-bt-cancela.
   RUN pi-disable-outros-botoes.


   RUN pi-limpa-campos.
   
   RUN pi-enable-campos.

   APPLY "entry" TO c-cod-estabel IN FRAME f-relat.

END.


ON CHOOSE OF bt-novo IN FRAME f-relat
DO:

   RUN pi-enable-bt-cancela.
   RUN pi-disable-outros-botoes.


   RUN pi-limpa-campos.
   
   RUN pi-enable-campos.

    
    FOR EACH tt-defpri.
        DELETE tt-defpri.
    END.

    FOR EACH tt-defsec.
        DELETE tt-defsec.
    END.


   APPLY "entry" TO c-cod-estabel IN FRAME f-relat.

END.

/*
ON CHOOSE OF bt-altera IN FRAME f-relat
DO:

   ASSIGN c-tipo-botao = "altera".

   RUN pi-enable-bt-cancela.
   RUN pi-disable-outros-botoes.


   RUN pi-enable-campos.

END.


ON CHOOSE OF bt-copia IN FRAME f-relat
DO:

   ASSIGN c-tipo-botao = "copia".

   RUN pi-enable-bt-cancela.
   RUN pi-disable-outros-botoes.


   RUN pi-enable-campos.

END.


ON CHOOSE OF bt-grava IN FRAME f-relat
DO:

    IF c-tipo-botao <> "novo"  AND 
       c-tipo-botao <> "copia" AND  
       c-tipo-botao <> "altera" THEN RETURN.

    /* Aqui colocar as validaá‰es dos campos antes de serem
       gravados no arquivo */



    /* ---------------------------------------------------- */

    ASSIGN c-tipo-botao2 = c-tipo-botao
           c-tipo-botao  = "".

    RUN pi-le-pela-chave.

    IF RETURN-VALUE = "nok" THEN DO: 
        RUN pi-limpa-campos.
        RUN pi-disable-campos.
        APPLY "choose" to bt-cancela IN FRAME f-relat.  
       RETURN.
    END.
       
    RUN pi-grava-registro.

    RUN pi-disable-campos.

    IF c-tipo-botao2 = "novo" THEN DO:
        APPLY "choose" to bt-novo IN FRAME f-relat.
        RETURN.
    END.
    ELSE DO:
        APPLY "choose" to bt-cancela IN FRAME f-relat.
        RETURN.
    END.

END.

*/
ON CHOOSE OF bt-ajuda IN FRAME f-relat
DO:


END.

ON CHOOSE OF btpeso IN FRAME f-relat /* Add */
DO:
   
   RUN sfc/essf0002D-A1.W.
   
   /* JosÇ Roberto - 02/12/2012 - Liberado para digitar peso
      manual enquanto estiver em convers∆o para o ems-206    */
      
      assign l-peso-bal = yes.
      
   /*--------------------------------------------------------*/  
   
    
   IF L-PESO-BAL = yes THEN DO :
       ASSIGN 
           btpeso:SENSITIVE IN FRAME f-relat         = NO
           D-PESO-liquido:SENSITIVE IN FRAME f-relat = YES
           D-PESO-liquido:col IN FRAME f-relat = 16
           D-PESO-bal:SENSITIVE IN FRAME f-relat     = NO
           D-PESO-bal:VISIBLE IN FRAME f-relat     = no
           .

   /* JosÇ Roberto - 02/12/2012 - Liberado para digitar peso
      manual enquanto estiver em convers∆o para o ems-206    */
      
       MESSAGE "DIGITAÄ«O DE PESAGEM LIBERADA TEMPORARIAMENTE," SKIP "ENQUANTO SESS«O ESTIVER ATIVA."       
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
             
   /*--------------------------------------------------------*/  
             
   END.
       

END.

ON CHOOSE OF bt-sai IN FRAME f-relat
DO:

    FIND FIRST tt-estacao WHERE
        tt-estacao.estacao = 0
        NO-LOCK NO-ERROR.

    IF AVAIL tt-estacao AND tt-estacao.nr-prim-bob <> 0 THEN DO:

c-temp-jr = session:temp-directory.

        OUTPUT TO value(c-temp-jr + "essf0021.txt").
        
        FOR EACH tt-estacao NO-LOCK.
        
            IF tt-estacao.estacao = 0 THEN DO:
        
                PUT UNFORMATTED 
                    tt-estacao.estacao        AT 01
                    tt-estacao.nr-ord-off-spc AT 10
                    tt-estacao.nr-ord-off-grd AT 20
                    tt-estacao.letra-bobina   AT 30
                    tt-estacao.nr-prim-bob    AT 40 SKIP.
        
            END.
        
            IF tt-estacao.estacao <> 0 THEN DO:
        
                PUT UNFORMATTED
                    tt-estacao.estacao        AT 01
                    tt-estacao.nr-ord-produ   AT 10
                    tt-estacao.it-codigo      AT 20
                    tt-estacao.nr-pedido      AT 40
                    tt-estacao.seq-ped        AT 50 
                    tt-estacao.nome-abrev     AT 60
                    tt-estacao.cod-depos      AT 80
                    tt-estacao.cod-localiz    AT 90
                    tt-estacao.gera-pallet    AT 100 
                    tt-estacao.lote    AT 103 SKIP.
        
            END.
        
        END.
        
        OUTPUT CLOSE.

    END.

   apply "close" to this-procedure.

END.


/*
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
   run pi-troca-pagina.
END.

ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
   run pi-troca-pagina.
END.

ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
   run pi-troca-pagina.
END.

ON MOUSE-SELECT-CLICK OF im-pg-dig IN FRAME f-relat
DO:
   run pi-troca-pagina.
END.
*/
/* ***************************  Main Block  *************************** */

i-nr-ord-produ:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME f-relat.
c-cod-operador:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME f-relat.
c-cod-estabel:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME f-relat.
c-cod-ctrab:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME f-relat.

 c-hr-inic-prod = substring(replace(STRING(TIME,"HH:MM:SS"),":",""),1,4).
 c-hr-fim-prod = substring(replace(STRING(TIME,"HH:MM:SS"),":",""),1,4).

/* leitura de pesagem via prota COM1*/

run pi-configura.



ASSIGN CURRENT-WINDOW             = {&WINDOW-NAME}
   THIS-PROCEDURE:CURRENT-WINDOW  = {&WINDOW-NAME}.

assign v-cod-prog-gerado = "essf0022".



def var c-tit as char no-undo.

ASSIGN c-tit = "essf0022 - Reporte de Produá∆o no Corte/Recorte/Metalizaá∆o".
assign {&WINDOW-NAME}:title = c-tit
       c-arq-old        = c-arquivo
       c-impressora-old = c-arquivo.


assign {&window-name}:virtual-width-chars  = {&window-name}:width-chars  
       {&window-name}:virtual-height-chars = {&window-name}:height-chars 
       {&window-name}:min-width-chars      = {&window-name}:width-chars  
       {&window-name}:max-width-chars      = {&window-name}:width-chars  
       {&window-name}:min-height-chars     = {&window-name}:height-chars 
       {&window-name}:max-height-chars     = {&window-name}:height-chars.
assign c-terminal = " Terminal".


ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.


def var wh-label-sel     as widget-handle no-undo.
def var wh-label-cla     as widget-handle no-undo.
def var wh-label-par     as widget-handle no-undo.
def var wh-label-dig     as widget-handle no-undo.
def var wh-label-imp     as widget-handle no-undo.
def var wh-group         as widget-handle no-undo.
def var wh-child         as widget-handle no-undo.
def var c-list-folders   as char          no-undo.
def var i-current-folder as integer       no-undo.
def var i-new-folder     as integer       no-undo.
def var c-aux            as char          no-undo.
def var i-aux            as integer       no-undo.

ON  CLOSE OF THIS-PROCEDURE DO:
    RUN disable_ui. 
END.

on  CTRL-TAB,SHIFT-CTRL-TAB anywhere do:
    define variable h_handle  as handle no-undo.       
    define variable c_imagem  as character no-undo.
    define variable l_direita as logical no-undo.            

    l_direita = last-event:label = 'CTRL-TAB'.
        
    block1:
    repeat:        
        if  l_direita then do:
            if  i-current-folder = num-entries(c-list-folders) then
                i-current-folder = 1.
            else
                i-current-folder = i-current-folder + 1.
        end.
        else do:
            if  i-current-folder = 1 then
                i-current-folder = num-entries(c-list-folders).
            else
                i-current-folder = i-current-folder - 1.
        end.
    
        assign c_imagem = entry(i-current-folder,c-list-folders)
               h_handle = frame f-relat:first-child
               h_handle = h_handle:first-child.

        do  while valid-handle(h_handle):
            if  h_handle:type = 'image' and
                h_handle:name =  c_imagem then do:
                if  h_handle:sensitive = no then 
                    next block1.
                apply 'mouse-select-click' to h_handle.
                leave block1.
            end.
            h_handle = h_handle:next-sibling.
        end.
    end.
end.



/********************************************************** 
** Procedure de troca de p†gina por CTRL-TAB 
**********************************************************/
procedure pi-first-child:
        
    define input parameter wh-entry-folder as widget-handle.
    
    assign wh-entry-folder = wh-entry-folder:first-child
           wh-entry-folder = wh-entry-folder:first-child.
    do  while(valid-handle(wh-entry-folder)):
        if  wh-entry-folder:sensitive = yes 
        and wh-entry-folder:type <> 'rectangle' 
        and wh-entry-folder:type <> 'image'
        and wh-entry-folder:type <> 'browse' then do:
            apply 'entry' to wh-entry-folder.
            leave.
        end.
        else
            assign wh-entry-folder = wh-entry-folder:next-sibling.    
    end.
    
end.


/* define procedure externa para execucao do programa de visualizacao do relatorio */

PROCEDURE WinExec EXTERNAL "kernel32.dll":
  DEF INPUT  PARAM prg_name                          AS CHARACTER.
  DEF INPUT  PARAM prg_style                         AS SHORT.
END PROCEDURE.



PAUSE 0 BEFORE-HIDE.

MAIN-BLOCK:

DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    IF SESSION:SET-WAIT-STATE("":U) THEN.
    RUN enable_UI.
c-temp-jr = session:temp-directory.


FOR EACH tt-estacao.
        DELETE tt-estacao.
    END.
    

    ASSIGN c-arquivo-jr = search(c-temp-jr + "essf0021.txt"). 
    
    IF c-arquivo-jr <> ? then do: 

        INPUT FROM VALUE(c-arquivo-jr).

    REPEAT:

       IMPORT UNFORMATTED text-string.

       IF INT(SUBSTRING(text-string,1,9)) = 0 THEN DO:

           CREATE tt-estacao.

           ASSIGN tt-estacao.estacao         = INT(SUBSTRING(text-string,1,9))    
                  tt-estacao.nr-ord-off-spc  = INT(SUBSTRING(text-string,10,9))    
                  tt-estacao.nr-ord-off-grd  = INT(SUBSTRING(text-string,20,9))    
                  tt-estacao.letra-bobina    = SUBSTRING(text-string,30,1)         
                  tt-estacao.nr-prim-bob     = INT(SUBSTRING(text-string,40,9)).

       END.

       ELSE DO:

          CREATE tt-estacao.
          
          ASSIGN tt-estacao.estacao         = INT(SUBSTRING(text-string,1,9))
                 tt-estacao.nr-ord-produ    = INT(SUBSTRING(text-string,10,9))
                 tt-estacao.it-codigo       = SUBSTRING(text-string,20,18)
                 tt-estacao.nr-pedido       = int(SUBSTRING(text-string,40,9))     
                 tt-estacao.seq-ped         = INT(SUBSTRING(text-string,50,9))
                 tt-estacao.nome-abrev      = SUBSTRING(text-string,60,15)
                 tt-estacao.cod-depos       = SUBSTRING(text-string,80,9)
                 tt-estacao.cod-localiz     = SUBSTRING(text-string,90,10)
                 tt-estacao.gera-pallet     = SUBSTRING(text-string,100,3) 
                 tt-estacao.lote            = SUBSTRING(text-string,103,10). 
       END.

    END.
    
    end.

    RUN pi-disable-bt-grava.   


    assign v-cod-pg-mouse-selec = "im-pg-sel".

     view c-win.
     apply "entry" to frame f-Relat.
     apply "entry" to c-win.


    /* ----------------------------- */

   IF  NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.

END.

/* **********************  Internal Procedures  *********************** */

PROCEDURE adm-row-available :
   /* Define variables needed by this internal procedure.             */
  /* Process the newly available records (i.e. display fields, 
     open queries, and/or pass records on to any RECORD-TARGETS).    */
END PROCEDURE.

PROCEDURE disable_UI :
   IF SESSION:DISPLAY-TYPE = "GUI" AND VALID-HANDLE(C-Win)
   THEN DELETE WIDGET C-Win.
   IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

PROCEDURE enable_UI :

   
   /* JosÇ Roberto - 02/12/2012 - Liberado para digitar peso
      manual enquanto estiver em convers∆o para o ems-206    */
      
      assign l-peso-bal = yes.
      
   /*--------------------------------------------------------*/  
   


    DISPLAY c-cod-estabel     
            c-cod-ctrab       
            c-cod-operador    
            c-lote-cons       
            c-emenda-1        
            c-emenda-2        
                              
            c-nome-estab      
            c-desc-ctrab      
            c-nome-operador   
                              
            c-desc-item-cons  
            c-desc-item-emen1 
            c-desc-item-emen2 
                              
            c-it-codigo-cons  
            c-it-codigo-emen1 
            c-it-codigo-emen2 
                              
            d-saldo-bob       
                              
            i-nr-doff         
                              
            dt-trans-ini      
            c-hr-inic-prod    
                              
            dt-trans-fim      
            c-hr-fim-prod     
                              
            i-estacao         
            i-nr-ord-produ    
            c-it-codigo-prod  
            i-nr-pedido       
            i-seq-ped         
                              
            c-lote-bobina     
            d-peso-liquido  
            d-peso-bal

            i-turno      
            i-maquina    
            c-turma      
            i-largura    
            i-diin       
            i-diex       
            i-espessura  
            i-compr      
            i-larg-util  
            i-emendas
            i-delta      
            i-dureza-e
            i-dureza-c
            i-dureza-d

        WITH FRAME f-relat IN WINDOW C-Win.

    D-PESO-BAL:TOOLTIP IN FRAME f-relat =  "Posicione a bobina na Balanáa e confirme o Peso".
    IF l-peso-bal THEN DO:
         D-PESO-liquido:SENSITIVE IN FRAME f-relat =  YES.
         D-PESO-liquido:col IN FRAME f-relat = 16.
         D-PESO-bal:SENSITIVE IN FRAME f-relat = NO.
         D-PESO-bal:VISIBLE IN FRAME f-relat     = no.
    END.
    ELSE DO:
         D-PESO-liquido:SENSITIVE IN FRAME f-relat =  no.
          D-PESO-liquido:bgcolor IN FRAME f-relat = 15.
         D-PESO-bal:SENSITIVE IN FRAME f-relat = yes.
         D-PESO-liquido:col IN FRAME f-relat = 17.
    END.


   ENABLE bt-novo    
          bt-cancela
          bt-sai
          btpeso
          bt-ajuda
           
   WITH FRAME f-relat IN WINDOW C-Win.

   
  VIEW C-Win.

  
END PROCEDURE.

PROCEDURE local-exit :
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN.
END PROCEDURE.  

PROCEDURE pi-troca-pagina:


END PROCEDURE.

PROCEDURE send-records :
    /* Define variables needed by this internal procedure.               */ 
    /* For each requested table, put it':Us ROWID in the output list.      */
    /* Deal with any unexpected table requests before closing.           */ 
END PROCEDURE.

PROCEDURE state-changed :
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
    run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

Procedure pi-muda-cor-label-folder:
   def input parameter p-cod-label as char  no-undo.
   def var wh-pai   as widget-handle.
   def var wh-filho as widget-handle.

    assign wh-pai = frame f-relat:handle
           wh-pai = wh-pai:first-child.
   do while wh-pai <> ?:
       do  while valid-handle(wh-pai):
           assign wh-filho = wh-pai:first-child.
           do  while valid-handle(wh-filho):
               if  wh-filho:type = "TEXT"
                   then
                       if  wh-filho:screen-value = p-cod-label
                       then
                           assign wh-filho:fgcolor = 7.
                       assign wh-filho = wh-filho:next-sibling.
           end.
           assign wh-pai = wh-pai:next-sibling.
       end.
   end.
END PROCEDURE.

PROCEDURE OpenDocument:

    def input param c-doc as char  no-undo.
    def var c-exec as char  no-undo.
    def var h-Inst as int  no-undo.

    assign c-exec = fill("x",255).
    run FindExecutableA (input c-doc,
                         input "",
                         input-output c-exec,
                         output h-inst).

    if h-inst >= 0 and h-inst <=32 then
      run ShellExecuteA (input 0,
                         input "open",
                         input "rundll32.exe",
                         input "shell32.dll,OpenAs_RunDLL " + c-doc,
                         input "",
                         input 1,
                         output h-inst).

    run ShellExecuteA (input 0,
                       input "open",
                       input c-doc,
                       input "",
                       input "",
                       input 1,
                       output h-inst).

    if h-inst < 0 or h-inst > 32 then return "OK".
    else return "NOK".

END PROCEDURE.

PROCEDURE FindExecutableA EXTERNAL "Shell32.dll" persistent:

    define input parameter lpFile as char  no-undo.
    define input parameter lpDirectory as char  no-undo.
    define input-output parameter lpResult as char  no-undo.
    define return parameter hInstance as long.

END.

PROCEDURE ShellExecuteA EXTERNAL "Shell32.dll" persistent:

    define input parameter hwnd as long.
    define input parameter lpOperation as char  no-undo.
    define input parameter lpFile as char  no-undo.
    define input parameter lpParameters as char  no-undo.
    define input parameter lpDirectory as char  no-undo.
    define input parameter nShowCmd as long.
    define return parameter hInstance as long.

END PROCEDURE.

PROCEDURE pi-mostra-registro.
/*
    ASSIGN am-tp-texto-romaneio.sequencia:SCREEN-VALUE IN FRAME f-relat = STRING (am-tp-texto-romaneio.sequencia)
           opcao-jr:SCREEN-VALUE IN FRAME f-relat = STRING (am-tp-texto-romaneio.situacao)
           am-tp-texto-romaneio.descricao:SCREEN-VALUE IN FRAME f-pg-sel = STRING (am-tp-texto-romaneio.descricao)
           c-texto-jr:SCREEN-VALUE IN FRAME f-pg-sel = STRING (am-tp-texto-romaneio.texto)
           c-narrativa-jr:SCREEN-VALUE IN FRAME f-pg-sel = STRING (am-tp-texto-romaneio.narrativa).  
*/
END PROCEDURE.

PROCEDURE pi-le-primeiro.


END PROCEDURE.


PROCEDURE pi-le-proximo.

    

END PROCEDURE.


PROCEDURE pi-le-anterior.

    

END PROCEDURE.

PROCEDURE pi-le-ultimo.

    

END PROCEDURE.



PROCEDURE le-registro-goto.
    
      
END PROCEDURE.

PROCEDURE pi-mostra-mensagem.



END PROCEDURE.

PROCEDURE pi-limpa-campos.
/*
    ASSIGN i-nr-ord-produ:SCREEN-VALUE IN FRAME f-relat = ""
           i-estacao-ini:SCREEN-VALUE IN FRAME f-relat = "1"
           i-estacao-fim:SCREEN-VALUE IN FRAME f-relat = "20"
           c-letra-bob:SCREEN-VALUE IN FRAME f-relat = ""
           i-nr-bobina:SCREEN-VALUE IN FRAME f-relat = ""
           i-nr-ord-specs:SCREEN-VALUE IN FRAME f-relat = ""
           i-nr-ord-grade:SCREEN-VALUE IN FRAME f-relat = ""
           c-it-codigo:SCREEN-VALUE IN FRAME f-relat = ""
           c-it-codigo-spc:SCREEN-VALUE IN FRAME f-relat = ""
           c-it-codigo-grd:SCREEN-VALUE IN FRAME f-relat = ""
           c-desc-item:SCREEN-VALUE IN FRAME f-relat = ""
           c-desc-item-spc:SCREEN-VALUE IN FRAME f-relat = ""
           c-desc-item-grd:SCREEN-VALUE IN FRAME f-relat = "".


    FOR EACH tt-estacao.
        DELETE tt-estacao.
    END.
*/
END PROCEDURE.

PROCEDURE pi-enable-campos.

    ENABLE  c-cod-estabel     
            c-cod-ctrab       
            c-cod-operador    
            c-lote-cons       
            c-emenda-1        
            c-emenda-2        
                              
            rs-opcao
            bt-setup
  
            i-nr-doff         
                              
            dt-trans-ini      
            c-hr-inic-prod    
                              
            dt-trans-fim      
            c-hr-fim-prod     
                              
            rs-tipo

            i-estacao 

        /*
            i-nr-ord-produ    
            i-nr-pedido       
            i-seq-ped         
        */

            c-lote-bobina     
             
            i-turno
            i-maquina    
            c-turma 
            i-largura    
            i-diin       
            i-diex
            i-espessura  
            i-compr      
            i-larg-util  
            i-emendas
            /*i-delta      */
            i-dureza-e 
            i-dureza-c
            i-dureza-d

            bt-defpri
            bt-defsec

            bt-atualiza
            bt-cancela2

        WITH FRAME f-relat.



    APPLY "entry" TO c-cod-estabel IN FRAME f-relat.
                   /* ========================================================================
    Rotina para sugerir o operador logado */



  FIND FIRST operador WHERE
      operador.char-2 = c-seg-usuario
      NO-LOCK NO-ERROR.
 
  IF AVAIL operador THEN DO:
      ASSIGN c-cod-operador:SCREEN-VALUE  IN FRAME f-relat = operador.cod-operador.
      APPLY "leave" TO c-cod-operador IN FRAME f-relat.
             
  END.

  ASSIGN c-cod-operador:SENSITIVE  IN FRAME f-relat  = NO.

 
/* fim Rotina para sugerir o operador logado 
========================================================================   */



END PROCEDURE.


PROCEDURE pi-disable-campos.

END PROCEDURE.

PROCEDURE pi-le-pela-chave.


    RETURN "ok".

END PROCEDURE.


PROCEDURE pi-grava-registro.


    RETURN "ok".
                

END PROCEDURE.

PROCEDURE pi-disable-bt-grava.

    assign /*bt-grava:SENSITIVE in frame f-relat   = no */
           bt-cancela:SENSITIVE in frame f-relat = no.

END PROCEDURE.

PROCEDURE pi-enable-bt-cancela.

    ENABLE bt-cancela
        WITH FRAME f-relat.

END PROCEDURE.

PROCEDURE pi-disable-outros-botoes.
   ASSIGN /*bt-primeiro:SENSITIVE in frame f-relat = no  
          bt-anterior:SENSITIVE in frame f-relat   = no  
          bt-proximo:SENSITIVE in frame f-relat    = no   
          bt-final:SENSITIVE in frame f-relat      = no  
          bt-goto:SENSITIVE in frame f-relat       = no 
          bt-pesquisa:SENSITIVE in frame f-relat   = no */
          bt-novo:SENSITIVE in frame f-relat       = no. 
        /*  bt-copia:SENSITIVE in frame f-relat    = no 
          bt-altera:SENSITIVE in frame f-relat     = no 
          bt-deleta:SENSITIVE in frame f-relat     = no */
   
END PROCEDURE.

PROCEDURE pi-enable-outros-botoes.

   ENABLE /*bt-primeiro 
          bt-anterior 
          bt-proximo  
          bt-final 
          bt-goto
          bt-pesquisa */
         /* bt-novo */
          bt-cancela   
         /* bt-copia
          bt-altera
          bt-deleta */
          bt-sai
          btpeso
   WITH FRAME f-relat IN WINDOW C-Win.
   
END PROCEDURE.

PROCEDURE pi-monta-browse-lotes:
Do:

END.

END PROCEDURE.
  

PROCEDURE CalculaQuantSaldo :
   
    DEFINE VARIABLE i-ct-emenda AS INTEGER    NO-UNDO.
    

    FOR EACH tt-lote-cons.
        DELETE tt-lote-cons.
    END.

    i-ct-emenda = -1. 
    ASSIGN saldo-lote-cons = 0.

    IF c-lote-cons:screen-value in frame f-relat <> "" THEN DO:
            i-ct-emenda = i-ct-emenda + 1.
        for each saldo-estoq fields (qtidade-atu qt-alocada qt-aloc-ped it-codigo cod-depos cod-refer cod-estabel cod-localiz lote
                                     qt-aloc-prod) use-index lote no-lock where
                 saldo-estoq.lote        = c-lote-cons:screen-value in frame f-relat       and
                 saldo-estoq.cod-estabel = c-cod-estabel:screen-value in frame f-relat     AND
                 NOT (saldo-estoq.cod-depos  = "EXP" AND  (saldo-estoq.cod-estabel = "412" OR saldo-estoq.cod-estabel = "422")  /*solic-318*/ ) and
                 saldo-estoq.cod-depos  <> substring(param-cp.char-2,1,3) AND
                 saldo-estoq.qtidade-atu > 0,
        
            FIRST deposito  FIELDS (cod-depos ind-dep-cq)
                WHERE deposito.cod-depos = saldo-estoq.cod-depos
                  AND deposito.ind-dep-cq = NO :
        
            assign saldo-lote-cons = saldo-lote-cons + (saldo-estoq.qtidade-atu - 
                                     saldo-estoq.qt-alocada  - 
                                     saldo-estoq.qt-aloc-ped -
                                     saldo-estoq.qt-aloc-prod).

            FIND FIRST tt-lote-cons WHERE
                tt-lote-cons.seq          = 1                       AND
                tt-lote-cons.lote         = saldo-estoq.lote        AND
                tt-lote-cons.it-codigo    = saldo-estoq.it-codigo   AND
                tt-lote-cons.cod-depos    = saldo-estoq.cod-depos   AND
                tt-lote-cons.cod-localiz  = saldo-estoq.cod-localiz AND
                tt-lote-cons.cod-refer    = saldo-estoq.cod-refer   AND
                tt-lote-cons.cod-estabel  = saldo-estoq.cod-estabel
                NO-ERROR.

            IF NOT AVAIL tt-lote-cons THEN DO:

                CREATE tt-lote-cons.

                ASSIGN tt-lote-cons.seq          = 1 
                       tt-lote-cons.lote         = saldo-estoq.lote        
                       tt-lote-cons.it-codigo    = saldo-estoq.it-codigo   
                       tt-lote-cons.cod-depos    = saldo-estoq.cod-depos   
                       tt-lote-cons.cod-localiz  = saldo-estoq.cod-localiz 
                       tt-lote-cons.cod-refer    = saldo-estoq.cod-refer   
                       tt-lote-cons.cod-estabel  = saldo-estoq.cod-estabel.

            END.

            ASSIGN tt-lote-cons.saldo = tt-lote-cons.saldo + saldo-estoq.qtidade-atu.

        end.

    END.

    ASSIGN saldo-emenda1-cons = 0.

    IF c-emenda-1:screen-value in frame f-relat <> "" THEN DO:

        i-ct-emenda = i-ct-emenda + 1.
            
        for each saldo-estoq fields (qtidade-atu qt-alocada qt-aloc-ped it-codigo cod-depos cod-refer cod-estabel cod-localiz lote
                                     qt-aloc-prod) use-index lote no-lock where
                 saldo-estoq.lote        = c-emenda-1:screen-value in frame f-relat        and
                 saldo-estoq.cod-estabel = c-cod-estabel:screen-value in frame f-relat     AND
                 NOT (saldo-estoq.cod-depos  = "EXP" AND  (saldo-estoq.cod-estabel = "412" OR saldo-estoq.cod-estabel = "422")  /*solic-318*/ ) and
                 saldo-estoq.cod-depos  <> substring(param-cp.char-2,1,3) AND
                 saldo-estoq.qtidade-atu > 0,
        
            FIRST deposito  FIELDS (cod-depos ind-dep-cq)
                WHERE deposito.cod-depos = saldo-estoq.cod-depos
                  AND deposito.ind-dep-cq = NO :
        
            assign saldo-emenda1-cons = saldo-emenda1-cons + (saldo-estoq.qtidade-atu - 
                                        saldo-estoq.qt-alocada  - 
                                        saldo-estoq.qt-aloc-ped -
                                        saldo-estoq.qt-aloc-prod).

            FIND FIRST tt-lote-cons WHERE
                tt-lote-cons.seq          = 2                       AND
                tt-lote-cons.lote         = saldo-estoq.lote        AND
                tt-lote-cons.it-codigo    = saldo-estoq.it-codigo   AND
                tt-lote-cons.cod-depos    = saldo-estoq.cod-depos   AND
                tt-lote-cons.cod-localiz  = saldo-estoq.cod-localiz AND
                tt-lote-cons.cod-refer    = saldo-estoq.cod-refer   AND
                tt-lote-cons.cod-estabel  = saldo-estoq.cod-estabel
                NO-ERROR.

            IF NOT AVAIL tt-lote-cons THEN DO:

                CREATE tt-lote-cons.

                ASSIGN tt-lote-cons.seq          = 2  
                       tt-lote-cons.lote         = saldo-estoq.lote        
                       tt-lote-cons.it-codigo    = saldo-estoq.it-codigo   
                       tt-lote-cons.cod-depos    = saldo-estoq.cod-depos   
                       tt-lote-cons.cod-localiz  = saldo-estoq.cod-localiz 
                       tt-lote-cons.cod-refer    = saldo-estoq.cod-refer   
                       tt-lote-cons.cod-estabel  = saldo-estoq.cod-estabel.

            END.

            ASSIGN tt-lote-cons.saldo = tt-lote-cons.saldo + saldo-estoq.qtidade-atu.

        end.

    END.

    ASSIGN saldo-emenda2-cons = 0.

    IF c-emenda-2:screen-value in frame f-relat <> "" THEN DO:
            
        i-ct-emenda = i-ct-emenda + 1.

        for each saldo-estoq fields (qtidade-atu qt-alocada qt-aloc-ped it-codigo cod-depos cod-refer cod-estabel cod-localiz lote
                                     qt-aloc-prod) use-index lote no-lock where
                 saldo-estoq.lote        = c-emenda-2:screen-value in frame f-relat        and
                 saldo-estoq.cod-estabel = c-cod-estabel:screen-value in frame f-relat     AND
                 NOT (saldo-estoq.cod-depos  = "EXP" AND  (saldo-estoq.cod-estabel = "412" OR saldo-estoq.cod-estabel = "422")  /*solic-318*/ ) and
                 saldo-estoq.cod-depos  <> substring(param-cp.char-2,1,3) AND
                 saldo-estoq.qtidade-atu > 0,
        
            FIRST deposito  FIELDS (cod-depos ind-dep-cq)
                WHERE deposito.cod-depos = saldo-estoq.cod-depos
                  AND deposito.ind-dep-cq = NO :
        
            assign saldo-emenda2-cons = saldo-emenda2-cons + (saldo-estoq.qtidade-atu - 
                                        saldo-estoq.qt-alocada  - 
                                        saldo-estoq.qt-aloc-ped -
                                        saldo-estoq.qt-aloc-prod).

            FIND FIRST tt-lote-cons WHERE
                tt-lote-cons.seq          = 3                       AND
                tt-lote-cons.lote         = saldo-estoq.lote        AND
                tt-lote-cons.it-codigo    = saldo-estoq.it-codigo   AND
                tt-lote-cons.cod-depos    = saldo-estoq.cod-depos   AND
                tt-lote-cons.cod-localiz  = saldo-estoq.cod-localiz AND
                tt-lote-cons.cod-refer    = saldo-estoq.cod-refer   AND
                tt-lote-cons.cod-estabel  = saldo-estoq.cod-estabel
                NO-ERROR.

            IF NOT AVAIL tt-lote-cons THEN DO:

                CREATE tt-lote-cons.

                ASSIGN tt-lote-cons.seq          = 3
                       tt-lote-cons.lote         = saldo-estoq.lote        
                       tt-lote-cons.it-codigo    = saldo-estoq.it-codigo   
                       tt-lote-cons.cod-depos    = saldo-estoq.cod-depos   
                       tt-lote-cons.cod-localiz  = saldo-estoq.cod-localiz 
                       tt-lote-cons.cod-refer    = saldo-estoq.cod-refer   
                       tt-lote-cons.cod-estabel  = saldo-estoq.cod-estabel.

            END.

            ASSIGN tt-lote-cons.saldo = tt-lote-cons.saldo + saldo-estoq.qtidade-atu.
        
        end.

    END.

    assign d-saldo-bob = saldo-lote-cons    +
                         saldo-emenda1-cons +
                         saldo-emenda2-cons.
    
    
    i-emendas = i-ct-emenda.

    disp d-saldo-bob
         i-emendas with frame f-relat.


END PROCEDURE.




PROCEDURE pi-especificacao :
/*------------------------------------------------------------------------------
  Purpose: exibir especificaá∆o especial do cliente x item
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     
      FIND b-ord-prod WHERE b-ord-prod.nr-ord-produ =  ord-prod.nr-ord-produ NO-LOCK NO-ERROR.
      IF NOT AVAIL b-ord-prod THEN RETURN.

      FIND ped-venda WHERE ped-venda.nr-pedido = int(b-ord-prod.nr-pedido) NO-LOCK NO-ERROR.
      IF  AVAIL ped-venda THEN 
        RUN pdp\upc\upc-pd4000-a.p (INPUT b-ord-prod.cod-estabel,INPUT b-ord-prod.it-codigo, INPUT ped-venda.nome-abrev).


END PROCEDURE.



  PROCEDURE criaTT:

      FOR EACH tt-reporta.
          DELETE tt-reporta.
      END.

      FOR EACH tt-lote-reporte.
          DELETE tt-lote-reporte.
      END.

      FIND first ord-prod no-lock where
         ord-prod.nr-ord-produ = int(i-nr-ord-produ:SCREEN-VALUE IN FRAME f-relat)  NO-ERROR.
      IF NOT AVAIL ord-prod THEN RETURN "nok".


      create tt-reporta.

      ASSIGN   tt-reporta.nr-ord-produ = ord-prod.nr-ord-produ
               tt-reporta.it-codigo    = ord-prod.it-codigo
               tt-reporta.nr-pedcli    = ord-prod.nr-pedido
               tt-reporta.nome-abrev   = ord-prod.nome-abrev
               tt-reporta.qt-ordem     = ord-prod.qt-ordem - ord-prod.qt-produzida
               tt-reporta.qt-reporte   = 0
               tt-reporta.cod-depos    = ord-prod.cod-depos /*param-cp.dep-fabrica */ 
               tt-reporta.gera-pallet =  tt-estacao.gera-pallet.

            /*nao permite deposito cq na ordem*/
            
            if tt-reporta.cod-depos = "cq" then   tt-reporta.cod-depos =  param-cp.dep-fabrica.
            
            if index(tt-reporta.it-codigo,"OG" ) > 0 OR INPUT FRAME f-relat rs-opcao = 2 then do:
            
             find item where item.it-codigo =  tt-reporta.it-codigo no-lock no-error.
             if (avail item and item.ge-codigo = 47) OR INPUT FRAME f-relat rs-opcao = 2 then 
             assign
                   tt-reporta.cod-localiz = item.inform-compl
                   tt-reporta.cod-depos = "ARC".
            
            end.  

           
            
        CREATE tt-lote-reporte.
        ASSIGN tt-lote-reporte.nr-ord-produ = ord-prod.nr-ord-produ
               tt-lote-reporte.lote         = c-lote-bobina:SCREEN-VALUE IN FRAME f-relat
               tt-lote-reporte.quantidade   = DEC(d-peso-liquido:SCREEN-VALUE IN FRAME f-relat)
               tt-lote-reporte.dt-vali-lote = TODAY + 180
               tt-reporta.qt-reporte        = tt-lote-reporte.quantidade.
               
              IF ITEM.fm-codigo = "46-15" OR ITEM.fm-codigo = "46-17" THEN
                  tt-lote-reporte.dt-vali-lote = TODAY + 270.

              IF ITEM.fm-codigo = "46-23" OR ITEM.fm-codigo = "46-27" THEN
                  tt-lote-reporte.dt-vali-lote = TODAY + 60.



  END PROCEDURE.


   PROCEDURE ValidaCampos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    def var contOP1 as int no-undo.
    def var contOP2 as int no-undo.
    def var totMaxReporte as decimal no-undo.
    DEF VAR c-nr-pallet AS CHAR NO-UNDO.
    def var d-saldo-baixa as decimal no-undo.
    

 
       FIND ord-prod WHERE ord-prod.nr-ord-produ = 
               INT(i-nr-ord-produ:SCREEN-VALUE IN FRAME f-relat) NO-LOCK NO-ERROR.

       find first ctrab where ctrab.cod-ctrab = c-cod-ctrab:SCREEN-VALUE IN FRAME f-relat
               no-lock no-error.

       if avail ord-prod then 
       find first oper-ord where oper-ord.nr-ord-produ = ord-prod.nr-ord-produ
               no-lock no-error.

       if avail oper-ord and avail ctrab and oper-ord.gm-codigo <> ctrab.gm-codigo then do:
           run utp/ut-msgs.p (input "show",
                                      input 17006,
                                      input "Centro de trabalho " + ctrab.cod-ctrab + " informado, incompat°vel com grupo de m†quina " + oper-ord.gm-codigo + " da ordem selecionada! ").
           
           return "nok".

       end.

       

       find first param-sfc no-lock.




    for each tt-erro:
        delete tt-erro.
    end.
        
    do with frame f-relat :
    
        if not can-find (first estabelec where
                               estabelec.cod-estabel = input c-cod-estabel) then
            run criaErro (17006,"Estabelecimento n∆o encontrado!").

        
        
        
                
        if input dt-trans-ini = ? then
            run criaErro (17006,"Data inicial inv†lida!").
        
        if input dt-trans-fim = ? then
            run criaErro (17006,"Data final inv†lida!").
        
        if input dt-trans-fim < INPUT dt-trans-ini THEN 
            run criaErro (17006,"Data final menor que Data inicial!").
        
        if (param-sfc.log-tipo-relogio and 
            substr(input c-hr-inic-prod,3,2) > '59') or  
            substr(input c-hr-inic-prod,1,2) > "23" then
            run criaErro (3046,"Inicio").

        if input c-hr-inic-prod = ? or
           length(input c-hr-inic-prod) < 4  then
            run criaErro (17006,"Hora inicial inv†lida!").
        
                   
        if (param-sfc.log-tipo-relogio and 
            substr(input c-hr-fim-prod,3,2) > '59') or  
            substr(input c-hr-fim-prod,1,2) > '23' then
            run criaErro (3046,"Final").
        
        if input c-hr-fim-prod = ? or
           length(input c-hr-fim-prod) < 4  then
            run criaErro (17006,"Hora final inv†lida!").
        
        if input dt-trans-fim = INPUT dt-trans-ini THEN DO:
            IF SUBSTRING(INPUT c-hr-inic-prod,1,2) = SUBSTRING(INPUT c-hr-fim-prod,1,2) THEN DO:
                IF SUBSTRING(INPUT c-hr-fim-prod,3,2) < SUBSTRING(INPUT c-hr-inic-prod,3,2) THEN
                    run criaErro (17006,"Hora final menor que Hora inicial!").
            END.
            ELSE IF SUBSTRING(INPUT c-hr-fim-prod,1,2) < SUBSTRING(INPUT c-hr-inic-prod,1,2) THEN 
                run criaErro (17006,"Hora final menor que Hora inicial!").
        END.

         if input dt-trans-fim:SCREEN-VALUE IN FRAME f-relat = INPUT dt-trans-ini:SCREEN-VALUE IN FRAME f-relat
              
             AND INPUT c-hr-inic-prod:SCREEN-VALUE IN FRAME f-relat = INPUT c-hr-fim-prod:SCREEN-VALUE IN FRAME f-relat THEN DO:
            
                    run criaErro (17006,"Hora final igual que Hora inicial!").
            
            
        END.

        if not can-find (first operador where
                               operador.cod-operador = input c-cod-operador) then
            run criaErro (17006,"Operador n∆o encontrado!").
        
        if not can-find (first ctrab where
                               ctrab.cod-ctrab = input c-cod-ctrab) then
            run criaErro (17006,"Centro de trabalho n∆o encontrado!").
        
        FIND FIRST pol-param-estab
             WHERE pol-param-estab.cod-estabel = input c-cod-estabel NO-LOCK NO-ERROR.
        if not avail pol-param-estab then 
            run criaErro (17006,"ParÉmetro do Estabelecimento n∆o encontrado!").
        ELSE DO:
            
            if input dt-trans-fim > pol-param-estab.data-corte THEN 
                run criaErro (25997,"Transaá∆o n∆o permitida!Verifique Data de Corte informada nos ParÉmetros Estabelecimento - essf0008.").

            /* Numeracao do pallet Validaá∆o */
            for each tt-reporta
                WHERE tt-reporta.gera-pallet = "*":
                
                FIND FIRST ord-prod
                    WHERE ord-prod.nr-ord-prod = tt-reporta.nr-ord-produ NO-LOCK NO-ERROR.
       
                IF tt-reporta.nr-pedcli <> "" THEN DO:
                    FIND ped-item 
                        WHERE ped-item.nome-abrev   = tt-reporta.nome-abrev
                          AND ped-item.nr-pedcli    = tt-reporta.nr-pedcli
                          AND ped-item.nr-sequencia = ord-prod.nr-sequencia
                          AND ped-item.it-codigo    = tt-reporta.it-codigo
                          AND ped-item.cod-refer    = ord-prod.cod-refer NO-LOCK NO-ERROR.
                    IF AVAIL ped-item THEN DO:
                        FIND FIRST ped-venda
                            WHERE ped-venda.nome-abrev = tt-reporta.nome-abrev
                              AND ped-venda.nr-pedcli = tt-reporta.nr-pedcli NO-LOCK NO-ERROR.
        
                        ASSIGN c-nr-pallet = string(ped-venda.nr-pedido).
                    END.
                    ELSE run criaErro (17006,"Item do Pedido n∆o cadastrado para geraá∆o do Pallet!").
                END.
                ELSE ASSIGN c-nr-pallet = substring(string(ord-prod.nr-ord-produ,"99999999999"),6,6).
                IF LENGTH(c-nr-pallet) > 6 THEN
                    run criaErro (17006,"Numeraá∆o do Pallet excede 10 posiá‰es!").
            end.
            
            FIND FIRST tt-reporta
                WHERE tt-reporta.gera-pallet = "*" NO-LOCK NO-ERROR.
            IF AVAIL tt-reporta
            AND  DATE(dt-trans-fim:SCREEN-VALUE IN FRAME f-relat) > pol-param-estab.data-palete THEN
                run criaErro (17006,"Data de Paletizaá∆o n∆o pode ser superior a data Paletizaá∆o" + dt-trans-fim:SCREEN-VALUE IN FRAME f-relat + " - " +  STRING( pol-param-estab.data-palete )).
        END.
    END.
    /*Speto 15/01/2004*/
    assign contOP1 = 0
           contOP2 = 0.

    /*Somatoria*/
    for each tt-reporta:
        assign contOP1 = contOP1 + 1.
        if can-find(first tt-lote-reporte
                    where tt-lote-reporte.nr-ord-produ = tt-reporta.nr-ord-produ) then
           assign contOP2 = contOP2 + 1.
          
           find deposito where deposito.cod-depos = tt-reporta.cod-depos no-lock no-error.
          
           FIND ITEM WHERE ITEM.it-codigo = tt-reporta.it-codigo NO-LOCK NO-ERROR.

           if avail deposito and deposito.ind-dep-cq AND AVAIL ITEM then do:
           
              IF param-global.modulo-cq and 
                 param-cq.tipo-cq       > 1 and
                 STRING (f-item-uni-estab(item.it-codigo,
                                         IF c-cod-estabel:SCREEN-VALUE IN FRAME f-relat = '' THEN item.cod-estabel
                                         ELSE c-cod-estabel:SCREEN-VALUE IN FRAME f-relat ,
                                         "contr-qualid":U)) <> "yes":U THEN DO:
                                         
                  
                  run criaErro (17006,"ITEM " + string(item.it-codigo) + " n∆o esta marcado para controlar CQ!").
                     
               END.
           
           end.
           
    end.

  
    if contOP1 = 0 then do:
       
               run criaErro (17006,  " deve possuir ordem para reportar!").
       
    end. 

     if contOP2 = 0 then do:
      
               run criaErro (17006,  " deve possuir lotes para reportar!").
         
    end. 

     

    if temp-table tt-erro:has-records then do:
        run cdp/cd0666.w (input table tt-erro).
        return 'nok'.
    end.
    
     for each tt-reporta where tt-reporta.cod-depos = "cq" no-lock:
           if search("m:\dts\LOG_prd\essf0022.cq") <> ?  then do:

                OUTPUT TO m:\dts\LOG_prd\essf0022.cq APPEND.
                for each tt-lote-reporte where tt-lote-reporte.nr-ord-produ = tt-reporta.nr-ord-produ:
       

                    put "Prog: essf0022-APOS" FORMAT "x(19)" string(TODAY) "-" 
                         string(TIME,"hh:mm:ss") "-" 
                         " Logado: " string(c-seg-usuario) SKIP 
                        .
                        
                        put tt-reporta.cod-depos
                             " - Ordem:" tt-reporta.nr-ord-produ
                             " - Item:" tt-reporta.it-codigo
                             " - Qtde:" tt-reporta.qt-reporte 
                             " - lote:" tt-lote-reporte.lote
                             " - peso:" tt-lote-reporte.quantidade skip.
                             
                end.
                output close.
                
          end.
                         
             
     end.


        return 'ok'.

END PROCEDURE.


PROCEDURE criaErro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input param pCdErro as int  no-undo.
def input param pParam  as char no-undo.

    run utp/ut-msgs.p (input "msg",input pCdErro,input pParam).
    
    create tt-erro.
    assign tt-erro.cd-erro  = pCdErro
           tt-erro.mensagem = return-value.

return 'ok'.

END PROCEDURE.

PROCEDURE eliminaTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each tt-rep-prod:
    delete tt-rep-prod.
end.
for each tt-erro:
    delete tt-erro.
end.
for each tt-param:
    delete tt-param.
end.
for each tt-refugo:
    delete tt-refugo.
end.
for each tt-rep-oper-ctrab:
    delete tt-rep-oper-ctrab.
end.
for each tt-apont-mob:
    delete tt-apont-mob.
end.

FOR EACH tt-res-neg.
    DELETE tt-res-neg.
END.

END PROCEDURE.


PROCEDURE processaReportes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var lUndo as log no-undo init no.

find first param-global no-lock no-error.
find first param-cq no-lock no-error.
find first param-cp no-lock no-error.
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "Reportando...").

deTotal = 0.


for each tt-reporta:
    for each tt-lote-reporte where
             tt-lote-reporte.nr-ord-produ = tt-reporta.nr-ord-produ:

        IF l-peso-bal THEN DO:
          if search("m:\dts\LOG_prd\essf0022.LOG") <> ? then do:
                OUTPUT TO m:\dts\LOG_prd\essf0022.LOG APPEND.

                    DISP "Prog: essf0002 " FORMAT "x(19)" string(TODAY) "-" 
                         string(TIME,"hh:mm:ss") "-" 
                         " Logado: " string(c-seg-usuario) SKIP 
                         WITH WIDTH 300.
                    DISP  tt-lote-reporte.lote tt-lote-reporte.quantidade tt-lote-reporte.nr-ord-produ 
                         WITH WIDTH 300.

                    OUTPUT CLOSE.
            end.        

               
         END.

        assign deTotal = deTotal + tt-lote-reporte.quantidade.

        IF tt-reporta.cod-depos = "cq" THEN DO:
           if search("m:\dts\LOG_prd\essf0022.cq") <> ?  then do:
                OUTPUT TO m:\dts\LOG_prd\essf0022.cq APPEND.
                
       

                    put "Prog: essf0022-ANTES" FORMAT "x(19)" string(TODAY) "-" 
                         string(TIME,"hh:mm:ss") "-" 
                         " Logado: " string(c-seg-usuario) SKIP 
                        .
                        
                        put tt-reporta.cod-depos
                             " - Ordem:" tt-reporta.nr-ord-produ
                             " - Item:" tt-reporta.it-codigo
                             " - Qtde:" tt-reporta.qt-reporte 
                             " - lote:" tt-lote-reporte.lote
                             " - peso:" tt-lote-reporte.quantidade skip.
                             
                
                output close.
               end.          
             
         end.
    end.
    

    
end.
assign deRefUni  = 0
       deLote    = saldo-lote-cons    
       deEmenda1 = saldo-emenda1-cons 
       deEmenda2 = saldo-emenda2-cons.

run cpp/cpapi001.p persistent set hReporte (input-output table tt-rep-prod,
                                            input        table tt-refugo,
                                            input        table tt-res-neg,
                                            input        table tt-apont-mob,
                                            input-output table tt-erro,
                                            input        true).

run cpp/cpapi012.p persistent set hRequis (input        table tt-requis,
                                           input-output table tt-erro,
                                           input        false).

run inbo/boin536.p persistent set hRepSfc.

for each tt-digita:
    delete tt-digita.
end.

assign lErro = false.

reportes:
do trans on error undo, leave:
        
  if lErro = false then do:
    reportes:
    for each tt-reporta
        break by tt-reporta.nr-ord-produ:

            FOR EACH  reservas WHERE
                   reservas.nr-ord-produ = tt-reporta.nr-ord-produ .
                 reservas.estado   = 2.
            END.


            FOR EACH tt-lote-cons NO-LOCK.

              FOR EACH  reservas WHERE
                   reservas.nr-ord-produ = tt-reporta.nr-ord-produ   AND
                   reservas.it-codigo    = tt-lote-cons.it-codigo AND
                   reservas.cod-refer    = tt-lote-cons.cod-refer.
                  reservas.estado = 1.
                  reservas.lote-serie   = "".


              END.

              FIND FIRST  reservas WHERE
                     reservas.nr-ord-produ = tt-reporta.nr-ord-produ   AND
                     reservas.it-codigo    = tt-lote-cons.it-codigo  AND
                     reservas.cod-refer    = tt-lote-cons.cod-refer  AND
                     reservas.estado       = 1 NO-LOCK NO-ERROR.

                    IF NOT AVAIL reservas  THEN DO:

              FIND FIRST  reservas WHERE
               reservas.nr-ord-produ = tt-reporta.nr-ord-produ   AND
               reservas.it-codigo    = tt-lote-cons.it-codigo AND
               trim(reservas.cod-refer) <> "" NO-LOCK NO-ERROR.

              IF AVAIL reservas THEN DO:

                      FIND FIRST  bf-reservas WHERE
                           bf-reservas.nr-ord-produ = tt-reporta.nr-ord-produ   AND
                           bf-reservas.it-codigo    = tt-lote-cons.it-codigo AND
                           bf-reservas.cod-refer    = tt-lote-cons.cod-refer NO-ERROR.
    
                      IF NOT AVAIL bf-reservas AND tt-lote-cons.cod-refer <> "" THEN DO:
                          CREATE bf-reservas.
                          BUFFER-COPY reservas EXCEPT cod-refer  estado TO bf-reservas
                              ASSIGN bf-reservas.cod-refer = tt-lote-cons.cod-refer
                                     bf-reservas.estado = 1.
    
                      END.
                  
                  END.
    
                  FIND FIRST  reservas WHERE
                   reservas.nr-ord-produ = tt-reporta.nr-ord-produ   AND
                   reservas.it-codigo    = tt-lote-cons.it-codigo AND
                   reservas.cod-refer    = tt-lote-cons.cod-refer AND
                   reservas.estado = 1 NO-LOCK NO-ERROR.
    
                  IF NOT AVAIL reservas  THEN DO:
                       run utp/ut-msgs.p (input "show",
                                 input  17006,
                                 input "Verifique as reservas cadastradas na ordem e se est∆o ativas!").
                      assign lErro = true.
                      undo reportes, leave reportes.
    
                  END.
    
                  
    
              END.
            END.


        


          









        find first ord-prod no-lock where
                   ord-prod.nr-ord-produ = tt-reporta.nr-ord-produ.
        find first item no-lock where
                   item.it-codigo = ord-prod.it-codigo.
        assign cInformCompl = "":U.
        if avail item then do:
           find first item-uni-estab
                where item-uni-estab.cod-estabel = ord-prod.cod-estabel 
                  and item-uni-estab.it-codigo   = item.it-codigo no-lock no-error.

           if avail item-uni-estab then do:
              if item-uni-estab.ind-refugo = 2 then
                 assign cInformCompl = item.inform-compl.
           end.
        end.
        find first lin-prod no-lock where
                   lin-prod.nr-linha    = ord-prod.nr-linha and
                   lin-prod.cod-estabel = ord-prod.cod-estabel.

        assign iSeqEtq = 0.
        for each tt-lote-reporte where
                 tt-lote-reporte.nr-ord-produ = tt-reporta.nr-ord-produ break by tt-lote-reporte.lote:
        
            assign iSeqEtq = iSeqEtq + 1.

            run eliminaTT.

            assign lErro = false.
    
            run createRepProd.


            /* tentativa de evitar erro troca de referencia de ordens 16/04/2007 - edson */
            IF tt-reporta.nr-pedcli <> "" THEN DO:   
                  FIND FIRST ped-item 
                        WHERE ped-item.nome-abrev   = tt-reporta.nome-abrev
                          AND ped-item.nr-pedcli    = tt-reporta.nr-pedcli
                          AND ped-item.nr-sequencia = ord-prod.nr-sequencia
                          AND ped-item.it-codigo    = tt-reporta.it-codigo
                                NO-LOCK USE-INDEX ch-item-ped NO-ERROR.

                    IF AVAIL ped-item THEN DO:
                         find CURRENT ord-prod exclusive-lock no-error.
                         IF AVAIL ord-prod THEN
                              ASSIGN ord-prod.cod-refer    = ped-item.cod-refer
                                     tt-rep-prod.cod-refer = ped-item.cod-refer.

                         find first ord-prod no-lock where
                              ord-prod.nr-ord-produ = tt-reporta.nr-ord-produ NO-ERROR.


                    END.

            END.
            ELSE DO:
                    IF   AVAIL ITEM AND item.tipo-con-est = 4 THEN DO:

                        find CURRENT ord-prod EXCLUSIVE-LOCK NO-ERROR.

                        IF AVAIL ord-prod AND 
                                 ord-prod.nr-estrut <> DEC(ord-prod.cod-refer) AND 
                                 ord-prod.nr-estrut <> 1 THEN
                                      ASSIGN ord-prod.cod-refer    = string(ord-prod.nr-estrut,"99999999")
                                         tt-rep-prod.cod-refer = ord-prod.cod-refer.
                                         
                         IF AVAIL ord-prod AND 
                                 ord-prod.nr-estrut <> DEC(ord-prod.cod-refer) AND 
                                 ord-prod.nr-estrut = 1 THEN
                                      ASSIGN ord-prod.nr-estrut = int(ord-prod.cod-refer)
                                         tt-rep-prod.cod-refer = ord-prod.cod-refer.
                 
                        
                        find first ord-prod no-lock WHERE ord-prod.nr-ord-produ = tt-reporta.nr-ord-produ NO-ERROR.

                    END.
                        

            END.
            /* fim tentativa de evitar erro de troca de referencia */
    
          
           /*Ajusta o  deposito de CQ*/
           
            IF tt-rep-prod.cod-depos = "cq" THEN DO:
                tt-rep-prod.cod-depos = "pro" .
            end.
            
           IF param-global.modulo-cq and 
              param-cq.tipo-cq       > 1 and
              STRING (f-item-uni-estab(tt-reporta.it-codigo, 
                                      IF c-cod-estabel:SCREEN-VALUE IN FRAME f-relat = '' THEN ord-prod.cod-estabel
                                      ELSE c-cod-estabel:SCREEN-VALUE IN FRAME f-relat  ,
                                      "contr-qualid":U)) = "yes":U THEN
               FOR FIRST estabelec FIELDS (deposito-cq) WHERE 
                         estabelec.cod-estabel = IF c-cod-estabel:SCREEN-VALUE IN FRAME f-relat   = ''
                                                 THEN ord-prod.cod-estabel
                                                 ELSE c-cod-estabel:SCREEN-VALUE IN FRAME f-relat    NO-LOCK:
                         assign tt-rep-prod.cod-depos = estabelec.deposito-cq.
               END.
          
           
            IF tt-rep-prod.cod-depos = "cq" THEN DO:
                if search("m:\dts\LOG_prd\essf0022.cq") <> ?  then do:

                            OUTPUT TO m:\dts\LOG_prd\essf0022.cq APPEND.
                            
                   
            
                                put "Prog: essf0022-durante" FORMAT "x(19)" string(TODAY) "-" 
                                     string(TIME,"hh:mm:ss") "-" 
                                     " Logado: " string(c-seg-usuario) SKIP 
                                    .
                                    
                                    put "tt-reporta:" tt-reporta.cod-depos 
                                    "- tt-rep-prod:" tt-rep-prod.cod-depos
                                         " - Ordem:" tt-reporta.nr-ord-produ
                                         " - Item report:" tt-reporta.it-codigo
                                          " - Item.it-codigo:" item.it-codigo
                                          " - tem cq:"  STRING (f-item-uni-estab(tt-reporta.it-codigo,
                                      IF c-cod-estabel:SCREEN-VALUE IN FRAME f-relat = '' THEN item.cod-estabel
                                      ELSE c-cod-estabel:SCREEN-VALUE IN FRAME f-relat ,
                                      "contr-qualid":U))
                                      " - Estab-item:" item.cod-estabel
                                         " - Qtde:" tt-reporta.qt-reporte 
                                         " - lote:" tt-rep-prod.lote
                                         " - peso:" tt-rep-prod.qt-reporte skip.
                                         
                            
                            output close.
                     end.                
                         
            END.
                     
            assign tt-rep-prod.qt-reporte = tt-rep-prod.qt-reporte + tt-rep-prod.qt-refugo
                   deQuantNec             = tt-rep-prod.qt-reporte.
            
            if tt-rep-prod.qt-refugo > 0 then do:
               create tt-refugo.
               assign tt-refugo.nr-ord-produ = tt-rep-prod.nr-ord-produ
                      tt-refugo.codigo-rejei = if ord-prod.cod-estabel = "413" OR ord-prod.cod-estabel = "423" then 2 else 1 /*solic-318*/ 
                      tt-refugo.qt-refugo    = tt-rep-prod.qt-refugo.
            
               run pi-recebe-tt-refugo in hReporte (input table tt-refugo).
            end.

            run pi-recebe-tt-erro in hReporte (input table tt-erro).



            run pi-recebe-tt-rep-prod in hReporte (input table tt-rep-prod).


            /* Rotina da nova tabela de rastreabilidade
               JosÇ Roberto 29/03/2005 */

            ASSIGN nr-ord-produ-jr-pro = tt-rep-prod.nr-ord-produ 
                   cod-estabel-jr-pro  = c-cod-estabel:SCREEN-VALUE IN FRAME f-relat
                   it-codigo-jr-pro    = tt-rep-prod.it-codigo
                   lote-jr-pro         = tt-rep-prod.lote   
                   lote-jr-con1        = c-lote-cons:SCREEN-VALUE IN FRAME f-relat  
                   lote-jr-con2        = c-emenda-1:SCREEN-VALUE IN FRAME f-relat  
                   lote-jr-con3        = c-emenda-2:SCREEN-VALUE IN FRAME f-relat . 
                
            RUN sfc/essf0002-esp.p (INPUT nr-ord-produ-jr-pro,
                                         INPUT cod-estabel-jr-pro, 
                                         INPUT it-codigo-jr-pro,   
                                         INPUT lote-jr-pro,    
                                         INPUT lote-jr-con1,       
                                         INPUT lote-jr-con2,       
                                         INPUT lote-jr-con3)
                                         NO-ERROR.

            /*----------------------------------------------------------------*/

            /*MOB-GGF*/
            find first oper-ord
                 where oper-ord.nr-ord-produ = ord-prod.nr-ord-produ no-lock no-error.
            find first split-operac
                 where split-operac.nr-ord-produ   = oper-ord.nr-ord-produ
                   and split-operac.num-operac-sfc = oper-ord.num-operac-sfc exclusive-lock no-error.
            if avail split-operac then do:


               RUN gera-mob-ggf-automatico.


               run pi-recebe-tt-apont-mob in hReporte (input table tt-apont-mob).
            end.
            /*MOB-GGF*/
            run pi-valida-rep-prod in hReporte (input  false,
                                                input  ord-prod.nr-ord-produ,
                                                output cErro,
                                                output cTexto).
            if return-value = 'nok' then do:
                assign lErro = true.
                do iCont = 1 to num-entries (cErro):
                    run criaErro (input int (entry (iCont, cErro)),
                                  input entry (iCont, cTexto)).
                end.
                undo reportes, leave reportes.
            end.
    


            create tt-param.
            assign tt-param.it-codigo  = c-it-codigo-cons:SCREEN-VALUE IN FRAME f-relat
                   tt-param.lote       = c-lote-cons:SCREEN-VALUE IN FRAME f-relat
                   tt-param.emenda1    = c-emenda-1:SCREEN-VALUE IN FRAME f-relat 
                   tt-param.emenda2    = c-emenda-2:SCREEN-VALUE IN FRAME f-relat 
                   tt-param.qt-lote    =  saldo-lote-cons   
                   tt-param.qt-emenda1 =  saldo-emenda1-cons
                   tt-param.qt-emenda2 =  saldo-emenda2-cons.

      
            
            /* Requisita materiais */

           
            case lin-prod.sum-requis:
                when 1 then do:
                    assign tt-param.h-handle = hReporte.
                    run sfc/essf0002c.p (input-output table tt-param,
                                              input        tt-rep-prod.qt-reporte).
                end.
                when 2 then do:
                    assign tt-param.h-handle = hRequis.
                    run sfc/essf0002b.p (input-output table tt-param,
                                              input        table tt-rep-prod,
                                              input-output table tt-erro).
                    if return-value = 'nok' then do:
                        assign lErro = true.
                        undo reportes, leave reportes.
                    end.
                end.
            end case.

            find first tt-param.
            assign deLote    = tt-param.qt-lote
                   deEmenda1 = tt-param.qt-emenda1
                   deEmenda2 = tt-param.qt-emenda2.
            /*2 - tt-rep-prod*/
            run pi-processa-reportes in hReporte (input-output table tt-rep-prod,
                                                  input        table tt-refugo,
                                                  input        table tt-res-neg,
                                                  input-output table tt-erro,
                                                  input        yes,
                                                  input        yes).
    
            if temp-table tt-erro:has-records then do:
                assign lErro = true.
                for each tt-erro:
                    /*find cad-msgs
                         where cad-msgs.cd-msg = tt-erro.cd-erro no-lock no-error.
                    if avail cad-msgs and 
                       cad-msgs.tipo-msg = 1 /*Erro*/ then*/
                       undo reportes, leave reportes.
                end.
            end.

            run pi-formatted-time-to-sec (input  replace(c-hr-inic-prod:SCREEN-VALUE IN FRAME f-relat,":",""),
                                          output deSegIni).
            run pi-formatted-time-to-sec (input  replace(c-hr-fim-prod:SCREEN-VALUE IN FRAME f-relat,":",""),
                                          output deSegFim).
            find last rep-oper-ctrab no-lock where 
                      rep-oper-ctrab.nr-ord-produ = ord-prod.nr-ord-produ no-error.
            find last oper-ord no-lock where 
                      oper-ord.nr-ord-produ = ord-prod.nr-ord-produ no-error.
            find first split-operac no-lock where 
                       split-operac.nr-ord-produ   = oper-ord.nr-ord-produ and 
                       split-operac.num-operac-sfc = oper-ord.num-operac-sfc no-error.
            
            run createRepOperCtrab.
            
            run GerarRepOperCtrab in hRepSfc (input table tt-rep-oper-ctrab,
                                              input table tt-rep-refugo-oper,
                                              input table tt-rep-ic-oper,
                                              input table tt-rep-ic-oper-tab).

            /*Caracteristicas do Lote reportado*/
            find first tt-rep-oper-ctrab no-error.

            IF NOT AVAIL tt-rep-oper-ctrab THEN  RUN pi-grava-erro-log-ctl (INPUT 
                                                                           string(TODAY) + "-" + string(TIME,"hh:mm:ss") + "-" + 
                                                                           " Logado: " +  string(c-seg-usuario) +
                                                                           "Erro tt-rep-oper-ctrab Ordem:" + 
                                                                           STRING(tt-lote-reporte.nr-ord-produ) + " - Item:" + 
                                                                           tt-reporta.it-codigo + " - Lote:" +     tt-lote-reporte.lote
                                                                           
                                                                           ).
            if avail tt-rep-oper-ctrab then do:
               find first rep-oper-ctrab
                    where rep-oper-ctrab.nr-ord-produ = tt-rep-oper-ctrab.nr-ord-produ
                      and rep-oper-ctrab.num-seq-rep  = tt-rep-oper-ctrab.num-seq-rep no-lock no-error.

               IF NOT AVAIL rep-oper-ctrab THEN RUN pi-grava-erro-log-ctl (INPUT 
                                                                           string(TODAY) + "-" + string(TIME,"hh:mm:ss") + "-" + 
                                                                           " Logado: " +  string(c-seg-usuario) +
                                                                           "Erro rep-oper-ctrab Ordem:" + 
                                                                           STRING(tt-rep-oper-ctrab.nr-ord-produ) + " - Seq:" + 
                                                                           STRING(tt-rep-oper-ctrab.num-seq-rep) + " - Item:" + 
                                                                           tt-reporta.it-codigo + " - Lote:" +     tt-lote-reporte.lote
                                                                           
                                                                           ).

               find last movto-mat
                   where movto-mat.nr-ord-produ = rep-oper-ctrab.nr-ord-produ and
                         movto-mat.esp-docto = 1  no-lock use-index ordem no-error.

                 IF NOT AVAIL movto-mat THEN  RUN pi-grava-erro-log-ctl (INPUT 
                                                                           string(TODAY) + "-" + string(TIME,"hh:mm:ss") + "-" + 
                                                                           " Logado: " +  string(c-seg-usuario) +
                                                                           "Erro movto-mat Ordem:" + 
                                                                           STRING(tt-lote-reporte.nr-ord-produ) + " - Item:" + 
                                                                           tt-reporta.it-codigo + " - Lote:" +     tt-lote-reporte.lote
                                                                           
                                                                           ).

               if avail movto-mat then do:
                  find item
                      where item.it-codigo = movto-mat.it-codigo no-lock no-error.

                  IF NOT(avail item and
                     item.cd-folh-lote <> "" AND movto-mat.lote <> "RECICL") THEN  RUN pi-grava-erro-log-ctl (INPUT 
                                                                           string(TODAY) + "-" + string(TIME,"hh:mm:ss") + "-" + 
                                                                           " Logado: " +  string(c-seg-usuario) +
                                                                           "cd-folh-lote Ordem:" + 
                                                                           STRING(tt-lote-reporte.nr-ord-produ) + " - Item:" + 
                                                                           tt-reporta.it-codigo + " - Lote:" +     tt-lote-reporte.lote
                                                                           
                                                                           ).

                  if avail item and
                     item.cd-folh-lote <> "" AND movto-mat.lote <> "RECICL" then do:
                     run atualizaLoteItem.
                  end.
               end.


            end.
        end.

        FOR EACH  reservas WHERE
                   reservas.nr-ord-produ = tt-reporta.nr-ord-produ .
                 reservas.estado   = 1.
        END.
    end.
    end. /*lErro*/

    /*Novo*/
    if lErro then do:
        for each tt-erro2:
            delete tt-erro2.
        end.
    
        for each tt-erro:
           /*find cad-msgs
                where cad-msgs.cd-msg = tt-erro.cd-erro no-lock no-error.
           if avail cad-msgs and 
              cad-msgs.tipo-msg > 1 then do:*/
              create tt-erro2.
              buffer-copy tt-erro to tt-erro2.
              delete tt-erro.
           /*end.*/
        end.
    
        run pi-finalizar in h-acomp.
    
        if can-find(first tt-erro) then do:
           run cdp/cd0666.w (input table tt-erro).
           assign lUndo = yes.
           undo reportes, leave reportes.
           /*return 'nok'.*/
        end.
    
        if can-find(first tt-erro2) then do:
           run cdp/cd0666.w (input table tt-erro2).
        end.

        if not valid-handle(h-acomp) then do:
            run utp/ut-acomp.p persistent set h-acomp.
            run pi-inicializar in h-acomp (input "Continuando...").
        end.

        lErro = false.
    end. /* tt-reporta*/
/*volta situacao reservas original*/

end.

run pi-finalizar in hReporte.
run pi-finalizar in hRequis.

if valid-handle(h-acomp) then
   run pi-finalizar in h-acomp.

delete procedure hRepSfc.


if lUndo = yes then 
   return "NOK":U.

/*Se nao encontrou erro entao imprime as etiquetas
if not can-find(first tt-lote-reporte
                where tt-lote-reporte.nr-ord-produ = tt-reporta.nr-ord-produ) and
   deTotRefugo > 0  then do:
   run utp/ut-msgs.p (input "show",
                      input 15825,
                      input "Resta refugar " + string(deTotRefugo, "->>>>>,>>9.9999") + "!"). 
end.*/
FIND FIRST tt-lote-reporte NO-LOCK NO-ERROR.

if AVAIL  tt-lote-reporte AND tt-lote-reporte.lote <> "RECICL" then do:
   run utp/ut-msgs.p (input "show",
                      input 27100,
                      input "Deseja imprimir a(s) Etiqueta(s)?").
   if return-value = "yes" then do:
      run pi-etiqueta.
   end.
end.

return 'ok'.


END PROCEDURE.


PROCEDURE createRepProd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    
    create tt-rep-prod.
assign tt-rep-prod.tipo                  = 1
       tt-rep-prod.nr-ord-produ          = tt-reporta.nr-ord-produ
       tt-rep-prod.it-codigo             = ord-prod.it-codigo
       tt-rep-prod.un                    = item.un
       tt-rep-prod.ct-codigo             = ord-prod.ct-codigo
       tt-rep-prod.sc-codigo             = ord-prod.sc-codigo
       tt-rep-prod.data                  = input FRAME f-relat dt-trans-fim 
       tt-rep-prod.qt-reporte            = tt-lote-reporte.quantidade
       tt-rep-prod.cod-refer             = ord-prod.cod-refer
       tt-rep-prod.cod-depos             = tt-reporta.cod-depos
       tt-rep-prod.cod-localiz           = tt-reporta.cod-localiz
       tt-rep-prod.cod-depos-sai         = ?
       tt-rep-prod.cod-local-sai         = ?
       tt-rep-prod.dep-refugo            = substring(param-cp.char-2,1,3)
       tt-rep-prod.loc-refugo            = cInformCompl
       tt-rep-prod.lote-serie            = tt-lote-reporte.lote
       tt-rep-prod.dt-vali-lote          = tt-lote-reporte.dt-vali-lote
       tt-rep-prod.procura-saldos        = false
       tt-rep-prod.carrega-reservas      = false
       tt-rep-prod.reserva               = false
       tt-rep-prod.prog-seg              = "essf0022"
       tt-rep-prod.cod-versao-integracao = 001
    .
END PROCEDURE.



PROCEDURE gera-mob-ggf-automatico :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var d-tempo as decimal no-undo.
  DEFINE VARIABLE i-estac AS INTEGER  INITIAL 0  NO-UNDO.

  find first param-sfc no-lock no-error.
  IF NOT AVAIL param-sfc THEN return "NOK":U.

  /* Calcula tempo reporte */
  RUN pi-calcula-tempo-mob-ggf (OUTPUT d-tempo).


    FOR each b-tt-estacao NO-LOCK .
            IF b-tt-estacao.nr-ord-produ <> 0 THEN
              i-estac = i-estac + 1.
          
    
    END.
  /* Gera GGF autom†tico - Carrega automaticamente o browser MOB/GGF do SF0303B 
   com os movimentos de GGF da operaá∆o corrente */
   
   /*
message
"log-ggf"  param-sfc.log-gera-ggf-autom  skip
"d-ord rep ggf"     ord-prod.reporte-ggf  skip
         "tt-apont-mob.hora-fim        "  int(entry(1,c-hr-fim-prod:SCREEN-VALUE IN FRAME f-relat,":"))              skip
         " tt-apont-mob.hora-ini        "  int(entry(1,c-hr-inic-prod:SCREEN-VALUE IN FRAME f-relat,":"))    skip
         " tt-apont-mob.min-fim         "  int(entry(2,c-hr-fim-prod:SCREEN-VALUE IN FRAME f-relat,":"))     skip         
         " tt-apont-mob.min-ini         "  int(entry(2,c-hr-inic-prod:SCREEN-VALUE IN FRAME f-relat,":"))  skip
         " tt-apont-mob.tempo           "  (d-tempo * 60 / i-estac) / 60
         "d-tempo" d-tempo  skip
         "i-estac" i-estac view-as alert-box.
         
         */

IF param-sfc.log-gera-ggf-autom and
   ord-prod.reporte-ggf = 1 THEN DO:  
   
   CREATE tt-apont-mob.
   assign tt-apont-mob.nr-ord-prod     =  split-operac.nr-ord-produ
          tt-apont-mob.op-codigo       =  split-operac.op-codigo
          tt-apont-mob.it-codigo       =  split-operac.it-codigo
          tt-apont-mob.cod-roteiro     =  split-operac.cod-roteiro
          tt-apont-mob.tipo-movto      =  2
          tt-apont-mob.gm-codigo       =  split-operac.gm-codigo
          tt-apont-mob.hora-fim        =  int(entry(1,c-hr-fim-prod:SCREEN-VALUE IN FRAME f-relat,":"))            
          tt-apont-mob.hora-ini        =  int(entry(1,c-hr-inic-prod:SCREEN-VALUE IN FRAME f-relat,":"))  
          tt-apont-mob.min-fim         =  int(entry(2,c-hr-fim-prod:SCREEN-VALUE IN FRAME f-relat,":"))            
          tt-apont-mob.min-ini         =  int(entry(2,c-hr-inic-prod:SCREEN-VALUE IN FRAME f-relat,":"))
          tt-apont-mob.tempo           =  (d-tempo * 60 / i-estac) / 60
          tt-apont-mob.referencia      =  ord-prod.cod-refer.
   /* Rel¢gio Hexadecimal ou centesimal */                                                   

   IF INT(param-sfc.log-tipo-relogio) = 1 THEN
      ASSIGN tt-apont-mob.tipo-relogio = 1.
   ELSE 
      ASSIGN tt-apont-mob.tipo-relogio = 3. 
   
END.

/* Gera MOB autom†tico - Carrega automaticamente o browser MOB/GGF do SF0303B 
   com os movimentos de MOB da operaá∆o corrente */

IF param-sfc.log-gera-mod-autom and
   ord-prod.reporte-mob = 1  THEN DO:
   
    CREATE tt-apont-mob.
    assign tt-apont-mob.nr-ord-prod     =  split-operac.nr-ord-produ
           tt-apont-mob.op-codigo       =  split-operac.op-codigo
           tt-apont-mob.it-codigo       =  split-operac.it-codigo
           tt-apont-mob.cod-roteiro     =  split-operac.cod-roteiro
           tt-apont-mob.tipo-movto      =  1
           tt-apont-mob.cd-mob-dir      =  ""
           tt-apont-mob.gm-codigo       =  split-operac.gm-codigo
           tt-apont-mob.hora-fim        =  int(entry(1,c-hr-fim-prod:SCREEN-VALUE IN FRAME f-relat,":"))            
           tt-apont-mob.hora-ini        =  int(entry(1,c-hr-inic-prod:SCREEN-VALUE IN FRAME f-relat,":"))  
           tt-apont-mob.min-fim         =  int(entry(2,c-hr-fim-prod:SCREEN-VALUE IN FRAME f-relat,":"))            
           tt-apont-mob.min-ini         =  int(entry(2,c-hr-inic-prod:SCREEN-VALUE IN FRAME f-relat,":"))

           tt-apont-mob.tempo           =  d-tempo / i-estac
           tt-apont-mob.referencia      =  ord-prod.cod-refer. 

    

    /* Rel¢gio Hexadecimal ou centesimal */                                                   
    IF INT(param-sfc.log-tipo-relogio) = 1 THEN
       ASSIGN tt-apont-mob.tipo-relogio = 1.
    ELSE 
       ASSIGN tt-apont-mob.tipo-relogio = 3. 
END.
    
RETURN "OK":U.

              
              
END PROCEDURE.



PROCEDURE pi-calcula-tempo-mob-ggf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAM p-tempo AS DECIMAL NO-UNDO.

   def var de-qtd-segs-inic-rep as decimal no-undo.
   def var de-qtd-segs-fim-rep  as decimal no-undo.
   def var de-qtd-tempo-util    as decimal no-undo.
   def var da-ini-aux           as date no-undo.
   def var da-fim-aux           as date no-undo.

   def var v-qtd-tempo-proces  as decimal.
   def var v-qtd-tempo-extra   as decimal.
   def var v-cod-model-turno   as char.
   def var v-num-turno         as integer. 

   def var v-val-refer-inic-rep as decimal no-undo.
   def var v-val-refer-fim-rep  as decimal no-undo.

   def var v-qtd-tempo-parada as decimal no-undo.
   
   if  param-sfc.log-tipo-relogio then do:
          run pi-formatted-time-to-sec (input replace(c-hr-inic-prod:SCREEN-VALUE IN FRAME f-relat,":",""),  
                                        output de-qtd-segs-inic-rep).
          run pi-formatted-time-to-sec (input  replace(c-hr-fim-prod:SCREEN-VALUE IN FRAME f-relat,":",""),
                                        output de-qtd-segs-fim-rep).
      end.
      else
          assign de-qtd-segs-inic-rep = integer(replace(c-hr-inic-prod:SCREEN-VALUE IN FRAME f-relat,":","")) * 36
                 de-qtd-segs-fim-rep  = integer(replace(c-hr-fim-prod:SCREEN-VALUE IN FRAME f-relat,":","")) * 36.

      if  date(dt-trans-ini:SCREEN-VALUE IN FRAME f-relat)  <> ?
      and date(dt-trans-fim:SCREEN-VALUE IN FRAME f-relat) <> ? then do:
          assign de-qtd-tempo-util = ?
                 da-ini-aux =  DATE(dt-trans-ini:SCREEN-VALUE IN FRAME f-relat) 
                 da-fim-aux =  DATE(dt-trans-fim:SCREEN-VALUE IN FRAME f-relat) .
             

             run pi-sfc-reporte-tempo IN hRepSfc (input split-operac.cod-ctrab,  
                                                  input da-fim-aux, 
                                                  input de-qtd-segs-fim-rep, 
                                                  input da-ini-aux, 
                                                  input de-qtd-segs-inic-rep, 
                                                  output v-qtd-tempo-proces, 
                                                  output v-qtd-tempo-extra, 
                                                  output v-cod-model-turno, 
                                                  output v-num-turno). 
                                                                 
             find grup-maquina where
                  grup-maquina.gm-codigo = split-operac.gm-codigo no-lock.
             IF AVAIL grup-maquina THEN DO:
                run pi-converte-data-segs-valor (input  da-ini-aux,
                                                 input  de-qtd-segs-inic-rep,
                                                 output v-val-refer-inic-rep).

                run pi-converte-data-segs-valor (input  da-fim-aux,
                                                 input  de-qtd-segs-fim-rep,
                                                 output v-val-refer-fim-rep).


                if grup-maquina.ind-tip-ctrab = 4  then 
                   run pi-sfc-tempo-paradas-mod IN hRepSfc (input  split-operac.cod-ctrab,
                                                            input  split-operac.gm-codigo,
                                                            input  "",
                                                            input  v-val-refer-inic-rep,
                                                            input  v-val-refer-fim-rep,
                                                            output v-qtd-tempo-parada). 
                else 
                  run pi-sfc-tempo-paradas-maq IN hRepSfc  (input  split-operac.cod-ctrab,
                                                              input  split-operac.gm-codigo,
                                                              input  v-val-refer-inic-rep,
                                                              input  v-val-refer-fim-rep,
                                                              output v-qtd-tempo-parada). 
                                                                   
                ASSIGN p-tempo =   v-qtd-tempo-proces + v-qtd-tempo-extra - v-qtd-tempo-parada.
             END.

      end. 
      RETURN "OK":U.

END PROCEDURE.


PROCEDURE pi-formatted-time-to-sec:

    def  input param p-hra-formatted-time as character format "99:99:99" no-undo.
    def output param p-num-seconds        as integer   format ">>>,>>9"  no-undo.

    assign p-num-seconds = integer(substring(p-hra-formatted-time,1,2)) * 3600
                         + integer(substring(p-hra-formatted-time,3,2)) * 60
                         + integer(substring(p-hra-formatted-time,5,2)).
END PROCEDURE.

PROCEDURE pi-sec-to-formatted-time:

    def  input param p-num-seconds        as integer   format ">>>,>>9"  no-undo.
    def output param p-hra-formatted-time as character format "99:99:99" no-undo.

    assign p-hra-formatted-time = replace(string(p-num-seconds,"hh:mm:ss":U),":","").

END PROCEDURE.

PROCEDURE pi-converte-data-segs-valor:

    def Input param p-dat-refer-1           as date format "99/99/9999"     no-undo.
    def Input param p-qtd-segs-refer-1      as dec  format "->>>>,>>9.9999" decimals 4 no-undo.
    def output param p-val-refer-perf-capac as dec  format "9999999999999"  decimals 0 no-undo.

    assign p-val-refer-perf-capac = decimal(string(year(p-dat-refer-1),"9999") +
                                            string(month(p-dat-refer-1),"99")  +
                                            string(day(p-dat-refer-1),"99")    +
                                            string(p-qtd-segs-refer-1,"99999")).
END PROCEDURE.

PROCEDURE pi-converte-valor-data-segs:

    def input param p-val-refer-perf-capac as dec  format "9999999999999"  decimals 0 no-undo.
    def output param p-dat-refer-1           as date format "99/99/9999"     no-undo.
    def output param p-qtd-segs-refer-1      as dec  format "->>>>,>>9.9999" decimals 4 no-undo.

    def var v-cod-refer-dat as char format "9999999999999".

    assign v-cod-refer-dat     = string(p-val-refer-perf-capac)
           p-dat-refer-1       = 
                                    date(substr(v-cod-refer-dat,7,2) + "/" +
                                         substr(v-cod-refer-dat,5,2) + "/" +
                                         substr(v-cod-refer-dat,1,4))
                                 
           p-qtd-segs-refer-1  = integer(substr(v-cod-refer-dat,9,5)).
END PROCEDURE.


PROCEDURE createRepOperCtrab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
create tt-rep-oper-ctrab.
assign tt-rep-oper-ctrab.cod-ferr-prod         = ""
       tt-rep-oper-ctrab.dat-fim-setup         = ?
       tt-rep-oper-ctrab.dat-inic-setup        = ?
       tt-rep-oper-ctrab.qtd-segs-fim-setup    = 0
       tt-rep-oper-ctrab.qtd-segs-inic-setup   = 0
       tt-rep-oper-ctrab.dat-fim-reporte       = DATE(dt-trans-fim:SCREEN-VALUE IN FRAME f-relat)
       tt-rep-oper-ctrab.dat-inic-reporte      = DATE(dt-trans-ini:SCREEN-VALUE IN FRAME f-relat)
       tt-rep-oper-ctrab.qtd-operac-refgda     = 0
       tt-rep-oper-ctrab.qtd-operac-aprov      = deQuantNec
       tt-rep-oper-ctrab.qtd-operac-reptda     = deQuantNec
       tt-rep-oper-ctrab.qtd-operac-retrab     = 0
       tt-rep-oper-ctrab.qtd-segs-fim-reporte  = deSegFim
       tt-rep-oper-ctrab.qtd-segs-inic-reporte = deSegIni
       tt-rep-oper-ctrab.cod-equipe            = c-cod-operador:SCREEN-VALUE IN FRAME f-relat
       tt-rep-oper-ctrab.num-contador-inic     = 0
       tt-rep-oper-ctrab.num-contador-fim      = 0
       /*** ParÉmetros p/ reporte ***/
       tt-rep-oper-ctrab.nr-ord-produ          = split-operac.nr-ord-produ
       tt-rep-oper-ctrab.num-seq-rep           = if avail rep-oper-ctrab then rep-oper-ctrab.num-seq-rep + 1
                                                                         else 1
       tt-rep-oper-ctrab.num-operac-sfc        = split-operac.num-operac-sfc
       tt-rep-oper-ctrab.num-split-oper        = split-operac.num-split-oper
       tt-rep-oper-ctrab.cod-ctrab             = c-cod-ctrab:SCREEN-VALUE IN FRAME f-relat.

END PROCEDURE.

PROCEDURE pi-deltarho.

    d-durezamaior = 0.
    d-durezamenor = 9999999.

    IF DEC(i-dureza-e:SCREEN-VALUE IN FRAME f-relat) > d-durezamaior THEN
        d-durezamaior = DEC(i-dureza-e:SCREEN-VALUE IN FRAME f-relat).

    IF DEC(i-dureza-e:SCREEN-VALUE IN FRAME f-relat) < d-durezamenor THEN
        d-durezamenor = DEC(i-dureza-e:SCREEN-VALUE IN FRAME f-relat).

    IF DEC(i-dureza-c:SCREEN-VALUE IN FRAME f-relat) > d-durezamaior THEN
        d-durezamaior = DEC(i-dureza-c:SCREEN-VALUE IN FRAME f-relat).

    IF DEC(i-dureza-c:SCREEN-VALUE IN FRAME f-relat) < d-durezamenor THEN
        d-durezamenor = DEC(i-dureza-c:SCREEN-VALUE IN FRAME f-relat).

    IF DEC(i-dureza-d:SCREEN-VALUE IN FRAME f-relat) > d-durezamaior THEN
        d-durezamaior = DEC(i-dureza-d:SCREEN-VALUE IN FRAME f-relat).

    IF DEC(i-dureza-d:SCREEN-VALUE IN FRAME f-relat) < d-durezamenor THEN
        d-durezamenor = DEC(i-dureza-d:SCREEN-VALUE IN FRAME f-relat).

   ASSIGN d-deltarho = IF d-durezamaior = 0 THEN 0 ELSE (d-durezamaior - d-durezamenor).

   i-delta:SCREEN-VALUE IN FRAME f-relat  = STRING(d-deltarho).

END PROCEDURE.

PROCEDURE atualizaLoteItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


find first lote-item
     where lote-item.it-codigo = tt-reporta.it-codigo /*ord-prod.it-codigo*/
       and lote-item.lote      = tt-lote-reporte.lote /*movto-mat.lote*/ exclusive-lock no-error.

 IF NOT avail lote-item THEN  RUN pi-grava-erro-log-ctl (INPUT 
                                                                           string(TODAY) + "-" + string(TIME,"hh:mm:ss") + "-" + 
                                                                           " Logado: " +  string(c-seg-usuario) +
                                                                           "lote-item Ordem:" + 
                                                                           STRING(tt-lote-reporte.nr-ord-produ) + " - Item:" + 
                                                                           tt-reporta.it-codigo + " - Lote:" +     tt-lote-reporte.lote
                                                                           
                                                                           ).

   RUN pi-deltarho.

  
if avail lote-item then do:
    /*Etiqueta da OP reportada*/
    assign lote-item.dt-vali-lote = tt-lote-reporte.dt-vali-lote.
 
    IF NOT CAN-FIND(FIRST  lote-carac-tec WHERE 
             lote-carac-tec.it-codigo = lote-item.it-codigo AND
             lote-carac-tec.lote      =  lote-item.lote NO-LOCK) THEN
                               RUN pi-grava-erro-log-ctl (INPUT 
                                                                           string(TODAY) + "-" + string(TIME,"hh:mm:ss") + "-" + 
                                                                           " Logado: " +  string(c-seg-usuario) +
                                                                           "lote-carac-tec Ordem:" + 
                                                                           STRING(tt-lote-reporte.nr-ord-produ) + " - Item:" + 
                                                                           tt-reporta.it-codigo + " - Lote:" +     tt-lote-reporte.lote
                                                                           
                                                                           ).


    FOR EACH lote-carac-tec WHERE 
             lote-carac-tec.it-codigo = lote-item.it-codigo AND
             lote-carac-tec.lote =  lote-item.lote .

        IF lote-carac-tec.cd-comp = "TURNO" THEN
            lote-carac-tec.vl-result = DEC(i-turno:SCREEN-VALUE IN FRAME f-relat).

        IF lote-carac-tec.cd-comp = "MAQ" THEN
            lote-carac-tec.vl-result = DEC(i-maquina:SCREEN-VALUE IN FRAME f-relat).

        IF lote-carac-tec.cd-comp = "TURMA" THEN 
            lote-carac-tec.observacao = c-turma:SCREEN-VALUE IN FRAME f-relat.

        IF lote-carac-tec.cd-comp = "DOFF" THEN 
            lote-carac-tec.vl-result = DEC(i-nr-doff:SCREEN-VALUE IN FRAME f-relat).

        IF lote-carac-tec.cd-comp = "LARGURA" THEN 
            lote-carac-tec.vl-result = DEC(i-largura:SCREEN-VALUE IN FRAME f-relat).

        IF lote-carac-tec.cd-comp = "DIIN" THEN 
            lote-carac-tec.vl-result = DEC(i-diin:SCREEN-VALUE IN FRAME f-relat).

        IF lote-carac-tec.cd-comp = "DIEX" THEN 
        assign 
            lote-carac-tec.vl-result = DEC(i-diex:SCREEN-VALUE IN FRAME f-relat)
             lote-carac-tec.observacao = i-diex:SCREEN-VALUE IN FRAME f-relat.

 
        IF lote-carac-tec.cd-comp = "ESPES" THEN 
            lote-carac-tec.vl-result = DEC(i-espessura:SCREEN-VALUE IN FRAME f-relat).

        IF lote-carac-tec.cd-comp = "COMPR" THEN 
            lote-carac-tec.vl-result = DEC(i-compr:SCREEN-VALUE IN FRAME f-relat).

        IF lote-carac-tec.cd-comp = "DUREZAE" THEN 
            lote-carac-tec.vl-result = DEC(i-dureza-e:SCREEN-VALUE IN FRAME f-relat).

        IF lote-carac-tec.cd-comp = "DUREZAC" THEN 
            lote-carac-tec.vl-result = DEC(i-dureza-c:SCREEN-VALUE IN FRAME f-relat).

        IF lote-carac-tec.cd-comp = "DUREZAD" THEN 
            lote-carac-tec.vl-result = DEC(i-dureza-D:SCREEN-VALUE IN FRAME f-relat).

        IF lote-carac-tec.cd-comp = "DELTARHO" THEN 
            lote-carac-tec.vl-result = DEC(i-delta:SCREEN-VALUE IN FRAME f-relat).

        IF lote-carac-tec.cd-comp = "LARGUTIL" THEN 
            lote-carac-tec.vl-result = DEC(i-larg-util:SCREEN-VALUE IN FRAME f-relat).

        IF lote-carac-tec.cd-comp = "ESTACAO" THEN 
            lote-carac-tec.vl-result = DEC(i-estacao:SCREEN-VALUE IN FRAME f-relat).


        IF lote-carac-tec.cd-comp = "EMENDA" THEN 
            lote-carac-tec.vl-result = DEC(i-emendas:SCREEN-VALUE IN FRAME f-relat).

        IF lote-carac-tec.cd-comp = "DEFPRI" THEN DO:

            FOR EACH lote-res-carac
                                     where lote-res-carac.cd-comp    = lote-carac-tec.cd-comp 
                                       and lote-res-carac.cd-folha   = lote-carac-tec.cd-folh
                                       and lote-res-carac.it-codigo  = lote-carac-tec.it-codigo
                                       and lote-res-carac.lote       = lote-carac-tec.lote
                                       and lote-res-carac.nr-tabela  = lote-carac-tec.nr-tabela .

                DELETE lote-res-carac.
            END.

            FOR EACH tt-defpri.

                FIND FIRST lote-res-carac
                                     where lote-res-carac.cd-comp    = lote-carac-tec.cd-comp 
                                       and lote-res-carac.cd-folha   = lote-carac-tec.cd-folh
                                       and lote-res-carac.it-codigo  = lote-carac-tec.it-codigo
                                       and lote-res-carac.lote       = lote-carac-tec.lote
                                       and lote-res-carac.nr-tabela  = lote-carac-tec.nr-tabela 
                                       and lote-res-carac.sequencia  =  tt-defpri.cod-def NO-ERROR.

                IF NOT AVAIL lote-res-carac  THEN DO:
                    CREATE lote-res-carac.

                    ASSIGN 
                        lote-res-carac.cd-comp    = lote-carac-tec.cd-comp 
                        lote-res-carac.cd-folha   = lote-carac-tec.cd-folh
                        lote-res-carac.it-codigo  = lote-carac-tec.it-codigo
                        lote-res-carac.lote       = lote-carac-tec.lote
                        lote-res-carac.nr-tabela  = lote-carac-tec.nr-tabela 
                        lote-res-carac.sequencia  = tt-defpri.cod-def .
                END.
            END.                                                       
        END.

        IF lote-carac-tec.cd-comp = "DEFSEC" THEN DO:

            FOR EACH lote-res-carac
                                     where lote-res-carac.cd-comp    = lote-carac-tec.cd-comp 
                                       and lote-res-carac.cd-folha   = lote-carac-tec.cd-folh
                                       and lote-res-carac.it-codigo  = lote-carac-tec.it-codigo
                                       and lote-res-carac.lote       = lote-carac-tec.lote
                                       and lote-res-carac.nr-tabela  = lote-carac-tec.nr-tabela .

                DELETE lote-res-carac.
            END.

            FOR EACH tt-defsec.

                FIND FIRST lote-res-carac
                                     where lote-res-carac.cd-comp    = lote-carac-tec.cd-comp 
                                       and lote-res-carac.cd-folha   = lote-carac-tec.cd-folh
                                       and lote-res-carac.it-codigo  = lote-carac-tec.it-codigo
                                       and lote-res-carac.lote       = lote-carac-tec.lote
                                       and lote-res-carac.nr-tabela  = lote-carac-tec.nr-tabela 
                                       and lote-res-carac.sequencia  =  tt-defsec.cod-def NO-ERROR.

                IF NOT AVAIL lote-res-carac  THEN DO:
                    CREATE lote-res-carac.

                    ASSIGN 
                        lote-res-carac.cd-comp    = lote-carac-tec.cd-comp 
                        lote-res-carac.cd-folha   = lote-carac-tec.cd-folh
                        lote-res-carac.it-codigo  = lote-carac-tec.it-codigo
                        lote-res-carac.lote       = lote-carac-tec.lote
                        lote-res-carac.nr-tabela  = lote-carac-tec.nr-tabela 
                        lote-res-carac.sequencia  = tt-defsec.cod-def .


                END.
            END.  /*for each tt-def sec*/                      

        END. /*if defsec*/                                     

    END. /*for each caracteristica do lote*/
    


    create tt-digita.
    assign tt-digita.nr-ord-produ = tt-rep-oper-ctrab.nr-ord-produ
          tt-digita.cod-estabel  = ""
          tt-digita.nr-linha     = 0
          tt-digita.rw-lote-item = rowid(lote-item)
          tt-digita.arquivo      = "etq" + string(tt-rep-oper-ctrab.nr-ord-produ) + "_" + string(iSeqEtq) + ".lst".
    
    assign grw-lote-item  = rowid(lote-item)
           gc-estado      = "TravaBarra":U.
    
    run pi-finalizar in h-acomp.
    
    assign C-Win:sensitive = no.
    run sfc\essf0013.w.
    assign C-Win:sensitive = yes.
    
    run utp/ut-acomp.p persistent set h-acomp.
    run pi-inicializar in h-acomp (input "Reportando...").

end. 

END PROCEDURE.




PROCEDURE pi-gera-pallet:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF var de-qt-palete AS DECIMAL NO-UNDO.
DEF VAR i-nr-ped     LIKE ped-venda.nr-pedido NO-UNDO.
def var i-nr-seq     like ped-item.nr-sequencia no-undo.
DEF VAR c-nr-pallet  LIKE pallet.nr-pallet NO-UNDO.
DEF VAR r-pallet     AS ROWID NO-UNDO.
DEF VAR c-pallet-criado AS CHAR NO-UNDO.                                   
FIND FIRST pol-param-estab
    WHERE pol-param-estab.cod-estabel = c-cod-estabel:SCREEN-VALUE IN FRAME f-relat NO-LOCK NO-ERROR.

for each tt-reporta
    WHERE tt-reporta.gera-pallet = "*":
    
    ASSIGN de-qt-palete = 0
           c-pallet-criado = "".

    FIND FIRST ord-prod
        WHERE ord-prod.nr-ord-prod = tt-reporta.nr-ord-produ NO-LOCK NO-ERROR.

    IF tt-reporta.nr-pedcli <> "" THEN DO:
        
        FIND ped-item 
            WHERE ped-item.nome-abrev   = tt-reporta.nome-abrev
              AND ped-item.nr-pedcli    = tt-reporta.nr-pedcli
              AND ped-item.nr-sequencia = ord-prod.nr-sequencia
              AND ped-item.it-codigo    = tt-reporta.it-codigo
              AND ped-item.cod-refer    = ord-prod.cod-refer NO-LOCK NO-ERROR.

        IF AVAIL ped-item THEN DO:
            FIND FIRST ped-venda
                WHERE ped-venda.nome-abrev = ped-item.nome-abrev
                  AND ped-venda.nr-pedcli = ped-item.nr-pedcli NO-LOCK NO-ERROR.
            FIND FIRST cot-est-mast
                WHERE cot-est-mast.item-cotacao = ped-item.it-codigo 
                  AND cot-est-mast.nr-estrut    = ped-item.nr-config NO-LOCK NO-ERROR.
    
            IF AVAIL cot-est-mast THEN DO:
              /*Quantidade de unidades no Palete */
                FIND  var-result 
                WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                  AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                  AND var-result.nome-var     = "BOBPALETE"  NO-LOCK no-error.
                IF AVAIL var-result THEN 
                   ASSIGN de-qt-palete = DEC(var-result.des-result).
                   
                /*Caso n∆o encontre valor em "BOBPALETE" verificar se tem valor em "CODEMBAL"*/
                IF de-qt-palete = 0 THEN DO:
                    FIND  var-result 
                    WHERE var-result.item-cotacao = cot-est-mast.item-cotacao
                      AND var-result.nr-estrut    = cot-est-mast.nr-estrut
                      AND var-result.nome-var     = "CODEMBAL"  NO-LOCK no-error.
                    IF AVAIL var-result THEN 
                     ASSIGN de-qt-palete =  DEC(SUBSTRING(var-result.des-result,4,2)).
                END.
                
                ASSIGN c-nr-pallet = SUBSTRING(ped-venda.tp-pedido,1,1)
                                   + STRING(ped-venda.nr-pedido)
                       i-nr-ped    = ped-venda.nr-pedido
                       i-nr-seq    = ped-item.nr-sequencia.
            END.
        END.
    END.
    ELSE DO:
        /*Quantidade de unidades no Palete */
        FIND var-result 
            WHERE var-result.item-cotacao = ord-prod.it-codigo
              AND var-result.nr-estrut    = ord-prod.nr-estrut
              AND var-result.nome-var     = "BOBPALETE"  NO-LOCK NO-ERROR.
             
        IF AVAIL var-result then
            ASSIGN de-qt-palete = DEC(var-result.des-result).

        /*Caso n∆o encontre valor em "BOBPALETE" verificar se tem valor em "CODEMBAL"*/
        IF de-qt-palete = 0 THEN DO:
            FIND  var-result 
            WHERE var-result.item-cotacao = ord-prod.it-codigo
              AND var-result.nr-estrut    = ord-prod.nr-estrut
              AND var-result.nome-var     = "CODEMBAL"  NO-LOCK no-error.
            IF AVAIL var-result THEN 
             ASSIGN de-qt-palete =  DEC(SUBSTRING(var-result.des-result,4,2)).

        END.

        ASSIGN c-nr-pallet = "X" + SUBSTRING(string(tt-reporta.nr-ord-produ,"99999999999"),6,6).
    END.
    
    IF de-qt-palete = 0 THEN DO:
       /*palete n∆o ser† gerado quando n∆o possui embalagem na configuraá∆o*/
       run utp/ut-msgs.p (input "show",
                          input 15825,
                          input "Pallet n∆o gerado para a OP " + string(ord-prod.nr-ord-prod) + "!" + "~~" +
                                "N∆o h† embalagem para a configuraá∆o."). 
       NEXT.
    END.
    
    for each tt-lote-reporte 
        WHERE tt-lote-reporte.nr-ord-produ = tt-reporta.nr-ord-produ:
         
        RUN pi-cria-pallet(INPUT i-nr-ped,
                           INPUT i-nr-seq,
                           INPUT c-nr-pallet,
                           INPUT de-qt-palete,
                           OUTPUT r-pallet).
    
        FIND FIRST pallet 
            WHERE ROWID(pallet) = r-pallet NO-LOCK.

        IF pallet.situacao  = 1 AND pallet.nr-bobina < de-qt-palete THEN DO:

        
                find first saldo-estoq
                     where saldo-estoq.cod-estabel  =  pallet.cod-estabel
                       and saldo-estoq.cod-depos    <> substring(param-cp.char-2,1,3)
                       and saldo-estoq.lote         = tt-lote-reporte.lote
                       and saldo-estoq.it-codigo    = pallet.it-codigo
                       and (saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada + saldo-estoq.qt-aloc-prod + saldo-estoq.qt-aloc-ped)) > 0 
                       no-lock no-error.
                if avail saldo-estoq then do:
                    CREATE it-pallet.
                    assign it-pallet.cod-estabel     = pallet.cod-estabel
                           it-pallet.it-codigo       = pallet.it-codigo
                           it-pallet.nr-pallet       = pallet.nr-pallet
                           it-pallet.lote-bobina     = saldo-estoq.lote
                           it-pallet.cod-depos-b     = saldo-estoq.cod-depos   
                           it-pallet.cod-localiz-b   = saldo-estoq.cod-localiz 
                           it-pallet.cod-refer-b     = saldo-estoq.cod-refer   
                           it-pallet.dt-vali-lote-b  = saldo-estoq.dt-vali-lote
                           it-pallet.saldo-bobina    = tt-lote-reporte.quantidade .
                         /*  pallet.nr-bobina          = pallet.nr-bobina + 1
                           pallet.peso-liquido       = pallet.peso-liquido + tt-lote-reporte.quantidade
                           pallet.peso-bruto         = pallet.peso-bruto   + tt-lote-reporte.quantidade */
                end.
                FIND FIRST tt-pallet WHERE
                           tt-pallet.nr-pallet = pallet.nr-pallet NO-LOCK NO-ERROR.
                IF NOT AVAIL tt-pallet THEN DO:
                   CREATE tt-pallet.
                   ASSIGN tt-pallet.nr-pallet    = pallet.nr-pallet
                          tt-pallet.nr-ord-produ = ord-prod.nr-ord-produ
                          tt-pallet.situacao     = 2 /*atualizado com mais bobinas nesta transaá∆o*/.
                END.
        END.
    end.
end.

/*
FOR EACH tt-pallet:
    /*Informar† os pallets criados ou atualizados na transaá∆o*/
    IF tt-pallet.situacao = 1 THEN DO:
        run criaErro (15825,"Criado o Pallet " + tt-pallet.nr-pallet + " para a OP " + string(tt-pallet.nr-ord-produ) + "!"). 
    END.
    ELSE DO:
        run criaErro (15825,"Atualizado o Pallet " + tt-pallet.nr-pallet + " para a OP " + string(tt-pallet.nr-ord-produ) + "!"). 
    END.
END.
*/

if temp-table tt-erro:has-records then do:
   run cdp/cd0666.w (input table tt-erro).
end.

END PROCEDURE.


PROCEDURE pi-cria-pallet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER i-nr-ped     LIKE ped-venda.nr-pedido.
DEF INPUT PARAMETER i-nr-seq     LIKE ped-item.nr-sequencia.
DEF INPUT PARAMETER c-nr-pallet    AS CHAR NO-UNDO.
DEF INPUT parameter de-qt-palete   AS DECIMAL NO-UNDO.
DEF OUTPUT PARAMETER r-pallet      AS ROWID NO-UNDO.

DEF VAR i-seq-pallet     AS INTEGER NO-UNDO.
DEF VAR i-seq-ult-pallet AS INTEGER NO-UNDO.

ASSIGN i-seq-pallet = 0
       i-seq-ult-pallet = 0.

/* Procura palletes gerados por Pedidos e Por Ordens
   com a finalidade de encontrar o £ltimo numero sequencial gerado 
   no codigo chave inteligente da numeraá∆o do Pallet
   Por isto utiliza o nr-pallet, pois o pedido pode ser Zero */

FOR each pallet
    WHERE pallet.nr-pedido = i-nr-ped
      and pallet.nr-sequencia = i-nr-seq
      AND pallet.cod-estab = ord-prod.cod-estabel
      AND pallet.it-codigo = tt-reporta.it-codigo
      AND ENTRY(1,pallet.nr-pallet,"/") = c-nr-pallet NO-LOCK :

/*    ASSIGN i-seq-ult-pallet = DEC(entry(2,pallet.nr-pallet,"/")). */

    IF  pallet.nr-bobina < de-qt-palete 
    AND pallet.situacao = 1 THEN DO:
        ASSIGN i-seq-pallet = DEC(entry(2,pallet.nr-pallet,"/")).
        LEAVE.
    END.
END.


IF i-seq-pallet = 0 THEN DO:

    ASSIGN i-seq-ult-pallet = 0.
        
   
    
       REPEAT:
    
        ASSIGN i-seq-ult-pallet = i-seq-ult-pallet + 1.
        IF i-seq-ult-pallet > 100 THEN DO:
            MESSAGE "PALLET N«O SERµ CRIADO, PROBLEMAS NA NUMERACAO"  VIEW-AS ALERT-BOX.
            RETURN.
        
        END.
        
         FIND FIRST PALLET WHERE PALLET.NR-PALLET = c-nr-pallet + "/" + STRING(i-seq-ult-pallet,"99") NO-LOCK NO-ERROR.
         IF NOT AVAIL PALLET THEN DO:
                 
               c-nr-pallet = c-nr-pallet + "/" + STRING(i-seq-ult-pallet,"99").
               LEAVE.
         END.
       END.        
               
              
    
        
        
    FIND FIRST PALLET WHERE PALLET.NR-PALLET = c-nr-pallet NO-LOCK NO-ERROR.
         IF  AVAIL PALLET THEN DO:
          MESSAGE "PALLET N«O SERµ CRIADO, PROBLEMAS NA NUMERACAO"  VIEW-AS ALERT-BOX.
            RETURN.
        
        END.

   
      
    
    CREATE pallet.
    ASSIGN pallet.cod-estabel    = ord-prod.cod-estabel
           pallet.it-codigo      = tt-reporta.it-codigo
           pallet.nr-pallet      = c-nr-pallet
           pallet.cod-operador   = c-cod-operador:SCREEN-VALUE IN FRAME f-relat 
           pallet.cod-refer      = ord-prod.cod-refer 
           pallet.data-pallet    = TODAY 
           pallet.nr-bobina      = 0
           pallet.situacao       = 1.
    
    IF SUBSTRING(c-nr-pallet,1,1) <> "X" THEN DO:
        
        ASSIGN pallet.nr-pedido    = i-nr-ped /*int(SUBSTRING(c-nr-pallet,2,6))*/
               pallet.nr-sequencia = ord-prod.nr-sequencia. 
               
               if pallet.nr-pedido = 0 then 
                   pallet.nr-sequencia = 0.
    END.
    
    /*grava palletes criados na transaá∆o*/
    FIND FIRST tt-pallet WHERE
               tt-pallet.nr-pallet = pallet.nr-pallet NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-pallet THEN DO:
       CREATE tt-pallet.
       ASSIGN tt-pallet.nr-pallet    = pallet.nr-pallet
              tt-pallet.nr-ord-produ = ord-prod.nr-ord-produ
              tt-pallet.situacao     = 1 /*criado*/.
    END.
    
END.
ELSE ASSIGN  c-nr-pallet = c-nr-pallet + "/" + STRING(i-seq-pallet,"99").

FIND FIRST pallet 
    WHERE pallet.nr-pedido   = i-nr-ped 
      AND pallet.cod-estabel = ord-prod.cod-estabel
      AND pallet.it-codigo   = tt-reporta.it-codigo
      AND pallet.nr-pallet   = c-nr-pallet NO-ERROR.

ASSIGN r-pallet = ROWID(pallet).

END PROCEDURE.

function fGetData returns character (input pData as char):
   def var cAux   as char no-undo.
   def var de-tot as dec no-undo.
   def var i-tot  as int no-undo.
         
   find first lote-carac-tec
              where lote-carac-tec.it-codigo = lote-item.it-codigo
                and lote-carac-tec.lote      = lote-item.lote
                and lote-carac-tec.cd-folh   = item.cd-folh-lote
                and lote-carac-tec.cd-comp   = pData no-lock no-error.


   if not avail lote-carac-tec then
      return "":U.

   find first comp-folh
        where comp-folh.cd-folh = lote-carac-tec.cd-folh
          and comp-folh.cd-comp = pData no-lock no-error.

   if avail lote-carac-tec and 
      avail comp-folh      then do:
      case lote-carac-tec.tipo-result:
          when 1 then cAux = string(lote-carac-tec.vl-result, comp-folh.formato).
          otherwise cAux = "?".
      end case.
   end.

   if pData = "mediadens" then do:
      assign de-tot = 0
             i-tot  = 0.
      for each lote-carac-tec
         where lote-carac-tec.it-codigo = item.it-codigo
           and lote-carac-tec.lote      = movto-mat.lote
           and lote-carac-tec.cd-folh   = item.cd-folh-lote
           and lote-carac-tec.cd-comp  begins("dens") 
           and lote-carac-tec.vl-result > 0 no-lock:
           assign de-tot = de-tot + lote-carac-tec.vl-result
                  i-tot  = i-tot + 1.
      end.
      assign cAux = string(de-tot / i-tot).
   end.

   return cAux.
end function.

PROCEDURE pi-etiqueta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var raw-param as raw no-undo.

for each tt-param2:
    delete tt-param2.
end.

create tt-param2.
assign tt-param2.usuario    = c-seg-usuario
       tt-param2.destino    = 3
       tt-param2.data-exec  = today
       tt-param2.hora-exec  = time
       tt-param2.dir-etq    = ""
       tt-param2.arquivo    = "v:\temp\etiqueta.txt".

raw-transfer tt-param2 to raw-param.

for each tt-raw-digita:
    delete tt-raw-digita.
end.
for each tt-digita:
    create tt-raw-digita.
    raw-transfer tt-digita to tt-raw-digita.raw-digita.
end.

run sfc/essf0013prp.p (input raw-param,
                            input table tt-raw-digita).

END PROCEDURE.



PROCEDURE pi-grava-erro-log-ctl.
    DEFINE INPUT PARAM c-mensagem AS CHAR FORMAT "x(300)".
    OUTPUT TO m:\dts\LOG_prd\essf0022-erro.LOG APPEND.

    PUT c-mensagem SKIP.

    OUTPUT CLOSE.

END PROCEDURE.



procedure pi-configura.

    c-porta = "COM1".
    conf = " mode " + c-porta + ":" + "4800" + "," + "e" + 
                     "," + "7" + "," + "2".
    
    DOS SILENT VALUE(conf).
 
end procedure. 

PROCEDURE pi-peso:

DEF VAR I-TIME-INI AS INTEGER NO-UNDO.
DEF VAR I-TIME-FIM AS INTEGER NO-UNDO.
DEFINE VARIABLE cCheckString AS CHARACTER NO-UNDO. 

DEFINE VARIABLE c-peso AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-ct  AS INTEGER    NO-UNDO.
I-TIME-INI = TIME.

do  on error undo, return error
    on stop  undo, return error: 
 i-ct = 0.
 
    d-peso-liquido:BGCOLOR IN FRAME f-relat = 10.
 
    INPUT STREAM StreamName FROM value(c-porta).
    /*OUTPUT STREAM StreamLog CLOSE.
    */
    cCheckString = "".

    DO WHILE TRUE:
              
            
            i-ct = i-ct + 1.
            READKEY PAUSE 0.
            IF LASTKEY = 9  THEN do:
                

                  
                LEAVE   .
            END.

            READKEY STREAM StreamName PAUSE 0.
              IF LASTKEY = 13 THEN DO:
                LEAVE.
            END.
        
            ELSE
                IF LASTKEY <> -1 THEN
               ASSIGN
                  cCheckString = cCheckString + CHR(LASTKEY).


          if i-ct > 5000000000 then leave.
         
         I-TIME-FIM = TIME.

         IF (I-TIME-FIM - I-TIME-INI) > 10 THEN LEAVE.
   
            
    END.

    c-peso = "".
    
    c-peso = string( dec(substring(cCheckString,2,index(cCheckString,"kg") - 2)),">>>>>>9.99") no-error.
    
    d-peso-liquido:SCREEN-VALUE IN FRAME f-relat = trim(c-peso).
    
    d-peso-liquido:bgcolor in FRAME f-relat = 15.
    
 
    APPLY "leave" TO d-peso-liquido IN FRAME f-relat.
     
    RETURN ERROR.
END.

END PROCEDURE.

