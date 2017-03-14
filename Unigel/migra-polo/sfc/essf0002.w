    
/* Connected Databases 
          mgadm            PROGRESS
*/

/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/************************************************************************
**
**  i-prgvrs.i - Programa para cria?ío do log de todos os programas 
**               e objetos do EMS 2.0 para objetos
**  {1} = objeto   provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/

def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[2.00.00.000[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.000"
       c-prg-obj = "essf0002".
if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "essf0002"
        no-lock no-error.
        
   if not avail prog_dtsul then do:
          if  c-prg-obj begins "btb":U then
              assign c-prg-obj = "btb~/":U + c-prg-obj.
          else if c-prg-obj begins "men":U then
                  assign c-prg-obj = "men~/":U + c-prg-obj.
          else if c-prg-obj begins "sec":U then
                  assign c-prg-obj = "sec~/":U + c-prg-obj.
          else if c-prg-obj begins "utb":U then
                  assign c-prg-obj = "utb~/":U + c-prg-obj.
          find prog_dtsul where
               prog_dtsul.nom_prog_ext begins c-prg-obj no-lock no-error.
   end .            /*if*/
    
    output to value(c-arquivo-log) append.
    put "essf0002" at 1 "2.00.00.000" at 69 today at 84 string(time,'HH:MM:SS':U) at 94 skip.
    if  avail prog_dtsul then do:
        if  prog_dtsul.nom_prog_dpc <> "" then
            put "DPC : ":U at 5 prog_dtsul.nom_prog_dpc  at 12 skip.
        if  prog_dtsul.nom_prog_appc <> "" then
            put "APPC: ":U at 5 prog_dtsul.nom_prog_appc at 12 skip.
        if  prog_dtsul.nom_prog_upc <> "" then
            put "UPC : ":U at 5 prog_dtsul.nom_prog_upc  at 12 skip.
    end.
    output close.        
end.  
error-status:error = no.
/***************************************************
** i_dbtype.i - Tipo de Gerenciadores utilizados
***************************************************/


        
    /* Preprocessadores que identificam os bancos do Produto EMS 5 */
                    
    /* Preprocessadores que identificam os bancos do Produto EMS 2 */
                                                                                                            
    
    /* Preprocessadores que identificam os bancos do Produto HR 2 */
            

/* Fim */

 

/*alteracao Anderson(tech540) em 04/02/2003 Include com a definicao 
da temp table utilizada nas includes btb008za.i1 e btb008za.i2 para 
execucao de programas via rpc*/
def temp-table tt-control-prog NO-UNDO
    field cod-versao-integracao as integer       format '999'
    field cod-erro              as integer       format '99999'
    field desc-erro             as character     format 'x(60)'
    field wgh-servid-rpc        as widget-handle format '>>>>>>9'.
 
 
/*fim alteracao Anderson 04/02/2003*/

/* alteraá∆o feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */

DEFINE NEW GLOBAL SHARED VARIABLE hWenController AS HANDLE     NO-UNDO.
/*Constante utilizada apenas para verificar se a vari†vel acima foi definida */ 

/* fim da alateraá∆o */

 

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/*fiSaldoBobina*/

def new global shared var h-acomp as handle no-undo.

/*** Esta include no ems 2.01 n∆o dever† possuir nenhum c¢digo, serve 
     apenas para desenvolvimento tratar o conceito de miniflexibilizaá∆o.
     Utilizado apenas para MANUFATURA. ***/

/*** RELEASE 2.02 ***/

/* Funá‰es ch∆o f†brica e lista de componentes 2.02 - Logoplaste - Manufatura */


/*** RELEASE 2.03 ***/

/* Integraá∆o Magnus x SFC 2.03 - Klimmek - Manufatura *//* Relacionamento Linha Produá∆o x Estabelecimento     *//* Transaá∆o Reporte Ass°ncrono                        *//* Alteraá‰es Gerais EMS 2.03                          */

/*** RELEASE 2.04 ***/

/* Alteraá‰es Gerais EMS 2.04                          *//* Integraá∆o EAI                                     */ 
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
 
 
/**** Procedimentos para convers∆o de data / segundos a um valor YYYYMMDDSSSSS ***/

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
     /* Pi-converte-data-segs-valor */

def temp-table tt-rep-oper-ctrab  no-undo like rep-oper-ctrab
    field cod-ferr-prod                   like split-operac.cod-ferr-prod
    field dat-fim-setup                   like split-operac.dat-fim-setup
    field dat-inic-setup                  like split-operac.dat-inic-setup
    field qtd-segs-fim-setup              like split-operac.qtd-segs-fim-setup
    field qtd-segs-inic-setup             like split-operac.qtd-segs-inic-setup.

def temp-table tt-rep-refugo-oper no-undo like rep-refugo-oper.
def temp-table tt-rep-ic-oper     no-undo like rep-ic-oper.
def temp-table tt-rep-ic-oper-tab no-undo like rep-ic-oper-tab.



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
 

def temp-table tt-digita no-undo
    field nr-ord-produ like ord-prod.nr-ord-produ
    field cod-estabel  like ord-prod.cod-estabel
    field nr-linha     like ord-prod.nr-linha
    field rw-lote-item as rowid 
    field arquivo      as char.


def new global shared var grw-lote-item as rowid  no-undo.
def new global shared var gc-estado     as char   no-undo.

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

def var deTotRefugo like ord-prod.qt-ordem no-undo.
def var deTotOrdens like ord-prod.qt-ordem no-undo.

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


/* Vari†veis para atualizaá∆o da nova tabela de rastreabilidade */

define VARIABLE nr-ord-produ-jr-pro LIKE movto-estoq.nr-ord-produ.
define variable cod-estabel-jr-pro  LIKE movto-estoq.cod-estabel.
define variable it-codigo-jr-pro    LIKE movto-estoq.it-codigo.
define variable lote-jr-pro  AS CHAR          NO-UNDO.
define variable lote-jr-con1 AS CHAR          NO-UNDO.
define variable lote-jr-con2 AS CHAR          NO-UNDO.
define variable lote-jr-con3 AS CHAR          NO-UNDO.

DEFINE VARIABLE c-wk-refer1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-wk-refer2 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-wk-refer3 AS CHARACTER   NO-UNDO. 

DEFINE BUFFER bf-reservas FOR reservas.
/*---------------------------------------------------------------*/ 

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

def temp-table tt-erro2 like tt-erro.

define temp-table tt-pallet no-undo
    field nr-pallet    as char
    field nr-ord-produ AS INT
    field situacao     AS INT
    INDEX codigo
          nr-pallet.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* Name of first Frame and/or Browse and/or first Query                 */

/* Internal Tables (found by Frame, Query & Browse Queries)             */

/* Definitions for BROWSE brReporte                                     */


/* Definitions for FRAME fPage0                                         */

/* Standard List Definitions                                            */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */




/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Arquivo"      
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */

DEFINE BUTTON btpeso 
     IMAGE-UP FILE "image\im-bloq":U
     IMAGE-INSENSITIVE FILE "image\im-bloqi":U
     LABEL "peso" 
     SIZE 4 BY 1.25
     FONT 4.


DEFINE BUTTON btAdd 
     IMAGE-UP FILE "image\im-add":U
     IMAGE-INSENSITIVE FILE "image\ii-add":U
     LABEL "Add" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btCancel 
     IMAGE-UP FILE "image\im-can":U
     IMAGE-INSENSITIVE FILE "image\im-can":U
     LABEL "Cancel" 
     SIZE 4 BY 1.25 TOOLTIP "Cancelar"
     FONT 4.

DEFINE BUTTON btElimina 
     LABEL "Elimina" 
     SIZE 12 BY 1.

DEFINE BUTTON btLotes 
     LABEL "Lotes" 
     SIZE 12 BY 1.

DEFINE BUTTON btModifica 
     LABEL "Modifica" 
     SIZE 12 BY 1.

DEFINE BUTTON btSave 
     IMAGE-UP FILE "image\im-sav":U
     IMAGE-INSENSITIVE FILE "image\ii-sav":U
     LABEL "Save" 
     SIZE 4 BY 1.25 TOOLTIP "Salvar"
     FONT 4.

DEFINE BUTTON btSelecao 
     LABEL "Seleá∆o" 
     SIZE 12 BY 1.

DEFINE VARIABLE fiCodCtrab AS CHARACTER FORMAT "X(16)":U 
     LABEL "Centro de Trabalho" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fiCodEstabel AS CHARACTER FORMAT "X(03)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fiCodOperador AS CHARACTER FORMAT "99999-9":U 
     LABEL "Operador" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fiDescricao AS CHARACTER FORMAT "X(60)":U 
     LABEL "Descriá∆o" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .88 NO-UNDO.

DEFINE VARIABLE fiDesCtrab AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .88 NO-UNDO.

DEFINE VARIABLE fiDtInicio AS DATE FORMAT "99/99/9999":U 
     LABEL "In°cio" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fiDtTermino AS DATE FORMAT "99/99/9999":U 
     LABEL "TÇrmino" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fiEmenda1 AS CHARACTER FORMAT "X(10)":U 
     LABEL "Emenda 1" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fiEmenda2 AS CHARACTER FORMAT "X(10)":U 
     LABEL "Emenda 2" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fiHrInicio AS CHARACTER FORMAT "99:99":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fiHrTermino AS CHARACTER FORMAT "99:99":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fiItem AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fiLote AS CHARACTER FORMAT "X(10)":U 
     LABEL "Lote" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fiNomeEstabel AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .88 NO-UNDO.

DEFINE VARIABLE fiNomeOperador AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .88 NO-UNDO.

DEFINE VARIABLE fiQtEmenda1 AS DECIMAL FORMAT "->>>,>>>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fiQtEmenda2 AS DECIMAL FORMAT "->>>,>>>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fiQtLote AS DECIMAL FORMAT "->>>,>>>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fiQtTotal AS DECIMAL FORMAT "->>>,>>>,>>9.9999":U INITIAL 0 
     LABEL "Total Qtde" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fiRefugo AS DECIMAL FORMAT ">>>,>>>,>>9.9999":U INITIAL 0 
     LABEL "Refugo" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fiSaldoBobina AS DECIMAL FORMAT "->>>,>>>,>>9.9999":U INITIAL 0 
     LABEL "Saldo da Bobina" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 90 BY 4.5.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 90 BY 3.5.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 90 BY 1.5.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */

DEFINE QUERY brReporte FOR 
      tt-reporta SCROLLING.


/* Browse definitions                                                   */
DEFINE BROWSE brReporte

  QUERY brReporte NO-LOCK DISPLAY
      tt-reporta.nr-ord-produ  
tt-reporta.it-codigo
tt-reporta.nr-pedcli
tt-reporta.nome-abrev
tt-reporta.qt-reporte    
tt-reporta.cod-depos     
tt-reporta.cod-localiz  
tt-reporta.gera-pallet
tt-reporta.des-carac     
enable 
tt-reporta.cod-depos     
tt-reporta.cod-localiz
/* _UIB-CODE-BLOCK-END */

    WITH NO-ROW-MARKERS SEPARATORS SIZE 90 BY 4.5 ROW-HEIGHT-CHARS .54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fPage0
     btAdd AT ROW 1.13 COL 2 HELP
          "Inclui nova ocorrància"
     btCancel AT ROW 1.13 COL 6 HELP
          "Cancela alteraá‰es"
     btSave AT ROW 1.13 COL 10 HELP
          "Confirma alteraá‰es"
     btpeso AT ROW 1.13 COL 60 HELP
          "Libera digitaá∆o de peso"
     fiCodEstabel AT ROW 2.75 COL 24 COLON-ALIGNED
     fiNomeEstabel AT ROW 2.75 COL 29.43 COLON-ALIGNED NO-LABEL
     fiCodCtrab AT ROW 3.75 COL 24 COLON-ALIGNED
     fiDesCtrab AT ROW 3.75 COL 36.43 COLON-ALIGNED NO-LABEL
     fiCodOperador AT ROW 4.75 COL 24 COLON-ALIGNED
     fiNomeOperador AT ROW 4.75 COL 32.43 COLON-ALIGNED NO-LABEL
     fiDtInicio AT ROW 5.75 COL 24 COLON-ALIGNED
     fiHrInicio AT ROW 5.75 COL 34.43 COLON-ALIGNED NO-LABEL
     fiDtTermino AT ROW 5.75 COL 52 COLON-ALIGNED
     fiHrTermino AT ROW 5.75 COL 62.43 COLON-ALIGNED NO-LABEL
     fiLote AT ROW 7.5 COL 14 COLON-ALIGNED
     fiQtLote AT ROW 7.5 COL 24.43 COLON-ALIGNED NO-LABEL
     fiItem AT ROW 7.5 COL 52 COLON-ALIGNED
     fiEmenda1 AT ROW 8.5 COL 14 COLON-ALIGNED
     fiQtEmenda1 AT ROW 8.5 COL 24.43 COLON-ALIGNED NO-LABEL
     fiDescricao AT ROW 8.5 COL 52 COLON-ALIGNED
     fiEmenda2 AT ROW 9.5 COL 14 COLON-ALIGNED
     fiQtEmenda2 AT ROW 9.5 COL 24.43 COLON-ALIGNED NO-LABEL
     brReporte AT ROW 10.75 COL 1
     btSelecao AT ROW 15.25 COL 1
     btModifica AT ROW 15.25 COL 13
     btElimina AT ROW 15.25 COL 25
     btLotes AT ROW 15.25 COL 37
     fiSaldoBobina AT ROW 16.75 COL 17 COLON-ALIGNED
     fiRefugo AT ROW 16.75 COL 41 COLON-ALIGNED
     fiQtTotal AT ROW 16.75 COL 68 COLON-ALIGNED
     RECT-12 AT ROW 2.5 COL 1
     RECT-13 AT ROW 7.25 COL 1
     RECT-14 AT ROW 16.5 COL 1
     rt-button AT ROW 1 COL 1
     "Consumo" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 7 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.


/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
 */


/* *************************  Create Window  ************************** */


IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 16.96
         WIDTH              = 90
         MAX-HEIGHT         = 17.96
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17.96
         VIRTUAL-WIDTH      = 90
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE w-livre = CURRENT-WINDOW.

ASSIGN w-livre:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */



/* ************************* Included-Libraries *********************** */




/*-------------------------------------------------------------------------
    Library     : containr.i  
    Purpose     : Default Main Block code and Method Procedures
                  for UIB-generated ADM Container procedures.

    Syntax      : {src/adm/method/containr.i}

    Description :

    Author(s)   :
    Created     :
    HISTORY:
-------------------------------------------------------------------------*/
/***********************  DEFINITIONS  ***********************************/

/* System Variable Definitions ---                                       */
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

/********************************************************************************
** Programa : include/i-sysvar.i
**
** Data : 02/06/1999
**
** Criaá∆o : John Cleber Jaraceski
**
** Objetivo : Definicao das System Variables
**
** Ultima Alt : ?
*******************************************************************************/


        
    DEFINE VARIABLE c-programa-mg97       AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-versao-mg97         AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-modulo-mg97         AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-titulo-prog-mg97    AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-nom-manual-hlp-mg97 AS CHARACTER FORMAT "x(06)":U NO-UNDO.
    DEFINE VARIABLE c-cod-mod-mg97        AS CHARACTER                  NO-UNDO.
    DEFINE VARIABLE d-data-contrato       AS DATE                       NO-UNDO.
    DEFINE VARIABLE i-num-topico-hlp-mg97 AS INTEGER                    NO-UNDO.
    DEFINE VARIABLE i-user-conectados     AS INTEGER                    NO-UNDO.
    DEFINE VARIABLE i-licenca-usuar       AS INTEGER                    NO-UNDO.
    DEFINE VARIABLE l-acesso-livre        AS LOGICAL                    NO-UNDO.


/* include/i-sysvar.i ---                                                     */

 

/* Local Variable Definitions ---                                        */
DEFINE VARIABLE i-ctrl-tab-page   AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-ctrl-tab-folder AS INTEGER   NO-UNDO.
DEFINE VARIABLE c-state-folder    AS CHARACTER NO-UNDO.








/* Dialog program to run to set runtime attributes - if not defined in master */



/* +++ This is the list of attributes whose values are to be returned
   by get-attribute-list, that is, those whose values are part of the
   definition of the object instance and should be passed to init-object
   by the UIB-generated code in adm-create-objects. */



/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 1.5
         WIDTH              = 38.43.
/* END WINDOW DEFINITION */
                                                                        */





/* ************************* Included-Libraries *********************** */




/*-------------------------------------------------------------------------
    File        : smart.i  
    Purpose     : Provides basic SmartObject functionality.

    Syntax      : {src/adm/method/smart.i}

    Description :

    Author(s)   :
    Created     :
    Notes       :
--------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

def var c-nom-prog-upc-mg97  as char format "x(50)" no-undo.
def var c-nom-prog-appc-mg97 as char format "x(50)" no-undo.
def var c-nom-prog-dpc-mg97  as char format "x(50)" no-undo.
def var c-ctrl-tab           as char                no-undo.
def var h-ctrl-tab           as handle              no-undo.
def var wh-entry-field       as widget-handle       no-undo.



/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 2.93
         WIDTH              = 35.14.
/* END WINDOW DEFINITION */
                                                                        */





/* ************************* Included-Libraries *********************** */

/****************************************************************************
     PROCEDURE: attribut.i

       PURPOSE: holds general-use variable and table definitions
                for ADM Method Libraries

       REMARKS:

    PARAMETERS: NONE

      HISTORY:
*****************************************************************************/

/* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1994-6 - All Rights Reserved. */

/* Make sure not already included */


/* The new Progress widget attribute ADM-DATA is used to store ADM
   attributes and other ADM-specific information. This is new to 8.1, 
   so use PRIVATE-DATA to preserve the ability to compile with 8.0.
   Also there is a new keyword UNLESS-HIDDEN which allows a DISPLAY/ENABLE
   to bypass fields which are hidden. This is used in building alternate
   layouts. */
/* &IF PROVERSION GE "8.1":U &THEN   */
/*   &GLOB    adm-data      ADM-DATA */
/*   &GLOB    unless-hidden          */
/* &ELSE                             */

/* O teste de vers∆o do progress foi retirado pois na vers∆o 10 passaria a causar erros, 
j† que o teste usa string e neste caso 10 Ç menor que 8. Tivemos alguns problemas j† ao testar
a vers∆o beta e foi cadastrado um chamado de Bug - SW */

      
/* &ENDIF */

DEFINE VAR adm-object-hdl       AS HANDLE NO-UNDO. /* current object's handle */
DEFINE VAR adm-query-opened        AS LOGICAL NO-UNDO INIT NO.
DEFINE VAR adm-row-avail-state     AS LOGICAL NO-UNDO INIT ?.
DEFINE VAR adm-initial-lock        AS CHARACTER NO-UNDO INIT "NO-LOCK":U.
DEFINE VAR adm-new-record          AS LOGICAL NO-UNDO INIT no.
DEFINE VAR adm-updating-record     AS LOGICAL NO-UNDO INIT no.
DEFINE VAR adm-check-modified-all  AS LOGICAL NO-UNDO INIT no.

DEFINE NEW GLOBAL SHARED VAR adm-broker-hdl    AS HANDLE  NO-UNDO.



 
 

/* _UIB-CODE-BLOCK-END */







/* ***************************  Main Block  *************************** */

/* The code to assign the object handle (which becomes the ADM-OBJECT-HANDLE
   attribute below) for containers and for other objects has been combined
   here. Note that setting adm-object-hdl later in user code (including the
   main block of a MLI) will have no effect on the value of the attribute.
   To override these default settings (which should be appropriate for 
   virtually all objects) user code must 
     RUN set-attribute-list ('ADM-OBJECT-HANDLE=...').

   For SmartContainers, set the handle to the Frame handle if the
   Container Type is FRAME or DIALOG-BOX, else to WINDOW, unless the
   Container is "virtual" (no visualization), in which case leave it unknown.

   For other objects, set the handle to the default Frame handle if 
   there is one.
*/


  
    ASSIGN adm-object-hdl    =   w-livre.
  


/* Traduá∆o de Hard-Coded View-as */ 

    
        run pi-trad-widgets (input frame fPage0:handle).
    






/* If the broker handle either isn't valid or isn't the right process
   (it's possible the handle has been reused), then start the broker. 
   (But don't let the broker try to start itself!) */

RUN get-attribute IN adm-broker-hdl ('TYPE':U) NO-ERROR.
IF RETURN-VALUE NE "ADM-Broker":U THEN 
DO: 
    RUN adm/objects/broker.p PERSISTENT set adm-broker-hdl. 
    RUN set-broker-owner IN adm-broker-hdl (THIS-PROCEDURE).
END.


/* Initialize all the attributes which all SmartObjects must have. */

THIS-PROCEDURE:ADM-DATA = 
     'ADM1.1~`':U +         /* Version attribute */
     'w-livre~`':U +      /* Type attribute */
     'WINDOW~`':U +       /* Container-Type attribute */
   
     'NO ~`':U +
   
     '~`':U +    /* External-Tables attribute */
     'tt-reporta~`':U +    /* Internal-Tables attribute */
   
     '~`':U +     /* Enabled-Tables attribute */
   
     (IF adm-object-hdl = ? THEN "":U ELSE STRING(adm-object-hdl))
        + "~`":U +    /* Adm-Object-Handle attribute */
   
     'Layout,Hide-on-Init~`':U +  /* Attribute-List attribute */
   
   
     '~`':U + /* Supported-Links attribute */
   
     '~`':U +  /* ADM-Dispatch-Qualifier attr */
     '~`~`~`~`~`~`~`~`~`~`~`':U +   /* Placeholders for ADM-Parent, Layout,
                                      Enabled, Hidden, COntainer-Hidden,
                                      Initialized, Fields-Enabled, Current-Page,
                                      ADM-New-Record, UIB-Mode, 
                                      ADM-Deactivate-Links */
    /* PLUS THERE IS AN EXTRA TICK FOR THE DUMMY PREPROC
       which marks the end of the list. Do not disturb. */ 
     IF THIS-PROCEDURE:ADM-DATA = "":U OR THIS-PROCEDURE:ADM-DATA = ? 
         THEN "^^":U             /* plus placeholders for user-defined attrs. */
     /* Or if there are already attributes defined, don't throw them away. */
     ELSE "^":U + ENTRY(2, THIS-PROCEDURE:ADM-DATA, "^":U) + 
          "^":U + ENTRY(3, THIS-PROCEDURE:ADM-DATA, "^":U).


/* An "apply-layout" method is not necessary if there are no layout-cases */

  

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */




PROCEDURE adm-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Applies "ENTRY" to the first enabled field or other 
               object in the SmartObject.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VAR c_Handle AS CHAR NO-UNDO.
  ASSIGN c_Handle = "".
  RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE, 
                                         INPUT 'TABLEIO-SOURCE':U,
                                         OUTPUT c_Handle ).
  IF c_Handle <> "" THEN                                       
  RUN broker-apply-entry IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */









PROCEDURE adm-destroy :
/* -----------------------------------------------------------
      Purpose:     Basic routine to delete a procedure and its
                   CONTAINED descendents
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/   

 
        /***************************************************************
**
** I-EPC100.I - EPC para Evento DESTROY de Container
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:

    run value(c-nom-prog-dpc-mg97) (input "DESTROY":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame fPage0:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "DESTROY":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame fPage0:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "DESTROY":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame fPage0:handle,
                                    input "",
                                    input ?).    

end.                                       
/* I-EPC100 */
 
 

 RUN broker-destroy IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-disable :
/* -----------------------------------------------------------
      Purpose:     Disables all enabled objects in the frame.
                   Note that this includes db fields if any.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/   
    /* EPC Before Disable do Container */
    
           /***************************************************************
**
** I-EPC009.I - EPC para Evento Before DISABLE de Container
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:

    run value(c-nom-prog-dpc-mg97) (input "BEFORE-DISABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame fPage0:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "BEFORE-DISABLE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame fPage0:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "BEFORE-DISABLE", 
                                    input "CONTAINER",
                                    input this-procedure,
                                    input frame fPage0:handle,
                                    input "",
                                    input ?).    

end.                                       
/* I-EPC009.I */

 
    

    
    DISABLE btAdd btpeso RECT-12 RECT-13 RECT-14 rt-button WITH FRAME fPage0.
    RUN dispatch ('disable-fields':U).  
    

    /* EPC Disable do Container */
    
           /***************************************************************
**
** I-EPC009.I - EPC para Evento DISABLE de Container
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:

    run value(c-nom-prog-dpc-mg97) (input "DISABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame fPage0:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "DISABLE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame fPage0:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "DISABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame fPage0:handle,
                                    input "",
                                    input ?).    

end.                                       
/* I-EPC009.I */

 
    


    RUN set-attribute-list ('ENABLED=no':U).

    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-edit-attribute-list :
/* -----------------------------------------------------------
      Purpose:    Runs the dialog to get runtime parameter settings
      Parameters:  <none>
      Notes:       Generally run by the UIB in design mode
    -------------------------------------------------------------*/   
  /* Must be defined in the Object*/
      RUN adm/support/contnrd.w (INPUT THIS-PROCEDURE).
  

      RETURN. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-enable :
/* -----------------------------------------------------------
      Purpose:    Enable an object - all components except db fields,
                  which are enabled using enable-fields.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/   
   /* EPC Before Enable do Container */
   
          /***************************************************************
**
** I-EPC008.I - EPC para Evento Before ENABLE de Container
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:

    run value(c-nom-prog-dpc-mg97) (input "BEFORE-ENABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame fPage0:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "BEFORE-ENABLE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame fPage0:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "BEFORE-ENABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame fPage0:handle,
                                    input "",
                                    input ?).    

end.                                       
/* I-EPC008 */

 
   

   
    ENABLE UNLESS-HIDDEN btAdd btpeso RECT-12 RECT-13 RECT-14 rt-button WITH FRAME fPage0.
    IF L-PESO-BAL THEN 
       ASSIGN btpeso:SENSITIVE IN FRAME fPage0 = NO.

    /* We also run enable_UI from here. */ 
    RUN enable_UI IN THIS-PROCEDURE NO-ERROR.
   

   /* EPC Enable do Container */
   
          /***************************************************************
**
** I-EPC008.I - EPC para Evento ENABLE de Container
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:

    run value(c-nom-prog-dpc-mg97) (input "ENABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame fPage0:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "ENABLE":U, 
                                     input "CONTAINER",
                                     input this-procedure,
                                     input frame fPage0:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "ENABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame fPage0:handle,
                                    input "",
                                    input ?).    

end.                                       
/* I-EPC008 */

 
   

   RUN set-attribute-list ('ENABLED=yes':U).

   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-exit :
/* -----------------------------------------------------------
      Purpose: Passes an exit request to its container    
      Parameters:  <none>
      Notes:  The convention is that the standard routine always
          passes an exit request to its CONTAINER-SOURCE. The container 
          that is actually able to initiate the exit should define
          a local version and *not* call the standard one.    
          That local-exit is built into the SmartWindow template.
    -------------------------------------------------------------*/   

     RUN notify ('exit':U).

  RETURN.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-hide :
/* -----------------------------------------------------------
      Purpose:     Hides an object and sets any active links which
                   are dependent on hide/view off.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/   
  RUN broker-hide IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-initialize :
/* -----------------------------------------------------------
      Purpose:     Enables and Views an object unless its attributes
                   indicate this should not be done.
                   Cascades 'initialize' to descendents.
      Parameters:  <none>
      Notes:       
   -------------------------------------------------------------*/   
   /* altera?ío feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */
   
   /* est† verificaá∆o se faz necess†ria devido aos programas */
      


    /* Tenta identificar pelo ADM-CONTAINER */
    
    
        

                    
        
    

    /* Se ainda nío identificou se ? window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    

    /* Se ainda nío identificou se ? window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
    
    /*
    &IF DEFINED(WenIsWindow) &THEN
        RUN registerWindow IN hWenController (THIS-PROCEDURE, CURRENT-WINDOW).
    &ELSEIF DEFINED(WenIsDialog) &THEN
        RUN registerWindow IN hWenController (THIS-PROCEDURE, CURRENT-WINDOW).
    &ENDIF
    */

    /* Se tem janela */
    

        ON 'U9':U ANYWHERE
        DO:
            IF VALID-HANDLE(hWenController) THEN DO:
                RUN registerWindow IN hWenController (THIS-PROCEDURE, SELF).
            END.
        END.

    

    /* Se tem dialog */
    


            /* criados pelos DataViewer n∆o utilizarem a include i-prgvrs */ 
   /* e dessa forma n∆o chamarem a include i-wendef.i que define essa veri†vel. */
   
   /* fim da alateraá∆o */

   /* EPC Before Initialize do Container */ 
   
          /***************************************************************
**
** I-EPC007.I - EPC para Evento Before INITIALIZE de Container
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:

    run value(c-nom-prog-dpc-mg97) (input "BEFORE-INITIALIZE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame fPage0:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "BEFORE-INITIALIZE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame fPage0:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "BEFORE-INITIALIZE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame fPage0:handle,
                                    input "",
                                    input ?).    

end.                                       
/* I-EPC007.I */

 
   

   /* EPC Before Initialize do Viewer */ 
   

   /* EPC Before Initialize do Browser */
   

   RUN broker-initialize IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.
   /*   Alteraá∆o para corrigir o problema de algumas viewers n∆o mostrar
        o primeiro registro quando o programa Ç inicializado               
        Data : 20/Fev/97  - J.Carlos (PGS)  -  Egolf e SÇrgio (DATASUL)  */  
   
        
             IF  frame fPage0:scrollable THEN
                 ASSIGN frame fPage0:virtual-width-chars  = frame fPage0:width-chars
                        frame fPage0:virtual-height-chars = frame fPage0:height-chars.
        
   
                                                               /*
    Nome :  C-PAGE.i       J.Carlos - 21/Fev/97
    Fun?ío : Guardar a pagina e o container-source da VIEWER.
*/

   def var c_Aux-var as char no-undo.
   RUN get-link-handle IN adm-broker-hdl (INPUT  THIS-PROCEDURE,
                                          INPUT  "CONTAINER-SOURCE":U,
                                          OUTPUT c_Aux-var).
   RUN set-attribute-list ("W-Container-Source = ":U + string(c_Aux-var)).
   RUN What-is-the-Page IN adm-broker-hdl (INPUT THIS-PROCEDURE).
   RUN set-attribute-list ("W-Page = ":U + RETURN-VALUE). 
 

   
        run get-link-handle in adm-broker-hdl
             (input this-procedure,
              input 'page':U,
              output c-ctrl-tab).
        assign h-ctrl-tab = if c-ctrl-tab <> "" then widget-handle(c-ctrl-tab) else ?.
   

   /* EPC - Initialize do Container */ 
   
          /***************************************************************
**
** I-EPC007.I - EPC para Evento INITIALIZE de Container
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.

/* DPC */
if  c-nom-prog-dpc-mg97 <> ""
and c-nom-prog-dpc-mg97 <> ? then do:

    run value(c-nom-prog-dpc-mg97) (input "INITIALIZE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame fPage0:handle,
                                    input "",
                                    input ?).    

end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    

    run value(c-nom-prog-appc-mg97) (input "INITIALIZE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame fPage0:handle,
                                     input "",
                                     input ?).    

end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:

    run value(c-nom-prog-upc-mg97) (input "INITIALIZE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame fPage0:handle,
                                    input "",
                                    input ?).    

end.                                       
/* I-EPC007.I */

 
   

   /* EPC - Initialize do Viewer */ 
   

   /* EPC - Initialize do Browser */
   

   
       RUN get-attribute IN THIS-PROCEDURE ("ApplyFillIn":U).
       IF ENTRY(1, RETURN-VALUE, "|":U) = "YES":U THEN
          RUN ApplyFillIn IN WIDGET-HANDLE(ENTRY(2, RETURN-VALUE, "|":U)).
   

   /*Traduá∆o dos campos de tela*/
   
   /*final da traduá∆o dos campos de tela*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-show-errors :
/* -----------------------------------------------------------
      Purpose:  Display system error messages on a runtime error.
      Parameters:  <none>
      Notes:    A localization of this method can look at the message
                number to display a custom error or suppress standard
                error display.
    -------------------------------------------------------------*/

    DEFINE VARIABLE        cntr                  AS INTEGER   NO-UNDO.

    DO cntr = 1 TO ERROR-STATUS:NUM-MESSAGES:
        MESSAGE ERROR-STATUS:GET-MESSAGE(cntr).
    END.

    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-UIB-mode :
/*--------------------------------------------------------------------------
  Purpose     : Set the objects attributes in "UIB Mode".  This is the
                "mode" it will have in design-mode in the UIB.
  Notes       :
  ------------------------------------------------------------------------*/

  RUN broker-UIB-mode IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE adm-view :
/* -----------------------------------------------------------
      Purpose:     Views an object and sets active links on.
      Parameters:  <none>
      Notes:       
    -------------------------------------------------------------*/   

  RUN broker-view IN adm-broker-hdl (THIS-PROCEDURE) NO-ERROR.

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE dispatch :
/* -----------------------------------------------------------
      Purpose:    Determines whether to run the LOCAL or STANDARD (adm-)
                  or no-prefix version of a method in the current procedure.
      Parameters: INPUT base method name (with no prefix),
      Notes:      In addition, if the developer has defined a custom prefix
                  as ADM-DISPATCH-QUALIFIER, then a method with this prefix
                  will be searched for after "local-" and before "adm-".
                  If the preprocessor ADM-SHOW-DISPATCH-ERRORS is defined
                  then the show-errors method will be dispatched if a
                  method name is not found in any form. This can be 
                  useful for debugging purposes.
    -------------------------------------------------------------*/   

    DEFINE INPUT PARAMETER p-method-name    AS CHARACTER NO-UNDO.

    RUN broker-dispatch IN adm-broker-hdl 
        (THIS-PROCEDURE, p-method-name) NO-ERROR.
    IF RETURN-VALUE = "ADM-ERROR":U THEN RETURN "ADM-ERROR":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE get-attribute :
/* -----------------------------------------------------------
      Purpose:     Returns the value of a std variable or attribute-table entry.
      Parameters:  INPUT attribute name, RETURN-VALUE (string)
      Notes:       
    -------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-attr-name    AS CHARACTER NO-UNDO.

  RUN broker-get-attribute IN adm-broker-hdl
      (INPUT THIS-PROCEDURE, INPUT p-attr-name) NO-ERROR.

  RETURN RETURN-VALUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE get-attribute-list :
/* -----------------------------------------------------------
      Purpose:     Returns a list of all settable object attributes.
      Parameters:  OUTPUT comma-separated attribute list
      Notes:       This procedure does not return a list of *all*
                   attributes, but only those which are defined and
                   set by users (e.g., not HIDDEN, ENABLED... ).
                   In Version 8.1., an INPUT parameter has been added
                   to broker-get-attribute-list to allow a caller to
                   specify a particular list of attributes to return.
                   This standard call does not specify a list, so
                   the attributes in the ADM-ATTRIBUTE-LIST attribute
                   are returned.
    -------------------------------------------------------------*/   

  DEFINE OUTPUT PARAMETER p-attr-list AS CHARACTER NO-UNDO.

  RUN broker-get-attribute-list IN adm-broker-hdl
      (INPUT THIS-PROCEDURE, 
       INPUT ?,           /* Use the defined list of attributes to return */
       OUTPUT p-attr-list) NO-ERROR.

  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE new-state :
/* -----------------------------------------------------------
   Purpose:     Stub to send state message off to the broker process.
   Parameters:  state name (CHARACTER) - may also contain one or more
                link names to pass state message through, as part of a
                comma-separated list.
   Notes:       
-------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  RUN broker-new-state IN adm-broker-hdl (THIS-PROCEDURE, p-state) NO-ERROR.

  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE notify :
/* -----------------------------------------------------------
   Purpose:     Stub to pass notify command to broker process
   Parameters:  method name (CHARACTER) - may also include one or more
                link types to pass message through as part of commas-separated
                list.
   Notes:       
-------------------------------------------------------------*/   
  DEFINE INPUT PARAMETER p-method AS CHARACTER NO-UNDO.

  RUN broker-notify IN adm-broker-hdl (THIS-PROCEDURE, p-method) NO-ERROR.
  IF RETURN-VALUE = "ADM-ERROR":U THEN 
      RETURN "ADM-ERROR":U.  

  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-trad-widgets :
/*------------------------------------------------------------------------------
  Purpose:    Traduá∆o dos hard-coded view-as de atributos 
  Parameters: p-wh-frame - handle do frame
  Notes:       
------------------------------------------------------------------------------*/

  define input param p-wh-frame as widget-handle no-undo.

  define var wh-child     as widget-handle no-undo. 
  define var c-aux        as char          no-undo.
  define var i-aux        as integer       no-undo.  
  define var c-contexto   as char          no-undo init "".

  
  assign p-wh-frame:BGCOLOR = ?
         p-wh-frame:FONT    = 1
         p-wh-frame = p-wh-frame:FIRST-CHILD
         wh-child   = p-wh-frame:FIRST-CHILD.
  

  do  while valid-handle(wh-child):

      

      case wh-child:type:
          when "RADIO-SET" then do:
              if  wh-child:table <> ? then do:
                  assign c-aux = wh-child:radio-buttons.
                  if  wh-child:private-data <> "" 
                  and wh-child:private-data <> ? then 
                      assign c-contexto = wh-child:private-data. 
                  else
                      assign c-contexto = "*".  
                  do  i-aux = 1 to num-entries(wh-child:radio-buttons):
                      if  (i-aux mod 2) <> 0 then do:
                          run utp/ut-liter.p (input replace(entry(i-aux, wh-child:radio-buttons), chr(32), "_"),
                                              input c-contexto,
                                              input "R"). 
                          assign entry(i-aux, c-aux) = return-value.
                      end.
                  end.                                              
                  assign wh-child:radio-buttons = c-aux.
              end.
          end.
          when "BUTTON" then do:
              if  wh-child:label <> ?
              and wh-child:label <> "" then do:
                  run utp/ut-liter.p (input replace(wh-child:label, chr(32), "_"),
                                      input "",
                                      input "C"). 
                  assign wh-child:label = trim(return-value).
              end. 
              if  wh-child:help <> "" 
              and wh-child:help <> ? then do:
                  run utp/ut-liter.p (input replace(wh-child:help, chr(32), "_"),
                                      input "",
                                      input "R"). 
                  assign wh-child:help = return-value
                         wh-child:tooltip = trim(return-value).
              end.         

          end.
      end case.
      assign wh-child = wh-child:next-sibling.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE set-attribute-list :
/* -----------------------------------------------------------
      Purpose:     Accepts the value of the complete object attribute list
                   and runs procedures to set individual attributes.
      Parameters:  INPUT comma-separated attribute list.
      Notes:       Not all attributes are settable. Those which are a
                   part of an event such as enable/disable (which set
                   ENABLED on/off) or hide/view (which set HIDDEN on/off)
                   can be queried through get-attribute but cannot be set.
    -------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-attr-list    AS CHARACTER NO-UNDO.

  RUN broker-set-attribute-list IN adm-broker-hdl
      (INPUT THIS-PROCEDURE, INPUT p-attr-list) NO-ERROR.

  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE set-position :
/* -----------------------------------------------------------
  Purpose:     Moves an object to a specified position.
  Parameters:  ROW and COLUMN 
  Notes:       
-------------------------------------------------------------*/

    DEFINE INPUT PARAMETER p-row    AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER p-col    AS DECIMAL NO-UNDO.

    IF VALID-HANDLE(adm-object-hdl) THEN
    DO:     
      /* If this is a Window or a Dialog box which is being positioned,
         then the special value 0 means to center the object in that
         dimension (0,0 means center on the screen - 0 can be used to
         signal this because 0 is an invalid row or column position). */
      
        DEFINE VARIABLE parent-hdl AS HANDLE NO-UNDO.
        IF adm-object-hdl:TYPE = "WINDOW":U THEN
        DO:
          IF p-row = 0 THEN p-row = 
            (SESSION:HEIGHT-CHARS - adm-object-hdl:HEIGHT-CHARS) / 2.
          IF p-col = 0 THEN p-col = 
            (SESSION:WIDTH-CHARS - adm-object-hdl:WIDTH-CHARS) / 2.
        END.
        /* A Dialog naturally centers on its parent and positions relative
           to its parent, so we must adjust for that. */
        ELSE IF adm-object-hdl:TYPE = "DIALOG-BOX":U THEN
        DO:
          parent-hdl = adm-object-hdl:PARENT.
          IF p-row = 0 THEN p-row = 
            ((SESSION:HEIGHT-CHARS - adm-object-hdl:HEIGHT-CHARS) / 2) -
              parent-hdl:ROW.
          IF p-col = 0 THEN p-col = 
            ((SESSION:WIDTH-CHARS - adm-object-hdl:WIDTH-CHARS) / 2) -
              parent-hdl:COL.
        END.
        /* If the row or column wound up being between 0 and 1 after the 
           calculation, change it, because otherwise Progress will complain. */
        IF p-row GE 0 AND p-row < 1 THEN p-row = 1.
        IF p-col GE 0 AND p-col < 1 THEN p-col = 1.
      
      /* Set object's position */
      ASSIGN adm-object-hdl:ROW    =   p-row 
             adm-object-hdl:COLUMN =   p-col.
    END.  
    RETURN.

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */




 

/* _UIB-CODE-BLOCK-END */







/* ***************************  Main Block  *************************** */

/* Initialize page number and object handle attributes. */
RUN set-attribute-list ("CURRENT-PAGE=0,ADM-OBJECT-HANDLE=":U +
    STRING(adm-object-hdl)). 


/* Best default for GUI applications - this will apply to the whole session: */
PAUSE 0 BEFORE-HIDE.

on  CTRL-TAB anywhere do:
    if valid-handle(focus) then
      apply "leave" to focus.
    assign c-state-folder = "no".

    if  valid-handle(h-ctrl-tab) then do:
        run get-attribute in h-ctrl-tab('folder-labels':U).
        assign i-ctrl-tab-folder = NUM-ENTRIES(return-value,'|':U).

        run get-attribute('current-page':U).
        assign i-ctrl-tab-page = int(return-value) + 1.

        do while c-state-folder = "no":
            run pi-state-folder in h-ctrl-tab (input i-ctrl-tab-page).
            assign c-state-folder = RETURN-VALUE.

            if c-state-folder = "no" 
            then assign i-ctrl-tab-page = i-ctrl-tab-page + 1.
            else run select-page(i-ctrl-tab-page).

            if i-ctrl-tab-page > i-ctrl-tab-folder 
            then do: 
                assign i-ctrl-tab-page = 1.
                leave.
            end.
        end.

        do while c-state-folder = "no":
            run pi-state-folder in h-ctrl-tab (input i-ctrl-tab-page).
            assign c-state-folder = RETURN-VALUE.

            if c-state-folder = "no" 
            then assign i-ctrl-tab-page = i-ctrl-tab-page + 1.
            else run select-page(i-ctrl-tab-page).

            if i-ctrl-tab-page > i-ctrl-tab-folder 
            then do: 
                assign i-ctrl-tab-page = 1.
                leave.
            end.
        end.
    end.
end.

on  SHIFT-CTRL-TAB anywhere do:
    if valid-handle(focus) then
      apply "leave" to focus.
    assign c-state-folder = "no".

    if  valid-handle(h-ctrl-tab) then do:
        run get-attribute in h-ctrl-tab('folder-labels':U).
        assign i-ctrl-tab-folder = NUM-ENTRIES(return-value,'|':U).

        run get-attribute('current-page':U).
        assign i-ctrl-tab-page = int(return-value) - 1.

        do while c-state-folder = "no":
            run pi-state-folder in h-ctrl-tab (input i-ctrl-tab-page).
            assign c-state-folder = RETURN-VALUE.

            if c-state-folder = "no" 
            then assign i-ctrl-tab-page = i-ctrl-tab-page - 1.
            else run select-page (i-ctrl-tab-page).

            if i-ctrl-tab-page < 1
            then do: 
                assign i-ctrl-tab-page = i-ctrl-tab-folder.
                leave.
            end.
        end.

        do while c-state-folder = "no":
            run pi-state-folder in h-ctrl-tab (input i-ctrl-tab-page).
            assign c-state-folder = RETURN-VALUE.

            if c-state-folder = "no" 
            then assign i-ctrl-tab-page = i-ctrl-tab-page - 1.
            else run select-page(i-ctrl-tab-page).

            if i-ctrl-tab-page < 1
            then do: 
                assign i-ctrl-tab-page = i-ctrl-tab-folder.
                leave.
            end.
        end.
    end.
end.

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */




PROCEDURE adm-change-page :
/* -----------------------------------------------------------
      Purpose:    Views objects on a newly selected page, initializing
                  them if the page has not yet been seen.
      Parameters: <none>
      Notes:      In character mode, when switching from the main window
                  to a page which is another window (in GUI), the
                  main window's default frame must be hidden; and when
                  returning it must be viewed. This is done below.
-------------------------------------------------------------*/   

  /* EPC - Change Page de Container */ 
  
        /***************************************************************
**
** I-EPC014.I - EPC para Evento CHANGE-PAGE de Container 
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.


    
        run get-attribute('current-page':U). 
         /* DPC */
        if  c-nom-prog-dpc-mg97 <> "" and
            c-nom-prog-dpc-mg97 <> ? then do:                  
            run value(c-nom-prog-dpc-mg97) (input "CHANGE-PAGE":U, 
                                            input "CONTAINER":U,
                                            input h-ctrl-tab,
                                            input frame fPage0:handle  ,
                                            input "",
            
                                            input ?).
            
        end.
        
         /* APPC */
        if  c-nom-prog-appc-mg97 <> "" and
            c-nom-prog-appc-mg97 <> ? then do:           
            run value(c-nom-prog-appc-mg97)  (input "CHANGE-PAGE":U, 
                                             input "CONTAINER":U,
                                             input h-ctrl-tab,
                                             input frame fPage0:handle  ,
                                             input "",
            
                                             input ?).
            
        end.
         /* UPC */
        if  c-nom-prog-upc-mg97 <> "" and
            c-nom-prog-upc-mg97 <> ? then do:                  
            run value(c-nom-prog-upc-mg97) (input "CHANGE-PAGE":U, 
                                            input "CONTAINER":U,
                                            input h-ctrl-tab,
                                            input  frame fPage0:handle,
                                            input "",
            
                                            input ?).
            
        end. 
    

/* I-EPC014.I */
 
  

  RUN broker-change-page IN adm-broker-hdl (INPUT THIS-PROCEDURE) NO-ERROR.

  /* EPC - After Change Page de Container */ 
  
        /***************************************************************
**
** I-EPC014.I - EPC para Evento After CHANGE-PAGE de Container 
** 
***************************************************************/


     ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.


    
        run get-attribute('current-page':U). 
         /* DPC */
        if  c-nom-prog-dpc-mg97 <> "" and
            c-nom-prog-dpc-mg97 <> ? then do:                  
            run value(c-nom-prog-dpc-mg97) (input "AFTER-CHANGE-PAGE":U, 
                                            input "CONTAINER":U,
                                            input h-ctrl-tab,
                                            input frame fPage0:handle  ,
                                            input "",
            
                                            input ?).
            
        end.
        
         /* APPC */
        if  c-nom-prog-appc-mg97 <> "" and
            c-nom-prog-appc-mg97 <> ? then do:           
            run value(c-nom-prog-appc-mg97)  (input "AFTER-CHANGE-PAGE":U, 
                                             input "CONTAINER":U,
                                             input h-ctrl-tab,
                                             input frame fPage0:handle  ,
                                             input "",
            
                                             input ?).
            
        end.
         /* UPC */
        if  c-nom-prog-upc-mg97 <> "" and
            c-nom-prog-upc-mg97 <> ? then do:                  
            run value(c-nom-prog-upc-mg97) (input "AFTER-CHANGE-PAGE":U, 
                                            input "CONTAINER":U,
                                            input h-ctrl-tab,
                                            input  frame fPage0:handle,
                                            input "",
            
                                            input ?).
            
        end. 
    

/* I-EPC014.I */
 
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE delete-page :
/* -----------------------------------------------------------
      Purpose:     Destroys all objects on the current page.
      Parameters:  INPUT page number
      Notes:       
    -------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-page# AS INTEGER NO-UNDO.

  RUN broker-delete-page IN adm-broker-hdl 
      (INPUT THIS-PROCEDURE, INPUT p-page#).

  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE init-object :
/* -----------------------------------------------------------
   Purpose:     RUNS an object procedure PERSISTENT and initializes
                default links
   Parameters:  INPUT procedure name, parent handle, attribute-list,
                OUTPUT procedure handle
   Notes:       init-object calls are generated by the UIB 
                in adm-create-objects
-------------------------------------------------------------*/   
  DEFINE INPUT PARAMETER  p-proc-name   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER  p-parent-hdl  AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER  p-attr-list   AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER p-proc-hdl    AS HANDLE    NO-UNDO.

  RUN broker-init-object IN adm-broker-hdl
      (INPUT THIS-PROCEDURE, INPUT p-proc-name, INPUT p-parent-hdl,
       INPUT p-attr-list, OUTPUT p-proc-hdl) NO-ERROR.
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE init-pages :
/* -----------------------------------------------------------
      Purpose:     Initializes one or more pages in a paging
                   control without actually viewing them. 
                   This can be used either for initializing pages
                   at startup without waiting for them to be
                   selected, or for creating additional or
                   replacement pages after startup.
      Parameters:  INPUT comma-separated list of page numbers
      Notes:       Generally this method does not need to be used,
                   unless the user specifically wants to incur the
                   overhead of creating and initializing pages before
                   they are first viewed. When one page in a multi-page
                   SmartContainer has a SmartLink dependency on another
                   page, the UIB will automatically generate the calls
                   to init-pages to assure that the right other pages have been
                   initialized when a page is selected for the first time.
    -------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-page-list      AS CHARACTER NO-UNDO.  

  RUN broker-init-pages IN adm-broker-hdl (INPUT THIS-PROCEDURE,
      INPUT p-page-list) NO-ERROR.

  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-retorna-appc :
/*------------------------------------------------------------------------------
  Purpose:  Retorna o nome do programa APPC   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  return c-nom-prog-appc-mg97.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-retorna-dpc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  return c-nom-prog-dpc-mg97.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-retorna-upc :
/*------------------------------------------------------------------------------
  Purpose:  Retonra o nome do programa UPC    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  return c-nom-prog-upc-mg97.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-retorna-vars-hlp :
/*------------------------------------------------------------------------------
  Purpose:   Retorna variaveis de acesso ao Help
  Parameters: p-num-topico-hlp - numero do topico do programa
              p-nom-manual-hlp - nome do arquivo hlp do modulo do programa
  Notes:       
------------------------------------------------------------------------------*/

define output parameter p-num-topico-hlp as integer no-undo.
define output parameter p-nom-manual-hlp as char format "x(06)" no-undo.

assign p-num-topico-hlp = i-num-topico-hlp-mg97
       p-nom-manual-hlp = c-nom-manual-hlp-mg97.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE select-page :
/* -----------------------------------------------------------
      Purpose:     Makes a new set of objects associated with a page number
                   current, by hiding the previous page, if any,
                   creating the objects in the new page if the page hasn't
                   been initialized, and viewing the new page.
      Parameters:  INPUT new page number
      Notes:       
    -------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-page#     AS INTEGER   NO-UNDO.

  RUN broker-select-page IN adm-broker-hdl (INPUT THIS-PROCEDURE,
      INPUT p-page#) NO-ERROR.

  END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE view-page :
/* -----------------------------------------------------------
      Purpose:     Makes a new set of objects associated with a page number
                   current, without hiding the previous page, if any,
                   creating the objects in the new page if the page hasn't
                   been initialized, and viewing the new page.
      Parameters:  INPUT new page number
      Notes:       This method does not reset the value of adm-current-page,
                   because the new page is being viewed without hiding the
                   old one. adm-current-page is the most recently "selected"
                   page.
    -------------------------------------------------------------*/   

  DEFINE INPUT PARAMETER p-page#      AS INTEGER   NO-UNDO.

  RUN broker-view-page IN adm-broker-hdl (INPUT THIS-PROCEDURE,
      INPUT p-page#).

  END PROCEDURE.
/* This ENDIF statement needs to stay here (or with the last procedure in the
   include file) to balance the &IF adm-container at the top: */


/* _UIB-CODE-BLOCK-END */




 

/* Procedure Description
"Library para window consulta simples"
*/


/*--------------------------------------------------------------------------
    Library     : w-livre.i
    Purpose     : Permitir customizaá∆o para as window de consulta simples

    Syntax      : {include/w-livre.i}

    Description : Library utilizada para customizaá∆o da window de consulta
                  simples

    Author(s)   : Gilsinei
    Created     : 06/03/1997
    Notes       :
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define variable wh-pesquisa                as handle no-undo.
define variable wh-relacionamento          as handle no-undo.
define variable wh-consulta                as handle no-undo.
define variable v-row-table                as rowid no-undo.
define variable wh-programa                as handle no-undo.
define variable c-container                as char   no-undo.
define variable wh-container               as handle no-undo.
define variable container                  as char   no-undo.
def new global shared var r-registro-atual as rowid  no-undo.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */
 


/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 



/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */







/* ***************************  Main Block  *************************** */

assign current-window:max-width-chars = current-window:width-chars
       current-window:max-height-chars = current-window:height-chars.

run pi-trad-menu (input w-livre:menubar).

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */




PROCEDURE pi-after-initialize :
/*------------------------------------------------------------------------------
  Purpose:     C¢digo a ser executado ap¢s a inicializaá∆o
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if  valid-handle(h_p-exihel) then
      run set-prog-parent in h_p-exihel (program-name(1)).

  /*Traduá∆o dos campos de tela*/

/*fim traduá∆o dos campos de tela*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-before-initialize :
/*------------------------------------------------------------------------------
  Purpose:     C¢digo a ser executado antes da inicializaá∆o
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-consulta :
/*------------------------------------------------------------------------------
  Purpose:     Chama o programa de pesquisa e reposiciona a query
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var i-inicio    as integer no-undo.
def var i-fim       as integer no-undo.
def var rw-reserva  as rowid   no-undo.

    RUN Who-Is-The-Container IN adm-broker-hdl
        (INPUT this-procedure,
         OUTPUT c-container).

    assign i-inicio     = r-index(THIS-PROCEDURE:file-name,"/") + 1
           i-fim        = r-index(THIS-PROCEDURE:file-name,".w").

    if i-fim < r-index(THIS-PROCEDURE:file-name,".r") then
       i-fim = r-index(THIS-PROCEDURE:file-name,".r").
    if i-inicio < r-index(THIS-PROCEDURE:file-name,"\") then
       i-inicio = r-index(THIS-PROCEDURE:file-name,"\") + 1.

    run utp/ut-cons.w (input substring(THIS-PROCEDURE:file-name,i-inicio , i-fim - i-inicio)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-disable-menu :
def var p-button-enable as char no-undo.
  
  RUN get-button-enable IN h_p-exihel (OUTPUT p-button-enable).
  assign menu-item mi-consultas:sensitive in menu m-livre = (entry(1,p-button-enable)= string(yes))
         menu-item mi-imprimir:sensitive in menu m-livre = (entry(2,p-button-enable)= string(yes))
         menu-item mi-sair:sensitive in menu m-livre = (entry(3,p-button-enable)= string(yes))
         menu-item mi-conteudo:sensitive in menu m-livre = (entry(4,p-button-enable)= string(yes))
         menu-item mi-sobre:sensitive in menu m-livre = (entry(4,p-button-enable)= string(yes)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-trad-menu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/****************************************************************
**
**  I-TRDMN.I - Traduá∆o dos Menus das Janelas
**              Conte£do da pi-trad-menu nas Method Library
**  20/03/1997 - Gilsinei
**  01/07/1998 - John C. Jaraceski
****************************************************************/

define input param p-wh-menu as widget-handle no-undo.

define var wh-menu-child      as widget-handle no-undo.
define var wh-menu-grandchild as widget-handle no-undo.

assign p-wh-menu = p-wh-menu:FIRST-CHILD.

do while valid-handle(p-wh-menu):
    if p-wh-menu:LABEL <> ? then do:
        if p-wh-menu:LABEL = "A&juda" or 
           p-wh-menu:LABEL = "&Ajuda" then
            run utp/ut-liter.p (input "Aj&uda", input "*", input "R").
        else
            run utp/ut-liter.p (input replace(p-wh-menu:LABEL, chr(32), "_"),
                                input "*",
                                input "R").
        
        assign p-wh-menu:LABEL = trim(RETURN-VALUE).
    end.
    
    if can-query(p-wh-menu, "FIRST-CHILD") then do:
        assign wh-menu-child = p-wh-menu:FIRST-CHILD.
        
        do while valid-handle(wh-menu-child):
            if  wh-menu-child:LABEL <> ? then do:
                if wh-menu-child:LABEL = "A&juda" or 
                   wh-menu-child:LABEL = "&Ajuda" then
                    run utp/ut-liter.p (input "Aj&uda", input "*", input "R").
                else
                    run utp/ut-liter.p (input replace(wh-menu-child:LABEL, chr(32), "_"),
                                        input "*",
                                        input "R").
                
                assign wh-menu-child:LABEL = trim(RETURN-VALUE).
            end.
            
            if can-query(wh-menu-child, "FIRST-CHILD") then do:
                assign wh-menu-grandchild = wh-menu-child:FIRST-CHILD.
                
                do while valid-handle(wh-menu-grandchild):
                    if wh-menu-grandchild:LABEL <> ? then do:
                        if wh-menu-grandchild:LABEL = "A&juda" or
                           wh-menu-grandchild:LABEL = "&Ajuda" then
                            run utp/ut-liter.p (input "Aj&uda", input "*", input "R").
                        else
                            run utp/ut-liter.p (input replace(wh-menu-grandchild:LABEL, chr(32), "_"),
                                                input "*",
                                                input "R").
                        
                        assign wh-menu-grandchild:LABEL = trim(RETURN-VALUE).
                    end.
                    
                    assign wh-menu-grandchild = wh-menu-grandchild:NEXT-SIBLING.
                end.
            end.
            
            assign wh-menu-child = wh-menu-child:NEXT-SIBLING.
        end.
    end.
    
    assign p-wh-menu = p-wh-menu:NEXT-SIBLING.
end.

/* I-TRDMN.I */
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE pi-trata-state :
/*------------------------------------------------------------------------------
  Purpose:     Trata as mudanáas de estado (State-Changed)
  Parameters:  INPUT Handle da procedure pai
               INPUT C¢digo do Estado
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl   AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state        AS CHAR   NO-UNDO.

  CASE entry(1, p-state, "|":U):
      WHEN 'Consulta':U THEN DO:
          run pi-consulta.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */




 
/***************************************************************************
**  Programa : UT-GLOB.I
**  Include padr„o para definiÁ„o de variaveis globais.
***************************************************************************/ 
/* altera?ío feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */

/* est† verificaá∆o se faz necess†ria devido aos programas */
   


    /* Tenta identificar pelo ADM-CONTAINER */
    
    
        

                    
        
    

    /* Se ainda nío identificou se ? window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    

    /* Se ainda nío identificou se ? window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
    
    /*
    &IF DEFINED(WenIsWindow) &THEN
        RUN registerWindow IN hWenController (THIS-PROCEDURE, CURRENT-WINDOW).
    &ELSEIF DEFINED(WenIsDialog) &THEN
        RUN registerWindow IN hWenController (THIS-PROCEDURE, CURRENT-WINDOW).
    &ENDIF
    */

    /* Se tem janela */
    

        ON 'U9':U ANYWHERE
        DO:
            IF VALID-HANDLE(hWenController) THEN DO:
                RUN registerWindow IN hWenController (THIS-PROCEDURE, SELF).
            END.
        END.

    

    /* Se tem dialog */
    


            /* criados pelos DataViewer n∆o utilizarem a include i-prgvrs */ 
/* e dessa forma n∆o chamarem a include i-wendef.i que define essa veri†vel. */

/* fim da alateraá∆o */


     
     def new global shared var i-ep-codigo-usuario  like mgmulti.empresa.ep-codigo no-undo.
     def new Global shared var l-implanta           as logical    init no.
     def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
     def new global shared var i-num-ped-exec-rpw   as integer no-undo.   
     def var rw-log-exec                            as rowid no-undo.
     def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
     def new global shared var l-rpc as logical no-undo.
     def var c-erro-rpc as character format "x(60)" initial " " no-undo.
     def var c-erro-aux as character format "x(60)" initial " " no-undo.
     def var c-ret-temp as char no-undo.
     def var h-servid-rpc as handle no-undo.     
     def new global shared var r-registro-atual as rowid no-undo.
     def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
     def new global shared var h-rsocial as handle no-undo.
     def new global shared var l-achou-prog as logical no-undo.

      /* Vari·veis Padr„o DWB / Datasul HR */
     def new global shared var i-num-ped as integer no-undo.         
     def new global shared var v_cdn_empres_usuar   like mgmulti.empresa.ep-codigo        no-undo.
     def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.
     def new global shared var h_prog_segur_estab     as handle                   no-undo.
     def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
     def new global shared var v_num_tip_aces_usuar   as int                      no-undo.


/* Transformacao Window */

    if session:window-system <> "TTY" then do:
                /* ---------------------------------------------------------------------------------------------------
   Autor      : Medeiros
   Data       : 01/Dez/97
   Objetivo   : Include usada nos programas que fazem interface 
                con a API do windows.
    
   Parametros : Nenhum.
--------------------------------------------------------------------------------- */


  
    /* 32-bit definitions, Progress 8.2+ */

    /* data types */
                   /* libraries */
                     /* messages */
/* mouse buttons */
/* scrollbars */
/* editors */
   /* some window styles */
/* some extended window styles */
/* system commands/menu */

/* placement order (Z-order) */
 
/* window-positioning flags */
/* get a handle to the procedure definitions */

   DEFINE NEW GLOBAL SHARED VARIABLE hpApi AS HANDLE NO-UNDO.
    IF NOT VALID-HANDLE(hpApi) OR
          hpApi:TYPE <> "PROCEDURE":U OR 
          hpApi:FILE-NAME <> "utp/ut-win.p":U THEN 
      RUN utp/ut-win.p PERSISTENT SET hpApi.
    /* forward function declarations. Must not be included in windows.p : */
   /* ---------------------------------------------------------------------------------------------------
   Autor      : Medeiros
   Data       : 01/Dez/97
   Objetivo   : Include usada nos programas que fazem interface 
                con a API do windows.
    
   Parametros : Nenhum.
--------------------------------------------------------------------------------- */   

/* prevent multiple inclusion: */


/* start persistent procedure holding the function implementations.
   The forward declarations are needed in winfunc.p, but the
   "run winfunc.p persistent" part must be prevented in winfunc.p : */
     
DEFINE NEW GLOBAL SHARED VARIABLE hpWinFunc AS HANDLE NO-UNDO.

  IF NOT VALID-HANDLE(hpWinFunc) or  
         hpWinFunc:TYPE <> "PROCEDURE":U or
         hpWinFunc:FILE-NAME <> "utp/ut-func.p":U THEN 
     RUN utp/ut-func.p PERSISTENT SET hpWinFunc.


/* --- the forward declarations : --- */

FUNCTION GetLastError      /* 1:1 implementation of API */
         RETURNS INTEGER   /* = dwErrorID */
         () 
         IN hpWinFunc.    

FUNCTION GetParent         /* 1:1 implementation of API */
         RETURNS INTEGER   /* = hWnd van parent */
         (input hwnd as INTEGER) 
         IN hpWinFunc.    

FUNCTION ShowLastError     /* calls GetLastError and views it as alert-box */
         RETURNS INTEGER   /* = dwErrorID */
         () 
         IN hpWinFunc.    

FUNCTION CreateProcess     /* wrapper for the big API definition */
         RETURNS INTEGER   /* = if success then hProcess else 0  */
         (input CommandLine as CHAR,
          input CurrentDir  as CHAR,
          input wShowWindow as INTEGER) 
         in hpWinFunc.    

/* &IF DEFINED(WINFUNC_I)=0 */

 

/* &IF DEFINED(WINDOWS_I)=0 */

 
      define var h-prog     as handle  no-undo.
      define var h-pai      as handle  no-undo.
      define var c-prog-tec as char    no-undo format "x(256)".
      define var i-template as integer no-undo.
    end.  

/* Transformacao Window */
/* Retorno RPC */

    procedure pi-seta-return-value:
    def input param ret as char no-undo.
    return ret.
  end procedure.


/* Retorno RPC */

/* ut-glob.i */

 

/* _UIB-CODE-BLOCK-END */





/* ***********  Runtime Attributes and AppBuilder Settings  *********** */


/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fPage0
   L-To-R                                                               */
/* BROWSE-TAB brReporte fiQtEmenda2 fPage0 */
/* SETTINGS FOR BROWSE brReporte IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btCancel IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btElimina IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btLotes IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btModifica IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btSave IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btSelecao IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCodCtrab IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCodEstabel IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiCodOperador IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDescricao IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDesCtrab IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDtInicio IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiDtTermino IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiEmenda1 IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiEmenda2 IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiHrInicio IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiHrTermino IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiItem IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiLote IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiNomeEstabel IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiNomeOperador IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiQtEmenda1 IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiQtEmenda2 IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiQtLote IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiQtTotal IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiRefugo IN FRAME fPage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSaldoBobina IN FRAME fPage0
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */



/* Setting information for Queries and Browse Widgets fields            */


/* Query rebuild information for BROWSE brReporte
     _START_FREEFORM
open query brReporte
    for each tt-reporta .
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brReporte */


 



/* ************************  Control Triggers  ************************ */


ON END-ERROR OF w-livre /* Template Livre <Insira complemento> */
OR ENDKEY OF w-livre ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON WINDOW-CLOSE OF w-livre /* Template Livre <Insira complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON MOUSE-SELECT-DBLCLICK OF brReporte IN FRAME fPage0
DO:
 IF brReporte:NUM-SELECTED-ROWS > 0 THEN DO ON ERROR UNDO, RETURN NO-APPLY:
     for first tt-reporta 
         where tt-reporta.nr-ord-produ = int(tt-reporta.nr-ord-produ:screen-value 
               in browse brReporte) exclusive-lock:
     end.
     if avail tt-reporta then do:                                     
         if tt-reporta.gera-pallet:screen-value in browse brReporte = "        *":U 
         and tt-reporta.gera-pallet =  "        *" then do:
             assign tt-reporta.gera-pallet:screen-value in browse brReporte = "":U
                    tt-reporta.gera-pallet = "".
         end.
         else do:
             assign tt-reporta.gera-pallet:screen-value in browse brReporte =  "        *":U
                    tt-reporta.gera-pallet =  "        *".
         end.
         apply "LEAVE":U to browse brReporte.
     end.
  END.   
END.

/* _UIB-CODE-BLOCK-END */




ON ROW-LEAVE OF brReporte IN FRAME fPage0
DO:
 
    if avail tt-reporta then do:
       assign tt-reporta.qt-reporte = input browse brReporte tt-reporta.qt-reporte.
       run calculaQuant.
    end.

END.

/* _UIB-CODE-BLOCK-END */

ON CHOOSE OF btpeso IN FRAME fPage0 /* Add */
DO:
   
   RUN sfc/essf0002D-A1.W.
   IF L-PESO-BAL THEN DO :
       ASSIGN btpeso:SENSITIVE IN FRAME fPage0 = NO.
       MESSAGE "DIGITAÄ«O DE PESAGEM LIBERADA TEMPORARIAMENTE," SKIP "ENQUANTO SESS«O ESTIVER ATIVA."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
       

END.

ON CHOOSE OF btAdd IN FRAME fPage0 /* Add */
DO:

    def var cTime as char no-undo.
    
    do with frame fPage0.

        enable fiCodEstabel fiCodCtrab fiCodOperador                           fiDtInicio fiHrInicio fiDtTermino fiHrTermino                           fiLote fiEmenda1 fiEmenda2 btSelecao                           btModifica btElimina fiRefugo  btLotes btCancel btSave.
        display fiCodEstabel fiCodCtrab fiCodOperador                           fiDtInicio fiHrInicio fiDtTermino fiHrTermino                           fiLote fiEmenda1 fiEmenda2 btSelecao                           btModifica btElimina fiRefugo  btLotes.
        display fiNomeEstabel fiDesCtrab fiNomeOperador fiQtLote                             fiQtEmenda1 fiQtEmenda2 fiDescricao                              fiRefugo fiQtTotal fiItem.
        disable btAdd .
        
        

/* ========================================================================
    Rotina para sugerir o operador logado */



  FIND FIRST operador WHERE
      operador.char-2 = c-seg-usuario
      NO-LOCK NO-ERROR.

  IF AVAIL operador THEN DO:
      ASSIGN fiCodOperador:SCREEN-VALUE = operador.cod-operador
             finomeoperador:SCREEN-VALUE = operador.nom-operador.
  END.

  ASSIGN fiCodOperador:SENSITIVE = NO.


/* fim Rotina para sugerir o operador logado 
========================================================================   */




        find first param-cp no-lock.

        run pi-sec-to-formatted-time (input time,
                                      output cTime).
            
        assign fiCodEstabel:screen-value = "{cdp\poloestab.i 422}" /*solic-318*//*param-cp.cod-estabel*/
               fiDtInicio:screen-value   = string(today)
               fiDtTermino:screen-value  = string(today)
               fiHrInicio:screen-value   = cTime
               fiHrTermino:screen-value  = cTime.

        apply 'leave' to fiCodEstabel.

    end.

    empty temp-table tt-reporta.
    empty temp-table tt-lote-reporte.
    open query brReporte     for each tt-reporta .

    apply "entry":U to fiCodCtrab in frame fPage0.
    return no-apply.

END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btCancel IN FRAME fPage0 /* Cancel */
DO:
  
    do with frame fPage0.
        assign fiSaldoBobina = 0
               fiQtTotal     = 0.
        disable fiCodEstabel fiCodCtrab fiCodOperador                           fiDtInicio fiHrInicio fiDtTermino fiHrTermino                           fiLote fiEmenda1 fiEmenda2 btSelecao                           btModifica btElimina fiRefugo  btLotes btCancel btSave.
        display fiCodEstabel fiCodCtrab fiCodOperador                           fiDtInicio fiHrInicio fiDtTermino fiHrTermino                           fiLote fiEmenda1 fiEmenda2 btSelecao                           btModifica btElimina fiRefugo  btLotes.
        display fiNomeEstabel fiDesCtrab fiNomeOperador fiQtLote                             fiQtEmenda1 fiQtEmenda2 fiDescricao                              fiRefugo fiQtTotal fiItem.
        enable btAdd .
    end.
    disp fiSaldoBobina
         fiQtTotal
       with frame fPage0.

    empty temp-table tt-reporta.
    empty temp-table tt-lote-reporte.
    open query brReporte     for each tt-reporta .

END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btElimina IN FRAME fPage0 /* Elimina */
DO:
  
    if  brReporte:num-selected-rows > 0 then do on error undo, return no-apply:
        get current brReporte.
        for each tt-lote-reporte where
                 tt-lote-reporte.nr-ord-produ = tt-reporta.nr-ord-produ:
            delete tt-lote-reporte.
        end.
        delete tt-reporta.
        brReporte:delete-current-row() in frame fPage0.
        run calculaQuant.
    end.

END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btLotes IN FRAME fPage0 /* Lotes */
DO:
  
    def var rRaw      as raw no-undo.
    def var deQuant   as dec no-undo.
    def var deSaldo   as dec no-undo.
    def var deTotProd as dec no-undo.

    def var rw-tt-reporta as rowid no-undo.

    if  brReporte:num-selected-rows > 0 then do on error undo, return no-apply:
        
        get current brReporte.
        assign rw-tt-reporta = rowid(tt-reporta).

        raw-transfer tt-reporta to rRaw.

        assign deSaldo = input frame fPage0 fiQtLote + 
                         input frame fPage0 fiQtEmenda1 + 
                         input frame fPage0 fiQtEmenda2.

        assign deTotProd = 0.
        for each tt-reporta:
            assign deTotProd = deTotProd + tt-reporta.qt-reporte.
        end.
        FIND ord-prod WHERE ord-prod.nr-ord-produ = 
            INT(tt-reporta.nr-ord-produ:screen-value in browse brReporte) NO-LOCK NO-ERROR.
            
        find first ctrab where ctrab.cod-ctrab = fiCodCtrab:SCREEN-VALUE
                no-lock no-error.
        
        if avail ord-prod then 
        find first oper-ord where oper-ord.nr-ord-produ = ord-prod.nr-ord-produ
                no-lock no-error.
                
        if avail oper-ord and avail ctrab and oper-ord.gm-codigo <> ctrab.gm-codigo then do:
            run utp/ut-msgs.p (input "show",
                                       input 17006,
                                       input "Centro de trabalho " + ctrab.cod-ctrab + " informado, incompat°vel com grupo de m†quina " + oper-ord.gm-codigo + " da ordem selecionada! ").
            
            if today = 05/16/2010 then 
            run utp/ut-msgs.p (input "show",
                                       input 17006,
                                       input "Esta Ç uma nova trava para pesagens incorretas, qualquer dificuldade ligue 35 9988 3054 - Edson ").
            

            
            return no-apply.
            
        end.
         

            
        ASSIGN h-paizao = CURRENT-WINDOW. 
        IF (fiCodCtrab:SCREEN-VALUE = "gal1" OR fiCodCtrab:SCREEN-VALUE = "applied") OR l-peso-bal OR (AVAIL ord-prod AND ord-prod.cod-estabel <> "412" AND ord-prod.cod-estabel <> "422") THEN /*solic-318*/
            run sfc/essf0002d.w (input rRaw,
                                  input-output table tt-lote-reporte,
                                  input deSaldo,
                                  input deTotProd).
            ELSE
            run sfc/essf0002d-a.w (input rRaw,
                                  input-output table tt-lote-reporte,
                                  input deSaldo,
                                  input deTotProd).

        find first tt-reporta
            where rowid(tt-reporta) = rw-tt-reporta exclusive-lock no-error.
        
         IF l-peso-bal THEN DO:
                OUTPUT TO m:\dts\log_prd\essf0002.LOG APPEND.

                    DISP "Prog: essf0002 " FORMAT "x(19)" string(TODAY) "-" 
                         string(TIME,"hh:mm:ss") "-" 
                         " Logado: " string(c-seg-usuario) SKIP 
                         WITH WIDTH 300.
               
         END.

        for each tt-lote-reporte where
                 tt-lote-reporte.nr-ord-produ = tt-reporta.nr-ord-produ:

            assign deQuant = deQuant + tt-lote-reporte.quantidade.

            IF l-peso-bal THEN DO:
                DISP  tt-lote-reporte.lote tt-lote-reporte.quantidade tt-lote-reporte.nr-ord-produ 
                         WITH WIDTH 300.
                
            END.
        end.
         IF l-peso-bal THEN
            OUTPUT CLOSE.

        assign tt-reporta.qt-reporte = deQuant.

        brReporte:refresh().
        
       

        run /*calculaQuant.*/
        calculaQuantbaixaAut.
    
    end.

END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btModifica IN FRAME fPage0 /* Modifica */
DO:
  
    find current tt-reporta no-error.
    if avail tt-reporta then
        apply 'entry' to tt-reporta.cod-depos in browse brReporte.

END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btSave IN FRAME fPage0 /* Save */
DO:
    
    run calculaQuantRefugo.

    FIND FIRST param-global.
    find first param-cq no-lock no-error.
    find first param-cp no-lock no-error.


    run validaCampos .
    if return-value = 'nok' then
        return no-apply.

    run processaReportes.
    if return-value = 'nok' then
        return no-apply.
    
    RUN pi-gera-pallet.
    if return-value = 'nok' then
        return no-apply.

    do with frame fPage0.
        disable fiCodEstabel fiCodCtrab fiCodOperador                           fiDtInicio fiHrInicio fiDtTermino fiHrTermino                           fiLote fiEmenda1 fiEmenda2 btSelecao                           btModifica btElimina fiRefugo  btLotes btSave btCancel.
        enable btAdd .
    end.

    empty temp-table tt-reporta.
    empty temp-table tt-lote-reporte.
    open query brReporte     for each tt-reporta .
    

END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btSelecao IN FRAME fPage0 /* Seleá∆o */
DO:
  
    if can-find (first item where
                       item.it-codigo = fiItem:screen-value in frame fPage0) then do:
        run sfc/essf0002a.w (input fiItem:screen-value       in frame fPage0,
                                  input fiCodEstabel:screen-value in frame fPage0,
                                  input fiLote:screen-value       in frame fPage0,
                                  input fiEmenda1:screen-value    in frame fPage0,
                                  input fiEmenda2:screen-value    in frame fPage0,
                                  input-output table tt-reporta).
        open query brReporte     for each tt-reporta .
        run calculaQuant.
    end.
    else do:
        run utp/ut-msgs.p (input "show",
                           input 17006,
                           input "N∆o encontrado item para o lote de Consumo!").
    end.


END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF fiCodCtrab IN FRAME fPage0 /* Centro de Trabalho */
DO:
  
    for first ctrab fields (des-ctrab) no-lock where
              ctrab.cod-ctrab = self:screen-value:
        assign fiDesCtrab:screen-value in frame fPage0 = ctrab.des-ctrab.
                
    end.

END.

/* _UIB-CODE-BLOCK-END */




ON MOUSE-SELECT-DBLCLICK OF fiCodCtrab IN FRAME fPage0 /* Centro de Trabalho */
or 'f5' of fiCodCtrab DO:
  
    


/*--------------------------------------------------------------------------
    File        : ZoomVar.i
    Purpose     : Chamar o programa de zoom para as chaves estrangeiras
                  para vari†veis e atributos que receber∆o seus valores
                  de atributos com nome diferente.

    Syntax      : {include/zoomvar.i}

    Description : Chama o programa de zoom para os atributos que representam
                  chaves estrangeiras

    Author(s)   : Vanei
    Created     : 16/01/1997
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
  
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
        
        assign c-lista-campo = string(fiCodCtrab:handle in frame fPage0) + '|':U + 'cod-ctrab'.
        
            assign c-lista-campo = c-lista-campo + chr(10) + string(fiDesCtrab:handle in frame fPage0) + '|':U + 'des-ctrab'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    

    
  end.
                    
assign l-implanta = no.



/* _UIB-CODE-BLOCK-END */



  

END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF fiCodEstabel IN FRAME fPage0 /* Estabelecimento */
DO:
  
    for first estabelec fields (nome) no-lock where
              estabelec.cod-estabel = self:screen-value:
        assign fiNomeEstabel:screen-value in frame fPage0 = estabelec.nome.
    end.

END.

/* _UIB-CODE-BLOCK-END */




ON MOUSE-SELECT-DBLCLICK OF fiCodEstabel IN FRAME fPage0 /* Estabelecimento */
or 'f5' of fiCodEstabel DO:
  
    


/*--------------------------------------------------------------------------
    File        : ZoomVar.i
    Purpose     : Chamar o programa de zoom para as chaves estrangeiras
                  para vari†veis e atributos que receber∆o seus valores
                  de atributos com nome diferente.

    Syntax      : {include/zoomvar.i}

    Description : Chama o programa de zoom para os atributos que representam
                  chaves estrangeiras

    Author(s)   : Vanei
    Created     : 16/01/1997
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
  
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
        
        assign c-lista-campo = string(fiCodEstabel:handle in frame fPage0) + '|':U + 'cod-estabel'.
        
            assign c-lista-campo = c-lista-campo + chr(10) + string(fiNomeEstabel:handle in frame fPage0) + '|':U + 'nome'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    

    
  end.
                    
assign l-implanta = no.



/* _UIB-CODE-BLOCK-END */



  

END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF fiCodOperador IN FRAME fPage0 /* Operador */
DO:
    
    for first operador fields (nom-operador) no-lock where
              operador.cod-operador = replace(self:screen-value,"-",""):
        assign fiNomeOperador:screen-value in frame fPage0 = operador.nom-operador.
    end.

END.

/* _UIB-CODE-BLOCK-END */




ON MOUSE-SELECT-DBLCLICK OF fiCodOperador IN FRAME fPage0 /* Operador */
or 'f5' of fiCodoperador DO:
  
    


/*--------------------------------------------------------------------------
    File        : ZoomVar.i
    Purpose     : Chamar o programa de zoom para as chaves estrangeiras
                  para vari†veis e atributos que receber∆o seus valores
                  de atributos com nome diferente.

    Syntax      : {include/zoomvar.i}

    Description : Chama o programa de zoom para os atributos que representam
                  chaves estrangeiras

    Author(s)   : Vanei
    Created     : 16/01/1997
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
        
  
  
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
        
        assign c-lista-campo = string(fiCodOperador:handle in frame fPage0) + '|':U + 'cod-operador'.
        
            assign c-lista-campo = c-lista-campo + chr(10) + string(fiNomeOperador:handle in frame fPage0) + '|':U + 'nom-operador'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    

    
  end.
                    
assign l-implanta = no.



/* _UIB-CODE-BLOCK-END */



  

END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF fiEmenda1 IN FRAME fPage0 /* Emenda 1 */
DO:

    def var deQuant as dec no-undo.

    
                  c-wk-refer2 = "".

    
    if self:screen-value <> "" then do:
    
        if not can-find (first saldo-estoq use-index lote where
                               saldo-estoq.lote      = self:screen-value and
                               saldo-estoq.it-codigo = fiItem:screen-value in frame fPage0 and
                               saldo-estoq.cod-estabel = fiCodEstabel:screen-value in frame fPage0) then do:
            
            run utp/ut-msgs.p (input "show",
                               input 17006,
                               input "Lote n∆o encontrado").
    
        end.
        else do:

            for each saldo-estoq fields (qtidade-atu qt-alocada qt-aloc-ped cod-depos
                                         qt-aloc-prod cod-refer) use-index lote no-lock where
                     saldo-estoq.it-codigo   = fiItem:screen-value in frame fPage0 and
                     saldo-estoq.lote        = self:screen-value and
                     saldo-estoq.cod-estabel = fiCodEstabel:screen-value in frame fPage0 and
                     saldo-estoq.cod-depos  <> substring(param-cp.char-2,1,3) AND
                     NOT (saldo-estoq.cod-depos  = "EXP" AND  (saldo-estoq.cod-estabel = "412" OR saldo-estoq.cod-estabel = "422") ): /*solic-318*/

                IF (saldo-estoq.qtidade-atu - 
                                          saldo-estoq.qt-alocada  - 
                                          saldo-estoq.qt-aloc-ped -
                                          saldo-estoq.qt-aloc-prod) > 0 THEN
                  c-wk-refer2 =  saldo-estoq.cod-refer.


                assign deQuant = deQuant + (saldo-estoq.qtidade-atu - 
                                            saldo-estoq.qt-alocada  - 
                                            saldo-estoq.qt-aloc-ped -
                                            saldo-estoq.qt-aloc-prod).
            end.

            assign fiQtEmenda1:screen-value in frame fPage0 = string(deQuant).
        
        end.

    end.
    else
        assign fiQtEmenda1:screen-value in frame fPage0 = string(0).


    run CalculaQuantSaldo.
END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF fiEmenda2 IN FRAME fPage0 /* Emenda 2 */
DO:
  
    def var deQuant as dec no-undo.

   
                  c-wk-refer3 = "".

    
    if self:screen-value <> "" then do:
    
        if not can-find (first saldo-estoq use-index lote where
                               saldo-estoq.lote      = self:screen-value and
                               saldo-estoq.it-codigo = fiItem:screen-value in frame fPage0 and
                               saldo-estoq.cod-estabel = fiCodEstabel:screen-value in frame fPage0) then do:
            
            run utp/ut-msgs.p (input "show",
                               input 17006,
                               input "Lote n∆o encontrado").
    
        end.
        else do:

            for each saldo-estoq fields (qtidade-atu qt-alocada qt-aloc-ped cod-depos
                                         qt-aloc-prod cod-refer) use-index lote no-lock where
                     saldo-estoq.it-codigo   = fiItem:screen-value in frame fPage0 and
                     saldo-estoq.lote        = self:screen-value and
                     saldo-estoq.cod-estabel = fiCodEstabel:screen-value in frame fPage0 and
                     saldo-estoq.cod-depos  <> substring(param-cp.char-2,1,3) AND
                     NOT (saldo-estoq.cod-depos  = "EXP" AND (saldo-estoq.cod-estabel = "412" OR saldo-estoq.cod-estabel = "422")):/*solic-318*/

                IF (saldo-estoq.qtidade-atu - 
                                          saldo-estoq.qt-alocada  - 
                                          saldo-estoq.qt-aloc-ped -
                                          saldo-estoq.qt-aloc-prod) > 0 THEN
                  c-wk-refer3 = saldo-estoq.cod-refer.


                assign deQuant = deQuant + (saldo-estoq.qtidade-atu - 
                                            saldo-estoq.qt-alocada  - 
                                            saldo-estoq.qt-aloc-ped -
                                            saldo-estoq.qt-aloc-prod).
            end.

            assign fiQtEmenda2:screen-value in frame fPage0 = string(deQuant).
        
        end.

    end.
    else
        assign fiQtEmenda2:screen-value in frame fPage0 = string(0).

    run CalculaQuantSaldo.

END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF fiLote IN FRAME fPage0 /* Lote */
DO:
        
    def var deQuant as dec no-undo.
    c-wk-refer1 = "".

    if self:screen-value <> "" then do:
/* edson- 23/05/2006 - colocado para tentar primeiro pegar lote com saldo cajo haja duplicado */     
        for first saldo-estoq fields (it-codigo) use-index lote no-lock where
                  saldo-estoq.lote        = self:screen-value and
                  saldo-estoq.qtidade-atu <> 0 AND
                  saldo-estoq.cod-estabel = fiCodEstabel:screen-value in frame fPage0 AND
                     NOT (saldo-estoq.cod-depos  = "EXP" AND  (saldo-estoq.cod-estabel = "412" OR saldo-estoq.cod-estabel = "422")): end./*solic-318*/
         IF NOT AVAIL saldo-estoq THEN DO:
             FOR first saldo-estoq fields (it-codigo) use-index lote no-lock where
                  saldo-estoq.lote        = self:screen-value and
                  saldo-estoq.cod-estabel = fiCodEstabel:screen-value in frame fPage0 AND
                     NOT (saldo-estoq.cod-depos  = "EXP" AND  (saldo-estoq.cod-estabel = "412" OR saldo-estoq.cod-estabel = "422")): end./*solic-318*/
         END.
        if avail saldo-estoq then do:
        
            for first item fields (it-codigo desc-item) no-lock where
                      item.it-codigo = saldo-estoq.it-codigo: end.
            assign fiItem:screen-value      in frame fPage0 = item.it-codigo
                   fiDescricao:screen-value in frame fPage0 = item.desc-item.
            
            for each saldo-estoq fields (qtidade-atu qt-alocada qt-aloc-ped
                                         qt-aloc-prod cod-refer) use-index lote no-lock where
                     saldo-estoq.it-codigo   = item.it-codigo    and
                     saldo-estoq.lote        = self:screen-value and
                     saldo-estoq.cod-estabel = fiCodEstabel:screen-value in frame fPage0 AND
                     NOT (saldo-estoq.cod-depos  = "EXP" AND  (saldo-estoq.cod-estabel = "412" OR saldo-estoq.cod-estabel = "422")) and/*solic-318*/
                     saldo-estoq.cod-depos  <> substring(param-cp.char-2,1,3),
                FIRST deposito  FIELDS (cod-depos ind-dep-cq)
                    WHERE deposito.cod-depos = saldo-estoq.cod-depos
                      AND deposito.ind-dep-cq = NO :

                IF (saldo-estoq.qtidade-atu - 
                                            saldo-estoq.qt-alocada  - 
                                            saldo-estoq.qt-aloc-ped -
                                            saldo-estoq.qt-aloc-prod) > 0 THEN
                    c-wk-refer1 =  saldo-estoq.cod-refer.

                assign deQuant = deQuant + (saldo-estoq.qtidade-atu - 
                                            saldo-estoq.qt-alocada  - 
                                            saldo-estoq.qt-aloc-ped -
                                            saldo-estoq.qt-aloc-prod).
            end.

            assign fiQtlote:screen-value in frame fPage0  = string(deQuant).
        
        end.
        else do:
            assign fiItem:screen-value      in frame fPage0 = "*"
                   fiDescricao:screen-value in frame fPage0 = "*"
                   fiQtLote:screen-value    in frame fPage0 = string(0).
    
            run utp/ut-msgs.p (input "show",
                               input 17006,
                               input "Lote n∆o encontrado").
    
        end.

    end.
    else
        assign fiItem:screen-value      in frame fPage0 = "*"
               fiDescricao:screen-value in frame fPage0 = "*"
               fiQtLote:screen-value    in frame fPage0 = string(0).

    run CalculaQuantSaldo.

END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF fiRefugo IN FRAME fPage0 /* Refugo */
DO:



  run calculaQuantRefugo.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME fPage0
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */




ON MENU-DROP OF MENU mi-programa /* Arquivo */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  /*************************************************************************
**
** SOBRE.I - Include padrío para chamada do Sobre
** Data Cria?ío: 22/07/97
** Criado por..: Fabiano
**
**************************************************************************/


    def var c-nom-prog-ext     as char no-undo.
    def var c-nom-prog-ext-aux as char no-undo.
    
    assign c-nom-prog-ext = program-name(1).
    if c-nom-prog-ext begins "USER-INTERFACE-TRIGGER":U then
        assign c-nom-prog-ext = substr(c-nom-prog-ext,24)
               file-info:file-name = c-nom-prog-ext
               c-nom-prog-ext-aux = file-info:full-pathname.
    run btb/btb901zb.p (c-programa-mg97,
                        c-nom-prog-ext-aux,
                        c-versao-mg97).    

/* include/sobre.i */
 
END.

/* _UIB-CODE-BLOCK-END */







/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
/* windowmn.i - Main Block code for objects which create windows.*/
/* Skip all of this if no window was created. */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
IF VALID-HANDLE(w-livre) THEN DO:
    ASSIGN CURRENT-WINDOW                = w-livre 
       w-livre:KEEP-FRAME-Z-ORDER = YES
       THIS-PROCEDURE:CURRENT-WINDOW = w-livre.

    /* The CLOSE event can be used from inside or outside the procedure to  */
    /* terminate it.                                                        */
    ON CLOSE OF THIS-PROCEDURE 
       RUN dispatch IN THIS-PROCEDURE ('destroy':U).

    RUN dispatch ('create-objects':U).

/* Execute this code only if not being run PERSISTENT, i.e., if in test mode
   of one kind or another or if this is a Main Window. Otherwise postpone 
   'initialize' until told to do so. */


IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:

    /* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
    MAIN-BLOCK:
    DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
       ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
 
    /* Now enable the interface and wait for the exit condition.            */
       RUN dispatch ('initialize':U).
       
       IF RETURN-VALUE = 'ADM-ERROR':U THEN RETURN.
       
       IF NOT THIS-PROCEDURE:PERSISTENT THEN
           WAIT-FOR CLOSE OF THIS-PROCEDURE.
    END.

END.

END.

 

fiCodEstabel:load-mouse-pointer ("image/lupa.cur")  in frame fPage0.
fiCodCtrab:load-mouse-pointer ("image/lupa.cur")    in frame fPage0.
fiCodOperador:load-mouse-pointer ("image/lupa.cur") in frame fPage0.

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */


PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME fPage0:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             btSave:HANDLE IN FRAME fPage0 , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  /* row-head.i - */
  DEFINE VARIABLE tbl-list           AS CHARACTER INIT "":U NO-UNDO.
  DEFINE VARIABLE rowid-list         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE row-avail-cntr     AS INTEGER INIT 0 NO-UNDO.
  DEFINE VARIABLE row-avail-rowid    AS ROWID NO-UNDO.
  DEFINE VARIABLE row-avail-enabled  AS LOGICAL NO-UNDO.
  DEFINE VARIABLE link-handle        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE record-source-hdl  AS HANDLE NO-UNDO.
  DEFINE VARIABLE different-row      AS LOGICAL NO-UNDO INIT no.
  DEFINE VARIABLE key-name           AS CHARACTER INIT ? NO-UNDO.
  DEFINE VARIABLE key-value          AS CHARACTER INIT ? NO-UNDO.
 
  /* Check that the previous record hasn't been modifed. */
  RUN check-modified IN THIS-PROCEDURE ('check':U) NO-ERROR.
  
  /* If nothing's been modified but we're in an update, then the record
     we're getting is the same one we're just finishing up with
     (update-complete state after an Add, for instance). So ignore it. */
  IF adm-updating-record THEN RETURN.

  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = IF RETURN-VALUE = "YES":U THEN yes ELSE no.  
  
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'RECORD-SOURCE':U,
      OUTPUT link-handle) NO-ERROR.
  IF link-handle = "":U THEN     /* There's no active record source */
      RETURN.
  ASSIGN record-source-hdl = WIDGET-HANDLE(ENTRY(1,link-handle)).
  IF NUM-ENTRIES(link-handle) > 1 THEN  /* A list indicates multiple sources */
      MESSAGE "row-available in ":U THIS-PROCEDURE:FILE-NAME 
          "encountered more than one RECORD-SOURCE.":U SKIP
          "The first - ":U record-source-hdl:file-name " - will be used.":U
             VIEW-AS ALERT-BOX ERROR.
  
  /* Get the key needed by this Record-Target. */         
  RUN get-attribute ('Key-Name':U).
  key-name = RETURN-VALUE.
  IF key-name NE ? THEN DO:
    RUN send-key IN record-source-hdl (INPUT key-name, OUTPUT key-value)
      NO-ERROR.
    IF key-value NE ? THEN  /* At design time this won't succeed, so skip it. */
      RUN set-attribute-list (SUBSTITUTE ('Key-Value="&1"':U, key-value)).
  END.
 

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  /* row-end.i */
IF VALID-HANDLE (adm-object-hdl) THEN  /* If there's a Frame, etc. then */
    RUN dispatch IN THIS-PROCEDURE ('display-fields':U). /* display the fields*/
/* Note: open-query does its own notify of row-available */
RUN notify IN THIS-PROCEDURE ('row-available':U).


 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE atualizaLoteItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var l-dureza as logical no-undo.


find first lote-item
     where lote-item.it-codigo = ord-prod.it-codigo
       and lote-item.lote      = tt-lote-reporte.lote /*movto-mat.lote*/ exclusive-lock no-error.
if avail lote-item then do:
    /*Etiqueta da OP reportada*/
    assign lote-item.dt-vali-lote = tt-lote-reporte.dt-vali-lote.

    create tt-digita.
    assign tt-digita.nr-ord-produ = tt-rep-oper-ctrab.nr-ord-produ
          tt-digita.cod-estabel  = ""
          tt-digita.nr-linha     = 0
          tt-digita.rw-lote-item = rowid(lote-item)
          tt-digita.arquivo      = "etq" + string(tt-rep-oper-ctrab.nr-ord-produ) + "_" + string(iSeqEtq) + ".lst".
    
    assign grw-lote-item  = rowid(lote-item)
           gc-estado      = "TravaBarra":U.
    
    run pi-finalizar in h-acomp.
    
    assign w-livre:sensitive = no.
    run sfc/essf0013.w.

        l-dureza = no.
    
    find first lote-carac-tec WHERE 
       lote-carac-tec.it-codigo = lote-item.it-codigo  AND 
       lote-carac-tec.lote = lote-item.lote AND
       lote-carac-tec.cd-comp = "DUREZAE" no-lock no-error.
    
    if not avail  lote-carac-tec then 
         l-dureza = yes.

         
         
    repeat:
        if l-dureza = yes then leave.
        
        l-dureza = yes.
        
        FOR EACH lote-carac-tec WHERE 
            lote-carac-tec.it-codigo = lote-item.it-codigo  AND 
            lote-carac-tec.lote = lote-item.lote AND
            SUBSTRING(lote-carac-tec.cd-comp,1,6) = "DUREZA" no-lock.
            
            
            IF lote-carac-tec.cd-comp = "DUREZA" THEN NEXT.
            
            if lote-carac-tec.vl-result = 0 then 
                     l-dureza = no.
  
        end.
        
        if l-dureza = no then DO:
                    run utp/ut-msgs.p (input "show", input 17006, input "POR FAVOR PREENCHA OS CAMPOS DE DUREZAS " ).

             run sfc/essf0013.w.
        END.             
        
   end.
  




    assign w-livre:sensitive = yes.
    
    run utp/ut-acomp.p persistent set h-acomp.
    run pi-inicializar in h-acomp (input "Reportando...").

end. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE calculaQuant :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    def buffer b-tt-reporta for tt-reporta.
    
    def var deQtReporte as dec no-undo.
    def var deSaldo     as dec no-undo.
    DEFINE VARIABLE l-vai-baixar AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE d-peso-uma AS DECIMAL    NO-UNDO.
    
    for each b-tt-reporta:
        assign 
            deQtReporte = deQtReporte + b-tt-reporta.qt-reporte
            d-peso-uma  = b-tt-reporta.qt-reporte.
    end.

    assign deSAldo = input frame fPage0 fiQtLote + 
                     input frame fPage0 fiQtEmenda1 + 
                     input frame fPage0 fiQtEmenda2.
    
    assign fiSaldoBobina = deSaldo - deQtReporte /*Lotes - Reporte*/
           fiQtTotal     = deQtReporte           /*Reporte*/                  
           fiRefugo      = deSaldo - fiSaldoBobina - fiQtTotal.
        
    
     /*
    
    IF  fiSaldoBobina > 0 and fiQtTotal >  0 /*AND (fiSaldoBobina < 50 OR  /*baixa menos de 50 quilos*/
        fiSaldoBobina < d-peso-uma OR /* baixa quando saldo < que o peso de uma bobina*/
        ((fiSaldoBobina / fiQtTotal) * 100 ) < 50)*/   /* saldo < 50% do total prod*/
        THEN DO:


       run utp/ut-msgs.p (input "show",
                          input 701,
                          INPUT "Geraá∆o refugo autom†tico (Saldo:" + STRING(fiSaldoBobina) + ")" ).

     IF RETURN-VALUE = "yes" THEN
        assign fiRefugo      = fiSaldoBobina
               fiQtTotal     = deQtReporte + fiSaldoBobina
               fiSaldoBobina = 0.
         


    END.
*/
  
    disp  fiSaldoBobina
          fiQtTotal    
          fiRefugo     
        with frame fPage0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

PROCEDURE calculaQuantbaixaAut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    def buffer b-tt-reporta for tt-reporta.
    
    def var deQtReporte as dec no-undo.
    def var deSaldo     as dec no-undo.
    DEFINE VARIABLE l-vai-baixar AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE d-peso-uma AS DECIMAL    NO-UNDO.
    
    for each b-tt-reporta:
        assign 
            deQtReporte = deQtReporte + b-tt-reporta.qt-reporte
            d-peso-uma  = b-tt-reporta.qt-reporte.
    end.

    assign deSAldo = input frame fPage0 fiQtLote + 
                     input frame fPage0 fiQtEmenda1 + 
                     input frame fPage0 fiQtEmenda2.
    
    assign fiSaldoBobina = deSaldo - deQtReporte /*Lotes - Reporte*/
           fiQtTotal     = deQtReporte           /*Reporte*/                  
           fiRefugo      = deSaldo - fiSaldoBobina - fiQtTotal.
        
    

    
    IF  fiSaldoBobina > 0 and fiQtTotal >  0 AND (fiSaldoBobina < 500 OR  /*baixa menos de 500 quilos*/
        fiSaldoBobina < d-peso-uma OR /* baixa quando saldo < que o peso de uma bobina*/
        ((fiSaldoBobina / fiQtTotal) * 100 ) < 50)   /* saldo < 50% do total prod*/
        THEN DO:


       run utp/ut-msgs.p (input "show",
                          input 701,
                          INPUT "Geraá∆o refugo autom†tico (Saldo:" + STRING(fiSaldoBobina) + ")" ).

     IF RETURN-VALUE = "yes" THEN
        assign fiRefugo      = fiSaldoBobina
               fiQtTotal     = deQtReporte + fiSaldoBobina
               fiSaldoBobina = 0.
         


    END.

  
    disp  fiSaldoBobina
          fiQtTotal    
          fiRefugo     
        with frame fPage0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */





PROCEDURE calculaQuantRefugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def buffer b-tt-reporta for tt-reporta.
    
    def var deQtReporte as dec no-undo.
    def var deSaldo     as dec no-undo.
    DEFINE VARIABLE d-peso-uma AS DECIMAL    NO-UNDO.
    
    for each b-tt-reporta:
        assign deQtReporte = deQtReporte + b-tt-reporta.qt-reporte
               d-peso-uma  = b-tt-reporta.qt-reporte.
    end.

    assign deSAldo = input frame fPage0 fiQtLote + 
                     input frame fPage0 fiQtEmenda1 + 
                     input frame fPage0 fiQtEmenda2.

    fiRefugo = input frame fPage0 fiRefugo.
    
    assign fiSaldoBobina = deSaldo - deQtReporte - input frame fPage0 fiRefugo /*Lotes - Reporte*/
           fiQtTotal     = deQtReporte + fiRefugo                     /*Reporte*/.
/*
 IF  fiSaldoBobina > 0 and fiQtTotal > 0 /*AND (fiSaldoBobina < 50 OR  /*baixa menos de 50 quilos*/
        fiSaldoBobina < d-peso-uma OR /* baixa quando saldo < que o peso de uma bobina*/
        ((fiSaldoBobina / fiQtTotal) * 100 ) < 50)*/   /* saldo < 50% do total prod*/
        THEN DO:


       run utp/ut-msgs.p (input "show",
                          input 701,
                          INPUT "Geraá∆o refugo automatico (Saldo:" + STRING(fiSaldoBobina) + ")" ).

     IF RETURN-VALUE = "yes" THEN
        assign fiRefugo      = fiSaldoBobina + input frame fPage0 fiRefugo
               fiSaldoBobina = 0
               fiQtTotal     = deQtReporte + fiRefugo  .    
  END.
  */

    disp  fiSaldoBobina
          fiRefugo
          fiQtTotal    
        with frame fPage0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE CalculaQuantSaldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

assign fiSaldoBobina = input frame fPage0 fiQtLote +
                       input frame fPage0 fiQtEmenda1 +
                       input frame fPage0 fiQtEmenda2.

disp fiSaldoBobina with frame fPage0.
RUN calculaQuantRefugo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



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
       tt-rep-oper-ctrab.dat-fim-reporte       = input frame fPage0 fiDtTermino
       tt-rep-oper-ctrab.dat-inic-reporte      = input frame fPage0 fiDtInicio
       tt-rep-oper-ctrab.qtd-operac-refgda     = 0
       tt-rep-oper-ctrab.qtd-operac-aprov      = deQuantNec
       tt-rep-oper-ctrab.qtd-operac-reptda     = deQuantNec
       tt-rep-oper-ctrab.qtd-operac-retrab     = 0
       tt-rep-oper-ctrab.qtd-segs-fim-reporte  = deSegFim
       tt-rep-oper-ctrab.qtd-segs-inic-reporte = deSegIni
       tt-rep-oper-ctrab.cod-equipe            = input frame fPage0 fiCodOperador
       tt-rep-oper-ctrab.num-contador-inic     = 0
       tt-rep-oper-ctrab.num-contador-fim      = 0
       /*** ParÉmetros p/ reporte ***/
       tt-rep-oper-ctrab.nr-ord-produ          = split-operac.nr-ord-produ
       tt-rep-oper-ctrab.num-seq-rep           = if avail rep-oper-ctrab then rep-oper-ctrab.num-seq-rep + 1
                                                                         else 1
       tt-rep-oper-ctrab.num-operac-sfc        = split-operac.num-operac-sfc
       tt-rep-oper-ctrab.num-split-oper        = split-operac.num-split-oper
       tt-rep-oper-ctrab.cod-ctrab             = input frame fPage0 fiCodCtrab.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



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
       tt-rep-prod.data                  = input frame fPage0 fiDtTermino
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
       tt-rep-prod.prog-seg              = "essf0002"
       tt-rep-prod.cod-versao-integracao = 001.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



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

/* _UIB-CODE-BLOCK-END */



PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiCodEstabel fiNomeEstabel fiCodCtrab fiDesCtrab fiCodOperador 
          fiNomeOperador fiDtInicio fiHrInicio fiDtTermino fiHrTermino fiLote 
          fiQtLote fiItem fiEmenda1 fiQtEmenda1 fiDescricao fiEmenda2 
          fiQtEmenda2 fiSaldoBobina fiRefugo fiQtTotal 
      WITH FRAME fPage0 IN WINDOW w-livre.
  ENABLE btAdd RECT-12 RECT-13 RECT-14 rt-button 
      WITH FRAME fPage0 IN WINDOW w-livre.
  open query brReporte     for each tt-reporta .
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE gera-mob-ggf-automatico :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var d-tempo as decimal no-undo.

  find first param-sfc no-lock no-error.
  IF NOT AVAIL param-sfc THEN return "NOK":U.

  /* Calcula tempo reporte */
  RUN pi-calcula-tempo-mob-ggf (OUTPUT d-tempo).

  /* Gera GGF autom†tico - Carrega automaticamente o browser MOB/GGF do SF0303B 
   com os movimentos de GGF da operaá∆o corrente */

IF param-sfc.log-1 and
   ord-prod.reporte-ggf = 1 THEN DO:  
   
   CREATE tt-apont-mob.
   assign tt-apont-mob.nr-ord-prod     =  split-operac.nr-ord-produ
          tt-apont-mob.op-codigo       =  split-operac.op-codigo
          tt-apont-mob.it-codigo       =  split-operac.it-codigo
          tt-apont-mob.cod-roteiro     =  split-operac.cod-roteiro
          tt-apont-mob.tipo-movto      =  2
          tt-apont-mob.gm-codigo       =  split-operac.gm-codigo
          tt-apont-mob.hora-fim        =  int(entry(1,fiHrTermino:SCREEN-VALUE IN FRAME fPage0,":"))            
          tt-apont-mob.hora-ini        =  int(entry(1,fiHrInicio:SCREEN-VALUE IN FRAME fPage0,":"))  
           tt-apont-mob.min-fim         =  int(entry(2,fiHrTermino:SCREEN-VALUE IN FRAME fPage0,":"))            
          tt-apont-mob.min-ini         =  int(entry(2,fiHrInicio:SCREEN-VALUE IN FRAME fPage0,":"))
          tt-apont-mob.tempo           =  d-tempo
          tt-apont-mob.referencia      =  ord-prod.cod-refer.
   /* Rel¢gio Hexadecimal ou centesimal */                                                   

   IF INT(param-sfc.log-tipo-relogio) = 1 THEN
      ASSIGN tt-apont-mob.tipo-relogio = 1.
   ELSE 
      ASSIGN tt-apont-mob.tipo-relogio = 3. 
   
END.

/* Gera MOB autom†tico - Carrega automaticamente o browser MOB/GGF do SF0303B 
   com os movimentos de MOB da operaá∆o corrente */

IF param-sfc.log-2 and
   ord-prod.reporte-mob = 1  THEN DO:
   
    CREATE tt-apont-mob.
    assign tt-apont-mob.nr-ord-prod     =  split-operac.nr-ord-produ
           tt-apont-mob.op-codigo       =  split-operac.op-codigo
           tt-apont-mob.it-codigo       =  split-operac.it-codigo
           tt-apont-mob.cod-roteiro     =  split-operac.cod-roteiro
           tt-apont-mob.tipo-movto      =  1
           tt-apont-mob.cd-mob-dir      =  ""
           tt-apont-mob.gm-codigo       =  split-operac.gm-codigo
           tt-apont-mob.hora-fim        =  int(entry(1,fiHrTermino:SCREEN-VALUE IN FRAME fPage0,":"))            
           tt-apont-mob.hora-ini        =  int(entry(1,fiHrInicio:SCREEN-VALUE IN FRAME fPage0,":"))  
           tt-apont-mob.min-fim         =  int(entry(2,fiHrTermino:SCREEN-VALUE IN FRAME fPage0,":"))            
           tt-apont-mob.min-ini         =  int(entry(2,fiHrInicio:SCREEN-VALUE IN FRAME fPage0,":"))
           tt-apont-mob.tempo           =  d-tempo 
           tt-apont-mob.referencia      =  ord-prod.cod-refer. 

    

    /* Rel¢gio Hexadecimal ou centesimal */                                                   
    IF INT(param-sfc.log-tipo-relogio) = 1 THEN
       ASSIGN tt-apont-mob.tipo-relogio = 1.
    ELSE 
       ASSIGN tt-apont-mob.tipo-relogio = 3. 
END.
    
RETURN "OK":U.

              
              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  /*************************************************************************
**
** I-LOGFIN.I - Encerra o Log de Execu?ío
**
**************************************************************************/

/*************************************************************************
**
** I-LOGFIN1.I - Encerra o Log de Execucao
**
**************************************************************************/

run btb/btb918zb.p (input c-programa-mg97,
                    input-output rw-log-exec,
                    input no).
 

/* Transformacao Window */

if session:window-system <> "TTY":U then do:
    case i-template:
        when 9 or when 10 or when 20 or when 30 or when 31 then do: 
            assign h-pai:sensitive = yes.
            apply "ENTRY":U to h-pai.
        end.
        when 13 then do:
            assign h-pai:sensitive = yes.
            apply "ENTRY":U to h-pai.
            run pi-entry-atributos-chave.
        end.
    end case.
end.  

/* Transformacao Window */
/* Elimina?ío de arquivos temporˇrios */


/* Fim da elimina?ío de arquivos temporˇrios */

/* i-logfin */
 

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  /***********************************************************************
**
**  WIN-SIZE.I - Realiza o ajuste no tamanho da window e da frame
**               igualando ambos
*************************************************************************/

if w-livre:width-chars < frame fPage0:width-chars then
    assign frame fPage0:width-chars = w-livre:width-chars.
else if frame fPage0:width-chars < w-livre:width-chars then
    assign w-livre:width-chars = frame fPage0:width-chars.

if w-livre:height-chars < frame fPage0:height-chars then
    assign frame fPage0:height-chars = w-livre:height-chars.
else if frame fPage0:height-chars < w-livre:height-chars then
    assign w-livre:height-chars = frame fPage0:height-chars.

assign w-livre:virtual-width-chars  = w-livre:width-chars
       w-livre:virtual-height-chars = w-livre:height-chars
       w-livre:min-width-chars      = w-livre:width-chars
       w-livre:max-width-chars      = w-livre:width-chars
       w-livre:min-height-chars     = w-livre:height-chars
       w-livre:max-height-chars     = w-livre:height-chars.

/* win-size.i */
 

  /***********************************************************************
**  /*   */
**  UT9000.I - Definiá∆o das vari†veis de ambiente do Magnus 97
**  {1} = programa provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/

/* System Variable Definitions ---                                       */
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

/********************************************************************************
** Programa : include/i-sysvar.i
**
** Data : 02/06/1999
**
** Criaá∆o : John Cleber Jaraceski
**
** Objetivo : Definicao das System Variables
**
** Ultima Alt : ?
*******************************************************************************/



/* include/i-sysvar.i ---                                                     */

 
def var c_cod_empres_usuar as char no-undo.
def var c_nom_razao_social as char no-undo.



    /*rodar pi-rsocial persistent para verificaá∆o empresa usuario*/
    if not valid-handle(h-rsocial)
    or h-rsocial:type <> "procedure":U
    or h-rsocial:file-name <> "utp/ut-rsocial.p":U then do:
        if l-achou-prog then
            run utp/ut-rsocial.p persistent set h-rsocial.
    end.
    if l-achou-prog then
        run pi-rsocial in h-rsocial (output c_cod_empres_usuar, output c_nom_razao_social).

    assign c-programa-mg97 = caps("essf0002")
           c-versao-mg97   = "2.00.00.000".
    find prog_dtsul no-lock
        where prog_dtsul.cod_prog_dtsul = c-programa-mg97 no-error.
    if  avail prog_dtsul then do:
        assign c-titulo-prog-mg97    = prog_dtsul.des_prog_dtsul
               c-nom-prog-upc-mg97   = prog_dtsul.nom_prog_upc
               c-nom-prog-appc-mg97  = prog_dtsul.nom_prog_appc
               c-nom-prog-dpc-mg97   = prog_dtsul.nom_prog_dpc
               i-num-topico-hlp-mg97 = prog_dtsul.num_topico.
       
     

        
        find procedimento no-lock
            where procedimento.cod_proced = prog_dtsul.cod_proced no-error.
        if  avail procedimento then do:
            find modul_dtsul no-lock
                where modul_dtsul.cod_modul_dtsul = procedimento.cod_modul_dtsul no-error. 
            if  avail modul_dtsul then do:
                assign c-modulo-mg97         = caps(modul_dtsul.nom_modul_dtsul_menu)
                       c-cod-mod-mg97        = caps(modul_dtsul.cod_modul_dtsul)
                       c-nom-manual-hlp-mg97 = "dochlp~/" + string(modul_dtsul.num_manual_documen, "999999") + ".hlp".
            end.
        end.
    end.                                                      
    else do:
        assign c-titulo-prog-mg97    = caps(c-programa-mg97)
               c-nom-prog-upc-mg97   = ""
               c-nom-prog-appc-mg97  = ""
               i-num-topico-hlp-mg97 = 0
               c-nom-manual-hlp-mg97 = "dochlp~/000000.hlp".
    end.                 
     
    
         assign w-livre:title = if l-achou-prog then
                                       c-titulo-prog-mg97
                                     + " - " 
                                     + c-programa-mg97 
                                     + " - " 
                                     + c-versao-mg97  
                                     + " - " 
                                     + c_cod_empres_usuar
                                     + " - " 
                                     + c_nom_razao_social
                                     else 
                                       c-titulo-prog-mg97
                                     + " - " 
                                     + c-programa-mg97 
                                     + " - " 
                                     + c-versao-mg97.
    
    
 if today > 03/01/1998 then do:    
 /******************************* Validaá∆o ***********************************/   

    /* Verificaá∆o do registro do produto */
    if c-programa-mg97 <> "UT-CONF" then do:
      run utp/ut-vfreg.p (output l-acesso-livre).
      if l-acesso-livre = no then do:
        run utp/ut-msgs.p (input "show",
                           input 8525,
                           input "").      
        apply "close" to this-procedure.
        return.
      end.    
    end.  

    /* Verificaá∆o da data de validade do contrato */
    if c-programa-mg97 <> "UT-CONF" then do:
      run utp/ut-vfvld.p (output d-data-contrato).
      if d-data-contrato < today then do:
        run utp/ut-msgs.p (input "show",
                           input 8536,
                           input string(d-data-contrato)).      
        apply "close" to this-procedure.
        return.
      end.
    end.  

    /* Verificaá∆o do acesso ao modulo do programa com base no contrato */
    if c-programa-mg97 <> "UT-CONF" then do:
      run utp/ut-vfmod.p (input c-cod-mod-mg97, output l-acesso-livre).
      if l-acesso-livre = no then do:
        run utp/ut-msgs.p (input "show",
                           input 8527,
                           input c-cod-mod-mg97).      
        apply "close" to this-procedure.
        return.
      end.  
    end.  
    
    /* Verificaá∆o de usu†rios ativos */
    if c-programa-mg97 <> "UT-CONF" then do:
      run utp/ut-vfusr.p (output i-user-conectados, output i-licenca-usuar).
      if i-user-conectados > i-licenca-usuar then do:
        run utp/ut-msgs.p (input "show",
                           input 8532,
                           input string(i-user-conectados) + "~~" +
                                 string(i-licenca-usuar)).      
        apply "close" to this-procedure.
        return.
      end.
    end.  

/******************************************************************************/
 end.
    
    /* Verificaá∆o da seguranáa e login informado */
    /*--------------------------------------------------------------------------
    File        : UTP/UT-VFSEC.I
    Purpose     : Verificaá∆o da Seguranáa

    Syntax      :

    Description : Verificar a seguranáa

    Author(s)   : Fabiano
    Created     : 31/12/1997
    Notes       :
------------------------------------------------------------------------*/
/* N∆o faz a validaá∆o para programas do tipo V† Para */
if index(replace(program-name(1),"~\","~/"),"go/g") = 0 then do:
  run men/men901za.p (input c-programa-mg97).
  if  return-value = "2012" then do:
      run utp/ut-msgs.p (input "show",
                         input 2858,
                         input c-programa-mg97).
      if "w-livre" = "SmartDialog" or this-procedure:persistent = no then
        return "adm-error".
      else do:     
        delete procedure this-procedure.
        return.
      end.  
  end.                       

  if  return-value = "2014" then do:
      run utp/ut-msgs.p (input "show",
                         input 3045,
                         input c-programa-mg97).
      if "w-livre" = "SmartDialog" then
        return "adm-error".
      else do:
        delete procedure this-procedure.
        return.
      end.  
  end.
end.  

/* ut-vfsec.i */
 
    
    /* Inicio do log de execuá∆o de programas */
    /*************************************************************************
**                                                                        
**  I-LOGINI.I - Inicia Log de Execuá∆o
**
*************************************************************************/

run btb/btb918zb.p (input c-programa-mg97,
                    input-output rw-log-exec,
                    input yes).

/* i-logini */

  
    
    
      if session:window-system <> "TTY" then do:
       /********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

/********************************************************************************
** Programa : include/i-trswin.i
**
** Data : 29/12/97
**
** Criaá∆o : John Cleber Jaraceski
**
** Objetivo : Realizar alteracoes em todos os programas que possuam interface 
**            com o usuario (window/dialog). 
**            Estas alteracoes sao :
**              - Centralizar Window
**              - Desabilitar MAX - RESIZE
**              - Ocultar MAX - MIN
**              - Tornar uma Window Modal
**
** Ultima Alt : 29/12/1997
*******************************************************************************/

/* Transformacao Window *****************************************************/

    case i-template:
        when 2 then do: /* Cadastro Simples */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.
        end.
        when 3 then do: /* Cadastro Simples - Alteracao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.
        end.
        when 4 then do: /* Cadastro Complexo */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 5 then do: /* Cadastro Complexo - Alteracao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 6 then do: /* Cadastro Simples - Inclusao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 7 then do: /* Cadastro PaiXFilho - Atualiza Filho */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 8 then do: /* Cadastro PaiXFilho - Atualiza Ambos */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
            
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 9 then do: /* Inclui/Modifica Pai */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 10 then do: /* Inclui/Modifica Filho */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 11 then do: /* Formacao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 12 then do: /* Parametros Unicos */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 13 then do: /* Pesquisa */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 14 then do: /* Consulta Simples */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 15 then do: /* Consulta Complexa */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 16 then do: /* Consulta Relacionamento */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 17 then do: /* Relatorio */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 20 then do: /* Janela Detalhe */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD.

            if  h-pai:handle = w-livre:handle then
                assign h-pai           = h-pai:NEXT-SIBLING
                       h-pai:SENSITIVE = no.
            else 
                assign  h-pai = w-livre:handle
                        h-pai = h-pai:parent.

            h-pai:sensitive = no.
  
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 21 then do: /* Janela Mestre */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.        
        end.
        when 26 then do: /* Importacao/Exportacao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.
        end.
        when 29 then do: /* Relatorio Gerado Pelo DataViewer */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.
        end.
        when 30 then do: /* Formacao Sem Navegacao */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.
        end.
        when 31 then do: /* Estrutura */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.
        end.
        when 32 then do: /* Digitaá∆o Rapida */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-livre:hWnd).
            delete procedure h-prog.
            
            assign w-livre:HIDDEN = yes
            w-livre:HIDDEN = no.
            apply "ENTRY":U to w-livre.
        end.
    end case.

/* Transformacao Window *****************************************************/

 
      end. 
    



/* ut9000.i */
 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  enable brReporte with frame fPage0.

  /* Code placed here will execute AFTER standard behavior.    */
  find first param-cp no-lock no-error.

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



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
          run pi-formatted-time-to-sec (input (input frame fPage0 fiHrInicio),
                                        output de-qtd-segs-inic-rep).
          run pi-formatted-time-to-sec (input  (input frame fPage0 fiHrTermino),
                                        output de-qtd-segs-fim-rep).
      end.
      else
          assign de-qtd-segs-inic-rep = integer(input frame fPage0 fiHrInicio) * 36
                 de-qtd-segs-fim-rep  = integer(input frame fPage0 fiHrTermino)  * 36.

      if  input frame fPage0 fiDtInicio  <> ?
      and input frame fPage0 fiDtTermino <> ? then do:
          assign de-qtd-tempo-util = ?
                 da-ini-aux = date(string(input frame fPage0 fiDtInicio,"99/99/9999"))
                 da-fim-aux = date(string(input frame fPage0 fiDtTermino,"99/99/9999")).
             

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

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-cria-pallet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER i-nr-ped     LIKE ped-venda.nr-pedido.
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
      AND pallet.cod-estab = ord-prod.cod-estabel
      AND pallet.it-codigo = tt-reporta.it-codigo
      AND ENTRY(1,pallet.nr-pallet,"/") = c-nr-pallet NO-LOCK :

    ASSIGN i-seq-ult-pallet = DEC(entry(2,pallet.nr-pallet,"/")). 

    IF  pallet.nr-bobina < de-qt-palete 
    AND pallet.situacao = 1 THEN DO:
        ASSIGN i-seq-pallet = DEC(entry(2,pallet.nr-pallet,"/")).
        LEAVE.
    END.
END.
IF i-seq-pallet = 0 THEN DO:
    IF i-seq-ult-pallet > 0 THEN
        ASSIGN i-seq-ult-pallet = i-seq-ult-pallet + 1
               c-nr-pallet = c-nr-pallet + "/" + STRING(i-seq-ult-pallet,"99").
    ELSE 
        ASSIGN c-nr-pallet = c-nr-pallet + "/01".
    
    CREATE pallet.
    ASSIGN pallet.cod-estabel    = ord-prod.cod-estabel
           pallet.it-codigo      = tt-reporta.it-codigo
           pallet.nr-pallet      = c-nr-pallet
           pallet.cod-operador   = input frame fPage0 fiCodOperador 
           pallet.cod-refer      = ord-prod.cod-refer 
           pallet.data-pallet    = TODAY 
           pallet.nr-bobina      = 0
           pallet.situacao       = 1.
    
    IF SUBSTRING(c-nr-pallet,1,1) <> "X" THEN DO:
        
        ASSIGN pallet.nr-pedido    = i-nr-ped /*int(SUBSTRING(c-nr-pallet,2,6))*/
               pallet.nr-sequencia = ord-prod.nr-sequencia. 
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

/* _UIB-CODE-BLOCK-END */



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

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-gera-pallet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF var de-qt-palete AS DECIMAL NO-UNDO.
DEF VAR i-nr-ped     LIKE ped-venda.nr-pedido NO-UNDO.
DEF VAR c-nr-pallet  LIKE pallet.nr-pallet NO-UNDO.
DEF VAR r-pallet     AS ROWID NO-UNDO.
DEF VAR c-pallet-criado AS CHAR NO-UNDO.                                   
FIND FIRST pol-param-estab
    WHERE pol-param-estab.cod-estabel = input frame fPage0 fiCodEstabel NO-LOCK NO-ERROR.

for each tt-reporta
    WHERE tt-reporta.gera-pallet = "        *":
    
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
                       i-nr-ped    = ped-venda.nr-pedido.
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
                           INPUT c-nr-pallet,
                           INPUT de-qt-palete,
                           OUTPUT r-pallet).
    
        FIND FIRST pallet 
            WHERE ROWID(pallet) = r-pallet NO-LOCK.
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
        
    end.
end.
FOR EACH tt-pallet:
    /*Informar† os pallets criados ou atualizados na transaá∆o*/
    IF tt-pallet.situacao = 1 THEN DO:
        run criaErro (15825,"Criado o Pallet " + tt-pallet.nr-pallet + " para a OP " + string(tt-pallet.nr-ord-produ) + "!"). 
    END.
    ELSE DO:
        run criaErro (15825,"Atualizado o Pallet " + tt-pallet.nr-pallet + " para a OP " + string(tt-pallet.nr-ord-produ) + "!"). 
    END.
END.
if temp-table tt-erro:has-records then do:
   run cdp/cd0666.w (input table tt-erro).
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



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
        assign deTotal = deTotal + tt-lote-reporte.quantidade.
        IF tt-reporta.cod-depos = "cq" THEN DO:

                OUTPUT TO m:\dts\log_prd\essf0002.cq APPEND.
                
       

                    put "Prog: essf0002-ANTES" FORMAT "x(19)" string(TODAY) "-" 
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
assign deRefUni  = input frame fPage0 fiRefugo / deTotal
       deLote    = input frame fPage0 fiQtLote
       deEmenda1 = input frame fPage0 fiQtEmenda1
       deEmenda2 = input frame fPage0 fiQtEmenda2.
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
    /*
    /* Speto - 22/01/2004 */
    if not can-find(first tt-lote-reporte) then do:
       assign deTotOrdens = 0
              deTotRefugo = input frame fPage0 fiRefugo.
       for each tt-reporta:
           find first ord-prod no-lock where
                      ord-prod.nr-ord-produ = tt-reporta.nr-ord-produ no-error.
           assign deTotOrdens = deTotOrdens + (ord-prod.qt-ordem - ord-prod.qt-produzida).
       end.
       if deTotOrdens > input frame fPage0 fiRefugo then do:
          run utp/ut-msgs.p (input "show",
                             input  17006,
                             input "Quantidade de refugo dispon°vel nas OPÔs Ç maior que a quantidade de refugo indicada!").
          assign lErro = true.
          undo reportes, leave reportes.
       end.
    end.
    /* Speto - 22/01/2004 */
    */

    if lErro = false then do:
    reportes:
     for each tt-reporta
        break by tt-reporta.nr-ord-produ:

          FOR EACH  reservas WHERE
                   reservas.nr-ord-produ = tt-reporta.nr-ord-produ .
                 reservas.estado   = 2.
          END.


          
          FOR EACH  reservas WHERE
               reservas.nr-ord-produ = tt-reporta.nr-ord-produ   AND
               reservas.it-codigo    = input frame fPage0 fiItem.

              
                IF trim(reservas.cod-refer) <> "" THEN DO:
                    IF INDEX ( c-wk-refer1 + " - " + c-wk-refer2 + " - " + c-wk-refer3 , reservas.cod-refer) =  0  THEN NEXT.
                END.
              reservas.estado = 1.
              reservas.lote-serie   = "".
          END.

          FIND FIRST  reservas WHERE
               reservas.nr-ord-produ = tt-reporta.nr-ord-produ   AND
               reservas.it-codigo    = input frame fPage0 fiItem AND
               
              reservas.estado = 1 NO-LOCK NO-ERROR.

          IF NOT AVAIL reservas  THEN DO:

              FIND FIRST  reservas WHERE
               reservas.nr-ord-produ = tt-reporta.nr-ord-produ   AND
               reservas.it-codigo    = input frame fPage0 fiItem AND
               trim(reservas.cod-refer) <> "" NO-LOCK NO-ERROR.

              IF AVAIL reservas THEN DO:

                  FIND FIRST  bf-reservas WHERE
                       bf-reservas.nr-ord-produ = tt-reporta.nr-ord-produ   AND
                       bf-reservas.it-codigo    = input frame fPage0 fiItem AND
                       bf-reservas.cod-refer    = c-wk-refer1 NO-ERROR.

                  IF NOT AVAIL bf-reservas AND c-wk-refer1 <> "" THEN DO:
                      CREATE bf-reservas.
                      BUFFER-COPY reservas EXCEPT cod-refer  estado TO bf-reservas
                          ASSIGN bf-reservas.cod-refer = c-wk-refer1
                                 bf-reservas.estado = 1.

                  END.

                  FIND FIRST  bf-reservas WHERE
                       bf-reservas.nr-ord-produ = tt-reporta.nr-ord-produ   AND
                       bf-reservas.it-codigo    = input frame fPage0 fiItem AND
                       bf-reservas.cod-refer    = c-wk-refer2 NO-ERROR.

                  IF NOT AVAIL bf-reservas AND c-wk-refer2 <> "" THEN DO:
                      CREATE bf-reservas.
                      BUFFER-COPY reservas EXCEPT cod-refer  estado TO bf-reservas
                          ASSIGN bf-reservas.cod-refer = c-wk-refer2
                                 bf-reservas.estado = 1.

                  END.

                  FIND FIRST  bf-reservas WHERE
                       bf-reservas.nr-ord-produ = tt-reporta.nr-ord-produ   AND
                       bf-reservas.it-codigo    = input frame fPage0 fiItem AND
                       bf-reservas.cod-refer    = c-wk-refer3 NO-ERROR.

                  IF NOT AVAIL bf-reservas AND c-wk-refer3 <> "" THEN DO:
                      CREATE bf-reservas.
                      BUFFER-COPY reservas EXCEPT cod-refer  estado TO bf-reservas
                          ASSIGN bf-reservas.cod-refer = c-wk-refer3
                                 bf-reservas.estado = 1.

                  END.
                  

              END.

              FIND FIRST  reservas WHERE
               reservas.nr-ord-produ = tt-reporta.nr-ord-produ   AND
               reservas.it-codigo    = input frame fPage0 fiItem AND
               reservas.estado = 1 NO-LOCK NO-ERROR.

              IF NOT AVAIL reservas  THEN DO:
                   run utp/ut-msgs.p (input "show",
                             input  17006,
                             input "Verifique as reservas cadastradas na ordem e se est∆o ativas!").
                  assign lErro = true.
                  undo reportes, leave reportes.

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
        /*SPETO*/
        if not can-find(first tt-lote-reporte
                        where tt-lote-reporte.nr-ord-produ = tt-reporta.nr-ord-produ) then do:
           run reportaRefugoTotal.
           if return-value = "NOK":U then
              undo reportes, leave reportes.
           next.
        end.
        /*SPETO*/
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
                                      IF fiCodEstabel:screen-value in frame fPage0 = '' THEN item.cod-estabel
                                      ELSE fiCodEstabel:screen-value in frame fPage0 ,
                                      "contr-qualid":U)) = "yes":U THEN
               FOR FIRST estabelec FIELDS (deposito-cq) WHERE 
                         estabelec.cod-estabel = IF fiCodEstabel:screen-value in frame fPage0 = ''
                                                 THEN item.cod-estabel
                                                 ELSE fiCodEstabel:screen-value in frame fPage0  NO-LOCK:
                         assign tt-rep-prod.cod-depos = estabelec.deposito-cq.
               END.
         /*  ELSE 
               assign tt-rep-prod.cod-depos = f-item-uni-estab(item.it-codigo,
                                              IF fiCodEstabel:screen-value in frame {&frame-name} = ''
                                              THEN item.cod-estabel
                                              ELSE fiCodEstabel:screen-value in frame {&frame-name} , "deposito-pad":U).
           Passa a aceitar a modificacao do deposito no browse , o deposito inicial vem da OP*/                                              

           /*Ajusta o  deposito de CQ*/
           
            IF tt-rep-prod.cod-depos = "cq" THEN DO:

                            OUTPUT TO m:\dts\log_prd\essf0002.cq APPEND.
                            
                   
            
                                put "Prog: essf0002-durante" FORMAT "x(19)" string(TODAY) "-" 
                                     string(TIME,"hh:mm:ss") "-" 
                                     " Logado: " string(c-seg-usuario) SKIP 
                                    .
                                    
                                    put "tt-reporta:" tt-reporta.cod-depos 
                                    "- tt-rep-prod:" tt-rep-prod.cod-depos
                                         " - Ordem:" tt-reporta.nr-ord-produ
                                         " - Item report:" tt-reporta.it-codigo
                                          " - Item.it-codigo:" item.it-codigo
                                          " - tem cq:"  STRING (f-item-uni-estab(tt-reporta.it-codigo,
                                      IF fiCodEstabel:screen-value in frame fPage0 = '' THEN item.cod-estabel
                                      ELSE fiCodEstabel:screen-value in frame fPage0 ,
                                      "contr-qualid":U))
                                      " - Estab-item:" item.cod-estabel
                                         " - Qtde:" tt-reporta.qt-reporte 
                                         " - lote:" tt-rep-prod.lote
                                         " - peso:" tt-rep-prod.qt-reporte skip.
                                         
                            
                            output close.
                                     
                         
                     end.
                     
            if not last(tt-reporta.nr-ord-produ) or
               not last(tt-lote-reporte.lote) then
                assign tt-rep-prod.qt-refugo = tt-rep-prod.qt-reporte * deRefUni
                       deRefTot              = deRefTot + tt-rep-prod.qt-refugo.
            else
                assign tt-rep-prod.qt-refugo = input frame fPage0 fiRefugo - deRefTot.
    
            assign tt-rep-prod.qt-reporte = tt-rep-prod.qt-reporte + tt-rep-prod.qt-refugo
                   deQuantNec             = tt-rep-prod.qt-reporte.
    
            if tt-rep-prod.qt-refugo > 0 then do:
               create tt-refugo.
               assign tt-refugo.nr-ord-produ = tt-rep-prod.nr-ord-produ
                      tt-refugo.codigo-rejei = if ord-prod.cod-estabel = "413" OR ord-prod.cod-estabel = "423" then 2 else 1/*solic-318*/
                      tt-refugo.qt-refugo    = tt-rep-prod.qt-refugo.

               run pi-recebe-tt-refugo in hReporte (input table tt-refugo).
            end.
            run pi-recebe-tt-erro in hReporte (input table tt-erro).


            run pi-recebe-tt-rep-prod in hReporte (input table tt-rep-prod).


            /* Rotina da nova tabela de rastreabilidade
               JosÇ Roberto 29/03/2005 */

            ASSIGN nr-ord-produ-jr-pro = tt-rep-prod.nr-ord-produ 
                   cod-estabel-jr-pro  = input frame fPage0 ficodestabel
                   it-codigo-jr-pro    = tt-rep-prod.it-codigo
                   lote-jr-pro         = tt-rep-prod.lote
                   lote-jr-con1        = input frame fPage0 fiLote     
                   lote-jr-con2        = input frame fPage0 fiEmenda1 
                   lote-jr-con3        = input frame fPage0 fiEmenda2. 
                
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
            assign tt-param.it-codigo  = input frame fPage0 fiItem
                   tt-param.lote       = input frame fPage0 fiLote
                   tt-param.emenda1    = input frame fPage0 fiEmenda1
                   tt-param.emenda2    = input frame fPage0 fiEmenda2
                   tt-param.qt-lote    = deLote
                   tt-param.qt-emenda1 = deEmenda1
                   tt-param.qt-emenda2 = deEmenda2.
            
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

            run pi-formatted-time-to-sec (input  replace(fiHrInicio:screen-value in frame fPage0,":",""),
                                          output deSegIni).
            run pi-formatted-time-to-sec (input  replace(fiHrTermino:screen-value in frame fPage0,":",""),
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
            if avail tt-rep-oper-ctrab then do:
               find first rep-oper-ctrab
                    where rep-oper-ctrab.nr-ord-produ = tt-rep-oper-ctrab.nr-ord-produ
                      and rep-oper-ctrab.num-seq-rep  = tt-rep-oper-ctrab.num-seq-rep no-lock no-error.
               find last movto-mat
                   where movto-mat.nr-reporte = rep-oper-ctrab.nr-reporte no-lock no-error.
               if avail movto-mat then do:
                  find item
                      where item.it-codigo = movto-mat.it-codigo no-lock no-error.
                  if avail item and
                     item.cd-folh-lote <> "" then do:
                     run atualizaLoteItem.
                  end.
               end.
            end.
        end.


          FOR EACH  reservas WHERE
                   reservas.nr-ord-produ = tt-reporta.nr-ord-produ .
                 reservas.estado   = 1.
          END.
     END.
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
/*           end.*/
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
    end.


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

if can-find(first tt-lote-reporte) then do:
   run utp/ut-msgs.p (input "show",
                      input 27100,
                      input "Deseja imprimir a(s) Etiqueta(s)?").
   if return-value = "yes" then do:
      run pi-etiqueta.
   end.
end.

return 'ok'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE reportaRefugoTotal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER b-ord-rep FOR ord-prod.
    /* criado um buffere de leitura de orden para tentar resolver o erro de troca de referencia durante reporte */
    FIND FIRST b-ord-rep where b-ord-rep.nr-ord-produ = tt-reporta.nr-ord-produ.

            find first param-cp no-lock no-error.
            
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
    
            assign lErro = false.
            
            /*assign deTotRefugo = input frame fPage0 fiRefugo.*/

            create tt-rep-prod.
            assign tt-rep-prod.tipo                  = 1
                   tt-rep-prod.nr-ord-produ          = tt-reporta.nr-ord-produ
                   tt-rep-prod.it-codigo             = ord-prod.it-codigo
                   tt-rep-prod.un                    = item.un
                   tt-rep-prod.ct-codigo             = ord-prod.ct-codigo
                   tt-rep-prod.sc-codigo             = ord-prod.sc-codigo 
                   tt-rep-prod.data                  = input frame fPage0 fiDtTermino
                   
                   tt-rep-prod.qt-reporte            = input frame fPage0 fiRefugo /*(ord-prod.qt-ordem - ord-prod.qt-produzida)*/
                   tt-rep-prod.cod-refer             = IF AVAIL b-ord-rep THEN b-ord-rep.cod-refer ELSE ord-prod.cod-refer
                    

                   tt-rep-prod.cod-depos-sai         = ?
                   tt-rep-prod.cod-local-sai         = ?  

                   tt-rep-prod.cod-depos             = tt-reporta.cod-depos
                   tt-rep-prod.cod-localiz           = tt-reporta.cod-localiz
                   
                   tt-rep-prod.dep-refugo            = substring(param-cp.char-2,1,3)
                   tt-rep-prod.loc-refugo            = cInformCompl

                   tt-rep-prod.lote-serie            = "RECICL":U /*Antes era o fiLote*/
                   tt-rep-prod.dt-vali-lote          = 12/31/9999
                   tt-rep-prod.procura-saldos        = false
                   tt-rep-prod.carrega-reservas      = false
                   tt-rep-prod.reserva               = false
                   tt-rep-prod.prog-seg              = "essf0002"
                   tt-rep-prod.cod-versao-integracao = 001
                   deQuantNec                        = tt-rep-prod.qt-reporte
                   /*deTotOrdens                       = deTotOrdens - (ord-prod.qt-ordem - ord-prod.qt-produzida)*/ .

            /*Ajusta o  deposito de CQ*/
            IF param-global.modulo-cq and 
               param-cq.tipo-cq       > 1 and
               STRING (f-item-uni-estab(item.it-codigo,
                                       IF fiCodEstabel:screen-value in frame fPage0 = '' THEN item.cod-estabel
                                       ELSE fiCodEstabel:screen-value in frame fPage0 ,
                                       "contr-qualid":U)) = "yes":U THEN
                FOR FIRST estabelec FIELDS (deposito-cq) WHERE 
                          estabelec.cod-estabel = IF fiCodEstabel:screen-value in frame fPage0 = ''
                                                  THEN item.cod-estabel
                                                  ELSE fiCodEstabel:screen-value in frame fPage0  NO-LOCK:
                          assign tt-rep-prod.cod-depos = estabelec.deposito-cq.
                END.
            ELSE 
                assign tt-rep-prod.cod-depos = f-item-uni-estab(item.it-codigo,
                                               IF fiCodEstabel:screen-value in frame fPage0 = ''
                                               THEN item.cod-estabel
                                              ELSE fiCodEstabel:screen-value in frame fPage0 , "deposito-pad":U).
             
            /*Ajusta o  deposito de CQ*/

            assign tt-rep-prod.qt-refugo  = tt-rep-prod.qt-reporte.
    
            if tt-rep-prod.qt-refugo > 0 then do:
               create tt-refugo.
               assign tt-refugo.nr-ord-produ = tt-rep-prod.nr-ord-produ
                      tt-refugo.codigo-rejei = if ord-prod.cod-estabel = "413" OR ord-prod.cod-estabel = "423" then 2 else 1/*solic-318*/
                      tt-refugo.qt-refugo    = tt-rep-prod.qt-refugo.
               run pi-recebe-tt-refugo in hReporte (input table tt-refugo).
            end.

            run pi-recebe-tt-erro in hReporte (input table tt-erro).
    
            run pi-recebe-tt-rep-prod in hReporte (input table tt-rep-prod).
    
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
                return "NOK":U.
            end.
            
            /*****************************************************************/
            create tt-param.
            assign tt-param.it-codigo  = input frame fPage0 fiItem
                   tt-param.lote       = input frame fPage0 fiLote
                   tt-param.emenda1    = input frame fPage0 fiEmenda1
                   tt-param.emenda2    = input frame fPage0 fiEmenda2
                   tt-param.qt-lote    = deLote
                   tt-param.qt-emenda1 = deEmenda1
                   tt-param.qt-emenda2 = deEmenda2.
            
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
                        return "NOK":U.
                    end.
                end.
            end case.
            
            find first tt-param.
            assign deLote    = tt-param.qt-lote
                   deEmenda1 = tt-param.qt-emenda1
                   deEmenda2 = tt-param.qt-emenda2.
                   
            /*****************************************************************/

            /*1 - c¢pia tt-rep-prod*/
            run pi-processa-reportes in hReporte (input-output table tt-rep-prod,
                                                  input        table tt-refugo,
                                                  input        table tt-res-neg,
                                                  input-output table tt-erro,
                                                  input        yes,
                                                  input        yes).
            
            
            /*Speto - 22/01/2004*/
            if temp-table tt-erro:has-records then do:
                assign lErro = true.
                for each tt-erro:
                    /*find cad-msgs
                         where cad-msgs.cd-msg = tt-erro.cd-erro no-lock no-error.
                    if avail cad-msgs and 
                       cad-msgs.tipo-msg = 1 /*Erro*/ then*/
                       return "NOK":U.
                end.
            end.
            /*Speto - 22/01/2004*/
    
            run pi-formatted-time-to-sec (input  replace(fiHrInicio:screen-value in frame fPage0,":",""),
                                          output deSegIni).
            run pi-formatted-time-to-sec (input  replace(fiHrTermino:screen-value in frame fPage0,":",""),
                                          output deSegFim).
    
            find last rep-oper-ctrab no-lock where 
                      rep-oper-ctrab.nr-ord-produ = ord-prod.nr-ord-produ no-error.
            
            find last oper-ord no-lock where 
                      oper-ord.nr-ord-produ = ord-prod.nr-ord-produ no-error.
    
            find first split-operac no-lock where 
                       split-operac.nr-ord-produ   = oper-ord.nr-ord-produ and 
                       split-operac.num-operac-sfc = oper-ord.num-operac-sfc no-error.
    
            create tt-rep-oper-ctrab.
            assign tt-rep-oper-ctrab.cod-ferr-prod         = ""
                   tt-rep-oper-ctrab.dat-fim-setup         = ?
                   tt-rep-oper-ctrab.dat-inic-setup        = ?
                   tt-rep-oper-ctrab.qtd-segs-fim-setup    = 0
                   tt-rep-oper-ctrab.qtd-segs-inic-setup   = 0
                   tt-rep-oper-ctrab.dat-fim-reporte       = input frame fPage0 fiDtTermino
                   tt-rep-oper-ctrab.dat-inic-reporte      = input frame fPage0 fiDtInicio
                   tt-rep-oper-ctrab.qtd-operac-refgda     = deQuantNec
                   tt-rep-oper-ctrab.qtd-operac-aprov      = 0
                   tt-rep-oper-ctrab.qtd-operac-reptda     = deQuantNec
                   tt-rep-oper-ctrab.qtd-operac-retrab     = 0
                   tt-rep-oper-ctrab.qtd-segs-fim-reporte  = deSegFim
                   tt-rep-oper-ctrab.qtd-segs-inic-reporte = deSegIni
                   tt-rep-oper-ctrab.cod-equipe            = input frame fPage0 fiCodOperador
                   tt-rep-oper-ctrab.num-contador-inic     = 0
                   tt-rep-oper-ctrab.num-contador-fim      = 0
                   /*** ParÉmetros p/ reporte ***/
                   tt-rep-oper-ctrab.nr-ord-produ          = split-operac.nr-ord-produ
                   tt-rep-oper-ctrab.num-seq-rep           = if avail rep-oper-ctrab then rep-oper-ctrab.num-seq-rep + 1
                                                                                     else 1
                   tt-rep-oper-ctrab.num-operac-sfc        = split-operac.num-operac-sfc
                   tt-rep-oper-ctrab.num-split-oper        = split-operac.num-split-oper
                   tt-rep-oper-ctrab.cod-ctrab             = input frame fPage0 fiCodCtrab.
            
            run GerarRepOperCtrab in hRepSfc (input table tt-rep-oper-ctrab,
                                              input table tt-rep-refugo-oper,
                                              input table tt-rep-ic-oper,
                                              input table tt-rep-ic-oper-tab).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  /* snd-head.i - 7/23/95 */
  DEFINE INPUT PARAMETER p-tbl-list AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER p-rowid-list AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE link-handle  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE rowid-string AS CHARACTER NO-UNDO.
  
  DO i = 1 TO NUM-ENTRIES(p-tbl-list):
      IF i > 1 THEN p-rowid-list = p-rowid-list + ",":U.
      CASE ENTRY(i, p-tbl-list):
 

  /* For each requested table, put it's ROWID in the output list.      */
  /* snd-list - 8/21/95 */
    WHEN "tt-reporta":U THEN p-rowid-list = p-rowid-list + 
        IF AVAILABLE tt-reporta THEN STRING(ROWID(tt-reporta))
        ELSE "?":U.
   
 

  /* Deal with any unexpected table requests before closing.           */
  /* snd-end.i */
        OTHERWISE 
        DO:
            RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE,
                INPUT "RECORD-SOURCE":U, OUTPUT link-handle) NO-ERROR.
            IF link-handle NE "":U THEN 
            DO:
                IF NUM-ENTRIES(link-handle) > 1 THEN  
                    MESSAGE "send-records in ":U THIS-PROCEDURE:FILE-NAME 
                            "encountered more than one RECORD-SOURCE.":U SKIP
                            "The first will be used.":U 
                            VIEW-AS ALERT-BOX ERROR.
                RUN send-records IN WIDGET-HANDLE(ENTRY(1,link-handle))
                    (INPUT ENTRY(i, p-tbl-list), OUTPUT rowid-string).
                p-rowid-list = p-rowid-list + rowid-string.
            END.
            ELSE
            DO:
                MESSAGE "Requested table":U ENTRY(i, p-tbl-list) 
                        "does not match tables in send-records":U 
                        "in procedure":U THIS-PROCEDURE:FILE-NAME ".":U SKIP
                        "Check that objects are linked properly and that":U
                        "database qualification is consistent.":U
                    VIEW-AS ALERT-BOX ERROR.     
                RETURN ERROR.
            END.
        END.
        END CASE.        
    END.                 /* i = 1 to num-entries */
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



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
    

    /*Vefifica a qtde maxima a reportar*/
    assign totMaxReporte = 0.
    for each tt-lote-reporte:
        assign totMaxReporte = totMaxReporte + tt-lote-reporte.quantidade.
    end.
    assign totMaxReporte = totMaxReporte + input frame fPage0 fiRefugo.
    if totMaxReporte > input frame fPage0 fiQtLote +
                       input frame fPage0 fiQtEmenda1 +
                       input frame fPage0 fiQtEmenda2 then do:
       run utp/ut-msgs.p (input "show":U,
                          input 17006,
                          input "Quantidade a reportar maior que quantidade a consumir").
       return "NOK":U.
    end.






    find first param-sfc no-lock.

    for each tt-erro:
        delete tt-erro.
    end.
        
    do with frame fPage0:
    
        if not can-find (first estabelec where
                               estabelec.cod-estabel = input fiCodEstabel) then
            run criaErro (17006,"Estabelecimento n∆o encontrado!").
                
        if input fiDtInicio = ? then
            run criaErro (17006,"Data inicial inv†lida!").
        
        if input fiDtTermino = ? then
            run criaErro (17006,"Data final inv†lida!").
        
        if input fiDtTermino < INPUT fidtInicio THEN 
            run criaErro (17006,"Data final menor que Data inicial!").
        
        if (param-sfc.log-tipo-relogio and 
            substr(input fiHrInicio,3,2) > '59') or  
            substr(input fiHrInicio,1,2) > "23" then
            run criaErro (3046,"Inicio").

        if input fiHrInicio = ? or
           length(input fiHrInicio) < 4  then
            run criaErro (17006,"Hora inicial inv†lida!").
        
                   
        if (param-sfc.log-tipo-relogio and 
            substr(input fiHrTermino,3,2) > '59') or  
            substr(input fiHrTermino,1,2) > '23' then
            run criaErro (3046,"Final").
        
        if input fiHrTermino = ? or
           length(input fiHrTermino) < 4  then
            run criaErro (17006,"Hora final inv†lida!").
        
        if input fiDtTermino = INPUT fidtInicio THEN DO:
            IF SUBSTRING(INPUT fihrinicio,1,2) = SUBSTRING(INPUT fihrtermino,1,2) THEN DO:
                IF SUBSTRING(INPUT fihrtermino,3,2) < SUBSTRING(INPUT fihrinicio,3,2) THEN
                    run criaErro (17006,"Hora final menor que Hora inicial!").
            END.
            ELSE IF SUBSTRING(INPUT fihrtermino,1,2) < SUBSTRING(INPUT fihrinicio,1,2) THEN 
                run criaErro (17006,"Hora final menor que Hora inicial!").
        END.

         if input fiDtTermino = INPUT fidtInicio AND INPUT fihrinicio = INPUT fihrtermino THEN DO:
            
                    run criaErro (17006,"Hora final igual que Hora inicial!").
            
            
        END.

        if not can-find (first operador where
                               operador.cod-operador = input fiCodOperador) then
            run criaErro (17006,"Operador n∆o encontrado!").
        
        if not can-find (first ctrab where
                               ctrab.cod-ctrab = input fiCodCtrab) then
            run criaErro (17006,"Centro de trabalho n∆o encontrado!").
        
        FIND FIRST pol-param-estab
             WHERE pol-param-estab.cod-estabel = input fiCodEstabel NO-LOCK NO-ERROR.
        if not avail pol-param-estab then 
            run criaErro (17006,"ParÉmetro do Estabelecimento n∆o encontrado!").
        ELSE DO:
            
            if input fiDtTermino > pol-param-estab.data-corte THEN 
                run criaErro (25997,"Transaá∆o n∆o permitida!Verifique Data de Corte informada nos ParÉmetros Estabelecimento - essf0008.").

            /* Numeracao do pallet Validaá∆o */
            for each tt-reporta
                WHERE tt-reporta.gera-pallet = "        *":
                
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
                WHERE tt-reporta.gera-pallet = "        *" NO-LOCK NO-ERROR.
            IF AVAIL tt-reporta
            AND TODAY > pol-param-estab.data-palete THEN
                run criaErro (17006,"Data de Paletizaá∆o n∆o pode ser superior a data Paletizaá∆o").
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
                                         IF fiCodEstabel:screen-value in frame fPage0 = '' THEN item.cod-estabel
                                         ELSE fiCodEstabel:screen-value in frame fPage0 ,
                                         "contr-qualid":U)) <> "yes":U THEN DO:
                                         
                  
                  run criaErro (17006,"ITEM " + string(item.it-codigo) + " n∆o esta marcado para controlar CQ!").
                    
                              if today = 05/16/2010 then 
                                run utp/ut-msgs.p (input "show",
                                       input 17006,
                                       input "Esta Ç uma nova trava para CQ incorreto, qualquer dificuldade ligue 35 9988 3054 - Edson ").
  
                     
               END.

           
           
           end.
           
           
           
           
    end.

    /*Se mais de uma OP a reportar somente se mais de uma OP*/
    if contOP1 > 1 then do:
        for each tt-reporta:
            if not can-find(first tt-lote-reporte
                            where tt-lote-reporte.nr-ord-produ = tt-reporta.nr-ord-produ) then
               run criaErro (17006,"Ordem " + string(tt-reporta.nr-ord-produ) + " deve possuir lotes para reportar!").
        end.
    end. 

    /*Verifica se pode refugar*/
    if contOP1 = 1 and contOP2 = 0 and input frame fPage0 fiRefugo = 0 then do:
       run criaErro (17006,"Quantidade refugada deve ser maior que 0 (zero)!").
    end.

    if temp-table tt-erro:has-records then do:
        run cdp/cd0666.w (input table tt-erro).
        return 'nok'.
    end.
    
     for each tt-reporta where tt-reporta.cod-depos = "cq" no-lock:

                OUTPUT TO m:\dts\log_prd\essf0002.cq APPEND.
                for each tt-lote-reporte where tt-lote-reporte.nr-ord-produ = tt-reporta.nr-ord-produ:
       

                    put "Prog: essf0002-APOS" FORMAT "x(19)" string(TODAY) "-" 
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


        return 'ok'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */






