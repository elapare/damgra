/* Connected Databases 
          movind           PROGRESS
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
**  i-prgvrs.i - Programa para criaªío do log de todos os programas 
**               e objetos do EMS 2.0 para objetos
**  {1} = objeto   provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/
def buffer empresa for mgmulti.empresa.
def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[2.00.00.000[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.000"
       c-prg-obj = "essf0903C".
if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "essf0903C"
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
    put "essf0903C" at 1 "2.00.00.000" at 69 today at 84 string(time,'HH:MM:SS':U) at 94 skip.
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
def input  parameter p-rw-pallet as rowid no-undo.
def output parameter p-ok        as log   no-undo init no.
def var c-codembal  as char no-undo.
def var c-estabelec as char no-undo.
def var c-formato   as char no-undo.
def var l-flag      as log  init yes no-undo.
def var i-empresa   as char no-undo.
def new shared var i-emp050      like conta-contab.ep-codigo.
def new shared var i-cta050      like conta-contab.ct-codigo.
def new shared var i-sbc050      like conta-contab.sc-codigo.
def new shared var i-red050      like conta-contab.reduzida.
def new global shared var l-req-embal as logical INITIAL NO.
DEFINE BUFFER operacao FOR mgemp.operacao.
def temp-table tt-embalagem no-undo
    field it-pai     like item.it-codigo  
    field it-codigo  like item.it-codigo
    field descricao  as char format "x(40)" label "Descricao"
    field quantidade like estrutura.quant-usada.
DEFINE BUFFER b-tt-embalagem FOR tt-embalagem.
/***   Include para os PrÇ-Processadores do Aplicativo de Distribuiá∆o    ***/
/*** Serve para o desenvolvimento tratar o conceito de miniflexibilizaá∆o ***/ 
/*** Funcoes de Uso Geral ***/
/* Utilizado para Teste de Release */
/*** Funcoes Liberadas na 2.02 ***/
/* Recebimento Fisico *//* Unidade de Negocio *//* Verifica Controle de Verba no Investimento *//* Melhorias em Controle de Contratos *//* Melhorias da Aprovacao Eletronica *//* Desatualizacao AP *//* Conciliacao Transitoria Fornecedores *//* Consulta Multi-Moeda em Controle Contratos *//* Conversao da Tabela ext-ord-per da 2.01 p/ 2.02 *//* Tipo de Ressuprimento de Estoque *//* Consumo e Estatistica por Estabelec *//* Importacao x MRP */
/* novo campo cod-tax especifico para compras no item-mat *//* Conta Cont†bil Investimentos */
/*** Funcoes Liberadas na 2.04 ***/
/* Fator Multiplicativo/Divisivo Conversao Moedas *//* Ident Imposto do Item Conforme a Natureza *//* Inclusao Impostos nas Despesas da Nota (MAT e DIS) *//* Selecao Estabelecimentos no Recebimento *//* Contas Transitorias por Estabelecimento *//* Fechamento por Estabelecimento *//* Componentes Usados em Certificado *//* Tipo de Compra na Natureza de Operacao para Recebimento *//* Tratamento de Multiplas Referencias por Lote *//* Estorno Devolucao AP/CR *//* Consumo e Estatistica por Estabelec - Fase II *//* Especie Fiscal no Recebimento *//* Operacao Triangular - Materiais *//* Rateio Despesas Recebimento Internacional *//* Reporte Automatico do Acabado *//* Melhorias em Contratos (Permissoes, Aprovacao, etc...) *//* ParÉmetros Item/Fam°lia por Estabelecimento *//* Nota Fiscal de Simples Remessa e Despesas da Nota Fiscal Complementar */
/*** Funcoes Pendentes ***/
/*&glob bf_mat_devol_cli_inter       yes   /* Devolucao de Cliente do Internacional */*/
/*&glob bf_mat_estorno_cr            yes   /* Estorno Contas a Receber */*/
/*&glob bf_mat_custo_on_line         yes   /* Custo On-line */*/
    /* Definiá∆o preprocessadores materiais*/
/*** Esta include no ems 2.01 n∆o dever† possuir nenhum c¢digo, serve 
     apenas para desenvolvimento tratar o conceito de miniflexibilizaá∆o.
     Utilizado apenas para MANUFATURA. ***/
/*** RELEASE 2.02 ***/
/* Funá‰es ch∆o f†brica e lista de componentes 2.02 - Logoplaste - Manufatura */
/*** RELEASE 2.03 ***/
/* Integraá∆o Magnus x SFC 2.03 - Klimmek - Manufatura *//* Relacionamento Linha Produá∆o x Estabelecimento     *//* Transaá∆o Reporte Ass°ncrono                        *//* Alteraá‰es Gerais EMS 2.03                          */
/*** RELEASE 2.04 ***/
/* Alteraá‰es Gerais EMS 2.04                          *//* Integraá∆o EAI                                     */     /* Definiá∆o preprocessadores */
/*** Esta include no ems 2.01 n∆o dever† possuir nenhum c¢digo, serve 
     apenas para desenvolvimento tratar o conceito de miniflexibilizaá∆o.
     Utilizado apenas para MANUTENÄ«O INDUSTRIAL. ***/
/* Funá‰es EMS 2.03 - Manutená∆o Industrial *//* Funá‰es EMS 2.04 - Manutená∆o Industrial *//* Funcoes Modulo Calibracao apartir 202 */   /* Definiá∆o preprocessadores manutená∆o industrial */
 
/* Local Variable Definitions ---                                       */
 
def temp-table tt-ficha 
    field cod-depos    like ficha-cq.cod-depos
    field cod-estabel  like ficha-cq.cod-estabel
    field it-codigo    like ficha-cq.it-codigo
    field cod-localiz  like ficha-cq.cod-localiz
    field lote         like ficha-cq.lote
    field nr-ficha     like ficha-cq.nr-ficha
    field nr-ord-produ like ficha-cq.nr-ord-produ
    field op-seq       like ficha-cq.op-seq   
    field nr-ordem     like ficha-cq.nr-ordem
    field nro-docto    like ficha-cq.nro-docto
    field origem       like ficha-cq.origem
    field qt-original  like ficha-cq.qt-original
    field ct-destino   like ficha-cq.ct-destino
    field sc-destino   like ficha-cq.sc-destino
    field serie-docto  like ficha-cq.serie-docto
    field cod-emitente like ficha-cq.cod-emitente
    field nat-operacao like ficha-cq.nat-operacao
    field parcela      like ficha-cq.parcela
    field baixa-estoq  like ficha-cq.baixa-estoq
    field inspecionado like ficha-cq.inspecionado
    field liberada     like ficha-cq.liberada
    field estado       like ficha-cq.estado
    field qt-aprovada  like ficha-cq.qt-aprovada
    field narrativa    like ficha-cq.narrativa.
 
def var i-ficha-cq like ficha-cq.nr-ficha. 
 
def buffer b-mov-es for movto-estoq.
def buffer b-movto for movto-mat.
def buffer b-saldo for saldo-estoq.
def buffer b-ordem for ord-prod.     
def buffer b-ord-manut for ord-manut.     
def new global shared var r-item-docto    as rowid no-undo.
def new global shared var l-alerta        as log init yes no-undo.
def new global shared var c-serie-movto like movto-estoq.serie no-undo.
def var i-ant-anterior as integer.
def new shared var i-op-codigo    like operacao.op-codigo.
def new shared var c-cod-roteiro  like rot-fabric.cod-roteiro.
def new shared var c-op-descricao like operacao.descricao.
def new shared var i-fi-codigo    like operacao.fi-codigo.
def new shared var c-gm-codigo    like operacao.gm-codigo.
def new shared var i-ordem       like ord-prod.nr-ord-prod.
 
/*def new shared var c-estab       like saldo-estoq.cod-estabel.*/
def new global shared var c-estab       like saldo-estoq.cod-estabel no-undo.
 
 
 
def new shared var c-lote-serie  like saldo-estoq.lote.
 
def new shared var c-deposito    like saldo-estoq.cod-depos.
def new shared var i-requis like movto-estoq.nro-docto.
def new shared var c-cod-estabel like estabelec.cod-estabel.
 
 
def new global shared var c-it-codigo like saldo-estoq.it-codigo no-undo.
 
def new shared var c-it-descricao as character format "x(36)".
def new shared var c-it-un like item.un.
def new shared var i-ge-codigo   like item.ge-codigo.
def new shared var c-fm-codigo   like item.fm-codigo.
def new shared var c-referencia  like saldo-estoq.cod-refer.
 
def var c-ant-it-codigo        as char init "?" no-undo.
def var l-executou-zoom-item   as log  no-undo.
def var l-saldo-novo           as logical.
def var r-saldo-estoq          as rowid.
def var r-item                 as rowid.
def var l-resposta             as logical format "Sim/Nao".
def var l-x                    as logical.
def var l-erro-x               as logical init no no-undo.
def var i-cont                 as integer.
def var i-conta                as integer.
def var i-subconta             as integer.
def var r-registro             as rowid.
def var c-opcao                as character no-undo.
                               
def var l-achou                as logical no-undo.
def var c-conta-inv            like conta-contab.conta-contabil no-undo.
def var c-par-inv              as char no-undo.
def var c-desc-ord-inv         as char no-undo.
DEF VAR l-implantado-invest AS LOG INIT NO NO-UNDO.
/***   Include para os PrÇ-Processadores do Aplicativo de Distribuiá∆o    ***/
/***     Esta include no EMS 2.01 n∆o dever† possuir nenhum c¢digo        ***/
/*** Serve para o desenvolvimento tratar o conceito de miniflexibilizaá∆o ***/ 
/*** Funcoes de Uso Geral ***/
/* Utilizado pelo Pre-Faturamento e Teste de Release */
/*** Funcoes Liberadas na 2.02 ***/
/* Usuario e Moeda de Credito *//* Faturamento Outra Moeda *//* Controle Devolucao a Fornecedor *//* Formato do campo nr-siscomex "caracter" *//* Faturamento de Remito por Embarque *//* Contrato de Fornecimento para Pedido *//* Nota Credito no Internacional *//* Existe nr-proc-exp na Entrega do Pedido *//* MP para Programacao de Entrega */
/*** Funcoes Liberadas na 2.03 ***/
/* Ciap *//* Descontos e Bonificacoes *//* Administracao Precos Vendas *//* Controle de Fretes *//* Modulo de Administracao de Cotas *//* Geracao de Referencia por Nota Fiscal *//* Consistir Conta Contabil *//* Consiste Unidade de Medida *//* Unidade de Negocio *//* MP para Programacao de Entrega *//* Novo Campo p/ Alocacao Negativa Pre-Faturamento */
/*** Funcoes Liberadas na 2.04 ***/
/* Relacionamento Usu†rios X Estabelecimento *//* Relacionamento ParÉmetros Faturamento X Estabelecimento *//* Relacionamento SÇrie X Estabelecimento *//* Relacionamento Natureza Operaá∆o X UF *//* Frete compras recebimento */
 
DEF VAR c-cod-estabel-usuar LIKE estabelec.cod-estabel.
def var c-titulo as character format "x(33)".
def var de-aux-1 like ficha-cq.nr-ficha.
def var l-resp as logical no-undo format "Sim/Nao".
def var l-confirma as logical no-undo format "Sim/Nao".
def var i-seq    like oper-ord.sequencia.
def var serie-docto like movto-estoq.serie-docto.
def var nro-docto like movto-estoq.nro-docto.
def var numero-ord like movto-estoq.nr-ord-produ.
def var conta-contabil like movto-estoq.conta-contabil.
def var reduzida like conta-contab.reduzida.
def var lote like movto-estoq.lote.
def var l-primeira as logical initial yes.
def var de-saldo-calc like aloca-reserva.quant-aloc no-undo.
DEFINE VARIABLE c-emb-it-codigo AS CHARACTER  NO-UNDO.
DEFINE VARIABLE d-emb-saldo     AS DECIMAL    NO-UNDO.
  
/* Vari†veis de controle para movimentaá‰es repetitivas --------------------------*/
define variable l-repetiu       as logical initial no                       no-undo.
 
define variable da-data-repet   as date                                     no-undo.
define variable i-ordem-repet   as integer                                  no-undo.
define variable c-docto-repet   as character                                no-undo.
define variable c-serie-repet   as character                                no-undo.
define variable i-reduz-repet   as integer                                  no-undo.
define variable c-conta-repet   as character                                no-undo.
define variable c-descr-repet   as character                                no-undo.
 
def temp-table tt-erro-aux no-undo
        field i-sequen as int             
        field cd-erro  as int
        field mensagem as char format "x(255)"
        field c-param  as char. 
/* Utilizada na include ce0205.i */ 
def var de-qtde-atualizada-aloca-reserva as decimal no-undo.
def var h-cpapi013                       as handle  no-undo.
 
/* def new shared var l-implanta as logical init no. */
/**************************************************************************
**
**   ceapi001.i - Include de definicoes da temp-table e variaveis 
**
**                da API ceapi001.p
**
**************************************************************************/
def temp-table tt-movto  
    field cod-versao-integracao as integer format "999"
    field cod-prog-orig         like movto-estoq.cod-prog-orig
    field l-mov-erro            as logical initial no
    field r-mov-inv             as rowid    
    field r-mov-orig            as rowid  /* registro original para  
                                             valorizar o estorno,
                                             devoluá∆o,retorno */
    field sequen-nf             like movto-estoq.sequen-nf
    field cod-depos             like movto-estoq.cod-depos
    field cod-emitente          like movto-estoq.cod-emitente
    field cod-estabel           like movto-estoq.cod-estabel
    field cod-refer             like movto-estoq.cod-refer
    field ct-codigo             like movto-estoq.ct-codigo
    field descricao-db          like movto-estoq.descricao-db
    field dt-nf-saida           like movto-estoq.dt-nf-saida
    field dt-trans              like movto-estoq.dt-trans
    field esp-docto             like movto-estoq.esp-docto
    field it-codigo             like movto-estoq.it-codigo
    field cod-localiz           like movto-estoq.cod-localiz
    field lote                  like movto-estoq.lote
    field nat-operacao          like movto-estoq.nat-operacao
    field nro-docto             like movto-estoq.nro-docto
    field num-sequen            like movto-estoq.num-sequen
    field numero-ordem          like movto-estoq.numero-ordem
    field nr-ord-produ          like movto-estoq.nr-ord-produ
    field peso-liquido          like movto-estoq.peso-liquido
    field quantidade            like movto-estoq.quantidade
    field referencia            like movto-estoq.referencia
    field sc-codigo             like movto-estoq.sc-codigo
    field serie-docto           like movto-estoq.serie-docto
    field tipo-preco            like movto-estoq.tipo-preco
    field tipo-trans            like movto-estoq.tipo-trans
    field tipo-valor            like movto-estoq.tipo-valor
    field un                    like movto-estoq.un         
    field valor-mat-m           like movto-estoq.valor-mat-m
    field valor-mat-o           like movto-estoq.valor-mat-o
    field valor-mat-p           like movto-estoq.valor-mat-p
    field valor-mob-m           like movto-estoq.valor-mob-m
    field valor-mob-o           like movto-estoq.valor-mob-o
    field valor-mob-p           like movto-estoq.valor-mob-p
    field valor-ggf-m           like movto-estoq.valor-ggf-m
    field valor-ggf-o           like movto-estoq.valor-ggf-o
    field valor-ggf-p           like movto-estoq.valor-ggf-p
    field valor-nota            like movto-estoq.valor-nota
    field vl-nota-fasb          like movto-estoq.vl-nota-fasb
    field nr-ord-refer          like movto-estoq.nr-ord-refer
    field nr-req-sum            like movto-estoq.nr-req-sum
    field cod-roteiro           like movto-estoq.cod-roteiro
    field nr-reporte            like movto-estoq.nr-reporte
    field item-pai              like movto-estoq.item-pai
    field op-codigo             like movto-estoq.op-codigo
    field cod-usu-ult-alter     like movto-estoq.cod-usu-ult-alter
    field conta-contabil        like movto-estoq.conta-contabil
    field conta-db              like movto-estoq.conta-contabil
    field ct-db                 like movto-estoq.ct-codigo
    field sc-db                 like movto-estoq.sc-codigo
    field dt-vali-lote          like saldo-estoq.dt-vali-lote
    field op-seq                like movto-estoq.op-seq
    field usuario               like movto-estoq.usuario
    field nr-trans              like movto-estoq.nr-trans 
    field cod-estabel-des       like movto-estoq.cod-estabel-des
    field origem-valor          like movto-estoq.origem-valor
    field num-ord-des           like movto-estoq.num-ord-des
    field num-seq-des           like movto-estoq.num-seq-des
    field num-ord-inv           like movto-estoq.num-ord-inv
    field valor-ipi             like movto-estoq.valor-ipi
    field valor-iss             like movto-estoq.valor-iss
    field valor-icm             like movto-estoq.valor-icm
    field vl-icm-fasb           like movto-estoq.vl-icm-fasb
    field vl-iss-fasb           like movto-estoq.vl-iss-fasb
    field vl-ipi-fasb           like movto-estoq.vl-ipi-fasb 
    field per-ppm               like movto-estoq.per-ppm
    field atualiza-ul-ent       as logical
    field i-sequen              as integer
    field gera-saldo            as logical init no
    field qt-alocada            as decimal.    
     
/* Fim Include ceapi001.i */
     /* Definicao de temp-table do movto-estoq */
/******************************************************************************
 ** 
 **  INCLUDE  : CPAPI013.I 
 **
 **  OBJETIVO : Definir a temp-table da API de Aloc. de Materiais 
 **
 ******************************************************************************/
 
 def temp-table tt-alocacao no-undo
     field cod-versao-integracao as   integer format "999"
     field tipo-trans            as   integer init 1
     field sit-aloc              as   integer init 1
     field nr-ord-produ          like ord-prod.nr-ord-produ
     field prioridade            as   integer init 3
     field informa-dep           as   logical
     field reaproveita           as   logical
     field aloca-altern          as   logical init ?
     field prog-seg              as   char
     field perc-proporcional     as   decimal init 100.
     
 def temp-table tt-deposito
     field indicador    as logical format "*/ "
     field cod-depos    like deposito.cod-depos
     field nome         like deposito.nome
     field ind-tipo-dep as char format "x(15)"
     field ind-processo like deposito.ind-processo
     field alocado      like deposito.alocado
     field prioridade   as int init 0
     index deposito is unique primary cod-depos
     index prioridade prioridade.
     
 
     /* Definicao da temp-table de alocacao    */
/*******************************************************************
**
**  CD0666.I - Definicao temp-table de erros
**
*******************************************************************/
def new global shared var l-multi as logical initial yes.
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
       /* Definicao da temp-table de erros */
/********************************************************************
* cd9731.i3 - Objetivo: Ativar a Funá∆o de Integraá∆o  CE0205 com o m¢dulo de 
                        investimento 
*                 Data: 10/09/2003
*********************************************************************/
def var l-ce0205-invest as logical init no no-undo.
if not avail param-global then 
    find first param-global no-lock no-error.
    
        if param-global.modulo-in then do:
            if can-find (funcao where
                         funcao.cd-funcao = "requis_min" and
                         funcao.ativo     = yes) then
                assign l-ce0205-invest = Yes.
        end.
    
      /* Verifica se utiliza o modulo de investimento*/
DEF VAR wh-pop-menu AS WIDGET-HANDLE NO-UNDO.
DEF VAR wh-item1    AS WIDGET-HANDLE NO-UNDO.
DEF VAR wh-item2    AS WIDGET-HANDLE NO-UNDO.
def var h-acomp as handle no-undo.
def buffer b-item for item.
/* _UIB-CODE-BLOCK-END */
/* ********************  Preprocessor Definitions  ******************** */
/* Name of first Frame and/or Browse and/or first Query                 */
/* Internal Tables (found by Frame, Query & Browse Queries)             */
/* Definitions for BROWSE br-embalagem                                  */
/* Definitions for FRAME F-Main                                         */
/* Standard List Definitions                                            */
/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
/* _UIB-PREPROCESSOR-BLOCK-END */
/* ***********************  Control Definitions  ********************** */
/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.
/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.
DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.
DEFINE BUTTON bt-elimina 
     LABEL "Elimina" 
     SIZE 10 BY 1.
DEFINE BUTTON bt-inclui 
     LABEL "Inclui" 
     SIZE 10 BY 1.
DEFINE BUTTON bt-modifica  NO-FOCUS
     LABEL "Modifica" 
     SIZE 10 BY 1.
DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.
DEFINE VARIABLE c-conta-contabil AS CHARACTER FORMAT "x(17)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.
DEFINE VARIABLE c-titulo-conta AS CHARACTER FORMAT "x(32)" 
     VIEW-AS FILL-IN 
     SIZE 33.14 BY .88.
DEFINE VARIABLE fi-dt-trans LIKE movto-estoq.dt-trans
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .88 NO-UNDO.
DEFINE VARIABLE fi-nro-docto LIKE movto-estoq.nro-docto
     VIEW-AS FILL-IN 
     SIZE 17.43 BY .88 NO-UNDO.
DEFINE VARIABLE fi-serie-docto LIKE movto-estoq.serie-docto
     VIEW-AS FILL-IN 
     SIZE 9.43 BY .88 NO-UNDO.
DEFINE VARIABLE i-reduzida AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Conta Reduzida":R17 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 86.43 BY 1.38
     BGCOLOR 7 .
DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 86.43 BY 3.42.
DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 86.43 BY 7.21.
     
 DEFINE BUTTON btpeso 
     IMAGE-UP FILE "image\im-key":U
     IMAGE-INSENSITIVE FILE "image\ii-key":U
     LABEL "REQ" 
     SIZE 4 BY 1.25
     FONT 4.
/* Query definitions                                                    */
DEFINE QUERY br-embalagem FOR 
      tt-embalagem SCROLLING.
/* Browse definitions                                                   */
DEFINE BROWSE br-embalagem
  QUERY br-embalagem DISPLAY
      tt-embalagem.it-codigo format "x(20)"
      tt-embalagem.descricao
      tt-embalagem.quantidade
enable
          tt-embalagem.it-codigo
          tt-embalagem.quantidade  
/* _UIB-CODE-BLOCK-END */
    WITH NO-ROW-MARKERS SEPARATORS SIZE 84.72 BY 5.63.
/* ************************  Frame Definitions  *********************** */
DEFINE FRAME F-Main
     bt-modifica AT ROW 10.58 COL 66.29
     fi-dt-trans AT ROW 1.25 COL 16.86 COLON-ALIGNED
     fi-nro-docto AT ROW 2.29 COL 16.86 COLON-ALIGNED
     fi-serie-docto AT ROW 2.29 COL 53.14 COLON-ALIGNED
     i-reduzida AT ROW 3.29 COL 16.86 COLON-ALIGNED
     c-conta-contabil AT ROW 3.29 COL 31.14 COLON-ALIGNED HELP
          "F5 para zoom" NO-LABEL
     c-titulo-conta AT ROW 3.29 COL 48.86 COLON-ALIGNED NO-LABEL
     br-embalagem AT ROW 4.79 COL 1.86
     btpeso AT ROW 10.5 COL 50 HELP "Libera Bot‰es de Embalagens"
     bt-inclui AT ROW 10.58 COL 56
     bt-elimina AT ROW 10.58 COL 76.57
     bt-ok AT ROW 12.21 COL 1.72
     bt-cancelar AT ROW 12.21 COL 12.72
     bt-ajuda AT ROW 12.21 COL 76.86
     RECT-1 AT ROW 12 COL 1
     RECT-2 AT ROW 1 COL 1
     RECT-3 AT ROW 4.63 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.43 BY 12.58.
/* *********************** Procedure Settings ************************ */
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
/* *************************  Create Window  ************************** */
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert Custom SmartWindow title>"
         HEIGHT             = 12.71
         WIDTH              = 86.57
         MAX-HEIGHT         = 21.13
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.13
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE w-window = CURRENT-WINDOW.
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
  
    ASSIGN adm-object-hdl    =   w-window.
  
/* Traduá∆o de Hard-Coded View-as */ 
    
        run pi-trad-widgets (input frame F-Main:handle).
    
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
     'JanelaDetalhe~`':U +      /* Type attribute */
     'WINDOW~`':U +       /* Container-Type attribute */
   
     'NO ~`':U +
   
     '~`':U +    /* External-Tables attribute */
     'tt-embalagem~`':U +    /* Internal-Tables attribute */
   
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
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    
    run value(c-nom-prog-appc-mg97) (input "DESTROY":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame F-Main:handle,
                                     input "",
                                     input ?).    
end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:
    run value(c-nom-prog-upc-mg97) (input "DESTROY":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame F-Main:handle,
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
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    
    run value(c-nom-prog-appc-mg97) (input "BEFORE-DISABLE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame F-Main:handle,
                                     input "",
                                     input ?).    
end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:
    run value(c-nom-prog-upc-mg97) (input "BEFORE-DISABLE", 
                                    input "CONTAINER",
                                    input this-procedure,
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    
end.                                       
/* I-EPC009.I */
 
    
    
    DISABLE bt-modifica fi-nro-docto fi-serie-docto br-embalagem bt-inclui bt-elimina bt-ok btpeso  bt-cancelar bt-ajuda RECT-1 RECT-2 RECT-3 WITH FRAME F-Main.
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
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    
    run value(c-nom-prog-appc-mg97) (input "DISABLE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame F-Main:handle,
                                     input "",
                                     input ?).    
end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:
    run value(c-nom-prog-upc-mg97) (input "DISABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame F-Main:handle,
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
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    
    run value(c-nom-prog-appc-mg97) (input "BEFORE-ENABLE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame F-Main:handle,
                                     input "",
                                     input ?).    
end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:
    run value(c-nom-prog-upc-mg97) (input "BEFORE-ENABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    
end.                                       
/* I-EPC008 */
 
   
   
    ENABLE UNLESS-HIDDEN /*bt-modifica*/ fi-nro-docto fi-serie-docto br-embalagem /*bt-inclui bt-elimina*/ bt-ok btpeso  bt-cancelar bt-ajuda RECT-1 RECT-2 RECT-3 WITH FRAME F-Main.
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
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    
    run value(c-nom-prog-appc-mg97) (input "ENABLE":U, 
                                     input "CONTAINER",
                                     input this-procedure,
                                     input frame F-Main:handle,
                                     input "",
                                     input ?).    
end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:
    run value(c-nom-prog-upc-mg97) (input "ENABLE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame F-Main:handle,
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
   /* alteraªío feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */
   
   /* est† verificaá∆o se faz necess†ria devido aos programas */
      
    /* Tenta identificar pelo ADM-CONTAINER */
    
    
        
                    
        
    
    /* Se ainda nío identificou se ≤ window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
    /* Se ainda nío identificou se ≤ window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
    
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
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    
    run value(c-nom-prog-appc-mg97) (input "BEFORE-INITIALIZE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame F-Main:handle,
                                     input "",
                                     input ?).    
end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:
    run value(c-nom-prog-upc-mg97) (input "BEFORE-INITIALIZE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame F-Main:handle,
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
   
        
             IF  frame F-Main:scrollable THEN
                 ASSIGN frame F-Main:virtual-width-chars  = frame F-Main:width-chars
                        frame F-Main:virtual-height-chars = frame F-Main:height-chars.
        
   
                                                               /*
    Nome :  C-PAGE.i       J.Carlos - 21/Fev/97
    Funªío : Guardar a pagina e o container-source da VIEWER.
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
                                    input frame F-Main:handle,
                                    input "",
                                    input ?).    
end.
/* APPC */
if  c-nom-prog-appc-mg97 <> ""
and c-nom-prog-appc-mg97 <> ? then do:           
    
    run value(c-nom-prog-appc-mg97) (input "INITIALIZE":U, 
                                     input "CONTAINER":U,
                                     input this-procedure,
                                     input frame F-Main:handle,
                                     input "",
                                     input ?).    
end.                                       
/* UPC */
if  c-nom-prog-upc-mg97 <> ""
and c-nom-prog-upc-mg97 <> ? then do:
    run value(c-nom-prog-upc-mg97) (input "INITIALIZE":U, 
                                    input "CONTAINER":U,
                                    input this-procedure,
                                    input frame F-Main:handle,
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
                                            input frame F-Main:handle  ,
                                            input "",
            
                                            input ?).
            
        end.
        
         /* APPC */
        if  c-nom-prog-appc-mg97 <> "" and
            c-nom-prog-appc-mg97 <> ? then do:           
            run value(c-nom-prog-appc-mg97)  (input "CHANGE-PAGE":U, 
                                             input "CONTAINER":U,
                                             input h-ctrl-tab,
                                             input frame F-Main:handle  ,
                                             input "",
            
                                             input ?).
            
        end.
         /* UPC */
        if  c-nom-prog-upc-mg97 <> "" and
            c-nom-prog-upc-mg97 <> ? then do:                  
            run value(c-nom-prog-upc-mg97) (input "CHANGE-PAGE":U, 
                                            input "CONTAINER":U,
                                            input h-ctrl-tab,
                                            input  frame F-Main:handle,
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
                                            input frame F-Main:handle  ,
                                            input "",
            
                                            input ?).
            
        end.
        
         /* APPC */
        if  c-nom-prog-appc-mg97 <> "" and
            c-nom-prog-appc-mg97 <> ? then do:           
            run value(c-nom-prog-appc-mg97)  (input "AFTER-CHANGE-PAGE":U, 
                                             input "CONTAINER":U,
                                             input h-ctrl-tab,
                                             input frame F-Main:handle  ,
                                             input "",
            
                                             input ?).
            
        end.
         /* UPC */
        if  c-nom-prog-upc-mg97 <> "" and
            c-nom-prog-upc-mg97 <> ? then do:                  
            run value(c-nom-prog-upc-mg97) (input "AFTER-CHANGE-PAGE":U, 
                                            input "CONTAINER":U,
                                            input h-ctrl-tab,
                                            input  frame F-Main:handle,
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
 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :
    Syntax      :
    Description :
    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
define variable wh-pesquisa             as handle               no-undo.
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
         HEIGHT             = 1.83
         WIDTH              = 40.
 /* END WINDOW DEFINITION */
                                                                        */
 
/* ***************************  Main Block  *************************** */
/* _UIB-CODE-BLOCK-END */
/* **********************  Internal Procedures  *********************** */
PROCEDURE pi-after-initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-before-initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-enter-go :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    apply 'CHOOSE':U to bt-ok in frame F-Main.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-entry :
/*------------------------------------------------------------------------------
  Purpose:     Trata as mudanªas de estado (State-Changed)
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    RUN new-state in THIS-PROCEDURE ("apply-entry":u).
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-trata-state :
/*------------------------------------------------------------------------------
  Purpose:     Trata as mudanªas de estado (State-Changed)
  Parameters:  INPUT Handle da procedure pai
               INPUT CΩdigo do Estado
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl   AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state        AS CHAR   NO-UNDO.
  
  case entry(1, p-state, '|'):
  end.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
 
/***************************************************************************
**  Programa : UT-GLOB.I
**  Include padr„o para definiÁ„o de variaveis globais.
***************************************************************************/ 
/* alteraªío feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */
/* est† verificaá∆o se faz necess†ria devido aos programas */
   
    /* Tenta identificar pelo ADM-CONTAINER */
    
    
        
                    
        
    
    /* Se ainda nío identificou se ≤ window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
    /* Se ainda nío identificou se ≤ window ou dialog (Os ifs sío feitos assim para nío dar erro de sintaxe) */
    
    
    
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
     
     def new global shared var i-ep-codigo-usuario  like empresa.ep-codigo no-undo.
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
     def new global shared var v_cdn_empres_usuar   like empresa.ep-codigo        no-undo.
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
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* BROWSE-TAB br-embalagem c-titulo-conta F-Main */
/* SETTINGS FOR FILL-IN c-conta-contabil IN FRAME F-Main
   NO-ENABLE 1 2 3                                                      */
/* SETTINGS FOR FILL-IN c-titulo-conta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-trans IN FRAME F-Main
   NO-ENABLE 1 LIKE = movind.movto-estoq.dt-trans EXP-SIZE              */
/* SETTINGS FOR FILL-IN fi-nro-docto IN FRAME F-Main
   1 LIKE = movind.movto-estoq.nro-docto EXP-SIZE                       */
/* SETTINGS FOR FILL-IN fi-serie-docto IN FRAME F-Main
   1 LIKE = movind.movto-estoq.serie-docto EXP-SIZE                     */
/* SETTINGS FOR FILL-IN i-reduzida IN FRAME F-Main
   NO-ENABLE 1 2 3                                                      */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.
/* _RUN-TIME-ATTRIBUTES-END */
/* Setting information for Queries and Browse Widgets fields            */
/* Query rebuild information for BROWSE br-embalagem
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-embalagem.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-embalagem */
 
/* ************************  Control Triggers  ************************ */
ON END-ERROR OF w-window /* <insert Custom SmartWindow title> */
OR ENDKEY OF w-window ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.
/* _UIB-CODE-BLOCK-END */
ON WINDOW-CLOSE OF w-window /* <insert Custom SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
/* _UIB-CODE-BLOCK-END */
ON END-ERROR OF br-embalagem IN FRAME F-Main
ANYWHERE 
DO:
  
        if  br-embalagem:new-row in frame F-Main then do:
            if avail tt-embalagem then
               delete tt-embalagem.
            if br-embalagem:delete-current-row() in frame F-Main then. 
        end.                                                               
        else do:
            get current br-embalagem.
            display tt-embalagem.it-codigo
                    tt-embalagem.quantidade 
                with browse br-embalagem. 
        end.
        return no-apply.
  
END.
/* _UIB-CODE-BLOCK-END */
ON ROW-LEAVE OF br-embalagem IN FRAME F-Main
DO:
       if br-embalagem:NEW-ROW in frame F-Main then 
        do transaction on error undo, return no-apply:
            if input browse br-embalagem tt-embalagem.it-codigo <> "" then do:
                create tt-embalagem.
                assign input browse br-embalagem tt-embalagem.it-codigo
                       input browse br-embalagem tt-embalagem.quantidade.
                br-embalagem:CREATE-RESULT-LIST-ENTRY() in frame F-Main.
            end.
        end.
        else do transaction on error undo, return no-apply:
            assign input browse br-embalagem tt-embalagem.it-codigo
                   input browse br-embalagem tt-embalagem.quantidade.
        end.
END.
/* _UIB-CODE-BLOCK-END */
on entry OF tt-embalagem.quantidade in browse br-embalagem do:
   if bt-inclui:SENSITIVE IN FRAME F-Main = NO THEN DO:
         MESSAGE "Alteraá∆o de embalagem n∆o permitida"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'ENTRY' TO tt-embalagem.it-codigo IN BROWSE br-embalagem.
        RETURN NO-APPLY.
   END.
end.
ON CHOOSE OF btpeso IN FRAME F-Main /* Add */
DO:
   assign l-req-embal = no.
   
   RUN SFC/essf0903c-A1.W.
   
     IF l-req-embal = yes THEN DO:
     
         ASSIGN bt-elimina:SENSITIVE IN FRAME F-Main = yes
         bt-inclui:SENSITIVE IN FRAME F-Main = yes.
         
         MESSAGE "ELIMINACAO DE ITENS DE EMBALAGEM LIBERADA TEMPORARIAMENTE."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
             
     END.
              ELSE
              
             ASSIGN bt-elimina:SENSITIVE IN FRAME F-Main = NO
                    bt-inclui:SENSITIVE IN FRAME F-Main  = NO. 
.
   assign l-req-embal = no.
  /*MESSAGE l-req-embal
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */          
END.
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME F-Main
DO:
  /*************************************************************************
**
** AJUDA.I - Include padr∆o para chamada do Help
**
**************************************************************************/
    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).
RETURN NO-APPLY.
/* include/ajuda.i */
 
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  assign p-ok = no.
  apply "close":U to this-procedure.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF bt-elimina IN FRAME F-Main /* Elimina */
DO:
  if not avail tt-embalagem then
     return no-apply.
      
    if  br-embalagem:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-embalagem.
        delete tt-embalagem.
        if br-embalagem:delete-current-row() in frame F-Main then.
    end.
    
    if num-results("br-embalagem":U) = 0 then
       assign bt-modifica:SENSITIVE in frame F-Main = no
              bt-elimina:SENSITIVE in frame F-Main  = no.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF bt-inclui IN FRAME F-Main /* Inclui */
DO:
    assign bt-modifica:SENSITIVE in frame F-Main = yes
           bt-elimina:SENSITIVE  in frame F-Main = yes.
    
    if num-results("br-embalagem":U) > 0 then
        br-embalagem:INSERT-ROW("after":U) in frame F-Main.
    else do transaction:
        create tt-embalagem.
        assign tt-embalagem.it-pai = "<none>":U.
        open query br-embalagem for each tt-embalagem.
        apply "entry":U to tt-embalagem.it-codigo in browse br-embalagem. 
    end.
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF bt-modifica IN FRAME F-Main /* Modifica */
DO:
  if not avail tt-embalagem then
     return no-apply.
  apply 'entry':U to tt-embalagem.it-codigo in browse br-embalagem. 
  
END.
/* _UIB-CODE-BLOCK-END */
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
   def buffer blote-carac-tec for lote-carac-tec.
   def buffer blote-res-carac for lote-res-carac.
   def var c-pri-lote  as char no-undo.
   def var c-pri-refer as char no-undo.
   /* Nova Validade - JosÇ Roberto */
   
   DEFINE VARIABLE dt-vali-jr AS DATE  NO-UNDO.
   
   assign dt-vali-jr = 12/31/9999.
   
  
   /* Fim da Nova Validade */
   if input frame F-Main fi-dt-trans < pallet.data-pallet then do:
      run utp/ut-msgs.p (input "show", input 17006, input "A Data n∆o pode ser menor que a data do Pallet!").
      apply 'entry' to fi-dt-trans in frame F-Main.
      return no-apply. 
   end.
   for each tt-movto exclusive-lock: delete tt-movto. end. 
   for each tt-erro exclusive-lock:  delete tt-erro.  end.
   find param-global no-lock no-error.
   find param-cp     no-lock no-error.
   if input frame F-Main fi-dt-trans > today then do:
       run utp/ut-msgs.p (input "show", input 1929, input "").
       apply 'entry' to fi-dt-trans in frame F-Main.
       return no-apply.
   end.
   /* edson cometou 06/02/2007 - verificar a finalidade deste teste que esta com chave invertida
   se a ideia era previnir que se requisitasse duas veses na repaletizaá∆o ou outra funá∆o
   
   if can-find(first pallet
               where pallet.nro-docto   = input frame F-Main fi-serie-docto 
                 and pallet.serie-docto = input frame F-Main fi-nro-docto no-lock) then  do:
      run utp/ut-msgs.p (input "show", input 3175, input "").
      return no-apply.
   end.*/
   find first b-mov-es where (b-mov-es.esp-docto = 28 or b-mov-es.esp-docto = 5) and
                              b-mov-es.serie-docto = input frame F-Main fi-serie-docto and
                              b-mov-es.nro-docto   = input frame F-Main fi-nro-docto no-lock no-error.
   if avail b-mov-es then do:
      run utp/ut-msgs.p (input "show", input 3175, input "").
   end.
   c-estabelec = pallet.cod-estabel.
   assign i-empresa = param-global.empresa-prin WHEN AVAIL param-global.          
   
       find estabelec where
            estabelec.cod-estabel = c-estabelec no-lock no-error.          
       run cdp/cd9970.p (input rowid(estabelec),
                         output i-empresa).
   
   assign c-formato = input frame F-Main c-conta-contabil no-error.
   if  error-status:get-number(1) <> 0 then do:
       assign c-conta-contabil:format in frame F-Main = "x(17)".
   end.
   else do:
       assign c-conta-contabil:format in frame F-Main = param-global.formato-conta-contabil WHEN AVAIL param-global.
   end.
   find first conta-contab where conta-contab.ep-codigo      = i-empresa and
                                 conta-contab.conta-contabil = input frame F-Main c-conta-contabil no-lock no-error.
   if  not available conta-contab then do:
       run utp/ut-msgs.p (input "show", input 1883,input "").
       apply 'entry' to c-conta-contabil in frame F-Main.
       return no-apply.
   end.          
   if  conta-contab.estado <> 3 then do:
       run utp/ut-msgs.p (input "show",input 443,input "").
       apply 'entry' to c-conta-contabil in frame F-Main.
       return no-apply.
   end.              
   if conta-contab.estoque = 4 or conta-contab.estoque = 7 or 
      conta-contab.estoque = 8 or conta-contab.estoque = 9 or 
      conta-contab.estoque = 10                  then do:
        run utp/ut-msgs.p (input "show",input 1884,input "").
        apply 'entry' to c-conta-contabil in frame F-Main.
        return no-apply.
   end.
   if  conta-contab.estoque = 5 then do:
       run utp/ut-msgs.p (input "show",input 8629,input "").
       apply 'entry' to c-conta-contabil in frame F-Main.
       return no-apply.
   end.
   find item where item.it-codigo = pallet.it-codigo no-lock no-error.
   if item.cod-obsoleto = 4 then do:
      run utp/ut-msgs.p (input "show",input 1885,input "").
      return no-apply.
   end.
   if item.it-codigo = "" or item.tipo-contr = 4 then do:
      run utp/ut-msgs.p (input "show",input 1886,input "").
      return no-apply.
   end.
   for each tt-embalagem no-lock:
       find item where item.it-codigo = pallet.it-codigo no-lock no-error.
       if item.cod-obsoleto = 4 then do:
          run utp/ut-msgs.p (input "show",input 17006, input "Item est† obsoleto: " + tt-embalagem.it-codigo).
          return no-apply.
       end.
       if item.it-codigo = "" or item.tipo-contr = 4 then do:
          run utp/ut-msgs.p (input "show",input 17006, input "Item de dÇbito direto: " + tt-embalagem.it-codigo).
          return no-apply.
       end.
   end.
   find first conta-contab where conta-contab.conta-contabil = input frame F-Main c-conta-contabil and
                                 conta-contab.ep-codigo      = i-empresa no-lock no-error.
   run utp/ut-acomp.p persistent set h-acomp.
   run pi-inicializar in h-acomp (input "Montando Pallet").
   run pi-desabilita-cancela in h-acomp.
   blk-do:
   do transaction on error undo blk-do, return no-apply:
       
       run pi-acompanhar in h-acomp (input "Buscando embalagem...").
       /*REQUISICAO*/
       for each tt-embalagem no-lock:
            
           find item where item.it-codigo = tt-embalagem.it-codigo no-lock no-error.
           find item-uni-estab where 
                item-uni-estab.it-codigo   = tt-embalagem.it-codigo and
                item-uni-estab.cod-estabel =  pallet.cod-estabel no-lock no-error.
    
           find first saldo-estoq
                where saldo-estoq.cod-estabel = pallet.cod-estabel
                  and saldo-estoq.it-codigo   = tt-embalagem.it-codigo
                  and saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada   +
                                                 saldo-estoq.qt-aloc-prod + 
                                                 saldo-estoq.qt-aloc-ped) >= tt-embalagem.quantidade no-lock no-error.
                                                 
                                                 
              
           if item-uni-estab.perm-saldo-neg = 1  and not avail saldo-estoq then do:
              run pi-finalizar in h-acomp.
              run utp/ut-msgs.p (input "show":U,
                                 input 17006,
                                 input "Item " + tt-embalagem.it-codigo + " sem saldo!" + "~~" + "Item n∆o possui saldo-em estoque!").
              undo blk-do, return no-apply.
           end.
           IF NOT AVAIL saldo-estoq THEN
               find first saldo-estoq
               where saldo-estoq.cod-estabel = pallet.cod-estabel
                 and saldo-estoq.it-codigo   = tt-embalagem.it-codigo  no-lock no-error.
            if   not avail saldo-estoq then do:
              run pi-finalizar in h-acomp.
              run utp/ut-msgs.p (input "show":U,
                                 input 17006,
                                 input "Item " + tt-embalagem.it-codigo + " sem saldo!" + "~~" + "Item n∆o possui saldo-em estoque!").
              undo blk-do, return no-apply.
           end.
    
           create tt-movto.  
           assign tt-movto.cod-versao-integ = 1
                  tt-movto.dt-trans         = input frame F-Main fi-dt-trans
                  tt-movto.nr-ord-produ     = 0
                  tt-movto.num-ord-inv      = 0
                  tt-movto.nro-docto        = input frame F-Main fi-nro-docto
                  tt-movto.serie-docto      = input frame F-Main fi-serie-docto
                  tt-movto.conta-contabil   = input frame F-Main c-conta-contabil
                  tt-movto.it-codigo        = tt-embalagem.it-codigo
                  tt-movto.cod-estabel      = pallet.cod-estabel
                  tt-movto.cod-depos        = saldo-estoq.cod-depos
                  tt-movto.cod-localiz      = saldo-estoq.cod-localiz
                  tt-movto.cod-refer        = saldo-estoq.cod-refer
                  tt-movto.lote             = saldo-estoq.lote
                  tt-movto.quantidade       = tt-embalagem.quantidade
                  tt-movto.un               = item.un
                  tt-movto.tipo-trans       = 2  /* Saida      */
                  tt-movto.esp-docto        = 28 /* requisicao */
                  tt-movto.cod-prog-orig    = "essf0903"
                  tt-movto.dt-vali-lote     = 12/31/9999
                  tt-movto.op-seq           = 0
                  tt-movto.usuario          = c-seg-usuario. 
           
           if avail conta-contab then do:
              assign tt-movto.ct-codigo = conta-contab.ct-codigo
                     tt-movto.sc-codigo = conta-contab.sc-codigo.
           end.
    
       end.
    
       find conta-contab
            where conta-contab.ep-codigo      = i-empresa
              and conta-contab.conta-contabil = param-estoq.conta-transf no-lock no-error.
    
       run pi-acompanhar in h-acomp (input "Transferindo Lotes...").
       /*TRANSFERENCIA*/
       assign c-pri-lote = ""
              c-pri-refer = "".
       for each it-pallet
          where it-pallet.cod-estabel = pallet.cod-estabel
            and it-pallet.it-codigo   = pallet.it-codigo
            and it-pallet.nr-pallet   = pallet.nr-pallet exclusive-lock:
           
           if c-pri-lote = "" then
              assign c-pri-lote = it-pallet.lote-bobina. 
    
           find item where item.it-codigo = it-pallet.it-codigo no-lock no-error.
           find item-uni-estab
                where item-uni-estab.it-codigo   = it-pallet.it-codigo
                  and item-uni-estab.cod-estabel = pallet.cod-estabel
                  no-lock no-error.
           
           find saldo-estoq
                where saldo-estoq.cod-estabel = pallet.cod-estabel
                  and saldo-estoq.cod-depos   = it-pallet.cod-depos-b
                  and saldo-estoq.cod-localiz = it-pallet.cod-localiz-b
                  and saldo-estoq.lote        = it-pallet.lote-bobina
                  and saldo-estoq.it-codigo   = it-pallet.it-codigo
                  and saldo-estoq.cod-refer   = it-pallet.cod-refer-b no-lock no-error.
           if not avail saldo-estoq then do:
              run pi-finalizar in h-acomp.
              run utp/ut-msgs.p (input "show":U,
                                 input 17006,
                                 input "Saldo n∆o encontrado para o item " + it-pallet.it-codigo).
              undo blk-do, return no-apply.
           end.
    
           /*Saida*/
           create tt-movto.
           assign tt-movto.cod-versao-integracao  = 1
                  tt-movto.ct-codigo              = conta-contab.ct-codigo
                  tt-movto.sc-codigo              = conta-contab.sc-codigo
                  tt-movto.cod-prog-orig          = "essf0903"
                  tt-movto.tipo-trans             = 2
                  tt-movto.esp-docto              = 33
                  tt-movto.conta-contabil         = param-estoq.conta-transf
                  tt-movto.dt-trans               = input frame F-Main fi-dt-trans
                  tt-movto.dt-vali-lote           = 12/31/9999
                  tt-movto.nro-docto              = input frame F-Main fi-nro-docto
                  tt-movto.serie-docto            = input frame F-Main fi-serie-docto
                  tt-movto.cod-depos              = saldo-estoq.cod-depos
                  tt-movto.cod-estabel            = saldo-estoq.cod-estabel
                  tt-movto.it-codigo              = saldo-estoq.it-codigo
                  tt-movto.cod-refer              = saldo-estoq.cod-refer
                  tt-movto.cod-localiz            = saldo-estoq.cod-localiz
                  tt-movto.lote                   = saldo-estoq.lote
                  tt-movto.quantidade             = it-pallet.saldo-bobina
                  tt-movto.un                     = item.un
                  tt-movto.usuario                = c-seg-usuario.
         /*  assign it-pallet.cod-depos-b    = saldo-estoq.cod-depos
                  it-pallet.cod-localiz-b  = saldo-estoq.cod-localiz
                  it-pallet.cod-refer-b    = saldo-estoq.cod-refer
                  it-pallet.dt-vali-lote-b = saldo-estoq.dt-vali-lote.*/
           
           /*Armazena a referància da primeira bobina do pallet*/
           if c-pri-refer = ""
              then assign c-pri-refer = saldo-estoq.cod-refer.
           /*Compara e armazena a data de validade mais antiga das bobinas do palete - Edson */
         
           /* Nova Validade - JosÇ Roberto */
           
           
           if saldo-estoq.dt-vali-lote < dt-vali-jr then
              assign dt-vali-jr = saldo-estoq.dt-vali-lote.
              
           
           /* Fim da Nova Validade */
       end. /*for each it-pallet*/
       /*Entrada - Pallet*/
       create tt-movto.
       assign tt-movto.cod-versao-integracao  = 1
              tt-movto.ct-codigo              = conta-contab.ct-codigo
              tt-movto.sc-codigo              = conta-contab.sc-codigo
              tt-movto.cod-prog-orig          = "essf0903"
              tt-movto.tipo-trans             = 1
              tt-movto.esp-docto              = 33
              tt-movto.conta-contabil         = param-estoq.conta-transf
              tt-movto.dt-trans               = input frame F-Main fi-dt-trans
           
           /* Nova Validade - JosÇ Roberto */
           
    /*          tt-movto.dt-vali-lote           = pallet.data-pallet + 180 /* 6 meses +/- */ */
           
              tt-movto.dt-vali-lote           = dt-vali-jr 
              
           
           /* Fim da Nova Validade */
              
              tt-movto.nro-docto              = input frame F-Main fi-nro-docto
              tt-movto.serie-docto            = input frame F-Main fi-serie-docto
              tt-movto.cod-depos              = "EXP":U
              tt-movto.cod-estabel            = pallet.cod-estabel
              tt-movto.it-codigo              = pallet.it-codigo
              tt-movto.cod-refer              = if pallet.nr-pedido <> 0 then pallet.cod-refer else c-pri-refer
              tt-movto.cod-localiz            = pallet.cod-localiz
              tt-movto.lote                   = pallet.nr-pallet
              tt-movto.quantidade             = pallet.peso-liquido
              tt-movto.un                     = item.un
              tt-movto.usuario                = c-seg-usuario.
    
       run pi-acompanhar in h-acomp (input "Efetivando Transaá∆o...Aguarde...").
       run cep/ceapi001.p (input-output table tt-movto,
                           input-output table tt-erro,
                           input yes).     
    
       FIND FIRST tt-erro NO-LOCK NO-ERROR.
       IF AVAIL tt-erro or return-value = "NOK":U THEN DO:
          run pi-finalizar in h-acomp.
          run cdp/cd0666.w (input table tt-erro).
          undo blk-do, return no-apply.
       END.
       find item where item.it-codigo = pallet.it-codigo  no-lock no-error.
       find first saldo-estoq
            where saldo-estoq.cod-estabel = pallet.cod-estabel
              and saldo-estoq.it-codigo   = pallet.it-codigo
              and saldo-estoq.lote        = pallet.nr-pallet no-lock no-error.
       if avail saldo-estoq then do:
          find first lote-item
               where lote-item.it-codigo = pallet.it-codigo
                 and lote-item.lote      = pallet.nr-pallet no-lock no-error.
          if not avail lote-item then do:
             create lote-item.
             assign lote-item.it-codigo = pallet.it-codigo
                    lote-item.lote      = pallet.nr-pallet.
          end.
          for each lote-carac-tec
             where lote-carac-tec.it-codigo = pallet.it-codigo
               and lote-carac-tec.lote      = c-pri-lote 
               and lote-carac-tec.cd-folh   = item.cd-folh-lote no-lock:
               find first blote-carac-tec
                    where blote-carac-tec.it-codigo = pallet.it-codigo
                      and blote-carac-tec.lote      = pallet.nr-pallet
                      and blote-carac-tec.cd-folh   = lote-carac-tec.cd-folh
                      and blote-carac-tec.cd-comp   = lote-carac-tec.cd-comp exclusive-lock no-error.
               if not avail blote-carac-tec then do:
                  create blote-carac-tec.
               end.
               buffer-copy lote-carac-tec except lote to blote-carac-tec. 
               assign blote-carac-tec.lote      = pallet.nr-pallet.
               for each lote-res-carac
                  where lote-res-carac.it-codigo = pallet.it-codigo
                    and lote-res-carac.lote      = c-pri-lote
                    and lote-res-carac.cd-folh   = lote-carac-tec.cd-folh
                    and lote-res-carac.cd-comp   = lote-carac-tec.cd-comp no-lock:
                    find first blote-res-carac
                         where blote-res-carac.it-codigo = pallet.it-codigo
                           and blote-res-carac.lote      = pallet.nr-pallet
                           and blote-res-carac.cd-folh   = lote-res-carac.cd-folh
                           and blote-res-carac.cd-comp   = lote-res-carac.cd-comp
                           and blote-res-carac.nr-tabela = lote-res-carac.nr-tabela
                           and blote-res-carac.sequencia = lote-res-carac.sequencia exclusive-lock no-error.
                    if not avail blote-res-carac then do:
                       create blote-res-carac.
                    end.
                    buffer-copy lote-res-carac except lote to blote-res-carac. 
                    assign blote-res-carac.lote      = pallet.nr-pallet.
               end.
          end.
       end.
       /*Palete Confirmado*/
       find current pallet exclusive-lock no-error.
       assign pallet.situacao    = 2
              pallet.nro-docto   = input frame F-Main fi-nro-docto   
              pallet.serie-docto = input frame F-Main fi-serie-docto
              pallet.data-pallet = input frame F-Main fi-dt-trans.
       /*Se o palete foi produzido para estoque armazena a referencia da primeira bobina*/
       if pallet.nr-pedido = 0 then
          assign pallet.cod-refer   = c-pri-refer.
           
   end.
   run pi-finalizar in h-acomp.
   assign p-ok = yes.
   apply "close":U to this-procedure.
END.
/* _UIB-CODE-BLOCK-END */
ON ENTRY OF c-conta-contabil IN FRAME F-Main
DO:
    assign c-conta-contabil:format in frame F-Main = param-global.formato-conta-contabil WHEN AVAIL param-global.
END.
/* _UIB-CODE-BLOCK-END */
ON F5 OF c-conta-contabil IN FRAME F-Main
DO:
    IF NOT AVAIL estabelec THEN DO:
        FIND FIRST estabelec WHERE estabelec.cod-estabel = c-estabelec NO-LOCK NO-ERROR.
    END.
   find first param-global no-lock no-error.
   assign i-ep-codigo-usuario = param-global.empresa-prin WHEN AVAIL param-global.
   assign i-ep-codigo-usuario = estabelec.ep-codigo WHEN AVAIL estabelec.
 
 /*  if input frame {&frame-name} i-reduzida = 0 then do:*/
      
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
      wh-pesquisa:FILE-NAME = "adzoom/z01ad049.w":U then
        return.
      
  RUN adzoom/z01ad049.w persistent set wh-pesquisa.
  if  not valid-handle(wh-pesquisa) or 
          wh-pesquisa:TYPE <> "PROCEDURE":U or 
          wh-pesquisa:FILE-NAME <> "adzoom/z01ad049.w":U then
      return.
      
  run pi-seta-est in wh-pesquisa (input c-estabelec).
  
  RUN dispatch IN wh-pesquisa ('initialize':U).
  
  if valid-handle(wh-pesquisa) and
     wh-pesquisa:TYPE = "PROCEDURE":U and
     wh-pesquisa:FILE-NAME = "adzoom/z01ad049.w":U then do:
    
        RUN pi-entry IN wh-pesquisa.
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(c-conta-contabil:handle in frame F-Main) + '|':U + 'conta-contabil'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    
    
  end.
                    
assign l-implanta = no.
/* _UIB-CODE-BLOCK-END */
 
/*   end.*/
END.
/* _UIB-CODE-BLOCK-END */
ON LEAVE OF c-conta-contabil IN FRAME F-Main
DO:  
 assign c-formato = input frame F-Main c-conta-contabil no-error.
 if  error-status:get-number(1) <> 0 then do:
     assign c-conta-contabil:format in frame F-Main = "x(17)".
 end.
 else do:
     assign c-conta-contabil:format in frame F-Main = param-global.formato-conta-contabil WHEN AVAIL param-global.
 end.             
 if l-flag then do :
    do with frame F-Main:
        assign
            c-conta-contabil.
        assign i-empresa = param-global.empresa-prin WHEN AVAIL param-global.
          
        
          
            find estabelec where
                 estabelec.cod-estabel = c-estabelec no-lock no-error.
      
            run cdp/cd9970.p (input rowid(estabelec),
                              output i-empresa).
        
            
        find conta-contab 
            where conta-contab.ep-codigo    = i-empresa
              and conta-contab.conta-contab = c-conta-contabil 
            no-lock no-error.
 
        if avail conta-contab then do with frame F-Main:
            disable i-reduzida.
                    /*movto-estoq.nr-ord-prod.*/
 
            display conta-contab.reduzida       @ i-reduzida
                    conta-contab.conta-contabil @ c-conta-contabil
                    conta-contab.titulo         @ c-titulo-conta.
                    /*0                           @ movto-estoq.nr-ord-produ.*/
        end.
        else do:
            display 0  @ i-reduzida
                    "" @ c-titulo-conta.
 
          if avail empresa and empresa.usa-reduzida = yes
          then enable i-reduzida.
 
          IF AVAIL param-global AND param-global.modulo-cp = no and 
             param-global.modulo-mi = no
          then.
          else enable movto-estoq.nr-ord-produ WITH FRAME f-main.
        end.
    end.
 end.   
END.
/* _UIB-CODE-BLOCK-END */
ON MOUSE-SELECT-DBLCLICK OF c-conta-contabil IN FRAME F-Main
DO:
      apply 'F5' to self.
END.
/* _UIB-CODE-BLOCK-END */
ON F5 OF i-reduzida IN FRAME F-Main /* Conta Reduzida */
DO:
    IF NOT AVAIL estabelec THEN DO:
        FIND FIRST estabelec WHERE estabelec.cod-estabel = c-estabelec NO-LOCK NO-ERROR.
     END.
    find first param-global no-lock no-error.
    assign i-emp050 = param-global.empresa-prin WHEN AVAIL param-global.
    assign i-emp050 = estabelec.ep-codigo WHEN AVAIL estabelec.
    
   assign  
          l-implanta = no.
    
   find first param-global no-lock no-error.
   assign i-ep-codigo-usuario = estabelec.ep-codigo WHEN AVAIL estabelec
          l-implanta          = yes.
 
   
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
      wh-pesquisa:FILE-NAME = "adzoom/z01ad049.w":U then
        return.
      
  RUN adzoom/z01ad049.w persistent set wh-pesquisa.
  if  not valid-handle(wh-pesquisa) or 
          wh-pesquisa:TYPE <> "PROCEDURE":U or 
          wh-pesquisa:FILE-NAME <> "adzoom/z01ad049.w":U then
      return.
      
  run pi-seta-est in wh-pesquisa (input c-estabelec).
  
  RUN dispatch IN wh-pesquisa ('initialize':U).
  
  if valid-handle(wh-pesquisa) and
     wh-pesquisa:TYPE = "PROCEDURE":U and
     wh-pesquisa:FILE-NAME = "adzoom/z01ad049.w":U then do:
    
        RUN pi-entry IN wh-pesquisa.
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(i-reduzida:handle in frame F-Main) + '|':U + 'reduzida'.
        
        
        
        
        
        
        
        
        
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
ON LEAVE OF i-reduzida IN FRAME F-Main /* Conta Reduzida */
DO:
  if l-flag then do with frame F-Main:
        assign i-empresa = param-global.empresa-prin WHEN AVAIL param-global
               c-conta-contabil:format in frame F-Main = param-global.formato-conta-contabil WHEN AVAIL param-global.
        
            find estabelec where
                 estabelec.cod-estabel = c-estabelec no-lock no-error.
            run cdp/cd9970.p (input rowid(estabelec),
                              output i-empresa).
        
        find conta-contab use-index reduzida
            where conta-contab.ep-codigo = i-empresa
              and conta-contab.reduzida  = input i-reduzida
            no-lock no-error.
        if avail conta-contab then do:
            disable c-conta-contabil movto-estoq.nr-ord-produ.
            display conta-contab.conta-contabil @ c-conta-contabil
                    conta-contab.titulo         @ c-titulo-conta.
                    /*0                           @ movto-estoq.nr-ord-produ.*/
        end.
        else do:
           if AVAIL param-global AND param-global.modulo-cp = no and
              param-global.modulo-mi = no
           then. 
           else do:
              enable c-conta-contabil movto-estoq.nr-ord-produ.
              display "" @ c-conta-contabil
                      "" @ c-titulo-conta.
           end.           
        end.
    end.        
    
END.
/* _UIB-CODE-BLOCK-END */
ON MOUSE-SELECT-DBLCLICK OF i-reduzida IN FRAME F-Main /* Conta Reduzida */
DO:
  apply 'F5' to self.
END.
/* _UIB-CODE-BLOCK-END */
/* ***************************  Main Block  *************************** */
i-reduzida:load-mouse-pointer("image/lupa.cur") in frame F-Main.
c-conta-contabil:load-mouse-pointer("image/lupa.cur") in frame F-Main.
ON 'f5':U OF tt-embalagem.it-codigo in browse BR-EMBALAGEM or
   'mouse-select-dblclick':U OF tt-embalagem.it-codigo in browse BR-EMBALAGEM
DO:
   
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
      wh-pesquisa:FILE-NAME = "inzoom/z01in172.w":U then
        return.
      
  RUN inzoom/z01in172.w persistent set wh-pesquisa.
  if  not valid-handle(wh-pesquisa) or 
          wh-pesquisa:TYPE <> "PROCEDURE":U or 
          wh-pesquisa:FILE-NAME <> "inzoom/z01in172.w":U then
      return.
      
  
  
  RUN dispatch IN wh-pesquisa ('initialize':U).
  
  if valid-handle(wh-pesquisa) and
     wh-pesquisa:TYPE = "PROCEDURE":U and
     wh-pesquisa:FILE-NAME = "inzoom/z01in172.w":U then do:
    
      
    
        
            define variable c-lista-campo as char init '' no-undo.
        
        assign c-lista-campo = string(tt-embalagem.it-codigo:handle in browse br-embalagem) + '|':U + 'it-codigo'.
        
        
        
        
        
        
        
        
        
        run pi-seta-atributos-chave in wh-pesquisa (c-lista-campo).
    RUN add-link IN adm-broker-hdl
                   (INPUT wh-pesquisa,
                    INPUT 'State':U,
                    INPUT this-procedure).
    
    
  end.
                    
assign l-implanta = no.
wait-for close of wh-pesquisa.
/* _UIB-CODE-BLOCK-END */
  
    RETURN.
END.
ON 'leave':U OF tt-embalagem.it-codigo in browse br-embalagem 
DO:
    if avail tt-embalagem then do:
       disp tt-embalagem.it-codigo
            tt-embalagem.quantidade
           with browse br-embalagem. 
    end.
    RETURN.
END.
/* Include custom  Main Block code for SmartWindows. */
/* windowmn.i - Main Block code for objects which create windows.*/
/* Skip all of this if no window was created. */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
IF VALID-HANDLE(w-window) THEN DO:
    ASSIGN CURRENT-WINDOW                = w-window 
       w-window:KEEP-FRAME-Z-ORDER = YES
       THIS-PROCEDURE:CURRENT-WINDOW = w-window.
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
       FIND FIRST param-globa NO-LOCK NO-ERROR.
       find first pallet where rowid(pallet) = p-rw-pallet no-lock no-error.
       c-estabelec = pallet.cod-estabel.
       assign i-empresa = param-global.empresa-prin WHEN AVAIL param-global.          
   
       find estabelec where
            estabelec.cod-estabel = c-estabelec no-lock no-error.          
       run cdp/cd9970.p (input rowid(estabelec),
                         output i-empresa).
       
       IF RETURN-VALUE = 'ADM-ERROR':U THEN RETURN.
       
       IF NOT THIS-PROCEDURE:PERSISTENT THEN
           WAIT-FOR CLOSE OF THIS-PROCEDURE.
    END.
END.
END.
 
/* _UIB-CODE-BLOCK-END */
/* **********************  Internal Procedures  *********************** */
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
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
  DISPLAY fi-dt-trans fi-nro-docto fi-serie-docto i-reduzida c-conta-contabil 
          c-titulo-conta 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE /*bt-modifica*/ fi-nro-docto fi-serie-docto br-embalagem /*bt-inclui 
         bt-elimina */ bt-ok bt-cancelar bt-ajuda RECT-1 RECT-2 RECT-3 
      WITH FRAME F-Main IN WINDOW w-window.
  OPEN QUERY br-embalagem FOR EACH tt-embalagem.
  VIEW w-window.
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
** I-LOGFIN.I - Encerra o Log de Execuªío
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
/* Eliminaªío de arquivos temporˇrios */
/* Fim da eliminaªío de arquivos temporˇrios */
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
  /***********************************************************************
**
**  WIN-SIZE.I - Realiza o ajuste no tamanho da window e da frame
**               igualando ambos
*************************************************************************/
if w-window:width-chars < frame F-Main:width-chars then
    assign frame F-Main:width-chars = w-window:width-chars.
else if frame F-Main:width-chars < w-window:width-chars then
    assign w-window:width-chars = frame F-Main:width-chars.
if w-window:height-chars < frame F-Main:height-chars then
    assign frame F-Main:height-chars = w-window:height-chars.
else if frame F-Main:height-chars < w-window:height-chars then
    assign w-window:height-chars = frame F-Main:height-chars.
assign w-window:virtual-width-chars  = w-window:width-chars
       w-window:virtual-height-chars = w-window:height-chars
       w-window:min-width-chars      = w-window:width-chars
       w-window:max-width-chars      = w-window:width-chars
       w-window:min-height-chars     = w-window:height-chars
       w-window:max-height-chars     = w-window:height-chars.
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
    assign c-programa-mg97 = caps("essf0903C")
           c-versao-mg97   = "2.00.00.000".
    find prog_dtsul no-lock
        where prog_dtsul.cod_prog_dtsul = c-programa-mg97 no-error.
    if  avail prog_dtsul then do:
        assign c-titulo-prog-mg97    = prog_dtsul.des_prog_dtsul
               c-nom-prog-upc-mg97   = prog_dtsul.nom_prog_upc
               c-nom-prog-appc-mg97  = prog_dtsul.nom_prog_appc
               c-nom-prog-dpc-mg97   = prog_dtsul.nom_prog_dpc
               i-num-topico-hlp-mg97 = prog_dtsul.num_topico.
       /*
          if session:window-system <> "TTY" then 
            assign i-template          = prog_dtsul.ind_template.
       */
        
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
     
    
         assign w-window:title = if l-achou-prog then
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
      if "JanelaDetalhe" = "SmartDialog" or this-procedure:persistent = no then
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
      if "JanelaDetalhe" = "SmartDialog" then
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
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.
        end.
        when 3 then do: /* Cadastro Simples - Alteracao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.
        end.
        when 4 then do: /* Cadastro Complexo */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 5 then do: /* Cadastro Complexo - Alteracao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 6 then do: /* Cadastro Simples - Inclusao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 7 then do: /* Cadastro PaiXFilho - Atualiza Filho */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 8 then do: /* Cadastro PaiXFilho - Atualiza Ambos */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
            
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 9 then do: /* Inclui/Modifica Pai */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 10 then do: /* Inclui/Modifica Filho */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 11 then do: /* Formacao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 12 then do: /* Parametros Unicos */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 13 then do: /* Pesquisa */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 14 then do: /* Consulta Simples */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 15 then do: /* Consulta Complexa */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 16 then do: /* Consulta Relacionamento */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 17 then do: /* Relatorio */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 20 then do: /* Janela Detalhe */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD.
            if  h-pai:handle = w-window:handle then
                assign h-pai           = h-pai:NEXT-SIBLING
                       h-pai:SENSITIVE = no.
            else 
                assign  h-pai = w-window:handle
                        h-pai = h-pai:parent.
            h-pai:sensitive = no.
  
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 21 then do: /* Janela Mestre */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.        
        end.
        when 26 then do: /* Importacao/Exportacao */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.
        end.
        when 29 then do: /* Relatorio Gerado Pelo DataViewer */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.
        end.
        when 30 then do: /* Formacao Sem Navegacao */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.
        end.
        when 31 then do: /* Estrutura */
            /*** Ocultar MAX - MIN ***/
            run utp/ut-style.p persistent set h-prog.
            run DeleteMinMaxButtons in h-prog(input w-window:hWnd).
            delete procedure h-prog.
    
            /*** Tornar Window Modal ***/
            assign h-pai           = SESSION:FIRST-CHILD
                   h-pai           = h-pai:NEXT-SIBLING
                   h-pai:SENSITIVE = no.
    
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.
        end.
        when 32 then do: /* Digitaá∆o Rapida */
            /*** Desabilitar MAX - RESIZE ***/
            run utp/ut-style.p persistent set h-prog.
            run DisableMaxButton in h-prog(input w-window:hWnd).
            delete procedure h-prog.
            
            assign w-window:HIDDEN = yes
            w-window:HIDDEN = no.
            apply "ENTRY":U to w-window.
        end.
    end case.
/* Transformacao Window *****************************************************/
 
      end. 
    
/* ut9000.i */
 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  /* Code placed here will execute AFTER standard behavior.    */
  run pi-add-record.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
PROCEDURE pi-add-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  find first param-global no-lock no-error.
  if not avail param-global then do:
     run utp/ut-msgs.p (input "show",
                       input 16,
                       input "").
     return no-apply.
  end.
 
  enable i-reduzida 
         c-conta-contabil with frame F-Main.
  find estabelec where
                estabelec.cod-estabel = c-estabelec no-lock no-error.
           run cdp/cd9970.p (input rowid(estabelec),
                             output i-empresa).
 
  IF AVAIL param-global THEN
      find first empresa where empresa.ep-codigo = i-empresa no-lock no-error.
  if avail empresa and empresa.usa-reduzida = no
  then
      disable i-reduzida with frame F-Main.
 
  find first param-estoq no-lock no-error.
  if not avail param-estoq then do:
     run utp/ut-msgs.p (input "show",
                       input 1059,
                       input "").
     return no-apply.
  end.
 
  find first param-cq no-lock no-error.
 
  /*disp today @ fi-dt-trans with frame {&frame-name}.*/
  find first pallet where rowid(pallet) = p-rw-pallet no-lock no-error.
  /*alterado 26/02/2004 - solicitado por Edson - FO 996.814*/
  disp pallet.data-pallet @ fi-dt-trans with frame F-Main.
  if pallet.nr-pedido <> 0 then do:
      find first ped-venda
           where ped-venda.nr-pedido = pallet.nr-pedido no-lock no-error.
      if not avail ped-venda then do:
         run utp/ut-msgs.p (input "show",
                            input 2,
                            input "Pedido":U).
         return no-apply.
      end.
    
      find last  ped-item
           where ped-item.nome-abrev = ped-venda.nome-abrev
             and ped-item.nr-pedcli  = ped-venda.nr-pedcli
             and ped-item.it-codigo  = pallet.it-codigo no-lock no-error.
      if not avail ped-item then do:
         run utp/ut-msgs.p (input "show",
                            input 2,
                            input "Item do Pedido":U).
         return no-apply.
      end.
      
      find first var-result
           where var-result.item-cotacao = ped-item.it-codigo
             and var-result.nr-estrut    = ped-item.nr-config
             and var-result.nome-var     = "CODEMBAL":U no-lock no-error.
     
      if not avail var-result then do:
         run utp/ut-msgs.p (input "show":U,
                            input 17006,
                            input "Embalagem n∆o encontradas!").
         return no-apply.
      end.
    /* Nova Rotina de embalagem */
      for each tt-embalagem:
          delete tt-embalagem.
      end.
      IF INT (VAR-resul.valor-char) < 999999 /*OR 
         INT (VAR-resul.valor-char) > 9999999 */
          THEN DO:
       
         for each estrutura
            where estrutura.it-codigo = var-result.valor-char and estrutura.it-codigo <> "" no-lock:
             find b-item
                  where b-item.it-codigo = estrutura.es-codigo no-lock no-error.
             create tt-embalagem.
             assign tt-embalagem.it-pai     = estrutura.it-codigo
                    tt-embalagem.it-codigo  = estrutura.es-codigo
                    tt-embalagem.descricao  = if avail b-item then b-item.desc-item else "":U
                    tt-embalagem.quantidade = estrutura.quant-usada.
         end.
      END.
      ELSE DO:
            FIND FIRST polo-embalagem WHERE
                polo-embalagem.cod-embal = INT (VAR-resul.valor-char)
                NO-LOCK NO-ERROR.
        
            IF AVAIL polo-embalagem THEN DO:
            
               FOR EACH polo-embalagem-estrut NO-LOCK where
                        polo-embalagem-estrut.cod-estabel =  polo-embalagem.cod-estabel AND
                        polo-embalagem-estrut.cod-mercado =  polo-embalagem.cod-mercado AND
                        polo-embalagem-estrut.cod-embal   =  polo-embalagem.cod-embal :
                
                   find b-item
                        where b-item.it-codigo = polo-embalagem-estrut.it-codigo no-lock no-error.
                   create tt-embalagem.
                   assign tt-embalagem.it-pai     = STRING (polo-embalagem.cod-embal)
                          tt-embalagem.it-codigo  = polo-embalagem-estrut.it-codigo
                          tt-embalagem.descricao  = if avail b-item then b-item.desc-item else "":U.
                   IF polo-embalagem-estrut.tipo-cons = "P" THEN
                      ASSIGN tt-embalagem.quantidade = polo-embalagem-estrut.quantidade.
                   ELSE
                      ASSIGN tt-embalagem.quantidade = (polo-embalagem-estrut.quantidade *
                                    pallet.nr-bobinas).
               END.
            END.
      END.
  end.
  else do:
  
        c-codembal = pallet.cod-embal.
        
        if c-codembal = "" then do:
        
        
            for each it-pallet
                     where it-pallet.cod-estabel = pallet.cod-estabel
                       and it-pallet.it-codigo   = pallet.it-codigo
                       and it-pallet.nr-pallet   = pallet.nr-pallet exclusive-lock:
                      
                       find first var-result
                            where var-result.item-cotacao = pallet.it-codigo
                              and var-result.nr-estrut    = int(it-pallet.cod-refer-b)
                              and var-result.nome-var     = "CODEMBAL":U no-lock no-error.
                      
                              
                       if  avail var-result then do:
                          c-codembal = VAR-resul.valor-char.
                          leave.
                       end.
             
             end.    
             
         end.  
  
  
      if c-codembal <> "":U then do:
         for each tt-embalagem:
             delete tt-embalagem.
         end.
      
       IF INT (c-codembal) < 999999 /*OR 
          INT (c-codembal) > 9999999  */
          THEN DO:            
          for each estrutura
            where estrutura.it-codigo = C-CODEMBAL no-lock:
             find b-item
                  where b-item.it-codigo = estrutura.es-codigo no-lock no-error.
             create tt-embalagem.
             assign tt-embalagem.it-pai     = estrutura.it-codigo
                    tt-embalagem.it-codigo  = estrutura.es-codigo
                    tt-embalagem.descricao  = if avail b-item then b-item.desc-item else "":U
                    tt-embalagem.quantidade = estrutura.quant-usada.
          end.
       END.
       ELSE DO:
           FIND FIRST polo-embalagem WHERE
               polo-embalagem.cod-embal = INT (c-codembal)
               NO-LOCK NO-ERROR.
           IF AVAIL polo-embalagem THEN DO:
              FOR EACH polo-embalagem-estrut NO-LOCK where
                       polo-embalagem-estrut.cod-estabel =  polo-embalagem.cod-estabel AND
                       polo-embalagem-estrut.cod-mercado =  polo-embalagem.cod-mercado AND
                       polo-embalagem-estrut.cod-embal   =  polo-embalagem.cod-embal :
                  find b-item
                       where b-item.it-codigo = polo-embalagem-estrut.it-codigo no-lock no-error.
                  create tt-embalagem.
                  assign tt-embalagem.it-pai     = STRING (polo-embalagem.cod-embal)
                         tt-embalagem.it-codigo  = polo-embalagem-estrut.it-codigo
                         tt-embalagem.descricao  = if avail b-item then b-item.desc-item else "":U.
                  IF polo-embalagem-estrut.tipo-cons = "P" THEN
                     ASSIGN tt-embalagem.quantidade = polo-embalagem-estrut.quantidade.
                  ELSE
                     ASSIGN tt-embalagem.quantidade = (polo-embalagem-estrut.quantidade *
                                   pallet.nr-bobinas).
              END.
           END.
       END.
      end.
  end.
  
  if pallet.cod-estabel = "413":U OR pallet.cod-estabel = "423":U then /*solic-318*/
     assign c-conta-contabil = "44589944113".
  if pallet.cod-estabel = "412":U OR pallet.cod-estabel = "422":U then /*solic-318*/
     assign c-conta-contabil = "44589944113".
  assign fi-nro-docto = pallet.nr-pallet.
  disp fi-nro-docto c-conta-contabil with frame F-Main.
/* N«O VAI PEDIR MAIS ITENS DE EMBALAGENS QUANDO FOR REPALETIZAÄ«O - EDSON - 27/03/2006*/
  for each tt-embalagem WHERE substring(tt-embalagem.it-codigo,LENGTH(tt-embalagem.it-codigo),1) <> "7":
               c-emb-it-codigo = substring(tt-embalagem.it-codigo,1,LENGTH(tt-embalagem.it-codigo) - 1) + "7".
          
               find item where item.it-codigo = c-emb-it-codigo no-lock no-error.
               IF NOT AVAIL ITEM THEN NEXT.
               find first saldo-estoq
                   where saldo-estoq.cod-estabel = pallet.cod-estabel
                     and saldo-estoq.it-codigo   = c-emb-it-codigo AND
                         saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada   +
                                                            saldo-estoq.qt-aloc-prod + 
                                                            saldo-estoq.qt-aloc-ped) > 0
                     no-lock no-error.
               IF NOT AVAIL saldo-estoq THEN NEXT.
               d-emb-saldo =  saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada   +
                                                    saldo-estoq.qt-aloc-prod + 
                                                    saldo-estoq.qt-aloc-ped) .
              
              IF d-emb-saldo > tt-embalagem.quantidade THEN DO:
                  d-emb-saldo = tt-embalagem.quantidade.
              END.
             
              tt-embalagem.quantidade = tt-embalagem.quantidade - d-emb-saldo.
              IF d-emb-saldo > 0 THEN DO:
                  CREATE b-tt-embalagem.
                  BUFFER-COPY tt-embalagem EXCEPT it-codigo quantidade TO b-tt-embalagem 
                      ASSIGN b-tt-embalagem.it-codigo  =  c-emb-it-codigo
                             b-tt-embalagem.quantidade = d-emb-saldo .
              END.
              
 
            IF tt-embalagem.quantidade = 0 THEN delete tt-embalagem.
       end.
/*
  IF can-find(FIRST movto-estoq WHERE
     movto-estoq.cod-estabel = pallet.cod-estabel AND
     movto-estoq.lote        = pallet.nr-pallet   AND
     movto-estoq.cod-depos   = "EXP"              AND
     movto-estoq.it-codigo   = pallet.it-codigo   AND
     movto-estoq.tipo-trans  = 1                  AND 
     movto-estoq.esp-docto   = 33 NO-LOCK) THEN 
     DO:
      
      for each tt-embalagem:
           
                 delete tt-embalagem.
           end.
       
     END.
  
  */
    IF SUBSTRING(pallet.nr-pallet,1,2)  = "PT" THEN  DO:
           for each tt-embalagem:
           
                 delete tt-embalagem.
           end.
    END.
  open query br-embalagem for each tt-embalagem.
  
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
    WHEN "tt-embalagem":U THEN p-rowid-list = p-rowid-list + 
        IF AVAILABLE tt-embalagem THEN STRING(ROWID(tt-embalagem))
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
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
