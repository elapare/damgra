/* Connected Databases 
          movmfg           PROGRESS
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

def buffer operacao for mgemp.operacao .

def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[2.00.00.000[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "2.00.00.000"
       c-prg-obj = "ESSF0303".
if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "ESSF0303"
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
    put "ESSF0303" at 1 "2.00.00.000" at 69 today at 84 string(time,'HH:MM:SS':U) at 94 skip.
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
/*****************************************************************************
** I-VRTAB.I - Define Vari†veis Globais Rowid de Tabela 
** {1} - tabela
*****************************************************************************/
def new global shared var gr-area-produc-ctrab     as rowid no-undo.

/* i-dfvrgl.i */
 

def var rw-area-produc-ctrab as rowid no-undo.

/*****************************************************************************
** I-VRTAB.I - Define Vari†veis Globais Rowid de Tabela 
** {1} - tabela
*****************************************************************************/
def new global shared var gr-oper-ord     as rowid no-undo.

/* i-dfvrgl.i */
 

def temp-table tt-itens-reporte
    field it-codigo    like item.it-codigo
    field desc-item    like item.desc-item
    field quantidade   like movto-estoq.quantidade   column-label "Quantidade ACA"
    field cod-depos    like saldo-estoq.cod-depos    column-label "Depos." 
    field localizacao  like saldo-estoq.cod-localiz  column-label "Localizaá∆o" 
    field lote         like saldo-estoq.lote         column-label "Lote       "   
    field val-lote     like saldo-estoq.dt-vali-lote init 12/31/9999 column-label "Dt.Val.Lote".

def buffer b-tt-itens-reporte for tt-itens-reporte.

/*** Esta include no ems 2.01 n∆o dever† possuir nenhum c¢digo, serve 
     apenas para desenvolvimento tratar o conceito de miniflexibilizaá∆o.
     Utilizado apenas para MANUFATURA. ***/

/*** RELEASE 2.02 ***/

/* Funá‰es ch∆o f†brica e lista de componentes 2.02 - Logoplaste - Manufatura */


/*** RELEASE 2.03 ***/

/* Integraá∆o Magnus x SFC 2.03 - Klimmek - Manufatura *//* Relacionamento Linha Produá∆o x Estabelecimento     *//* Transaá∆o Reporte Ass°ncrono                        *//* Alteraá‰es Gerais EMS 2.03                          */

/*** RELEASE 2.04 ***/

/* Alteraá‰es Gerais EMS 2.04                          *//* Integraá∆o EAI                                     */ 
/***   Include para os PrÇ-Processadores do Aplicativo de Distribuiá∆o    ***/
/*** Serve para o desenvolvimento tratar o conceito de miniflexibilizaá∆o ***/ 

/*** Funcoes de Uso Geral ***/
/* Utilizado para Teste de Release */
/*** Funcoes Liberadas na 2.02 ***/
/* Recebimento Fisico *//* Unidade de Negocio *//* Verifica Controle de Verba no Investimento *//* Melhorias em Controle de Contratos *//* Melhorias da Aprovacao Eletronica *//* Desatualizacao AP *//* Conciliacao Transitoria Fornecedores *//* Consulta Multi-Moeda em Controle Contratos *//* Conversao da Tabela ext-ord-per da 2.01 p/ 2.02 *//* Tipo de Ressuprimento de Estoque *//* Consumo e Estatistica por Estabelec */
/* novo campo cod-tax especifico para compras no item-mat *//* Conta Cont†bil Investimentos */
/*** Funcoes Liberadas na 2.03 ***/
/* Fator Multiplicativo/Divisivo Conversao Moedas *//* Ident Imposto do Item Conforme a Natureza *//* Inclusao Impostos nas Despesas da Nota (MAT e DIS) *//* Selecao Estabelecimentos no Recebimento *//* Contas Transitorias por Estabelecimento *//* Fechamento por Estabelecimento *//* Componentes Usados em Certificado *//* Tipo de Compra na Natureza de Operacao para Recebimento *//* Tratamento de Multiplas Referencias por Lote *//* Estorno Devolucao AP/CR *//* Consumo e Estatistica por Estabelec - Fase II *//* Especie Fiscal no Recebimento *//* Operacao Triangular - Materiais *//* Rateio Despesas Recebimento Internacional *//* Reporte Automatico do Acabado *//* Melhorias em Contratos (Permissoes, Aprovacao, etc...) *//* ParÉmetros Item/Fam°lia por Estabelecimento */
/*** Funcoes Pendentes ***/
/*&glob bf_mat_devol_cli_inter   yes   /* Devolucao de Cliente do Internacional */*/
/*&glob bf_mat_estorno_cr        yes   /* Estorno Contas a Receber */*/
/*&glob bf_mat_custo_on_line     yes   /* Custo On-line */*/
 
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
 

{cdp/cdcfgman.i}
{cdp/cdcfgmat.i}
{cpp\cpapi001.i}

/**********************************************************************************
**
** Include - CPAPI301.I
** Funá∆o  - Definiá∆o das temp-tables
**
***********************************************************************************/

def temp-table tt-ord-prod      like ord-prod use-index codigo
    field dt-disponibilidade    as date
    field ind-tipo-movto        as integer
    field faixa-numeracao       as integer init 1
    field verifica-compras      as logical 
    field aloca-reserva         as logical init ?
    field aloca-lote            as logical init ?
    field rw-ord-prod           as rowid
    field gera-relacionamentos  as logical init yes
    
    field gera-reservas         as logical init yes
    
    field prog-seg              as char
    field seg-usuario           as char
    field ep-codigo-usuario     as integer
    field cod-versao-integracao as integer format "999"
    field considera-dias-desl   as logical init no.

def temp-table tt-reapro
    field it-codigo         like ord-prod.it-codigo
    field cod-refer         like ord-prod.cod-refer
    field descricao         as char format "x(36)"
    field un                like reservas.un
    field quant-orig        like reservas.quant-orig.
 
/**************************************************************************
**
**   ceapi001.i - Include de definicoes da temp-table e variaveis 
**
**                da API ceapi001.p
**
**************************************************************************/

def temp-table tt-movto  
    FIELD cod-versao-integracao AS INTEGER FORMAT "999"
    FIELD cod-prog-orig         LIKE movto-estoq.cod-prog-orig
    FIELD l-mov-erro            AS LOGICAL INITIAL NO
    FIELD r-mov-inv             AS ROWID    
    FIELD r-mov-orig            AS ROWID /*registro original para valorizar o estorno, devoluá∆o,retorno*/
    FIELD sequen-nf             LIKE movto-estoq.sequen-nf
    FIELD cod-depos             LIKE movto-estoq.cod-depos
    FIELD cod-emitente          LIKE movto-estoq.cod-emitente
    FIELD cod-estabel           LIKE movto-estoq.cod-estabel
    FIELD cod-refer             LIKE movto-estoq.cod-refer
    FIELD ct-codigo             LIKE movto-estoq.ct-codigo
    FIELD descricao-db          LIKE movto-estoq.descricao-db
    FIELD dt-nf-saida           LIKE movto-estoq.dt-nf-saida
    FIELD dt-trans              LIKE movto-estoq.dt-trans
    FIELD esp-docto             LIKE movto-estoq.esp-docto
    FIELD it-codigo             LIKE movto-estoq.it-codigo
    FIELD cod-localiz           LIKE movto-estoq.cod-localiz
    FIELD lote                  LIKE movto-estoq.lote
    FIELD nat-operacao          LIKE movto-estoq.nat-operacao
    FIELD nro-docto             LIKE movto-estoq.nro-docto
    FIELD num-sequen            LIKE movto-estoq.num-sequen
    FIELD numero-ordem          LIKE movto-estoq.numero-ordem
    FIELD nr-ord-produ          LIKE movto-estoq.nr-ord-produ
    FIELD peso-liquido          LIKE movto-estoq.peso-liquido
    FIELD quantidade            LIKE movto-estoq.quantidade
    FIELD referencia            LIKE movto-estoq.referencia
    FIELD sc-codigo             LIKE movto-estoq.sc-codigo
    FIELD serie-docto           LIKE movto-estoq.serie-docto
    FIELD tipo-preco            LIKE movto-estoq.tipo-preco
    FIELD tipo-trans            LIKE movto-estoq.tipo-trans
    FIELD tipo-valor            LIKE movto-estoq.tipo-valor
    FIELD un                    LIKE movto-estoq.un         
    FIELD valor-mat-m           LIKE movto-estoq.valor-mat-m
    FIELD valor-mat-o           LIKE movto-estoq.valor-mat-o
    FIELD valor-mat-p           LIKE movto-estoq.valor-mat-p
    FIELD valor-mob-m           LIKE movto-estoq.valor-mob-m
    FIELD valor-mob-o           LIKE movto-estoq.valor-mob-o
    FIELD valor-mob-p           LIKE movto-estoq.valor-mob-p
    FIELD valor-ggf-m           LIKE movto-estoq.valor-ggf-m
    FIELD valor-ggf-o           LIKE movto-estoq.valor-ggf-o
    FIELD valor-ggf-p           LIKE movto-estoq.valor-ggf-p
    FIELD valor-nota            LIKE movto-estoq.valor-nota
    FIELD vl-nota-fasb          LIKE movto-estoq.vl-nota-fasb
    FIELD nr-ord-refer          LIKE movto-estoq.nr-ord-refer
    FIELD nr-req-sum            LIKE movto-estoq.nr-req-sum
    FIELD cod-roteiro           LIKE movto-estoq.cod-roteiro
    FIELD nr-reporte            LIKE movto-estoq.nr-reporte
    FIELD item-pai              LIKE movto-estoq.item-pai
    FIELD op-codigo             LIKE movto-estoq.op-codigo
    FIELD cod-usu-ult-alter     LIKE movto-estoq.cod-usu-ult-alter
    FIELD conta-contabil        LIKE movto-estoq.conta-contabil
    FIELD conta-db              LIKE movto-estoq.conta-contabil
    FIELD ct-db                 LIKE movto-estoq.ct-codigo
    FIELD sc-db                 LIKE movto-estoq.sc-codigo
    FIELD dt-vali-lote          LIKE saldo-estoq.dt-vali-lote
    FIELD op-seq                LIKE movto-estoq.op-seq
    FIELD usuario               LIKE movto-estoq.usuario
    FIELD nr-trans              LIKE movto-estoq.nr-trans 
    FIELD cod-estabel-des       LIKE movto-estoq.cod-estabel-des
    FIELD origem-valor          LIKE movto-estoq.origem-valor
    FIELD num-ord-des           LIKE movto-estoq.num-ord-des
    FIELD num-seq-des           LIKE movto-estoq.num-seq-des
    FIELD num-ord-inv           LIKE movto-estoq.num-ord-inv
    FIELD valor-ipi             LIKE movto-estoq.valor-ipi
    FIELD valor-iss             LIKE movto-estoq.valor-iss
    FIELD valor-icm             LIKE movto-estoq.valor-icm
    FIELD vl-icm-fasb           LIKE movto-estoq.vl-icm-fasb
    FIELD vl-iss-fasb           LIKE movto-estoq.vl-iss-fasb
    FIELD vl-ipi-fasb           LIKE movto-estoq.vl-ipi-fasb 
    FIELD per-ppm               LIKE movto-estoq.per-ppm
    FIELD atualiza-ul-ent       AS LOGICAL
    FIELD i-sequen              AS INTEGER
    FIELD gera-saldo            AS LOGICAL INIT NO
    FIELD qt-alocada            AS DECIMAL
  &if '{&bf_lote_avancado_liberado}' = 'yes' &then
    field i-sequen-pai             as integer
    field log-ficha                as log
  &endif
  .
     
/* Fim Include ceapi001.i */
 

    
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

def var h-acomp as handle no-undo.


def temp-table tt-relatorio no-undo
    field it-codigo like item.it-codigo
    field descricao as char format "x(80)"
    field erro      as log init yes.

def temp-table tt-rel-aux   no-undo
    field it-codigo like item.it-codigo
    field i-sequen  like tt-erro.i-sequen
    field cd-erro   like tt-erro.cd-erro
    field descricao like tt-erro.mensagem.
    
def temp-table tt-lote-a-config   no-undo
    field it-codigo like item.it-codigo
    field desc-item like item.desc-item
    field lote      like saldo-estoq.lote.


/*SF0303A*/
def var h-boin533    as handle no-undo.
def var h-boin536    as handle no-undo.
def var h-cpapi301   as handle no-undo.
def var i-ind-refugo as int    no-undo.

def temp-table tt-rep-oper-ctrab  no-undo like rep-oper-ctrab
    field cod-ferr-prod                   like split-operac.cod-ferr-prod
    field dat-fim-setup                   like split-operac.dat-fim-setup
    field dat-inic-setup                  like split-operac.dat-inic-setup
    field qtd-segs-fim-setup              like split-operac.qtd-segs-fim-setup
    field qtd-segs-inic-setup             like split-operac.qtd-segs-inic-setup.

def temp-table tt-rep-refugo-oper no-undo like rep-refugo-oper.
def temp-table tt-rep-ic-oper     no-undo like rep-ic-oper.
def temp-table tt-rep-ic-oper-tab no-undo like rep-ic-oper-tab.

/********************************************************************************
**
**  SF0303A.I - Definiá∆o de temp-table tt-reporte.
**
*********************************************************************************/

def temp-table tt-reporte no-undo
    field rw-split-operac       as rowid
    field cod-ferr-prod         like split-operac.cod-ferr-prod
    field dat-fim-setup         like split-operac.dat-fim-setup
    field dat-inic-setup        like split-operac.dat-inic-setup
    field qtd-segs-fim-setup    like split-operac.qtd-segs-fim-setup
    field qtd-segs-inic-setup   like split-operac.qtd-segs-inic-setup
    field dat-fim-reporte       like rep-oper-ctrab.dat-fim-reporte
    field dat-inic-reporte      like rep-oper-ctrab.dat-inic-reporte
    field qtd-operac-refgda     like rep-oper-ctrab.qtd-operac-refgda
    field qtd-operac-reptda     like rep-oper-ctrab.qtd-operac-reptda
    field qtd-operac-retrab     like rep-oper-ctrab.qtd-operac-retrab
    field qtd-operac-aprov      like rep-oper-ctrab.qtd-operac-aprov
    field qtd-segs-fim-reporte  like rep-oper-ctrab.qtd-segs-fim-reporte
    field qtd-segs-inic-reporte like rep-oper-ctrab.qtd-segs-inic-reporte
    field num-contador-inic     like rep-oper-ctrab.num-contador-inic
    field num-contador-fim      like rep-oper-ctrab.num-contador-fim
    field dep-refugo            like ord-prod.cod-depos
    field loc-refugo            like reservas.cod-localiz
    field cod-equipe            like rep-oper-ctrab.cod-equipe
    field baixa-reservas        as int
    field informa-deposito      as log
    field informa-localizacao   as log
    field requisicao-automatica as log
    field busca-saldos          as log
    field requisita-configurado as log
    field dep-acab              like ord-prod.cod-depos
    field loc-acab              like reservas.cod-localiz
    field cod-depos             like reservas.cod-depos
    field cod-localiz           like reservas.cod-localiz
    field lote-serie            like ord-prod.lote-serie
    field cod-refer             like ord-prod.cod-refer
    field dt-vali-lote          like saldo-estoq.dt-vali-lote
    field conta-refugo          as char 
    field conta-debito          as char 
    index id is unique primary rw-split-operac.
  /* Temp-table tt-reporte */

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
     /* pi-formatted-time-to-sec e pi-sec-to-formatted-time */
PROCEDURE pi-det-ultimo-evento-ctrab:

    def output param v-dat-ult-event as date no-undo.
    def output param v-qtd-segs-ult-event as dec no-undo.

    def var v-val-dat-atual as decimal no-undo.

    run pi-converte-data-segs-valor (Input today,
                                     Input time,
                                     output v-val-dat-atual).

    find last rep-oper-ctrab
        where rep-oper-ctrab.cod-ctrab       = ctrab.cod-ctrab
          and rep-oper-ctrab.val-refer-inic <= v-val-dat-atual no-lock no-error.

    if  available rep-oper-ctrab
    and (v-dat-ult-event = ? 
     or  rep-oper-ctrab.dat-fim-reporte > v-dat-ult-event
     or (rep-oper-ctrab.dat-fim-reporte = v-dat-ult-event
     and rep-oper-ctrab.qtd-segs-fim-reporte > v-qtd-segs-ult-event)) then do:
        assign v-dat-ult-event      = rep-oper-ctrab.dat-fim-reporte
               v-qtd-segs-ult-event = rep-oper-ctrab.qtd-segs-fim-reporte.
    end.

    /*** Verificar se existe uma parada imediata antes do reporte ***/
    find last rep-parada-ctrab no-lock
        where rep-parada-ctrab.cod-ctrab              = ctrab.cod-ctrab
          and rep-parada-ctrab.val-refer-inic-parada <= v-val-dat-atual no-error.

    if  available rep-parada-ctrab
    and (v-dat-ult-event = ? 
     or  rep-parada-ctrab.dat-fim-parada > v-dat-ult-event
     or (rep-parada-ctrab.dat-fim-parada = v-dat-ult-event
     and rep-parada-ctrab.qtd-segs-fim > v-qtd-segs-ult-event))
    then do:
        assign v-dat-ult-event      = rep-parada-ctrab.dat-fim-parada
               v-qtd-segs-ult-event = rep-parada-ctrab.qtd-segs-fim.
    end.

    if  v-dat-ult-event = ? then  
        assign v-dat-ult-event      = today
               v-qtd-segs-ult-event = 0.
END PROCEDURE.

PROCEDURE pi-det-ultimo-evento-equipe:

    def output param v-dat-ult-event as date no-undo.
    def output param v-qtd-segs-ult-event as dec no-undo.

    def var v-val-dat-atual                  as decimal         no-undo.

    run pi-converte-data-segs-valor (Input today,
                                     Input time,
                                     output v-val-dat-atual).

    /*** Verificar os £ltimos reportes do operador ***/

/***

    find last movto-mod-sfc
        where movto-mod-sfc.cod-equipe-produc       = equipe-produc.cod-equipe-produc
          and movto-mod-sfc.dat-fim-rep-oper-ctrab <= today 
         use-index mvtmdsfc-equipe no-lock no-error.

    if  available movto-mod-sfc then 
        assign v-dat-ult-event      = movto-mod-sfc.dat-fim-rep-oper-ctrab
               v-qtd-segs-ult-event = movto-mod-sfc.qtd-segs-fim-reporte.

***/

    /*** Verificar se existe uma parada imediata antes do reporte ***/
    find last rep-parada-mod no-lock
        where rep-parada-mod.cod-equipe            = equipe-prod.cod-equipe
          and rep-parada-mod.val-refer-inic-parada < v-val-dat-atual no-error.

    if  available rep-parada-mod
    and (v-dat-ult-event = ?
     or  rep-parada-mod.dat-fim-parada > v-dat-ult-event
     or (rep-parada-mod.dat-fim-parada = v-dat-ult-event
     and rep-parada-mod.qtd-segs-fim   > v-qtd-segs-ult-event)) then do:

        assign  v-dat-ult-event      = rep-parada-mod.dat-fim-parada
                v-qtd-segs-ult-event = rep-parada-mod.qtd-segs-fim.
    end.

    if  v-dat-ult-event = ? then  
        assign v-dat-ult-event      = today
               v-qtd-segs-ult-event = 0.
END PROCEDURE.

PROCEDURE pi-det-ultimo-evento-operador:

    def output param v-dat-ult-event as date no-undo.
    def output param v-qtd-segs-ult-event as dec no-undo.

    def var v-val-dat-atual                  as decimal         no-undo. /*local*/

    /*** Verificar os £ltimos reportes do operador ***/
    run pi-converte-data-segs-valor (Input today,
                                     Input time,
                                     output v-val-dat-atual).

/****

    /*** Verificar os £ltimos reportes do operador ***/
    find last movto-mod-sfc no-lock
        where movto-mod-sfc.cod-operador = operador-sfc.cod-operador
          and movto-mod-sfc.dat-fim-rep-oper-ctrab <= today 
        use-index mvtmdsfc-operador no-error.
    if  available movto-mod-sfc then 
        assign v-dat-ult-event      = movto-mod-sfc.dat-fim-rep-oper-ctrab
               v-qtd-segs-ult-event = movto-mod-sfc.qtd-segs-fim-reporte.

***/

    /*** Verificar se existe uma parada imediata antes do reporte ***/
    find last rep-parada-mod no-lock
        where rep-parada-mod.cod-operador          = operador.cod-operador
          and rep-parada-mod.val-refer-inic-parada < v-val-dat-atual no-error.

    if  available rep-parada-mod
    and (v-dat-ult-event = ?
     or  rep-parada-mod.dat-fim-parada > v-dat-ult-event
     or (rep-parada-mod.dat-fim-parada = v-dat-ult-event
    and  rep-parada-mod.qtd-segs-fim   > v-qtd-segs-ult-event)) then do:

        assign v-dat-ult-event      = rep-parada-mod.dat-fim-parada
               v-qtd-segs-ult-event = rep-parada-mod.qtd-segs-fim.
    end.

    if  v-dat-ult-event = ? then  
        assign v-dat-ult-event      = today
               v-qtd-segs-ult-event = 0.
END PROCEDURE.

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
           p-dat-refer-1       = date(substr(v-cod-refer-dat,7,2) + "/" +
                                   substr(v-cod-refer-dat,5,2) + "/" +
                                   substr(v-cod-refer-dat,1,4))
           p-qtd-segs-refer-1  = integer(substr(v-cod-refer-dat,9,5)).
END PROCEDURE.
 
     /* pi-det-ultimo-evento-ctrab */
/*******************************************************************************
**
** Procedure pi-sfc-valid-qtd-reptda-pert.
**
********************************************************************************/

procedure pi-sfc-valid-qtd-reptda-pert:

    def input param i-nr-ord-produ   like ord-prod.nr-ord-produ   no-undo.
    def input param i-num-operac-sfc like oper-ord.num-operac-sfc no-undo.
    def output param de-val-max      as dec no-undo.
     
    def var de-val-max-aux  as dec no-undo.
    def var de-val-perc-max as dec no-undo.
    
    def buffer b-oper-ord   for oper-ord.
    def buffer b1-oper-ord  for oper-ord.
    DEF BUFFER b-ordem-rede FOR ord-prod.
    DEF BUFFER b-pert-ordem FOR pert-ordem.
    
    find first b-oper-ord
         where b-oper-ord.nr-ord-produ   = i-nr-ord-produ
           and b-oper-ord.num-operac-sfc = i-num-operac-sfc no-lock no-error.
    if  not avail b-oper-ord then
        return.
       
    FOR FIRST b-ordem-rede FIELDS(nr-ord-produ rep-prod)
        WHERE b-ordem-rede.nr-ord-produ = i-nr-ord-produ NO-LOCK: END.
    
    assign de-val-perc-max = 99999999999999999999.

    blk-pert:
    for each b-pert-ordem fields (nr-ord-produ num-operac-suces num-operac-predec) use-index suces-ativ
       where b-pert-ordem.nr-ord-produ     = b-oper-ord.nr-ord-produ
         AND b-pert-ordem.num-operac-suces = b-oper-ord.num-operac-sfc NO-LOCK:
    
        find FIRST b1-oper-ord use-index sfc
             where b1-oper-ord.nr-ord-produ   = b-pert-ordem.nr-ord-produ
               AND b1-oper-ord.num-operac-sfc = b-pert-ordem.num-operac-predec no-lock no-error.
        if  not available b1-oper-ord then
            next.
        
        find FIRST grup-maquina use-index codigo 
             WHERE grup-maquina.gm-codigo = b1-oper-ord.gm-codigo no-lock no-error.
        
        IF NOT AVAIL grup-maquina THEN NEXT.

        if  not grup-maquina.log-pto-control
        OR (b-ordem-rede.rep-prod    = 3
        AND b1-oper-ord.pto-controle = 0) then do:
            run pi-sfc-valid-qtd-reptda-pert (input i-nr-ord-produ,
                                              input b-pert-ordem.num-operac-predec, 
                                              output de-val-max).

            assign de-val-perc-max = min(de-val-perc-max, 
                                         b1-oper-ord.qt-produzida).
        end.
        else                                                   
            assign de-val-perc-max = min(de-val-perc-max, b1-oper-ord.qt-produzida).
        
        if  de-val-perc-max <= 0 then
            leave blk-pert.
    end.       

    IF de-val-perc-max = 99999999999999999999 THEN
          ASSIGN de-val-perc-max = oper-ord.qtd-previs-operac.
    
    assign de-val-max = de-val-perc-max - b-oper-ord.qt-produzida
           de-val-max = max(de-val-max, 0).

end procedure.

/* Fim Procedure */
     /* pi-sfc-valid-qtd-reptda-pert */
/********************************************************************************* 
**     Procedure: pi-corrige-data-tempo.
**     Objetivo.: Atualizar a data e a hora de tÇrmino em funá∆o do tempo de
**                processamento.
**********************************************************************************/

procedure pi-corrige-data-tempo:

    def input-output param dat-fim          as date no-undo.
    def input-output param qtd-segs-fim     as dec no-undo.
    def input        param qtd-tempo-proces as dec no-undo.
    
    assign dat-fim = dat-fim + INT((qtd-tempo-proces + qtd-segs-fim) / 86400 - 0.5)
           qtd-segs-fim = (qtd-segs-fim + qtd-tempo-proces) MOD 86400.

end procedure.
     /* pi-corrige-data-tempo */
/**********************************************************************************************************
** 
** PROCEDURE CalcularTempoPadraoO
** Objetivo: Calcula o tempo padr∆o para produzir uma quantidade de operaá∆o em funá∆o de dados de
             da OPER-ORD, centro de trabalho e ferramenta utilizada
** ParÉmetros: buffer da operaá∆o da ordem
               input  c¢digo de centro de trabalho (opcional)
               input  c¢digo de ferramenta utilizada (opcional)
               input  quantidade de operaá∆o a ser produzida
               output tempo padr∆o de m†quina
               output tempo padr∆o de mod
               output capacidade padr∆o de m†quina
**
**********************************************************************************************************/
procedure CalcularTempoPadraoO:

   /**********************************************************************************
** 
** C†lculo de padr∆o de tempos em funá∆o da operaá∆o da ordem, operaá∆o engenharia
** ou operaá∆o alternativa 
** Operaá∆o da ordem:   CalcularTempoPadraoO  {1}: oper-ord
                                              {2}: qtd-previs-operac
                                              {3}: b-oper-ord.op-altern
   Operacao Engenharia: CalcularTempoPadraoE  {1}: operacao
                                              {2}: nr-unidades
                                              {3}: 0
   Operacao Alternativa: CalcularTempoPadraoA {1}: op-altern
                                              {2}: nr-unidades
                                              {3}: op-altern.op-altern
**                      
***********************************************************************************/    

    define parameter buffer b-oper-ord for oper-ord.
    
    define input  parameter  p-cod-ctrab   like split-operac.cod-ctrab      no-undo.
    define input  parameter  p-cod-ferr    like split-operac.cod-ferr-prod  no-undo.
    define input  parameter  p-qtd-refer                     as decimal     no-undo.
    define output parameter  p-tempo-pad-maq                    as decimal  no-undo.
    define output parameter  p-tempo-pad-mod                    as decimal  no-undo.
    define output parameter  p-capac-pad                        as decimal  no-undo.  
    
    def var de-fator              as decimal no-undo.
    def var de-nr-lotes           as decimal no-undo.
    def var de-qtd-carga-batch    as decimal no-undo.
    def var de-ciclos-seg         as decimal no-undo.
    def var de-un-ciclo           as decimal no-undo.

    case b-oper-ord.un-med-tempo:
         when 1 then
              assign de-fator = 3600.
         when 2 then                     
              assign  de-fator = 60.
         when 3 then         
              assign  de-fator = 1.
         when 4 then
              assign de-fator = 86400.
    end case.
    
    assign p-capac-pad        = b-oper-ord.qtd-capac-operac * p-qtd-refer / b-oper-ord.qtd-previs-operac
           de-qtd-carga-batch = b-oper-ord.qtd-carga-batch.
    
    case b-oper-ord.ind-tempo-operac:
        /** Proporcional **/
        when 1 then  
             assign p-tempo-pad-maq = b-oper-ord.tempo-maquin * de-fator * p-qtd-refer / b-oper-ord.qtd-previs-operac  
                    p-tempo-pad-mod = b-oper-ord.tempo-homem  * de-fator * p-qtd-refer / b-oper-ord.qtd-previs-operac.  

        /** Fixo **/
        when 2 then
             assign p-tempo-pad-maq = b-oper-ord.tempo-maquin * de-fator 
                    p-tempo-pad-mod = b-oper-ord.tempo-homem  * de-fator.

        /** Por Lote **/
        when 3 then do: 
            /** O tamanho da batelada depende do centro de trabalho ***/
            if  p-cod-ctrab <> "" then 
                find ctrab
                    where ctrab.cod-ctrab = p-cod-ctrab no-lock no-error.
        
            if  available ctrab then
                assign de-qtd-carga-batch = ctrab.qtd-carga-batch.
            
            if  de-qtd-carga-batch = 0 then 
                assign de-qtd-carga-batch = p-capac-pad.   

            assign de-nr-lotes = p-capac-pad / de-qtd-carga-batch.
            if  round(de-nr-lotes,0) < (de-nr-lotes * 0.995) then
                assign de-nr-lotes = round(de-nr-lotes,0) + 1.
            else 
                assign de-nr-lotes = round(de-nr-lotes,0).
            
            assign p-tempo-pad-maq = b-oper-ord.tempo-maquin * de-fator * de-nr-lotes 
                   p-tempo-pad-mod = b-oper-ord.tempo-homem  * de-fator * de-nr-lotes
                   p-capac-pad     = p-capac-pad / de-nr-lotes.
        end.
        
        /** Dep. Ferramenta **/
        when 4 then do:

            assign p-tempo-pad-maq = b-oper-ord.tempo-maquin * de-fator * p-qtd-refer / b-oper-ord.qtd-previs-operac  
                   p-tempo-pad-mod = b-oper-ord.tempo-homem  * de-fator * p-qtd-refer / b-oper-ord.qtd-previs-operac.  


            if  p-cod-ferr <> "" and p-cod-ctrab <> "" then do:
                assign de-un-ciclo     = 0.
            
                find ferr-prod
                    where ferr-prod.cod-ferr-prod = p-cod-ferr no-lock no-error.
                if  available ferr-prod then
                    assign de-un-ciclo = ferr-prod.un-ciclo.

                if  de-un-ciclo = 0 then do:
                    find first op-ferram
                        where op-ferram.num-id-operacao = b-oper-ord.num-id-operacao
                          and op-ferram.op-altern       = b-oper-ord.op-altern
                          and op-ferram.ferramenta      = p-cod-ferr no-lock no-error.
                    if  available op-ferram then
                        assign de-un-ciclo = op-ferram.un-ciclo.
                end.
  
                if  de-un-ciclo > 0 then do: 
                    find first ctrab-ferram
                         where ctrab-ferram.cod-ctrab     = p-cod-ctrab
                           and ctrab-ferram.cod-ferr-prod = p-cod-ferr no-lock no-error.
                    if  avail ctrab-ferram then do:
                        case ctrab-ferram.un-med-tempo:
                            when 1 then
                                assign de-ciclos-seg = ctrab-ferram.qtd-ciclos-umt / 3600.
                            when 2 then
                                assign de-ciclos-seg = ctrab-ferram.qtd-ciclos-umt / 60.
                            when 3 then
                                assign de-ciclos-seg = ctrab-ferram.qtd-ciclos-umt.
                            when 4 then
                                assign de-ciclos-seg = ctrab-ferram.qtd-ciclos-umt / 86400.
                        end case.    
  
                        assign p-tempo-pad-maq = p-qtd-refer / (de-un-ciclo * de-ciclos-seg) 
                               p-tempo-pad-mod = p-tempo-pad-maq.
                    end.
                end.    
            end.  
        end.
    end case.
 

end procedure.


/**********************************************************************************************************
** 
** PROCEDURE CalcularTempoPadraoE
** Objetivo: Calcula o tempo padr∆o para produzir uma quantidade de operaá∆o em funá∆o de dados de
             da OPERACAO de ENGENHARIA, centro de trabalho e ferramenta utilizada
** ParÉmetros: buffer da operaá∆o da ordem
               input  c¢digo de centro de trabalho (opcional)
               input  c¢digo de ferramenta utilizada (opcional)
               input  quantidade de operaá∆o a ser produzida
               output tempo padr∆o de m†quina
               output tempo padr∆o de mod
               output capacidade padr∆o de m†quina
**
**********************************************************************************************************/
procedure CalcularTempoPadraoE:

   /**********************************************************************************
** 
** C†lculo de padr∆o de tempos em funá∆o da operaá∆o da ordem, operaá∆o engenharia
** ou operaá∆o alternativa 
** Operaá∆o da ordem:   CalcularTempoPadraoO  {1}: oper-ord
                                              {2}: qtd-previs-operac
                                              {3}: b-oper-ord.op-altern
   Operacao Engenharia: CalcularTempoPadraoE  {1}: operacao
                                              {2}: nr-unidades
                                              {3}: 0
   Operacao Alternativa: CalcularTempoPadraoA {1}: op-altern
                                              {2}: nr-unidades
                                              {3}: op-altern.op-altern
**                      
***********************************************************************************/    

    define parameter buffer b-operacao for mgemp.operacao.
    
    define input  parameter  p-cod-ctrab   like split-operac.cod-ctrab      no-undo.
    define input  parameter  p-cod-ferr    like split-operac.cod-ferr-prod  no-undo.
    define input  parameter  p-qtd-refer                     as decimal     no-undo.
    define output parameter  p-tempo-pad-maq                    as decimal  no-undo.
    define output parameter  p-tempo-pad-mod                    as decimal  no-undo.
    define output parameter  p-capac-pad                        as decimal  no-undo.  
    
    def var de-fator              as decimal no-undo.
    def var de-nr-lotes           as decimal no-undo.
    def var de-qtd-carga-batch    as decimal no-undo.
    def var de-ciclos-seg         as decimal no-undo.
    def var de-un-ciclo           as decimal no-undo.

    case b-operacao.un-med-tempo:
         when 1 then
              assign de-fator = 3600.
         when 2 then                     
              assign  de-fator = 60.
         when 3 then         
              assign  de-fator = 1.
         when 4 then
              assign de-fator = 86400.
    end case.
    
    assign p-capac-pad        = b-operacao.qtd-capac-operac * p-qtd-refer / b-operacao.nr-unidades
           de-qtd-carga-batch = b-operacao.qtd-carga-batch.
    
    case b-operacao.ind-tempo-operac:
        /** Proporcional **/
        when 1 then  
             assign p-tempo-pad-maq = b-operacao.tempo-maquin * de-fator * p-qtd-refer / b-operacao.nr-unidades  
                    p-tempo-pad-mod = b-operacao.tempo-homem  * de-fator * p-qtd-refer / b-operacao.nr-unidades.  

        /** Fixo **/
        when 2 then
             assign p-tempo-pad-maq = b-operacao.tempo-maquin * de-fator 
                    p-tempo-pad-mod = b-operacao.tempo-homem  * de-fator.

        /** Por Lote **/
        when 3 then do: 
            /** O tamanho da batelada depende do centro de trabalho ***/
            if  p-cod-ctrab <> "" then 
                find ctrab
                    where ctrab.cod-ctrab = p-cod-ctrab no-lock no-error.
        
            if  available ctrab then
                assign de-qtd-carga-batch = ctrab.qtd-carga-batch.
            
            if  de-qtd-carga-batch = 0 then 
                assign de-qtd-carga-batch = p-capac-pad.   

            assign de-nr-lotes = p-capac-pad / de-qtd-carga-batch.
            if  round(de-nr-lotes,0) < (de-nr-lotes * 0.995) then
                assign de-nr-lotes = round(de-nr-lotes,0) + 1.
            else 
                assign de-nr-lotes = round(de-nr-lotes,0).
            
            assign p-tempo-pad-maq = b-operacao.tempo-maquin * de-fator * de-nr-lotes 
                   p-tempo-pad-mod = b-operacao.tempo-homem  * de-fator * de-nr-lotes
                   p-capac-pad     = p-capac-pad / de-nr-lotes.
        end.
        
        /** Dep. Ferramenta **/
        when 4 then do:

            assign p-tempo-pad-maq = b-operacao.tempo-maquin * de-fator * p-qtd-refer / b-operacao.nr-unidades  
                   p-tempo-pad-mod = b-operacao.tempo-homem  * de-fator * p-qtd-refer / b-operacao.nr-unidades.  


            if  p-cod-ferr <> "" and p-cod-ctrab <> "" then do:
                assign de-un-ciclo     = 0.
            
                find ferr-prod
                    where ferr-prod.cod-ferr-prod = p-cod-ferr no-lock no-error.
                if  available ferr-prod then
                    assign de-un-ciclo = ferr-prod.un-ciclo.

                if  de-un-ciclo = 0 then do:
                    find first op-ferram
                        where op-ferram.num-id-operacao = b-operacao.num-id-operacao
                          and op-ferram.op-altern       = 0
                          and op-ferram.ferramenta      = p-cod-ferr no-lock no-error.
                    if  available op-ferram then
                        assign de-un-ciclo = op-ferram.un-ciclo.
                end.
  
                if  de-un-ciclo > 0 then do: 
                    find first ctrab-ferram
                         where ctrab-ferram.cod-ctrab     = p-cod-ctrab
                           and ctrab-ferram.cod-ferr-prod = p-cod-ferr no-lock no-error.
                    if  avail ctrab-ferram then do:
                        case ctrab-ferram.un-med-tempo:
                            when 1 then
                                assign de-ciclos-seg = ctrab-ferram.qtd-ciclos-umt / 3600.
                            when 2 then
                                assign de-ciclos-seg = ctrab-ferram.qtd-ciclos-umt / 60.
                            when 3 then
                                assign de-ciclos-seg = ctrab-ferram.qtd-ciclos-umt.
                            when 4 then
                                assign de-ciclos-seg = ctrab-ferram.qtd-ciclos-umt / 86400.
                        end case.    
  
                        assign p-tempo-pad-maq = p-qtd-refer / (de-un-ciclo * de-ciclos-seg) 
                               p-tempo-pad-mod = p-tempo-pad-maq.
                    end.
                end.    
            end.  
        end.
    end case.
 

end procedure.


/**********************************************************************************************************
** 
** PROCEDURE CalcularTempoPadraoA
** Objetivo: Calcula o tempo padr∆o para produzir uma quantidade de operaá∆o em funá∆o de dados de
             da OPERACAO ALTERNATIVA da ENGENHARIA, centro de trabalho e ferramenta utilizada
** ParÉmetros: buffer da operaá∆o da ordem
               input  c¢digo de centro de trabalho (opcional)
               input  c¢digo de ferramenta utilizada (opcional)
               input  quantidade de operaá∆o a ser produzida
               output tempo padr∆o de m†quina
               output tempo padr∆o de mod
               output capacidade padr∆o de m†quina
**
**********************************************************************************************************/
procedure CalcularTempoPadraoA:

   /**********************************************************************************
** 
** C†lculo de padr∆o de tempos em funá∆o da operaá∆o da ordem, operaá∆o engenharia
** ou operaá∆o alternativa 
** Operaá∆o da ordem:   CalcularTempoPadraoO  {1}: oper-ord
                                              {2}: qtd-previs-operac
                                              {3}: b-oper-ord.op-altern
   Operacao Engenharia: CalcularTempoPadraoE  {1}: operacao
                                              {2}: nr-unidades
                                              {3}: 0
   Operacao Alternativa: CalcularTempoPadraoA {1}: op-altern
                                              {2}: nr-unidades
                                              {3}: op-altern.op-altern
**                      
***********************************************************************************/    

    define parameter buffer b-op-altern for op-altern.
    
    define input  parameter  p-cod-ctrab   like split-operac.cod-ctrab      no-undo.
    define input  parameter  p-cod-ferr    like split-operac.cod-ferr-prod  no-undo.
    define input  parameter  p-qtd-refer                     as decimal     no-undo.
    define output parameter  p-tempo-pad-maq                    as decimal  no-undo.
    define output parameter  p-tempo-pad-mod                    as decimal  no-undo.
    define output parameter  p-capac-pad                        as decimal  no-undo.  
    
    def var de-fator              as decimal no-undo.
    def var de-nr-lotes           as decimal no-undo.
    def var de-qtd-carga-batch    as decimal no-undo.
    def var de-ciclos-seg         as decimal no-undo.
    def var de-un-ciclo           as decimal no-undo.

    case b-op-altern.un-med-tempo:
         when 1 then
              assign de-fator = 3600.
         when 2 then                     
              assign  de-fator = 60.
         when 3 then         
              assign  de-fator = 1.
         when 4 then
              assign de-fator = 86400.
    end case.
    
    assign p-capac-pad        = b-op-altern.qtd-capac-operac * p-qtd-refer / b-op-altern.nr-unidades
           de-qtd-carga-batch = b-op-altern.qtd-carga-batch.
    
    case b-op-altern.ind-tempo-operac:
        /** Proporcional **/
        when 1 then  
             assign p-tempo-pad-maq = b-op-altern.tempo-maquin * de-fator * p-qtd-refer / b-op-altern.nr-unidades  
                    p-tempo-pad-mod = b-op-altern.tempo-homem  * de-fator * p-qtd-refer / b-op-altern.nr-unidades.  

        /** Fixo **/
        when 2 then
             assign p-tempo-pad-maq = b-op-altern.tempo-maquin * de-fator 
                    p-tempo-pad-mod = b-op-altern.tempo-homem  * de-fator.

        /** Por Lote **/
        when 3 then do: 
            /** O tamanho da batelada depende do centro de trabalho ***/
            if  p-cod-ctrab <> "" then 
                find ctrab
                    where ctrab.cod-ctrab = p-cod-ctrab no-lock no-error.
        
            if  available ctrab then
                assign de-qtd-carga-batch = ctrab.qtd-carga-batch.
            
            if  de-qtd-carga-batch = 0 then 
                assign de-qtd-carga-batch = p-capac-pad.   

            assign de-nr-lotes = p-capac-pad / de-qtd-carga-batch.
            if  round(de-nr-lotes,0) < (de-nr-lotes * 0.995) then
                assign de-nr-lotes = round(de-nr-lotes,0) + 1.
            else 
                assign de-nr-lotes = round(de-nr-lotes,0).
            
            assign p-tempo-pad-maq = b-op-altern.tempo-maquin * de-fator * de-nr-lotes 
                   p-tempo-pad-mod = b-op-altern.tempo-homem  * de-fator * de-nr-lotes
                   p-capac-pad     = p-capac-pad / de-nr-lotes.
        end.
        
        /** Dep. Ferramenta **/
        when 4 then do:

            assign p-tempo-pad-maq = b-op-altern.tempo-maquin * de-fator * p-qtd-refer / b-op-altern.nr-unidades  
                   p-tempo-pad-mod = b-op-altern.tempo-homem  * de-fator * p-qtd-refer / b-op-altern.nr-unidades.  


            if  p-cod-ferr <> "" and p-cod-ctrab <> "" then do:
                assign de-un-ciclo     = 0.
            
                find ferr-prod
                    where ferr-prod.cod-ferr-prod = p-cod-ferr no-lock no-error.
                if  available ferr-prod then
                    assign de-un-ciclo = ferr-prod.un-ciclo.

                if  de-un-ciclo = 0 then do:
                    find first op-ferram
                        where op-ferram.num-id-operacao = b-op-altern.num-id-operacao
                          and op-ferram.op-altern       = op-altern.op-altern
                          and op-ferram.ferramenta      = p-cod-ferr no-lock no-error.
                    if  available op-ferram then
                        assign de-un-ciclo = op-ferram.un-ciclo.
                end.
  
                if  de-un-ciclo > 0 then do: 
                    find first ctrab-ferram
                         where ctrab-ferram.cod-ctrab     = p-cod-ctrab
                           and ctrab-ferram.cod-ferr-prod = p-cod-ferr no-lock no-error.
                    if  avail ctrab-ferram then do:
                        case ctrab-ferram.un-med-tempo:
                            when 1 then
                                assign de-ciclos-seg = ctrab-ferram.qtd-ciclos-umt / 3600.
                            when 2 then
                                assign de-ciclos-seg = ctrab-ferram.qtd-ciclos-umt / 60.
                            when 3 then
                                assign de-ciclos-seg = ctrab-ferram.qtd-ciclos-umt.
                            when 4 then
                                assign de-ciclos-seg = ctrab-ferram.qtd-ciclos-umt / 86400.
                        end case.    
  
                        assign p-tempo-pad-maq = p-qtd-refer / (de-un-ciclo * de-ciclos-seg) 
                               p-tempo-pad-mod = p-tempo-pad-maq.
                    end.
                end.    
            end.  
        end.
    end case.
 

end procedure.
  /* CalcularTempoPadrao   */

def buffer b-oper-ord for oper-ord.
def buffer b-ord-prod for ord-prod.
    
def var de-qtd-segs-inic-prep    as dec no-undo.
def var de-qtd-segs-fim-prep     as dec no-undo.
def var de-qtd-segs-inic-reporte like rep-oper-ctrab.qtd-segs-inic-reporte no-undo.
def var de-qtd-segs-fim-reporte  like rep-oper-ctrab.qtd-segs-fim-reporte  no-undo.
def var l-erro-ver as log no-undo.

def var i-num-seq-rep as int no-undo.

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
 
 
/****************************************************************************
**
**   Include: CPAPI012.I1  - Definiá∆o das Temp-Tables Internas
**
*****************************************************************************/
 

 def temp-table tt-aloca 
     field cod-estabel      like aloca-reserva.cod-estabel
     field it-codigo        like aloca-reserva.it-codigo
     field nr-ord-produ     like aloca-reserva.nr-ord-produ
     field cod-depos        like aloca-reserva.cod-depos
     field cod-localiz      like aloca-reserva.cod-localiz
     field lote-serie       like reservas.lote-serie
     field quant-aloc       like aloca-reserva.quant-aloc   
     field quant-calc       like aloca-reserva.quant-aloc
     field qt-a-req         like aloca-reserva.quant-aloc
     field cod-refer        like saldo-estoq.cod-refer
     field op-codigo        like aloca-reserva.op-codigo
     field cod-roteiro      like aloca-reserva.cod-roteiro
     field item-pai         like aloca-reserva.item-pai
     field un               like reservas.un
     field dt-vali-lote     like saldo-estoq.dt-vali-lote
     field rw-aloca-reserva as rowid
     field sequencia        as integer
   &IF DEFINED (bf_man_per_ppm) &THEN
     field veiculo          like reservas.veiculo
     field per-ppm          like reservas.per-ppm
     field per-ppm-lote     like reservas.per-ppm
     field tipo-formula     like reservas.tipo-formula
     field qt-a-req-fis     like aloca-reserva.quant-aloc
     field qt-aloc-lote     like aloca-reserva.qt-aloc-lote
     field l-balanceado     as log
   &ENDIF
     index seq is primary unique nr-ord-produ sequencia
     index aloca item-pai 
                 cod-roteiro         
                 op-codigo
                 it-codigo
                 cod-estabel
                 cod-depos
                 cod-localiz
                 lote-serie
     index rowid-aloca-reserva rw-aloca-reserva.

 def temp-table tt-reservas 
    field selec               as logical init yes format "*/ "
    field proporcao           as decimal
    field log-sem-saldo       as logical
    field nr-ord-produ      like reservas.nr-ord-produ
    field cod-refer         like reservas.cod-refer
    field it-codigo         like reservas.it-codigo
    field quant-orig        like reservas.quant-orig  
    field quant-aloc        like reservas.quant-orig
    field quant-atend       like reservas.quant-orig
    field quant-calc        like reservas.quant-orig
    field quant-requis      like reservas.quant-orig
    field quant-requis-aloc like reservas.quant-orig
    field cod-depos         like reservas.cod-depos
    field cod-localiz       like reservas.cod-localiz
    field lote-serie        like reservas.lote-serie
    field dt-vali-lote      like saldo-estoq.dt-vali-lote
    field dt-saida            as date format "99/99/9999" init today
    field un                like reservas.un
    field estado            like reservas.estado
    field tipo-sobra        like reservas.tipo-sobra
    field item-pai          like reservas.item-pai
    field op-codigo         like reservas.op-codigo
    field cod-roteiro       like reservas.cod-roteiro

    field per-ppm           like reservas.per-ppm
    field tipo-formula      like reservas.tipo-formula
    field qt-atend-lote     like reservas.qt-atend-lote
 
    field processada          as logical
    field rw-reserva          as rowid
    field rw-saldo-estoq      as rowid
    field rw-mov-orig         as rowid
    field tipo-ordem          as integer
    field tempo               as integer
    field tentativas          as integer
    field sequencia           as integer
    index seq is primary unique nr-ord-produ sequencia
    index proc            nr-ord-produ processada
    index rw-reserva      rw-reserva
    index codigo          nr-ord-produ quant-orig processada
    index tempo           nr-ord-produ quant-orig processada tempo tentativas
    index idx             nr-ord-produ item-pai it-codigo cod-roteiro op-codigo cod-depos cod-localiz
    index item            nr-ord-produ it-codigo.      
    

 &IF DEFINED (bf_man_sfc_lc) &THEN
 def temp-table tt-mat-reciclado 
     field nr-ord-produ like ord-prod.nr-ord-produ
     field es-codigo    like item-lista-compon.es-codigo
     field cod-depos    like reservas.cod-depos
     field cod-localiz  like reservas.cod-localiz
     field quant-orig   like reservas.quant-orig
     field quant-atend  like reservas.quant-atend
     field quant-requis like reservas.quant-requis
     field perc-requis  as dec format ">>9.99"
     field old-quant    like reservas.quant-requis
     field nro-ord-seq  as integer
     index codigo is unique primary nr-ord-produ nro-ord-seq es-codigo.
 &ENDIF
 

def var v-qtd-segs-inic-aux      as dec    no-undo.  
def var v-qtd-segs-fim-aux       as dec    no-undo.

def new global shared var grw-lote-item as rowid no-undo.
def new global shared var gc-estado     as char  no-undo.

def var c-item as char no-undo.
def var c-lote as char no-undo.

def input parameter p-rw-split-operac as rowid no-undo.
def output parameter p-ok as log no-undo.


def temp-table tt-digita no-undo
    field nr-ord-produ like ord-prod.nr-ord-produ
    field cod-estabel  like ord-prod.cod-estabel
    field nr-linha     like ord-prod.nr-linha
    field rw-lote-item as rowid 
    field arquivo      as char.

def var c-arq-erro as char no-undo.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* Name of first Frame and/or Browse and/or first Query                 */

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

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE c-cod-ctrab AS CHARACTER FORMAT "X(16)":U 
     LABEL "C.Trab." 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-des-ctrab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-des-oper AS CHARACTER FORMAT "X(34)":U 
     VIEW-AS FILL-IN 
     SIZE 29.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-operacao AS CHARACTER FORMAT "X(45)":U 
     LABEL "Operaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 25.72 BY .88 NO-UNDO.

DEFINE VARIABLE c-tit-split AS CHARACTER FORMAT "X(15)":U 
      VIEW-AS TEXT 
     SIZE 13.14 BY .67 NO-UNDO.

DEFINE VARIABLE fi-cod-operador AS CHARACTER FORMAT "99999-9":U INITIAL "000000" 
     LABEL "Operador" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-operador AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-trans-f AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Fim" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-trans-i AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Inicio" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-hr-trans-f AS CHARACTER FORMAT "99:99":U 
     VIEW-AS FILL-IN 
     SIZE 8.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-hr-trans-i AS CHARACTER FORMAT "99:99":U 
     VIEW-AS FILL-IN 
     SIZE 8.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-prod AS DECIMAL DECIMALS 4 FORMAT "->,>>>,>>9.9999":U INITIAL 0 
     LABEL "Qtd.Prod" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-ref AS DECIMAL DECIMALS 4 FORMAT "->,>>>,>>9.9999":U INITIAL 0 
     LABEL "Qtd.Refugada" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     FONT 0 NO-UNDO.

DEFINE VARIABLE fi-qtd-total AS DECIMAL DECIMALS 4 FORMAT "->,>>>,>>9.9999":U INITIAL 0 
     LABEL "Qtd.Requisitada" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tempo AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Tempo Operaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 8.72 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 77.86 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78.86 BY 3.42.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78.86 BY 4.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     c-cod-ctrab AT ROW 1.58 COL 16.86 COLON-ALIGNED
     c-des-ctrab AT ROW 1.58 COL 34.57 COLON-ALIGNED NO-LABEL
     split-operac.nr-ord-produ AT ROW 2.58 COL 16.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.72 BY .88
     split-operac.cod-item-op AT ROW 2.58 COL 55 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     c-operacao AT ROW 3.58 COL 16.86 COLON-ALIGNED
     split-operac.num-split-operac AT ROW 3.58 COL 43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     c-des-oper AT ROW 3.58 COL 47.86 COLON-ALIGNED NO-LABEL
     fi-cod-operador AT ROW 5 COL 12 COLON-ALIGNED
     fi-desc-operador AT ROW 5 COL 24 COLON-ALIGNED NO-LABEL
     fi-dt-trans-i AT ROW 5.96 COL 12 COLON-ALIGNED
     fi-hr-trans-i AT ROW 5.96 COL 24 COLON-ALIGNED NO-LABEL
     fi-qtd-total AT ROW 5.96 COL 60 COLON-ALIGNED
     fi-dt-trans-f AT ROW 6.92 COL 12 COLON-ALIGNED
     fi-hr-trans-f AT ROW 6.92 COL 24 COLON-ALIGNED NO-LABEL
     fi-qtd-prod AT ROW 6.92 COL 60 COLON-ALIGNED
     fi-tempo AT ROW 7.92 COL 24 COLON-ALIGNED
     fi-qtd-ref AT ROW 7.92 COL 60 COLON-ALIGNED
     bt-ok AT ROW 9.42 COL 2.72
     bt-cancelar AT ROW 9.42 COL 13.72
     bt-ajuda AT ROW 9.42 COL 68.72
     c-tit-split AT ROW 1 COL 2.14 NO-LABEL
     RECT-1 AT ROW 9.21 COL 1.72
     RECT-2 AT ROW 1.29 COL 1.57
     RECT-4 AT ROW 4.83 COL 1.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 9.79.


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
         HEIGHT             = 9.88
         WIDTH              = 80
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
     '~`':U +    /* Internal-Tables attribute */
   
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

 
    

    
    DISABLE fi-cod-operador fi-dt-trans-i fi-hr-trans-i fi-dt-trans-f fi-hr-trans-f bt-ok bt-cancelar bt-ajuda RECT-1 RECT-2 RECT-4 WITH FRAME F-Main.
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

 
   

   
    ENABLE UNLESS-HIDDEN /*fi-cod-operador*/ fi-dt-trans-i fi-hr-trans-i fi-dt-trans-f fi-hr-trans-f bt-ok bt-cancelar bt-ajuda RECT-1 RECT-2 RECT-4 WITH FRAME F-Main.

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
        if  c-nom-prog-dpc-mg97 <> "" then do:                  
            run value(c-nom-prog-dpc-mg97) (input "CHANGE-PAGE":U, 
                                            input "CONTAINER":U,
                                            input h-ctrl-tab,
                                            input frame F-Main:handle  ,
                                            input "",
            
                                            input ?).
            
        end.
        
         /* APPC */
        if  c-nom-prog-appc-mg97 <> "" then do:           
            run value(c-nom-prog-appc-mg97)  (input "CHANGE-PAGE":U, 
                                             input "CONTAINER":U,
                                             input h-ctrl-tab,
                                             input frame F-Main:handle  ,
                                             input "",
            
                                             input ?).
            
        end.
         /* UPC */
        if  c-nom-prog-upc-mg97 <> "" then do:                  
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
        if  c-nom-prog-dpc-mg97 <> "" then do:                  
            run value(c-nom-prog-dpc-mg97) (input "AFTER-CHANGE-PAGE":U, 
                                            input "CONTAINER":U,
                                            input h-ctrl-tab,
                                            input frame F-Main:handle  ,
                                            input "",
            
                                            input ?).
            
        end.
        
         /* APPC */
        if  c-nom-prog-appc-mg97 <> "" then do:           
            run value(c-nom-prog-appc-mg97)  (input "AFTER-CHANGE-PAGE":U, 
                                             input "CONTAINER":U,
                                             input h-ctrl-tab,
                                             input frame F-Main:handle  ,
                                             input "",
            
                                             input ?).
            
        end.
         /* UPC */
        if  c-nom-prog-upc-mg97 <> "" then do:                  
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


/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR FILL-IN c-cod-ctrab IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-des-ctrab IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-des-oper IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-operacao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-tit-split IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN split-operac.cod-item-op IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-operador IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-prod IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-ref IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-total IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tempo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN split-operac.nr-ord-produ IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN split-operac.num-split-operac IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */


 



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




ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:

  if input frame F-Main fi-dt-trans-i = ? then do:
     run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Data inicial inv†lida!").
     return no-apply.
  end.

  if input frame F-Main fi-dt-trans-f = ? then do:
     run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Data final inv†lida!").
     return no-apply.
  end.

  if (param-sfc.log-tipo-relogio 
      and substr(input frame F-Main fi-hr-trans-i,3,2) > '59')
      or  substr(input frame F-Main fi-hr-trans-i,1,2) > '23' then do:
          run utp/ut-msgs.p (input "show":U, 
                             input 3046, 
                             input "Inicio").
          apply 'entry' to fi-hr-trans-i in frame F-Main.
          return no-apply.
  end.

  if input frame F-Main fi-hr-trans-i = ? or
     length(input frame F-Main fi-hr-trans-i) < 4  then do:
     run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Hora inicial inv†lida!").
     return no-apply.
  end.

  if (param-sfc.log-tipo-relogio 
      and substr(input frame F-Main fi-hr-trans-f,3,2) > '59')
      or  substr(input frame F-Main fi-hr-trans-f,1,2) > '23' then do:
          run utp/ut-msgs.p (input "show":U, 
                             input 3046, 
                             input "Final").
          apply 'entry' to fi-hr-trans-f in frame F-Main.
          return no-apply.
  end.

  if input frame F-Main fi-hr-trans-f = ? or
     length(input frame F-Main fi-hr-trans-f) < 4  then do:
     run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Hora final inv†lida!").
     return no-apply.
  end.

  find first operador
       where operador.cod-operador = input frame F-Main fi-cod-operador 
       no-lock no-error.
  if not avail operador then do:
     run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Operador n∆o encontrado!").
     return no-apply.
  end.

  /**********************************************************************************/

  run utp/ut-acomp.p persistent set h-acomp.
  run pi-inicializar in h-acomp (input "Validando Itens...").
  run pi-desabilita-cancela in h-acomp.
  
  do transaction on error undo, return no-apply:
     run pi-efetiva-reportes.
     if return-value = "NOK" then do:
        run pi-finalizar in h-acomp.
        undo, return no-apply.
     end.
  end.
  run pi-finalizar in h-acomp.

  /*****************************************/
  /* Impressao da etiqueta                 *
  find ord-prod where 
       ord-prod.nr-ord-produ = input frame {&frame-name} split-operac.nr-ord-produ no-lock no-error.
  find last movto-mat 
       where movto-mat.nr-ord-produ = input frame {&frame-name} split-operac.nr-ord-produ
         and movto-mat.esp-docto    = 1
         and movto-mat.it-codigo    = ord-prod.it-codigo no-lock no-error.
  if avail movto-mat then do:
     find first lote-item 
          where lote-item.it-codigo = movto-mat.it-codigo
            and lote-item.lote      = movto-mat.lote no-lock no-error.
     if avail lote-item then do:
        for each tt-digita:
            delete tt-digita.
        end.
        create tt-digita.
        assign tt-digita.nr-ord-produ = movto-mat.nr-ord-produ
               tt-digita.cod-estabel  = ""
               tt-digita.nr-linha     = 0
               tt-digita.rw-lote-item = ?
               tt-digita.arquivo      = "\spool\etq" + string(movto-mat.nr-ord-produ) + ".lst".
        run sfc\essf0013p.w (input table tt-digita).
     end.
  end.
  *****************************************/
  assign p-ok = yes.

  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */




ON F5 OF fi-cod-operador IN FRAME F-Main /* Operador */
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
        
        assign c-lista-campo = string(fi-cod-operador:handle in frame F-Main) + '|':U + 'cod-operador'.
        
            assign c-lista-campo = c-lista-campo + chr(10) + string(fi-desc-operador:handle in frame F-Main) + '|':U + 'nom-operador'.
        
        
        
        
        
        
        
        
        
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




ON LEAVE OF fi-cod-operador IN FRAME F-Main /* Operador */
DO:
  find first operador
       where operador.cod-operador = input frame F-Main fi-cod-operador 
       no-lock no-error.
  assign fi-desc-operador:screen-value in frame F-Main = if avail operador then operador.nom-operador else "".

END.

/* _UIB-CODE-BLOCK-END */




ON MOUSE-SELECT-DBLCLICK OF fi-cod-operador IN FRAME F-Main /* Operador */
DO:
  apply "F5":U to self.
END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF fi-hr-trans-f IN FRAME F-Main
DO:
  run pi-horas (input 2).
END.

/* _UIB-CODE-BLOCK-END */




ON LEAVE OF fi-hr-trans-i IN FRAME F-Main
DO:
  run pi-horas (input 1).
END.

/* _UIB-CODE-BLOCK-END */







/* ***************************  Main Block  *************************** */
fi-cod-operador:load-mouse-pointer("image/lupa.cur") in frame F-Main.

procedure WinExec external "kernel32.dll":
    define input parameter prog_name    as character.
    define input parameter visual_style as short.
end procedure.


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
  DISPLAY c-cod-ctrab c-des-ctrab c-operacao c-des-oper fi-cod-operador 
          fi-desc-operador fi-dt-trans-i fi-hr-trans-i fi-qtd-total 
          fi-dt-trans-f fi-hr-trans-f fi-qtd-prod fi-tempo fi-qtd-ref 
          c-tit-split 
      WITH FRAME F-Main IN WINDOW w-window.
  IF AVAILABLE split-operac THEN 
    DISPLAY split-operac.nr-ord-produ split-operac.cod-item-op 
          split-operac.num-split-operac 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE /*fi-cod-operador*/ fi-dt-trans-i fi-hr-trans-i fi-dt-trans-f 
         fi-hr-trans-f bt-ok bt-cancelar bt-ajuda RECT-1 RECT-2 RECT-4 
      WITH FRAME F-Main IN WINDOW w-window.
      
      

/* ========================================================================
    Rotina para sugerir o operador logado */



  FIND FIRST operador WHERE
      operador.char-2 = c-seg-usuario
      NO-LOCK NO-ERROR.

  IF AVAIL operador THEN DO:
      ASSIGN fi-Cod-Operador:SCREEN-VALUE in FRAME F-Main = operador.cod-operador
             fi-desc-operador:SCREEN-VALUE in FRAME F-Main = operador.nom-operador.
  END.

  ASSIGN fi-Cod-Operador:SENSITIVE in FRAME F-Main = NO.


/* fim Rotina para sugerir o operador logado 
========================================================================   */


      
      
  
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

    assign c-programa-mg97 = caps("ESSF0303")
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
 

  /* Code placed here will execute AFTER standard behavior.    */
  find first param-cp no-lock no-error.
  find first param-sfc no-lock no-error.

   find split-operac
        where rowid(split-operac) = p-rw-split-operac no-lock no-error.

   find area-produc-ctrab
        where area-produc-ctrab.cod-area-produc = split-operac.cod-area-produc  AND
              area-produc-ctrab.cod-ctrab = split-operac.cod-ctrab 
      no-lock no-error.

   gr-area-produc-ctrab = rowid(area-produc-ctrab).


  find area-produc-ctrab
        where rowid(area-produc-ctrab) = gr-area-produc-ctrab no-lock no-error.
 
  find ord-prod 
       where ord-prod.nr-ord-produ = split-operac.nr-ord-produ exclusive-lock no-error.
  find item
        where item.it-codigo = ord-prod.it-codigo no-lock no-error.

    FIND FIRST operacao WHERE 
               operacao.op-codigo = split-operac.op-codigo AND
               operacao.it-codigo = split-operac.it-codigo NO-LOCK NO-ERROR.
    
    find first oper-ord
        where oper-ord.nr-ord-produ = split-operac.nr-ord-produ
          and oper-ord.it-codigo    = split-operac.it-codigo 
          and oper-ord.cod-roteiro  = split-operac.cod-roteiro
          and oper-ord.op-codigo    = split-operac.op-codigo no-lock no-error.

    find first param-global no-lock no-error.

    if avail oper-ord then DO:
        if  oper-ord.log-operac-final
        
         /* Ems */
        
        then do:
       
            if  can-find (first b-oper-ord
                          where b-oper-ord.nr-ord-produ     = split-operac.nr-ord-produ
                            and b-oper-ord.log-operac-final 
                            and rowid(b-oper-ord)          <> rowid(oper-ord)) then do:
                /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "v†rias",
                    input "*",
                    input "R") no-error.
                    
                    
/* ut-liter.i */                    
 
                run utp/ut-msgs.p (input 'show':U,
                                   input 361,
                                   input return-value).                      
                apply 'choose':U to bt-cancelar.
            end.
        end.
       
        assign gr-oper-ord = rowid(oper-ord).
    end.

    /**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Operaá∆o",
                    input "*",
                    input "R") no-error.
                    
                    
/* ut-liter.i */                    
 
    assign c-operacao:label in frame F-Main = trim(return-value)
           c-operacao = split-operac.it-codigo   + " / " +
                        split-operac.cod-roteiro + " / " +
                        string(split-operac.op-codigo,">>>>9").
  
    find ctrab
        where ctrab.cod-ctrab = area-produc-ctrab.cod-ctrab no-lock no-error.

    find first oper-ord
        where oper-ord.nr-ord-produ   = split-operac.nr-ord-produ
          and oper-ord.num-operac-sfc = split-operac.num-operac-sfc no-lock no-error.
  
    assign c-des-ctrab = if  avail ctrab
                         then ctrab.des-ctrab
                         else ""
           c-des-oper  = if  avail oper-ord
                         then oper-ord.descricao
                         else ""
           c-cod-ctrab = if  avail ctrab 
                         then ctrab.cod-ctrab
                         else "".
                       
    for each movto-mat
       where movto-mat.nr-ord-produ = split-operac.nr-ord-produ no-lock:

        if movto-mat.esp-docto = 1 then
           assign fi-qtd-prod = fi-qtd-prod + movto-mat.quantidade.
        if movto-mat.esp-docto = 8 then
           assign fi-qtd-prod = fi-qtd-prod - movto-mat.quantidade.
        IF movto-mat.esp-docto = 38 THEN 
            assign fi-qtd-prod = fi-qtd-prod + ( movto-mat.quantidade * 
                                                (IF movto-mat.tipo-trans = 1 THEN 1 ELSE -1) * 
                                                (IF ord-prod.val-relac-refugo-item <> 0 THEN ord-prod.val-relac-refugo-item ELSE 1)
                                                ).

        if movto-mat.esp-docto = 28 then
           assign fi-qtd-total = fi-qtd-total + movto-mat.quantidade.
        if movto-mat.esp-docto = 31 then
           assign fi-qtd-total = fi-qtd-total - movto-mat.quantidade.


    end.

    assign fi-qtd-ref = fi-qtd-total - fi-qtd-prod.
    if fi-qtd-ref <= 0 then
       assign fi-qtd-ref = 0.

    assign fi-dt-trans-i = today
           fi-dt-trans-f = today
           fi-hr-trans-i = replace(string(time, "HH:MM"), ":", "")
           fi-hr-trans-f = replace(string(time, "HH:MM"), ":", "")
           /*fi-qtd-total  = if ord-prod.qt-requisita > 0 then ord-prod.qt-requisita else 0
           fi-qtd-prod   = ord-prod.qt-produzida
           fi-qtd-ref    = if ord-prod.qt-requisita > ord-prod.qt-produzida then ord-prod.qt-requisita - ord-prod.qt-produzida
                           else 0*/ .

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
  


     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-efetiva-reportes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def buffer b-item for item.

for each tt-relatorio:
    delete tt-relatorio.
end.
for each tt-rel-aux:
    delete tt-rel-aux.
end.
for each tt-lote-a-config:
    delete tt-lote-a-config.
end.
for each tt-reservas:
    delete tt-reservas.
end.

RUN inbo/boin533.p PERSISTENT SET h-boin533.
RUN inbo/boin536.p PERSISTENT SET h-boin536.

for each tt-itens-reporte:
    delete tt-itens-reporte.
end.

find item where item.it-codigo = ord-prod.it-codigo no-lock no-error.

create tt-itens-reporte.
assign tt-itens-reporte.it-codigo   = ord-prod.it-codigo
       tt-itens-reporte.desc-item   = item.desc-item
       tt-itens-reporte.quantidade  = input frame F-Main fi-qtd-ref 
       tt-itens-reporte.cod-depos   = ord-prod.cod-depos
       tt-itens-reporte.localizacao = ""
       tt-itens-reporte.lote        = "RECICL":U
       tt-itens-reporte.val-lote    = 12/31/9999.

find first item
     where item.it-codigo = ord-prod.it-codigo no-lock no-error.
if avail item then do:
   find first item-uni-estab
        where item-uni-estab.cod-estabel = ord-prod.cod-estabel 
          and item-uni-estab.it-codigo   = item.it-codigo no-lock no-error.
   if avail item-uni-estab then do:
      if item-uni-estab.ind-refugo = 2 then
         assign tt-itens-reporte.localizacao = item.inform-compl.
   end.
end.

blk-geral:
for each tt-itens-reporte exclusive-lock
    transaction on stop undo blk-geral, next blk-geral
                on error undo blk-geral, next blk-geral:

    run pi-inicializar in h-acomp (input "Item: " + tt-itens-reporte.it-codigo).
    run pi-desabilita-cancela in h-acomp.

    for each tt-erro:
        delete tt-erro.
    end.

    run pi-acompanhar in h-acomp (input "Reportando OP...").
    
    run pi-processa-reporte-sfc.
    if return-value = "NOK":U then
       undo blk-geral, next blk-geral.
       
    /*run pi-transfere-refugo.
    if return-value = "NOK":U then
       undo blk-geral, next blk-geral.*/

end.

delete procedure h-boin533.
delete procedure h-boin536.

if can-find(first tt-relatorio) then do:
    
    run pi-acompanhar in h-acomp (input "Imprimindo...Aguarde":U).
    
    run sfc\essf0001rp.p (input table tt-relatorio,
                               input table tt-rel-aux,
                               input table tt-lote-a-config).
    
    DEF VAR cEditor     AS CHAR.
    DEF VAR vLog        AS LOGICAL.

    GET-KEY-VALUE SECTION "Datasul_EMS2":U KEY "Show-Report-Program":U VALUE cEditor.
    
    find first usuar_mestre
         where usuar_mestre.cod_usuario = c-seg-usuario no-lock no-error.
    assign c-arq-erro = if avail usuar_mestre then usuar_mestre.nom_dir_spool else "v:\temp":U
           c-arq-erro = c-arq-erro + "\" + c-seg-usuario + "_erro.txt".

    IF  SEARCH(cEditor) = ? THEN DO:
        ASSIGN  cEditor = OS-GETENV("windir") + "\notepad.exe"
                vLog    = YES.
        IF  SEARCH(cEditor) = ? THEN DO:
            ASSIGN  cEditor = OS-GETENV("windir") + "\write.exe".
            IF  SEARCH(cEditor) = ? THEN DO:
                RUN utp/ut-msgs.p (INPUT "show",
                                   INPUT 27576,
                                   INPUT c-arq-erro).
                ASSIGN  vLog    = NO.
            END.
        END.
    END.
    
    RUN WinExec (INPUT cEditor + CHR(32) + c-arq-erro, 1).
    
    return "NOK":U.

end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-horas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input parameter p-hora as int no-undo.
/*
  p-hora = 1 then inicio
  p-hora = 2 then fim
*/

def var de-tempo-oper as dec no-undo.
def var c-cod-calend  as char no-undo.
def var c-cod-area    as char no-undo.
def var c-gm-codigo   as char no-undo.

def var dat-inic-reporte as date no-undo.
def var dat-fim-reporte  as date no-undo.

def var c-hra-inic-rep   as char no-undo.
def var c-hra-fim-rep   as char no-undo.

def var h-boin469b as handle no-undo.

run inbo/boin469b.p persistent set h-boin469b.

if input frame F-Main fi-dt-trans-i <> ? and 
   input frame F-Main fi-dt-trans-f <> ? then do:

        if  param-sfc.log-tipo-relogio then do:
            run pi-formatted-time-to-sec (Input input frame F-Main fi-hr-trans-f,
                                          output de-qtd-segs-fim-reporte).
            run pi-formatted-time-to-sec (Input input frame F-Main fi-hr-trans-i,
                                          output de-qtd-segs-inic-reporte).
        end.
    
        else do :
            assign de-qtd-segs-inic-reporte = integer(input frame F-Main fi-hr-trans-i) * 36
                   de-qtd-segs-fim-reporte  = integer(input frame F-Main fi-hr-trans-f)  * 36.
         end.
 
        assign de-tempo-oper    = ?
               dat-inic-reporte = input frame F-Main fi-dt-trans-i
               dat-fim-reporte  = input frame F-Main fi-dt-trans-f
               c-cod-calend     = ?
               c-cod-area       = ?
               c-gm-codigo      = ?.

        run CalcularTemposCtrab in h-boin469b (input-output c-cod-area,
                                               input-output c-gm-codigo,
                                               input        ?,
                                               input-output c-cod-calend,
                                               Input-output dat-fim-reporte,
                                               Input-output de-qtd-segs-fim-reporte,
                                               Input-output dat-inic-reporte,
                                               Input-output de-qtd-segs-inic-reporte,
                                               Input-output de-tempo-oper).
               
        assign fi-tempo = de-tempo-oper.

        disp fi-tempo
             with frame F-Main.
end.

if input frame F-Main fi-dt-trans-i <> ? and 
   input frame F-Main fi-dt-trans-f <> ? then do:

   if p-hora = 1 then do:
      if  param-sfc.log-tipo-relogio then
          run pi-formatted-time-to-sec (Input input frame F-Main fi-hr-trans-i,
                                        output de-qtd-segs-inic-reporte).
      else
          assign de-qtd-segs-inic-reporte = int(input frame F-Main fi-hr-trans-i) * 36.

      assign dat-fim-reporte         = ?
             de-qtd-segs-fim-reporte = ?
             de-tempo-oper           = input frame F-Main fi-tempo
             c-cod-calend            = ?
             c-cod-area              = split-operac.cod-area-produc
             c-gm-codigo             = ?.

      run CalcularTemposCtrab in h-boin469b (input-output c-cod-area,
                                             input-output c-gm-codigo,
                                             input        ?,
                                             input-output c-cod-calend,
                                             Input-output dat-fim-reporte,
                                             Input-output de-qtd-segs-fim-reporte,
                                             Input-output dat-inic-reporte,
                                             Input-output de-qtd-segs-inic-reporte,
                                             Input-output de-tempo-oper).
      if  param-sfc.log-tipo-relogio then
          run pi-sec-to-formatted-time (Input de-qtd-segs-fim-reporte,
                                        output c-hra-fim-rep).
      else
          assign c-hra-fim-rep = string(int(de-qtd-segs-fim-reporte / 36), "9999").

      assign fi-hr-trans-f = c-hra-fim-rep
             fi-dt-trans-f = dat-fim-reporte.

      disp fi-hr-trans-f
           fi-dt-trans-f
          with frame F-Main.
   end.

   if p-hora = 2 then do:
      
      if param-sfc.log-tipo-relogio then
         run pi-formatted-time-to-sec (Input input frame F-Main fi-hr-trans-f,
                                       output de-qtd-segs-fim-reporte).
      else
         assign de-qtd-segs-fim-reporte = integer(input frame F-Main fi-hr-trans-f) * 36.

        assign dat-inic-reporte         = ?
               de-qtd-segs-inic-reporte = ?
               de-tempo-oper            = input frame F-Main fi-tempo
               c-cod-calend     = ?
               c-cod-area       = ?
               c-gm-codigo      = ?.

        run CalcularTemposCtrab in h-boin469b (input-output c-cod-area,
                                               input-output c-gm-codigo,
                                               input        ?,
                                               input-output c-cod-calend,
                                               Input-output dat-fim-reporte,
                                               Input-output de-qtd-segs-fim-reporte,
                                               Input-output dat-inic-reporte,
                                               Input-output de-qtd-segs-inic-reporte,
                                               Input-output de-tempo-oper).
                                               
        if param-sfc.log-tipo-relogio then
           run pi-sec-to-formatted-time (Input de-qtd-segs-inic-reporte,
                                         output c-hra-inic-rep).
        else
           assign c-hra-inic-rep = string(int(de-qtd-segs-inic-reporte / 36), "9999"). 

        assign fi-hr-trans-i = c-hra-inic-rep
               fi-dt-trans-i = dat-inic-reporte.

        disp fi-hr-trans-i
             fi-dt-trans-i
            with frame F-Main.
   end.


end.

if  param-sfc.log-tipo-relogio then do:
    run pi-formatted-time-to-sec (Input input frame F-Main fi-hr-trans-i,
                                  output v-qtd-segs-inic-aux).
    run pi-formatted-time-to-sec (Input input frame F-Main fi-hr-trans-f,
                                  output v-qtd-segs-fim-aux).
end.
else do:
    assign v-qtd-segs-inic-aux = int(input frame F-Main fi-hr-trans-i) * 36
           v-qtd-segs-fim-aux  = int(input frame F-Main fi-hr-trans-f) * 36.
end.

delete procedure h-boin469b.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-processa-reporte-sfc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER bf-item FOR ITEM.

    find first param-cp no-lock no-error.
     
    assign l-erro-ver = no.

    for each tt-refugo:
        delete tt-refugo.
    end.
    for each tt-rep-prod:
        delete tt-rep-prod.
    end.
    for each tt-reporte:
        delete tt-reporte.
    end.

    run pi-requisita-material.
    if return-value = "NOK":U then
       return "NOK":U.

    assign i-ind-refugo = INT (f-item-uni-estab (input ITEM.it-codigo,
                                                 input ord-prod.cod-estabel,
                                                 "ind-refugo":U)).

    find first oper-ord
         where oper-ord.nr-ord-produ = ord-prod.nr-ord-produ no-lock no-error.
    if not avail oper-ord then do:
       create tt-relatorio.
       assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
              tt-relatorio.descricao = "Operaá∆o da Ordem n∆o encontrada!".
       return "NOK":U.
    end.

    find first split-operac
         where split-operac.nr-ord-produ   = oper-ord.nr-ord-produ
           and split-operac.num-operac-sfc = oper-ord.num-operac-sfc exclusive-lock no-error.
    if not avail split-operac then
       run GerarOrdemSFC in h-boin533 (buffer ord-prod).

    FIND FIRST estab-mfg NO-LOCK
        WHERE estab-mfg.cod-estabel = ord-prod.cod-estabel NO-ERROR.
    
    create tt-rep-prod.
    assign tt-rep-prod.cod-versao-integracao = 1
           tt-rep-prod.nr-ord-prod       = ord-prod.nr-ord-prod
           tt-rep-prod.it-codigo         = ord-prod.it-codigo
           tt-rep-prod.tipo              = ord-prod.rep-prod
           tt-rep-prod.op-codigo         = if avail oper-ord then oper-ord.op-codigo    else 0
           tt-rep-prod.cod-roteiro       = if avail oper-ord then oper-ord.cod-roteiro  else ""
           tt-rep-prod.it-oper           = if avail oper-ord then oper-ord.it-codigo    else ""
           tt-rep-prod.pto-controle      = if avail oper-ord then oper-ord.pto-controle else 0
           tt-rep-prod.data              = input frame F-Main fi-dt-trans-f
           tt-rep-prod.un                = ord-prod.un
           tt-rep-prod.qt-reporte        = tt-itens-reporte.quantidade
           tt-rep-prod.qt-refugo         = tt-itens-reporte.quantidade
           tt-rep-prod.lote-serie        = tt-itens-reporte.lote
           tt-rep-prod.cod-refer         = ord-prod.cod-refer
           tt-rep-prod.dt-vali-lote      = tt-itens-reporte.val-lote
           tt-rep-prod.ct-refugo         = if avail estab-mfg then estab-mfg.ct-refugo else param-cp.ct-refugo
           tt-rep-prod.sc-refugo         = if avail estab-mfg then estab-mfg.sc-refugo else param-cp.sc-refugo
           tt-rep-prod.prog-seg          = 'sf0303'
           tt-rep-prod.nro-docto         = STRING(ord-prod.nr-ord-prod)
           tt-rep-prod.carrega-reservas  = yes
           tt-rep-prod.reserva           = yes
           tt-rep-prod.procura-saldos    = no
           tt-rep-prod.requis-automatica = no
           tt-rep-prod.baixa-reservas    = 1.

    assign tt-rep-prod.cod-depos-sai     = ?
           tt-rep-prod.cod-local-sai     = ?
           tt-rep-prod.cod-depos         = tt-itens-reporte.cod-depos
           tt-rep-prod.cod-localiz       = ""
           tt-rep-prod.dep-refugo        = substring(param-cp.char-2,1,3)
           tt-rep-prod.loc-refugo        = tt-itens-reporte.localizacao.

    IF ord-prod.cod-item-refugo <> "" THEN DO:
        FIND FIRST bf-item NO-LOCK
            WHERE bf-item.it-codigo = ord-prod.cod-item-refugo no-error.

        IF AVAIL bf-item THEN
            ASSIGN tt-rep-prod.dep-refugo = bf-item.deposito-pad
                   tt-rep-prod.loc-refugo = bf-item.cod-localiz.
    END.

        
    /*Speto - 22/01/2004*/
    create tt-refugo.
    assign tt-refugo.nr-ord-produ = tt-rep-prod.nr-ord-produ
           tt-refugo.codigo-rejei = if ord-prod.cod-estabel = "413" OR ord-prod.cod-estabel = "423" then 2 else 1 /*solic-318*/
           tt-refugo.qt-refugo    = tt-rep-prod.qt-refugo.
    /*Speto - 22/01/2004*/                                                      

    /* n∆o tem a conta reduzida de aplicacao nessa tabela
    if  tt-rep-prod.ct-codigo = "" and 
        item.tipo-contr = 3 /* Consignado */ then do:
        
        find first estab-mfg where 
                   estab-mfg.cod-estabel = ord-prod.cod-estabel no-lock no-error.
        if  avail estab-mfg and 
                  estab-mfg.conta-aplic <> ? then
            assign tt-rep-prod.ct-codigo = estab-mfg.conta-aplic. 
        else 
        
            assign tt-rep-prod.conta-aplic = param-cp.conta-aplic.
    end.*/
    
    run cpp/cpapi001.p (input-output table tt-rep-prod,
                        input        table tt-refugo,
                        input        table tt-res-neg,
                        input        table tt-apont-mob,
                        input-output table tt-erro,
                        input yes).

    if return-value = "NOK":U then do:
       create tt-relatorio.
       assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
              tt-relatorio.descricao = "Erro ao reportar a OP.".
       for each tt-erro:
           create tt-rel-aux.
           assign tt-rel-aux.it-codigo = tt-relatorio.it-codigo
                  tt-rel-aux.i-sequen  = tt-erro.i-sequen
                  tt-rel-aux.cd-erro   = tt-erro.cd-erro
                  tt-rel-aux.descricao = tt-erro.mensagem.
       end.

       /*Speto - 22/01/2004
         Para dispistar a API que retorna mensagem de informaá∆o com "NOK"
       */
       if temp-table tt-erro:has-records then do:
          for each tt-erro:
              find cadast_msg
                    where cadast_msg.cdn_msg = tt-erro.cd-erro no-lock no-error.
              if avail cadast_msg and 
                 cadast_msg.idi_tip_msg = 1 /*Erro*/ then
                 return "NOK":U.
          end.
       end.
       /*Speto - 22/01/2004*/
       
    end.
       
        
    /*** Atualizaá∆o das tabelas do Ch∆o de F†brica ***/
    find first ctrab
         where ctrab.cod-ctrab = area-produc-ctrab.cod-ctrab no-lock no-error.
    if not avail ctrab then do:
       create tt-relatorio.
       assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
              tt-relatorio.descricao = "Centro de trabalho n∆o encontrado!".
       return "NOK":U.
    end.
    

    create tt-reporte.
    assign tt-reporte.rw-split-operac       = rowid(split-operac)
           tt-reporte.cod-ferr-prod         = ""
           tt-reporte.dat-fim-setup         = ?
           tt-reporte.dat-inic-setup        = ?
           tt-reporte.qtd-segs-fim-setup    = 0
           tt-reporte.qtd-segs-inic-setup   = 0
           tt-reporte.dat-fim-reporte       = input frame F-Main fi-dt-trans-f
           tt-reporte.dat-inic-reporte      = input frame F-Main fi-dt-trans-i
           tt-reporte.qtd-operac-refgda     = tt-itens-reporte.quantidade
           tt-reporte.qtd-operac-aprov      = 0
           tt-reporte.qtd-operac-reptda     = tt-itens-reporte.quantidade
           tt-reporte.qtd-operac-retrab     = 0
           
           tt-reporte.qtd-segs-fim-reporte  = v-qtd-segs-fim-aux
           tt-reporte.qtd-segs-inic-reporte = v-qtd-segs-inic-aux

           tt-reporte.cod-equipe            = input frame F-Main fi-cod-operador
           tt-reporte.num-contador-inic     = 0
           tt-reporte.num-contador-fim      = 0
    
        /*** ParÉmetros p/ reporte ***/
           tt-reporte.baixa-reservas        = 1
           tt-reporte.requisicao-automatica = no
           tt-reporte.busca-saldos          = no
           tt-reporte.requisita-configurado = no.

    
    find first tt-rep-prod no-error.
    if not avail tt-rep-prod then do:
       create tt-relatorio.
       assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
              tt-relatorio.descricao = "Reporte n∆o efetuado! SFC".
       return "NOK":U.
    end.

    find last rep-oper-ctrab where 
              rep-oper-ctrab.nr-ord-produ = split-operac.nr-ord-produ NO-LOCK no-error.
    if  avail rep-oper-ctrab then
        assign i-num-seq-rep = rep-oper-ctrab.num-seq-rep + 1.
    else
        assign i-num-seq-rep = 1.
        
    find last ord-rep no-lock
        where ord-rep.nr-ord-produ = split-operac.nr-ord-produ no-error.        
    
    create tt-rep-oper-ctrab.
    buffer-copy tt-reporte to tt-rep-oper-ctrab
    assign tt-rep-oper-ctrab.nr-ord-produ   = split-operac.nr-ord-produ
           tt-rep-oper-ctrab.num-seq-rep    = i-num-seq-rep
           tt-rep-oper-ctrab.num-operac-sfc = split-operac.num-operac-sfc
           tt-rep-oper-ctrab.num-split-oper = split-operac.num-split-oper
           tt-rep-oper-ctrab.cod-ctrab      = ctrab.cod-ctrab
           tt-rep-oper-ctrab.nr-reporte     = if avail ord-rep then ord-rep.nr-reporte else 0.

    run GerarRepOperCtrab in h-boin536 (input table tt-rep-oper-ctrab,
                                        input table tt-rep-refugo-oper,
                                        input table tt-rep-ic-oper,
                                        input table tt-rep-ic-oper-tab).

    if (oper-ord.val-perc-avanco   >= param-cp.var-rep or   
        param-cp.var-rep           >= 999) then do:
        
        if can-find(first b-oper-ord
                    where b-oper-ord.nr-ord-produ     = oper-ord.nr-ord-produ
                      and not b-oper-ord.log-operac-final 
                      and b-oper-ord.val-perc-avanco  < oper-ord.val-perc-avanco) then do:
            run utp/ut-msgs.p (input "msg":U,
                               input (if  param-sfc.log-consid-rede-pert  
                                      then 17988
                                      else 17987),
                               input "").
            create tt-relatorio.
            assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
                   tt-relatorio.descricao = return-value + " SFC".
            return "NOK":U.
        end.

        
        find first lin-prod where 
                   lin-prod.cod-estabel = ord-prod.cod-estabel and 
                   lin-prod.nr-linha    = ord-prod.nr-linha no-lock no-error.
        
        
        if avail lin-prod   AND 
           param-cp.ver-req and 
           lin-prod.sum-requis = 2 /*OP Servico*/ THEN
           run cpp/cp0311g.p (input rowid(ord-prod)      ,
                              output l-erro-ver          ,
                              input "2"                  ,
                              input ord-prod.qt-reportada,
                              input yes                  ,
                              input tt-reporte.baixa-reservas).
            
    end.

    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-requisita-material :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var h-cpapi012 as handle no-undo.
def var c-return   as char   no-undo.

for each tt-requis:
    delete tt-requis.
end.

create tt-requis.
assign tt-requis.tipo-trans            = 1
       tt-requis.nr-ord-produ          = ord-prod.nr-ord-produ
       tt-requis.quantidade            = ord-prod.qt-ordem
       tt-requis.data                  = input frame F-Main fi-dt-trans-f
       tt-requis.item-ini              = ""
       tt-requis.item-fim              = "ZZZZZZZZZZZZZZZZ"
       tt-requis.procura-saldos        = no 
       tt-requis.carrega-reservas      = no
       tt-requis.op-codigo-ini         = 0
       tt-requis.op-codigo-fim         = 999999
       tt-requis.prog-seg              = "polsfc001"
       tt-requis.cod-versao-integracao = 1
       tt-requis.nro-docto             = string(ord-prod.nr-ord-produ)
       tt-requis.deposito-ini          = ""
       tt-requis.deposito-fim          = "ZZZ".
    
    
run cpp/cpapi012.p persistent set h-cpapi012  (input        table tt-requis,
                                               input-output table tt-erro,
                                               input        yes).     

run pi-recebe-tt-reservas in h-cpapi012 (input        table tt-reservas).
run pi-processa-requis    in h-cpapi012 (input        table tt-requis,
                                         input-output table tt-erro,
                                         input        yes).
assign c-return = return-value.
run pi-finalizar in h-cpapi012.

if c-return = "NOK":U then do:
   create tt-relatorio.
   assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
          tt-relatorio.descricao = "Erro ao requisitar os materiais!".
   for each tt-erro:
       create tt-rel-aux.
       assign tt-rel-aux.it-codigo = tt-relatorio.it-codigo
              tt-rel-aux.i-sequen  = tt-erro.i-sequen
              tt-rel-aux.cd-erro   = tt-erro.cd-erro
              tt-rel-aux.descricao = tt-erro.mensagem.
       create tt-rel-aux.
       assign tt-rel-aux.it-codigo = tt-relatorio.it-codigo
              tt-rel-aux.i-sequen  = tt-erro.i-sequen
              tt-rel-aux.cd-erro   = tt-erro.cd-erro
              tt-rel-aux.descricao = "Verifique as parametrizaá‰es do Item: Localizaá∆o Unica":U.
   end.
   return "NOK":U.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi-transfere-refugo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var i-empresa as char no-undo.
    

    find item-uni-estab 
         where item-uni-estab.cod-estabel = ord-prod.cod-estabel
           and item-uni-estab.it-codigo = ord-prod.it-codigo no-lock no-error.
    if not avail item-uni-estab then do:
       return "NOK":U.
    end.

    if item-uni-estab.ind-refugo = 1  then 
       return "OK":U.

    if item.inform-compl         = "" then 
       return "OK":U.

    find first param-estoq no-lock no-error.
    
    for each tt-movto:
        delete tt-movto.
    end.

    FIND estabelec WHERE estabelec.cod-estabel = ord-prod.cod-estabel NO-LOCK NO-ERROR.


    assign i-empresa = estabelec.ep-codigo.

    

    find first cta_ctbl 
         where cta_ctbl.cod_plano_cta_ctbl = "Brasil"
           AND cta_ctbl.cod_cta_ctbl       = param-estoq.ct-tr-transf no-lock no-error.
 
    /*SAIDA*/
    create tt-movto.
    assign
        tt-movto.cod-versao-integracao  = 1
        tt-movto.ct-codigo              = param-estoq.ct-tr-transf
        tt-movto.sc-codigo              = param-estoq.sc-tr-transf
        tt-movto.cod-prog-orig          = "ESSF0303"
        tt-movto.tipo-trans             = 2
        tt-movto.esp-docto              = 33
        tt-movto.dt-trans               = input frame F-Main fi-dt-trans-i
        tt-movto.dt-vali-lote           = 12/31/9999
        tt-movto.nro-docto              = ""
        tt-movto.serie-docto            = "SFCT"
        tt-movto.cod-depos              = substring(param-cp.char-2,1,3)
        tt-movto.cod-estabel            = ord-prod.cod-estabel
        tt-movto.it-codigo              = item.it-codigo
        tt-movto.cod-refer              = ""
        tt-movto.cod-localiz            = ""
        tt-movto.lote                   = "RECICL":U
        tt-movto.quantidade             = tt-itens-reporte.quantidade
        tt-movto.un                     = item.un
        tt-movto.usuario                = c-seg-usuario.
 
    /**************************entrada***************************/
    
    

    find first cta_ctbl 
         where cta_ctbl.cod_plano_cta_ctbl = "Brasil"
           AND cta_ctbl.cod_cta_ctbl       = param-estoq.ct-tr-transf no-lock no-error.
 
    /*ENTRADA*/

    create tt-movto.
    assign
        tt-movto.cod-versao-integracao = 1
        tt-movto.ct-codigo             = param-estoq.ct-tr-transf
         tt-movto.sc-codigo            = param-estoq.sc-tr-transf
        tt-movto.cod-prog-orig         = "ESSF0303"
        tt-movto.dt-trans              = input frame F-Main fi-dt-trans-i
        tt-movto.dt-vali-lote          = 12/31/9999
        tt-movto.nro-docto             = ""
        tt-movto.serie-docto           = "SFCT"
        tt-movto.cod-depos             = substring(param-cp.char-2, 1,3)
        tt-movto.cod-estabel           = ord-prod.cod-estabel
        tt-movto.it-codigo             = item.it-codigo
        tt-movto.cod-refer             = ""
        tt-movto.cod-localiz           = item.inform-compl
        tt-movto.lote                  = "RECICL":U
        tt-movto.quantidade            = tt-itens-reporte.quantidade
        tt-movto.un                    = item.un
        tt-movto.tipo-trans            = 1
        tt-movto.esp-docto             = 33
        tt-movto.usuario               = c-seg-usuario.
 
    /* Efetua a atualizacao de Estoque ---------------------------------------*/
    run cep/ceapi001.p (input-output table tt-movto,
                        input-output table tt-erro,
                        input yes).
 
    find first tt-erro
        no-lock no-error.
    if avail tt-erro then do:
       for each tt-erro:
           create tt-relatorio.
           assign tt-relatorio.it-codigo = tt-itens-reporte.it-codigo
                  tt-relatorio.descricao = string(tt-erro.cd-erro) + " - " + tt-erro.mensagem.
       end.
       return "NOK":U.
    end.

    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this JanelaDetalhe, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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


