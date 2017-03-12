/********************************************************************************
 Programa   : ESFT0039RP.P
 Autor      : Grupo Unigel - Ercole Ricci
 Data       : Janeiro de 2010
 Descri‡Æo  : Listagem das Ultimas vendas
 *******************************************************************************/
DEF BUFFER empresa FOR mgmulti.empresa.
/* ***************************  Definitions  ************************** */

/* Temporary Table Definitions ---                                      */
define temp-table tt-param no-undo
    field destino           as integer
    field arquivo           as char format "x(35)"
    field usuario           as char format "x(12)"
    field data-exec         as date
    field hora-exec         as integer
    field classifica        as integer
    field desc-classifica   as char format "x(40)"
    field estab-ini         like estabelec.cod-estabel
    field estab-fim         like estabelec.cod-estabel
    FIELD data-saldo        like ordem-compra.data-pedido
    FIELD data-periodo      like ordem-compra.data-pedido.

define temp-table tt-digita no-undo
    field natureza         LIKE movto-estoq.nat-operacao
    index codigo
          natureza ASCENDING.
   
def temp-table tt-raw-digita
   field raw-digita      as raw.

/* Parameters Definitions ---                                           */

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

/* Transfer Definitions */

create tt-param.
raw-transfer raw-param to tt-param.

/* Definicao de Variaveis */

{utp/ut-glob.i}
{include/i-prgvrs.i "ESFT0039RP" "2.00.00.000"}

{include/i-rpvar.i}

assign c-programa     = "ESFT0039RP"
       c-versao       = "2.00.00.000"
       c-sistema      = "Custo"
       c-titulo-relat = "LISTAGEM DAS ULTIMAS VENDAS".
             
{include/i-rpc255.i}

def var c-cab-152-1 as c form "x(300)" no-undo.
def var c-cab-152-2 as c form "x(300)" no-undo.
def var c-rod-152-1 as c form "x(300)" no-undo.

assign c-cab-152-1 = fill("-",300)
       c-cab-152-2 = fill("-",300).

/* assign c-titulo-relat = fill(" ",integer((100 - length(c-titulo-relat)) / 2)) + */
/*                        c-titulo-relat.                                          */

FIND FIRST param-global NO-LOCK NO-ERROR.

FIND empresa WHERE
    empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR.

form header
     c-cab-152-1            AT 01
     empresa.nome FORM 'x(50)'          AT 01
     c-titulo-relat         AT 150
     " Folha:"              AT 290
     page-number form ">>>9"
     c-cab-152-2            AT 01 
    skip(1)
    with no-box page-top width 500 frame f-cabec.

assign c-rod-152-1 = " " + "DATASUL - " + c-programa + " - " +
       c-versao + " - " +
                  string(today,"99/99/9999") + " - " + string(time, "HH:MM:SS").
assign c-rod-152-1 = fill("-", (300 - length(c-rod-152-1))) + c-rod-152-1.

form header
     c-rod-152-1
     with no-box page-bottom width 400 frame f-rodape.

/* ************** Forms ******************* */

/* ************** Programa Prinicpal ******************* */

DEF VAR c-arq             AS CHAR                NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR                NO-UNDO.   
DEF VAR l-cria            AS LOG                 NO-UNDO.
DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
DEF VAR i-linha           AS INT    INIT 7             NO-UNDO.
DEF VAR c-coluna          AS char                NO-UNDO.

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.


def new global shared var i-ep-codigo-usuario  like mgmulti.empresa.ep-codigo no-undo.
DEF VAR h-acomp AS HANDLE                  NO-UNDO.
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
DEFINE VARIABLE dt-refe AS DATE       NO-UNDO.
DEFINE VARIABLE c-grup-estoques AS CHARACTER  NO-UNDO.
DEF VAR C-NR-NOTA-FIS       LIKE it-nota-fisc.nr-nota-fis               NO-UNDO.
DEF VAR d-dt-emis-nota      LIKE it-nota-fisc.dt-emis-nota              NO-UNDO.
DEF VAR C-NR-NOTA-FIS-ex       LIKE it-nota-fisc.nr-nota-fis               NO-UNDO.
DEF VAR d-dt-emis-nota-ex      LIKE it-nota-fisc.dt-emis-nota              NO-UNDO.

def var de-tot-saldo        like movto-estoq.quantidade                 no-undo init 0.
def var de-tot-aux          like movto-estoq.quantidade                 no-undo init 0.
def var de-tot-venda        like movto-estoq.quantidade                 no-undo init 0.
def var total-quantidade    like saldo-estoq.qtidade-atu                no-undo.
DEFINE VARIABLE c-emitente-in AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-emitente-ex AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-cod-emitente-in AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-cod-emitente-ex AS INTEGER    NO-UNDO.
DEFINE TEMP-TABLE tt-item
    FIELD cod-estabel      LIKE ITEM.cod-estabel
    FIELD it-codigo        LIKE ITEM.it-codigo
    FIELD un               LIKE ITEM.un
    FIELD qtdade-venda     LIKE saldo-estoq.qtidade-atu
    FIELD quantidade       LIKE saldo-estoq.qtidade-atu
    FIELD saldo-data       LIKE saldo-estoq.qtidade-atu
    FIELD Data-Final       like ordem-compra.data-emissao         
    FIELD preco-unit       LIKE movto-estoq.valor-mat-m[1]
    FIELD preco-total      LIKE movto-estoq.valor-mat-m[1]
    FIELD preco-total-ggf  LIKE movto-estoq.valor-ggf-m[1]
    FIELD preco-total-mob  LIKE movto-estoq.valor-mob-m[1]
    
    index codigo
       cod-estabel  ASCENDING 
       it-codigo    ASCENDING.

DEF VAR c-periodo-ini       AS DATE NO-UNDO.
DEF VAR c-periodo-val       AS DATE NO-UNDO.

DEF VAR c-cod-estabel-ini   LIKE estabelec.cod-estabel  NO-UNDO.
DEF VAR c-cod-estabel-fim   LIKE estabelec.cod-estabel  NO-UNDO.

DEF VAR c-arquivo-rel   AS CHAR FORMAT "X(45)"   NO-UNDO.
DEF VAR v-aliq-pis      AS DECIMAL NO-UNDO.
DEF VAR v-aliq-cof      AS DECIMAL NO-UNDO.
DEF VAR aux-vl-ipi-it   AS DECIMAL NO-UNDO.
DEF VAR aux-vl-icms-it  AS DECIMAL NO-UNDO.
DEF VAR v-pis           AS DECIMAL NO-UNDO.
DEF VAR v-cofins        AS DECIMAL NO-UNDO.
DEF VAR encargos-financ AS DECIMAL  NO-UNDO.
DEF VAR aux-vl-despes-it AS DECIMAL NO-UNDO.
DEF VAR valor-liquido   AS DECIMAL  NO-UNDO.
DEF VAR valor-liquido-in   AS DECIMAL  NO-UNDO.
DEF VAR valor-liquido-ex   AS DECIMAL  NO-UNDO.
DEF VAR qt-faturada-in   AS DECIMAL  NO-UNDO.
DEF VAR qt-faturada-ex   AS DECIMAL  NO-UNDO.
DEF VAR kg-faturada-in   AS DECIMAL  NO-UNDO.
DEF VAR kg-faturada-ex   AS DECIMAL  NO-UNDO.
DEF VAR vl-custo-in   AS DECIMAL  NO-UNDO.
DEF VAR vl-custo-ex   AS DECIMAL  NO-UNDO.

DEF VAR de-qtd-peso     AS DECIMAL  NO-UNDO.
DEF VAR de-val-peso     AS DECIMAL  NO-UNDO.

DEF VAR aux-qt-faturada   LIKE it-nota-fisc.qt-faturada[1]  NO-UNDO.
DEF VAR aux-vl-tot-item   LIKE it-nota-fisc.vl-tot-item     NO-UNDO.
DEF VAR aux-vl-merc-liq   LIKE it-nota-fisc.vl-merc-liq     NO-UNDO.

ASSIGN c-periodo-ini        = tt-param.data-saldo
       c-periodo-val        = tt-param.data-periodo
       c-cod-estabel-ini    = tt-param.estab-ini
       c-cod-estabel-fim    = tt-param.estab-fim.

ASSIGN c-arquivo-rel = tt-param.arquivo.

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "Gerando Dados...").

/**********************************
unigel   01 e 02
acrinor  11 e 13
proquigel  02 12 13 21 22 
**********************************/

/* coloque aqui as respectivas grup-estoques */

/************* ANTES DA MIGRAÇÃO **********************
IF  i-ep-codigo-usuario = 380 THEN
    ASSIGN c-grup-estoques = '01,02'.          /*coloque aqui as respectivas grup-estoques*/
IF  i-ep-codigo-usuario = 390 THEN    
    ASSIGN c-grup-estoques = '02,12,13,21,22'. /*coloque aqui as respectivas grup-estoques*/
IF  i-ep-codigo-usuario = 400 THEN
    ASSIGN c-grup-estoques = '11,13'.
*******************************************************/

IF  SUBSTRING(STRING(tt-param.estab-ini,'999'),1,2) = '38'  OR SUBSTRING(STRING(tt-param.estab-ini,'999'),1,3) = '435' OR SUBSTRING(STRING(tt-param.estab-ini,'999'),1,3) = '432' or SUBSTRING(STRING(tt-param.estab-ini,'999'),1,3) = '412' THEN /*solic-318*/
    ASSIGN c-grup-estoques = '11,12'.       /*coloque aqui as respectivas grup-estoques*/
IF  SUBSTRING(STRING(tt-param.estab-ini,'999'),1,2) = '39' THEN
    ASSIGN c-grup-estoques = '12,22,31,32'. /*coloque aqui as respectivas grup-estoques*/
IF  SUBSTRING(STRING(tt-param.estab-ini,'999'),1,2) = '40' THEN
    ASSIGN c-grup-estoques = '21,22'.

FOR EACH grup-estoque NO-LOCK
    WHERE index(c-grup-estoques,STRING(grup-estoque.ge-codigo,'99')) <> 0:

    FOR  EACH ITEM NO-LOCK
        WHERE ITEM.ge-codigo = grup-estoque.ge-codigo:

      /* ************ Pesquisa Saldo de Itens ******************** */

     RUN pi-acompanhar IN h-acomp (INPUT "Item:" +  ITEM.it-codigo).


    ASSIGN de-tot-saldo = 0
           de-tot-aux   = 0.
                   
    FOR EACH saldo-estoq NO-LOCK 
        WHERE saldo-estoq.it-codigo    = ITEM.it-codigo 
        AND   saldo-estoq.cod-estabel >= c-cod-estabel-ini
        AND   saldo-estoq.cod-estabel <= c-cod-estabel-fim
       /* AND   saldo-estoq.qtidade-atu > 0*/  USE-INDEX ITEM:
               
        FIND FIRST tt-item WHERE 
            tt-item.it-codigo   = item.it-codigo            AND 
            tt-item.cod-estabel = saldo-estoq.cod-estabel   NO-ERROR.
        IF  NOT AVAIL tt-item THEN DO:

            CREATE tt-item.
            ASSIGN tt-item.cod-estabel    = saldo-estoq.cod-estabel 
                   tt-item.it-codigo      = ITEM.it-codigo
                   tt-item.un             = ITEM.un
                   tt-item.saldo-data     = saldo-estoq.qtidade-atu
                   tt-item.data-final     = TODAY.
        END. /* NÆo Existe tt-item */
        ELSE  
            ASSIGN tt-item.saldo-data = tt-item.saldo-data + saldo-estoq.qtidade-atu.
               
    END. /* Fim for each saldo-estoq */
            
  END. /* FIm for each Item */
END. /*fim for each grup-estoque*/
/* *************** Retrocendendo tt-item ************************ */       

FOR EACH tt-item  EXCLUSIVE-LOCK :       

    FIND FIRST item WHERE 
        item.it-codigo = tt-item.it-codigo NO-LOCK NO-ERROR.
    IF  NOT AVAIL item THEN 
        NEXT. 

    ASSIGN de-tot-saldo = tt-item.saldo-data.
    
    /* Atualiza o saldo do produto para a data de transacao */

    FOR EACH movto-estoq NO-LOCK 
        WHERE movto-estoq.it-codigo   = ITEM.it-codigo
        AND   movto-estoq.cod-estabel = tt-item.cod-estabel 
        AND   movto-estoq.dt-trans    > c-periodo-ini
        AND   movto-estoq.dt-trans    <= tt-item.data-final
        USE-INDEX item-data :
      
        IF  movto-estoq.tipo-trans = 1 THEN
            ASSIGN de-tot-saldo = de-tot-saldo - movto-estoq.quantidade.
        ELSE
            ASSIGN de-tot-saldo = de-tot-saldo + movto-estoq.quantidade.
                                  
    END. /* FIm for each movto-estoq */

    ASSIGN tt-item.quantidade = de-tot-saldo.
                 
    /* ***** Buscando Valores do Item **** */

    FIND FIRST sl-it-per WHERE
        sl-it-per.cod-estabel = tt-item.cod-estabel  AND 
        sl-it-per.it-codigo   = tt-item.it-codigo    AND
        sl-it-per.periodo    >= c-periodo-val        USE-INDEX ITEM NO-LOCK NO-ERROR.
    IF  AVAIL sl-it-per AND sl-it-per.val-unit-mat-m[1] > 0  THEN DO:
        ASSIGN tt-item.preco-unit      = sl-it-per.val-unit-mat-m[1]
               tt-item.preco-total     = sl-it-per.val-unit-mat-m[1] * tt-item.quantidade
               tt-item.preco-total-ggf = sl-it-per.val-unit-ggf-m[1] * tt-item.quantidade
               tt-item.preco-total-mob = sl-it-per.val-unit-mob-m[1] * tt-item.quantidade.
               .
    END. /* Existe sl-it-per */
    ELSE DO:
        FIND FIRST item-estab WHERE
            item-estab.it-codigo   = tt-item.it-codigo      AND
            item-estab.cod-estabel = tt-item.cod-estabel    NO-LOCK NO-ERROR.
        IF  AVAIL item-estab THEN DO:
            ASSIGN tt-item.preco-unit      = item-estab.val-unit-mat-m[1]
                   tt-item.preco-total     = item-estab.val-unit-mat-m[1] * tt-item.quantidade
                   tt-item.preco-total-ggf = item-estab.val-unit-ggf-m[1] * tt-item.quantidade
                   tt-item.preco-total-mob = item-estab.val-unit-mob-m[1] * tt-item.quantidade.
                   .
        END. /* Existe Item-estab */
        ELSE DO:
            FIND FIRST item-estab WHERE
                item-estab.it-codigo   = tt-item.it-codigo NO-LOCK NO-ERROR.
            IF  AVAIL item-estab THEN DO:
                ASSIGN tt-item.preco-unit      = item-estab.val-unit-mat-m[1]
                       tt-item.preco-total     = item-estab.val-unit-mat-m[1] * tt-item.quantidade
                       tt-item.preco-total-ggf = item-estab.val-unit-ggf-m[1] * tt-item.quantidade
                       tt-item.preco-total-mob = item-estab.val-unit-mob-m[1] * tt-item.quantidade.
                       .
            END. /* Fim else */

        END. /* NÆo Existe sl-it-per */
    END.

END. /* Fim for each tt-item */

/* FOR EACH tt-item NO-LOCK */
/*     WHERE tt-item.it-codigo = 'BR-101903': */
/*    */
/*     MESSAGE 'tt-item.it-codigo ' tt-item.it-codigo SKIP */
/*             'tt-item.quantidade' tt-item.quantidade */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/* END. */


CREATE "Excel.Application" c-excel.
ASSIGN c-excel:DisplayAlerts = FALSE.

ASSIGN c-modelo-planilha = search("modelos/mod-esft0039.xls")  
       c-arq             = SESSION:TEMP-DIRECTORY.

RUN pi-cria-planilha.


/* OUTPUT TO value(trim(c-arquivo-rel)). */

FOR EACH tt-item WHERE tt-item.quantidade > 0  NO-LOCK:

    FIND FIRST item WHERE 
        item.it-codigo = tt-item.it-codigo NO-LOCK NO-ERROR.
    IF  NOT AVAIL item THEN 
        NEXT. 

    FIND FIRST familia WHERE
        familia.fm-codigo = ITEM.fm-codigo NO-LOCK NO-ERROR.

    ASSIGN C-NR-NOTA-FIS  = ''
           d-dt-emis-nota = ?
           C-NR-NOTA-FIS-ex  = ''
           d-dt-emis-nota-ex = ?
           c-emitente-in = ""
           c-emitente-ex = ""
           i-cod-emitente-in = 0
           i-cod-emitente-ex = 0.

    ASSIGN aux-qt-faturada   = 0
           aux-vl-tot-item   = 0
           aux-vl-ipi-it     = 0
           aux-vl-icms-it    = 0
           aux-vl-despes-it  = 0
           aux-vl-merc-liq   = 0.

    ASSIGN v-aliq-pis = 0
           v-aliq-cof = 0.

    ASSIGN v-pis    = 0
           v-cofins = 0.

    ASSIGN encargos-financ  = 0
           aux-vl-despes-it = 0.

    ASSIGN valor-liquido = 0
        valor-liquido-in = 0
        valor-liquido-ex = 0
        qt-faturada-in = 0
        qt-faturada-ex = 0
        kg-faturada-in = 0
        kg-faturada-ex = 0.

    /* ***** Buscando ultima Venda do item na base atual **** */

    FOR EACH it-nota-fisc USE-INDEX CH-ITEM-NOTA NO-LOCK
        WHERE it-nota-fisc.cod-estabel   = tt-item.cod-estabel
        AND   it-nota-fisc.it-codigo     = tt-item.it-codigo
        AND   it-nota-fisc.dt-emis-nota <= c-periodo-ini ,
        FIRST nota-fiscal OF it-nota-fisc WHERE nota-fiscal.dt-cancela = ? NO-LOCK,
        FIRST tab-finan WHERE tab-finan.nr-tab-finan = nota-fiscal.nr-tab-finan NO-LOCK,
        FIRST natur-oper WHERE  
            natur-oper.nat-operacao = it-nota-fisc.nat-operacao AND
            natur-oper.emite-duplic = YES  NO-LOCK.

        ASSIGN aux-qt-faturada   = 0
               aux-vl-tot-item   = 0
               aux-vl-ipi-it     = 0
               aux-vl-icms-it    = 0
               aux-vl-despes-it  = 0
               aux-vl-merc-liq   = 0.

        ASSIGN v-aliq-pis = 0
               v-aliq-cof = 0.

        ASSIGN v-pis    = 0
               v-cofins = 0.

        ASSIGN encargos-financ  = 0
               aux-vl-despes-it = 0.

        ASSIGN valor-liquido = 0.
        IF  SUBSTRING(natur-oper.nat-operacao,1,1) <> '7' THEN
            ASSIGN C-NR-NOTA-FIS    = it-nota-fisc.nr-nota-fis
                   d-dt-emis-nota   = it-nota-fisc.dt-emis-nota
                   c-emitente-in    = nota-fiscal.nome-ab-cli
                   i-cod-emitente-in = nota-fiscal.cod-emitente.
        ELSE
            ASSIGN C-NR-NOTA-FIS-ex    = it-nota-fisc.nr-nota-fis
                   d-dt-emis-nota-ex   = it-nota-fisc.dt-emis-nota
                   c-emitente-ex       = nota-fiscal.nome-ab-cli
                   i-cod-emitente-ex = nota-fiscal.cod-emitente.


        ASSIGN aux-qt-faturada   = it-nota-fisc.qt-faturada[1]
               aux-vl-tot-item   = it-nota-fisc.vl-tot-item
               aux-vl-ipi-it     = (IF it-nota-fisc.cd-trib-ipi <> 2 AND 
                                       it-nota-fisc.cd-trib-ipi <> 3 THEN it-nota-fisc.vl-ipi-it  ELSE 0)

               aux-vl-icms-it    = (IF it-nota-fisc.cd-trib-icm <> 2 AND 
                                       it-nota-fisc.cd-trib-icm <> 3 THEN it-nota-fisc.vl-icms-it ELSE 0)

               aux-vl-despes-it  = it-nota-fisc.vl-despes-it
               aux-vl-merc-liq   = it-nota-fisc.vl-merc-liq.

                                                                                                       
        IF  it-nota-fisc.dt-emis-nota < 11/01/02 THEN
            ASSIGN v-aliq-pis = dec(SUBSTRING(natur-oper.char-1, 77, 4))
                   v-aliq-cof = dec(SUBSTRING(natur-oper.char-1, 82, 4)).
        ELSE
            ASSIGN v-aliq-pis = DEC(SUBSTRING(it-nota-fisc.char-2, 76, 5))
                   v-aliq-cof = DEC(SUBSTRING(it-nota-fisc.char-2, 81, 5)).

        IF  v-aliq-pis > 0 AND v-aliq-cof > 0 THEN
            ASSIGN v-pis    = ((aux-vl-tot-item - (aux-vl-ipi-it)) * v-aliq-pis) / 100
                   v-cofins = ((aux-vl-tot-item - (aux-vl-ipi-it)) * v-aliq-cof) / 100.
        ELSE
            ASSIGN v-pis    = 0
                   v-cofins = 0.

        IF  nota-fiscal.nr-ind-finan > 0 THEN                                                    
            ASSIGN encargos-financ  = aux-vl-merc-liq - (aux-vl-merc-liq / nota-fiscal.tab-ind-fin)
                   aux-vl-despes-it = it-nota-fisc.vl-despes-it.
        ELSE
            ASSIGN encargos-financ  = 0
                   aux-vl-despes-it = it-nota-fisc.vl-despes-it.

        ASSIGN valor-liquido = aux-vl-tot-item - ( aux-vl-icms-it + 
                                              aux-vl-ipi-it + v-pis + v-cofins + aux-vl-despes-it + encargos-financ).

         ASSIGN dt-refe = date(month(nota-fiscal.dt-emis-nota),01,YEAR(nota-fiscal.dt-emis-nota)) +  32
                dt-refe = DATE (MONTH(dt-refe),01,YEAR(dt-refe)) - 1.

            FIND FIRST sl-it-per WHERE
                 sl-it-per.cod-estabel = nota-fiscal.cod-estabel  AND 
                 sl-it-per.it-codigo   = it-nota-fisc.it-codigo    AND
                 sl-it-per.periodo     = dt-refe        USE-index ITEM NO-LOCK NO-ERROR.
             IF  AVAIL sl-it-per AND sl-it-per.val-unit-mat-m[1] > 0  THEN DO:
                 ASSIGN vl-custo-in      = sl-it-per.val-unit-mat-m[1].
             END.

        ASSIGN valor-liquido = valor-liquido / (it-nota-fisc.qt-faturada[1] * ITEM.peso-liquido).
        IF  SUBSTRING(natur-oper.nat-operacao,1,1) <> '7' THEN
            ASSIGN valor-liquido-in = valor-liquido
                   qt-faturada-in = it-nota-fisc.qt-faturada[1]  
                   kg-faturada-in = it-nota-fisc.qt-faturada[1] * ITEM.peso-liquido
                   vl-custo-in    = IF  AVAIL sl-it-per AND sl-it-per.val-unit-mat-m[1] > 0  THEN  sl-it-per.val-unit-mat-m[1] + sl-it-per.val-unit-ggf-m[1] + sl-it-per.val-unit-mob-m[1] ELSE 0.
        ELSE
            ASSIGN valor-liquido-ex = valor-liquido
                   qt-faturada-ex = it-nota-fisc.qt-faturada[1]  
                   kg-faturada-ex = it-nota-fisc.qt-faturada[1] * ITEM.peso-liquido
                   vl-custo-ex    = IF  AVAIL sl-it-per AND sl-it-per.val-unit-mat-m[1] > 0  THEN  sl-it-per.val-unit-mat-m[1] + sl-it-per.val-unit-ggf-m[1] + sl-it-per.val-unit-mob-m[1] ELSE 0.

    END.

    ASSIGN de-qtd-peso = (tt-item.quantidade * ITEM.peso-liquido)
           de-val-peso = (tt-item.preco-total + tt-item.preco-total-ggf + tt-item.preco-total-mob) / de-qtd-peso.

    /* DISP tt-item.cod-estabel                            COLUMN-LABEL "Est"
         tt-item.it-codigo                              COLUMN-LABEL "Item"
         tt-item.un                                     COLUMN-LABEL "Unid"
         tt-item.quantidade                             COLUMN-LABEL "Qtdade Saldo"
         tt-item.preco-unit                             COLUMN-LABEL "Vl Unit Medio"
         tt-item.preco-total                            COLUMN-LABEL "Valor Total MAT"  FORMAT "->>>,>>>,>>9.99"
         tt-item.preco-total-ggf                        COLUMN-LABEL "Valor Total GGF"  FORMAT "->>>,>>>,>>9.99"
         de-qtd-peso                                    COLUMN-LABEL "Qtdade Saldo/KG"  FORMAT "->,>>>,>>9.99"
         de-val-peso                                    COLUMN-LABEL "Vl Unit Peso/KG"  FORMAT "->,>>>,>>9.99"
          ITEM.ge-codigo                                COLUMN-LABEL "Gr.Estoque"
         familia.fm-codigo + " - " + familia.descricao  COLUMN-LABEL "Familia"          FORMAT "x(40)"
         C-NR-NOTA-FIS                                  COLUMN-LABEL "Ult Nota MI"  
         d-dt-emis-nota                                 COLUMN-LABEL "Data Ult Nota MI"    FORMAT "99/99/9999" 
         valor-liquido-in                                  COLUMN-LABEL "Vl Unit MI"          FORMAT "->,>>>,>>9.99"
         c-emitente-in                                  COLUMN-LABEL "Emitente MI"    FORMAT "x(12)"
         C-NR-NOTA-FIS-ex                                  COLUMN-LABEL "Ult Nota ME"  
         d-dt-emis-nota-ex                                 COLUMN-LABEL "Data Ult Nota ME"    FORMAT "99/99/9999" 
         valor-liquido-ex                                  COLUMN-LABEL "Vl Unit ME"          FORMAT "->,>>>,>>9.99"
         c-emitente-ex                                  COLUMN-LABEL "Emitente ME"    FORMAT "x(12)"
        WITH DOWN WIDTH 300 STREAM-IO FRAME f-1.
    DOWN WITH FRAME f-1. */

    RUN pi-acompanhar IN h-acomp (INPUT "Listando Item:" +  tt-item.it-codigo + "Linha:" + STRING(i-linha) ).

    ASSIGN  i-linha = i-linha + 1     
            c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-item.cod-estabel                                                                     
            c-relatorio:range("B" + STRING(i-linha)):VALUE = tt-item.it-codigo                                                                      
            c-relatorio:range("C" + STRING(i-linha)):VALUE = ITEM.desc-item                                                                         
            c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-item.un                                                                             
            c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-item.quantidade                                                                     
            c-relatorio:range("F" + STRING(i-linha)):VALUE = tt-item.preco-unit                                                                     
            c-relatorio:range("G" + STRING(i-linha)):VALUE = tt-item.preco-total                                                                    
            c-relatorio:range("H" + STRING(i-linha)):VALUE = tt-item.preco-total-ggf                                                                             
            c-relatorio:range("I" + STRING(i-linha)):VALUE = tt-item.preco-total-mob                                                                             
            
            c-relatorio:range("J" + STRING(i-linha)):VALUE = de-qtd-peso           
            c-relatorio:range("K" + STRING(i-linha)):VALUE = de-val-peso                                            
            c-relatorio:range("L" + STRING(i-linha)):VALUE = ITEM.ge-codigo                                             
            c-relatorio:range("M" + STRING(i-linha)):VALUE = familia.fm-codigo + " - " + familia.descricao           
            c-relatorio:range("N" + STRING(i-linha)):VALUE = i-cod-emitente-in                                     
            c-relatorio:range("O" + STRING(i-linha)):VALUE = c-emitente-in                                   
            c-relatorio:range("P" + STRING(i-linha)):VALUE = C-NR-NOTA-FIS                                       
            c-relatorio:range("Q" + STRING(i-linha)):VALUE = d-dt-emis-nota                                      
            c-relatorio:range("R" + STRING(i-linha)):VALUE = kg-faturada-in                                
            c-relatorio:range("S" + STRING(i-linha)):VALUE = qt-faturada-in                                        
            c-relatorio:range("T" + STRING(i-linha)):VALUE = valor-liquido-in                                   
            c-relatorio:range("U" + STRING(i-linha)):VALUE = vl-custo-in                                           
            c-relatorio:range("V" + STRING(i-linha)):VALUE = i-cod-emitente-ex                                                       
            c-relatorio:range("W" + STRING(i-linha)):VALUE = c-emitente-ex           
            c-relatorio:range("X" + STRING(i-linha)):VALUE = C-NR-NOTA-FIS-ex          
            c-relatorio:range("Y" + STRING(i-linha)):VALUE = d-dt-emis-nota-ex                
            c-relatorio:range("Z" + STRING(i-linha)):VALUE =  kg-faturada-ex 
            c-relatorio:range("AA" + STRING(i-linha)):VALUE  = qt-faturada-ex              
            c-relatorio:range("AB" + STRING(i-linha)):VALUE = valor-liquido-ex               
            c-relatorio:range("AC" + STRING(i-linha)):VALUE = vl-custo-ex.                                           

END.

RUN pi-finalizar IN h-acomp.

/* OUTPUT CLOSE. */

RUN pi-finaliza-impressao.

RETURN "OK:U".

/***************************/
PROCEDURE pi-cria-planilha:
/**************************/
DEF VAR c-arquivo  AS CHAR NO-UNDO.
DEF VAR cdir       AS CHAR NO-UNDO.
DEF VAR cdiretorio AS CHAR NO-UNDO.
DEFINE VARIABLE icont AS INTEGER     NO-UNDO.
DEFINE VARIABLE errStatus AS INTEGER     NO-UNDO.

/*ASSIGN c-arquivo = c-arq + 'saldo_venda_' + STRING(i-ep-codigo-usuario) + "_" + STRING(c-periodo-ini,"999999")  + '.xls'.

OS-COPY VALUE(c-modelo-planilha) VALUE(c-arquivo).

ASSIGN c-planilha  = c-excel:Workbooks:OPEN(c-arquivo)
       c-relatorio = c-planilha:Sheets:item(1)
       c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo.

       c-relatorio:activate().*/

ASSIGN c-arquivo = c-arq + 'saldo_venda_' + STRING(i-ep-codigo-usuario) + "_" + STRING(c-periodo-ini,"999999") + "_" + STRING(TIME)  + '.xls'.

  session:set-wait-state ("general").
  /*cria a aplica¯Êo do excel*/
  create "Excel.Application" c-excel CONNECT no-error.
  if ERROR-STATUS:error then do:
    create "Excel.application" c-excel.
  end.
  
  assign cdir = entry(1,c-arquivo, '~\') 
         cdiretorio = c-arquivo.
        /*Troca caracteres inv˜lidos para evitar erro na cria¯Êo da planilha*/
  do icont = 2 to num-entries(cdiretorio, '~\') - 1:
    assign cdiretorio = replace(cdiretorio, '"', '_').
    os-create-dir value(cdir + "~\" + entry(icont, cdiretorio, "~\")).
    assign cdir = cdir + "~\" + entry(icont, cdiretorio, "~\").
  end.
  
  /*Verifica erros do sistema operacional*/
     assign errStatus = os-error.
     if errStatus <> 0 AND
        errStatus <> 2 then do:
        case errStatus:
        when 1 then
            run utp/ut-msgs.p ('show', 26692, '').
        when 8 then 
            run utp/ut-msgs.p ('show', 26694, ''). 
        when 3    then 
            run utp/ut-msgs.p ('show', 26695, '').
        WHEN 7     then  
            run utp/ut-msgs.p ('show', 26696, '').       
        when 17 then
            run utp/ut-msgs.p ('show', 26697, '').
       end case.
       return no-apply.
     end. 
  
/*Busca a planilha modelo*/
  assign c-modelo-planilha = search("modelos/mod-esft0039.xls").
     if c-modelo-planilha <> ? then do:
          os-copy value(c-modelo-planilha) value(cDiretorio).
     end.
     else do:
          run utp/ut-msgs.p (input "show",
                             input 1332  ,
                             input "modelos/mod-esft0039.xls":U).
           return "NOK":U.
     end.

 /*Abre o arquivo do excel*/
  assign c-planilha  = c-excel:workbooks:open(cdiretorio).
  assign c-relatorio = c-planilha:sheets:ITEM(1).
  ASSIGN c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo.

  c-relatorio:activate().

END PROCEDURE.

/********************************/
PROCEDURE pi-finaliza-impressao:
/********************************/
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



