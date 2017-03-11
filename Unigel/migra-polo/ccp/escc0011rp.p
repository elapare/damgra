/*****************************************************************************
**
**       Programa: escc0011rp.p
**
**       Data....: 11/02/2008
**
**       Author..: Amgra / JosÇ Roberto
**
**       Objetivo: Importa Planilha da Tabela de Preáos de Fornecedores
**
**       Vers∆o..: 1.00.000 
**
**       OBS.....: 
**
*******************************************************************************/


{include/i-prgvrs.i escc0011rp 1.00.00.000}

/* definiá∆o das temp-tables para recebimento de parÉmetros */


define temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    field v_num_tip_aces_usuar as integer
    field ep-codigo            as character
    field c-arquivo-jr         AS CHAR
    field cod-estabel          like am-cc-tab-preco.cod-estabel 
    field cod-emitente         like am-cc-tab-preco.cod-emitente
    field nr-tab-preco         like am-cc-tab-preco.nr-tab-preco.


def temp-table tt-raw-digita
    	field raw-digita	as raw.

/*
DEFINE TEMP-TABLE tt-custo
    
    FIELD cod-estabel          AS CHAR 
    FIELD nr-ord-produ         AS INTEGER
    FIELD cod-roteiro          AS CHAR
    FIELD it-codigo            AS CHAR 
    FIELD qtde-original        AS DECIMAL
    FIELD qtde-reportada       AS DECIMAL
    FIELD novo-roteiro         AS CHAR
    FIELD hr-calculada         AS DEC
    FIELD hr-reportada         AS DEC
    FIELD cc-codigo            AS CHAR 
    INDEX ch-tt-roteiro IS PRIMARY UNIQUE  cod-estabel
                                           nr-ord-produ
                                           novo-roteiro 
                                           cc-codigo. 

*/


def new shared var c-cod-estabel   like am-cc-tab-preco.cod-estabel   format "x(03)"    initial "422"   no-undo.
def new shared var c-cod-emitente  like am-cc-tab-preco.cod-emitente  format ">>>>>>>9" initial 0       no-undo.
def new shared var c-nr-tab-preco  like am-cc-tab-preco.nr-tab-preco  format ">>>>>>>9" initial 0       no-undo.





/* recebimento de parÉmetros */
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.
create tt-param.
raw-transfer raw-param to tt-param.
/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}
/* definiá∆o de vari†veis  */


    def var rw-log-exec                            as rowid no-undo.
    def var c-erro-rpc as character format "x(60)" initial " " no-undo.
    def var c-erro-aux as character format "x(60)" initial " " no-undo.
    def var c-ret-temp as char no-undo.
    def var h-servid-rpc as handle no-undo.   
    define new shared stream str-rp.

def var h-acomp as handle no-undo.
def new global shared var i-ep-codigo-usuario  like mgmulti.empresa.ep-codigo no-undo.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def var v-num-reg-lidos      as int    no-undo.


DEFINE VARIABLE data-ini-jr     AS DATE       NO-UNDO.
DEFINE VARIABLE data-fim-jr     AS DATE       NO-UNDO.
DEFINE VARIABLE estab-jr        AS CHAR       NO-UNDO.
DEFINE VARIABLE notas-jr        AS CHAR       NO-UNDO.
DEFINE VARIABLE cod-estabel-jr  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE descricao-jr1   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE descricao-jr2   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE motivo-jr       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE flag-erro       AS INTEGER    NO-UNDO.
DEFINE VARIABLE idx-jr          AS INTEGER    NO-UNDO.
DEFINE VARIABLE valor-dec       AS DECIMAL    NO-UNDO.

DEFINE VARIABLE cod-cond-pag-jr    AS INTEGER    NO-UNDO.
DEFINE VARIABLE cod-transp-jr      AS INTEGER    NO-UNDO.
DEFINE VARIABLE cod-cond-pag-jr2   AS INTEGER    NO-UNDO.
DEFINE VARIABLE it-codigo-jr       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cod-emitente-jr    AS INTEGER    NO-UNDO.
DEFINE VARIABLE usuar-comprador-jr AS CHARACTER  NO-UNDO.

def new global shared var v_cdn_empres_usuar   like mgmulti.empresa.ep-codigo        no-undo.


/* para executar o Excel */

DEF VAR c-arq             AS CHAR                NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR                NO-UNDO.   
DEF VAR l-cria            AS LOG                 NO-UNDO.
DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
DEF VAR i-linha           AS INT                 NO-UNDO.
DEF VAR c-coluna          AS char                NO-UNDO.

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.
def var c-relatorio1       as com-handle.
/* definiá∆o de frames do relat¢rio */


form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    fill("-", 112) format "x(110)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabec.

     
form descricao-jr1             COLUMN-LABEL "Descriá∆o 1"       format "x(20)"             AT 001
     descricao-jr2             COLUMN-LABEL "Descriá∆o 1"       format "x(20)"             AT 022
     Motivo-jr                 COLUMN-LABEL "Motivo do Erro"    format "x(75)"             AT 043
     with down width 132 no-box stream-io frame f-relat-09-132.

form header
    c-rodape format "x(132)"
    with stream-io width 132 no-labels no-box page-bottom frame f-rodape.

/*assign i-ep-codigo-usuario = tt-param.ep-codigo
           v_cdn_empres_usuar  = i-ep-codigo-usuario.
  */       
find first mgmulti.empresa no-lock
    where empresa.ep-codigo = tt-param.ep-codigo no-error.
if  avail empresa
then
    assign c-empresa = empresa.razao-social.
else
    assign c-empresa = "".


/* include padr∆o para output de relat¢rios */

{include/i-rpout.i &STREAM="stream str-rp"}

/* bloco principal do programa */

assign c-programa 	= "escc0011RP"
	c-versao	= "1.00"
	c-revisao	= ".00.000"
	c-empresa 	= " "
	c-sistema	= ""
	c-titulo-relat = "Importaá∆o da Tab.Preáos do Fornecedor".
    
    run utp/ut-acomp.p persistent set h-acomp.
    {utp/ut-liter.i Imprimindo *}
    run pi-inicializar in h-acomp (input RETURN-VALUE).

assign v-num-reg-lidos = 0.

    
/* Cria Aplicaá∆o do Excel */

CREATE "Excel.Application" c-excel.
ASSIGN c-excel:DisplayAlerts = FALSE.

ASSIGN c-modelo-planilha = search("modelos\mod-escc0011.xls") 
       c-arq             = SESSION:TEMP-DIRECTORY.


DEF VAR c-arquivo AS CHAR NO-UNDO.

    ASSIGN c-arquivo = tt-param.c-arquivo-jr.
    
    assign c-planilha   = c-excel:Workbooks:OPEN(c-arquivo)
           c-relatorio  = c-excel:Sheets:item(1)
           c-arq-anexo  = (IF c-arq-anexo <> "" THEN "," ELSE "") + c-arquivo.



/*
FOR EACH tt-roteiro :
    DELETE tt-roteiro.
END.                 
*/

ASSIGN flag-erro = 0.


FIND FIRST am-cc-tab-preco WHERE
    am-cc-tab-preco.cod-estabel   = tt-param.cod-estabel  AND
    am-cc-tab-preco.cod-emitente  = tt-param.cod-emitente AND
    am-cc-tab-preco.nr-tab-preco  = tt-param.nr-tab-preco
    NO-LOCK NO-ERROR.

IF AVAIL am-cc-tab-preco THEN DO:

    ASSIGN flag-erro = 9
           descricao-jr1  = "Tabela de Preáos:"
           descricao-jr2  = STRING (tt-param.nr-tab-preco)
           motivo-jr      = "J† Existe Esta Tabela de Preáo".
         
             view stream str-rp frame f-cabec.
             view stream str-rp frame f-rodape.
             /*assign l-imprime = yes.*/
             display stream str-rp
                 descricao-jr1
                 descricao-jr2
                 motivo-jr
                 with stream-io frame f-relat-09-132.
                 down stream str-rp with frame f-relat-09-132. 

END.

ASSIGN data-ini-jr = DATE (c-relatorio:range("E" + STRING(2)):VALUE)
       data-fim-jr = DATE (c-relatorio:range("E" + STRING(3)):VALUE).

IF data-fim-jr < data-ini-jr THEN DO:

    ASSIGN flag-erro = 9
           descricao-jr1  = "Datas de Validade"
           descricao-jr2  = " "
           motivo-jr      = "Data Final Menor que Data Inicial".
         
             view stream str-rp frame f-cabec.
             view stream str-rp frame f-rodape.
             /*assign l-imprime = yes.*/
             display stream str-rp
                 descricao-jr1
                 descricao-jr2
                 motivo-jr
                 with stream-io frame f-relat-09-132.
                 down stream str-rp with frame f-relat-09-132. 
   
END.


IF data-ini-jr = ? OR  data-fim-jr = ? THEN DO:

    ASSIGN flag-erro = 9
           descricao-jr1  = "Datas de Validade"
           descricao-jr2  = " "
           motivo-jr      = "Falta Data de Validade da Tabela".
         
             view stream str-rp frame f-cabec.
             view stream str-rp frame f-rodape.
             /*assign l-imprime = yes.*/
             display stream str-rp
                 descricao-jr1
                 descricao-jr2
                 motivo-jr
                 with stream-io frame f-relat-09-132.
                 down stream str-rp with frame f-relat-09-132. 
   
END.



FIND FIRST am-cc-tab-preco WHERE
    am-cc-tab-preco.cod-estabel   = tt-param.cod-estabel  AND
    am-cc-tab-preco.cod-emitente  = tt-param.cod-emitente AND
    data-ini-jr >= am-cc-tab-preco.dt-valid-ini AND
    data-fim-jr <= am-cc-tab-preco.dt-valid-fim
    USE-INDEX validade
    NO-LOCK NO-ERROR.

IF AVAIL am-cc-tab-preco THEN DO:

    ASSIGN flag-erro = 9
           descricao-jr1  = "Datas de Validade"
           descricao-jr2  = STRING ("Tabela: " + STRING (am-cc-tab-preco.nr-tab-preco))
           motivo-jr      = "J† Existe Tabela deste Fornecedor nesta Validade".
         
             view stream str-rp frame f-cabec.
             view stream str-rp frame f-rodape.
             /*assign l-imprime = yes.*/
             display stream str-rp
                 descricao-jr1
                 descricao-jr2
                 motivo-jr
                 with stream-io frame f-relat-09-132.
                 down stream str-rp with frame f-relat-09-132. 

END.

FIND FIRST estabelec WHERE
    estabelec.cod-estabel = tt-param.cod-estabel
    NO-LOCK NO-ERROR.

IF NOT AVAIL estabelec THEN DO:
    ASSIGN flag-erro = 9
           descricao-jr1  = "Estab.:"
           descricao-jr2  = tt-param.cod-estabel
           motivo-jr      = "Estabelecimento Desconhecido".
         
             view stream str-rp frame f-cabec.
             view stream str-rp frame f-rodape.
             /*assign l-imprime = yes.*/
             display stream str-rp
                 descricao-jr1
                 descricao-jr2
                 motivo-jr
                 with stream-io frame f-relat-09-132.
                 down stream str-rp with frame f-relat-09-132. 

END.
   
ASSIGN cod-cond-pag-jr = INT (c-relatorio:range("G" + STRING(2)):VALUE).

FIND FIRST cond-pagto WHERE
    cond-pagto.cod-cond-pag = cod-cond-pag-jr 
    NO-LOCK NO-ERROR.

IF NOT AVAIL cond-pagto THEN DO:

    ASSIGN flag-erro = 9
           descricao-jr1  = "Cond.Pagamento"
           descricao-jr2  = STRING (cod-cond-pag-jr)
           motivo-jr      = "Condiá∆o de Pagamento N∆o Existe".
         
             view stream str-rp frame f-cabec.
             view stream str-rp frame f-rodape.
             /*assign l-imprime = yes.*/
             display stream str-rp
                 descricao-jr1
                 descricao-jr2
                 motivo-jr
                 with stream-io frame f-relat-09-132.
                 down stream str-rp with frame f-relat-09-132. 
   
END.

   
ASSIGN cod-emitente-jr = INT (c-relatorio:range("B" + STRING(2)):VALUE).

IF cod-emitente-jr <> tt-param.cod-emitente THEN DO:

    ASSIGN flag-erro = 9
           descricao-jr1  = "Fornecedor"
           descricao-jr2  = STRING (cod-emitente-jr)
           motivo-jr      = "Fornecedor da Planilha <> Fornec.da Seleá∆o".
         
             view stream str-rp frame f-cabec.
             view stream str-rp frame f-rodape.
             /*assign l-imprime = yes.*/
             display stream str-rp
                 descricao-jr1
                 descricao-jr2
                 motivo-jr
                 with stream-io frame f-relat-09-132.
                 down stream str-rp with frame f-relat-09-132. 
   
END.

ASSIGN cod-transp-jr = INT (c-relatorio:range("G" + STRING(3)):VALUE).

FIND FIRST transporte WHERE
    transporte.cod-transp = cod-transp-jr 
    NO-LOCK NO-ERROR.

IF NOT AVAIL transporte AND cod-transp-jr <> 0 THEN DO:

    ASSIGN flag-erro = 9
           descricao-jr1  = "Cod.Transportadora"
           descricao-jr2  = STRING (cod-transp-jr)
           motivo-jr      = "Transportadora N∆o Existe".
         
             view stream str-rp frame f-cabec.
             view stream str-rp frame f-rodape.
             /*assign l-imprime = yes.*/
             display stream str-rp
                 descricao-jr1
                 descricao-jr2
                 motivo-jr
                 with stream-io frame f-relat-09-132.
                 down stream str-rp with frame f-relat-09-132. 
   
END.


ASSIGN usuar-comprador-jr = STRING (c-relatorio:range("G" + STRING(4)):VALUE).

FIND FIRST usuar-mater WHERE
    usuar-mater.cod-usuario = usuar-comprador-jr 
    NO-LOCK NO-ERROR.

IF NOT AVAIL usuar-mater OR usuar-mater.usuar-comprador = NO THEN DO:

    ASSIGN flag-erro = 9
           descricao-jr1  = "Comprador"
           descricao-jr2  = STRING (usuar-comprador-jr)
           motivo-jr      = "Comprador n∆o Existe".
         
             view stream str-rp frame f-cabec.
             view stream str-rp frame f-rodape.
             /*assign l-imprime = yes.*/
             display stream str-rp
                 descricao-jr1
                 descricao-jr2
                 motivo-jr
                 with stream-io frame f-relat-09-132.
                 down stream str-rp with frame f-relat-09-132. 
   
END.

ASSIGN i-linha = 5. 

DO WHILE i-linha <> 0 :
       
   ASSIGN i-linha  = i-linha + 1.

   ASSIGN it-codigo-jr = string (c-relatorio:range("A" + STRING(i-linha)):VALUE).

   IF it-codigo-jr = "" OR it-codigo-jr = ? THEN LEAVE.

   run pi-acompanhar in h-acomp (input ( /*"Verificando Erros na Planilha da Tab.Preáos - " +*/  it-codigo-jr)).
    
   
   FIND FIRST item WHERE
       item.it-codigo = trim(it-codigo-jr)
       NO-LOCK NO-ERROR.

    
   IF NOT AVAIL item THEN DO:

       
       ASSIGN flag-erro = 9
              descricao-jr1  = "Item: "
              descricao-jr2  = STRING (it-codigo-jr)
              motivo-jr      = "Item N∆o Existe".
            
                view stream str-rp frame f-cabec.
                view stream str-rp frame f-rodape.
                /*assign l-imprime = yes.*/
                display stream str-rp
                    descricao-jr1
                    descricao-jr2
                    motivo-jr
                    with stream-io frame f-relat-09-132.
                    down stream str-rp with frame f-relat-09-132. 
   
   END. 

   IF  AVAIL ITEM AND
       string (c-relatorio:range("C" + STRING(i-linha)):VALUE) <> ITEM.un THEN DO:

       ASSIGN flag-erro = 9
              descricao-jr1  = "Item: "
              descricao-jr2  = STRING (it-codigo-jr)
              motivo-jr      = "Unidade de Compra Diferente da Unidade do Item".
            
                view stream str-rp frame f-cabec.
                view stream str-rp frame f-rodape.
                /*assign l-imprime = yes.*/
                display stream str-rp
                    descricao-jr1
                    descricao-jr2
                    motivo-jr
                    with stream-io frame f-relat-09-132.
                    down stream str-rp with frame f-relat-09-132. 
   
   END. 


   IF DEC (c-relatorio:range("D" + STRING(i-linha)):VALUE) <= 0 THEN DO:

       ASSIGN flag-erro = 9
              descricao-jr1  = "Item: "
              descricao-jr2  = STRING (it-codigo-jr)
              motivo-jr      = "Preáo do Item Errado".
            
                view stream str-rp frame f-cabec.
                view stream str-rp frame f-rodape.
                /*assign l-imprime = yes.*/
                display stream str-rp
                    descricao-jr1
                    descricao-jr2
                    motivo-jr
                    with stream-io frame f-relat-09-132.
                    down stream str-rp with frame f-relat-09-132. 
   
   END.

   
   ASSIGN cod-cond-pag-jr2 = INT (c-relatorio:range("F" + STRING(i-linha)):VALUE).

   IF cod-cond-pag-jr2 = 0 THEN
       ASSIGN cod-cond-pag-jr2 = cod-cond-pag-jr.

   FIND FIRST cond-pagto WHERE
       cond-pagto.cod-cond-pag = cod-cond-pag-jr2 
       NO-LOCK NO-ERROR.

   IF NOT AVAIL cond-pagto THEN DO:

       ASSIGN flag-erro = 9
              descricao-jr1  = "Cond.Pagamento do Item"
              descricao-jr2  = STRING (cod-cond-pag-jr2)
              motivo-jr      = "Condiá∆o de Pagamento N∆o Existe".

                view stream str-rp frame f-cabec.
                view stream str-rp frame f-rodape.
                /*assign l-imprime = yes.*/
                display stream str-rp
                    descricao-jr1
                    descricao-jr2
                    motivo-jr
                    with stream-io frame f-relat-09-132.
                    down stream str-rp with frame f-relat-09-132. 

   END.

END. /* do while da linha da planilha */

IF flag-erro = 0 THEN DO: /* Planilha Sem erro */

    FOR EACH am-cc-tab-preco WHERE
        am-cc-tab-preco.cod-estabel  = tt-param.cod-estabel  AND
        am-cc-tab-preco.cod-emitente = tt-param.cod-emitente AND
        am-cc-tab-preco.nr-tab-preco = tt-param.nr-tab-preco 
        exclusive-lock.

        FOR EACH am-cc-it-tab-preco OF am-cc-tab-preco EXCLUSIVE-LOCK.

          DELETE am-cc-it-tab-preco.

        END.

        DELETE am-cc-tab-preco.

    END.  

    CREATE am-cc-tab-preco.

    ASSIGN am-cc-tab-preco.cod-estabel  = tt-param.cod-estabel
           am-cc-tab-preco.cod-emitente = tt-param.cod-emitente
           am-cc-tab-preco.nr-tab-preco = tt-param.nr-tab-preco.

    ASSIGN am-cc-tab-preco.aprovada        = NO
           am-cc-tab-preco.dt-valid-ini    = data-ini-jr
           am-cc-tab-preco.dt-valid-fim    = data-fim-jr
           am-cc-tab-preco.cod-cond-pag    = cod-cond-pag-jr
           am-cc-tab-preco.cod-transp      = cod-transp-jr
           am-cc-tab-preco.usuar-comprador = usuar-comprador-jr
           am-cc-tab-preco.dt-aprovacao   = ?
           am-cc-tab-preco.narrativa      = string (c-relatorio:range("B" + STRING(4)):VALUE).


    ASSIGN i-linha = 5. 

    DO WHILE i-linha <> 0 :

       ASSIGN i-linha  = i-linha + 1.

       ASSIGN it-codigo-jr = string (c-relatorio:range("A" + STRING(i-linha)):VALUE).


       IF it-codigo-jr = "" OR it-codigo-jr = ? THEN LEAVE. 

       run pi-acompanhar in h-acomp (input ("Gravando a Planilha da Tab.Preáos - " + it-codigo-jr)).
       
       CREATE am-cc-it-tab-preco.
    
       ASSIGN am-cc-it-tab-preco.cod-estabel  = tt-param.cod-estabel
              am-cc-it-tab-preco.cod-emitente = tt-param.cod-emitente
              am-cc-it-tab-preco.nr-tab-preco = tt-param.nr-tab-preco
              am-cc-it-tab-preco.it-codigo    = it-codigo-jr.
       
       ASSIGN cod-cond-pag-jr2 = INT (c-relatorio:range("F" + STRING(i-linha)):VALUE).

       IF cod-cond-pag-jr2 = 0 THEN
           ASSIGN cod-cond-pag-jr2 = cod-cond-pag-jr. 


       ASSIGN am-cc-it-tab-preco.unid            = string (c-relatorio:range("C" + STRING(i-linha)):VALUE)
              am-cc-it-tab-preco.cod-cond-pag    = cod-cond-pag-jr2
              am-cc-it-tab-preco.qtde-minima     = DEC (c-relatorio:range("E" + STRING(i-linha)):VALUE)
              am-cc-it-tab-preco.preco-item      = DEC (c-relatorio:range("D" + STRING(i-linha)):VALUE)
              am-cc-it-tab-preco.perc-icms       = DEC (c-relatorio:range("G" + STRING(i-linha)):VALUE)
              am-cc-it-tab-preco.perc-ipi        = DEC (c-relatorio:range("H" + STRING(i-linha)):VALUE).


       IF DEC (c-relatorio:range("I" + STRING(i-linha)):VALUE) > 0 THEN
          ASSIGN am-cc-it-tab-preco.perc-rat-compra = DEC (c-relatorio:range("I" + STRING(i-linha)):VALUE).
       ELSE
          ASSIGN am-cc-it-tab-preco.perc-rat-compra = 100.


    END. /* do while da linha da planilha */

    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.

    PUT STREAM str-rp
        " " AT 01
        "IMPORTAÄ«O DA TABELA REALIZADA SEM ERROS !!!"  AT 01
        "------------------------------------------- "  AT 01
        " "                                   AT 01.


END. /* planilha sem erro. */

run pi-finalizar in h-acomp.

c-planilha:CLOSE().

/* fechamento do output do relat¢rio  */

{include/i-rpclo.i &STREAM="stream str-rp"}

return "OK":U.


