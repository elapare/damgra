/*****************************************************************************
**
**       Programa: escc0026rp.p
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


{include/i-prgvrs.i escc0026rp 1.00.00.000}

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
    field cod-estabel          like contrato-for.cod-estabel 
    field cod-emitente         like contrato-for.cod-emitente
    field nr-contrato         like contrato-for.nr-contrato.


def temp-table tt-raw-digita
    	field raw-digita	as raw.

define temp-table tt-item-contrato no-undo
  field im-preco-unit           like item-contrat.preco-fornec
  field im-qtd-minima           like item-contrat.qtd-minima
  field im-it-codigo            like item-contrat.it-codigo   
  field im-cod-refer            like item-contrat.cod-refer     
  field im-codigo-ipi           like item-contrat.codigo-ipi  
  field im-codigo-icm           like item-contrat.codigo-icm
  field im-un                   like item-contrat.un      
  field im-num-seq-item         like item-contrat.num-seq-item
  field im-frequencia           like item-contrat.frequencia    
  field im-ind-sit-item         like item-contrat.ind-sit-item    
  field im-ind-un-contrato      like item-contrat.ind-un-contrato
  field im-ind-tipo-control-val like item-contrat.ind-tipo-control-val
  field im-log-control-event    like item-contrat.log-control-event
  field im-log-caract-item      like item-contrat.ind-caract-item
  field im-log-obrig-item       like item-contrat.log-obrig-item
  field im-log-ind-multa        like item-contrat.log-ind-multa
  field im-perc-multa-dia       like item-contrat.perc-multa-dia
  field im-perc-multa-limite    like item-contrat.perc-multa-limite
  field im-cod-depos            like item-contrat.cod-depos
  field im-aliquota-icm         like item-contrat.aliquota-icm
  field im-aliquota-ipi         like item-contrat.aliquota-ipi
  field im-aliquota-iss         like item-contrat.aliquota-iss
  field im-preco-fornec         like item-contrat.preco-fornec
  field im-taxa-financ          like item-contrat.taxa-financ
  field im-tp-despesa           like item-contrat.tp-despesa
  field im-val-frete            like item-contrat.val-frete      
  field im-val-taxa             like item-contrat.val-taxa        
  field im-prazo-ent            like item-contrat.prazo-ent  
  field im-dat-cotac            like item-contrat.dat-cotac     
  field im-preco-base           like item-contrat.preco-base   
  field im-perc-desconto        like item-contrat.perc-desconto
  field im-narrat-compra        like item-contrat.narrat-compra
  field im-ind-tipo-control     like item-contrat.ind-tipo-control
  field im-pre-unit-for         like item-contrat.pre-unit-for  
  field im-dat-base             like item-contrat.dat-base         
  field im-ordem-base           like item-contrat.ordem-base
  field im-cod-emitente         like contrato-for.cod-emitente
  field im-sld-qtd-receb        like item-contrat.sld-qtd-rece
  field im-nr-contrat           like contrato-for.nr-contrato
  field im-sld-val              like contrato-for.sld-val
  field im-val-fatur-minimo     like contrato-for.val-fatur-minimo
  field im-mo-codigo            like contrato-for.mo-codigo
  field im-log-libera           like contrato-for.log-libera
  field im-val-total            like contrato-for.val-total
  field im-contato              like contrato-for.contato
  field im-qtd-total            like contrato-for.qtd-total
  field im-acum-rec-val         like contrato-for.acum-rec-val
  field im-acum-rec-qtd         like contrato-for.acum-rec-qtd
  field im-sld-qtd-liber        like contrato-for.sld-qtd-liber
  field im-sld-qtd              like contrato-for.sld-qtd
  field im-sld-val-liber        like contrato-for.sld-val-liber
  field im-ind-caract-item      like item-contrat.ind-caract-item
  field im-cod-cond-pag         like contrato-for.cod-cond-pag
  field im-frete-ped            like item-contrat.frete
  field im-cod-comprado         like contrato-for.cod-comprado
  field im-sld-val-receb        like contrato-for.sld-val-receb.         


define buffer b-item-contrat for item-contrat.
define buffer b-contrato-for  for contrato-for.

def shared var c-cod-estabel   like contrato-for.cod-estabel   format "x(03)"    initial "422"   no-undo.
def shared var c-cod-emitente  like contrato-for.cod-emitente  format ">>>>>>>9" initial 0       no-undo.
def shared var i-nr-contrato   like contrato-for.nr-contrato  format ">>>>>>>9" initial 0      no-undo.
def shared var c-arquivo-import as char       no-undo.

def var c-linha      as char    no-undo.
def var de-tipo      as char    format "x(2)"   no-undo.
def var de-sequencia as integer format "99"     no-undo.
def var i-tipo-desp as integer no-undo.
def var l-com-problema as logical no-undo.

def var raw-import           as raw     no-undo.


create tt-param.
tt-param.destino = 2.
tt-param.arquivo = session:temp-directory + "escc0026" + ".tmp".
 
/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}
/* definiá∆o de vari†veis  */


    def var rw-log-exec                            as rowid no-undo.
    def var c-erro-rpc as character format "x(60)" initial " " no-undo.
    def var c-erro-aux as character format "x(60)" initial " " no-undo.
    def var c-ret-temp as char no-undo.
    def var h-servid-rpc as handle no-undo.   
    define new shared stream str-rp.
    
    def var i-cod-emitente as integer no-undo.
    def var i-seq          as integer no-undo.

    def var i-cont          as integer no-undo.


def var h-acomp as handle no-undo.
def new global shared var i-ep-codigo-usuario  like mgmulti.empresa.ep-codigo no-undo.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def var v-num-reg-lidos      as int    no-undo.

define var d-preco as dec no-undo.
DEFINE VARIABLE data-ini-jr     AS DATE       NO-UNDO.
DEFINE VARIABLE data-fim-jr     AS DATE       NO-UNDO.
DEFINE VARIABLE estab-jr        AS CHAR       NO-UNDO.
DEFINE VARIABLE notas-jr        AS CHAR       NO-UNDO.
DEFINE VARIABLE cod-estabel-jr  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE descricao-jr1   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE descricao-jr2   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE motivo-jr       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE flag-erro       AS INTEGER    NO-UNDO.
DEFINE VARIABLE flag-erro-item  AS INTEGER    NO-UNDO.

DEFINE VARIABLE idx-jr          AS INTEGER    NO-UNDO.
DEFINE VARIABLE valor-dec       AS DECIMAL    NO-UNDO.

DEFINE VARIABLE cod-cond-pag-jr    AS INTEGER    NO-UNDO.
DEFINE VARIABLE cod-transp-jr      AS INTEGER    NO-UNDO.
DEFINE VARIABLE cod-cond-pag-jr2   AS INTEGER    NO-UNDO.
DEFINE VARIABLE it-codigo-jr       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cod-emitente-jr    AS INTEGER    NO-UNDO.
DEFINE VARIABLE usuar-comprador-jr AS CHARACTER  NO-UNDO.
def var d-tt-qtd as dec no-undo.
def var d-tt-val as dec no-undo.
def new global shared var v_cdn_empres_usuar   like mgmulti.empresa.ep-codigo        no-undo.

def var c-unid-aux as char no-undo.

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
    "Folha:" at 122 page-number /*(str-rp)*/ at 128 format ">>>>9" skip
    fill("-", 112) format "x(110)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabec.

     
form i-linha                   COLUMN-LABEL "Linha"       format "999999"             AT 001
     descricao-jr1             COLUMN-LABEL "item"        format "x(20)"             AT 010
     descricao-jr2             COLUMN-LABEL "Descricao"       format "x(20)"             AT 033
     Motivo-jr                 COLUMN-LABEL "Motivo do Erro"    format "x(75)"             AT 055
     with down width 132 no-box stream-io frame f-relat-09-132.

form header
    c-rodape format "x(132)"
    with stream-io width 132 no-labels no-box page-bottom frame f-rodape.

/*assign i-ep-codigo-usuario = tt-param.ep-codigo
           v_cdn_empres_usuar  = i-ep-codigo-usuario.
  */       
find first mgmulti.empresa no-lock
    where empresa.ep-codigo = string(tt-param.ep-codigo) no-error.
if  avail empresa
then
    assign c-empresa = empresa.razao-social.
else
    assign c-empresa = "".


/* include padr∆o para output de relat¢rios */
/*
{include/i-rpout.i &STREAM="stream str-rp"}
*/

output   /*stream str-rp*/ to value(tt-param.arquivo)  no-convert.

/* bloco principal do programa */

assign c-programa 	= "escc0026RP"
	c-versao	= "1.00"
	c-revisao	= ".00.000"
	c-empresa 	= " "
	c-sistema	= ""
	c-titulo-relat = "Importaá∆o da Tab.Preáos do Fornecedor".
    
    
assign v-num-reg-lidos = 0.

    
/* Cria Aplicaá∆o do Excel */


 
ASSIGN c-modelo-planilha = search(c-arquivo-import) 
       c-arq             = SESSION:TEMP-DIRECTORY.

if c-modelo-planilha = ? then do:

message "arquivo n∆o encontrado"
view-as alert-box.

return "nok".


end.

CREATE "Excel.Application" c-excel.
ASSIGN c-excel:DISPLAYAlerts = FALSE.
      tt-param.c-arquivo-jr = c-modelo-planilha.
      
DEF VAR c-arquivo AS CHAR NO-UNDO.

    ASSIGN c-arquivo = tt-param.c-arquivo-jr.
    
    assign c-planilha   = c-excel:Workbooks:OPEN(c-arquivo)
           c-relatorio  = c-excel:Sheets:item(1)
           c-arq-anexo  = (IF c-arq-anexo <> "" THEN "," ELSE "") + c-arquivo.


run utp/ut-acomp.p persistent set h-acomp.
    {utp/ut-liter.i Imprimindo *}
    run pi-inicializar in h-acomp (input RETURN-VALUE).
  


 view   /*stream str-rp*/  frame f-cabec.
 view   /*stream str-rp*/  frame f-rodape.


 

ASSIGN flag-erro = 0
       flag-erro-item = 0.
i-cod-emitente = int(c-relatorio:range("B" + STRING(1)):VALUE).
i-nr-contrato = int(c-relatorio:range("B" + STRING(2)):VALUE).

 


FIND FIRST contrato-for WHERE
    contrato-for.cod-emitente  = i-cod-emitente AND
    contrato-for.nr-contrato  = i-nr-contrato
    NO-LOCK NO-ERROR.
 
IF not AVAIL contrato-for THEN DO:
i-linha = 1.
    ASSIGN flag-erro = 9
           descricao-jr1  = "Contrato:"
           descricao-jr2  = STRING (i-nr-contrato)
           motivo-jr      = "N∆o Existe Este Contrato para este fornecedor".
         
             /*view   stream str-rp  frame f-cabec.
             view   stream str-rp  frame f-rodape.*/
             /*assign l-imprime = yes.*/
             DISPLAY /*stream str-rp */
                 i-linha
                 descricao-jr1 
                 descricao-jr2
                 motivo-jr
                 with stream-io frame f-relat-09-132.
                 down  with stream-io frame f-relat-09-132. 

END.


  
   

ASSIGN usuar-comprador-jr = if avail contrato-for then contrato-for.cod-comprado else "".

FIND FIRST usuar-mater WHERE
    usuar-mater.cod-usuario = usuar-comprador-jr 
    NO-LOCK NO-ERROR.

    IF NOT AVAIL usuar-mater OR usuar-mater.usuar-comprador = NO THEN DO:
    
        ASSIGN flag-erro = 9
               descricao-jr1  = "Comprador"
               descricao-jr2  = STRING (usuar-comprador-jr)
               motivo-jr      = "Comprador n∆o Existe".
             
                 /*view   stream str-rp  frame f-cabec.
                 view   stream str-rp  frame f-rodape.*/
                 /*assign l-imprime = yes.*/
                 DISPLAY /*stream str-rp */
                     i-linha
                     descricao-jr1
                     descricao-jr2
                     motivo-jr
                     with stream-io frame f-relat-09-132.
                     down  with stream-io frame f-relat-09-132. 
       
    END.

  find first pedido-compr where pedido-compr.nr-contrato = (if avail contrato-for then contrato-for.nr-contrato else 0) no-lock no-error.
 
   if avail pedido-compr then 
      cod-cond-pag-jr2 =  pedido-compr.cod-cond-pag.
      
    

   FIND FIRST cond-pagto WHERE
       cond-pagto.cod-cond-pag = cod-cond-pag-jr2 
       NO-LOCK NO-ERROR.

   IF NOT AVAIL cond-pagto or cod-cond-pag-jr2 = 0 THEN DO:

       ASSIGN flag-erro = 9
              i-linha
              descricao-jr1  = STRING (cod-cond-pag-jr2)
              descricao-jr2  = "Cond.Pagamento do Item"
              motivo-jr      = "Condiá∆o de Pagamento N∆o Existe ou zerada:" + STRING (cod-cond-pag-jr2).

/*                view   stream str-rp  frame f-cabec.
                view   stream str-rp  frame f-rodape.*/
                /*assign l-imprime = yes.*/
                DISPLAY /*stream str-rp */
                    descricao-jr1
                    descricao-jr2
                    motivo-jr
                    with stream-io frame f-relat-09-132.
                    down  with stream-io frame f-relat-09-132. 

   END.

 if  flag-erro = 0 and avail contrato-for then do:
    assign d-tt-qtd  = 0 
           d-tt-val  = 0.
           
    for each item-contrat of contrato-for no-lock.
    
    assign d-tt-qtd  = d-tt-qtd + item-contrat.qtd-total
           d-tt-val  = d-tt-val + item-contrat.val-total. 
    
    end.
       
    ASSIGN i-linha = 4. 


    DO WHILE i-linha <> 0 :
           flag-erro = 0 .
       ASSIGN i-linha  = i-linha + 1.
    
       ASSIGN it-codigo-jr = string (c-relatorio:range("A" + STRING(i-linha)):VALUE).
    
       IF it-codigo-jr = "" OR it-codigo-jr = ? THEN LEAVE.
   

        assign d-tt-qtd  = d-tt-qtd + decimal(string (c-relatorio:range("I" + STRING(i-linha)):VALUE))
               d-tt-val  = d-tt-val + decimal(string (c-relatorio:range("J" + STRING(i-linha)):VALUE)).
        

    
    end.
    
    if contrato-for.dec-1 > 0 and d-tt-qtd > contrato-for.dec-1 then do:
    
        ASSIGN flag-erro = 9
               flag-erro-item = 9
           descricao-jr1  = "Quantidade"
           descricao-jr2  = "Exceder† quantidade total"
           motivo-jr      = "Total em quantidade dos itens do contrato mais itens importados maior que o permitido".
         
             /*view   stream str-rp  frame f-cabec.
             view   stream str-rp  frame f-rodape.*/
             /*assign l-imprime = yes.*/
             DISPLAY /*stream str-rp */
                  i-linha
                 descricao-jr1
                 descricao-jr2
                 motivo-jr
                 with stream-io frame f-relat-09-132.

    
    end.
    
      if contrato-for.dec-2 > 0 and d-tt-val > contrato-for.dec-2 then do:
    
        ASSIGN flag-erro = 9
                flag-erro-item = 9 
           descricao-jr1  = "Valor"
           descricao-jr2  = "Exceder† Valor total"
           motivo-jr      = "Total em valor dos itens do contrato mais itens importados maior que o permitido".
         
             /*view   stream str-rp  frame f-cabec.
             view   stream str-rp  frame f-rodape.*/
             /*assign l-imprime = yes.*/
             DISPLAY /*stream str-rp */
                 i-linha 
                 descricao-jr1
                 descricao-jr2
                 motivo-jr
                 with stream-io frame f-relat-09-132.

    
    end.

    
  i-seq = 0.
    find last item-contrat no-lock
            where  item-contrat.nr-contrato  =  (if avail contrato-for then contrato-for.nr-contrato else 0) no-error.
   
   if avail item-contrat then do:
      i-seq = item-contrat.num-seq-item.
   end.   
   
ASSIGN i-linha = 4. 
  
if flag-erro = 0 and avail contrato-for then 

    DO WHILE i-linha <> 0 :
           flag-erro = 0 .
       ASSIGN i-linha  = i-linha + 1.
    
       ASSIGN it-codigo-jr = string (c-relatorio:range("A" + STRING(i-linha)):VALUE)       
                       descricao-jr1  = STRING (it-codigo-jr)
                      descricao-jr2  = string (c-relatorio:range("b" + STRING(i-linha)):VALUE).

       IF it-codigo-jr = "" OR it-codigo-jr = ? THEN LEAVE.
    
       run pi-acompanhar in h-acomp (input ( /*"Verificando Erros na Planilha da Tab.Preáos - " +*/  it-codigo-jr)).
        
       
       FIND FIRST item WHERE
           item.it-codigo = trim(it-codigo-jr)
           NO-LOCK NO-ERROR.
    
        
       IF NOT AVAIL item THEN DO:
    
           
           ASSIGN flag-erro = 9
                 flag-erro-item = 9 


                  motivo-jr      = "Item N∆o Existe".
                
/*                    view   stream str-rp  frame f-cabec.
                    view   stream str-rp  frame f-rodape.*/
                    /*assign l-imprime = yes.*/
                    DISPLAY /*stream str-rp */
                        i-linha
                        descricao-jr1
                        descricao-jr2
                        motivo-jr
                        with stream-io frame f-relat-09-132.
                        down  with stream-io frame f-relat-09-132. 
                  next.
       END. 
    
       
    
    
    
    d-preco = 0.
    d-preco = DEC (c-relatorio:range("D" + STRING(i-linha)):VALUE) no-error.
    
       IF d-preco <= 0 THEN DO:
    
           ASSIGN flag-erro = 9
                flag-erro-item = 9 
                  
                  motivo-jr      = "Preáo do Item Errado: " + string(d-preco).
                
/*                    view   stream str-rp  frame f-cabec.
                    view   stream str-rp  frame f-rodape.*/
                    /*assign l-imprime = yes.*/
                    DISPLAY /*stream str-rp*/ 
                         i-linha
                        descricao-jr1
                        descricao-jr2
                        motivo-jr
                        with stream-io frame f-relat-09-132.
                        down  with stream-io frame f-relat-09-132. 
       
       END.
    
      
     
    
     
    i-tipo-desp  = 0.
    i-tipo-desp = int (c-relatorio:range("H" + STRING(i-linha)):VALUE) no-error.
    
    find first tipo-rec-desp where tipo-rec-desp.tp-codigo = i-tipo-desp no-lock no-error.
    
    
    if not avail tipo-rec-desp then 
        find first tipo-rec-desp where tipo-rec-desp.tp-codigo = (if avail item then item.tp-desp-padrao else 0) no-lock no-error.
    if avail tipo-rec-desp and avail item then

    i-tipo-desp = item.tp-desp-padrao.    
 
     if not avail tipo-rec-desp then do:
    
     
    
           ASSIGN flag-erro = 9
                flag-erro-item = 9
                 
                  motivo-jr      = "tipo de receita e despesa invalido:" + string(i-tipo-desp).
                
/*                    view   stream str-rp  frame f-cabec.
                    view   stream str-rp  frame f-rodape.*/
                    /*assign l-imprime = yes.*/
                    DISPLAY /*stream str-rp*/ 
                    i-linha
                        descricao-jr1
                        descricao-jr2
                        motivo-jr
                        with stream-io frame f-relat-09-132.
                        down  with stream-io frame f-relat-09-132. 
       
     END.

    
       FIND FIRST item-contrat OF contrato-for WHERE
           item-contrat.it-codigo = trim(it-codigo-jr)
           NO-LOCK NO-ERROR.
    
        
       IF  AVAIL item-contrat THEN DO:
    
           
           ASSIGN flag-erro = 9
                   flag-erro-item = 9 
                  
                  motivo-jr      = "Item j† Existe no contrato seq:" + string(item-contrat.num-seq-item).
                
/*                    view   stream str-rp  frame f-cabec.
                    view   stream str-rp  frame f-rodape.*/
                    /*assign l-imprime = yes.*/
                    DISPLAY /*stream str-rp */
                        i-linha
                        descricao-jr1
                        descricao-jr2
                        motivo-jr
                        with stream-io frame f-relat-09-132.
                        down  with stream-io frame f-relat-09-132. 
       
       END. 
       
       

       for each  b-item-contrat where b-item-contrat.it-codigo  = it-codigo-jr and
                                  b-item-contrat.nr-contrato <> contrato-for.nr-contrato no-lock,
       each b-contrato-for of b-item-contrat no-lock.
   
          if 
           b-contrato-for.dt-ter-validade <= contrato-for.dt-ter-validade and
           b-contrato-for.dt-ter-validade >=  contrato-for.dt-ini-validade  then do:
                        
           ASSIGN flag-erro = 9
                   flag-erro-item = 9 
                  
                  motivo-jr      = "Item j† cadastrado em outro contrato vigente seq:" + string(b-item-contrat.nr-contrato) + "-" + string(b-item-contrat.num-seq-item).
                
/*                    view   stream str-rp  frame f-cabec.
                    view   stream str-rp  frame f-rodape.*/
                    /*assign l-imprime = yes.*/
                    DISPLAY /*stream str-rp */
                        i-linha
                        descricao-jr1
                        descricao-jr2
                        motivo-jr
                        with stream-io frame f-relat-09-132.
                        down  with stream-io frame f-relat-09-132. 

          end.
       end.

       
       c-unid-aux = string (c-relatorio:range("C" + STRING(i-linha)):VALUE).
                  
                      find first tab-unidade where tab-unidade.un = c-unid-aux
                                 no-lock no-error. 
                        if not avail tab-unidade then do:           
                    
                              
                              ASSIGN flag-erro = 9
                                    flag-erro-item = 9 
                                   
                                     motivo-jr      = "unidade de medida n∆o cadastrada: " + c-unid-aux .
                                   
                   /*                    view   stream str-rp  frame f-cabec.
                                       view   stream str-rp  frame f-rodape.*/
                                       /*assign l-imprime = yes.*/
                                       DISPLAY /*stream str-rp */
                                            i-linha
                                           descricao-jr1
                                           descricao-jr2
                                           motivo-jr
                                           with stream-io frame f-relat-09-132.
                                           down  with stream-io frame f-relat-09-132. 
                    
              end.
        

       
       find first item-fornec no-lock
                where item-fornec.it-codigo    =  trim(it-codigo-jr) and
                      item-fornec.cod-emitente <> contrato-for.cod-emitente and
                      item-fornec.perc-comp   > 0 no-error.
       
       if avail item-fornec then do:
           
           ASSIGN flag-erro = 9
                 flag-erro-item = 9 
                  
                  motivo-jr      = "Item com percentual relacionamento outro fornecedor: " + string(item-fornec.cod-emitente) .
                
/*                    view   stream str-rp  frame f-cabec.
                    view   stream str-rp  frame f-rodape.*/
                    /*assign l-imprime = yes.*/
                    DISPLAY /*stream str-rp */
                        i-linha
                        descricao-jr1
                        descricao-jr2
                        motivo-jr
                        with stream-io frame f-relat-09-132.
                        down  with stream-io frame f-relat-09-132. 
                        
                        next.
        end.
        
         find first item-fornec no-lock
                where item-fornec.it-codigo    =  trim(it-codigo-jr) and
                      item-fornec.cod-emitente = contrato-for.cod-emitente  no-error.
       
       if avail item-fornec then do:
             
             
             
             if  item-fornec.unid-med-for <> string (c-relatorio:range("C" + STRING(i-linha)):VALUE)
                then do:
                 
                 ASSIGN flag-erro = 9
                       flag-erro-item = 9 
                         
                        motivo-jr      = "unidade de medida diferente da do fornecedor: " + item-fornec.unid-med-for .
                      
      /*                    view   stream str-rp  frame f-cabec.
                          view   stream str-rp  frame f-rodape.*/
                          /*assign l-imprime = yes.*/
                          DISPLAY /*stream str-rp */
                                i-linha
                              descricao-jr1
                              descricao-jr2
                              motivo-jr
                              with stream-io frame f-relat-09-132.
                              down  with stream-io frame f-relat-09-132. 
                              next.
              end.
              
       end.       
       
              
                  
        
            if flag-erro = 0  then do:
            
            for first item-fornec  exclusive-lock
                where item-fornec.it-codigo    =  trim(it-codigo-jr) and
                      item-fornec.cod-emitente = contrato-for.cod-emitente .
             item-fornec.contr-forn = yes.
             item-fornec.perc-comp = 100.
             
            end.
            
            find first item-fornec  no-lock
                where item-fornec.it-codigo    =  trim(it-codigo-jr) and
                      item-fornec.cod-emitente = contrato-for.cod-emitente  no-error.

            end.
             
      
        
          

        
      
      
      if index("1 , 2",trim(string (int(c-relatorio:range("E" + STRING(i-linha)):VALUE)) ) ) = 0 then do:
        
           ASSIGN flag-erro = 9
                 flag-erro-item = 9 
               
                  motivo-jr      = "Tipo de icm deve ser 1 ou 2" .
                
/*                    view   stream str-rp  frame f-cabec.
                    view   stream str-rp  frame f-rodape.*/
                    /*assign l-imprime = yes.*/
                    DISPLAY /*stream str-rp */
                        i-linha 
                        descricao-jr1
                        descricao-jr2
                        motivo-jr
                        with stream-io frame f-relat-09-132.
                        down  with stream-io frame f-relat-09-132. 
       end.                        
    
      IF flag-erro = 0 THEN DO: /* Planilha Sem erro */
    
      
    
    
       
     
    
           ASSIGN it-codigo-jr = string (c-relatorio:range("A" + STRING(i-linha)):VALUE).
    
    
           IF it-codigo-jr = "" OR it-codigo-jr = ? THEN LEAVE. 
    
           run pi-acompanhar in h-acomp (input ("Gravando itens no contrato - " + it-codigo-jr)).
           
     
        
       
       FIND FIRST item WHERE
           item.it-codigo = trim(it-codigo-jr)
           NO-LOCK NO-ERROR.
    
     
     
     
     create tt-item-contrato.
    
     
        assign l-com-problema = no
               tt-item-contrato.im-cod-emitente         = contrato-for.cod-emitente
              
               tt-item-contrato.im-nr-contrat           = contrato-for.nr-contrato
              
               tt-item-contrato.im-preco-unit           = DEC (d-preco) * 100000
               tt-item-contrato.im-qtd-minima           =  10000
               tt-item-contrato.im-sld-val              = 0
               tt-item-contrato.im-val-fatur-minimo     = 0
               tt-item-contrato.im-mo-codigo            = contrato-for.mo-codigo
               tt-item-contrato.im-log-libera           = yes
               tt-item-contrato.im-it-codigo            = trim(it-codigo-jr)
               tt-item-contrato.im-val-total            = 0
               tt-item-contrato.im-cod-refer            = ""
               tt-item-contrato.im-codigo-ipi           = no
               tt-item-contrato.im-codigo-icm           = int(string (c-relatorio:range("E" + STRING(i-linha)):VALUE))
               tt-item-contrato.im-un                   = string (c-relatorio:range("C" + STRING(i-linha)):VALUE)
               tt-item-contrato.im-contato              = "Vendas"
               i-seq = i-seq + 1
               tt-item-contrato.im-num-seq-item         = i-seq 
               tt-item-contrato.im-frequencia           = 1
               tt-item-contrato.im-ind-sit-item         = 1 
               tt-item-contrato.im-qtd-total            = decimal(string (c-relatorio:range("I" + STRING(i-linha)):VALUE)) * 10000 
               tt-item-contrato.im-val-total            = decimal(string (c-relatorio:range("J" + STRING(i-linha)):VALUE)) * 10000 
               tt-item-contrato.im-ind-un-contrato      = 1
               tt-item-contrato.im-sld-qtd              = 0
               tt-item-contrato.im-acum-rec-val         = 0
               tt-item-contrato.im-acum-rec-qtd         = 0
               tt-item-contrato.im-ind-tipo-control-val = 1
               tt-item-contrato.im-sld-qtd-liber        = 0 
               tt-item-contrato.im-sld-val-liber        = 0 
               tt-item-contrato.im-log-control-event    = no
               tt-item-contrato.im-ind-caract-item      = 1
               tt-item-contrato.im-log-obrig-item       = no
               tt-item-contrato.im-log-ind-multa        = no
               tt-item-contrato.im-perc-multa-dia       = 0
               tt-item-contrato.im-perc-multa-limite    = 0
               tt-item-contrato.im-cod-depos            = item.deposito-pad
               tt-item-contrato.im-aliquota-icm         =  DEC (c-relatorio:range("F" + STRING(i-linha)):VALUE) * 100
               tt-item-contrato.im-aliquota-ipi         =  DEC (c-relatorio:range("G" + STRING(i-linha)):VALUE) * 100
               tt-item-contrato.im-aliquota-iss         = 0
               tt-item-contrato.im-tp-despesa           = i-tipo-desp
               tt-item-contrato.im-cod-cond-pag         = cod-cond-pag-jr2
               tt-item-contrato.im-frete-ped            = no
     
               tt-item-contrato.im-preco-fornec     = tt-item-contrato.im-preco-unit 
               tt-item-contrato.im-taxa-financ      = no
               tt-item-contrato.im-val-frete        = 0
               tt-item-contrato.im-val-taxa         = 0
               tt-item-contrato.im-prazo-ent        = int (c-relatorio:range("B" + STRING(3)):VALUE)
               tt-item-contrato.im-dat-cotac        = today
               tt-item-contrato.im-preco-base       =  tt-item-contrato.im-preco-unit 
               tt-item-contrato.im-cod-comprado     = contrato-for.cod-comprado 
               tt-item-contrato.im-perc-desconto    = 0
               tt-item-contrato.im-narrat-compra    = ""
               tt-item-contrato.im-ind-tipo-control = 2
               tt-item-contrato.im-pre-unit-for     =  tt-item-contrato.im-preco-unit 
               tt-item-contrato.im-dat-base         = today
               tt-item-contrato.im-sld-qtd-receb    = 0
               tt-item-contrato.im-sld-val-receb    = 0
               tt-item-contrato.im-ordem-base       = 0
               
               .
    
    
            raw-transfer tt-item-contrato to raw-import.
            run cnp/cn0206d1.p (input raw-import, "IC", 00).
            run cnp/cn0206d1.p (input raw-import, "IC", 01).
          i-cont = i-cont + 1.
          
          find first item where item.it-codigo = trim(it-codigo-jr) no-lock no-error.
          
       find first item-fornec  
                where item-fornec.it-codigo    =  trim(it-codigo-jr) and
                      item-fornec.cod-emitente = contrato-for.cod-emitente  no-error.
       if not avail item-fornec and avail item then do:
           create item-fornec.
     
             assign item-fornec.it-codigo = item.it-codigo
             item-fornec.cod-emitente = contrato-for.cod-emitente
             item-fornec.item-do-forn = item.it-codigo
             item-fornec.unid-med-for = tt-item-contrato.im-un
             item-fornec.contr-forn = yes.
             assign item-fornec.fator-conver = 1
             item-fornec.num-casa-dec = 0.
                                                   
             assign item-fornec.tempo-ressup = item.res-for-comp
             item-fornec.cod-cond-pag = tt-item-contrato.im-cod-cond-pag
             item-fornec.ativo = yes
             item-fornec.lote-minimo = 1
             item-fornec.lote-mul-for = 1
             item-fornec.perc-compra = 100
             item-fornec.cot-aut = no
             item-fornec.classe-repro = 1. 
       end.               
       
         if item.un <> tt-item-contrato.im-un then do:
             ASSIGN  
                  descricao-jr1  = "***alerta Linha:" + string(i-linha)
                  descricao-jr2  = "unidade item x fornecedor diferente verifique fator de convers∆o item x fornecedor"
                  motivo-jr      = "ajustar fator de convers∆o" .
                
/*                    view   stream str-rp  frame f-cabec.
                    view   stream str-rp  frame f-rodape.*/
                    /*assign l-imprime = yes.*/
                    DISPLAY /*stream str-rp */
                        descricao-jr1
                        descricao-jr2
                        motivo-jr
                        with stream-io frame f-relat-09-132.
                        down  with stream-io frame f-relat-09-132. 

         end.
             
    
     end. /*sem erro*/
     
    END. /* do while da linha da planilha */
    
end.    

 
if i-cont = 0 then do:
PUT /*stream str-rp*/
        " " AT 01
        "IMPORTAÄ«O DA TABELA N«O FOI REALIZADA  !!!"  AT 01
        "------------------------------------------- "  AT 01.
                                  
end.
else do:

IF flag-erro-item = 0 THEN DO: /* Planilha Sem erro */
 

    PUT /*stream str-rp*/
        " " AT 01
        "IMPORTAÄ«O DA TABELA REALIZADA SEM ERROS !!!"  AT 01
        "------------------------------------------- "  AT 01
        "IMPORTADOS:" AT 01 I-CONT                                   .

end.
ELSE DO:

 PUT /*stream str-rp*/
        " " AT 01
        "IMPORTAÄ«O DA TABELA REALIZADA COM ERROS !!!"  AT 01
        "------------------------------------------- "  AT 01
        "IMPORTADOS:"  AT 01 I-CONT                                  .

END.

def var i-seq-anexo as integer no-undo.


def buffer b-anexo-contrat for anexo-contrat.

if i-cont > 0 then do:


        FIND FIRST contrato-for WHERE
            contrato-for.cod-emitente  = i-cod-emitente AND
            contrato-for.nr-contrato  = i-nr-contrato
            NO-LOCK NO-ERROR.
        
            IF  AVAIL contrato-for THEN DO:
                find first anexo-contrat where 
                     anexo-contrat.nr-contrato = contrato-for.nr-contrato and
                     anexo-contrat.des-anexo     = "Itens do Contrato" no-lock no-error.
                     
                 if not avail anexo-contrat then do:
                   
                    find last anexo-contrat where 
                         anexo-contrat.nr-contrato = contrato-for.nr-contrato   no-lock no-error.
                        i-seq-anexo = 1.
                        if avail anexo-contrat then 
                          i-seq-anexo = anexo-contrat.num-seq-anexo + 1.
                          
                        create b-anexo-contrat.
                        assign b-anexo-contrat.nr-contrato   = contrato-for.nr-contrato
                               b-anexo-contrat.num-seq-anexo = i-seq-anexo
                               b-anexo-contrat.des-anexo     = "Itens do Contrato"
                               b-anexo-contrat.dat-revisao   = today
                               b-anexo-contrat.nr-alter      = 0
                               b-anexo-contrat.ind-sit-anexo = 1
                               b-anexo-contrat.usuario       = c-seg-usuario.
            
                         
                 
                 end.
            
            end.
        end.
 end.
run pi-finalizar in h-acomp.

c-planilha:CLOSE().

release object c-relatorio.
release object c-planilha.
release object c-excel.




/* fechamento do output do relat¢rio  */
output  /*stream str-rp*/ close.
/*
{include/i-rpclo.i &STREAM="stream str-rp"}
*/

dos silent copy value(tt-param.arquivo) v:\temp.


return "OK":U.


