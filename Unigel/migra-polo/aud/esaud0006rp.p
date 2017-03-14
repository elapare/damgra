/*****************************************************************************
**
**       Programa: esaud0006rp.p
**
**       Author...........: Amgra / Edson
**       Created..........: 25/04/2011     
**
**       Objetivo: Exporta‡Æo de Cota‡Æo Compras para auditoria
**
**       OBS.....: 
**
*******************************************************************************/
define buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "esaud0006rp".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.


DEFINE input param c-estab-ini AS CHARACTER  NO-UNDO.  /*solic-318*/ 
DEFINE input param c-estab-fim AS CHARACTER  NO-UNDO.  /*solic-318*/ 
DEFINE input param data-emissao-ini  AS DATE   INITIAL 05/01/2012    NO-UNDO.
DEFINE input param data-emissao-fim AS DATE    INITIAL 05/15/2012   NO-UNDO.
define input param tg-imprime as logical no-undo.

/* 
DEFINE var c-estab-ini AS CHARACTER  INITIAL "362" NO-UNDO.
DEFINE var c-estab-fim AS CHARACTER  INITIAL "362"  NO-UNDO.
DEFINE var data-emissao-ini  AS DATE   INITIAL 01/01/2012    NO-UNDO.
DEFINE var data-emissao-fim AS DATE    INITIAL 08/15/2012   NO-UNDO.
define var tg-imprime as logical no-undo.
*/


/*
run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").
*/

/****************** Defini‡Æo de Tabelas Tempor rias do Relat¢rio **********************/

        

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

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 



/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

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
DEFINE VARIABLE arquivo-jr   AS CHARACTER  FORMAT "x(50)" NO-UNDO.
/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

/****************** Defini‡ao de Vari veis dos Calculos do Relat¢rio *************************/ 


/***************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/
def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.

/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form HEADER
    fill("-", 170) format "x(170)" SKIP 
    WITH DOWN WIDTH 170 NO-BOX STREAM-IO FRAME f-relat-linha.

form HEADER
    fill(" ", 170) format "x(170)" SKIP 
    WITH DOWN WIDTH 170 NO-BOX STREAM-IO FRAME f-relat-branco.


 
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
DEFINE VARIABLE dt-sai-transf AS DATE       NO-UNDO.
DEFINE VARIABLE dt-ENT-transf AS DATE       NO-UNDO.
define new shared stream str-rp.
DEF VAR c-arquivo_2 AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR c-arquivo_txt AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR c-arquivo_xls AS CHAR FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE c-aprov AS CHARACTER  FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE c-aprov-ax AS CHARACTER  FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE l-aprov AS LOGICAL    NO-UNDO.
DEFINE VARIABLE c-motivo AS CHARACTER  NO-UNDO. 
DEFINE VARIABLE c-data AS CHARACTER  NO-UNDO. 
DEFINE VARIABLE d-p-ipi AS DECIMAL    NO-UNDO.
DEFINE VARIABLE c-chave AS CHARACTER   NO-UNDO.
DEFINE BUFFER b-comprador FOR  usuar_mestre.
DEFINE BUFFER b-aprov       FOR  usuar_mestre.
DEFINE BUFFER b-aprov-alter FOR  usuar_mestre.
DEFINE BUFFER b-doc FOR mla-doc-pend-aprov.
DEFINE TEMP-TABLE tt-aprov
    FIELD num-pedido LIKE requisicao.nr-requisicao
    FIELD seq AS INTEGER
    FIELD data AS DATE  FORMAT "99/99/9999"
    FIELD aprovador AS CHAR FORMAT "x(60)"
    FIELD transacao LIKE mla-doc-pend-aprov.nr-trans 
    FIELD marca AS LOGICAL INITIAL NO
    
    INDEX seq IS PRIMARY UNIQUE
                     seq.




assign c-programa     = "esaud0006rp"
       c-versao       = "2.00"
       c-revisao      = "1.00.000"
       c-titulo-relat = "Exporta‡Æo de Cota‡Æo Compras para auditoria"
       c-sistema      = "".

form header
    fill("-", 170) format "x(170)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 159 page-number(str-rp) at 166 format ">>>>9" skip
    fill("-", 148) format "x(148)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") SKIP
    fill("-", 170) format "x(170)" skip
    with stream-io width 170 no-labels no-box page-top frame f-cabec.

form header
    c-rodape format "x(170)"
    with stream-io width 170 no-labels no-box page-bottom frame f-rodape.
 
/* for each e disp */



 
run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Gerando Relat¢rio, Aguarde... ").

assign v-num-reg-lidos = 0.
   
ASSIGN c-arquivo_txt = session:TEMP-DIRECTORY + "esaud0006_cotacoes_" + replace(STRING(TIME,"HH:MM:SS"),":","") + ".csv".

OUTPUT TO VALUE (c-arquivo_txt) NO-CONVERT.   
PUT      ";" 
    SKIP.
  PUT 
        "Exporta‡Æo de Cota‡Æo Compras para auditoria" ";"
         SKIP.
PUT      ";" 
    SKIP.
PUT      ";" 
    SKIP.
PUT      ";" 
    SKIP.

    
PUT   UNFORMATTED
    "Estab"  ";"
    "Nr.Cota‡Æo"  ";"
    "Data Cota‡Æo"  ";"
    "Data Ordem Compra"  ";"    
    "Situa‡Æo Cota‡Æo" ";"
    "No da Requisi‡Æo"  ";"
    "No Ordem Compra"  ";"
    "Requisitante"  ";"
    "Comprador"  ";"    
    "Usu rio Ordem"  ";"
    "Usu rio Cota‡Æo"  ";"
    "Aprovou Pedido/Contrato"  ";"
    "Nr.Contrato"  ";"
    "Cod.Fornecedor"  ";"
    "nome Fornecedor"  ";"
    "C¢digo do Item"  ";"
    "Descri‡Æo do Item"  ";"
    "Narrativa"  ";"    
    "Quant. do Item"  ";"
    "Pre‡o Unit rio do Item"  ";"
    "Data do Cancelamento"  ";"   
    "Vlr.Total Cota‡Æo"  ";"   
    "Tipo Compra"  ";"   
    "Dias Validade"  ";"   
    "Contato Cliente"  ";"   
    "Condi‡Æo Pagto"  
    SKIP.

 

 FOR EACH  ordem-compra where
    
     ordem-compra.data-emissao >= data-emissao-ini AND
     ordem-compra.data-emissao <= data-emissao-fim AND
      ordem-compra.cod-estabel >= c-estab-ini  AND
      ordem-compra.cod-estabel <= c-estab-fim 
     NO-LOCK,

     EACH cotacao-item OF ordem-compra NO-LOCK,


     
     EACH prazo-compra OF ordem-compra NO-LOCK,    

     EACH ITEM WHERE ITEM.it-codigo = ordem-compra.it-codigo NO-LOCK.  
     
     if ordem-compra.nr-contrato <> 0 and
         can-find (first item-contrat where 
        item-contrat.nr-contrato = ordem-compra.nr-contrato  and
        item-contrat.num-seq-item = ordem-compra.num-seq-item and
        item-contrat.ind-tipo-control = 1 no-lock) then next.


     
      assign v-num-reg-lidos = v-num-reg-lidos + 1.
       if substring(string(v-num-reg-lidos,"99999999"),7,2) = "00" then 
        run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
       

     FIND FIRST emitente WHERE
         emitente.cod-emitente = cotacao-item.cod-emitente
         NO-LOCK NO-ERROR.
     
         c-data = "".
      find first pedido-compr where pedido-compr.num-pedido = ordem-compra.num-pedido no-lock no-error.
      
     c-aprov = "".
      
      if avail pedido-compr then 
           RUN pi-aprovadores.
     
      FOR EACH tt-aprov WHERE tt-aprov.marca BREAK BY tt-aprov.seq.
           c-aprov-ax = tt-aprov.aprovador.
           

             IF c-aprov = "" THEN 
                 c-aprov = c-aprov-ax.
             ELSE DO:
                 IF INDEX (c-aprov,c-aprov-ax) = 0 THEN
                  c-aprov = c-aprov + "," + c-aprov-ax.
             END.
       
        END.
       


      
       find first requisicao OF ordem-compra NO-LOCK no-error.
      
     PUT UNFORMATTED
         ordem-compra.cod-estabel ";" 
         cotacao-item.seq-cotac ";"
         cotacao-item.data-cotacao ";"
         ordem-compra.data-emissao ";"
         IF cotacao-item.cot-aprovada = YES THEN "Aprovada" ELSE 
                    "NÆo Aprovada" ";"
         ordem-compra.nr-requisicao  ";"
         ordem-compra.numero-ordem  ";"
         if avail requisicao then requisicao.nome-abrev else ordem-compra.requisitante ";"
         ordem-compra.cod-comprado  ";"
         ordem-compra.usuario  ";"
         
         cotacao-item.usuario ";"
         c-aprov   ";"
         ordem-compra.nr-contrato ";"
         cotacao-item.cod-emitente ";"
         IF AVAIL emitente THEN emitente.nome-abrev ELSE 
           " " ";"
         item.it-codigo ";"
         replace(REPLACE(IF SUBSTRING(item.desc-item,1,1) = CHR(34) THEN SUBSTRING(item.desc-item,2,60) ELSE item.desc-item ,";",","),chr(10),",")  ";"
         REPLACE(replace(replace(replace(ordem-compra.narrativa,CHR(10)," "),CHR(13)," "),CHR(9)," "),";",",")  ";"

         prazo-compra.quantidade ";"
         cotacao-item.preco-fornec ";"
         " "  ";"   /* Data do Cancelamento */
         prazo-compra.quantidade * cotacao-item.preco-unit  ";"
         IF ordem-compra.natureza = 1 THEN "Compra" ELSE 
                    IF ordem-compra.natureza = 2 THEN "Servi‡o" ELSE
                        "Beneficiamento" ";"
         cotacao-item.dias-validade ";"
         
         ordem-compra.contato ";"
         ordem-compra.cod-cond-pag

    SKIP.


 END.



    
    

 OUTPUT CLOSE.


c-arquivo_xls = session:TEMP-DIRECTORY + "esaud0006_cotacoes_" + replace(STRING(TIME,"HH:MM:SS"),":","") + ".xlsx".

 

    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

            
    /* cria planilha*/
    c-arquivo_2 = c-arquivo_xls.       
    c-planilha  = c-excel:Workbooks:OPEN(c-arquivo_txt).
    c-planilha:SAVEas(c-arquivo_2,51,,,,,).
             
    c-planilha:CLOSE().

 
          
    ASSIGN c-planilha  = c-excel:Workbooks:OPEN(c-arquivo_2)
    c-relatorio = c-excel:Sheets:item(1).
     
       run pi-acompanhar in h-acomp(input "Aguarde formatando planilha gerada").

         

    RUN pi-salva-planilha.
 run pi-acompanhar in h-acomp(input "Aguarde copiando planilha gerada").
 dos silent copy value(c-arquivo_2) v:\temp.

    RUN pi-finalizar IN h-acomp.

   
    
    if tg-imprime = NO then 
      c-excel:visible = yes.
    else do: 
     
      c-excel:QUIT().
     end.
      

    RELEASE OBJECT c-relatorio.
    RELEASE OBJECT c-planilha.

    RELEASE OBJECT c-excel.
 

RETURN 'OK'.



PROCEDURE pi-salva-planilha:
    
 
            c-relatorio:range("a2"):Select.
            c-relatorio:range("a2"):Font:FontStyle = "Negrito".
            c-relatorio:range("a2"):Font:Size = 14. 

            c-relatorio:range("a6:z6"):Interior:ColorIndex = 55.
            
 
            c-relatorio:range("a6:z6"):Font:Name = "Arial".
             
            c-relatorio:range("a6:z6"):Font:FontStyle = "Negrito".
            c-relatorio:range("a6:z6"):Font:Size = 10.
            c-relatorio:range("a6:z6"):Font:ColorIndex = 2.
            
            c-relatorio:Rows("6:6"):Autofilter (,,,).

      
            c-relatorio:Columns("c:d"):NumberFormat = "dd/mm/aaaa".
            c-relatorio:Columns("u:u"):NumberFormat = "dd/mm/aaaa".

 
             c-relatorio:Columns("r:t"):NumberFormat = "#.##0,0000".
              c-relatorio:Columns("v:v"):NumberFormat = "#.##0,0000".

          

            
            c-relatorio:Cells:Select.
            c-relatorio:Cells:EntireColumn:AutoFit.
            c-relatorio:Columns("A:A"):ColumnWidth = 14.
            c-relatorio:range("a2"):Select.

            c-relatorio:range("a1"):Select.



     c-planilha:SAVE().

END PROCEDURE.


PROCEDURE pi-aprovadores.

DEFINE VARIABLE i-tipo AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-tipo-1 AS INTEGER     NO-UNDO.
DEFINE VARIABLE l-aprovado AS LOGICAL     NO-UNDO.

                
         l-aprovado = YES.
    IF pedido-comp.nr-contrato = 0  THEN DO:

    
         DO i-tipo-1 = 7 TO 8:
             FOR first b-doc WHERE  
                 not b-doc.historico and
                 b-doc.cod-tip-doc = i-tipo-1 AND
                 b-doc.chave-doc = STRING( pedido-comp.num-pedido) AND  
                (b-doc.ind-situacao = 1)
               NO-LOCK USE-INDEX pend-3.
            
                    assign l-aprovado = no.
        
             END.
         END.
    
         IF NOT l-aprovado THEN RETURN "NOK".
    
         FOR EACH tt-aprov.
             DELETE tt-aprov.
         END.

         DO i-tipo = 7 TO 8:
             
             FOR EACH mla-doc-pend-aprov WHERE  
                      not mla-doc-pend-aprov.historico and
                      mla-doc-pend-aprov.cod-tip-doc = (15 - i-tipo) AND
                      mla-doc-pend-aprov.chave-doc = STRING( pedido-comp.num-pedido)  
                      NO-LOCK USE-INDEX pend-3 BREAK BY   mla-doc-pend-aprov.chave-doc BY mla-doc-pend-aprov.dt-aprova DESCEND .
        
                FIND FIRST b-aprov WHERE b-aprov.cod_usuario = mla-doc-pend-aprov.cod-usuar NO-LOCK NO-ERROR.
                FIND FIRST b-aprov-alter WHERE b-aprov-alter.cod_usuario = mla-doc-pend-aprov.cod-usuar-altern  NO-LOCK NO-ERROR.
                FIND FIRST b-comprador WHERE b-comprador.cod_usuario = pedido-compr.responsavel NO-LOCK NO-ERROR.
        
                IF mla-doc-pend-aprov.ind-situacao = 2 OR
                   mla-doc-pend-aprov.ind-situacao = 4 THEN
        
                    FIND FIRST tt-aprov WHERE tt-aprov.seq = mla-doc-pend-aprov.seq-aprov NO-ERROR.
        
                    IF NOT AVAIL tt-aprov THEN
                    DO:
                        CREATE tt-aprov.
                        ASSIGN tt-aprov.seq        = mla-doc-pend-aprov.seq-aprov 
                               tt-aprov.data       = mla-doc-pend-aprov.dt-aprova
                               tt-aprov.aprovador  = IF mla-doc-pend-aprov.cod-usuar-altern <> "" and
                                                        AVAIL(b-aprov-alter) THEN b-aprov-alter.nom_usuario ELSE
                                                            (IF AVAIL b-aprov THEN b-aprov.nom_usuario ELSE "")
                               tt-aprov.transacao  = mla-doc-pend-aprov.nr-trans 
                               tt-aprov.num-pedido = pedido-compr.num-pedido.
                    END.
                    ELSE DO:
                             IF  tt-aprov.transacao <  mla-doc-pend-aprov.nr-trans  THEN DO:
                                 ASSIGN
                                         tt-aprov.seq        = mla-doc-pend-aprov.seq-aprov 
                                         tt-aprov.data       = mla-doc-pend-aprov.dt-aprova
                                         tt-aprov.aprovador  = IF mla-doc-pend-aprov.cod-usuar-altern <> "" and
                                                                AVAIL(b-aprov-alter) THEN b-aprov-alter.nom_usuario ELSE
                                                                    (IF AVAIL b-aprov THEN b-aprov.nom_usuario ELSE "")
                                         tt-aprov.transacao  =   mla-doc-pend-aprov.nr-trans
                                         tt-aprov.num-pedido = pedido-compr.num-pedido.
                    
                             END.
                    END.
                     
             END.
         END.
    END.
    ELSE 
    DO:
              
             FOR first b-doc WHERE  
                 not b-doc.historico and
                 b-doc.cod-tip-doc = 13 AND
                 b-doc.chave-doc = STRING( pedido-comp.nr-contrato) AND  
                (b-doc.ind-situacao = 1)
               NO-LOCK USE-INDEX pend-3.
            
                    assign l-aprovado = no.
        
             END.
        
    
         IF NOT l-aprovado THEN RETURN "NOK".
    
         FOR EACH tt-aprov.
             DELETE tt-aprov.
         END.

         DO:
             
             FOR EACH mla-doc-pend-aprov WHERE  
                      not mla-doc-pend-aprov.historico  and
                      mla-doc-pend-aprov.cod-tip-doc = 13 AND
                      mla-doc-pend-aprov.chave-doc = STRING( pedido-comp.nr-contrato)  
                      NO-LOCK USE-INDEX pend-3 BREAK BY   mla-doc-pend-aprov.chave-doc BY mla-doc-pend-aprov.dt-aprova DESCEND .
        
                FIND FIRST b-aprov WHERE b-aprov.cod_usuario = mla-doc-pend-aprov.cod-usuar NO-LOCK NO-ERROR.
                FIND FIRST b-aprov-alter WHERE b-aprov-alter.cod_usuario = mla-doc-pend-aprov.cod-usuar-altern  NO-LOCK NO-ERROR.
                FIND FIRST b-comprador WHERE b-comprador.cod_usuario = pedido-compr.responsavel NO-LOCK NO-ERROR.
        
                IF mla-doc-pend-aprov.ind-situacao = 2 OR
                   mla-doc-pend-aprov.ind-situacao = 4 THEN
        
                    FIND FIRST tt-aprov WHERE tt-aprov.seq = mla-doc-pend-aprov.seq-aprov NO-ERROR.
        
                    IF NOT AVAIL tt-aprov THEN
                    DO:
                        CREATE tt-aprov.
                        ASSIGN tt-aprov.seq        = mla-doc-pend-aprov.seq-aprov 
                               tt-aprov.data       = mla-doc-pend-aprov.dt-aprova
                               tt-aprov.aprovador  = IF mla-doc-pend-aprov.cod-usuar-altern <> "" and
                                                        AVAIL(b-aprov-alter) THEN b-aprov-alter.nom_usuario ELSE
                                                            (IF AVAIL b-aprov THEN b-aprov.nom_usuario ELSE "")
                               tt-aprov.transacao  = mla-doc-pend-aprov.nr-trans 
                               tt-aprov.num-pedido = pedido-compr.num-pedido.
                    END.
                    ELSE DO:
                             IF  tt-aprov.transacao <  mla-doc-pend-aprov.nr-trans  THEN DO:
                                 ASSIGN
                                         tt-aprov.seq        = mla-doc-pend-aprov.seq-aprov 
                                         tt-aprov.data       = mla-doc-pend-aprov.dt-aprova
                                         tt-aprov.aprovador  = IF mla-doc-pend-aprov.cod-usuar-altern <> "" and
                                                                AVAIL(b-aprov-alter) THEN b-aprov-alter.nom_usuario ELSE
                                                                    (IF AVAIL b-aprov THEN b-aprov.nom_usuario ELSE "")
                                         tt-aprov.transacao  =   mla-doc-pend-aprov.nr-trans
                                         tt-aprov.num-pedido = pedido-compr.num-pedido.
                    
                             END.
                    END.
                     
             END.
         END.

    END.
         FOR EACH tt-aprov BREAK BY aprovador
            BY transacao DESCENDING
            .
            IF FIRST-OF(aprovador) AND FIRST-OF(transacao) THEN
                tt-aprov.marca = YES.
        
        END.
    
END PROCEDURE.
/* fim do programa */

