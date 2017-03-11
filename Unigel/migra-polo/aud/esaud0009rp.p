/*****************************************************************************
**
**       Programa: esaud0009rp.p
**
**       Author...........: Amgra / Edson
**       Created..........: 13/08/2012    
**
**       Objetivo: Exporta‡Æo de Notas de Entradas para auditoria
**
**       OBS.....: 
**
*******************************************************************************/
define buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "esaud0009rp".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

 
DEFINE input param c-estab-ini AS CHARACTER  INITIAL "422" NO-UNDO.
DEFINE input param c-estab-fim AS CHARACTER  INITIAL "422"  NO-UNDO.
DEFINE input param i-cod-emitente-ini as integer initial "0" no-undo.  
DEFINE input param i-cod-emitente-fim as integer initial "9999999" no-undo.  
DEFINE input param dt-emis-nota-ini  AS DATE   INITIAL 05/01/2012    NO-UNDO.
DEFINE input param dt-emis-nota-fim AS DATE    INITIAL 05/10/2012   NO-UNDO.
def input param tg-imprime as logical no-undo.
 


/*
DEFINE var c-estab-ini AS CHARACTER  INITIAL "362" NO-UNDO.
DEFINE var c-estab-fim AS CHARACTER  INITIAL "362"  NO-UNDO.
DEFINE var i-cod-emitente-ini as integer initial "1155" no-undo.  
DEFINE var i-cod-emitente-fim as integer initial "1155" no-undo.  
DEFINE var dt-emis-nota-ini  AS DATE   INITIAL 12/01/2011    NO-UNDO.
DEFINE var dt-emis-nota-fim AS DATE    INITIAL 12/31/2011   NO-UNDO.
def var tg-imprime as logical no-undo.
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
DEFINE VARIABLE c-mot-apr AS CHARACTER  NO-UNDO. 
define variable c-dt-venc as char no-undo.
define variable c-ordem as char no-undo.
define variable c-pedido as char no-undo.


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




assign c-programa     = "esaud0009rp"
       c-versao       = "2.00"
       c-revisao      = "1.00.000"
       c-titulo-relat = "Exporta‡Æo de Notas de Entradas para auditoria"
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
   
ASSIGN c-arquivo_txt = session:TEMP-DIRECTORY + "esaud0009_notas_" + replace(STRING(TIME,"HH:MM:SS"),":","") + ".csv".

OUTPUT TO VALUE (c-arquivo_txt) NO-CONVERT.   
PUT      ";" 
    SKIP.
  PUT 
        "Exporta‡Æo de Notas de Entradas para auditoria" ";"
         SKIP.
PUT      ";" 
    SKIP.
PUT      ";" 
    SKIP.
PUT      ";" 
    SKIP.



    
PUT   UNFORMATTED
"Estab" ";"
"No da Nota Fiscal" ";"
"Serie" ";"
"C¢digo do Fornecedor" ";"
"Nome do Fornecedor" ";"
"CFOP" ";"
"Situa‡Æo" ";"
"Data da EmissÆo" ";"
"Data de Lan‡amento" ";"
"Data de Vencimento" ";"
"Data de Cancelamento" ";"
"Valor Total da Nota Fiscal" ";"
"No do Pedido de Compra" ";"
"No do Ordem de Compra" ";"
"No do Contrato" ";"
"C¢digo do Item" ";"
"Descri‡Æo do Item" ";"
"Quant. do Item" ";"



"Valor unit rio do Item" ";"
"Valor Total do Item" ";"
"Base de ICMS" ";"
"Cd.Tributa‡Æo ICMS" ";"
"Valor de ICMS" ";"
"Base de IPI" ";"
"Cd.Tributa‡Æo IPI" ";"
"Valor de IPI" ";"
"Base de ISS" ";"
"Valor de ISS"
SKIP.


 
   
      
  FOR EACH docum-est WHERE  
      docum-est.cod-estabel >= c-estab-ini  AND
      docum-est.cod-estabel <= c-estab-fim AND
      docum-est.esp-docto = 21
                     AND docum-est.tipo-docto = 1 
                     AND    docum-est.tipo-nota  = 1  
                     AND docum-est.dt-trans >= dt-emis-nota-ini  
                     AND docum-est.dt-trans <= dt-emis-nota-fim  
                     AND docum-est.estado = 1 
                     and docum-est.cod-emitente >= i-cod-emitente-ini 
                     and docum-est.cod-emitente <= i-cod-emitente-fim 
                     NO-LOCK,  
    EACH item-doc-est OF docum-est WHERE item-doc-est.sit-item = 1
                                     /*AND item-doc-est.nat-operacao >= nat-operacao-ini
                                     AND item-doc-est.nat-operacao <= nat-operacao-fim*/
                                     AND /*(IF tt-param.l-nota THEN*/ SUBSTRING(item-doc-est.nat-operacao,2,2) <> "99" /*ELSE TRUE )*/
                                     /*AND item-doc-est.it-codigo >= it-codigo-ini   
                                     AND item-doc-est.it-codigo <= it-codigo-fim */  NO-LOCK, 
    EACH item WHERE item.it-codigo = item-doc-est.it-codigo
                AND item.ge-codigo <> 81   NO-LOCK,
    EACH emitente OF docum-est  NO-LOCK,
    EACH familia WHERE familia.fm-codigo = item.fm-codigo NO-LOCK,
    EACH natur-oper WHERE natur-oper.nat-operacao = item-doc-est.nat-operacao AND natur-oper.emite-duplic :

      FIND estabelec WHERE estabelec.cod-estabel = docum-est.cod-estabel NO-LOCK NO-ERROR NO-WAIT.
      FIND empresa   WHERE empresa.ep-codigo     = estabelec.ep-codigo  NO-LOCK NO-ERROR NO-WAIT.
      FIND FIRST ordem-compra WHERE ordem-compra.numero-ordem = item-doc-est.numero-ordem NO-LOCK NO-ERROR.
      
    assign
          c-dt-venc = ""
          c-pedido = ""
          c-ordem = "".
          
      if avail ordem-compra then 
          assign c-ordem = string(ordem-compra.numero-ordem)
                 c-pedido = string(ordem-compra.num-pedido).
      else do:
         for each rat-ordem of item-doc-est no-lock.
            FIND FIRST ordem-compra WHERE ordem-compra.numero-ordem = rat-ordem.numero-ordem no-lock no-error.
             if avail ordem-compra and index(c-ordem , string(ordem-compra.numero-ordem)) = 0 then 
               assign c-ordem = c-ordem + string(ordem-compra.numero-ordem) + " - ".
 
               if avail ordem-compra and index(c-pedido , string(ordem-compra.num-pedido)) = 0 then 
               assign  c-pedido = c-pedido + string(ordem-compra.num-pedido) + " - ".
         

     
         
            
         end.
      
      
      
      end.               
      for each  dupli-apagar OF docum-est NO-LOCK .
         c-dt-venc = c-dt-venc + string(dupli-apagar.dt-vencim,"99/99/9999") + " - " .
      end.
      
      assign 
           c-dt-venc = trim(substring(c-dt-venc,1,length(trim(c-dt-venc)) - 1)).
      if index(c-pedido,"-") > 0 then      
           c-pedido = trim(substring(c-pedido,1,length(trim(c-pedido)) - 1)).
       if index(c-ordem,"-") > 0 then     
           c-ordem = trim(substring(c-ordem,1,length(trim(c-ordem)) - 1)).

      
       
       assign v-num-reg-lidos = v-num-reg-lidos + 1.
   if substring(string(v-num-reg-lidos,"99999999"),7,2) = "00" then 
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
/*  
   Estab	No da Nota Fiscal	serie
   	codigo	nome	cfop	situacao	
   	dt emiss	dt-lancamento	dt venc	dt canc
   		vl total
   		 nota	pedido	ordem
   		 	contrato
   		 		item	desc	qtde	
   		 		unitario	tota item	Base de ICMS	Cd.Tributa‡Æo ICMS	Valor de ICMS	Base de IPI	Cd.Tributa‡Æo IPI	Valor de IPI	Base de ISS	Valor de ISS
*/
     PUT UNFORMATTED
     docum-est.cod-estabel ";"
    docum-est.nro-docto 
";"  docum-est.serie-docto 
";" docum-est.cod-emitente
";" replace(emitente.nome-emit,";"," ")
";" item-doc-est.nat-operacao
";" "ativa"
";" docum-est.dt-emissao 
";" docum-est.dt-trans
";"  c-dt-venc
";" ""
";" docum-est.tot-valor
 ";" c-pedido 
";" c-ordem
";" IF AVAIL ordem-compra THEN ordem-compra.nr-contrato ELSE 0
";" item-doc-est.it-codigo
";" replace(replace((IF SUBSTRING(item.desc-item,1,1) = CHR(34) THEN SUBSTRING(item.desc-item,2,60) ELSE item.desc-item) + " - " + REPLACE(replace(replace(replace(item-doc-est.narrativa,CHR(10)," "),CHR(13)," "),CHR(9)," "),";",","),";",","),chr(10)," ") 
";" item-doc-est.quantidade
";" item-doc-est.preco-unit[1] 
";" item-doc-est.preco-total[1] + item-doc-est.valor-ipi[1]
";" item-doc-est.base-icm[1] + item-doc-est.icm-ntrib[1] + item-doc-est.icm-outras[1]
";" entry( item-doc-est.cd-trib-icm, {ininc/i07in122.i 03})
";" item-doc-est.valor-icm[1]
";" item-doc-est.base-ipi[1] + item-doc-est.ipi-outras[1] + item-doc-est.ipi-ntrib[1] 
";" entry( item-doc-est.cd-trib-ipi, {ininc/i10in172.i 03})
";" item-doc-est.valor-ipi[1]
";" item-doc-est.base-iss[1] 
";" item-doc-est.valor-iss[1] 
    SKIP.
         

   
  END.


    
    

 OUTPUT CLOSE.


c-arquivo_xls = session:TEMP-DIRECTORY + "esaud0009_notas_" + replace(STRING(TIME,"HH:MM:SS"),":","") + ".xlsx".

 

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

            c-relatorio:range("a6:ab6"):Interior:ColorIndex = 55.
            
 
            c-relatorio:range("a6:ab6"):Font:Name = "Arial".
             
            c-relatorio:range("a6:ab6"):Font:FontStyle = "Negrito".
            c-relatorio:range("a6:ab6"):Font:Size = 10.
            c-relatorio:range("a6:ab6"):Font:ColorIndex = 2.
            
            c-relatorio:Rows("6:6"):Autofilter (,,,).

      
            c-relatorio:Columns("h:k"):NumberFormat = "dd/mm/aaaa".
            
              c-relatorio:Columns("l:l"):NumberFormat = "#.##0,00".

             c-relatorio:Columns("r:s"):NumberFormat = "#.##0,0000".
             c-relatorio:Columns("t:ab"):NumberFormat = "#.##0,00".
             
          

            
            c-relatorio:Cells:Select.
            c-relatorio:Cells:EntireColumn:AutoFit.
            c-relatorio:Columns("A:A"):ColumnWidth = 14.
            c-relatorio:range("a2"):Select.

            c-relatorio:range("a1"):Select.



     c-planilha:SAVE().

END PROCEDURE.


PROCEDURE PI-APROV.

     l-aprov = NO.
     c-aprov = "".

 FOR EACH    doc-pend-aprov WHERE doc-pend-aprov.ind-tip-doc = 3  and
    doc-pend-aprov.ind-situacao = 2 and
     doc-pend-aprov.numero-ordem = ordem-compra.numero-ordem NO-LOCK.

       IF trim(doc-pend-aprov.cod-aprov-altern) = "" AND trim(doc-pend-aprov.cod-aprov) = "" THEN NEXT.
      c-aprov-ax = IF trim(doc-pend-aprov.cod-aprov-altern) <> "" THEN doc-pend-aprov.cod-aprov-altern ELSE doc-pend-aprov.cod-aprov.

     IF c-aprov = "" THEN 
         c-aprov = c-aprov-ax.
     ELSE DO:
         IF INDEX (c-aprov,c-aprov-ax) = 0 THEN
          c-aprov = c-aprov + "," + c-aprov-ax.
     END.
 END.

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
                      not mla-doc-pend-aprov.historico  and
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
                      not mla-doc-pend-aprov.historico and
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

