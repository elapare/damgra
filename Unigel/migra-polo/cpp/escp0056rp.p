/*****************************************************************************
**
**       Programa: escp0056rp.p
**
**       Data....: 03/04/2009   
**
**       Autor...: Amgra/Jos‚ Roberto
**
**       Objetivo: Pallets Produzidos no Per¡odo 
**
**       OBS.....: 
**
*******************************************************************************/
define buffer empresa for mgmulti.empresa.

define variable c-prog-gerado as character no-undo initial "escp0056rp".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

/****************** Defini‡Æo de Tabelas Tempor rias do Relat¢rio **********************/

def buffer bf-ped-venda for ped-venda.

def temp-table tt-raw-digita
    field raw-digita as raw.


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
    field c-cod-estabel        AS CHAR 
    field c-fm-codigo-ini      AS CHAR 
    field c-fm-codigo-fim      AS CHAR  
    field i-ge-codigo-ini      AS INTEGER  
    field i-ge-codigo-fim      AS INTEGER   
    field dt-trans-ini         AS DATE 
    field dt-trans-fim         AS DATE 
    field c-it-codigo-ini      AS CHAR 
    field c-it-codigo-fim      AS CHAR 
.


DEFINE TEMP-TABLE tt-pallet
    FIELD nr-pallet            AS CHAR 
    FIELD it-codigo            AS CHAR 
    FIELD peso-liquido         AS DEC
    FIELD cod-estabel          AS CHAR
    FIELD peso-bruto           AS DEC
    FIELD dt-trans             AS DATE 
    FIELD nr-linha             AS INT
    FIELD tipo-prod            AS CHAR
    FIELD nome-abrev           AS CHAR
    FIELD mercado              AS CHAR
    FIELD largura              AS INT
    FIELD diin                 AS INT
    FIELD diex                 AS INT 
    FIELD nr-nota-fis          AS CHAR 
    field tipo-nf              as char 
    INDEX ch-tt-pallet IS PRIMARY UNIQUE  it-codigo
                                          nr-pallet. 



DEFINE TEMP-TABLE tt-resumo-1
    FIELD tipo                 AS CHAR 
    FIELD peso-liquido         AS DEC
    INDEX ch-tt-resumo-1 IS PRIMARY UNIQUE tipo.


DEFINE TEMP-TABLE tt-resumo-2
    FIELD tipo                 AS CHAR 
    FIELD peso-liquido         AS DEC
    INDEX ch-tt-resumo-2 IS PRIMARY UNIQUE  tipo.


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


def new shared var c-cod-estabel        AS CHAR     format "x(3)"                                  no-undo. /*solic-318*/ 
def new shared var c-fm-codigo-ini      AS CHAR     format "x(8)"       initial ""                 no-undo.
def new shared var c-fm-codigo-fim      AS CHAR     format "x(8)"       initial "ZZZZZZZZ"         no-undo.
def new shared var i-ge-codigo-ini      AS INTEGER  format ">>9"        initial 0                  no-undo.
def new shared var i-ge-codigo-fim      AS INTEGER  format ">>9"        initial 999                no-undo.
def new shared var dt-trans-ini         AS DATE     format "99/99/9999" initial TODAY              no-undo.
def new shared var dt-trans-fim         AS DATE     format "99/99/9999" initial TODAY              no-undo.
def new shared var c-it-codigo-ini      AS CHAR     format "x(16)"      initial ""                 no-undo.
def new shared var c-it-codigo-fim      AS CHAR     format "x(16)"      initial "ZZZZZZZZZZZZZZZZ" no-undo.


/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

DEFINE VARIABLE esp-jr         AS INTEGER    NO-UNDO.
DEFINE VARIABLE larg-jr        AS INTEGER    NO-UNDO.
DEFINE VARIABLE larg-jrx       AS char       NO-UNDO.
DEFINE VARIABLE diex-jrx       AS char       NO-UNDO.
DEFINE VARIABLE diex-jr        AS INTEGER    NO-UNDO.
DEFINE VARIABLE diin-jr        AS INTEGER    NO-UNDO.
DEFINE VARIABLE linha-jr       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE qtd-bob-jr     AS INTEGER    NO-UNDO.
DEFINE VARIABLE tem-pallet-jr  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE tipo-prod-jr   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE tipo-prod-1    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE total-jr-1     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE total-jr-2     AS DECIMAL    NO-UNDO.

/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 


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

DEFINE VARIABLE ext-jr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-obs-pallet AS CHARACTER  NO-UNDO.

DEF VAR c-arq             AS CHAR                NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR                NO-UNDO.   
DEF VAR l-cria            AS LOG                 NO-UNDO.
DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
DEF VAR i-linha           AS INT                 NO-UNDO.
DEF VAR c-coluna          AS char                NO-UNDO.

/* para executar o Excel */

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.




/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form 
     with down width 184 no-box stream-io frame f-relat-09-132.

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

assign c-programa     = "escp0056rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Relat¢rio de Pedidos a Embarcar"
       c-sistema      = "PDP".

if  tt-param.formato = 2 then do:


form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    fill("-", 112) format "x(110)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabec.

form header
    fill("-", 132) format "x(132)" 
    with stream-io width 132 no-labels no-box frame f-linha.

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

    assign c-cod-estabel      = tt-param.c-cod-estabel     
           c-fm-codigo-ini    = tt-param.c-fm-codigo-ini   
           c-fm-codigo-fim    = tt-param.c-fm-codigo-fim   
           i-ge-codigo-ini    = tt-param.i-ge-codigo-ini   
           i-ge-codigo-fim    = tt-param.i-ge-codigo-fim   
           dt-trans-ini       = tt-param.dt-trans-ini      
           dt-trans-fim       = tt-param.dt-trans-fim      
           c-it-codigo-ini    = tt-param.c-it-codigo-ini   
           c-it-codigo-fim    = tt-param.c-it-codigo-fim   

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
            assign v-cod-destino-impres = "Excel".


run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").


IF tt-param.destino = 4 THEN DO:
    
    /* Cria Aplica‡Æo do Excel */

    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

    ASSIGN c-modelo-planilha = search("modelos/mod-escp0056.xls") 
           c-arq             = SESSION:TEMP-DIRECTORY.

    RUN pi-cria-planilha.


END.

IF tt-param.destino = 4 THEN DO:
       
       ASSIGN c-relatorio:range("C" + STRING(2)):VALUE = tt-param.dt-trans-ini        
              c-relatorio:range("E" + STRING(2)):VALUE = tt-param.dt-trans-fim.        

END.

assign v-num-reg-lidos = 0.


ASSIGN i-linha = 4. 


FOR EACH tt-pallet:
    DELETE tt-pallet.
END.

ASSIGN esp-jr = 33.

DO WHILE esp-jr = 33 :
  
  for each movto-estoq no-lock
         where movto-estoq.esp-docto    = esp-jr          AND
               movto-estoq.cod-estabel  = c-cod-estabel   and 
               movto-estoq.dt-trans    >= dt-trans-ini    and 
               movto-estoq.dt-trans    <= dt-trans-fim    and 
               movto-estoq.it-codigo   >= c-it-codigo-ini AND
               movto-estoq.it-codigo   <= c-it-codigo-fim AND
               movto-estoq.tipo-trans = 1                 AND
               movto-estoq.lote        <> "recicl"        AND 
               movto-estoq.cod-depos   <> "ARC"           AND
               movto-estoq.quantidade  <> 0
               USE-INDEX esp-data,

      EACH pallet WHERE
          pallet.it-codigo = movto-estoq.it-codigo AND
          pallet.nr-pallet = movto-estoq.lote      AND
          pallet.situacao = 2
          NO-LOCK. 
    
      FIND FIRST ITEM WHERE ITEM.it-codigo = movto-estoq.it-codigo
           USE-INDEX codigo NO-LOCK NO-ERROR.
     
      IF NOT AVAIL ITEM THEN NEXT.
     
      IF ITEM.ge-codigo < i-ge-codigo-ini OR item.ge-codigo > i-ge-codigo-fim or
         ITEM.fm-codigo < c-fm-codigo-ini OR item.fm-codigo > c-fm-codigo-fim   
         THEN NEXT.

      FIND FIRST tt-pallet WHERE
          tt-pallet.it-codigo = pallet.it-codigo AND
          tt-pallet.nr-pallet = pallet.nr-pallet
          NO-ERROR.

      IF AVAIL tt-pallet THEN NEXT.
     
      FIND FIRST it-pallet OF pallet NO-LOCK.
     
      IF NOT AVAIL it-pallet THEN NEXT.
     
      FIND LAST movto-mat WHERE
          movto-mat.it-codigo = it-pallet.it-codigo     AND
          movto-mat.lote      = it-pallet.lote-bobina   AND
          movto-mat.dt-trans >= dt-trans-ini            and 
          movto-mat.dt-trans <= dt-trans-fim            and
          movto-mat.esp-docto = 1
          USE-INDEX lote 
          NO-LOCK NO-ERROR.
     
      IF NOT AVAIL movto-mat THEN NEXT.
     
         
      FIND FIRST ord-prod WHERE
         ord-prod.nr-ord-produ = movto-mat.nr-ord-produ AND
         ord-prod.it-codigo    = movto-mat.it-codigo    AND
         ord-prod.cod-estabel  = movto-mat.cod-estabel    
         USE-INDEX estabel NO-LOCK NO-ERROR.
     
      IF NOT AVAIL ord-prod THEN NEXT.
     
      FIND FIRST lote-carac-tec WHERE
                          lote-carac-tec.it-codigo   = movto-mat.it-codigo
                          and lote-carac-tec.lote    = movto-mat.lote
                          and lote-carac-tec.cd-comp = "largura"
                          NO-LOCK NO-ERROR.
     
      if avail lote-carac-tec then
         ASSIGN larg-jr = lote-carac-tec.vl-resul.
      ELSE
         ASSIGN larg-jr = 0.
     
      FIND FIRST lote-carac-tec WHERE
                          lote-carac-tec.it-codigo   = movto-mat.it-codigo
                          and lote-carac-tec.lote    = movto-mat.lote
                          and lote-carac-tec.cd-comp = "diin"
                          NO-LOCK NO-ERROR.
     
      if avail lote-carac-tec then
         ASSIGN diin-jr = lote-carac-tec.vl-resul.
      ELSE
         ASSIGN diin-jr = 0.
     
      FIND FIRST lote-carac-tec WHERE
                          lote-carac-tec.it-codigo   = movto-estoq.it-codigo
                          and lote-carac-tec.lote    = movto-estoq.lote
                          and lote-carac-tec.cd-comp = "diex"
                          NO-LOCK NO-ERROR.
     
      if avail lote-carac-tec then
         ASSIGN diex-jr = lote-carac-tec.vl-resul.
      ELSE
         ASSIGN diex-jr = 0.
      
      CREATE tt-pallet.

      ASSIGN tt-pallet.it-codigo = pallet.it-codigo
             tt-pallet.nr-pallet = pallet.nr-pallet.

      ASSIGN tt-pallet.cod-estabel  = movto-estoq.cod-estabel
             tt-pallet.peso-liquido = pallet.peso-liquido
             tt-pallet.peso-bruto   = pallet.peso-bruto  
             tt-pallet.dt-trans     = movto-estoq.dt-trans
             tt-pallet.nr-linha     = ord-prod.nr-linha
             tt-pallet.larg         = larg-jr                
             tt-pallet.diin         = diin-jr                
             tt-pallet.diex         = diex-jr.   

      ASSIGN tipo-prod-jr = "".
   
 
      CASE SUBSTRING(pallet.nr-pallet,1,1):
           WHEN "X" THEN tipo-prod-jr = "ESTOQUE".           
           WHEN "A" THEN tipo-prod-jr = "AMOSTRA".           
           WHEN "P" THEN tipo-prod-jr = "VENDA".             
           WHEN "D" THEN tipo-prod-jr = "DOA€ÇO".            
           WHEN "E" THEN tipo-prod-jr = "ESTOQ.ESTRAT.". 
           WHEN "R" THEN tipo-prod-jr = "REPOSI€ÇO".         
           WHEN "Q" THEN tipo-prod-jr = "OFF-SPECS".         
           WHEN "Z" THEN tipo-prod-jr = "TRANSFERENCIA".  
      end.   
             
      IF SUBSTRING(pallet.nr-pallet,1,1) <> "X" AND
         SUBSTRING(pallet.nr-pallet,1,1) <> "A" AND
         SUBSTRING(pallet.nr-pallet,1,1) <> "P" AND
         SUBSTRING(pallet.nr-pallet,1,1) <> "D" AND
         SUBSTRING(pallet.nr-pallet,1,1) <> "E" AND
         SUBSTRING(pallet.nr-pallet,1,1) <> "R" AND
         SUBSTRING(pallet.nr-pallet,1,1) <> "Q" AND
         SUBSTRING(pallet.nr-pallet,1,1) <> "Z" THEN 
          ASSIGN tipo-prod-jr = "OUTROS".
  

      ASSIGN tt-pallet.tipo-prod = tipo-prod-jr.


      IF pallet.nr-pedido <> 0 THEN DO:

         FIND FIRST ped-venda WHERE
              ped-venda.nr-pedido = pallet.nr-pedido
              NO-LOCK NO-ERROR.

         IF AVAIL ped-venda THEN DO:
             ASSIGN tt-pallet.nome-abrev = ped-venda.nome-abrev.

             IF substring(ped-venda.nat-operacao,1,1) < "7"  THEN
                 ASSIGN tt-pallet.mercado = "INT".
             ELSE
                 ASSIGN tt-pallet.mercado = "EXT".

         END.

      END.     
 
             
      FIND LAST fat-ser-lote WHERE
           fat-ser-lote.it-codigo       = pallet.it-codigo AND
           fat-ser-lote.log-disp-planej = NO               AND
           fat-ser-lote.nr-serlote      = pallet.nr-pallet
           USE-INDEX ITEM
           NO-LOCK NO-ERROR.

      IF AVAIL fat-ser-lote THEN do:
          ASSIGN tt-pallet.nr-nota-fis = fat-ser-lote.nr-nota-fis.
          
          find first it-nota-fisc of fat-ser-lote
               no-lock no-error.
               
               if avail it-nota-fisc and substring(it-nota-fisc.nat-operacao,1,4) = "5905" then
                  assign tt-pallet.tipo-nf = "Transf".
                  else
                  assign tt-pallet.tipo-nf = "Faturamento".              
               
               
      end.    

     
      assign v-num-reg-lidos = v-num-reg-lidos + 1.
      run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

      ASSIGN tipo-prod-1 = tipo-prod-jr.

      IF tipo-prod-1 = "VENDA" THEN DO:

          IF tt-pallet.mercado = "EXT" THEN
              ASSIGN tipo-prod-1 = "VENDA-ME".
              ELSE
                  ASSIGN tipo-prod-1 = "VENDA-MI".

      END.

      IF tipo-prod-1 = "ESTOQUE" THEN DO:

          IF ITEM.fm-codigo = "46-17" OR
              ITEM.fm-codigo = "46-15" THEN 
              ASSIGN tipo-prod-1 = "ESTOQ.CIGARREIRA".
          ELSE 
              IF ITEM.fm-cod-com = "46-10-40" THEN
                  ASSIGN tipo-prod-1 = "ESTOQ.METAL".

      END.

      FIND FIRST tt-resumo-1 WHERE
          tt-resumo-1.tipo = tipo-prod-1
          NO-ERROR.

      IF NOT AVAIL tt-resumo-1 THEN DO:
          CREATE tt-resumo-1.
          ASSIGN tt-resumo-1.tipo = tipo-prod-1.
      END.

      ASSIGN tt-resumo-1.peso-liquido = tt-resumo-1.peso-liquido + tt-pallet.peso-liquido
             total-jr-1 = total-jr-1 + tt-pallet.peso-liquido.

      IF tipo-prod-jr = "VENDA" THEN DO:

         FIND FIRST tt-resumo-1 WHERE
             tt-resumo-1.tipo = "VENDA-TOTAL"
             NO-ERROR.
        
         IF NOT AVAIL tt-resumo-1 THEN DO:
             CREATE tt-resumo-1.
             ASSIGN tt-resumo-1.tipo = "VENDA-TOTAL".
         END.
        
         ASSIGN tt-resumo-1.peso-liquido = tt-resumo-1.peso-liquido + tt-pallet.peso-liquido.

      END.



      IF tt-pallet.nr-nota-fis <> "" and tt-pallet.tipo-nf = "Faturamento" THEN DO:

          ASSIGN tipo-prod-1 = tipo-prod-jr.
    
          IF tipo-prod-1 = "VENDA" THEN DO:
    
              IF tt-pallet.mercado = "EXT" THEN
                  ASSIGN tipo-prod-1 = "VENDA-ME".
                  ELSE
                      ASSIGN tipo-prod-1 = "VENDA-MI".
    
          END.
    
          IF tipo-prod-1 = "ESTOQUE" THEN DO:
    
              IF ITEM.fm-codigo = "46-17" OR
                  ITEM.fm-codigo = "46-15" THEN 
                  ASSIGN tipo-prod-1 = "ESTOQ.CIGARREIRA".
              ELSE 
                  IF ITEM.fm-cod-com = "46-10-40" THEN
                      ASSIGN tipo-prod-1 = "ESTOQ.METAL".
    
          END.
    
          FIND FIRST tt-resumo-2 WHERE
              tt-resumo-2.tipo = tipo-prod-1
              NO-ERROR.
    
          IF NOT AVAIL tt-resumo-2 THEN DO:
              CREATE tt-resumo-2.
              ASSIGN tt-resumo-2.tipo = tipo-prod-1.
          END.
    
          ASSIGN tt-resumo-2.peso-liquido = tt-resumo-2.peso-liquido + tt-pallet.peso-liquido
                 total-jr-2 = total-jr-2 + tt-pallet.peso-liquido.
    
          IF tipo-prod-jr = "VENDA" THEN DO:
    
             FIND FIRST tt-resumo-2 WHERE
                 tt-resumo-2.tipo = "VENDA-TOTAL"
                 NO-ERROR.
            
             IF NOT AVAIL tt-resumo-2 THEN DO:
                 CREATE tt-resumo-2.
                 ASSIGN tt-resumo-2.tipo = "VENDA-TOTAL".
             END.
            
             ASSIGN tt-resumo-2.peso-liquido = tt-resumo-2.peso-liquido + tt-pallet.peso-liquido.
    
          END.

      END.

  END.

  ASSIGN esp-jr = 0.

END.

ASSIGN v-num-reg-lidos = 0.

for each tt-pallet no-lock.

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input "Gerando Planilha: " + string(v-num-reg-lidos)).

    IF tt-param.destino = 4 THEN DO:

       ASSIGN i-linha = i-linha + 1.
       
       assign larg-jrx = string(tt-pallet.larg) + "mm"
              diex-jrx = string(tt-pallet.diex) + "mm".

       ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-pallet.cod-estabel
              c-relatorio:range("B" + STRING(i-linha)):VALUE = tt-pallet.it-codigo      
              c-relatorio:range("C" + STRING(i-linha)):VALUE = tt-pallet.dt-trans        
              c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-pallet.nr-linha        
              c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-pallet.nr-pallet
              c-relatorio:range("F" + STRING(i-linha)):VALUE = tt-pallet.peso-bruto
              c-relatorio:range("G" + STRING(i-linha)):VALUE = tt-pallet.peso-liquido
              c-relatorio:range("H" + STRING(i-linha)):VALUE = tt-pallet.tipo-prod
              c-relatorio:range("I" + STRING(i-linha)):VALUE = tt-pallet.nome-abrev
              c-relatorio:range("J" + STRING(i-linha)):VALUE = tt-pallet.mercado
              c-relatorio:range("K" + STRING(i-linha)):VALUE = larg-jrx        
              c-relatorio:range("L" + STRING(i-linha)):VALUE = diex-jrx  
              c-relatorio:range("M" + STRING(i-linha)):VALUE = tt-pallet.diin  
              c-relatorio:range("N" + STRING(i-linha)):VALUE = tt-pallet.nr-nota-fis  
              c-relatorio:range("O" + STRING(i-linha)):VALUE = tt-pallet.tipo-nf.  



    END. 

END.

/* resumo pallets */

ASSIGN i-linha = i-linha + 3
       c-relatorio:range("H" + STRING(i-linha)):VALUE = "RESUMO DO PRODUZIDO" 
       i-linha = i-linha + 1.

       ASSIGN c-relatorio:range("H" + STRING(i-linha)):VALUE = "__________________________________________".

FOR EACH tt-resumo-1 NO-LOCK.

    ASSIGN i-linha = i-linha + 1
           c-relatorio:range("H" + STRING(i-linha)):VALUE = tt-resumo-1.tipo
           c-relatorio:range("I" + STRING(i-linha)):VALUE = (tt-resumo-1.peso-liquido / 1000)
           c-relatorio:range("J" + STRING(i-linha)):VALUE = (tt-resumo-1.peso-liquido / total-jr-1).
           
END.

ASSIGN i-linha = i-linha + 1
       c-relatorio:range("H" + STRING(i-linha)):VALUE = "__________________________________________".

ASSIGN i-linha = i-linha + 1
       c-relatorio:range("H" + STRING(i-linha)):VALUE = "TOTAL ==>"
       c-relatorio:range("I" + STRING(i-linha)):VALUE = (total-jr-1 / 1000).


/* resumo pallets embarcados */

ASSIGN i-linha = i-linha + 3
       c-relatorio:range("H" + STRING(i-linha)):VALUE = "RESUMO DOS EMBARCADOS NO PERÖODO" 
       c-relatorio:range("K" + STRING(i-linha - 1)):VALUE = "%EMBQ." 
       c-relatorio:range("K" + STRING(i-linha)):VALUE = "s/PROD." 
       c-relatorio:range("L" + STRING(i-linha - 1)):VALUE = "SALDO" 
       c-relatorio:range("L" + STRING(i-linha)):VALUE = "PRD - EMBQ " 
       i-linha = i-linha + 1.

       ASSIGN c-relatorio:range("H" + STRING(i-linha)):VALUE = "__________________________________________".

FOR EACH tt-resumo-2 NO-LOCK.

    find first tt-resumo-1 where
         tt-resumo-1.tipo = tt-resumo-2.tipo
         no-lock no-error.         
         

    ASSIGN i-linha = i-linha + 1
           c-relatorio:range("H" + STRING(i-linha)):VALUE = tt-resumo-2.tipo
           c-relatorio:range("I" + STRING(i-linha)):VALUE = (tt-resumo-2.peso-liquido / 1000)
           c-relatorio:range("J" + STRING(i-linha)):VALUE = (tt-resumo-2.peso-liquido / total-jr-2).

    assign c-relatorio:range("K" + STRING(i-linha)):VALUE = if avail tt-resumo-1 then ((tt-resumo-2.peso-liquido / 1000) / (tt-resumo-1.peso-liquido / 1000)) ELSE 0 .

    assign c-relatorio:range("L" + STRING(i-linha)):VALUE = if avail tt-resumo-1 then ((tt-resumo-1.peso-liquido / 1000) - (tt-resumo-2.peso-liquido / 1000)) ELSE 0 .
          
END.

ASSIGN i-linha = i-linha + 1
       c-relatorio:range("H" + STRING(i-linha)):VALUE = "__________________________________________".

ASSIGN i-linha = i-linha + 1
       c-relatorio:range("H" + STRING(i-linha)):VALUE = "TOTAL ==>"
       c-relatorio:range("I" + STRING(i-linha)):VALUE = (total-jr-2 / 1000)
       c-relatorio:range("K" + STRING(i-linha)):VALUE = (total-jr-2 / 1000) / (total-jr-1 / 1000) .



run pi-finalizar in h-acomp.

if  tt-param.destino <> 4 then DO:

    if  tt-param.destino <> 1 then
    
        page stream str-rp.
    
    else do:
    
        if   tt-param.parametro = yes then
    
             page stream str-rp.
    
    end.
    
    if  tt-param.parametro then do:
    
    
       disp stream str-rp "SELE€ÇO" skip(01) with stream-io frame f-imp-sel.
       disp stream str-rp 
          c-cod-estabel colon 20 "|< >|"   at 44 c-cod-estabel no-label
            with stream-io side-labels overlay row 028 frame f-imp-sel.
    
       disp stream str-rp "CLASSIFICA€ÇO" skip(01) with stream-io frame f-imp-cla.
       disp stream str-rp "   PDP"
            with stream-io side-labels overlay row 028 frame f-imp-cla.
    
       put stream str-rp unformatted skip(1) "IMPRESSÇO" skip(1).
    
       put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
       put stream str-rp unformatted skip "    " "Execu‡Æo: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
       put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
       put stream str-rp unformatted skip "    " "Usu rio : " tt-param.usuario.
    
    end.
    
    else
        output stream str-rp close.
END.


IF tt-param.destino = 4 THEN DO:

   RUN pi-finaliza-impressao.

   RETURN 'OK'.

END.

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


PROCEDURE pi-cria-planilha:

DEF VAR c-arquivo AS CHAR NO-UNDO.

    c-arquivo = c-arq + 'escp0056' + STRING(time)+ '.xls'.

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

return 'OK'.

/* fim do programa */
      
