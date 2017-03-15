
/*------------------------------------------------------------------------
File.............: esft0025rp.p
Description......: Lista de Embarque por N.Fiscal
Input Parameters : 
Output Parameters: 
Author...........: Amgra - Jos‚ Roberto
Created..........: 17/06/2008
OBS..............: 
------------------------------------------------------------------------*/
def buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "esft0025RP".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

/****************** Defini‡Æo de Tabelas Tempor rias do Relat¢rio **********************/

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
    field c-cod-estabel        like nota-fiscal.cod-estabel
    field dt-emis-nota-ini     like nota-fiscal.dt-emis-nota
    field dt-emis-nota-fim     like nota-fiscal.dt-emis-nota
    FIELD c-serie              AS CHAR
    field c-nr-nota-fis-ini    like nota-fiscal.nr-nota-fis
    field c-nr-nota-fis-fim    like nota-fiscal.nr-nota-fis
    field i-cod-emitente-ini   like nota-fiscal.cod-emitente
    field i-cod-emitente-fim   like nota-fiscal.cod-emitente.


DEFINE TEMP-TABLE tt-nota-fiscal no-undo
    FIELD nr-nota-fis          AS CHAR FORMAT "x(10)"   LABEL "N.Fiscal"
    FIELD nr-seq-fat           AS INT  FORMAT "zzzzzz9" LABEL "Seq-NF"
    FIELD nr-pallet            AS CHAR FORMAT "x(15)"   label "Nr.Palete"
    FIELD largura              AS int  FORMAT "zzz9"    LABEL "Larg"
    FIELD diin                 AS int  FORMAT "zzz9"    LABEL "Diin"
    FIELD diex                 AS int  FORMAT "zzz9"    LABEL "Diex"
    FIELD it-codigo            LIKE movto-estoq.it-codigo LABEL "Item"
    FIELD cod-refer            LIKE it-nota-fisc.cod-refer LABEL "Refer."
    FIELD qtd-plt              AS INTEGER FORMAT "zz9"  LABEL "Qt.Plt"
    FIELD nr-bobinas           LIKE pallet.nr-bobinas   LABEL "Qt.Bobs"
    FIELD lg-plt               AS DECIMAL FORMAT "z9.999" LABEL "P-Larg"
    FIELD cm-plt               AS DECIMAL FORMAT "z9.999" LABEL "P-Comp"
    FIELD al-plt               AS DECIMAL FORMAT "z9.999" LABEL "P-Alt "
    FIELD volume               AS DECIMAL FORMAT "z9.9999" LABEL "Volume"
    FIELD peso-liq             AS DECIMAL LABEL "Peso Liquido" FORMAT "zzzzzzzz9.99"
    FIELD peso-bru             AS DECIMAL LABEL "Peso Bruto" FORMAT "zzzzzzzz9.99"
    FIELD nome-abrev           AS CHAR    
    FIELD nr-pedido            AS INT 
    FIELD nr-sequencia         AS INT
    INDEX chave IS PRIMARY UNIQUE nr-nota-fis
                                  nr-seq-fat
                                  nr-pallet.


DEFINE TEMP-TABLE tt-cliente no-undo
    FIELD nr-nota-fis          AS CHAR FORMAT "x(10)"    LABEL "N.Fiscal"
    FIELD cod-emitente         AS INT  FORMAT ">>>>>>>9" LABEL "Cliente"
    INDEX chave IS PRIMARY UNIQUE cod-emitente
                                  nr-nota-fis .

/*Recebimento de Parametros*/

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FOR EACH tt-raw-digita:
   CREATE tt-nota-fiscal.
   RAW-TRANSFER tt-raw-digita.raw-digita TO tt-nota-fiscal.
END.
def new shared var v_han_acomp as handle no-undo. /* deve ser criada */

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

def new shared var c-cod-estabel        like nota-fiscal.cod-estabel    FORMAT "x(3)"             NO-UNDO.  /*solic-318*/ 
def new shared var dt-emis-nota-ini     like nota-fiscal.dt-emis-nota   FORMAT "99/99/9999" INITIAL today      NO-UNDO. 
def new shared var dt-emis-nota-fim     like nota-fiscal.dt-emis-nota   FORMAT "99/99/9999" INITIAL today      NO-UNDO. 
def new shared var c-nr-nota-fis-ini    like nota-fiscal.nr-nota-fis    FORMAT "x(10)"      INITIAL ""         NO-UNDO. 
def new shared var c-nr-nota-fis-fim    like nota-fiscal.nr-nota-fis    FORMAT "x(10)"      INITIAL "ZZZZZZZZ" NO-UNDO.
def new shared var i-cod-emitente-ini   like nota-fiscal.cod-emitente   FORMAT ">>>>>>>9"   INITIAL 0          NO-UNDO.
def new shared var i-cod-emitente-fim   like nota-fiscal.cod-emitente   FORMAT ">>>>>>>9"   INITIAL 99999999   NO-UNDO.



DEFINE VARIABLE sequencia-plt      AS INTEGER               NO-UNDO.
DEFINE VARIABLE linhas-jr          AS INTEGER               NO-UNDO.
DEFINE VARIABLE soma-bob           AS INTEGER               NO-UNDO.
DEFINE VARIABLE soma-plt           AS INTEGER               NO-UNDO.
DEFINE VARIABLE soma-vol           AS DECIMAL               NO-UNDO.
DEFINE VARIABLE soma-pliquido      AS DECIMAL               NO-UNDO.
DEFINE VARIABLE soma-pbruto        AS DECIMAL               NO-UNDO.
DEFINE VARIABLE largura-jr         AS INTEGER               NO-UNDO.
DEFINE VARIABLE diex-jr            AS INTEGER               NO-UNDO.
DEFINE VARIABLE diin-jr            AS INTEGER               NO-UNDO.
DEFINE VARIABLE zint-jr            AS INTEGER               NO-UNDO.
DEFINE VARIABLE it-codigo-jr       AS CHARACTER             NO-UNDO.
DEFINE VARIABLE pedcli-jr          AS CHARACTER             NO-UNDO.
DEFINE VARIABLE c-nr-order         AS CHARACTER  FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE data-jr            AS DATE                  NO-UNDO.
DEFINE VARIABLE nome-abrev-jr      LIKE ped-venda.nome-abrev.
DEFINE VARIABLE soma-bob-pd        AS INTEGER               NO-UNDO.
DEFINE VARIABLE soma-plt-pd        AS INTEGER               NO-UNDO.
DEFINE VARIABLE soma-qt-palete     AS INTEGER               NO-UNDO.
DEFINE VARIABLE soma-vol-pd        AS DECIMAL               NO-UNDO.
DEFINE VARIABLE soma-pliquido-pd   AS DECIMAL               NO-UNDO.
DEFINE VARIABLE soma-pbruto-pd     AS DECIMAL               NO-UNDO.
DEFINE VARIABLE nr-seq-fat         AS INTEGER               NO-UNDO.
DEFINE VARIABLE nr-seq-fat-ant     AS INTEGER               NO-UNDO.
DEFINE VARIABLE lin-jr             AS INTEGER INITIAL 0     NO-UNDO.
DEFINE VARIABLE c-arquivo          AS CHAR   FORMAT "x(50)" NO-UNDO.


DEFINE VARIABLE larg-jr    AS INTEGER    NO-UNDO.

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 


DEFINE VARIABLE c-lg-plt-1 AS DECIMAL FORMAT "9.999" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-cm-plt-1 AS DECIMAL FORMAT "9.999" 
     VIEW-AS FILL-IN   
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-al-plt-1 AS DECIMAL FORMAT "9.999" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-comercial-invoice AS INTEGER FORMAT "9999999":U 
     LABEL "Nota Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 TOOLTIP "Informe Nr.Nota Fiscal " INITIAL 0 NO-UNDO.

DEFINE VARIABLE c-carreta AS CHAR FORMAT "x(10)":U 
     LABEL "Placa" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 TOOLTIP "Informe a Placa da Carreta " NO-UNDO.

DEFINE VARIABLE c-container AS CHAR FORMAT "x(15)":U 
     LABEL "Transp." 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 TOOLTIP "Informe o Transportador" NO-UNDO.

DEFINE VARIABLE c-obs1 AS CHAR FORMAT "x(30)":U 
     LABEL "Observ-1" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .88 TOOLTIP "Informe a Observa‡Æo 1 " NO-UNDO.

DEFINE VARIABLE c-obs2 AS CHAR FORMAT "x(50)":U 
     LABEL "Observ-2" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .88 TOOLTIP "Informe a Observa‡Æo 2 " NO-UNDO.





/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/* Vari veis usadas para gerar planilha excel. */

DEF VAR c-arq             AS CHAR  FORMAT "x(50)"  NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR  FORMAT "x(50)"  NO-UNDO.   
DEF VAR l-cria            AS LOG                 NO-UNDO.
DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
DEF VAR i-linha           AS INT                 NO-UNDO.

/* para executar o Excel */

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.


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

form
    with down width 132 no-box stream-io frame f-relat-09-132.


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

assign c-programa     = "esft0025rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Lista de Embarque por Nota Fiscal)"
       c-sistema      = "".

if  tt-param.formato = 1 then do:


form header
    fill("-", 80) format "x(80)" skip
    c-empresa format "x(24)" c-titulo-relat at 30 format "x(30)"
    "Folha:" at 70 page-number(str-rp) at 76 format ">>>>9" skip
    fill("-", 60) format "x(58)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 80 no-labels no-box page-top frame f-cabec-80.
        
form header
    fill("-", 80) format "x(80)" skip
    c-empresa format "x(24)" c-titulo-relat at 30 format "x(30)"
    "Folha:" at 70 page-number(str-rp) at 76 format ">>>>9" skip
    "Per¡odo:" i-numper-x at 08 "-"
    da-iniper-x at 14 "to" da-fimper-x
    fill("-", 22) format "x(20)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 80 no-labels no-box page-top frame f-cabper-80.

run grapi/gr2005.p.

form header
    c-rodape format "x(80)"
    with stream-io width 80 no-labels no-box page-bottom frame f-rodape-80.

end. /* tt-param.formato = 1 */ 

if  tt-param.formato = 2 then do:


form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    fill("-", 112) format "x(110)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabec.

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

    assign c-cod-estabel = tt-param.c-cod-estabel
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
           ASSIGN v-cod-destino-impres = "excel".


FOR EACH tt-nota-fiscal.
    DELETE tt-nota-fiscal.
END.

FOR EACH tt-cliente.
    DELETE tt-cliente.
END.


run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp (input "Lendo Notas Fiscais ...Aguarde"). 

for EACH nota-fiscal WHERE
    nota-fiscal.dt-emis-nota >= tt-param.dt-emis-nota-ini    AND
    nota-fiscal.dt-emis-nota <= tt-param.dt-emis-nota-fim    AND
    nota-fiscal.cod-estabel   = tt-param.c-cod-estabel       and 
    nota-fiscal.serie         = tt-param.c-serie             AND
    nota-fiscal.nr-nota-fis  >= tt-param.c-nr-nota-fis-ini   AND
    nota-fiscal.nr-nota-fis  <= tt-param.c-nr-nota-fis-fim   AND
    nota-fiscal.cod-emitente >= tt-param.i-cod-emitente-ini  AND
    nota-fiscal.cod-emitente <= tt-param.i-cod-emitente-fim  
    USE-INDEX ch-sit-nota
    NO-LOCK.

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    ASSIGN larg-jr = 0
           diin-jr = 0
           diex-jr = 0.  

    FOR EACH it-nota-fisc OF nota-fiscal.
 
      /*** Pesquisar a largura do item ***/
       FIND FIRST  ped-item  WHERE
             ped-item.nome-abrev   = nota-fiscal.nome-abrev    AND
             ped-item.nr-pedcli    = it-nota-fisc.nr-pedcli    AND
             ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped   AND
             ped-item.it-codigo    = it-nota-fisc.it-codigo    AND
             ped-item.cod-refer    = it-nota-fisc.cod-refer 
             USE-INDEX ch-cli-ped NO-LOCK NO-ERROR.

       IF AVAIL ped-item THEN DO:

         FIND var-result USE-INDEX id WHERE 
            var-result.item-cotacao  = it-nota-fisc.it-codigo   AND
            var-result.nr-estrut     = ped-item.nr-config       AND
            var-result.nome-var      = "LARGURA"  NO-LOCK NO-ERROR.
  
         IF AVAIL VAR-result THEN
           ASSIGN larg-jr = INT(var-result.des-result).

         FIND var-result USE-INDEX id WHERE 
            var-result.item-cotacao  = it-nota-fisc.it-codigo   AND
            var-result.nr-estrut     = ped-item.nr-config       AND
            var-result.nome-var      = "DIIN"  NO-LOCK NO-ERROR.
  
         IF AVAIL VAR-result THEN
           ASSIGN diin-jr = INT(var-result.des-result).


         FIND var-result USE-INDEX id WHERE 
            var-result.item-cotacao  = it-nota-fisc.it-codigo   AND
            var-result.nr-estrut     = ped-item.nr-config       AND
            var-result.nome-var      = "DIEX"  NO-LOCK NO-ERROR.
  
         IF AVAIL VAR-result THEN
           ASSIGN diex-jr = INT(var-result.des-result).

       END.

       ELSE DO:

        FIND FIRST fat-ser-lote WHERE
          fat-ser-lote.cod-estabel = nota-fiscal.cod-estabel  AND
          fat-ser-lote.serie       = nota-fiscal.serie        AND
          fat-ser-lote.nr-nota-fis = it-nota-fisc.nr-nota-fis AND
          fat-ser-lote.nr-seq-fat  = it-nota-fisc.nr-seq-fat  AND
          fat-ser-lote.it-codigo   = it-nota-fisc.it-codigo
          NO-LOCK NO-ERROR.
  
        IF AVAIL fat-ser-lote THEN DO:
          
          FIND FIRST it-pallet WHERE 
              it-pallet.cod-estabel = nota-fiscal.cod-estabel AND
              it-pallet.it-codigo   = it-nota-fisc.it-codigo  AND
              it-pallet.nr-pallet   = fat-ser-lote.nr-serlote  
              NO-LOCK NO-ERROR.
  
          IF AVAIL it-pallet THEN DO:
  
            FIND FIRST lote-carac-tec WHERE 
                lote-carac-tec.it-codigo = it-pallet.it-codigo AND
                lote-carac-tec.lote      = it-pallet.lote-bobina AND
                lote-carac-tec.cd-comp   = "largura"
                NO-LOCK NO-ERROR.
  
             IF AVAIL lote-carac-tec THEN
                  ASSIGN larg-jr = INT (lote-carac-tec.vl-result).
  
             FIND FIRST lote-carac-tec WHERE 
                 lote-carac-tec.it-codigo = it-pallet.it-codigo AND
                 lote-carac-tec.lote      = it-pallet.lote-bobina AND
                 lote-carac-tec.cd-comp   = "diin"
                 NO-LOCK NO-ERROR.

              IF AVAIL lote-carac-tec THEN
                   ASSIGN diin-jr = INT (lote-carac-tec.vl-result).
  
             FIND FIRST lote-carac-tec WHERE 
                 lote-carac-tec.it-codigo = it-pallet.it-codigo AND
                 lote-carac-tec.lote      = it-pallet.lote-bobina AND
                 lote-carac-tec.cd-comp   = "diex"
                 NO-LOCK NO-ERROR.

              IF AVAIL lote-carac-tec THEN
                   ASSIGN diex-jr = INT (lote-carac-tec.vl-result).
          END.
        END.
       END.   /*else do*/
    
       FOR EACH fat-ser-lote WHERE
         fat-ser-lote.cod-estabel = nota-fiscal.cod-estabel  AND
         fat-ser-lote.serie       = nota-fiscal.serie        AND
         fat-ser-lote.nr-nota-fis = it-nota-fisc.nr-nota-fis AND
         fat-ser-lote.nr-seq-fat  = it-nota-fisc.nr-seq-fat  AND
         fat-ser-lote.it-codigo   = it-nota-fisc.it-codigo
         NO-LOCK.
        
         FIND pallet WHERE
              pallet.it-codigo = it-nota-fisc.it-codigo AND 
              pallet.nr-pallet = fat-ser-lote.nr-serlote
              NO-LOCK NO-ERROR.
        
         IF NOT AVAIL pallet THEN NEXT.


         FIND tt-cliente WHERE
              tt-cliente.cod-emitente = nota-fiscal.cod-emitente AND
              tt-cliente.nr-nota-fis  = it-nota-fisc.nr-nota-fis 
              NO-ERROR.
        
         IF NOT AVAIL tt-cliente THEN 
              CREATE tt-cliente.
        
         ASSIGN tt-cliente.cod-emitente = nota-fiscal.cod-emitente 
                tt-cliente.nr-nota-fis  = it-nota-fisc.nr-nota-fis.


         
         FIND tt-nota-fiscal WHERE
              tt-nota-fiscal.nr-nota-fis = it-nota-fisc.nr-nota-fis AND
              tt-nota-fiscal.nr-seq-fat  = it-nota-fisc.nr-seq-fat  AND
              tt-nota-fiscal.nr-pallet   = fat-ser-lote.nr-serlote
              NO-ERROR.
        
         IF NOT AVAIL tt-nota-fiscal THEN 
              CREATE tt-nota-fiscal.
        
         ASSIGN tt-nota-fiscal.nr-nota-fis = it-nota-fisc.nr-nota-fis 
                tt-nota-fiscal.nr-seq-fat  = it-nota-fisc.nr-seq-fat  
                tt-nota-fiscal.nr-pallet   = fat-ser-lote.nr-serlote.
        
        
         /* itens sem referencia - largura = 0 */
         IF it-nota-fisc.cod-refer = "" THEN
         ASSIGN larg-jr = 0.   
        
        
         ASSIGN tt-nota-fiscal.largura = larg-jr
                tt-nota-fiscal.diin    = diin-jr
                tt-nota-fiscal.diex    = diex-jr.
        
         ASSIGN tt-nota-fiscal.it-codigo = it-nota-fisc.it-codigo
                tt-nota-fiscal.cod-refer = it-nota-fisc.cod-refer
                tt-nota-fiscal.qtd-plt   = 1
                tt-nota-fiscal.nr-bobinas = pallet.nr-bobinas
                tt-nota-fiscal.peso-liq   = pallet.peso-liq
                tt-nota-fiscal.peso-bru   = pallet.peso-bruto
                tt-nota-fiscal.nr-pedido  = pallet.nr-pedido
                tt-nota-fiscal.nr-sequencia = pallet.nr-sequencia.
  
  
              IF pallet.nr-pedido <> 0 THEN DO:              
                  FIND FIRST ped-venda WHERE ped-venda.nr-pedido = pallet.nr-pedido NO-LOCK NO-ERROR.
                  IF AVAIL ped-venda THEN 
                      ASSIGN tt-nota-fiscal.nome-abrev = ped-venda.nome-abrev.                  
              END.               
                
      
         ASSIGN tt-nota-fiscal.lg-plt     = c-lg-plt-1
                tt-nota-fiscal.cm-plt     = c-cm-plt-1
                tt-nota-fiscal.al-plt     = c-al-plt-1.
         

       END.
    END.
END.
          

/* Gera‡Æo da Planilha Excel */

    /* Cria Aplica‡Æo do Excel */

CREATE "Excel.Application" c-excel.
ASSIGN c-excel:DisplayAlerts = FALSE.


ASSIGN c-modelo-planilha = search("modelos\mod-esft0025.xls") 
       c-arq             = SESSION:TEMP-DIRECTORY.


FOR EACH tt-cliente NO-LOCK.

    RUN pi-cria-planilha.
    RUN pi-gera-planilha.
    RUN pi-mostra-planilha.

END.

/* Fim da Gera‡Æo da Planilha Excel */


RUN pi-finaliza-impressao.


run pi-finalizar in h-acomp.

return 'OK'.

/* fim do programa */




procedure pi-gera-planilha.

    IF tt-param.destino = 4 THEN DO:
    
       /* Rotina Cabe‡alho no Excel */
    
        IF c-cod-estabel <> "421" AND c-cod-estabel <> "411" THEN DO: /*solic-318*/ 
            ASSIGN i-linha = 3
                   c-relatorio:range("B" + STRING(i-linha)):VALUE = 
                     "BR 386 - Km 423 - Via 1 - Nr.280 - / Distrito Industrial / Montenegro-RS / Brasil".
            ASSIGN i-linha = 4
                   c-relatorio:range("B" + STRING(i-linha)):VALUE = 
                     "Phone: 55(51)457-5000 / Fax: 55(51)457-5050 ".
        END.
    
         ASSIGN i-linha = 10
                c-relatorio:range("D" + STRING(i-linha)):VALUE = " "
                c-relatorio:range("M" + STRING(i-linha)):VALUE = "1".
    
    
         ASSIGN i-linha = 16.
    
    END.

    ASSIGN soma-plt      = 0
           soma-bob      = 0
           soma-vol      = 0
           soma-pliquido = 0
           soma-pbruto   = 0
    
           soma-plt-pd      = 0
           soma-bob-pd      = 0
           soma-vol-pd      = 0
           soma-qt-palete   = 0
           soma-pliquido-pd = 0
           soma-pbruto-pd   = 0
           nr-seq-fat-ant   = 0.
    
    FOR EACH tt-nota-fiscal WHERE
        tt-nota-fiscal.nr-nota-fis = tt-cliente.nr-nota-fis
        NO-LOCK :
    
        ASSIGN tt-nota-fiscal.volume = (tt-nota-fiscal.lg-plt *
                                       tt-nota-fiscal.cm-plt *
                                       tt-nota-fiscal.al-plt).
        
        FIND FIRST nota-fiscal WHERE
            nota-fiscal.cod-estabel = tt-param.c-cod-estabel AND
            nota-fiscal.serie       = tt-param.c-serie       AND
            nota-fiscal.nr-nota-fis = tt-nota-fiscal.nr-nota-fis
            NO-LOCK NO-ERROR.
    
        IF NOT AVAIL nota-fiscal THEN 

         FIND FIRST nota-fiscal WHERE
            nota-fiscal.cod-estabel = tt-param.c-cod-estabel AND
            nota-fiscal.serie       = tt-param.c-serie       AND
            nota-fiscal.nr-nota-fis = tt-nota-fiscal.nr-nota-fis
            NO-LOCK NO-ERROR.
    
        IF NOT AVAIL nota-fiscal THEN NEXT.
    
        ASSIGN soma-qt-palete   = soma-qt-palete + 1.
    
        ASSIGN soma-bob = soma-bob + tt-nota-fiscal.nr-bobinas
               soma-vol = soma-vol + tt-nota-fiscal.volume
               soma-plt = soma-plt + tt-nota-fiscal.qtd-plt
               soma-pliquido = soma-pliquido + tt-nota-fiscal.peso-liq
               soma-pbruto   = soma-pbruto + tt-nota-fiscal.peso-bru. 
    
        IF tt-nota-fiscal.nr-seq-fat <> nr-seq-fat-ant AND 
           nr-seq-fat-ant <> 0 AND
           tt-param.destino = 4 THEN DO:
           
            
            IF i-linha > 68 THEN DO:
                ASSIGN lin-jr = lin-jr + 1.
    
                c-excel:Rows(i-linha):select no-error .
                c-excel:SELECTION:INSERT .
    
            END.
    
            ASSIGN i-linha = i-linha + 1
                   c-relatorio:range("F" + string(i-linha)):VALUE = "Subtotal"
                   c-relatorio:range("G" + STRING(i-linha)):VALUE = soma-bob-pd
                   c-relatorio:range("H" + STRING(i-linha)):VALUE = soma-pliquido-pd
                   c-relatorio:range("I" + STRING(i-linha)):VALUE = soma-pbruto-pd
    
                   c-relatorio:range("F" + string(i-linha)):Font:Bold = True
                   c-relatorio:range("G" + STRING(i-linha)):Font:Bold = True
                   c-relatorio:range("H" + STRING(i-linha)):Font:Bold = True
                   c-relatorio:range("I" + STRING(i-linha)):Font:Bold = True
    
                   soma-plt-pd      = 0
                   soma-bob-pd      = 0
                   soma-vol-pd      = 0
                   soma-pliquido-pd = 0
                   soma-pbruto-pd   = 0. 
        
        END.
    
        ASSIGN soma-bob-pd = soma-bob-pd + tt-nota-fiscal.nr-bobinas
               soma-vol-pd = soma-vol-pd + tt-nota-fiscal.volume
               soma-plt-pd = soma-plt-pd + tt-nota-fiscal.qtd-plt
               soma-pliquido-pd = soma-pliquido-pd + tt-nota-fiscal.peso-liq
               soma-pbruto-pd   = soma-pbruto-pd + tt-nota-fiscal.peso-bru
               nr-seq-fat-ant   = tt-nota-fiscal.nr-seq-fat
    
               nome-abrev-jr = nota-fiscal.nome-ab-cli.
    
        /***  CàDIGO PARA SAÖDA EM 132 COLUNAS ***/
    
    
     IF tt-param.destino = 4 THEN DO:
    
       /* Rotina Detalhes no Excel */
    
            
         IF i-linha > 68 THEN DO:
            ASSIGN lin-jr = lin-jr + 1.
    
            c-excel:Rows(i-linha):select no-error .
            c-excel:SELECTION:INSERT .
    
         END.
         
    
         ASSIGN i-linha = i-linha + 1
                c-relatorio:range("F" + string(i-linha)):Font:Bold = false 
                c-relatorio:range("G" + STRING(i-linha)):Font:Bold = false
                c-relatorio:range("H" + STRING(i-linha)):Font:Bold = false
                c-relatorio:range("I" + STRING(i-linha)):Font:Bold = FALSE
        
                c-relatorio:range("B" + STRING(i-linha)):VALUE = STRING (tt-nota-fiscal.nr-seq-fat) + "-" + STRING (tt-nota-fiscal.nr-pallet) + "-" + STRING (tt-nota-fiscal.cod-refer)
     
                c-relatorio:range("C" + STRING(i-linha)):VALUE = tt-nota-fiscal.largura
                c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-nota-fiscal.diin
                c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-nota-fiscal.diex
                c-relatorio:range("F" + STRING(i-linha)):VALUE = tt-nota-fiscal.it-codigo
                c-relatorio:range("G" + STRING(i-linha)):VALUE = tt-nota-fiscal.nr-bobinas
                c-relatorio:range("H" + STRING(i-linha)):VALUE = tt-nota-fiscal.peso-liq
                c-relatorio:range("I" + STRING(i-linha)):VALUE = tt-nota-fiscal.peso-bru
                c-relatorio:range("L" + STRING(i-linha)):VALUE = tt-nota-fiscal.nome-abrev + "/" + string(tt-nota-fiscal.nr-pedido) + "-" + STRING(tt-nota-fiscal.nr-sequencia)
             .
    
     END.
    
    end.
    
    IF tt-param.destino = 4 THEN DO:
          
          
          
          IF i-linha > 68 THEN DO:
             ASSIGN lin-jr = lin-jr + 1.
    
             c-excel:Rows(i-linha):select no-error .
             c-excel:SELECTION:INSERT .
    
          END.
          ASSIGN i-linha = i-linha + 1
                 c-relatorio:range("F" + STRING(i-linha)):VALUE = "Subtotal"
                 c-relatorio:range("G" + STRING(i-linha)):VALUE = soma-bob-pd
                 c-relatorio:range("H" + STRING(i-linha)):VALUE = soma-pliquido-pd
                 c-relatorio:range("I" + STRING(i-linha)):VALUE = soma-pbruto-pd
        
                 c-relatorio:range("F" + string(i-linha)):Font:Bold = True
                 c-relatorio:range("G" + STRING(i-linha)):Font:Bold = True
                 c-relatorio:range("H" + STRING(i-linha)):Font:Bold = True
                 c-relatorio:range("I" + STRING(i-linha)):Font:Bold = True
    
                 i-linha = 70 + lin-jr 
                 c-relatorio:range("G" + STRING(i-linha)):VALUE = soma-bob
                 c-relatorio:range("H" + STRING(i-linha)):VALUE = soma-pliquido
                 c-relatorio:range("I" + STRING(i-linha)):VALUE = soma-pbruto
          
                 c-relatorio:range("F" + string(i-linha)):Font:Bold = True
                 c-relatorio:range("G" + STRING(i-linha)):Font:Bold = True
                 c-relatorio:range("H" + STRING(i-linha)):Font:Bold = True
                 c-relatorio:range("I" + STRING(i-linha)):Font:Bold = True.
            
    END.
    
    ASSIGN data-jr = TODAY.
    
    
     IF tt-param.destino = 4 THEN DO:
    
       /* Rotina Totais no Excel */

         ASSIGN c-relatorio:range("D" + STRING(10)):VALUE = nota-fiscal.nr-nota-fis.

    
         ASSIGN i-linha = 75 + lin-jr
                c-relatorio:range("D" + STRING(i-linha)):VALUE = " "
    
                i-linha = 76 + lin-jr
                c-relatorio:range("D" + STRING(i-linha)):VALUE =  " "
    
                i-linha = 72 + lin-jr
                c-relatorio:range("D" + STRING(i-linha)):VALUE =  " "
    
                i-linha = 73 + lin-jr
                c-relatorio:range("D" + STRING(i-linha)):VALUE =  " "
    
                i-linha = 79 + lin-jr
                c-relatorio:range("M" + STRING(i-linha)):VALUE = soma-qt-palete
    
                i-linha = 81 + lin-jr
                c-relatorio:range("K" + STRING(i-linha)):VALUE = nome-abrev-jr
    
                i-linha = 9
                c-relatorio:range("C" + STRING(i-linha)):VALUE = nome-abrev-jr
    
                i-linha = 81 + lin-jr
                c-relatorio:range("M" + STRING(i-linha)):VALUE = data-jr.
    
    
     END.

END PROCEDURE.


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

    c-arquivo = c-arq + 'esft0025' + "-" + STRING(tt-cliente.nr-nota-fis)+ '.xls'. 

    OS-COPY VALUE(c-modelo-planilha) VALUE(c-arquivo).

    assign c-planilha  = c-excel:Workbooks:OPEN(c-arquivo)
           c-relatorio = c-excel:Sheets:item(1)
           c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo.

END PROCEDURE.


PROCEDURE pi-mostra-planilha:

DEF VAR i         AS INT  NO-UNDO.

    c-planilha:SAVE().
    c-planilha:CLOSE().
     
    c-excel:VISIBLE = true.

   
        c-planilha  = c-excel:Workbooks:OPEN(c-arquivo).
    

END PROCEDURE.


PROCEDURE pi-finaliza-impressao:

    RELEASE OBJECT c-excel.
 
END PROCEDURE.   

