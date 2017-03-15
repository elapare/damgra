/*****************************************************************************
**
**       Programa: esft0034rp.p
**
**       Author...........: Amgra / Edson
**       Created..........: 25/03/2011     
**
**       Objetivo: gera arquivo texto para limer cart
**
**       OBS.....: 
**
*******************************************************************************/
def buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "esft0034RP".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 
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
DEFINE VARIABLE pedido-ant         AS INTEGER               NO-UNDO.
DEFINE VARIABLE lin-jr             AS INTEGER INITIAL 0     NO-UNDO.

/* Vari†veis usadas para gerar planilha excel. */

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

DEFINE TEMP-TABLE tt-digita no-undo
    FIELD pal-nr-pedido            AS INT  FORMAT "zzzzzz9" LABEL "Seq-NF"
    FIELD pal-nr-pallet            AS CHAR FORMAT "x(15)"   label "Nr.Palete"
    FIELD pal-nr-bobina            AS CHAR FORMAT "x(10)"   LABEL "Nr.Bobina"
    FIELD pal-largura              AS int  FORMAT "zzz9"    LABEL "Larg"
    FIELD pal-diin                 AS int  FORMAT "zzz9"    LABEL "Diin"
    FIELD pal-diex                 AS int  FORMAT "zzz9"    LABEL "Diex"
    FIELD pal-it-codigo            LIKE movto-estoq.it-codigo LABEL "Item"
    FIELD pal-cod-refer            LIKE it-nota-fisc.cod-refer LABEL "Refer."
    FIELD pal-qtd-plt              AS INTEGER FORMAT "zz9"  LABEL "Qt.Plt"
    FIELD pal-nr-bobinas           LIKE pallet.nr-bobinas   LABEL "Qt.Bobs"
    FIELD pal-lg-plt               AS DECIMAL FORMAT "z9.999" LABEL "P-Larg"
    FIELD pal-cm-plt               AS DECIMAL FORMAT "z9.999" LABEL "P-Comp"
    FIELD pal-al-plt               AS DECIMAL FORMAT "z9.999" LABEL "P-Alt "
    FIELD pal-volume               AS DECIMAL FORMAT "z9.9999" LABEL "Volume"
    FIELD pal-peso-liq             AS DECIMAL LABEL "Peso Liquido" FORMAT "zzzzzzzz9.99"
    FIELD pal-peso-bru             AS DECIMAL LABEL "Peso Bruto" FORMAT "zzzzzzzz9.99"
    FIELD pal-cliente              AS CHAR LABEL "Cliente/Pedido" FORMAT "x(21)"
    INDEX chave IS PRIMARY UNIQUE pal-nr-pedido
                                  pal-nr-pallet
                                  pal-nr-bobina.



/*paramentros*/

DEFINE input param c-nr-nota-fis-i  AS CHARACTER initial "0032477"  NO-UNDO.
DEFINE input param c-nr-nota-fis-f  AS CHARACTER initial "0032477"  NO-UNDO.
DEFINE input param i-cod-emitente AS INTEGER   initial 17470  NO-UNDO.
DEFINE input param c-cod-estabel  AS CHARACTER    NO-UNDO. /*solic-318*/ 
DEFINE input param c-serie        AS CHARACTER initial "20" NO-UNDO.
  
/*
run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").
*/

/****************** Definiá∆o de Tabelas Tempor†rias do Relat¢rio **********************/


 

DEFINE VARIABLE c-lote-roteiro AS CHARACTER FORMAT "X(40)"  NO-UNDO.
DEFINE VARIABLE c-it-roteiro AS CHARACTER FORMAT "X(16)"  NO-UNDO.
DEFINE VARIABLE i-roteiro      AS INTEGER      NO-UNDO.
DEFINE VARIABLE c-lote-pesq      AS CHARACTER  FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE c-it-codigo-pesq  AS CHARACTER  FORMAT "X(16)"  NO-UNDO.
DEFINE VARIABLE c-estab-pesq      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dt-ficha          AS DATE        NO-UNDO.
DEFINE VARIABLE d-qt-requisitada AS decimal        NO-UNDO.
DEFINE VARIABLE d-vl-min AS decimal        NO-UNDO.
DEFINE VARIABLE d-vl-max AS decimal        NO-UNDO.


DEFINE VARIABLE c-nr-pedcli AS char     NO-UNDO.
DEFINE VARIABLE d-dt-pedido AS DATE        NO-UNDO.
DEFINE VARIABLE d-dt-nota   AS DATE        NO-UNDO.
DEFINE VARIABLE d-dt-validade AS DATE        NO-UNDO.
DEFINE VARIABLE d-dt-producao AS DATE        NO-UNDO.
DEFINE VARIABLE d-quantidade  AS DECIMAL NO-UNDO.
DEFINE BUFFER b-item FOR ITEM.
DEFINE BUFFER b-saldo-estoq FOR saldo-estoq.
DEFINE BUFFER b-movto-mat FOR movto-mat.  
DEFINE VARIABLE r-movto-mat AS ROWID       NO-UNDO.
DEFINE VARIABLE c-cli-1 AS CHARACTER  FORMAT "X(100)" NO-UNDO.
DEFINE VARIABLE c-cli-2 AS CHARACTER  FORMAT "X(100)" NO-UNDO.
DEFINE VARIABLE c-cli-3 AS CHARACTER  FORMAT "X(100)" NO-UNDO.
DEFINE VARIABLE c-cli-4 AS CHARACTER  FORMAT "X(100)" NO-UNDO.
DEFINE VARIABLE c-traco AS CHARACTER  NO-UNDO.

DEFINE VARIABLE C-emp  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE C-END1 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE C-END2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE C-END3 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE C-END4 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-end AS INTEGER     NO-UNDO.

define variable c-pedcli as char no-undo.

DEFINE TEMP-TABLE tt-lotes
    FIELD it-codigo      AS CHAR
    FIELD lote           AS CHAR
    FIELD lote-roteiro   AS CHAR
    FIELD it-roteiro     AS CHAR
    FIELD nr-roteiro     AS INTEGER
    FIELD dt-roteiro     AS DATE
    FIELD cod-emitente   AS INTEGER
    FIELD cod-estabel    AS CHAR
    FIELD serie          AS CHAR
    FIELD nr-nota-fis    AS CHAR
    FIELD dt-nota        AS DATE
    FIELD c-nr-pedcli    AS CHAR
    FIELD dt-pedido      AS DATE
    FIELD dt-validade    AS DATE
    FIELD quantidade     AS DECIMAL
    INDEX ch-lote IS PRIMARY UNIQUE  nr-nota-fis
                                     lote
                                     it-codigo.
        

DEFINE TEMP-TABLE tt-lotes-pesq
    FIELD it-codigo      AS CHAR
    FIELD lote           AS CHAR
 
    FIELD quantidade     AS DECIMAL
    INDEX ch-lote IS PRIMARY UNIQUE  lote
                                     it-codigo.
        

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
/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 

/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 



/****************** Definiáao de Vari†veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Definiáao de Vari†veis Campo do Layout do Relat¢rio **********************/ 

/****************** Definiáao de Vari†veis do Relat¢rio N∆o Pedidas em Tela ******************/ 

/* para executar o Excel */






DEF VAR c-coluna          AS char                NO-UNDO.




DEFINE VARIABLE arquivo-jr   AS CHARACTER  FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE larg-jr   AS INTEGER    NO-UNDO.



DEFINE VARIABLE c-data-jr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-anexo AS CHARACTER  NO-UNDO.
/****************** Definiáao de Vari†veis de Total do Relat¢rio *****************************/ 

/****************** Definiáao de Vari†veis dos Calculos do Relat¢rio *************************/ 


/***************** Definiáao de Vari†veis de Processamento do Relat¢rio *********************/
{utp/utapi019.i}
def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.

/****************** Definiáao de Forms do Relat¢rio 132 Colunas ***************************************/ 

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

define new shared stream str-rp.

assign c-programa     = "esft0034rp"
       c-versao       = "2.00"
       c-revisao      = "1.00.000"
       c-titulo-relat = "gera arquivo texto para limer cart"
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

run pi-inicializar in h-acomp(input "Gerando arquivo, Aguarde... ").

assign v-num-reg-lidos = 0.
ASSIGN
c-nr-nota-fis-i = STRING(INT(c-nr-nota-fis-i ),"9999999")
c-nr-nota-fis-f = STRING(INT(c-nr-nota-fis-f ),"9999999"). 


ASSIGN arquivo-jr = "v:\temp\esft0034" + STRING(TIME) + ".txt".

OUTPUT TO VALUE (arquivo-jr).     

FOR EACH nota-fiscal WHERE
    nota-fiscal.cod-estabel  = c-cod-estabel   AND  
    nota-fiscal.serie        = c-serie         AND
    nota-fiscal.nr-nota-fis >= c-nr-nota-fis-i AND 
    nota-fiscal.nr-nota-fis <= c-nr-nota-fis-f AND
    nota-fiscal.cod-emitente = i-cod-emitente
    NO-LOCK.

    FIND FIRST estabelec WHERE estabelec.cod-estabel = nota-fiscal.cod-estabel.

    find first emitente where 
       emitente.cod-emitente = nota-fiscal.cod-emitente no-lock no-error.
       
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    FOR EACH it-nota-fisc OF nota-fiscal.
            
    /*** Pesquisar a largura do item ***/

      ASSIGN larg-jr = 0
             diin-jr = 0
             diex-jr = 0
             c-pedcli = "".

      FIND FIRST  ped-item  WHERE
               ped-item.nome-abrev   = nota-fiscal.nome-ab-cli    AND
               ped-item.nr-pedcli    = it-nota-fisc.nr-pedcli    AND
               ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped   AND
               ped-item.it-codigo    = it-nota-fisc.it-codigo    AND
               ped-item.cod-refer    = it-nota-fisc.cod-refer 
               USE-INDEX ch-cli-ped NO-LOCK NO-ERROR.

      IF AVAIL ped-item THEN DO:
      
         FIND var-result USE-INDEX id WHERE 
              var-result.item-cotacao  = it-nota-fisc.it-codigo   AND
              var-result.nr-estrut     = ped-item.nr-config       AND
              var-result.nome-var      = "PEDCLI"  NO-LOCK NO-ERROR.
    
         IF AVAIL VAR-result THEN
             ASSIGN c-pedcli = var-result.des-result.



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

        
      
      FOR EACH fat-ser-lote WHERE
          fat-ser-lote.cod-estabel = nota-fiscal.cod-estabel  AND
          fat-ser-lote.serie       = nota-fiscal.serie        AND
          fat-ser-lote.nr-nota-fis = it-nota-fisc.nr-nota-fis AND
          fat-ser-lote.nr-seq-fat  = it-nota-fisc.nr-seq-fat  AND
          fat-ser-lote.it-codigo   = it-nota-fisc.it-codigo
          NO-LOCK.
        
          FIND FIRST pallet WHERE
               pallet.cod-estabel =  nota-fiscal.cod-estabel AND
               pallet.it-codigo = it-nota-fisc.it-codigo AND 
               pallet.nr-pallet = fat-ser-lote.nr-serlote
               NO-LOCK NO-ERROR.

          IF NOT AVAIL pallet THEN NEXT.

          ASSIGN c-data-jr = STRING (nota-fiscal.dt-emis-nota, "99/99/9999").

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


          PUT string(nota-fiscal.nr-nota-fis, "9999999")FORMAT "x(7)" AT 01 ";" 
              string(estabelec.cgc) FORMAT "x(14)" ";"
              string(nota-fiscal.serie) FORMAT "x(05)" ";"              
              string(c-data-jr) FORMAT "x(10)" ";"
              string(c-pedcli) FORMAT "x(10)" ";"
              string(it-nota-fisc.it-codigo) FORMAT "x(16)" ";"
              replace( string(it-nota-fisc.vl-preuni,"999999.9999"),",",".") format "x(12)" ";"
              string(fat-ser-lote.nr-serlote) FORMAT "x(40)" ";"
              string(larg-jr, "9999999") FORMAT "x(07)" ";"
              string(pallet.peso-liq, "9999999999.99") FORMAT "x(13)" ";"
              string(diin-jr, "9999999") FORMAT "x(7)" ";"
              string(diex-jr, "9999999") FORMAT "x(7)".

          FOR EACH it-pallet OF pallet NO-LOCK.

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


            PUT string(nota-fiscal.nr-nota-fis, "9999999")FORMAT "x(7)" AT 01 ";" 
                string(estabelec.cgc) FORMAT "x(14)" ";"
                string(nota-fiscal.serie) FORMAT "x(05)" ";" 
                string(c-data-jr) FORMAT "x(10)" ";"
                string(c-pedcli) FORMAT "x(10)" ";"
                string(it-nota-fisc.it-codigo) FORMAT "x(16)" ";"
                replace( string(it-nota-fisc.vl-preuni,"999999.9999"),",",".") format "x(12)" ";"
                string(fat-ser-lote.nr-serlote) FORMAT "x(40)" ";"
                string(it-pallet.lote-bobina) FORMAT "x(40)" ";"
                string(larg-jr, "9999999") FORMAT "x(07)" ";"
                string(it-pallet.saldo-bobina, "9999999999.99") FORMAT "x(13)" ";"
                string(diin-jr, "9999999") FORMAT "x(7)" ";"
                string(diex-jr, "9999999") FORMAT "x(7)".



 

                     FIND FIRST  tt-digita WHERE
                          tt-digita.pal-nr-pedido = it-nota-fisc.nr-seq-fat AND
                          tt-digita.pal-nr-pallet = fat-ser-lote.nr-serlote AND
                          tt-digita.pal-nr-bobina = it-pallet.lote-bobina
                          NO-ERROR.
                    
                     IF NOT AVAIL tt-digita THEN 
                          CREATE tt-digita.
                    
                     ASSIGN tt-digita.pal-nr-pedido = it-nota-fisc.nr-seq-fat 
                            tt-digita.pal-nr-pallet = fat-ser-lote.nr-serlote
                            tt-digita.pal-nr-bobina = it-pallet.lote-bobina
                            . 
                    
                     IF pallet.nr-pedido <> 0 THEN DO:
                         FIND FIRST ped-venda WHERE ped-venda.nr-pedido = pallet.nr-pedido NO-LOCK NO-ERROR.
                         IF AVAIL ped-venda THEN DO:
                             ASSIGN tt-digita.pal-cliente   = ped-venda.nome-abrev + "/"+ string(pallet.nr-pedido).
                         END.
                     END.
                    
                    
                     /* itens sem referencia - largura = 0 */
                     IF it-nota-fisc.cod-refer = "" THEN
                     ASSIGN larg-jr = 0.   
                    
                    
                     ASSIGN tt-digita.pal-largura = larg-jr
                            tt-digita.pal-diin    = diin-jr
                            tt-digita.pal-diex    = diex-jr.
                    
                     ASSIGN tt-digita.pal-it-codigo = it-nota-fisc.it-codigo
                            tt-digita.pal-cod-refer = it-nota-fisc.cod-refer
                            tt-digita.pal-qtd-plt   = 1
                            tt-digita.pal-nr-bobinas = 1
                            tt-digita.pal-peso-liq   = it-pallet.saldo-bobina
                            tt-digita.pal-peso-bru   = it-pallet.saldo-bobina.
                    
                     ASSIGN tt-digita.pal-lg-plt     = 0 /*c-lg-plt-1*/
                            tt-digita.pal-cm-plt     = 0 /*c-cm-plt-1*/
                            tt-digita.pal-al-plt     = 0 /*c-al-plt-1*/ .
                    
                     assign v-num-reg-lidos = v-num-reg-lidos + 1.
                   
              
                  


          END.

      END.

    END.

END.
 OUTPUT CLOSE.
RUN pi-finalizar IN h-acomp.

 
IF  v-num-reg-lidos > 0 THEN RUN pi-envia-email.

RETURN 'OK'.

PROCEDURE pi-envia-email.
 
    /* *** Definicao de Parametros *** */

     
    /* *** Definicao de Variaveis Locais *** */
    
    DEF VAR c-nome                  LIKE usuar_mestre.nom_usuario           NO-UNDO.
    DEF VAR c-remetente             LIKE usuar_mestre.cod_e_mail_local      NO-UNDO.
    DEF VAR c-responsavel           LIKE usuar_mestre.cod_e_mail_local      NO-UNDO.
                DEFINE VARIABLE c-assunto AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-destino AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-copia-esp AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-corpo AS CHARACTER  NO-UNDO.
c-anexo = arquivo-jr.

    FIND usuar_mestre NO-LOCK WHERE
             usuar_mestre.cod_usuario = c-seg-usuario NO-ERROR.
    
    ASSIGN c-remetente = ''.
    ASSIGN c-remetente = usuar_mestre.cod_e_mail_local WHEN AVAIL usuar_mestre.
  

    IF c-remetente = '' THEN DO:

      c-remetente = "victor.franca@polofilms.com.br".
    
    


    END.
     
        c-destino  = "".

        FOR EACH  cont-emit WHERE 
            cont-emit.cod-emitente = i-cod-emitente AND
            substring(cont-emit.nome,1,7) = "PACKING" and
            cont-emit.e-mail <> "" NO-LOCK .

            IF c-destino = "" THEN c-destino   = cont-emit.e-mail.
            ELSE
               c-destino = c-destino + "," +  cont-emit.e-mail .

        END.
        
        if c-destino = "" then 
            c-destino = emitente.e-mail.

       
      RUN pi-excel-tiv.

      c-anexo = c-anexo + "," + c-arq-anexo .

   
    FIND FIRST param-global NO-LOCK NO-ERROR.

     /* *** Delecao da Tabela Temporaria *** */
   
     FOR each tt-envio2 :  
         DELETE tt-envio2. 
     END.      
     FOR EACH tt-mensagem.
         DELETE tt-mensagem.
     END.
   
      c-assunto = "NOTAS FISCAIS POLO INDÈSTRIA E COMêRCIO LTDA".
      IF c-nr-nota-fis-i <> c-nr-nota-fis-f THEN
          c-corpo = "Conforme solicitado, segue arquivo das nossas notas fiscais nr." + c-nr-nota-fis-i + " a " + c-nr-nota-fis-f + ".".
      ELSE
          c-corpo = "Conforme solicitado, segue arquivo da nossa nota fiscal nr." + c-nr-nota-fis-i + ".".
    
    RUN utp/utapi019.p persistent set h-utapi019.

    /*
    c-destino = "edson@damgra.com.br".
*/


    CREATE tt-envio2.
    ASSIGN tt-envio2.versao-integracao  = 1
           tt-envio2.exchange           = param-global.log-1
           tt-envio2.remetente          = c-remetente
           tt-envio2.destino            = c-destino
           tt-envio2.copia              = ""
           tt-envio2.assunto            = c-assunto
           tt-envio2.importancia        = 2
           tt-envio2.log-enviada        = yes
           tt-envio2.log-lida           = yes
           tt-envio2.acomp              = yes
           tt-envio2.arq-anexo          = c-anexo
           tt-envio2.formato            = "".
           
 
 
    CREATE tt-mensagem.
    ASSIGN tt-mensagem.seq-mensagem = 1
           tt-mensagem.mensagem     = c-corpo.
            
    /*OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "envemail.txt").       */
    /*OUTPUT TO VALUE("v:\temp\envemail.txt").  */     
    
    RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                   INPUT  TABLE tt-mensagem,    
          	                       OUTPUT TABLE tt-erros).
 

    IF  RETURN-VALUE = "NOK" THEN DO:
        FOR EACH tt-erros:
        OUTPUT TO VALUE("v:\temp\envemail.txt").
           DISP tt-erros WITH 1 COLUMN WIDTH 300.  
        END.                   
        output close.            
    END.
    
    dos silent copy value(c-anexo) v:\temp.

  


    DELETE procedure h-utapi019.
 
    RETURN.

    END PROCEDURE.


/* fim do programa */



    PROCEDURE pi-excel-tiv.

        

               /* Cria Aplicaá∆o do Excel */
 
   CREATE "Excel.Application" c-excel.
   ASSIGN c-excel:DisplayAlerts = FALSE.
   ASSIGN c-modelo-planilha = search("modelos/mod-esft0014.xls") 
          c-arq             = SESSION:TEMP-DIRECTORY.

   RUN pi-cria-planilha.  

 


       /* Rotina Cabeáalho no Excel */
        for first estabelec where estabelec.cod-estabel = c-cod-estabel  no-lock.
       
        end.
        for first emitente where emitente.cod-emitente = estabelec.cod-emitente no-lock .
        end.

        
            ASSIGN i-linha = 3
                   c-relatorio:range("B" + STRING(i-linha)):VALUE = 
                     estabelec.endereco + " / " + estabelec.bairro + " / " +  estabelec.cidade + "-" + estabelec.estado + " / " +  estabelec.pais .

            ASSIGN i-linha = 4
                   c-relatorio:range("B" + STRING(i-linha)):VALUE = 
                   "Phone/Fax: " + emitente.telefone[1] + " / " + emitente.telefone[2].
       

         ASSIGN i-linha = 10
                c-relatorio:range("C" + STRING(i-linha)):VALUE = STRING (c-nr-nota-fis-i + " - " + string(c-serie) )
                c-relatorio:range("M" + STRING(i-linha)):VALUE = "1".


         ASSIGN i-linha = 16.


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
           soma-pbruto-pd   = 0.



            ASSIGN c-relatorio:range("B7"):VALUE = "LISTA DE EMBARQUE (POR BOBINAS)"
                   c-relatorio:range("B14"):VALUE = "/ Nr.Da Bobina".



    FOR EACH tt-digita:
        ASSIGN tt-digita.pal-volume = (tt-digita.pal-lg-plt *
                                       tt-digita.pal-cm-plt *
                                       tt-digita.pal-al-plt).

        FIND FIRST nota-fiscal WHERE
            nota-fiscal.cod-estabel = c-cod-estabel AND
            nota-fiscal.serie       = c-serie           AND
            nota-fiscal.nr-nota-fis = c-nr-nota-fis-i
            NO-LOCK NO-ERROR.

        IF NOT AVAIL nota-fiscal THEN NEXT.

        ASSIGN soma-qt-palete   = soma-qt-palete + 1.

        ASSIGN soma-bob = soma-bob + pal-nr-bobinas
               soma-vol = soma-vol + pal-volume
               soma-plt = soma-plt + pal-qtd-plt
               soma-pliquido = soma-pliquido + pal-peso-liq
               soma-pbruto   = soma-pbruto + pal-peso-bru. 

        IF tt-digita.pal-nr-pedido <> pedido-ant AND 
           pedido-ant <> 0   THEN DO:



            IF i-linha > 67 THEN DO:
                ASSIGN lin-jr = lin-jr + 1.

                c-excel:Rows(i-linha + 1):select no-error .
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

        ASSIGN soma-bob-pd = soma-bob-pd + pal-nr-bobinas
               soma-vol-pd = soma-vol-pd + pal-volume
               soma-plt-pd = soma-plt-pd + pal-qtd-plt
               soma-pliquido-pd = soma-pliquido-pd + pal-peso-liq
               soma-pbruto-pd   = soma-pbruto-pd + pal-peso-bru
               pedido-ant       = tt-digita.pal-nr-pedido

               nome-abrev-jr = nota-fiscal.nome-ab-cli.

        /***  C‡DIGO PARA SA÷DA EM 132 COLUNAS ***/



       /* Rotina Detalhes no Excel */     


         IF i-linha > 67 THEN DO:
            ASSIGN lin-jr = lin-jr + 1.

            c-excel:Rows(i-linha + 1):select no-error .
            c-excel:SELECTION:INSERT .

         END.


         ASSIGN i-linha = i-linha + 1
                c-relatorio:range("F" + string(i-linha)):Font:Bold = false 
                c-relatorio:range("G" + STRING(i-linha)):Font:Bold = false
                c-relatorio:range("H" + STRING(i-linha)):Font:Bold = false
                c-relatorio:range("I" + STRING(i-linha)):Font:Bold = FALSE.

         
             ASSIGN c-relatorio:range("B" + STRING(i-linha)):VALUE = STRING (pal-nr-pedido) + "-" + trim(STRING (pal-nr-pallet)) + "-" + STRING (pal-nr-bobina).


         ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE = pal-largura
                c-relatorio:range("D" + STRING(i-linha)):VALUE = pal-diin
                c-relatorio:range("E" + STRING(i-linha)):VALUE = pal-diex
                c-relatorio:range("F" + STRING(i-linha)):VALUE = pal-it-codigo
                c-relatorio:range("G" + STRING(i-linha)):VALUE = pal-nr-bobinas
                c-relatorio:range("H" + STRING(i-linha)):VALUE = pal-peso-liq
                c-relatorio:range("I" + STRING(i-linha)):VALUE = pal-peso-bru
                c-relatorio:range("l" + STRING(i-linha)):VALUE = pal-cliente.



    end.





          IF i-linha > 67 THEN DO:
             ASSIGN lin-jr = lin-jr + 1.

             c-excel:Rows(i-linha + 1):select no-error .
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



    ASSIGN data-jr = TODAY.




       /* Rotina Totais no Excel */

         ASSIGN i-linha = 75 + lin-jr
                c-relatorio:range("D" + STRING(i-linha)):VALUE = "" /*tt-param.c-obs1*/

                i-linha = 76 + lin-jr
                c-relatorio:range("D" + STRING(i-linha)):VALUE = "" /*tt-param.c-obs2*/

                i-linha = 72 + lin-jr
                c-relatorio:range("D" + STRING(i-linha)):VALUE = "" /*tt-param.c-carreta*/

                i-linha = 73 + lin-jr
                c-relatorio:range("D" + STRING(i-linha)):VALUE = "" /*tt-param.c-container*/

                i-linha = 79 + lin-jr
                c-relatorio:range("M" + STRING(i-linha)):VALUE = soma-qt-palete

                i-linha = 81 + lin-jr
                c-relatorio:range("K" + STRING(i-linha)):VALUE = nome-abrev-jr

                i-linha = 9
                c-relatorio:range("C" + STRING(i-linha)):VALUE = nome-abrev-jr

                i-linha = 81 + lin-jr
                c-relatorio:range("M" + STRING(i-linha)):VALUE = data-jr.




          RUN pi-finaliza-impressao.

          RETURN 'OK'.

    END PROCEDURE.

    PROCEDURE pi-cria-planilha:

    DEF VAR c-arquivo AS CHAR FORMAT "x(50)" NO-UNDO.

        c-arquivo = c-arq + 'pcklist-vga' + STRING(time)+ '.xls'. 

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

        c-excel:VISIBLE = no.

       /*
        DO i = 1 TO NUM-ENTRIES(c-arq-anexo):
            c-arquivo = ENTRY(i,c-arq-anexo).
            c-planilha  = c-excel:Workbooks:OPEN(c-arquivo).

        END.
        */

        /*c-excel:QUIT().*/
        RELEASE OBJECT c-excel.

    END PROCEDURE.


    /* fim do programa */


