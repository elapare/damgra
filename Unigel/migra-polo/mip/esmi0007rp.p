
/*------------------------------------------------------------------------
File.............: esmi0007rp.p
Description......: Relat¢rio de OM's Suspensas
Input Parameters : 
Output Parameters: 
Author...........: Amgra - Jos‚ Roberto
Created..........: 23/10/2010  
OBS..............: 
------------------------------------------------------------------------*/
define buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "esmi0007rp".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

/****************** Defini‡Æo de Tabelas Tempor rias do Relat¢rio **********************/
DEF BUFFER bf-saldo-estoq FOR saldo-estoq.
def buffer bf-ord-manut for ord-manut.

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
    field i-nr-ord-produ-ini   AS INT 
    field i-nr-ord-produ-fim   AS INT 
    field dt-emissao-ini       as DATE 
    field dt-emissao-fim       as date
    field c-cd-equipto-ini     as CHAR 
    field c-cd-equipto-fim     as CHAR 
    FIELD i-opcao              as integer
.


/**********Tabela temporaria************/

define temp-table tt-ordem NO-UNDO
    FIELD nr-ord-produ    AS INTEGER FORMAT ">>>>>>>>>9" 
    FIELD cd-equipto      AS CHAR    FORMAT "x(16)" 
    FIELD desc-equipto    AS CHAR    FORMAT "x(45)" 
    FIELD des-man-corr    AS CHAR    FORMAT "x(50)" 
    FIELD narrativa-om    AS CHAR    FORMAT "x(2000)"
    FIELD tipo            AS INTEGER FORMAT ">9" 
    FIELD numero-ordem    AS INTEGER FORMAT ">>>>>>>>>9"
    field status-ordem    as char    format "x(20)" 
    FIELD narrativa-oc    AS CHAR    FORMAT "x(2000)"
    FIELD data-emissao    AS DATE    FORMAT "99/99/9999"
    FIELD data-cotacao    AS DATE    FORMAT "99/99/9999"
    FIELD data-aprova     AS DATE    FORMAT "99/99/9999"
    FIELD num-pedido      AS INTEGER FORMAT ">>>>>>>>>9" 
    FIELD data-entrega    AS DATE    FORMAT "99/99/9999"
    FIELD data-atualiz    AS DATE    FORMAT "99/99/9999"
    INDEX ch-tt-ordem     IS PRIMARY UNIQUE nr-ord-produ
                                            numero-ordem.

    
    .

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

def new shared var c-cod-estabel        AS CHAR    format "X(3)"        initial "{cdp\poloestab.i 422}"             no-undo./*solic-318*/
def new shared var i-nr-ord-produ-ini   AS INT     format ">>>>>>>>>9"  initial 0                 no-undo.
def new shared var i-nr-ord-produ-fim   AS INT     format ">>>>>>>>>9"  initial 999999999         no-undo.
def new shared var dt-emissao-ini       as DATE    format 99/99/9999    initial today             no-undo.
def new shared var dt-emissao-fim       as date    format 99/99/9999    initial TODAY             no-undo.
def new shared var c-cd-equipto-ini     as CHAR    format "x(16)"       initial ""                no-undo.
def new shared var c-cd-equipto-fim     as CHAR    format "x(16)"       initial "ZZZZZZZZZZZZZZZ" no-undo.


/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

DEFINE VARIABLE data-hora-jr            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE conta-jr                AS INTEGER    NO-UNDO.

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

form "    "  at 001
     with down width 132 side-labels no-box stream-io frame f-relat-01-132.


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

assign c-programa     = "esmi0007rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Relat¢rio de OM's Suspensas"
       c-sistema      = "MIP".

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

    assign c-cod-estabel        = tt-param.c-cod-estabel      
           i-nr-ord-produ-ini   = tt-param.i-nr-ord-produ-ini 
           i-nr-ord-produ-fim   = tt-param.i-nr-ord-produ-fim 
           dt-emissao-ini       = tt-param.dt-emissao-ini     
           dt-emissao-fim       = tt-param.dt-emissao-fim     
           c-cd-equipto-ini     = tt-param.c-cd-equipto-ini   
           c-cd-equipto-fim     = tt-param.c-cd-equipto-fim   
           i-opcao              = tt-param.i-opcao        

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

    ASSIGN c-modelo-planilha = search("modelos/mod-esmi0007.xls") 
           c-arq             = SESSION:TEMP-DIRECTORY.

    RUN pi-cria-planilha.


END.

ASSIGN data-hora-jr = STRING (TODAY) + " - " + string(time, "HH:MM").


assign v-num-reg-lidos = 0.

ASSIGN c-relatorio:range("Q" + STRING(3)):VALUE = data-hora-jr.  

ASSIGN i-linha = 6. 


/* L¢gica do Programa */
FOR EACH ord-manut NO-LOCK WHERE
    ord-manut.nr-ord-produ >= i-nr-ord-produ-ini AND
    ord-manut.nr-ord-produ <= i-nr-ord-produ-fim AND
    ord-manut.cod-estabel   = c-cod-estabel      AND 
    ord-manut.cd-equipto   >= c-cd-equipto-ini   AND 
    ord-manut.cd-equipto   <= c-cd-equipto-fim   AND 
    ord-manut.dt-manut     >= dt-emissao-ini     AND
    ord-manut.dt-manut     <= dt-emissao-fim     AND
    ord-manut.estado-om     < 4
    USE-INDEX dt-om.

    IF i-opcao = 2 AND ord-manut.estado-om <> 3 THEN NEXT.
    
            
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input "Lendo Ordens de Manuten‡Æo: " + string(v-num-reg-lidos)).

    find FIRST equipto
        where equipto.cd-equipto = ord-manut.cd-equipto
        no-lock no-error.

    CREATE tt-ordem.

    ASSIGN tt-ordem.nr-ord-produ  = ord-manut.nr-ord-produ
           tt-ordem.cd-equipto    = ord-manut.cd-equipto
           tt-ordem.desc-equipto  = IF AVAIL equipto THEN equipto.descricao ELSE "" 
           tt-ordem.des-man-corr  = ord-manut.des-man-corr  
           tt-ordem.tipo          = ord-manut.cd-tipo.  


    FIND FIRST msg-ord-man WHERE
        msg-ord-man.nr-ord-produ = ord-manut.nr-ord-produ
        NO-LOCK NO-ERROR.

    IF AVAIL msg-ord-man THEN 
      ASSIGN tt-ordem.narrativa-om = msg-ord-man.msg-exp.

    ASSIGN conta-jr = 0.

    FOR EACH am-mi-compras-manut WHERE
        am-mi-compras-manut.nr-ord-prod = ord-manut.nr-ord-produ
        NO-LOCK.

        IF NOT CAN-FIND(FIRST ordem-compra WHERE
             ordem-compra.numero-ordem = am-mi-compras-manut.numero-ordem and
             ordem-compra.situacao <> 4) THEN NEXT.
        ASSIGN conta-jr = conta-jr + 1.

        IF conta-jr > 1 THEN DO:

           CREATE tt-ordem.
           
           ASSIGN tt-ordem.nr-ord-produ  = ord-manut.nr-ord-produ
                  tt-ordem.cd-equipto    = ord-manut.cd-equipto
                  tt-ordem.desc-equipto  = IF AVAIL equipto THEN equipto.descricao ELSE "" 
                  tt-ordem.des-man-corr  = ord-manut.des-man-corr  
                  tt-ordem.tipo          = ord-manut.cd-tipo.  
           
           FIND FIRST msg-ord-man WHERE
               msg-ord-man.nr-ord-produ = ord-manut.nr-ord-produ
               NO-LOCK NO-ERROR.
           
           IF AVAIL msg-ord-man THEN 
             ASSIGN tt-ordem.narrativa-om = msg-ord-man.msg-exp.

        END.

        FIND FIRST ordem-compra WHERE
             ordem-compra.numero-ordem = am-mi-compras-manut.numero-ordem and
             ordem-compra.situacao <> 4
            NO-LOCK NO-ERROR.

        IF AVAIL ordem-compra THEN DO: 
        
            ASSIGN tt-ordem.numero-ordem    = ordem-compra.numero-ordem 
                   tt-ordem.narrativa-oc    = ordem-compra.narrativa
                   tt-ordem.data-emissao    = ordem-compra.data-emissao
                   tt-ordem.num-pedido      = ordem-compra.num-pedido
                   tt-ordem.data-cotacao    = ordem-compra.data-cotacao.
                   
            if ordem-compra.situacao = 1 then 
               assign tt-ordem.status-ordem = "NÆo Confirmada".       
    
            if ordem-compra.situacao = 2 then 
               assign tt-ordem.status-ordem = "Confirmada".       
    
            if ordem-compra.situacao = 3 then 
               assign tt-ordem.status-ordem = "Cotada".       
    
            if ordem-compra.situacao = 4 then 
               assign tt-ordem.status-ordem = "Eliminada".       
    
            if ordem-compra.situacao = 5 then 
               assign tt-ordem.status-ordem = "Em Cota‡Æo".       
    
            if ordem-compra.situacao = 6 then 
               assign tt-ordem.status-ordem = "Recebida".       
    
            FIND FIRST prazo-compra WHERE
                prazo-compra.numero-ordem = ordem-compra.numero-ordem   
                USE-INDEX ordem NO-LOCK NO-ERROR.

            IF AVAIL prazo-compra THEN 
                ASSIGN tt-ordem.data-entrega = prazo-compra.data-entrega
                       tt-ordem.data-atualiz = prazo-compra.data-entrega.

            FIND last cotacao-item OF ordem-compra NO-LOCK NO-ERROR.
            
            IF AVAIL cotacao-item and cotacao-item.data-cotacao <> 11/11/1111 THEN
                tt-ordem.data-cotacao    = cotacao-item.data-cotacao.                
                  
            IF AVAIL cotacao-item and cotacao-item.data-cotacao = 11/11/1111 THEN
                tt-ordem.data-cotacao    = cotacao-item.data-atualiz.                
                  
            FIND LAST doc-pend-aprov OF ordem-compra WHERE
                      doc-pend-aprov.ind-tip-doc  = 3 
                      NO-LOCK NO-ERROR.

            IF AVAIL doc-pend-aprov THEN 
               ASSIGN tt-ordem.data-aprova  = doc-pend-aprov.dt-aprova.
               
/*               

            FIND FIRST pedido-compr WHERE 
                 pedido-compr.num-pedido   = ordem-compra.num-pedido
                     USE-INDEX numero NO-LOCK NO-ERROR.

            IF AVAIL pedido-compr THEN
                assign tt-ordem.data-entrega = pedido-compr.data-entrega
                       tt-ordem.data-atualiz = pedido-compr.data-pedido.
                       
*/      

            if ordem-compra.situacao = 1 then
               assign tt-ordem.num-pedido    = 0                 
                      tt-ordem.data-entrega  = ?
                      tt-ordem.data-atualiz  = ?
                      tt-ordem.data-cotacao  = ?
                      tt-ordem.data-aprova   = ?.
                      
            if ordem-compra.situacao = 5 then
               assign tt-ordem.num-pedido    = 0                 
                      tt-ordem.data-entrega  = ?
                      tt-ordem.data-atualiz  = ?
                      tt-ordem.data-aprova   = ?.
                      
            if ordem-compra.situacao = 3 then
               assign tt-ordem.num-pedido    = 0                 
                      tt-ordem.data-entrega  = ?
                      tt-ordem.data-atualiz  = ?.                      
                      

        END.   

    END.

END.

assign v-num-reg-lidos = 0.

for each tt-ordem no-lock

    by tt-ordem.cd-equipto
    by tt-ordem.nr-ord-produ
    by tt-ordem.data-atualiz descend.

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input "Gerando a Planilha Excel: " + string(v-num-reg-lidos)).

    ASSIGN i-linha = i-linha + 1.
 
    assign tt-ordem.narrativa-om = replace(tt-ordem.narrativa-om,(chr(10) + chr(10))," " )
           tt-ordem.narrativa-oc = replace(tt-ordem.narrativa-oc,(chr(10) + chr(10))," ").

    assign tt-ordem.narrativa-om = replace(tt-ordem.narrativa-om, chr(10)," " )
           tt-ordem.narrativa-oc = replace(tt-ordem.narrativa-oc, chr(10)," ").
        
    ASSIGN c-relatorio:range("B" + STRING(i-linha)):VALUE = tt-ordem.nr-ord-produ  
           c-relatorio:range("C" + STRING(i-linha)):VALUE = tt-ordem.cd-equipto
           c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-ordem.desc-equipto      
           c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-ordem.des-man-corr      
           c-relatorio:range("F" + STRING(i-linha)):VALUE = tt-ordem.narrativa-om
           c-relatorio:range("G" + STRING(i-linha)):VALUE = tt-ordem.tipo
           c-relatorio:range("H" + STRING(i-linha)):VALUE = tt-ordem.numero-ordem 
           c-relatorio:range("I" + STRING(i-linha)):VALUE = tt-ordem.narrativa-oc 
           c-relatorio:range("J" + STRING(i-linha)):VALUE = tt-ordem.status-ordem 
           c-relatorio:range("K" + STRING(i-linha)):VALUE = tt-ordem.data-emissao 
           c-relatorio:range("L" + STRING(i-linha)):VALUE = tt-ordem.data-cotacao 
           c-relatorio:range("M" + STRING(i-linha)):VALUE = tt-ordem.data-aprova  
           c-relatorio:range("N" + STRING(i-linha)):VALUE = tt-ordem.num-pedido   
           c-relatorio:range("O" + STRING(i-linha)):VALUE = tt-ordem.data-entrega 
           c-relatorio:range("P" + STRING(i-linha)):VALUE = tt-ordem.data-atualiz .      

END.





RUN pi-finalizar IN h-acomp.
RUN pi-finaliza-impressao.

   RETURN 'OK'.

RETURN 'OK'.

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

    c-arquivo = c-arq + 'esmi0007' + STRING(time)+ '.xls'.

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
