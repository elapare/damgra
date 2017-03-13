/*****************************************************************************
**
**       Programa: escr007rp.p
**
**       Data....: 23/06/2013   
**
**       Autor...: Damgra/Edson
**
**       Objetivo: Relaá∆o de t°tulos do contas a receber - AGING Filmes
**
**       OBS.....: 
**
*******************************************************************************/

DEFINE BUFFER empresa      FOR  mgmulti.empresa.
DEFINE BUFFER unid_negoc   for  ems5.unid_negoc.

define variable c-prog-gerado as character no-undo initial "escr007rp".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2002.p (input c-prog-gerado, input "2.00.00.000").

/****************** Definiá∆o de Tabelas Tempor†rias do Relat¢rio **********************/

def buffer bf-ped-venda for ped-venda.

  
    def buffer b_movto_tit_acr_aux
        for movto_tit_acr.
   

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
    field ep-codigo            as character
    field dt-corte             AS DATE 
    field dt-vencto-ini        AS DATE 
    field dt-vencto-fim        AS DATE 
    field c-cod-esp-ini        AS CHAR 
    field c-cod-esp-fim        AS CHAR 
    field c-cod-estabel-ini        AS CHAR 
    field c-cod-estabel-fim        AS CHAR 

    field c-mercado            AS CHAR. 

  DEFINE TEMP-TABLE tAging
    FIELD Cliente AS CHAR FORMAT "x(20)"
    FIELD vencerAte30 AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD vencerDe31a60 AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD vencerDe61a90 AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD vencerMais90 AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD vencerVX AS DECIMAL FORMAT ">>>,>>>,>>9.99"

    FIELD vencidosAte30 AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD vencidosDe31a60 AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD vencidosDe61a90 AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD vencidosMais90 AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD estado AS CHAR
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
/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 

DEFINE VARIABLE dt-programac-jr AS DATE       NO-UNDO.
DEFINE VARIABLE dt-pallet-jr    AS DATE       NO-UNDO.
DEFINE VARIABLE dt-ini-prod-jr  AS DATE       NO-UNDO.
DEFINE VARIABLE dt-fim-prod-jr  AS DATE       NO-UNDO.
DEFINE VARIABLE maquina-jr      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE maq-jr          AS INTEGER    NO-UNDO.

/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 


def new shared var c-cod-estabel-ini    like pallet.cod-estabel           format "X(3)"         initial ""                 no-undo.
def new shared var c-cod-estabel-fim    like pallet.cod-estabel           format "X(3)"         initial "ZZZ"              no-undo.
def new shared var dt-entrega-ini       like pallet.data-pallet           format "99/99/9999"   initial "01/01/1800"       no-undo.
def new shared var dt-entrega-fim       like pallet.data-pallet           format "99/99/9999"   initial "12/31/9999"       no-undo.
def new shared var c-nome-abrev-ini     like emitente.nome-abrev          format "X(12)"        initial ""                 no-undo.
def new shared var c-nome-abrev-fim     like emitente.nome-abrev          format "X(12)"        initial "ZZZZZZZZZZZZ"     no-undo.
def new shared var i-cod-emitente-ini   like ped-venda.cod-emitente       format ">>>>>>>9"     initial 0                  no-undo.
def new shared var i-cod-emitente-fim   like ped-venda.cod-emitente       format ">>>>>>>9"     initial 99999999           no-undo.
def new shared var i-nr-pedido-ini      like ped-venda.nr-pedido          format ">>>>>>>9"     initial 0                  no-undo.
def new shared var i-nr-pedido-fim      like ped-venda.nr-pedido          format ">>>>>>>9"     initial 99999999           no-undo.
def new shared var c-it-codigo-ini      like pallet.it-codigo             format "X(16)"        initial ""                 no-undo.
def new shared var c-it-codigo-fim      like pallet.it-codigo             format "X(16)"        initial "ZZZZZZZZZZZZZZZZ" no-undo.
def new shared var i-canal-venda-ini    like nota-fiscal.cod-canal-venda  format ">>9"          initial 0                  no-undo.
def new shared var i-canal-venda-fim    like nota-fiscal.cod-canal-venda  format ">>9"          initial 999                no-undo.
def new shared var c-tp-pedido-ini      like ped-venda.tp-pedido          format "X(1)"         initial ""                 no-undo.
def new shared var c-tp-pedido-fim      like ped-venda.tp-pedido          format "X(1)"         initial "Z"                no-undo.

/****************** Definiáao de Vari†veis p/ Campos Virtuais do Relat¢rio *******************/ 

DEFINE VARIABLE merc-jr            AS CHAR                      NO-UNDO.
DEFINE VARIABLE desc-aging         AS CHAR                      NO-UNDO.
DEFINE VARIABLE d-cotacao AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dt-cot AS DATE    NO-UNDO.
 def var v_cod_cart_bcia
        as character
        format "x(3)":U
        label "Carteira"
        column-label "Carteira"
        no-undo.
    def var v_cod_portador
        as character
        format "x(5)":U
        label "Portador"
        column-label "Portador"
        no-undo.


DEFINE VARIABLE saldo-atend        AS DECIMAL                   NO-UNDO.

/****************** Definiáao de Vari†veis Campo do Layout do Relat¢rio **********************/ 

/****************** Definiáao de Vari†veis do Relat¢rio N∆o Pedidas em Tela ******************/ 


/****************** Definiáao de Vari†veis de Total do Relat¢rio *****************************/ 


/****************** Definiáao de Vari†veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Definiáao de Vari†veis de Processamento do Relat¢rio *********************/

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


    DEFINE VAR pDtCorte         AS DATE.
    DEFINE VAR pMercado         AS CHAR.
    DEFINE VAR pDataIni         AS DATE.
    DEFINE VAR pDataFim         AS DATE.
    DEFINE VAR pCodEspIni       AS CHAR.
    DEFINE VAR pCodEspFim       AS CHAR.
    DEFINE VAR pEnderecoAbrir   AS CHAR.
    DEFINE VAR pEnderecoSalvar  AS CHAR.

    DEFINE VARIABLE NOME-ABREV-ANT AS CHARACTER   NO-UNDO.
    
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DEFINE VARIABLE linhaInicial AS INTEGER NO-UNDO.
    


/****************** Definiáao de Forms do Relat¢rio 132 Colunas ***************************************/ 

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

assign c-programa     = "escr007rp"
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


run grapi/gr2009.p (input tt-param.destino,
                    input tt-param.arquivo,
                    input tt-param.usuario,
                    input no).

    assign i-ep-codigo-usuario = tt-param.ep-codigo
           v_cdn_empres_usuar  = i-ep-codigo-usuario.



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
    
    /* Cria Aplicaá∆o do Excel */

    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

    ASSIGN c-modelo-planilha = search("modelos/mod-escr006.xls") 
           c-arq             = "v:\temp\". /*SESSION:TEMP-DIRECTORY.*/

    RUN pi-cria-planilha.


END.

    ASSIGN linhaInicial = 5.

    ASSIGN i = linhaInicial.         

ASSIGN
    pDtCorte          = tt-param.dt-corte      
    pMercado         = (IF tt-param.c-mercado = "i" THEN "int" ELSE  IF tt-param.c-mercado = "e" THEN "ext" ELSE "A")
    pDataIni         = tt-param.dt-vencto-ini  
    pDataFim         = tt-param.dt-vencto-fim  
    pCodEspIni       = tt-param.c-cod-esp-ini  
    pCodEspFim       = tt-param.c-cod-esp-fim .
    
    RUN Aging (pDtCorte, pMercado, pDataIni, pDataFim, pCodEspIni, pCodEspFim).

assign v-num-reg-lidos = 0.


ASSIGN i-linha = 7. 


     IF tt-param.destino = 4 THEN DO:

       

        FOR EACH tAging.
            assign v-num-reg-lidos = v-num-reg-lidos + 1.
            run pi-acompanhar in h-acomp(input "Gerando Planilha: " + string(v-num-reg-lidos)).


            c-Excel:Worksheets("Plan1"):cells(i,1):VALUE = Cliente.
            c-Excel:Worksheets("Plan1"):cells(i,14):VALUE = IF TaGING.estado = "ex" THEN "EXT" ELSE "INT".
            c-excel:Worksheets("Plan1"):cells(i,2):VALUE = vencerAte30.
            c-excel:Worksheets("Plan1"):cells(i,3):VALUE = vencerDe31a60.
            c-excel:Worksheets("Plan1"):cells(i,4):VALUE = vencerDe61a90.
            c-excel:Worksheets("Plan1"):cells(i,5):VALUE = vencerMais90.

            c-excel:Worksheets("Plan1"):cells(i,6):Formula = "=sum(B" + string(i) + ":E" + string(i) + ")".

            c-excel:Worksheets("Plan1"):cells(i,7):VALUE = vencidosAte30.
            c-excel:Worksheets("Plan1"):cells(i,8):VALUE = vencidosDe31a60.
            c-excel:Worksheets("Plan1"):cells(i,9):VALUE = vencidosDe61a90.
            c-excel:Worksheets("Plan1"):cells(i,10):VALUE = vencidosMais90.

            c-excel:Worksheets("Plan1"):cells(i,11):Formula = "=sum(G" + string(i) + ":J" + string(i) + ")".

            c-excel:Worksheets("Plan1"):cells(i,12):VALUE = vencerVX.

            c-excel:Worksheets("Plan1"):cells(i,13):Formula = "=F" + string(i) + "+K" + string(i) + "+L" + string(i).


            ASSIGN i = i + 1.
        END.

        ASSIGN i = i + 1.

        /* ------  Totais   ------- */


        c-excel:Worksheets("Plan1"):cells(2,2):VALUE = "Aging Contas a Receber " + STRING(pDataIni) + " - " + STRING(pDataFim).

        c-excel:Worksheets("Plan1"):cells(i,2):Formula = "=sum(B" + string(linhaInicial) + ":B" + string(i - 1) + ")".
        c-excel:Worksheets("Plan1"):cells(i,3):Formula = "=sum(C" + string(linhaInicial) + ":C" + string(i - 1) + ")".
        c-excel:Worksheets("Plan1"):cells(i,4):Formula = "=sum(D" + string(linhaInicial) + ":D" + string(i - 1) + ")".
        c-excel:Worksheets("Plan1"):cells(i,5):Formula = "=sum(E" + string(linhaInicial) + ":E" + string(i - 1) + ")".

        c-excel:Worksheets("Plan1"):cells(i,6):Formula = "=sum(F" + string(linhaInicial) + ":F" + string(i - 1) + ")".

        c-excel:Worksheets("Plan1"):cells(i,7):Formula = "=sum(G" + string(linhaInicial) + ":G" + string(i - 1) + ")".
        c-excel:Worksheets("Plan1"):cells(i,8):Formula = "=sum(H" + string(linhaInicial) + ":H" + string(i - 1) + ")".
        c-excel:Worksheets("Plan1"):cells(i,9):Formula = "=sum(I" + string(linhaInicial) + ":I" + string(i - 1) + ")".
        c-excel:Worksheets("Plan1"):cells(i,10):Formula = "=sum(J" + string(linhaInicial) + ":J" + string(i - 1) + ")".

        c-excel:Worksheets("Plan1"):cells(i,11):Formula = "=sum(K" + string(linhaInicial) + ":K" + string(i - 1) + ")".

        c-excel:Worksheets("Plan1"):cells(i,12):Formula = "=sum(L" + string(linhaInicial) + ":L" + string(i - 1) + ")".

        c-excel:Worksheets("Plan1"):cells(i,13):Formula = "=sum(M" + string(linhaInicial) + ":M" + string(i - 1) + ")".
        c-excel:Worksheets("Plan1"):cells(1,15):Formula = "=sum(M" + string(linhaInicial) + ":M" + string(i - 1) + ")".
        c-excel:Worksheets("Plan1"):cells(1,16):Formula = "=sum(L" + string(linhaInicial) + ":L" + string(i - 1) + ")".




END.

run pi-finalizar in h-acomp.

if  tt-param.destino <> 4 then DO:

    if  tt-param.destino <> 1 then
    
        page stream str-rp.
    
    else do:
    
        if   tt-param.parametro = yes then
    
             page stream str-rp.
    
    end.
    
    if  tt-param.parametro then do:
    
    
       disp stream str-rp "SELEÄ«O" skip(01) with stream-io frame f-imp-sel.
       disp stream str-rp 
          c-cod-estabel-ini colon 20 "|< >|"   at 44 c-cod-estabel-fim no-label
            with stream-io side-labels overlay row 028 frame f-imp-sel.
    
       disp stream str-rp "CLASSIFICAÄ«O" skip(01) with stream-io frame f-imp-cla.
       disp stream str-rp "   PDP"
            with stream-io side-labels overlay row 028 frame f-imp-cla.
    
       put stream str-rp unformatted skip(1) "IMPRESS«O" skip(1).
    
       put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
       put stream str-rp unformatted skip "    " "Execuá∆o: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
       put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
       put stream str-rp unformatted skip "    " "Usu†rio : " tt-param.usuario.
    
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

    c-arquivo = c-arq + 'escr007' + STRING(time)+ '.xls'.

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



PROCEDURE Aging :
    DEFINE INPUT PARAMETER pDtCorte AS DATE.
    DEFINE INPUT PARAMETER pMercado AS CHAR.
    DEFINE INPUT PARAMETER pDataIni AS DATE.
    DEFINE INPUT PARAMETER pDataFim AS DATE.
    DEFINE INPUT PARAMETER pCodEspIni AS CHAR.
    DEFINE INPUT PARAMETER pCodEspFim AS CHAR.

    DEFINE VARIABLE vVencerAte30 AS DECIMAL NO-UNDO.
    DEFINE VARIABLE vVencerDe31a60 AS DECIMAL NO-UNDO.
    DEFINE VARIABLE vVencerDe61a90 AS DECIMAL NO-UNDO.
    DEFINE VARIABLE vVencerMais90 AS DECIMAL NO-UNDO.
    DEFINE VARIABLE vVencerVX AS DECIMAL NO-UNDO.

    DEFINE VARIABLE vVencidosAte30 AS DECIMAL NO-UNDO.
    DEFINE VARIABLE vVencidosDe31a60 AS DECIMAL NO-UNDO.
    DEFINE VARIABLE vVencidosDe61a90 AS DECIMAL NO-UNDO.
    DEFINE VARIABLE vVencidosMais90 AS DECIMAL NO-UNDO.

    DEFINE VARIABLE vValor AS DECIMAL NO-UNDO.
   
    DEFINE VARIABLE diasVencimento AS INTEGER NO-UNDO.
    DEFINE VARIABLE i-ge AS INTEGER     NO-UNDO.
    DEFINE VARIABLE c-arq-tmp AS CHARACTER   NO-UNDO.

    c-arq-tmp =  SESSION:TEMP-DIRECTORY + "titulos" + STRING(DAY(TODAY)) + STRING(MONTH(TODAY)) + STRING(YEAR(TODAY)) + 
                                replace(STRING(TIME,"HH:MM:SS"),":","") +  ".txt".
    OUTPUT TO VALUE(c-arq-tmp).
    PUT 
     pDtCorte FORMAT "99/99/9999" SKIP
     pMercado SKIP
     pDataIni FORMAT "99/99/9999" SKIP
     pDataFim FORMAT "99/99/9999" SKIP
     pCodEspIni SKIP
     pCodEspFim SKIP(2).

    ASSIGN vVencerAte30 = 0
                    vVencerDe31a60 = 0
                    vVencerDe61a90 = 0
                    vVencerMais90 = 0
                    vVencerVX = 0     
                    vVencidosAte30 = 0
                    vVencidosDe31a60 = 0
                    vVencidosDe61a90 = 0
                    vVencidosMais90 = 0
                    NOME-ABREV-ANT = "".

      
ASSIGN v-num-reg-lidos = 0.
    FOR each estabelec where  index(tt-param.c-cod-estabel-ini,estabelec.cod-estabel) > 0  
    no-lock,
     EACH tit_acr NO-LOCK USE-INDEX titacr_vencto  WHERE
        tit_acr.log_tit_acr_estordo = no  and
        /* tit_acr.cdn_cliente >= 19833 AND 
         tit_acr.cdn_cliente <= 19833 AND 
         */
        tit_acr.cod_estab   = estabelec.cod-estabel AND
        tit_acr.dat_vencto_tit_acr >= pDataIni AND
        tit_acr.dat_vencto_tit_acr <= pDataFim AND 
        (tit_acr.cod_espec_docto <> "AC" and tit_acr.cod_espec_docto <> "CP")  AND        
        ((tit_acr.dat_emis_docto <= pDtCorte AND tit_acr.dat_liquidac_tit_acr = 12/31/9999) OR 
         (tit_acr.dat_emis_docto <= pDtCorte AND tit_acr.dat_liquidac_tit_acr > pDtCorte )) AND         
        (tit_acr.cod_espec_docto >= pCodEspIni AND 
         tit_acr.cod_espec_docto <= pCodEspFim),
        EACH emitente NO-LOCK USE-INDEX codigo WHERE
            emitente.cod-emitente = tit_acr.cdn_cliente
            BREAK BY emitente.nome-abrev.

           
            assign v_cod_portador  = tit_acr.cod_portador
                   v_cod_cart_bcia = tit_acr.cod_cart_bcia.

           

         if pDtCorte  < today
            then do:
            run pi_retornar_portador_tit_acr_na_epoca (buffer tit_acr,
                                                       Input  pDtCorte ,
                                                       output v_cod_portador,
                                                       output v_cod_cart_bcia) /*pi_retornar_portador_tit_acr_na_epoca*/.
    
             if  v_cod_portador = "" then
                assign v_cod_portador  = tit_acr.cod_portador
                       v_cod_cart_bcia = tit_acr.cod_cart_bcia.

        end.

                    
              /*     if can-find (first  movto_tit_acr of tit_acr where  movto_tit_acr.ind_trans_acr =  "liquidaá∆o Transf Estab" no-lock) then do:
                   
                   
                   
                   end.
*/

        i-ge = 0.

        IF v_cod_cart_bcia <> "DES" THEN DO:
                        
                FOR FIRST it-nota-fisc WHERE it-nota-fisc.nr-nota-fis  = tit_acr.cod_tit_acr AND
                                     it-nota-fisc.serie        = tit_acr.cod_ser_docto  AND
                                     it-nota-fisc.cod-estabel  = tit_acr.cod_estab  NO-LOCK,
                    FIRST ITEM FIELDS (ge-codigo) WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK.
                    i-ge = item.ge-codigo.
                END.
        
                IF i-ge = 0 THEN  DO:
                
                    FOR FIRST it-nota-fisc WHERE it-nota-fisc.nr-nota-fis  = tit_acr.cod_tit_acr AND
                                         it-nota-fisc.serie        = "20"  AND
                                         it-nota-fisc.cod-estabel  = tit_acr.cod_estab AND
                                         it-nota-fisc.nome-ab-cli   = emitente.nome-abrev NO-LOCK,
                        FIRST ITEM FIELDS (ge-codigo) WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK.
                        i-ge = item.ge-codigo.
                    END.
                END.
        
                FIND FIRST val_tit_acr OF tit_acr NO-LOCK NO-ERROR.
        
                IF i-ge = 0 AND AVAIL val_tit_acr THEN DO: 
                
                    FOR FIRST unid_negoc WHERE unid_negoc.cod_unid_negoc = val_tit_acr.cod_unid_negoc AND
                       substring(unid_negoc.cod_unid_negoc,1,1) <> "4" AND (INDEX(unid_negoc.des_unid_negoc,"bopp") > 0 OR INDEX(unid_negoc.des_unid_negoc,"film") > 0)  NO-LOCK.
                
                        i-ge = 46.
                    
                    END.
                END.
        
               
        
        
                IF i-ge = 0 THEN  DO:
                
                    FOR FIRST it-nota-fisc WHERE it-nota-fisc.nr-nota-fis  = tit_acr.cod_tit_acr AND
                                         it-nota-fisc.serie        = "20"  AND
                                         (it-nota-fisc.cod-estabel  = "434" OR it-nota-fisc.cod-estabel  = "442") AND
                                         it-nota-fisc.nome-ab-cli   = emitente.nome-abrev NO-LOCK,
                        FIRST ITEM FIELDS (ge-codigo) WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK.
                        i-ge = item.ge-codigo.
                    END.
                END.
        
                IF i-ge = 0 THEN  DO:
                
                    FOR FIRST it-nota-fisc WHERE it-nota-fisc.nr-nota-fis  = tit_acr.cod_tit_acr AND
                                         it-nota-fisc.serie        = "20"  AND
                                         (it-nota-fisc.cod-estabel  = "432" OR it-nota-fisc.cod-estabel  = "443") AND /*solic-318*/
                                         it-nota-fisc.nome-ab-cli   = emitente.nome-abrev NO-LOCK,
                        FIRST ITEM FIELDS (ge-codigo) WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK.
                        i-ge = item.ge-codigo.
                    END.
                END.
        
                IF i-ge = 0 THEN  DO:
                
                    FOR FIRST it-nota-fisc WHERE it-nota-fisc.nr-nota-fis  = tit_acr.cod_tit_acr AND
                                         it-nota-fisc.serie        = "20"  AND
                                         (it-nota-fisc.cod-estabel  = "422" OR it-nota-fisc.cod-estabel  = "412") AND
                                         it-nota-fisc.nome-ab-cli   = emitente.nome-abrev NO-LOCK,
                        FIRST ITEM FIELDS (ge-codigo) WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK.
                        i-ge = item.ge-codigo.
                    END.
                END.
        
                IF i-ge = 0 THEN  DO:
                
                    FOR FIRST it-nota-fisc WHERE it-nota-fisc.nr-nota-fis  = tit_acr.cod_tit_acr AND
                                         it-nota-fisc.serie        = "20"  AND
                                         it-nota-fisc.cod-estabel  = "424" AND
                                         it-nota-fisc.nome-ab-cli   = emitente.nome-abrev NO-LOCK,
                        FIRST ITEM FIELDS (ge-codigo) WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK.
                        i-ge = item.ge-codigo.
                    END.
                END.
        
                 IF i-ge = 0 THEN DO: 
                    IF tit_acr.cod_estab = "422" OR tit_acr.cod_estab = "412" OR tit_acr.cod_estab = "434"  OR tit_acr.cod_estab = "424" OR tit_acr.cod_estab = "442" THEN /*solic-318*/
                        i-ge = 49.
        
                END.
        

                IF i-ge <> 0 AND (i-ge < 40 OR i-ge > 49) THEN i-ge = 0 .
        
        END.
                  
   

        IF NOT ((pMercado = "int" AND emitente.estado <> "EX") OR (pMercado <> "int" AND emitente.estado = "EX") OR (pMercado = "A")) THEN
              i-ge = 0.
        
        assign v-num-reg-lidos = v-num-reg-lidos + 1.
           run pi-acompanhar in h-acomp(input "Lendo Movimentos: " + string(v-num-reg-lidos)).

    
        IF emitente.nome-abrev <> NOME-ABREV-ANT THEN DO:

            NOME-ABREV-ANT = emitente.nome-abrev . 
            ASSIGN vVencerAte30 = 0
                vVencerDe31a60 = 0
                vVencerDe61a90 = 0
                vVencerMais90 = 0
                vVencerVX = 0     
                vVencidosAte30 = 0
                vVencidosDe31a60 = 0
                vVencidosDe61a90 = 0
                vVencidosMais90 = 0.
                
        END.
    

        IF i-ge <> 0 THEN DO:
        

            ASSIGN diasVencimento = tit_acr.dat_vencto_tit_acr - pDtCorte.

              ASSIGN d-cotacao = 1.
              
                          IF  tit_acr.cod_indic_econ <> "REAL" THEN DO:
    
                                dt-cot = pDtCorte /* tit_acr.dat_emis_docto*/  .
                                REPEAT:
                                    IF dt-cot < tit_acr.dat_emis_docto - 35 THEN LEAVE.
    
                                    FIND FIRST  cotac_parid WHERE 
                                         cotac_parid.cod_indic_econ_base  =  tit_acr.cod_indic_econ AND
                                         cotac_parid.ind_tip_cotac_parid  = "real" and
                                         cotac_parid.cod_indic_econ_idx = "real"  and
                                         cotac_parid.dat_cotac_indic_econ = dt-cot NO-LOCK no-error.
                                    IF AVAIL cotac_parid THEN  LEAVE.
                                    dt-cot = dt-cot - 1.
    
                                END.
    
                                IF NOT AVAIL cotac_parid THEN   DO:
                                
                                  FIND FIRST  cotac_parid WHERE 
                                         cotac_parid.cod_indic_econ_base  =  tit_acr.cod_indic_econ AND
                                         cotac_parid.ind_tip_cotac_parid  = "real" and
                                         cotac_parid.dat_cotac_indic_econ >= dt-cot NO-LOCK no-error.
                                    IF not AVAIL cotac_parid THEN do:
   
                                       MESSAGE "Documento:"  tit_acr.num_id_tit_acr " - Falta cotacao para o dia "  tit_acr.dat_emis_docto
                                           VIEW-AS ALERT-BOX INFO BUTTONS OK.
                                           ASSIGN d-cotacao = 999999999999.
                                        
                                    end.
                                     ELSE
    
                                        ASSIGN
                                           d-cotacao  =   cotac_parid.val_cotac_indic_econ.
                                END.
                                ELSE
    
                                ASSIGN
                                   d-cotacao  =   cotac_parid.val_cotac_indic_econ.
    
    
                        END.


            ASSIGN vValor = tit_acr.val_sdo_tit_acr .
           
            assign vValor = vValor / d-cotacao.
            
            run pi_verifica_movtos_tit_acr_em_aberto (Input pDtCorte , INPUT-OUTPUT vValor) /*pi_verifica_movtos_tit_acr_em_aberto*/.
            
            
          /*  IF tit_acr.val_origin_tit_acr <> tit_acr.val_sdo_tit_acr THEN DO:
                 FOR EACH mgmov.mov-tit OF tit_acr WHERE 
                          (mgmov.mov-tit.cod-esp = "VX" or mgmov.mov-tit.flag-contab) AND
                          mgmov.mov-tit.dt-trans > pDtCorte AND
                          mgmov.mov-tit.sit-titulo <> 2
                          NO-LOCK.

                     IF mgmov.mov-tit.lancamento = 2 THEN 
                         ASSIGN vValor = vValor - (mgmov.mov-tit.vl-baixa / d-cotacao).
                     ELSE
                         ASSIGN vValor = vValor + (mgmov.mov-tit.vl-baixa / d-cotacao).
                 END.

            END.
            */          
            desc-aging = "".
            
            IF tit_acr.cod_espec_docto = "VE" or tit_acr.cod_espec_docto = "ZV" /*OR  tit_acr.modalidade = 7*/  THEN DO:
                ASSIGN vVencerVX = vVencerVX + vValor
                       desc-aging =  "ZV".
            END.
            ELSE DO:
    
                IF diasVencimento >= 0 AND diasVencimento <= 30 THEN
                    ASSIGN vVencerAte30 = vVencerAte30 + vValor
                            desc-aging = "VencerAte30".
    
                IF diasVencimento > 30 AND diasVencimento <= 60 THEN
                    ASSIGN vVencerDe31a60 = vVencerDe31a60 + vValor
                            desc-aging = "VencerDe31a60"                    .
        
                IF diasVencimento > 60 AND diasVencimento <= 90 THEN
                    ASSIGN vVencerDe61a90 = vVencerDe61a90 + vValor
                            desc-aging = "VencerDe61a90"                    .
        
                IF diasVencimento > 90 THEN
                    ASSIGN vVencerMais90 = vVencerMais90 + vValor
                            desc-aging = "VencerMais90"                    .
        
                /* -------------------------------- */
        
                IF diasVencimento < 0 AND diasVencimento >= -30 THEN
                    ASSIGN vVencidosAte30 = vVencidosAte30 + vValor
                               desc-aging = "VencidosAte30 "  .
                IF diasVencimento < -30 AND diasVencimento >= -60 THEN
                    ASSIGN vVencidosDe31a60 = vVencidosDe31a60 + vValor
                               desc-aging = "VencidosDe31a60"  .
        
                IF diasVencimento < -60 AND diasVencimento >= -90 THEN
                    ASSIGN vVencidosDe61a90 = vVencidosDe61a90 + vValor
                               desc-aging = "VencidosDe61a90"  .
        
                IF diasVencimento < -90 THEN
                    ASSIGN vVencidosMais90 = vVencidosMais90 + vValor
                               desc-aging = "VencidosMais90"  .
    
            END.
        END.
    

          
        IF LAST-OF (emitente.nome-abrev) THEN DO:


                  IF
                    vVencerAte30        +
                    vVencerDe31a60      +
                    vVencerDe61a90      +
                    vVencerMais90       +
                    vVencerVX           +
                    vVencidosAte30      +
                    vVencidosDe31a60    +
                    vVencidosDe61a90    +
                    vVencidosMais90     > 0 THEN

                 DO:
                   /*IF emitente.nome-abrev = "Arflex" THEN
                     MESSAGE vVencerAte30 vVencidosAte30
                         VIEW-AS ALERT-BOX INFO BUTTONS OK.
                     */

                CREATE tAging.
                ASSIGN Cliente = emitente.nome-abrev
                    tAging.vencerAte30     = vVencerAte30
                    tAging.vencerDe31a60   = vVencerDe31a60
                    tAging.vencerDe61a90   = vVencerDe61a90
                    tAging.vencerMais90    = vVencerMais90
                    tAging.vencerVX        = vVencerVX
                    tAging.vencidosAte30   = vVencidosAte30
                    tAging.vencidosDe31a60 = vVencidosDe31a60
                    tAging.vencidosDe61a90 = vVencidosDe61a90
                    tAging.vencidosMais90  = vVencidosMais90
                    tAging.estado = emitente.estado
                    .
                END.
        END.
            
        IF i-ge <> 0 THEN DO:
            PUT
                tit_acr.cod_tit_acr
                tit_acr.cod_parcela
                v_cod_cart_bcia
                 tit_acr.cod_cart_bcia
                emitente.cod-emitente
                emitente.nome-abrev
                tit_acr.nom_abrev
                tit_acr.dat_liquidac_tit_acr
                tit_acr.cod_espec_docto
                vValor
                diasVencimento 
                string (tit_acr.dat_vencto_tit_acr,"99/99/9999") format "x(10)"
                desc-aging format "x(20)"SKIP.
        END.

    END.

    OUTPUT CLOSE.

    DOS SILENT COPY VALUE(c-arq-tmp) v:\temp.

END PROCEDURE.

return 'OK'.

/* fim do programa */


/*****************************************************************************
** Procedure Interna.....: pi_verifica_movtos_tit_acr_em_aberto
** Descricao.............: pi_verifica_movtos_tit_acr_em_aberto
** Criado por............: Uno
** Criado em.............: 02/01/1997 16:49:44
** Alterado por..........: fut1228
** Alterado em...........: 01/11/2006 10:21:28
*****************************************************************************/
PROCEDURE pi_verifica_movtos_tit_acr_em_aberto:

    /************************ Parameter Definition Begin ************************/

    def Input param p_dat_tit_acr_aber
        as date
        format "99/99/9999"
        no-undo.

    DEFINE INPUT-OUTPUT PARAM val_sdo_tit_acr  AS DECIMAL    NO-UNDO.

    /************************* Parameter Definition End *************************/

    /************************** Buffer Definition Begin *************************/

     def var v_num_multiplic as dec no-undo.
    
    def buffer b_movto_tit_acr_avo
        for movto_tit_acr.
     
     
    def buffer b_movto_tit_acr_pai
        for movto_tit_acr.
     
     
    def buffer b_val_tit_acr
        for val_tit_acr.
     


    /*************************** Buffer Definition End **************************/

    /************************* Variable Definition Begin ************************/

    def var v_val_unid_negoc
        as decimal
        format "->>,>>>,>>>,>>9.99":U
        decimals 2
        no-undo.
    def var v_cod_estab                      as character       no-undo. /*local*/
    def var v_log_liq_perda                  as logical         no-undo. /*local*/
    def var v_num_id_movto_tit_acr           as integer         no-undo. /*local*/
    def var l-transf                          as logical         no-undo.

    /************************** Variable Definition End *************************/

    /* ** IMPORTANTE: AS PIs ABAIXO DEVEM SER MANTIDAS EM SINCRONIA (Barth)
    *
    *    pi_verifica_movtos_tit_acr           (RAZAO)
    *    pi_verifica_movtos_tit_acr_em_aberto (TIT ABERTO)
    *    pi_sit_acr_acessar_movto_tit_acr     (SITUACAO ACR)
    */

    /* ** TRATAMENTO PARA MOVIMENTO DE LIQUIDAÄ«O PERDA DEDUTIVEL / ESTORNO DE TITULO ***/
    /* ** DETALHE: ESTES MOVIMENTOS N«O T“M VAL_MOVTO_TIT_ACR.           (Barth) ***/
    find first b_movto_tit_acr_pai
        where b_movto_tit_acr_pai.cod_estab         = tit_acr.cod_estab
        and   b_movto_tit_acr_pai.num_id_tit_acr    = tit_acr.num_id_tit_acr
        and   b_movto_tit_acr_pai.dat_transacao    <= p_dat_tit_acr_aber
        and   b_movto_tit_acr_pai.log_movto_estordo = no
        and  (b_movto_tit_acr_pai.ind_trans_acr     = "Liquidaá∆o Perda Dedut°vel" /*l_liquidacao_perda_dedutivel*/ 
        or    b_movto_tit_acr_pai.ind_trans_acr     = "Estorno de T°tulo" /*l_estorno_de_titulo*/ )
        use-index mvtttcr_id no-lock no-error.
    if  avail b_movto_tit_acr_pai then do:
        assign v_log_liq_perda = YES
        val_sdo_tit_acr       = 0
        .
        
    end.

l-transf = no.
    /* ** VOLTA O SALDO DO T÷TULO, DE ACORDO COM OS MOVTOS POSTERIORES A DATA DE CORTE ***/
    movto_block:
    
    
    
    for each movto_tit_acr no-lock
        where movto_tit_acr.cod_estab      = tit_acr.cod_estab
        and   movto_tit_acr.num_id_tit_acr = tit_acr.num_id_tit_acr
        and   movto_tit_acr.dat_transacao  > p_dat_tit_acr_aber:




        

        if    movto_tit_acr.ind_trans_acr  = "Alteraá∆o Data Vencimento" /*l_alteracao_data_vencimento*/ 
        or  movto_tit_acr.ind_trans_acr  = "Alteraá∆o n∆o Cont†bil" /*l_alteracao_nao_contabil*/ 
        or  movto_tit_acr.ind_trans_acr  = "Implantaá∆o" /*l_implantacao*/ 
        or  movto_tit_acr.ind_trans_acr  = "Implantaá∆o a CrÇdito" /*l_implantacao_a_credito*/ 
/*        or  movto_tit_acr.ind_trans_acr  = "Transf Estabelecimento" /*l_transf_estabelecimento*/ */
        or  movto_tit_acr.ind_trans_acr  = "Renegociaá∆o" /*l_renegociacao*/ 
        or  movto_tit_acr.ind_trans_acr  = "Liquidaá∆o Perda Dedut°vel" /*l_liquidacao_perda_dedutivel*/ 
        or  movto_tit_acr.ind_trans_acr  = "Estorno de T°tulo" /*l_estorno_de_titulo*/  then
            next movto_block.




        if movto_tit_acr.ind_trans_acr  = "Transf Estabelecimento" then l-transf = yes.
        /* ** LIQUIDAÄ«O AP‡S PERDA DEDUT÷VEL N«O CONTA, N«O DEVE VOLTAR SALDO ***/
        if  v_log_liq_perda = yes
        and   movto_tit_acr.log_recuper_perda = yes
              then
            next movto_block.

        /* ** IGNORA ESTORNADOS QUE N«O CONTABILIZAM ***/
        if   movto_tit_acr.log_ctbz_aprop_ctbl = no
        and (movto_tit_acr.ind_trans_acr      begins "Estorno" /*l_estorno*/ 
        or   movto_tit_acr.log_movto_estordo  = yes) then do:
            /* ** A LIQUIDAÄ«O DA ANTECIPAÄ«O NUNCA CONTABILIZA, VERIFICA A DA DUPLICATA (Barth) ***/
            if  tit_acr.ind_tip_espec_docto        = "Antecipaá∆o" /*l_antecipacao*/ 
            and (movto_tit_acr.ind_trans_acr_abrev = "ELIQ" /*l_eliq*/ 
            or   movto_tit_acr.ind_trans_acr_abrev = "LIQ" /*l_liq*/ ) then do:
                find b_movto_tit_acr_pai
                    where b_movto_tit_acr_pai.cod_estab            = movto_tit_acr.cod_estab_tit_acr_pai
                    and   b_movto_tit_acr_pai.num_id_movto_tit_acr = movto_tit_acr.num_id_movto_tit_acr_pai
                    no-lock no-error.
                if  movto_tit_acr.ind_trans_acr_abrev = "ELIQ" /*l_eliq*/  then do:
                    /* ** O PAI DO ESTORNO ê A LIQUIDAÄ«O. O PAI DA LIQUIDAÄ«O ê A LIQUIDAÄ«O DA DUPLICATA ***/
                    find b_movto_tit_acr_avo
                        where b_movto_tit_acr_avo.cod_estab            = b_movto_tit_acr_pai.cod_estab_tit_acr_pai
                        and   b_movto_tit_acr_avo.num_id_movto_tit_acr = b_movto_tit_acr_pai.num_id_movto_tit_acr_pai
                        no-lock no-error.
                    if  b_movto_tit_acr_avo.log_ctbz_aprop_ctbl = no then
                        next movto_block.
                end.
                else
                    if  b_movto_tit_acr_pai.log_ctbz_aprop_ctbl = no then
                        next movto_block.
            end.
            else
                next movto_block.
        end.

        if  movto_tit_acr.ind_trans_acr begins "Estorno" /*l_estorno*/  then do:
            assign v_num_id_movto_tit_acr = movto_tit_acr.num_id_movto_tit_acr_pai
                   v_cod_estab            = movto_tit_acr.cod_estab_tit_acr_pai
                   v_num_multiplic        = -1. /* O estorno ser† subtra°do do saldo, por esse motivo o 
                                                   v_num_multiplic ser† multiplicado pela cotaá∆o */
        end.
        else do:
            assign v_num_id_movto_tit_acr = movto_tit_acr.num_id_movto_tit_acr
                   v_cod_estab            = movto_tit_acr.cod_estab
                   v_num_multiplic        = 1.
        end.

        /* ** VOLTA ESTORNO DE PERDAS DEDUT÷VEIS (Barth) ***/
        if  movto_tit_acr.ind_trans_acr = "Estorno de Liquidacao" /*l_estorno_de_liquidacao*/  then do:
            find b_movto_tit_acr_pai
                where b_movto_tit_acr_pai.cod_estab            = movto_tit_acr.cod_estab_tit_acr_pai
                and   b_movto_tit_acr_pai.num_id_movto_tit_acr = movto_tit_acr.num_id_movto_tit_acr_pai
                no-lock no-error.
            if  avail b_movto_tit_acr_pai
            and b_movto_tit_acr_pai.ind_trans_acr = "Liquidaá∆o Perda Dedut°vel" /*l_liquidacao_perda_dedutivel*/  then do:
                if  b_movto_tit_acr_pai.dat_transacao <= p_dat_tit_acr_aber then do:
                    for each aprop_ctbl_acr no-lock
                        where aprop_ctbl_acr.cod_estab             = b_movto_tit_acr_pai.cod_estab
                        and   aprop_ctbl_acr.num_id_movto_tit_acr  = b_movto_tit_acr_pai.num_id_movto_tit_acr
                       /* and   aprop_ctbl_acr.cod_unid_negoc       >= v_cod_unid_negoc_ini
                        and   aprop_ctbl_acr.cod_unid_negoc       <= v_cod_unid_negoc_fim*/
                        and   aprop_ctbl_acr.ind_natur_lancto_ctbl = 'CR',
                        each val_aprop_ctbl_acr no-lock
                        where val_aprop_ctbl_acr.cod_estab             = aprop_ctbl_acr.cod_estab
                        and   val_aprop_ctbl_acr.num_id_aprop_ctbl_acr = aprop_ctbl_acr.num_id_aprop_ctbl_acr
                        and   val_aprop_ctbl_acr.cod_finalid_econ      = "corrente":
 
                          

                        assign val_sdo_tit_acr = val_sdo_tit_acr - val_aprop_ctbl_acr.val_aprop_ctbl.
                                
                    end.
                end.
                next movto_block.
            end.
        end.


        do:
                        /* GRAVA O VALOR ORIGINAL E SALDO DO T÷TULO NA FINALIDADE ORIGINAL */
            val_block:
            for each val_movto_tit_acr no-lock
                where val_movto_tit_acr.cod_estab            = v_cod_estab
                and   val_movto_tit_acr.num_id_movto_tit_acr = v_num_id_movto_tit_acr
                and   val_movto_tit_acr.cod_finalid_econ     = "corrente":


                /* Begin_Include: i_recompoe_saldo_titulo_acr */
                /* code_block: */
                case movto_tit_acr.ind_trans_acr:
                    when "Acerto Valor a DÇbito" /*l_acerto_valor_a_debito*/         or
                    when "Acerto Valor a Maior" /*l_acerto_valor_a_maior*/         or
                    when "Estorno Acerto Val DÇbito" /*l_estorno_acerto_val_debito*/         or
                    when "Estorno Acerto Val Maior" /*l_estorno_acerto_val_maior*/ then
                        assign val_sdo_tit_acr = val_sdo_tit_acr - ( val_movto_tit_acr.val_ajust_val_tit_acr / (1 * v_num_multiplic) ).

                    when "Acerto Valor a CrÇdito" /*l_acerto_valor_a_credito*/         or
                    when "Acerto Valor a Menor" /*l_acerto_valor_a_menor*/         or
                    when "Estorno Acerto Val CrÇdito" /*l_estorno_acerto_val_credito*/         or
                    when "Estorno Acerto Val Menor" /*l_estorno_acerto_val_menor*/ then
                        assign val_sdo_tit_acr = val_sdo_tit_acr + ( val_movto_tit_acr.val_ajust_val_tit_acr / (1 * v_num_multiplic) ).

                    when "Liquidaá∆o" /*l_liquidacao*/         or
                    when "Devoluá∆o" /*l_devolucao*/         or
                    when "Liquidaá∆o Enctro Ctas" /*l_liquidacao_enctro_ctas*/         or
                    when "Estorno de Liquidacao" /*l_estorno_de_liquidacao*/ then
                        assign val_sdo_tit_acr = val_sdo_tit_acr + ( ( val_movto_tit_acr.val_liquidac_tit_acr 
                                                         + val_movto_tit_acr.val_abat_tit_acr
                                                         + val_movto_tit_acr.val_desconto ) / (1 * v_num_multiplic) ).

                    when "Liquidaá∆o Renegociac" /*l_liquidacao_renegociac*/         or
                    when "Estorno Liquidacao Subst" /*l_estorno_liquidacao_subst*/         or
                    when "Estorno Liquid Renegociac" /*l_estorno_liquid_renegociac*/ then
                        assign val_sdo_tit_acr = val_sdo_tit_acr + ( val_movto_tit_acr.val_saida_subst_nf_dupl / (1 * v_num_multiplic) ).

                    when "Liquidaá∆o Transf Estab" /*l_liquidacao_transf_estab*/         or
                    when "Estorno Liquid Transf Estab" /*l_estorno_liquid_transf_estab*/ then
                        assign val_sdo_tit_acr = val_sdo_tit_acr + ( val_movto_tit_acr.val_transf_estab / (1 * v_num_multiplic) ).


        
                    when "Correá∆o de Valor" /*l_correcao_de_valor*/         or
                    when "Correá∆o Valor na Liquidac" /*l_correcao_valor_na_liquidac*/         or
                    when "Estorno Correá∆o Valor" /*l_estorno_correcao_valor*/         or
                    when "Estorno Correá∆o Val Liquidac" /*l_estorno_correcao_val_liquidac*/ then
                        assign val_sdo_tit_acr = val_sdo_tit_acr - ( ( val_movto_tit_acr.val_variac_cambial
                                                         + val_movto_tit_acr.val_acerto_cmcac
                                                         + val_movto_tit_acr.val_ganho_perda_cm
                                                         + val_movto_tit_acr.val_ganho_perda_projec ) / (1 * v_num_multiplic) ).

                    when "Transf Unidade Neg¢cio" /*l_transf_unidade_negocio*/         or
                    when "Estorno Transf Unid Negoc" /*l_estorno_transf_unid_negoc*/ then
                        assign val_sdo_tit_acr = val_sdo_tit_acr + ( ( val_movto_tit_acr.val_saida_transf_unid_negoc
                                                         - val_movto_tit_acr.val_entr_transf_unid_negoc ) / (1 * v_num_multiplic) ).
                end /* case code_block */.
                /* End_Include: i_recompoe_saldo_titulo_acr */

                         if movto_tit_acr.ind_trans_acr  = "Estorno Liquid Transf Estab" then l-transf = no.
                 
            end.
        end.
    end.
    
    if l-transf then val_sdo_tit_acr = 0.
    

END PROCEDURE. /* pi_verifica_movtos_tit_acr_em_aberto */

PROCEDURE pi_retornar_portador_tit_acr_na_epoca:

    /************************ Parameter Definition Begin ************************/

    def param buffer p_tit_acr
        for tit_acr.
    def Input param p_dat_tit_acr_aber
        as date
        format "99/99/9999"
        no-undo.
    def output param p_cod_portador
        as character
        format "x(5)"
        no-undo.
    def output param p_cod_cart_bcia
        as character
        format "x(3)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************** Buffer Definition Begin *************************/

    &if "{&emsfin_version}" >= "5.01" &then
    def buffer b_movto_tit_acr_aux
        for movto_tit_acr.
    &endif


    /*************************** Buffer Definition End **************************/

    /************************* Variable Definition Begin ************************/

    def var v_dat_aux                        as date            no-undo. /*local*/
    def var v_dat_aux_2                      as date            no-undo. /*local*/
    def var v_hra_aux                        as character       no-undo. /*local*/


    /************************** Variable Definition End *************************/

    assign v_dat_aux   = &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/0001 &ENDIF
           v_dat_aux_2 = &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/0001 &ENDIF
           v_hra_aux   = "".

    for each b_movto_tit_acr_aux no-lock use-index mvtttcr_id
        where b_movto_tit_acr_aux.cod_estab      = p_tit_acr.cod_estab
        and   b_movto_tit_acr_aux.num_id_tit_acr = p_tit_acr.num_id_tit_acr
        and  (b_movto_tit_acr_aux.ind_trans_acr = "Alteraá∆o n∆o Cont†bil" /*l_alteracao_nao_contabil*/ 
        or    b_movto_tit_acr_aux.ind_trans_acr = "Implantaá∆o" /*l_implantacao*/ 
        or    b_movto_tit_acr_aux.ind_trans_acr = "Transf Estabelecimento" /*l_transf_estabelecimento*/ 
        or    b_movto_tit_acr_aux.ind_trans_acr = "Renegociaá∆o" /*l_renegociacao*/ 
        or    b_movto_tit_acr_aux.ind_trans_acr = "Desconto Banc†rio" /*l_desconto_bancario*/ )
        and   b_movto_tit_acr_aux.dat_transacao <= p_dat_tit_acr_aber:

        if   b_movto_tit_acr_aux.dat_transacao > v_dat_aux_2
        or  (b_movto_tit_acr_aux.dat_transacao = v_dat_aux_2
        and (b_movto_tit_acr_aux.dat_gerac_movto > v_dat_aux
        or  (b_movto_tit_acr_aux.dat_gerac_movto = v_dat_aux
        and  b_movto_tit_acr_aux.hra_gerac_movto > v_hra_aux)))
        or  (b_movto_tit_acr_aux.dat_transacao <= v_dat_aux_2
        and (b_movto_tit_acr_aux.dat_gerac_movto > v_dat_aux
        or  (b_movto_tit_acr_aux.dat_gerac_movto = v_dat_aux
        and  b_movto_tit_acr_aux.hra_gerac_movto > v_hra_aux))) then
            assign v_dat_aux_2     = b_movto_tit_acr_aux.dat_transacao
                   v_dat_aux       = b_movto_tit_acr_aux.dat_gerac_movto
                   v_hra_aux       = b_movto_tit_acr_aux.hra_gerac_movto
                   p_cod_portador  = b_movto_tit_acr_aux.cod_portador
                   p_cod_cart_bcia = b_movto_tit_acr_aux.cod_cart_bcia.
    end.

    if  p_cod_portador = "" then
        assign p_cod_portador  = p_tit_acr.cod_portador
               p_cod_cart_bcia = p_tit_acr.cod_cart_bcia.
END PROCEDURE. /* pi_retornar_portador_tit_acr_na_epoca */
