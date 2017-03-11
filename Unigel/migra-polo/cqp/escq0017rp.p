/*****************************************************************************
**
**       Programa: escq0017rp.p
**
**       Data....: 12/09/2005
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: CONTROLE DE MATERIAL COM RESTRIÄ«O 
**
**       Vers∆o..: 1.00.000 - jrrcampos
**
**       OBS.....: 
**
*******************************************************************************/
define variable c-prog-gerado as character no-undo initial "escq0017RP".
def buffer empresa for mgmulti.empresa.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

/****************** Definiá∆o de Tabelas Tempor†rias do Relat¢rio **********************/

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
    field c-cod-estabel-ini like ped-venda.cod-estabel
    field c-cod-estabel-fim like ped-venda.cod-estabel
    field c-nr-data-ini     like movto-estoq.dt-trans
    field c-nr-data-fim     like movto-estoq.dt-trans
    field c-nr-familia-ini  like item.fm-codigo
    field c-nr-familia-fim  like item.fm-codigo
    field c-nr-lote-ini     like movto-estoq.lote
    field c-nr-lote-fim     like movto-estoq.lote
    field c-nr-linha-ini    AS INTEGER 
    field c-nr-linha-fim    AS INTEGER
    field c-it-codigo-ini   like item.it-codigo
    field c-it-codigo-fim   like item.it-codigo
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


/* Vari†veis usadas para gerar planilha excel. */

DEF VAR h-acomp           AS HANDLE              NO-UNDO.
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


/****************** FIM INCLUDE COM VARIµVEIS GLOBAIS *********************/
/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 

/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 

def new shared var c-cod-estabel-ini like ped-venda.cod-estabel format "x(3)" initial "422" no-undo.
def new shared var c-cod-estabel-fim like ped-venda.cod-estabel format "x(3)" initial "ZZZ" no-undo.
def new shared var c-nr-data-ini     like movto-estoq.dt-trans  format "99/99/9999" initial "01/01/2004" no-undo.
def new shared var c-nr-data-fim     like movto-estoq.dt-trans  format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var c-nr-familia-ini  like item.fm-codigo        FORMAT "x(10)" INITIAL "" NO-UNDO.
def new shared var c-nr-familia-fim  like item.fm-codigo        FORMAT "x(10)" INITIAL "ZZZZZZZZZZ" NO-UNDO.
def new shared var c-nr-lote-ini     like movto-estoq.lote      FORMAT "x(13)" INITIAL "" NO-UNDO.
def new shared var c-nr-lote-fim     like movto-estoq.lote      FORMAT "x(13)" INITIAL "ZZZZZZZZZZ" NO-UNDO.
def new shared var c-nr-linha-ini    AS INTEGER                 FORMAT ">>9"   INITIAL 0 NO-UNDO.
def new shared var c-nr-linha-fim    AS INTEGER                 FORMAT ">>9"   INITIAL 999 NO-UNDO.
def new shared var c-it-codigo-ini   like item.it-codigo        FORMAT "x(16)" INITIAL "" NO-UNDO.
def new shared var c-it-codigo-fim   like item.it-codigo        FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZ" NO-UNDO.

/****************** Definiáao de Vari†veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Definiáao de Vari†veis Campo do Layout do Relat¢rio **********************/ 
DEFINE VARIABLE faixa-jr        AS CHARACTER   FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE motivo-jr       AS CHARACTER   FORMAT "x(60)" NO-UNDO.
DEFINE VARIABLE motivo-jr1      AS CHARACTER   FORMAT "x(90)" NO-UNDO.
DEFINE VARIABLE situacao-jr     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE status-jr       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE saldo-jr        AS DECIMAL     NO-UNDO.

/****************** Definiáao de Vari†veis do Relat¢rio N∆o Pedidas em Tela ******************/ 

/****************** Definiáao de Vari†veis de Total do Relat¢rio *****************************/ 

/****************** Definiáao de Vari†veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Definiáao de Vari†veis de Processamento do Relat¢rio *********************/

def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.

/****************** Definiáao de Forms do Relat¢rio 132 Colunas ***************************************/ 

FORM polo-lote-restricao.lote           COLUMN-LABEL "Mill Roll"         FORMAT "x(10)"       AT 001
     polo-lote-restricao.it-codigo      COLUMN-LABEL "Tipo de Filme"     FORMAT "x(16)"       AT 012
     polo-lote-restricao.peso-total     COLUMN-LABEL "Peso total"        FORMAT ">>>>>>>9.99" AT 029
     polo-lote-restricao.peso-rejeitado COLUMN-LABEL "Peso Rejt."        FORMAT ">>>>>>>9.99" AT 040
     polo-lote-restricao.larg-rejeitada COLUMN-LABEL "Larg Rejt."        FORMAT ">>>>>>>9.99" AT 051
     faixa-jr                           COLUMN-LABEL "Faixa Rejeitada"   FORMAT "x(40)"       AT 062
     motivo-jr                          COLUMN-LABEL "Motivo"            FORMAT "x(60)"       AT 103
     situacao-jr                        COLUMN-LABEL "Liberado?"         FORMAT "x(9)"        AT 164
     status-jr                          COLUMN-LABEL "Status"            FORMAT "x(20)"       AT 174
     with down width 200 no-box stream-io frame f-relat-09-132.


form HEADER
    fill("-", 200) format "x(132)" SKIP 
    WITH DOWN WIDTH 200 NO-BOX STREAM-IO FRAME f-relat-linha.

form HEADER
    fill(" ", 200) format "x(132)" SKIP 
    WITH DOWN WIDTH 200 NO-BOX STREAM-IO FRAME f-relat-branco.

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

assign c-programa     = "escq0017rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "CONTROLE DE MATERIAL COM RESTRIÄ«O"
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
    "Per°odo:" i-numper-x at 08 "-"
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

    assign c-cod-estabel-ini  = tt-param.c-cod-estabel-ini
           c-cod-estabel-fim  = tt-param.c-cod-estabel-fim
           c-nr-data-ini      = tt-param.c-nr-data-ini    
           c-nr-data-fim      = tt-param.c-nr-data-fim    
           c-nr-familia-ini   = tt-param.c-nr-familia-ini 
           c-nr-familia-fim   = tt-param.c-nr-familia-fim 
           c-nr-lote-ini      = tt-param.c-nr-lote-ini    
           c-nr-lote-fim      = tt-param.c-nr-lote-fim    
           c-nr-linha-ini     = tt-param.c-nr-linha-ini   
           c-nr-linha-fim     = tt-param.c-nr-linha-fim   
           c-it-codigo-ini    = tt-param.c-it-codigo-ini  
           c-it-codigo-fim    = tt-param.c-it-codigo-fim.  

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


run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

IF tt-param.destino = 4 THEN DO:
    
    /* Cria Aplicaá∆o do Excel */

    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

    ASSIGN c-modelo-planilha = search("modelos/mod-escq0017.xls") 
           c-arq             = SESSION:TEMP-DIRECTORY.

    RUN pi-cria-planilha.


END.

ASSIGN i-linha = 5.

FOR Each polo-lote-restricao no-lock WHERE
         polo-lote-restricao.cod-estabel =  c-cod-estabel-ini AND
         polo-lote-restricao.it-codigo   >= c-it-codigo-ini AND
         polo-lote-restricao.it-codigo   <= c-it-codigo-fim AND
         polo-lote-restricao.lote        >= c-nr-lote-ini AND
         polo-lote-restricao.lote        <= c-nr-lote-fim AND
         polo-lote-restricao.dt-incl-alt >= c-nr-data-ini AND
         polo-lote-restricao.dt-incl-alt <= c-nr-data-fim :
        

         FIND FIRST ITEM WHERE
             ITEM.it-codigo = polo-lote-restricao.it-codigo
             NO-LOCK NO-ERROR.

         IF NOT AVAIL ITEM THEN NEXT.

         IF ITEM.fm-codigo < c-nr-familia-ini or
            ITEM.fm-codigo > c-nr-familia-fim THEN NEXT.

         ASSIGN saldo-jr = 0.
/*
        FOR EACH saldo-estoq NO-LOCK WHERE
            saldo-estoq.lote      = polo-lote-restricao.lote AND
            saldo-estoq.it-codigo = polo-lote-restricao.it-codigo AND
            saldo-estoq.lote      = polo-lote-restricao.lote AND
            saldo-estoq.cod-depos <> "ARC" AND
            saldo-estoq.qtidade-atu > 0
            USE-INDEX lote.

            ASSIGN saldo-jr = saldo-jr + saldo-estoq.qtidade-atu.

        END.

        IF saldo-jr <= 0 THEN NEXT.
*/
        FIND FIRST lote-prod WHERE
            lote-prod.it-codigo = polo-lote-restricao.it-codigo AND
            lote-prod.lote      = polo-lote-restricao.lote
            NO-LOCK NO-ERROR.

        IF NOT AVAIL lote-prod THEN NEXT.

        FIND FIRST ord-prod WHERE
            ord-prod.nr-ord-produ = lote-prod.nr-ord-produ
            NO-LOCK NO-ERROR.

        IF NOT AVAIL ord-prod THEN NEXT.

        IF ord-prod.nr-linha < c-nr-linha-ini OR
           ord-prod.nr-linha > c-nr-linha-fim THEN NEXT.

        ASSIGN faixa-jr = ""
               motivo-jr1 = "".

        RUN pi-print-editor (INPUT polo-lote-restricao.faixa-rejeitada , 40).

        FOR EACH tt-editor.
            IF tt-editor.linha = 1 THEN
                faixa-jr = tt-editor.conteudo.
            ELSE
                faixa-jr = faixa-jr + " " + tt-editor.conteudo.
        END.

        RUN pi-print-editor (INPUT polo-lote-restricao.motivo , 90).

        FOR EACH tt-editor.           
            IF tt-editor.linha = 1 THEN
                motivo-jr1 = tt-editor.conteudo.
            ELSE
                motivo-jr1 = motivo-jr1 + " " + tt-editor.conteudo.
        END.                          

        ASSIGN motivo-jr = motivo-jr1.
    
        IF polo-lote-restricao.situacao = 1 THEN
            situacao-jr = "SIM".
           ELSE
               situacao-jr = "N«O".
    
        IF polo-lote-restricao.status-lote = 1 THEN
            status-jr = "PRIME C/RESTRIÄ«O".
           ELSE
               status-jr = "OFF GRADE".
        
        IF tt-param.destino <> 4 THEN DO:
    /***  C‡DIGO PARA SA÷DA EM 132 COLUNAS ***/

        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp
            polo-lote-restricao.lote           
            polo-lote-restricao.it-codigo      
            polo-lote-restricao.peso-total     
            polo-lote-restricao.peso-rejeitado 
            polo-lote-restricao.larg-rejeitada 
            faixa-jr                           
            motivo-jr                          
            situacao-jr                        
            status-jr                          
            with stream-io frame f-relat-09-132.
            down stream str-rp with frame f-relat-09-132.  
     END.

     IF tt-param.destino = 4 THEN DO:
           
           ASSIGN i-linha = i-linha + 1
                  c-relatorio:range("A" + STRING(i-linha)):VALUE = polo-lote-restricao.lote
                  c-relatorio:range("B" + STRING(i-linha)):VALUE = polo-lote-restricao.it-codigo
                  c-relatorio:range("C" + STRING(i-linha)):VALUE = polo-lote-restricao.peso-total
                  c-relatorio:range("D" + STRING(i-linha)):VALUE = polo-lote-restricao.peso-rejeitado
                  c-relatorio:range("E" + STRING(i-linha)):VALUE = polo-lote-restricao.larg-rejeitada
                  c-relatorio:range("F" + STRING(i-linha)):VALUE = faixa-jr
                  c-relatorio:range("G" + STRING(i-linha)):VALUE = motivo-jr1
                  c-relatorio:range("H" + STRING(i-linha)):VALUE = situacao-jr
                  c-relatorio:range("I" + STRING(i-linha)):VALUE = status-jr.
                                                                             
     END.
        

     assign v-num-reg-lidos = v-num-reg-lidos + 1.
     run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

END. 

IF tt-param.destino <> 4 THEN DO:

    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp 
    with stream-io frame f-relat-branco.
    DOWN 2 stream str-rp with frame f-relat-branco.
    
    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp 
    with stream-io frame f-relat-linha.
    DOWN stream str-rp with frame f-relat-linha.
    
END.

if  l-imprime = no then do:
    if  tt-param.formato = 1 then do:
        view stream str-rp frame f-cabec-80.
        view stream str-rp frame f-rodape-80.
    end.

    if  tt-param.formato = 2 then do:
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
    end.
    disp stream str-rp " " with stream-io frame f-nulo.
end.

run pi-finalizar in h-acomp.

IF tt-param.destino = 4 THEN DO:

   RUN pi-finaliza-impressao.

   RETURN 'OK'.

END.



if  tt-param.destino <> 1 then

    page stream str-rp.

else do:

    if   tt-param.parametro = yes then

         page stream str-rp.

end.

if  tt-param.parametro then do:


   disp stream str-rp "SELEÄ«O" skip(01) with stream-io frame f-imp-sel.
   disp stream str-rp 
      c-cod-estabel-ini colon 23 "|< >|"   at 47 c-cod-estabel-fim no-label
      c-nr-lote-ini colon 23 "|< >|"   at 47 c-nr-lote-fim no-label
        with stream-io side-labels overlay row 034 frame f-imp-sel.

   disp stream str-rp "CLASSIFICAÄ«O" skip(01) with stream-io frame f-imp-cla.
   disp stream str-rp "   CONTROLE DE MATERIAL COM RESTRIÄ«O"
        with stream-io side-labels overlay row 034 frame f-imp-cla.

   put stream str-rp unformatted skip(1) "IMPRESS«O" skip(1).

   put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
   put stream str-rp unformatted skip "    " "Execuá∆o: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
   put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
   put stream str-rp unformatted skip "    " "Usu†rio : " tt-param.usuario.

end.

else
    output stream str-rp close.

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

    c-arquivo = c-arq + 'mod-escq0017' + STRING(time)+ '.xls'.

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
