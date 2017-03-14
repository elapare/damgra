/*****************************************************************************
**
**       Programa: escc0014rp.p
**
**       Data....: 03/03/2008
**
**       Autor...: Amgra - JosÇ Roberto
**
**       Objetivo: Geraá∆o de Ordens de Compras da Lista de Necessidades
**
**       Vers∆o..: 
**
**       OBS.....: 
**
*******************************************************************************/
define buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "escc0014RP".

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
    field cod-estabel          like am-cc-tab-preco.cod-estabel 
    field c-arquivo-jr         AS CHAR
    FIELD pesq-jr              AS INT 
    FIELD cod-emitente-jr      AS INT .


DEFINE TEMP-TABLE tt-digita no-undo
    FIELD cod-estabel              AS CHAR     FORMAT "x(4)"          LABEL "Est"
    FIELD cod-emitente             AS INT      FORMAT ">>>>>>>9"      LABEL "Fornec."
    FIELD nome-abrev               AS CHAR     FORMAT "x(12)"         LABEL "Nome        "
    FIELD it-codigo                AS CHAR     FORMAT "x(16)"         LABEL "Item"
    FIELD unid                     AS CHAR     FORMAT "x(2)"          LABEL "Un"
    FIELD quantidade               AS DEC      FORMAT "->>>>>>>>>>9.9999" LABEL "Quantidade"
    FIELD preco-item               AS DEC      FORMAT ">>>>>>>>>9.99"     LABEL "Preco Unit"
    FIELD cod-cond-pag             AS INT      FORMAT ">>9"               LABEL "C.Pag"
    FIELD dt-entrega               AS DATE     FORMAT "99/99/9999"        LABEL "Dt.Entrega"
    FIELD nr-tab-preco             AS INT      FORMAT ">>>>>>>>9"         LABEL "Nr.Tab.Pr"
    FIELD perc-icms                AS DEC      FORMAT ">>>>9.99"          LABEL "%Icms"
    FIELD perc-ipi                 AS DEC      FORMAT ">>>>9.99"          LABEL "%Ipi"
    FIELD perc-rat-compra          AS DEC      FORMAT ">>>>9.99"          LABEL "%Rat.Comp"
    FIELD usuar-comprador          AS CHAR     FORMAT "x(12)"             LABEL "Comprador"
    FIELD gerado-ordem             AS LOG      FORMAT "Sim/N∆o"           LABEL "Gerado Ord"
    FIELD descricao                AS CHAR     FORMAT "x(40)"             LABEL "Descriá∆o"
    FIELD narrativa                AS CHAR     FORMAT "x(100)"            LABEL "Narrativa"
    FIELD ct-codigo                AS CHAR     FORMAT "x(8)"              LABEL "Cta.Cont."
    FIELD sc-codigo                AS CHAR     FORMAT "x(8)"              LABEL "C.Custo."
    FIELD log-erro                 AS LOG
    FIELD linha                    AS INT
    FIELD sequencia                AS INT
    FIELD numero-ordem             AS INT
    FIELD usuar-aprov-chefe        AS CHAR     FORMAT "x(12)"
    FIELD origem                   AS INT 
    FIELD cod-transp               AS INT
    INDEX codigo IS PRIMARY UNIQUE cod-estabel
                                   it-codigo
                                   linha
                                   sequencia
                                   cod-emitente.


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
def new shared var c-cod-estabel     like am-cc-tab-preco.cod-estabel   format "x(03)"    initial "412"   no-undo. /*solic-318*/ 

/****************** Definiáao de Vari†veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Definiáao de Vari†veis Campo do Layout do Relat¢rio **********************/ 

/****************** Definiáao de Vari†veis do Relat¢rio N∆o Pedidas em Tela ******************/ 

/* Vari†veis necess†rias para a rastreabilidade de bobinas 
   Programa externo escp033 e escp034 */


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

FORM tt-digita.linha        COLUMN-LABEL "Linha"      FORMAT ">>>>>9"    AT 001
     tt-digita.it-codigo    COLUMN-LABEL "Item"       FORMAT "x(16)"     AT 008
     tt-digita.descricao    COLUMN-LABEL "Descriá∆o"  FORMAT "x(40)"     AT 025
     tt-digita.unid         COLUMN-LABEL "Un"         FORMAT "x(02)"     AT 066
     tt-digita.quantidade   COLUMN-LABEL "Qtde."      FORMAT "->>>>>>>>>>9.99" AT 069
     tt-digita.dt-entrega   COLUMN-LABEL "Dt.Entrega" FORMAT "99/99/9999"      AT 086
     tt-digita.ct-codigo    COLUMN-LABEL "Conta"      FORMAT "x(8)"            AT 097
     tt-digita.sc-codigo    COLUMN-LABEL "C.Custo"    FORMAT "x(8)"            AT 106              
     tt-digita.narrativa    COLUMN-LABEL "Mensagem de Erro"  FORMAT "x(50)"    AT 115
     with down width 180 no-box stream-io frame f-relat-09-132.

FORM tt-digita.gerado-ordem COLUMN-LABEL "Ger"        FORMAT "Sim/N∆o"   AT 001
     tt-digita.numero-ordem COLUMN-LABEL "Nr.Ordem"   FORMAT ">>>>>>>>9" AT 005
     tt-digita.linha        COLUMN-LABEL "Linha"      FORMAT ">>>>>9"    AT 015
     tt-digita.it-codigo    COLUMN-LABEL "Item"       FORMAT "x(16)"     AT 022
     tt-digita.descricao    COLUMN-LABEL "Descriá∆o"  FORMAT "x(40)"     AT 039
     tt-digita.unid         COLUMN-LABEL "Un"         FORMAT "x(02)"     AT 080
     tt-digita.quantidade   COLUMN-LABEL "Qtde."      FORMAT "->>>>>>>>>>9.99" AT 083
     tt-digita.dt-entrega   COLUMN-LABEL "Dt.Entrega" FORMAT "99/99/9999"      AT 099
     tt-digita.ct-codigo    COLUMN-LABEL "Conta"      FORMAT "x(8)"            AT 110
     tt-digita.sc-codigo    COLUMN-LABEL "C.Custo"    FORMAT "x(8)"            AT 119              
     tt-digita.narrativa    COLUMN-LABEL "Narrativa ou Mensagem Erro"  FORMAT "x(50)"    AT 128
     with down width 180 no-box stream-io frame f-relat-02-132.


form HEADER
    fill("-", 132) format "x(132)" SKIP 
    WITH DOWN WIDTH 132 NO-BOX STREAM-IO FRAME f-relat-linha.

form HEADER
    fill(" ", 132) format "x(132)" SKIP 
    WITH DOWN WIDTH 132 NO-BOX STREAM-IO FRAME f-relat-branco.

create tt-param.
raw-transfer raw-param to tt-param.

FOR EACH tt-raw-digita:
   CREATE tt-digita.
   RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

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

assign c-programa     = "escc0014rp"
       c-versao       = "1.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Necessidades de Compras"
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
    fill("-", 170) format "x(132)" skip
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

    assign i-ep-codigo-usuario = string(tt-param.ep-codigo)
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
           ASSIGN v-cod-destino-impres = "excel".


run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.
   
FOR EACH tt-digita where
    tt-digita.log-erro = YES NO-LOCK:      

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)). 
     
          
    /***  C‡DIGO PARA SA÷DA EM 132 COLUNAS ***/
      
             view stream str-rp frame f-cabec.
             view stream str-rp frame f-rodape.
             assign l-imprime = yes.
             display stream str-rp
               tt-digita.linha       
               tt-digita.it-codigo   
               tt-digita.descricao   
               tt-digita.unid        
               tt-digita.quantidade  
               tt-digita.dt-entrega  
               tt-digita.ct-codigo
               tt-digita.sc-codigo
               tt-digita.narrativa   
               with stream-io frame f-relat-09-132.
               down stream str-rp with frame f-relat-09-132.  
END.

PUT STREAM str-rp " " AT 01
                  "-----------------------------------------------" AT 01
                  " " AT 01.

FOR EACH tt-digita where
    tt-digita.log-erro = NO NO-LOCK:      

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)). 
     
          
    /***  C‡DIGO PARA SA÷DA EM 132 COLUNAS ***/
      
             view stream str-rp frame f-cabec.
             view stream str-rp frame f-rodape.
             assign l-imprime = yes.
             display stream str-rp
               tt-digita.gerado-ordem  
               tt-digita.numero-ordem
               tt-digita.linha       
               tt-digita.it-codigo   
               tt-digita.descricao   
               tt-digita.unid        
               tt-digita.quantidade  
               tt-digita.dt-entrega  
               tt-digita.ct-codigo
               tt-digita.sc-codigo
               tt-digita.narrativa   
               with stream-io frame f-relat-02-132.
               down stream str-rp with frame f-relat-02-132.  

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

if  tt-param.destino <> 1 then

    page stream str-rp.

else do:

    if   tt-param.parametro = yes then

         page stream str-rp.

end.

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


return 'OK'.

/* fim do programa */
