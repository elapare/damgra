/*****************************************************************************
**
**       Programa: escp0057rp.p
**
**       Data....: 04/04/2009  
**
**       Autor...: Amgra/Jos‚ Roberto
**
**       Objetivo: Observa‡äes de Qualidade em Mill Rolls 
**
**       OBS.....: 
**
*******************************************************************************/

define buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "escp0057rp".

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
    field c-cod-estabel-ini    AS CHAR 
    field c-dt-data-ini        AS DATE 
    field c-dt-data-fim        AS DATE 
    field c-it-codigo-ini      AS CHAR 
    field c-it-codigo-fim      AS CHAR 
    field c-nr-lote-ini        AS CHAR 
    field c-nr-lote-fim        AS CHAR 
    .



DEFINE TEMP-TABLE tt-lotes
    FIELD lote                 AS CHAR 
    FIELD it-codigo            AS CHAR 
    FIELD cod-estabel          AS CHAR
    FIELD dt-trans             AS DATE 
    FIELD nr-linha             AS INT
    FIELD obs-cq               AS CHAR
    FIELD obs-prod             AS CHAR
    FIELD operador             AS CHAR 
    INDEX ch-tt-lotes IS PRIMARY UNIQUE  lote
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
/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 


/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 


def new shared var c-cod-estabel-ini    AS CHAR format "x(3)"          initial "422"              no-undo.
def new shared var c-dt-data-ini        AS DATE format "99/99/9999"    initial TODAY              no-undo.
def new shared var c-dt-data-fim        AS DATE format "99/99/9999"    initial TODAY              no-undo.
def new shared var c-it-codigo-ini      AS CHAR format "x(16)"         initial ""                 no-undo.
def new shared var c-it-codigo-fim      AS CHAR FORMAT "x(16)"         INITIAL "ZZZZZZZZZZZZZZZZ" NO-UNDO.
def new shared var c-nr-lote-ini        AS CHAR FORMAT "x(10)"         INITIAL ""                 NO-UNDO.
def new shared var c-nr-lote-fim        AS CHAR FORMAT "x(10)"         INITIAL "ZZZZZZZZZZ"       NO-UNDO.


/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

DEFINE VARIABLE esp-jr         AS INTEGER    NO-UNDO.
DEFINE VARIABLE obs-cq-jr      AS CHAR       NO-UNDO.
DEFINE VARIABLE obs-prod-jr    AS CHAR       NO-UNDO.
DEFINE VARIABLE linha-jr       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE qtd-bob-jr     AS INTEGER    NO-UNDO.
DEFINE VARIABLE tem-pallet-jr  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE nome-op-jr     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lote-jr        AS CHARACTER  NO-UNDO.

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

assign c-programa     = "escp0057rp"
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

    assign c-cod-estabel-ini    = tt-param.c-cod-estabel-ini    
           c-dt-data-ini        = tt-param.c-dt-data-ini        
           c-dt-data-fim        = tt-param.c-dt-data-fim        
           c-it-codigo-ini      = tt-param.c-it-codigo-ini      
           c-it-codigo-fim      = tt-param.c-it-codigo-fim      
           c-nr-lote-ini        = tt-param.c-nr-lote-ini        
           c-nr-lote-fim        = tt-param.c-nr-lote-fim.   



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

    ASSIGN c-modelo-planilha = search("modelos/mod-escp0057.xls") 
           c-arq             = SESSION:TEMP-DIRECTORY.

    RUN pi-cria-planilha.


END.


assign v-num-reg-lidos = 0.


ASSIGN i-linha = 4. 


FOR EACH tt-lotes:
    DELETE tt-lotes.
END.

for each movto-estoq no-lock
       where movto-estoq.esp-docto    = 1                AND
             movto-estoq.cod-estabel  = c-cod-estabel    and 
             movto-estoq.dt-trans    >= c-dt-data-ini    and 
             movto-estoq.dt-trans    <= c-dt-data-fim    and 
             movto-estoq.it-codigo   >= c-it-codigo-ini  AND
             movto-estoq.it-codigo   <= c-it-codigo-fim  AND
             movto-estoq.lote        >= c-nr-lote-ini    AND
             movto-estoq.lote        <= c-nr-lote-fim    AND
             movto-estoq.lote        <> "recicl"         AND 
             movto-estoq.cod-depos   <> "ARC"            AND
             movto-estoq.quantidade  <> 0
             USE-INDEX esp-data.


    FIND FIRST movto-mat WHERE
        movto-mat.nr-reporte = movto-estoq.nr-reporte AND
        movto-mat.esp-docto = 8
        USE-INDEX reporte
        NO-LOCK NO-ERROR.
    
    IF AVAIL movto-mat THEN NEXT.
       
    FIND FIRST ord-prod WHERE
       ord-prod.nr-ord-produ = movto-estoq.nr-ord-produ AND
       ord-prod.it-codigo    = movto-estoq.it-codigo    AND
       ord-prod.cod-estabel  = movto-estoq.cod-estabel    
       USE-INDEX estabel NO-LOCK NO-ERROR.
   
    IF NOT AVAIL ord-prod THEN NEXT.
   
    IF ord-prod.nr-linha > 99 THEN NEXT.

    FIND FIRST tt-lotes WHERE
        tt-lotes.it-codigo = movto-estoq.it-codigo AND
        tt-lotes.lote      = movto-estoq.lote
        NO-ERROR.

    IF AVAIL tt-lotes THEN NEXT.

    ASSIGN obs-cq-jr = ""
           obs-prod-jr = "".
   
    FIND FIRST lote-carac-tec WHERE
                        lote-carac-tec.it-codigo   = movto-estoq.it-codigo
                        and lote-carac-tec.lote    = movto-estoq.lote
                        and lote-carac-tec.cd-comp = "obscq"
                        NO-LOCK NO-ERROR.

   
    if avail lote-carac-tec THEN DO:

        FIND FIRST lote-msg-carac where
               lote-msg-carac.it-codigo  = lote-carac-tec.it-codigo AND
               lote-msg-carac.lote       = lote-carac-tec.lote      AND
               lote-msg-carac.cd-comp    = lote-carac-tec.cd-comp   AND
               lote-msg-carac.cd-folha   = lote-carac-tec.cd-folha  
            NO-LOCK NO-ERROR.

        IF AVAIL lote-msg-carac THEN
           ASSIGN obs-cq-jr = trim(lote-msg-carac.msg-exp).
           ELSE
            ASSIGN obs-cq-jr = "".

    END.


    FIND FIRST lote-carac-tec WHERE
                        lote-carac-tec.it-codigo   = movto-estoq.it-codigo
                        and lote-carac-tec.lote    = movto-estoq.lote
                        and lote-carac-tec.cd-comp = "obs"
                        NO-LOCK NO-ERROR.
   
    if avail lote-carac-tec THEN do:

        FIND FIRST lote-msg-carac where
               lote-msg-carac.it-codigo  = lote-carac-tec.it-codigo AND
               lote-msg-carac.lote       = lote-carac-tec.lote      AND
               lote-msg-carac.cd-comp    = lote-carac-tec.cd-comp   AND
               lote-msg-carac.cd-folha   = lote-carac-tec.cd-folha  
            NO-LOCK NO-ERROR.

        IF AVAIL lote-msg-carac THEN

           ASSIGN obs-prod-jr = trim(lote-msg-carac.msg-exp).
              ELSE
                ASSIGN obs-prod-jr = "".

    END.


    ASSIGN obs-cq-jr = replace(obs-cq-jr,CHR(13)," ")
           obs-cq-jr = replace(obs-cq-jr,CHR(10)," ")
           obs-cq-jr = replace(obs-cq-jr,CHR(9)," ").

    ASSIGN obs-prod-jr = replace(obs-prod-jr,CHR(13)," ")
           obs-prod-jr = replace(obs-prod-jr,CHR(10)," ")
           obs-prod-jr = replace(obs-prod-jr,CHR(9)," ").

    ASSIGN lote-jr = movto-estoq.lote.
    RUN pi-acha-operador.
    
    CREATE tt-lotes.

    ASSIGN tt-lotes.it-codigo = movto-estoq.it-codigo
           tt-lotes.lote      = movto-estoq.lote.

    ASSIGN tt-lotes.cod-estabel  = movto-estoq.cod-estabel
           tt-lotes.dt-trans     = movto-estoq.dt-trans
           tt-lotes.nr-linha     = ord-prod.nr-linha
           tt-lotes.obs-cq       = obs-cq-jr                
           tt-lotes.obs-prod     = obs-prod-jr
           tt-lotes.operador     = nome-op-jr.                


   
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

END.


ASSIGN v-num-reg-lidos = 0.

for each tt-lotes no-lock.

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input "Gerando Planilha: " + string(v-num-reg-lidos)).

    IF tt-param.destino = 4 THEN DO:

       ASSIGN i-linha = i-linha + 1.

       ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-lotes.cod-estabel
              c-relatorio:range("B" + STRING(i-linha)):VALUE = tt-lotes.it-codigo      
              c-relatorio:range("C" + STRING(i-linha)):VALUE = tt-lotes.dt-trans        
              c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-lotes.nr-linha        
              c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-lotes.lote
              c-relatorio:range("F" + STRING(i-linha)):VALUE = tt-lotes.operador
              c-relatorio:range("G" + STRING(i-linha)):VALUE = tt-lotes.obs-cq
              c-relatorio:range("H" + STRING(i-linha)):VALUE = tt-lotes.obs-prod.

    END. 

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

    c-arquivo = c-arq + 'escp0057' + STRING(time)+ '.xls'.

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



PROCEDURE pi-acha-operador.

    ASSIGN nome-op-jr = "".

    FIND FIRST lote-prod WHERE
        lote-prod.lote = lote-jr 
        USE-INDEX lote
        NO-LOCK NO-ERROR.

    IF AVAIL lote-prod THEN DO:

        FIND LAST movto-mat WHERE
            movto-mat.lote = lote-prod.lote AND
            movto-mat.it-codigo = lote-prod.it-codigo AND
            movto-mat.esp-docto = 1
            USE-INDEX lote
            NO-LOCK NO-ERROR.

        IF AVAIL movto-mat THEN  DO:

            FIND FIRST rep-oper-ctrab WHERE
                rep-oper-ctrab.nr-ord-prod = movto-mat.nr-ord-prod and
                rep-oper-ctrab.num-seq-rep = movto-mat.num-sequen 
                
                NO-LOCK NO-ERROR.


           IF AVAIL rep-oper-ctrab THEN DO:

              FIND FIRST rep-oper-mod WHERE
                  rep-oper-mod.nr-ord-produ = movto-mat.nr-ord-prod AND
                  rep-oper-mod.num-seq-rep  = rep-oper-ctrab.num-seq-rep
                  USE-INDEX id
                  NO-LOCK NO-ERROR.
           
              IF AVAIL rep-oper-mod THEN DO:
           
                  FIND FIRST operador WHERE
                      operador.cod-operador = rep-oper-mod.cod-operador
                      USE-INDEX id
                      NO-LOCK NO-ERROR.
           
                  IF AVAIL operador THEN
                      ASSIGN nome-op-jr = operador.nom-operador.
           
              END.

           END.

        END.    

    END.

END PROCEDURE.





return 'OK'.

/* fim do programa */
