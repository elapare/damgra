/*****************************************************************************
**
**       Programa: escq0055rp.p
**
**       Data....: 18/06/2009   
**
**       Autor...: Amgra/Jos‚ Roberto
**
**       Objetivo: Propriedades de An lises de Laboratorio 
**
**       OBS.....: 
**
*******************************************************************************/
def buffer empresa for mgmulti.empresa.

define variable c-prog-gerado as character no-undo initial "escq0055rp".

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
    field c-cod-estabel-ini    like pallet.cod-estabel
    field dt-producao-ini      like pallet.data-pallet
    field dt-producao-fim      like pallet.data-pallet
    field i-nr-linha-ini       like ord-prod.nr-linha
    field i-nr-linha-fim       like ord-prod.nr-linha
    field c-nr-lote-ini        like movto-estoq.lote
    field c-nr-lote-fim        like movto-estoq.lote
    field c-it-codigo-ini      like pallet.it-codigo
    field c-it-codigo-fim      like pallet.it-codigo
    field i-cod-exame-ini      like comp-exame.cod-exame
    field i-cod-exame-fim      like comp-exame.cod-exame
    field i-cod-comp-ini       like comp-exame.cod-comp
    field i-cod-comp-fim        like comp-exame.cod-comp.


define temp-table tt-lote NO-UNDO
    field nr-linha        AS   INT  
    field it-codigo       AS   CHAR 
    field cod-exame       AS   INT  
    field cod-comp        AS   INT  
    field nr-lote         AS   CHAR 
    field menor-result    AS   DEC  
    field maior-result    AS   DEC  
    field ultima-seqa     AS   INT 
    field resultado       AS   DEC  EXTENT 50 
    INDEX chave IS PRIMARY UNIQUE nr-linha
                                  it-codigo  
                                  cod-exame
                                  cod-comp   
                                  nr-lote.




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

def new shared var c-cod-estabel-ini    like pallet.cod-estabel           format "X(3)"         initial "422"              no-undo.
def new shared var dt-producao-ini      like pallet.data-pallet           format "99/99/9999"   initial TODAY              no-undo.
def new shared var dt-producao-fim      like pallet.data-pallet           format "99/99/9999"   initial TODAY              no-undo.
def new shared var c-nr-lote-ini        like movto-estoq.lote             format "X(10)"        initial ""                 no-undo.
def new shared var c-nr-lote-fim        like movto-estoq.lote             format "X(10)"        initial "ZZZZZZZZZZ"       no-undo.
def new shared var i-nr-linha-ini       like ord-prod.nr-linha            format ">>>>9"        initial 1                  no-undo.
def new shared var i-nr-linha-fim       like ord-prod.nr-linha            format "999"          initial 499                no-undo.
def new shared var i-cod-exame-ini      like comp-exame.cod-exame         format ">>>>>>9"      initial 0                  no-undo.
def new shared var i-cod-exame-fim      like comp-exame.cod-exame         format ">>>>>>9"      initial 9999999            no-undo.
def new shared var i-cod-comp-ini       like comp-exame.cod-comp          format ">>>>>>9"      initial 0                  no-undo.
def new shared var i-cod-comp-fim       like comp-exame.cod-comp          format ">>>>>>9"      initial 9999999            no-undo.
def new shared var c-it-codigo-ini      like pallet.it-codigo             format "X(16)"        initial ""                 no-undo.
def new shared var c-it-codigo-fim      like pallet.it-codigo             format "X(16)"        initial "ZZZZZZZZZZZZZZZZ" no-undo.


/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

DEFINE VARIABLE soma-result    AS INTEGER    NO-UNDO.
DEFINE VARIABLE soma-col       AS INTEGER    NO-UNDO.
DEFINE VARIABLE seq-jr         AS INTEGER    NO-UNDO.
DEFINE VARIABLE descricao-jr   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE it-codigo-ant  AS CHARACTER  INITIAL "ZZZZZZZZ" NO-UNDO.
DEFINE VARIABLE nr-linha-ant   AS INTEGER    INITIAL 9999       NO-UNDO.
DEFINE VARIABLE cod-exame-ant  AS INTEGER    INITIAL 99999999   NO-UNDO.
DEFINE VARIABLE cod-comp-ant   AS INTEGER    INITIAL 99999999   NO-UNDO.

/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 
DEFINE VARIABLE col-jr         AS CHARACTER  NO-UNDO.
ASSIGN col-jr = "J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,AB,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AM,AN,AO,AP,AQ,AR,AS,AT,AU,AV,AW,AX,AY,AZ,BA,BB,BC,BD,BE,BF,BG,BH,BI,BJ,BK,BL,BM,BN,BO,BP,BQ,BR,BS,BT,BU,BV,BW,BX,BY,BZ,CA".

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

assign c-programa     = "escq0055rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Propriedades de An lises de Laboratorio"
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

    assign c-cod-estabel-ini   = tt-param.c-cod-estabel-ini  
           dt-producao-ini     = tt-param.dt-producao-ini    
           dt-producao-fim     = tt-param.dt-producao-fim    
           i-nr-linha-ini      = tt-param.i-nr-linha-ini     
           i-nr-linha-fim      = tt-param.i-nr-linha-fim     
           c-nr-lote-ini       = tt-param.c-nr-lote-ini      
           c-nr-lote-fim       = tt-param.c-nr-lote-fim      
           c-it-codigo-ini     = tt-param.c-it-codigo-ini    
           c-it-codigo-fim     = tt-param.c-it-codigo-fim    
           i-cod-exame-ini     = tt-param.i-cod-exame-ini    
           i-cod-exame-fim     = tt-param.i-cod-exame-fim    
           i-cod-comp-ini      = tt-param.i-cod-comp-ini     
           i-cod-comp-fim      = tt-param.i-cod-comp-fim.     

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

    ASSIGN c-modelo-planilha = search("modelos/mod-escq0055.xls") 
           c-arq             = SESSION:TEMP-DIRECTORY.

    RUN pi-cria-planilha.


END.


assign v-num-reg-lidos = 0.

ASSIGN c-relatorio:range("G3"):VALUE = dt-producao-ini
       c-relatorio:range("I3"):VALUE = dt-producao-fim
       c-relatorio:range("L3"):VALUE = c-cod-estabel-ini.

ASSIGN i-linha = 6. 

  
for each movto-estoq where
         movto-estoq.esp-docto    = 1                  AND
         movto-estoq.cod-estabel  = c-cod-estabel-ini  and 
         movto-estoq.dt-trans    >= dt-producao-ini    and 
         movto-estoq.dt-trans    <= dt-producao-fim    and 
         movto-estoq.it-codigo   >= c-it-codigo-ini    and 
         movto-estoq.it-codigo   <= c-it-codigo-fim    and 
         movto-estoq.lote        >= c-nr-lote-ini      and 
         movto-estoq.lote        <= c-nr-lote-fim      and 
         movto-estoq.lote        <> "recicl"           AND 
         movto-estoq.cod-depos   <> "ARC"              AND
         movto-estoq.quantidade  <> 0
         USE-INDEX esp-data
         NO-LOCK.

    FIND FIRST movto-mat WHERE
        movto-mat.nr-reporte = movto-estoq.nr-reporte AND
        movto-mat.esp-docto = 8
        USE-INDEX reporte
        NO-LOCK NO-ERROR.

    IF AVAIL movto-mat THEN NEXT.

    FIND FIRST ord-prod WHERE 
         ord-prod.nr-ord-produ = movto-estoq.nr-ord-produ
         NO-LOCK NO-ERROR.

    IF NOT AVAIL ord-prod THEN NEXT.

    IF ord-prod.nr-linha < i-nr-linha-ini OR 
       ord-prod.nr-linha > i-nr-linha-fim THEN NEXT.

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input "Lendo Lotes/An lises: " + string(v-num-reg-lidos)).

    FOR EACH ficha-cq NO-LOCK WHERE
        ficha-cq.it-codigo   = movto-estoq.it-codigo AND
        ficha-cq.lote        = movto-estoq.lote /*AND
        ficha-cq.cod-estabel = movto-estoq.cod-estabel AND
        ficha-cq.cod-depos   = movto-estoq.cod-depos  AND
        ficha-cq.cod-localiz = movto-estoq.cod-localiz 29052012 - edson cod deposito estava  diferente nao gerava*/
        USE-INDEX it-lote :
       
       FOR EACH pol-res-fic-cq-leitura NO-LOCK WHERE
           pol-res-fic-cq-leitura.nr-ficha   = ficha-cq.nr-ficha AND
           pol-res-fic-cq-leitura.cod-exame >= i-cod-exame-ini   AND
           pol-res-fic-cq-leitura.cod-exame <= i-cod-exame-fim   AND
           pol-res-fic-cq-leitura.cod-comp  >= i-cod-comp-ini    AND       
           pol-res-fic-cq-leitura.cod-comp  <= i-cod-comp-fim:
           
           IF DEC(pol-res-fic-cq-leitura.resultado)  = 0 THEN NEXT.

           FIND FIRST tt-lote WHERE
               tt-lote.nr-linha  = ord-prod.nr-linha                AND
               tt-lote.it-codigo = movto-estoq.it-codigo            AND
               tt-lote.nr-lote   = movto-estoq.lote                 AND
               tt-lote.cod-exame = pol-res-fic-cq-leitura.cod-exame AND
               tt-lote.cod-comp  = pol-res-fic-cq-leitura.cod-comp
               NO-ERROR.

           IF NOT AVAIL tt-lote THEN DO:

              CREATE tt-lote.
              ASSIGN tt-lote.cod-comp  = pol-res-fic-cq-leitura.cod-comp
                     tt-lote.nr-linha  = ord-prod.nr-linha    
                     tt-lote.cod-exame = pol-res-fic-cq-leitura.cod-exame
                     tt-lote.it-codigo = movto-estoq.it-codigo  
                     tt-lote.nr-lote   = movto-estoq.lote.

              ASSIGN tt-lote.menor-result = 9999999999
                     tt-lote.maior-result = 0.

           END.

           IF DEC (pol-res-fic-cq-leitura.resultado) < tt-lote.menor-result THEN
              ASSIGN tt-lote.menor-result = DEC (pol-res-fic-cq-leitura.resultado).

           IF DEC (pol-res-fic-cq-leitura.resultado) > tt-lote.maior-result THEN
              ASSIGN tt-lote.maior-result = DEC (pol-res-fic-cq-leitura.resultado).

           ASSIGN tt-lote.ultima-seq = tt-lote.ultima-seq + 1.

           IF tt-lote.ultima-seq < 51 THEN
               ASSIGN tt-lote.resultado [tt-lote.ultima-seq] = DEC (pol-res-fic-cq-leitura.resultado).

            
       END.

    END.

END.
   

assign v-num-reg-lidos = 0.

for each tt-lote no-lock.

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input "Gerando Planilha: " + string(v-num-reg-lidos)).

    FIND FIRST comp-exame WHERE
        comp-exame.cod-exame = tt-lote.cod-exame    AND
        comp-exame.cod-comp  = tt-lote.cod-comp
        NO-LOCK NO-ERROR.

    IF AVAIL comp-exame THEN
        ASSIGN descricao-jr = comp-exame.descricao.
    ELSE
        ASSIGN descricao-jr = "".

    IF tt-param.destino = 4 THEN DO:

       ASSIGN i-linha = i-linha + 1.

       c-Excel:Rows(i-linha):select no-error.
       c-Excel:Selection:COPY.   

       c-Excel:Rows(i-linha + 1):select no-error.
       c-Excel:Selection:INSERT.   

       IF tt-lote.nr-linha  <> nr-linha-ant  OR
          tt-lote.it-codigo <> it-codigo-ant OR
          tt-lote.cod-exame <> cod-exame-ant OR
          tt-lote.cod-comp  <> cod-comp-ant THEN 

          ASSIGN seq-jr = 0
                 nr-linha-ant  = tt-lote.nr-linha
                 it-codigo-ant = tt-lote.it-codigo
                 cod-exame-ant = tt-lote.cod-exame
                 cod-comp-ant  = tt-lote.cod-comp.


       ASSIGN seq-jr      = seq-jr + 1
              soma-result = 1
              soma-col    = 0.

       ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-lote.nr-linha      
              c-relatorio:range("B" + STRING(i-linha)):VALUE = tt-lote.it-codigo      
              c-relatorio:range("C" + STRING(i-linha)):VALUE = descricao-jr        
              c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-lote.menor-result        
              c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-lote.maior-result        
              c-relatorio:range("H" + STRING(i-linha)):VALUE = seq-jr       
              c-relatorio:range("I" + STRING(i-linha)):VALUE = tt-lote.nr-lote.      

       DO WHILE soma-result < 51.

           IF tt-lote.resultado [soma-result] <> 0 THEN

               ASSIGN soma-col = soma-col + 1
                      c-relatorio:Range(trim(entry(soma-col,col-jr) + string(i-linha))):VALUE = tt-lote.resultado [soma-result].

           ASSIGN soma-result = soma-result + 1.

       END. 

    END. 

END.

run pi-finalizar in h-acomp.

if  tt-param.destino = 4 then
       c-Excel:Rows(1):select no-error.


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
          c-cod-estabel-ini colon 20 no-label
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

    c-arquivo = c-arq + 'escq0055' + STRING(time)+ '.xls'.

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
