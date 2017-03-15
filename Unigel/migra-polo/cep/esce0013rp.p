/*****************************************************************************
**
**       Programa: ESCE0013rp.p
**
**       Data....: 08/03/2006
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Estat¡stica de Consumo de Materiais
**
**       VersÆo..: 1.00.000 - Jos‚ Roberto R Campos
**
**       OBS.....: 
**
*******************************************************************************/
define variable c-prog-gerado as character no-undo initial "ESCE0013RP".
define buffer empresa for mgmulti.empresa.
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
    field c-cod-estabel        like movto-estoq.cod-estabel
    field c-ano-ini            AS INTEGER 
    field c-mes-ini            AS INTEGER
    field c-ano-fim            AS INTEGER 
    field c-mes-fim            AS INTEGER
    FIELD c-fm-codigo-ini      LIKE item.fm-codigo
    FIELD c-fm-codigo-fim      LIKE item.fm-codigo
    FIELD c-ge-codigo-ini      LIKE item.ge-codigo
    FIELD c-ge-codigo-fim      LIKE item.ge-codigo
    FIELD c-it-codigo-ini      LIKE item.it-codigo
    FIELD c-it-codigo-fim      LIKE item.it-codigo
    FIELD pesq-jr              AS INT  
    .

DEFINE TEMP-TABLE tt-itens
    FIELD tt-it-codigo         LIKE movto-estoq.it-codigo
    FIELD tt-fm-codigo         LIKE item.fm-codigo
    FIELD tt-ge-codigo         LIKE item.ge-codigo
    FIELD tt-consumo           AS DECIMAL EXTENT 13
    INDEX ch-tt-itens IS PRIMARY UNIQUE  tt-ge-codigo
                                         tt-fm-codigo
                                         tt-it-codigo.


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

def new shared var c-cod-estabel        like movto-estoq.cod-estabel   format "x(3)" initial "" no-undo.  /*solic-318*/ 
def new shared var c-ano-ini            AS INTEGER                     format 9999 INITIAL 2005. 
def new shared var c-mes-ini            AS INTEGER                     format 99 INITIAL 1.
def new shared var c-ano-fim            AS INTEGER                     format 9999 INITIAL 2005. 
def new shared var c-mes-fim            AS INTEGER                     format 99 INITIAL 1.
def new shared var c-fm-codigo-ini      LIKE item.fm-codigo            format "x(10)" INITIAL "" no-undo.
def new shared var c-fm-codigo-fim      LIKE item.fm-codigo            FORMAT "x(10)" INITIAL "ZZZZZZZZZZ" NO-UNDO.
def new shared var c-ge-codigo-ini      LIKE item.ge-codigo            format 99 initial 0  no-undo.
def new shared var c-ge-codigo-fim      LIKE item.ge-codigo            format 99 initial 99 no-undo.
def new shared var c-it-codigo-ini      LIKE item.it-codigo            format "x(16)" INITIAL "" no-undo.
def new shared var c-it-codigo-fim      LIKE item.it-codigo            FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZ" NO-UNDO.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 
DEFINE VARIABLE meses-x      AS CHARACTER  INITIAL "JANFEVMARABRMAIJUNJULAGOSETOUTNOVDEZ" NO-UNDO.
DEFINE VARIABLE meses-x1     AS CHARACTER  INITIAL "JanFevMarAbrMaiJunJulAgoSetOutNovDez" NO-UNDO.
DEFINE VARIABLE mes-cab      AS CHARACTER  FORMAT "x(160)" NO-UNDO.
DEFINE VARIABLE mes-jr       AS INTEGER    NO-UNDO.
DEFINE VARIABLE ano-jr       AS INTEGER    NO-UNDO.
DEFINE VARIABLE soma-jr      AS INTEGER    INITIAL 1  NO-UNDO.
DEFINE VARIABLE TOTAL-cons   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE data-ini-jr  AS DATE       NO-UNDO.
DEFINE VARIABLE data-fim-jr  AS DATE       NO-UNDO.
DEFINE VARIABLE consumo-jr   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dia-jr       AS INT        INITIAL 1 NO-UNDO.
DEFINE VARIABLE c-ano-mes    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-ano-mes    AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-ano-tra    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-ano-tra    AS INTEGER    NO-UNDO.
DEFINE VARIABLE idx-jr       AS INTEGER    NO-UNDO.

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

/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form tt-consumo [1]         NO-LABEL        FORMAT "->>>>>>>9.99" AT 001
     tt-consumo [2]         NO-LABEL        FORMAT "->>>>>>>9.99" AT 014
     tt-consumo [3]         NO-LABEL        FORMAT "->>>>>>>9.99" AT 027
     tt-consumo [4]         NO-LABEL        FORMAT "->>>>>>>9.99" AT 040
     tt-consumo [5]         NO-LABEL        FORMAT "->>>>>>>9.99" AT 053
     tt-consumo [6]         NO-LABEL        FORMAT "->>>>>>>9.99" AT 066
     tt-consumo [7]         NO-LABEL        FORMAT "->>>>>>>9.99" AT 079
     tt-consumo [8]         NO-LABEL        FORMAT "->>>>>>>9.99" AT 092
     tt-consumo [9]         NO-LABEL        FORMAT "->>>>>>>9.99" AT 105
     tt-consumo [10]        NO-LABEL        FORMAT "->>>>>>>9.99" AT 118
     tt-consumo [11]        NO-LABEL        FORMAT "->>>>>>>9.99" AT 131
     tt-consumo [12]        NO-LABEL        FORMAT "->>>>>>>9.99" AT 144
     tt-consumo [13]        NO-LABEL        FORMAT "->>>>>>>9.99" AT 157
     with down width 170 no-box stream-io frame f-relat-09-132.

form HEADER
    fill("-", 170) format "x(170)" SKIP 
    WITH DOWN WIDTH 170 NO-BOX STREAM-IO FRAME f-relat-linha.

form HEADER
    fill(" ", 170) format "x(170)" SKIP 
    WITH DOWN WIDTH 170 NO-BOX STREAM-IO FRAME f-relat-branco.


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

assign c-programa     = "ESCE0013rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Estat¡stica de Consumo de Materiais"
       c-sistema      = "".

form header
    fill("-", 170) format "x(170)" skip
    c-empresa c-titulo-relat at 50
    "Per¡odo: " AT 102 c-mes-ini "/" c-ano-ini " a " c-mes-fim "/" c-ano-fim
    "Folha:" at 159 page-number(str-rp) at 166 format ">>>>9" skip
    fill("-", 148) format "x(148)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") 
    mes-cab AT 01 "  TOTAL" SKIP
    fill("-", 170) format "x(170)" skip
    with stream-io width 170 no-labels no-box page-top frame f-cabec.


run grapi/gr2004.p.

form header
    c-rodape format "x(170)"
    with stream-io width 170 no-labels no-box page-bottom frame f-rodape.

run grapi/gr2013c.p (input tt-param.destino,
                    input tt-param.arquivo,
                    input tt-param.usuario,
                    input no).

    assign i-ep-codigo-usuario = tt-param.ep-codigo
           v_cdn_empres_usuar  = i-ep-codigo-usuario.
           
    assign c-cod-estabel    = tt-param.c-cod-estabel   
           c-ano-ini        = tt-param.c-ano-ini       
           c-mes-ini        = tt-param.c-mes-ini       
           c-ano-fim        = tt-param.c-ano-fim       
           c-mes-fim        = tt-param.c-mes-fim       
           c-fm-codigo-ini  = tt-param.c-fm-codigo-ini 
           c-fm-codigo-fim  = tt-param.c-fm-codigo-fim 
           c-ge-codigo-ini  = tt-param.c-ge-codigo-ini 
           c-ge-codigo-fim  = tt-param.c-ge-codigo-fim 
           c-it-codigo-ini  = tt-param.c-it-codigo-ini 
           c-it-codigo-fim  = tt-param.c-it-codigo-fim.


find first empresa no-lock
    where empresa.ep-codigo = tt-param.ep-codigo no-error.
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

run pi-inicializar in h-acomp(input "Movimentos Encontrados:").

assign v-num-reg-lidos = 0.

ASSIGN mes-jr = c-mes-ini.
   
    
DO WHILE soma-jr < 13.  

   ASSIGN mes-cab = mes-cab + "      " + SUBSTRING (meses-x,((mes-jr * 3) - 2),3) + "    "
          mes-jr  = mes-jr + 1
          soma-jr = soma-jr + 1.

   IF mes-jr > 12 THEN
      ASSIGN mes-jr = 1.

END. 

FOR EACH tt-itens :
    DELETE tt-itens.
END.

ASSIGN data-ini-jr = DATE(c-mes-ini,1,c-ano-ini)
       data-fim-jr = DATE(c-mes-fim,25,c-ano-fim)
       data-fim-jr = data-fim-jr + 8
       data-fim-jr = DATE (MONTH (data-fim-jr),1,YEAR (data-fim-jr))
       data-fim-jr = data-fim-jr - 1.

ASSIGN i-ano-mes = (c-ano-ini * 12) + c-mes-ini.
                                                                    

IF tt-param.destino = 4 THEN DO:
    
    /* Cria Aplica‡Æo do Excel */

    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

    ASSIGN c-modelo-planilha = search("modelos/mod-ESCE0013.xls") 
           c-arq             = SESSION:TEMP-DIRECTORY.

    RUN pi-cria-planilha.


END.

IF tt-param.destino = 4 THEN DO:

    ASSIGN i-linha = 1.

    IF tt-param.pesq-jr = 2 THEN
        ASSIGN c-relatorio:range("G" + STRING(i-linha)):VALUE = "** Valorizado **".
    
    ASSIGN c-relatorio:range("J" + STRING(i-linha)):VALUE = (STRING (c-mes-ini) + "/" + STRING (c-ano-ini) + " a " + STRING (c-mes-fim) + "/" + STRING (c-ano-fim) ).  

END.

IF tt-param.destino = 4 THEN DO:

    ASSIGN soma-jr = 1
           i-linha = 3
           mes-jr = c-mes-ini.
    
    DO WHILE soma-jr < 13.  

       IF soma-jr = 1  THEN ASSIGN c-relatorio:range("F" + STRING(i-linha)):VALUE = SUBSTRING (meses-x1,((mes-jr * 3) - 2),3). 
       IF soma-jr = 2  THEN ASSIGN c-relatorio:range("G" + STRING(i-linha)):VALUE = SUBSTRING (meses-x1,((mes-jr * 3) - 2),3). 
       IF soma-jr = 3  THEN ASSIGN c-relatorio:range("H" + STRING(i-linha)):VALUE = SUBSTRING (meses-x1,((mes-jr * 3) - 2),3). 
       IF soma-jr = 4  THEN ASSIGN c-relatorio:range("I" + STRING(i-linha)):VALUE = SUBSTRING (meses-x1,((mes-jr * 3) - 2),3). 
       IF soma-jr = 5  THEN ASSIGN c-relatorio:range("J" + STRING(i-linha)):VALUE = SUBSTRING (meses-x1,((mes-jr * 3) - 2),3). 
       IF soma-jr = 6  THEN ASSIGN c-relatorio:range("K" + STRING(i-linha)):VALUE = SUBSTRING (meses-x1,((mes-jr * 3) - 2),3). 
       IF soma-jr = 7  THEN ASSIGN c-relatorio:range("L" + STRING(i-linha)):VALUE = SUBSTRING (meses-x1,((mes-jr * 3) - 2),3). 
       IF soma-jr = 8  THEN ASSIGN c-relatorio:range("M" + STRING(i-linha)):VALUE = SUBSTRING (meses-x1,((mes-jr * 3) - 2),3). 
       IF soma-jr = 9  THEN ASSIGN c-relatorio:range("N" + STRING(i-linha)):VALUE = SUBSTRING (meses-x1,((mes-jr * 3) - 2),3). 
       IF soma-jr = 10 THEN ASSIGN c-relatorio:range("O" + STRING(i-linha)):VALUE = SUBSTRING (meses-x1,((mes-jr * 3) - 2),3). 
       IF soma-jr = 11 THEN ASSIGN c-relatorio:range("P" + STRING(i-linha)):VALUE = SUBSTRING (meses-x1,((mes-jr * 3) - 2),3). 
       IF soma-jr = 12 THEN ASSIGN c-relatorio:range("Q" + STRING(i-linha)):VALUE = SUBSTRING (meses-x1,((mes-jr * 3) - 2),3). 

       ASSIGN mes-jr  = mes-jr + 1
              soma-jr = soma-jr + 1.

       IF mes-jr > 12 THEN
          ASSIGN mes-jr = 1.

    END.

END.


ASSIGN i-linha = 3.

FOR EACH ITEM NO-LOCK WHERE
    ITEM.it-codigo >= c-it-codigo-ini AND
    ITEM.it-codigo <= c-it-codigo-fim AND
    ITEM.fm-codigo >= c-fm-codigo-ini AND
    ITEM.fm-codigo <= c-fm-codigo-fim AND
    ITEM.GE-codigo >= c-ge-codigo-ini AND
    ITEM.GE-codigo <= c-ge-codigo-fim.
    
    FOR each item-estab WHERE 
             item-estab.cod-estabel = c-cod-estabel AND 
             item-estab.it-codigo = ITEM.it-codigo NO-LOCK:
       
        ASSIGN consumo-jr = 0.
      
        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input item.it-codigo).
        
        FOR EACH movto-estoq NO-LOCK WHERE
            movto-estoq.it-codigo = item.it-codigo AND
            movto-estoq.dt-trans >= data-ini-jr AND
            movto-estoq.dt-trans <= data-fim-jr AND
            movto-estoq.cod-estabel = ITEM-estab.cod-estabel AND
            (movto-estoq.esp-docto = 5  OR
             movto-estoq.esp-docto = 7  OR
             movto-estoq.esp-docto = 28 OR
             movto-estoq.esp-docto = 30 OR
             movto-estoq.esp-docto = 31)
            USE-INDEX ITEM-data.
      
            ASSIGN ano-jr = INT (YEAR (movto-estoq.dt-trans))
                   mes-jr = INT (MONTH (movto-estoq.dt-trans)).

            ASSIGN i-ano-tra = (ano-jr * 12) + mes-jr.
            
            ASSIGN idx-jr = (i-ano-tra - i-ano-mes) + 1.
            
            FIND FIRST tt-itens WHERE
                tt-itens.tt-it-codigo  = movto-estoq.it-codigo AND
                tt-itens.tt-fm-codigo  = item.fm-codigo        AND
                tt-itens.tt-ge-codigo  = item.ge-codigo        
                NO-LOCK NO-ERROR.
      
            IF NOT AVAIL tt-itens THEN DO:
                CREATE tt-itens.
                ASSIGN tt-itens.tt-it-codigo  = movto-estoq.it-codigo
                       tt-itens.tt-fm-codigo  = item.fm-codigo       
                       tt-itens.tt-ge-codigo  = item.ge-codigo.       
            END.

            IF tt-param.pesq-jr = 1 THEN DO:

              IF movto-estoq.tipo-trans = 1 THEN
                 ASSIGN tt-itens.tt-consumo [idx-jr] = tt-itens.tt-consumo [idx-jr] - movto-estoq.quantidade
                        tt-itens.tt-consumo [13] = tt-itens.tt-consumo [13] - movto-estoq.quantidade.
                  ELSE
                      ASSIGN tt-itens.tt-consumo [idx-jr] = tt-itens.tt-consumo [idx-jr] + movto-estoq.quantidade
                             tt-itens.tt-consumo [13] = tt-itens.tt-consumo [13] + movto-estoq.quantidade.
            END.


            IF tt-param.pesq-jr = 2 THEN DO:

              IF movto-estoq.tipo-trans = 1 THEN
                 ASSIGN tt-itens.tt-consumo [idx-jr] = tt-itens.tt-consumo [idx-jr] - (movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-mob-m[1])
                        tt-itens.tt-consumo [13] = tt-itens.tt-consumo [13] - (movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-mob-m[1]).
                  ELSE
                      ASSIGN tt-itens.tt-consumo [idx-jr] = tt-itens.tt-consumo [idx-jr] + (movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-mob-m[1])
                             tt-itens.tt-consumo [13] = tt-itens.tt-consumo [13] + (movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-mob-m[1]).
            END.

        END. /*(movto-estoq)*/

    END. /*(item-estab)*/

END. /*(item)*/


FOR EACH tt-itens NO-LOCK
    BREAK BY tt-ge-codigo
          BY tt-fm-codigo
          BY tt-it-codigo.

    FIND FIRST ITEM WHERE 
        ITEM.it-codigo = tt-itens.tt-it-codigo
        NO-LOCK NO-ERROR.

    IF NOT AVAIL ITEM THEN NEXT.

    IF tt-param.destino <> 4 THEN DO:
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
            
        PUT STREAM str-rp " " AT 01
            tt-it-codigo AT 01
            " " ITEM.descricao-1 " Unid: " item.un " Fam¡lia: " tt-fm-codigo " Grupo: " tt-ge-codigo.
      
      
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp
            tt-consumo [1]     NO-LABEL
            tt-consumo [2]     NO-LABEL
            tt-consumo [3]     NO-LABEL
            tt-consumo [4]     NO-LABEL
            tt-consumo [5]     NO-LABEL
            tt-consumo [6]     NO-LABEL
            tt-consumo [7]     NO-LABEL
            tt-consumo [8]     NO-LABEL
            tt-consumo [9]     NO-LABEL
            tt-consumo [10]    NO-LABEL
            tt-consumo [11]    NO-LABEL
            tt-consumo [12]    NO-LABEL
            tt-consumo [13]    NO-LABEL
            with stream-io frame f-relat-09-132.
            down stream str-rp with frame f-relat-09-132. 
    END.

    IF tt-param.destino = 4 THEN DO:

        ASSIGN i-linha = i-linha + 1.

        ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-it-codigo       
               c-relatorio:range("B" + STRING(i-linha)):VALUE = item.desc-item       
               c-relatorio:range("C" + STRING(i-linha)):VALUE = item.un       
               c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-fm-codigo       
               c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-ge-codigo.       

        ASSIGN c-relatorio:range("F" + STRING(i-linha)):VALUE = tt-consumo [1]       
               c-relatorio:range("G" + STRING(i-linha)):VALUE = tt-consumo [2]       
               c-relatorio:range("H" + STRING(i-linha)):VALUE = tt-consumo [3]       
               c-relatorio:range("I" + STRING(i-linha)):VALUE = tt-consumo [4]       
               c-relatorio:range("J" + STRING(i-linha)):VALUE = tt-consumo [5]       
               c-relatorio:range("K" + STRING(i-linha)):VALUE = tt-consumo [6]       
               c-relatorio:range("L" + STRING(i-linha)):VALUE = tt-consumo [7]       
               c-relatorio:range("M" + STRING(i-linha)):VALUE = tt-consumo [8]       
               c-relatorio:range("N" + STRING(i-linha)):VALUE = tt-consumo [9]       
               c-relatorio:range("O" + STRING(i-linha)):VALUE = tt-consumo [10]       
               c-relatorio:range("P" + STRING(i-linha)):VALUE = tt-consumo [11]       
               c-relatorio:range("Q" + STRING(i-linha)):VALUE = tt-consumo [12]       
               c-relatorio:range("R" + STRING(i-linha)):VALUE = tt-consumo [13].    

    END. 

END. 

IF tt-param.destino = 4 THEN DO:

   RUN pi-finaliza-impressao.
   RUN pi-finalizar IN h-acomp.

   RETURN 'OK'.

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


if  tt-param.parametro then do:


   disp stream str-rp "SELE€ÇO" skip(01) with stream-io frame f-imp-sel.
   disp stream str-rp 
        with stream-io side-labels overlay row 034 frame f-imp-sel.

   disp stream str-rp "CLASSIFICA€ÇO" skip(01) with stream-io frame f-imp-cla.
   disp stream str-rp "  Estat¡stica de Consumo de Materiais"
        with stream-io side-labels overlay row 034 frame f-imp-cla.

   put stream str-rp unformatted skip(1) "IMPRESSÇO" skip(1).

   put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
   put stream str-rp unformatted skip "    " "Execu‡Æo: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
   put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
   put stream str-rp unformatted skip "    " "Usu rio : " tt-param.usuario.

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

    c-arquivo = c-arq + 'ESCE0013' + STRING(time)+ '.xls'.

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
