/*****************************************************************************
**
**Programa.: esft0028rp.p
**Autor....: Amgra - Jos� Roberto
**Objetivo.: Etiqueta Especial - Bobinas Philip Morris 
**Data.....: 25/09/2008
**
***************************************************************************/
def buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "esft0028rp".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

/****************** Defini��o de Tabelas Tempor�rias do Relat�rio **********************/

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
    field classifica           as integer
    field desc-classifica      as char format "x(40)"
    field i-nr-bobinas         AS integer
    field c-cod-estabel-ini    like ped-venda.cod-estabel
    field c-nr-pedido-ini      like ped-venda.nr-pedido
    field c-nr-sequencia-ini   like ped-item.nr-sequencia
    field c-nr-pallet-ini      like pallet.nr-pallet
    field i-seq-ini            AS INT 
    field dir-etq              as char.


DEFINE TEMP-TABLE tt-digita no-undo
    FIELD pal-nr-pedido            AS INT                          FORMAT "zzzzzz9"         LABEL "Pedido"
    FIELD pal-nr-sequencia         AS INT                          FORMAT "zzzz9"           LABEL "Sq.Ped"
    FIELD pal-nr-pallet            LIKE pallet.nr-pallet           FORMAT "x(10)"           LABEL "Nr.Pallet"
    FIELD pal-it-codigo            LIKE pallet.it-codigo           FORMAT "x(16)"           LABEL "Item"
    FIELD pal-nr-bobinas           LIKE pallet.nr-bobinas          FORMAT ">>>>>>9"         LABEL "Qt.Bobs"
    FIELD pal-prod-cli             AS CHAR                         FORMAT "x(20)"           LABEL "Cod.Prod.Cliente"
    FIELD pal-pedcli               AS CHAR                         FORMAT "x(20)"           LABEL "Nr.Ped.Cliente"
    FIELD pal-largura              AS INT                          FORMAT ">>>9"            LABEL "Larg"
    FIELD pal-obsetq               AS CHAR                         FORMAT "x(20)"           LABEL "Metragem"
    FIELD pal-nome-abrev           LIKE ped-venda.nome-abrev       FORMAT "x(20)"           LABEL "Cliente"
    INDEX chave IS PRIMARY UNIQUE pal-nr-pedido
                                  pal-nr-sequencia
                                  pal-nr-pallet.


/****************** INCLUDE COM VARI�VEIS GLOBAIS *********************/

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

def new shared var i-num9100     as integer.
def new shared var i-tam9100     as integer.
def new shared var de-val9100    as decimal.
def new shared var c-ext9100     as character extent 10.

/****************** FIM INCLUDE COM VARI�VEIS GLOBAIS *********************/

/****************** Defini�ao de Par�metros do Relat�rio *********************/ 

/****************** Defini�ao de Vari�veis de Sele��o do Relat�rio *********************/ 

/****************** Defini�ao de Vari�veis p/ Campos Virtuais do Relat�rio *******************/ 

DEFINE VARIABLE qt-tot-pallets   AS INTEGER    NO-UNDO.
DEFINE VARIABLE filme-jr         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE barra-1          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE barra-2          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE barra-3          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE seq-bobinas      AS INTEGER    NO-UNDO.
DEFINE VARIABLE seq-bobinas-jr   AS INTEGER    NO-UNDO.
DEFINE VARIABLE prod-cliente     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pedcli-jr        AS CHARACTER  NO-UNDO.

DEFINE VARIABLE var-b            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE var-c            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE var-de           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE var-g            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE var-h            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE var-i            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE var-k            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE var-k1           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE var-f1           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE var-f2           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE var-pallet       AS CHARACTER  NO-UNDO.

DEFINE VARIABLE b-idx            AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-idx            AS INTEGER    NO-UNDO.
DEFINE VARIABLE d-data           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE d-data-1         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-data-val       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE d-data-val       AS date  NO-UNDO.



DEFINE VARIABLE seq-pallet       AS INTEGER    NO-UNDO.
def var c-output as char no-undo.
def var c-disp   as char no-undo.

/****************** Defini�ao de Vari�veis Campo do Layout do Relat�rio **********************/ 

/****************** Defini�ao de Vari�veis do Relat�rio N�o Pedidas em Tela ******************/ 

/****************** Defini�ao de Vari�veis de Total do Relat�rio *****************************/ 

/****************** Defini�ao de Vari�veis dos Calculos do Relat�rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Defini�ao de Vari�veis de Processamento do Relat�rio *********************/

def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.
DEF VAR i         AS INT  NO-UNDO.  
DEF VAR c-arquivo AS CHAR NO-UNDO.
DEF VAR c-arquivo-t AS CHAR NO-UNDO.

/****************** Defini�ao de Forms do Relat�rio 132 Colunas ***************************************/ 


form HEADER
    fill("-", 132) format "x(132)" SKIP 
    WITH DOWN WIDTH 132 NO-BOX STREAM-IO FRAME f-relat-linha.

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
define var c-tt-digita-relat  as character format "x(50)"      no-undo.
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

assign c-programa     = "esft0028rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-tt-digita-relat = "Etiqueta Bobinas - Philip Morris"
       c-sistema      = "".

if  tt-param.formato = 2 then do:


form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-tt-digita-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    fill("-", 112) format "x(110)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabec.

form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-tt-digita-relat at 50
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

       
FIND first empresa no-lock
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
        assign v-cod-destino-impres = "Terminal".


def stream etiqueta.


find first usuar_mestre
     where usuar_mestre.cod_usuario = c-seg-usuario no-lock no-error.
if avail usuar_mestre then 
   assign tt-param.dir-etq = usuar_mestre.nom_dir_spool.

if substring(tt-param.dir-etq, length(tt-param.dir-etq), 1) = "\" or
   substring(tt-param.dir-etq, length(tt-param.dir-etq), 1) = "/" then
   assign tt-param.dir-etq = substring(tt-param.dir-etq, 1, length(tt-param.dir-etq) - 1).

assign c-output = tt-param.dir-etq + "\etq"  + substring(c-seg-usuario,1,3) + replace(string(time, "hh:mm:ss"), ":", "") + ".lst"
       c-output = replace(c-output, "/", "\").

assign tt-param.arquivo = c-output.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat�rio").

assign v-num-reg-lidos = 0.


for each tt-digita NO-LOCK :

    FIND emitente WHERE emitente.nome-abrev = tt-digita.pal-nome-abrev
            NO-LOCK NO-ERROR.

    IF NOT AVAIL emitente THEN NEXT.

    ASSIGN seq-bobinas-jr = tt-param.i-seq-ini
           seq-bobinas    = 1.

    ASSIGN b-idx    = 0
           c-idx    = 0
           d-data   = ""
           d-data-1 = "".

    FIND FIRST polo-esp-cliente-cq WHERE
               polo-esp-cliente-cq.cod-estabel = tt-param.c-cod-estabel-ini AND
               polo-esp-cliente-cq.nome-abrev  = tt-digita.pal-nome-abrev   AND
               polo-esp-cliente-cq.it-codigo   = tt-digita.pal-it-codigo   
               NO-LOCK NO-ERROR.

    IF AVAIL polo-esp-cliente-cq THEN DO:

        ASSIGN b-idx = INDEX(polo-esp-cliente-cq.descricao,tt-digita.pal-prod-cli,1).
        
        IF b-idx > 0 THEN
            ASSIGN c-idx = INDEX(polo-esp-cliente-cq.descricao,"-",b-idx).
        
        IF c-idx > 0 THEN
            ASSIGN d-data = SUBSTRING(polo-esp-cliente-cq.descricao,c-idx + 1,8).


    END.

    ASSIGN d-data-1 = REPLACE(d-data,".","").




    ASSIGN filme-jr = tt-digita.pal-it-codigo + " / Width: " + string(tt-digita.pal-largura) + " / Length: " +
                      substring(tt-digita.pal-obsetq,1,5).

    ASSIGN prod-cliente = REPLACE(tt-digita.pal-prod-cli,".","").

    ASSIGN barra-1 = string(prod-cliente, "999999") + 
                     STRING (today,"999999") + "000000" +
                     STRING(int(tt-digita.pal-nr-pedido)) + 
                     "A".

    ASSIGN var-b = STRING (tt-digita.pal-prod-cli).

    ASSIGN pedcli-jr = SUBSTRING (tt-digita.pal-pedcli,1,10) + "  " +
                       SUBSTRING (tt-digita.pal-pedcli,11,2).
    
    ASSIGN var-c = "PO: " + string(pedcli-jr).

    ASSIGN var-de = "POLO / " + "POG".

    ASSIGN var-g = "TS: " + STRING (tt-digita.pal-prod-cli) +
           ". POG." + STRING(d-data).

    ASSIGN var-i = "Width: " + string(tt-digita.pal-larg) + " mm".

    ASSIGN var-k = "Length: " + string(tt-digita.pal-obsetq) + " m".

    ASSIGN var-k1 = "Batch No: " + string(int(tt-digita.pal-nr-pedido)) + "-" +
                                   STRING(int(tt-digita.pal-nr-sequencia)).

    ASSIGN var-h = string(tt-digita.pal-it-codigo).
    
    assign seq-pallet = 1.

    ASSIGN b-idx = 0
           b-idx = INDEX(tt-digita.pal-nr-pallet,"/",1).

    IF b-idx > 0 THEN
       ASSIGN b-idx = b-idx + 1
              seq-pallet = INT(substring(tt-digita.pal-nr-pallet,b-idx,LENGTH(tt-digita.pal-nr-pallet))).    
    

    DO WHILE seq-bobinas <= tt-digita.pal-nr-bobinas:

        ASSIGN var-k1 = "Batch No: " + string(int(tt-digita.pal-nr-pedido)) + "-" +
                                       STRING(int(tt-digita.pal-nr-sequencia)).

        ASSIGN var-pallet = " - P" + string(seq-pallet, "999") 
            + " - B" + STRING (seq-bobinas-jr, "999").

        ASSIGN var-k1 = var-k1 + var-pallet.
        
        
        /*Data de validade fixada em 9 meses da data de impress�o*/
        
        d-data-val = today   + 30 * 10.
        d-data-val = date(month(d-data-val),1,year(d-data-val)) - 1.
        c-data-val = "Expire:" + string(d-data-val,"99/99/9999").
        
  
        
        output stream etiqueta to value(c-output) paged page-size 64 convert target "iso8859-1".


        put stream etiqueta unformatted

             ' '   skip
             '" "' skip
             'N'   skip
             'q800'    skip
             'Q639,24+0'   skip
             'S2' skip
              if tt-param.c-cod-estabel-ini = '422' then ('D7') else 'D1' skip
             'ZT' skip
             'A50,567,3,3,2,2,N,"' var-h '"' skip
             'A125,548,3,3,2,2,N,"' var-de '"' SKIP
             'A188,470,3,3,2,1,N,"' var-g '"' SKIP
             'A255,424,3,3,2,1,N,"' var-i '"' SKIP
             'A308,430,3,3,2,1,N,"' var-k '"' SKIP
             'A364,548,3,3,2,1,N,"' var-k1 '"' SKIP
             'B408,540,3,1,2,10,102,B,"' TRIM(barra-1) '"' skip
             'A552,384,3,4,3,2,N,"' var-b '"' SKIP
             'A635,548,3,3,2,2,N,"' var-c '"' SKIP
             'A690,420,3,3,2,2,N,"<--------->"' SKIP
             'A740,384,3,3,2,1,N,"' c-data-val '"'  SKIP
             'P1'          skip. 

        ASSIGN seq-bobinas = seq-bobinas + 1
               seq-bobinas-jr = seq-bobinas-jr + 1.
 

        output stream etiqueta close.

        find first imprsor_usuar
             where imprsor_usuar.cod_usuario       = c-seg-usuario 
               and imprsor_usuar.log_imprsor_princ = yes no-lock no-error.
        if avail imprsor_usuar then do:
           
         
           assign c-disp = imprsor_usuar.nom_disposit_so.

         /*  IF c-seg-usuario = "super" THEN c-disp = "\\polvga-epc04\eltron".*/

           if c-disp = "printer" then c-disp = "LPT1".
             
           dos silent type value(c-output) > value(c-disp). 
           dos silent type value(c-output) > value(c-disp). 
           
        END.

    END.


END.

run pi-finalizar in h-acomp.

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

/*



skip 
'" "'
'N'
q800
Q639,24+0
S2
D8
ZT
A50,567,3,3,2,2,N,"WRAPPING FILM XXXXX"
A125,548,3,3,2,2,N,"VENDOR NAME /  ABC"
A188,470,3,3,2,1,N,"TS: 46.0378.ABC.02.04.03"
A255,424,3,3,2,1,N,"Width: 110 mm"
A308,430,3,3,2,1,N,"Length: 3000 m"
A364,451,3,3,2,1,N,"Batch No: T1234"
B428,518,3,1,2,2,102,B,"460378120207000000t1234A"
A572,384,3,4,3,2,N,"46.0378"
A655,518,3,3,2,2,N,"PO: 0004745615 01"
'A710,420,3,3,2,2,N,"<--------->"'
'P1'
skip
*/



return 'OK'.

/* fim do programa */