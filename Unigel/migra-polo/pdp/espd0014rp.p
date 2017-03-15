/*****************************************************************************
**
**       Programa: espd0014rp.p
**
**       Data....: 02/03/04
**
**       Autor...: DATASUL S.A.
** 
**       Objetivo: SITUAÄ«O DO PEDIDO DE VENDA
**
**       Vers∆o..: 1.00.000 - jrrcampos
**
**       OBS.....: 
**
*******************************************************************************/
{bf/buffersUni2.i}

define buffer if-ped-venda for espmulti.if-ped-venda. 

define variable c-prog-gerado as character no-undo initial "espd0014RP".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.
DEFINE VARIABLE i-esp-docto AS INTEGER    NO-UNDO.
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
    field ep-codigo            as character
    field da-cod-estabel       like ped-venda.cod-estabel
    field da-nr-pedido         like ped-venda.nr-pedido
    FIELD nr-sequencia         AS INT
    .

DEFINE TEMP-TABLE TAB-palete NO-UNDO
    FIELD tt-pal-nr-pallet  LIKE pallet.nr-pallet
    FIELD tt-pal-peso-liq   LIKE pallet.peso-liquido
    FIELD tt-pal-peso-bru   LIKE pallet.peso-bruto
    FIELD tt-pal-dt-fatur   AS   DATE
    FIELD tt-pal-nota       AS   CHAR
    FIELD tt-pal-qt-bobs    AS   INT
    FIELD tt-pal-peso-fat   AS   DECIMAL
    FIELD tt-pal-peso-ori   AS   DECIMAL
    FIELD tt-nome-abrev       LIKE emitente.nome-abrev.

DEFINE TEMP-TABLE TAB-ord-prod NO-UNDO
    FIELD tt-nr-ord-produ  LIKE ord-prod.nr-ord-produ
    FIELD tt-quantidade    AS decimal.

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

def new global shared var i-ep-codigo-usuario  like mgmulti.empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def new global shared var i-num-ped-exec-rpw  as integer no-undo.   
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
def new global shared var l-rpc as logical no-undo.
def new global shared var r-registro-atual as rowid no-undo.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var i-num-ped as integer no-undo.         
def new global shared var v_cdn_empres_usuar   like mgmulti.empresa.ep-codigo        no-undo.
def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab     as handle                   no-undo.
def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
def new global shared var v_num_tip_aces_usuar   as int                      no-undo.
def new global shared var rw-log-exec            as rowid                    no-undo.


/****************** FIM INCLUDE COM VARIµVEIS GLOBAIS *********************/
/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 

/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 

def new shared var da-cod-estabel like ped-venda.cod-estabel format "999" no-undo.
def new shared var da-nr-pedido   like ped-venda.nr-pedido   format ">>>>>>9" no-undo.
def new shared var da-nr-sequencia like ped-item.nr-sequencia format ">>>>9" no-undo.


/****************** Definiáao de Vari†veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Definiáao de Vari†veis Campo do Layout do Relat¢rio **********************/ 
DEFINE BUFFER b-movto-estoq FOR movto-estoq.
DEFINE BUFFER bf-ped-venda  FOR ped-venda.

def var i-nr-pedido as integer no-undo.
def var c-nome-abrev as char no-undo.
def var saldo-ped as decimal label "Saldo".
DEFINE VARIABLE cod-estab-jr  AS CHARACTER FORMAT "X(3)"  NO-UNDO .
DEFINE VARIABLE nome-abrev-jr AS CHARACTER FORMAT "x(20)" NO-UNDO.
DEFINE VARIABLE status-jr     AS CHARACTER FORMAT "x(20)" NO-UNDO.
DEFINE VARIABLE largura-jr    AS INTEGER   FORMAT ">>>9"  NO-UNDO.
DEFINE VARIABLE diin-jr       AS INTEGER   FORMAT ">>>9"  NO-UNDO.
DEFINE VARIABLE diex-jr       AS INTEGER   FORMAT ">>>9"  NO-UNDO.
DEFINE VARIABLE ped-palete-jr AS decimal                  NO-UNDO.
DEFINE VARIABLE ped-embarq-jr AS decimal    NO-UNDO.
DEFINE VARIABLE ped-estoq-jr  AS decimal    NO-UNDO.
DEFINE VARIABLE estoque-kg-jr AS DECIMAL    NO-UNDO.
DEFINE VARIABLE estoque-br-jr AS DECIMAL    NO-UNDO.
DEFINE VARIABLE teorico-jr    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE qt-pal-estoq  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE kg-pal-estoq  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE br-pal-estoq  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE qt-pal-fatur  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE kg-pal-fatur  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE nr-pallet-imp AS CHARACTER  NO-UNDO.
DEFINE VARIABLE peso-liq-imp  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE peso-bru-imp  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE conta-lin     AS INTEGER    NO-UNDO.
DEFINE VARIABLE tot-qt-pal-estoq AS INTEGER    NO-UNDO.
DEFINE VARIABLE tot-qt-pal-fatur AS INTEGER    NO-UNDO.
DEFINE VARIABLE tot-qt-pal-pedid AS INTEGER    NO-UNDO.
DEFINE VARIABLE tot-kg-pal-estoq AS DECIMAL    NO-UNDO.
DEFINE VARIABLE tot-br-pal-estoq AS DECIMAL    NO-UNDO.
DEFINE VARIABLE tot-teorico      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE it-codigo-ant    AS CHARACTER  NO-UNDO INITIAL "XXX".
DEFINE VARIABLE saldo-jr         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE traco-jr         AS CHARACTER  FORMAT "x(132)" NO-UNDO.
DEFINE VARIABLE soma-outros      AS DECIMAL    NO-UNDO.


/*VARIAVEIS PARA CALCULO DE EMENDAS*/
    DEFINE  VAR nr-pedido-jr AS INT NO-UNDO.
    define  var nr-sequencia-jr as int no-undo.

    DEFINE  VAR bob-ped-53  AS INT NO-UNDO.
    DEFINE  VAR bob-pro-53  AS INT NO-UNDO.
    DEFINE  VAR bob-eme-53  AS INT NO-UNDO.
    DEFINE VARIABLE emenda-jr AS INTEGER    NO-UNDO.
    DEFINE VARIABLE perc-emendas AS DECIMAL     NO-UNDO.

    DEFINE BUFFER bf-ped-item  FOR ped-item.                          
    DEFINE BUFFER bf-pallet    FOR pallet.                          
    DEFINE BUFFER bf-it-pallet FOR it-pallet.                       
/*------------------------------------------*/
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

/* para executar o Excel */

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def new global shared var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.

DEF VAR c-arq             AS CHAR                NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR                NO-UNDO.   
DEF VAR l-cria            AS LOG                 NO-UNDO.
DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
DEF VAR i-linha           AS INT                 NO-UNDO.
DEF VAR c-coluna          AS char                NO-UNDO.


/****************** Definiáao de Forms do Relat¢rio 132 Colunas ***************************************/ 

FORM "Nr. do Pedido.:" AT 001 ped-venda.tp-pedido   FORMAT "x(1)" AT 017
                              ped-item.nr-pedcli    FORMAT "x(8)" at 018
     "Cliente.......:" AT 040 nome-abrev-jr         FORMAT "x(20)" AT 056 skip (1)
     "Produto.......:" AT 001 ped-item.it-codigo    format "x(15)" at 017
     "Larg.:"          AT 040 largura-jr            FORMAT ">>>9"  AT 046
     "D.Int:"          AT 051 diin-jr               FORMAT ">>>9"  AT 057
     "D.Ext:"          AT 062 diex-jr               FORMAT ">>>9"  AT 068 skip (2)
      traco-jr AT 001 
     "Qtde.Pedida...:"    AT 001 ped-item.qt-pedida    FORMAT "->>>,>>>,>>9.99" AT 017
     "Qtd.Paletizada...:" AT 036 ped-palete-jr         FORMAT "->>>,>>>,>>9.99" AT 055 skip (1)
     "Qtde.Embarcada:"    AT 001 ped-embarq-jr         FORMAT "->>>,>>>,>>9.99" AT 017
     "Saldo Ö Paletizar:" AT 036 saldo-jr              FORMAT "->>>,>>>,>>9.99" AT 055 
     with down width 132 no-box stream-io frame f-relat-09-132.

FORM
    nr-pallet-imp COLUMN-LABEL "Nr.Palete" FORMAT "x(12)" AT 01
    peso-liq-imp  COLUMN-LABEL "Saldo Atual" FORMAT ">>>>,>>9.99" AT 15
    peso-bru-imp  COLUMN-LABEL "Peso Bruto" FORMAT ">>>>,>>9.99" AT 28
    tt-pal-qt-bob COLUMN-LABEL "Bob.Pal"    FORMAT ">>>9" AT 041
    tt-pal-dt-fatur COLUMN-LABEL "Data Fat." FORMAT "99/99/9999" AT 50
    tt-pal-nota     COLUMN-LABEL "Nota Fiscal" FORMAT "x(10)" at 62
    tt-pal-peso-fat COLUMN-LABEL "Peso Fatur." FORMAT ">>>>,>>9.99" AT 75
    tt-pal-peso-ori COLUMN-LABEL "Kgs.Original" FORMAT ">>>>,>>9.99" AT 87
    tt-nome-abrev   COLUMN-LABEL "Ult.Destino" FORMAT "X(12)" AT 101
    WITH DOWN WIDTH 132 NO-BOX STREAM-IO FRAME f-relat-pallet.

FORM
    tt-nr-ord-produ COLUMN-LABEL "Ord.Prod." FORMAT ">>>>>>>9" AT 01
    tt-quantidade   COLUMN-LABEL "Qtd.Produzida" FORMAT ">>>>,>>9.99" AT 20
    WITH DOWN WIDTH 132 NO-BOX STREAM-IO FRAME f-relat-ord-prod.


form HEADER
    fill("-", 132) format "x(132)" SKIP 
    WITH DOWN WIDTH 132 NO-BOX STREAM-IO FRAME f-relat-linha.

    
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
define var c-empresa       as character format "x(30)"      no-undo.
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

assign c-programa     = "espd0014rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "SITUAÄ«O DO PEDIDO DE VENDA"
       c-sistema      = "".

if  tt-param.formato = 2 then do:


form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 40
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    fill("-", 112) format "x(110)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabec.

form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat AT 40
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

    assign da-cod-estabel = tt-param.da-cod-estabel
           da-nr-pedido   = tt-param.da-nr-pedido.
       
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
        IF tt-param.destino = 3 THEN
        assign v-cod-destino-impres = "Terminal".
        ELSE
            assign v-cod-destino-impres = "Excel".


run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.


IF tt-param.destino = 4 THEN DO:
    
    /* Cria Aplicaá∆o do Excel */

    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

    ASSIGN c-modelo-planilha = search("modelos/mod-espd0014.xls") 
           c-arq             = SESSION:TEMP-DIRECTORY.

    RUN pi-cria-planilha.


END. 

     
ASSIGN traco-jr = fill("-", 132). 

FIND FIRST ped-venda WHERE 
     ped-venda.nr-pedido = da-nr-pedido 
     NO-LOCK NO-ERROR.
     
 /*    
find first if-ped-venda where if-ped-venda.nr-pedido = ped-venda.nr-pedido no-lock no-error.

if avail if-ped-venda then do:
     find ped-venda WHERE ped-venda.nr-pedido =   if-ped-venda.nr-pedido-relac no-lock no-error.
     if not avail ped-venda then 
     FIND FIRST ped-venda WHERE 
     ped-venda.nr-pedido = da-nr-pedido 
     NO-LOCK NO-ERROR.
end.     
   */      
 
for each ped-item of ped-venda NO-LOCK
         where ped-item.nr-sequencia = tt-param.nr-sequencia AND
               /*ped-item.cod-sit-item < 5 AND*/
               ped-item.ind-componen < 3 /*AND
               ped-item.dt-canseq = ?*/ 
    /*USE-INDEX ch-cli-ped */:

    status-jr = "".

    CASE ped-item.cod-sit-item :
        WHEN 1 THEN status-jr = "* ABERTO *".
        WHEN 2 THEN status-jr = "* ATEND.PARCIAL *".
        WHEN 3 THEN status-jr = "* ATEND.TOTAL *".
        WHEN 4 THEN status-jr = "* PENDENTE *".
        WHEN 5 THEN status-jr = "* SUSPENSO *".
        WHEN 6 THEN status-jr = "* CANCELADO *".


    END CASE.

    assign saldo-ped = ped-item.qt-pedida - ped-item.qt-atendida.
    
    ASSIGN nome-abrev-jr = ped-venda.nome-abrev
           largura-jr    = 0
           diin-jr       = 0
           diex-jr       = 0
           ped-palete-jr = 0
           ped-embarq-jr = 0
           ped-estoq-jr  = 0
           estoque-kg-jr = 0
           estoque-br-jr = 0
           teorico-jr    = 0.

    FIND FIRST var-result 
         WHERE var-result.item-cotacao = ped-item.it-codigo
         AND var-result.nr-estrut    = ped-item.nr-config
         AND var-result.nome-var     = "LARGURA"  NO-LOCK NO-ERROR.

    IF AVAIL var-result THEN 
       ASSIGN largura-jr = var-result.valor-dec.
    
    FIND FIRST var-result 
         WHERE var-result.item-cotacao = ped-item.it-codigo
         AND var-result.nr-estrut    = ped-item.nr-config
         AND var-result.nome-var     = "DIIN"  NO-LOCK NO-ERROR.

    IF AVAIL var-result THEN 
       ASSIGN diin-jr = var-result.valor-dec.

    FIND FIRST var-result 
         WHERE var-result.item-cotacao = ped-item.it-codigo
         AND var-result.nr-estrut    = ped-item.nr-config
         AND var-result.nome-var     = "DIEX"  NO-LOCK NO-ERROR.

    IF AVAIL var-result THEN 
       ASSIGN diex-jr = var-result.valor-dec.
    
    ASSIGN qt-pal-estoq = 0
           kg-pal-estoq = 0
           qt-pal-fatur = 0
           kg-pal-fatur = 0
           br-pal-estoq = 0.
        
    FOR EACH tab-palete:
        DELETE tab-palete.
    END.

    i-nr-pedido = ped-venda.nr-pedido.
    
    
    run pi-saldo.
    
            FIND FIRST if-ped-venda WHERE
                if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido
                NO-LOCK NO-ERROR.
            
            IF AVAIL if-ped-venda THEN DO:
            
                i-nr-pedido = if-ped-venda.nr-pedido .
                
                run pi-saldo.
            end.

        FIND FIRST if-ped-venda WHERE
                if-ped-venda.nr-pedido = ped-venda.nr-pedido
                NO-LOCK NO-ERROR.
            
            IF AVAIL if-ped-venda THEN DO:
            
                i-nr-pedido = if-ped-venda.nr-pedido-relac .
                
                run pi-saldo.
            end.


    

    ASSIGN ped-embarq-jr = qt-pal-fatur
           ped-estoq-jr  = qt-pal-estoq
           estoque-kg-jr = kg-pal-estoq
           estoque-br-jr = br-pal-estoq.
      
      assign v-num-reg-lidos = v-num-reg-lidos + 1.
      run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

       /***  C‡DIGO PARA SA÷DA EM 132 COLUNAS ***/
/*       
       message
"kg-pal-estoq"  kg-pal-estoq skip
"ped-palete-jr"  ped-palete-jr skip
"kg-pal-fatur"   kg-pal-fatur

       view-as alert-box.
*/        
        ASSIGN ped-palete-jr = kg-pal-estoq
               ped-palete-jr = ped-palete-jr + kg-pal-fatur
               ped-embarq-jr = kg-pal-fatur
               saldo-jr = ped-item.qt-pedida - ped-palete-jr.

        IF saldo-jr < 0  THEN ASSIGN saldo-jr = 0.

        IF tt-param.destino <> 4 THEN DO:

           view stream str-rp frame f-cabec.
           view stream str-rp frame f-rodape.
           assign l-imprime = yes.
           display stream str-rp
               ped-venda.tp-pedido NO-LABEL
               ped-item.nr-pedcli  NO-LABEL
               nome-abrev-jr       NO-LABEL
               ped-item.it-codigo  NO-LABEL
               largura-jr          NO-LABEL
               diin-jr             NO-LABEL
               diex-jr             NO-LABEL
               traco-jr            NO-LABEL
               ped-item.qt-pedida  NO-LABEL
               ped-palete-jr       NO-LABEL
               ped-embarq-jr       NO-LABEL
               saldo-jr            NO-LABEL
               with stream-io frame f-relat-09-132.
               down stream str-rp with frame f-relat-09-132.
          
           view stream str-rp frame f-cabec.
           view stream str-rp frame f-rodape.
           assign l-imprime = yes.
           display stream str-rp 
           with stream-io frame f-relat-linha.
           down stream str-rp with frame f-relat-linha.

        END.
        
         c-nome-abrev = ped-venda.nome-abrev.
         
          FIND FIRST if-ped-venda WHERE
                if-ped-venda.nr-pedido = ped-venda.nr-pedido
                NO-LOCK NO-ERROR.
            
            IF AVAIL if-ped-venda THEN DO:
            
                FIND bf-ped-venda WHERE
                    bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac
                    NO-LOCK NO-ERROR.
            
                IF AVAIL bf-ped-venda THEN DO:

                   c-nome-abrev = bf-ped-venda.nome-abrev.
                end.
            end.
                

        IF tt-param.destino = 4 THEN DO:
        
        

            ASSIGN i-linha = 3.
        
            ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE = STRING ((ped-venda.tp-pedido) + STRING (ped-venda.nr-pedido))
                   c-relatorio:range("F" + STRING(i-linha)):VALUE = nome-abrev-jr  + if c-nome-abrev = nome-abrev-jr  then "" else " - (" + c-nome-abrev + ")"          
                   c-relatorio:range("J" + STRING(i-linha)):VALUE = ped-item.dt-entrega
                   c-relatorio:range("D" + STRING(i-linha)):VALUE = string("Sq.Pd: " + STRING(ped-item.nr-sequencia)) 
                   c-relatorio:range("J" + STRING(1)):VALUE = status-jr.
                        

            ASSIGN i-linha = 4.
        
            ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE = ped-item.it-codigo
                   c-relatorio:range("F" + STRING(i-linha)):VALUE = largura-jr           
                   c-relatorio:range("J" + STRING(i-linha)):VALUE = diex-jr               
                   c-relatorio:range("H" + STRING(i-linha)):VALUE = diin-jr.               

            ASSIGN i-linha = 6.
        
            ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE = ped-item.qt-pedida.
                  /* c-relatorio:range("G" + STRING(i-linha)):VALUE = ped-palete-jr.*/           


            ASSIGN i-linha = 7.
        
            ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE = ped-embarq-jr
                   c-relatorio:range("G" + STRING(i-linha)):VALUE = saldo-jr.           

            RUN pi-verifica-emendas.

            ASSIGN i-linha = 9.

            FIND FIRST if-ped-venda WHERE
                if-ped-venda.nr-pedido = ped-venda.nr-pedido
                NO-LOCK NO-ERROR.
            
            IF AVAIL if-ped-venda THEN DO:
            
                FIND bf-ped-venda WHERE
                    bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac
                    NO-LOCK NO-ERROR.
            
                IF AVAIL bf-ped-venda THEN DO:

                    ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE = bf-ped-venda.nr-pedido
                           c-relatorio:range("E" + STRING(i-linha)):VALUE = bf-ped-venda.nome-abrev
                           c-relatorio:range("H" + STRING(i-linha)):VALUE = bf-ped-venda.nr-pedcli
                           c-relatorio:range("J" + STRING(i-linha)):VALUE = bf-ped-venda.cod-estabel.

                END.

            END.

    END. /*tt-param.destino = 4*/

    
    FOR EACH tab-ord-prod:
        DELETE tab-ord-prod.
    END.

 



 FOR EACH ord-prod NO-LOCK WHERE
                 ord-prod.nome-abrev   = nome-abrev-jr and   
                 ord-prod.nr-pedido    = ped-item.nr-pedcli AND     
                 ord-prod.nr-sequencia = ped-item.nr-sequencia AND
                 ord-prod.it-codigo    = ped-item.it-codigo
        USE-INDEX cliente-ped :

 
        FIND FIRST tab-ord-prod WHERE
            tt-nr-ord-produ = ord-prod.nr-ord-produ
            NO-ERROR.

        IF NOT AVAIL tab-ord-prod THEN 
           CREATE tab-ord-prod.

        ASSIGN tt-nr-ord-produ = ord-prod.nr-ord-produ.

        for each movto-estoq no-lock
            WHERE movto-estoq.nr-ord-produ = ord-prod.nr-ord-produ AND 
            (movto-estoq.esp-docto = 1 OR movto-estoq.esp-docto = 8) AND
             movto-estoq.lote <> "recicl" and
             movto-estoq.cod-depos <> "ARC"
            USE-INDEX ord-seq.

            IF movto-estoq.esp-docto = 1 THEN
               ASSIGN tt-quantidade = tt-quantidade + movto-estoq.quantidade.
                ELSE
                  ASSIGN tt-quantidade = tt-quantidade - movto-estoq.quantidade.

        END.
    END.

    if nome-abrev-jr <> c-nome-abrev then do:
             
        FOR EACH ord-prod NO-LOCK WHERE
                     ord-prod.nome-abrev   = c-nome-abrev and   
                     ord-prod.nr-pedido    = ped-item.nr-pedcli AND     
                     ord-prod.nr-sequencia = ped-item.nr-sequencia AND
                     ord-prod.it-codigo    = ped-item.it-codigo
            USE-INDEX cliente-ped :
    
     
            FIND FIRST tab-ord-prod WHERE
                tt-nr-ord-produ = ord-prod.nr-ord-produ
                NO-ERROR.
    
            IF NOT AVAIL tab-ord-prod THEN 
               CREATE tab-ord-prod.
    
            ASSIGN tt-nr-ord-produ = ord-prod.nr-ord-produ.
    
            for each movto-estoq no-lock
                WHERE movto-estoq.nr-ord-produ = ord-prod.nr-ord-produ AND 
                (movto-estoq.esp-docto = 1 OR movto-estoq.esp-docto = 8) AND
                 movto-estoq.lote <> "recicl" and
                 movto-estoq.cod-depos <> "ARC"
                USE-INDEX ord-seq.
    
                IF movto-estoq.esp-docto = 1 THEN
                   ASSIGN tt-quantidade = tt-quantidade + movto-estoq.quantidade.
                    ELSE
                      ASSIGN tt-quantidade = tt-quantidade - movto-estoq.quantidade.
    
            END.
        END.
    end.
    ASSIGN conta-lin = 0
           soma-outros = 0
           i-linha   = 11.

      FOR EACH TAB-ord-prod NO-LOCK:

          ASSIGN conta-lin = conta-lin + 1. 

          IF tt-param.destino <> 4 THEN  DO:
          
             view stream str-rp frame f-cabec.
             view stream str-rp frame f-rodape.
             assign l-imprime = yes.
             display stream str-rp 
               tt-nr-ord-produ 
               tt-quantidade 
             with stream-io frame f-relat-ord-prod.
             down stream str-rp with frame f-relat-ord-prod.

          END.

          ELSE DO:

              if conta-lin > 7 then
                  assign soma-outros = soma-outros + tt-quantidade.

              IF conta-lin < 8 THEN DO:

                  IF conta-lin = 1 THEN ASSIGN c-coluna = "B".
                  IF conta-lin = 2 THEN ASSIGN c-coluna = "C".
                  IF conta-lin = 3 THEN ASSIGN c-coluna = "D".
                  IF conta-lin = 4 THEN ASSIGN c-coluna = "E".
                  IF conta-lin = 5 THEN ASSIGN c-coluna = "F".
                  IF conta-lin = 6 THEN ASSIGN c-coluna = "G".
                  IF conta-lin = 7 THEN ASSIGN c-coluna = "H".

                  ASSIGN c-relatorio:range(c-coluna + STRING(i-linha)):VALUE = tt-nr-ord-produ
                         c-relatorio:range(c-coluna + STRING(i-linha + 1)):VALUE = tt-quantidade.

              END.

          END.

      END.

      if soma-outros > 0 then
          assign c-relatorio:range("I" + STRING(11)):VALUE = "Outras"
                 c-relatorio:range("I" + STRING(12)):VALUE = soma-outros.

      IF conta-lin <> 0 AND tt-param.destino <> 4 THEN DO:
         view stream str-rp frame f-cabec.
          view stream str-rp frame f-rodape.
          assign l-imprime = yes.
          display stream str-rp 
            with stream-io frame f-relat-linha.
            down stream str-rp with frame f-relat-linha.
      END.
      
      ASSIGN conta-lin = 0
             i-linha   = 14.

      FOR EACH TAB-palete NO-LOCK:
          ASSIGN nr-pallet-imp = tt-pal-nr-pallet.
          ASSIGN peso-liq-imp  = tt-pal-peso-liq.
          ASSIGN peso-bru-imp  = tt-pal-peso-bru.
          ASSIGN conta-lin = 9.

          IF tt-param.destino <> 4 THEN  DO:
              view stream str-rp frame f-cabec.
              view stream str-rp frame f-rodape.
              assign l-imprime = yes.
              display stream str-rp 
                nr-pallet-imp 
                peso-liq-imp 
                peso-bru-imp
                tt-pal-qt-bobs
                tt-pal-dt-fatur
                tt-pal-nota
                tt-pal-peso-fat
                tt-pal-peso-ori
                tt-nome-abrev
                with stream-io frame f-relat-pallet.
                down stream str-rp with frame f-relat-pallet.
          END.

          ELSE DO:

            ASSIGN i-linha = i-linha + 1.
        
            ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = nr-pallet-imp  
                   c-relatorio:range("B" + STRING(i-linha)):VALUE = peso-liq-imp    
                   c-relatorio:range("C" + STRING(i-linha)):VALUE = peso-bru-imp     
                   c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-pal-qt-bobs   
                   c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-pal-dt-fatur  
                   c-relatorio:range("F" + STRING(i-linha)):VALUE = tt-pal-nota      
                   c-relatorio:range("G" + STRING(i-linha)):VALUE = tt-pal-peso-fat  
                   c-relatorio:range("H" + STRING(i-linha)):VALUE = tt-pal-peso-ori  
                   c-relatorio:range("I" + STRING(i-linha)):VALUE = tt-nome-abrev.    

          END.

      END.

      IF conta-lin <> 0 AND tt-param.destino <> 4 THEN DO:
         view stream str-rp frame f-cabec.
          view stream str-rp frame f-rodape.
          assign l-imprime = yes.
          display stream str-rp 
            with stream-io frame f-relat-linha.
            down stream str-rp with frame f-relat-linha.
      END.
 END.

 IF it-codigo-ant <> "XXX" AND tt-param.destino <> 4 THEN DO:

            view stream str-rp frame f-cabec.
            view stream str-rp frame f-rodape.
            assign l-imprime = yes.
            display stream str-rp 
            with stream-io frame f-relat-linha.
            down stream str-rp with frame f-relat-linha.
          
 END.


 IF tt-param.destino = 4 THEN DO:

    RUN pi-finaliza-impressao.
    RUN pi-finalizar IN h-acomp.

    RETURN 'OK'.

 END. 


 run pi-finalizar in h-acomp.


 IF tt-param.destino <> 4 THEN DO:


        if  l-imprime = no then do:
            if  tt-param.formato = 2 then do:
                view stream str-rp frame f-cabec.
                view stream str-rp frame f-rodape.
            end.
            disp stream str-rp " " with stream-io frame f-nulo.
        end.
        
        
        if  tt-param.destino <> 1 then
        
            page stream str-rp.
        
        else do:
        
            if   tt-param.parametro = yes then
        
                 page stream str-rp.
        
        end.
        
        if  tt-param.parametro then do:
        
        
           disp stream str-rp "SELEÄ«O" skip(01) with stream-io frame f-imp-sel.
           disp stream str-rp 
                da-nr-pedido no-label  
                with stream-io side-labels overlay row 038 frame f-imp-sel.
        
           disp stream str-rp "CLASSIFICAÄ«O" skip(01) with stream-io frame f-imp-cla.
           disp stream str-rp "   SITUAÄ«O DO PEDIDO DE VENDA"
                with stream-io side-labels overlay row 038 frame f-imp-cla.
        
           put stream str-rp unformatted skip(1) "IMPRESS«O" skip(1).
        
           put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
           put stream str-rp unformatted skip "    " "Execuá∆o: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
           put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
           put stream str-rp unformatted skip "    " "Usu†rio : " tt-param.usuario.
        
        end.
        
        else
            output stream str-rp close.

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

    c-arquivo = c-arq + 'escp030' + STRING(time)+ '.xls'.

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

PROCEDURE pi-verifica-emendas.


    ASSIGN nr-pedido-jr = ped-venda.nr-pedido
           nr-sequencia-jr = tt-param.nr-sequencia.

    ASSIGN bob-ped-53 = 0
           bob-pro-53 = 0
           bob-eme-53 = 0.

    run pi-soma-emendas.
    
    find first if-ped-venda where if-ped-venda.nr-pedido =  ped-venda.nr-pedido  no-lock no-error.
    if avail if-ped-venda then do:
         nr-pedido-jr = if-ped-venda.nr-pedido-relac.
         run pi-soma-emendas.
    end. 
    find first if-ped-venda where if-ped-venda.nr-pedido-relac =  ped-venda.nr-pedido no-lock no-error.
    if avail if-ped-venda then do:
         nr-pedido-jr = if-ped-venda.nr-pedido.
         run pi-soma-emendas.
    end.    
         

    IF bob-eme-53 > 0 THEN
       ASSIGN perc-emendas = bob-eme-53 / bob-pro-53.
    ELSE
        ASSIGN perc-emendas = 0.

    ASSIGN c-relatorio:range("J" + STRING(7)):VALUE = perc-emendas.


END PROCEDURE.


 procedure pi-soma-emendas.   
    FIND FIRST bf-ped-venda WHERE 
               bf-ped-venda.nr-pedido = nr-pedido-jr
               NO-LOCK  NO-ERROR.

    FIND FIRST bf-ped-item OF bf-ped-venda WHERE
               bf-ped-item.ind-componen  <> 3 and
               bf-ped-item.nr-sequencia = nr-sequencia-jr 
               NO-LOCK NO-ERROR.

    IF AVAIL bf-ped-venda AND AVAIL bf-ped-item THEN DO:

        FIND FIRST var-result WHERE
             var-result.nome-var     = "qtdbob"                AND 
             var-result.nr-estrut    = bf-ped-item.nr-config   AND
             var-result.item-cotacao = bf-ped-item.it-codigo 
             NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
             ASSIGN bob-ped-53 = var-result.valor-dec.

        FOR EACH bf-pallet WHERE
            bf-pallet.nr-pedido   = bf-ped-venda.nr-pedido AND
            bf-pallet.nr-sequencia = bf-ped-item.nr-sequencia and 
            bf-pallet.cod-estabel = bf-ped-venda.cod-estabel and
            bf-pallet.situacao    = 2
            USE-INDEX pedido
            NO-LOCK ,

            EACH bf-it-pallet OF bf-pallet NO-LOCK.

              ASSIGN bob-pro-53 = bob-pro-53 + 1.

              FIND FIRST lote-carac-tec WHERE
                              lote-carac-tec.it-codigo = bf-it-pallet.it-codigo
                              and lote-carac-tec.lote  = bf-it-pallet.lote-bobina
                              and lote-carac-tec.cd-comp = "emenda"
                              NO-LOCK NO-ERROR.

              if avail lote-carac-tec then
                 ASSIGN emenda-jr = lote-carac-tec.vl-resul.

              IF emenda-jr <> 0 THEN
                  ASSIGN bob-eme-53 = bob-eme-53 + 1.


        END.



    END.                                              

end procedure.


    procedure pi-saldo.
    
    def var c-estoq-estab as char no-undo.
    
    
    FOR EACH pallet WHERE
                 pallet.it-codigo = ped-item.it-codigo and  
                 pallet.nr-pedido = i-nr-pedido AND
                 pallet.nr-sequencia = ped-item.nr-sequencia AND
                 pallet.situacao = 2
                 NO-LOCK:
           
 
       CREATE TAB-palete.
       ASSIGN tt-pal-nr-pallet = pallet.nr-pallet
              tt-pal-qt-bobs   = pallet.nr-bobinas
              tt-pal-peso-bru = pallet.peso-bruto
              tt-pal-peso-ori = pallet.peso-liquido.
       
       FOR EACH saldo-estoq NO-LOCK WHERE
                saldo-estoq.lote = pallet.nr-pallet AND
                saldo-estoq.cod-depos <> "ARC"
                USE-INDEX lote :

          IF saldo-estoq.qtidade-atu > 0 THEN 
             ASSIGN qt-pal-estoq = qt-pal-estoq + 1
                    kg-pal-estoq = kg-pal-estoq + saldo-estoq.qtidade-atu
                    br-pal-estoq = br-pal-estoq + pallet.peso-bruto
                    tt-pal-peso-liq = saldo-estoq.qtidade-atu
                    c-estoq-estab = saldo-estoq.cod-estabel.

       END.

         da-cod-estabel = pallet.cod-estabel.
         
         
       FOR EACH deposito WHERE deposito.ind-acabado = TRUE NO-LOCK.
          DO i-esp-docto = 22 TO 23:
            FOR EACH movto-estoq WHERE
                     movto-estoq.cod-estabel = da-cod-estabel AND
                     movto-estoq.it-codigo = pallet.it-codigo AND
                     movto-estoq.lote = pallet.nr-pallet AND
                     movto-estoq.cod-depos = deposito.cod-depos AND 
                     movto-estoq.esp-docto = i-esp-docto
            
                     AND movto-estoq.tipo-trans = 2 :
                     
                     
                 IF  movto-estoq.esp-docto = 22 THEN DO:
                      FIND  natur-oper WHERE natur-oper.nat-operacao = movto-estoq.nat-operacao NO-LOCK NO-ERROR.
                      IF NOT AVAIL natur-oper THEN NEXT.
                      
                      

                      IF  natur-oper.terceiros THEN DO: 
                      
                   
                      
                           IF CAN-FIND(FIRST b-movto-estoq WHERE
                                   b-movto-estoq.cod-estabel = movto-estoq.cod-estabel AND
                                   b-movto-estoq.it-codigo   = movto-estoq.it-codigo AND
                                   b-movto-estoq.lote = movto-estoq.lote AND
                                   b-movto-estoq.cod-depos = movto-estoq.cod-depos AND
                                   b-movto-estoq.cod-localiz = movto-estoq.cod-localiz AND
                                   b-movto-estoq.esp-docto = 22  AND
                                   b-movto-estoq.cod-emitente <> movto-estoq.cod-emitente)
                                       THEN NEXT.
                            IF CAN-FIND(LAST b-movto-estoq WHERE
                                   b-movto-estoq.cod-estabel = movto-estoq.cod-estabel AND
                                   b-movto-estoq.it-codigo   = movto-estoq.it-codigo AND
                                   b-movto-estoq.lote = movto-estoq.lote AND
                                   b-movto-estoq.cod-depos = movto-estoq.cod-depos AND
                                   b-movto-estoq.cod-localiz = movto-estoq.cod-localiz AND
                                   b-movto-estoq.esp-docto = 22  AND
                                   b-movto-estoq.nro-docto > movto-estoq.nro-docto)
                                       THEN NEXT.
                                       
                                   
                                       
                           IF CAN-FIND(FIRST b-movto-estoq WHERE
                                  b-movto-estoq.cod-estabel = movto-estoq.cod-estabel AND
                                  b-movto-estoq.it-codigo   = movto-estoq.it-codigo AND
                                  b-movto-estoq.lote = movto-estoq.lote AND
                                  b-movto-estoq.cod-depos = movto-estoq.cod-depos AND
                                  b-movto-estoq.cod-localiz = movto-estoq.cod-localiz AND
                                  b-movto-estoq.esp-docto = 23 AND 
                                  b-movto-estoq.tipo-trans = 2 and
                                  b-movto-estoq.cod-emitente <> movto-estoq.cod-emitente use-index item-est-dep)
                                      THEN NEXT.
                      END.
                 END.
   
                     
                   ASSIGN qt-pal-fatur = qt-pal-fatur + 1.
            
                   FIND emitente WHERE emitente.cod-emitente = movto-estoq.cod-emitente NO-LOCK NO-ERROR.
                   
                   if movto-estoq.esp-docto = 22 then
                   ASSIGN  kg-pal-fatur = kg-pal-fatur +  movto-estoq.quantidade * (if movto-estoq.tipo-trans = 2 then 1 else -1)
                           tt-pal-peso-fat = tt-pal-peso-fat +  movto-estoq.quantidade * (if movto-estoq.tipo-trans = 2 then 1 else -1).
                   assign 
                           tt-pal-dt-fatur = movto-estoq.dt-trans
                           tt-pal-nota     = movto-estoq.nro-docto
                           tt-nome-abrev   = IF AVAIL emitente THEN emitente.nome-abrev ELSE "".
                           
                        
            END.
                     /* comentado para melhorar performance pois faz tempo que n∆o existe estabelecimetno 424, isso foi na virada da implantaá∆o de unigel comercial
                  if tt-nome-abrev = "" and (pallet.cod-estabel = "432" OR pallet.cod-estabel = "443") then do: /*solic-318*/ 
                     
                       FOR EACH movto-estoq WHERE
                            movto-estoq.cod-estabel = "424" AND
                            movto-estoq.it-codigo = pallet.it-codigo AND
                            movto-estoq.lote = pallet.nr-pallet AND
                            movto-estoq.cod-depos = deposito.cod-depos AND 
                            movto-estoq.esp-docto = i-esp-docto
                   
                            AND movto-estoq.tipo-trans = 2 :
                            
                              FIND emitente WHERE emitente.cod-emitente = movto-estoq.cod-emitente NO-LOCK NO-ERROR.
                 
                            

                            
                             assign 
                                tt-pal-dt-fatur = movto-estoq.dt-trans
                                tt-pal-nota     = movto-estoq.nro-docto
                                tt-nome-abrev   = IF AVAIL emitente THEN emitente.nome-abrev ELSE "".

                       end.
                       
                   end.  
                   */
                   
          END.
       END.
       
       if tt-nome-abrev = "" and c-estoq-estab <> "" then do:
               FIND emitente WHERE emitente.cod-emitente = INT(c-estoq-estab) NO-LOCK NO-ERROR.
                        
               tt-nome-abrev = IF AVAIL emitente THEN emitente.nome-abrev ELSE c-estoq-estab.   
       end.
    END.
end procedure.

return 'OK'.

/* fim do programa */
