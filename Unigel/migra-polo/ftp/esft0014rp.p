/*****************************************************************************
**
**       Programa: esft0014rp.p
**
**       Data....: 07/12/2005
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Lista de Embarque (Nota Fiscal / Pallet)
**
**       VersÆo..: 1.00.000 - jrrcampos
**
**
*******************************************************************************/
def buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "esft0014RP".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").
{utp/utapi019.i}
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
    field c-cod-estabel-ini    like ped-venda.cod-estabel
    field classifica              as integer
    field desc-classifica         as char format "x(40)"
    FIELD nr-nota-fis-ini  AS CHAR FORMAT "x(07)"
    FIELD nr-nota-fis-fim  AS CHAR FORMAT "x(07)"
    FIELD c-serie                 AS CHAR FORMAT "x(5)"
    FIELD c-obs1                  AS CHAR FORMAT "x(30)"
    FIELD c-obs2                  AS CHAR FORMAT "x(50)"
    FIELD c-carreta               AS CHAR FORMAT "x(10)"
    FIELD c-container             AS CHAR FORMAT "x(15)"
    FIELD i-opcao                 AS INT
    FIELD l-email                 AS LOG.
DEFINE TEMP-TABLE tt-digita NO-UNDO
    FIELD pal-nr-pedido            AS INT  FORMAT "zzzzzz9" LABEL "Seq-NF"
    FIELD pal-nr-pallet            AS CHAR FORMAT "x(40)"   label "Nr.Palete"
    FIELD pal-nr-bobina            AS CHAR FORMAT "x(40)"   LABEL "Nr.Bobina"
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
    FIELD pal-cliente              AS CHAR LABEL "Cliente/Pedido" FORMAT "x(31)"
    INDEX chave IS PRIMARY UNIQUE pal-nr-pedido
                                  pal-nr-pallet
                                  pal-nr-bobina
    INDEX cliente-pal    pal-cliente
                         pal-nr-pedido 
                         pal-nr-pallet 
                         pal-nr-bobina .




/*Recebimento de Parametros*/

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FOR EACH tt-raw-digita:
   CREATE tt-digita.
   RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.
def new shared var v_han_acomp as handle no-undo. /* deve ser criada */

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
DEFINE VARIABLE sequencia-plt      AS INTEGER               NO-UNDO.
DEFINE VARIABLE linhas-jr          AS INTEGER               NO-UNDO.
DEFINE VARIABLE soma-bob           AS INTEGER               NO-UNDO.
DEFINE VARIABLE soma-plt           AS INTEGER               NO-UNDO.
DEFINE VARIABLE soma-vol           AS DECIMAL               NO-UNDO.
DEFINE VARIABLE soma-pliquido      AS DECIMAL               NO-UNDO.
DEFINE VARIABLE soma-pbruto        AS DECIMAL               NO-UNDO.
DEFINE VARIABLE larg-jr    AS INTEGER    NO-UNDO.
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

DEFINE VARIABLE c-lg-plt-1 AS DECIMAL FORMAT "9.999" .
 

DEFINE VARIABLE c-cm-plt-1 AS DECIMAL FORMAT "9.999" .
   

DEFINE VARIABLE c-al-plt-1 AS DECIMAL FORMAT "9.999" .
  



/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/* Vari veis usadas para gerar planilha excel. */
DEFINE VARIABLE c-anexo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-nr-nota-atu AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-emails AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-destino AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-cod-emitente AS INTEGER     NO-UNDO.

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


/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

/****************** Defini‡ao de Vari veis dos Calculos do Relat¢rio *************************/ 

/***************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/

def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.

/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form ped-venda.tp-pedido NO-LABEL                   format "x(1)" at 001 
     pal-nr-pedido       column-label "Seq-NF"      format ">>>>>>9" at 002
     pal-largura         column-label "Larg"        format ">>>9" at 010
     pal-diin            column-label "Diin"        format ">>>9" at 016
     pal-diex            column-label "Diex"        format ">>>9" at 022
     pal-it-codigo       column-label "Produto"     format "x(15)" at 028
     pal-qtd-plt         COLUMN-LABEL "Nr.Plt"      FORMAT ">>>>>9" AT 045
     pal-nr-bobinas      column-label "Nr.Bobinas"  format ">>>>>>>>9" at 052
     pal-lg-plt          COLUMN-LABEL "P-lg"        FORMAT ">>>9" AT 064
     pal-cm-plt          COLUMN-LABEL "P-Cm"        FORMAT ">>>9" AT 068
     pal-al-plt          COLUMN-LABEL "P-Al"        FORMAT ">>>9" AT 074
     pal-volume          column-label "Vol.M3"      FORMAT ">>9.9999" at 080
     pal-peso-liq        column-label "Peso-liq."   format ">>>>>>>9.99" at 089
     pal-peso-bru        column-label "Peso-Bruto"  format ">>>>>>>9.99" at 101
     with down width 132 no-box stream-io frame f-relat-09-132.

FORM
    "NOTA FISCAL :" AT 01
    tt-param.c-serie               FORMAT "x(5)"   AT 15
    tt-param.nr-nota-fis-ini   FORMAT "x(20)"  AT 25 
    "PAGINA NR.:" AT 80 "1" AT 95
    with down width 132 no-box stream-io frame f-relat-cab-1.


form HEADER
    fill("-", 132) format "x(132)" SKIP 
    WITH DOWN WIDTH 132 NO-BOX STREAM-IO FRAME f-relat-linha.

form 
     "Total ...........:" AT 017
     soma-plt            FORMAT ">>>>>9" AT 045
     soma-bob            format ">>>>>>>>9" at 052
     soma-vol            format ">>9.9999" at 080
     soma-pliquido       format ">>>>>>>9.99" at 089
     soma-pbruto         format ">>>>>>>9.99" at 101
     with down width 132 no-box stream-io frame f-relat-total.
    
form 
     "OBSERVA€åES:"        AT 001
     tt-param.c-obs1       AT 014
     "Carreta...:"         AT 078
     tt-param.c-carreta    AT 090 SKIP
     tt-param.c-obs2       AT 014
     "Container.:"         AT 078
     tt-param.c-container  AT 090 
     with down width 132 no-box stream-io frame f-relat-obs.
    
FORM
    "CLIENTE:" AT 70
    nota-fiscal.nome-ab-cli FORMAT "x(20)"  AT 79 
    "DATA.:" AT 101 data-jr FORMAT "99/99/9999" AT 108
    with down width 132 no-box stream-io frame f-relat-cab-2.


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

assign c-programa     = "esft0014rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Lista de Embarque (Nota Fiscal / Pallet)"
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
    "Per¡odo:" i-numper-x at 08 "-"
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

           
          /** Montando a tabela para tt-digita ***/
          run utp/ut-acomp.p persistent set h-acomp.
        
          {utp/ut-liter.i Gerando_movimento * I}
        
          run pi-inicializar in h-acomp (input "Carregando Paletes ...Aguarde"). 

          c-destino = "".


FOR EACH nota-fiscal WHERE
          nota-fiscal.cod-estabel = tt-param.c-cod-estabel-ini AND
          nota-fiscal.serie       = tt-param.c-serie           AND
          nota-fiscal.nr-nota-fis >= tt-param.nr-nota-fis-ini AND
          nota-fiscal.nr-nota-fis <= tt-param.nr-nota-fis-fim and
          nota-fiscal.dt-cancela = ? NO-LOCK USE-INDEX ch-nota,
    FIRST emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK
          BREAK BY nota-fiscal.cod-emitente
                BY nota-fiscal.nr-nota-fis.

    c-nr-nota-atu = nota-fiscal.nr-nota-fis.

    IF FIRST-OF(nota-fiscal.cod-emitente) THEN
        ASSIGN c-anexo = ""
               lin-jr = 0
               pedido-ant = 0
               i-cod-emitente = nota-fiscal.cod-emitente .
      
    IF FIRST-OF(nota-fiscal.nr-nota-fis) THEN DO:
         
        FOR EACH tt-digita.
            DELETE tt-digita.
        END.

        assign v-num-reg-lidos = 0.

        run pi-gera-digita. 

        RUN pi-gera-embarque.

        IF c-arq-anexo <> "" THEN DO:

            IF c-anexo = "" THEN
               c-anexo = c-arq-anexo.
            ELSE
               c-anexo = c-anexo + "," + c-arq-anexo.
        END.
    END.

    IF LAST-OF(nota-fiscal.cod-emitente) AND c-anexo <> "" AND tt-param.l-email THEN DO:

        RUN pi-envia-email.    

    END.
        
    
END.

RUN pi-finalizar IN h-acomp.

 
    


PROCEDURE pi-gera-embarque.


   /* Cria Aplica‡Æo do Excel */
 
   CREATE "Excel.Application" c-excel.
   ASSIGN c-excel:DisplayAlerts = FALSE.
   ASSIGN c-modelo-planilha = search("modelos/mod-esft0014.xls") 
          c-arq             = SESSION:TEMP-DIRECTORY.

   RUN pi-cria-planilha.  


   /* Rotina Cabe‡alho no Excel */

    IF tt-param.c-cod-estabel-ini <> "423" AND tt-param.c-cod-estabel-ini <> "413" THEN DO:/*solic-318*/
        ASSIGN i-linha = 3
               c-relatorio:range("B" + STRING(i-linha)):VALUE = 
                 "BR 386 - Km 423 - Via 1 - Nr.280 - / Distrito Industrial / Montenegro-RS / Brasil".
        ASSIGN i-linha = 4
               c-relatorio:range("B" + STRING(i-linha)):VALUE = 
                 "Phone: 55(51)457-5000 / Fax: 55(51)457-5050 ".
    END.

     ASSIGN i-linha = 10
            c-relatorio:range("D" + STRING(i-linha)):VALUE = STRING (string(tt-param.c-serie) + "-" + STRING(c-nr-nota-atu))
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


    IF tt-param.i-opcao = 2 AND tt-param.destino = 4 THEN
        ASSIGN c-relatorio:range("B7"):VALUE = "LISTA DE EMBARQUE (POR BOBINAS)"
               c-relatorio:range("B14"):VALUE = "/ Nr.Da Bobina".



    FOR EACH tt-digita.     
    
        ASSIGN tt-digita.pal-volume = (tt-digita.pal-lg-plt *
                                       tt-digita.pal-cm-plt *
                                       tt-digita.pal-al-plt).
           
    
        ASSIGN soma-qt-palete   = soma-qt-palete + 1.
    
        ASSIGN soma-bob = soma-bob + pal-nr-bobinas
               soma-vol = soma-vol + pal-volume
               soma-plt = soma-plt + pal-qtd-plt
               soma-pliquido = soma-pliquido + pal-peso-liq
               soma-pbruto   = soma-pbruto + pal-peso-bru. 
    
        IF tt-digita.pal-nr-pedido <> pedido-ant AND 
           pedido-ant <> 0  THEN DO:
    
           
            
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
    
         IF tt-param.i-opcao = 1 THEN
             ASSIGN c-relatorio:range("B" + STRING(i-linha)):VALUE = STRING (pal-nr-pedido) + "-" + STRING (pal-nr-pallet) + "-" + STRING (pal-cod-refer).
         ELSE
             ASSIGN c-relatorio:range("B" + STRING(i-linha)):VALUE = STRING (pal-nr-pedido) + "-" + STRING (pal-nr-pallet) + "-" + STRING (pal-nr-bobina).
    
     
         ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE = pal-largura
                c-relatorio:range("D" + STRING(i-linha)):VALUE = pal-diin
                c-relatorio:range("E" + STRING(i-linha)):VALUE = pal-diex
                c-relatorio:range("F" + STRING(i-linha)):VALUE = pal-it-codigo
                c-relatorio:range("G" + STRING(i-linha)):VALUE = pal-nr-bobinas
                c-relatorio:range("H" + STRING(i-linha)):VALUE = pal-peso-liq
                c-relatorio:range("I" + STRING(i-linha)):VALUE = pal-peso-bru
                c-relatorio:range("L" + STRING(i-linha)):VALUE = pal-cliente.
    
     
    
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


     ASSIGN i-linha = 75 + lin-jr
            c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-param.c-obs1

            i-linha = 76 + lin-jr
            c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-param.c-obs2

            i-linha = 72 + lin-jr
            c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-param.c-carreta

            i-linha = 73 + lin-jr
            c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-param.c-container

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

DEF VAR c-arquivo AS CHAR FORMAT "x(50)" NO-UNDO.

    c-arquivo = c-arq + 'pcklist-' + c-nr-nota-atu + "-" + replace(STRING(time,"HH:MM:SS"),":","-") + '.xls'. 

    OS-COPY VALUE(c-modelo-planilha) VALUE(c-arquivo).

    assign c-planilha  = c-excel:Workbooks:OPEN(c-arquivo)
           c-relatorio = c-excel:Sheets:item(1)
           c-arq-anexo = c-arquivo.

END PROCEDURE.


PROCEDURE pi-finaliza-impressao:

DEF VAR i         AS INT  NO-UNDO.
DEF VAR c-arquivo AS CHAR NO-UNDO.
 
    c-planilha:SAVE().
    c-planilha:CLOSE().

 
       c-excel:VISIBLE = true.

    DO i = 1 TO NUM-ENTRIES(c-arq-anexo):
        c-arquivo = ENTRY(i,c-arq-anexo).
        
        os-copy value(c-arquivo) v:\temp.
        c-planilha  = c-excel:Workbooks:OPEN(c-arquivo).

    END.

    /*c-excel:QUIT().*/
    RELEASE OBJECT c-excel.

END PROCEDURE.

return 'OK'.

/* fim do programa */

PROCEDURE pi-envia-email.
 
    /* *** Definicao de Parametros *** */

     
    /* *** Definicao de Variaveis Locais *** */
    
    DEF VAR c-nome                  LIKE usuar_mestre.nom_usuario           NO-UNDO.
    DEF VAR c-remetente             LIKE usuar_mestre.cod_e_mail_local      NO-UNDO.
    DEF VAR c-responsavel           LIKE usuar_mestre.cod_e_mail_local      NO-UNDO.
                DEFINE VARIABLE c-assunto AS CHARACTER  NO-UNDO.
    
    DEFINE VARIABLE c-copia-esp AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-corpo AS CHARACTER  NO-UNDO.
  

    FIND usuar_mestre NO-LOCK WHERE
             usuar_mestre.cod_usuario = c-seg-usuario NO-ERROR.
    
    ASSIGN c-remetente = ''.
    ASSIGN c-remetente = usuar_mestre.cod_e_mail_local WHEN AVAIL usuar_mestre.
  

    IF c-remetente = '' THEN DO:

      c-remetente = "polofilms@polofilms.com.br".
    
    


    END.
     
        c-destino  = "".

        FOR EACH  cont-emit WHERE 
            cont-emit.cod-emitente = i-cod-emitente AND
            substring(cont-emit.nome,1,8) = "LISTAEMB" and
            cont-emit.e-mail <> "" NO-LOCK .

            IF c-destino = "" THEN c-destino   = cont-emit.e-mail.
            ELSE
               c-destino = c-destino + "," +  cont-emit.e-mail .

        END.
        
        if c-destino = "" then 
            RETURN.


   
    FIND FIRST param-global NO-LOCK NO-ERROR.

     /* *** Delecao da Tabela Temporaria *** */
   
     FOR each tt-envio2 :  
         DELETE tt-envio2. 
     END.      
     FOR EACH tt-mensagem.
         DELETE tt-mensagem.
     END.
   
      c-assunto = "NOTAS FISCAIS POLOFILMS - INFORMA€åES DE EMBARQUE".
  
          c-corpo = "Segue em anexo arquivo(s) com informa‡äes de embarque.".
    
    RUN utp/utapi019.p persistent set h-utapi019.

    
    /*c-destino = "edson@damgra.com.br,ricardo.polimeno@unigel.com.br".
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
        

    DELETE procedure h-utapi019.
 
    RETURN.

    END PROCEDURE.


    PROCEDURE pi-gera-digita.

     
      ASSIGN larg-jr = 0
             diin-jr = 0
             diex-jr = 0.

      DO:

          assign v-num-reg-lidos = v-num-reg-lidos + 1.
          run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

        FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
          
            
      /*** Pesquisar a largura do item ***/
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
            
   
              
              IF tt-param.i-opcao = 1 THEN DO:

                  FIND FIRST  tt-digita WHERE
                       tt-digita.pal-nr-pedido = it-nota-fisc.nr-seq-fat AND
                       tt-digita.pal-nr-pallet = fat-ser-lote.nr-serlote AND
                       tt-digita.pal-nr-bobina = ""
                       NO-ERROR.
                
                  IF NOT AVAIL tt-digita THEN 
                       CREATE tt-digita.
                
                  ASSIGN tt-digita.pal-nr-pedido = it-nota-fisc.nr-seq-fat 
                         tt-digita.pal-nr-pallet = fat-ser-lote.nr-serlote
                         tt-digita.pal-nr-bobina = ""
                         . 
                
                  IF pallet.nr-pedido <> 0 THEN DO:
                      FIND FIRST ped-venda WHERE ped-venda.nr-pedido = pallet.nr-pedido NO-LOCK NO-ERROR.
                      IF AVAIL ped-venda THEN DO:
                          ASSIGN tt-digita.pal-cliente   = ped-venda.nome-abrev + "/" + string(pallet.nr-pedido) + "-" + STRING(pallet.nr-sequencia).
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
                         tt-digita.pal-nr-bobinas = pallet.nr-bobinas
                         tt-digita.pal-peso-liq   = pallet.peso-liq
                         tt-digita.pal-peso-bru   = pallet.peso-bruto.
                
                 ASSIGN tt-digita.pal-lg-plt     = c-lg-plt-1
                        tt-digita.pal-cm-plt     = c-cm-plt-1
                        tt-digita.pal-al-plt     = c-al-plt-1.
                
                 assign v-num-reg-lidos = v-num-reg-lidos + 1.
                 run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
              
              END.

              
              IF tt-param.i-opcao = 2 THEN DO:

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
                    
                     ASSIGN tt-digita.pal-lg-plt     = c-lg-plt-1
                            tt-digita.pal-cm-plt     = c-cm-plt-1
                            tt-digita.pal-al-plt     = c-al-plt-1.
                    
                     assign v-num-reg-lidos = v-num-reg-lidos + 1.
                     run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
              
                  END.

              END.  

          END.

        END.


     END.


    END PROCEDURE.

