/*****************************************************************************
**
**       Programa: escp0030rp.p
**
**       Data....: 15/08/2007
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Bobinas Cortadas no Per°odo
**
**       Vers∆o..: 1.00.000 - jrrcampos
**
**       OBS.....: 
**
*******************************************************************************/
define variable c-prog-gerado as character no-undo initial "escp0030RP".
define buffer empresa for mgmulti.empresa.
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
    field c-cod-estabel-ini    like movto-estoq.cod-estabel
    field c-dt-trans-ini       like movto-estoq.dt-trans
    FIELD i-hr-trans-ini       AS INT
    field c-dt-trans-fim       like movto-estoq.dt-trans
    FIELD i-hr-trans-fim       AS INT
    field c-it-codigo-ini      like movto-estoq.it-codigo
    field c-it-codigo-fim      like movto-estoq.it-codigo
    field c-lote-ini           like movto-estoq.lote
    field c-lote-fim           like movto-estoq.lote
    field c-maq-ini            AS INT 
    field c-maq-fim            AS int
    field c-param-corte        AS int
    field c-param-status       AS int   
    field c-param-recorte      AS LOGICAL    
    .

DEFINE TEMP-TABLE tt-bobinas
    FIELD ttbob-lote           LIKE movto-estoq.lote
    FIELD ttbob-sequencia      AS   INTEGER
    FIELD ttbob-dt-trans       LIKE movto-estoq.dt-trans
    FIELD ttbob-nr-linha       LIKE ord-prod.nr-linha
    FIELD ttbob-it-codigo      LIKE movto-estoq.it-codigo 
    FIELD ttbob-quantidade     LIKE movto-estoq.quantidade 
    FIELD ttbob-defpri         AS   INTEGER  EXTENT 10
    FIELD ttbob-tabpri         AS   INTEGER  EXTENT 10
    FIELD ttbob-defsec         AS   INTEGER  EXTENT 10
    FIELD ttbob-tabsec         AS   INTEGER  EXTENT 10

    FIELD ttbob-lote-r1        LIKE movto-estoq.lote
    FIELD ttbob-dt-trans-r1    LIKE movto-estoq.dt-trans
    FIELD ttbob-nr-linha-r1    LIKE ord-prod.nr-linha
    FIELD ttbob-qt-peso-r1     LIKE movto-mat.quantidade
    FIELD ttbob-qt-largura  LIKE movto-mat.quantidade




    FIELD ttbob-lote-r2        LIKE movto-estoq.lote
    FIELD ttbob-dt-trans-r2    LIKE movto-estoq.dt-trans
    FIELD ttbob-nr-linha-r2    LIKE ord-prod.nr-linha
    FIELD ttbob-qt-peso-r2     LIKE movto-mat.quantidade
    FIELD ttbob-qt-largura-r2  LIKE movto-mat.quantidade


    FIELD ttbob-lote-r3        LIKE movto-estoq.lote
    FIELD ttbob-dt-trans-r3    LIKE movto-estoq.dt-trans
    FIELD ttbob-nr-linha-r3    LIKE ord-prod.nr-linha
    FIELD ttbob-qt-peso-r3     LIKE movto-mat.quantidade
    FIELD ttbob-qt-largura-r3  LIKE movto-mat.quantidade


    
    INDEX ch-tt-bobinas IS PRIMARY UNIQUE  ttbob-lote
                                           ttbob-sequencia. 

DEFINE TEMP-TABLE tt-bobinas2
    FIELD ttbob2-lote           LIKE movto-estoq.lote
    INDEX ch-tt-bobinas2 IS PRIMARY UNIQUE  ttbob2-lote. 

DEFINE TEMP-TABLE tt-defeitos-n
    FIELD ttdefn-def-secundario       AS CHAR FORMAT "x(15)"
    FIELD ttdefn-it-codigo            LIKE movto-estoq.it-codigo
    FIELD ttdefn-quantidade           AS DECIMAL
    INDEX ch-tt-defeitos-n IS PRIMARY UNIQUE  ttdefn-def-secundario
                                              ttdefn-it-codigo.

DEFINE TEMP-TABLE tt-defeitos-r
    FIELD ttdefr-def-primario         AS CHAR FORMAT "x(15)"
    FIELD ttdefr-it-codigo            LIKE movto-estoq.it-codigo
    FIELD ttdefr-quantidade           AS DECIMAL
    INDEX ch-tt-defeitos-r IS PRIMARY UNIQUE  ttdefr-def-primario
                                              ttdefr-it-codigo.  


DEFINE TEMP-TABLE mr-defeitos-n
    FIELD mrdefn-lote-mr              AS CHAR
    FIELD mrdefn-def-secundario       AS CHAR FORMAT "x(15)"
    FIELD mrdefn-it-codigo            LIKE movto-estoq.it-codigo
    FIELD mrdefn-quantidade           AS DECIMAL
    INDEX ch-mr-defeitos-n IS PRIMARY UNIQUE  mrdefn-lote-mr
                                              mrdefn-def-secundario
                                              mrdefn-it-codigo.

DEFINE TEMP-TABLE mr-defeitos-r
    FIELD mrdefr-lote-mr              AS CHAR
    FIELD mrdefr-def-primario         AS CHAR FORMAT "x(15)"
    FIELD mrdefr-it-codigo            LIKE movto-estoq.it-codigo
    FIELD mrdefr-quantidade           AS DECIMAL
    INDEX ch-mr-defeitos-r IS PRIMARY UNIQUE  mrdefr-lote-mr
                                              mrdefr-def-primario
                                              mrdefr-it-codigo.  


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
DEFINE VARIABLE sequencia-jr AS INTEGER    NO-UNDO.
/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 

def new shared var c-cod-estabel-ini    like movto-estoq.cod-estabel FORMAT "x(3)"       INITIAL "" NO-UNDO.
def new shared var c-dt-trans-ini       like movto-estoq.dt-trans    FORMAT "99/99/9999" INITIAL "01/01/2005" NO-UNDO. 
def new shared var c-dt-trans-fim       like movto-estoq.dt-trans    FORMAT "99/99/9999" INITIAL today NO-UNDO. 
def new shared var c-it-codigo-ini      like movto-estoq.it-codigo   FORMAT "x(15)"      INITIAL "" NO-UNDO. 
def new shared var c-it-codigo-fim      like movto-estoq.it-codigo   FORMAT "x(15)"      INITIAL "ZZZZZZZZZZZZZZZ" NO-UNDO. 
def new shared var c-lote-ini           like movto-estoq.lote        FORMAT "x(15)"      INITIAL "" NO-UNDO.
def new shared var c-lote-fim           like movto-estoq.lote        FORMAT "x(15)"      INITIAL "ZZZZZZZZZZZZZZZ" NO-UNDO.
def new shared var c-maq-ini            AS INT                       FORMAT ">>9"        INITIAL 0 NO-UNDO.
def new shared var c-maq-fim            AS int                       FORMAT ">>9"        INITIAL 999 NO-UNDO.
def new shared var c-param-corte        AS int                       FORMAT "9"          INITIAL 9   NO-UNDO.
def new shared var c-param-status       AS int                       FORMAT "9"          INITIAL 9   NO-UNDO.
def new shared var c-param-recorte      AS LOGICAL                   NO-UNDO.
def new shared VAR i-hr-trans-ini       AS INT                       NO-UNDO.
def new shared VAR i-hr-trans-fim       AS INT                       NO-UNDO.
/****************** Definiáao de Vari†veis p/ Campos Virtuais do Relat¢rio *******************/ 
DEFINE VARIABLE lote-mr       AS CHAR                      NO-UNDO.
DEFINE VARIABLE lote-mr1      AS CHAR                      NO-UNDO.
DEFINE VARIABLE lote-mr2      AS CHAR                      NO-UNDO.
DEFINE VARIABLE lote-mr3      AS CHAR                      NO-UNDO.
DEFINE VARIABLE lote-mr4      AS CHAR                      NO-UNDO.
DEFINE VARIABLE lote-mr5      AS CHAR                      NO-UNDO.
DEFINE VARIABLE ln-lote       LIKE ord-prod.nr-linha       NO-UNDO.
DEFINE VARIABLE ln-mr1        LIKE ord-prod.nr-linha       NO-UNDO.
DEFINE VARIABLE ln-mr2        LIKE ord-prod.nr-linha       NO-UNDO.
DEFINE VARIABLE ln-mr3        LIKE ord-prod.nr-linha       NO-UNDO.
DEFINE VARIABLE ln-mr4        LIKE ord-prod.nr-linha       NO-UNDO.
DEFINE VARIABLE ln-mr5        LIKE ord-prod.nr-linha       NO-UNDO.
DEFINE VARIABLE qt-peso-1     LIKE movto-mat.quantidade    NO-UNDO.
DEFINE VARIABLE qt-peso-2     LIKE movto-mat.quantidade    NO-UNDO.
DEFINE VARIABLE qt-peso-3     LIKE movto-mat.quantidade    NO-UNDO.
DEFINE VARIABLE qt-peso-4     LIKE movto-mat.quantidade    NO-UNDO.
DEFINE VARIABLE qt-peso-5     LIKE movto-mat.quantidade    NO-UNDO.

DEFINE VARIABLE qt-largura-1     LIKE movto-mat.quantidade    NO-UNDO.
DEFINE VARIABLE qt-largura-2     LIKE movto-mat.quantidade    NO-UNDO.
DEFINE VARIABLE qt-largura-3     LIKE movto-mat.quantidade    NO-UNDO.
DEFINE VARIABLE qt-largura-4     LIKE movto-mat.quantidade    NO-UNDO.
DEFINE VARIABLE qt-largura-5     LIKE movto-mat.quantidade    NO-UNDO.


DEFINE VARIABLE dt-mr1        LIKE movto-mat.dt-trans      NO-UNDO.
DEFINE VARIABLE dt-mr2        LIKE movto-mat.dt-trans      NO-UNDO.
DEFINE VARIABLE dt-mr3        LIKE movto-mat.dt-trans      NO-UNDO.
DEFINE VARIABLE dt-mr4        LIKE movto-mat.dt-trans      NO-UNDO.
DEFINE VARIABLE dt-mr5        LIKE movto-mat.dt-trans      NO-UNDO.
DEFINE VARIABLE bobina        AS CHAR                      NO-UNDO.
DEFINE VARIABLE item-mr       LIKE ped-item.it-codigo      NO-UNDO.
DEFINE VARIABLE lote-req      LIKE movto-estoq.lote        NO-UNDO.
DEFINE VARIABLE cmkt-req      LIKE movto-estoq.it-codigo   NO-UNDO.
DEFINE VARIABLE ordpro-req    LIKE movto-estoq.nr-ord-prod NO-UNDO.
DEFINE VARIABLE numseq-req    LIKE movto-estoq.num-sequen  NO-UNDO.
DEFINE VARIABLE flag-erro     AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE saldo-bobina  AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE tot-lin       AS INTEGER                   NO-UNDO.
DEFINE VARIABLE fim-mr        AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE DEF-primario  AS CHARACTER  FORMAT "x(15)" EXTENT 10 NO-UNDO.
DEFINE VARIABLE DEF-secundario AS CHARACTER FORMAT "x(15)" EXTENT 10 NO-UNDO.
DEFINE VARIABLE defpri-jr     AS INTEGER   EXTENT 10      NO-UNDO.
DEFINE VARIABLE tabpri-jr     AS INTEGER   EXTENT 10      NO-UNDO.
DEFINE VARIABLE defsec-jr     AS INTEGER   EXTENT 10      NO-UNDO.
DEFINE VARIABLE tabsec-jr     AS INTEGER   EXTENT 10      NO-UNDO.
DEFINE VARIABLE maq-jr        AS INTEGER                   NO-UNDO.
DEFINE VARIABLE tot-qtde      AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE tot-defn      AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE tot-defr      AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE mr-tot-defn   AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE mr-tot-defr   AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE perc-ac-def   AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE tot-qtde-def  AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE soma-def-n    AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE soma-def-r    AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE VAR-total     AS CHARACTER FORMAT "x(34)"  NO-UNDO.

DEFINE VARIABLE lote-jr       AS CHARACTER FORMAT "x(10)"  NO-UNDO.
DEFINE VARIABLE nome-op-jr    AS CHARACTER FORMAT "x(12)"  NO-UNDO.
DEFINE VARIABLE turma-jr1     AS CHAR                      NO-UNDO.
DEFINE VARIABLE maq-jr1       AS INTEGER                   NO-UNDO.
DEFINE VARIABLE it-codigo-jr1 AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE lote-jr1      AS CHARACTER                 NO-UNDO.

DEFINE VARIABLE imp-it-codigo    AS CHARACTER FORMAT "x(13)"    NO-UNDO.
DEFINE VARIABLE imp-lote         AS CHARACTER FORMAT "x(12)"    NO-UNDO.
DEFINE VARIABLE imp-lote-r1      AS CHARACTER FORMAT "x(12)"    NO-UNDO.
DEFINE VARIABLE imp-lote-r2      AS CHARACTER FORMAT "x(12)"    NO-UNDO.
DEFINE VARIABLE imp-lote-r3      AS CHARACTER FORMAT "x(12)"    NO-UNDO.
DEFINE VARIABLE imp-dt-trans     AS DATE  FORMAT "99/99/9999"   NO-UNDO.
DEFINE VARIABLE imp-qt-peso-r1   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE imp-qt-peso-r2   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE imp-qt-peso-r3   AS DECIMAL    NO-UNDO.

DEFINE VARIABLE imp-qt-largura   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE imp-qt-largura-r2   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE imp-qt-largura-r3   AS DECIMAL    NO-UNDO.



DEFINE VARIABLE imp-dt-trans-r1  AS DATE  FORMAT "99/99/9999"   NO-UNDO.
DEFINE VARIABLE imp-dt-trans-r2  AS DATE  FORMAT "99/99/9999"   NO-UNDO.
DEFINE VARIABLE imp-dt-trans-r3  AS DATE  FORMAT "99/99/9999"   NO-UNDO.

DEFINE VARIABLE imp-quantidade   AS DECIMAL FORMAT "->>>>>9.99" NO-UNDO.
DEFINE VARIABLE i-tt             AS INTEGER                     NO-UNDO.

DEFINE VARIABLE i-jr             AS INTEGER                     NO-UNDO.

DEFINE VARIABLE nr-linha-jr      AS INTEGER    NO-UNDO.

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




/****************** Definiáao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form imp-it-codigo        COLUMN-LABEL "Filme"           FORMAT "x(13)" at 001
     imp-lote-r1          COLUMN-LABEL "Mill Roll"       FORMAT "x(12)" AT 015
     imp-dt-trans-r1      COLUMN-LABEL "Dt. Prod."       FORMAT "99/99/9999" AT 028
     imp-lote-r2          COLUMN-LABEL "Bob.Prim†ria"    FORMAT "x(12)"      AT 039
     imp-dt-trans-r2      COLUMN-LABEL "Dt. Corte"       FORMAT "99/99/9999" AT 052

     imp-lote-r3          COLUMN-LABEL "Bob.Metaliz."    FORMAT "x(12)"      AT 063
     imp-dt-trans-r3      COLUMN-LABEL "Dt.Metalz"       FORMAT "99/99/9999" AT 076


     imp-lote             COLUMN-LABEL "Bob.Secund."     FORMAT "x(12)"      AT 087
     imp-dt-trans         COLUMN-LABEL "Dt.Recorte"      FORMAT "99/99/9999" AT 100
     imp-quantidade       COLUMN-LABEL "Peso Final"      FORMAT "->>>>>9.99" AT 111
     def-primario         COLUMN-LABEL "Df.de Rejeiá∆o"  FORMAT "x(15)" AT 122
     def-secundario       COLUMN-LABEL "Df.N∆o Rejeiá∆o" FORMAT "x(15)" AT 138
     with down width 160 no-box stream-io frame f-relat-09-132.

form "Total.....:" at 092
     tot-qtde             NO-LABEL                      FORMAT "->>>>>>>>>>9.99" AT 106
     with down width 160 no-box stream-io frame f-total-09-132.

form HEADER
    fill("-", 160) format "x(160)" SKIP 
    WITH DOWN WIDTH 200 NO-BOX STREAM-IO FRAME f-relat-linha.

form HEADER
    fill(" ", 160) format "x(160)" SKIP 
    WITH DOWN WIDTH 200 NO-BOX STREAM-IO FRAME f-relat-branco.

form "Resumo por Tipo de Defeito - N∆o Rejeiá∆o" AT 05 
     "-----------------------------------------" AT 05
     "      " AT 05
    WITH DOWN WIDTH 200 NO-BOX STREAM-IO FRAME f-relat-def-n.

form "Resumo por Tipo de Defeito - Rejeiá∆o" AT 05 
     "-------------------------------------" AT 05
     "      " AT 05
    WITH DOWN WIDTH 200 NO-BOX STREAM-IO FRAME f-relat-def-r.


form ttdefn-def-secundario   COLUMN-LABEL "Defeito"     FORMAT "x(15)" at 001
     ttdefn-it-codigo        COLUMN-LABEL "filme"       FORMAT "x(15)" AT 020
     ttdefn-quantidade       COLUMN-LABEL "Quantidade"  FORMAT "->>>>>9.99" AT 040
     perc-ac-def             COLUMN-LABEL " % "         FORMAT "->>9.99" AT 055
     with down width 132 no-box stream-io frame f-relat-defeitos-n.

form ttdefr-def-primario     COLUMN-LABEL "Defeito"     FORMAT "x(15)" at 001
     ttdefr-it-codigo        COLUMN-LABEL "filme"       FORMAT "x(15)" AT 020
     ttdefr-quantidade       COLUMN-LABEL "Quantidade"  FORMAT "->>>>>9.99" AT 040
     perc-ac-def             COLUMN-LABEL " % "         FORMAT "->>9.99" AT 055
     with down width 132 no-box stream-io frame f-relat-defeitos-r.

form var-total               NO-LABEL                   FORMAT "x(34)" AT 001
     tot-qtde-def            NO-LABEL                   FORMAT "->>>>>>>>>>9.99" AT 035
     perc-ac-def             NO-LABEL                   FORMAT "->>9.99" AT 055
     with down width 132 no-box stream-io frame f-total-def.  

form mrdefn-lote-mr          COLUMN-LABEL "M.Roll"      FORMAT "x(15)" at 001
     mrdefn-def-secundario   COLUMN-LABEL "Defeito"     FORMAT "x(15)" at 017
     mrdefn-quantidade       COLUMN-LABEL "Quantidade"  FORMAT "->>>>>9.99" AT 040
     perc-ac-def             COLUMN-LABEL " % "         FORMAT "->>9.99" AT 055
     with down width 132 no-box stream-io frame f-mr-defeitos-n.

form mrdefr-lote-mr          COLUMN-LABEL "M.Roll"      FORMAT "x(15)" at 001
     mrdefr-def-primario     COLUMN-LABEL "Defeito"     FORMAT "x(15)" at 017
     mrdefr-quantidade       COLUMN-LABEL "Quantidade"  FORMAT "->>>>>9.99" AT 040
     perc-ac-def             COLUMN-LABEL " % "         FORMAT "->>9.99" AT 055
     with down width 132 no-box stream-io frame f-mr-defeitos-r.


form "Resumo por Mill Roll - N∆o Rejeiá∆o" AT 05 
     "-----------------------------------------" AT 05
     "      " AT 05
    WITH DOWN WIDTH 200 NO-BOX STREAM-IO FRAME mr-relat-def-n.

form "Resumo por Mill Roll - Rejeiá∆o" AT 05 
     "-------------------------------------" AT 05
     "      " AT 05
    WITH DOWN WIDTH 200 NO-BOX STREAM-IO FRAME mr-relat-def-r.


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
define var c-titulo-relat  as character format "x(50)"      no-undo.
define var c-empresa       as character format "x(40)"      no-undo.
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

assign c-programa     = "escp0030rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Bobinas Cortadas no Per°odo"
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
    "Per°odo....:" AT 01 c-dt-trans-ini FORMAT "99/99/9999" AT 014 "  a   " AT 025
    c-dt-trans-fim FORMAT "99/99/9999" AT 032 SKIP 
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

    assign c-cod-estabel-ini = tt-param.c-cod-estabel-ini
           c-dt-trans-ini    = tt-param.c-dt-trans-ini   
           c-dt-trans-fim    = tt-param.c-dt-trans-fim   
           i-hr-trans-ini    = tt-param.i-hr-trans-ini
           i-hr-trans-fim    = tt-param.i-hr-trans-fim
           c-it-codigo-ini   = tt-param.c-it-codigo-ini  
           c-it-codigo-fim   = tt-param.c-it-codigo-fim  
           c-lote-ini        = tt-param.c-lote-ini       
           c-lote-fim        = tt-param.c-lote-fim       
           c-maq-ini         = tt-param.c-maq-ini        
           c-maq-fim         = tt-param.c-maq-fim        
           c-param-corte     = tt-param.c-param-corte    
           c-param-status    = tt-param.c-param-status   
           c-param-recorte   = tt-param.c-param-recorte   

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
    
    /* Cria Aplicaá∆o do Excel */

    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

    ASSIGN c-modelo-planilha = search("modelos/mod-escp0030.xls") 
           c-arq             = SESSION:TEMP-DIRECTORY.

    RUN pi-cria-planilha.


END.


assign v-num-reg-lidos = 0.

IF tt-param.destino = 4 THEN DO:

        ASSIGN i-linha = 3.

        ASSIGN c-relatorio:range("B" + STRING(i-linha)):VALUE = c-dt-trans-ini  
               c-relatorio:range("C" + STRING(i-linha)):VALUE = "a"    
               c-relatorio:range("D" + STRING(i-linha)):VALUE = c-dt-trans-fim.

END.

FOR EACH tt-bobinas NO-LOCK:
    DELETE tt-bobinas.
END.

ASSIGN i-linha = 5.


FOR EACH movto-estoq NO-LOCK USE-INDEX esp-data
    where movto-estoq.esp-docto = 1 
      AND movto-estoq.dt-trans  >= c-dt-trans-ini 
      AND movto-estoq.dt-trans  <= c-dt-trans-fim 
      AND movto-estoq.it-codigo >= c-it-codigo-ini 
      AND movto-estoq.it-codigo <= c-it-codigo-fim 
      AND movto-estoq.cod-estabel = c-cod-estabel-ini :
    /*AND movto-estoq.cod-depos <> "ARC" */

    IF movto-estoq.lote = "recicl" THEN next.  

    FIND FIRST ITEM WHERE ITEM.it-codigo = movto-estoq.it-codigo
         USE-INDEX codigo NO-LOCK NO-ERROR.
    
    IF NOT AVAIL ITEM OR item.ge-codigo < 40 OR item.ge-codigo > 49 THEN NEXT.

    if item.ge-codigo <> 47 and movto-estoq.cod-depos = "ARC" then next.

     assign v-num-reg-lidos = v-num-reg-lidos + 1.
     run pi-acompanhar in h-acomp(input string(item.it-codigo)).    
 
     FIND FIRST movto-mat NO-LOCK USE-INDEX reporte
         WHERE movto-mat.nr-reporte = movto-estoq.nr-reporte 
           AND movto-mat.esp-docto = 8                       NO-ERROR.

    IF AVAIL movto-mat THEN NEXT.
    
    FIND FIRST rep-prod NO-LOCK use-index codigo 
        WHERE rep-prod.nr-reporte = movto-estoq.nr-reporte NO-ERROR.

    IF AVAIL rep-prod AND rep-prod.qt-estorno <> 0 THEN NEXT.

    FIND FIRST movto-mat USE-INDEX num-seq
        WHERE movto-mat.nr-ord-produ = movto-estoq.nr-ord-produ
          AND movto-mat.num-sequen   = movto-estoq.num-sequen    NO-ERROR.

    FIND FIRST rep-oper-ctrab NO-LOCK 
        WHERE rep-oper-ctrab.nr-ord-prod = movto-mat.nr-ord-prod 
          AND rep-oper-ctrab.nr-reporte  = movto-mat.nr-reporte NO-ERROR.

    IF AVAIL rep-oper-ctrab THEN DO:
        IF  rep-oper-ctrab.dat-inic-reporte < c-dt-trans-ini OR 
           (rep-oper-ctrab.dat-inic-reporte = c-dt-trans-ini AND rep-oper-ctrab.qtd-segs-inic < i-hr-trans-ini) THEN NEXT.

        IF  rep-oper-ctrab.dat-inic-reporte > c-dt-trans-fim OR 
           (rep-oper-ctrab.dat-inic-reporte = c-dt-trans-fim AND rep-oper-ctrab.qtd-segs-inic > i-hr-trans-fim) THEN NEXT.
    END.

    FIND FIRST ord-prod NO-LOCK 
        WHERE ord-prod.nr-ord-produ = movto-estoq.nr-ord-produ NO-ERROR.

    IF NOT AVAIL ord-prod THEN NEXT.

    IF c-param-corte = 1 AND (ord-prod.nr-linha < 200 OR ord-prod.nr-linha > 299)
         THEN NEXT.

    IF c-param-corte = 2 AND (ord-prod.nr-linha < 400 OR ord-prod.nr-linha > 499)
         THEN NEXT.

    IF c-param-corte = 3 AND (ord-prod.nr-linha < 200 OR ord-prod.nr-linha > 499)
         THEN NEXT.

    IF c-param-corte = 3 AND (ord-prod.nr-linha > 299 AND ord-prod.nr-linha < 400)
         THEN NEXT.
 
    ASSIGN defpri-jr = 0
           defsec-jr = 0
           tabpri-jr = 0
           tabsec-jr = 0
           ln-lote   = ord-prod.nr-linha.   

    FIND FIRST lote-carac-tec NO-LOCK use-index codigo 
        WHERE lote-carac-tec.it-codigo = movto-estoq.it-codigo
          and lote-carac-tec.lote = movto-estoq.lote
          and lote-carac-tec.cd-comp = "defpri"     NO-ERROR.
    
    if avail lote-carac-tec then do:
        ASSIGN i-jr = 0.
        
        FOR EACH lote-res-carac NO-LOCK
             where lote-res-carac.cd-folha   = lote-carac-tec.cd-folha 
               AND lote-res-carac.it-codigo  = lote-carac-tec.it-codigo 
               AND lote-res-carac.lote       = lote-carac-tec.lote 
               AND lote-res-carac.nr-tabela  = lote-carac-tec.nr-tabela 
               AND lote-res-carac.sequencia <> 0 
               AND lote-res-carac.cd-comp    = lote-carac-tec.cd-comp:

            ASSIGN i-jr = i-jr + 1.
            
            assign defpri-jr [i-jr] = lote-res-carac.sequencia
                   tabpri-jr [i-jr] = lote-res-carac.nr-tabela.
        END.
    END.

       /*IF defpri-jr <> 103 THEN next.*/

    FIND FIRST lote-carac-tec NO-LOCK use-index codigo 
        WHERE lote-carac-tec.it-codigo = movto-estoq.it-codigo
          and lote-carac-tec.lote = movto-estoq.lote
          and lote-carac-tec.cd-comp = "defsec" NO-ERROR.

    if avail lote-carac-tec then do:
        
        ASSIGN i-jr = 0. 

        FOR EACH lote-res-carac NO-LOCK 
            where lote-res-carac.cd-folha = lote-carac-tec.cd-folha 
              AND lote-res-carac.it-codigo = lote-carac-tec.it-codigo 
              AND lote-res-carac.lote = lote-carac-tec.lote 
              AND lote-res-carac.nr-tabela = lote-carac-tec.nr-tabela 
              AND lote-res-carac.sequencia <> 0 
              AND lote-res-carac.cd-comp = lote-carac-tec.cd-comp.
            
            ASSIGN i-jr = i-jr + 1.

            assign defsec-jr [i-jr] = lote-res-carac.sequencia
                   tabsec-jr [i-jr] = lote-res-carac.nr-tabela.
        END.
    END.

    IF c-param-status = 1 AND defpri-jr [1] = 0 THEN NEXT.

    ASSIGN maq-jr = 1.

    IF movto-estoq.cod-estabel = "422" THEN DO: 
        FIND FIRST lote-carac-tec NO-LOCK use-index codigo 
            WHERE lote-carac-tec.it-codigo = movto-estoq.it-codigo
              AND lote-carac-tec.lote = movto-estoq.lote
              and lote-carac-tec.cd-comp = "maq"         NO-ERROR.

       if avail lote-carac-tec then
          ASSIGN maq-jr = lote-carac-tec.vl-resul. 
    END.

    IF movto-estoq.cod-estabel <> "422" THEN DO: 
       FIND FIRST lote-carac-tec NO-LOCK use-index codigo 
           WHERE lote-carac-tec.it-codigo = movto-estoq.it-codigo
             and lote-carac-tec.lote = movto-estoq.lote
             and lote-carac-tec.cd-comp = "slitter"   NO-ERROR.

       if avail lote-carac-tec then
          ASSIGN maq-jr = lote-carac-tec.vl-resul. 
    END.

    IF maq-jr < c-maq-ini OR maq-jr > c-maq-fim THEN NEXT. 

    ASSIGN fim-mr   = "X".
    ASSIGN lote-mr  = "X".
    ASSIGN lote-mr1 = "X".
    ASSIGN lote-mr2 = "X".
    ASSIGN lote-mr3 = "X".
    ASSIGN lote-mr4 = "X".
    ASSIGN lote-mr5 = "X".
    ASSIGN saldo-bobina = movto-estoq.quantidade.
    
    FIND FIRST movto-mat USE-INDEX num-seq 
        WHERE movto-mat.nr-ord-prod = movto-estoq.nr-ord-prod 
          AND movto-mat.num-sequen = (movto-estoq.num-sequen - 1) NO-LOCK NO-ERROR.
               
    IF AVAIL movto-mat AND movto-mat.lote = "recicl" THEN DO:

        FIND FIRST movto-mat NO-LOCK USE-INDEX num-seq 
            WHERE movto-mat.nr-ord-prod = movto-estoq.nr-ord-prod 
              AND movto-mat.num-sequen = (movto-estoq.num-sequen - 2) NO-ERROR.
    END.

    IF AVAIL movto-mat AND movto-mat.esp-docto = 28 AND movto-mat.lote <> "recicl" THEN do: 

       ASSIGN lote-req = movto-mat.lote.
       ASSIGN cmkt-req = movto-mat.it-codigo.  

       FIND LAST movto-mat NO-LOCK USE-INDEX lote 
           WHERE movto-mat.it-codigo = cmkt-req 
             AND movto-mat.lote = lote-req 
             AND movto-mat.esp-docto = 1        NO-ERROR.

       FIND FIRST lote-prod NO-LOCK 
          WHERE lote-prod.it-codigo = cmkt-req 
            AND lote-prod.lote      = lote-req NO-ERROR.

       IF NOT AVAIL lote-prod THEN NEXT.
      
       ASSIGN lote-mr = "X".
       ASSIGN flag-erro = "X".
    
       DO WHILE lote-mr = "X" AND flag-erro = "X" :
       
         IF AVAIL movto-mat THEN DO:

            FIND FIRST ord-prod NO-LOCK USE-INDEX estabel 
                WHERE ord-prod.nr-ord-produ = movto-mat.nr-ord-produ 
                  AND ord-prod.it-codigo = movto-mat.it-codigo 
                  AND ord-prod.cod-estabel = movto-mat.cod-estabel NO-ERROR.

            IF NOT AVAIL ord-prod THEN 
               ASSIGN flag-erro = "erro". 
         END.
         ELSE DO:
             IF AVAIL lote-prod THEN DO:
                 FIND FIRST ord-prod NO-LOCK USE-INDEX estabel 
                     WHERE ord-prod.nr-ord-produ = lote-prod.nr-ord-produ NO-ERROR.
             END.
         END.

         IF AVAIL ord-prod THEN
             nr-linha-jr = ord-prod.nr-linha.
         ELSE
             nr-linha-jr  = 1.

         IF nr-linha-jr < 100 AND flag-erro = "X" THEN DO:

            ASSIGN lote-mr = lote-prod.lote.
            assign item-mr = lote-prod.it-codigo.
            ASSIGN lote-mr5 = lote-prod.lote
                   dt-mr5   = IF AVAIL movto-mat THEN movto-mat.dt-trans ELSE IF AVAIL lote-prod THEN lote-prod.dt-prim-uso ELSE ?
                   qt-peso-5 = IF AVAIL movto-mat THEN movto-mat.quantidade ELSE IF AVAIL lote-prod THEN lote-prod.qt-produzida ELSE 0
                   ln-mr5   = nr-linha-jr. 


             FIND FIRST lote-carac-tec WHERE
             lote-carac-tec.it-codigo   = lote-prod.it-codigo
             and lote-carac-tec.lote    = lote-prod.lote
             and lote-carac-tec.cd-comp = "largura"
             NO-LOCK NO-ERROR.

             if avail lote-carac-tec then 
                ASSIGN qt-largura-5 = lote-carac-tec.vl-result.


         END.

         IF lote-mr = "X" AND flag-erro = "X" THEN DO:

             IF lote-mr1 = "X" THEN DO:
            
                ASSIGN lote-mr1 = IF AVAIL movto-mat THEN movto-mat.lote ELSE IF AVAIL lote-prod THEN lote-prod.lote ELSE ""
                       dt-mr1   = IF AVAIL movto-mat THEN movto-mat.dt-trans ELSE IF AVAIL lote-prod THEN lote-prod.dt-prim-uso ELSE ?
                       qt-peso-1= IF AVAIL movto-mat THEN movto-mat.quantidade ELSE IF AVAIL lote-prod THEN lote-prod.qt-produzida ELSE 0
                       ln-mr1   = nr-linha-jr.

                    FIND FIRST lote-carac-tec WHERE
                    lote-carac-tec.it-codigo   = lote-prod.it-codigo  
                    and lote-carac-tec.lote    = lote-mr1
                    and lote-carac-tec.cd-comp = "largura"
                    NO-LOCK NO-ERROR.
        
                    if avail lote-carac-tec then 
                       ASSIGN qt-largura-1 = lote-carac-tec.vl-result.



             END.
                         
             IF lote-mr1 <> "X" AND lote-mr1 <> lote-prod.lote THEN  DO:

             
                ASSIGN lote-mr2 = IF AVAIL movto-mat THEN movto-mat.lote ELSE IF AVAIL lote-prod THEN lote-prod.lote ELSE ""
                       dt-mr2   = IF AVAIL movto-mat THEN movto-mat.dt-trans ELSE IF AVAIL lote-prod THEN lote-prod.dt-prim-uso ELSE ?
                       qt-peso-2= IF AVAIL movto-mat THEN movto-mat.quantidade ELSE IF AVAIL lote-prod THEN lote-prod.qt-produzida ELSE 0
                       ln-mr2   = IF AVAIL ord-prod THEN ord-prod.nr-linha ELSE nr-linha-jr. 


                    FIND FIRST lote-carac-tec WHERE
                    lote-carac-tec.it-codigo   = lote-prod.it-codigo
                    and lote-carac-tec.lote    = lote-prod.lote
                    and lote-carac-tec.cd-comp = "largura"
                    NO-LOCK NO-ERROR.
        
                    if avail lote-carac-tec then 
                       ASSIGN qt-largura-2 = lote-carac-tec.vl-result.

             END.

             IF lote-mr2 <> "X" AND lote-mr2 <> lote-prod.lote THEN DO:
             
                ASSIGN lote-mr3 = IF AVAIL movto-mat THEN movto-mat.lote ELSE IF AVAIL lote-prod THEN lote-prod.lote ELSE ""
                       dt-mr3   = IF AVAIL movto-mat THEN movto-mat.dt-trans ELSE IF AVAIL lote-prod THEN lote-prod.dt-prim-uso ELSE ?
                       qt-peso-3= IF AVAIL movto-mat THEN movto-mat.quantidade ELSE IF AVAIL lote-prod THEN lote-prod.qt-produzida ELSE 0
                       ln-mr3   = IF AVAIL ord-prod THEN ord-prod.nr-linha ELSE nr-linha-jr.    

                FIND FIRST lote-carac-tec WHERE
                lote-carac-tec.it-codigo   = lote-prod.it-codigo
                and lote-carac-tec.lote    = lote-prod.lote
                and lote-carac-tec.cd-comp = "largura"
                NO-LOCK NO-ERROR.

                if avail lote-carac-tec then 
                   ASSIGN qt-largura-3 = lote-carac-tec.vl-result.


             END.
                         
             IF lote-mr3 <> "X" AND lote-mr3 <> lote-prod.lote THEN DO:
             
                ASSIGN lote-mr4 = IF AVAIL movto-mat THEN movto-mat.lote ELSE IF AVAIL lote-prod THEN lote-prod.lote ELSE ""
                       dt-mr4   = IF AVAIL movto-mat THEN movto-mat.dt-trans ELSE IF AVAIL lote-prod THEN lote-prod.dt-prim-uso ELSE ?
                       qt-peso-4= IF AVAIL movto-mat THEN movto-mat.quantidade ELSE IF AVAIL lote-prod THEN lote-prod.qt-produzida ELSE 0
                       ln-mr4   = IF AVAIL ord-prod THEN ord-prod.nr-linha ELSE nr-linha-jr. 

                  FIND FIRST lote-carac-tec WHERE
                  lote-carac-tec.it-codigo   = lote-prod.it-codigo
                  and lote-carac-tec.lote    = lote-prod.lote
                  and lote-carac-tec.cd-comp = "largura"
                  NO-LOCK NO-ERROR.

                  if avail lote-carac-tec then 
                     ASSIGN qt-largura-4 = lote-carac-tec.vl-result.

             END.
                         
             IF lote-mr4 <> "X" AND lote-mr4 <> lote-prod.lote THEN DO:
             
                ASSIGN lote-mr5 = IF AVAIL movto-mat THEN movto-mat.lote ELSE IF AVAIL lote-prod THEN lote-prod.lote ELSE ""
                       dt-mr5   = IF AVAIL movto-mat THEN movto-mat.dt-trans ELSE IF AVAIL lote-prod THEN lote-prod.dt-prim-uso ELSE ?
                       qt-peso-5= IF AVAIL movto-mat THEN movto-mat.quantidade ELSE IF AVAIL lote-prod THEN lote-prod.qt-produzida ELSE 0
                       ln-mr5   = IF AVAIL ord-prod THEN ord-prod.nr-linha ELSE nr-linha-jr.  

                  FIND FIRST lote-carac-tec WHERE
                  lote-carac-tec.it-codigo   = lote-prod.it-codigo
                  and lote-carac-tec.lote    = lote-prod.lote
                  and lote-carac-tec.cd-comp = "largura"
                  NO-LOCK NO-ERROR.

                  if avail lote-carac-tec then 
                     ASSIGN qt-largura-5 = lote-carac-tec.vl-result.
                                     
             END.


             ASSIGN ordpro-req = IF AVAIL movto-mat THEN movto-mat.nr-ord-prod ELSE IF AVAIL ord-prod THEN ord-prod.nr-ord-produ ELSE 0.
             ASSIGN numseq-req = IF AVAIL movto-mat THEN (movto-mat.num-sequen - 1) ELSE 0.


             FIND FIRST movto-mat WHERE
                movto-mat.nr-ord-prod = ordpro-req and
                movto-mat.num-sequen = numseq-req AND
                movto-mat.esp-docto = 28
                USE-INDEX num-seq NO-LOCK NO-ERROR.

             IF NOT AVAIL movto-mat THEN
                ASSIGN flag-erro = "erro".
              
             IF flag-erro = "X" THEN DO:
            
                ASSIGN lote-req = IF AVAIL movto-mat THEN movto-mat.lote ELSE IF AVAIL lote-prod THEN lote-prod.lote ELSE "".
                ASSIGN cmkt-req = IF AVAIL movto-mat THEN movto-mat.it-codigo ELSE IF AVAIL lote-prod THEN lote-prod.it-codigo ELSE "".

                FIND FIRST movto-mat WHERE
                    movto-mat.it-codigo = cmkt-req and
                    movto-mat.lote = lote-req AND
                    movto-mat.esp-docto = 1
                    USE-INDEX lote NO-LOCK NO-ERROR.

                FIND FIRST lote-prod WHERE
                    lote-prod.it-codigo = cmkt-req AND
                    lote-prod.lote      = lote-req 
                    NO-LOCK NO-ERROR.

                 IF NOT AVAIL lote-prod THEN 
                   ASSIGN flag-erro = "erro".
                 
             END.
         END.
      END. 
    END.
     
     IF lote-mr1 = "X" THEN  ASSIGN lote-mr1 = "".
                                    
     IF lote-mr2 = "X" THEN  ASSIGN lote-mr2 = "".
                                    
     IF lote-mr3 = "X" THEN  ASSIGN lote-mr3 = "".
                                    
     IF lote-mr4 = "X" THEN  ASSIGN lote-mr4 = "".
                                    
     IF lote-mr5 = "X" THEN  ASSIGN lote-mr5 = "".

     IF lote-mr1 = ""  THEN  ASSIGN lote-mr1 = lote-mr5
                                    ln-mr1   = ln-mr5
                                    dt-mr1   = dt-mr5
                                    qt-peso-1= qt-peso-5
                                    qt-largura-1 = qt-largura-5
                                    lote-mr5 = "".
                                                  
     IF lote-mr2 = ""  THEN  ASSIGN lote-mr2 = lote-mr5
                                    dt-mr2   = dt-mr5
                                    qt-peso-2= qt-peso-5
                                    qt-largura-2 = qt-largura-5
                                    ln-mr2   = ln-mr5
                                    lote-mr5 = "".

     IF lote-mr3 = ""  THEN  ASSIGN lote-mr3 = lote-mr5
                                    dt-mr3   = dt-mr5 
                                    qt-peso-3= qt-peso-5
                                    qt-largura-3 = qt-largura-5
                                    ln-mr3   = ln-mr5
                                    lote-mr5 = "".

     IF lote-mr4 = ""  THEN  ASSIGN lote-mr4 = lote-mr5
                                    dt-mr4   = dt-mr5
                                    qt-peso-4= qt-peso-5
                                    qt-largura-4 = qt-largura-5
                                    ln-mr4   = ln-mr5
                                    lote-mr5 = "".

     ASSIGN sequencia-jr = sequencia-jr + 1.

     FIND tt-bobinas WHERE
               ttbob-lote = movto-estoq.lote AND
               ttbob-sequencia = sequencia-jr
               NO-ERROR.
        
        IF NOT AVAIL tt-bobinas THEN DO:

            CREATE tt-bobinas.
            ASSIGN ttbob-lote = movto-estoq.lote
                   ttbob-sequencia = sequencia-jr.

        END.
     
        
     ASSIGN ttbob-it-codigo  = movto-estoq.it-codigo
            ttbob-dt-trans   = IF AVAIL movto-estoq THEN movto-estoq.dt-trans ELSE ?
            ttbob-nr-linha   = ln-lote
            ttbob-quantidade = ttbob-quantidade + IF AVAIL movto-estoq THEN movto-estoq.quantidade ELSE 0.



      FIND FIRST lote-carac-tec WHERE
                  lote-carac-tec.it-codigo   = lote-prod.it-codigo
                  and lote-carac-tec.lote    = lote-prod.lote
                  and lote-carac-tec.cd-comp = "largura"
                  NO-LOCK NO-ERROR.

                  if avail lote-carac-tec then 
                     ASSIGN  ttbob-qt-largura = lote-carac-tec.vl-result.


     ASSIGN i-jr = 0.

     DO WHILE i-jr < 10.
         ASSIGN i-jr = i-jr + 1
            ttbob-defpri [i-jr]  = defpri-jr [i-jr]
            ttbob-defsec [i-jr]  = defsec-jr [i-jr]
            ttbob-tabpri [i-jr]  = tabpri-jr [i-jr]
            ttbob-tabsec [i-jr]  = tabsec-jr [i-jr].
     END.

     IF ttbob-defpri [1] <> 0 THEN
         ASSIGN ttbob-defsec = 0
                ttbob-tabsec = 0.
                     
     IF lote-mr1 <> "" AND ln-mr1 > 0 AND ln-mr1 < 200 THEN
        ASSIGN ttbob-lote-r1     = lote-mr1
               ttbob-dt-trans-r1 = dt-mr1
               ttbob-qt-peso-r1  = qt-peso-1
               ttbob-nr-linha-r1 = ln-mr1.

     IF lote-mr2 <> "" AND ln-mr2 > 0 AND ln-mr2 < 200 THEN
        ASSIGN ttbob-lote-r1     = lote-mr2
               ttbob-dt-trans-r1 = dt-mr2
               ttbob-qt-peso-r1  = qt-peso-2
               ttbob-nr-linha-r1 = ln-mr2.

     IF lote-mr3 <> "" AND ln-mr3 > 0 AND ln-mr3 < 200 THEN
        ASSIGN ttbob-lote-r1     = lote-mr3
               ttbob-dt-trans-r1 = dt-mr3
               ttbob-qt-peso-r1  = qt-peso-3
               ttbob-nr-linha-r1 = ln-mr3.

     IF lote-mr4 <> "" AND ln-mr4 > 0 AND ln-mr4 < 200 THEN
        ASSIGN ttbob-lote-r1     = lote-mr4
               ttbob-dt-trans-r1 = dt-mr4
               ttbob-qt-peso-r1  = qt-peso-4
               ttbob-nr-linha-r1 = ln-mr4.

     IF lote-mr5 <> "" AND ln-mr5 > 0 AND ln-mr5 < 200 THEN
        ASSIGN ttbob-lote-r1     = lote-mr5
               ttbob-dt-trans-r1 = dt-mr5
               ttbob-qt-peso-r1  = qt-peso-5
               ttbob-nr-linha-r1 = ln-mr5.

     IF lote-mr1 <> "" AND ln-mr1 > 199 AND ln-mr1 < 300 THEN
        ASSIGN ttbob-lote-r2     = lote-mr1
               ttbob-dt-trans-r2 = dt-mr1
               ttbob-qt-peso-r2  = qt-peso-1
               ttbob-qt-largura-r2  = qt-largura-1
               ttbob-nr-linha-r2 = ln-mr1.

     IF lote-mr2 <> "" AND ln-mr2 > 199 AND ln-mr2 < 300 THEN
        ASSIGN ttbob-lote-r2     = lote-mr2
               ttbob-dt-trans-r2 = dt-mr2
               ttbob-qt-peso-r2  = qt-peso-2
               ttbob-qt-largura-r2  = qt-largura-2
               ttbob-nr-linha-r2 = ln-mr2.

     IF lote-mr3 <> "" AND ln-mr3 > 199 AND ln-mr3 < 300 THEN
        ASSIGN ttbob-lote-r2     = lote-mr3
               ttbob-dt-trans-r2 = dt-mr3
               ttbob-qt-peso-r2  = qt-peso-3
               ttbob-qt-largura-r2  = qt-largura-3
               ttbob-nr-linha-r2 = ln-mr3.

     IF lote-mr4 <> "" AND ln-mr4 > 199 AND ln-mr4 < 300 THEN
        ASSIGN ttbob-lote-r2     = lote-mr4
               ttbob-dt-trans-r2 = dt-mr4
               ttbob-qt-peso-r2  = qt-peso-4
               ttbob-qt-largura-r2  = qt-largura-4
               ttbob-nr-linha-r2 = ln-mr4.

     IF lote-mr5 <> "" AND ln-mr5 > 199 AND ln-mr5 < 300 THEN
        ASSIGN ttbob-lote-r2     = lote-mr5
               ttbob-dt-trans-r2 = dt-mr5
               ttbob-qt-peso-r2  = qt-peso-5
               ttbob-qt-largura-r2  = qt-largura-5
               ttbob-nr-linha-r2 = ln-mr5.


     /* Bob Metalizada */

     IF lote-mr1 <> "" AND ln-mr1 > 299 AND ln-mr1 < 400 THEN
        ASSIGN ttbob-lote-r3     = lote-mr1
               ttbob-dt-trans-r3 = dt-mr1
               ttbob-qt-peso-r3  = qt-peso-1
               ttbob-qt-largura-r3  = qt-largura-1
               ttbob-nr-linha-r3 = ln-mr1.

     IF lote-mr2 <> "" AND ln-mr2 > 299 AND ln-mr2 < 400 THEN
        ASSIGN ttbob-lote-r3     = lote-mr2
               ttbob-dt-trans-r3 = dt-mr2
               ttbob-qt-peso-r3  = qt-peso-2
               ttbob-qt-largura-r3  = qt-largura-2
               ttbob-nr-linha-r3 = ln-mr2.

     IF lote-mr3 <> "" AND ln-mr3 > 299 AND ln-mr3 < 400 THEN
        ASSIGN ttbob-lote-r3     = lote-mr3
               ttbob-dt-trans-r3 = dt-mr3
               ttbob-qt-peso-r3  = qt-peso-3
               ttbob-qt-largura-r3  = qt-largura-3
               ttbob-nr-linha-r3 = ln-mr3.

     IF lote-mr4 <> "" AND ln-mr4 > 299 AND ln-mr4 < 400 THEN
        ASSIGN ttbob-lote-r3     = lote-mr4
               ttbob-dt-trans-r3 = dt-mr4
               ttbob-qt-peso-r3  = qt-peso-4
               ttbob-qt-largura-r3  = qt-largura-4
               ttbob-nr-linha-r3 = ln-mr4.

     IF lote-mr5 <> "" AND ln-mr5 > 299 AND ln-mr5 < 400 THEN
        ASSIGN ttbob-lote-r3     = lote-mr5
               ttbob-dt-trans-r3 = dt-mr5
               ttbob-qt-peso-r3  = qt-peso-5
               ttbob-qt-largura-r3  = qt-largura-5
               ttbob-nr-linha-r3 = ln-mr5.

     
     /* Fim da Bob Metalizada */
     
     
     IF ttbob-lote-r1 = "" THEN
        ASSIGN ttbob-dt-trans-r1 = ?
               ttbob-nr-linha-r1 = 0
               ttbob-qt-peso-r1 = 0
               ttbob-lote-r1     = " ".

     IF ttbob-lote-r2 = "" THEN
        ASSIGN ttbob-dt-trans-r2 = ?
               ttbob-nr-linha-r2 = 0
               ttbob-qt-peso-r2 = 0
               ttbob-qt-largura-r2= 0
               ttbob-lote-r2     = " ".

     IF ttbob-lote-r3 = "" THEN
        ASSIGN ttbob-dt-trans-r3 = ?
               ttbob-nr-linha-r3 = 0
               ttbob-qt-peso-r3 = 0
               ttbob-qt-largura-r3= 0
               ttbob-lote-r3     = " ".

     
END.
 
 assign v-num-reg-lidos = 0.

 
FOR EACH tt-bobinas NO-LOCK 
    WHERE ttbob-nr-linha >= 400 
      AND ttbob-nr-linha <= 500 
      AND c-param-recorte = NO.
                                
    FIND tt-bobinas2 
        WHERE ttbob2-lote = ttbob-lote-r2 NO-ERROR.

    IF NOT AVAIL tt-bobinas2 THEN DO:
        CREATE tt-bobinas2.
        ASSIGN ttbob2-lote = ttbob-lote-r2.
    END.

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
END.
 
assign v-num-reg-lidos = 0 .

/* Imprime o relat¢rio */

     ASSIGN tot-qtde    = 0
            tot-defn    = 0
            tot-defr    = 0
            mr-tot-defn = 0
            mr-tot-defr = 0.
     
     FOR EACH tt-bobinas NO-LOCK
         BREAK BY ttbob-nr-linha
               BY ttbob-it-codigo.

       assign v-num-reg-lidos = v-num-reg-lidos + 1.
       run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
        
       IF ttbob-lote-r1 < c-lote-ini OR ttbob-lote-r1 > c-lote-fim THEN NEXT.

       IF ttbob-nr-linha >= 200 AND 
          ttbob-nr-linha <= 300 and
          c-param-recorte = NO THEN DO:
          
          FIND tt-bobinas2 WHERE
               ttbob2-lote = ttbob-lote
               NO-ERROR.

          IF AVAIL tt-bobinas2 THEN next. 

       END. 
 
       ASSIGN DEF-primario   = ""
              DEF-secundario = ""
              i-jr           = 0.

       DO WHILE i-jr < 10.

           ASSIGN i-jr = i-jr + 1.

             IF ttbob-defpri [i-jr] > 0 THEN DO:
            
                FIND FIRST c-tab-res
                        where c-tab-res.nr-tabela = ttbob-tabpri [i-jr] AND
                              c-tab-res.sequencia = ttbob-defpri [i-jr]
                              NO-LOCK NO-ERROR.
                
                IF AVAIL c-tab-res THEN
                   ASSIGN DEF-primario [i-jr] = c-tab-res.descricao.
                   ELSE
                       ASSIGN DEF-primario [i-jr] = "" .
            
            END.
            
            IF ttbob-defsec [i-jr] > 0 THEN DO:
            
               FIND FIRST c-tab-res
                       where c-tab-res.nr-tabela = ttbob-tabsec [i-jr] AND
                             c-tab-res.sequencia = ttbob-defsec [i-jr]
                             NO-LOCK NO-ERROR.
               
               IF AVAIL c-tab-res THEN
                  ASSIGN DEF-secundario [i-jr] = c-tab-res.descricao.
                  ELSE
                      ASSIGN DEF-secundario [i-jr] = "".
            
            END.

       END.

       ASSIGN imp-it-codigo   = ttbob-it-codigo
              imp-lote-r1     = ttbob-lote-r1
              imp-dt-trans-r1 = ttbob-dt-trans-r1
              imp-qt-peso-r1 =  ttbob-qt-peso-r1
              imp-lote-r2     = ttbob-lote-r2
              imp-dt-trans-r2 = ttbob-dt-trans-r2
              imp-qt-peso-r2 =  ttbob-qt-peso-r2
              imp-qt-largura-r2 = ttbob-qt-largura-r2
              imp-lote-r3     = ttbob-lote-r3
              imp-dt-trans-r3 = ttbob-dt-trans-r3
              imp-qt-peso-r3 =  ttbob-qt-peso-r3
              imp-qt-largura-r3 = ttbob-qt-largura-r3
              imp-quantidade  = ttbob-quantidade.

       IF ttbob-nr-linha > 399 THEN
          ASSIGN imp-lote     = ttbob-lote
                 imp-dt-trans = ttbob-dt-trans
                 imp-qt-largura  = ttbob-qt-largura.
            ELSE
              ASSIGN imp-lote        = ""
                     imp-dt-trans    = ?
                     imp-lote-r2     = ttbob-lote
                     imp-dt-trans-r2 = ttbob-dt-trans
                     imp-qt-largura  = 0.  


     /***  C‡DIGO PARA SA÷DA EM 132 COLUNAS ***/
     IF tt-param.destino <> 4 THEN DO:

        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp
            imp-it-codigo      
            imp-lote-r1        
            imp-dt-trans-r1
            imp-qt-peso-r1
            imp-qt-largura
            imp-lote-r2        
            imp-dt-trans-r2 
            imp-qt-peso-r2
            imp-qt-largura-r2
            imp-lote-r3        
            imp-dt-trans-r3
            imp-qt-peso-r3
            imp-qt-largura-r3
            imp-lote           
            imp-dt-trans       
            imp-quantidade     
            def-primario [1]         
            def-secundario [1]  
            with stream-io frame f-relat-09-132.
            down stream str-rp with frame f-relat-09-132.  
     END.

     IF tt-param.destino = 4 THEN DO:

        ASSIGN i-linha = i-linha + 1.

        ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = imp-it-codigo  
               c-relatorio:range("B" + STRING(i-linha)):VALUE = imp-lote-r1       
               c-relatorio:range("C" + STRING(i-linha)):VALUE = imp-qt-peso-r1
               c-relatorio:range("D" + STRING(i-linha)):VALUE = imp-dt-trans-r1

               c-relatorio:range("H" + STRING(i-linha)):VALUE = imp-lote-r2    
               
               c-relatorio:range("i" + STRING(i-linha)):VALUE = imp-qt-largura-r2    
               
               c-relatorio:range("J" + STRING(i-linha)):VALUE = imp-qt-peso-r2    
               c-relatorio:range("K" + STRING(i-linha)):VALUE = imp-dt-trans-r2

               c-relatorio:range("O" + STRING(i-linha)):VALUE = imp-lote-r3   
               
               c-relatorio:range("P" + STRING(i-linha)):VALUE = imp-qt-largura-r3    
               c-relatorio:range("Q" + STRING(i-linha)):VALUE = imp-qt-peso-r3    
               
               c-relatorio:range("R" + STRING(i-linha)):VALUE = imp-dt-trans-r3

               c-relatorio:range("V" + STRING(i-linha)):VALUE = imp-lote       
               
               c-relatorio:range("W" + STRING(i-linha)):VALUE = imp-qt-largura   
               c-relatorio:range("X" + STRING(i-linha)):VALUE = imp-dt-trans   
               c-relatorio:range("AB" + STRING(i-linha)):VALUE = imp-quantidade 
               c-relatorio:range("AC" + STRING(i-linha)):VALUE = def-primario [1]  
               c-relatorio:range("AD" + STRING(i-linha)):VALUE = def-secundario [1]. 


        ASSIGN lote-jr = imp-lote-r1.
        RUN pi-acha-operador.
        ASSIGN c-relatorio:range("G" + STRING(i-linha)):VALUE = nome-op-jr.

        ASSIGN lote-jr = imp-lote-r2.
        RUN pi-acha-operador.
        ASSIGN c-relatorio:range("N" + STRING(i-linha)):VALUE = nome-op-jr.

        ASSIGN lote-jr = imp-lote-r3.
        RUN pi-acha-operador.
        ASSIGN c-relatorio:range("U" + STRING(i-linha)):VALUE = nome-op-jr.

        ASSIGN lote-jr = imp-lote.
        RUN pi-acha-operador.
        ASSIGN c-relatorio:range("AA" + STRING(i-linha)):VALUE = nome-op-jr. 

        ASSIGN lote-jr1 = imp-lote-r1.

        RUN ver-turma-maq.
        ASSIGN c-relatorio:range("E" + STRING(i-linha)):VALUE = maq-jr1  
               c-relatorio:range("F" + STRING(i-linha)):VALUE = turma-jr1.    


        ASSIGN lote-jr1 = imp-lote-r2.

        RUN ver-turma-maq.
        ASSIGN c-relatorio:range("L" + STRING(i-linha)):VALUE = maq-jr1  
               c-relatorio:range("M" + STRING(i-linha)):VALUE = turma-jr1.    


        ASSIGN lote-jr1 = imp-lote-r3.

        RUN ver-turma-maq.
        ASSIGN c-relatorio:range("S" + STRING(i-linha)):VALUE = maq-jr1  
               c-relatorio:range("T" + STRING(i-linha)):VALUE = turma-jr1.    


        ASSIGN lote-jr1 = imp-lote.

        RUN ver-turma-maq.
        ASSIGN c-relatorio:range("Y" + STRING(i-linha)):VALUE = maq-jr1  
               c-relatorio:range("Z" + STRING(i-linha)):VALUE = turma-jr1.    


     ASSIGN i-jr = 1.

     DO WHILE i-jr < 10.

         ASSIGN i-jr = i-jr + 1.

         IF def-primario [i-jr] <> "" OR def-secundario [i-jr] <> "" THEN DO:
             ASSIGN i-linha = i-linha + 1.
             ASSIGN c-relatorio:range("AC" + STRING(i-linha)):VALUE = def-primario [i-jr]    
                    c-relatorio:range("AD" + STRING(i-linha)):VALUE = def-secundario [i-jr]. 
         END.

     END.

     END.

       ASSIGN tot-qtde = tot-qtde + ttbob-quantidade.

       IF DEF-secundario [1] <> "" THEN DO:
       
         FIND FIRST tt-defeitos-n WHERE
            ttdefn-def-secundario = def-secundario [1] AND
            ttdefn-it-codigo = ttbob-it-codigo
            USE-INDEX ch-tt-defeitos-n NO-ERROR.

         IF NOT AVAIL tt-defeitos-n THEN DO:
            CREATE tt-defeitos-n.
            ASSIGN ttdefn-def-secundario = def-secundario [1] 
                   ttdefn-it-codigo = ttbob-it-codigo.
         END.

         ASSIGN ttdefn-quantidade = ttdefn-quantidade + ttbob-quantidade.

         ASSIGN tot-defn = tot-defn + ttbob-quantidade.

         FIND FIRST mr-defeitos-n WHERE
            mrdefn-lote-mr = ttbob-lote-r1 AND
            mrdefn-def-secundario = def-secundario [1] AND
            mrdefn-it-codigo = ""
            USE-INDEX ch-mr-defeitos-n NO-ERROR.

         IF NOT AVAIL mr-defeitos-n THEN DO:
            CREATE mr-defeitos-n.
            ASSIGN mrdefn-def-secundario = def-secundario [1]
                   mrdefn-it-codigo = ""
                   mrdefn-lote-mr   = ttbob-lote-r1. 
         END.

         ASSIGN mrdefn-quantidade = mrdefn-quantidade + ttbob-quantidade.

         ASSIGN mr-tot-defn = mr-tot-defn + ttbob-quantidade.   


       END.


       IF DEF-primario [1] <> "" THEN DO:
       
         FIND FIRST tt-defeitos-r WHERE
            ttdefr-def-primario = def-primario [1] AND
            ttdefr-it-codigo = ttbob-it-codigo
            USE-INDEX ch-tt-defeitos-r NO-ERROR.

         IF NOT AVAIL tt-defeitos-r THEN DO:
            CREATE tt-defeitos-r.
            ASSIGN ttdefr-def-primario = def-primario [1] 
                   ttdefr-it-codigo = ttbob-it-codigo.
         END.

         ASSIGN ttdefr-quantidade = ttdefr-quantidade + ttbob-quantidade.
         
         ASSIGN tot-defr = tot-defr + ttbob-quantidade.
         
         FIND FIRST mr-defeitos-r WHERE
            mrdefr-lote-mr = ttbob-lote-r1 AND
            mrdefr-def-primario = def-primario [1] AND
            mrdefr-it-codigo = ""
            USE-INDEX ch-mr-defeitos-r NO-ERROR.

         IF NOT AVAIL mr-defeitos-r THEN DO:
            CREATE mr-defeitos-r.
            ASSIGN mrdefr-def-primario = def-primario [1]
                   mrdefr-it-codigo = ""
                   mrdefr-lote-mr   = ttbob-lote-r1. 
         END.

         ASSIGN mrdefr-quantidade = mrdefr-quantidade + ttbob-quantidade.

         ASSIGN mr-tot-defr = mr-tot-defr + ttbob-quantidade. 

       END.

   
end.

IF tt-param.destino <> 4 THEN DO:

        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp
            with stream-io frame f-relat-linha.
            down stream str-rp with frame f-relat-linha.  

        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp
            tot-qtde NO-LABEL
            with stream-io frame f-total-09-132.
            down stream str-rp with frame f-total-09-132.  

         view stream str-rp frame f-cabec.
         view stream str-rp frame f-rodape.
         assign l-imprime = yes.
         display stream str-rp
             with stream-io frame f-relat-linha.
             down stream str-rp with frame f-relat-linha.


         view stream str-rp frame f-cabec.
         view stream str-rp frame f-rodape.
         assign l-imprime = yes.
         display stream str-rp
             with stream-io frame f-relat-def-n.
             down stream str-rp with frame f-relat-def-n. 

END.

IF tt-param.destino = 4 THEN DO:

   ASSIGN i-linha = i-linha + 2.

   ASSIGN c-relatorio:range("M" + STRING(i-linha)):VALUE = "TOTAL.....:".  
          c-relatorio:range("N" + STRING(i-linha)):VALUE = tot-qtde.

   ASSIGN i-linha = i-linha + 2.

   ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE =
            "RESUMO POR TIPO DE DEFEITO - N«O REJEIÄ«O".

   ASSIGN i-linha = i-linha + 2.

   ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE = "Defeito"
          c-relatorio:range("H" + STRING(i-linha)):VALUE = "Filme"
          c-relatorio:range("M" + STRING(i-linha)):VALUE = "Quantidade"
          c-relatorio:range("N" + STRING(i-linha)):VALUE = "Percentual".

   ASSIGN i-linha = i-linha + 1.

END.

    
    FOR EACH tt-defeitos-n NO-LOCK :
        
        ASSIGN perc-ac-def = (ttdefn-quantidade / tot-defn) * 100.

        IF tt-param.destino <> 4 THEN DO:
        
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp 
            ttdefn-def-secundario
            ttdefn-it-codigo
            ttdefn-quantidade
            perc-ac-def
            with stream-io frame f-relat-defeitos-n.
            down stream str-rp with frame f-relat-defeitos-n.  

        END.

        ELSE DO:

           ASSIGN i-linha = i-linha + 1.
        
           ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE = ttdefn-def-secundario
                  c-relatorio:range("H" + STRING(i-linha)):VALUE = ttdefn-it-codigo
                  c-relatorio:range("M" + STRING(i-linha)):VALUE = ttdefn-quantidade
                  c-relatorio:range("N" + STRING(i-linha)):VALUE = perc-ac-def.      

        END.


    END.

    

    ASSIGN tot-qtde-def = tot-defn
           perc-ac-def  = (tot-defn / tot-qtde) * 100.  

    IF tt-param.destino <> 4 THEN DO:

    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp
        with stream-io frame f-relat-linha.
        down stream str-rp with frame f-relat-linha.

    ASSIGN VAR-total = "Tot.Defs.N∆o Rej.s/Tot.Produzido:".

    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp
        VAR-total    NO-LABEL
        tot-qtde-def NO-LABEL
        perc-ac-def  NO-LABEL
        with stream-io frame f-total-def.
        down stream str-rp with frame f-total-def.  

     view stream str-rp frame f-cabec.
     view stream str-rp frame f-rodape.
     assign l-imprime = yes.
     display stream str-rp
         with stream-io frame f-relat-linha.
         down stream str-rp with frame f-relat-linha.


     view stream str-rp frame f-cabec.
     view stream str-rp frame f-rodape.
     assign l-imprime = yes.
     display stream str-rp
         with stream-io frame f-relat-def-r.
         down stream str-rp with frame f-relat-def-r. 

    END.

    ELSE DO:
        ASSIGN i-linha = i-linha + 2.

        ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE =
               "Tot.Defs.N∆o Rej.s/Tot.Produzido"
               c-relatorio:range("M" + STRING(i-linha)):VALUE = tot-qtde-def
               c-relatorio:range("N" + STRING(i-linha)):VALUE = perc-ac-def.

        ASSIGN i-linha = i-linha + 2.

        ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE =
               "RESUMO POR TIPO DE DEFEITO - REJEIÄ«O". 

        ASSIGN i-linha = i-linha + 2.

        ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE = "Defeito"
               c-relatorio:range("H" + STRING(i-linha)):VALUE = "Filme"
               c-relatorio:range("M" + STRING(i-linha)):VALUE = "Quantidade"
               c-relatorio:range("N" + STRING(i-linha)):VALUE = "Percentual".

        ASSIGN i-linha = i-linha + 1.

    END.

    FOR EACH tt-defeitos-r NO-LOCK :
        
        ASSIGN perc-ac-def = (ttdefr-quantidade / tot-defr) * 100.
        
        IF tt-param.destino <> 4 THEN DO:
        
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp 
            ttdefr-def-primario
            ttdefr-it-codigo
            ttdefr-quantidade
            perc-ac-def
            with stream-io frame f-relat-defeitos-r.
            down stream str-rp with frame f-relat-defeitos-r.  

        END.

        ELSE DO:

           ASSIGN i-linha = i-linha + 1.
        
           ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE = ttdefr-def-primario
                  c-relatorio:range("H" + STRING(i-linha)):VALUE = ttdefr-it-codigo
                  c-relatorio:range("M" + STRING(i-linha)):VALUE = ttdefr-quantidade
                  c-relatorio:range("N" + STRING(i-linha)):VALUE = perc-ac-def.      


        END.

    END.

    ASSIGN tot-qtde-def = tot-defr
           perc-ac-def  = (tot-defr / tot-qtde) * 100.  

    IF tt-param.destino <> 4 THEN DO:
    
    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp
        with stream-io frame f-relat-linha.
        down stream str-rp with frame f-relat-linha.

    ASSIGN VAR-total = "Tot.Defs.Rejeiá∆o s/Tot.Produzido:".

    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp
        VAR-total    NO-LABEL
        tot-qtde-def NO-LABEL
        perc-ac-def  NO-LABEL
        with stream-io frame f-total-def.
        down stream str-rp with frame f-total-def.  

     view stream str-rp frame f-cabec.
     view stream str-rp frame f-rodape.
     assign l-imprime = yes.
     display stream str-rp
         with stream-io frame f-relat-linha.
         down stream str-rp with frame f-relat-linha.

    END.

    ELSE DO:

        ASSIGN i-linha = i-linha + 2.

        ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE =
               "Tot.Defs.Rejeiá∆o s/Tot.Produzido"
               c-relatorio:range("M" + STRING(i-linha)):VALUE = tot-qtde-def
               c-relatorio:range("N" + STRING(i-linha)):VALUE = perc-ac-def.

        ASSIGN i-linha = i-linha + 2.

        ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE =
               "RESUMO POR MILL ROLL - N«O REJEIÄ«O". 

        ASSIGN i-linha = i-linha + 2.

        ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE = "Mill Roll"
               c-relatorio:range("H" + STRING(i-linha)):VALUE = "Defeito"
               c-relatorio:range("M" + STRING(i-linha)):VALUE = "Quantidade"
               c-relatorio:range("N" + STRING(i-linha)):VALUE = "Percentual".

        ASSIGN i-linha = i-linha + 1.

    END.

/* Resumo de Defeitos por Mill Roll */  


    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp
        with stream-io frame mr-relat-def-n.
        down stream str-rp with frame mr-relat-def-n. 

    FOR EACH mr-defeitos-n NO-LOCK :
        
        ASSIGN perc-ac-def = (mrdefn-quantidade / mr-tot-defn) * 100.

        IF tt-param.destino <> 4  THEN DO:
        
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp
            mrdefn-lote-mr
            mrdefn-def-secundario
            mrdefn-quantidade
            perc-ac-def
            with stream-io frame f-mr-defeitos-n.
            down stream str-rp with frame f-mr-defeitos-n.  
      
        END.

        ELSE DO:
           
           ASSIGN i-linha = i-linha + 1.
        
           ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE = mrdefn-lote-mr
                  c-relatorio:range("H" + STRING(i-linha)):VALUE = mrdefn-def-secundario
                  c-relatorio:range("M" + STRING(i-linha)):VALUE = mrdefn-quantidade
                  c-relatorio:range("N" + STRING(i-linha)):VALUE = perc-ac-def.     
           
        END.
    END.

    ASSIGN tot-qtde-def = mr-tot-defn
           perc-ac-def  = (mr-tot-defn / tot-qtde) * 100.  

    IF tt-param.destino <> 4 THEN DO:
    
    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp
        with stream-io frame f-relat-linha.
        down stream str-rp with frame f-relat-linha.

    ASSIGN VAR-total = "Tot.Defs.N∆o Rej.s/Tot.Produzido:".

    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp
        VAR-total    NO-LABEL
        tot-qtde-def NO-LABEL
        perc-ac-def  NO-LABEL
        with stream-io frame f-total-def.
        down stream str-rp with frame f-total-def.  

     view stream str-rp frame f-cabec.
     view stream str-rp frame f-rodape.
     assign l-imprime = yes.
     display stream str-rp
         with stream-io frame f-relat-linha.
         down stream str-rp with frame f-relat-linha.


     view stream str-rp frame f-cabec.
     view stream str-rp frame f-rodape.
     assign l-imprime = yes.
     display stream str-rp
         with stream-io frame mr-relat-def-r.
         down stream str-rp with frame mr-relat-def-r. 
    
    END.

    ELSE DO:
        ASSIGN i-linha = i-linha + 2.

        ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE =
               "Tot.Defs.N∆o Rej.s/Tot.Produzido"
               c-relatorio:range("M" + STRING(i-linha)):VALUE = tot-qtde-def
               c-relatorio:range("N" + STRING(i-linha)):VALUE = perc-ac-def.

        ASSIGN i-linha = i-linha + 2.

        ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE =
               "RESUMO POR MILL ROLL - REJEIÄ«O". 

        ASSIGN i-linha = i-linha + 2.

        ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE = "Mill Roll"
               c-relatorio:range("H" + STRING(i-linha)):VALUE = "Defeito"
               c-relatorio:range("M" + STRING(i-linha)):VALUE = "Quantidade"
               c-relatorio:range("N" + STRING(i-linha)):VALUE = "Percentual".

        ASSIGN i-linha = i-linha + 1.  

    END.

    FOR EACH mr-defeitos-r NO-LOCK :
        
        ASSIGN perc-ac-def = (mrdefr-quantidade / mr-tot-defr) * 100.

        IF tt-param.destino <> 4 THEN DO:
        
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp 
            mrdefr-lote-mr
            mrdefr-def-primario
            mrdefr-quantidade
            perc-ac-def
            with stream-io frame f-mr-defeitos-r.
            down stream str-rp with frame f-mr-defeitos-r.  

        END.

        ELSE DO:
           
           ASSIGN i-linha = i-linha + 1.
        
           ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE = mrdefr-lote-mr
                  c-relatorio:range("H" + STRING(i-linha)):VALUE = mrdefr-def-primario
                  c-relatorio:range("M" + STRING(i-linha)):VALUE = mrdefr-quantidade
                  c-relatorio:range("N" + STRING(i-linha)):VALUE = perc-ac-def.     
                  
        END.      

    END.

    ASSIGN tot-qtde-def = mr-tot-defr
           perc-ac-def  = (mr-tot-defr / tot-qtde) * 100.  

    IF tt-param.destino <> 4 THEN DO:

    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp
        with stream-io frame f-relat-linha.
        down stream str-rp with frame f-relat-linha.

    ASSIGN VAR-total = "Tot.Defs.Rejeiá∆o s/Tot.Produzido:".

    view stream str-rp frame f-cabec.
    view stream str-rp frame f-rodape.
    assign l-imprime = yes.
    display stream str-rp
        VAR-total    NO-LABEL
        tot-qtde-def NO-LABEL
        perc-ac-def  NO-LABEL
        with stream-io frame f-total-def.
        down stream str-rp with frame f-total-def.  

     view stream str-rp frame f-cabec.
     view stream str-rp frame f-rodape.
     assign l-imprime = yes.
     display stream str-rp
         with stream-io frame f-relat-linha.
         down stream str-rp with frame f-relat-linha.

    END.

    ELSE DO:
        ASSIGN i-linha = i-linha + 2.

        ASSIGN c-relatorio:range("C" + STRING(i-linha)):VALUE =
               "Tot.Defs.Rejeiá∆o s/Tot.Produzido"
               c-relatorio:range("M" + STRING(i-linha)):VALUE = tot-qtde-def
               c-relatorio:range("N" + STRING(i-linha)):VALUE = perc-ac-def.
                                  
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


   disp stream str-rp "SELEÄ«O" skip(01) with stream-io frame f-imp-sel.
   disp stream str-rp 
      c-cod-estabel-ini colon 23 
        with stream-io side-labels overlay row 034 frame f-imp-sel.

   disp stream str-rp "CLASSIFICAÄ«O" skip(01) with stream-io frame f-imp-cla.
   disp stream str-rp "   Bobinas Cortadas no Per°odo"
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

    c-arquivo = c-arq + 'escp0030' + STRING(time)+ '.xls'.

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


           IF nome-op-jr = "" THEN DO:

               FIND FIRST movto-estoq OF movto-mat NO-LOCK.

               IF AVAIL movto-estoq THEN DO:
               
                   FIND FIRST usuar_mestre WHERE
                       usuar_mestre.cod_usuario = movto-estoq.usuario
                       NO-LOCK NO-ERROR.

                   IF AVAIL usuar_mestre THEN
                       ASSIGN nome-op-jr = usuar_mestre.nom_usuario.

               END.

           END.

        END.    

    END.

END PROCEDURE.

PROCEDURE ver-turma-maq.

    ASSIGN turma-jr1    = ""
           maq-jr1      = 0.


    FIND FIRST lote-prod WHERE
        lote-prod.lote = lote-jr1 
        USE-INDEX lote
        NO-LOCK NO-ERROR.

    IF AVAIL lote-prod THEN DO:
    
        FIND FIRST lote-carac-tec WHERE
               lote-carac-tec.it-codigo   = lote-prod.it-codigo
               and lote-carac-tec.lote    = lote-prod.lote
               and lote-carac-tec.cd-comp = "maq"
               NO-LOCK NO-ERROR.
        
        if avail lote-carac-tec then 
            ASSIGN maq-jr1 = lote-carac-tec.vl-result.
        
        FIND FIRST lote-carac-tec WHERE
               lote-carac-tec.it-codigo   = lote-prod.it-codigo
               and lote-carac-tec.lote    = lote-prod.lote
               and lote-carac-tec.cd-comp = "turma"
               NO-LOCK NO-ERROR.
        
        if avail lote-carac-tec then 
            ASSIGN turma-jr1 = lote-carac-tec.observacao.

    END.
        
END PROCEDURE.



return 'OK'.

/* fim do programa */
