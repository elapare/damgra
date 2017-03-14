/*****************************************************************************
**
**       PROGRAMA: esft0012RP
**
**       DATA....: Setembro de 2004
**
**       AUTOR...: Eldo Santiago
**
**       OBJETIVO: Demonstrativo de Faturamento
**
**       VERSAO..: 2.00
**
*****************************************************************************/
def buffer empresa for mgmulti.empresa.
{include/i-prgvrs.i esft0012RP 2.00.00.006}  /*** 010006 ***/
 
define temp-table tt-param
    field destino          as integer
    field arquivo          as char
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    FIELD cod-estabel-ini  AS CHAR FORMAT "X(3)"
    FIELD cod-estabel-fim  AS CHAR FORMAT "X(3)" INIT 999
    FIELD estado-ini       AS CHAR FORMAT "x(2)"  
    FIELD estado-fim       AS CHAR FORMAT "x(2)" INIT "ZZ"
    FIELD cod-emitente-ini AS INT FORMAT ">>>>>>>>9"
    FIELD cod-emitente-fim AS INT FORMAT ">>>>>>>>9" INIT "99999999"
    FIELD fm-codigo-ini    AS CHAR FORMAT "x(8)"
    FIELD fm-codigo-fim    AS CHAR FORMAT "x(8)" INIT "ZZZZZZZZ"
    FIELD fm-cod-com-ini AS CHAR FORMAT "x(8)"
    FIELD fm-cod-com-fim AS CHAR FORMAT "x(8)"   INIT "ZZZZZZZZ"
    FIELD it-codigo-ini    AS CHAR FORMAT "x(20)"
    FIELD it-codigo-fim    AS CHAR FORMAT "x(20)" INIT "ZZZZZZZZZZZZZZZZZZZZ" 
    FIELD cod-canal-venda-ini AS INT FORMAT ">>>>>"
    FIELD cod-canal-venda-fim AS INT FORMAT ">>>>>" INIT 99999
    FIELD nat-operacao-ini AS CHAR FORMAT "x(8)"
    FIELD nat-operacao-fim AS CHAR FORMAT "x(8)"   INIT "ZZZZZZZZ"
    FIELD ge-codigo-ini  AS  INT FORMAT "99999"
    FIELD ge-codigo-fim  AS  INT FORMAT "99999" INIT 999
    FIELD dt-emis-nota-ini  AS DATE FORMAT "99/99/9999" INIT "01/01/0001"
    FIELD dt-emis-nota-fim  AS DATE FORMAT "99/99/9999" INIT "12/31/9999"
    FIELD nr-nota-fis-ini  AS CHAR FORMAT "x(8)"
    FIELD nr-nota-fis-fim  AS CHAR FORMAT "x(8)"
    FIELD cod-gr-cli-ini  AS  INT FORMAT "99999"
    FIELD cod-gr-cli-fim  AS  INT FORMAT "99999" INIT 999
    FIELD nome-mic-reg-ini AS CHAR FORMAT "x(12)" 
    FIELD nome-mic-reg-fim AS CHAR FORMAT "x(12)" INIT "ZZZZZZZZZZZZ"
    FIELD r-tipo          AS LOGICAL INIT "yes"
    FIELD l-nota          AS LOGICAL INIT "yes"
    FIELD n-devol          AS LOGICAL INIT "yes"
    FIELD l-item          AS LOGICAL INIT "yes"
    FIELD t-moeda          AS LOGICAL INIT "yes"
    FIELD t-total         AS LOGICAL INIT "yes"
    FIELD s-desc          AS LOGICAL INIT "no"
    FIELD n-fatur         AS LOGICAL INIT "yes"
    FIELD l-imposto       AS LOGICAL INIT "no"
    FIELD rs-classif      AS INTEGER INIT 1
    FIELD t-devoluc       AS LOGICAL INIT "yes"
    FIELD l-consig        AS LOGICAL INIT "no"
    FIELD l-email         AS LOGICAL INIT "no"
    field l-rpw           as logical  INIT "no"
    FIELD c-grp-usuar    AS CHAR 
    field saida           as integer
.



DEFINE TEMP-TABLE tt-notas
    FIELD cod-estabel      LIKE nota-fiscal.cod-estabel
    FIELD serie            LIKE nota-fiscal.serie 
    FIELD nr-nota-fis      LIKE nota-fiscal.nr-nota-fis
    FIELD identific        AS int
    INDEX ch-tt-notas IS PRIMARY UNIQUE  cod-estabel
                                         serie
                                         nr-nota-fis.



{utp/utapi019.i}
/*CUSTOMIZACAO*/
DEFINE NEW GLOBAL SHARED  VARIABLE c-seg-usuario AS CHARACTER  NO-UNDO.

/*DEFINICAO DE VARIAVEIS PARA ACUMULUDADORES por PRODUTO*/
DEFINE VARIABLE dt-atu AS DATE        NO-UNDO.
def var acum-peso-liq-fat-des       as dec format "->>>,>>>,>>9.999" init 0.
def var acum-qt-faturada-des        as dec format "->>>,>>>,>>9.999" init 0.
def var acum-vl-tot-item-des        as dec format "->>>,>>>,>>9.99"  init 0.
def var acum-vl-icms-it-des         as dec format "->>,>>>,>>9.99"   init 0.
def var acum-vl-ipi-it-des          as dec format "->>,>>>,>>9.99"   init 0.
def var acum-vl-finsocial-des       as dec format "->>,>>>,>>9.99"   init 0.
def var acum-vl-pis-des             as dec format "->>,>>>,>>9.99"   init 0.
def var acum-enc-financ-des         as dec format "->>,>>>,>>9.99"   init 0.
def var acum-vl-merc-liq-des        as dec format "->>>,>>>,>>9.99"  init 0.
def var acum-vl-despes-it-des       as dec format "->>,>>>,>>9.99"   init 0.

def var acum-vl-tot-item-dolar-des        as dec format "->>>,>>>,>>9.99" init 0.
def var acum-vl-icms-it-dolar-des         as dec format "->>,>>>,>>9.99"  init 0.
def var acum-vl-ipi-it-dolar-des          as dec format "->>,>>>,>>9.99"  init 0.
def var acum-vl-finsocial-dolar-des       as dec format "->>,>>>,>>9.99"  init 0.
def var acum-vl-pis-dolar-des             as dec format "->>,>>>,>>9.99"  init 0.
def var acum-enc-financ-dolar-des         as dec format "->>,>>>,>>9.99"  init 0.
def var acum-vl-merc-liq-dolar-des        as dec format "->>>,>>>,>>9.99" init 0.
def var acum-vl-despes-it-dolar-des       as dec format "->>,>>>,>>9.99"  init 0.

/*definicao de variaveis de totais gerais por produto*/

def var t-acum-peso-liq-fat-des      as dec format "->>>,>>>,>>9.999" init 0.
def var t-acum-qt-faturada-des       as dec format "->>>,>>>,>>9.999" init 0.
def var t-acum-vl-tot-item-des       as dec format "->>>,>>>,>>9.99"  init 0.
def var t-acum-vl-icms-it-des        as dec format "->>,>>>,>>9.99"   init 0.
def var t-acum-vl-ipi-it-des         as dec format "->>,>>>,>>9.99"   init 0.
def var t-acum-vl-finsocial-des      as dec format "->>,>>>,>>9.99"   INIT 0.
def var t-acum-vl-pis-des            as dec format "->>,>>>,>>9.99"   INIT 0.
def var t-acum-vl-despes-it-des      as dec format "->>,>>>,>>9.99"   init 0.
def var t-acum-enc-financ-des        as dec format "->>,>>>,>>9.99"   init 0.
def var t-acum-vl-merc-liq-des       as dec format "->>>,>>>,>>9.99"  init 0.

def var t-acum-vl-tot-item-dolar-des       as dec format "->>>,>>>,>>9.99" init 0.
def var t-acum-vl-icms-it-dolar-des        as dec format "->>,>>>,>>9.99"  init 0.
def var t-acum-vl-ipi-it-dolar-des         as dec format "->>,>>>,>>9.99"  init 0.
def var t-acum-vl-finsocial-dolar-des      as dec format "->>,>>>,>>9.99"  init 0.
def var t-acum-vl-pis-dolar-des            as dec format "->>,>>>,>>9.99"  init 0.
def var t-acum-vl-despes-it-dolar-des      as dec format "->>,>>>,>>9.99"  init 0.
def var t-acum-enc-financ-dolar-des        as dec format "->>,>>>,>>9.99"  init 0.
def var t-acum-vl-merc-liq-dolar-des       as dec format "->>>,>>>,>>9.99" init 0.

/*DEFINICAO DE VARIAVEIS PARA ACUMULUDADORES POR cfoILIA*/

def var acum-peso-liq-fat-cfo       as dec format "->>>,>>>,>>9.999" init 0.
def var acum-qt-faturada-cfo        as dec format "->>>,>>>,>>9.999" init 0.
def var acum-vl-tot-item-cfo        as dec format "->>>,>>>,>>9.99"  init 0.
def var acum-vl-icms-it-cfo         as dec format "->>,>>>,>>9.99"   init 0.
def var acum-vl-ipi-it-cfo          as dec format "->>,>>>,>>9.99"   init 0.
def var acum-vl-finsocial-cfo       as dec format "->>,>>>,>>9.99"   init 0.
def var acum-vl-pis-cfo             as dec format "->>,>>>,>>9.99"   init 0.
def var acum-enc-financ-cfo         as dec format "->>,>>>,>>9.99"   init 0.
def var acum-vl-merc-liq-cfo        as dec format "->>>,>>>,>>9.99"  init 0.
def var acum-vl-despes-it-cfo       as dec format "->>,>>>,>>9.99"   init 0.

def var acum-vl-tot-item-dolar-cfo        as dec format "->>>,>>>,>>9.99" init 0.
def var acum-vl-icms-it-dolar-cfo         as dec format "->>,>>>,>>9.99"  init 0.
def var acum-vl-ipi-it-dolar-cfo          as dec format "->>,>>>,>>9.99"  init 0.
def var acum-vl-finsocial-dolar-cfo       as dec format "->>,>>>,>>9.99"  init 0.
def var acum-vl-pis-dolar-cfo             as dec format "->>,>>>,>>9.99"  init 0.
def var acum-enc-financ-dolar-cfo         as dec format "->>,>>>,>>9.99"  init 0.
def var acum-vl-merc-liq-dolar-cfo        as dec format "->>>,>>>,>>9.99" init 0.
def var acum-vl-despes-it-dolar-cfo       as dec format "->>,>>>,>>9.99"  init 0.

/*definicao de variaveis de totais gerais por cfoilia*/

def var t-acum-peso-liq-fat-cfo            as dec format "->>>,>>>,>>9.999"  INIT 0.
def var t-acum-qt-faturada-cfo             as dec format "->>>,>>>,>>9.999"  INIT 0.
def var t-acum-vl-tot-item-cfo             as dec format "->>>,>>>,>>9.99"   init 0.
def var t-acum-vl-icms-it-cfo              as dec format "->>,>>>,>>9.99"    init 0.
def var t-acum-vl-ipi-it-cfo               as dec format "->>,>>>,>>9.99"    init 0.
def var t-acum-vl-finsocial-cfo            as dec format "->>,>>>,>>9.99"    init 0.
def var t-acum-vl-pis-cfo                  as dec format "->>,>>>,>>9.99"    init 0.
def var t-acum-vl-despes-it-cfo            as dec format "->>,>>>,>>9.99"    init 0.
def var t-acum-enc-financ-cfo              as dec format "->>,>>>,>>9.99"    init 0.
def var t-acum-vl-merc-liq-cfo             as dec format "->>>,>>>,>>9.99"   init 0.
def var t-acum-vl-tot-item-dolar-cfo       as dec format "->>>,>>>,>>9.99"   init 0.
def var t-acum-vl-icms-it-dolar-cfo        as dec format "->>,>>>,>>9.99"    init 0.
def var t-acum-vl-ipi-it-dolar-cfo         as dec format "->>,>>>,>>9.99"    init 0.
def var t-acum-vl-finsocial-dolar-cfo      as dec format "->>,>>>,>>9.99"    init 0.
def var t-acum-vl-pis-dolar-cfo            as dec format "->>,>>>,>>9.99"    init 0.
def var t-acum-vl-despes-it-dolar-cfo      as dec format "->>,>>>,>>9.99"    init 0.
def var t-acum-enc-financ-dolar-cfo        as dec format "->>,>>>,>>9.99"    init 0.
def var t-acum-vl-merc-liq-dolar-cfo       as dec format "->>>,>>>,>>9.99"   init 0.

/*definicao de variaveis para propositos gerais*/
DEF VAR aux-dt-emis-nota             LIKE it-nota-fisc.dt-emis-nota.
def var de-cotacao                   as dec FORMAT ">>>,>>9.99999999" no-undo.
def var v-pis                        as dec format ">,>>>,>>9.99".
def var v-cofins                     as dec format ">,>>>,>>9.99".
DEF VAR valor-liquido                as dec format "->>>,>>>,>>9.99".
DEF VAR aux-vl-despes-it             as dec format "->>,>>>,>>9.99".
DEF VAR aux-vl-merc-liq              as dec format "->>>,>>>,>>9.99".
DEF VAR encargos-financ              as dec format "->>,>>>,>>9.99".
DEF VAR aux-qt-faturada              as dec format "->>>,>>>,>>9.999" INIT 0.
DEF VAR aux-peso-liq-fat             as dec format "->>>,>>>,>>9.999".
DEF VAR aux-vl-tot-item              as dec format "->>>,>>>,>>9.99".
DEF VAR aux-vl-ipi-it                as dec format "->>,>>>,>>9.99".
DEF VAR aux-vl-ipi-it-jr             as dec format "->>,>>>,>>9.99".
DEF VAR aux-vl-icms-it-jr            as dec format "->>,>>>,>>9.99".
DEF VAR aux-vl-icms-it               as dec format "->>,>>>,>>9.99".
def var v-pis-dev                    as dec format ">,>>>,>>9.99".
def var v-cofins-dev                 as dec format ">,>>>,>>9.99".
DEF VAR valor-liquido-dev            as dec format "->>>,>>>,>>9.99".
DEF VAR aux-vl-despes-it-dev         as dec format "->>,>>>,>>9.99".
DEF VAR aux-vl-merc-liq-dev          as dec format "->>>,>>>,>>9.99".
DEF VAR encargos-financ-dev          as dec format "->>,>>>,>>9.99".
DEF VAR aux-qt-dev                   as dec format "->>>,>>>,>>9.999" INIT 0.
DEF VAR aux-qt-faturada-dev          as dec format "->>>,>>>,>>9.999" INIT 0.
DEF VAR aux-peso-liq-fat-dev         as dec format "->>>,>>>,>>9.999".
DEF VAR aux-vl-tot-item-dev          as dec format "->>>,>>>,>>9.99".
DEF VAR aux-vl-ipi-it-dev            as dec format "->>,>>>,>>9.99".
DEF VAR aux-vl-icms-it-dev           as dec format "->>,>>>,>>9.99".

DEF VAR tt-aux-peso-liq-fat-dev     as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-aux-qt-faturada-dev      as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-aux-vl-tot-item-dev      as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-aux-vl-icms-it-dev       as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-aux-vl-ipi-it-dev        as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-v-cofins-dev             as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-v-pis-dev                as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-encargos-financ-dev      as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-valor-liquido-dev        as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-aux-vl-despes-it-dev     as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.

DEF VAR v-aliq-pis                   as dec.
DEF VAR v-aliq-cof                   as dec.
DEF VAR tipo      AS CHAR EXTENT 7.
DEF VAR I         AS INT INIT 1.
DEF VAR aux-campo LIKE ITEM.desc-item FORMAT "x(20)".
DEF VAR aux-campo1 LIKE ITEM.desc-item FORMAT "x(20)".
DEF VAR recebe    AS CHAR FORMAT "x(10)".
DEF VAR auxpeso   AS DEC FORMAT "->>>,>>>,>>9.999".
DEF VAR auxmer    AS CHAR FORMAT "x(35)".
DEF VAR auxdesc   AS CHAR FORMAT "x(35)".
DEF VAR hExcel            AS COM-HANDLE NO-UNDO.
def var c-planilha         as COM-HANDLE NO-UNDO.
def var c-dir-dest           as char.
def var c-arq-dest           as char.

DEF VAR iLin              AS INTEGER    NO-UNDO.
DEF VAR icolimp           AS INTEGER    NO-UNDO.
DEF VAR h-acomp           AS HANDLE NO-UNDO.
DEFINE VARIABLE detalhe2  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE detalhe1  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE tex-qtd-peso AS CHARACTER  NO-UNDO.
DEFINE VARIABLE impost    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE imp-linha AS CHARACTER  NO-UNDO.
DEFINE VARIABLE acel      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE fullname  AS CHARACTER  NO-UNDO.

DEFINE BUFFER nfco-natur-oper   FOR natur-oper.
DEFINE BUFFER nfco-it-nota-fisc FOR it-nota-fisc.
DEFINE BUFFER b-movto-estoq     FOR movto-estoq.

DEFINE VARIABLE v-icms     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE v-ipi      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE v-vl-tot   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE v-qtde     AS DECIMAL    NO-UNDO.

DEF NEW GLOBAL SHARED VAR lContaFtPorCliente AS LOG NO-UNDO INIT ?.
DEFINE VARIABLE h-cd9500 AS HANDLE      NO-UNDO.
DEFINE VARIABLE r-conta-ft AS ROWID       NO-UNDO.
DEFINE VARIABLE c-conta-receita AS CHARACTER FORMAT "x(16)"   NO-UNDO.
DEFINE VARIABLE c-sc-receita    AS CHARACTER FORMAT "x(16)"   NO-UNDO.
run cdp/cd9500.p persistent set h-cd9500.

/* VENCIMENTO MêDIO */
DEFINE VARIABLE dt-vencto-medio AS DATE        NO-UNDO.
DEFINE VARIABLE i-qtd-vencto    AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-qt-parcela    AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-dias-medio    AS INTEGER     NO-UNDO.

DEF BUFFER b-fat-duplic FOR fat-duplic.

DEF BUFFER b-natur-oper FOR natur-oper.
    def var c-modelo-planilha as char no-undo.
    def var c-modelo-logo as char no-undo.
    def new global shared var i-ep-codigo-usuario  like empresa.ep-codigo no-undo.

ASSIGN detalhe2 = "---------------        ---------------     ------------   
   ------------     ------------      -----------    --------------      
--------------"
       detalhe1 = "---------------        ---------------     ------------   
   ------------     ------------      -----------      -----------   
---------------    --------------      --------------".

aCel = 
"A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,AB,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AM,AN,AO,AP,AQ,AR,AS,AT,AU,AV,AW,AX,AY,AZ".

def temp-table tt-raw-digita
    field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

IF  l-rpw AND l-email THEN DO:  /*se for via rpw sempre roda a partir do dia anterior*/

    assign 
             tt-param.dt-emis-nota-ini = today - 1
             tt-param.dt-emis-nota-fim = today - 1.
 
END.



FIND FIRST estabelec WHERE estabelec.cod-estabel >= tt-param.cod-estabel-ini NO-LOCK NO-ERROR.
IF NOT AVAIL estabelec THEN
   DO:
      MESSAGE "N∆o foi poss°vel encontrar o 
       modelo para o aplicativo Microsoft Excel."
            VIEW-AS ALERT-BOX ERROR.
      UNDO, RETURN.
   END.

FIND empresa WHERE 
     empresa.ep-codigo = estabelec.ep-codigo NO-LOCK NO-ERROR.

/*c-modelo-logo = IF SEARCH("logo" + STRING(estabelec.ep-codigo) + ".jpg") = ?
     THEN SEARCH("logo120.jpg") ELSE SEARCH("logo" + STRING(estabelec.ep-codigo) + ".jpg").*/

c-modelo-logo = SEARCH("logo320.jpg").

     /* dos silent del VALUE(SESSION:TEMP-DIRECTORY + "esft0012*.x*"). */

   
run utp/ut-acomp.p persistent set h-acomp.

{utp/ut-glob.i}

/*********{include/i-rpvar.i}**********************************************/
/*****************************************************************************
**
**  I-RPVAR.I - Variaveis para Impress o do Cabecalho Padr o (ex-CD9500.I)
**
*****************************************************************************/

define var c-empresa       as character format "x(40)"      no-undo.
define var c-titulo-relat  as character format "x(50)"      no-undo.
define var c-sistema       as character format "x(25)"      no-undo.
define var i-numper-x      as integer   format "ZZ"         no-undo.
define var da-iniper-x     as date      format "99/99/9999" no-undo.
define var da-fimper-x     as date      format "99/99/9999" no-undo.
define var c-rodape        as character                     no-undo.
define var v_num_count     as integer                       no-undo.
define var c-arq-control   as character                     no-undo.
define var i-page-size-rel as integer                       no-undo.
define var c-programa      as character format "x(08)"      no-undo.
define var c-versao        as character format "x(04)"      no-undo.
define var c-revisao       as character format "999"        no-undo.
define var c-impressora   as character                      no-undo.
define var c-layout       as character                      no-undo.
DEFINE VARIABLE v_output_file AS CHARACTER   NO-UNDO.
DEFINE VARIABLE v_cod_relat AS CHARACTER   NO-UNDO.
DEFINE VARIABLE v_cod_file_config AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h_pdf_controller AS HANDLE      NO-UNDO.
define buffer b_ped_exec_style for ped_exec.
define buffer b_servid_exec_style for servid_exec.
&IF "{&SHARED}" = "YES":U &THEN
    define shared stream str-rp.
&ELSE
    define new shared stream str-rp.
&ENDIF
{include/i-lgcode.i}
/* i-rpvar.i */



    /*************************************************/
find first param-global no-lock no-error.
find first tt-param no-lock no-error.

{include/i-rpcab.i}
/*{include/i-rpout.i}*/
    /**************************************************************************
**
** I-RPOUT - Define sa°da para impress∆o do relat¢rio - ex. cd9520.i
** Parametros: {&stream} = nome do stream de saida no formato "stream nome"
**             {&append} = append    
**             {&tofile} = nome da vari†vel ou campo com arquivo de destino
**             {&pagesize} = tamanho da pagina
***************************************************************************/

def new global shared var c-dir-spool-servid-exec as char no-undo.
def new global shared var i-num-ped-exec-rpw as int no-undo.

if  tt-param.destino = 1 then do:
   &if "{&tofile}" = "" &then 
    if num-entries(tt-param.arquivo,":") = 2 then do:
   &elseif "{&tofile}" <> "" &then
    if num-entries({&tofile},":") = 2 then do:
   &endif.                            
    &if "{&tofile}" = "" &then 
        assign c-impressora = substring(tt-param.arquivo,1,index(tt-param.arquivo,":") - 1).
        assign c-layout     = substring(tt-param.arquivo,index(tt-param.arquivo,":") + 1,length(tt-param.arquivo) - index(tt-param.arquivo,":")). 
    &elseif "{&tofile}" <> "" &then
        assign c-impressora = substring({&tofile},1,index({&tofile},":") - 1).
        assign c-layout     = substring({&tofile},index({&tofile},":") + 1,length({&tofile}) - index({&tofile},":")). 
    &endif.                            

    find layout_impres no-lock
        where layout_impres.nom_impressora    = c-impressora
        and   layout_impres.cod_layout_impres = c-layout no-error.
    find imprsor_usuar no-lock
        where imprsor_usuar.nom_impressora = c-impressora
        and   imprsor_usuar.cod_usuario    = tt-param.usuario
        use-index imprsrsr_id no-error.
    find impressora  of imprsor_usuar no-lock no-error.
    find tip_imprsor of impressora    no-lock no-error.

    if  i-num-ped-exec-rpw <> 0 then do:
        find b_ped_exec_style where b_ped_exec_style.num_ped_exec = i-num-ped-exec-rpw no-lock no-error. 
        find b_servid_exec_style of b_ped_exec_style no-lock no-error.
        find servid_exec_imprsor of b_servid_exec_style
          where servid_exec_imprsor.nom_impressora = imprsor_usuar.nom_impressora no-lock no-error.

        if  available b_servid_exec_style and b_servid_exec_style.ind_tip_fila_exec = 'UNIX'
        then do:
            output {&stream} through value(servid_exec_imprsor.nom_disposit_so)
                   page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
        end /* if */.
        else do:
            output {&stream}  to value(servid_exec_imprsor.nom_disposit_so)
                   page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
        end /* else */.
    end.
    else do:
        if layout_impres.num_lin_pag = 0 then do:
            /* sem salta pògina */
            output  {&stream} 
                    to value(imprsor_usuar.nom_disposit_so)
                    page-size 0
                    convert target tip_imprsor.cod_pag_carac_conver . 
        end.
        else do:
            /* com salta p†gina */
            output {&stream} 
                    to value(imprsor_usuar.nom_disposit_so)
                    paged page-size value(layout_impres.num_lin_pag) 
                    convert target tip_imprsor.cod_pag_carac_conver.
        end.
    end.

    for each configur_layout_impres no-lock
        where configur_layout_impres.num_id_layout_impres = layout_impres.num_id_layout_impres
        by configur_layout_impres.num_ord_funcao_imprsor:

        find configur_tip_imprsor no-lock
            where configur_tip_imprsor.cod_tip_imprsor        = layout_impres.cod_tip_imprsor
            and   configur_tip_imprsor.cod_funcao_imprsor     = configur_layout_impres.cod_funcao_imprsor
            and   configur_tip_imprsor.cod_opc_funcao_imprsor = configur_layout_impres.cod_opc_funcao_imprsor
            use-index cnfgrtpm_id no-error.
    
        do v_num_count = 1 to extent(configur_tip_imprsor.num_carac_configur):
          case configur_tip_imprsor.num_carac_configur[v_num_count]:
            when 0 then put {&stream} control null.
            when ? then leave.
            otherwise   put {&stream} control CODEPAGE-CONVERT(chr(configur_tip_imprsor.num_carac_configur[v_num_count]),
                                                               session:cpinternal, 
                                                               tip_imprsor.cod_pag_carac_conver).
          end case.
        end.
    end.
  end.
  else do:
    &if "{&tofile}" = "" &then 
        assign c-impressora  = entry(1,tt-param.arquivo,":").
        assign c-layout      = entry(2,tt-param.arquivo,":"). 
        if num-entries(tt-param.arquivo,":") = 4 then
          assign c-arq-control = entry(3,tt-param.arquivo,":") + ":" + entry(4,tt-param.arquivo,":").
        else 
          assign c-arq-control = entry(3,tt-param.arquivo,":").
    &elseif "{&tofile}" <> "" &then
        assign c-impressora  = entry(1,{&tofile},":").
        assign c-layout      = entry(2,{&tofile},":").
      &if "{&tofile}" = "" &then 
        if num-entries(tt-param.arquivo,":") = 4 then
      &elseif "{&tofile}" <> "" &then
        if num-entries({&tofile},":") = 4 then
      &endif
          assign c-arq-control = entry(3,{&tofile},":") + ":" + entry(4,{&tofile},":").
        else
          assign c-arq-control = entry(3,{&tofile},":").
    &endif.                            

    find layout_impres no-lock
        where layout_impres.nom_impressora    = c-impressora
        and   layout_impres.cod_layout_impres = c-layout no-error.
    find imprsor_usuar no-lock
        where imprsor_usuar.nom_impressora = c-impressora
        and   imprsor_usuar.cod_usuario    = tt-param.usuario
        use-index imprsrsr_id no-error.
    find impressora  of imprsor_usuar no-lock no-error.
    find tip_imprsor of impressora    no-lock no-error.

    if  i-num-ped-exec-rpw <> 0 then do:
        find b_ped_exec_style where b_ped_exec_style.num_ped_exec = i-num-ped-exec-rpw no-lock no-error. 
        find b_servid_exec_style of b_ped_exec_style no-lock no-error.
        find servid_exec_imprsor of b_servid_exec_style 
          where servid_exec_imprsor.nom_impressora = imprsor_usuar.nom_impressora no-lock no-error.

        if  available b_servid_exec_style and b_servid_exec_style.ind_tip_fila_exec = 'UNIX'
        then do:
            output {&stream} to value(c-dir-spool-servid-exec + "~/" + c-arq-control)
                   page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
        end /* if */.
        else do:
            output {&stream}  to value(c-dir-spool-servid-exec + "~/" + c-arq-control)
                   page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
        end /* else */.
    end.
    else do:
        if layout_impres.num_lin_pag = 0 then do:
            /* sem salta pògina */
            output  {&stream} 
                    to value(c-arq-control)
                    page-size 0
                    convert target tip_imprsor.cod_pag_carac_conver . 
        end.
        else do:
            /* com salta p†gina */
            output {&stream} 
                    to value(c-arq-control)
                    paged page-size value(layout_impres.num_lin_pag) 
                    convert target tip_imprsor.cod_pag_carac_conver.
        end.
    end.

    for each configur_layout_impres no-lock
        where configur_layout_impres.num_id_layout_impres = layout_impres.num_id_layout_impres
        by configur_layout_impres.num_ord_funcao_imprsor:

        find configur_tip_imprsor no-lock
            where configur_tip_imprsor.cod_tip_imprsor        = layout_impres.cod_tip_imprsor
            and   configur_tip_imprsor.cod_funcao_imprsor     = configur_layout_impres.cod_funcao_imprsor
            and   configur_tip_imprsor.cod_opc_funcao_imprsor = configur_layout_impres.cod_opc_funcao_imprsor
            use-index cnfgrtpm_id no-error.
    
        do v_num_count = 1 to extent(configur_tip_imprsor.num_carac_configur):
          case configur_tip_imprsor.num_carac_configur[v_num_count]:
            when 0 then put {&stream} control null.
            when ? then leave.
            otherwise   put {&stream} control 
              CODEPAGE-CONVERT(chr(configur_tip_imprsor.num_carac_configur[v_num_count]),
                               session:cpinternal, 
                               tip_imprsor.cod_pag_carac_conver).
                            
          end case.
        end.
    end.
  end.  
end.
else do:
    &if "{&tofile}" = "" &then
        if  i-num-ped-exec-rpw <> 0 then do:
          &if "{&pagesize}" = "" &then
            output {&stream} 
                   to value(c-dir-spool-servid-exec + "~/" + tt-param.arquivo) 
                   paged page-size 64
                   convert target "iso8859-1" {&append}.
          &else         
            assign i-page-size-rel = integer("{&pagesize}").
            output {&stream} 
                   to value(c-dir-spool-servid-exec + "~/" + tt-param.arquivo) 
                   paged page-size value(i-page-size-rel)
                   convert target "iso8859-1" {&append}.
          &endif          
        end.                             
        else do:
          &if "{&pagesize}" = "" &then
            output {&stream} 
                   to value(tt-param.arquivo) 
                   paged page-size 64 
                   convert target "iso8859-1" {&append}.
          &else
            assign i-page-size-rel = integer("{&pagesize}").
            output {&stream} 
                   to value(tt-param.arquivo) 
                   paged page-size value(i-page-size-rel)
                   convert target "iso8859-1" {&append}.
          &endif         
        end.    
    &else    
        if  i-num-ped-exec-rpw <> 0 then do:
          &if "{&pagesize}" = "" &then
            output {&stream} 
                   to value(c-dir-spool-servid-exec + "~/" + {&tofile}) 
                   paged page-size 64
                   convert target "iso8859-1" {&append}.         
          &else         
            assign i-page-size-rel = integer("{&pagesize}").
            output {&stream} 
                   to value(c-dir-spool-servid-exec + "~/" + {&tofile}) 
                   paged page-size value(i-page-size-rel)
                   convert target "iso8859-1" {&append}.         
          &endif         
        end.        
        else do:
          &if "{&pagesize}" = "" &then
            output {&stream} 
                   to value({&tofile}) 
                   paged page-size 64 
                   convert target "iso8859-1" {&append}.         
          &else         
            assign i-page-size-rel = integer("{&pagesize}").
            output {&stream} 
                   to value({&tofile}) 
                   paged page-size value(i-page-size-rel)
                   convert target "iso8859-1" {&append}.         
          &endif         
        end.  
    &endif
end.

/* i-rpout */
 



        c-dir-dest = replace (c-dir-spool-servid-exec,"/","\") + "\".
        
        c-dir-dest = replace(c-dir-dest,"\\","\").
        

FORM
"*----------------------------- S E L E C A O --------------------------*" 
SKIP
SPACE(25)        "  DE                       ATE  " SKIP
SPACE(25)        " ----------              ---------- "   SKIP
SKIP(1)
"CODIGO ITEM:"      SPACE(5)  tt-param.it-codigo-ini     SPACE(13) tt-param.it-codigo-fim  SKIP
"CLIENTE:"          SPACE(5)  tt-param.cod-emitente-ini  SPACE(26) tt-param.cod-emitente-fim   SKIP
"FAMILIA:"          SPACE(5)  tt-param.fm-codigo-ini     SPACE(26) tt-param.fm-codigo-fim      SKIP
"FAMILIA COMERCIAL" SPACE(5)  tt-param.fm-cod-com-ini    SPACE(10) tt-param.fm-cod-com-fim     SKIP
"NOTA:"             SPACE(5)  tt-param.nr-nota-fis-ini   SPACE(13) tt-param.nr-nota-fis-fim    SKIP
"CF0:"              SPACE(5)  tt-param.nat-operacao-ini  SPACE(13) tt-param.nat-operacao-fim   SKIP
"DATA:"             SPACE(5)  tt-param.dt-emis-nota-ini  SPACE(13) tt-param.dt-emis-nota-fim   SKIP
"GRUPO CLIENTE:"    SPACE(5)  tt-param.cod-gr-cli-ini    SPACE(13) tt-param.cod-gr-cli-fim     SKIP
"GRUPO ESTOQUE:"    SPACE(5)  tt-param.ge-codigo-ini     SPACE(13) tt-param.ge-codigo-fim      SKIP
"CANAL VENDAS:"     SPACE(5)  tt-param.cod-canal-venda-ini SPACE(13) tt-param.cod-canal-venda-fim SKIP
"ESTABELECIMENTO:"  SPACE(5)  tt-param.cod-estabel-ini   SPACE(13) tt-param.cod-estabel-fim   SKIP
"ESTADO:"           SPACE(5)  tt-param.estado-ini        SPACE(13) tt-param.estado-fim        SKIP
" *-------------------------------------------------------------------* " 
SKIP
WITH no-box frame selecao width 500 NO-LABEL.

IF t-moeda THEN
   ASSIGN recebe = "REAL".
ELSE
   ASSIGN recebe = "DOLAR".

form header
    FILL("-",250) FORMAT "x(250)" skip
    param-global.grupo  "RELATORIO DE VENDAS EM " + recebe  AT 110 FORMAT "x(28)"
    FILL(" ",101) + "Pag.: " + string(page-number,"9999") FORMAT "x(111)" 
skip
    FILL(" ",106) + "PERIODO: " + string(dt-emis-nota-ini,"99/99/9999") + " A  " + string(dt-emis-nota-fim,"99/99/9999") FORMAT "x(245)" SKIP
    FILL("-",250) FORMAT "x(250)" SKIP(1)
with width 500 no-box frame f-cab page-top.

form header
  STRING(TODAY,"99/99/9999") + " - " + STRING(TIME,"hh:mm") + "h " + FILL("-",250) + " esft0012 - 2.00.00.001" FORMAT "X(250)"
  with width 500 no-box frame f-rod page-bottom.

IF l-item THEN
   tex-qtd-peso = "Qtd.".
ELSE
   tex-qtd-peso = "Peso".

IF l-imposto THEN
   ASSIGN impost    = "    Despesa      Enc. Financ.      "
          imp-linha = "  -----------   ---------------    ".
ELSE
    ASSIGN impost    = ""
           imp-linha = "".

FORM header
  "NF       SEQ CFO     Dt. Emissao Cliente         Item                     
         " + tex-qtd-peso +
  " UM           Vl. Total             ICMS              IPI           
COFINS              PIS      " + impost +
  "Vlr. Liquido       Vlr. Unitario" FORMAT "x(300)" SKIP
  "-------- --- ------- ----------- --------------- ----------------      
--------------- --     ---------------     ------------     ------------     
------------      -----------    " +
  imp-linha + "--------------      --------------" FORMAT "x(300)"
WITH WIDTH 500 PAGE-TOP FRAME f-tudo OVERLAY NO-BOX NO-LABELS.

FORM header
  FILL(" ",82) + tex-qtd-peso +
  "              Vl. Total             ICMS              IPI           
COFINS              PIS      " + impost +
  "Vlr. Liquido       Vlr. Unitario" FORMAT "x(300)" SKIP
  FILL(" ",71) + "---------------        ---------------     ------------    
  ------------     ------------      -----------    " +
      imp-linha + "--------------      --------------" FORMAT "x(300)"
WITH WIDTH 500 PAGE-TOP FRAME f-acum-tudo OVERLAY NO-BOX NO-LABELS.

/**************************************************************************************************************************************/

        
        
IF saida = 2 THEN DO:

   RUN pi-inicializar IN h-acomp (INPUT "Demonstrativo de Faturamento (Excel)").

/*   fullname = SEARCH("esp/planilha/esft0012.xlt") NO-ERROR.*/
   
   /* fullname = SEARCH("i:\ems204\ung\modelos\mod-esft0012a.xls") NO-ERROR. */
    
   fullname = SEARCH("modelos\mod-esft0012r.xlsx") NO-ERROR. 

   IF fullname = ? THEN DO:
      MESSAGE "N∆o foi poss°vel encontrar o arquivo modelo para o aplicativo Microsoft Excel."
            VIEW-AS ALERT-BOX ERROR.
      UNDO, RETURN.
   END.
   
        

     
     c-arq-dest = SESSION:TEMP-DIRECTORY + "esft0012" + string(today,"99-99-9999") + "-" + replace(string(time,"HH:MM:SS"),":","-") + ".xlsx".

        
        
     OS-COPY VALUE(fullname) VALUE(c-arq-dest).
   
     ASSIGN fullname = c-arq-dest.
     
   CREATE "Excel.Application" hExcel NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE ERROR-STATUS:NUM-MESSAGES
              "Ocorreu um ERRO durante a abertura do EXCEL." SKIP(1)
              "Deseja ver o ERRO ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE view-errs AS LOGICAL.
      IF view-errs THEN
         DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
            MESSAGE ERROR-STATUS:GET-NUMBER(i)
                    ERROR-STATUS:GET-MESSAGE(i).
         END.
      UNDO, RETURN.
   END.

   FILE-INFO:FILE-NAME = fullname.
   hExcel:workbooks:OPEN(FILE-INFO:FULL-PATHNAME,TRUE). 
   hExcel:sheets:ITEM(1).  
   hExcel:sheets:ITEM(1):activate.
   hExcel:APPLICATION:DisplayAlerts = FALSE.
   
   /*assign c-planilha  = hExcel:Workbooks:OPEN(fullname) NO-ERROR.

   /*hExcel:Workbooks:OPEN(fullname) NO-ERROR.*/
   hExcel:VISIBLE = FALSE NO-ERROR.

   hExcel:Range("d3"):select.*/
   /* hExcel:Shapes:AddPicture(c-modelo-logo,1,0,20,20,120,35). */
   /*hExcel:Sheets:Item(1):Shapes:AddPicture (c-modelo-logo,1,0,20,20,94,30).


   hExcel:Sheets:Item(1):select.*/

   iLin     = 16.

   hExcel:Range(trim(entry(10,aCel) + string(11))):value = "EM " + recebe.
   hExcel:Range(trim(entry(10,aCel) + string(12))):value = "PERIODO: " + STRING(dt-emis-nota-ini,"99/99/9999") + " A " + STRING(dt-emis-nota-fim,"99/99/9999").

   /*- CABECALHO 1-------------------------------------------------------------------------------*/
   IF r-tipo THEN DO:
      DO i = 4 TO 08:
         hExcel:Range(trim(entry(i,aCel) + string(13))):select no-error.
         hExcel:Selection:Borders(07):LineStyle  = 0 NO-ERROR.
         hExcel:Selection:Borders(07):Weight     = 0 NO-ERROR.
         hExcel:Selection:Borders(10):LineStyle  = 0 NO-ERROR.
         hExcel:Selection:Borders(10):Weight     = 0 NO-ERROR.
      END.
      hExcel:Range(trim(entry(4,aCel) + string(13)) + ":" + trim(entry(12,aCel) + string(13))):select no-error.
      hExcel:Selection:ClearContents.
   END.

   hExcel:Range(trim(entry(10,aCel) + string(13))):value = (IF l-item THEN "QUANTIDADE" ELSE "PESO") NO-ERROR.

   ASSIGN iColimp = 17.
   IF l-imposto THEN DO:
      hExcel:Range(trim(entry(iColimp,aCel)) + ":" + trim(entry(iColimp + 1,aCel))):select no-error.
      hExcel:Selection:INSERT.
      hExcel:Range(trim(entry(iColimp,aCel) + string(13)) + ":" + trim(entry(iColimp,aCel) + string(14))):SELECT.
      hExcel:Selection:MergeCells = TRUE.
      hExcel:Range(trim(entry(iColimp,aCel) + string(13))):VALUE = "DESPESA".
      hExcel:Columns(trim(entry(iColimp,aCel) + ":" + entry(iColimp,aCel))):select no-error.
      hExcel:Selection:ColumnWidth = 14 no-error.

      ASSIGN iColimp = iColimp + 1.

      hExcel:Range(trim(entry(iColimp,aCel) + string(13))):value = "ENCARGOS".
      hExcel:Range(trim(entry(iColimp,aCel) + string(14))):value = "FINANCEIROS".
      hExcel:Columns(trim(entry(iColimp,aCel) + ":" + entry(iColimp,aCel))):select no-error.
      hExcel:Selection:ColumnWidth = 14 no-error.
      hExcel:Rows(STRING(14) + ":" + STRING(14)):EntireRow:AutoFit.

      ASSIGN iColimp = iColimp + 1.
   END.
END.
ELSE
   RUN pi-inicializar IN h-acomp (INPUT "Demonstrativo de Faturamento").

 

 
DO dt-atu = dt-emis-nota-ini TO dt-emis-nota-fim:

    FOR EACH estabelec WHERE 
        estabelec.cod-estabel >= cod-estabel-ini and
        estabelec.cod-estabel <= cod-estabel-fim 
        NO-LOCK.

        RUN utp\esut001.p (INPUT estabelec.cod-estabel,INPUT "esft0012,nomsg").
        IF RETURN-VALUE = "nok" THEN NEXT.

        FOR EACH nota-fiscal WHERE nota-fiscal.dt-emis-nota = dt-atu AND
                               nota-fiscal.cod-estabel  = estabelec.cod-estabel AND
                               nota-fiscal.nat-operacao >= "5" AND   
                               nota-fiscal.cod-emitente >= cod-emitente-ini AND   
                               nota-fiscal.cod-emitente <= cod-emitente-fim NO-LOCK USE-INDEX nfftrm-20.
        
            RUN pi-acompanhar IN h-acomp (INPUT "NFS..: " + nota-fiscal.nr-nota-fis). 
        
            FIND FIRST tt-notas WHERE
                tt-notas.cod-estabel = nota-fiscal.cod-estabel AND
                tt-notas.serie       = nota-fiscal.serie       AND
                tt-notas.nr-nota-fis = nota-fiscal.nr-nota-fis
                NO-ERROR.
        
            IF NOT AVAIL tt-notas THEN DO:
                CREATE tt-notas.
                ASSIGN tt-notas.cod-estabel = nota-fiscal.cod-estabel
                       tt-notas.serie       = nota-fiscal.serie      
                       tt-notas.nr-nota-fis = nota-fiscal.nr-nota-fis.
            END.
        
            ASSIGN tt-notas.identific = 0.
        END.

        FOR EACH devol-cli NO-LOCK WHERE
            devol-cli.cod-emitente >= cod-emitente-ini AND   
            devol-cli.cod-emitente <= cod-emitente-fim AND
            devol-cli.cod-estabel  = estabelec.cod-estabel AND
            devol-cli.dt-devol     = dt-atu USE-INDEX ch-estabel.
            
            RUN pi-acompanhar IN h-acomp (INPUT "DEVOLUÄÂES: " + devol-cli.nr-nota-fis). 
            
            FIND FIRST nota-fiscal WHERE
                nota-fiscal.cod-estabel = devol-cli.cod-estabel AND
                nota-fiscal.serie       = devol-cli.serie       AND
                nota-fiscal.nr-nota-fis = devol-cli.nr-nota-fis
                NO-LOCK NO-ERROR.
        
            IF NOT AVAIL nota-fiscal THEN NEXT.
            
            FIND FIRST tt-notas WHERE
                tt-notas.cod-estabel = nota-fiscal.cod-estabel AND
                tt-notas.serie       = nota-fiscal.serie       AND
                tt-notas.nr-nota-fis = nota-fiscal.nr-nota-fis
                NO-ERROR.
        
            IF NOT AVAIL tt-notas THEN DO:
                CREATE tt-notas.
                ASSIGN tt-notas.cod-estabel = nota-fiscal.cod-estabel
                       tt-notas.serie       = nota-fiscal.serie      
                       tt-notas.nr-nota-fis = nota-fiscal.nr-nota-fis
                       tt-notas.identific = 9.
            END.
        
        END. 
    END.
END.
FOR EACH tt-notas
    WHERE tt-notas.nr-nota-fis  >= nr-nota-fis-ini 
    AND   tt-notas.nr-nota-fis  <= nr-nota-fis-fim NO-LOCK,
    EACH nota-fiscal 
    WHERE nota-fiscal.cod-estabel        = tt-notas.cod-estabel 
    AND   nota-fiscal.serie              = tt-notas.serie       
    AND   nota-fiscal.nr-nota-fis        = tt-notas.nr-nota-fis 
    AND   nota-fiscal.estado            >= estado-ini 
    AND   nota-fiscal.estado            <= estado-fim 
    AND   nota-fiscal.cod-canal-venda   >= cod-canal-venda-ini 
    AND   nota-fiscal.cod-canal-venda   <= cod-canal-venda-fim 
    AND   nota-fiscal.cod-emitente      >= cod-emitente-ini 
    AND   nota-fiscal.cod-emitente      <= cod-emitente-fim 
    /* AND   nota-fiscal.ind-sit-nota      >= 2 */
    AND   nota-fiscal.dt-cancela         = ? NO-LOCK,

    EACH it-nota-fisc OF nota-fiscal
    WHERE it-nota-fisc.nat-operacao >= nat-operacao-ini 
    AND   it-nota-fisc.nat-operacao <= nat-operacao-fim 
/*     AND   NOT (SUBSTRING(it-nota-fisc.nat-operacao,1,3) = "515"  */
/*     OR         SUBSTRING(it-nota-fisc.nat-operacao,1,3) = "615"  */
/*     OR         SUBSTRING(it-nota-fisc.nat-operacao,1,3) = "715") */
    NO-LOCK,
    FIRST ITEM OF it-nota-fisc WHERE 
        item.it-codigo    >= it-codigo-ini  AND 
        item.it-codigo    <= it-codigo-fim  AND 
        item.ge-codigo    >= ge-codigo-ini  AND 
        item.ge-codigo    <= ge-codigo-fim  AND 
        item.fm-codigo    >= fm-codigo-ini  AND 
        item.fm-codigo    <= fm-codigo-fim  AND 
        item.fm-cod-com   >= fm-cod-com-ini AND 
        item.fm-cod-com   <= fm-cod-com-fim NO-LOCK,
    FIRST emitente   WHERE 
        emitente.cod-emitente  = nota-fiscal.cod-emitente AND
        emitente.nome-mic-reg >= nome-mic-reg-ini         AND
        emitente.nome-mic-reg <= nome-mic-reg-fim         AND
        emitente.cod-gr-cli   >= cod-gr-cli-ini           AND
        emitente.cod-gr-cli   <= cod-gr-cli-fim           NO-LOCK,
    FIRST familia where     familia.fm-codigo     = item.fm-codigo NO-LOCK,
    FIRST b-natur-oper WHERE  
          b-natur-oper.nat-operacao = it-nota-fisc.nat-operacao AND
          IF tt-param.l-nota THEN b-natur-oper.emite-duplic ELSE TRUE
    BREAK BY (IF s-desc THEN item.desc-item
             ELSE IF rs-classif = 14 THEN item.it-codigo
             ELSE IF rs-classif = 10 THEN (IF emitente.pais = "brasil" THEN "BRA" ELSE "EXT")
             ELSE item.it-codigo)
             BY (IF rs-classif = 14      THEN it-nota-fisc.nat-operacao
             ELSE IF rs-classif = 10 THEN item.fm-codigo
             ELSE it-nota-fisc.nat-operacao)
             BY it-nota-fisc.dt-emis-nota:

    ASSIGN v-icms   = 0                
           v-ipi    = 0
           v-vl-tot = 0.


    FIND FIRST classif-fisc WHERE classif-fisc.class-fiscal = item.class-fiscal NO-LOCK NO-ERROR.

    IF  tt-param.l-nota AND tt-param.l-consig THEN
        RUN pi-consignacao.
   
    /*IF  FIRST(IF s-desc THEN item.desc-item
            ELSE IF rs-classif = 14 THEN ITEM.it-codigo
            ELSE IF rs-classif = 10 THEN (IF emitente.pais = "brasil" THEN "BRA" ELSE "EXT")
            ELSE ITEM.it-codigo) AND saida = 2 THEN DO:

        FIND estabelec WHERE 
            estabelec.cod-estabel = nota-fiscal.cod-estabel NO-LOCK NO-ERROR NO-WAIT.
        FIND empresa   WHERE 
            empresa.ep-codigo     = estabelec.ep-codigo NO-LOCK NO-ERROR NO-WAIT.
      
        ASSIGN hExcel:Range(trim(entry(7,aCel) + string(4))):VALUE = TRIM(empresa.razao-social)
               hExcel:Range(trim(entry(7,aCel) + string(5))):VALUE = TRIM(estabelec.endereco) + " - " + TRIM(estabelec.bairro)
               hExcel:Range(trim(entry(7,aCel) + string(6))):VALUE = TRIM(estabelec.cidade) + "/" + TRIM(estabelec.estado) + "/" + 
               TRIM(estabelec.pais)
               hExcel:Range(trim(entry(7,aCel) + string(7))):VALUE = "CEP - " + STRING(STRING(estabelec.cep),"99.999-999") NO-ERROR.
    END.*/

    IF  FIRST-OF(IF s-desc THEN item.desc-item
               ELSE IF rs-classif = 14 THEN ITEM.it-codigo
               ELSE IF rs-classif = 10 THEN (IF emitente.pais = "brasil" THEN "BRA" ELSE "EXT")
               ELSE ITEM.it-codigo) AND saida <> 2 THEN DO:
        VIEW FRAME f-cab.
        VIEW FRAME f-rod.
        IF  r-tipo THEN
            VIEW FRAME f-acum-tudo.
        ELSE
            VIEW FRAME f-tudo.
    END.

    IF SUBSTRING(STRING(ilin,"99999999"),8,1) = "0" THEN RUN pi-acompanhar IN h-acomp (INPUT "Item...: " + item.it-codigo +  " Linha: " + STRING(ilin,"99999999")). 
    /*RUN pi-acompanhar IN h-acomp (INPUT "Item...: " + item.it-codigo.*/

    /*TRAZ TODAS AS NOTAS INCLUSIVE ANTECIPADAS*/
    IF  n-fatur THEN
        find natur-oper where natur-oper.nat-operacao = it-nota-fisc.nat-operacao  no-lock no-error.

    /*NAO TRAZ AS NOTAS ANTECIPADAS*/
    IF  n-fatur = no THEN
        find natur-oper where 
            natur-oper.nat-operacao = it-nota-fisc.nat-operacao and
            natur-oper.ind-entfut   = NO no-lock no-error.

    /*SO TRAZ AS NOTAS QUE SATISFAÄAM ESTA CONDIÄAO, A DEPENDER DA SELECAO DO USUARIO, SE CONSIDERA NOTAS ANTECIPADAS OU NAO*/

    IF  AVAIL natur-oper THEN DO:
        
        /*VERIFICA A COTACAO DO DOLAR*/

        find cotacao where 
            cotacao.mo-codigo = 1 and
            string(year(nota-fiscal.dt-emis-nota),"9999") = substring(cotacao.ano-periodo,1,4) AND
            string(month(nota-fiscal.dt-emis-nota),"99")  = substring(cotacao.ano-periodo,5,2) NO-LOCK no-error.

        if  not avail cotacao then
            assign de-cotacao = 1.
        else
            assign de-cotacao = cotacao.cotacao[day(nota-fiscal.dt-emis-nota)].

        IF  it-nota-fisc.peso-liq-fat = 0 THEN DO:
            IF (ITEM.peso-liquido = 0) THEN
                ASSIGN auxpeso = 1.
            IF (ITEM.peso-liquido > 0) THEN
                ASSIGN auxpeso = ITEM.peso-liquido * it-nota-fisc.qt-faturada[1].
        END.

        IF  it-nota-fisc.peso-liq-fat <> 0 THEN
            ASSIGN auxpeso = it-nota-fisc.peso-liq-fat.

        /*SE CONSIDERA DEVOLUCOES*/
        IF  n-devol THEN DO:

            /*VERIFICA SE TEM NOTAS DEVOLVIDAS*/
            
            
            ASSIGN tt-aux-peso-liq-fat-dev    = 0
                   tt-aux-qt-faturada-dev     = 0
                   tt-aux-vl-tot-item-dev     = 0
                   tt-aux-vl-icms-it-dev      = 0
                   tt-aux-vl-ipi-it-dev       = 0
                   tt-v-cofins-dev            = 0
                   tt-v-pis-dev               = 0
                   tt-encargos-financ-dev     = 0
                   tt-valor-liquido-dev       = 0
                   tt-aux-vl-despes-it-dev    = 0.
            

            FOR EACH devol-cli 
                where devol-cli.it-codigo    = it-nota-fisc.it-codigo   
                AND   devol-cli.nr-nota-fis  = it-nota-fisc.nr-nota-fis 
                and   devol-cli.serie        = it-nota-fisc.serie       
                and   devol-cli.cod-estabel  = it-nota-fisc.cod-estabel 
                AND   devol-cli.nr-sequencia = it-nota-fisc.nr-seq-fat  
                AND   devol-cli.cod-emitente = nota-fiscal.cod-emitente NO-LOCK .

                IF  t-devoluc = NO OR (devol-cli.dt-devol   >= dt-emis-nota-ini and devol-cli.dt-devol    <= dt-emis-nota-fim) THEN DO:
        
                    ASSIGN aux-qt-faturada-dev  = -1 * (devol-cli.qt-devolvida)
                           aux-peso-liq-fat-dev = -1 * ((auxpeso * devol-cli.qt-devolvida) / it-nota-fisc.qt-faturada[1])
                           aux-vl-tot-item-dev  = -1 * (devol-cli.vl-devol).


                     IF  (nota-fiscal.ind-tip-nota = 3) THEN
                    ASSIGN aux-qt-faturada-dev  = 0
                           aux-peso-liq-fat-dev = 0.
                           

        
                    IF  it-nota-fisc.cd-trib-ipi <> 2 AND it-nota-fisc.cd-trib-ipi <> 3 THEN
                        ASSIGN aux-vl-ipi-it-dev = -1 * (it-nota-fisc.vl-ipi-it  * devol-cli.vl-devol / it-nota-fisc.vl-tot-item).
                    ELSE
                        ASSIGN aux-vl-ipi-it-dev = 0.
        
                    IF  it-nota-fisc.cd-trib-icm <> 2 AND it-nota-fisc.cd-trib-icm <> 3 THEN
                        ASSIGN aux-vl-icms-it-dev = -1 * (it-nota-fisc.vl-icms-it * devol-cli.vl-devol / it-nota-fisc.vl-tot-item).
        	      ELSE
        	          ASSIGN aux-vl-icms-it-dev = 0.
        
                    if  devol-cli.vl-devol = it-nota-fisc.vl-tot-item then
                        assign aux-vl-merc-liq-dev = - it-nota-fisc.vl-merc-liq.
                    else
                        ASSIGN aux-vl-merc-liq-dev  = -1 * (it-nota-fisc.vl-merc-liq * devol-cli.vl-devol / it-nota-fisc.vl-tot-item).
        
                    IF  it-nota-fisc.dt-emis-nota < 11/01/02 THEN
                        assign v-aliq-pis = dec(SUBSTRING(natur-oper.char-1, 77, 4))
                               v-aliq-cof = dec(SUBSTRING(natur-oper.char-1, 82, 4)).
                    ELSE
                        ASSIGN v-aliq-pis = DEC(SUBSTRING(it-nota-fisc.char-2, 76, 5))
                               v-aliq-cof = DEC(SUBSTRING(it-nota-fisc.char-2, 81, 5)).
        
                    assign v-pis-dev    = ((aux-vl-tot-item-dev - aux-vl-ipi-it-dev) * v-aliq-pis) / 100
                           v-cofins-dev = ((aux-vl-tot-item-dev - aux-vl-ipi-it-dev) * v-aliq-cof) / 100.
        
                    IF  nota-fiscal.nr-ind-finan = 0 OR l-imposto = NO THEN
                        ASSIGN encargos-financ-dev  = 0
                               aux-vl-despes-it-dev = 0.
                    ELSE
                        ASSIGN encargos-financ-dev = aux-vl-merc-liq-dev - 
                         (aux-vl-merc-liq-dev / nota-fiscal.tab-ind-fin)
                              aux-vl-despes-it-dev = -1 * (it-nota-fisc.vl-despes-it 
                              - (it-nota-fisc.vl-despes-it * devol-cli.vl-devol / 
                              it-nota-fisc.vl-tot-item)).
        
                    ASSIGN valor-liquido-dev = aux-vl-tot-item-dev - ( 
                      aux-vl-icms-it-dev + aux-vl-ipi-it-dev + v-pis-dev + v-cofins-dev + 
                      aux-vl-despes-it-dev + encargos-financ-dev).
        
                    IF  t-total THEN
                        ASSIGN aux-campo  = ITEM.desc-item
                               aux-campo1 = it-nota-fisc.it-codigo.
                    ELSE
                        ASSIGN aux-campo  = it-nota-fisc.it-codigo  
                               aux-campo1 = ITEM.desc-item.

                    ASSIGN aux-qt-dev = (IF l-item THEN aux-qt-faturada-dev ELSE aux-peso-liq-fat-dev) * -1.
        
                       IF SUBSTRING(STRING(ilin,"99999999"),8,1) = "0" THEN       RUN pi-acompanhar IN h-acomp (INPUT "Item...: " + item.it-codigo +  " Linha: " + STRING(ilin,"99999999")). 

                    /* SE FOR POR QUANTIDADE E FOR DETALHADO E FOR REAL */
                    IF  r-tipo = NO THEN DO:
                        IF  saida = 2 THEN DO:
                            ASSIGN hExcel:Range(trim(entry(04,aCel) + string(ilin))):value = devol-cli.nro-docto
                                   hExcel:Range(trim(entry(05,aCel) + string(ilin))):value = it-nota-fisc.nr-seq-fat
                                   hExcel:Range(trim(entry(06,aCel) + string(ilin))):value = devol-cli.nat-operacao
                                   hExcel:Range(trim(entry(07,aCel) + string(ilin))):value = devol-cli.dt-devol
                                   hExcel:Range(trim(entry(08,aCel) + string(ilin))):value = nota-fiscal.nome-ab-cli
                                   hExcel:Range(trim(entry(09,aCel) + string(ilin))):value = aux-campo
                                   hExcel:Range(trim(entry(10,aCel) + string(ilin))):value = (IF l-item THEN aux-qt-faturada-dev ELSE aux-peso-liq-fat-dev)
                                   hExcel:Range(trim(entry(11,aCel) + string(ilin))):value = (IF l-item THEN it-nota-fisc.un-fatur[1] ELSE "Kg")
                                   hExcel:Range(trim(entry(12,aCel) + string(ilin))):value = (IF t-moeda THEN aux-vl-tot-item-dev ELSE (aux-vl-tot-item-dev / de-cotacao))
                                   hExcel:Range(trim(entry(13,aCel) + string(ilin))):value = (IF t-moeda THEN aux-vl-icms-it-dev  ELSE (aux-vl-icms-it-dev / de-cotacao))
                                   hExcel:Range(trim(entry(14,aCel) + string(ilin))):value = (IF t-moeda THEN aux-vl-ipi-it-dev   ELSE (aux-vl-ipi-it-dev / de-cotacao))
                                   hExcel:Range(trim(entry(15,aCel) + string(ilin))):value = (IF t-moeda THEN v-cofins-dev        ELSE (v-cofins-dev / de-cotacao))
                                   hExcel:Range(trim(entry(16,aCel) + string(ilin))):value = (IF t-moeda THEN v-pis-dev           ELSE (v-pis-dev / de-cotacao)).
                                   ASSIGN iColimp = 17.
                            IF  l-imposto THEN
                                ASSIGN hExcel:Range(trim(entry(iColimp,aCel) + string(ilin))):value = (IF t-moeda THEN aux-vl-despes-it-dev ELSE 
                                           (aux-vl-despes-it-dev / de-cotacao))
                                       hExcel:Range(trim(entry(iColimp + 1,aCel)+ string(ilin))):value = (IF t-moeda THEN encargos-financ-dev ELSE 
                                           (encargos-financ-dev / de-cotacao))
                                       iColimp = 19.
                            ASSIGN hExcel:Range(trim(entry(iColimp,aCel) + string(ilin))):value = (IF t-moeda THEN valor-liquido-dev ELSE 
                                        (valor-liquido-dev / de-cotacao))
                                   hExcel:Range(trim(entry(iColimp + 1,aCel) + string(ilin))):value = (IF aux-qt-faturada-dev > 0 THEN ( (IF t-moeda THEN (-1 * (valor-liquido-dev / (IF 
                                        l-item THEN aux-qt-faturada-dev ELSE aux-peso-liq-fat-dev))) ELSE (-1 * ((valor-liquido-dev / (IF l-item THEN aux-qt-faturada-dev ELSE 
                                        aux-peso-liq-fat-dev)) / de-cotacao)))) ELSE 0).
        
                            hExcel:Range(trim(entry(iColimp + 2,aCel) + string(ilin))):value = nota-fiscal.cod-estabel. 
                            hExcel:Range(trim(entry(iColimp + 3,aCel) + string(ilin))):value = it-nota-fisc.nr-pedcli.
                            hExcel:Range(trim(entry(iColimp + 4,aCel) + string(ilin))):value = item.fm-codigo.
                            hExcel:Range(trim(entry(iColimp + 5,aCel) + string(ilin))):value = item.fm-cod-com.
                            
                            hExcel:Range(trim(entry(iColimp + 6,aCel) + string(ilin))):value = it-nota-fisc.nr-nota-fis. 
                            hExcel:Range(trim(entry(iColimp + 7,aCel) + string(ilin))):value = IF nota-fiscal.cidade-cif <> "" THEN "CIF" ELSE "FOB". 
                            hExcel:Range(trim(entry(iColimp + 8,aCel) + string(ilin))):value = nota-fiscal.nome-transp. 
                            hExcel:Range(trim(entry(iColimp + 9,aCel) + string(ilin))):value = nota-fiscal.cidade. 
                            hExcel:Range(trim(entry(iColimp + 10,aCel) + string(ilin))):value = nota-fiscal.estado.
                          /*hExcel:Range(trim(entry(iColimp + 11,aCel) + string(ilin))):value = VENCTO
                            hExcel:Range(trim(entry(iColimp + 12,aCel) + string(ilin))):value = VENCTO MêDIO */
                            hExcel:Range(trim(entry(iColimp + 13,aCel) + string(ilin))):value = nota-fiscal.cdd-embarq.
                            hExcel:Range(trim(entry(iColimp + 14,aCel) + string(ilin))):value = nota-fiscal.cod-emitente.
                            hExcel:Range(trim(entry(iColimp + 15,aCel) + string(ilin))):value = aux-campo1.
            
                            FIND FIRST movto-estoq WHERE 
                                    movto-estoq.serie-docto  = it-nota-fisc.serie        AND
                                    movto-estoq.sequen-nf    = it-nota-fisc.nr-seq-fat   AND
                                    movto-estoq.nro-docto    = it-nota-fisc.nr-nota-fis  AND
                                    movto-estoq.nat-operacao = it-nota-fisc.nat-operacao AND
                                    movto-estoq.it-codigo    = it-nota-fisc.it-codigo    AND
                                    movto-estoq.cod-emitente = nota-fiscal.cod-emitente  NO-LOCK NO-ERROR.

                            FIND FIRST     b-movto-estoq WHERE 
                                b-movto-estoq.serie-docto  = devol-cli.serie-docto       AND                                  
                                b-movto-estoq.nro-docto    = devol-cli.nro-docto   AND
                                b-movto-estoq.nat-operacao = devol-cli.nat-operacao AND
                                b-movto-estoq.it-codigo    = devol-cli.it-codigo     AND
                                b-movto-estoq.cod-emitente = devol-cli.cod-emitente  NO-LOCK NO-ERROR.
                 
                                                  
                            ASSIGN hExcel:Range(trim(entry(iColimp + 16,aCel) + string(ilin))):value =  IF NOT AVAIL movto-estoq THEN 0 ELSE
                                                     (aux-qt-dev * (movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-MOB-m[1]) / movto-estoq.quantidade) * -1.  /* edson 26/7/2012 custo para Polo*/
                            ASSIGN hExcel:Range(trim(entry(iColimp + 17,aCel) + string(ilin))):value =  IF NOT AVAIL movto-estoq THEN 0 ELSE
                                                     ((aux-qt-dev * movto-estoq.valor-mat-m[1]) / movto-estoq.quantidade) * -1.  /* edson 04/7/2013 custo para Polo*/

                            ASSIGN hExcel:Range(trim(entry(iColimp + 18,aCel) + string(ilin))):value =  IF NOT AVAIL movto-estoq THEN 0 ELSE
                                                     ((aux-qt-dev * movto-estoq.valor-MOB-m[1]) / movto-estoq.quantidade) * -1.  /* edson 04/7/2013 custo para Polo*/
                            ASSIGN hExcel:Range(trim(entry(iColimp + 19,aCel) + string(ilin))):value =  IF NOT AVAIL movto-estoq THEN 0 ELSE
                                                     ((aux-qt-dev * movto-estoq.valor-GGF-m[1]) / movto-estoq.quantidade) * -1.  /* edson 04/7/2013 custo para Polo*/

                            ASSIGN hExcel:Range(trim(entry(iColimp + 20,aCel) + string(ilin))):value =  ITEM.class-fiscal
                                   hExcel:Range(trim(entry(iColimp + 21,aCel) + string(ilin))):value =  IF AVAIL classif-fisc THEN classif-fisc.descricao ELSE "".  /* rodrigo 03/10/2013 marco longue*/

                            ASSIGN hExcel:Range(trim(entry(iColimp + 22,aCel) + string(ilin))):value =  IF AVAIL b-movto-estoq THEN b-movto-estoq.ct-codigo ELSE IF AVAIL movto-estoq THEN movto-estoq.ct-codigo ELSE "". /* rodrigo 09/10/13 - Ricardo */
                            ASSIGN hExcel:Range(trim(entry(iColimp + 23,aCel) + string(ilin))):value =  IF AVAIL b-movto-estoq THEN b-movto-estoq.sc-codigo ELSE IF AVAIL movto-estoq THEN movto-estoq.sc-codigo ELSE "". /* rodrigo 09/10/13 - Ricardo */
                            c-conta-receita = "".
                            
                            RUN pi-conta-receita-DEV .
                            ASSIGN hExcel:Range(trim(entry(iColimp + 24,aCel) + string(ilin))):value =  c-conta-receita. /* edson 21/03/2014 - Ricardo */
                            ASSIGN hExcel:Range(trim(entry(iColimp + 25,aCel) + string(ilin))):value =  c-sc-receita. /* edson 21/03/2014 - Ricardo */

                            /*CNPJ Cliente - 31/05/2016*/
                            ASSIGN hExcel:Range(trim(entry(iColimp + 26,aCel) + string(ilin))):value = IF emitente.natureza = 1 THEN string(emitente.cgc,param-global.formato-id-pessoal)
                                                                                                       ELSE IF emitente.natureza = 2 THEN string(emitente.cgc,param-global.formato-id-federal)
                                                                                                       ELSE emitente.cgc.

                            iLin = iLin + 1.
                            hExcel:Rows(iLin):select no-error.
                            hExcel:Selection:INSERT.
                            hExcel:Range(trim(entry(4,aCel) + string(iLin)) + ":" + trim(entry(iColimp + 1,aCel) + string(iLin))):select no-error.
                            hExcel:Selection:ClearContents.
                            hExcel:Selection:Interior:ColorIndex = 0.
                        END. /* saida = 2*/
                        ELSE
                            IF  l-item THEN DO:
                                PUT UNFORMATTED devol-cli.nro-docto     FORMAT "x(10)"
                                    STRING(it-nota-fisc.nr-seq-fat)     FORMAT "x(3)"
                                    devol-cli.nat-operacao              FORMAT "x(8)"
                                    devol-cli.dt-devol                  FORMAT "99/99/9999"
                                    nota-fiscal.nome-ab-cli      AT 34  format "x(16)"
                                    aux-campo                           FORMAT "x(20)"
                                    aux-qt-faturada-dev          AT 71  format "->>>,>>>,>>9.999"
                                    it-nota-fisc.un-fatur[1]     AT 88  FORMAT "x(4)".
                    
                                IF  t-moeda THEN DO:
                                    PUT UNFORMATTED aux-vl-tot-item-dev       AT 092 format "->>,>>>,>>>,>>9.99"
                                        aux-vl-icms-it-dev                    AT 110 format "->,>>>,>>>,>>9.99"
                                        aux-vl-ipi-it-dev                     AT 127 FORMAT "->,>>>,>>>,>>9.99"
                                        v-cofins-dev                          AT 144 format "->,>>>,>>>,>>9.99"
                                        v-pis-dev                             AT 161 format "->,>>>,>>>,>>9.99".
                    
                                    IF  tt-param.l-imposto THEN
                                        PUT UNFORMATTED aux-vl-despes-it-dev      AT 178 format "->,>>>,>>>,>>9.99"
                                            encargos-financ-dev                   AT 196 format "->,>>>,>>>,>>9.99"
                                            valor-liquido-dev                     AT 213 format "->,>>>>,>>>,>>9.99"
                                                  (-1 * (valor-liquido-dev / aux-qt-faturada-dev)) AT 231 format "->>,>>>,>>>,>>9.9999" SKIP.
                                    ELSE
                                        PUT UNFORMATTED valor-liquido-dev    AT 178 format "->>,>>>,>>>,>>9.99"
                                                  (-1 * (valor-liquido-dev / aux-qt-faturada-dev)) AT 196 format "->>,>>>,>>>,>>9.9999" SKIP.
                                END.
                                ELSE DO:
                                    PUT UNFORMATTED (aux-vl-tot-item-dev / de-cotacao)  AT 092  format "->>,>>>,>>>,>>9.99"
                                        (aux-vl-icms-it-dev / de-cotacao)               AT 110  format "->,>>>,>>>,>>9.99"
                                        (aux-vl-ipi-it-dev / de-cotacao)                AT 127  format "->,>>>,>>>,>>9.99"
                                        (v-cofins-dev  / de-cotacao)                    AT 144  format "->,>>>,>>>,>>9.99"
                                        (v-pis-dev  / de-cotacao)                       AT 161  format "->,>>>,>>>,>>9.99".
                    
                                    IF  tt-param.l-imposto THEN
                                        PUT UNFORMATTED (aux-vl-despes-it-dev  / de-cotacao)  AT 178 format "->,>>>,>>>,>>9.99"
                                            (encargos-financ-dev / de-cotacao)                AT 196 format "->,>>>,>>>,>>9.99"
                                            (valor-liquido-dev / de-cotacao)                  AT 213 format "->>,>>>,>>>,>>9.99"
                                            (-1 * (valor-liquido-dev / aux-qt-faturada-dev) / de-cotacao) AT 231 format "->>,>>>,>>>,>>9.9999" 
                                            SKIP.
                                    ELSE
                                        PUT UNFORMATTED (valor-liquido-dev / de-cotacao)    AT 178 format "->>,>>>,>>>,>>9.99"
                                            (-1 * (valor-liquido-dev / aux-qt-faturada-dev) / de-cotacao) AT 196 format "->>,>>>,>>>,>>9.9999" 
                                            SKIP.
                                END.
                            END. /*L-ITEM = TRUE*/
                            ELSE DO:   /*IF FOR POR PESO e DETALHADO E FOR REAL*/    
                                PUT UNFORMATTED 
                                    devol-cli.nro-docto               FORMAT "x(10)"
                                    string(it-nota-fisc.nr-seq-fat)   FORMAT "x(3)"
                                    devol-cli.nat-operacao            FORMAT "x(8)"
                                    devol-cli.dt-devol                FORMAT "99/99/9999"
                                    nota-fiscal.nome-ab-cli           AT 34  format "x(16)"
                                    aux-campo                         FORMAT "x(20)"
                                    aux-peso-liq-fat-dev              AT 71  format "->>>,>>>,>>9.999"
                                    "Kg"                              AT 88  FORMAT "x(4)".
                
                                IF  t-moeda THEN DO:
                                    PUT UNFORMATTED 
                                        aux-vl-merc-liq-dev       AT 092  format "->>,>>>,>>>,>>9.99"
                                        aux-vl-icms-it-dev        AT 110  format "->,>>>,>>>,>>9.99"
                                        aux-vl-ipi-it-dev         AT 127  format "->,>>>,>>>,>>9.99"
                                        v-cofins-dev              AT 144  format "->,>>>,>>>,>>9.99"
                                        v-pis-dev                 AT 161  format "->,>>>,>>>,>>9.99".
            
                                    IF  tt-param.l-imposto THEN
                                        PUT UNFORMATTED 
                                            aux-vl-despes-it-dev        AT 178 format "->,>>>,>>>,>>9.99"
                                            encargos-financ-dev         AT 196 format "->,>>>,>>>,>>9.99"
                                            valor-liquido-dev           AT 213 format "->>,>>>,>>>,>>9.99"
                                            (-1 * valor-liquido-dev / 
                                            aux-peso-liq-fat-dev)    AT 231 format "->>,>>>,>>>,>>9.9999".
                                    ELSE
                                        PUT UNFORMATTED 
                                            valor-liquido-dev               AT 178 format "->>,>>>,>>>,>>9.99"
                                            (-1 * valor-liquido-dev / 
                                            aux-peso-liq-fat-dev)           AT 196 format "->>,>>>,>>>,>>9.9999".
                                END.
                                ELSE DO:
                                        PUT UNFORMATTED 
                                            (aux-vl-merc-liq-dev  / de-cotacao) AT 092  format "->>,>>>,>>>,>>9.99"
                                            (aux-vl-icms-it-dev  / de-cotacao)  AT 110  format "->,>>>,>>>,>>9.99"
                                            (aux-vl-ipi-it-dev  / de-cotacao)   AT 127  format "->,>>>,>>>,>>9.99"
                                            (v-cofins-dev  / de-cotacao)        AT 144  format "->,>>>,>>>,>>9.99"
                                            (v-pis-dev / de-cotacao)            AT 161  format "->,>>>,>>>,>>9.99".
            
                                        IF  tt-param.l-imposto THEN
                                            PUT UNFORMATTED 
                                                (aux-vl-despes-it-dev / de-cotacao) AT 178 format "->,>>>,>>>,>>9.99"
                                                (encargos-financ-dev / de-cotacao)  AT 196 format "->,>>>,>>>,>>9.99"
                                                (valor-liquido-dev  / de-cotacao)   AT 213 format "->>,>>>,>>>,>>9.99"
                                                (-1 * (valor-liquido-dev / 
                                                aux-peso-liq-fat-dev) / de-cotacao) AT 231 format "->>,>>>,>>>,>>9.9999".
                                        ELSE
                                            PUT UNFORMATTED     
                                                (valor-liquido-dev  / de-cotacao)   AT 178 format "->>,>>>,>>>,>>9.99"
                                                (-1 * (valor-liquido-dev / 
                                                aux-peso-liq-fat-dev) / de-cotacao) AT 196 format "->>,>>>,>>>,>>9.9999".
                                END.
                            END. /*IF FOR POR PESO e DETALHADO E FOR REAL LITEM = FALSE*/
                    END. /*R-TIPO = NO */


                    /* ACUMULA VALORES DAS DEVOLUÄÂES */
                    ASSIGN tt-aux-peso-liq-fat-dev    = tt-aux-peso-liq-fat-dev + aux-peso-liq-fat-dev
                           tt-aux-qt-faturada-dev     = tt-aux-qt-faturada-dev  + aux-qt-faturada-dev
                           tt-aux-vl-tot-item-dev     = tt-aux-vl-tot-item-dev  + aux-vl-tot-item-dev
                           tt-aux-vl-icms-it-dev      = tt-aux-vl-icms-it-dev   + aux-vl-icms-it-dev
                           tt-aux-vl-ipi-it-dev       = tt-aux-vl-ipi-it-dev    + aux-vl-ipi-it-dev
                           tt-v-cofins-dev            = tt-v-cofins-dev         + v-cofins-dev
                           tt-v-pis-dev               = tt-v-pis-dev            + v-pis-dev  
                           tt-encargos-financ-dev     = tt-encargos-financ-dev  + encargos-financ-dev
                           tt-valor-liquido-dev       = tt-valor-liquido-dev    + valor-liquido-dev
                           tt-aux-vl-despes-it-dev    = tt-aux-vl-despes-it-dev + aux-vl-despes-it-dev.


                END. /* SE TIVER MARCADO PARA DEVOL:GERENCIAL OU NO PER÷ODO SELECIONADO */
                
                
                
            END. /*FOR EACH DEVOL-CLI - SE TIVER NOTA DEVOLVIDA*/
            
        END. /*DO SE CONSIDERA DEVOLUCOES  */

        /*MOSTRA AS NOTAS QUE FORAM EMITIDAS APENAS NA SELECAO*/

        IF  tt-notas.identific <> 9 THEN do:

            assign aux-vl-ipi-it-jr  = IF it-nota-fisc.vl-ipiit-e[3]  <> 0 THEN it-nota-fisc.vl-ipiit-e[3]  ELSE 0.
            assign aux-vl-icms-it-jr = IF it-nota-fisc.vl-icmsit-e[3] <> 0 THEN it-nota-fisc.vl-icmsit-e[3] ELSE 0.
          
            ASSIGN aux-qt-faturada   = it-nota-fisc.qt-faturada[1] /*IF it-nota-fisc.baixa-estoq = TRUE THEN it-nota-fisc.qt-faturada[1] ELSE 0*/
                   aux-peso-liq-fat  =  auxpeso /* IF it-nota-fisc.baixa-estoq = TRUE THEN auxpeso ELSE 0*/
                   aux-vl-tot-item   = it-nota-fisc.vl-tot-item
                   aux-vl-ipi-it     = (IF it-nota-fisc.cd-trib-ipi <> 2 AND 
                                         it-nota-fisc.cd-trib-ipi <> 3 THEN it-nota-fisc.vl-ipi-it  ELSE 0)
                                      
                   aux-vl-ipi-it     = aux-vl-ipi-it + aux-vl-ipi-it-jr + v-ipi /* somando consignacao quando selecionado*/
                 
                   aux-vl-icms-it    = (IF it-nota-fisc.cd-trib-icm <> 2 AND 
                                         it-nota-fisc.cd-trib-icm <> 3 THEN it-nota-fisc.vl-icms-it ELSE 0)
                                        
                   aux-vl-icms-it    = aux-vl-icms-it + aux-vl-icms-it-jr + v-icms
                                        
                   aux-vl-despes-it  = it-nota-fisc.vl-despes-it
                   aux-vl-merc-liq   = it-nota-fisc.vl-merc-liq.
    
            IF  (nota-fiscal.ind-tip-nota = 3) THEN
                ASSIGN aux-qt-faturada  = 0
                       aux-peso-liq-fat = 0.
    
            IF  (it-nota-fisc.cd-trib-ipi = 3) THEN
                ASSIGN aux-vl-ipi-it = 0.
    
            IF  it-nota-fisc.dt-emis-nota < 11/01/02 THEN
                assign v-aliq-pis = dec(SUBSTRING(natur-oper.char-1, 77, 4))
                       v-aliq-cof = dec(SUBSTRING(natur-oper.char-1, 82, 4)).
            ELSE
               ASSIGN v-aliq-pis = DEC(SUBSTRING(it-nota-fisc.char-2, 76, 5))
                      v-aliq-cof = DEC(SUBSTRING(it-nota-fisc.char-2, 81, 5)).
    
            ASSIGN v-pis    = ((aux-vl-tot-item - (aux-vl-ipi-it)) * v-aliq-pis) / 100
                   v-cofins = ((aux-vl-tot-item - (aux-vl-ipi-it)) * v-aliq-cof) / 100.
    
            IF  nota-fiscal.nr-ind-finan = 0 OR l-imposto = NO THEN
                ASSIGN encargos-financ  = 0
                       aux-vl-despes-it = 0.
            ELSE
               ASSIGN encargos-financ  = aux-vl-merc-liq - (aux-vl-merc-liq / nota-fiscal.tab-ind-fin)
                      aux-vl-despes-it = it-nota-fisc.vl-despes-it.
    
        end. /* END IF IDENTIFIC */

        ASSIGN valor-liquido = aux-vl-tot-item - ( aux-vl-icms-it + aux-vl-ipi-it + v-pis + v-cofins + aux-vl-despes-it + encargos-financ).

        /* alterado o nome das variaveis de devoluá∆o que fazem a somatoria colocando tt- na frente - Ercole 06/08/2012 */
        
        ASSIGN acum-peso-liq-fat-des = acum-peso-liq-fat-des + (aux-peso-liq-fat + tt-aux-peso-liq-fat-dev)
               acum-qt-faturada-des  = acum-qt-faturada-des  + (aux-qt-faturada  + tt-aux-qt-faturada-dev)
               acum-vl-tot-item-des  = acum-vl-tot-item-des  + (aux-vl-tot-item  + tt-aux-vl-tot-item-dev)
               acum-vl-icms-it-des   = acum-vl-icms-it-des   + (aux-vl-icms-it   + tt-aux-vl-icms-it-dev)
               acum-vl-ipi-it-des    = acum-vl-ipi-it-des    + (aux-vl-ipi-it    + tt-aux-vl-ipi-it-dev)
               acum-vl-finsocial-des = acum-vl-finsocial-des + (v-cofins         + tt-v-cofins-dev)
               acum-vl-pis-des       = acum-vl-pis-des       + (v-pis            + tt-v-pis-dev)
               acum-enc-financ-des   = acum-enc-financ-des   + (encargos-financ  + tt-encargos-financ-dev)
               acum-vl-merc-liq-des  = acum-vl-merc-liq-des  + (valor-liquido    + tt-valor-liquido-dev)
               acum-vl-despes-it-des = acum-vl-despes-it-des + (aux-vl-despes-it + tt-aux-vl-despes-it-dev).

        ASSIGN acum-vl-tot-item-dolar-des  = acum-vl-tot-item-dolar-des  + ((aux-vl-tot-item  + tt-aux-vl-tot-item-dev) / de-cotacao)
               acum-vl-icms-it-dolar-des   = acum-vl-icms-it-dolar-des   + ((aux-vl-icms-it   + tt-aux-vl-icms-it-dev) / de-cotacao)
               acum-vl-ipi-it-dolar-des    = acum-vl-ipi-it-dolar-des    + ((aux-vl-ipi-it    + tt-aux-vl-ipi-it-dev) / de-cotacao)
               acum-vl-finsocial-dolar-des = acum-vl-finsocial-dolar-des + ((v-cofins         + tt-v-cofins-dev) / de-cotacao)
               acum-vl-pis-dolar-des       = acum-vl-pis-dolar-des       + ((v-pis            + tt-v-pis-dev) / de-cotacao)
               acum-enc-financ-dolar-des   = acum-enc-financ-dolar-des   + ((encargos-financ  + tt-encargos-financ-dev) / de-cotacao)
               acum-vl-merc-liq-dolar-des  = acum-vl-merc-liq-dolar-des  + ((valor-liquido    + tt-valor-liquido-dev) / de-cotacao)
               acum-vl-despes-it-dolar-des = acum-vl-despes-it-dolar-des + ((aux-vl-despes-it + tt-aux-vl-despes-it-dev) / de-cotacao).

        ASSIGN acum-peso-liq-fat-cfo = acum-peso-liq-fat-cfo + (aux-peso-liq-fat + tt-aux-peso-liq-fat-dev)
               acum-qt-faturada-cfo  = acum-qt-faturada-cfo  + (aux-qt-faturada  + tt-aux-qt-faturada-dev)
               acum-vl-tot-item-cfo  = acum-vl-tot-item-cfo  + (aux-vl-tot-item  + tt-aux-vl-tot-item-dev)
               acum-vl-icms-it-cfo   = acum-vl-icms-it-cfo   + (aux-vl-icms-it   + tt-aux-vl-icms-it-dev)
               acum-vl-ipi-it-cfo    = acum-vl-ipi-it-cfo    + (aux-vl-ipi-it    + tt-aux-vl-ipi-it-dev)
               acum-vl-finsocial-cfo = acum-vl-finsocial-cfo + (v-cofins         + tt-v-cofins-dev)
               acum-vl-pis-cfo       = acum-vl-pis-cfo       + (v-pis            + tt-v-pis-dev)
               acum-enc-financ-cfo   = acum-enc-financ-cfo   + (encargos-financ  + tt-encargos-financ-dev)
               acum-vl-merc-liq-cfo  = acum-vl-merc-liq-cfo  + (valor-liquido    + tt-valor-liquido-dev)
               acum-vl-despes-it-cfo = acum-vl-despes-it-cfo + (aux-vl-despes-it + tt-aux-vl-despes-it-dev).

        ASSIGN acum-vl-tot-item-dolar-cfo  = acum-vl-tot-item-dolar-cfo  + ((aux-vl-tot-item  + tt-aux-vl-tot-item-dev) / de-cotacao)
               acum-vl-icms-it-dolar-cfo   = acum-vl-icms-it-dolar-cfo   + ((aux-vl-icms-it   + tt-aux-vl-icms-it-dev) / de-cotacao)
               acum-vl-ipi-it-dolar-cfo    = acum-vl-ipi-it-dolar-cfo    + ((aux-vl-ipi-it    + tt-aux-vl-ipi-it-dev) / de-cotacao)
               acum-vl-finsocial-dolar-cfo = acum-vl-finsocial-dolar-cfo + ((v-cofins         + tt-v-cofins-dev) / de-cotacao)
               acum-vl-pis-dolar-cfo       = acum-vl-pis-dolar-cfo       + ((v-pis            + tt-v-pis-dev) / de-cotacao)
               acum-enc-financ-dolar-cfo   = acum-enc-financ-dolar-cfo   + ((encargos-financ  + tt-encargos-financ-dev) / de-cotacao)
               acum-vl-merc-liq-dolar-cfo  = acum-vl-merc-liq-dolar-cfo  + ((valor-liquido    + tt-valor-liquido-dev) / de-cotacao)
               acum-vl-despes-it-dolar-cfo = acum-vl-despes-it-dolar-cfo + ((aux-vl-despes-it + tt-aux-vl-despes-it-dev) / de-cotacao).

        ASSIGN valor-liquido-dev    = 0
               aux-vl-despes-it-dev = 0
               aux-vl-merc-liq-dev  = 0
               encargos-financ-dev  = 0
               aux-qt-faturada-dev  = 0
               aux-peso-liq-fat-dev = 0
               aux-vl-tot-item-dev  = 0
               aux-vl-ipi-it-dev    = 0
               aux-vl-icms-it-dev   = 0
               v-pis-dev            = 0
               v-cofins-dev         = 0
               aux-campo            = "".

    IF  tt-notas.identific <> 9 THEN DO:

        IF  t-total THEN
            ASSIGN aux-campo =   ITEM.desc-item
                   aux-campo1 = it-nota-fisc.it-codigo.
        ELSE
            ASSIGN aux-campo = it-nota-fisc.it-codigo  
                   aux-campo1 = ITEM.desc-item.

        /*SE FOR POR QUANTIDADE E FOR DETALHADO */
        IF  r-tipo = NO THEN DO:
            IF  saida = 2 THEN DO:
                FIND FIRST fat-duplic  WHERE 
                    fat-duplic.cod-estabel = nota-fiscal.cod-estabel and
                    fat-duplic.serie       = nota-fiscal.serie       and
                    fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis  NO-LOCK NO-ERROR.

                /* Rodrigo 20/04/2014 - condiá∆o de pagamento - vencidmento mÇdio */
                /*FIND FIRST cond-pagto
                     WHERE cond-pagto.cod-cond-pag = nota-fiscal.cod-cond-pag NO-LOCK NO-ERROR.
                IF AVAIL cond-pagto THEN  DO:
                    ASSIGN i-dias-medio = cond-pagto.qtd-dias-prazo-medio.
                END.
                ELSE DO:
                    ASSIGN i-qt-parcela = 0
                           i-qtd-vencto = 0.
                    FOR EACH fat-duplic  
                       WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel 
                         AND fat-duplic.serie       = nota-fiscal.serie       
                         AND fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis  NO-LOCK
                          BY fat-duplic.dt-venciment:
                        ASSIGN i-qtd-vencto = i-qtd-vencto + (fat-duplic.dt-venciment - nota-fiscal.dt-emis-nota)
                               i-qt-parcela = i-qt-parcela + 1.
                    END.

                    IF i-qt-parcela = 0 THEN ASSIGN i-qt-parcela = 1.

                    ASSIGN i-dias-medio = i-qtd-vencto / i-qt-parcela.
                END.*/

                ASSIGN i-qt-parcela = 0.
                FOR EACH b-fat-duplic  
                   WHERE b-fat-duplic.cod-estabel = nota-fiscal.cod-estabel 
                     AND b-fat-duplic.serie       = nota-fiscal.serie       
                     AND b-fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis  NO-LOCK:
                    ASSIGN i-qt-parcela = i-qt-parcela + 1.
                END.

                IF i-qt-parcela > 1 THEN DO:
                    ASSIGN dt-vencto-medio = nota-fiscal.dt-emis-nota + nota-fiscal.nr-praz-med.  /*+ i-dias-medio.*/

                    RUN pi-verif-data-util (INPUT-OUTPUT dt-vencto-medio).
                END.
                ELSE IF AVAIL fat-duplic THEN ASSIGN dt-vencto-medio = fat-duplic.dt-venciment.  /*+ i-dias-medio.*/


                IF SUBSTRING(STRING(ilin,"99999999"),8,1) = "0" THEN       RUN pi-acompanhar IN h-acomp (INPUT "Item...: " + item.it-codigo +  " Linha: " + STRING(ilin,"99999999")). 

                ASSIGN hExcel:Range(trim(entry(04,aCel) + string(ilin))):value = it-nota-fisc.nr-nota-fis
                       hExcel:Range(trim(entry(05,aCel) + string(ilin))):value = it-nota-fisc.nr-seq-fat
                       hExcel:Range(trim(entry(06,aCel) + string(ilin))):value = it-nota-fisc.nat-operacao
                       hExcel:Range(trim(entry(07,aCel) + string(ilin))):value = it-nota-fisc.dt-emis-nota
                       hExcel:Range(trim(entry(08,aCel) + string(ilin))):value = nota-fiscal.nome-ab-cli
                       hExcel:Range(trim(entry(09,aCel) + string(ilin))):value = aux-campo
                       hExcel:Range(trim(entry(10,aCel) + string(ilin))):value = (IF l-item THEN aux-qt-faturada 
                                                                              ELSE aux-peso-liq-fat)
                       hExcel:Range(trim(entry(11,aCel) + string(ilin))):value = (IF l-item THEN it-nota-fisc.un-fatur[1] 
                                                                              ELSE "Kg")
                       hExcel:Range(trim(entry(12,aCel) + string(ilin))):value = (IF t-moeda THEN aux-vl-tot-item 
                                                                              ELSE (aux-vl-tot-item / de-cotacao))
                       hExcel:Range(trim(entry(13,aCel) + string(ilin))):value = (IF t-moeda THEN aux-vl-icms-it  
                                                                              ELSE (aux-vl-icms-it / de-cotacao))
                       hExcel:Range(trim(entry(14,aCel) + string(ilin))):value = (IF t-moeda THEN aux-vl-ipi-it   
                                                                              ELSE (aux-vl-ipi-it / de-cotacao))
                       hExcel:Range(trim(entry(15,aCel) + string(ilin))):value = (IF t-moeda THEN v-cofins        
                                                                              ELSE (v-cofins / de-cotacao))
                       hExcel:Range(trim(entry(16,aCel) + string(ilin))):value = (IF t-moeda THEN v-pis           
                                                                              ELSE (v-pis / de-cotacao)).
                ASSIGN iColimp = 17.
                
                IF  l-imposto THEN
                    ASSIGN hExcel:Range(trim(entry(iColimp,aCel) + string(ilin))):value = (IF t-moeda THEN aux-vl-despes-it 
                                                                                             ELSE (aux-vl-despes-it / de-cotacao))
                           hExcel:Range(trim(entry(iColimp + 1,aCel) + string(ilin))):value = (IF t-moeda THEN encargos-financ 
                                                                                                 ELSE (encargos-financ / de-cotacao))
                           iColimp = 19.

                    ASSIGN hExcel:Range(trim(entry(iColimp,aCel) + string(ilin))):value = (IF t-moeda THEN valor-liquido 
                                                                                          ELSE (valor-liquido / de-cotacao))
                           hExcel:Range(trim(entry(iColimp + 1,aCel) + string(ilin))):value = (IF t-moeda THEN (valor-liquido / 
                                                                                                 (IF l-item THEN aux-qt-faturada 
                                                                                                  ELSE aux-peso-liq-fat)) 
                                                                                              ELSE
                                                                                                 ((valor-liquido / (IF l-item THEN 
                                                                                                                       aux-qt-faturada 
                                                                                                                    ELSE aux-peso-liq-fat)) /
                                                                                                                         de-cotacao)).

                    ASSIGN hExcel:Range(trim(entry(iColimp + 2,aCel) + string(ilin))):value = nota-fiscal.cod-estabel
                           hExcel:Range(trim(entry(iColimp + 3,aCel) + string(ilin))):value = it-nota-fisc.nr-pedcli
                           hExcel:Range(trim(entry(iColimp + 4,aCel) + string(ilin))):value = item.fm-codigo
                           hExcel:Range(trim(entry(iColimp + 5,aCel) + string(ilin))):value = item.fm-cod-com
                           
                           hExcel:Range(trim(entry(iColimp + 6,aCel) + string(ilin))):value = it-nota-fisc.nr-docum
                           hExcel:Range(trim(entry(iColimp + 7,aCel) + string(ilin))):value = IF nota-fiscal.cidade-cif <> "" THEN "CIF" 
                                                                                             ELSE "FOB"
                           hExcel:Range(trim(entry(iColimp + 8,aCel) + string(ilin))):value = nota-fiscal.nome-transp
                           hExcel:Range(trim(entry(iColimp + 9,aCel) + string(ilin))):value = nota-fiscal.cidade
                           hExcel:Range(trim(entry(iColimp + 10,aCel) + string(ilin))):value = nota-fiscal.estado.
                    
                    IF  AVAIL fat-duplic THEN 
                        ASSIGN hExcel:Range(trim(entry(iColimp + 11,aCel) + string(ilin))):value = fat-duplic.dt-venciment.

                    assign hExcel:Range(trim(entry(iColimp + 12,aCel) + string(ilin))):value = dt-vencto-medio
                           hExcel:Range(trim(entry(iColimp + 13,aCel) + string(ilin))):value = nota-fiscal.cdd-embarq
                           hExcel:Range(trim(entry(iColimp + 14,aCel) + string(ilin))):value = nota-fiscal.cod-emitente
                           hExcel:Range(trim(entry(iColimp + 15,aCel) + string(ilin))):value = aux-campo1.
                          
                    FIND FIRST movto-estoq WHERE 
                         movto-estoq.serie-docto =  it-nota-fisc.serie AND
                         movto-estoq.sequen-nf = it-nota-fisc.nr-seq-fat  AND
                         movto-estoq.nro-docto = it-nota-fisc.nr-nota-fis  AND
                         movto-estoq.nat-operacao = it-nota-fisc.nat-operacao AND
                         movto-estoq.it-codigo = it-nota-fisc.it-codigo  AND
                         movto-estoq.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.
                       
                     


                     ASSIGN hExcel:Range(trim(entry(iColimp + 16,aCel) + string(ilin))):value =  IF NOT AVAIL movto-estoq THEN 0 ELSE
                                              it-nota-fisc.qt-faturada[1] * (movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-MOB-m[1]) / movto-estoq.quantidade.  /* edson 26/7/2012 custo para Polo*/
                     ASSIGN hExcel:Range(trim(entry(iColimp + 17,aCel) + string(ilin))):value =  IF NOT AVAIL movto-estoq THEN 0 ELSE
                                              (it-nota-fisc.qt-faturada[1] * movto-estoq.valor-mat-m[1]) / movto-estoq.quantidade.  /* edson 04/7/2013 custo para Polo*/

                     ASSIGN hExcel:Range(trim(entry(iColimp + 18,aCel) + string(ilin))):value =  IF NOT AVAIL movto-estoq THEN 0 ELSE
                                              (it-nota-fisc.qt-faturada[1] * movto-estoq.valor-MOB-m[1]) / movto-estoq.quantidade.  /* edson 04/7/2013 custo para Polo*/
                     ASSIGN hExcel:Range(trim(entry(iColimp + 19,aCel) + string(ilin))):value =  IF NOT AVAIL movto-estoq THEN 0 ELSE
                                              (it-nota-fisc.qt-faturada[1] * movto-estoq.valor-GGF-m[1]) / movto-estoq.quantidade.  /* edson 04/7/2013 custo para Polo*/

                     ASSIGN hExcel:Range(trim(entry(iColimp + 20,aCel) + string(ilin))):value =  ITEM.class-fiscal
                            hExcel:Range(trim(entry(iColimp + 21,aCel) + string(ilin))):value =  IF AVAIL classif-fisc THEN classif-fisc.descricao ELSE "".  /* rodrigo 03/10/2013 marco longue*/

                     ASSIGN hExcel:Range(trim(entry(iColimp + 22,aCel) + string(ilin))):value =  IF AVAIL movto-estoq THEN movto-estoq.ct-codigo ELSE "". /* rodrigo 09/10/13 - Ricardo */
                     ASSIGN hExcel:Range(trim(entry(iColimp + 23,aCel) + string(ilin))):value =  IF AVAIL movto-estoq THEN movto-estoq.sc-codigo ELSE "". /* rodrigo 09/10/13 - Ricardo */
                     c-conta-receita = "".
                     RUN pi-conta-receita .
                     ASSIGN hExcel:Range(trim(entry(iColimp + 24,aCel) + string(ilin))):value =  c-conta-receita. /* edson 21/03/2014 - Ricardo */
                     ASSIGN hExcel:Range(trim(entry(iColimp + 25,aCel) + string(ilin))):value =  c-sc-receita. /* edson 21/03/2014 - Ricardo */

                    /*CNPJ Cliente - 31/05/2016*/
                    ASSIGN hExcel:Range(trim(entry(iColimp + 26,aCel) + string(ilin))):value = IF emitente.natureza = 1 THEN string(emitente.cgc,param-global.formato-id-pessoal)
                                                                                               ELSE IF emitente.natureza = 2 THEN string(emitente.cgc,param-global.formato-id-federal)
                                                                                               ELSE emitente.cgc.


                    iLin = iLin + 1.
                    hExcel:Rows(iLin):select no-error.
                    hExcel:Selection:INSERT.
                    hExcel:Range(trim(entry(4,aCel) + string(iLin)) + ":" + trim(entry(iColimp + 1,aCel) + string(iLin))):select no-error.
                    hExcel:Selection:ClearContents.
                    hExcel:Selection:Interior:ColorIndex = 0.
            END. /* saida = 2 */
            ELSE DO:
                IF  l-item THEN DO:
                    PUT UNFORMATTED 
                        it-nota-fisc.nr-nota-fis                 FORMAT "x(10)"
                        STRING(it-nota-fisc.nr-seq-fat)          FORMAT "x(3)"
                        it-nota-fisc.nat-operacao                FORMAT "x(8)"
                        it-nota-fisc.dt-emis-nota                FORMAT "99/99/9999"
                        nota-fiscal.nome-ab-cli           AT 34  FORMAT "x(16)"
                        aux-campo                                FORMAT "x(20)"
                        aux-qt-faturada                   AT 71  FORMAT "->>>,>>>,>>9.999"
                        it-nota-fisc.un-fatur[1]          AT 88  FORMAT "x(04)".

                    /*SE FOR POR QUANTIDADE E FOR DETALHADO E FOR REAL COM IMPOSTO*/
                    IF  t-moeda THEN DO:
                        PUT UNFORMATTED 
                            aux-vl-tot-item                   AT 092 format "->>,>>>,>>>,>>9.99"
                            aux-vl-icms-it                    AT 110 format "->,>>>,>>>,>>9.99"
                            aux-vl-ipi-it                     AT 127 format "->,>>>,>>>,>>9.99"
                            v-cofins                          AT 144 format "->,>>>,>>>,>>9.99"
                            v-pis                             AT 161 format "->,>>>,>>>,>>9.99".

                        IF  tt-param.l-imposto THEN
                            PUT UNFORMATTED 
                                aux-vl-despes-it                  AT 178 format "->,>>>,>>>,>>9.99"
                                encargos-financ                   AT 196 format "->,>>>,>>>,>>9.99"
                                valor-liquido                     AT 213 format "->>,>>>,>>>,>>9.99"
                                (valor-liquido / aux-qt-faturada) AT 231 format "->>,>>>,>>>,>>9.9999" SKIP.
                        ELSE
                            PUT UNFORMATTED 
                                valor-liquido                     AT 178 format "->>,>>>,>>>,>>9.99"
                                (valor-liquido / aux-qt-faturada) AT 196 format "->>,>>>,>>>,>>9.9999" SKIP.
                    END.
                    ELSE DO:
                        PUT UNFORMATTED 
                            (aux-vl-tot-item / de-cotacao)     AT 092 format "->>,>>>,>>>,>>9.99"
                            (aux-vl-icms-it / de-cotacao)      AT 110 format "->,>>>,>>>,>>9.99"
                            (aux-vl-ipi-it / de-cotacao)       AT 127 format "->,>>>,>>>,>>9.99"
                            (v-cofins  / de-cotacao)           AT 144 format "->,>>>,>>>,>>9.99"
                            (v-pis  / de-cotacao)              AT 161 format "->,>>>,>>>,>>9.99".

                        IF  tt-param.l-imposto THEN
                            PUT UNFORMATTED 
                                (aux-vl-despes-it  / de-cotacao)  AT 178 format "->,>>>,>>>,>>9.99"
                                (encargos-financ / de-cotacao)    AT 196 format "->,>>>,>>>,>>9.99"
                                (valor-liquido / de-cotacao)      AT 213 format "->>,>>>,>>>,>>9.99"
                                ((valor-liquido / aux-qt-faturada) / de-cotacao) AT 231 format "->>,>>>,>>>,>>9.9999" SKIP.
                        ELSE
                            PUT UNFORMATTED 
                                (valor-liquido / de-cotacao)      AT 178 format "->>,>>>,>>>,>>9.99"
                                ((valor-liquido / aux-qt-faturada) / de-cotacao) AT 196 format "->>,>>>,>>>,>>9.9999" SKIP.
                    END.
                END. /* IF  l-item THEN DO: */
                ELSE DO:
                    /*IF FOR POR PESO e DETALHADO E FOR REAL*/
                    PUT UNFORMATTED 
                        it-nota-fisc.nr-nota-fis              FORMAT "x(10)"
                        string(it-nota-fisc.nr-seq-fat)       FORMAT "x(3)"
                        it-nota-fisc.nat-operacao             FORMAT "x(8)"
                        it-nota-fisc.dt-emis-nota             format "99/99/9999"
                        nota-fiscal.nome-ab-cli        AT 34  format "x(16)"
                        aux-campo                             FORMAT "x(20)"
                        aux-peso-liq-fat               AT 71  format "->>>,>>>,>>9.999"
                        "Kg"                           AT 88  FORMAT "x(04)".

                    /*SE FOR POR QUANTIDADE E FOR DETALHADO E FOR REAL COM IMPOSTO*/
                    IF  t-moeda THEN DO:
                        PUT UNFORMATTED 
                            aux-vl-tot-item              AT 092 format "->>,>>>,>>>,>>9.99"
                            aux-vl-icms-it               AT 110 format "->,>>>,>>>,>>9.99"
                            aux-vl-ipi-it                AT 127 format "->,>>>,>>>,>>9.99"
                            v-cofins                     AT 144 format "->,>>>,>>>,>>9.99"
                            v-pis                        AT 161 format "->,>>>,>>>,>>9.99".

                        IF  tt-param.l-imposto THEN
                            PUT UNFORMATTED 
                                aux-vl-despes-it           AT 178 format "->,>>>,>>>,>>9.99"
                                encargos-financ            AT 196 format "->,>>>,>>>,>>9.99"
                                valor-liquido              AT 213 format "->>,>>>,>>>,>>9.99"
                                (valor-liquido / aux-peso-liq-fat) AT 231 format "->>,>>>,>>>,>>9.9999" SKIP.
                        ELSE   
                            PUT UNFORMATTED 
                                valor-liquido              AT 178 format "->>,>>>,>>>,>>9.99"
                                (valor-liquido / aux-peso-liq-fat) AT 196 format "->>,>>>,>>>,>>9.9999" SKIP.
                    END.
                    ELSE DO:
                        PUT UNFORMATTED 
                            (aux-vl-tot-item / de-cotacao)         AT 092 FORMAT "->>,>>>,>>>,>>9.99"
                            (aux-vl-icms-it / de-cotacao)          AT 110 format "->,>>>,>>>,>>9.99"
                            (aux-vl-ipi-it / de-cotacao)           AT 127 format "->,>>>,>>>,>>9.99"
                            (v-cofins  / de-cotacao)               AT 144 format "->,>>>,>>>,>>9.99"
                            (v-pis  / de-cotacao)                  AT 161 format "->,>>>,>>>,>>9.99".

                        IF  tt-param.l-imposto THEN
                            PUT UNFORMATTED 
                                (aux-vl-despes-it  / de-cotacao)     AT 178 format "->,>>>,>>>,>>9.99"
                                (encargos-financ / de-cotacao)       AT 196 format "->,>>>,>>>,>>9.99"
                                (valor-liquido / de-cotacao)         AT 213 format "->>,>>>,>>>,>>9.99"
                                ((valor-liquido / aux-peso-liq-fat) / de-cotacao) AT 231 format "->>,>>>,>>>,>>9.9999" SKIP.
                        ELSE
                            PUT UNFORMATTED (valor-liquido / de-cotacao) AT 178 format "->>,>>>,>>>,>>9.99"
                               ((valor-liquido / aux-peso-liq-fat) / de-cotacao) AT 196 format "->>,>>>,>>>,>>9.9999" SKIP.
                    END.
                END. /*l-item*/
            END. /*saida*/
        END. /*r-tipo*/
/**************************************************************************************************************************************/
    END.  /* tt-notas.identific <> 9.*/

    IF  LAST-OF(IF rs-classif = 14 THEN it-nota-fisc.nat-operacao
                 ELSE IF rs-classif = 10 THEN item.fm-codigo
                 ELSE it-nota-fisc.nat-operacao) THEN DO:

        IF  rs-classif = 14 THEN
            ASSIGN auxmer  = CAPS(it-nota-fisc.nat-operacao)
                   auxdesc = "TOTAL POR CFO.....:".
        ELSE IF  rs-classif = 10 THEN
                 ASSIGN auxmer  = CAPS(familia.descricao)
                        auxdesc = "TOTAL POR FAMILIA.:".

        IF  r-tipo = NO AND saida <> 2 THEN
            IF tt-param.l-imposto THEN
               PUT UNFORMATTED detalhe1 AT 072 SKIP.
            ELSE
               PUT UNFORMATTED detalhe2 AT 072 SKIP.

        /*SE A ESCOLHA FOR QUANTIDADE E O TIPO DE MOEDA FOR REAL*/
        IF  saida = 2 THEN DO:
            ASSIGN hExcel:Range(trim(entry(08,aCel) + string(ilin))):value = auxmer
                   hExcel:Range(trim(entry(09,aCel) + string(ilin))):value = auxdesc
                   hExcel:Range(trim(entry(10,aCel) + string(ilin))):value = (IF l-item  THEN acum-qt-faturada-cfo  
                                                                              ELSE acum-peso-liq-fat-cfo)
                   hExcel:Range(trim(entry(12,aCel) + string(ilin))):value = (IF t-moeda THEN acum-vl-tot-item-cfo  
                                                                              ELSE acum-vl-tot-item-dolar-cfo)
                   hExcel:Range(trim(entry(13,aCel) + string(ilin))):value = (IF t-moeda THEN acum-vl-icms-it-cfo   
                                                                              ELSE acum-vl-icms-it-dolar-cfo)
                   hExcel:Range(trim(entry(14,aCel) + string(ilin))):value = (IF t-moeda THEN acum-vl-ipi-it-cfo    
                                                                              ELSE acum-vl-ipi-it-dolar-cfo)
                   hExcel:Range(trim(entry(15,aCel) + string(ilin))):value = (IF t-moeda THEN acum-vl-finsocial-cfo 
                                                                              ELSE acum-vl-finsocial-dolar-cfo)
                   hExcel:Range(trim(entry(16,aCel) + string(ilin))):value = (IF t-moeda THEN acum-vl-pis-cfo       
                                                                              ELSE acum-vl-pis-dolar-cfo).
                   ASSIGN iColimp = 17.
                   IF l-imposto THEN
                      ASSIGN hExcel:Range(trim(entry(iColimp,aCel) + string(ilin))):value = (IF t-moeda THEN acum-vl-despes-it-cfo 
                                                                                             ELSE acum-vl-despes-it-dolar-cfo)
                             hExcel:Range(trim(entry(iColimp + 1,aCel) + string(ilin))):value = (IF t-moeda THEN acum-enc-financ-cfo 
                                                                                                 ELSE acum-enc-financ-dolar-cfo)
                             iColimp = 19.
                   ASSIGN hExcel:Range(trim(entry(iColimp,aCel) + string(ilin))):value = (IF t-moeda THEN acum-vl-merc-liq-cfo 
                                                                                          ELSE acum-vl-merc-liq-dolar-cfo)
                          hExcel:Range(trim(entry(iColimp + 1,aCel) + string(ilin))):value = (IF t-moeda THEN (acum-vl-merc-liq-cfo / 
                                                                                                              (IF l-item THEN
                                                                                                                   acum-qt-faturada-cfo 
                                                                                                               ELSE acum-peso-liq-fat-cfo)) 
                                                                                              ELSE (acum-vl-merc-liq-dolar-cfo / 
                                                                                                   (IF l-item THEN acum-qt-faturada-cfo 
                                                                                                    ELSE acum-peso-liq-fat-cfo))).
                   IF r-tipo = NO THEN DO:
                      hExcel:Range(trim(entry(08,aCel) + string(iLin)) + ":" + trim(entry(iColimp + 1,aCel) + string(iLin))):select.
                      hExcel:Selection:Font:Bold = True.
                      hExcel:Selection:Interior:ColorIndex = 24.
                   END.

                    /* TESTE TOTAL */
                    
                   ASSIGN 
                        acum-qt-faturada-cfo         = 0
                        acum-peso-liq-fat-cfo        = 0
                        acum-vl-tot-item-cfo         = 0
                        acum-vl-tot-item-dolar-cfo   = 0
                        acum-vl-icms-it-cfo          = 0
                        acum-vl-icms-it-dolar-cfo    = 0
                        acum-vl-ipi-it-cfo           = 0 
                        acum-vl-ipi-it-dolar-cfo     = 0 
                        acum-vl-finsocial-cfo        = 0 
                        acum-vl-finsocial-dolar-cfo  = 0 
                        acum-vl-pis-cfo              = 0 
                        acum-vl-pis-dolar-cfo        = 0 
                        acum-vl-despes-it-cfo        = 0 
                        acum-vl-despes-it-dolar-cfo  = 0 
                        acum-enc-financ-cfo          = 0 
                        acum-enc-financ-dolar-cfo    = 0 
                        acum-vl-merc-liq-cfo         = 0 
                        acum-vl-merc-liq-dolar-cfo   = 0
                        aux-qt-faturada              = 0 
                        aux-peso-liq-fat             = 0
                        aux-vl-tot-item              = 0
                        aux-vl-icms-it               = 0
                        aux-vl-ipi-it                = 0
                        v-cofins                     = 0
                        v-pis                        = 0
                        aux-vl-despes-it             = 0
                        encargos-financ              = 0
                        valor-liquido                = 0.

                   /* TESTE TOTAL */


                   iLin = iLin + 1.
                   hExcel:Rows(iLin):select no-error.
                   hExcel:Selection:INSERT.
                   hExcel:Range(trim(entry(4,aCel) + string(iLin)) + ":" + trim(entry(iColimp + 1,aCel) + string(iLin))):select no-error.
                   hExcel:Selection:ClearContents.
                   hExcel:Selection:Font:Bold = FALSE.
                   hExcel:Selection:Interior:ColorIndex = 0.
        END.
        ELSE 
            IF  l-item THEN DO:
                PUT UNFORMATTED 
                    auxmer                AT 14 format "x(35)"
                    auxdesc
                    acum-qt-faturada-cfo  AT 71 format "->>>,>>>,>>9.999".
    
                IF  t-moeda THEN DO:
                    PUT UNFORMATTED acum-vl-tot-item-cfo     AT 092 FORMAT "->>,>>>,>>>,>>9.99"
                        acum-vl-icms-it-cfo                 AT 110 format "->,>>>,>>>,>>9.99"
                        acum-vl-ipi-it-cfo                  AT 127 format "->,>>>,>>>,>>9.99"
                        acum-vl-finsocial-cfo               AT 144 format "->,>>>,>>>,>>9.99"
                        acum-vl-pis-cfo                     AT 161 format "->,>>>,>>>,>>9.99".
    
                    IF  tt-param.l-imposto THEN DO:
                        PUT UNFORMATTED acum-vl-despes-it-cfo  AT 178 format "->,>>>,>>>,>>9.99"
                            acum-enc-financ-cfo                 AT 196 format "->,>>>,>>>,>>9.99"
                            acum-vl-merc-liq-cfo                AT 213 format "->>,>>>,>>>,>>9.99"
                            (acum-vl-merc-liq-cfo / acum-qt-faturada-cfo) AT  231 format "->>,>>>,>>>,>>9.9999".
                        IF r-tipo THEN 
                            PUT SKIP. 
                        ELSE 
                            PUT SKIP(1).
                    END.
                    ELSE DO:
                        PUT UNFORMATTED acum-vl-merc-liq-cfo     AT 178 format "->>,>>>,>>>,>>9.99"
                            (acum-vl-merc-liq-cfo / acum-qt-faturada-cfo) AT 196 format "->>,>>>,>>>,>>9.9999".
                        IF r-tipo THEN 
                            PUT SKIP. 
                        ELSE 
                            PUT SKIP(1).
                    END.
                END.
                ELSE DO:
                    PUT UNFORMATTED 
                        acum-vl-tot-item-dolar-cfo  AT 092 format "->>,>>>,>>>,>>9.99"
                        acum-vl-icms-it-dolar-cfo   AT 110 format "->,>>>,>>>,>>9.99"
                        acum-vl-ipi-it-dolar-cfo    AT 127 format "->,>>>,>>>,>>9.99"
                        acum-vl-finsocial-dolar-cfo AT 144 format "->,>>>,>>>,>>9.99"
                        acum-vl-pis-dolar-cfo	    AT 161 format "->,>>>,>>>,>>9.99".
    
                    IF  tt-param.l-imposto THEN DO:
                        PUT UNFORMATTED 
                            acum-vl-despes-it-dolar-cfo AT 178 format "->,>>>,>>>,>>9.99"
                            acum-enc-financ-dolar-cfo   AT 196 FORMAT "->,>>>,>>>,>>9.99"
                            acum-vl-merc-liq-dolar-cfo  AT 213 format "->>,>>>,>>>,>>9.99"
                            (acum-vl-merc-liq-dolar-cfo / acum-qt-faturada-cfo) AT 231 FORMAT "->>,>>>,>>>,>>9.9999".
                        IF r-tipo THEN PUT SKIP. ELSE PUT SKIP(1).
                    END.
                    ELSE DO:
                        PUT UNFORMATTED 
                            acum-vl-merc-liq-dolar-cfo    AT 178 format "->>,>>>,>>>,>>9.99"
                            (acum-vl-merc-liq-dolar-cfo / acum-qt-faturada-cfo) AT 196 FORMAT "->>,>>>,>>>,>>9.9999".
                        IF r-tipo THEN PUT SKIP. ELSE PUT SKIP(1).
                    END.
                END.
            END.
            ELSE DO:
                  /*SE A ESCOLHA FOR PESO E A MOEDA FOR REAL*/
                PUT UNFORMATTED auxmer    AT 14 format "x(35)"
                     auxdesc
                     acum-peso-liq-fat-cfo AT 71 format "->>>,>>>,>>9.999".
    
                IF t-moeda THEN DO:
                   PUT UNFORMATTED 
                        acum-vl-tot-item-cfo       AT 092 format "->>,>>>,>>>,>>9.99"
                        acum-vl-icms-it-cfo        AT 110 format "->,>>>,>>>,>>9.99"
                        acum-vl-ipi-it-cfo         AT 127 format "->,>>>,>>>,>>9.99"
                        acum-vl-finsocial-cfo      AT 144 format "->,>>>,>>>,>>9.99"
                        acum-vl-pis-cfo            AT 161 format "->,>>>,>>>,>>9.99".
    
                   IF tt-param.l-imposto THEN DO:
                      PUT UNFORMATTED acum-vl-despes-it-cfo AT 178 format "->,>>>,>>>,>>9.99"
                           acum-enc-financ-cfo              AT 196 FORMAT "->,>>>,>>>,>>9.99"
                           acum-vl-merc-liq-cfo             AT 213 format "->>,>>>,>>>,>>9.99"
                           (acum-vl-merc-liq-cfo / acum-peso-liq-fat-cfo) AT 231 FORMAT "->>,>>>,>>>,>>9.9999".
                      IF r-tipo THEN PUT SKIP. ELSE PUT SKIP(1).
                   END.
                   ELSE DO:
                      PUT UNFORMATTED 
                          acum-vl-merc-liq-cfo                AT 178 format "->>,>>>,>>>,>>9.99"
                          (acum-vl-merc-liq-cfo / acum-peso-liq-fat-cfo) AT 196 FORMAT "->>,>>>,>>>,>>9.9999".
                      IF r-tipo THEN PUT SKIP. ELSE PUT SKIP(1).
                   END.
                END.
                ELSE DO:
                   PUT UNFORMATTED 
                       acum-vl-tot-item-dolar-cfo   AT 092 format "->>,>>>,>>>,>>9.99"
                       acum-vl-icms-it-dolar-cfo    AT 110 format "->,>>>,>>>,>>9.99"
                       acum-vl-ipi-it-dolar-cfo     AT 127 format "->,>>>,>>>,>>9.99"
                       acum-vl-finsocial-dolar-cfo  AT 144 format "->,>>>,>>>,>>9.99"
                       acum-vl-pis-dolar-cfo		AT 161 format "->,>>>,>>>,>>9.99".
    
                   IF tt-param.l-imposto THEN DO:
                      PUT UNFORMATTED 
                          acum-vl-despes-it-dolar-cfo  AT 178 format "->,>>>,>>>,>>9.99"
                          acum-enc-financ-dolar-cfo    AT 196 FORMAT "->,>>>,>>>,>>9.99"
                          acum-vl-merc-liq-dolar-cfo   AT 213 format "->>,>>>,>>>,>>9.99"
                          (acum-vl-merc-liq-dolar-cfo / acum-peso-liq-fat-cfo) AT 231 format "->>,>>>,>>>,>>9.9999".
                      IF r-tipo THEN PUT SKIP. ELSE PUT SKIP(1).
                   END.
                   ELSE DO:
                      PUT UNFORMATTED 
                          acum-vl-merc-liq-dolar-cfo AT 178 format "->>,>>>,>>>,>>9.99"
                          (acum-vl-merc-liq-dolar-cfo / acum-peso-liq-fat-cfo) AT 196 format "->>,>>>,>>>,>>9.9999".
                      IF r-tipo THEN PUT SKIP. ELSE PUT SKIP(1).
                   END.
            END.
        END.
    
         ASSIGN t-acum-peso-liq-fat-cfo = t-acum-peso-liq-fat-cfo + acum-peso-liq-fat-cfo
                t-acum-qt-faturada-cfo  = t-acum-qt-faturada-cfo  + acum-qt-faturada-cfo
                t-acum-vl-tot-item-cfo  = t-acum-vl-tot-item-cfo  + acum-vl-tot-item-cfo
                t-acum-vl-icms-it-cfo   = t-acum-vl-icms-it-cfo   + acum-vl-icms-it-cfo
                t-acum-vl-ipi-it-cfo    = t-acum-vl-ipi-it-cfo    + acum-vl-ipi-it-cfo
                t-acum-vl-finsocial-cfo = t-acum-vl-finsocial-cfo + acum-vl-finsocial-cfo
                t-acum-vl-pis-cfo       = t-acum-vl-pis-cfo       + acum-vl-pis-cfo
                t-acum-enc-financ-cfo   = t-acum-enc-financ-cfo   + acum-enc-financ-cfo
                t-acum-vl-merc-liq-cfo  = t-acum-vl-merc-liq-cfo  + acum-vl-merc-liq-cfo
                t-acum-vl-despes-it-cfo = t-acum-vl-despes-it-cfo + acum-vl-despes-it-cfo.

         ASSIGN t-acum-vl-tot-item-dolar-cfo  = t-acum-vl-tot-item-dolar-cfo  + acum-vl-tot-item-dolar-cfo
                t-acum-vl-icms-it-dolar-cfo   = t-acum-vl-icms-it-dolar-cfo   + acum-vl-icms-it-dolar-cfo
                t-acum-vl-ipi-it-dolar-cfo    = t-acum-vl-ipi-it-dolar-cfo    + acum-vl-ipi-it-dolar-cfo
                t-acum-vl-finsocial-dolar-cfo = t-acum-vl-finsocial-dolar-cfo + acum-vl-finsocial-dolar-cfo
                t-acum-vl-pis-dolar-cfo       = t-acum-vl-pis-dolar-cfo       + acum-vl-pis-dolar-cfo
                t-acum-enc-financ-dolar-cfo   = t-acum-enc-financ-dolar-cfo   + acum-enc-financ-dolar-cfo
                t-acum-vl-merc-liq-dolar-cfo  = t-acum-vl-merc-liq-dolar-cfo  + acum-vl-merc-liq-dolar-cfo
                t-acum-vl-despes-it-dolar-cfo = t-acum-vl-despes-it-dolar-cfo + acum-vl-despes-it-dolar-cfo.

         ASSIGN 
             acum-qt-faturada-cfo         = 0
             acum-peso-liq-fat-cfo        = 0
             acum-vl-tot-item-cfo         = 0
             acum-vl-tot-item-dolar-cfo   = 0
             acum-vl-icms-it-cfo          = 0
             acum-vl-icms-it-dolar-cfo    = 0
             acum-vl-ipi-it-cfo           = 0 
             acum-vl-ipi-it-dolar-cfo     = 0 
             acum-vl-finsocial-cfo        = 0 
             acum-vl-finsocial-dolar-cfo  = 0 
             acum-vl-pis-cfo              = 0 
             acum-vl-pis-dolar-cfo        = 0 
             acum-vl-despes-it-cfo        = 0 
             acum-vl-despes-it-dolar-cfo  = 0 
             acum-enc-financ-cfo          = 0 
             acum-enc-financ-dolar-cfo    = 0 
             acum-vl-merc-liq-cfo         = 0 
             acum-vl-merc-liq-dolar-cfo   = 0
             aux-qt-faturada              = 0 
             aux-peso-liq-fat             = 0
             aux-vl-tot-item              = 0
             aux-vl-icms-it               = 0
             aux-vl-ipi-it                = 0
             v-cofins                     = 0
             v-pis                        = 0
             aux-vl-despes-it             = 0
             encargos-financ              = 0
             valor-liquido                = 0.

         ASSIGN acum-peso-liq-fat-cfo       = 0
                acum-qt-faturada-cfo        = 0
                acum-vl-tot-item-cfo        = 0
                acum-vl-icms-it-cfo         = 0
                acum-vl-ipi-it-cfo          = 0
                acum-vl-finsocial-cfo       = 0
                acum-vl-pis-cfo             = 0
                acum-vl-merc-liq-cfo        = 0
                acum-vl-despes-it-cfo       = 0
                acum-enc-financ-cfo         = 0
                acum-vl-tot-item-dolar-cfo  = 0
                acum-vl-icms-it-dolar-cfo   = 0
                acum-vl-ipi-it-dolar-cfo    = 0
                acum-vl-finsocial-dolar-cfo = 0
                acum-vl-pis-dolar-cfo       = 0
                acum-vl-merc-liq-dolar-cfo  = 0
                acum-vl-despes-it-dolar-cfo = 0
                acum-enc-financ-dolar-cfo   = 0.
      END.

      IF LAST-OF(IF s-desc THEN ITEM.desc-item
                 ELSE IF rs-classif = 14 THEN ITEM.it-codigo
                      ELSE IF rs-classif = 10 THEN (IF emitente.pais = "brasil" THEN "BRA" 
                                                    ELSE "EXT")
                           ELSE ITEM.it-codigo) THEN DO:

         IF s-desc THEN
            ASSIGN auxdesc = "TOTAL POR DESCRICAO.:"
                   auxmer  = aux-campo.
         ELSE IF rs-classif = 14 THEN
                 ASSIGN auxdesc = "TOTAL POR PRODUTO.:"
                        auxmer  = aux-campo.
              ELSE IF rs-classif = 10 THEN DO:
                      IF emitente.pais = "brasil" THEN
                          ASSIGN auxmer = "MERCADO INTERNO".
                      ELSE
                          ASSIGN auxmer = "MERCADO EXTERNO".
                      ASSIGN auxdesc = "TOTAL POR MERCADO.:".
                    END.

         IF r-tipo = NO AND saida <> 2 THEN
            IF tt-param.l-imposto THEN
               PUT UNFORMATTED detalhe1 AT 072 SKIP.
            ELSE
               PUT UNFORMATTED detalhe2 AT 072 SKIP.

         /* SE FOR QUANTIDADE E A MOEDA FOR O REAL*/
         IF saida = 2 THEN DO:
            ASSIGN hExcel:Range(trim(entry(08,aCel) + string(ilin))):value = auxmer
                   hExcel:Range(trim(entry(09,aCel) + string(ilin))):value = auxdesc
                   hExcel:Range(trim(entry(10,aCel) + string(ilin))):value = (IF l-item  THEN acum-qt-faturada-des  
                                                                              ELSE acum-peso-liq-fat-des)
                   hExcel:Range(trim(entry(12,aCel) + string(ilin))):value = (IF t-moeda THEN acum-vl-tot-item-des  
                                                                              ELSE acum-vl-tot-item-dolar-des)
                   hExcel:Range(trim(entry(13,aCel) + string(ilin))):value = (IF t-moeda THEN acum-vl-icms-it-des   
                                                                              ELSE acum-vl-icms-it-dolar-des)
                   hExcel:Range(trim(entry(14,aCel) + string(ilin))):value = (IF t-moeda THEN acum-vl-ipi-it-des    
                                                                              ELSE acum-vl-ipi-it-dolar-des)
                   hExcel:Range(trim(entry(15,aCel) + string(ilin))):value = (IF t-moeda THEN acum-vl-finsocial-des 
                                                                              ELSE acum-vl-finsocial-dolar-des)
                   hExcel:Range(trim(entry(16,aCel) + string(ilin))):value = (IF t-moeda THEN acum-vl-pis-des       
                                                                              ELSE acum-vl-pis-dolar-des).
            ASSIGN iColimp = 17.
            IF l-imposto THEN
                ASSIGN hExcel:Range(trim(entry(iColimp,aCel) + string(ilin))):value = (IF t-moeda THEN acum-vl-despes-it-des 
                                                                                       ELSE acum-vl-despes-it-dolar-des)
                       hExcel:Range(trim(entry(iColimp + 1,aCel) + string(ilin))):value = (IF t-moeda THEN acum-enc-financ-des 
                                                                                           ELSE acum-enc-financ-dolar-des)
                       iColimp = 19.
                ASSIGN hExcel:Range(trim(entry(iColimp,aCel) + string(ilin))):value = (IF t-moeda THEN acum-vl-merc-liq-des 
                                                                                       ELSE acum-vl-merc-liq-dolar-des)
                       hExcel:Range(trim(entry(iColimp + 1,aCel) + string(ilin))):value = (IF t-moeda THEN (acum-vl-merc-liq-des / 
                                                                                                           (IF l-item THEN 
                                                                                                               acum-qt-faturada-des 
                                                                                                            ELSE acum-peso-liq-fat-des)) 
                                                                                           ELSE (acum-vl-merc-liq-dolar-des / 
                                                                                                (IF l-item THEN acum-qt-faturada-des 
                                                                                                 ELSE acum-peso-liq-fat-des))).
                IF r-tipo = NO THEN DO:
                     hExcel:Range(trim(entry(08,aCel) + string(iLin)) + ":" + trim(entry(iColimp + 1,aCel) + string(iLin))):select.
                     hExcel:Selection:Font:Bold = True.
                     hExcel:Selection:Interior:ColorIndex = 24.
                END.
                iLin = iLin + 1.
                hExcel:Rows(iLin):select no-error.
                hExcel:Selection:INSERT.
                hExcel:Range(trim(entry(4,aCel) + string(iLin)) + ":" + trim(entry(iColimp + 1,aCel) + string(iLin))):select no-error.
                hExcel:Selection:ClearContents.
                hExcel:Selection:Font:Bold = FALSE.
                hExcel:Selection:Interior:ColorIndex = 0.
         END.
         ELSE IF l-item THEN DO:
            PUT UNFORMATTED 
                 auxmer                AT 14 FORMAT "x(35)"
                 auxdesc
                 acum-qt-faturada-des  AT 71 format "->>>,>>>,>>9.999".

            IF t-moeda THEN DO:
               PUT UNFORMATTED             
                   acum-vl-tot-item-des     AT 092 format "->>,>>>,>>>,>>9.99"
                    acum-vl-icms-it-des     AT 110 format "->,>>>,>>>,>>9.99"
                    acum-vl-ipi-it-des      AT 127 format "->,>>>,>>>,>>9.99"
                    acum-vl-finsocial-des   AT 144 format "->,>>>,>>>,>>9.99"
                    acum-vl-pis-des         AT 161 format "->,>>>,>>>,>>9.99".

               IF tt-param.l-imposto THEN DO:
                  PUT UNFORMATTED 
                      acum-vl-despes-it-des AT 178 format "->,>>>,>>>,>>9.99"
                       acum-enc-financ-des  AT 196 FORMAT "->,>>>,>>>,>>9.99"
                       acum-vl-merc-liq-des AT 213 format "->>,>>>,>>>,>>9.99"
                       (acum-vl-merc-liq-des / acum-qt-faturada-des) AT 231 format "->>,>>>,>>>,>>9.9999".
               IF r-tipo THEN PUT SKIP. ELSE PUT SKIP(1).
               END.
               ELSE DO:
                  PUT UNFORMATTED 
                      acum-vl-merc-liq-des AT 178 format "->>,>>>,>>>,>>9.99"
                      (acum-vl-merc-liq-des / acum-qt-faturada-des) AT 196 format "->>,>>>,>>>,>>9.9999".
                  IF r-tipo THEN PUT SKIP. ELSE PUT SKIP(1).
               END.
            END.
            ELSE DO:
               PUT UNFORMATTED 
                   acum-vl-tot-item-dolar-des   AT 092 format "->>,>>>,>>>,>>9.99"
                    acum-vl-icms-it-dolar-des   AT 110 format "->,>>>,>>>,>>9.99"
                    acum-vl-ipi-it-dolar-des    AT 127 format "->,>>>,>>>,>>9.99"
                    acum-vl-finsocial-dolar-des AT 144 format "->,>>>,>>>,>>9.99"
                    acum-vl-pis-dolar-des	    AT 161 format "->,>>>,>>>,>>9.99".

               IF tt-param.l-imposto THEN DO:
                  PUT UNFORMATTED 
                      acum-vl-despes-it-dolar-des  AT 178 format "->,>>>,>>>,>>9.99"
                      acum-enc-financ-dolar-des    AT 196 FORMAT "->,>>>,>>>,>>9.99"
                      acum-vl-merc-liq-dolar-des   AT 213 format "->>,>>>,>>>,>>9.99"
                      (acum-vl-merc-liq-dolar-des / acum-qt-faturada-des)  AT 231 format "->>,>>>,>>>,>>9.9999".
                  IF r-tipo THEN PUT SKIP. ELSE PUT SKIP(1).
               END.
               ELSE DO:
                  PUT UNFORMATTED 
                      acum-vl-merc-liq-dolar-des  AT 178 format "->>,>>>,>>>,>>9.99"
                      (acum-vl-merc-liq-dolar-des / acum-qt-faturada-des) AT 196 format "->>,>>>,>>>,>>9.9999".
                  IF r-tipo THEN PUT SKIP. ELSE PUT SKIP(1).
               END.
            END.
        END.
        ELSE DO:
          /*SE FOR PESO E TIPO DA MOEDA FOR REAL*/
            PUT UNFORMATTED 
                 auxmer                AT 14 FORMAT "x(35)"
                 auxdesc
                 acum-peso-liq-fat-des AT 71 format "->>>,>>>,>>9.999".

            IF t-moeda THEN DO:
               PUT UNFORMATTED 
                    acum-vl-tot-item-des   AT 092 format "->>,>>>,>>>,>>9.99"
                    acum-vl-icms-it-des    AT 110 format "->,>>>,>>>,>>9.99"
                    acum-vl-ipi-it-des     AT 127 format "->,>>>,>>>,>>9.99"
                    acum-vl-finsocial-des  AT 144 format "->,>>>,>>>,>>9.99"
                    acum-vl-pis-des        AT 161 format "->,>>>,>>>,>>9.99".

               IF tt-param.l-imposto THEN DO:
                  PUT UNFORMATTED 
                       acum-vl-despes-it-des AT 178 format "->,>>>,>>>,>>9.99"
                       acum-enc-financ-des   AT 196 FORMAT "->,>>>,>>>,>>9.99"
                       acum-vl-merc-liq-des  AT 213 format "->>,>>>,>>>,>>9.99"
                       (acum-vl-merc-liq-des / acum-peso-liq-fat-des) AT 231 format "->>,>>>,>>>,>>9.9999".
                  IF r-tipo THEN PUT SKIP. ELSE PUT SKIP(1).
               END.
               ELSE DO:
                  PUT UNFORMATTED 
                      acum-vl-merc-liq-des  AT 178 format "->>,>>>,>>>,>>9.99"
                      (acum-vl-merc-liq-des / acum-peso-liq-fat-des) AT 196 format "->>,>>>,>>>,>>9.9999".
                  IF r-tipo THEN PUT SKIP. ELSE PUT SKIP(1).
               END.
            END.
            ELSE DO:
               PUT UNFORMATTED 
                    acum-vl-tot-item-dolar-des  AT 092 FORMAT "->>,>>>,>>>,>>9.99"
                    acum-vl-icms-it-dolar-des   AT 110 format "->,>>>,>>>,>>9.99"
                    acum-vl-ipi-it-dolar-des    AT 127 format "->,>>>,>>>,>>9.99"
                    acum-vl-finsocial-dolar-des AT 144 FORMAT "->,>>>,>>>,>>9.99"
                    acum-vl-pis-dolar-des	    AT 161 format "->,>>>,>>>,>>9.99".

               IF tt-param.l-imposto THEN DO:
                  PUT UNFORMATTED 
                      acum-vl-despes-it-dolar-des   AT 178 format "->,>>>,>>>,>>9.99"
                       acum-enc-financ-dolar-des    AT 196 FORMAT "->,>>>,>>>,>>9.99"
                       acum-vl-merc-liq-dolar-des   AT 213 format "->>,>>>,>>>,>>9.99"
                       (acum-vl-merc-liq-dolar-des / acum-peso-liq-fat-des) AT 231 format "->>,>>>,>>>,>>9.9999".
                  IF r-tipo THEN PUT SKIP. ELSE PUT SKIP(1).
               END.
               ELSE DO:
                  PUT UNFORMATTED 
                      acum-vl-merc-liq-dolar-des AT 178 format "->>,>>>,>>>,>>9.99"
                      (acum-vl-merc-liq-dolar-des / acum-peso-liq-fat-des) AT 196 format "->>,>>>,>>>,>>9.9999".
                  IF r-tipo THEN PUT SKIP. ELSE PUT SKIP(1).
               END.
            END.
        END.

        ASSIGN  t-acum-peso-liq-fat-des = t-acum-peso-liq-fat-des + acum-peso-liq-fat-des
                t-acum-qt-faturada-des  = t-acum-qt-faturada-des  + acum-qt-faturada-des
                t-acum-vl-tot-item-des  = t-acum-vl-tot-item-des  + acum-vl-tot-item-des
                t-acum-vl-icms-it-des   = t-acum-vl-icms-it-des   + acum-vl-icms-it-des
                t-acum-vl-ipi-it-des    = t-acum-vl-ipi-it-des    + acum-vl-ipi-it-des
                t-acum-vl-finsocial-des = t-acum-vl-finsocial-des + acum-vl-finsocial-des
                t-acum-vl-pis-des       = t-acum-vl-pis-des       + acum-vl-pis-des
                t-acum-enc-financ-des   = t-acum-enc-financ-des   + acum-enc-financ-des
                t-acum-vl-merc-liq-des  = t-acum-vl-merc-liq-des  + acum-vl-merc-liq-des
                t-acum-vl-despes-it-des = t-acum-vl-despes-it-des + acum-vl-despes-it-des.

        ASSIGN  t-acum-vl-tot-item-dolar-des  = t-acum-vl-tot-item-dolar-des  + acum-vl-tot-item-dolar-des
                t-acum-vl-icms-it-dolar-des   = t-acum-vl-icms-it-dolar-des   + acum-vl-icms-it-dolar-des
                t-acum-vl-ipi-it-dolar-des    = t-acum-vl-ipi-it-dolar-des    + acum-vl-ipi-it-dolar-des
                t-acum-vl-finsocial-dolar-des = t-acum-vl-finsocial-dolar-des + acum-vl-finsocial-dolar-des
                t-acum-vl-pis-dolar-des       = t-acum-vl-pis-dolar-des       + acum-vl-pis-dolar-des
                t-acum-enc-financ-dolar-des   = t-acum-enc-financ-dolar-des   + acum-enc-financ-dolar-des
                t-acum-vl-merc-liq-dolar-des  = t-acum-vl-merc-liq-dolar-des  + acum-vl-merc-liq-dolar-des
                t-acum-vl-despes-it-dolar-des = t-acum-vl-despes-it-dolar-des + acum-vl-despes-it-dolar-des.

         ASSIGN acum-peso-liq-fat-des       = 0
                acum-qt-faturada-des        = 0
                acum-vl-tot-item-des        = 0
                acum-vl-icms-it-des         = 0
                acum-vl-ipi-it-des          = 0
                acum-vl-finsocial-des       = 0
                acum-vl-pis-des             = 0
                acum-vl-merc-liq-des        = 0
                acum-vl-despes-it-des       = 0
                acum-enc-financ-des         = 0
                acum-vl-tot-item-dolar-des  = 0
                acum-vl-icms-it-dolar-des   = 0
                acum-vl-ipi-it-dolar-des    = 0
                acum-vl-finsocial-dolar-des = 0
                acum-vl-pis-dolar-des       = 0
                acum-vl-merc-liq-dolar-des  = 0
                acum-vl-despes-it-dolar-des = 0
                acum-enc-financ-dolar-des   = 0.
      END.
   END. /* do if avail  natur-oper*/
END. /* for each tt-notas*/


/* SE FOR QUANTIDADE E A MOEDA FOR O REAL*/
IF saida = 2 THEN DO:

   ASSIGN hExcel:Range(trim(entry(09,aCel) + string(ilin))):value = "TOTAL GERAL.......:"
          hExcel:Range(trim(entry(10,aCel) + string(ilin))):value = (IF l-item  THEN t-acum-qt-faturada-des  ELSE t-acum-peso-liq-fat-des)
          hExcel:Range(trim(entry(12,aCel) + string(ilin))):value = (IF t-moeda THEN t-acum-vl-tot-item-des  ELSE t-acum-vl-tot-item-dolar-des)
          hExcel:Range(trim(entry(13,aCel) + string(ilin))):value = (IF t-moeda THEN t-acum-vl-icms-it-des   ELSE t-acum-vl-icms-it-dolar-des)
          hExcel:Range(trim(entry(14,aCel) + string(ilin))):value = (IF t-moeda THEN t-acum-vl-ipi-it-des    ELSE t-acum-vl-ipi-it-dolar-des)
          hExcel:Range(trim(entry(15,aCel) + string(ilin))):value = (IF t-moeda THEN t-acum-vl-finsocial-des ELSE t-acum-vl-finsocial-dolar-des)
          hExcel:Range(trim(entry(16,aCel) + string(ilin))):value = (IF t-moeda THEN t-acum-vl-pis-des       ELSE t-acum-vl-pis-dolar-des).
          ASSIGN iColimp = 17.
          IF l-imposto THEN
             ASSIGN hExcel:Range(trim(entry(iColimp,aCel) + string(ilin))):value = (IF t-moeda THEN t-acum-vl-despes-it-des 
                                                                                    ELSE t-acum-vl-despes-it-dolar-des)
                    hExcel:Range(trim(entry(iColimp + 1,aCel) + string(ilin))):value = (IF t-moeda THEN t-acum-enc-financ-des 
                                                                                        ELSE t-acum-enc-financ-dolar-des)
                    iColimp = 19.
          ASSIGN hExcel:Range(trim(entry(iColimp,aCel) + string(ilin))):value = (IF t-moeda THEN t-acum-vl-merc-liq-des 
                                                                                 ELSE t-acum-vl-merc-liq-dolar-des)
                 hExcel:Range(trim(entry(iColimp + 1,aCel) + string(ilin))):value = (IF t-moeda THEN (t-acum-vl-merc-liq-des / 
                                                                                                     (IF l-item THEN t-acum-qt-faturada-des 
                                                                                                      ELSE t-acum-peso-liq-fat-des)) 
                                                                                     ELSE (t-acum-vl-merc-liq-dolar-des / 
                                                                                           (IF l-item THEN t-acum-qt-faturada-des 
                                                                                            ELSE t-acum-peso-liq-fat-des))).
          hExcel:Range(trim(entry(09,aCel) + string(iLin)) + ":" + trim(entry(iColimp + 1,aCel) + string(iLin))):select.
          hExcel:Selection:Font:Bold = True.
          hExcel:Selection:Interior:ColorIndex = 24.
          hExcel:Columns(trim(entry(4,aCel)) + ":" + trim(entry(iColimp + 1,aCel))):autofit.
END.
ELSE IF l-item THEN DO:
   IF r-tipo THEN PUT SKIP(1).
   PUT UNFORMATTED "TOTAL GERAL.......:" AT 49
        t-acum-qt-faturada-des           AT 71 format "->>>,>>>,>>9.999".

   IF t-moeda THEN DO:
      PUT UNFORMATTED 
           t-acum-vl-tot-item-des      AT 092 FORMAT "->>,>>>,>>>,>>9.99"
           t-acum-vl-icms-it-des        AT 110 format "->,>>>,>>>,>>9.99"
           t-acum-vl-ipi-it-des         AT 127 format "->,>>>,>>>,>>9.99"
           t-acum-vl-finsocial-des      AT 144 format "->,>>>,>>>,>>9.99"
           t-acum-vl-pis-des            AT 161 format "->,>>>,>>>,>>9.99".

      IF tt-param.l-imposto THEN
         PUT UNFORMATTED 
              t-acum-vl-despes-it-des AT 178 format "->,>>>,>>>,>>9.99"
              t-acum-enc-financ-des   AT 196 FORMAT "->,>>>,>>>,>>9.99"
              t-acum-vl-merc-liq-des  AT 213 format "->>,>>>,>>>,>>9.99"
              (t-acum-vl-merc-liq-des / t-acum-qt-faturada-des) AT 231 format "->>,>>>,>>>,>>9.9999" SKIP.
      ELSE
         PUT UNFORMATTED 
               t-acum-vl-merc-liq-des  AT 178 FORMAT "->>,>>>,>>>,>>9.99"
              (t-acum-vl-merc-liq-des / t-acum-qt-faturada-des) AT 196 format "->>,>>>,>>>,>>9.9999" SKIP.
   END.
   ELSE DO:
       PUT UNFORMATTED 
           t-acum-vl-tot-item-dolar-des    AT 092 FORMAT "->>,>>>,>>>,>>9.99"
           t-acum-vl-icms-it-dolar-des     AT 110 format "->,>>>,>>>,>>9.99"
           t-acum-vl-ipi-it-dolar-des      AT 127 format "->,>>>,>>>,>>9.99"
           t-acum-vl-finsocial-dolar-des   AT 144 format "->,>>>,>>>,>>9.99"
           t-acum-vl-pis-dolar-des         AT 161 format "->,>>>,>>>,>>9.99".

       IF tt-param.l-imposto THEN
          PUT UNFORMATTED 
               t-acum-vl-despes-it-dolar-des AT 178 format "->,>>>,>>>,>>9.99"
               t-acum-enc-financ-dolar-des   AT 196 FORMAT  "->,>>>,>>>,>>9.99"
               t-acum-vl-merc-liq-dolar-des  AT 213 FORMAT "->>,>>>,>>>,>>9.99"
               (t-acum-vl-merc-liq-dolar-des / t-acum-qt-faturada-des) AT 231 format "->>,>>>,>>>,>>9.9999" SKIP.
       ELSE
          PUT UNFORMATTED 
               t-acum-vl-merc-liq-dolar-des   AT 178 format "->>,>>>,>>>,>>9.99"
               (t-acum-vl-merc-liq-dolar-des / t-acum-qt-faturada-des) AT 196 format "->>,>>>,>>>,>>9.9999" SKIP.
   END.
END.
ELSE DO:   /*SE FOR PESO E A TAXA FOR O real*/
   IF r-tipo THEN PUT SKIP(1).
   PUT UNFORMATTED "TOTAL GERAL.......:" AT 49
        t-acum-peso-liq-fat-des          AT 71 format "->>>,>>>,>>9.999".

   IF t-moeda THEN DO:
      PUT UNFORMATTED 
           t-acum-vl-tot-item-des       AT 092 FORMAT "->>,>>>,>>>,>>9.99"
           t-acum-vl-icms-it-des        AT 110 format "->,>>>,>>>,>>9.99"
           t-acum-vl-ipi-it-des         AT 127 format "->,>>>,>>>,>>9.99"
           t-acum-vl-finsocial-des      AT 144 format "->,>>>,>>>,>>9.99"
           t-acum-vl-pis-des            AT 161 format "->,>>>,>>>,>>9.99".

      IF tt-param.l-imposto THEN
         PUT UNFORMATTED 
              t-acum-vl-despes-it-des AT 178 format "->,>>>,>>>,>>9.99"
              t-acum-enc-financ-des   AT 196 FORMAT "->,>>>,>>>,>>9.99"
              t-acum-vl-merc-liq-des  AT 213 format "->>,>>>,>>>,>>9.99"
              (t-acum-vl-merc-liq-des / t-acum-peso-liq-fat-des) AT 231 format "->>,>>>,>>>,>>9.9999" SKIP.
      ELSE
         PUT UNFORMATTED 
              t-acum-vl-merc-liq-des  AT 178 format "->>,>>>,>>>,>>9.99"
              (t-acum-vl-merc-liq-des / t-acum-peso-liq-fat-des) AT 196 format "->>,>>>,>>>,>>9.9999" SKIP.
   END.
   ELSE DO:
      PUT UNFORMATTED 
          t-acum-vl-tot-item-dolar-des   AT 092 FORMAT "->>,>>>,>>>,>>9.99"
          t-acum-vl-icms-it-dolar-des    AT 110 format "->,>>>,>>>,>>9.99"
          t-acum-vl-ipi-it-dolar-des     AT 127 format "->,>>>,>>>,>>9.99"
          t-acum-vl-finsocial-dolar-des  AT 144 format "->,>>>,>>>,>>9.99"
          t-acum-vl-pis-dolar-des        AT 161 format "->,>>>,>>>,>>9.99".

      IF tt-param.l-imposto THEN
         PUT UNFORMATTED 
            t-acum-vl-despes-it-dolar-des     AT 178 format "->,>>>,>>>,>>9.99"
              t-acum-enc-financ-dolar-des     AT 196 FORMAT "->,>>>,>>>,>>9.99"
              t-acum-vl-merc-liq-dolar-des    AT 213 format "->>,>>>,>>>,>>9.99"
              (t-acum-vl-merc-liq-dolar-des /  t-acum-peso-liq-fat-des) AT 231 format "->>,>>>,>>>,>>9.9999" SKIP.
      ELSE
         PUT UNFORMATTED 
             t-acum-vl-merc-liq-dolar-des  AT 178 format "->>,>>>,>>>,>>9.99"
             (t-acum-vl-merc-liq-dolar-des /  t-acum-peso-liq-fat-des) AT 196 format "->>,>>>,>>>,>>9.9999" SKIP.
   END.
END.


ASSIGN t-acum-peso-liq-fat-des       = 0
       t-acum-qt-faturada-des        = 0
       t-acum-vl-tot-item-des        = 0
       t-acum-vl-icms-it-des         = 0
       t-acum-vl-ipi-it-des          = 0
       t-acum-vl-finsocial-des       = 0
       t-acum-vl-despes-it-des       = 0
       t-acum-vl-pis-des             = 0
       t-acum-vl-merc-liq-des        = 0
       t-acum-enc-financ-des         = 0
       t-acum-vl-tot-item-dolar-des  = 0
       t-acum-vl-icms-it-dolar-des   = 0
       t-acum-vl-ipi-it-dolar-des    = 0
       t-acum-vl-finsocial-dolar-des = 0
       t-acum-vl-despes-it-dolar-des = 0
       t-acum-vl-pis-dolar-des       = 0
       t-acum-vl-merc-liq-dolar-des  = 0
       t-acum-enc-financ-dolar-des   = 0.

IF saida = 2 then do:
    
    IF l-rpw = no  THEN DO:
        c-planilha:SAVE().
        hExcel:VISIBLE = TRUE.      
        RELEASE OBJECT hExcel NO-ERROR.
      
        c-dir-dest = fullname.
        c-dir-dest = "v:\temp".
      
        OS-COPY VALUE(fullname) value(c-dir-dest) .

    END.
    ELSE DO:
        c-arq-dest = c-dir-dest + "esft0012" + string(today,"99-99-9999") + "-" + replace(string(time,"HH:MM:SS"),":","-") + ".xlsx".
        hExcel:ActiveWorkBook:saveas(c-arq-dest,,,,,,).
        hExcel:ActiveWorkBook:CLOSE().
        hExcel:APPLICATION:QUIT.
        RELEASE OBJECT hExcel NO-ERROR.
        put c-arq-dest format "x(200)" skip.
    END.

    IF l-email THEN
        RUN pi-envia-email.
END.
ELSE DO:
    PUT SKIP(5).

    FIND FIRST tt-param NO-ERROR.
    DISP
    
"*-------------------------------SELECAO----------------------------------------------------------*" 
SKIP(1)
    SPACE(30)                     " DE                                      
ATE " SKIP
    SPACE(30)                  "------------                           
--------------"  SKIP(1)
    "   CODIGO ITEM:"      SPACE(15)  tt-param.it-codigo-ini       SPACE(12) 
  tt-param.it-codigo-fim       SKIP
    "   CLIENTE:"          SPACE(15)  tt-param.cod-emitente-ini    SPACE(25) 
  tt-param.cod-emitente-fim    SKIP
    "   FAMILIA:"          SPACE(20)  tt-param.fm-codigo-ini       SPACE(21) 
  tt-param.fm-codigo-fim       SKIP
    "   FAMILIA COMERCIAL" SPACE(10)  tt-param.fm-cod-com-ini      SPACE(20) 
  tt-param.fm-cod-com-fim      SKIP
    "   NOTA:"             SPACE(20)  tt-param.nr-nota-fis-ini     SPACE(24) 
  tt-param.nr-nota-fis-fim     SKIP
    "   CF0:"              SPACE(20)  tt-param.nat-operacao-ini    SPACE(25) 
  tt-param.nat-operacao-fim    SKIP
    "   DATA:"             SPACE(24)  tt-param.dt-emis-nota-ini    SPACE(20) 
  tt-param.dt-emis-nota-fim    SKIP
    "   GRUPO CLIENTE:"    SPACE(15)  tt-param.cod-gr-cli-ini      SPACE(23) 
  tt-param.cod-gr-cli-fim      SKIP
    "   GRUPO ESTOQUE:"    SPACE(14)  tt-param.ge-codigo-ini       SPACE(23) 
  tt-param.ge-codigo-fim       SKIP
    "   CANAL VENDAS:"     SPACE(15)  tt-param.cod-canal-venda-ini SPACE(25) 
  tt-param.cod-canal-venda-fim SKIP
    "   ESTABELECIMENTO:"  SPACE(10)  tt-param.cod-estabel-ini     SPACE(25) 
  tt-param.cod-estabel-fim     SKIP
    "   ESTADO:"           SPACE(20)  tt-param.estado-ini          SPACE(28) 
  tt-param.estado-fim          SKIP(1)
    
"*-------------------------------------------------------------------------------------------------*" 
SKIP(2)
    WITH FRAME SEL DOWN WIDTH 120 3 COL NO-LABEL.

    PUT 
"-----------------PARAMETROS-----------------------------------------------------------------------" 
SKIP.

    DO i = 1 TO 7 :
      ASSIGN   tipo[i] = " ".
    END.

    IF r-tipo THEN
       ASSIGN tipo[1] = "Resumido".
    ELSE
       ASSIGN tipo[1] = "Detalhado".

    IF l-item THEN
       ASSIGN tipo[2] = "Qtd. Item".
    ELSE
       ASSIGN tipo[2] = "Peso Item".

    IF l-nota THEN
       ASSIGN tipo[3] = "Vendas".
    ELSE
       ASSIGN tipo[3] = "Todas".

    IF n-devol THEN
       ASSIGN tipo[4] = "Sim".
    ELSE
       ASSIGN tipo[4] = "Nao".

    IF t-moeda THEN
       ASSIGN tipo[5] = "Real".
    ELSE
       ASSIGN tipo[5] = "Dolar".

    IF l-imposto THEN
       ASSIGN tipo[6] = "Com Imposto".
    ELSE
       ASSIGN tipo[6] = "Sem Imposto".

    IF n-fatur THEN
       ASSIGN tipo[7] = "Notas Antecipadas".
    ELSE
       ASSIGN tipo[7] = "Sem Notas Antecipadas".

    DISP "   TIPO RELATORIO..:"          TIPO[1] FORMAT "X(15)"
         "   IMPRIMIR..:"                TIPO[2] FORMAT "X(15)"
         "   LISTAR NOTAS..:"            TIPO[3] FORMAT "X(15)"
         "   CONSIDERAR DEVOLUCOES..:"   TIPO[4] FORMAT "X(15)"
         "   MOEDA..:"                   TIPO[5] FORMAT "X(15)"
         "   VENDAS..:"                  TIPO[6] FORMAT "X(15)"
         "   CONSIDERAR NOTAS..:"        TIPO[7] FORMAT "X(15)"
    WITH FRAME parametro NO-LABEL DOWN STREAM-IO WIDTH 80 2 COL.
END.

RUN pi-finalizar IN h-acomp.

/*{include/i-rpclo.i}*/
/**************************************************************************
**
** I-RPCLO - Define sa≠da para impressío do relatΩrio - ex. cd9540.i
** Parametros: {&stream} = nome do stream de saida no formato "stream nome"
***************************************************************************/

output {&stream} close.

/* i-rpout */



    IF VALID-HANDLE   (h-cd9500) THEN  delete widget h-cd9500.
 
RETURN "OK".

PROCEDURE pi-consignacao:
           ASSIGN v-icms   = 0                
                  v-ipi    = 0
                  v-vl-tot = 0.

           IF  CAN-FIND (FIRST nfco-natur-oper  /* Se a natureza estiver parametrizada como "Operacao
                com Terceiro, e "Faturamento Consignacao", entao busca o item da remessa */
                      WHERE nfco-natur-oper.nat-operacao = it-nota-fisc.nat-operacao
                        AND nfco-natur-oper.terceiros    = YES  /*Operacao com Terceiros*/
                        AND nfco-natur-oper.tp-oper-terc = 4)   /*Faturamento Consignacao*/ THEN DO:

                FIND FIRST nfco-it-nota-fisc WHERE nfco-it-nota-fisc.cod-estabel = it-nota-fisc.cod-estabel
                                               AND nfco-it-nota-fisc.serie       = it-nota-fisc.serie-docum
                                               AND nfco-it-nota-fisc.nr-nota-fis = it-nota-fisc.nr-docum
                                               AND nfco-it-nota-fisc.it-codigo   = it-nota-fisc.it-codigo
                                               AND nfco-it-nota-fisc.nr-seq-fat  = it-nota-fisc.int-1 NO-LOCK NO-ERROR.
    
                IF  AVAIL nfco-it-nota-fisc THEN
                    ASSIGN v-icms   = (nfco-it-nota-fisc.vl-icms-it  / nfco-it-nota-fisc.peso-liq-fat) * it-nota-fisc.peso-liq-fat
                           v-ipi    = (nfco-it-nota-fisc.vl-ipi-it   / nfco-it-nota-fisc.peso-liq-fat) * it-nota-fisc.peso-liq-fat
                           v-vl-tot = (nfco-it-nota-fisc.vl-tot-item / nfco-it-nota-fisc.peso-liq-fat) * it-nota-fisc.peso-liq-fat.
                
           END.
END PROCEDURE.
                                                                                                                                           
PROCEDURE pi-envia-email . /*customizacao para enviar planilha por email*/

 /* *** Definicao de Variaveis Locais *** */

        DEF VAR c-nome                  LIKE usuar_mestre.nom_usuario           NO-UNDO.
        DEF VAR c-remetente             LIKE usuar_mestre.cod_e_mail_local      NO-UNDO.
        DEF VAR c-responsavel           LIKE usuar_mestre.cod_e_mail_local      NO-UNDO.
        DEFINE VARIABLE i-seq  AS INTEGER     NO-UNDO.
        DEFINE VARIABLE c-anexo AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE c-destino AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE c-linha   AS CHARACTER   NO-UNDO.
        
        DEFINE VARIABLE c-assunto AS CHARACTER   NO-UNDO.      
def var c-notas as char no-undo.
 

  
                         C-NOTAS =  '<HTML>' +  
                            '<HEAD>' +                                                            
                            '<TITLE>' +                                                           
                            '</TITLE>' +                                                          
                            '<META NAME="GENERATOR" Content="Microsoft Visual Studio">'         +
                            '<META HTTP-EQUIV="Content-Type" content="text/html; charset=UTF-8">' +
                            '</HEAD>' + 
                            '<body>' 
                            + '<p> RELATORIO DE FATURAMENTO - esft0012,</p></br>'
                            + '<p>Prezado Usuario,</p>'
                            + '<p> Segue Relatorio gerado ' 
                            + string(TODAY,"99/99/9999") 
                            
                            + "."
                            + "</TABLE></BODY></HTML>".

                         

       
       

          c-destino = "".
          /*c-destino = search("\\ungusb-vap01\Sistemas\DTS\Log_Prd\esft0012-mail.txt").
          if c-destino <> ? then do: 
            input from value(c-destino).
            repeat:
            import unformatted c-linha.
            leave.
            end.
             input close.
             
             c-destino = c-linha.
          end.
          else*/
           do:
                FOR EACH ext_usuar_grp_usuar WHERE 
                    ext_usuar_grp_usuar.ativo AND
                    ext_usuar_grp_usuar.cod_grp_usuar = tt-param.c-grp-usuar   NO-LOCK,
                    EACH  usuar_mestre NO-LOCK WHERE
                       usuar_mestre.cod_usuario = ext_usuar_grp_usuar.cod_usuario .
                    
                    IF usuar_mestre.cod_e_mail_local <> "" THEN 
                        c-destino =  c-destino + "," + usuar_mestre.cod_e_mail_local .

                    IF ext_usuar_grp_usuar.email_alter <> "" THEN 
                        c-destino =  c-destino + "," + ext_usuar_grp_usuar.email_alter .
      
                END.
      
                c-destino = SUBSTRING(c-destino,2,LENGTH(c-destino)).
                
          end.

        
        ASSIGN c-remetente = 'usrtisis@unigel.com.br'.
       
        c-anexo = c-arq-dest.


        FIND FIRST param-global NO-LOCK NO-ERROR.

         /* *** Delecao da Tabela Temporaria *** */

         FOR each tt-envio2 :  
             DELETE tt-envio2. 
         END.      
         FOR EACH tt-mensagem.
             DELETE tt-mensagem.
         END.

         c-assunto =  "ESFT001 - RELATORIO FATURAMENTO".
         
        

        RUN utp/utapi019.p persistent set h-utapi019.

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
               tt-envio2.formato            = "HTML".


     DO i-seq = 1 TO 1:
         CREATE tt-mensagem.
        ASSIGN tt-mensagem.seq-mensagem = i-seq
               tt-mensagem.mensagem     = c-notas.

     END.


        /*OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "envemail.txt").       */
        /*OUTPUT TO VALUE("v:\temp\envemail.txt").  */     

        RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                       INPUT  TABLE tt-mensagem,    
                                       OUTPUT TABLE tt-erros).

        IF  RETURN-VALUE = "NOK" THEN DO:
            FOR EACH tt-erros:
               /*DISP tt-erros WITH 1 COLUMN WIDTH 300.*/  
            END.                               
        END.




        DELETE procedure h-utapi019.


    END PROCEDURE.

                                                                                                                        




PROCEDURE pi-conta-receita.
          c-conta-receita =  "".
          FOR FIRST sumar-ft WHERE
              sumar-ft.ct-conta >= "31" and
              sumar-ft.ct-conta < "32"  and
              sumar-ft.cod-estabel   = nota-fiscal.cod-estabel AND         
              sumar-ft.nr-nota-fis   = nota-fiscal.nr-nota-fis AND
              sumar-ft.serie         = nota-fiscal.serie AND
              sumar-ft.cod-unid-negoc = it-nota-fisc.cod-unid-negoc  NO-LOCK .

             c-conta-receita = sumar-ft.ct-conta.
             c-sc-receita    = sumar-ft.sc-conta.

          END.

         IF c-conta-receita =  "" THEN DO:

             run pi-cd9500 in h-cd9500(nota-fiscal.cod-estabel,
                                      emitente.cod-gr-cli,
                                      rowid(item),
                                      it-nota-fisc.nat-oper,
                                      (IF lContaFtPorCliente THEN string(nota-fiscal.cod-emitente) ELSE it-nota-fisc.serie),
                                      it-nota-fisc.cod-depos,
                                      nota-fiscal.cod-canal-venda,
                                      output r-conta-ft).


                        find conta-ft
                                     where rowid(conta-ft) = r-conta-ft no-lock no-error.

                         IF AVAIL conta-ft THEN
                            assign c-conta-receita = conta-ft.ct-recven
                                   c-sc-receita    = conta-ft.sc-recven.
         END.


     IF c-conta-receita < "31" OR c-conta-receita >= "32" THEN
         assign c-conta-receita = ""
                c-sc-receita    = "".

END PROCEDURE.

PROCEDURE pi-conta-receita-DEV.
          c-conta-receita =  "".
          c-sc-receita    =  "".


        FOR   FIRST estabelec WHERE estabelec.cod-estabel = NOTA-FISCAL.cod-estabel NO-LOCK,
       /* EACH item-doc-est OF docum-est WHERE
             item-doc-est.it-codigo = devol-cli.it-codigo and
             item-doc-est.sequencia = devol-cli.sequencia  NO-LOCK,*/
    FIRST  tit_acr WHERE 
          tit_acr.cdn_cliente  = NOTA-FISCAL.cod-emitente AND
          tit_acr.cod_empresa  = STRING(estabelec.ep-codigo) AND
          tit_acr.cod_espec_docto = "DP" AND
          tit_acr.cod_estab  = NOTA-FISCAL.cod-estabel and
          tit_acr.cod_parcela  = "01" AND
          tit_acr.cod_ser_docto =  devol-cli.serie  AND
          tit_acr.cod_tit_acr   = devol-cli.nr-nota-fis AND
        NOT tit_acr.log_tit_acr_estordo NO-LOCK ,
       EACH movto_tit_acr OF tit_acr /*WHERE index(movto_tit_acr.ind_trans_acr,"implant") > 0*/
                             NO-LOCK ,

       
        each aprop_ctbl_acr OF movto_tit_acr WHERE /*aprop_ctbl_acr.cod_cta_ctbl >= "32" AND 
        aprop_ctbl_acr.cod_cta_ctbl <= "32Z" AND*/
         aprop_ctbl_acr.cod_unid_negoc = it-NOTA-FISC.cod-unid-negoc
        NO-LOCK,
             FIRST conta-contab WHERE conta-contab.ep-codigo = estabelec.ep-codigo and
              conta-contab.conta-contabil =  aprop_ctbl_acr.cod_cta_ctbl + aprop_ctbl_acr.cod_ccusto  
              AND index( conta-contab.titulo,"dev") > 0 NO-LOCK.

            c-conta-receita = aprop_ctbl_acr.cod_cta_ctbl.
            c-sc-receita    = aprop_ctbl_acr.cod_ccusto + "ap".
            
            
END.
                
                
        



     IF c-conta-receita =  "" THEN  


          FOR EACH sumar-ft WHERE
              
              sumar-ft.cod-estabel   = nota-fiscal.cod-estabel AND         
              sumar-ft.nr-nota-fis   = nota-fiscal.nr-nota-fis AND
              sumar-ft.serie         = nota-fiscal.serie AND
              sumar-ft.cod-unid-negoc = it-nota-fisc.cod-unid-negoc  NO-LOCK ,
          FIRST conta-contab WHERE conta-contab.ep-codigo = estabelec.ep-codigo and
              conta-contab.conta-contabil =  sumar-ft.ct-conta 
              AND index( conta-contab.titulo,"dev") > 0 NO-LOCK.

             assign c-conta-receita = sumar-ft.ct-conta + "su"
                    c-sc-receita = sumar-ft.sc-conta.

          END.

         IF c-conta-receita =  "" THEN DO:

             run pi-cd9500 in h-cd9500(nota-fiscal.cod-estabel,
                                      emitente.cod-gr-cli,
                                      rowid(item),
                                      devol-cli.serie-docto,
                                      (IF lContaFtPorCliente THEN string(nota-fiscal.cod-emitente) ELSE it-nota-fisc.serie),
                                      it-nota-fisc.cod-depos,
                                      nota-fiscal.cod-canal-venda,
                                      output r-conta-ft).


                        find conta-ft
                                     where rowid(conta-ft) = r-conta-ft no-lock no-error.

                         IF AVAIL conta-ft THEN
                            ASSIGN c-conta-receita = conta-ft.conta-dev-rec + "ft"
                                   c-sc-receita    = "".
                            
         END.


     IF c-conta-receita < "32" OR c-conta-receita >= "33" THEN DO:
         ASSIGN c-conta-receita = ""
                c-sc-receita    = "".
     END.
         

END PROCEDURE.

PROCEDURE pi-verif-data-util:
    DEF INPUT-OUTPUT PARAM p-data-aux AS DATE NO-UNDO.

    FIND FIRST estabelec 
         WHERE estabelec.cod-estabel = nota-fiscal.cod-estabel NO-LOCK NO-ERROR.

    REPEAT:

        FIND FIRST calen-coml 
             WHERE calen-coml.ep-codigo   = estabelec.ep-codigo
               AND calen-coml.cod-estabel = nota-fiscal.cod-estabel
               AND calen-coml.data        = p-data-aux NO-LOCK NO-ERROR.
         IF AVAIL calen-coml THEN DO:
             IF calen-coml.tipo-dia <> 1 THEN 
                 ASSIGN p-data-aux = p-data-aux + 1.
             ELSE LEAVE.
         END.
         ELSE LEAVE.
    END.

END PROCEDURE.


