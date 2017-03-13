/*****************************************************************************
**
**       Programa: escp0032rp.p
**
**       Data....: 16/03/2005
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Estat°stica de Aceitaá∆o por Mill Roll - Por Quilogramas
**
**       Vers∆o..: 1.00.000 - jrrcampos
**
**
*******************************************************************************/
define buffer empresa for mgmulti.empresa.
define variable c-prog-gerado as character no-undo initial "escp0032RP".

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
    field c-dt-trans-fim       like movto-estoq.dt-trans
    field c-lote-ini           like movto-estoq.lote
    field c-lote-fim           like movto-estoq.lote
    FIELD c-pesq1              AS INTEGER
.

DEFINE TEMP-TABLE tt-filmes
    FIELD ttfil-cod-estabel          LIKE movto-estoq.cod-estabel
    FIELD ttfil-lote                 LIKE movto-estoq.lote
    FIELD ttfil-it-codigo            LIKE movto-estoq.it-codigo
    FIELD ttfil-qt-prod              AS DECIMAL
    FIELD ttfil-qt-aceita            AS DECIMAL
    FIELD ttfil-qt-rejeit            AS DECIMAL
    FIELD ttfil-it-codigo-x          LIKE movto-estoq.it-codigo
    FIELD ttfil-familia-x            LIKE movto-estoq.it-codigo
    INDEX ch-tt-filmes IS PRIMARY UNIQUE  ttfil-cod-estabel
                                          ttfil-lote.


DEFINE TEMP-TABLE tt-defeitos
    FIELD ttdef-cod-estabel          LIKE movto-estoq.cod-estabel
    FIELD ttdef-lote                 LIKE movto-estoq.lote
    FIELD ttdef-it-codigo            LIKE movto-estoq.it-codigo
    FIELD ttdef-cod-def              LIKE lote-res-carac.sequencia
    FIELD ttdef-nr-tabela            LIKE lote-res-carac.nr-tabela
    FIELD ttdef-qt-rejeit            AS DECIMAL
    INDEX ch-tt-defeitos IS PRIMARY UNIQUE  ttdef-cod-estabel
                                            ttdef-lote
                                            ttdef-cod-def.


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
def new shared var c-cod-estabel-ini  like movto-estoq.cod-estabel format "x(3)" initial "" no-undo.
def new shared var c-dt-trans-ini     like movto-estoq.dt-trans    format "99/99/9999" initial "01/01/2005" no-undo.
def new shared var c-dt-trans-fim     like movto-estoq.dt-trans    format "99/99/9999" initial today no-undo.
def new shared var c-lote-ini         like movto-estoq.lote        format "x(15)" initial "" no-undo.
def new shared var c-lote-fim         like movto-estoq.lote        format "x(15)" initial "ZZZZZZZZZZZZZZZ" no-undo.


/****************** Definiáao de Vari†veis p/ Campos Virtuais do Relat¢rio *******************/ 
DEFINE VARIABLE lote-mr       LIKE movto-estoq.lote        NO-UNDO.
DEFINE VARIABLE lote-req      LIKE movto-estoq.lote        NO-UNDO.
DEFINE VARIABLE lote-mr1      AS CHAR                      NO-UNDO.
DEFINE VARIABLE lote-mr2      AS CHAR                      NO-UNDO.
DEFINE VARIABLE lote-mr3      AS CHAR                      NO-UNDO.
DEFINE VARIABLE lote-mr4      AS CHAR                      NO-UNDO.
DEFINE VARIABLE lote-mr5      AS CHAR                      NO-UNDO.
DEFINE VARIABLE fim-mr        AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE saldo-bobina  AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE item-mr       LIKE ped-item.it-codigo      NO-UNDO.
DEFINE VARIABLE cmkt-req   LIKE movto-estoq.it-codigo   NO-UNDO.
DEFINE VARIABLE ordpro-req LIKE movto-estoq.nr-ord-prod NO-UNDO.
DEFINE VARIABLE numseq-req LIKE movto-estoq.num-sequen  NO-UNDO.
DEFINE VARIABLE linha-mr   LIKE ord-prod.nr-linha       NO-UNDO.
DEFINE VARIABLE flag-erro  AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE cod-def    AS INTEGER                   NO-UNDO.
DEFINE VARIABLE qtdbob-jr  AS INTEGER                   NO-UNDO.
DEFINE VARIABLE c-def-jr   AS INTEGER                   NO-UNDO.
DEFINE VARIABLE cdef1-jr   AS INTEGER                   NO-UNDO.
DEFINE VARIABLE cdef2-jr   AS INTEGER                   NO-UNDO.
DEFINE VARIABLE cdef3-jr   AS INTEGER                   NO-UNDO.
DEFINE VARIABLE qdef1-jr   AS decimal                   NO-UNDO.
DEFINE VARIABLE qdef2-jr   AS decimal                   NO-UNDO.
DEFINE VARIABLE qdef3-jr   AS decimal                   NO-UNDO.
DEFINE VARIABLE per-ac     AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE DESC-def   AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE per-ac-def AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE conta-lin  AS INTEGER                   NO-UNDO.
DEFINE VARIABLE ntab1-jr   AS INT                       NO-UNDO.
DEFINE VARIABLE ntab2-jr   AS INT                       NO-UNDO.
DEFINE VARIABLE ntab3-jr   AS INT                       NO-UNDO.
DEFINE VARIABLE ntab0-jr   AS INT                       NO-UNDO.
DEFINE VARIABLE qtdbob-j9  AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE qdef1-j9   AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE qdef2-j9   AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE qdef3-j9   AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE pesq-jr1   AS INT                       NO-UNDO.
DEFINE VARIABLE chave-item AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE filme-jr   AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE mill-roll-jr AS CHARACTER FORMAT "x(13)" NO-UNDO.

DEFINE VARIABLE esp-jr     AS INTEGER                   NO-UNDO.

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

/****************** Definiáao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form ttfil-cod-estabel column-label "Est" format "x(3)" at 001
     ttfil-lote        COLUMN-LABEL "Lote" FORMAT "x(13)" AT 5
     ttfil-it-codigo   column-label "Filme" format "x(16)" at 020
     ttfil-qt-prod     COLUMN-LABEL "Produzido(Kg)" FORMAT "->>>,>>>,>>9.99" AT 038
     ttfil-qt-aceita   COLUMN-LABEL "Aceitas(Kg)"    FORMAT "->>>,>>>,>>9.99" AT 054
     ttfil-qt-rejeit   COLUMN-LABEL "Rejeitadas(kg)" FORMAT "->>>,>>>,>>9.99" AT 073
     per-ac            COLUMN-LABEL "% Aceit."     FORMAT "->>>>9.99" AT 090
     with down width 132 no-box stream-io frame f-relat-09-132.


FORM
    ttdef-cod-def   FORMAT ">>9" AT 22
    DESC-def        FORMAT "x(20)" AT 38
    ttdef-qt-rejeit FORMAT "->>>,>>>,>>9.99" AT 73
    per-ac-def      FORMAT "->>>>9.99" AT 90
    WITH DOWN WIDTH 132 NO-BOX STREAM-IO FRAME f-relat-defeitos.

form HEADER
    fill("-", 132) format "x(132)" SKIP 
    WITH DOWN WIDTH 132 NO-BOX STREAM-IO FRAME f-relat-linha.

form HEADER
    fill(" ", 132) format "x(132)" SKIP 
    WITH DOWN WIDTH 132 NO-BOX STREAM-IO FRAME f-relat-branco.

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

assign c-programa     = "escp0032rp"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Estat.de Aceitaá∆o por Mill Roll - Por Quilogramas"
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
    "Per°odo..:" AT 001 c-dt-trans-ini "  a  " c-dt-trans-fim SKIP
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
           c-dt-trans-ini     = tt-param.c-dt-trans-ini   
           c-dt-trans-fim     = tt-param.c-dt-trans-fim   
           c-lote-ini         = tt-param.c-lote-ini       
           c-lote-fim         = tt-param.c-lote-fim       
           c-pesq1            = tt-param.c-pesq1          
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
        assign v-cod-destino-impres = "Terminal".


run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

FOR EACH tt-filmes:
            DELETE tt-filmes.
END.

FOR EACH tt-defeitos:
            DELETE tt-defeitos.
END.


ASSIGN esp-jr = 1.

DO WHILE esp-jr = 1 OR esp-jr = 8 :
  
  for each movto-estoq no-lock
         where movto-estoq.esp-docto = esp-jr AND
               movto-estoq.cod-estabel  = c-cod-estabel-ini and 
               movto-estoq.dt-trans  >= c-dt-trans-ini  and 
               movto-estoq.dt-trans  <= c-dt-trans-fim  and 
               movto-estoq.lote <> "recicl"   AND 
               movto-estoq.cod-depos <> "ARC" AND
               movto-estoq.quantidade <> 0
               USE-INDEX esp-data.
    
    FIND FIRST ITEM WHERE ITEM.it-codigo = movto-estoq.it-codigo
         USE-INDEX codigo NO-LOCK NO-ERROR.

    IF NOT AVAIL ITEM THEN NEXT.

   /* IF ITEM.GE-codigo < 55 OR item.GE-codigo > 69 THEN NEXT.  BR*/

    IF ITEM.GE-codigo < 46 OR item.GE-codigo > 49 THEN NEXT.
    

/* Rotina de Rastreabilidade para obter o mill roll de origem.  */

    ASSIGN fim-mr   = "X".
    ASSIGN lote-mr  = "X".
    ASSIGN lote-mr1 = "X".
    ASSIGN lote-mr2 = "X".
    ASSIGN lote-mr3 = "X".
    ASSIGN lote-mr4 = "X".
    ASSIGN lote-mr5 = "X".
    assign mill-roll-jr = "".
    ASSIGN saldo-bobina = movto-estoq.quantidade.
    
    FIND FIRST movto-mat WHERE
               movto-mat.nr-ord-prod = movto-estoq.nr-ord-prod and
               movto-mat.num-sequen = (movto-estoq.num-sequen - 1)
               USE-INDEX num-seq NO-LOCK NO-ERROR.
               
    IF AVAIL movto-mat AND movto-mat.esp-docto = 28 THEN do: 

       ASSIGN lote-req = movto-mat.lote.
       ASSIGN cmkt-req = movto-mat.it-codigo.  

      FIND FIRST movto-mat WHERE
               movto-mat.it-codigo = cmkt-req and
               movto-mat.lote = lote-req AND
               movto-mat.esp-docto = 1
               USE-INDEX lote NO-LOCK NO-ERROR.
                
      IF NOT AVAIL movto-mat THEN NEXT.

      ASSIGN lote-mr = "X".
      ASSIGN flag-erro = "X".
    
      DO WHILE lote-mr = "X" AND flag-erro = "X" :
       
         FIND FIRST ord-prod WHERE
            ord-prod.nr-ord-produ = movto-mat.nr-ord-produ AND
            ord-prod.it-codigo = movto-mat.it-codigo AND
            ord-prod.cod-estabel = movto-mat.cod-estabel
            USE-INDEX estabel NO-LOCK NO-ERROR.

         IF NOT AVAIL ord-prod THEN 
            ASSIGN flag-erro = "erro". 

         IF ord-prod.nr-linha > 0 and ord-prod.nr-linha < 100 THEN 
            assign mill-roll-jr = movto-mat.lote. 
          
         IF ord-prod.nr-linha < 100 AND flag-erro = "X" THEN DO:
            ASSIGN lote-mr = movto-mat.lote.
            assign item-mr = movto-mat.it-codigo.
            ASSIGN lote-mr5 = movto-mat.lote. 
         END.
       
         IF lote-mr = "X" AND flag-erro = "X" THEN DO:

             IF lote-mr1 = "X" THEN
                ASSIGN lote-mr1 = movto-mat.lote.
                         
             IF lote-mr1 <> "X" AND lote-mr1 <> movto-mat.lote THEN
                ASSIGN lote-mr2 = movto-mat.lote.
                         
             IF lote-mr2 <> "X" AND lote-mr2 <> movto-mat.lote THEN
                ASSIGN lote-mr3 = movto-mat.lote.
                         
             IF lote-mr3 <> "X" AND lote-mr3 <> movto-mat.lote THEN
                ASSIGN lote-mr4 = movto-mat.lote.
                         
             IF lote-mr4 <> "X" AND lote-mr4 <> movto-mat.lote THEN
                ASSIGN lote-mr5 = movto-mat.lote.

             ASSIGN ordpro-req = movto-mat.nr-ord-prod.
             ASSIGN numseq-req = (movto-mat.num-sequen - 1).

             FIND FIRST movto-mat WHERE
                movto-mat.nr-ord-prod = ordpro-req and
                movto-mat.num-sequen = numseq-req AND
                movto-mat.esp-docto = 28
                USE-INDEX num-seq NO-LOCK NO-ERROR.

             IF NOT AVAIL movto-mat THEN
                ASSIGN flag-erro = "erro".
              
             IF flag-erro = "X" THEN DO:
            
                ASSIGN lote-req = movto-mat.lote.
                ASSIGN cmkt-req = movto-mat.it-codigo.

                FIND FIRST movto-mat WHERE
                    movto-mat.it-codigo = cmkt-req and
                    movto-mat.lote = lote-req AND
                    movto-mat.esp-docto = 1
                    USE-INDEX lote NO-LOCK NO-ERROR.

                 IF NOT AVAIL movto-mat THEN 
                 ASSIGN flag-erro = "erro".
                 
             END.
         END.
      END. 
    END.
    
/* Fim da Rotina de Rastreabilidade */    

    IF mill-roll-jr < c-lote-ini OR mill-roll-jr > c-lote-fim THEN NEXT.

    ASSIGN qtdbob-jr = 1.
    ASSIGN cdef1-jr  = 0.
    ASSIGN cdef2-jr  = 0.
    ASSIGN cdef3-jr  = 0.
    ASSIGN qdef1-jr  = 0.
    ASSIGN qdef2-jr  = 0.
    ASSIGN qdef3-jr  = 0.
    ASSIGN cod-def   = 0.
    ASSIGN ntab0-jr  = 0.
    ASSIGN ntab1-jr  = 0.
    ASSIGN ntab2-jr  = 0.
    ASSIGN ntab3-jr  = 0.

    FIND FIRST lote-carac-tec WHERE
                        lote-carac-tec.it-codigo = movto-estoq.it-codigo
                        and lote-carac-tec.lote = movto-estoq.lote
                        and lote-carac-tec.cd-comp = "qtdbob"
                        NO-LOCK NO-ERROR.

    if avail lote-carac-tec then
       ASSIGN qtdbob-jr = lote-carac-tec.vl-resul.

    IF qtdbob-jr < 1 THEN
       ASSIGN qtdbob-jr = 1.

    IF qtdbob-jr <> 1 THEN DO:

       FIND FIRST lote-carac-tec WHERE
                        lote-carac-tec.it-codigo = movto-estoq.it-codigo
                        and lote-carac-tec.lote = movto-estoq.lote
                        and lote-carac-tec.cd-comp = "qdef1"
                        NO-LOCK NO-ERROR.
       
       if avail lote-carac-tec then
          ASSIGN qdef1-jr = lote-carac-tec.vl-resul.

       IF qdef1-jr > 0 THEN DO:

          FIND FIRST lote-carac-tec WHERE
                         lote-carac-tec.it-codigo = movto-estoq.it-codigo
                         and lote-carac-tec.lote = movto-estoq.lote
                         and lote-carac-tec.cd-comp = "cdef1"
                         NO-LOCK NO-ERROR.

          if avail lote-carac-tec then DO:
           
          FIND FIRST lote-res-carac
               where lote-res-carac.cd-folha = lote-carac-tec.cd-folha and
               lote-res-carac.it-codigo = lote-carac-tec.it-codigo and
               lote-res-carac.lote = lote-carac-tec.lote and
               lote-res-carac.nr-tabela = lote-carac-tec.nr-tabela and
               lote-res-carac.sequencia <> 0 and
               lote-res-carac.cd-comp = lote-carac-tec.cd-comp
               NO-LOCK NO-ERROR.

               IF AVAIL lote-res-carac THEN DO:
                   assign cdef1-jr = lote-res-carac.sequencia.
                   assign ntab1-jr = lote-res-carac.nr-tabela.
               END.


          END.
       END.

       FIND FIRST lote-carac-tec WHERE
                        lote-carac-tec.it-codigo = movto-estoq.it-codigo
                        and lote-carac-tec.lote = movto-estoq.lote
                        and lote-carac-tec.cd-comp = "qdef2"
                        NO-LOCK NO-ERROR.
       
       if avail lote-carac-tec then
          ASSIGN qdef2-jr = lote-carac-tec.vl-resul.

       IF qdef2-jr > 0 THEN DO:

          FIND FIRST lote-carac-tec WHERE
                          lote-carac-tec.it-codigo = movto-estoq.it-codigo
                          and lote-carac-tec.lote = movto-estoq.lote
                          and lote-carac-tec.cd-comp = "cdef2"
                          NO-LOCK NO-ERROR.

          if avail lote-carac-tec then DO:

          FIND FIRST lote-res-carac
               where lote-res-carac.cd-folha = lote-carac-tec.cd-folha and
               lote-res-carac.it-codigo = lote-carac-tec.it-codigo and
               lote-res-carac.lote = lote-carac-tec.lote and
               lote-res-carac.nr-tabela = lote-carac-tec.nr-tabela and
               lote-res-carac.sequencia <> 0 and
               lote-res-carac.cd-comp = lote-carac-tec.cd-comp
               NO-LOCK NO-ERROR.

               IF AVAIL lote-res-carac THEN DO:
                   assign cdef2-jr = lote-res-carac.sequencia.
                   assign ntab2-jr = lote-res-carac.nr-tabela.
               END.

          END.
       END.

       FIND FIRST lote-carac-tec WHERE
                        lote-carac-tec.it-codigo = movto-estoq.it-codigo
                        and lote-carac-tec.lote = movto-estoq.lote
                        and lote-carac-tec.cd-comp = "qdef3"
                        NO-LOCK NO-ERROR.
       
       if avail lote-carac-tec then
          ASSIGN qdef3-jr = lote-carac-tec.vl-resul.

       IF qdef3-jr > 0 THEN DO:

          FIND FIRST lote-carac-tec WHERE
                          lote-carac-tec.it-codigo = movto-estoq.it-codigo
                          and lote-carac-tec.lote = movto-estoq.lote
                          and lote-carac-tec.cd-comp = "cdef3"
                          NO-LOCK NO-ERROR.

          if avail lote-carac-tec then DO:

          FIND FIRST lote-res-carac
               where lote-res-carac.cd-folha = lote-carac-tec.cd-folha and
               lote-res-carac.it-codigo = lote-carac-tec.it-codigo and
               lote-res-carac.lote = lote-carac-tec.lote and
               lote-res-carac.nr-tabela = lote-carac-tec.nr-tabela and
               lote-res-carac.sequencia <> 0 and
               lote-res-carac.cd-comp = lote-carac-tec.cd-comp
               NO-LOCK NO-ERROR.

               IF AVAIL lote-res-carac THEN DO:
                   assign cdef3-jr = lote-res-carac.sequencia.
                   assign ntab3-jr = lote-res-carac.nr-tabela.
               END.

          END.
       END.

    END.

    IF qdef1-jr = 0  THEN ASSIGN cdef1-jr = 0.
    IF qdef2-jr = 0  THEN ASSIGN cdef2-jr = 0.
    IF qdef3-jr = 0  THEN ASSIGN cdef3-jr = 0.

    IF qtdbob-jr > 0 THEN DO:
   
       FIND FIRST lote-carac-tec WHERE
                        lote-carac-tec.it-codigo = movto-estoq.it-codigo
                        and lote-carac-tec.lote = movto-estoq.lote
                        and lote-carac-tec.cd-comp = "defpri"
                        NO-LOCK NO-ERROR.

       if avail lote-carac-tec then do:
                            
          FIND FIRST lote-res-carac
               where lote-res-carac.cd-folha = lote-carac-tec.cd-folha and
               lote-res-carac.it-codigo = lote-carac-tec.it-codigo and
               lote-res-carac.lote = lote-carac-tec.lote and
               lote-res-carac.nr-tabela = lote-carac-tec.nr-tabela and
               lote-res-carac.sequencia <> 0 and
               lote-res-carac.cd-comp = lote-carac-tec.cd-comp
               NO-LOCK NO-ERROR.

          IF AVAIL lote-res-carac THEN DO:
                   assign cod-def  = lote-res-carac.sequencia.
                   assign ntab0-jr = lote-res-carac.nr-tabela.
          END.
                   
       END.
    END.

    
    IF movto-estoq.esp-docto = 1 THEN DO:
       ASSIGN qtdbob-j9 = saldo-bobina
              qdef1-j9  = qdef1-jr
              qdef2-j9  = qdef2-jr
              qdef3-j9  = qdef3-jr.
    END.

    IF movto-estoq.esp-docto = 8 THEN DO:
       ASSIGN qtdbob-j9 = saldo-bobina * -1
              qdef1-j9  = qdef1-jr * -1
              qdef2-j9  = qdef2-jr * -1
              qdef3-j9  = qdef3-jr * -1.
    END.


    FIND FIRST tt-filmes WHERE
         ttfil-cod-estabel = movto-estoq.cod-estabel AND
         ttfil-lote        = mill-roll-jr  
         USE-INDEX ch-tt-filmes NO-ERROR.

    IF NOT AVAIL tt-filmes THEN DO:
       CREATE tt-filmes.
       ASSIGN ttfil-cod-estabel = movto-estoq.cod-estabel.
       ASSIGN ttfil-lote        = mill-roll-jr.
    END.

    ASSIGN ttfil-it-codigo-x = movto-estoq.it-codigo.   
    ASSIGN ttfil-it-codigo   = movto-estoq.it-codigo.   

    IF qtdbob-jr > 0 THEN DO:
       ASSIGN ttfil-qt-prod = ttfil-qt-prod + qtdbob-j9.
       IF cod-def = 0 THEN
          ASSIGN ttfil-qt-aceita = ttfil-qt-aceita + qtdbob-j9.
        ELSE
          ASSIGN ttfil-qt-rejeit = ttfil-qt-rejeit + qtdbob-j9.
    END.


    /* Grava Temp-table do total da linha */

    FIND FIRST tt-filmes WHERE
         ttfil-cod-estabel = movto-estoq.cod-estabel AND
         ttfil-lote        = "x-TOTAL GERAL"  
         USE-INDEX ch-tt-filmes NO-ERROR.

    IF NOT AVAIL tt-filmes THEN DO:
       CREATE tt-filmes.
       ASSIGN ttfil-cod-estabel = movto-estoq.cod-estabel.
       ASSIGN ttfil-lote        = "x-TOTAL GERAL".
    END.

    IF qtdbob-jr > 0 THEN DO:
       ASSIGN ttfil-qt-prod = ttfil-qt-prod + qtdbob-j9.
       IF cod-def = 0 THEN
          ASSIGN ttfil-qt-aceita = ttfil-qt-aceita + qtdbob-j9.
        ELSE
          ASSIGN ttfil-qt-rejeit = ttfil-qt-rejeit + qtdbob-j9.
    END.


    IF qtdbob-jr > 0 AND cod-def > 0 THEN DO:

       ASSIGN c-def-jr = cod-def.
       RUN agrupa-defeitos.
       ASSIGN cod-def = c-def-jr.   
       
       FIND FIRST tt-defeitos WHERE
          ttdef-cod-estabel = movto-estoq.cod-estabel AND
          ttdef-lote        = mill-roll-jr AND
          ttdef-cod-def = cod-def
          USE-INDEX ch-tt-defeitos NO-ERROR.

       IF NOT AVAIL tt-defeitos THEN DO:
          CREATE tt-defeitos.
          ASSIGN ttdef-cod-estabel = movto-estoq.cod-estabel.
          ASSIGN ttdef-lote        = mill-roll-jr.
          ASSIGN ttdef-nr-tabela = ntab0-jr.
          ASSIGN ttdef-cod-def = cod-def.
       END.

       ASSIGN ttdef-qt-rejeit = ttdef-qt-rejeit + qtdbob-j9.

    END. 

    run pi-acompanhar in h-acomp (input movto-estoq.it-codigo ). 
 
  END.
  
  IF esp-jr = 1 THEN ASSIGN esp-jr = 8.
  ELSE
      ASSIGN esp-jr = 99.

END.



FOR EACH tt-filmes NO-LOCK:

    ASSIGN per-ac = (ttfil-qt-aceita / ttfil-qt-prod) * 100.

    
    /***  C‡DIGO PARA SA÷DA EM 132 COLUNAS ***/

        IF conta-lin <> 9 THEN DO:
           view stream str-rp frame f-cabec.
           view stream str-rp frame f-rodape.
           assign l-imprime = yes.
           display stream str-rp 
           with stream-io frame f-relat-branco.
           down stream str-rp with frame f-relat-branco.
       END.

       IF ttfil-it-codigo = "x-TOTAL GERAL" THEN  DO:
          view stream str-rp frame f-cabec.
          view stream str-rp frame f-rodape.
          assign l-imprime = yes.
          display stream str-rp 
          with stream-io frame f-relat-linha.
          down stream str-rp with frame f-relat-linha.

       END.

        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp 
            ttfil-cod-estabel
            ttfil-lote
            ttfil-it-codigo
            ttfil-qt-prod
            ttfil-qt-aceita
            ttfil-qt-rejeit
            per-ac
            with stream-io frame f-relat-09-132.
            down stream str-rp with frame f-relat-09-132.


        IF ttfil-it-codigo = "x-TOTAL GERAL" THEN  DO:
           view stream str-rp frame f-cabec.
           view stream str-rp frame f-rodape.
           assign l-imprime = yes.
           display stream str-rp 
           with stream-io frame f-relat-linha.
           down stream str-rp with frame f-relat-linha.
           
        END.

        ASSIGN conta-lin = 0.

    FOR EACH tt-defeitos NO-LOCK WHERE
             ttdef-cod-estabel = ttfil-cod-estabel AND
             ttdef-lote        = ttfil-lote :
        
        ASSIGN per-ac-def = (ttdef-qt-rejeit / ttfil-qt-prod) * 100.

        ASSIGN conta-lin = 9.

        FIND FIRST c-tab-res
                where c-tab-res.nr-tabela = ttdef-nr-tabela AND
                      c-tab-res.sequencia = ttdef-cod-def
                      NO-LOCK NO-ERROR.

        IF AVAIL c-tab-res THEN
           DESC-def = c-tab-res.descricao.
        ELSE
           DESC-def = "".

        IF ttfil-cod-estabel = "423" OR ttfil-cod-estabel = "413" THEN DO: /*solic-318*/
            IF ttdef-cod-def = 1 THEN
                DESC-def = "PROCESSOS".

            ELSE IF  ttdef-cod-def = 2 THEN
                DESC-def = "DIMENSIONAL".

            ELSE IF  ttdef-cod-def = 3 THEN
                DESC-def = "BOBINAMENTO".

            ELSE IF  ttdef-cod-def = 4 THEN
                DESC-def = "GEOMETRIA".

            ELSE IF  ttdef-cod-def = 5 THEN
                DESC-def = "APAR“NCIA/OTICOS".

            ELSE IF  ttdef-cod-def = 6 THEN
                DESC-def = "PLANURA".

            ELSE IF  ttdef-cod-def = 7 THEN
                DESC-def = "SUPERF÷CIE".

            ELSE IF  ttdef-cod-def = 8 THEN
                DESC-def = "OUTROS".
        END.

        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp 
            ttdef-cod-def    NO-LABEL
            desc-def         NO-LABEL
            ttdef-qt-rejeit  NO-LABEL
            per-ac-def       NO-LABEL
            with stream-io frame f-relat-defeitos.
            down stream str-rp with frame f-relat-defeitos.
      
    END.

    IF conta-lin <> 0 THEN DO:
       view stream str-rp frame f-cabec.
       view stream str-rp frame f-rodape.
       assign l-imprime = yes.
       display stream str-rp 
       with stream-io frame f-relat-linha.
       down stream str-rp with frame f-relat-linha.
       END.

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
      c-cod-estabel-ini colon 20 
        with stream-io side-labels overlay row 032 frame f-imp-sel.

   disp stream str-rp "CLASSIFICAÄ«O" skip(01) with stream-io frame f-imp-cla.
   disp stream str-rp "   Estat.de Aceitaá∆o por Mill Roll por Bobinas"
        with stream-io side-labels overlay row 032 frame f-imp-cla.

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

PROCEDURE agrupa-defeitos.

  IF c-cod-estabel-ini = "423" AND tt-param.c-pesq1 = 1 THEN DO:

    IF (c-def-jr = 7 OR c-def-jr = 83) THEN  
        ASSIGN c-def-jr = 1.

      ELSE
       IF (c-def-jr = 75 OR c-def-jr = 47 OR c-def-jr = 63 OR c-def-jr = 89) THEN  
           ASSIGN c-def-jr = 2.

       ELSE
        IF (c-def-jr = 99 OR c-def-jr = 91 OR c-def-jr = 59 OR c-def-jr = 113 OR
            c-def-jr = 11 OR c-def-jr = 13 OR c-def-jr = 51 OR c-def-jr = 67 OR
            c-def-jr = 17 OR c-def-jr = 33)  THEN  
            ASSIGN c-def-jr = 3.

        ELSE
         IF (c-def-jr = 77 OR c-def-jr = 65 OR c-def-jr = 45)  THEN  
             ASSIGN c-def-jr = 4.

         ELSE
          IF (c-def-jr = 3 OR c-def-jr = 35 OR c-def-jr = 37 OR c-def-jr = 39 OR
              c-def-jr = 69 OR c-def-jr = 79)  THEN  
              ASSIGN c-def-jr = 5.  

          ELSE
           IF (c-def-jr = 15 OR c-def-jr = 111 OR c-def-jr = 21 OR c-def-jr = 55) THEN  
               ASSIGN c-def-jr = 6.

           ELSE
            IF (c-def-jr = 123 OR c-def-jr = 105 OR c-def-jr = 103 OR c-def-jr = 93 OR
               c-def-jr = 41)  THEN  
               ASSIGN c-def-jr = 7.

           ELSE
               ASSIGN c-def-jr = 8.

  END.

END PROCEDURE.

return 'OK'.

/* fim do programa */
