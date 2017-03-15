/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESPD0067RP 1.00.00.000}  /*** 010000 ***/
/*****************************************************************************
**
**       Programa: espd0067rp.p
**
**       Data....: 13/08/2007
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Relat¢rio de Pedidos a Embarcar
**
**       Vers∆o..: 1.00.003 - Rodrigo
**
**       OBS.....: Trocado nomenclatura de mk002po para padrao de especifico 
**
*******************************************************************************/
def buffer empresa for mgmulti.empresa.

DEF NEW GLOBAL SHARED VAR c-dir-spool-servid-exec AS CHAR NO-UNDO.

/****************** Definiá∆o de Tabelas Tempor†rias do Relat¢rio **********************/
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
  field c-cod-estabel-ini    like ped-venda.cod-estabel
  field c-cod-estabel-fim    like ped-venda.cod-estabel
  field data-ini             like ped-item.dt-entorig
  field data-fim             like ped-item.dt-entorig
  field c-tp-pedido-ini      like ped-venda.tp-pedido
  field c-tp-pedido-fim      like ped-venda.tp-pedido
  field c-nome-abrev-ini     like ped-venda.nome-abrev
  field c-nome-abrev-fim     like ped-venda.nome-abrev
  field i-cod-emitente-ini   like ped-venda.cod-emitente
  field i-cod-emitente-fim   like ped-venda.cod-emitente
  field c-it-codigo-ini      like ped-item.it-codigo
  field c-it-codigo-fim      like ped-item.it-codigo
  FIELD da-codrep-ini        LIKE repres.cod-rep
  FIELD da-codrep-fim        LIKE repres.cod-rep
  FIELD rs-mercado           AS INTEGER
  field da-canal-venda-ini   like nota-fiscal.cod-canal-venda
  field da-canal-venda-fim   like nota-fiscal.cod-canal-venda
  FIELD c-perc-atend         AS DEC
  field l-unig-com           as logical
  field l-simula-embarque    as logical
  field dt-embarque          AS DATE
  FIELD l-email              AS LOG
  FIELD c-grp-usuar          AS CHAR.

DEFINE TEMP-TABLE tt-digita NO-UNDO
  FIELD nome-abrev   LIKE emitente.nome-abrev
  FIELD cod-emitente LIKE emitente.cod-emitente
  INDEX ch-nome-abrev IS PRIMARY nome-abrev.

def temp-table tt-raw-digita
    field raw-digita as raw.

DEFINE TEMP-TABLE tt-param-aux      LIKE tt-param.

DEFINE TEMP-TABLE tt-notas NO-UNDO
    FIELD nro-docto    LIKE saldo-terc.nro-docto
    FIELD sequencia    LIKE saldo-terc.sequencia
    FIELD saldo        LIKE saldo-terc.quantidade
    FIELD cod-emitente LIKE saldo-terc.cod-emitente
    FIELD serie        LIKE saldo-terc.serie
    FIELD cod-estabel  LIKE saldo-terc.cod-estabel
    FIELD it-codigo    LIKE saldo-terc.it-codigo
    INDEX chave-notas IS PRIMARY UNIQUE
            nro-docto   
            sequencia   
            cod-emitente
            serie       
            cod-estabel.
                 
DEFINE TEMP-TABLE tt-lotes NO-UNDO
    FIELD nro-docto    LIKE saldo-terc.nro-docto
    FIELD serie        LIKE saldo-terc.serie
    FIELD sequencia    LIKE saldo-terc.sequencia
    FIELD lote         LIKE saldo-terc.lote
    FIELD remessa      LIKE saldo-terc.quantidade
    FIELD retorno      LIKE saldo-terc.quantidade
    FIELD saldo        LIKE saldo-terc.quantidade
    FIELD cod-estabel  LIKE saldo-terc.cod-estabel
    FIELD cod-emitente LIKE saldo-terc.cod-emitente
    FIELD it-codigo    LIKE saldo-terc.it-codigo
    INDEX chave-lotes IS PRIMARY UNIQUE    it-codigo
                    lote
                    nro-docto
                    sequencia
                    cod-emitente
                    cod-estabel
                    serie.

DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

{utp/utapi019.i}
{btb/btb912zb.i}
{utp/ut-glob.i}

/****************** Definiáao de buffer *********************/ 
DEF BUFFER bf-saldo-estoq  FOR saldo-estoq.
DEF BUFFER bf1-saldo-estoq FOR saldo-estoq.
def buffer bf-ped-venda    for ped-venda.
def buffer bf-ped-item     for ped-item.
def buffer if-ped-venda    for if-ped-venda.
def buffer b-ped-venda     for ped-venda.
def buffer bf-if-ped-venda for if-ped-venda.

/****************** Definiáao de Vari†veis p/ Campos Virtuais do Relat¢rio *******************/ 
def var c-est as char initial "412,422,434,432,442,443" no-undo.
def var i-estab as integer no-undo.

DEFINE VARIABLE nome-ab-rep-jr     AS CHARACTER NO-UNDO.
DEFINE VARIABLE codrep-jr          AS INTEGER   NO-UNDO.
DEFINE VARIABLE merc-jr            AS CHAR      NO-UNDO.
DEFINE VARIABLE saldo-atend        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE tem-transf-jr      AS CHARACTER NO-UNDO.
DEFINE VARIABLE nr-pedcli-jr       AS CHARACTER NO-UNDO.
DEFINE VARIABLE linha-jr           AS CHARACTER NO-UNDO.
DEFINE VARIABLE tem-data-jr        AS DATE      NO-UNDO.
DEFINE VARIABLE saldo-pedido       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE c-nome-plan-ped    AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nome-plan-fat    AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nome-plan-mes    AS CHARACTER NO-UNDO.

/****************** Definiáao de Vari†veis Campo do Layout do Relat¢rio **********************/ 

/****************** Definiáao de Vari†veis do Relat¢rio N∆o Pedidas em Tela ******************/ 

/****************** Definiáao de Vari†veis de Total do Relat¢rio *****************************/ 

def var de-qt-atendida-tt-002 like ped-item.qt-atendida no-undo.
def var de-qt-pedida-tt-001   like ped-item.qt-pedida   no-undo.
def var qt-atendida-jr        like ped-item.qt-atendida no-undo.
def var qt-pedida-jr          like ped-item.qt-pedida   no-undo.
DEF VAR d-perc-atend          AS DEC                    NO-UNDO.
def var tem-saldo-estoq       as logical                no-undo.
DEF VAR d-saldo-terc-sp       AS DECIMAL                NO-UNDO.
DEF VAR d-saldo-terc-rs       AS DECIMAL                NO-UNDO.
DEF VAR cod-emitente-terc     AS INTEGER EXTENT 10      NO-UNDO.

DEF VARIABLE sld-emitente-terc  AS DECIMAL EXTENT 10 NO-UNDO.
DEF VARIABLE cod-emitente-jr    AS INTEGER           NO-UNDO.
DEF VARIABLE i-idx              AS INTEGER           NO-UNDO.
def buffer  b-movto-estoq for  movto-estoq.
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
def var i-ext                AS INT    NO-UNDO.
DEF VAR ext-jr               AS CHAR   NO-UNDO.
DEF VAR c-obs-ped            AS CHAR   NO-UNDO.
DEF VAR c-obs-pallet         AS CHAR   NO-UNDO.
DEF VAR dt-validade          AS DATE   NO-UNDO.
DEF VAR dt-producao          AS DATE   NO-UNDO.
DEF VAR c-arq                AS CHAR   NO-UNDO.
DEF VAR c-arq-anexo          AS CHAR   NO-UNDO.   
DEF VAR l-cria               AS LOG    NO-UNDO.
DEF VAR i-pagina             AS INT    INIT 1 NO-UNDO.
DEF VAR i-linha              AS INT    NO-UNDO.
DEF VAR i-linhax             AS INT    NO-UNDO.
DEF VAR c-coluna             AS char   NO-UNDO.

/* para executar o Excel */

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.

/****************** Definiáao de Vari†veis p/ Campos Virtuais do Relat¢rio *******************/ 
DEFINE VARIABLE pedido-cliente  AS CHARACTER  NO-UNDO.
/****************** Definiáao de Vari†veis Campo do Layout do Relat¢rio **********************/ 
DEFINE VARIABLE ped-atend-jr       AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE saldo-jr           AS DECIMAL                   NO-UNDO.
/****************** Definiáao de Vari†veis de Total do Relat¢rio *****************************/ 

DEF VAR var-qped AS DEC FORMAT ">>>,>>>.99" LABEL "Qt.Pedida".

/**********Tabela temporaria************/

define temp-table tt-fat NO-UNDO
    FIELD var-QTDBOB      AS INTEGER FORMAT ">>>>9"         LABEL "QT.BOB."
    FIELD var-QTDPEDIDO     AS INTEGER FORMAT ">>>>,>>>,>>9.9999"         LABEL "QTDPEDIDO"
    FIELD var-Larg         AS INTEGER FORMAT ">,>>9"         LABEL "Larg"
    FIELD var-diin         AS INTEGER FORMAT ">,>>9"         LABEL "DIN"
    FIELD var-diex         AS INTEGER FORMAT ">,>>9"         LABEL "DIEX"
    FIELD var-pedcli       AS CHAR    FORMAT "x(14)"         LABEL "NR.PED.CLIENTE"
    FIELD nr-ext           AS CHAR    FORMAT "x(12)"         LABEL "NR.EXT"
    FIELD var-ung-rs       AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Vga"
    FIELD var-mtn          AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Mtn"
    FIELD var-ung-sbc      AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Gru"
    FIELD var-sbc          AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Sbc"
    FIELD var-outros       AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Out"
    FIELD var-terc         AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Terc"
    FIELD var-transito     AS DECIMAL FORMAT "->>>>,>>9.99"  LABEL "Embal.Trans"
    FIELD nome-merc        AS CHARACTER FORMAT "X(7)"        LABEL "Mercado"
    FIELD dep-terceiro     AS CHAR FORMAT "x12)"
    field tem-transf       AS CHAR FORMAT "x(3)"
    field cod-estabel      like ped-venda.cod-estabel
    field cod-estabel-fat  like ped-venda.cod-estabel
    field dt-entrega       like ped-item.dt-entrega
    field dt-implant       like ped-venda.dt-implant
    field nr-pedcli        like ped-venda.nr-pedcli
    FIELD nr-sequencia     LIKE ped-item.nr-sequencia
    field tp-pedido        like ped-venda.tp-pedido
    FIELD cod-canal-venda  LIKE ped-venda.cod-canal-venda
    field nome-abrev       like ped-venda.nome-abrev
    field it-codigo        like ped-item.it-codigo
    FIELD nr-pedido        LIKE ped-venda.nr-pedido
    FIELD qt-pedida        LIKE ped-item.qt-pedida
    FIELD qt-atendida      LIKE ped-item.qt-atendida
    FIELD vl-preuni        LIKE ped-item.vl-preuni
    FIELD mo-codigo        LIKE ped-venda.mo-codigo
    FIELD nome-ab-rep-jr   AS CHAR FORMAT "x(12)" LABEL "Represent."
    FIELD nome-transp      AS CHAR FORMAT "x(12)"    LABEL "Transport." 
    FIELD nome-transp-red  AS CHAR FORMAT "x(12)"    LABEL "Transp.Red" 
    FIELD obs-pallet       AS CHAR FORMAT "x(100)"    LABEL "Obs-Pallet." 
    FIELD dt-val           AS DATE FORMAT "99/99/9999" LABEL "Dt.validade"
    FIELD dt-producao      AS DATE FORMAT "99/99/9999" LABEL "Dt.Produá∆o"
    FIELD dt-faturamento   AS DATE FORMAT "99/99/9999"
    FIELD dt-entrega-cli   AS DATE FORMAT "99/99/9999"
    FIELD liber-fat        AS CHAR
    FIELD obs-fat          AS CHAR
    FIELD preco-ex-imp     AS DEC
    field preco-icms       AS DEC
    FIELD total-ped        AS DEC
    FIELD cif-fob          AS CHAR 
    FIELD cond-pagto       AS CHAR 
    field perc-enc-fin     as dec
    field liber-financ     as char
    FIELD sld-emitente     AS DECIMAL EXTENT 10
    FIELD cod-prod-cliente AS CHAR
    FIELD unigel-com       AS CHAR
    FIELD perc-desc        AS DEC
    FIELD preco-sem-desc   AS DEC
    FIELD embarque         AS CHAR
    FIELD desc-embarque    AS CHAR
    FIELD tipo-info        AS CHAR
    INDEX ch-tipo-info IS PRIMARY tipo-info.

DEF TEMP-TABLE tt-imp-nota
    FIELD cod-estabel    LIKE it-nota-fisc.cod-estabel                                                                                 
    FIELD nome-ab-cli    LIKE it-nota-fisc.nome-ab-cli                                                                                 
    FIELD cd-emitente    LIKE it-nota-fisc.cd-emitente
    FIELD tp-pedido      LIKE ped-venda.tp-pedido                                                                                      
    FIELD nr-pedcli      LIKE it-nota-fisc.nr-pedcli                                                                                   
    FIELD nr-pedido      LIKE it-nota-fisc.nr-pedido                                                                                   
    FIELD nr-seq-ped     LIKE it-nota-fisc.nr-seq-ped                                                                                  
    FIELD it-codigo      LIKE it-nota-fisc.it-codigo                                                                                   
    FIELD pedido-cliente AS CHARACTER                                                                                       
    FIELD nr-nota-fis    LIKE it-nota-fisc.nr-nota-fis                                                                                 
    FIELD vl-preuni      LIKE it-nota-fisc.vl-preuni                                                                                   
    FIELD qt-faturada    LIKE it-nota-fisc.qt-faturada[1]                                                                              
    FIELD dt-emis-nota   LIKE it-nota-fisc.dt-emis-nota                                                                                
    FIELD codrep-jr      LIKE codrep-jr                                                                                                
    FIELD nome-ab-rep-jr LIKE nome-ab-rep-jr                                                                                           
    FIELD var-Larg       LIKE var-Larg                                                                                                 
    FIELD saldo-jr       AS DECIMAL
    FIELD observacoes    LIKE ped-venda.observacoes
    FIELD dt-entrega-cli AS CHARACTER.

/****************** Definiáao Variaveis Locais ***************************************/ 

DEFINE VARIABLE var-Larg     LIKE tt-fat.var-Larg.
DEFINE VARIABLE var-QTDPEDIDO  LIKE tt-fat.var-QTDPEDIDO.
DEFINE VARIABLE var-QTDBOB  LIKE tt-fat.var-QTDBOB.
DEFINE VARIABLE var-diin     LIKE tt-fat.var-diin.
DEFINE VARIABLE var-diex     LIKE tt-fat.var-diex.
DEFINE VARIABLE var-ung-rs   LIKE tt-fat.var-ung-rs    .
DEFINE VARIABLE var-mtn      LIKE tt-fat.var-mtn    .
DEFINE VARIABLE var-ung-sbc  LIKE tt-fat.var-ung-sbc    .
DEFINE VARIABLE var-sbc      LIKE tt-fat.var-sbc    .
DEFINE VARIABLE var-outros   LIKE tt-fat.var-outros .
DEFINE VARIABLE var-outros-TOT   LIKE tt-fat.var-outros .
DEFINE VARIABLE var-terc     LIKE tt-fat.var-terc   .
DEFINE VARIABLE var-transito LIKE tt-fat.var-transito.
DEFINE VARIABLE qtde-jr      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE qtde-jr-t    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE qtde-jr-tr   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE soma-terc-jr AS DECIMAL    NO-UNDO.
DEFINE VARIABLE soma-terc-tr AS DECIMAL    NO-UNDO.
DEFINE VARIABLE var-Merc     LIKE tt-fat.nome-merc.
DEFINE VARIABLE var-pedcli   AS CHARACTER FORMAT "x(14)"  NO-UNDO.
DEFINE VARIABLE var-acu      AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
DEFINE VARIABLE var-acu-t    AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
DEFINE VARIABLE var-acu-jr   AS DECIMAL FORMAT ">>>,>>>,>>9.99" .

DEFINE VARIABLE var-atendida AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
DEFINE VARIABLE var-pedida AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
DEFINE VARIABLE var-emb-merc   AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
DEFINE VARIABLE var-emb-merc-t AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
DEFINE VARIABLE VAR-branco   AS CHARACTER  INITIAL "" NO-UNDO.

def var l-imprime    as logical no-undo.
DEF VAR l-tt-digita  AS LOGICAL NO-UNDO.

/****************** Definiáao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form tt-fat.cod-estabel label "Estabelecimento" format "x(3)" at 001
     with down width 132 side-labels no-box stream-io frame f-relat-01-132.

form tt-fat.dt-entrega label "Dt Embarque" format "99/99/9999" at 001
     with down width 132 side-labels no-box stream-io frame f-relat-02-132.

form tt-fat.tp-pedido       column-label "TP"          format "x(2)" at 001
     tt-fat.cod-canal-venda COLUMN-LABEL "CV"          FORMAT ">9" AT 04
     tt-fat.nr-pedido       column-label "Pedido"      format ">>>>>>>9" at 007
     tt-fat.nome-abrev      column-label "Cliente"     format "x(12)" at 016
     tt-fat.it-codigo       column-label "Item"        format "x(16)" at 029
     tt-fat.qt-pedida       column-label "Qt Pedida"   format ">>>>,>>9.99" at 046
     tt-fat.qt-atendida     column-label "Qt Faturada" format ">>>>,>>9.99" at 060
     /*tt-fat.dt-entrega column-label "Dt Fatur" format "99/99/9999" at 074*/
     with down width 184 no-box stream-io frame f-relat-09-132.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.

define new shared stream str-rp.

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
   create tt-digita.
   raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

{include/i-rpvar.i}

{include/i-rpcab.i &STREAM="str-rp"}
{include/i-rpout.i &STREAM="stream str-rp"}

find first empresa no-lock
    where empresa.ep-codigo = i-ep-codigo-usuario no-error.
if avail empresa then
    assign c-empresa = empresa.razao-social.
else
    assign c-empresa = "".

assign l-imprime   = no
       l-tt-digita = CAN-FIND(FIRST tt-digita).

CASE tt-param.destino:
    WHEN 1 THEN assign v-cod-destino-impres = "Impressora".
    WHEN 2 THEN assign v-cod-destino-impres = "Arquivo".
    WHEN 3 THEN assign v-cod-destino-impres = "Terminal".
    WHEN 4 THEN assign v-cod-destino-impres = "Excel".
END CASE.

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").


IF tt-param.destino = 4 THEN DO:
    
    /* Cria Aplicaá∆o do Excel */
    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

    ASSIGN c-modelo-planilha = search("modelos/mod-espd0067.xlsx") 
           c-arq             = if i-num-ped-exec-rpw = 0 THEN SESSION:TEMP-DIRECTORY ELSE c-dir-spool-servid-exec + "\".

    RUN pi-cria-planilha.
END.


assign v-num-reg-lidos   = 0
       cod-emitente-terc = 0
       i-linhax          = 7. 

RUN pi-carrega-pedido.
RUN pi-pedidos.

/* S¢ no Excel */
IF tt-param.destino = 4 THEN DO:
    RUN pi-mes-posterior.
    RUN pi-estoque.
    RUN pi-faturamento.
    
    /* Elimina ABA Estoque */
    IF l-tt-digita = YES THEN DO:
        c-excel:sheets("Estoque"):activate no-error.
        c-relatorio = c-excel:Sheets:Item(3).
        c-excel:ActiveWindow:SelectedSheets:Delete.
    END.
END.

run pi-finalizar in h-acomp.

RUN pi-finaliza-impressao.
    
RETURN 'OK'.

procedure pi-print-editor:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
------------------------------------------------------------------------------*/

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

end procedure. /* pi-print-editor */

PROCEDURE pi-cria-planilha:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Criaá∆o da Planilha Impress∆o
------------------------------------------------------------------------------*/


    c-arquivo = c-arq + 'espd0067' + STRING(time)+ '.xlsx'.

    OS-COPY VALUE(c-modelo-planilha) VALUE(c-arquivo).

    assign c-planilha  = c-excel:Workbooks:OPEN(c-arquivo)
           c-relatorio = c-excel:Sheets:item(1)
           c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo.

END PROCEDURE. /* pi-cria-planilha */


PROCEDURE pi-finaliza-impressao:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Finaliza Impress∆o
------------------------------------------------------------------------------*/

    DEF VAR i         AS INT  NO-UNDO.
    DEF VAR c-arquivo AS CHAR NO-UNDO.

    c-excel:sheets("Pedidos"):activate no-error.
    c-relatorio = c-excel:Sheets:Item(1).
    c-relatorio:NAME = c-nome-plan-ped.

    c-planilha:SAVE().
    c-planilha:CLOSE().

    DO i = 1 TO NUM-ENTRIES(c-arq-anexo):
        c-arquivo = ENTRY(i,c-arq-anexo).
        c-planilha  = c-excel:Workbooks:OPEN(c-arquivo).
    END.

    IF i-num-ped-exec-rpw <> 0 THEN DO:
        c-excel:QUIT().
        OS-COPY VALUE(c-arquivo) c-dir-spool-servid-exec.
    END.
    ELSE OS-COPY VALUE(c-arquivo) V:\TEMP.

    /* Dispara proximo verificao */
    IF i-num-ped-exec-rpw <> 0 THEN
        run pi-recria-pedido.

    IF tt-param.l-email THEN
        RUN pi-envia-email.

    IF i-num-ped-exec-rpw = 0 THEN
        c-excel:VISIBLE = true.

    RELEASE OBJECT c-excel.

END PROCEDURE. /* pi-finaliza-impressao */

PROCEDURE pi-carrega-pedido.
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Carrega Pedido
------------------------------------------------------------------------------*/

    DEFINE VARIABLE dt-aux-ini AS DATE        NO-UNDO.
    DEFINE VARIABLE dt-aux-fim AS DATE        NO-UNDO.

    /* acerta nr-config se algum pedido perdido*/
    if can-find(first ped-item 
                where ped-item.nr-config = 1 
                  and int (ped-item.cod-refer) > 1 USE-INDEX ch-config) then do:

        /* acerta nr-config se algum pedido perdido*/
        for each ped-item 
           where ped-item.nr-config = 1 
             and int (ped-item.cod-refer) > 1 USE-INDEX ch-config EXCLUSIVE-LOCK:
            assign ped-item.nr-config  = int (ped-item.cod-refer) NO-ERROR.
        end.
    end.

    /*cria saldo de terceiros em lote*/
    EMPTY TEMP-TABLE tt-lotes.
    EMPTY TEMP-TABLE tt-notas.
    
    DO i-estab = 1 TO 6:
        FOR EACH saldo-terc 
           WHERE saldo-terc.cod-estabel  = entry(i-estab,c-est) 
             AND saldo-terc.quantidade   > 0 
             AND saldo-terc.lote        <> "" 
             AND saldo-terc.it-codigo   >= tt-param.c-it-codigo-ini 
             AND saldo-terc.it-codigo   <= tt-param.c-it-codigo-fim NO-LOCK:

            FIND FIRST natur-oper NO-LOCK 
                 WHERE natur-oper.nat-operacao = saldo-terc.nat-operacao NO-ERROR.
            
            IF NOT AVAIL natur-oper OR (natur-oper.terceiro = NO AND natur-oper.transf   = NO) THEN NEXT.
                 
            IF natur-oper.tp-oper-terc > 2 THEN NEXT. /* consignaá∆o*/
    
            v-num-reg-lidos = v-num-reg-lidos + 1.
    
            IF SUBSTRING(STRING(v-num-reg-lidos,"99999999"),7,2) = "00" THEN
                RUN pi-acompanhar IN h-acomp(INPUT "Gerando saldo terceiro" + string(v-num-reg-lidos)).
            
            FIND FIRST nota-fiscal 
                 WHERE nota-fiscal.cod-estabel = saldo-terc.cod-estabel 
                   AND nota-fiscal.serie       = saldo-terc.serie-docto 
                   AND nota-fiscal.nr-nota-fis = saldo-terc.nro-docto NO-LOCK NO-ERROR.
            IF NOT AVAIL nota-fiscal THEN DO:
                FIND FIRST tt-lotes 
                     WHERE tt-lotes.it-codigo    = saldo-terc.it-codigo 
                       AND tt-lotes.nro-docto    = saldo-terc.nro-docto 
                       AND tt-lotes.sequencia    = saldo-terc.sequencia 
                       AND tt-lotes.lote         = saldo-terc.lote 
                       AND tt-lotes.cod-estabel  = saldo-terc.cod-estabel 
                       AND tt-lotes.cod-emitente = saldo-terc.cod-emitente NO-LOCK NO-ERROR.

                IF NOT AVAIL tt-lotes THEN 
                   CREATE tt-lotes.
    
                ASSIGN tt-lotes.nro-docto    = saldo-terc.nro-docto 
                       tt-lotes.sequencia    = saldo-terc.sequencia 
                       tt-lotes.lote         = saldo-terc.lote
                       tt-lotes.cod-estabel  = saldo-terc.cod-estabel
                       tt-lotes.cod-emitente = saldo-terc.cod-emitente
                       tt-lotes.it-codigo    = saldo-terc.it-codigo.
    
                ASSIGN tt-lotes.saldo        = tt-lotes.saldo + saldo-terc.quantidade.   
            END.
            ELSE DO:
                FIND FIRST tt-notas 
                     WHERE tt-notas.nro-docto    = saldo-terc.nro-docto 
                       AND tt-notas.sequencia    = saldo-terc.sequencia 
                       AND tt-notas.cod-emitente = saldo-terc.cod-emitente 
                       AND tt-notas.serie        = saldo-terc.serie 
                       AND tt-notas.cod-estabel  = saldo-terc.cod-estabel NO-LOCK NO-ERROR.
                IF AVAIL tt-notas THEN NEXT.

                CREATE tt-notas.
                ASSIGN tt-notas.nro-docto    = saldo-terc.nro-docto 
                       tt-notas.sequencia    = saldo-terc.sequencia 
                       tt-notas.cod-emitente = saldo-terc.cod-emitente 
                       tt-notas.serie        = saldo-terc.serie 
                       tt-notas.cod-estabel  = saldo-terc.cod-estabel 
                       tt-notas.saldo        = saldo-terc.quantidade
                       tt-notas.it-codigo    = saldo-terc.it-codigo.
    
                FOR EACH fat-ser-lote NO-LOCK 
                   WHERE fat-ser-lote.cod-estabel = saldo-terc.cod-estabel 
                     AND fat-ser-lote.serie       = saldo-terc.serie-docto 
                     AND fat-ser-lote.nr-nota-fis = saldo-terc.nro-docto   
                     AND fat-ser-lote.nr-serlote   <> ""  
                     AND fat-ser-lote.nr-seq-fat  = saldo-terc.sequencia:

                    FIND FIRST tt-lotes 
                         WHERE tt-lotes.it-codigo    = saldo-terc.it-codigo 
                           AND tt-lotes.nro-docto    = saldo-terc.nro-docto 
                           AND tt-lotes.sequencia    = saldo-terc.sequencia 
                           AND tt-lotes.lote         = fat-ser-lote.nr-serlote 
                           AND tt-lotes.cod-estabel  = saldo-terc.cod-estabel 
                           AND tt-lotes.cod-emitente = saldo-terc.cod-emitente NO-LOCK NO-ERROR.

                    IF NOT AVAIL tt-lotes THEN DO:
                        CREATE tt-lotes.
                        ASSIGN tt-lotes.nro-docto    = saldo-terc.nro-docto 
                               tt-lotes.sequencia    = saldo-terc.sequencia 
                               tt-lotes.lote         = fat-ser-lote.nr-serlote
                               tt-lotes.cod-estabel  = saldo-terc.cod-estabel
                               tt-lotes.cod-emitente = saldo-terc.cod-emitente
                               tt-lotes.it-codigo    = fat-ser-lote.it-codigo.
    
                        ASSIGN tt-lotes.remessa   = tt-lotes.remessa + fat-ser-lote.qt-baixada [1]
                               tt-lotes.saldo     = tt-lotes.remessa - tt-lotes.retorno.          
    
                    END.
                END.
    
            END.
    
            FOR EACH tt-notas NO-LOCK:

                if substring(string(v-num-reg-lidos,"99999999"),7,2) = "00" then 
                run pi-acompanhar in h-acomp(input "Gerando saldo terceiro" + string(v-num-reg-lidos)).

                FIND FIRST componente 
                     WHERE componente.cod-emitente = tt-notas.cod-emitente 
                       AND componente.serie-comp   = tt-notas.serie        
                       AND componente.nro-comp     = tt-notas.nro-docto    
                       AND componente.seq-comp     = tt-notas.sequencia    
                       AND componente.componente   = 1                     
                       AND componente.dt-retorno   < 01/01/2012 NO-LOCK NO-ERROR.  
                IF AVAIL componente THEN NEXT.
    
                FOR EACH componente NO-LOCK 
                   WHERE componente.cod-emitente = tt-notas.cod-emitente 
                     AND componente.serie-comp   = tt-notas.serie
                     AND componente.nro-comp     = tt-notas.nro-docto
                     AND componente.seq-comp     = tt-notas.sequencia,
                    EACH rat-lote NO-LOCK 
                   WHERE rat-lote.lote        <> "" 
                     AND rat-lote.serie-docto  = componente.serie-docto 
                     AND rat-lote.nro-docto    = componente.nro-docto   
                     AND rat-lote.cod-emitente = tt-notas.cod-emitente  
                     AND rat-lote.sequencia    = componente.sequencia   
                     AND rat-lote.nat-oper     = componente.nat-oper:

                    FIND FIRST fat-ser-lote 
                         WHERE fat-ser-lote.cod-estabel = tt-notas.cod-estabel 
                           AND fat-ser-lote.serie       = tt-notas.serie       
                           AND fat-ser-lote.nr-nota-fis = tt-notas.nro-docto   
                           AND fat-ser-lote.nr-seq-fat  = tt-notas.sequencia NO-LOCK NO-ERROR.
    
                    IF NOT AVAIL fat-ser-lote THEN NEXT.
    
                    FIND FIRST tt-lotes 
                         WHERE tt-lotes.it-codigo    = rat-lote.it-codigo 
                           AND tt-lotes.nro-docto    = tt-notas.nro-docto 
                           AND tt-lotes.sequencia    = tt-notas.sequencia 
                           AND tt-lotes.lote         = rat-lote.lote
                           AND tt-lotes.cod-estabel  = tt-notas.cod-estabel
                           AND tt-lotes.cod-emitente = tt-notas.cod-emitente NO-LOCK NO-ERROR.
                    IF NOT AVAIL tt-lotes THEN DO:
                        CREATE tt-lotes.
                        ASSIGN tt-lotes.it-codigo    = rat-lote.it-codigo
                               tt-lotes.nro-docto    = tt-notas.nro-docto 
                               tt-lotes.sequencia    = tt-notas.sequencia 
                               tt-lotes.lote         = rat-lote.lote
                               tt-lotes.cod-estabel  = tt-notas.cod-estabel
                               tt-lotes.cod-emitente = tt-notas.cod-emitente.
                    end.
                    
                    ASSIGN tt-lotes.retorno   = tt-lotes.retorno + rat-lote.quantidade
                           tt-lotes.saldo     = tt-lotes.remessa - tt-lotes.retorno.          
    
                END.
            END.
        END.
    END.

    ASSIGN c-nome-plan-ped = STRING(MONTH(tt-param.data-ini),"99") + "-" + STRING(YEAR(tt-param.data-ini),"9999")
           c-nome-plan-fat = "Faturamento " + STRING(MONTH(tt-param.data-ini),"99") + "-" + STRING(YEAR(tt-param.data-ini),"9999").

    RUN pi-tt-fat (INPUT "MesCorrente", INPUT tt-param.data-ini, INPUT tt-param.data-fim).
    
    IF MONTH(tt-param.data-ini) = 12 THEN
        ASSIGN dt-aux-ini = DATE("01/01/" + STRING(YEAR(tt-param.data-ini) + 1))
               dt-aux-fim = DATE("31/01/" + STRING(YEAR(tt-param.data-ini) + 1)).

    ELSE IF MONTH(tt-param.data-ini) = 11 THEN
        ASSIGN dt-aux-ini = DATE("01/12/" + STRING(YEAR(tt-param.data-ini)) )
               dt-aux-fim = DATE("31/12/" + STRING(YEAR(tt-param.data-ini)) ).

    ELSE
        ASSIGN dt-aux-ini = DATE(STRING(DAY(tt-param.data-ini)) + "/" + STRING(MONTH(tt-param.data-ini) + 1) + "/" + STRING(YEAR(tt-param.data-ini)) )
               dt-aux-fim = DATE("01/" + STRING(MONTH(tt-param.data-fim) + 2) + "/" + STRING(YEAR(tt-param.data-fim)) ) - 1.

    ASSIGN c-nome-plan-mes = STRING(MONTH(dt-aux-ini),"99") + "-" + STRING(YEAR(dt-aux-ini),"9999").

    /* IF l-tt-digita = NO THEN DO: */
        RUN pi-tt-fat (INPUT "MesPosterior", INPUT dt-aux-ini, INPUT dt-aux-fim).
    
        ASSIGN dt-aux-ini = tt-param.data-ini - 150
               dt-aux-ini = DATE(MONTH(dt-aux-ini),01,YEAR(dt-aux-ini))
               dt-aux-fim = tt-param.data-fim + 182
               dt-aux-fim = DATE(MONTH(dt-aux-fim),01,YEAR(dt-aux-fim)) - 1.
    
        RUN pi-tt-fat (INPUT "Estoque", INPUT dt-aux-ini, INPUT dt-aux-fim).
    /* END. */
    
END PROCEDURE. /* pi-carrega-pedido */

PROCEDURE pi-tt-fat.
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Carrega TT
------------------------------------------------------------------------------*/
    DEF INPUT PARAM p-tipo   AS CHARACTER NO-UNDO.
    DEF INPUT PARAM p-dt-ini AS DATE      NO-UNDO.
    DEF INPUT PARAM p-dt-fim AS DATE      NO-UNDO.

    FOR EACH ped-item 
       WHERE ped-item.dt-entrega >= p-dt-ini
         AND ped-item.dt-entrega <= p-dt-fim
         AND ped-item.it-codigo >= tt-param.c-it-codigo-ini 
         AND ped-item.it-codigo <= tt-param.c-it-codigo-fim 
         AND ped-item.ind-componen  <> 3           
         AND ped-item.cod-sit-item  < 3 USE-INDEX peditem-09 NO-LOCK, 
        EACH ped-venda of ped-item NO-LOCK
       WHERE ped-venda.cod-emitente >= tt-param.i-cod-emitente-ini 
         AND ped-venda.cod-emitente <= tt-param.i-cod-emitente-fim 
         AND ped-venda.cod-estabel >= tt-param.c-cod-estabel-ini
         AND ped-venda.cod-estabel <= tt-param.c-cod-estabel-fim
         AND ped-venda.nome-abrev >= tt-param.c-nome-abrev-ini
         AND ped-venda.nome-abrev <= tt-param.c-nome-abrev-fim
         AND ped-venda.tp-pedido >= tt-param.c-tp-pedido-ini
         AND ped-venda.tp-pedido <= tt-param.c-tp-pedido-fim       
         AND ped-venda.cod-canal-venda >= tt-param.da-canal-venda-ini 
         AND ped-venda.cod-canal-venda <= tt-param.da-canal-venda-fim 
         AND ped-venda.cod-sit-ped < 3
       /*BREAK BY ped-venda.cod-estabel
             BY ped-item.dt-entrega
             BY ped-venda.nome-abrev*/ :

        ASSIGN nome-ab-rep-jr = ""
               codrep-jr = 0.
    
        /* S¢ considera itens da Polo */
        FIND FIRST ITEM OF ped-item NO-LOCK.
        IF NOT AVAIL ITEM THEN NEXT.
        IF ITEM.ge-codigo < 40 OR ITEM.ge-codigo > 49 THEN NEXT.
        /* -------------------------- */

        if  p-tipo = "Estoque" AND ped-venda.tp-pedido <> "E" THEN NEXT.
    
        FIND ped-repres OF ped-venda NO-LOCK NO-ERROR.
        IF AVAIL ped-repres THEN DO:
            FIND FIRST repres 
                 WHERE repres.nome-abrev = ped-repres.nome-ab-rep NO-LOCK NO-ERROR.
            IF AVAIL repres THEN
                ASSIGN nome-ab-rep-jr = ped-repres.nome-ab-rep
                       codrep-jr = repres.cod-rep.
        END.

        IF codrep-jr < tt-param.da-codrep-ini OR codrep-jr > tt-param.da-codrep-fim THEN NEXT.

        var-Merc = "".
    
        IF integer(substring(string(ped-item.nat-operacao),1,1)) >= 7 THEN
            ASSIGN merc-jr  = "E"
                   var-Merc = "Externo".
        ELSE
            ASSIGN merc-jr  = "I"
                   var-Merc = "Interno".

        c-obs-ped = ped-venda.observacoes.
        
        FIND FIRST if-ped-venda 
             WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
    
        IF AVAIL if-ped-venda THEN DO:
            if not tt-param.l-unig-com then next.

            FIND FIRST bf-ped-venda 
                 WHERE bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac NO-LOCK NO-ERROR.
            IF AVAIL bf-ped-venda THEN DO:
                c-obs-ped = bf-ped-venda.observacoes.
                IF integer(substring(string(bf-ped-venda.nat-operacao),1,1)) >= 7 THEN
                    ASSIGN merc-jr  = "E"
                          var-Merc = "Externo".
                ELSE
                    ASSIGN merc-jr = "I"
                          var-Merc = "Interno".
            END.
        END.
        
        IF tt-param.rs-mercado = 1 AND merc-jr <> "I" OR
           tt-param.rs-mercado = 2 AND merc-jr <> "E" THEN NEXT.

        IF l-tt-digita AND 
           NOT CAN-FIND(FIRST tt-digita
                        WHERE tt-digita.nome-abrev = ped-venda.nome-abrev ) THEN NEXT.
           
        assign v-num-reg-lidos = v-num-reg-lidos + 1.
         /* if substring(string(v-num-reg-lidos,"99999999"),7,2) = "00" then */
        run pi-acompanhar in h-acomp(input "Gerando TempTable:" + string(v-num-reg-lidos)).
              
        ASSIGN var-QTDBOB = 0
               var-QTDPEDIDO  = 0
               var-Larg     = 0 
               var-diin     = 0
               var-diex     = 0
               var-ung-rs   = 0
               var-mtn      = 0
               var-ung-sbc  = 0
               var-sbc      = 0
               var-outros   = 0
               var-terc     = 0
               var-transito = 0
               tem-transf-jr = "" 
               
               var-pedcli    = ""
               tem-data-jr = 01/01/1900.

         FIND FIRST var-result WHERE var-result.nome-var = "QTDBOB" AND 
         var-result.nr-estrut = int(ped-item.cod-refer) AND
         var-result.item-cotacao = ped-item.it-codigo 
         /*var-result.sequencia = ped-item.nr-sequencia */ NO-LOCK NO-ERROR.
    IF AVAIL var-result THEN
         ASSIGN var-QTDBOB = var-result.valor-dec.

    
        FIND FIRST var-result WHERE var-result.nome-var = "QTDPEDIDO" AND 
             var-result.nr-estrut = int(ped-item.cod-refer) AND
             var-result.item-cotacao = ped-item.it-codigo 
             /*var-result.sequencia = ped-item.nr-sequencia */ NO-LOCK NO-ERROR.
        IF AVAIL var-result THEN
             ASSIGN var-QTDPEDIDO = var-result.valor-dec.

        FIND FIRST var-result WHERE var-result.nome-var = "Largura" AND 
             var-result.nr-estrut = int(ped-item.cod-refer) AND
             var-result.item-cotacao = ped-item.it-codigo 
             /*var-result.sequencia = ped-item.nr-sequencia */ NO-LOCK NO-ERROR.
        IF AVAIL var-result THEN
             ASSIGN var-Larg = var-result.valor-dec.
    
        FIND FIRST var-result WHERE var-result.nome-var = "pedcli" AND 
             var-result.nr-estrut = int(ped-item.cod-refer) AND
             var-result.item-cotacao = ped-item.it-codigo 
             /*var-result.sequencia = ped-item.nr-sequencia */ NO-LOCK NO-ERROR.
        IF AVAIL var-result THEN
             ASSIGN var-pedcli = var-result.valor-char.
    
        FIND FIRST var-result WHERE var-result.nome-var = "DIIN" AND 
             var-result.nr-estrut = int(ped-item.cod-refer) AND
             var-result.item-cotacao = ped-item.it-codigo 
             /*var-result.sequencia = ped-item.nr-sequencia */ NO-LOCK NO-ERROR.
        IF AVAIL var-result THEN
             ASSIGN var-diin = var-result.valor-dec.
    
        FIND FIRST var-result WHERE var-result.nome-var = "DIEX" AND 
             var-result.nr-estrut = int(ped-item.cod-refer) AND
             var-result.item-cotacao = ped-item.it-codigo 
             /*var-result.sequencia = ped-item.nr-sequencia*/ NO-LOCK NO-ERROR.
        IF AVAIL var-result THEN
             ASSIGN var-diex = var-result.valor-dec.
    
        ASSIGN c-obs-pallet = ""
               dt-validade = 12/31/9999
               dt-producao = 01/01/2000.
    
        ASSIGN sld-emitente-terc = 0.
    
    
        RUN pi-saldo-pallet (INPUT ped-venda.nr-pedido).
    
        IF AVAIL if-ped-venda  THEN 
            RUN pi-saldo-pallet (INPUT if-ped-venda.nr-pedido-relac).
    
        FIND FIRST bf-if-ped-venda WHERE
                 bf-if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido
                 NO-LOCK NO-ERROR.
    
        IF AVAIL bf-if-ped-venda THEN DO:
            RUN pi-saldo-pallet (INPUT bf-if-ped-venda.nr-pedido).
            find first b-ped-venda where b-ped-venda.nr-pedido = bf-if-ped-venda.nr-pedido no-lock no-error.
        end.
    
        IF ped-venda.tp-pedido = "A" THEN
            ASSIGN var-Merc = "Amostra".
         
        ASSIGN ext-jr = "".
          
        if merc-jr = "E" then do:
         
            i-ext = index (c-obs-ped,"EX") .
            
            if i-ext > 0 then do:
               ext-jr = substring(c-obs-ped, i-ext,length(c-obs-ped)).
            
               i-ext = index(ext-jr,")").
               
               if i-ext > 0  then 
                  ext-jr = substring(ext-jr,1,i-ext + 1 ).
                  
               i-ext = index(ext-jr,"-").
               
               if i-ext > 0 and i-ext > 8   then 
                  ext-jr = substring(ext-jr,1,i-ext - 1 ).
            end.
    
        end.
    
        CREATE tt-fat.
        ASSIGN 
            tt-fat.var-QTDBOB      = var-QTDBOB
            tt-fat.var-QTDPEDIDO     = var-QTDPEDIDO
            tt-fat.var-Larg        = var-Larg
            tt-fat.var-diin        = var-diin
            tt-fat.var-diex        = var-diex
            tt-fat.var-pedcli      = var-pedcli
            tt-fat.nr-ext          = ext-jr
            tt-fat.var-ung-rs      = var-ung-rs
            tt-fat.var-mtn         = var-mtn
            tt-fat.var-ung-sbc     = var-ung-sbc
            tt-fat.var-sbc         = var-sbc
            tt-fat.var-outros      = var-outros
            tt-fat.var-terc        = var-terc
            tt-fat.var-transito    = var-transito
            tt-fat.tem-transf      = tem-transf-jr
            tt-fat.nome-merc       = var-Merc
            tt-fat.cod-estabel     = if AVAIL bf-if-ped-venda and avail b-ped-venda then b-ped-venda.cod-estabel else ped-venda.cod-estabel
            tt-fat.cod-estabel-fat = if avail bf-if-ped-venda then ped-venda.cod-estabel else if avail  if-ped-venda then if-ped-venda.cod-estab-atend else ped-venda.cod-estabel
            tt-fat.dt-implant      = ped-venda.dt-implant
            tt-fat.nr-pedcli       = ped-venda.nr-pedcli
            tt-fat.nr-sequencia    = ped-item.nr-sequencia
            tt-fat.dt-entrega      = ped-item.dt-entrega
            tt-fat.tp-pedido       = ped-venda.tp-pedido
            tt-fat.cod-canal-venda = ped-venda.cod-canal-venda
            tt-fat.nome-abrev      = ped-venda.nome-abrev
            tt-fat.it-codigo       = ped-item.it-codig
            tt-fat.nr-pedido       = ped-venda.nr-pedido
            tt-fat.nr-pedcli       = ped-venda.nr-pedcli
            tt-fat.qt-pedida       = ped-item.qt-pedida
            tt-fat.qt-atendida     = ped-item.qt-atendida
            tt-fat.vl-preuni       = ped-item.vl-preuni
            tt-fat.preco-icms      = ped-item.vl-preuni 
            tt-fat.mo-codigo       = ped-venda.mo-codigo
            tt-fat.nome-ab-rep-jr  = nome-ab-rep-jr
            tt-fat.nome-transp     = ped-venda.nome-transp
            tt-fat.nome-transp-red = ped-venda.nome-tr-red
            tt-fat.obs-pallet      = c-obs-pallet
            tt-fat.dt-val          = dt-validade
            tt-fat.dt-producao     = dt-producao
            tt-fat.unigel-com      = if avail bf-if-ped-venda  then "UC" else ""
            tt-fat.embarque        = ""
            tt-fat.desc-embarque   = ""
            tt-fat.perc-desc       = 0
            tt-fat.preco-sem-desc  = 0
            tt-fat.tipo-info       = p-tipo.
    
        FIND FIRST am-pd-prod-cliente 
             WHERE am-pd-prod-cliente.cod-emitente  = (IF AVAIL if-ped-venda and AVAIL bf-ped-venda then bf-ped-venda.cod-emitente else ped-venda.cod-emitente) 
               AND am-pd-prod-cliente.it-codigo     = ped-item.it-codigo     
               AND am-pd-prod-cliente.largura       = var-Larg  NO-LOCK NO-ERROR.
    
        IF AVAIL am-pd-prod-cliente THEN 
           ASSIGN 
               tt-fat.cod-prod-cliente =  am-pd-prod-cliente.cod-prod-cliente.
           ELSE
               ASSIGN tt-fat.cod-prod-cliente = "". 
    
        FIND FIRST estabelec OF ped-venda NO-LOCK.
    
        IF AVAIL estabelec THEN DO:
    
            FIND FIRST pd-compl-pedido WHERE
                 pd-compl-pedido.ep-codigo     = estabelec.ep-codigo   AND 
                 pd-compl-pedido.nr-pedido     = ped-venda.nr-pedido   AND
                 pd-compl-pedido.nr-sequencia  = ped-item.nr-sequencia 
                 NO-LOCK NO-ERROR.
    
            IF AVAIL pd-compl-pedido THEN DO:
    
               ASSIGN 
                   tt-fat.preco-ex-imp   =  pd-compl-pedido.preco-pis-cof    
                   tt-fat.dt-faturamento =  pd-compl-pedido.dt-faturamento   
                   tt-fat.dt-entrega-cli =  pd-compl-pedido.dt-entrega-cli
                   tt-fat.obs-fat        =  pd-compl-pedido.narrativa
                   tt-fat.perc-desc      =  DEC(SUBSTRING(pd-compl-pedido.char-1,31,10))
                   tt-fat.preco-sem-desc =  DEC(SUBSTRING(pd-compl-pedido.char-1,41,10)).  
                                          
               IF  tt-fat.preco-sem-desc = 0 THEN 
                   ASSIGN tt-fat.preco-sem-desc = tt-fat.preco-ex-imp
                          tt-fat.perc-desc = 0.
                   
               IF pd-compl-pedido.lib-faturamento = YES THEN
                   ASSIGN tt-fat.liber-fat = "Sim".
               ELSE
                   ASSIGN tt-fat.liber-fat = "N∆o".
    
               IF pd-compl-pedido.cod-estabel-fat <> "" THEN
                   ASSIGN tt-fat.cod-estabel-fat = pd-compl-pedido.cod-estabel-fat
                          tt-fat.preco-icms      = DEC (substring(pd-compl-pedido.char-1,16,15)).
               ELSE
                   ASSIGN tt-fat.cod-estabel-fat = pd-compl-pedido.cod-estabel-prod.
    
            END.
    
        END.
        
        IF AVAIL if-ped-venda and if-ped-venda.cod-estab-atend <> "" THEN 
            ASSIGN tt-fat.cod-estabel-fat = if-ped-venda.cod-estab-atend.
          
        IF AVAIL bf-if-ped-venda  THEN
            ASSIGN tt-fat.cod-estabel-fat = ped-venda.cod-estabel.
    
        /* Rotina para vers∆o embarque */
    
        find first natur-oper of ped-item no-lock no-error.
    
        ASSIGN tt-fat.total-ped = (tt-fat.preco-icms * (1 + ((if natur-oper.cd-trib-ipi = 1 then ped-item.aliquota-ipi else 0) / 100))).
        
        find first tab-finan where
             tab-finan.nr-tab-finan = ped-venda.nr-tab-finan
             no-lock no-error.
    
        IF AVAIL tab-finan THEN DO:
    
            FIND FIRST tab-finan-indice WHERE
                tab-finan-indice.nr-tab-finan = tab-finan.nr-tab-finan AND
                tab-finan-indice.num-seq      = ped-venda.nr-ind-finan
                NO-LOCK NO-ERROR.
    
            IF AVAIL tab-finan-indice THEN
                assign tt-fat.perc-enc-fin = 
                    (tab-finan-indice.tab-ind-fin - 1) * 100.
    
        END.
    
        if ped-venda.cod-sit-aval = 2 or ped-venda.cod-sit-aval = 3 then
           assign tt-fat.liber-financ = "Sim".
        else          
           assign tt-fat.liber-financ = "N∆o".
    
        FIND FIRST cond-pagto WHERE
            cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag
            NO-LOCK NO-ERROR.
    
        IF AVAIL cond-pagto THEN
           ASSIGN tt-fat.cond-pagto = cond-pagto.descricao.
    
        IF ped-venda.cidade-cif = "" THEN
            ASSIGN tt-fat.cif-fob = "Fob".
        ELSE
            ASSIGN tt-fat.cif-fob = "Cif".
    
        ASSIGN i-idx = 1.
    
        DO WHILE i-idx < 11.
            ASSIGN tt-fat.sld-emitente[i-idx] = sld-emitente-terc [i-idx].
            ASSIGN i-idx = i-idx + 1.
    
        END.
    
        IF tt-param.l-simula-embarque = NO THEN DO:
            ASSIGN tt-fat.embarque = ""
                   tt-fat.desc-embarque = "".
        END.
        ELSE DO:
            ASSIGN saldo-pedido = (tt-fat.var-ung-rs + tt-fat.var-mtn + tt-fat.var-ung-sbc +
                                   tt-fat.var-sbc + tt-fat.var-outros + tt-fat.var-terc + 
                                   tt-fat.var-transito).  
    
              /*  IF NOT AVAIL if-ped-venda AND tt-fat.liber-fat = "sim" AND tt-fat.tp-pedido <> "E" AND
                    tt-fat.dt-entrega <= tt-param.dt-embarque THEN
    
                    ASSIGN tt-fat.embarque = "OK"
                           tt-fat.desc-embarque = "FAT". 
                */
                ASSIGN i-idx   = 1 
                       d-saldo-terc-sp = tt-fat.var-ung-sbc + tt-fat.var-sbc
                       d-saldo-terc-rs = tt-fat.var-ung-rs + tt-fat.var-mtn.
    
                DO WHILE i-idx < 10.
    
                    IF cod-emitente-terc [i-idx] <> 0 THEN DO:
    
                        FIND FIRST emitente WHERE emitente.cod-emitente = cod-emitente-terc [i-idx]
                            no-lock NO-ERROR.
    
                        IF AVAIL emitente AND emitente.estado = "SP" THEN 
                             d-saldo-terc-sp = d-saldo-terc-sp + tt-fat.sld-emitente[i-idx].
    
                        IF AVAIL emitente AND emitente.estado = "RS" THEN 
                             d-saldo-terc-rs = d-saldo-terc-rs + tt-fat.sld-emitente[i-idx].
    
    
    
                    END.
                     ASSIGN i-idx = i-idx + 1.
    
                END.   
    
    
    
    
                IF (tt-fat.cod-estabel-fat = "432" OR tt-fat.cod-estabel-fat = "443") AND tt-fat.liber-fat = "sim" AND /*solic-318*/ 
                   tt-fat.dt-entrega <= tt-param.dt-embarque AND
                     tt-fat.dt-entrega + (IF weekday(tt-param.dt-embarque) = 2 THEN 3 ELSE 1) <> tt-param.dt-embarque  THEN
    
                    ASSIGN tt-fat.embarque = "OK"
                           tt-fat.desc-embarque = "TROCA NOTA".  
    
    
    
                IF (tt-fat.cod-estabel-fat = "432" OR tt-fat.cod-estabel-fat = "443") AND tt-fat.liber-fat = "sim" AND /*solic-318*/ 
                    tt-fat.dt-entrega <= tt-param.dt-embarque AND 
                    tt-fat.dt-entrega + (IF weekday(tt-param.dt-embarque) = 2 THEN 3 ELSE 1) <> tt-param.dt-embarque AND
                   ( tt-fat.var-ung-sbc + tt-fat.var-sbc + d-saldo-terc-sp) > 0 THEN
    
                    ASSIGN tt-fat.embarque = "OK"
                           tt-fat.desc-embarque = "FAT".  
    
                IF (tt-fat.cod-estabel-fat = "432" OR tt-fat.cod-estabel-fat = "443") AND tt-fat.liber-fat = "sim" AND /*solic-318*/ 
                    tt-fat.dt-entrega <= tt-param.dt-embarque AND 
                    tt-fat.dt-entrega + (IF weekday(tt-param.dt-embarque) = 2 THEN 3 ELSE 1) <> tt-param.dt-embarque AND
                    d-saldo-terc-rs > 0 THEN
    
                    ASSIGN tt-fat.embarque = "OK"
                           tt-fat.desc-embarque = "TROCA NOTA". 
    
    
    
                IF  (tt-fat.cod-estabel-fat = "434" OR tt-fat.cod-estabel-fat = "442") AND  /*solic-318*/ 
                    tt-fat.liber-fat = "sim"       AND
                    tt-fat.dt-entrega <= tt-param.dt-embarque  AND 
                    tt-fat.dt-entrega + (IF weekday(tt-param.dt-embarque) = 2 THEN 3 ELSE 1) <> tt-param.dt-embarque  THEN
    
                    ASSIGN tt-fat.embarque = "OK"
                           tt-fat.desc-embarque = "FAT". 
    
    
                d-perc-atend = (( tt-fat.qt-atendida + d-saldo-terc-sp) / tt-fat.qt-pedida ) * 100.
                
    
                IF ((tt-fat.cod-estabel-fat = "432" AND tt-fat.cod-estabel = "422") OR (tt-fat.cod-estabel-fat = "443" AND tt-fat.cod-estabel = "412")) AND /*solic-318*/ 
                   tt-fat.tp-pedido = "E" AND d-saldo-terc-rs > 0 THEN
    
                    ASSIGN tt-fat.embarque = "OK"
                           tt-fat.desc-embarque = "ARMAZENA CD". 
    
                 IF ((tt-fat.cod-estabel-fat = "432" AND tt-fat.cod-estabel = "422") OR (tt-fat.cod-estabel-fat = "443" AND tt-fat.cod-estabel = "412")) AND /*solic-318*/ 
                   tt-fat.liber-fat <> "sim" AND d-saldo-terc-rs > 0 AND d-perc-atend <= 90 THEN
    
                    ASSIGN tt-fat.embarque = "OK"
                           tt-fat.desc-embarque = "ARMAZENA CD". 
    
    
                 IF (tt-fat.cod-estabel-fat = "432" OR tt-fat.cod-estabel-fat = "443") AND tt-fat.liber-fat <> "sim" AND d-perc-atend <= 90 AND /*solic-318*/ 
                    tt-fat.dt-entrega = tt-param.dt-embarque  AND d-saldo-terc-rs = 0 and
                    (d-saldo-terc-sp / (tt-fat.qt-pedida - tt-fat.qt-atendida)) * 100 < 90 
                     THEN
                     ASSIGN tt-fat.embarque = "OK"
                            tt-fat.desc-embarque = "ARMAZENA CD".  
    
    
                  IF ((tt-fat.cod-estabel-fat = "432" AND tt-fat.cod-estabel = "422") OR (tt-fat.cod-estabel-fat = "443" AND tt-fat.cod-estabel = "412")) AND d-perc-atend <= 90 AND /*solic-318*/ 
                   tt-fat.liber-fat = "sim" AND d-saldo-terc-rs > 0 and
                      tt-fat.dt-entrega >=  tt-param.dt-embarque  + 9 - WEEKDAY(tt-param.dt-embarque)THEN
    
                    ASSIGN tt-fat.embarque = "OK"
                           tt-fat.desc-embarque = "ARMAZENA CD". 
    
    
                  IF  tt-fat.desc-embarque = "ARMAZENA CD" AND d-saldo-terc-rs = 0 AND tt-fat.var-transito > 0 THEN
                      ASSIGN tt-fat.desc-embarque = ""
                             tt-fat.embarque = "".
    
    
        END. /* else do */

        /* Retirado do Next do break by por causa de perda de registros */
        ASSIGN saldo-atend = tt-fat.qt-pedida - tt-fat.qt-atendida.
        IF tt-param.c-perc-atend > 0 AND tt-fat.qt-atendida > 0 THEN DO:
            IF ((saldo-atend / tt-fat.qt-pedida) * 100) < tt-param.c-perc-atend THEN DELETE tt-fat.
        END.
    END.

END PROCEDURE. /* pi-carrega-pedido */

PROCEDURE pi-saldo-pallet.
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Verifica saldo do Pallet
------------------------------------------------------------------------------*/

    DEF INPUT PARAM p-nr-pedido LIKE ped-venda.nr-pedido NO-UNDO.

    FOR EACH pallet USE-INDEX pedido NO-LOCK
       WHERE pallet.nr-pedido    = p-nr-pedido 
         AND pallet.it-codigo    = ped-item.it-codigo 
         AND pallet.nr-sequencia = ped-item.nr-sequencia 
         AND pallet.situacao    = 2 :

        FOR EACH it-pallet OF pallet NO-LOCK:

            FIND LAST movto-mat 
                WHERE movto-mat.it-codigo = it-pallet.it-codigo 
                  AND movto-mat.lote      = it-pallet.lote-bobina 
                  AND movto-mat.esp-docto = 01 USE-INDEX lote NO-LOCK NO-ERROR.
            IF AVAIL movto-mat AND movto-mat.dt-trans > dt-producao THEN 
                ASSIGN dt-producao = movto-mat.dt-trans.
        END.

        IF substring(pallet.char-1,1,20) <> "" THEN
            c-obs-pallet = c-obs-pallet + trim(pallet.nr-pallet) + "-" + trim(substring(pallet.char-1,1,20)) + ", ".

        ASSIGN qtde-jr    = 0
               qtde-jr-t  = 0
               qtde-jr-tr = 0.

        FOR EACH saldo-estoq NO-LOCK USE-INDEX lote
           WHERE saldo-estoq.lote      = pallet.nr-pallet 
             AND saldo-estoq.it-codigo = pallet.it-codigo 
             AND saldo-estoq.cod-refer = pallet.cod-refer 
             AND saldo-estoq.qtidade-atu > 0  :

            IF saldo-estoq.dt-vali-lote < dt-validade THEN
                dt-validade = saldo-estoq.dt-vali-lote.

            CASE saldo-estoq.cod-estabel:
                WHEN "423" THEN ASSIGN var-outros  = var-outros  + saldo-estoq.qtidade-atu.
                WHEN "413" THEN ASSIGN var-outros  = var-outros  + saldo-estoq.qtidade-atu. /*solic-318*/ 
                WHEN "422" THEN ASSIGN var-mtn     = var-mtn     + saldo-estoq.qtidade-atu.
                WHEN "412" THEN ASSIGN var-mtn     = var-mtn     + saldo-estoq.qtidade-atu. /*solic-318*/ 
                WHEN "432" THEN ASSIGN var-ung-sbc = var-ung-sbc + saldo-estoq.qtidade-atu.
                WHEN "443" THEN ASSIGN var-ung-sbc = var-ung-sbc + saldo-estoq.qtidade-atu. /*solic-318*/ 
                WHEN "434" THEN ASSIGN var-ung-rs  = var-ung-rs  + saldo-estoq.qtidade-atu.
                WHEN "442" THEN ASSIGN var-ung-rs  = var-ung-rs  + saldo-estoq.qtidade-atu. /*solic-318*/ 
                WHEN "421" THEN ASSIGN var-outros  = var-outros  + saldo-estoq.qtidade-atu.
                WHEN "411" THEN ASSIGN var-outros  = var-outros  + saldo-estoq.qtidade-atu. /*solic-318*/ 
                WHEN "424" THEN ASSIGN var-sbc     = var-sbc     + saldo-estoq.qtidade-atu.
                OTHERWISE       ASSIGN var-outros  = var-outros  + saldo-estoq.qtidade-atu.
            END CASE.
        
            ASSIGN qtde-jr = qtde-jr + saldo-estoq.qtidade-atu.
            
            FOR EACH bf-saldo-estoq FIELDS(it-codigo cod-refer cod-estabel cod-depos lote cod-localiz) 
               WHERE bf-saldo-estoq.it-codigo = saldo-estoq.it-codigo 
                 AND bf-saldo-estoq.lote      = saldo-estoq.lote NO-LOCK USE-INDEX lote,
                EACH movto-estoq FIELDS (nro-docto dt-trans) 
               WHERE movto-estoq.it-codigo   = bf-saldo-estoq.it-codigo   
                 AND movto-estoq.cod-refer   = bf-saldo-estoq.cod-refer   
                 AND movto-estoq.cod-estabel = bf-saldo-estoq.cod-estabel 
                 AND movto-estoq.cod-depos   = bf-saldo-estoq.cod-depos   
                 AND movto-estoq.lote        = bf-saldo-estoq.lote        
                 AND movto-estoq.cod-localiz = bf-saldo-estoq.cod-localiz 
                 AND movto-estoq.esp-docto = 23 NO-LOCK USE-INDEX item-estab:

                IF tem-transf-jr = ""  THEN
                    ASSIGN tem-transf-jr = movto-estoq.nro-docto
                           tem-data-jr   = movto-estoq.dt-trans.

                IF movto-estoq.dt-trans > tem-data-jr THEN
                    ASSIGN tem-transf-jr = movto-estoq.nro-docto
                           tem-data-jr    = movto-estoq.dt-trans.

            END.
        END.

        /* Procura no Terceiro */
        ASSIGN qtde-jr-t    = 0
               soma-terc-jr = 0
               qtde-jr-tr   = 0
               soma-terc-tr = 0
               tem-saldo-estoq = no.


        IF qtde-jr = 0 THEN DO:
            FIND FIRST saldo-estoq 
                 WHERE saldo-estoq.lote      = pallet.nr-pallet 
                   AND saldo-estoq.it-codigo = pallet.it-codigo 
                 /*AND saldo-estoq.cod-refer = pallet.cod-refer */ NO-LOCK USE-INDEX lote NO-ERROR.
            IF AVAIL saldo-estoq AND saldo-estoq.dt-vali-lote < dt-validade THEN
                ASSIGN dt-validade = saldo-estoq.dt-vali-lote.
            
            ASSIGN soma-terc-jr = 0
                   soma-terc-tr = 0
                      /* tem-transf-jr = ""*/.                   
           
            FOR EACH tt-lotes 
               WHERE tt-lotes.lote      = pallet.nr-pallet 
                 AND tt-lotes.it-codigo = pallet.it-codigo 
                 AND tt-lotes.saldo > 0 NO-LOCK:
           
                FIND FIRST estabelec  
                     WHERE estabelec.cod-emitente = tt-lotes.cod-emitente NO-LOCK NO-ERROR.
                IF AVAIL estabelec THEN DO:
                    ASSIGN tem-transf-jr = tt-lotes.nro-docto.
                           soma-terc-tr  = soma-terc-tr + tt-lotes.saldo.
                END.
                ELSE DO:
                    
                    ASSIGN soma-terc-jr = soma-terc-jr + tt-lotes.saldo.                                              
                       
                    ASSIGN cod-emitente-jr = tt-lotes.cod-emitente
                           i-idx = 1.
           
                    DO WHILE i-idx < 10:
                        
                        IF cod-emitente-terc [i-idx] = cod-emitente-jr OR
                           cod-emitente-terc [i-idx] = 0               THEN DO:
                            ASSIGN cod-emitente-terc [i-idx] = cod-emitente-jr.
                            LEAVE.
                        END.
                        ASSIGN i-idx = i-idx + 1.
                    END.
           
                    ASSIGN sld-emitente-terc [i-idx] = sld-emitente-terc [i-idx] + tt-lotes.saldo.
                END. /*ELSE DO TERCEIROS*/
            END.  /*TT-LOTES*/
           
            ASSIGN var-terc     = var-terc +  soma-terc-jr 
                   var-transito = var-transito + soma-terc-tr.

        END. /*IF SEM SALDO VAI VER TERCEIRO*/
    END.   /*Pallet*/
    

END PROCEDURE. /* pi-saldo-pallet */

PROCEDURE pi-pedidos:


    IF tt-param.destino = 4 THEN DO:
    
        ASSIGN i-linhax = 4
               i-idx   = 1. 
    
        DO WHILE i-idx < 10.
    
            IF cod-emitente-terc [i-idx] <> 0 THEN DO:
            
                FIND FIRST emitente WHERE emitente.cod-emitente = cod-emitente-terc [i-idx]
                    no-lock NO-ERROR.
                
                IF AVAIL emitente THEN DO:
    
                    IF i-idx = 1 THEN ASSIGN c-relatorio:range("AI" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 2 THEN ASSIGN c-relatorio:range("AJ" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 3 THEN ASSIGN c-relatorio:range("AK" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 4 THEN ASSIGN c-relatorio:range("AL" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 5 THEN ASSIGN c-relatorio:range("AM" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 6 THEN ASSIGN c-relatorio:range("AN" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 7 THEN ASSIGN c-relatorio:range("AO" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                                                                  
                    ASSIGN i-linhax = 7.                          
                                                                  
                    IF i-idx = 1 THEN ASSIGN c-relatorio:range("AI" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 2 THEN ASSIGN c-relatorio:range("AJ" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 3 THEN ASSIGN c-relatorio:range("AK" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 4 THEN ASSIGN c-relatorio:range("AL" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 5 THEN ASSIGN c-relatorio:range("AM" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 6 THEN ASSIGN c-relatorio:range("AN" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 7 THEN ASSIGN c-relatorio:range("AO" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
    
    
                    ASSIGN i-linhax = 4.
    
                END.
    
            END.
            ELSE DO:
            
            
                 IF i-idx = 1 THEN ASSIGN c-relatorio:Columns("AI:AI"):Hidden = True.
                 IF i-idx = 2 THEN ASSIGN c-relatorio:Columns("AJ:AJ"):Hidden = True.
                 IF i-idx = 3 THEN ASSIGN c-relatorio:Columns("AK:AK"):Hidden = True.
                 IF i-idx = 4 THEN ASSIGN c-relatorio:Columns("AL:AL"):Hidden = True.
                 IF i-idx = 5 THEN ASSIGN c-relatorio:Columns("AM:AM"):Hidden = True.
                 IF i-idx = 6 THEN ASSIGN c-relatorio:Columns("AN:AN"):Hidden = True.
                 IF i-idx = 7 THEN ASSIGN c-relatorio:Columns("AO:AO"):Hidden = True.            
             END.
    
            ASSIGN i-idx = i-idx + 1.
    
        END.
    /*
        IF  tt-param.l-simula-embarque THEN DO:
             ASSIGN c-relatorio:Columns("AR:AS"):Hidden = TRUE
                    c-relatorio:Columns("AX:BD"):Hidden = TRUE
                    c-relatorio:Columns("Z:AC"):Hidden = True.
                 IF var-outros-TOT = 0 THEN
                      ASSIGN c-relatorio:Columns("AP:AP"):Hidden = True.
        END.
        ELSE DO:
           ASSIGN c-relatorio:Columns("O:P"):Hidden = True.
        END.
      */
    
    END.

    ASSIGN i-linhax = 7.

    v-num-reg-lidos = 0.
    
    for each tt-fat no-lock
       WHERE tt-fat.tipo-info  = "MesCorrente"
         AND tt-fat.tp-pedido <> "E"
        break by tt-fat.cod-estabel
              by tt-fat.dt-entrega
              BY tt-fat.nome-merc
              by tt-fat.nome-abrev:
              
       
        
        v-num-reg-lidos = v-num-reg-lidos + 1.
        if substring(string(v-num-reg-lidos,"99999999"),8,1) = "0" then 
            run pi-acompanhar in h-acomp(input "Gerando planilha:" + string(v-num-reg-lidos)).
    
    
    
        if first-of(tt-fat.dt-entrega) then do:
            assign de-qt-atendida-tt-002 = 0
                   de-qt-pedida-tt-001 = 0.
        end.
        if first-of(tt-fat.nome-merc) then do:
            assign var-atendida = 0
                   var-pedida   = 0
                   var-emb-merc   = 0
                   var-emb-merc-t = 0.
        end.

        /* Esse comando n∆o pode ser executado com o break by
        
        ASSIGN saldo-atend = tt-fat.qt-pedida - tt-fat.qt-atendida.
        
        IF tt-param.c-perc-atend > 0 AND tt-fat.qt-atendida > 0 THEN DO:
    
            IF ((saldo-atend / tt-fat.qt-pedida) * 100) < tt-param.c-perc-atend THEN NEXT.
    
        END.*/
         /***  C‡DIGO PARA SA÷DA EM 132 COLUNAS ***/
    
         IF tt-param.destino <> 4 THEN DO:
    
    
           if  tt-param.formato = 2 then do:
    
            view stream str-rp frame f-cabec.
            view stream str-rp frame f-rodape.
            assign l-imprime = yes.
            if  first-of(tt-fat.cod-estabel) then do:
                display stream str-rp " " with stream-io no-box frame f-branco.
                display stream str-rp tt-fat.cod-estabel
                        with stream-io frame f-relat-01-132.
            end.
    
            if  first-of(tt-fat.dt-entrega) then do:
                display stream str-rp " " with stream-io no-box frame f-branco.
                display stream str-rp tt-fat.dt-entrega
                        with stream-io frame f-relat-02-132.
                display stream str-rp " " with stream-io no-box frame f-branco.
            end.
    
            display stream str-rp 
                tt-fat.tp-pedido
                tt-fat.cod-canal-venda
                tt-fat.nr-pedido
                tt-fat.nome-abrev
                tt-fat.it-codigo
                tt-fat.qt-pedida
                tt-fat.qt-atendida
                tt-fat.var-pedcli
                tt-fat.nr-ext
                tt-fat.var-Larg
                tt-fat.var-diin
                tt-fat.var-diex
                tt-fat.var-mtn
                tt-fat.var-terc
                tt-fat.nome-merc
                tt-fat.nome-ab-rep-jr
                tt-fat.nome-transp
                    with stream-io frame f-relat-09-132.
                down stream str-rp with frame f-relat-09-132.
          end.
         END.
    
         IF tt-param.destino = 4 THEN DO:
    
     
            ASSIGN linha-jr = ""
                   i-linhax = i-linhax + 1.
     
            ASSIGN linha-jr = 
                   STRING(tt-fat.cod-estabel)                     + ";" +
                   STRING(tt-fat.cod-estabel-fat)                 + ";" +
    
                   STRING(tt-fat.dt-entrega    )                 + ";" +
                   (IF tt-fat.dt-faturamento = ? THEN "" ELSE STRING(tt-fat.dt-faturamento,"99/99/9999"))                + ";" +
                   (IF tt-fat.dt-entrega-cli = ? THEN "" ELSE STRING(tt-fat.dt-entrega-cli,"99/99/9999"))             + ";" +
                    STRING(tt-fat.tp-pedido     )                 + ";" +
                   STRING(tt-fat.nr-pedido     )                 + ";" +
                   STRING(tt-fat.nr-sequencia  )                 + ";" +
                   STRING(tt-fat.nr-pedcli     )                 + ";" +
                   STRING(tt-fat.unigel-com    )                 + ";" +
                   STRING(tt-fat.liber-fat     )                 + ";" +
    
                
                   replace(replace(STRING(tt-fat.obs-fat       ) , CHR(10)," "),";","-")            + ";" +
    
                   STRING(tt-fat.embarque      )                 + ";" +
                   STRING(tt-fat.desc-embarque )                 + ";" +
    
                   STRING(tt-fat.tem-transf    )                 + ";" +
                   STRING(tt-fat.nome-abrev    )                 + ";" +
                   STRING(tt-fat.it-codigo     )                 + ";" +
                   STRING(tt-fat.var-pedcli    )                 + ";" +
                   STRING(tt-fat.qt-pedida     )                 + ";" +
                   STRING(tt-fat.qt-atendida   )                 + ";" +
                   STRING(tt-fat.var-QTDPEDIDO   )                 + ";" +
                   STRING(tt-fat.var-QTDBOB    )                 + ";" +
                   STRING(tt-fat.var-Larg      )                 + ";" +
                   STRING(tt-fat.var-diin      )                 + ";" +
                   STRING(tt-fat.var-diex      )                 + ";" +
                   STRING(tt-fat.preco-sem-desc)                 + ";" +
                   STRING(tt-fat.perc-desc     )                 + ";" +
                   STRING(tt-fat.preco-ex-imp )                  + ";" +
                   STRING(tt-fat.preco-icms   )                  + ";" +
                   STRING(tt-fat.total-ped * 
                          (tt-fat.var-ung-rs + tt-fat.var-mtn + tt-fat.var-ung-sbc +
                           tt-fat.var-sbc + tt-fat.var-outros + tt-fat.var-terc + 
                           tt-fat.var-transito))                 + ";" +
    
                   STRING(tt-fat.var-mtn      )                 + ";" +
                   STRING(tt-fat.var-transito )                 + ";" +
                   STRING(tt-fat.var-ung-rs   )                 + ";" +
                   STRING(tt-fat.var-ung-sbc  )                 + ";" +
    
                   STRING(IF tt-fat.sld-emitente [1] <> 0 THEN tt-fat.sld-emitente [1] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [2] <> 0 THEN tt-fat.sld-emitente [2] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [3] <> 0 THEN tt-fat.sld-emitente [3] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [4] <> 0 THEN tt-fat.sld-emitente [4] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [5] <> 0 THEN tt-fat.sld-emitente [5] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [6] <> 0 THEN tt-fat.sld-emitente [6] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [7] <> 0 THEN tt-fat.sld-emitente [7] ELSE 0)                 + ";" +
    
                   STRING(tt-fat.var-outros)                 + ";" +
    
                   STRING((tt-fat.var-ung-rs + tt-fat.var-mtn + tt-fat.var-ung-sbc +                
                          tt-fat.var-sbc + tt-fat.var-outros + tt-fat.var-terc +      
                          tt-fat.var-transito))               + ";" +
    
    
                   STRING(tt-fat.nr-ext    )                 + ";" +
                   STRING(tt-fat.nome-merc )                 + ";" +
                   STRING(tt-fat.cif-fob   )                 + ";" +
    
    
                   STRING(tt-fat.nome-transp     )                 + ";" +
                   STRING(tt-fat.nome-transp-red )                 + ";" +
                   STRING(tt-fat.cond-pagto      )                 + ";" +
                   STRING(tt-fat.nome-ab-rep-jr  )                 + ";" +
    
    
                   STRING(if tt-fat.perc-enc-fin <> 0 then tt-fat.perc-enc-fin else 0) + ";" +
    
    
                   STRING(tt-fat.liber-financ    )                 + ";" +
                   STRING(tt-fat.dt-implant      )                 + ";" +
    
              
                   STRING(tt-fat.cod-prod-cliente)                 + ";" .
    
                          ASSIGN var-outros-TOT = var-outros-TOT + tt-fat.var-outros.
    
            FIND FIRST if-ped-venda WHERE
                if-ped-venda.nr-pedido = tt-fat.nr-pedido
                NO-LOCK NO-ERROR.
    
            IF AVAIL if-ped-venda THEN DO:
    
                FIND bf-ped-venda WHERE
                    bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac
                    NO-LOCK NO-ERROR.
    
                IF AVAIL bf-ped-venda THEN DO:
    
                    FIND FIRST bf-ped-item OF bf-ped-venda WHERE
                           bf-ped-item.nr-sequencia = tt-fat.nr-sequencia AND
                           bf-ped-item.ind-componen <> 3
                           NO-LOCK NO-ERROR.
    
                    IF AVAIL bf-ped-item THEN DO:
    
                        ASSIGN linha-jr = linha-jr + STRING(bf-ped-venda.nome-abrev) + ";" +
                                                     STRING(bf-ped-venda.nr-pedido) + ";" + 
                                                     STRING(bf-ped-item.qt-pedida) + ";" +  
                                                     STRING(bf-ped-item.qt-atendida).
    
                    END.
                       
    
                END.
    
            END.
     
            ASSIGN c-relatorio:range("A" + STRING(i-linhax)):VALUE = string(linha-jr).     
    
         END.
    
    
         IF tt-param.destino <> 4 THEN DO:
    
           ASSIGN var-acu = var-acu + tt-fat.var-mtn
                  VAR-acu-t = VAR-acu-t + tt-fat.VAR-terc
                   var-emb-merc = var-emb-merc + tt-fat.var-mtn
                   var-emb-merc-t = var-emb-merc-t + tt-fat.var-terc.
          
           assign de-qt-pedida-tt-001 = de-qt-pedida-tt-001 + 
                                                tt-fat.qt-pedida
                  de-qt-atendida-tt-002 = de-qt-atendida-tt-002 + 
                                                  tt-fat.qt-atendida
                  var-atendida = var-atendida + tt-fat.qt-atendida
                  var-pedida = var-pedida + tt-fat.qt-pedida.
           
           if  last-of(tt-fat.nome-merc) then do:
               if  tt-param.formato = 2 then do:
                   
                   display stream str-rp 
                       "" @
                       tt-fat.it-codigo
                       "-----------" @ 
                       tt-fat.qt-atendida
                       "-----------" @ 
                       tt-fat.qt-pedida
                       "------------" @
                       tt-fat.var-mtn
                       "------------" @
                       tt-fat.var-terc
                       with stream-io frame f-relat-09-132.
                   down stream str-rp with frame f-relat-09-132.
               end.
               
               PUT STREAM str-rp "Total Mercado: " TO 044.
               put stream str-rp var-pedida format ">>>>,>>9.99" to 056.
               put stream str-rp var-atendida format ">>>>,>>9.99" to 070.
               PUT STREAM str-rp var-emb-merc   FORMAT ">>>>,>>9.99" TO 116.
               PUT STREAM str-rp var-emb-merc-t FORMAT ">>>>,>>9.99" TO 129.
               put stream str-rp unformatted skip(1).
               put stream str-rp unformatted skip(1).
              
           end.
          
           if  last-of(tt-fat.dt-entrega) then do:
               if  tt-param.formato = 2 then do:
                   
                   display stream str-rp 
                       "" @
                       tt-fat.it-codigo
                       "-----------" @ 
                       tt-fat.qt-atendida
                       "-----------" @ 
                       tt-fat.qt-pedida
                       "------------" @
                       tt-fat.var-mtn
                       "------------" @
                       tt-fat.var-terc
                       with stream-io frame f-relat-09-132.
                   down stream str-rp with frame f-relat-09-132.
               end.
               
               PUT STREAM str-rp "Total dia: " TO 044.
               put stream str-rp de-qt-pedida-tt-001 format ">>>>,>>9.99" to 056.
               put stream str-rp de-qt-atendida-tt-002 format ">>>>,>>9.99" to 070.
               PUT STREAM str-rp var-acu FORMAT ">>>>,>>9.99" TO 116.
               PUT STREAM str-rp var-acu-t FORMAT ">>>>,>>9.99" TO 129.
               put stream str-rp unformatted skip(1).
               put stream str-rp unformatted skip(1).
          
              ASSIGN qt-pedida-jr   = qt-pedida-jr   + de-qt-pedida-tt-001
                     qt-atendida-jr = qt-atendida-jr + de-qt-atendida-tt-002
                     VAR-acu-jr = VAR-acu-jr + VAR-acu + VAR-acu-t
                     var-acu = 0 
                     VAR-acu-t = 0.
                  
          
           end.
        END.
    
        ASSIGN var-mtn      = 0
               var-ung-rs      = 0
               var-ung-sbc      = 0
               var-sbc      = 0
               var-outros   = 0
               var-transito = 0
               var-terc     = 0.
    
    END.
    
    IF tt-param.destino <> 4 THEN DO:
       
       view stream str-rp frame f-cabec.
       view stream str-rp frame f-rodape.
       assign l-imprime = yes.
           display stream str-rp 
                with stream-io no-box frame f-Linha.
    
       PUT STREAM str-rp "Total do Periodo: " TO 044.
       put stream str-rp qt-pedida-jr   format ">>>>,>>9.99" to 056.
       put stream str-rp qt-atendida-jr format ">>>>,>>9.99" to 070.
       PUT STREAM str-rp var-acu-jr     FORMAT ">>>>,>>9.99" TO 116.
       
       view stream str-rp frame f-cabec.
       view stream str-rp frame f-rodape.
       assign l-imprime = yes.
           display stream str-rp 
                with stream-io no-box frame f-Linha.
    
    
       if  l-imprime = no then do:
           if  tt-param.formato = 2 then do:
               view stream str-rp frame f-cabec.
               view stream str-rp frame f-rodape.
           end.
           disp stream str-rp " " with stream-io frame f-nulo.
       end.
    
    END.
    
    if  tt-param.destino <> 4 then DO:
    
        if  tt-param.destino <> 1 then
        
            page stream str-rp.
        
        else do:
        
            if   tt-param.parametro = yes then
        
                 page stream str-rp.
        
        end.
        
        if  tt-param.parametro then do:
        
        
           disp stream str-rp "SELEÄ«O" skip(01) with stream-io frame f-imp-sel.
           disp stream str-rp 
              tt-param.c-cod-estabel-ini colon 20 "|< >|"   at 44 tt-param.c-cod-estabel-fim no-label
              tt-param.data-ini colon 20 "|< >|"   at 44 tt-param.data-fim no-label
              tt-param.c-tp-pedido-ini colon 20 "|< >|"   at 44 tt-param.c-tp-pedido-fim no-label
              tt-param.c-nome-abrev-ini colon 20 "|< >|"   at 44 tt-param.c-nome-abrev-fim no-label
              tt-param.i-cod-emitente-ini colon 20 "|< >|"   at 44 tt-param.i-cod-emitente-fim no-label
              tt-param.c-it-codigo-ini colon 20 "|< >|"   at 44 tt-param.c-it-codigo-fim no-label
                with stream-io side-labels overlay row 028 frame f-imp-sel.
        
           disp stream str-rp "CLASSIFICAÄ«O" skip(01) with stream-io frame f-imp-cla.
           disp stream str-rp "   PDP"
                with stream-io side-labels overlay row 028 frame f-imp-cla.
        
           put stream str-rp unformatted skip(1) "IMPRESS«O" skip(1).
        
           put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
           put stream str-rp unformatted skip "    " "Execuá∆o: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
           put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
           put stream str-rp unformatted skip "    " "Usu†rio : " tt-param.usuario.
        
        end.
        
        else
            output stream str-rp close.
    END.
    
    
    IF tt-param.destino = 4 THEN DO:
    
    
        c-relatorio:range("a8:a" + STRING(i-linhax  )):SELECT.
        c-excel:SELECTION:TextToColumns (,
                                             1,
                                             ,
                                             ,
                                             ,
                                             TRUE,
                                             ,
                                             ,
                                             ,
                                             ,
                                             ,
                                             ) NO-ERROR.
      
       
    
    END.

END PROCEDURE.

PROCEDURE pi-mes-posterior:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Mes Posterior
------------------------------------------------------------------------------*/

    c-excel:sheets("Màs Posterior"):activate no-error.
    c-relatorio = c-excel:Sheets:Item(2) NO-ERROR.
    c-relatorio:NAME = c-nome-plan-mes.

    /*IF l-tt-digita = NO THEN DO:*/
        ASSIGN i-linhax = 4
               i-idx   = 1. 
    
        DO WHILE i-idx < 10.
    
            IF cod-emitente-terc [i-idx] <> 0 THEN DO:
            
                FIND FIRST emitente WHERE emitente.cod-emitente = cod-emitente-terc [i-idx]
                    no-lock NO-ERROR.
                
                IF AVAIL emitente THEN DO:
    
                    IF i-idx = 1 THEN ASSIGN c-relatorio:range("AI" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 2 THEN ASSIGN c-relatorio:range("AJ" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 3 THEN ASSIGN c-relatorio:range("AK" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 4 THEN ASSIGN c-relatorio:range("AL" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 5 THEN ASSIGN c-relatorio:range("AM" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 6 THEN ASSIGN c-relatorio:range("AN" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 7 THEN ASSIGN c-relatorio:range("AO" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                                                                  
                    ASSIGN i-linhax = 7.                          
                                                                  
                    IF i-idx = 1 THEN ASSIGN c-relatorio:range("AI" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 2 THEN ASSIGN c-relatorio:range("AJ" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 3 THEN ASSIGN c-relatorio:range("AK" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 4 THEN ASSIGN c-relatorio:range("AL" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 5 THEN ASSIGN c-relatorio:range("AM" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 6 THEN ASSIGN c-relatorio:range("AN" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 7 THEN ASSIGN c-relatorio:range("AO" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
    
    
                    ASSIGN i-linhax = 4.
    
                END.
    
            END.
            ELSE DO:
            
            
                 IF i-idx = 1 THEN ASSIGN c-relatorio:Columns("AI:AI"):Hidden = True.
                 IF i-idx = 2 THEN ASSIGN c-relatorio:Columns("AJ:AJ"):Hidden = True.
                 IF i-idx = 3 THEN ASSIGN c-relatorio:Columns("AK:AK"):Hidden = True.
                 IF i-idx = 4 THEN ASSIGN c-relatorio:Columns("AL:AL"):Hidden = True.
                 IF i-idx = 5 THEN ASSIGN c-relatorio:Columns("AM:AM"):Hidden = True.
                 IF i-idx = 6 THEN ASSIGN c-relatorio:Columns("AN:AN"):Hidden = True.
                 IF i-idx = 7 THEN ASSIGN c-relatorio:Columns("AO:AO"):Hidden = True.            
             END.
    
            ASSIGN i-idx = i-idx + 1.
    
        END.
    /*
        IF  tt-param.l-simula-embarque THEN DO:
             ASSIGN c-relatorio:Columns("AR:AS"):Hidden = TRUE
                    c-relatorio:Columns("AX:BD"):Hidden = TRUE
                    c-relatorio:Columns("Z:AC"):Hidden = True.
                 IF var-outros-TOT = 0 THEN
                      ASSIGN c-relatorio:Columns("AP:AP"):Hidden = True.
        END.
        ELSE DO:
           ASSIGN c-relatorio:Columns("O:P"):Hidden = True.
        END.
      */  
    
        ASSIGN i-linhax = 7.
    
        v-num-reg-lidos = 0.
        
        for each tt-fat no-lock
           WHERE tt-fat.tipo-info  = "MesPosterior"
             AND tt-fat.tp-pedido <> "E"
            break by tt-fat.cod-estabel
                  by tt-fat.dt-entrega
                  BY tt-fat.nome-merc
                  by tt-fat.nome-abrev:
            
            v-num-reg-lidos = v-num-reg-lidos + 1.
            if substring(string(v-num-reg-lidos,"99999999"),8,1) = "0" then 
                run pi-acompanhar in h-acomp(input "Gerando planilha:" + string(v-num-reg-lidos)).
        
        
        
            if first-of(tt-fat.dt-entrega) then do:
                assign de-qt-atendida-tt-002 = 0
                       de-qt-pedida-tt-001 = 0.
            end.
            if first-of(tt-fat.nome-merc) then do:
                assign var-atendida = 0
                       var-pedida   = 0
                       var-emb-merc   = 0
                       var-emb-merc-t = 0.
            end.
            
            ASSIGN linha-jr = ""
                   i-linhax = i-linhax + 1.
         
            ASSIGN linha-jr = 
                   STRING(tt-fat.cod-estabel)                     + ";" +
                   STRING(tt-fat.cod-estabel-fat)                 + ";" +
        
                   STRING(tt-fat.dt-entrega    )                 + ";" +
                   (IF tt-fat.dt-faturamento = ? THEN "" ELSE STRING(tt-fat.dt-faturamento,"99/99/9999"))                + ";" +
                   (IF tt-fat.dt-entrega-cli = ? THEN "" ELSE STRING(tt-fat.dt-entrega-cli,"99/99/9999"))             + ";" +
                    STRING(tt-fat.tp-pedido     )                 + ";" +
                   STRING(tt-fat.nr-pedido     )                 + ";" +
                   STRING(tt-fat.nr-sequencia  )                 + ";" +
                   STRING(tt-fat.nr-pedcli     )                 + ";" +
                   STRING(tt-fat.unigel-com    )                 + ";" +
                   STRING(tt-fat.liber-fat     )                 + ";" +
        
                
                   replace(replace(STRING(tt-fat.obs-fat       ) , CHR(10)," "),";","-")            + ";" +
        
                   STRING(tt-fat.embarque      )                 + ";" +
                   STRING(tt-fat.desc-embarque )                 + ";" +
        
                   STRING(tt-fat.tem-transf    )                 + ";" +
                   STRING(tt-fat.nome-abrev    )                 + ";" +
                   STRING(tt-fat.it-codigo     )                 + ";" +
                   STRING(tt-fat.var-pedcli    )                 + ";" +
                   STRING(tt-fat.qt-pedida     )                 + ";" +
                   STRING(tt-fat.qt-atendida   )                 + ";" +
                   STRING(tt-fat.var-QTDPEDIDO   )                 + ";" +
                   STRING(tt-fat.var-QTDBOB    )                 + ";" +
                   STRING(tt-fat.var-Larg      )                 + ";" +
                   STRING(tt-fat.var-diin      )                 + ";" +
                   STRING(tt-fat.var-diex      )                 + ";" +
                   STRING(tt-fat.preco-sem-desc)                 + ";" +
                   STRING(tt-fat.perc-desc     )                 + ";" +
                   STRING(tt-fat.preco-ex-imp )                  + ";" +
                   STRING(tt-fat.preco-icms   )                  + ";" +
                   STRING(tt-fat.total-ped * 
                          (tt-fat.var-ung-rs + tt-fat.var-mtn + tt-fat.var-ung-sbc +
                           tt-fat.var-sbc + tt-fat.var-outros + tt-fat.var-terc + 
                           tt-fat.var-transito))                 + ";" +
        
                   STRING(tt-fat.var-mtn      )                 + ";" +
                   STRING(tt-fat.var-transito )                 + ";" +
                   STRING(tt-fat.var-ung-rs   )                 + ";" +
                   STRING(tt-fat.var-ung-sbc  )                 + ";" +
        
                   STRING(IF tt-fat.sld-emitente [1] <> 0 THEN tt-fat.sld-emitente [1] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [2] <> 0 THEN tt-fat.sld-emitente [2] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [3] <> 0 THEN tt-fat.sld-emitente [3] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [4] <> 0 THEN tt-fat.sld-emitente [4] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [5] <> 0 THEN tt-fat.sld-emitente [5] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [6] <> 0 THEN tt-fat.sld-emitente [6] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [7] <> 0 THEN tt-fat.sld-emitente [7] ELSE 0)                 + ";" +
        
                   STRING(tt-fat.var-outros)                 + ";" +
        
                   STRING((tt-fat.var-ung-rs + tt-fat.var-mtn + tt-fat.var-ung-sbc +                
                          tt-fat.var-sbc + tt-fat.var-outros + tt-fat.var-terc +      
                          tt-fat.var-transito))               + ";" +
        
        
                   STRING(tt-fat.nr-ext    )                 + ";" +
                   STRING(tt-fat.nome-merc )                 + ";" +
                   STRING(tt-fat.cif-fob   )                 + ";" +
        
        
                   STRING(tt-fat.nome-transp     )                 + ";" +
                   STRING(tt-fat.nome-transp-red )                 + ";" +
                   STRING(tt-fat.cond-pagto      )                 + ";" +
                   STRING(tt-fat.nome-ab-rep-jr  )                 + ";" +
        
        
                   STRING(if tt-fat.perc-enc-fin <> 0 then tt-fat.perc-enc-fin else 0) + ";" +
        
        
                   STRING(tt-fat.liber-financ    )                 + ";" +
                   STRING(tt-fat.dt-implant      )                 + ";" +
        
              
                   STRING(tt-fat.cod-prod-cliente)                 + ";" .
        
                          ASSIGN var-outros-TOT = var-outros-TOT + tt-fat.var-outros.
        
            FIND FIRST if-ped-venda 
                 WHERE if-ped-venda.nr-pedido = tt-fat.nr-pedido NO-LOCK NO-ERROR.
        
            IF AVAIL if-ped-venda THEN DO:
                
                FIND FIRST bf-ped-venda 
                     WHERE bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac NO-LOCK NO-ERROR.
        
                IF AVAIL bf-ped-venda THEN DO:
        
                    FIND FIRST bf-ped-item OF bf-ped-venda WHERE
                           bf-ped-item.nr-sequencia = tt-fat.nr-sequencia AND
                           bf-ped-item.ind-componen <> 3
                           NO-LOCK NO-ERROR.
        
                    IF AVAIL bf-ped-item THEN DO:
        
                        ASSIGN linha-jr = linha-jr + STRING(bf-ped-venda.nome-abrev) + ";" +
                                                     STRING(bf-ped-venda.nr-pedido) + ";" + 
                                                     STRING(bf-ped-item.qt-pedida) + ";" +  
                                                     STRING(bf-ped-item.qt-atendida).
        
                    END.
                END.
            END.
         
            ASSIGN c-relatorio:range("A" + STRING(i-linhax)):VALUE = string(linha-jr).     
        
            ASSIGN var-mtn     = 0
                  var-ung-rs   = 0
                  var-ung-sbc  = 0
                  var-sbc      = 0
                  var-outros   = 0
                  var-transito = 0
                  var-terc     = 0.
        END.
    
        c-relatorio:range("a8:a" + STRING(i-linhax  )):SELECT.
        c-excel:SELECTION:TextToColumns (,
                                         1,
                                         ,
                                         ,
                                         ,
                                         TRUE,
                                         ,
                                         ,
                                         ,
                                         ,
                                         ,
                                         ) NO-ERROR.
    /*END.*/

END PROCEDURE. /* pi-mes-posterior */


PROCEDURE pi-estoque:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Estoque
------------------------------------------------------------------------------*/

    c-excel:sheets("Estoque"):activate no-error.
    c-relatorio = c-excel:Sheets:Item(3).

    IF l-tt-digita = NO THEN DO:
    
        ASSIGN i-linhax = 4
               i-idx   = 1. 
    
        DO WHILE i-idx < 10.
    
            IF cod-emitente-terc [i-idx] <> 0 THEN DO:
            
                FIND FIRST emitente WHERE emitente.cod-emitente = cod-emitente-terc [i-idx]
                    no-lock NO-ERROR.
                
                IF AVAIL emitente THEN DO:
    
                    IF i-idx = 1 THEN ASSIGN c-relatorio:range("AI" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 2 THEN ASSIGN c-relatorio:range("AJ" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 3 THEN ASSIGN c-relatorio:range("AK" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 4 THEN ASSIGN c-relatorio:range("AL" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 5 THEN ASSIGN c-relatorio:range("AM" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 6 THEN ASSIGN c-relatorio:range("AN" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 7 THEN ASSIGN c-relatorio:range("AO" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                                                                  
                    ASSIGN i-linhax = 7.                          
                                                                  
                    IF i-idx = 1 THEN ASSIGN c-relatorio:range("AI" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 2 THEN ASSIGN c-relatorio:range("AJ" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 3 THEN ASSIGN c-relatorio:range("AK" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 4 THEN ASSIGN c-relatorio:range("AL" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 5 THEN ASSIGN c-relatorio:range("AM" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 6 THEN ASSIGN c-relatorio:range("AN" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
                    IF i-idx = 7 THEN ASSIGN c-relatorio:range("AO" + STRING(i-linhax)):VALUE = string(emitente.nome-abrev + " - " + emitente.estado).     
    
    
                    ASSIGN i-linhax = 4.
    
                END.
    
            END.
            ELSE DO:
            
            
                 IF i-idx = 1 THEN ASSIGN c-relatorio:Columns("AI:AI"):Hidden = True.
                 IF i-idx = 2 THEN ASSIGN c-relatorio:Columns("AJ:AJ"):Hidden = True.
                 IF i-idx = 3 THEN ASSIGN c-relatorio:Columns("AK:AK"):Hidden = True.
                 IF i-idx = 4 THEN ASSIGN c-relatorio:Columns("AL:AL"):Hidden = True.
                 IF i-idx = 5 THEN ASSIGN c-relatorio:Columns("AM:AM"):Hidden = True.
                 IF i-idx = 6 THEN ASSIGN c-relatorio:Columns("AN:AN"):Hidden = True.
                 IF i-idx = 7 THEN ASSIGN c-relatorio:Columns("AO:AO"):Hidden = True.            
             END.
    
            ASSIGN i-idx = i-idx + 1.
    
        END.
    /*
        IF  tt-param.l-simula-embarque THEN DO:
             ASSIGN c-relatorio:Columns("AR:AS"):Hidden = TRUE
                    c-relatorio:Columns("AX:BD"):Hidden = TRUE
                    c-relatorio:Columns("Z:AC"):Hidden = True.
                 IF var-outros-TOT = 0 THEN
                      ASSIGN c-relatorio:Columns("AP:AP"):Hidden = True.
        END.
        ELSE DO:
           ASSIGN c-relatorio:Columns("O:P"):Hidden = True.
        END.
      */  
    
        ASSIGN i-linhax = 7.
    
        v-num-reg-lidos = 0.
        
        for each tt-fat no-lock
           WHERE tt-fat.tipo-info  = "Estoque"
             AND tt-fat.tp-pedido = "E"
            break by tt-fat.cod-estabel
                  by tt-fat.dt-entrega
                  BY tt-fat.nome-merc
                  by tt-fat.nome-abrev:
            
            v-num-reg-lidos = v-num-reg-lidos + 1.
            if substring(string(v-num-reg-lidos,"99999999"),8,1) = "0" then 
                run pi-acompanhar in h-acomp(input "Gerando planilha:" + string(v-num-reg-lidos)).
        
        
        
            if first-of(tt-fat.dt-entrega) then do:
                assign de-qt-atendida-tt-002 = 0
                       de-qt-pedida-tt-001 = 0.
            end.
            if first-of(tt-fat.nome-merc) then do:
                assign var-atendida = 0
                       var-pedida   = 0
                       var-emb-merc   = 0
                       var-emb-merc-t = 0.
            end.
            
            ASSIGN linha-jr = ""
                   i-linhax = i-linhax + 1.
         
            ASSIGN linha-jr = 
                   STRING(tt-fat.cod-estabel)                     + ";" +
                   STRING(tt-fat.cod-estabel-fat)                 + ";" +
        
                   STRING(tt-fat.dt-entrega    )                 + ";" +
                   (IF tt-fat.dt-faturamento = ? THEN "" ELSE STRING(tt-fat.dt-faturamento,"99/99/9999"))                + ";" +
                   (IF tt-fat.dt-entrega-cli = ? THEN "" ELSE STRING(tt-fat.dt-entrega-cli,"99/99/9999"))             + ";" +
                    STRING(tt-fat.tp-pedido     )                 + ";" +
                   STRING(tt-fat.nr-pedido     )                 + ";" +
                   STRING(tt-fat.nr-sequencia  )                 + ";" +
                   STRING(tt-fat.nr-pedcli     )                 + ";" +
                   STRING(tt-fat.unigel-com    )                 + ";" +
                   STRING(tt-fat.liber-fat     )                 + ";" +
        
                
                   replace(replace(STRING(tt-fat.obs-fat       ) , CHR(10)," "),";","-")            + ";" +
        
                   STRING(tt-fat.embarque      )                 + ";" +
                   STRING(tt-fat.desc-embarque )                 + ";" +
        
                   STRING(tt-fat.tem-transf    )                 + ";" +
                   STRING(tt-fat.nome-abrev    )                 + ";" +
                   STRING(tt-fat.it-codigo     )                 + ";" +
                   STRING(tt-fat.var-pedcli    )                 + ";" +
                   STRING(tt-fat.qt-pedida     )                 + ";" +
                   STRING(tt-fat.qt-atendida   )                 + ";" +
                   STRING(tt-fat.var-QTDPEDIDO   )                 + ";" +
                   STRING(tt-fat.var-QTDBOB    )                 + ";" +
                   STRING(tt-fat.var-Larg      )                 + ";" +
                   STRING(tt-fat.var-diin      )                 + ";" +
                   STRING(tt-fat.var-diex      )                 + ";" +
                   STRING(tt-fat.preco-sem-desc)                 + ";" +
                   STRING(tt-fat.perc-desc     )                 + ";" +
                   STRING(tt-fat.preco-ex-imp )                  + ";" +
                   STRING(tt-fat.preco-icms   )                  + ";" +
                   STRING(tt-fat.total-ped * 
                          (tt-fat.var-ung-rs + tt-fat.var-mtn + tt-fat.var-ung-sbc +
                           tt-fat.var-sbc + tt-fat.var-outros + tt-fat.var-terc + 
                           tt-fat.var-transito))                 + ";" +
        
                   STRING(tt-fat.var-mtn      )                 + ";" +
                   STRING(tt-fat.var-transito )                 + ";" +
                   STRING(tt-fat.var-ung-rs   )                 + ";" +
                   STRING(tt-fat.var-ung-sbc  )                 + ";" +
        
                   STRING(IF tt-fat.sld-emitente [1] <> 0 THEN tt-fat.sld-emitente [1] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [2] <> 0 THEN tt-fat.sld-emitente [2] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [3] <> 0 THEN tt-fat.sld-emitente [3] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [4] <> 0 THEN tt-fat.sld-emitente [4] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [5] <> 0 THEN tt-fat.sld-emitente [5] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [6] <> 0 THEN tt-fat.sld-emitente [6] ELSE 0)                 + ";" +
                   STRING(IF tt-fat.sld-emitente [7] <> 0 THEN tt-fat.sld-emitente [7] ELSE 0)                 + ";" +
        
                   STRING(tt-fat.var-outros)                 + ";" +
        
                   STRING((tt-fat.var-ung-rs + tt-fat.var-mtn + tt-fat.var-ung-sbc +                
                          tt-fat.var-sbc + tt-fat.var-outros + tt-fat.var-terc +      
                          tt-fat.var-transito))               + ";" +
        
        
                   STRING(tt-fat.nr-ext    )                 + ";" +
                   STRING(tt-fat.nome-merc )                 + ";" +
                   STRING(tt-fat.cif-fob   )                 + ";" +
        
        
                   STRING(tt-fat.nome-transp     )                 + ";" +
                   STRING(tt-fat.nome-transp-red )                 + ";" +
                   STRING(tt-fat.cond-pagto      )                 + ";" +
                   STRING(tt-fat.nome-ab-rep-jr  )                 + ";" +
        
        
                   STRING(if tt-fat.perc-enc-fin <> 0 then tt-fat.perc-enc-fin else 0) + ";" +
        
        
                   STRING(tt-fat.liber-financ    )                 + ";" +
                   STRING(tt-fat.dt-implant      )                 + ";" +
        
              
                   STRING(tt-fat.cod-prod-cliente)                 + ";" .
        
                          ASSIGN var-outros-TOT = var-outros-TOT + tt-fat.var-outros.
        
            FIND FIRST if-ped-venda 
                 WHERE if-ped-venda.nr-pedido = tt-fat.nr-pedido NO-LOCK NO-ERROR.
        
            IF AVAIL if-ped-venda THEN DO:
                
                FIND FIRST bf-ped-venda 
                     WHERE bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac NO-LOCK NO-ERROR.
        
                IF AVAIL bf-ped-venda THEN DO:
        
                    FIND FIRST bf-ped-item OF bf-ped-venda WHERE
                           bf-ped-item.nr-sequencia = tt-fat.nr-sequencia AND
                           bf-ped-item.ind-componen <> 3
                           NO-LOCK NO-ERROR.
        
                    IF AVAIL bf-ped-item THEN DO:
        
                        ASSIGN linha-jr = linha-jr + STRING(bf-ped-venda.nome-abrev) + ";" +
                                                     STRING(bf-ped-venda.nr-pedido) + ";" + 
                                                     STRING(bf-ped-item.qt-pedida) + ";" +  
                                                     STRING(bf-ped-item.qt-atendida).
        
                    END.
                END.
            END.
         
            ASSIGN c-relatorio:range("A" + STRING(i-linhax)):VALUE = string(linha-jr).     
        
            ASSIGN var-mtn     = 0
                  var-ung-rs   = 0
                  var-ung-sbc  = 0
                  var-sbc      = 0
                  var-outros   = 0
                  var-transito = 0
                  var-terc     = 0.
        END.
    
        c-relatorio:range("a8:a" + STRING(i-linhax  )):SELECT.
        c-excel:SELECTION:TextToColumns (,
                                         1,
                                         ,
                                         ,
                                         ,
                                         TRUE,
                                         ,
                                         ,
                                         ,
                                         ,
                                         ,
                                         ) NO-ERROR.
    END.

END PROCEDURE. /* pi-estoque */

PROCEDURE pi-faturamento:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Criaá∆o da ABA faturamento
------------------------------------------------------------------------------*/

    c-excel:sheets("Faturamento"):activate no-error.
    c-relatorio = c-excel:Sheets:Item(4).
    c-relatorio:NAME = c-nome-plan-fat.


    IF NOT l-tt-digita AND
       (tt-param.i-cod-emitente-ini <> 0         OR
        tt-param.i-cod-emitente-fim <> 999999999 OR
        tt-param.c-nome-abrev-ini   <> ""        OR
        tt-param.c-nome-abrev-fim   <> "ZZZZZZZZZZZZ") THEN DO:
        FOR EACH emitente
           WHERE emitente.cod-emitente >= tt-param.i-cod-emitente-ini
             AND emitente.cod-emitente <= tt-param.i-cod-emitente-fim
             AND emitente.nome-abrev   >= tt-param.c-nome-abrev-ini  
             AND emitente.nome-abrev   <= tt-param.c-nome-abrev-fim  
             AND (emitente.identific = 1
              OR  emitente.identific = 3) :

            CREATE tt-digita.
            ASSIGN tt-digita.cod-emitente = emitente.cod-emitente
                   tt-digita.nome-abrev   = emitente.nome-abrev.  
        END.
    END.

    EMPTY TEMP-TABLE tt-imp-nota.
    IF CAN-FIND(FIRST tt-digita) THEN DO:
        FOR EACH tt-digita,
            EACH nota-fiscal NO-LOCK
           WHERE nota-fiscal.cod-emitente  = tt-digita.cod-emitente
             AND nota-fiscal.dt-emis-nota >= tt-param.data-ini
             AND nota-fiscal.dt-emis-nota <= tt-param.data-fim
             AND nota-fiscal.cod-estabel  >= tt-param.c-cod-estabel-ini
             AND nota-fiscal.cod-estabel  <= tt-param.c-cod-estabel-fim
             AND nota-fiscal.dt-cancela    = ? USE-INDEX ch-sit-nota,
            EACH it-nota-fisc OF nota-fiscal NO-LOCK 
           WHERE it-nota-fisc.it-codigo   >= tt-param.c-it-codigo-ini
             AND it-nota-fisc.it-codigo   <= tt-param.c-it-codigo-fim,
           FIRST ITEM 
           WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK:
    
            RUN pi-alimenta-fat.
        end.
    END.
    ELSE DO:
        FOR EACH nota-fiscal NO-LOCK
           WHERE nota-fiscal.dt-emis-nota >= tt-param.data-ini
             AND nota-fiscal.dt-emis-nota <= tt-param.data-fim
             AND nota-fiscal.cod-estabel  >= tt-param.c-cod-estabel-ini
             AND nota-fiscal.cod-estabel  <= tt-param.c-cod-estabel-fim
             AND nota-fiscal.cod-emitente >= tt-param.i-cod-emitente-ini
             AND nota-fiscal.cod-emitente <= tt-param.i-cod-emitente-fim
             AND nota-fiscal.nome-ab-cli  >= tt-param.c-nome-abrev-ini
             AND nota-fiscal.nome-ab-cli  <= tt-param.c-nome-abrev-fim
             AND nota-fiscal.dt-cancela    = ? USE-INDEX ch-sit-nota,
            EACH it-nota-fisc OF nota-fiscal NO-LOCK 
           WHERE it-nota-fisc.it-codigo   >= tt-param.c-it-codigo-ini
             AND it-nota-fisc.it-codigo   <= tt-param.c-it-codigo-fim,
           FIRST ITEM 
           WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK:
    
            RUN pi-alimenta-fat.
        end.
    END.

    
    
    ASSIGN i-linha = 3.
    FOR EACH tt-imp-nota
       BREAK BY tt-imp-nota.cod-estabel
             BY tt-imp-nota.cd-emitente
             BY tt-imp-nota.nome-ab-cli
             BY tt-imp-nota.it-codigo
             BY tt-imp-nota.nr-nota-fis:

        ASSIGN i-linha = i-linha + 1.

        ASSIGN c-relatorio:range("A" + STRING(i-linha)):VALUE = tt-imp-nota.cod-estabel   
               c-relatorio:range("B" + STRING(i-linha)):VALUE = tt-imp-nota.nome-ab-cli   
               c-relatorio:range("C" + STRING(i-linha)):VALUE = tt-imp-nota.tp-pedido     
               c-relatorio:range("D" + STRING(i-linha)):VALUE = tt-imp-nota.nr-pedcli     
               c-relatorio:range("E" + STRING(i-linha)):VALUE = tt-imp-nota.nr-pedido     
               c-relatorio:range("F" + STRING(i-linha)):VALUE = tt-imp-nota.nr-seq-ped    
               c-relatorio:range("G" + STRING(i-linha)):VALUE = tt-imp-nota.it-codigo     
               c-relatorio:range("H" + STRING(i-linha)):VALUE = tt-imp-nota.pedido-cliente
               c-relatorio:range("I" + STRING(i-linha)):VALUE = tt-imp-nota.nr-nota-fis   
               c-relatorio:range("J" + STRING(i-linha)):VALUE = tt-imp-nota.vl-preuni     
               c-relatorio:range("K" + STRING(i-linha)):VALUE = tt-imp-nota.qt-faturada   
               c-relatorio:range("L" + STRING(i-linha)):VALUE = tt-imp-nota.dt-emis-nota  
               c-relatorio:range("M" + STRING(i-linha)):VALUE = tt-imp-nota.codrep-jr     
               c-relatorio:range("N" + STRING(i-linha)):VALUE = tt-imp-nota.nome-ab-rep-jr
               c-relatorio:range("O" + STRING(i-linha)):VALUE = tt-imp-nota.var-Larg      
               c-relatorio:range("P" + STRING(i-linha)):VALUE = tt-imp-nota.saldo-jr      
               c-relatorio:range("R" + STRING(i-linha)):VALUE = tt-imp-nota.observacoes   
               c-relatorio:range("Q" + STRING(i-linha)):VALUE = tt-imp-nota.dt-entrega-cli.
    END.

END PROCEDURE. /* pi-faturamento */

PROCEDURE pi-alimenta-fat:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Alimenta faturamento
------------------------------------------------------------------------------*/

    IF SUBSTRING(it-nota-fisc.nat-operacao,1,1) < "5" THEN NEXT.

    IF integer(substring(string(it-nota-fisc.nat-operacao),1,1,"character")) >= 7 THEN 
        ASSIGN merc-jr = "E".
    ELSE
        ASSIGN merc-jr = "I".

    IF tt-param.rs-mercado = 1 AND merc-jr <> "I" OR
       tt-param.rs-mercado = 2 AND merc-jr <> "E" THEN NEXT.

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    IF ITEM.ge-codigo < 40 OR ITEM.ge-codigo > 49 THEN NEXT.

    FIND FIRST ped-item 
         WHERE ped-item.nome-abrev   = it-nota-fisc.nome-ab-cli 
           AND ped-item.nr-pedcli    = it-nota-fisc.nr-pedcli 
           AND ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped 
           AND ped-item.it-codigo    = it-nota-fisc.it-codigo 
           AND ped-item.cod-refer    = it-nota-fisc.cod-refer NO-LOCK NO-ERROR.
    
    FIND FIRST ped-venda 
         WHERE ped-venda.nr-pedcli  = ped-item.nr-pedcli 
           AND ped-venda.nome-abrev = ped-item.nome-abrev NO-LOCK NO-ERROR.

    IF NOT AVAIL ped-venda THEN NEXT.

    IF ped-venda.tp-pedido < tt-param.c-tp-pedido-ini OR
       ped-venda.tp-pedido > tt-param.c-tp-pedido-fim THEN NEXT.

    /* Desconsidera pedido unigel comercial */
    FIND FIRST if-ped-venda 
         WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
    IF AVAIL if-ped-venda THEN DO:

        FIND FIRST bf-ped-venda 
             WHERE bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac NO-LOCK NO-ERROR.

        FIND FIRST emitente
             WHERE emitente.nome-abrev = it-nota-fisc.nome-ab-cli NO-LOCK NO-ERROR.

        IF CAN-FIND(FIRST estabelec
                    WHERE estabelec.cod-estabel = STRING(emitente.cod-emitente)) THEN NEXT.

        
    END.

    /*IF l-tt-digita AND 
        NOT CAN-FIND(FIRST tt-digita
                     WHERE tt-digita.nome-abrev = nota-fiscal.nome-abrev ) THEN NEXT.*/

    IF ped-venda.cod-sit-ped = 3 THEN
        ASSIGN ped-atend-jr = "T"
               saldo-jr = 0.  
    ELSE
        ASSIGN ped-atend-jr = "P"
               saldo-jr = ped-item.qt-pedida - ped-item.qt-atendida.

    ASSIGN nome-ab-rep-jr = ""
           codrep-jr = 0.

    FIND FIRST ped-repres OF ped-venda NO-LOCK NO-ERROR.
    IF AVAIL ped-repres THEN DO:
        
        FIND FIRST repres 
             WHERE repres.nome-abrev = ped-repres.nome-ab-rep NO-LOCK NO-ERROR.

        IF AVAIL repres THEN
            ASSIGN nome-ab-rep-jr = ped-repres.nome-ab-rep
                   codrep-jr = repres.cod-rep.
    END.

    IF codrep-jr < tt-param.da-codrep-ini OR codrep-jr > tt-param.da-codrep-fim THEN NEXT.

    var-larg = 0.
    var-qped = 0.
    IF AVAIL ped-item THEN DO:
        var-qped = ped-item.qt-pedida.
        FIND FIRST var-result 
             WHERE var-result.nome-var     = "Largura" 
               AND var-result.nr-estrut    = ped-item.nr-config 
               AND var-result.item-cotacao = ped-item.it-codigo  NO-LOCK NO-ERROR.
   
        IF AVAIL var-result THEN
            ASSIGN var-Larg = var-result.valor-dec.

        ASSIGN pedido-cliente = " ".
        FIND FIRST var-result 
             WHERE var-result.nome-var = "PEDCLI" 
               AND var-result.nr-estrut = ped-item.nr-config 
               AND var-result.item-cotacao = ped-item.it-codigo NO-LOCK NO-ERROR.

        IF AVAIL var-result THEN
            ASSIGN pedido-cliente = var-result.valor-char.
    END.
        
    
    FIND FIRST estabelec OF ped-venda NO-LOCK.
    IF AVAIL estabelec THEN
        FIND FIRST pd-compl-pedido 
             WHERE pd-compl-pedido.ep-codigo     = estabelec.ep-codigo
               AND pd-compl-pedido.nr-pedido     = ped-venda.nr-pedido
               AND pd-compl-pedido.nr-sequencia  = it-nota-fisc.nr-seq-ped NO-LOCK NO-ERROR.

    CREATE tt-imp-nota.
    ASSIGN tt-imp-nota.cod-estabel    = it-nota-fisc.cod-estabel
           tt-imp-nota.nome-ab-cli    = it-nota-fisc.nome-ab-cli
           tt-imp-nota.cd-emitente    = it-nota-fisc.cd-emitente
           tt-imp-nota.tp-pedido      = ped-venda.tp-pedido     
           tt-imp-nota.nr-pedcli      = it-nota-fisc.nr-pedcli  
           tt-imp-nota.nr-pedido      = it-nota-fisc.nr-pedido
           tt-imp-nota.nr-seq-ped     = it-nota-fisc.nr-seq-ped                               
           tt-imp-nota.it-codigo      = it-nota-fisc.it-codigo                                
           tt-imp-nota.pedido-cliente = pedido-cliente                                        
           tt-imp-nota.nr-nota-fis    = it-nota-fisc.nr-nota-fis                              
           tt-imp-nota.vl-preuni      = it-nota-fisc.vl-preuni 
           tt-imp-nota.qt-faturada    = it-nota-fisc.qt-faturada[1]                           
           tt-imp-nota.dt-emis-nota   = it-nota-fisc.dt-emis-nota                             
           tt-imp-nota.codrep-jr      = codrep-jr                                             
           tt-imp-nota.nome-ab-rep-jr = nome-ab-rep-jr                                        
           tt-imp-nota.var-Larg       = var-Larg                                              
           tt-imp-nota.saldo-jr       = saldo-jr
           tt-imp-nota.observacoes    = IF integer(substring(string(ped-venda.nat-operacao),1,1)) >= 7 THEN ped-venda.observacoes  ELSE ""
           tt-imp-nota.dt-entrega-cli = IF AVAIL pd-compl-pedido THEN " " +  string(pd-compl-pedido.dt-entrega-cli,"99/99/9999") +  " "  else "".
    
    /*IF AVAIL bf-ped-venda THEN DO:
        ASSIGN tt-imp-nota.nr-pedido   = bf-ped-venda.nr-pedido
               tt-imp-nota.nome-ab-cli = it-nota-fisc.nome-ab-cli + "-" + bf-ped-venda.nome-abrev
               tt-imp-nota.cod-estabel = it-nota-fisc.cod-estabel + "-" + bf-ped-venda.cod-estabel.
    END.*/

    ASSIGN var-Larg = 0.


END PROCEDURE. /* pi-alimenta-fat */

PROCEDURE pi-envia-email:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: customizacao para enviar planilha por email
------------------------------------------------------------------------------*/

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
       + '<p>Bom dia,</p></br>'
       + '<p>Segue anexo carteira atualizada.</p>'
       + "."
       + "</TABLE></BODY></HTML>".

    c-destino = "".
    FOR EACH ext_usuar_grp_usuar WHERE 
        ext_usuar_grp_usuar.ativo AND
        ext_usuar_grp_usuar.cod_grp_usuar = tt-param.c-grp-usuar   NO-LOCK,
        EACH usuar_mestre NO-LOCK WHERE
           usuar_mestre.cod_usuario = ext_usuar_grp_usuar.cod_usuario .
        
        IF usuar_mestre.cod_e_mail_local <> "" THEN 
            c-destino =  c-destino + "," + usuar_mestre.cod_e_mail_local .

        IF ext_usuar_grp_usuar.email_alter <> "" THEN 
            c-destino =  c-destino + "," + ext_usuar_grp_usuar.email_alter .

    END.

    c-destino = SUBSTRING(c-destino,2,LENGTH(c-destino)).

    
    ASSIGN c-remetente = 'usrtisis@unigel.com.br'.
    c-anexo = c-arquivo.

    FIND FIRST param-global NO-LOCK NO-ERROR.
    FIND FIRST param_email  NO-LOCK NO-ERROR.

     /* *** Delecao da Tabela Temporaria *** */

     FOR each tt-envio2 :  
         DELETE tt-envio2. 
     END.      
     FOR EACH tt-mensagem.
         DELETE tt-mensagem.
     END.

     c-assunto =  "POLO  Carteira " + STRING(TODAY,"99/99/9999").

    RUN utp/utapi019.p persistent set h-utapi019.

    CREATE tt-envio2.
    ASSIGN tt-envio2.versao-integracao  = 1
           tt-envio2.servidor           = param_email.cod_servid_e_mail
           tt-envio2.porta              = param_email.num_porta 
           tt-envio2.exchange           = param_email.log_servid_exchange
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

    RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                   INPUT  TABLE tt-mensagem,    
                                   OUTPUT TABLE tt-erros).

    DELETE procedure h-utapi019.

END PROCEDURE.

procedure pi-recria-pedido:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE c-cod_servid_exec AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dt-aux     AS DATE        NO-UNDO.
    DEFINE VARIABLE i-cont-aux AS INTEGER     NO-UNDO.

    create tt-param-aux.
    BUFFER-COPY tt-param TO tt-param-aux.
    assign tt-param-aux.usuario   = tt-param.usuario
           tt-param-aux.destino   = 4
           tt-param-aux.data-exec = TODAY + 1
           tt-param-aux.hora-exec = tt-param-aux.hora-exec
           tt-param-aux.arquivo   = "ESPD0067.LST".


    ASSIGN dt-aux = TODAY + 1.

    ASSIGN tt-param-aux.data-ini = DATE("01/" + string(MONTH(dt-aux),"99") + "/" + STRING(year(dt-aux),"9999")).

    IF MONTH(tt-param-aux.data-ini) = 12 THEN
        ASSIGN tt-param-aux.data-fim = DATE("31/12/" + STRING(year(tt-param-aux.data-ini),"9999")).
    ELSE
        ASSIGN tt-param-aux.data-fim = DATE("01/" + string(MONTH(tt-param-aux.data-ini) + 1,"99") + "/" + STRING(year(tt-param-aux.data-ini),"9999")) - 1.

    IF i-num-ped-exec-rpw = 0 THEN DO:
        FIND FIRST usuar_mestre
             WHERE usuar_mestre.cod_usuario = tt-param.usuario NO-LOCK NO-ERROR.

        IF AVAIL usuar_mestre AND usuar_mestre.cod_servid_exec <> "" THEN
            ASSIGN c-cod_servid_exec = usuar_mestre.cod_servid_exec.
        ELSE
            ASSIGN c-cod_servid_exec = "rpw1".
    END.
    ELSE DO:
        FIND FIRST ped_exec
             WHERE ped_exec.num_ped_exec = i-num-ped-exec-rpw NO-LOCK NO-ERROR.
        IF AVAIL ped_exec THEN
            ASSIGN c-cod_servid_exec = ped_exec.cod_servid_exec.
        ELSE
            ASSIGN c-cod_servid_exec = "".
    END.

    create tt_param_segur.
    assign tt_param_segur.tta_num_vers_integr_api      = 3
           tt_param_segur.tta_cod_aplicat_dtsul_corren = "MAN"
           tt_param_segur.tta_cod_empres_usuar         = v_cod_empres_usuar
           tt_param_segur.tta_cod_grp_usuar_lst        = v_cod_grp_usuar_lst
           tt_param_segur.tta_cod_idiom_usuar          = v_cod_idiom_usuar
           tt_param_segur.tta_cod_modul_dtsul_corren   = "PDP"
           tt_param_segur.tta_cod_pais_empres_usuar    = "BRA"
           tt_param_segur.tta_cod_usuar_corren         = v_cod_usuar_corren
           tt_param_segur.tta_cod_usuar_corren_criptog = v_cod_usuar_corren_criptog.

    create tt_ped_exec.
    assign tt_ped_exec.tta_num_seq                     = 1
           tt_ped_exec.tta_cod_usuario                 = v_cod_usuar_corren
           tt_ped_exec.tta_cod_prog_dtsul              = "ESPD0067"
           tt_ped_exec.tta_cod_prog_dtsul_rp           = "pdp/espd0067rp.p"
           tt_ped_exec.tta_cod_release_prog_dtsul      = "2.00.00.001"
           tt_ped_exec.tta_dat_exec_ped_exec           = tt-param-aux.data-exec
           tt_ped_exec.tta_hra_exec_ped_exec           = replace(string(tt-param-aux.hora-exec, "HH:MM:SS"),":","")
           tt_ped_exec.tta_cod_servid_exec             = c-cod_servid_exec
           tt_ped_exec.tta_cdn_estil_dwb               = 97.

    create tt_ped_exec_param.
    assign tt_ped_exec_param.tta_num_seq               = 1
           tt_ped_exec_param.tta_cod_dwb_file          = "pdp/espd0067rp.p"
           tt_ped_exec_param.tta_cod_dwb_output        = "Arquivo"
           tt_ped_exec_param.tta_nom_dwb_printer       = tt-param-aux.arquivo
           tt_ped_exec_param.tta_cod_dwb_print_layout  = tt-param-aux.arquivo.

    raw-transfer tt-param-aux to tt_ped_exec_param.tta_raw_param_ped_exec.

    assign i-cont-aux = 1.
    for each tt-raw-digita no-lock:
        create tt_ped_exec_param_aux.
        assign tt_ped_exec_param_aux.tta_num_seq            = 1
               tt_ped_exec_param_aux.tta_raw_param_ped_exec = tt-raw-digita.raw-digita
               tt_ped_exec_param_aux.tta_num_dwb_order      = i-cont-aux.
        assign i-cont-aux = i-cont-aux + 1.
    end.

    run btb/btb912zb.p (input-output table tt_param_segur,
                        input-output table tt_ped_exec,
                        input table tt_ped_exec_param,
                        input table tt_ped_exec_param_aux,
                        input table tt_ped_exec_sel).


end procedure.


/* fim do programa */



