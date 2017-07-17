/**************************************************************\
***************************************************************
**  Programa: esp\upc\upc-pd4000cpy.p
**  Objetivo: programa para tratar copia de cabecalho de pedido
**             
**  Autor...: Edson Lourenco da Aparecida - Amgra - VGA
**  Data....: Marco/2006
**  Versao..: I.00.000
***************************************************************
\**************************************************************/

DEF NEW GLOBAL SHARED VAR gwh-bt-next     AS WIDGET-HANDLE.
def new global shared var gr-ped-venda as rowid no-undo.
DEF NEW GLOBAL SHARED VAR gwh-frame AS WIDGET-HANDLE.
def var hShowMsg         as handle no-undo.
def var l-copia-efetuada as log    no-undo.
def var l-exp-dt-entrega as log    no-undo.
def var i-situacao       as int    no-undo init 1.
def var i-natur-oper     as int    no-undo init 1.
def VAR p-rw-ped-venda as rowid no-undo.
DEFINE VARIABLE c-cfop-uc AS CHARACTER  NO-UNDO. /* caso esta fazendo copia de filho de unigel comercial*/
DEFINE VARIABLE c-cod-estabel-uc AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cidade-cif AS CHARACTER   NO-UNDO.

DEFINE BUFFER b-ped-venda FOR ped-venda.
DEFINE BUFFER bb-ped-venda FOR ped-venda.
DEFINE BUFFER if-ped-venda FOR if-ped-venda.

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

DEF NEW GLOBAL SHARED VAR c-cod-table AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR rw-row-table AS ROWID no-undo.
DEFINE VARIABLE i-nr-pedido AS INTEGER    NO-UNDO.

def var h-acomp          as handle no-undo.
def var bo-ped-venda     as handle no-undo.


def temp-table tt-item-copia
    field nr-sequencia like ped-item.nr-sequencia
    field it-codigo    like ped-item.it-codigo
    field desc-item    like item.desc-item
    field qt-pedida    like ped-item.qt-pedida
    field dt-entrega   like ped-item.dt-entrega
    field selecionado  as log init yes.

def temp-table tt-ped-copia
    field nome-abrev      like ped-venda.nome-abrev
    field nr-pedido       like ped-venda.nr-pedido
    field nr-pedcli       like ped-venda.nr-pedcli
    field dt-emissao      like ped-venda.dt-emissao
    field dt-entrega      like ped-venda.dt-entrega
    field nr-tab-finan    like ped-venda.nr-tab-finan
    field nr-ind-finan    like ped-venda.nr-ind-finan
    field cod-cond-pag    like ped-venda.cod-cond-pag
    field cod-entrega     like ped-venda.cod-entrega
    field nome-transp     like ped-venda.nome-transp
    field perc-desco1     like ped-venda.perc-desco1
    field esp-ped         like ped-venda.esp-ped
    field tp-preco        like ped-venda.tp-preco
    field vl-desconto     like ped-venda.vl-desconto
    field e-mail          like emitente.e-mail
    field ind-apr-cred    like emitente.ind-apr-cred
    field ind-cre-cli     like emitente.ind-cre-cli.

def temp-table tt-cliente
    field nome-abrev   like emitente.nome-abrev
    field cod-emitente like emitente.cod-emitente
    field nome-emit    like emitente.nome-emit
    field cod-gr-cli   like emitente.cod-gr-cli
    field cod-entrega  like emitente.cod-entrega
    field selecionado  as log format "X/ " init no
    index id-nome   is primary nome-abrev
    index id-gr-cli cod-gr-cli.

assign l-copia-efetuada = NO.

if c-seg-usuario = "elapare" then 
message
"dentro cpy " string(rw-row-table) skip c-cod-table
view-as alert-box.

IF rw-row-table <> ? AND c-cod-table = "ped-venda" THEN

DO:
   FOR EACH tt-ped-copia.
       DELETE tt-ped-copia.
   END.
   FOR EACH tt-item-copia.
       DELETE tt-item-copia.
   END.

   FIND b-ped-venda WHERE ROWID(b-ped-venda) = rw-row-table NO-ERROR.
   IF AVAIL b-ped-venda THEN
   DO:
               ASSIGN   p-rw-ped-venda    = rw-row-table        
                        c-cfop-uc         = b-ped-venda.nat-operacao
                        c-cod-estabel-uc  = b-ped-venda.cod-estabel.

            FIND FIRST if-ped-venda WHERE if-ped-venda.nr-pedido-relac = b-ped-venda.nr-pedido NO-LOCK NO-ERROR.
            IF AVAIL if-ped-venda THEN DO:
                 c-cfop-uc        = if-ped-venda.nat-oper-orig.
                 c-cod-estabel-uc = (IF index("432,434",b-ped-venda.cod-estabel) > 0 then "422" else IF index("442,443",b-ped-venda.cod-estabel) > 0 then "412" ELSE b-ped-venda.cod-estabel). /*solic-318*/ 
                 
                 FIND FIRST bb-ped-venda WHERE bb-ped-venda.nr-pedido = if-ped-venda.nr-pedido NO-LOCK NO-ERROR.
                 IF AVAIL bb-ped-venda THEN
                           c-cod-estabel-uc = bb-ped-venda.cod-estabel.

            END.

            FIND emitente WHERE emitente.nome-abrev = b-ped-venda.nome-abrev NO-LOCK NO-ERROR.
            IF NOT AVAIL emitente THEN RETURN "nok".
        
            assign i-nr-pedido = NEXT-VALUE (seq-nr-pedido).
            CREATE tt-ped-copia.
            ASSIGN 
             
            tt-ped-copia.nome-abrev      = b-ped-venda.nome-abrev
            tt-ped-copia.nr-pedido       = i-nr-pedido
            tt-ped-copia.nr-pedcli       = string(i-nr-pedido)
            tt-ped-copia.dt-emissao      = IF (b-ped-venda.dt-emissao < (TODAY)) THEN TODAY ELSE b-ped-venda.dt-emissao
            tt-ped-copia.dt-entrega      = IF (b-ped-venda.dt-entrega < TODAY) THEN TODAY ELSE b-ped-venda.dt-entrega
            tt-ped-copia.nr-tab-finan    = b-ped-venda.nr-tab-finan
            tt-ped-copia.nr-ind-finan    = b-ped-venda.nr-ind-finan
            tt-ped-copia.cod-cond-pag    = b-ped-venda.cod-cond-pag
            tt-ped-copia.cod-entrega     = b-ped-venda.cod-entrega
            tt-ped-copia.nome-transp     = b-ped-venda.nome-transp
            tt-ped-copia.perc-desco1     = b-ped-venda.perc-desco1
            tt-ped-copia.esp-ped         = b-ped-venda.esp-ped
            tt-ped-copia.tp-preco        = b-ped-venda.tp-preco
            tt-ped-copia.vl-desconto     = b-ped-venda.vl-desconto
            tt-ped-copia.e-mail          = emitente.e-mail
            tt-ped-copia.ind-apr-cred    = emitente.ind-apr-cred
            tt-ped-copia.ind-cre-cli     = emitente.ind-cre-cli
            c-cidade-cif                 = b-ped-venda.cidade-cif   .
        
        
   
        
        
          run utp/ut-acomp.p persistent set h-acomp.
          run pi-inicializar in h-acomp ("Copiando Pedido").
          run pi-acompanhar in h-acomp (input "Criando pedido" + STRING(i-nr-pedido)).
          
          if not valid-handle(bo-ped-venda) or
             bo-ped-venda:type <> "PROCEDURE":U or
             bo-ped-venda:file-name <> "dibo/bodi159cpy.p" then
             run dibo/bodi159cpy.p persistent set bo-ped-venda.
        
          for each RowErrors:
              delete RowErrors.
          end.
        
          run copyOrder in bo-ped-venda(input  p-rw-ped-venda,
                                        input  table tt-ped-copia,
                                        input  table tt-item-copia,
                                        input  i-situacao,
                                        input  i-natur-oper,
                                        input  l-exp-dt-entrega,
                                        output gr-ped-venda,
                                        output table RowErrors).
          delete procedure bo-ped-venda.
        
          if  can-find(first RowErrors
                       where RowErrors.ErrorType <> "INTERNAL":U) then do:
              {method/ShowMessage.i1}
              {method/ShowMessage.i2}
          end.
        
          if  not can-find(first RowErrors
                           where RowErrors.ErrorType <> "INTERNAL":U
                             and RowErrors.ErrorSubType = "Error":U) then do:
        
              for each tt-ped-copia:
                  delete tt-ped-copia.
              end.
        
              assign l-copia-efetuada = yes.
           
          end.
          
        
          run pi-finalizar in h-acomp.

           find first ped-venda where
               rowid(ped-venda) = gr-ped-venda
                no-error.
          IF AVAIL ped-venda THEN
                assign
                    ped-venda.nat-operacao = c-cfop-uc     
                    ped-venda.cod-estabel  = c-cod-estabel-uc
                    ped-venda.cond-redespa = "" 
                    ped-venda.cidade-cif = c-cidade-cif .
        
          find first ped-venda where
               rowid(ped-venda) = gr-ped-venda
               NO-LOCK no-error.
          IF AVAIL ped-venda THEN
              
           IF  valid-handle(gwh-frame) AND valid-handle(gwh-bt-next) THEN DO:
             


              
             RUN repositionRecord IN gwh-frame (gr-ped-venda).
        
           END.
          
   END.
   IF l-copia-efetuada = no THEN
       RETURN "NOK".
END.
ELSE
DO:
    RUN utp/ut-msgs.p (INPUT "SHOW":U, INPUT 366, INPUT "Posicione no cabe‡alho para copia, Pedido":U).
    IF  valid-handle(gwh-frame) THEN  APPLY "ENTRY" TO gwh-frame.

END.


