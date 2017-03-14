/***********************************************************************************
**
**       Programa: wsp/wscrm0100a.p
**
**       Data....: Maio/2016.
** 
**       Autor...: Rodrigo - DLC Tecnologia.
**
**       Objetivo: Criaá∆o pedido de Venda CRM
**
************************************************************************************/
{bf/buffersUni2.i}

    
DEF INPUT PARAMETER p-cod-usuar AS CHAR NO-UNDO.
DEF INPUT PARAMETER p-nomeAbrev AS CHAR NO-UNDO.
DEF INPUT PARAMETER p-nrPedcli  AS CHAR NO-UNDO.

DEF BUFFER b-ws-ped-venda FOR ws-ped-venda.
DEF BUFFER b-ws-ped-item  FOR ws-ped-item.
DEF BUFFER bf-ped-venda   FOR ped-venda.
DEF BUFFER b-if-ped-venda FOR if-ped-venda.
DEF BUFFER b-ped-venda    FOR ped-venda.
DEF BUFFER b2-ped-venda   FOR ped-venda.
DEF BUFFER b3-ped-venda   FOR ped-venda.
DEF BUFFER c-ws-ped-venda FOR ws-ped-venda.
DEF BUFFER d-ws-ped-venda FOR ws-ped-venda.
def buffer c-ws-ped-item  for ws-ped-item.
def buffer d-ws-ped-item  for ws-ped-item.
DEF BUFFER b-ped-compl    FOR ped-venda.

DEFINE NEW GLOBAL SHARED VAR c_descr_erro AS CHAR     NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR log_registro AS LOGICAL  NO-UNDO.
DEF VAR i-nr-pedido           AS INT                  NO-UNDO.
DEF VAR i-nr-pedido-next      AS INT                  NO-UNDO.
DEF VAR c-nr-pedcli           AS CHAR                 NO-UNDO.
def var r-ped-venda           as rowid                no-undo.
def var h-bodi157             as handle               no-undo.
DEF VAR l-produto             AS LOG                  NO-UNDO.
DEF VAR l-servico             AS LOG                  NO-UNDO.
def var i-cont                as int                  no-undo.
def var l-nr                  as log                  no-undo.
def var l-cli                 as log                  no-undo.
def var i-nr-ini              as int                  no-undo.
DEF VAR i-total-registros     AS INT NO-UNDO.

DEF VAR c-num-pedido          AS CHAR                 NO-UNDO.
DEF VAR c-data-emissao        AS CHAR                 NO-UNDO.
DEF VAR c-complemento         AS CHAR                 NO-UNDO.
DEF VAR c-cod-unid-atend      AS CHAR NO-UNDO.
DEF VAR de-preco-pai   AS DEC NO-UNDO.
DEF VAR de-preco-filho AS DEC NO-UNDO.

DEF VAR i-perc AS DEC EXTENT 6 NO-UNDO.
DEF VAR l-ctrl-tot as logical no-undo.

DEFINE TEMP-TABLE tt-registros LIKE ws-ped-item-imposto.
DEF VAR c-campo-aux AS CHAR NO-UNDO.
DEF VAR d-campo-aux AS DECIMAL NO-UNDO.
DEF VAR de-pis-valor AS DECIMAL.
DEF VAR de-cofins-valor AS DECIMAL.
DEF VAR l-campo-aux AS LOGICAL NO-UNDO.
DEF VAR l-update-cabec AS LOGICAL NO-UNDO.

DEFINE VARIABLE i-nr-ped-aux AS INTEGER     NO-UNDO.
def var l-cria-item     as log init no                      no-undo.
DEF VAR de-imp-aux  AS DEC FORMAT '->>>,>>>,>>9.9999'NO-UNDO.
DEF VAR d-campo-aux2 AS DECIMAL NO-UNDO.
DEF VAR d-campo-aux3 AS DECIMAL NO-UNDO.
DEF VAR d-preco-icms AS DECIMAL NO-UNDO.

/* Buffers para apagar os dados */
DEF BUFFER bf_ped_venda_cot1 FOR ped-venda.
DEF BUFFER bf_ped_venda_cot2 FOR ped-venda.
DEF BUFFER bf_if_ped_venda   FOR if-ped-venda.

DEFINE VARIABLE h-bodi159com AS HANDLE      NO-UNDO.
DEFINE VARIABLE rw-ped-venda AS ROWID       NO-UNDO.
DEFINE VARIABLE row-ped-item AS ROWID       NO-UNDO.
DEFINE VARIABLE bo-ped-item-cal AS HANDLE      NO-UNDO.

{ungws/wscrm0100a.i} /* Definiá∆o variaveis e temp-tables pedidos */

/* Necess†rio para calcular imposto */
DEFINE NEW GLOBAL SHARED VARIABLE i-pais-impto-usuario AS INTEGER FORMAT ">>9"       NO-UNDO.
ASSIGN i-pais-impto-usuario = 1.

/* ----------------------Instanciaondo as BO'S ---------------- */

/* Localiza os parametros do sistema e especifico ws-param */
ASSIGN log_registro = FALSE.

FIND FIRST para-ped NO-LOCK NO-ERROR.
FIND FIRST ws-param NO-LOCK NO-ERROR.

FIND FIRST b-ws-ped-venda 
     WHERE b-ws-ped-venda.nome-abrev = p-nomeAbrev
       AND b-ws-ped-venda.nr-pedcli  = p-nrPedcli 
       AND b-ws-ped-venda.origem     = "CRM" 
       AND b-ws-ped-venda.processado = 0 EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL b-ws-ped-venda AND 
         b-ws-ped-venda.cod-estabel <> "" THEN DO:

    FIND FIRST estabelec WHERE estabelec.cod-estabel = b-ws-ped-venda.cod-estabel NO-LOCK NO-ERROR.
    IF NOT AVAIL estabelec THEN FIND FIRST estabelec WHERE estabelec.cod-estabel = para-ped.estab-padrao NO-LOCK NO-ERROR.

    ASSIGN i-nr-pedido = 0
           c-cod-unid-atend = "".
    ASSIGN b-ws-ped-venda.erros = "".

    FIND emitente NO-LOCK
        WHERE emitente.nome-abrev = p-nomeAbrev NO-ERROR.
    IF AVAIL emitente THEN DO:
        {ungws/wscrm0100a2.i}
        
        FIND FIRST loc-entr NO-LOCK 
             WHERE loc-entr.nome-abrev = emitente.nome-abrev NO-ERROR.

        /* Representante a usar */
        IF b-ws-ped-venda.no-ab-reppri <> "" THEN
            FIND FIRST repres WHERE repres.nome-abrev = b-ws-ped-venda.no-ab-reppri NO-LOCK NO-ERROR.
        IF NOT AVAIL repres OR b-ws-ped-venda.no-ab-reppri = "" THEN
            FIND repres WHERE repres.cod-rep = emitente.cod-rep NO-LOCK NO-ERROR.

        /* Sequencia nr-pedido */
        FIND LAST ped-venda use-index ch-pedseq NO-LOCK no-error.   
        /*if avail ped-venda then assign i-nr-pedido = ped-venda.nr-pedido.
        ASSIGN i-nr-pedido = i-nr-pedido + 1.*/

        /* Checar dados sobre Natureza de Operacao */
        IF b-ws-ped-venda.nat-operacao = '' THEN
            ASSIGN b-ws-ped-venda.nat-operacao = emitente.nat-operacao.
        IF b-ws-ped-venda.nat-operacao <> '' THEN
            find FIRST natur-oper WHERE natur-oper.nat-operacao = b-ws-ped-venda.nat-operacao no-lock no-error.
        ELSE DO:
            IF emitente.estado = estabelec.estado THEN
                find FIRST natur-oper WHERE natur-oper.nat-operacao = ws-param.nat-operacao no-lock no-error.
            ELSE IF emitente.estado = "EX"  THEN 
                find FIRST natur-oper WHERE natur-oper.nat-operacao = ws-param.nat-operacao-3 no-lock no-error.
            ELSE 
                find FIRST natur-oper WHERE natur-oper.nat-operacao = ws-param.nat-operacao-2 no-lock no-error.
        END.
        
        assign l-ctrl-tot = true.

        /* Considerar campos operacao I - incluir U A  - Alterar */ 
        /* ws-ped-venda.operacao = "I" */
        /* Verifica se existe o pedido para alterar os dados do cabeáalho */
        assign l-update-cabec = FALSE.

        FIND FIRST bf-ped-venda NO-LOCK
             WHERE bf-ped-venda.nome-abrev = p-nomeAbrev
               AND bf-ped-venda.nr-pedcli  = p-nrPedcli NO-ERROR.

        ASSIGN c_descr_erro = "".

        IF AVAIL natur-oper THEN DO:

            CREATE ttPedVenda.
            IF AVAIL bf-ped-venda THEN DO: 
                BUFFER-COPY bf-ped-venda TO ttPedVenda.
                ASSIGN i-nr-pedido = ttPedVenda.nr-pedido.
            END.
            ELSE i-nr-pedido = next-value(seq-nr-pedido).
			
			FIND FIRST cond-pagto NO-LOCK
			     WHERE cond-pagto.cod-cond-pag = b-ws-ped-venda.cod-cond-pag NO-ERROR.

            ASSIGN ttPedVenda.cod-estabel          = b-ws-ped-venda.cod-estabel WHEN b-ws-ped-venda.cod-estabel <> ''
                   ttPedVenda.cod-estabel          = para-ped.estab-padrao      WHEN b-ws-ped-venda.cod-estabel = ''
                   ttPedVenda.nome-abrev           = emitente.nome-abrev
                   ttPedVenda.nr-pedcli            = b-ws-ped-venda.nr-pedcli
                   ttPedVenda.nr-pedrep            = b-ws-ped-venda.nr-pedcli
                   ttPedVenda.dt-emissao           = b-ws-ped-venda.dt-emissao
                   ttPedVenda.dt-implant           = TODAY /* b-ws-ped-venda.dt-implant  */
                   ttPedVenda.dt-entorig           = b-ws-ped-venda.dt-entorig
                   ttPedVenda.dt-entrega           = b-ws-ped-venda.dt-entrega
                   ttPedVenda.nat-operacao         = natur-oper.nat-operacao          
                   ttPedVenda.cod-cond-pag         = b-ws-ped-venda.cod-cond-pag WHEN b-ws-ped-venda.cod-cond-pag <> 0
                   ttPedVenda.cod-cond-pag         = ws-param.cod-cond-pag       WHEN b-ws-ped-venda.cod-cond-pag = 0
				   /*********************************************************/
				   /* 04/11/2014 - Marcelo solicitou que somente usasse a informacao da tabela cond-pagto
                   ttPedVenda.nr-tab-finan         = b-ws-ped-venda.nr-tab-fin   WHEN b-ws-ped-venda.nr-tab-fin <> 0
                   ttPedVenda.nr-tab-finan         = ws-param.nr-tab-fin         WHEN b-ws-ped-venda.nr-tab-fin = 0
                   ttPedVenda.nr-ind-finan         = b-ws-ped-venda.nr-ind-finan WHEN b-ws-ped-venda.nr-ind-finan <> 0
                   ttPedVenda.nr-ind-finan         = ws-param.nr-ind-finan       WHEN b-ws-ped-venda.nr-ind-finan = 0
				   */
                   ttPedVenda.nr-tab-finan         = cond-pagto.nr-tab-finan
                   ttPedVenda.nr-ind-finan         = cond-pagto.nr-ind-finan
				   /*********************************************************/
                   ttPedVenda.nr-tabpre            = b-ws-ped-venda.nr-tabpre    WHEN b-ws-ped-venda.nr-tabpre <> ''
                   ttPedVenda.nr-tabpre            = ws-param.nr-tabpre          WHEN b-ws-ped-venda.nr-tabpre = ''
                   ttPedVenda.tp-pedido            = IF b-ws-ped-venda.tp-pedido = "140000001" THEN "E" ELSE "P"
                   ttPedVenda.cod-canal-venda      = b-ws-ped-venda.cod-canal-venda WHEN b-ws-ped-venda.cod-canal-venda <> 0
                   ttPedVenda.cod-canal-venda      = ws-param.cod-canal-venda       WHEN b-ws-ped-venda.cod-canal-venda = 0
                   ttPedVenda.cod-priori           = 0
                   ttPedVenda.cod-entrega          = b-ws-ped-venda.cod-entrega  WHEN b-ws-ped-venda.cod-entrega <> ''
                   ttPedVenda.cod-entrega          = ws-param.cod-entrega        WHEN b-ws-ped-venda.cod-entrega = ''

                   ttPedVenda.local-entreg         = loc-entr.endereco
                   ttPedVenda.Bairro               = loc-entr.bairro
                   ttPedVenda.Cidade               = loc-entr.cidade
                   ttPedVenda.Estado               = loc-entr.estado
                   ttPedVenda.Cep                  = loc-entr.cep
                   ttPedVenda.pais                 = loc-entr.pais
                   ttPedVenda.Cgc                  = loc-entr.cgc        
                   ttPedVenda.ins-estadual         = loc-entr.ins-estadual       

                   ttPedVenda.cond-espec           = "1"
                   ttPedVenda.cod-portador         = b-ws-ped-venda.cod-portador WHEN b-ws-ped-venda.cod-portador <> 0
                   ttPedVenda.cod-portador         = emitente.portador         WHEN (b-ws-ped-venda.cod-portador = 0 OR b-ws-ped-venda.cod-portador = ?)
                   ttPedVenda.Modalidade           = b-ws-ped-venda.modalidade   WHEN b-ws-ped-venda.modalidade <> 0
                   ttPedVenda.Modalidade           = emitente.modalidade       WHEN b-ws-ped-venda.modalidade = 0
                   ttPedVenda.Observacoes          = b-ws-ped-venda.observacoes
                   ttPedVenda.nome-transp          = b-ws-ped-venda.nome-transp WHEN b-ws-ped-venda.nome-transp <> ''
                   ttPedVenda.nome-transp          = ws-param.nome-transp     WHEN b-ws-ped-venda.nome-transp = ''
                   ttPedVenda.tp-preco             = 1    
                   ttPedVenda.mo-codigo            = b-ws-ped-venda.int-1     /* 23/08/2015 - Campo moeda para integraá∆o do pedido */
                   ttPedVenda.mo-codigo            = para-ped.moeda-padrao    WHEN ttPedVenda.mo-codigo = ? /* 23/08/2015 caso nao venha do CRM, considerar Padrao */

                   ttPedVenda.no-ab-reppri         = IF AVAIL repres THEN repres.nome-abrev ELSE ""
                   ttPedVenda.cod-emitente         = emitente.cod-emit     
                   ttPedVenda.estab-atend          = "" /* b-ws-ped-venda.cod-estabel WHEN b-ws-ped-venda.cod-estabel <> '' */
                   /* ttPedVenda.estab-atend          = ws-param.cod-estabel     WHEN b-ws-ped-venda.cod-estabel = '' */

                   ttPedVenda.Origem               = 4   
                   ttPedVenda.dsp-pre-fat          = YES 
                   ttPedVenda.ind-fat-par          = YES 
                   ttPedVenda.cod-mensagem         = natur-oper.cod-mensagem 
                   ttPedVenda.tp-pedido            = "4"
                   ttPedVenda.cod-unid-neg         = b-ws-ped-venda.cod-unid-neg WHEN b-ws-ped-venda.cod-unid-neg <> ''
                   ttPedVenda.cod-unid-neg         = ws-param.cod-unid-neg     WHEN b-ws-ped-venda.cod-unid-neg = ''
                   ttPedVenda.Cidade-Cif           = b-ws-ped-venda.cidade-cif
                   ttPedVenda.nome-prog            = "WSCRM0105"
                   ttPedVenda.nome-tr-red          = b-ws-ped-venda.nome-tr-red
                   ttPedVenda.nome-abrev-tri       = b-ws-ped-venda.nome-abrev-tri.

            /* Include para campos e validacoes do cliente */
            {ungws/wscrm0100b.i} 

            ASSIGN ttPedVenda.nr-pedido = i-nr-pedido.

            /* Altera pedido de venda */
            IF AVAIL bf-ped-venda THEN DO:
                assign l-update-cabec = true.
                run emptyRowErrors  IN hDBOPedVenda.
                RUN openQueryStatic IN hDBOPedVenda(input "Main":U).
                run getRowErrors    IN hDBOPedVenda(output table RowErrors).
                RUN gotokey         IN hDBOPedVenda(INPUT ttpedvenda.nome-abrev,
                                                    INPUT ttpedvenda.nr-pedcli).
                RUN repositionrecord IN hDBOPedVenda(INPUT ROWID(bf-ped-venda)).
                RUN setRecord        in hDBOPedVenda(input table ttpedvenda).
                RUN updaterecord     IN hDBOPedVenda.
                run getRowErrors    IN hDBOPedVenda(output table RowErrors).
                
                IF CAN-FIND (FIRST RowErrors) THEN DO:
                   FOR EACH RowErrors:
                        ASSIGN b-ws-ped-venda.erros = b-ws-ped-venda.erros + string(RowErrors.ErrorSequence)
                                     + "|" + STRING(RowErrors.ErrorNumber) + "|" + 
                                    replace(RowErrors.ErrorDESCRIPTION,";",",") + "|" + /* replace(RowErrors.ErrorHELP,";",",")*/ ";".
                   END.
                   {ungws/wscrm0100a-msg.i}
                END.

                /* Manda atualizar o Item */
                FOR EACH b-ws-ped-item 
                   WHERE b-ws-ped-item.nome-abrev = b-ws-ped-venda.nome-abrev 
                     AND b-ws-ped-item.nr-pedcli  = b-ws-ped-venda.nr-pedcli 
                     AND b-ws-ped-item.origem     = "CRM" 
                     AND b-ws-ped-item.processado = 0 EXCLUSIVE-LOCK:
                    
                    /* Atualiza o registro do item novamente */
                    RUN ungws\wscrm0100a-sep.p (INPUT b-ws-ped-item.nome-abrev,
                                                INPUT b-ws-ped-item.nr-pedcli,
                                                INPUT b-ws-ped-item.nr-sequencia,
                                                INPUT b-ws-ped-item.it-codigo,
                                                INPUT ROWID(b-ws-ped-item),
                                                INPUT NO).
                END.
            END.
            /* Criando pedido novo */
            ELSE DO:
                /* -----------  Limpa as temp tables de comunicacao -------------- */
                RUN emptyRowErrors     IN  hDBOPedVenda   NO-ERROR.
                RUN emptyRowObject     IN  hDBOPedVenda   NO-ERROR.
                RUN emptyRowObjectAux  IN  hDBOPedVenda   NO-ERROR.

                RUN newRecord          IN  hDBOPedVenda   NO-ERROR.
                RUN setRecord          IN  hDBOPedVenda  (INPUT TABLE ttPedVenda)  NO-ERROR.
                RUN validateRecord     IN  hDBOPedVenda  (INPUT "Create" )         NO-ERROR. 


                /* ---------------  Cria os pedidos no EMS --------------- */
                RUN createRecord    IN  hDBOPedVenda   NO-ERROR. 
                RUN getRecord       IN  hDBOPedVenda   (OUTPUT TABLE ttPedVenda) NO-ERROR.
                RUN getRowid        IN  hDBOPedVenda   (OUTPUT rPedVenda)  NO-ERROR .
                run getRowErrors    IN  hDBOPedVenda(output table RowErrors).

                IF CAN-FIND (FIRST RowErrors) THEN DO:
                   FOR EACH RowErrors:
                        ASSIGN b-ws-ped-venda.erros = b-ws-ped-venda.erros + string(RowErrors.ErrorSequence)
                                     + "|" + STRING(RowErrors.ErrorNumber) + "|" + 
                                    replace(RowErrors.ErrorDESCRIPTION,";",",") + "|" + /* replace(RowErrors.ErrorHELP,";",",")*/ ";".
                   END.
                   {ungws/wscrm0100a-msg.i}
                END.

                run dibo/bodi157.r persistent set h-bodi157.  
                run createOrdersRepresentatives in h-bodi157 (input rPedVenda).
                delete procedure h-bodi157.
                FIND FIRST ttpedvenda.
                IF NOT CAN-FIND (FIRST RowErrors) THEN DO:

                    ASSIGN nr-seq = 0
                           l-cria-item = no.
                    FOR EACH b-ws-ped-item WHERE
                             b-ws-ped-item.nome-abrev = b-ws-ped-venda.nome-abrev AND
                             b-ws-ped-item.nr-pedcli  = b-ws-ped-venda.nr-pedcli AND
                             b-ws-ped-item.origem     = "CRM" AND
                             b-ws-ped-item.processado = 0 EXCLUSIVE-LOCK BREAK BY b-ws-ped-item.nr-sequencia:
                        
                        IF FIRST-OF(b-ws-ped-item.nr-sequencia) THEN DO:
                            FIND FIRST item WHERE item.it-codigo = b-ws-ped-item.it-codigo NO-LOCK NO-ERROR.

                            IF b-ws-ped-item.nr-sequencia <> 0 THEN ASSIGN nr-seq = b-ws-ped-item.nr-sequencia.
                                                               ELSE ASSIGN nr-seq = nr-seq  + 10.

                            /* 20/07/2015 */
                            find first ped-item
                                where ped-item.Nome-abrev          = b-ws-ped-item.nome-abrev
                                and   ped-item.nr-pedcli           = b-ws-ped-item.nr-pedcli 
                                and   ped-item.it-codigo           = b-ws-ped-item.it-codigo 
                                and   ped-item.nr-sequencia        = b-ws-ped-item.nr-sequencia
                                no-lock no-error.
                            if avail ped-item then 
                                next.
                            /* 20/07/2015 */

                            RUN emptyRowErrors     IN  hDBOPedItem   NO-ERROR.
                            RUN emptyRowObject     IN  hDBOPedItem   NO-ERROR.
                            RUN emptyRowObjectAux  IN  hDBOPedItem   NO-ERROR.

                            CREATE ttPedItem.
                            ASSIGN ttPedItem.Nome-abrev          = ttPedVenda.nome-abrev
                                   ttPedItem.nr-pedcli           = ttPedVenda.nr-pedcli 
                                   ttPedItem.it-codigo           = b-ws-ped-item.it-codigo .     

                            RUN inputTable      IN  hDBOPedItemSdf  (INPUT TABLE ttPedItem) NO-ERROR.       
                            RUN setDefaultItem  IN  hDBOPedItemSdf   NO-ERROR .
                            RUN outputTable     IN  hDBOPedItemSdf  (OUTPUT TABLE ttPedItem) NO-ERROR.       

                            FIND FIRST ttPedItem .
                            ASSIGN ttPedItem.nr-sequencia            = nr-seq 
                                   ttPedItem.dt-entorig              = b-ws-ped-item.dt-entrega /* dt-entorig */
                                   ttPedItem.dt-entrega              = b-ws-ped-item.dt-entrega
                                   ttPedItem.qt-pedida               = b-ws-ped-item.qt-pedida
                                   ttPedItem.qt-un-fat               = ttPedItem.qt-pedida
                                   ttPedItem.tp-preco = 1
                                   ttPedItem.vl-pretab               = b-ws-ped-item.vl-preuni
                                   ttPedItem.vl-preori               = b-ws-ped-item.vl-preuni /* Sera o valor com encargos enviado pelo CRM */
                                  /* ttPedItem.vl-preuni               = b-ws-ped-item.vl-preuni  */
                                   ttPedItem.des-pct-desconto-inform = '0'
                                   ttPedItem.nat-operacao            = ttPedVenda.nat-operacao 
                                   ttPedItem.cod-entrega             = ttPedVenda.cod-entrega
                                   ttPedItem.tipo-atend              = 2
                                   ttPedItem.Observacao              = b-ws-ped-item.observacao
                                   ttPedItem.val-preco-suger         = b-ws-ped-item.dec-1 /* 13/08/2015 - valor original do item sem os impostos */
                                   ttPedItem.cod-unid-neg            = item.cod-unid-neg. /*ttPedVenda.cod-unid-neg. */
                                   
                            if ttPedItem.dt-entrega = ? then assign ttPedItem.dt-entrega = ttPedVenda.dt-entrega.
                            if ttPedItem.dt-entorig = ? then assign ttPedItem.dt-entorig = ttPedItem.dt-entrega.
                            IF ttPedItem.Observacao = "" THEN ASSIGN ttPedItem.Observacao = ".".

                            RUN pi-icms-pis-cofins.

                            /* 11/12/2014 Preencher ttPedItem.cod-refer = "NORMAL" 
                                quando item nao configuravel (politiva <> 5 )
                                     e item tipo controle est de referencia (tipo-con-est = 4) */
                            IF ITEM.politica <> 5 
                                AND ITEM.tipo-con-est = 4 THEN
                                ASSIGN ttPedItem.cod-refer = "NORMAL".
                            
                            /* Configurado para pegar o configurador */
                            IF ITEM.politica = 5 THEN
                                ASSIGN ttPedItem.cod-refer = "00000001".
                            
                            /*02/06/2015*/
                            find first ped-item
                                where ped-item.Nome-abrev          = ttpeditem.nome-abrev
                                and   ped-item.nr-pedcli           = ttpeditem.nr-pedcli 
                                and   ped-item.it-codigo           = ttpeditem.it-codigo 
                                and   ped-item.nr-sequencia        = ttpeditem.nr-sequencia
                                and   ped-item.cod-refer           = ttpeditem.cod-refer
                                no-lock no-error.
                            if avail ped-item then do:
                                delete ttpeditem.
                                next.
                            end.
                            /*02/06/2015*/

                            /* ----------------------- Cria o registro ---------------------- */
                            RUN newRecord          IN  hDBOPedItem   NO-ERROR.
                            RUN setRecord          IN  hDBOPedItem  (INPUT TABLE ttPedItem)  NO-ERROR.
                            RUN setRecord          IN  hDBOPedItem  (INPUT TABLE ttPedItem)  NO-ERROR.
                            RUN validateRecord     IN  hDBOPedItem  (INPUT "Create" )        NO-ERROR. 
                            IF RETURN-VALUE  = "OK":U  THEN DO:

                                RUN createRecord    IN  hDBOPedItem   NO-ERROR. 
                                RUN getRecord       IN  hDBOPedItem   (OUTPUT TABLE ttPedItem) NO-ERROR.

                                FIND FIRST ttPedItem .
                                ASSIGN b-ws-ped-item.nr-pedido    = ttPedVenda.nr-pedido
                                       b-ws-ped-item.nr-sequencia = nr-seq.

                                IF AVAIL ITEM AND ITEM.politica = 5 THEN DO:
                                    if not valid-handle(bo-ped-item-cal) or
                                         bo-ped-item-cal:type <> "PROCEDURE":U or
                                         bo-ped-item-cal:file-name <> "dibo/bodi154cal.p":U then
                                         run dibo/bodi154cal.p persistent set bo-ped-item-cal.                   

                                    run getRowid in hDBOPedItem(output row-ped-item).
                                    run setItemConfiguration in bo-ped-item-cal (input row-ped-item,
                                                                                 input 1).

                                    delete procedure bo-ped-item-cal.
                                    assign bo-ped-item-cal = ?.
                                END.

                                /* Executa programa pos gravacao do item */
                                RUN ungws/wscrm0100d.p (INPUT ttPedItem.nome-abrev,
                                                        INPUT ttPedItem.nr-pedcli,
                                                        INPUT ttPedItem.nr-sequencia,
                                                        INPUT ttPedVenda.nr-pedido).

                                ASSIGN b-ws-ped-item.processado   = 1. /* Indicar que foi processado */
                            END.
                            ELSE DO:
                                /* Registrar erro dos item no pedido */
                                RUN getRowErrors       IN  hDBOPedItem  (OUTPUT TABLE RowErrors)  NO-ERROR.
                                IF CAN-FIND (FIRST RowErrors) THEN DO:
                                    assign l-ctrl-tot = false.
                                    IF AVAIL b-ws-ped-venda THEN DO:
                                        FOR EACH RowErrors:
                                            ASSIGN b-ws-ped-venda.erros = b-ws-ped-venda.erros + string(RowErrors.ErrorSequence)
                                                 + "|" + STRING(RowErrors.ErrorNumber) + "|" + 
                                                replace(RowErrors.ErrorDESCRIPTION,";",",") + "(" + ttPedItem.it-codigo + ")|" /*+ replace(RowErrors.ErrorHELP,";",",")*/ + ";".
                                        END.
                                        {ungws/wscrm0100a-msg.i}
                                    END.
                                END.
                            END.
                            DELETE ttPedItem.
                        END.
                    END. /*for each tabela de item*/
                END.
                ELSE DO:
                    RUN getRowErrors   IN  hDBOPedVenda  (OUTPUT TABLE RowErrors)  NO-ERROR.
                    IF CAN-FIND (FIRST RowErrors) THEN DO:
                        assign l-ctrl-tot = false.
                        IF AVAIL b-ws-ped-venda THEN DO:
                            FOR EACH RowErrors:
                                ASSIGN b-ws-ped-venda.erros = b-ws-ped-venda.erros + string(RowErrors.ErrorSequence)
                                     + "|" + STRING(RowErrors.ErrorNumber) + "|" + 
                                    replace(RowErrors.ErrorDESCRIPTION,";",",") + "|" + /* replace(RowErrors.ErrorHELP,";",",")*/ ";".
                            END.
                            {ungws/wscrm0100a-msg.i}
                        END.
                    END.
                END.
            END.

            /* Valida dados para controle especifico do cliente (if-ped-venda) */
            EMPTY TEMP-TABLE RowErrors.
            FIND FIRST ped-venda              
                 WHERE ped-venda.nome-abrev = ttPedVenda.nome-abrev
                   AND ped-venda.nr-pedcli  = ttpedvenda.nr-pedcli EXCLUSIVE-LOCK NO-ERROR.
            IF c_descr_erro = "" AND AVAIL ped-venda THEN DO:
                /* registra dados especificos do CABEÄALHO */
                {ungws/wscrm0100e.i}
            END.
            ELSE DO:
                RUN pi-cria-rowerrors (INPUT ENTRY(2,c_descr_erro,"|"),
                                       INPUT ENTRY(3,c_descr_erro,"|"),
                                       INPUT "").
            END.

            FOR EACH RowErrors:
                ASSIGN b-ws-ped-venda.erros = b-ws-ped-venda.erros + string(RowErrors.ErrorSequence)
                     + "|" + STRING(RowErrors.ErrorNumber) + "|" + 
                    replace(RowErrors.ErrorDESCRIPTION,";",",") + "|" + /* replace(RowErrors.ErrorHELP,";",",")*/ ";".
            END.
            {ungws/wscrm0100a-msg.i}

            IF NOT CAN-FIND (FIRST RowErrors WHERE RowErrors.ErrorNumber = 15825) THEN DO:
                FIND FIRST ped-venda
                     WHERE ped-venda.nome-abrev = ttPedVenda.nome-abrev
                       AND ped-venda.nr-pedcli  = ttpedvenda.NR-PEDCLI NO-LOCK NO-ERROR.

                IF (c_descr_erro = "") AND AVAIL ped-venda THEN DO:
                    FIND FIRST b-ped-compl
                         WHERE b-ped-compl.nr-pedido = ped-venda.nr-pedido EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAIL b-ped-compl THEN DO:
                        ASSIGN b-ped-compl.completo = NO.
                        RELEASE b-ped-compl.
                    END.

                    /* registra dados especificos do CABEÄALHO */
                    {ungws/wscrm0100f.i} 

                    EMPTY TEMP-TABLE RowErrors  NO-ERROR.
                    RUN setUserLog      IN hDBoComPedVenda (INPUT p-cod-usuar). 
                    RUN emptyRowErrors  IN hDBoComPedVenda.
                    RUN completeorder   IN hDBoComPedVenda (INPUT ROWID(ped-venda),
                                                            OUTPUT TABLE rowerrors).

                    
                    FOR EACH RowErrors:
                        CREATE ws-ped-venda-msg.
                        ASSIGN  ws-ped-venda-msg.nome-abrev = ttPedVenda.nome-abrev
                                ws-ped-venda-msg.nr-pedcli  = ttPedVenda.nr-pedcli
                                ws-ped-venda-msg.origem     = "ERP"
                                ws-ped-venda-msg.operacao   = "I"  /* 12/12/2014 alterado para gerar com I. */
                                ws-ped-venda-msg.processado = 0
                                ws-ped-venda-msg.situacao   = 1
                                ws-ped-venda-msg.data-operacao = NOW
                                ws-ped-venda-msg.ErrorSequence = RowErrors.ErrorSequence
                                ws-ped-venda-msg.ErrorNumber   = RowErrors.ErrorNumber
                                ws-ped-venda-msg.ErrorDescription = substr(RowErrors.ErrorDescription,1,100)
                                ws-ped-venda-msg.ErrorHelp        = RowErrors.ErrorHelp.
                    END.
                    
                    FIND FIRST ttPedVenda.

                    ASSIGN b-ws-ped-venda.nr-pedido  = ttPedVenda.nr-pedido
                           b-ws-ped-venda.erros      = "OK. Pedido Implantado com sucesso!"
                           b-ws-ped-venda.dt-implant = ttPedVenda.dt-implant
                           b-ws-ped-venda.processado = 1. /* Indicar que foi processado */
                    /* MANDA EFETIVAR DE NOVO PARA GARANTIR QUE CALCULA IMPOSTO */
                    FIND FIRST b-ped-compl
                         WHERE b-ped-compl.nr-pedido = ped-venda.nr-pedido EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAIL b-ped-compl THEN DO:
                        ASSIGN b-ped-compl.completo = NO.

                        EMPTY TEMP-TABLE RowErrors  NO-ERROR.
                        RUN setUserLog      IN hDBoComPedVenda (INPUT p-cod-usuar). 
                        RUN emptyRowErrors  IN hDBoComPedVenda.
                        RUN completeorder   IN hDBoComPedVenda (INPUT ROWID(b-ped-compl),
                                                                OUTPUT TABLE rowerrors).
                    END.
                END.
            END.

            /* Atualiza registros para caso necessario elimina o registros */
            RUN pi-valida-pedido. 

            DELETE ttPedVenda.
        END. /* natur-oper */
        {ungws/wscrm0100a3.i}
    END. /* emitente */
END.

/* 06/11/2014 - Marcar todos os registros relacionados ao pedido como processado */
RUN pi-processa-registros.

ASSIGN log_registro = FALSE.
/* ----------------------Delete das BO'S ---------------- */

RETURN "OK".
/* fim do programa */

PROCEDURE pi-valida-pedido:
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

    FIND FIRST ped-venda
         WHERE ped-venda.nome-abrev = ttPedVenda.nome-abrev
           AND ped-venda.nr-pedcli  = ttpedvenda.nr-pedcli EXCLUSIVE-LOCK NO-ERROR.
    
    
    /* Marcar registros como processados e caso de delete no orcamento */
    IF AVAIL ped-venda THEN DO:
        FIND FIRST estabelec NO-LOCK
            WHERE estabelec.cod-estabel = ws-ped-venda.cod-estabel NO-ERROR.
        /* Atualiza os registros como processados */
        FOR EACH ws-ped-item 
            WHERE ws-ped-item.nome-abrev = b-ws-ped-venda.nome-abrev 
              AND ws-ped-item.nr-pedcli  = b-ws-ped-venda.nr-pedcli
              AND ws-ped-item.origem     = b-ws-ped-venda.origem:
            assign ws-ped-item.nr-pedido  = ped-venda.nr-pedido WHEN AVAIL ped-venda
                   ws-ped-item.processado = 1.
        END.
        FOR EACH ws-ped-item-esp 
            WHERE ws-ped-item-esp.nome-abrev = b-ws-ped-venda.nome-abrev 
              AND ws-ped-item-esp.nr-pedcli  = b-ws-ped-venda.nr-pedcli
              AND ws-ped-item-esp.origem     = b-ws-ped-venda.origem:
            assign ws-ped-item-esp.nr-pedido  = ped-venda.nr-pedido WHEN AVAIL ped-venda
                   ws-ped-item-esp.processado = 1.
        END.
        /*FOR EACH ws-ped-item-esp-orc 
            WHERE ws-ped-item-esp-orc.nome-abrev = b-ws-ped-venda.nome-abrev 
              AND ws-ped-item-esp-orc.nr-pedcli  = b-ws-ped-venda.nr-pedcli
              AND ws-ped-item-esp-orc.origem     = b-ws-ped-venda.origem:
            assign l-orc.nr-pedido  = ped-venda.nr-pedido WHEN AVAIL ped-venda
                   ws-ped-item-esp-orc.processado = 1.
    
            IF b-ws-ped-venda.orcamento = 1 THEN DO:
                /* Elimina os registros das tabelas pd-config-pedido e pd-compl-pedido caso existam */
                find first pd-config-pedido 
                     where pd-config-pedido.ep-codigo    = estabelec.ep-codigo 
                       and pd-config-pedido.nr-pedido    = ped-venda.nr-pedido 
                       and pd-config-pedido.nr-sequencia = ws-ped-item-esp-orc.nr-sequencia no-error.
                IF AVAIL pd-config-pedido THEN DELETE pd-config-pedido.
                find first pd-compl-pedido 
                     where pd-compl-pedido.ep-codigo    = estabelec.ep-codigo 
                       and pd-compl-pedido.nr-pedido    = ped-venda.nr-pedido 
                       and pd-compl-pedido.nr-sequencia = ws-ped-item-esp-orc.nr-sequencia no-error.
                IF AVAIL pd-compl-pedido THEN DELETE pd-compl-pedido.
            END.
        END.*/

        assign b-ws-ped-venda.nr-pedido = ped-venda.nr-pedido WHEN AVAIL ped-venda
               b-ws-ped-venda.processado = 1
               b-ws-ped-venda.dt-implant = ped-venda.dt-implant WHEN AVAIL ped-venda.
        IF c_descr_erro <> "" THEN ASSIGN b-ws-ped-venda.erros = c_descr_erro.
    END.
    
    /* Elimina registro sem relacionamento na if-ped-venda (19/07/2014)*/
    FOR EACH if-ped-venda 
        WHERE if-ped-venda.nome-abrev = ttPedVenda.nome-abrev
          AND if-ped-venda.nr-pedcli  = ttPedVenda.nr-pedcli:
        IF if-ped-venda.nr-pedido-relac = 0 THEN DELETE if-ped-venda.
    END.

END PROCEDURE. /* pi-valida-pedido */

PROCEDURE pi-cria-rowerrors:
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

    DEFINE INPUT PARAMETER p-number AS INTEGER.
    DEFINE INPUT PARAMETER p-descr  AS CHAR.
    DEFINE INPUT PARAMETER p-help   AS CHAR.
    
    DEF VAR erro_seq AS INT INITIAL 1.
    
    FIND LAST rowerrors NO-ERROR.
    IF AVAIL rowerrors THEN erro_seq = RowErrors.ErrorSequence + 1.
    CREATE rowerrors.
    ASSIGN  RowErrors.ErrorSequence = erro_seq
            RowErrors.ErrorNumber   = p-number
            RowErrors.ErrorDESCRIPTION = p-descr
            RowErrors.ErrorHELP = p-help.

END PROCEDURE. /* pi-cria-rowerrors */

PROCEDURE pi-valid-campos :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

    DEFINE VARIABLE cListaEmb AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE l-parcial as logical     no-undo.

    if (ttPedVenda.cod-estabel = "422" OR ttPedVenda.cod-estabel = "412") and /*solic-318*/
        c-cod-unid-atend = "" then do:

        FOR FIRST ped-venda EXCLUSIVE-LOCK
            WHERE ped-venda.nome-abrev = ttPedVenda.nome-abrev
              AND ped-venda.nr-pedcli  = ttPedVenda.nr-pedcli
              and ped-venda.cod-estabel = ttPedVenda.cod-estabel:
    
            FIND FIRST if-ped-venda exclusive-LOCK
                 WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido no-error.
            if avail if-ped-venda then do:
                if if-ped-venda.nr-pedido-relac = 0  then do:
                    delete if-ped-venda.
                    return "ok".
                END.
                else assign c-cod-unid-atend = if-ped-venda.cod-estab-atend. 
            END.
        END.
    END.

    FOR FIRST ped-venda NO-LOCK
        WHERE ped-venda.nome-abrev = ttPedVenda.nome-abrev
          AND ped-venda.nr-pedcli  = ttPedVenda.nr-pedcli
          AND ped-venda.cod-estabel = ttPedVenda.cod-estabel
             /* AND ped-venda.completo   = YES*/,
        FIRST if-ped-venda NO-LOCK
        WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido:
    
        if (ped-venda.cod-estabel = "422" OR ped-venda.cod-estabel = "412") then do: /*solic-318*/
            IF if-ped-venda.cod-estab-atend <> c-cod-unid-atend
                and c-cod-unid-atend = "" THEN DO:
                RUN pi-cria-rowerrors (INPUT 15825,
                                       INPUT "N∆o Ç permitido alterar unidade de atendimento para branco pedidos efetivados que possuem incentivo fiscal.",
                                       INPUT "Informar Unidade de Atendimento valida.").
            END.
            /*IF c-cod-unid-atend <> ped-venda.cod-estabel THEN DO:
                RUN pi-cria-rowerrors (INPUT 15825,
                                       INPUT "N∆o Ç permitido alterar estabelecimento para pedidos efetivados que possuem incentivo fiscal.",
                                       INPUT "Informar Estabelecimento valido.").
            END.*/
            
            IF ttPedVenda.nat-operacao <> ped-venda.nat-operacao THEN DO:
                RUN pi-cria-rowerrors (INPUT 15825,
                                       INPUT "N∆o Ç permitido alterar natureza de operaá∆o para pedidos efetivados que possuem incentivo fiscal.",
                                       INPUT "Informar NAtureza de operaá∆o valida.").
            END.
        END.
    
        /* Completa Pedido */
        IF if-ped-venda.cod-estab-atend <> c-cod-unid-atend OR
           ped-venda.cod-estabel        <> ttPedVenda.cod-estabel  OR 
           ped-venda.nat-operacao       <> ttPedVenda.nat-operacao  THEN DO:
            
            FOR EACH pre-fatur NO-LOCK OF ped-venda:
                ASSIGN cListaEmb = cListaEmb + "," + STRING(pre-fatur.nr-embarque).
            END.
    
            IF cListaEmb <> "" THEN DO:
                ASSIGN cListaEmb = SUBSTRING(cListaEmb, 2).
                RUN pi-cria-rowerrors (INPUT 15825,
                                       INPUT "Pedido j† relacionado com o(s) embarque(s) " + cListaEmb + ". Favor liberar o pedido do(s) embarque(s) para efetuar este tipo de alteraá∆o.",
                                       INPUT "Pedido relacionado com embarque!!!").
            END.
            /* ASSIGN ped-venda.completo = NO. */
        END.
    
        IF ped-venda.nome-tr-red <> ttPedVenda.nome-tr-red THEN DO:
            FIND FIRST b-if-ped-venda NO-LOCK
                     WHERE rowid(b-if-ped-venda) = ROWID(if-ped-venda) NO-ERROR.
            DO WHILE AVAIL b-if-ped-venda:
                FIND FIRST b2-ped-venda exclusive-LOCK
                     WHERE b2-ped-venda.nr-pedido = b-if-ped-venda.nr-pedido-relac NO-ERROR.
        
                FIND FIRST b-if-ped-venda NO-LOCK
                     WHERE b-if-ped-venda.nr-pedido = b2-ped-venda.nr-pedido AND b-if-ped-venda.nr-pedido-relac  <> 0  NO-ERROR.
                
                 if avail  b2-ped-venda then ASSIGN b2-ped-venda.nome-tr-red  = ttPedVenda.nome-tr-red
                                                    b2-ped-venda.nome-trans   = ttPedVenda.nome-transp.
            END.
        END.
        if (ped-venda.cod-estabel <> "422" AND ped-venda.cod-estabel <> "412") then do: /*solic-318*/
            FOR FIRST b2-ped-venda EXCLUSIVE-LOCK
                WHERE b2-ped-venda.nome-abrev = ttPedVenda.nome-abrev
                  AND b2-ped-venda.nr-pedcli  = ttPedVenda.nr-pedcli,
                FIRST b-if-ped-venda NO-LOCK
                WHERE b-if-ped-venda.nr-pedido-relac = b2-ped-venda.nr-pedido:
    
                 if c-cod-unid-atend <> "" then do:
                     RUN pi-cria-rowerrors (INPUT 15825,
                                            INPUT "N∆o Ç permitido colocar unidade de atendimento em pedido relacionado, somente em pedido inicial.",
                                            INPUT "Unidade de atendimento invalida.").
                 END.
            END.
        END.
    END.
    
END PROCEDURE. /* pi-valid-campos */


PROCEDURE pi-troca-unid-polo :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

    define var l-fora as logical no-undo.

    define buffer buf-estabelec for estabelec .
    define buffer buf-emitente for emitente .
    define buffer b3-ped-item for ped-item.
    
    /* Troca Natureza e cliente para unigel comercial polo */
    FIND FIRST if-natur-oper
         WHERE if-natur-oper.cod-estab-orig  = ttPedVenda.cod-estabel
           AND if-natur-oper.cod-estab-inter = ""
           AND if-natur-oper.cod-estab-dest  = c-cod-unid-atend
           AND if-natur-oper.nat-oper-pedido = if-ped-venda.nat-oper-orig NO-LOCK NO-ERROR.
    IF AVAIL if-natur-oper THEN DO:
        ASSIGN ped-venda.nat-operacao = if-natur-oper.nat-oper-v-ung.

        FIND FIRST b3-ped-venda
             WHERE b3-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL b3-ped-venda THEN DO:
            ASSIGN b3-ped-venda.completo    = NO
                   b3-ped-venda.cod-estabel = if-natur-oper.cod-estab-dest.

            find first buf-estabelec where buf-estabelec.cod-estabel = b3-ped-venda.cod-estabel no-lock no-error.
            find first buf-emitente  where buf-emitente.cod-emitente = b3-ped-venda.cod-emitente no-lock no-error.
        
            l-fora = (buf-estabelec.estado <> buf-emitente.estado).
        
            ASSIGN b3-ped-venda.nat-operacao = (if l-fora and nat-oper-venda-inter <> "" then nat-oper-venda-inter else if-natur-oper.nat-oper-venda).
            for each b3-ped-item of b3-ped-venda exclusive-lock.
                assign b3-ped-item.nat-operacao = b3-ped-venda.nat-operacao. 
            end.
        END.

    END.
    else do:
           find FIRST if-ped-venda exclusive-LOCK
           WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido no-error.
                 
           if avail if-ped-venda then do:
                   assign c-cod-unid-atend = if-ped-venda.cod-estab-atend. 
                   RUN pi-cria-rowerrors (INPUT 15825,
                                          INPUT "N∆o Ç permitido alterar unidade de atendimento.",
                                          INPUT "Informar Unidade de Atendimento valida.").
           end.  
    end.
END PROCEDURE. /* pi-troca-unid-polo */


/* 06/11/2014 - Marcelo solicitou que tudo deve ser marcado como processado igual a 1  */
PROCEDURE pi-processa-registros:
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

    /* Atualiza os registros como processados */
    FOR EACH b-ws-ped-venda 
     WHERE b-ws-ped-venda.nome-abrev = p-nomeAbrev
       AND b-ws-ped-venda.nr-pedcli  = p-nrPedcli 
       AND b-ws-ped-venda.origem     = "CRM" 
       AND b-ws-ped-venda.processado = 0 EXCLUSIVE-LOCK:

        /* 09/04/2015 - caso nao seja atualizacao do item, marca como processado */
        if l-update-cabec = FALSE then DO:
            FOR EACH ws-ped-item 
                WHERE ws-ped-item.nome-abrev = b-ws-ped-venda.nome-abrev
                  AND ws-ped-item.nr-pedcli  = b-ws-ped-venda.nr-pedcli
                  AND ws-ped-item.origem     = b-ws-ped-venda.origem
                  AND ws-ped-item.processado = 0:
                assign ws-ped-item.processado = 1.
            END.
            FOR EACH ws-ped-item-esp 
                WHERE ws-ped-item-esp.nome-abrev = b-ws-ped-venda.nome-abrev 
                  AND ws-ped-item-esp.nr-pedcli  = b-ws-ped-venda.nr-pedcli
                  AND ws-ped-item-esp.origem     = b-ws-ped-venda.origem
                  AND ws-ped-item-esp.processado = 0:
                assign ws-ped-item-esp.processado = 1.
            END.
            /*FOR EACH ws-ped-item-esp-orc 
                WHERE ws-ped-item-esp-orc.nome-abrev = b-ws-ped-venda.nome-abrev 
                  AND ws-ped-item-esp-orc.nr-pedcli  = b-ws-ped-venda.nr-pedcli
                  AND ws-ped-item-esp-orc.origem     = b-ws-ped-venda.origem
                  AND ws-ped-item-esp-orc.processado = 0:
        
                assign ws-ped-item-esp-orc.processado = 1.
            END.*/
        
        end.
        ASSIGN b-ws-ped-venda.processado = 1.

        /* 04/12/2014 - Caso o pedido tenha a natureza de operacao ou a condicao de pagamento nula, retornar mensagem de erro */
        EMPTY TEMP-TABLE RowErrors.
        IF b-ws-ped-venda.nat-operacao = ? THEN DO:
            CREATE rowerrors.
            ASSIGN  RowErrors.ErrorSequence = 1
                    RowErrors.ErrorNumber   = 17006
                    RowErrors.ErrorDESCRIPTION = "Natureza de Operaá∆o n∆o informada."
                    RowErrors.ErrorHELP = "Informar a natureza de operaá∆o corretamente".
        END.
        IF b-ws-ped-venda.cod-cond-pag = ? THEN DO:
            CREATE rowerrors.
            ASSIGN  RowErrors.ErrorSequence = 2
                    RowErrors.ErrorNumber   = 17006
                    RowErrors.ErrorDESCRIPTION = "Condiá∆o de pagamento n∆o informada."
                    RowErrors.ErrorHELP = "Informar a condiá∆o de pagamento corretamente".
    
        END.
    END.

END PROCEDURE.

PROCEDURE pi-icms-pis-cofins:
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

    DEFINE VARIABLE de-vl-preori-imp  AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE de-aliquota-icm   AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE l-proc-ok-aux     AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE h-bodi317im1br    AS HANDLE      NO-UNDO.

    FIND FIRST estabelec NO-LOCK
         WHERE estabelec.cod-estabel = ttPedVenda.cod-estabel NO-ERROR.


    RUN dibo/bodi317im1br.p PERSISTENT SET h-bodi317im1br.
    run calculaAliquotaICMS in h-bodi317im1br(input  emitente.contrib-icms,
                                              input  emitente.natureza,
                                              input  estabelec.estado,
                                              input  estabelec.pais,
                                              input  ttPedVenda.estado,
                                              input  ttPedItem.it-codigo,
                                              input  ttPedItem.nat-operacao,
                                              output de-aliquota-icm, 
                                              output l-proc-ok-aux).
    DELETE PROCEDURE h-bodi317im1br.

    /* Demais impostos */
    FIND FIRST natur-oper NO-LOCK
         WHERE natur-oper.nat-operacao = ttPedVenda.nat-operacao NO-ERROR.
    IF AVAIL natur-oper THEN DO:
        ASSIGN de-vl-preori-imp = (ttPedItem.vl-preori / ((100 - (de-aliquota-icm + natur-oper.perc-pis[1] + natur-oper.per-fin-soc[1])) / 100)) * (100 - de-aliquota-icm) / 100.

        ASSIGN ttPedItem.vl-pretab = de-vl-preori-imp
               ttPedItem.vl-preori = de-vl-preori-imp.
    END.

END PROCEDURE. /* pi-icms-pis-cofins */


