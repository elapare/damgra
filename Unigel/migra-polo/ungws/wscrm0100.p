/***********************************************************************************
**
**       Programa: wsp/wscrm0100.p
**
**       Data....: Maio/2016.
** 
**       Autor...: Rodrigo - DLC Tecnologia.
**
**       Objetivo: Criaá∆o pedido de Venda CRM
**
************************************************************************************/
{ungws/wscrm0100.i}

DEF INPUT  PARAM p-cod-usuar AS CHARACTER NO-UNDO.
DEF INPUT  PARAM TABLE FOR tt-ws-ped-venda.
DEF INPUT  PARAM TABLE FOR tt-ws-ped-item.
DEF OUTPUT PARAM TABLE FOR tt-ws-ped-item-imposto.
DEF OUTPUT PARAM TABLE FOR tt-ws-ped-venda-msg.

DEFINE VARIABLE bo-ped-venda-sdf AS HANDLE      NO-UNDO.
DEFINE VARIABLE i-return         AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-nome-abrev     AS CHARACTER   NO-UNDO.

DEFINE VARIABLE hWebService AS HANDLE NO-UNDO.
DEFINE VARIABLE hCRMIntegrationSoap AS HANDLE NO-UNDO.
DEFINE VARIABLE c-end-webservice AS CHARACTER   NO-UNDO.

ASSIGN p-cod-usuar = "rlfrohl".

RUN Ungws\ALIAS.p.
RUN Ungws\LoginCRM.p (INPUT p-cod-usuar).

/* Verificacao se ser† atualizado banco producao 1 ou teste 2 */
FIND FIRST param-crm NO-LOCK NO-ERROR.
IF INDEX(PROPATH,"TST") > 0 OR INDEX(PROPATH,"DSV") > 0 THEN DO:
    ASSIGN c-end-webservice = param-crm.end-webservice-homol.
END.
ELSE DO:
    ASSIGN c-end-webservice = param-crm.end-webservice-prod.
END. 


/**************************************************************************************
 *************************** Valida Antes da Integraá∆o *******************************
 **************************************************************************************/
FOR EACH tt-ws-ped-venda:

    /* CRM manda um a um */
    FIND FIRST tt-ws-ped-item NO-ERROR.
    IF NOT AVAIL tt-ws-ped-item THEN DO:

        FIND FIRST emitente
             WHERE emitente.cod-emitente = tt-ws-ped-venda.cod-emitente NO-LOCK NO-ERROR.

        CREATE ws-ped-venda-msg-orc.
        ASSIGN ws-ped-venda-msg-orc.nome-abrev       = emitente.nome-abrev
               ws-ped-venda-msg-orc.nr-pedcli        = tt-ws-ped-venda.nr-pedcli
               ws-ped-venda-msg-orc.origem           = "ERP"
               ws-ped-venda-msg-orc.operacao         = "I"  /* 12/12/2014 alterado para gerar com I. */
               ws-ped-venda-msg-orc.processado       = 1
               ws-ped-venda-msg-orc.situacao         = 1
               ws-ped-venda-msg-orc.data-operacao    = NOW
               ws-ped-venda-msg-orc.ErrorSequence    = 1
               ws-ped-venda-msg-orc.ErrorNumber      = 7117
               ws-ped-venda-msg-orc.ErrorDescription = "Pedido n∆o possui itens, imposs°vel completar"
               ws-ped-venda-msg-orc.ErrorHelp        = "Pedido n∆o possui itens, imposs°vel completar".
    
    
        CREATE tt-ws-ped-venda-msg.
        BUFFER-COPY ws-ped-venda-msg-orc TO tt-ws-ped-venda-msg.
    
        CREATE ws_ped_venda_msg_orc.
        ASSIGN ws_ped_venda_msg_orc.nome_abrev       = ws-ped-venda-msg-orc.nome-abrev
               ws_ped_venda_msg_orc.nr_pedcli        = ws-ped-venda-msg-orc.nr-pedcli
               ws_ped_venda_msg_orc.ErrorSequence    = ws-ped-venda-msg-orc.ErrorSequence
               ws_ped_venda_msg_orc.ErrorNumber      = ws-ped-venda-msg-orc.ErrorNumber
               ws_ped_venda_msg_orc.ErrorDescription = ws-ped-venda-msg-orc.ErrorDescription.
    END.
END.

IF CAN-FIND(FIRST ws_ped_venda_msg_orc) THEN DO:
    
    CREATE SERVER hWebService.
    hWebService:CONNECT("-WSDL '" + trim(c-end-webservice) + "'").
    RUN CRMIntegrationSoap SET hCRMIntegrationSoap ON hWebService.

    /* Envia erros cotaá∆o */
    RUN UpsertMensagemRetornoCotacao IN hCRMIntegrationSoap(INPUT DATASET cotacao, OUTPUT UpsertMsgRetornoCotacaoResult).

    RETURN "NOK".
END.

/**************************************************************************************
 ************************* Atualizaá∆o Tabela de Integraá∆o ***************************
 **************************************************************************************/

/* Apaga os registros j† existente porque sen∆o ele se perde */
FOR EACH tt-ws-ped-venda:

    FIND FIRST emitente
         WHERE emitente.cod-emitente = tt-ws-ped-venda.cod-emitente NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN
        ASSIGN c-nome-abrev               = emitente.nome-abrev.

    FIND FIRST ws-ped-venda
         WHERE ws-ped-venda.nome-abrev = c-nome-abrev
           AND ws-ped-venda.nr-pedcli  = tt-ws-ped-venda.nr-pedcli EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ws-ped-venda THEN DO:

        FOR EACH ws-ped-item
           WHERE ws-ped-item.nome-abrev   = ws-ped-venda.nome-abrev  
             AND ws-ped-item.nr-pedcli    = ws-ped-venda.nr-pedcli EXCLUSIVE-LOCK:
            DELETE ws-ped-item.
        END.

        FOR EACH ws-ped-item-imposto
           WHERE ws-ped-item-imposto.nome-abrev   = ws-ped-venda.nome-abrev  
             AND ws-ped-item-imposto.nr-pedcli    = ws-ped-venda.nr-pedcli EXCLUSIVE-LOCK:
            DELETE ws-ped-item-imposto.
        END.

        FOR EACH ws-ped-venda-msg-orc
           WHERE ws-ped-venda-msg-orc.nome-abrev = ws-ped-venda.nome-abrev
             AND ws-ped-venda-msg-orc.nr-pedcli  = ws-ped-venda.nr-pedcli EXCLUSIVE-LOCK:
            DELETE ws-ped-venda-msg-orc.
        END.

        /* Transferido pro wscrm0105
        
        FOR EACH ws-ped-venda-msg
           WHERE ws-ped-venda-msg.nome-abrev = ws-ped-venda.nome-abrev
             AND ws-ped-venda-msg.nr-pedcli  = ws-ped-venda.nr-pedcli EXCLUSIVE-LOCK:
            DELETE ws-ped-venda-msg.
        END.*/

        DELETE ws-ped-venda.
    END.
END.


FOR EACH tt-ws-ped-venda:

    ASSIGN tt-ws-ped-venda.operacao = "A". /* teste */

    IF tt-ws-ped-venda.cidade-cif <> "" THEN
        ASSIGN tt-ws-ped-venda.cidade-cif = ENTRY(3,tt-ws-ped-venda.cidade-cif,"|") NO-ERROR.

    /* Quando FOB limpa cidade cif */
    IF tt-ws-ped-venda.cod-modalid-frete = "1" THEN
        ASSIGN tt-ws-ped-venda.cidade-cif = "".

    FIND FIRST emitente
         WHERE emitente.cod-emitente = tt-ws-ped-venda.cod-emitente NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN
        ASSIGN tt-ws-ped-venda.nome-abrev = emitente.nome-abrev
               c-nome-abrev               = emitente.nome-abrev.


    MESSAGE "verificar as chamadas " c-nome-abrev " - " tt-ws-ped-venda.nr-pedcli SKIP.

    FIND FIRST ws-ped-venda
         WHERE ws-ped-venda.nome-abrev = c-nome-abrev
           AND ws-ped-venda.nr-pedcli  = tt-ws-ped-venda.nr-pedcli EXCLUSIVE-LOCK NO-ERROR.

    IF NOT AVAIL ws-ped-venda THEN DO:
        CREATE ws-ped-venda.
        ASSIGN ws-ped-venda.nome-abrev = c-nome-abrev
               ws-ped-venda.nr-pedcli  = tt-ws-ped-venda.nr-pedcli.
    END.
    BUFFER-COPY tt-ws-ped-venda EXCEPT nome-abrev nr-pedcli TO ws-ped-venda.

    ASSIGN ws-ped-venda.orcamento = 1. /* esse webservice Ç de oráamento */

    IF ws-ped-venda.cod-estabel = "422" THEN
        ASSIGN ws-ped-venda.cod-estab-atend = "434".

    /* Carrega informaá‰es padr‰es */
    run dibo/bodi159sdf.p persistent set bo-ped-venda-sdf.
    run setUserLog in bo-ped-venda-sdf (input p-cod-usuar).
    run setDefaultOrderNumber in bo-ped-venda-sdf ( output i-return ). 
    DELETE PROCEDURE bo-ped-venda-sdf.
    ASSIGN ws-ped-venda.nr-pedido = i-return.

    FIND FIRST repres
         WHERE repres.cod-rep = tt-ws-ped-venda.cod-rep NO-LOCK NO-ERROR.
    IF AVAIL repres THEN
        ASSIGN ws-ped-venda.no-ab-reppri = repres.nome-abrev.

    ASSIGN ws-ped-venda.nr-tab-finan = 1
           ws-ped-venda.nr-ind-finan = 1
           ws-ped-venda.origem       = "CRM"
           ws-ped-venda.processado   = 0
           ws-ped-venda.erros        = "".

    IF ws-ped-venda.observacoes = ? OR ws-ped-venda.observacoes = "" THEN
        ASSIGN ws-ped-venda.observacoes = ".".

    IF AVAIL emitente THEN DO:

        ASSIGN ws-ped-venda.cgc             = emitente.cgc
               ws-ped-venda.nome-abrev      = emitente.nome-abrev
               ws-ped-venda.ins-estadual    = emitente.ins-estadual
               ws-ped-venda.cod-canal-venda = emitente.cod-canal-venda.

        FIND FIRST loc-entr 
             WHERE loc-entr.nome-abrev = emitente.nome-abrev NO-LOCK NO-ERROR.
        IF AVAIL loc-entr THEN
            ASSIGN ws-ped-venda.cod-entrega  = loc-entr.cod-entrega
                   ws-ped-venda.local-entreg = loc-entr.endereco.


        IF ws-ped-venda.cod-portador = ? THEN
            ASSIGN ws-ped-venda.cod-portador = emitente.portador.

        IF ws-ped-venda.modalidade = ? THEN
            ASSIGN ws-ped-venda.modalidade = emitente.modalidade.
    END.
        
    FOR EACH tt-ws-ped-item:
        ASSIGN tt-ws-ped-item.operacao = "A". /* teste */
    
        FIND FIRST ws-ped-item
             WHERE ws-ped-item.nome-abrev   = ws-ped-venda.nome-abrev  
               AND ws-ped-item.nr-pedcli    = ws-ped-venda.nr-pedcli   
               AND ws-ped-item.nr-sequencia = tt-ws-ped-item.nr-sequencia
               AND ws-ped-item.it-codigo    = tt-ws-ped-item.it-codigo EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL ws-ped-item THEN DO:
            CREATE ws-ped-item.
            ASSIGN ws-ped-item.nome-abrev   = ws-ped-venda.nome-abrev  
                   ws-ped-item.nr-pedcli    = ws-ped-venda.nr-pedcli   
                   ws-ped-item.nr-sequencia = tt-ws-ped-item.nr-sequencia
                   ws-ped-item.it-codigo    = tt-ws-ped-item.it-codigo.
        END.
        BUFFER-COPY tt-ws-ped-item EXCEPT nr-sequencia it-codigo TO ws-ped-item.

        ASSIGN ws-ped-item.origem       = "CRM"
               ws-ped-item.processado   = 0
               ws-ped-item.nat-operacao = ws-ped-venda.nat-operacao
               ws-ped-item.vl-preori    = IF ws-ped-item.vl-preuni = ? THEN ws-ped-item.vl-pretab ELSE ws-ped-item.vl-preuni
               ws-ped-item.vl-preuni    = IF ws-ped-item.vl-preuni = ? THEN ws-ped-item.vl-pretab ELSE ws-ped-item.vl-preuni.

        IF ws-ped-item.observacao = ? OR ws-ped-item.observacao = "" THEN
            ASSIGN ws-ped-item.observacao = ".".

        FIND FIRST ITEM
             WHERE ITEM.it-codigo = tt-ws-ped-item.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN
            ASSIGN ws-ped-item.un            = ITEM.un
                   ws-ped-venda.cod-unid-neg = ITEM.cod-unid-neg.

        /* Carrega Item configurado */
        IF AVAIL ITEM AND ITEM.politica = 5 THEN DO:
            FIND FIRST ws-ped-item-esp
                 WHERE ws-ped-item-esp.nome-abrev   = ws-ped-venda.nome-abrev  
                   AND ws-ped-item-esp.nr-pedcli    = ws-ped-venda.nr-pedcli   
                   AND ws-ped-item-esp.nr-sequencia = ws-ped-item.nr-sequencia
                   AND ws-ped-item-esp.it-codigo    = ws-ped-item.it-codigo EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAIL ws-ped-item-esp THEN DO:
                CREATE ws-ped-item-esp.
                ASSIGN ws-ped-item-esp.nome-abrev   = ws-ped-venda.nome-abrev  
                       ws-ped-item-esp.nr-pedcli    = ws-ped-venda.nr-pedcli   
                       ws-ped-item-esp.nr-sequencia = ws-ped-item.nr-sequencia
                       ws-ped-item-esp.it-codigo    = ws-ped-item.it-codigo.
            END.
            BUFFER-COPY tt-ws-ped-item EXCEPT nr-sequencia it-codigo TO ws-ped-item-esp.
    
            ASSIGN ws-ped-item-esp.qtde-pedida-cliente = tt-ws-ped-item.qt-pedida
                   ws-ped-item-esp.integrado-ems       = NO
                   ws-ped-item-esp.cod-prod-cliente    = tt-ws-ped-item.it-codigo
                   ws-ped-item-esp.unid-fat-cliente    = ws-ped-item.un
                   ws-ped-item-esp.preco-unit          = IF tt-ws-ped-item.vl-preuni = ? THEN tt-ws-ped-item.vl-pretab ELSE tt-ws-ped-item.vl-preuni
                   ws-ped-item-esp.origem              = "CRM"
                   ws-ped-item-esp.processado          = 0.
        END.

        ASSIGN ws-ped-venda.dt-entrega = ws-ped-item.dt-entrega
               ws-ped-venda.dt-entorig = ws-ped-item.dt-entorig.
    END.
END.

/**************************************************************************************
 ************************* Atualizaá∆o Tabela do EMS2 *********************************
 **************************************************************************************/

/* Criar os pedidos de Venda incluidos pelo sistema CRM */
FOR EACH tt-ws-ped-venda,
    EACH ws-ped-venda 
    WHERE ws-ped-venda.nome-abrev = tt-ws-ped-venda.nome-abrev
      AND ws-ped-venda.nr-pedcli  = tt-ws-ped-venda.nr-pedcli 
      AND ws-ped-venda.origem     = "CRM"
      AND ws-ped-venda.operacao   <> "D" /* Exclusao nao entra neste caso */
      AND ws-ped-venda.processado = 0 NO-LOCK 
    BREAK BY ws-ped-venda.nome-abrev
          BY ws-ped-venda.nr-pedcli:

    /* Apaga erros */
    FOR EACH ws-ped-venda-msg-orc
       WHERE ws-ped-venda-msg-orc.nome-abrev = ws-ped-venda.nome-abrev
         AND ws-ped-venda-msg-orc.nr-pedcli  = ws-ped-venda.nr-pedcli EXCLUSIVE-LOCK:
        DELETE ws-ped-venda-msg-orc.
    END.

    ASSIGN c-nome-abrev = ws-ped-venda.nome-abrev.

    /* Gera Pedido */
    RUN ungws\wscrm0100a.p (INPUT p-cod-usuar,
                            INPUT ws-ped-venda.nome-abrev,
                            INPUT ws-ped-venda.nr-pedcli).

    /**************************************************************************************
     ***************************** Retornar Informaá‰es Pedido ****************************
     **************************************************************************************/
    FOR EACH ws-ped-item-imposto NO-LOCK
       WHERE ws-ped-item-imposto.nome-abrev = c-nome-abrev
         AND ws-ped-item-imposto.nr-pedcli  = ws-ped-venda.nr-pedcli:
        CREATE tt-ws-ped-item-imposto.
        BUFFER-COPY ws-ped-item-imposto TO tt-ws-ped-item-imposto.
    END.

    /**************************************************************************************
     ****************************** Retorno Mensagens de Erro *****************************
     **************************************************************************************/
    FOR EACH ws-ped-venda-msg-orc
       WHERE ws-ped-venda-msg-orc.nome-abrev = ws-ped-venda.nome-abrev
         AND ws-ped-venda-msg-orc.nr-pedcli  = ws-ped-venda.nr-pedcli
         AND ws-ped-venda-msg-orc.processado = 0 EXCLUSIVE-LOCK:

        FIND FIRST cadast_msg
             WHERE cadast_msg.cdn_msg = ws-ped-venda-msg-orc.ErrorNumber NO-LOCK NO-ERROR.

        ASSIGN ws-ped-venda-msg-orc.situacao   = IF AVAIL cadast_msg AND cadast_msg.idi_tip_msg <> 1 THEN 2 ELSE 1.
               ws-ped-venda-msg-orc.processado = 1. /* Atualiza para n∆o aparecer no proximo */

        CREATE tt-ws-ped-venda-msg.
        BUFFER-COPY ws-ped-venda-msg-orc TO tt-ws-ped-venda-msg.

        CREATE ws_ped_venda_msg_orc.
        ASSIGN ws_ped_venda_msg_orc.nome_abrev       = ws-ped-venda-msg-orc.nome-abrev
               ws_ped_venda_msg_orc.nr_pedcli        = ws-ped-venda-msg-orc.nr-pedcli
               ws_ped_venda_msg_orc.ErrorSequence    = ws-ped-venda-msg-orc.ErrorSequence
               ws_ped_venda_msg_orc.ErrorNumber      = ws-ped-venda-msg-orc.ErrorNumber
               ws_ped_venda_msg_orc.ErrorDescription = ws-ped-venda-msg-orc.ErrorDescription.
    END.

    IF CAN-FIND(FIRST ws_ped_venda_msg_orc) THEN DO:

        CREATE SERVER hWebService.
        hWebService:CONNECT("-WSDL '" + trim(c-end-webservice) + "'").
        RUN CRMIntegrationSoap SET hCRMIntegrationSoap ON hWebService.

        /* Envia erros cotaá∆o */
        RUN UpsertMensagemRetornoCotacao IN hCRMIntegrationSoap(INPUT DATASET cotacao, OUTPUT UpsertMsgRetornoCotacaoResult).
    END.
END.

RETURN "OK".
