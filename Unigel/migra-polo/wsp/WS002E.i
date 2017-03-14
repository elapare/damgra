/******************************************************************************
** Programa: WS002E.I
** Descricao: Validacoes do cabeáalho de pedido (ped-venda) apos gravar
** Data:      29/06/2014
** Autor: Mario Martin
*******************************************************************************/

/* Gera dados na if-ped-venda */
ASSIGN c-cod-unid-atend = IF AVAIL b-ws-ped-venda THEN b-ws-ped-venda.cod-estab-atend ELSE "". 
IF c-cod-unid-atend = ? THEN ASSIGN c-cod-unid-atend = "".

/* Criar registro if-ped-venda */
IF c-cod-unid-atend <> "" AND AVAIL ped-venda THEN DO:
    FIND FIRST if-ped-venda EXCLUSIVE-LOCK
         WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-ERROR.
    IF NOT AVAIL if-ped-venda THEN DO:
       CREATE if-ped-venda.
       ASSIGN if-ped-venda.nr-pedido  = ped-venda.nr-pedido
              if-ped-venda.nome-abrev = ped-venda.nome-abrev
              if-ped-venda.nr-pedcli  = ped-venda.nr-pedcli.
    END.
    ASSIGN if-ped-venda.cod-estab-atend = c-cod-unid-atend.
/*    MESSAGE "criou if-ped-venda versao 2.0" SKIP
        if-ped-venda.cod-estab-atend
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
END.

IF AVAIL ped-venda THEN DO:

   FIND FIRST if-ped-venda NO-LOCK
        WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido AND 
              if-ped-venda.nr-pedido-relac <> 0 NO-ERROR.
   IF AVAIL if-ped-venda THEN DO:
      IF ped-venda.cod-estabel <> "422"  AND ttPedVenda.cod-estabel <> "412" THEN DO:   /*solic-318*/ 

          FIND FIRST if-estabelec NO-LOCK
               WHERE if-estabelec.cod-estab-orig  = ttPedVenda.cod-estabel
                 AND if-estabelec.cod-estab-inter = "" NO-ERROR.

          IF AVAIL if-estabelec THEN DO:
             c-cod-unid-atend = if-estabelec.cod-estab-dest.
                
             FIND FIRST if-natur-oper OF if-estabelec
                  WHERE if-natur-oper.nat-oper-pedido = if-ped-venda.nat-oper-orig NO-LOCK NO-ERROR.
                
             if avail if-natur-oper then  ASSIGN ttPedVenda.nat-operacao  = if-natur-oper.nat-oper-v-ung.

          END.
      END.
   END.
END.

IF c-cod-unid-atend  <> "" THEN DO:
    FIND FIRST estabelec NO-LOCK
         WHERE estabelec.cod-estabel = c-cod-unid-atend  NO-ERROR.
    IF NOT AVAIL estabelec THEN DO:
        RUN pi-cria-rowerrors (INPUT 15825,
                               INPUT "Unidade de Atendimento n∆o encontrado. unidade de Atendimento n∆o encontrado.",
                               INPUT "Informar Unidade de Atendimento valida.").
    END.
    ELSE DO:
        /* Cria a tabela de relacionamento */

        IF ped-venda.cod-estabel = c-cod-unid-atend THEN DO:
            RUN pi-cria-rowerrors (INPUT 15825,
                                   INPUT "Unidade de Atendimento n∆o pode ser o mesmo estabelecimento de origem.",
                                   INPUT "Informar Unidade de Atendimento valida.").
        END.

        /* Verifica dados no sistema */
        RUN pi-valid-campos.


        IF NOT CAN-FIND (FIRST RowErrors WHERE RowErrors.ErrorNumber = 15825) THEN DO:
            FIND FIRST ped-venda exclusive-LOCK
                 WHERE ped-venda.nome-abrev = ttPedVenda.nome-abrev
                   AND ped-venda.nr-pedcli  = ttPedVenda.nr-pedcli NO-ERROR.
            IF AVAIL ped-venda THEN DO:
                FIND FIRST if-ped-venda no-LOCK
                     WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-ERROR.
                IF NOT AVAIL if-ped-venda THEN DO:
                    ped-venda.completo = no.
                end.
                ELSE IF if-ped-venda.cod-estab-atend <> c-cod-unid-atend AND ped-venda.cod-sit-ped = 1 AND ped-venda.cod-estabel <> "999" THEN DO:
                    /* Troca Natureza e cliente para unigel comercial polo */
                    RUN pi-troca-unid-polo.
                END.
            END.
        END.
    END.
END.
ELSE DO:
    /* N∆o deixa alterar a unidade de atendimento uma vez o pedido completo */
    RUN pi-valid-campos.
END.

