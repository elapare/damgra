/******************************************************************************
** Programa: WS002F.I
** Descricao: Validacoes do cabe‡alho de pedido (ped-venda) apos completeorder
** Data:      10/07/2014
** Autor: Mario Martin
*******************************************************************************/

/* Gera dados na if-ped-venda */
IF c-cod-unid-atend  <> "" THEN DO:
    FIND FIRST ped-venda NO-LOCK
         WHERE ped-venda.nome-abrev = ttPedVenda.nome-abrev
           AND ped-venda.nr-pedcli  = ttPedVenda.nr-pedcli
           AND ped-venda.nr-pedido  = ttPedVenda.nr-pedido NO-ERROR.
    IF AVAIL ped-venda THEN DO:
        FIND FIRST if-ped-venda no-LOCK
             WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido NO-ERROR.
        IF AVAIL if-ped-venda THEN DO: 
            FIND first b-ped-venda 
                 where b-ped-venda.nr-pedido =  if-ped-venda.nr-pedido no-lock no-error.
            if avail if-ped-venda and avail b-ped-venda and (b-ped-venda.cod-estabel = "422" OR b-ped-venda.cod-estabel = "412") then run pi-replica-data-tipo-filho.  /*solic-318*/
        END.
        else do:
            FIND FIRST if-ped-venda no-LOCK
                 WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido and if-ped-venda.nr-pedido-relac <> 0 NO-ERROR.
            IF  AVAIL if-ped-venda THEN DO:
                if avail if-ped-venda and (ped-venda.cod-estabel = "422" OR ped-venda.cod-estabel = "412") then run pi-replica-data-tipo.  /*solic-318*/
            END.
        END.
    END.
END.
