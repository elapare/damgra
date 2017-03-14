/***************************************************************************
   PROGRAMA: IFBODI317EF.P
   OBJETIVO: EPC para faturamento das demais notas e bloqueio
      AUTOR: Rodrigo Lu°s Frîhlich
       DATA: 08/10/2009
     Vers∆o: 001 - Kraft - Inclus∆o de chamada a upcbodi317ef
       
****************************************************************************/
define buffer if-ped-venda for espmulti.if-ped-venda.

{include/i-prgvrs.i ifbodi317ef.p 2.06.00.001}

/* Include i-epc200.i: Definiá∆o Temp-Table tt-epc */
{include/i-epc200.i1}

DEF TEMP-TABLE tt-notas-geradas NO-UNDO
    FIELD rw-nota-fiscal AS ROWID
    FIELD nr-nota        LIKE nota-fiscal.nr-nota-fis
    FIELD seq-wt-docto   LIKE wt-docto.seq-wt-docto.
DEFINE BUFFER bpes-nota-fiscal FOR nota-fiscal.
DEFINE VARIABLE r-pesagem AS RECID            NO-UNDO.
DEFINE VARIABLE h-bodi317ef   AS HANDLE      NO-UNDO.
DEFINE VARIABLE l-ok          AS LOGICAL     NO-UNDO.
DEFINE VARIABLE l-enc-pesagem AS LOGICAL     NO-UNDO.
DEFINE VARIABLE dPesoTotal   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dValorReduc   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dValorFrete   AS DECIMAL     NO-UNDO.

DEFINE VARIABLE de-taxa-fatur AS DECIMAL     NO-UNDO.
DEFINE VARIABLE l-verif-frete AS LOGICAL     NO-UNDO.
DEFINE VARIABLE de-frete-unid AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-frete-ton  AS DECIMAL     NO-UNDO.

DEFINE VARIABLE c-mensagem AS CHARACTER   NO-UNDO.

DEFINE BUFFER b-ped-venda FOR ped-venda.


DEF INPUT PARAM p-ind-event AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.


IF p-ind-event = "beforeEfetivaNota" THEN DO:

    FIND FIRST tt-epc
         WHERE tt-epc.cod-event = "beforeEfetivaNota" /* "afterEfetivaNota" */
           AND tt-epc.cod-param = "Table-Rowid" NO-ERROR.
    IF AVAIL tt-epc THEN DO:
        find first wt-docto no-lock where rowid(wt-docto) = to-rowid(tt-epc.val-parameter) no-error.
        if avail wt-docto then do:      

            FIND FIRST if-obser-nota
                 WHERE if-obser-nota.cdd-embarq = wt-docto.cdd-embarq
                   AND if-obser-nota.nr-resumo   = wt-docto.nr-resumo  
                   AND if-obser-nota.nome-abrev  = wt-docto.nome-abrev 
                   AND if-obser-nota.nr-pedcli   = wt-docto.nr-pedcli  EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAIL if-obser-nota THEN DO:
                CREATE if-obser-nota.
                ASSIGN if-obser-nota.cdd-embarq = wt-docto.cdd-embarq
                       if-obser-nota.nr-resumo   = wt-docto.nr-resumo  
                       if-obser-nota.nome-abrev  = wt-docto.nome-abrev 
                       if-obser-nota.nr-pedcli   = wt-docto.nr-pedcli.
            END.

            ASSIGN if-obser-nota.cod-cond-pag    = wt-docto.cod-cond-pag
                   if-obser-nota.nat-operacao    = wt-docto.nat-operacao
                   if-obser-nota.no-ab-reppri    = wt-docto.no-ab-reppri
                   if-obser-nota.cod-canal-venda = wt-docto.cod-canal-venda
                   if-obser-nota.cidade-cif      = wt-docto.cidade-cif
                   if-obser-nota.observ-nota     = wt-docto.observ-nota.

            FIND FIRST natur-oper
                 WHERE natur-oper.nat-operacao = wt-docto.nat-operacao NO-LOCK NO-ERROR.
            IF NOT AVAIL natur-oper THEN RETURN "OK".

            /* Leva tambÇm a mensagens da nota */
            FOR EACH wt-msg-docto NO-LOCK
               WHERE wt-msg-docto.seq-wt-docto = wt-docto.seq-wt-docto, 
               FIRST mensagem NO-LOCK
               WHERE mensagem.cod-mensagem = wt-msg-docto.cod-mensagem:

                IF natur-oper.cod-mensagem = wt-msg-docto.cod-mensagem THEN NEXT.

                ASSIGN if-obser-nota.observ-nota = if-obser-nota.observ-nota + mensagem.texto-mensag.

            END.
        END.
    END.
END.


/* Processamento */
IF p-ind-event = "EndEfetivaNota" THEN DO:

    FIND FIRST tt-epc
         WHERE tt-epc.cod-event = "EndEfetivaNota" /* "afterEfetivaNota" */
           AND tt-epc.cod-param = "object-handle" NO-ERROR.
    IF AVAIL tt-epc THEN DO:
    
        FOR EACH tt-notas-geradas:
            DELETE tt-notas-geradas.
        END.
    
        ASSIGN h-bodi317ef = WIDGET-HANDLE(tt-epc.val-parameter).
    
        RUN buscattNotasGeradas IN h-bodi317ef (OUTPUT l-ok,
                                                OUTPUT TABLE tt-notas-geradas).
    
        FOR EACH tt-notas-geradas:
    
            FIND FIRST nota-fiscal 
                 WHERE ROWID(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL nota-fiscal THEN DO:
                IF nota-fiscal.cdd-embarq <> 0 THEN DO:
                    ASSIGN l-enc-pesagem = NO.
        
                    FIND  FIRST mgesp.pesagem NO-LOCK
                        WHERE pesagem.cod-estabel = nota-fiscal.cod-estabel
                          AND pesagem.serie       = nota-fiscal.serie
                          AND pesagem.cdd-embarq = nota-fiscal.cdd-embarq NO-ERROR.

                    IF NOT AVAIL mgesp.pesagem THEN DO:

                        r-pesagem = ?.

                         FOR FIRST if-relac-embarque WHERE if-relac-embarque.cdd-embarq-cli = nota-fiscal.cdd-embarq NO-LOCK,
                            first embarque WHERE embarque.cdd-embarq = if-relac-embarque.cdd-embarq-ung-com NO-LOCK,
                            FIRST bpes-nota-fiscal WHERE bpes-nota-fiscal.cdd-embarq =    embarque.cdd-embarq NO-LOCK,
                            FIRST mgesp.pesagem NO-LOCK
                                            WHERE
                                               pesagem.cdd-embarq = embarque.cdd-embarq AND
                                               pesagem.cod-estabel = embarque.cod-estabel  AND
                                               pesagem.serie       = bpes-nota-fiscal.serie.
                             r-pesagem = Recid(pesagem).
                         END.

                         IF r-pesagem <> ? THEN
                               FIND  FIRST mgesp.pesagem NO-LOCK
                                  WHERE recid(pesagem) = r-pesagem.

                    END.

                    IF AVAIL mgesp.pesagem THEN DO:                    
        
                        ASSIGN l-enc-pesagem = YES.


    
                        ASSIGN nota-fiscal.observ-nota = nota-fiscal.observ-nota + 
                                                         " | Motorista: "       + pesagem.cod-motorista +
                                                         " | Placa Carreta: "   + pesagem.placa-carreta +
                                                         " | Placa Carreta 2: " + pesagem.placa-carreta-2 +
                                                         " | Placa Cavalo: "    + pesagem.placa-veiculo   +
                                                         " | Lacres: "          + pesagem.lacres.
    
                        ASSIGN nota-fiscal.observ-nota = nota-fiscal.observ-nota + pesagem.observacao.

                        FOR FIRST ped-venda where
                                  ped-venda.nome-abrev = nota-fiscal.nome-ab-cli  AND
                                  ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli NO-LOCK.                       
                            ASSIGN nota-fiscal.observ-nota = nota-fiscal.observ-nota + " - " + ped-venda.observacoes.
                        END.
        

                        /*IF pesagem.perc-fat-pur <> 0 THEN
                            ASSIGN nota-fiscal.observ-nota = nota-fiscal.observ-nota + " % Fator de Pureza: " + TRIM(STRING(pesagem.perc-fat-pur)).*/
                    END.





    
                    IF l-enc-pesagem THEN DO:
                        FOR EACH espmulti.wt-msg-docto-pes NO-LOCK
                           WHERE wt-msg-docto-pes.cdd-embarq = nota-fiscal.cdd-embarq
                             AND wt-msg-docto-pes.nr-resumo   = nota-fiscal.nr-resumo, 
                           FIRST mensagem NO-LOCK
                           WHERE mensagem.cod-mensagem = wt-msg-docto-pes.cod-mensagem:
    
                            ASSIGN nota-fiscal.observ-nota = nota-fiscal.observ-nota + mensagem.texto-mensag.
                        END.
                    END.
                END.

                /* Grava o nome da transportadora em branco s¢ pode levar a unigel comercial o nome da transportadora */
                FIND FIRST ped-venda NO-LOCK
                     WHERE ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                       AND ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli NO-ERROR.
                FIND FIRST if-ped-venda NO-LOCK
                     WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-ERROR.
                IF AVAIL if-ped-venda THEN DO:
    
                    IF nota-fiscal.nome-transp <> "" AND nota-fiscal.cod-estabel <> "422" AND nota-fiscal.cod-estabel <> "412" THEN  /*solic-318*/  /* COLOCADO A REGRA DO 434 CONFORME SOLICITADO PELO RICARDO POLIMENO 09-10 */
                        ASSIGN nota-fiscal.nome-transp  = "Destinario".


                    /* Grava observaá∆o na primeira nota */
                    ASSIGN nota-fiscal.observ-nota = "".
                    FOR FIRST natur-oper NO-LOCK
                        WHERE natur-oper.nat-operacao = nota-fiscal.nat-operacao,
                        FIRST mensagem NO-LOCK
                        WHERE mensagem.cod-mensagem = natur-oper.cod-mensagem:
                        
                        ASSIGN nota-fiscal.observ-nota = mensagem.texto-mensag.
                    END.
                        
    
                    /*ASSIGN c-mensagem = "Devido esta operaá∆o de venda ser com incentivo fiscal, foram impressas duas Danfes:" + CHR(10) +
                                        "1¶. Danfe - trata-se de uma venda para Unigel Comercial, esta danfe ser† de uso interno e n∆o dever† sair das dependàncias da empresa." + CHR(10) +
                                        "2¶. Danfe - trata-se de uma Revenda da Unigel Comercial para o Cliente, esta sim dever† acompanhar o transporte atÇ o destino final.".
    
    
                    RUN _insertErrorManual IN h-bodi317ef (input "0",
                                                           INPUT "EMS":U,
                                                           INPUT "WARNING":U,
                                                           INPUT "Operaá∆o Faturamento Unigel Comercial",
                                                           INPUT c-mensagem,
                                                           INPUT "":U).*/


                    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK,
                       FIRST ped-venda NO-LOCK
                       WHERE ped-venda.nome-abrev = it-nota-fisc.nome-ab-cli
                         AND ped-venda.nr-pedcli  = it-nota-fisc.nr-pedcli,
                        EACH ped-item OF ped-venda EXCLUSIVE-LOCK:
        
                        FIND FIRST if-ped-item EXCLUSIVE-LOCK
                             WHERE if-ped-item.nome-abrev   = ped-item.nome-abrev  
                               AND if-ped-item.nr-pedcli    = ped-item.nr-pedcli   
                               AND if-ped-item.nr-sequencia = ped-item.nr-sequencia
                               AND if-ped-item.it-codigo    = ped-item.it-codigo   
                               AND if-ped-item.cod-refer    = ped-item.cod-refer NO-ERROR.
                        IF AVAIL if-ped-item THEN DO:
                            ASSIGN ped-item.vl-preori        = if-ped-item.vl-preori
                                   ped-item.vl-preori-un-fat = if-ped-item.vl-preori-un-fat
                                   ped-item.vl-pretab        = if-ped-item.vl-pretab
                                   ped-item.vl-preuni        = if-ped-item.vl-preuni.
    
                            /*DELETE if-ped-item.*/
                        END.
                    END.
    
    
                END.
            END.
       END. /* FOR EACH tt-notas-geradas... */
    END. /* IF AVAIL tt-epc... */
END.

/*run ftp\upc\upcbodi317ef.p (input p-ind-event,
                            input-output table tt-epc ).*/



