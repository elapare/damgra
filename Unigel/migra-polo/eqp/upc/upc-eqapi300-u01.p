/******************************************************************************
**   Programa: EQAPIU300-U01
**   Objetivo: Programa UPC - UPC do GT0703 - Consulta Nota Fiscal
**       Data: Junho de 2011 / Rodrigo Luis - Vertice
**  Descricao: 2.04.00.000 - Cria campo para visualizaá∆o do romaneio
*******************************************************************************/

&SCOPED-DEFINE base mguni
DEF BUFFER empresa                FOR {&base}.empresa.

{include/i-prgvrs.i UPC-EQAPI300-U01 2.06.00.000}

{include/i-epc200.i1}

/*WPA - Buffers*/
define buffer if-ped-venda  for espmulti.if-ped-venda.
define buffer if-natur-oper for espmulti.if-natur-oper.
define buffer if-estabelec  for espmulti.if-estabelec.

/* Definicao de Parametros */
DEF INPUT PARAM  p-ind-event AS CHAR NO-UNDO. 
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

/* Definicao Variaveis */
DEFINE VARIABLE i-nr-pedido AS INTEGER     NO-UNDO.
DEFINE VARIABLE l-libera AS LOGICAL     NO-UNDO.

l-libera = NO.

DEF BUFFER b-ped-venda FOR ped-venda.

IF p-ind-event = "antes-validar-nota" THEN DO:

    FIND FIRST tt-epc 
         WHERE tt-epc.cod-event     = p-ind-event 
           AND tt-epc.cod-parameter = "pedido-rowid" NO-ERROR.
    IF AVAIL tt-epc THEN DO:
        FIND FIRST ped-venda
             WHERE ROWID(ped-venda) = TO-ROWID(tt-epc.val-parameter) NO-LOCK NO-ERROR.
        IF AVAIL ped-venda THEN DO:

            FIND FIRST natur-oper
                 WHERE natur-oper.nat-operacao = ped-venda.nat-operacao NO-LOCK NO-ERROR.

            /* libera para embarcar polo sem unigel comercial qualquer um usuario cadastrado no escd0020 eq0506unc*/

            IF substring(ped-venda.nome-abrev,1,3) = "unc" AND  CAN-FIND(FIRST ext_usuar_grp_usuar WHERE ext_usuar_grp_usuar.cod_grp_usuar = "eq0506unc") THEN DO:
                l-libera = YES.
            END.

            /* N∆o embarca o pedido sen∆o tiver na unigel comercial - POLO */
            IF ped-venda.cod-estabel = "422" AND natur-oper.emite-duplic and ped-venda.nat-operacao < "7"  AND NOT l-libera THEN DO:

                FIND FIRST if-ped-venda
                     WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

                IF NOT AVAIL if-ped-venda OR if-ped-venda.nr-pedido-relac = 0 THEN DO:
                    RUN utp/ut-msgs.p (INPUT "Show",
                                       INPUT  17006,
                                       INPUT "Pedido n∆o est† com unidade de atendimento informada no PD4000." ).
            
                    RETURN "NOK".

                END.
            END.
            
            /* Procura pedido final */
            ASSIGN i-nr-pedido = 0.
            FIND FIRST if-ped-venda NO-LOCK
                 WHERE if-ped-venda.nr-pedido         = ped-venda.nr-pedido
                   AND if-ped-venda.nr-pedido-relac  <> 0 NO-ERROR.
            IF AVAIL if-ped-venda THEN DO:
        
                DO WHILE AVAIL if-ped-venda:
                    FIND FIRST b-ped-venda EXCLUSIVE-LOCK
                         WHERE b-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac NO-ERROR.
        
                    FIND FIRST if-ped-venda NO-LOCK
                         WHERE if-ped-venda.nr-pedido = b-ped-venda.nr-pedido NO-ERROR.
                    
                    ASSIGN i-nr-pedido = b-ped-venda.nr-pedido.
                END.
            END.
        
            IF i-nr-pedido > 0 THEN DO:
                FIND FIRST b-ped-venda
                     WHERE b-ped-venda.nr-pedido = i-nr-pedido NO-LOCK NO-ERROR.
                IF AVAIL b-ped-venda AND b-ped-venda.cod-sit-aval <> 3 AND b-ped-venda.cod-sit-aval <> 2 AND b-ped-venda.cod-estabel <> "432" THEN DO:

                    RUN utp/ut-msgs.p (INPUT "Show",
                                       INPUT  17006,
                                       INPUT "Pedido Cliente Final n∆o est† aprovado.~~Pedido/Cliente:" + TRIM(b-ped-venda.nr-pedcli) + "/" + TRIM(ped-venda.nome-abrev) +
                                             " est† com status: " + {diinc/i03di159.i 04 b-ped-venda.cod-sit-aval} ).
            
                    RETURN "NOK".

                END.
            END.
        END.
    END.
END.
