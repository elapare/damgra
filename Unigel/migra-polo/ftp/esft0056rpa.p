/*******************************************************************************
* Empresa    : DLC TEC
* Cliente    : Unigel
* Programa   : ftp\esft0056rpa.p
* Descricao  : L¢gica do programa Programas Auxiliares Faturamento -
               Recebe/Embarca T12
* Autor      : JosÇ Carlos de Almeida Marques
* Data       : 03/08/2015
* Versao     : 12.4
* Atualizacao: 
******************************************************************************/

define input  param p-c-cod-estabel like nota-fiscal.cod-estabel no-undo.
define input  param p-c-serie       like nota-fiscal.serie       no-undo.
define input  param p-c-nr-nota-fis like nota-fiscal.nr-nota-fis no-undo.
DEFINE OUTPUT PARAM p-cdd-embarq    LIKE embarque.cdd-embarq     NO-UNDO.

{bf/buffersUni2.i}

{include/i-prgvrs.i IFBODI135 2.06.00.000}

/*WPA - Buffers*/
define buffer if-ped-venda   for espmulti.if-ped-venda.
define buffer if-natur-oper  for espmulti.if-natur-oper.
define buffer if-estabelec   for espmulti.if-estabelec.
define buffer if-relac-nota  for espmulti.if-relac-nota.
define buffer tab-generica   FOR mgemp.tab-generica.

/* Include i-epc200.i: Definiá∆o Temp-Table tt-epc */
{include/i-epc200.i1}
{cdp/cd0666.i}
{cdp/cd0667.i -aux}   /* Temp-table de erros (auxiliar)      */
{utp/ut-glob.i}
{method/dbotterr.i} /* Row Erros */
{btb/btb008za.i0}
{upc/ifbodi135.i} /* Definiá∆o Temp-tables ---*/
{btb/btb009za.i}
{utp/utapi019.i} /*Envio de e-mail*/  

DEFINE TEMP-TABLE tt-tab-generica NO-UNDO LIKE mgemp.tab-generica
   field r-rowid as rowid.

DEFINE TEMP-TABLE tt-despreza NO-UNDO LIKE tt-aloc-man.

DEFINE VARIABLE c-ultimo-metodo-exec      AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-msg-erro                AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-msg-ajuda               AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-char-aux                AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-serie                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-resultado               AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-mensagem                AS CHARACTER NO-UNDO.
DEFINE VARIABLE v_cod_refer_api           AS CHARACTER NO-UNDO.
DEFINE VARIABLE v_cod_modulo              AS CHARACTER NO-UNDO.
DEFINE VARIABLE dt-emis-nota              AS DATE      NO-UNDO.
DEFINE VARIABLE dt-bas-dupl               AS DATE      NO-UNDO.
DEFINE VARIABLE c-date-aux                AS DATE      NO-UNDO.
DEFINE VARIABLE de-qt-a-alocar            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE h-bodi317ef               AS HANDLE    NO-UNDO.
DEFINE VARIABLE h-bodi317pr               AS HANDLE    NO-UNDO.
DEFINE VARIABLE h-bodi317sd               AS HANDLE    NO-UNDO.
DEFINE VARIABLE h-bodi317im1bra           AS HANDLE    NO-UNDO.
DEFINE VARIABLE h-bodi317va               AS HANDLE    NO-UNDO.
DEFINE VARIABLE h-bodi317in               AS HANDLE    NO-UNDO.
DEFINE VARIABLE h-bodi317                 AS HANDLE    NO-UNDO.
DEFINE VARIABLE h-bodi162                 AS HANDLE    NO-UNDO.
DEFINE VARIABLE h-ft4026                  AS HANDLE    NO-UNDO.
DEFINE VARIABLE h-bo-emitente             AS HANDLE    NO-UNDO.
DEFINE VARIABLE h-bodi176                 AS HANDLE    NO-UNDO.
DEFINE VARIABLE h-bocx312c                AS HANDLE    NO-UNDO.
DEFINE VARIABLE h-bosc014                 AS HANDLE    NO-UNDO.
DEFINE VARIABLE h-bodi041                 AS HANDLE    NO-UNDO.
DEFINE VARIABLE h-ft4015                  AS HANDLE    NO-UNDO.
DEFINE VARIABLE h-eqapi300                AS HANDLE    NO-UNDO.
DEFINE VARIABLE i-sequencia               AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-seq-aloc-man            AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-embarque                AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-nr-pedido               AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-linhas                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-seq-wt-docto            AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-natureza                AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-registros               AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-cod-erro                AS INTEGER   NO-UNDO.
DEFINE VARIABLE l-proc-ok-aux             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-ok                      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-erro-atend              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-transacao               AS LOGICAL   INIT NO  NO-UNDO.
DEFINE VARIABLE l-exportacao              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-cidade-cif              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-log-mostra-simul-embarq AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE VARIABLE l-eq-log-integr-wms       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-eq-log-aloca-ord-produc AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-log-executa-simul       AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE VARIABLE l-possui-ordens           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-possui-erro             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v_log_refer_uni           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v_log_tit_acr_unico       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v_log_atualiz_ok          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE c-gera-titulo             AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-ems5                    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-conecta                 AS LOGICAL   NO-UNDO.
DEFINE VARIABLE h-btb009za                AS HANDLE    NO-UNDO.
DEFINE VARIABLE l-conec-bas               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-conec-fin               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-conec-uni               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE i-empresa                 LIKE param-global.empresa-prin NO-UNDO.
DEFINE VARIABLE l-erro                    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-ja-conec-emsuni         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-ja-conec-emsbas         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-ja-conec-emsfin         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE c-cod-estab-atend         AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-lst-nota                AS CHARACTER NO-UNDO.
DEFINE VARIABLE i-cont                    AS INTEGER   NO-UNDO.
DEFINE VARIABLE c-cod-estab               AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nr-nota-fis             AS CHARACTER NO-UNDO.
DEFINE VARIABLE i-pos-ini                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-pos-fim                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE c-inf-nota                AS CHARACTER NO-UNDO.
DEFINE VARIABLE rw-nota-fiscal            AS ROWID     NO-UNDO.
DEFINE VARIABLE c-nota-orig               AS CHARACTER NO-UNDO.
DEFINE VARIABLE rw-nota-orig              AS ROWID     NO-UNDO.
DEFINE VARIABLE i-nr-emb-orig             AS INTEGER   NO-UNDO.
DEFINE VARIABLE h-acomp                   AS HANDLE    NO-UNDO.
DEFINE VARIABLE l-lib-sefaz               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE r-docum-est               AS ROWID     NO-UNDO.
DEFINE VARIABLE raw-param                 AS RAW       NO-UNDO.
DEFINE VARIABLE l-imp-danfe               AS LOGICAL   NO-UNDO.

def buffer b-estabelec for estabelec.
DEF BUFFER bt-ped-venda        FOR ped-venda. /*tranferencia*/
DEF BUFFER bt2-ped-venda        FOR ped-venda. /*tranferencia*/

DEF BUFFER bt-ped-item         FOR ped-item. /*tranferencia*/
DEF BUFFER bt2-ped-item         FOR ped-item. /*tranferencia*/

DEF BUFFER bt-if-ped-venda     FOR espmulti.if-ped-venda. /*tranferencia*/

DEF BUFFER b-ped-venda        FOR ped-venda.
DEF BUFFER b2-ped-venda       FOR ped-venda.
DEF BUFFER b3-ped-venda       FOR ped-venda.
DEF BUFFER b4-ped-venda       FOR ped-venda.
DEF BUFFER b5-ped-venda       FOR ped-venda.
DEF BUFFER bf-ped-venda       FOR ped-venda.
DEF BUFFER bf-ped-item        FOR ped-item.    
DEF BUFFER b2-ped-item        FOR ped-item.    
DEF BUFFER b3-ped-item        FOR ped-item.    
DEF BUFFER b-tt-res-cli       FOR tt-res-cli.
DEF BUFFER b-tt-notas-geradas FOR tt-notas-geradas.
DEF BUFFER b-it-nota-fisc     FOR it-nota-fisc.
DEF BUFFER b2-it-nota-fisc    FOR it-nota-fisc.
DEF BUFFER b3-it-nota-fisc    FOR it-nota-fisc.
DEF BUFFER b-nota-fiscal      FOR nota-fiscal.
DEF BUFFER b2-nota-fiscal     FOR nota-fiscal.
DEF BUFFER b3-nota-fiscal     FOR nota-fiscal.
DEF BUFFER b4-nota-fiscal     FOR nota-fiscal.
DEF BUFFER b5-nota-fiscal     FOR nota-fiscal.
DEF BUFFER b6-nota-fiscal     FOR nota-fiscal.
DEF BUFFER b7-nota-fiscal     FOR nota-fiscal.
DEF BUFFER b-tt-notas-ger-aux FOR tt-notas-ger-aux.
DEF BUFFER b-natur-oper       FOR natur-oper.
DEF BUFFER b2-natur-oper      FOR natur-oper.
DEF BUFFER b-sit-nf-eletro    FOR sit-nf-eletro.
DEF BUFFER b-embarque         FOR embarque.
DEF BUFFER b-if-relac-nota    FOR espmulti.if-relac-nota.
DEF BUFFER b2-if-relac-nota   FOR espmulti.if-relac-nota.
DEF BUFFER b-ext-embarque     FOR ext-embarque.    
    

DEF NEW GLOBAL SHARED VAR l-permit-cancel-if AS LOG NO-UNDO.

/*for each tt-epc no-lock:
    message 'p-ind-event.........: ' p-ind-event          skip
            'tt-epc.cod-event....: ' tt-epc.cod-event     skip
            'tt-epc.cod-parameter: ' tt-epc.cod-parameter skip view-as alert-box.
end.*/

/* Processamento */
DO TRANS:

    FOR EACH  nota-fiscal NO-LOCK
        WHERE nota-fiscal.cod-estabel = p-c-cod-estabel
        AND   nota-fiscal.serie       = p-c-serie      
        AND   nota-fiscal.nr-nota-fis = p-c-nr-nota-fis:

        ASSIGN rw-nota-fiscal = ROWID(nota-fiscal)
               c-lst-nota     = "".

        FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:

            FIND FIRST ped-venda NO-LOCK
                 WHERE ped-venda.nome-abrev = it-nota-fisc.nome-ab-cli
                   AND ped-venda.nr-pedcli  = it-nota-fisc.nr-pedcli NO-ERROR.
            IF AVAIL ped-venda AND ped-venda.cond-redespa BEGINS "Pedido(s) relacionado(s)" THEN DO:

                /* Para n∆o executar duas vezes */
                /*IF CAN-FIND(FIRST espmulti.if-relac-nota
                            WHERE if-relac-nota.cod-estabel  = nota-fiscal.cod-estabel
                              AND if-relac-nota.nr-nota-fis  = nota-fiscal.nr-nota-fis
                              AND if-relac-nota.serie        = nota-fiscal.serie) THEN NEXT.*/

                ASSIGN l-imp-danfe = NO.
                
                RUN pi-sit-imp.

                IF l-imp-danfe = NO THEN DO:
                    LEAVE.
                END.
                ELSE DO:
                    ASSIGN rw-nota-orig  = ROWID(nota-fiscal)
                           i-nr-emb-orig = nota-fiscal.cdd-embarq
                           c-nota-orig   = TRIM(nota-fiscal.cod-estabel) + "-" + TRIM(nota-fiscal.serie) + "_" + TRIM(nota-fiscal.nr-nota-fis).
    
                    FIND FIRST if-ped-venda NO-LOCK
                         WHERE if-ped-venda.nr-pedido         = ped-venda.nr-pedido
                           AND if-ped-venda.nr-pedido-relac  <> 0 NO-ERROR.
                    IF AVAIL if-ped-venda THEN DO:
    
                        FIND FIRST b2-ped-venda EXCLUSIVE-LOCK
                             WHERE b2-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac NO-ERROR.

                        RUN pi-fatura-ped.
                                                     /* Cliente Remessa Triangular */
                        IF b2-ped-venda.nome-abrev-tri <> "" THEN
                            RUN pi-carreg-if-triang.
                    END.

                    IF l-possui-erro THEN DO:

                        RUN pi-cancel-nota-orig (INPUT nota-fiscal.cod-estabel,
                                                 INPUT nota-fiscal.serie,
                                                 INPUT nota-fiscal.nr-nota-fis).
                        LEAVE.
                    END.
                END.
                
            END.
        END. /* FOR EACH it-nota-fisc OF nota-fiscal ... */

        RUN pi-fatura-triang.
    END.

    /*PAUSE.

    UNDO,LEAVE.*/

END.

RETURN "OK".

PROCEDURE pi-fatura-ped :
/*------------------------------------------------------------------------------
  Purpose:     Gera Embarque
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE l-gera-transf AS LOGICAL  INITIAL NO  NO-UNDO. /*transferencia sem pedido Polo*/

    /* Verifica se existe problema de alocaá∆o */
    RUN pi-sppd309.

    FIND FIRST b-ped-venda EXCLUSIVE-LOCK
         WHERE b-ped-venda.nr-pedido = b2-ped-venda.nr-pedido NO-ERROR.
    IF AVAIL b-ped-venda THEN DO:

        IF b-ped-venda.cod-sit-pre >= 3 THEN NEXT /*totalmente alocado */.
        
        /*IF CAN-FIND(FIRST b-it-nota-fisc
                    WHERE b-it-nota-fisc.nome-ab-cli = b-ped-venda.nome-abrev
                      AND b-it-nota-fisc.nr-pedcli   = b-ped-venda.nr-pedcli) THEN NEXT.*/

        /* Faturamento das Notas - Clientes */
        /*RUN pi-recebimento.
        IF l-possui-erro THEN RETURN "NOK".*/


        FIND FIRST if-natur-oper NO-LOCK
        WHERE if-natur-oper.cod-estab-orig  = ped-venda.cod-estabel
          AND if-natur-oper.cod-estab-inter = ""
          AND if-natur-oper.cod-estab-dest  = b2-ped-venda.cod-estabel
          AND if-natur-oper.nat-oper-pedido = if-ped-venda.nat-oper-orig 
          AND if-natur-oper.nat-oper-transf <> "" no-error.
            
        IF AVAIL if-natur-oper and (nota-fiscal.cod-estabel = "422" OR nota-fiscal.cod-estabel = "412") THEN DO: /*solic-318*/ 
            RUN pi-gera-transferencia-sem-pedido.   /* notas de tranferencia sem pedido POLO*/
        end.    
        ELSE DO:
            
            RUN pi-remessa-armazenagem.
            IF l-possui-erro THEN RETURN "NOK".
            RUN pi-gera-embarque.
            IF l-possui-erro OR RETURN-VALUE = "NOK" THEN RETURN "NOK".
            /*RUN pi-calc-embarque.
            IF l-possui-erro THEN RETURN "NOK".*/
        END.
        IF c-lst-nota <> "" THEN DO:
            FIND FIRST b-nota-fiscal EXCLUSIVE-LOCK
                 WHERE ROWID(b-nota-fiscal) = rw-nota-orig NO-ERROR.
            IF AVAIL b-nota-fiscal THEN DO:
                ASSIGN b-nota-fiscal.cond-redespa = "Nota(s) relacionada(s) " + TRIM(c-lst-nota)
                       b-nota-fiscal.ind-sit-nota = 2.
    
                FOR EACH b2-it-nota-fisc OF b-nota-fiscal:
                    ASSIGN b2-it-nota-fisc.ind-sit-nota = 2.
                END.
            END.
        END.
    END.

    RETURN "OK".

END PROCEDURE.

PROCEDURE pi-gera-transferencia-sem-pedido:
/*------------------------------------------------------------------------------
  Purpose:     Emite nota de transferencia
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE i-seq-wt-it-docto AS INTEGER     NO-UNDO.
    DEFINE VARIABLE c-estab-saida-transf LIKE estabelec.cod-estabel NO-UNDO.
    DEFINE VARIABLE d-perc-atend AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE i-ultimo-resumo AS INTEGER     NO-UNDO.
    DEFINE VARIABLE tt-qt-volumes AS INTEGER     NO-UNDO.
    DEFINE VARIABLE tt-qt-lotes AS INTEGER     NO-UNDO.
    DEFINE BUFFER b4-it-nota-fisc FOR it-nota-fisc.

    DEFINE VARIABLE l-dif AS LOGICAL     NO-UNDO.

    IF AVAIL if-natur-oper  THEN DO:

        FIND FIRST estabelec NO-LOCK
              WHERE estabelec.cod-emitente = nota-fiscal.cod-emitente NO-ERROR.

        IF NOT AVAIL estabelec  THEN  RETURN "NOK".
        
        c-estab-saida-transf = estabelec.cod-estabel.

   
        FIND FIRST estabelec WHERE estabelec.cod-estabel = if-natur-oper.cod-estab-dest NO-LOCK NO-ERROR.

        IF NOT AVAIL estabelec  THEN  RETURN "NOK".


        FIND FIRST emitente
             WHERE emitente.cod-emitente = estabelec.cod-emitente NO-LOCK NO-ERROR.

        RUN dibo/bodi317in.p PERSISTENT SET h-bodi317in.

        RUN inicializabos IN h-bodi317in(OUTPUT h-bodi317pr,
                                         OUTPUT h-bodi317sd,
                                         OUTPUT h-bodi317im1bra,
                                         OUTPUT h-bodi317va).
    
     /*   RUN inicializaAcompanhamento IN  h-bodi317sd.
       */
        RUN emptyRowErrors   IN h-bodi317in.

        RUN pi-atualiz-ser-estab (INPUT c-estab-saida-transf).
    

       
        FIND FIRST wt-docto where            
            wt-docto.cod-estabel  = c-estab-saida-transf  AND
            wt-docto.dt-emis-nota = TODAY                 AND
            wt-docto.nat-operacao = if-natur-oper.nat-oper-transf AND
            wt-docto.nome-abrev   = emitente.nome-abrev       AND
            wt-docto.serie        =  nota-fiscal.serie AND
             INDEX (wt-docto.observ-nota,STRING(nota-fiscal.cdd-embarq)) > 0 EXCLUSIVE NO-ERROR.

        IF AVAIL wt-docto THEN
           ASSIGN  i-seq-wt-docto = wt-docto.seq-wt-docto .
        ELSE DO:
    
            RUN criaWtDocto IN h-bodi317sd (INPUT c-seg-usuario,                     /* usu†rio                                */
                                            INPUT c-estab-saida-transf,              /* estabelecimento                        */
                                            INPUT nota-fiscal.serie,                 /* serie                                  */
                                            INPUT "",                                /* numero de nota caso seja notas manuais */
                                            INPUT emitente.nome-abrev,               /* nome-abrev emitente                    */
                                            INPUT ?,                                 /* pedido                                 */
                                            INPUT 4,                                 /* tipo de nota                           */
                                            INPUT 4003,                              /* codigo do programa que  gerou a nota   */
                                            INPUT TODAY,                             /* data emissao nota                      */
                                            INPUT 0,                                 /* embarque                               */
                                            INPUT if-natur-oper.nat-oper-transf,     /* natureza operacao                      */
                                            INPUT 0,                                 /* canal venda                            */
                                           OUTPUT i-seq-wt-docto,                    /* seq docto                              */
                                           OUTPUT l-proc-ok-aux).                    /* ok Sim/Nao?                            */
        
            /* busca poss°veis erros que ocorreram nas validaá‰es */
            RUN devolveErrosBodi317sd IN h-bodi317sd (OUTPUT c-ultimo-metodo-exec,
                                                      OUTPUT table RowErrors).
        
            /* pesquisa algum erro ou advertància que tenha ocorrido */
            IF CAN-FIND(FIRST RowErrors 
                        WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
        
               RUN pi-error.
               RUN pi-cancela.
               RETURN "NOK".
            END.
        
            
            /* disponibilizar o registro wt-docto na bodi317sd */
            FIND FIRST wt-docto where wt-docto.seq-wt-docto = i-seq-wt-docto EXCLUSIVE NO-ERROR.

            ASSIGN wt-docto.observ-nota =  "(" + "Embarque: " + STRING(nota-fiscal.cdd-embarq) + ") " + wt-docto.observ-nota .


        END.
        /* disponibilizar o registro wt-docto na bodi317sd */
        ASSIGN wt-docto.cod-emitente = emitente.cod-emitente
               wt-docto.nome-abrev   = emitente.nome-abrev
               wt-docto.no-ab-reppri = "Rep. Padr∆o"
               wt-docto.modalidade   = 1
               wt-docto.cod-portador = 99999.

        ASSIGN wt-docto.cod-cond-pag = 0
               wt-docto.nr-tab-finan = 1
               wt-docto.nr-ind-finan = 1
               OVERLAY(wt-docto.char-1,158,1) = "0"
               wt-docto.cidade-cif   = "Sao Bernardo do Campo".

        /* Alimenta transportador */
        FIND FIRST embarque
             WHERE embarque.cdd-embarq = nota-fiscal.cdd-embarq NO-LOCK NO-ERROR.

        ASSIGN wt-docto.nome-transp   = IF AVAIL embarque THEN embarque.nome-transp ELSE nota-fiscal.nome-transp.

        RUN atualizaDadosGeraisNota IN h-bodi317sd(INPUT  i-seq-wt-docto,
                                                   OUTPUT l-proc-ok-aux).

        IF l-proc-ok-aux = NO THEN DO:
            RUN pi-error.
            RUN pi-cancela.
            RETURN "NOK".
        END.

        /* limpar a tabela de erros em todas as bos */
        RUN emptyRowErrors IN h-bodi317in.
    
        RUN localizaWtDocto IN h-bodi317sd(INPUT  i-seq-wt-docto,
                                           OUTPUT l-proc-ok-aux).
        IF l-proc-ok-aux = NO THEN DO:
            RUN pi-error.
            RUN pi-cancela.
            RETURN "NOK".
        END.


        ASSIGN i-ultimo-resumo = 0
               tt-qt-volumes   = 0.

        FOR EACH  res-emb NO-LOCK
           WHERE res-emb.cdd-embarq = nota-fiscal.cdd-embarq 
              BY res-emb.cdd-embarq
              BY res-emb.nr-resumo:
              ASSIGN  i-ultimo-resumo = res-emb.nr-resumo
                  tt-qt-volumes = tt-qt-volumes + res-emb.qt-volumes.
        END.
         
        IF nota-fiscal.nr-resumo =  i-ultimo-resumo THEN DO:
            
            FOR FIRST res-emb EXCLUSIVE-LOCK
               WHERE res-emb.cdd-embarq = nota-fiscal.cdd-embarq:
    
                run criaAlteraWtNotaEmbal in h-bodi317sd(input  YES,
                                                         input  i-seq-wt-docto,
                                                         input  res-emb.sigla-emb,
                                                         input  tt-qt-volumes,
                                                         input  res-emb.desc-vol,
                                                         output l-proc-ok-aux).
            END.
          
        
            run devolveErrosbodi317sd in h-bodi317sd(output c-ultimo-metodo-exec,
                                                 output table RowErrors).
    
            /* pesquisa algum erro ou advertància que tenha ocorrido */
            IF CAN-FIND(FIRST RowErrors 
                        WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
        
               RUN pi-error.
               RUN pi-cancela.
               RETURN "NOK".
            END.

        END.

        FIND FIRST b2-natur-oper
             WHERE b2-natur-oper.nat-operacao = if-natur-oper.nat-oper-transf NO-LOCK NO-ERROR.
    
        FOR EACH b-it-nota-fisc OF nota-fiscal NO-LOCK:

            /* cria um item para nota fiscal. */
            RUN criaWtItDocto IN h-bodi317sd(INPUT ?,                                 /*    row da ped-ent           */
                                             INPUT "",                                /*     nome da tabela ped-ent          */
                                             INPUT b-it-nota-fisc.nr-seq-fat,         /* sequencia     */
                                             INPUT b-it-nota-fisc.it-codigo,          /* item          */
                                             INPUT b-it-nota-fisc.cod-refer,          /* referencia    */
                                             INPUT if-natur-oper.nat-oper-transf,     /* nat. operacao */
                                            OUTPUT i-seq-wt-it-docto,                 /* sequencia     */
                                            OUTPUT l-proc-ok-aux).                    /* erro          */

            /* busca poss°veis erros que ocorreram nas validaá‰es */
            RUN devolveErrosBodi317sd IN h-bodi317sd(OUTPUT c-ultimo-metodo-exec,
                                                     OUTPUT TABLE RowErrors).
            IF CAN-FIND(FIRST RowErrors 
                        WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
               RUN pi-error.
               RUN pi-cancela.
               RETURN "NOK".
            END.
            
            tt-qt-lotes = 0.

            IF b2-natur-oper.baixa-estoq THEN DO:
                FOR EACH fat-ser-lote OF b-it-nota-fisc NO-LOCK:

                    tt-qt-lotes = tt-qt-lotes + 1.

                    RUN criaAlteraWtFatSerLote IN h-bodi317sd(INPUT YES,                       /* Inclusao      */
                                                              INPUT i-seq-wt-docto,            /* Seq Docto     */
                                                              INPUT i-seq-wt-it-docto,         /* Seq ItDocto   */
                                                              INPUT fat-ser-lote.it-codigo,    /* Item          */
                                                              INPUT fat-ser-lote.cod-depos,    /* Deposito      */
                                                              INPUT fat-ser-lote.cod-localiz,  /* Localizacao   */
                                                              INPUT fat-ser-lote.nr-serlote,   /* Lote          */
                                                              INPUT fat-ser-lote.qt-baixada[1],   /* Quantidade    */
                                                              INPUT fat-ser-lote.qt-baixada[1],   /* Quantidade Contada   */
                                                              INPUT fat-ser-lote.dt-vali-lote, /* Dt.Valid.Lote */
                                                             OUTPUT l-proc-ok-aux).            /* erro          */
    
                   IF CAN-FIND(FIRST RowErrors 
                               WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
                      RUN pi-error.
                      RUN pi-cancela.
                      RETURN "NOK".
                   END. 
                END.
            END.






            /* Coloca a Embalagem */
            FOR FIRST res-emb EXCLUSIVE-LOCK
               WHERE res-emb.cdd-embarq = nota-fiscal.cdd-embarq
                 AND res-emb.nr-resumo   = nota-fiscal.nr-resumo:

                run criaVolumesWtItemEmbal in h-bodi317sd (input  tt-qt-lotes,
                                                           input  i-seq-wt-docto,
                                                           input  i-seq-wt-it-docto,
                                                           input  res-emb.sigla-emb,
                                                           input  tt-qt-lotes,
                                                           output l-proc-ok-aux).
            END.
            

            RUN devolveErrosBodi317sd IN h-bodi317sd(OUTPUT c-ultimo-metodo-exec,
                                                     OUTPUT TABLE RowErrors).

            IF CAN-FIND(FIRST RowErrors 
                        WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
               RUN pi-error.
               RUN pi-cancela.
               RETURN "NOK".
            END.


            

            /* grava informaá‰es gerais para o item da nota */
            RUN gravaInfGeraisWtItDocto IN h-bodi317sd (INPUT i-seq-wt-docto,                /* Sequencia                        */
                                                        INPUT i-seq-wt-it-docto,             /* sequencia do item                */
                                                        INPUT b-it-nota-fisc.qt-faturada[1], /* quantidade                       */
                                                        INPUT truncate(b-it-nota-fisc.vl-preuni * 0.88 ,2) ,      /* preco do item                    */
                                                        INPUT 0,                             /* percentual de desconto tab-preco */
                                                        INPUT 0).                            /* percentual de desconto do item   */


            /* Logica para observaá∆o da Nota */
           
            FIND FIRST wt-it-docto EXCLUSIVE-LOCK
                 WHERE wt-it-docto.seq-wt-docto = i-seq-wt-docto
                   AND wt-it-docto.seq-wt-it-docto = i-seq-wt-it-docto
                  NO-ERROR.
            IF AVAIL wt-it-docto THEN
            DO:
                ASSIGN wt-it-docto.quantidade[2] = wt-it-docto.quantidade[1]
                       wt-it-docto.un[2]         = wt-it-docto.un[1] .
                RELEASE wt-it-docto.
            END.
           
            FIND FIRST wt-docto WHERE wt-docto.seq-wt-docto = i-seq-wt-docto EXCLUSIVE-LOCK NO-ERROR.
           
            /* limpar a tabela de erros em todas as bos */
            RUN emptyRowErrors IN h-bodi317in.
           
            /* disp. registro wt-docto, wt-it-docto e wt-it-imposto na bodi317pr */
            RUN localizaWtDocto IN h-bodi317pr (INPUT i-seq-wt-docto,
                                               OUTPUT l-proc-ok-aux).
           
            RUN localizaWtItdocto IN h-bodi317pr (INPUT i-seq-wt-docto,
                                                  INPUT i-seq-wt-it-docto,
                                                 OUTPUT l-proc-ok-aux).
           
            RUN localizaWtItImposto IN h-bodi317pr (INPUT i-seq-wt-docto,
                                                    INPUT i-seq-wt-it-docto,
                                                   OUTPUT l-proc-ok-aux).


            /* atualiza dados c†lculados do item */
            RUN atualizaDadosItemNota IN h-bodi317pr(OUTPUT l-proc-ok-aux).
           
            FIND FIRST wt-it-docto EXCLUSIVE-LOCK USE-INDEX seq-tabela
                 WHERE wt-it-docto.seq-wt-docto    = i-seq-wt-docto
                   AND wt-it-docto.seq-wt-it-docto = i-seq-wt-it-docto NO-ERROR.
            IF AVAIL wt-it-docto THEN DO:
                ASSIGN wt-it-docto.quantidade[2] = wt-it-docto.quantidade[1]
                       wt-it-docto.un[2]         = wt-it-docto.un[1]. 
                RELEASE wt-it-docto.
            END.
           
            /* busca poss°veis erros que ocorreram nas validaá‰es */
            RUN devolveErrosBodi317pr IN h-bodi317pr(OUTPUT c-ultimo-metodo-exec,
                                                     OUTPUT table RowErrors).
           
            IF CAN-FIND(FIRST RowErrors 
                        WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
                RUN pi-error.
                RUN pi-cancela.
                RETURN "NOK".
            END.

            /* limpar a tabela de erros em todas as bos */
            RUN emptyRowErrors IN h-bodi317in.
            /* valida informaá‰es do item */
           
            RUN validaItemDaNota IN h-bodi317va(INPUT i-seq-wt-docto,
                                                INPUT i-seq-wt-it-docto,
                                                OUTPUT l-proc-ok-aux).
           
            /* busca poss°veis erros que ocorreram nas validaá‰es */
            RUN devolveErrosBodi317va IN h-bodi317va(OUTPUT c-ultimo-metodo-exec,
                                                     OUTPUT table RowErrors).
           
            IF CAN-FIND(FIRST RowErrors 
                        WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
                RUN pi-error.
                RUN pi-cancela.
                RETURN "NOK".
            END.
        END.

        
  
        RUN pi-verifica-qtd-emb-transf (OUTPUT l-dif).
        /* Se estiver diferente pula para proxima nota */
    
        IF NOT l-dif THEN DO:

            RUN pi-alimenta-tms-transf.
    
            run emptyRowErrors           in h-bodi317in.
            run disableMessage15299      in h-bodi317va.
            run inicializaAcompanhamento in h-bodi317pr.
            run calculaWtDocto in h-bodi317pr (input  i-seq-wt-docto,
                                               output l-proc-ok-aux).
            run finalizaAcompanhamento   in h-bodi317pr.
            run devolveErrosbodi317pr    in h-bodi317pr(output c-ultimo-metodo-exec,
         
                                                           output table RowErrors).
            FOR FIRST RowErrors
                where RowErrors.ErrorSubType = "ERROR":U 
                and   RowErrors.ErrorNumber <> 15299: 
                /* MENSAGEM: 15299 - Duplicatas n∆o foram geradas para a nota, n∆o dever† serr considerada. */
                DELETE RowErrors. 
            END.
      

            IF CAN-FIND(FIRST RowErrors 
                        WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
                RUN pi-error.
                RUN pi-cancela.
                RETURN "NOK".
            END.
    
            run emptyRowErrors           in h-bodi317in.
           /* RUN pi-cancela. */
    
            run dibo/bodi317ef.p persistent set h-bodi317ef.
    
            run inicializaAcompanhamento in h-bodi317ef.
            run setaHandlesBOS           in h-bodi317ef(h-bodi317pr,     h-bodi317sd, 
                                                        h-bodi317im1bra, h-bodi317va).
            
            run efetivaNota              in h-bodi317ef(input i-seq-wt-docto,
                                                        input yes,
                                                        output l-proc-ok-aux).
             
            run finalizaAcompanhamento   in h-bodi317ef.

             
            run devolveErrosbodi317ef    in h-bodi317ef(output c-ultimo-metodo-exec,
                                                        output table RowErrors).
    
    
            IF CAN-FIND(FIRST RowErrors 
                        WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
                RUN pi-error.
                RUN pi-cancela.
                RETURN "NOK".
            END.
    
            run buscaTTNotasGeradas in h-bodi317ef(output l-proc-ok-aux,
                                                   output table tt-notas-geradas).
            delete procedure h-bodi317ef.
            ASSIGN h-bodi317ef = ?. 
       
            /*     RUN pi-cancela.
            
           
         */

            IF CAN-FIND(FIRST tt-erro) THEN DO:
                FOR EACH tt-erro:
                    RUN pi-gera-erro(INPUT tt-erro.cd-erro,
                                     INPUT "ERROR":U,
                                     INPUT tt-erro.mensagem,
                                     INPUT "").
                END.
            END.
            ELSE DO:
                FOR FIRST tt-notas-geradas NO-LOCK,
                    FIRST b3-nota-fiscal exclusive-LOCK
                    WHERE ROWID(b3-nota-fiscal) = tt-notas-geradas.rw-nota-fiscal:
                    
                    c-lst-nota = "".
 
                   
 
                    FOR EACH b4-nota-fiscal WHERE b4-nota-fiscal.cdd-embarq = nota-fiscal.cdd-embarq  .



                        ASSIGN b4-nota-fiscal.cond-redespa = "Nota relacionada " + TRIM(b3-nota-fiscal.cod-estabel) + "-" +
                               TRIM(b3-nota-fiscal.serie) + "_" + TRIM(b3-nota-fiscal.nr-nota-fis)
                               b4-nota-fiscal.ind-sit-nota = 2.

                        FOR EACH b4-it-nota-fisc OF b4-nota-fiscal:
                            ASSIGN b4-it-nota-fisc.ind-sit-nota = 2.
                        END.

                        ASSIGN c-lst-nota = c-lst-nota + TRIM(b4-nota-fiscal.cod-estabel) + "-" +
                                   TRIM(b4-nota-fiscal.serie) + "_" + TRIM(b4-nota-fiscal.nr-nota-fis) + ",".

                        
    
                        /* Relacionamento Nota */
                        FIND LAST b-if-relac-nota NO-LOCK
                            WHERE b-if-relac-nota.cod-estabel  = b4-nota-fiscal.cod-estabel
                              AND b-if-relac-nota.nr-nota-fis  = b4-nota-fiscal.nr-nota-fis
                              AND b-if-relac-nota.serie        = b4-nota-fiscal.serie NO-ERROR.
        
                        IF AVAIL b-if-relac-nota THEN DO:
        
                            FIND LAST b2-if-relac-nota NO-LOCK NO-ERROR.
        
                            CREATE if-relac-nota.
                            ASSIGN if-relac-nota.seq-relac       = IF AVAIL b2-if-relac-nota THEN b2-if-relac-nota.seq-relac + 1 ELSE 1
                                   if-relac-nota.cod-estabel     = b3-nota-fiscal.cod-estabel
                                   if-relac-nota.nr-nota-fis     = b3-nota-fiscal.nr-nota-fis
                                   if-relac-nota.serie           = b3-nota-fiscal.serie
                                   if-relac-nota.nro-docto       = b-if-relac-nota.nro-docto
                                   if-relac-nota.serie-docto     = b-if-relac-nota.serie-docto
                                   if-relac-nota.cod-emitente    = b-if-relac-nota.cod-emitente
                                   if-relac-nota.nat-operacao    = b-if-relac-nota.nat-operacao
                                   if-relac-nota.recid-nf-origem = RECID(b4-nota-fiscal).            .
                        END.

                         
        
                    END.

                    ASSIGN b3-nota-fiscal.cond-redespa = "Notas Original Relacionadas " + TRIM(c-lst-nota).

                    ASSIGN c-lst-nota = "".
                                        
                    ASSIGN c-mensagem = string(b3-nota-fiscal.nr-nota-fis) + "~~" +
                                        string(b3-nota-fiscal.cod-estabel)  + "~~" +
                                        string(b3-nota-fiscal.serie).
                
                    RUN pi-gera-erro(INPUT  15263,
                                     INPUT "WARNING",
                                     INPUT c-mensagem,
                                     INPUT "").
                END.
            END.
        END.
    END.

    IF CAN-FIND(FIRST tt-erro WHERE tt-erro.cd-erro <> 15263 ) THEN DO:
                RUN pi-error.
                RUN pi-cancela.
                RETURN "NOK".
       

    END.
    ELSE do:

            
        d-perc-atend = 0.
        FOR EACH b4-nota-fiscal WHERE b4-nota-fiscal.cdd-embarq = nota-fiscal.cdd-embarq  NO-LOCK ,


          EACH b-it-nota-fisc OF b4-nota-fiscal NO-LOCK,

           each  bt-ped-venda where bt-ped-venda.nr-pedcli  = b-it-nota-fisc.nr-pedcli and
                                    bt-ped-venda.nome-abrev = b-it-nota-fisc.nome-ab-cli no-lock,
         
           each  bt-if-ped-venda where bt-if-ped-venda.nr-pedido = bt-ped-venda.nr-pedido no-lock,
           each  bt-ped-item  where  bt-ped-item.nr-pedcli       = bt-ped-venda.nr-pedcli  and
                                     bt-ped-item.nome-abrev      = bt-ped-venda.nome-abrev and
                                     bt-ped-item.nr-sequencia    = b-it-nota-fisc.nr-seq-ped  exclusive-lock,
                                     
  
           
           
           each  bt2-ped-venda where bt2-ped-venda.nr-pedido   = bt-if-ped-venda.nr-pedido-relac no-lock, 
                     
           each  bt2-ped-item where  bt2-ped-item.nr-pedcli    = bt2-ped-venda.nr-pedcli  and
                                     bt2-ped-item.nome-abrev   = bt2-ped-venda.nome-abrev and
                                     bt2-ped-item.nr-sequencia = bt-ped-item.nr-sequencia  and
                                     bt2-ped-item.it-codigo    = bt-ped-item.it-codigo    exclusive-lock
                                     by bt-ped-item.nome-abrev
                                     by bt-ped-item.nr-pedcli                                      
                                     by bt-ped-item.nr-sequencia
                                     by bt-ped-item.ind-componen.
              d-perc-atend = 0.                            
          
               if bt-ped-item.ind-componen < 3 then 
                      d-perc-atend = bt-ped-item.qt-atendida / bt-ped-item.qt-pedida   * 100.

               IF d-perc-atend >= 90  THEN do:
                                                          
           
                   FIND FIRST b-estabelec WHERE
                       b-estabelec.cod-estabel = bt2-ped-venda.cod-estabel
                       NO-LOCK NO-ERROR.
            
                   IF AVAIL b-estabelec THEN DO:
                   
                   
                   
                      FIND FIRST pd-compl-pedido WHERE
                           pd-compl-pedido.ep-codigo            = b-estabelec.ep-codigo   AND
                           pd-compl-pedido.nr-pedido            = bt2-ped-venda.nr-pedido   AND
                           pd-compl-pedido.nr-sequencia         = bt2-ped-item.nr-sequencia 
                           NO-LOCK NO-ERROR.
                      
                      IF AVAIL pd-compl-pedido THEN DO:
                             bt-ped-item.dt-entrega     = pd-compl-pedido.dt-faturamento.
                             bt2-ped-item.dt-entrega    = pd-compl-pedido.dt-faturamento.
    
                      
                           for each ped-ent of bt-ped-item exclusive-lock.
                               ped-ent.dt-entrega = pd-compl-pedido.dt-faturamento.
                           end. 
                           for each ped-ent of bt2-ped-item exclusive-lock.
                               ped-ent.dt-entrega = pd-compl-pedido.dt-faturamento.
                           end.                                                                      
                                                         
                       end.
                       
                   end.    
               end.
        end.
    
    
    end.

     ASSIGN c-lst-nota = "".
        RETURN "OK".
END PROCEDURE.

PROCEDURE pi-remessa-armazenagem :
    /*------------------------------------------------------------------------------
  Purpose:     Carrega as informaá‰es para faturamento posterior
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE i-seq-wt-it-docto AS INTEGER     NO-UNDO.

        FIND FIRST if-natur-oper NO-LOCK
             WHERE if-natur-oper.cod-estab-orig  = ped-venda.cod-estabel
               AND if-natur-oper.cod-estab-inter = ""
               AND if-natur-oper.cod-estab-dest  = b2-ped-venda.cod-estabel
               AND if-natur-oper.nat-oper-pedido = if-ped-venda.nat-oper-orig NO-ERROR.
        IF AVAIL if-natur-oper AND if-natur-oper.nat-oper-remes-armaz <> "" AND if-natur-oper.cod-emitente <> 0 THEN DO:

            FIND FIRST emitente
                 WHERE emitente.cod-emitente = if-natur-oper.cod-emitente NO-LOCK NO-ERROR.

            RUN dibo/bodi317in.p PERSISTENT SET h-bodi317in.

            RUN inicializabos IN h-bodi317in(OUTPUT h-bodi317pr,
                                             OUTPUT h-bodi317sd,
                                             OUTPUT h-bodi317im1bra,
                                             OUTPUT h-bodi317va).

            RUN inicializaAcompanhamento IN  h-bodi317sd.

            RUN emptyRowErrors   IN h-bodi317in.

            RUN criaWtDocto IN h-bodi317sd (INPUT c-seg-usuario,                     /* usu†rio                                */
                                            INPUT b2-ped-venda.cod-estabel,           /* estabelecimento                        */
                                            INPUT nota-fiscal.serie,                 /* serie                                  */
                                            INPUT "",                                /* numero de nota caso seja notas manuais */
                                            INPUT emitente.nome-abrev,               /* nome-abrev emitente                    */
                                            INPUT ?,                                 /* pedido                                 */
                                            INPUT 4,                                 /* tipo de nota                           */
                                            INPUT 4003,                              /* codigo do programa que  gerou a nota   */
                                            INPUT TODAY,                             /* data emissao nota                      */
                                            INPUT 0,                                 /* embarque                               */
                                            INPUT if-natur-oper.nat-oper-remes-armaz,/* natureza operacao                      */
                                            INPUT 0,                                 /* canal venda                            */
                                           OUTPUT i-seq-wt-docto,                    /* seq docto                              */
                                           OUTPUT l-proc-ok-aux).                    /* ok Sim/Nao?                            */

            /* busca poss°veis erros que ocorreram nas validaá‰es */
            RUN devolveErrosBodi317sd IN h-bodi317sd (OUTPUT c-ultimo-metodo-exec,
                                                      OUTPUT table RowErrors).

            /* pesquisa algum erro ou advertància que tenha ocorrido */
            IF CAN-FIND(FIRST RowErrors 
                        WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:

               RUN pi-error.
               RUN pi-cancela.
               RETURN "NOK".
            END.

            /* disponibilizar o registro wt-docto na bodi317sd */
            FIND FIRST wt-docto where wt-docto.seq-wt-docto = i-seq-wt-docto NO-ERROR.

            /* disponibilizar o registro wt-docto na bodi317sd */
            ASSIGN wt-docto.cod-emitente = emitente.cod-emitente
                   wt-docto.nome-abrev   = emitente.nome-abrev
                   wt-docto.no-ab-reppri = nota-fiscal.no-ab-reppri
                   wt-docto.modalidade   = nota-fiscal.modalidade
                   wt-docto.cod-portador = nota-fiscal.cod-portador.

            FIND FIRST cond-pagto NO-LOCK
                 WHERE cond-pagto.cod-cond-pag = nota-fiscal.cod-cond-pag NO-ERROR.
            IF AVAIL cond-pagto THEN DO:
                ASSIGN wt-docto.cod-cond-pag = cond-pagto.cod-cond-pag
                       wt-docto.nr-tab-finan = cond-pagto.nr-tab-finan
                       wt-docto.nr-ind-finan = cond-pagto.nr-ind-finan.
            END.

            RUN atualizaDadosGeraisNota IN h-bodi317sd(INPUT  i-seq-wt-docto,
                                                       OUTPUT l-proc-ok-aux).
    
            IF l-proc-ok-aux = NO THEN DO:
                RUN pi-error.
                RUN pi-cancela.
                RETURN "NOK".
            END.

            /* limpar a tabela de erros em todas as bos */
            RUN emptyRowErrors IN h-bodi317in.

            RUN localizaWtDocto IN h-bodi317sd(INPUT  i-seq-wt-docto,
                                               OUTPUT l-proc-ok-aux).
            IF l-proc-ok-aux = NO THEN DO:
                RUN pi-error.
                RUN pi-cancela.
                RETURN "NOK".
            END.

            FOR EACH res-emb EXCLUSIVE-LOCK
               WHERE res-emb.cdd-embarq = nota-fiscal.cdd-embarq
                 AND res-emb.nr-resumo   = nota-fiscal.nr-resumo:

                run criaAlteraWtNotaEmbal in h-bodi317sd(input  YES,
                                                         input  i-seq-wt-docto,
                                                         input  res-emb.sigla-emb,
                                                         input  res-emb.qt-volumes,
                                                         input  res-emb.desc-vol,
                                                         output l-proc-ok-aux).
            END.

            run devolveErrosbodi317sd in h-bodi317sd(output c-ultimo-metodo-exec,
                                                     output table RowErrors).

            /* pesquisa algum erro ou advertància que tenha ocorrido */
            IF CAN-FIND(FIRST RowErrors 
                        WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:

               RUN pi-error.
               RUN pi-cancela.
               RETURN "NOK".
            END.


            FIND FIRST b2-natur-oper
                 WHERE b2-natur-oper.nat-operacao = if-natur-oper.nat-oper-remes-armaz NO-LOCK NO-ERROR.

            FOR EACH b-it-nota-fisc OF nota-fiscal NO-LOCK:

                /* cria um item para nota fiscal. */
                RUN criaWtItDocto IN h-bodi317sd(INPUT ?,                                 /*               */
                                                 INPUT "",                                /*               */
                                                 INPUT b-it-nota-fisc.nr-seq-fat,         /* sequencia     */
                                                 INPUT b-it-nota-fisc.it-codigo,          /* item          */
                                                 INPUT "",                                /* referencia    */
                                                 INPUT if-natur-oper.nat-oper-remes-armaz,/* nat. operacao */
                                                OUTPUT i-seq-wt-it-docto,                 /* sequencia     */
                                                OUTPUT l-proc-ok-aux).                    /* erro          */

                /* busca poss°veis erros que ocorreram nas validaá‰es */
                RUN devolveErrosBodi317sd IN h-bodi317sd(OUTPUT c-ultimo-metodo-exec,
                                                         OUTPUT TABLE RowErrors).
                IF CAN-FIND(FIRST RowErrors 
                            WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
                   RUN pi-error.
                   RUN pi-cancela.
                   RETURN "NOK".
                END.

                /* Coloca a Embalagem */
                FOR EACH res-emb EXCLUSIVE-LOCK
                   WHERE res-emb.cdd-embarq = nota-fiscal.cdd-embarq
                     AND res-emb.nr-resumo   = nota-fiscal.nr-resumo:

                    run criaVolumesWtItemEmbal in h-bodi317sd (input  res-emb.qt-volumes,
                                                               input  i-seq-wt-docto,
                                                               input  i-seq-wt-it-docto,
                                                               input  res-emb.sigla-emb,
                                                               input  res-emb.qt-volumes,
                                                               output l-proc-ok-aux).
                END.


                RUN devolveErrosBodi317sd IN h-bodi317sd(OUTPUT c-ultimo-metodo-exec,
                                                         OUTPUT TABLE RowErrors).

                IF CAN-FIND(FIRST RowErrors 
                            WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
                   RUN pi-error.
                   RUN pi-cancela.
                   RETURN "NOK".
                END.


                IF b2-natur-oper.baixa-estoq THEN DO:
                    FOR EACH fat-ser-lote OF b-it-nota-fisc NO-LOCK:

                        RUN criaAlteraWtFatSerLote IN h-bodi317sd(INPUT YES,                       /* Inclusao      */
                                                                  INPUT i-seq-wt-docto,            /* Seq Docto     */
                                                                  INPUT i-seq-wt-it-docto,         /* Seq ItDocto   */
                                                                  INPUT fat-ser-lote.it-codigo,    /* Item          */
                                                                  INPUT fat-ser-lote.cod-depos,    /* Deposito      */
                                                                  INPUT fat-ser-lote.cod-localiz,  /* Localizacao   */
                                                                  INPUT fat-ser-lote.nr-serlote,   /* Lote          */
                                                                  INPUT it-nota-fisc.qt-faturada[2],   /* Quantidade    */
                                                                  INPUT it-nota-fisc.qt-faturada[2],   /* Quantidade Contada   */
                                                                  INPUT fat-ser-lote.dt-vali-lote, /* Dt.Valid.Lote */
                                                                 OUTPUT l-proc-ok-aux).            /* erro          */

                       IF CAN-FIND(FIRST RowErrors 
                                   WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
                          RUN pi-error.
                          RUN pi-cancela.
                          RETURN "NOK".
                       END. 
                    END.
                END.


                /* grava informaá‰es gerais para o item da nota */
                RUN gravaInfGeraisWtItDocto IN h-bodi317sd (INPUT i-seq-wt-docto,                /* Sequencia                        */
                                                            INPUT i-seq-wt-it-docto,             /* sequencia do item                */
                                                            INPUT b-it-nota-fisc.qt-faturada[1], /* quantidade                       */
                                                            INPUT b-it-nota-fisc.vl-preuni,      /* preco do item                    */
                                                            INPUT 0,                             /* percentual de desconto tab-preco */
                                                            INPUT 0).                            /* percentual de desconto do item   */


                /* Logica para observaá∆o da Nota */

                FIND FIRST wt-it-docto EXCLUSIVE-LOCK
                     WHERE wt-it-docto.seq-wt-docto = i-seq-wt-docto
                       AND wt-it-docto.seq-wt-it-docto = i-seq-wt-it-docto
                      NO-ERROR.
                IF AVAIL wt-it-docto THEN
                DO:
                    ASSIGN wt-it-docto.quantidade[2] = wt-it-docto.quantidade[1]
                           wt-it-docto.un[2]         = wt-it-docto.un[1]
                           wt-it-docto.baixa-estoq   = NO.
                    RELEASE wt-it-docto.
                END.

                FIND FIRST wt-docto WHERE wt-docto.seq-wt-docto = i-seq-wt-docto EXCLUSIVE-LOCK NO-ERROR.

                /* limpar a tabela de erros em todas as bos */
                RUN emptyRowErrors IN h-bodi317in.

                /* disp. registro wt-docto, wt-it-docto e wt-it-imposto na bodi317pr */
                RUN localizaWtDocto IN h-bodi317pr (INPUT i-seq-wt-docto,
                                                   OUTPUT l-proc-ok-aux).

                RUN localizaWtItdocto IN h-bodi317pr (INPUT i-seq-wt-docto,
                                                      INPUT i-seq-wt-it-docto,
                                                     OUTPUT l-proc-ok-aux).

                RUN localizaWtItImposto IN h-bodi317pr (INPUT i-seq-wt-docto,
                                                        INPUT i-seq-wt-it-docto,
                                                       OUTPUT l-proc-ok-aux).


                /* atualiza dados c†lculados do item */
                RUN atualizaDadosItemNota IN h-bodi317pr(OUTPUT l-proc-ok-aux).

                FIND FIRST wt-it-docto EXCLUSIVE-LOCK USE-INDEX seq-tabela
                     WHERE wt-it-docto.seq-wt-docto    = i-seq-wt-docto
                       AND wt-it-docto.seq-wt-it-docto = i-seq-wt-it-docto NO-ERROR.
                IF AVAIL wt-it-docto THEN DO:
                    ASSIGN wt-it-docto.quantidade[2] = wt-it-docto.quantidade[1]
                           wt-it-docto.un[2]         = wt-it-docto.un[1]. 
                    RELEASE wt-it-docto.
                END.

                /* busca poss°veis erros que ocorreram nas validaá‰es */
                RUN devolveErrosBodi317pr IN h-bodi317pr(OUTPUT c-ultimo-metodo-exec,
                                                         OUTPUT table RowErrors).

                IF CAN-FIND(FIRST RowErrors 
                            WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
                    RUN pi-error.
                    RUN pi-cancela.
                    RETURN "NOK".
                END.

                /* limpar a tabela de erros em todas as bos */
                RUN emptyRowErrors IN h-bodi317in.
                /* valida informaá‰es do item */

                RUN validaItemDaNota IN h-bodi317va(INPUT i-seq-wt-docto,
                                                    INPUT i-seq-wt-it-docto,
                                                    OUTPUT l-proc-ok-aux).

                /* busca poss°veis erros que ocorreram nas validaá‰es */
                RUN devolveErrosBodi317va IN h-bodi317va(OUTPUT c-ultimo-metodo-exec,
                                                         OUTPUT table RowErrors).

                IF CAN-FIND(FIRST RowErrors 
                            WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
                    RUN pi-error.
                    RUN pi-cancela.
                    RETURN "NOK".
                END.
            END.

            run emptyRowErrors           in h-bodi317in.
            run disableMessage15299      in h-bodi317va.
            run inicializaAcompanhamento in h-bodi317pr.
            run calculaWtDocto in h-bodi317pr (input  i-seq-wt-docto,
                                               output l-proc-ok-aux).
            run finalizaAcompanhamento   in h-bodi317pr.
            run devolveErrosbodi317pr    in h-bodi317pr(output c-ultimo-metodo-exec,
                                                        output table RowErrors).
            FOR FIRST RowErrors
                where RowErrors.ErrorSubType = "ERROR":U 
                and   RowErrors.ErrorNumber <> 15299: 
                /* MENSAGEM: 15299 - Duplicatas n∆o foram geradas para a nota, n∆o dever† serr considerada. */
                DELETE RowErrors. 
            END.

            IF CAN-FIND(FIRST RowErrors 
                        WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
                RUN pi-error.
                RUN pi-cancela.
                RETURN "NOK".
            END.

            run emptyRowErrors           in h-bodi317in.
            RUN pi-cancela.

            run dibo/bodi317ef.p persistent set h-bodi317ef.

            run inicializaAcompanhamento in h-bodi317ef.
            run setaHandlesBOS           in h-bodi317ef(h-bodi317pr,     h-bodi317sd, 
                                                        h-bodi317im1bra, h-bodi317va).
            run efetivaNota              in h-bodi317ef(input i-seq-wt-docto,
                                                        input yes,
                                                        output l-proc-ok-aux).

            run finalizaAcompanhamento   in h-bodi317ef.
            run devolveErrosbodi317ef    in h-bodi317ef(output c-ultimo-metodo-exec,
                                                        output table RowErrors).


            IF CAN-FIND(FIRST RowErrors 
                        WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
                RUN pi-error.
                RUN pi-cancela.
                RETURN "NOK".
            END.

            run buscaTTNotasGeradas in h-bodi317ef(output l-proc-ok-aux,
                                                   output table tt-notas-geradas).

            RUN pi-cancela.

            delete procedure h-bodi317ef.
            ASSIGN h-bodi317ef = ?.

            IF CAN-FIND(FIRST tt-erro) THEN DO:
                FOR EACH tt-erro:
                    RUN pi-gera-erro(INPUT tt-erro.cd-erro,
                                     INPUT "ERROR":U,
                                     INPUT tt-erro.mensagem,
                                     INPUT "").
                END.
            END.
            ELSE DO:
                FOR FIRST tt-notas-geradas NO-LOCK,
                    FIRST b3-nota-fiscal NO-LOCK
                    WHERE ROWID(b3-nota-fiscal) = tt-notas-geradas.rw-nota-fiscal:

                    ASSIGN c-mensagem = string(b3-nota-fiscal.nr-nota-fis) + "~~" +
                                        string(b3-nota-fiscal.cod-estabel)  + "~~" +
                                        string(b3-nota-fiscal.serie).

                    RUN pi-gera-erro(INPUT  15263,
                                     INPUT "WARNING",
                                     INPUT c-mensagem,
                                     INPUT "").
                END.
            END.
        END.

        IF CAN-FIND(FIRST tt-erro) THEN
            RETURN "NOK".
        ELSE
            RETURN "OK".


END PROCEDURE.

PROCEDURE pi-carreg-if-triang :
/*------------------------------------------------------------------------------
  Purpose:     Carrega as informaá‰es para faturamento posterior
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE l-encont-estab   AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE c-cod-estab-nota AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE rw-nota-estab    AS ROWID       NO-UNDO.

    FIND FIRST if-natur-oper NO-LOCK
         WHERE if-natur-oper.cod-estab-orig  = ped-venda.cod-estabel
           AND if-natur-oper.cod-estab-inter = ""
           AND if-natur-oper.cod-estab-dest  = b2-ped-venda.cod-estabel
           AND if-natur-oper.nat-oper-pedido = if-ped-venda.nat-oper-orig NO-ERROR.
    IF AVAIL if-natur-oper AND if-natur-oper.nat-oper-entr-ct-ord <> "" AND if-natur-oper.nat-oper-said-ct-ord <> "" THEN DO:

        FOR EACH tt-notas-ger-aux NO-LOCK,
           FIRST b2-nota-fiscal NO-LOCK
           WHERE ROWID(b2-nota-fiscal) = tt-notas-ger-aux.rw-nota-fiscal,
           FIRST estabelec NO-LOCK
           WHERE estabelec.cod-estabel = string(b2-nota-fiscal.cod-emitente):
            ASSIGN l-encont-estab = YES.
        END.

        IF l-encont-estab THEN DO:
    
            ASSIGN c-cod-estab-nota = ""
                   rw-nota-estab    = ?.
    
            FOR EACH tt-notas-ger-aux NO-LOCK,
               FIRST b2-nota-fiscal NO-LOCK
               WHERE ROWID(b2-nota-fiscal) = tt-notas-ger-aux.rw-nota-fiscal:
    
                /* Pegar somente a nota que foi emitida para o cliente */
                FIND FIRST estabelec
                     WHERE estabelec.cod-estabel = string(b2-nota-fiscal.cod-emitente) NO-LOCK NO-ERROR.
                IF AVAIL estabelec THEN DO:
                    ASSIGN c-cod-estab-nota = estabelec.cod-estabel
                           rw-nota-estab    = ROWID(b2-nota-fiscal).
                    NEXT.
                END.

                FIND FIRST if-nota-triang EXCLUSIVE-LOCK
                     WHERE if-nota-triang.cod-estabel = b2-nota-fiscal.cod-estabel
                       AND if-nota-triang.serie       = b2-nota-fiscal.serie
                       AND if-nota-triang.nr-nota-fis = b2-nota-fiscal.nr-nota-fis NO-ERROR.
                IF NOT AVAIL if-nota-triang THEN DO:
                    CREATE if-nota-triang.
                    ASSIGN if-nota-triang.cod-estabel         = b2-nota-fiscal.cod-estabel
                           if-nota-triang.serie               = b2-nota-fiscal.serie      
                           if-nota-triang.nr-nota-fis         = b2-nota-fiscal.nr-nota-fis
                           if-nota-triang.cod-emitente        = b2-nota-fiscal.cod-emitente
                           if-nota-triang.cod-estabel-fat     = c-cod-estab-nota
                           if-nota-triang.rw-nota-ung         = TRIM(STRING(rw-nota-estab))
                           if-nota-triang.rw-nota-cli         = TRIM(STRING(ROWID(b2-nota-fiscal)))
                           if-nota-triang.nat-oper-entr-ct-ord = if-natur-oper.nat-oper-entr-ct-ord
                           if-nota-triang.nat-oper-said-ct-ord = if-natur-oper.nat-oper-said-ct-ord.
    
                END.
            END.
        END.
    END.

END PROCEDURE. /* pi-carreg-if-triang */

PROCEDURE pi-fatura-triang :
/*------------------------------------------------------------------------------
  Purpose:     Gera Embarque
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND FIRST if-nota-triang NO-LOCK
         WHERE if-nota-triang.cod-estabel = nota-fiscal.cod-estabel
           AND if-nota-triang.serie       = nota-fiscal.serie
           AND if-nota-triang.nr-nota-fis = nota-fiscal.nr-nota-fis NO-ERROR.
    
    IF AVAIL if-nota-triang THEN DO:
        
        /* Verificar se nota est† autorizada */
        FOR FIRST sit-nf-eletro 
            WHERE sit-nf-eletro.cod-estabel  = nota-fiscal.cod-estabel  
              AND sit-nf-eletro.cod-serie    = nota-fiscal.serie        
              AND sit-nf-eletro.cod-nota-fis = nota-fiscal.nr-nota-fis  NO-LOCK: 
        END.

        FIND FIRST b4-nota-fiscal
             WHERE ROWID(b4-nota-fiscal) = TO-ROWID(rw-nota-ung) NO-LOCK NO-ERROR.
        IF AVAIL b4-nota-fiscal THEN
            FIND FIRST b-sit-nf-eletro 
                 WHERE b-sit-nf-eletro.cod-estabel  = b4-nota-fiscal.cod-estabel  
                   AND b-sit-nf-eletro.cod-serie    = b4-nota-fiscal.serie        
                   AND b-sit-nf-eletro.cod-nota-fis = b4-nota-fiscal.nr-nota-fis  NO-LOCK NO-ERROR.

        IF AVAIL sit-nf-eletro   AND sit-nf-eletro.idi-sit-nf-eletro   = 3 AND
           AVAIL b-sit-nf-eletro AND b-sit-nf-eletro.idi-sit-nf-eletro = 3 THEN DO:

            /* Faturamento das Notas - Clientes */
            RUN pi-recebimento-triang.
            IF l-possui-erro THEN RETURN "NOK".
            RUN pi-gera-nota-compl.
            IF l-possui-erro THEN RETURN "NOK".
        END.
    END.

    RETURN "OK".

END PROCEDURE. /* pi-fatura-triang */

PROCEDURE pi-gera-embarque :
/*------------------------------------------------------------------------------
  Purpose:     Gera Embarque
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE i-tipo-atend AS INTEGER     NO-UNDO.

    EMPTY TEMP-TABLE tt-alocar.
    EMPTY TEMP-TABLE tt-embarque.
    EMPTY TEMP-TABLE tt-ped-venda-aux.
    
    RUN pi-cria-tt-alocar.
    IF NOT CAN-FIND(FIRST tt-alocar NO-LOCK
                    WHERE tt-alocar.qt-atender > 0) THEN DO:

        /*RUN pi-gera-erro(INPUT 17006,                                
                         INPUT "ERROR":U,                            
                         INPUT "N∆o h† embarques para serem gerados",
                         INPUT "").*/
        
        RETURN "NOK".
    END.

    ASSIGN i-sequencia = 0.
    
    RUN eqp/eqapi300.p PERSISTENT SET h-eqapi300.

    FIND FIRST embarque
         WHERE embarque.cdd-embarq = nota-fiscal.cdd-embarq NO-LOCK NO-ERROR.

    gera-embarque:
    DO TRANSACTION:
        FOR EACH tt-alocar NO-LOCK
           WHERE tt-alocar.qt-atender > 0
           BREAK BY(tt-alocar.nome-abrev):

            IF FIRST-OF(tt-alocar.nome-abrev) THEN DO:
                EMPTY TEMP-TABLE tt-embarque.
                EMPTY TEMP-TABLE tt-aloc-man.
                EMPTY TEMP-TABLE tt-erro.
                EMPTY TEMP-TABLE tt-erro-aux.
                EMPTY TEMP-TABLE tt-ped-venda-aux.
                
                FIND FIRST if-relac-embarque
                     WHERE if-relac-embarque.cdd-embarq-ung-com = nota-fiscal.cdd-embarq NO-LOCK NO-ERROR.

                IF AVAIL if-relac-embarque THEN DO:
                    ASSIGN i-sequencia = i-sequencia + 1
                           i-embarque  = if-relac-embarque.cdd-embarq-cli.

                    CREATE tt-embarque.
                    ASSIGN tt-embarque.usuario       = c-seg-usuario
                           tt-embarque.identific     = c-seg-usuario
                           tt-embarque.cdd-embarq   = i-embarque
                           tt-embarque.dt-embarque   = TODAY
                           tt-embarque.i-sequen      = i-sequencia
                           tt-embarque.ind-oper      = 2 /* Alteraá∆o */
                           tt-embarque.cod-estabel   = tt-alocar.cod-estabel
                           tt-embarque.cod-rota      = tt-alocar.cod-rota
                           tt-embarque.nome-transp   = IF AVAIL embarque THEN embarque.nome-transp ELSE nota-fiscal.nome-transp
                           tt-embarque.placa         = nota-fiscal.placa
                           tt-embarque.uf-placa      = IF AVAIL embarque THEN embarque.uf-placa  ELSE ""
                           tt-embarque.motorista     = IF AVAIL embarque THEN embarque.motorista ELSE "".
                END.
                ELSE DO:
                    FIND LAST b-embarque NO-LOCK NO-ERROR.
                    ASSIGN i-embarque  = IF AVAIL b-embarque THEN b-embarque.cdd-embarq + 1 ELSE 1
                           i-sequencia = i-sequencia + 1.

                    CREATE tt-embarque.
                    ASSIGN tt-embarque.usuario       = c-seg-usuario
                           tt-embarque.identific     = IF AVAIL embarque THEN embarque.identific ELSE c-seg-usuario
                           tt-embarque.cdd-embarq   = i-embarque
                           tt-embarque.dt-embarque   = IF AVAIL embarque THEN embarque.dt-embarque ELSE TODAY
                           tt-embarque.i-sequen      = i-sequencia
                           tt-embarque.ind-oper      = 1 /* Inclusao */
                           tt-embarque.cod-estabel   = tt-alocar.cod-estabel
                           tt-embarque.cod-rota      = tt-alocar.cod-rota
                           tt-embarque.nome-transp   = IF AVAIL embarque THEN embarque.nome-transp ELSE nota-fiscal.nome-transp
                           tt-embarque.placa         = nota-fiscal.placa
                           tt-embarque.uf-placa      = IF AVAIL embarque THEN embarque.uf-placa  ELSE ""
                           tt-embarque.motorista     = IF AVAIL embarque THEN embarque.motorista ELSE "".
                END.

                FOR EACH b3-it-nota-fisc OF nota-fiscal  NO-LOCK,
                   FIRST b4-ped-venda NO-LOCK
                   WHERE b4-ped-venda.nome-abrev = b3-it-nota-fisc.nome-ab-cli
                     AND b4-ped-venda.nr-pedcli  = b3-it-nota-fisc.nr-pedcli,
                   FIRST if-ped-venda NO-LOCK
                   WHERE if-ped-venda.nr-pedido = b4-ped-venda.nr-pedido,
                   FIRST b5-ped-venda EXCLUSIVE-LOCK
                   WHERE b5-ped-venda.nr-pedido  = if-ped-venda.nr-pedido-relac
                     AND b5-ped-venda.nome-abrev = tt-alocar.nome-abrev,
                    EACH fat-ser-lote OF b3-it-nota-fisc NO-LOCK:

                    /* IF b3-it-nota-fisc.it-codigo = "PLP0C01-0100-112" THEN LEAVE. */

                    ASSIGN i-tipo-atend = b3-it-nota-fisc.tipo-atend.

                    CREATE tt-despreza.
                    ASSIGN tt-despreza.cdd-embarq  = i-embarque
                           tt-despreza.nr-resumo    = nota-fiscal.nr-resumo
                           tt-despreza.nome-abrev   = tt-alocar.nome-abrev
                           tt-despreza.nr-pedcli    = b3-it-nota-fisc.nr-pedcli 
                           tt-despreza.nr-sequencia = b3-it-nota-fisc.nr-seq-ped
                           tt-despreza.it-codigo    = b3-it-nota-fisc.it-codigo 
                           tt-despreza.cod-estabel  = tt-alocar.cod-estabel
                           tt-despreza.cod-depos    = fat-ser-lote.cod-depos
                           tt-despreza.cod-localiz  = fat-ser-lote.cod-localiz
                           tt-despreza.lote         = fat-ser-lote.nr-serlote
                           tt-despreza.cod-refer    = b3-it-nota-fisc.cod-refer
                           tt-despreza.qt-a-alocar  = fat-ser-lote.qt-baixada[1]
                           tt-despreza.i-sequen     = i-sequencia
                           tt-despreza.nr-entrega   = b3-it-nota-fisc.nr-entrega.
                END.
            END.

            CREATE tt-ped-venda-aux.
            ASSIGN tt-ped-venda-aux.i-sequen    = i-sequencia
                   tt-ped-venda-aux.cdd-embarq  = i-embarque
                   tt-ped-venda-aux.nome-abrev  = tt-alocar.nome-abrev
                   tt-ped-venda-aux.nr-pedcli   = tt-alocar.nr-pedcli
                   tt-ped-venda-aux.ind-oper    = 1. /* Inclusao */

            IF LAST-OF(tt-alocar.nome-abrev) THEN DO:

                RUN pi-recebe-tt-embarque  IN h-eqapi300 (INPUT TABLE tt-embarque).
                RUN pi-trata-tt-embarque   IN h-eqapi300 (INPUT 'msg', INPUT YES).


                RUN pi-recebe-tt-ped-venda IN h-eqapi300 (INPUT TABLE tt-ped-venda-aux).
                RUN pi-trata-tt-ped-venda  IN h-eqapi300 (INPUT NO).

                RUN pi-despreza-itens.

                run pi-recebe-tt-aloc-man  IN h-eqapi300 (INPUT TABLE tt-aloc-man).
                run pi-trata-tt-aloc-man   IN h-eqapi300 (INPUT YES).

                RUN pi-ajusta-qtd-aloca.

                run pi-recebe-tt-aloc-man  IN h-eqapi300 (INPUT TABLE tt-aloc-man).
                run pi-trata-tt-aloc-man   IN h-eqapi300 (INPUT YES).

                /**/
                RUN pi-devolve-tt-erro     IN h-eqapi300 (OUTPUT TABLE tt-erro-aux).

                IF CAN-FIND(FIRST tt-erro-aux) THEN DO:
                    ASSIGN l-erro = NO.
                    FOR EACH tt-erro-aux:
                    
                        RUN utp/ut-msgs.p (INPUT "type",
                                           INPUT tt-erro-aux.cd-erro,
                                           INPUT "").
                           
                        if return-value <> "Erro" then
                            next.

                        ASSIGN l-erro = YES.

                        RUN pi-gera-erro(INPUT tt-erro-aux.cd-erro,                                
                                         INPUT "ERROR":U,                            
                                         INPUT tt-erro-aux.mensagem,
                                         INPUT "").
                    END.

                    IF l-erro THEN
                        UNDO gera-embarque, LEAVE.
                END.

                IF CAN-FIND(FIRST pre-fatur
                            WHERE pre-fatur.cdd-embarq = tt-embarque.cdd-embarq) THEN DO:


                    RUN pi-recebe-tt-embarque IN h-eqapi300 (INPUT TABLE tt-embarque).
                    RUN pi-encerra-embarque   IN h-eqapi300 (INPUT YES).

                    FIND FIRST if-relac-embarque
                         WHERE if-relac-embarque.cdd-embarq-ung-com = nota-fiscal.cdd-embarq
                           AND if-relac-embarque.cdd-embarq-cli     = tt-embarque.cdd-embarq NO-LOCK NO-ERROR.
                    IF NOT AVAIL if-relac-embarque THEN DO:
                        CREATE if-relac-embarque.
                        ASSIGN if-relac-embarque.cdd-embarq-ung-com = nota-fiscal.cdd-embarq
                               if-relac-embarque.cdd-embarq-cli     = tt-embarque.cdd-embarq.

                        RUN pi-alimenta-tms.
                    END.

                    FIND FIRST ext-embarque EXCLUSIVE-LOCK
                         WHERE ext-embarque.cdd-embarq = nota-fiscal.cdd-embarq NO-ERROR.
                    IF AVAIL ext-embarque THEN DO:
                        FIND FIRST b-ext-embarque
                             WHERE b-ext-embarque.cdd-embarq = tt-embarque.cdd-embarq NO-LOCK NO-ERROR.
                        IF NOT AVAIL b-ext-embarque THEN DO:
                            CREATE b-ext-embarque.
                            ASSIGN b-ext-embarque.cdd-embarq   = tt-embarque.cdd-embarq
                                   b-ext-embarque.ind-cli-retira = ext-embarque.ind-cli-retira.

                        END.
                    END.

                    FOR EACH pre-fatur EXCLUSIVE-LOCK
                       WHERE pre-fatur.cdd-embarq = tt-embarque.cdd-embarq:
                        ASSIGN pre-fatur.ind-sit-embarque = 1. /* livre */

                        /* Embalagens */
                        RUN pi-embalagens.
                    END.

                    ASSIGN p-cdd-embarq = i-embarque.
                    
                END.
                ELSE DO:
                    FIND FIRST embarque EXCLUSIVE-LOCK
                         WHERE embarque.cdd-embarq = tt-embarque.cdd-embarq NO-ERROR.
                    IF NOT CAN-FIND(FIRST pre-fatur OF embarque) THEN
                        DELETE embarque.
                END.
            END.
        END.
    END. /* fim transacao gera-embarque */

    RUN pi-finalizar IN h-eqapi300.

    EMPTY TEMP-TABLE tt-alocar.
    EMPTY TEMP-TABLE tt-erro.

END PROCEDURE. /* pi-gera-emb */

PROCEDURE pi-cria-tt-alocar :
/*------------------------------------------------------------------------------
  Purpose:     Cria tt-alocar
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Alocar os itens da nota fatura */
    FOR EACH b-it-nota-fisc OF nota-fiscal NO-LOCK,
       FIRST b4-ped-venda NO-LOCK
       WHERE b4-ped-venda.nome-abrev = b-it-nota-fisc.nome-ab-cli
         AND b4-ped-venda.nr-pedcli  = b-it-nota-fisc.nr-pedcli,
       FIRST if-ped-venda NO-LOCK
       WHERE if-ped-venda.nr-pedido = b4-ped-venda.nr-pedido,
       FIRST b5-ped-venda EXCLUSIVE-LOCK
       WHERE b5-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac:

        IF b5-ped-venda.completo = NO THEN NEXT.

        ASSIGN b5-ped-venda.cod-sit-aval = 3 /* Aprovado */.
        
        IF b5-ped-venda.dsp-pre-fat = NO THEN DO:
            FIND FIRST bf-ped-venda EXCLUSIVE-LOCK
                WHERE ROWID(bf-ped-venda) = ROWID(ped-venda) NO-ERROR.
            IF AVAIL bf-ped-venda THEN
                ASSIGN bf-ped-venda.dsp-pre-fat = YES. /* sem este valor o embarque nao e gerado e a api nao acusa o erro */
        END.
        
        /* validacao de alocacao */
        IF b5-ped-venda.cod-sit-pre = 3 THEN NEXT /*totalmente alocado */.

        FOR EACH ped-item NO-LOCK
           WHERE ped-item.nome-abrev   = b5-ped-venda.nome-abrev
             AND ped-item.nr-pedcli    = b5-ped-venda.nr-pedcli
             AND ped-item.nr-sequencia = b-it-nota-fisc.nr-seq-ped
             AND ped-item.it-codigo    = b-it-nota-fisc.it-codigo
             AND ped-item.cod-refer    = b-it-nota-fisc.cod-refer:
    
            /* PEGAR A QUANTIDADE ALOCADA NO PEDIDO ANTERIOR
            
            ASSIGN de-qt-a-alocar = 0.
            FOR EACH ped-ent OF ped-item NO-LOCK:
                IF ped-ent.tipo-atend = 1 /* TOTAL */ THEN
                    ASSIGN de-qt-a-alocar = de-qt-a-alocar + (ped-ent.qt-pedida - ped-ent.qt-alocada - ped-ent.dec-1).
                ELSE IF ped-ent.qt-log-aloca > 0 THEN
                    ASSIGN de-qt-a-alocar = de-qt-a-alocar + ped-ent.qt-log-aloca.
                ELSE
                    ASSIGN de-qt-a-alocar = de-qt-a-alocar + (ped-ent.qt-pedida - ped-ent.qt-alocada - ped-ent.dec-1).
            END.*/
    
            ASSIGN de-qt-a-alocar = b-it-nota-fisc.qt-faturada[1].
    
            IF de-qt-a-alocar <= 0 THEN NEXT.
    
            IF ped-item.cod-sit-it >= 3 THEN NEXT. /* somente itens em aberto */ 
    
            FIND FIRST natur-oper OF ped-item NO-LOCK NO-ERROR.
            IF NOT AVAIL natur-oper OR (AVAIL natur-oper AND natur-oper.baixa-estoq = NO) THEN DO:
                RUN pi-gera-erro(INPUT 17006,                                
                                 INPUT "ERROR":U,                            
                                 INPUT "Nat. de Operaá∆o " + TRIM(ped-item.nat-operacao) + " Inv†lida ou n∆o baixa estoque.",
                                 INPUT "Verifique o cadastro de natureza de operaá∆o").
                NEXT.
            END.
          
            /* criacao de temp-table com os detalhes de alocacao */
            IF NOT CAN-FIND(FIRST tt-alocar OF ped-item) THEN DO:
                IF ped-item.per-minfat > 0 THEN DO:
                    FIND FIRST bf-ped-item EXCLUSIVE-LOCK
                         WHERE ROWID(bf-ped-item) = ROWID(ped-item) NO-ERROR.
                    ASSIGN bf-ped-item.per-minfat = 0.
                END.

                CREATE tt-alocar.
                ASSIGN tt-alocar.nome-abrev    = ped-item.nome-abrev
                       tt-alocar.nr-pedcli     = ped-item.nr-pedcli
                       tt-alocar.nr-sequencia  = ped-item.nr-sequencia
                       tt-alocar.it-codigo     = ped-item.it-codigo
                       tt-alocar.cod-estabel   = b-ped-venda.cod-estabel
                       tt-alocar.cod-refer     = ped-item.cod-refer
                       tt-alocar.cod-rota      = b-ped-venda.cod-rota 
                       tt-alocar.qt-pedida     = ped-item.qt-pedida
                       tt-alocar.qt-atendida   = ped-item.qt-atendida
                       tt-alocar.qt-saldo      = (ped-item.qt-pedida - ped-item.qt-atendida)
                       tt-alocar.qt-saldo-ori  = tt-alocar.qt-saldo
                       tt-alocar.qt-atender    = tt-alocar.qt-atender + de-qt-a-alocar.
            END.
        END.
    END. /* for each ped-item */

END PROCEDURE. /* pi-cria-tt-alocar */


PROCEDURE pi-calc-embarque :
/*------------------------------------------------------------------------------
  Purpose:     Calcula os embarques e gera as notas
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE l-dif AS LOGICAL     NO-UNDO.

    RUN initializeDBOs.

    FIND FIRST para-fat NO-LOCK NO-ERROR.

    RUN pi-verifica-qtd-emb (OUTPUT l-dif).
    /* Se estiver diferente pula para proxima nota */
    IF l-dif THEN DO:
        NEXT.
    END.

    FOR EACH tt-embarque,
        EACH embarque EXCLUSIVE-LOCK
       WHERE embarque.cdd-embarq = tt-embarque.cdd-embarq:

        /*Para liberar os registros presos*/
        RUN pi-cancel-nota.

        ASSIGN c-serie = nota-fiscal.serie.
    
        RUN pi-atualiz-ser-estab (INPUT embarque.cod-estabel).

        ASSIGN dt-emis-nota = nota-fiscal.dt-emis-nota.
    
        IF dt-emis-nota = ? THEN DO:
            RUN leaveSerie IN h-bodi317sd (INPUT  embarque.cod-estabel,
                                           INPUT  c-serie,
                                           INPUT  NO,
                                           OUTPUT c-date-aux,
                                           OUTPUT c-char-aux).
            ASSIGN dt-emis-nota  = c-date-aux
                   dt-bas-dupl = dt-emis-nota.
        END.
    
        IF dt-bas-dupl = ? THEN
            ASSIGN dt-bas-dupl = dt-emis-nota.
    
        RUN emptyRowErrors IN h-bodi317in.
        
        RUN validaNrEmbarque      IN h-bodi317va(INPUT  embarque.cdd-embarq,
                                                 OUTPUT l-proc-ok-aux).
    
        RUN devolveErrosbodi317va IN h-bodi317va(OUTPUT c-ultimo-metodo-exec,
                                                 OUTPUT TABLE RowErrors).
    
        FOR EACH RowErrors 
            WHERE RowErrors.ErrorSubType = "WARNING":U EXCLUSIVE-LOCK:
            DELETE RowErrors.
            ASSIGN l-proc-ok-aux = YES.
        END.
    
        FIND FIRST RowErrors NO-LOCK NO-ERROR.
        IF AVAIL RowErrors THEN DO:
            RUN pi-gera-erro(INPUT RowErrors.ErrorNumber,
                             INPUT RowErrors.ErrorType,                            
                             INPUT RowErrors.ErrorDescription,
                             INPUT RowErrors.ErrorHelp).

            RUN pi-cancel-nota.
    
            NEXT.
        END.
        IF NOT l-proc-ok-aux THEN NEXT.
    
        RUN setPreFaturInUse IN h-bodi162(INPUT embarque.cdd-embarq).
        RUN ftp/ft4026.w PERSISTENT SET h-ft4026.
        RUN Pi-Trava-Embarque IN h-ft4026 (INPUT embarque.cdd-embarq).
        
        RUN geraResumoEmbarque.
        IF RETURN-VALUE <> "NOK" THEN DO:
            RUN executaCalculo.
        END.
    END.
    
    RUN finalizaDBOs.

END PROCEDURE. /* pi-calc-embarque */

PROCEDURE pi-verifica-qtd-emb :
/*------------------------------------------------------------------------------
  Purpose:     Verifica Quantidade Embarcada
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAM p-dif AS LOGICAL NO-UNDO.

    DEFINE VARIABLE de-qt-aloc-origem AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE de-qt-aloc-ungcom AS DECIMAL     NO-UNDO.


    /* Quantidade Unigel Origem */
    FOR EACH embarque NO-LOCK
       WHERE embarque.cdd-embarq = nota-fiscal.cdd-embarq,
        EACH pre-fatur  OF embarque  NO-LOCK,
        EACH it-pre-fat OF pre-fatur NO-LOCK:
        ASSIGN de-qt-aloc-origem = de-qt-aloc-origem + it-pre-fat.qt-alocada.
    END.

    /* Quantidade Unigel Comercial */
    FOR EACH tt-embarque,
        EACH embarque NO-LOCK
       WHERE embarque.cdd-embarq = tt-embarque.cdd-embarq,
        EACH pre-fatur  OF embarque  NO-LOCK,
        EACH it-pre-fat OF pre-fatur NO-LOCK:
        ASSIGN de-qt-aloc-ungcom = de-qt-aloc-ungcom + it-pre-fat.qt-alocada.
    END.

    IF de-qt-aloc-origem <> de-qt-aloc-ungcom THEN
        ASSIGN p-dif = YES.
    ELSE
        ASSIGN p-dif = NO.

END PROCEDURE. /* pi-verifica-qtd-emb */



PROCEDURE pi-verifica-qtd-emb-transf :
/*------------------------------------------------------------------------------
  Purpose:     Verifica Quantidade Embarcada
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAM p-dif AS LOGICAL NO-UNDO.

    DEFINE VARIABLE de-qt-aloc-origem AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE de-qt-aloc-ungcom AS DECIMAL     NO-UNDO.


    /* Quantidade Unigel Origem */
    FOR EACH embarque NO-LOCK
       WHERE embarque.cdd-embarq = nota-fiscal.cdd-embarq,
        EACH pre-fatur  OF embarque  NO-LOCK,
        EACH it-pre-fat OF pre-fatur NO-LOCK:
        ASSIGN de-qt-aloc-origem = de-qt-aloc-origem + it-pre-fat.qt-alocada.
    END.

    /* Quantidade ja transferida */
    FOR FIRST wt-docto WHERE (wt-docto.cod-estabel  = "434" OR wt-docto.cod-estabel  = "442")  and /*solic-318*/ 
             wt-docto.cod-emitente  = 432 and
             INDEX (wt-docto.observ-nota,STRING(nota-fiscal.cdd-embarq)) > 0 NO-LOCK,
        EACH  wt-it-docto OF wt-docto NO-LOCK.
      
        ASSIGN de-qt-aloc-ungcom = de-qt-aloc-ungcom + wt-it-docto.quantidade[2].
    END.

    IF de-qt-aloc-origem <> de-qt-aloc-ungcom THEN
        ASSIGN p-dif = YES.
    ELSE
        ASSIGN p-dif = NO.

END PROCEDURE. /* pi-verifica-qtd-emb */

PROCEDURE initializeDBOs :
/*------------------------------------------------------------------------------
  Purpose:     Inicializa DBOs
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    /*--- Verifica se o DBO j† est† inicializado ---*/
    IF NOT VALID-HANDLE(h-bodi041) THEN
        RUN dibo/bodi041-2.p PERSISTENT SET h-bodi041.

    RUN openQueryStatic IN h-bodi041 (INPUT "Default":U) NO-ERROR.

    /*--- Verifica se o DBO j† est† inicializado ---*/
    IF NOT VALID-HANDLE(h-bodi176) THEN
        RUN dibo/bodi176.p PERSISTENT SET h-bodi176.
    
    IF NOT VALID-HANDLE(h-bodi162) THEN
        RUN dibo/bodi162-2.p PERSISTENT SET h-bodi162.

    IF NOT VALID-HANDLE(h-bodi317in) THEN DO:
        RUN dibo/bodi317in.p PERSISTENT SET h-bodi317in.
        RUN inicializaBOS IN h-bodi317in(OUTPUT h-bodi317pr,
                                         OUTPUT h-bodi317sd,     
                                         OUTPUT h-bodi317im1bra,
                                         OUTPUT h-bodi317va).
    END.

    IF NOT VALID-HANDLE(h-bodi317) THEN
        RUN dibo/bodi317.p PERSISTENT SET h-bodi317.


    IF NOT AVAIL param-global THEN
        FIND FIRST param-global NO-LOCK NO-ERROR.
    IF param-global.modulo-ex THEN DO:
        ASSIGN l-exportacao = YES.
        &IF "{&bf_comex206b-1}" = "YES" &THEN
            IF NOT VALID-HANDLE(h-bocx367) THEN
              RUN cxbo/bocx367.p PERSISTENT SET h-bocx367.
        &ENDIF
    END.
    
    IF NOT VALID-HANDLE(h-bo-emitente)
     OR h-bo-emitente:TYPE <> "PROCEDURE"
     OR h-bo-emitente:FILE-NAME <> "adbo/boad098.p" THEN
        RUN adbo/boad098.p PERSISTENT SET h-bo-emitente.

    RETURN "OK":U.

END PROCEDURE. /* initializeDBOs */

PROCEDURE finalizaDBOs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF VALID-HANDLE(h-bodi317in) THEN
        RUN finalizaBOS IN h-bodi317in.

    IF VALID-HANDLE(h-bodi317) THEN DO:
        DELETE PROCEDURE h-bodi317.
        ASSIGN h-bodi317 = ?.
    END.

    IF VALID-HANDLE(h-bodi162) THEN DO:
        DELETE PROCEDURE h-bodi162.
        ASSIGN h-bodi162 = ?.
    end.

    IF VALID-HANDLE(h-ft4026) THEN DO:
       DELETE PROCEDURE h-ft4026.
       ASSIGN h-ft4026 = ?.
    END.

    IF VALID-HANDLE(h-bo-emitente) THEN DO: 
        DELETE PROCEDURE h-bo-emitente.
        ASSIGN h-bo-emitente = ?.
    END.
    
    &IF "{&bf_comex206b-1}" = "YES" &THEN
        IF  VALID-HANDLE(h-bocx367) then DO:
            DELETE PROCEDURE h-bocx367.
            ASSIGN h-bocx367 = ?.
        END.
    &ENDIF
    
END PROCEDURE. /* finalizaDBOs */


PROCEDURE pi-cancel-nota :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    run setPreFaturFree                    in h-bodi162  (input  embarque.cdd-embarq).
    run eliminaRegistrosWorkTableXEmbarque in h-bodi317sd(input  embarque.cdd-embarq,
                                                          input  0,
                                                          input  9999999,
                                                          output l-proc-ok-aux).

END. /* pi-cancel-nota */

PROCEDURE pi-atualiz-ser-estab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM p-cod-estabel AS CHARACTER NO-UNDO.

    FOR FIRST ser-estab EXCLUSIVE-LOCK
        WHERE ser-estab.serie       = nota-fiscal.serie
          AND ser-estab.cod-estabel = p-cod-estabel:

        IF nota-fiscal.dt-emis-nota > ser-estab.dt-ult-fat THEN
            ASSIGN ser-estab.dt-ult-fat  = nota-fiscal.dt-emis-nota
                   ser-estab.dt-prox-fat = nota-fiscal.dt-emis-nota + ser-estab.nr-dias-abe.
    END.
    

END PROCEDURE. /* pi-atualiz-ser-estab */

PROCEDURE geraResumoEmbarque :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   RUN inicializaAcompanhamento IN h-bodi317sd.

   RUN setConstraintEmbarque IN h-bodi176 (INPUT embarque.cdd-embarq).
   RUN openQueryEmbarque     IN h-bodi176.
   RUN getBatchRecords       IN h-bodi176 (?, YES, ?, OUTPUT i-linhas, OUTPUT TABLE tt-res-cli).

   FOR EACH tt-res-cli
      WHERE tt-res-cli.situacao = 3, /* A Faturar */
      FIRST pre-fatur no-lock
      WHERE pre-fatur.cdd-embarq = tt-res-cli.cdd-embarq
        AND pre-fatur.nr-resumo  = tt-res-cli.nr-resumo:

        FIND FIRST natur-oper
             WHERE natur-oper.nat-operacao = pre-fatur.nat-operacao NO-LOCK NO-ERROR.
        IF NOT AVAIL natur-oper THEN DO:
            RUN pi-gera-erro(INPUT 17006,
                             INPUT "ERROR":U,
                             INPUT "Natureza de Operaá∆o n∆o encontrada",
                             INPUT "").
            RUN pi-cancel-nota.
            LEAVE.
        END.
   
       RUN emptyRowErrors                   IN h-bodi317in.
    
       RUN setaValidaExp                    IN h-bodi317va(INPUT YES).
       RUN geraResumosDoEmbarqueParaCalculo IN h-bodi317sd(INPUT  c-seg-usuario,
                                                           INPUT  pre-fatur.cdd-embarq,
                                                           INPUT  tt-res-cli.nr-resumo,
                                                           INPUT  tt-res-cli.nr-resumo,
                                                           INPUT  c-serie,
                                                           INPUT  dt-emis-nota,
                                                           INPUT  dt-bas-dupl,
                                                           INPUT  ?,
                                                           OUTPUT l-proc-ok-aux).
       ASSIGN tt-res-cli.data-1 = dt-bas-dupl.
    
       run devolveErrosbodi317sd            IN h-bodi317sd(OUTPUT c-ultimo-metodo-exec,
                                                           OUTPUT TABLE RowErrors2).
    
       FOR EACH RowErrors2:
           CREATE RowErrors.
           BUFFER-COPY RowErrors2 TO RowErrors.
           ASSIGN RowErrors.ErrorDescription = RowErrors.ErrorDescription +
                  IF RowErrors2.ErrorSubType <> "WARNING":U 
                  THEN " (Resumo " + STRING(tt-res-cli.nr-resumo) + ")"
                  ELSE "".
       END.
    
       IF l-proc-ok-aux then
           RUN pi-marca-resumo.
    
       RUN retornaSeqWtDoctoDoEmbarque IN h-bodi317pr(INPUT  tt-res-cli.cdd-embarq,
                                                      INPUT  tt-res-cli.nr-resumo,
                                                      OUTPUT i-seq-wt-docto, 
                                                      OUTPUT l-proc-ok-aux).
    
       IF  i-seq-wt-docto <> ? THEN DO:
           RUN setConstraintDefault IN h-bodi317(INPUT c-seg-usuario).
           RUN openQueryStatic      IN h-bodi317(INPUT "Default":U).
           RUN goToKey              IN h-bodi317(INPUT i-seq-wt-docto).
           RUN getIntField          IN h-bodi317(INPUT "cod-cond-pag":U,     OUTPUT tt-res-cli.cod-cond-pag).
           RUN getIntField          IN h-bodi317(INPUT "mo-codigo":U,        OUTPUT tt-res-cli.mo-codigo   ).
           RUN getCharField         IN h-bodi317(INPUT "no-ab-reppri":U,     OUTPUT tt-res-cli.no-ab-reppri).
           RUN getCharField         IN h-bodi317(INPUT "nat-operacao":U,     OUTPUT tt-res-cli.nat-operacao).
           RUN getDecField          IN h-bodi317(INPUT "peso-liq-tot-inf":U, OUTPUT tt-res-cli.peso-liq-tot).
           RUN getDecField          IN h-bodi317(INPUT "peso-bru-tot-inf":U, OUTPUT tt-res-cli.peso-bru-tot).

           /* Busca Peso Liquido */
           /*FIND FIRST pesagem NO-LOCK
                WHERE pesagem.cdd-embarq = i-nr-emb-orig NO-ERROR.
           IF AVAIL pesagem THEN DO:
               RUN getRecord IN h-bodi317 (OUTPUT TABLE tt-wt-docto).
               FOR FIRST tt-wt-docto
                   WHERE tt-wt-docto.seq-wt-docto = i-seq-wt-docto:
                   ASSIGN tt-wt-docto.peso-bru-tot-inf = pesagem.peso-liquido
                          tt-wt-docto.peso-liq-tot-inf = pesagem.peso-liquido.
               END.
               RUN setRecord    IN h-bodi317 (INPUT TABLE tt-wt-docto). 
               RUN updateRecord IN h-bodi317.
               RUN getRowErrors IN h-bodi317 (OUTPUT TABLE RowErrors APPEND).
           END.*/

           RUN pi-observacao.
       END.
   END.

   FOR EACH RowErrors
      WHERE RowErrors.ErrorSubType = "WARNING":U
      BREAK BY RowErrors.ErrorNum:
       /*IF NOT FIRST-OF(RowErrors.ErrorNum) THEN*/
           DELETE RowErrors.
   END.

   RUN finalizaAcompanhamento IN h-bodi317sd.
   
   FIND FIRST RowErrors NO-LOCK NO-ERROR.
   IF  AVAIL RowErrors THEN DO:
       RUN pi-gera-erro(INPUT RowErrors.ErrorNumber,
                        INPUT RowErrors.ErrorType,
                        INPUT RowErrors.ErrorDescription,
                        INPUT RowErrors.ErrorHelp).

       RUN pi-cancel-nota.
   END.

END PROCEDURE. /* geraResumoEmbarque */


PROCEDURE executaCalculo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*IF TRANS AND NOT l-transacao THEN DO:

        RUN utp/ut-msgs.p(INPUT "msg",
                          INPUT 27976,
                          INPUT "").

        RUN pi-gera-erro(INPUT 27976,
                         INPUT "ERROR",
                         INPUT RETURN-VALUE,
                         INPUT "").

        RETURN NO-APPLY.
    END.*/

    RUN pi-alt-aloca.
    IF RETURN-VALUE = "NOK":U THEN RETURN.

    FOR EACH tt-notas-ger-aux:
        DELETE tt-notas-ger-aux.
    END.
    ASSIGN rw-nota-fiscal = ?.

    IF l-exportacao AND VALID-HANDLE(h-bo-emitente) THEN DO:
        FOR FIRST b-tt-res-cli
            WHERE b-tt-res-cli.situacao = 3 NO-LOCK:
        END. 
        IF AVAIL tt-res-cli THEN DO:
            RUN findNome    IN h-bo-emitente (INPUT b-tt-res-cli.nome-abrev,
                                              OUTPUT c-resultado).
            RUN getIntField IN h-bo-emitente (INPUT 'natureza',
                                              OUTPUT i-natureza).
    
            &IF "{&bf_comex206b-1}" = "YES" &THEN
                IF  i-natureza = 3 OR
                    i-natureza = 4 THEN DO:
            
                    RUN piVerificaDespesas IN h-bocx367 (INPUT  i-seq-wt-docto,
                                                         OUTPUT l-proc-ok-aux).
                    IF  NOT l-proc-ok-aux THEN DO:
                        RUN utp/ut-msgs.p (INPUT "msg":U,
                                           INPUT "31390",
                                           INPUT "Despesas de Exportaá∆o").

                        RUN pi-gera-erro(INPUT 31390,
                                         INPUT "ERROR",
                                         INPUT RETURN-VALUE,
                                         INPUT "").
                        
                        IF  RETURN-VALUE = "NO" THEN
                            RETURN NO-APPLY.
                    END.
         
                    RUN piComparaDespesas IN h-bocx367 (INPUT i-seq-wt-docto,
                                                        OUTPUT l-proc-ok-aux,
                                                        OUTPUT TABLE tt-param-nota-proc).
                    FIND FIRST tt-param-nota-proc NO-ERROR.
                  
                    IF AVAIL tt-param-nota-proc THEN DO:
                        IF  NOT l-proc-ok-aux THEN DO:
                            RUN utp/ut-msgs.p (INPUT "msg":U,
                                               INPUT "32686",
                                               INPUT tt-param-nota-proc.cod-emitente-desp    + "~~" +   
                                                     tt-param-nota-proc.cod-cond-pagto       + "~~" +  
                                                     tt-param-nota-proc.cod-itiner           + "~~" +  
                                                     tt-param-nota-proc.cod-pto-contr        + "~~" +  
                                                     tt-param-nota-proc.mo-codigo            + "~~" +  
                                                     tt-param-nota-proc.dat-base-cobr-despes + "~~" +
                                                     tt-param-nota-proc.log-forma-preco      + "~~" +
                                                     tt-param-nota-proc.log-despes-integr-ap + "~~" +
                                                     tt-param-nota-proc.cdn-classif-despes). 

                            RUN pi-gera-erro(INPUT 32686,
                                             INPUT "ERROR",
                                             INPUT RETURN-VALUE,
                                             INPUT "").

                            RETURN NO-APPLY.
                        END.
                    END.
                END.
            &ENDIF
        END.
    END.

    RUN validacaoPermissoesBOs.
    IF RETURN-VALUE = "NOK":U THEN RETURN.
    ELSE RUN calculaEmbarque.

    IF RETURN-VALUE = "NOK":U THEN
        RETURN NO-APPLY.
    ELSE DO: /* Mostrar as notas geradas */
        FOR EACH tt-notas-ger-aux NO-LOCK,
            FIRST b2-nota-fiscal 
            WHERE ROWID(b2-nota-fiscal) = tt-notas-ger-aux.rw-nota-fiscal EXCLUSIVE-LOCK:
            ASSIGN rw-nota-fiscal = ROWID(b2-nota-fiscal).
            ASSIGN c-lst-nota = c-lst-nota + TRIM(b2-nota-fiscal.cod-estabel) + "-" +
                                TRIM(b2-nota-fiscal.serie) + "_" + TRIM(b2-nota-fiscal.nr-nota-fis) + ",".

            ASSIGN b2-nota-fiscal.cond-redespa = "Nota Original Relacionada " + TRIM(c-nota-orig).
        END.

        /* N«O ê NECESSµRIA VISUALIZAR AS NOTAS GERADAS PARA OUTROS ESTABELECIMENTOS*/
        IF NOT param-global.modulo-tf OR 
           NOT l-cidade-cif           OR
           NOT l-log-mostra-simul-embarq THEN DO:

            PAUSE 2.

            FOR FIRST tt-notas-ger-aux NO-LOCK:
                FIND LAST b-tt-notas-ger-aux NO-ERROR.
                FOR FIRST b2-nota-fiscal 
                    WHERE ROWID(b2-nota-fiscal) = tt-notas-ger-aux.rw-nota-fiscal NO-LOCK:
                END.
                BELL.

                IF tt-notas-ger-aux.nr-nota = b-tt-notas-ger-aux.nr-nota THEN DO:
                    ASSIGN c-mensagem = string(tt-notas-ger-aux.nr-nota) + "~~" +
                                        string(b2-nota-fiscal.cod-estabel) + "~~" +
                                        string(b2-nota-fiscal.serie).

                    RUN pi-gera-erro(INPUT  15263,
                                     INPUT "WARNING",
                                     INPUT c-mensagem,
                                     INPUT "").
                END.
                ELSE DO:

                    ASSIGN c-mensagem = string(tt-notas-ger-aux.nr-nota)   + "~~" +
                                        string(b-tt-notas-ger-aux.nr-nota) + "~~" +
                                        string(b2-nota-fiscal.cod-estabel) + "~~" +
                                        string(b2-nota-fiscal.serie).

                    RUN pi-gera-erro(INPUT 15264,
                                     INPUT "WARNING",
                                     INPUT c-mensagem,
                                     INPUT "").
                END.
            END.
        END.
    END.

    FOR FIRST b-tt-res-cli
        WHERE b-tt-res-cli.situacao = 3 NO-LOCK:
    END. 
    IF AVAIL b-tt-res-cli THEN DO:

        FOR EACH tt-res-cli
            WHERE tt-res-cli.situacao = 3:

            RUN retornaSeqWtDoctoDoEmbarque IN h-bodi317pr(INPUT  tt-res-cli.cdd-embarq,
                                                           INPUT  tt-res-cli.nr-resumo,
                                                           OUTPUT i-seq-wt-docto, 
                                                           OUTPUT l-proc-ok-aux).

            RUN goToKey IN h-bodi317 (INPUT i-seq-wt-docto).
            IF RETURN-VALUE = "OK":U THEN
                RUN getDateField IN h-bodi317 (INPUT  "dt-base-dup":U, OUTPUT tt-res-cli.data-1).
        END.
    END.

END PROCEDURE. /* executaCalculo */


PROCEDURE validacaoPermissoesBOs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF  NOT CAN-FIND(FIRST ttPrograma) THEN DO:
        CREATE ttPrograma. ASSIGN ttPrograma.cPrograma = "bodi317".
        CREATE ttPrograma. ASSIGN ttPrograma.cPrograma = "bodi317ef".
        CREATE ttPrograma. ASSIGN ttPrograma.cPrograma = "bodi317ef2".
        CREATE ttPrograma. ASSIGN ttPrograma.cPrograma = "bodi317im1br".
        CREATE ttPrograma. ASSIGN ttPrograma.cPrograma = "bodi317im2bra".
        CREATE ttPrograma. ASSIGN ttPrograma.cPrograma = "bodi317in".
        CREATE ttPrograma. ASSIGN ttPrograma.cPrograma = "bodi317int".
        CREATE ttPrograma. ASSIGN ttPrograma.cPrograma = "bodi317na".
        CREATE ttPrograma. ASSIGN ttPrograma.cPrograma = "bodi317pd".
        CREATE ttPrograma. ASSIGN ttPrograma.cPrograma = "bodi317pr".
        CREATE ttPrograma. ASSIGN ttPrograma.cPrograma = "bodi317q1".
        CREATE ttPrograma. ASSIGN ttPrograma.cPrograma = "bodi317sd".
        CREATE ttPrograma. ASSIGN ttPrograma.cPrograma = "bodi317usa".
        CREATE ttPrograma. ASSIGN ttPrograma.cPrograma = "bodi317va".
    
        FOR EACH ttPrograma:
            RUN men/men901za.p (INPUT ttPrograma.cPrograma).
            IF  RETURN-VALUE = "2012" THEN DO:
                /* Usuario sem permissao para acessar o programa. */
                RUN utp/ut-msgs.p ("msg",2858,ttPrograma.cPrograma). 

                RUN pi-gera-erro(INPUT 2858,
                                 INPUT "ERROR":U,
                                 INPUT RETURN-VALUE,
                                 INPUT "").

                RETURN "NOK":U.
            END.
        END.
    END.
    RETURN "OK":U.

END PROCEDURE. /* validacaoPermissoesBOs */


PROCEDURE calculaEmbarque :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR h-bodi317ef            as HANDLE  NO-UNDO.
    DEF VAR h-tf0516               AS HANDLE  NO-UNDO.
    DEF VAR h-tfapi001             AS HANDLE  NO-UNDO.
    DEF VAR l-possui-impressao-aut as LOGICAL NO-UNDO.
    DEF VAR l-warning              as LOGICAL NO-UNDO.
    DEF VAR l-log-frete-vda        AS LOGICAL NO-UNDO.

    DEF BUFFER b-ser-estab FOR ser-estab.

    FOR EACH RowErrors:
        DELETE RowErrors.
    END.

    RUN emptyRowErrors IN h-bodi317in.

    /* Se for um c†lculo, efetua validaá∆o do retorno do wms. Se for simulaá∆o, permite continuar. */
    IF  l-eq-log-integr-wms THEN DO:
        FOR EACH tt-res-cli 
           WHERE tt-res-cli.situacao = 3
             AND tt-res-cli.log-2    = YES:

            RUN validateEmbarqueWMS IN h-bodi317va (INPUT tt-res-cli.cdd-embarq,
                                                    INPUT tt-res-cli.nr-resumo,
                                                    OUTPUT l-proc-ok-aux).

            RUN devolveErrosbodi317va IN h-bodi317va(OUTPUT c-ultimo-metodo-exec,
                                                     OUTPUT TABLE RowErrors).

            FOR EACH RowErrors 
                WHERE RowErrors.ErrorSubType = "WARNING":U EXCLUSIVE-LOCK:
                DELETE RowErrors.
                ASSIGN l-proc-ok-aux = YES.
            END.

            FIND FIRST RowErrors NO-LOCK NO-ERROR.
            IF  AVAIL RowErrors THEN DO:
                
                RUN pi-gera-erro(INPUT RowErrors.ErrorNumber,
                                 INPUT RowErrors.ErrorType,
                                 INPUT RowErrors.ErrorDescription,
                                 INPUT RowErrors.ErrorHelp).

                RUN pi-cancel-nota.
            END.
            IF NOT l-proc-ok-aux THEN
                RETURN "NOK".
        END.
    END.
    
    /*****/
    RUN validaCabecalhoDoEmbarque IN h-bodi317va(INPUT  embarque.cdd-embarq,
                                                 INPUT  embarque.cod-estabel,
                                                 INPUT  c-serie,
                                                 INPUT  dt-emis-nota,
                                                 INPUT  dt-bas-dupl,
                                                 INPUT  ?,
                                                 OUTPUT l-proc-ok-aux).

    RUN devolveErrosbodi317va     IN h-bodi317va(OUTPUT c-ultimo-metodo-exec,
                                                 OUTPUT TABLE RowErrors).

    FOR EACH RowErrors 
        WHERE RowErrors.ErrorSubType = "WARNING":U EXCLUSIVE-LOCK:
        DELETE RowErrors.
        ASSIGN l-proc-ok-aux = YES.
    END.

    FIND FIRST RowErrors NO-LOCK NO-ERROR.
    IF AVAIL RowErrors THEN DO:
        RUN pi-gera-erro(INPUT RowErrors.ErrorNumber,
                         INPUT RowErrors.ErrorType,
                         INPUT RowErrors.ErrorDescription,
                         INPUT RowErrors.ErrorHelp).

        RUN pi-cancel-nota.
    END.

    IF NOT l-proc-ok-aux THEN
        RETURN "NOK".

    RUN liberaWtDoctoDoEbmarque IN h-bodi317pr (INPUT  embarque.cdd-embarq,
                                                OUTPUT l-proc-ok-aux).

    RUN liberaWtDoctoDoEbmarque IN h-bodi317 (INPUT  embarque.cdd-embarq,
                                              OUTPUT l-proc-ok-aux).

    RUN liberaWtDoctoDoEbmarque IN h-bodi317va (INPUT  embarque.cdd-embarq,
                                                OUTPUT l-proc-ok-aux).
    RUN liberaPedVenda  IN h-bodi317va.
    RUN liberaPedVenda  IN h-bodi317sd.

    {utp/ut-run.i ftp/ft4001a.p "input  yes, 
                                 INPUT  c-serie,
                                 INPUT  dt-emis-nota,
                                 INPUT  dt-bas-dupl,
                                 INPUT  ?,
                                 INPUT  TABLE tt-res-cli,
                                 INPUT  ?,
                                 OUTPUT TABLE RowErrors,
                                 OUTPUT TABLE tt-notas-ger-aux,
                                 OUTPUT l-proc-ok-aux"
                                 "YES"}

    /*RUN ftp/ft4001a.p (input  yes, 
                       INPUT  c-serie,
                       INPUT  dt-emis-nota,
                       INPUT  dt-bas-dupl,
                       INPUT  ?,
                       INPUT  TABLE tt-res-cli,
                       INPUT  ?,
                       OUTPUT TABLE RowErrors,
                       OUTPUT TABLE tt-notas-ger-aux,
                       OUTPUT l-proc-ok-aux).*/

    /* integraá∆o TMS */
    /*IF NOT CAN-FIND(FIRST RowErrors) THEN DO:
        RUN upc/appc-nota-fiscal-g.p (INPUT TABLE tt-notas-ger-aux, INPUT "END_FT4001A", OUTPUT TABLE RowErrors).

        FOR EACH RowErrors
           WHERE RowErrors.ErrorNumber = 17006:
            ASSIGN RowErrors.ErrorDescription = "TMS - " + RowErrors.ErrorDescription.
        END.

        FOR EACH RowErrors
           WHERE RowErrors.ErrorNumber <> 17006:

            IF RowErrors.ErrorNumber = 34078 THEN
                ASSIGN RowErrors.ErrorNumber      = 17006
                       RowErrors.ErrorDescription = "TMS - Erro ao calcular frete da nota".
            ELSE
                ASSIGN RowErrors.ErrorNumber      = 17006
                       RowErrors.ErrorDescription = "TMS - " + RowErrors.ErrorDescription.
        END.
    END.*/

    FOR EACH RowErrors NO-LOCK:
        RUN pi-gera-erro(INPUT RowErrors.ErrorNumber,
                         INPUT RowErrors.ErrorType,
                         INPUT RowErrors.ErrorDescription,
                         INPUT RowErrors.ErrorHelp).

        RUN pi-cancel-nota.
    END.

    IF  NOT CAN-FIND(FIRST tt-notas-ger-aux) AND NOT l-proc-ok-aux THEN 
        RETURN "NOK".

    /*** Enviar nf-e - Se n∆o houver erro no c†lculo ***/
    
    IF  NOT VALID-HANDLE(h-bodi317ef) THEN
        RUN dibo/bodi317ef.p PERSISTENT SET h-bodi317ef.

    for each tt-notas-ger-aux no-lock,
        each b5-nota-fiscal
       where rowid(b5-nota-fiscal) = tt-notas-ger-aux.rw-nota-fiscal NO-LOCK
          by b5-nota-fiscal.nr-nota-fis:

        /* Atualizaá‰es Autom†ticas */
        if  not avail natur-oper
                  or  natur-oper.nat-operacao <> b5-nota-fiscal.nat-operacao then
            for first natur-oper no-lock
                where natur-oper.nat-operacao = b5-nota-fiscal.nat-operacao: end.
    
        if  not avail emitente
                  or  emitente.cod-emitente <> b5-nota-fiscal.cod-emitente then
            for first emitente 
                where emitente.cod-emitente = b5-nota-fiscal.cod-emitente no-lock: end.
            
        if  not avail estabelec
                  or  estabelec.cod-estabel <> b5-nota-fiscal.cod-estabel then
            for first estabelec NO-LOCK
                where estabelec.cod-estabel = b5-nota-fiscal.cod-estabel: end.

        /* Verifica se deve ser enviada NF-e */
        IF CAN-FIND(funcao WHERE funcao.cd-funcao = "spp-nfe":U AND funcao.ativo) AND
           
          CAN-FIND(FIRST b-ser-estab WHERE
                         b-ser-estab.cod-estabel = b5-nota-fiscal.cod-estabel AND
                         b-ser-estab.serie       = b5-nota-fiscal.serie       AND
                         b-ser-estab.log-nf-eletro) THEN DO:

               IF  VALID-HANDLE(h-bodi317ef) THEN DO:
                   RUN pi-setaNota IN h-bodi317ef (INPUT ROWID(b5-nota-fiscal)).
                   RUN pi-nfe      IN h-bodi317ef.
               END.

        END.

    END.

    IF  VALID-HANDLE(h-bodi317ef) then do:
        DELETE PROCEDURE h-bodi317ef.
        ASSIGN h-bodi317ef = ?.
    END.

    /*** Fim ***/

    FIND FIRST param-global NO-LOCK NO-ERROR.
    &IF "{&mguni_version}" < "2.07" &THEN
        IF  CONNECTED("mgscm") AND param-global.modulo-tf THEN DO:
            IF NOT VALID-HANDLE(h-bosc014) THEN DO:

                RUN scbo/bosc014.p PERSISTENT SET h-bosc014.
                RUN openQueryStatic IN h-bosc014 (INPUT 'Default':U).
    
                RUN goToKey in h-bosc014 (INPUT embarque.cod-estabel).
                IF RETURN-VALUE = 'OK' THEN DO:
    
                    RUN getLogField IN h-bosc014 (INPUT 'log-frete-vda':U, OUTPUT l-log-frete-vda).
    
                    &IF '{&BF_DIS_VERSAO_EMS}' = '2.04' &THEN
                        RUN getDecField IN h-bosc014 (INPUT 'dec-1':U, OUTPUT d-dec-1).
                        RUN getIntField IN h-bosc014 (INPUT 'int-2':U, OUTPUT i-int-2).
                        IF i-int-2 = 1 THEN
                            ASSIGN l-log-executa-simul = YES.
                        ELSE
                            ASSIGN l-log-executa-simul = NO.
                        IF d-dec-1 = 1 THEN
                            ASSIGN l-log-mostra-simul-embarq = YES.
                        ELSE
                            ASSIGN l-log-mostra-simul-embarq = NO.
    
                    &ELSE
                        &IF '{&BF_DIS_VERSAO_EMS}' >= '2.05' &THEN
                            RUN getLogField IN h-bosc014 (INPUT 'log-executa-simul':U, 
                                                          OUTPUT l-log-executa-simul).
                            RUN getLogField IN h-bosc014 (INPUT 'log-mostra-simul-embarq':U, 
                                                          OUTPUT l-log-mostra-simul-embarq).
                        &ENDIF
                    &ENDIF
                END.
            END.
        END.
    &ENDIF

    IF VALID-HANDLE (h-bosc014) THEN DO:
        DELETE PROCEDURE h-bosc014.
        ASSIGN h-bosc014 = ?.
    END.
    
    /*&IF "{&mguni_version}" < "2.07" &THEN
        IF  CONNECTED("mgscm") AND param-global.modulo-tf AND l-log-frete-vda THEN DO:
            IF l-log-executa-simul THEN DO:
                RUN tfp/tfapi001.p PERSISTENT SET h-tfapi001.

                RUN VerificaCidadeCif IN h-tfapi001 (INPUT TABLE tt-notas-ger-aux,
                                                     OUTPUT l-cidade-cif).
                DELETE PROCEDURE h-tfapi001.
                ASSIGN h-tfapi001 = ?.
    
                IF l-cidade-cif THEN DO:
                    IF l-log-mostra-simul-embarq THEN
                        RUN tfp/tf0516.w (INPUT TABLE tt-notas-ger-aux,
                                          INPUT embarque.cod-estabel).
                    ELSE DO:
                       RUN tfp/tf0516.w PERSISTENT SET h-tf0516 (INPUT TABLE tt-notas-ger-aux,
                                                                 INPUT embarque.cod-estabel).
                       RUN ExecutaSimulacaoSemMostrar IN h-tf0516.
                       RUN AtualizaTransp IN h-tf0516.
    
                       DELETE PROCEDURE h-tf0516.
                       ASSIGN h-tf0516 = ?.
                    END.
                END.
            END.
        END.
    &ENDIF */

    RUN dibo/bodi317ef.p PERSISTENT SET h-bodi317ef.
    RUN enviaTtNotasGeradas IN h-bodi317ef(INPUT  TABLE tt-notas-ger-aux,
                                           OUTPUT l-proc-ok-aux).
    FOR EACH tt-notas-ger-aux NO-LOCK,
       FIRST b2-nota-fiscal EXCLUSIVE-LOCK
       WHERE ROWID(b2-nota-fiscal) = tt-notas-ger-aux.rw-nota-fiscal,
        EACH b-it-nota-fisc OF nota-fiscal EXCLUSIVE-LOCK:
        /*ASSIGN b2-nota-fiscal.ind-sit-nota = 2
               b-it-nota-fisc.ind-sit-nota = 2.*/

        RUN pi-contas-a-receber-automatico.

        /* Atualiza OT */
        /*FOR FIRST b-embarque
            WHERE b-embarque.cdd-embarq = nota-fiscal.cdd-embarq NO-LOCK,
             LAST ordem-transp
            WHERE ordem-transp.origem    = "Faturamento"
              AND ordem-transp.cod-chave = b-embarque.cdd-embarq EXCLUSIVE-LOCK:

            ASSIGN ordem-transp.cod-estabel = embarque.cod-estabel
                   ordem-transp.cod-chave   = embarque.cdd-embarq.
        END.*/

    END.

    /*
    /* NF-e nao deve imprimir a nota fiscal */
    /*IF NOT CAN-FIND(FIRST funcao 
                    WHERE funcao.cd-funcao = "spp-nfe":U
                      AND funcao.ativo     = YES)  OR
       NOT CAN-FIND (first ser-estab 
                     WHERE ser-estab.cod-estabel = tt-wt-docto.cod-estabel
                       AND ser-estab.serie       = tt-wt-docto.serie
                       AND &IF "{&bf_dis_versao_ems}":U >= "2.07":U &THEN
                               ser-estab.log-nf-eletro
                           &ElSE
                              SUBSTRING(ser-estab.char-1,1,3) = "yes":U
                           &ENDIF) THEN DO:
                           
        /* Se houver impressao automatica imprime as notas fiscais */
        RUN possuiImpressaoAutomatica IN h-bodi317ef (OUTPUT l-possui-impressao-aut). 
        IF l-possui-impressao-aut THEN DO:*/
            /* Temp-table auxiliar para impress∆o autom†tica, para que n∆o haja nescessidade de se 
               criar um novo programa de impress∆o autom†tica. Ser† utilizado o programa antigo */
            FOR EACH tt-notas-ger-aux-impressao:
                DELETE tt-notas-ger-aux-impressao.
            END.
    
            FOR EACH tt-notas-ger-aux:
                CREATE tt-notas-ger-aux-impressao.
                ASSIGN tt-notas-ger-aux-impressao.rw-nota-fiscal = tt-notas-ger-aux.rw-nota-fiscal
                       tt-notas-ger-aux-impressao.nr-nota        = tt-notas-ger-aux.nr-nota.
            END.
            IF i-pais-impto-usuario = 1 THEN
                RUN ftp/ft2019.w(INPUT TABLE tt-notas-ger-aux-impressao).
            ELSE
                RUN ftp/ft2018.w(INPUT TABLE tt-notas-ger-aux-impressao).
        END.
    END.*/

    RETURN "OK".

END PROCEDURE. /* calculaEmbarque */

PROCEDURE pi-marca-resumo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF  NOT AVAIL para-fat THEN
        FIND FIRST para-fat NO-LOCK NO-ERROR.

    &IF "{&bf_dis_versao_ems}":U >= "2.04":U &THEN
        &IF "{&bf_dis_versao_ems}":U >= "2.05":U &THEN
            ASSIGN l-eq-log-integr-wms = para-fat.log-integra-wms.
        &ELSE
            ASSIGN l-eq-log-integr-wms = (SUBSTRING(para-fat.char-1,21,1) = "1":U).
        &ENDIF
        
        ASSIGN l-eq-log-aloca-ord-produc = (SUBSTRING(para-fat.char-1,97,1) = "1":U).
    &ENDIF
    
    IF l-eq-log-aloca-ord-produc OR l-eq-log-integr-wms THEN DO:
        
        RUN emptyRowErrors IN h-bodi317va.
                            
        IF l-eq-log-aloca-ord-produc THEN
            RUN retornaResumoPossuiOrdem IN h-bodi317va(INPUT  tt-res-cli.cdd-embarq,
                                                        INPUT  tt-res-cli.nr-resumo,
                                                        INPUT  tt-res-cli.nome-abrev,
                                                        OUTPUT l-possui-ordens,
                                                        OUTPUT l-proc-ok-aux).

        RUN devolveErrosbodi317va IN h-bodi317va(OUTPUT c-ultimo-metodo-exec,
                                                 OUTPUT TABLE RowErrors2).

        FOR EACH RowErrors 
            WHERE RowErrors.ErrorSubType = "WARNING":U EXCLUSIVE-LOCK:
            DELETE RowErrors.
            ASSIGN l-proc-ok-aux = YES.
        END.

        FOR EACH RowErrors2:
            CREATE RowErrors.
            BUFFER-COPY RowErrors2 TO RowErrors.
            ASSIGN RowErrors.ErrorDescription = RowErrors.ErrorDescription +
                   IF RowErrors2.ErrorSubType <> "WARNING":U 
                   THEN " (Resumo " + STRING(tt-res-cli.nr-resumo) + ")"
                   ELSE "".

            RUN pi-cancel-nota.
        END.

        IF NOT l-proc-ok-aux THEN
            RETURN NO-APPLY.
        ELSE
            ASSIGN tt-res-cli.log-2 = YES.
    END.
    ELSE
        ASSIGN tt-res-cli.log-2 = YES.

END PROCEDURE. /* pi-marca-resumo */

PROCEDURE pi-recebimento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE tt-erro.

    FOR FIRST b2-nota-fiscal NO-LOCK
        WHERE ROWID(b2-nota-fiscal) = rw-nota-fiscal:
        
        RUN esp/ifreapi190.p (INPUT ROWID(b2-nota-fiscal), OUTPUT r-docum-est, OUTPUT TABLE tt-erro).

        IF RETURN-VALUE <> "OK" THEN DO:
            RUN pi-gera-erro(INPUT 17006,
                             INPUT "ERROR":U,
                             INPUT "Ocorreram erros de integraá∆o no recebimento~~" + RETURN-VALUE,
                             INPUT "").
        END.

    END.

    /* Elimina erro padr∆o */
    FOR EACH tt-erro
       WHERE tt-erro.cd-erro = 13
          OR tt-erro.cd-erro = 6506:
         /* AND tt-erro.mensagem BEGINS "Estabelecimento": */
        DELETE tt-erro.
    END.

    FOR EACH tt-erro NO-LOCK:
        RUN pi-gera-erro(INPUT tt-erro.cd-erro,
                         INPUT "ERROR":U,
                         INPUT tt-erro.mensagem,
                         INPUT "").
    END.

    IF CAN-FIND(FIRST tt-erro) THEN
        RETURN "NOK".

END PROCEDURE. /* pi-recebimento */

PROCEDURE pi-gera-erro :
/*------------------------------------------------------------------------------
  Purpose:     Gera erro
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM p-nr-erro   AS INTEGER   NO-UNDO.
    DEF INPUT PARAM p-tip-erro  AS CHARACTER NO-UNDO.
    DEF INPUT PARAM p-desc-erro AS CHARACTER NO-UNDO.
    DEF INPUT PARAM p-desc-help AS CHARACTER NO-UNDO.

    /* Advertencia Empresa */
    IF p-nr-erro = 25853 THEN RETURN.

    /* Imprimir mensagem de erro porque o produto n∆o est† preparado */

    IF p-nr-erro = 17006 THEN
        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT p-nr-erro,
                           INPUT p-desc-erro + "~~" + p-desc-help).
    ELSE
        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT p-nr-erro,
                           INPUT p-desc-erro).

    IF p-tip-erro = "ERROR" THEN
        ASSIGN l-possui-erro = YES.

END PROCEDURE.

PROCEDURE pi-contas-a-receber-automatico:
/*------------------------------------------------------------------------------
  Purpose:     Atualiza contas a Receber
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF  NOT AVAIL para-fat THEN
        FIND FIRST para-fat NO-LOCK NO-ERROR.

    RUN pi-trata-nfe.
    IF  RETURN-VALUE = "NOK":U THEN
        RETURN "OK":U. /* sair da procedure, e n∆o continuar a atualizaá∆o no ftapi001 */

    RUN btb/btb009za.p PERSISTENT SET h-btb009za.

    if can-find(funcao where funcao.cd-funcao = "adm-acr-ems-5.00" 
                         and funcao.ativo     = yes
                         and funcao.log-1     = yes) then 
        assign l-ems5 = yes.

    if l-ems5 then do:
        assign l-conecta = yes.
        run pi-conecta-ems-5 (input  l-conecta,
                              input  i-empresa,
                              output l-erro).  /* EMS5 */
    end.

    EMPTY TEMP-TABLE tt-nota-ftapi.


    create tt-nota-ftapi.
    assign tt-nota-ftapi.r-nota-fiscal = rowid(b2-nota-fiscal)
           tt-nota-ftapi.atualizar     = yes
          
          /* o campo (para.fat-char-1,16,5) representa as informaá‰es do endereáo de cobranáa do t°tulo,
             a geraá∆o dos documentos assumir† o que estiver informado nos parÉmetros de faturamento */
           c-gera-titulo               = if  substr(para-fat.char-1,16,5) = "2" 
                                         then "2"  /* gera t°tulo p/ endereáo de entrega */
                                         else "1". /* gera t°tulo p/ endereáo de cobranáa */


    run ftp/ftapi001.p (input  3,                              /* vers∆o de integraá∆o */
                        input  i-pais-impto-usuario,           /* pa°s do usu†rio */
                        input  0,                              /* portador */   
                        input  c-gera-titulo,
                        input  "spool/" +                      /* nome do arquivo de exportaá∆o */
                               trim(b2-nota-fiscal.cod-estabel) + 
                               trim(b2-nota-fiscal.serie) +
                               trim(b2-nota-fiscal.nr-nota-fis) +
                               ".d",
                        input  table tt-nota-ftapi,
                        output table tt-retorno-nota-fiscal,  
                        output table tt-total-refer).

    FOR EACH tt-retorno-nota-fiscal
       WHERE tt-retorno-nota-fiscal.cod-erro <> "9999":
        RUN pi-gera-erro(INPUT 17006, /* tt-retorno-nota-fiscal.cod-erro, */
                         INPUT "WARNING":U,                            
                         INPUT tt-retorno-nota-fiscal.desc-erro,
                         INPUT "").
    END.

    IF l-ems5 then do:
        assign l-conecta = no.
        run pi-conecta-ems-5 (input  l-conecta,
                              input  i-empresa,
                              output l-erro).  /* EMS5 */
    END.

    IF VALID-HANDLE(h-btb009za) THEN
        DELETE PROCEDURE h-btb009za.

END PROCEDURE.


PROCEDURE pi-trata-nfe:
/*------------------------------------------------------------------------------
  Purpose:     Trata NFE
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE h-bodi135     AS HANDLE      NO-UNDO.
    DEFINE VARIABLE l-cons-nota   AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE c-msg-retorno AS CHARACTER   NO-UNDO.
    
    RUN dibo/bodi135.p PERSISTENT SET h-bodi135.
    
    IF  VALID-HANDLE(h-bodi135) THEN DO:
        RUN piAtualAutomNFe IN h-bodi135(INPUT  ROWID(b2-nota-fiscal),
                                         OUTPUT l-cons-nota,
                                         OUTPUT c-msg-retorno).
    END.
    
    DELETE PROCEDURE h-bodi135.
    ASSIGN h-bodi135 = ?.
    
    IF  NOT l-cons-nota THEN /* Nota n∆o pode ser atualizada */
        RETURN "NOK":U.
    ELSE
        RETURN "OK":U.

END PROCEDURE.
 
PROCEDURE pi-conecta-ems-5:
/*------------------------------------------------------------------------------
  Purpose:     Conecta EMS5
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    def input param l-conecta as logical no-undo.
    def input param p-empresa like param-global.empresa-prin no-undo.
    def output param l-erro   as logical no-undo.
    
    assign l-erro = no.
    
    if  l-conecta then do:

        if  search("prgfin/acr/acr900zf.r":U)  = ? 
        and search("prgfin/acr/acr900zf.py":U) = ? then do:

            run utp/ut-msgs.p (input "msg":U,
                               input 6246,
                               input "prgfin/acr/acr900zf.py":U).
            create tt-retorno-nota-fiscal.
            assign tt-retorno-nota-fiscal.tipo         = 1
                   tt-retorno-nota-fiscal.cod-erro  = "6246":U
                   tt-retorno-nota-fiscal.cod-chave = ",,,,,,,,,"
                   tt-retorno-nota-fiscal.desc-erro = return-value
                   tt-retorno-nota-fiscal.situacao  = no.
            assign l-erro = yes.                                

        end.
        else do:
            if  connected("emsuni") 
                then assign l-ja-conec-emsuni = yes.
                else assign l-ja-conec-emsuni = no.
            
            if  connected("emsbas") 
                then assign l-ja-conec-emsbas = yes.
                else assign l-ja-conec-emsbas = no.

            if  connected("emsfin") 
                then assign l-ja-conec-emsfin = yes.
                else assign l-ja-conec-emsfin = no.

            if  l-ja-conec-emsuni = no or 
                l-ja-conec-emsbas = no or
                l-ja-conec-emsfin = no then do:
                
                run pi-conecta-bco IN h-btb009za (Input 1, /* Versao API  */
                                                  input 1, /* 1 - conexao, 2 - Desconexao */
                                                  input p-empresa, /* Codigo da empresa */
                                                  input "all":U, /* nome do banco */
                                                  output table tt_erros_conexao). /* temp-table de erros */
                if  return-value <> "OK":U then do:
                    find first tt_erros_conexao no-lock no-error.
                    if avail tt_erros_conexao then do:
                        create tt-retorno-nota-fiscal.
                        assign tt-retorno-nota-fiscal.tipo   = 1
                            tt-retorno-nota-fiscal.cod-erro  = string(tt_erros_conexao.cd-erro)
                            tt-retorno-nota-fiscal.desc-erro = tt_erros_conexao.param-1
                            tt-retorno-nota-fiscal.cod-chave = ",":U + ",":U + ",":U + ",":U + ",":U + ",":U + ",":U + ",":U
                            tt-retorno-nota-fiscal.situacao  = no.
                        assign l-erro = yes.                                              
                    end.               
                end.                
            end. 
        end.
    end.
    
    if  not l-conecta or l-erro then do:
        if l-ja-conec-emsuni = no 
        or l-ja-conec-emsbas = no 
        or l-ja-conec-emsfin = no then do: 
            run pi-conecta-bco IN h-btb009za (Input 1, /* Versao API  */
                                              input 2, /* 1 - conexao, 2 - Desconexao */
                                              input p-empresa, /* Codigo da empresa */
                                              input "all":U, /* nome do banco */
                                              output table tt_erros_conexao). /* temp-table de erros */
            if return-value <> "OK":U then do:
                find first tt_erros_conexao no-lock no-error.
                if avail tt_erros_conexao then do:
                    create tt-retorno-nota-fiscal.
                    assign tt-retorno-nota-fiscal.tipo   = 1
                        tt-retorno-nota-fiscal.cod-erro  = string(tt_erros_conexao.cd-erro)
                        tt-retorno-nota-fiscal.desc-erro = tt_erros_conexao.param-1
                        tt-retorno-nota-fiscal.cod-chave = ",":U + ",":U + ",":U + ",":U + ",":U + ",":U + ",":U + ",":U
                        tt-retorno-nota-fiscal.situacao  = no.
                    assign l-erro = yes.                                              
                end.
            end.
        end.
    end.

END PROCEDURE.

PROCEDURE pi-alt-aloca :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST pesagem NO-LOCK
         WHERE pesagem.cdd-embarq = i-nr-emb-orig NO-ERROR.
    IF NOT AVAIL pesagem THEN RETURN.

    FOR EACH tt-it-pre-fat:
        DELETE tt-it-pre-fat.
    END.

    FOR EACH tt-erro-aux:
        DELETE tt-erro-aux.
    END.

    RUN eqp/eqapi300.p PERSISTENT SET h-eqapi300. 

    FOR EACH pre-fatur  OF embarque NO-LOCK,
        EACH it-pre-fat OF pre-fatur NO-LOCK,
       FIRST ITEM NO-LOCK
       WHERE ITEM.it-codigo = it-pre-fat.it-codigo:

        CREATE tt-it-pre-fat.
        BUFFER-COPY it-pre-fat TO tt-it-pre-fat.

        ASSIGN tt-it-pre-fat.qt-a-alocar = (pesagem.peso-liquido / ITEM.peso-liquido) - it-pre-fat.qt-alocada
               tt-it-pre-fat.i-sequen    = 1
               tt-it-pre-fat.cdd-embarq = embarque.cdd-embarq.

        RUN pi-recebe-tt-it-pre-fat IN h-eqapi300 (INPUT TABLE tt-it-pre-fat).
        RUN pi-trata-tt-it-pre-fat  IN h-eqapi300 (INPUT YES).
        RUN pi-devolve-tt-erro      IN h-eqapi300 (OUTPUT TABLE tt-erro-aux).
    END.

    IF CAN-FIND(FIRST tt-erro-aux) THEN DO:
        FOR EACH tt-erro-aux:
            RUN pi-gera-erro(INPUT tt-erro-aux.cd-erro,                                
                             INPUT "ERROR":U,                            
                             INPUT tt-erro-aux.mensagem,
                             INPUT "").
        END.
        /*RUN cdp/cd0667.w (INPUT TABLE tt-erro).*/
        RETURN "NOK".
    END.

    RUN pi-finalizar IN h-eqapi300.

END PROCEDURE.

PROCEDURE pi-cancel-nota-orig :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    DEF INPUT PARAM p-cod-estabel AS CHARACTER NO-UNDO.
    DEF INPUT PARAM p-serie       AS CHARACTER NO-UNDO.
    DEF INPUT PARAM p-nr-nota-fis AS CHARACTER NO-UNDO.

    DEFINE VARIABLE bo-cancela AS HANDLE      NO-UNDO.

    ASSIGN l-permit-cancel-if = YES.

    EMPTY TEMP-TABLE RowErrors.

    /*RUN pi-gera-erro(INPUT 17006,                                
                     INPUT "ERROR":U,                            
                     INPUT "Nota est† sendo cancelada. ~~ Nota est† sendo cancelada devido ao erro da integraá∆o com a Unigel Comercial. Favor entrar em contato com a TI.",
                     INPUT "").

    ASSIGN nota-fiscal.dt-at-est = ?.

    /* Cancela nota original */
    RUN dibo/bodi135cancel.p persistent set bo-cancela.

    RUN cancelaNotaFiscal IN bo-cancela (INPUT nota-fiscal.cod-estabel,
                                         INPUT nota-fiscal.serie,
                                         INPUT nota-fiscal.nr-nota-fis,
                                         INPUT TODAY,
                                         INPUT "Erro no processamento de Nota Fiscal.",
                                         INPUT NO,
                                         INPUT YES,
                                         INPUT YES,
                                         INPUT NO).
    
    RUN getRowErrors IN bo-cancela (OUTPUT TABLE RowErrors).
    
    RUN destroy IN bo-cancela .

    IF  CAN-FIND(FIRST funcao NO-LOCK 
                 WHERE funcao.cd-funcao = "SPP-NFE":U
                 AND funcao.ativo = YES)
    AND NOT CAN-FIND (FIRST RowErrors
                      WHERE RowErrors.ErrorNumber = 19531) /*19531 - Indica que a nota foi cancelada. Neste caso n∆o chamar rotina de solicitaá∆o de cancelamento*/
    AND AVAIL nota-fiscal THEN DO:

        
        
        RUN pi-Trata-NFe-cancel (INPUT TODAY,    
                                 INPUT "Nota Fiscal cancelada automaticamente pelo sistema",  
                                 INPUT YES, 
                                 INPUT YES).
    END.*/

    /* Nota Cancelada com exito */
    IF CAN-FIND(FIRST RowErrors
                WHERE RowErrors.ErrorNumber = 19531) THEN DO:

        /* Deletando relacionamento */
        FIND FIRST if-relac-embarque
             WHERE if-relac-embarque.cdd-embarq-ung-com = nota-fiscal.cdd-embarq EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL if-relac-embarque THEN
            DELETE if-relac-embarque.

        RUN pi-cancela-recebimento.
    END.

    FOR EACH RowErrors NO-LOCK:
        RUN pi-gera-erro(INPUT RowErrors.ErrorNumber,
                         INPUT RowErrors.ErrorType,                            
                         INPUT RowErrors.ErrorDescription,
                         INPUT RowErrors.ErrorHelp).
    END.

END PROCEDURE.

PROCEDURE pi-cancela-recebimento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND FIRST docum-est 
         WHERE ROWID(docum-est) = r-docum-est NO-LOCK NO-ERROR.
    IF AVAIL docum-est THEN DO:

        CREATE tt-docum-est.
        BUFFER-COPY docum-est to tt-docum-est NO-ERROR.
        FOR EACH item-doc-est of docum-est NO-LOCK:
            CREATE tt-item-doc-est.
            BUFFER-COPY item-doc-est TO tt-item-doc-est.
        END.
        FOR EACH dupli-apagar OF docum-est NO-LOCK:
            CREATE tt-dupli-apagar.
            BUFFER-COPY dupli-apagar TO tt-dupli-apagar.
        END.

        CREATE tt-digita.
        ASSIGN tt-digita.serie-docto  = docum-est.serie-docto 
               tt-digita.nro-docto    = docum-est.nro-docto   
               tt-digita.cod-emitente = docum-est.cod-emitente 
               tt-digita.nat-operacao = docum-est.nat-operacao.

        CREATE tt-param.
        ASSIGN tt-param.destino             = 2
               tt-param.arquivo             = SESSION:TEMP-DIRECTORY + "RE0402.tmp"
               tt-param.usuario             = c-seg-usuario
               tt-param.data-exec           = TODAY
               tt-param.hora-exec           = TIME
               tt-param.da-data-ini         = 01.01.0001
               tt-param.da-data-fim         = 12.31.9999
               tt-param.c-esp-ini           = ""
               tt-param.c-esp-fim           = "ZZZ"
               tt-param.da-atual-ini        = 01.01.0001
               tt-param.da-atual-fim        = 12.31.9999
               tt-param.c-usuario-ini       = ""
               tt-param.c-usuario-fim       = "ZZZZZZZZZZZZ"
               tt-param.i-tipo-ini          = 1
               tt-param.i-tipo-fim          = 99
               tt-param.l-of                = no
               tt-param.l-saldo             = no
               tt-param.l-desatual          = yes
               tt-param.l-custo-padrao      = no
               tt-param.l-desatualiza-ap    = yes
               tt-param.l-desatualiza-cr    = yes
               tt-param.l-desatualiza-aca   = no
               tt-param.i-prc-custo         = 2
               tt-param.c-custo             = "Medio"
               tt-param.c-destino           = "Arquivo"
               tt-param.l-desatualiza-wms   = no
               tt-param.l-desatualiza-draw  = no.

        RAW-TRANSFER tt-param TO raw-param.
        FOR EACH tt-raw-digita:
            DELETE tt-raw-digita.
        END.
        FOR EACH tt-digita:
            CREATE tt-raw-digita.
            RAW-TRANSFER tt-digita TO tt-raw-digita.raw-digita.
        END.

        RUN rep/re0402rp.p (INPUT raw-param, INPUT TABLE tt-raw-digita).

        DOS SILENT notepad VALUE (SESSION:TEMP-DIRECTORY + "RE0402.tmp").

        /*eliminacao-recebimento*/

        run rep/reapi316b.p (input  "del",  /*cTranAction*/
                             input  table tt-docum-est,
                             input  table tt-rat-docum,
                             input  table tt-item-doc-est,
                             input  table tt-dupli-apagar,
                             input  table tt-dupli-imp,
                             output table tt-erro-receb).


        IF CAN-FIND(FIRST tt-erro-receb) THEN DO:
            FOR EACH tt-erro-receb:
                RUN pi-gera-erro(INPUT tt-erro-receb.cd-erro,
                                 INPUT "ERROR":U,
                                 INPUT tt-erro-receb.desc-erro,
                                 INPUT "").
            END.
        END.

        /*eliminacao-recebimento*/
        
    END.
    
    /*desatualizacao-recebimento*/

END PROCEDURE. /* pi-cancela-recebimento */

PROCEDURE pi-Trata-NFe-cancel:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


    DEFINE INPUT  PARAMETER pdt-cancela        AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pc-desc-cancela    AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pl-reabre-resumo   AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pl-cancela-titulos AS LOGICAL     NO-UNDO.
    
    DEFINE VARIABLE i-cont-progs         AS INTEGER    NO-UNDO.
    DEFINE VARIABLE i-cont-seg           AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lExecutadoPeloFT0911 AS LOGICAL    NO-UNDO.

    ASSIGN dt-cancela        = pdt-cancela    
           c-desc-cancela    = pc-desc-cancela
           l-reabre-resumo   = pl-reabre-resumo  
           l-cancela-titulos = pl-cancela-titulos.

    
    RUN pi-Cancel-NFe.

    /*---
    Quando a Situaá∆o da NF-e estiver 12 ou 13, em processo de cancel/inut, e no retorno (ft0911) n∆o foi poss°vel cancelar a nota, gravar na ret-nf-eletro,
    para mostrar os erros salvos no arquivo ft2200.lst (ret-nf-eleltro Ç mostrada no FT0909)
    ---*/

    ASSIGN lExecutadoPeloFT0911 = NO.

    DO  i-cont-progs = 2 TO 20:
        IF  PROGRAM-NAME(i-cont-progs) MATCHES "*ft0911*" THEN DO:
            ASSIGN lExecutadoPeloFT0911 = YES.
            LEAVE.
        END.
    END.

    IF  lExecutadoPeloFT0911 /*Executado pela Rotina de Cancelamento (sit-nf-eletro 12 ou 13, quando Ç modo ass°ncrono)*/
    AND &if "{&bf_dis_versao_ems}":U >= "2.07":U &then
          (nota-fiscal.idi-sit-nf-eletro   = 12 OR nota-fiscal.idi-sit-nf-eletro   = 13) /* 12 - NF-e em Processo de Cancelamento | 13 - NF-e em Processo de Inutilizaá∆o */
        &else
          CAN-FIND (FIRST sit-nf-eletro NO-LOCK
                    WHERE sit-nf-eletro.cod-estabel  = nota-fiscal.cod-estabel
                      AND sit-nf-eletro.cod-serie    = nota-fiscal.serie
                      AND sit-nf-eletro.cod-nota-fis = nota-fiscal.nr-nota-fis
                      AND (sit-nf-eletro.idi-sit-nf-eletro = 12
                      OR   sit-nf-eletro.idi-sit-nf-eletro = 13))
        &endif
    THEN
        FOR EACH RowErrors NO-LOCK
           WHERE RowErrors.ErrorSubType = "Erro":U: /*Erros que impediram o cancelamento*/

            /*Gravar na ret-nf-eletro*/
            CREATE ret-nf-eletro.
            ASSIGN ret-nf-eletro.cod-estabel = nota-fiscal.cod-estabel
                   ret-nf-eletro.cod-serie   = nota-fiscal.serie
                   ret-nf-eletro.nr-nota-fis = nota-fiscal.nr-nota-fis
                   ret-nf-eletro.cod-msg     = "0"
                   ret-nf-eletro.dat-ret     = TODAY
                   ret-nf-eletro.hra-ret     = REPLACE(STRING(TIME + i-cont-seg, "HH:MM:SS"),":","") 
                   ret-nf-eletro.log-ativo   = YES
                   ret-nf-eletro.cod-livre-2 = "[Msg FT2200] " + (IF INDEX(TRIM(RowErrors.ErrorHelp),TRIM(RowErrors.ErrorDescription)) > 0 /*Conte£do do ErrorDescription est† contido no ErrorHelp*/
                                                                  THEN TRIM(RowErrors.ErrorHelp)
                                                                  ELSE TRIM(RowErrors.ErrorDescription) + " - " + TRIM(RowErrors.ErrorHelp)).

            ASSIGN i-cont-seg = i-cont-seg + 5. /*Incrementar 5 segundos para n∆o dar problema de chave duplicada*/
        END.
    /*---*/

    /* RELEASE nota-fiscal. */
    RELEASE sit-nf-eletro.
    
    IF  VALID-HANDLE(h-axsep002) THEN DO:
        DELETE PROCEDURE h-axsep002.
        ASSIGN h-axsep002 = ?.
    END.
    IF  VALID-HANDLE(h-axsep003) THEN DO:
        DELETE PROCEDURE h-axsep003.
        ASSIGN h-axsep003 = ?.
    END.

END PROCEDURE.

PROCEDURE pi-Cancel-NFe:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    

        IF  CAN-FIND(FIRST funcao NO-LOCK
                     WHERE funcao.cd-funcao = "SPP-INTEG-TSS-SINCRONO":U
                       AND funcao.ativo) THEN DO:
                      
            RUN ftp/ftapi512.p (INPUT "Cancelamento",
                                INPUT ROWID(nota-fiscal),
                                INPUT c-desc-cancela,
                                OUTPUT i-tipo-transacao).
        
        END.              
        ELSE DO:
                
            IF  NOT VALID-HANDLE(h-axsep002)
            OR  h-axsep002:TYPE <> "PROCEDURE":U
            OR  h-axsep002:FILE-NAME <> "adapters/xml/ep2/axsep002.p":U THEN
                RUN adapters/xml/ep2/axsep002.p PERSISTENT SET h-axsep002 (OUTPUT TABLE tt_log_erro_nfe).
    
            RUN PITransUpsert IN h-axsep002 (INPUT "upd":U,
                                             INPUT c-desc-cancela,
                                             INPUT ROWID(nota-fiscal),
                                             OUTPUT i-tipo-transacao, /* 1 - Sincrona | 2 - Assincrona */
                                             OUTPUT TABLE tt_log_erro_nfe,
                                             OUTPUT TABLE tt_nfe_erro).
                                             
        END.
        
        IF  i-tipo-transacao = ? OR i-tipo-transacao = 0 THEN
            NEXT.
        
        /*---- ASS÷NCRONA ---*/
        IF  i-tipo-transacao = 2 THEN DO:

            /*FT - Atualizaá∆o Situaá∆o*/   /*12 - NF-e em processo de Cancelamento*/
            &if "{&bf_dis_versao_ems}" >= "2.07" &then
            
                FIND CURRENT nota-fiscal EXCLUSIVE-LOCK NO-ERROR.
        
                ASSIGN nota-fiscal.idi-sit-nf-eletro = 12. /*12 - NF-e em processo de Cancelamento*/
        
                FIND CURRENT nota-fiscal NO-LOCK        NO-ERROR.

            &else
                FOR FIRST bf-sit-nf-eletro EXCLUSIVE-LOCK
                    WHERE bf-sit-nf-eletro.cod-estabel   = nota-fiscal.cod-estabel 
                      AND bf-sit-nf-eletro.cod-serie     = nota-fiscal.serie       
                      AND bf-sit-nf-eletro.cod-nota-fisc = nota-fiscal.nr-nota-fis :

                    ASSIGN bf-sit-nf-eletro.idi-sit-nf-eletro = 12.  /*12 - NF-e em processo de Cancelamento*/
                END.
                RELEASE bf-sit-nf-eletro.
            &endif
            
            /*
            Grava na sit-nf-eletro toda a parametrizaá∆o feita em tela, para que, quando o receiver executar a rotina de cancelamento, possa cancelar conforme os parametros selecionados pelo usuario
            */
            
            FOR FIRST sit-nf-eletro EXCLUSIVE-LOCK
                WHERE sit-nf-eletro.cod-estabel  = nota-fiscal.cod-estabel
                  AND sit-nf-eletro.cod-serie    = nota-fiscal.serie
                  AND sit-nf-eletro.cod-nota-fis = nota-fiscal.nr-nota-fis:
            END.
            
            IF  NOT AVAIL sit-nf-eletro THEN DO:
                CREATE sit-nf-eletro.
                ASSIGN sit-nf-eletro.cod-estabel  = nota-fiscal.cod-estabel
                       sit-nf-eletro.cod-serie    = nota-fiscal.serie
                       sit-nf-eletro.cod-nota-fis = nota-fiscal.nr-nota-fis.
            END.
        
            ASSIGN sit-nf-eletro.cod-livre-1 = ((IF l-reabre-resumo   THEN "S" ELSE "N") + "#" +
                                                (IF l-cancela-titulos THEN "S" ELSE "N") + "#" +
                                                 TRIM(c-desc-cancela)).
                                                
            RELEASE sit-nf-eletro.

        END. /*fim IF  i-tipo-transacao = 2*/

    

END PROCEDURE.

PROCEDURE pi-embalagens:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF BUFFER b-res-emb FOR res-emb.

    FOR EACH res-emb EXCLUSIVE-LOCK
       WHERE res-emb.cdd-embarq = nota-fiscal.cdd-embarq
         AND res-emb.nr-resumo   = nota-fiscal.nr-resumo:

        FIND FIRST b-res-emb
             WHERE b-res-emb.cdd-embarq = pre-fatur.cdd-embarq
               AND b-res-emb.nr-resumo   = pre-fatur.nr-resumo  
               AND b-res-emb.sigla-emb   = res-emb.sigla-emb EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL b-res-emb THEN DO:
            CREATE b-res-emb.
            BUFFER-COPY res-emb EXCEPT cdd-embarq nr-resumo TO b-res-emb.
            ASSIGN b-res-emb.cdd-embarq = pre-fatur.cdd-embarq
                   b-res-emb.nr-resumo   = pre-fatur.nr-resumo.
        END.

    END.

END PROCEDURE.


PROCEDURE pi-observacao:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    RUN getRecord IN h-bodi317 (OUTPUT TABLE tt-wt-docto).

    FOR FIRST tt-wt-docto
        WHERE tt-wt-docto.seq-wt-docto = i-seq-wt-docto:

        FIND FIRST if-obser-nota
             WHERE if-obser-nota.cdd-embarq = nota-fiscal.cdd-embarq
               AND if-obser-nota.nr-resumo   = tt-res-cli.nr-resumo
               AND if-obser-nota.nome-abrev  = nota-fiscal.nome-ab-cli
               AND if-obser-nota.nr-pedcli   = tt-wt-docto.nr-pedcli  NO-LOCK NO-ERROR.
        IF NOT AVAIL if-obser-nota THEN
            FIND FIRST if-obser-nota
                 WHERE if-obser-nota.cdd-embarq     = nota-fiscal.cdd-embarq
                   AND if-obser-nota.nome-abrev      = nota-fiscal.nome-ab-cli
                   AND if-obser-nota.cod-cond-pag    = nota-fiscal.cod-cond-pag   
                   AND if-obser-nota.nat-operacao    = nota-fiscal.nat-operacao   
                   AND if-obser-nota.no-ab-reppri    = nota-fiscal.no-ab-reppri   
                   AND if-obser-nota.cod-canal-venda = nota-fiscal.cod-canal-venda
                   AND if-obser-nota.cidade-cif      = nota-fiscal.cidade-cif NO-LOCK NO-ERROR.

        IF AVAIL if-obser-nota THEN
            ASSIGN tt-wt-docto.observ-nota = if-obser-nota.observ-nota.

        FOR FIRST b-natur-oper NO-LOCK
            WHERE b-natur-oper.nat-operacao = tt-wt-docto.nat-operacao,
            FIRST mensagem NO-LOCK
            WHERE mensagem.cod-mensagem = b-natur-oper.cod-mensagem:
        
            ASSIGN tt-wt-docto.observ-nota = tt-wt-docto.observ-nota + mensagem.texto-mensag.
        END.
        RUN repositionRecord in h-bodi317 (input tt-wt-docto.r-rowid).
        RUN setRecord    IN h-bodi317 (INPUT TABLE tt-wt-docto). 
        RUN updateRecord IN h-bodi317.
        RUN getRowErrors IN h-bodi317 (OUTPUT TABLE RowErrors APPEND).
    END.

END PROCEDURE.


PROCEDURE pi-gera-nota-compl:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i-seq-wt-it-docto AS INTEGER     NO-UNDO.

    RUN dibo/bodi317in.p PERSISTENT SET h-bodi317in.

    RUN inicializabos IN h-bodi317in(OUTPUT h-bodi317pr,
                                     OUTPUT h-bodi317sd,
                                     OUTPUT h-bodi317im1bra,
                                     OUTPUT h-bodi317va).

    RUN inicializaAcompanhamento IN  h-bodi317sd.

    RUN emptyRowErrors   IN h-bodi317in.

    RUN criaWtDocto IN h-bodi317sd (INPUT c-seg-usuario,                      /* usu†rio                                */
                                    INPUT if-nota-triang.cod-estabel-fat,     /* estabelecimento                        */
                                    INPUT nota-fiscal.serie,                  /* serie                                  */
                                    INPUT "",                                 /* numero de nota caso seja notas manuais */
                                    INPUT nota-fiscal.nome-ab-cli,            /* nome-abrev emitente                    */
                                    INPUT ?,                                  /* pedido                                 */
                                    INPUT 4,                                  /* tipo de nota                           */
                                    INPUT 4003,                               /* codigo do programa que  gerou a nota   */
                                    INPUT TODAY,                              /* data emissao nota                      */
                                    INPUT 0,                                  /* embarque                               */
                                    INPUT if-nota-triang.nat-oper-said-ct-ord, /* natureza operacao                      */
                                    INPUT 0,                                  /* canal venda                            */
                                   OUTPUT i-seq-wt-docto,                     /* seq docto                              */
                                   OUTPUT l-proc-ok-aux).                     /* ok Sim/Nao?                            */

    /* busca poss°veis erros que ocorreram nas validaá‰es */
    RUN devolveErrosBodi317sd IN h-bodi317sd (OUTPUT c-ultimo-metodo-exec,
                                              OUTPUT table RowErrors).

    /* pesquisa algum erro ou advertància que tenha ocorrido */
    IF CAN-FIND(FIRST RowErrors 
                WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:

       RUN pi-error.
       RUN pi-cancela.
       RETURN "NOK".
    END.

    /* disponibilizar o registro wt-docto na bodi317sd */
    FIND FIRST wt-docto where wt-docto.seq-wt-docto = i-seq-wt-docto NO-ERROR.

    ASSIGN wt-docto.cod-emitente = nota-fiscal.cod-emitente
           wt-docto.nome-abrev   = nota-fiscal.nome-ab-cli
           wt-docto.no-ab-reppri = nota-fiscal.no-ab-reppri
           wt-docto.modalidade   = nota-fiscal.modalidade
           wt-docto.cod-portador = nota-fiscal.cod-portador.

    FIND FIRST cond-pagto NO-LOCK
         WHERE cond-pagto.cod-cond-pag = nota-fiscal.cod-cond-pag NO-ERROR.
    IF AVAIL cond-pagto THEN DO:
        ASSIGN wt-docto.cod-cond-pag = cond-pagto.cod-cond-pag
               wt-docto.nr-tab-finan = cond-pagto.nr-tab-finan
               wt-docto.nr-ind-finan = cond-pagto.nr-ind-finan.
    END.

    RUN atualizaDadosGeraisNota IN h-bodi317sd(INPUT  i-seq-wt-docto,
                                                   OUTPUT l-proc-ok-aux).

    IF l-proc-ok-aux = NO THEN DO:
        RUN pi-error.
        RUN pi-cancela.
        RETURN "NOK".
    END.

    /* limpar a tabela de erros em todas as bos */
    RUN emptyRowErrors IN h-bodi317in.

    RUN localizaWtDocto IN h-bodi317sd(INPUT  i-seq-wt-docto,
                                       OUTPUT l-proc-ok-aux).
    IF l-proc-ok-aux = NO THEN DO:
        RUN pi-cancela.
        RETURN "NOK".
    END.

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:

        /* cria um item para nota fiscal. */
        RUN criaWtItDocto IN h-bodi317sd(INPUT ?,                                 /*               */
                                         INPUT "",                                /*               */
                                         INPUT it-nota-fisc.nr-seq-fat,        /* sequencia     */
                                         INPUT it-nota-fisc.it-codigo,         /* item          */
                                         INPUT it-nota-fisc.cod-refer,         /* referencia    */
                                         INPUT if-nota-triang.nat-oper-said-ct-ord, /* nat. operacao */
                                         OUTPUT i-seq-wt-it-docto,                /* sequencia     */
                                         OUTPUT l-proc-ok-aux).                   /* erro          */

        /* busca poss°veis erros que ocorreram nas validaá‰es */
        RUN devolveErrosBodi317sd IN h-bodi317sd(OUTPUT c-ultimo-metodo-exec,
                                                 OUTPUT TABLE RowErrors).
        IF CAN-FIND(FIRST RowErrors 
                    WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
           RUN pi-error.
           RUN pi-cancela.
           RETURN "NOK".
        END.

        /* grava informaá‰es gerais para o item da nota */
        RUN gravaInfGeraisWtItDocto IN h-bodi317sd (INPUT i-seq-wt-docto,                 /* Sequencia                        */
                                                    INPUT i-seq-wt-it-docto,              /* sequencia do item                */
                                                    INPUT it-nota-fisc.qt-faturada[2],    /* quantidade                       */
                                                    INPUT it-nota-fisc.vl-preori,         /* preco do item                    */
                                                    INPUT 0,                              /* percentual de desconto tab-preco */
                                                    INPUT 0).                             /* percentual de desconto do item   */
        /* Logica para observaá∆o da Nota */
       
        FIND FIRST wt-it-docto EXCLUSIVE-LOCK
             WHERE wt-it-docto.seq-wt-docto = i-seq-wt-docto
               AND wt-it-docto.seq-wt-it-docto = i-seq-wt-it-docto
              NO-ERROR.
        IF AVAIL wt-it-docto THEN
        DO:
            ASSIGN wt-it-docto.quantidade[2] = wt-it-docto.quantidade[1]
                   wt-it-docto.un[2]         = wt-it-docto.un[1].
            RELEASE wt-it-docto.
        END.
       
        FIND FIRST wt-docto WHERE wt-docto.seq-wt-docto = i-seq-wt-docto EXCLUSIVE-LOCK NO-ERROR.
       
        /* limpar a tabela de erros em todas as bos */
        RUN emptyRowErrors IN h-bodi317in.
       
        /* disp. registro wt-docto, wt-it-docto e wt-it-imposto na bodi317pr */
        RUN localizaWtDocto IN h-bodi317pr (INPUT i-seq-wt-docto,
                                           OUTPUT l-proc-ok-aux).
       
        RUN localizaWtItdocto IN h-bodi317pr (INPUT i-seq-wt-docto,
                                              INPUT i-seq-wt-it-docto,
                                             OUTPUT l-proc-ok-aux).
       
        RUN localizaWtItImposto IN h-bodi317pr (INPUT i-seq-wt-docto,
                                                INPUT i-seq-wt-it-docto,
                                               OUTPUT l-proc-ok-aux).
        /* atualiza dados c†lculados do item */
        RUN atualizaDadosItemNota IN h-bodi317pr(OUTPUT l-proc-ok-aux).
       
        FIND FIRST wt-it-docto EXCLUSIVE-LOCK USE-INDEX seq-tabela
             WHERE wt-it-docto.seq-wt-docto    = i-seq-wt-docto
               AND wt-it-docto.seq-wt-it-docto = i-seq-wt-it-docto NO-ERROR.
        IF AVAIL wt-it-docto THEN DO:
            ASSIGN wt-it-docto.quantidade[2] = wt-it-docto.quantidade[1]
                   wt-it-docto.un[2]         = wt-it-docto.un[1]. 
            RELEASE wt-it-docto.
        END.
       
        /* busca poss°veis erros que ocorreram nas validaá‰es */
        RUN devolveErrosBodi317pr IN h-bodi317pr(OUTPUT c-ultimo-metodo-exec,
                                                 OUTPUT table RowErrors).
       
        IF CAN-FIND(FIRST RowErrors 
                    WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
            RUN pi-error.
            RUN pi-cancela.
            RETURN "NOK".
        END.
       
        /* limpar a tabela de erros em todas as bos */
        RUN emptyRowErrors IN h-bodi317in.
        /* valida informaá‰es do item */
       
        RUN validaItemDaNota IN h-bodi317va(INPUT i-seq-wt-docto,
                                            INPUT i-seq-wt-it-docto,
                                            OUTPUT l-proc-ok-aux).
       
        /* busca poss°veis erros que ocorreram nas validaá‰es */
        RUN devolveErrosBodi317va IN h-bodi317va(OUTPUT c-ultimo-metodo-exec,
                                                 OUTPUT table RowErrors).
       
        IF CAN-FIND(FIRST RowErrors 
                    WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
            RUN pi-error.
            RUN pi-cancela.
            RETURN "NOK".
        END.

    END.

    run emptyRowErrors           in h-bodi317in.
    run disableMessage15299      in h-bodi317va.
    run inicializaAcompanhamento in h-bodi317pr.
    run calculaWtDocto in h-bodi317pr (input  i-seq-wt-docto,
                                       output l-proc-ok-aux).
    run finalizaAcompanhamento   in h-bodi317pr.
    run devolveErrosbodi317pr    in h-bodi317pr(output c-ultimo-metodo-exec,
                                                output table RowErrors).
    FOR FIRST RowErrors
        where RowErrors.ErrorSubType = "ERROR":U 
        and   RowErrors.ErrorNumber <> 15299: 
        /* MENSAGEM: 15299 - Duplicatas n∆o foram geradas para a nota, n∆o dever† serr considerada. */
        DELETE RowErrors. 
    END.

    IF CAN-FIND(FIRST RowErrors 
                WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
        RUN pi-error.
        RUN pi-cancela.
        RETURN "NOK".
    END.

    run dibo/bodi317ef.p persistent set h-bodi317ef.

    run emptyRowErrors           in h-bodi317in.
    run inicializaAcompanhamento in h-bodi317ef.
    run setaHandlesBOS           in h-bodi317ef(h-bodi317pr,     h-bodi317sd, 
                                                h-bodi317im1bra, h-bodi317va).
    run efetivaNota              in h-bodi317ef(input i-seq-wt-docto,
                                                input yes,
                                                output l-proc-ok-aux).
    run finalizaAcompanhamento   in h-bodi317ef.
    run devolveErrosbodi317ef    in h-bodi317ef(output c-ultimo-metodo-exec,
                                                output table RowErrors).
    
    IF CAN-FIND(FIRST RowErrors 
                WHERE RowErrors.errorSubType = "ERROR":U) THEN DO:
        RUN pi-error.
        RUN pi-cancela.
        RETURN "NOK".
    END.
    
    run buscaTTNotasGeradas in h-bodi317ef(output l-proc-ok-aux,
                                           output table tt-notas-geradas).

    RUN pi-cancela.
    
    delete procedure h-bodi317ef.
    ASSIGN h-bodi317ef = ?.

    IF CAN-FIND(FIRST tt-erro) THEN DO:
        FOR EACH tt-erro:
            RUN pi-gera-erro(INPUT tt-erro.cd-erro,
                             INPUT "ERROR":U,
                             INPUT tt-erro.mensagem,
                             INPUT "").
        END.
    END.
    ELSE DO:
        FOR FIRST tt-notas-geradas NO-LOCK,
            FIRST b3-nota-fiscal NO-LOCK
            WHERE ROWID(b3-nota-fiscal) = tt-notas-geradas.rw-nota-fiscal:

            ASSIGN c-mensagem = string(b3-nota-fiscal.nr-nota-fis) + "~~" +
                                string(b3-nota-fiscal.cod-estabel)  + "~~" +
                                string(b3-nota-fiscal.serie).
        
            RUN pi-gera-erro(INPUT  15263,
                             INPUT "WARNING",
                             INPUT c-mensagem,
                             INPUT "").
        END.
    END.

END PROCEDURE.

PROCEDURE pi-recebimento-triang :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE tt-erro.


    
    IF AVAIL b4-nota-fiscal THEN DO:

        RUN esp/ifreapi190-triang.p (INPUT ROWID(b4-nota-fiscal), 
                                     INPUT if-nota-triang.nat-oper-entr-ct-ord,
                                     OUTPUT TABLE tt-erro).

        IF CAN-FIND(FIRST tt-erro) THEN DO:
            RUN pi-gera-erro(INPUT 17006,
                             INPUT "ERROR":U,
                             INPUT "Ocorreram erros de integraá∆o no recebimento~~" + RETURN-VALUE,
                             INPUT "").
        END.
    END.

    /* Elimina erro padr∆o */
    FOR EACH tt-erro
       WHERE tt-erro.cd-erro = 13
          OR tt-erro.cd-erro = 6506:
         /* AND tt-erro.mensagem BEGINS "Estabelecimento": */
        DELETE tt-erro.
    END.

    FOR EACH tt-erro NO-LOCK:
        RUN pi-gera-erro(INPUT tt-erro.cd-erro,
                         INPUT "ERROR":U,
                         INPUT tt-erro.mensagem,
                         INPUT "").
    END.

END PROCEDURE. /* pi-recebimento-triang */

PROCEDURE pi-cancela:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   IF VALID-HANDLE(h-bodi317sd) THEN
       RUN finalizaAcompanhamento IN h-bodi317sd.
        
    DELETE PROCEDURE h-bodi317in     NO-ERROR.
    DELETE PROCEDURE h-bodi317pr     NO-ERROR.
    DELETE PROCEDURE h-bodi317sd     NO-ERROR.
    DELETE PROCEDURE h-bodi317im1bra NO-ERROR.
    DELETE PROCEDURE h-bodi317va     NO-ERROR.

END PROCEDURE. /* pi-cancela */

PROCEDURE pi-error:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE tt-erro NO-ERROR.
    FOR EACH rowerrors
       WHERE RowErrors.ErrorSubType = "ERROR":
        CREATE tt-erro.
        ASSIGN tt-erro.i-sequen = RowErrors.ErrorSequence
               tt-erro.cd-erro  = RowErrors.ErrorNumber
               tt-erro.mensagem = RowErrors.ErrorDescription.

        /* Advertencia Empresa */
        IF RowErrors.ErrorNumber = 25853 THEN RETURN.
    
        /* Imprimir mensagem de erro porque o produto n∆o est† preparado */
        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT RowErrors.ErrorNumber,
                           INPUT RowErrors.ErrorDescription).

        ASSIGN l-possui-erro = YES.
    END.

END PROCEDURE. /* pi-error */

PROCEDURE pi-alimenta-tms:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE h-bodi582    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE c-utilizacao AS CHARACTER NO-UNDO.

    /* Sen∆o encontrar dados  */
    FIND FIRST tab-generica EXCLUSIVE-LOCK
         WHERE tab-generica.utilizacao = "embarque-ems":U 
           AND tab-generica.char-1     = STRING(if-relac-embarque.cdd-embarq-ung-com) NO-ERROR.
    IF NOT AVAIL tab-generica THEN
        RETURN "OK".

    ASSIGN c-utilizacao = "embarque-ems":U.

    IF NOT VALID-HANDLE(h-bodi582) THEN DO:
        RUN dibo/bodi582.p PERSISTENT SET h-bodi582.
    END.
    RUN setConstraintUtilizacao IN h-bodi582(INPUT c-utilizacao).
    RUN openQueryStatic IN h-bodi582 (INPUT "MAIN":U) NO-ERROR.
        
    IF VALID-HANDLE(h-bodi582) THEN DO:
        RUN goToKeyEmbarque IN h-bodi582 (INPUT STRING(if-relac-embarque.cdd-embarq-cli)). /* cdd-embarq - ft4001 */
    END.
    
    IF RETURN-VALUE = "OK":U THEN DO:
    
        RUN getRecord IN h-bodi582 (OUTPUT TABLE tt-tab-generica).
        
        FIND FIRST tt-tab-generica EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL tt-tab-generica THEN DO:
            ASSIGN tt-tab-generica.char-2     = tab-generica.char-2.
    
            RUN emptyRowErrors  IN h-bodi582.
            RUN setRecord       IN h-bodi582 (INPUT TABLE tt-tab-generica).
            RUN updateRecord    IN h-bodi582.
        END.
    END.    
    ELSE DO:
    
        RUN newRecord    IN h-bodi582.
        RUN getRecord    IN h-bodi582 (OUTPUT TABLE tt-tab-generica).
    
        FIND FIRST tt-tab-generica NO-ERROR.
        IF AVAIL tt-tab-generica THEN DO:
    
            ASSIGN tt-tab-generica.utilizacao = c-utilizacao
                   tt-tab-generica.char-1     = string(if-relac-embarque.cdd-embarq-cli)
                   tt-tab-generica.char-2     = tab-generica.char-2.
    
            RUN emptyRowErrors  IN h-bodi582.                    
            RUN setRecord       IN h-bodi582 (INPUT TABLE tt-tab-generica).
            RUN createRecord    IN h-bodi582.
        END.
    END.    
    
    IF RETURN-VALUE <> "OK":U THEN DO:
        RUN getRowErrors    IN h-bodi582 (output table RowErrors).
    
        IF  CAN-FIND (FIRST RowErrors) THEN DO:
            FOR EACH RowErrors NO-LOCK:
                RUN pi-gera-erro(INPUT RowErrors.ErrorNumber,
                                 INPUT RowErrors.ErrorType,
                                 INPUT RowErrors.ErrorDescription,
                                 INPUT RowErrors.ErrorHelp).
        
                RUN pi-cancel-nota.
            END.
        END.
    END.
    
    IF VALID-HANDLE(h-bodi582) THEN DO:
        DELETE PROCEDURE h-bodi582.
    END.
    
    
    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-alimenta-tms-transf:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE h-bodi582    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE c-utilizacao AS CHARACTER NO-UNDO.

    /* Senío encontrar dados  */
    FIND FIRST tab-generica EXCLUSIVE-LOCK
         WHERE tab-generica.utilizacao = "embarque-ems":U 
           AND tab-generica.char-1     = STRING(nota-fiscal.cdd-embarq) NO-ERROR.
    IF NOT AVAIL tab-generica THEN
        RETURN "OK".

    ASSIGN c-utilizacao = "wtdocto":U.

    IF NOT VALID-HANDLE(h-bodi582) THEN DO:
        RUN dibo/bodi582.p PERSISTENT SET h-bodi582.
    END.
    RUN setConstraintUtilizacao IN h-bodi582(INPUT c-utilizacao).
    RUN openQueryStatic IN h-bodi582 (INPUT "MAIN":U) NO-ERROR.
        
    IF VALID-HANDLE(h-bodi582) THEN DO:
        RUN goToKeyEmbarque IN h-bodi582 (INPUT STRING(i-seq-wt-docto)). /* cdd-embarq - ft4001 */
    END.
    
    IF RETURN-VALUE = "OK":U THEN DO:
    
        RUN getRecord IN h-bodi582 (OUTPUT TABLE tt-tab-generica).
        
        FIND FIRST tt-tab-generica EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL tt-tab-generica THEN DO:
            ASSIGN tt-tab-generica.char-2     = tab-generica.char-2.
    
            RUN emptyRowErrors  IN h-bodi582.
            RUN setRecord       IN h-bodi582 (INPUT TABLE tt-tab-generica).
            RUN updateRecord    IN h-bodi582.
        END.
    END.    
    ELSE DO:
    
        RUN newRecord    IN h-bodi582.
        RUN getRecord    IN h-bodi582 (OUTPUT TABLE tt-tab-generica).
    
        FIND FIRST tt-tab-generica NO-ERROR.
        IF AVAIL tt-tab-generica THEN DO:
    
            ASSIGN tt-tab-generica.utilizacao = c-utilizacao
                   tt-tab-generica.char-1     = STRING(i-seq-wt-docto)
                   tt-tab-generica.char-2     = tab-generica.char-2.
    
            RUN emptyRowErrors  IN h-bodi582.                    
            RUN setRecord       IN h-bodi582 (INPUT TABLE tt-tab-generica).
            RUN createRecord    IN h-bodi582.
        END.
    END.

    IF RETURN-VALUE <> "OK":U THEN DO:
        RUN getRowErrors    IN h-bodi582 (output table RowErrors).
    
        IF  CAN-FIND (FIRST RowErrors) THEN DO:
            FOR EACH RowErrors NO-LOCK:
                RUN pi-gera-erro(INPUT RowErrors.ErrorNumber,
                                 INPUT RowErrors.ErrorType,
                                 INPUT RowErrors.ErrorDescription,
                                 INPUT RowErrors.ErrorHelp).
        
                RUN pi-cancel-nota.
            END.
        END.
    END.
    
    IF VALID-HANDLE(h-bodi582) THEN DO:
        DELETE PROCEDURE h-bodi582.
    END.
    
    
    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-sit-imp :
/*------------------------------------------------------------------------------
  Purpose: Cria log das pesagens apagadas
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE i-sit-nfe          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE i-tp-emis          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE c-sit-nfe          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE h-bodi135na-sitnfe AS HANDLE    NO-UNDO.
    

    IF  NOT VALID-HANDLE(h-bodi135na-sitnfe) THEN DO:
        RUN dibo/bodi135na.p PERSISTENT SET h-bodi135na-sitnfe.
    END. 
    
    RUN retornaSitNF-e IN h-bodi135na-sitnfe (INPUT nota-fiscal.cod-estabel,
                                              INPUT nota-fiscal.serie,
                                              INPUT nota-fiscal.nr-nota-fis,
                                              INPUT nota-fiscal.dt-emis-nota,
                                              OUTPUT i-sit-nfe,
                                              OUTPUT c-sit-nfe).
    
    IF  VALID-HANDLE(h-bodi135na-sitnfe) THEN DO:
        DELETE PROCEDURE h-bodi135na-sitnfe.
        ASSIGN h-bodi135na-sitnfe = ?.
    END.
    
    
    ASSIGN i-tp-emis = nota-fiscal.idi-forma-emis-nf-eletro. 
    
    ASSIGN l-imp-danfe = IF (i-tp-emis = 1 AND i-sit-nfe =  3) OR
                            (i-tp-emis = 3 AND i-sit-nfe =  3) OR
                            (i-tp-emis = 4 AND i-sit-nfe = 15) OR 
                            (i-tp-emis = 4 AND i-sit-nfe =  3) OR 
                           ((i-tp-emis = 2 OR i-tp-emis = 7 OR
                             i-tp-emis = 5) AND ( i-sit-nfe <> 4 
                                            AND i-sit-nfe <> 5
                                            AND i-sit-nfe <> 6
                                            AND i-sit-nfe <> 7
                                            AND i-sit-nfe <> 12
                                            AND i-sit-nfe <> 13)) THEN YES ELSE NO.

END PROCEDURE.



PROCEDURE pi-sppd309 :
/*------------------------------------------------------------------------------
  Purpose: Verifica alocaá∆o dos itens e se estiver errada executa programa de acerto
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND FIRST b-ped-venda NO-LOCK
         WHERE b-ped-venda.nr-pedido = b2-ped-venda.nr-pedido NO-ERROR.
    IF AVAIL b-ped-venda THEN DO:

        IF CAN-FIND(FIRST b2-ped-item OF b-ped-venda
                    WHERE b2-ped-item.qt-alocada > b2-ped-item.qt-atendida) THEN DO:

            create tt-param-sppd309.
            assign tt-param-sppd309.destino        = 2
                   tt-param-sppd309.usuario        = c-seg-usuario
                   tt-param-sppd309.arquivo        = SESSION:TEMP-DIRECTORY + "sppd309a-ung.tmp"
                   tt-param-sppd309.data-exec      = today           
                   tt-param-sppd309.c-nome-abrev   = b-ped-venda.nome-abrev
                   tt-param-sppd309.c-nr-pedcli    = b-ped-venda.nr-pedcli
                   tt-param-sppd309.de-cdd-embarq  = 0
                   tt-param-sppd309.i-tipo         = 1
                   tt-param-sppd309.hora-exec      = time.

            RAW-TRANSFER tt-param-sppd309 TO raw-param.
            FOR EACH tt-raw-digita:
                DELETE tt-raw-digita.
            END.
            FOR EACH tt-digita:
                CREATE tt-raw-digita.
                RAW-TRANSFER tt-digita TO tt-raw-digita.raw-digita.
            END.

            RUN spp/pdp/sppd309a-ung.p (INPUT raw-param, INPUT TABLE tt-raw-digita).

        END.


    END.

END PROCEDURE.

PROCEDURE pi-despreza-itens:
/*------------------------------------------------------------------------------
  Purpose: Despreza os itens que n∆o precisam ser alocados
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH tt-ped-venda-aux,
        EACH it-pre-fat NO-LOCK
       WHERE it-pre-fat.cdd-embarq = i-embarque
         AND it-pre-fat.nome-abrev = tt-ped-venda-aux.nome-abrev
         AND it-pre-fat.nr-pedcli  = tt-ped-venda-aux.nr-pedcli:

        FIND FIRST tt-despreza
             WHERE tt-despreza.cdd-embarq   = it-pre-fat.cdd-embarq  
               AND tt-despreza.nr-resumo    = it-pre-fat.nr-resumo   
               AND tt-despreza.nome-abrev   = it-pre-fat.nome-abrev  
               AND tt-despreza.nr-pedcli    = it-pre-fat.nr-pedcli   
               AND tt-despreza.nr-sequencia = it-pre-fat.nr-sequencia
               AND tt-despreza.it-codigo    = it-pre-fat.it-codigo EXCLUSIVE-LOCK NO-ERROR.  
        IF NOT AVAIL tt-despreza THEN DO:

            FOR EACH it-dep-fat
               WHERE it-dep-fat.cdd-embarq   = it-pre-fat.cdd-embarq
                 AND it-dep-fat.nr-resumo    = it-pre-fat.nr-resumo   /* 25-02-2008  sevarolli */
                 AND it-dep-fat.nome-abrev   = it-pre-fat.nome-abrev
                 AND it-dep-fat.nr-pedcli    = it-pre-fat.nr-pedcli
                 AND it-dep-fat.nr-sequencia = it-pre-fat.nr-sequencia
                 AND it-dep-fat.it-codigo    = it-pre-fat.it-codigo :

                ASSIGN i-seq-aloc-man = i-seq-aloc-man + 1.

                CREATE tt-aloc-man.
                ASSIGN tt-aloc-man.cdd-embarq   = it-pre-fat.cdd-embarq
                       tt-aloc-man.nr-resumo    = it-pre-fat.nr-resumo    
                       tt-aloc-man.nome-abrev   = it-pre-fat.nome-abrev
                       tt-aloc-man.nr-pedcli    = it-pre-fat.nr-pedcli                     
                       tt-aloc-man.nr-sequencia = it-pre-fat.nr-sequencia           
                       tt-aloc-man.it-codigo    = it-pre-fat.it-codigo
                       tt-aloc-man.cod-estabel  = it-dep-fat.cod-estabel         
                       tt-aloc-man.cod-depos    = it-dep-fat.cod-depos           
                       tt-aloc-man.cod-localiz  = it-dep-fat.cod-localiz         
                       tt-aloc-man.lote         = it-dep-fat.nr-serlote          
                       tt-aloc-man.cod-refer    = it-dep-fat.cod-refer           
                       tt-aloc-man.qt-a-alocar  = it-dep-fat.qt-alocada * -1
                       tt-aloc-man.i-sequen     = i-seq-aloc-man
                       tt-aloc-man.nr-entrega   = it-pre-fat.nr-entrega.
            END.
        END.
        ELSE DO:
            ASSIGN tt-despreza.qt-a-alocar = tt-despreza.qt-a-alocar - it-pre-fat.qt-alocada .
        END.
    END.


END PROCEDURE. /* pi-despreza-itens */


PROCEDURE pi-ajusta-qtd-aloca:
/*------------------------------------------------------------------------------
  Purpose: Ajusta Quantidade da alocaá∆o
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE tt-aloc-man.

    FOR EACH tt-despreza:
        CREATE tt-aloc-man.
        BUFFER-COPY tt-despreza TO tt-aloc-man.
    END.

END PROCEDURE. /* pi-ajusta-qtd-aloca */
