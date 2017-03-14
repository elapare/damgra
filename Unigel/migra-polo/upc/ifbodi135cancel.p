/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
&SCOPED-DEFINE base mguni
DEF BUFFER empresa                FOR {&base}.empresa.


{include/i-prgvrs.i IFBODI135CANCEL 2.00.00.000}  /*** 010000 ***/
/*******************************************************************************
**  Programa: IFBODI135CANCEL
**  Objetivo: Tratamento para cancelamento de notas relacionadas IF
**  Autor...: Vertice
**  Data....: 27/02/2011
*******************************************************************************/
{include/i-epc200.i1}

def input param  p-ind-event  as char          no-undo.
def input-output param table for tt-epc.

/*WPA - Buffers*/
define buffer if-ped-venda   for espmulti.if-ped-venda.
define buffer if-natur-oper  for espmulti.if-natur-oper.
define buffer if-estabelec   for espmulti.if-estabelec.
define buffer if-relac-nota  for espmulti.if-relac-nota.

define temp-table tt-param
    field destino            as integer
    field arquivo            as char
    field usuario            as char format "x(12)"
    field data-exec          as date
    field hora-exec          as integer
    field da-data-ini        as date format "99/99/9999"
    field da-data-fim        as date format "99/99/9999"
    field c-esp-ini          as char
    field c-esp-fim          as char
    field c-ser-ini          as char
    field c-ser-fim          as char
    field c-num-ini          as char
    field c-num-fim          as char
    field i-emit-ini         as integer
    field i-emit-fim         as integer
    field c-nat-ini          as char
    field c-nat-fim          as char
    field c-estab-ini        as char
    field c-estab-fim        as char
    field da-atual-ini       as date format "99/99/9999"
    field da-atual-fim       as date format "99/99/9999"
    field c-usuario-ini      as char 
    field c-usuario-fim      as char
    field i-tipo-ini         as integer
    field i-tipo-fim         as integer
    field l-of               as logical
    field l-saldo            as logical
    field l-desatual         as logical
    field l-custo-padrao     as logical
    field l-desatualiza-ap   as logical
    field l-desatualiza-aca  as logical    
    field i-prc-custo        as integer
    field c-custo            as char
    field c-destino          as char
    field l-desatualiza-wms  as logical
    field l-desatualiza-draw as logical
    field l-desatualiza-cr   as logical
    FIELD l-imp-param        AS LOGICAL
    FIELD mot-canc           AS CHAR.

define temp-table tt-digita
    field serie-docto  like docum-est.serie-docto
    field nro-docto    like docum-est.nro-docto
    field cod-emitente like docum-est.cod-emitente
    field nat-operacao like docum-est.nat-operacao.

def temp-table tt-docum-est no-undo like docum-est
    field r-rowid as rowid.

def temp-table tt-rat-docum no-undo like rat-docum
    field r-rowid as rowid.

def temp-table tt-item-doc-est no-undo like item-doc-est
    field r-rowid as rowid.

def temp-table tt-dupli-apagar no-undo like dupli-apagar
    field r-rowid as rowid.

def temp-table tt-dupli-imp no-undo like dupli-imp
    field r-rowid as rowid.

def temp-table tt-erro no-undo
    field identif-segment as char
    field cd-erro         as integer
    field desc-erro       as char format "x(80)".

{cdp/cd0667.i -aux}   /* Temp-table de erros (auxiliar)      */

define temp-table tt-param-ft0605
    field destino          as integer
    field arquivo          as char
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field rs-preco         as integer
    field desc-preco       as char format "x(15)"
    field da-periodo-ini   as date
    field da-periodo-fim   as date
    field c-estabel-ini    as character
    field c-estabel-fim    as character
    field rs-preco-custo   as integer
    field rs-opcao         as integer.

DEFINE TEMP-TABLE tt-param-ft2200 NO-UNDO
    FIELD destino            AS INTEGER
    FIELD arquivo            AS CHAR FORMAT "x(35)"
    FIELD usuario            AS CHAR FORMAT "x(12)"
    FIELD data-exec          AS DATE
    FIELD hora-exec          AS INTEGER
    FIELD cod-estabel        LIKE nota-fiscal.cod-estabel
    FIELD serie              LIKE nota-fiscal.serie
    FIELD nr-nota-fis        LIKE nota-fiscal.nr-nota-fis
    FIELD dt-cancela         LIKE nota-fiscal.dt-cancela
    FIELD desc-cancela       LIKE nota-fiscal.desc-cancela
    FIELD arquivo-estoq      AS CHAR
    FIELD reabre-resumo      AS LOG 
    FIELD cancela-titulos    AS LOG 
    FIELD imprime-ajuda      AS LOG 
    FIELD l-valida-dt-saida  AS LOG
    FIELD elimina-nota-nfse  AS LOG.

DEF TEMP-TABLE tt-embarque NO-UNDO LIKE embarque USE-INDEX ch-emb
    FIELD i-sequen AS INTEGER
    FIELD ind-oper AS INTEGER. /* 1 - Inclus∆o
                                  2 - Alteraá∆o
                                  3 - Eliminaá∆o */

DEF TEMP-TABLE tt-ped-venda-aux NO-UNDO
    FIELD i-sequen    AS INTEGER
    FIELD cdd-embarq LIKE embarque.cdd-embarq
    FIELD nome-abrev  LIKE ped-venda.nome-abrev
    FIELD nr-pedcli   LIKE ped-venda.nr-pedcli
    FIELD ind-oper    AS INTEGER /* 1 - Alocar / 2 - Desalocar */
    INDEX ch-pedido IS PRIMARY
          nome-abrev
          nr-pedcli.

DEFINE BUFFER bf-nota-fiscal  FOR nota-fiscal.
DEFINE BUFFER b-nota-fiscal   FOR nota-fiscal.
DEFINE BUFFER bf-it-nota-fisc FOR it-nota-fisc.
DEFINE BUFFER b2-it-nota-fisc FOR it-nota-fisc.
DEFINE BUFFER b3-it-nota-fisc FOR it-nota-fisc.
DEFINE BUFFER bf-if-ped-venda FOR espmulti.if-ped-venda.
DEFINE BUFFER bf-ped-venda    FOR ped-venda.
DEFINE BUFFER b2-ped-venda    FOR ped-venda.
DEFINE BUFFER b3-ped-venda    FOR ped-venda.
DEFINE BUFFER b2-nota-fiscal  FOR nota-fiscal.
DEFINE BUFFER b3-nota-fiscal  FOR nota-fiscal.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEFINE VARIABLE raw-param    AS RAW         NO-UNDO.
DEFINE VARIABLE l-bodi135    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE i-aux        AS INTEGER     NO-UNDO.
DEFINE VARIABLE l-tem-nf-res AS LOGICAL     NO-UNDO.

DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita        AS RAW.

IF p-ind-event = "onvalidatecancel" THEN DO:
    FIND FIRST tt-epc 
         WHERE tt-epc.cod-event     = p-ind-event
           AND tt-epc.cod-parameter = "TABLE-ROWID" NO-ERROR.
    IF AVAIL tt-epc THEN DO:
        FIND FIRST nota-fiscal NO-LOCK
             WHERE ROWID(nota-fiscal) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.
        IF AVAIL nota-fiscal THEN DO:
            FIND FIRST tt-epc 
                 WHERE tt-epc.cod-event     = p-ind-event
                   AND tt-epc.cod-parameter = "OBJECT-HANDLE" NO-ERROR.
            IF AVAIL tt-epc THEN DO:

                /*IF (TODAY - nota-fiscal.dt-emis-nota) > 7 THEN DO:
                    RUN _insertErrorManual IN widget-handle(tt-epc.val-parameter) (INPUT 17006,
                                                                                   INPUT "EMS",
                                                                                   INPUT "ERROR", 
                                                                                   INPUT "Nota Fiscal com prazo vencido para cancelamento",
                                                                                   INPUT "A data de emiss∆o desta nota superou 7 (sete) dias e seu cancelamento n∆o ser† permitido",
                                                                                   INPUT "":U).
                    RETURN "NOK".
                END.
                ELSE DO:*/

                    ASSIGN l-bodi135 = NO.
                    DO i-aux = 1 TO 20:
                        IF INDEX(program-name(i-aux),"ifbodi135.p") > 0 THEN
                            ASSIGN l-bodi135 = YES.
                    END.

                    IF NOT l-bodi135 THEN DO:
                        FIND FIRST ped-venda NO-LOCK
                             WHERE ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                               AND ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli NO-ERROR.
                        FIND FIRST if-ped-venda NO-LOCK
                             WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-ERROR.
                        IF AVAIL if-ped-venda THEN DO:
                            
                           /*FOR LAST it-nota-fisc NO-LOCK
                               WHERE it-nota-fisc.nr-pedido = if-ped-venda.nr-pedido-relac,
                               FIRST nota-fiscal OF it-nota-fisc NO-LOCK
                               WHERE nota-fiscal.dt-cancela = ?:*/

                            FOR EACH it-nota-fisc NO-LOCK
                               WHERE it-nota-fisc.nr-pedido = if-ped-venda.nr-pedido-relac 
                                  BY it-nota-fisc.nr-nota-fis DESCENDING:

                                FIND FIRST b-nota-fiscal OF it-nota-fisc NO-LOCK
                                     WHERE b-nota-fiscal.dt-cancela <> ? NO-ERROR.
                                IF AVAIL b-nota-fiscal THEN LEAVE.

                                FIND FIRST if-relac-nota
                                     WHERE if-relac-nota.cod-estabel  = nota-fiscal.cod-estabel
                                       AND if-relac-nota.serie        = nota-fiscal.serie
                                       AND if-relac-nota.nr-nota-fis  = nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
                                IF NOT AVAIL if-relac-nota THEN LEAVE.

                                /* Desconsidera relac de transf */
                                FIND FIRST if-relac-nota
                                     WHERE if-relac-nota.recid-nf-origem = RECID(nota-fiscal) NO-LOCK NO-ERROR.
                                IF AVAIL if-relac-nota THEN LEAVE.

                                RUN _insertErrorManual IN widget-handle(tt-epc.val-parameter) (INPUT 17006,
                                                                                               INPUT "EMS",
                                                                                               INPUT "ERROR", 
                                                                                               INPUT "Nota Fiscal relacionada a Incentivo Fiscal n∆o pode ser cancelada",
                                                                                               INPUT "O cancelamento dever† ocorrer na unidade de atendimento: " + if-ped-venda.cod-estab-atend,
                                                                                               INPUT "":U).
                                RETURN "NOK".
                            END.
                        END.
                    END.
                /* END. */
            END.

            /* SENF */
            FIND FIRST senf-wt-docto
                 WHERE senf-wt-docto.cod-estabel = nota-fiscal.cod-estabel
                   AND senf-wt-docto.serie       = nota-fiscal.serie
                   AND senf-wt-docto.nr-nota-fis = nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
            IF AVAIL senf-wt-docto THEN DO:
                FIND FIRST senf
                     WHERE senf.senf = senf-wt-docto.senf EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL senf THEN DO:
                    ASSIGN senf.seq-wt-docto = 0.
                END.
            END.
        END.
    END.
END.

IF p-ind-event = "Fim_CancelaNotaFiscal" THEN DO:

    FIND FIRST tt-epc 
         WHERE tt-epc.cod-event     = p-ind-event
           AND tt-epc.cod-parameter = "notafiscal-rowid" NO-ERROR.
    IF AVAIL tt-epc THEN DO:

        FIND FIRST nota-fiscal NO-LOCK
             WHERE ROWID(nota-fiscal) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.

        IF AVAIL nota-fiscal AND nota-fiscal.cod-estabel <> "432" AND nota-fiscal.cod-estabel <> "443" THEN DO: /*solic-318*/

            FIND FIRST if-relac-nota
                 WHERE if-relac-nota.cod-estabel  = nota-fiscal.cod-estabel
                   AND if-relac-nota.serie        = nota-fiscal.serie
                   AND if-relac-nota.nr-nota-fis  = nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
            IF AVAIL if-relac-nota AND if-relac-nota.recid-nf-origem <> 0 THEN DO:
                FOR EACH  if-relac-nota
                     WHERE if-relac-nota.cod-estabel  = nota-fiscal.cod-estabel
                       AND if-relac-nota.serie        = nota-fiscal.serie
                       AND if-relac-nota.nr-nota-fis  = nota-fiscal.nr-nota-fis EXCLUSIVE-LOCK .

                    FOR   FIRST bf-nota-fiscal NO-LOCK
                        WHERE RECID(bf-nota-fiscal) = if-relac-nota.recid-nf-origem
                          AND bf-nota-fiscal.dt-cancela = ? :
    
                        RUN pi-desatualiza-recebimento.
                        RUN pi-desatualiza-estatica.
                        RUN pi-cancela-faturamento.
    
                    END.

                    /* Verifica se a nota foi eliminada do recebimento e eliminar a if-relac-nota */
                    FIND FIRST docum-est OF if-relac-nota NO-LOCK NO-ERROR.
                    IF NOT AVAIL docum-est THEN
                        DELETE if-relac-nota.
                END.

            END.
            ELSE DO:
                FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
    
                    FIND FIRST ped-venda NO-LOCK
                         WHERE ped-venda.nome-abrev = it-nota-fisc.nome-ab-cli
                           AND ped-venda.nr-pedcli  = it-nota-fisc.nr-pedcli NO-ERROR.
                    FIND FIRST if-ped-venda NO-LOCK
                         WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-ERROR.
                    IF NOT AVAIL if-ped-venda THEN DO:
                        FIND FIRST if-ped-venda NO-LOCK
                             WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido NO-ERROR.
                        DO WHILE CAN-FIND(FIRST bf-if-ped-venda
                                          WHERE bf-if-ped-venda.nr-pedido-relac = if-ped-venda.nr-pedido):
                            FIND FIRST bf-if-ped-venda
                                 WHERE bf-if-ped-venda.nr-pedido-relac = if-ped-venda.nr-pedido NO-ERROR.
                            FIND FIRST if-ped-venda NO-LOCK
                                 WHERE if-ped-venda.nr-pedido-relac = bf-if-ped-venda.nr-pedido NO-ERROR.
                        END.
                
                        IF AVAIL if-ped-venda THEN DO:
                            FIND FIRST ped-venda NO-LOCK
                                 WHERE ped-venda.nr-pedido = if-ped-venda.nr-pedido NO-ERROR.
                
                            IF AVAIL ped-venda THEN DO:
                                FIND FIRST if-estabelec NO-LOCK
                                     WHERE if-estabelec.cod-estab-orig = ped-venda.cod-estabel
                                       AND if-estabelec.cod-estab-dest = nota-fiscal.cod-estabel NO-ERROR.
                                IF AVAIL if-estabelec THEN DO:
    
                                    /* Verifica se as notas canceladas est∆o atrelas ao resumo anterior */
                                    ASSIGN l-tem-nf-res = NO.
                                    FOR EACH bf-it-nota-fisc NO-LOCK
                                       WHERE bf-it-nota-fisc.nr-pedido = ped-venda.nr-pedido ,
                                       FIRST bf-nota-fiscal OF bf-it-nota-fisc NO-LOCK
                                       WHERE bf-nota-fiscal.dt-cancela = ?,
            
                                        EACH b2-it-nota-fisc OF bf-nota-fiscal NO-LOCK,
                                       FIRST b2-ped-venda NO-LOCK
                                       WHERE b2-ped-venda.nome-abrev = b2-it-nota-fisc.nome-ab-cli
                                         AND b2-ped-venda.nr-pedcli  = b2-it-nota-fisc.nr-pedcli,
                                       FIRST if-ped-venda NO-LOCK
                                       WHERE if-ped-venda.nr-pedido = b2-ped-venda.nr-pedido,
                                       FIRST b3-ped-venda EXCLUSIVE-LOCK
                                       WHERE b3-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac,
                                        EACH b3-it-nota-fisc NO-LOCK
                                       WHERE b3-it-nota-fisc.nr-pedido = b3-ped-venda.nr-pedido,
                                       FIRST b2-nota-fiscal OF b3-it-nota-fisc NO-LOCK: 
            
                                        IF b2-nota-fiscal.dt-cancela = ? AND ROWID(b2-nota-fiscal) <> ROWID(bf-nota-fiscal) THEN DO:
                                            ASSIGN l-tem-nf-res = YES.
                                            LEAVE.
                                        END.
                                    END.
    
                                    IF l-tem-nf-res = YES THEN
                                        RETURN "OK".
    
                                    RUN pi-desaloca-embarque.
                                    IF RETURN-VALUE = "NOK" THEN RETURN "NOK".
                
                                    FOR EACH bf-it-nota-fisc NO-LOCK
                                       WHERE bf-it-nota-fisc.nr-pedido = ped-venda.nr-pedido ,
                                       FIRST bf-nota-fiscal OF bf-it-nota-fisc NO-LOCK
                                       WHERE bf-nota-fiscal.dt-cancela = ? :

                                        FIND FIRST if-relac-nota OF bf-nota-fiscal EXCLUSIVE-LOCK NO-ERROR.
                                        IF AVAIL if-relac-nota THEN DO:

                                            RUN pi-desatualiza-recebimento.

                                            /* Verifica se a nota foi eliminada do recebimento e eliminar a if-relac-nota */
                                            FIND FIRST docum-est OF if-relac-nota NO-LOCK NO-ERROR.
                                            IF NOT AVAIL docum-est THEN
                                                DELETE if-relac-nota.
                                        END.

                                        RUN pi-desatualiza-estatica.
                                        RUN pi-cancela-faturamento.
        
                                    END.
                                END.
                            END.
                        END.
                    END.
                END.
            END.
        END.
    END.
END.

RETURN "OK".


PROCEDURE pi-desatualiza-recebimento:
/*------------------------------------------------------------------------------
  Purpose:     Desatualiza Nota no recebimento
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /*desatualizacao-recebimento*/
    EMPTY TEMP-TABLE tt-param  NO-ERROR.
    EMPTY TEMP-TABLE tt-digita NO-ERROR.

    FIND FIRST docum-est OF if-relac-nota NO-LOCK NO-ERROR.
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
                             output table tt-erro).
    
        /*eliminacao-recebimento*/
        
    END.
    
    /*desatualizacao-recebimento*/

END PROCEDURE.

PROCEDURE pi-desatualiza-estatica:
/*------------------------------------------------------------------------------
  Purpose:     Desatualiza Estatistica
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* desatualiza estatistica faturamento */
    EMPTY TEMP-TABLE tt-param-ft0605.

    find first para-fat no-lock no-error.

    create tt-param-ft0605.
    assign tt-param-ft0605.usuario        = c-seg-usuario
           tt-param-ft0605.arquivo        = SESSION:TEMP-DIRECTORY + "ft0605.tmp"
           tt-param-ft0605.destino        = 2 /* arquivo */
           tt-param-ft0605.data-exec      = today
           tt-param-ft0605.hora-exec      = time
           tt-param-ft0605.da-periodo-ini = bf-nota-fiscal.dt-emis-nota
           tt-param-ft0605.da-periodo-fim = bf-nota-fiscal.dt-emis-nota
           tt-param-ft0605.c-estabel-ini  = bf-nota-fiscal.cod-estabel
           tt-param-ft0605.c-estabel-fim  = bf-nota-fiscal.cod-estabel
           tt-param-ft0605.rs-opcao       = 2 /* Desatualiza */
           tt-param-ft0605.rs-preco-custo = integer(substring(para-fat.char-2,20,1))
           tt-param-ft0605.rs-preco       = integer(substring(para-fat.char-2,20,1))
           tt-param-ft0605.desc-preco     = substring(para-fat.char-2,21,20).

    RAW-TRANSFER tt-param-ft0605 TO raw-param.
    RUN ftp/ft0605rp.p(INPUT raw-param, INPUT TABLE tt-raw-digita).
    DOS SILENT notepad VALUE (SESSION:TEMP-DIRECTORY + "ft0605.tmp").

    /* desatualiza estatistica faturamento */

END PROCEDURE.

PROCEDURE pi-cancela-faturamento:
/*------------------------------------------------------------------------------
  Purpose:     Cancela Faturameto
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /*cancelamento-faturamento*/
    FOR EACH tt-param-ft2200:
        DELETE tt-param-ft2200.
    END.
    
    CREATE tt-param-ft2200.
    ASSIGN tt-param-ft2200.usuario             = c-seg-usuario
           tt-param-ft2200.destino             = 2 /* arquivo */
           tt-param-ft2200.data-exec           = TODAY
           tt-param-ft2200.hora-exec           = TIME
           tt-param-ft2200.cod-estabel         = bf-nota-fiscal.cod-estabel
           tt-param-ft2200.serie               = bf-nota-fiscal.serie
           tt-param-ft2200.nr-nota-fis         = bf-nota-fiscal.nr-nota-fis
           tt-param-ft2200.dt-cancela          = TODAY
           tt-param-ft2200.desc-cancela        = "Nota Fiscal cancelada automaticamente pelo sistema":U
           tt-param-ft2200.arquivo-estoq       = SESSION:TEMP-DIRECTORY + "FT2100-orig.tmp"
           tt-param-ft2200.arquivo             = SESSION:TEMP-DIRECTORY + "FT2200-orig.tmp"
           tt-param-ft2200.l-valida-dt-saida   = NO /*faz gerar erro no cancelamento da nota que tem Data Saida de Mercadoria informada*/
           tt-param-ft2200.imprime-ajuda       = YES
           /*Parametros salvos no momento da solicitacao de cancelamento/inutlizacao (gravados no ft2200/ft2201)*/
           tt-param-ft2200.reabre-resumo       = YES
           tt-param-ft2200.cancela-titulos     = YES.


    /* Deletando relacionamento */
    FIND FIRST if-relac-embarque
         WHERE if-relac-embarque.cdd-embarq-ung-com = bf-nota-fiscal.cdd-embarq EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL if-relac-embarque THEN
        DELETE if-relac-embarque.

    RAW-TRANSFER tt-param-ft2200 TO raw-param.
    RUN ftp/ft2200rp.p(INPUT raw-param, INPUT TABLE tt-raw-digita).
    /* DOS SILENT notepad VALUE (SESSION:TEMP-DIRECTORY + "FT2100-orig.tmp"). */
    DOS SILENT notepad VALUE (SESSION:TEMP-DIRECTORY + "FT2200-orig.tmp").

    /*cancelamento-faturamento*/

END PROCEDURE.

PROCEDURE pi-desaloca-embarque:
/*------------------------------------------------------------------------------
  Purpose:     Desaloca Embarque
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE h-eqapi300  AS HANDLE      NO-UNDO.
    DEFINE VARIABLE i-sequencia AS INTEGER     NO-UNDO.

    DEF BUFFER b-pre-fatur FOR pre-fatur.

    EMPTY TEMP-TABLE tt-embarque.
    EMPTY TEMP-TABLE tt-ped-venda-aux.
    
    ASSIGN i-sequencia = 0.
    
    RUN eqp/eqapi300.p PERSISTENT SET h-eqapi300.

    gera-embarque:
    DO TRANSACTION:
        EMPTY TEMP-TABLE tt-embarque.
        EMPTY TEMP-TABLE tt-ped-venda-aux.
        EMPTY TEMP-TABLE tt-erro-aux.

        FOR FIRST bf-ped-venda OF it-nota-fisc NO-LOCK,
             EACH pre-fatur OF bf-ped-venda NO-LOCK:

            ASSIGN i-sequencia = i-sequencia + 1.

            CREATE tt-embarque.
            ASSIGN tt-embarque.usuario       = c-seg-usuario
                   tt-embarque.identific     = c-seg-usuario
                   tt-embarque.cdd-embarq   = pre-fatur.cdd-embarq
                   tt-embarque.dt-embarque   = TODAY
                   tt-embarque.i-sequen      = i-sequencia
                   tt-embarque.ind-oper      = 2 /* Alteraá∆o */
                   tt-embarque.cod-estabel   = nota-fiscal.cod-estabel
                   tt-embarque.cod-rota      = nota-fiscal.cod-rota
                   tt-embarque.nome-transp   = nota-fiscal.nome-transp
                   tt-embarque.placa         = nota-fiscal.placa.
        
            CREATE tt-ped-venda-aux.
            ASSIGN tt-ped-venda-aux.i-sequen        = i-sequencia
                   tt-ped-venda-aux.cdd-embarq     = pre-fatur.cdd-embarq
                   tt-ped-venda-aux.nome-abrev      = it-nota-fisc.nome-ab-cli
                   tt-ped-venda-aux.nr-pedcli       = it-nota-fisc.nr-pedcli
                   tt-ped-venda-aux.ind-oper        = 2. /* Eliminacao */
 
            IF NOT CAN-FIND(FIRST b-pre-fatur 
                            WHERE b-pre-fatur.cdd-embarq = pre-fatur.cdd-embarq
                              AND b-pre-fatur.nr-resumo  <> pre-fatur.nr-resumo) THEN
                ASSIGN tt-embarque.ind-oper = 3. /* Eliminaá∆o */
            
            
            RUN pi-recebe-tt-ped-venda IN h-eqapi300 (INPUT TABLE tt-ped-venda-aux).
            RUN pi-trata-tt-ped-venda  IN h-eqapi300 (INPUT YES).
            RUN pi-recebe-tt-embarque  IN h-eqapi300 (INPUT TABLE tt-embarque).
            RUN pi-trata-tt-embarque   IN h-eqapi300 (INPUT 'msg', INPUT YES).
            RUN pi-devolve-tt-erro     IN h-eqapi300 (OUTPUT TABLE tt-erro-aux).
        END.
        
        IF CAN-FIND(FIRST tt-erro-aux) THEN DO:
            FOR EACH tt-erro-aux:
                RUN utp/ut-msgs.p(INPUT "show",
                                  INPUT 17006,
                                  INPUT tt-erro-aux.mensagem).
            END.

            UNDO gera-embarque, LEAVE.
        END.
        ELSE DO:
            FOR EACH tt-embarque :

                FIND FIRST embarque
                     WHERE embarque.cdd-embarq = tt-embarque.cdd-embarq NO-LOCK NO-ERROR.

                IF NOT AVAIL embarque THEN DO:

                    FIND FIRST if-relac-embarque
                         WHERE if-relac-embarque.cdd-embarq-cli = tt-embarque.cdd-embarq EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAIL if-relac-embarque THEN DO:
                        DELETE if-relac-embarque.
                    END.
                END.
            END.
        END.

    END. /* fim transacao gera-embarque */

    RUN pi-finalizar IN h-eqapi300.

    IF CAN-FIND(FIRST tt-erro-aux) THEN
        RETURN "NOK".
    
    EMPTY TEMP-TABLE tt-erro-aux.

    RETURN "OK".


END PROCEDURE.

