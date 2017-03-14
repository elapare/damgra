 
/************************************************************************ 
 *** Programa: ESPD0008R                                              ***    
 *** Objetivo: Relatorido de Expdicao                                 ***
 *** Empresa: UNIGEL                                                  ***
 *** Programador: Ress Informatica                                    ***
 ***********************************************************************/

/* **************** Alteraá‰es no Programa ******************************

   Data     : 21/06/2004
   Autor    : Ricardo Pinho
   Objetivo : 1. Incluir campo natureza de operaá∆o
              2. retirado if para o campo "saldo para atender pedido", antes 
              ele n∆o calculado para pedidos de exportaá∆o, agora est† para todas
              as situaá‰es.
   
   Data     : 18/10/2005
   Autor    : Bruno Sanches Fr¢es
   Objetivo : Incluir coluna da Avaliaá∆o de CrÇdito.
   
   Data     : 23/05/2006
   Autor    : Claudio Varela
   Objetivo : Alteraá∆o do caminho de localizaá∆o do modelo excel para \ung\modelos.
   
   Data     : 02/10/2012
   Autor    : Rodrigo Luis Frohlich
   Objetivo : Impress∆o Cliente Final - Unigel Comercial
             
   ********************************************************************** */

 
{include/i-prgvrs.i ESPD0008RP 2.00.00.001}  /*** 010006 ***/
DEF BUFFER bf-item FOR item.
DEF BUFFER b-ped-venda FOR ped-venda.

define temp-table tt-param
    FIELD destino                   AS INTEGER
    FIELD arquivo                   AS CHAR
    FIELD usuario                   AS CHAR
    FIELD data-exec                 AS DATE
    FIELD hora-exec                 AS INTEGER
    FIELD cod-estabel-ini           LIKE ped-venda.cod-estabel
    FIELD cod-estabel-fim           LIKE ped-venda.cod-estabel
    FIELD nr-pedido-ini             LIKE ped-venda.nr-pedido
    FIELD nr-pedido-fim             LIKE ped-venda.nr-pedido
    FIELD nr-pedcli-ini             LIKE ped-venda.nr-pedcli
    FIELD nr-pedcli-fim             LIKE ped-venda.nr-pedcli
    FIELD nome-abrev-ini            LIKE ped-venda.nome-abrev
    FIELD nome-abrev-fim            LIKE ped-venda.nome-abrev
    FIELD cod-depos-1               AS CHAR FORMAT "x(3)"
    FIELD cod-depos-2               AS CHAR FORMAT "x(3)"
    FIELD cod-depos-3               AS CHAR FORMAT "x(3)"
    FIELD cod-depos-4               AS CHAR FORMAT "x(3)"
    FIELD fm-codigo-ini             LIKE ITEM.fm-codigo
    FIELD fm-codigo-fim             LIKE ITEM.fm-codigo
    FIELD it-codigo-ini             LIKE ITEM.it-codigo
    FIELD it-codigo-fim             LIKE ITEM.it-codigo
    FIELD rs-classifica             AS INTEGER
    FIELD rs-pedido                 AS INTEGER
    FIELD tg-localiza               AS LOGICAL
    FIELD tg-especial               AS INT /* LOGICAL */
    FIELD tg-compl                  AS LOGICAL
    FIELD dt-implanta-ini           AS DATE
    FIELD dt-implanta-fim           AS DATE
    FIELD rs-mercado                AS INT
    FIELD arquivo-excel             AS CHAR
    FIELD dt-corte                  AS DATE .

DEFINE TEMP-TABLE temp-item
  FIELD it-codigo         LIKE item.it-codigo /* as char format "xxxx-xxxxXxxxxXxxxx" */
  FIELD desc-item         LIKE ITEM.desc-item /* 04.01.2011 - inclus∆o da descriá∆o */
  FIELD cod-refer         LIKE ITEM.cod-refer
  FIELD cod-emitente      like emitente.cod-emitente
  FIELD nr-pedcli         like ped-item.nr-pedcli
  field nome-abrev        like ped-venda.nome-abrev
  field nome-ab-ung-com   like ped-venda.nome-abrev
  FIELD nr-pedido         like ped-venda.nr-pedido
  FIELD nr-pedido-ung-com like ped-venda.nr-pedido
  FIELD cod-priori        like ped-venda.cod-priori
  FIELD qtd-estoq         as dec format ">>>,>>>,>>9.99-"
  FIELD qtd-pedida        as dec format ">>>,>>>,>>9.99-"
  FIELD qtd-atend         as dec format ">>>,>>>,>>9.99-"
  FIELD qtd-saldo         as dec format ">>>,>>>,>>9.99-"
  FIELD qtd-pend          as dec format ">>>,>>>,>>9.99-"
  FIELD qtd-transfer      as dec format ">>>,>>>,>>9.99-"
  FIELD qtd-transito      as dec format ">>>,>>>,>>9.99-"
  FIELD dt-entrega        as date format "99/99/9999"
  FIELD dt-exp-bahia      as date /* format "99/99/9999" */
  FIELD dt-pedido         as date format "99/99/9999"
  FIELD cod-estabel       like estabelec.cod-estabel
  FIELD nome-tr-red       LIKE ped-venda.nome-tr-red
  FIELD nome-transp       LIKE ped-venda.nome-transp
  FIELD mercado           AS CHAR FORMAT "x(1)"
  FIELD nat-operacao      LIKE ped-venda.nat-operacao
  FIELD cod-sit-aval      LIKE ped-venda.cod-sit-aval
  FIELD cidade-cif        LIKE ped-venda.cidade-cif 
  FIELD vl-liq-it         like ped-item.vl-liq-it
    INDEX it-codigo /* IS UNIQUE PRIMARY */
          it-codigo ASCENDING
    INDEX cod-emitente IS PRIMARY
          cod-emitente ASCENDING.
 
DEF TEMP-TABLE tt-saldo NO-UNDO
    FIELD cod-estabel LIKE estabelec.cod-estabel
    FIELD it-codigo   LIKE ITEM.it-codigo
    FIELD saldo       AS decimal
    FIELD cod-refer   LIKE SALDO-ESTOQ.cod-refer  
    INDEX Codigo IS UNIQUE PRIMARY
        cod-estabel ASCENDING
        it-codigo   ASCENDING
        cod-refer   ascending.

DEF TEMP-TABLE tt-transfer NO-UNDO
    FIELD cod-estabel  LIKE estabelec.cod-estabel
    FIELD it-codigo    LIKE ITEM.it-codigo
    field cod-refer    like saldo-terc.cod-refer   
    FIELD cod-emitente LIKE saldo-terc.cod-emitente
    FIELD saldo        AS decimal
    INDEX Codigo IS UNIQUE PRIMARY
        cod-estabel    ASCENDING
        it-codigo      ASCENDING
        cod-refer      ASCENDING
        cod-emitente   ASCENDING 
    INDEX Item
        it-codigo      ASCENDING
        cod-refer      ASCENDING.

DEF TEMP-TABLE tt-item NO-UNDO
    FIELD cod-estabel LIKE estabelec.cod-estabel
    FIELD cod-depos   LIKE saldo-estoq.cod-depos
    FIELD cod-localiz LIKE saldo-estoq.cod-localiz
    FIELD lote        LIKE saldo-estoq.lote
    FIELD cod-refer   LIKE saldo-estoq.cod-refer   
    FIELD it-codigo   LIKE ITEM.it-codigo
    FIELD saldo       AS decimal
    INDEX Codigo IS UNIQUE PRIMARY
        cod-estabel ASCENDING
        cod-depos   ASCENDING
        cod-refer   ASCENDING
        cod-localiz ASCENDING
        lote        ASCENDING
        it-codigo   ASCENDING.

DEFINE VARIABLE c-mercado   AS CHARACTER   NO-UNDO.
DEF VAR i-linha             AS INTEGER              NO-UNDO.
DEF VAR i-coluna            AS INTEGER              NO-UNDO.
DEF VAR i-coluna-pd           AS INTEGER              NO-UNDO.
DEF VAR de-espessura        AS CHARACTER            NO-UNDO.
DEF VAR de-saldo            AS DECIMAL              NO-UNDO.
DEF VAR de-resto            AS DECIMAL              NO-UNDO.
DEF VAR de-saldo-trans      AS DECIMAL              NO-UNDO.
DEF VAR de-resto-trans      AS DECIMAL              NO-UNDO.
DEF VAR de-peso-item        AS DECIMAL              NO-UNDO.
DEF VAR de-peso-expedir     AS DECIMAL              NO-UNDO.
DEF VAR de-peso-produzir    AS DECIMAL              NO-UNDO.
DEF VAR c-saldo-item        AS CHAR FORMAT "x(20)"  NO-UNDO.
DEFINE VARIABLE saldo-kg    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE flag-conta  AS int        NO-UNDO.
DEFINE VARIABLE SALDO-ATR-BA  AS int        NO-UNDO.
DEFINE VARIABLE i-nr-pedido AS INTEGER     NO-UNDO.

def var v-it-codigo-ini like item.it-codigo.
def var v-it-codigo-fin like item.it-codigo init "ZZZZZZZZZZZZZZZZ".

def var tot-cli-1   as dec format "->>>,>>>,>>9".
def var v-numemb    as int format ">>>".

def var v-salini-fil    as int.
def var v-salini-mat    as int.
def var v-qtped         like v-salini-mat.
def var v-saldo         like v-salini-mat.
def var v-atend         like v-salini-mat.
def var v-pend          like v-salini-mat.

def var v-titulo    as char format "x(35)".
def var pagina      as int.

def var tot-qtd-solic       as dec format "->>>,>>>,>>9".
def var tot-saldo-ped       as dec format "->>>,>>>,>>9".
def var tot-saldo-a-atend   as dec format "->>>,>>>,>>9".
def var tot-qtd-trans       as int format "->>>,>>>,>>9".
def var tot-sug-transf      as dec format "->>>,>>>,>>9".
def var tot-qtd-exped       as dec format "->>>,>>>,>>9".
def var tot-qtd-produz      as dec format "->>>,>>>,>>9".
def var tot-dias-peso       as dec format "->>>,>>>,>>9".
def var tot-peso            as dec format "->>>,>>>,>>9".
def var indicador-atraso    as dec format "->>>,>>>,>>9".
def var tot-dias-peso-81    as dec format "->>>,>>>,>>9".
def var tot-peso-81         as dec format "->>>,>>>,>>9".
def var indicador-atraso-81  as dec format "->>>,>>>,>>9".
def var tot-dias-peso-82     as dec format "->>>,>>>,>>9".
def var tot-peso-82          as dec format "->>>,>>>,>>9".
def var indicador-atraso-82  as dec format "->>>,>>>,>>9".

def var tot-9       as dec format ">>>,>>9.999" decimals 3.
def var tot-10      as dec format ">>>,>>9.999" decimals 3.
def var tot-11      as dec format "->>>,>>9.999" decimals 3.
def var tot-12      as int format ">>>,>>9-".
def var tot-13      as int format ">>>,>>9-".
def var tot-14      as int format ">>>,>>9-".
def var saldo-ped   as dec format ">>>,>>9.99" decimals 2.
DEF VAR de-valor-unit LIKE ped-item.vl-liq-it.

DEF BUFFER bf-componente    FOR componente.

DEF VAR v-quantidade    LIKE  componente.quantidade NO-UNDO.

DEF VAR desc-sit-aval AS CHAR FORMAT "x(25)"  EXTENT 5  INIT ["N∆o Avaliado", "Avaliado", "Aprovado", "N∆o Aprovado", "Pendente de Aprovaá∆o"].

def var h-acomp as handle no-undo.

def var stat as log.

DEF TEMP-TABLE tt-raw-digita 
    FIELD raw-digita AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FIND FIRST param-global NO-LOCK NO-ERROR.

FIND FIRST tt-param NO-LOCK NO-ERROR.

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "Imprimindo...").

FOR EACH temp-item.
  DELETE temp-item.
END.

FOR EACH tt-saldo:
    DELETE tt-saldo.
END.

FOR EACH tt-item:
    DELETE tt-item.
END.

FOR EACH tt-transfer:
    DELETE tt-transfer.
END.

FOR EACH ped-venda NO-LOCK
    WHERE ped-venda.nome-abrev  >= tt-param.nome-abrev-ini
    AND   ped-venda.nome-abrev  <= tt-param.nome-abrev-fim
    AND   ped-venda.nr-pedido   >= tt-param.nr-pedido-ini
    AND   ped-venda.nr-pedido   <= tt-param.nr-pedido-fim 
    AND   ped-venda.nr-pedcli   >= tt-param.nr-pedcli-ini
    AND   ped-venda.nr-pedcli   <= tt-param.nr-pedcli-fim 
    AND   ped-venda.cod-estabel >= tt-param.cod-estabel-ini 
    AND   ped-venda.cod-estabel <= tt-param.cod-estabel-fim 
    AND   ped-venda.dt-implant  >= tt-param.dt-implanta-ini
    AND   ped-venda.dt-implant  <= tt-param.dt-implanta-fim
    AND   ped-venda.cod-sit-ped < 4,
    FIRST natur-oper 
    WHERE natur-oper.nat-operacao = ped-venda.nat-operacao 
    AND   natur-oper.mercado      = tt-param.rs-mercado NO-LOCK:  /* 1-Interno  2-Externo */

    IF tt-param.dt-corte < ped-venda.dt-implant THEN NEXT. 

    FIND FIRST emitente WHERE
        emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.
       
    FOR EACH ped-item of ped-venda 
        WHERE (ped-item.cod-sit-item >= 1
        AND    ped-item.cod-sit-item <= 2)
     /* OR    (ped-item.cod-sit-item >= 4
        AND    ped-item.cod-sit-item <= 5) */ NO-LOCK,
        FIRST ITEM WHERE
            ITEM.it-codigo  = ped-item.it-codigo        AND
            ITEM.fm-codigo >= tt-param.fm-codigo-ini    AND
            ITEM.fm-codigo <= tt-param.fm-codigo-fim    NO-LOCK:

        IF  ped-item.it-codigo < tt-param.it-codigo-ini OR ped-item.it-codigo > tt-param.it-codigo-fim THEN
            NEXT.
            
        if  tt-param.tg-especial = 1 AND ped-item.cod-refer <> 'ESPECIAL' THEN 
            NEXT.

        if  tt-param.tg-especial = 2 AND ped-item.cod-refer =  'ESPECIAL' THEN 
            NEXT.

        RUN pi-acompanhar in h-acomp (input "Familia " + ITEM.fm-codigo + " Item:" + ITEM.it-codigo).

        FIND FIRST tt-saldo WHERE
            tt-saldo.it-codigo   = ped-item.it-codigo   AND
            tt-saldo.cod-refer   = ped-item.cod-refer   NO-ERROR.
    
        IF  NOT AVAIL tt-saldo THEN DO:
            FOR EACH saldo-estoq 
                WHERE saldo-estoq.it-codigo = item.it-codigo 
                AND (saldo-estoq.cod-depos  = cod-depos-1  
                OR   saldo-estoq.cod-depos  = cod-depos-2  
                OR   saldo-estoq.cod-depos  = cod-depos-3  
                OR   saldo-estoq.cod-depos  = cod-depos-4) 
                and  saldo-estoq.cod-refer  = ped-item.cod-refer NO-LOCK:
                
                IF  saldo-estoq.qtidade-atu = 0 THEN
                    NEXT.

                IF  tt-param.tg-especial = 1 AND 
                    saldo-estoq.cod-refer <> 'ESPECIAL' THEN NEXT.

                IF  tt-param.tg-especial = 2 AND 
                    saldo-estoq.cod-refer =  'ESPECIAL' THEN NEXT.
                
                FIND FIRST tt-saldo WHERE
                    tt-saldo.cod-estabel = saldo-estoq.cod-estabel AND
                    tt-saldo.it-codigo   = saldo-estoq.it-codigo   AND
                    tt-saldo.cod-refer   = saldo-estoq.cod-refer   NO-ERROR.

                IF  NOT AVAIL tt-saldo THEN DO:
                    CREATE tt-saldo.
                    ASSIGN tt-saldo.cod-estabel = saldo-estoq.cod-estabel
                           tt-saldo.it-codigo   = saldo-estoq.it-codigo
                           tt-saldo.cod-refer   = saldo-estoq.cod-refer.
                END.

                ASSIGN tt-saldo.saldo = tt-saldo.saldo + saldo-estoq.qtidade-atu.
    
                FIND FIRST tt-item WHERE
                    tt-item.cod-estabel = saldo-estoq.cod-estabel AND
                    tt-item.cod-depos   = saldo-estoq.cod-depos   AND
                    tt-item.cod-localiz = saldo-estoq.cod-localiz AND
                    tt-item.lote        = saldo-estoq.lote        AND
                    tt-item.it-codigo   = saldo-estoq.it-codigo   AND
                    tt-item.cod-refer   = saldo-estoq.cod-refer   NO-ERROR.
                IF  NOT AVAIL tt-item THEN DO:
                    CREATE tt-item.
                    ASSIGN tt-item.cod-estabel = saldo-estoq.cod-estabel
                           tt-item.cod-depos   = saldo-estoq.cod-depos  
                           tt-item.cod-localiz = saldo-estoq.cod-localiz
                           tt-item.lote        = saldo-estoq.lote 
                           tt-item.cod-refer   = saldo-estoq.cod-refer   
                           tt-item.it-codigo   = saldo-estoq.it-codigo. 
                END.
                ASSIGN tt-item.saldo = tt-item.saldo + saldo-estoq.qtidade-atu.
    
            END.
        END. /* IF  NOT AVAIL tt-saldo THEN DO: */
           
/*         IF  AVAIL tt-saldo THEN                                   */
/*             MESSAGE 'tt-saldo.item      ' tt-saldo.it-codigo SKIP */
/*                     'tt-saldo.saldo     ' tt-saldo.saldo     SKIP */
/*                     'tt-saldo.cod-refer ' tt-saldo.cod-refer      */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK.                */

        FIND FIRST tt-transfer WHERE
         /* tt-transfer.cod-estabel   = ped-venda.cod-estabel  AND */
            tt-transfer.it-codigo     = ped-item.it-codigo     AND
            tt-transfer.cod-refer     = ped-item.cod-refer     NO-ERROR.

        IF  NOT AVAIL tt-transfer THEN DO:

            FOR EACH componente NO-LOCK
                WHERE componente.it-codigo = ped-item.it-codigo
                AND   componente.cod-refer = ped-item.cod-refer:

                IF  componente.componente = 2 THEN
                    NEXT.

                FIND FIRST saldo-terc WHERE 
                    saldo-terc.serie-docto    = componente.serie-docto  AND
                    saldo-terc.nro-docto      = componente.nro-docto    AND
                    saldo-terc.cod-emitente   = componente.cod-emitente AND
                    saldo-terc.nat-operacao   = componente.nat-operacao AND
                    saldo-terc.it-codigo      = componente.it-codigo    AND
                    saldo-terc.cod-refer      = componente.cod-refer    AND
                    saldo-terc.sequencia      = componente.sequencia    NO-LOCK NO-ERROR.

                IF  NOT AVAIL saldo-terc THEN
                    NEXT.

                IF  saldo-terc.quantidade = 0 THEN
                    NEXT.

                IF  saldo-terc.tipo-sal-terc <> 3 THEN /* transferencia */
                    NEXT.

                ASSIGN v-quantidade = componente.quantidade.

                /* NOTAS DE RETORNO - Notas filhas */
                FOR EACH bf-componente NO-LOCK
                    WHERE bf-componente.serie-comp   = componente.serie-docto 
                    AND   bf-componente.nro-comp     = componente.nro-docto   
                    AND   bf-componente.nat-comp     = componente.nat-operacao
                    AND   bf-componente.cod-emitente = componente.cod-emitente
                    AND   bf-componente.seq-comp     = componente.sequencia   
                    AND   bf-componente.it-codigo    = componente.it-codigo:
                    ASSIGN v-quantidade     = v-quantidade  - bf-componente.quantidade.
                END.

                FIND FIRST tt-transfer WHERE
                    tt-transfer.cod-estabel  = saldo-terc.cod-estabel    AND
                    tt-transfer.it-codigo    = saldo-terc.it-codigo      AND
                    tt-transfer.cod-refer    = saldo-terc.cod-refer      /* AND
                    tt-transfer.cod-emitente = saldo-terc.cod-emitente   */ NO-ERROR.
                IF  NOT AVAIL tt-transfer THEN DO:
                    CREATE tt-transfer.
                    ASSIGN tt-transfer.cod-estabel  = saldo-terc.cod-estabel
                           tt-transfer.it-codigo    = saldo-terc.it-codigo
                           tt-transfer.cod-refer    = saldo-terc.cod-refer 
                           tt-transfer.cod-emitente = saldo-terc.emite-comp. /* saldo-terc.cod-emitente. */
                END.

                ASSIGN tt-transfer.saldo = tt-transfer.saldo + v-quantidade. /* saldo-terc.quantidade. */

            END. /* FOR EACH componente NO-LOCK */

            /**************** Substituido pelo bloco acima em 04/04/20008 por Ercole
            
            FOR EACH saldo-terc NO-LOCK
                WHERE saldo-terc.it-codigo     = ped-item.it-codigo
                AND   saldo-terc.cod-refer     = ped-item.cod-refer:

                IF  saldo-terc.tipo-sal-terc <> 3 THEN
                    NEXT.

                IF  saldo-terc.quantidade = 0 THEN
                    NEXT.

                FIND FIRST componente WHERE 
                    componente.serie-docto    = saldo-terc.serie-docto  AND
                    componente.nro-docto      = saldo-terc.nro-docto    AND
                    componente.cod-emitente   = saldo-terc.cod-emitente AND
                    componente.nat-operacao   = saldo-terc.nat-operacao AND
                    componente.it-codigo      = saldo-terc.it-codigo    AND
                    componente.cod-refer      = saldo-terc.cod-refer    AND
                    componente.sequencia      = saldo-terc.sequencia    NO-LOCK NO-ERROR.

                IF  NOT AVAIL componente THEN
                    NEXT.

                ASSIGN v-quantidade = componente.quantidade.

                /* NOTAS DE RETORNO - Notas filhas */
                FOR EACH bf-componente NO-LOCK
                    WHERE bf-componente.serie-comp   = componente.serie-docto 
                    AND   bf-componente.nro-comp     = componente.nro-docto   
                    AND   bf-componente.nat-comp     = componente.nat-operacao
                    AND   bf-componente.cod-emitente = componente.cod-emitente
                    AND   bf-componente.seq-comp     = componente.sequencia   
                    AND   bf-componente.it-codigo    = componente.it-codigo:
                    ASSIGN v-quantidade     = v-quantidade  - bf-componente.quantidade.
                END.

                FIND FIRST tt-transfer WHERE
                    tt-transfer.cod-estabel  = saldo-terc.cod-estabel    AND
                    tt-transfer.it-codigo    = saldo-terc.it-codigo      AND
                    tt-transfer.cod-refer    = saldo-terc.cod-refer      /* AND
                    tt-transfer.cod-emitente = saldo-terc.cod-emitente   */ NO-ERROR.
                IF  NOT AVAIL tt-transfer THEN DO:
                    CREATE tt-transfer.
                    ASSIGN tt-transfer.cod-estabel  = saldo-terc.cod-estabel
                           tt-transfer.it-codigo    = saldo-terc.it-codigo
                           tt-transfer.cod-refer    = saldo-terc.cod-refer 
                           tt-transfer.cod-emitente = saldo-terc.cod-emitente.
                END.

                ASSIGN tt-transfer.saldo = tt-transfer.saldo + v-quantidade. /* saldo-terc.quantidade. */

            END.
            *****************************************************/
            
/*             IF  AVAIL tt-transfer THEN                          */
/*             MESSAGE 'tt-transfer.saldo ' tt-transfer.saldo SKIP */
/*                     'v-quantidade      ' v-quantidade SKIP      */
/*                     'est ' tt-transfer.cod-estabel SKIP         */
/*                     'item' tt-transfer.it-codigo   SKIP         */
/*                     'refe' tt-transfer.cod-refer   SKIP         */
/*                     'emit' tt-transfer.cod-emitente             */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK.              */

        END.  /* IF  NOT AVAIL tt-transfer THEN DO: */

        CREATE temp-item.
        ASSIGN temp-item.it-codigo      = ped-item.it-codigo
               temp-item.desc-item      = ITEM.desc-item /* 04.01.2011 - inclus∆o da descriá∆o */
               temp-item.cod-refer      = ped-item.cod-refer
               temp-item.nome-abrev     = ped-venda.nome-abrev
               temp-item.cod-emitente   = ped-venda.cod-emitente
               temp-item.nr-pedcli      = ped-item.nr-pedcli
               temp-item.nr-pedido      = ped-venda.nr-pedido
               temp-item.cod-priori     = ped-venda.cod-priori
               temp-item.qtd-pedida     = ped-item.qt-pedida
               temp-item.qtd-atend      = ped-item.qt-atend
               temp-item.qtd-saldo      = (ped-item.qt-pedida - ped-item.qt-atend)
               temp-item.nat-operacao   = ped-item.nat-operacao
               temp-item.qtd-estoq      = 0
               temp-item.qtd-pend       = 0
               temp-item.qtd-transfer   = 0
               temp-item.qtd-transito   = 0
               temp-item.nome-tr-red    = ped-venda.nome-tr-red
               temp-item.cidade-cif     = ped-venda.cidade-cif 
               temp-item.nome-transp    = ped-venda.nome-transp
               temp-item.dt-entrega     = ped-item.dt-entrega /* ped-venda.dt-entrega */
               temp-item.dt-exp-bahia   = ped-item.dt-entorig 
               temp-item.dt-pedido      = ped-venda.dt-implant
               temp-item.cod-estabel    = ped-venda.cod-estabel
               temp-item.cod-sit-aval   = ped-venda.cod-sit-aval
               temp-item.vl-liq-it      = ped-item.vl-liq-it.


        ASSIGN i-nr-pedido = 0.
        FIND FIRST espmulti.if-ped-venda NO-LOCK
             WHERE espmulti.if-ped-venda.nr-pedido         = ped-venda.nr-pedido
               AND espmulti.if-ped-venda.nr-pedido-relac  <> 0 NO-ERROR.
        IF AVAIL espmulti.if-ped-venda THEN DO:
    
            DO WHILE AVAIL espmulti.if-ped-venda:
                FIND FIRST b-ped-venda EXCLUSIVE-LOCK
                     WHERE b-ped-venda.nr-pedido = espmulti.if-ped-venda.nr-pedido-relac NO-ERROR.

                IF NOT AVAIL b-ped-venda THEN
                    LEAVE.
    
                FIND FIRST espmulti.if-ped-venda NO-LOCK
                     WHERE espmulti.if-ped-venda.nr-pedido = b-ped-venda.nr-pedido NO-ERROR.
                
                ASSIGN i-nr-pedido = b-ped-venda.nr-pedido.
            END.
        END.
    
        IF i-nr-pedido > 0 THEN DO:
            FIND FIRST b-ped-venda
                 WHERE b-ped-venda.nr-pedido = i-nr-pedido NO-LOCK NO-ERROR.
            IF AVAIL b-ped-venda THEN DO:

                ASSIGN temp-item.nome-abrev        = b-ped-venda.nome-abrev
                       temp-item.cod-emitente      = b-ped-venda.cod-emitente
                       temp-item.nr-pedido         = b-ped-venda.nr-pedido
                       temp-item.cod-sit-aval      = b-ped-venda.cod-sit-aval
                       temp-item.nome-ab-ung-com   = ped-venda.nome-abrev
                       temp-item.nr-pedido-ung-com = ped-venda.nr-pedido. 
            END.
        END.

    END. /* FOR EACH ped-item of ped-venda */
    
END.  
/*
OUTPUT TO VALUE("v:\temp\lista.txt") /*c:/tmp/lista.txt*/ NO-CONVERT. /*03.01.2011 */
FOR EACH temp-item NO-LOCK
    BREAK BY temp-item.dt-entrega
          BY temp-item.dt-pedido
          BY temp-item.cod-estabel
          BY temp-item.it-codigo
          BY temp-item.nr-pedido:

    DISP temp-item.dt-entrega
         temp-item.dt-pedido
         temp-item.cod-estabel
temp-item.it-codigo      
temp-item.cod-refer      
temp-item.nome-abrev     
temp-item.cod-emitente   
temp-item.nr-pedcli      
temp-item.nr-pedido      
temp-item.cod-priori     
temp-item.qtd-pedida     
temp-item.qtd-atend      
temp-item.qtd-saldo
temp-item.nat-operacao   
        WITH DOWN WIDTH 300 FRAME f-1.
    DOWN WITH FRAME f-1.

END.
OUTPUT CLOSE.

OUTPUT TO VALUE("v:\temp\Saldo_item.txt") /*c:/tmp/Saldo_item.txt*/ NO-CONVERT. /* 03.01.2010 */
FOR EACH tt-item.
                    
    DISP tt-item.cod-estabel 
         tt-item.cod-depos   
         tt-item.cod-localiz 
         tt-item.lote      
         tt-item.cod-refer 
         tt-item.it-codigo 
         tt-item.saldo
        WITH DOWN WIDTH 300 FRAME f-2.
    DOWN WITH FRAME f-2.

END.
OUTPUT CLOSE.

OUTPUT TO VALUE("v:\temp\Saldo_Trans.txt") /*c:/tmp/Saldo_Trans.txt*/ NO-CONVERT. /* 03.01.2011 */
FOR EACH tt-transfer.
    DISP tt-transfer.cod-estabel  
         tt-transfer.it-codigo    
         tt-transfer.cod-refer    
         tt-transfer.cod-emitente 
         tt-transfer.saldo
        WITH DOWN WIDTH 300 FRAME f-3.
    DOWN WITH FRAME f-3.

END.
OUTPUT CLOSE.
*/


FOR EACH temp-item NO-LOCK
    BREAK BY temp-item.dt-entrega
          BY temp-item.dt-pedido
          BY temp-item.cod-estabel
          BY temp-item.it-codigo
          BY temp-item.nr-pedido:

    FIND emitente WHERE
        emitente.cod-emitente = temp-item.cod-emitente NO-LOCK NO-ERROR.

    ASSIGN de-saldo = 0.

    FIND FIRST tt-saldo WHERE
        tt-saldo.cod-estabel = temp-item.cod-estabel AND
        tt-saldo.it-codigo   = temp-item.it-codigo   AND 
        tt-saldo.cod-refer   = temp-item.cod-refer   NO-ERROR.
    IF  AVAIL tt-saldo THEN DO:
        IF  tt-saldo.saldo >= temp-item.qtd-saldo THEN
            ASSIGN de-saldo       = temp-item.qtd-saldo
                   tt-saldo.saldo = tt-saldo.saldo - temp-item.qtd-saldo. 
        ELSE
            ASSIGN de-saldo       = tt-saldo.saldo
                   tt-saldo.saldo = 0. 
    END.
    
    ASSIGN temp-item.qtd-estoq = de-saldo.
    
    ASSIGN de-saldo-trans = 0
           de-resto-trans = 0.

    /** Verifica se existe saldo em transito de outro estabelecimeto   **/

    IF  temp-item.cod-estabel = '382' OR temp-item.cod-estabel = '432' OR temp-item.cod-estabel = '443' OR temp-item.cod-estabel = '436' THEN DO: /*solic-318*/ 

/*         FOR EACH tt-transfer                                                    */
/*             WHERE tt-transfer.cod-estabel  <> temp-item.cod-estabel             */
/*             AND   tt-transfer.it-codigo     = temp-item.it-codigo               */
/*             AND   tt-transfer.cod-refer     = temp-item.cod-refer               */
/*             AND   tt-transfer.saldo         > 0 NO-LOCK:                        */
/*                                                                                 */
/*             FIND estabelec WHERE                                                */
/*                 estabelec.cod-estabel = temp-item.cod-estabel NO-LOCK NO-ERROR. */
/*             IF  estabelec.cod-emitente <> tt-transfer.cod-emitente THEN         */
/*                 NEXT.                                                           */
/*                                                                                 */
/*             ASSIGN de-saldo-trans = de-saldo-trans + tt-transfer.saldo.         */
/*                                                                                 */
/*         END.                                                                    */

        IF  (temp-item.qtd-saldo - de-saldo) > 0 THEN DO:

            ASSIGN de-saldo-trans = (temp-item.qtd-saldo - de-saldo)                      
                   de-resto-trans = de-saldo-trans.                                                              
                                                                                                                 
            FOR EACH tt-transfer                                                                                 
                WHERE tt-transfer.cod-estabel  <> temp-item.cod-estabel /* Estabelecimento do Pedido de Venda */
                AND   tt-transfer.it-codigo     = temp-item.it-codigo                                            
                AND   tt-transfer.cod-refer     = temp-item.cod-refer  
                AND   tt-transfer.saldo         > 0 NO-LOCK:                                                     
                                                                                                                 
                FIND estabelec WHERE                                                                             
                     estabelec.cod-estabel = temp-item.cod-estabel NO-LOCK NO-ERROR.                              
    
                IF  estabelec.cod-emitente <> tt-transfer.cod-emitente THEN  NEXT.                                                                                        
                                                                                                                 
                IF  tt-transfer.saldo >= de-resto-trans THEN                                                     
                    ASSIGN tt-transfer.saldo = tt-transfer.saldo - de-resto-trans                                
                           de-resto-trans    = 0.                                                                
                ELSE                                                                                             
                    ASSIGN de-resto-trans    = de-resto-trans - tt-transfer.saldo                                
                           tt-transfer.saldo = 0.                                                                
                                                                                                                 
                    /* MESSAGE  " tt-transfer.it-codigo " tt-transfer.it-codigo SKIP
                                    " tt-transfer.cod-estabel " tt-transfer.cod-estabel SKIP
                                    "tt-item.cod-estabel "  tt-item.cod-estabel SKIP
                                    "tt-transfer.cod-refer "  tt-transfer.cod-refer SKIP
                                    " tt-transfer.quantidade " tt-transfer.saldo SKIP
                                    "de-saldo-trans " de-saldo-trans
                        VIEW-AS ALERT-BOX INFO BUTTONS OK. */
                IF  de-resto-trans = 0 THEN                                                                      
                    LEAVE.                                                                                       
                                                                                                                 
            END.                                                                                                 
                                                                                                             
            ASSIGN de-saldo-trans = de-saldo-trans - de-resto-trans.                                             
                                                                                                             
        END.                                                                                                     
        ELSE                                                                                                     
            ASSIGN de-saldo-trans = 0.                                                                           

        ASSIGN de-saldo = de-saldo + de-saldo-trans.

        ASSIGN temp-item.qtd-transito = de-saldo-trans.
    
        IF  (temp-item.qtd-saldo > (temp-item.qtd-estoq + temp-item.qtd-transito)) THEN DO:

           FIND FIRST tt-saldo WHERE
                tt-saldo.cod-estabel <> temp-item.cod-estabel   AND
                tt-saldo.it-codigo    = temp-item.it-codigo     AND 
                tt-saldo.cod-refer    = temp-item.cod-refer     NO-ERROR.
           IF  AVAIL tt-saldo THEN DO:
               IF  tt-saldo.saldo >= (temp-item.qtd-saldo - temp-item.qtd-estoq  - temp-item.qtd-transito) THEN
                   ASSIGN temp-item.qtd-transfer = (temp-item.qtd-saldo - temp-item.qtd-estoq  - temp-item.qtd-transito)
                          tt-saldo.saldo         = tt-saldo.saldo - temp-item.qtd-transfer. 
               ELSE
                   ASSIGN temp-item.qtd-transfer = tt-saldo.saldo
                          tt-saldo.saldo         = 0. 
           END.
    
        END.
    
    END. /* IF  temp-item.cod-estabel = '382' THEN DO: */

    ASSIGN temp-item.qtd-pend = (temp-item.qtd-saldo - temp-item.qtd-estoq  - temp-item.qtd-transito - temp-item.qtd-transfer).

END.

/************************************************************************/
{utp/ut-glob.i} 

{utp/utapi008.i}

/* *** Deletar Tabelas Tempor†rias *** */

for each tt-configuracao:
    delete tt-configuracao.
end.

for each tt-dados:
    delete tt-dados.
end.

IF  NOT tt-param.tg-localiza THEN DO: /* 03.01.2011 - antes estava buscando do 'I:\' */
    OS-COPY value(SEARCH("modelos\MODELO0008.XLSm"))  VALUE(tt-param.arquivo-excel).
    ASSIGN i-coluna-pd = 28. /* antes era 28 - 05/11/2011 */
END.
ELSE DO:
   OS-COPY value(SEARCH("modelos\MODELO0008a.XLSm")) VALUE(tt-param.arquivo-excel).
   ASSIGN i-coluna-pd = 29. /* antes era 27 - 05/11/2011 */
END.
    
/* IF  not tt-param.tg-localiza THEN DO:                                                            */
/*     OS-COPY value('I:\PRG_Ems204\DSV\modelos\MODELO0008.XLS')  VALUE(tt-param.arquivo-excel). */
/*     ASSIGN i-coluna-pd = 28.                                                                     */
/* END.                                                                                             */
/* ELSE DO:                                                                                         */
/*    OS-COPY value('I:\PRG_Ems204\DSV\modelos\MODELO0008A.XLS') VALUE(tt-param.arquivo-excel).  */
/*    ASSIGN i-coluna-pd = 27.                                                                      */
/* END.                                                                                             */
    
create tt-configuracao.
assign tt-configuracao.versao-integracao   = 001
       tt-configuracao.arquivo-num         = 01
       tt-configuracao.arquivo             = tt-param.arquivo-excel
       tt-configuracao.abrir-excel-termino = yes.

ASSIGN i-linha          = 5
       i-coluna         = 0
       de-peso-expedir  = 0
       de-peso-produzir = 0.

FOR EACH temp-item NO-LOCK
    BREAK BY temp-item.dt-entrega
          BY temp-item.dt-pedido
          BY temp-item.cod-estabel
          BY temp-item.it-codigo
          BY temp-item.nr-pedcli 
          /* BY temp-item.nr-pedido */: /*** Solicitado pela Expediá∆o trocar o Nr. do Pedido pelo Pedido Cliente ***/

    IF  tt-param.Rs-Pedido = 1 AND 
        temp-item.qtd-pend = 0 THEN  NEXT. 

    FIND emitente WHERE 
         emitente.cod-emitente = temp-item.cod-emitente NO-LOCK NO-ERROR.

    find first item where
               item.it-codigo = temp-item.it-codigo no-lock no-error.
    if not avail item then  next.
  
    ASSIGN i-linha = i-linha + 1.
    ASSIGN i-coluna = i-coluna + 1.

    assign saldo-ped =  temp-item.qtd-pedida - temp-item.qtd-atend.
    
    assign tot-qtd-solic      = tot-qtd-solic      + temp-item.qtd-pedida 
           tot-saldo-ped      = tot-saldo-ped      + saldo-ped
           tot-saldo-a-atend  = tot-saldo-a-atend  + (temp-item.qtd-estoq * ITEM.peso-liquido)
           tot-qtd-trans      = tot-qtd-trans      + temp-item.qtd-transito
           tot-sug-transf     = tot-sug-transf     + temp-item.qtd-transfer
           tot-qtd-exped      = tot-qtd-exped      + temp-item.qtd-estoq
           tot-qtd-produz     = tot-qtd-produz     + temp-item.qtd-pend. 
            
/*     do i-coluna = 2 to 14: */
/*           create tt-dados. */
/*           assign tt-dados.arquivo-num        = 1 */
/*                  tt-dados.planilha-num       = 01 */
/*                  tt-dados.celula-coluna      = i-coluna */
/*                  tt-dados.celula-linha       = i-linha */
/*                  tt-dados.celula-formula     = "" */
/*                  tt-dados.retorna-valor      = no. */
/*    */
/*           case i-coluna: */
/*               when 2 then */
/*                  ASSIGN tt-dados.celula-valor = temp-item.cod-estabel. */
/*           END CASE. */
/*     END. */
          
            
    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = temp-item.cod-estabel 
           tt-dados.retorna-valor      = no.   

    ASSIGN i-coluna = i-coluna + 1.

    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = emitente.estado 
           tt-dados.retorna-valor      = no.   

    ASSIGN i-coluna = i-coluna + 1.
    
    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna 
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = emitente.nome-abrev 
           tt-dados.retorna-valor      = no.   

    ASSIGN i-coluna = i-coluna + 1.

    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = STRING(temp-item.nr-pedcli) + "." /*** Solicitado pela Expediá∆o trocar o Nr. do Pedido pelo Pedido Cliente ***/
           tt-dados.retorna-valor      = no.
    
    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna-pd
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = STRING(temp-item.nr-pedido) + "." /*** Solicitado para incluir numero do pedido no final da planilha***/
           tt-dados.retorna-valor      = no.

    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = (i-coluna-pd + 1)
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = temp-item.cidade-cif 
           tt-dados.retorna-valor      = no.

    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = (i-coluna-pd + 2)
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = temp-item.nome-tr-red  
           tt-dados.retorna-valor      = no.

    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = (i-coluna-pd + 3)
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = temp-item.nome-ab-ung-com
           tt-dados.retorna-valor      = no.

    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = (i-coluna-pd + 4)
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = string(temp-item.nr-pedido-ung-com)
           tt-dados.retorna-valor      = no.

    ASSIGN i-coluna = i-coluna + 1.

/*     if (temp-item.nat-operacao = "613") then do: */
    if (temp-item.nat-operacao = "6124") then do:
        create tt-dados.
        assign tt-dados.arquivo-num        = 1
               tt-dados.planilha-num       = 01
               tt-dados.celula-coluna      = i-coluna
               tt-dados.celula-linha       = i-linha
               tt-dados.celula-formula     = ""
               tt-dados.celula-valor       = "TOLLING" 
               tt-dados.retorna-valor      = no.   
    end.
    else do:
        create tt-dados.
        assign tt-dados.arquivo-num        = 1
               tt-dados.planilha-num       = 01
               tt-dados.celula-coluna      = i-coluna
               tt-dados.celula-linha       = i-linha
               tt-dados.celula-formula     = ""
               tt-dados.celula-valor       = " " 
               tt-dados.retorna-valor      = no.   
    end.


    ASSIGN i-coluna = i-coluna + 1.
         
    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = STRING(temp-item.dt-pedido,'99/99/9999')
           tt-dados.retorna-valor      = no.   
    
    ASSIGN i-coluna = i-coluna + 1.

    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = STRING(temp-item.dt-entrega,'99/99/9999')
           tt-dados.retorna-valor      = no.   
           
    
    ASSIGN i-coluna = i-coluna + 1.
    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = STRING(temp-item.dt-exp-bahia,'99/99/9999')
           tt-dados.retorna-valor      = no.   

    ASSIGN i-coluna = i-coluna + 1.

    ASSIGN SALDO-ATR-BA = 0.
    if (tt-param.dt-corte - temp-item.dt-exp-bahia) > 0 then do:
        find first ped-venda-ext where
                  /* ped-venda-ext.cod-estabel = temp-item.cod-estabel and */
                   ped-venda-ext.nome-abrev  = temp-item.nome-abrev  and
                   ped-venda-ext.nr-pedcli   = temp-item.nr-pedcli   no-lock no-error.
        if avail ped-venda-ext           and
           ped-venda-ext.Bloqueado = yes then .
        else do:   
           IF  (temp-item.cod-estabel = "382" OR temp-item.cod-estabel = '384' OR temp-item.cod-estabel = '432' OR temp-item.cod-estabel = '443' OR temp-item.cod-estabel = '436') THEN DO: /*solic-318*/ 
               if  saldo-ped > (temp-item.qtd-estoq + temp-item.qtd-transito ) then do:
                   create tt-dados.
                   assign tt-dados.arquivo-num        = 1
                          tt-dados.planilha-num       = 01
                          tt-dados.celula-coluna      = i-coluna
                          tt-dados.celula-linha       = i-linha
                          tt-dados.celula-formula     = ""
                          tt-dados.celula-valor       = STRING(tt-param.dt-corte - temp-item.dt-exp-bahia)
                          tt-dados.retorna-valor      = no.   
                   ASSIGN SALDO-ATR-BA = tt-param.dt-corte - temp-item.dt-exp-bahia.        
               end.
          end.
          else do:
              IF temp-item.nome-transp = "emitente"   or 
                 temp-item.nome-transp = "redespacho" THEN DO:
                 create tt-dados.
                 assign tt-dados.arquivo-num        = 1
                        tt-dados.planilha-num       = 01
                        tt-dados.celula-coluna      = i-coluna
                        tt-dados.celula-linha       = i-linha
                        tt-dados.celula-formula     = ""
                        tt-dados.celula-valor       = STRING(tt-param.dt-corte - temp-item.dt-exp-bahia)
                        tt-dados.retorna-valor      = no.   
                   ASSIGN SALDO-ATR-BA = tt-param.dt-corte - temp-item.dt-exp-bahia.        
              end.           
              else do.               
                   if  saldo-ped > temp-item.qtd-estoq then do:
                        create tt-dados.
                        assign tt-dados.arquivo-num        = 1
                               tt-dados.planilha-num       = 01
                               tt-dados.celula-coluna      = i-coluna
                               tt-dados.celula-linha       = i-linha
                               tt-dados.celula-formula     = ""
                               tt-dados.celula-valor       = STRING(tt-param.dt-corte - temp-item.dt-entrega)
                               tt-dados.retorna-valor      = no.   
                        ASSIGN SALDO-ATR-BA = tt-param.dt-corte - temp-item.dt-exp-bahia.        
                        assign flag-conta = 1.       
                   end.               
               end.   
          end.           
       end.
    end.       

    ASSIGN i-coluna = i-coluna + 1.
    ASSIGN saldo-kg = saldo-ped * ITEM.peso-liquido.
    assign flag-conta = 0.       

    if length(emitente.estado) < 3 then do.
        IF saldo-ped > 0 THEN DO:
           find first ped-venda-ext where
                     /* ped-venda-ext.cod-estabel = temp-item.cod-estabel and */
                      ped-venda-ext.nome-abrev  = temp-item.nome-abrev  and
                      ped-venda-ext.nr-pedcli   = temp-item.nr-pedcli   no-lock no-error.
           if avail ped-venda-ext           and
              ped-venda-ext.Bloqueado = yes then .
           else do:   
                IF temp-item.nome-transp = "emitente"   or 
                   temp-item.nome-transp = "redespacho" THEN DO:
                        create tt-dados.
                        assign tt-dados.arquivo-num        = 1
                               tt-dados.planilha-num       = 01
                               tt-dados.celula-coluna      = i-coluna
                               tt-dados.celula-linha       = i-linha
                               tt-dados.celula-formula     = ""
                               tt-dados.celula-valor       = STRING(tt-param.dt-corte - temp-item.dt-entrega)
                               tt-dados.retorna-valor      = no.   
                        assign flag-conta = 1.       
                end.
                else do.               
                   if  saldo-ped > temp-item.qtd-estoq then do:
                        create tt-dados.
                        assign tt-dados.arquivo-num        = 1
                               tt-dados.planilha-num       = 01
                               tt-dados.celula-coluna      = i-coluna
                               tt-dados.celula-linha       = i-linha
                               tt-dados.celula-formula     = ""
                               tt-dados.celula-valor       = STRING(tt-param.dt-corte - temp-item.dt-entrega)
                               tt-dados.retorna-valor      = no.   
                        assign flag-conta = 1.       
                   end.               
                end.   
                if flag-conta = 1 then do:
                    if  (tt-param.dt-corte - temp-item.dt-entrega) > 0 then do:      
                        if temp-item.cod-estabel = "381" then 
                            assign tot-peso-81      = tot-peso-81      + saldo-kg
                                   tot-dias-peso-81 = tot-dias-peso-81 + (saldo-kg * (tt-param.dt-corte - temp-item.dt-entrega)).
                        else DO:
                            IF SALDO-ATR-BA = 0 THEN
                               assign tot-peso-82      = tot-peso-82      + saldo-kg
                                      tot-dias-peso-82 = tot-dias-peso-82 + (saldo-kg * (tt-param.dt-corte - temp-item.dt-entrega)).
                        END.              
                                   
                        assign tot-peso      = tot-peso      + saldo-kg
                               tot-dias-peso = tot-dias-peso + (saldo-kg * (tt-param.dt-corte - temp-item.dt-entrega)).
                    end.    
                END.
            end.
        END.
    end.
    
    ASSIGN i-coluna = i-coluna + 1.

    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = temp-item.it-codigo
           tt-dados.retorna-valor      = no.   
    
    ASSIGN i-coluna = i-coluna + 1.

    /* -> 04.01.2011 - exclusao da cor/espessura/dimensao e inclus∆o da descriá∆o do item */
    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = temp-item.desc-item
           tt-dados.retorna-valor      = no.  

    ASSIGN i-coluna = i-coluna + 1.
   /*
    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = SUBSTRING(temp-item.it-codigo,1,4)
           tt-dados.retorna-valor      = no.   
    
    ASSIGN i-coluna = i-coluna + 1.

    ASSIGN de-espessura = SUBSTRING(temp-item.it-codigo,5,2) + ',' + SUBSTRING(temp-item.it-codigo,7,2).
       
    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = DE-ESPESSURA /* STRING(de-espessura,'99.99') */
           tt-dados.retorna-valor      = no.   
    
    ASSIGN i-coluna = i-coluna + 1.
     
    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = SUBSTRING(temp-item.it-codigo,9,3) + 'X' + SUBSTRING(temp-item.it-codigo,12,5)
           tt-dados.retorna-valor      = no.   
    
    ASSIGN i-coluna = i-coluna + 1.
     */
    /* <- 04.01.2011 - exclusao da cor/espessura/dimensao e inclus∆o da descriá∆o do item */

    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = IF temp-item.cod-refer <> "" THEN STRING(temp-item.cod-refer)
                                         ELSE " "
           tt-dados.retorna-valor      = no.

    ASSIGN i-coluna = i-coluna + 1.

    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = string(temp-item.vl-liq-it,">>>,>>>,>>9.99")     
           tt-dados.retorna-valor      = no.
    
    ASSIGN i-coluna = i-coluna + 1.
    ASSIGN de-valor-unit   = temp-item.vl-liq-it / ( item.peso-liquido * temp-item.qtd-pedida).

    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = IF temp-item.it-codigo <> "" THEN STRING(de-valor-unit, ">>>,>>>,>>9.99999") ELSE " "
           tt-dados.retorna-valor      = no.   
    
    ASSIGN i-coluna = i-coluna + 1.


    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = string(temp-item.qtd-pedida,">>>,>>>,>>9.99")     
           tt-dados.retorna-valor      = no.   
    
    ASSIGN i-coluna = i-coluna + 1.
    
    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = string(saldo-ped,">>>,>>>,>>9.99")     
           tt-dados.retorna-valor      = no.   

    ASSIGN i-coluna = i-coluna + 1.

    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = STRING(saldo-kg,">>>,>>>,>>9.99")
        /* string(temp-item.qtd-estoq,">>>,>>>,>>9.99")      */
           tt-dados.retorna-valor      = no.   
           
    ASSIGN i-coluna = i-coluna + 1.

    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
/*            tt-dados.celula-valor       = IF  (temp-item.cod-estabel = '82' OR temp-item.cod-estabel = '84') THEN  */
/*                                             string(temp-item.qtd-transito,"->>,>>>,>>9.99") /* Qtd em transito */ */
/*                                          else                                                                     */
/*                                             ' '                                                                   */
           tt-dados.celula-valor       = IF  (temp-item.cod-estabel = '382' OR temp-item.cod-estabel = '432' OR temp-item.cod-estabel = '443' OR temp-item.cod-estabel = '436') THEN  /*solic-318*/ 
                                             string(temp-item.qtd-transito,"->>,>>>,>>9.99") /* Qtd em transito */
                                         else
                                             ' '
           tt-dados.retorna-valor      = no.   
    
    ASSIGN i-coluna = i-coluna + 1.

    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = string(temp-item.qtd-transfer,"->>,>>>,>>9.99") /* qtd transferencia */
           tt-dados.retorna-valor      = no.   

    ASSIGN i-coluna = i-coluna + 1.

    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = string(temp-item.qtd-estoq,"->>,>>>,>>9.99") /* Qtd a Expedir */
           tt-dados.retorna-valor      = no.   
    
    ASSIGN i-coluna = i-coluna + 1.
    ASSIGN de-peso-item    = item.peso-liquido * temp-item.qtd-estoq
           de-peso-expedir = de-peso-expedir   + de-peso-item.

    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = i-coluna
           tt-dados.celula-linha       = i-linha
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = string(de-peso-item,"->>,>>>,>>9.99") /* Kg a Expedir */
           tt-dados.retorna-valor      = no.   
    
    ASSIGN i-coluna = i-coluna + 1.
    
 /*   if temp-item.qtd-pend > 0 then do. */
        create tt-dados.
        assign tt-dados.arquivo-num        = 1
               tt-dados.planilha-num       = 01
               tt-dados.celula-coluna      = i-coluna
               tt-dados.celula-linha       = i-linha
               tt-dados.celula-formula     = ""
               tt-dados.celula-valor       = string(temp-item.qtd-pend,"->>,>>>,>>9.99")     
               tt-dados.retorna-valor      = no.   
        
        ASSIGN i-coluna = i-coluna + 1.
        ASSIGN de-peso-item     = item.peso-liquido * temp-item.qtd-pend
               de-peso-produzir = de-peso-produzir  + de-peso-item.
    
        create tt-dados.
        assign tt-dados.arquivo-num        = 1
               tt-dados.planilha-num       = 01
               tt-dados.celula-coluna      = i-coluna
               tt-dados.celula-linha       = i-linha
               tt-dados.celula-formula     = ""
               tt-dados.celula-valor       = string(de-peso-item,"->>,>>>,>>9.99")     
               tt-dados.retorna-valor      = no.   
/*    end.
    else do.
        create tt-dados.
        assign tt-dados.arquivo-num        = 1
               tt-dados.planilha-num       = 01
               tt-dados.celula-coluna      = i-coluna
               tt-dados.celula-linha       = i-linha
               tt-dados.celula-formula     = ""
               tt-dados.celula-valor       = "0"     
               tt-dados.retorna-valor      = no.   
        
        ASSIGN i-coluna = i-coluna + 1.
    
        create tt-dados.
        assign tt-dados.arquivo-num        = 1
               tt-dados.planilha-num       = 01
               tt-dados.celula-coluna      = i-coluna
               tt-dados.celula-linha       = i-linha
               tt-dados.celula-formula     = ""
               tt-dados.celula-valor       = "0"     
               tt-dados.retorna-valor      = no.       
    end.       */
    
    ASSIGN i-coluna = i-coluna + 1.

    if tt-param.tg-compl then do:
        create tt-dados.
        assign tt-dados.arquivo-num        = 1
               tt-dados.planilha-num       = 01
               tt-dados.celula-coluna      = i-coluna
               tt-dados.celula-linha       = i-linha
               tt-dados.celula-formula     = ""
               tt-dados.celula-valor       = temp-item.nome-transp.
               tt-dados.retorna-valor      = no.   
    end.
    if avail ped-venda-ext           and
       ped-venda-ext.Bloqueado = yes then DO:
       ASSIGN i-coluna = i-coluna + 1.

       create tt-dados.
       assign tt-dados.arquivo-num        = 1
              tt-dados.planilha-num       = 01
              tt-dados.celula-coluna      = i-coluna
              tt-dados.celula-linha       = i-linha
              tt-dados.celula-formula     = ""
              tt-dados.celula-valor       = "BLOQUEADO".
              tt-dados.retorna-valor      = no.
       
       ASSIGN i-coluna = i-coluna + 1.
    END.
    ELSE
       ASSIGN i-coluna = i-coluna + 2.
    
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num    = 1
           tt-dados.planilha-num   = 01
           tt-dados.celula-coluna  = i-coluna
           tt-dados.celula-linha   = i-linha
           tt-dados.celula-formula = ""
           tt-dados.celula-valor   = desc-sit-aval[temp-item.cod-sit-aval].
           tt-dados.retorna-valor  = no.

    IF  tt-param.tg-localiza THEN DO:

/*         FOR EACH tt-item NO-LOCK */
/*             WHERE tt-item.cod-estabel = temp-item.cod-estabel */
/*             AND   tt-item.it-codigo   = temp-item.it-codigo */
/*             AND   tt-item.saldo       > 0 */
/*             BY tt-ITEM.saldo DESCENDING: */
/*    */
/*             ASSIGN c-saldo-item = c-saldo-item + STRING(tt-item.cod-depos)   + '-' + */
/*                                   STRING(tt-item.cod-localiz) + '-' + */
/*                                   STRING(tt-item.lote)        + '-' + */
/*                                   STRING(tt-item.saldo,'>>>,>>>,>>9.99' + ' / '). */
/*         end. */ /*         if c-saldo-item  <> "" then do. */
/*             ASSIGN i-coluna = i-coluna + 1 . */
/*    */
/*             create tt-dados. */
/*             assign tt-dados.arquivo-num        = 1 */
/*                    tt-dados.planilha-num       = 01 */
/*                    tt-dados.celula-coluna      = i-coluna */
/*                    tt-dados.celula-linha       = i-linha */
/*                    tt-dados.celula-formula     = "" */
/*                    tt-dados.celula-valor       = c-saldo-item */
/*                    tt-dados.retorna-valor      = no. */
/*    */
/*            ASSIGN C-SALDO-ITEM = "". */
/*         END. */

        ASSIGN i-coluna = i-coluna + 1.
        
        FOR EACH tt-item NO-LOCK
            WHERE tt-item.cod-estabel = temp-item.cod-estabel
            AND   tt-item.it-codigo   = temp-item.it-codigo
            AND   tt-item.saldo       > 0
            BY tt-ITEM.saldo DESCENDING:
  
            ASSIGN c-saldo-item = c-saldo-item + STRING(tt-item.cod-depos)   + '-' +
                                  STRING(tt-item.cod-localiz) + '-' +
                                  STRING(tt-item.lote)        + '-' +
                                  STRING(tt-item.saldo,'->>,>>>,>>9.99').
            
            if  c-saldo-item  <> "" then do.
            
                create tt-dados.
                assign tt-dados.arquivo-num        = 1
                       tt-dados.planilha-num       = 01
                       tt-dados.celula-coluna      = i-coluna
                       tt-dados.celula-linha       = i-linha
                       tt-dados.celula-formula     = ""
                       tt-dados.celula-valor       = c-saldo-item
                       tt-dados.retorna-valor      = no.
      
               ASSIGN C-SALDO-ITEM = "".
               
               assign i-linha  = i-linha  + 1.

            END.
    
        end.
  
    END.
  
    ASSIGN i-coluna = 0.
    
END.

if tot-peso-81 > 0 then do:
    assign indicador-atraso = tot-dias-peso-81 / tot-peso-81.
    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = 20
           tt-dados.celula-linha       = 3
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = STRING(indicador-atraso)
           tt-dados.retorna-valor      = no.   

end.
if tot-peso-82 > 0 then do:
    assign indicador-atraso = tot-dias-peso-82 / tot-peso-82.
    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = 23
           tt-dados.celula-linha       = 3
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = STRING(indicador-atraso)
           tt-dados.retorna-valor      = no.   

end.
if tot-peso > 0 then do:
    assign indicador-atraso = tot-dias-peso / tot-peso.
    create tt-dados.
    assign tt-dados.arquivo-num        = 1
           tt-dados.planilha-num       = 01
           tt-dados.celula-coluna      = 17
           tt-dados.celula-linha       = 3
           tt-dados.celula-formula     = ""
           tt-dados.celula-valor       = STRING(indicador-atraso)
           tt-dados.retorna-valor      = no.   

end.
   
assign i-linha = i-linha + 1.

create tt-dados.
assign tt-dados.arquivo-num        = 1
       tt-dados.planilha-num       = 01
       tt-dados.celula-coluna      = 16
       tt-dados.celula-linha       = i-linha
       tt-dados.celula-formula     = ""
       tt-dados.celula-valor       = string(tot-qtd-solic,"->>,>>>,>>9.99")     
       tt-dados.retorna-valor      = no.   
create tt-dados.
assign tt-dados.arquivo-num        = 1
       tt-dados.planilha-num       = 01
       tt-dados.celula-coluna      = 17
       tt-dados.celula-linha       = i-linha
       tt-dados.celula-formula     = ""
       tt-dados.celula-valor       = string(tot-saldo-ped,"->>,>>>,>>9.99")     
       tt-dados.retorna-valor      = no.   
create tt-dados.
assign tt-dados.arquivo-num        = 1
       tt-dados.planilha-num       = 01
       tt-dados.celula-coluna      = 18
       tt-dados.celula-linha       = i-linha
       tt-dados.celula-formula     = ""
       tt-dados.celula-valor       = string(tot-saldo-a-atend,"->>,>>>,>>9.99")     
       tt-dados.retorna-valor      = no.   
create tt-dados.
assign tt-dados.arquivo-num        = 1
       tt-dados.planilha-num       = 01
       tt-dados.celula-coluna      = 19
       tt-dados.celula-linha       = i-linha
       tt-dados.celula-formula     = ""
       tt-dados.celula-valor       = string(tot-qtd-trans,"->>,>>>,>>9.99")     
       tt-dados.retorna-valor      = no.   
create tt-dados.
assign tt-dados.arquivo-num        = 1
       tt-dados.planilha-num       = 01
       tt-dados.celula-coluna      = 20
       tt-dados.celula-linha       = i-linha
       tt-dados.celula-formula     = ""
       tt-dados.celula-valor       = string(tot-sug-transf,"->>,>>>,>>9.99")             
       tt-dados.retorna-valor      = no.   

create tt-dados.
assign tt-dados.arquivo-num        = 1
       tt-dados.planilha-num       = 01
       tt-dados.celula-coluna      = 21
       tt-dados.celula-linha       = i-linha
       tt-dados.celula-formula     = ""
       tt-dados.celula-valor       = string(tot-qtd-exped,"->>,>>>,>>9.99")     
       tt-dados.retorna-valor      = no.   

ASSIGN i-coluna = 22.

create tt-dados.
assign tt-dados.arquivo-num        = 1
       tt-dados.planilha-num       = 01
       tt-dados.celula-coluna      = i-coluna
       tt-dados.celula-linha       = i-linha
       tt-dados.celula-formula     = ""
       tt-dados.celula-valor       = string(de-peso-expedir,"->>,>>>,>>9.99")     
       tt-dados.retorna-valor      = no.   

create tt-dados.
assign tt-dados.arquivo-num        = 1
       tt-dados.planilha-num       = 01
       tt-dados.celula-coluna      = 23
       tt-dados.celula-linha       = i-linha
       tt-dados.celula-formula     = ""
       tt-dados.celula-valor       = string(tot-qtd-produz,"->>,>>>,>>9.99")     
       tt-dados.retorna-valor      = no.   

ASSIGN i-coluna = 24.

create tt-dados.
assign tt-dados.arquivo-num        = 1
       tt-dados.planilha-num       = 01
       tt-dados.celula-coluna      = i-coluna
       tt-dados.celula-linha       = i-linha
       tt-dados.celula-formula     = ""
       tt-dados.celula-valor       = string(de-peso-produzir,"->>,>>>,>>9.99")     
       tt-dados.retorna-valor      = no.   

/* *** Define Temp-Table *** */

run pi-finalizar in h-acomp.      

run utp\utapi008.p (input-output table tt-configuracao,
                    input-output table tt-dados,
                    input-output table tt-erros).

if  return-value = "NOK" then do:
    for each tt-erros no-lock:
       message tt-erros.cod-erro 
               tt-erros.desc-erro view-as alert-box.
    end. /* Fim for each tt-erros */
end. /* Fim ocorreu Erros */

RETURN "OK:U".

/* fim do programa */





