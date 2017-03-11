/*******************************************************************************
**
**    PROGRAMA: ftp/ESFT0808rp.p
**        DATA: Mar‡o de 2017
**
**    OBJETIVO: Relat¢rio de Faturamento e Margem de Contribui‡Æo em Excel
**
**       AUTOR: Edson-Damgra
**
**      VERSAO: 5.06.00.MTQ - 22/02/2017
**      TICKET: 12060
**
** Este fonte e de propriedade exclusiva da MANTIQUEIRA ALIMENTOS LTDA, 
** sua reproducao parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
********************************************************************************/
{include/i-prgvrs.i ESFT0808RP 5.06.00.MTQ}

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ESFT0808 ftp}
&ENDIF

{include/i_fnctrad.i}
{include/i-rpvar.i}
{include/i-rpcab.i}
{include/tt-edit.i}
{include/pi-edit.i}
{utp/ut-glob.i}

DEF TEMP-TABLE tt-param NO-UNDO
    FIELD destino               AS INT
    FIELD execucao              AS INT
    FIELD arquivo               AS CHAR FORMAT "x(35)"
    FIELD usuario               AS CHAR FORMAT "x(12)"
    FIELD data-exec             AS DATE
    FIELD enviar-email          AS LOG
    FIELD destinatarios         AS CHAR
    FIELD hora-exec             AS INT
    FIELD modelo-rtf            AS CHAR FORMAT "x(35)"
    FIELD l-habilitaRtf         AS LOG
    /*FIELD v_num_tip_aces_usuar  AS INT*/
    FIELD c-cod-estabel-ini     AS CHAR /* Folder Sel */
    FIELD c-cod-estabel-fim     AS CHAR
    FIELD c-serie-docto-ini     AS CHAR
    FIELD c-serie-docto-fim     AS CHAR
    FIELD c-nat-operacao-ini    AS CHAR
    FIELD c-nat-operacao-fim    AS CHAR
    FIELD i-cod-emitente-ini    AS INT
    FIELD i-cod-emitente-fim    AS INT
    FIELD c-nro-docto-ini       AS CHAR
    FIELD c-nro-docto-fim       AS CHAR
    FIELD dt-emisdoc-ini        AS DATE
    FIELD dt-emisdoc-fim        AS DATE
    FIELD dt-trandoc-ini        AS DATE
    FIELD dt-trandoc-fim        AS DATE
    FIELD c-item-ini            AS CHAR
    FIELD c-item-fim            AS CHAR
    FIELD i-ge-codigo-ini       AS INT 
    FIELD i-ge-codigo-fim       AS INT
    FIELD c-fm-codigo-ini       AS CHAR  
    FIELD c-fm-codigo-fim       AS CHAR
    FIELD c-fm-cod-com-ini      AS CHAR
    FIELD c-fm-cod-com-fim      AS CHAR /* Fim - Folder Sel */
    FIELD tip-natoper           AS INT  /* Folder Par */
    FIELD parametro-1           AS LOG  
    FIELD l-estabel             AS LOG
    FIELD l-grupo               AS LOG
    FIELD l-familia             AS LOG
    FIELD l-familia-com         AS LOG
    FIELD l-item                AS LOG
    FIELD l-emitente            AS LOG
    FIELD l-representante       AS LOG
    FIELD l-estado              AS LOG
    FIELD l-cidade              AS LOG
    FIELD l-documento           AS LOG  /* Fim - Folder Par */.   
 
DEF TEMP-TABLE tt-digita NO-UNDO
    FIELD ordem                 AS INT   FORMAT ">>>>9"
    FIELD exemplo               AS CHAR FORMAT "x(30)"
    INDEX id ordem.
    
DEF TEMP-TABLE tt-reg NO-UNDO
    FIELD cod-estabel              AS CHAR FORMAT "x(10)"
    FIELD cod-emitente             AS CHAR FORMAT "x(10)"
    FIELD nome                     AS CHAR FORMAT "x(50)"
    FIELD cnpj                     AS CHAR FORMAT "x(50)"
    FIELD insc-estadual            AS CHAR FORMAT "x(50)"
    FIELD cidade                   AS CHAR FORMAT "x(50)"
    FIELD estado                   AS CHAR FORMAT "x(50)"
    FIELD representante            AS CHAR FORMAT "x(50)"
    FIELD transp                   AS CHAR FORMAT "x(50)"
    FIELD especie                  AS CHAR FORMAT "x(50)"
    FIELD nat-op                   AS CHAR FORMAT "x(50)"
    FIELD cfop                     AS CHAR FORMAT "x(50)"
    FIELD un                       AS CHAR FORMAT "x(50)"
    FIELD serie                    AS CHAR FORMAT "x(50)"
    FIELD doc-fiscal               AS CHAR FORMAT "x(50)"
    FIELD dt-emissao               AS CHAR FORMAT "x(50)"
    FIELD dt-docto                 AS CHAR FORMAT "x(50)"
    FIELD cst                      AS CHAR FORMAT "x(50)"
    FIELD seq                      AS CHAR FORMAT "x(50)"
    FIELD ITEM                     AS CHAR FORMAT "x(50)"
    FIELD desc-item                AS CHAR FORMAT "x(50)"
    FIELD ge-codigo                AS CHAR FORMAT "x(50)"
    FIELD fm-codigo                AS CHAR FORMAT "x(50)"
    FIELD fm-cod-com               AS CHAR FORMAT "x(50)"
    FIELD ncm                      AS CHAR FORMAT "x(50)"
    FIELD qtde                     AS DEC
    FIELD unitario                 AS CHAR FORMAT "x(50)"
    FIELD total-produto            AS DEC
    FIELD custo-medio              AS DEC
    FIELD custo-prod               AS DEC
    /*FIELD reprocessamento          AS DEC*/
    FIELD ovo-natura               AS DEC
    FIELD embalagem                AS DEC
    FIELD ggf                      AS DEC
    /*FIELD ovo-MT                   AS CHAR FORMAT "x(50)"*/
    FIELD vl-contabil              AS DEC
    FIELD perc-icms                AS CHAR FORMAT "x(50)"
    FIELD icms-base                AS DEC
    FIELD icms-vi                  AS DEC
    /*FIELD icms-cst                 AS CHAR FORMAT "x(50)"*/
    FIELD perc-pis                 AS CHAR FORMAT "x(50)"
    FIELD pis-base                 AS DEC
    FIELD pis-vi                   AS DEC
    FIELD perc-cofins              AS CHAR FORMAT "x(50)"
    FIELD cofins-base              AS DEC
    FIELD cofins-vi                AS DEC
    FIELD iss-base                 AS DEC
    FIELD perc-iss                 AS CHAR FORMAT "x(50)"
    FIELD iss-vi                   AS DEC
    FIELD impostos                 AS CHAR FORMAT "x(50)"
    FIELD desc-fin                 AS DEC
    FIELD frete                    AS CHAR FORMAT "x(50)"
    FIELD margem-cont              AS CHAR FORMAT "x(50)"
    FIELD desp-com                 AS CHAR FORMAT "x(50)"
    FIELD desp-adm                 AS CHAR FORMAT "x(50)"
    FIELD desp-fin                 AS CHAR FORMAT "x(50)"
    FIELD outros                   AS CHAR FORMAT "x(50)"
    FIELD chave-nfe                AS CHAR FORMAT "x(50)"
    FIELD situ                     AS CHAR FORMAT "x(50)"
    FIELD nome-matriz              AS CHAR FORMAT "x(50)"
    FIELD placa                    AS CHAR FORMAT "x(50)".
    
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS RAW.
       
DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.
     
CREATE tt-param.   

RAW-TRANSFER raw-param TO tt-param.  

FOR EACH tt-raw-digita:
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

DEF STREAM st-saida.
DEF STREAM s-log.
DEF STREAM s-log-email.
DEF STREAM str-rp.
DEF VAR h-acomp              AS HANDLE NO-UNDO.
DEF VAR c-arquivo            AS CHAR   NO-UNDO.
  
{cdp/cd0666.i}

DEF VAR c-acompanha          AS CHAR                                           NO-UNDO.
DEF VAR i-cont               AS INT                                            NO-UNDO.
DEF VAR c-arq-modelo         AS CHAR EXTENT 2 NO-UNDO.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

RUN pi-inicializar IN h-acomp(INPUT "Acompanhamento Relatorio").

/*------------------------------------------------------------------------------
             Definicao de Variaveis para trabalhar com Excel
------------------------------------------------------------------------------*/
DEF NEW SHARED VAR ChExcelApplication   AS COM-HANDLE.
DEF NEW SHARED VAR chWorkbook           AS COM-HANDLE.
DEF NEW SHARED VAR ChWorksheet          AS COM-HANDLE   EXTENT 3.
DEF NEW SHARED VAR iColumn              AS INT          INITIAL 2.
DEF NEW SHARED VAR cColumn              AS CHAR.
DEF NEW SHARED VAR cRange               AS CHAR.
DEF NEW SHARED VAR chWebview            AS COM-HANDLE.
DEF VAR c-lastCol                       AS CHAR         NO-UNDO.
DEF VAR i-linha                         AS INT          NO-UNDO.
/*-------------------------------------------------------------------------------*/

/* ----------------------------------------------------------------------------------------------------------------- *
 * Definicao de Variaveis de Sel                                                                                     *
 * ----------------------------------------------------------------------------------------------------------------- */
DEF VAR c-cod-estabel-ini               AS CHAR.
DEF VAR c-cod-estabel-fim               AS CHAR.
DEF VAR c-serie-docto-ini               AS CHAR.
DEF VAR c-serie-docto-fim               AS CHAR.
DEF VAR c-nat-operacao-ini              AS CHAR.
DEF VAR c-nat-operacao-fim              AS CHAR.
DEF VAR i-cod-emitente-ini              AS INT.
DEF VAR i-cod-emitente-fim              AS INT.
DEF VAR c-nro-docto-ini                 AS CHAR.
DEF VAR c-nro-docto-fim                 AS CHAR.
DEF VAR dt-emisdoc-ini                  AS DATE.
DEF VAR dt-emisdoc-fim                  AS DATE.
DEF VAR dt-trandoc-ini                  AS DATE.
DEF VAR dt-trandoc-fim                  AS DATE.
DEF VAR c-item-ini                      AS CHAR.
DEF VAR c-item-fim                      AS CHAR.
DEF VAR i-ge-codigo-ini                 AS INT.
DEF VAR i-ge-codigo-fim                 AS INT.
DEF VAR c-fm-codigo-ini                 AS CHAR.
DEF VAR c-fm-codigo-fim                 AS CHAR.
DEF VAR c-fm-cod-com-ini                AS CHAR.
DEF VAR c-fm-cod-com-fim                AS CHAR.
    
DEF VAR tip-natoper                     AS INT.
def var i-tipo-custo                    as int.
DEF VAR parametro-1                     AS LOG.
DEF VAR l-estabel                       AS LOG.
DEF VAR l-grupo                         AS LOG.
DEF VAR l-familia                       AS LOG.
DEF VAR l-familia-com                   AS LOG.
DEF VAR l-item                          AS LOG.
DEF VAR l-emitente                      AS LOG.
DEF VAR l-cidade                        AS LOG.
DEF VAR l-estado                        AS LOG.
DEF VAR l-documento                     AS LOG.
DEF VAR l-n-gera-dupli                  AS LOG.

ASSIGN
    c-cod-estabel-ini   = tt-param.c-cod-estabel-ini 
    c-cod-estabel-fim   = tt-param.c-cod-estabel-fim 
    c-serie-docto-ini   = tt-param.c-serie-docto-ini 
    c-serie-docto-fim   = tt-param.c-serie-docto-fim 
    c-nat-operacao-ini  = tt-param.c-nat-operacao-ini
    c-nat-operacao-fim  = tt-param.c-nat-operacao-fim
    i-cod-emitente-ini  = tt-param.i-cod-emitente-ini
    i-cod-emitente-fim  = tt-param.i-cod-emitente-fim
    c-nro-docto-ini     = tt-param.c-nro-docto-ini   
    c-nro-docto-fim     = tt-param.c-nro-docto-fim   
    dt-emisdoc-ini      = tt-param.dt-emisdoc-ini    
    dt-emisdoc-fim      = tt-param.dt-emisdoc-fim    
    dt-trandoc-ini      = tt-param.dt-trandoc-ini    
    dt-trandoc-fim      = tt-param.dt-trandoc-fim    
    c-item-ini          = tt-param.c-item-ini        
    c-item-fim          = tt-param.c-item-fim        
    i-ge-codigo-ini     = tt-param.i-ge-codigo-ini   
    i-ge-codigo-fim     = tt-param.i-ge-codigo-fim   
    c-fm-codigo-ini     = tt-param.c-fm-codigo-ini   
    c-fm-codigo-fim     = tt-param.c-fm-codigo-fim   
    c-fm-cod-com-ini    = tt-param.c-fm-cod-com-ini  
    c-fm-cod-com-fim    = tt-param.c-fm-cod-com-fim  
                                  
    tip-natoper         = tt-param.tip-natoper       
    i-tipo-custo        = tt-param.i-tipo-custo
    parametro-1         = tt-param.parametro-1       
    l-estabel           = tt-param.l-estabel         
    l-grupo             = tt-param.l-grupo           
    l-familia           = tt-param.l-familia         
    l-familia-com       = tt-param.l-familia-com     
    l-item              = tt-param.l-item            
    l-emitente          = tt-param.l-emitente        
    l-cidade            = tt-param.l-cidade          
    l-estado            = tt-param.l-estado          
    l-documento         = tt-param.l-documento
    l-n-gera-dupli      = tt-param.l-n-gera-dupli.

/* ----------------------------------------------------------------------------------------------------------------- */

/* ----------------------------------------------------------------------------------------------------------------- *
 * Bloco de controle de SW                                                                                           *
 * ----------------------------------------------------------------------------------------------------------------- */

FIND mgcad.empresa WHERE 
     mgcad.empresa.ep-codigo = i-ep-codigo-usuario NO-LOCK NO-ERROR.

IF avail mgcad.empresa THEN 
    ASSIGN c-empresa = empresa.razao-social.
ELSE 
    ASSIGN c-empresa = "Mantiqueira Alimentos Ltda".

ASSIGN c-programa = "ESFT0808"
       c-sistema  = "ftp"
       c-versao   = "5.06.00."
       c-revisao  = "MTQ".

{utp/ut-liter.i Relat¢rio_de_Faturamento_e_Margem_de_Contribui‡Æo  * L} /* Titulo do relatorio */ 
ASSIGN c-titulo-relat = TRIM(RETURN-VALUE).
{utp/ut-liter.i ftp * L}
ASSIGN c-sistema      = TRIM(RETURN-VALUE).
/* ----------------------------------------------------------------------------------------------------------------- */

RUN gerar-cabecalho.

ASSIGN ChExcelApplication:DisplayAlerts = FALSE
       ChExcelApplication:VISIBLE       = NO.
    
RUN gerar-excel.
      
DOS SILENT DEL VALUE(tt-param.arquivo).

chWorkbook:SaveAs(tt-param.arquivo,51,,,,,) NO-ERROR.

IF  tt-param.DESTINO = 3 AND tt-param.execucao = 1 THEN DO:
    ChExcelApplication:VISIBLE = YES.
END.
                      
RUN pi-finalizar IN h-acomp.

/* Rotina de enviar e-mail */
{masters/i-send-email.i}
  
RELEASE OBJECT ChExcelApplication NO-ERROR.
RELEASE OBJECT chWorkbook         NO-ERROR.
RELEASE OBJECT ChWorksheet[1]     NO-ERROR.

RETURN "OK".
/* ------------------------------------------------ Fim do programa ------------------------------------------------ */

FUNCTION int-to-excelColumn RETURNS CHAR (INPUT columnNumber AS INT):
    DEF VAR dividend    AS INT      NO-UNDO.
    DEF VAR columnName  AS CHAR     NO-UNDO.
    DEF VAR modu        AS INT      NO-UNDO.
    ASSIGN  dividend = columnNumber.
    
    DO WHILE (dividend > 0):
        ASSIGN  modu        = (dividend - 1) MODULO 26
                columnName  = CHR(65 + modu) + columnName
                dividend    = (dividend - modu) / 26.
    END.

    RETURN columnName.
END.

FUNCTION fn-validar-chave-nfe RETURNS CHAR (INPUT pSerie AS CHAR, INPUT pDocto AS CHAR, INPUT pEmitente AS INT64):
    FOR FIRST consist-nota FIELDS (serie-docto
                                   nro-docto
                                   cod-emitente
                                   nat-operacao
                                   mensagem
                                   tipo
                                   calculado
                                   informado
                                   int-1
                                   ) NO-LOCK
              WHERE 
                serie-docto  = pSerie    AND
                nro-docto    = pDocto    AND
                cod-emitente = pEmitente AND 
                mensagem     = 33187: 
    END.
    IF avail(consist-nota) THEN DO:
        RUN utp/ut-msgs.p (INPUT "msg":U, INPUT 33187, INPUT "").
        RETURN RETURN-VALUE.
    END.
    ELSE RETURN "Chave Validada".
END.

FUNCTION fn-cst-icms RETURNS INT64 (INPUT p-cd-trib-icm    AS INT64,  /* natur-oper.cd-trib-icm    */
                                    INPUT p-aliquota-icm   AS DEC,    /* natur-oper.aliquota-icm   */
                                    INPUT p-ind-it-sub-dif AS LOG,    /* natur-oper.ind-it-sub-dif */
                                    INPUT p-ind-tipo-vat   AS LOG,    /* natur-oper.ind-tipo-vat   */
                                    INPUT p-consum-final   AS LOG,    /* natur-oper.consum-final   */
                                    INPUT p-subs-trib      AS LOG,    /* natur-oper.subs-trib      */
                                    INPUT p-icms-subs-trib AS DEC,    /* natur-oper.icms-subs-trib */
                                    INPUT p-perc-red-icm   AS DEC):   /* natur-oper.perc-red-icm   */

    IF p-cd-trib-icm    = 1  AND 
       p-aliquota-icm   > 0  AND 
       p-ind-it-sub-dif = NO AND
       p-ind-tipo-vat   = NO AND
       p-consum-final   = NO AND
       p-subs-trib      = NO AND
       p-icms-subs-trib = 0  AND
       p-perc-red-icm   = 0         
        THEN RETURN 0.
   
    IF p-cd-trib-icm    = 1   AND 
       p-aliquota-icm   > 0   AND 
       p-ind-it-sub-dif = NO  AND
       p-ind-tipo-vat   = NO  AND
       p-consum-final   = NO  AND
       p-subs-trib      = YES AND
       p-icms-subs-trib > 0   AND
       p-perc-red-icm   = 0          
        THEN RETURN 10.
    
    IF p-cd-trib-icm    = 4  AND 
       p-aliquota-icm   > 0  AND 
       p-ind-it-sub-dif = NO AND
       p-ind-tipo-vat   = NO AND
       p-consum-final   = NO AND
       p-subs-trib      = NO AND
       p-icms-subs-trib = 0  AND
       p-perc-red-icm   > 0          
        THEN RETURN 20.
   
    IF p-cd-trib-icm    = 2   AND 
       p-aliquota-icm   = 0   AND 
       p-ind-it-sub-dif = NO  AND
       p-ind-tipo-vat   = YES AND
       p-consum-final   = NO  AND
       p-subs-trib      = YES AND
       p-icms-subs-trib > 0   AND
       p-perc-red-icm   = 0        
        THEN RETURN 30.
    
    IF p-cd-trib-icm    = 2   AND 
       p-aliquota-icm   = 0   AND 
       p-ind-it-sub-dif = NO  AND
       p-ind-tipo-vat   = NO  AND
       p-consum-final   = NO  AND
       p-subs-trib      = NO  AND
       p-icms-subs-trib = 0   AND
       p-perc-red-icm   = 0         
        THEN RETURN 40.
    
    IF p-cd-trib-icm    = 2   AND 
       p-aliquota-icm   > 0   AND 
       p-ind-it-sub-dif = NO  AND
       p-ind-tipo-vat   = YES AND
       p-consum-final   = NO  AND
       p-subs-trib      = NO  AND
       p-icms-subs-trib = 0   AND
       p-perc-red-icm   = 0         
        THEN RETURN 41.
    
    IF p-cd-trib-icm    = 2   AND 
       p-aliquota-icm   = 0   AND 
       p-ind-it-sub-dif = NO  AND
       p-ind-tipo-vat   = NO  AND
       p-consum-final   = NO  AND
       p-subs-trib      = NO  AND
       p-icms-subs-trib = 0   AND
       p-perc-red-icm   = 0         
        THEN RETURN 50.
    
    IF p-cd-trib-icm    = 5   AND 
       p-aliquota-icm   = 0   AND 
       p-ind-it-sub-dif = YES AND
       p-ind-tipo-vat   = NO  AND
       p-consum-final   = NO  AND
       p-subs-trib      = NO  AND
       p-icms-subs-trib = 0   AND
       p-perc-red-icm   = 0         
        THEN RETURN 51.
    
    IF p-cd-trib-icm    = 3   AND 
       p-aliquota-icm   = 0   AND 
       p-ind-it-sub-dif = NO  AND
       p-ind-tipo-vat   = NO  AND
       p-consum-final   = NO  AND
       p-subs-trib      = YES AND
       p-icms-subs-trib = 0   AND
       p-perc-red-icm   = 0         
        THEN RETURN 60.
    
    IF p-cd-trib-icm    = 4   AND 
       p-aliquota-icm   > 0   AND 
       p-ind-it-sub-dif = NO  AND
       p-ind-tipo-vat   = NO  AND
       p-consum-final   = NO  AND
       p-subs-trib      = YES AND
       p-icms-subs-trib > 0   AND
       p-perc-red-icm   > 0          
        THEN RETURN 70.
    
    IF p-cd-trib-icm    = 3   AND 
       p-aliquota-icm   = 0   AND 
       p-ind-it-sub-dif = NO  AND
       p-ind-tipo-vat   = NO  AND
       p-consum-final   = NO  AND
       p-subs-trib      = NO  AND
       p-icms-subs-trib = 0   AND
       p-perc-red-icm   = 0         
        THEN RETURN 90.

    RETURN ?.
END.

PROCEDURE gerar-cabecalho.
    CREATE "Excel.Application" ChExcelApplication.                  
    chWorkbook = ChExcelApplication:Workbooks:ADD().
END PROCEDURE.

PROCEDURE gerar-excel:
    DEF VAR i-num-reg       AS INT      NO-UNDO.
    DEF VAR de-total-mov    AS DEC      NO-UNDO. 
    DEF VAR c-narrativa     AS CHAR     NO-UNDO.
    DEF VAR c-lastCol       AS CHAR     NO-UNDO.
    DEF VAR c-valor         AS CHAR     NO-UNDO.
    DEF VAR i-ct            AS INT      NO-UNDO.
    

    ASSIGN c-valor   = "Estab"                      +
            CHR(160) + "Cod.Emitente"               +
            CHR(160) + "Nome"                       +
            CHR(160) + "CNPJ"                       +
            CHR(160) + "Insc.Estadual"              +
            CHR(160) + "Cidade"                     +
            CHR(160) + "Estado"                     +
            CHR(160) + "Representante"              +
            CHR(160) + "Transportador"              +
            CHR(160) + "Esp‚cie"                    +
            CHR(160) + "Nat.Opera‡Æo"               +
            CHR(160) + "CFOP"                       +
            CHR(160) + "UN"                         +
            CHR(160) + "S‚rie"                      +
            CHR(160) + "Doc.Fiscal"                 +
            CHR(160) + "Dt.EmissÆo"                 +
            CHR(160) + "Dt.Docto"                   +
            CHR(160) + "CST"                        +
            CHR(160) + "Seq"                        +
            CHR(160) + "Item"                       +
            CHR(160) + "Descri‡Æo do Item"          +
            CHR(160) + "Grupo de Estoque"           +
            CHR(160) + "Fam¡lia"                    +
            CHR(160) + "Fam¡lia Comercial"          +            
            CHR(160) + "NCM"                        +
            CHR(160) + "Qtde"                       +
            CHR(160) + "Unit rio"                   +
            CHR(160) + "Total Produto"              +
            CHR(160) + "Custo M‚dio"                +
            CHR(160) + "Custo Produ‡Æo"             +
            /*CHR(160) + "Reprocessamento"            +*/
            CHR(160) + "Ovo in Natura"              +
            CHR(160) + "Embalagem"                  +
            CHR(160) + "GGF"                        +
            /*CHR(160) + "Voltando o ovo para o MT"   +*/
            CHR(160) + "Vl.Cont bil"                +
            CHR(160) + "% ICMS"                     +
            CHR(160) + "ICMS Base"                  +
            CHR(160) + "ICMS Vl"                    +
            /*CHR(160) + "CST ICMS"                   +*/
            CHR(160) + "% PIS"                      +
            CHR(160) + "PIS Base"                   +
            CHR(160) + "PIS Vl"                     +
            CHR(160) + "% Cofins"                   +
            CHR(160) + "Cofins Base"                +
            CHR(160) + "Cofins Vl"                  +
            CHR(160) + "ISS Base"                   +
            CHR(160) + "% ISS"                      +
            CHR(160) + "ISS Vl"                     +
        /**/CHR(160) + "IMPOSTOS"                   +
            CHR(160) + "Desconto Fin."              +
            CHR(160) + "Frete"                      +
            CHR(160) + "Margem BRUTA"               +
            CHR(160) + "Despesas Comerciais"        +
            CHR(160) + "Despesa Adm"                +
            CHR(160) + "Despesa Fin"                +
            CHR(160) + "Outros"                     +
            CHR(160) + "Chave NFe"                  +
            CHR(160) + "Situa‡Æo"                   +
            CHR(160) + "Nome Matriz"                +
            CHR(160) + "Placa"                      +
            
            /*CHR(160) + "Conta Contabil"*/         .
           
    ASSIGN ChExcelApplication:Range("A4"):VALUE = c-valor
           i-ct                                 = 0
           i-linha                              = 4
           c-lastCol = int-to-excelColumn(NUM-ENTRIES( c-valor, CHR(160) )).

    /* loop de preenchimento */
    RUN Movimentos.
                  
    ChExcelApplication:Range("A4:A" + STRING(i-linha + 1)):SELECT.
  
    /* Explode a primeira coluna nas colunas respectivas */
    ChExcelApplication:SELECTION:TextToColumns    (, /* Destination          */
                                         1,          /* DataType             */
                                         ,           /* TextQualifier        */
                                         ,           /* ConsecutiveDelimiter */
                                         ,           /* Tab                  */
                                         ,           /* Semicolon            */
                                         ,           /* Comma                */
                                         ,           /* Space                */
                                         TRUE,       /* Other                */
                                         CHR(160),   /* OtherChar            */
                                         ,           /* FieldInfo            */
                                         ) NO-ERROR.
      
    ASSIGN  ChExcelApplication:Range("A1"):VALUE            = "Relat¢rio de margem em excel"
            ChExcelApplication:Range("A1"):FONT:FontStyle   = "Negrito"
            ChExcelApplication:Range("A1"):FONT:SIZE        = 14
            ChExcelApplication:Range("A1"):FONT:NAME        = "Arial"
            ChExcelApplication:Range("A4:" + c-lastCol + "4"):Interior:ColorIndex   = 37
            ChExcelApplication:Range("A2:" + c-lastCol + "4"):FONT:BOLD             = TRUE
            ChExcelApplication:Sheets:ITEM(1):NAME                                  = "Detalhado".
            
    ChExcelApplication:Rows("4:4"):Autofilter (,,,).
    ChExcelApplication:Cells:EntireColumn:AutoFit.
    
    ChExcelApplication:COLUMNS("A:A"):ColumnWidth = 10.                       
    
END PROCEDURE.

PROCEDURE Movimentos:
    DEF VAR c-cst-pis       AS CHAR                     NO-UNDO.
    DEF VAR c-cst-cofins    AS CHAR                     NO-UNDO.
    DEF VAR c-placa         AS CHAR                     NO-UNDO.
    DEF VAR c-nfe           AS CHAR                     NO-UNDO.
    DEF VAR c-cfop          AS CHAR                     NO-UNDO. 
    DEF VAR c-id            AS CHAR                     NO-UNDO.
    DEF VAR c-situacao      AS CHAR                     NO-UNDO. 
    DEF VAR v-perc-pis      AS DEC  FORMAT ">9.99999"   NO-UNDO.
    DEF VAR v-perc-cofins   AS DEC  FORMAT ">9.99999"   NO-UNDO.
    DEF VAR icms-cst        AS DEC                      NO-UNDO.
    DEF VAR c-valor         AS CHAR                     NO-UNDO.

    ASSIGN c-nfe        = ""
           c-cst-pis    = ""
           c-cst-cofins = ""
           c-placa      = "".
            
    /* natur-oper.tipo: entrada=1 e saida=2 */
    IF natur-oper.tipo = 1 THEN DO:
       FIND docum-est WHERE 
            docum-est.serie-docto  = doc-fiscal.serie        AND
            docum-est.nro-docto    = doc-fiscal.nr-doc-fis   AND
            docum-est.cod-emitente = doc-fiscal.cod-emitente AND
            docum-est.nat-operacao = doc-fiscal.nat-operacao
            NO-LOCK NO-ERROR.
       IF avail(docum-est) THEN 
           FIND FIRST item-doc-est OF docum-est NO-LOCK WHERE
               item-doc-est.it-codigo = ITEM.it-codigo NO-ERROR.
       ELSE DO:
           FIND nota-fiscal NO-LOCK WHERE
               nota-fiscal.cod-estabel = doc-fiscal.cod-estabel AND
               nota-fiscal.serie       = doc-fiscal.serie       AND
               nota-fiscal.nr-nota-fis = doc-fiscal.nr-doc-fis  NO-ERROR.
           IF avail(nota-fiscal) THEN 
               FIND FIRST it-nota-fisc OF nota-fiscal NO-LOCK WHERE
                    it-nota-fisc.it-codigo = ITEM.it-codigo NO-ERROR.

           /* MESSAGE "doc-fiscal.cod-estabel="     doc-fiscal.cod-estabel skip  
                      "doc-fiscal.serie="           doc-fiscal.serie       skip  
                      "doc-fiscal.nr-doc-fis="      doc-fiscal.nr-doc-fis  skip  
                      "item.it-codigo="             item.it-codigo         skip  
                      "natur-oper.tipo="            natur-oper.tipo        skip  
                      "avail(item-doc-est)="        avail(item-doc-est)    skip  
                      "avail(it-nota-fisc)="        avail(it-nota-fisc)          
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.                             */
       END.
    END. ELSE DO:
       FIND nota-fiscal NO-LOCK WHERE
           nota-fiscal.cod-estabel = doc-fiscal.cod-estabel AND
           nota-fiscal.serie       = doc-fiscal.serie AND
           nota-fiscal.nr-nota-fis = doc-fiscal.nr-doc-fis NO-ERROR.
       IF avail(nota-fiscal) THEN DO:
           ASSIGN c-nfe   = nota-fiscal.cod-chave-aces-nf-eletro
                  c-placa = nota-fiscal.placa.
           FIND FIRST it-nota-fisc OF nota-fiscal NO-LOCK WHERE
                it-nota-fisc.it-codigo = it-doc-fisc.it-codigo NO-ERROR.
           IF avail(it-nota-fisc) THEN
               ASSIGN c-cst-pis    = it-nota-fisc.cod-sit-tributar-pis
                      c-cst-cofins = it-nota-fisc.cod-sit-tributar-cofins.
       END.
    END.

    FIND emitente WHERE
         emitente.cod-emitente = doc-fiscal.cod-emitente
         NO-LOCK NO-ERROR.

    IF doc-fiscal.ind-ori-doc <> 1 THEN DO:
        FIND LAST sit-tribut-relacto  NO-LOCK WHERE
                  sit-tribut-relacto.cdn-tribut        = 2 /* 2=PIS */                                                                    AND
                  sit-tribut-relacto.idi-tip-docto     = natur-oper.tipo /* 1=Entrada e 2=Saida */                                        AND 
                 (sit-tribut-relacto.cod-estab         BEGINS doc-fiscal.cod-estabel   OR sit-tribut-relacto.cod-estab BEGINS "*")        AND
                 (sit-tribut-relacto.cod-natur-operac  BEGINS doc-fiscal.nat-operacao  OR sit-tribut-relacto.cod-natur-operac BEGINS "*") AND
                 (sit-tribut-relacto.cod-ncm           BEGINS ITEM.class-fiscal        OR sit-tribut-relacto.cod-ncm BEGINS "*")          AND
                 (sit-tribut-relacto.cod-item          BEGINS ITEM.it-codigo           OR sit-tribut-relacto.cod-item BEGINS "*")         AND
                 (sit-tribut-relacto.cdn-grp-emit      = emitente.cod-gr-forn          OR sit-tribut-relacto.cdn-grp-emit = 0)            AND
                 (sit-tribut-relacto.cdn-emitente      = doc-fiscal.cod-emitente       OR sit-tribut-relacto.cdn-emitente = 0)            AND
                  sit-tribut-relacto.dat-valid-inic   <= doc-fiscal.dt-docto NO-ERROR.
        IF avail(sit-tribut-relacto) THEN 
            ASSIGN c-cst-pis = STRING(sit-tribut-relacto.cdn-sit-tribut,"99").

        FIND LAST sit-tribut-relacto  NO-LOCK WHERE
                  sit-tribut-relacto.cdn-tribut        = 3 /* 3=COFINS */                                                                 AND
                  sit-tribut-relacto.idi-tip-docto     = natur-oper.tipo /* 1=Entrada e 2=Saida */                                        AND 
                 (sit-tribut-relacto.cod-estab         BEGINS doc-fiscal.cod-estabel   OR sit-tribut-relacto.cod-estab BEGINS "*")        AND
                 (sit-tribut-relacto.cod-natur-operac  BEGINS doc-fiscal.nat-operacao  OR sit-tribut-relacto.cod-natur-operac BEGINS "*") AND
                 (sit-tribut-relacto.cod-ncm           BEGINS ITEM.class-fiscal        OR sit-tribut-relacto.cod-ncm BEGINS "*")          AND
                 (sit-tribut-relacto.cod-item          BEGINS ITEM.it-codigo           OR sit-tribut-relacto.cod-item BEGINS "*")         AND
                 (sit-tribut-relacto.cdn-grp-emit      = emitente.cod-gr-forn          OR sit-tribut-relacto.cdn-grp-emit = 0)            AND
                 (sit-tribut-relacto.cdn-emitente      = doc-fiscal.cod-emitente       OR sit-tribut-relacto.cdn-emitente = 0)            AND
                  sit-tribut-relacto.dat-valid-inic   <= doc-fiscal.dt-docto NO-ERROR.
        IF avail(sit-tribut-relacto) THEN 
            ASSIGN c-cst-cofins = STRING(sit-tribut-relacto.cdn-sit-tribut,"99").
    END.
    
    ASSIGN c-cfop = substr(natur-oper.cod-cfop, 1, 4)
           c-nfe  = doc-fiscal.cod-chave-aces-nf-eletro.
    
    IF LENGTH(doc-fiscal.cgc) < 12 THEN
         ASSIGN c-id  = "'" + STRING(doc-fiscal.cgc,"999.999.999-99").
    ELSE ASSIGN c-id  = "'" + STRING(doc-fiscal.cgc,"99.999.999/9999-99").

    IF doc-fiscal.ind-sit-doc = 01 THEN
         ASSIGN c-situacao = "Normal".
    ELSE ASSIGN c-situacao = "Cancelada".

    CREATE reg.
    ASSIGN  v-perc-pis    = DECIMAL(SUBSTRING(it-doc-fisc.char-2, 22, 8))
            v-perc-cofins = DECIMAL(SUBSTRING(it-doc-fisc.char-2, 30, 8))
            icms-cst      = fn-cst-icms(INPUT IF natur-oper.tipo = 1 AND avail(item-doc-est) THEN item-doc-est.cd-trib-icm    ELSE it-nota-fisc.cd-trib-icm,   
                                        INPUT IF natur-oper.tipo = 1 AND avail(item-doc-est) THEN item-doc-est.aliquota-icm   ELSE it-nota-fisc.aliquota-icm,  
                                        INPUT natur-oper.ind-it-sub-dif,
                                        INPUT natur-oper.ind-tipo-vat,  
                                        INPUT natur-oper.consum-final,  
                                        INPUT natur-oper.subs-trib,     
                                        INPUT natur-oper.icms-subs-trib,
                                        INPUT IF natur-oper.tipo = 1 THEN natur-oper.perc-red-icm ELSE it-nota-fisc.perc-red-icm)
            tt-reg.cod-estabel     = STRING(doc-fiscal.cod-estabel) 
            tt-reg.cod-emitente    = STRING(doc-fiscal.cod-emitente)
            tt-reg.nome            = STRING(doc-fiscal.nome-ab-emi) 
            tt-reg.cnpj            = STRING(c-id)                   
            tt-reg.insc-estadual   = STRING(doc-fiscal.ins-estadual) 
            tt-reg.cidade          = STRING(doc-fiscal.cidade)       
            tt-reg.estado          = STRING(doc-fiscal.estado)      
            tt-reg.representante   = STRING("TODO:REPRESENTANTE")   
            tt-reg.transp          = STRING("TODO:Transportadora")
            tt-reg.especie         = STRING(doc-fiscal.esp-docto)    
            tt-reg.nat-op          = STRING(it-doc-fisc.nat-operacao)
            tt-reg.cfop            = STRING(c-cfop) /*substr(it-doc-fisc.nat-operacao, 1, 4)*/                
            tt-reg.un              = STRING(it-doc-fisc.un)         
            tt-reg.serie           = STRING(doc-fiscal.serie)       
            tt-reg.doc-fiscal      = STRING(doc-fiscal.nr-doc-fis)  
            tt-reg.dt-emissao      = STRING(doc-fiscal.dt-emis-doc) 
            tt-reg.dt-docto        = STRING(doc-fiscal.dt-docto)    
     /*?*/  tt-reg.cst             = STRING(c-cst-pis)
            tt-reg.seq             = STRING(it-doc-fisc.nr-seq-doc) 
            tt-reg.ITEM            = STRING("'" + STRING(ITEM.it-codigo))        
            tt-reg.desc-item       = STRING(ITEM.descricao-1 + ITEM.descricao-2) 
            tt-reg.ge-codigo       = STRING("TODO:GRUPO ESTOQUE")                
            tt-reg.fm-codigo       = STRING("TODO:FAMILIA")                      
            tt-reg.fm-cod-com      = STRING("TODO:FAMILIA COM")                  
            tt-reg.ncm             = STRING(it-doc-fisc.class-fiscal)            
            tt-reg.qtde            = it-doc-fisc.quantidade                      
            tt-reg.unitario        = STRING(doc-fiscal.vl-cont-doc)    
            tt-reg.total-produto   = it-doc-fisc.aliquota-icm          
            tt-reg.custo-medio     = it-doc-fisc.vl-tot-item / it-doc-fisc.quantidade                         
            tt-reg.custo-prod      = it-doc-fisc.vl-tot-item                                                  
            /*tt-reg.reprocessamento = 0 /*"TODO:Reprocessamento"*/*/
            tt-reg.ovo-natura      = 0 /*"TODO:Ovo in Natura"  */                                             
            tt-reg.embalagem       = 0 /*"TODO:Embalagem"      */                                             
            tt-reg.ggf             = 0 /*"TODO:GGF"            */                                             
            tt-reg.ovo-MT          = STRING("TODO:Voltando o ovo para o MT")                                  
            
            tt-reg.vl-contabil     = 0 /*TODO:Valor Contabil*/         
            tt-reg.perc-icms       = STRING("TODO:Perc ICMS")          
            tt-reg.icms-base       = it-doc-fisc.vl-bicms-it           
            tt-reg.icms-vi         = it-doc-fisc.vl-icms-it            
            /*tt-reg.icms-cst        = STRING(icms-cst)*/
            tt-reg.perc-pis        = STRING(v-perc-pis)                
            tt-reg.pis-base        = it-doc-fisc.val-base-calc-pis     
            tt-reg.pis-vi          = it-doc-fisc.val-pis               
            tt-reg.perc-cofins     = STRING(v-perc-cofins)             
            tt-reg.cofins-base     = it-doc-fisc.val-base-calc-cofins  
            tt-reg.cofins-vi       = it-doc-fisc.val-cofins            
            tt-reg.iss-base        = it-doc-fisc.vl-biss-it            
            tt-reg.perc-iss        = STRING(it-doc-fisc.aliquota-iss)  
            tt-reg.iss-vi          = it-doc-fisc.vl-iss-it
            tt-reg.impostos        = "TODO:Impostos"
            tt-reg.desc-fin        = 0 /*"TODO:Desconto Fin*/          
            tt-reg.frete           = STRING("TODO:Frete")              
            tt-reg.margem-cont     = STRING("TODO:Margem Cont")        
            tt-reg.desp-com        = STRING("TODO:Despesas Comerciais")
            tt-reg.desp-adm        = STRING("TODO:Despesa Adm")
            tt-reg.desp-fin        = STRING("TODO:Desp Fin")
            tt-reg.outros          = STRING(doc-fiscal.val-desp-outros)
                                /* = STRING("'" + SUBSTRING(doc-fiscal.char-2, 155, 44))*/
            tt-reg.chave-nfe       = STRING(fn-validar-chave-nfe(INPUT doc-fiscal.serie,
                                                                 INPUT doc-fiscal.nr-doc-fis,
                                                                 INPUT doc-fiscal.cod-emitente))
            tt-reg.situ            = STRING(c-situacao)
            tt-reg.nome-matriz     = STRING(emitente.nome-matriz)
            tt-reg.placa           = STRING(c-placa).
           
    /*i-linha = i-linha + 1.
    ChExcelApplication:Range("A" + STRING(i-linha)):VALUE = c-valor.*/
END PROCEDURE.
