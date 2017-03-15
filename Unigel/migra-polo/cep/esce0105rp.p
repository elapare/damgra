/********************************************************************************
** Copyright MM Pereira
** Todos os Direitos Reservados.
*******************************************************************************/
/*********************************************************************************
**
** Programa: esce0105
**
** Funcao..: Despesas Realizadas e Empenhadas
**
** Autor..: Moises Pereira
** Data...: 15/01/2017
*********************************************************************************/
{include/i-prgvrs.i esce0105RP 2.00.00.000}  /*** 010000 ***/
/*********************************************************************************
                                    DEFINIÄÂES
*********************************************************************************/   

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-TEMP-TABLES INTERNAS -=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
define temp-table tt-conta
      field cod-estabel as char 
      field ep-codigo as char
      field dt-trans as date
      field cod-emitente as int 
      field nr-trans as int
      field nro-docto as char
      field modulo as char
      field sequen-nf    like movto-estoq.sequen-nf 
      field esp-docto as inte
      field quantidade as dec
      field it-codigo as char
      field serie-docto as char
      field numero-ordem as int
      field parcela as int
      field num-pedido as int
      field narrativa like ordem-compra.narrativa
      field sc-codigo like movto-estoq.sc-codigo 
      field ct-codigo like movto-estoq.ct-codigo 
      field valor-rm as dec format "->>>,>>>,>>>,>>9.9999"
      field valor-nf as dec format "->>>,>>>,>>>,>>9.9999"
      field valor-oc as dec format "->>>,>>>,>>>,>>9.9999"
      index mov is primary unique

        cod-estabel
        sc-codigo
        ct-codigo
        nr-trans.

/* -=-=-=-=-=-=-=-=-=-=-=-=-=- VARIÊVEIS INTERNAS -=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
def temp-table tt-raw-digita
    field raw-digita as raw.


def new global shared var c-dir-spool-servid-exec as char                 no-undo.                     
def new global shared var i-num-ped-exec-rpw      as int                  no-undo.                           
DEF VAR c-arquivo-excel                           AS CHAR FORMAT "x(100)" NO-UNDO.
DEF VAR c-arquivo_txt                             AS CHAR FORMAT "x(50)"  NO-UNDO.
DEF VAR c-arquivo_xls                             AS CHAR FORMAT "x(50)"  NO-UNDO.
DEF VAR c-arquivo_2                               AS CHAR FORMAT "x(50)"  NO-UNDO.
def var ct-reg                                    as dec                  no-undo.
DEF VAR h-acomp                                   AS HANDLE               NO-UNDO.
DEF VAR i-acomp                                   AS INT                  NO-UNDO.
def var c-excel                                   as com-handle.
def var c-planilha                                as com-handle.
def var c-relatorio                               as com-handle.

/* -=-=-=-=-=-=-=-=-=-=-=-=-=- VARIÊVEIS GLOBAIS -=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- INCLUDES -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

{cdp/cdcfgman.i}
{cdp/cd0666.i}
{utp/ut-glob.i}
{method/dbotterr.i} /*rowErrors*/
{include/i-rpvar.i}
{cep\esce0105.i}
/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- BUFFERS -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
define buffer ccusto for emsuni.ccusto.
/* =-=-=-=-=-=-=-=-=-=-=-=-=-=- FRAMES E FORMS -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

/****************************************************************************/

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- PAR∂METROS -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */                                              
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.
/****************************************************************************/
                    
/* -=-=-=-=-=- Transferància de parÉmetros para temp-table padr∆o -=-=-=-=-=-*/
create tt-param.
raw-transfer raw-param to tt-param.

FOR EACH tt-raw-digita:
    CREATE tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
END.

/****************************************************************************/

/*********************************************************************************
                                   MAIN-BLOCK
*********************************************************************************/
FIND FIRST tt-param NO-ERROR.

FIND FIRST Param-global NO-LOCK NO-ERROR.
ASSIGN c-programa     = "esce0105":U
       c-versao       = "2.00":U
       c-revisao      = ".00.000":U
       c-empresa      = param-global.grupo
       c-sistema      = "Estoque"
       c-titulo-relat = "Despesas Realizadas e Empenhadas".

/* Outras includes */  
{include/i-rpcab.i} 
{include/i-rpout.i}

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

RUN pi-inicializa.

RUN pi-gera-planilha.

RUN pi-finaliza.

/** Fecha o output para arquivo **/
{include/i-rpclo.i}

/*********************************************************************************
                                   PROCEDURES
**********************************************************************************
**********************************************************************************/

PROCEDURE pi-inicializa:
    
    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

    RUN pi-inicializar IN h-acomp (INPUT 'Despesas Realizadas e Empenhadas').

    ASSIGN c-arquivo-excel = IF i-num-ped-exec-rpw = 0 
                             THEN tt-param.arquivo 
                             ELSE c-dir-spool-servid-exec + '\' + tt-param.arquivo.

    IF num-entries(c-arquivo-excel,".":U) <> 0 THEN DO:
        IF entry(num-entries(c-arquivo-excel,".":U),c-arquivo-excel,".":U) <> "xlsx" THEN
            assign c-arquivo-excel = replace(c-arquivo-excel,".":U + entry(num-entries(c-arquivo-excel,".":U),c-arquivo-excel,".":U),".xlsx":U).
    END.
    ELSE
        ASSIGN c-arquivo-excel = c-arquivo-excel + ".xlsx".

    ASSIGN c-arquivo-excel = REPLACE(c-arquivo-excel,"/","\")
           c-arquivo-excel = REPLACE(c-arquivo-excel,"xlsx","xls").

    OS-DELETE VALUE(c-arquivo-excel) NO-ERROR.

    IF tt-param.destino <> 3 THEN
        PUT "Arquivo excel gerado em " c-arquivo-excel SKIP.
    ELSE DO:
        ASSIGN c-arquivo-excel = "".

        PUT "Arquivo excel gerado em TERMINAL" SKIP.
    END.

    RETURN "OK".
END.

PROCEDURE pi-finaliza:

    RUN pi-finalizar IN h-acomp.
    
    RETURN "OK".
END.


procedure pi-gera-planilha:

    def var i-nr-trans  as integer                 no-undo.
    def var i-lidos     as integer                 no-undo.
    def var i-esp       as INTEGER                 no-undo.
    def var i-esp-1     as integer EXTENT 40       no-undo.
    def var dt-mov      as date                    no-undo.
    def var d-valor     as dec                     no-undo.
    def var dt-ini      as date label "Dt.Inicial" no-undo.
    def var dt-fim      as date label "Dt.Final"   no-undo.
    def var c-est-ini   as char label "Estab.Ini"  no-undo.
    def var c-est-fim   as char label "Estab.Fim"  no-undo.
    DEF VAR i-esp-docto AS INT                     NO-UNDO.
    DEF VAR c-esp-docto AS CHAR EXTENT 38          NO-UNDO.
    
     /*solic-318*/ 
     /*solic-318*/ 
    def var c-ct-codigo-ini      AS CHAR     FORMAT "x(08)"       INITIAL ""             NO-UNDO.
    def var c-ct-codigo-fim      AS CHAR     FORMAT "x(08)"       INITIAL "ZZZZZZZZ"     NO-UNDO.
    def var c-sc-codigo-ini      AS CHAR     FORMAT "x(08)"       INITIAL ""             NO-UNDO.
    def var c-sc-codigo-fim      AS CHAR     FORMAT "x(08)"       INITIAL "ZZZZZZZZ"     NO-UNDO.
    def var dt-emissao-ini       AS DATE     FORMAT "99/99/9999"  INITIAL 01/01/2012     NO-UNDO.
    def var dt-emissao-fim       AS DATE     FORMAT "99/99/9999"  INITIAL TODAY          NO-UNDO.
    DEF VAR l-detalhe            AS LOGICAL                       INITIAL no             NO-UNDO.


    assign c-est-ini        = tt-param.cod-estabel-ini 
           c-est-fim        = tt-param.cod-estabel-fim 
           c-ct-codigo-ini  = tt-param.ct-codigo-ini   
           c-ct-codigo-fim  = tt-param.ct-codigo-fim            
           c-sc-codigo-ini  = tt-param.sc-codigo-ini   
           c-sc-codigo-fim  = tt-param.sc-codigo-fim   
           dt-ini           = tt-param.periodo-ini     
           dt-fim           = tt-param.periodo-fim    
           l-detalhe        = tt-param.detalhado.

    assign i-esp-1[1] =0
           i-esp-1[2] =0
           i-esp-1[3] =0
           i-esp-1[4] =21
           i-esp-1[5] =28
           i-esp-1[6] =28
           i-esp-1[7] =28
           i-esp-1[8] =0
           i-esp-1[9] =0
           i-esp-1[10] =0
           i-esp-1[11] =0
           i-esp-1[12] =0
           i-esp-1[13] =0
           i-esp-1[14] =21
           i-esp-1[15] =0
           i-esp-1[16] =0
           i-esp-1[17] =0
           i-esp-1[18] =21
           i-esp-1[19] =21
           i-esp-1[20] =21
           i-esp-1[21] =21
           i-esp-1[22] =21
           i-esp-1[23] =0
           i-esp-1[24] =0
           i-esp-1[25] =0
           i-esp-1[26] =0
           i-esp-1[27] =0
           i-esp-1[28] =28
           i-esp-1[29] =0
           i-esp-1[30] =28
           i-esp-1[31] =28
           i-esp-1[32] =0
           i-esp-1[33] =0
           i-esp-1[34] =0
           i-esp-1[35] =0
           i-esp-1[36] =0
           i-esp-1[37] =0
           i-esp-1[38] =0.

    REPEAT i-esp-docto = 1 TO 38:
        ASSIGN c-esp-docto[i-esp-docto] = entry(i-esp-docto, {ininc/i03in218.i 03}) .
    END.


    EMPTY temp-table tt-conta.
    
    ASSIGN i-nr-trans = 0.

    /******************************************************************************************************
    ******************************************************************************************************
    CONTABILIDADE
    ******************************************************************************************************
    ******************************************************************************************************/

    FOR EACH estabelec NO-LOCK
          where estabelec.cod-estabel >= c-est-ini 
            AND estabelec.cod-estabel <= c-est-fim  
          BREAK BY estabelec.ep-codigo:
        IF FIRST-OF(estabelec.ep-codigo) THEN DO:
            
            FOR EACH item_lancto_ctbl NO-LOCK USE-INDEX tmlnctcb_data_lancto
                where item_lancto_ctbl.cod_empresa = string(estabelec.ep-codigo) 
                  AND item_lancto_ctbl.dat_lancto_ctbl >=  dt-ini 
                  AND item_lancto_ctbl.dat_lancto_ctbl <=  dt-fim:

                i-lidos = i-lidos + 1.
                if i-lidos MODULO 100 = 0 then 
                run pi-acompanhar in h-acomp (input "Despesa FGL : " + string(item_lancto_ctbl.dat_lancto_ctbl) + " - " + STRING(i-lidos) ).
                
                IF item_lancto_ctbl.cod_estab < c-est-ini OR 
                   item_lancto_ctbl.cod_estab > c-est-fim THEN NEXT.
    
                IF item_lancto_ctbl.cod_plano_cta_ctbl <> "Brasil"  THEN NEXT.
    
                IF item_lancto_ctbl.cod_cta_ctbl    < c-ct-codigo-ini OR
                   item_lancto_ctbl.cod_cta_ctbl    > c-ct-codigo-fim OR
                   item_lancto_ctbl.cod_ccusto      < c-sc-codigo-ini OR
                   item_lancto_ctbl.cod_ccusto      > c-sc-codigo-fim OR
                   item_lancto_ctbl.ind_sit_lancto_ctbl   <> "ctbz"   OR
                   item_lancto_ctbl.cod_indic_econ        <> "real"   THEN NEXT.
    
                FIND first lancto_ctbl of item_lancto_ctbl NO-ERROR.
                
                IF lancto_ctbl.cod_modul_dtsul <> "fgl" THEN NEXT.
    
                if l-detalhe then i-nr-trans = i-nr-trans + 1.
    
                find first tt-conta where tt-conta.nr-trans       =  i-nr-trans and
                                          tt-conta.cod-estabel    = item_lancto_ctbl.cod_estab and
                                          tt-conta.ct-codigo      = item_lancto_ctbl.cod_cta_ctbl  and
                                          tt-conta.sc-codigo      = item_lancto_ctbl.cod_ccusto   no-error.
    
                if not avail tt-conta then do:
                  create tt-conta.
                  assign tt-conta.nr-trans       = i-nr-trans
                         tt-conta.cod-estabel    = item_lancto_ctbl.cod_estab 
                         tt-conta.ct-codigo      = item_lancto_ctbl.cod_cta_ctbl  
                         tt-conta.sc-codigo      = item_lancto_ctbl.cod_ccusto.
                end.
                
                ASSIGN tt-conta.ep-codigo = item_lancto_ctbl.cod_empresa
                       tt-conta.modulo    = "FGL-" + item_lancto_ctbl.ind_natur_lancto_ctbl
                       d-valor            = item_lancto_ctbl.val_lancto_ctbl * (if item_lancto_ctbl.ind_natur_lancto_ctbl = "DB" then 1 else -1)
                       tt-conta.valor-nf  = tt-conta.valor-nf + d-valor.
    
                if l-detalhe then do:     
                    ASSIGN tt-conta.dt-trans     = item_lancto_ctbl.dat_lancto_ctbl.
                           tt-conta.nro-docto    = string(item_lancto_ctbl.num_lancto_ctbl). /*lancto_ctbl.num_lancto_ctbl lancto_ctbl.num_lote_ctbl*/
                           tt-conta.sequen-nf    = item_lancto_ctbl.num_seq_lancto_ctbl.
                           tt-conta.narrativa    = item_lancto_ctbl.des_histor_lancto_ctbl.  
                END.
            END.
        END.
    END.

    /******************************************************************************************************
    ******************************************************************************************************
    ESTOQUE
    ******************************************************************************************************
    ******************************************************************************************************/

    FOR EACH movto-estoq NO-LOCK USE-INDEX data-conta
        WHERE movto-estoq.dt-trans    >= dt-ini 
          AND movto-estoq.dt-trans    <= dt-fim
          AND movto-estoq.cod-estabel >= c-est-ini
          AND movto-estoq.cod-estabel <= c-est-fim:
    
        ASSIGN i-lidos = i-lidos + 1.
    
        if i-lidos MODULO 100 = 0 then 
          run pi-acompanhar in h-acomp (input "Despesa CEP : " + string(movto-estoq.dt-trans) + " " + STRING(i-lidos,"99999999") ).
    
        FIND FIRST estabelec NO-LOCK
            WHERE estabelec.cod-estabel = movto-estoq.cod-estabel NO-ERROR.
    
        IF i-esp-1[movto-estoq.esp-docto] = 0 THEN NEXT.
    
        IF movto-estoq.sc-codigo < c-sc-codigo-ini   OR
           movto-estoq.sc-codigo > c-sc-codigo-fim   OR
           movto-estoq.ct-codigo < c-ct-codigo-ini   OR
           movto-estoq.ct-codigo > c-ct-codigo-fim   THEN NEXT.
    
        FIND FIRST ITEM NO-LOCK
            WHERE ITEM.it-codigo = movto-estoq.it-codigo NO-ERROR.
    
        IF ITEM.ge-codigo > 40 AND item.ge-codigo < 50 THEN NEXT.
        /*
        find first plano_cta_unid_organ no-lock 
             WHERE plano_cta_unid_organ.cod_unid_organ         = estabelec.ep-codigo
               and plano_cta_unid_organ.ind_tip_plano_cta_ctbl = "Prim†rio" no-error.
    
        FIND FIRST plano_cta_ctbl
             WHERE plano_cta_ctbl.cod_plano_cta_ctbl = plano_cta_unid_organ.cod_plano_cta_ctbl NO-LOCK NO-ERROR.
    
        FIND FIRST cta_ctbl_integr NO-LOCK 
             WHERE cta_ctbl_integr.cod_modul_dtsul    = "CEP"
               AND cta_ctbl_integr.cod_plano_cta_ctbl = plano_cta_ctbl.cod_plano_cta_ctbl
               AND cta_ctbl_integr.cod_cta_ctbl       = movto-estoq.ct-codigo 
               AND cta_ctbl_integr.ind_finalid_ctbl   = "Consumo"
               AND cta_ctbl_integr.dat_inic_valid    <= movto-estoq.dt-trans
               AND cta_ctbl_integr.dat_fim_valid      > movto-estoq.dt-trans NO-ERROR.
        IF NOT AVAIL cta_ctbl_integr THEN NEXT.
        */
        if l-detalhe then i-nr-trans = i-nr-trans + 1.
    
        find first tt-conta 
            where tt-conta.nr-trans       =  i-nr-trans 
              AND tt-conta.cod-estabel    = movto-estoq.cod-estabel 
              AND tt-conta.ct-codigo      = movto-estoq.ct-codigo  
              AND tt-conta.sc-codigo      = movto-estoq.sc-codigo   no-error.
        
        if not avail tt-conta then do:
            create tt-conta.
            assign tt-conta.nr-trans    =  i-nr-trans
                   tt-conta.cod-estabel    = movto-estoq.cod-estabel 
                   tt-conta.ct-codigo      = movto-estoq.ct-codigo  
                   tt-conta.sc-codigo      = movto-estoq.sc-codigo  .
        END.
        ASSIGN tt-conta.ep-codigo = estabelec.ep-codigo
               tt-conta.modulo = c-esp-docto[movto-estoq.esp-docto]
               d-valor = (movto-estoq.valor-ggf-m[1] +
                          movto-estoq.valor-mat-m[1] +
                          movto-estoq.valor-mob-m[1] )  .
        
        if d-valor = 0 then do:
            FIND FIRST item-estab NO-LOCK 
                where item-estab.it-codigo   = movto-estoq.it-codigo  
                  AND item-estab.cod-estabel = movto-estoq.cod-estabel NO-ERROR.
            
            FIND LAST pr-it-per NO-LOCK 
                WHERE pr-it-per.cod-estabel = movto-estoq.cod-estabel 
                  AND pr-it-per.it-codigo   = movto-estoq.it-codigo  
                  AND pr-it-per.periodo    <= movto-estoq.dt-trans NO-ERROR.
            
            if avail pr-it-per then do:
                ASSIGN d-valor  =   (round(movto-estoq.quantidade * (IF AVAIL item-estab AND item-estab.val-unit-ggf-m[1] <> ? THEN item-estab.val-unit-ggf-m[1] ELSE 
                                    IF AVAIL pr-it-per AND pr-it-per.val-unit-ggf-m[1] <> ? THEN pr-it-per.val-unit-ggf-m[1]  ELSE 0)  ,2))
                                  + (round(movto-estoq.quantidade * (IF AVAIL item-estab AND item-estab.val-unit-mat-m[1] <> ? THEN item-estab.val-unit-mat-m[1] ELSE 
                                    IF AVAIL pr-it-per AND pr-it-per.val-unit-mat-m[1] <> ? THEN pr-it-per.val-unit-mat-m[1]  ELSE 0)  ,2))
                                  + (round(movto-estoq.quantidade * (IF AVAIL item-estab AND item-estab.val-unit-mob-m[1] <> ? THEN item-estab.val-unit-mob-m[1] ELSE 
                                    IF AVAIL pr-it-per AND pr-it-per.val-unit-mob-m[1] <> ? THEN pr-it-per.val-unit-mob-m[1]  ELSE 0)  ,2)).
            END.
        END.
        
        if i-esp-1[movto-estoq.esp-docto] = 28 then 
            tt-conta.valor-rm = tt-conta.valor-rm + d-valor * if movto-estoq.tipo-trans = 1 then -1 else 1.
        if i-esp-1[movto-estoq.esp-docto] = 21 then 
            tt-conta.valor-nf = tt-conta.valor-nf + d-valor * if movto-estoq.tipo-trans = 1 then -1 else 1.
        
        if l-detalhe THEN do:                 
            ASSIGN tt-conta.cod-emitente   = movto-estoq.cod-emitente 
                   tt-conta.narrativa      = movto-estoq.descricao-db 
                   tt-conta.dt-trans       = movto-estoq.dt-trans 
                   tt-conta.esp-docto      = movto-estoq.esp-docto 
                   tt-conta.it-codigo      = movto-estoq.it-codigo 
                   tt-conta.nro-docto      = movto-estoq.nro-docto 
                   tt-conta.quantidade     = movto-estoq.quantidade 
                   tt-conta.sequen-nf      = movto-estoq.sequen-nf 
                   tt-conta.serie-docto    = movto-estoq.serie-docto
                   tt-conta.narrativa      = (if item.tipo-contr = 2 then (item.desc-item + item.narrativa ) else 
                                             if trim(movto-estoq.descricao-db) = "" then item.desc-item else   movto-estoq.descricao-db)
                   tt-conta.narrativa      = replace(tt-conta.narrativa,chr(10)," ").                                                                             

            if movto-estoq.esp-docto = 21 then do:
    
                find first item-doc-est no-lock 
                    WHERE item-doc-est.cod-emitente = movto-estoq.cod-emitente 
                      AND item-doc-est.it-codigo    = movto-estoq.it-codigo 
                      and item-doc-est.nat-operacao = movto-estoq.nat-operacao 
                      AND item-doc-est.nro-docto    = movto-estoq.nro-docto 
                      AND item-doc-est.sequencia    = movto-estoq.sequen-nf 
                      AND item-doc-est.serie-docto  = movto-estoq.serie-docto no-error.
                if avail item-doc-est then do:
                    ASSIGN tt-conta.numero-ordem = item-doc-est.numero-ordem
                           tt-conta.parcela      = item-doc-est.parcela
                           tt-conta.num-pedido   = item-doc-est.num-pedido.
                END.
            END. /* esp-docto 21*/
        END. /*if l-detalhe*/
    END.
 
    /******************************************************************************************************
    ******************************************************************************************************
    COMPRAS
    ******************************************************************************************************
    ******************************************************************************************************/

    for each estabelec 
        where estabelec.cod-estabel >= c-est-ini  
          AND estabelec.cod-estabel <= c-est-fim no-lock,
        each prazo-compra use-index data no-lock
        where prazo-compra.data-entrega >= dt-ini 
          and prazo-compra.data-entrega <= dt-fim 
          AND prazo-compra.situacao = 2,
        each ordem-compra of prazo-compra 
        where ordem-compra.cod-estabel =  estabelec.cod-estabel 
          AND ordem-compra.sc-codigo >= c-sc-codigo-ini 
          AND ordem-compra.sc-codigo <= c-sc-codigo-fim 
          AND ordem-compra.ct-codigo >= c-ct-codigo-ini 
          AND ordem-compra.ct-codigo <= c-ct-codigo-fim no-lock,
        first item fields (desc-item) 
        where item.it-codigo = ordem-compra.it-codigo 
          AND item.tipo-contr <> 1 no-lock.

        IF ordem-compra.ct-codigo = "" THEN NEXT.

        /*
            find first plano_cta_unid_organ no-lock 
                 WHERE plano_cta_unid_organ.cod_unid_organ         = estabelec.ep-codigo
                   and plano_cta_unid_organ.ind_tip_plano_cta_ctbl = "Prim†rio" no-error.
            
            FIND FIRST plano_cta_ctbl
                 WHERE plano_cta_ctbl.cod_plano_cta_ctbl = plano_cta_unid_organ.cod_plano_cta_ctbl NO-LOCK NO-ERROR.
            
            FIND FIRST cta_ctbl_integr NO-LOCK 
                 WHERE cta_ctbl_integr.cod_modul_dtsul    = "CEP"
                   AND cta_ctbl_integr.cod_plano_cta_ctbl = plano_cta_ctbl.cod_plano_cta_ctbl
                   AND cta_ctbl_integr.cod_cta_ctbl       = ordem-compra.ct-codigo
                   AND cta_ctbl_integr.ind_finalid_ctbl   = "Consumo"
                   AND cta_ctbl_integr.dat_inic_valid    <= prazo-compra.data-entrega
                   AND cta_ctbl_integr.dat_fim_valid      > prazo-compra.data-entrega NO-ERROR.
            IF NOT AVAIL cta_ctbl_integr THEN NEXT.
        */
              
        ASSIGN i-lidos = i-lidos + 1.
        
        if i-lidos MODULO 100 = 0 then 
            run pi-acompanhar in h-acomp (input "selecionando despesa COMPRAS: " + STRING(i-lidos) ).
        
        if l-detalhe then i-nr-trans = i-nr-trans + 1.
        
        find first tt-conta 
            where tt-conta.nr-trans       =  i-nr-trans 
              AND tt-conta.cod-estabel    = ordem-compra.cod-estabel 
              AND tt-conta.ct-codigo      = ordem-compra.ct-codigo  
              AND tt-conta.sc-codigo      = ordem-compra.sc-codigo   no-error.

        if not avail tt-conta then do:
            
            create tt-conta.
            assign tt-conta.nr-trans       =  i-nr-trans
                   tt-conta.cod-estabel    = ordem-compra.cod-estabel 
                   tt-conta.ct-codigo      = ordem-compra.ct-codigo
                   tt-conta.sc-codigo      = ordem-compra.sc-codigo                     .
        END.

        ASSIGN tt-conta.ep-codigo = estabelec.ep-codigo
               tt-conta.modulo    = "MCP-OC"  
               d-valor            = ordem-compra.preco-un  * prazo-compra.quant-saldo
               tt-conta.valor-oc  = tt-conta.valor-oc + d-valor.
        
        if l-detalhe THEN do:                 
            ASSIGN tt-conta.cod-emitente   = ordem-compra.cod-emitente 
                   tt-conta.dt-trans       = prazo-compra.data-entrega
                   tt-conta.esp-docto      = 0
                   tt-conta.it-codigo      = ordem-compra.it-codigo 
                   tt-conta.num-pedido     = ordem-compra.num-pedido
                   tt-conta.numero-ordem   = ordem-compra.numero-ordem                    
                   tt-conta.parcela        = prazo-compra.parcela
                   tt-conta.narrativa      = if trim(ordem-compra.narrativa) = "" then item.desc-item else ordem-compra.narrativa
                   tt-conta.narrativa      = replace(tt-conta.narrativa,chr(10)," ")
                   tt-conta.quantidade     = prazo-compra.quant-saldo .
        END.
    END.
    
    /******************************************************************************************************
    ******************************************************************************************************
    EXPORTA ARQUIVO CSV
    ******************************************************************************************************
    ******************************************************************************************************/

    c-arquivo_txt = session:TEMP-DIRECTORY + "esce0105_" + replace(STRING(TIME,"HH:MM:SS"),":","") + ".csv".
    OUTPUT TO VALUE(c-arquivo_txt) NO-CONVERT.
    PUT ";" SKIP.
    PUT "RELAT‡RIO DE DESPESAS REALIZADAS E EMPENHADAS" ";" SKIP.

    PUT ";;;;;;;;;;;;;;;;;;;;" SKIP.
    PUT ";;;;;;;;;;;;;;;;;;;;" SKIP.
    PUT ";;;;;;;;;;;;;;;;;;;;" SKIP.
    
    put  unformatted
                 "Estab. "
              ";" "C.Custo" 
              ";" "Descricao Centro de custo"  
              ";" "Conta Contabil"  
              ";" "Titulo Conta"            
              ";" "Realizado RMs" 
              ";" "Realizado NFs"
              ";" "Empenhado OCs"
              ";" "Total Despesa"  
              ";" "Data Mov."
              ";" "Origem"              
              ";" "Cod.Emitente"
              ";" "Numero Ordem"
              ";" "Parc."
              ";" "Nr.Pedido"
              ";" "Nr.Docto." 
              ";" "Serie"
              ";" "Seq"
              ";" "Esp"
              ";" "Quantidade"
              ";" "Item" 
              ";" "Narrativa"
              
              
           skip.
    
    ct-reg = 0.
    
    
    for each tt-conta.
 
        ct-reg = ct-reg + 1.
           
        if ct-reg MODULO 100 = 0 then 
            run pi-acompanhar in h-acomp (input "Listando despesa : " + STRING(ct-reg) ).

        FIND FIRST estabelec NO-LOCK 
            WHERE estabelec.cod-estabel = tt-conta.cod-estabel NO-ERROR.

        find first plano_cta_unid_organ no-lock 
            WHERE plano_cta_unid_organ.cod_unid_organ         = estabelec.ep-codigo
              and plano_cta_unid_organ.ind_tip_plano_cta_ctbl = "Prim†rio" no-error.
        
        FIND FIRST plano_cta_ctbl
            WHERE plano_cta_ctbl.cod_plano_cta_ctbl = plano_cta_unid_organ.cod_plano_cta_ctbl NO-LOCK NO-ERROR.
        
        FIND FIRST cta_ctbl
            WHERE cta_ctbl.cod_plano_cta_ctbl = plano_cta_ctbl.cod_plano_cta_ctbl
              AND cta_ctbl.cod_cta_ctbl       = tt-conta.ct-codigo NO-LOCK NO-ERROR.
        
        FIND first plano_ccusto 
            WHERE plano_ccusto.dat_fim_valid >= today
              AND plano_ccusto.cod_empresa = string(estabelec.ep-codigo)  NO-LOCK NO-ERROR.
        
        FIND FIRST ccusto
            WHERE ccusto.cod_plano_ccusto = plano_ccusto.cod_plano_ccusto
              AND ccusto.cod_ccusto       = tt-conta.sc-codigo NO-LOCK NO-ERROR.
        
        put  unformatted
             tt-conta.cod-estabel  
          ";" tt-conta.sc-codigo 
          ";" if avail ccusto then ccusto.des_tit_ctbl  else ""  
          ";" tt-conta.ct-codigo
          ";" IF AVAIL cta_ctbl THEN cta_ctbl.des_tit_ctbl ELSE ""
          ";" tt-conta.valor-rm
          ";" tt-conta.valor-nf
          ";" tt-conta.valor-oc
          ";" (tt-conta.valor-rm +
          tt-conta.valor-nf +
          tt-conta.valor-oc)  

          ";" tt-conta.dt-trans 
          ";" tt-conta.modulo              
          ";" tt-conta.cod-emitente 
          ";" tt-conta.numero-ordem 
          ";" tt-conta.parcela 
          ";" tt-conta.num-pedido
          ";" tt-conta.nro-docto 
          ";" tt-conta.serie-docto              
          ";" tt-conta.sequen-nf     
          ";" tt-conta.esp-docto
          ";" tt-conta.quantidade  
          ";" tt-conta.it-codigo               
          ";" replace(replace(replace(replace(replace(tt-conta.narrativa ,";"," "),chr(10)," "),chr(9)," "),chr(8)," "),"  "," ")
       skip.
    END.
    output close.

    /******************************************************************************************************
    ******************************************************************************************************
    GERACAO ARQUIVO EXCEL
    ******************************************************************************************************
    ******************************************************************************************************/
 
    c-arquivo_xls = session:TEMP-DIRECTORY + "esce0105_" + replace(STRING(TIME,"HH:MM:SS"),":","") + ".xls".
    
    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.
    
    /* cria planilha*/
    IF c-arquivo-excel = "" THEN
        c-arquivo_2 = c-arquivo_xls.       
    ELSE
        c-arquivo_2 = c-arquivo-excel.

    c-planilha  = c-excel:Workbooks:OPEN(c-arquivo_txt).

    c-planilha:SAVEas(c-arquivo_2,1,,,,,).
             
    c-planilha:CLOSE().

    DOS SILENT COPY VALUE(c-arquivo_txt) V:\TEMP.
            
    ASSIGN c-planilha  = c-excel:Workbooks:OPEN(c-arquivo_2)
           c-relatorio = c-excel:Sheets:item(1).
            
    RUN pi-salva-planilha.

    IF c-arquivo-excel = "" THEN DO:
        c-excel:visible = yes.
    END.
    ELSE DO:
        c-excel:Cursor = -4143.
        c-excel:QUIT().
    END.

    RELEASE OBJECT c-relatorio.
    RELEASE OBJECT c-planilha.
    RELEASE OBJECT c-excel.

end procedure.

PROCEDURE pi-salva-planilha:

    c-relatorio:range("a2"):Select.
    c-relatorio:range("a2"):Font:FontStyle = "Negrito".
    c-relatorio:range("a2"):Font:Size = 14.

    c-relatorio:range("a6:U6"):Interior:ColorIndex = 55.
    c-relatorio:range("a6:U6"):Font:Name = "Arial".
    c-relatorio:range("a6:U6"):Font:FontStyle = "Negrito".
    c-relatorio:range("a6:U6"):Font:Size = 10.
    c-relatorio:range("a6:U6"):Font:ColorIndex = 2.
    
    c-relatorio:Rows("6:6"):Autofilter (,,,).

    c-relatorio:Columns("G:G"):NumberFormat = "@".
    c-relatorio:Columns("F:I"):NumberFormat = "#.##0,00".
    
    c-relatorio:range("E5"):VALUE = "TOTAL".
   
    c-relatorio:range("F5"):Select.
    c-relatorio:range("F5"):value = ("=SUBTOTAL(9,F7:F" + trim(string(ct-reg + 10)) + ")").
    c-relatorio:range("G5"):value = ("=SUBTOTAL(9,G7:G" + trim(string(ct-reg + 10)) + ")").
    c-relatorio:range("H5"):value = ("=SUBTOTAL(9,H7:H" + trim(string(ct-reg + 10)) + ")").
    c-relatorio:range("I5"):value = ("=SUBTOTAL(9,I7:I" + trim(string(ct-reg + 10)) + ")").                                       

    c-relatorio:range("E5:I5"):Font:FontStyle = "Negrito".

    
    c-relatorio:Cells:Select.
    c-relatorio:Cells:EntireColumn:AutoFit.
    c-relatorio:Columns("A:A"):ColumnWidth = 8.
    c-relatorio:Columns("U:U"):ColumnWidth = 60.
    c-relatorio:range("a2"):Select.

    c-relatorio:range("a1"):Select.

    c-planilha:SAVE().

END PROCEDURE.

