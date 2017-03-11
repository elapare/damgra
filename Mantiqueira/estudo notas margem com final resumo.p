DEF VAR h-acomp              AS HANDLE NO-UNDO.
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

RUN pi-inicializar IN h-acomp(INPUT "Acompanhamento Relatorio").

find first param-global no-lock.


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
    FIELD v_num_tip_aces_usuar  AS INT
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
    FIELD l-documento           AS LOG  /* Fim - Folder Par */
    FIELD l-fat-dupl           AS LOG
    FIELD l-gera-duplicata      AS LOG
    field l-medio               as log.


active-window:width = 180.


DEFINE TEMP-TABLE tt-notas
    FIELD cod-estabel      LIKE nota-fiscal.cod-estabel
    FIELD serie            LIKE nota-fiscal.serie 
    FIELD nr-nota-fis      LIKE nota-fiscal.nr-nota-fis
    FIELD identific        AS int
    field motivo           as char
    INDEX ch-tt-notas IS PRIMARY UNIQUE  cod-estabel
                                         serie
                                         nr-nota-fis.




 define temp-table tt-dados    
       field  cod-estabel           as char   
       field  cod-emitente          as int    
       field  nome-ab-cli           as char          
       field  cgc                   as char   
       field  cidade                as char   
       field  estado                as char   
       field  no-ab-reppri          as char    
       field  nome-transp           as char                     
       field  cod-oper              as char     
       field  nat-operacao          as char 
       field  cod-cfop              as char 
       field  fat-dup               as char
       field  un                    as char
       field  serie-docto           as char 
       field  nro-docto             as char 
       field  dt-docto              as date 
       field  it-codigo             as char 
       field  desc-item             as char
       field  ge-codigo             as int
       field  fm-codigo             as char
       field  fm-cod-com            as char
       field  qt-faturada           as dec   
       field  preco                 as dec 
       field  tot-item              as dec 
       field  receita-bruta         as dec 
       field  impostos              as dec
       field  valor-desc            as dec
       field  receita-liquida       as dec
       field  custo-prod            as dec
       field  custo-total           as dec
       field  custo-ovo             as dec
       field  custo-emb             as dec
       field  custo-ggf             as dec    
       field  valor-frete           as dec
       field  margem-bruta          as dec
       field  perc-margem-bruta     as dec
       field  valor-comerc          as dec
       field  valor-adm             as dec
       field  valor-fin             as dec                                 
       field  margem-liquida        as dec         
       field  perc-margem-liquida   as dec
       field  motivo                as char
       index chave
               cod-estabel  /* l-estabel         */
               ge-codigo    /* l-grupo           */
               fm-codigo    /* l-familia         */
               fm-cod-com   /* l-familia-com     */
               it-codigo    /* l-item            */
               cod-emitente /* l-emitente        */
               no-ab-reppri /* l-representante   */
               estado       /* l-estado          */
               cidade       /* l-cidade          */
               fat-dup      /* l-fat-dup         */
                  /* l-documento       */
            
         .
            


/*definicao de variaveis para propositos gerais*/
DEF VAR v-aliq-pis                   as dec.
DEF VAR v-aliq-cof                   as dec.
DEF VAR aux-dt-emis-nota             LIKE it-nota-fisc.dt-emis-nota.
def var de-cotacao                   as dec FORMAT ">>>,>>9.99999999" no-undo.
def var v-pis                        as dec format "->,>>>,>>9.99".
def var v-cofins                     as dec format "->,>>>,>>9.99".
DEF VAR valor-liquido                as dec format "->>>,>>>,>>9.99".
DEF VAR valor-margem                 as dec format "->>>,>>>,>>9.99" label "Margem".
DEF VAR aux-vl-despes-it             as dec format "->>,>>>,>>9.99".
DEF VAR aux-vl-merc-liq              as dec format "->>>,>>>,>>9.99".
DEF VAR encargos-financ              as dec format "->>,>>>,>>9.99".
DEF VAR aux-qt-faturada              as dec format "->>>,>>>,>>9.999" INIT 0.
DEF VAR aux-peso-liq-fat             as dec format "->>>,>>>,>>9.999".
DEF VAR aux-vl-tot-item              as dec format "->>>,>>>,>>9.99".
DEF VAR aux-vl-ipi-it                as dec format "->>,>>>,>>9.99".
DEF VAR aux-vl-ipi-it-jr             as dec format "->>,>>>,>>9.99".
DEF VAR aux-vl-icms-it-jr            as dec format "->>,>>>,>>9.99".
DEF VAR aux-vl-icms-it               as dec format "->>,>>>,>>9.99".
def var v-pis-dev                    as dec format "->,>>>,>>9.99".
def var v-cofins-dev                 as dec format "->,>>>,>>9.99".
DEF VAR valor-liquido-dev            as dec format "->>>,>>>,>>9.99".
DEF VAR aux-vl-despes-it-dev         as dec format "->>,>>>,>>9.99".
DEF VAR aux-vl-merc-liq-dev          as dec format "->>>,>>>,>>9.99".
DEF VAR encargos-financ-dev          as dec format "->>,>>>,>>9.99".
DEF VAR aux-qt-dev                   as dec format "->>>,>>>,>>9.999" INIT 0.
DEF VAR aux-qt-faturada-dev          as dec format "->>>,>>>,>>9.999" INIT 0.
DEF VAR aux-peso-liq-fat-dev         as dec format "->>>,>>>,>>9.999".
DEF VAR aux-vl-tot-item-dev          as dec format "->>>,>>>,>>9.99".
DEF VAR aux-vl-ipi-it-dev            as dec format "->>,>>>,>>9.99".
DEF VAR aux-vl-icms-it-dev           as dec format "->>,>>>,>>9.99".


DEF VAR tt-aux-peso-liq-fat-dev     as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-aux-qt-faturada-dev      as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-aux-vl-tot-item-dev      as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-aux-vl-icms-it-dev       as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-aux-vl-ipi-it-dev        as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-v-cofins-dev             as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-v-pis-dev                as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-encargos-financ-dev      as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-valor-liquido-dev        as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR tt-aux-vl-despes-it-dev     as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR v-impostos-dev              as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.

DEF VAR v-receita-bruta              as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR v-receita-liquida            as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR v-margem-bruta               as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR v-margem-liquida             as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR v-perc-margem-bruta          as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.
DEF VAR v-perc-margem-liquida        as dec format "->>>,>>>,>>9.99" NO-UNDO INIT 0.


DEFINE VARIABLE dt-atu AS DATE        NO-UNDO.
DEF VAR v-impostos                   as dec format "->>>,>>>,>>9.99".
DEF VAR v-custo-total                as dec format "->>>,>>>,>>9.99".
DEF VAR v-custo-medio                as dec format "->>>,>>>,>>9.99".
DEF VAR v-preco-unit                 as dec format "->>>,>>>,>>9.99".
DEFINE VARIABLE  c-cod-oper AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dt-emis-nota-ini AS DATE        NO-UNDO.
DEFINE VARIABLE dt-emis-nota-fim AS DATE        NO-UNDO.
DEFINE VARIABLE cod-estabel-ini AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cod-estabel-fim AS CHARACTER   NO-UNDO.

define buffer b-movto-estoq for movto-estoq.
define buffer b-natur-oper for natur-oper.
create tt-param.
assign
tt-param.c-item-ini     = ""
tt-param.c-item-fim     = "zzz"
tt-param.i-ge-codigo-ini     = 20
tt-param.i-ge-codigo-fim     = 21
tt-param.c-fm-codigo-ini     = ""
tt-param.c-fm-codigo-fim     = "zz"
tt-param.c-fm-cod-com-ini    = ""
tt-param.c-fm-cod-com-fim    = "zz"
tt-param.c-nro-docto-ini   = ""
tt-param.c-nro-docto-fim   = "zz"
tt-param.i-cod-emitente-ini = 0
tt-param.i-cod-emitente-fim = 9999999
tt-param.c-cod-estabel-ini  = ""
tt-param.c-cod-estabel-fim  = "zzz"
tt-param.c-nat-operacao-ini =  ""
tt-param.c-nat-operacao-fim =  "zzz"
tt-param.dt-emisdoc-ini = 02/01/2017
tt-param.dt-emisdoc-fim = 02/28/2017
tt-param.l-gera-duplicata = no
tt-param.l-medio = yes
tt-param.tip-natoper = 3
    tt-param.l-estabel             = yes
    tt-param.l-grupo               = yes
    tt-param.l-familia             = yes
    tt-param.l-familia-com         = yes
    tt-param.l-item                = yes
    tt-param.l-emitente            = no
    tt-param.l-representante       = no
    tt-param.l-estado              = no
    tt-param.l-cidade              = no
    tt-param.l-documento           = no
    tt-param.l-fat-dupl            = yes .
define temp-table tt-custo
    field it-codigo        like movto-estoq.it-codigo
    field cod-estabel      like movto-estoq.cod-estabel
    field quantidade       like movto-estoq.quantidade    
    field valor            as decimal label "Valor"          format "->>>,>>>,>>>,>>9.9999"    
    field medio            as decimal label "Medio Producao" format "->>>,>>>,>>>,>>9.9999"
    field medio-mat        as decimal label "Medio Mat"      format "->>>,>>>,>>>,>>9.9999"
    field valor-ovo        as decimal label "Valor ovo"      format "->>>,>>>,>>>,>>9.9999"
    field medio-ovo        as decimal label "Medio ovo"      format "->>>,>>>,>>>,>>9.9999"
    field valor-rac        as decimal label "Valor rac"      format "->>>,>>>,>>>,>>9.9999"
    field medio-rac        as decimal label "Medio rac"      format "->>>,>>>,>>>,>>9.9999"
    field valor-emb        as decimal label "Valor Emb"      format "->>>,>>>,>>>,>>9.9999"
    field medio-emb        as decimal label "Medio Emb"      format "->>>,>>>,>>>,>>9.9999"
    field valor-ggf        as decimal label "Valor ggf"      format "->>>,>>>,>>>,>>9.9999"
    field medio-ggf        as decimal label "Medio ggf"      format "->>>,>>>,>>>,>>9.9999"
    field valor-ggf-req    as decimal label "Valor ggf req"  format "->>>,>>>,>>>,>>9.9999"
    field medio-ggf-req    as decimal label "Medio ggf req"  format "->>>,>>>,>>>,>>9.9999"
    field valor-ggf-proc   as decimal label "Valor ggf proc" format "->>>,>>>,>>>,>>9.9999"
    field medio-ggf-proc   as decimal label "Medio ggf proc" format "->>>,>>>,>>>,>>9.9999"

    index chave is primary unique
    cod-estabel  
    it-codigo
     .



define temp-table tt-custo-medio
    field it-codigo     like movto-estoq.it-codigo
    field cod-estabel   like movto-estoq.cod-estabel  
    field medio         as decimal label "Medio Producao" format "->>>,>>>,>>>,>>9.9999"
    index chave is primary unique
    cod-estabel  
    it-codigo .

define temp-table tt-contas
    field tipo as integer
    field ct-inicio as char
    field ct-fim    as char
    index chave 
        tipo.
define temp-table tt-despesas
    field cod-estabel as char
    field tipo as integer
    field vl-debito  as dec   format "->>>,>>>,>>>,>>9.99"
    field vl-credito as dec   format "->>>,>>>,>>>,>>9.99" 
    field vl-unit    as dec   format "->>>,>>9.999999" 
    index chave 
        cod-estabel
        tipo.

define temp-table tt-vendas
    field cod-estabel as char  
    field vl-saidas         as dec   format "->>>,>>>,>>>,>>9.99" 
    field vl-saidas-frete   as dec   format "->>>,>>>,>>>,>>9.99" 
    index chave 
        cod-estabel.
       


    create tt-contas.
    assign
        tt-contas.tipo      = 1  /* despesas frete*/
        tt-contas.ct-inicio = "42030106"
        tt-contas.ct-fim    = "42030106".

    create tt-contas.
    assign
        tt-contas.tipo      = 2  /* despesas comerciais*/
        tt-contas.ct-inicio = "42030000"
        tt-contas.ct-fim    = "42030105".
    create tt-contas.
    assign
        tt-contas.tipo      = 2  /* despesas comerciais*/
        tt-contas.ct-inicio = "42030107"
        tt-contas.ct-fim    = "42039999".
    create tt-contas.
    assign
        tt-contas.tipo      = 3  /* despesas ADM*/
        tt-contas.ct-inicio = "42010000"
        tt-contas.ct-fim    = "42019999".
    create tt-contas.
    assign
        tt-contas.tipo      = 3  /* despesas ADM*/
        tt-contas.ct-inicio = "42020000"
        tt-contas.ct-fim    = "42029999".
    create tt-contas.
    assign
        tt-contas.tipo      = 4  /* despesas FIN*/
        tt-contas.ct-inicio = "43000000"
        tt-contas.ct-fim    = "43010106".
    create tt-contas.
    assign
        tt-contas.tipo      = 4  /* despesas FIN*/
        tt-contas.ct-inicio = "43010108"
        tt-contas.ct-fim    = "43999999".
def var dt-rel-ini                   like param-estoq.mensal-ate   NO-UNDO.
def var dt-rel-fim                   like param-estoq.mensal-ate   NO-UNDO.

def var dt-ref-ini                   like param-estoq.mensal-ate   NO-UNDO.
def var dt-ref-fim                   like param-estoq.mensal-ate   NO-UNDO.

DEFINE VARIABLE c-estab-ini AS CHARACTER initial "100"  NO-UNDO.
DEFINE VARIABLE c-estab-fim AS CHARACTER initial "199"  NO-UNDO.
DEFINE VARIABLE i-ge-codigo-ini AS INTEGER  initial 0   NO-UNDO.
DEFINE VARIABLE i-ge-codigo-fim AS INTEGER  initial 31   NO-UNDO.
DEFINE VARIABLE c-fm-codigo-ini AS char   initial ""   NO-UNDO.
DEFINE VARIABLE c-fm-codigo-fim AS char   initial "zzzz"   NO-UNDO.
DEFINE VARIABLE d-valor-frete AS DECIMAL  format "->>>,>>>,>>>,>>9.99"    NO-UNDO.
DEFINE VARIABLE d-valor-comerc AS DECIMAL  format "->>>,>>>,>>>,>>9.99"    NO-UNDO.
DEFINE VARIABLE d-valor-adm AS DECIMAL format "->>>,>>>,>>>,>>9.99"     NO-UNDO.
DEFINE VARIABLE d-valor-desc AS DECIMAL format "->>>,>>>,>>>,>>9.99"     NO-UNDO.
DEFINE VARIABLE d-valor-fin AS DECIMAL  format "->>>,>>>,>>>,>>9.99"    NO-UNDO.
DEFINE VARIABLE d-desc AS DECIMAL   format "->>>,>>>,>>>,>>9.99"   NO-UNDO.
DEFINE VARIABLE d-custo-prod AS DECIMAL format "->>>,>>>,>>>,>>9.99"     NO-UNDO.
DEFINE VARIABLE d-custo-medio AS DECIMAL  format "->>>,>>>,>>>,>>9.99"    NO-UNDO.
DEFINE VARIABLE d-custo-ovo   AS DECIMAL  format "->>>,>>>,>>>,>>9.99"    NO-UNDO.
DEFINE VARIABLE d-custo-emb   AS DECIMAL  format "->>>,>>>,>>>,>>9.99"    NO-UNDO.
DEFINE VARIABLE d-custo-ggf   AS DECIMAL  format "->>>,>>>,>>>,>>9.99"    NO-UNDO.
DEFINE VARIABLE l-devol AS LOGICAL     NO-UNDO.
DEFINE VARIABLE l-saida AS LOGICAL     NO-UNDO.

DEFINE VARIABLE ict AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-esp-atu AS INTEGER     NO-UNDO.

DEFINE VARIABLE i-sinal AS INTEGER     NO-UNDO.
 
find first param-estoq no-lock.
assign
    l-saida = (tt-param.tip-natoper = 2 or tt-param.tip-natoper  = 3)
    l-devol = (tt-param.tip-natoper = 1 or tt-param.tip-natoper  = 3).
assign 
    dt-rel-ini = tt-param.dt-emisdoc-ini
    dt-rel-fim = tt-param.dt-emisdoc-fim.
assign
    dt-ref-fim = param-estoq.mensal-ate
    dt-ref-ini = date(month(dt-ref-fim),1,year(dt-ref-fim)).

assign 
    c-estab-ini =  tt-param.c-cod-estabel-ini       
    c-estab-fim =  tt-param.c-cod-estabel-fim .

empty temp-table tt-despesas.
empty temp-table tt-vendas. 
empty temp-table tt-custo.

run pi-custo-prod.

for each nota-fiscal where 
    nota-fiscal.cod-estabel    >= c-estab-ini   and  
    nota-fiscal.cod-estabel    <= c-estab-fim   and     
    nota-fiscal.dt-cancela     = ?  and
    nota-fiscal.dt-emis-nota   >= dt-ref-ini   and 
    nota-fiscal.dt-emis-nota   <= dt-ref-fim  use-index nfftrm-20  no-lock ,
   each it-nota-fisc of nota-fiscal no-lock,
    first item where item.it-codigo = it-nota-fisc.it-codigo and
         item.ge-codigo >= 20 and 
         item.ge-codigo <= 21 no-lock.
    ict = ict + 1.
    if ict modulo 100 = 0 then do:
        run pi-acompanhar in h-acomp (input 
         nota-fiscal.cod-estabel + " " + string(item.ge-codigo) + " " + string( it-nota-fisc.dt-emis-nota) + " " + item.it-codigo).
    end.

    find first tt-vendas where
        tt-vendas.cod-estabel = nota-fiscal.cod-estabel no-error.
    if not avail tt-vendas  then do:
        create tt-vendas.
        assign
            tt-vendas.cod-estabel = nota-fiscal.cod-estabel.
    end.
    tt-vendas.vl-saidas = tt-vendas.vl-saidas + it-nota-fisc.vl-tot-item.
    if not(nota-fiscal.nome-transp = "DESTINATARIO" or nota-fiscal.nome-transp = "PADRAO 1") then do:    
        tt-vendas.vl-saidas-frete = tt-vendas.vl-saidas-frete + it-nota-fisc.vl-tot-item.
    end.
end.

     
for each tt-contas no-lock,
    each  sdo_ctbl  where
    sdo_ctbl.dat_sdo_ctbl = dt-ref-fim and
    sdo_ctbl.cod_cenar_ctbl = "fiscal" and
    sdo_ctbl.cod_finalid_econ = "corrente" and
    sdo_ctbl.cod_plano_cta_ctbl =  "padrao" and
    sdo_ctbl.cod_cta_ctbl >= tt-contas.ct-inicio and 
    sdo_ctbl.cod_cta_ctbl <= tt-contas.ct-fim and 
    sdo_ctbl.cod_ccusto = "" and
    sdo_ctbl.cod_estab >= c-estab-ini   and
    sdo_ctbl.cod_estab <= c-estab-fim 
    no-lock.

    ict = ict + 1.
    if ict modulo 100 = 0 then do:
        run pi-acompanhar in h-acomp (input sdo_ctbl.cod_estab + " " + sdo_ctbl.cod_cta_ctbl) .
    end.

    find first tt-despesas where
        tt-despesas.cod-estabel = sdo_ctbl.cod_estab and 
        tt-despesas.tipo        = tt-contas.tipo no-error.

    if not avail tt-despesas then do:
        create tt-despesas.
        assign tt-despesas.cod-estabel = sdo_ctbl.cod_estab
               tt-despesas.tipo        = tt-contas.tipo.
    end.
    assign
        tt-despesas.vl-debito  =  tt-despesas.vl-debito  + sdo_ctbl.val_sdo_ctbl_db
        tt-despesas.vl-credito =  tt-despesas.vl-credito + sdo_ctbl.val_sdo_ctbl_cr .

end.

for each tt-despesas.
    find first tt-vendas where
        tt-vendas.cod-estabel = tt-despesas.cod-estabel no-error.

    if tt-despesas.tipo = 1 then do:
        tt-despesas.vl-unit = (if not avail tt-vendas or  tt-vendas.vl-saidas-frete = 0 then 0 else (tt-despesas.vl-debito  - tt-despesas.vl-credito) /  tt-vendas.vl-saidas-frete).  
    end.
    else do:
        tt-despesas.vl-unit = (if not avail tt-vendas or tt-vendas.vl-saidas = 0 then 0 else (tt-despesas.vl-debito  - tt-despesas.vl-credito) /  tt-vendas.vl-saidas ).  
    end.

end. 

DO dt-atu = tt-param.dt-emisdoc-ini TO tt-param.dt-emisdoc-fim:
    FOR EACH estabelec WHERE 
        estabelec.cod-estabel >= tt-param.c-cod-estabel-ini and
        estabelec.cod-estabel <= tt-param.c-cod-estabel-fim 
        NO-LOCK.
        if l-saida then do:    

            FOR EACH nota-fiscal WHERE nota-fiscal.dt-emis-nota = dt-atu AND
                                   nota-fiscal.esp-docto = 22 and
                                   nota-fiscal.cod-estabel  = estabelec.cod-estabel and                                   
                                   nota-fiscal.nat-operacao >= tt-param.c-nat-operacao-ini and
                                   nota-fiscal.nat-operacao <= tt-param.c-nat-operacao-fim and
                                   nota-fiscal.cod-emitente >= tt-param.i-cod-emitente-ini AND   
                                   nota-fiscal.cod-emitente <= tt-param.i-cod-emitente-fim NO-LOCK USE-INDEX nfftrm-20.                    
            
                FIND FIRST tt-notas WHERE
                    tt-notas.cod-estabel = nota-fiscal.cod-estabel AND
                    tt-notas.serie       = nota-fiscal.serie       AND
                    tt-notas.nr-nota-fis = nota-fiscal.nr-nota-fis
                    NO-ERROR.
            
                IF NOT AVAIL tt-notas THEN DO:
                    CREATE tt-notas.
                    ASSIGN tt-notas.cod-estabel = nota-fiscal.cod-estabel
                           tt-notas.serie       = nota-fiscal.serie      
                           tt-notas.nr-nota-fis = nota-fiscal.nr-nota-fis.
                END.
            
                ASSIGN tt-notas.identific = 0.
            end.
        end.
        if l-devol then do:
        
            FOR EACH devol-cli NO-LOCK WHERE
                devol-cli.cod-emitente >= tt-param.i-cod-emitente-ini AND   
                devol-cli.cod-emitente <= tt-param.i-cod-emitente-fim AND
                devol-cli.nat-operacao >= tt-param.c-nat-operacao-ini and
                devol-cli.nat-operacao <= tt-param.c-nat-operacao-fim and
                devol-cli.cod-estabel  = estabelec.cod-estabel AND
                devol-cli.dt-devol     = dt-atu USE-INDEX ch-estabel.              
    
                find first motivo-devol  where
                          motivo-devol.cod-estabel  = devol-cli.cod-estabel   and           
                          motivo-devol.serie-docto  = devol-cli.serie-docto       and            
                          motivo-devol.nro-docto    = devol-cli.nro-docto  and            
                          motivo-devol.cod-emitente = devol-cli.cod-emitente     and
                          motivo-devol.nat-operacao = devol-cli.nat-operacao  no-lock no-error.
                find motivo-esp no-lock where motivo-esp.cod-motivo = motivo-devol.cod-motivo no-error.
                                         
                FIND FIRST nota-fiscal WHERE
                    nota-fiscal.esp-docto = 22 and
                    nota-fiscal.nat-operacao >= "5" and
                    nota-fiscal.cod-estabel = devol-cli.cod-estabel AND
                    nota-fiscal.serie       = devol-cli.serie       AND
                    nota-fiscal.nr-nota-fis = devol-cli.nr-nota-fis
                    NO-LOCK NO-ERROR.
            
                IF NOT AVAIL nota-fiscal THEN NEXT.
                
                FIND FIRST tt-notas WHERE
                    tt-notas.cod-estabel = nota-fiscal.cod-estabel AND
                    tt-notas.serie       = nota-fiscal.serie       AND
                    tt-notas.nr-nota-fis = nota-fiscal.nr-nota-fis
                    NO-ERROR.
            
                IF NOT AVAIL tt-notas THEN DO:
                    CREATE tt-notas.
                    ASSIGN tt-notas.cod-estabel = nota-fiscal.cod-estabel
                           tt-notas.serie       = nota-fiscal.serie      
                           tt-notas.nr-nota-fis = nota-fiscal.nr-nota-fis
                           tt-notas.identific = 9
                           tt-notas.motivo     = (if avail motivo-devol and avail motivo-esp then motivo-esp.descricao else "").
                END.
            
            END. 
        end.
    END.
END.

     

FOR EACH tt-notas
    WHERE tt-notas.nr-nota-fis  >= tt-param.c-nro-docto-ini 
        AND   tt-notas.nr-nota-fis  <= tt-param.c-nro-docto-fim NO-LOCK,
    EACH nota-fiscal 
    WHERE nota-fiscal.cod-estabel        = tt-notas.cod-estabel 
        AND   nota-fiscal.serie              = tt-notas.serie       
        AND   nota-fiscal.nr-nota-fis        = tt-notas.nr-nota-fis 
        AND   nota-fiscal.dt-cancela         = ? NO-LOCK,
    EACH it-nota-fisc OF nota-fiscal
    WHERE it-nota-fisc.nat-operacao >= tt-param.c-nat-operacao-ini 
        AND   it-nota-fisc.nat-operacao <= tt-param.c-nat-operacao-fim NO-LOCK,
    FIRST ITEM OF it-nota-fisc WHERE 
        item.it-codigo    >= tt-param.c-item-ini  AND 
        item.it-codigo    <= tt-param.c-item-fim  AND 
        item.ge-codigo    >= tt-param.i-ge-codigo-ini  AND 
        item.ge-codigo    <= tt-param.i-ge-codigo-fim  AND 
        item.fm-codigo    >= tt-param.c-fm-codigo-ini  AND 
        item.fm-codigo    <= tt-param.c-fm-codigo-fim  AND 
        item.fm-cod-com   >= tt-param.c-fm-cod-com-ini AND 
        item.fm-cod-com   <= tt-param.c-fm-cod-com-fim NO-LOCK,
    FIRST emitente   WHERE 
            emitente.cod-emitente  = nota-fiscal.cod-emitente ,
    FIRST familia where     familia.fm-codigo     = item.fm-codigo NO-LOCK,
    FIRST natur-oper WHERE  
          natur-oper.nat-operacao = it-nota-fisc.nat-operacao AND
          IF tt-param.l-gera-duplicata THEN natur-oper.emite-duplic ELSE true :
          
          find first estabelec where estabelec.cod-estabel = tt-notas.cod-estabel no-lock no-error.

           /*SE CONSIDERA DEVOLUCOES*/
        IF tt-notas.identific = 9 and l-devol THEN DO:

            /*VERIFICA SE TEM NOTAS DEVOLVIDAS*/
                        
            ASSIGN tt-aux-peso-liq-fat-dev    = 0
                   tt-aux-qt-faturada-dev     = 0
                   tt-aux-vl-tot-item-dev     = 0
                   tt-aux-vl-icms-it-dev      = 0
                   tt-aux-vl-ipi-it-dev       = 0
                   tt-v-cofins-dev            = 0
                   tt-v-pis-dev               = 0
                   tt-encargos-financ-dev     = 0
                   tt-valor-liquido-dev       = 0
                   tt-aux-vl-despes-it-dev    = 0.

            FOR EACH devol-cli 
                where devol-cli.it-codigo    = it-nota-fisc.it-codigo   
                AND   devol-cli.nr-nota-fis  = it-nota-fisc.nr-nota-fis 
                and   devol-cli.serie        = it-nota-fisc.serie       
                and   devol-cli.cod-estabel  = it-nota-fisc.cod-estabel 
                AND   devol-cli.nr-sequencia = it-nota-fisc.nr-seq-fat  
                AND   devol-cli.cod-emitente = nota-fiscal.cod-emitente NO-LOCK .
            
                IF  devol-cli.dt-devol   >= tt-param.dt-emisdoc-ini and devol-cli.dt-devol    <= tt-param.dt-emisdoc-fim THEN DO:
            
                    ASSIGN aux-qt-faturada-dev  = -1 * (devol-cli.qt-devolvida)
                           aux-vl-tot-item-dev  = -1 * (devol-cli.vl-devol).            
            
                     IF  (nota-fiscal.ind-tip-nota = 3) THEN
                        ASSIGN aux-qt-faturada-dev  = 0
                               aux-peso-liq-fat-dev = 0.
                                   
                    IF  it-nota-fisc.cd-trib-ipi <> 2 AND it-nota-fisc.cd-trib-ipi <> 3 THEN
                        ASSIGN aux-vl-ipi-it-dev = -1 * (it-nota-fisc.vl-ipi-it  * devol-cli.vl-devol / it-nota-fisc.vl-tot-item).
                    ELSE
                        ASSIGN aux-vl-ipi-it-dev = 0.
            
                    IF  it-nota-fisc.cd-trib-icm <> 2 AND it-nota-fisc.cd-trib-icm <> 3 THEN
                        ASSIGN aux-vl-icms-it-dev = -1 * (it-nota-fisc.vl-icms-it * devol-cli.vl-devol / it-nota-fisc.vl-tot-item).
                    ELSE
                      ASSIGN aux-vl-icms-it-dev = 0.
            
                    if  devol-cli.vl-devol = it-nota-fisc.vl-tot-item then
                        assign aux-vl-merc-liq-dev = - it-nota-fisc.vl-merc-liq.
                    else
                        ASSIGN aux-vl-merc-liq-dev  = -1 * (it-nota-fisc.vl-merc-liq * devol-cli.vl-devol / it-nota-fisc.vl-tot-item).
            
                    IF  it-nota-fisc.dt-emis-nota < 11/01/02 THEN
                        assign v-aliq-pis = dec(SUBSTRING(natur-oper.char-1, 77, 4))
                               v-aliq-cof = dec(SUBSTRING(natur-oper.char-1, 82, 4)).
                    ELSE
                        ASSIGN v-aliq-pis = DEC(SUBSTRING(it-nota-fisc.char-2, 76, 5))
                               v-aliq-cof = DEC(SUBSTRING(it-nota-fisc.char-2, 81, 5)).
            
                    assign v-pis-dev    = ((aux-vl-tot-item-dev - aux-vl-ipi-it-dev) * v-aliq-pis) / 100
                           v-cofins-dev = ((aux-vl-tot-item-dev - aux-vl-ipi-it-dev) * v-aliq-cof) / 100.
                                                                    
                    ASSIGN aux-qt-dev = aux-qt-faturada-dev  * -1.

                    FIND FIRST movto-estoq WHERE 
                             movto-estoq.serie-docto =  it-nota-fisc.serie AND
                             movto-estoq.sequen-nf = it-nota-fisc.nr-seq-fat  AND
                             movto-estoq.nro-docto = it-nota-fisc.nr-nota-fis  AND
                             movto-estoq.nat-operacao = it-nota-fisc.nat-operacao AND
                             movto-estoq.it-codigo = it-nota-fisc.it-codigo  AND
                             movto-estoq.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.

                    if avail movto-estoq and (movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-MOB-m[1]) > 0   then do:
                       assign v-custo-total =  -1 * aux-qt-dev * (movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-MOB-m[1]) / movto-estoq.quantidade
                              v-custo-medio = v-custo-total / aux-qt-dev.
                    end.
                    else do:
                        for first  item-estab
                                        where item-estab.it-codigo   = it-nota-fisc.it-codigo
                                        and   item-estab.cod-estabel = nota-fiscal.cod-estabel 
                                        no-lock  .
                            assign v-custo-medio  = -1 * (   item-estab.val-unit-mat-m[1]
                                                     + item-estab.val-unit-mob-m[1]
                                                     + item-estab.val-unit-ggf-m[1])
                                   v-custo-total = v-custo-medio *  aux-qt-dev.
                       end.
                    end.
                                                                            
                    ASSIGN 
                        v-impostos-dev    = aux-vl-icms-it-dev + aux-vl-ipi-it-dev + v-pis-dev + v-cofins-dev
                        valor-liquido-dev = aux-vl-tot-item-dev - v-impostos
                        v-preco-unit = aux-vl-tot-item-dev / aux-qt-dev .

                    find first b-natur-oper where b-natur-oper.nat-operacao = devol-cli.nat-operacao no-lock no-error.

                    c-cod-oper = "NFD".

                    assign 
                        d-valor-frete  = 0
                        d-valor-comerc = 0
                        d-valor-adm    = 0
                        d-valor-desc   = 0
                        d-valor-fin    = 0
                        d-desc         = 0
                        d-custo-prod   = 0
                        d-custo-ovo    = 0
                        d-custo-emb    = 0
                        d-custo-ggf    = 0
                        .            

                                
                    if it-nota-fisc.emite-duplic then do:
                            for first clien_financ where 
                                    clien_financ.cdn_cliente = nota-fiscal.cod-emitente and
                                    clien_financ.cod_empresa = estabelec.ep-codigo no-lock.
                                    d-desc = dec(clien_financ.cod_classif_msg_cobr) no-error.                
                                    d-valor-desc = -1 * it-nota-fisc.vl-tot-item * d-desc / 100 * (devol-cli.vl-devol / it-nota-fisc.vl-tot-item).
                            end.           
                    end.                     
                
                    for first tt-custo where
                                    tt-custo.it-codigo   =  it-nota-fisc.it-codigo   and
                                    tt-custo.cod-estabel =  it-nota-fisc.cod-estabel no-lock.
                         assign d-custo-prod = -1 * tt-custo.medio * aux-qt-dev .
                         assign d-custo-prod = -1 * tt-custo.medio * aux-qt-dev 
                                d-custo-ovo  = tt-custo.medio-ovo 
                                d-custo-emb  = (tt-custo.medio-rac + tt-custo.medio-emb) 
                                d-custo-ggf  = tt-custo.medio-ggf-proc .
                    end.

                    assign 
                        d-valor-frete   =  0
                        d-valor-comerc  =  0
                        d-valor-adm     =  0
                        d-valor-fin     =  0.        

                  
                       
                       assign v-receita-bruta       = if not it-nota-fisc.emite-duplic then 0 else aux-vl-tot-item-dev
                              v-receita-liquida     =  v-receita-bruta - d-valor-desc - v-impostos-dev
                              v-margem-bruta        =  v-receita-liquida - d-valor-frete - (if l-medio then v-custo-total else d-custo-prod)
                              v-margem-liquida      =  v-margem-bruta  - (d-valor-comerc + d-valor-adm  + d-valor-fin   )
                              v-perc-margem-bruta   = (if v-receita-liquida <> 0 then v-margem-bruta / v-receita-liquida else 0) * 100
                              v-perc-margem-liquida = (if v-receita-liquida <> 0 then v-margem-liquida / v-receita-liquida else 0) * 100.


                    assign valor-margem = valor-liquido-dev - (d-valor-frete + d-valor-comerc + d-valor-adm + d-valor-fin).
                      
                     if l-documento then do:              
                         create tt-dados.                 
                         assign                                
                           tt-dados.cod-estabel          =        nota-fiscal.cod-estabel
                           tt-dados.cod-emitente         =        nota-fiscal.cod-emitente
                           tt-dados.nome-ab-cli          =        nota-fiscal.nome-ab-cli                         
                           tt-dados.cgc                  =        nota-fiscal.cgc
                           tt-dados.cidade               =        nota-fiscal.cidade
                           tt-dados.estado               =        nota-fiscal.estado                         
                           tt-dados.no-ab-reppri         =        nota-fiscal.no-ab-reppri  
                           tt-dados.nome-transp          =        nota-fiscal.nome-transp                    
                           tt-dados.cod-oper             =        c-cod-oper                 
                           tt-dados.nat-operacao         =        devol-cli.nat-operacao  
                           tt-dados.cod-cfop             =        b-natur-oper.cod-cfop 
                           tt-dados.fat-dup              =        (if it-nota-fisc.emite-duplic then "Sim" else "NÆo")
                           tt-dados.un                   =        it-nota-fisc.un-fatur[1]
                           tt-dados.serie-docto          =        devol-cli.serie-docto
                           tt-dados.nro-docto            =        devol-cli.nro-docto  .
                                 assign 
                           tt-dados.dt-docto             =        devol-cli.dt-devol  
                           tt-dados.it-codigo            =        it-nota-fisc.it-codigo
                           tt-dados.desc-item            =        item.desc-item    .
                                 assign
                           tt-dados.ge-codigo            =        item.ge-codigo
                           tt-dados.fm-codigo            =        item.fm-codigo                  
                           tt-dados.fm-cod-com           =        item.fm-cod-com              .
                           assign tt-dados.qt-faturada          =        aux-qt-faturada-dev          
                           tt-dados.preco                =        v-preco-unit                           
                           tt-dados.tot-item             =        aux-vl-tot-item-dev          
                           tt-dados.receita-bruta        =        v-receita-bruta              
                           tt-dados.impostos             =        v-impostos                   
                           tt-dados.valor-desc           =        d-valor-desc                 
                           tt-dados.receita-liquida      =        v-receita-liquida            
                           tt-dados.custo-prod           =        d-custo-prod                 
                           tt-dados.custo-total          =        v-custo-total                
                           tt-dados.custo-ovo            =        d-custo-ovo  / 100           
                           tt-dados.custo-emb            =        d-custo-emb  / 100                
                           tt-dados.custo-ggf            =        d-custo-ggf  / 100           
                           tt-dados.valor-frete          =        d-valor-frete                
                           tt-dados.margem-bruta         =        v-margem-bruta               
                           tt-dados.perc-margem-bruta    =        v-perc-margem-bruta / 100    
                           tt-dados.valor-comerc         =        d-valor-comerc               
                           tt-dados.valor-adm            =        d-valor-adm                                          
                           tt-dados.valor-fin            =        d-valor-fin                  
                           tt-dados.margem-liquida       =        v-margem-liquida             
                           tt-dados.perc-margem-liquida  =        v-perc-margem-liquida / 100  
                           tt-dados.motivo               =        tt-notas.motivo .             
                     end.
                     else do:
                         find first tt-dados where
                               tt-dados.cod-estabel    = (if not l-estabel       then "" else nota-fiscal.cod-estabel )  and
                               tt-dados.ge-codigo      = (if not l-grupo         then 0  else item.ge-codigo          )  and
                               tt-dados.fm-codigo      = (if not l-familia       then "" else item.fm-codigo          )  and
                               tt-dados.fm-cod-com     = (if not l-familia-com   then "" else item.fm-cod-com         )  and
                               tt-dados.it-codigo      = (if not l-item          then "" else it-nota-fisc.it-codigo  )  and
                               tt-dados.cod-emitente   = (if not l-emitente      then 0  else nota-fiscal.cod-emitente)  and
                               tt-dados.no-ab-reppri   = (if not l-representante then "" else nota-fiscal.no-ab-reppri)  and
                               tt-dados.estado         = (if not l-estado        then "" else nota-fiscal.estado      )  and
                               tt-dados.cidade         = (if not l-cidade        then "" else nota-fiscal.cidade)        and
                               tt-dados.fat-dup        = (if not tt-param.l-fat-dupl then "" else (if it-nota-fisc.emite-duplic then "Sim" else "NÆo") no-error.
                           

                         if not avail tt-dados then do:                 
                            create tt-dados.   
                            assign 
                                 tt-dados.cod-estabel    = if not l-estabel       then "" else nota-fiscal.cod-estabel  
                                 tt-dados.ge-codigo      = if not l-grupo         then 0  else item.ge-codigo           
                                 tt-dados.fm-codigo      = if not l-familia       then "" else item.fm-codigo           
                                 tt-dados.fm-cod-com     = if not l-familia-com   then "" else item.fm-cod-com          
                                 tt-dados.it-codigo      = if not l-item          then "" else it-nota-fisc.it-codigo   
                                 tt-dados.cod-emitente   = if not l-emitente      then 0  else nota-fiscal.cod-emitente 
                                 tt-dados.no-ab-reppri   = if not l-representante then "" else nota-fiscal.no-ab-reppri 
                                 tt-dados.estado         = if not l-estado        then "" else nota-fiscal.estado       
                                 tt-dados.cidade         = if not l-cidade        then "" else nota-fiscal.cidade 
                                 tt-dados.fat-dup        = (if not tt-param.l-fat-dupl then "" else (if it-nota-fisc.emite-duplic then "Sim" else "NÆo").
                         end.

                         assign
                              tt-dados.nome-ab-cli          =   if not l-emitente then "" else nota-fiscal.nome-ab-cli 
                              tt-dados.cgc                  =   if not l-emitente then "" else nota-fiscal.cgc                        
                              tt-dados.nome-transp          =   if not l-emitente then "" else nota-fiscal.nome-transp        
                              tt-dados.cod-oper             =   ""
                              tt-dados.nat-operacao         =   "" 
                              tt-dados.cod-cfop             =   ""                             
                              tt-dados.un                   =   if not l-item     then "" else it-nota-fisc.un-fatur[1]
                              tt-dados.serie-docto          =   ""
                              tt-dados.nro-docto            =   ""
                              tt-dados.dt-docto             =   ?                       
                              tt-dados.desc-item            =   if not l-item          then "" else item.desc-item                         
                              tt-dados.qt-faturada          =   tt-dados.qt-faturada + aux-qt-faturada-dev                                                   
                              tt-dados.tot-item             =   tt-dados.tot-item + aux-vl-tot-item-dev  
                              tt-dados.preco                =   tt-dados.tot-item / aux-qt-faturada-dev
                              tt-dados.receita-bruta        =   tt-dados.receita-bruta + v-receita-bruta              
                              tt-dados.impostos             =   tt-dados.impostos + v-impostos                   
                              tt-dados.valor-desc           =   tt-dados.valor-desc + d-valor-desc                 
                              tt-dados.receita-liquida      =   tt-dados.receita-liquida + v-receita-liquida            
                              tt-dados.custo-prod           =   tt-dados.custo-prod + d-custo-prod                 
                              tt-dados.custo-total          =   tt-dados.custo-total + v-custo-total                
                              tt-dados.custo-ovo            =   tt-dados.custo-total * d-custo-ovo  / 100           
                              tt-dados.custo-emb            =   tt-dados.custo-total * d-custo-emb  / 100                
                              tt-dados.custo-ggf            =   tt-dados.custo-total * d-custo-ggf  / 100           
                              tt-dados.valor-frete          =   tt-dados.valor-frete + d-valor-frete                
                              tt-dados.margem-bruta         =   tt-dados.margem-bruta + v-margem-bruta               
                              tt-dados.perc-margem-bruta    =   (if tt-dados.receita-liquida <> 0 then tt-dados.margem-bruta / tt-dados.receita-liquida else 0)  
                              tt-dados.valor-comerc         =   tt-dados.valor-comerc + d-valor-comerc               
                              tt-dados.valor-adm            =   tt-dados.valor-adm + d-valor-adm                                          
                              tt-dados.valor-fin            =   tt-dados.valor-fin + d-valor-fin                  
                              tt-dados.margem-liquida       =   tt-dados.margem-liquida + v-margem-liquida             
                              tt-dados.perc-margem-liquida  =   (if tt-dados.receita-liquida <> 0 then tt-dados.margem-liquida / tt-dados.receita-liquida else 0)    
                              tt-dados.motivo               =   "".        

                     end.
                end.
            end.
        end.
      if  tt-notas.identific <> 9 then do:
      
          ASSIGN aux-qt-faturada   = it-nota-fisc.qt-faturada[1] 
                   aux-vl-tot-item   = it-nota-fisc.vl-tot-item
                   aux-vl-ipi-it     = (IF it-nota-fisc.cd-trib-ipi <> 2 AND 
                                         it-nota-fisc.cd-trib-ipi <> 3 THEN it-nota-fisc.vl-ipi-it  ELSE 0)                                                                     
                   aux-vl-icms-it    = (IF it-nota-fisc.cd-trib-icm <> 2 AND 
                                         it-nota-fisc.cd-trib-icm <> 3 THEN it-nota-fisc.vl-icms-it ELSE 0)                                                                                                  
                   aux-vl-despes-it  = it-nota-fisc.vl-despes-it
                   aux-vl-merc-liq   = it-nota-fisc.vl-merc-liq.
    
            IF  (nota-fiscal.ind-tip-nota = 3) THEN
                ASSIGN aux-qt-faturada  = 0
                       aux-peso-liq-fat = 0.
    
            IF  (it-nota-fisc.cd-trib-ipi = 3) THEN
                ASSIGN aux-vl-ipi-it = 0.
    
            IF  it-nota-fisc.dt-emis-nota < 11/01/02 THEN
                assign v-aliq-pis = dec(SUBSTRING(natur-oper.char-1, 77, 4))
                       v-aliq-cof = dec(SUBSTRING(natur-oper.char-1, 82, 4)).
            ELSE
               ASSIGN v-aliq-pis = DEC(SUBSTRING(it-nota-fisc.char-2, 76, 5))
                      v-aliq-cof = DEC(SUBSTRING(it-nota-fisc.char-2, 81, 5)).
    
            ASSIGN v-pis    = ((aux-vl-tot-item - (aux-vl-ipi-it)) * v-aliq-pis) / 100
                   v-cofins = ((aux-vl-tot-item - (aux-vl-ipi-it)) * v-aliq-cof) / 100.
            FIND FIRST movto-estoq WHERE 
                             movto-estoq.serie-docto =  it-nota-fisc.serie AND
                             movto-estoq.sequen-nf = it-nota-fisc.nr-seq-fat  AND
                             movto-estoq.nro-docto = it-nota-fisc.nr-nota-fis  AND
                             movto-estoq.nat-operacao = it-nota-fisc.nat-operacao AND
                             movto-estoq.it-codigo = it-nota-fisc.it-codigo  AND
                             movto-estoq.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.

            if avail movto-estoq and (movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-MOB-m[1]) > 0   then do:
               assign v-custo-total = it-nota-fisc.qt-faturada[1] * (movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-MOB-m[1]) / movto-estoq.quantidade
                      v-custo-medio = v-custo-total / it-nota-fisc.qt-faturada[1].
            end.
            else do:
                for first  item-estab
                                where item-estab.it-codigo   = it-nota-fisc.it-codigo
                                and   item-estab.cod-estabel = nota-fiscal.cod-estabel 
                                no-lock  .
                    assign v-custo-medio  =    item-estab.val-unit-mat-m[1]
                                             + item-estab.val-unit-mob-m[1]
                                             + item-estab.val-unit-ggf-m[1]
                           v-custo-total = v-custo-medio *  it-nota-fisc.qt-faturada[1].
               end.
            end.                        
            
            ASSIGN 
                v-preco-unit  = it-nota-fisc.vl-tot-item / it-nota-fisc.qt-faturada[1] 
                v-impostos    = aux-vl-icms-it + aux-vl-ipi-it + v-pis + v-cofins 
                valor-liquido = aux-vl-tot-item - v-impostos
                c-cod-oper    = entry(nota-fiscal.esp-docto, {ininc/i03in218.i 03}).

            assign 
                d-valor-frete  = 0
                d-valor-comerc = 0
                d-valor-adm    = 0
                d-valor-desc   = 0
                d-valor-fin    = 0
                d-desc         = 0
                d-custo-prod   = 0                
                d-custo-ovo    = 0
                d-custo-emb    = 0
                d-custo-ggf    = 0
                .            
           
            if it-nota-fisc.emite-duplic then do:
                    for first clien_financ where 
                            clien_financ.cdn_cliente = nota-fiscal.cod-emitente and
                            clien_financ.cod_empresa = estabelec.ep-codigo no-lock.
                        assign
                            d-desc       = dec(clien_financ.cod_classif_msg_cobr) no-error.                
                        assign
                            d-valor-desc = it-nota-fisc.vl-tot-item * d-desc / 100.
                    end.           
            end. 

            if item.ge-codigo >= 20 and 
               item.ge-codigo <= 21   then do:
                                       
                for each tt-despesas where 
                    tt-despesas.cod-estabel = nota-fiscal.cod-estabel no-lock.
                    if tt-despesas.tipo = 1  then do:
                        d-valor-frete = tt-despesas.vl-unit .
                    end.
                    if tt-despesas.tipo = 2  then do:
                        d-valor-comerc = tt-despesas.vl-unit .
                    end.
                    if tt-despesas.tipo = 3  then do:
                        d-valor-adm = tt-despesas.vl-unit .
                    end.
                    if tt-despesas.tipo = 4  then do:
                        d-valor-fin = tt-despesas.vl-unit .
                    end.
                end.            
            end.
            
            if (nota-fiscal.nome-transp = "DESTINATARIO" or nota-fiscal.nome-transp = "PADRAO 1") then do:  
                d-valor-frete = 0.
            end.
            for first tt-custo where
                            tt-custo.it-codigo   =  it-nota-fisc.it-codigo   and
                            tt-custo.cod-estabel =  it-nota-fisc.cod-estabel no-lock.
                assign d-custo-prod = tt-custo.medio  * it-nota-fisc.qt-faturada[1] 
                       d-custo-ovo  = tt-custo.medio-ovo   
                       d-custo-emb  = (tt-custo.medio-rac + tt-custo.medio-emb)  
                       d-custo-ggf  = tt-custo.medio-ggf-proc .
            end.

            assign 
                d-valor-frete   =  d-valor-frete  * it-nota-fisc.vl-tot-item 
                d-valor-comerc  =  d-valor-comerc * it-nota-fisc.vl-tot-item 
                d-valor-adm     =  d-valor-adm    * it-nota-fisc.vl-tot-item 
                d-valor-fin     =  d-valor-fin    * it-nota-fisc.vl-tot-item .
            
             assign valor-margem = valor-liquido - (d-valor-frete + d-valor-comerc + d-valor-adm + d-valor-fin).
             assign v-receita-bruta       = if not it-nota-fisc.emite-duplic then 0 else it-nota-fisc.vl-tot-item
                    v-receita-liquida     =  v-receita-bruta - d-valor-desc - v-impostos
                    v-margem-bruta        =  v-receita-liquida - d-valor-frete - (if l-medio then v-custo-total else d-custo-prod)
                    v-margem-liquida      =  v-margem-bruta  - (d-valor-comerc + d-valor-adm  + d-valor-fin   )
                    v-perc-margem-bruta   = (if v-receita-liquida <> 0 then v-margem-bruta / v-receita-liquida else 0) * 100
                    v-perc-margem-liquida = (if v-receita-liquida <> 0 then v-margem-liquida / v-receita-liquida else 0) * 100.

              
             if l-documento then do:
                 create tt-dados.      
                 assign
                   tt-dados.cod-estabel          =   nota-fiscal.cod-estabel
                   tt-dados.cod-emitente         =   nota-fiscal.cod-emitente        
                   tt-dados.nome-ab-cli          =   nota-fiscal.nome-ab-cli 
                   tt-dados.cgc                  =   nota-fiscal.cgc
                   tt-dados.cidade               =   nota-fiscal.cidade
                   tt-dados.estado               =   nota-fiscal.estado
                   tt-dados.no-ab-reppri         =   nota-fiscal.no-ab-reppri  
                   tt-dados.nome-transp          =   nota-fiscal.nome-transp        
                   tt-dados.cod-oper             =   c-cod-oper
                   tt-dados.nat-operacao         =   it-nota-fisc.nat-operacao
                   tt-dados.cod-cfop             =   natur-oper.cod-cfop
                   tt-dados.fat-dup              =   if it-nota-fisc.emite-duplic then "Sim" else "NÆo"
                   tt-dados.un                   =   it-nota-fisc.un-fatur[1]
                   tt-dados.serie-docto          =   nota-fiscal.serie
                   tt-dados.nro-docto            =   nota-fiscal.nr-nota-fis 
                   tt-dados.dt-docto             =   nota-fiscal.dt-emis-nota
                   tt-dados.it-codigo            =   it-nota-fisc.it-codigo
                   tt-dados.desc-item            =   item.desc-item  
                   tt-dados.ge-codigo            =   item.ge-codigo
                   tt-dados.fm-codigo            =   item.fm-codigo               
                   tt-dados.fm-cod-com           =   item.fm-cod-com              
                   tt-dados.qt-faturada          =   aux-qt-faturada              
                   tt-dados.preco                =   v-preco-unit                 
                   tt-dados.tot-item             =   aux-vl-tot-item              
                   tt-dados.receita-bruta        =   v-receita-bruta              
                   tt-dados.impostos             =   v-impostos                   
                   tt-dados.valor-desc           =   d-valor-desc                 
                   tt-dados.receita-liquida      =   v-receita-liquida            
                   tt-dados.custo-prod           =   d-custo-prod                 
                   tt-dados.custo-total          =   v-custo-total                
                   tt-dados.custo-ovo            =   d-custo-ovo  / 100           
                   tt-dados.custo-emb            =   d-custo-emb  / 100                
                   tt-dados.custo-ggf            =   d-custo-ggf  / 100           
                   tt-dados.valor-frete          =   d-valor-frete                
                   tt-dados.margem-bruta         =   v-margem-bruta               
                   tt-dados.perc-margem-bruta    =   v-perc-margem-bruta / 100    
                   tt-dados.valor-comerc         =   d-valor-comerc               
                   tt-dados.valor-adm            =   d-valor-adm                                          
                   tt-dados.valor-fin            =   d-valor-fin                  
                   tt-dados.margem-liquida       =   v-margem-liquida             
                   tt-dados.perc-margem-liquida  =   v-perc-margem-liquida / 100  
                   tt-dados.motivo               =   tt-notas.motivo.        
             end.
             else do:

                  find first tt-dados where
                        tt-dados.cod-estabel    = (if not l-estabel       then "" else nota-fiscal.cod-estabel) and
                        tt-dados.ge-codigo      = (if not l-grupo         then 0  else item.ge-codigo  )         and
                        tt-dados.fm-codigo      = (if not l-familia       then "" else item.fm-codigo          ) and
                        tt-dados.fm-cod-com     = (if not l-familia-com   then "" else item.fm-cod-com         ) and
                        tt-dados.it-codigo      = (if not l-item          then "" else it-nota-fisc.it-codigo  ) and
                        tt-dados.cod-emitente   = (if not l-emitente      then 0  else nota-fiscal.cod-emitente) and
                        tt-dados.no-ab-reppri   = (if not l-representante then "" else nota-fiscal.no-ab-reppri) and
                        tt-dados.estado         = (if not l-estado        then "" else nota-fiscal.estado      )  and
                        tt-dados.cidade         = (if not l-cidade        then "" else nota-fiscal.cidade)       and
                        tt-dados.fat-dup        = (if not tt-param.l-fat-dupl then "" else (if it-nota-fisc.emite-duplic then "Sim" else "NÆo")
                        no-error.
                   

                  if not avail tt-dados then do:                 
                     create tt-dados.   
                     assign 
                          tt-dados.cod-estabel    = if not l-estabel       then "" else nota-fiscal.cod-estabel  
                          tt-dados.ge-codigo      = if not l-grupo         then 0  else item.ge-codigo           
                          tt-dados.fm-codigo      = if not l-familia       then "" else item.fm-codigo           
                          tt-dados.fm-cod-com     = if not l-familia-com   then "" else item.fm-cod-com          
                          tt-dados.it-codigo      = if not l-item          then "" else it-nota-fisc.it-codigo   
                          tt-dados.cod-emitente   = if not l-emitente      then 0  else nota-fiscal.cod-emitente 
                          tt-dados.no-ab-reppri   = if not l-representante then "" else nota-fiscal.no-ab-reppri 
                          tt-dados.estado         = if not l-estado        then "" else nota-fiscal.estado       
                          tt-dados.cidade         = if not l-cidade        then "" else nota-fiscal.cidade 
                          tt-dados.fat-dup        = (if not tt-param.l-fat-dupl then "" else (if it-nota-fisc.emite-duplic then "Sim" else "NÆo").
                  end.
      
                  assign
                       tt-dados.nome-ab-cli          =   if not l-emitente then "" else nota-fiscal.nome-ab-cli 
                       tt-dados.cgc                  =   if not l-emitente then "" else nota-fiscal.cgc                        
                       tt-dados.nome-transp          =   if not l-emitente then "" else nota-fiscal.nome-transp        
                       tt-dados.cod-oper             =   ""
                       tt-dados.nat-operacao         =   "" 
                       tt-dados.cod-cfop             =   ""                        
                       tt-dados.un                   =   if not l-item     then "" else it-nota-fisc.un-fatur[1]
                       tt-dados.serie-docto          =   ""
                       tt-dados.nro-docto            =   ""
                       tt-dados.dt-docto             =   ?                       
                       tt-dados.desc-item            =   if not l-item          then "" else item.desc-item                         
                       tt-dados.qt-faturada          =   tt-dados.qt-faturada + aux-qt-faturada                                                     
                       tt-dados.tot-item             =   tt-dados.tot-item + aux-vl-tot-item  
                       tt-dados.preco                =   tt-dados.tot-item / tt-dados.qt-faturada
                       tt-dados.receita-bruta        =   tt-dados.receita-bruta + v-receita-bruta              
                       tt-dados.impostos             =   tt-dados.impostos + v-impostos                   
                       tt-dados.valor-desc           =   tt-dados.valor-desc + d-valor-desc                 
                       tt-dados.receita-liquida      =   tt-dados.receita-liquida + v-receita-liquida            
                       tt-dados.custo-prod           =   tt-dados.custo-prod + d-custo-prod                 
                       tt-dados.custo-total          =   tt-dados.custo-total + v-custo-total                
                       tt-dados.custo-ovo            =   tt-dados.custo-total * d-custo-ovo  / 100           
                       tt-dados.custo-emb            =   tt-dados.custo-total * d-custo-emb  / 100                
                       tt-dados.custo-ggf            =   tt-dados.custo-total * d-custo-ggf  / 100           
                       tt-dados.valor-frete          =   tt-dados.valor-frete + d-valor-frete                
                       tt-dados.margem-bruta         =   tt-dados.margem-bruta + v-margem-bruta               
                       tt-dados.perc-margem-bruta    =   (if tt-dados.receita-liquida <> 0 then tt-dados.margem-bruta / tt-dados.receita-liquida else 0)  
                       tt-dados.valor-comerc         =   tt-dados.valor-comerc + d-valor-comerc               
                       tt-dados.valor-adm            =   tt-dados.valor-adm + d-valor-adm                                          
                       tt-dados.valor-fin            =   tt-dados.valor-fin + d-valor-fin                  
                       tt-dados.margem-liquida       =   tt-dados.margem-liquida + v-margem-liquida             
                       tt-dados.perc-margem-liquida  =   (if tt-dados.receita-liquida <> 0 then tt-dados.margem-liquida / tt-dados.receita-liquida else 0)    
                       tt-dados.motivo               =   "".        
             end.
      end.
end.

output to c:\temp\analise-margem-fev-2017.txt no-convert.

 put unformatted       
     "Estab"   ";"
 "Cod.Emitente"   ";"
 "Nome"   ";"
 "CNPJ/CPF"   ";"
 "Cidade"   ";"
 "UF"   ";"
 "Representante"   ";"
 "Transportadora"   ";"
 "Espec"   ";"
 "Nat.Oper"   ";"    
 "CFOP"   ";"
 "Fat.Dpl" ";"
 "UN"   ";"
 "S‚rie"   ";"
 "Nr.Docto"   ";"
 "Dt.EmissÆo"   ";"
 "Item"   ";"
 "Descri‡Æo do Item"   ";"
 "Fam¡lia"   ";"
 "Fam.Comercial"   ";"
 "Quantidade"   ";"
 "Pre‡o"   ";"
 "Total do Item"   ";"
 "Receita Bruta"   ";"
 "Impostos"   ";"
 "Desconto Fin."   ";"
 "Receita Liquida"   ";"
 "Custo Produ‡Æo"   ";"
 "Custo M‚dio"   ";"
 "Ovo In Natura"   ";"
 "Embalagem"   ";"
 "GGF"   ";"
 "Despesa Frete"   ";"
 "Margem Bruta"   ";"
 "% Margem Bruta"   ";"
 "Despesa COM"   ";"
 "Despesa ADM"   ";"
 "Despesa FIN"   ";"
 "Margem L¡quida"   ";"
 "% Margem L¡quida"   ";"
 "Motivo"   skip.

for each tt-dados.
    put unformatted
        tt-dados.cod-estabel         ";"
        tt-dados.cod-emitente        ";"
        tt-dados.nome-ab-cli         ";"
        tt-dados.cgc                 ";"
        tt-dados.cidade              ";"
        tt-dados.estado              ";"
        tt-dados.no-ab-reppri        ";"
        tt-dados.nome-transp         ";"
        tt-dados.cod-oper            ";"
        tt-dados.nat-operacao        ";"
        tt-dados.cod-cfop            ";"
        tt-dados.fat-dup             ";"
        tt-dados.un                  ";"
        tt-dados.serie-docto         ";"
        tt-dados.nro-docto           ";"
        if tt-dados.dt-docto = ? then "" else string(tt-dados.dt-docto,"99/99/9999")           ";"
        tt-dados.it-codigo           ";"
        tt-dados.desc-item           ";"
        tt-dados.ge-codigo           ";"
        tt-dados.fm-codigo           ";"
        tt-dados.fm-cod-com          ";"
        tt-dados.qt-faturada         ";"
        tt-dados.preco               ";"
        tt-dados.tot-item            ";"
        tt-dados.receita-bruta       ";"
        tt-dados.impostos            ";"
        tt-dados.valor-desc          ";"
        tt-dados.receita-liquida     ";"
        tt-dados.custo-prod          ";"
        tt-dados.custo-total         ";"
        if l-documento then tt-dados.custo-ovo   else tt-dados.custo-ovo  /  tt-dados.custo-total       ";"
        if l-documento then tt-dados.custo-emb   else tt-dados.custo-emb  /  tt-dados.custo-total       ";"
        if l-documento then tt-dados.custo-ggf   else tt-dados.custo-ggf  /  tt-dados.custo-total       ";"        
        tt-dados.valor-frete         ";"
        tt-dados.margem-bruta        ";"
        tt-dados.perc-margem-bruta   ";"
        tt-dados.valor-comerc        ";"
        tt-dados.valor-adm           ";"
        tt-dados.valor-fin           ";"
        tt-dados.margem-liquida      ";"
        tt-dados.perc-margem-liquida ";"
        tt-dados.motivo              
        skip.             
end.
output close.
RUN pi-finalizar IN h-acomp.

procedure pi-custo-prod.
DEFINE VARIABLE i-esp-atu AS INTEGER     NO-UNDO.
DEFINE VARIABLE dt-atu AS DATE        NO-UNDO.
DEFINE VARIABLE dt-ini AS DATE        NO-UNDO.
DEFINE VARIABLE dt-fim AS DATE        NO-UNDO.
DEFINE VARIABLE i-sinal AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-trans AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ict     AS INTEGER     NO-UNDO.
DEFINE VARIABLE d-tot-mat   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE d-tot-custo AS DECIMAL     NO-UNDO.
define buffer bitem for item.
      
      

    c-trans = "1,8,5,7,31,28,30".

    do dt-atu = dt-ref-ini to dt-ref-fim with frame f1:      
      do ict = 1 to 7:             
            i-esp-atu = int(entry(ict,c-trans)).
            run pi-acompanhar in h-acomp (input string( dt-atu) + " " + string( i-esp-atu)). 
          
            for each movto-estoq fields (it-codigo cod-estabel quantidade esp-docto tipo-trans valor-ggf-m movto-estoq.valor-mat-m movto-estoq.valor-mob-m) where
                movto-estoq.cod-estabel >= tt-param.c-cod-estabel-ini  and
                movto-estoq.cod-estabel <= tt-param.c-cod-estabel-fim  and
                movto-estoq.dt-trans     = dt-atu and
                movto-estoq.esp-docto    = i-esp-atu no-lock,                 
                    first ord-prod fields (it-codigo) where 
                        ord-prod.nr-ord-produ = movto-estoq.nr-ord-produ and
                        ord-prod.it-codigo   >= tt-param.c-item-ini  and
                        ord-prod.it-codigo   <= tt-param.c-item-fim  and
                        ord-prod.nr-linha    <> 4 and
                     ((ord-prod.ct-codigo = "11040207") or (ord-prod.ct-codigo = "11040211")) no-lock,
                    first bitem fields (it-codigo ge-codigo) where bitem.it-codigo = ord-prod.it-codigo and
                              bitem.ge-codigo    >= tt-param.i-ge-codigo-ini    and
                              bitem.ge-codigo    <= tt-param.i-ge-codigo-fim    and
                              bitem.fm-codigo    >= tt-param.c-fm-codigo-ini    and
                              bitem.fm-codigo    <= tt-param.c-fm-codigo-fim    and
                              bitem.fm-cod-com   >= tt-param.c-fm-cod-com-ini   and
                              bitem.fm-cod-com   <= tt-param.c-fm-cod-com-fim no-lock.
    
                i-sinal = (if movto-estoq.tipo-trans = 1 then 1 else -1).
                find first tt-custo where                     
                    tt-custo.it-codigo   =  bitem.it-codigo   and
                    tt-custo.cod-estabel =  movto-estoq.cod-estabel no-error.
                if not avail tt-custo then do:
                    create tt-custo.
                    assign tt-custo.it-codigo   =  bitem.it-codigo   
                           tt-custo.cod-estabel =  movto-estoq.cod-estabel .
                end.
    
                if i-esp-atu <> 1 and i-esp-atu <> 8 then do:                     
                    if movto-estoq.it-codigo = "2600100" then 
                        assign tt-custo.valor-ovo = tt-custo.valor-ovo + ((movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-mob-m[1]) * i-sinal * -1).                                                   
                    else
                    if movto-estoq.it-codigo = "2100002" then 
                        assign tt-custo.valor-ovo = tt-custo.valor-ovo + ((movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-mob-m[1]) * i-sinal * -1).
                                                   else
                        if movto-estoq.it-codigo = "2600001" then 
                        assign tt-custo.valor-rac = tt-custo.valor-rac + ((movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-mob-m[1]) * i-sinal * -1).
                               
                    else
                        if movto-estoq.it-codigo = "2600002" then 
                        assign tt-custo.valor-rac = tt-custo.valor-rac + ((movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-mob-m[1]) * i-sinal * -1).
                               
                    else
                        assign tt-custo.valor-emb = tt-custo.valor-emb + ((movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-mob-m[1]) * i-sinal * -1).
                               
                end. 
                else do:
                    assign                 
                        tt-custo.quantidade = tt-custo.quantidade + (movto-estoq.quantidade * i-sinal)
                        tt-custo.valor      = tt-custo.valor      + ((movto-estoq.valor-mat-m[1]) * i-sinal)
                        tt-custo.valor-ggf  = tt-custo.valor-ggf  + ((movto-estoq.valor-ggf-m[1] + movto-estoq.valor-mob-m[1]) * i-sinal).
                end.                 
            end.           
      end.


        for each movto-ggf fields (it-codigo cod-estabel tipo-trans 
                                   movto-ggf.valor-ggf-1-m
                                   movto-ggf.valor-ggf-2-m
                                   movto-ggf.valor-ggf-3-m
                                   movto-ggf.valor-ggf-4-m
                                   movto-ggf.valor-ggf-5-m
                                   movto-ggf.valor-ggf-6-m) where
                                movto-ggf.cod-estabel >= tt-param.c-cod-estabel-ini  and
                                movto-ggf.cod-estabel <= tt-param.c-cod-estabel-fim  and
                                movto-ggf.it-codigo   >= tt-param.c-item-ini    and
                                movto-ggf.it-codigo   <= tt-param.c-item-fim    and
                                movto-ggf.dt-trans     = dt-atu   no-lock,                 
                            first ord-prod fields (it-codigo) where ord-prod.nr-ord-produ = movto-ggf.nr-ord-produ and
                                                                    ord-prod.nr-linha <> 4 no-lock,
                            first bitem fields (it-codigo ge-codigo) where bitem.it-codigo = ord-prod.it-codigo and
                                  bitem.ge-codigo >= tt-param.i-ge-codigo-ini and
                                  bitem.ge-codigo <= tt-param.i-ge-codigo-fim no-lock.

              i-sinal = (if movto-ggf.tipo-trans = 1 then 1 else -1).

           find first tt-custo where                    
                    tt-custo.it-codigo    =  bitem.it-codigo   and
                    tt-custo.cod-estabel  =  movto-ggf.cod-estabel no-error.
            if not avail tt-custo then do:
                create tt-custo.
                assign  
                       tt-custo.it-codigo    =  bitem.it-codigo   
                       tt-custo.cod-estabel  =  movto-ggf.cod-estabel .
            end.
                    
            assign 
                tt-custo.valor-ggf-proc = tt-custo.valor-ggf-proc + ((movto-ggf.valor-ggf-1-m[1] + 
                                                                      movto-ggf.valor-ggf-2-m[1] + 
                                                                      movto-ggf.valor-ggf-3-m[1] +
                                                                      movto-ggf.valor-ggf-4-m[1] +
                                                                      movto-ggf.valor-ggf-5-m[1] +
                                                                      movto-ggf.valor-ggf-6-m[1] ) * i-sinal).              
         end.
    end.


    for each tt-custo  no-lock.

        if  tt-custo.quantidade > 0 then do:
            assign                 
                d-tot-custo        = tt-custo.valor + tt-custo.valor-ggf
                d-tot-mat          = tt-custo.valor-rac + tt-custo.valor-ovo + tt-custo.valor-emb 
                tt-custo.medio     = d-tot-custo / tt-custo.quantidade
                tt-custo.medio-mat = d-tot-custo - tt-custo.valor-ggf-proc                
                tt-custo.medio-ggf-proc = tt-custo.valor-ggf-proc / d-tot-custo * 100.
            if (d-tot-mat) > 0 then
                assign                
                    tt-custo.medio-emb = (tt-custo.medio-mat * (tt-custo.valor-emb / d-tot-mat )) /  d-tot-custo * 100 
                    tt-custo.medio-ovo = (tt-custo.medio-mat * (tt-custo.valor-ovo / d-tot-mat )) /  d-tot-custo * 100
                    tt-custo.medio-rac = (tt-custo.medio-mat * (tt-custo.valor-rac / d-tot-mat )) /  d-tot-custo * 100.
        end.
    end.



end.

