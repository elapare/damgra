/********************************************************************************
** Copyright MM Pereira
** Todos os Direitos Reservados.
*******************************************************************************/
/*********************************************************************************
**
** Programa: esmi0013
**
** Funcao..: Relat¢rio Plano Equipamento
**
** Autor..: Moises Pereira
** Data...: 19/08/2016
*********************************************************************************/
{include/i-prgvrs.i esmi0013RP 2.00.00.000}  /*** 010000 ***/
/*********************************************************************************
                                    DEFINI€åES
*********************************************************************************/   
{utp/ut-glob.i}

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-TEMP-TABLES INTERNAS -=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
DEF TEMP-TABLE tt-mi-espec NO-UNDO
    FIELD tp-especial  AS CHAR
    FIELD descricao    AS CHAR
    FIELD abreviatura  AS CHAR
    INDEX ch-principal IS PRIMARY UNIQUE tp-especial.

DEF TEMP-TABLE tt-man-equip NO-UNDO
    FIELD id-linha            AS INT
    FIELD cd-manut            LIKE man-equip.cd-manut
    FIELD cd-equipto          LIKE man-equip.cd-equipto
    FIELD desc-equipto        LIKE equipto.descricao                 LABEL "Descri‡Æo Equipto"           COLUMN-LABEL "Descri‡Æo Equipto"
    FIELD fm-equipto          LIKE equipto.fm-equipto                
    FIELD cd-tag              LIKE equipto.cd-tag                    
    FIELD c-situacao          AS CHAR FORMAT "X(20)"                 LABEL "Situa‡Æo"                    COLUMN-LABEL "Situa‡Æo" 
    FIELD c-controle          AS CHAR FORMAT "x(20)"                 LABEL "Controle"                    COLUMN-LABEL "Controle"
    FIELD faixa-tempo         LIKE man-equip.faixa-tempo   
    FIELD faixa-utiliz        LIKE man-equip.faixa-utiliz  
    FIELD descricao           LIKE man-equip.descricao  
    FIELD custo-planejado     AS DEC FORMAT "->>>,>>>,>>9.99"        LABEL "Custo Planejado"             COLUMN-LABEL "Custo Planejado"
    FIELD cd-parada           LIKE man-equip.cd-parada
    FIELD cd-tipo             LIKE man-equip.cd-tipo
    FIELD corringa-1          AS CHAR                                LABEL "corringa-1"                  COLUMN-LABEL "corringa-1"
    FIELD tempo-tot-tar       AS DEC DECIMALS 4 FORMAT "->>>,>>9.99" LABEL "Tempo Tarefa"
	FIELD gut-gravidade       LIKE ext-gut.gravidade
	FIELD gut-urgencia	      LIKE ext-gut.urgencia
	FIELD gut-tendencia       LIKE ext-gut.tendencia
	FIELD prioridade          LIKE ord-manut.prioridade
    FIELD dat-ult-realiz      AS DATE FORMAT "99/99/9999"            LABEL "Data Ult Realiz"             COLUMN-LABEL "Data Ult Realiz"
    FIELD nr-ord-produ        LIKE ord-manut.nr-ord-produ            LABEL "Ordem Ult Realiz"            COLUMN-LABEL "Ordem Ult Realiz"
    FIELD c-estado-om         AS CHAR FORMAT "X(12)"                 LABEL "Estado OM"
    FIELD dat-prox-realiz     AS DATE FORMAT "99/99/9999"            LABEL "Data Pr¢x Realiz"            COLUMN-LABEL "Data Pr¢x Realiz"
    FIELD narrativa           LIKE man-equip.narrativa               LABEL "Narrativa"                   COLUMN-LABEL "Narrativa"
    FIELD ficha-metodo        AS CHAR FORMAT "x(1500)"               LABEL "Ficha M‚todo"                COLUMN-LABEL "Ficha M‚todo".

DEF TEMP-TABLE tt-reservas NO-UNDO
    FIELD cd-manut            LIKE man-equip.cd-manut
    FIELD cd-equipto          LIKE man-equip.cd-equipto
    FIELD desc-equipto        LIKE equipto.descricao                 LABEL "Descri‡Æo Equipto"           COLUMN-LABEL "Descri‡Æo Equipto"
    FIELD fm-equipto          LIKE equipto.fm-equipto                
    FIELD cd-tag              LIKE equipto.cd-tag                    
    FIELD c-situacao          AS CHAR FORMAT "X(20)"                 LABEL "Situa‡Æo"                    COLUMN-LABEL "Situa‡Æo" 
    FIELD c-controle          AS CHAR FORMAT "x(20)"                 LABEL "Controle"                    COLUMN-LABEL "Controle"
    FIELD faixa-tempo         LIKE man-equip.faixa-tempo   
    FIELD faixa-utiliz        LIKE man-equip.faixa-utiliz  
    FIELD descricao           LIKE man-equip.descricao  
    FIELD cd-parada           LIKE man-equip.cd-parada
    FIELD cd-tipo             LIKE man-equip.cd-tipo
	FIELD gut-gravidade       LIKE ext-gut.gravidade
	FIELD gut-urgencia	      LIKE ext-gut.urgencia
	FIELD gut-tendencia       LIKE ext-gut.tendencia
	FIELD prioridade          LIKE ord-manut.prioridade
    FIELD dat-ult-realiz      AS DATE FORMAT "99/99/9999"            LABEL "Data Ult Realiz"             COLUMN-LABEL "Data Ult Realiz"
    FIELD nr-ord-produ        LIKE ord-manut.nr-ord-produ            LABEL "Ordem Ult Realiz"            COLUMN-LABEL "Ordem Ult Realiz"
    FIELD c-estado-om         AS CHAR FORMAT "X(12)"                 LABEL "Estado OM"
    FIELD dat-prox-realiz     AS DATE FORMAT "99/99/9999"            LABEL "Data Pr¢x Realiz"            COLUMN-LABEL "Data Pr¢x Realiz"
    /*Dados de Materiais*/
    FIELD cd-tarefa           LIKE ord-taref.cd-tarefa
    FIELD it-codigo           LIKE ITEM.it-codigo
    FIELD un                  LIKE ITEM.un
    FIELD desc-item           LIKE ITEM.desc-item                          LABEL "Descri‡Æo Item"        COLUMN-LABEL "Descri‡Æo Item"
    FIELD quantidade          LIKE movto-estoq.quantidade
    FIELD valor-unit          AS DEC DECIMALS 4 FORMAT "->>>,>>>,>>9.9999" LABEL "Valor Unitµrio"
    FIELD valor-total         AS DEC DECIMALS 4 FORMAT "->>>,>>>,>>9.9999" LABEL "Valor Total"
    FIELD qtidade-estoque     LIKE saldo-estoq.qtidade-atu                 LABEL "Qtde Estoque"          COLUMN-LABEL "Qtde Estoque"
    FIELD localizacoes        AS CHAR FORMAT "x(1500)"                     LABEL "Localiza‡äes"          
    INDEX ch-principal IS PRIMARY UNIQUE cd-manut cd-equipto cd-tarefa it-codigo.

DEF TEMP-TABLE tt-tar-esp-eq NO-UNDO LIKE tar-esp-eq.

DEF TEMP-TABLE tt-tar-fich-met-eq NO-UNDO
    FIELD cd-equipto      LIKE tar-fich-met-eq.cd-equipto
    FIELD cd-manut        LIKE tar-fich-met-eq.cd-manut  
    FIELD cd-tarefa       LIKE tar-fich-met-eq.cd-tarefa 
    FIELD fi-codigo       LIKE tar-fich-met-eq.fi-codigo         LABEL "Ficha"            COLUMN-LABEL "Ficha"          
    FIELD fi-complemento  LIKE mnt-ficha-metodo.fi-complemento   LABEL "Complento"        COLUMN-LABEL "Complento"      
    FIELD descricao       LIKE mnt-ficha-metodo.descricao        LABEL "Descri‡Æo Ficha"  COLUMN-LABEL "Descri‡Æo Ficha"
    FIELD responsavel     LIKE mnt-ficha-metodo.responsavel      LABEL "Respons vel"      COLUMN-LABEL "Respons vel"    
    FIELD narrativa       LIKE mnt-ficha-metodo.narrativa        LABEL "Narrativa"        COLUMN-LABEL "Narrativa"      
    INDEX ch-principal IS PRIMARY UNIQUE cd-equipto cd-manut cd-tarefa fi-codigo.

def temp-table tt-raw-digita
    field raw-digita as raw.

/* -=-=-=-=-=-=-=-=-=-=-=-=-=- VARIµVEIS INTERNAS -=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

DEF VAR c-estado-om        AS CHAR EXTENT 4                            NO-UNDO.
DEF VAR h-acomp            AS HANDLE                                   NO-UNDO.
DEF VAR i-acomp            AS INT                                      NO-UNDO.
/* -=-=-=-=-=-=-=-=-=-=-=-=-=- VARIµVEIS GLOBAIS -=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
{mip/esmiapi0001g.i}
/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- INCLUDES -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

{cdp/cdcfgman.i}
{cdp/cd0666.i}
{utp/ut-glob.i}
{method/dbotterr.i} /*rowErrors*/
{include/i-rpvar.i}
{mip\esmi0013.i}
{utp/esutapi003a.i} /*tt-paginas*/
{utp/esutapi006a.i} /*tt-paginas-imp*/
{utp/esutapi004a.i "new"} 
{mip/mi0303.i1 NEW}          /*tt-mip-ord-simul*/
/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- BUFFERS -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

/* =-=-=-=-=-=-=-=-=-=-=-=-=-=- FRAMES E FORMS -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

/****************************************************************************/

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- PAR¶METROS -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */                                              
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.
/****************************************************************************/
                    
/* -=-=-=-=-=- Transferˆncia de parƒmetros para temp-table padrÆo -=-=-=-=-=-*/
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
ASSIGN c-programa     = "esmi0013":U
       c-versao       = "2.00":U
       c-revisao      = ".00.000":U
       c-empresa      = param-global.grupo
       c-sistema      = "Manuten‡Æo"
       c-titulo-relat = "Relat¢rio Plano Equipamento".

/* Outras includes */  
{include/i-rpcab.i} 
{include/i-rpout.i}

VIEW FRAME f-cabec.
VIEW FRAME f-rodape. 

RUN pi-inicializa.

RUN pi-gera-dados-exportacao.

RUN pi-exporta-dados.
    
PUT "Execu‡Æo realizada com sucesso" SKIP.

RUN pi-finaliza.

/** Fecha o output para arquivo **/
{include/i-rpclo.i}

/*********************************************************************************
                                   PROCEDURES
**********************************************************************************
**********************************************************************************/

PROCEDURE pi-inicializa:
    
    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

    RUN pi-inicializar IN h-acomp (INPUT 'Relat¢rios Planos').

    ASSIGN c-estado-om[1] = "NÆo Iniciada"
           c-estado-om[2] = "Iniciada"
           c-estado-om[3] = "Suspensa"
           c-estado-om[4] = "Terminada".

    RUN pi-param-espec. /*Ajusta por usu rio as especilidades que serÆo listadas por padrao*/

    EMPTY TEMP-TABLE tt-mi-espec.
    FOR EACH mi-espec NO-LOCK:
        FIND FIRST tt-mi-espec
            WHERE tt-mi-espec.tp-especial = mi-espec.tp-especial NO-ERROR.
        IF NOT AVAIL tt-mi-espec THEN DO:
            CREATE tt-mi-espec.
            ASSIGN tt-mi-espec.tp-especial = mi-espec.tp-especial
                   tt-mi-espec.descricao   = mi-espec.descricao.

            IF NUM-ENTRIES(tt-mi-espec.descricao,"[") = 2 THEN
                ASSIGN tt-mi-espec.abreviatura = ENTRY(1,ENTRY(2,tt-mi-espec.descricao,"["),"]").
        END.
    END.

    RETURN "OK".

END.

PROCEDURE pi-finaliza:

    RUN pi-finalizar IN h-acomp.
    
    RETURN "OK".
END.


PROCEDURE pi-gera-dados-exportacao:
    DEF VAR i-acomp       AS INT                   NO-UNDO.
    DEF VAR i-id-linha    AS INT                   NO-UNDO.
    DEF VAR de-valor      AS DEC DECIMALS 4        NO-UNDO.
    DEF VAR i-cst         AS INT                   NO-UNDO.
    DEF VAR i-gut         AS INT                   NO-UNDO.
    DEF VAR i-prioridade  AS INT                   NO-UNDO.
    DEF VAR d-data-refer  AS DATE                  NO-UNDO.
    DEF VAR de-tempo-esp  LIKE tt-tar-esp-eq.tempo NO-UNDO.

    EMPTY TEMP-TABLE tt-man-equip.
    EMPTY TEMP-TABLE tt-reservas.
    EMPTY TEMP-TABLE tt-tar-fich-met-eq .

    FOR EACH man-equip NO-LOCK
        WHERE man-equip.cd-manut     >= tt-param.cd-manut-ini
          AND man-equip.cd-manut     <= tt-param.cd-manut-fim
          AND man-equip.cd-equipto   >= tt-param.cd-equipto-ini
          AND man-equip.cd-equipto   <= tt-param.cd-equipto-fim
          AND man-equip.cd-planejado >= tt-param.cd-planejador-ini
          AND man-equip.cd-planejado <= tt-param.cd-planejador-fim
          AND man-equip.descricao    MATCHES tt-param.descricao:

        FIND FIRST equipto NO-LOCK
            WHERE equipto.cd-equipto = man-equip.cd-equipto NO-ERROR.
        IF NOT AVAIL equipto THEN NEXT.

        IF equipto.fm-equipto < tt-param.fm-codigo-ini OR
           equipto.fm-equipto > tt-param.fm-codigo-fim THEN NEXT.

        IF equipto.cd-tag < tt-param.cod-tag-ini OR
           equipto.cd-tag > tt-param.cod-tag-fim THEN NEXT.

        ASSIGN i-acomp = i-acomp + 1.

        IF i-acomp MODULO 5 = 0 THEN
            run pi-acompanhar in h-acomp (input "Buscando Plano " + STRING(i-acomp)).

        ASSIGN i-id-linha = i-id-linha + 1.

        CREATE tt-man-equip.
        BUFFER-COPY man-equip TO tt-man-equip
            ASSIGN tt-man-equip.id-linha = i-id-linha.

        ASSIGN tt-man-equip.desc-equipto = equipto.descricao 
               tt-man-equip.fm-equipto   = equipto.fm-equipto
               tt-man-equip.cd-tag       = equipto.cd-tag.

        CASE man-equip.situacao:
            WHEN 1 THEN ASSIGN tt-man-equip.c-situacao = "Ativo".
            WHEN 2 THEN ASSIGN tt-man-equip.c-situacao = "Suspensa".
            WHEN 3 THEN ASSIGN tt-man-equip.c-situacao = "Eliminada".
        END CASE.

        CASE man-equip.controle:
            WHEN 1 THEN ASSIGN tt-man-equip.c-controle = "Faixa Tempo".
            WHEN 2 THEN ASSIGN tt-man-equip.c-controle = "Utiliza‡Æo".
            WHEN 3 THEN ASSIGN tt-man-equip.c-controle = "Ambos".
        END CASE.

        ASSIGN tt-man-equip.narrativa    = man-equip.narrativa
               tt-man-equip.narrativa    = REPLACE(tt-man-equip.narrativa,CHR(13),"|")
               tt-man-equip.narrativa    = REPLACE(tt-man-equip.narrativa,CHR(10),"|")
               tt-man-equip.narrativa    = REPLACE(tt-man-equip.narrativa,"–","|").

        FOR EACH tar-item-eq NO-LOCK
            WHERE tar-item-eq.cd-manut   = man-equip.cd-manut
              AND tar-item-eq.cd-equipto = man-equip.cd-equipto:

            FIND FIRST ITEM NO-LOCK
                WHERE ITEM.it-codigo = tar-item-eq.it-codigo NO-ERROR.
            IF NOT AVAIL ITEM THEN NEXT.

            FIND FIRST tt-reservas 
                WHERE tt-reservas.cd-manut   = man-equip.cd-manut      
                  AND tt-reservas.cd-equipto = man-equip.cd-equipto    
                  AND tt-reservas.cd-tarefa  = tar-item-eq.cd-tarefa   
                  AND tt-reservas.it-codigo  = tar-item-eq.it-codigo   NO-ERROR.
            IF NOT AVAIL tt-reservas THEN DO:
                CREATE tt-reservas.
                BUFFER-COPY tt-man-equip TO tt-reservas
                    ASSIGN tt-reservas.cd-manut   = man-equip.cd-manut  
                           tt-reservas.cd-equipto = man-equip.cd-equipto
                           tt-reservas.cd-tarefa  = tar-item-eq.cd-tarefa 
                           tt-reservas.it-codigo  = tar-item-eq.it-codigo 
                           tt-reservas.desc-item  = ITEM.desc-item
                           tt-reservas.un         = ITEM.un
                           tt-reservas.quantidade = 0.

                /*Montando o custo previsto*/
                FIND LAST pr-it-per NO-LOCK
                    WHERE pr-it-per.cod-estabel = equipto.cod-estabel 
                      AND pr-it-per.it-codigo   = tar-item-eq.it-codigo      NO-ERROR.
                IF AVAIL pr-it-per THEN DO:
                    ASSIGN tt-reservas.valor-unit = (pr-it-per.val-unit-mat-m[1] + 
                                                     pr-it-per.val-unit-mob-m[1] + 
                                                     pr-it-per.val-unit-ggf-m[1]).
                END.
                ELSE DO:
                    FIND FIRST item-uni-estab NO-LOCK
                        WHERE item-uni-estab.cod-estabel = equipto.cod-estabel
                          AND item-uni-estab.it-codigo   = tar-item-eq.it-codigo      NO-ERROR.
                    IF AVAIL item-uni-estab THEN DO:
                        IF item-uni-estab.preco-ul-ent <> 0 THEN
                            ASSIGN tt-reservas.valor-unit = item-uni-estab.preco-ul-ent.
                        ELSE IF item-uni-estab.preco-repos <> 0 THEN 
                                 ASSIGN tt-reservas.valor-unit = item-uni-estab.preco-repos.
                             ELSE IF item-uni-estab.preco-base <> 0 THEN
                                      ASSIGN tt-reservas.valor-unit = item-uni-estab.preco-base.
                                  ELSE
                                      ASSIGN tt-reservas.valor-unit = 1.
                    END.
                END.
            END.

            ASSIGN tt-reservas.quantidade  = tt-reservas.quantidade + tar-item-eq.quantidade
                   tt-reservas.valor-total = tt-reservas.quantidade * tt-reservas.valor-unit.
        END.

        ASSIGN tt-man-equip.custo-planejado = 0.
        FOR EACH tt-reservas
            WHERE tt-reservas.cd-manut   = man-equip.cd-manut
              AND tt-reservas.cd-equipto = man-equip.cd-equipto:
            
            ASSIGN tt-man-equip.custo-planejado = tt-man-equip.custo-planejado + tt-reservas.valor-total. 

            ASSIGN tt-reservas.localizacoes    = ""
                   tt-reservas.qtidade-estoque = 0.

            FOR EACH saldo-estoq NO-LOCK
                WHERE saldo-estoq.cod-estabel  = equipto.cod-estabel
                  AND saldo-estoq.it-codigo    = tt-reservas.it-codigo
                  AND saldo-estoq.qtidade-atu <> 0:

                IF tt-reservas.localizacoes <> "" THEN 
                    ASSIGN tt-reservas.localizacoes = tt-reservas.localizacoes + "|".
                
                ASSIGN tt-reservas.localizacoes = tt-reservas.localizacoes + 
                                                  STRING(saldo-estoq.cod-depos,"x(3)") + "/" + STRING(saldo-estoq.cod-localiz,"x(10)") + " - " +
                                                  "Qt Disp  " + STRING(saldo-estoq.qtidade-atu - 
                                                                       (saldo-estoq.qt-alocada + 
                                                                       saldo-estoq.qt-aloc-ped + 
                                                                       saldo-estoq.qt-aloc-prod),"->>>,>>>,>>9.9999")  + " - " +
                                                  "Qt Atual " + STRING(saldo-estoq.qtidade-atu,"->>>,>>>,>>9.9999")    + " - " +
                                                  "Qt Aloc  " + STRING(saldo-estoq.qt-alocada + 
                                                                       saldo-estoq.qt-aloc-ped + 
                                                                       saldo-estoq.qt-aloc-prod,"->>>,>>>,>>9.9999").
                ASSIGN tt-reservas.qtidade-estoque = tt-reservas.qtidade-estoque + (saldo-estoq.qtidade-atu - 
                                                                                    (saldo-estoq.qt-alocada + 
                                                                                     saldo-estoq.qt-aloc-ped + 
                                                                                     saldo-estoq.qt-aloc-prod)).
            END.     
        END.

        FOR EACH mi-eq-tar NO-LOCK
            WHERE mi-eq-tar.cd-manut   = man-equip.cd-manut
              AND mi-eq-tar.cd-equipto = man-equip.cd-equipto:

            IF CAN-FIND(FIRST tar-esp-eq NO-LOCK
                        WHERE tar-esp-eq.cd-manut     = mi-eq-tar.cd-manut
                          AND tar-esp-eq.cd-equipto   = mi-eq-tar.cd-equipto
                          AND tar-esp-eq.cd-tarefa    = mi-eq-tar.cd-tarefa) THEN DO:
                
                FOR EACH tar-esp-eq NO-LOCK
                    WHERE tar-esp-eq.cd-manut     = mi-eq-tar.cd-manut  
                      AND tar-esp-eq.cd-equipto   = mi-eq-tar.cd-equipto
                      AND tar-esp-eq.cd-tarefa    = mi-eq-tar.cd-tarefa:

                    CREATE tt-tar-esp-eq.
                    ASSIGN tt-tar-esp-eq.cd-manut     = tar-esp-eq.cd-manut
                           tt-tar-esp-eq.cd-equipto   = tar-esp-eq.cd-equipto
                           tt-tar-esp-eq.cd-tarefa    = tar-esp-eq.cd-tarefa   
                           tt-tar-esp-eq.tp-especial  = tar-esp-eq.tp-especial 
                           tt-tar-esp-eq.nr-homens    = tar-esp-eq.nr-homens   
                           tt-tar-esp-eq.tipo-tempo   = tar-esp-eq.tipo-tempo
                           tt-tar-esp-eq.tempo        = tar-esp-eq.tempo.
                    
                    /**************************************************************************************************
                    Tipo Tempo - Informar tipo de tempo da especialidade, as op‡äes dispon¡veis sÆo:
                          Individual: significa que o tempo informado ‚ para um homem.
                          Total: significa que o tempo representa o total de horas, considerando todos os homens.
                    **************************************************************************************************      
                    IF tar-esp-eq.tipo-tempo = 1 THEN
                        ASSIGN tt-tar-esp-eq.tempo = tt-tar-esp-eq.tempo * tt-tar-esp-eq.nr-homens.
                    */

                    IF tt-tar-esp-eq.tipo-tempo = 1 AND tt-tar-esp-eq.nr-homens > 1 THEN 
                        ASSIGN de-tempo-esp = tt-tar-esp-eq.tempo * tt-tar-esp-eq.nr-homens.
                    ELSE
                        ASSIGN de-tempo-esp = tt-tar-esp-eq.tempo.

                    /*Custo Previsto*/
                    FIND FIRST mi-espec NO-LOCK
                        WHERE mi-espec.tp-especial = tar-esp-eq.tp-especial NO-ERROR.
                    IF AVAIL mi-espec THEN DO:
                        IF mi-espec.custo = 2 /*Valor Informado*/ THEN DO:
                            ASSIGN tt-man-equip.custo-planejado = tt-man-equip.custo-planejado + (mi-espec.valor-hora * de-tempo-esp).
                        END.
                        ELSE DO:
                            FIND LAST ext-per-custo NO-LOCK
                                WHERE ext-per-custo.cc-codigo = mi-espec.cc-codigo
                                  AND ext-per-custo.mo-codigo = 0                  NO-ERROR.
                            IF AVAIL ext-per-custo THEN DO:
                                REPEAT i-cst = 1 TO 6:
                                    ASSIGN tt-man-equip.custo-planejado = tt-man-equip.custo-planejado + (ext-per-custo.custo-prev[i-cst] * de-tempo-esp).
                                END.
                            END.

                            IF mi-espec.cd-mob-dir <> "" THEN DO:
                                FIND FIRST tab-mob-dir NO-LOCK
                                    WHERE tab-mob-dir.cd-mob-dir = mi-espec.cd-mob-dir NO-ERROR.
                                IF AVAIL tab-mob-dir THEN DO:
                                    ASSIGN tt-man-equip.custo-planejado = tt-man-equip.custo-planejado + (tab-mob-dir.vl-orcado[1] * de-tempo-esp).
                                END.
                            END.
                        END.
                    END.

                    ASSIGN tt-man-equip.tempo-tot-tar = tt-man-equip.tempo-tot-tar  + de-tempo-esp.
                END.
            END.
            ELSE DO:
                CREATE tt-tar-esp-eq.
                ASSIGN tt-tar-esp-eq.cd-manut     = mi-eq-tar.cd-manut
                       tt-tar-esp-eq.cd-equipto   = mi-eq-tar.cd-equipto
                       tt-tar-esp-eq.cd-tarefa    = mi-eq-tar.cd-tarefa   
                       tt-tar-esp-eq.tp-especial  = ""
                       tt-tar-esp-eq.nr-homens    = 1
                       tt-tar-esp-eq.tipo-tempo   = 1
                       tt-tar-esp-eq.tempo        = mi-eq-tar.tempo.

                ASSIGN tt-man-equip.tempo-tot-tar = tt-man-equip.tempo-tot-tar + tt-tar-esp-eq.tempo.

            END.
        END.


        ASSIGN tt-man-equip.prioridade    = ?
               tt-man-equip.gut-gravidade = ? 
               tt-man-equip.gut-urgencia  = ? 
               tt-man-equip.gut-tendencia = ?.

        FIND FIRST ext-gut NO-LOCK
            WHERE ext-gut.nr-ord-produ = 0
              AND ext-gut.cd-equipto   = man-equip.cd-equipto
              AND ext-gut.cd-manut     = man-equip.cd-manut   NO-ERROR.
        
        IF AVAIL ext-gut THEN DO:
            ASSIGN tt-man-equip.gut-gravidade = ext-gut.gravidade 
                   tt-man-equip.gut-urgencia  = ext-gut.urgencia  
                   tt-man-equip.gut-tendencia = ext-gut.tendencia.

            ASSIGN i-gut = ext-gut.gravidade *
                           ext-gut.urgencia  *
                           ext-gut.tendencia.

            ASSIGN i-prioridade = ?.
            IF i-gut  >= 100 AND i-prioridade = ? THEN ASSIGN i-prioridade = 100.
            IF i-gut  >=  75 AND i-prioridade = ? THEN ASSIGN i-prioridade = 200.
            IF i-gut  >=  50 AND i-prioridade = ? THEN ASSIGN i-prioridade = 300.
            IF i-gut  >=  25 AND i-prioridade = ? THEN ASSIGN i-prioridade = 400.
            IF i-gut  >    0 AND i-prioridade = ? THEN ASSIGN i-prioridade = 500.
            
            ASSIGN tt-man-equip.prioridade = i-prioridade.
        END.

        find last ord-manut no-lock use-index data-equipto
            where ord-manut.cd-equipto = man-equip.cd-equipto
            and   ord-manut.cd-manut   = man-equip.cd-manut   no-error.
        IF AVAIL ord-manut THEN DO:
            ASSIGN tt-man-equip.nr-ord-produ = ord-manut.nr-ord-produ
                   tt-man-equip.c-estado-om  = c-estado-om[ord-manut.estado-om].

            IF ord-manut.estado = 4 THEN
                ASSIGN tt-man-equip.dat-ult-realiz = ord-manut.dt-fecham.
            ELSE
                ASSIGN tt-man-equip.dat-ult-realiz = ord-manut.dt-prev.
        END.
        ELSE DO:
            ASSIGN tt-man-equip.dat-ult-realiz = MAX(MAX(equipto.data-aquis,equipto.data-ativa),man-equip.dt-inicio).
        END.

        IF man-equip.situacao = 1 /*Ativo*/ AND man-equip.controle <> 2 THEN DO:
            /*Calculo simples da proxima data*/
            ASSIGN tt-man-equip.dat-prox-realiz = tt-man-equip.dat-ult-realiz + tt-man-equip.faixa-tempo.
        END.

        ASSIGN tt-man-equip.ficha-metodo = "".
        FOR EACH tar-fich-met-eq NO-LOCK
            WHERE tar-fich-met-eq.cd-manut   = man-equip.cd-manut
              AND tar-fich-met-eq.cd-equipto = man-equip.cd-equipto:

            CREATE tt-tar-fich-met-eq.
            BUFFER-COPY tar-fich-met-eq TO tt-tar-fich-met-eq.

            FIND FIRST mnt-ficha-metodo NO-LOCK
                WHERE mnt-ficha-metodo.fi-codigo = tt-tar-fich-met-eq.fi-codigo NO-ERROR.
            IF AVAIL mnt-ficha-metodo THEN DO:

                IF tt-man-equip.ficha-metodo <> "" THEN
                    ASSIGN tt-man-equip.ficha-metodo = tt-man-equip.ficha-metodo + "|".

                ASSIGN tt-man-equip.ficha-metodo = tt-man-equip.ficha-metodo + string(tar-fich-met-eq.fi-codigo,"99999") + " - " + string(mnt-ficha-metodo.fi-complemento,"x(10)") + " / " + string(mnt-ficha-metodo.descricao,"x(30)").

                ASSIGN tt-tar-fich-met-eq.fi-complemento = mnt-ficha-metodo.fi-complemento  
                       tt-tar-fich-met-eq.descricao      = mnt-ficha-metodo.descricao       
                       tt-tar-fich-met-eq.responsavel    = mnt-ficha-metodo.responsavel     
                       tt-tar-fich-met-eq.narrativa      = mnt-ficha-metodo.narrativa.

                ASSIGN tt-tar-fich-met-eq.narrativa      = REPLACE(tt-tar-fich-met-eq.narrativa,CHR(13),"|")
                       tt-tar-fich-met-eq.narrativa      = REPLACE(tt-tar-fich-met-eq.narrativa,CHR(10),"|")
                       tt-tar-fich-met-eq.narrativa      = REPLACE(tt-tar-fich-met-eq.narrativa,"–","|").
            END.
        END.
    END.

    RETURN "OK".                                
END.

PROCEDURE pi-exporta-dados:
    DEF VAR c-arquivo     AS CHAR NO-UNDO.
    DEF VAR c-formato     AS CHAR NO-UNDO.
    DEF VAR c-delim       AS CHAR NO-UNDO.
    DEF VAR c-tp-especial AS CHAR NO-UNDO.
    DEF VAR i-sequencia   AS INT  NO-UNDO.
    DEF VAR i-aux         AS INT  NO-UNDO.
    
    DEF VAR c-lista-campos  AS CHAR NO-UNDO.

    EMPTY TEMP-TABLE tt-paginas.
    EMPTY TEMP-TABLE tt-paginas-ext.

    ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "\" + "man-equipto" + ".txt"
           c-formato = ""
           c-delim   = "&^%".

    EMPTY TEMP-TABLE tt-campos-var.
    EMPTY TEMP-TABLE tt-campos-var-id.

    /*Lista de campos fixos para atender demanda do usu rio*/
    IF LOOKUP("SEM-ESP",c-gsv-mip-lista-especialid,",") = 0 THEN
        ASSIGN c-gsv-mip-lista-especialid = "SEM-ESP" + (IF c-gsv-mip-lista-especialid = "" THEN "" ELSE ",") + c-gsv-mip-lista-especialid.

    IF c-gsv-mip-lista-especialid <> "" THEN DO:
        REPEAT i-aux = 1 TO NUM-ENTRIES(c-gsv-mip-lista-especialid,","):
            ASSIGN c-tp-especial = ENTRY(i-aux,c-gsv-mip-lista-especialid,",").
            FIND FIRST tt-mi-espec NO-LOCK 
                WHERE tt-mi-espec.tp-especial = c-tp-especial NO-ERROR.
            IF AVAIL tt-mi-espec AND tt-mi-espec.abreviatura <> "" THEN
                ASSIGN c-tp-especial = tt-mi-espec.abreviatura.
            IF c-tp-especial = "SEM-ESP" THEN
                ASSIGN c-tp-especial = "SEM".
            
            /*corringa-1*/
            FIND FIRST tt-campos-var
                WHERE tt-campos-var.campo-var = "corringa-1"
                  AND tt-campos-var.titulo    = c-tp-especial + " [Q]" NO-ERROR.
            IF NOT AVAIL tt-campos-var THEN DO:
                FIND LAST tt-campos-var
                    WHERE tt-campos-var.campo-var = "corringa-1" NO-ERROR.
                IF AVAIL tt-campos-var THEN
                    ASSIGN i-sequencia = tt-campos-var.sequencia + 1.
                ELSE
                    ASSIGN i-sequencia = 1.

                CREATE tt-campos-var.
                ASSIGN tt-campos-var.campo-var = "corringa-1"                 
                       tt-campos-var.sequencia = i-sequencia
                       tt-campos-var.titulo    = c-tp-especial + " [Q]"
                       tt-campos-var.formato   = ">>>9"
                       tt-campos-var.tipo-dado = "Integer".
            END.
            
            /*corringa-1*/
            FIND FIRST tt-campos-var
                WHERE tt-campos-var.campo-var = "corringa-1"
                  AND tt-campos-var.titulo    = c-tp-especial + " [H]" NO-ERROR.
            IF NOT AVAIL tt-campos-var THEN DO:
                FIND LAST tt-campos-var
                    WHERE tt-campos-var.campo-var = "corringa-1" NO-ERROR.
                IF AVAIL tt-campos-var THEN
                    ASSIGN i-sequencia = tt-campos-var.sequencia + 1.
                ELSE
                    ASSIGN i-sequencia = 1.

                CREATE tt-campos-var.
                ASSIGN tt-campos-var.campo-var = "corringa-1"                 
                       tt-campos-var.sequencia = i-sequencia
                       tt-campos-var.titulo    = c-tp-especial + " [H]" 
                       tt-campos-var.formato   = "->>>,>>>,>>9.99"
                       tt-campos-var.tipo-dado = "Decimal".
            END.
        END.
    END.

    FOR EACH tt-man-equip:
        FOR EACH tt-tar-esp-eq NO-LOCK
            WHERE tt-tar-esp-eq.cd-manut   = tt-man-equip.cd-manut
              AND tt-tar-esp-eq.cd-equipto = tt-man-equip.cd-equipto:

            IF tt-tar-esp-eq.tp-especial = "" THEN 
                ASSIGN c-tp-especial = "SEM-ESP".
            ELSE
                ASSIGN c-tp-especial = tt-tar-esp-eq.tp-especial.

            FIND FIRST tt-mi-espec NO-LOCK 
                WHERE tt-mi-espec.tp-especial = c-tp-especial NO-ERROR.
            IF AVAIL tt-mi-espec AND tt-mi-espec.abreviatura <> "" THEN
                ASSIGN c-tp-especial = tt-mi-espec.abreviatura.
            IF c-tp-especial = "SEM-ESP" THEN
                ASSIGN c-tp-especial = "SEM".

            /*corringa-1*/
            FIND FIRST tt-campos-var
                WHERE tt-campos-var.campo-var = "corringa-1"
                  AND tt-campos-var.titulo    = c-tp-especial + " [Q]" NO-ERROR.
            IF NOT AVAIL tt-campos-var THEN DO:
                FIND LAST tt-campos-var
                    WHERE tt-campos-var.campo-var = "corringa-1" NO-ERROR.
                IF AVAIL tt-campos-var THEN
                    ASSIGN i-sequencia = tt-campos-var.sequencia + 1.
                ELSE
                    ASSIGN i-sequencia = 1.

                CREATE tt-campos-var.
                ASSIGN tt-campos-var.campo-var = "corringa-1"                 
                       tt-campos-var.sequencia = i-sequencia
                       tt-campos-var.titulo    = c-tp-especial + " [Q]"
                       tt-campos-var.formato   = ">>>9"
                       tt-campos-var.tipo-dado = "Integer".
            END.

            FIND FIRST tt-campos-var-id 
                WHERE tt-campos-var-id.id-linha  = tt-man-equip.id-linha
                  AND tt-campos-var-id.campo-var = tt-campos-var.campo-var
                  AND tt-campos-var-id.sequencia = tt-campos-var.sequencia    NO-ERROR.
            IF NOT AVAIL tt-campos-var-id THEN DO:
                CREATE tt-campos-var-id.
                ASSIGN tt-campos-var-id.id-linha  = tt-man-equip.id-linha
                       tt-campos-var-id.campo-var = tt-campos-var.campo-var  
                       tt-campos-var-id.sequencia = tt-campos-var.sequencia.
            END.
            ASSIGN tt-campos-var-id.valor = STRING(INT(tt-campos-var-id.valor) + tt-tar-esp-eq.nr-homens,tt-campos-var.formato).

            /*corringa-1*/
            FIND FIRST tt-campos-var
                WHERE tt-campos-var.campo-var = "corringa-1"
                  AND tt-campos-var.titulo    = c-tp-especial + " [H]" NO-ERROR.
            IF NOT AVAIL tt-campos-var THEN DO:
                FIND LAST tt-campos-var
                    WHERE tt-campos-var.campo-var = "corringa-1" NO-ERROR.
                IF AVAIL tt-campos-var THEN
                    ASSIGN i-sequencia = tt-campos-var.sequencia + 1.
                ELSE
                    ASSIGN i-sequencia = 1.

                CREATE tt-campos-var.
                ASSIGN tt-campos-var.campo-var = "corringa-1"                 
                       tt-campos-var.sequencia = i-sequencia
                       tt-campos-var.titulo    = c-tp-especial + " [H]"
                       tt-campos-var.formato   = "->>>,>>>,>>9.99"
                       tt-campos-var.tipo-dado = "Decimal".
            END.
            FIND FIRST tt-campos-var-id 
                WHERE tt-campos-var-id.id-linha  = tt-man-equip.id-linha
                  AND tt-campos-var-id.campo-var = tt-campos-var.campo-var
                  AND tt-campos-var-id.sequencia = tt-campos-var.sequencia    NO-ERROR.
            IF NOT AVAIL tt-campos-var-id THEN DO:
                CREATE tt-campos-var-id.
                ASSIGN tt-campos-var-id.id-linha  = tt-man-equip.id-linha
                       tt-campos-var-id.campo-var = tt-campos-var.campo-var  
                       tt-campos-var-id.sequencia = tt-campos-var.sequencia.
            END.
            ASSIGN tt-campos-var-id.valor = STRING(DEC(tt-campos-var-id.valor) + tt-tar-esp-eq.tempo,tt-campos-var.formato).
        END.
    END.

    RUN utp\esutapi004b.p (INPUT        ?,                                                                /*h-browse       */
                           INPUT        TEMP-TABLE tt-man-equip      :DEFAULT-BUFFER-HANDLE,              /*tt-dados-buffer*/
                           INPUT        "",                                                               /*p-cod-programa */
                           INPUT-OUTPUT c-arquivo,                                                        /*c-arquivo      */
                           INPUT-OUTPUT c-formato,                                                        /*c-formato      */
                           INPUT-OUTPUT c-delim,                                                          /*c-delim        */
                           INPUT        YES,                                                              /*p-cabec        */
                           INPUT        NO,                                                               /*p-browse       */
                           INPUT        "for each tt-man-equip",                                          /*p-query        */
                           INPUT        "",                                                               /*Lista de campos que serao exportados*/
                           INPUT        "id-linha",                                                       /*Lista de campos que devem ser desconsiderados*/
                           INPUT        "corringa-1",                                                     /*Lista de Campos Variaveis*/
                           INPUT        NO,                                                               /*Lista formato dos campos*/
                           INPUT        2).                                                               /*Formato Extendido 1 com [], 2 sem []*/
                                                                                                        
    CREATE tt-paginas.
    ASSIGN tt-paginas.pagina      = 1
           tt-paginas.nome-pagina = "Man-Equipto"
           tt-paginas.arquivo     = c-arquivo
           tt-paginas.formato     = c-formato
           tt-paginas.delimitador = c-delim.
    
    /*Narrativa*/
    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 1
           tt-paginas-ext.comando        = "REPLACE"
           tt-paginas-ext.parametros[1]  = "Narrativa"
           tt-paginas-ext.parametros[2]  = "|"
           tt-paginas-ext.parametros[3]  = CHR(10).

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 2
           tt-paginas-ext.comando        = "AjustaTamColuna"
           tt-paginas-ext.parametros[1]  = "Narrativa"
           tt-paginas-ext.parametros[2]  = "80".

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 3
           tt-paginas-ext.comando        = "WrapText"
           tt-paginas-ext.parametros[1]  = "Narrativa".

    /*Campo Ficha Metodo*/
    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 4
           tt-paginas-ext.comando        = "REPLACE"
           tt-paginas-ext.parametros[1]  = "Ficha M‚todo"
           tt-paginas-ext.parametros[2]  = "|"
           tt-paginas-ext.parametros[3]  = CHR(10).

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 5
           tt-paginas-ext.comando        = "WrapText"
           tt-paginas-ext.parametros[1]  = "Ficha M‚todo".


    ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "\" + "man-equipto-item" + ".txt"
           c-formato = ""
           c-delim   = "&^%".

    EMPTY TEMP-TABLE tt-campos-var.
    EMPTY TEMP-TABLE tt-campos-var-id.
    
    RUN utp\esutapi004a.p (INPUT        ?,                                                                /*h-browse       */
                           INPUT        TEMP-TABLE tt-reservas:DEFAULT-BUFFER-HANDLE,                     /*tt-dados-buffer*/
                           INPUT        "",                                                               /*p-cod-programa */
                           INPUT-OUTPUT c-arquivo,                                                        /*c-arquivo      */
                           INPUT-OUTPUT c-formato,                                                        /*c-formato      */
                           INPUT-OUTPUT c-delim,                                                          /*c-delim        */
                           INPUT        YES,                                                              /*p-cabec        */
                           INPUT        NO,                                                               /*p-browse       */
                           INPUT        "for each tt-reservas",                                           /*p-query        */
                           INPUT        "",                                                               /*Lista de campos que serao exportados*/
                           INPUT        "",                                                               /*Lista de campos que devem ser desconsiderados*/
                           INPUT        "",                                                               /*Lista de Campos Variaveis*/
                           INPUT        NO).                                                              /*Lista formato dos campos*/
                                                                                                        
    CREATE tt-paginas.
    ASSIGN tt-paginas.pagina      = 2
           tt-paginas.nome-pagina = "Materiais"
           tt-paginas.arquivo     = c-arquivo
           tt-paginas.formato     = c-formato
           tt-paginas.delimitador = c-delim.

    /*Campo Localizacoes*/
    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 2
           tt-paginas-ext.sequencia      = 1
           tt-paginas-ext.comando        = "REPLACE"
           tt-paginas-ext.parametros[1]  = "Localiza‡äes"
           tt-paginas-ext.parametros[2]  = "|"
           tt-paginas-ext.parametros[3]  = CHR(10).

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 2
           tt-paginas-ext.sequencia      = 2
           tt-paginas-ext.comando        = "AjustaTamColuna"
           tt-paginas-ext.parametros[1]  = "Localiza‡äes"
           tt-paginas-ext.parametros[2]  = "118".

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 2
           tt-paginas-ext.sequencia      = 3
           tt-paginas-ext.comando        = "WrapText"
           tt-paginas-ext.parametros[1]  = "Localiza‡äes".

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 2
           tt-paginas-ext.sequencia      = 4
           tt-paginas-ext.comando        = "Font"
           tt-paginas-ext.parametros[1]  = "Localiza‡äes"
           tt-paginas-ext.parametros[2]  = "Courier".

    ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "\" + "tar-fich-met-eq" + ".txt"
           c-formato = ""
           c-delim   = "&^%".

    EMPTY TEMP-TABLE tt-campos-var.
    EMPTY TEMP-TABLE tt-campos-var-id.
    
    RUN utp\esutapi004a.p (INPUT        ?,                                                                /*h-browse       */
                           INPUT        TEMP-TABLE tt-tar-fich-met-eq:DEFAULT-BUFFER-HANDLE,              /*tt-dados-buffer*/
                           INPUT        "",                                                               /*p-cod-programa */
                           INPUT-OUTPUT c-arquivo,                                                        /*c-arquivo      */
                           INPUT-OUTPUT c-formato,                                                        /*c-formato      */
                           INPUT-OUTPUT c-delim,                                                          /*c-delim        */
                           INPUT        YES,                                                              /*p-cabec        */
                           INPUT        NO,                                                               /*p-browse       */
                           INPUT        "for each tt-tar-fich-met-eq",                                    /*p-query        */
                           INPUT        "",                                                               /*Lista de campos que serao exportados*/
                           INPUT        "",                                                               /*Lista de campos que devem ser desconsiderados*/
                           INPUT        "",                                                               /*Lista de Campos Variaveis*/
                           INPUT        NO).                                                              /*Lista formato dos campos*/
                                                                                                        
    CREATE tt-paginas.
    ASSIGN tt-paginas.pagina      = 3
           tt-paginas.nome-pagina = "Ficha Metodo"
           tt-paginas.arquivo     = c-arquivo
           tt-paginas.formato     = c-formato
           tt-paginas.delimitador = c-delim.

    /*Narrativa*/
    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 3
           tt-paginas-ext.sequencia      = 1
           tt-paginas-ext.comando        = "REPLACE"
           tt-paginas-ext.parametros[1]  = "Narrativa"
           tt-paginas-ext.parametros[2]  = "|"
           tt-paginas-ext.parametros[3]  = CHR(10).

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 3
           tt-paginas-ext.sequencia      = 2
           tt-paginas-ext.comando        = "AjustaTamColuna"
           tt-paginas-ext.parametros[1]  = "Narrativa"
           tt-paginas-ext.parametros[2]  = "80".

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 3
           tt-paginas-ext.sequencia      = 3
           tt-paginas-ext.comando        = "WrapText"
           tt-paginas-ext.parametros[1]  = "Narrativa".

    RUN utp\esutapi003a.p(1,
                          "",
                          INPUT TABLE tt-paginas,
                          INPUT TABLE tt-paginas-ext).
END.

PROCEDURE pi-cria-erro:
    DEF INPUT PARAMETER p-cd-erro AS INT  NO-UNDO.
    DEF INPUT PARAMETER p-msg     AS CHAR NO-UNDO.

    CREATE tt-erro.
    ASSIGN tt-erro.cd-erro  = p-cd-erro
           tt-erro.mensagem = p-msg.
END.
