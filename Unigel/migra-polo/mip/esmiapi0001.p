/********************************************************************************
** Copyright MM Pereira
** Todos os Direitos Reservados.
*******************************************************************************/
{include/i-prgvrs.i esmiapi0001 2.00.00.000} 
/*********************************************************************************
**
** Programa: esmiapi0001.p
**
** Funcao..: Geraá∆o de Dados da Manutená∆o Industrial
**
** Autor..: MoisÇs Pereira
** Data...: 18/08/2016
**********************************************************************************/
/*********************************************************************************
                                    DEFINIÄÂES
*********************************************************************************/   
/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- INCLUDES -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
{cdp/cdcfgman.i} /* Miniflexibilizaá∆o */
{cdp/cd0666.i}
{utp/esutapi003a.i}
{utp/esutapi004a.i}
{utp/ut-glob.i}
/* -=-=-=-=-=-=-=-=-=-=-=-=-=- TEMP-TABLES INTERNAS -=-=-=-=-=-=-=-=-=-=-=-=-=- */
{mip\esmi0340.i}

DEF TEMP-TABLE tt-estr-tag NO-UNDO
    FIELD cod-tipo  LIKE tipo-nivel.cod-tipo
    FIELD sequencia AS INT
    FIELD cd-tag    AS CHAR
    FIELD descricao AS CHAR.

DEF TEMP-TABLE tt-mi-espec NO-UNDO
    FIELD tp-especial  AS CHAR
    FIELD descricao    AS CHAR
    FIELD abreviatura  AS CHAR
    INDEX ch-principal IS PRIMARY UNIQUE tp-especial.

DEF TEMP-TABLE tt-deposito NO-UNDO
    FIELD cod-depos   LIKE deposito.cod-depos
    FIELD log-localiz AS LOG INIT NO
    FIELD sequencia   AS INT
    INDEX ch-principal IS PRIMARY UNIQUE cod-depos
    INDEX ch-seq       sequencia.

/* -=-=-=-=-=-=-=-=-=-=-=-=-=- VARIµVEIS INTERNAS -=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
DEF VAR h-acomp               AS HANDLE                    NO-UNDO.
DEF VAR i-acomp               AS INT                       NO-UNDO.
DEF VAR c-estado-om           AS CHAR EXTENT 4             NO-UNDO.
DEF VAR c-estado              AS CHAR EXTENT 8             NO-UNDO.
DEF VAR l-considera-dep       AS LOG                       NO-UNDO.
/* -=-=-=-=-=-=-=-=-=-=-=-=-=- VARIµVEIS GLOBAIS -=-=-=-=-=-=-=-=-=-=-=-=-=-=- */
{mip/esmiapi0001g.i}
/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- BUFFERS -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- PAR∂METROS -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

/*********************************************************************************
                                   MAIN-BLOCK
*********************************************************************************/  

RUN piInicializa.

RETURN "OK".
/*********************************************************************************
                                     PROCEDURES
**********************************************************************************
piInicializa

piAtualizaOrdens
--piCriaTecnico
--piCriaOM
----piDetalhaNec
----piAtualizaTempo
----piAjustaSeqTecnico

piDetalhaOM

piAtualizaTecnico

piGeraCarga

piAtualizaMI0408

piCorrigeGUT

**********************************************************************************/
FUNCTION fnCustoItem RETURN DEC (p-cod-estabel AS CHAR, p-it-codigo AS CHAR):
    /*Montando o custo previsto*/
    FIND LAST pr-it-per NO-LOCK
        WHERE pr-it-per.cod-estabel = p-cod-estabel 
          AND pr-it-per.it-codigo   = p-it-codigo      NO-ERROR.
    IF AVAIL pr-it-per THEN 
        RETURN pr-it-per.val-unit-mat-m[1] + pr-it-per.val-unit-mob-m[1] + pr-it-per.val-unit-ggf-m[1].
    ELSE DO:
        FIND FIRST item-uni-estab NO-LOCK
            WHERE item-uni-estab.cod-estabel = p-cod-estabel
              AND item-uni-estab.it-codigo   = p-it-codigo      NO-ERROR.
        IF AVAIL item-uni-estab THEN DO:
            IF item-uni-estab.preco-ul-ent <> 0 THEN
                RETURN item-uni-estab.preco-ul-ent.
            ELSE IF item-uni-estab.preco-repos <> 0 THEN 
                     RETURN item-uni-estab.preco-repos.
                 ELSE IF item-uni-estab.preco-base <> 0 THEN
                          RETURN item-uni-estab.preco-base.
                      ELSE
                          RETURN 1.
        END.
    END.
    RETURN 0.
END.

PROCEDURE piInicializa:
    DEF VAR i-sequencia AS INT  NO-UNDO.

    ASSIGN c-estado-om[1] = "N∆o Iniciada"
           c-estado-om[2] = "Iniciada"
           c-estado-om[3] = "Suspensa"
           c-estado-om[4] = "Terminada".

    ASSIGN c-estado[1] = "N∆o Iniciada"
           c-estado[2] = "Liberada"
           c-estado[3] = "Alocada"
           c-estado[4] = "Separada"
           c-estado[5] = "Requisitada"
           c-estado[6] = "Iniciada"
           c-estado[7] = "Finalizada"
           c-estado[8] = "Terminada".

    ASSIGN i-sequencia = 0.
    FOR EACH tipo-nivel NO-LOCK:
        ASSIGN i-sequencia = i-sequencia + 1.
        CREATE tt-estr-tag.
        ASSIGN tt-estr-tag.cod-tipo  = tipo-nivel.cod-tipo
               tt-estr-tag.sequencia = i-sequencia.
    END.

    RUN pi-param-espec. /*Ajusta por usu†rio as especilidades que ser∆o listadas por padrao*/
    
    RUN pi-param-depos.

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
END.

PROCEDURE pi-param-depos:
    DEF VAR i-depos     AS INT  NO-UNDO.
    DEF VAR c-depos     AS CHAR NO-UNDO.
    DEF VAR i-sequencia AS INT  NO-UNDO.

    EMPTY TEMP-TABLE tt-deposito.
    FIND FIRST ext-usuar-mi NO-LOCK
        WHERE ext-usuar-mi.cod_usuario = c-seg-usuario NO-ERROR.
    IF AVAIL ext-usuar-mi THEN DO:
        ASSIGN i-sequencia = 0.
        IF ext-usuar-mi.cod-depos-mi <> "" THEN DO:
            ASSIGN c-depos = ext-usuar-mi.cod-depos-mi.
            REPEAT i-depos = 1 TO NUM-ENTRIES(c-depos,";"):
                FIND FIRST tt-deposito NO-LOCK
                    WHERE tt-deposito.cod-depos = ENTRY(i-depos,c-depos,";") NO-ERROR.
                IF NOT AVAIL tt-deposito THEN DO:
                    
                    ASSIGN i-sequencia = i-sequencia + 1.

                    CREATE tt-deposito.
                    ASSIGN tt-deposito.cod-depos   = ENTRY(i-depos,c-depos,";")
                           tt-deposito.log-localiz = YES
                           tt-deposito.sequencia   = i-sequencia.
                END.
            END.
        END.

        IF ext-usuar-mi.deposito-saldo <> "" THEN DO:
            ASSIGN c-depos = ext-usuar-mi.deposito-saldo.
            REPEAT i-depos = 1 TO NUM-ENTRIES(c-depos,";"):
                FIND FIRST tt-deposito NO-LOCK
                    WHERE tt-deposito.cod-depos = ENTRY(i-depos,c-depos,";") NO-ERROR.
                IF NOT AVAIL tt-deposito THEN DO:
                    
                    ASSIGN i-sequencia = i-sequencia + 1.

                    CREATE tt-deposito.
                    ASSIGN tt-deposito.cod-depos   = ENTRY(i-depos,c-depos,";")
                           tt-deposito.log-localiz = NO
                           tt-deposito.sequencia   = i-sequencia.
                END.
            END.
        END.
    END.

    ASSIGN l-considera-dep = CAN-FIND(FIRST tt-deposito).

    RETURN "OK".
END.

PROCEDURE piAtualizaOrdens:
    DEF VAR i-busca          AS INT                          NO-UNDO.

    EMPTY TEMP-TABLE tt-ord-manut.
    EMPTY TEMP-TABLE tt-ord-taref.
    EMPTY TEMP-TABLE tt-tecn-mi.
    EMPTY TEMP-TABLE tt-ord-esp.
    EMPTY TEMP-TABLE tt-ord-fich-met.
    EMPTY TEMP-TABLE tt-reservas.
    EMPTY TEMP-TABLE tt-reservas-sdo.
    EMPTY TEMP-TABLE tt-almoxarifado.

    FIND FIRST ttSelecao NO-LOCK NO-ERROR.
    IF NOT AVAIL ttSelecao THEN CREATE ttSelecao.

    run utp/ut-acomp.p persistent set h-acomp.
    
    run pi-inicializar in h-acomp(input "Buscando Ordens").

    ASSIGN i-acomp        = 0
           i-busca        = 0.

    /*Numero de OM*/
    IF i-busca = 0 AND ttSelecao.nr-ord-produ-ini = ttSelecao.nr-ord-produ-fim THEN
        ASSIGN i-busca = 2.

    /*Terminada = no*/
    IF i-busca = 0 AND ttSelecao.log-estado-4 = NO THEN
        ASSIGN i-busca = 1.
   
    /*Terminada = yes*/
    IF i-busca = 0 AND ttSelecao.log-estado-4 = YES THEN DO:
        ASSIGN i-busca = 3.

        IF ttSelecao.dt-manut-ini = 01/01/1800 OR
           ttSelecao.dt-manut-ini = 12/31/9999 THEN DO:
            RUN utp/ut-msgs.p("show",17006,"Datas Manutená∆o Inv†lidas~~Para buscar as OMs (terminadas) deve-se especificar uma faixa de datas para ter uma melhor performance").
            RETURN "NOK".
        END.
    END.

    /*Cria os dados de tecnico*/
    RUN piCriaTecnico(1,"").

    /*OM em aberto*/
    IF i-busca = 1 THEN DO:
        FOR EACH ord-manut NO-LOCK USE-INDEX sit-om
            WHERE ord-manut.om-encerrada = NO
              AND ord-manut.cod-estabel >= ttSelecao.cod-estabel-ini
              AND ord-manut.cod-estabel <= ttSelecao.cod-estabel-fim:
            RUN piCriaOM(YES).
        END.
    END.
    
    /*OM pela OM*/
    IF i-busca = 2 THEN DO:
        FOR EACH ord-manut NO-LOCK USE-INDEX numero
            WHERE ord-manut.nr-ord-produ >= ttSelecao.nr-ord-produ-ini
              AND ord-manut.nr-ord-produ <= ttSelecao.nr-ord-produ-fim:
            RUN piCriaOM(YES).
        END.
    END.

    /*OM pela data manutená∆o*/
    IF i-busca = 3 THEN DO:
        FOR EACH ord-manut NO-LOCK USE-INDEX dt-om
            WHERE ord-manut.dt-manut    >= ttSelecao.dt-manut-ini
              AND ord-manut.dt-manut    <= ttSelecao.dt-manut-fim
              AND ord-manut.cod-estabel >= ttSelecao.cod-estabel-ini
              AND ord-manut.cod-estabel <= ttSelecao.cod-estabel-fim:
            RUN piCriaOM(YES).
        END.
    END.

    RUN piAtualizaTempo("","ZZZZZZZZ",NO).
    RUN piAjustaSeqTecnico.

    run pi-finalizar in h-acomp.
END.

PROCEDURE piCriaOMResumida:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    IF CAN-FIND(FIRST tt-ord-manut 
                WHERE tt-ord-manut.nr-ord-produ = ord-manut.nr-ord-produ) THEN RETURN "NOK".

    ASSIGN i-acomp = i-acomp + 1.
    IF i-acomp MODULO 100 = 0 AND VALID-HANDLE(h-acomp) THEN 
        RUN pi-acompanhar IN h-acomp (INPUT 'Buscando OM ' + STRING(i-acomp)).

    CREATE tt-ord-manut.
    BUFFER-COPY ord-manut TO tt-ord-manut.
    ASSIGN tt-ord-manut.estado-om   = ord-manut.estado-om
           tt-ord-manut.dt-termino  = ord-manut.dt-fecham
           tt-ord-manut.c-estado-om = c-estado-om[ord-manut.estado-om]
           tt-ord-manut.c-estado    = c-estado[ord-manut.estado].

    ASSIGN tt-ord-manut.data-abertura = DATE(SUBSTRING(ord-manut.char-1,1,10))
           tt-ord-manut.hora-abertura = STRING(INT(substring(ord-manut.char-1,11,6)),"HH:MM:SS") NO-ERROR.

    ASSIGN tt-ord-manut.usuario-abertura = ord-manut.usuario-alt.
    
    RUN piDetalhaMat(BUFFER tt-ord-manut,YES).

    RETURN "OK".

END PROCEDURE.

PROCEDURE piCriaOM:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-filtra AS LOG NO-UNDO.

    DEF VAR c-cd-equipe AS CHAR NO-UNDO.

    IF CAN-FIND(FIRST tt-ord-manut 
                WHERE tt-ord-manut.nr-ord-produ = ord-manut.nr-ord-produ) THEN RETURN "NOK".

    ASSIGN i-acomp = i-acomp + 1.
    IF i-acomp MODULO 100 = 0 AND VALID-HANDLE(h-acomp) THEN 
        RUN pi-acompanhar IN h-acomp (INPUT 'Buscando OM ' + STRING(i-acomp)).
    
    IF NOT AVAIL ttSelecao THEN
        FIND FIRST ttSelecao NO-LOCK NO-ERROR.

    IF p-filtra = YES THEN DO:
        IF ord-manut.cod-estabel    < ttSelecao.cod-estabel-ini    OR
           ord-manut.cod-estabel    > ttSelecao.cod-estabel-fim    OR
           ord-manut.cd-planejado   < ttSelecao.cod-planejad-ini   OR
           ord-manut.cd-planejado   > ttSelecao.cod-planejad-fim   THEN RETURN "OK".

        IF ord-manut.cd-equip-res   < ttSelecao.cd-equipe-res-ini  OR 
           ord-manut.cd-equip-res   > ttSelecao.cd-equipe-res-fim  OR 
           ord-manut.cd-tag         < ttSelecao.cd-tag-ini         OR 
           ord-manut.cd-tag         > ttSelecao.cd-tag-fim         OR 
           ord-manut.cd-equipto     < ttSelecao.cd-equipto-ini     OR 
           ord-manut.cd-equipto     > ttSelecao.cd-equipto-fim     OR 
           ord-manut.nr-ord-produ   < ttSelecao.nr-ord-produ-ini   OR 
           ord-manut.nr-ord-produ   > ttSelecao.nr-ord-produ-fim   THEN RETURN "OK". 

        IF ord-manut.dt-prev-manut  < ttSelecao.dt-manut-ini       OR
           ord-manut.dt-prev-manut  > ttSelecao.dt-manut-fim       OR
           ord-manut.cd-manut       < ttSelecao.cd-manut-ini       OR
           ord-manut.cd-manut       > ttSelecao.cd-manut-fim       OR
           ord-manut.tp-manut       < ttSelecao.tp-manut-ini       OR
           ord-manut.tp-manut       > ttSelecao.tp-manut-fim       OR
           ord-manut.prioridade     < ttSelecao.prioridade-ini     OR
           ord-manut.prioridade     > ttSelecao.prioridade-fim     THEN RETURN "OK".

        IF ord-manut.cd-parada      < ttSelecao.cd-parada-ini      OR        
           ord-manut.cd-parada      > ttSelecao.cd-parada-fim      OR        
           ord-manut.sequencia      < ttSelecao.sequencia-ini      OR        
           ord-manut.sequencia      > ttSelecao.sequencia-fim      THEN RETURN "OK".

        IF ord-manut.cd-projeto     < ttSelecao.cd-projeto-ini     OR
           ord-manut.cd-projeto     > ttSelecao.cd-projeto-fim     OR
           ord-manut.cod-unid-negoc < ttSelecao.cod-unid-negoc-ini OR
           ord-manut.cod-unid-negoc > ttSelecao.cod-unid-negoc-fim OR
           ord-manut.cd-sint-padr   < ttSelecao.cd-sint-padr-ini   OR
           ord-manut.cd-sint-padr   > ttSelecao.cd-sint-padr-fim   OR
           ord-manut.cd-causa-padr  < ttSelecao.cd-causa-padr-ini  OR
           ord-manut.cd-causa-padr  > ttSelecao.cd-causa-padr-fim  OR
           ord-manut.cd-interv-padr < ttSelecao.cd-interv-padr-ini OR
           ord-manut.cd-interv-padr > ttSelecao.cd-interv-padr-fim OR
           ord-manut.sc-codigo      < ttSelecao.ccusto-ini         OR
           ord-manut.sc-codigo      > ttSelecao.ccusto-fim         THEN RETURN "OK".

        IF ord-manut.fm-equipto     < ttSelecao.familia-ini        OR        
           ord-manut.fm-equipto     > ttSelecao.familia-fim        OR        
           ord-manut.cd-pend-padr   < ttSelecao.cd-pend-padr-ini   OR        
           ord-manut.cd-pend-padr   > ttSelecao.cd-pend-padr-fim   THEN RETURN "OK".

        IF ord-manut.estado-om = 1 AND ttSelecao.log-estado-1 = NO THEN RETURN "OK".
        IF ord-manut.estado-om = 2 AND ttSelecao.log-estado-2 = NO THEN RETURN "OK".
        IF ord-manut.estado-om = 3 AND ttSelecao.log-estado-3 = NO THEN RETURN "OK".
        IF ord-manut.estado-om = 4 AND ttSelecao.log-estado-4 = NO THEN RETURN "OK".

        IF ttSelecao.log-tecn-sem = NO THEN DO:
            IF NOT CAN-FIND(FIRST ext-ord-manut-tecn
                            WHERE ext-ord-manut-tecn.nr-ord-produ = ord-manut.nr-ord-produ) THEN RETURN "OK".
        END.

        IF ttSelecao.log-tecn-com = NO THEN DO:
            IF CAN-FIND(FIRST ext-ord-manut-tecn
                        WHERE ext-ord-manut-tecn.nr-ord-produ = ord-manut.nr-ord-produ) THEN RETURN "OK".
        END.

        IF (ord-manut.tipo = 1 AND ttSelecao.log-preventiva = NO) OR
           (ord-manut.tipo = 2 AND ttSelecao.log-corretiva  = NO) OR
           (ord-manut.tipo = 3 AND ttSelecao.log-preditiva  = NO) OR
           (ord-manut.tipo = 4 AND ttSelecao.log-outros     = NO) THEN RETURN "OK".

        IF ttSelecao.estado-ordem[ord-manut.estado] = NO THEN RETURN "OK".
    END.

    CREATE tt-ord-manut.
    BUFFER-COPY ord-manut TO tt-ord-manut.
    ASSIGN tt-ord-manut.estado-om   = ord-manut.estado-om
           tt-ord-manut.dt-termino  = ord-manut.dt-fecham
           tt-ord-manut.c-estado-om = c-estado-om[ord-manut.estado-om]
           tt-ord-manut.c-estado    = c-estado[ord-manut.estado].

    ASSIGN tt-ord-manut.data-abertura = DATE(SUBSTRING(ord-manut.char-1,1,10))
           tt-ord-manut.hora-abertura = STRING(INT(substring(ord-manut.char-1,11,6)),"HH:MM:SS") NO-ERROR.

    ASSIGN tt-ord-manut.usuario-abertura = ord-manut.usuario-alt.
    IF tt-ord-manut.usuario-abertura <> "" THEN DO:
        FIND FIRST usuar_mestre NO-LOCK
            WHERE usuar_mestre.cod_usuar = tt-ord-manut.usuario-abertura NO-ERROR.
        IF AVAIL usuar_mestre THEN
            ASSIGN tt-ord-manut.nom-usuar-abertura = usuar_mestre.nom_usuar.
    END.

    FIND FIRST mnt-planejador NO-LOCK
        WHERE mnt-planejador.cd-planejad = tt-ord-manut.cd-planejad NO-ERROR.
    IF AVAIL mnt-planejador THEN
        ASSIGN tt-ord-manut.nome-planejad = mnt-planejador.nome.

    FIND FIRST equipto NO-LOCK
        WHERE equipto.cd-equipto = tt-ord-manut.cd-equipto NO-ERROR.
    IF AVAIL equipto THEN
        ASSIGN tt-ord-manut.desc-equipto = equipto.descricao
               tt-ord-manut.fm-equipto   = equipto.fm-equipto.
    
    FIND FIRST equipe NO-LOCK
        WHERE equipe.cd-equipe = tt-ord-manut.cd-equip-res NO-ERROR.
    IF AVAIL equipe THEN
        ASSIGN tt-ord-manut.desc-equipe  = equipe.desc-equipe.
    
    ASSIGN c-cd-equipe = tt-ord-manut.cd-equip-res.
    FIND FIRST ext-ord-manut-tecn NO-LOCK
        WHERE ext-ord-manut-tecn.nr-ord-produ  = tt-ord-manut.nr-ord-produ
          AND ext-ord-manut-tecn.log-principal = YES                      NO-ERROR.
    IF AVAIL ext-ord-manut-tecn THEN DO:
        FIND FIRST tecn-mi NO-LOCK
            WHERE tecn-mi.cd-tecnico = ext-ord-manut-tecn.cd-tecnico NO-ERROR.
        ASSIGN tt-ord-manut.cd-tecnico = tecn-mi.cd-tecnico
               tt-ord-manut.nome-compl = tecn-mi.nome-compl.
        ASSIGN c-cd-equipe = tecn-mi.cd-equipe.
    END.
    ELSE DO:
        ASSIGN tt-ord-manut.cd-tecnico = ""
               tt-ord-manut.nome-compl = "".
    END.

    RUN piRetornaFlex(OUTPUT tt-ord-manut.flexib-prorr).

    IF tt-ord-manut.estado-om < 4 THEN DO:
        IF tt-ord-manut.dt-prev-manut + tt-ord-manut.flexib-prorr > TODAY THEN 
            ASSIGN tt-ord-manut.dias-atraso = (tt-ord-manut.dt-prev-manut + tt-ord-manut.flexib-prorr) - TODAY.
    END.


    FIND FIRST pol-param-relatorio NO-LOCK USE-INDEX idx_param_rel_3
        WHERE pol-param-relatorio.ano-refer      = ord-manut.nr-ord-prod 
          AND pol-param-relatorio.cod-prog-dtsul = "MI0307"   no-error.
    IF AVAIL pol-param-relatorio THEN DO:
        ASSIGN tt-ord-manut.usuar-encerramento = pol-param-relatorio.cod-usuario.
        IF tt-ord-manut.usuar-encerramento <> "" THEN DO:
            FIND FIRST usuar_mestre NO-LOCK
                WHERE usuar_mestre.cod_usuar = tt-ord-manut.usuar-encerramento NO-ERROR.
            IF AVAIL usuar_mestre THEN 
                ASSIGN tt-ord-manut.nome-usuar-enc = usuar_mestre.nom_usuario.

            IF pol-param-relatorio.data-1 <> ? THEN 
                ASSIGN tt-ord-manut.dt-fecham = pol-param-relatorio.data-1
                       tt-ord-manut.hr-fecham = STRING(INT(pol-param-relatorio.int-1),"HH:MM:SS").
        END.
    END.

    FIND FIRST ext-gut NO-LOCK
        WHERE ext-gut.nr-ord-produ = ord-manut.nr-ord-produ 
          AND ext-gut.cd-equipto   = ""
          AND ext-gut.cd-manut     = ""                    NO-ERROR.
    IF AVAIL ext-gut THEN
        ASSIGN tt-ord-manut.gut-gravidade = ext-gut.gravidade 
               tt-ord-manut.gut-urgencia  = ext-gut.urgencia  
               tt-ord-manut.gut-tendencia = ext-gut.tendencia
               tt-ord-manut.seq-exec      = ext-gut.seq-exec.
    ELSE
        ASSIGN tt-ord-manut.gut-gravidade = ?
               tt-ord-manut.gut-urgencia  = ?
               tt-ord-manut.gut-tendencia = ?
               tt-ord-manut.seq-exec      = ?.

    
    IF tt-ord-manut.cd-pend-padr <> "" THEN DO:
        FIND FIRST pend-padrao NO-LOCK
            WHERE pend-padrao.cd-pend-padr = tt-ord-manut.cd-pend-padr NO-ERROR.
        IF AVAIL pend-padrao THEN 
            ASSIGN tt-ord-manut.desc-pend = pend-padrao.descricao.
    END.

    RUN piDetalhaNec(BUFFER tt-ord-manut).

    RETURN "OK".

END PROCEDURE.

PROCEDURE piDetalhaNec:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define parameter buffer b-tt-ord-manut for tt-ord-manut.

    DEF VAR i-cd-tarefa         AS INT                NO-UNDO.
    DEF VAR l-taref             AS LOG                NO-UNDO.
    DEF VAR i-cst               AS INT                NO-UNDO.
    DEF VAR de-valor            AS DEC DECIMALS 4     NO-UNDO.
    DEF VAR l-log-atua-mat-cst  AS LOG                NO-UNDO.

    ASSIGN b-tt-ord-manut.nec-tecn     = ""
           b-tt-ord-manut.log-nec-tecn = NO.

    FOR EACH tt-ord-taref
        WHERE tt-ord-taref.nr-ord-produ = b-tt-ord-manut.nr-ord-produ:
        DELETE tt-ord-taref.
    END.

    FOR EACH tt-ord-esp NO-LOCK
        WHERE tt-ord-esp.nr-ord-produ = b-tt-ord-manut.nr-ord-produ:
        IF tt-ord-esp.encerrada = NO THEN DO:
            /**********************************************************
            Acumulador Por Equipe
            **********************************************************/
            FIND FIRST tt-tecn-mi
                WHERE tt-tecn-mi.tipo          = 1
                  AND tt-tecn-mi.cd-equipe-res = b-tt-ord-manut.cd-equip-res NO-ERROR.
            IF AVAIL tt-tecn-mi THEN 
                ASSIGN tt-tecn-mi.backlog = tt-tecn-mi.backlog - MAX(0,(tt-ord-esp.tempo-tot - tt-ord-esp.tempo-aloc)).
            /**********************************************************
            Acumulador Total
            **********************************************************/
            FIND FIRST tt-tecn-mi
                WHERE tt-tecn-mi.tipo          = 3
                  AND tt-tecn-mi.cd-equipe-res = ""
                  AND tt-tecn-mi.cd-tecnico    = "" NO-ERROR.
            IF AVAIL tt-tecn-mi THEN 
                ASSIGN tt-tecn-mi.backlog = tt-tecn-mi.backlog - MAX(0,(tt-ord-esp.tempo-tot - tt-ord-esp.tempo-aloc)).
        END.

        DELETE tt-ord-esp.
    END.

    ASSIGN l-log-atua-mat-cst = NO.
    IF b-tt-ord-manut.log-atua-mat-cst = YES THEN
        ASSIGN b-tt-ord-manut.log-atua-mat-cst = NO
               b-tt-ord-manut.tempo-tot-tar    = 0
               b-tt-ord-manut.tempo-realiz-tar = 0
               b-tt-ord-manut.custo-planejado  = 0
               b-tt-ord-manut.custo-alocado    = 0
               l-log-atua-mat-cst              = YES.

    /*
    tt-ord-esp.tempo      - Tempo da Tarefa
    tt-ord-esp.tempo-tot  - Tempo da Tarefa * Nr Homens (Caso o tempo seja individual)
    
    tt-ord-esp.tempo-rep  - Tempo da Tarefa Real
    
    tt-ord-taref.backlog  - Tempo da Tarefa que n∆o foi finalizada (Somatoria tt-ord-esp.tempo-tot apenas de tarefas n∆o encerrada)
    
    b-tt-ord-manut.tempo-tot-tar - Tempo total da tarefa (Somatoria tt-ord-esp.tempo-tot)
    */

    FOR EACH ord-taref NO-LOCK
        WHERE ord-taref.nr-ord-produ = b-tt-ord-manut.nr-ord-produ:

        IF l-log-atua-mat-cst THEN
            ASSIGN b-tt-ord-manut.tempo-realiz-tar = b-tt-ord-manut.tempo-realiz-tar  + ord-taref.tempo-real.

        CREATE tt-ord-taref.
        BUFFER-COPY b-tt-ord-manut TO tt-ord-taref
            ASSIGN tt-ord-taref.cd-tarefa  = ord-taref.cd-tarefa
                   tt-ord-taref.des-tarefa = ord-taref.descricao.

        IF CAN-FIND(FIRST ord-esp NO-LOCK
                    WHERE ord-esp.nr-ord-produ = ord-taref.nr-ord-produ
                      AND ord-esp.cd-tarefa    = ord-taref.cd-tarefa) THEN DO:
            FOR EACH ord-esp NO-LOCK
                WHERE ord-esp.nr-ord-produ = b-tt-ord-manut.nr-ord-produ
                  AND ord-esp.cd-tarefa    = ord-taref.cd-tarefa:

                CREATE tt-ord-esp.
                ASSIGN tt-ord-esp.nr-ord-produ = ord-esp.nr-ord-produ
                       tt-ord-esp.cd-tarefa    = ord-esp.cd-tarefa   
                       tt-ord-esp.tp-especial  = ord-esp.tp-especial 
                       tt-ord-esp.nr-homens    = ord-esp.nr-homens   
                       tt-ord-esp.tipo-tempo   = ord-esp.tipo-tempo
                       tt-ord-esp.tempo        = ord-esp.tempo    
                       tt-ord-esp.tempo-tot    = ord-esp.tempo    
                       tt-ord-esp.tempo-rep    = ord-esp.tempo-rep
                       tt-ord-esp.encerrada    = ord-esp.encerrada.
                
                /**************************************************************************************************
                Tipo Tempo - Informar tipo de tempo da especialidade, as opá‰es dispon°veis s∆o:
                      Individual: significa que o tempo informado Ç para um homem.
                      Total: significa que o tempo representa o total de horas, considerando todos os homens.
                ***************************************************************************************************/
                IF ord-esp.tipo-tempo = 1 THEN
                    ASSIGN tt-ord-esp.tempo-tot = tt-ord-esp.tempo * tt-ord-esp.nr-homens.

                IF l-log-atua-mat-cst THEN DO:
                    /*Custo Previsto*/
                    FIND FIRST mi-espec NO-LOCK
                        WHERE mi-espec.tp-especial = ord-esp.tp-especial NO-ERROR.
                    IF AVAIL mi-espec THEN DO:
                        IF mi-espec.custo = 2 /*Valor Informado*/ THEN DO:
                            ASSIGN b-tt-ord-manut.custo-planejado = b-tt-ord-manut.custo-planejado + (mi-espec.valor-hora * tt-ord-esp.tempo-tot).
                        END.
                        ELSE DO:
                            FIND LAST ext-per-custo NO-LOCK
                                WHERE ext-per-custo.cc-codigo = mi-espec.cc-codigo
                                  AND ext-per-custo.mo-codigo = 0                  NO-ERROR.
                            IF AVAIL ext-per-custo THEN DO:
                                REPEAT i-cst = 1 TO 6:
                                    ASSIGN b-tt-ord-manut.custo-planejado = b-tt-ord-manut.custo-planejado + (ext-per-custo.custo-prev[i-cst] * tt-ord-esp.tempo-tot).
                                END.
                            END.

                            IF mi-espec.cd-mob-dir <> "" THEN DO:
                                FIND FIRST tab-mob-dir NO-LOCK
                                    WHERE tab-mob-dir.cd-mob-dir = mi-espec.cd-mob-dir NO-ERROR.
                                IF AVAIL tab-mob-dir THEN DO:
                                    ASSIGN b-tt-ord-manut.custo-planejado = b-tt-ord-manut.custo-planejado + (tab-mob-dir.vl-orcado[1] * tt-ord-esp.tempo-tot).
                                END.
                            END.
                        END.
                    END.

                    ASSIGN b-tt-ord-manut.tempo-tot-tar = b-tt-ord-manut.tempo-tot-tar  + tt-ord-esp.tempo-tot.
                END.

                IF ord-taref.encerrada = NO THEN 
                    ASSIGN tt-ord-taref.backlog = tt-ord-taref.backlog + tt-ord-esp.tempo-tot.
            END.
        END.
        ELSE DO:
            CREATE tt-ord-esp.
            ASSIGN tt-ord-esp.nr-ord-produ = ord-taref.nr-ord-produ
                   tt-ord-esp.cd-tarefa    = ord-taref.cd-tarefa   
                   tt-ord-esp.tp-especial  = ""
                   tt-ord-esp.nr-homens    = 1
                   tt-ord-esp.tipo-tempo   = 1
                   tt-ord-esp.tempo        = ord-taref.tempo       
                   tt-ord-esp.tempo-tot    = ord-taref.tempo
                   tt-ord-esp.tempo-rep    = ord-taref.tempo-real
                   tt-ord-esp.encerrada    = ord-taref.encerrada.

            IF l-log-atua-mat-cst THEN
                ASSIGN b-tt-ord-manut.tempo-tot-tar = b-tt-ord-manut.tempo-tot-tar  + tt-ord-esp.tempo-tot.
            
            IF ord-taref.encerrada = NO THEN 
                ASSIGN tt-ord-taref.backlog = tt-ord-taref.backlog + tt-ord-esp.tempo-tot.
        END.
    END.
    
    IF NOT CAN-FIND(FIRST tt-ord-taref
                    WHERE tt-ord-taref.nr-ord-produ = b-tt-ord-manut.nr-ord-produ) THEN DO:
        CREATE tt-ord-taref.
        BUFFER-COPY b-tt-ord-manut TO tt-ord-taref.
    END.

    FOR EACH ext-ord-manut-tecn NO-LOCK
        WHERE ext-ord-manut-tecn.nr-ord-produ = b-tt-ord-manut.nr-ord-produ:
        FIND FIRST tt-ord-esp
            WHERE tt-ord-esp.nr-ord-produ = ext-ord-manut-tecn.nr-ord-produ 
              AND tt-ord-esp.cd-tarefa    = ext-ord-manut-tecn.cd-tarefa    
              AND tt-ord-esp.tp-especial  = ext-ord-manut-tecn.tp-especial   NO-ERROR.
        IF NOT AVAIL tt-ord-esp THEN DO:
            CREATE tt-ord-esp.
            ASSIGN tt-ord-esp.nr-ord-produ = ext-ord-manut-tecn.nr-ord-produ
                   tt-ord-esp.cd-tarefa    = ext-ord-manut-tecn.cd-tarefa   
                   tt-ord-esp.tp-especial  = ext-ord-manut-tecn.tp-especial.
        END.
        ASSIGN tt-ord-esp.nr-hom-aloc = tt-ord-esp.nr-hom-aloc + 1
               tt-ord-esp.tempo-aloc  = tt-ord-esp.tempo-aloc  + ext-ord-manut-tecn.tempo.
        
        FIND FIRST tt-ord-taref 
            WHERE tt-ord-taref.nr-ord-produ = ext-ord-manut-tecn.nr-ord-produ
              AND tt-ord-taref.cd-tarefa    = ext-ord-manut-tecn.cd-tarefa    NO-ERROR.
        IF AVAIL tt-ord-taref THEN
            ASSIGN tt-ord-taref.tempo = tt-ord-taref.tempo + tt-ord-esp.tempo.
    END.
    
    ASSIGN i-cd-tarefa = 0.
    FOR EACH tt-ord-esp
        WHERE tt-ord-esp.nr-ord-produ = b-tt-ord-manut.nr-ord-produ
          AND tt-ord-esp.encerrada    = NO:

        IF tt-ord-esp.nr-hom-aloc >= tt-ord-esp.nr-homens AND 
           tt-ord-esp.tempo-aloc  >= tt-ord-esp.tempo     THEN NEXT.
        
        IF i-cd-tarefa <> tt-ord-esp.cd-tarefa THEN DO:
            IF b-tt-ord-manut.nec-tecn <> "" THEN
                ASSIGN b-tt-ord-manut.nec-tecn = b-tt-ord-manut.nec-tecn + ")|".

            ASSIGN i-cd-tarefa = tt-ord-esp.cd-tarefa.

            ASSIGN b-tt-ord-manut.nec-tecn = b-tt-ord-manut.nec-tecn + "T:" + STRING(i-cd-tarefa) + " (".

            ASSIGN b-tt-ord-manut.nec-tecn = b-tt-ord-manut.nec-tecn + tt-ord-esp.tp-especial + ":" + STRING(tt-ord-esp.nr-homens - tt-ord-esp.nr-hom-aloc) + "-" + STRING(tt-ord-esp.tempo-tot - tt-ord-esp.tempo-aloc) + "h".
        END.
        ELSE
            ASSIGN b-tt-ord-manut.nec-tecn = b-tt-ord-manut.nec-tecn + "," + tt-ord-esp.tp-especial + ":" + STRING(tt-ord-esp.nr-homens - tt-ord-esp.nr-hom-aloc) + "-" + STRING(tt-ord-esp.tempo-tot - tt-ord-esp.tempo-aloc) + "h".

        /**********************************************************
        Acumulador Por Equipe
        **********************************************************/
        FIND FIRST tt-tecn-mi
            WHERE tt-tecn-mi.tipo          = 1
              AND tt-tecn-mi.cd-equipe-res = b-tt-ord-manut.cd-equip-res NO-ERROR.
        IF AVAIL tt-tecn-mi THEN 
            ASSIGN tt-tecn-mi.backlog = tt-tecn-mi.backlog + MAX(0,(tt-ord-esp.tempo-tot - tt-ord-esp.tempo-aloc)).
        /**********************************************************
        Acumulador Total
        **********************************************************/
        FIND FIRST tt-tecn-mi
            WHERE tt-tecn-mi.tipo          = 3
              AND tt-tecn-mi.cd-equipe-res = ""
              AND tt-tecn-mi.cd-tecnico    = "" NO-ERROR.
        IF AVAIL tt-tecn-mi THEN 
            ASSIGN tt-tecn-mi.backlog = tt-tecn-mi.backlog + MAX(0,(tt-ord-esp.tempo-tot - tt-ord-esp.tempo-aloc)).

    END.

    IF b-tt-ord-manut.nec-tecn <> "" THEN
        ASSIGN b-tt-ord-manut.nec-tecn     = b-tt-ord-manut.nec-tecn + ")"
               b-tt-ord-manut.log-nec-tecn = YES.

    RUN piDetalhaMat(BUFFER b-tt-ord-manut,l-log-atua-mat-cst).

END PROCEDURE.

PROCEDURE piDetalhaMat:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define parameter buffer b-tt-ord-manut for tt-ord-manut.
    DEF INPUT PARAMETER p-l-log-atua-mat-cst AS LOG NO-UNDO.

    IF p-l-log-atua-mat-cst THEN DO:
        FOR EACH reservas NO-LOCK
            WHERE reservas.nr-ord-produ = b-tt-ord-manut.nr-ord-produ:
            
            FIND FIRST ITEM NO-LOCK
                WHERE ITEM.it-codigo = reservas.it-codigo NO-ERROR.
            IF NOT AVAIL ITEM THEN NEXT.

            FIND FIRST tt-reservas
                WHERE tt-reservas.nr-ord-produ = reservas.nr-ord-produ
                  AND tt-reservas.cd-tarefa    = reservas.op-codigo
                  AND tt-reservas.it-codigo    = reservas.it-codigo     NO-ERROR.
            IF NOT AVAIL tt-reservas THEN DO:
                CREATE tt-reservas.
                ASSIGN tt-reservas.nr-ord-produ = reservas.nr-ord-produ
                       tt-reservas.cd-tarefa    = reservas.op-codigo   
                       tt-reservas.it-codigo    = reservas.it-codigo
                       tt-reservas.desc-item    = ITEM.desc-item
                       tt-reservas.un           = ITEM.un.

                ASSIGN tt-reservas.valor-unit = fnCustoItem(b-tt-ord-manut.cod-estabel,reservas.it-codigo).
            END.

            ASSIGN tt-reservas.quant-orig = tt-reservas.quant-orig + reservas.quant-orig
                   tt-reservas.quant-aloc = tt-reservas.quant-aloc + reservas.quant-aloc
                   tt-reservas.valor-orig = tt-reservas.quant-orig * tt-reservas.valor-unit
                   tt-reservas.valor-aloc = tt-reservas.quant-aloc * tt-reservas.valor-unit.
        END.
        
        ASSIGN tt-ord-manut.custo-realizado = 0.
        
        FOR EACH movto-estoq NO-LOCK
            WHERE movto-estoq.nr-ord-produ = b-tt-ord-manut.nr-ord-produ:

            IF movto-estoq.esp-docto    = 1 THEN 
                ASSIGN tt-ord-manut.custo-realizado = tt-ord-manut.custo-realizado + (movto-estoq.valor-mat-m[1] + 
                                                                                      movto-estoq.valor-mob-m[1] +
                                                                                      movto-estoq.valor-ggf-m[1]).
            IF movto-estoq.esp-docto    = 8 THEN
                ASSIGN tt-ord-manut.custo-realizado = tt-ord-manut.custo-realizado - (movto-estoq.valor-mat-m[1] + 
                                                                                      movto-estoq.valor-mob-m[1] +
                                                                                      movto-estoq.valor-ggf-m[1]).

            IF movto-estoq.esp-docto = 1 OR movto-estoq.esp-docto = 8 THEN NEXT.
            
            FIND FIRST tt-reservas
                WHERE tt-reservas.nr-ord-produ = movto-estoq.nr-ord-produ
                  AND tt-reservas.cd-tarefa    = movto-estoq.op-codigo
                  AND tt-reservas.it-codigo    = movto-estoq.it-codigo     NO-ERROR.
            IF NOT AVAIL tt-reservas THEN DO:

                FIND FIRST ITEM NO-LOCK
                    WHERE ITEM.it-codigo = tt-reservas.it-codigo NO-ERROR.
                IF NOT AVAIL ITEM THEN NEXT.

                CREATE tt-reservas.
                ASSIGN tt-reservas.nr-ord-produ = movto-estoq.nr-ord-produ
                       tt-reservas.cd-tarefa    = movto-estoq.op-codigo   
                       tt-reservas.it-codigo    = movto-estoq.it-codigo
                       tt-reservas.desc-item    = ITEM.desc-item
                       tt-reservas.un           = ITEM.un.

                ASSIGN tt-reservas.valor-unit = fnCustoItem(b-tt-ord-manut.cod-estabel,reservas.it-codigo).
            END.
            IF movto-estoq.tipo-trans = 2 THEN
                ASSIGN tt-reservas.quant-requis = tt-reservas.quant-requis + movto-estoq.quantidade.
            ELSE
                ASSIGN tt-reservas.quant-requis = tt-reservas.quant-requis - movto-estoq.quantidade.

            ASSIGN tt-reservas.valor-requis = tt-reservas.quant-requis * tt-reservas.valor-unit.
        END.

        ASSIGN b-tt-ord-manut.materiais        = ""
               b-tt-ord-manut.materiais-aloc   = ""
               b-tt-ord-manut.materiais-realiz = "".

        FOR EACH tt-reservas NO-LOCK
            WHERE tt-reservas.nr-ord-produ = b-tt-ord-manut.nr-ord-produ:

            FIND FIRST ITEM NO-LOCK
                WHERE ITEM.it-codigo = tt-reservas.it-codigo NO-ERROR.

            ASSIGN b-tt-ord-manut.custo-planejado = b-tt-ord-manut.custo-planejado + tt-reservas.valor-orig
                   b-tt-ord-manut.custo-alocado   = b-tt-ord-manut.custo-alocado   + tt-reservas.valor-aloc.
            
            ASSIGN tt-reservas.localizacoes    = ""
                   tt-reservas.qtidade-estoque = 0.

            IF tt-reservas.quant-orig > 0 THEN 
                ASSIGN b-tt-ord-manut.materiais = b-tt-ord-manut.materiais + 
                                                  (IF b-tt-ord-manut.materiais = "" THEN "" ELSE "|") + 
                                                  STRING(tt-reservas.it-codigo,"x(16)") + " / " + STRING(ITEM.desc-item,"x(60)") + " / " + STRING(tt-reservas.quant-orig,"->>>,>>>,>>9.9999").

            IF tt-reservas.quant-aloc > 0 THEN 
                ASSIGN b-tt-ord-manut.materiais-aloc = b-tt-ord-manut.materiais-aloc + 
                                                      (IF b-tt-ord-manut.materiais-aloc = "" THEN "" ELSE "|") + 
                                                       STRING(tt-reservas.it-codigo,"x(16)") + " / " + STRING(ITEM.desc-item,"x(60)") + " / " + STRING(tt-reservas.quant-aloc,"->>>,>>>,>>9.9999").

            IF tt-reservas.quant-requis > 0 THEN 
                ASSIGN b-tt-ord-manut.materiais-realiz = b-tt-ord-manut.materiais-realiz + 
                                                      (IF b-tt-ord-manut.materiais-realiz = "" THEN "" ELSE "|") + 
                                                       STRING(tt-reservas.it-codigo,"x(16)") + " / " + STRING(ITEM.desc-item,"x(60)") + " / " + STRING(tt-reservas.quant-requis,"->>>,>>>,>>9.9999").

            FOR EACH saldo-estoq NO-LOCK
                WHERE saldo-estoq.cod-estabel  = b-tt-ord-manut.cod-estabel
                  AND saldo-estoq.it-codigo    = tt-reservas.it-codigo
                  AND saldo-estoq.qtidade-atu <> 0:

                IF l-considera-dep AND NOT CAN-FIND(FIRST tt-deposito
                                                    WHERE tt-deposito.cod-depos = saldo-estoq.cod-depos) THEN NEXT.
                
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

                CREATE tt-reservas-sdo.
                ASSIGN tt-reservas-sdo.r-rowid-reserva  = ROWID(tt-reservas)
                       tt-reservas-sdo.cod-depos        = saldo-estoq.cod-depos
                       tt-reservas-sdo.cod-localiz      = saldo-estoq.cod-localiz
                       tt-reservas-sdo.qtidade-disp     = saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada + saldo-estoq.qt-aloc-ped + saldo-estoq.qt-aloc-prod).
            END.     
        END.
    END.
END PROCEDURE.

PROCEDURE piDetalhaOM:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR i-tecn    AS INT  NO-UNDO.
    DEF VAR c-tag-aux AS CHAR NO-UNDO.

    run utp/ut-acomp.p persistent set h-acomp.
    
    run pi-inicializar in h-acomp(input "Detalha OM").

    ASSIGN i-acomp = 0.

    FOR EACH tt-ord-manut 
        WHERE tt-ord-manut.tipo-registro = 1
        BREAK BY tt-ord-manut.cd-tag:
        
        ASSIGN tt-ord-manut.tipo-registro = 2.
        
        ASSIGN i-acomp = i-acomp + 1.
        IF i-acomp MODULO 100 = 0 AND VALID-HANDLE(h-acomp) THEN 
            RUN pi-acompanhar IN h-acomp (INPUT 'Completando Dados ' + STRING(i-acomp)).

        ASSIGN tt-ord-manut.ficha-metodo = "".
        FOR EACH ord-fich-met NO-LOCK
            WHERE ord-fich-met.nr-ord-produ = tt-ord-manut.nr-ord-produ:

            CREATE tt-ord-fich-met.
            BUFFER-COPY ord-fich-met TO tt-ord-fich-met.

            FIND FIRST mnt-ficha-metodo NO-LOCK
                WHERE mnt-ficha-metodo.fi-codigo = tt-ord-fich-met.fi-codigo NO-ERROR.
            
            IF AVAIL mnt-ficha-metodo THEN DO:

                IF tt-ord-manut.ficha-metodo <> "" THEN
                    ASSIGN tt-ord-manut.ficha-metodo = tt-ord-manut.ficha-metodo + "|".

                ASSIGN tt-ord-manut.ficha-metodo = tt-ord-manut.ficha-metodo + string(ord-fich-met.fi-codigo,"99999") + " - " + string(mnt-ficha-metodo.fi-complemento,"x(10)") + " / " + string(mnt-ficha-metodo.descricao,"x(30)").

                ASSIGN tt-ord-fich-met.fi-complemento = mnt-ficha-metodo.fi-complemento  
                       tt-ord-fich-met.descricao      = mnt-ficha-metodo.descricao       
                       tt-ord-fich-met.responsavel    = mnt-ficha-metodo.responsavel     
                       tt-ord-fich-met.narrativa      = mnt-ficha-metodo.narrativa.

                ASSIGN tt-ord-fich-met.narrativa      = REPLACE(tt-ord-fich-met.narrativa,CHR(13),"|")
                       tt-ord-fich-met.narrativa      = REPLACE(tt-ord-fich-met.narrativa,CHR(10),"|")
                       tt-ord-fich-met.narrativa      = REPLACE(tt-ord-fich-met.narrativa,"ñ","|").
            END.
        END.

        FIND FIRST tag NO-LOCK
            WHERE tag.cd-tag = tt-ord-manut.cd-tag NO-ERROR.
        IF AVAIL tag THEN
            ASSIGN tt-ord-manut.desc-tag     = tag.descricao.

        FIND FIRST msg-ord-man no-lock
            WHERE msg-ord-man.nr-ord-produ = tt-ord-manut.nr-ord-produ NO-ERROR. 
        IF AVAIL msg-ord-man THEN
            ASSIGN tt-ord-manut.narrativa    = msg-ord-man.msg-exp
                   tt-ord-manut.narrativa    = REPLACE(tt-ord-manut.narrativa,CHR(13),"|")
                   tt-ord-manut.narrativa    = REPLACE(tt-ord-manut.narrativa,CHR(10),"|")
                   tt-ord-manut.narrativa    = REPLACE(tt-ord-manut.narrativa,"ñ","|").

        ASSIGN i-tecn = 0.
        FOR EACH ext-ord-manut-tecn NO-LOCK
            WHERE ext-ord-manut-tecn.nr-ord-produ = tt-ord-manut.nr-ord-produ:

            FIND FIRST tecn-mi NO-LOCK
                WHERE tecn-mi.cd-tecnico = ext-ord-manut-tecn.cd-tecnico NO-ERROR.

            IF ext-ord-manut-tecn.log-principal = YES THEN
                ASSIGN tt-ord-manut.cd-tecnico = tecn-mi.cd-tecnico
                       tt-ord-manut.nome-compl = tecn-mi.nome-compl.
            ELSE DO:
                ASSIGN i-tecn = i-tecn + 1.

                IF i-tecn <= 10 THEN
                    ASSIGN tt-ord-manut.cd-tecnico-aux[i-tecn] = tecn-mi.cd-tecnico        
                           tt-ord-manut.nome-compl-aux[i-tecn] = tecn-mi.nome-compl.       
            END.
        END.

        FIND FIRST mi-per-parada NO-LOCK
            WHERE mi-per-parada.cd-parada = tt-ord-manut.cd-parada
              AND mi-per-parada.sequencia = tt-ord-manut.sequencia NO-ERROR.
        IF AVAIL mi-per-parada THEN 
            ASSIGN tt-ord-manut.dt-ini-parada = mi-per-parada.dt-inicio
                   tt-ord-manut.dt-fim-parada = mi-per-parada.dt-termino.
        ELSE
            ASSIGN tt-ord-manut.dt-ini-parada = ?
                   tt-ord-manut.dt-fim-parada = ?.

        IF tt-ord-manut.cd-sint-padr <> "" THEN DO:
            FIND FIRST sint-padrao NO-LOCK
                WHERE sint-padrao.cd-sint-padr = tt-ord-manut.cd-sint-padr NO-ERROR.
            IF AVAIL sint-padrao THEN
                ASSIGN tt-ord-manut.desc-sint = sint-padrao.descricao.
        END.

        IF tt-ord-manut.cd-causa-padr <> "" THEN DO:
            FIND FIRST causa-padrao NO-LOCK
                WHERE causa-padrao.cd-causa-padr = tt-ord-manut.cd-causa-padr NO-ERROR.
            IF AVAIL causa-padrao THEN
                ASSIGN tt-ord-manut.desc-causa = causa-padrao.descricao.
        END.

        IF tt-ord-manut.cd-interv-padr <> "" THEN DO:
            FIND FIRST interv-padrao NO-LOCK
                WHERE interv-padrao.cd-interv-padr = tt-ord-manut.cd-interv-padr NO-ERROR.
            IF AVAIL interv-padrao THEN
                ASSIGN tt-ord-manut.desc-interv = interv-padrao.descricao.
        END.

        FIND FIRST fam-equipto NO-LOCK
            WHERE fam-equipto.fm-equipto = tt-ord-manut.fm-equipto NO-ERROR.
        IF AVAIL fam-equipto THEN
            ASSIGN tt-ord-manut.descricao-fm = fam-equipto.descricao.
        
        IF FIRST-OF(tt-ord-manut.cd-tag) THEN DO:
            FOR EACH tt-estr-tag:
                ASSIGN tt-estr-tag.cd-tag    = ""
                       tt-estr-tag.descricao = "".
            END.

            ASSIGN c-tag-aux = tt-ord-manut.cd-tag.
            b-estrutura:
            REPEAT:

                FIND FIRST estr-tag NO-LOCK
                    WHERE estr-tag.tag-filho = c-tag-aux NO-ERROR.
                IF NOT AVAIL estr-tag THEN LEAVE b-estrutura.

                FIND tag NO-LOCK
                    WHERE tag.cd-tag = estr-tag.tag-pai NO-ERROR.
                FIND FIRST tt-estr-tag
                    WHERE tt-estr-tag.cod-tipo = tag.cod-tipo NO-ERROR.
                ASSIGN tt-estr-tag.cd-tag    = tag.cd-tag
                       tt-estr-tag.descricao = tag.descricao.

                ASSIGN c-tag-aux = estr-tag.tag-pai.
            END.
        END.

        FOR EACH tt-estr-tag:
            IF tt-estr-tag.sequencia > 10 THEN NEXT.
            ASSIGN tt-ord-manut.estrutura-tag[tt-estr-tag.sequencia] = tt-estr-tag.descricao.
        END.

        FOR EACH tt-reservas 
            WHERE tt-reservas.nr-ord-produ = tt-ord-manut.nr-ord-produ:
            BUFFER-COPY tt-ord-manut TO tt-reservas.
        END.

    END.

    run pi-finalizar in h-acomp. 

END PROCEDURE.

PROCEDURE piAtualizaTempo:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-cd-tecnico-ini AS CHAR NO-UNDO.
    DEF INPUT PARAMETER p-cd-tecnico-fim AS CHAR NO-UNDO.
    DEF INPUT PARAMETER p-dispon         AS LOG  NO-UNDO.

    DEF VAR de-conclusao   LIKE ord-esp.conclusao NO-UNDO.
    DEF VAR l-considera    AS LOG                 NO-UNDO.
    DEF VAR i-flex         AS INT                 NO-UNDO.
    DEF VAR i-seq          AS INT                 NO-UNDO.
    DEF VAR i-seq-aux      AS INT                 NO-UNDO.
    DEF VAR d-dt-refer     AS DATE                NO-UNDO.
    DEF VAR de-tempo-aux-1 AS DEC DECIMALS 4      NO-UNDO.
    DEF VAR de-tempo-aux-2 AS DEC DECIMALS 4      NO-UNDO.
    DEF VAR de-tempo-aux-3 AS DEC DECIMALS 4      NO-UNDO.

    DEF BUFFER bf-tt-tecn-mi-1 FOR tt-tecn-mi.
    DEF BUFFER bf-tt-tecn-mi-3 FOR tt-tecn-mi.

    FOR EACH tt-tecn-mi 
        WHERE tt-tecn-mi.tipo        = 2
          AND tt-tecn-mi.cd-tecnico >= p-cd-tecnico-ini
          AND tt-tecn-mi.cd-tecnico <= p-cd-tecnico-fim:

        FIND FIRST bf-tt-tecn-mi-1
            WHERE bf-tt-tecn-mi-1.tipo          = 1
              AND bf-tt-tecn-mi-1.cd-equipe-res = tt-tecn-mi.cd-equipe-res NO-ERROR.
        FIND FIRST bf-tt-tecn-mi-3
            WHERE bf-tt-tecn-mi-3.tipo          = 3                        NO-ERROR.
        
        ASSIGN bf-tt-tecn-mi-1.num-om            = bf-tt-tecn-mi-1.num-om            - tt-tecn-mi.num-om                           
               bf-tt-tecn-mi-1.num-om-no-prazo   = bf-tt-tecn-mi-1.num-om-no-prazo   - tt-tecn-mi.num-om-no-prazo          
               bf-tt-tecn-mi-1.num-om-fora-prazo = bf-tt-tecn-mi-1.num-om-fora-prazo - tt-tecn-mi.num-om-fora-prazo        
               bf-tt-tecn-mi-1.tempo             = bf-tt-tecn-mi-1.tempo             - tt-tecn-mi.tempo.

        ASSIGN bf-tt-tecn-mi-3.num-om            = bf-tt-tecn-mi-3.num-om            - tt-tecn-mi.num-om                           
               bf-tt-tecn-mi-3.num-om-no-prazo   = bf-tt-tecn-mi-3.num-om-no-prazo   - tt-tecn-mi.num-om-no-prazo          
               bf-tt-tecn-mi-3.num-om-fora-prazo = bf-tt-tecn-mi-3.num-om-fora-prazo - tt-tecn-mi.num-om-fora-prazo        
               bf-tt-tecn-mi-3.tempo             = bf-tt-tecn-mi-3.tempo             - tt-tecn-mi.tempo.

        ASSIGN tt-tecn-mi.num-om            = 0
               tt-tecn-mi.num-om-no-prazo   = 0
               tt-tecn-mi.num-om-fora-prazo = 0
               tt-tecn-mi.tempo             = 0.

        FOR EACH ext-ord-manut-tecn NO-LOCK
            WHERE ext-ord-manut-tecn.log-ativo = YES
              AND ext-ord-manut-tecn.cd-tecnico = tt-tecn-mi.cd-tecnico:

            FIND FIRST ord-manut NO-LOCK
                WHERE ord-manut.nr-ord-produ = ext-ord-manut-tecn.nr-ord-produ NO-ERROR.

            ASSIGN l-considera  = NO
                   de-conclusao = 0.
            IF ext-ord-manut-tecn.cd-tarefa = 0 THEN DO:
                IF AVAIL ord-manut AND ord-manut.estado-om < 4 THEN
                    ASSIGN l-considera = YES.
            END.
            IF ext-ord-manut-tecn.tp-especial = "" AND ext-ord-manut-tecn.cd-tarefa <> 0 THEN DO:
                FIND FIRST ord-taref NO-LOCK
                    WHERE ord-taref.nr-ord-produ = ext-ord-manut-tecn.nr-ord-produ
                      AND ord-taref.cd-tarefa    = ext-ord-manut-tecn.cd-tarefa    NO-ERROR.
                IF AVAIL ord-taref AND ord-taref.encerrada = NO THEN
                    ASSIGN l-considera = YES.
            END.
            IF ext-ord-manut-tecn.tp-especial <> "" THEN DO:
                FIND FIRST ord-esp NO-LOCK
                    WHERE ord-esp.nr-ord-produ = ext-ord-manut-tecn.nr-ord-produ
                      AND ord-esp.cd-tarefa    = ext-ord-manut-tecn.cd-tarefa    
                      AND ord-esp.tp-especial  = ext-ord-manut-tecn.tp-especial NO-ERROR.
                IF AVAIL ord-esp AND ord-esp.encerrada = NO THEN
                    ASSIGN l-considera = YES
                           de-conclusao = ord-esp.conclusao.
            END.

            IF l-considera = YES THEN DO:

                RUN piRetornaFlex(OUTPUT i-flex).

                IF ord-manut.dt-manut + i-flex <= TODAY THEN
                    ASSIGN tt-tecn-mi.num-om-no-prazo        = tt-tecn-mi.num-om-no-prazo + 1
                           bf-tt-tecn-mi-1.num-om-no-prazo   = bf-tt-tecn-mi-1.num-om-no-prazo + 1
                           bf-tt-tecn-mi-3.num-om-no-prazo   = bf-tt-tecn-mi-3.num-om-no-prazo + 1.
                ELSE
                    ASSIGN tt-tecn-mi.num-om-fora-prazo      = tt-tecn-mi.num-om-fora-prazo + 1
                           bf-tt-tecn-mi-1.num-om-fora-prazo = bf-tt-tecn-mi-1.num-om-fora-prazo + 1
                           bf-tt-tecn-mi-3.num-om-fora-prazo = bf-tt-tecn-mi-3.num-om-fora-prazo + 1.

                ASSIGN tt-tecn-mi.num-om            = tt-tecn-mi.num-om + 1                
                       tt-tecn-mi.tempo             = tt-tecn-mi.tempo + (ext-ord-manut-tecn.tempo * ((100 - de-conclusao) / 100)).

                ASSIGN bf-tt-tecn-mi-1.num-om       = bf-tt-tecn-mi-1.num-om  + 1
                       bf-tt-tecn-mi-1.tempo        = bf-tt-tecn-mi-1.tempo   + (ext-ord-manut-tecn.tempo * ((100 - de-conclusao) / 100)).
                                                                              
                ASSIGN bf-tt-tecn-mi-3.num-om       = bf-tt-tecn-mi-3.num-om  + 1
                       bf-tt-tecn-mi-3.tempo        = bf-tt-tecn-mi-3.tempo   + (ext-ord-manut-tecn.tempo * ((100 - de-conclusao) / 100)).

                IF p-dispon = YES THEN DO:

                    ASSIGN d-dt-refer = ord-manut.dt-prev-manut.
                    IF ord-manut.dt-prev-manut < TODAY      THEN
                        ASSIGN d-dt-refer = TODAY.
                    IF ord-manut.dt-prev-manut > TODAY + 29 THEN
                        ASSIGN d-dt-refer = TODAY + 29.

                    ASSIGN i-seq          = d-dt-refer - TODAY + 1
                           de-tempo-aux-1 = (ext-ord-manut-tecn.tempo * ((100 - de-conclusao) / 100)).

                    REPEAT i-seq-aux = i-seq TO 30:

                        IF tt-tecn-mi.disponibilidade[i-seq-aux] = "NP" THEN NEXT.
                        IF tt-tecn-mi.disponibilidade[i-seq-aux] = "0"  THEN NEXT.

                        IF de-tempo-aux-1 <= 0 THEN NEXT.

                        ASSIGN de-tempo-aux-2 = DEC(tt-tecn-mi.disponibilidade[i-seq-aux]).

                        IF de-tempo-aux-2 <= 0 THEN NEXT.

                        ASSIGN de-tempo-aux-3                        = min(de-tempo-aux-1,de-tempo-aux-2)
                               tt-tecn-mi.disponibilidade[i-seq-aux] = STRING(de-tempo-aux-2 - de-tempo-aux-3,">9.9")
                               de-tempo-aux-1                        = de-tempo-aux-1 - de-tempo-aux-3.
                    END.

                    IF de-tempo-aux-1 > 0 THEN
                        ASSIGN tt-tecn-mi.falta = tt-tecn-mi.falta + de-tempo-aux-1.
                END.
            END.
        END.
    END.
    
END PROCEDURE.

PROCEDURE piAtualizaTecnico:
    DEF INPUT PARAMETER p-cd-tecnico AS CHAR NO-UNDO.

    DEF BUFFER bf-ext-ord-manut-tecn FOR ext-ord-manut-tecn.

    FIND FIRST ttSelecao NO-LOCK NO-ERROR.

    /*Cria os dados de tecnico*/
    RUN piCriaTecnico(2,p-cd-tecnico).

    FOR EACH bf-ext-ord-manut-tecn NO-LOCK
        WHERE bf-ext-ord-manut-tecn.log-ativo  = YES
          AND bf-ext-ord-manut-tecn.cd-tecnico = p-cd-tecnico:
        
        FIND FIRST ord-manut NO-LOCK
            WHERE ord-manut.nr-ord-produ = bf-ext-ord-manut-tecn.nr-ord-produ NO-ERROR.
        IF NOT AVAIL ord-manut THEN NEXT.

        RUN piCriaOM(NO).

        FOR EACH tt-ord-taref
            WHERE tt-ord-taref.nr-ord-produ = bf-ext-ord-manut-tecn.nr-ord-produ:
            ASSIGN tt-ord-taref.cd-tecnico = p-cd-tecnico.
        END.
    END.

    RUN piAtualizaTempo("","ZZZZZZZZ",NO).
    RUN piAjustaSeqTecnico.
END.

PROCEDURE piAjustaSeqTecnico:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER bf-tt-tecn-mi FOR tt-tecn-mi.

    DEF VAR i-sequencia AS INT NO-UNDO.

    FOR EACH tt-tecn-mi 
        WHERE tt-tecn-mi.tipo = 1:

        ASSIGN i-sequencia = i-sequencia + 1.    
        ASSIGN tt-tecn-mi.sequencia = i-sequencia.

        FOR EACH bf-tt-tecn-mi
            WHERE bf-tt-tecn-mi.tipo = 2
              AND bf-tt-tecn-mi.cd-equipe-res = tt-tecn-mi.cd-equipe-res
              BY bf-tt-tecn-mi.nome:
            ASSIGN i-sequencia = i-sequencia + 1.
            ASSIGN bf-tt-tecn-mi.sequencia = i-sequencia.
            
            FOR EACH esp-tecn NO-LOCK
                WHERE esp-tecn.cd-tecnico = bf-tt-tecn-mi.cd-tecnico:
                ASSIGN bf-tt-tecn-mi.tp-especial = bf-tt-tecn-mi.tp-especial + (IF bf-tt-tecn-mi.tp-especial = "" THEN "" ELSE ",") + esp-tecn.tp-especial.
            END.
        END.
    END.
    FIND FIRST tt-tecn-mi
        WHERE tt-tecn-mi.tipo = 3 NO-ERROR.
    IF AVAIL tt-tecn-mi THEN DO:
        ASSIGN i-sequencia = i-sequencia + 1.    
        ASSIGN tt-tecn-mi.sequencia = i-sequencia.
    END.

END PROCEDURE.

PROCEDURE piCriaTecnico:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-tipo-geracao AS INT  NO-UNDO.
    DEF INPUT PARAMETER p-cd-tecnico   AS CHAR NO-UNDO.

    DEF VAR c-cd-equipe-aux AS CHAR NO-UNDO.

    IF p-tipo-geracao = 1 THEN DO:
        EMPTY TEMP-TABLE tt-tecn-mi.

        FOR EACH equipe NO-LOCK
            WHERE equipe.cd-equipe >= ttSelecao.cd-equipe-res-ini
              AND equipe.cd-equipe <= ttSelecao.cd-equipe-res-fim:
            /**********************************************************
            Acumulador Por Equipe
            **********************************************************/
            FIND FIRST tt-tecn-mi
                WHERE tt-tecn-mi.tipo          = 1
                  AND tt-tecn-mi.cd-equipe-res = equipe.cd-equipe NO-ERROR.
            IF NOT AVAIL tt-tecn-mi THEN DO:
                CREATE tt-tecn-mi.
                ASSIGN tt-tecn-mi.tipo          = 1                      
                       tt-tecn-mi.cd-equipe-res = equipe.cd-equipe
                       tt-tecn-mi.cd-equipe     = equipe.cd-equipe
                       tt-tecn-mi.nome          = equipe.desc-equipe.
            END.
            /***********************************************************
            Acumulador por Tecnico
            ***********************************************************/
            FOR EACH tecn-mi NO-LOCK
                WHERE tecn-mi.cd-equipe = equipe.cd-equipe
                  AND tecn-mi.sit-tecn  = 1:

                FIND FIRST tt-tecn-mi
                    WHERE tt-tecn-mi.tipo          = 2
                      AND tt-tecn-mi.cd-equipe-res = equipe.cd-equipe
                      AND tt-tecn-mi.cd-tecnico    = tecn-mi.cd-tecnico NO-ERROR.
                IF NOT AVAIL tt-tecn-mi THEN DO:
                    CREATE tt-tecn-mi.
                    ASSIGN tt-tecn-mi.tipo          = 2
                           tt-tecn-mi.cd-equipe-res = equipe.cd-equipe
                           tt-tecn-mi.cd-tecnico    = tecn-mi.cd-tecnico
                           tt-tecn-mi.nome          = tecn-mi.nome-compl.
                END.
            END.
        END.
    END.
    ELSE DO:
        IF p-cd-tecnico = "" THEN RETURN "NOK".

        FIND FIRST tecn-mi NO-LOCK
            WHERE tecn-mi.cd-tecnico = p-cd-tecnico NO-ERROR.
        IF AVAIL tecn-mi THEN
            ASSIGN c-cd-equipe-aux = tecn-mi.cd-equipe.
        FOR EACH equipe NO-LOCK
            WHERE equipe.cd-equipe = c-cd-equipe-aux:
            /**********************************************************
            Acumulador Por Equipe
            **********************************************************/
            FIND FIRST tt-tecn-mi
                WHERE tt-tecn-mi.tipo          = 1
                  AND tt-tecn-mi.cd-equipe-res = equipe.cd-equipe NO-ERROR.
            IF NOT AVAIL tt-tecn-mi THEN DO:
                CREATE tt-tecn-mi.
                ASSIGN tt-tecn-mi.tipo          = 1                      
                       tt-tecn-mi.cd-equipe-res = equipe.cd-equipe
                       tt-tecn-mi.cd-equipe     = equipe.cd-equipe
                       tt-tecn-mi.nome          = equipe.desc-equipe.
            END.
            /***********************************************************
            Acumulador por Tecnico
            ***********************************************************/
            FOR EACH tecn-mi NO-LOCK
                WHERE tecn-mi.cd-equipe = equipe.cd-equipe
                  AND tecn-mi.sit-tecn  = 1:

                FIND FIRST tt-tecn-mi
                    WHERE tt-tecn-mi.tipo          = 2
                      AND tt-tecn-mi.cd-equipe-res = equipe.cd-equipe
                      AND tt-tecn-mi.cd-tecnico    = tecn-mi.cd-tecnico NO-ERROR.
                IF NOT AVAIL tt-tecn-mi THEN DO:
                    CREATE tt-tecn-mi.
                    ASSIGN tt-tecn-mi.tipo          = 2
                           tt-tecn-mi.cd-equipe-res = equipe.cd-equipe
                           tt-tecn-mi.cd-tecnico    = tecn-mi.cd-tecnico
                           tt-tecn-mi.nome          = tecn-mi.nome-compl.
                END.
            END.
        END.
    END.

    /**********************************************************
    Acumulador Total
    **********************************************************/
    FIND FIRST tt-tecn-mi
        WHERE tt-tecn-mi.tipo          = 3
          AND tt-tecn-mi.cd-equipe-res = ""
          AND tt-tecn-mi.cd-tecnico    = "" NO-ERROR.
    IF NOT AVAIL tt-tecn-mi THEN DO:
        CREATE tt-tecn-mi.
        ASSIGN tt-tecn-mi.tipo          = 3
               tt-tecn-mi.cd-equipe-res = ""
               tt-tecn-mi.cd-tecnico    = ""
               tt-tecn-mi.nome          = "TOTAL".
    END.

END PROCEDURE.

PROCEDURE piGeraCarga:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR d-data-ini   AS DATE                         NO-UNDO.
    DEF VAR d-data-fim   AS DATE                         NO-UNDO.
    DEF VAR d-data-ref   AS DATE                         NO-UNDO.
    DEF VAR c-hora-ini   AS CHAR                         NO-UNDO.
    DEF VAR c-hora-fim   AS CHAR                         NO-UNDO.
    DEF VAR i-tipo-dia   LIKE data-turno-calen.tipo-dia  NO-UNDO.
    DEF VAR i-tipo-hora  LIKE data-turno-calen.tipo-hora NO-UNDO.
    DEF VAR de-hora      AS DEC                          NO-UNDO.
    DEF VAR d-dt-ini-cap AS DATE                         NO-UNDO.
    DEF VAR d-dt-fim-cap AS DATE                         NO-UNDO.
    DEF VAR d-data-aux-1 AS DATE                         NO-UNDO.
    DEF VAR d-data-aux-2 AS DATE                         NO-UNDO.
    DEF VAR i-seq        AS INT                          NO-UNDO.
    
    DEF BUFFER bf-cap-tecn-turno FOR cap-tecn-turno.

    ASSIGN d-data-ini = TODAY
           d-data-fim = TODAY + 29. 
    FOR EACH tt-tecn-mi
        WHERE tt-tecn-mi.tipo = 1:
        REPEAT i-seq = 1 TO 30:
            ASSIGN d-data-aux-1 = TODAY - 1 + i-seq.
            ASSIGN tt-tecn-mi.disponibilidade[i-seq] = STRING(DAY(d-data-aux-1),"99") + "-" + STRING(MONTH(d-data-aux-1),"99").
        END.
    END.
    
    FOR EACH tt-tecn-mi
        WHERE tt-tecn-mi.tipo = 2:

        ASSIGN tt-tecn-mi.disponibilidade = ""
               tt-tecn-mi.falta           = 0.

        FIND FIRST tecn-mi NO-LOCK
            WHERE tecn-mi.cd-tecnico = tt-tecn-mi.cd-tecnico NO-ERROR.

        FIND FIRST cap-tecn-turno NO-LOCK
            WHERE cap-tecn-turno.cd-tecnico = tecn-mi.cd-tecnico
              AND cap-tecn-turno.dt-efetivacao >= d-data-ini     NO-ERROR.
        IF NOT AVAIL cap-tecn-turno THEN DO:
            FIND LAST cap-tecn-turno NO-LOCK
                WHERE cap-tecn-turno.cd-tecnico = tecn-mi.cd-tecnico NO-ERROR.
            IF AVAIL cap-tecn-turno THEN
                ASSIGN d-dt-ini-cap = cap-tecn-turno.dt-efetivacao.
        END.
        ELSE DO:
            IF cap-tecn-turno.dt-efetivacao = d-data-ini THEN
                ASSIGN d-dt-ini-cap = d-data-ini.
            ELSE DO:
                FIND LAST cap-tecn-turno NO-LOCK
                    WHERE cap-tecn-turno.cd-tecnico    = tecn-mi.cd-tecnico
                      AND cap-tecn-turno.dt-efetivacao < d-data-ini     NO-ERROR.
                ASSIGN d-dt-ini-cap = cap-tecn-turno.dt-efetivacao.
            END.
        END.    
        
        FOR EACH cap-tecn-turno NO-LOCK OF tecn-mi
            WHERE cap-tecn-turno.dt-efetivacao >= d-dt-ini-cap
              AND cap-tecn-turno.dt-efetivacao <= d-data-fim
               BY cap-tecn-turno.dt-efetivacao:
            
            IF d-data-ini < d-dt-ini-cap THEN  
                ASSIGN d-data-aux-1 = d-dt-ini-cap.
            ELSE
                ASSIGN d-data-aux-1 = d-data-ini.

            FIND bf-cap-tecn-turno NO-LOCK
                WHERE ROWID(bf-cap-tecn-turno) = ROWID(cap-tecn-turno) NO-ERROR.
            FIND NEXT bf-cap-tecn-turno NO-LOCK NO-ERROR.

            IF AVAIL bf-cap-tecn-turno THEN
                ASSIGN d-data-aux-2 = MAX(d-data-fim,bf-cap-tecn-turno.dt-efetivacao - 1).
            ELSE
                ASSIGN d-data-aux-2 = d-data-fim.

            FOR EACH data-turno-calen NO-LOCK
                WHERE data-turno-calen.cd-calen = tecn-mi.cd-calen
                  AND data-turno-calen.cd-turno = cap-tecn-turno.cd-turno
                  AND data-turno-calen.data    >= d-data-aux-1
                  AND data-turno-calen.data    <= d-data-aux-2
                  BREAK BY data-turno-calen.data 
                        BY data-turno-calen.sequencia:

                FIND exc-tecn-data NO-LOCK
                    WHERE exc-tecn-data.cd-tecnico = tecn-mi.cd-tecnico
                      AND exc-tecn-data.cd-turno   = cap-tecn-turno.cd-turno
                      AND exc-tecn-data.data       = data-turno-calen.data
                      AND exc-tecn-data.sequencia  = data-turno-calen.sequencia NO-ERROR.

                IF NOT AVAIL exc-tecn-data THEN 
                    ASSIGN d-data-ref  = data-turno-calen.data          
                           c-hora-ini  = data-turno-calen.hora-inicio   
                           c-hora-fim  = data-turno-calen.hora-termino
                           i-tipo-dia  = data-turno-calen.tipo-dia      
                           i-tipo-hora = data-turno-calen.tipo-hora.
                ELSE 
                    ASSIGN d-data-ref  = data-turno-calen.data          
                           c-hora-ini  = exc-tecn-data.hora-inicio    
                           c-hora-fim  = exc-tecn-data.hora-termino 
                           i-tipo-dia  = IF exc-tecn-data.hora-trab = YES THEN 1 ELSE 2
                           i-tipo-hora = exc-tecn-data.tipo-hora .

                IF i-tipo-dia <> 1 THEN NEXT.

                ASSIGN c-hora-ini                                         = REPLACE(c-hora-ini,":","")
                       c-hora-fim                                         = REPLACE(c-hora-fim,":","")
                       de-hora                                            = (INT(SUBSTRING(c-hora-fim,1,2)) * 3600 + 
                                                                             INT(SUBSTRING(c-hora-fim,3,2)) * 60 + 
                                                                             INT(SUBSTRING(c-hora-fim,5,2))) - 
                                                                            (INT(SUBSTRING(c-hora-ini,1,2)) * 3600 + 
                                                                             INT(SUBSTRING(c-hora-ini,3,2)) * 60 +   
                                                                             INT(SUBSTRING(c-hora-ini,5,2)))
                       de-hora                                            = de-hora / 3600
                       tt-tecn-mi.disponibilidade[d-data-ref - TODAY + 1] = STRING(DEC(tt-tecn-mi.disponibilidade[d-data-ref - TODAY + 1]) + de-hora,">9.9").
            END.    
        END.
        
        REPEAT i-seq = 1 TO 30:
            IF DEC(tt-tecn-mi.disponibilidade[i-seq]) = 0 THEN
                ASSIGN tt-tecn-mi.disponibilidade[i-seq] = "NP".
        END.

        RUN piAtualizaTempo(tt-tecn-mi.cd-tecnico,tt-tecn-mi.cd-tecnico,YES).
    END.

END PROCEDURE.

PROCEDURE piRetornaFlex:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER p-flex AS INT INIT 0 NO-UNDO.
    
    IF NOT AVAIL ord-manut THEN RETURN "NOK".

    IF ord-manut.cd-manut <> "" THEN DO:
        case ord-manut.plano:
            when "man-equip" then do:
                find man-equip
                    where man-equip.cd-equipto = ord-manut.cd-equipto
                    and   man-equip.cd-manut   = ord-manut.cd-manut   no-lock no-error.
                if  avail man-equip then
                    ASSIGN p-flex = man-equip.flexib-prorr.
            END.
            when "man-fam" then do:
                find man-fam
                    where man-fam.fm-equipto = tt-ord-manut.fm-equipto
                    and   man-fam.cd-manut   = tt-ord-manut.cd-manut  no-lock no-error.

                if  avail man-fam then
                    ASSIGN p-flex = man-fam.flexib-prorr.
            end.
            when "man-tag-fam" then do:
                find man-tag-fam
                    where man-tag-fam.fm-equipto = tt-ord-manut.fm-equipto
                    and   man-tag-fam.cd-tag     = tt-ord-manut.cd-tag
                    and   man-tag-fam.cd-manut   = tt-ord-manut.cd-manut   NO-LOCK no-error.
                if  avail man-tag-fam then
                    ASSIGN p-flex = man-tag-fam.flexib-prorr.
            end.
            when "man-eq-tag" then do:
                find man-eq-tag
                    where man-eq-tag.cd-equipto = tt-ord-manut.cd-equipto
                    and   man-eq-tag.cd-tag     = tt-ord-manut.cd-tag
                    and   man-eq-tag.cd-manut   = tt-ord-manut.cd-manut   no-lock no-error.
                if  avail man-eq-tag then
                    ASSIGN p-flex = man-eq-tag.flexib-prorr.
            end.
        end case.
    END.

    RETURN "OK".

END PROCEDURE.

PROCEDURE piAtualizaMI0408:
    DEF INPUT PARAMETER p-tipo-exec     AS INT  NO-UNDO. /*1 - Completa , 2 - Almoxarifado*/
    DEF INPUT PARAMETER p-arquivo-excel AS CHAR NO-UNDO.

    EMPTY TEMP-TABLE tt-ord-manut.
    EMPTY TEMP-TABLE tt-ord-taref.
    EMPTY TEMP-TABLE tt-tecn-mi.
    EMPTY TEMP-TABLE tt-ord-esp.
    EMPTY TEMP-TABLE tt-ord-fich-met.
    EMPTY TEMP-TABLE tt-reservas.
    EMPTY TEMP-TABLE tt-reservas-sdo.
    EMPTY TEMP-TABLE tt-almoxarifado.

    FIND FIRST ttSelecao NO-LOCK NO-ERROR.
    IF NOT AVAIL ttSelecao THEN CREATE ttSelecao.

    run utp/ut-acomp.p persistent set h-acomp.
    
    run pi-inicializar in h-acomp(input "Buscando Ordens").

    ASSIGN i-acomp = 0.
    
    IF p-tipo-exec = 1 THEN DO:
        FOR EACH tt-ord-mi0408:
            FIND FIRST ord-manut NO-LOCK
                WHERE ord-manut.nr-ord-produ = tt-ord-mi0408.nr-ord-produ NO-ERROR.
            IF NOT AVAIL ord-manut THEN NEXT.

            RUN piCriaOM(NO).
        END.
    END.
    ELSE DO:
        FOR EACH tt-ord-mi0408:
            FIND FIRST ord-manut NO-LOCK
                WHERE ord-manut.nr-ord-produ = tt-ord-mi0408.nr-ord-produ NO-ERROR.
            IF NOT AVAIL ord-manut THEN NEXT.

            RUN piCriaOMResumida.
            IF RETURN-VALUE = "NOK" THEN NEXT.

            FOR EACH tt-reservas NO-LOCK
                WHERE tt-reservas.nr-ord-produ = tt-ord-manut.nr-ord-produ:

                BUFFER-COPY tt-ord-manut TO tt-reservas.

                CREATE tt-almoxarifado.
                BUFFER-COPY tt-reservas TO tt-almoxarifado
                    ASSIGN tt-almoxarifado.r-rowid-reserva = ROWID(tt-reservas).
            END.
        END.
    END.
    run pi-finalizar in h-acomp.
    
    IF p-tipo-exec = 1 THEN DO:
        RUN pi-impressao-dados("",p-arquivo-excel).
    END.
    ELSE DO:
        RUN pi-impressao-almox(p-arquivo-excel).
    END.

    RETURN "OK".
END.

PROCEDURE pi-impressao-almox:
    DEF INPUT PARAMETER p-arquivo-excel AS CHAR NO-UNDO.

    EMPTY TEMP-TABLE tt-paginas.
    EMPTY TEMP-TABLE tt-paginas-ext.
    EMPTY TEMP-TABLE tt-campos-var.
    EMPTY TEMP-TABLE tt-campos-var-id.

    RUN pi-cria-dados-almox(1).

    IF p-arquivo-excel = "" THEN
        RUN utp\esutapi003a.p(1,"",INPUT TABLE tt-paginas,INPUT TABLE tt-paginas-ext).
    ELSE
        RUN utp\esutapi003a.p(2,p-arquivo-excel,INPUT TABLE tt-paginas,INPUT TABLE tt-paginas-ext).

    RETURN "OK".

END.

PROCEDURE pi-impressao-dados:
    DEF INPUT PARAMETER p-Dimensao      AS CHAR NO-UNDO.
    DEF INPUT PARAMETER p-arquivo-excel AS CHAR NO-UNDO.

    DEF VAR c-arquivo       AS CHAR NO-UNDO.
    DEF VAR c-formato       AS CHAR NO-UNDO.
    DEF VAR c-delim         AS CHAR NO-UNDO.
    
    DEF VAR c-lista-campos  AS CHAR NO-UNDO.
    DEF VAR c-query         AS CHAR NO-UNDO.
    DEF VAR i-id-linha      AS INT  NO-UNDO.
    DEF VAR i-sequencia     AS INT  NO-UNDO.
    DEF VAR c-tp-especial   AS CHAR NO-UNDO.
    DEF VAR i-aux           AS INT  NO-UNDO.
    
    EMPTY TEMP-TABLE exp-tt-ord-manut.
    EMPTY TEMP-TABLE exp-tt-reservas.
    EMPTY TEMP-TABLE exp-tt-ord-fich-met.
    EMPTY TEMP-TABLE tt-almoxarifado.
    EMPTY TEMP-TABLE tt-paginas.
    EMPTY TEMP-TABLE tt-paginas-ext.
    EMPTY TEMP-TABLE tt-campos-var.
    EMPTY TEMP-TABLE tt-campos-var-id.

    /*Lista de campos fixos para atender demanda do usu†rio*/
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

    RUN piDetalhaOM.
       
    ASSIGN i-id-linha = 0.
    FOR EACH tt-ord-manut USE-INDEX ch-dimensao
        WHERE tt-ord-manut.cod-dimensao BEGINS p-Dimensao:

        ASSIGN tt-ord-manut.cd-parada-2 = tt-ord-manut.cd-parada.

        FOR EACH tt-reservas NO-LOCK
            WHERE tt-reservas.nr-ord-produ = tt-ord-manut.nr-ord-produ:
            CREATE exp-tt-reservas.
            BUFFER-COPY tt-reservas TO exp-tt-reservas
                ASSIGN exp-tt-reservas.r-rowid = ROWID(tt-reservas).

            CREATE tt-almoxarifado.
            BUFFER-COPY tt-reservas TO tt-almoxarifado
                ASSIGN tt-almoxarifado.r-rowid-reserva = ROWID(tt-reservas).
        END.

        CREATE exp-tt-ord-manut.
        BUFFER-COPY tt-ord-manut TO exp-tt-ord-manut
            ASSIGN exp-tt-ord-manut.id-linha = i-id-linha.
        
        ASSIGN i-id-linha = i-id-linha + 1.
        FOR EACH tt-ord-esp NO-LOCK
            WHERE tt-ord-esp.nr-ord-produ = tt-ord-manut.nr-ord-produ:

            IF tt-ord-esp.tp-especial = "" THEN 
                ASSIGN c-tp-especial = "SEM".
            ELSE DO:
                ASSIGN c-tp-especial = tt-ord-esp.tp-especial.
                FIND FIRST tt-mi-espec NO-LOCK 
                    WHERE tt-mi-espec.tp-especial = c-tp-especial NO-ERROR.
                IF AVAIL tt-mi-espec AND tt-mi-espec.abreviatura <> "" THEN
                    ASSIGN c-tp-especial = tt-mi-espec.abreviatura.
            END.

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
                WHERE tt-campos-var-id.id-linha  = exp-tt-ord-manut.id-linha
                  AND tt-campos-var-id.campo-var = tt-campos-var.campo-var
                  AND tt-campos-var-id.sequencia = tt-campos-var.sequencia    NO-ERROR.
            IF NOT AVAIL tt-campos-var-id THEN DO:
                CREATE tt-campos-var-id.
                ASSIGN tt-campos-var-id.id-linha  = exp-tt-ord-manut.id-linha
                       tt-campos-var-id.campo-var = tt-campos-var.campo-var  
                       tt-campos-var-id.sequencia = tt-campos-var.sequencia.
            END.
            ASSIGN tt-campos-var-id.valor = STRING(INT(tt-campos-var-id.valor) + tt-ord-esp.nr-homens,tt-campos-var.formato).

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
                WHERE tt-campos-var-id.id-linha  = exp-tt-ord-manut.id-linha
                  AND tt-campos-var-id.campo-var = tt-campos-var.campo-var
                  AND tt-campos-var-id.sequencia = tt-campos-var.sequencia    NO-ERROR.
            IF NOT AVAIL tt-campos-var-id THEN DO:
                CREATE tt-campos-var-id.
                ASSIGN tt-campos-var-id.id-linha  = exp-tt-ord-manut.id-linha
                       tt-campos-var-id.campo-var = tt-campos-var.campo-var  
                       tt-campos-var-id.sequencia = tt-campos-var.sequencia.
            END.
            ASSIGN tt-campos-var-id.valor = STRING(DEC(tt-campos-var-id.valor) + tt-ord-esp.tempo,tt-campos-var.formato).
        END.

        FOR EACH tt-ord-fich-met
            WHERE tt-ord-fich-met.nr-ord-produ = tt-ord-manut.nr-ord-produ:
            CREATE exp-tt-ord-fich-met.
            BUFFER-COPY tt-ord-fich-met TO exp-tt-ord-fich-met.
        END.
    END.

    ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "\" + "ordens" + ".txt"
           c-formato = ""
           c-delim   = "&^%".

    ASSIGN c-query = "for each exp-tt-ord-manut".

    ASSIGN c-lista-campos = "l-marca;cod-dimensao;cd-erro;mensagem;tipo-registro;r-rowid;estado-om;" + 
                            "nova-dt-prev-manut;nova-cd-parada;nova-sequencia;nova-cd-projeto;" + 
                            "nova-seq-exec;id-linha;log-atua-mat-cst;novo-sc-desp;nova-num-ord-inv;" + 
                            "tp-manut;tipo;cd-projeto;dias-atraso;log-nec-tecn;flexib-prorr".

    ASSIGN c-delim = "ñ".
    
    RUN utp\esutapi004b.p (INPUT        ?,                                                       /*h-browse       */
                           INPUT        TEMP-TABLE exp-tt-ord-manut:DEFAULT-BUFFER-HANDLE,       /*tt-dados-buffer*/
                           INPUT        "",                                                      /*p-cod-programa */
                           INPUT-OUTPUT c-arquivo,                                               /*c-arquivo      */
                           INPUT-OUTPUT c-formato,                                               /*c-formato      */
                           INPUT-OUTPUT c-delim,                                                 /*c-delim        */
                           INPUT        YES,                                                     /*p-cabec        */
                           INPUT        NO,                                                      /*p-browse       */
                           INPUT        c-query,                                                 /*p-query        */
                           INPUT        "",                                                      /*Lista de campos que serío exportados*/
                           INPUT        c-lista-campos,                                          /*Lista de campos que devem ser desconsiderados*/
                           INPUT        "corringa-1",                                            /*Lista de Campos Variaveis*/
                           INPUT        NO,                                                      /*Lista formato dos campos*/
                           INPUT        2).                                                      /*Formato Extendido 1 com [], 2 sem []*/
    IF RETURN-VALUE = "NOK" THEN RETURN "NOK".
    
    CREATE tt-paginas.
    ASSIGN tt-paginas.pagina      = 1
           tt-paginas.nome-pagina = "Ordens"
           tt-paginas.arquivo     = c-arquivo
           tt-paginas.formato     = c-formato
           tt-paginas.delimitador = c-delim.

    /*Campo Narrativa*/
    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 1
           tt-paginas-ext.comando        = "REPLACE"
           tt-paginas-ext.parametros[1]  = "NARRATIVA"
           tt-paginas-ext.parametros[2]  = "|"
           tt-paginas-ext.parametros[3]  = CHR(10).

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 2
           tt-paginas-ext.comando        = "AjustaTamColuna"
           tt-paginas-ext.parametros[1]  = "NARRATIVA"
           tt-paginas-ext.parametros[2]  = "100".

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 3
           tt-paginas-ext.comando        = "WrapText"
           tt-paginas-ext.parametros[1]  = "NARRATIVA".

    /*Campo Ficha MÇtodo*/
    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 4
           tt-paginas-ext.comando        = "REPLACE"
           tt-paginas-ext.parametros[1]  = "FICHA MêTODO"
           tt-paginas-ext.parametros[2]  = "|"
           tt-paginas-ext.parametros[3]  = CHR(10).

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 5
           tt-paginas-ext.comando        = "AjustaTamColuna"
           tt-paginas-ext.parametros[1]  = "FICHA MêTODO"
           tt-paginas-ext.parametros[2]  = "80".

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 6
           tt-paginas-ext.comando        = "WrapText"
           tt-paginas-ext.parametros[1]  = "FICHA MêTODO".
    
    /*Altera Campo PARADA_ PARA PARADA*/
    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 7
           tt-paginas-ext.comando        = "AlteraLabel"
           tt-paginas-ext.parametros[1]  = "PARADA_"
           tt-paginas-ext.parametros[2]  = "PARADA"
           tt-paginas-ext.parametros[3]  = "1".
    

    /*Campo Materiais*/
    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 8
           tt-paginas-ext.comando        = "REPLACE"
           tt-paginas-ext.parametros[1]  = "MATERIAIS"
           tt-paginas-ext.parametros[2]  = "|"
           tt-paginas-ext.parametros[3]  = CHR(10).

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 9
           tt-paginas-ext.comando        = "AjustaTamColuna"
           tt-paginas-ext.parametros[1]  = "MATERIAIS"
           tt-paginas-ext.parametros[2]  = "115".

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 10
           tt-paginas-ext.comando        = "WrapText"
           tt-paginas-ext.parametros[1]  = "MATERIAIS".

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 11
           tt-paginas-ext.comando        = "Font"
           tt-paginas-ext.parametros[1]  = "MATERIAIS"
           tt-paginas-ext.parametros[2]  = "Courier".
               
    /*Campo Materiais Alocados*/
    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 12
           tt-paginas-ext.comando        = "REPLACE"
           tt-paginas-ext.parametros[1]  = "MATERIAIS ALOCADOS"
           tt-paginas-ext.parametros[2]  = "|"
           tt-paginas-ext.parametros[3]  = CHR(10).

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 13
           tt-paginas-ext.comando        = "AjustaTamColuna"
           tt-paginas-ext.parametros[1]  = "MATERIAIS ALOCADOS"
           tt-paginas-ext.parametros[2]  = "115".

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 14
           tt-paginas-ext.comando        = "WrapText"
           tt-paginas-ext.parametros[1]  = "MATERIAIS ALOCADOS".

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 15
           tt-paginas-ext.comando        = "Font"
           tt-paginas-ext.parametros[1]  = "MATERIAIS ALOCADOS"
           tt-paginas-ext.parametros[2]  = "Courier".
    
    /*Campo Materiais Realizados*/
    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 16
           tt-paginas-ext.comando        = "REPLACE"
           tt-paginas-ext.parametros[1]  = "MATERIAIS REALIZADOS"
           tt-paginas-ext.parametros[2]  = "|"
           tt-paginas-ext.parametros[3]  = CHR(10).

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 17
           tt-paginas-ext.comando        = "AjustaTamColuna"
           tt-paginas-ext.parametros[1]  = "MATERIAIS REALIZADOS"
           tt-paginas-ext.parametros[2]  = "115".

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 18
           tt-paginas-ext.comando        = "WrapText"
           tt-paginas-ext.parametros[1]  = "MATERIAIS REALIZADOS".

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 1
           tt-paginas-ext.sequencia      = 19
           tt-paginas-ext.comando        = "Font"
           tt-paginas-ext.parametros[1]  = "MATERIAIS REALIZADOS"
           tt-paginas-ext.parametros[2]  = "Courier".

    ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "\" + "reservas" + ".txt"
           c-formato = ""
           c-delim   = "&^%".

    ASSIGN c-lista-campos = "r-rowid;id-linha".

    EMPTY TEMP-TABLE tt-campos-var.
    EMPTY TEMP-TABLE tt-campos-var-id.

    IF l-considera-dep THEN DO:

        FOR EACH tt-deposito USE-INDEX ch-seq:
            /*corringa-1*/
            FIND FIRST tt-campos-var
                WHERE tt-campos-var.campo-var = "corringa-1"
                  AND tt-campos-var.titulo    = "QTDE " + tt-deposito.cod-depos NO-ERROR.
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
                       tt-campos-var.titulo    = "QTDE " + tt-deposito.cod-depos
                       tt-campos-var.formato   = "->>>,>>>,>>9.9999"
                       tt-campos-var.tipo-dado = "Decimal".
            END.

            IF tt-deposito.log-localiz = YES THEN DO:
                /*corringa-1*/
                FIND FIRST tt-campos-var
                    WHERE tt-campos-var.campo-var = "corringa-1"
                      AND tt-campos-var.titulo    = "LOCALIZAÄ«O " + tt-deposito.cod-depos NO-ERROR.
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
                           tt-campos-var.titulo    = "LOCALIZAÄ«O " + tt-deposito.cod-depos
                           tt-campos-var.formato   = "x(100)"
                           tt-campos-var.tipo-dado = "Character".
                END.
            END.
        END.

    END.

    ASSIGN i-id-linha = 0.
    FOR EACH exp-tt-reservas:

        ASSIGN i-id-linha = i-id-linha + 1.

        ASSIGN exp-tt-reservas.id-linha = i-id-linha.

        FOR EACH tt-reservas-sdo 
            WHERE tt-reservas-sdo.r-rowid-reserva = exp-tt-reservas.r-rowid:
            /*corringa-1*/
            FIND FIRST tt-campos-var
                WHERE tt-campos-var.campo-var = "corringa-1"
                  AND tt-campos-var.titulo    = "QTDE " + tt-reservas-sdo.cod-depos NO-ERROR.
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
                       tt-campos-var.titulo    = "QTDE " + tt-reservas-sdo.cod-depos
                       tt-campos-var.formato   = "->>>,>>>,>>9.9999"
                       tt-campos-var.tipo-dado = "Decimal".
            END.

            FIND FIRST tt-campos-var-id 
                WHERE tt-campos-var-id.id-linha  = exp-tt-reservas.id-linha
                  AND tt-campos-var-id.campo-var = tt-campos-var.campo-var
                  AND tt-campos-var-id.sequencia = tt-campos-var.sequencia    NO-ERROR.
            IF NOT AVAIL tt-campos-var-id THEN DO:
                CREATE tt-campos-var-id.
                ASSIGN tt-campos-var-id.id-linha  = exp-tt-reservas.id-linha
                       tt-campos-var-id.campo-var = tt-campos-var.campo-var  
                       tt-campos-var-id.sequencia = tt-campos-var.sequencia.
            END.
            ASSIGN tt-campos-var-id.valor = STRING(DEC(tt-campos-var-id.valor) + tt-reservas-sdo.qtidade-disp,tt-campos-var.formato).

            IF l-considera-dep AND tt-reservas-sdo.cod-localiz <> "" THEN DO:
                /*corringa-1*/
                FIND FIRST tt-campos-var
                    WHERE tt-campos-var.campo-var = "corringa-1"
                      AND tt-campos-var.titulo    = "LOCALIZAÄ«O " + tt-reservas-sdo.cod-depos NO-ERROR.
                IF AVAIL tt-campos-var THEN DO:
                    FIND FIRST tt-campos-var-id 
                        WHERE tt-campos-var-id.id-linha  = exp-tt-reservas.id-linha
                          AND tt-campos-var-id.campo-var = tt-campos-var.campo-var
                          AND tt-campos-var-id.sequencia = tt-campos-var.sequencia    NO-ERROR.
                    IF NOT AVAIL tt-campos-var-id THEN DO:
                        CREATE tt-campos-var-id.
                        ASSIGN tt-campos-var-id.id-linha  = exp-tt-reservas.id-linha
                               tt-campos-var-id.campo-var = tt-campos-var.campo-var  
                               tt-campos-var-id.sequencia = tt-campos-var.sequencia.
                    END.
                    ASSIGN tt-campos-var-id.valor = tt-campos-var-id.valor + (IF tt-campos-var-id.valor = "" THEN "" ELSE "|") + tt-reservas-sdo.cod-localiz.
                END.
            END.
        END.
    END.

    RUN utp\esutapi004b.p (INPUT        ?,                                                                /*h-browse       */
                           INPUT        TEMP-TABLE exp-tt-reservas:DEFAULT-BUFFER-HANDLE,                 /*tt-dados-buffer*/
                           INPUT        "",                                                               /*p-cod-programa */
                           INPUT-OUTPUT c-arquivo,                                                        /*c-arquivo      */
                           INPUT-OUTPUT c-formato,                                                        /*c-formato      */
                           INPUT-OUTPUT c-delim,                                                          /*c-delim        */
                           INPUT        YES,                                                              /*p-cabec        */
                           INPUT        NO,                                                               /*p-browse       */
                           INPUT        "for each exp-tt-reservas",                                       /*p-query        */
                           INPUT        "",                                                               /*Lista de campos que serao exportados*/
                           INPUT        c-lista-campos,                                                   /*Lista de campos que devem ser desconsiderados*/
                           INPUT        "corringa-1",                                                     /*Lista de Campos Variaveis*/
                           INPUT        NO,                                                               /*Lista formato dos campos*/
                           INPUT        2).                                                               /*Formato Extendido 1 com [], 2 sem []*/

    CREATE tt-paginas.
    ASSIGN tt-paginas.pagina      = 2
           tt-paginas.nome-pagina = "Materiais"
           tt-paginas.arquivo     = c-arquivo
           tt-paginas.formato     = c-formato
           tt-paginas.delimitador = c-delim.

    /*Campo Localizaá‰es*/
    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 2
           tt-paginas-ext.sequencia      = 1
           tt-paginas-ext.comando        = "REPLACE"
           tt-paginas-ext.parametros[1]  = "Localizaá‰es"
           tt-paginas-ext.parametros[2]  = "|"
           tt-paginas-ext.parametros[3]  = CHR(10).

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 2
           tt-paginas-ext.sequencia      = 2
           tt-paginas-ext.comando        = "AjustaTamColuna"
           tt-paginas-ext.parametros[1]  = "Localizaá‰es"
           tt-paginas-ext.parametros[2]  = "118".

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 2
           tt-paginas-ext.sequencia      = 3
           tt-paginas-ext.comando        = "WrapText"
           tt-paginas-ext.parametros[1]  = "Localizaá‰es".

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = 2
           tt-paginas-ext.sequencia      = 4
           tt-paginas-ext.comando        = "Font"
           tt-paginas-ext.parametros[1]  = "Localizaá‰es"
           tt-paginas-ext.parametros[2]  = "Courier".

    ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "\" + "ord-fich-met" + ".txt"
           c-formato = ""
           c-delim   = "&^%".

    EMPTY TEMP-TABLE tt-campos-var.
    EMPTY TEMP-TABLE tt-campos-var-id.

    RUN utp\esutapi004a.p (INPUT        ?,                                                                /*h-browse       */
                           INPUT        TEMP-TABLE exp-tt-ord-fich-met:DEFAULT-BUFFER-HANDLE,             /*tt-dados-buffer*/
                           INPUT        "",                                                               /*p-cod-programa */
                           INPUT-OUTPUT c-arquivo,                                                        /*c-arquivo      */
                           INPUT-OUTPUT c-formato,                                                        /*c-formato      */
                           INPUT-OUTPUT c-delim,                                                          /*c-delim        */
                           INPUT        YES,                                                              /*p-cabec        */
                           INPUT        NO,                                                               /*p-browse       */
                           INPUT        "for each exp-tt-ord-fich-met",                                   /*p-query        */
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

    /*Campo Narrativa*/
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

    RUN pi-cria-dados-almox(4).

    IF p-arquivo-excel = "" THEN
        RUN utp\esutapi003a.p(1,"",INPUT TABLE tt-paginas,INPUT TABLE tt-paginas-ext).
    ELSE
        RUN utp\esutapi003a.p(2,p-arquivo-excel,INPUT TABLE tt-paginas,INPUT TABLE tt-paginas-ext).

    RETURN "OK".
END.

PROCEDURE pi-cria-dados-almox:
    DEF INPUT PARAMETER p-pagina AS INT NO-UNDO.

    DEF VAR c-arquivo       AS CHAR NO-UNDO.
    DEF VAR c-formato       AS CHAR NO-UNDO.
    DEF VAR c-delim         AS CHAR NO-UNDO.
    
    DEF VAR c-lista-campos  AS CHAR NO-UNDO.
    DEF VAR c-query         AS CHAR NO-UNDO.
    DEF VAR i-id-linha      AS INT  NO-UNDO.
    DEF VAR i-sequencia     AS INT  NO-UNDO.
    DEF VAR c-tp-especial   AS CHAR NO-UNDO.
    DEF VAR i-aux           AS INT  NO-UNDO.

    ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "\" + "almox" + ".txt"
           c-formato = ""
           c-delim   = "&^%".

    ASSIGN c-lista-campos = "r-rowid-reserva;id-linha".

    EMPTY TEMP-TABLE tt-campos-var.
    EMPTY TEMP-TABLE tt-campos-var-id.
    
    IF l-considera-dep THEN DO:

        FOR EACH tt-deposito USE-INDEX ch-seq:
            /*corringa-1*/
            FIND FIRST tt-campos-var
                WHERE tt-campos-var.campo-var = "corringa-1"
                  AND tt-campos-var.titulo    = "QTDE " + tt-deposito.cod-depos NO-ERROR.
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
                       tt-campos-var.titulo    = "QTDE " + tt-deposito.cod-depos
                       tt-campos-var.formato   = "->>>,>>>,>>9.9999"
                       tt-campos-var.tipo-dado = "Decimal".
            END.

            IF tt-deposito.log-localiz = YES THEN DO:
                /*corringa-1*/
                FIND FIRST tt-campos-var
                    WHERE tt-campos-var.campo-var = "corringa-1"
                      AND tt-campos-var.titulo    = "LOCALIZAÄ«O " + tt-deposito.cod-depos NO-ERROR.
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
                           tt-campos-var.titulo    = "LOCALIZAÄ«O " + tt-deposito.cod-depos
                           tt-campos-var.formato   = "x(100)"
                           tt-campos-var.tipo-dado = "Character".
                END.
            END.
        END.

    END.
    
    ASSIGN i-id-linha = 0.
    FOR EACH tt-almoxarifado:

        ASSIGN i-id-linha = i-id-linha + 1.

        ASSIGN tt-almoxarifado.id-linha = i-id-linha.

        FOR EACH tt-reservas-sdo 
            WHERE tt-reservas-sdo.r-rowid-reserva = tt-almoxarifado.r-rowid-reserva:
            /*corringa-1*/
            FIND FIRST tt-campos-var
                WHERE tt-campos-var.campo-var = "corringa-1"
                  AND tt-campos-var.titulo    = "QTDE " + tt-reservas-sdo.cod-depos NO-ERROR.
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
                       tt-campos-var.titulo    = "QTDE " + tt-reservas-sdo.cod-depos
                       tt-campos-var.formato   = "->>>,>>>,>>9.9999"
                       tt-campos-var.tipo-dado = "Decimal".
            END.

            FIND FIRST tt-campos-var-id 
                WHERE tt-campos-var-id.id-linha  = tt-almoxarifado.id-linha
                  AND tt-campos-var-id.campo-var = tt-campos-var.campo-var
                  AND tt-campos-var-id.sequencia = tt-campos-var.sequencia    NO-ERROR.
            IF NOT AVAIL tt-campos-var-id THEN DO:
                CREATE tt-campos-var-id.
                ASSIGN tt-campos-var-id.id-linha  = tt-almoxarifado.id-linha
                       tt-campos-var-id.campo-var = tt-campos-var.campo-var  
                       tt-campos-var-id.sequencia = tt-campos-var.sequencia.
            END.
            ASSIGN tt-campos-var-id.valor = STRING(DEC(tt-campos-var-id.valor) + tt-reservas-sdo.qtidade-disp,tt-campos-var.formato).

            IF l-considera-dep AND tt-reservas-sdo.cod-localiz <> "" THEN DO:
                /*corringa-1*/
                FIND FIRST tt-campos-var
                    WHERE tt-campos-var.campo-var = "corringa-1"
                      AND tt-campos-var.titulo    = "LOCALIZAÄ«O " + tt-reservas-sdo.cod-depos NO-ERROR.
                IF AVAIL tt-campos-var THEN DO:
                    FIND FIRST tt-campos-var-id 
                        WHERE tt-campos-var-id.id-linha  = tt-almoxarifado.id-linha
                          AND tt-campos-var-id.campo-var = tt-campos-var.campo-var
                          AND tt-campos-var-id.sequencia = tt-campos-var.sequencia    NO-ERROR.
                    IF NOT AVAIL tt-campos-var-id THEN DO:
                        CREATE tt-campos-var-id.
                        ASSIGN tt-campos-var-id.id-linha  = tt-almoxarifado.id-linha
                               tt-campos-var-id.campo-var = tt-campos-var.campo-var  
                               tt-campos-var-id.sequencia = tt-campos-var.sequencia.
                    END.
                    ASSIGN tt-campos-var-id.valor = tt-campos-var-id.valor + (IF tt-campos-var-id.valor = "" THEN "" ELSE "|") + tt-reservas-sdo.cod-localiz.
                END.
            END.
        END.
    END.
    
    RUN utp\esutapi004b.p (INPUT        ?,                                                                /*h-browse       */
                           INPUT        TEMP-TABLE tt-almoxarifado:DEFAULT-BUFFER-HANDLE,                 /*tt-dados-buffer*/
                           INPUT        "",                                                               /*p-cod-programa */
                           INPUT-OUTPUT c-arquivo,                                                        /*c-arquivo      */
                           INPUT-OUTPUT c-formato,                                                        /*c-formato      */
                           INPUT-OUTPUT c-delim,                                                          /*c-delim        */
                           INPUT        YES,                                                              /*p-cabec        */
                           INPUT        NO,                                                               /*p-browse       */
                           INPUT        "for each tt-almoxarifado",                                       /*p-query        */
                           INPUT        "",                                                               /*Lista de campos que serao exportados*/
                           INPUT        c-lista-campos,                                                   /*Lista de campos que devem ser desconsiderados*/
                           INPUT        "corringa-1",                                                     /*Lista de Campos Variaveis*/
                           INPUT        NO,                                                               /*Lista formato dos campos*/
                           INPUT        2).                                                               /*Formato Extendido 1 com [], 2 sem []*/
                                                                                                        
    CREATE tt-paginas.
    ASSIGN tt-paginas.pagina      = p-pagina
           tt-paginas.nome-pagina = "Almox"
           tt-paginas.arquivo     = c-arquivo
           tt-paginas.formato     = c-formato
           tt-paginas.delimitador = c-delim.

    /*Campo Localizaá‰es*/
    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = p-pagina
           tt-paginas-ext.sequencia      = 1
           tt-paginas-ext.comando        = "REPLACE"
           tt-paginas-ext.parametros[1]  = "Localizaá‰es"
           tt-paginas-ext.parametros[2]  = "|"
           tt-paginas-ext.parametros[3]  = CHR(10).

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = p-pagina
           tt-paginas-ext.sequencia      = 2
           tt-paginas-ext.comando        = "AjustaTamColuna"
           tt-paginas-ext.parametros[1]  = "Localizaá‰es"
           tt-paginas-ext.parametros[2]  = "118".

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = p-pagina
           tt-paginas-ext.sequencia      = 3
           tt-paginas-ext.comando        = "WrapText"
           tt-paginas-ext.parametros[1]  = "Localizaá‰es".

    CREATE tt-paginas-ext.
    ASSIGN tt-paginas-ext.pagina         = p-pagina
           tt-paginas-ext.sequencia      = 4
           tt-paginas-ext.comando        = "Font"
           tt-paginas-ext.parametros[1]  = "Localizaá‰es"
           tt-paginas-ext.parametros[2]  = "Courier".

    RETURN "OK".
END.

PROCEDURE piCorrigeGUT:
    DEF VAR i-gut        AS INT    NO-UNDO.
    DEF VAR i-prioridade AS INT    NO-UNDO.

    DEF BUFFER bf-ord-manut FOR ord-manut.

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

    RUN pi-inicializar IN h-acomp (INPUT 'Corrigindo GUT').

    ASSIGN i-acomp = 0.

    FOR EACH ext-estabelec-man NO-LOCK:
        IF SUBSTRING(ext-estabelec-man.char-2,1,1) = "S" THEN DO:
            FOR EACH ord-manut NO-LOCK USE-INDEX estabel
                WHERE ord-manut.cod-estabel = ext-estabelec-man.cod-estabel:

                ASSIGN i-acomp = i-acomp + 1.
                IF i-acomp MODULO 100 = 0 THEN
                    run pi-acompanhar in h-acomp (input "Corrigindo Ordem " + STRING(i-acomp)).

                FIND FIRST ext-gut NO-LOCK
                    WHERE ext-gut.nr-ord-produ = ord-manut.nr-ord-produ
                      AND ext-gut.cd-equipto   = ""
                      AND ext-gut.cd-manut     = ""                     NO-ERROR.
                IF NOT AVAIL ext-gut THEN NEXT.
                
                /*
                GUT	PRIORIZAÄ«O
                Resultado entre 100 e 125	100
                Resultado entre 75 e 100	200
                Resultado entre 50 e 75	    300
                Resultado entre 25 e 50	    400
                Resultado entre 0 e 25	    500
                */
                ASSIGN i-gut = ext-gut.gravidade *
                               ext-gut.urgencia  *
                               ext-gut.tendencia.

                ASSIGN i-prioridade = ?.
                IF i-gut  >= 100 AND i-prioridade = ? THEN ASSIGN i-prioridade = 100.
                IF i-gut  >=  75 AND i-prioridade = ? THEN ASSIGN i-prioridade = 200.
                IF i-gut  >=  50 AND i-prioridade = ? THEN ASSIGN i-prioridade = 300.
                IF i-gut  >=  25 AND i-prioridade = ? THEN ASSIGN i-prioridade = 400.
                IF i-gut  >    0 AND i-prioridade = ? THEN ASSIGN i-prioridade = 500.

                IF ord-manut.prioridade <> i-prioridade THEN DO:
                    FIND FIRST bf-ord-manut EXCLUSIVE-LOCK
                        WHERE bf-ord-manut.nr-ord-produ = ord-manut.nr-ord-produ NO-ERROR.
                    IF AVAIL bf-ord-manut THEN DO:

                        ASSIGN bf-ord-manut.prioridade = i-prioridade.

                        RELEASE bf-ord-manut.
                    END.
                END.
            END.
        END.
    END.

    RUN pi-finalizar IN h-acomp.

    RUN utp/ut-msgs.p("show",
                  input 27979,
                  input "Procedimento realizado com sucesso").
    RETURN "OK".
END.
