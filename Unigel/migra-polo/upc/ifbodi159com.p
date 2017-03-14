/*******************************************************************************
**  Programa : ifbodi159com.p
**  Objetivo : Atendimento Total do Pedido e Geraá∆o de outros pedidos
**  Analista : Rodrigo Lu°s Frîhlich - Vertice
**  Data     : 25/02/2011
**  Versao   : 2.04.00.001
*******************************************************************************/
    
/*WPA - Buffer*/
define buffer pd-vendor  for mgemp.pd-vendor.
define buffer if-ped-venda  for espmulti.if-ped-venda.   
define buffer if-estabelec  for espmulti.if-estabelec. 
define buffer if-natur-oper for espmulti.if-natur-oper.
define buffer if-natur-fam  for espmulti.if-natur-fam.
define buffer if-natur-item for espmulti.if-natur-item.
define buffer bb-ped-ent    for ped-ent.
define buffer bpd-estabelec for estabelec.
define buffer bpd1-estabelec for estabelec.
define buffer bf-pd-compl-pedido for pd-compl-pedido.


  define buffer buf-estabelec for estabelec .
  define buffer buf-emitente for emitente .
   
{include/i-prgvrs.i IFBODI159COM 2.06.00.000}

{include/i-epc200.i bodi159com}
{utp/ut-glob.i}

/*definicao de parametros */
DEF INPUT PARAM p-ind-event  AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

/* Definiá∆o Temp-tables ---*/
{upc/ifbodi159com.i}


/* Definiá∆o Variavies Locais ---*/
DEFINE VARIABLE gr-ped-venda      AS ROWID               NO-UNDO.
DEFINE VARIABLE h-bodi159com      AS HANDLE              NO-UNDO.
DEFINE VARIABLE bo-ped-venda      AS HANDLE              NO-UNDO.
DEFINE VARIABLE bo-ped-repre      AS HANDLE              NO-UNDO.
DEFINE VARIABLE h-bodi159sdf      AS HANDLE              NO-UNDO.
DEFINE VARIABLE bo-ped-item       AS HANDLE              NO-UNDO.
DEFINE VARIABLE bo-ped-ent        AS HANDLE              NO-UNDO.
DEFINE VARIABLE bo-cond-ped       AS HANDLE              NO-UNDO.
DEFINE VARIABLE bo-ped-antecip    AS HANDLE              NO-UNDO.
DEFINE VARIABLE bo-ped-item-sdf   AS HANDLE              NO-UNDO.
DEFINE VARIABLE h-bosc014         AS HANDLE              NO-UNDO.
DEFINE VARIABLE c-aux-nome-transp AS CHAR FORMAT "x(12)" NO-UNDO.
DEFINE VARIABLE c-aux-cod-rota    AS CHAR FORMAT "x(12)" NO-UNDO.
DEFINE VARIABLE c-nome-abrev      AS CHARACTER           NO-UNDO.
DEFINE VARIABLE nr-ped-venda      AS CHARACTER           NO-UNDO.
DEFINE VARIABLE i-cont            AS INTEGER             NO-UNDO.
DEFINE VARIABLE i-nr-pedido       AS INTEGER             NO-UNDO.
DEFINE VARIABLE i-nr-pedido-next  AS INTEGER             NO-UNDO.
DEFINE VARIABLE i-campos-chave    AS INTEGER             NO-UNDO.
DEFINE VARIABLE i-cod-servico     AS INTEGER             NO-UNDO.
DEFINE VARIABLE l-criou           AS LOGICAL             NO-UNDO.
DEFINE VARIABLE c-nr-pedcli       AS CHARACTER           NO-UNDO.
DEFINE VARIABLE c-lst-ped         AS CHARACTER           NO-UNDO.
DEFINE VARIABLE i-cont-ped        AS INTEGER             NO-UNDO.
DEFINE VARIABLE i-cod-emit-orig   AS INTEGER             NO-UNDO.
DEFINE VARIABLE l-erro            AS LOGICAL             NO-UNDO.
DEFINE VARIABLE c-type-error      AS CHARACTER           NO-UNDO.
DEFINE VARIABLE l-it-n-if         AS LOGICAL             NO-UNDO. /* N∆o tem Incentivo Fiscal */
DEFINE VARIABLE l-it-t-if         AS LOGICAL             NO-UNDO. /* Tem Incentivo Fiscal */
DEFINE VARIABLE c-nome-abrev-orig AS CHARACTER           NO-UNDO.
DEFINE VARIABLE row-ped-item      AS ROW                 NO-UNDO.
DEFINE VARIABLE bo-ped-item-cal   AS HANDLE              NO-UNDO.
DEFINE VARIABLE i-nr-estrutura    AS INTEGER             NO-UNDO.
DEFINE VARIABLE c-cod-estab-dest  like if-estabelec.cod-estab-dest      NO-UNDO.

 
DEF BUFFER b-if-ped-venda   FOR espmulti.if-ped-venda. /*WPA - Buffer*/
DEF BUFFER b-estabelec      FOR estabelec.
DEF BUFFER b-ped-venda      FOR ped-venda.
DEF BUFFER b3-ped-venda      FOR ped-venda.
DEF BUFFER b2-ped-venda     FOR ped-venda.
DEF BUFFER b-ped-antecip    FOR ped-antecip.
DEF BUFFER b-ped-repre      FOR ped-repre.
DEF BUFFER b-ped-item       FOR ped-item.
DEF BUFFER b-ped-ent        FOR ped-ent.
DEF BUFFER b-cond-ped       FOR cond-ped.




/*
output to v:\temp\eventos.txt append.

   FOR each tt-epc 
        WHERE tt-epc.cod-event     = p-ind-event 
          AND tt-epc.cod-parameter = "Table-Rowid" NO-LOCK:


put unformatted
p-ind-event skip
tt-epc.cod-parameter
skip
" . " skip.

end.


output close.
*/

if index(  program-name(1) +  program-name(2) +  program-name(3) +  program-name(4) + 
           program-name(5) +  program-name(6) +  program-name(7) +  program-name(8) +
           program-name(9) +  program-name(10) , "pi-atualiza-pai") > 0 then return "ok".


IF p-ind-event = "beforeCompleteOrder" THEN DO:  

    FOR FIRST tt-epc 
        WHERE tt-epc.cod-event = p-ind-event NO-LOCK:
        ASSIGN h-bodi159com = WIDGET-HANDLE(tt-epc.val-parameter).
    END.

    FOR FIRST tt-epc 
        WHERE tt-epc.cod-event     = p-ind-event 
          AND tt-epc.cod-parameter = "Table-Rowid" NO-LOCK:

        ASSIGN gr-ped-venda = TO-ROWID(tt-epc.val-parameter).

        RUN InicializaHandles.

        EMPTY TEMP-TABLE tt-ped-comp-ava.
                       

        ASSIGN l-erro = NO.
        DO TRANS:
            FOR FIRST ped-venda EXCLUSIVE-LOCK 
                WHERE ROWID(ped-venda) = gr-ped-venda:
                
                
                
                FIND FIRST if-ped-venda exclusive-lock
                     WHERE if-ped-venda.nr-pedido  = ped-venda.nr-pedido and if-ped-venda.nr-pedido-relac <> 0  NO-ERROR.
                if avail if-ped-venda then do:                                                
                            
                         find first b-ped-venda where b-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac no-error.
                         if not avail b-ped-venda then if-ped-venda.nr-pedido-relac = 0.                                                   
                                                 
                end.
                
                IF  CAN-FIND(FIRST ped-item of ped-venda where ped-item.cod-refer = "00000001" and ped-item.ind-componen < 3) THEN DO:
                    RUN _insertErrorManual IN h-bodi159com (INPUT "2",
                                                            INPUT "EMS":U,
                                                            INPUT "ERROR":U,
                                                            INPUT "N∆o pode efetivar pedido com referància 00000001",
                                                            INPUT "Por favor Volte e termine a configuraá∆o",
                                                            INPUT "":U).
                    ASSIGN l-erro = YES.
                    UNDO,LEAVE.
                END.
                
                IF (ped-venda.cod-estabel = "422" OR ped-venda.cod-estabel = "412") and     /*solic-318*/  /*forcei mas precisa ver para outras empresa quando faz opia de pedido relecioando para comecar  processo*/
                   ped-venda.cond-redespa BEGINS "Pedido original relacionado:" THEN
                   ped-venda.cond-redespa = "".
                    
                IF ped-venda.cond-redespa BEGINS "Pedido original relacionado:" THEN do:
                     FIND FIRST if-ped-venda exclusive-lock
                     WHERE if-ped-venda.nr-pedido-relac  = ped-venda.nr-pedido   NO-ERROR.
                     if avail if-ped-venda then do:
                     
                         if-ped-venda.cod-estab-atend = ped-venda.cod-estabel.
                         
                      find first b-ped-venda where b-ped-venda.nr-pedido = if-ped-venda.nr-pedido and (b-ped-venda.cod-estabel = "422" OR b-ped-venda.cod-estabel = "412" ) AND   /*solic-318*/ 
                                                   b-ped-venda.cod-sit-ped < 3 exclusive-lock no-error.
                                                   
                                                 
                      
                      if avail b-ped-venda then run pi-atualiza-pai.
                     
                     end.                  
                   
                    LEAVE.
                END.
                RUN pi-verifica-if-nor.

                IF RETURN-VALUE <> "OK" THEN DO:
                    ASSIGN l-erro = YES.
                    UNDO,LEAVE.
                END.
                ELSE IF l-it-n-if = YES AND l-it-t-if = NO THEN DO: /* Pedido sem incentivo fiscal */
                    UNDO,LEAVE.
                END.

                FIND FIRST if-ped-venda EXCLUSIVE-LOCK
                     WHERE if-ped-venda.nr-pedido         = ped-venda.nr-pedido 
                       NO-ERROR.
       
         
                IF not AVAIL if-ped-venda and (ped-venda.cod-estabel = "422" OR ped-venda.cod-estabel = "412") THEN do:   /*solic-318*/ 
 
                  leave.  /* n∆o cria relacionamento para 422 com unidade atendimento branca*/
                end.
                
                FIND FIRST if-ped-venda EXCLUSIVE-LOCK
                     WHERE if-ped-venda.nr-pedido         = ped-venda.nr-pedido 
                       AND if-ped-venda.nr-pedido-relac <> 0 NO-ERROR.

                IF AVAIL if-ped-venda THEN DO:
                
                   find first b-ped-venda where b-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac no-lock no-error.
                   if avail b-ped-venda  then do:
                   
                        if if-ped-venda.cod-estab-atend <> "" and if-ped-venda.cod-estab-atend <> b-ped-venda.cod-estabel  then 
                              if-ped-venda.cod-estab-atend = b-ped-venda.cod-estabel.
                              
                         find first buf-estabelec where buf-estabelec.cod-estabel = b-ped-venda.cod-estabel no-lock no-error.                       
                         find first buf-emitente  where buf-emitente.cod-emitente = buf-estabelec.cod-emitente no-lock no-error.
                     
           
     
                        
                        if avail buf-emitente and ped-venda.cod-estabel <> "422" AND ped-venda.cod-estabel <> "412" and (buf-emitente.nome-abrev <> ped-venda.nome-abrev or    /*solic-318*/ 
                                                                                      buf-emitente.cod-emitente <> ped-venda.cod-emitente )  then do:
                              run pi-ajusta-nome-abrev.
                        end.   
                   end.
                
                
                
                
                    DO WHILE AVAIL if-ped-venda:
                        FIND FIRST b-ped-venda EXCLUSIVE-LOCK
                             WHERE b-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac NO-ERROR.

                        RUN pi-atualiz-ped-relac.

                        FIND FIRST if-ped-venda NO-LOCK
                             WHERE if-ped-venda.nr-pedido = b-ped-venda.nr-pedido NO-ERROR.
                    END.
				  
					RUN pi-aprova. /* aprovaá∆o crÇdito - WPA - 20121017 */
                END.
                ELSE DO:
                    /* Verifica e Cria tabela de extensao para relacionamento de pedidos */
                    IF NOT CAN-FIND(FIRST if-ped-venda NO-LOCK
                                    WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido) THEN DO:
                        RUN pi-cria-if.
                    END.

                    /* Criaá∆o dos demais pedidos relacionados */
                    FOR FIRST if-ped-venda EXCLUSIVE-LOCK
                        WHERE if-ped-venda.nr-pedido        = ped-venda.nr-pedido
                          AND if-ped-venda.cod-estab-atend <> "",
                        FIRST if-estabelec
                        WHERE if-estabelec.cod-estab-orig = ped-venda.cod-estabel
                          AND if-estabelec.cod-estab-dest = if-ped-venda.cod-estab-atend:
                         
                         
                         
                        /* Carrega Ped-Ent */
                        EMPTY TEMP-TABLE tt-ped-ent-aux.
                        FOR EACH ped-item OF ped-venda NO-LOCK,
                            EACH ped-ent  OF ped-item  NO-LOCK:
                            CREATE tt-ped-ent-aux.
                            BUFFER-COPY ped-ent TO tt-ped-ent-aux.
                        END.
                         
 
                        FIND FIRST param-global NO-LOCK NO-ERROR.
            
                        /* se o estabelecimento for de outra base alterar o webservice */
        
                        FIND FIRST if-natur-oper OF if-estabelec
                             WHERE if-natur-oper.nat-oper-pedido = ped-venda.nat-operacao NO-LOCK NO-ERROR.
                        IF NOT AVAIL if-natur-oper THEN DO:
                            RUN _insertErrorManual IN h-bodi159com (INPUT "2",
                                                                    INPUT "EMS":U,
                                                                    INPUT "ERROR":U,
                                                                    INPUT "Natureza(s) de Operaá∆o n∆o cadastradas.",
                                                                    INPUT "Verifique o programa IF001",
                                                                    INPUT "":U).
                            ASSIGN l-erro = YES.
                            UNDO,LEAVE.
                        END.
                        
                         if if-ped-venda.nat-oper-orig = "" then  if-ped-venda.nat-oper-orig = ped-venda.nat-operacao.
                         
        
                        /* Altera o cliente do pedido original */
                        ASSIGN i-cod-emit-orig = ped-venda.cod-emitente.

                        /***** Pedidos Intermedi†rios *****/
                        IF if-estabelec.cod-estab-inter <> "" THEN DO:
                            RUN pi-carga-tt   (INPUT if-estabelec.cod-estab-inter).
                            RUN pi-alt-cli-tt (INPUT if-estabelec.cod-estab-dest, INPUT YES).
        
                            RUN pi-executa.
    
                            IF if-ped-venda.nr-pedido-relac = 0 THEN
                                ASSIGN if-ped-venda.nr-pedido-relac = i-nr-pedido-next.
            
                            IF AVAIL b-if-ped-venda THEN
                                ASSIGN b-if-ped-venda.nr-pedido-relac = i-nr-pedido-next.
            
                            CREATE b-if-ped-venda.
                            BUFFER-COPY if-ped-venda EXCEPT nr-pedcli nr-pedido nr-pedido-relac TO b-if-ped-venda.
                            ASSIGN b-if-ped-venda.nr-pedcli       = c-nr-pedcli
                                   b-if-ped-venda.nr-pedido       = i-nr-pedido-next.
                        END.
                        /**********************************/

                        IF NOT CAN-FIND(FIRST tt-erros-geral-aux
                                        WHERE tt-erros-geral-aux.cod-maq-origem = 2) THEN DO:
        
                            /* Altera cliente pedido origem */
                            IF if-estabelec.cod-estab-inter = "" THEN DO:
                                c-cod-estab-dest = if if-estabelec.cod-estab-orig = "412" then "442" else if if-estabelec.cod-estab-orig = "422" then "434" else  if-estabelec.cod-estab-dest.  /*solic-318*/  /* regra fixa porque para 422 TUDO tem de passar pela UNG-RS */
                                
                                RUN pi-alt-cliente (INPUT c-cod-estab-dest, INPUT NO).
                            END.
                            ELSE DO:
                                RUN pi-alt-cliente (INPUT if-estabelec.cod-estab-inter, INPUT NO).
                            END.
            
                            /***** Pedidos Unidade Atend  *****/
                            RUN pi-carga-tt   (INPUT if-estabelec.cod-estab-dest).
                            RUN pi-alt-cli-tt (INPUT if-estabelec.cod-estab-dest, INPUT NO).
                            RUN pi-executa.
                
                            IF if-ped-venda.nr-pedido-relac = 0 THEN
                                ASSIGN if-ped-venda.nr-pedido-relac = i-nr-pedido-next.
                
                            IF AVAIL b-if-ped-venda THEN
                                ASSIGN b-if-ped-venda.nr-pedido-relac = i-nr-pedido-next.
                            /**********************************/
                
                            ASSIGN ped-venda.cond-redespa   = "Pedido(s) relacionado(s): " + c-lst-ped
                                   ped-venda.cidade-cif     = ""
                                   ped-venda.nome-abrev-tri = "" 
                                   ped-venda.nome-transp    = ""
                                   ped-venda.nome-tr-red    = ""
                                   /*ped-venda.observacoes    = ""*/
                                   ped-venda.cod-mensagem   = 0
                                   OVERLAY(ped-venda.char-2,109,8) = "9".
                            /* rotina para criar complemento de pedido para o novo pedido  EDSON 13112012*/  
                            
                             IF if-ped-venda.nr-pedido-relac <> 0 THEN do:
                                find first bpd-estabelec where bpd-estabelec.cod-estabel = ped-venda.cod-estabel no-lock no-error.
                                find first bpd1-estabelec where bpd1-estabelec.cod-estabel =  if-ped-venda.cod-estab-atend no-lock no-error.
                                
                                FOR EACH ped-item OF ped-venda  where ped-item.ind-componen <> 3 no-lock.
                                   
                                   find first pd-compl-pedido where    
                                                              pd-compl-pedido.ep-codigo    = bpd-estabelec.ep-codigo and
                                                              pd-compl-pedido.nr-pedido    = ped-venda.nr-pedido and
                                                              pd-compl-pedido.nr-sequencia = ped-item.nr-sequencia no-lock no-error.
                                                              
                                   if avail pd-compl-pedido then do:
                                   
                                      find first bf-pd-compl-pedido where 
                                                              bf-pd-compl-pedido.ep-codigo    = bpd1-estabelec.ep-codigo and
                                                              bf-pd-compl-pedido.nr-pedido    = if-ped-venda.nr-pedido-relac and
                                                              bf-pd-compl-pedido.nr-sequencia = ped-item.nr-sequencia no-lock no-error.
                                                              
                                                              
                                      if not avail  bf-pd-compl-pedido then 
                                                  create bf-pd-compl-pedido.        
                                      buffer-copy pd-compl-pedido  except nr-pedido ep-codigo to bf-pd-compl-pedido
                                                assign bf-pd-compl-pedido.ep-codigo = bpd1-estabelec.ep-codigo
                                                       bf-pd-compl-pedido.nr-pedido = if-ped-venda.nr-pedido-relac.

                                   end.                             
                                                              
                                END.

                             
                             
                             end.  /* fim da rotina de criar complemento de pedido par ao novo pedido*/
                             

                            /* Atualiza com a mensagem da natureza */
                            FIND FIRST natur-oper NO-LOCK
                                 WHERE natur-oper.nat-operacao = ped-venda.nat-operacao NO-ERROR.
                            IF AVAIL natur-oper THEN DO:
                                ASSIGN ped-venda.cod-mensagem = natur-oper.cod-mensagem.

                                FOR EACH ped-item OF ped-venda EXCLUSIVE-LOCK:
                                    ASSIGN ped-item.ind-icm-ret  = natur-oper.subs-trib
                                           ped-item.per-des-icms = natur-oper.per-des-icms.
                                END.

                                
                            END.
                                
    				  
                            RUN pi-aprova. /* aprovaá∆o crÇdito */
                        END.

                        /* Aplica o percentual de desconto */
                        /*Comentado por WPA - DLCTec - 01/08/2012*/
                        /*FOR EACH ped-item OF ped-venda EXCLUSIVE-LOCK:
                            /* ASSIGN ped-item.des-pct-desconto-inform = trim(string(if-estabelec.perc-reduc,">,>>>,>>>,>>9.99")). */

                            ASSIGN ped-item.vl-preori = ped-item.vl-preori - ((ped-item.vl-preori * if-estabelec.perc-reduc) / 100)
                                   ped-item.vl-preuni = ped-item.vl-preori
                                   ped-item.vl-preori-un-fat = ped-item.vl-preori.

                            /* ASSIGN ped-item.val-desconto-inform = (ped-item.vl-preuni * if-estabelec.perc-reduc) / 100. */
                        END.*/
                        /*&*/                        
                    END. /* for first if-ped-venda */
                END.
            END. /* for first ped-venda */
    
            /* Inclui os erros ocorrido na bo do produto */
            FOR EACH tt-erros-geral-aux:

                ASSIGN c-type-error = IF tt-erros-geral-aux.cod-maq-origem = 1 THEN "WARNING" ELSE "ERROR".

                RUN _insertErrorManual IN h-bodi159com (INPUT tt-erros-geral-aux.cod-erro,
                                                        INPUT "EMS":U,
                                                        INPUT c-type-error,
                                                        INPUT tt-erros-geral-aux.des-erro,
                                                        INPUT "",
                                                        INPUT "":U).
            END.
            
            IF CAN-FIND(FIRST tt-erros-geral-aux
                        WHERE tt-erros-geral-aux.cod-maq-origem = 2) THEN DO:
                ASSIGN l-erro = YES.
                UNDO,LEAVE.
            END.
        END.
                
        RUN pi-completa-ped. /* Completa o Pedido */
        
            
        IF l-erro THEN DO:
            RETURN "NOK". 
        END.
        
        /* forcando aprovacao no pedido da unigel comercial Edson*/
        
         
          FOR FIRST ped-venda EXCLUSIVE-LOCK 
                WHERE ROWID(ped-venda) = gr-ped-venda and
                ped-venda.cod-sit-aval <> 3:

                FIND FIRST if-ped-venda EXCLUSIVE-LOCK
                     WHERE if-ped-venda.nr-pedido         = ped-venda.nr-pedido
                       AND if-ped-venda.nr-pedido-relac  <> 0 NO-ERROR.
                  
                       
                IF AVAIL if-ped-venda THEN DO:
	             RUN pi-aprova. /* aprovaá∆o crÇdito - WPA - 20121017 */
                END.
          end.
        

        RUN pi-elimina-handle.

        FOR EACH tt-ped-ent-aux NO-LOCK:
        
            /* copia com teste de j† existància EDSON-AMGRA*/

            if not can-find(first bb-ped-ent
                        WHERE bb-ped-ent.nome-abrev   = c-nome-abrev-orig  
                          AND bb-ped-ent.nr-pedcli    = tt-ped-ent-aux.nr-pedcli   
                          AND bb-ped-ent.nr-sequencia = tt-ped-ent-aux.nr-sequencia
                          AND bb-ped-ent.it-codigo    = tt-ped-ent-aux.it-codigo   
                          AND bb-ped-ent.cod-refer    = tt-ped-ent-aux.cod-refer no-lock) then do:
               
               CREATE ped-ent.
               BUFFER-COPY tt-ped-ent-aux EXCEPT nome-abrev TO ped-ent.
               ASSIGN ped-ent.nome-abrev = c-nome-abrev-orig.  
            end.   
        END.
    END. /* FOR FIRST tt-epc*/
END.

IF p-ind-event = "afterCompleteOrder" THEN DO:

    FOR FIRST tt-epc 
        WHERE tt-epc.cod-event = p-ind-event NO-LOCK:
        ASSIGN h-bodi159com = WIDGET-HANDLE(tt-epc.val-parameter).
    END.

    FOR FIRST tt-epc 
        WHERE tt-epc.cod-event     = p-ind-event 
          AND tt-epc.cod-parameter = "Table-Rowid" NO-LOCK:

        ASSIGN gr-ped-venda = TO-ROWID(tt-epc.val-parameter).

        
        FOR FIRST ped-venda EXCLUSIVE-LOCK 
            WHERE ROWID(ped-venda) = gr-ped-venda,
            FIRST if-ped-venda NO-LOCK
            WHERE if-ped-venda.nr-pedido        = ped-venda.nr-pedido
              AND if-ped-venda.nr-pedido-relac <> 0:

            /* Coloca portador do emitente */
            FIND FIRST emitente
                 WHERE emitente.cod-emitente = ped-venda.cod-emitente NO-LOCK NO-ERROR.
            IF AVAIL emitente THEN
                ASSIGN ped-venda.cod-portador = emitente.portador
                       ped-venda.modalidade   = emitente.modalidade.
        END.
    END.
END.

RETURN "OK".

/*upc/ifbodi159com.p*/

PROCEDURE pi-carga-tt:    
/*------------------------------------------------------------------------------
  Purpose:     Executa Carga nas temp-tables
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM p-cod-estab AS CHARACTER NO-UNDO.

    /* tt-ped-venda */
    CREATE tt-ped-venda.
    BUFFER-COPY ped-venda TO tt-ped-venda.

    ASSIGN i-cont-ped = i-cont-ped + 1.

    /* busca no numero do pedido de venda , caso nao tenha sido informado */    
    RUN setDefaultOrderNumber IN h-bodi159sdf (OUTPUT i-nr-pedido-next).
    ASSIGN c-nr-pedcli = TRIM(ped-venda.nr-pedcli)
           /* c-nr-pedcli = TRIM(ped-venda.nr-pedcli + "/" + TRIM(STRING(i-cont-ped))) */
           c-lst-ped   = c-lst-ped + TRIM(STRING(i-nr-pedido-next)) + ","
           tt-ped-venda.nr-pedcli    = c-nr-pedcli
           tt-ped-venda.nr-pedido    = i-nr-pedido-next
           tt-ped-venda.cod-estabel  = p-cod-estab
           tt-ped-venda.cond-redespa = "Pedido original relacionado: " + TRIM(STRING(ped-venda.nr-pedcli)).

    /* tt-ped-antecip */
    FIND FIRST ped-antecip NO-LOCK
         WHERE ped-antecip.nr-pedido = ped-venda.nr-pedido NO-ERROR.
    IF AVAIL ped-antecip THEN DO:
        CREATE tt-ped-antecip.
        BUFFER-COPY ped-antecip TO tt-ped-antecip.
        ASSIGN tt-ped-antecip.nr-pedido = tt-ped-venda.nr-pedido.
    END.

    /* tt-ped-repre */
    /*FOR EACH ped-repre NO-LOCK
       WHERE ped-repre.nr-pedido = ped-venda.nr-pedido:
        CREATE tt-ped-repre.
        BUFFER-COPY ped-repre TO tt-ped-repre.
        ASSIGN tt-ped-repre.nr-pedido = tt-ped-venda.nr-pedido.
    END.*/

    /* tt-pd-vendor */
    FIND FIRST pd-vendor NO-LOCK
         WHERE pd-vendor.nr-pedido = ped-venda.nr-pedido NO-ERROR.
    IF AVAIL pd-vendor THEN DO:
        CREATE tt-pd-vendor.
        BUFFER-COPY pd-vendor TO tt-pd-vendor.
        ASSIGN tt-pd-vendor.nr-pedido = tt-ped-venda.nr-pedido.
    END.

    /* tt-ped-item */
    FOR EACH ped-item OF ped-venda NO-LOCK:
        CREATE tt-ped-item.
        BUFFER-COPY ped-item TO tt-ped-item.
        ASSIGN tt-ped-item.nr-pedcli = tt-ped-venda.nr-pedcli.

        /* tt-ped-ent */
        FOR EACH ped-ent OF ped-item NO-LOCK:
            CREATE tt-ped-ent.
            BUFFER-COPY ped-ent TO tt-ped-ent.
            ASSIGN tt-ped-ent.nr-pedcli = tt-ped-venda.nr-pedcli.
        END.
    END.

    /* tt-con-pges */
    FOR EACH cond-ped OF ped-venda NO-LOCK:
        CREATE tt-con-pges.
        BUFFER-COPY cond-ped TO tt-con-pges.
        ASSIGN tt-con-pges.nr-pedido = tt-ped-venda.nr-pedido.
    END.

END PROCEDURE. /* pi-carga-tt */

PROCEDURE pi-executa:    
/*------------------------------------------------------------------------------
  Purpose:     Gera Pedidos
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF  NOT VALID-HANDLE(bo-ped-venda) OR 
        bo-ped-venda:TYPE       <> "PROCEDURE":U OR 
        bo-ped-venda:FILE-NAME  <> "dibo/bodi159.p":U THEN DO:           
        RUN dibo/bodi159.p PERSISTENT SET bo-ped-venda.
        /*RUN openQueryStatic IN bo-ped-venda(INPUT "Default":U).*/
        RUN openQueryStatic IN bo-ped-venda(INPUT "Defaultpd4000":U).
    END.

    blocoPedidos:
    DO :

        FIND FIRST tt-ped-venda.

        ASSIGN i-cont = 0.  

        IF CAN-FIND(FIRST tt-ped-antecip) THEN 
           ASSIGN tt-ped-venda.ind-antecip = YES.

        IF i-cont > 1 THEN DO:
           /*"Mais de um Representante Indireto informado" */
           RUN utp/ut-msgs.p ('msg', 5889, '').
           CREATE tt-erros-geral.
           ASSIGN tt-erros-geral.identif-msg    = tt-ped-venda.nome-abrev + CHR(24) + tt-ped-venda.nr-pedcli
                  tt-erros-geral.cod-erro       = 5889
                  tt-erros-geral.des-erro       = RETURN-VALUE
                  tt-erros-geral.cod-maq-origem = 2.
        END.
        ELSE DO:          
           
           IF  NOT VALID-HANDLE(bo-ped-repre) OR
               bo-ped-repre:TYPE <> "PROCEDURE":U OR 
               bo-ped-repre:FILE-NAME <> "dibo/bodi157.p":U THEN DO:
               RUN dibo/bodi157.p PERSISTENT SET bo-ped-repre.
               RUN openQueryStatic in bo-ped-repre(INPUT "Defaultpd4000":U).
           END.

           /* verifica representantes */
           RUN searchRepresentativesInImport IN bo-ped-repre(INPUT TABLE tt-ped-venda,
                                                             INPUT-OUTPUT TABLE tt-ped-repre).
           IF tt-ped-venda.no-ab-reppri = "" THEN DO:
              FIND FIRST tt-ped-repre WHERE 
                         tt-ped-repre.ind-repbase = YES NO-ERROR.
              IF AVAIL tt-ped-repre THEN
                 ASSIGN tt-ped-venda.no-ab-reppri = tt-ped-repre.nome-ab-rep.
           END.                

           CREATE RowPedVenda.
           BUFFER-COPY tt-ped-venda EXCEPT ind-tipo-movto nome-abrev cod-maq-origem num-processo num-sequencia TO RowPedVenda.

           ASSIGN RowPedVenda.nome-abrev = tt-ped-venda.nome-abrev
                  c-nome-abrev           = tt-ped-venda.nome-abrev.

           CREATE RowPedParam.

           FOR FIRST user-coml FIELDS(ind-utiliza-desconto usuario multiplica ind-qt-un-cli) NO-LOCK WHERE user-coml.usuario = c-seg-usuario :
               ASSIGN rowPedParam.multiplicar-qtde   = user-coml.multiplica.  
                      rowPedParam.qtde-un-medida-cli = IF  tt-ped-venda.esp-ped <> 1 THEN user-coml.ind-qt-un-cli ELSE rowPedParam.qtde-un-medida-cli.  
           END.

           FOR EACH tt-pd-vendor:
               CREATE RowPedVendor.
               ASSIGN RowPedVendor.data-base    = tt-pd-vendor.data-base
                      RowPedVendor.dias-base    = tt-pd-vendor.dias-base
                      RowPedVendor.cod-cond-pag = tt-pd-vendor.cod-cond-cli
                      RowPedVendor.taxa-cliente = tt-pd-vendor.taxa-cliente.
           END.

           /*** SETA INFORMAÄÂES ENTREGA ***/
           RUN inputTable IN h-bodi159sdf (INPUT TABLE rowPedVenda).
           RUN setDefaultDelivery IN h-bodi159sdf.
           RUN outputTable IN h-bodi159sdf (OUTPUT TABLE rowPedVenda).

           /*** FRETES ***/
           FIND FIRST rowPedVenda NO-ERROR.

           IF c-aux-nome-transp <> "" THEN
              ASSIGN rowPedVenda.nome-transp = c-aux-nome-transp .
           ELSE 
               IF rowPedVenda.nome-transp = "" THEN 
                  ASSIGN rowPedVenda.nome-transp  = tt-ped-venda.nome-transp .

           IF c-aux-cod-rota <> "" THEN
              ASSIGN rowPedVenda.cod-rota    = c-aux-cod-rota   .
           ELSE 
               IF rowPedVenda.cod-rota = "" THEN 
                  ASSIGN rowPedVenda.cod-rota     = tt-ped-venda.cod-rota   .

           &IF "{&mguni_version}" < "2.07" &THEN
               IF param-global.modulo-tf AND CONNECTED('mgscm') AND
                  AVAIL rowPedVenda AND
                  rowPedVenda.cidade-cif <> "":U AND
                  rowPedVenda.dec-1 = 0 THEN DO:
    
                     RUN pi-frete.
               END.
           &ENDIF
           
           RUN emptyRowErrors  IN bo-ped-venda.
           RUN inputRowParam   IN bo-ped-venda(INPUT TABLE RowPedParam).
           RUN inputRowVendor  IN bo-ped-venda(INPUT TABLE RowPedVendor).
           RUN setRecord       IN bo-ped-venda(INPUT TABLE RowPedVenda).
           RUN createRecord    IN bo-ped-venda.
           RUN getRowErrors    IN bo-ped-venda(OUTPUT TABLE RowErrors).
           RUN getIntField     IN bo-ped-venda(INPUT  "nr-pedido",
                                               OUTPUT i-nr-pedido).
                                               
                                    

           FOR EACH RowErrors WHERE
                    RowErrors.ErrorType <> "INTERNAL":U:
                    
                    
                    
                    
               CREATE tt-erros-geral.
               ASSIGN tt-erros-geral.identif-msg = tt-ped-venda.nome-abrev + CHR(24) + tt-ped-venda.nr-pedcli
                      tt-erros-geral.cod-erro    = RowErrors.ErrorNumber
                      tt-erros-geral.des-erro    = RowErrors.ErrorDescription + "/" + tt-ped-venda.nome-abrev + "/" + tt-ped-venda.nr-pedcli
                      tt-erros-geral.cod-maq-origem = IF RowErrors.ErrorSubType <> "ERROR" THEN 1 ELSE 2 .
               LEAVE blocoPedidos.
           END.

           IF  NOT CAN-FIND(FIRST RowErrors WHERE
                                  RowErrors.ErrorSubType  = "Error":U AND
                                  RowErrors.ErrorType    <> "INTERNAL":U) THEN DO:

               CREATE tt-ped-venda-rep.
               ASSIGN tt-ped-venda-rep.nome-abrev  = tt-ped-venda.nome-abrev
                      tt-ped-venda-rep.nr-pedcli   = tt-ped-venda.nr-pedcli
                      tt-ped-venda-rep.cod-estabel = tt-ped-venda.cod-estabel.

               /************* Itens ***************/

               IF  NOT VALID-HANDLE(bo-ped-item) OR
                   bo-ped-item:TYPE <> "PROCEDURE":U OR 
                   bo-ped-item:FILE-NAME <> "dibo/bodi154.p":U THEN DO:           
                   RUN dibo/bodi154.p PERSISTENT SET bo-ped-item.
                   RUN openQueryStatic in bo-ped-item(INPUT "Defaultpd4000":U).
               END.
               
               /*------ rotina limpa componentes - Edson */
               for each tt-ped-item  WHERE  tt-ped-item.ind-compon = 3 :    /*S‡PARA N«O DELETAR*/
                   /* Caso seja componente de produto composto que jˇ tenha sido criado 
                      automaticamente pela BO, as informa?Ñes do TXT devem prevalecer */
                                                   
                   IF CAN-FIND(FIRST ped-item 
                               WHERE ped-item.nr-pedcli = tt-ped-item.nr-pedcli 
                                 AND ped-item.nome-abrev = tt-ped-item.nome-abrev 
                                 AND ped-item.it-codigo = tt-ped-item.it-codigo 
                                 AND ped-item.cod-refer = tt-ped-item.cod-refer
                                 AND ped-item.nr-sequencia = tt-ped-item.nr-sequencia ) THEN DO:
                       
                       RUN goToKey IN bo-ped-item (INPUT tt-ped-item.nome-abrev,
                                                   INPUT tt-ped-item.nr-pedcli,
                                                   INPUT tt-ped-item.nr-sequencia,
                                                   INPUT tt-ped-item.it-codigo,
                                                   INPUT tt-ped-item.cod-refer).
                       RUN emptyRowErrors  IN bo-ped-item.
                       RUN deleteRecord    IN bo-ped-item.
                       RUN getRowErrors    IN bo-ped-item(OUTPUT TABLE RowErrors).
                   END.
               END.  /* fim rotina componentes*/
               
               FOR EACH tt-ped-item  
                  BREAK BY tt-ped-item.nome-abrev
                        BY tt-ped-item.nr-pedcli
                        BY tt-ped-item.nr-sequencia
                        BY tt-ped-item.ind-componen :

                   if  tt-ped-item.ind-componen <> 3  then do:    
                       &IF "{&ems_dbtype}" <> "progress":U &THEN  
                            EMPTY TEMP-TABLE RowPedItem.
                       &endif
                       CREATE RowPedItem.
                       BUFFER-COPY tt-ped-item EXCEPT ind-tipo-movto nome-abrev cod-maq-origem num-processo num-sequencia TO RowPedItem.                                           
                       ASSIGN RowPedItem.nome-abrev = c-nome-abrev.

                       IF l-criou = NO THEN DO:
                           /* trata descontos dos itens */
                           IF  tt-ped-item.log-usa-tabela-desconto THEN 
                               RUN pi-TrataDesconto.    

                           /* tratamento de IVA */
                           IF  i-pais-impto-usuario <> 1
                           AND i-pais-impto-usuario <> 3 THEN
                               RUN pi-TrataImposto.

                           RUN emptyRowErrors  IN bo-ped-item.
                           RUN inputRowParam   IN bo-ped-item(INPUT TABLE RowPedParam).
                           RUN inputRowDescPedItem IN bo-ped-item(INPUT TABLE tt-desc-ped-item).
                           RUN setRecord       IN bo-ped-item(INPUT TABLE RowPedItem).
                           RUN createRecord    IN bo-ped-item.
                           RUN getRowErrors    IN bo-ped-item(OUTPUT TABLE RowErrors).
                           
                           FOR EACH tt-desc-ped-item:
                               DELETE tt-desc-ped-item.
                           END.
                       END.
                       ASSIGN l-criou = NO.

                       FOR EACH RowErrors
                          WHERE RowErrors.ErrorType <> "INTERNAL":U:
                           CREATE tt-erros-geral.
                           ASSIGN tt-erros-geral.identif-msg = RowPedItem.nome-abrev + CHR(24) + RowPedItem.nr-pedcli + CHR(24) + STRING(RowPedItem.nr-sequencia)
                                  tt-erros-geral.cod-erro    = RowErrors.ErrorNumber
                                  tt-erros-geral.des-erro    = RowErrors.ErrorDescription + CHR(24) + RowPedItem.nr-pedcli + CHR(24) + STRING(RowPedItem.nr-sequencia)
                                  tt-erros-geral.cod-maq-origem = IF RowErrors.ErrorSubType <> "ERROR" THEN 1 ELSE 2.
                       END.

                       /* rotina de criar componente de item configurado - Edson*/                   
                
                       i-nr-estrutura = tt-ped-item.nr-config .
                       
                       IF NOT CAN-FIND(FIRST tt-erros-geral) AND /*NAo achou erro na criacao*/
                          i-nr-estrutura <> 0      THEN DO:

                           FOR EACH ped-item OF ped-venda 
                              where ped-item.ind-componen >= 3 
                                and ped-item.nr-sequencia = tt-ped-item.nr-sequencia NO-LOCK:

                               CREATE b-ped-item.
                               BUFFER-COPY ped-item except nome-abrev TO b-ped-item.
                               ASSIGN b-ped-item.nome-abrev = c-nome-abrev.

                               /* b-ped-ent crianto componentes e ped-ent foráada*/
                               FOR EACH ped-ent OF ped-item  NO-LOCK:
                                   CREATE b-ped-ent.
                                   BUFFER-COPY ped-ent except nome-abrev TO b-ped-ent.
                                   ASSIGN b-ped-ent.nome-abrev = c-nome-abrev.
                               END.
                           END.
 
                        
                            /* vamos ver se funciona sem bo para estrutura
                            run getRowid in bo-ped-item(output row-ped-item).               
                            if not valid-handle(bo-ped-item-cal) or
                                 bo-ped-item-cal:type <> "PROCEDURE":U or
                                 bo-ped-item-cal:file-name <> "dibo/bodi154cal.p":U then
                                 run dibo/bodi154cal.p persistent set bo-ped-item-cal.                   
               
                            run setItemConfiguration in bo-ped-item-cal (input row-ped-item,
                                                                           input i-nr-estrutura).
                            delete procedure bo-ped-item-cal.
                            assign bo-ped-item-cal = ?.
                            */
                       END. 

                       run getRowErrors in bo-ped-item(output table RowErrors).
                  
                       FOR EACH RowErrors 
                          WHERE RowErrors.ErrorType <> "INTERNAL":U:
                           CREATE tt-erros-geral.
                           ASSIGN tt-erros-geral.identif-msg = RowPedItem.nome-abrev + CHR(24) + RowPedItem.nr-pedcli + CHR(24) + STRING(RowPedItem.nr-sequencia)
                                  tt-erros-geral.cod-erro    = RowErrors.ErrorNumber
                                  tt-erros-geral.des-erro    = RowErrors.ErrorDescription + CHR(24) + RowPedItem.nr-pedcli + CHR(24) + STRING(RowPedItem.nr-sequencia)
                                  tt-erros-geral.cod-maq-origem = IF RowErrors.ErrorSubType <> "ERROR" THEN 1 ELSE 2.
                       END.
                       /* fim da rotina de criar componente*/

                   end. /* if ind-componen = 3*/
                                      
                   FOR EACH tt-erros-geral:
                       
                       IF tt-erros-geral.cod-maq-origem <> 1 THEN DO:                         
                           IF AVAIL RowPedItem THEN
                              DELETE RowPedItem.
                          LEAVE blocoPedidos.
                       END.
                   END.
                   IF AVAIL RowPedItem THEN
                       DELETE RowPedItem.


                   /************ PROGRAMAÄ«O DE ENTREGAS E CONTRATO DE FORNECIMENTO ************/
                   IF tt-ped-venda.esp-ped <> 1 THEN DO:

                       /*
                       /*SE ENCONTRAR ENTREGAS, ELIMINAR A PED-ENT CRIADA, E CRIAR AS ENTREGAS 
                         UTILIZANDO A TEMP-TABLE QUE FOI PREVIAMENTE GERADA, CONFORME TXT*/
                       IF CAN-FIND(FIRST tt-ped-ent) THEN DO:
                          FIND FIRST tt-ped-ent WHERE
                                     tt-ped-ent.nome-abrev   = tt-ped-item.nome-abrev   and   
                                     tt-ped-ent.nr-pedcli    = tt-ped-item.nr-pedcli    and   
                                     tt-ped-ent.nr-sequencia = tt-ped-item.nr-sequencia and   
                                     tt-ped-ent.it-codigo    = tt-ped-item.it-codigo    and   
                                     tt-ped-ent.cod-refer    = tt-ped-item.cod-refer    and
                                     tt-ped-ent.nr-entrega   = 10    EXCLUSIVE-LOCK NO-ERROR.
                          IF AVAIL tt-ped-ent THEN DO:
                             FIND FIRST ped-ent WHERE
                                        ped-ent.nome-abrev   = tt-ped-ent.nome-abrev   and   
                                        ped-ent.nr-pedcli    = tt-ped-ent.nr-pedcli    and   
                                        ped-ent.nr-sequencia = tt-ped-ent.nr-sequencia and   
                                        ped-ent.it-codigo    = tt-ped-ent.it-codigo    and   
                                        ped-ent.cod-refer    = tt-ped-ent.cod-refer    and
                                        ped-ent.nr-entrega   = tt-ped-ent.nr-entrega   EXCLUSIVE-LOCK NO-ERROR.
                             IF AVAIL ped-ent THEN DO:
                                 DELETE ped-ent.
                             END.
                          END.
                       END.*/
                       /************* CRIAÄ«O ENTREGAS ************/

                       FOR EACH tt-ped-ent WHERE 
                                tt-ped-ent.nome-abrev   = tt-ped-item.nome-abrev   and     
                                tt-ped-ent.nr-pedcli    = tt-ped-item.nr-pedcli    and
                                tt-ped-ent.nr-sequencia = tt-ped-item.nr-sequencia and
                                tt-ped-ent.it-codigo    = tt-ped-item.it-codigo    and
                                tt-ped-ent.cod-refer    = tt-ped-item.cod-refer:    
                        

                             CREATE RowPedEnt.
                             BUFFER-COPY tt-ped-ent TO RowPedEnt.
                             ASSIGN RowPedEnt.nr-pedcli    = tt-ped-item.nr-pedcli.

                             RUN emptyRowErrors IN bo-ped-ent.
                             RUN inputRowParam  IN bo-ped-ent(INPUT TABLE RowPedParam).
                             RUN setRecord      IN bo-ped-ent(INPUT TABLE RowPedEnt).
                             RUN createRecord   IN bo-ped-ent.
                             RUN getRowErrors   IN bo-ped-ent(OUTPUT TABLE RowErrors).

                             DELETE tt-ped-ent.
                             DELETE RowPedEnt.

                             FOR EACH RowErrors WHERE
                                      RowErrors.ErrorType <> "INTERNAL":U:
                                 CREATE tt-erros-geral.
                                 ASSIGN tt-erros-geral.identif-msg = tt-ped-venda.nome-abrev + CHR(24) + tt-ped-venda.nr-pedcli + CHR(24) + "*"
                                        tt-erros-geral.cod-erro    = RowErrors.ErrorNumber
                                        tt-erros-geral.des-erro    = RowErrors.ErrorDescription + "/" + tt-ped-venda.nome-abrev + "/" + tt-ped-venda.nr-pedcli
                                                                     + CHR(24) + " PedEnt"
                                        tt-erros-geral.cod-maq-origem = IF RowErrors.ErrorSubType <> "ERROR" THEN 1 ELSE 2 .
                                 DELETE RowErrors.
                                 
                                 LEAVE blocoPedidos.
                             END.
                       END.
                   END.
               END.

               /* rotina de criar if-ped-item para XTRIM e calculo de desconto do Item*/                   
               IF NOT CAN-FIND(FIRST tt-erros-geral)  THEN DO:

                   FOR EACH ped-item OF ped-venda 
                      where ped-item.ind-componen <> 3 NO-LOCK:

                       FIND FIRST if-ped-item EXCLUSIVE-LOCK
                            WHERE if-ped-item.nome-abrev   = ped-item.nome-abrev  
                              AND if-ped-item.nr-pedcli    = ped-item.nr-pedcli   
                              AND if-ped-item.nr-sequencia = ped-item.nr-sequencia
                              AND if-ped-item.it-codigo    = ped-item.it-codigo   
                              AND if-ped-item.cod-refer    = ped-item.cod-refer NO-ERROR.
                       IF NOT AVAIL if-ped-item THEN DO:
                           CREATE if-ped-item.
                           ASSIGN if-ped-item.nome-abrev   = ped-item.nome-abrev  
                                  if-ped-item.nr-pedcli    = ped-item.nr-pedcli   
                                  if-ped-item.nr-sequencia = ped-item.nr-sequencia
                                  if-ped-item.it-codigo    = ped-item.it-codigo   
                                  if-ped-item.cod-refer    = ped-item.cod-refer.
                       END.

                       ASSIGN if-ped-item.vl-preori        = ped-item.vl-preori       
                              if-ped-item.vl-preori-un-fat = ped-item.vl-preori-un-fat
                              if-ped-item.vl-pretab        = ped-item.vl-pretab       
                              if-ped-item.vl-preuni        = ped-item.vl-preuni.
                   END.
               END.

               /************* Representante ***************/

               IF  NOT VALID-HANDLE(bo-ped-repre) OR
                   bo-ped-repre:TYPE <> "PROCEDURE":U OR 
                   bo-ped-repre:FILE-NAME <> "dibo/bodi157.p":U THEN DO:
                   RUN dibo/bodi157.p PERSISTENT SET bo-ped-repre.
                   RUN openQueryStatic IN bo-ped-repre(INPUT "Defaultpd4000":U).
               END.

               FOR EACH tt-ped-repre:

                   CREATE RowPedRepre.
                   BUFFER-COPY tt-ped-repre EXCEPT cod-maq-origem num-processo num-sequencia nr-pedcli
                                            TO RowPedRepre.

                   ASSIGN RowPedRepre.nr-pedido = i-nr-pedido.

                   RUN emptyRowErrors  IN bo-ped-repre.
                   RUN setRecord       IN bo-ped-repre(INPUT TABLE RowPedRepre).
                   RUN createRecord    IN bo-ped-repre.
                   RUN getRowErrors    IN bo-ped-repre(OUTPUT TABLE RowErrors).

                   DELETE RowPedRepre.

                   FOR EACH RowErrors WHERE
                            RowErrors.ErrorType <> "INTERNAL":U : 
                       CREATE tt-erros-geral.
                       ASSIGN tt-erros-geral.identif-msg = tt-ped-venda.nome-abrev + CHR(24) + tt-ped-venda.nr-pedcli + CHR(24) + "*"
                              tt-erros-geral.cod-erro    = RowErrors.ErrorNumber
                              tt-erros-geral.des-erro    = RowErrors.ErrorDescription + CHR(24) + tt-ped-venda.nr-pedcli + CHR(24) + " PedRepre"
                              tt-erros-geral.cod-maq-origem = IF RowErrors.ErrorSubType <> "ERROR" THEN 1 ELSE 2.
                       DELETE RowErrors.
                       LEAVE blocoPedidos.
                   END.                                    

               END.

               /************* Condiá‰es de Pagto ***************/
               IF  NOT VALID-HANDLE(bo-cond-ped) OR
                   bo-cond-ped:TYPE <> "PROCEDURE":U OR 
                   bo-cond-ped:FILE-NAME <> "dibo/bodi018.p":U THEN DO:
                   RUN dibo/bodi018.p PERSISTENT SET bo-cond-ped.
                   RUN openQueryStatic IN bo-cond-ped(INPUT "DefaultPD4000":U).
               END.

               FOR EACH tt-con-pges:
                   CREATE RowCondPed.
                   BUFFER-COPY tt-con-pges EXCEPT cod-maq-origem num-processo num-sequencia nr-pedcli tipo
                                           TO RowCondPed.
                   ASSIGN RowCondPed.nr-pedido = i-nr-pedido.            

                   RUN emptyRowErrors IN bo-cond-ped.
                   RUN setRecord IN bo-cond-ped(INPUT TABLE RowCondPed).
                   RUN createRecord IN bo-cond-ped.
                   RUN getRowErrors IN bo-cond-ped(OUTPUT TABLE RowErrors).

                   DELETE RowCondPed.

                   FOR EACH RowErrors WHERE
                            RowErrors.ErrorType <> "INTERNAL":U :
                       CREATE tt-erros-geral.
                       ASSIGN tt-erros-geral.identif-msg = tt-ped-venda.nome-abrev + CHR(24) + tt-ped-venda.nr-pedcli + CHR(24) + "*"
                              tt-erros-geral.cod-erro    = RowErrors.ErrorNumber
                              tt-erros-geral.des-erro    = RowErrors.ErrorDescription + CHR(24) + tt-ped-venda.nr-pedcli + CHR(24) + " CondPed"
                              tt-erros-geral.cod-maq-origem = IF RowErrors.ErrorSubType <> "ERROR" THEN 1 ELSE 2 .
                       DELETE RowErrors.
                       LEAVE blocoPedidos.
                   END.

               END.

               /************* Antecipaá‰es ***************/
               IF  NOT VALID-HANDLE(bo-ped-antecip) OR
                   bo-ped-antecip:TYPE <> "PROCEDURE":U OR 
                   bo-ped-antecip:FILE-NAME <> "dibo/bodi146.p":U THEN DO:
                   RUN dibo/bodi146.p PERSISTENT SET bo-ped-antecip.
                   RUN openQueryStatic IN bo-ped-antecip(INPUT "Default":U).
               END.

               FOR EACH tt-ped-antecip:
                   CREATE RowPedAntecip.
                   BUFFER-COPY tt-ped-antecip EXCEPT cod-maq-origem num-processo num-sequencia nr-pedcli
                                              TO RowPedAntecip.
                   ASSIGN RowPedAntecip.nr-pedido = i-nr-pedido.

                   RUN emptyRowErrors IN bo-ped-antecip.
                   RUN setRecord IN bo-ped-antecip(INPUT TABLE RowPedAntecip).
                   RUN createRecord IN bo-ped-antecip.
                   RUN getRowErrors IN bo-ped-antecip(OUTPUT TABLE RowErrors).

                   DELETE RowPedAntecip.

                   FOR EACH RowErrors WHERE
                            RowErrors.ErrorType <> "INTERNAL":U:
                       CREATE tt-erros-geral.
                       ASSIGN tt-erros-geral.identif-msg = tt-ped-venda.nome-abrev + CHR(24) + tt-ped-venda.nr-pedcli + CHR(24) + "*"
                              tt-erros-geral.cod-erro    = RowErrors.ErrorNumber
                              tt-erros-geral.des-erro    = RowErrors.ErrorDescription + CHR(24) + tt-ped-venda.nr-pedcli + CHR(24) + " PedAntecip"
                              tt-erros-geral.cod-maq-origem = IF RowErrors.ErrorSubType <> "ERROR" THEN 1 ELSE 2  .
                       DELETE RowErrors.
                       LEAVE blocoPedidos.
                   END.
               END.
           END.
           
           /************* Despesas do Pedido ***************/

           /**** DAFT ***/
           &IF '{&BF_DIS_VERSAO_EMS}' >= '2.042' &THEN

               IF  NOT VALID-HANDLE(bo-desp-pedido) OR
                   bo-desp-pedido:type <> "PROCEDURE":U OR
                   bo-desp-pedido:FILE-NAME <> "dibo/bodi376.p":U THEN DO:
                   RUN dibo/bodi376.p PERSISTENT SET bo-desp-pedido.
                   RUN openQueryStatic IN bo-desp-pedido (INPUT "Main":U).
               END.

               FOR EACH tt-desp-pedido:

                   create RowDespPedido.
                   buffer-copy tt-desp-pedido to RowDespPedido.
                   assign RowDespPedido.nr-pedido = i-nr-pedido.        

                   RUN emptyRowErrors IN bo-desp-pedido.
                   RUN setRecord IN bo-desp-pedido(INPUT TABLE RowDespPedido).
                   RUN createRecord IN bo-desp-pedido.
                   RUN getRowErrors IN bo-desp-pedido(OUTPUT TABLE RowErrors).

                   for EACH RowErrors
                       WHERE RowErrors.ErrorType <> "INTERNAL":U:
                       CREATE tt-erros-geral.
                       ASSIGN tt-erros-geral.identif-msg = tt-ped-venda.nome-abrev + CHR(24) + tt-ped-venda.nr-pedcli + CHR(24) + "*"
                              tt-erros-geral.cod-erro    = RowErrors.ErrorNumber
                              tt-erros-geral.des-erro    = RowErrors.ErrorDescription + CHR(24) + tt-ped-venda.nr-pedcli + CHR(24) + "DespPedido"
                              tt-erros-geral.cod-maq-origem = IF RowErrors.ErrorSubType <> "ERROR" THEN 1 ELSE 2 .
                       DELETE RowErrors.
                   END.

                   DELETE RowDespPedido.

               END.
           &ENDIF
        END.    
    END.  /*blocoPedidos*/


    FOR EACH tt-erros-geral:
        CREATE tt-erros-geral-aux.
        ASSIGN tt-erros-geral-aux.identif-msg        = tt-erros-geral.identif-msg
               tt-erros-geral-aux.num-sequencia-erro = tt-erros-geral.num-sequencia-erro
               tt-erros-geral-aux.cod-erro           = tt-erros-geral.cod-erro
               tt-erros-geral-aux.des-erro           = tt-erros-geral.des-erro
               tt-erros-geral-aux.cod-maq-origem     = tt-erros-geral.cod-maq-origem     .
    END. 

    /** N∆o implanta pedidos com itens rejeitados **/

    FIND FIRST tt-ped-venda NO-ERROR.

    FOR EACH tt-erros-geral-aux:
       /* Garantir que existam tràs informaá‰es no campo chave do pedido/item pedido */
       DO i-campos-chave = NUM-ENTRIES(tt-erros-geral-aux.identif-msg,CHR(24)) TO 3:
          ASSIGN tt-erros-geral-aux.identif-msg = tt-erros-geral-aux.identif-msg + CHR(24).
       END.
    END.

    /*Exclus∆o das mensagens de alerta*/

    FOR EACH tt-erros-geral-aux WHERE 
    	     tt-erros-geral-aux.cod-maq-origem = 1:
	    DELETE tt-erros-geral-aux.
	END.
    
    FIND FIRST tt-erros-geral-aux WHERE 
       ENTRY(1,tt-erros-geral-aux.identif-msg,CHR(24)) = tt-ped-venda.nome-abrev AND 
       ENTRY(2,tt-erros-geral-aux.identif-msg,CHR(24)) = tt-ped-venda.nr-pedcli  AND 
      (ENTRY(3,tt-erros-geral-aux.identif-msg,CHR(24)) >= "0" OR 
       ENTRY(3,tt-erros-geral-aux.identif-msg,CHR(24))  = "*") and
       tt-erros-geral-aux.cod-maq-origem = 2 NO-ERROR.

    IF AVAIL tt-erros-geral-aux AND 
             tt-erros-geral-aux.cod-erro <> 2088 THEN DO:
        RUN emptyRowErrors IN bo-ped-venda.
        RUN goToKey        IN bo-ped-venda(INPUT tt-ped-venda.nome-abrev,
                                           INPUT tt-ped-venda.nr-pedcli).
        RUN deleteRecord   IN bo-ped-venda.
    END.

    /** Verifica se o pedido possui pelo menos um item implantado, sen∆o elimina **/

    IF (CAN-FIND(FIRST ped-venda WHERE 
                       ped-venda.nome-abrev = tt-ped-venda.nome-abrev AND 
                       ped-venda.nr-pedcli  = tt-ped-venda.nr-pedcli) AND  
        NOT CAN-FIND(FIRST ped-item WHERE
                           ped-item.nome-abrev = tt-ped-venda.nome-abrev AND 
                           ped-item.nr-pedcli  = tt-ped-venda.nr-pedcli)) THEN DO:

       CREATE tt-erros-geral-aux.
       ASSIGN tt-erros-geral-aux.identif-msg        = tt-ped-venda.nome-abrev + CHR(24) + tt-ped-venda.nr-pedcli
              tt-erros-geral-aux.num-sequencia-erro = 000
              tt-erros-geral-aux.cod-erro           = 667.

       {utp/ut-liter.i Pedido_n∆o_possui_itens_ou_os_itens_foram_rejeitados}
       ASSIGN tt-erros-geral-aux.des-erro           = TRIM(RETURN-VALUE).

       RUN emptyRowErrors IN bo-ped-venda.
       RUN goToKey        IN bo-ped-venda(INPUT tt-ped-venda.nome-abrev,
                                          INPUT tt-ped-venda.nr-pedcli).
       RUN deleteRecord   IN bo-ped-venda.

    END.


    ASSIGN nr-ped-venda = IF AVAIL tt-ped-venda THEN tt-ped-venda.nr-pedcli ELSE "0".

    /* Completa Pedido Gerado */
    IF NOT CAN-FIND(FIRST tt-erros-geral-aux) THEN DO:
        CREATE tt-ped-completo.
        ASSIGN tt-ped-completo.nr-pedido = tt-ped-venda.nr-pedido.
    END.

    FOR EACH tt-ped-venda:
        DELETE tt-ped-venda.
    END.
    FOR EACH tt-ped-item:
        DELETE tt-ped-item.
    END.
    FOR EACH tt-ped-ent:
        DELETE tt-ped-ent.
    END.
    FOR EACH tt-ped-repre:
        DELETE tt-ped-repre.
    END.
    FOR EACH tt-con-pges:
        DELETE tt-con-pges.
    END.
    FOR EACH tt-ped-antecip:
        DELETE tt-ped-antecip.
    END.
    FOR EACH tt-pd-vendor:
        DELETE tt-pd-vendor.
    END.
    FOR EACH RowPedVenda:
        DELETE RowPedVenda.
    END.
    FOR EACH RowPedItem:
        DELETE RowPedItem.
    END.
    FOR EACH RowPedEnt:
        DELETE RowPedEnt.
    END.
    FOR EACH RowPedRepre:
        DELETE RowPedRepre.
    END.
    FOR EACH RowCondPed:
        DELETE RowCondPed.
    END.
    FOR EACH RowPedAntecip:
        DELETE RowPedAntecip.
    END.
    FOR EACH RowPedVendor:
        DELETE RowPedVendor.
    END.
    FOR EACH rowPedParam:
        DELETE rowPedParam.
    END.
    FOR EACH tt-erros-geral:
        DELETE tt-erros-geral.
    END.
    FOR EACH rowErrors:
        DELETE rowErrors.
    END.

    FOR EACH tt_erros_modulo:
        CREATE tt-erros-geral-aux.
        ASSIGN tt-erros-geral-aux.identif-msg        = tt_erros_modulo.identifi-msg
               tt-erros-geral-aux.num-sequencia-erro = tt_erros_modulo.num-sequencia-erro
               tt-erros-geral-aux.cod-erro           = tt_erros_modulo.cod-erro
               tt-erros-geral-aux.des-erro           = tt_erros_modulo.des-erro.
    END.

    FOR EACH tt_erros_modulo:
        DELETE tt_erros_modulo.
    END.

END PROCEDURE. /* pi-executa */

PROCEDURE pi-TrataDesconto :
/*------------------------------------------------------------------------------
  Purpose:     Trata Desconto
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* 
       Essa api vai alimentar as seguintes informaá‰es :
       * na ped-item, os 5 descontos da val-desconto
       * ir† preparar as informaá‰es para criar  a desc-ped-item. 
     */
     
    IF  NOT VALID-HANDLE(bo-ped-item-sdf) OR 
        bo-ped-item-sdf:TYPE <> "PROCEDURE":U OR 
        bo-ped-item-sdf:FILE-NAME <> "dibo/bodi154sdf.p":U THEN 
            RUN dibo/bodi154sdf.p PERSISTENT SET bo-ped-item-sdf.

    FOR EACH tt-desc-ped-item-bo.
        DELETE tt-desc-ped-item-bo.
    END.

    FOR EACH tt-ped-item-bo:
       DELETE tt-ped-item-bo.
    END. 

    FOR EACH tt-ped-venda-bo:
       DELETE tt-ped-venda-bo.
    END.

    CREATE tt-ped-item-bo.
    BUFFER-COPY tt-ped-item TO tt-ped-item-bo.

    CREATE tt-ped-venda-bo.
    BUFFER-COPY tt-ped-venda TO tt-ped-venda-bo.

    RUN inputParentTable   IN bo-ped-item-sdf (input table tt-ped-venda-bo).
    RUN inputTable         IN bo-ped-item-sdf (input table tt-ped-item-bo).
    RUN setDefaultDiscount IN bo-ped-item-sdf.
    RUN outputTable        IN bo-ped-item-sdf (output table tt-ped-item-bo).

    FIND FIRST tt-ped-item-bo NO-LOCK NO-ERROR .
    ASSIGN RowPedItem.val-desconto[1] = tt-ped-item-bo.val-desconto[1]
           RowPedItem.val-desconto[2] = tt-ped-item-bo.val-desconto[2]
           RowPedItem.val-desconto[3] = tt-ped-item-bo.val-desconto[3]    
           RowPedItem.val-desconto[4] = tt-ped-item-bo.val-desconto[4]    
           RowPedItem.val-desconto[5] = tt-ped-item-bo.val-desconto[5]
           RowPedItem.val-pct-desconto-periodo = tt-ped-item-bo.val-pct-desconto-periodo
           RowPedItem.val-pct-desconto-prazo   = tt-ped-item-bo.val-pct-desconto-prazo
           RowPedItem.log-concede-bonif-qtd    = tt-ped-item-bo.log-concede-bonif-qtd.    
       
    
    RUN outputDiscountTable IN bo-ped-item-sdf (output table tt-desc-ped-item-bo).

    for each tt-desc-ped-item-bo:

        create tt-desc-ped-item.
        buffer-copy tt-desc-ped-item-bo                      to tt-desc-ped-item.
        assign      tt-desc-ped-item.num-desconto             = tt-desc-ped-item-bo.cod-desconto
                    tt-desc-ped-item.nom-abrev-cliente        = tt-ped-item.nome-abrev
                    tt-desc-ped-item.cod-pedido-cliente       = tt-ped-item.nr-pedcli
                    tt-desc-ped-item.num-sequencia            = tt-ped-item.nr-sequencia
                    tt-desc-ped-item.cod-item                 = tt-ped-item.it-codigo
                    tt-desc-ped-item.cod-refer                = tt-ped-item.cod-refer
                    tt-desc-ped-item.val-pct-desconto-periodo = tt-desc-ped-item-bo.val-pct-desc-periodo
                    tt-desc-ped-item.val-pct-desconto-prazo   = tt-desc-ped-item-bo.val-pct-desc-prazo.                   
        
    END.

    FOR EACH tt-desc-ped-item-bo.
        DELETE tt-desc-ped-item-bo.
    END.

END PROCEDURE. /* pi-TrataDesconto */

PROCEDURE pi-TrataImposto :
/*------------------------------------------------------------------------------
  Purpose:     Trata Imposto
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     
    IF  NOT VALID-HANDLE(bo-ped-item-sdf) OR 
        bo-ped-item-sdf:TYPE <> "PROCEDURE":U OR 
        bo-ped-item-sdf:FILE-NAME <> "dibo/bodi154sdf.p":U THEN 
            RUN dibo/bodi154sdf.p PERSISTENT SET bo-ped-item-sdf.

    FOR EACH tt-ped-item-bo:
       DELETE tt-ped-item-bo.
    END. 

    FOR EACH tt-ped-venda-bo:
       DELETE tt-ped-venda-bo.
    END.

    CREATE tt-ped-item-bo.
    BUFFER-COPY tt-ped-item TO tt-ped-item-bo.

    CREATE tt-ped-venda-bo.
    BUFFER-COPY tt-ped-venda TO tt-ped-venda-bo.

    RUN inputParentTable   IN bo-ped-item-sdf (input table tt-ped-venda-bo).
    RUN inputTable         IN bo-ped-item-sdf (input table tt-ped-item-bo).
    RUN setDefaultItemTax  IN bo-ped-item-sdf.
    RUN outputTable        IN bo-ped-item-sdf (output table tt-ped-item-bo).

    FIND FIRST tt-ped-item-bo NO-LOCK NO-ERROR .
    ASSIGN RowPedItem.cod-vat = tt-ped-item-bo.cod-vat.   

END PROCEDURE. /* pi-TrataImposto */


PROCEDURE pi-frete:
/*------------------------------------------------------------------------------
  Purpose:     Trata Frete
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE l-log-frete AS LOGICAL    NO-UNDO INIT NO.

    ASSIGN i-cod-servico = 0.

    IF NOT VALID-HANDLE(h-bosc014) OR 
       h-bosc014:TYPE      <> "PROCEDURE":U OR 
       h-bosc014:FILE-NAME <> "scbo/bosc014.p":U THEN DO:
       RUN scbo/bosc014.p PERSISTENT SET h-bosc014.
       RUN openQueryStatic IN h-bosc014 (INPUT "Default":U).
    END.

    RUN goToKey in h-bosc014 (INPUT RowPedVenda.cod-estabel).
    IF RETURN-VALUE = "OK":U THEN 
    DO:
        RUN getLogField IN h-bosc014(INPUT "log-frete-vda",
                                     OUTPUT l-log-frete).
        IF l-log-frete THEN
            RUN getIntField IN h-bosc014 (INPUT  "cod-servico":U,
                                          OUTPUT i-cod-servico).

    END.
    ASSIGN RowPedVenda.dec-1 = i-cod-servico.
    DELETE PROCEDURE h-bosc014.
    ASSIGN h-bosc014 = ?.

END PROCEDURE. /* pi-frete */

PROCEDURE InicializaHandles:
/*------------------------------------------------------------------------------
  Purpose:     Instancia os handles .
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF NOT VALID-HANDLE(h-bodi159sdf) OR 
       h-bodi159sdf:TYPE <> "PROCEDURE":U OR 
       h-bodi159sdf:FILE-NAME <> "dibo/bodi159sdf.p":U THEN 
       RUN dibo/bodi159sdf.p PERSISTENT SET h-bodi159sdf.

    IF NOT VALID-HANDLE(bo-ped-ent) OR
       bo-ped-ent:TYPE <> "PROCEDURE":U OR 
       bo-ped-ent:FILE-NAME <> "dibo/bodi149.p":U THEN DO:
        RUN dibo/bodi149.p PERSISTENT SET bo-ped-ent.
        RUN openQueryStatic IN bo-ped-ent(INPUT "Default":U).
    END.

    RETURN "OK".

END PROCEDURE. /* InicializaHandles */


PROCEDURE pi-elimina-handle :
/*------------------------------------------------------------------------------
  Purpose:     Elimina os handles instanciados.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF  VALID-HANDLE(bo-cond-ped) THEN DO:
        RUN destroy IN bo-cond-ped.
        ASSIGN bo-cond-ped = ?.
    END.

    IF  VALID-HANDLE(bo-ped-antecip) THEN DO:
        RUN destroy IN bo-ped-antecip.
        ASSIGN bo-ped-antecip = ?.
    END.

    IF  VALID-HANDLE(bo-ped-item) THEN DO:
        RUN destroyBO IN bo-ped-item.
        RUN destroy IN bo-ped-item.
        ASSIGN bo-ped-item = ?.
    END.

    IF  VALID-HANDLE(bo-ped-item-sdf) THEN DO:
        DELETE PROCEDURE bo-ped-item-sdf.
        ASSIGN bo-ped-item-sdf = ?.
    END.

    IF  VALID-HANDLE(bo-ped-ent) THEN DO:
        RUN destroy IN bo-ped-ent.
        ASSIGN bo-ped-ent = ?.
    END.

    IF  VALID-HANDLE(bo-ped-repre) THEN DO:
        RUN destroy IN bo-ped-repre.
        ASSIGN bo-ped-repre = ?.
    END.

    IF  VALID-HANDLE(bo-ped-venda) THEN DO:
        RUN destroyBO IN bo-ped-venda.
        RUN destroy IN bo-ped-venda.
        ASSIGN bo-ped-venda = ?.
    END. 

    &if '{&BF_DIS_VERSAO_EMS}' >= '2.062':U &then 
    IF  VALID-HANDLE(h-cdapi024) THEN DO:
        DELETE PROCEDURE h-cdapi024.
        ASSIGN h-cdapi024 = ?.
    END.
    &endif

    IF  VALID-HANDLE(h-bodi159sdf) THEN DO:
        RUN destroy IN h-bodi159sdf.
        ASSIGN h-bodi159sdf = ?.
    END. 
    &IF "{&ems_dbtype}" <> "progress":U &THEN
     IF  VALID-HANDLE(h-bodi154sdf) THEN DO:
         RUN destroy IN h-bodi154sdf.
         ASSIGN h-bodi154sdf = ?.
     END.
    &endif

    RETURN "OK".
END PROCEDURE. /* pi-elimina-handle */

PROCEDURE pi-verifica-if-nor :
/*------------------------------------------------------------------------------
  Purpose:     Verifica se existem itens incentivados e itens normais no mesmo pedido
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN l-it-n-if = NO
           l-it-t-if = NO.

    IF CAN-FIND(FIRST if-ped-venda
                WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido) THEN RETURN "OK".

    IF ped-venda.cond-redespa BEGINS "Pedido(s) relacionado(s)" THEN RETURN "OK".

    FOR EACH ped-item OF ped-venda where  ped-item.ind-componen < 3   /* esta pegado itens da estrutura que tem outras familias nao incentivadas*/
 NO-LOCK,
       FIRST item NO-LOCK
       WHERE item.it-codigo = ped-item.it-codigo:

        FIND FIRST if-natur-fam NO-LOCK
             WHERE if-natur-fam.cod-estab-orig  = ped-venda.cod-estabel
               AND if-natur-fam.nat-oper-pedido = ped-item.nat-operacao
               AND if-natur-fam.fm-codigo       = item.fm-codigo NO-ERROR.

        FIND FIRST if-natur-item NO-LOCK
             WHERE if-natur-item.cod-estab-orig  = ped-venda.cod-estabel
               AND if-natur-item.nat-oper-pedido = ped-item.nat-operacao
               AND if-natur-item.it-codigo       = ped-item.it-codigo NO-ERROR.

        IF NOT AVAIL if-natur-fam THEN DO:
            ASSIGN l-it-n-if = YES.
        END.
        ELSE IF NOT AVAIL if-natur-item THEN DO:
            ASSIGN l-it-t-if = YES.
        END.
    END.

    /* Cliente em exceá∆o da Unigel Comercial */
    FIND FIRST if-exec-cli NO-LOCK
         WHERE if-exec-cli.cod-emitente = ped-venda.cod-emitente NO-ERROR.
    IF AVAIL if-exec-cli THEN
        ASSIGN l-it-n-if = YES
               l-it-t-if = NO.

    IF l-it-n-if AND l-it-t-if THEN DO:
        FIND FIRST if-ped-venda EXCLUSIVE-LOCK
             WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-ERROR.

        IF AVAIL if-ped-venda THEN
            DELETE if-ped-venda.

        ASSIGN ped-venda.completo = NO.

        RUN _insertErrorManual IN h-bodi159com (INPUT 0,
                                                INPUT "EMS":U,
                                                INPUT "ERROR":U,
                                                INPUT "N∆o Ç permitido efetivar pedido com itens incentivados e itens normais.",
                                                INPUT "Para isso dever† ser gerados dois pedidos separados",
                                                INPUT "":U).

        RETURN "NOK".
    END.

    RETURN "OK".

END PROCEDURE. /* pi-verifica-if-nor */

PROCEDURE pi-cria-if :
/*------------------------------------------------------------------------------
  Purpose:     Cria tabela com pedido que possui itens fiscais
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE l-cria-if AS LOGICAL     NO-UNDO.

    ASSIGN l-cria-if = NO.
    FOR EACH ped-item OF ped-venda where ped-item.ind-compon < 3 NO-LOCK,
       FIRST item NO-LOCK
       WHERE item.it-codigo = ped-item.it-codigo:

        FIND FIRST if-natur-fam NO-LOCK
             WHERE if-natur-fam.cod-estab-orig  = ped-venda.cod-estabel
               AND if-natur-fam.nat-oper-pedido = ped-item.nat-operacao
               AND if-natur-fam.fm-codigo       = item.fm-codigo NO-ERROR.

        FIND FIRST if-natur-item NO-LOCK
             WHERE if-natur-item.cod-estab-orig  = ped-venda.cod-estabel
               AND if-natur-item.nat-oper-pedido = ped-item.nat-operacao
               AND if-natur-item.it-codigo       = ped-item.it-codigo NO-ERROR.

        IF AVAIL if-natur-fam AND NOT AVAIL if-natur-item THEN DO:
            ASSIGN l-cria-if = YES.
            LEAVE.
        END.
    END.

    IF l-cria-if THEN DO:
        CREATE if-ped-venda.
        ASSIGN if-ped-venda.nr-pedido     = ped-venda.nr-pedido
               if-ped-venda.nome-abrev    = ped-venda.nome-abrev
               if-ped-venda.nr-pedcli     = ped-venda.nr-pedcli
               if-ped-venda.nat-oper-orig = ped-venda.nat-operacao.

        FOR EACH if-estabelec NO-LOCK
           WHERE if-estabelec.cod-estab-orig  = ped-venda.cod-estabel
             AND if-estabelec.cod-estab-inter = "":
            ASSIGN if-ped-venda.cod-estab-atend = if-estabelec.cod-estab-dest.
            LEAVE.
        END.
    END.

    RETURN "OK".

END PROCEDURE. /* pi-cria-if */


PROCEDURE pi-alt-cliente :
/*------------------------------------------------------------------------------
  Purpose:     Altera Cliente
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAM p-cod-estab AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAM p-mudouEstab  AS LOGICAL     NO-UNDO.

    FIND FIRST ws-ped-venda
         WHERE ws-ped-venda.nome-abrev = ped-venda.nome-abrev
           AND ws-ped-venda.nr-pedcli  = ped-venda.nr-pedcli  EXCLUSIVE-LOCK NO-ERROR.

    FIND FIRST b-estabelec NO-LOCK
         WHERE b-estabelec.cod-estabel = ped-venda.cod-estabel NO-ERROR.

    FIND FIRST estabelec NO-LOCK
         WHERE estabelec.cod-estabel = p-cod-estab NO-ERROR.
    
    FIND FIRST emitente NO-LOCK
         WHERE emitente.cod-emitente = estabelec.cod-emitente NO-ERROR.

    IF NOT AVAIL emitente THEN DO:
        RUN _insertErrorManual IN h-bodi159com (INPUT 0,
                                                INPUT "EMS":U,
                                                INPUT "ERROR",
                                                INPUT "Emitente do estabelecimento " + TRIM(ped-venda.cod-estabel) + " deve estar cadastro no CD0602",
                                                INPUT "",
                                                INPUT "":U).
    END.

    ASSIGN ped-venda.nat-operacao = IF estabelec.ep-codigo <> b-estabelec.ep-codigo THEN if-natur-oper.nat-oper-v-ung
                                    ELSE if-natur-oper.nat-oper-transf
           ped-venda.cod-cond-pag = IF estabelec.ep-codigo <> b-estabelec.ep-codigo THEN ped-venda.cod-cond-pag
                                    ELSE 0.

    /* tt-ped-item */
     
    
    FOR EACH ped-item OF ped-venda EXCLUSIVE-LOCK:
         

        /*WPA - 20121017 - Necess†rio para quando altera o estabelecimento, visto que n∆o estava alterando as ped-ent para o novo nome-abrev*/
        IF p-mudouEstab THEN DO:
            /* tt-ped-ent*/            
            FOR EACH ped-ent OF ped-item EXCLUSIVE-LOCK:
                find first bb-ped-ent
                        WHERE bb-ped-ent.nome-abrev   = emitente.nome-abrev  
                          AND bb-ped-ent.nr-pedcli    = ped-item.nr-pedcli   
                          AND bb-ped-ent.nr-sequencia = ped-item.nr-sequencia
                          AND bb-ped-ent.it-codigo    = ped-item.it-codigo   
                          AND bb-ped-ent.cod-refer    = ped-item.cod-refer no-error.
                if not avail  bb-ped-ent then do:
                    create bb-ped-ent.
                    
                    
                    buffer-copy ped-ent except nome-abrev 
                    to bb-ped-ent
                     assign bb-ped-ent.nome-abrev = emitente.nome-abrev .
                   delete ped-ent.
                end. 
                else do:
                    buffer-copy ped-ent except nome-abrev 
                    to bb-ped-ent.

                
                end.       
                 
            
            END.
        END.
        
        ASSIGN ped-item.nome-abrev   = emitente.nome-abrev
               ped-item.nat-operacao = ped-venda.nat-operacao.

       
    END.

    ASSIGN ped-venda.cod-emitente = emitente.cod-emitente
           ped-venda.nome-abrev   = emitente.nome-abrev
           c-nome-abrev-orig      = emitente.nome-abrev.

    /* Inclus∆o de regra para geraá∆o do CRM */
    IF AVAIL ws-ped-venda THEN DO:

        FOR EACH ws-ped-item 
           WHERE ws-ped-item.nome-abrev = ws-ped-venda.nome-abrev
             AND ws-ped-item.nr-pedcli  = ws-ped-venda.nr-pedcli EXCLUSIVE-LOCK:

            ASSIGN ws-ped-item.nome-abrev   = ped-venda.nome-abrev
                   ws-ped-item.nat-operacao = ped-venda.nat-operacao.
        END.

        ASSIGN ws-ped-venda.cod-emitente = ped-venda.cod-emitente
               ws-ped-venda.nome-abrev   = ped-venda.nome-abrev  
               ws-ped-venda.nat-operacao = ped-venda.nat-operacao
               ws-ped-venda.cod-cond-pag = ped-venda.cod-cond-pag.
    END.

    IF ped-venda.cod-emitente = 0 THEN DO:
        RUN _insertErrorManual IN h-bodi159com (INPUT 0,
                                                INPUT "EMS":U,
                                                INPUT "ERROR",
                                                INPUT "Emitente do estabelecimento " + TRIM(ped-venda.cod-estabel) + " deve estar cadastro no CD0602",
                                                INPUT "",
                                                INPUT "":U).
    END.


END PROCEDURE. /* pi-alt-cliente */


PROCEDURE pi-alt-cli-tt :
/*------------------------------------------------------------------------------
  Purpose:     Altera Cliente
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAM p-cod-estab AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAM p-intermed  AS LOGICAL   NO-UNDO.

    define var l-fora as logical no-undo.
    
    
    FIND FIRST b-estabelec NO-LOCK
         WHERE b-estabelec.cod-estabel = string(i-cod-emit-orig) NO-ERROR.
    IF NOT AVAIL b-estabelec THEN
        FIND FIRST b-estabelec NO-LOCK
             WHERE b-estabelec.cod-estabel = ped-venda.cod-estabel NO-ERROR.

    FIND FIRST estabelec NO-LOCK
         WHERE estabelec.cod-estabel = p-cod-estab NO-ERROR.

    FIND FIRST tt-ped-venda NO-ERROR.

    IF p-intermed THEN DO:

        FIND FIRST emitente NO-LOCK
             WHERE emitente.cod-emitente = estabelec.cod-emitente NO-ERROR.

        ASSIGN tt-ped-venda.nat-operacao = if-natur-oper.nat-oper-transf
               tt-ped-venda.cod-cond-pag = 0.
    END.
    ELSE DO:

        FIND FIRST emitente NO-LOCK
             WHERE emitente.cod-emitente = i-cod-emit-orig NO-ERROR.
        
        find first buf-estabelec where buf-estabelec.cod-estabel = tt-ped-venda.cod-estabel no-lock no-error.
        find first buf-emitente where buf-emitente.cod-emitente = i-cod-emit-orig no-lock no-error.
        
        l-fora = (buf-estabelec.estado <> buf-emitente.estado).
        
        
      
         
        ASSIGN tt-ped-venda.nat-operacao = IF estabelec.ep-codigo <> b-estabelec.ep-codigo THEN (if l-fora and nat-oper-venda-inter <> "" then nat-oper-venda-inter else if-natur-oper.nat-oper-venda)
                                           ELSE if-natur-oper.nat-oper-transf.


        ASSIGN tt-ped-venda.cod-cond-pag = IF estabelec.ep-codigo <> b-estabelec.ep-codigo THEN tt-ped-venda.cod-cond-pag
                                           ELSE 0.
        
    END.

    /* tt-ped-item */
    FOR EACH tt-ped-item EXCLUSIVE-LOCK
       WHERE tt-ped-item.nome-abrev = tt-ped-venda.nome-abrev
         AND tt-ped-item.nr-pedcli  = tt-ped-venda.nr-pedcli :

        ASSIGN tt-ped-item.nome-abrev   = emitente.nome-abrev
               tt-ped-item.nat-operacao = tt-ped-venda.nat-operacao.

        /* tt-ped-ent */
        FOR EACH tt-ped-ent EXCLUSIVE-LOCK
           WHERE tt-ped-ent.nome-abrev   = tt-ped-item.nome-abrev  
             AND tt-ped-ent.nr-pedcli    = tt-ped-item.nr-pedcli   
             AND tt-ped-ent.nr-sequencia = tt-ped-item.nr-sequencia
             AND tt-ped-ent.it-codigo    = tt-ped-item.it-codigo   
             AND tt-ped-ent.cod-refer    = tt-ped-item.cod-refer:
            ASSIGN tt-ped-ent.nome-abrev = emitente.nome-abrev.
        END.

        /***** VERIFICAÄ«O PARA TRANSFERENCIA ******/

        FIND FIRST natur-oper
             WHERE natur-oper.nat-operacao = tt-ped-item.nat-operacao NO-LOCK NO-ERROR.

        IF NOT AVAIL natur-oper THEN NEXT. 
        
        find first item where item.it-codigo = tt-ped-item.it-codigo no-lock no-error.
        if not avail item then next.  /*edson estranho n∆o estava lendo tabela item*/

        if  (((item.tipo-contr = 4 or (item.aliquota-iss > 0 and item.tipo-contr <> 2)) and
             (item.baixa-estoq and natur-oper.baixa-estoq)) 
        or  
            ((item.tipo-contr = 1 or item.tipo-contr = 4) and          
             (natur-oper.terceiros or natur-oper.transf))
        or  
             (item.tipo-contr = 2 or item.tipo-contr = 3) and
              natur-oper.terceiros and
             (not item.baixa-estoq or not natur-oper.baixa-estoq)) and
              not ped-venda.log-cotacao then do:

            ASSIGN OVERLAY(tt-ped-item.char-2,11,17) = ITEM.conta-aplicacao.

        END.
    END.

    ASSIGN tt-ped-venda.cod-emitente = emitente.cod-emitente
           tt-ped-venda.nome-abrev   = emitente.nome-abrev.

    IF tt-ped-venda.cod-emitente = 0 THEN DO:
        RUN _insertErrorManual IN h-bodi159com (INPUT 0,
                                                INPUT "EMS":U,
                                                INPUT "ERROR",
                                                INPUT "Emitente do estabelecimento " + TRIM(tt-ped-venda.cod-estabel) + " deve estar cadastro no CD0602",
                                                INPUT "",
                                                INPUT "":U).
    END.

END PROCEDURE. /* pi-alt-cliente */

PROCEDURE pi-completa-ped :
/*------------------------------------------------------------------------------
  Purpose:     Completa Pedido
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE bo-completa-ped AS HANDLE NO-UNDO.
    DEFINE VARIABLE rw-ped-venda    AS ROWID  NO-UNDO.

    RUN dibo/bodi159com.p PERSISTENT SET bo-completa-ped.

    FOR EACH tt-ped-completo:
    
        FIND FIRST ped-venda NO-LOCK
             WHERE ped-venda.nr-pedido = tt-ped-completo.nr-pedido NO-ERROR.
    
        /* forca igualar tudo nas ped-ent  POR Garantia EDSON */ 
        FOR  EACH PED-ITEM OF ped-venda where ped-item.cod-sit-item < 3 NO-LOCK,
            EACH ped-ent OF PED-ITEM  .
            
            assign ped-ent.qt-pedida = ped-item.qt-pedida.
            
            find first b3-ped-venda where b3-ped-venda.NR-PEDCLI = ped-venda.NR-PEDCLI  and
                                          b3-ped-venda.NR-pedido <> ped-venda.nr-pedido no-lock no-error.
            
            if avail b3-ped-venda then do:
            
               find first bb-ped-ent
                                  WHERE bb-ped-ent.nome-abrev   = b3-ped-venda.nome-abrev  
                                    AND bb-ped-ent.nr-pedcli    = b3-ped-venda.nr-pedcli   
                                    AND bb-ped-ent.nr-sequencia = ped-item.nr-sequencia
                                    AND bb-ped-ent.it-codigo    = ped-item.it-codigo   
                                    AND bb-ped-ent.cod-refer    = ped-item.cod-refer no-error.
                          if not avail  bb-ped-ent then do:
                              create bb-ped-ent.
                              buffer-copy ped-ent except nome-abrev   to bb-ped-ent
                               assign bb-ped-ent.nome-abrev = b3-ped-venda.nome-abrev
                                      .
                          
                          end.  
                          
                   IF bb-ped-ent.cod-sit-ent = ped-item.cod-sit-item  
                         THEN   assign bb-ped-ent.qt-pedida = ped-item.qt-pedida.
            
            
               
            
            end.
        end.  
    
        /********* Cria Pedido Duplicado no CRM **********/
        FIND FIRST ws-ped-venda 
             WHERE ws-ped-venda.nome-abrev = ped-venda.nome-abrev
               AND ws-ped-venda.nr-pedcli  = ped-venda.nr-pedcli EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL ws-ped-venda THEN DO:
            CREATE ws-ped-venda.
            ASSIGN ws-ped-venda.origem        = "ERP"
                   ws-ped-venda.operacao      = "A" /* solicitado por marcelo 29/6 c-operacao */
                   ws-ped-venda.processado    = 0
                   ws-ped-venda.situacao      = 1
                   ws-ped-venda.data-operacao = NOW
                   ws-ped-venda.nome-abrev    = ped-venda.nome-abrev
                   ws-ped-venda.nr-pedcli     = ped-venda.nr-pedcli.
        END.
        BUFFER-COPY ped-venda EXCEPT nome-abrev  nr-pedcli    origem 
                                     cod-sit-ped cod-sit-aval TO ws-ped-venda.
        
        FOR EACH ped-item OF ped-venda NO-LOCK:

            FIND FIRST ws-ped-item 
                 WHERE ws-ped-item.nome-abrev   = ped-item.nome-abrev
                   AND ws-ped-item.nr-pedcli    = ped-item.nr-pedcli 
                   AND ws-ped-item.nr-sequencia = ped-item.nr-sequencia
                   AND ws-ped-item.it-codigo    = ped-item.it-codigo EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAIL ws-ped-item THEN DO:
                CREATE ws-ped-item.
                ASSIGN ws-ped-item.origem        = "ERP"
                       ws-ped-item.operacao      = "A" /* 29/6 solciitado po Marcelo c-operacao */
                       ws-ped-item.processado    = 0
                       ws-ped-item.situacao      = 1
                       ws-ped-item.data-operacao = NOW
                       ws-ped-item.nr-pedido     = ped-venda.nr-pedido
                       ws-ped-item.nome-abrev   = ped-item.nome-abrev  
                       ws-ped-item.nr-pedcli    = ped-item.nr-pedcli   
                       ws-ped-item.nr-sequencia = ped-item.nr-sequencia
                       ws-ped-item.it-codigo    = ped-item.it-codigo.
            END.

            BUFFER-COPY ped-item EXCEPT nome-abrev   nr-pedcli 
                                        nr-sequencia it-codigo 
                                        cod-sit-item TO ws-ped-item.
        END.
        /********* Cria Pedido Duplicado no CRM **********/
    
        ASSIGN rw-ped-venda = ROWID(ped-venda).
                         
 
        RUN completeOrder IN bo-completa-ped(INPUT rw-ped-venda,
                                             OUTPUT TABLE RowErrors).
 
                                             
    
        FOR EACH RowErrors:
    
            RUN _insertErrorManual IN h-bodi159com (INPUT RowErrors.ErrorNumber,
                                                    INPUT "EMS":U,
                                                    INPUT RowErrors.ErrorSubType,
                                                    INPUT RowErrors.ErrorDescription,
                                                    INPUT RowErrors.ErrorHelp,
                                                    INPUT "":U).

            IF RowErrors.ErrorSubType = "Error" THEN
                ASSIGN l-erro = YES.

            DELETE RowErrors.
        END.

        FOR EACH tt-ped-comp-ava,
           FIRST b2-ped-venda EXCLUSIVE-LOCK
           WHERE b2-ped-venda.nr-pedido = tt-ped-comp-ava.nr-pedido :

            ASSIGN b2-ped-venda.quem-aprovou = ped-venda.quem-aprovou 
                   b2-ped-venda.cod-sit-aval = ped-venda.cod-sit-aval
                   b2-ped-venda.dt-apr-cred  = ped-venda.dt-apr-cred                                  
                   b2-ped-venda.desc-bloq-cr = ped-venda.desc-bloq-cr                                  
                   b2-ped-venda.desc-forc-cr = ped-venda.desc-forc-cr.
        END.
    END.

    DELETE PROCEDURE bo-completa-ped.

END PROCEDURE. /* pi-completa-ped */


PROCEDURE pi-atualiz-ped-relac :
/*------------------------------------------------------------------------------
  Purpose:    atualiza pedido relacionado
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE c-frete AS CHARACTER   NO-UNDO.

    IF TRIM(SUBSTRING(ped-venda.char-2,109,8)) <> "9" THEN
        ASSIGN c-frete = SUBSTRING(ped-venda.char-2,109,8).
    ELSE 
        ASSIGN c-frete = SUBSTRING(b-ped-venda.char-2,109,8).
 
    ASSIGN ped-venda.cond-redespa   = "Pedido(s) relacionado(s): " + c-lst-ped
           ped-venda.cidade-cif     = ""
           ped-venda.nome-abrev-tri = "" 
           ped-venda.nome-transp    = ""
           ped-venda.nome-tr-red    = ""
           ped-venda.cod-mensagem   = 0
           OVERLAY(ped-venda.char-2,109,8) = "9".

    /* Atualiza com a mensagem da natureza */
    FIND FIRST natur-oper NO-LOCK
         WHERE natur-oper.nat-operacao = ped-venda.nat-operacao NO-ERROR.
    IF AVAIL natur-oper THEN DO:
        ASSIGN ped-venda.cod-mensagem = natur-oper.cod-mensagem.

        FOR EACH ped-item OF ped-venda EXCLUSIVE-LOCK:
            ASSIGN ped-item.ind-icm-ret  = natur-oper.subs-trib
                   ped-item.per-des-icms = natur-oper.per-des-icms.
        END.
    END.

    if ped-venda.cod-estabel = "422" OR ped-venda.cod-estabel = "412" then do:    /*solic-318*/ 
    
            BUFFER-COPY ped-venda EXCEPT 
                                     nr-pedido 
                                     nr-pedcli 
                                     cod-emitente 
                                     nome-abrev 
                                     cod-estabel 
                                     cond-redespa 
                                     nat-operacao 
                                     cidade-cif 
                                     nome-abrev-tri 
                                     nome-transp 
                                     nome-tr-red 
                                     cod-mensagem 
                                      
                                      /*edson----- */
                                       
                                        dt-devolucao
                                        cod-entrega
                                        local-entreg
                                        bairro
                                        cidade
                                        estado
                                        cep
                                        caixa-postal
                                        pais
                                        cgc
                                        ins-estadual
                                        cod-sit-ped
                                        ind-aprov
                                        quem-aprovou
                                        dt-apr-cred
                                        vl-tot-ped
                                        vl-liq-ped
                                        vl-liq-abe
                                        user-aprov
                                        vl-mer-abe
                                        cod-sit-aval
                                        desc-bloq-cr
                                        desc-forc-cr
                                        cod-sit-pre
                                        vl-cred-lib
                                        aprov-forcado
                                        dsp-pre-fat
                                        cod-gr-cli
                                        atendido
                                        ind-ent-completa
                                        cod-sit-com
                                        cod-entrega-tri
                                        cdn-motiv-reprovac-cr
                                        char-1
                                        char-2 
                                        observacoes
                 
                                      /*------------*/
                                     
                                     TO b-ped-venda.
    
    
    
    
    end.
    else do:

        

        
        BUFFER-COPY ped-venda EXCEPT nr-pedido 
                                     nr-pedcli 
                                     cod-emitente 
                                     nome-abrev 
                                     cod-estabel 
                                     cond-redespa 
                                     nat-operacao 
                                     cidade-cif 
                                     nome-abrev-tri 
                                     nome-transp 
                                     nome-tr-red 
                                     cod-mensagem 
                                     observacoes
                                     TO b-ped-venda.
    end.

    IF trim(ped-venda.observacoes) <> "" THEN
       ASSIGN
         b-ped-venda.observacoes = ped-venda.observacoes
         ped-venda.observacoes = "".


    ASSIGN OVERLAY(b-ped-venda.char-2,109,8) = c-frete.
    if trim(substring(b-ped-venda.char-2,109,8)) = ""   then 
            OVERLAY(b-ped-venda.char-2,109,8) = "0".

    FOR EACH ped-antecip NO-LOCK
       WHERE ped-antecip.nr-pedido = ped-venda.nr-pedido:

        FIND FIRST b-ped-antecip EXCLUSIVE-LOCK
             WHERE b-ped-antecip.nr-pedido = b-ped-venda.nr-pedido NO-ERROR.
        IF AVAIL b-ped-antecip THEN DO:
            BUFFER-COPY ped-antecip EXCEPT nr-pedido TO b-ped-antecip.
        END.
        ELSE DO:
            CREATE b-ped-antecip.
            BUFFER-COPY ped-antecip EXCEPT nr-pedido TO b-ped-antecip.
            ASSIGN b-ped-antecip.nr-pedido = b-ped-venda.nr-pedido.
        END.
            
    END.

    FOR EACH ped-repre NO-LOCK
       WHERE ped-repre.nr-pedido = ped-venda.nr-pedido:

        FIND FIRST b-ped-repre EXCLUSIVE-LOCK
             WHERE b-ped-repre.nr-pedido         = b-ped-venda.nr-pedido
               AND b-ped-repre.nome-ab-rep       = ped-repre.nome-ab-rep      
               AND b-ped-repre.cod-classificador = ped-repre.cod-classificador NO-ERROR.
        IF AVAIL b-ped-repre THEN DO:
            BUFFER-COPY ped-repre EXCEPT nr-pedido TO b-ped-repre.
        END.
        ELSE DO:
            CREATE b-ped-repre.
            BUFFER-COPY ped-repre EXCEPT nr-pedido TO b-ped-repre.
            ASSIGN b-ped-repre.nr-pedido = b-ped-venda.nr-pedido.
        END.
    END.

    FOR EACH ped-item OF ped-venda NO-LOCK:
 

        FIND FIRST b-ped-item EXCLUSIVE-LOCK
             WHERE b-ped-item.nome-abrev   = b-ped-venda.nome-abrev
               AND b-ped-item.nr-pedcli    = b-ped-venda.nr-pedcli
               AND b-ped-item.nr-sequencia = ped-item.nr-sequencia
               AND b-ped-item.it-codigo    = ped-item.it-codigo   
               AND b-ped-item.cod-refer    = ped-item.cod-refer NO-ERROR.
        IF AVAIL b-ped-item THEN DO:
        
           if ped-venda.cod-estabel = "422" OR ped-venda.cod-estabel = "412" then  /*solic-318*/ 
                       BUFFER-COPY ped-item EXCEPT nome-abrev nr-pedcli nat-operacao ind-icm-ret per-des-icms
                       
                        qt-alocada qt-atendida qt-devolvida 
                        qt-fatenf qt-log-aloca qt-ordens qt-pendente 
                        qtd-alocar qt-transfer qt-trans-mp 
                        nr-ord-produ nr-ordem cod-sit-item cod-sit-pre
                       
                        TO b-ped-item.
           else
                       BUFFER-COPY ped-item EXCEPT nome-abrev nr-pedcli nat-operacao ind-icm-ret per-des-icms TO b-ped-item.
 
            
        END.
        ELSE DO:
            CREATE b-ped-item.
            BUFFER-COPY ped-item EXCEPT nome-abrev nr-pedcli nat-operacao TO b-ped-item.
            ASSIGN b-ped-item.nome-abrev   = b-ped-venda.nome-abrev
                   b-ped-item.nr-pedcli    = b-ped-venda.nr-pedcli
                   b-ped-item.nat-operacao = b-ped-venda.nat-operacao.
                   
        END.

        FOR EACH ped-ent OF ped-item NO-LOCK:

            FIND FIRST b-ped-ent EXCLUSIVE-LOCK
                 WHERE b-ped-ent.nome-abrev   = b-ped-venda.nome-abrev
                   AND b-ped-ent.nr-pedcli    = b-ped-venda.nr-pedcli 
                   AND b-ped-ent.nr-sequencia = ped-ent.nr-sequencia
                   AND b-ped-ent.it-codigo    = ped-ent.it-codigo   
                   AND b-ped-ent.cod-refer    = ped-ent.cod-refer   
                   AND b-ped-ent.nr-entrega   = ped-ent.nr-entrega   NO-ERROR.
            IF AVAIL b-ped-ent THEN DO:
            
              if ped-venda.cod-estabel = "422" OR ped-venda.cod-estabel = "412" THEN   /*solic-318*/ 
                BUFFER-COPY ped-ent EXCEPT nome-abrev nr-pedcli 
                                           cod-sit-ent cod-sit-pre dt-canent qt-alocada qt-atendida 
                                           qt-devolvida qt-fatenf qt-log-aloca qt-pendente qt-trans-mp 
                                           qt-transfer qtd-aloc-op qtd-reporta-op-ped
                   TO b-ped-ent.
                
                else
                 BUFFER-COPY ped-ent EXCEPT nome-abrev nr-pedcli  TO b-ped-ent.

            END.
            ELSE DO:
                CREATE b-ped-ent.
                BUFFER-COPY ped-ent EXCEPT nome-abrev nr-pedcli TO b-ped-ent.
                ASSIGN b-ped-ent.nome-abrev = b-ped-venda.nome-abrev
                       b-ped-ent.nr-pedcli  = b-ped-venda.nr-pedcli.
            END.
        END.

        FIND FIRST if-ped-item EXCLUSIVE-LOCK
             WHERE if-ped-item.nome-abrev   = ped-item.nome-abrev  
               AND if-ped-item.nr-pedcli    = ped-item.nr-pedcli   
               AND if-ped-item.nr-sequencia = ped-item.nr-sequencia
               AND if-ped-item.it-codigo    = ped-item.it-codigo   
               AND if-ped-item.cod-refer    = ped-item.cod-refer NO-ERROR.
        IF NOT AVAIL if-ped-item THEN DO:
            CREATE if-ped-item.
            ASSIGN if-ped-item.nome-abrev   = ped-item.nome-abrev  
                   if-ped-item.nr-pedcli    = ped-item.nr-pedcli   
                   if-ped-item.nr-sequencia = ped-item.nr-sequencia
                   if-ped-item.it-codigo    = ped-item.it-codigo   
                   if-ped-item.cod-refer    = ped-item.cod-refer.
        END.

        ASSIGN if-ped-item.vl-preori        = ped-item.vl-preori       
               if-ped-item.vl-preori-un-fat = ped-item.vl-preori-un-fat
               if-ped-item.vl-pretab        = ped-item.vl-pretab       
               if-ped-item.vl-preuni        = ped-item.vl-preuni.
    END.

    FOR EACH cond-ped OF ped-venda NO-LOCK:

        FIND FIRST b-cond-ped EXCLUSIVE-LOCK
             WHERE b-cond-ped.nr-pedido    = b-ped-venda.nr-pedido
               AND b-cond-ped.nr-sequencia = cond-ped.nr-sequencia NO-ERROR.
        IF AVAIL b-cond-ped THEN DO:
            BUFFER-COPY cond-ped EXCEPT nr-pedido TO b-cond-ped.
        END.
        ELSE DO:
            CREATE b-cond-ped.
            BUFFER-COPY cond-ped EXCEPT nr-pedido TO b-cond-ped.
            ASSIGN b-cond-ped.nr-pedido = b-ped-venda.nr-pedido.
        END.
    END.

    /* Quando troca o estabelecimento do pedido */
    /* REGRA NAO ESTµ CORRETA PORQUE ELE NAO PEGA A NATUREZA ORIGINAL
    
    FIND FIRST if-estabelec NO-LOCK
         WHERE if-estabelec.cod-estab-orig  = ped-venda.cod-estabel
           AND if-estabelec.cod-estab-inter = "" NO-ERROR.
    IF AVAIL if-estabelec AND if-estabelec.cod-estab-dest <> b-ped-venda.cod-estabel THEN DO:

        FIND FIRST if-natur-oper OF if-estabelec
             WHERE if-natur-oper.nat-oper-pedido = ped-venda.nat-operacao NO-LOCK NO-ERROR.

       if avail if-natur-oper then do:  /*Edson - n∆o estava testando if avail*/
          
           RUN pi-alt-cliente (INPUT if-estabelec.cod-estab-dest, INPUT YES).
   
           ASSIGN b-ped-venda.cod-estabel      = if-estabelec.cod-estab-dest
                  if-ped-venda.cod-estab-atend = if-estabelec.cod-estab-dest.
       end.        
    END.*/

    CREATE tt-ped-completo.
    ASSIGN tt-ped-completo.nr-pedido = b-ped-venda.nr-pedido.

END PROCEDURE. /* pi-atualiz-ped-relac */


PROCEDURE pi-aprova :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE h-bodi261 AS HANDLE      NO-UNDO.
    DEFINE VARIABLE h-bodi159cal AS HANDLE      NO-UNDO.
    DEFINE VARIABLE h-bodi159mps AS HANDLE      NO-UNDO.

    FIND FIRST param-global NO-LOCK NO-ERROR.
    
        
    assign ped-venda.desc-forc-cr = "Aprovaá∆o autom†tica para pedido Intercompany."
           ped-venda.dt-apr-cred  = TODAY
           ped-venda.cod-sit-aval = 3
           ped-venda.quem-aprovou = "Autom†tica"
           ped-venda.dsp-pre-fat  = YES.


    find first tt-ped-comp-ava where tt-ped-comp-ava.nr-pedido = ped-venda.nr-pedido no-error.
    if not avail tt-ped-comp-ava then do:
        CREATE tt-ped-comp-ava.
        ASSIGN tt-ped-comp-ava.nr-pedido = ped-venda.nr-pedido.
    end.
    /* Atualizaá∆o de Cotas */
    IF AVAIL param-global AND
       param-global.modulo-08 THEN DO:

       IF  NOT VALID-HANDLE(h-bodi261) OR 
           h-bodi261:TYPE      <> "PROCEDURE":U OR 
           h-bodi261:FILE-NAME <> "dibo/bodi261.p":U THEN 
           RUN dibo/bodi261.p PERSISTENT SET h-bodi261 NO-ERROR.

        IF ped-venda.cod-sit-com <> 1 THEN 
           RUN setarLogAtualizacao IN h-bodi261.     
        RUN atualizarCotasPedido IN h-bodi261(INPUT ROWID(ped-venda),
                                              INPUT 1, /*atualiza*/
                                              INPUT TABLE tt-ped-item-cotas).

       IF VALID-HANDLE(h-bodi261) THEN DO:
          DELETE PROCEDURE h-bodi261.
          ASSIGN h-bodi261 = ?.
       END.    
    END.

    /* antigo cd4100*/
    IF  NOT VALID-HANDLE(h-bodi159cal) OR 
        h-bodi159cal:TYPE      <> "PROCEDURE":U OR 
        h-bodi159cal:FILE-NAME <> "dibo/bodi159cal.p":U THEN 
        RUN dibo/bodi159cal.p PERSISTENT SET h-bodi159cal.
    RUN setInvoicingAvaiable in h-bodi159cal(INPUT ROWID(ped-venda)).
    IF VALID-HANDLE(h-bodi159cal) THEN DO:
       DELETE PROCEDURE h-bodi159cal.
       ASSIGN h-bodi159cal = ?.
    END.    

    if param-global.modulo-mp then do:
       if  not valid-handle(h-bodi159mps) or
          h-bodi159mps:type      <> "PROCEDURE":U or
          h-bodi159mps:file-name <> "dibo/bodi159mps.p":U then
          run dibo/bodi159mps.p persistent set h-bodi159mps.
          run pi-transacao-mp in h-bodi159mps(input rowid(ped-venda),
                                              input rowid(ped-venda),
                                              input "ModificarPedido":U).

       if  valid-handle(h-bodi159mps) then do:
           delete procedure h-bodi159mps.
           assign h-bodi159mps = ?.
       end.     
    end.   

END PROCEDURE.

 

PROCEDURE pi-atualiza-pai:
/* Atualiza pedido pai a partir do filho*/

            BUFFER-COPY ped-venda EXCEPT 
                                     nr-pedido 
                                     nr-pedcli 
                                     cod-emitente 
                                     nome-abrev 
                                     cod-estabel 
                                     cond-redespa 
                                     nat-operacao 
                                     cidade-cif 
                                     nome-abrev-tri 
                                     nome-transp 
                                     nome-tr-red 
                                     cod-mensagem 
                                      
                                      /*edson----- */
                                       
                                        dt-devolucao
                                        cod-entrega
                                        local-entreg
                                        bairro
                                        cidade
                                        estado
                                        cep
                                        caixa-postal
                                        pais
                                        cgc
                                        ins-estadual
                                        cod-sit-ped
                                        ind-aprov
                                        quem-aprovou
                                        dt-apr-cred
                                        vl-tot-ped
                                        vl-liq-ped
                                        vl-liq-abe
                                        user-aprov
                                        vl-mer-abe
                                        cod-sit-aval
                                        desc-bloq-cr
                                        desc-forc-cr
                                        cod-sit-pre
                                        vl-cred-lib
                                        aprov-forcado
                                        dsp-pre-fat
                                        cod-gr-cli
                                        atendido
                                        ind-ent-completa
                                        cod-sit-com
                                        cod-entrega-tri
                                        cdn-motiv-reprovac-cr
                                        char-1
                                        char-2 
                                      /*------------*/
                                     
                                     TO b-ped-venda.
    
    
    
    FOR EACH ped-antecip NO-LOCK
       WHERE ped-antecip.nr-pedido = ped-venda.nr-pedido:

        FIND FIRST b-ped-antecip EXCLUSIVE-LOCK
             WHERE b-ped-antecip.nr-pedido = b-ped-venda.nr-pedido NO-ERROR.
        IF AVAIL b-ped-antecip THEN DO:
            BUFFER-COPY ped-antecip EXCEPT nr-pedido TO b-ped-antecip.
        END.
        ELSE DO:
            CREATE b-ped-antecip.
            BUFFER-COPY ped-antecip EXCEPT nr-pedido TO b-ped-antecip.
            ASSIGN b-ped-antecip.nr-pedido = b-ped-venda.nr-pedido.
        END.
            
    END.

    FOR EACH ped-repre NO-LOCK
       WHERE ped-repre.nr-pedido = ped-venda.nr-pedido:

        FIND FIRST b-ped-repre EXCLUSIVE-LOCK
             WHERE b-ped-repre.nr-pedido         = b-ped-venda.nr-pedido
               AND b-ped-repre.nome-ab-rep       = ped-repre.nome-ab-rep      
               AND b-ped-repre.cod-classificador = ped-repre.cod-classificador NO-ERROR.
        IF AVAIL b-ped-repre THEN DO:
            BUFFER-COPY ped-repre EXCEPT nr-pedido TO b-ped-repre.
        END.
        ELSE DO:
            CREATE b-ped-repre.
            BUFFER-COPY ped-repre EXCEPT nr-pedido TO b-ped-repre.
            ASSIGN b-ped-repre.nr-pedido = b-ped-venda.nr-pedido.
        END.
    END.

    FOR EACH ped-item OF ped-venda NO-LOCK:
 

        FIND FIRST b-ped-item EXCLUSIVE-LOCK
             WHERE b-ped-item.nome-abrev   = b-ped-venda.nome-abrev
               AND b-ped-item.nr-pedcli    = b-ped-venda.nr-pedcli
               AND b-ped-item.nr-sequencia = ped-item.nr-sequencia
               AND b-ped-item.it-codigo    = ped-item.it-codigo   
               AND b-ped-item.cod-refer    = ped-item.cod-refer and
               b-ped-item.ind-componen < 3 and
               b-ped-item.cod-sit-item < 3 NO-ERROR.
        IF AVAIL b-ped-item THEN DO:
            FIND FIRST b-ped-item EXCLUSIVE-LOCK
             WHERE b-ped-item.nome-abrev   = b-ped-venda.nome-abrev
               AND b-ped-item.nr-pedcli    = b-ped-venda.nr-pedcli
               AND b-ped-item.nr-sequencia = ped-item.nr-sequencia
               AND b-ped-item.it-codigo    = ped-item.it-codigo   
               AND b-ped-item.cod-refer    = ped-item.cod-refer  NO-ERROR.

                 IF AVAIL b-ped-item THEN DO:

                       BUFFER-COPY ped-item EXCEPT nome-abrev nr-pedcli nat-operacao ind-icm-ret per-des-icms
                       
                        qt-alocada qt-atendida qt-devolvida 
                        qt-fatenf qt-log-aloca qt-ordens qt-pendente 
                        qtd-alocar qt-transfer qt-trans-mp 
                        nr-ord-produ nr-ordem cod-sit-item cod-sit-pre to 
                        b-ped-item.
                       
                 end.
                 
                 
                 
            
        END.
        

        FOR EACH ped-ent OF ped-item :

            IF  ped-item.qt-pedida <> ped-ent.qt-pedida THEN  ped-ent.qt-pedida = ped-item.qt-pedida.

            FIND FIRST b-ped-ent EXCLUSIVE-LOCK
                 WHERE b-ped-ent.nome-abrev   = b-ped-venda.nome-abrev
                   AND b-ped-ent.nr-pedcli    = b-ped-venda.nr-pedcli 
                   AND b-ped-ent.nr-sequencia = ped-ent.nr-sequencia
                   AND b-ped-ent.it-codigo    = ped-ent.it-codigo   
                   AND b-ped-ent.cod-refer    = ped-ent.cod-refer   
                   AND b-ped-ent.nr-entrega   = ped-ent.nr-entrega   NO-ERROR.
            IF AVAIL b-ped-ent THEN DO:
            
            
                  FIND FIRST b-ped-item no-LOCK
                       WHERE b-ped-item.nome-abrev   = b-ped-venda.nome-abrev
                         AND b-ped-item.nr-pedcli    = b-ped-venda.nr-pedcli
                         AND b-ped-item.nr-sequencia = ped-item.nr-sequencia
                         AND b-ped-item.it-codigo    = ped-item.it-codigo   
                         AND b-ped-item.cod-refer    = ped-item.cod-refer and
                         b-ped-item.ind-componen < 3 and
                         b-ped-item.cod-sit-item < 3 NO-ERROR.
          
            
            
            
            
               IF AVAIL b-ped-item THEN DO:
                
                BUFFER-COPY ped-ent EXCEPT nome-abrev nr-pedcli 
                                           cod-sit-ent cod-sit-pre dt-canent qt-alocada qt-atendida 
                                           qt-devolvida qt-fatenf qt-log-aloca qt-pendente qt-trans-mp 
                                           qt-transfer qtd-aloc-op qtd-reporta-op-ped
                   TO b-ped-ent.
                  IF  b-ped-item.qt-pedida <> b-ped-ent.qt-pedida THEN  b-ped-ent.qt-pedida = b-ped-item.qt-pedida.
                
               END.

            END.
             
        END.
    END.

    FOR EACH cond-ped OF ped-venda NO-LOCK:

        FIND FIRST b-cond-ped EXCLUSIVE-LOCK
             WHERE b-cond-ped.nr-pedido    = b-ped-venda.nr-pedido
               AND b-cond-ped.nr-sequencia = cond-ped.nr-sequencia NO-ERROR.
        IF AVAIL b-cond-ped THEN DO:
            BUFFER-COPY cond-ped EXCEPT nr-pedido TO b-cond-ped.
        END.
        ELSE DO:
            CREATE b-cond-ped.
            BUFFER-COPY cond-ped EXCEPT nr-pedido TO b-cond-ped.
            ASSIGN b-cond-ped.nr-pedido = b-ped-venda.nr-pedido.
        END.
    END.

    b-ped-venda.completo = no.

    CREATE tt-ped-completo.
    ASSIGN tt-ped-completo.nr-pedido = b-ped-venda.nr-pedido.
    
    run pi-completa-ped.


END PROCEDURE.

procedure pi-ajusta-nome-abrev.
   
    
        /* Quando troca o estabelecimento do pedido */
    FIND FIRST if-estabelec NO-LOCK
             WHERE if-estabelec.cod-estab-orig  = ped-venda.cod-estabel
               AND if-estabelec.cod-estab-inter = "" and
                   if-estabelec.cod-estab-dest = if-ped-venda.cod-estab-atend  NO-ERROR.
    IF AVAIL if-estabelec  THEN DO:

            FIND FIRST if-natur-oper OF if-estabelec
                 WHERE if-natur-oper.nat-oper-pedido = if-ped-venda.nat-oper-orig NO-LOCK NO-ERROR.

            if avail if-natur-oper then do:  /*Edson - n∆o estava testando if avail*/


                ASSIGN ped-venda.nat-operacao = if-natur-oper.nat-oper-v-ung.

                 

               RUN pi-alt-cliente (INPUT if-estabelec.cod-estab-dest, INPUT YES).
               IF RETURN-VALUE = "NO-ERROR" THEN
                    RETURN "NO-ERROR":U.
           end.        
    END.

   

end procedure.


