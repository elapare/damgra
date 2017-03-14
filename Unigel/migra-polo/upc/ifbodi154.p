/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i IFBODI154 2.00.00.000}  /*** 010000 ***/
/*******************************************************************************
**  Programa: IFBODI154
**  Objetivo: Tratamento para inc/mod/del de itens de pedidos relacionados IF
**  Autor...: Vertice
**  Data....: 26/02/2011
*******************************************************************************/
{include/i-epc200.i1}
{method/dbotterr.i}
{utp/ut-glob.i}

DEF TEMP-TABLE tt-ped-item-del LIKE ped-item
    FIELD r-rowid AS ROWID.

def input param  p-ind-event  as char          no-undo.
def input-output param table for tt-epc.
def var l-pai-total as logical no-undo.
def var l-432-polo as logical no-undo.
define buffer bf-ped-venda for ped-venda.
define buffer bf-ped-item for ped-item.
define buffer if-ped-venda for espmulti.if-ped-venda.
define buffer b-ped-venda for ped-venda.
DEFINE BUFFER b-ped-item  FOR ped-item.

DEF VAR h_bodi154      AS HANDLE  NO-UNDO. 
DEF VAR h-bodi154-erro AS HANDLE  NO-UNDO.
DEF VAR i-nr-pedido    AS INTEGER NO-UNDO.


IF p-ind-event = "afterupdateRecord" THEN DO:
   FIND FIRST tt-epc 
         WHERE tt-epc.cod-event     = p-ind-event
           AND tt-epc.cod-parameter = "Table-Rowid" NO-ERROR.
    IF AVAIL tt-epc THEN DO:
        FIND FIRST ped-item NO-LOCK
       WHERE ROWID(ped-item) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.
        IF AVAIL ped-item THEN
            FIND FIRST ped-venda OF ped-item NO-LOCK NO-ERROR.
        IF AVAIL ped-venda /*AND ped-venda.completo*/ THEN DO:
        
                   FIND FIRST if-ped-venda no-LOCK
                        WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido NO-ERROR.
                   IF  AVAIL if-ped-venda THEN DO: 
                        find first b-ped-venda where b-ped-venda.nr-pedido =  if-ped-venda.nr-pedido no-lock no-error.
                       
                        if avail if-ped-venda and avail b-ped-venda and (b-ped-venda.cod-estabel = "422" OR b-ped-venda.cod-estabel = "412" )  then run pi-replica-data-tipo-filho.  /*solic-318*/
                                    
                   END.
                   else do:
                   
                        fIND FIRST if-ped-venda no-LOCK
                             WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido and if-ped-venda.nr-pedido-relac <> 0 NO-ERROR.
                        IF  AVAIL if-ped-venda THEN DO:
                             if avail if-ped-venda and (ped-venda.cod-estabel = "422" OR ped-venda.cod-estabel = "412")  then run pi-replica-data-tipo. /*solic-318*/
                
                        end.
           
                    
                
                    
                    END.

    
    
    
        
        end.      
    end.      
end.           

IF p-ind-event = "AfterValidateRecord" THEN DO:
    FIND FIRST tt-epc 
         WHERE tt-epc.cod-event     = p-ind-event
           AND tt-epc.cod-parameter = "Table-Rowid" NO-ERROR.
    IF AVAIL tt-epc THEN DO:
        FIND FIRST ped-item NO-LOCK
       WHERE ROWID(ped-item) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.
        IF AVAIL ped-item THEN
            FIND FIRST ped-venda OF ped-item NO-LOCK NO-ERROR.
        IF AVAIL ped-venda /*AND ped-venda.completo*/ THEN DO:
            FIND FIRST tt-epc 
                 WHERE tt-epc.cod-event     = p-ind-event
                   AND tt-epc.cod-parameter = "Object-Handle" NO-ERROR.
            IF AVAIL tt-epc THEN DO:
            
 
                l-432-polo  = no.
                
                find FIRST if-ped-venda 
                            WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido no-lock no-error.
                            
                if avail if-ped-venda then do:
                    find first bf-ped-venda where bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido no-lock no-error.
                    if avail bf-ped-venda then do:
                    
                       if bf-ped-venda.cod-estabel = "422" OR bf-ped-venda.cod-estabel = "412" then l-432-polo = yes.  /*solic-318*/
                      
                    end.
                end.

       if index (  program-name(3) +
                   program-name(4) +
                   program-name(5) +
                   program-name(6) 
                   ,"delete") > 0 then   l-432-polo = no.
       if index (  program-name(3) +
                   program-name(4) +
                   program-name(5) +
                   program-name(6) 
                   ,"create") > 0 then   l-432-polo = no.                   

                IF CAN-FIND(FIRST if-ped-venda
                            WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido)  and l-432-polo = no THEN DO:


                    IF NOT CAN-FIND(FIRST if-permis-alt-ped
                                    WHERE if-permis-alt-ped.cod-estabel = ped-venda.cod-estabel
                                      AND if-permis-alt-ped.cod-usuario = c-seg-usuario) THEN DO:

                        RUN _insertErrorManual IN widget-handle(tt-epc.val-parameter) (INPUT 17006,
                                                                                       INPUT "ERROR",
                                                                                       INPUT "ERROR", 
                                                                                       INPUT "Item Pedido Relacionado a Incentivo Fiscal n∆o pode ser incluso/modificado/excluido",
                                                                                       INPUT "Caso seja necess†rio, qualquer manutená∆o deve ser realizada no item pedido original",
                                                                                       INPUT "":U).
                        RETURN "NOK".
                    END.
                END.
            END.
        END.
    END.
END.

/* Apaga Item Unigel Comercial */
IF p-ind-event = "beforeDeleteRecord" THEN DO:
    FIND FIRST tt-epc 
         WHERE tt-epc.cod-event     = p-ind-event
           AND tt-epc.cod-parameter = "Table-Rowid" NO-ERROR.
    IF AVAIL tt-epc THEN DO:
        FIND FIRST ped-item NO-LOCK
            WHERE ROWID(ped-item) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.    
        IF AVAIL ped-item THEN DO:

            /* Inserir mensagem de erro */
            FIND FIRST tt-epc 
                 WHERE tt-epc.cod-event     = p-ind-event
                   AND tt-epc.cod-parameter = "Object-Handle" NO-ERROR.
            IF AVAIL tt-epc THEN
                ASSIGN h-bodi154-erro = WIDGET-HANDLE(tt-epc.val-parameter).
            
            FIND FIRST ped-venda OF ped-item NO-LOCK NO-ERROR.

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
                IF AVAIL b-ped-venda THEN DO:
                    RUN dibo/bodi154.p PERSISTENT SET h_bodi154.
                    FOR EACH b-ped-item
                       WHERE b-ped-item.nome-abrev   = b-ped-venda.nome-abrev
                         AND b-ped-item.nr-pedcli    = b-ped-venda.nr-pedcli
                         AND b-ped-item.nr-sequencia = ped-item.nr-sequencia
                         AND b-ped-item.it-codigo    = ped-item.it-codigo   
                         AND b-ped-item.cod-refer    = ped-item.cod-refer NO-LOCK:

                        EMPTY TEMP-TABLE tt-ped-item-del.

                        CREATE tt-ped-item-del.
                        BUFFER-COPY b-ped-item TO tt-ped-item-del.
                        ASSIGN tt-ped-item-del.r-rowid = ROWID(b-ped-item).

                        RUN emptyRowErrors  IN h_bodi154.
                        RUN openQueryStatic IN h_bodi154(input "Main":U).
                        RUN gotokey         IN h_bodi154(INPUT tt-ped-item-del.nome-abrev,
                                                         INPUT tt-ped-item-del.nr-pedcli,
                                                         INPUT tt-ped-item-del.nr-sequencia,
                                                         INPUT tt-ped-item-del.it-codigo,
                                                         INPUT tt-ped-item-del.nr-sequencia).
                        RUN repositionrecord IN h_bodi154 (INPUT ROWID(b-ped-item)).
                        RUN setRecord        IN h_bodi154 (INPUT TABLE tt-ped-item-del).
                        RUN deleterecord     IN h_bodi154.
                        RUN getRowErrors     IN h_bodi154(OUTPUT TABLE RowErrors APPEND).
                    END.
                    DELETE PROCEDURE h_bodi154.

                    IF VALID-HANDLE(h-bodi154-erro) THEN DO:
                        FOR EACH RowErrors:
                            RUN _insertErrorManual IN h-bodi154-erro (INPUT RowErrors.ErrorNumber,
                                                                      INPUT "EMS":U,
                                                                      INPUT RowErrors.ErrorSubType,
                                                                      INPUT RowErrors.ErrorDescription,
                                                                      INPUT RowErrors.ErrorHelp,
                                                                      INPUT "":U).
                
                            DELETE RowErrors.
                        END.
                        RETURN "NOK".
                    END.
                END.
            END.
        END.
    END.
END.



procedure pi-replica-data-tipo.

define buffer b-ped-item for ped-item.
define buffer b-ped-ent for ped-ent.
define buffer bpai-ped-venda for ped-venda.
if ped-item.ind-componen < 3 then do:
   for each estabelec where estabelec.cod-estabel = ped-venda.cod-estabel no-lock,
      each  b-ped-venda exclusive-lock where b-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac ,
        each b-ped-item of b-ped-venda where b-ped-item.nr-sequencia = ped-item.nr-sequencia 
                                             exclusive-lock.
        
       
         
         assign
            b-ped-venda.dt-entrega  = ped-venda.dt-entrega            
            b-ped-venda.tp-pedido   = ped-venda.tp-pedido
            b-ped-item.dt-entrega   = ped-item.dt-entrega.
                                          
            find first bpai-ped-venda where rowid(bpai-ped-venda)  = rowid(ped-venda) exclusive-lock.
           
            if avail bpai-ped-venda then do:
            
                 if ped-venda.observacoes <> "" then 
                    assign b-ped-venda.observacoes = ped-venda.observacoes
                           bpai-ped-venda.observacoes = "".
              
                 if bpai-ped-venda.nome-tr-red <> "" then do:
                     find first transporte where transporte.nome-abrev = bpai-ped-venda.nome-tr-red no-lock no-error.

                     if avail transporte then 
                       assign b-ped-venda.nome-tr-red = bpai-ped-venda.nome-tr-red
                              bpai-ped-venda.nome-tr-red = "".
                              
                  end.
                  
                   if bpai-ped-venda.nome-transp <> "" and bpai-ped-venda.nome-transp <> "padr∆o" then do:
                     find first transporte where transporte.nome-abrev = bpai-ped-venda.nome-transp no-lock no-error.

                     if avail transporte then 
                       assign b-ped-venda.nome-transp = bpai-ped-venda.nome-transp
                              bpai-ped-venda.nome-transp = "".
                              
                  end.

                              
                 if bpai-ped-venda.cidade-cif <> "" then 
                       assign b-ped-venda.cidade-cif = bpai-ped-venda.cidade-cif
                              bpai-ped-venda.cidade-cif = "".
              
                         
                                      
                                      
            end.
        
        for each b-ped-ent of b-ped-item exclusive-lock.
             b-ped-ent.dt-entrega = b-ped-item.dt-entrega .
        
        end.
        
        for first pd-compl-pedido where 
                        pd-compl-pedido.ep-codigo    = estabelec.ep-codigo and
                        pd-compl-pedido.nr-pedido    = ped-venda.nr-pedido and
                        pd-compl-pedido.nr-sequencia = ped-item.nr-sequencia   no-lock.
   
        
            run pdp\upc\trw-pd-compl-pedido-manual (input rowid(pd-compl-pedido)).
         
         end.
     end.     
END.

end procedure.


procedure pi-replica-data-tipo-filho.

define buffer b-ped-item for ped-item.
define buffer b-ped-ent for ped-ent.
define buffer bfilho-ped-venda for ped-venda.

if ped-item.ind-componen < 3 then do:
     for each estabelec where estabelec.cod-estabel = ped-venda.cod-estabel no-lock,
      each  b-ped-venda exclusive-lock where b-ped-venda.nr-pedido = if-ped-venda.nr-pedido ,
        each b-ped-item of b-ped-venda where b-ped-item.nr-sequencia = ped-item.nr-sequencia 
                                             exclusive-lock.
        
       
         
         assign
            b-ped-venda.dt-entrega  = ped-venda.dt-entrega            
            b-ped-venda.tp-pedido   = ped-venda.tp-pedido
            b-ped-item.dt-entrega   = ped-item.dt-entrega.
                                          
           
             
        
        for each b-ped-ent of b-ped-item exclusive-lock.
             b-ped-ent.dt-entrega = b-ped-item.dt-entrega .
        
        end.
        
        for first pd-compl-pedido where 
                        pd-compl-pedido.ep-codigo    = estabelec.ep-codigo and
                        pd-compl-pedido.nr-pedido    = ped-venda.nr-pedido and
                        pd-compl-pedido.nr-sequencia = ped-item.nr-sequencia   no-lock.
   
        
            run pdp\upc\trw-pd-compl-pedido-manual (input rowid(pd-compl-pedido)).
         
        end.
    end.     
END.

end procedure.
