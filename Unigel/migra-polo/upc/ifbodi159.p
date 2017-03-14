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

define buffer if-ped-venda for espmulti.if-ped-venda.

define buffer bf-ped-venda for ped-venda.
define buffer b-ped-venda for ped-venda.

{include/i-prgvrs.i IFBODI159 2.00.00.000}  /*** 010000 ***/
/*******************************************************************************
**  Programa: IFBODI159
**  Objetivo: Tratamento para mod/del de pedidos relacionados IF
**  Autor...: Vertice
**  Data....: 26/02/2011
*******************************************************************************/

 
    
{include/i-epc200.i1}
{utp/ut-glob.i}

def input param  p-ind-event  as char          no-undo.
def input-output param table for tt-epc.
def var l-pai-total as logical no-undo.
def var l-432-polo as logical no-undo.
DEF VAR h-BO                AS HANDLE NO-UNDO.
DEFINE VARIABLE l-erro AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE idx AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-progs AS CHARACTER   NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pd4000-atu-transp AS LOGICAL NO-UNDO.

DEF TEMP-TABLE tt-ped-venda LIKE ped-venda 
          FIELD r-rowid AS ROWID. 
 
    
IF p-ind-event = "aftercreateRecord"  THEN DO:
    c-progs = "".
    DO idx = 1 TO 20:         
        IF PROGRAM-NAME(idx) <> ? THEN
           c-progs = c-progs + PROGRAM-NAME(idx).
    END.
    

    FIND FIRST tt-epc 
         WHERE tt-epc.cod-event     = p-ind-event
           AND tt-epc.cod-parameter = "Table-Rowid" NO-ERROR.
    IF AVAIL tt-epc THEN DO:
        FIND FIRST ped-venda EXCLUSIVE-LOCK
             WHERE ROWID(ped-venda) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.
        
        IF AVAIL ped-venda /*AND ped-venda.completo*/ THEN DO:
    
            IF  INDEX (c-progs,"pi-btOrderCopy" )  > 0 OR INDEX (c-progs,"copyOrder" ) > 0 THEN DO:
                if  ped-venda.cidade-cif <> ""  and  
                  substring(ped-venda.char-2,109,8) <> "0" and substring(ped-venda.char-2,109,8) <> " " then
                     ped-venda.cidade-cif = "".

            END.
        END.
    END.

END.

IF p-ind-event = "afterupdateRecord" THEN DO:
    c-progs = "".
    DO idx = 1 TO 20:         
        IF PROGRAM-NAME(idx) <> ? THEN
           c-progs = c-progs + PROGRAM-NAME(idx).
    END.
    
    FIND FIRST tt-epc 
         WHERE tt-epc.cod-event     = p-ind-event
           AND tt-epc.cod-parameter = "Table-Rowid" NO-ERROR.
    IF AVAIL tt-epc THEN DO:
        FIND FIRST ped-venda EXCLUSIVE-LOCK
             WHERE ROWID(ped-venda) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.
        
        IF AVAIL ped-venda /*AND ped-venda.completo*/ THEN DO:
                      
            /* Caso altere no pedido final */
            FIND FIRST if-ped-venda
                 WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido  NO-ERROR.
            IF AVAIL if-ped-venda THEN DO:
                find first b-ped-venda 
                     where b-ped-venda.nr-pedido =  if-ped-venda.nr-pedido no-lock no-error.
                if avail b-ped-venda AND (b-ped-venda.cod-estabel = "422" OR b-ped-venda.cod-estabel = "412") then do:   /*solic-318*/ 
                    assign if-ped-venda.cod-estab-atend = ped-venda.cod-estabel.
                    run pi-replica-data-tipo-filho.
                END.
            END.
            ELSE DO:
                FIND FIRST if-ped-venda no-LOCK
                     WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido 
                       AND if-ped-venda.nr-pedido-relac <> 0 NO-ERROR.
                IF AVAIL if-ped-venda THEN DO:
                    IF (ped-venda.cod-estabel = "422" OR ped-venda.cod-estabel = "412") THEN    /*solic-318*/ 
                        run pi-replica-data-tipo.
                    
                    find first b-ped-venda 
                         where b-ped-venda.nr-pedido =  if-ped-venda.nr-pedido-relac EXCLUSIVE-LOCK no-error.
                                             
                    c-progs = "".
                    DO idx = 1 TO 20:         
                        IF PROGRAM-NAME(idx) <> ? THEN
                           c-progs = c-progs + PROGRAM-NAME(idx).
                    END.

                    if avail b-ped-venda AND INDEX (c-progs,"pd4000") > 0 THEN DO:

                        if trim(ped-venda.observacoes) <> "" then 
                            assign b-ped-venda.observacoes = ped-venda.observacoes
                                   ped-venda.observacoes = "".

                        IF trim(ped-venda.nome-tr-red) <> "" THEN
                            ASSIGN b-ped-venda.nome-tr-red = ped-venda.nome-tr-red
                                   ped-venda.nome-tr-red   = "".

                        IF trim(ped-venda.nome-trans) <> "" THEN
                            ASSIGN b-ped-venda.nome-transp = ped-venda.nome-trans
                                   ped-venda.nome-trans    = "".

                        IF trim(ped-venda.cidade-cif) <> "" THEN
                            ASSIGN
                               b-ped-venda.cidade-cif  = ped-venda.cidade-cif
                               ped-venda.cidade-cif    = "".

                        IF SUBSTRING(ped-venda.char-2,109,8) <> "9" THEN DO:                        
                            OVERLAY(b-ped-venda.char-2,109,8) = SUBSTR(ped-venda.char-2,109,8).
                            OVERLAY(ped-venda.char-2,109,8) = "9".
                        END.

                        ASSIGN wh-pd4000-atu-transp = YES.

                    END.
                END.
                    
            END.
        end.      
    end.      
end.

IF p-ind-event = "AfterValidateRecord" THEN DO:
    c-progs = "".
    DO idx = 1 TO 20:         
        IF PROGRAM-NAME(idx) <> ? THEN
           c-progs = c-progs + PROGRAM-NAME(idx).
    END.
    FIND FIRST tt-epc 
         WHERE tt-epc.cod-event     = p-ind-event
           AND tt-epc.cod-parameter = "Table-Rowid" NO-ERROR.
    IF AVAIL tt-epc THEN DO:
        FIND FIRST ped-venda NO-LOCK
       WHERE ROWID(ped-venda) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.
        IF AVAIL ped-venda THEN DO:
            FIND FIRST tt-epc 
                 WHERE tt-epc.cod-event     = p-ind-event
                   AND tt-epc.cod-parameter = "Object-Handle" NO-ERROR.
            IF AVAIL tt-epc THEN DO:
            
            
            
                    IF  CAN-FIND(FIRST ped-item of ped-venda where ped-item.cod-refer = "00000001" and ped-item.ind-componen < 3) THEN DO:
            
                        RUN _insertErrorManual IN widget-handle(tt-epc.val-parameter) (INPUT 17006,
                                                                                       INPUT "ERROR",
                                                                                       INPUT "ERROR", 
                                                                                       INPUT "N∆o pode efetivar pedido com referància 00000001",
                                                                                       INPUT "Por favor Volte e termine a configuraá∆o",
                                                                                       INPUT "":U).
                        RETURN "NOK".
            
                    END.

            
            
            
           
                l-432-polo = no.
                
                find FIRST if-ped-venda 
                            WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido no-lock no-error.
 
                if avail if-ped-venda then do:
                    find first bf-ped-venda where bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido no-lock no-error.
                    if   (bf-ped-venda.cod-estabel = "422"  OR bf-ped-venda.cod-estabel = "412") then l-432-polo = yes.    /*solic-318*/ 
                    
                end.
                
                                        
                IF CAN-FIND(FIRST if-ped-venda 
                            WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido) and c-seg-usuario <> "elapare" and c-seg-usuario <> "xxrpolime" and c-seg-usuario <> "eltpala" and
                              l-432-polo = no  THEN DO:


                    IF NOT CAN-FIND(FIRST if-permis-alt-ped
                                    WHERE if-permis-alt-ped.cod-estabel = ped-venda.cod-estabel
                                      AND if-permis-alt-ped.cod-usuario = c-seg-usuario) THEN DO:
            
                        RUN _insertErrorManual IN widget-handle(tt-epc.val-parameter) (INPUT 17006,
                                                                                       INPUT "ERROR",
                                                                                       INPUT "ERROR", 
                                                                                       INPUT "Pedido Relacionado a Incentivo Fiscal n∆o pode ser modificado/excluido",
                                                                                       INPUT "Caso seja necess†rio, qualquer manutená∆o deve ser realizada no pedido original",
                                                                                       INPUT "":U).
                        RETURN "NOK".
            
                    END.
                END.
                
                                      
                        
                    
                            ASSIGN h-bo = WIDGET-HANDLE(tt-epc.val-parameter).
                            
                            RUN getRecord IN h-bo (OUTPUT TABLE tt-ped-venda).
                            
                            FIND FIRST tt-ped-venda NO-LOCK NO-ERROR.
                            IF AVAIL tt-ped-venda THEN DO:
                           
                              
                                  IF SUBSTRING(tt-ped-venda.nat-operacao,1,1) = "7" and
                                               tt-ped-venda.cod-rota = "" THEN DO: 
                                     
                                      
                                      
                                       RUN _insertErrorManual IN widget-handle(tt-epc.val-parameter) (INPUT 17006,
                                                                                       INPUT "ERROR",
                                                                                       INPUT "ERROR", 
                                                                                       INPUT "Campo Rota n∆o foi preenchido para Exportaá∆o ",
                                                                                       INPUT "Volte e informe uma rota v†lida para pedido com CFOP de Exportaá∆o",
                                                                                       INPUT "":U).
                                          RETURN "NOK".


                                  END.
                                  
                              l-erro = no.                                                         

                              if  tt-ped-venda.cidade-cif <> ""  and  
                                   substring(tt-ped-venda.char-2,109,8) <> "0" then l-erro = yes.
                              if  tt-ped-venda.cidade-cif = ""  and  (substring(tt-ped-venda.char-2,109,8) <> "1" and substring(tt-ped-venda.char-2,109,8) <> "9") then l-erro = yes.                               
 
                               
                              if l-erro  and
                                 INDEX (c-progs,"pi-btOrderCopy" )  = 0 AND INDEX (c-progs,"copy" ) = 0
                                  then  do:
                     
                                  
                                       RUN _insertErrorManual IN widget-handle(tt-epc.val-parameter) (INPUT 17006,
                                                                                       INPUT "ERROR",
                                                                                       INPUT "ERROR", 
                                                                                       INPUT "Informaá∆o de tipo de frete inv†lido ",
                                                                                       INPUT "Volte e informe um tipo de frete correto ou cidade CIF v†lida",
                                                                                       INPUT "":U).
                                          RETURN "NOK".
                              end.
                          END.
                                    
            END.
        END.
    END.
END.


procedure pi-replica-data-tipo.

define buffer b-ped-item for ped-item.
define buffer b-ped-ent for ped-ent.
define buffer bpai-ped-venda for ped-venda.
for each ped-item of ped-venda where  ped-item.ind-componen < 3 no-lock,
    each estabelec where estabelec.cod-estabel = ped-venda.cod-estabel no-lock,
      each  b-ped-venda exclusive-lock where b-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac ,
        each b-ped-item of b-ped-venda where b-ped-item.nr-sequencia = ped-item.nr-sequencia 
                                             exclusive-lock.
        
       
         
         assign
            b-ped-venda.dt-entrega  = ped-venda.dt-entrega            
            b-ped-venda.tp-pedido   = ped-venda.tp-pedido
            b-ped-item.dt-entrega   = ped-item.dt-entrega.
                                          
            find first bpai-ped-venda where rowid(bpai-ped-venda)  = rowid(ped-venda) exclusive-lock.
           
            if avail bpai-ped-venda then do:
            
                 if trim(ped-venda.observacoes) <> "" then 
                    assign b-ped-venda.observacoes = ped-venda.observacoes
                           bpai-ped-venda.observacoes = "".
              
                 if trim(bpai-ped-venda.nome-tr-red) <> "" then do:
                     find first transporte where transporte.nome-abrev = bpai-ped-venda.nome-tr-red no-lock no-error.

                     if avail transporte then 
                       assign b-ped-venda.nome-tr-red = bpai-ped-venda.nome-tr-red
                              bpai-ped-venda.nome-tr-red = "".
                              
                  end.
                  
                   if trim(bpai-ped-venda.nome-transp) <> "" and bpai-ped-venda.nome-transp <> "padr∆o" then do:
                     find first transporte where transporte.nome-abrev = bpai-ped-venda.nome-transp no-lock no-error.

                     if avail transporte then 
                       assign b-ped-venda.nome-transp = bpai-ped-venda.nome-transp
                              bpai-ped-venda.nome-transp = "".
                              
                  end.

                              
                 if trim(bpai-ped-venda.cidade-cif) <> "" then 
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
          
END.

end procedure.


procedure pi-replica-data-tipo-filho.

define buffer b-ped-item for ped-item.
define buffer b-ped-ent for ped-ent.
define buffer bfilho-ped-venda for ped-venda.

for each ped-item of ped-venda where ped-item.ind-componen < 3 no-lock,
      each estabelec where estabelec.cod-estabel = ped-venda.cod-estabel no-lock,
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
         
END.

end procedure.


RETURN "OK".




