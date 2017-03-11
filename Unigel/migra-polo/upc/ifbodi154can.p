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

{include/i-prgvrs.i IFBODI154CAN 2.00.00.000}  /*** 010000 ***/
/*******************************************************************************
**  Programa: IFBODI154CAN
**  Objetivo: Tratamento para cancelamento de itens pedidos relacionados IF
**  Autor...: Vertice
**  Data....: 25/02/2011
*******************************************************************************/
{include/i-epc200.i1}

def input param  p-ind-event  as char          no-undo.
def input-output param table for tt-epc.
                                       
DEFINE BUFFER bf-ped-item FOR ped-item.
DEFINE BUFFER bf-ped-venda FOR ped-venda.
 

IF p-ind-event = "AfterValidateCancelation" THEN DO:
    FIND FIRST tt-epc 
         WHERE tt-epc.cod-event     = "afterValidateCancelation"
           AND tt-epc.cod-parameter = "TABLE-ROWID" NO-ERROR.
    IF AVAIL tt-epc THEN DO:
        FIND FIRST ped-item NO-LOCK
       WHERE ROWID(ped-item) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.
       
       
        IF AVAIL ped-item THEN
            FIND FIRST ped-venda OF ped-item NO-LOCK NO-ERROR.
        IF AVAIL ped-venda /*AND ped-venda.completo*/ THEN DO:
        
         
                            /*  message
                              ped-venda.nr-pedido
                              view-as alert-box.*/


            FIND FIRST tt-epc 
                 WHERE tt-epc.cod-event     = "afterValidateCancelation"
                   AND tt-epc.cod-parameter = "OBJECT-HANDLE" NO-ERROR.
            IF AVAIL tt-epc THEN DO:
                IF CAN-FIND(FIRST if-ped-venda 
                            WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido) THEN DO:
                            
                            
                    find FIRST if-ped-venda 
                            WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido no-lock no-error.
                            
                             /* message
                              ped-venda.nr-pedido
                              view-as alert-box.
                              */


                    find first bf-ped-venda where bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido and bf-ped-venda.cod-estabel = "422" no-lock no-error.
                    IF AVAIL bf-ped-venda  THEN DO:
                    
                        find first ord-prod where ord-prod.nr-pedido = bf-ped-venda.nr-pedcli and 
                                                  ord-prod.nome-abrev = bf-ped-venda.nome-abrev and
                                                  ord-prod.nr-sequencia = ped-item.nr-sequencia no-lock no-error. 
                        find first bf-ped-item OF bf-ped-venda NO-LOCK
                                 WHERE bf-ped-item.nr-sequencia = ped-item.nr-sequencia and bf-ped-item.it-codigo = ped-item.it-codigo and bf-ped-item.ind-componen < 3  no-error.
                                 

                        if avail ord-prod and avail bf-ped-item and bf-ped-item.cod-sit-item <> 6 and bf-ped-item.cod-sit-item <> 3 and bf-ped-venda.cod-sit-ped <> 6 and bf-ped-venda.cod-sit-ped <> 3 then do:
                           RUN _insertErrorManual IN widget-handle(tt-epc.val-parameter) (INPUT 17006,
                                                                                   INPUT "ERROR",
                                                                                   INPUT "ERROR", 
                                                                                   INPUT "Item Pedido Relacionado com Ordem de Produá∆o no pedido original",
                                                                                   INPUT "Favor desvincule a orden de produá∆o do pedido: " + string(bf-ped-venda.nr-pedido),
                                                                                   INPUT "":U).
                            RETURN "NOK".

                        end.                          

                                                           
                    
                    
                   /*   message
                              bf-ped-venda.nr-pedido
                              view-as alert-box.
*/
                     

                            for each  bf-ped-item OF bf-ped-venda NO-LOCK
                                 WHERE bf-ped-item.nr-sequencia = ped-item.nr-sequencia.
/*                              message
                              bf-ped-item.nr-sequencia skip
                              bf-ped-item.it-codigo
                              view-as alert-box.
                              */
                                  if bf-ped-item.cod-sit-item <> 6 and bf-ped-item.cod-sit-item <> 3 and bf-ped-venda.cod-sit-ped <> 6 and bf-ped-venda.cod-sit-ped <> 3 then do: 
                                  
                                         FIND FIRST motivo
                                             WHERE motivo.ind-tp-trans = 1 NO-LOCK NO-ERROR.                                                                                   
                                
                                         run updateCancelation in widget-handle(tt-epc.val-parameter) (input ROWID(bf-ped-item),
                                                                                              input "Item Pedido Relacionado Incentivo Fiscal",
                                                                                              input TODAY,
                                                                                              input motivo.cod-motivo).
                                                                                            

                                  end.
                            
                            end.
                            return "ok".
                            
                    end.        

                    
                     
                     
             
                            
                    RUN _insertErrorManual IN widget-handle(tt-epc.val-parameter) (INPUT 17006,
                                                                                   INPUT "ERROR",
                                                                                   INPUT "ERROR", 
                                                                                   INPUT "Item Pedido Relacionado a Incentivo Fiscal n∆o pode ser cancelado",
                                                                                   INPUT "O cancelamento dever† ocorrer no item pedido original",
                                                                                   INPUT "":U).
                    RETURN "NOK".
                END.
                ELSE DO:
                    FIND FIRST motivo
                         WHERE motivo.ind-tp-trans = 1 NO-LOCK NO-ERROR.
                    FIND FIRST if-ped-venda 
                         WHERE if-ped-venda.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
                    DO WHILE AVAIL if-ped-venda:
                        FIND FIRST bf-ped-venda NO-LOCK
                             WHERE bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido-relac NO-ERROR.
                        IF AVAIL bf-ped-venda THEN DO:
                            if bf-ped-venda.cod-sit-ped <> 6 and bf-ped-venda.cod-sit-ped <> 3 then do:
                            
                          /*  message  bf-ped-venda.nr-pedido "cancela"
                            view-as alert-box.
                            */
                                for each  bf-ped-item OF bf-ped-venda NO-LOCK
                                     WHERE bf-ped-item.nr-sequencia = ped-item.nr-sequencia  and
                                           bf-ped-item.cod-sit-item <> 6 and 
                                           bf-ped-item.cod-sit-item <> 3 :
                /*    message  bf-ped-venda.nr-pedido "cancela item " bf-ped-item.it-codigo bf-ped-item.nr-sequencia
                            view-as alert-box.  
*/
                          
                                    run updateCancelation in widget-handle(tt-epc.val-parameter) (input ROWID(bf-ped-item),
                                                                                              input "Item Pedido Relacionado Incentivo Fiscal",
                                                                                              input TODAY,
                                                                                              input motivo.cod-motivo).
            
                                end.      
                            end.                                                                   
                            FIND FIRST if-ped-venda 
                                 WHERE if-ped-venda.nr-pedido = bf-ped-venda.nr-pedido NO-LOCK NO-ERROR.
                        END.
                        ELSE LEAVE.
                    END.
                END.
            END.
        END.
    END.
END.

RETURN "OK".

