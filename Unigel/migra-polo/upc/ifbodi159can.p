/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

   
define buffer if-ped-venda for espmulti.if-ped-venda.    

{include/i-prgvrs.i IFBODI159CAN 2.00.00.000}  /*** 010000 ***/
/*******************************************************************************
**  Programa: IFBODI159CAN
**  Objetivo: Tratamento para cancelamento de pedidos relacionados IF
**  Autor...: Vertice
**  Data....: 25/02/2011
*******************************************************************************/
{include/i-epc200.i1}

def input param  p-ind-event  as char          no-undo.
def input-output param table for tt-epc.

DEFINE BUFFER bf-ped-venda FOR ped-venda.
def var l-pai-total as logical no-undo.
def var l-filho as logical no-undo.

IF p-ind-event = "AfterValidateCancelation" THEN DO:
    FIND FIRST tt-epc 
         WHERE tt-epc.cod-event     = "afterValidateCancelation"
           AND tt-epc.cod-parameter = "TABLE-ROWID" NO-ERROR.
    IF AVAIL tt-epc THEN DO:
        FIND FIRST ped-venda NO-LOCK
       WHERE ROWID(ped-venda) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.
        IF AVAIL ped-venda /*AND ped-venda.completo*/ THEN DO:
            FIND FIRST tt-epc 
                 WHERE tt-epc.cod-event     = "afterValidateCancelation"
                   AND tt-epc.cod-parameter = "OBJECT-HANDLE" NO-ERROR.
            IF AVAIL tt-epc THEN DO:
            
                l-pai-total = no.
                l-filho = no.
                
                find FIRST if-ped-venda 
                            WHERE if-ped-venda.nr-pedido-relac = ped-venda.nr-pedido no-lock no-error.
                            
                if avail if-ped-venda then do:
                    l-filho = yes.
                    find first bf-ped-venda where bf-ped-venda.nr-pedido = if-ped-venda.nr-pedido and bf-ped-venda.cod-estabel = "422" no-lock no-error.
                    
                    if avail bf-ped-venda then do:
                    
                        if  bf-ped-venda.cod-sit-ped <> 3 and bf-ped-venda.cod-sit-ped <> 6 then do:
                            FIND FIRST motivo
                                WHERE motivo.ind-tp-trans = 1 NO-LOCK NO-ERROR.
                    
                     
                            run updateCancelation in widget-handle(tt-epc.val-parameter) (input ROWID(bf-ped-venda),
                                                                                          input "Pedido Relacionado Incentivo Fiscal",
                                                                                          input TODAY,
                                                                                          input motivo.cod-motivo).                                        
                            return "ok".                                                                                  
                         end.
                         else return "ok".   
                    end. 
                
                end.
                

            
                IF l-filho = yes   THEN DO:
                    RUN _insertErrorManual IN widget-handle(tt-epc.val-parameter) (INPUT 17006,
                                                                                   INPUT "ERROR",
                                                                                   INPUT "ERROR", 
                                                                                   INPUT "Pedido Relacionado a Incentivo Fiscal n∆o pode ser cancelado",
                                                                                   INPUT "O cancelamento dever† ocorrer no pedido original",
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
                        IF AVAIL bf-ped-venda then do:
                        
                         
                           if bf-ped-venda.cod-sit-ped <> 6 and bf-ped-venda.cod-sit-ped <> 3 THEN
                                run updateCancelation in widget-handle(tt-epc.val-parameter) (input ROWID(bf-ped-venda),
                                                                                              input "Pedido Relacionado Incentivo Fiscal",
                                                                                              input TODAY,
                                                                                              input motivo.cod-motivo).
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
