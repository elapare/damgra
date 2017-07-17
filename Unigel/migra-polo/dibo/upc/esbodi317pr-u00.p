/*************************************************************************************
**  Programa: ESBODI317PR-U00.P
**  Objetivo: Tratamento da Unidade de Neg¢cio para o Faturamento referente a BOBINA
**  Data....: 03/12/2003
**  VersÆo..: 2.04.001 - Edgar Bispo
**          Fazer a tratativa para os embarques que tenham a regra de n‚gocio BB X KG
**************************************************************************************/
def buffer empresa for mgmulti.empresa.
{include/i-prgvrs.i ESBODI317PR-U00 2.04.00.001}
{include/i-epc200.i1} /* Definicao da temp-table tt-epc */

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.



    DEFINE VARIABLE d-peso-liq AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE d-peso-bru AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE d-peso-item AS DECIMAL NO-UNDO.
    DEFINE VARIABLE l-peso AS LOGICAL    NO-UNDO.
    DEF VAR r-wt-docto      AS ROWID NO-UNDO.

  
/*----- CONVERSÇO DE KG PARA BB - EDGAR BISPO ------------*/
run dibo/upc/esBODI317PR-u01.p (INPUT p-ind-event,
                             INPUT-OUTPUT TABLE tt-epc).
                             
FIND FIRST tt-epc 
     WHERE tt-epc.cod-event     = p-ind-event   AND
           tt-epc.cod-parameter = "Table-Rowid" NO-LOCK NO-ERROR.

IF  AVAIL tt-epc AND  p-ind-event = "afterGeraWtItDoctoComItensDoEmbarque" THEN  DO:

    ASSIGN r-wt-docto = TO-ROWID(tt-epc.val-parameter).
          
    FIND FIRST wt-docto
        WHERE rowid(wt-docto) = r-wt-docto NO-ERROR.
        
         IF AVAIL wt-docto  THEN DO:
            find natur-oper where natur-oper.nat-operacao = wt-docto.nat-operacao no-lock.
            


         end.
        
    
    IF AVAIL wt-docto and avail natur-oper 
            and natur-oper.tipo = 2 and 
            natur-oper.baixa-estoq THEN DO:

           FIND FIRST estabelec WHERE estabelec.cod-estabel = wt-docto.cod-estabel NO-LOCK NO-ERROR.

        IF AVAIL estabelec AND (estabelec.ep-codigo = "420" or estabelec.ep-codigo = "410" or estabelec.ep-codigo = "700" or estabelec.cod-estabel = "442" or estabelec.cod-estabel = "434" or estabelec.cod-estabel = "432" or estabelec.cod-estabel = "443") THEN DO:  /*solic-318*/ 

        
             
             ASSIGN   d-peso-liq = 0 
                      d-peso-bru = 0
                      l-peso = NO.
           
           
            FOR EACH  wt-it-docto        OF wt-docto  .
               
                ASSIGN d-peso-item = 0.
    
               FIND ITEM WHERE ITEM.IT-CODIGO = wt-it-docto.it-codigo NO-LOCK NO-ERROR.
               
               IF AVAIL ITEM AND ITEM.TIPO-CON-EST > 2 AND item.ge-codigo > 40 AND item.ge-codigo < 50 
               and item.baixa-estoq   THEN DO:
    
                   
                   FOR EACH wt-fat-ser-lote OF wt-it-docto NO-LOCK:
                       d-peso-item = d-peso-item + wt-fat-ser-lote.quantidade[1].
                   END.
                   IF ITEM.un = "KG" THEN
                       ASSIGN wt-it-docto.peso-liq-it = d-peso-item
                              l-peso = YES.
               END.
               
                  ASSIGN d-peso-liq = d-peso-liq + wt-it-docto.peso-liq-it 
                         d-peso-bru = d-peso-bru +  wt-it-docto.peso-bruto-it .
            END.
            
            
     
            IF l-peso AND wt-docto.peso-liq-tot-inf <> d-peso-liq THEN DO:
                 FOR FIRST  tt-epc WHERE
                          tt-epc.cod-event = p-ind-event AND
                          tt-epc.cod-parameter = "OBJECT-HANDLE"  : /*handle da bo*/
                            
                    
                        RUN _insertErrorManual IN widget-handle(tt-epc.val-parameter) 
                                                         ( INPUT 0,
                                                           INPUT "EMS":U,
                                                           INPUT "ERROR":U,
                                                           INPUT "PROBLEMA NOS PESOS DESTA NOTA" ,
                                                           INPUT "bodi317PR - Peso da nota: " + 
                                                           STRING(wt-docto.peso-liq-tot-inf) + " - Soma dos itens: " + STRING(d-peso-liq),
                                                           INPUT "":U).
                    END.
    
            END.
    
        
        END.

    END.

END.
                             

if return-value = "NOK" then
   RETURN "NOK".

else
   return "OK".



