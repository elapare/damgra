/*******************************************************
** Autor...: Carlos Rafael Nobrega
** UPC para ceapi001
** item.tipo-con-est 1 Serial 2 Nœmero S²rie 3 Lote 4 Refer¼ncia
*******************************************************/
/* Include i-epc200.i: Defini‡Æo Temp-Table tt-epc */
{include/i-epc200.i ceapi001k}

def input param p-ind-event as char no-undo.
def input-output param table for tt-epc.
 
def buffer b-movto-mat for movto-mat.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def new Global shared var gv-cont       as INTEGER no-undo.
def var h-tt-movto as handle no-undo.
def var h-tt-erro  as handle no-undo.
def var h-field    as handle no-undo.


 
  if p-ind-event = "Inicio-pi-valida-lote":U then do:

   &IF integer(entry(1,proversion,".")) >= 9 &THEN
   find first tt-epc
        where tt-epc.cod-event = "Inicio-pi-valida-lote"
          and cod-parameter    = "tt-movto(handle)" no-lock no-error.

 
    if avail tt-epc then
      assign h-tt-movto = widget-handle(tt-epc.val-parameter) NO-ERROR.
   
   find first tt-epc
        where tt-epc.cod-event = "Inicio-pi-valida-lote"
          and cod-parameter    = "tt-erro(handle)" no-lock no-error.
   if avail tt-epc then
      assign h-tt-erro = widget-handle(tt-epc.val-parameter).

    
 
        IF valid-handle(h-tt-movto:default-buffer-handle) THEN DO:
        
         run pi-valida-conteudo.
  

         IF RETURN-VALUE = "NOK" THEN
             RETURN "nok".
        
         
       END.

   &ENDIF

end.

  
procedure pi-valida-conteudo:
  
       def var i-esp-docto  as int  no-undo.
    def var c-lote       as char no-undo.
    def var i-tipo-trans as int  no-undo.
    def var c-it-codigo  as char no-undo.

    run getRecord (output i-esp-docto,
                   output c-lote,
                   output i-tipo-trans,
                   output c-it-codigo).

    if i-tipo-trans = 1 then do:
        /*Valida itens controlados por lote ou referˆncia*/

        FIND ITEM WHERE
             ITEM.it-codigo = c-it-codigo NO-LOCK NO-ERROR.
   
        if AVAIL ITEM AND
                (ITEM.tipo-con-est > 2 AND
                 i-esp-docto = 1 /*Aca*/   and
                 c-lote      <> "RECICL":U)  then do:
              find first lote-item
                   where lote-item.lote = c-lote no-lock no-error.
     
              /*Colocado saldo-estoq para itens controlados por lote sem folha do lote*/
              find first saldo-estoq 
                   where saldo-estoq.it-codigo = c-it-codigo
                     and saldo-estoq.lote      = c-lote no-lock no-error.

              if avail lote-item   or
                 avail saldo-estoq then do:
                 
                 if  avail saldo-estoq then do:
        
                         FIND estabelec WHERE estabelec.cod-estabel = saldo-estoq.cod-estabel NO-LOCK NO-ERROR.
                         IF NOT AVAIL estabelec THEN RETURN.
        
                         IF (estabelec.ep-codigo <> "420" AND estabelec.ep-codigo <> "410") THEN RETURN.  /*solic-318*/ 
                 end.

                 if avail lote-item then
                     find last movto-mat
                         where movto-mat.it-codigo = lote-item.it-codigo
                           and movto-mat.lote      = lote-item.lote 
                           and movto-mat.esp-docto = 1 no-lock no-error.
                 else
                     find last movto-mat
                         where movto-mat.it-codigo = saldo-estoq.it-codigo
                           and movto-mat.lote      = saldo-estoq.lote 
                           and movto-mat.esp-docto = 1 no-lock no-error.

                 if avail movto-mat then do:
                    FIND estabelec WHERE estabelec.cod-estabel = movto-mat.cod-estabel NO-LOCK NO-ERROR.
                         IF NOT AVAIL estabelec THEN RETURN.
        
                         IF (estabelec.ep-codigo <> "420" AND estabelec.ep-codigo <> "410")  THEN RETURN.  /*solic-318*/ 

                    if not can-find(first b-movto-mat
                                    where b-movto-mat.nr-reporte = movto-mat.nr-reporte
                                      and b-movto-mat.esp-docto  = 8 no-lock) then do:
                       /*Se entrou aqui vai barrar pois nao tem o estorno*/
                       if avail lote-item then
                          run pi-cria-erro (input 1, 17006, "Este Lote j  foi informado: " + lote-item.lote + "!":U).
                       else
                          run pi-cria-erro (input 1, 17006, "Este Lote j  foi informado: " + saldo-estoq.lote + "!":U).
                       return "NOK":U.
                  end.
                    /*Se chegou aqui vai criar o lote pois tem o estorno do mesmo*/
                 end.
                 else do:
                    
                    if avail lote-item then
                       run pi-cria-erro (input 1, 17006, "Este Lote j  foi informado: " + lote-item.lote + "!":U).
                    else
                       run pi-cria-erro (input 1, 17006, "Este Lote j  foi informado: " + saldo-estoq.lote + "!":U).

                    return "NOK":U.
                 end.
    
              end.
          end.
    end.

    

end procedure.

procedure getRecord:
    def output parameter p-i-esp-docto  as int  no-undo.
    def output parameter p-c-lote       as char no-undo.
    def output parameter p-i-tipo-trans as int  no-undo.
    def output parameter p-it-codigo    as char no-undo.

    def var hfield   as handle no-undo.
    def var hbuffer  as handle no-undo.
    def var hbuffer2 as handle no-undo.
    def var hquery   as handle no-undo.

    def var l-ok    as log    no-undo.
    

    hbuffer = h-tt-movto:default-buffer-handle. 
    
     


    /*create buffer hbuffer2 for table hBuffer buffer-name "b-tt-movto". */

    /*create query hquery. 
    hquery:set-buffers(hbuffer). 
    hquery:query-prepare("FOR EACH tt-movto"). 
    hquery:query-open. 
  
   



    ASSIGN l-ok = hquery:get-last() NO-ERROR.

      

    IF l-ok THEN*/
    assign hfield         = hbuffer:buffer-field("tipo-trans")
           p-i-tipo-trans = int(hfield:buffer-value)
           hfield         = hbuffer:buffer-field("esp-docto")
           p-i-esp-docto  = int(hfield:buffer-value)
           hfield         = hbuffer:buffer-field("lote")
           p-c-lote       = string(hfield:buffer-value)
           hfield         = hbuffer:buffer-field("it-codigo")
           p-it-codigo    = string(hfield:buffer-value).


    
 /*   def var p-it-codigo2 as char no-undo.
    DEFINE VARIABLE i-ct AS INTEGER    NO-UNDO.
    assign l-ok = hquery:get-first() NO-ERROR.


         
    

    do while l-ok = yes:

       assign hfield       = hbuffer:buffer-field("it-codigo")
              p-it-codigo2 = hfield:buffer-value no-error.

     

       l-ok = hquery:get-next() NO-ERROR.
    end.


      
     assign l-ok = hquery:get-last() NO-ERROR.

    delete object hquery. 
   */ 
end procedure.

procedure pi-cria-erro:
    def input parameter p-i-sequen as int  no-undo.
    def input parameter p-cd-erro  as int  no-undo.
    def input parameter p-mensagem as char no-undo.

    def var hfield  as handle no-undo.
    def var hbuffer as handle no-undo.
    def var hquery  as handle no-undo.

    hbuffer = h-tt-erro:default-buffer-handle. 
    
    create query hquery. 
    hquery:set-buffers(hbuffer). 
    hquery:query-prepare("FOR EACH tt-erro"). 
    hquery:query-open. 
    hbuffer:buffer-create.
    assign h-field              = hbuffer:buffer-field("i-sequen":u)
           h-field:buffer-value = p-i-sequen
           h-field              = hbuffer:buffer-field("cd-erro":u)
           h-field:buffer-value = p-cd-erro
           h-field              = hbuffer:buffer-field("mensagem":u)
           h-field:buffer-value = p-mensagem.
    hbuffer:buffer-release.
    delete object hquery. 

end procedure.

 
