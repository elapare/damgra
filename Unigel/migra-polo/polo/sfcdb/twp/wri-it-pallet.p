/***********************************************************************
**  Descricao : Upc WRITE it-pallet
**  Speto
************************************************************************/
/* valida‡Æo de quantidade de emendas no pedido - Edson - Amgra - 06/12/07 */

TRIGGER PROCEDURE FOR WRITE OF it-pallet OLD BUFFER b-it-pallet.

find first pallet
     where pallet.cod-estabel = it-pallet.cod-estabel
       and pallet.it-codigo   = it-pallet.it-codigo
       and pallet.nr-pallet   = it-pallet.nr-pallet exclusive-lock no-error.
if avail pallet then do:
   assign pallet.peso-liquido      = pallet.peso-liquido + it-pallet.saldo-bobina
          pallet.peso-bruto        = pallet.peso-bruto   + it-pallet.saldo-bobina.

   find first item 
        where item.it-codigo = it-pallet.it-codigo
        no-lock no-error.
   if avail item then do: 
      if item.cd-folh-lote <> "" then

         for first lote-carac-tec
             where lote-carac-tec.it-codigo = it-pallet.it-codigo
               and lote-carac-tec.lote      = it-pallet.lote-bobina
               and lote-carac-tec.cd-folha  = item.cd-folh-lote
               and lote-carac-tec.cd-comp   = "QTDBOB" no-lock:
         end.
         if avail lote-carac-tec AND lote-carac-tec.vl-result > 0 then 
            assign pallet.nr-bobinas         = pallet.nr-bobinas   + lote-carac-tec.vl-result.
         else
            assign pallet.nr-bobinas         = pallet.nr-bobinas   + 1.

   end.

/* valida‡Æo de quantidade de emendas no pedido - Edson - Amgra - 06/12/07 */
   IF pallet.nr-pedido <> 0 and
        index ( PROGRAM-NAME(1) +
                PROGRAM-NAME(2) +
                PROGRAM-NAME(3) +
                PROGRAM-NAME(4) +
                PROGRAM-NAME(5) +
                PROGRAM-NAME(6) +
                PROGRAM-NAME(7) +
                PROGRAM-NAME(8) +
                PROGRAM-NAME(9)  ,"1005") = 0 and
                 index ( PROGRAM-NAME(1) +
                PROGRAM-NAME(2) +
                PROGRAM-NAME(3) +
                PROGRAM-NAME(4) +
                PROGRAM-NAME(5) +
                PROGRAM-NAME(6) +
                PROGRAM-NAME(7) +
                PROGRAM-NAME(8) +
                PROGRAM-NAME(9)  ,"1001") = 0 and
                 index ( PROGRAM-NAME(1) +
                PROGRAM-NAME(2) +
                PROGRAM-NAME(3) +
                PROGRAM-NAME(4) +
                PROGRAM-NAME(5) +
                PROGRAM-NAME(6) +
                PROGRAM-NAME(7) +
                PROGRAM-NAME(8) +
                PROGRAM-NAME(9)  ,"receb") = 0


       THEN 
        RUN sfc\essf0003a-amg.p (INPUT pallet.nr-pedido).          
end.
return "OK":U.
