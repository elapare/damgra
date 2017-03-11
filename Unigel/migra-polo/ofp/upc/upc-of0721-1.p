/*****************************************************************************
**  programa: upc-of07216-1.p
**  autor...: Edson - Amgra
**  empresa.: Unigel
**  data....: 11/10/2009
**  versao..: 2.04.00.000
**  objetivo: Impedir gerar se ainda tiver notas n∆o atualizadas.
******************************************************************************/
/* definicao de parametros   *************************************************/
DEF VAR wgh-fpage1     AS WIDGET-HANDLE NO-UNDO.
DEF VAR wh-frame       AS WIDGET-HANDLE NO-UNDO.
DEF VAR wgh-child      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-desc-motivo  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-dt-emissao-ini   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-dt-emissao-fim   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-c-cod-estabel-amg    AS WIDGET-HANDLE NO-UNDO.


DEF NEW GLOBAL SHARED VAR wh-btok-amg   AS WIDGET-HANDLE NO-UNDO.

 
  
IF  valid-handle(wh-c-cod-estabel-amg) and
        valid-handle(wh-dt-emissao-ini) and
        valid-handle(wh-dt-emissao-fim ) 
    THEN DO:


       IF substring(wh-c-cod-estabel-amg:screen-value,1,2) <> "42" THEN RETURN.



       find first nota-fiscal where 
               nota-fiscal.cod-estabel    = wh-c-cod-estabel-amg:screen-value and
               nota-fiscal.dt-emis-nota >= date(wh-dt-emissao-ini:screen-value) and
               nota-fiscal.dt-emis-nota <= date(wh-dt-emissao-fim:screen-value) and
               nota-fiscal.dt-cancel = ? and
               nota-fiscal.dt-at-ofest = ?
               no-lock no-error.
              
              
      

            IF avail nota-fiscal THEN DO:
             
                run utp/ut-msgs.p (input "show":U, input 17006, "Existe nota fiscal neste periodo n∆o atualizada em Obrigaá‰es Fiscais~~NOTA FISCAL:" + nota-fiscal.nr-nota-fis + " DO DIA:"+ STRING(NOTA-FISCAL.DT-EMIS-NOTA,"99/99/9999")).
      
                 APPLY 'entry' TO wh-c-cod-estabel-amg.
                 RETURN ERROR.
            END.
                
END.

