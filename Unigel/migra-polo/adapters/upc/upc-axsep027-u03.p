/*******************************************************************************
**  Programa: upc-axsep027-u03
**  Objetivo: <comment>
**  Autor...: Felipe - DLCTec
**  Data....: 28/11/2012
*******************************************************************************/
def buffer b-estabelec              for estabelec.
{include/i-epc200.i  axsep027}
{include/i-epc200.i2 axsep027}

DEFINE INPUT PARAM  p-ind-event  AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE  FOR tt-epc.
 

DEFINE NEW GLOBAL SHARED VAR l-grava-log-axsep027 AS LOGICAL NO-UNDO. /*solic-318*/
DEFINE VARIABLE c-log-axsep027    AS CHARACTER   NO-UNDO.             /*solic-318*/
DEFINE VARIABLE i-time AS INTEGER     NO-UNDO.
DEFINE VARIABLE dt-time AS DATE        NO-UNDO.
DEFINE VARIABLE hQ_ttdet          AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE h_ttdet           AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE wh_CodEstabelNF   AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE wh_SerieNF        AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE wh_NrNotaFisNF    AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE wh_nItemPed       AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE wh_xProd          AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE wh_xPed           AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE wh_uCom           AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE wh_uTrib          AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE wh_vUnCom         AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE wh_vUnTrib        AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE wh_qCom           AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE wh_qTrib          AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE wh_vProd          AS WIDGET-HANDLE        NO-UNDO.

DEFINE VARIABLE wh_pedido         AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE wh_item           AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE wh_xml            AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE wh_nr_seq_fat     AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE h-GenXml          AS HANDLE               NO-UNDO.
DEFINE VARIABLE h-xml-new         AS HANDLE               NO-UNDO.
DEFINE VARIABLE c-tag             AS CHARACTER            NO-UNDO.
DEFINE VARIABLE c-linha           AS CHARACTER            NO-UNDO.
DEFINE VARIABLE c-linha-aux       AS CHARACTER            NO-UNDO.
DEFINE VARIABLE c-arq             AS CHARACTER            NO-UNDO.
DEFINE VARIABLE i-cont            AS INTEGER              NO-UNDO.
DEFINE VARIABLE i-aux             AS INTEGER              NO-UNDO.
DEFINE VARIABLE i-item            AS INTEGER              NO-UNDO.
DEFINE VARIABLE pedcli-jr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE unfat-jr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE sq-pedcli-jr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-seq-ped AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-desc-prod AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-larg AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-diin AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-diex AS CHARACTER   NO-UNDO.

DEFINE VARIABLE hQ_ttcompra          AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE h_ttcompra           AS WIDGET-HANDLE        NO-UNDO.

DEFINE VARIABLE wh_dhSaiEnt           AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE hQ_ttIde          AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE h_ttIde           AS WIDGET-HANDLE        NO-UNDO.
DEFINE VARIABLE c-time AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-data AS CHARACTER   NO-UNDO.

c-log-axsep027 = SESSION:TEMP-DIRECTORY +    "log-axsep027.txt".

function f-limpa returns char (input pcampo as char) forward.

    IF l-grava-log-axsep027 THEN DO:
   
        OUTPUT TO value(c-log-axsep027) NO-CONVERT APPEND.
          FOR EACH  tt-epc NO-LOCK.
             PUT UNFORMATTED  "p-ind-event " p-ind-event SKIP
                              "tt-epc.cod-event " tt-epc.cod-event  SKIP                 
                              "tt-epc.cod-parameter" tt-epc.cod-parameter SKIP.
          END.
          PUT UNFORMATTED "----------------------------------" SKIP.
        OUTPUT CLOSE.

    END.

    
IF p-ind-event = "AtualizaDadosNFe" THEN DO:



    FIND FIRST tt-epc 
         WHERE tt-epc.cod-event     = "AtualizaDadosNFe"
           AND tt-epc.cod-parameter = "ttDet" NO-LOCK NO-ERROR.

    IF AVAIL tt-epc THEN DO:

        ASSIGN h_ttdet = WIDGET-HANDLE(tt-epc.val-parameter)
               h_ttdet = h_ttdet:DEFAULT-BUFFER-HANDLE
               i-cont  = 0.

        CREATE QUERY hQ_ttdet.                        
        hQ_ttdet:SET-BUFFERS(h_ttdet).              
        hQ_ttdet:QUERY-PREPARE("FOR EACH ttdet").   
        hQ_ttdet:QUERY-OPEN.                          
        hQ_ttdet:GET-FIRST.                           

        DO WHILE NOT hQ_ttdet:QUERY-OFF-END:
            ASSIGN wh_CodEstabelNF  = h_ttdet:BUFFER-FIELD("CodEstabelNF")
                   wh_SerieNF       = h_ttdet:BUFFER-FIELD("SerieNF")
                   wh_NrNotaFisNF   = h_ttdet:BUFFER-FIELD("NrNotaFisNF")
                   wh_item          = h_ttdet:BUFFER-FIELD("ItCodigoNF")
                   wh_nr_seq_fat    = h_ttdet:BUFFER-FIELD("NrSeqFatNF")
                   wh_nItemPed      = h_ttdet:BUFFER-FIELD("nItemPed")
                   wh_xProd         = h_ttdet:BUFFER-FIELD("xProd")
                   wh_xPed          = h_ttdet:BUFFER-FIELD("xPed")
                   wh_uCom          = h_ttdet:BUFFER-FIELD("uCom")
                   wh_uTrib         = h_ttdet:BUFFER-FIELD("uTrib")
                   wh_vUnCom        = h_ttdet:BUFFER-FIELD("vUnCom") 
                   wh_vUnTrib       = h_ttdet:BUFFER-FIELD("vUnTrib")
                   wh_qCom          = h_ttdet:BUFFER-FIELD("qCom") 
                   wh_qTrib         = h_ttdet:BUFFER-FIELD("qTrib")
                   wh_vProd         = h_ttdet:BUFFER-FIELD("vProd").

            FIND FIRST it-nota-fisc NO-LOCK
                WHERE  it-nota-fisc.cod-estabel = wh_CodEstabelNF:BUFFER-VALUE
                  AND  it-nota-fisc.serie       = wh_SerieNF     :BUFFER-VALUE 
                  AND  it-nota-fisc.nr-nota-fis = wh_NrNotaFisNF :BUFFER-VALUE  
                  AND  it-nota-fisc.it-codigo   = wh_item        :BUFFER-VALUE  
                  AND  it-nota-fisc.nr-seq-fat  = wh_nr_seq_fat  :BUFFER-VALUE   NO-ERROR.

            IF AVAIL it-nota-fisc AND it-nota-fisc.nr-pedcli <> "" THEN DO:
                FOR FIRST natur-oper NO-LOCK
                    WHERE natur-oper.nat-operacao = it-nota-fisc.nat-operacao:

                    IF natur-oper.especie-doc <> "NFE" THEN DO:
                        ASSIGN wh_nItemPed:BUFFER-VALUE = STRING(it-nota-fisc.nr-seq-ped).
                    END.
                END.

                IF valid-handle(wh_xProd) AND CAN-FIND(FIRST ITEM WHERE 
                                                                               ITEM.it-codigo = it-nota-fisc.it-codigo AND 
                                                                               ITEM.ge-codigo >= 41 AND item.ge-codigo <= 49 )   THEN DO:
                    c-desc-prod = wh_xProd:BUFFER-VALUE.

                    IF INDEX(c-desc-prod ,"BOPP") > 0  THEN DO:
                       RUN pi-busca-largura.
                    END.

                    FOR FIRST emitente WHERE emitente.nome-abrev   = it-nota-fisc.nome-ab-cli NO-LOCK, 
                        FIRST item-cli WHERE item-cli.it-codigo    = it-nota-fisc.it-codigo AND
                                             item-cli.cod-emitente = emitente.cod-emitente AND
                                             item-cli.log-2  NO-LOCK.
                         c-desc-prod =    item-cli.item-do-cli + " " + item-cli.narrativa .
                         
                    END.
                    wh_xProd:BUFFER-VALUE = c-desc-prod.


                
                END.
                ELSE
                IF valid-handle(wh_xProd) THEN DO:                                      

                    FOR FIRST emitente WHERE emitente.nome-abrev   = it-nota-fisc.nome-ab-cli NO-LOCK, 
                        FIRST item-cli WHERE item-cli.it-codigo    = it-nota-fisc.it-codigo AND
                                             item-cli.cod-emitente = emitente.cod-emitente AND
                                             item-cli.log-2  NO-LOCK.                    
                        c-desc-prod =    item-cli.item-do-cli + " " + item-cli.narrativa .
                        wh_xProd:BUFFER-VALUE = c-desc-prod.                         
                    END.
                                   
                END.
                
                find first b-estabelec where b-estabelec.cod-estabel = it-nota-fisc.cod-estabel no-lock no-error.
                if avail b-estabelec then do:
                  
                        assign pedcli-jr    = ""
                               sq-pedcli-jr = ""
                               unfat-jr     = "".

                        for first ped-item where 
                                   ped-item.nome-abrev   = it-nota-fisc.nome-ab-cli and
                                   ped-item.nr-pedcli    = it-nota-fisc.nr-pedcli and
                                   ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped and
                                   ped-item.ind-componen < 3 NO-LOCK.
                           FOR  FIRST var-result WHERE var-result.nome-var = "pedcli" AND 
                                 var-result.nr-estrut =  ped-item.nr-config AND
                                 var-result.item-cotacao = ped-item.it-codigo 
                                 NO-LOCK .
                         
                             ASSIGN pedcli-jr = var-result.valor-char
                                    sq-pedcli-jr = STRING(ped-item.nr-sequencia).
                           END.
                           FOR  FIRST var-result WHERE var-result.nome-var = "UNFAT" AND 
                                  var-result.nr-estrut =  ped-item.nr-config AND
                                  var-result.item-cotacao = ped-item.it-codigo 
                                  NO-LOCK .
                            
                              ASSIGN unfat-jr = var-result.valor-char.
                                      
                           END.


                           
                        END.
                       /* IF pedcli-jr = ""  THEN DO: 12-09-2016*/
                            find first pd-compl-pedido where 
                                   pd-compl-pedido.ep-codigo    = b-estabelec.ep-codigo and
                                   pd-compl-pedido.nr-pedido    = it-nota-fisc.nr-pedido and
                                   pd-compl-pedido.nr-sequencia = it-nota-fisc.nr-seq-ped no-lock no-error.
                          
                             if avail pd-compl-pedido and trim(substring(pd-compl-pedido.char-1,51,10)) <> "" then do:
                                   ASSIGN pedcli-jr = substring(pd-compl-pedido.char-1,51,10) .

                                 if trim(substring(pd-compl-pedido.char-1,61,4)) <> "" then 
                                   ASSIGN        sq-pedcli-jr = trim(string(int(substring(pd-compl-pedido.char-1,61,4)),">>99")) NO-ERROR.      
                                   
                                   
                
                             end.
                        /*END. 12-09-2016*/

                        if pedcli-jr <> "" then pedcli-jr = f-limpa(pedcli-jr).

                        IF pedcli-jr <> ""  THEN do:
                            ASSIGN wh_xPed:BUFFER-VALUE = substring(replace(replace(replace(replace(pedcli-jr,CHR(39),""),CHR(34),""),CHR(10),""),CHR(9),""),1,10).
                             
                            i-seq-ped = 0.

                            ASSIGN i-seq-ped = INT(sq-pedcli-jr) NO-ERROR.
                            IF i-seq-ped <> 0 and sq-pedcli-jr <> "" THEN wh_nItemPed:BUFFER-VALUE = trim(string(i-seq-ped,">>99")).
                            
                        END.

                        IF unfat-jr = "BB" THEN DO:                            
                                 
                            ASSIGN wh_uCom:BUFFER-VALUE = "UN"  
                                   wh_uTrib:BUFFER-VALUE = "UN"
                                   wh_vUnTrib:BUFFER-VALUE = (IF wh_vUnTrib:BUFFER-VALUE = wh_vUnCom:BUFFER-VALUE THEN (dec( wh_vProd:BUFFER-VALUE) / it-nota-fisc.qt-faturada[2]) ELSE
                                                             (wh_vUnTrib:BUFFER-VALUE * DEC(wh_qTrib:BUFFER-VALUE) / it-nota-fisc.qt-faturada[2]))
                                   wh_vUnCom:BUFFER-VALUE = dec( wh_vProd:BUFFER-VALUE) / it-nota-fisc.qt-faturada[2]                                    
                                   wh_qCom:BUFFER-VALUE =  it-nota-fisc.qt-faturada[2]  
                                   wh_qTrib:BUFFER-VALUE = it-nota-fisc.qt-faturada[2]. 

                        END.

                          
                end.                
            END.                                                                        
            hQ_ttdet:GET-NEXT().
        END.
        hQ_ttdet:QUERY-CLOSE().
        DELETE OBJECT hQ_ttdet.
    END.

 
  
    /*ajusta pedido de compra*/
    FIND FIRST tt-epc 
     WHERE tt-epc.cod-event     = "AtualizaDadosNFe"
       AND tt-epc.cod-parameter = "ttCompra" NO-LOCK NO-ERROR.

    IF AVAIL tt-epc THEN DO:
    
        ASSIGN h_ttcompra = WIDGET-HANDLE(tt-epc.val-parameter)
               h_ttcompra = h_ttcompra:DEFAULT-BUFFER-HANDLE
               i-cont  = 0.
    
        CREATE QUERY hQ_ttcompra.                        
        hQ_ttcompra:SET-BUFFERS(h_ttcompra).              
        hQ_ttcompra:QUERY-PREPARE("FOR EACH ttcompra").   
        hQ_ttcompra:QUERY-OPEN.                          
        hQ_ttcompra:GET-FIRST.                           
    
        DO WHILE NOT hQ_ttcompra:QUERY-OFF-END:
            ASSIGN wh_CodEstabelNF  = h_ttcompra:BUFFER-FIELD("CodEstabelNF")
                   wh_SerieNF       = h_ttcompra:BUFFER-FIELD("SerieNF")
                   wh_NrNotaFisNF   = h_ttcompra:BUFFER-FIELD("NrNotaFisNF")
                   wh_xPed          = h_ttcompra:BUFFER-FIELD("xPed").
    
    
            FIND FIRST it-nota-fisc NO-LOCK
                WHERE  it-nota-fisc.cod-estabel = wh_CodEstabelNF:BUFFER-VALUE
                  AND  it-nota-fisc.serie       = wh_SerieNF     :BUFFER-VALUE 
                  AND  it-nota-fisc.nr-nota-fis = wh_NrNotaFisNF :BUFFER-VALUE  
                  AND it-nota-fisc.nr-pedcli <> "" NO-ERROR.
    
            IF AVAIL it-nota-fisc  THEN DO:
                
    
                find first b-estabelec where b-estabelec.cod-estabel = it-nota-fisc.cod-estabel no-lock no-error.
                if avail b-estabelec then do:
    
                        assign pedcli-jr = "".
    
                        for first ped-item where 
                                   ped-item.nome-abrev   = it-nota-fisc.nome-ab-cli and
                                   ped-item.nr-pedcli    = it-nota-fisc.nr-pedcli and
                                   ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped and
                                   ped-item.ind-componen < 3 no-lock ,            
                             FIRST var-result WHERE var-result.nome-var = "pedcli" AND 
                                 var-result.nr-estrut =  ped-item.nr-config AND
                                 var-result.item-cotacao = ped-item.it-codigo 
                                 NO-LOCK .
    
                             ASSIGN pedcli-jr = var-result.valor-char.
                                    
                        END.
                        /*IF pedcli-jr = ""  THEN DO:  alterado 12-09-2016 para se tiver digitado no pd4000 prevalece*/
                            find first pd-compl-pedido where 
                                   pd-compl-pedido.ep-codigo    = b-estabelec.ep-codigo and
                                   pd-compl-pedido.nr-pedido    = it-nota-fisc.nr-pedido and
                                   pd-compl-pedido.nr-sequencia = it-nota-fisc.nr-seq-ped no-lock no-error.
    
                             if avail pd-compl-pedido and trim(substring(pd-compl-pedido.char-1,51,10)) <> "" then do:
                                   ASSIGN pedcli-jr = substring(pd-compl-pedido.char-1,51,10) .
    
                             end.
                        /*END. 12-09-2016*/
    
                        if pedcli-jr <> "" then pedcli-jr = f-limpa(pedcli-jr).

                        IF pedcli-jr <> ""  THEN do:
                            ASSIGN wh_xPed:BUFFER-VALUE = substring(replace(replace(replace(replace(pedcli-jr,CHR(39),""),CHR(34),""),CHR(10),""),CHR(9),""),1,15).    
    
                        END.
    
                end.                
            END.                                                                        
            hQ_ttcompra:GET-NEXT().
        END.
        hQ_ttcompra:QUERY-CLOSE().
        DELETE OBJECT hQ_ttcompra.
    END.

        /*ajusta chave de data e hora*/
    FIND FIRST tt-epc 
     WHERE tt-epc.cod-event     = "AtualizaDadosNFe"
       AND tt-epc.cod-parameter = "ttIde" NO-LOCK NO-ERROR.

    IF AVAIL tt-epc THEN DO:
    
        ASSIGN h_ttIde = WIDGET-HANDLE(tt-epc.val-parameter)
               h_ttIde = h_ttIde:DEFAULT-BUFFER-HANDLE
               i-cont  = 0.
    
        IF l-grava-log-axsep027 THEN DO:

    OUTPUT TO value(c-log-axsep027) NO-CONVERT APPEND.
      PUT UNFORMATTED
       "VALID-HANDLE(h_ttIde)"  VALID-HANDLE(h_ttIde) skip
                
          "-----------------------------------------------------" SKIP.
    OUTPUT CLOSE.
END.

        CREATE QUERY hQ_ttIde.                        
        hQ_ttIde:SET-BUFFERS(h_ttIde).              
        hQ_ttIde:QUERY-PREPARE("FOR EACH ttIde").   
        hQ_ttIde:QUERY-OPEN.                          
        hQ_ttIde:GET-FIRST.                           
    
        DO WHILE NOT hQ_ttIde:QUERY-OFF-END:
            ASSIGN wh_CodEstabelNF  = h_ttIde:BUFFER-FIELD("CodEstabelNF")
                   wh_SerieNF       = h_ttIde:BUFFER-FIELD("SerieNF")
                   wh_NrNotaFisNF   = h_ttIde:BUFFER-FIELD("NrNotaFisNF")
                   wh_dhSaiEnt      = h_ttIde:BUFFER-FIELD("dhSaiEnt").
    
IF l-grava-log-axsep027 THEN DO:

    OUTPUT TO value(c-log-axsep027) NO-CONVERT APPEND.
      PUT UNFORMATTED
       "VALID-HANDLE(wh_CodEstabelNF)"  VALID-HANDLE(wh_CodEstabelNF) skip
       "VALID-HANDLE(wh_SerieNF     )"  VALID-HANDLE(wh_SerieNF     ) skip
       "VALID-HANDLE(wh_NrNotaFisNF )"  VALID-HANDLE(wh_NrNotaFisNF ) skip
       "VALID-HANDLE(wh_dhSaiEnt    )"  VALID-HANDLE(wh_dhSaiEnt    ) SKIP          
          "-----------------------------------------------------" SKIP.
    OUTPUT CLOSE.
END.


            IF  VALID-HANDLE(wh_CodEstabelNF) AND
                VALID-HANDLE(wh_SerieNF     ) AND
                VALID-HANDLE(wh_NrNotaFisNF ) AND
                VALID-HANDLE(wh_dhSaiEnt    ) THEN DO:
           
                c-time = "".

                FIND FIRST nota-fiscal NO-LOCK
                    WHERE  nota-fiscal.cod-estabel = wh_CodEstabelNF:BUFFER-VALUE
                      AND  nota-fiscal.serie       = wh_SerieNF     :BUFFER-VALUE 
                      AND  nota-fiscal.nr-nota-fis = wh_NrNotaFisNF :BUFFER-VALUE NO-ERROR.
        
                IF AVAIL nota-fiscal AND nota-fiscal.cod-estabel = "433" THEN DO:
                    FOR FIRST natur-oper WHERE 
                        natur-oper.nat-operacao = nota-fiscal.nat-operacao NO-LOCK.
                
                        FIND FIRST nota-fisc-adc WHERE
                            nota-fisc-adc.cod-estab          = nota-fiscal.cod-estabel  and
                            nota-fisc-adc.cod-serie          = nota-fiscal.serie        and
                            nota-fisc-adc.cod-nota-fisc      = nota-fiscal.nr-nota-fis  and
                            nota-fisc-adc.cdn-emitente       = nota-fiscal.cod-emitente and
                            nota-fisc-adc.cod-natur-operac   = natur-oper.cod-cfop      and
                            nota-fisc-adc.idi-tip-dado       = 20                       and
                            nota-fisc-adc.num-seq            = 1 NO-ERROR.
                
                        IF NOT AVAIL nota-fisc-adc THEN DO:
                            CREATE nota-fisc-adc.
                            ASSIGN  nota-fisc-adc.cod-estab          = nota-fiscal.cod-estabel 
                                    nota-fisc-adc.cod-serie          = nota-fiscal.serie       
                                    nota-fisc-adc.cod-nota-fisc      = nota-fiscal.nr-nota-fis 
                                    nota-fisc-adc.cdn-emitente       = nota-fiscal.cod-emitente
                                    nota-fisc-adc.cod-natur-operac   = natur-oper.cod-cfop     
                                    nota-fisc-adc.idi-tip-dado       = 20                      
                                    nota-fisc-adc.num-seq            = 1.
                        END.
                
                        c-time = substring(nota-fisc-adc.cod-livre-3,1,25).

                        IF trim(c-time) = "" OR c-time = ? THEN
                            ASSIGN
                                i-time = TIME + 1800
                                dt-time = TODAY + (IF i-time >= 86400 THEN 1 ELSE 0)
                                c-time = STRING(i-time,"HH:MM:SS")
                                c-data = STRING(YEAR(dt-time),"9999") + "-" + STRING(month(dt-time),"99") + "-" + STRING(day(dt-time),"99")
                                nota-fisc-adc.hra-saida          =   REPLACE(c-time,":","")                   
                                overlay(nota-fisc-adc.cod-livre-1,1,1)        =  "9"                         
                                overlay(nota-fisc-adc.cod-livre-2,1,1)        =  "1"                         
                                overlay(nota-fisc-adc.cod-livre-3,1,25)       =  c-data + "T" + c-time + "-03:00"
                                nota-fisc-adc.dat-livre-1        =  dt-time.  
                        c-time = substring(nota-fisc-adc.cod-livre-3,1,25).
                    END.
                    c-data = wh_dhSaiEnt:BUFFER-VALUE.

                    IF trim(c-data) = "" OR c-data = ? THEN
                        ASSIGN wh_dhSaiEnt:BUFFER-VALUE = c-time.    

                    IF l-grava-log-axsep027 THEN DO:
                   
                        OUTPUT TO value(c-log-axsep027) NO-CONVERT APPEND.
                          PUT UNFORMATTED
                              "nota-fiscal.cod-estabel " nota-fiscal.cod-estabel skip
                              "nota-fiscal.serie       " nota-fiscal.serie       skip
                              "nota-fiscal.nr-nota-fis " nota-fiscal.nr-nota-fis skip
                              "c-data "  c-data                                  skip
                              "c-time "  c-time SKIP
                              "-----------------------------------------------------" SKIP.
                        OUTPUT CLOSE.
                    END.

                END.
                      
            END. /*valid-handle*/
    
                                                                                    
            hQ_ttIde:GET-NEXT().
        END.
        hQ_ttIde:QUERY-CLOSE().
        DELETE OBJECT hQ_ttIde.
    END.

END.

RETURN "OK".
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
function f-limpa returns char (input pcampo as char).
 DEFINE VARIABLE idx AS INTEGER     NO-UNDO.
 DEFINE VARIABLE c-aux AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE i-letra AS INTEGER     NO-UNDO.

     c-aux = trim(pcampo).
     pcampo = "".
    
     do idx = 1 to length(c-aux).
       i-letra = asc(caps(substring(c-aux,idx,1))).
       
       if (i-letra = 32 or (i-letra >= 45 and i-letra <= 57) or (i-letra >= 65 and i-letra <= 90)) then
         pcampo = pcampo + caps(substring(c-aux,idx,1)).
    
            
     end.
     pcampo = replace(pcampo,"  "," ").

    return pcampo.

end function.


PROCEDURE pi-busca-largura.
     /*** Pesquisar a largura do item ***/
    DEFINE VARIABLE c-esp-ext AS CHARACTER   NO-UNDO.

     ASSIGN  c-larg = "".             
 
     FOR FIRST  fat-ser-lote WHERE
              fat-ser-lote.cod-estabel = it-nota-fisc.cod-estabel  AND
              fat-ser-lote.serie       = it-nota-fisc.serie        AND
              fat-ser-lote.nr-nota-fis = it-nota-fisc.nr-nota-fis AND
              fat-ser-lote.nr-seq-fat  = it-nota-fisc.nr-seq-fat  AND
              fat-ser-lote.it-codigo   = it-nota-fisc.it-codigo
              NO-LOCK,
              FIRST pallet WHERE
                   pallet.cod-estabel =  it-nota-fisc.cod-estabel AND
                   pallet.it-codigo = it-nota-fisc.it-codigo AND 
                   pallet.nr-pallet = fat-ser-lote.nr-serlote
                   NO-LOCK,
   
                FIRST it-pallet WHERE 
                    it-pallet.cod-estabel = it-nota-fisc.cod-estabel AND
                    it-pallet.it-codigo   = it-nota-fisc.it-codigo  AND
                    it-pallet.nr-pallet   = fat-ser-lote.nr-serlote  
                    NO-LOCK:

                  FIND FIRST lote-carac-tec WHERE 
                      lote-carac-tec.it-codigo = it-pallet.it-codigo AND
                      lote-carac-tec.lote      = it-pallet.lote-bobina AND
                      lote-carac-tec.cd-comp   = "largura"
                      NO-LOCK NO-ERROR.
        
                   IF AVAIL lote-carac-tec THEN
                        ASSIGN c-larg = STRING (lote-carac-tec.vl-result).                            
     END.

     IF c-larg = "" THEN DO:
       
            FIND var-result USE-INDEX id WHERE 
                 var-result.item-cotacao  = it-nota-fisc.it-codigo   AND
                 var-result.nr-estrut     = INT(it-nota-fisc.cod-refer )      AND
                 var-result.nome-var      = "LARGURA"  NO-LOCK NO-ERROR.

            IF AVAIL VAR-result THEN
                ASSIGN c-larg = string(var-result.des-result).            
     END.
          
     IF SUBSTRING(it-nota-fisc.nat-operacao,1,1) = "7"  THEN DO:

         c-esp-ext = substring(it-nota-fisc.it-codigo,1,2).

         if substring(it-nota-fisc.it-codigo,1,4) = "20TB" THEN 
             ASSIGN c-esp-ext = "28,6".
         ELSE
         if substring(it-nota-fisc.it-codigo,1,4) = "23TB" THEN 
             ASSIGN c-esp-ext = "32,8".
         ELSE
         if substring(it-nota-fisc.it-codigo,1,4) = "26TB" THEN 
             ASSIGN c-esp-ext = "37,1".
         ELSE
         if substring(it-nota-fisc.it-codigo,1,4) = "22TB" THEN 
             ASSIGN c-esp-ext = "31,4".
         ELSE
         if substring(it-nota-fisc.it-codigo,1,4) = "35BM" THEN 
             ASSIGN c-esp-ext = "32,8".
         ELSE
         if substring(it-nota-fisc.it-codigo,1,4) = "40BM" THEN 
              ASSIGN c-esp-ext = "37,1".
         

         IF c-larg <> ""  THEN 
                ASSIGN 
                              c-desc-prod = "FILME DE POLIPROPILENO BIAXIALMENTE ORIENTADO"
                              c-desc-prod = c-desc-prod + " LARGURA:" + c-larg + "MM "
                              c-desc-prod = c-desc-prod + "ESPESSURA:" + c-esp-ext + " MICRA"
                              c-desc-prod = c-desc-prod + ", EM ROLOS".  
            ELSE
                ASSIGN 
                              c-desc-prod = "FILME DE POLIPROPILENO BIAXIALMENTE ORIENTADO"
            
                              c-desc-prod = c-desc-prod + "ESPESSURA:" + c-esp-ext + " MICRA"
                              c-desc-prod = c-desc-prod + ", EM ROLOS". 

     END.
     ELSE DO:
          IF c-larg <> "" AND it-nota-fisc.cod-refer <> ""  THEN 
                ASSIGN c-desc-prod = c-desc-prod + " LARGURA:" + c-larg + "MM ".         
     END.

               
END PROCEDURE.
