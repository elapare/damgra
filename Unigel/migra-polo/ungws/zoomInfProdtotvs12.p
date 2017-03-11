/*****************  ******************************************************************
**
**       Programa: ungws/zoomInfProdtotvs12.p
**
**       Data....: dezembro/2014.
** 
**       Autor...: Edson - Damgra.
**
**       Objetivo: webservice para pesquisar pedidos, itens e notas fiscais
**
************************************************************************************/
/*parametros*/
DEFINE INPUT  PARAM c_funcao          AS CHARACTER   NO-UNDO.
DEFINE input  PARAM c_param_funcao    AS CHARACTER   NO-UNDO.
DEFINE INPUT  param lctabela1        AS LONGCHAR   NO-UNDO.
DEFINE OUTPUT param lctabela2        AS LONGCHAR   NO-UNDO.
DEFINE INPUT-OUTPUT PARAM c_param_retorno1  AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAM c_param_retorno2  AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAM c_param_retorno3  AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAM c_param_retorno4  AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAM c_status          AS CHARACTER   NO-UNDO.
Def Var c-corpo     As Char.

Def Var c-campo     As Char.
Def Var c-byte      As Char.
DEFINE VARIABLE c-nome-emit AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cidade    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE Var-Class AS CHARACTER FORMAT "X(20)".
DEFINE VARIABLE c-pasta AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.
def var c-cod-estabel-ini as char  no-undo.
def var c-cod-estabel-fim as char  no-undo.
def var c-it-codigo-ini as char  no-undo.
def var c-it-codigo-fim as char  no-undo.


def var c-nome-abrev as char  no-undo.
def var dt-nota-ini as date no-undo.
def var dt-nota-fim as date no-undo.
def var c-nr-pedcli-ini as char no-undo.
def var c-nr-pedcli-fim as char no-undo.
def var c-nr-nota-fis-ini as char no-undo.
def var c-nr-nota-fis-fim as char no-undo.
def var c-notas as char no-undo.
def var c-bobinas as char no-undo.




DEFINE VARIABLE vTipoItem  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vTotalLiq  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVendedor  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vCodEstabel  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cArrayReturn     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE NomeCod AS CHARACTER   NO-UNDO.
DEFINE VARIABLE QNome AS CHARACTER   NO-UNDO.
DEFINE VARIABLE qcli AS CHARACTER   NO-UNDO.

assign
c-nr-nota-fis-ini = ""
c-nr-nota-fis-fim = "9999999"
c-nr-pedcli-ini   =  ""
c-nr-pedcli-fim   = "ZZZZZZZZ"
c-it-codigo-ini   = ""
c-it-codigo-fim   = "ZZZZZZZ"
dt-nota-ini       =  01/01/2013
dt-nota-fim       =  today
c-cod-estabel-ini = "422"
c-cod-estabel-fim = "434"
    qcli = "".

  if num-entries(c_param_funcao,"|") = 2 then do:
    ASSIGN 
        QNome   = ENTRY(1,c_param_funcao,"|")   /* paramentro de pesquisa*/
        qcli  = ENTRY(2,c_param_funcao,"|").   /* cliente da ultima pesquisa*/
        
        if num-entries (QNome,";") = 8 then do:
        
            assign
                    dt-nota-ini       =  if trim(entry(1,QNome,";")) = "" then dt-nota-ini else date(trim(entry(1,QNome,";")))
                    dt-nota-fim       =  date(trim(entry(2,QNome,";")))
                    c-nr-nota-fis-ini =  trim(entry(3,QNome,";"))
                    c-nr-nota-fis-fim =  trim(entry(4,QNome,";"))
                    c-nr-pedcli-ini   =  trim(entry(5,QNome,";"))
                    c-nr-pedcli-fim   =  trim(entry(6,QNome,";"))
                    c-it-codigo-ini   =  trim(entry(7,QNome,";"))
                    c-it-codigo-fim   =  trim(entry(8,QNome,";")).
        
        
        end.
        
  end.
               
c-nome-abrev = qcli.


c-pasta = "\\ungccb-vbd04\esp\temp\".
c-arquivo = "zoomprod" + STRING(TODAY,"99-99-9999") + "-" + replace(STRING(TIME,"HH:MM:SS"),":","-") + ".html".
 


define temp-table tt-acento no-undo
      
      field  i-asc             as INT 
      field  convertido        as char format "x(50)".

FUNCTION fGetData RETURNS CHARACTER (INPUT pData AS CHAR, INPUT c-it-codigo AS CHAR, INPUT c-cod-refer AS CHAR):
    DEF VAR cAux   AS CHAR NO-UNDO.
    DEF VAR de-tot AS DEC NO-UNDO.
    DEF VAR i-tot  AS INT NO-UNDO.


    FIND cot-est-mast WHERE cot-est-mast.item-cotacao  = c-it-codigo AND
                            cot-est-mast.nr-estrut     = int(c-cod-refer) NO-LOCK NO-ERROR.
    
    IF AVAIL cot-est-mast THEN DO:
        FIND var-result WHERE 
             var-result.item-cotacao  = cot-est-mast.item-cotacao AND
             var-result.nr-estrut     = cot-est-mast.nr-estrut AND
             var-result.nome-var      = pData NO-LOCK NO-ERROR.
            
        IF AVAIL var-result THEN DO:
            RETURN string(var-result.valor-dec).
        END.
        ELSE DO:
            RETURN string(1).
        END.

    END.
    ELSE DO:
        RETURN string(0).
    END.
    
END FUNCTION.

FUNCTION acentos_iso RETURNS CHAR (INPUT c-string AS CHAR) FORWARD.
RUN carrega_acentos.
  
  assign  lctabela2 = lctabela2 +
    "<HTML>":U + chr(10) +
    "<HEAD>":U + chr(10) +
      "<TITLE> .::Zoom itens de notas fiscais::. </TITLE>":U + chr(10) +
    "<a name=~"top~"></a>" +
    "</HEAD>":U + chr(10) +
    "<BODY>":U + chr(10) 
    .

  
    assign  lctabela2 = lctabela2 +
     "<table>"  +
    "<form action=~"?~"  method=~"post~">" +       
    "<tr>" +
        "<td class=barcad colspan=1>" +
        "<SPAN class=Labels>" +
            "Data Nt.Fiscal: " + "</td><td class=barcad colspan=2>" +
            "<input name=~"QDTini~" id=~"QDTini~" type=~"text~" value=~"" + string(dt-nota-ini,"99/99/9999") + "~" class=DefaultField>" +
            "<SPAN class=Labels>" + "&nbsp;&nbsp;A&nbsp;&nbsp;</SPAN>" +
            "<input name=~"QDTfim~" id=~"QDTfim~" type=~"text~" value=~"" + string(dt-nota-fim,"99/99/9999") + "~" class=DefaultField>" +
    "</td></tr>" +
    "<tr>" +
        "<td class=barcad colspan=1>" +
        "<SPAN class=Labels>" +
            "Nr.Nota Fiscal: " + "</td><td class=barcad colspan=2>" +
            "<input name=~"QNFini~" id=~"QNFini~" type=~"text~" value=~"" + c-nr-nota-fis-ini + "~" class=DefaultField>" +
            "<SPAN class=Labels>" + "&nbsp;&nbsp;A&nbsp;&nbsp;</SPAN>" +
            "<input name=~"QNFfim~" id=~"QNFfim~" type=~"text~" value=~"" + c-nr-nota-fis-fim + "~" class=DefaultField>" +
    "</td></tr>" +
    "<tr>" +
        "<td class=barcad colspan=1>" +
        "<SPAN class=Labels>" +
            "Pedido Cliente: " + "</td><td class=barcad colspan=2>" +
            "<input name=~"QPDini~" id=~"QPDini~" type=~"text~" value=~"" + c-nr-pedcli-ini + "~" class=DefaultField>" +
            "<SPAN class=Labels>" + "&nbsp;&nbsp;A&nbsp;&nbsp;</SPAN>" +
            "<input name=~"QPDfim~" id=~"QPDfim~" type=~"text~" value=~"" + c-nr-pedcli-fim + "~" class=DefaultField>" +
    "</td></tr>" +
    "<tr>" +
        "<td class=barcad colspan=1>" +
        "<SPAN class=Labels>" +
            "Cod. Produto: " + "</td><td class=barcad colspan=2>" +
            "<input name=~"QITini~" id=~"QITini~" type=~"text~" value=~"" + c-it-codigo-ini + "~" class=DefaultField>" +
            "<SPAN class=Labels>" + "&nbsp;&nbsp;A&nbsp;&nbsp;</SPAN>" +
            "<input name=~"QITfim~" id=~"QITfim~" type=~"text~" value=~"" + c-it-codigo-fim + "~" class=DefaultField>" +
    "</td></tr>" +


    

    "<tr>" +
       
         "<td class=barcad colspan=1>" +
        "<SPAN class=Labels>" +
            "Nome Abrev: " + "</td><td class=barcad colspan=2>" +
          "<input name=~"qclitela~" id=~"qclitela~" type=~"text~" value=~"" + qcli + "~" disabled='disabled' class=DefaultField>" +
          "<input name=~"qcli~" id=~"qcli~" type=~"hidden~" value=~"" + qcli + "~" class=DefaultField>" +

        "&nbsp;&nbsp;<input name=~"Pesquisar~" value=~"Pesquisar~" type=~"submit~" class=~"DefaultField~"  >" +
        "</td>" +
    "</tr>" +
    
    "</form>" +
    "</table>".

  /* Output your custom HTML to WEBSTREAM here (using {&OUT}).                */
    
    
   assign  lctabela2 = lctabela2 +
        "<table width=~"650px~">" +
        "<tr class=HeaderCadBG>"  +
            "<td><SPAN class=HeaderCad>" +
            "Nr. Pedido"  +
            "</SPAN></td>"  +
             "<td><SPAN class=HeaderCad>" +
            "Ped.Cliente"  +
            "</SPAN></td>"  +
            "<td><SPAN class=HeaderCad>"  +
            "It. Codigo" +
            "</SPAN></td>"  +
            "<td><SPAN class=HeaderCad>"  +
            "Vl. Unit."  +
            "</SPAN></td>"  +

            
            "<td><SPAN class=HeaderCad>"  +
            "Dt Emissao"  +
            "</SPAN></td>"  +
            "<td><SPAN class=HeaderCad>"  +
            "Nota Fiscal"  +
            "</SPAN></td>"  +
            "<td><SPAN class=HeaderCad>"  +
            "Data Nota"  +
            "</SPAN></td>"  +
"<td><SPAN class=HeaderCad>"  +
            "Estabel."  +
            "</SPAN></td>"  +
"<td><SPAN class=HeaderCad>"  +
            "Serie"  +
            "</SPAN></td>"  +

"<td><SPAN class=HeaderCad>"  +
            " "  +
            "</SPAN></td>"  +


        "</tr>"
        .
    


   /*****************************************************************/


def var i-ct as integer no-undo.

for each ped-venda where 
     ped-venda.nome-abrev = c-nome-abrev and
     ped-venda.nr-pedcli >= c-nr-pedcli-ini and 
     ped-venda.nr-pedcli <= c-nr-pedcli-fim NO-LOCK USE-INDEX ch-pedido,
    each ped-item of ped-venda where 
         ped-item.it-codigo >= c-it-codigo-ini and
         ped-item.it-codigo <= c-it-codigo-fim and
         ped-item.ind-componen < 3 and 
         (can-find (first item where item.it-codigo = ped-item.it-codigo and item.ge-codigo >= 41 and item.ge-codigo <= 49)) NO-LOCK USE-INDEX ch-item-ped,
    each it-nota-fisc where
            it-nota-fisc.nr-nota-fis >= c-nr-nota-fis-ini and
            it-nota-fisc.nr-nota-fis <= c-nr-nota-fis-fim and            
            it-nota-fisc.nr-pedido   =  ped-venda.nr-pedido and
            it-nota-fisc.nr-seq-ped  =  ped-item.nr-sequencia  and
            it-nota-fisc.nome-ab-cli =  ped-item.nome-abrev and
            it-nota-fisc.dt-emis-nota >= dt-nota-ini and
            it-nota-fisc.dt-emis-nota <= dt-nota-fim and
            (it-nota-fisc.cod-estabel >= c-cod-estabel-ini  and 
            it-nota-fisc.cod-estabel <= c-cod-estabel-fim ) and
            it-nota-fisc.dt-cancela = ?  NO-LOCK USE-INDEX ch-pedseq,
         each nota-fiscal of  it-nota-fisc where
        
         nota-fiscal.dt-cancela = ? no-lock USE-INDEX ch-nota
                
               break by ped-venda.nr-pedcli  
                     by ped-item.nr-sequencia  .
               
               if  first-of(ped-item.nr-sequencia) then   assign   c-notas = "" c-bobinas = "".
               
                      c-notas = nota-fiscal.nr-nota-fis  + "-" + c-notas.
                      
                      for each fat-ser-lote of it-nota-fisc no-lock.
                         for each it-pallet where it-pallet.cod-estabel  = it-nota-fisc.cod-estabel and
                                                 it-pallet.nr-pallet =  fat-ser-lote.nr-serlote and
                                                 it-pallet.it-codigo = it-nota-fisc.it-codigo  no-lock.
                                                 c-bobinas = replace(replace(replace(it-pallet.lote-bobina,"'",""),'"',''),",","") + "-" + c-bobinas .
                         end.
                      end.
                                                 

                      ASSIGN vTipoItem = STRING(ped-item.it-codigo)
                                                vTotalLiq = STRING(it-nota-fisc.vl-preuni  / (IF nota-fiscal.vl-taxa-exp = 0 THEN 1 ELSE nota-fiscal.vl-taxa-exp), ">>>,>>9.99")
                                                vCodEstabel = it-nota-fisc.cod-estabel. 
                                                /*STRING((it-nota-fisc.vl-tot-item)/(it-nota-fisc.qt-faturada[1]))*/
  if last-of(ped-item.nr-sequencia) THEN DO:
  
                      
                       
/*aqui disp*/
                      
                                          ASSIGN cArrayReturn = "'" + STRING(c-bobinas)             + "'," +
                                                                "'" + STRING(ped-venda.nr-pedido) + " - " + STRING(ped-item.nr-sequencia)             + "'," +
                                                                "'" + STRING(ped-item.it-codigo)             + "'," +
                                                                "'" + c-notas       + "'," +
                                                                "'" + acentos_iso(Replace(STRING(ped-venda.observacoes),"'", " "))          + "'," +
                                                                /*"'" + STRING(ped-item.qt-pedida)           + "'," + - clmiran - comentada e duplicada a linha qt-atendid*/
                                                                "'" + STRING(ped-item.qt-atendida)           + "'," +
                                                                "'" + STRING(ped-item.qt-atendida)           + "'," +
                                                                "'" + STRING(ped-venda.dt-emissao)           + "'," +
                                                                "'" + TRIM(STRING((it-nota-fisc.vl-tot-item  / (IF nota-fiscal.vl-taxa-exp = 0 THEN 1 ELSE nota-fiscal.vl-taxa-exp))/(it-nota-fisc.qt-faturada[1]), ">>>,>>9.99"))  + "','" + 
                                            /* Apenas ICMS    "'" + STRING(it-nota-fisc.vl-preuni,">>>,>>9.99")  + "','" + */
                                                                fGetdata("largura",ped-item.it-codigo,ped-item.cod-refer ) + "','" + it-nota-fisc.cod-estabel + "'" /*Adicionado clmiran - 12/12/2006*/ .

                                     IF Var-Class = "BrowseLineDestaq" THEN DO:
                                            ASSIGN Var-Class = "BrowseLineNormal".
                                        END.
                                        ELSE DO:
                                            ASSIGN Var-Class = "BrowseLineDestaq".
                                        END.   
                                  i-ct = i-ct + 1.
                                    assign  lctabela2 = lctabela2 +
                                            "<tr>"  +
                                                "<td class=" + Var-Class + " width=~"80px~">":U + chr(10) +
                                                 
                                
                                          
                                        "<a href=~"javascript:selectValue(new Array("  +
                                        cArrayReturn   +
                                        "));~" class=~"BrowserLink~">"  +
                                        STRING(ped-venda.nr-pedido) + " - " + STRING(ped-item.nr-sequencia) +
                                        "</a>" +
                                
                                                "</td>"  +
                                                 "<td class=" + Var-Class + ">"  +
                                                   STRING(ped-item.nr-pedcli)  +
                                                "</td>"  +

                                                "<td class=" + Var-Class + ">"  +
                                                   STRING(ped-item.it-codigo)  +
                                                "</td>"  +
                                                
                                                "<td class=" + Var-Class + ">"  +
                                                    TRIM(STRING((it-nota-fisc.vl-tot-item  / (IF nota-fiscal.vl-taxa-exp = 0 THEN 1 ELSE nota-fiscal.vl-taxa-exp))/(it-nota-fisc.qt-faturada[1]), ">>>,>>9.99"))  +
                                                "</td>"  +

                                                
                                               
                                                                                
                                                "<td class=" + Var-Class + ">"  +
                                                    STRING(ped-venda.dt-emissao)  +
                                                "</td>"  +
                                                "<td class=" + Var-Class + ">"  +
                                                    STRING(nota-fiscal.nr-nota-fis) + "-" + 
                                                    STRING(it-nota-fisc.nr-seq-fat) + "</td>"  +
                                                    "<td class=" + Var-Class + ">"  + 
                                                    STRING(nota-fiscal.dt-emis-nota,"99/99/9999") + "</td>"  +
                                                    "<td class=" + Var-Class + ">"  +                                                                                                    
                                                    STRING(nota-fiscal.cod-estabel) + "</td>"  +
                                                    "<td class=" + Var-Class + ">"  + 
                                                    STRING(nota-fiscal.serie)  + "</td>"  +
                                                    "<td class=" + Var-Class + ">"  + 
                                                    (  if i-ct > 15 then                                                   
                                                    "<a href='#top'>Topo</a>" else "")
                                                      
                                                    
                                                + "</td>"  +

                                            "</tr>" + chr(10).
                                            .
                                            
                                            if i-ct > 15 then i-ct = 0.
         
         
         END.
         end.


   
   assign  lctabela2 = lctabela2 +
        "</table>".

  assign  lctabela2 = lctabela2 +
    "</BODY>":U + chr(10) +
    "</HTML>":U + chr(10)
    .
 
 
c_param_retorno1 = c_param_funcao.


        Procedure pi_tira_acento:

            Def Var c-byte1     As Char Extent 10 Form "X(70)".
            Def Var idx         As Int.
            Def Var idx1        As Int.
            Def Var achou       As Log.
            Def Var c-acento    As Char Form "x(01)" Extent 23.
            Def Var c-acento1   As Char Form "x(01)" Extent 23.
            Assign c-acento1[1] = "a"
                   c-acento1[2] = "e"
                   c-acento1[3] = "i"
                   c-acento1[4] = "o"
                   c-acento1[5] = "u"
                   c-acento1[6] = "a"
                   c-acento1[7] = "e"
                   c-acento1[8] = "i"
                   c-acento1[9] = "o"
                   c-acento1[10] = "o"
                   c-acento1[11] = "a"
                   c-acento1[12] = "e"
                   c-acento1[13] = "i"
                   c-acento1[14] = "o"
                   c-acento1[15] = "u"
                   c-acento1[16] = "a"
                   c-acento1[17] = "o"
                   c-acento1[18] = "a"
                   c-acento1[19] = "e"
                   c-acento1[20] = "i"
                   c-acento1[21] = "o"
                   c-acento1[22] = "u"
                   c-acento1[23] = "c".

            Assign c-acento[1] = "†"
                   c-acento[2] = "Ç"
                   c-acento[3] = "°"
                   c-acento[4] = "¢"
                   c-acento[5] = "£"
                   c-acento[6] = "Ö"
                   c-acento[7] = "ä"
                   c-acento[8] = "ç"
                   c-acento[9] = "ï"
                   c-acento[10] = "ï"
                   c-acento[11] = "É"
                   c-acento[12] = "à"
                   c-acento[13] = "å"
                   c-acento[14] = "ì"
                   c-acento[15] = "ñ"
                   c-acento[16] = "∆"
                   c-acento[17] = "‰"
                   c-acento[18] = "Ñ"
                   c-acento[19] = "â"
                   c-acento[20] = "ã"
                   c-acento[21] = "î"
                   c-acento[22] = "Å"
                   c-acento[23] = "á".



          Assign c-byte = "".
          Do idx = 1 To Length(c-campo).
              Assign achou = No.
              Do idx1 = 1 To 23:
                  
                 If Substr(c-campo,idx,1) = c-acento[idx1] Then
                 Do:
                     Assign c-byte = c-byte  + c-acento1[idx1]
                             achou = Yes.
                     Leave.
                 End.
              End.
              If Not achou Then
                Assign c-byte = c-byte  + Substr(c-campo,idx,1).
          End.

        End Procedure.

        
 
 PROCEDURE carrega_acentos .       
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  193 tt-acento.convertido = "&Aacute;". /*¡ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  225 tt-acento.convertido = "&aacute;". /*· */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  194 tt-acento.convertido = "&Acirc;". /*¬ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  194 tt-acento.convertido = "&Acirc;". /*¬ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  226 tt-acento.convertido = "&acirc;". /*‚ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  192 tt-acento.convertido = "&Agrave;". /*¿ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  224 tt-acento.convertido = "&agrave;". /*‡ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  197 tt-acento.convertido = "&Aring;". /*≈ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  229 tt-acento.convertido = "&aring;". /*Â */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  195 tt-acento.convertido = "&Atilde;". /*√ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  227 tt-acento.convertido = "&atilde;". /*„ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  196 tt-acento.convertido = "&Auml;". /*ƒ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  228 tt-acento.convertido = "&auml;". /*‰ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  198 tt-acento.convertido = "&AElig;". /*∆ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  230 tt-acento.convertido = "&aelig;". /*Ê */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  201 tt-acento.convertido = "&Eacute;". /*… */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  233 tt-acento.convertido = "&eacute;". /*È */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  202 tt-acento.convertido = "&Ecirc;". /*  */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  234 tt-acento.convertido = "&ecirc;". /*Í */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  200 tt-acento.convertido = "&Egrave;". /*» */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  232 tt-acento.convertido = "&egrave;". /*Ë */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  203 tt-acento.convertido = "&Euml;". /*À */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  235 tt-acento.convertido = "&euml;". /*Î */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  208 tt-acento.convertido = "&ETH;". /*– */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  240 tt-acento.convertido = "&eth;". /* */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  205 tt-acento.convertido = "&Iacute;". /*Õ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  237 tt-acento.convertido = "&iacute;". /*Ì */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  206 tt-acento.convertido = "&Icirc;". /*Œ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  238 tt-acento.convertido = "&icirc;". /*Ó */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  204 tt-acento.convertido = "&Igrave;". /*Ã */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  236 tt-acento.convertido = "&igrave;". /*Ï */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  207 tt-acento.convertido = "&Iuml;". /*œ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  239 tt-acento.convertido = "&iuml;". /*Ô */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  211 tt-acento.convertido = "&Oacute;". /*” */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  243 tt-acento.convertido = "&oacute;". /*Û */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  212 tt-acento.convertido = "&Ocirc;". /*‘ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  244 tt-acento.convertido = "&ocirc;". /*Ù */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  210 tt-acento.convertido = "&Ograve;". /*“ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  242 tt-acento.convertido = "&ograve;". /*Ú */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  216 tt-acento.convertido = "&Oslash;". /*ÿ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  248 tt-acento.convertido = "&oslash;". /*¯ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  213 tt-acento.convertido = "&Otilde;". /*’ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  245 tt-acento.convertido = "&otilde;". /*ı */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  214 tt-acento.convertido = "&Ouml;". /*÷ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  246 tt-acento.convertido = "&ouml;". /*ˆ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  218 tt-acento.convertido = "&Uacute;". /*⁄ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  250 tt-acento.convertido = "&uacute;". /*˙ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  219 tt-acento.convertido = "&Ucirc;". /*€ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  251 tt-acento.convertido = "&ucirc;". /*˚ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  217 tt-acento.convertido = "&Ugrave;". /*Ÿ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  249 tt-acento.convertido = "&ugrave;". /*˘ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  220 tt-acento.convertido = "&Uuml;". /*‹ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  252 tt-acento.convertido = "&uuml;". /*¸ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  199 tt-acento.convertido = "&Ccedil;". /*« */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  231 tt-acento.convertido = "&ccedil;". /*Á */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  209 tt-acento.convertido = "&Ntilde;". /*— */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  241 tt-acento.convertido = "&ntilde;". /*Ò */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  34 tt-acento.convertido = "&quot;". /*" */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  174 tt-acento.convertido = "&reg;". /*Æ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  169 tt-acento.convertido = "&copy;". /*© */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  221 tt-acento.convertido = "&Yacute;". /*› */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  253 tt-acento.convertido = "&yacute;". /*˝ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  222 tt-acento.convertido = "&THORN;". /*ﬁ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  254 tt-acento.convertido = "&thorn;". /*˛ */
     CREATE tt-acento.  ASSIGN tt-acento.i-asc =  223 tt-acento.convertido = "&szlig;". /*ﬂ */


       

 END PROCEDURE. 

FUNCTION acentos_iso RETURNS CHAR (INPUT c-string AS CHAR).       

    DEFINE VARIABLE nova_string AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE i AS INTEGER    NO-UNDO.
    nova_string = ''.
          
    DO i = 1 TO LENGTH(c-string):

        FIND FIRST tt-acento WHERE tt-acento.i-asc = ASC(SUBSTRING(c-string,i,1)) NO-LOCK NO-ERROR.

        IF AVAIL tt-acento THEN DO:               
                  nova_string = nova_string + tt-acento.convertido.
        END.
        ELSE DO:
             nova_string = nova_string + SUBSTRING(c-string,i,1).

        END.

     END.

       
    RETURN nova_string.    

END FUNCTION.   



