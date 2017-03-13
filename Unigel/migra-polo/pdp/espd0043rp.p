/*******************************************************************************
**
**    PROGRAMA: pdp/espd0043rp.p
**        DATA: Novembro de 2016
**
**    OBJETIVO: Estat¡stica atendimento de Pedidos
**                      
**       AUTOR: Edson - damgra
**
**      VERSAO: 12.00.00.000 - 21/12/2016
**      SOLICITA€ÇO: 218
** Este fonte e de propriedade exclusiva da UNIGEL S.A, 
** sua reproducao parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
********************************************************************************/
{include/i-prgvrs.i espd0043RP 12.00.00.000}

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i espd0043 MPD}
&ENDIF

{include/i_fnctrad.i}
{include/i-rpvar.i}
{include/i-rpcab.i}
{include/tt-edit.i}
{include/pi-edit.i}
{utp/ut-glob.i}

define temp-table tt-param no-undo
    field destino           as integer
    field arquivo           as char format "x(35)"
    field usuario           as char format "x(12)"
    FIELD execucao          AS INT
    field data-exec         as date
    field hora-exec         as integer
    field classifica        as integer
    field desc-classifica   as char format "x(40)"
    field modelo-rtf        as char format "x(35)"
    field l-habilitaRtf     as LOG
    FIELD dt-entrega-ini    AS DATE    
    FIELD dt-entrega-fim    AS DATE    
    FIELD c-ep-codigo-ini   AS CHARACTER
    FIELD c-ep-codigo-fim   AS CHARACTER
    FIELD c-cod-estabel-ini AS CHARACTER
    FIELD c-cod-estabel-fim AS CHARACTER
    FIELD c-nome-abrev-ini  AS CHARACTER 
    FIELD c-nome-abrev-fim  AS CHARACTER 
    FIELD c-it-codigo-ini   AS CHARACTER 
    FIELD c-it-codigo-fim   AS CHARACTER 
    FIELD i-ge-codigo-ini   AS INTEGER   
    FIELD i-ge-codigo-fim   AS INTEGER    
    FIELD p-perc-ini        AS DECIMAL  
    FIELD p-perc-fim        AS DECIMAL  .


define temp-table tt-digita no-undo
    field ordem                 as integer   format ">>>>9"
    field exemplo               as character format "x(30)"
    index id ordem.
    
def temp-table tt-raw-digita
    field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.
     
create tt-param.   

raw-transfer raw-param to tt-param.  

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

def stream st-saida.
def stream s-log.
def stream s-log-email.
def stream str-rp.
def var h-acomp              as handle no-undo.
def var c-arquivo            as char   no-undo.
  
{cdp/cd0666.i}

def var c-acompanha          as char                                           no-undo.
def var i-cont               as int                                            no-undo.
def var c-arq-modelo         as char extent 2 no-undo.


DEFINE VARIABLE idx               AS int     NO-UNDO.
DEFINE VARIABLE dt-entrega-ini    AS DATE    initial 12/01/2016 format "99/99/9999"   NO-UNDO.
DEFINE VARIABLE dt-entrega-fim    AS DATE    initial 12/10/2016 format "99/99/9999"   NO-UNDO.
DEFINE VARIABLE c-ep-codigo-ini   AS CHARACTER initial "{cdp\poloestab.i 422}"  NO-UNDO.
DEFINE VARIABLE c-ep-codigo-fim   AS CHARACTER initial "{cdp\poloestab.i 434}"  NO-UNDO.
DEFINE VARIABLE c-cod-estabel-ini AS CHARACTER initial "{cdp\poloestab.i 422}"  NO-UNDO.
DEFINE VARIABLE c-cod-estabel-fim AS CHARACTER initial "{cdp\poloestab.i 434}"  NO-UNDO.
DEFINE VARIABLE c-nome-abrev-ini  AS CHARACTER initial ""  NO-UNDO.
DEFINE VARIABLE c-nome-abrev-fim  AS CHARACTER initial "zzzz"  NO-UNDO.
DEFINE VARIABLE c-it-codigo-ini   AS CHARACTER initial ""  NO-UNDO.
DEFINE VARIABLE c-it-codigo-fim   AS CHARACTER initial "zzzz"  NO-UNDO.
DEFINE VARIABLE i-ge-codigo-ini   AS INTEGER   initial 41  NO-UNDO.
DEFINE VARIABLE i-ge-codigo-fim   AS INTEGER   initial 49  NO-UNDO.
DEFINE VARIABLE p-perc-ini        AS DECIMAL  FORMAT "->>>,>>9.9999" initial 10 NO-UNDO.
DEFINE VARIABLE p-perc-fim        AS DECIMAL  FORMAT "->>>,>>9.9999" initial 13  NO-UNDO.
DEFINE VARIABLE p-perc-atu        AS DECIMAL  FORMAT "->>>,>>9.9999" INITIAL 3  NO-UNDO.
DEFINE VARIABLE dt-atu            AS DATE        NO-UNDO.
DEFINE VARIABLE var-Larg          AS INT NO-UNDO.
DEFINE VARIABLE var-diin          AS INT NO-UNDO.
DEFINE VARIABLE var-diex          AS INT NO-UNDO.


/*------------------------------------------------------------------------------
             Definicao de Variaveis para trabalhar com Excel
------------------------------------------------------------------------------*/
DEFINE NEW SHARED VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE NEW SHARED VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE NEW SHARED VARIABLE chWorksheet             AS COM-HANDLE               EXTENT 3.
DEFINE NEW SHARED VARIABLE iColumn                 AS INTEGER                  INITIAL 2.
DEFINE NEW SHARED VARIABLE cColumn                 AS CHARACTER.
DEFINE NEW SHARED VARIABLE cRange                  AS CHARACTER.
DEFINE NEW SHARED VARIABLE chWebview               AS com-handle.
/*-------------------------------------------------------------------------------*/
DEFINE VARIABLE i-ct                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE i_seq                 AS INTEGER     NO-UNDO.
DEFINE VARIABLE i_idx                 AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-linha               AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-linha-final         AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-linha-vl-pedido     AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-pagina              AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-valor               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-ult-col             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-mo-codigo AS INTEGER     NO-UNDO.
DEFINE VARIABLE de-preco-conv AS DECIMAL     NO-UNDO.
ASSIGN
    dt-entrega-ini     = tt-param.dt-entrega-ini   
    dt-entrega-fim     = tt-param.dt-entrega-fim   
    c-ep-codigo-ini    = tt-param.c-ep-codigo-ini  
    c-ep-codigo-fim    = tt-param.c-ep-codigo-fim  
    c-cod-estabel-ini  = tt-param.c-cod-estabel-ini
    c-cod-estabel-fim  = tt-param.c-cod-estabel-fim
    c-nome-abrev-ini   = tt-param.c-nome-abrev-ini 
    c-nome-abrev-fim   = tt-param.c-nome-abrev-fim 
    c-it-codigo-ini    = tt-param.c-it-codigo-ini  
    c-it-codigo-fim    = tt-param.c-it-codigo-fim  
    i-ge-codigo-ini    = tt-param.i-ge-codigo-ini  
    i-ge-codigo-fim    = tt-param.i-ge-codigo-fim  
    p-perc-ini         = tt-param.p-perc-ini       
    p-perc-fim         = tt-param.p-perc-fim  .     
      

find mgmulti.empresa where 
     mgmulti.empresa.ep-codigo = i-ep-codigo-usuario no-lock no-error.

if avail mgmulti.empresa then 
    assign c-empresa = empresa.razao-social.
else 
    assign c-empresa = "Unigel S.A".

assign c-programa = "espd0043"
       c-sistema  = "MPD"
       c-versao   = "12.00.00"
       c-revisao  = "000".
 
{utp/ut-liter.i Estat¡stica_atendimento_de_Pedidos) * L}
assign c-titulo-relat = trim(return-value).
{utp/ut-liter.i MPD * L}
assign c-sistema = trim(return-value).
 
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Separando_movimentos... * L}
run pi-inicializar in h-acomp (input return-value).                    

run gerar-cabecalho.

assign chExcelApplication:DisplayAlerts = false
       chExcelApplication:visible       = NO.
    
run gerar-excel.
      
dos silent del VALUE(tt-param.arquivo).

chWorkbook:SaveAs(tt-param.arquivo,51,,,,,) no-error.

if  tt-param.DESTINO = 3 and tt-param.execucao = 1 then do:
    chExcelApplication:visible = yes.
end.
                      
run pi-finalizar in h-acomp.

  
release object chExcelApplication no-error.
release object chWorkbook         no-error.
release object chWorksheet[1]     no-error.

return "OK".
/* Fim do programa */

procedure gerar-cabecalho.
     
    create "Excel.Application" chExcelApplication.                  
    chWorkbook = ChExcelApplication:Workbooks:add().
       
end procedure.

procedure gerar-excel:
    def var i-num-reg       as integer no-undo.

    assign c-valor =               
       "Empr."   
     + CHR(160) + "Estab."      
     + CHR(160) + "Cliente"    
     + CHR(160) + "Nome Abrev."  
     + CHR(160) + "Pedido"  
     + CHR(160) + "Ped.Cliente"  
     + CHR(160) + "TP"  
     + CHR(160) + "Item"        
     + CHR(160) + "UN"  
     + CHR(160) + "GE"  
     + CHR(160) + "Seq."  
     + CHR(160) + "Dt.Entrega"  
     + CHR(160) + "Qt Atendida"  
     + CHR(160) + "Qt Pedida"   
     + CHR(160) + "% Atendido"  
     + CHR(160) + "Larg" 
     + CHR(160) + "DIEX"  
     + CHR(160) + "DIIN"           
     + CHR(160) + "Pre‡o Item"  
     + CHR(160) + "Valor Diferen‡a"      
     + CHR(160) + "Descri‡ao item"  .
    
           
    assign chExcelApplication:range("a"  + string(4)):VALUE = c-valor
           i-ct     = 0
           i-linha  = 4.    

DO dt-atu = dt-entrega-ini TO dt-entrega-fim .
   
    FOR EACH ped-item WHERE
        ped-item.dt-entrega  = dt-atu AND        
        ped-item.nome-abrev >= c-nome-abrev-ini AND
        ped-item.nome-abrev <= c-nome-abrev-fim AND
        ped-item.it-codigo  >= c-it-codigo-ini AND
        ped-item.it-codigo  <= c-it-codigo-fim AND
        ped-item.ind-componen < 3 AND
        ped-item.cod-sit-item = 3 NO-LOCK USE-INDEX peditem-09,
        EACH ITEM WHERE
        ITEM.it-codigo = ped-item.it-codigo AND
            ITEM.ge-codigo >= i-ge-codigo-ini AND
            ITEM.ge-codigo <= i-ge-codigo-fim NO-LOCK,
        EACH ped-venda OF ped-item WHERE
            ped-venda.cod-estabel >= c-cod-estabel-ini AND
            ped-venda.cod-estabel <= c-cod-estabel-fim NO-LOCK,
        FIRST estabelec WHERE estabelec.cod-estabel = ped-venda.cod-estabel AND
                estabelec.ep-codigo >= c-ep-codigo-ini AND
                estabelec.ep-codigo <= c-ep-codigo-fim no-lock.
    
            p-perc-atu = 0.                                               
            ASSIGN p-perc-atu = (ped-item.qt-pedida - ped-item.qt-atendida) /  ped-item.qt-pedida * 100 NO-ERROR.
    
            IF  NOT   (p-perc-atu >= p-perc-ini AND
                    p-perc-atu <= p-perc-fim) THEN NEXT.

            ASSIGN 
                 var-Larg     = 0 
                 var-diin     = 0
                 var-diex     = 0.
            
            FIND FIRST var-result WHERE var-result.nome-var = "Largura" AND 
                 var-result.nr-estrut = int(ped-item.cod-refer) AND
                 var-result.item-cotacao = ped-item.it-codigo 
                 /*var-result.sequencia = ped-item.nr-sequencia */ NO-LOCK NO-ERROR.
            IF AVAIL var-result THEN
                 ASSIGN var-Larg = var-result.valor-dec.
            
            FIND FIRST var-result WHERE var-result.nome-var = "DIIN" AND 
                     var-result.nr-estrut = int(ped-item.cod-refer) AND
                     var-result.item-cotacao = ped-item.it-codigo 
                     /*var-result.sequencia = ped-item.nr-sequencia */ NO-LOCK NO-ERROR.
            IF AVAIL var-result THEN
                 ASSIGN var-diin = var-result.valor-dec.
        
            FIND FIRST var-result WHERE var-result.nome-var = "DIEX" AND 
                 var-result.nr-estrut = int(ped-item.cod-refer) AND
                 var-result.item-cotacao = ped-item.it-codigo 
                 /*var-result.sequencia = ped-item.nr-sequencia*/ NO-LOCK NO-ERROR.
            IF AVAIL var-result THEN
                 ASSIGN var-diex = var-result.valor-dec.
        
            i-mo-codigo = ped-venda.mo-codigo.
                              
             IF i-mo-codigo > 0 THEN
                DO:
                  RUN cdp/cd0812.p (INPUT i-mo-codigo,
                                    INPUT 0,
                                    INPUT ped-item.vl-preuni,
                                    INPUT ped-item.dt-entrega,
                                    OUTPUT de-preco-conv).
                  IF de-preco-conv = ? THEN
                     ASSIGN de-preco-conv = ped-item.vl-preuni.
                END.
                ELSE
                ASSIGN de-preco-conv = ped-item.vl-preuni.
    
                 i-ct = i-ct + 1.                        
                        
                        if substring(string(i-ct,"99999999"),8,1) = "0"  then do:                
                            assign c-acompanha =  "Listando % movtos => " + " lidos:" + string(i-ct).                                   
                            run pi-acompanhar in h-acomp (input c-acompanha).
                        end.                       
                           

                        ASSIGN c-valor = string(estabelec.ep-codigo)
                            + chr(160) + string(ped-venda.cod-estabel)
                            + chr(160) + string(ped-venda.cod-emitente)
                            + chr(160) + string(ped-item.nome-abrev)
                            + chr(160) + string(ped-venda.nr-pedido)
                            + chr(160) + string(ped-item.nr-pedcli )
                            + chr(160) + string(ped-venda.tp-pedido)
                            + chr(160) + string(ped-item.it-codigo )
                            + chr(160) + string(ITEM.un)
                            + chr(160) + string(ITEM.GE-codigo)
                            + chr(160) + string(ped-item.nr-sequencia)
                      /*L*/ + chr(160) + string(ped-item.dt-entrega,"99/99/9999")
                      /*M*/ + chr(160) + STRING(ped-item.qt-atendida)
                      /*N*/ + chr(160) + string(ped-item.qt-pedida)
                      /*O*/ + chr(160) + string(p-perc-atu)
                      /*P*/ + chr(160) + string(var-Larg)
                      /*Q*/ + chr(160) + string(var-diex)
                      /*R*/ + chr(160) + string(var-diin)
                      /*S*/ + chr(160) + string(de-preco-conv)
                      /*T*/ + chr(160) + string((ped-item.qt-atendida - ped-item.qt-pedida) * de-preco-conv )
                      /*u*/ + chr(160) + string(trim(replace(replace(replace(item.desc-item,";",","),chr(8)," "),chr(10)," "))).       
                        
                        assign  i-linha = i-linha + 1. 
                                  chExcelApplication:range("a"  + string(i-linha)):VALUE = c-valor.       
     
    END.
END.
    chExcelApplication:Range("A" + string(4) + ":A" + string(i-Linha + 1)):select.      
  
    /* explode a primeira coluna nas colunas respectivas*/

    chExcelApplication:selection:TextToColumns    (, /* Destination          */
                                         1,          /* DataType             */
                                         ,           /* TextQualifier        */
                                         ,           /* ConsecutiveDelimiter */
                                         ,           /* Tab                  */
                                         ,           /* Semicolon            */
                                         ,           /* Comma                */
                                         ,           /* Space                */
                                         true,       /* Other                */
                                         CHR(160),   /* OtherChar            */
                                         ,           /* FieldInfo            */
                                         ) no-error.
      
    ASSIGN  chExcelApplication:range("a"  + string(1)):VALUE = "Estat¡stica atendimento de Pedidos no per¡odo: " + string(dt-entrega-ini,"99/99/9999") + " a " + string(dt-entrega-fim,"99/99/9999")  .

     ASSIGN  
         chExcelApplication:range("t"   + string(3)):VALUE = "=subtotal(9,t5:t" + string(i-linha) + ")"
          
    c-ult-col = "u".
    chExcelApplication:range("a1"):Font:FontStyle = "Negrito".
    chExcelApplication:range("a1"):Font:Size = 14. 
    chExcelApplication:range("a1"):Font:Name = "Arial".            
    chExcelApplication:range("a4:"  +  c-ult-col + "4"):Interior:ColorIndex = 37.     
    chExcelApplication:range("s3:s" + STRING(i-linha)):NumberFormat = "#.##0,0000".    
    chExcelApplication:range("t3:t" + STRING(i-linha)):NumberFormat = "#.##0,00".    
    chExcelApplication:range("m3:o" + STRING(i-linha)):NumberFormat = "#.##0,00".    
    chExcelApplication:range("a2:" +  c-ult-col + "4"):Font:FontStyle = "Negrito".
    chExcelApplication:Rows("4:4"):Autofilter (,,,).     
    chExcelApplication:Columns("u:u"):ColumnWidth = 30. 
    chExcelApplication:Columns("u:u"):wraptext = YES. 
    chExcelApplication:Cells:EntireColumn:AutoFit.
    chExcelApplication:Columns("a:a"):ColumnWidth = 10.   
    chExcelApplication:Columns("b:b"):ColumnWidth = 7. 
    chExcelApplication:range("a1"):select.
    
end procedure.


