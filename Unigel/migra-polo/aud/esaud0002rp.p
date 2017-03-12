/*****************************************************************************
**
**       Programa: esaud0002rp.p
**
**       Author...........: Amgra / Edson
**       Created..........: 25/04/2011     
**
**       Objetivo: Exporta‡Æo de Itens para auditoria
**
**       OBS.....: 
**
*******************************************************************************/
define buffer empresa for mgmulti.empresa.

define variable c-prog-gerado as character no-undo initial "esaud0002rp".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

 
 
def input param c-cod-estabel-ini as char  no-undo. /*solic-318*/
def input param c-cod-estabel-fim as char  no-undo. /*solic-318*/
def input param i-ge-codigo-ini as integer initial 82 no-undo.
def input param i-ge-codigo-fim as integer initial 82 no-undo.
def input param c-fm-codigo-ini as char initial "" no-undo.
def input param c-fm-codigo-fim as char initial "zzz" no-undo.
def input param tg-imprime as logical no-undo.
def var d-dt-ref-compra as date initial 01/01/2012 no-undo.



/*
run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").
*/

/****************** Defini‡Æo de Tabelas Tempor rias do Relat¢rio **********************/

        

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

def new global shared var i-ep-codigo-usuario  like empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def new global shared var i-num-ped-exec-rpw  as integer no-undo.   
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
def new global shared var l-rpc as logical no-undo.
def new global shared var r-registro-atual as rowid no-undo.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var i-num-ped as integer no-undo.         
def new global shared var v_cdn_empres_usuar   like empresa.ep-codigo        no-undo.
def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab     as handle                   no-undo.
def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
def new global shared var v_num_tip_aces_usuar   as int                      no-undo.
def new global shared var rw-log-exec            as rowid                    no-undo.

/****************** FIM INCLUDE COM VARIµVEIS GLOBAIS *********************/
/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 



/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/* para executar o Excel */

DEF VAR c-arq             AS CHAR                NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR                NO-UNDO.   
DEF VAR l-cria            AS LOG                 NO-UNDO.
DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
DEF VAR i-linha           AS INT                 NO-UNDO.
DEF VAR c-coluna          AS char                NO-UNDO.

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.
DEFINE VARIABLE arquivo-jr   AS CHARACTER  FORMAT "x(50)" NO-UNDO.
/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

/****************** Defini‡ao de Vari veis dos Calculos do Relat¢rio *************************/ 


/***************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/
def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.

/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form HEADER
    fill("-", 170) format "x(170)" SKIP 
    WITH DOWN WIDTH 170 NO-BOX STREAM-IO FRAME f-relat-linha.

form HEADER
    fill(" ", 170) format "x(170)" SKIP 
    WITH DOWN WIDTH 170 NO-BOX STREAM-IO FRAME f-relat-branco.


 
def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.


def var rw-log-exec                            as rowid no-undo.
def var c-erro-rpc as character format "x(60)" initial " " no-undo.
def var c-erro-aux as character format "x(60)" initial " " no-undo.
def var c-ret-temp as char no-undo.
def var h-servid-rpc as handle no-undo.     
define var c-empresa       as character format "x(40)"      no-undo.
define var c-titulo-relat  as character format "x(50)"      no-undo.
define var i-numper-x      as integer   format "ZZ"         no-undo.
define var da-iniper-x     as date      format "99/99/9999" no-undo.
define var da-fimper-x     as date      format "99/99/9999" no-undo.
define var i-page-size-rel as integer                       no-undo.
define var c-programa      as character format "x(08)"      no-undo.
define var c-versao        as character format "x(04)"      no-undo.
define var c-revisao       as character format "999"        no-undo.

define new shared var c-impressora   as character                      no-undo.
define new shared var c-layout       as character                      no-undo.
define new shared var v_num_count     as integer                       no-undo.
define new shared var c-arq-control   as character                     no-undo.
define new shared var c-sistema       as character format "x(25)"      no-undo.
define new shared var c-rodape        as character                     no-undo.

define new shared buffer b_ped_exec_style for ped_exec.
define new shared buffer b_servid_exec_style for servid_exec.
 
define new shared stream str-rp.
DEF VAR c-arquivo_2 AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR c-arquivo_txt AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR c-arquivo_xls AS CHAR FORMAT "x(50)" NO-UNDO.
 
  
DEFINE VARIABLE r-mov AS CHAR       NO-UNDO.
def var  i-ct as integer no-undo.

def var d-dt-ult-entrada as date  no-undo.
def var d-dt-ult-saida as date  no-undo.



assign c-programa     = "esaud0002rp"
       c-versao       = "2.00"
       c-revisao      = "1.00.000"
       c-titulo-relat = "Exporta‡Æo de Itens para auditoria"
       c-sistema      = "".

form header
    fill("-", 170) format "x(170)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 159 page-number(str-rp) at 166 format ">>>>9" skip
    fill("-", 148) format "x(148)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") SKIP
    fill("-", 170) format "x(170)" skip
    with stream-io width 170 no-labels no-box page-top frame f-cabec.

form header
    c-rodape format "x(170)"
    with stream-io width 170 no-labels no-box page-bottom frame f-rodape.
 
/* for each e disp */



 
run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Gerando Relat¢rio, Aguarde... ").

assign v-num-reg-lidos = 0.
   
ASSIGN c-arquivo_txt = session:TEMP-DIRECTORY + "esaud0002_itens_" + replace(STRING(TIME,"HH:MM:SS"),":","") + ".csv".

OUTPUT TO VALUE (c-arquivo_txt) NO-CONVERT.   
PUT      ";" 
    SKIP.
  PUT 
        "Exporta‡Æo de Itens para auditoria" ";"
         SKIP.
PUT      ";" 
    SKIP.
PUT      ";" 
    SKIP.
PUT      ";" 
    SKIP.

    
PUT    UNFORMATTED
"Estabel"
";" "Cod. Item"
";" "Descri‡Æo do Item"
";" "Grupo Estoque"
";" "DESC.Grupo"
";" "Fam¡lia"
";" "DESC.Familia"
";" "Status do item "
";" "Unidade de medida"
";" "Tipo Est.Seg"
";" "Quant.Seguran‡a"
";" "Tempo Seguran‡a"
";" "Ponto Encomenda"
";" "Lote M¡nimo"
";" "Lote M ximo"
";" "Dt.Ult.Entrada"
";" "Dt.Ult.Sa¡da"

    SKIP.
  

  
  FOR each item-uni-estab where 
        item-uni-estab.cod-estabel >= c-cod-estabel-ini and 
        item-uni-estab.cod-estabel <= c-cod-estabel-fim  no-lock, 
        EACH ITEM where  
                item-uni-estab.it-codigo = item.it-codigo and
                item.ge-codigo >= i-ge-codigo-ini and
                item.ge-codigo <= i-ge-codigo-fim and
                item.fm-codigo >= c-fm-codigo-ini and
                item.fm-codigo <= c-fm-codigo-fim NO-LOCK  ,                
        EACH grup-estoque OF ITEM  
         no-lock,
         each familia of item no-lock.
         

   
         assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

         assign d-dt-ult-entrada = ? 
                d-dt-ult-saida   = ? 
                r-mov            = "".
         
        for first ordem-compra where ordem-compra.it-codigo = item.it-codigo and
                    ordem-compra.cod-estabel = item-uni-estab.cod-estabel and
                    ordem-compra.data-emissao >= d-dt-ref-compra no-lock.
                    
                      r-mov = ordem-compra.cod-estabel. 
                    
         end.

        if r-mov = "" then                                    
        if can-find (first  movto-estoq   WHERE 
                       movto-estoq.it-codigo = ITEM.it-codigo AND
                       movto-estoq.cod-estabel = item-uni-estab.cod-estabel no-lock ) then              
                          r-mov = item-uni-estab.cod-estabel.
        else next.
        
        
        
        for  last   movto-estoq fields(movto-estoq.dt-trans movto-estoq.cod-estabel) WHERE 
                       movto-estoq.it-codigo = ITEM.it-codigo AND
                       movto-estoq.cod-estabel = item-uni-estab.cod-estabel and
                       movto-estoq.tipo-trans = 2 AND
                       movto-estoq.esp-docto <> 33   use-index item-data NO-LOCK.
                       
                       d-dt-ult-saida = movto-estoq.dt-trans.
                     
        end.
        
        for  last   movto-estoq fields(movto-estoq.dt-trans movto-estoq.cod-estabel) WHERE 
                       movto-estoq.it-codigo = ITEM.it-codigo AND
                       movto-estoq.cod-estabel = item-uni-estab.cod-estabel and
                       movto-estoq.tipo-trans = 1 AND
                       movto-estoq.esp-docto <> 33   use-index item-data NO-LOCK.
                       
                       d-dt-ult-entrada = movto-estoq.dt-trans.
                     
        end.

  
   
          PUT   UNFORMATTED 
           item-uni-estab.cod-estabel     
           ";" item.it-codigo 
           ";" REPLACE((IF SUBSTRING(item.desc-item,1,1) = CHR(34) THEN SUBSTRING(item.desc-item,2,60) ELSE item.desc-item) + " - " + REPLACE(replace(replace(replace(ITEM.narrativa,CHR(10)," "),CHR(13)," "),CHR(9)," "),";",","),";",",") 
           ";" grup-estoque.ge-codigo
           ";" grup-estoque.descricao
           ";" familia.fm-codigo
           ";" familia.descricao           
           ";" IF item.cod-obsoleto = 1 THEN "Ativo" ELSE "Obsoleto"
           ";" item.un 
           ";" entry(item-uni-estab.tipo-est-seg,"Quantidade,Tempo")
           ";" item-uni-estab.quant-segur
           ";" item-uni-estab.tempo-segur
           ";" item-uni-estab.ponto-encomenda  
           ";" item-uni-estab.lote-minimo 
           ";" item-uni-estab.lote-per-max
           ";" if d-dt-ult-entrada = ? then "" else   STRING(d-dt-ult-entrada,"99/99/9999")  
           ";" if d-dt-ult-saida   = ? then "" else   STRING(d-dt-ult-saida,"99/99/9999")           
           SKIP.
    
          /* i-ct = i-ct + 1.
           
           if substring(string(i-ct,"99999999"),7,2) = "00" then  message i-ct.
      */
     
         
  
END.


    
    

 OUTPUT CLOSE.


c-arquivo_xls = session:TEMP-DIRECTORY + "esaud0002_itens_" + replace(STRING(TIME,"HH:MM:SS"),":","") + ".xlsx".

 

    CREATE "Excel.Application" c-excel.
    ASSIGN c-excel:DisplayAlerts = FALSE.

            
    /* cria planilha*/
    c-arquivo_2 = c-arquivo_xls.       
    c-planilha  = c-excel:Workbooks:OPEN(c-arquivo_txt).
    c-planilha:SAVEas(c-arquivo_2,51,,,,,).
             
    c-planilha:CLOSE().

 
          
    ASSIGN c-planilha  = c-excel:Workbooks:OPEN(c-arquivo_2)
    c-relatorio = c-excel:Sheets:item(1).
     
       run pi-acompanhar in h-acomp(input "Aguarde formatando planilha gerada").

         

    RUN pi-salva-planilha.
 run pi-acompanhar in h-acomp(input "Aguarde copiando planilha gerada").
 dos silent copy value(c-arquivo_2) v:\temp.

    RUN pi-finalizar IN h-acomp.

   
    
    if tg-imprime = NO then 
      c-excel:visible = yes.
    else do: 
     
      c-excel:QUIT().
     end.
      

    RELEASE OBJECT c-relatorio.
    RELEASE OBJECT c-planilha.

    RELEASE OBJECT c-excel.
 

RETURN 'OK'.



PROCEDURE pi-salva-planilha:
    
 
            c-relatorio:range("a2"):Select.
            c-relatorio:range("a2"):Font:FontStyle = "Negrito".
            c-relatorio:range("a2"):Font:Size = 14. 

            c-relatorio:range("a6:q6"):Interior:ColorIndex = 55.
            
 
            c-relatorio:range("a6:q6"):Font:Name = "Arial".
             
            c-relatorio:range("a6:q6"):Font:FontStyle = "Negrito".
            c-relatorio:range("a6:q6"):Font:Size = 10.
            c-relatorio:range("a6:q6"):Font:ColorIndex = 2.
            
            c-relatorio:Rows("6:6"):Autofilter (,,,).

          /*  c-relatorio:Columns("G:K"):NumberFormat = "dd/mm/aaaa".*/
            c-relatorio:Columns("p:q"):NumberFormat = "dd/mm/aaaa".

            c-relatorio:Columns("k:o"):NumberFormat = "#.##0,0000".
            
            
            c-relatorio:Cells:Select.
            c-relatorio:Cells:EntireColumn:AutoFit.
            c-relatorio:Columns("A:A"):ColumnWidth = 20.
            c-relatorio:range("a2"):Select.

            c-relatorio:range("a1"):Select.



     c-planilha:SAVE().

END PROCEDURE.

/* fim do programa */
