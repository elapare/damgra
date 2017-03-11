/**************************************************************\
***************************************************************
**  Programa: cep\upc\upc-ce0919rp.p
**  Objetivo: Programa para Gerar em Excel o Saldo em Estoque
**             
**  Autor...: Jos‚ Roberto - VGA
**  Data....: Maio/2006
**  Versao..: I.00.000
***************************************************************
\**************************************************************/
DEF NEW GLOBAL SHARED VAR gwh-ce0919-rs-destino     AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR gwh-ce0919-rs-classif     AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR gwh-ce0919-rs-execucao    AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR gwh-ce0919-c-arquivo      AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR gwh-ce0919-bt-executa     AS WIDGET-HANDLE.

def var h-acomp              as handle no-undo.
def var v-num-reg-lidos      as int    no-undo.

/* Vari veis usadas para gerar planilha excel. */

DEF VAR c-arq-ex          AS CHAR  FORMAT "x(50)"  NO-UNDO.
DEF VAR c-arq-anexo       AS CHAR  FORMAT "x(50)"  NO-UNDO.   
DEF VAR l-cria            AS LOG                 NO-UNDO.
DEF VAR i-pagina          AS INT INIT 1          NO-UNDO.
DEF VAR i-linha           AS INT INIT 7          NO-UNDO.

DEFINE VARIABLE i-larg AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-diin AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-diex AS INTEGER    NO-UNDO.

DEFINE VARIABLE i-larg-p AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-diin-p AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-diex-p AS INTEGER    NO-UNDO.
DEFINE VARIABLE dt-entrega-jr AS date  NO-UNDO.
DEFINE VARIABLE dt-trans-jr AS date  NO-UNDO.
define variable c-nr-nf-trans as char no-undo.
define variable c-estab-trans as char no-undo.
DEFINE VARIABLE c-cod-estabel AS CHARACTER   NO-UNDO.



DEFINE VARIABLE c-nome-abrev AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-nr-pedcli AS CHARACTER  NO-UNDO.
/* para executar o Excel */

def var c-modelo-planilha  as char format "x(50)"         no-undo.
def var c-excel            as com-handle                  NO-UNDO.
def var c-planilha         as com-handle.
def var c-relatorio        as com-handle.

DEFINE VARIABLE desc-item-jr  LIKE item.desc-item  NO-UNDO.
DEFINE VARIABLE it-codigo-jr  LIKE item.it-codigo  NO-UNDO.
DEFINE VARIABLE un-jr         LIKE item.un         NO-UNDO.
DEFINE VARIABLE obs-jr        AS CHAR              NO-UNDO.
DEFINE VARIABLE data-jr       AS CHAR              NO-UNDO.
DEFINE VARIABLE GE-codigo-JR  LIKE item.ge-codigo  NO-UNDO.
DEFINE VARIABLE c-linha   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-arq-jr  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-arq-usuario AS CHARACTER  FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE l-imp-info    AS LOGICAL     NO-UNDO.

ASSIGN c-arq-usuario = SESSION:TEMP-DIRECTORY
       c-arq-usuario = c-arq-usuario + "ce0919.tmp".

IF valid-HANDLE(gwh-ce0919-rs-destino) THEN
    gwh-ce0919-rs-destino:SCREEN-VALUE = '2'. 

IF valid-HANDLE(gwh-ce0919-rs-classif) THEN
    gwh-ce0919-rs-classif:SCREEN-VALUE = '1'. 

IF valid-HANDLE(gwh-ce0919-rs-execucao) THEN
    gwh-ce0919-rs-execucao:SCREEN-VALUE = '1'. 

IF valid-HANDLE(gwh-ce0919-c-arquivo) THEN
    gwh-ce0919-c-arquivo:SCREEN-VALUE = STRING (c-arq-usuario). 

APPLY 'CHOOSE' TO gwh-ce0919-bt-executa.
 

ASSIGN c-arq-jr = c-arq-usuario.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.


IF SEARCH(c-arq-jr) <> ? THEN DO:
  INPUT FROM VALUE(c-arq-jr).
    

               /* Cria Aplica‡Æo do Excel */
 
   CREATE "Excel.Application" c-excel.
   ASSIGN c-excel:DisplayAlerts = FALSE.
   ASSIGN c-modelo-planilha = search("modelos/mod-ce0919.xls") 
          c-arq-ex             = SESSION:TEMP-DIRECTORY.
   
   RUN pi-cria-planilha.  
   
    REPEAT:
        IMPORT UNFORMATTED c-linha.

        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp (input "Gerando Excel ==> " + string(v-num-reg-lidos)).
        
        IF SUBSTRING (c-linha,1,5) =  "Grupo" THEN
            ASSIGN ge-codigo-jr = int (SUBSTRING(c-linha,16,2)).

        IF SUBSTRING (c-linha,1,16) <> "" THEN DO:

           FIND FIRST ITEM WHERE ITEM.it-codigo = SUBSTRING (c-linha,1,16)
                NO-LOCK NO-ERROR.

           IF AVAIL ITEM THEN
               ASSIGN it-codigo-jr = ITEM.it-codigo
                      desc-item-jr = ITEM.desc-item
                      un-jr        = string (SUBSTRING(c-linha,43,2))
                      obs-jr       = string (SUBSTRING(c-linha,45,2)).

        END.


        IF SUBSTRING (c-linha,67,7) =  "Itens -" THEN
            ASSIGN data-jr = string (SUBSTRING(c-linha,75,10))
                   c-relatorio:range("I" + string(2)):VALUE = data-jr.

        /*IF STRING (SUBSTRING(c-linha,51,3)) < "420" OR 
           STRING (SUBSTRING(c-linha,51,3)) > "429" THEN NEXT.
          */

        FIND estabelec WHERE estabelec.cod-estabel = SUBSTRING(c-linha,49,3) NO-LOCK NO-ERROR.

        /* Grava localiza‡Æo e Lote e informa‡äes do pallet */
        IF NOT AVAIL estabelec AND l-imp-info THEN DO:

            assign  dt-trans-jr = ?
                    c-nr-nf-trans = ""
                    c-estab-trans = "".
            
            for each fat-ser-lote WHERE
                fat-ser-lote.it-codigo       = it-codigo-jr   AND
                fat-ser-lote.log-disp-planej = NO                       AND
                fat-ser-lote.nr-serlote      = substring(c-linha,6,40) and
                fat-ser-lote.cod-estabel     <> c-cod-estabel
                USE-INDEX ITEM NO-LOCK ,
            
                each  nota-fiscal OF fat-ser-lote 
                    NO-LOCK ,
            
                each  natur-oper WHERE 
                       natur-oper.nat-operacao = nota-fiscal.nat-operacao
                       and natur-oper.transf = YES no-lock.
            
                      if dt-trans-jr = ? then assign dt-trans-jr = nota-fiscal.dt-emis-nota
                                                     c-nr-nf-trans = nota-fiscal.nr-nota-fis
                                                     c-estab-trans = nota-fiscal.cod-estabel.
            
                      if nota-fiscal.dt-emis-nota > dt-trans-jr then
                      assign dt-trans-jr = nota-fiscal.dt-emis-nota
                             c-nr-nf-trans = nota-fiscal.nr-nota-fis
                             c-estab-trans = nota-fiscal.cod-estabel.
            
            
            
            END.
            
            if dt-trans-jr <> ? then
            assign
                 c-relatorio:range("W" + string(i-linha)):VALUE = dt-trans-jr
                 c-relatorio:range("X" + string(i-linha)):VALUE = c-nr-nf-trans
                 c-relatorio:range("Y" + string(i-linha)):VALUE = c-estab-trans.




            ASSIGN l-imp-info    = NO
                   c-nome-abrev  = ""
                   c-nr-pedcli   = ""
                   dt-entrega-jr = ?
                   i-larg-p      = 0
                   i-diin-p      = 0
                   i-diex-p      = 0.

            FIND FIRST pallet 
                 WHERE pallet.cod-estabel = c-cod-estabel
                   AND pallet.nr-pallet   = substring(c-linha,6,40)
                   AND pallet.it-codigo   = it-codigo-jr NO-LOCK NO-ERROR.
            IF NOT AVAIL pallet THEN
                FIND FIRST pallet 
                     WHERE pallet.nr-pallet = substring(c-linha,6,40)
                       AND pallet.it-codigo = it-codigo-jr NO-LOCK NO-ERROR.

            IF AVAIL pallet THEN DO:
                FIND FIRST ped-venda 
                     WHERE ped-venda.nr-pedido = pallet.nr-pedido NO-LOCK NO-ERROR.
                IF AVAIL ped-venda THEN
                    ASSIGN c-nr-pedcli   = string(ped-venda.nr-pedido)
                           c-nome-abrev  = ped-venda.nome-abrev
                           dt-entrega-jr = ped-venda.dt-entrega.

                FIND first it-pallet OF pallet NO-LOCK NO-ERROR.
                IF AVAIL it-pallet THEN DO:

                    FIND FIRST lote-carac-tec 
                         WHERE lote-carac-tec.it-codigo = it-pallet.it-codigo
                           AND lote-carac-tec.lote      = it-pallet.lote-bobina
                           AND lote-carac-tec.cd-comp   = "largura" NO-LOCK NO-ERROR.
                    if avail lote-carac-tec AND INT(lote-carac-tec.vl-resul) <> 0 then
                        ASSIGN i-larg-p = INT (lote-carac-tec.vl-resul).

                    FIND FIRST lote-carac-tec 
                         WHERE lote-carac-tec.it-codigo = it-pallet.it-codigo
                           AND lote-carac-tec.lote      = it-pallet.lote-bobina
                           AND lote-carac-tec.cd-comp   = "diin" NO-LOCK NO-ERROR.
                    IF avail lote-carac-tec AND INT(lote-carac-tec.vl-resul) <> 0 THEN
                        ASSIGN i-diin-p = INT (lote-carac-tec.vl-resul).

                    FIND FIRST lote-carac-tec 
                         WHERE lote-carac-tec.it-codigo = it-pallet.it-codigo
                           AND lote-carac-tec.lote      = it-pallet.lote-bobina
                           AND lote-carac-tec.cd-comp   = "diex" NO-LOCK NO-ERROR.
                    if avail lote-carac-tec AND INT(lote-carac-tec.vl-resul) <> 0 THEN
                        ASSIGN i-diex-p = INT (lote-carac-tec.vl-resul).
    
                END. /* avail it-pallet */
            END. /* avail pallet */

            ASSIGN c-relatorio:range("I" + string(i-linha)):VALUE = substring(c-linha,6,40)
                   c-relatorio:range("M" + string(i-linha)):VALUE = i-larg-p
                   c-relatorio:range("N" + string(i-linha)):VALUE = i-diin-p
                   c-relatorio:range("O" + string(i-linha)):VALUE = i-diex-p
                   c-relatorio:range("P" + string(i-linha)):VALUE = c-nome-abrev
                   c-relatorio:range("Q" + string(i-linha)):VALUE = c-nr-pedcli.
        END.
            

        IF not AVAIL estabelec  THEN NEXT.

        ASSIGN i-larg   = 0
               i-diin   = 0
               i-diex   = 0
               .


        FIND FIRST var-result USE-INDEX id 
             WHERE var-result.item-cotacao  = it-codigo-jr   
               AND var-result.nr-estrut     = INT(substring(c-linha,59,8))
               AND var-result.nome-var      = "LARGURA"  NO-LOCK NO-ERROR.

        IF AVAIL VAR-result THEN
            ASSIGN i-larg = INT(var-result.des-result).

        FIND FIRST var-result USE-INDEX id 
             WHERE var-result.item-cotacao  = it-codigo-jr
               AND var-result.nr-estrut     = INT(substring(c-linha,59,8)) 
               AND var-result.nome-var      = "DIIN"  NO-LOCK NO-ERROR.
        IF AVAIL VAR-result THEN
            ASSIGN i-diin = INT(var-result.des-result).

        FIND FIRST var-result USE-INDEX id 
             WHERE var-result.item-cotacao  = it-codigo-jr
               AND var-result.nr-estrut     = INT(substring(c-linha,59,8))
               AND var-result.nome-var      = "DIEX"  NO-LOCK NO-ERROR.
        IF AVAIL VAR-result THEN
            ASSIGN i-diex = INT(var-result.des-result).


        ASSIGN c-cod-estabel = substring(c-linha,49,3).
 

        ASSIGN i-linha = i-linha + 1
               c-relatorio:range("A" + string(i-linha)):VALUE = substring(c-linha,49,3)
               c-relatorio:range("B" + string(i-linha)):VALUE = ge-codigo-jr
               c-relatorio:range("C" + string(i-linha)):VALUE = it-codigo-jr
               c-relatorio:range("D" + string(i-linha)):VALUE = desc-item-jr
               c-relatorio:range("E" + string(i-linha)):VALUE = un-jr
               c-relatorio:range("F" + string(i-linha)):VALUE = obs-jr
               c-relatorio:range("G" + string(i-linha)):VALUE = substring(c-linha,55,3)
               c-relatorio:range("H" + string(i-linha)):VALUE = substring(c-linha,59,8)
               c-relatorio:range("J" + string(i-linha)):VALUE = STRING (substring(c-linha,82,08))
               c-relatorio:range("K" + string(i-linha)):VALUE = substring(c-linha,113,10)
               c-relatorio:range("L" + string(i-linha)):VALUE = dec (substring(c-linha,93,19))
               c-relatorio:range("R" + string(i-linha)):VALUE = i-larg
               c-relatorio:range("S" + string(i-linha)):VALUE = i-diin
               c-relatorio:range("T" + string(i-linha)):VALUE = i-diex
               c-relatorio:range("U" + string(i-linha)):VALUE = IF AVAIL pallet THEN trim(substring(pallet.char-1,1,20)) ELSE ""
               c-relatorio:range("V" + string(i-linha)):VALUE = dt-entrega-jr.

            l-imp-info    = YES.
            
      

    END.  

   run pi-finalizar in h-acomp.

   RUN pi-finaliza-impressao.

END.

RETURN.


PROCEDURE pi-cria-planilha:

DEF VAR c-arquivo-jr AS CHAR FORMAT "x(50)" NO-UNDO.

    c-arquivo-jr = c-arq-ex + 'ce0919' + STRING(time)+ '.xls'. 
    OS-COPY VALUE(c-modelo-planilha) VALUE(c-arquivo-jr).
    assign c-planilha  = c-excel:Workbooks:OPEN(c-arquivo-jr)
           c-relatorio = c-excel:Sheets:item(1)
           c-arq-anexo = (IF c-arq-anexo <> '' THEN ',' ELSE '') + c-arquivo-jr.

END PROCEDURE.


PROCEDURE pi-finaliza-impressao:

DEF VAR i            AS INT  NO-UNDO.
DEF VAR c-arquivo-jr AS CHAR NO-UNDO.
 
    c-planilha:SAVE().
    c-planilha:CLOSE().

    c-excel:VISIBLE = true.

    DO i = 1 TO NUM-ENTRIES(c-arq-anexo):
        c-arquivo-jr = ENTRY(i,c-arq-anexo).
        c-planilha   = c-excel:Workbooks:OPEN(c-arquivo-jr).

    END.

    /*c-excel:QUIT().*/
    RELEASE OBJECT c-excel.

END PROCEDURE.




