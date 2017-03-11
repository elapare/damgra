/*******************************************************************************
* Empresa    : DLC TEC
* Cliente    : Unigel
* Programa   : escd0027rp.p
* Descricao  : L¢gica para Eliminar o Relacionamento Item Estabelecimento
* Autor      : Jos‚ Carlos de Almeida Marques
* Data       : 15/06/2015
* Versao     : 12.4
* Atualizacao: 
******************************************************************************/

{include/i-prgvrs.i escd0027RP 12.4}

define temp-table tt-param
    field destino          as integer
    field arquivo          as char
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field ini-cod-estabel  as char
    field fim-cod-estabel  as char
    FIELD ini-it-codigo    like item.it-codigo
    field fim-it-codigo    like item.it-codigo
    field ini-ge-codigo    like item.ge-codigo
    field fim-ge-codigo    like item.ge-codigo
    field ini-data-implant like item.data-implant
    field fim-data-implant like item.data-implant
    field opcao            AS INTEGER. 

define temp-table tt-digita
    field cod-estabel   like estabelec.cod-estabel format "x(5)"
    field cod-depos     like deposito.cod-depos    format "x(5)"
    index id is primary cod-estabel cod-depos.

def temp-table tt-raw-digita
   field raw-digita      as raw.

DEFINE TEMP-TABLE tt-item-estab NO-UNDO
    FIELD it-codigo   LIKE ITEM.it-codigo
    FIELD cod-estabel AS CHAR
    FIELD existe      AS LOG
    INDEX idx-tt-item-estab AS UNIQUE PRIMARY
          it-codigo
          cod-estabel.

DEFine BUFFER b-item-uni-estab FOR item-uni-estab.
DEFine BUFFER b-item-orig      FOR item-uni-estab.
DEFINE BUFFER b-tt-item-estab  FOR tt-item-estab.

/* Defini‡Æo dos Parƒmetros de Entrada */
define input parameter raw-param as raw no-undo.
define input parameter table for tt-raw-digita.

/* Passagem de parƒmetros */
create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

/* Defini‡Æo das Vari veis Locais */                                                    
define variable h-acomp            as HANDLE                   no-undo.
DEFINE VARIABLE c-diretorio        AS CHAR                     NO-UNDO.
DEFINE variable c-arquivo          AS CHAR                     NO-UNDO.
DEFINE variable c-sufixo           AS CHAR                     NO-UNDO.
DEFINE VARIABLE l-existe           AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE c-relacionamento   AS CHARACTER FORMAT "X(60)" NO-UNDO.
DEFINE VARIABLE chExcelApplication AS COM-HANDLE               NO-UNDO.

FIND FIRST tt-param
     NO-LOCK NO-ERROR.

IF AVAIL tt-param THEN
/*      MESSAGE "tt-param.destino          : " tt-param.destino           SKIP */
/*              "tt-param.arquivo          : " tt-param.arquivo           skip */
/*              "tt-param.usuario          : " tt-param.usuario           skip */
/*              "tt-param.data-exec        : " tt-param.data-exec         skip */
/*              "tt-param.hora-exec        : " tt-param.hora-exec         skip */
/*              "tt-param.classifica       : " tt-param.classifica        skip */
/*              "tt-param.ini-it-codigo    : " tt-param.ini-it-codigo     skip */
/*              "tt-param.fim-it-codigo    : " tt-param.fim-it-codigo     skip */
/*              "tt-param.ini-ge-codigo    : " tt-param.ini-ge-codigo     skip */
/*              "tt-param.fim-ge-codigo    : " tt-param.fim-ge-codigo     skip */
/*              "tt-param.ini-data-implant : " tt-param.ini-data-implant  skip */
/*              "tt-param.fim-data-implant : " tt-param.fim-data-implant  skip */
/*              "tt-param.opcao            : " tt-param.opcao             skip */
/*              VIEW-AS ALERT-BOX INFO BUTTONS OK.                             */

/* In¡cio Programa */

/* Executando de forma persistente o utilit rio de acompanhamento */
run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp (input "Elimina Relac Estab x Item sem Movimento").

ASSIGN c-arquivo = tt-param.arquivo.

OUTPUT TO value(c-arquivo).

    EMPTY TEMP-TABLE tt-item-estab.
    FOR EACH  ITEM NO-LOCK
        WHERE ITEM.it-codigo               >= tt-param.ini-it-codigo
        AND   ITEM.it-codigo               <= tt-param.fim-it-codigo
        AND   ITEM.ge-codigo               >= tt-param.ini-ge-codigo
        AND   ITEM.ge-codigo               <= tt-param.fim-ge-codigo
        AND   ITEM.data-implant            >= tt-param.ini-data-implant 
        AND   ITEM.data-implant            <= tt-param.fim-data-implant,
        EACH  b-item-uni-estab NO-LOCK    
        WHERE b-item-uni-estab.it-codigo    = ITEM.it-codigo
        and   b-item-uni-estab.cod-estabel >= tt-param.ini-cod-estabel
        and   b-item-uni-estab.cod-estabel <= tt-param.fim-cod-estabel
/*         AND   b-item-uni-estab.cod-estabel = "433"  */
        :

        FIND tt-item-estab
             WHERE tt-item-estab.it-codigo   = b-item-uni-estab.it-codigo
             AND   tt-item-estab.cod-estabel = b-item-uni-estab.cod-estabel
             EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL tt-item-estab THEN DO:

             CREATE tt-item-estab.
             ASSIGN tt-item-estab.it-codigo   = b-item-uni-estab.it-codigo   
                    tt-item-estab.cod-estabel = b-item-uni-estab.cod-estabel. 

        END. /* IF NOT AVAIL tt-item-estab THEN DO: */
            
        run pi-acompanhar in h-acomp (INPUT "Item/Estab: " + b-item-uni-estab.it-codigo + "/" + b-item-uni-estab.cod-estabel).        
    
        run pi-mov-estab (input  b-item-uni-estab.it-codigo,
                          input  b-item-uni-estab.cod-estabel,
                          output l-existe,
                          OUTPUT c-relacionamento).

        ASSIGN tt-item-estab.existe = l-existe.
    
        if not(l-existe) then do:
    
           EXPORT DELIMITER ";"
                  b-item-uni-estab.it-codigo  
                  b-item-uni-estab.cod-estabel
                  "Item sem relacionamento" 
                  SKIP.
    
           IF tt-param.opcao = 2 THEN DO: /* 2 = "Elimina" */

               find FIRST item-estab
                    where item-estab.it-codigo   = b-item-uni-estab.it-codigo 
                    AND   item-estab.cod-estabel = b-item-uni-estab.cod-estabel
                    EXCLUSIVE-LOCK no-error.
               if available item-estab then
                    delete item-estab.
        
               find FIRST item-uni-estab 
                    where item-uni-estab.it-codigo   = b-item-uni-estab.it-codigo   
                    and   item-uni-estab.cod-estabel = b-item-uni-estab.cod-estabel 
                    EXCLUSIVE-LOCK no-error.
               if available item-uni-estab then
                    DELETE item-uni-estab.

           END. /* IF tt-param.opcao = 2 THEN DO: /* 2 = "Elimina" */ */

        end. /* if not(l-existe) then do: */
        ELSE DO:

            EXPORT DELIMITER ";"
                   b-item-uni-estab.it-codigo  
                   b-item-uni-estab.cod-estabel
                   c-relacionamento 
                   SKIP.

        END. /* ELSE DO: if not(l-existe) then do: */
    
    end. /* FOR EACH  ITEM NO-LOCK */

OUTPUT CLOSE.

/* L¢gica para tratar a abertura e a impressÆo do arquivo */
CREATE "Excel.Application" chExcelApplication.
chExcelApplication:Visible = NO.
chExcelApplication:Workbooks:open(c-arquivo).
    
chExcelApplication:cells:COLUMNS:AutoFit.

IF tt-param.destino = 1 THEN  /* 1 = Impressora, 2 = Arquivo e 3 = Terminal*/
   chExcelApplication:ActiveSheet:printout NO-ERROR.
ELSE DO:

   chExcelApplication:CutCopyMode = False. /* Desabilita a C¢pia */
   chExcelApplication:VISIBLE     = (IF tt-param.destino = 3 THEN TRUE ELSE FALSE).  /* Habilita para visualizar o arquivo gerado */
   chExcelApplication:ActiveWorkbook:SaveAs(c-arquivo,,,,,,). /* Salva Arquivo gerado */

   IF tt-param.destino = 2 THEN
      chExcelApplication:ActiveWorkbook:CLOSE().

END. /* ELSE DO: */

IF VALID-HANDLE(chExcelApplication) THEN
   RELEASE OBJECT chExcelApplication NO-ERROR.
/* Final - L¢gica para tratar a abertura e a impressÆo do arquivo */

RUN pi-finalizar in h-acomp.

return "OK":U.
/* Final Programa */

/* Defini‡Æo das Procedures */
procedure pi-mov-estab.
    def input  param p-it-codigo   as char no-undo.
    def input  param p-cod-estabel as char no-undo.
    def output param l-existe      as log  no-undo.
    def output param p-relac       as CHAR no-undo.

    ASSIGN p-relac  = ""
           l-existe = NO.

    bc-mov-estab:
    DO TRANS:

       FOR EACH  ped-item NO-LOCK
           WHERE ped-item.it-codigo    = p-it-codigo,
           FIRST ped-venda OF ped-item NO-LOCK
           WHERE ped-venda.cod-estabel = p-cod-estabel:

           run pi-acompanhar in h-acomp (INPUT "ped-item Item/Estab: " + p-it-codigo + "/" + p-cod-estabel).

           ASSIGN p-relac  = "ped-item"
                  l-existe = YES.

           LEAVE bc-mov-estab.

        END. /* FOR EACH ped-item NO-LOCK */
    
        FOR EACH  item-contrat NO-LOCK
            WHERE item-contrat.it-codigo   = p-it-codigo,
            FIRST contrato-for OF item-contrat NO-LOCK
            WHERE contrato-for.cod-estabel = p-cod-estabel:

            run pi-acompanhar in h-acomp (INPUT "item-contrat Item/Estab: " + p-it-codigo + "/" + p-cod-estabel).   
    
            ASSIGN p-relac  = "item-contrat"
                   l-existe = YES.
    
            LEAVE bc-mov-estab.
    
        END. /* FOR EACH item-contrat NO-LOCK */        
    
        IF CAN-FIND(FIRST it-requisicao
                    WHERE it-requisicao.it-codigo   = p-it-codigo
                      AND it-requisicao.cod-estabel = p-cod-estabel) THEN DO:
    
           run pi-acompanhar in h-acomp (INPUT "it-requisicao Item/Estab: " + p-it-codigo + "/" + p-cod-estabel).

           ASSIGN p-relac  = "it-requisicao"
                  l-existe = YES.

           LEAVE bc-mov-estab.
    
        END. /* IF CAN-FIND(FIRST it-requisicao */
    
        /* Caso nÆo tenha achado mais de um ele desconsiderra */
        IF NOT CAN-FIND(FIRST b-item-orig
                        WHERE b-item-orig.it-codigo    = p-it-codigo
                          AND b-item-orig.cod-estabel <> p-cod-estabel ) THEN DO:

           run pi-acompanhar in h-acomp (INPUT "apenas um estab Item/Estab: " + p-it-codigo + "/" + p-cod-estabel). 
    
           ASSIGN p-relac  = "apenas um estab"
                  l-existe = YES.

           LEAVE bc-mov-estab.
    
        END. /* IF NOT CAN-FIND(FIRST b-item-orig */
    
        if can-find (first movto-estoq
                     where movto-estoq.it-codigo   = p-it-codigo
                       and movto-estoq.cod-estabel = p-cod-estabel) THEN DO:

            run pi-acompanhar in h-acomp (INPUT "movto-estoq Item/Estab: " + p-it-codigo + "/" + p-cod-estabel).

            ASSIGN p-relac  = "movto-estoq"
                   l-existe = YES.

            LEAVE bc-mov-estab.

        END. /* if can-find (first movto-estoq */
    
        IF can-find (first ordem-compra 
                     where ordem-compra.it-codigo   = p-it-codigo 
                       and ordem-compra.cod-estabel = p-cod-estabel) THEN DO:

            run pi-acompanhar in h-acomp (INPUT "ordem-compra Item/Estab: " + p-it-codigo + "/" + p-cod-estabel).
    
            ASSIGN p-relac  = "ordem-compra"
                   l-existe = YES.

            LEAVE bc-mov-estab.
    
        END. /* IF can-find (first ordem-compra */
    
        IF can-find (first ord-prod 
                     where ord-prod.it-codigo   = p-it-codigo 
                       and ord-prod.cod-estabel = p-cod-estabel) THEN DO:

            run pi-acompanhar in h-acomp (INPUT "ord-prod Item/Estab: " + p-it-codigo + "/" + p-cod-estabel).                     
    
            ASSIGN p-relac  = "ord-prod"
                   l-existe = YES.

            LEAVE bc-mov-estab.
    
        END. /* IF can-find (first ord-prod  */
    
        IF can-find (first item-fornec-estab 
                     where item-fornec-estab.it-codigo   = p-it-codigo 
                       and item-fornec-estab.cod-estabel = p-cod-estabel) THEN DO:

            run pi-acompanhar in h-acomp (INPUT "item-fornec-estab Item/Estab: " + p-it-codigo + "/" + p-cod-estabel). 
    
            ASSIGN p-relac  = "item-fornec-estab"
                   l-existe = YES.

            LEAVE bc-mov-estab.
    
        END. /* IF can-find (first item-fornec-estab */
    
        IF can-find (first consumo-estab
                     where consumo-estab.it-codigo        = p-it-codigo
                       and consumo-estab.cod-estabel-prin = p-cod-estabel) THEN DO:

            run pi-acompanhar in h-acomp (INPUT "consumo-estab Item/Estab: " + p-it-codigo + "/" + p-cod-estabel).

            ASSIGN p-relac  = "consumo-estab"
                   l-existe = YES.

            LEAVE bc-mov-estab.

        END. /* IF can-find (first consumo-estab */

        IF can-find (first item-estab
                     where item-estab.it-codigo   = p-it-codigo
                       and item-estab.cod-estabel = p-cod-estabel) THEN DO:

            run pi-acompanhar in h-acomp (INPUT "item-estab Item/Estab: " + p-it-codigo + "/" + p-cod-estabel).

            ASSIGN p-relac  = "item-estab"
                   l-existe = YES.

            LEAVE bc-mov-estab.

        END. /* IF can-find (first item-estab */

        IF can-find (first saldo-estoq
                     where saldo-estoq.it-codigo   = p-it-codigo
                       and saldo-estoq.cod-estabel = p-cod-estabel) THEN DO:

            run pi-acompanhar in h-acomp (INPUT "saldo-estoq Item/Estab: " + p-it-codigo + "/" + p-cod-estabel).

            ASSIGN p-relac  = "saldo-estoq"
                   l-existe = YES.

            LEAVE bc-mov-estab.

        END. /* IF can-find (first saldo-estoq */

        IF can-find (first it-nota-fisc use-index ch-item-data
                     where it-nota-fisc.cod-estabel = p-cod-estabel
                       and it-nota-fisc.it-codigo   = p-it-codigo) THEN DO:

            run pi-acompanhar in h-acomp (INPUT "it-nota-fisc  Item/Estab: " + p-it-codigo + "/" + p-cod-estabel).

            ASSIGN p-relac  = "it-nota-fisc"
                   l-existe = YES.

            LEAVE bc-mov-estab.

        END. /* IF can-find (first it-nota-fisc use-index ch-item-data  */

        for each recebimento where
                 recebimento.it-codigo = p-it-codigo no-lock:
            if can-find (first ordem-compra where
                               ordem-compra.numero-ordem = recebimento.numero-ordem and
                               ordem-compra.cod-estabel  = p-cod-estabel) then do:

                run pi-acompanhar in h-acomp (INPUT "recebimento - ordem-compra  Item/Estab: " + p-it-codigo + "/" + p-cod-estabel).

                ASSIGN p-relac  = "recebimento - ordem-compra"
                       l-existe = YES.

                LEAVE bc-mov-estab.

            end.
        end. /* for each recebimento where */

        for each pl-prod where pl-prod.cod-estabel = p-cod-estabel no-lock:
            if  can-find (first pl-it-prod use-index ch-item where
                                pl-it-prod.it-codigo = item.it-codigo and
                                pl-it-prod.cd-plano  = pl-prod.cd-plano AND
                                pl-it-prod.cod-estabel = p-cod-estabel) then do:

                run pi-acompanhar in h-acomp (INPUT "pl-it-prod  Item/Estab: " + p-it-codigo + "/" + p-cod-estabel).

                ASSIGN p-relac  = "pl-it-prod"
                       l-existe = YES.

                LEAVE bc-mov-estab.

            end.
        end. /* for each pl-prod where pl-prod.cod-estabel = p-cod-estabel no-lock: */
    
        /* Unigel Comercial */
        FIND FIRST if-estabelec
             WHERE if-estabelec.cod-estab-dest = p-cod-estabel 
             NO-LOCK NO-ERROR.
        IF AVAIL if-estabelec THEN DO:

             run pi-acompanhar in h-acomp (INPUT "if-estabelec Item/Estab: " + p-it-codigo + "/" + p-cod-estabel).

             /* Verifica se o estabelecimento de origem possui relacionamento com o Item */
             FIND b-tt-item-estab
                  WHERE b-tt-item-estab.it-codigo   = p-it-codigo                
                  AND   b-tt-item-estab.cod-estabel = if-estabelec.cod-estab-orig
                  NO-LOCK NO-ERROR.
             IF AVAIL b-tt-item-estab THEN DO:       
               
                  IF b-tt-item-estab.existe THEN DO:

                     ASSIGN p-relac  = "item orig UNC"
                            l-existe = YES.
                   
                     LEAVE bc-mov-estab.

                  END. /* IF b-tt-item-estab.existe THEN DO: */

             END. /* IF AVAIL b-tt-item-estab THEN DO: */
             ELSE DO:

                  run pi-mov-estab (input  p-it-codigo,                
                                    input  if-estabelec.cod-estab-orig,
                                    output l-existe,
                                    OUTPUT p-relac).
               
             END. /* ELSE DO: IF AVAIL b-tt-item-estab THEN DO: */

        END. /* IF AVAIL if-estabelec THEN DO: */

    END. /* DO TRANS: */
    
end procedure. /* procedure pi-mov-estab. */
