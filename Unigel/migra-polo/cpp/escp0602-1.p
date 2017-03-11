
/*------------------------------------------------------------------------
File.............: escp0602-1.p
Description......: Gera a configura‡Æo e ordens de produ‡Æo do Otimizador 
Input Parameters : 
Output Parameters: 
Author...........: Amgra - Edson / Jos‚ Roberto.
Created..........: 27/04/2011   
OBS..............: 
------------------------------------------------------------------------*/
define buffer empresa for mgmulti.empresa.
def new global shared var i-ep-codigo-usuario  like empresa.ep-codigo no-undo.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.  
DEFINE VARIABLE i-cont-erro AS INTEGER    NO-UNDO.

DEFINE TEMP-TABLE tt-var-form NO-UNDO                                     
       FIELD nome-var    LIKE var-modelo.nome-var   
       FIELD sequencia   LIKE var-modelo.sequencia  
       FIELD tipo-result LIKE var-modelo.tipo-result
       FIELD formula     LIKE var-mod-form.formula. 

/******************************************************************************
 ** 
 **  INCLUDE  : CDAPI020A.I 
 **
 **  OBJETIVO : Definir a temp-table da API do Interpretador de F¢rmulas
 **
 ******************************************************************************/
 
 def temp-table tt-formula no-undo
     field formula               as char
     field result                as decimal format ">>>>,>>9.9999"
     field texto                 as char    format "x(256)"
     field tipo-res              as integer format "9" init 1 /* 1-Decimal, 2-Caracter */
     field cod-versao-integracao as integer format "999"
     field sequencia             as integer init 1
     index codigo is primary unique sequencia.
      
 def temp-table tt-var no-undo         
     field nome              as character         
     field valor             as decimal format ">>>,>>>,>>9.999999999"         
     field texto             as char    format "x(60)"         
     field nr-tabela         as integer format ">>9"         
     field tipo-res          as integer format "9"         
     field abrev-nar         as char    format "x(29)"
     field sequencia         as integer init 1
     index nome is primary nome sequencia. 


/* componente cont‚m c¢digo do Item / Fam¡lia / Sub-modelo               */
/* tipo       cont‚m o tipo de componente I / F / M                      */
/* it-compon  ‚ preenchido para os Itens que fazem parte de uma fam¡lia  */
/* compon-familia ‚ yes na chamada do programa de itens de uma fam¡lia   */

/* Vari veis / Constantes utilizadas na configura‡Æo - cf0201b.w */
def temp-table tt-variavel no-undo
    field seq-mod        as   integer /*guarda a seq do modelo corrente*/                 
    FIELD seq-pai        AS   INTEGER /*guarda a seq do modelo pai*/
    field seq-var        as   integer                  
    field c-config       as   character format "x(1)"
    field nivel          as integer
    FIELD l-primeiro     AS   LOGICAL INITIAL NO
    field mo-codigo      like var-result.mo-codigo
    field nome-var       like var-result.nome-var
    field nome-cte       like var-result.nome-cte
    field sequencia-res  like var-result.sequencia
    field sequencia-var  like var-modelo.sequencia
    field descricao      like var-result.descricao
    field tipo-valor     like var-result.tipo-valor
    field valor-char     like var-result.valor-char
    field valor-dec      like var-result.valor-dec
    field tipo-result    like var-modelo.tipo-result 
    field c-tipo-result  like var-modelo.tipo-result 
    field abreviatura    like var-modelo.abreviatura
    field gera-narrativa like var-modelo.gera-narrativa
    field ind-tipo-var   like var-modelo.ind-tipo-var
    field nr-tabela      like var-modelo.nr-tabela
    field endereco       as rowid
    index codigo is primary unique seq-var
    index nivel nivel
    index modelo mo-codigo nivel seq-mod tipo-result
    index nome nome-var nivel mo-codigo seq-var.

DEFINE TEMP-TABLE tt-abre-ordem no-undo
    FIELD fase                 AS INT     FORMAT ">>>>"  
    FIELD nr-linha             AS INT     FORMAT ">>>9"
    FIELD it-codigo            AS CHAR    FORMAT "x(16)"
    FIELD nome-abrev           AS CHAR    FORMAT "x(16)"
    FIELD nr-pedido            AS INT     FORMAT ">>>>>>>9"       
    FIELD seq-ped              AS INT     FORMAT ">>>>"       
    FIELD cod-refer            AS CHAR    FORMAT "x(8)"
    FIELD nr-estrut            AS INT     FORMAT ">>>>>>>9"       
    FIELD diin                 AS INT     FORMAT ">>>>"       
    FIELD diex                 AS INT     FORMAT ">>>>"       
    FIELD qt-bobinas           AS INT     FORMAT ">>>>>>9"    
    FIELD larg                 AS INT     FORMAT ">>>>"       
    FIELD peso-kg              AS DEC     FORMAT ">>>>>9.999" 
    FIELD dt-inicio            AS DATE    FORMAT "99/99/9999"
    FIELD dt-fim               AS DATE    FORMAT "99/99/9999"
    FIELD it-codigo-res        AS CHAR    FORMAT "x(16)"
    FIELD cod-refer-res-1      AS CHAR    FORMAT "x(8)"
    FIELD cod-refer-res-2      AS CHAR    FORMAT "x(8)"
    FIELD peso-kg-res          AS DEC     FORMAT ">>>>>9.999" 
    FIELD nr-ord-produ         AS INT     FORMAT ">>>>>>>9"
    INDEX chave IS PRIMARY UNIQUE fase
                                  it-codigo
                                  larg
                                  diin
                                  diex
                                  nr-pedido  
                                  seq-ped.   

def input-output parameter table for tt-abre-ordem.


/*
CREATE tt-abre-ordem.
ASSIGN
    tt-abre-ordem.fase            = 1  
    tt-abre-ordem.nr-linha        = 400  
    tt-abre-ordem.it-codigo       = "20tsy32"
    tt-abre-ordem.nome-abrev      = "" 
    tt-abre-ordem.nr-pedido       = 0  
    tt-abre-ordem.seq-ped         = 0  
    tt-abre-ordem.cod-refer       = ""  
    tt-abre-ordem.nr-estrut       = 0  
    tt-abre-ordem.diin            = 153  
    tt-abre-ordem.diex            = 700  
    tt-abre-ordem.qt-bobinas      =  3 
    tt-abre-ordem.larg            = 800  
    tt-abre-ordem.peso-kg         = 600  
    tt-abre-ordem.dt-inicio       = TODAY  
    tt-abre-ordem.dt-fim          = TODAY + 15  
    tt-abre-ordem.it-codigo-res   = "20tsy32mr"
    tt-abre-ordem.cod-refer-res-o = ""  
    tt-abre-ordem.cod-refer-res-e = ""  
    tt-abre-ordem.peso-kg-res     = 600  
    tt-abre-ordem.nr-ord-produ    = 0.
    
*/    

    /*Cria‡Æo OP para pedido*/

    DEFINE VARIABLE amg-nr-pedcli AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE tipo-ped AS INTEGER  INITIAL 1  NO-UNDO.
    def var iSeq         as int   no-undo.
    DEFINE VARIABLE i-sequencia AS INTEGER    NO-UNDO.
    define variable c-seq-alter      as integer.
    DEFINE VARIABLE de-proporcao     AS DEC     NO-UNDO INITIAL 0.
    define variable r-ped-item       as rowid   no-undo. 
    define variable r-ped-ent        as rowid   no-undo.
    DEFINE VARIABLE hDBOCotEstMast AS HANDLE NO-UNDO.

DEFINE VARIABLE l-cria-ordem-pedido AS LOGICAL    NO-UNDO.
    /*********************************************************
     ** CD0530.i3 - Defini‡Æo TT-COMPONENTE
     *********************************************************/
    def temp-table tt-componente no-undo
        field seleciona      as logical format "*/ "
        field seq-pai-tt     as integer
        field r-cot-estrut   as rowid
        field pai-dd         as char format "x(1)" 
        field nr-estrutura  like cot-estrut.nr-estrut
        field it-codigo      like item.it-codigo 
        field descricao      as char format "x(36)"
        field nivel          as integer format "99"
        field un             like item.un
        field res-cq-fabr    like item.res-cq-fabr
        field ressup-fabr    like item.ressup-fabr
        field res-cq-acum    like item.res-cq-fabr
        field ressup-acum    like item.ressup-fabr
        field quant-ordem    like ord-prod.qt-produzida
        field nr-ordem       like ord-prod.nr-ord-prod
        field ind-aprov      as char format "x(20)"
        field sequencia      as integer
        field dt-entrega     like ped-ent.dt-entrega
        FIELD nr-entrega     LIKE ped-ent.nr-entrega . /* edson mudan‡a tt-componente EMS-padrao */
    /* FIM - CD0530.I3 */
      /*tt-componente*/
    def  temp-table tt-erro no-undo
        field i-sequen as int             
        field cd-erro  as int
        field mensagem as char format "x(255)".

    define temp-table tt-rowid-ped-ent
         field it-codigo      like item.it-codigo 
         field seq-componente as integer
         field seq-ped-ent    as integer
         field r-ped-ent      as rowid.

    /*********************************************************************************
 ** Defini‡Æo tt-digita
 *********************************************************************************/
 
 def temp-table tt-digita no-undo
     field nome-abrev    like ped-item.nome-abrev
     field nr-pedcli     like ped-item.nr-pedcli   
     field nr-config     like ped-item.nr-config
     field it-codigo     like ped-item.it-codigo
     field qt-pedida     like ped-item.qt-pedida
     field nr-ordem      as integer
     field obs           as char format "x(30)"
     field msgs          as char format "x(2000)"
     field ordem-destino as integer.

 def temp-table tt-ord-prod      NO-UNDO like ord-prod use-index codigo 
    field dt-disponibilidade    as date
    field ind-tipo-movto        as integer
    field faixa-numeracao       as integer init 1
    field verifica-compras      as logical 
    field aloca-reserva         as logical init ?
    field aloca-lote            as logical init ?
    field rw-ord-prod           as rowid
    field gera-relacionamentos  as logical init yes
    
    field gera-reservas         as logical init yes
    
    field prog-seg              as char
    field seg-usuario           as char
    field ep-codigo-usuario     as char
    field cod-versao-integracao as integer format "999"
    field considera-dias-desl   as logical init no.

def temp-table tt-reapro    NO-UNDO
    field it-codigo         like ord-prod.it-codigo
    field cod-refer         like ord-prod.cod-refer
    field descricao         as char format "x(36)"
    field un                like reservas.un
    field quant-orig        like reservas.quant-orig.
 



    DEFINE BUFFER b-tt-erro FOR tt-erro.

    DEFINE BUFFER b-ped-venda FOR ped-venda.

        def temp-table tt-componente-aux no-undo
    field seleciona      as logical format "*/ "
    field seq-pai-tt     as integer
    field r-cot-estrut   as rowid
    field pai-dd         as char format "x(1)" 
    field nr-estrutura like cot-estrut.nr-estrut
    field it-codigo    like item.it-codigo 
    field descricao      as char format "x(36)"
    field nivel          as integer format "99"
    field un           like item.un
    field res-cq-fabr  like item.res-cq-fabr
    field ressup-fabr  like item.ressup-fabr
    field res-cq-acum  like item.res-cq-fabr
    field ressup-acum  like item.ressup-fabr
    field quant-ordem  like ord-prod.qt-produzida
    field nr-ordem     like ord-prod.nr-ord-prod
    field ind-aprov    as char format "x(20)"
    field sequencia      as integer
    field dt-entrega   like ped-ent.dt-entrega.

define buffer b-tt-componente-aux for tt-componente-aux.
define buffer b2-tt-componente-aux for tt-componente-aux.
define buffer b3-tt-componente-aux for tt-componente-aux.


    def temp-table ttErroAux NO-UNDO like tt-erro.
        def   var  h-acomp as handle no-undo.
        /*Cria‡Æo OP para estoque */
        DEF VAR i-nr-estrut AS INT NO-UNDO.
        DEF VAR i-nr-estrut-criada AS INT NO-UNDO.
        DEF VAR i-config-similar AS INT no-undo.

        def temp-table ttCotEstMastPesquisa like cot-est-mast
            field nr-pedcli    like ped-item.nr-pedcli
            field nr-sequencia like ped-item.nr-sequencia
            field nome-abrev   like ped-item.nome-abrev 
            index id is primary unique item-cotacao nr-estrut nr-pedcli nr-sequencia nome-abrev.

        def temp-table ttVarModelo
            field sequencia like var-modelo.sequencia
            field nome-var  like var-modelo.nome-var
            field tipo      like var-modelo.tipo-result
            field nr-tabela like var-modelo.nr-tabela
            field procura    as log
            field resultChar as char format "x(60)"
            field resultDec  as dec  format "->,>>>,>>9.9999"
            FIELD tipo-var   AS INT 
            index id is primary unique sequencia nome-var.
        /*******/

if not valid-handle(h-acomp) then
   RUN cpp/escp0602-acomp.p PERSISTENT SET h-acomp.
   
   RUN pi-inicializar IN h-acomp (INPUT "Gera‡Æo Ordens de Produ‡Æo").
   
    find first param-cp no-lock no-error.

    FOR EACH ttErroAux:
       DELETE ttErroAux.
    END.

i-cont-erro = 0.

   blk-do:
   do transaction on error undo blk-do, return error:
   
   
     
       FOR EACH tt-abre-ordem. 
        
           
              
              
              
              
              
            RUN piCriaOPPedCampanha.
            
            if valid-handle(h-acomp) then
                run pi-acompanhar in h-acomp (input "item : " + tt-abre-ordem.it-codigo + " - " + " Ordem:" + STRING(tt-abre-ordem.nr-ord-produ)). 
  
         
                IF  RETURN-VALUE = "NOK":U THEN
                   undo blk-do, return error.
                
            
      END.
        
                       
                
   end.
   
   
if valid-handle(h-acomp) then
   run pi-finalizar in h-acomp.
   
if can-find(first ttErroAux) then do:
   run cdp/cd0666.w (input table ttErroAux).
end.

RETURN "OK":U.



PROCEDURE piCriaOPPedCampanha:
   
    
         FIND FIRST ped-venda WHERE ped-venda.nr-pedido = tt-abre-ordem.nr-pedido NO-LOCK NO-ERROR.
         IF AVAIL ped-venda THEN
             DO:
             
             
                          

                    FIND FIRST ped-item WHERE
                         ped-item.nome-abrev   = tt-abre-ordem.nome-abrev AND
                         ped-item.nr-pedcli    = ped-venda.nr-pedcli AND
                         ped-item.nr-sequencia = tt-abre-ordem.seq-ped AND
                         ped-item.it-codigo    = tt-abre-ordem.it-codigo AND
                         ped-item.cod-refer    = tt-abre-ordem.cod-refer 
                         NO-LOCK NO-ERROR.
                         
                        
                          
                         
                    IF AVAIL ped-item THEN DO:
                        find first cot-est-mast where 
                                   cot-est-mast.nr-estrut    = ped-item.nr-config AND
                                   cot-est-mast.item-cotacao = ped-item.it-codigo no-lock no-error.   
                                   
                                   
                        IF AVAIL cot-est-mast THEN DO:
                           RUN pi-cria-op-pedido (ROWID(cot-est-mast),
                                                  tt-abre-ordem.peso-kg,
                                                  ped-item.nome-abrev,
                                                  ped-item.nr-pedcli,
                                                  ped-item.nr-sequencia).
                        END.
                           
                    END.
                
            END.
            ELSE IF tt-abre-ordem.nr-pedido = 0  THEN DO: /*Pedido para Estoque */
                /*Verificar se todas as tt-abre-ordem tem programa de corte efetivado*/
               
                /*Gera‡Æo automatica configura‡Æo para Ponta utilizando c¢pia de configura‡Æo similar*/
                RUN pi-busca-config (INPUT  tt-abre-ordem.it-codigo,
                                     INPUT  tt-abre-ordem.larg,   
                                     INPUT  tt-abre-ordem.diin,        
                                     INPUT  tt-abre-ordem.diex,
                                     INPUT  tt-abre-ordem.qt-bobinas,
                                     OUTPUT i-config-similar).
                /*cria nova configura‡Æo*/
                IF i-config-similar <> 0 THEN DO:
                    /*criar  nova configura‡Æo atrav‚s da c¢pia da configura‡Æo similar encontrada*/
                    
                   run pi-acompanhar in h-acomp (input "Config. item : " + tt-abre-ordem.it-codigo + " - " + " Fase:" + STRING(tt-abre-ordem.fase)).
                     
                    RUN sfc/essf0011fc.p(input tt-abre-ordem.it-codigo,
                                         INPUT tt-abre-ordem.qt-bobinas,
                                         INPUT tt-abre-ordem.peso-kg,
                                         input i-config-similar,
                                         OUTPUT i-nr-estrut-criada,
                                         output table tt-erro).
                    
                        for each al-res-op.
                           delete al-res-op.
                        end.
                        
                    IF CAN-FIND(FIRST tt-erro) THEN DO: /*Quando api f¢rmula retorna erro*/
                        ASSIGN l-cria-ordem-pedido = NO.
                        FOR EACH tt-erro:
                            ASSIGN i-sequencia = 1.
        
                            FOR EACH ttErroAux:
                                IF  ttErroAux.i-sequen > i-sequencia THEN
                                    ASSIGN i-sequencia = ttErroAux.i-sequen + 1.
                            END.
                          
                            create ttErroAux.
                            assign ttErroAux.i-sequen = i-sequencia
                                   ttErroAux.cd-erro  = tt-erro.cd-erro
                                   ttErroAux.mensagem = tt-erro.mensagem.
                           return "NOK":U.
                        END.
                    END.
                    
                    IF i-nr-estrut-criada <> 0 THEN DO:
                       
                        RUN pi-ajusta-variaveis (INPUT  tt-abre-ordem.it-codigo,
                                     INPUT  tt-abre-ordem.larg,   
                                     INPUT  tt-abre-ordem.diin,        
                                     INPUT  tt-abre-ordem.diex,
                                     INPUT  tt-abre-ordem.qt-bobinas,
                                     INPUT  i-nr-estrut-criada,
                                     INPUT  tt-abre-ordem.peso-kg).
                    
                       ASSIGN i-nr-estrut = i-nr-estrut-criada
                              l-cria-ordem-pedido = YES
                              tt-abre-ordem.cod-refer = string(i-nr-estrut-criada,"99999999").
                    END.

                    ELSE ASSIGN l-cria-ordem-pedido = NO.
                END.
                ELSE DO: /*NÆo encontrou configura‡Æo similar*/
                
                       if valid-handle(h-acomp) then
                            run pi-finalizar in h-acomp.
   
                     RUN utp/ut-msgs.p (INPUT "show",
                                        INPUT 17006,
                                        INPUT " Gera‡Æo da Ordem para Estoque nÆo permitida."
                                        + "~~" 
                                        + "NÆo encontrada configura‡Æo para largura " +
                                        STRING(tt-abre-ordem.larg) +
                                        " diƒmetro interno " +
                                        STRING(tt-abre-ordem.diin) +
                                        " diƒmetro externo " +
                                        STRING(tt-abre-ordem.diex) +
                                        " linha " +
                                        STRING(tt-abre-ordem.nr-linha) + 
                                        " Item " +
                                        STRING(tt-abre-ordem.it-codigo) +
                                        ".").
                     ASSIGN l-cria-ordem-pedido = NO.
                     return "NOK":U.
                END.
                /**/
                IF l-cria-ordem-pedido THEN DO:
                    /*Encontrando configura‡Æo para a Ponta inclu¡da gerar a ordem para estoque*/
                    find first cot-est-mast where 
                               cot-est-mast.nr-estrut    = i-nr-estrut AND
                               cot-est-mast.item-cotacao = tt-abre-ordem.it-codigo no-lock no-error.   
                    IF AVAIL cot-est-mast THEN DO:
                       RUN pi-cria-op-estoque (ROWID(cot-est-mast),
                                               tt-abre-ordem.peso-kg,
                                               "",
                                               "",
                                               0).
                    END.
                    
                END.
            END.

         FIND FIRST tt-componente NO-LOCK NO-ERROR.

         IF AVAIL tt-componente then
           ASSIGN tt-abre-ordem.nr-ord-produ = tt-componente.nr-ordem.

        RUN pi-altera-linha-op.
    
    return "OK":U.

end procedure.



PROCEDURE pi-cria-op-pedido:
 def input  param rCotEstMast as rowid                         no-undo. 
 def input  param deQtOrdem   as decimal   format ">>>,>>9.99" no-undo.
 def input  param cNomeAbrev  as character                     no-undo.
 def input  param cNrPedido   as character                     no-undo.
 def input  param iSequencia  as integer                       no-undo.

    /*--- Verifica se o DBO j  est  inicializado ---*/
    
    
    IF NOT VALID-HANDLE(hDBOCotEstMast) THEN DO:
        run mfbo/bomf067op.p persistent set hDBOCotEstMast.
    END.

    RUN openQueryStatic IN hDBOCotEstMast (INPUT "Main":U) NO-ERROR.

    for each tt-componente:
      delete tt-componente.
    end.

    run criaTTComponentePai /*in hDBOCotEstMast*/ (input        rCotEstMast,
                                               input        deQtOrdem,
                                               input        cNomeAbrev,
                                               input        cNrPedido,
                                               input        iSequencia,
                                               input-output iSeq,
                                               input-output table tt-componente).

    
    FOR EACH tt-componente:
    
     
    
       ASSIGN tt-componente.seleciona = YES.
    END.

    if return-value = "OK":U then do:
       ASSIGN c-seq-alter = 0
              amg-nr-pedcli = "".

       FIND FIRST b-ped-venda WHERE b-ped-venda.nr-pedido = tt-abre-ordem.nr-pedido NO-LOCK NO-ERROR.

       IF AVAIL b-ped-venda THEN
           amg-nr-pedcli = b-ped-venda.nr-pedcli.

       FOR EACH tt-componente WHERE tt-componente.seleciona = YES, 
       EACH ped-ent FIELDS (nome-abrev  nr-pedcli  it-codigo    nr-sequencia 
                            dt-entrega  cod-refer  nr-programa  qt-pedida) NO-LOCK 
       WHERE ped-ent.nome-abrev   = tt-abre-ordem.nome-abrev
         AND ped-ent.nr-pedcli    = amg-nr-pedcli
         AND ped-ent.nr-sequencia = tt-abre-ordem.seq-ped
         AND ped-ent.it-codigo    = tt-abre-ordem.it-codigo
       BREAK BY ped-ent.dt-entrega:
    
               FIND ped-item of ped-ent NO-LOCK NO-ERROR.
               
               IF ped-item.ind-compon = 2 THEN DO:
                   
                  CREATE tt-componente-aux.
                  ASSIGN tt-componente-aux.seleciona    = yes
                         tt-componente-aux.seq-pai-tt   = c-seq-alter
                         tt-componente-aux.r-cot-estrut = tt-componente.r-cot-estrut
                         tt-componente-aux.pai-dd       = tt-componente.pai-dd
                         tt-componente-aux.nr-estrutura = tt-componente.nr-estrutura
                         tt-componente-aux.it-codigo    = ped-ent.it-codigo
                         tt-componente-aux.descricao    = tt-componente.descricao
                         tt-componente-aux.nivel        = tt-componente.nivel
                         tt-componente-aux.un           = tt-componente.un
                         tt-componente-aux.res-cq-fabr  = tt-componente.res-cq-fabr
                         tt-componente-aux.ressup-fabr  = tt-componente.ressup-fabr
                         tt-componente-aux.res-cq-acum  = tt-componente.res-cq-acum
                         tt-componente-aux.ressup-acum  = tt-componente.ressup-acum
                         tt-componente-aux.quant-ordem  = deQtOrdem
                         tt-componente-aux.nr-ordem     = ?
                         tt-componente-aux.ind-aprov    = tt-componente.ind-aprov
                         c-seq-alter = c-seq-alter + 2
                         tt-componente-aux.sequencia    = c-seq-alter - 1
                         tt-componente-aux.dt-entrega   = ped-ent.dt-entrega.
                  CREATE tt-rowid-ped-ent.
                  ASSIGN tt-rowid-ped-ent.it-codigo       = ped-ent.it-codigo
                         tt-rowid-ped-ent.seq-ped-ent     = ped-ent.nr-sequencia
                         tt-rowid-ped-ent.seq-componente  = IF AVAIL tt-componente-aux
                                                            THEN tt-componente-aux.sequencia
                                                            ELSE 0.
                         tt-rowid-ped-ent.r-ped-ent       = ROWID(ped-ent).
    
               END.  
               ASSIGN r-ped-item = ROWID(ped-item)
                      r-ped-ent  = ROWID(ped-ent). 

       END.
    
       for each tt-componente:
           delete tt-componente.
       end.
    
       for each tt-componente-aux break by tt-componente-aux.dt-entrega:
           create tt-componente.
           buffer-copy tt-componente-aux to tt-componente.
       end.
    
       for each tt-componente-aux:
           delete tt-componente-aux.
       end.
    
       FIND FIRST tt-componente NO-LOCK NO-ERROR.
    
       IF AVAIL tt-componente then do: 
          /*run cdp/cd9088.p (input rCotEstMast,
                            input r-ped-item,
                            input '422',
                            input-output table tt-componente,
                            INPUT r-ped-ent,
                            input TABLE tt-rowid-ped-ent).*/
                            
           run pi-acompanhar in h-acomp (input "Pedido item : " + tt-abre-ordem.it-codigo + " - " + " Fase:" + STRING(tt-abre-ordem.fase)).
                           
           run sfc/essf0011fb.p (input rCotEstMast,
                                 input r-ped-item,
                                 input '422',
                                 input-output table tt-componente,
                                 INPUT r-ped-ent,
                                 input TABLE tt-rowid-ped-ent,
                                 OUTPUT TABLE tt-digita).
            for each al-res-op.
               delete al-res-op.
            end.
           IF CAN-FIND(FIRST tt-digita WHERE
                             tt-digita.nr-ordem = 0) THEN DO: /*Quando api produ‡Æo retorna erro neste campo ‚ gravado zero - cd9088a*/
                FOR EACH tt-digita WHERE
                         tt-digita.nr-ordem = 0:

                    ASSIGN i-sequencia = 1.

                    FOR EACH ttErroAux:
                        IF  ttErroAux.i-sequen > i-sequencia THEN
                            ASSIGN i-sequencia = ttErroAux.i-sequen + 1.
                    END.
                  
                    create ttErroAux.
                    assign ttErroAux.i-sequen = i-sequencia
                           ttErroAux.cd-erro  = 17006
                           ttErroAux.mensagem = tt-digita.msgs.
                   return "NOK":U.
                END.
           END.
       end.

            /*Atualizar a espec¡fica com o numero da OP gerada*/
        FIND FIRST tt-componente NO-LOCK NO-ERROR.
         IF AVAIL tt-componente then
           ASSIGN tt-abre-ordem.nr-ord-produ = tt-componente.nr-ordem
     .
        
    END.

    if valid-handle (hDBOCotEstMast) then do:
       run destroy in hDBOCotEstMast.
    end.
END PROCEDURE.

PROCEDURE pi-cria-op-estoque:
 def input  param rCotEstMast as rowid                         no-undo. 
 def input  param deQtOrdem   as decimal   format ">>>,>>9.99" no-undo.
 def input  param cNomeAbrev  as character                     no-undo.
 def input  param cNrPedido   as character                     no-undo.
 def input  param iSequencia  as integer                       no-undo.

    /*--- Verifica se o DBO j  est  inicializado ---*/
    
    
    IF NOT VALID-HANDLE(hDBOCotEstMast) THEN DO:
        run mfbo/bomf067op.p persistent set hDBOCotEstMast.
    END.

    RUN openQueryStatic IN hDBOCotEstMast (INPUT "Main":U) NO-ERROR.

    for each tt-componente:
      delete tt-componente.
    end.
    
    run criaTTComponentePai /*in hDBOCotEstMast*/ (input        rCotEstMast,
                                               input        deQtOrdem,
                                               input        cNomeAbrev,
                                               input        cNrPedido,
                                               input        iSequencia,
                                               input-output iSeq,
                                               input-output table tt-componente).

    FOR EACH tt-componente:
       ASSIGN tt-componente.seleciona = YES.
    END.

    if return-value = "OK":U then do:
        find first tt-componente where tt-componente.seleciona = yes 
        no-lock no-error.
        if avail tt-componente then do:
            /*Substitu¡do pois nÆo retorna erros da api de produ‡Æo*/
           /*run cdp/cd9080.p (input rCotEstMast,
                             input ?,
                             input '422',
                             input-output table tt-componente).*/
                             
               run pi-acompanhar in h-acomp (input "Estoque item : " + tt-abre-ordem.it-codigo + " - " + " Fase:" + STRING(tt-abre-ordem.fase)).                             
                            
            run sfc/essf0011fa.p(input rCotEstMast,
                                 input ?,
                                 input '422',
                                 input-output table tt-componente,
                                 output table tt-digita).

            IF CAN-FIND(FIRST tt-digita WHERE
                              tt-digita.nr-ordem = 0) THEN DO: /*Quando api produ‡Æo retorna erro neste campo ‚ gravado zero*/
                FOR EACH tt-digita WHERE
                         tt-digita.nr-ordem = 0:

                    ASSIGN i-sequencia = 1.

                    FOR EACH ttErroAux:
                        IF  ttErroAux.i-sequen > i-sequencia THEN
                            ASSIGN i-sequencia = ttErroAux.i-sequen + 1.
                    END.
                  
                    create ttErroAux.
                    assign ttErroAux.i-sequen = i-sequencia
                           ttErroAux.cd-erro  = 17006
                           ttErroAux.mensagem = tt-digita.msgs.
                    
                    return "NOK":U.
                END.
            END.
        end.
                 /*Atualizar a espec¡fica com o numero da OP gerada*/
        FIND FIRST tt-componente NO-LOCK NO-ERROR.
         IF AVAIL tt-componente then
           ASSIGN tt-abre-ordem.nr-ord-produ = tt-componente.nr-ordem
     .
    END.

    if valid-handle (hDBOCotEstMast) then do:
       run destroy in hDBOCotEstMast.
    end.
END PROCEDURE.


PROCEDURE RetornaPesquisaVar :
def input  param pItem as char no-undo.
def input  param table for ttVarModelo.
def output param table for ttCotEstMastPesquisa.

def var cPesquisa as char no-undo.

for each ttCotEstMastPesquisa:
    delete ttCotEstMastPesquisa.
end.

for each ttVarModelo where
         ttVarModelo.procura = yes break by ttVarModelo.procura:
    
    if first(ttVarModelo.procura) then do:

        if ttVarModelo.tipo = 2 or
            ttVarModelo.tipo = 3 THEN 
            assign cPesquisa = ttVarModelo.nome-var + ttVarModelo.resultChar.
        
        else if ttVarModelo.tipo = 8 THEN 
            assign cPesquisa = "". /********* ACERTAR ********/
        
        ELSE         
            assign cPesquisa = ttVarModelo.nome-var + trim(string(ttVarModelo.resultDec)).
        
    end.
    else do:
        if ttVarModelo.tipo = 2 or
           ttVarModelo.tipo = 3 then
            assign cPesquisa = cPesquisa + /*" & "*/ " " + ttVarModelo.nome-var + ttVarModelo.resultChar.
        else if ttVarModelo.tipo = 8 then
            assign cPesquisa = "". /********* ACERTAR ********/
        else
            assign cPesquisa = cPesquisa + /*" & "*/ " " + ttVarModelo.nome-var + trim(string(ttVarModelo.resultDec)).
    end.
    
end.
if cPesquisa <> "" then do:
        for LAST cot-relat where
                 cot-relat.it-codigo = pItem /*and
                 cot-relat.des-carac-cf contains cPesquisa*/ :
            find cot-est-mast where
                 cot-est-mast.item-cotacao = cot-relat.it-codigo and
                 cot-est-mast.nr-estrut    = cot-relat.nr-estrut no-error.
            if avail cot-est-mast then do:
                create ttCotEstMastPesquisa.
                buffer-copy cot-est-mast to ttCotEstMastPesquisa
                    assign ttCotEstMastPesquisa.nr-pedcli    = cot-relat.nr-pedcli
                           ttCotEstMastPesquisa.nr-sequencia = cot-relat.nr-sequencia
                           ttCotEstMastPesquisa.nome-abrev   = cot-relat.nome-abrev.
            end.
        end.
    
end.
END PROCEDURE.

PROCEDURE pi-busca-config:
/*Procura uma configura‡Æo similar com Largura,DIIN e DIEX*/
def input  param p-item          as char no-undo.
def input  param p-largura       as INT no-undo.
def input  param p-diam-int      as INT no-undo.
def input  param p-diam-ext-conj as INT no-undo.
def input  param p-nr-bobinas    as INT no-undo.
def OUTPUT param p-conf-similar  as INT no-undo.

 for each ttVarModelo:
     delete ttVarModelo.
 end.
  
 for each var-modelo where
          var-modelo.mo-codigo = p-item and
         (var-modelo.nome-var  = "LARGURA" OR
          var-modelo.nome-var  = "DIIN" or
          var-modelo.nome-var  = "DIEX") NO-LOCK:
      
    create ttVarModelo.
    assign ttVarModelo.sequencia  = var-modelo.sequencia
           ttVarModelo.nome-var   = var-modelo.nome-var
           ttVarModelo.tipo       = var-modelo.tipo-result
           ttVarModelo.nr-tabela  = var-modelo.nr-tabela
           ttVarModelo.resultChar = ""
           ttVarModelo.tipo-var   = var-modelo.ind-tipo-var
           ttVarModelo.procura    = yes.

    CASE var-modelo.nome-var:
      WHEN "LARGURA" THEN ASSIGN ttVarModelo.resultDec  = p-largura.
      WHEN "DIIN"    THEN ASSIGN ttVarModelo.resultDec  = p-diam-int.
      WHEN "DIEX"    THEN ASSIGN ttVarModelo.resultDec  = p-diam-ext-conj.
    END CASE.
 end.

 run RetornaPesquisaVar (input  p-item,
                         input  table ttVarModelo,
                         output table ttCotEstMastPesquisa).

 FOR LAST ttCotEstMastPesquisa WHERE
          ttCotEstMastPesquisa.nr-pedcli = "":
 END.
 IF AVAIL ttCotEstMastPesquisa THEN 
    ASSIGN p-conf-similar = ttCotEstMastPesquisa.nr-estrut.
 ELSE 
    FOR LAST ttCotEstMastPesquisa:
    END.
    IF AVAIL ttCotEstMastPesquisa THEN 
       ASSIGN p-conf-similar = ttCotEstMastPesquisa.nr-estrut.
 /*fim procura configura‡Æo similar*/
END PROCEDURE.


PROCEDURE pi-altera-linha-op:
    /*Alterar a linha da OP para a linha cadastrada no POLSF012*/
    FOR EACH tt-ord-prod: DELETE tt-ord-prod. END.
    FOR EACH tt-erro:     DELETE tt-erro.     END.
    FIND FIRST ord-prod
         WHERE ord-prod.nr-ord-produ = tt-abre-ordem.nr-ord-produ NO-LOCK NO-ERROR.
    IF AVAIL ord-prod THEN DO:
     /*   FIND FIRST ctrab-linha
             WHERE ctrab-linha.cod-estabel = '422'
               AND ctrab-linha.cod-ctrab   = campanha-polo.gm-codigo NO-LOCK NO-ERROR.*/
       /* IF AVAIL ctrab-linha THEN DO:*/
            CREATE tt-ord-prod.
            BUFFER-COPY ord-prod TO tt-ord-prod.
            ASSIGN tt-ord-prod.cod-versao-integracao = 003
                   tt-ord-prod.faixa-numeracao       = 2
                   tt-ord-prod.prog-seg              = "pol011f"
                   tt-ord-prod.ep-codigo-usuario     = i-ep-codigo-usuario
                   tt-ord-prod.seg-usuario           = c-seg-usuario
                   tt-ord-prod.ind-tipo-movto        = 2
                   tt-ord-prod.nr-linha              = tt-abre-ordem.nr-linha.

            RUN cpp/cpapi301.p (INPUT-OUTPUT TABLE tt-ord-prod,
                                INPUT-OUTPUT TABLE tt-reapro,
                                INPUT-OUTPUT TABLE tt-erro,
                                YES).
           
            IF CAN-FIND(FIRST tt-erro) THEN DO:

                FOR EACH tt-erro:
                    ASSIGN i-sequencia = 1.
                    /* Ignora a mensagem 8609 para permitir pedidos avaliados */
                    if tt-erro.cd-erro = 8609 then next.

                    FOR EACH ttErroAux:
                        IF  ttErroAux.i-sequen > i-sequencia THEN
                            ASSIGN i-sequencia = ttErroAux.i-sequen + 1.
                    END.
                  
                    create ttErroAux.
                    assign ttErroAux.i-sequen = i-sequencia
                           ttErroAux.cd-erro  = tt-erro.cd-erro
                           ttErroAux.mensagem = tt-erro.mensagem.
                    
                    return "NOK":U.
                END.

            END.
        /*END.*/
    END.
    return "OK":U. 
END PROCEDURE.

PROCEDURE CriaTTComponentePai :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input        param pCotEstMast as rowid no-undo.
def input        param pDeQtOrdem  as dec format ">>>,>>9.99" no-undo.
def input        param pNomeAbrev  as char.
def input        param pNrPedido   as char.
def input        param pSequencia  as int.
def input-output param pSeq        as int   no-undo.
def input-output param table       for tt-componente.

  def var cMensagem as char no-undo.

  /********** Cria tt-componente do pai *********/
  find first cot-est-mast where
             rowid(cot-est-mast) = pCotEstMast no-lock no-error.
  
  find first item where
             item.it-codigo = cot-est-mast.item-cotacao no-lock no-error.

  if item.tipo-contr = 1 or item.tipo-contr = 3 then do:
      if item.tipo-contr = 1 then 
         {utp/ut-liter.i F¡sico MCP C}  
      else
         {utp/ut-liter.i Consignado MCP C}  

      assign cMensagem = return-value.
      {method/svc/errors/inserr.i
                   &ErrorNumber="6864"
                   &ErrorType="EMS"
                   &ErrorSubType="ERROR"
                   &ErrorParameters="item.it-codigo + '~~~~' + cMensagem"}
      RETURN "NOK":U.
  end.
  
  find last ord-prod where
            /*ord-prod.nome-abrev    = pNomeAbrev                     and */
            ord-prod.it-codigo     = item.it-codigo                 and
            ord-prod.nr-estrut     = cot-est-mast.nr-estrut         and
            ord-prod.item-cotacao  = ""                             and
            ord-prod.sequencia     = 0                              and
            ord-prod.es-codigo     = cot-est-mast.item-cotacao      and
            ord-prod.qt-ordem      = pDeQtOrdem                  /*   and
            ord-prod.nr-pedido     = pNrPedido                      and
            ord-prod.nr-sequencia  = pSequencia                    */
  no-lock no-error.

  assign pSeq = 1.

  create tt-componente.
  assign tt-componente.seleciona     = if avail ord-prod
                                       then no
                                       else yes
         tt-componente.r-cot-estrut    = ?
         tt-componente.nr-estrutura  = cot-est-mast.nr-estrut
         tt-componente.it-codigo     = cot-est-mast.item-cotacao
         tt-componente.descricao     = item.desc-item
         tt-componente.un            = item.un
         tt-componente.ind-aprov     = {mfinc/i01mf613.i 04 cot-est-mast.ind-aprov }
         tt-componente.sequencia     = pSeq
         tt-componente.pai-dd        = {ininc/i02in068.i 4 1}
         tt-componente.quant-ordem   = pDeQtOrdem
         tt-componente.ressup-fabr   = item.ressup-fabri
         tt-componente.res-cq-fabr   = item.res-cq-fabr
         tt-componente.nr-ordem      = if avail ord-prod then
                                          ord-prod.nr-ord-prod
                                       else 0.
  if item.baixa-estoq = no then assign tt-componente.seleciona = no.                                       

  find first var-result where
             var-result.item-cotacao = cot-est-mast.item-cotacao and
             var-result.nr-estrut    = cot-est-mast.nr-estrut    and
             var-result.ind-tipo-var = 3 no-lock no-error.
             
  if available var-result then 
     assign tt-componente.ressup-fabr = var-result.valor-dec. 
                                       
  RETURN "OK":U.                                    

END PROCEDURE.

PROCEDURE pi-ajusta-variaveis:
/*Ajsuta uma configura‡Æo similar com Largura,DIIN e DIEX*/
def input  param p-item          as char no-undo.
def input  param p-largura       as INT no-undo.
def input  param p-diam-int      as INT no-undo.
def input  param p-diam-ext      as INT no-undo.
def input  param p-nr-bobinas    as INT no-undo.
def INPUT  param p-nr-config     as INT no-undo.
DEF INPUT  PARAMETER p-qt-pedida    AS DEC NO-UNDO.

/*LARGURA */
    FIND var-result 
        WHERE var-result.item-cotacao = p-item
          AND var-result.nr-estrut    = p-nr-config
          AND var-result.nome-var     = "LARGURA"  EXCLUSIVE-LOCK NO-ERROR.
 
    IF AVAIL var-result then
        ASSIGN var-result.valor-char = ""               
               var-result.des-result = string(p-largura)    
               var-result.valor-dec  = p-largura.  /*otimizador - ped-campanha.qt-pedida*/         

/*DIIN */
    FIND var-result 
        WHERE var-result.item-cotacao = p-item
          AND var-result.nr-estrut    = p-nr-config
          AND var-result.nome-var     = "DIIN"  EXCLUSIVE-LOCK NO-ERROR.
 
    IF AVAIL var-result then
        ASSIGN var-result.valor-char = ""               
               var-result.des-result = string(p-DIAM-INT)    
               var-result.valor-dec  = p-DIAM-INT.  /*otimizador - ped-campanha.qt-pedida*/         
          
    
/*DIEX */
    FIND var-result 
        WHERE var-result.item-cotacao = p-item
          AND var-result.nr-estrut    = p-nr-config
          AND var-result.nome-var     = "DIEX"  EXCLUSIVE-LOCK NO-ERROR.
 
    IF AVAIL var-result then
        ASSIGN var-result.valor-char = ""               
               var-result.des-result = string(p-DIAM-EXT)    
               var-result.valor-dec  = p-DIAM-EXT.  /*otimizador - ped-campanha.qt-pedida*/         
    
  
/*QTDPEDIDO */
    FIND var-result 
        WHERE var-result.item-cotacao = p-item
          AND var-result.nr-estrut    = p-nr-config
          AND var-result.nome-var     = "QTDPEDIDO"  EXCLUSIVE-LOCK NO-ERROR.
 
    IF AVAIL var-result then
        ASSIGN var-result.valor-char = ""               
               var-result.des-result = string(p-qt-pedida)    
               var-result.valor-dec  = p-qt-pedida.  /*otimizador - ped-campanha.qt-pedida*/         
    
    /*QTDBOB */
    FIND var-result 
        WHERE var-result.item-cotacao = p-item
          AND var-result.nr-estrut    = p-nr-config
          AND var-result.nome-var     = "QTDBOB"  EXCLUSIVE-LOCK NO-ERROR.
 
    IF AVAIL var-result then
        ASSIGN var-result.valor-char = "" 
               var-result.des-result = string(p-nr-bobinas)
               var-result.valor-dec  = p-nr-bobinas. /*otimizador - ped-campanha.nr-bobinas*/

     /*QTDCALC */
    FIND var-result 
        WHERE var-result.item-cotacao = p-item
          AND var-result.nr-estrut    = p-nr-config
          AND var-result.nome-var     = "QTDCALC"  EXCLUSIVE-LOCK NO-ERROR.
 
    IF AVAIL var-result then
        ASSIGN var-result.valor-char = ""               
               var-result.des-result = string(p-qt-pedida)    
               var-result.valor-dec  = p-qt-pedida. /*otimizador - ped-campanha.qt-pedida*/
    
    /*APLIC */
    FIND var-result 
        WHERE var-result.item-cotacao = p-item      
          AND var-result.nr-estrut    = p-nr-config 
          AND var-result.nome-var     = "APLIC"  EXCLUSIVE-LOCK NO-ERROR.
 
    IF AVAIL var-result then
        ASSIGN var-result.valor-char = ""
               var-result.des-result = ""
               var-result.valor-dec  = 0.
               
     
    /*PRODCLIE*/
    FIND var-result 
        WHERE var-result.item-cotacao = p-item      
          AND var-result.nr-estrut    = p-nr-config 
          AND var-result.nome-var     = "PRODCLIE"  EXCLUSIVE-LOCK NO-ERROR.
 
    IF AVAIL var-result then
        ASSIGN var-result.valor-char = ""
               var-result.des-result = ""
               var-result.valor-dec  = 0.

    /*PEDCLI*/
    FIND var-result 
        WHERE var-result.item-cotacao = p-item      
          AND var-result.nr-estrut    = p-nr-config 
          AND var-result.nome-var     = "PEDCLI"  EXCLUSIVE-LOCK NO-ERROR.
 
    IF AVAIL var-result then
        ASSIGN var-result.valor-char = ""
               var-result.des-result = ""
               var-result.valor-dec  = 0.

    /*OBSEMB */
    FIND var-result 
        WHERE var-result.item-cotacao = p-item      
          AND var-result.nr-estrut    = p-nr-config 
          AND var-result.nome-var     = "OBSEMB"  EXCLUSIVE-LOCK NO-ERROR.
 
    IF AVAIL var-result then
        ASSIGN var-result.valor-char = ""
               var-result.des-result = ""
               var-result.valor-dec  = 0.

    /*OBSCQ */
    FIND var-result 
        WHERE var-result.item-cotacao = p-item      
          AND var-result.nr-estrut    = p-nr-config 
          AND var-result.nome-var     = "OBSCQ"  EXCLUSIVE-LOCK NO-ERROR.
 
    IF AVAIL var-result then
        ASSIGN var-result.valor-char = "PONTA"
               var-result.des-result = "PONTA"
               var-result.valor-dec  = 0.

    /*OBSGER */
    FIND var-result 
        WHERE var-result.item-cotacao = p-item      
          AND var-result.nr-estrut    = p-nr-config 
          AND var-result.nome-var     = "OBSGER"  EXCLUSIVE-LOCK NO-ERROR.
 
    IF AVAIL var-result then
        ASSIGN var-result.valor-char = ""
               var-result.des-result = ""
               var-result.valor-dec  = 0.

    /*OBSETQ */
    FIND var-result 
        WHERE var-result.item-cotacao = p-item      
          AND var-result.nr-estrut    = p-nr-config 
          AND var-result.nome-var     = "OBSETQ"  EXCLUSIVE-LOCK NO-ERROR.
 
    IF AVAIL var-result then
        ASSIGN var-result.valor-char = ""
               var-result.des-result = ""
               var-result.valor-dec  = 0.
   
    RUN CalculaVarFormula(INPUT p-item,        
                          INPUT p-nr-config).   
    
     /*CODEMBAL */
    FIND var-result 
        WHERE var-result.item-cotacao = p-item      
          AND var-result.nr-estrut    = p-nr-config 
          AND var-result.nome-var     = "CODEMBAL"  EXCLUSIVE-LOCK NO-ERROR.
 
    IF AVAIL var-result then
        ASSIGN var-result.valor-char = ""
               var-result.des-result = ""
               var-result.valor-dec  = 0.

    
  RETURN "OK".

END PROCEDURE.

PROCEDURE CalculaVarFormula:
    
 DEF INPUT PARAMETER p-item AS CHAR NO-UNDO.
 DEF INPUT PARAMETER p-config AS INT NO-UNDO.

    for each tt-variavel:
      delete tt-variavel.
    end.
    
    for each tt-var:
      delete tt-var.
    end.
    
    run CarregaConstantes (input-output table tt-variavel).
    
    for each tt-variavel: 
        
          find first tt-var 
               where tt-var.nome = tt-variavel.nome-var no-error.
          if not avail tt-var then
              create tt-var.
              assign tt-var.nome      = tt-variavel.nome-cte
                     tt-var.valor     = tt-variavel.valor-dec
                     tt-var.texto     = tt-variavel.valor-char
                     tt-var.nr-tabela = 0
                     tt-var.tipo-res  = tt-variavel.tipo-result
                     tt-var.abrev-nar = tt-variavel.abreviatura
                     tt-var.sequencia = tt-variavel.seq-var.
       
    end.
  
    for each var-result where 
             var-result.nr-estrut    = p-config 
        AND  var-result.item-cotacao = p-item  NO-LOCK: 

        find first var-modelo
             where var-modelo.mo-codigo = var-result.mo-codigo
               and var-modelo.nome-var  = var-result.nome-var no-lock no-error.

        IF (var-modelo.tipo-result > 3 AND 
            var-modelo.tipo-result < 7) OR
            var-modelo.tipo-result = 8 THEN DO:
            find var-mod-form where
                 var-mod-form.mo-codigo = var-modelo.mo-codigo and
                 var-mod-form.sequencia = var-modelo.sequencia no-lock no-error.
            IF AVAIL var-mod-form THEN DO:
                 create tt-var.
                 assign tt-var.nome      = var-modelo.nome-var
                        tt-var.valor     = var-result.valor-dec 
                        tt-var.texto     = var-result.valor-char 
                        tt-var.nr-tabela = 0
                        tt-var.tipo-res  = var-modelo.tipo-result
                        tt-var.abrev-nar = var-modelo.abreviatura
                        tt-var.sequencia = var-mod-form.sequencia.

                 /*cria tt com variaveis formulas*/
                 CREATE tt-var-form.
                 ASSIGN tt-var-form.nome-var     = var-modelo.nome-var
                        tt-var-form.sequencia    = var-modelo.sequencia
                        tt-var-form.tipo-result  = var-modelo.tipo-result
                        tt-var-form.formula      = var-mod-form.formula.
                
            END. /*var-mod-form*/
            
        END.
        ELSE DO:
           create tt-var.
           assign tt-var.nome      = var-modelo.nome-var
                  tt-var.valor     = var-result.valor-dec
                  tt-var.texto     = var-result.valor-char
                  tt-var.nr-tabela = 0
                  tt-var.tipo-res  = var-modelo.tipo-result
                  tt-var.abrev-nar = var-modelo.abreviatura
                  tt-var.sequencia = var-modelo.sequencia.
        END.
    END.
    /*Api de formula*/
    FOR EACH tt-var-form  NO-LOCK 
          BY tt-var-form.sequencia:

        IF (tt-var-form.nome-var = "PESOBOB" OR
            tt-var-form.nome-var = "QTDBOB" OR
            tt-var-form.nome-var = "QTDCALC") THEN NEXT.

        for each tt-formula:
            delete tt-formula.
        end.   
    
        create tt-formula.
        assign tt-formula.formula               = tt-var-form.formula
               tt-formula.tipo-res              = if tt-var-form.tipo-result = 8 then 2
                                                                                else 1
               tt-formula.cod-versao-integracao = 001.

        run cdp/cdapi020a.p (input-output table tt-formula,
                             input        table tt-var,
                             input-output table tt-erro,
                             input        yes).
        
        IF CAN-FIND(FIRST tt-erro) THEN RETURN "NOK".
         
        find first tt-formula.   
        IF AVAIL tt-formula THEN DO:

            IF tt-var-form.tipo-result = 5 THEN 
                 ASSIGN tt-formula.RESULT = TRUNCATE (tt-formula.RESULT, 0). 
             
            /*Atualiza resultado e tt-var*/
            FIND var-result 
                WHERE var-result.item-cotacao = p-item      
                  AND var-result.nr-estrut    = p-config 
                  AND var-result.nome-var     = tt-var-form.nome-var  EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL var-result THEN DO:
                ASSIGN var-result.valor-char = tt-formula.texto.
                       /*var-result.des-result = STRING(tt-formula.RESULT).*/
                if var-result.tipo-result <> 2 and 
                   var-result.tipo-result <> 3 and
                   var-result.tipo-result <> 8 then do:
                     assign var-result.des-result = trim(string(tt-formula.RESULT, "->>>>>>>9.9<<<<<"))
                            var-result.valor-dec  = tt-formula.RESULT. 
                                
                end.                             
                else  assign var-result.des-result = tt-formula.texto. 
                
                /*atualiza valor na tt-var das vari veis recalculadas*/
                FIND FIRST tt-var WHERE
                           tt-var.nome  = tt-var-form.nome-var EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL tt-var THEN
                        ASSIGN tt-var.valor = var-result.valor-dec 
                               tt-var.texto = var-result.valor-char. 
            END.
            
        END.
    END.

    RETURN "OK".
END PROCEDURE.

PROCEDURE CarregaConstantes :
def input-output param table for tt-variavel.

   def buffer b-tt-variavel for tt-variavel.

   for each constante
      where constante.ind-ativa = yes
      no-lock:

      find last b-tt-variavel no-error.

      create tt-variavel.
      assign tt-variavel.seq-mod        = 0
             tt-variavel.seq-var        = if avail b-tt-variavel then
                                             b-tt-variavel.seq-var + 1
                                          else 1
             tt-variavel.c-config       = "*"
             tt-variavel.mo-codigo      = ""
             tt-variavel.nome-var       = ""
             tt-variavel.nome-cte       = constante.nome-cte
             tt-variavel.sequencia-var  = 0
             tt-variavel.sequencia-res  = 10
             tt-variavel.descricao      = constante.descricao
             tt-variavel.tipo-valor     = 1
             tt-variavel.valor-char     = ""
             tt-variavel.valor-dec      = constante.valor-dec
             tt-variavel.abreviatura    = ""
             tt-variavel.gera-narrativa = no
             tt-variavel.ind-tipo-var   = 0
             tt-variavel.tipo-result    = 1
             tt-variavel.nr-tabela      = 0
             tt-variavel.endereco       = rowid(constante).
   end.
   
   RETURN "OK":U.

END PROCEDURE.


