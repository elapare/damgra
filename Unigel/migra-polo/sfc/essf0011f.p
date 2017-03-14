/**************************************************************
** GERACAO DE ORDEM DE PRODUCAO DAS CAMPANHAS
** Speto!
** Reformulado por Vera - As ordens da ped-campanha devem ser geradas
** conforme Configurador Produtos.
**************************************************************/
def buffer empresa for mgmulti.empresa.
/***************************************************************************
**  Programa : UT-GLOB.I
**  Include padr„o para definiÁ„o de variaveis globais.
***************************************************************************/ 
/* alteraªío feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */

/* est† verificaá∆o se faz necess†ria devido aos programas *//* e dessa forma n∆o chamarem a include i-wendef.i que define essa veri†vel. */

/* fim da alateraá∆o */


     
     def new global shared var i-ep-codigo-usuario  like empresa.ep-codigo no-undo.
     def new Global shared var l-implanta           as logical    init no.
     def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
     def new global shared var i-num-ped-exec-rpw   as integer no-undo.   
     def var rw-log-exec                            as rowid no-undo.
     def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
     def new global shared var l-rpc as logical no-undo.
     def var c-erro-rpc as character format "x(60)" initial " " no-undo.
     def var c-erro-aux as character format "x(60)" initial " " no-undo.
     def var c-ret-temp as char no-undo.
     def var h-servid-rpc as handle no-undo.     
     def new global shared var r-registro-atual as rowid no-undo.
     def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
     def new global shared var h-rsocial as handle no-undo.
     def new global shared var l-achou-prog as logical no-undo.

      /* Vari·veis Padr„o DWB / Datasul HR */
     def new global shared var i-num-ped as integer no-undo.         
     def new global shared var v_cdn_empres_usuar   like empresa.ep-codigo        no-undo.
     def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.
     def new global shared var h_prog_segur_estab     as handle                   no-undo.
     def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
     def new global shared var v_num_tip_aces_usuar   as int                      no-undo.


/* Transformacao Window */

    if session:window-system <> "TTY" then do:
                /* ---------------------------------------------------------------------------------------------------
   Autor      : Medeiros
   Data       : 01/Dez/97
   Objetivo   : Include usada nos programas que fazem interface 
                con a API do windows.
    
   Parametros : Nenhum.
--------------------------------------------------------------------------------- */


  
    /* 32-bit definitions, Progress 8.2+ */

    /* data types */
                   /* libraries */
                     /* messages */
/* mouse buttons */
/* scrollbars */
/* editors */
   /* some window styles */
/* some extended window styles */
/* system commands/menu */

/* placement order (Z-order) */
 
/* window-positioning flags */
/* get a handle to the procedure definitions */

   DEFINE NEW GLOBAL SHARED VARIABLE hpApi AS HANDLE NO-UNDO.
    IF NOT VALID-HANDLE(hpApi) OR
          hpApi:TYPE <> "PROCEDURE":U OR 
          hpApi:FILE-NAME <> "utp/ut-win.p":U THEN 
      RUN utp/ut-win.p PERSISTENT SET hpApi.
    /* forward function declarations. Must not be included in windows.p : */
   /* ---------------------------------------------------------------------------------------------------
   Autor      : Medeiros
   Data       : 01/Dez/97
   Objetivo   : Include usada nos programas que fazem interface 
                con a API do windows.
    
   Parametros : Nenhum.
--------------------------------------------------------------------------------- */   

/* prevent multiple inclusion: */


/* start persistent procedure holding the function implementations.
   The forward declarations are needed in winfunc.p, but the
   "run winfunc.p persistent" part must be prevented in winfunc.p : */
     
DEFINE NEW GLOBAL SHARED VARIABLE hpWinFunc AS HANDLE NO-UNDO.

  IF NOT VALID-HANDLE(hpWinFunc) or  
         hpWinFunc:TYPE <> "PROCEDURE":U or
         hpWinFunc:FILE-NAME <> "utp/ut-func.p":U THEN 
     RUN utp/ut-func.p PERSISTENT SET hpWinFunc.


/* --- the forward declarations : --- */

FUNCTION GetLastError      /* 1:1 implementation of API */
         RETURNS INTEGER   /* = dwErrorID */
         () 
         IN hpWinFunc.    

FUNCTION GetParent         /* 1:1 implementation of API */
         RETURNS INTEGER   /* = hWnd van parent */
         (input hwnd as INTEGER) 
         IN hpWinFunc.    

FUNCTION ShowLastError     /* calls GetLastError and views it as alert-box */
         RETURNS INTEGER   /* = dwErrorID */
         () 
         IN hpWinFunc.    

FUNCTION CreateProcess     /* wrapper for the big API definition */
         RETURNS INTEGER   /* = if success then hProcess else 0  */
         (input CommandLine as CHAR,
          input CurrentDir  as CHAR,
          input wShowWindow as INTEGER) 
         in hpWinFunc.    

/* &IF DEFINED(WINFUNC_I)=0 */

 

/* &IF DEFINED(WINDOWS_I)=0 */

 
      define var h-prog     as handle  no-undo.
      define var h-pai      as handle  no-undo.
      define var c-prog-tec as char    no-undo format "x(256)".
      define var i-template as integer no-undo.
    end.  

/* Transformacao Window */
/* Retorno RPC */

    procedure pi-seta-return-value:
    def input param ret as char no-undo.
    return ret.
  end procedure.


/* Retorno RPC */

/* ut-glob.i */

 
/*** Esta include no ems 2.01 n∆o dever† possuir nenhum c¢digo, serve 
     apenas para desenvolvimento tratar o conceito de miniflexibilizaá∆o.
     Utilizado apenas para MANUFATURA. ***/

/*** RELEASE 2.02 ***/

/* Funá‰es ch∆o f†brica e lista de componentes 2.02 - Logoplaste - Manufatura */


/*** RELEASE 2.03 ***/

/* Integraá∆o Magnus x SFC 2.03 - Klimmek - Manufatura *//* Relacionamento Linha Produá∆o x Estabelecimento     *//* Transaá∆o Reporte Ass°ncrono                        *//* Alteraá‰es Gerais EMS 2.03                          */

/*** RELEASE 2.04 ***/

/* Alteraá‰es Gerais EMS 2.04                          *//* Integraá∆o EAI                                     */ 
/**********************************************************************************
**
** Include - CPAPI301.I
** Funá∆o  - Definiá∆o das temp-tables
**
***********************************************************************************/

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
 
/**********************************************************************************
**
** Include - CPAPI301.I20
** Funá∆o  - f-gera-numero-op-manual
**           f-gera-numero-op-automatica
**
***********************************************************************************/

function f-gera-numero-op-manual returns integer.

    def var i-nr-ord-prod as  integer no-undo.
    def buffer b-ord-prod for ord-prod.

    find first param-cp no-lock no-error.
    find first param-global no-lock no-error. 

    assign i-nr-ord-prod = param-cp.prim-ord-man.
    for last  b-ord-prod fields (nr-ord-produ)
        where b-ord-prod.nr-ord-produ <= param-cp.ult-ord-man
          and b-ord-prod.nr-ord-produ >= param-cp.prim-ord-man no-lock: end.
    if avail b-ord-prod then do:
       assign i-nr-ord-prod = b-ord-prod.nr-ord-produ + 1.
       if i-nr-ord-prod > param-cp.ult-ord-man then
          assign i-nr-ord-prod = param-cp.prim-ord-man.
    end.
    else 
       assign i-nr-ord-prod = param-cp.prim-ord-man.
    repeat:
           if can-find (b-ord-prod where 
                        b-ord-prod.nr-ord-produ = i-nr-ord-prod no-lock) then do:
              assign i-nr-ord-prod = i-nr-ord-prod + 1.
              
              /* --- Bancos Hist¢ricos --- */
              if param-global.modulo-bh then do:
                 find last his-ord-prod no-lock no-error.
                 if avail his-ord-prod and
                    his-ord-prod.nr-ord-produ > i-nr-ord-prod then
                    assign i-nr-ord-prod = (his-ord-prod.nr-ord-produ + 1).
              end.
              /* ------------------------- */
              
              if i-nr-ord-prod > param-cp.ult-ord-man then return -1.               
           end. 
           else if param-global.modulo-mi and
                   can-find (ord-manut where 
                             ord-manut.nr-ord-produ = i-nr-ord-prod) then
                   assign i-nr-ord-prod = i-nr-ord-prod + 1.
                else leave.
    end.
    
    return i-nr-ord-prod.
    
end function.
    
function f-gera-numero-op-automatica returns integer.
    def var i-nr-ord-prod as  integer no-undo.
    def buffer b-ord-prod for ord-prod.

    find first param-cp no-lock no-error.
    find first param-global no-lock no-error. 
    for last  b-ord-prod fields (nr-ord-produ)
        where b-ord-prod.nr-ord-produ <= param-cp.ult-ord-aut-cp
          and b-ord-prod.nr-ord-produ >= param-cp.prim-ord-aut-cp no-lock: end.
    if avail b-ord-prod then do:
       assign i-nr-ord-prod = b-ord-prod.nr-ord-produ + 1.
       if i-nr-ord-prod > param-cp.ult-ord-aut-cp then
          assign i-nr-ord-prod = param-cp.prim-ord-aut-cp.
    end.
    else 
       assign i-nr-ord-prod = param-cp.prim-ord-aut-cp.    
    repeat:
           if can-find (b-ord-prod where 
                        b-ord-prod.nr-ord-produ = i-nr-ord-prod no-lock) then do:
              assign i-nr-ord-prod = i-nr-ord-prod + 1.
              
              /* --- Bancos Hist¢ricos --- */
              if param-global.modulo-bh then do:
                 find last his-ord-prod no-lock no-error.
                 if avail his-ord-prod and
                    his-ord-prod.nr-ord-produ > i-nr-ord-prod then
                    assign i-nr-ord-prod = (his-ord-prod.nr-ord-produ + 1).
              end.
              /* ------------------------- */
              
              if i-nr-ord-prod > param-cp.ult-ord-aut-cp then return -1.         
           end. 
           else if param-global.modulo-mi and
                   can-find (ord-manut where 
                             ord-manut.nr-ord-produ = i-nr-ord-prod) then
                   assign i-nr-ord-prod = i-nr-ord-prod + 1.
                else leave.
    end.
        
    find current param-cp exclusive-lock.
    assign param-cp.prox-ord-aut = i-nr-ord-prod + 1.
    find current param-cp no-lock.
    return i-nr-ord-prod.
    
end function.

  /* f-gera-numero-op */ 
/*******************************************************************
**
**  CD0666.I - Definicao temp-table de erros
**
*******************************************************************/
def new global shared var l-multi as logical initial yes.

def var l-del-erros as logical init yes.
def var v-nom-arquivo-cb as char format "x(50)" no-undo.
def var c-mensagem-cb    as char format "x(132)" no-undo.

def  temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)".

form
    space(04)
    tt-erro.cd-erro 
    space (02)
    c-mensagem-cb
    with width 132 no-box down stream-io frame f-consiste.

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Mensagem",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tt-erro.cd-erro:label in frame f-consiste = trim(return-value).

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr∆o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Descriá∆o",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-mensagem-cb:label in frame f-consiste = trim(return-value).
 
/*********************************************************************************
 ** Definiá∆o tt-digita
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
  
 def var c-observacao as char format "x(50)".
   /*tt-digita - OP das pontas*/

def temp-table ttErroAux NO-UNDO like tt-erro.

DEFINE INPUT PARAMETER p-rw-campanha AS ROWID NO-UNDO.

DEFINE VARIABLE l-cria-ordem-producao AS LOGICAL NO-UNDO.
DEFINE VARIABLE l-cria-ordem-pedido   AS LOGICAL NO-UNDO.
DEFINE VARIABLE i-sequencia           AS INTEGER NO-UNDO.

/*Criaá∆o OP para pedido*/
def var iSeq         as int   no-undo.
define variable c-seq-alter      as integer.
DEFINE VARIABLE de-proporcao     AS DEC     NO-UNDO INITIAL 0.
define variable r-ped-item       as rowid   no-undo. 
define variable r-ped-ent        as rowid   no-undo.
DEFINE VARIABLE hDBOCotEstMast AS HANDLE NO-UNDO.

/*********************************************************
 ** CD0530.i3 - Definiá∆o TT-COMPONENTE
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
    FIELD nr-entrega     LIKE ped-ent.nr-entrega . /* edson mudanáa tt-componente EMS-padrao */
/* FIM - CD0530.I3 */
  /*tt-componente*/

define buffer b-tt-componente for tt-componente.
define buffer b2-tt-componente for tt-componente.
DEFINE BUFFER b2-ped-item     FOR ped-item.
DEFINE BUFFER b2-ped-ent      FOR ped-ent.
define buffer b-ped-item      for ped-item.

define temp-table tt-rowid-ped-ent
         field it-codigo      like item.it-codigo 
         field seq-componente as integer
         field seq-ped-ent    as integer
         field r-ped-ent      as rowid.

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

/******/
/*Criaá∆o OP para estoque - Pontas*/
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

def new global shared var h-acomp as handle no-undo.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Geraá∆o Ordens de Produá∆o").

find first param-cp no-lock no-error.

FOR EACH ttErroAux:
   DELETE ttErroAux.
END.

blk-do:
do TRANSACTION ON ERROR UNDO, LEAVE:
    blk-for:
    FOR FIRST campanha-polo
        WHERE ROWID(campanha-polo) = p-rw-campanha NO-LOCK:

        ASSIGN l-cria-ordem-producao = NO.

        IF CAN-FIND(FIRST ped-campanha WHERE
                          ped-campanha.gm-codigo    = campanha-polo.gm-codigo     AND
                          ped-campanha.ano-campanha = campanha-polo.ano-campanha  AND
                          ped-campanha.mes-campanha = campanha-polo.mes-campanha  AND
                          ped-campanha.cd-produto   = campanha-polo.cd-produto    AND
                          ped-campanha.sequencia    = campanha-polo.sequencia     AND
                          ped-campanha.nr-ord-produ <> 0) THEN DO:
           
               RUN utp/ut-msgs.p (INPUT "show",
                                  INPUT 17006,
                                  INPUT "Pedido da Campanha j† possui Ordem Produá∆o associada." + "~~" +
                                  "S¢ Ç poss°vel a criaá∆o de uma OP para cada Pedido da Campanha.").
                                  
               ASSIGN l-cria-ordem-producao = NO.
               LEAVE blk-do.
    
        END.
        ELSE ASSIGN l-cria-ordem-producao = YES.
        
        FIND FIRST prog-campanha  
                   WHERE prog-campanha.gm-codigo    = campanha-polo.gm-codigo
                   AND   prog-campanha.ano-campanha = campanha-polo.ano-campanha
                   AND   prog-campanha.mes-campanha = campanha-polo.mes-campanha
                   AND   prog-campanha.cd-produto   = campanha-polo.cd-produto
                   AND   prog-campanha.sequencia    = campanha-polo.sequencia
                   NO-LOCK NO-ERROR.
        IF  AVAIL prog-campanha and
                  l-cria-ordem-producao THEN DO:

            RUN piCriaOPPedCampanha.
            IF  RETURN-VALUE = "NOK":U THEN
                undo blk-for, leave blk-for.
       
        END.
        ELSE DO:
            ASSIGN i-sequencia = 1.

            FOR EACH ttErroAux:
                IF  ttErroAux.i-sequen > i-sequencia THEN
                    ASSIGN i-sequencia = ttErroAux.i-sequen + 1.
            END.

            CREATE ttErroAux.
            ASSIGN ttErroAux.i-sequen = i-sequencia
                   ttErroAux.cd-erro  = 25997
                   ttErroAux.mensagem = "Campanha n∆o possui uma Programaá∆o de Corte.".
        END.
        
    END.

    if can-find(first ttErroAux) then do:
       undo blk-do, leave blk-do.
    end.
end.

run pi-finalizar in h-acomp.

if can-find(first ttErroAux) then do:
   run cdp/cd0666.w (input table ttErroAux).
end.

RETURN "OK":U.

/*****************************************************************
** Procedures
*****************************************************************/

procedure piCriaOPPedCampanha:
    for each ped-campanha exclusive-lock of campanha-polo:
        /*Descarta ped-campanha com qtde zerada - casos em que a qtde n∆o equivale a 1 bobina pelo menos*/
        IF ped-campanha.qt-pedida = 0 THEN NEXT.

        CASE ped-campanha.tipo-ped:
            WHEN 1 THEN DO: /*Pedido*/
                /*Verificar se todas as ped-campanha tem programa de corte efetivado*/
                IF NOT CAN-FIND(FIRST prog-corte WHERE
                                      prog-corte.gm-codigo    = ped-campanha.gm-codigo AND
                                      prog-corte.ano-campanha = ped-campanha.ano-campanha AND
                                      prog-corte.mes-campanha = ped-campanha.mes-campanha AND
                                      prog-corte.cd-produto   = ped-campanha.cd-produto AND
                                      prog-corte.sequencia    = ped-campanha.sequencia and
                                      prog-corte.diam-int     = ped-campanha.diam-int and
                                      prog-corte.diam-ext     = ped-campanha.diam-ext-conj) THEN DO:
                    
                    RUN utp/ut-msgs.p (INPUT "show",
                                       INPUT 17006,
                                       INPUT " Geraá∆o da Ordem para Pedido n∆o permitida."
                                       + "~~" 
                                       + "N∆o encontrado Programa de Corte para largura " +
                                       STRING(ped-campanha.largura) +
                                       " e diÉmetro externo " +
                                       STRING(ped-campanha.diam-ext-conj) +
                                       ".").
                    ASSIGN l-cria-ordem-pedido = NO.
                    return "NOK":U.
              
                END.
                ELSE ASSIGN l-cria-ordem-pedido = YES.
                
                IF l-cria-ordem-pedido THEN DO:
                    FIND ped-item WHERE
                         ped-item.nome-abrev   = ped-campanha.nome-abrev AND
                         ped-item.nr-pedcli    = ped-campanha.nr-pedcli AND
                         ped-item.nr-sequencia = ped-campanha.nr-sequencia AND
                         ped-item.it-codigo    = ped-campanha.it-codigo AND
                         ped-item.cod-refer    = ped-campanha.cod-refer NO-LOCK NO-ERROR.
                    IF AVAIL ped-item THEN DO:
                        find first cot-est-mast where 
                                   cot-est-mast.nr-estrut    = ped-item.nr-config AND
                                   cot-est-mast.item-cotacao = ped-item.it-codigo no-lock no-error.   
                        IF AVAIL cot-est-mast THEN DO:
                           RUN pi-cria-op-pedido (ROWID(cot-est-mast),
                                                  ped-campanha.qt-pedida,
                                                  ped-item.nome-abrev,
                                                  ped-item.nr-pedcli,
                                                  ped-item.nr-sequencia).
                        END.
                           
                    END.
                END.
            END.
            WHEN 3 THEN DO: /*Pedido para Estoque - Pontas*/
                /*Verificar se todas as ped-campanha tem programa de corte efetivado*/
                IF NOT CAN-FIND(FIRST prog-corte WHERE
                                      prog-corte.gm-codigo    = ped-campanha.gm-codigo AND
                                      prog-corte.ano-campanha = ped-campanha.ano-campanha AND
                                      prog-corte.mes-campanha = ped-campanha.mes-campanha AND
                                      prog-corte.cd-produto   = ped-campanha.cd-produto AND
                                      prog-corte.sequencia    = ped-campanha.sequencia and
                                      prog-corte.diam-int     = ped-campanha.diam-int and
                                      prog-corte.diam-ext     = ped-campanha.diam-ext-conj) THEN DO:
                    
                    RUN utp/ut-msgs.p (INPUT "show",
                                       INPUT 17006,
                                       INPUT " Geraá∆o da Ordem para Estoque n∆o permitida."
                                       + "~~" 
                                       + "N∆o encontrado Programa de Corte para largura " +
                                       STRING(ped-campanha.largura) +
                                       " e diÉmetro externo " +
                                       STRING(ped-campanha.diam-ext-conj) +
                                       ".").
                    ASSIGN l-cria-ordem-pedido = NO.
                    return "NOK":U.
              
                END.
                ELSE ASSIGN l-cria-ordem-pedido = YES.
                
                /*Geraá∆o automatica configuraá∆o para Ponta utilizando c¢pia de configuraá∆o similar*/
                RUN pi-busca-config (INPUT  ped-campanha.it-codigo,
                                     INPUT  ped-campanha.largura,
                                     INPUT  ped-campanha.diam-int,
                                     INPUT  ped-campanha.diam-ext-conj,
                                     INPUT  ped-campanha.nr-bobinas,
                                     OUTPUT i-config-similar).
                /*cria nova configuraá∆o*/
                IF i-config-similar <> 0 THEN DO:
                    /*criar† nova configuraá∆o atravÇs da c¢pia da configuraá∆o similar encontrada*/
                    RUN sfc/essf0011fc.p(input ped-campanha.it-codigo,
                                              INPUT ped-campanha.nr-bobinas,
                                              INPUT ped-campanha.qt-pedida,
                                              input i-config-similar,
                                              OUTPUT i-nr-estrut-criada,
                                              output table tt-erro).
                    
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
                    
                    IF i-nr-estrut-criada <> 0 THEN
                       ASSIGN i-nr-estrut = i-nr-estrut-criada
                              l-cria-ordem-pedido = YES.
                    ELSE ASSIGN l-cria-ordem-pedido = NO.
                END.
                ELSE DO: /*N∆o encontrou configuraá∆o similar*/
                     RUN utp/ut-msgs.p (INPUT "show",
                                        INPUT 17006,
                                        INPUT " Geraá∆o da Ordem para Estoque (Pontas) n∆o permitida."
                                        + "~~" 
                                        + "N∆o encontrada configuraá∆o para largura " +
                                        STRING(ped-campanha.largura) +
                                        " diÉmetro interno " +
                                        STRING(ped-campanha.diam-int) +
                                        " diÉmetro externo " +
                                        STRING(ped-campanha.diam-ext-conj) +
                                        ".").
                     ASSIGN l-cria-ordem-pedido = NO.
                     return "NOK":U.
                END.
                /**/
                IF l-cria-ordem-pedido THEN DO:
                    /*Encontrando configuraá∆o para a Ponta inclu°da gerar a ordem para estoque*/
                    find first cot-est-mast where 
                               cot-est-mast.nr-estrut    = i-nr-estrut AND
                               cot-est-mast.item-cotacao = ped-campanha.it-codigo no-lock no-error.   
                    IF AVAIL cot-est-mast THEN DO:
                       RUN pi-cria-op-estoque (ROWID(cot-est-mast),
                                               ped-campanha.qt-pedida,
                                               "",
                                               "",
                                               0).
                    END.
                    
                END.
            END.
        END CASE.
        RUN pi-altera-linha-op.
    END. /*for each pe-campanha*/
    return "OK":U.

end procedure.

PROCEDURE pi-altera-linha-op:
    /*Alterar a linha da OP para a linha cadastrada no POLSF012*/
    FOR EACH tt-ord-prod: DELETE tt-ord-prod. END.
    FOR EACH tt-erro:     DELETE tt-erro.     END.
    FIND FIRST ord-prod
         WHERE ord-prod.nr-ord-produ = ped-campanha.nr-ord-produ NO-LOCK NO-ERROR.
    IF AVAIL ord-prod THEN DO:
        FIND FIRST ctrab-linha
             WHERE ctrab-linha.cod-estabel = '412' OR ctrab-linha.cod-estabel = '422'/*solic-318*/
               AND ctrab-linha.cod-ctrab   = campanha-polo.gm-codigo NO-LOCK NO-ERROR.
        IF AVAIL ctrab-linha THEN DO:
            CREATE tt-ord-prod.
            BUFFER-COPY ord-prod TO tt-ord-prod.
            ASSIGN tt-ord-prod.cod-versao-integracao = 003
                   tt-ord-prod.faixa-numeracao       = 2
                   tt-ord-prod.prog-seg              = "pol011f"
                   tt-ord-prod.ep-codigo-usuario     = i-ep-codigo-usuario
                   tt-ord-prod.seg-usuario           = c-seg-usuario
                   tt-ord-prod.ind-tipo-movto        = 2
                   tt-ord-prod.nr-linha              = ctrab-linha.int-1.

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
        END.
    END.
    return "OK":U. 
END PROCEDURE.

PROCEDURE pi-cria-op-pedido:
 def input  param rCotEstMast as rowid                         no-undo. 
 def input  param deQtOrdem   as decimal   format ">>>,>>9.99" no-undo.
 def input  param cNomeAbrev  as character                     no-undo.
 def input  param cNrPedido   as character                     no-undo.
 def input  param iSequencia  as integer                       no-undo.

    /*--- Verifica se o DBO j† est† inicializado ---*/
    
    
    IF NOT VALID-HANDLE(hDBOCotEstMast) THEN DO:
        run mfbo/bomf067op.p persistent set hDBOCotEstMast.
    END.

    RUN openQueryStatic IN hDBOCotEstMast (INPUT "Main":U) NO-ERROR.

    for each tt-componente:
      delete tt-componente.
    end.

    run criaTTComponentePai in hDBOCotEstMast (input        rCotEstMast,
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
       ASSIGN c-seq-alter = 0.

       FOR EACH tt-componente WHERE tt-componente.seleciona = YES, 
       EACH ped-ent FIELDS (nome-abrev  nr-pedcli  it-codigo    nr-sequencia 
                            dt-entrega  cod-refer  nr-programa  qt-pedida) NO-LOCK 
       WHERE ped-ent.nome-abrev   = ped-campanha.nome-abrev
         AND ped-ent.nr-pedcli    = ped-campanha.nr-pedcli
         AND ped-ent.nr-sequencia = ped-campanha.nr-sequencia
         AND ped-ent.it-codigo    = ped-campanha.it-codigo
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
               ELSE DO:
                    find first b-tt-componente where b-tt-componente.it-codigo = ped-ent.it-codigo no-lock no-error.
                    find first b2-tt-componente-aux where b2-tt-componente-aux.dt-entrega = ped-ent.dt-entrega no-lock no-error.
                    find last  b3-tt-componente-aux where b3-tt-componente-aux.it-codigo  = b2-tt-componente-aux.it-codigo no-lock no-error.
    
                    FIND FIRST item WHERE item.it-codigo = ped-ent.it-codigo NO-LOCK NO-ERROR.
    
                    CREATE b-tt-componente-aux.
                    ASSIGN b-tt-componente-aux.quant-ordem = deQtOrdem * de-proporcao / 100.
    
                    IF item.fraciona = NO THEN DO:
    
                       ASSIGN b-tt-componente-aux.quant-ordem = ROUND(b-tt-componente-aux.quant-ordem,0).
    
                    END.
    
                    ASSIGN b-tt-componente-aux.seleciona    = yes
                           b-tt-componente-aux.seq-pai-tt   = IF AVAIL b2-tt-componente-aux THEN b2-tt-componente-aux.sequencia /*if c-seq-alter = 0 then 1 else c-seq-alter */
                                                              ELSE c-seq-alter
                           b-tt-componente-aux.r-cot-estrut = tt-componente.r-cot-estrut
                           b-tt-componente-aux.pai-dd       = tt-componente.pai-dd
                           b-tt-componente-aux.nr-estrutura = tt-componente.nr-estrutura
                           b-tt-componente-aux.it-codigo    = ped-ent.it-codigo
                           b-tt-componente-aux.descricao    = tt-componente.descricao
                           b-tt-componente-aux.nivel        = tt-componente.nivel
                           b-tt-componente-aux.un           = tt-componente.un
                           b-tt-componente-aux.res-cq-fabr  = tt-componente.res-cq-fabr
                           b-tt-componente-aux.ressup-fabr  = tt-componente.ressup-fabr
                           b-tt-componente-aux.res-cq-acum  = tt-componente.res-cq-acum
                           b-tt-componente-aux.ressup-acum  = tt-componente.ressup-acum
                           b-tt-componente-aux.quant-ordem  = ped-ent.qt-pedida
                           b-tt-componente-aux.nr-ordem     = ?
                           b-tt-componente-aux.ind-aprov    = tt-componente.ind-aprov
                           b-tt-componente-aux.sequencia    = IF AVAIL b3-tt-componente-aux AND c-seq-alter = 0 
                                                                 THEN b3-tt-componente-aux.sequencia 
                                                              ELSE c-seq-alter
                           b-tt-componente-aux.dt-entrega   = ped-ent.dt-entrega
                           c-seq-alter                      = IF AVAIL b3-tt-componente-aux AND c-seq-alter = 0 
                                                                 THEN b3-tt-componente-aux.sequencia + 1 
                                                              ELSE c-seq-alter + 1.
    
                    CREATE tt-rowid-ped-ent.
                    ASSIGN tt-rowid-ped-ent.it-codigo       = ped-ent.it-codigo
                           tt-rowid-ped-ent.seq-ped-ent     = ped-ent.nr-sequencia
                           tt-rowid-ped-ent.seq-componente  = b-tt-componente-aux.sequencia
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
                            input '{cdp\poloestab.i 422}' ,
                            input-output table tt-componente,
                            INPUT r-ped-ent,
                            input TABLE tt-rowid-ped-ent).*//*solic-318*/
           run sfc/essf0011fb.p (input rCotEstMast,
                                      input r-ped-item,
                                      input '{cdp\poloestab.i 422}' /*solic-318*/,
                                      input-output table tt-componente,
                                      INPUT r-ped-ent,
                                      input TABLE tt-rowid-ped-ent,
                                      OUTPUT TABLE tt-digita).

           IF CAN-FIND(FIRST tt-digita WHERE
                             tt-digita.nr-ordem = 0) THEN DO: /*Quando api produá∆o retorna erro neste campo Ç gravado zero - cd9088a*/
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
       /*Atualizar a espec°fica com o numero da OP gerada*/
       FIND FIRST tt-componente NO-LOCK NO-ERROR.
       IF AVAIL tt-componente then
           ASSIGN ped-campanha.nr-ord-produ = tt-componente.nr-ordem.
       
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

    /*--- Verifica se o DBO j† est† inicializado ---*/
    
    
    IF NOT VALID-HANDLE(hDBOCotEstMast) THEN DO:
        run mfbo/bomf067op.p persistent set hDBOCotEstMast.
    END.

    RUN openQueryStatic IN hDBOCotEstMast (INPUT "Main":U) NO-ERROR.

    for each tt-componente:
      delete tt-componente.
    end.
    
    run criaTTComponentePai in hDBOCotEstMast (input        rCotEstMast,
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
            /*Substitu°do pois n∆o retorna erros da api de produá∆o*/
           /*run cdp/cd9080.p (input rCotEstMast,
                             input ?,
                             input '{cdp\poloestab.i 422}' ,
                             input-output table tt-componente).*//*solic-318*/
            run sfc/essf0011fa.p(input rCotEstMast,
                                      input ?,
                                      input '{cdp\poloestab.i 422}' /*solic-318*/,
                                      input-output table tt-componente,
                                      output table tt-digita).

            IF CAN-FIND(FIRST tt-digita WHERE
                              tt-digita.nr-ordem = 0) THEN DO: /*Quando api produá∆o retorna erro neste campo Ç gravado zero*/
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
        /*Atualizar a espec°fica com o numero da OP gerada*/
        FIND FIRST tt-componente NO-LOCK NO-ERROR.
        IF AVAIL tt-componente then
           ASSIGN ped-campanha.nr-ord-produ = tt-componente.nr-ordem.
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
        for each cot-relat where
                 cot-relat.it-codigo = pItem and
                 cot-relat.des-carac-cf contains cPesquisa:
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
/*Procura uma configuraá∆o similar com Largura,DIIN e DIEX*/
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
 /*fim procura configuraá∆o similar*/
END PROCEDURE.


