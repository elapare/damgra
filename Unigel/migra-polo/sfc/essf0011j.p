/**************************************************************
** GERACAO DE ORDEM DE PRODUCAO DAS CAMPANHAS
** Speto!
** Reformulado por Vera - As ordens da ped-campanha devem ser geradas
** conforme Configurador Produtos.
**************************************************************/
def buffer empresa for mgmulti.empresa.
/*** Esta include no ems 2.01 n∆o dever† possuir nenhum c¢digo, serve 
     apenas para desenvolvimento tratar o conceito de miniflexibilizaá∆o.
     Utilizado apenas para MANUFATURA. ***/

/*** RELEASE 2.02 ***/

/* Funá‰es ch∆o f†brica e lista de componentes 2.02 - Logoplaste - Manufatura */


/*** RELEASE 2.03 ***/

/* Integraá∆o Magnus x SFC 2.03 - Klimmek - Manufatura *//* Relacionamento Linha Produá∆o x Estabelecimento     *//* Transaá∆o Reporte Ass°ncrono                        *//* Alteraá‰es Gerais EMS 2.03                          */

/*** RELEASE 2.04 ***/

/* Alteraá‰es Gerais EMS 2.04                          *//* Integraá∆o EAI                                     */ 
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
 

DEF TEMP-TABLE ttErroAux   NO-UNDO LIKE tt-erro.
DEF TEMP-TABLE tt-campanha LIKE campanha-polo
    FIELD rw-campanha AS ROWID.

DEFINE INPUT  PARAMETER TABLE FOR tt-campanha.
DEFINE OUTPUT PARAMETER i-nr-ord-produ LIKE ord-prod.nr-ord-produ NO-UNDO.

DEFINE VARIABLE l-cria-ordem-producao AS LOGICAL NO-UNDO.
DEFINE VARIABLE i-sequencia           AS INTEGER NO-UNDO.
DEFINE VARIABLE de-total-op           AS DECIMAL NO-UNDO.
DEFINE VARIABLE c-cd-produto          LIKE ITEM.it-codigo NO-UNDO.
DEFINE VARIABLE c-gm-codigo           AS CHAR NO-UNDO.
/*DEFINE VARIABLE i-nr-ord-produ        LIKE ord-prod.nr-ord-produ NO-UNDO.*/

DEF TEMP-TABLE tt-componente-aux NO-UNDO
    FIELD seleciona      AS LOGICAL FORMAT "*/ "
    FIELD seq-pai-tt     AS INTEGER
    FIELD r-cot-estrut   AS ROWID
    FIELD pai-dd         AS CHAR FORMAT "x(1)"
    FIELD nr-estrutura LIKE cot-estrut.nr-estrut
    FIELD it-codigo    LIKE ITEM.it-codigo
    FIELD descricao      AS CHAR FORMAT "x(36)"
    FIELD nivel          AS INTEGER FORMAT "99"
    FIELD un           LIKE ITEM.un
    FIELD res-cq-fabr  LIKE ITEM.res-cq-fabr
    FIELD ressup-fabr  LIKE ITEM.ressup-fabr
    FIELD res-cq-acum  LIKE ITEM.res-cq-fabr
    FIELD ressup-acum  LIKE ITEM.ressup-fabr
    FIELD quant-ordem  LIKE ord-prod.qt-produzida
    FIELD nr-ordem     LIKE ord-prod.nr-ord-prod
    FIELD ind-aprov      AS CHAR FORMAT "x(20)"
    FIELD sequencia      AS INTEGER
    FIELD dt-entrega   LIKE ped-ent.dt-entrega.

DEF NEW GLOBAL SHARED VAR h-acomp AS HANDLE NO-UNDO.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Geraá∆o Ordens de Produá∆o").

FIND FIRST param-cp NO-LOCK NO-ERROR.

FOR EACH ttErroAux:
   DELETE ttErroAux.
END.

blk-do:
do TRANSACTION ON ERROR UNDO, LEAVE:
    blk-for:

    FOR EACH tt-campanha:

        FOR FIRST campanha-polo
            WHERE ROWID(campanha-polo) = tt-campanha.rw-campanha EXCLUSIVE-LOCK:
    
            IF   campanha-polo.tipo <> 1 THEN DO:
                ASSIGN i-sequencia = 1.
    
                FOR EACH ttErroAux:
                    IF  ttErroAux.i-sequen > i-sequencia THEN
                        ASSIGN i-sequencia = ttErroAux.i-sequen + 1.
                END.
    
                CREATE ttErroAux.
                ASSIGN ttErroAux.i-sequen = i-sequencia
                       ttErroAux.cd-erro  = 25997
                       ttErroAux.mensagem = "Campanha Ç uma parada. N∆o Ç poss°vel gerar Ordem de Produá∆o.".
            END.
            IF CAN-FIND(FIRST ttErroAux) THEN DO:
                  RUN cdp/cd0666.w (INPUT TABLE ttErroAux).
                  UNDO blk-do, LEAVE blk-do.
            END.
    
            ASSIGN de-total-op  = de-total-op + campanha-polo.quantidade
                   c-cd-produto = campanha-polo.cd-produto
                   c-gm-codigo  = campanha-polo.gm-codigo.
                    
            ASSIGN campanha-polo.situacao = 3.

        END.
    END.

    RUN piCriaOPCampanha.
    IF RETURN-VALUE = "NOK":U THEN DO:
        RUN cdp/cd0666.w (INPUT TABLE ttErroAux).
        UNDO blk-do, LEAVE blk-do.
    END.
    ELSE
        FOR EACH tt-campanha:
            FIND campanha-polo
                 WHERE ROWID(campanha-polo) = tt-campanha.rw-campanha EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL campanha-polo THEN

                ASSIGN campanha-polo.nr-ord-produ = i-nr-ord-produ.
        END.

    
END.

RUN pi-finalizar IN h-acomp.

RETURN "OK":U.

/*****************************************************************
** Procedures
*****************************************************************/

PROCEDURE piCriaOPCampanha:

    FIND FIRST ITEM
         WHERE ITEM.it-codigo = c-cd-produto NO-LOCK NO-ERROR.  
    
    FIND FIRST item-uni-estab
         WHERE item-uni-estab.it-codigo = c-cd-produto
           AND (item-uni-estab.cod-estabel = "412" OR item-uni-estab.cod-estabel = "422") NO-LOCK NO-ERROR. /*solic-318*/ 

    FIND FIRST ctrab-linha WHERE
               (ctrab-linha.cod-estabel = '412' OR ctrab-linha.cod-estabel = '422') AND /*solic-318*/ 
               ctrab-linha.cod-ctrab = c-gm-codigo NO-LOCK NO-ERROR.

    IF NOT AVAIL ctrab-linha THEN DO:
        CREATE ttErroAux.
        ASSIGN ttErroAux.i-sequen = 999998
               ttErroAux.cd-erro  = 17006
               ttErroAux.mensagem = "Linha de Produá∆o inexistente para o Gr. M†q. da campanha!" +
                                    " Verifique o relacionamento no POLSF012".
        RETURN "NOK":U.

    END.
    
    FIND FIRST lin-prod
         WHERE (lin-prod.cod-estabel = "412" OR lin-prod.cod-estabel = "422") /*solic-318*/ 
           AND lin-prod.nr-linha    = ctrab-linha.nr-linha NO-LOCK NO-ERROR.

    FOR EACH tt-ord-prod: DELETE tt-ord-prod. END.

    CREATE tt-ord-prod.
    ASSIGN tt-ord-prod.cod-versao-integracao = 003
           tt-ord-prod.prog-seg              = "essf0011"
           tt-ord-prod.aloca-reserva         = NO
           tt-ord-prod.ind-tipo-movto        = 1
           tt-ord-prod.faixa-numeracao       = 2
           tt-ord-prod.nr-ord-produ          = f-gera-numero-op-automatica()
           tt-ord-prod.it-codigo             = c-cd-produto
           tt-ord-prod.cod-refer             = ""
           tt-ord-prod.qt-ordem              = de-total-op
           tt-ord-prod.tipo                  = 1 /*Interna*/
           tt-ord-prod.cod-estabel           = STRING({cdp\poloestab.i 422}) /*solic-318*/ 
           tt-ord-prod.estado                = 1
           tt-ord-prod.nr-linha              = lin-prod.nr-linha 
           tt-ord-prod.un                    = ITEM.un
           tt-ord-prod.dt-termino            = TODAY /*campanha.dt-termino */
           tt-ord-prod.dt-inicio             = TODAY /*campanha.dt-inicio*/
           tt-ord-prod.dt-orig               = TODAY
           tt-ord-prod.dt-emissao            = TODAY
           tt-ord-prod.cod-depos             = item-uni-estab.deposito-pad
           tt-ord-prod.conta-ordem           = lin-prod.conta-ordem
           tt-ord-prod.lote-serie            = "" 
           tt-ord-prod.ep-codigo-usuario     = i-ep-codigo-usuario
           tt-ord-prod.seg-usuario           = c-seg-usuario
           tt-ord-prod.narrativa             = ""
           tt-ord-prod.origem                = "CP":U.

    IF param-cp.planej-op = 1 THEN
       ASSIGN tt-ord-prod.cd-planejado = item-uni-estab.cd-planejado.
    ELSE
       ASSIGN tt-ord-prod.cd-planejado = lin-prod.cd-planejado.

    RUN pi-acompanhar IN h-acomp (INPUT "Campanha - Ordem de Produá∆o: " + STRING(tt-ord-prod.nr-ord-produ)).

    RUN cpp/cpapi301.p (INPUT-OUTPUT TABLE tt-ord-prod,
                        INPUT-OUTPUT TABLE tt-reapro,
                        INPUT-OUTPUT TABLE tt-erro,
                        INPUT YES).

    FIND FIRST tt-ord-prod NO-ERROR.
    
    IF RETURN-VALUE = "NOK" THEN DO:

        FOR EACH tt-erro:
            CREATE ttErroAux.
            BUFFER-COPY tt-erro TO ttErroAux.
        END.

        CREATE ttErroAux.
        ASSIGN ttErroAux.i-sequen = 999999
               ttErroAux.cd-erro  = 17006
               ttErroAux.mensagem = "Houve falha durante a geraá∆o da OP: " + 
                                    STRING(tt-ord-prod.nr-ord-produ) + 
                                    " Produto: " + tt-ord-prod.it-codigo.
       RETURN "NOK":U.
    END.
    ELSE DO:

        ASSIGN i-nr-ord-produ = tt-ord-prod.nr-ord-produ.

        RETURN "OK":U.
    END.

END PROCEDURE.
